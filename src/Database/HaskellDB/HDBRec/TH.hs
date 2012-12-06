{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.HaskellDB.HDBRec.TH (
  ConName, VarName,

  defineFieldType,
  defineFieldExpr,
  defineField,

  defineRelationType,
  defineResultType,
  defineRelationExpr,
  defineRelation,

  defineTable,
  defineTableDefault,

  defineRecordType,
  defineRecordInstance,
  defineRecord,
  defineRecordDefault

  ) where

import Data.Char (toUpper, toLower)
import Database.HaskellDB (Attr, Expr)
import Database.HaskellDB.HDBRec (RecCons, RecNil, FieldTag(fieldName), (#))
import Database.HaskellDB.Database (GetRec(getRec), getValue)
import Database.HaskellDB.DBLayout (Table, mkAttr, baseTable, hdbMakeEntry)

import Language.Haskell.TH (Q, Dec, Name)
import Language.Haskell.TH
  (mkName,
   dataD, cxt, normalC,
   conT,
   normalB, litE, stringL,
   sigD, valD, varP, conE,
   tySynD,
   recC, varStrictType, isStrict, strictType,
   recConE, -- appE, varE,
   ppr, runQ)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.PprLib as TH
import qualified Language.Haskell.TH.Syntax as TH


capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize ""     = ""

newtype ConName = ConName { unCon :: Name }
newtype VarName = VarName { unVar :: Name }

conName :: String -> ConName
conName =  ConName . mkName . capitalize

varName :: String -> VarName
varName =  VarName . mkName . unCapitalize


defineFieldType :: ConName -- ^ Name of the type and constructor for this field.
                -> String  -- ^ Name of the column this field represents. Can be any string, though it should match the database column name.
                -> Q [Dec]
defineFieldType (ConName typeName) colName = do
  -- data declaration representing this field.
  fieldD <- dataD (cxt []) typeName [] [normalC typeName []] []
  -- instance declaration in FieldTag class for this field.
  fieldI <- [d| instance FieldTag $(conT typeName) where
                  fieldName _ = $(litE (stringL colName)) |]
  return $ fieldD : fieldI

defineFieldExpr :: VarName  -- ^ Name of the function which makes an attribute for this field (i.e, Attr <typeName> <fieldType>). Must be a legal function name.
                -> ConName  -- ^ Name of the type and constructor for this field. Must be a legal type name.
                -> TH.TypeQ -- ^ The type of the field.
                -> Q [Dec]
defineFieldExpr (VarName attrName) (ConName typeName) typeQ = do
  -- Type signature for the function which creates an Attr value for the field.
  fieldS <- sigD attrName [t|Attr $(conT typeName) $typeQ|]
  -- actual function.
  fieldF <- valD (varP attrName) (normalB [|mkAttr $(conE typeName)|]) []

  return [fieldS, fieldF]

-- | Creates necessary data and function declarations to represent
-- the given field. Used internally by other make functions. All strings given must
-- be legal, as they will not be transformed in any way.
defineField :: ConName  -- ^ Name of the type and constructor for this field. Must be a legal type name.
            -> VarName  -- ^ Name of the function which makes an attribute for this field
                         --   (i.e, Attr <typeName> <fieldType>). Must be a legal function name.
            -> String   -- ^ Name of the column this field represents. Can be any string,
                         --   though it should match the database column name.
            -> TH.TypeQ -- ^ The type of the field.
            -> Q [Dec]
defineField typeName attrName colName typeQ = do
  typ <- defineFieldType typeName colName
  val <- defineFieldExpr attrName typeName typeQ
  return $ typ ++ val


-- | Build the type of the table from the fields given.
mkRelationType :: [(ConName, TH.TypeQ)] -> TH.TypeQ
mkRelationType =  foldr (\(ConName n,e) exp' -> [t|RecCons $(conT n) (Expr $e) $exp'|]) [t|RecNil|]

mkResultType :: [(ConName, TH.TypeQ)] -> TH.TypeQ
mkResultType =  foldr (\(ConName n,e) exp' -> [t|RecCons $(conT n) $e $exp'|]) [t|RecNil|]

-- | Creates a type synonym for a table with the name given, using
-- the list of fields given.
defineRelationType :: ConName               -- ^ Name of the type synonym.
                   -> [(ConName, TH.TypeQ)] -- ^ List of fields in the table.
                                            --   Must be legal, properly type constructor names.
                   -> TH.DecQ               -- ^ The type synonym declaration.
defineRelationType (ConName typeName) fields = do
  -- Create the type synonmym representing for our table.
  tySynD typeName [] $ mkRelationType fields

defineResultType :: ConName
                 -> [(ConName, TH.TypeQ)]
                 -> TH.DecQ
defineResultType (ConName typeName) fields =
  tySynD typeName [] $ mkResultType fields

tableColumns :: [ConName] -> TH.ExpQ
tableColumns []             = TH.runIO . ioError . userError
                              $ "tlbCols: zero length list of name is not allowed."
tableColumns (ConName f:[]) = [|hdbMakeEntry $(conE f) |]
tableColumns (ConName f:fs) = [|hdbMakeEntry $(conE f) # $(tableColumns fs)|]

tableBoby :: String -> [ConName] -> TH.ExpQ
tableBoby sqlName fields = [| baseTable sqlName $(tableColumns fields) |]

defineRelationExpr :: VarName   -- ^ Var name of table can be used in HaksellDB relational algebra query.
                   -> ConName   -- ^ Name of the type synonym of table type.
                   -> String    -- ^ Actual table name string in SQL
                   -> [ConName] -- ^ Data constructor name list
                   -> Q [Dec]
defineRelationExpr (VarName exprName) (ConName typeName) sqlName fields = do
  sig <- sigQ
  val <- valQ
  return [sig, val]
  where
    sigQ = sigD exprName [t| Table $(conT typeName) |]
    valQ = valD (varP exprName) (normalB $ tableBoby sqlName fields) []

defineRelation :: VarName               -- ^ Var name of table can be used in HaksellDB relational algebra query
               -> ConName               -- ^ Name of the type synonym of table type.
               -> String                -- ^ Actual table name string in SQL
               -> [(ConName, TH.TypeQ)] -- ^ List of fields in the table.
               -> Q [Dec]
defineRelation relExprName relTypeName sqlName fields = do
  typ  <- defineRelationType relTypeName fields
  expr <- defineRelationExpr relExprName relTypeName sqlName (map fst fields)
  return $ typ : expr

defineTable :: VarName
            -> ConName
            -> String
            -> [((VarName, String), (ConName, TH.TypeQ))]
            -> ConName
            -> Q [Dec]
defineTable relExprName relTypeName sqlName fields resultTypeName = do
  let types = map snd fields
  rel    <- defineRelation relExprName relTypeName sqlName types
  result <- defineResultType resultTypeName types
  let defF :: ((VarName, String), (ConName, TH.TypeQ)) -> Q [Dec]
      defF ((var, name), (con, typ)) = defineField con var name typ
  flds <- fmap concat . mapM defF $ fields
  return $ rel ++ result : flds

defineTableDefault :: String               -- ^ schema name string of table in SQL
                   -> String               -- ^ table name string in SQL
                   -> [(String, TH.TypeQ)] -- ^ field name string in SQL and field types
                   -> Q [Dec]
defineTableDefault schema name fields =
  defineTable relExpr relType sqlName fldsInfo resultType
  where
    relExpr = varCamelcaseName name
    relType = conCamelcaseName name
    resultType = conCamelcaseName $ name ++ "_result"
    sqlName = schema ++ '.' : name
    fldsInfo = map
               (\ (n, t) -> ((varCamelcaseName n, n), (conCamelcaseName n, t)))
               fields


defineRecordType :: ConName               -- ^ Name of the data type of table record type.
                 -> [(VarName, TH.TypeQ)] -- ^ List of fields in the table. Must be legal, properly cased record fields.
                 -> TH.DecQ               -- ^ The data type record declaration.
defineRecordType (ConName typeName) fields = do
  dataD (cxt []) typeName [] [recC typeName (map fld fields)] []
  where
    fld (VarName n, tq) = varStrictType n (strictType isStrict tq)

defineRecordInstance :: TH.TypeQ -- ^ table type.
                     -> ConName  -- ^ Name of the data type of table record type.
                     -> [(VarName, String)] -- ^ 
                     -> Q [Dec]
defineRecordInstance tableType (ConName typeName) fields =
  [d| instance GetRec $tableType $(conT typeName) where
        getRec vfs _ _ stmt =
          return $(recConE typeName (recFields [| vfs |] [| stmt |] fields))
  |]
  where
    fieldQ vfsE stmtE (VarName n, f) =
      (,) n `fmap`
      [| getValue $vfsE $stmtE $(litE . stringL $ f) |]
    recFields vfsE stmtE = map (fieldQ vfsE stmtE)

defineRecord :: TH.TypeQ                        -- ^ table type.
             -> ConName                         -- ^ Name of the data type of table record type.
             -> [(VarName, (String, TH.TypeQ))] -- ^ (fieldVarName, (fieldInSQL, fieldTypeInTable))
             -> Q [Dec]
defineRecord tableType typeName fields = do
  let defs = map (\ (n, (_, t)) -> (n, t)) fields
  typ  <- defineRecordType typeName defs
  let getter = map (\ (n, (s, _)) -> (n, s)) fields
  inst <- defineRecordInstance tableType typeName getter
  return $ typ:inst

defineRecordDefault :: TH.TypeQ
                    -> String
                    -> [(String, TH.TypeQ)]
                    -> Q [Dec]
defineRecordDefault tableType name fields =
  defineRecord tableType recType fldsInfo
  where
    recType = conCamelcaseName name
    fldsInfo = map
               (\(n, t) -> (varCamelcaseName n, (n, t)))
               fields

nameChars :: String
nameChars =  ['0' .. '9'] ++ ['A' .. 'B'] ++  ['a' .. 'z']

splitForName :: String -> [String]
splitForName str
  | rest /= [] = tk : splitForName (tail rest)
  | otherwise  = [tk]
  where
    (tk, rest) = span (`elem` nameChars) str

camelcaseUpper :: String -> String
camelcaseUpper =  concat . map capitalize . splitForName

-- camelcaseLower :: String -> String
-- camelcaseLower =  unCapitalize . camelcaseUpper

conCamelcaseName :: String -> ConName
conCamelcaseName =  conName . camelcaseUpper

varCamelcaseName :: String -> VarName
varCamelcaseName =  varName . camelcaseUpper

pprQ :: (Functor m, TH.Quasi m, TH.Ppr a) => Q a -> m TH.Doc
pprQ =  fmap ppr . runQ
