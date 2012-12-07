{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.HaskellDB.Schema.DB2 (
  defineTableDefault
  ) where

import Data.Char (toUpper, toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import System.Time (CalendarTime)
import Language.Haskell.TH (Q, Dec, Type, runIO)

import Database.HDBC (IConnection)
import Database.HaskellDB (
  Query, Rel, Record,
  query, table, constant, restrict, (!.), (.==.))
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)

import Database.HaskellDB.Connect.HDBC.Simple (hdbcSession)

import qualified Database.HaskellDB.HDBRec.TH as Base
import Database.HaskellDB.HDBRec.SelectType (RecordSelect)


$(Base.defineTableDefault
  "SYSCAT" "columns"
  [
    --                                タイプ・
    -- 列名                           スキーマ  タイプ名           長さ    位取り NULL
    -- ------------------------------ --------- ------------------ -------- ----- ------
    -- TABSCHEMA                      SYSIBM    VARCHAR                 128     0 いいえ
    ("tabschema", [t|String|]),
    -- TABNAME                        SYSIBM    VARCHAR                 128     0 いいえ
    ("tabname", [t|String|]),
    -- COLNAME                        SYSIBM    VARCHAR                 128     0 いいえ
    ("colname", [t|String|]),
    -- COLNO                          SYSIBM    SMALLINT                  2     0 いいえ
    ("colno", [t|Int|]),
    -- TYPESCHEMA                     SYSIBM    VARCHAR                 128     0 いいえ
    ("typeschema", [t|String|]),
    -- TYPENAME                       SYSIBM    VARCHAR                  18     0 いいえ
    ("typename", [t|String|]),
    -- LENGTH                         SYSIBM    INTEGER                   4     0 いいえ
    ("length", [t|Int|]),
    -- SCALE                          SYSIBM    SMALLINT                  2     0 いいえ
    ("scale", [t|Int|]),
    -- DEFAULT                        SYSIBM    VARCHAR                 254     0 はい  
    ("default", [t|Maybe String|]),
    -- NULLS                          SYSIBM    CHARACTER                 1     0 いいえ
    ("nulls", [t|String|]),
    -- CODEPAGE                       SYSIBM    SMALLINT                  2     0 いいえ
    ("codepage", [t|Int|]),
    -- LOGGED                         SYSIBM    CHARACTER                 1     0 いいえ
    ("logged", [t|String|]),
    -- COMPACT                        SYSIBM    CHARACTER                 1     0 いいえ
    ("compact", [t|String|]),
    -- COLCARD                        SYSIBM    BIGINT                    8     0 いいえ
    ("colcard", [t|Integer|]),
    -- HIGH2KEY                       SYSIBM    VARCHAR                 254     0 はい  
    ("high2key", [t|Maybe String|]),
    -- LOW2KEY                        SYSIBM    VARCHAR                 254     0 はい  
    ("low2key", [t|Maybe String|]),
    -- AVGCOLLEN                      SYSIBM    INTEGER                   4     0 いいえ
    ("avgcollen", [t|Int|]),
    -- KEYSEQ                         SYSIBM    SMALLINT                  2     0 はい  
    ("keyseq", [t|Maybe Int|]),
    -- PARTKEYSEQ                     SYSIBM    SMALLINT                  2     0 はい  
    ("partkeyseq", [t|Maybe Int|]),
    -- NQUANTILES                     SYSIBM    SMALLINT                  2     0 いいえ
    ("nquantiles", [t|Int|]),
    -- NMOSTFREQ                      SYSIBM    SMALLINT                  2     0 いいえ
    ("nmostfreq", [t|Int|]),
    -- NUMNULLS                       SYSIBM    BIGINT                    8     0 いいえ
    ("numnulls", [t|Integer|]),
    -- TARGET_TYPESCHEMA              SYSIBM    VARCHAR                 128     0 はい  
    ("target_typeschema", [t|Maybe String|]),
    -- TARGET_TYPENAME                SYSIBM    VARCHAR                  18     0 はい  
    ("target_typename", [t|Maybe String|]),
    -- SCOPE_TABSCHEMA                SYSIBM    VARCHAR                 128     0 はい  
    ("scope_tabschema", [t|Maybe String|]),
    -- SCOPE_TABNAME                  SYSIBM    VARCHAR                 128     0 はい  
    ("scope_tabname", [t|Maybe String|]),
    -- SOURCE_TABSCHEMA               SYSIBM    VARCHAR                 128     0 はい  
    ("source_tabschema", [t|Maybe String|]),
    -- SOURCE_TABNAME                 SYSIBM    VARCHAR                 128     0 はい  
    ("source_tabname", [t|Maybe String|]),
    -- DL_FEATURES                    SYSIBM    CHARACTER                10     0 はい  
    ("dl_features", [t|Maybe String|]),
    -- SPECIAL_PROPS                  SYSIBM    CHARACTER                 8     0 はい  
    ("special_props", [t|Maybe String|]),
    -- HIDDEN                         SYSIBM    CHARACTER                 1     0 いいえ
    ("hidden", [t|String|]),
    -- INLINE_LENGTH                  SYSIBM    INTEGER                   4     0 いいえ
    ("inline_length", [t|Int|]),
    -- IDENTITY                       SYSIBM    CHARACTER                 1     0 いいえ
    ("identity", [t|String|]),
    -- GENERATED                      SYSIBM    CHARACTER                 1     0 いいえ
    ("generated", [t|String|]),
    -- TEXT                           SYSIBM    CLOB                  65538     0 はい  
    ("text", [t|Maybe String|]),
    -- REMARKS                        SYSIBM    VARCHAR                 254     0 はい  
    ("remarks", [t|Maybe String|])

    --   36 レコードが選択されました。
  ])


mapFromSql :: Map String (Q Type)
mapFromSql =
  fromList [("VARCHAR",   [t|String|]),
            ("CHAR",      [t|String|]),
            ("CHARACTER", [t|String|]),
            ("TIMESTAMP", [t|CalendarTime|]),
            ("DATE",      [t|CalendarTime|]),
            ("SMALLINT",  [t|Int|]),
            ("INTEGER",   [t|Int|]),
            ("BIGINT",    [t|Integer|]),
            ("BLOB",      [t|String|]),
            ("CLOB",      [t|String|])]

getType :: (RecordSelect Typename String r,
            RecordSelect Colname  String r,
            RecordSelect Nulls    String r)
           => Record r -> (String, Q Type)
getType rec =
  (map toLower $ rec !. colname,
   mayNull $ mapFromSql Map.! (rec !. typename))
  where mayNull typ = if rec !. nulls == "Y"
                      then [t|Maybe $(typ) |]
                      else typ

columnsQuery :: String -> String -> Query (Rel Columns)
columnsQuery scm tbl =
  do col <- table columns
     restrict $ col !. tabschema .==. constant scm
     restrict $ col !. tabname   .==. constant tbl
     return col

getColumns :: IConnection conn
           => IO conn
           -> String
           -> String
           -> IO [ColumnsRecord]
getColumns connAct scm tbl =
   hdbcSession defaultSqlGenerator connAct
   (\_conn -> (`query` columnsQuery scm tbl))

defineTableDefault :: IConnection conn
                   => IO conn
                   -> String
                   -> String
                   -> Q [Dec]
defineTableDefault connAct scm tbl =
  do cols <- runIO $ getColumns connAct scm (map toUpper tbl)
     Base.defineTableDefault scm tbl (map getType cols)
