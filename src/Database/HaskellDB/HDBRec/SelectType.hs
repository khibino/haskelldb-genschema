{-# LANGUAGE ConstraintKinds #-}

module Database.HaskellDB.HDBRec.SelectType (
  RelationSelect, ResultSelect
  ) where

import Database.HaskellDB (Select, Attr, Expr)

type RelationSelect fld val rel = Select (Attr fld val) rel (Expr val)
type ResultSelect fld val rel = Select (Attr fld val) rel val
