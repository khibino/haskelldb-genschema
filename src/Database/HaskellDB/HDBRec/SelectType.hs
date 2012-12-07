{-# LANGUAGE ConstraintKinds #-}

module Database.HaskellDB.HDBRec.SelectType (
  RelationSelect, RecordSelect
  ) where

import Database.HaskellDB (Select, Attr, Expr, Record, Rel)

type RelationSelect fld val rec = Select (Attr fld val) (Rel rec) (Expr val)
type RecordSelect fld val rec = Select (Attr fld val) (Record rec) val
