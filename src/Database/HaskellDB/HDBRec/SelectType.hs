{-# LANGUAGE ConstraintKinds #-}

module Database.HaskellDB.HDBRec.SelectType (
  RelationSelect, RecordSelect
  ) where

import Database.HaskellDB (Select, Attr, Expr)

type RelationSelect tbl val rel = Select (Attr tbl val) rel (Expr val)
type RecordSelect tbl val rel = Select (Attr tbl val) rel val
