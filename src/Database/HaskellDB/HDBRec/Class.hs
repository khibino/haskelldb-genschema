{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.HaskellDB.HDBRec.Class (
  PiRecord(piRec), (##)
  ) where

import Database.HaskellDB (Record, Expr, copy, (#))
import Database.HaskellDB.HDBRec (RecCons, RecNil, FieldTag(fieldName))
import Database.HaskellDB.Query (Rel(Rel), Attr(Attr))

class PiRecord r pr' pr where
  piRec :: Rel r -> Record pr' -> Record pr

instance PiRecord RecNil r r where
  piRec _ r = r

instance (PiRecord rest pr' pr, FieldTag f)
         => PiRecord (RecCons f (Expr a) rest) pr' (RecCons f (Expr a) pr) where
  piRec t@(Rel v s) pr = copy (attr . fieldT $ t) t # (piRec (restT t) pr)
    where
      attr :: FieldTag f => f -> Attr f a
      attr =  Attr . fieldName
      fieldT :: Rel (RecCons f a rest) -> f
      fieldT =  error "PiRecord: fieldT"
      restT  :: Rel (RecCons f a rest) -> Rel rest
      restT _ = Rel v s

(##) :: PiRecord r pr' pr => Rel r -> Record pr' -> Record pr
(##) =  piRec

infixr 5 ##
