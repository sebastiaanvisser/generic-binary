{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeOperators
 #-}
module Data.Binary.Generic
( encode
, decode
, put
, get
) where

import Control.Applicative
import GHC.Generics
import Data.Binary (Binary, Put, Get)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy

import qualified Data.Binary as B

encode :: (Generic a, GBinary (Rep a)) => a -> ByteString
encode = runPut . put

decode :: (Generic a, GBinary (Rep a)) => ByteString -> a
decode = runGet get

put :: (Generic a, GBinary (Rep a)) => a -> Put
put = gput . from

get :: (Generic b, GBinary (Rep b)) => Get b
get = to <$> gget

class GBinary f where
  gput :: f a -> Put
  gget :: Get (f a)

instance GBinary U1 where
  gput _ = return ()
  gget = pure U1

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
  gput (x :*: y) = do gput x; gput y
  gget = (:*:) <$> gget <*> gget

instance (GBinary a, GBinary b) => GBinary (a :+: b) where
  gput (L1 l) = do B.put False; gput l
  gput (R1 r) = do B.put True; gput r
  gget = B.get >>= \v -> if not v then L1 <$> gget else R1 <$> gget

instance GBinary a => GBinary (M1 D c a) where
  gput = gput . unM1
  gget = M1 <$> gget

instance GBinary a => GBinary (M1 C c a) where
  gput = gput . unM1
  gget = M1 <$> gget

instance GBinary a => GBinary (M1 S s a) where
  gput = gput . unM1
  gget = M1 <$> gget

instance Binary a => GBinary (K1 i a) where
  gput = B.put . unK1
  gget = K1 <$> B.get

