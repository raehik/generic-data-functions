{-# LANGUAGE UndecidableInstances #-} -- for IDKMANP
{-# LANGUAGE TemplateHaskell #-}
module Test where

import Generic.Data.MetaParse.Cstr
import Generic.Data.MetaParse.Symparsec
import GHC.Generics ( Generic )
import Symparsec.Parsers
import GHC.TypeNats

import Generic.Data.Function.Traverse

data Raeh = Raeh1 | Raeh2 | RaehFF deriving stock (Generic, Show)
type RaehP = Literal "Raeh" :*>: NatHex
instance CstrParser' Raeh where
    type CstrParseResult Raeh = ResultOf RaehP
$(pure [])
instance CstrParser Raeh where
    type ParseCstr Raeh str = SymIt RaehP str
    type ReifyCstrParseResult Raeh n = KnownNat n

instance GenericTraverse Raeh where
    type GenericTraverseF Raeh = Maybe
    type GenericTraverseC Raeh a = ()
    genericTraverseAction _dtName _cstrName _mrec _idx = Nothing
