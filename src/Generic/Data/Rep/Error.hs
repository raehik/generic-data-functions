-- | Common descriptions for common generic data representation errors.

module Generic.Data.Rep.Error where

import GHC.TypeLits ( ErrorMessage(Text) )

wrapE :: String -> String -> String
wrapE msgGot msgWhyBad =
       "Generic.Data.Rep.Error:\n"
    <> "Attempted to use an invalid generic instance: \n"
    <> "got: "<>msgGot<>"\n"
    <> "but: "<>msgWhyBad<>"\n"
    <> "You can likely catch such errors during compilation.\n"
    <> "See the generic-data-functions package on Hackage."

-- | Common type error string for when you attempt to use a generic instance
--   at an empty data type (e.g. 'Data.Void.Void', 'GHC.Generics.V1').
type ENoEmpty = 'Text "Requested generic instance disallows empty data type"
eNoEmpty :: String
eNoEmpty = wrapE "empty data type" "disallowed"

-- | Common type error string for when GHC is asked to derive a non-sum
--   instance, but the data type in question turns out to be a sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedSum =
    'Text "Cannot derive non-sum generic instance for sum data type"
eNoSum :: String
eNoSum = wrapE "sum data type" "cannot use non-sum generics"

-- | Common type error string for when GHC is asked to derive a sum instance,
--   but the data type in question turns out to be a non-sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedNonSum =
    'Text "Refusing to derive sum generic instance for non-sum data type"
eNeedSum :: String
eNeedSum = wrapE "non-sum data type" "cannot use sum-only generics"
