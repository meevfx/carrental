{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_car_rental (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "car_rental"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A car rental backend built with Haskell and Scotty"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://example.com"
