{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Proxy
import Type.Records.TH
import GHC.TypeLits

data Rec = Rec { fldA, fldB :: Nat }

makeFieldSelectors ''Rec

type V = 'Rec 42 0

assertM :: Bool -> IO ()
assertM True = return ()
assertM False = fail "uh oh"

main :: IO ()
main = do
    assertM $ natVal (Proxy @(FldA V)) == 42
    assertM $ natVal (Proxy @(FldB V)) == 0
