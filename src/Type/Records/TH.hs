{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Type.Records.TH
    ( makeFieldSelectors
    , makeFieldSelectors'
    , FieldSelectorConfig(..)
    , defaultFieldSelectorConfig
    ) where

import Control.Monad (zipWithM)
import Data.Char (toUpper)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Datatype.TyVarBndr (bndrReq)

data FieldSelectorConfig
    = FieldSelectorConfig { fieldNameToSelectorName :: String -> String }

defaultFieldSelectorConfig :: FieldSelectorConfig
defaultFieldSelectorConfig = FieldSelectorConfig simpleFieldNameToSelectorName

simpleFieldNameToSelectorName :: String -> String
simpleFieldNameToSelectorName (c:cs) = toUpper c : cs
simpleFieldNameToSelectorName [] = []

deriveName :: (String -> String) -> Name -> Name
deriveName f (Name occ _) =
     mkName $ f $ occString occ

makeFieldSelectors :: Name -> Q [Dec]
makeFieldSelectors = makeFieldSelectors' defaultFieldSelectorConfig

makeFieldSelectors' :: FieldSelectorConfig -> Name -> Q [Dec]
makeFieldSelectors' cfg nm = do
    dti <- reifyDatatype nm
    ci <- case datatypeCons dti of
      []    -> fail "Can't generate selectors for void type"
      [ci] -> return ci
      _     -> fail "Can't generate selectors for sum type"

    makeFieldSelectorsCon cfg ci

makeFieldSelectorsCon :: FieldSelectorConfig -> ConstructorInfo -> Q [Dec]
makeFieldSelectorsCon cfg ci
  | RecordConstructor names <- constructorVariant ci = do
      concat <$> zipWithM (makeFieldSelector cfg ci) [0..] names
  | otherwise = fail "Expected record constructor"

makeFieldSelector :: FieldSelectorConfig -> ConstructorInfo -> Int -> Name -> Q [Dec]
makeFieldSelector cfg ci n fldName = do
    let famName = deriveName (fieldNameToSelectorName cfg) fldName
    vars <- zipWithM (\i _ -> newName $ "v" ++ show i) [0 :: Int ..] (constructorFields ci)
    let pat = pure $ foldr (flip AppT . VarT) (PromotedT $ constructorName ci) (reverse vars)
    lhs <- conT famName `appT` pat
    rhs <- varT $ vars !! n

    tv <- newName "a"
    let bndr = PlainTV tv bndrReq
    let famHead = TypeFamilyHead famName [bndr] NoSig Nothing
    let eqn = TySynEqn Nothing lhs rhs
    return [ClosedTypeFamilyD famHead [eqn]]

