{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [
    testCase "Empty string is invalid" $ assertEqual "" Nothing (parseAll ""),

    testCase "Single character is a variable" $
      assertEqual "" (Just $ Var 'a') (parseAll "a"),

    testCase "Application of two variables" $
      assertEqual "" (Just $ App (Var 'a') (Var 'b')) (parseAll "(ab)"),

    testCase "Application without closing parenthesis is invalid" $
      assertEqual "" Nothing (parseAll "(ab"),

    testCase "Identity lambda" $
      assertEqual "" (Just $ Lam 'x' (Var 'x')) (parseAll "(\\x.x)")
  ]

