{-# LANGUAGE OverloadedStrings #-}
module Main where

--import QA
import JavaScript
import qualified Parser as P
import Test

main :: IO ()
main = putStrLn . concatMap codegenTop . suiteToJS $ [NamedTest "login" login]

login :: Test ()
login = do
    goToUrl    "http://qa.trackmypos.com/apollo"
    true       "return true;"
    textExists "Log In"
    assign     "#UserName" "apollo"
    assign     "#Password" ""
    click      ".login-action"
    textExists "Dashboard"

{-
main :: IO ()
main = do
    putStrLn . concatMap codegenTop . suiteToJS $ [login]

login' :: IO [Test]
login' = do
    test <- P.parse P.test <$> readFile "login.test"
    case test of
        Left  err -> print err >> return [Test "PARSEFAIL" []]
        Right val -> return [val]

login :: Test
login = Test "login"
    [Act    $ GoToUrl "http://qa.trackmypos.com/apollo",
     Assert $ TextExists "Log In",
     Act    $ Assign "#UserName" "apollo",
     Act    $ Assign "#Password" "",
     Act    $ Click  ".login-action",
     Assert $ TextExists "Dashboard"]
-}
