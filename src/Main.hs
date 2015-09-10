{-# LANGUAGE OverloadedStrings #-}
module Main where

import QA
import JavaScript
import qualified Parser as P

main :: IO ()
main = putStrLn . concatMap codegenTop . suiteToJS =<< login' -- $ [login]

login' :: IO [Test]
login' = do
    test <- P.parse P.test <$> readFile "login.test"
    case test of
        Left  err -> print err >> return [Test "PARSEFAIL" []]
        Right val -> return [val]
{-return <$>
    either (const $ Test "PARSEFAIL" []) id . P.parse P.test <$>
            readFile "login.test"-}

login :: Test
login = Test "login"
    [Act    $ GoToUrl "http://qa.trackmypos.com/apollo",
     Assert $ TextExists "Log In",
     Act    $ Assign "#UserName" "apollo",
     Act    $ Assign "#Password" "1234",
     Act    $ Click  ".login-action",
     Assert $ TextExists "Dashboard"]
