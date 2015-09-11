{-# LANGUAGE ExistentialQuantification #-}
module Test where

import Data.Foldable
import Control.Monad.Free

import Element
import JavaScript

data NamedTest = forall a. NamedTest String (Test a)

testName :: NamedTest -> String
testName (NamedTest name _) = name

----------------
-- Test Monad --
----------------

type Test = Free TestF

goToUrl :: String -> Test ()
goToUrl url = liftF $ GoToUrl url ()

click :: Element -> Test ()
click element = liftF $ Click element ()

assign :: Element -> String -> Test ()
assign element val = liftF $ Assign element val ()

execute :: String -> Test ()
execute js = liftF $ Execute js ()

textExists :: String -> Test ()
textExists str = liftF $ TextExists str ()

------------------
-- Test Functor --
------------------

data TestF next =
    -- Actions
    GoToUrl String         next
  | Click   Element        next
  | Assign  Element String next
  | Execute String         next
    -- Assertions
  | TextExists String      next
  deriving (Show, Eq)

instance Functor TestF where
    fmap f (GoToUrl url x)    = GoToUrl url (f x)
    fmap f (Click e x)        = Click e (f x)
    fmap f (Assign e str x)   = Assign e str (f x)
    fmap f (Execute js x)     = Execute js (f x)
    fmap f (TextExists str x) = TextExists str (f x)

flatten :: Test a -> [TestF ()]
flatten (Free (GoToUrl    u next)) = GoToUrl    u () : flatten next
flatten (Free (Click      e next)) = Click      e () : flatten next
flatten (Free (Assign   e t next)) = Assign   e t () : flatten next
flatten (Free (Execute    j next)) = Execute    j () : flatten next
flatten (Free (TextExists t next)) = TextExists t () : flatten next
flatten (Pure _)                   = []

-------------------
-- To JavaScript --
-------------------

-- | Generate the JS functions for each test,
--   then call each function
suiteToJS :: [NamedTest] -> [JSTop]
suiteToJS tests =
    let defs = map testToJS tests
    in defs ++ callTests
  where
    callTests :: [JSTop]
    callTests = map (JSTopStmt . JSExpr . jsCall . JSVar . testName) tests

-- | Generate a JS function for a test.
--   !!! This does not call the resulting function; see 'suiteToJS'
testToJS :: NamedTest -> JSTop
testToJS (NamedTest name test) = JSFunction name [] [body]
  where
    body :: JSStmt
    body = startCasperTest $ setupCasper ++ steps ++ [runCasper]

    steps :: [JSStmt]
    steps = addScreenshots 0 $ testToStmts test

    -- | "casper.test.begin(function(casperTest) { ... })"
    startCasperTest :: [JSStmt] -> JSStmt
    startCasperTest actions = JSExpr $ JSApp (casper ~> "test" ~> "begin")
        [JSStr name,
         JSInt assertCount,
         JSLam ["casperTest"] actions]

    -- | Casper requires a "start" call with a starting URL. I'm
    --   using google.com
    setupCasper :: [JSStmt]
    setupCasper =
        [JSExpr $ (casper ~> "start") ~$ JSStr "http://google.com/"]

    -- | "casper.run(function(){casperTest.done();})"
    runCasper :: JSStmt
    runCasper = JSExpr . jsApp (casper ~> "run") $
        jsLam $ jsCall (casperTest ~> "done")

    -- | Add "casper.then(function(){casper.capture(screenN.png);}"
    --   after every step
    addScreenshots :: Int -> [JSStmt] -> [JSStmt]
    addScreenshots i (x:xs) =
        let capture = (casper ~> "capture") ~$
                      JSStr ("screen" ++ show i ++ ".png")
            func    = jsLam capture
            expr    = (casper ~> "then") ~$ func
        in x : JSExpr expr : addScreenshots (i+1) xs
    addScreenshots _ [] = []

    -- | The amount of asserts that occur in the test.
    --   this is an argument to 'casper.test.begin'
    --   for some reason
    assertCount :: Int
    assertCount = length . filter isAssert . flatten $ test

-- | The JS statements for a test. This cannot be
--   executed directly; casper has to be setup and
--   cleaned up. See 'testToJS'
testToStmts :: Test a -> [JSStmt]
testToStmts (Free (GoToUrl url next)) =
    (JSExpr $ (casper ~> "thenOpen") ~$ JSStr url) :
    testToStmts next
testToStmts (Free (Click element next)) =
    thenEvaluate [JSExpr $ jsCall (JSQuery element ~> "click")] :
    testToStmts next
testToStmts (Free (Assign element val next)) =
    thenEvaluate [JSExpr $ (JSQuery element ~> "val") ~$ JSStr val] :
    testToStmts next
testToStmts (Free (Execute js next)) =
    thenEvaluate [JSExpr $ JSRaw js] :
    testToStmts next
testToStmts (Free (TextExists str next)) =
    (JSExpr $ casper ~> "then" ~$ jsLam
        (JSApp (casperTest ~> "assertSelectorHasText")
               [JSStr "*", JSStr str])) :
    testToStmts next
testToStmts (Pure _) = []

-------------
-- Helpers --
-------------

isAssert :: TestF a -> Bool
isAssert (TextExists _ _) = True
isAssert _                = False

thenEvaluate :: [JSStmt] -> JSStmt
thenEvaluate actions = JSExpr $
    (casper ~> "thenEvaluate") ~$ JSLam [] actions

(~>) :: JSExpr -> String -> JSExpr
(~>) = JSProp
infixl 6 ~>

(~$) :: JSExpr -> JSExpr -> JSExpr
(~$) = jsApp
infixr 3 ~$

jsCall :: JSExpr -> JSExpr
jsCall f = JSApp f []

jsApp :: JSExpr -> JSExpr -> JSExpr
jsApp f x = JSApp f [x]

jsLam :: JSExpr -> JSExpr
jsLam = JSLam [] . return . JSExpr

casper :: JSExpr
casper = JSVar "casper"

casperTest :: JSExpr
casperTest = JSVar "casperTest"
