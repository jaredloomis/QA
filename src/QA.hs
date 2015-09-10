module QA where

import JavaScript
import Element

type Suite = [Test]

data Test = Test {
    testName  :: String,
    testSteps :: [Step]
    } deriving (Show, Eq)

data Step = Act Action | Assert Assertion
  deriving (Show, Eq)

data Action =
    GoToUrl String
  | Click   Element
  | Assign  Element String
  | Execute String
  deriving (Show, Eq)

data Assertion =
    TextExists String
  deriving (Show, Eq)

suiteToJS :: Suite -> [JSTop]
suiteToJS tests =
    let defs = map testToJS tests
    in defs ++ callTests
  where
    callTests :: [JSTop]
    callTests = map (JSTopStmt . JSExpr . jsCall . JSVar . testName) tests

testToJS :: Test -> JSTop
testToJS test = JSFunction (testName test) [] body
  where
    body :: [JSStmt]
    body = [startCasperTest $ setupCasper ++ steps ++ [runCasper]]

    steps :: [JSStmt]
    steps = addScreenshots 0 $ map stepToJS (testSteps test)

    -- | "casper.test.begin(function(casperTest) { ... })"
    startCasperTest :: [JSStmt] -> JSStmt
    startCasperTest actions = JSExpr $ JSApp (casper ~> "test" ~> "begin")
        [JSStr $ testName test,
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
    assertCount = length $ filter isAssert (testSteps test)

stepToJS :: Step -> JSStmt
stepToJS (Act (GoToUrl url)) =
    JSExpr $ (casper ~> "thenOpen") ~$ JSStr url
stepToJS (Act (Assign element val)) = 
    thenEvaluate [JSExpr $ (JSQuery element ~> "val") ~$ JSStr val]
stepToJS (Act (Click element)) =
    thenEvaluate [JSExpr $ jsCall (JSQuery element ~> "click")]
stepToJS (Act (Execute js)) =
    thenEvaluate [JSExpr $ JSRaw js]
stepToJS (Assert (TextExists str)) = JSExpr $
    casper ~> "then" ~$ jsLam
        (JSApp (casperTest ~> "assertSelectorHasText")
               [JSStr "*", JSStr str])

-------------
-- Helpers --
-------------

isAssert :: Step -> Bool
isAssert (Assert _) = True
isAssert _          = False


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
