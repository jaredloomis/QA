module JavaScript where

import Data.List (intercalate)

import Element

type Name = String

data JSTop =
    -- | function f(x, y) { ... }
    JSFunction Name [Name] [JSStmt]
  | JSTopStmt JSStmt
  deriving (Show, Eq)

data JSStmt =
    -- | x;
    JSExpr   JSExpr
    -- | return x;
  | JSReturn JSExpr
    -- | if(b) {t;} else {e;}
  | JSIf JSExpr [JSStmt] (Maybe [JSStmt])
    -- | var x = y;
  | JSDecl Name JSExpr
    -- | x = y;
  | JSAssign JSExpr JSExpr
  deriving (Show, Eq)

data JSExpr =
    -- | 55
    JSInt   !Int
    -- | 43.5
  | JSFlt   !Float
    -- | true OR false
  | JSBool  !Bool
    -- | "hello"
  | JSStr   String

    -- | Reference a var
  | JSVar   Name
    -- | Raw JS code
  | JSRaw   String

    -- | x.y
  | JSProp  JSExpr Name
    -- | x[y]
  | JSIndex JSExpr JSExpr
    -- | $(x)
  | JSQuery Element

    -- | f(x, y, z)
  | JSApp   JSExpr [JSExpr]
    -- | x + y
  | JSBinOp              Name JSExpr JSExpr
    -- | ++x OR x++ ETC
  | JSUnOp  !UnaryFixity Name JSExpr
    -- | function(x, y) { ... }
  | JSLam   [Name] [JSStmt]
  deriving (Show, Eq)

data UnaryFixity = Prefix | Postfix
  deriving (Show, Eq)

codegenTop :: JSTop -> String
codegenTop (JSFunction name params body) =
    "function " ++ name ++ " (" ++
    intercalate ", " params ++ ") {\n" ++
    codegenSeq body ++
    "}"
codegenTop (JSTopStmt stmt) = codegenStmt stmt

codegenStmt :: JSStmt -> String
codegenStmt (JSExpr expr) = codegenExpr expr ++ ";"
codegenStmt (JSReturn expr) =
    "return " ++ codegenExpr expr ++ ";"
codegenStmt (JSIf b t (Just e)) =
    "if(" ++ codegenExpr b ++ ") {\n" ++
    codegenSeq t ++
    "} else {\n" ++
    codegenSeq e ++
    "}"
codegenStmt (JSIf b t Nothing) =
    "if(" ++ codegenExpr b ++ ") {\n" ++
    codegenSeq t ++
    "}"
codegenStmt (JSDecl name e) =
    "var " ++ name ++ " = " ++ codegenExpr e ++ ";"
codegenStmt (JSAssign lval rval) =
    codegenExpr lval ++ " = " ++ codegenExpr rval ++ ";"

codegenSeq :: [JSStmt] -> String
codegenSeq = (++"\n") . intercalate "\n" . map codegenStmt

codegenExpr :: JSExpr -> String
codegenExpr (JSInt      i) = show i
codegenExpr (JSFlt      f) = show f
codegenExpr (JSBool  True) = "true"
codegenExpr (JSBool False) = "false"
codegenExpr (JSStr    str) = show str
codegenExpr (JSRaw    raw) = raw
codegenExpr (JSVar    var) = var
codegenExpr (JSProp expr prop) =
    "(" ++ codegenExpr expr ++ ")." ++ prop
codegenExpr (JSIndex expr index) =
    "(" ++ codegenExpr expr  ++ ")" ++
    "[" ++ codegenExpr index ++ "]"
codegenExpr (JSApp f xs) =
    codegenExpr f ++
    "(" ++
    intercalate ", " (map codegenExpr xs) ++
    ")"
codegenExpr (JSBinOp op x y) =
    "(" ++ codegenExpr x ++ ") " ++
    op ++ " " ++
    "(" ++ codegenExpr y ++ ")"
codegenExpr (JSUnOp Prefix op x) =
    op ++ "(" ++ codegenExpr x ++ ")"
codegenExpr (JSUnOp Postfix op x) =
    "(" ++ codegenExpr x ++ ")" ++ op
codegenExpr (JSLam params body) =
    "(function(" ++ 
    intercalate ", " params ++
    ") {\n" ++
    codegenSeq body ++
    "})"
codegenExpr (JSQuery element) =
    "$(" ++ show (elementQuery element) ++ ")"
