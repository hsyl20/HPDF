---------------------------------------------------------
-- |
-- Copyright   : (c) 2023, Henning Thielemann
-- License     : BSD-style
--
-- Maintainer  : haskell@henning-thielemann.de
-- Stability   : experimental
-- Portability : portable
--
-- PDF expressions and functions
---------------------------------------------------------
module Graphics.PDF.Expression (
    PDFExpression (..),
    serialize,

    (==%), (/=%),
    (<%), (>=%), (>%), (<=%),
    min, max,
    true, false,
    (&&*), (||*), not,
    ifThenElse,

    sqrt,
    sinDeg, cosDeg, log, log10,
    pow, atan2Deg,

    floor, ceiling, round, truncate,
    ) where
     
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Ratio as Ratio

import qualified Control.Monad.Trans as MT
import qualified Control.Monad.Writer as MW
import qualified Control.Monad.State as MS
import Control.Monad (when)
import Control.Applicative (pure, liftA2, liftA3, (<*>))

import Prelude hiding
        (not, min, max, log, sqrt, floor, ceiling, round, truncate)


data Token = Token C.ByteString | Index Int
    deriving (Show)

type ExprMonad = MW.Writer [Token]

newtype PDFExpression a = PDFExpression (ExprMonad ())


serialize :: (Function f) => f -> CL.ByteString
serialize f =
    CL.unwords $
    (\(n,stream) ->
        map (\token ->
                case token of
                    Token str -> CL.fromStrict str
                    Index k -> CL.pack $ show $ n-1-k) stream) $
    MW.runWriter $ do
        n <- MS.execStateT (serializeFunction f) 0
        tokens $ replicate n "pop"
        return n


class Function f where
    serializeFunction :: f -> MS.StateT Int ExprMonad ()

instance (Argument a, Function f) => Function (a -> f) where
    serializeFunction f = do
        arg <- argumentExpression
        serializeFunction $ f arg

instance Function (PDFExpression a) where
    serializeFunction = serializeResult

instance (Result a, Result b) => Function (a,b) where
    serializeFunction = serializeResult

instance (Result a, Result b, Result c) => Function (a,b,c) where
    serializeFunction = serializeResult

instance
    (Result a, Result b, Result c, Result d) =>
        Function (a,b,c,d) where
    serializeFunction = serializeResult


{-
Extra class is needed in order
to forbid nested functions like (a -> (a, a->a))
-}
class Result a where
    serializeResult :: a -> MS.StateT Int ExprMonad ()

instance Result (PDFExpression a) where
    serializeResult (PDFExpression a) = do
        n <- MS.get
        MT.lift $ a >> when (n>0) (tokens [show (n+1), "1", "roll"])

instance (Result a, Result b) => Result (a,b) where
    serializeResult (a,b) =
        serializeResult a >> serializeResult b

instance (Result a, Result b, Result c) => Result (a,b,c) where
    serializeResult (a,b,c) =
        serializeResult a >> serializeResult b >> serializeResult c

instance
    (Result a, Result b, Result c, Result d) =>
        Result (a,b,c,d) where
    serializeResult (a,b,c,d) =
        serializeResult a >> serializeResult b >>
        serializeResult c >> serializeResult d



class Argument a where
    argumentExpression :: (Monad m) => MS.StateT Int m a

instance Argument (PDFExpression a) where
    argumentExpression = MS.StateT $ \n -> pure (argument n, n+1)

instance (Argument a, Argument b) => Argument (a,b) where
    argumentExpression =
        liftA2 (,) argumentExpression argumentExpression

instance (Argument a, Argument b, Argument c) => Argument (a,b,c) where
    argumentExpression =
        liftA3 (,,) argumentExpression argumentExpression argumentExpression

instance
    (Argument a, Argument b, Argument c, Argument d) =>
        Argument (a,b,c,d) where
    argumentExpression =
        pure (,,,)
            <*> argumentExpression
            <*> argumentExpression
            <*> argumentExpression
            <*> argumentExpression


tokens :: [String] -> ExprMonad ()
tokens = MW.tell . map (Token . C.pack)


argument :: Int -> PDFExpression a
argument k = PDFExpression $ MW.tell [Index k, Token $ C.pack "index"]


function1 :: String -> PDFExpression a -> PDFExpression b
function1 name (PDFExpression a) =
    PDFExpression $ a >> tokens [name]

function2 :: String -> PDFExpression a -> PDFExpression b -> PDFExpression c
function2 name (PDFExpression a) (PDFExpression b) =
    PDFExpression $ a >> b >> tokens [name]


infix 4 ==%, /=%, <%, <=%, >%, >=%

(==%), (/=%) ::
   (Eq a) => PDFExpression a -> PDFExpression a -> PDFExpression Bool
(==%) = function2 "eq"
(/=%) = function2 "ne"

(<%), (>=%), (>%), (<=%) ::
   (Ord a) => PDFExpression a -> PDFExpression a -> PDFExpression Bool
(<%)  = function2 "lt"
(>=%) = function2 "ge"
(>%)  = function2 "gt"
(<=%) = function2 "le"


minMax ::
    (Ord a) => String -> PDFExpression a -> PDFExpression a -> PDFExpression a
minMax cmp (PDFExpression a) (PDFExpression b) =
    PDFExpression $ do
        a
        b
        tokens ["2", "copy", cmp, "{1 pop}", "{exch 1 pop}", "ifelse"]

min, max :: (Ord a) => PDFExpression a -> PDFExpression a -> PDFExpression a
min = minMax "lt"
max = minMax "gt"


true, false :: PDFExpression Bool
true = PDFExpression $ tokens ["true"]
false = PDFExpression $ tokens ["false"]

infixr 3 &&*
(&&*) :: PDFExpression Bool -> PDFExpression Bool -> PDFExpression Bool
(&&*) = function2 "and"

infixr 2 ||*
(||*) :: PDFExpression Bool -> PDFExpression Bool -> PDFExpression Bool
(||*) = function2 "or"

not :: PDFExpression Bool -> PDFExpression Bool
not = function1 "not"



ifThenElse ::
    (Ord a) =>
    PDFExpression Bool -> PDFExpression a -> PDFExpression a -> PDFExpression a
ifThenElse (PDFExpression cond) (PDFExpression a) (PDFExpression b) =
    PDFExpression $ do
        cond
        tokens ["{"] >> a >> tokens ["}"]
        tokens ["{"] >> b >> tokens ["}"]
        tokens ["ifelse"]




instance (Num a) => Num (PDFExpression a) where
    fromInteger k = PDFExpression $ tokens [show k]
    negate = function1 "negate"
    abs = function1 "abs"
    (+) = function2 "add"
    (-) = function2 "sub"
    (*) = function2 "mul"
    signum (PDFExpression a) =
        PDFExpression $ do
            a
            tokens ["dup", "0", "gt"]
            tokens   ["{", "pop", "1", "}"]
            tokens   ["{", "0", "lt", "{-1}", "{0}", "ifelse", "}"]
            tokens ["ifelse"]

instance (Fractional a) => Fractional (PDFExpression a) where
    fromRational r =
        PDFExpression $
        tokens [show (Ratio.numerator r), show (Ratio.denominator r), "div"]
    (/) = function2 "div"


sqrt, sinDeg, cosDeg, log, log10 ::
    (Floating a) => PDFExpression a -> PDFExpression a
sqrt = function1 "sqrt"
sinDeg = function1 "sin"
cosDeg = function1 "cos"
log = function1 "ln"
log10 = function1 "log"

pow, atan2Deg ::
    (Floating a) => PDFExpression a -> PDFExpression a -> PDFExpression a
pow = function2 "exp"
atan2Deg = function2 "atan"



floor, ceiling, round, truncate ::
    (RealFrac a) => PDFExpression a -> PDFExpression Int
floor    = function1 "floor"
ceiling  = function1 "ceiling"
round    = function1 "round"
truncate = function1 "truncate"
