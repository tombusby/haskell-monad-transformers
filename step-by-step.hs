module Transformers where
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp
    = Lit Integer
    | Var Name
    | Plus Exp Exp
    | Abs Name Exp
    | App Exp Exp
    deriving (Show)

data Value
    = IntVal Integer
    | FunVal Env Name Exp
    deriving (Show)

type Env = Map.Map Name Value

-- Non-monadic:

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
    let IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
    in  IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
    in case val1 of
        FunVal env0 n body -> eval0 (Map.insert n val2 env0) body

-- Basic monadic (no transformers, eval errors cause exception):

type Eval1 α = Identity α

runEval1 :: Eval1 α -> α
runEval1 ev = runIdentity ev

eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (fail $ "undefined variable: " ++ n) return
    $ Map.lookup n env
eval1 env (Plus e1 e2) = do
    ~(IntVal i1) <- eval1 env e1
    ~(IntVal i2) <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
        FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- Basic monad transformer (error handing added):

type Eval2 α = ExceptT String Identity α

runEval2 :: Eval2 α -> Either String α
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = maybe (throwError $ "Undefined Variable: " ++ n) return
    $ Map.lookup n env
eval2a env (Plus e1 e2) = do
    e1' <- eval2a env e1
    e2' <- eval2a env e2
    case (e1', e2') of
        (IntVal i1 ,IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "Type Error"
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do
    val1 <- eval2a env e1
    val2 <- eval2a env e2
    case val1 of
        FunVal env' n body -> eval2a (Map.insert n val2 env') body
        _ -> throwError "Type Error"

-- Basic monad transformer (exploit implicit `fail` calls with failed matching):

eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = maybe (throwError $ "Undefined Variable: " ++ n) return
    $ Map.lookup n env
eval2c env (Plus e1 e2) = do
    ~(IntVal i1) <- eval2c env e1
    ~(IntVal i2) <- eval2c env e2
    return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do
    ~(FunVal env' n body) <- eval2c env e1
    val2 <- eval2c env e2
    eval2a (Map.insert n val2 env') body

-- Basic monad transformer (final version with good error messages)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Nothing -> throwError $ "Undefined variable: " ++ n
    Just val -> return val
eval2 env (Plus e1 e2) = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "Type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _ -> throwError "Type error in application"
