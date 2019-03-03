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
        FunVal env' n body -> eval0 (Map.insert n val2 env') body

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

-- ReaderT is added to the stack:

type Eval3 α = ReaderT Env (ExceptT String Identity) α

runEval3 :: Env -> Eval3 α -> Either String α
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
    mval <- asks $ Map.lookup n
    case mval of
        Nothing -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval3 (Plus e1 e2) = do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval3 (Abs n e) = do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2) = do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval3 body)
        _ -> throwError "type error in application"

-- Adding StateT to keep track of the number of eval steps:

type Eval4 α = ReaderT Env (ExceptT String (StateT Integer Identity)) α

runEval4 :: Env -> Integer -> Eval4 α -> (Either String α, Integer)
runEval4 env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
    tick
    return $ IntVal i
eval4 (Var n) = do
    tick
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval4 (Plus e1 e2) = do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval4 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2) = do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
        FunVal env' n body ->
            local (const $ Map.insert n val2 env') (eval4 body)
        _ -> throwError "type error in application"

-- Providing logging capabilities with WriterT

type Eval5 α =
    ReaderT Env(ExceptT String (WriterT [String] (StateT Integer Identity))) α

runEval5 :: Env -> Integer -> Eval5 α -> ((Either String α, [String]), Integer)
runEval5 env st ev =
    runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
    tick
    return $ IntVal i
eval5 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval5 (Plus e1 e2) = do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _ -> throwError "type error in addition"
eval5 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2) = do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval5 body)
        _ -> throwError "type error in application"
