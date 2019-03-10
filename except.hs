module Except where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

-- Note: if this was reversed, it wouldn't return a state if an error occured
type ExceptionTest a = ExceptT String (StateT Int IO) a

runExceptionTest :: ExceptionTest a -> IO (Either String a, Int)
runExceptionTest e = runStateT (runExceptT e) 0 -- opposite order to above

somethingThatReturnsAVal :: ExceptionTest Int
somethingThatReturnsAVal = do
    liftIO $ putStrLn "Returning Val from somethingThatReturnsAVal"
    return 4

somethingThatErrors :: ExceptionTest ()
somethingThatErrors = do
    liftIO $ putStrLn "Executing somethingThatErrors"
    liftIO $ putStrLn "Getting number from state"
    n <- get
    when (n < 5) $ do
        liftIO $ putStrLn "Throwing error"
        throwError ("n is less than 5")
    liftIO $ putStrLn "If n is >= 5 then this is called, otherwise not"

exceptionTest :: ExceptionTest ()
exceptionTest = do
    liftIO $ putStrLn "Starting Monad Transformer Computation"
    n <- somethingThatReturnsAVal
    liftIO $ putStrLn "Storing returned number in state"
    put n
    somethingThatErrors `catchError` \e -> do
        liftIO . putStrLn $ "Catching error from somethingThatErrors: " ++ e
    liftIO $ putStrLn "Since error was caught, this will be called"
    put 2
    somethingThatErrors
    -- Below is not called since somethingThatErrors throws an error
    liftIO $ putStrLn "Error was not caught so the rest is short circuited"

main :: IO ()
main = do
    (result, finalState) <- runExceptionTest exceptionTest
    case result of
        Left a -> putStrLn $ "Computation failed with error: " ++ a
        Right a -> putStrLn "Computation succeeded"
    putStrLn $ "Final state was " ++ (show finalState)
