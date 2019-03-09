module Except where

import Control.Monad.Identity
import Control.Monad.Except

type ExceptionTest a = ExceptT String IO a

runExceptionTest :: ExceptionTest a -> IO (Either String a)
runExceptionTest e = runExceptT e

somethingThatReturnsAVal :: ExceptionTest Int
somethingThatReturnsAVal = do
    liftIO $ putStrLn "Returning Val from somethingThatReturnsAVal"
    return 4

somethingThatErrors :: Int -> ExceptionTest ()
somethingThatErrors n = do
    liftIO $ putStrLn "Executing somethingThatErrors"
    when (n < 5) $ do
        liftIO $ putStrLn "Throwing error"
        throwError ("n is less than 5")
    liftIO $ putStrLn "If n is >= 5 then this is called, otherwise not"

exceptionTest :: ExceptionTest ()
exceptionTest = do
    liftIO $ putStrLn "Starting Monad Transformer Computation"
    n <- somethingThatReturnsAVal
    somethingThatErrors n `catchError` \e -> do
        liftIO . putStrLn $ "Catching error thrown by somethingThatErrors" ++ e
    liftIO $ putStrLn "Since error was caught, this will be called"
    somethingThatErrors n
    -- Below is not called since somethingThatErrors throws an error
    liftIO $ putStrLn "Error was not caught so the rest is short circuited"

main :: IO ()
main = do
    result <- runExceptionTest exceptionTest
    case result of
        Left a -> putStrLn $ "Computation failed with error: " ++ a
        Right a -> putStrLn "Computation succeeded"
