module Env where

import Data.IORef
import Expr (Expr (Atom), Env, IOThrowsError, SchemeError (UnboundVar))
import Data.Functor
import Data.Maybe (isJust)
import Control.Monad.Except (MonadIO(liftIO), MonadError (throwError))

emptyEnv :: IO Env
emptyEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef key = isJust . lookup key <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError Expr
getVar envRef key = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting unbound variable" key)
        (liftIO . readIORef)
        (lookup key env)

setVar :: Env -> String -> Expr -> IOThrowsError Expr
setVar envRef key val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting unbound variable" key)
        (liftIO . (`writeIORef` val))
        (lookup key env)
  return $ Atom key

defineVar :: Env -> String -> Expr -> IOThrowsError Expr
defineVar envRef key val = do
  isDefined <- liftIO $ isBound envRef key
  if isDefined
    then setVar envRef key val
    else liftIO $ do
      valRef <- newIORef val
      env    <- readIORef envRef
      writeIORef envRef ((key, valRef):env)
      return $ Atom key

bindVars :: Env -> [(String, Expr)] -> IO Env
bindVars envRef v =
  readIORef envRef >>= extendEnv v >>= newIORef
  where
    extendEnv v env = (++ env) <$> mapM addVar v
    addVar (key, val) = newIORef val >>= (\ref -> return (key, ref))
