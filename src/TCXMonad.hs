module TCXMonad (
  M, runM,
  mget, mstate, mupdate, mfail, tell, listen, censor, catchError
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict
import Control.Monad.Except

type M r s w a =
  ReaderT r (WriterT w (ExceptT String  (State s))) a

runM :: M r s w a -> r -> s -> (Either String (a, w), s)
runM ma r =
  runState (runExceptT (runWriterT (runReaderT ma r)))

mget :: Monoid w => M r s w r
mget = ask

mstate :: Monoid w => M r s w s
mstate = get

mupdate :: Monoid w => (s -> s) -> M r s w s
mupdate g = do
  s <- get
  modify g
  return s

mfail :: Monoid w => String -> M r s w a
mfail = throwError

