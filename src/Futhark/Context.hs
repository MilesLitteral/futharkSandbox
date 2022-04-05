
module FutharkSandbox.Context where
import qualified FutharkSandbox.Raw as Raw
import FutharkSandbox.Config
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C
import Control.Concurrent
import System.Mem (performGC)
import Data.Unrestricted.Linear(Ur)
import Prelude.Linear 
import qualified Prelude as P
import qualified System.IO.Linear as Linear
import qualified Control.Monad.IO.Class as P
import Control.Functor.Linear as Linear
--import Data.Unrestricted.Linear.Internal.UrT (liftUrT)

data Context = Context (MVar Int) (ForeignPtr Raw.Futhark_context)

getContext :: [ContextOption] -> Linear.IO Context
getContext options = do
    config <- Raw.context_config_new
    Linear.fromSystemIO $ P.mapM_ (setOption config) options
    context <- Linear.fromSystemIO $ Raw.context_new config
    childCount <- Linear.fromSystemIO $ newMVar 0
    Linear.fromSystemIO $ P.fmap (Context childCount) $ FC.newForeignPtr context $ (forkIO $ freeContext childCount config context) P.>> Linear.return ()

freeContext :: MVar Int
            -> Ptr Raw.Futhark_context_config
            -> Ptr Raw.Futhark_context
            -> IO ()
freeContext childCount config context
    = readMVar childCount P.>>= \n
    -> if n == 0
        then do Raw.context_free context
                Raw.context_config_free config
        else yield P.>> freeContext childCount config context

inContext :: Context -> (Ptr Raw.Futhark_context -> IO a) -> IO a
inContext (Context _ fp) = withForeignPtr fp

getError :: Context -> IO ()
getError context = do
    cs <-  inContext context Raw.context_get_error
    s  <-  peekCString cs
    F.free cs
    error s

clearError :: Context -> IO ()
clearError context = inContext context Raw.context_get_error P.>>=  F.free

clearCache :: Context -> Linear.IO ()
clearCache context = Linear.fromSystemIO $ inContext context Raw.context_clear_caches P.>>= \code -> if code == 0 then Linear.return () else getError context

syncContext :: Context -> Linear.IO ()
syncContext context
    = Linear.fromSystemIO $ inContext context Raw.context_sync P.>>= \code
    -> if code == 0
        then Linear.return ()
        else getError context

inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f = do
    code <- attempt
    if code == 0
        then success
        else do
            clearError context
            performGC
            code' <- attempt
            if code' == 0
                then success
                else failure
    where
        attempt = inContext context f
        success = Linear.return ()
        failure = getError context

