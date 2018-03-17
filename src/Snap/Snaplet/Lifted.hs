{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Lifted
    ( module Monad.Snaplet
    , module Snap.Snaplet.Lifted
    )
where

-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Handler, Snaplet, SnapletLens, SnapletInit)
import qualified Snap.Snaplet as S


-- snap-core -----------------------------------------------------------------
import           Snap.Core (Snap)


-- snap-layers ---------------------------------------------------------------
import           Control.Monad.Lift.Initializer
                     ( MonadInitializer, liftInitializer
                     )
import           Control.Monad.Lift.Handler (MonadHandler, liftHandler)
import           Control.Monad.Lift.Snap (MonadSnapFunctor, hoistSnap)
import           Monad.Snaplet (MonadSnaplet (..))
import           Snap.Layers ()


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


------------------------------------------------------------------------------
getSnapletState :: MonadHandler b v m => m (Snaplet v)
getSnapletState = liftHandler S.getSnapletState


------------------------------------------------------------------------------
putSnapletState :: MonadHandler b v m => Snaplet v -> m ()
putSnapletState = liftHandler . S.putSnapletState


------------------------------------------------------------------------------
modifySnapletState :: MonadHandler b v m => (Snaplet v -> Snaplet v) -> m ()
modifySnapletState = liftHandler . S.modifySnapletState


------------------------------------------------------------------------------
getsSnapletState :: MonadHandler b v m => (Snaplet v -> a) -> m a
getsSnapletState = liftHandler . S.getsSnapletState


------------------------------------------------------------------------------
nestSnaplet :: MonadInitializer b v m
    => ByteString -> SnapletLens v w -> SnapletInit b w -> m (Snaplet w)
nestSnaplet b l = liftInitializer . S.nestSnaplet b l


------------------------------------------------------------------------------
embedSnaplet :: MonadInitializer b v m
    => ByteString -> SnapletLens v w -> SnapletInit w w -> m (Snaplet w)
embedSnaplet b l = liftInitializer . S.embedSnaplet b l


------------------------------------------------------------------------------
onUnload :: MonadInitializer b v m => IO () -> m ()
onUnload = liftInitializer . S.onUnload


------------------------------------------------------------------------------
addPostInitHook :: MonadInitializer b v m => (v -> IO (Either Text v)) -> m ()
addPostInitHook = liftInitializer . S.addPostInitHook


------------------------------------------------------------------------------
addPostInitHookBase :: MonadInitializer b v m
    => (Snaplet b -> IO (Either Text (Snaplet b))) -> m ()
addPostInitHookBase = liftInitializer . S.addPostInitHookBase


------------------------------------------------------------------------------
printInfo :: MonadInitializer b v m => Text -> m ()
printInfo = liftInitializer . S.printInfo


------------------------------------------------------------------------------
getRoutes :: MonadInitializer b v m => m [ByteString]
getRoutes = liftInitializer S.getRoutes


------------------------------------------------------------------------------
getEnvironment :: MonadInitializer b v m => m String
getEnvironment = liftInitializer S.getEnvironment


------------------------------------------------------------------------------
addRoutes :: MonadInitializer b v m => [(ByteString, Handler b v ())] -> m ()
addRoutes = liftInitializer . S.addRoutes


------------------------------------------------------------------------------
wrapSite :: MonadInitializer b v m
    => (Handler b v () -> Handler b v ()) -> m ()
wrapSite = liftInitializer . S.wrapSite


------------------------------------------------------------------------------
failIfNotLocal :: MonadSnapFunctor Snap m m => m a -> m a
failIfNotLocal = hoistSnap S.failIfNotLocal


------------------------------------------------------------------------------
reloadSite :: MonadHandler b v m => m ()
reloadSite = liftHandler S.reloadSite


------------------------------------------------------------------------------
modifyMaster :: MonadHandler b v m => v -> m ()
modifyMaster = liftHandler . S.modifyMaster
