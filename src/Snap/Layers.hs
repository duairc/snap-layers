{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Layers
    ()
where

-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Control.Applicative (Alternative)
import           Control.Exception (SomeException)
import           Control.Monad (MonadPlus)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (IORef)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mappend, mempty)
#endif
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup (Semigroup, (<>))
#endif
import           Unsafe.Coerce (unsafeCoerce)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
import           Control.Monad.Lift.Base (MonadBase)
import           Control.Monad.Lift.IO (controlIO, hoistIO)
import           Control.Monad.Lift.Top (liftT)
import           Monad.Abort (MonadAbort, abort)
import           Monad.Mask (MonadMask, getMaskingState, setMaskingState)
import           Monad.Reader (MonadReader, reader, ask, local)
import           Monad.Recover (MonadRecover, recover)
import           Monad.ST
                     ( MonadST, newRef, readRef, writeRef, atomicModifyRef
                     )
import           Monad.State (MonadState, state, get, put)
import           Monad.Try (MonadTry, mtry)


-- lens ----------------------------------------------------------------------
import           Control.Lens.Lens (ALens', (^#), storing)


-- monad-control -------------------------------------------------------------
import           Control.Monad.Trans.Control (MonadBaseControl)


-- monad-control-layers ------------------------------------------------------
import           Control.Monad.Trans.Control.Layers ()


-- mtl -----------------------------------------------------------------------
import qualified Control.Monad.Reader.Class as MTL
import qualified Control.Monad.State.Class as MTL


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Handler, Initializer, Snaplet)


-- snap-core -----------------------------------------------------------------
import           Snap.Core (Snap, MonadSnap, liftSnap)
import qualified Snap.Internal.Core as I


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Control.Monad.Trans.State (StateT (StateT))


------------------------------------------------------------------------------
newtype Zero = Zero I.Zero


#if MIN_VERSION_base(4, 9, 0)
------------------------------------------------------------------------------
instance Semigroup Zero where
    Zero I.PassOnProcessing <> a = a
    a <> _ = a
    {-# INLINE (<>) #-}


#endif
------------------------------------------------------------------------------
instance Monoid Zero where
    mempty = Zero I.PassOnProcessing

    mappend (Zero I.PassOnProcessing) a = a
    mappend a _ = a
    {-# INLINE mappend #-}


------------------------------------------------------------------------------
newtype SnapT m a = SnapT (ExceptT Zero (StateT I.SnapState m) a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO
    )


------------------------------------------------------------------------------
instance Iso1 (SnapT m) where
    type Codomain1 (SnapT m) = ExceptT Zero (StateT I.SnapState m)
    to1 (SnapT m) = m
    from1 = SnapT


------------------------------------------------------------------------------
instance MonadTrans SnapT where
    lift = defaultLift2


------------------------------------------------------------------------------
instance MInvariant SnapT where
    hoistiso = defaultHoistiso2


------------------------------------------------------------------------------
instance MFunctor SnapT where
    hoist = defaultHoist2


------------------------------------------------------------------------------
instance MonadTransControl SnapT where
    suspend = defaultSuspend2
    resume = defaultResume2
    capture = defaultCapture2
    extract = defaultExtract2


------------------------------------------------------------------------------
type instance LayerResult SnapT =
    DefaultLayerResult2 (ExceptT Zero) (StateT I.SnapState)
type instance LayerState SnapT =
    DefaultLayerState2 (ExceptT Zero) (StateT I.SnapState)


------------------------------------------------------------------------------
instance Iso1 Snap where
    type Codomain1 Snap = SnapT IO

    to1 (I.Snap k) = SnapT $ ExceptT $ StateT $ k right left
      where
        left a s = pure (Left (Zero a), s)
        right a s = pure (Right a, s)
    {-# INLINE to1 #-}

    from1 (SnapT (ExceptT (StateT m))) = I.Snap $ \right left s -> do
        (e, s') <- m s
        case e of
            Left (Zero a) -> left a s'
            Right a -> right a s'
    {-# INLINE from1 #-}


------------------------------------------------------------------------------
instance MonadInner IO Snap where
    liftI = liftIO


------------------------------------------------------------------------------
instance MonadInnerInvariant IO Snap IO Snap where
    hoistisoI = defaultHoistisoI


------------------------------------------------------------------------------
instance MonadInnerFunctor IO Snap IO Snap where
    hoistI = defaultHoistI


------------------------------------------------------------------------------
instance MonadInnerControl IO Snap where
    suspendI = defaultSuspendI
    resumeI = defaultResumeI
    captureI = defaultCaptureI
    extractI = defaultExtractI
    mapI = defaultMapI


------------------------------------------------------------------------------
instance MonadBase Snap Snap


------------------------------------------------------------------------------
instance MonadAbort SomeException Snap where
    abort = liftIO . abort
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadMask Snap where
    getMaskingState = liftIO getMaskingState
    {-# INLINE getMaskingState #-}
    setMaskingState s = hoistIO (setMaskingState s)
    {-# INLINE setMaskingState #-}


------------------------------------------------------------------------------
instance MonadRecover SomeException Snap where
    recover a handler = controlIO $ \run -> recover (run a) (run . handler)
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadST IORef Snap where
    newRef = liftIO . newRef
    {-# INLINE newRef #-}
    readRef = liftIO . readRef
    {-# INLINE readRef #-}
    writeRef = (liftIO .) . writeRef
    {-# INLINE writeRef #-}
    atomicModifyRef = (liftIO .) . atomicModifyRef
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance MonadTry Snap where
    mtry = from' . fmap (either (Left . from') Right) . mtry . to'
      where
        from' = from1 . SnapT
        to' m = let SnapT m' = to1 m in m'
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( MonadSnap m, MonadInner m (t m)
    , Functor (t m), Applicative (t m), Monad (t m), Alternative (t m)
    , MonadPlus (t m), MonadIO (t m), MonadBaseControl IO (t m)
    )
  =>
    MonadSnap (t m)
  where
    liftSnap = liftT . liftSnap
    {-# INLINE liftSnap #-}


------------------------------------------------------------------------------
instance MonadInner IO (Initializer b v) where
    liftI = liftIO


------------------------------------------------------------------------------
instance MonadAbort SomeException (Initializer b v) where
    abort = liftIO . abort
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadST IORef (Initializer b v) where
    newRef = liftIO . newRef
    {-# INLINE newRef #-}
    readRef = liftIO . readRef
    {-# INLINE readRef #-}
    writeRef = (liftIO .) . writeRef
    {-# INLINE writeRef #-}
    atomicModifyRef = (liftIO .) . atomicModifyRef
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance MonadBase (Initializer b v) (Initializer b v)


------------------------------------------------------------------------------
newtype HandlerT b v m a = HandlerT
    (ReaderT (ALens' (Snaplet b) (Snaplet v)) (StateT (Snaplet b) m) a)
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO
    )


------------------------------------------------------------------------------
instance Iso1 (HandlerT b v m) where
    type Codomain1 (HandlerT b v m) = ReaderT (ALens' (Snaplet b) (Snaplet v))
        (StateT (Snaplet b) m)
    to1 (HandlerT m) = m
    from1 = HandlerT


------------------------------------------------------------------------------
instance MonadTrans (HandlerT b v) where
    lift = defaultLift2


------------------------------------------------------------------------------
instance MInvariant (HandlerT b v) where
    hoistiso = defaultHoistiso2


------------------------------------------------------------------------------
instance MFunctor (HandlerT b v) where
    hoist = defaultHoist2


------------------------------------------------------------------------------
instance MonadTransControl (HandlerT b v) where
    suspend = defaultSuspend2
    resume = defaultResume2
    capture = defaultCapture2
    extract = defaultExtract2


------------------------------------------------------------------------------
type instance LayerResult (HandlerT b v) = DefaultLayerResult2
    (ReaderT (ALens' (Snaplet b) (Snaplet v))) (StateT (Snaplet b))
type instance LayerState (HandlerT b v) = DefaultLayerState2
    (ReaderT (ALens' (Snaplet b) (Snaplet v))) (StateT (Snaplet b))


------------------------------------------------------------------------------
instance Iso1 (Handler b v) where
    type Codomain1 (Handler b v) = HandlerT b v Snap

    to1 m = HandlerT $ ReaderT $ \lens -> StateT $ \b -> do
        (a, v, b') <- f lens (b ^# lens) b
        pure (a, storing lens v b')
      where
        Handler (Lensed f) = toHandler_ m
    {-# INLINE to1 #-}

    from1 (HandlerT (ReaderT f)) = fromHandler_ $
        Handler $ Lensed $ \lens v b -> do
            let StateT f' = f lens
            (a, b') <- f' (storing lens v b)
            pure (a, b' ^# lens, b')
    {-# INLINE from1 #-}


------------------------------------------------------------------------------
instance MonadInner IO (Handler b v) where
    liftI = liftIO


------------------------------------------------------------------------------
instance MonadInnerInvariant IO (Handler b v) IO (Handler b v) where
    hoistisoI = defaultHoistisoI


------------------------------------------------------------------------------
instance MonadInnerFunctor IO (Handler b v) IO (Handler b v) where
    hoistI = defaultHoistI


------------------------------------------------------------------------------
instance MonadInnerControl IO (Handler b v) where
    suspendI = defaultSuspendI
    resumeI = defaultResumeI
    captureI = defaultCaptureI
    extractI = defaultExtractI
    mapI = defaultMapI


------------------------------------------------------------------------------
instance MonadInner Snap (Handler b v) where
    liftI = liftSnap


------------------------------------------------------------------------------
instance MonadInnerInvariant Snap (Handler b v) Snap (Handler b v) where
    hoistisoI = defaultHoistisoI


------------------------------------------------------------------------------
instance MonadInnerFunctor Snap (Handler b v) Snap (Handler b v) where
    hoistI = defaultHoistI


------------------------------------------------------------------------------
instance MonadInnerControl Snap (Handler b v) where
    suspendI = defaultSuspendI
    resumeI = defaultResumeI
    captureI = defaultCaptureI
    extractI = defaultExtractI
    mapI = defaultMapI


------------------------------------------------------------------------------
instance MonadBase (Handler b v) (Handler b v)


------------------------------------------------------------------------------
instance MonadAbort SomeException (Handler b v) where
    abort = liftIO . abort
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadMask (Handler b v) where
    getMaskingState = liftIO getMaskingState
    {-# INLINE getMaskingState #-}
    setMaskingState s = hoistIO (setMaskingState s)
    {-# INLINE setMaskingState #-}


------------------------------------------------------------------------------
instance MonadReader v (Handler b v) where
    reader = MTL.reader
    ask = MTL.ask
    local = MTL.local


------------------------------------------------------------------------------
instance MonadRecover SomeException (Handler b v) where
    recover a handler = controlIO $ \run -> recover (run a) (run . handler)
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadST IORef (Handler b v) where
    newRef = liftIO . newRef
    {-# INLINE newRef #-}
    readRef = liftIO . readRef
    {-# INLINE readRef #-}
    writeRef = (liftIO .) . writeRef
    {-# INLINE writeRef #-}
    atomicModifyRef = (liftIO .) . atomicModifyRef
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance MonadState v (Handler b v) where
    state = MTL.state
    get = MTL.get
    put = MTL.put


------------------------------------------------------------------------------
instance MonadTry (Handler b v) where
    mtry = from' . fmap (either (Left . from') Right) . mtry . to'
      where
        from' = from1 . HandlerT
        to' m = let HandlerT m' = to1 m in m'
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
newtype Handler_ b v a = Handler (Lensed (Snaplet b) (Snaplet v) Snap a)
newtype Lensed b v m a = Lensed (ALens' b v -> v -> b -> m (a, v, b))


------------------------------------------------------------------------------
toHandler_ :: Handler b v a -> Handler_ b v a
toHandler_ = unsafeCoerce
{-# INLINE toHandler_ #-}


------------------------------------------------------------------------------
fromHandler_ :: Handler_ b v a -> Handler b v a
fromHandler_ = unsafeCoerce
{-# INLINE fromHandler_ #-}
