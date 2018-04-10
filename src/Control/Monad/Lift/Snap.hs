{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Lift.Snap
    ( MonadSnap, liftSnap
    , MonadSnapControl, suspendSnap, resumeSnap, captureSnap, extractSnap
    , liftControlSnap, controlSnap, liftOpSnap, liftOpSnap_, liftDiscardSnap
    , MonadSnapInvariant, hoistisoSnap
    , MonadSnapFunctor, hoistSnap
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadInner, liftI
                     , MonadInnerControl
                     , OuterEffects, OuterResult, OuterState
                     , suspendI, resumeI, captureI, extractI
                     , liftControlI, controlI, liftOpI, liftOpI_, liftDiscardI
                     , MonadInnerInvariant, hoistisoI
                     , MonadInnerFunctor, hoistI
                     )


-- snap-core -----------------------------------------------------------------
import           Snap.Core (Snap)


-- snap-layers ---------------------------------------------------------------
import           Snap.Layers ()


------------------------------------------------------------------------------
type MonadSnap = MonadInner Snap


------------------------------------------------------------------------------
liftSnap :: MonadSnap m => Snap a -> m a
liftSnap = liftI


------------------------------------------------------------------------------
type MonadSnapControl = MonadInnerControl Snap


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendSnap :: MonadSnapControl m
    => m a
    -> OuterState Snap m
    -> Snap (OuterEffects Snap m a)
suspendSnap = suspendI


------------------------------------------------------------------------------
resumeSnap :: MonadSnapControl m
    => OuterEffects Snap m a
    -> m a
resumeSnap = resumeI (Pm :: Pm Snap)


------------------------------------------------------------------------------
captureSnap :: MonadSnapControl m
    => m (OuterState Snap m)
captureSnap = captureI (Pm :: Pm Snap)


------------------------------------------------------------------------------
extractSnap :: MonadSnapControl m => proxy m -> OuterResult Snap m a
    -> Either (OuterResult Snap m a) a
extractSnap = extractI (Pm :: Pm Snap)


------------------------------------------------------------------------------
liftControlSnap :: MonadSnapControl m
    => ((forall b. m b -> Snap (OuterEffects Snap m b)) -> Snap a)
    -> m a
liftControlSnap = liftControlI


------------------------------------------------------------------------------
controlSnap :: MonadSnapControl m
    => ((forall b. m b -> Snap (OuterEffects Snap m b))
        -> Snap (OuterEffects Snap m a))
    -> m a
controlSnap = controlI


------------------------------------------------------------------------------
liftOpSnap :: MonadSnapControl m
    => ((a -> Snap (OuterEffects Snap m b)) -> Snap (OuterEffects Snap m c))
    -> (a -> m b)
    -> m c
liftOpSnap = liftOpI


------------------------------------------------------------------------------
liftOpSnap_ :: MonadSnapControl m
    => (Snap (OuterEffects Snap m a) -> Snap (OuterEffects Snap m b))
    -> m a
    -> m b
liftOpSnap_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardSnap :: MonadSnapControl m => (Snap () -> Snap a) -> m () -> m a
liftDiscardSnap = liftDiscardI


------------------------------------------------------------------------------
type MonadSnapInvariant j n = MonadInnerInvariant j n Snap


------------------------------------------------------------------------------
hoistisoSnap :: MonadSnapInvariant j n m
    => (forall b. Snap b -> j b)
    -> (forall b. j b -> Snap b)
    -> m a
    -> n a
hoistisoSnap = hoistisoI


------------------------------------------------------------------------------
type MonadSnapFunctor j n = MonadInnerFunctor j n Snap


------------------------------------------------------------------------------
hoistSnap :: MonadSnapFunctor j n m => (forall b. Snap b -> j b) -> m a -> n a
hoistSnap = hoistI
