{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lift.Initializer
    ( MonadInitializer, liftInitializer
    , MonadInitializerControl, suspendInitializer, resumeInitializer
    , captureInitializer
    , extractInitializer
    , liftControlInitializer, controlInitializer, liftOpInitializer
    , liftOpInitializer_
    , liftDiscardInitializer
    , MonadInitializerInvariant, hoistisoInitializer
    , MonadInitializerFunctor, hoistInitializer
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (OuterEffects, OuterResult, OuterState)
import           Control.Monad.Lift.Base
                     ( MonadBase, liftB
                     , MonadBaseControl, suspendB, resumeB, captureB, extractB
                     , liftControlB, controlB, liftOpB, liftOpB_, liftDiscardB
                     , MonadBaseInvariant, hoistisoB
                     , MonadBaseFunctor, hoistB
                     )


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Initializer)


-- snap-layers ---------------------------------------------------------------
import           Snap.Layers ()


------------------------------------------------------------------------------
type MonadInitializer b v = MonadBase (Initializer b v)


------------------------------------------------------------------------------
liftInitializer :: MonadInitializer b v m => Initializer b v a -> m a
liftInitializer = liftB


------------------------------------------------------------------------------
type MonadInitializerControl b v m = MonadBaseControl (Initializer b v) m


------------------------------------------------------------------------------
suspendInitializer :: MonadInitializerControl b v m
    => m a
    -> OuterState (Initializer b v) m
    -> Initializer b v (OuterEffects (Initializer b v) m a)
suspendInitializer = suspendB


------------------------------------------------------------------------------
resumeInitializer :: MonadInitializerControl b v m
    => OuterEffects (Initializer b v) m a -> m a
resumeInitializer = resumeB


------------------------------------------------------------------------------
captureInitializer :: MonadInitializerControl b v m
    => m (OuterState (Initializer b v) m)
captureInitializer = captureB


------------------------------------------------------------------------------
extractInitializer :: MonadInitializerControl b v m
    => proxy m -> OuterResult (Initializer b v) m a -> Maybe a
extractInitializer = extractB


------------------------------------------------------------------------------
liftControlInitializer :: MonadInitializerControl b v m
    => ((forall c. m c
        -> Initializer b v (OuterEffects (Initializer b v) m c))
        -> Initializer b v a)
    -> m a
liftControlInitializer = liftControlB


------------------------------------------------------------------------------
controlInitializer :: MonadInitializerControl b v m
    => ((forall c. m c
        -> Initializer b v (OuterEffects (Initializer b v) m c))
        -> Initializer b v (OuterEffects (Initializer b v) m a))
    -> m a
controlInitializer = controlB


------------------------------------------------------------------------------
liftOpInitializer :: MonadInitializerControl b v m
    => ((a -> Initializer b v (OuterEffects (Initializer b v) m c))
        -> Initializer b v (OuterEffects (Initializer b v) m d))
    -> (a -> m c)
    -> m d
liftOpInitializer = liftOpB


------------------------------------------------------------------------------
liftOpInitializer_ :: MonadInitializerControl b v m
    => (Initializer b v (OuterEffects (Initializer b v) m a)
        -> Initializer b v (OuterEffects (Initializer b v) m b))
    -> m a
    -> m b
liftOpInitializer_ = liftOpB_


------------------------------------------------------------------------------
liftDiscardInitializer :: MonadInitializerControl b v m
    => (Initializer b v () -> Initializer b v a) -> m () -> m a
liftDiscardInitializer = liftDiscardB


------------------------------------------------------------------------------
type MonadInitializerInvariant b v j n m =
    MonadBaseInvariant j n (Initializer b v) m


------------------------------------------------------------------------------
hoistisoInitializer :: MonadInitializerInvariant b v j n m
    => (forall c. Initializer b v c -> j c)
    -> (forall c. j c -> Initializer b v c)
    -> m a
    -> n a
hoistisoInitializer = hoistisoB


------------------------------------------------------------------------------
type MonadInitializerFunctor b v j n m =
    MonadBaseFunctor j n (Initializer b v) m


------------------------------------------------------------------------------
hoistInitializer :: MonadInitializerFunctor b v j n m
    => (forall c. Initializer b v c -> j c) -> m a -> n a
hoistInitializer = hoistB
