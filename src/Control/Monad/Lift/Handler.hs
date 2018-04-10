{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lift.Handler
    ( MonadHandler, liftHandler
    , MonadHandlerControl, suspendHandler, resumeHandler, captureHandler
    , extractHandler
    , liftControlHandler, controlHandler, liftOpHandler, liftOpHandler_
    , liftDiscardHandler
    , MonadHandlerInvariant, hoistisoHandler
    , MonadHandlerFunctor, hoistHandler
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
import           Snap.Snaplet (Handler)


-- snap-layers ---------------------------------------------------------------
import           Snap.Layers ()


------------------------------------------------------------------------------
type MonadHandler b v = MonadBase (Handler b v)


------------------------------------------------------------------------------
liftHandler :: MonadHandler b v m => Handler b v a -> m a
liftHandler = liftB


------------------------------------------------------------------------------
type MonadHandlerControl b v m = MonadBaseControl (Handler b v) m


------------------------------------------------------------------------------
suspendHandler :: MonadHandlerControl b v m
    => m a
    -> OuterState (Handler b v) m
    -> Handler b v (OuterEffects (Handler b v) m a)
suspendHandler = suspendB


------------------------------------------------------------------------------
resumeHandler :: MonadHandlerControl b v m
    => OuterEffects (Handler b v) m a -> m a
resumeHandler = resumeB


------------------------------------------------------------------------------
captureHandler :: MonadHandlerControl b v m
    => m (OuterState (Handler b v) m)
captureHandler = captureB


------------------------------------------------------------------------------
extractHandler :: MonadHandlerControl b v m
    => proxy m -> OuterResult (Handler b v) m a
    -> Either (OuterResult (Handler b v) m b) a
extractHandler = extractB


------------------------------------------------------------------------------
liftControlHandler :: MonadHandlerControl b v m
    => ((forall c. m c -> Handler b v (OuterEffects (Handler b v) m c))
        -> Handler b v a)
    -> m a
liftControlHandler = liftControlB


------------------------------------------------------------------------------
controlHandler :: MonadHandlerControl b v m
    => ((forall c. m c -> Handler b v (OuterEffects (Handler b v) m c))
        -> Handler b v (OuterEffects (Handler b v) m a))
    -> m a
controlHandler = controlB


------------------------------------------------------------------------------
liftOpHandler :: MonadHandlerControl b v m
    => ((a -> Handler b v (OuterEffects (Handler b v) m c))
        -> Handler b v (OuterEffects (Handler b v) m d))
    -> (a -> m c)
    -> m d
liftOpHandler = liftOpB


------------------------------------------------------------------------------
liftOpHandler_ :: MonadHandlerControl b v m
    => (Handler b v (OuterEffects (Handler b v) m a)
        -> Handler b v (OuterEffects (Handler b v) m b))
    -> m a
    -> m b
liftOpHandler_ = liftOpB_


------------------------------------------------------------------------------
liftDiscardHandler :: MonadHandlerControl b v m
    => (Handler b v () -> Handler b v a) -> m () -> m a
liftDiscardHandler = liftDiscardB


------------------------------------------------------------------------------
type MonadHandlerInvariant b v j n m = MonadBaseInvariant j n (Handler b v) m


------------------------------------------------------------------------------
hoistisoHandler :: MonadHandlerInvariant b v j n m
    => (forall c. Handler b v c -> j c)
    -> (forall c. j c -> Handler b v c)
    -> m a
    -> n a
hoistisoHandler = hoistisoB


------------------------------------------------------------------------------
type MonadHandlerFunctor b v j n m = MonadBaseFunctor j n (Handler b v) m


------------------------------------------------------------------------------
hoistHandler :: MonadHandlerFunctor b v j n m
    => (forall c. Handler b v c -> j c) -> m a -> n a
hoistHandler = hoistB
