{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Snaplet
    ( MonadSnaplet, Base, Focus, SetFocus
    , with, withTop, with', withTop', getLens, getOpaqueConfig
    , getSnapletAncestry, getSnapletFilePath, getSnapletName
    , getSnapletDescription, getSnapletUserConfig, getSnapletRootURL
    , snapletURL
    )
where

-- configurator --------------------------------------------------------------
import           Data.Configurator.Types (Config)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift)


-- lens ----------------------------------------------------------------------
import           Control.Lens.Lens (ALens')


-- snap ----------------------------------------------------------------------
import           Snap.Snaplet (Snaplet, SnapletConfig, subSnaplet)
import qualified Snap.Snaplet as S


-- snap-layers ---------------------------------------------------------------
import           Snap.Layers ()


-- text ----------------------------------------------------------------------
import           Data.Text (Text)


------------------------------------------------------------------------------
class Monad m => MonadSnaplet m where
    type Inner m :: * -> * -> * -> *
    type Base m :: *
    type Focus m :: *
    type SetFocus v m :: * -> *

    with :: v ~ Focus m
        => ALens' v (Snaplet v') -> Inner m (Base m) v' a -> m a
    with = with' . subSnaplet

    withTop :: b ~ Base m
        => ALens' b (Snaplet v') -> Inner m (Base m) v' a -> m a
    withTop = withTop' . subSnaplet

    with' :: v ~ Focus m
        => ALens' (Snaplet v) (Snaplet v') -> Inner m (Base m) v' a -> m a

    withTop' :: (b ~ Base m, n ~ SetFocus v' m)
        => ALens' (Snaplet b) (Snaplet v') -> Inner m (Base m) v' a -> m a

    getLens :: (b ~ Base m, v ~ Focus m)
        => m (ALens' (Snaplet b) (Snaplet v))

    getOpaqueConfig :: m SnapletConfig

    getSnapletAncestry :: m [Text]

    getSnapletFilePath :: m FilePath

    getSnapletName :: m (Maybe Text)

    getSnapletDescription :: m Text

    getSnapletUserConfig :: m Config

    getSnapletRootURL :: m ByteString

    snapletURL :: ByteString -> m ByteString


------------------------------------------------------------------------------
instance (S.MonadSnaplet m, Monad (m b v)) => MonadSnaplet (m b v) where
    type Inner (m b v) = m
    type Base (m b v) = b
    type Focus (m b v) = v
    type SetFocus v' (m b v) = m b v'

    with = S.with
    withTop = S.withTop
    with' = S.with'
    withTop' = S.withTop'
    getLens = S.getLens
    getOpaqueConfig = S.getOpaqueConfig
    getSnapletAncestry = S.getSnapletAncestry
    getSnapletFilePath = S.getSnapletFilePath
    getSnapletName = S.getSnapletName
    getSnapletDescription = S.getSnapletDescription
    getSnapletUserConfig = S.getSnapletUserConfig
    getSnapletRootURL = S.getSnapletRootURL
    snapletURL = S.snapletURL


------------------------------------------------------------------------------
instance (MonadTrans t, Monad m, Monad (t m), MonadSnaplet m) =>
    MonadSnaplet (t m)
  where
    type Inner (t m) = Inner m
    type Base (t m) = Base m
    type Focus (t m) = Focus m
    type SetFocus v (t m) = t (SetFocus v m)

    with lens = lift . with lens
    withTop lens = lift . withTop lens
    with' lens = lift . with' lens
    withTop' lens = lift . withTop' lens
    getLens = getLens
    getOpaqueConfig = lift getOpaqueConfig
    getSnapletAncestry = lift getSnapletAncestry
    getSnapletFilePath = lift getSnapletFilePath
    getSnapletName = lift getSnapletName
    getSnapletDescription = lift getSnapletDescription
    getSnapletUserConfig = lift getSnapletUserConfig
    getSnapletRootURL = lift getSnapletRootURL
    snapletURL = lift . snapletURL
