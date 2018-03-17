{-# LANGUAGE FlexibleContexts #-}

module Snap.Core.Lifted
where

-- base ----------------------------------------------------------------------
import           Control.Exception (Exception)
import           Data.Word (Word64)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as L (ByteString)


-- case-insensitive ----------------------------------------------------------
import           Data.CaseInsensitive (CI)


-- io-streams ----------------------------------------------------------------
import           System.IO.Streams (InputStream, OutputStream)


-- readable ------------------------------------------------------------------
import           Data.Readable (Readable)


-- snap-core -----------------------------------------------------------------
import           Snap.Core
                    ( Snap
                    , Request, Response, Method, Params, Cookie
                    , EscapeHttpHandler
                    )
import qualified Snap.Core as S


-- snap-layers ---------------------------------------------------------------
import           Control.Monad.Lift.Snap
                     ( MonadSnap, liftSnap
                     , MonadSnapControl, controlSnap, liftControlSnap
                     , liftOpSnap, resumeSnap
                     , MonadSnapFunctor, hoistSnap
                     )
import           Snap.Layers ()


-- text ----------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)


------------------------------------------------------------------------------
finishWith :: MonadSnap m => Response -> m a
finishWith = liftSnap . S.finishWith


------------------------------------------------------------------------------
catchFinishWith :: MonadSnapControl m => m a -> m (Either Response a)
catchFinishWith m = liftControlSnap (\peel -> S.catchFinishWith (peel m))
    >>= either (pure . Left) (fmap Right . resumeSnap)


------------------------------------------------------------------------------
pass :: MonadSnap m => m a
pass = liftSnap S.pass


------------------------------------------------------------------------------
escapeHttp :: MonadSnap m => EscapeHttpHandler -> m ()
escapeHttp = liftSnap . S.escapeHttp


------------------------------------------------------------------------------
terminateConnection :: (Exception e, MonadSnap m) => e -> m a
terminateConnection = liftSnap . S.terminateConnection


------------------------------------------------------------------------------
method :: MonadSnapFunctor Snap m m => Method -> m a -> m a
method m = hoistSnap (S.method m)
{-# INLINE method #-}


------------------------------------------------------------------------------
methods :: MonadSnapFunctor Snap m m => [Method] -> m a -> m a
methods ms = hoistSnap (S.methods ms)
{-# INLINE methods #-}


------------------------------------------------------------------------------
path :: MonadSnapFunctor Snap m m => ByteString -> m a -> m a
path p = hoistSnap (S.path p)
{-# INLINE path #-}


------------------------------------------------------------------------------
dir :: MonadSnapFunctor Snap m m => ByteString -> m a -> m a
dir p = hoistSnap (S.path p)
{-# INLINE dir #-}


------------------------------------------------------------------------------
pathArg :: (MonadSnapControl m, Readable a) => (a -> m b) -> m b
pathArg = liftOpSnap S.pathArg


------------------------------------------------------------------------------
ifTop :: MonadSnapFunctor Snap m m => m a -> m a
ifTop = hoistSnap S.ifTop
{-# INLINE ifTop #-}


------------------------------------------------------------------------------
route :: MonadSnapControl m => [(ByteString, m a)] -> m a
route routes = controlSnap $ \peel -> S.route (map (fmap peel) routes)
{-# INLINE route #-}


------------------------------------------------------------------------------
routeLocal :: MonadSnapControl m => [(ByteString, m a)] -> m a
routeLocal routes = controlSnap $ \peel ->
    S.routeLocal (map (fmap peel) routes)
{-# INLINE routeLocal #-}


------------------------------------------------------------------------------
getRequest :: MonadSnap m => m Request
getRequest = liftSnap S.getRequest
{-# INLINE getRequest #-}


------------------------------------------------------------------------------
getsRequest :: MonadSnap m => (Request -> a) -> m a
getsRequest = liftSnap . S.getsRequest
{-# INLINE getsRequest #-}


------------------------------------------------------------------------------
getResponse :: MonadSnap m => m Response
getResponse = liftSnap S.getResponse
{-# INLINE getResponse #-}


------------------------------------------------------------------------------
getsResponse :: MonadSnap m => (Response -> a) -> m a
getsResponse = liftSnap . S.getsResponse
{-# INLINE getsResponse #-}


------------------------------------------------------------------------------
putRequest :: MonadSnap m => Request -> m ()
putRequest = liftSnap . S.putRequest
{-# INLINE putRequest #-}


------------------------------------------------------------------------------
putResponse :: MonadSnap m => Response -> m ()
putResponse = liftSnap . S.putResponse
{-# INLINE putResponse #-}


------------------------------------------------------------------------------
modifyRequest :: MonadSnap m => (Request -> Request) -> m ()
modifyRequest = liftSnap . S.modifyRequest
{-# INLINE modifyRequest #-}


------------------------------------------------------------------------------
modifyResponse :: MonadSnap m => (Response -> Response) -> m ()
modifyResponse = liftSnap . S.modifyResponse
{-# INLINE modifyResponse #-}


------------------------------------------------------------------------------
localRequest :: MonadSnapFunctor Snap m m
    => (Request -> Request) -> m a -> m a
localRequest f = hoistSnap (S.localRequest f)
{-# INLINE localRequest #-}


------------------------------------------------------------------------------
withRequest :: MonadSnap m => (Request -> m a) -> m a
withRequest = (getRequest >>=)
{-# INLINE withRequest #-}


------------------------------------------------------------------------------
withResponse :: MonadSnap m => (Response -> m a) -> m a
withResponse = (getResponse >>=)
{-# INLINE withResponse #-}


------------------------------------------------------------------------------
runRequestBody :: MonadSnap m => (InputStream ByteString -> IO a) -> m a
runRequestBody = liftSnap . S.runRequestBody


------------------------------------------------------------------------------
readRequestBody :: MonadSnap m => Word64 -> m L.ByteString
readRequestBody = liftSnap . S.readRequestBody


------------------------------------------------------------------------------
transformRequestBody
    :: (InputStream ByteString -> IO (InputStream ByteString)) -> Snap ()
transformRequestBody = liftSnap . S.transformRequestBody


------------------------------------------------------------------------------
ipHeaderFilter :: MonadSnap m => m ()
ipHeaderFilter = liftSnap S.ipHeaderFilter


------------------------------------------------------------------------------
ipHeaderFilter' :: MonadSnap m => CI ByteString -> m ()
ipHeaderFilter' = liftSnap . S.ipHeaderFilter'


------------------------------------------------------------------------------
getParam :: MonadSnap m => ByteString -> m (Maybe ByteString)
getParam = liftSnap . S.getParam
{-# INLINE getParam #-}


------------------------------------------------------------------------------
getPostParam :: MonadSnap m => ByteString -> m (Maybe ByteString)
getPostParam = liftSnap . S.getPostParam
{-# INLINE getPostParam #-}


------------------------------------------------------------------------------
getQueryParam :: MonadSnap m => ByteString -> m (Maybe ByteString)
getQueryParam = liftSnap . S.getQueryParam
{-# INLINE getQueryParam #-}


------------------------------------------------------------------------------
getParams :: MonadSnap m => m Params
getParams = liftSnap S.getParams


------------------------------------------------------------------------------
getPostParams :: MonadSnap m => m Params
getPostParams = liftSnap S.getPostParams


------------------------------------------------------------------------------
getQueryParams :: MonadSnap m => m Params
getQueryParams = liftSnap S.getQueryParams


------------------------------------------------------------------------------
expireCookie :: MonadSnap m => Cookie -> m ()
expireCookie = liftSnap . S.expireCookie


------------------------------------------------------------------------------
getCookie :: MonadSnap m => ByteString -> m (Maybe Cookie)
getCookie = liftSnap . S.getCookie


------------------------------------------------------------------------------
readCookie :: (MonadSnap m, Readable a) => ByteString -> m a
readCookie = liftSnap . S.readCookie


------------------------------------------------------------------------------
redirect :: MonadSnap m => ByteString -> m a
redirect = liftSnap . S.redirect


------------------------------------------------------------------------------
redirect' :: MonadSnap m => ByteString -> Int -> m a
redirect' = (liftSnap .) . S.redirect'


------------------------------------------------------------------------------
addToOutput :: MonadSnap m
    => (OutputStream Builder -> IO (OutputStream Builder)) -> m ()
addToOutput = liftSnap . S.addToOutput


------------------------------------------------------------------------------
writeBuilder :: MonadSnap m => Builder -> m ()
writeBuilder = liftSnap . S.writeBuilder


------------------------------------------------------------------------------
writeBS :: MonadSnap m => ByteString -> m ()
writeBS = liftSnap . S.writeBS


------------------------------------------------------------------------------
writeLBS :: MonadSnap m => L.ByteString -> m ()
writeLBS = liftSnap . S.writeLBS


------------------------------------------------------------------------------
writeLazyText :: MonadSnap m => L.Text -> m ()
writeLazyText = liftSnap . S.writeLazyText


------------------------------------------------------------------------------
writeText :: MonadSnap m => Text -> m ()
writeText = liftSnap . S.writeText


------------------------------------------------------------------------------
sendFile :: MonadSnap m => FilePath -> m ()
sendFile = liftSnap . S.sendFile


------------------------------------------------------------------------------
sendFilePartial :: MonadSnap m => FilePath -> (Word64, Word64) -> m ()
sendFilePartial = (liftSnap .) . S.sendFilePartial


------------------------------------------------------------------------------
setTimeout :: MonadSnap m => Int -> m ()
setTimeout = liftSnap . S.setTimeout


------------------------------------------------------------------------------
extendTimeout :: MonadSnap m => Int -> m ()
extendTimeout = liftSnap . S.extendTimeout


------------------------------------------------------------------------------
modifyTimeout :: MonadSnap m => (Int -> Int) -> m ()
modifyTimeout = liftSnap . S.modifyTimeout


------------------------------------------------------------------------------
getTimeoutModifier :: MonadSnap m => m ((Int -> Int) -> IO ())
getTimeoutModifier = liftSnap S.getTimeoutModifier
