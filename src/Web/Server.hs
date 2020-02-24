module Web.Server
  ( application
  )
where

import           Env
import qualified Web.GraphQL                   as GQL

import           Control.Arrow
import           Control.Applicative
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( lift
                                                , asks
                                                )

import           Data.Text.Lazy                 ( Text )

import           Network.Wai                    ( Application )
import           Network.HTTP.Types
import           Web.Scotty.Trans

-- Routes
----------------------------------------------------------------------

application :: AppContext -> IO Application
application ctx@(_, config, _) = scottyAppT (runEnvIO ctx) router

router :: ScottyT Text EnvM ()
router = do
  get "/api" $ do
    env <- lift $ asks env
    case env of
      Just Production -> sendError status404
      _               -> file "public/playground.html"

  post "/api" $ do
    response    <- GQL.api <$> (lift $ asks conn) <*> body
    rawResponse <- liftIO response
    setHeader "Content-Type" "application/json; charset=utf-8"
    status status200
    raw rawResponse

  notFound $ sendError status404

-- Helpers
----------------------------------------------------------------------

sendError :: (ScottyError e, Monad m) => Status -> ActionT e m ()
sendError s = status s >> sendErrorMessage s

sendErrorMessage status404 = text "Not found"
