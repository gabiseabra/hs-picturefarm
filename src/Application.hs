module Application
  ( main
  )
where

import           Env
import           GraphQL

import           Control.Applicative
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Except
import           Control.Monad.Reader           ( lift
                                                , asks
                                                )

import           Data.Text.Lazy                 ( Text )
import           Data.Pool                      ( withResource )

import           Network.HTTP.Types
import           Web.Scotty.Trans

main :: IO ()
main = initialize >>= runServer

application :: ScottyT Text EnvM ()
application = do
  get "/api" $ file "public/playground.html"

  post "/api" $ do
    response    <- api <$> (lift $ asks conn) <*> body
    rawResponse <- liftIO response
    setHeader "Content-Type" "application/json; charset=utf-8"
    status status200
    raw rawResponse

  notFound $ do
    status status404
    text "Not found"

runServer :: AppContext -> IO ()
runServer ctx@(_, config, _) = scottyT (port config) (runEnvIO ctx) application
