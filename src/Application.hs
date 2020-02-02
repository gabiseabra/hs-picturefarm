module Application
  ( main
  )
where

import           Env
import           Model.Picture

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Data.Text.Lazy                 ( Text )
import           Data.Pool                      ( withResource )

import           Network.HTTP.Types
import           Web.Scotty.Trans

main :: IO ()
main = initialize >>= runServer

application :: ScottyT Text EnvM ()
application = do
  get "/" $ text "hello"

  notFound $ do
    status status404
    text "Not found"

runServer :: AppContext -> IO ()
runServer ctx@(config, _) = scottyT (port config) (runEnvIO ctx) application
