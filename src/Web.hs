module Web
  ( main
  )
where

import           Env                            ( Config(..)
                                                , initialize
                                                )
import           Web.Router                     ( application )

import           Control.Exception              ( SomeException
                                                , catch
                                                )

import           Network.Wai.Handler.Warp

main :: IO ()
main = do
  ctx@(_, config, _) <- initialize
  app                <- application ctx
  print ("Starting server on http://localhost:" ++ show (port config))
  runSettings (warpSettings config) app

onException _req e = print ("Error: " ++ show e)

warpSettings :: Config -> Settings
warpSettings config@Config { port } =
  setPort port $ setOnException onException defaultSettings
