module Application (app) where

import Control.Monad.Trans.Except
import Config

app :: IO ()
app = do
    init <- runExceptT initialize
    case init of
        Left err -> putStrLn err
        Right config -> print config
