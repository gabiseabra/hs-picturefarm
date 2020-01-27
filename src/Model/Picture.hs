module Model.Picture (
  Picture,
  getByUuid
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.UUID

import qualified Database.PostgreSQL.Simple              as TQ
import qualified Database.PostgreSQL.Simple.TypedQuery   as TQ
import Data.String.QM

data Picture = Picture {
  uuid     :: UUID,
  fileName :: Text,
  fileHash :: Text,
  url      :: Text,
  mimeType :: Text
} deriving (Show)

instance FromJSON Picture where
  parseJSON (Object v) = do
    uuid      <-  v .: "uuid"
    fileName  <-  v .: "file_name"
    fileHash  <-  v .: "file_hash"
    url       <-  v .: "url"
    mimeType  <-  v .: "mime_type"
    return (Picture uuid fileName fileHash url mimeType)

  parseJSON _ = empty

-- Queries
--------------------

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

getByUuid :: Text -> TQ.Connection -> IO (Maybe (Result Picture))
getByUuid uuid conn = do
  safeHead <$> $(TQ.genJsonQuery [qq|
    select uuid      as uuid       -- UUID
         , file_name as file_name  -- Text
         , url       as url        -- Text
         , mime_type as mime_type  -- Text
         , file_hash as file_hash  -- Text
    from pictures
    where uuid = ?                 -- < uuid
  |]) conn >>= return . liftM fromJSON
