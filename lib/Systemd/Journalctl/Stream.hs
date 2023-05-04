----------------------------------------------------------------------------------------------------
module Systemd.Journalctl.Stream (
    -- * Journal entry
    Entry (..)
  , Cursor
    -- * Streaming
  , entryStream
  ) where

-- base
import System.IO (Handle)
-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
-- unix
import System.Posix.Types (CPid (..), ProcessID)
-- aeson
import Data.Aeson (FromJSON, (.:), (.:?))
import Data.Aeson qualified as JSON
-- time
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
-- process
import System.Process qualified as System
-- conduit
import Conduit (MonadResource)
import Data.Conduit (ConduitT, (.|), runConduitRes)
import Data.Conduit.Combinators qualified as Conduit

-- | A cursor is an opaque text string that uniquely describes
--   the position of an entry in the journal and is portable
--   across machines, platforms and journal files.
newtype Cursor = Cursor Text deriving (Eq, Show)

instance FromJSON Cursor where
  parseJSON = JSON.withText "Cursor" $ pure . Cursor

-- | A journal entry.
data Entry = Entry
  { -- | Process ID.
    entryPID :: ProcessID
    -- | The name of the originating host.
  , entryHostname :: Text
    -- | The cursor for the entry.
  , entryCursor :: Cursor
    -- | The time the entry was received by the journal.
  , entryTimestamp :: POSIXTime
    -- | Unit name.
  , entryUnit :: Maybe Text
    -- | Entry message.
  , entryMessage :: Text
    } deriving Show

newtype AsText a = AsText { asText :: a } deriving Show

instance FromJSON a => FromJSON (AsText a) where
  parseJSON = JSON.withText "AsText" $
    either fail (pure . AsText) . JSON.eitherDecodeStrict . encodeUtf8

instance FromJSON Entry where
  parseJSON = JSON.withObject "Entry" $ \o -> Entry
    <$> (CPid . asText <$> o .: "_PID")
    <*> o .: "_HOSTNAME"
    <*> o .: "__CURSOR"
    <*> (secondsToNominalDiffTime . (/1000000) . asText <$> o .: "__REALTIME_TIMESTAMP")
    <*> o .:? "UNIT"
    <*> o .: "MESSAGE"

-- | Stream of journal entries.
entryStream
  :: (MonadResource m, MonadFail m)
  => Maybe String -- ^ Filter by unit name.
  -> Int -- ^ Number of previous messages to stream.
  -> ConduitT i Entry m ()
entryStream munit n =
  let args :: [String]
      args =
        [ "--follow"
        , "--lines"
        , show n
        , "--output"
        , "json"
          ] ++
        (maybe [] (\unit -> ["--unit", unit]) munit)
      hdl :: IO Handle
      hdl = fmap (\(_, Just h, _, _) -> h)
          $ System.createProcess
          $ (System.proc "journalctl" args)
              { System.std_out = System.CreatePipe
                }
  in  Conduit.sourceIOHandle hdl
        .| Conduit.linesUnboundedAscii
        .| Conduit.mapM (either fail pure . JSON.eitherDecodeStrict)
