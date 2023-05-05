----------------------------------------------------------------------------------------------------

-- | Streaming interface to journalctl. Use 'entryStream' to stream
--   journalctl entries as they are created.
--
--   Designed with qualified import in mind.
--   For example, if you import it as @Journal@, then 'Entry' becomes
--   @Journal.Entry@, and 'Exception' becomes @Journal.Exception@.
--
module Systemd.Journalctl.Stream (
    -- * Journal entry
    Entry (..)
  , Cursor
    -- * Streaming
  , entryStream
    -- * Exceptions
  , Exception
  ) where

-- base
import System.IO (Handle)
import Data.Maybe (fromJust)
import Control.Exception qualified as Base
import System.Posix.Types (CPid (..), ProcessID)
-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
-- aeson
import Data.Aeson (FromJSON, (.:), (.:?))
import Data.Aeson qualified as JSON
-- time
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
-- process
import System.Process qualified as System
-- conduit
import Conduit (MonadResource, MonadThrow, throwM)
import Data.Conduit (ConduitT, (.|))
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
    -- | Namespace identifier.
  , entryNamespace :: Maybe Text
    -- | Process name.
  , entryProcess :: Text
    -- | File path to the executable.
  , entryExecutable :: Maybe FilePath
    -- | The cursor for the entry.
  , entryCursor :: Cursor
    -- | The time the entry was received by the journal.
  , entryTimestamp :: POSIXTime
    -- | Unit name, if present.
  , entryUnit :: Maybe Text
    -- | Entry message.
  , entryMessage :: Text
    } deriving Show

-- | Utility type to parse values (mainly numbers) that are received
--   as text.
newtype AsText a = AsText { asText :: a } deriving Show

instance FromJSON a => FromJSON (AsText a) where
  parseJSON = JSON.withText "AsText" $
    either fail (pure . AsText) . JSON.eitherDecodeStrict . encodeUtf8

{- Journal fields

For a more complete list of fields and documentation, go to:

https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html

-}

instance FromJSON Entry where
  parseJSON = JSON.withObject "Entry" $ \o -> Entry
    <$> (CPid . asText <$> o .: "_PID")
    <*> o .: "_HOSTNAME"
    <*> o .:? "_NAMESPACE"
    <*> o .: "_COMM"
    <*> o .:? "_EXE"
    <*> o .: "__CURSOR"
    <*> (secondsToNominalDiffTime . (/1000000) . asText <$> o .: "__REALTIME_TIMESTAMP")
    <*> o .:? "UNIT"
    <*> o .: "MESSAGE"

-- | Exception raised while streaming entries from journalctl.
data Exception = JSONError String deriving Show

instance Base.Exception Exception where

-- | Stream of journal entries.
entryStream
  :: (MonadResource m, MonadThrow m)
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
      hdl = fmap (\(_, h, _, _) -> fromJust h)
          $ System.createProcess
          $ (System.proc "journalctl" args)
              { System.std_out = System.CreatePipe
                }
  in  Conduit.sourceIOHandle hdl
        .| Conduit.linesUnboundedAscii
        .| Conduit.mapM (either (throwM . JSONError) pure . JSON.eitherDecodeStrict)
