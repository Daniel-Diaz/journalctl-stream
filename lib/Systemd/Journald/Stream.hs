
module Systemd.Journald.Stream (
    -- * Journal entry
    JournalEntry (..)
  ) where

import System.Posix.Types (ProcessID)

-- | A journal entry.
data JournalEntry = JournalEntry
  { -- | Process ID.
    entryPID :: ProcessID
    -- | The name of the originating host.
  , entryHostname :: Text
    }
