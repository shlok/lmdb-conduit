--------------------------------------------------------------------------------

module Database.LMDB.Conduit (sinkLMDB, sourceLMDB) where

--------------------------------------------------------------------------------

import Conduit (ConduitT, awaitForever, yield)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Database.LMDB.Raw (MDB_dbi', MDB_env)
import Database.LMDB.Resource (readLMDB, writeLMDB)

--------------------------------------------------------------------------------

-- | A read transaction is kept open for the duration of the stream.
-- Bear in mind LMDB's caveats regarding long-lived transactions.
sourceLMDB :: (MonadResource m) => MDB_env -> MDB_dbi' -> ConduitT i (ByteString, ByteString) m ()
sourceLMDB env dbi = readLMDB env dbi yield

--------------------------------------------------------------------------------

-- | A write transaction is kept open for the duration of the stream.
-- Bear in mind LMDB's caveats regarding long-lived transactions.
sinkLMDB :: (MonadResource m)
         => MDB_env
         -> MDB_dbi'
         -> Bool     -- ^ If 'True', an exception will be thrown when attempting to re-insert a key.
         -> ConduitT (ByteString, ByteString) o m ()
sinkLMDB env dbi noOverwrite = writeLMDB env dbi noOverwrite awaitForever

--------------------------------------------------------------------------------
