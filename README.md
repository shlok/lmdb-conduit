# lmdb-conduit

Stream data to or from an LMDB database using Haskell.

## First steps

A common practice with LMDB is to create an environment and database handle and simply leave them open for the remainder of your program’s execution. This is how you do it with the [lmdb](https://hackage.haskell.org/package/lmdb) library:

```haskell
import Database.LMDB.Raw (MDB_dbi', MDB_env, mdb_dbi_open',
                          mdb_env_create, mdb_env_open,
                          mdb_env_set_mapsize, mdb_txn_begin,
                          mdb_txn_commit)

main :: IO ()
main = do
    -- Create an environment.
    env <- mdb_env_create

    -- The map size needs to be set. If you think 1 TiB will
    -- be more than enough, you can specify it like this.
    mdb_env_set_mapsize env (1024 * 1024 * 1024 * 1024)

    -- Open the environment. We are here assuming an existing directory
    -- that is either empty or already contains an LMDB database.
    mdb_env_open env "/somewhere/great-lmdb-dir" []

    -- Obtain the database handle.
    txn <- mdb_txn_begin env Nothing True
    dbi <- mdb_dbi_open' txn Nothing []
    mdb_txn_commit txn
```

Congratulations. You now have `env` (of type `MDB_env`) and `dbi` (of type `MDB_dbi'`) that you can use with the functions provided by this library.

## Using this library

If you are familiar with the [conduit](https://hackage.haskell.org/package/conduit) library, you will feel right at home. The following functions, located in `Database.LMDB.Conduit`, allow you to stream key-value pairs to and from an LMDB database:

```haskell
import Conduit (ConduitT)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Database.LMDB.Raw (MDB_dbi', MDB_env)

sourceLMDB :: (MonadResource m)
           => MDB_env
           -> MDB_dbi'
           -> ConduitT i (ByteString, ByteString) m ()

sinkLMDB :: (MonadResource m)
         => MDB_env
         -> MDB_dbi'
         -> Bool     -- If True, an exception will be thrown when attempting to re-insert a key.
         -> ConduitT (ByteString, ByteString) o m ()
```

Do not supply empty keys in `sinkLMDB`; this is not allowed by LMDB. Note also that an LMDB transaction (read-only for `sourceLMDB` and read-write for `sinkLMDB`) is kept open for the duration of the stream; so when using this library, you should bear in mind LMDB’s caveats regarding long-lived transactions.
