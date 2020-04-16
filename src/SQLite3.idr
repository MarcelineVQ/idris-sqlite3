module SQLite3

import SQLite3.Internal.Primitive
import SQLite3.Internal.Types

sqliteOpen : String -> IO (Either SqlResult SQLite)
sqliteOpen fn = do
    ptr <- primIO $ sqlite_db_handle
    res <- primIO $ sqlite_open fn ptr
    pure $ if res == 0
      then Right (SQLiteH ptr)
      else Left (fromInt res)

sqliteClose : SQLite -> IO SqlResult
sqliteClose (SQLiteH ptr) = fromInt <$> primIO (sqlite_close ptr)

sqlitePrepare : SQLite -> String -> IO (Either SqlResult Stmnt)
sqlitePrepare (SQLiteH db) s = do
    st <- primIO $ sqlite_stmt_handle
    res <- primIO $ sqlite_prepare db s 1 st null
    pure $ if fromInt res == SQLITE_OK
      then Right (StmntH st)
      else Left (fromInt res)

test : IO ()
test = do
    Right db <- sqliteOpen ":memory:"
      | Left l => printLn "Meh"
    g <- sqliteClose db
    printLn "wa"

--