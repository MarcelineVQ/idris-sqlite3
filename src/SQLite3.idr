module SQLite3

import SQLite3.Internal.Primitive
import SQLite3.Internal.Types

import Data.Primitives.Views

import System.FFI

hexDigit : Int -> Char
hexDigit 0 = '0'
hexDigit 1 = '1'
hexDigit 2 = '2'
hexDigit 3 = '3'
hexDigit 4 = '4'
hexDigit 5 = '5'
hexDigit 6 = '6'
hexDigit 7 = '7'
hexDigit 8 = '8'
hexDigit 9 = '9'
hexDigit 10 = 'a'
hexDigit 11 = 'b'
hexDigit 12 = 'c'
hexDigit 13 = 'd'
hexDigit 14 = 'e'
hexDigit 15 = 'f'
hexDigit _ = 'X' -- TMP HACK: Ideally we'd have a bounds proof, generated below

||| Convert a positive integer into a list of (lower case) hexadecimal characters
export
asHex : Int -> String
asHex n =
  if n > 0
    then pack $ asHex' n []
    else "0"
  where
    asHex' : Int -> List Char -> List Char
    asHex' 0 hex = hex
    asHex' n hex with (n `divides` 16)
      asHex' (16 * div + rem) hex | DivBy div rem _ =
        asHex' (assert_smaller n div) (hexDigit rem :: hex)


sql : String
sql    = """
         DROP TABLE IF EXISTS Cars;
         CREATE TABLE Cars(Id INT, Name TEXT, Price INT); 
         INSERT INTO Cars VALUES(1, 'Audi', 52642); 
         INSERT INTO Cars VALUES(2, 'Mercedes', 57127); 
         INSERT INTO Cars VALUES(3, 'Skoda', 9000); 
         INSERT INTO Cars VALUES(4, 'Volvo', 29000); 
         INSERT INTO Cars VALUES(5, 'Bentley', 350000); 
         INSERT INTO Cars VALUES(6, 'Citroen', 21000); 
         INSERT INTO Cars VALUES(7, 'Hummer', 41400); 
         INSERT INTO Cars VALUES(8, 'Volkswagen', 21600);
         """

sqliteOpen : String -> IO (Either SqlResult DB)
sqliteOpen fn = do
    dbptr <- newPtr
    res <- primIO $ sqlite_open fn dbptr
    db' <- newPtr {t=DBPtr}
    db <- dereference dbptr
    -- putStrLn . ("1a " ++) =<< (primIO $ sqlite3_errmsg db) -- should not say bad
    -- putStrLn . ("1b " ++) . show =<< (primIO $ sqlite3_extended_errcode db)
    -- printLn $ cast {to=Bits32} {from=Bits64} $ believe_me db
    pure $ if fromInt res == SQLITE_OK
      then Right (MkDB dbptr db)
      else Left (fromInt res)

sqliteClose : DB -> IO SqlResult
sqliteClose (MkDB db_ptr db) = do
  res <- primIO (sqlite_close db) -- close frees db
  primIO (ptr_free (prim__forgetPtr db_ptr)) -- could use GCPtr instead
  pure $ fromInt res

sqlitePrepare : DB -> String -> IO (Either SqlResult Stmt)
sqlitePrepare (MkDB _ db) s = do
    stmt_ptr <- newPtr
    ns <- primIO $ null
    let nas = prim__castPtr ns
    res <- primIO $ sqlite_prepare db s (-1) stmt_ptr nas
    let response = fromInt res
    stmt' <- newPtr {t=StmtPtr}
    stmt <- dereference stmt_ptr
    pure $ if response == SQLITE_OK
      then Right (MkStmt stmt_ptr stmt)
      else Left response


sqliteFinalize : Stmt -> IO SqlResult
sqliteFinalize (MkStmt stmt_ptr stmt) = do
    res <- primIO (sqlite3_finalize stmt) -- finalize frees stmt
    primIO (ptr_free (prim__forgetPtr stmt_ptr)) -- could use GCPtr instead
    pure $ fromInt res

sqliteStep : Stmt -> IO (Either SqlResult StepResult)
sqliteStep (MkStmt _ stmt) = do
  result <- primIO $ sqlite_step stmt
  let res = unsafeFromCode result
  case sqlRestoStepRes res of
    Nothing => pure (Left res)
    Just r => pure (Right r)



-- it's looking like I want a Cmd type to ensure things are set up properly and ready for steps



main : IO ()
main = do
    primIO $ sqlite_ver
    Right db@(MkDB dbptr db') <- sqliteOpen "test.db"
      | Left l => printLn "Meh1"
    -- Right res <- sqlitePrepare db sql
    -- Right res <- sqlitePrepare db "SELECT	1 + 1; SELECT	1 + 1;"
    Right stmt <- sqlitePrepare db "SELECT	1 + 1;"
      | Left l => putStrLn =<< sqlite3ErrMsg db
    Right res <- sqliteStep stmt
      | Left l => putStrLn =<< sqlite3ErrMsg db

    _ <- sqliteFinalize stmt
    _ <- sqliteClose db
    printLn "wa"



--