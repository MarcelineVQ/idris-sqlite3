module SQLite3.Internal.Types

import SQLite3.Internal.Primitive

-- https://www.sqlite.org/c3ref/c_abort.html
public export
data SqlResult : Type where
  SQLITE_OK         : SqlResult -- Successful result 
  SQLITE_ERROR      : SqlResult -- Generic error 
  SQLITE_INTERNAL   : SqlResult -- Internal logic error in SQLite 
  SQLITE_PERM       : SqlResult -- Access permission denied 
  SQLITE_ABORT      : SqlResult -- Callback routine requested an abort 
  SQLITE_BUSY       : SqlResult -- The database file is locked 
  SQLITE_LOCKED     : SqlResult -- A table in the database is locked 
  SQLITE_NOMEM      : SqlResult -- A malloc() failed 
  SQLITE_READONLY   : SqlResult -- Attempt to write a readonly database 
  SQLITE_INTERRUPT  : SqlResult -- Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      : SqlResult -- Some kind of disk I/O error occurred 
  SQLITE_CORRUPT    : SqlResult -- The database disk image is malformed 
  SQLITE_NOTFOUND   : SqlResult -- Unknown opcode in sqlite3_file_control() 
  SQLITE_FULL       : SqlResult -- Insertion failed because database is full 
  SQLITE_CANTOPEN   : SqlResult -- Unable to open the database file 
  SQLITE_PROTOCOL   : SqlResult -- Database lock protocol error 
  SQLITE_EMPTY      : SqlResult -- Internal use only 
  SQLITE_SCHEMA     : SqlResult -- The database schema changed 
  SQLITE_TOOBIG     : SqlResult -- String or BLOB exceeds size limit 
  SQLITE_CONSTRAINT : SqlResult -- Abort due to constraint violation 
  SQLITE_MISMATCH   : SqlResult -- Data type mismatch 
  SQLITE_MISUSE     : SqlResult -- Library used incorrectly 
  SQLITE_NOLFS      : SqlResult -- Uses OS features not supported on host 
  SQLITE_AUTH       : SqlResult -- Authorization denied 
  SQLITE_FORMAT     : SqlResult -- Not used 
  SQLITE_RANGE      : SqlResult -- 2nd parameter to sqlite3_bind out of range 
  SQLITE_NOTADB     : SqlResult -- File opened that is not a database file 
  SQLITE_NOTICE     : SqlResult -- Notifications from sqlite3_log() 
  SQLITE_WARNING    : SqlResult -- Warnings from sqlite3_log() 
  SQLITE_ROW        : SqlResult -- sqlite3_step() has another row ready 
  SQLITE_DONE       : SqlResult -- sqlite3_step() has finished executing 
  Unknown           : SqlResult -- error code unknown

public export
fromInt : Int -> SqlResult
fromInt 0   = SQLITE_OK
fromInt 1   = SQLITE_ERROR
fromInt 2   = SQLITE_INTERNAL
fromInt 3   = SQLITE_PERM
fromInt 4   = SQLITE_ABORT
fromInt 5   = SQLITE_BUSY
fromInt 6   = SQLITE_LOCKED
fromInt 7   = SQLITE_NOMEM
fromInt 8   = SQLITE_READONLY
fromInt 9   = SQLITE_INTERRUPT
fromInt 10  = SQLITE_IOERR
fromInt 11  = SQLITE_CORRUPT
fromInt 12  = SQLITE_NOTFOUND
fromInt 13  = SQLITE_FULL
fromInt 14  = SQLITE_CANTOPEN
fromInt 15  = SQLITE_PROTOCOL
fromInt 16  = SQLITE_EMPTY
fromInt 17  = SQLITE_SCHEMA
fromInt 18  = SQLITE_TOOBIG
fromInt 19  = SQLITE_CONSTRAINT
fromInt 20  = SQLITE_MISMATCH
fromInt 21  = SQLITE_MISUSE
fromInt 22  = SQLITE_NOLFS
fromInt 23  = SQLITE_AUTH
fromInt 24  = SQLITE_FORMAT
fromInt 25  = SQLITE_RANGE
fromInt 26  = SQLITE_NOTADB
fromInt 27  = SQLITE_NOTICE
fromInt 28  = SQLITE_WARNING
fromInt 100 = SQLITE_ROW
fromInt 101 = SQLITE_DONE
fromInt _   = Unknown

public export
toInt : SqlResult -> Int
toInt SQLITE_OK         = 0
toInt SQLITE_ERROR      = 1
toInt SQLITE_INTERNAL   = 2
toInt SQLITE_PERM       = 3
toInt SQLITE_ABORT      = 4
toInt SQLITE_BUSY       = 5
toInt SQLITE_LOCKED     = 6
toInt SQLITE_NOMEM      = 7
toInt SQLITE_READONLY   = 8
toInt SQLITE_INTERRUPT  = 9
toInt SQLITE_IOERR      = 10
toInt SQLITE_CORRUPT    = 11
toInt SQLITE_NOTFOUND   = 12
toInt SQLITE_FULL       = 13
toInt SQLITE_CANTOPEN   = 14
toInt SQLITE_PROTOCOL   = 15
toInt SQLITE_EMPTY      = 16
toInt SQLITE_SCHEMA     = 17
toInt SQLITE_TOOBIG     = 18
toInt SQLITE_CONSTRAINT = 19
toInt SQLITE_MISMATCH   = 20
toInt SQLITE_MISUSE     = 21
toInt SQLITE_NOLFS      = 22
toInt SQLITE_AUTH       = 23
toInt SQLITE_FORMAT     = 24
toInt SQLITE_RANGE      = 25
toInt SQLITE_NOTADB     = 26
toInt SQLITE_NOTICE     = 27
toInt SQLITE_WARNING    = 28
toInt SQLITE_ROW        = 100
toInt SQLITE_DONE       = 101

implementation
Eq SqlResult where
  x == y = toInt x == toInt y

resultString : Int -> String
resultString = sqlite3_errstr


--