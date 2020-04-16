module SQLite3.Internal.Primitive

%cg chez "libsqlite3"
%cg chez "libsqlite.so"

-- | Backend Wrappers
export
libsqlite3 : String -> String
libsqlite3 fn = "C:" ++ fn ++ ",libsqlite3"

export
idris_sqlite : String -> String
idris_sqlite fn = "C:" ++ fn ++ ",idris_sqlite"

-- | Types

export
data SqlPtr : Type where

public export
data SQLite : Type where
  SQLiteH : (h : Ptr SqlPtr) -> SQLite

export
data StmtPtr : Type where

public export
data Stmnt : Type where
  StmntH : (h : Ptr StmtPtr) -> Stmnt

-- | IO Primitives
export
%foreign (idris_sqlite "sqlite_db_handle")
sqlite_db_handle : PrimIO (Ptr SqlPtr)

export
%foreign (idris_sqlite "sqlite_stmt_handle")
sqlite_stmt_handle : PrimIO (Ptr StmtPtr)

export
%foreign (libsqlite3 "sqlite3_open")
sqlite_open : String -> Ptr SqlPtr -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_close")
sqlite_close : Ptr SqlPtr -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_prepare_v2")
sqlite_prepare : Ptr SqlPtr -> String -> Int -> Ptr StmtPtr -> Ptr String -> PrimIO Int


-- | Non-IO Primitives

export
%foreign (libsqlite3 "sqlite3_errstr")
sqlite3_errstr : Int -> String

export
%foreign (idris_sqlite "null")
null : Ptr a


--