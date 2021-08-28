module SQLite3.Internal.Primitive

-- I'm trying to use what I can from libsqlite directly and wrap what I have to
-- This is so that it'd be painless to plug a different binding for sqlite
-- In the case that the backend we build with changes

-- | Backend Wrappers
export
libsqlite3 : String -> String
libsqlite3 fn = "C:" ++ fn ++ ",libsqlite3"

export
idris_sqlite : String -> String
idris_sqlite fn = "C:" ++ fn ++ ",idris_sqlite"

-- | Types

--------------------------------------------------

export
data DBPtr : Type where

-- things like sqlite3_open take a pointer to a sqlite3*, not a sqlite3*
-- So we track both
public export
data DB : Type where
  MkDB : (db_ptr : Ptr (Ptr DBPtr)) -> (db : Ptr DBPtr) -> DB

--------------------------------------------------

export
data StmtPtr : Type where

public export
data Stmt : Type where
  MkStmt : (stmt_ptr : Ptr (Ptr StmtPtr)) -> (stmt : Ptr StmtPtr) -> Stmt

--------------------------------------------------

-- | IO Primitives

export
%foreign (idris_sqlite "newptr")
newptr : PrimIO AnyPtr

export
%foreign (idris_sqlite "ptr_free")
ptr_free : AnyPtr -> PrimIO ()

export
%foreign (idris_sqlite "null")
null : PrimIO AnyPtr

export
%foreign (libsqlite3 "sqlite3_free")
sqlite3_free : AnyPtr -> PrimIO AnyPtr

export
%foreign (libsqlite3 "sqlite3_malloc")
sqlite3_malloc : Int -> PrimIO AnyPtr


export
%foreign (libsqlite3 "sqlite3_open")
sqlite_open : String -> Ptr (Ptr DBPtr) -> PrimIO Int

export
%foreign (idris_sqlite "sqlver")
sqlite_ver : PrimIO ()

export
%foreign (idris_sqlite "deref")
deref : Ptr AnyPtr -> PrimIO AnyPtr

-- open takes an sqlite3**
export
%foreign (libsqlite3 "sqlite3_open_v2")
sqlite_open_v2 : String -> Ptr (Ptr DBPtr) -> Int -> String -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_close")
sqlite_close : Ptr DBPtr -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_prepare_v2")
sqlite_prepare : Ptr DBPtr -> String -> Int -> Ptr (Ptr StmtPtr) -> Ptr String -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_prepare_v2")
sqlite_step : Ptr StmtPtr -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_exec")
sqlite_exec : Ptr DBPtr -> String -> AnyPtr -> AnyPtr -> Ptr String -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_finalize")
sqlite3_finalize : Ptr StmtPtr -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_extended_errcode")
sqlite3_extended_errcode : Ptr DBPtr -> PrimIO Int

export
%foreign (libsqlite3 "sqlite3_errmsg")
sqlite3_errmsg : Ptr DBPtr -> PrimIO String

export
sqlite3ErrMsg : HasIO io => DB -> io String
sqlite3ErrMsg (MkDB _ db) = primIO $ sqlite3_errmsg db

export
%foreign (libsqlite3 "sqlite3_errstr")
sqlite3_errstr : Int -> PrimIO String

export
newAnyPtr : HasIO io => io AnyPtr
newAnyPtr = primIO newptr

export
newPtr : HasIO io => io (Ptr t)
newPtr = prim__castPtr <$> newAnyPtr

export
dereference : HasIO io => Ptr (Ptr t) -> io (Ptr t)
dereference ptr =
  prim__castPtr <$> (primIO $ deref (believe_me ptr))


export
%foreign (idris_sqlite "mkString")
mkString : String -> PrimIO (Ptr String)

export
%foreign (idris_sqlite "getString")
getString : Ptr String -> PrimIO String


-- | Non-IO Primitives

-- export
-- %foreign (libsqlite3 "sqlite3_errstr")
-- sqlite3_errstr : Int -> PrimIO String

export
%foreign (idris_sqlite "null")
nullStr' : PrimIO (Ptr String)

export
nullStr : Ptr String
nullStr = unsafePerformIO $ primIO $ nullStr'

--