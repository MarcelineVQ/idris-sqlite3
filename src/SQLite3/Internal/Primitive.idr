module SQLite3.Internal.Primitive

-- I'm trying to use what I can from libsqlite directly and wrap what I have to
-- This is so that it'd be painless to plug a different binding for sqlite
-- In the case that the backend we build with changes

-- | Backend Wrappers
export
idris_sqlite : String -> String
idris_sqlite fn = "C:" ++ fn ++ ",idris_sqlite"

-- | Types

--------------------------------------------------

export
data DBPtr : Type where

public export
data DB : Type where
  MkDB : (db : Ptr DBPtr) -> DB

--------------------------------------------------

export
data StmtPtr : Type where

public export
data Stmt : Type where
  MkStmt : (stmt : Ptr StmtPtr) -> Stmt

--------------------------------------------------

-- | IO Primitives

export
%foreign (idris_sqlite "newptr")
newptr : PrimIO AnyPtr

export
%foreign (idris_sqlite "ptr_free")
ptr_free : AnyPtr -> PrimIO ()

export
ptrFree : HasIO io => Ptr t -> io ()
ptrFree ptr = primIO $ ptr_free $ prim__forgetPtr ptr

export
%foreign (idris_sqlite "null")
null : PrimIO AnyPtr

export
%foreign (idris_sqlite "deref")
deref : Ptr AnyPtr -> PrimIO AnyPtr

--------------------------------------------------

export
%foreign (idris_sqlite "sqlver")
sqlite_ver : PrimIO ()

export
%foreign (idris_sqlite "sqlite3_free")
sqlite3_free : AnyPtr -> PrimIO AnyPtr

export
%foreign (idris_sqlite "sqlite3_malloc")
sqlite3_malloc : Int -> PrimIO AnyPtr

export
%foreign (idris_sqlite "sqlite3_open")
sqlite_open : String -> Ptr (Ptr DBPtr) -> PrimIO Int

-- open takes an sqlite3**
export
%foreign (idris_sqlite "sqlite3_open_v2")
sqlite_open_v2 : String -> Ptr (Ptr DBPtr) -> Int -> String -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_close")
sqlite_close : Ptr DBPtr -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_prepare_v2")
sqlite_prepare : Ptr DBPtr -> String -> Int -> Ptr (Ptr StmtPtr) -> Ptr (Ptr String) -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_step")
sqlite_step : Ptr StmtPtr -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_exec")
sqlite_exec : Ptr DBPtr -> String -> AnyPtr -> AnyPtr -> Ptr String -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_finalize")
sqlite3_finalize : Ptr StmtPtr -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_extended_errcode")
sqlite3_extended_errcode : Ptr DBPtr -> PrimIO Int

export
%foreign (idris_sqlite "sqlite3_errmsg")
sqlite3_errmsg : Ptr DBPtr -> PrimIO String

export
%foreign (idris_sqlite "sqlite3_errstr")
sqlite3_errstr : Int -> PrimIO String

export
sqlite3ErrMsg : HasIO io => DB -> io String
sqlite3ErrMsg (MkDB db) = primIO $ sqlite3_errmsg db

--------------------------------------------------
-- Column Functions
-- https://www.sqlite.org/c3ref/column_blob.html
--------------------------------------------------

export
%foreign (idris_sqlite "sqlite3_column_text")
sqlite3_column_text : Ptr StmtPtr -> (iCol : Int) -> PrimIO String

export
sqlite3ColumnText : HasIO io => Stmt -> (iCol : Int) -> io String
sqlite3ColumnText (MkStmt stmt) i = primIO $ sqlite3_column_text stmt i

export
%foreign (idris_sqlite "sqlite3_column_double")
sqlite3_column_double : Ptr StmtPtr -> (iCol : Int) -> PrimIO Double

export
sqlite3ColumnDouble : HasIO io => Stmt -> (iCol : Int) -> io Double
sqlite3ColumnDouble (MkStmt stmt) i = primIO $ sqlite3_column_double stmt i

export
%foreign (idris_sqlite "sqlite3_column_int")
sqlite3_column_int : Ptr StmtPtr -> (iCol : Int) -> PrimIO Int

export
sqlite3ColumnInt : HasIO io => Stmt -> (iCol : Int) -> io Int
sqlite3ColumnInt (MkStmt stmt) i = primIO $ sqlite3_column_int stmt i

-- double sqlite3_column_double(sqlite3_stmt*, int iCol);

--------------------------------------------------
-- Binding Functions
-- https://www.sqlite.org/c3ref/bind_blob.html
--------------------------------------------------
-- Probably not gonna bother, we can assemble strigs programatically

export
%foreign (idris_sqlite "sqlite3_bind_text")
sqlite3_bind_text : Ptr StmtPtr -> (iArg : Int) -> String -> Int -> AnyPtr -> PrimIO Int

-- int sqlite3_bind_text(sqlite3_stmt*,int,const char*,int,void(*)(void*));

--------------------------------------------------


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
strToPtr : HasIO io => String -> io (Ptr String)
strToPtr x = primIO $ mkString x

export
%foreign (idris_sqlite "getString")
getString : Ptr String -> PrimIO String

export
ptrToStr : HasIO io => Ptr String -> io String
ptrToStr x = primIO $ getString x


-- | Non-IO Primitives

-- export
-- %foreign (idris_sqlite "sqlite3_errstr")
-- sqlite3_errstr : Int -> PrimIO String



-- export
-- nullStr : Ptr String
-- nullStr = unsafePerformIO $ primIO $ nullStr'

export
nullPtr : Ptr t
nullPtr = prim__castPtr $ unsafePerformIO $ primIO $ null

--