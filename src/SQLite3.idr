module SQLite3

import public SQLite3.Internal.Primitive
import public SQLite3.Internal.Types

import Data.Primitives.Views

import System.FFI

import Data.Vect
import Data.List

import Control.Monad.Managed
import Control.Monad.Either
import Control.Monad.Trans

export
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

||| Act with a pointer and free it afterwards
export
withPtr : HasIO io => Ptr a -> (Ptr a -> io b) -> io b
withPtr ptr act = act ptr <* ptrFree ptr

||| Allocate a pointer, act upon it, and free it afterwards
export
withPtrAlloc : HasIO io => (Ptr a -> io b) -> io b
withPtrAlloc act = newPtr >>= (`withPtr` act)

-- export
-- withStmt : String -> (Either SqlResult Stmt -> IO a) -> IO a
-- withStmt fn act = do
      -- Right db <- sqliteOpen fn
        -- | Left l => pure $ Left l
      -- act db <* sqliteClose db

-- main2 : IO ()
-- main2 = withDB "test.db" $ \db => ?sdf

export
sqliteOpen : String -> IO (Either SqlResult DB)
sqliteOpen fn = withPtrAlloc $ \db_ptr => do
    res <- fromInt <$> primIO (sqlite_open fn db_ptr)
    case res of
      SQLITE_OK => pure $ Right (MkDB !(dereference db_ptr))
      r         => pure (Left r)

export
sqliteClose : DB -> IO SqlResult
sqliteClose (MkDB db) = fromInt <$> primIO (sqlite_close db)

export
sqliteFinalize : Stmt -> IO SqlResult
sqliteFinalize (MkStmt stmt) = fromInt <$> primIO (sqlite3_finalize stmt)

export
sqlitePrepare : DB -> String -> IO (Either SqlResult Stmt)
sqlitePrepare (MkDB db) s = withPtrAlloc $ \stmt_ptr => do
    res <- fromInt <$> primIO (sqlite_prepare db s (-1) stmt_ptr nullPtr)
    case res of
      SQLITE_OK => pure $ Right (MkStmt !(dereference stmt_ptr))
      r         => pure (Left r)

-- prep that returns the remainder
export
sqlitePrepare' : DB -> String -> IO (Either SqlResult (Stmt,String))
sqlitePrepare' (MkDB db) s
  = withPtrAlloc $ \stmt_ptr => withPtrAlloc $ \strrem => do
      res <- fromInt <$> primIO (sqlite_prepare db s (-1) stmt_ptr strrem)
      stmt <- dereference stmt_ptr
      rem <- ptrToStr =<< dereference strrem
      case res of
        SQLITE_OK => pure $ Right (MkStmt stmt, rem)
        r         => pure (Left r)
        
export
sqliteStep : Stmt -> IO (Either SqlResult StepResult)
sqliteStep (MkStmt stmt) = do
    result <- primIO $ sqlite_step stmt
    let res = unsafeFromCode result
    pure $ case sqlRestoStepRes res of
      Nothing => Left res
      Just r => Right r

-- RowOf : Nat -> Type
-- RowOf n = Vect n Type

public export
interface FromSqlite t where
  getValue : Stmt -> (i : Int) -> IO t

export
FromSqlite String where
  getValue (MkStmt stmt) icol = primIO $ sqlite3_column_text stmt icol

export
FromSqlite Double where
  getValue (MkStmt stmt) icol = primIO $ sqlite3_column_double stmt icol

export
FromSqlite Int where
  getValue (MkStmt stmt) icol = primIO $ sqlite3_column_int stmt icol

-- FromSqlite t => FromSqlite (List t) where
  -- getValue (MkStmt stmt) icol = ?sdff

-- export
-- hconcat : (ts : Vect (S k) Type) -> Type
-- hconcat [x] = x
-- hconcat (x :: y :: xs) = (x, hconcat (y :: xs))
-- 
-- public export
-- data HList : (con : Type -> Type) -> Vect (S k) Type -> Type where
--   End : (ct : con t) => (x : t) -> HList con [t]
--   (::) : (ct : con t) => (x : t) -> HList con ts -> HList con (t :: ts)
-- 
-- 0 fef : (0 t : _) -> (0 ts : _) -> (t, hconcat ts) = hconcat (t :: ts)
-- fef x (y :: xs) = Refl
-- 
-- go : Stmt -> Int -> (ls : HList FromSqlite ts) -> IO (hconcat ts)
-- go stmt k (End {ct} x) = getValue stmt k
-- go stmt k ((::) {ct} {t} {ts} x xs) = do
--     r <- getValue @{ct} stmt k
--     rs <- go stmt (k+1) xs
--     pure $ rewrite sym (fef t ts) in (r, rs)


-- export
-- hconcat : (ts : Vect (S k) Type) -> Type
-- hconcat [x] = x
-- hconcat (x :: y :: xs) = (x, hconcat (y :: xs))
-- 
-- public export
-- data HList : (k : Type) -> (f : k -> Type) -> Vect (S n) Type -> Type where
--   End : (ct : f t) => (t : Type) -> HList k f [t]
--   (::) : (ct : f t) => (t : Type) -> HList k f ts -> HList k f (t :: ts)
-- 
-- 0 fef : (0 t : _) -> (0 ts : _) -> (t, hconcat ts) = hconcat (t :: ts)
-- fef x (y :: xs) = Refl
-- 
-- go : Stmt -> Int -> (ls : HList Type FromSqlite ts) -> IO (hconcat ts)
-- go stmt k (End {ct} t) = getValue @{ct} stmt k
-- go stmt _ _ = ?dffsdfsd
-- go stmt k ((::) {ct} {t} {ts} x xs) = do
--     r <- getValue @{ct} stmt k
--     rs <- go stmt (k+1) xs
--     pure $ rewrite sym (fef t ts) in (r, rs)

-- vectToHList : {z : Type} -> FromSqlite z => (ts : Vect (S k) z) -> HList FromSqlite ts
-- vectToHList [x] = End {ct = %search} ?sdffd
-- vectToHList (x :: xs) = (::) @{ct = %search} x (vectToHList xs)

-- public export
-- sqliteFetchRows'' : Stmt -> (tss : HList FromSqlite ts) -> IO (hconcat ts)
-- sqliteFetchRows'' stmt xs = go stmt 0 xs
-- 
-- export
-- sqliteFetchRows : Stmt -> (tss : HList FromSqlite ts) -> IO (hconcat ts)
-- sqliteFetchRows stmt xs = go stmt 0 xs
-- 
-- private
-- sqliteFetchRows' : Stmt -> (tss : HList FromSqlite ts) -> IO (hconcat ts)
-- sqliteFetchRows' stmt xs = go stmt 0 xs

-- fraf : Stmt -> IO (Int, Double, String)
-- fraf stmt = sqliteFetchRows stmt [Int, Double, String]

-- sqliteFetchRow stmt ts = runEitherT {m=IO} {e=SqlResult} $ do
--     SRRow <- MkEitherT $ sqliteStep stmt
--       | SRDone => ?dsffdsdsdsff
--       | SRBusy => left SQLITE_BUSY
--       | SRRow => ?dsffdsdseff
--       | SRError => left SQLITE_ERROR
--       | SRMisuse => ?dsffddddsdsf
--     ?sdffds

{-
CREATE TABLE Cars(Id INT, Name TEXT, Price INT);
INSERT INTO Cars VALUES(1, 'Audi', 52642);
-}

-- candleType : 
-- [String,String,Bool,Int,String,Double,String
-- ,Double,Double,Double,Double
-- ,Double,Double,Double,Double
-- ,Double,Double,Double,Double]

-- it's looking like I want a Cmd type to ensure things are set up properly and
-- ready for steps. I probably want to seperate statements by ; and feed them
-- one at a time, similar logic to what I'll need for rows

-- [String,String,Bool,Int,String,Double,String
      -- ,Double,Double,Double,Double
      -- ,Double,Double,Double,Double
      -- ,Double,Double,Double,Double]

data Query : Type -> Type where
  MkQuery : (sql : String) -> Query ts

data Result : Type -> Type where
  MkResult : (res : a) -> Result a

-- given a query, compute the result type and fetch it from the row
-- doQuery : HasIO io =>
--           {ts:_} -> Query ts ->
--           io (Result ts)
-- doQuery (MkQuery str stmt) = do
--     ?sdaSDfsfd

export
withDB : String -> (Either SqlResult DB -> IO a) -> IO a
withDB fn act = do
      res <- sqliteOpen fn
      a <- act res
      either (\_ => pure a) (\db => pure a <* sqliteClose db) res

export
withStmt : DB -> String -> (Either SqlResult Stmt -> IO a) -> IO a
withStmt db sql act = do
      res <- sqlitePrepare db sql
      a <- act res
      either (\_ => pure a) (\stmt => pure a <* sqliteFinalize stmt) res

export
managedPtr : Ptr a -> Managed (Ptr a)
managedPtr ptr = managed $ withPtr ptr

export
managedPtrAlloc : Managed (Ptr a)
managedPtrAlloc = managed $ withPtrAlloc

export
managedDB : String -> Managed (Either SqlResult DB)
managedDB str = managed $ withDB str

export
managedStmt : DB -> String -> Managed (Either SqlResult Stmt)
managedStmt db str = managed $ withStmt db str

export
main2 : IO ()
main2 = runManaged $ do
  ignore $ runEitherT {e=SqlResult} {m=Managed} $ do
    r <- MkEitherT $ use $ managedDB "test.db"
    ?dsfsdf
  pure ()
  -- db <- managedDB "test.db"
  -- ?dsfd

export
main : IO ()
main = withDB "test.db" $ \db => do
    primIO $ sqlite_ver
    Right db@(MkDB db') <- pure db
      | Left l => printLn "Meh1"
    Right (stmt,rem) <- sqlitePrepare' db sql
    -- Right stmt <- sqlitePrepare db sql
    -- Right stmt <- sqlitePrepare db "SELECT	1 + 1; SELECT	1 + 1;"
    -- Right stmt <- sqlitePrepare db "SELECT	1 + 1;"
    -- Right stmt@(MkStmt stmt') <- sqlitePrepare db "SELECT * from 'Cars';"
    -- Right stmt@(MkStmt stmt') <- sqlitePrepare db "SELECT ?1 from Cars;"
      | Left l => putStrLn =<< sqlite3ErrMsg db
    putStrLn rem
    Right res <- sqliteStep stmt
      | Left l => putStrLn =<< sqlite3ErrMsg db
    -- res' <- primIO $ sqlite3_bind_text stmt' 1 "Name" (-1) (prim__forgetPtr nullPtr)
    -- printLn =<< (primIO $ sqlite3_errstr res') -- ^ bad param it says, prob the ptr, check docs for misuse
    -- case res of
      -- SRRow => do printLn =<< (primIO $ sqlite3_column_int stmt' 0)
                  -- printLn =<< (primIO $ sqlite3_column_int stmt' 2)
      -- SRDone => ?sdffd_2b
      -- _ => pure ()

    _ <- sqliteFinalize stmt
    _ <- sqliteClose db
    printLn "wa"


{-

Should query include a list of bound vars
I dont technically need the capacity to bind vars, in that I can just construct the query string with values already in place. I just need retrieval.
Other people would want binding because their data could be harder to stringify

Remember you're not building industrial bindings to sqlite, you're making it work for your need. Storing and retrieving bar data
That said if haskell version is straightforward we may as well do binding too

U want bar data to include, bid, ask, mid, so that we have the most data available as we can in storage. You can train train oracle on mid without issue, but bid vs ask is critical for an ai that needs to learn buying and selling. We also probably want time stored in both forms, though if the conversion is easy then Unix timestamp is straightforward. See if sqlite had an time functions builtin

How to structure candle data in table forms?
Probably just slap it all in a row
bid_high  ask_close   etc

data Query : Type -> Type where
  MkQuery : (sql : String) -> Stmt -> Query a

data Result : Type -> Type where
  MkResult : (res : a) -> Result a

doQuery : HasIO io => Query a -> io (Result a)
doQuery (MkQuery str stmt) = do
  



-}


--