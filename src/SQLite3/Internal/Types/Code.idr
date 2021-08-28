module SQLite3.Internal.Types.Code

||| Providing a mapping from constructor to Int.
public export
interface ToCode a where
  total
  toCode : a -> Int

||| Providing a mapping from Int to constructor.
||| Two ways, if you're certain every Int has a mapping then use unsafeFromCode.
public export
interface FromCode a where
  unsafeFromCode : Int -> a
  total
  fromCode : Int -> Maybe a
