Poseidon - The Simple & Extensible PostgreSQL Library
=====================================================

Poseidon is a library (not an ORM) for running SQL query against PostgreSQL,
and returning custom datatypes (e.g. your User datatype, or Token…), thanks to
the GHC Generics of Haskell.

It is extensible, which means you can add mapping of custom PostgreSQL value to
your custom Haskell datatype in a easy and pluggable way (without hacking into
the library). If you want to know more about this, please refer to the
poseidon-postgis library.

How stable is this library?
---------------------------

Poseidon is heavily relying on the binding of the LibPQ available in Haskell,
and is used already in production in many projects (mainly websites).

How to use the library?
-----------------------

- Add `poseidon` to your haskell project
- Run your sqlQuery from the function Database.Poseidon.queryFromText as
  describe below::
   data User = User {
       first_name :: Text
     , is_admin :: Bool
     } deriving (Generic, Show)

   mainTest :: IO ()
   mainTest = do
     conn <- connectdb . encodeUtf8 $ "service=test"

     -- Simple representation of a result of PG to work with
     result <- queryFromText conn "SELECT CAST('Florian :)' AS TEXT), CAST('t' AS BOOL);" mempty :: IO [User]

     putStrLn ("User : " <> (show result))

   Output of mainTest:

   User : [User {first_name = "Florian :)", is_admin = True}]

All the follow supported types have the availability to be in a Maybe if the
PostgreSQL is nullable. Otherwise, it will raise an error. Some of the types
supports also to be in an array.

List of types supported to be deserialized from PostgreSQL:

+-------------------+---------------------------------+---------------------------------------+
| PostgreSQL Type   | Haskell Type                    | Poseidon Type for Single Value Return |
+===================+=================================+=======================================+
| TEXT NOT NULL     | Data.Text.Text                  | Data.Poseidon.PGText                  |
+-------------------+---------------------------------+---------------------------------------+
| BOOL              | Data.Bool.Bool                  | Data.Poseidon.PGBool                  |
+-------------------+---------------------------------+---------------------------------------+
| TIMESTAMP WITH TZ | Data.Time.UTCTime               | Data.Poseidon.PGTimestamp             |
+-------------------+---------------------------------+---------------------------------------+
| UUID              | Data.UUID.UUID                  | Data.Poseidon.PGUUID                  |
+-------------------+---------------------------------+---------------------------------------+
| JSON              | Data.Aeson.Value                | Data.Poseidon.PGJsonValue             |
+-------------------+---------------------------------+---------------------------------------+
| Binary            | Data.ByteString.ByteString      | Data.Poseidon.PGByteString            |
+-------------------+---------------------------------+---------------------------------------+
| Binary            | Data.ByteString.Lazy.ByteString | Data.Poseidon.PGLazyByteString        |
+-------------------+---------------------------------+---------------------------------------+
| SMALLINT          | Data.Integer.Integer            | Data.Poseidon.PGInteger               |
+-------------------+---------------------------------+---------------------------------------+
| DECIMAL           | Data.Float.Float                | Data.Poseidon.PGDecimal               |
+-------------------+---------------------------------+---------------------------------------+
| NUMERIC           | Data.Float.Float                |                                       |
+-------------------+---------------------------------+---------------------------------------+
| DOUBLE PRECISION  | Data.Double.Double              | Data.Poseidon.PGDouble                |
+-------------------+---------------------------------+---------------------------------------+

You're all free to contribute and add the missing types for you!

Contribute to the library
-------------------------

You're more than welcome to contribute to this library!

You can find good helpers function from the Makefile, and the issues of github
to communicate with me.

TODO list:
- Add more base type to deserialise from PostgreSQL to Haskell

Why another library?
--------------------

Even though I liked very much some of the available library out there for
PostgreSQL in Haskell, none of them gave me 100% satisfaction in my small to
medium project. Some of the library are not extensible without hacking, and
others are not simple. I decided to tackle this two problem by coding this
library. After a refacto to use the Generics of Haskell, the library is now
ready for the free (as of freedom) source world.

Thanks
------

- Sönke Hahn for making simple the use of the GHC.Generics with the
  generics-eot in the case of serialization and deserialization
- All the haskell community for helping me along the way to develop this
  library

References
----------

- PostgreSQL LibPQ bindings: https://hackage.haskell.org/package/postgresql-libpq
- Generics-EOT: https://generics-eot.readthedocs.io/en/stable/
