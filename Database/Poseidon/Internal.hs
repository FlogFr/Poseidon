module Database.Poseidon.Internal where

import Data.Binary
import Data.Binary.Get
import Data.Int

getRemainingWord8 :: Get [Word8]
getRemainingWord8 = do
  bempty <- isEmpty
  if bempty
    then return []
    else do current <- getWord8
            remainingWords <- getRemainingWord8
            return (current:remainingWords)

getDate :: Get (Integer, Integer)
getDate = do
  totalSeconds <- Data.Binary.get :: Get Int64
  let relDays    = (totalSeconds `div` 86400000000) + 51544
  let relSeconds = (totalSeconds `mod` 86400000000) `div` 1000000
  pure(fromIntegral relDays, fromIntegral relSeconds)


data PGRange = PGRange
  { pgH              :: !Word8
  , pgLengthStart    :: !Word32
  , pgRangeStartData :: ![Word8]
  , pgLengthStop     :: !Word32
  , pgRangeStopData  :: ![Word8]
  } deriving (Show)

getPGRange :: Get PGRange
getPGRange = do
  pgH' <- getWord8 -- get the first byte
  pgLengthStart' <- getWord32be
  pgRangeStartData' <- getNbPGArrayData pgLengthStart'
  pgLengthStop' <- getWord32be
  pgRangeStopData' <- getNbPGArrayData pgLengthStop'

  pure $! PGRange pgH' pgLengthStart' pgRangeStartData' pgLengthStop' pgRangeStopData'

--  Please refer to the documentation of array.h in postgresql repository
--  https://doxygen.postgresql.org/array_8h_source.html
-- *	  <vl_len_>		  - standard varlena header word
-- *	  <ndim>		    - number of dimensions of the array
-- *	  <dataoffset>	- offset to stored data, or 0 if no nulls bitmap
-- *	  <elemtype>	  - element type OID
-- *	  <dimensions>	- length of each array axis (C array of int)
-- *	  <lower bnds>	- lower boundary of each dimension (C array of int)
-- *	  <null bitmap> - bitmap showing locations of nulls (OPTIONAL)
-- *	  <actual data> - whatever is the stored data
data PGArray = PGArray
  { pgArrayVl_len_    :: !Word32
  , pgArrayNbDim      :: !Word16
  , pgArrayDataOffset :: !Word32
  , pgArrayElemType   :: !Word16
  , pgArrayDimensions :: !Word32
  , pgArrayLowerBound :: !Word32
  , pgArrayData       :: ![PGArrayData]
  } deriving (Show)

data PGArrayData = PGArrayData
  { pgArrayDataLength :: !Word32
  , pgArrayDataData   :: ![Word8]
  } deriving (Show)

getNb16PGArrayData :: Word16 -> Get [Word8]
getNb16PGArrayData 0 = pure []
getNb16PGArrayData nbWords = do
  currentWord <- getWord8
  remainingWords <- getNb16PGArrayData (nbWords - 1)
  pure (currentWord:remainingWords)

getNbPGArrayData :: Word32 -> Get [Word8]
getNbPGArrayData 0 = pure []
getNbPGArrayData nbWords = do
  currentWord <- getWord8
  remainingWords <- getNbPGArrayData (nbWords - 1)
  pure (currentWord:remainingWords)

getPGArrayData :: Get PGArrayData
getPGArrayData = do
  textLength <- getWord32be
  pgWord8' <- getNbPGArrayData textLength
  pure $ PGArrayData textLength pgWord8'

getPGArrayDataList :: Get [PGArrayData]
getPGArrayDataList = do
  bempty <- isEmpty
  if bempty
    then pure []
    else do pgArrayData' <- getPGArrayData
            pgArrayDatas' <- getPGArrayDataList
            pure (pgArrayData':pgArrayDatas')

getPGArray :: Get PGArray
getPGArray = do
  vl_len_' <- getWord32be
  nbDim'   <- getWord16be
  dataOffset' <- getWord32be
  elemType' <- getWord16be
  dimensions' <- getWord32be
  lowerBound' <- getWord32be

  -- A text is represented by the length of the next words
  -- then appended the text
  pgArrayData' <- getPGArrayDataList

  pure $! PGArray vl_len_' nbDim' dataOffset' elemType' dimensions' lowerBound' pgArrayData'
