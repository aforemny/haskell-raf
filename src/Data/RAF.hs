-- | This module provides some low level abstraction on Riot Archive Format
-- (RAF) files. This format is used by Riot Games Inc. most notably in their
-- game League of Legends. In case you are looking for a library to deal with
-- RAW Image Files this library is not for you.
--
-- Implementation is based on the following description of the file format as
-- of 19.09.2012. Implementation is straight forward and very close to the
-- description. There is some fiddling with endianness which has not been
-- tested extensively. Literal values, e.g. the magic number, are encoded in
-- big endian. Thus, conversion of arguments of some parser functions have to
-- take this into account, c.f. wordN.
--
-- <http://leagueoflegends.wikia.com/wiki/RAF:_Riot_Archive_File>
--
-- Some code is copied verbatim from the attoparsec-binary package.
--
-- <http://hackage.haskell.org/package/attoparsec-binary>

module Data.RAF
    ( 

      RAF(..)
    , FileList(..)
    , PathList(..)
    , FileEntry(..)
    , PathListEntry(..)

    , parse
    , parseFromFile

    ) where

import           Prelude hiding (take)

import           Control.Applicative
import           Data.Attoparsec      (Parser)
import           Data.Bits
import           Data.ByteString      (ByteString)
import           Data.Word
import           System.Endian

import qualified Data.ByteString as B
import qualified Data.Attoparsec as A

data RAF = RAF
    { magicNumber     :: Word32
    , version         :: Word32
    , mangerIndex     :: Word32
    , fileListOffset  :: Word32
    , pathListOffset  :: Word32
    , fileList        :: FileList
    , pathList        :: PathList
    } deriving (Show)

data FileList = FileList
    { numberOfEntries :: Word32
    , fileEntries     :: [FileEntry]
    } deriving (Show)

data FileEntry = FileEntry
    { fileEntryHash          :: ByteString
    , fileEntryDataOffset    :: Word32
    , fileEntryDataSize      :: Word32
    , fileEntryPathListIndex :: Word32
    } deriving (Show)

data PathList = PathList
    { pathListSize    :: Word32
    , pathListCount   :: Word32
    , pathListEntries :: [PathListEntry]
    , pathStrings     :: ByteString
    } deriving (Show)

data PathListEntry = PathListEntry
    { pathOffset :: Word32
    , pathLength :: Word32
    } deriving (Show)

parseFromFile :: FilePath -> IO (Either String RAF)
parseFromFile fn = parse <$> B.readFile fn

parse :: ByteString -> Either String RAF
parse s = A.parseOnly raf s
  where
    raf = RAF <$> (word32 0x18be0ef0)
              <*> anyWord32 <*> anyWord32
              <*> anyWord32 <*> anyWord32
              <*> fileList  <*> pathList

    fileList = do
        numberOfEntries <- anyWord32
        fileEntries     <- A.count (fromIntegral numberOfEntries) fileEntry
        return $ FileList numberOfEntries fileEntries

    fileEntry = FileEntry <$> take 4 <*> anyWord32 <*> anyWord32 <*> anyWord32

    pathList = do
        pathListSize    <- anyWord32
        pathListCount   <- fromIntegral <$> anyWord32
        pathListEntries <- A.count (fromIntegral pathListCount) pathListEntry
        pathStrings     <- A.takeWhile (const True)
        return $ PathList pathListSize pathListCount pathListEntries
                          pathStrings
    pathListEntry = PathListEntry <$> anyWord32 <*> anyWord32

    take :: Int -> Parser ByteString
    take n | isBigEndian    = B.reverse <$> A.take n
           | otherwise = A.take n

    anyWordN :: Bits a => (ByteString -> a) -> Parser a
    anyWordN = anyWordN' undefined
      where
        anyWordN' :: Bits a => a -> (ByteString -> a) -> Parser a
        anyWordN' d f
            | isBigEndian = f . B.reverse <$> A.take (byteSize d)
            | otherwise   = f             <$> A.take (byteSize d)

    anyWord16 :: Parser Word16
    anyWord16 = anyWordN pack

    anyWord32 :: Parser Word32
    anyWord32 = anyWordN pack

    anyWord64 :: Parser Word64
    anyWord64 = anyWordN pack

    wordN :: Bits a => (a -> B.ByteString) -> a -> Parser a
    wordN f v | isBigEndian = A.string (            f v) >> return v
              | otherwise   = A.string (B.reverse $ f v) >> return v

    word16 :: Word16 -> Parser Word16
    word16 = wordN unpack

    word32 :: Word32 -> Parser Word32
    word32 = wordN unpack

    word64 :: Word64 -> Parser Word64
    word64 = wordN unpack

    pack :: Bits a => ByteString -> a
    pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

    unpack :: (Bits a, Integral a) => a -> ByteString
    unpack x = B.pack $ map f $ reverse [ 0 .. byteSize x - 1 ]
      where f s = fromIntegral $ shiftR x (8 * s)

    byteSize :: Bits a => a -> Int
    byteSize = (`div` 8) . bitSize

    isBigEndian :: Bool
    isBigEndian = getSystemEndianness == BigEndian

