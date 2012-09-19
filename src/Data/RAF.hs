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
-- take this into account.
--
-- <http://leagueoflegends.wikia.com/wiki/RAF:_Riot_Archive_File>

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
import           Data.Attoparsec.Binary
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
    , managerIndex    :: Word32
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
parse = A.parseOnly raf
  where
    raf = do
        magicNumber    <- word32 0x18be0ef0
        version        <- anyWord32
        managerIndex   <- anyWord32
        fileListOffset <- anyWord32
        pathListOffset <- anyWord32
        let fileListSkip = fromIntegral fileListOffset - 20
        _              <- A.take fileListSkip
        fileList       <- fileList
        let pathListSkip = fromIntegral pathListOffset
                         - (24 + 20 * (fromIntegral $ numberOfEntries fileList))
        _              <- A.take pathListSkip
        pathList       <- pathList
        return $ RAF magicNumber version managerIndex fileListOffset
                     pathListOffset fileList pathList

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
    take n | isBigEndian = B.reverse <$> A.take n
           | otherwise   = A.take n

    anyWord32 :: Parser Word32
    anyWord32 | isBigEndian = anyWord32be
              | otherwise   = anyWord32le

    word32 :: Word32 -> Parser Word32
    word32 w | isBigEndian = word32be w
             | otherwise   = word32le w

    pack :: Bits a => ByteString -> a
    pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

    unpack :: (Bits a, Integral a) => a -> ByteString
    unpack x = B.pack $ map f $ reverse [ 0 .. byteSize x - 1 ]
      where f s = fromIntegral $ shiftR x (8 * s)

    byteSize :: Bits a => a -> Int
    byteSize = (`div` 8) . bitSize

    isBigEndian :: Bool
    isBigEndian = getSystemEndianness == BigEndian

