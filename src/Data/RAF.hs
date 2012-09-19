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

parseFromFile :: FilePath -> Endianness -> IO (Either String RAF)
parseFromFile fn en = flip parse en <$> B.readFile fn

parse :: ByteString -> Endianness -> Either String RAF
parse s en = A.parseOnly raf s
  where
    raf = do
        magicNumber    <- word32 0x18be0ef0
        version        <- anyWord32
        managerIndex   <- anyWord32
        fileListOffset <- anyWord32
        pathListOffset <- anyWord32
        fileList       <- fileList
        pathList       <- pathList
        return $ RAF magicNumber version managerIndex fileListOffset
                     pathListOffset fileList pathList
    fileList = do
        numberOfEntries <- anyWord32
        fileEntries     <- A.count (fromIntegral numberOfEntries) fileEntry
        return $ FileList numberOfEntries fileEntries

    fileEntry = do
        pathHash      <- take 4
        dataOffset    <- anyWord32
        dataSize      <- anyWord32
        pathListIndex <- anyWord32
        return $ FileEntry pathHash dataOffset dataSize pathListIndex

    pathList = do
        pathListSize    <- anyWord32
        pathListCount   <- fromIntegral <$> anyWord32
        pathListEntries <- A.count (fromIntegral pathListCount) pathListEntry
        pathStrings     <- A.takeWhile (const True)
        return $ PathList pathListSize pathListCount pathListEntries
                          pathStrings
    pathListEntry = do
         pathOffset <- anyWord32
         pathLength <- anyWord32
         return $ PathListEntry pathOffset pathLength

    take :: Int -> Parser ByteString
    take n | en == LittleEndian = A.take n
           | otherwise          = B.reverse <$> A.take n

    anyWordN :: Bits a => (ByteString -> a) -> Parser a
    anyWordN = anyWordN' undefined
      where
        anyWordN' :: Bits a => a -> (ByteString -> a) -> Parser a
        anyWordN' d f
            | en == LittleEndian = f             <$> A.take (byteSize d)
            | otherwise          = f . B.reverse <$> A.take (byteSize d)

    anyWord16 :: Parser Word16
    anyWord16 = anyWordN pack

    anyWord32 :: Parser Word32
    anyWord32 = anyWordN pack

    anyWord64 :: Parser Word64
    anyWord64 = anyWordN pack

    wordN :: Bits a => (a -> B.ByteString) -> a -> Parser a
    wordN f v | en == BigEndian    = A.string (B.reverse $ f v) >> return v
              | otherwise          = A.string (            f v) >> return v

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

