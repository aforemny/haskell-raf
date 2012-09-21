{-# LANGUAGE DoRec #-}
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
import           Control.Monad
import           Data.Attoparsec.Binary
import           Data.Attoparsec      (Parser)
import           Data.Bits
import           Data.ByteString      (ByteString)
import           Data.Word
import           System.Endian

import qualified Data.ByteString as B
import qualified Data.Attoparsec as A

import System.IO.Unsafe

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
    { pathHash      :: ByteString
    , dataOffset    :: Word32
    , dataSize      :: Word32
    , pathListIndex :: Word32
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
parse bs = do
    -- TODO: Recursive Do
    partial <- A.parseOnly raf bs
    let flOffset = fileListOffset $ partial undefined undefined
        plOffset = pathListOffset $ partial undefined undefined
    fileList <- A.parseOnly fileList $ B.drop (fromIntegral $ flOffset) bs
    pathList <- A.parseOnly pathList $ B.drop (fromIntegral $ plOffset) bs
    return $ partial fileList pathList
  where
    raf :: Parser (FileList -> PathList -> RAF)
    raf = RAF <$> (word32 0x18be0ef0)
              <*> anyWord32 <*> anyWord32
              <*> anyWord32 <*> anyWord32

    fileList = do
        numberOfEntries <- anyWord32
        FileList numberOfEntries <$> A.count (fromIntegral numberOfEntries)
                                             (fileEntry numberOfEntries)

    fileEntry numberOfEntries = do
        pathHash      <- take 4
        dataOffset    <- anyWord32
        dataSize      <- anyWord32
        pathListIndex <- anyWord32
        guard (pathListIndex < numberOfEntries)
        return $ FileEntry pathHash dataOffset dataSize pathListIndex

    pathList = do
        pathListSize    <- anyWord32
        pathListCount   <- anyWord32
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

    isBigEndian :: Bool
    isBigEndian = getSystemEndianness == BigEndian

writeToFile :: FilePath -> RAF -> IO ()
writeToFile fn = B.writeFile fn . write

write :: RAF -> ByteString
write raf = B.concat
    $ map (unpack . flip ($) raf) [ magicNumber, version, managerIndex
                                  , fileListOffset, pathListOffset ]
    ++ [writeFL (fileList raf), writePL (pathList raf)]
  where
    writeFL :: FileList -> ByteString
    writeFL fl = B.concat $ [ unpack (numberOfEntries fl) ]
                         ++ map writeFE (fileEntries fl)

    writeFE :: FileEntry -> ByteString
    writeFE fe = B.concat [ pathHash fe
                          , dump unpack [ dataOffset, dataSize, pathListIndex ]
                            fe ]

    writePL :: PathList -> ByteString
    writePL pl = B.concat $ [ unpack   (pathListSize    pl)
                            , unpack   (pathListCount   pl) ]
                            ++ map writePLE (pathListEntries pl)
                            ++ [ writePS  (pathStrings     pl) ]

    writePLE :: PathListEntry -> ByteString
    writePLE = dump unpack [ pathOffset, pathLength ]

    writePS = id

    dump :: (a -> ByteString) -> [(r -> a)] -> r -> ByteString
    dump fc fs r = B.concat $ map fc $ map (flip ($) r) $ fs

    unpack :: (Bits a, Integral a) => a -> ByteString
    unpack | isBigEndian = unpack'
           | otherwise   = B.reverse . unpack'
      where
        unpack' :: (Bits a, Integral a) => a -> ByteString
        unpack' x = B.pack $ map f $ reverse [ 0 .. byteSize x - 1 ]
          where f s = fromIntegral $ shiftR x (8 * s)

        byteSize :: Bits a => a -> Int
        byteSize = (`div` 8) . bitSize

    isBigEndian :: Bool
    isBigEndian = getSystemEndianness == BigEndian

