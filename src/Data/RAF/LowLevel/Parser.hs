module Data.RAF.LowLevel.Parser
    ( 

      parseFromFile
    , parse

    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Binary
import           Data.Attoparsec         ( Parser )
import           Data.ByteString         ( ByteString )
import           Data.RAF.LowLevel.Types
import           Data.Word
import           System.Endian

import qualified Data.ByteString as B
import qualified Data.Attoparsec as A

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
        pathHash      <- A.take 4
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

    {-take :: Int -> Parser ByteString
    take n | isBigEndian = B.reverse <$> A.take n
           | otherwise   = A.take n-}

    anyWord32 :: Parser Word32
    anyWord32 | isBigEndian = anyWord32be
              | otherwise   = anyWord32le

    word32 :: Word32 -> Parser Word32
    word32 w | isBigEndian = word32be w
             | otherwise   = word32le w

    isBigEndian :: Bool
    isBigEndian = getSystemEndianness == BigEndian

