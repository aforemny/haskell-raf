module Data.RAF.LowLevel.Writer
    ( 

      writeToFile
    , write

    ) where

import           Data.Bits
import           Data.ByteString         ( ByteString )
import           Data.RAF.LowLevel.Types
import           System.Endian

import qualified Data.ByteString as B

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

