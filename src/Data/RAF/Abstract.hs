-- | This module provides some abstraction on the data types exposed in
-- @Data.RAF.LowLevel@. This module is likely what you want to use when working
-- with RAF files.
--
-- In case you want to work with RAF files and their corresponding data files
-- have a look at @Data.RAF.Combined@ which is a light abstraction of this
-- module.

module Data.RAF.Abstract
    (

      RAF
    , parseFromFile
    , parse

    ) where

import           Control.Applicative
import           Data.ByteString      ( ByteString )
import           Data.Map             ( Map )

import qualified Data.Map              as M
import qualified Data.ByteString.Char8 as B
import qualified Data.RAF.LowLevel     as L

type RAF = Map FilePath (Int, Int)

parseFromFile :: FilePath -> IO (Either String RAF)
parseFromFile = fmap parse . B.readFile

parse :: ByteString -> Either String RAF
parse bs = M.fromList . mkAssoc <$> L.parse bs
  where
    mkAssoc :: L.RAF -> [(FilePath, (Int, Int))]
    mkAssoc raf = flip map (L.fileEntries (L.fileList raf)) $ \fe ->
        let n = fromIntegral $ L.pathListIndex fe
            s = fromIntegral $ L.dataOffset    fe
            c = fromIntegral $ L.dataSize      fe
        in (B.unpack $ lookupUnsafe n, (s, c))
      where
        lookupUnsafe n = portion (L.pathStrings $ L.pathList raf)
          where
            entry = L.pathListEntries (L.pathList raf) !! n

            from :: Int
            from  = fromIntegral (L.pathOffset entry)
                  - 8 * (1 + fromIntegral (L.pathListCount $ L.pathList raf))

            len :: Int
            len   = fromIntegral $ L.pathLength entry

            portion :: ByteString -> ByteString
            portion = B.take (len - 1) . B.drop from

