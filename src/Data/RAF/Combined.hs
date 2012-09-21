-- |This module is similar to @Data.RAF.Abstract@, but in addition to it it
-- already has read the data as lazy @ByteString@s instead of returning offset
-- and length information.

module Data.RAF.Combined
    (

      RAF
    , parseFromFile
    , parse

    ) where

import           Control.Applicative
import           Data.Map              ( Map )

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.Map              as M
import qualified Data.RAF.Abstract     as A

type RAF = Map FilePath L.ByteString

parseFromFile :: FilePath
              -> FilePath
              -> IO (Either String RAF)
parseFromFile fn fn' = parse <$> B.readFile fn <*> L.readFile fn'

parse :: B.ByteString -> L.ByteString -> Either String RAF
parse bs bs' = M.map (\(s, c) -> portion s c bs') <$> A.parse bs
  where
    portion s c = L.take (fromIntegral c) . L.drop (fromIntegral s)

