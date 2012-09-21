module Data.RAF.LowLevel.Types
    ( 

      RAF(..)
    , FileList(..)
    , PathList(..)
    , FileEntry(..)
    , PathListEntry(..)

    ) where

import           Data.ByteString      ( ByteString )
import           Data.Word            ( Word32 )

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

