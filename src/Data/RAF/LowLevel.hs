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

module Data.RAF.LowLevel
    ( 

      module Data.RAF.LowLevel.Types
    , module Data.RAF.LowLevel.Parser
    , module Data.RAF.LowLevel.Writer

    , relativeFileListOffset
    , relativePathListOffset
    , relativePathOffset
    , relativePathLength

    , numberOfFiles

    , filePath
    , filePathUnsafe

    ) where

import           Data.ByteString           ( ByteString )
import           Data.RAF.LowLevel.Parser
import           Data.RAF.LowLevel.Types
import           Data.RAF.LowLevel.Writer
import           Data.Word

import qualified Data.ByteString as B

-- |The @fileListOffset@ describes the offset from the start of the .RAF file
-- to the @FileList@. This function gives the relative file list offset, that
-- is the amount to skip to the @FileList@ after having read the
-- @fileListOffset@.

relativeFileListOffset :: RAF -> Word32
relativeFileListOffset = subtract 20 . fileListOffset


-- |Relative @pathListOffset@, compare to @relativeFileListOffset@.

relativePathListOffset :: RAF -> Word32
relativePathListOffset raf = subtract (28 + n * 20) $ pathListOffset raf
  where n = numberOfEntries $ fileList raf

-- |The @pathOffset@ specifies the offset from the @PathList@ to the
-- @PathListEntry@'s file path. This function gives the relative offset, that
-- is the amount to skip from @pathStrings@ to the file path.

relativePathOffset :: RAF -> PathListEntry -> Word32
relativePathOffset raf ple = subtract (8 * (n + 1)) $ pathOffset ple
  where n = pathListCount $ pathList raf

-- |@pathLength@ returns the file path length including the terminating \NUL
-- character. @relativePathLength@ returns the file path length withough the
-- terminating \NUL character.

relativePathLength :: PathListEntry -> Word32
relativePathLength = subtract 1 . pathLength

-- |Returns the number of file paths encoded in a RAF data structure. This is
-- the top-level lifting of @numberOfEntries@.

numberOfFiles :: RAF -> Word32
numberOfFiles = numberOfEntries . fileList

-- |@filePath n@ finds the file path of the n-th @PathListEntry@. Nothing is
-- returned if the index n is out of range.

filePath :: RAF -> Word32 -> Maybe ByteString
filePath raf n | n >= maxN = Nothing
               | otherwise = Just $ filePathUnsafe raf n
  where maxN = pathListCount $ pathList raf

-- |@filePathUnsafe n@ finds the file path of the n-th @PathListEntry@. No
-- bounds check is performed.

filePathUnsafe :: RAF -> Word32 -> ByteString
filePathUnsafe raf n = B.take c . B.drop s $ pathStrings $ pathList raf
  where
    s   = fromIntegral $ relativePathOffset raf ple
    c   = fromIntegral $ relativePathLength ple
    ple = (!! fromIntegral n) $ pathListEntries $ pathList raf

