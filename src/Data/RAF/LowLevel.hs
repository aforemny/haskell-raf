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

module Data.RAF.LowLevel
    ( 

      module Data.RAF.LowLevel.Types
    , module Data.RAF.LowLevel.Parser
    , module Data.RAF.LowLevel.Writer

    ) where

import Data.RAF.LowLevel.Types
import Data.RAF.LowLevel.Parser
import Data.RAF.LowLevel.Writer

