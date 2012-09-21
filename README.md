raf: A library for working with the Riot Archive File (RAF) format

Format Description
------------------

The .RAF format is a file format which is designed to work with .RAF.DAT files.
While .RAF files store references to files, the (compressed) content of these
files are stored in seperate .RAF.DAT files.  The .RAF maps file names to
offsets which map to binary data of these files.  All .RAF files are encoded in
little endian.

Features
--------

This library is able to parse .RAF files and combine the results with a
.RAF.DAT file into some high level Haskell data type. Is has some basic support
for writing .RAF files, too.

All features are independent of the endianness of the host machine. However,
not much testing has been done.

