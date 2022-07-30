[![Alire dcf](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dcf.json)](https://alire.ada.dev/crates/dcf.html)
[![Alire zipdcf](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/zipdcf.json)](https://alire.ada.dev/crates/zipdcf.html)
[![License](https://img.shields.io/github/license/onox/dcf-ada.svg?color=blue)](https://github.com/onox/dcf-ada/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/onox/dcf-ada.svg)](https://github.com/onox/dcf-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# dcf-ada

An Ada 2012 library for document container files, a Zip-based archive files
with some limitations:

 * Only the "store" (uncompressed) and "deflate" ([RFC 1951][url-deflate])
   compression methods can be used

 * Archives cannot be encrypted or contain digital signatures

 * Archives cannot be split into multiple files

This library is based on the [Zip-Ada][url-zip-ada] library, with extensive
modifications:

 * Binary and Windows-specific files have been removed with [The BFG Repo Cleaner][url-bfg]

 * Reformatted code to Ada default style guide

 * Removed obsolescent features and implementation-defined extensions

 * All packages except one that uses `Ada.Calendar` are preelaborated

 * Removed unneeded features

 * Removed lots of duplicated code and simplified the API, reducing SLOC from 12k to 4.5k

## Usage

Before the archived files in a document container file can be queried
or extracted, the archive first needs to be opened and loaded:

```ada
Archive_Stream : aliased DCF.Streams.File_Zipstream := DCF.Streams.Open (Archive_Name);
Info : DCF.Zip.Zip_Info;
```

Then load the archive by calling `DCF.Zip.Load (Info, Archive_Stream)`.
While the `Info` and `Archive_Stream` objects are in scope, the files
can be visited in order to extract them.

### Visiting archived files in a document container file

Files can be extracted by visiting over all the archived files in the
archive and then extracting each of them:

```ada
procedure Visit_File (File : DCF.Zip.Archived_File) is
begin
   Ada.Text_IO.Put_Line ("Visiting " & File.Name);
end Visit_File;

procedure Visit_All_Files is new DCF.Zip.Traverse (Visit_File);
procedure Visit_One_File  is new DCF.Zip.Traverse_One_File (Visit_File);
```

Call `Visit_All_Files (Info)` to visit all files. To extract a single
file, call `Visit_One_File (Info, "my-file.txt")` instead.

### Querying an archived file

A `File` object of type `Archived_File` can be queried:

 * `Name` returns the name (path) of the file

 * `Compressed_Size` gives the compressed size

 * `Uncompressed_Size` gives the uncompressed size

 * `Date_Time` returns the modification date and time

 * `Compressed` returns True if compressed with the "deflate" algorithm
   and False if stored uncompressed

 * `Encrypted` returns True if the file is encrypted and False otherwise.
   Note that encryption is prohibited and not supported, thus these files
   cannot be decrypted and extracted

 * `CRC_32` returns the CRC code. This number can be printed with
   `DCF.Zip.CRC.Image`

### Extracting an archived file

While visiting an archived file, the file can be extracted using a
`Stream_Writer` object. First create the file and then write to it:

```ada
declare
   File_Stream   : aliased DCF.Streams.File_Zipstream := DCF.Streams.Create (File.Name);
   Stream_Writer : DCF.Unzip.Streams.Stream_Writer (File_Stream'Access);
begin
   DCF.Unzip.Streams.Extract
     (Destination      => Stream_Writer,
      Archive_Info     => Info,
      File             => File,
      Verify_Integrity => False);
end;
```

If you want to extract to a `Stream_Element_Array`, use `DCF.Streams.Array_Zipstream`:

```ada
declare
   Byte_Stream   : aliased DCF.Streams.Array_Zipstream (My_Element_Array'Access);
   Stream_Writer : DCF.Unzip.Streams.Stream_Writer (Byte_Stream'Access);
begin
```

If you want to verify the integrity of the file without extracting it, set
`Verify_Integrity` to `True` and use `null` in the discriminant of `Stream_Writer`.

Note that you should verify that `File.Name` is a valid path and sanitize
it before attempting to create and write to the file.

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * [Alire][url-alire] and `make`

## Using the library

Use the library in your crates as follows:

```
alr with dcf
```

## Installing the tools

Some tools to compress or decompress document container files can be build with:

```
$ make
$ make PREFIX=~/.local install
```

## Thanks

Much thanks to @zertovitch for the Zip-Ada project.

## Contributing

Please read the [contributing guidelines][url-contributing] before opening
issues or pull requests.

## License

This library is distributed under the terms of the [MIT License][url-mit].
The first line of each Ada file should contain an SPDX license identifier
tag that refers to this license:

    SPDX-License-Identifier: MIT

  [url-alire]: https://alire.ada.dev/
  [url-bfg]: https://rtyley.github.io/bfg-repo-cleaner
  [url-contributing]: /CONTRIBUTING.md
  [url-deflate]: https://tools.ietf.org/html/rfc1951
  [url-mit]: https://opensource.org/licenses/mit
  [url-zip-ada]: https://unzip-ada.sourceforge.net
