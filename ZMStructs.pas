unit ZMStructs;

// ZMStructs.pas - Zip file internal structures

(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997 to 2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992 to 2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

 All rights reserved.
 For the purposes of Copyright and this license "DelphiZip" is the current
 authors, maintainers and developers of its code:
 Russell Peters and Roger Aelbrecht.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 * DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",
 "DelZip" and derivatives of those names for the use in or about this
 code and neither those names nor the names of its authors or
 contributors may be used to endorse or promote products derived from
 this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT'S AUTHORS OR CONTRIBUTERS BE
 LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

 contact: problems AT delphizip DOT org
 updates: http://www.delphizip.org
 *************************************************************************** *)
// modified 2013-12-19

interface

{$I   '.\ZipVers.inc'}

uses
{$IFDEF VERDXE2up}
  WinApi.Windows;
{$ELSE}
  Windows;
{$ENDIF}

type
  TWordRec = packed record
    Lo, Hi: Byte;
  end;

type
  TZipSystem = (ZsMSDOS, ZsAMIGA, ZsVMS, ZsUNIX, ZsVM_CMS, ZsATARI, ZsHPFS,
    ZsMACOS, ZsZSYSTEM, ZsCPM, ZsTOPS20, ZsNTFS, ZsQDOS, ZsRISCOS, ZsVFAT,
    ZsMVS, ZsBEOS, ZsTANDEM, ZsTHEOS);

const
  Invalid_Handle = -1;
  PathDelimAlt = '/';
  WILD_MULTI = '*';
  WILD_CHAR = '?';
  WILD_ALL = '*.*';
  DIR_PARENT = '..';
  DIR_THIS = '.';
  SPEC_SEP = '|';
  ZPASSWORDARG: Char = '<';
  ZSWITCH: Char = '/';
  ZSPECARG: Char = '>';
  ZFILE_SEPARATOR = '>>';
{$IFNDEF VERD7up}
  PathDelim = '\';
  DriveDelim = ':';
{$ENDIF}

(*
  Zip format
  [Multi-part marker]  // split or multi-part zips only
  Entry 1
    Local header
    Data
    [data descriptor]  // only if needed
  ...
  Entry n
    Local header
    Data
    [data descriptor]  // only if needed
  Central directory
  [Zip64 end of central record]
  [Zip64 end of central locator]
  End of central directory record
*)

type
  TZipLocalHeader = packed record
    HeaderSig: Longword; // (4)
    VersionNeeded: WORD; // (2)
    Flag: WORD; // (2)
    ComprMethod: WORD; // (2)
    // ModifTime: WORD;              // (2)
    // ModifDate: WORD;              // (2)
    ModifDateTime: LongWord;
    CRC32: Longword; // (4)
    ComprSize: Longword; // (4)
    UncomprSize: Longword; // (4)
    FileNameLen: WORD; // (2)
    ExtraLen: WORD; // (2)
    // not used as part of this record structure:
    // filename, extra data
  end;

  PZipLocalHeader = ^TZipLocalHeader;

  // An entry in the central dir:
type
  TZipCentralHeader = packed record // fixed part size : 42 bytes
    HeaderSig: Longword; // hex: 02014B50(4)
    // VersionMadeBy0: Byte;           //version made by(1)
    // VersionMadeBy1: Byte;           //host number(1)
    VersionMadeBy: WORD; // host number and version
    VersionNeeded: WORD; // version needed to extract(2)
    Flag: WORD; // generalPurpose bitflag(2)
    ComprMethod: WORD; // compression method(2)
    // ModifTime: WORD;                // modification time(2)
    // ModifDate: WORD;                // modification date(2)
    ModifDateTime: LongWord; // dos date/time
    CRC32: Longword; // Cycling redundancy check (4)
    ComprSize: Longword; // compressed file size  (4)
    UncomprSize: Longword; // uncompressed file size (4)
    FileNameLen: WORD; // (2)
    ExtraLen: WORD; // (2)
    FileComLen: WORD; // (2)
    DiskStart: WORD; // starts on disk number xx(2)
    IntFileAtt: WORD; // internal file attributes(2)
    ExtFileAtt: Longword; // external file attributes(4)
    RelOffLocalHdr: Longword; // relative offset of local file header(4)
    // not used as part of this record structure:
    // filename, extra data, file comment
  end;

  PZipCentralHeader = ^TZipCentralHeader;

type
  TZipDataDescriptor = packed record
    DataDescSig: Longword; // Should be 0x08074B50 (4)
    CRC32: Longword; // (4)
    ComprSize: Longword; // (4)
    UncomprSize: Longword; // (4)
  end;

type
  TZipDataDescriptor64 = packed record
    DataDescSig: Longword; // Should be 0x08074B50 (4)
    CRC32: Longword; // (4)
    ComprSize: Int64; // (8)
    UncomprSize: Int64; // (8)
  end;

type
  TZipEndOfCentral = packed record // Fixed part size : 22 bytes
    HeaderSig: Longword; // (4)  hex=06054B50
    ThisDiskNo: WORD; // (2)This disk's number
    CentralDiskNo: WORD; // (2)Disk number central dir start
    CentralEntries: WORD; // (2)Number of central dir entries on this disk
    TotalEntries: WORD; // (2)Number of entries in central dir
    CentralSize: Longword; // (4)Size of central directory
    CentralOffset: Longword; // (4)offset of central dir on 1st disk
    ZipCommentLen: WORD; // (2)
    // not used as part of this record structure:
    // ZipComment
  end;
  PZipEndOfCentral = ^TZipEndOfCentral;

type
  // must be same disk as EOC
  TZip64EOCLocator = packed record
    LocSig: Longword; // (4) Should be 0x07064B50
    EOC64DiskStt: Longword; // (4)
    EOC64RelOfs: Int64; // (8) relative to start of it's disk
    NumberDisks: Longword; // (4) total disks
  end;
  PZip64EOCLocator = ^TZip64EOCLocator;

type
  TZipEOC64 = packed record
    EOC64Sig: LongWord; // (4) should be 0x06064b50
    Vsize: Int64; // (8)    size of variable part
    // variable part   - fields as needed? (old field = 0XFFFF or 0XFF)
    VersionMade: WORD; // (2)
    VersionNeed: WORD; // (2)
    ThisDiskNo: LongWord; // (4)
    CentralDiskNo: LongWord; // (4)
    CentralEntries: Int64; // (8) Number of central dir entries on this disk
    TotalEntries: Int64; // (8) Number of entries in central dir
    CentralSize: Int64; // (8) Size of central directory
    CentralOffset: Int64; // (8) offsett of central dir on 1st disk
    // zip64 extensible data sector    (variable size)
  end;

type
  TZipCentralDigitSignature = packed record
    CenDigSig: LongWord; // (4) should be 0x05054b50
    Vsize: WORD; // (2)
    // data[vsize]
  end;

const { these are stored in reverse order }
  CentralFileHeaderSig = $02014B50; { 'PK'12 }
  LocalFileHeaderSig = $04034B50; { 'PK'34  (in file: 504b0304) }
  CentralDigSig = $05054B50; { 'PK'55 }
  EndCentralDirSig = $06054B50; { 'PK'56 }
  ExtLocalSig = $08074B50; { 'PK'78 }  // also 'span' mark
  EndCentral64Sig = $06064B50; { 'PK'66 }
  EOC64LocatorSig = $07064B50; { 'PK'67 }

  Zip64_data_tag = $0001; // Zip64 extra field tag
  NTFS_data_tag = $000A; // NTFS date stamps
  UPath_Data_Tag = $7075; // Info-Zip UTF8 path field
  UCmnt_Data_Tag = $6375; // Info-Zip UTF8 comment field
  // Tag - 2 byte
  // total size - 2 byte
  // version - 1 byte = 1
  // crc -4 bytes = crc of orig field
  // utf8 string - total size - 9

type
  PUString_Data_Header = ^TUString_Data_Header;

  TUString_Data_Header = packed record
    Tag: WORD;
    Totsiz: WORD;
    Version: Byte;
    Origcrc: DWORD;
  end;

type
  PExtra_Head = ^TExtra_Head;
  TExtra_Head = packed record
    case Boolean of
      True:
        (Tag: WORD; Size: WORD;);
      False:
        (_head: DWORD;);
  end;

type
  PNTFS_Times = ^TNTFS_Times;
  TNTFS_Times = packed record
    MTime: TFILETIME;
    ATime: TFILETIME;
    CTime: TFILETIME;
  end;

  TNTFS_Header_Head = packed record
    case Boolean of
      True:
        (Tag: WORD; Vsize: WORD;);
      False:
        (_head: DWORD;);
  end;

  PNTFS_Header = ^TNTFS_Header;
  TNTFS_Header = packed record
    Head: TNTFS_Header_Head;
    Rsrvd: DWORD;
    Tg1: WORD; // (2) should be 0x0001
    Sz1: WORD; // (2) should be 24
    Times: TNTFS_Times;
  end;

const
  MAX_UNSIGNED = $FFFFFFFF;
  MAX_WORD = $FFFF;
  MAX_BYTE = $FF;

const
  CRYPT_HEAD_SIZE = 12;

const
  FS_FAT = 0;
  FS_HPFS = 6;
  FS_NTFS = 11;
  FLAG_UTF8_BIT = $0800;
  FLAG_DATADESC_BIT = 8;
  FLAG_CRYPT_BIT = 1;

  ZIP64_VER = 45;
  OSMask = $FF00;
  VerMask = $00FF;

type
  TZM_StreamHeader = packed record
    Method: WORD;
    CRC: DWORD;
  end;

  // compression method values
const
  // TZMDeflateEncrypt = 1024; // or-ed with METHOD_DEFLATED in Stream Header
  METHOD_STORED = 0;
  METHOD_DEFLATED = 8;
  METHOD_ENHDEFLATED = 9;

  (*
   const
   // 'DZ'#0#1;  // header signature
   SFX_HEADER_SIG = $01005A44;
   // 'DZ'#10#1; // end of header signature
   SFX_HEADER_END_SIG = $010A5A44;

   // 'DZ'#20#1; // detached header signature
   // SFX_DETACHED_HEADER_SIG = $01145A44;
   // 'DZ'#20#2; // end of detached header signature
   // SFX_DETACHED_HEADER_END_SIG = $02145A44;

   type
   { sfx file header }
   PSFXFileHeader = ^TSFXFileHeader;
   TSFXFileHeader = packed record
   // signature: DZ#0#1 = version 01
   Signature: DWORD;//Cardinal;
   // overall header size (must be DWORD-Aligned!)
   Size: WORD;
   // flags
   Options: DWORD;//TSFXOpts;//TSFXOptions;
   // default overwrite mode
   DefOVW: DWORD;//TZMOvrOpts;//TSFXOverwriteMode;
   // length of the sfx dialog caption (0=default caption)
   CaptionSize,
   // length of the sfx default extraction path (0=current dir)
   PathSize,
   // length of the command line (0=no command line)
   CmdLineSize,
   // length of the extract path to be used if HK... failed (0=%temp%)
   RegFailPathSize,
   // length of the startup message (0=no message)
   StartMsgSize: byte;
   // MB_... (only used if StartMsgSize > 0)
   StartMsgType: DWORD;//Cardinal;
   {
   Caption: string;    if CaptionSize > 0, contains the caption of the sfx dialog (not #0-terminated)

   + will be expanded if it contains % signs and soExpandEnvStrings is set

   Path: string;       if PathSize > 0, contains the default extraction path (not #0-terminated)

   + will be expanded if it contains % signs and soExpandEnvStrings is set

   + if set to "><", then use  temp-dir

   + If the first two characters are "HK" the extraction-path //##FR
   will be read from the registry. If the registry-key doesn't exist, the
   default path will be set to the temp path. Either full names
   (HKEY_CURRENT_USER\...) or abbreviations (as known from INF-files)
   for the root keys HKCU, HKLM and HKU are supported.
   examples:
   "HKEY_CURRENT_USER\Software\Borland\Delphi\2.0\Library\SearchPath"
   "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\ProgramFilesDir"
   "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\Personal"
   "HKCU\Software\Microsoft\Office\8.0\Excel\Microsoft Excel\AddIn Path"
   For subdirectories to be created use the pipe symbol "|", e.g.
   "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\SystemRoot|NewDir"
   will parse to : "C:\Windows\NewDir" (or whereever your system root is).

   CmdLine: string;    if CmdLineSize > 0, contains a command to execute after extraction completed
   (not #0-terminated)

   + will be expanded if it contains % signs and soExpandEnvStrings is set

   + CmdLine has a special format:
   all occurrences of  "><" (greather than+less than) in this string will be replaced by
   the actual extraction path, e.g. :
   if the archive has been extracted to "c:\Program Files\foo", a CmdLine
   of "><bar\some.txt" will parse to "c:\Progra~1\foo\bar\some.txt"
   (because of limitations of the ShellExecute API on some platforms when using long
   filenames the short filename is generated by delzipsfx)

   if the pipe char "|" is found in the CmdLine, the part to the left of it is used as
   the application to run and the part to the right as its command line, e.g.:

   extraction path = "d:\unpack", CmdLine = "><setup.exe|><install.inf"
   -> exec "d:\unpack\setup.exe" with args "d:\unpack\install.inf"

   extraction path = "d:\unpack", CmdLine = "><readme.txt":
   -> open "d:\unpack\readme.txt" with its associated program, if there is any.

   extraction path = "d:\unpack", CmdLine = "><setup.exe":
   -> exec "d:\unpack\setup.exe" without special parameters.

   additionally, if soExpandEnvStrings is set in the Options field:
   extraction path = "d:\unpack", CmdLine = "%windir%notepad.exe|><readme.txt",
   Windows Dir="c:\Windows"
   -> exec "c:\windows\notepad.exe" with args "d:\unpack\readme.txt"

   + INF-scripts are accepted as well //##FR
   "><setup.inf" will run the [DefaultInstall] section of "d:\unpack\setup.inf".
   "><setup.inf|.ntx86" will run the [DefaultInstall] section if Win95 (98?),
   but [DefaultInstall.ntx86] section if WinNT.

   RegFailPath:        string if RegFailPathSize > 0, contains the default extraction path,
   if reading path from registry failed (not #0-terminated)

   + will be expanded if it contains % signs and soExpandEnvStrings is set

   + if set to "><", then use  temp-dir

   StartMsg: string;   if StartMsgSize > 0, contains a message to display before showing the main dialog.
   (not #0-terminated)

   + will be expanded if it contains % signs and soExpandEnvStrings is set

   if this messagebox is closed by pressing either IDCANCEL, IDABORT, IDNO or IDCLOSE button,
   sfx terminates.

   }
   end;

   // the following record is only used by TZipSFX20 and TZipMasterSFX to quickly find the
   // TSFXFileHeader in the file, it's not used by the sfx itself.
   // it's immediately stored after the TSFXFileHeader record
   PSFXFileEndOfHeader= ^TSFXFileEndOfHeader;
   TSFXFileEndOfHeader= packed record
   Signature: DWORD;    //must be SFX_HEADER_END_SIG
   HeaderSize: DWORD;   //must be equal to TSFXFileHeader.Size (DWORD for alignment issuses)
   end;
   (*
   // in detached sfx archives, the sfx contains an additional record following the header
   PSFXDetachedHeader= ^TSFXDetachedHeader;
   TSFXDetachedHeader= packed record
   Signature: DWORD; // must be SFX_DETACHED_HEADER_SIG or SFX_DETACHED_HEADER_END_SIG
   NameLen,
   ExtLen: DWORD; // contains the length of the filename/fileext of the archive (without path)
   {
   Name: string      if NameLen > 0, Name contains the file name (without extension) of
   the detached archive
   Ext: string      if ExtLen > 0, Ext contains the file Ext (without leading .) of
   the detached archive
   }
   end;
  *)
  { the structure of a zipsfx-file :
    - zipsfx-executable code (0-xxxxx)
   - TSFXFileHeader record (see above)
   - possibly one or more strings (depending on the headers' properties) (not #0 terminated)
   Caption                             sfx dialog caption
   Path                                sfx default extraction path
   CmdLine                             command line to execute after extraction
   RegFailPath                         default extract path if Path could not be read from registry
   StartMsg                            startup message

   NOTE: the complete header (including the strings) must be DWORD-aligned!

   if not detached from the archive:
   - the zip archive

   if detached from the archive:
   - TSFXDetachedHeader + filename + TSFXDetachedHeader
   - centraldirectories[] + endofcentraldir of the zip archive
  }
  (*
   type
   PSFXDetachedHeader = ^TSFXDetachedHeader;
   TSFXDetachedHeader= packed record
   Signature: DWORD; // must be SFX_DETACHED_HEADER_SIG or SFX_DETACHED_HEADER_END_SIG
   NameLen,
   ExtLen: Cardinal; // contains the length of the filename/fileext of the archive (without path)
   {
   Name: string      if NameLen > 0, Name contains the file name (without extension) of
   the detached archive
   Ext: string      if ExtLen > 0, Ext contains the file Ext (without leading .) of
   the detached archive
   }
   end;
  *)
type
  PImageResourceDirectory = ^_IMAGE_RESOURCE_DIRECTORY;
  _IMAGE_RESOURCE_DIRECTORY = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    NumberOfNamedEntries: WORD;
    NumberOfIdEntries: WORD;
  end;

  TImageResourceDirectory = _IMAGE_RESOURCE_DIRECTORY;
  PImageResourceDirectoryEntry = ^_IMAGE_RESOURCE_DIRECTORY_ENTRY;
  _IMAGE_RESOURCE_DIRECTORY_ENTRY = packed record
    Un1: record
    case Integer of
      1: (NameIsString: DWORD); // Bit 31
      2: (NameOffset: DWORD); // Bits 30..0
      3: (Name: DWORD);
      4: (Id: WORD);
    end;

    Un2: record
    case Integer of
      1:
        (OffsetToData: DWORD);
      2:
        (DataIsDirectory: DWORD); // Bit 31
      3:
        (OffsetToDirectory: DWORD); // Bits 30..0
    end;
  end;
  TImageResourceDirectoryEntry = _IMAGE_RESOURCE_DIRECTORY_ENTRY;
  PImageResourceDataEntry = ^_IMAGE_RESOURCE_DATA_ENTRY;
  _IMAGE_RESOURCE_DATA_ENTRY = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  TImageResourceDataEntry = _IMAGE_RESOURCE_DATA_ENTRY;

  PIconDir = ^ICONDIR;
  ICONDIR = packed record
    Reserved: WORD;
    ResType: WORD;
    ResCount: WORD;
    // idEntries[]: ICONDIRENTRY
  end;
  TIconDir = ICONDIR;

  PIconDirEntry = ^ICONDIRENTRY;
  ICONDIRENTRY = packed record
    BWidth, BHeight, BColorCount, BReserved: Byte;
    WPlanes, WBitCount: WORD;
    DwBytesInRes, DwImageOffset: DWORD;
  end;
  TIconDirEntry = ICONDIRENTRY;

const
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = $80000000;

const
  IMAGE_PE_SIGNATURE = $00004550;


  // ----------------Icon resource info  needed SFX -----------------------------------
  (*{$IFDEF VERD2D3}
   const
   IMAGE_DIRECTORY_ENTRY_RESOURCE = 2;
   IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
   IMAGE_SIZEOF_SHORT_NAME = 8;
   IMAGE_DOS_SIGNATURE     = $5A4D;
   IMAGE_NT_SIGNATURE      = $00004550;

   type
   pIMAGE_DOS_HEADER = ^IMAGE_DOS_HEADER;

   IMAGE_DOS_HEADER = packed record      { DOS .EXE header }
   e_magic: WORD;                      { Magic number }
   e_cblp: WORD;                       { Bytes on last page of file }
   e_cp: WORD;                         { Pages in file }
   e_crlc: WORD;                       { Relocations }
   e_cparhdr: WORD;                    { Size of header in paragraphs }
   e_minalloc: WORD;                   { Minimum extra paragraphs needed }
   e_maxalloc: WORD;                   { Maximum extra paragraphs needed }
   e_ss: WORD;                         { Initial (relative) SS value }
   e_sp: WORD;                         { Initial SP value }
   e_csum: WORD;                       { Checksum }
   e_ip: WORD;                         { Initial IP value }
   e_cs: WORD;                         { Initial (relative) CS value }
   e_lfarlc: WORD;                     { File address of relocation table }
   e_ovno: WORD;                       { Overlay number }
   e_res: packed array[0..3] of WORD;  { Reserved words }
   e_oemid: WORD;                      { OEM identifier (for e_oeminfo) }
   e_oeminfo: WORD;                    { OEM information; e_oemid specific }
   e_res2: packed array[0..9] of WORD; { Reserved words }
   _lfanew: Longint;                   { File address of new exe header }
   end;

   type
   pImageDataDirectory = ^IMAGE_DATA_DIRECTORY;

   _IMAGE_DATA_DIRECTORY = record
   VirtualAddress: DWORD;
   Size: DWORD;
   end;
   IMAGE_DATA_DIRECTORY = _IMAGE_DATA_DIRECTORY;

   type
   pImageFileHeader = ^IMAGE_FILE_HEADER;

   _IMAGE_FILE_HEADER = packed record
   Machine: WORD;
   NumberOfSections: WORD;
   TimeDateStamp: DWORD;
   PointerToSymbolTable: DWORD;
   NumberOfSymbols: DWORD;
   SizeOfOptionalHeader: WORD;
   Characteristics: WORD;
   end;
   IMAGE_FILE_HEADER = _IMAGE_FILE_HEADER;

   type
   pImageOptionalHeader = ^IMAGE_OPTIONAL_HEADER;

   _IMAGE_OPTIONAL_HEADER = packed record
   { Standard fields. }
   Magic: WORD;
   MajorLinkerVersion: Byte;
   MinorLinkerVersion: Byte;
   SizeOfCode: DWORD;
   SizeOfInitializedData: DWORD;
   SizeOfUninitializedData: DWORD;
   AddressOfEntryPoint: DWORD;
   BaseOfCode: DWORD;
   BaseOfData: DWORD;
   { NT additional fields. }
   ImageBase: DWORD;
   SectionAlignment: DWORD;
   FileAlignment: DWORD;
   MajorOperatingSystemVersion: WORD;
   MinorOperatingSystemVersion: WORD;
   MajorImageVersion: WORD;
   MinorImageVersion: WORD;
   MajorSubsystemVersion: WORD;
   MinorSubsystemVersion: WORD;
   Win32VersionValue: DWORD;
   SizeOfImage: DWORD;
   SizeOfHeaders: DWORD;
   CheckSum: DWORD;
   Subsystem: WORD;
   DllCharacteristics: WORD;
   SizeOfStackReserve: DWORD;
   SizeOfStackCommit: DWORD;
   SizeOfHeapReserve: DWORD;
   SizeOfHeapCommit: DWORD;
   LoaderFlags: DWORD;
   NumberOfRvaAndSizes: DWORD;
   DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of
   IMAGE_DATA_DIRECTORY;
   end;
   IMAGE_OPTIONAL_HEADER = _IMAGE_OPTIONAL_HEADER;

   type
   TISHMisc = packed record
   case Integer of
   0: (PhysicalAddress: DWORD);
   1: (VirtualSize: DWORD);
   end;

   pImageSectionHeader = ^IMAGE_SECTION_HEADER;

   _IMAGE_SECTION_HEADER = packed record
   Name: packed array[0..IMAGE_SIZEOF_SHORT_NAME - 1] of Byte;
   Misc: TISHMisc;
   VirtualAddress: DWORD;
   SizeOfRawData: DWORD;
   PointerToRawData: DWORD;
   PointerToRelocations: DWORD;
   PointerToLinenumbers: DWORD;
   NumberOfRelocations: WORD;
   NumberOfLinenumbers: WORD;
   Characteristics: DWORD;
   end;
   IMAGE_SECTION_HEADER = _IMAGE_SECTION_HEADER;
   {$ENDIF}  *)

type
  PISH = ^IMAGE_SECTION_HEADER;

  PEheader = packed record
    Signature: DWORD;
    _head: IMAGE_FILE_HEADER;
    Opt_head: IMAGE_OPTIONAL_HEADER;
    Section_header: IMAGE_SECTION_HEADER;
  end;

  IMAGE_RESOURCE_DIR_STRING_U = packed record
    Length: WORD;
    NameString: array [0 .. 0] of WChar;
  end;

  PIRDSU = ^IMAGE_RESOURCE_DIR_STRING_U;

  IMAGE_RESOURCE_DIRECTORY_ENTRY = packed record
    Un1: record
    case Integer of 1: (NameIsString: DWORD); // Bit 32
      2: (NameOffset: DWORD); // Bits 31..0
      3: (name: DWORD);
      4: (Id: WORD);
    end;
    Un2: record
      case Integer of
        1:
          (OffsetToData: DWORD);
        2:
          (DataIsDirectory: DWORD); // Bit 32
        3:
          (OffsetToDirectory: DWORD); // Bits 31..0
      end;
    end;
    PIRDirE = ^IMAGE_RESOURCE_DIRECTORY_ENTRY;


  PIDD = ^IMAGE_DATA_DIRECTORY;
  IMAGE_RESOURCE_DIRECTORY = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    NumberOfNamedEntries: WORD;
    NumberOfIdEntries: WORD;
  end;
  PIRD = ^IMAGE_RESOURCE_DIRECTORY;

  IMAGE_RESOURCE_DATA_ENTRY = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  PIRDatE = ^IMAGE_RESOURCE_DATA_ENTRY;
// ---- end SFX needed
// ---- ===== ----- =====  end of implementation data ===== ----- ===== -----

implementation

end.
