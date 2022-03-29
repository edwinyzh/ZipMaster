unit ZMSFXInt;

// this unit contains those definitions used by delzipsfx which
// may be used by 3rd party units (like TZipMaster) too.

(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* SFX for DelZip v1.80                                           *)
(* Copyright 2002-2005, 2008                                      *)
(*                                                                *)
(* originally written by Markus Stephany                          *)
(* modified by Russell Peters, Roger Aelbrecht                    *)
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
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
// modified 12-Nov-2009
{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  WinApi.Windows;
{$ELSE}
  Windows;
{$ENDIF}

const
  // 'DZ'#0#12;  // header signature
  SFX_HEADER_SIG = $0C005A44;
  // 'DZ'#7#12; // end of header signature
  // SFX_HEADER_END_SIG = $0C075A44;
  // // 'DZ'#0#1;  // header signature
  // SFX_HEADER_SIG = $01005A44;
  // // 'DZ'#10#1; // end of header signature
  // SFX_HEADER_END_SIG = $010A5A44;


  // 'DZ'#0#13; // detached header signature
  // @@  SFX_DETACHED_HEADER_SIG = $0D005A44;
  // 'DZ'#7#13; // end of detached header signature
  // SFX_DETACHED_HEADER_END_SIG = $0D075A44;
  // // 'DZ'#20#1; // detached header signature
  // SFX_DETACHED_HEADER_SIG = $01145A44;
  // // 'DZ'#20#2; // end of detached header signature
  // SFX_DETACHED_HEADER_END_SIG = $02145A44;

const
  // enum TSFXOverwriteMode { somOverwrite, somSkip, somAsk };
  Som_Overwrite = 0;
  Som_Skip = 1;
  Som_Ask = 3;

  So_AskCmdLine = $001;
  So_AskFiles = $002;
  So_HideOverWriteBox = $004;
  So_AutoRun = $008;
  So_NoSuccessMsg = $010;
  So_ExpandVariables = $020;
  So_InitiallyHideFiles = $040;
  So_ForceHideFiles = $080;
  So_CheckAutoRunFileName = $100;
  So_CanBeCancelled = $200;
  // so_Detached             = $400;  // has no meaning in the stub
  So_CreateEmptyDirs = $800;
  So_SuccessAlways = $1000;
  So_CompressedCmd = $8000;

  // command string IDs
  Sc_Caption = $01;
  Sc_Path = $02;
  Sc_CmdLine = $03;
  Sc_RegFailPath = $04;
  Sc_StartMsg = $05;
  // sc_DetachedName         = $06;   // optional - does not include number
  // sc_DetachedExtn         = $07;   // optional - includes leading dot

type
  // { file overwrite modes }
  // TSFXOverwriteMode = (somOverwrite, somSkip, somAsk);
  //
  // { options }
  // TSFXOption = (
  // soAskCmdLine, // allow user to prevent execution of the command line
  // soAskFiles, // allow user to prevent certain files from extraction
  // soHideOverWriteBox, // do not allow user to choose the overwrite mode
  // soAutoRun, // start extraction + evtl. command line automatically
  // // only if sfx filename starts with "!" or is "setup.exe"
  // soNoSuccessMsg, // don't show success message after extraction
  // soExpandVariables, // expand environment variables in path/cmd line...
  // soInitiallyHideFiles, // dont show file listview on startup
  // soForceHideFiles, // do not allow user to show files list
  // // (no effect if shfInitiallyShowFiles is set)
  // soCheckAutoRunFileName, // can only autorun if !... or setup.exe
  // soCanBeCancelled, // extraction can be cancelled
  // soDetached, // tells the stub that it has been created separate from the archive
  // soCreateEmptyDirs // recreate empty directories
  // );

  // set of TSFXOption
  // TSFXOptions = set of TSFXOption;
  (*
   { sfx Strings header }
   PSFXStringsHeader = ^TSFXStringsHeader;
   TSFXStringsHeader = packed record
   // signature: DZ#30#1 = version 01
   Signature: DWORD;
   // overall strings size (must be DWORD-Aligned!)  = header + string blocks
   Size: word;
   // number of language blocks
   Count: word;
   end;

   { sfx Strings records - for each language}
   PSFXStringsEntry = ^TSFXStringsEntry;
   TSFXStringsEntry = packed record
   //    CPage: WORD;    // target code page
   LangID: WORD;   // target locale
   DOfs: WORD;    // offset from SFXStringsHeader to SFXStringsData
   end;
  *)
  PSFXStringsData = ^TSFXStringsData;

  TSFXStringsData = packed record
    // overall block size (must be DWORD-Aligned!)  = header + compressed strings
    CSize: WORD; // compressed size (bytes)
    USize: WORD; // uncompressed size (bytes)
    CRC: DWORD;
    { data : array [1..DSize] of WORD - zip compressed }
  end;

  // Strings entries ident: word, siz: word, wchar[siz] ... followed 0;
  (*
   // the following record is only used by TZipSFX and TZipMasterSFX to quickly find the
   // TSFXStringsHeader in the file, it's not used by the sfx itself.
   // it's immediately stored after the TSFXStrings records
   PSFXStringsEndOfHeader= ^TSFXStringsEndOfHeader;
   TSFXStringsEndOfHeader= packed record
   Signature: DWORD;    //must be SFX_HEADER_END_SIG
   HeaderSize: DWORD;   //must be equal to TSFXStringsHeader.Size (dword for alignment issuses)
   end;
  *)
  // compressed language string data - stored in resources
  PSFX_LanguageData = ^TSFX_LanguageData;

  TSFX_LanguageData = packed record
    LangID: WORD; // target locale
    // the rest is TSFXStringsData
    // overall block size (must be DWORD-Aligned!)  = header + compressed strings
    CSize: WORD; // compressed size (bytes)
    USize: WORD; // uncompressed size (bytes)
    CRC: DWORD;
    { data : array [1..DSize] of WORD - zip compressed }
  end;
  // Strings entries ident: word, siz: word, wchar[siz] ... followed 0;

  { sfx file header }
  PSFXFileHeader = ^TSFXFileHeader;

  TSFXFileHeader = packed record
    // signature: DZ#0#1 = version 01
    Signature: DWORD;
    // overall header size (must be DWORD-Aligned!)
    Size: Word;
    // flags
    Options: WORD; // TSFXOptions;
    // default overwrite mode
    DefOVW: WORD; // TSFXOverwriteMode;
    // MB_... (only used if StartMsgSize > 0)
    StartMsgType: WORD; // DWORD;
    // Compressed: byte;  // if <> 0 TSFXStringsData will follow otherwise TSFXStringsHeader
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

  (******************************)
  (* SFX for DelZip v1.7        *)
  (* Copyright 2002-2005        *)
  (* written by Markus Stephany *)
const
  SFX_LANG_DEF = 127;
  SFX_LANG_BASE = 128;

const
  // 'DZ'#0#1;  // header signature
  SFX_HEADER_SIG_17 = $01005A44;
  // 'DZ'#10#1; // end of header signature
  SFX_HEADER_END_SIG_17 = $010A5A44;

  // 'DZ'#20#1; // detached header signature
  SFX_DETACHED_HEADER_SIG_17 = $01145A44;
  // 'DZ'#20#2; // end of detached header signature
  SFX_DETACHED_HEADER_END_SIG_17 = $02145A44;

  // type
  { file overwrite modes }
  // TSFXOverwriteMode_17 = (somOverwrite, somSkip, somAsk);

  { options }
  // TSFXOption_17 = (
const
  So_AskCmdLine_17 = $01; // allow user to prevent execution of the command line
  So_AskFiles_17 = $02; // allow user to prevent certain files from extraction
  So_HideOverWriteBox_17 = $04;
  // do not allow user to choose the overwrite mode
  So_AutoRun_17 = $08; // start extraction + evtl. command line automatically
  // only if sfx filename starts with "!" or is "setup.exe"
  So_NoSuccessMsg_17 = $10; // don't show success message after extraction
  So_ExpandVariables_17 = $20;
  // expand environment variables in path/cmd line...
  So_InitiallyHideFiles_17 = $40; // dont show file listview on startup
  So_ForceHideFiles_17 = $80; // do not allow user to show files list
  // (no effect if shfInitiallyShowFiles is set)
  So_CheckAutoRunFileName_17 = $100; // can only autorun if !... or setup.exe
  So_CanBeCancelled_17 = $200; // extraction can be cancelled
  So_Detached_17 = $400;
  // tells the stub that it has been created separate from the archive
  So_CreateEmptyDirs_17 = $800; // recreate empty directories
  // );

  // set of TSFXOption
  // TSFXOptions_17 = set of TSFXOption_17;
type
  { sfx file header }
  PSFXFileHeader_17 = ^TSFXFileHeader_17;

  TSFXFileHeader_17 = packed record
    // signature: DZ#0#1 = version 01
    Signature: Cardinal;
    // overall header size (must be DWORD-Aligned!)
    Size: Word;
    // flags
    Options: Word; // TSFXOptions_17;
    // default overwrite mode
    DefOVW: Byte; // TSFXOverwriteMode_17;
    // length of the sfx dialog caption (0=default caption)
    CaptionSize,
    // length of the sfx default extraction path (0=current dir)
    PathSize,
    // length of the command line (0=no command line)
    CmdLineSize,
    // length of the extract path to be used if HK... failed (0=%temp%)
    RegFailPathSize,
    // length of the startup message (0=no message)
    StartMsgSize: Byte;
    // MB_... (only used if StartMsgSize > 0)
    StartMsgType: Cardinal;
    {
     Caption: string;    if CaptionSize > 0, contains the caption of the sfx dialog (not #0-terminated)
     Path: string;       if PathSize > 0, contains the default extraction path (not #0-terminated)
     CmdLine: string;    if CmdLineSize > 0, contains a command to execute after extraction completed
     RegFailPath:        string if RegFailPathSize > 0, contains the default extraction path,
     StartMsg: string;   if StartMsgSize > 0, contains a message to display before showing the main dialog.
    }
  end;

type
  // the following record is only used by TZipSFX and TZipMasterSFX to quickly find the
  // TSFXFileHeader in the file, it's not used by the sfx itself.
  // it's immediately stored after the TSFXFileHeader record
  PSFXFileEndOfHeader_17 = ^TSFXFileEndOfHeader_17;

  TSFXFileEndOfHeader_17 = packed record
    Signature: DWORD; // must be SFX_HEADER_END_SIG
    HeaderSize: DWORD;
    // must be equal to TSFXFileHeader.Size (dword for alignment issuses)
  end;

  // in detached sfx archives, the sfx contains an additional record following the header
  PSFXDetachedHeader_17 = ^TSFXDetachedHeader_17;

  TSFXDetachedHeader_17 = packed record
    Signature: DWORD;
    // must be SFX_DETACHED_HEADER_SIG or SFX_DETACHED_HEADER_END_SIG
    NameLen, ExtLen: Cardinal;
    // contains the length of the filename/fileext of the archive (without path)
    {
     Name: string      if NameLen > 0, Name contains the file name (without extension) of
     the detached archive
     Ext: string      if ExtLen > 0, Ext contains the file Ext (without leading .) of
     the detached archive
    }
  end;

implementation

(* the structure of a zipsfx-file :

  - zipsfx-executable code (0-xxxxx)

 //  TSFXStringsHeader
 //    TSFXStringsBlock 1
 //        ...
 //    TSFXStringsBlock n
 //  TSFXStringsEndOfHeader

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
 - TSFXDetachedHeader
 - filename  (optional)
 - TSFXDetachedEndOfHeader
 - centraldirectories[] + endofcentraldir of the zip archive


*)

end.
