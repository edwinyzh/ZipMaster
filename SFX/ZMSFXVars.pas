(******************************************************************)
(* SFX for DelZip v1.8                                            *)
(* Copyright 2002-2004, 2008                                      *)
(*                                                                *)
(* written by Markus Stephany                                     *)
(* modified by Russell Peters, Roger Aelbrecht*)

(* ***************************************************************************
  ZMSFXDialogs.pas - dialog forms SFX for ZipMaster
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

	This file is part of TZipMaster Version 1.9.1.x

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
//modified 2012-04-04
unit ZMSFXVars;

{
  contains all global variables used
  by delzip sfx.

}

interface

uses
  Windows, ZMSFXDefs, ZMSFXInt, ZMSFXStructs;

var
  // the sfx file haeder read from InFile
  VRec_SFXHeader: TSFXFileHeader;

  { TSFXFileHeader strings }
  // caption of the main dialog
  VStr_SFX_Caption,

  // message to show before opening the main dialog
  VStr_SFX_StartMsg,

  // command line read from the special header
  VStr_SFX_CmdLine,

  // default extract path used if reading path from registry failed
  VStr_SFX_RegFailPath,

  // the default-directory stored in the special header
  VStr_SFX_Path: string;

  // may 08, 2002: paramstr(0) replacement
  VStr_ExeName: string;

  {zip processing stuff }
  // crc32 table for unzipping
  VArr_CRC32Table: TCRC32Table;

  // Running CRC (32 bit) value
  VDW_CRC32Val: Cardinal;

  // encryption key
  VArr_CryptKey: array[0..2] of longint;

  // archived file packed size
  VInt_BytesToGo: Int64;//longint;

  // protect from bombs
  VInt_MaxWrite: Int64;

  // input file (i myself)
  VH_InFile: THandle = INVALID_HANDLE_VALUE;

  // file to be written
  VH_OutFile: THandle = INVALID_HANDLE_VALUE;

  // central zip file header
  VRec_ZipHeader: TZ64CentralEntry;

  // file being extracted
  VStr_CurrentFile: string;

  // handle of the main window
  VH_MainWnd: hWnd;

  // password from input box
  VStr_Password: AnsiString = '';

  // actual extract path
  VStr_ExtractPath: string = '';

  // current file position
  VDW_CurrentFilePos: Int64;//Cardinal;

  // position of the TSFXFileHeader/ start of zip archive in the file
  VInt_FileBegin: {Int64;//}longint;

  // cancelled by user (WM_CLOSE during Extract)
  VBool_Cancelled: boolean;

  // possibly moved offset is stored here (wrong sfx creation)
  VDW_OffsetDelta: Int64;//cardinal;

  // shell image list handle
  VH_ShellImageList: THandle;

  // check volatile extract directory deletion
  VBool_CheckDeleteVolatilePath: Boolean = False;
  VStr_VolatilePath: string;
  VStr_VolatilePath_Unexpanded: string;

  // currently open file
  VStr_OutFile: string='';

  // detached archive support
  VH_TempFile: THandle = INVALID_HANDLE_VALUE; // temp file for spanning archive data
  VStr_TempFile: string = ''; // dito
  VInt_SpanType: integer = 0; // 0: not spanned, 1 (SFXSpanTypeSpanned): disk spanning, 2 (SFXSpanTypeMultiVol): multiple archives in one directory (xyz001.zip, xyz002.zip ...)
  VStr_DetachName: string; // name of the detached archive the sfx belongs to
  VStr_DetachExt: string; // file ext of the detached archive the sfx belongs to
  VBool_FixedDrive: boolean; // is drive fixed or removable
  VStr_SourceDir: string; // source directory of the spanned archives

  // multi-language support
//  VRec_Strings: PWord = nil;   // pointer to table of strings
  VP_SBuf: pByte = nil;
//  VRec_SHeader: TSFXStringsHeader;
//  VInt_LangOfs: longint = 0;     // offset of Langs header
//  VP_Langs: PSFXStringsHeader = nil;  // pointer to available languages
  VInt_CP: Integer = 0;   // selected codepage
  VInt_CurLang: integer = 0; // selected language entry
//  VRec_Langs: PSFXStringsEntry = nil; // pointer to available languages
//  VStr_Lang: string = '';    // the selected langauge
//  VInt_CmdLineSize: integer = 0;
{$ifdef DEBUG_SFX}
  Test_Stub_Size: Integer = 0;
{$ENDIF}
  VInt_LastSeq: Integer = -1;

implementation

end.
