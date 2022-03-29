 (******************************************************************)
 (* SFX for DelZip v1.8                                            *)
 (* Copyright 1997, Microchip Systems / Carl Bunton                *)
 (* e-mail: Twojags@cris.com                                       *)
 (* Web-page: http://www.concentric.net/~twojags                   *)
 (*                                                                *)
 (* modified by Markus Stephany                                    *)
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
unit ZMSFXProcs;

{
this unit contains utility functions and main function used by delzipsfx

}

interface

{ modifications marked with ##FR are enhancements and bug fixes by
Frank Reichert F.Rei@gmx.de, thanks!






!!!!!!!!!!!!! spanning/multivolume support based on Roger Aelbrecht's BCB
version of the sfx; thanks a lot Roger! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


}

uses Messages, Windows, ZMSFXDefs, ZMSFXInt, ZMSFXStrings,
{$ifdef DEBUG_SFX}
   SysUtils,  // Run1,
{$endif}
  ZMSFXStructs;

// execute  
procedure Run;  

 // resize or move a control on the main dialog (or the dialog itself)
 // depending on the visibility of the files listview
procedure ResizeControl(const wnd: HWND; const bReposition: boolean; yDiff: integer);

 // enable/disable all child controls of the given window
 // except of progress bars
procedure EnableChildren(const wnd: HWND; const bEnable: boolean);

// get the text for the run checkbox
function GetRunCheckBoxText: string;

// get an error message if ExcuteCMD failed
function GetRunErrorMessage: string;

// compare two strings / case insesitive
function CompareText(const s1, s2: string): boolean;

// get an argument out of VStr_SFX_CmdLine
function GetArgument(const iIndex: integer): string;

// check whether the file to execute is an .inf installation file
function TestForInf(const sr1: string): boolean;

// format a string (replace '><' by args)
function FmtStr1(const sFormat: string; const arg1: string): string;
function FmtStrID1(id: integer; const arg1: string): string;
function FmtStr2(const sFormat: string; const arg1, arg2: string): string;
//function FmtStr3(const sFormat: string; const arg1, arg2, arg3: string): string;
//function FmtStrID2(id: integer; const arg1, arg2: string): string;

 // angus johnson, ajohnson@rpi.net.au
 // set the filetime of an extracted file to the value stored in the archive
procedure FileSetDate(const hFile: THandle; const iAge: integer);

// unstore the current archive file / uncompressed
procedure Unstore;

// force the existence of a directory (and its parents)
function ForceDirectories(sDir: string): boolean; // RCV04

// change input file position
function FSeek(const Offset: int64; const MoveMethod: word): int64; //##FR was procedure

// fill crc32 buffer
procedure Crc32_Buf(str: PByte; len: integer; var crc: cardinal);

// handle relative paths, strip directory name
function ExtractFileName(const sFileName: string): string;

// return the smaller value
function Min(const I1, I2: longint): longint;

// ensure trailing backslash
function AppendDirSeparator(const sDir: string): string; //##FR modified

// ensure NO trailing backslash
function RemoveDirSeparator(const sDir: string): string;

// does the directory exist?
function DirectoryExists(const sDir: string): boolean;

// does the file exist?
function FileExists(const sFileName: string): boolean;

// extract the file's path
function ExtractFilePath(const sFilename: string): string;

// replace environment vars by their contents
function ExpandEnv(const Str: string): string;

// show a message box
function MsgBox(const wndpar: HWND; const sMsg, sTitle: string;
  const uType: cardinal): integer;

// show an error message
procedure ErrorMsgBox(const wndPar: HWND; const sMsg: string);
procedure ErrorMsgBoxFmt1(const wndPar: HWND; id: integer; const arg1: string);

// read from input file
function FRead(var Buffer; const cNum: cardinal): cardinal;

// read from a file and bail if not all data could be read
procedure CheckFRead(var Buffer; const cNumBytes: cardinal);

// write to a file and bail if not all data could be written
procedure CheckFWrite(const FH: THandle; const Buffer; const cNumBytes: cardinal;
  const FileName: string);


// read TSFXFileHeader from input file
procedure GetDefParams;

// execute the command-line read from the sfx header, if any
function ExecuteCMD: cardinal;

// check password
function decrypt_pw(Encrypt_Head: PAnsiChar; EncHead_len: byte; BitFlag: word;
    CRC, FileDate: longint; const sPassword: AnsiString): boolean;

// decrypt arcive contents
function decrypt_byte: integer;

// Update the encryption keys with the next byte of plain text
procedure UpdateKeys(c: byte);

// initially fill the crc table
procedure Make_CRC32Table;

 // from Angus Johnson's TZip-SFX code:
 // get the executable's file size to get rid of caring about the exe size
function GetExeSize: cardinal;

 // from Angus Johnson's TZip-SFX code:
 // fill the listview
procedure FillListView(wndOwner: hWnd);

// fatal error, exit                      
procedure ErrorHaltID(id: integer); overload;
procedure ErrorHaltFmt(id: integer; const arg1: string);
procedure ErrorHalt(const sMsg: string);

function Extract(wndOwner: hWnd): boolean;

function StrGetEditText(wndPar: HWND): string;


// listview handling
procedure AddFilesListViewCol(const wndDlg: HWND; const iIndex: integer;
  const szCaption: string; const iDirection, iWidth: integer);

procedure SelectAllInFilesListView(const wndDlg: HWND);

// get current directory
function GetCurDir: string;

// add an entry to the list view
procedure AddFileToList(const wndOwner: HWND; const sName: string;
  const Rec: TZ64CentralEntry; const IsDir: boolean);
//  const Rec: TZipCentralHeader; const IsDir: boolean);

procedure SetLangStrings(hLC: hWnd);

// return pointer to temporary buffer of at least size
function GetXBuf(size: integer): pByte;

// dispatch windows messages
procedure ProcessMessages;

// Int to Str
function Int2Str(n: int64; wide: integer = -1): String;

// return the Detached name
function DetachedName(const num: string): string;

function LoadResource(id: integer): Pointer;

// cleanup, free globals
procedure Finish;
//{$endif}

// check codepage of filename
implementation

uses
  ZMSFXDialogs, ZMSFXVars, ZMSFXInflate, ZMSFXWinTrust;

var
  xbuf: PByte = nil;
  xbufsize: integer = 0;

// return pointer to temporary buffer of at least size
function GetXBuf(size: integer): pByte;
begin
  if (xbuf <> nil) and (xbufsize > 0) and (size <= xbufsize) then
  begin
    // use the existing buffer
    Result := pByte(xbuf);
    Exit;
  end;

  if (size > xbufsize) or (size < 0) or (xbuf = nil) or (xbufsize <= 0) then
  begin
    // clear old buf
    if xbuf <> nil then
      FreeMem(xbuf);
    xbuf := nil;
    xbufsize := 0;
    Result := nil;
    if size <= 0 then
      exit;
  end;
  xbufsize := succ(size or $3FF);
  GetMem(xbuf, xbufsize);
  Result := pByte(xbuf);
  if Result = nil then
    ErrorHalt('no memory');  // probably need error message
end;

type
  TCharSet = set of AnsiChar;
function CharInSet(c: Char; theSet: TCharSet): boolean;
{$IFDEF UNICODE}
var
  ac: AnsiChar;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := False;
  if c < HIGH(AnsiChar) then
  begin
    ac := AnsiChar(Ord(c) and $FF);
    Result := ac in theSet;
  end;
{$ELSE}
    Result := c in theSet;
{$ENDIF}
end;

procedure BadArchive;
begin
  ErrorHaltFmt(SFX_Err_Archive, VStr_ExeName);
end;

// find extra data tag
//   IN x => extra data, size = length extra data
//   found x => tag data, size = data size, result = true
//   not found size = <= 0, result = true;
function ExtraData(var x: pByte; var size: integer; tag: word): boolean;
type
  TagHead = packed record
    tg: WORD;
    sz: WORD;
  end;
  pTagHead = ^TagHead;
var
  hed: TagHead;
begin
  Result := False;
//  size := 0;
  while size > (sizeof(TagHead) + 2) do
  begin
    hed := pTagHead(x)^;
    dec(size, sizeof(TagHead));
    if hed.tg = Tag then
    begin                                   
      Result := hed.sz <= size;
      if Result then
      begin
        inc(x, sizeof(TagHead));
        size := hed.sz;
      end
      else
        size := -1; // invalid
      exit;
    end;
    dec(size, hed.sz);
    inc(x, hed.sz + sizeof(TagHead));
  end;
  size := 0;
end;

(*----------------------------------------------------------------------------
3.4.0.0 17 Oct 2007 RA new function check if EOC is needed
*)
function NeedEOC64(const EOC: TZipEndOfCentral): bool;
begin
  Result := ((EOC.TotalEntries = MAX_WORD) or (EOC.CentralOffSet = MAX_UNSIGNED) or
    (EOC.CentralEntries = MAX_WORD) or (EOC.CentralSize = MAX_UNSIGNED) or
    (EOC.ThisDiskNo = MAX_WORD) or (EOC.CentralDiskNo = MAX_WORD));
end;

(*----------------------------------------------------------------------------
3.4.0.0 17 Oct 2007 RA new function locate and read EOC64
result:= 0 = OK ; <0 error
*)
procedure GetEOC64(EOCOffset: word; var EOC64: TZipEOC64);
var
  Posn: int64;
  Loc:  TZip64EOCLocator;
begin
  Posn := EOCOffset - SizeOf(TZip64EOCLocator);
  //  TZip64EOCLocator Loc;
  if (Posn >= 0) then
  begin
    FSeek(Posn, FILE_BEGIN);
    if (FRead(Loc, SizeOf(TZip64EOCLocator)) <> SizeOf(TZip64EOCLocator)) then
      BadArchive;
    if (Loc.LocSig = ZipEOC64LocatorSig) then
    begin
      // locator found
      if (FSeek(int64(Loc.EOC64RelOfs), 0) < 0) then
        BadArchive;
      if (FRead(EOC64, SizeOf(TZipEOC64)) <> SizeOf(TZipEOC64)) then
        BadArchive;
      if (EOC64.EOC64Sig <> ZipEndCentral64Sig) then
        BadArchive;
    end;
  end;
end;

(*----------------------------------------------------------------------------
3.4.0.0 12 May 2007 RA new function
  copy CFH to Z64CFH and read Z64 data if needed
*)
procedure GetZ64Entry(const CFH: TZipCentralHeader; var Z64CFH: TZ64CentralEntry);
var
  xlen: integer;
  p: pByte;
  wsz: word;
begin
  Move(CFH.HeaderSig, Z64CFH.HeaderSig, 22);  // copy headersig to crc32
  Z64CFH.FileNameLen := CFH.FileNameLen;
  Z64CFH.ExtraLen  := CFH.ExtraLen;
  Z64CFH.FileComLen := CFH.FileComLen;
  Z64CFH.IntFileAtt := CFH.IntFileAtt;
  Z64CFH.ExtFileAtt := CFH.ExtFileAtt;
  Z64CFH.ComprSize := CFH.ComprSize;    // values to be corrected for Z64
  Z64CFH.UnComprSize := CFH.UnComprSize;
  Z64CFH.RelOffLocal := CFH.RelOffLocal;
  Z64CFH.DiskStart := CFH.DiskStart;
  Z64CFH.MTime := 0;
  Z64CFH.ATime := 0;
  Z64CFH.CTime := 0;
  if CFH.ExtraLen = 0 then
    Exit; // no extra data
  // any ntfs stamps?
  xlen := CFH.ExtraLen;
  p := xbuf;
//  if ExtraData(p, xlen, NTFS_STAMP_TAG) and (xlen >= 24) then
  if ExtraData(p, xlen, NTFS_STAMP_TAG) and (xlen >= 32) then
  begin
    Inc(p, 4);  // skip Reserved and find sub-tag 1
    if ExtraData(p, xlen, 1) and (xlen >= 24) then
    begin
      Z64CFH.MTime := PXNTFData(p)^.MTime;
      Z64CFH.ATime := PXNTFData(p)^.ATime;
      Z64CFH.CTime := PXNTFData(p)^.CTime;
    end;
  end;
  if (CFH.VersionNeed < 45) {or (CFH.ExtraLen = 0)} then
    Exit; // nocorrection needed
  if (CFH.UnComprSize <> MAX_UNSIGNED) and (CFH.ComprSize <> MAX_UNSIGNED) and
      (CFH.RelOffLocal <> MAX_UNSIGNED) and (CFH.DiskStart = MAX_WORD) then
    Exit; // not Zip64
  xlen := CFH.ExtraLen;
  p := xbuf;
  if not ExtraData(p, xlen, Zip64_data_tag) then
    BadArchive;   // no Zip64 data
  wsz := xlen;
  if (CFH.UnComprSize = MAX_UNSIGNED) then
  begin
    if (wsz < 8) then
      BadArchive;
    Z64CFH.UnComprSize := pInt64(p)^;
    Inc(p, Sizeof(int64));
    wsz := wsz - word(SizeOf(int64));
  end;
  if (CFH.ComprSize = MAX_UNSIGNED) then
  begin
    if (wsz < 8) then  
      BadArchive;
    Z64CFH.ComprSize := pInt64(p)^;
    Inc(p, Sizeof(int64));
    wsz := wsz - word(SizeOf(int64));
  end;
  if (CFH.RelOffLocal = MAX_UNSIGNED) then
  begin
    if (wsz < 8) then
      BadArchive;
    Z64CFH.RelOffLocal := pInt64(p)^;
    Inc(p, Sizeof(int64));
    wsz := wsz - word(SizeOf(int64));
  end;
  if (CFH.DiskStart = MAX_WORD) then
  begin
    if (wsz < 4) then
      BadArchive;
    Z64CFH.DiskStart := pInt64(p)^;
  end;
end;

// return default Ansi codepage for locale
function DefCP(LangID: integer): integer;
var
  tmp: array[0..15] of char;
  i: integer;
  c: char;
begin
  Result := 0;
  if GetLocaleInfo(LangID, LOCALE_IDEFAULTANSICODEPAGE, PChar(@tmp[0]), 6) <> 0 then
  begin
    Result := 0;
    i := 0;
    c := tmp[0];
    while CharInSet(c, ['0'..'9']) do
    begin
      Result := (Result * 10) + (ord(c)-ord('0'));
      inc(i);
      if i > 6 then
        break;
      c := tmp[i];
    end;
  end;
end;

// set the filetime of an extracted file to the value stored in the archive
procedure FileSetDate(const hFile: THandle; const iAge: integer);
var
  LocalFileTime, FileTime: TFileTime;
begin
  DosDateTimeToFileTime(HIWORD(iAge), LOWORD(iAge), LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, FileTime);
  SetFileTime(hFile, nil, nil, @FileTime);
end;

(*--------------------------------------------------------------------------*)

function UpdC32(Octet: byte; Crc: cardinal): cardinal;
begin
  Result := VArr_CRC32Table[byte(Crc xor cardinal(Octet))] xor
    ((Crc shr 8) and $00FFFFFF);
end;

(*--------------------------------------------------------------------------*)

// fill crc32 buffer

procedure Crc32_Buf(str: PByte; len: integer; var crc: cardinal);
begin
  while len > 0 do
  begin
    crc := UpdC32(byte(str^), crc);
    Inc(str);
    Dec(len);
  end;
end;

(*--------------------------------------------------------------------------*)

// return the smaller value

function Min(const I1, I2: longint): longint;
begin
  if I2 < I1 then
    Result := I2
  else
    Result := I1;
end;

(*--------------------------------------------------------------------------*)

// unstore the current archive file / uncompressed

procedure Unstore;
var
  c: cardinal;
  cNumBytes: TWriteFileWritten;
  OutBuf: PAnsiChar;
begin
  GetMem(OutBuf, Min(VInt_BytesToGo, WSIZE) + 2);
  try
    while VInt_BytesToGo > 0 do
    begin
      cNumBytes := Min(VInt_BytesToGo, WSIZE);
      CheckFRead(OutBuf^, cNumBytes);
      Dec(VInt_BytesToGo, cNumBytes);
      if (VRec_ZipHeader.Flag and 1) = 1 then
        for c := 0 to cNumBytes - 1 do
        begin
          OutBuf[c] := AnsiChar(Byte(OutBuf[c]) xor decrypt_byte);
          {update_keys} UpdateKeys(byte(OutBuf[c]));
        end;
      CheckFWrite(VH_OutFile, OutBuf^, cNumBytes, VStr_OutFile);
      Crc32_Buf(PByte(outbuf), cNumBytes, VDW_CRC32Val);
    end;
  finally
    FreeMem(OutBuf);
  end;
end;

(*--------------------------------------------------------------------------*)


// change input file position

function FSeek(const Offset: int64; const MoveMethod: word): int64; //##FR
{$IFDEF VERD6up}
begin
  Result := FileSeek(VH_InFile, Offset, MoveMethod);
end;
{$ELSE}
type
  I64Rec = packed record
    case integer of
      0: (I: int64);
      1: (Lo, Hi: cardinal);
  end;
var
  r: I64Rec;
begin
  r.I  := Offset;
  r.Lo := SetFilePointer(VH_InFile, integer(r.Lo), @r.Hi, MoveMethod);
  if (r.Lo = cardinal(-1)) and (GetLastError <> 0) then
    r.I := -1;
  Result := r.i;
end;

{$ENDIF}
(*--------------------------------------------------------------------------*)

// read from input file

function FRead(var Buffer; const cNum: cardinal): cardinal;
var
  dummy: TWriteFileWritten;
begin
  if ReadFile(VH_InFile, Buffer, cNum, dummy, nil) then
    Result := dummy
  else
    Result := 0;
end;

(*--------------------------------------------------------------------------*)


// read from a file and bail if not all data could be read

procedure CheckFRead(var Buffer; const cNumBytes: cardinal);
var
  Read: TWriteFileWritten;
begin
  Read := 0;

  if (not ReadFile(VH_InFile, Buffer, cNumBytes, Read, nil)) or
    (cardinal(Read) <> cNumBytes) then
    ErrorHaltID(SFX_Err_ArchiveCorrupted);
end;

(*--------------------------------------------------------------------------*)


// write to a file and bail ifnot all data could be written

procedure CheckFWrite(const FH: THandle; const Buffer; const cNumBytes: cardinal;
  const FileName: string);
var
  Written: TWriteFileWritten;
begin
  Written := 0;
  // stop overrun
  if cNumBytes > VInt_MaxWrite then
    ErrorHaltFmt(SFX_Err_CannotWriteFile, FileName);
  // write to memory if file not open and address set
  if (FH = INVALID_HANDLE_VALUE) and (VP_SBuf <> nil) then
  begin
    Move(Buffer, VP_SBuf^, cNumBytes);
    Inc(VP_SBuf, cNumBytes);
    Written := cNumBytes;
  end
  else

    // don't know why, but sometimes WriteFile returns FALSE though
    // all bytes have successfully been written, so do not check the API's result
    WriteFile(FH, Buffer, cNumBytes, Written, nil);

  // seems to reliably show that all's ok or not
  if cardinal(Written) <> cNumBytes then
    ErrorHaltFmt(SFX_Err_CannotWriteFile, FileName);
  VInt_MaxWrite := VInt_MaxWrite - cNumBytes;
end;

(*--------------------------------------------------------------------------*)


// extract the file's path
function ExtractFilePath(const sFilename: string): string;
var
  i: integer;
begin
  (* Handle archive relative paths *)
  i := Length(sFilename);
  if (i = 3) and (Pos(':', sFilename) > 0) then
    Result := sFilename
  else
  begin
    while (i > 0) and not CharInSet(sFilename[i], ['\', '/', ':']) do
      Dec(i);
    if i > 0 then
    begin
      if CharInSet(sFilename[i], ['\', '/']) then
        if i <> 3 then
          Dec(i)
        else
        if sFilename[2] <> ':' then
          Dec(i);
    end;
    Result := Copy(sFilename, 1, i);
  end;
end;

(*--------------------------------------------------------------------------*)

// handle relative paths, strip directory name

function ExtractFileName(const sFileName: string): string;
var
  I: integer;
begin
  (* Handle archive relative paths *)
  I := Length(sFileName);
  while (I > 0) and not CharInSet(sFileName[I], ['\', '/', ':']) do
    Dec(I);
  Result := Copy(sFileName, I + 1, MaxInt);
end;

(*--------------------------------------------------------------------------*)


// does the directory exist?
function DirectoryExists(const sDir: string): boolean;
var
  Code: integer;
begin
  Code := GetFileAttributes(PChar(sDir));
  Result := (Code <> -1) and ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
end;

(*--------------------------------------------------------------------------*)

// does the file exist?
function FileExists(const sFileName: string): boolean;
var
  Code: Cardinal;
begin
  Code := GetFileAttributes(PChar(sFileName));
  Result := (Code <> Cardinal(-1));
end;

(*--------------------------------------------------------------------------*)

// force the existence of a directory (and its parents)

function ForceDirectories(sDir: string): boolean;
begin
  Result := True;
  sDir := RemoveDirSeparator(sDir);
  if Length(sDir) = 0 then
    exit;
  if DirectoryExists(sDir) or (ExtractFilePath(sDir) = sDir) then
    Exit; // avoid 'xyz:\' problem.
  if not ForceDirectories(ExtractFilePath(sDir)) then
    Result := False
  else
    Result := CreateDirectory(PChar(sDir), nil);
end;

(*--------------------------------------------------------------------------*)

// ensure trailing backslash

function AppendDirSeparator(const sDir: string): string; //##FR modified
var
  i: integer;
begin
  i := Length(sDir);
  Result := sDir;
  if i > 0 then
    if Result[i] <> Chr_DirSep then
      Result := Result + Chr_DirSep;
end;

(*--------------------------------------------------------------------------*)

// ensure NO trailing backslash

function RemoveDirSeparator(const sDir: string): string;
begin
  Result := sDir;
  while (Length(Result) > 0) and (Result[Length(Result)] = Chr_DirSep) do
    SetLength(Result, Length(Result) - 1);
end;

(*--------------------------------------------------------------------------*)

// expand environment variables

function ExpandEnv(const Str: string): string;
var
  pch: PChar;
begin
  GetMem(pch, MAX_PATH * 2);
  try
    FillChar(pch^, MAX_PATH * 2, 0);
    if ExpandEnvironmentStrings(PChar(Str), pch, (MAX_PATH * 2) - 1) > 0 then
      Result := pch
    else
      Result := '';
  finally
    FreeMem(pch);
  end;
end;

(*--------------------------------------------------------------------------*)

// show a message box

function MsgBox(const wndpar: HWND; const sMsg, sTitle: string;
  const uType: cardinal): integer;
begin
  Result := MessageBox(wndPar, PChar(sMsg), PChar(sTitle), uType or MB_TASKMODAL);
end;

(*--------------------------------------------------------------------------*)
// show an error message

procedure ErrorMsgBox(const wndPar: HWND; const sMsg: string);
begin
  MsgBox(wndPar, sMsg, PChar(SFXString(SFX_Cap_Err)), MB_ICONSTOP);
end;

procedure ErrorMsgBoxFmt1(const wndPar: HWND; id: integer; const arg1: string);
begin
  ErrorMsgBox(wndPar, FmtStrID1(id, arg1));
end;

(*--------------------------------------------------------------------------*)

// compare two strings / case insensitive

function CompareText(const s1, s2: string): boolean;
begin
  Result := (Length(s1) = Length(s2)) and (lstrcmpi(PChar(s1), PChar(s2)) = 0);
end;

(*--------------------------------------------------------------------------*)

// to check correct file size of the input file +++ 08/13/98

function FindEOCRecord: cardinal;
var
  pRec: PZipEndOfCentral;
  cBufferSize, cRead, cFilePos: cardinal;
  pBuffer: PAnsiChar;
  cCurrentPos: cardinal;
  c: cardinal;
  //loop counter only var for compiler optimization (register value) ##FR
  bOK: boolean;
begin
  Result := HIGH(cardinal);
  // get the needed size of the buffer ( max 65536 + SizeOf( eocd ), min SizeOf( file ) )
  bOK := False;
  cBufferSize := GetFileSize(VH_InFile, nil) - cardinal(VInt_FileBegin);
  if cBufferSize > 65558 then
    cBufferSize := 65558;

  if cBufferSize > sizeof(TZipEndOfCentral) then
    //if smaller, then no correct zip file
  begin
    GetMem(pBuffer, cBufferSize);
    try
      cCurrentPos := FSeek(0, FILE_CURRENT); //##FR mark the current file pos
      cFilePos := FSeek(-cBufferSize, FILE_END);
      //FSeek is now a function, not a proc, see sfxmisc.pas
      cRead := FRead(pBuffer[0], cBufferSize);
      FSeek(cCurrentPos, FILE_BEGIN); //##FR jump back to marked filepos
      if cRead = cBufferSize then
        for c := 0 to cBufferSize - sizeof(TZipEndOfCentral) do
        begin
          pRec := Pointer(cardinal(pBuffer) + c);
          if pRec^.HeaderSig = ZipEndOfCentralSig then
          begin
            // eocd is found, now check if size is correct ( = pos+22+eocd.commentsize)
            if (pRec^.ZipCommentLen + cFilePos + c +
              sizeof(TZipEndOfCentral)) = GetFileSize(VH_InFile, nil) then
            begin
              bOK := True; // set ok flag
              Result := cFilePos + c;
              Break;
            end;
          end;
        end;
    finally
      FreeMem(pBuffer);
    end;
  end;

  if not bOK then
    ErrorHaltID(SFX_Err_ArchiveCorrupted);
end;

(*--------------------------------------------------------------------------*)

// get the index of a string in an array / case insensitive

function StrArrayIndexOf(s1: string; const args: array of string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := Low(args) to High(args) do
    if CompareText(s1, args[i]) then
    begin
      Result := i;
      Break;
    end;
end;

(*--------------------------------------------------------------------------*)

// read a sfx header string
{$IFDEF UNICODE}
procedure ReadSFXString(var sToRead: string; const iLen: integer);
var
  utf8s: UTF8String;
begin
  if iLen > 0 then
  begin
    SetLength(utf8s, iLen);
    CheckFRead(utf8s[1], iLen);
  end;
  sToRead := String(utf8s);
end;
{$ELSE}
procedure ReadSFXString(var sToRead: string; const iLen: integer);
begin
  if iLen > 0 then
  begin
    SetLength(sToRead, iLen);
    CheckFRead(sToRead[1], iLen);
  end;
end;
{$ENDIF}

(*--------------------------------------------------------------------------*)

// read a path from registry or return ''
function GetPathFromRegistry(sPath: string): string;
var
  sRoot, sValue, sSuffix, sData: string;
  i: integer;
  c: cardinal;
  hkRoot, hkOpen: HKEY;
begin
  // format hkxy\reg path\value[|suffix]
  Result := '';
  sRoot  := '';
  sValue := '';
  sSuffix := '';

  i := Pos(Chr_DirSep, sPath); // root\...
  if i > 0 then
  begin
    sRoot := Copy(sPath, 1, i - 1);
    Delete(sPath, 1, i);
    i := Pos('|', sPath);  // ...|suffix
    if i > 0 then
    begin
      sSuffix := Copy(sPath, i + 1, MaxInt);
      Delete(sPath, i, MaxInt);
    end;

    i := Length(sPath);
    while (i > 0) and (sPath[i] <> Chr_DirSep) do
      Dec(i);
    if (i > 0) and (i < Length(sPath)) then // ..\value
    begin
      sValue := Copy(sPath, i + 1, MaxInt);
      Delete(sPath, i, MaxInt);
    end;

    case StrArrayIndexOf(sRoot, ['HKEY_CURRENT_USER', 'HKCU',
        'HKEY_LOCAL_MACHINE', 'HKLM', 'HKEY_USERS', 'HKU']) of
      0, 1: hkRoot := HKEY_CURRENT_USER;
      2, 3: hkRoot := HKEY_LOCAL_MACHINE;
      4, 5: hkRoot := HKEY_USERS;
      else
        hkRoot := 0;
    end;
    if (hkRoot <> 0) and (RegOpenKey(hkRoot, PChar(sPath), hkOpen) =
      ERROR_SUCCESS) then
    begin
      SetLength(sData, MAX_PATH * 2);
      c := Length(sData);
      if RegQueryValueEx(hkOpen, PChar(sValue), nil, nil, PByte(PChar(sData)),
        @c) = ERROR_SUCCESS then
      begin
        SetLength(sData, c - 1); // assumed to be reg_sz or reg_expand_sz
        i := Pos(';', sData);
        if i > 0 then
          Delete(sData, i, MaxInt);
        Result := AppendDirSeparator(PChar(sData)) + sSuffix;
      end;
      RegCloseKey(hkRoot);
    end;
  end;
end;

(*--------------------------------------------------------------------------*)

// expand path to include drive
function ExpandPath(const sRel: string): string;
var
  p: PChar;
begin
  SetLength(Result, MAX_PATH * 2);
  SetLength(Result, GetFullPathName(PChar(sRel), Length(Result), PChar(Result), p));
end;

(*--------------------------------------------------------------------------*)

// build a volatile path name (sfx_<unique name>)
function GetUniqueVolatilePath: string;
var
  LStrDir:  string;
  LIntLoop: integer;
begin
  VBool_CheckDeleteVolatilePath := False;
  Result := AppendDirSeparator(ExpandEnv('%temp%')) + 'sfx';
  for LIntLoop := 0 to 99999 do // just 8 chars (dos conventions)
  begin
    LStrDir := Int2Str(LIntLoop, 0);
    LStrDir := Copy('0000', 1, 5 - Length(LStrDir)) + LStrDir;
    if not FileExists(Result + LStrDir) then
    begin
      Result := AppendDirSeparator(Result + LStrDir);
      VBool_CheckDeleteVolatilePath := True;
      VStr_VolatilePath := Result; // remember volatile path
      VStr_VolatilePath_Unexpanded := 'sfx' + LStrDir;
      Break;
    end;
  end;
end;

(*--------------------------------------------------------------------------*)
function PRIMARYLANGID(lang: LANGID): LANGID; //inline;
begin
  Result := WORD(lang) and $3FF;
end;

procedure ClearLang(var dest: PByte);
begin
    ReAllocMem(dest, 0);
    VInt_CP := 0;
end;

// MaxCSize protects agains overruns on invalid data
function LoadStrings(var dest: PByte; MaxCSize: Integer): integer;
var
  DHead: TSFXStringsData;
begin
  // load strings
  Result := 0;
  ReAllocMem(dest, 0);
  CheckFRead(DHead, sizeof(TSFXStringsData));
  if DHead.CSize > MaxCSize then
    Result := -1  // failed sanity check
  else
  begin
    ReAllocMem(dest, DHead.USize + 4);
    VP_SBuf := dest;
    VDW_CRC32Val := CRC_MASK;
    VInt_MaxWrite := DHead.USize;
    VInt_BytesToGo := DHead.CSize;
    InFlate(nil, 0);
  end;
  if (Result <> 0) or (DHead.CRC <> (VDW_CRC32Val xor $FFFFFFFF)) then
  begin
    ClearLang(dest);
    Result := -1;
  end;
end;


// Set language strings
procedure SetLangStrings(hLC: hWnd);
var
  UILang: LANGID;
  i, lng: integer;
  idx: cardinal;
  PHead: PSFX_LanguageData;
  tmp: PChar;
begin
  if (hLC = 0) {or (VRec_SHeader.Count < 1) or (pl = nil)} then
    exit;
  UILang := GetUserDefaultLangID;
  idx := 0;

  tmp := PChar(GetXBuf(256));
  // add US (English) first
//  if GetLocaleInfo($0409, LOCALE_SNATIVELANGNAME, tmp, 120) <> 0 then
//    SendMessage(hLC, CB_ADDSTRING, 0, integer(tmp));
  SendMessage(hLC, CB_ADDSTRING, 0, LPARAM(pChar('Default (US)')));
  i := 1;
  PHead := LoadResource(SFX_LANG_BASE + i);
  while (PHead <> nil) and (PHead^.LangID <> 0) do
  begin
    lng := PHead^.LangID;
    if GetLocaleInfo(Lng, LOCALE_SNATIVELANGNAME, tmp, 120) <> 0 then
    begin
      SendMessage(hLC, CB_ADDSTRING, 0, LPARAM(tmp));
      if PRIMARYLANGID(lng) = PRIMARYLANGID(UILang) then
        idx := i;
    end;
    inc(i);
    PHead := LoadResource(SFX_LANG_BASE + i);
  end;
  SendMessage(hLC, CB_SETCURSEL, idx, 0); // set default
end;


function LoadLang(var dest: PByte; resID: integer): integer;
const
  MaxCSize = 10000; // ???
var
  PHead: PSFX_LanguageData;
  p: PAnsiChar;
begin
  // load selected strings
  Result := 0;
  ClearLang(dest);
  if resID <= 0 then
    exit;   // use default
  Result := -1;
  PHead := LoadResource(resID);
  if (PHead <> nil) and (PHead^.CSize < MaxCSize) then
  begin
    Result := PHead^.LangID;
    VInt_CP := DefCP(Result);
    ReAllocMem(dest, PHead.USize + 4);
    VP_SBuf := dest;
    VDW_CRC32Val := CRC_MASK;
    VInt_MaxWrite := PHead.USize;
    VInt_BytesToGo := PHead.CSize;
    p := PAnsiChar(PHead);
    inc(p, sizeof(TSFX_LanguageData)); // point to data
    InFlate(p ,PHead.CSize);
  end;
  if (Result <= 0) or (PHead.CRC <> (VDW_CRC32Val xor $FFFFFFFF)) then
  begin
    ClearLang(dest);
    Result := -1;
  end;
  VP_SBuf := nil;
end;

procedure SetLanguage;
var
  Def: LANGID;
  i, pri: integer;
  psd: PSFX_LanguageData;
begin
  VRec_Strings := nil;
  // strings are optional
  psd := LoadResource(SFX_LANG_BASE + 1);
  if psd <> nil then
  begin
    // we have strings - load initial
    pri := -1;
    // try for 'default' language
    Def := GetUserDefaultLangID;
    if Def <> $0409 then
    begin
      i := 1;//0;
      while psd <> nil do
      begin
        if psd^.LangID = Def then
          break;
        if (PRIMARYLANGID(psd^.LangID) = PRIMARYLANGID(Def)) then
          pri := i;
        inc(i);
        psd := LoadResource(SFX_LANG_BASE + i); // try next
      end;
      if pri > 0 then
        LoadLang(VRec_Strings, SFX_LANG_BASE + pri);
    end;
    // Display the dialog
    if DialogBox(hInstance, Str_Dlg_Language, 0, @LanguageQueryDialogProc) =
        ID_BTN_NO then
      Halt;

    // load selected strings
    if (VInt_CurLang <> pri) then
    begin
      if (VInt_CurLang > 0) then
        LoadLang(VRec_Strings, SFX_LANG_BASE + VInt_CurLang)
      else
        ClearLang(VRec_Strings);
    end;
  end;
  VP_SBuf := nil;
end;

//  Returns a boolean indicating whether or not we're running under XP or later.
function WinVersion: integer;
var
  osv: TOSVERSIONINFO;
begin
  osv.dwOSVersionInfoSize := sizeOf(OSVERSIONINFO);
  GetVersionEx(osv);
  Result := (osv.dwMajorVersion *1000) + osv.dwMinorVersion;
end;

// check manual override of AutoRun
function Manual: Boolean;
var
  cp: PChar;
  c0, c1, c2: Char;
begin
  Result := False;
  cp := GetCommandLine;
  if cp = nil then
    Exit;
  c0 := #0;
  c1 := #0;
  c2 := #0;
  while cp^ <> #0 do
  begin
    c0 := c1;
    c1 := c2;
    c2 := cp^;
    cp := CharNext(cp);
  end;
  if c0 > ' ' then
    Exit;
  if c1 <> '/' then
    Exit;
  Result := (c2 = 'm') or (c2 = 'M');
end;

// read TSFXFileHeader from input file
procedure GetDefParams; // reads the values from the special header
type
  T_DetachedArgs = packed record
    Size: WORD;    // size of full record including sig
    Pads: WORD;    // number of bytes to keep DWORD aligned
  end;
var
  Sig: cardinal;
  CmdStrs: PByte;
  PathSize: Integer;
  StartMsgSize: Integer;
begin
  CmdStrs := nil;
  VInt_FileBegin := GetExeSize;
  VP_SBuf := nil;
  FSeek(VInt_FileBegin, FILE_BEGIN);

  CheckFRead(VRec_SFXHeader, sizeof(VRec_SFXHeader));

  with VRec_SFXHeader do
  begin
    if (Signature <> SFX_HEADER_SIG) then
      ErrorHaltID(SFX_Err_ArchiveCorrupted);
    // get command strings
    if (so_CompressedCmd and Options) <> 0 then
    begin
      if LoadStrings(CmdStrs, VRec_SFXHeader.Size - sizeof(VRec_SFXHeader)) <> 0 then
        ErrorHaltID(SFX_Err_ArchiveCorrupted);
      VP_SBuf := nil;
      FSeek(VInt_FileBegin + VRec_SFXHeader.Size, FILE_BEGIN);
    end
    else
    begin
      ReAllocMem(CmdStrs, size - sizeof(TSFXFileHeader));
      CheckFRead(CmdStrs^, size - sizeof(TSFXFileHeader));
    end;
    // check for sfx header
    VInt_SpanType := SFXSpanTypeNone;  // default

    VStr_SFX_Caption := LoadSFXStr(CmdStrs, sc_Caption);
    if VStr_SFX_Caption = '' then
      VStr_SFX_Caption := SFXString(SFX_Cap_App);

    VStr_SFX_Path := LoadSFXStr(CmdStrs, sc_Path);
    VStr_SFX_CmdLine := LoadSFXStr(CmdStrs, sc_CmdLine);
    VStr_SFX_RegFailPath := LoadSFXStr(CmdStrs, sc_RegFailPath);
    VStr_SFX_StartMsg := LoadSFXStr(CmdStrs, sc_StartMsg);
    ReAllocMem(CmdStrs, 0);  // finished with it
    PathSize := Length(VStr_SFX_Path);
    StartMsgSize := Length(VStr_SFX_StartMsg);

    //get the path from registry, added 10/10/98 ##FR
    if CompareText('HK', Copy(VStr_SFX_Path, 1, 2)) then
    begin
      VStr_SFX_Path := GetPathFromRegistry(VStr_SFX_Path);
      if VStr_SFX_Path = '' then
      begin
        if VStr_SFX_RegFailPath <> '' then
          VStr_SFX_Path := VStr_SFX_RegFailPath
        else
          VStr_SFX_Path := '><'; // substitue to temp path below
      end;
    end;

    while Pos('><', VStr_SFX_Path) > 0 do
      VStr_SFX_Path := FmtStr1(VStr_SFX_Path, AppendDirSeparator(ExpandEnv('%temp%')));

    // added april 20, 2002: substitute environment variables
    if (so_ExpandVariables and Options) <> 0 then
    begin
      VStr_SFX_Path := ExpandEnv(VStr_SFX_Path);
      VStr_SFX_Caption := ExpandEnv(VStr_SFX_Caption);
      VStr_SFX_StartMsg := ExpandEnv(VStr_SFX_StartMsg);
      VStr_SFX_CmdLine := ExpandEnv(VStr_SFX_CmdLine);
    end;

    if PathSize = 0 then // Stored path
      VStr_ExtractPath := GetCurDir
    else
    begin
      // aug 26, 2002: added support for volatile extract directory
      if CompareText(VStr_SFX_Path, '<VOLATILE>') then
        VStr_ExtractPath := GetUniqueVolatilePath
      else
        VStr_ExtractPath := ExpandPath(VStr_SFX_Path);
    end;

    // shall we show the message ?
    if (StartMsgSize > 0) and (MsgBox(0, VStr_SFX_StartMsg, VStr_SFX_Caption,
      StartMsgType) in [idCancel, idAbort, idNo, IDCLOSE]) then
      Halt;

    // check autorun flag
    if ((so_AutoRun or so_CheckAutoRunFileName) and Options) =
      (so_AutoRun or so_CheckAutoRunFileName) then
    begin
      if ((WinVersion >= 6000) and (nvVerifyTrust(PChar(VStr_ExeName)) <> 0)) or Manual or
        (CompareText(ExtractFileName(VStr_ExeName), 'Setup.exe') or
        (ExtractFileName(VStr_ExeName)[1] = '!')) then
        Options := Options and (not so_AutoRun);
    end;
  end;

  // at beginning of file or detached header
  // might have detached header
  CheckFRead(Sig, sizeof(Sig));
(*@@
  if (Sig = SFX_DETACHED_HEADER_SIG) then
  begin
    // load detached name
    CheckFRead(DetArgs, sizeof(DetArgs));
    i := DetArgs.Size - SizeOf(TSFXDetachedHeader) - DetArgs.Pads;
    ReadSFXString(VStr_DetachName, i);
    // skip pads
    if DetArgs.Pads > 0 then
      FSeek(DetArgs.Pads, FILE_CURRENT);
    CheckFRead(Sig, sizeof(Sig));
  end;
*)
  // check the signature following
  if Sig = ZipCentralHeaderSig then
    VInt_SpanType := SFXSpanTypeMultiVol; // is span but type unknown
  // reposition to before local/central signature
  VInt_FileBegin := FSeek(-sizeof(Sig), FILE_CURRENT);
end;

(*--------------------------------------------------------------------------*)

//// get operating system type (nt/win)
//function IsWinNT: boolean;
//var
//  osvi: TOSVersionInfo;
//begin
//  osvi.dwOSVersionInfoSize := SizeOf(OSvi);
//  Result := GetVersionEx(OSVI) and (osvi.dwPlatformID = VER_PLATFORM_WIN32_NT);
//end;

(*--------------------------------------------------------------------------*)

//##FR execute inf-scripts using rundll, not nice but works!

function ExecInf(const Path: String): cardinal;
(*var
  osvi: TOSVersionInfo;*)
//var
//  cmd: AnsiString;
//  HINSTANCE: HINST;
begin
//  HINSTANCE
  Result := ShellExecute(VH_MainWnd, PChar('open'), PChar('rundll32.exe'),
  PChar('SetupApi,InstallHinfSection DefaultInstall 132 ' + Path),
         nil, SW_SHOW);
//  Result := HINSTANCE;// > 32;
  (*if Param = '.ntx86' then    Param := Param + ' '
  else
    Param := '';

  if IsWinNT
  then
    Path := 'rundll32.exe setupapi.dll,
        InstallHinfSection DefaultInstall' + Param + '132 ' + Path
  else*)
//  cmd := 'rundll.exe setupapi.dll,InstallHinfSection DefaultInstall 132 '
//    + AnsiString(Path);
//  Result := WinExec(PAnsiChar(cmd), SW_SHOW);
//  Result := WinExec(PAnsiChar(
//    'rundll.exe setupapi.dll,InstallHinfSection DefaultInstall 132 '
//    + Path), SW_SHOW);
  (*osvi.dwOSVersionInfoSize := SizeOf(OSvi);
  if GetVersionEx(OSVI) then
  begin
    case osvi.dwPlatformID of
      VER_PLATFORM_WIN32_WINDOWS: Path :=
          'rundll.exe setupx.dll,InstallHinfSection DefaultInstall 132 ' + Path;
      VER_PLATFORM_WIN32_NT: Path :=
          'rundll32.exe setupapi.dll,InstallHinfSection DefaultInstall' +
          Param + '132 ' + Path;
      else
        // no win32 s
    end;
    Result := WinExec(PChar(Path), SW_SHOW);
  end;*)

end;

(*--------------------------------------------------------------------------*)

function Trim(s: string): string;
  // strip trailing #0 and double separators
begin
  Result := PChar(s);
  while Pos('\\', Result) > 0 do
    Delete(Result, Pos('\\', Result), 1);
end;

(*--------------------------------------------------------------------------*)

// remove extract directory next time a user logs on

procedure RemoveDirEx;
var
  LStrCmd: string;
  LHKSub:  HKEY;
  LBoolSuccess: boolean;
begin
//  if IsWinNT then
//  if Win32Platform = VER_PLATFORM_WIN32_NT then
  if WinVersion >= 5000 then
    // the following does not work with all types of drives
    (*LStrCmd := 'cmd.exe /c @if exist "'+RemoveDirSeparator(VStr_VolatilePath)+
      '\nul" rd /s /q "'+RemoveDirSeparator(VStr_VolatilePath)+'"'*)
    LStrCmd := 'cmd.exe /c @rd /s /q "%temp%\' + VStr_VolatilePath_Unexpanded + '">nul 2>nul'
  else
    // the following does not work with all types of drives
    (*LStrCmd := 'command.com /c @if exist "'+RemoveDirSeparator(VStr_VolatilePath)+
      '\nul" deltree /y "'+RemoveDirSeparator(VStr_VolatilePath)+'"';*)
    LStrCmd := 'command.com /c deltree /y "%temp%\' + VStr_VolatilePath_Unexpanded + '">nul';

  if RegCreateKey(HKEY_LOCAL_MACHINE,
    'Software\Microsoft\Windows\CurrentVersion\RunOnce', LHKSub) = ERROR_SUCCESS then
  begin
    LBoolSuccess := RegSetValueEx(LHKSub, PChar(VStr_VolatilePath_Unexpanded),
      0, REG_SZ, PChar(LStrCmd), Length(LStrCmd) + 1) = ERROR_SUCCESS;
    RegCloseKey(LHKSub);
  end
  else
    LBoolSuccess := False;

  // try current user if not successfull
  if (not LBoolSuccess) and (RegCreateKey(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\RunOnce', LHKSub) =
    ERROR_SUCCESS) then
  begin
    RegSetValueEx(LHKSub, PChar(VStr_VolatilePath_Unexpanded), 0,
      REG_SZ, PChar(LStrCmd), Length(LStrCmd) + 1);
    RegCloseKey(LHKSub);
  end;
end;

(*--------------------------------------------------------------------------*)

//##FR modified to enable inf-scripts

function ExecuteCMD: cardinal;
  // parses and executes the stored command line after extraction
var
  sr1, sr2: string;
  srOld: string;
begin
  Result := 0;
    sr1 := Trim(GetArgument(1));
    sr2 := Trim(GetArgument(2));
    srOld := GetCurDir;
    if Length(VStr_ExtractPath) <> 0 then
      ChDir(VStr_ExtractPath);
    if Length(sr1) > 4 then
    begin
      if TestForInf(sr1) then
        Result := ExecInf(sr1) //error if < 32
      else
        Result := ShellExecute(0, 'open', PChar(sr1), PChar(sr2),
          PChar(VStr_ExtractPath), SW_SHOW);
    end;
    ChDir(srOld);
  // aug 26, 2002: added support for volatile extract path
  if (Result >= 32) and VBool_CheckDeleteVolatilePath and
    CompareText(RemoveDirSeparator(VStr_VolatilePath),
    RemoveDirSeparator(VStr_ExtractPath)) and
    DirectoryExists(RemoveDirSeparator(VStr_VolatilePath)) then
    RemoveDirEx;
end;

(*--------------------------------------------------------------------------*)


function FmtStrID1(id: integer; const arg1: string): string;
begin
  Result := FmtStr1(SFXString(id), arg1);
end;

function FmtStr1(const sFormat: string; const arg1: string): string;
var
  j: integer;
begin
  Result := sFormat;
  j := Pos('><', Result);
  if j > 0 then
    Result := Copy(Result, 1, j - 1) + arg1 + Copy(Result, j + 2, MaxInt);
end;

function FmtStr2(const sFormat: string; const arg1, arg2: string): string;
begin
  Result := FmtStr1(FmtStr1(sFormat, arg1), arg2);
end;

(*--------------------------------------------------------------------------*)

function GetArgument(const iIndex: integer): string;
  // gets an argument from the stored command line
  //                1 : the part before the pipe (if there's no pipe,
  //                                      returns the whole command line)
  //                2 : the part after the pipe (if no pipe, returns "")
  //                all "><" will be replaced by the extraction path
var
  pip: integer;
begin
  VStr_ExtractPath := AppendDirSeparator(VStr_ExtractPath);
  Result := VStr_SFX_CmdLine;
  pip := Pos('|', Result);
  if pip = 0 then
  begin
    if iIndex = 2 then
      Result := '';
  end
  else
  begin
    if iIndex = 1 then
      Result := Copy(Result, 1, pip - 1)
    else
      Result := Copy(Result, pip + 1, MAXINT);
  end;

  while Pos('><', Result) > 0 do
    Result := FmtStr1(Result, VStr_ExtractPath);

  // get the short (8+3)-filename (it seems that shellexecute has some problems with lfn)
  GetShortPathName(PChar(Result), PChar(Result), Length(Result));
end;

(*--------------------------------------------------------------------------*)

function TestForInf(const sr1: string): boolean;
begin
  Result := CompareText('.inf', Copy(sr1, Length(sr1) - 3, 4));
end;

(*--------------------------------------------------------------------------*)

function GetRunString(const sRun, sInst: string): string;
var
  sr1: string;
begin
  sr1 := ExtractFileName(GetArgument(1));
  if TestForInf(sr1) then
    Result := FmtStr2(sRun, sr1, ExtractFileName(GetArgument(2)))
  else
  begin
    if sr1 = '' then
    begin
      sr1 := GetArgument(1);
      if sr1 <> '' then
        sr1 := ExtractFileName(RemoveDirSeparator(sr1));
    end;
    Result := FmtStr2(sInst, sr1, ExtractFileName(GetArgument(2)));
  end;
end;

(*--------------------------------------------------------------------------*)

function GetRunCheckBoxText: string;
begin
  Result := GetRunString(SFXString(SFX_Msg_RunCheckBox_Run),
    SFXString(SFX_Msg_RunCheckBox_Inst));
end;

(*--------------------------------------------------------------------------*)

// get an error message if ExcuteCMD failed
function GetRunErrorMessage: string;
begin
  Result := GetRunString(SFXString(SFX_Err_Run_Run), SFXString(SFX_Err_Run_Inst));
end;

(*--------------------------------------------------------------------------*)


procedure ProcessMessages;
var
  Msg: TMsg;
begin
 { PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
  TranslateMessage(Msg);
  DispatchMessage(Msg);	}
  while (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) do
  begin
    if not IsDialogMessage(0, msg) then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;
end;

(*--------------------------------------------------------------------------*)

procedure Make_CRC32Table;
var
  i, j: word;
  r: cardinal;
const
  CRCPOLY  = $EDB88320;
  UCHAR_MAX = 255;
  CHAR_BIT = 8;
begin
  for i := 0 to UCHAR_MAX do
  begin
    r := i;
    for j := CHAR_BIT downto 1 do
      if (r and 1) > 0 then
        r := (r shr 1) xor CRCPOLY
      else
        r := r shr 1;
    VArr_CRC32Table[i] := r;
  end;
end;

(*--------------------------------------------------------------------------*)

// Update the encryption keys with the next byte of plain text

procedure UpdateKeys(c: byte);
begin
  VArr_CryptKey[0] := UpdC32(c, VArr_CryptKey[0]);
  VArr_CryptKey[1] := VArr_CryptKey[1] + VArr_CryptKey[0] and $000000FF;
  VArr_CryptKey[1] := VArr_CryptKey[1] * 134775813 + 1;
  VArr_CryptKey[2] := UpdC32(HIBYTE(HIWORD(VArr_CryptKey[1])), VArr_CryptKey[2]);
end;

(*--------------------------------------------------------------------------*)

// Initialize the encryption keys and the random header according to the given password.

procedure seedk(passwd: AnsiString);
var
  i: byte;
begin
  VArr_CryptKey[0] := 305419896;
  VArr_CryptKey[1] := 591751049;
  VArr_CryptKey[2] := 878082192;
  for i := 1 to LENGTH(passwd) do
    UpdateKeys(byte(passwd[i]));
end;

(*--------------------------------------------------------------------------*)

// Return the next byte in the pseudo-random sequence

function decrypt_byte: integer;
var
  temp: word;
begin
  temp := word(VArr_CryptKey[2] or 2);
  Result := integer(word((temp * (temp xor 1)) shr 8) and $FF);
end;

(*--------------------------------------------------------------------------*)

function decrypt_pw(Encrypt_Head: PAnsiChar; EncHead_len: byte; BitFlag: word;
    CRC, FileDate: longint; const sPassword: AnsiString): boolean;
var
  i, c, b: byte;
begin
  Result := False;
  if sPassword = '' then
    Exit;
  seedk(sPassword);
  for i := 0 to EncHead_len - 1 do
  begin
    c := byte(Encrypt_Head[i + EncHead_len]) xor decrypt_byte;
    UpdateKeys(c);
    Encrypt_Head[i] := AnsiChar(c);
  end;

  (* version 2.0+ *)
  b := byte(Encrypt_Head[EncHead_len - 1]);

  if not ((BitFlag and 8) = 8) then
  begin
    if b = HIBYTE(HIWORD(crc)) then
      Result := True;
  end
  else
  begin
    if b = LOWORD(FileDate) shr 8 then
      Result := True;
  end;
end;

(*--------------------------------------------------------------------------*)

 // added october 10, 1998
 // enable/disable all children of the given parent window
 // this is used to disable all main dialog's controls during archive extraction
 // thanks to David - Kazuya david-kazuya@usa.net for report

procedure EnableChildren(const wnd: HWND; const bEnable: boolean);

  function FindChE(wnd: HWND; lParam: LPARAM): Bool; stdcall;
  var
    pCH: array[0..64] of char;
  begin
    Result := True;
    GetClassName(wnd, PChar(@pCH), 63);
    if IsWindowVisible(wnd) and (pCH <> 'msctls_progress32') then
      EnableWindow(wnd, boolean(lParam));
  end;

begin
  EnumChildWindows(wnd, @FindChE, integer(bEnable));
end;

(*--------------------------------------------------------------------------*)

// resize dialog/control

procedure ResizeControl(const wnd: HWND; const bReposition: boolean; yDiff: integer);
var
  pl: TWindowPlacement;
begin
  yDiff := MulDiv(yDiff, HIWORD(GetDialogBaseUnits), 8);
  pl.length := sizeof(pl);
  GetWindowPlacement(wnd, @pl);
  if bReposition then
    pl.rcNormalPosition.Top :=
      pl.rcNormalPosition.Top + yDiff;
  pl.rcNormalPosition.Bottom := pl.rcNormalPosition.Bottom + yDiff;
  SetWindowPlacement(wnd, @pl);
end;

(*--------------------------------------------------------------------------*)

 // from Angus Johnson's TZip-SFX code:
 // get the executable's file size to get rid of caring about the exe size
function GetExeSize: cardinal;
{$ifdef DEBUG_SFX}
begin
  Result := Test_Stub_Size;
end;
{$else}
var
  p: PByte;
  i, NumSections: integer;
const
  IMAGE_PE_SIGNATURE = $00004550;
begin
  Result := 0;
  p := pointer(hinstance);
  if (PImageDosHeader(p).e_magic <> IMAGE_DOS_SIGNATURE) then
    exit;
  Inc(p, PImageDosHeader(p)._lfanew);
  if  (PCardinal(p)^ <> IMAGE_PE_SIGNATURE) then
    exit;
  Inc(p, sizeof(cardinal));
  NumSections := PImageFileHeader(p).NumberOfSections;
  Inc(p, sizeof(TImageFileHeader) + sizeof(TImageOptionalHeader));
  for i := 1 to NumSections do
  begin
    with PImageSectionHeader(p)^ do
      if PointerToRawData + SizeOfRawData > Result then
        Result := PointerToRawData + SizeOfRawData;
    Inc(p, sizeof(TImageSectionHeader));
  end;
end;
{$endif}
(*--------------------------------------------------------------------------*)

const
  MAX_IDX = ($80000000 div sizeof(TZ64CentralEntry)) - 1; // 2G storage limit
 // storage for records in list view
type
  PCentralRecords = ^TCentralRecords;
  TCentralRecords = packed array[0..MAX_IDX] of TZ64CentralEntry;

var
  p_Items:  PCentralRecords = nil;
  cb_Items: cardinal = 0;

function AddToItemData(const rec: TZ64CentralEntry): cardinal;
begin
  if (cb_Items and 63) = 0 then
    ReAllocMem(p_Items, sizeof(TZ64CentralEntry) * (cb_Items + 64));

  Inc(cb_Items);
  p_Items^[cb_Items - 1] := rec;
  Result := cb_Items - 1;
end;

// add an entry to the list view
procedure AddFileToList(const wndOwner: HWND; const sName: string;
  const Rec: TZ64CentralEntry; const IsDir: boolean);
var
  recItem: TLVItem;
  wndLV: HWND;
  iiItem: integer;
  sfi: TSHFileInfo;
  s: string;
begin
  wndLV := GetDlgItem(wndOwner, ID_LV_FILES);
  if not IsDir then
    SHGetFileInfo(PChar(ExtractFileName(sName)), FILE_ATTRIBUTE_NORMAL, sfi,
      sizeof(sfi), SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON or SHGFI_SYSICONINDEX)
  else
    SHGetFileInfo(PChar(ExtractFileName(RemoveDirSeparator(sName))),
      FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi), SHGFI_USEFILEATTRIBUTES or
      SHGFI_SMALLICON or SHGFI_SYSICONINDEX);

  with recItem do
  begin
    mask  := LVIF_TEXT or LVIF_PARAM or LVIF_STATE or LVIF_IMAGE;
    iItem := MaxInt;
    iSubItem := 0;
    // select only if no file selection is allowed (just for visual feedback)
    if (so_AskFiles and VRec_SFXHeader.Options) = 0 then
      state := 0
    else
      state := LVIS_SELECTED;
    stateMask := LVIS_SELECTED;
    pszText := PChar(sName);
    cchTextMax := Length(sName);
    iImage  := sfi.iIcon;
    lParam  := AddToItemData(Rec);
  end;
  iiItem := SendMessage(wndLV, LVM_INSERTITEM, 0, integer(@recItem));

  with recItem do
  begin
    mask  := LVIF_TEXT;// or LVIF_PARAM;
    iItem := iiItem;
    iSubItem := 1;
    s := Int2Str(Rec.UnComprSize, 0);
    pszText := PChar(s);
    cchTextMax := Length(s);
  end;
  SendMessage(wndLV, LVM_SETITEM, 0, integer(@recItem));
end;

(*--------------------------------------------------------------------------*)

// retrieve an item from the list
function GetFileFromList(const wndOwner: HWND; const iiItem: integer;
  var Rec: TZ64CentralEntry): string;
var
  Item:  TLVItem;
  wndLV: HWND;
  szBuf: array[0..MAX_PATH * 2] of char;
begin
  wndLV := GetDlgItem(wndOwner, ID_LV_FILES);

  // get lparam stored in routine above
  with Item do
  begin
    iItem := iiItem;
    iSubItem := 0;
    mask  := LVIF_PARAM;
  end;
  SendMessage(wndLV, LVM_GETITEM, 0, integer(@Item));
  Rec := p_Items^[Item.lParam];

  // path+file
  with Item do
  begin
    iItem := iiItem;
    iSubItem := 0;
    mask  := LVIF_TEXT;
    pszText := PChar(@szBuf);
    cchTextMax := sizeof(szBuf);
  end;
  SetString(Result, Item.pszText, SendMessage(wndLV, LVM_GETITEMTEXT,
    iiItem, integer(@Item)));
end;

(*--------------------------------------------------------------------------*)

procedure DeSelectInFilesListView(const wndDlg: HWND; const iItem: integer);
var
  Item: TLVItem;
begin
  with Item do
  begin
    stateMask := LVIS_SELECTED;
    state := 0;
  end;
  SendDlgItemMessage(wndDlg, ID_LV_FILES, LVM_SETITEMSTATE, iItem, longint(@Item));
end;

(*--------------------------------------------------------------------------*)

// may 11, 2002: show first selected item on extraction failure
procedure ShowFirstSelected(const wndList: HWND);
var
  i: integer;
begin
  for i := 0 to Pred(SendMessage(wndList, LVM_GETITEMCOUNT, 0, 0)) do
    if SendMessage(wndList, LVM_GETITEMSTATE, i, LVIS_SELECTED) = LVIS_SELECTED then
    begin
      SendMessage(wndList, LVM_ENSUREVISIBLE, i, integer(False));
      Break;
    end;
end;

// check whether spanned or multivol archive
procedure CheckSpan;
var
  len: integer;
  sName: string;
begin
  // prepare name using defaults unless supplied
  if VStr_DetachName = '' then
    VStr_DetachName := ExtractFileName(VStr_ExeName);
  len := Length(VStr_DetachName);
  while (len > 0) and (VStr_DetachName[len] <> '.') do
    dec(len);
  if (len > 0) then
  begin
    if VInt_SpanType = SFXSpanTypeUnknown then
      VStr_DetachExt := Copy(VStr_DetachName, len, 255)
    else
      VStr_DetachExt := '.zip';
    VStr_DetachName := Copy(VStr_DetachName, 1, len -1);
  end
  else
    VStr_DetachExt := '.zip';
    VStr_DetachName := AppendDirSeparator(ExtractFilePath(VStr_ExeName)) +
            VStr_DetachName;
  VInt_SpanType := SFXSpanTypeMultiVol;
    VStr_SourceDir := AppendDirSeparator(ExtractFilePath(VStr_ExeName));
    if VStr_SourceDir = '' then
    begin
      len := GetCurrentDirectory(0, nil);
      if len > 0 then
      begin
        SetLength(sName, len + 5);
        GetCurrentDirectory(len + 2, pChar(sName));
        VStr_SourceDir := AppendDirSeparator(pChar(sName));
      end;
    end;
    VBool_FixedDrive := GetDriveType(PChar(VStr_SourceDir)) in
      [DRIVE_FIXED, DRIVE_REMOTE, DRIVE_RAMDISK];
    if not VBool_FixedDrive then
    begin
      sName := DetachedName('001');
      if not FileExists(sName) then
        VInt_SpanType := SFXSpanTypeSpanned;
    end;
end;

(*----------------------------------------------------------------------------
3.3.1.0 11 Aug 2007 RA difference for dir and nondir in AddFileTo List addded
3.3.0.0 24 Jan 2006 RA soCreateEmptyDirs added
  fill the list view
*)
procedure FillListView(wndOwner: hWnd);
type
  PUString_Data = ^UString_Data;
  UString_Data = packed record
//    tag: word;
//    totsiz: word;
    version: byte;
    origcrc: DWORD;
  end;
const
  PKZIPW25: Integer = 25;//(FS_FAT * 256) + 25;
  PKZIPW26 = 26;//(FS_FAT * 256) + 26;
  PKZIPW40 = 40;//(FS_FAT * 256) + 40;
  UNIXATTRS = $FFFF0000;
  WZIP = $0B32;//(FS_NTFS * 256) + 50;
  //  FS_FAT: Integer = 0;
  //  FS_HPFS: Integer = 6;
  //  FS_NTFS: Integer = 11;
  //  FLAG_UTF8_BIT = $1000;
var
  eoc: TZipEndOfCentral;
  i, j: cardinal;
  cfh: TZipCentralHeader;
  buffer: array [0..MAX_PATH + 2] of AnsiChar;
  fn: string;
  p: PByte;
  x: integer;
  fnp: PAnsiChar; // source filename pointer;
  fnsz: integer;  // source filename size
  fncp: integer;  // source filename codepage

  EOC64:  TZipEOC64;
  Z64CFH: TZ64CentralEntry;
  EocPos, CenSize, TotalEntries: int64;
  HasEoc64: boolean;         
  over, sz: integer;
  crc: cardinal;
  pp:  PUString_Data;
  BadName: boolean;
  hasUPath: Boolean;
begin
  EocPos := FSeek(FindEOCRecord, FILE_BEGIN);

  CheckFRead(eoc, sizeof(eoc));
  HasEoc64 := NeedEOC64(EOC);
  if (HasEoc64) then
    GetEOC64(EocPos, EOC64);
  //how far out the header offsets are from reality (due to sfx stub)
  if HasEoc64 and (EOC.CentralSize = MAX_UNSIGNED) then
    censize := EOC64.CentralSize
  else
    censize := EOC.CentralSize;//          EOC64.CentralSize : EOC.CentralSize;
  VDW_OffsetDelta := EocPos - censize;
  if HasEoc64 and (EOC.CentralOffSet = MAX_UNSIGNED) then
    VDW_OffsetDelta := VDW_OffsetDelta - EOC64.CentralOffSet
  else
    VDW_OffsetDelta := VDW_OffsetDelta - EOC.CentralOffSet;
  if (HasEoc64) then
    VDW_OffsetDelta := VDW_OffsetDelta - EOC64.vsize + 12 + SizeOf(TZip64EOCLocator);
  if HasEOC64 then
    censize := censize + EOC64.vsize + 12
  else
    censize := censize + SizeOf(EOC);
  FSeek(-censize, FILE_CURRENT);
  TotalEntries := EOC.TotalEntries;
  if (HasEoc64 and (TotalEntries = MAX_WORD)) then
    TotalEntries := EOC64.TotalEntries;

  // is it multi-disk
  if EOC.ThisDiskNo <> 0 then
    CheckSpan;

  //how far out the header offsets are from reality (due to sfx stub)
  for i := 0 to TotalEntries - 1 do
    begin
      CheckFRead(cfh, sizeof(cfh));
      if (cfh.HeaderSig <> ZipCentralHeaderSig) or (cfh.FileNameLen = 0) or
         (cfh.FileNameLen > 500) then
        ErrorHaltID(SFX_Err_ArchiveCorrupted);

      CheckFRead(buffer[0], cfh.FileNameLen);
      buffer[cfh.FileNameLen] := #0;
      // read extra data
      p := nil;
      over := 0;
      x := cfh.ExtraLen;
      if x > 0 then
      begin
        if x > 2048 then
        begin
          over := 2048 - x;
          x := 2048;
        end;
        p := GetXBuf(x);
        if p <> nil then
          xbuf^ := 0;
        if x > 0 then
          CheckFRead(p^, x);
      end;
      GetZ64Entry(cfh, Z64cfh);
      BadName := false;
      fnp := PAnsiChar(@buffer[0]);   // filename source
      fnsz := cfh.FileNameLen;//-1;       // filename source length
      fncp := 0;
      hasUPath := False;

      if (p <> nil) and (cfh.VersionMadeBy0 >= 20) then
      begin
        sz := cfh.ExtraLen;
        if ExtraData(p, sz, UPath_Data_Tag) and
          (sz > sizeof(UString_Data)) then
        begin
          pp  := PUString_Data(p);;
          crc := $FFFFFFFF;
          Crc32_Buf(PByte(@buffer[0]), cfh.FileNameLen, crc);
          crc := crc xor $FFFFFFFF;
          if (pp^.version = 1) and (crc = pp^.origcrc) then
          begin
            sz := sz - sizeof(UString_Data);
            inc(p, sizeof(UString_Data));
            if sz > 0 then
            begin
              fnp := PAnsiChar(p);
              fnsz := sz;
              fncp := CP_UTF8;
              hasUPath := True;
            end;
          end;
        end;
      end;

      if not hasUPath then
      begin
        fncp := CP_ACP;
        if (cfh.Flag and FLAG_UTF8_BIT) <> 0 then
          fncp := CP_UTF8
        else
        begin
          if (cfh.VersionMadeBy1 = FS_FAT) or
              (cfh.VersionMadeBy1 = FS_HPFS) or
              ((cfh.VersionMadeBy1 = FS_NTFS) and (cfh.VersionMadeBy0 = 50)) then
              fncp := CP_OEMCP;
          end;
      end;

      fn := To_Str(fncp, fnp, fnsz, true, BadName);
        //swap slashes and get last char ...
      for J := 1 to Length(fn) do
      begin
        if fn[j] = '/' then
          fn[j] := Chr_DirSep;
        if fn[j] = '?' then
          BadName := True;
      end;

      if BadName then
      begin
        fn := SFXString(SFX_Err_InvalidFileName) + ' "' + fn + '"';
        x := MessageBox(wndOwner, pChar(fn), PChar(SFXString(SFX_Cap_Err)),
                MB_OKCANCEL or MB_ICONSTOP or MB_TASKMODAL);
        if x = IDOK then
          continue;
        break;
      end;

      // skip directory entries
      if fn[Length(fn)] <> Chr_DirSep then
      begin
        //store each filename and absolute file offset of cfh record ...
        AddFileToList(wndOwner, fn, Z64cfh, False);
      end
      else
      // new 09/19/2005, recreate empty directories
      if (so_CreateEmptyDirs and VRec_SFXHeader.Options) <> 0 then
        AddFileToList(wndOwner, fn, Z64cfh, True);

      if (over + cfh.FileComLen) <> 0 then
        FSeek(over + cfh.FileComLen, FILE_CURRENT);
    end;

end;

(*--------------------------------------------------------------------------*)

// close a handle, if not already closed
function CheckCloseHandle(var H: THandle): boolean;
begin
  if (H <> 0) and (H <> INVALID_HANDLE_VALUE) then
    Result := CloseHandle(H)
  else
    Result := True;
  H := INVALID_HANDLE_VALUE;
end;

// create a 00x number string
procedure Str_3(const i: integer; var S: string);
begin
  S := Int2Str(i, 3);
//  Str(i, S);
//  while Length(s) < 3 do
//    s := '0' + s;
end;

function IsRightDisk(DiskSeq: integer): boolean;
var
  SSeq:  string;
  Dummy1, Dummy2, DiskSerial: cardinal;
  VolName: array[0..MAX_PATH] of char;
  sTemp: string;
begin
  Result := DiskSeq = VInt_LastSeq;
  if Result then
    exit;
  Str_3(DiskSeq + 1, SSeq);
  if VInt_SpanType = SFXSpanTypeSpanned then
  begin
    // get volume info
    GetVolumeInformation(PChar(VStr_SourceDir), VolName, MAX_PATH, @DiskSerial, Dummy1,
      Dummy2, nil, 0);
    STemp  := VolName;
    // must be pkback# 00x
    Result := CompareText(STemp, 'PKBACK# ' + SSeq);
    if Result and (not CompareText(VStr_ExeName, DetachedName(''))) then
    begin
      if not CheckCloseHandle(VH_InFile) then
        ErrorHaltFmt(SFX_Err_CannotCloseFile, VStr_ExeName);
      VStr_ExeName := DetachedName(''); // use detached name
      // Open the input archive on this disk.
      VH_Infile := CreateFile(PChar(VStr_ExeName), GENERIC_READ,
        FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if VH_Infile = INVALID_HANDLE_VALUE then
        ErrorHaltFmt(SFX_Err_CannotOpenFile, VStr_ExeName);
      // assume no shifted offset on detached archives
      VDW_OffsetDelta := 0;
    end;
  end
  else
  begin
    // multi volume, filename = xyz00x.(xyz) and Actual File
    Result := CompareText(VStr_ExeName, DetachedName(SSeq));
  end;
end;

procedure GetNewDisk(wndOwner: HWND; DiskSeq: integer);
var
  SSeq: string;
begin
  if not CheckCloseHandle(VH_InFile) then
    ErrorHaltFmt(SFX_Err_CannotCloseFile, VStr_ExeName);

  Str_3(DiskSeq + 1, SSeq);
  repeat
      if not VBool_FixedDrive then
      begin
        if MsgBox(wndOwner, FmtStr2(SFXString(SFX_Msg_InsertDiskVolume),
          SSeq, VStr_SourceDir), VStr_SFX_Caption, MB_OKCANCEL) = idCancel then
          ErrorHalt('');
      end;

      if VInt_SpanType = SFXSpanTypeMultiVol then
        VStr_ExeName := DetachedName(SSeq)
      else
        VStr_ExeName := DetachedName('');
  until IsRightDisk(DiskSeq);

  // Open the input archive on this disk.
  VH_Infile := CreateFile(PChar(VStr_ExeName), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if VH_Infile = INVALID_HANDLE_VALUE then
    ErrorHaltFmt(SFX_Err_CannotOpenFile, VStr_ExeName);
  // assume no shifted offset on detached archives
  VDW_OffsetDelta := 0;
  VInt_LastSeq := DiskSeq;
end;


(*----------------------------------------------------------------------------
 added version 3.0 May 1, 2003
 read data and copy in temp file if needed or skip to next local header *)
procedure RWJoinData(wndOwner: HWND; var Buffer; ReadLen: integer;
  var DiskNbr: word; Copy: boolean);
var
  SizeR, ToRead: integer;
begin
  while ReadLen > 0 do
  begin
    ToRead := min(ReadLen, SFXBufSize);
    SizeR  := FRead(Buffer, ToRead);
    if SizeR <> ToRead then
    begin
      // Check if we are at the end of a input disk.
      if (VInt_SpanType = SFXSpanTypeNone) or
          (FSeek(0, FILE_CURRENT) <> FSeek(0, FILE_END)) then
        ErrorHaltID(SFX_Err_ArchiveCorrupted);

      // It seems we are at the end, so get a next disk.
      Inc(DiskNbr);
      GetNewDisk(wndOwner, DiskNbr);
    end;

    if SizeR > 0 then
    begin
      if Copy then
        CheckFWrite(VH_TempFile, Buffer, SizeR, VStr_TempFile);
      ReadLen := ReadLen - SizeR;
    end;
  end;
end;

// open the correct archive in spanned, multivolue or detached sfx's
procedure OpenRightArchive(wndOwner: HWND; const DiskNumber: integer);
begin
  if not IsRightDisk(DiskNumber) then
    GetNewDisk(wndOwner, DiskNumber); // we need another disk
end;



// spanned archive, extract local header and file data to a temporary file
procedure ExtractToTempFile(const wndOwner: HWND; var LocalOffset: cardinal;
  var OldHandle: THandle);
var
  Buf: array[0..SFXBufSize] of Char;
  DataToCopy: cardinal;
begin
  if VStr_TempFile = '' then
  begin
    ZeroMemory(@buf, sizeof(buf));
    // create a temporaray filename
    SetLength(VStr_TempFile, MAX_PATH * 2);
    if GetTempFileName(PChar(AppendDirSeparator(ExpandEnv('%temp%'))),
      'SFX', 0, Buf) <> 0 then
    begin
      VStr_TempFile := buf;
      DeleteFile(buf); // because created by GetTempFileName
    end
    else
      ErrorHaltFmt(SFX_Err_CannotOpenFile, VStr_TempFile);
  end;

  // create temp file to copy the deflated file from the archive and prepare it for inflate
  VH_TempFile := CreateFile(PChar(VStr_TempFile), GENERIC_READ or
    GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, 0);

  if VH_TEMPFILE = INVALID_HANDLE_VALUE then
    ErrorHaltFmt(SFX_Err_CannotWriteFile, VStr_TempFile);

  FSeek(LocalOffset, FILE_BEGIN);
  // read the local header         
  VInt_MaxWrite  := sizeof(buf); // do not allow overlength
  RWJoinData(wndOwner, Buf, sizeof(TZipLocalHeader), VRec_ZipHeader.DiskStart,
    True);
  with PZipLocalHeader(@Buf)^ do
  begin
    if HeaderSig <> ZipLocalHeaderSig then
      BadArchive;
    DataToCopy := VRec_ZipHeader.ComprSize + FileNameLen + ExtraLen;
    // ext local header?
    if (Flag and 8) = 8 then
      DataToCopy := DataToCopy + sizeof(TZipExtLocalHeader);
  end;                                 
  VInt_MaxWrite  := DataToCopy; // do not allow overlength
  RWJoinData(wndOwner, Buf, DataToCopy, VRec_ZipHeader.DiskStart, True);
  OldHandle := VH_InFile;
  VH_InFile := VH_TempFile;
  LocalOffset := 0;
end;

// reattach current archive and close tempfile
procedure RollBackTempFile(const wndOwner: HWND; const OldHandle: THandle);
begin
  VH_InFile := OldHandle;
  if not CheckCloseHandle(VH_TempFile) then
    ErrorMsgBoxFmt1(wndOwner, SFX_Err_CannotCloseFile, VStr_TempFile);
  DeleteFile(PChar(VStr_TempFile));
end;

function ExtractFile(wndOwner: hWnd; const Filename: string;
  const rec: TZ64CentralEntry; var bPasswordFailed: boolean): boolean;
var
  EncryptHDR: PAnsiChar;
  i: integer;
  rLocal: TZipLocalHeader;
  bIsTempFile: boolean;
  clOffset: cardinal;
  oldH: THandle;
begin
  bPasswordFailed := False;
  bIsTempFile := False;
  Result := False;
  // mst may 07, 2002: removed a sleep(100) where did this come from?
  VRec_ZipHeader := rec;
  with VRec_ZipHeader do
  begin
    if HeaderSig <> ZipCentralHeaderSig then   
      BadArchive;
    if not (ComprMethod in [0, 8]) then                         
      ErrorMsgBoxFmt1(wndOwner, SFX_Err_ZipUnknownComp, '')
    else
    begin
      clOffset := RelOffLocal + VDW_OffsetDelta;
      if (VInt_SpanType <> SFXSpanTypeNone{0}) then
      begin
        // assure the right disk is opened
        OpenRightArchive(wndOwner, DiskStart);

        // join the possibly split data in a temporary file
        ExtractToTempFile(wndOwner, clOffset, oldH);
        bIsTempFile := True;
      end;

      try
        // goto beginning of local header...
        FSeek(clOffset, FILE_BEGIN);
        CheckFRead(rLocal, sizeof(rLocal));
        if rLocal.HeaderSig <> ZipLocalHeaderSig then
          BadArchive;

        // mst may 07, 2002: added extrafieldlen to go to correct position
        // e.g. for zipfiles created with infozip's zip.exe
        FSeek(rLocal.FileNameLen + rLocal.ExtraLen, FILE_CURRENT);
        VInt_BytesToGo := ComprSize;
        VInt_MaxWrite  := UnComprSize; // do not allow overlength

        //password stuff...
        if (Flag and 1) = 1 then //if a password used...
        begin
          Dec(VInt_BytesToGo, RAND_HEAD_LEN);
          GetMem(EncryptHDR, RAND_HEAD_LEN * 2);
          try
            CheckFRead(EncryptHDR[0], RAND_HEAD_LEN);
            //make a copy of encrypted header in upper half of buffer...
            Move(EncryptHDR[0], EncryptHDR[RAND_HEAD_LEN], RAND_HEAD_LEN);
            if VStr_Password = '' then
              bPasswordFailed := True
            else
              bPasswordFailed :=
                not decrypt_pw(EncryptHDR, RAND_HEAD_LEN, Flag,
                CRC32, ModifTime, VStr_Password);
            if bPasswordFailed then
              for i := 0 to 2 do
              begin
                if DialogBox(hInstance, Str_Dlg_Password, wndOwner,
                  @PasswordQueryDialogProc) <> idOk then
                  Break;
                bPasswordFailed :=
                  not decrypt_pw(EncryptHDR, RAND_HEAD_LEN, Flag,
                  CRC32, ModifTime, VStr_Password);
                if not bPasswordFailed then
                  Break;
                Windows.Beep(0, 0); //it's a dud, ? try again...
              end;
          finally
            FreeMem(EncryptHDR);
          end;
        end;

        if bPasswordFailed then
          Exit;

        VDW_CRC32Val := CRC_MASK;
        ProcessMessages;
        if VBool_Cancelled then
          Exit;

        VStr_OutFile := FileName;
        VH_OutFile := CreateFile(PChar(Filename), GENERIC_WRITE,
          FILE_SHARE_WRITE, nil, CREATE_ALWAYS, ExtFileAtt and $7F, 0);

        if VH_OutFile = INVALID_HANDLE_VALUE then
        begin                        
          ErrorMsgBoxFmt1(wndOwner, SFX_Err_CannotWriteFile, VStr_OutFile);
          Exit;
        end;

        try
          case ComprMethod of
            0: Unstore;
            8: Inflate(nil, 0);
          end;
          // set file time
          if CTime = 0 then
            FileSetDate(VH_OutFile, ModifTime + 65536 * ModifDate)
          else
          begin
            SetFileTime(VH_OutFile, PFileTime(@CTime), PFileTime(@ATime), PFileTime(@MTime));
          end;

          // 01/13/04: do crc32 checking, bail a warning message
          //      but do not stop if checksums do not match
          if rec.CRC32 <> (VDW_CRC32Val xor $FFFFFFFF) then
          begin
            ErrorMsgBoxFmt1(wndOwner, SFX_Err_CRC32, VStr_OutFile);
            Result := False;
          end
          else
            Result := True;

        finally
          if not CheckCloseHandle(VH_OutFile) then
          begin                                            
            ErrorMsgBoxFmt1(wndOwner, SFX_Err_CannotCloseFile, VStr_OutFile);
            Result := False;
          end;
        end;
      finally
        if bIsTempFile then
          RollBackTempFile(wndOwner, oldH);
      end;
    end;
  end;
end;

(*--------------------------------------------------------------------------*)

function Extract(wndOwner: hWnd): boolean;
var
  i, FileCount: longint;
  wndList: HWND;
  wndProgressBar: HWND;
  bExtracted, bPWFailed: boolean;
  recCentral: TZ64CentralEntry;
begin
  wndList := GetDlgItem(wndOwner, ID_LV_FILES);
  wndProgressBar := GetDlgItem(wndOwner, ID_PRG_EXTRACT);
  FileCount := SendMessage(wndList, LVM_GETITEMCOUNT, 0, 0);
  SendMessage(wndProgressBar, PBM_SETRANGE, 0, FileCount shl 16);
  SendMessage(wndProgressBar, PBM_SETPOS, 0, 0);

  VInt_LastSeq := -1;
  bPWFailed  := False;
  bExtracted := False;
  for i := 0 to FileCount - 1 do
  begin
    ProcessMessages;
    if VBool_Cancelled then
      Break;

    // update progres bar
    SendMessage(wndProgressBar, PBM_SETPOS, i + 1, 0);

    if SendMessage(wndList, LVM_GETITEMSTATE, i, LVIS_SELECTED) = LVIS_SELECTED then
      //if selected then...
    begin
      //get the target filename...
      VStr_CurrentFile := AppendDirSeparator(VStr_ExtractPath) +
        GetFileFromList(wndOwner, i, recCentral);

      if (VStr_CurrentFile <> '') and
        (VStr_CurrentFile[Length(VStr_CurrentFile)] = Chr_DirSep) then
      begin
        if (so_CreateEmptyDirs and VRec_SFXHeader.Options) <> 0 then
        begin
          if not ForceDirectories(RemoveDirSeparator(VStr_CurrentFile)) then
          begin
            ErrorMsgBoxFmt1(wndOwner, SFX_Err_Directory,
              RemoveDirSeparator(VStr_CurrentFile));
            bExtracted := False;
          end
          else
            bExtracted := True;
        end
        else
          bExtracted := False;
      end
      else
      begin
        if not ForceDirectories(ExtractFilePath(VStr_CurrentFile)) then
        begin                                                         
          ErrorMsgBoxFmt1(wndOwner, SFX_Err_Directory,
              RemoveDirSeparator(VStr_CurrentFile));
          Break;
        end;

        if (Integer(VRec_SFXHeader.DefOVW) <> som_Overwrite) and
          FileExists(PChar(VStr_CurrentFile)) then
        begin
          if Integer(VRec_SFXHeader.DefOVW) = som_Skip then
            continue;
          case DialogBox(hInstance, Str_Dlg_FileExists, wndOwner,
              @FileExistsDialogProc) of
            ID_BTN_YES: ;
            ID_BTN_NO: continue;
          end;
        end;

        // make sure the correct zip archive is open
        if (VInt_SpanType <> SFXSpanTypeNone{0}) then
          OpenRightArchive(wndOwner, recCentral.DiskStart);

        bExtracted := ExtractFile(wndOwner, VStr_CurrentFile, recCentral,
          bPWFailed);
      end;

      if bPWFailed then
        break //stop further processing!!!
      else
      if bExtracted then
        //unselect the file if successfully extracted with no errors...
        DeSelectInFilesListView(wndOwner, i);
    end;
  end;
  Result := SendMessage(wndList, LVM_GETSELECTEDCOUNT, 0, 0) = 0;
  if not Result then
    ShowFirstSelected(wndList); //may 11, 2002 : better visual feedback
end;

(*--------------------------------------------------------------------------*)

procedure ErrorHaltFmt(id: integer; const arg1: string);
begin
  ErrorHalt(FmtStrID1(id, arg1));
end;

procedure ErrorHaltID(id: integer);
begin
  ErrorHalt(SFXString(id));
end;

// fatal error, exit
procedure ErrorHalt(const sMsg: string);
{$ifdef DEBUG_SFX}
var err: DWORD;  m: string;
begin
  err := GetLastError;
  m := sMsg;
  if err <> 0 then
    m := m + ' ['+ IntToHex(err, 8) + ' '+ SysErrorMessage(err)+']';
  if m <> '' then
    ErrorMsgBox(0, m{sMsg});
  raise Exception.Create('Program halted');
//  Halt(1);
end;
{$else}
begin
  if sMsg <> '' then
    ErrorMsgBox(0, sMsg);
  Halt(1);
end;
{$endif}
(*--------------------------------------------------------------------------*)

function StrGetEditText(wndPar: HWND): string;
begin
  SetLength(Result, GetWindowTextLength(GetDlgItem(wndPar, ID_EDITBOX)) * 2);
  if Result <> '' then
  begin
    GetDlgItemText(wndPar, ID_EDITBOX, PChar(Result), Length(Result));
    Result := PChar(Result); // match length
  end;
end;

procedure AddFilesListViewCol(const wndDlg: HWND; const iIndex: integer;
  const szCaption: string; const iDirection, iWidth: integer);
var
  wndLV:  HWND;
  recCol: TLVColumn;
begin
  wndLV := GetDlgItem(wndDlg, ID_LV_FILES);
  with recCol do
  begin
    mask := LVCF_FMT or LVCF_SUBITEM or LVCF_TEXT or LVCF_WIDTH;
    fmt := iDirection;
    cx := iWidth;
    pszText := PChar(szCaption);
    cchTextMax := Length(szCaption);
    iSubItem := iIndex;
  end;
  SendMessage(wndLV, LVM_INSERTCOLUMN, iIndex, integer(@recCol));
end;

procedure SelectAllInFilesListView(const wndDlg: HWND);
var
  Item: TLVItem;
begin
  with Item do
  begin
    stateMask := LVIS_SELECTED;
    state := LVIS_SELECTED;
  end;
  SendDlgItemMessage(wndDlg, ID_LV_FILES, LVM_SETITEMSTATE, WPARAM(-1), longint(@Item));
end;

// get current directory
function GetCurDir: string;
var
  szBuf: array[0..MAX_PATH] of char;
begin
  SetString(Result, szBuf, GetCurrentDirectory(MAX_PATH, szBuf));
end;

function Int2Str(n: int64; wide: integer = -1): String;
var
  rev: array[0..25] of Char;
  i, k: Integer;
  prev: PChar;
begin
  i := 0;
  prev := @rev[25];
  prev^ := #0;
  while n <> 0 do
  begin
    inc(i);
    dec(prev);
    k := n mod 10;
    prev^ := Char(Ord('0') + k);
    n := n div 10;
  end;
  while (i < wide) and (i < 24) do
  begin
    inc(i);
    dec(prev);
    prev^ := '0';
  end;
  Result := String(prev);
end;

// return the Detached name
function DetachedName(const num: string): string;
begin
  Result := VStr_DetachName;
  if num <> '' then
    Result := Result + num;
  Result := Result + VStr_DetachExt;
end;

function LoadResource(id: integer): Pointer;
var
  hFind, hRes: THandle;
Begin
  Result := nil;
  hFind := Windows.FindResource(HInstance, PChar(id), RT_RCDATA);
  if hFind <> 0 then
  begin
    hRes := Windows.LoadResource(HInstance, hFind);
    if hRes <> 0 then
      Result := Windows.LockResource(hRes);
  end;
End;


procedure Finish;
begin
  if xbuf <> nil then
    FreeMem(xbuf);
//  FreeMem(VRec_Langs);
  ReAllocMem(p_Items, 0);
  if not CheckCloseHandle(VH_InFile) then
    ErrorMsgBoxFmt1(0, SFX_Err_CannotCloseFile, VStr_ExeName);
  if not CheckCloseHandle(VH_TempFile) then
    ErrorMsgBoxFmt1(0, SFX_Err_CannotCloseFile, VStr_TempFile);
  if VStr_TempFile <> '' then
    DeleteFile(PChar(VStr_TempFile));
  if not CheckCloseHandle(VH_OutFile) then
    ErrorMsgBoxFmt1(0, SFX_Err_CannotCloseFile, VStr_OutFile);
  FreeMem(VRec_Strings);
  FreeMem(VRec_DefStrings);
end;

procedure Run;
var
  sfi: TSHFileInfo; // to get shell image list handle
{$IFNDEF DEBUG_SFX}
  pBuf: array[0..MAX_PATH] of Char; // buffer for paramstr(0)
{$ENDIF}
  sVar: string;
  CCInfo: TCCInitCommonControlsEx;
begin
  // may 11, 2002: added support for environment variable %TICKS%
  sVar := Int2Str(GetTickCount, 0);
  SetEnvironmentVariable('TICKS', PChar(sVar));

  // initialize common controls (for the progress bar and listview)
  CCInfo.dwICC := ICC_LISTVIEW_CLASSES or ICC_PROGRESS_CLASS or ICC_STANDARD_CLASSES;
  CCInfo.dwSize := sizeof(TCCInitCommonControlsEx);
  InitCommonControlsEx(@CCInfo);

  // Created in the initialisation section of SFXDialogs.pas
  Make_CRC32Table;

  // get default strings
  if LoadLang(VRec_DefStrings, SFX_LANG_BASE) <= 0 then
    ErrorHaltID(SFX_Err_Archive);
  VStr_SFX_Caption := SFXString(SFX_Cap_App);
  SetLanguage;
  VStr_SFX_Caption := SFXString(SFX_Cap_App); // may be diferent language

{$IFNDEF DEBUG_SFX}  
    // needs less code than ParamStr(0)
    SetString(VStr_ExeName, pBuf, GetModuleFileName(0, pBuf, sizeof(pBuf)));
{$ENDIF}

  // open the archive file (i myself!)
  VH_InFile := CreateFile(PChar(VStr_ExeName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  //  If error, notify and abort
  if VH_InFile = INVALID_HANDLE_VALUE then
    ErrorHaltFmt(SFX_Err_Archive, VStr_ExeName);

  // read the TSFXFileHeader record (and the appended strings) from the file
  GetDefParams;

  // get the shell's image list handle
  FillChar(sfi, sizeof(sfi),0);
  VH_ShellImageList := SHGetFileInfo(PChar(VStr_ExeName), 0, sfi, sizeof(sfi),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

  // Display the dialog
  DialogBox(hInstance, Str_Dlg_Main, 0, @MainDialogProc);
end;

{$ifndef DEBUG_SFX}
initialization
  VStr_SFX_Caption := '';//SFXString(SFX_Cap_App);

finalization
  // cleanup
  Finish;
{$endif}
end.

