unit ZMUtils;

// ZMUtils.pas - Some utility functions

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
// modified 2014-01-03

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF WIN64}
{$DEFINE NO_ASM}
{$ENDIF}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, System.SysUtils, WinApi.Windows, VCL.Graphics,
{$ELSE}
  Classes, SysUtils, Windows, Graphics,
{$ENDIF}
  ZipMstr;

type
  TCharSet = set of AnsiChar;

type
  TPathSlashDirection = (PsdExternal, PsdInternal);
  TZPathTypes = (ZptNone, ZptError, ZptAbsolute, ZptRelDrive, ZptRelCurDrive,
    ZptRelCurDir, ZptExt, ZptUNC);

 const // QueryZip return bit values and errors
  ZqbStartEXE = 1; // is EXE file may be SFX
  ZqbStartLocal = 2; // normal zip file start
  ZqbStartSpan = 4; // first part of span
  ZqbStartCentral = 8; // continuing Central Header
  ZqbHasComment = 16;
  // zqbGoodComment = 16;  // comment length good (no junk at end)
  ZqbHasLocal = 32; // first Central entry points to local header
  ZqbHasCentral = 64; // Central entry where it should be
  ZqbHasEOC = 128; // End of Central entry
  ZqbHasLoc64 = 256; // EOC64 locator entry
  ZqbHasEOC64 = 512; // Zip64 EOC
  ZqbJunkAtEnd = 1024; // junk at end of zip
  ZqbIsDiskZero = 2048; // is disk 0

  ZqFieldError = -5; // bad field value
  ZqFileError = -7; // file handling error
  ZqGeneralError = -9; // unspecified failure

function AbsErr(Err: Integer): Integer;

function AttribStr(Attrs: Cardinal): string;

function BoolStr(const Value: Boolean): string;

function CanHash(const FSpec: string): Boolean;

function CheckSFXType(const Name: string; var ZipName: string;
  var Size: Integer): Integer; overload;

function CheckSFXType(AStream: TStream; var ZipName: string; var Size: Integer)
  : Integer; overload;

function CleanPath(var PathOut: string; const PathIn: string;
  NoLead: Boolean): Integer;

function DelimitPath(const Path: string; Sep: Boolean): string;

function DirExists(const FName: string): Boolean;

function DiskAvailable(const Path: string): Boolean;

function DriveLetter(const Path: string): Char;

// return exe size (if < 4G)
// 0 _ not exe
function ExeSize(const Name: string): Cardinal; overload;

function ExeSize(AStream: TStream): Cardinal; overload;

function ExeVers(const FName: string): Integer;

function ExeVersion(const FName: string; var MS, LS: DWORD): Boolean;

function ExtractNameOfFile(const FileName: string): string;

function FileDateToLocalDateTime(Stamp: Integer): TDateTime;

function FileTimeToLocalDOSTime(const Ft: TFileTime): Cardinal;

function FileTimeToLocalDateTime(const Ft: TFileTime): TDateTime;

// stable replacement for depreciated FileAge()
function File_Age(const FName: string): Cardinal;

procedure File_Close(var Fh: Integer);

procedure File_Delete(const FName: string);

function File_Size(const FSpec: TFilename): Int64;

function ForceDirectory(const DirName: string): Boolean;

function FormTempName(const Where: string): string;

function HashFunc(const Str: string): Cardinal;

function HashFuncNoCase(const Str: string): Cardinal;

function HasSpanSig(const FName: string): Boolean;

// returns position of first wild character or 0
function HasWild(const FSpec: string): Integer;

function HasWildW(const FSpec: WideString): Integer;

function Hi64(I: Int64): Cardinal;

function IsExtPath(const APath: string): Boolean;

function IsFolder(const Name: string): Boolean;
{$IFDEF UNICODE}
overload;
function IsFolder(const Name: TZMRawBytes): Boolean; overload;
{$ENDIF}

function IsWild(const FSpec: string): Boolean;

// true we're running under XP or later.
function IsWinXP: Boolean;

function IsZipSFX(const SFXExeName: string): Integer;

function LastChar(const Name: string): Char;
{$IFDEF UNICODE}
overload;

function LastChar(const Name: TZMRawBytes): AnsiChar; overload;
{$ENDIF}
function LastPos(const S: string; Ch: Char; Before: Integer = MAXINT): Integer;

function LastPosW(const S: WideString; Wch: Widechar;
  Before: Integer = MAXINT): Integer;

function Lo64(I: Int64): Cardinal;

// return true if filename is obviously invalid
function NameIsBad(const Astr: string; AllowWild: Boolean): Boolean;

function OEMToStr(const Astr: Ansistring): string;

function OpenResStream(const ResName: string; const Rtype: PChar)
  : TResourceStream;

function PathConcat(const Path, Extra: string): string;

function PathIsAbsolute(const APath: string): Boolean;

function PathType(const APath: string): TZPathTypes;

function QualifiedName(const ZipName: string; const FileName: string): string;

function QueryZip(const FName: string): Integer;

// find last SubStr in a string
function RPos(const SubStr, AString: string): Integer; overload;

function RPos(const SubStr, AString: string; StartPos: Integer)
  : Integer; overload;

function SetSlash(const Path: string; Dir: TPathSlashDirection): string;

function SetSlashW(const Path: WideString; Dir: TPathSlashDirection)
  : WideString;

procedure SplitArgs(const Args: string; var Main: string; var Filearg: string;
  var Switches: string; var Password: string);

procedure SplitQualifiedName(const QName: string; var ZipName: string;
  var FileName: string);

function StrHasExt(const Astr: AnsiString): Boolean; overload;
{$IFDEF UNICODE}
// 1 return True if contains chars (<#31 ?) >#126
function StrHasExt(const Astr: string): Boolean; overload;

function StrHasExt(const Astr: TZMRawBytes): Boolean; overload;
{$ENDIF}
function StrToOEM(const Astr: string): string;

// trim and dequote
function Unquote(const FName: string): string;

function VersStr(Vers: Integer; Comma: Boolean = False): string;

function WinPathDelimiters(const Path: string): string;

function WinVersion: Integer;

function XData(const X: TZMRawBytes; Tag: Word; var Idx, Size: Integer)
  : Boolean;

function XDataAppend(var X: TZMRawBytes; const Src1; Siz1: Integer; const Src2;
  Siz2: Integer): Integer;

function XDataKeep(const X: TZMRawBytes; const Tags: array of Integer)
  : TZMRawBytes;

function XDataRemove(const X: TZMRawBytes; const Tags: array of Integer)
  : TZMRawBytes;

function ZSplitString(const Delim, Raw: string; var Rest: string): string;

// split string at last delim
function ZSplitStringLast(const Delim, Raw: string; var Rest: string): string;

// check for SFX header or detached header
// return <0 error
const
  CstNone = 0; // not found
  CstExe = 1; // might be stub of unknown type
  CstSFX17 = 17; // found 1.7 SFX headers
  CstSFX19 = 19; // found 1.9 SFX headers
  CstDetached = 2048;
  // is detached - if Name specified ZipName will modified for it

const
  FILETIME_ZERO = -109205; // Jan 1, 1601
  // return true if found (and no time conversion error)
function FileLastModified(const FileName: string;
  var LastMod: TDateTime): Boolean;

// test for invalid characters
function IsInvalidIntName(const FName: string): Boolean;

function AppendToName(const Name, Suffix: string): string;

function IntToBase36(Utim: Cardinal): string;

function SysErrorMsg(ErrNo: Cardinal = Cardinal(-1)): string;

function ShortPathName(const APath: string): string;
function ShortenPath(const APath: string): string;

const
  Z_BAD_DRIVE = -1;
  Z_BAD_UNC = -2;
  Z_BAD_SEP = -3;
  Z_BAD_SPACE = -4; // trail space
  Z_BAD_DOT = -5; // trailing dot
  Z_BAD_CLEN = -6; // component too long
  Z_BAD_CHAR = -7; // invalid char
  Z_BAD_NAME = -8; // has reserved Name
  Z_BAD_PARENT = -9; // attempt to back below root
  Z_IS_THIS = -10;
  Z_IS_PARENT = -11;
  Z_EMPTY = 1;
  Z_WILD = 2; // 1;

  // -------------------------- ------------ -------------------------
implementation

uses
{$IFDEF VERDXE2up}
  WinApi.ShellApi, VCL.Forms,
{$ELSE}
  ShellApi, Forms, {$IFNDEF VERD7up}ComObj, ActiveX, {$ENDIF}
{$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMStructs, ZMSFXInt, ZMWinFuncs, ZMMsg;

type
  TInt64Rec = packed record
    case Integer of
      0:
        (I: Int64);
      1:
        (Lo, Hi: Cardinal);
  end;

  // --------------------------------------------------------
function Lo64(I: Int64): Cardinal;
var
  R: TInt64Rec;
begin
  R.I := I;
  Result := R.Lo;
end;

function Hi64(I: Int64): Cardinal;
var
  R: TInt64Rec;
begin
  R.I := I;
  Result := R.Hi;
end;

// --------------------------------------------------------
function AbsErr(Err: Integer): Integer;
begin
  if Err < 0 then
    Result := -Err
  else
    Result := Err;
  Result := Result and ZERR_ERROR_MASK; // remove extended info
end;

function AttribStr(Attrs: Cardinal): string;
type
  Attrval = record
    V: Cardinal;
    D: array [Boolean] of Char;
  end;
const
  AttrVals: array [0 .. 6] of Attrval = ((V: 1; D: ('r', 'R')), (V: 2;
    D: ('h', 'H')), (V: 4; D: ('s', 'S')), (V: $20; D: ('a', 'A')), (V: $100;
    D: ('t', 'T')), (V: $1000; D: ('o', 'O')), (V: $4000; D: ('e', 'E')));
var
  I: Integer;
begin
  SetLength(Result, high(AttrVals) + 1);
  for I := low(AttrVals) to high(AttrVals) do
    Result[I + 1] := AttrVals[I].D[(Attrs and AttrVals[I].V) <> 0];
end;

function DelimitPath(const Path: string; Sep: Boolean): string;
begin
  Result := Path;
  if Length(Path) = 0 then
  begin
    if Sep then
      Result := PathDelim{'\'};
    // exit;
  end
  else
    if (AnsiLastChar(Path)^ = PathDelim) <> Sep then
    begin
      if Sep then
        Result := Path + PathDelim
      else
        Result := Copy(Path, 1, Pred(Length(Path)));
    end;
end;

function WinPathDelimiters(const Path: string): string;
var
  I: Integer;
begin
  Result := Path;
  for I := 1 to Length(Result) do
    if Result[I] = '/' then
      Result[I] := '\';
end;

function DirExists(const FName: string): Boolean;
begin
  Result := _Z_DirExists(FName);
end;

function DiskAvailable(const Path: string): Boolean;
var
  Drv: Integer;
  Em: Cardinal;
  Pth: string;
begin
  Result := False;
  Pth := ExpandUNCFileName(Path);
  if (Length(Pth) > 1) and (Pth[2] = DriveDelim) then
  begin
    Drv := Ord(Uppercase(Pth)[1]) - $40;
    Em := SetErrorMode(SEM_FAILCRITICALERRORS);
    Result := DiskSize(Drv) <> -1;
    SetErrorMode(Em);
  end;
end;

function ExeVersion(const FName: string; var MS, LS: DWORD): Boolean;
begin
  Result := _Z_GetExeVersion(FName, MS, LS);
end;

// format M.N.RR.BBB
// return Version as used by DelphiZip
function ExeVers(const FName: string): Integer;
var
  LS: DWORD;
  MS: DWORD;
begin
  Result := -1;
  if ExeVersion(FName, MS, LS) then
  begin
    Result := (Integer(MS) shr 16) * 1000000;
    Result := Result + (Integer(MS and $FFFF) * 100000);
    Result := Result + ((Integer(LS) shr 16) * 10000);
    Result := Result + Integer(LS and $FFFF) mod 1000;
  end;
end;

function ExtractNameOfFile(const FileName: string): string;
var
  I: Integer;
  J: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  J := LastDelimiter('.', FileName);
  if (J <= I) then
  begin
    J := MaxInt;
  end; // no ext
  Result := Copy(FileName, I + 1, J - (I + 1));
end;

function VersStr(Vers: Integer; Comma: Boolean = False): string;
const
  Fmt: array [Boolean] of string = ('%d.%d.%d.%4.4d', '%d,%d,%d,%d');
begin
  Result := Format(Fmt[Comma], [Vers div 1000000, (Vers mod 1000000) div 100000,
    (Vers mod 100000) div 10000, Vers mod 1000]);
end;

function OpenResStream(const ResName: string; const Rtype: PChar)
  : TResourceStream;
var
  HFindRes: Cardinal;
  IdNo: Integer;
  Inst: Integer;
  Rsn: PChar;
begin
  Result := nil;
  try
    Rsn := PChar(ResName);
    Inst := HInstance;
    if (Length(ResName) > 1) and (ResName[1] = '#') then
    begin
      IdNo := StrToInt(Copy(ResName, 2, 25));
      Rsn := PChar(IdNo);
    end;
    HFindRes := FindResource(Inst, Rsn, Rtype);
    if (HFindRes = 0) and ModuleIsLib then
    begin
      Inst := MainInstance;
      HFindRes := FindResource(Inst, Rsn, Rtype);
    end;
    if HFindRes <> 0 then
      Result := TResourceStream.Create(Inst, ResName, Rtype);
  except
    Result := nil;
  end;
end;

function File_Age(const FName: string): Cardinal;
var
  LocalFileTime: TFileTime;
  R: Integer;
  SRec: _Z_TSearchRec;
begin
  Result := Cardinal(-1);
  R := _Z_FindFirst(FName, FaAnyFile, SRec);
  if R = 0 then
  begin
    FileTimeToLocalFileTime(SRec.FindData.FtLastWriteTime, LocalFileTime);
    _Z_FindClose(SRec);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
      LongRec(Result).Lo);
  end;
end;

procedure File_Close(var Fh: Integer);
var
  H: Integer;
begin
  if Fh <> Invalid_Handle then
  begin
    H := Fh;
    Fh := Invalid_Handle;
    FileClose(H);
  end;
end;

procedure File_Delete(const FName: string);
begin
  _Z_EraseFile(FName, True);
end;

function File_Size(const FSpec: TFilename): Int64;
var
  Sr: _Z_TSearchRec;
begin
  Result := 0;
  if _Z_FindFirst(FSpec, FaAnyFile, Sr) = 0 then
  begin
    Result := Sr.Size;
    _Z_FindClose(Sr);
  end;
end;

// return true on success
function ForceDirectory(const DirName: string): Boolean;
begin
  Result := _Z_ForceDirectory(DirName);
end;

(*? HasWild
 returns position of first wild character or 0
*)
function HasWild(const FSpec: string): Integer;
var
  C: Char;
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(FSpec) do
  begin
    C := FSpec[I];
    if (C = WILD_MULTI) or (C = WILD_CHAR) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

(*? HasWildW
 returns position of first wild character or 0
*)
function HasWildW(const FSpec: WideString): Integer;
var
  C: Widechar;
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(FSpec) do
  begin
    C := FSpec[I];
    if (C = WILD_MULTI) or (C = WILD_CHAR) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

(*? IsWild
 1.73.4
 returns true if filespec contains wildcard(s)
*)
function IsWild(const FSpec: string): Boolean;
var
  C: Char;
  I: Integer;
  Len: Integer;
begin
  Result := True;
  Len := Length(FSpec);
  I := 1;
  while I <= Len do
  begin
    C := FSpec[I];
    if (C = WILD_MULTI) or (C = WILD_CHAR) then
      Exit;
    Inc(I);
  end;
  Result := False;
end;

(* ? IsZipSFX
  Return value:
 0 = The specified file is not a SFX
 >0 = It is one
 -7  = Open, read or seek error
 -8  = memory error
 -9  = exception error
 -10 = all other exceptions
*)
function IsZipSFX(const SFXExeName: string): Integer;
const
  SFXsig = ZqbStartEXE or ZqbHasCentral or ZqbHasEOC;
var
  N: string;
  R: Integer;
  Sz: Integer;
begin
  R := QueryZip(SFXExeName);
  // SFX = 1 + 128 + 64
  Result := 0;
  if (R and SFXsig) = SFXsig then
    Result := CheckSFXType(SFXExeName, N, Sz);
end;

function CanHash(const FSpec: string): Boolean;
var
  C: Char;
  I: Integer;
  Len: Integer;
begin
  Result := False;
  Len := Length(FSpec);
  I := 1;
  while I <= Len do
  begin
    C := FSpec[I];
    if (C = WILD_MULTI) or (C = WILD_CHAR) or (C = SPEC_SEP) then
      Exit;
    Inc(I);
  end;
  Result := True;
end;

// Returns a boolean indicating whether or not we're running under XP or later.
function IsWinXP: Boolean;
var
  Osv: TOSVERSIONINFO;
begin
  Osv.DwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(Osv);
  Result := (Osv.DwMajorVersion > 5) or
    ((Osv.DwMajorVersion = 5) and (Osv.DwMinorVersion >= 1));
end;

// Returns a boolean indicating whether or not we're running under XP or later.
function WinVersion: Integer;
var
  Osv: TOSVERSIONINFO;
begin
  Osv.DwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(Osv);
  Result := (Osv.DwMajorVersion * 100) + Osv.DwMinorVersion;
end;

(*? SetSlash
 1.76 use enum  TPathSlashDirection = (psdExternal, psdInternal)
 1.73
 forwardSlash = false = Windows normal backslash '\'
 forwardSlash = true = forward slash '/'
*)
function SetSlash(const Path: string; Dir: TPathSlashDirection): string;
{$IFDEF Delphi7up}
begin
  if Dir = PsdInternal then
    Result := AnsiReplaceStr(Path, PathDelim, PathDelimAlt)
  else
    Result := AnsiReplaceStr(Path, PathDelimAlt, PathDelim);
end;
{$ELSE}

var
  C, F, R: Char;
  I, Len: Integer;
begin
  Result := Path;
  Len := Length(Path);
  if Dir = PsdInternal then
  begin
    F := PathDelim{'\'};
    R := PathDelimAlt; // '/';
  end
  else
  begin
    F := PathDelimAlt; // '/';
    R := PathDelim{'\'};
  end;
  I := 1;
  while I <= Len do
  begin
    C := Path[I];
{$IFNDEF UNICODE}
    if C in LeadBytes then
    begin
      Inc(I, 2);
      Continue;
    end;
{$ENDIF}
    if C = F then
      Result[I] := R;
    Inc(I);
  end;
end;
{$ENDIF}

function SetSlashW(const Path: WideString; Dir: TPathSlashDirection)
  : WideString;
var
  C: Widechar;
  F: Widechar;
  I: Integer;
  Len: Integer;
  R: Widechar;
begin
  Result := Path;
  Len := Length(Path);
  if Dir = PsdInternal then
  begin
    F := PathDelim;
    R := PathDelimAlt;
  end
  else
  begin
    F := PathDelimAlt;
    R := PathDelim;
  end;
  I := 1;
  while I <= Len do
  begin
    C := Path[I];
    if C = F then
      Result[I] := R;
    Inc(I);
  end;
end;

// ---------------------------------------------------------------------------
// concat path
function PathConcat(const Path, Extra: string): string;
var
  PathLen: Integer;
  PathLst: Char;
begin
  PathLen := Length(Path);
  Result := Path;
  if PathLen > 0 then
  begin
    PathLst := AnsiLastChar(Path)^;
    if ((PathLst <> DriveDelim) and (Length(Extra) > 0)) and
      ((Extra[1] = PathDelim) = (PathLst = PathDelim)) then
    begin
      if PathLst = PathDelim then
        Result := Copy(Path, 1, PathLen - 1) // remove trailing
      else
        Result := Path + PathDelim;
    end;
  end;
  Result := Result + Extra;
end;


(* const           // QueryZip return bit values and errors
 zqbStartEXE    = 1;     // is EXE file may be SFX
 zqbStartLocal  = 2;     // normal zip file start
 zqbStartSpan   = 4;     // first part of span
 zqbStartCentral = 8;    // continuing Central Header
 zqbHasComment  = 16;
 zqbHasLocal    = 32;    // first Central entry points to local header
 zqbHasCentral  = 64;    // Central entry where it should be
 zqbHasEOC      = 128;   // End of Central entry
 zqbHasLoc64    = 256;   // EOC64 locator entry
 zqbHasEOC64    = 512;   // Zip64 EOC
 zqbJunkAtEnd   = 1024;  // junk at end of zip
 zqbIsDiskZero  = 2048;  // is disk 0

 zqFieldError   = -5;    // bad field value
 zqFileError    = -7;     // file handling error
 zqGeneralError = -9;  // unspecified failure
*)

function QueryZip(const FName: string): Integer;
const
  FileMask = (ZqbStartEXE or ZqbStartLocal or ZqbStartSpan or ZqbStartCentral or
    ZqbHasComment or ZqbJunkAtEnd);
var
  Buf: array of Byte;
  BufPos: Integer;
  CenDisk: Cardinal;
  CenOfs: Int64;
  DoCenDir: Boolean;
  EOC: TZipEndOfCentral;
  EOCLoc: TZip64EOCLocator;
  EOCPossible: Boolean;
  FileHandle: Integer;
  File_Sze: Int64;
  Fn: string;
  Fs: Int64;
  Need64: Boolean;
  PEOC: PZipEndOfCentral;
  PEOCLoc: PZip64EOCLocator;
  Pos0: Integer;
  ReadPos: Cardinal;
  Res: Integer;
  Sig: Cardinal;
  Size: Integer;
  ThisDisk: Cardinal;

  function NeedLoc64(const QEOC: TZipEndOfCentral): Boolean;
  begin
    Result := (QEOC.ThisDiskNo = MAX_WORD) or (QEOC.CentralDiskNo = MAX_WORD) or
      (QEOC.CentralEntries = MAX_WORD) or (QEOC.TotalEntries = MAX_WORD) or
      (QEOC.CentralSize = MAX_UNSIGNED) or (QEOC.CentralOffset = MAX_UNSIGNED);
  end;
// check central entry and, if same disk, its local header signal
  function CheckCen(Fh: Integer; This_Disk: Cardinal; CenOf: Int64): Integer;
  type
    TXData_tag = packed record
      Tag: Word;
      Siz: Word;
    end;

    PXData_tag = ^TXData_tag;

  var
    Ret: Integer;
    CentralHead: TZipCentralHeader;
    Sgn: Cardinal;
    Ofs: Int64;
    Xbuf: array of Byte;
    Xlen, Ver: Integer;
    Wtg, Wsz: Word;
    Has64: Boolean;
    P: PByte;
  begin // verify start of central
    Ret := 0;
    Result := ZqFieldError;
    if (FileSeek(Fh, CenOf, SoFromBeginning) <> -1) and
      (FileRead(Fh, CentralHead, Sizeof(CentralHead)) = Sizeof(CentralHead)) and
      (CentralHead.HeaderSig = CentralFileHeaderSig) then
    begin
      Ret := ZqbHasCentral; // has linked Central
      if (CentralHead.DiskStart = This_Disk) then
      begin
        Ver := CentralHead.VersionNeeded;
        if (Ver and VerMask) > ZIP64_VER then
          Exit;
        Ofs := CentralHead.RelOffLocalHdr;
        if (Ofs = MAX_UNSIGNED) and ((Ver and VerMask) >= ZIP64_VER) then
        begin
          if Ver > 45 then
            Exit; // bad version
          // have to read extra data
          Xlen := CentralHead.FileNameLen + CentralHead.ExtraLen;
          SetLength(Xbuf, Xlen); // easier to read filename + extra
          if FileRead(Fh, Xbuf, Xlen) <> Xlen then
            Exit; // error
          // find Zip64 extra data
          Has64 := False;
          Xlen := CentralHead.ExtraLen;
          P := @Xbuf[CentralHead.FileNameLen];
          Wsz := 0; // keep compiler happy
          while Xlen > Sizeof(TXData_tag) do
          begin
            Wtg := PXData_tag(P)^.Tag;
            Wsz := PXData_tag(P)^.Siz;
            if Wtg = Zip64_data_tag then
            begin
              Has64 := Xlen >= (Wsz + Sizeof(TXData_tag));
              Break;
            end;
            Inc(P, Wsz + Sizeof(TXData_tag));
          end;
          if (not Has64) or (Wsz > (Xlen - Sizeof(TXData_tag))) then
            Exit; // no data so rel ofs is bad
          Inc(P, Sizeof(TXData_tag)); // past header
          // locate offset  - values only exist if needed
          if CentralHead.UncomprSize = MAX_UNSIGNED then
          begin
            if Wsz < Sizeof(Int64) then
              Exit; // bad
            Inc(P, Sizeof(Int64));
            Dec(Wsz, Sizeof(Int64));
          end;
          if CentralHead.ComprSize = MAX_UNSIGNED then
          begin
            if Wsz < Sizeof(Int64) then
              Exit; // bad
            Inc(P, Sizeof(Int64));
            Dec(Wsz, Sizeof(Int64));
          end;
          if Wsz < Sizeof(Int64) then
            Exit; // bad
          Ofs := PInt64(P)^;
        end;
        if (FileSeek(Fh, Ofs, 0) <> -1) and
          (FileRead(Fh, Sgn, Sizeof(Sgn)) = Sizeof(Sgn)) and
          (Sgn = LocalFileHeaderSig) then
          Ret := ZqbHasCentral or ZqbHasLocal; // linked local
      end;
    end;
    Result := Ret;
  end;

begin
  EOCPossible := False;
  Result := ZqFileError;
  DoCenDir := True; // test central too
  if (FName <> '') and (FName[1] = '|') then
  begin
    DoCenDir := False;
    Fn := Copy(FName, 2, Length(FName) - 1);
  end
  else
    Fn := FName;
  Fn := Trim(Fn);
  if Fn = '' then
    Exit;
  FileHandle := Invalid_Handle;
  Res := 0;
  try
    try
      // Open the input archive, presumably the last disk.
      FileHandle := _Z_FileOpen(Fn, FmShareDenyWrite or FmOpenRead);
      if FileHandle = Invalid_Handle then
        Exit;
      Result := 0; // rest errors normally file too small

      // first we check if the start of the file has an IMAGE_DOS_SIGNATURE
      if (FileRead(FileHandle, Sig, Sizeof(Cardinal)) <> Sizeof(Cardinal)) then
        Exit;
      if LongRec(Sig).Lo = IMAGE_DOS_SIGNATURE then
        Res := ZqbStartEXE
      else
        if Sig = LocalFileHeaderSig then
          Res := ZqbStartLocal
        else
          if Sig = CentralFileHeaderSig then
            Res := ZqbStartCentral
            // part of split Central Directory
          else
            if Sig = ExtLocalSig then
              Res := ZqbStartSpan; // first part of span

      // A test for a zip archive without a ZipComment.
      Fs := FileSeek(FileHandle, -Int64(Sizeof(EOC)), SoFromEnd);
      if Fs = -1 then
        Exit; // not zip - too small
      File_Sze := Fs;
      // try no comment
      if (FileRead(FileHandle, EOC, Sizeof(EOC)) = Sizeof(EOC)) and
        (EOC.HeaderSig = EndCentralDirSig) and (EOC.ZipCommentLen = 0) then
      begin
        EOCPossible := True;
        Res := Res or ZqbHasEOC;
        CenDisk := EOC.CentralDiskNo;
        ThisDisk := EOC.ThisDiskNo;
        CenOfs := EOC.CentralOffset;
        Need64 := NeedLoc64(EOC);
        if (CenDisk = 0) and (ThisDisk = 0) then
          Res := Res or ZqbIsDiskZero;
        // check Zip64 EOC
        if Need64 and (Fs > Sizeof(TZip64EOCLocator)) then
        begin // check for locator
          if (FileSeek(FileHandle, Fs - Sizeof(TZip64EOCLocator),
            SoFromBeginning) <> -1) and
            (FileRead(FileHandle, EOCLoc, Sizeof(TZip64EOCLocator))
            = Sizeof(TZip64EOCLocator)) and (EOCLoc.LocSig = EOC64LocatorSig)
          then
          begin // found possible locator
            Res := Res or ZqbHasLoc64;
            CenDisk := 0;
            ThisDisk := 1;
            CenOfs := -1;
          end;
        end;
        if DoCenDir and (CenDisk = ThisDisk) then
        begin
          Res := Res or CheckCen(FileHandle, ThisDisk, CenOfs);
          Exit;
        end;
        Res := Res and FileMask; // remove rest
      end;
      // try to locate EOC
      Inc(File_Sze, Sizeof(EOC));
      Size := MAX_WORD + Sizeof(EOC) + Sizeof(TZip64EOCLocator);
      if Size > File_Sze then
        Size := File_Sze;
      SetLength(Buf, Size);
      Pos0 := Size - (MAX_WORD + Sizeof(TZipEndOfCentral));
      if Pos0 < 0 then
        Pos0 := 0; // lowest buf position for eoc
      ReadPos := File_Sze - Size;
      if (FileSeek(FileHandle, Int64(ReadPos), SoFromBeginning) <> -1) and
        (FileRead(FileHandle, Buf[0], Size) = Size) then
      begin
        // Finally try to find the EOC record within the last 65K...
        BufPos := Size - (Sizeof(EOC));
        PEOC := PZipEndOfCentral(@Buf[Size - Sizeof(EOC)]);
        // reverse search
        while BufPos > Pos0 do // reverse search
        begin
          Dec(BufPos);
          Dec(PAnsiChar(PEOC));
          if PEOC^.HeaderSig = EndCentralDirSig then
          begin // possible EOC found
            Res := Res or ZqbHasEOC; // EOC
            // check correct length comment
            if (BufPos + Sizeof(EOC) + PEOC^.ZipCommentLen) <= Size then
              Res := Res or ZqbHasComment; // good comment length
            if (BufPos + Sizeof(EOC) + PEOC^.ZipCommentLen) <> Size then
              Res := Res or ZqbJunkAtEnd; // has junk
            CenDisk := PEOC^.CentralDiskNo;
            ThisDisk := PEOC^.ThisDiskNo;
            if (CenDisk = 0) and (ThisDisk = 0) then
              Res := Res or ZqbIsDiskZero;
            CenOfs := PEOC^.CentralOffset;
            Need64 := NeedLoc64(PEOC^);
            // check Zip64 EOC
            if Need64 and ((BufPos - Sizeof(TZip64EOCLocator)) >= 0) then
            begin // check for locator
              PEOCLoc := PZip64EOCLocator
                (@Buf[BufPos - Sizeof(TZip64EOCLocator)]);
              if PEOCLoc^.LocSig = EOC64LocatorSig then
              begin // found possible locator
                Res := Res or ZqbHasLoc64;
                CenDisk := 0;
                ThisDisk := 1;
                CenOfs := -1;
              end;
            end;
            if DoCenDir and (CenDisk = ThisDisk) then
            begin // verify start of central
              Res := Res or CheckCen(FileHandle, ThisDisk, CenOfs);
              Break;
            end;
            Res := Res and FileMask; // remove rest
            Break;
          end;
        end; // while
      end;
      if EOCPossible then
        Res := Res or ZqbHasEOC;
    except
      Result := ZqGeneralError;
    end;
  finally
    File_Close(FileHandle);
    if Result = 0 then
      Result := Res;
  end;
end;

function OEMToStr(const Astr: Ansistring): string;
var
  Buf: string;
begin
  SetLength(Buf, Length(Astr) + 3); // allow worst case
  OemToChar(PAnsiChar(Astr), PChar(Buf));
  Result := PChar(Buf);
end;

function StrToOEM(const Astr: string): string;
var
  Buf: Ansistring;
begin
  SetLength(Buf, Length(Astr) + 3); // allow worst case
  CharToOem(PChar(Astr), PAnsiChar(Buf));
  Buf := PAnsiChar(Buf); // remove trailing nul
  Result := string(Buf);
end;

{
 return true if contains chars (<#31 ?) >#126
}
function StrHasExt(const Astr: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Astr) do
    if (Astr[I] > #126) or (Astr[I] < #31) then
    begin
      Result := True;
      Break;
    end;
end;

{$IFDEF UNICODE}
function StrHasExt(const Astr: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Astr) do
    if (Astr[I] > #126) or (Astr[I] < #31) then
    begin
      Result := True;
      Break;
    end;
end;

function StrHasExt(const Astr: TZMRawBytes): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Astr) do
    if (Astr[I] > #126) or (Astr[I] < #31) then
    begin
      Result := True;
      Break;
    end;
end;
{$ENDIF}

function LastPos(const S: string; Ch: Char; Before: Integer = MAXINT): Integer;
var
  I: Integer;
begin
  Result := 0; // not found
  for I := 1 to Length(S) do
  begin
    if I >= Before then
      Break;
    if S[I] = Ch then
      Result := I;
  end;
end;

function LastPosW(const S: WideString; Wch: Widechar;
  Before: Integer = MAXINT): Integer;
var
  I: Integer;
begin
  Result := 0; // not found
  for I := 1 to Length(S) do
  begin
    if I >= Before then
      Break;
    if S[I] = Wch then
      Result := I;
  end;
end;

function PathType(const APath: string): TZPathTypes;
const
  DriveChars = ['a' .. 'z', 'A' .. 'Z'];
const
  BadChars = [#0 .. #31, '<', '>', ':', '"', '*', '?', '/', '\', '|'];
var
  FirstChar: Char;
  I: Integer;
  InShare: Integer;
  Tmp: string;
begin
  Result := ZptNone;
  if APath = '' then
    Exit;
  Tmp := Copy(APath, 1, 8) + #0#0#0#0#0#0#0#0; // force at least 8 chars
  FirstChar := APath[1];
  if FirstChar = '\' then
  begin
    if Tmp[2] = FirstChar then
    begin
      // URL or Ext
      if (Tmp[3] = '?') and (Tmp[4] = FirstChar) then
      begin
        // really should check drive:\
        if CharInSet(Tmp[5], DriveChars) and (Tmp[6] = ':') and
          (Tmp[7] = FirstChar) and not CharInSet(Tmp[8], BadChars) then
          Result := ZptExt
        else
          Result := ZptError;
      end
      else
      begin
        InShare := 0;
        Result := ZptError;
        for I := 3 to Length(APath) do
        begin
          if CharInSet(APath[I], BadChars) then
          begin
            if APath[I] = FirstChar then
            begin
              if (InShare > 0) and (I > InShare + 1) then
              begin
                Result := ZptUNC;
                Break; // done
              end;
              if (I > 3) and (InShare = 0) then
                InShare := I
              else
                Break;
            end
            else
              Break; // bad
          end;
        end;
      end;
    end
    else
      Result := ZptRelCurDrive;
  end
  else
  begin
    if CharInSet(FirstChar, DriveChars) and (Tmp[2] = ':') then
    begin
      if Tmp[3] = '\' then
        Result := ZptAbsolute
      else
        Result := ZptRelDrive;
    end
    else
      Result := ZptRelCurDir;
  end;
end;

function IsFolder(const Name: string): Boolean;
var
  Ch: Char;
begin
  Result := False;
  if Name <> '' then
  begin
    Ch := Name[Length(Name)];
    Result := (Ch = PathDelim) or (Ch = PathDelimAlt);
  end;
end;

{$IFDEF UNICODE}
function IsFolder(const Name: TZMRawBytes): Boolean;
var
  Ch: AnsiChar;
begin
  Result := False;
  if Name <> '' then
  begin
    Ch := Name[Length(Name)];
    Result := (Ch = PathDelim) or (Ch = PathDelimAlt);
  end;
end;
{$ENDIF}

function LastChar(const Name: string): Char;
begin
  Result := #0;
  if Name <> '' then
    Result := Name[Length(Name)];
end;

{$IFDEF UNICODE}
function LastChar(const Name: TZMRawBytes): AnsiChar;
begin
  Result := #0;
  if Name <> '' then
    Result := Name[Length(Name)];
end;
{$ENDIF}

// return true if internal filename is obviously invalid
function NameIsBad(const Astr: string; AllowWild: Boolean): Boolean;
var
  C: Char;
  I: Integer;
begin
  Result := (Astr = '') or (Astr[1] = '\') or (Length(Astr) > MAX_PATH);
  if not Result then
  begin
    for I := 1 to Length(Astr) do
    begin
      C := Astr[I];
{$IFDEF UNICODE}
      if CharInSet(C, [#0 .. #31, ':', '<', '>', '|']) or
        ((not AllowWild) and CharInSet(C, ['*', '?'])) then
{$ELSE}
      if (C in [#0 .. #31, ':', '<', '>', '|']) or
        ((not AllowWild) and (C in ['*', '?'])) then
{$ENDIF}
      begin
        Result := True;
        Break;
      end;
    end;
  end;
  if not Result then
    Result := (AnsiPos('..\', Astr) > 0) or (AnsiPos(' \', Astr) > 0);
end;

// Drive:[\folder]
// \\server\user\[folder]  - can be simplified to '\\'
// .\[folder]   - relative to current dir
// ..\[folder]  - relative to owner of current dir
// \[folder] - root of current drive
function PathIsAbsolute(const APath: string): Boolean;
var
  Ch1: Char;
  Ch2: Char;
begin
  if APath = '' then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  Ch1 := APath[1];
  if Length(APath) > 1 then
    Ch2 := APath[2]
  else
    Ch2 := #0;
  if (Ch2 = ':') and CharInSet(Ch1, ['A' .. 'Z', 'a' .. 'z']) then
    Exit;
  if Ch1 = '\' then
    Exit;
  if Ch1 = '.' then
  begin
    if (Ch2 = '\') or ((Ch2 = '.') and (Length(APath) > 2) and (APath[3] = '\'))
    then
      Exit;
  end;
  Result := False;
end;

function NextDelim(const S: string; Stt: Integer; Delims: TSysCharSet): Integer;
begin
  Result := 0;
  if Stt < 1 then
    Stt := 1;
  while Stt <= Length(S) do
  begin
    if CharInSet(S[Stt], Delims) then
    begin
      Result := Stt;
      Break;
    end;
    Inc(Stt);
  end;
end;

function CheckComponent(const Part: string): Integer;
const
  Bad_Chars = [#0 .. #31, '<', '>', ':', '\', '/', '"', '|', '?', '*'];
  Wild_Chars = ['?', '*'];
  Reserved1 = ['a', 'A', 'c', 'C', 'l', 'L', 'n', 'N', 'p', 'P'];
  Reserved: array [0 .. 5] of string = ('COM', 'LPT', 'AUX', 'CON',
    'NUL', 'PRN');
var
  I: Integer;
  P3: string;
  Typ: Integer;
  Wilds: Integer;
begin
  Result := Z_BAD_SEP;
  if Length(Part) < 1 then
    Exit;
  Result := Z_BAD_CLEN;
  if Length(Part) > 255 then // MAX_PATH - 3
    Exit;
  Result := Z_BAD_SPACE;
  if LastChar(Part) = ' ' then
    Exit;
  Wilds := 0;
  Result := Z_BAD_CHAR;
  for I := 1 to Length(Part) do
    if CharInSet(Part[I], Bad_Chars) then
    begin
      if not CharInSet(Part[I], Wild_Chars) then
        Exit;
      Inc(Wilds);
    end;
  Result := Z_IS_THIS;
  if Part = '.' then
    Exit;
  Result := Z_IS_PARENT;
  if Part = '..' then
    Exit;
  Result := Z_BAD_DOT;
  if LastChar(Part) = '.' then
    Exit;
  // check reserved names
  if (Length(Part) >= 3) and CharInSet(Part[1], Reserved1) then
  begin
    P3 := Copy(Part, 1, 3);
    Typ := 0;
    for I := 0 to 5 do
      if CompareStr(P3, Reserved[I]) = 0 then
      begin
        // maybe bad
        if I < 2 then
          Typ := 1
        else
          Typ := 2;
        Break;
      end;
    if (Typ = 1) and (Length(Part) >= 4) and CharInSet(Part[4], ['1' .. '9'])
    then
    begin // com or lpt
      // technically com?.ext is permitted but it is not recommended
      if (Length(Part) = 4) or ((Length(Part) > 4) and (Part[5] = '.')) then
      begin
        Result := Z_BAD_NAME;
        Exit;
      end;
    end;
    if (Typ = 2) and ((Length(Part) = 3) or ((Length(Part) > 3) and
      (Part[4] = '.'))) then
    begin
      // technically aux?.ext is permitted but it is not recommended
      Result := Z_BAD_NAME;
      Exit;
    end;
  end;
  // good
  if Wilds > 0 then
    Result := Z_WILD
  else
    Result := 0;
end;

function IsExtPath(const APath: string): Boolean;
begin
  Result := (Length(APath) > 3) and (APath[1] = '\') and (APath[2] = '\') and
    (APath[3] = '?') and (APath[4] = '\');
end;

function CleanPath(var PathOut: string; const PathIn: string;
  NoLead: Boolean): Integer;
var
  Back: Boolean;
  Ch: Char;
  Cnt: Integer;
  Components: Integer;
  EOC: Integer;
  I: Integer;
  Part: string;
  Prev: Integer;
  Psn: Integer;
  Root: Integer;
  Wilds: Integer;
begin
  PathOut := '';
  Result := 0;
  if PathIn = '' then
  begin
    Result := Z_EMPTY;
    Exit;
  end;
  Psn := 1;
  if (Length(PathIn) >= 2) and (PathIn[2] = ':') and
    CharInSet(PathIn[1], ['A' .. 'Z', 'a' .. 'z']) then
  begin
    Psn := 3;
    PathOut := Copy(PathIn, 1, 2);
  end
  else
    if (Length(PathIn) >= 2) and (((PathIn[1] = '\') and (PathIn[2] = '\')) or
      ((PathIn[1] = '/') and (PathIn[2] = '/'))) then
    begin
      if IsExtPath(PathIn) then
      begin
        PathOut := PathIn;
        Exit; // do not parse
      end;
      // UNC  \\server\share
      Ch := PathIn[1];
      Cnt := 0;
      Psn := 3;
      while Psn <= Length(PathIn) do
      begin
        if PathIn[Psn] = Ch then
        begin
          Inc(Cnt);
          if (Length(PathIn) = Psn) or not CharInSet(PathIn[Psn + 1],
            ['A' .. 'Z', 'a' .. 'z']) then
          begin
            Result := Z_BAD_UNC;
            Exit;
          end;
          if Cnt = 2 then
            Break;
          Inc(Psn);
        end;
        Inc(Psn);
      end;
      if Cnt <> 2 then
      begin
        Result := Z_BAD_UNC;
        Exit;
      end;
      for I := 1 to Psn - 1 do
      begin
        Ch := PathIn[I];
        if Ch = '/' then
          Ch := '\';
        PathOut := PathOut + Ch; // copy correcting seps
      end;
    end;
  // Psn => first char past drive
  Ch := PathIn[Psn];
  if Ch = '/' then
    Ch := '\';
  if Ch = '\' then
  begin
    PathOut := PathOut + Ch;
    Inc(Psn);
  end;
  Wilds := 0;
  Root := Length(PathOut);
  Components := 0;
  while Psn <= Length(PathIn) do
  begin
    EOC := NextDelim(PathIn, Psn, ['/', '\']);
    if EOC > 0 then
    begin
      Part := Copy(PathIn, Psn, (EOC - Psn));
      Psn := EOC + 1; // next component
    end
    else
    begin
      Part := Copy(PathIn, Psn, MAXINT); // rest of string
      Psn := Length(PathIn) + 1; // past end
    end;
    Result := CheckComponent(Part);
    if (Result = Z_IS_THIS) and (Components = 0) and not NoLead then
      Result := 0;
    if Result < 0 then
    begin
      if Result > Z_BAD_PARENT then
        Break; // bad
      // keep leading dot & ddot
      if Components > 0 then
      begin
        if Result = Z_IS_THIS then
        begin
          Result := 0; // 15/04/2013 4:07:50 PM
          Continue; // ignore
        end;
        // is '..\' _ backup
        Prev := LastPos(PathOut, '\', Length(PathOut) - 2);
        if (Prev < 1) and ((Root = 0) or (PathOut[Root] = '\')) then
          Prev := Root;
        if Prev < Root then
        begin
          Result := Z_BAD_PARENT;
          Break;
        end;
        Back := True;
        if PathOut[1] = '.' then
        begin
          // don't remove previous  ..\
          if Length(PathOut) = 2 then
          begin
            PathOut := ''; // truncate
            Dec(Components);
            Back := False; // .\
          end
          else
            if ((Length(PathOut) - Prev) = 3) and (PathOut[Prev + 1] = '.') and
              (PathOut[Prev + 2] = '.') then
              Back := False; // ..\
        end;
        if Back then
        begin
          PathOut := Copy(PathOut, 1, Prev); // truncate
          Dec(Components);
          Continue;
        end;
      end
      else
        if Root > 0 then
        begin
          Result := Z_BAD_PARENT;
          Break;
        end;
    end;
    if Result = Z_WILD then
      Inc(Wilds);
    PathOut := PathOut + Part;
    if EOC < 1 then
      Break; // end of PathIn
    PathOut := PathOut + '\';
    Inc(Components);
  end;
  if Result = 0 then
  begin
    if Wilds > 0 then
      Result := Z_WILD;
    if PathOut = '' then
      Result := Result or Z_EMPTY
    else
      if NoLead and (Root = 0) and (PathOut[1] = '.') then
        Result := Z_BAD_PARENT;
  end;
end;

function ExeSize(const Name: string): Cardinal;
var
  Fs: TFileStream;
begin
  Fs := TFileStream.Create(Name, FmOpenRead or FmShareDenyWrite);
  try
    Result := ExeSize(Fs);
  finally
    Fs.Free;
  end;
end;

function CheckSFXType(const Name: string; var ZipName: string;
  var Size: Integer): Integer;
var
  Fs: TFileStream;
begin
  Result := 0;
  if FileExists(Name) and (AnsiCompareText(ExtractFileExt(Name), '.exe') = 0)
  then
  begin
    Fs := TFileStream.Create(Name, FmOpenRead or FmShareDenyWrite);
    try
      ZipName := Name;
      Result := CheckSFXType(Fs, ZipName, Size);
    finally
      Fs.Free;
    end;
  end;
end;

function FileDateToLocalDateTime(Stamp: Integer): TDateTime;
var
  FTime: TFileTime;
  LocTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  if DosDateTimeToFileTime(LongRec(Stamp).Hi, LongRec(Stamp).Lo, LocTime) and
    LocalFileTimeToFileTime(LocTime, FTime) and
    FileTimeToSystemTime(FTime, SysTime) then
    Result := SystemTimeToDateTime(SysTime);
end;

function FileTimeToLocalDateTime(const Ft: TFileTime): TDateTime;
var
  LocalTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(Ft, LocalTime);
  FileTimeToSystemTime(LocalTime, SystemTime);
  Result := EncodeDate(SystemTime.WYear, SystemTime.WMonth, SystemTime.WDay) +
    EncodeTime(SystemTime.WHour, SystemTime.WMinute, SystemTime.WSecond,
    SystemTime.WMilliseconds);
end;

function FileTimeToLocalDOSTime(const Ft: TFileTime): Cardinal;
var
  Lf: TFileTime;
  Wd: Word;
  Wt: Word;
begin
  Result := 0;
  if FileTimeToLocalFileTime(Ft, Lf) and FileTimeToDosDateTime(Lf, Wd, Wt) then
    Result := (Wd shl 16) or Wt;
end;

function FileLastModified(const FileName: string;
  var LastMod: TDateTime): Boolean;
var
  FindData: TWin32FindData;
  H: THandle;
  SysTime: TSystemTime;
begin
  Result := False;
  H := {$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.FindFirstFile(PChar(FileName),
    FindData);
  if (INVALID_HANDLE_VALUE <> H) then
  begin
{$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.FindClose(H);

    Result := FileTimeToSystemTime(FindData.FtLastWriteTime, SysTime);
    if Result then
      LastMod := SystemTimeToDateTime(SysTime);
  end;
end;

// Return true if found
// if found return idx --> tag, size = tag + data
function XData(const X: TZMRawBytes; Tag: Word; var Idx, Size: Integer)
  : Boolean;
var
  I: Integer;
  L: Integer;
  Wsz: Word;
  Wtg: Word;
begin
  Result := False;
  Idx := 0;
  Size := 0;
  I := 1;
  L := Length(X);
  while I <= L - (2 * SizeOf(Word)) do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if Wtg = Tag then
    begin
      Result := (I + Wsz + (2 * SizeOf(Word))) <= L + 1;
      if Result then
      begin
        Idx := I;
        Size := Wsz + (2 * SizeOf(Word));
      end;
      Break;
    end;
    I := I + Wsz + (2 * SizeOf(Word));
  end;
end;

function XData_HasTag(Tag: Integer; const Tags: array of Integer): Boolean;
var
  Ii: Integer;
begin
  Result := False;
  for Ii := 0 to high(Tags) do
    if Tags[Ii] = Tag then
    begin
      Result := True;
      Break;
    end;
end;

function XDataAppend(var X: TZMRawBytes; const Src1; Siz1: Integer; const Src2;
  Siz2: Integer): Integer;
var
  Newlen: Integer;
begin
  Result := Length(X);
  if (Siz1 < 0) or (Siz2 < 0) then
    Exit;
  Newlen := Result + Siz1 + Siz2;
  SetLength(X, Newlen);
  Move(Src1, X[Result + 1], Siz1);
  Result := Result + Siz1;
  if Siz2 > 0 then
  begin
    Move(Src2, X[Result + 1], Siz2);
    Result := Result + Siz2;
  end;
end;

function XDataKeep(const X: TZMRawBytes; const Tags: array of Integer)
  : TZMRawBytes;
var
  Di: Integer;
  I: Integer;
  L: Integer;
  Siz: Integer;
  Wsz: Word;
  Wtg: Word;
begin
  Result := '';
  Siz := 0;
  L := Length(X);
  if L < 4 then
    Exit; // invalid
  I := 1;
  while I <= L - 4 do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if (XData_HasTag(Wtg, Tags)) and ((I + Wsz + 4) <= L + 1) then
    begin
      Inc(Siz, Wsz + 4);
    end;
    I := I + Wsz + 4;
  end;
  SetLength(Result, Siz);
  Di := 1;
  I := 1;
  while I <= L - 4 do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if (XData_HasTag(Wtg, Tags)) and ((I + Wsz + 4) <= L + 1) then
    begin
      Wsz := Wsz + 4;
      while Wsz > 0 do
      begin
        Result[Di] := X[I];
        Inc(Di);
        Inc(I);
        Dec(Wsz);
      end;
    end
    else
      I := I + Wsz + 4;
  end;
end;

function XDataPrepend(var X: TZMRawBytes; const Src; Siz: Integer): Integer;
var
  Newx: TZMRawBytes;
begin
  Result := Length(X);
  if Siz < 0 then
    Exit;
  SetLength(Newx, Siz);
  Move(Src, Newx[1], Siz);
  X := Newx + X;
  Result := Length(X);
end;

function XDataRemove(const X: TZMRawBytes; const Tags: array of Integer)
  : TZMRawBytes;
var
  Di: Integer;
  I: Integer;
  L: Integer;
  Siz: Integer;
  Wsz: Word;
  Wtg: Word;
begin
  Result := '';
  Siz := 0;
  L := Length(X);
  if L < 4 then
    Exit; // invalid
  I := 1;
  while I <= L - 4 do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if (not XData_HasTag(Wtg, Tags)) and ((I + Wsz + 4) <= L + 1) then
    begin
      Inc(Siz, Wsz + 4);
    end;
    I := I + Wsz + 4;
  end;
  SetLength(Result, Siz);
  Di := 1;
  I := 1;
  while I <= L - 4 do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if (not XData_HasTag(Wtg, Tags)) and ((I + Wsz + 4) <= L + 1) then
    begin
      Wsz := Wsz + 4;
      while Wsz > 0 do
      begin
        Result[Di] := X[I];
        Inc(Di);
        Inc(I);
        Dec(Wsz);
      end;
    end
    else
      I := I + Wsz + 4;
  end;
end;

// P. J. Weinberger Hash function
function HashFunc(const Str: string): Cardinal;
var
  I: Cardinal;
  X: Cardinal;
begin
  Result := 0;
  for I := 1 to Length(Str) do
  begin
    Result := (Result shl 4) + Ord(Str[I]);
    X := Result and $F0000000;
    if (X <> 0) then
      Result := (Result xor (X shr 24)) and $0FFFFFFF;
  end;
end;

function HashFuncNoCase(const Str: string): Cardinal;
var
  I: Cardinal;
  V: Cardinal;
  X: Cardinal;
begin
  Result := 0;
  for I := 1 to Length(Str) do
  begin
    V := Ord(Str[I]);
    if (V >= Ord('a')) and (V <= Ord('z')) then
      V := V - $20;
    Result := (Result shl 4) + V;
    X := Result and $F0000000;
    if (X <> 0) then
      Result := (Result xor (X shr 24)) and $0FFFFFFF;
  end;
end;

procedure SplitArgs(const Args: string; var Main: string; var Filearg: string;
  var Switches: string; var Password: string);
var
  EPosn: Integer;
  FPosn: Integer;
  Ppos: Integer;
  Spos: Integer;
begin
  FPosn := Pos(ZFILE_SEPARATOR, Args);
  Spos := Pos(' /', Args);
  Ppos := Pos(ZPASSWORDARG, Args);
  EPosn := Length(Args);
  if Ppos > 0 then
  begin
    Password := Copy(Args, Ppos, 2048);
    EPosn := Ppos - 1;
    if Spos > Ppos then
      Spos := Ppos;
  end
  else
    Password := '';
  if Spos > 0 then
  begin
    Switches := Trim(Copy(Args, Spos + 1, (EPosn - Spos) - 2));
    EPosn := Spos - 1;
    if FPosn > Spos then
      FPosn := Spos;
  end
  else
    Switches := '';
  if FPosn > 0 then
  begin
    Filearg := Trim(Copy(Args, FPosn + 2, (EPosn - FPosn) - 1));
    EPosn := FPosn - 1;
  end
  else
    Filearg := '';
  Main := Trim(Copy(Args, 1, EPosn));
end;

function QualifiedName(const ZipName: string; const FileName: string): string;
begin
  Result := ZipName + ZFILE_SEPARATOR + FileName;
end;

function RPos(const SubStr, AString: string; StartPos: Integer)
  : Integer; overload;
var
  FirstCh: Char;
  PStr: PChar;
  PSub: PChar;
begin
  Result := 0;
  if (SubStr = '') or (Length(SubStr) > Length(AString)) or (StartPos < 1) then
    Exit;

  if StartPos >= Length(AString) then
    StartPos := Length(AString) - Length(SubStr) + 1;
  FirstCh := SubStr[1];

  PSub := PChar(SubStr);
  Inc(PSub); // already tested
  PStr := @(AString[StartPos]);
  while StartPos > 0 do
  begin
    if FirstCh = PStr^ then
    begin
      if CompareMem(PSub, (PStr + 1), (Length(SubStr) - 1) * SizeOf(Char)) then
      begin
        Result := StartPos;
        EXIT;
      end;
    end;
    Dec(PStr);
    Dec(StartPos);
  end;
end;

function RPos(Delim: Char; const AString: string): Integer; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(AString) downto 1 do
    if AString[I] = Delim then
    begin
      Result := I;
      Break;
    end;
end;

function RPos(const SubStr, AString: string): Integer; overload;
begin
  if Length(SubStr) = 1 then
    Result := RPos(SubStr[1], AString)
  else
    Result := RPos(SubStr, AString, Length(AString) - Length(SubStr) + 1);
end;

function ZSplitString(const Delim, Raw: string; var Rest: string): string;
var
  EPosn: Integer;
  FPosn: Integer;
  Sin: string;
begin
  FPosn := Pos(Delim, Raw);
  EPosn := Length(Raw);
  Sin := Raw;
  if FPosn > 0 then
  begin
    Rest := Copy(Raw, FPosn + Length(Delim), EPosn);
    EPosn := FPosn - 1;
  end
  else
    Rest := '';
  Result := Copy(Sin, 1, EPosn);
end;

// returns part before last delimiter, rest = part after delimiter
function ZSplitStringLast(const Delim, Raw: string; var Rest: string): string;
var
  FPosn: Integer;
begin
  FPosn := RPos(Delim, Raw);
  if FPosn > 0 then
  begin
    Result := Copy(Raw, 1, FPosn - 1);
    Rest := Copy(Raw, Length(Delim) + FPosn, MAXINT);
  end
  else
  begin
    Result := Raw;
    Rest := '';
  end;
end;

procedure SplitQualifiedName(const QName: string; var ZipName: string;
  var FileName: string);
begin
  ZipName := Trim(ZSplitStringLast(ZFILE_SEPARATOR, QName, FileName));
  FileName := Trim(FileName);
end;

// return exe size (if < 4G)
// 0 _ not exe
function ExeSize(AStream: TStream): Cardinal;
var
  Bad: Boolean;
  Did: Integer;
  DosHeader: TImageDOSHeader;
  FileHeader: TImageFileHeader;
  I: Integer;
  NumSections: Integer;
  SectionEnd: Cardinal;
  SectionHeader: TImageSectionHeader;
  Sig: DWORD;
const
  IMAGE_PE_SIGNATURE = $00004550;
  IMAGE_DOS_SIGNATURE = $5A4D;
  IMAGE_FILE_MACHINE_I386 = $14C;
begin
  Result := 0;
  Bad := True;
  try
    AStream.Position := 0;
    while True do
    begin
      Did := AStream.Read(DosHeader, Sizeof(TImageDOSHeader));
      if (Did <> Sizeof(TImageDOSHeader)) or
        (DosHeader.E_magic <> IMAGE_DOS_SIGNATURE) then
        Break;
      if AStream.Seek(DosHeader._lfanew, 0) < 0 then
        Break;
      Did := AStream.Read(Sig, Sizeof(DWORD));
      if (Did <> Sizeof(DWORD)) or (Sig <> IMAGE_PE_SIGNATURE) then
        Break;
      Did := AStream.Read(FileHeader, Sizeof(TImageFileHeader));
      if (Did <> Sizeof(TImageFileHeader)) or
        (FileHeader.Machine <> IMAGE_FILE_MACHINE_I386) then
        Break;
      NumSections := FileHeader.NumberOfSections;
      if AStream.Seek(Sizeof(TImageOptionalHeader), 1) < 0 then
        Break;
      Bad := False;
      for I := 1 to NumSections do
      begin
        Did := AStream.Read(SectionHeader, Sizeof(TImageSectionHeader));
        if (Did <> Sizeof(TImageSectionHeader)) then
        begin
          Bad := True;
          Break;
        end;
        SectionEnd := SectionHeader.PointerToRawData +
          SectionHeader.SizeOfRawData;
        if SectionEnd > Result then
          Result := SectionEnd;
      end;
    end;
  except
    Bad := True;
  end;
  if Bad then
    Result := 0;
end;

// return <0 error
// const
// cstNone = 0;      // not found
// cstExe  = 1;      // might be stub of unknown type
// cstSFX17 = 2;     // found 1.7 SFX headers
// cstSFX19 = 4;     // found 2.0 SFX headers
// cstDetached = 64; // is detached
// -7  = Open, read or seek error
// -8  = memory error
// -9  = exception error
// -10 = all other exceptions
// check for SFX header or detached header
function CheckSFXType(AStream: TStream; var ZipName: string;
  var Size: Integer): Integer;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    X: Word;
  end;
var
  Detached: TSFXDetachedHeader_17;
  Hed: T_header;
  Nsize: Integer;
  SFXHeader_end: TSFXFileEndOfHeader_17;
  Tmp: Ansistring;
begin
  Result := 0; // default none
  try
    Size := ExeSize(AStream);
    if Size > 0 then
    begin
      ZipName := ExtractNameOfFile(ZipName) + '.zip'; // use default
      while Result = 0 do
      begin
        Result := -7; // error - maybe read error?
        if AStream.Seek(Size, SoFromBeginning) <> Size then
          Break;
        // at end of stub - read file header
        if AStream.Read(Hed, Sizeof(T_header)) <> Sizeof(T_header) then
          Break;
        // valid?
        case Hed.Sig of
          SFX_HEADER_SIG:
            begin
              // it is new header
              Size := Size + Sizeof(T_header);
              // skip file header
              Nsize := Hed.Size - SizeOf(T_header);
              if AStream.Seek(Nsize, SoFromCurrent) < 0 then
                Break; // error
              // at end of stub - read file header
              if AStream.Read(Hed, Sizeof(T_header)) <> Sizeof(T_header) then
                Break; // invalid
              Size := Size + Nsize;
              if Hed.Sig = CentralFileHeaderSig then
                Result := CstSFX19 or CstDetached // found new detached
              else
                if Hed.Sig = LocalFileHeaderSig then
                  Result := CstSFX19; // found new
            end;
          SFX_HEADER_SIG_17:
            begin
              // is old header
              Size := Size + Sizeof(T_header);
              // skip file header
              Nsize := Hed.Size - SizeOf(T_header);
              if AStream.Seek(Nsize, SoFromCurrent) < 0 then
                Break; // error
              if AStream.Read(SFXHeader_end, Sizeof(SFXHeader_end)) <>
                Sizeof(SFXHeader_end) then
                Break; // invalid
              if (SFXHeader_end.Signature <> SFX_HEADER_END_SIG_17) then
                Break; // invalid
              // ignore header size check
              Size := Size + Nsize + Sizeof(SFXHeader_end);
              // at end of file header - check for detached header
              if AStream.Read(Detached, Sizeof(TSFXDetachedHeader_17)) <>
                Sizeof(TSFXDetachedHeader_17) then
                Break; // not detached
              if Detached.Signature = SFX_DETACHED_HEADER_SIG_17 then
              begin
                Size := Size + Sizeof(TSFXDetachedHeader_17);
                if Detached.NameLen > 0 then
                begin
                  SetLength(Tmp, Detached.NameLen);
                  if AStream.Read(PAnsiChar(Tmp)^, Detached.NameLen) <>
                    Integer(Detached.NameLen) then
                    Break; // invalid
                  ZipName := string(Tmp) + ExtractFileExt(ZipName);
                  Size := Size + Integer(Detached.NameLen);
                end;
                if Detached.ExtLen > 0 then
                begin
                  SetLength(Tmp, Detached.ExtLen);
                  if AStream.Read(PAnsiChar(Tmp)^, Detached.ExtLen) <>
                    Integer(Detached.ExtLen) then
                    Break; // invalid
                  Size := Size + Integer(Detached.ExtLen);
                  ZipName := ExtractNameOfFile(ZipName) + '.' + string(Tmp);
                end;
                // at end of file header - check for detached header end
                if (AStream.Read(Detached, Sizeof(TSFXDetachedHeader_17)) <>
                  Sizeof(TSFXDetachedHeader_17)) or
                  (Detached.Signature <> SFX_DETACHED_HEADER_END_SIG_17) then
                  Break; // invalid
                Size := Size + Sizeof(TSFXDetachedHeader_17);
                if AStream.Read(Hed, Sizeof(DWORD)) <> Sizeof(DWORD) then
                  Break; // invalid
                if Hed.Sig = CentralFileHeaderSig then
                  Result := CstSFX17 or CstDetached; // found old detached
              end;
              if Detached.Signature = LocalFileHeaderSig then
                Result := CstSFX17; // found old
            end;
        else
          begin
            Result := CstExe; // possibly stub of different loader
          end;
        end;
      end;
    end;
  except
    Result := -10;
  end;
end;

function DriveLetter(const Path: string): Char;
var
  S: string;
begin
  Result := #0;
  S := Uppercase(ExtractFileDrive(ExpandUNCFileName(Path)) + '\');
  if (Length(S) >= 3) and (S[2] = ':') then
    Result := S[1];
end;

// trim and dequote if needed
function Unquote(const FName: string): string;
var
  P: PChar;
begin
  Result := Trim(FName);
  if (Result <> '') and (Result[1] = '"') then
  begin
    P := PChar(Result);
    Result := AnsiExtractQuotedStr(P, '"');
  end;
end;

function BoolStr(const Value: Boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function FormTempName(const Where: string): string;
var
  I: Integer;
  Posn: Integer;
  S: string;
  Uid: TGuid;
begin
  S := '???';
{$IFDEF VERD7up}
  if CreateGUID(Uid) = S_OK then
    S := GUIDToString(Uid);
{$ELSE}
  if CoCreateGUID(Uid) = S_OK then
    S := GUIDToString(Uid);
{$ENDIF}
  S := Copy(S, 2, Length(S) - 2);
  for I := 1 to Length(S) do
    if S[I] = '-' then
      S[I] := '%';
  Posn := Pos('*', Where);
  if Posn < 1 then
    Result := Where + S
  else
  begin
    Result := Copy(Where, 1, Posn - 1) + S;
    Result := Result + Copy(Where, Posn + 1, Length(Where) - 1);
  end;
  if ExtractFileExt(Where) = '' then
    Result := Result + '.tmp';
end;

function HasSpanSig(const FName: string): Boolean;
var
  Fs: TFileStream;
  Sg: Cardinal;
begin
  Result := False;
  if FileExists(FName) then
  begin
    Fs := TFileStream.Create(FName, FmOpenRead or FmShareDenyWrite);
    try
      if (Fs.Size > (Sizeof(TZipLocalHeader) + Sizeof(Sg))) and
        (Fs.Read(Sg, Sizeof(Sg)) = Sizeof(Sg)) then
        Result := (Sg = ExtLocalSig) and (Fs.Read(Sg, Sizeof(Sg)) = Sizeof(Sg))
          and (Sg = LocalFileHeaderSig);
    finally
      Fs.Free;
    end;
  end;
end;

// test for invalid characters
function IsInvalidIntName(const FName: string): Boolean;
var
  C: Char;
  Clen: Integer;
  I: Integer;
  Len: Integer;
  N: Char;
  P: Char;
begin
  Result := True;
  Len := Length(FName);
  if (Len < 1) or (Len >= MAX_PATH) then
    Exit; // empty or too long
  C := FName[1];
  if (C = PathDelim) or (C = '.') or (C = ' ') then
    Exit; // invalid from root or below
  I := 1;
  Clen := 0;
  P := #0;
  while I <= Len do
  begin
    Inc(Clen);
    if Clen > 255 then
      Exit; // component too long
    C := FName[I];
    if I < Len then
      N := FName[I + 1]
    else
      N := #0;
    case C of
      WILD_MULTI, DriveDelim, WILD_CHAR, '<', '>', '|', #0:
        Exit;
      #1 .. #31:
        Exit; // invalid
      PathDelimAlt:
        begin
          if P = ' ' then
            Exit; // bad - component has Trailing space
          if (N = C) or (N = '.') or (N = ' ') then
            Exit; // \\ . leading space invalid
          Clen := 0;
        end;
      '.':
        begin
          N := FName[Succ(I)];
          if (N = PathDelim) or (N < ' ') then
            Exit;
        end;
      ' ':
        if I = Len then
          Exit; // invalid
    end;
    P := C;
    Inc(I);
  end;
  Result := False;
end;

function AppendToName(const Name, Suffix: string): string;
var
  DotPosn: Integer;
begin
  Result := Name;
  if Suffix = '' then
    Exit;
  if LastChar(Name) = '\' then
  begin
    Result := Copy(Name, 1, Length(Name) - 1) + Suffix + '\';
    Exit;
  end;
  DotPosn := LastPos(Name, '.');
  if DotPosn > 1 then
  begin
    Result := Copy(Name, 1, DotPosn - 1) + Suffix +
      Copy(Name, DotPosn, 1 + Length(Name) - DotPosn);
  end
  else
    Result := Name + Suffix;
end;

function IntToBase36(Utim: Cardinal): string;
var
  Ch: Char;
  U1: Integer;
begin
  Result := '';
  while Utim > 0 do
  begin
    U1 := Utim mod 36;
    Utim := Utim div 36;
    if U1 > 9 then
      Ch := Char(Ord('a') + U1 - 10)
    else
      Ch := Char(Ord('0') + U1);
    Result := Ch + Result;
  end;
end;

function SysErrorMsg(ErrNo: Cardinal = Cardinal(-1)): string;
const
  SysErrMsg = ' [0x%x] %s';
begin
  Result := '';
  if ErrNo = Cardinal(-1) then
    ErrNo := GetLastError;
  Result := Format(SysErrMsg, [ErrNo, SysErrorMessage(ErrNo)]);
end;

function ShortPathName(const APath: string): string;
var
  ShortName: string;
  ShortNameLen: Integer;
begin
  Result := '';
  if Length(APath) <= MAX_PATH then
  begin
    SetLength(ShortName, MAX_PATH + 1);
    ShortNameLen := GetShortPathName(PChar(APath), PChar(ShortName), MAX_PATH);
    if (ShortNameLen > 0) then
    begin
      SetLength(ShortName, ShortNameLen);
      Result := ShortName;
    end;
  end
  else
    Result := ShortenPath(APath);
end;

function ShortenPath(const APath: string): string;
var
  Path: string;
  Remainder: string;
  Rest: string;
begin
  Result := '';
  Path := '';
  Rest := APath;
  // get first part
  Remainder := APath;
  repeat
    if Path <> '' then
      Path := Path + '\';
    Path := Path + ZSplitString('\', Rest, Rest);
    Path := ShortPathName(Path);
    if Path = '' then
      Break;
    Result := Path;
    Remainder := Rest;
  until Rest = '';
  // Remainder is any unfound folders and/or file
  if Remainder <> '' then
  begin
    if Result = '' then
      Result := Remainder
    else
      Result := Result + '\' + Remainder;
  end;
  // restore trailing delimiter if existed
  if (APath <> '') and (APath[Length(APath)] = '\') and
    (Result[Length(Result)] <> '\') then
    Result := Result + '\';
  if Length(Result) > MAX_PATH then
    Result := '';
end;

end.
