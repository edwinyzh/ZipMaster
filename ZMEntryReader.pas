unit ZMEntryReader;

// ZMEntryReader.pas - Represents the 'Directory entry' read from a Zip file

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
// modified 2013-12-06

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VER180}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMZipDirectory, ZMStructs;

type
  TZMEntryReader = class(TZMEntryBase)
  private
    FFileName: string;
    FVariables: TZMRawBytes;
    Header: TZipCentralHeader;
    _CSize: Int64;
    _DiskStt: Cardinal;
    _RelOfs: Int64;
    _USize: Int64;
    function ClearStatusBit(const Values: Cardinal): Cardinal;
    function FixXData64: Integer;
    function GetDataString(Cmnt: Boolean): UTF8String;
    function Int2UTF(Field: TZMRecStrings; NoUD: Boolean = False): string;
    function StrHasExt_(IsName: Boolean): Boolean;
    function _XData(Tag: Word; var SttP: PByte; var Size: Integer): Boolean;
  protected
    function FindDataTag(Tag: Word; var Idx, Siz: Integer): Boolean;
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetEncoded: TZMEncodingOpts; override;
    function GetExtFileAttrib: Longword; override;
    function GetExtraData(Tag: Word): TZMRawBytes; override;
    function GetExtraField: TZMRawBytes; override;
    function GetExtraFieldLength: Word; override;
    function GetFileComment: string; override;
    function GetFileCommentLen: Word; override;
    function GetFileName: string; override;
    function GetFileNameLen: Word; override;
    function GetFlag: Word; override;
    function GetIntFileAttrib: Word; override;
    function GetModifDateTime: Longword; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
    function Get_CompressedSize: Cardinal; override;
    function Get_FileComment: TZMRawBytes; override;
    function Get_FileName: TZMRawBytes; override;
    function Get_RelOffLocalHdr: Cardinal; override;
    function Get_StartOnDisk: Word; override;
    function Get_UncompressedSize: Cardinal; override;
  public
    procedure ClearCachedName; override;
    function FetchNTFSTimes(var Times: TNTFS_Times): Integer;
    function Read: Integer;
    property IsEncoded: TZMEncodingOpts read GetIsEncoded write SetIsEncoded;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  ZMMsg, ZMZipEOC, ZMUtils, ZMUTF8, ZMBody,  ZMCRC;

const
  __UNIT__ = 16;

const
  MAX_BYTE = 255;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

function _XDataP(const Xdat: PByte; Len, Tag: Word; var Data: PByte;
  var Size: Integer): Boolean;
var
  I: Integer;
  P: PAnsiChar;
  Wsz: Word;
  Wtg: Word;
begin
  Result := False;
  Data := nil;
  Size := 0;
  I := 1;
  P := PAnsiChar(Xdat);
  while I < Len - (2 * SizeOf(Word)) do
  begin
    Wtg := PWord(P)^;
    Wsz := PWord(P + 2)^;
    if Wtg = Tag then
    begin
      Result := (I + Wsz + (2 * SizeOf(Word))) <= Len + 1;
      if Result then
      begin
        Data := PByte(P);
        Size := Wsz;
      end;
      Break;
    end;
    I := I + Wsz + (2 * SizeOf(Word));
    Inc(P, Wsz + (2 * SizeOf(Word)));
  end;
end;

// make safe version of external comment
function SafeComment(const Xcomment: string): string;
var
  C: Char;
  I: Integer;
begin
  if StrHasExt(Xcomment) then
    Result := StrToOEM(Xcomment)
  else
    Result := Xcomment;
  for I := 1 to Length(Result) do
  begin
    C := Result[I];
    if (C < ' ') or (C > #126) then
      Result[I] := '_';
  end;
end;

{ TZMEntryReader }
procedure TZMEntryReader.ClearCachedName;
begin
  FFileName := ''; // force reconvert - settings have changed
  ClearStatusBit(ZsbHashed);
  IsEncoded := ZeoAuto; // force re-evaluate
end;

function TZMEntryReader.ClearStatusBit(const Values: Cardinal): Cardinal;
begin
  Result := StatusBits and Values;
  StatusBits := StatusBits and not Values;
end;

function TZMEntryReader.FetchNTFSTimes(var Times: TNTFS_Times): Integer;
var
  Stag: PByte;
  Subtags: PByte;
  Sz: Integer;
  X: TZMRawBytes;
begin
  Result := 0;
  if ExtraFieldLength >= SizeOf(TNTFS_Header) then
  begin
    X := ExtraField;
    if X = '' then
    begin
      Result := ZM_Error({_LINE_}214, ZE_Zip64FieldError);
      Exit;
    end;
    if _XDataP(PByte(@X[1]), Length(X), NTFS_data_tag, Subtags, Sz) and
      (Sz >= (SizeOf(TNTFS_Times) + 4)) then
    begin
      Inc(Subtags, 8); // sizeof(hedr + reserved)
      // found NTFS data - find tag 1 (times)
      if _XDataP(Subtags, Sz - 4, 1, Stag, Sz) and (Sz = SizeOf(TNTFS_Times))
      then
      begin
        // found times
        Inc(Stag, 4); // sizeof(head)
        Move(Stag^, Times, SizeOf(TNTFS_Times));
        Result := 1;
      end;
    end;
  end;
end;

function TZMEntryReader.FindDataTag(Tag: Word; var Idx, Siz: Integer): Boolean;
begin
  Result := False;
  if XData(ExtraField, Tag, Idx, Siz) then
    Result := True;
end;

// 'fixes' the special Zip64  fields from extra data
// return <0 error, 0 none, 1 Zip64
function TZMEntryReader.FixXData64: Integer;
var
  P: PByte;
  Wsz: Integer;
begin
  Result := 0;
  if (VersionNeeded and VerMask) < ZIP64_VER then
    Exit;
  if not _XData(Zip64_data_tag, P, Wsz) then
    Exit;
  Inc(P, Sizeof(TExtra_Head)); // past header
  Dec(Wsz, Sizeof(TExtra_Head)); // discount header
  if Header.UncomprSize = MAX_UNSIGNED then
  begin
    if Wsz < SizeOf(Int64) then
    begin
      Result := ZM_Error({_LINE_}259, ZE_Zip64FieldError);
      Exit; // error
    end;
    _USize := PInt64(P)^;
    Inc(P, SizeOf(Int64));
    Dec(Wsz, SizeOf(Int64));
  end;
  if Header.ComprSize = MAX_UNSIGNED then
  begin
    if Wsz < SizeOf(Int64) then
    begin
      Result := ZM_Error({_LINE_}270, ZE_Zip64FieldError);
      Exit; // error
    end;
    _CSize := PInt64(P)^;
    Inc(P, SizeOf(Int64));
    Dec(Wsz, SizeOf(Int64));
  end;
  if Header.RelOffLocalHdr = MAX_UNSIGNED then
  begin
    if Wsz < SizeOf(Int64) then
    begin
      Result := ZM_Error({_LINE_}281, ZE_Zip64FieldError);
      Exit; // error
    end;
    _RelOfs := PInt64(P)^;
    Inc(P, SizeOf(Int64));
    Dec(Wsz, SizeOf(Int64));
  end;
  if Header.DiskStart = MAX_WORD then
  begin
    if Wsz < SizeOf(Cardinal) then
    begin
      Result := ZM_Error({_LINE_}292, ZE_Zip64FieldError);
      Exit; // error
    end;
    _DiskStt := PCardinal(P)^;
  end;
  Result := 1;
end;

// will return empty if not exists or invalid
function TZMEntryReader.GetCompressedSize: Int64;
begin
  if Header.ComprSize = MAX_UNSIGNED then
    Result := _CSize
  else
    Result := Header.ComprSize;
end;

function TZMEntryReader.GetCompressionMethod: Word;
begin
  Result := Header.ComprMethod;
end;

function TZMEntryReader.GetCRC32: Cardinal;
begin
  Result := Header.CRC32;
end;

function TZMEntryReader.GetDataString(Cmnt: Boolean): UTF8String;
var
  Crc: Cardinal;
  Field: TZMRawBytes;
  Idx: Integer;
  PH: PUString_Data_Header;
  PS: PAnsiChar;
  Siz: Integer;
  Tag: Word;
begin
  Result := '';
  if Cmnt then
  begin
    Tag := UCmnt_Data_Tag;
    Field := _FileComment;
    if Field = '' then
      Exit; // no point checking
  end
  else
  begin
    Tag := UPath_Data_Tag;
    Field := _FileName;
  end;
  if FindDataTag(Tag, Idx, Siz) then
  begin
    PS := @FVariables[Header.FileNameLen + Idx];
    PH := PUString_Data_Header(PS);
    if PH^.Version = 1 then
    begin
      Crc := ZCRC32(0, Field[1], Length(Field));
      if PH^.Origcrc = Crc then
      begin
        Siz := Siz - SizeOf(TUString_Data_Header);
        Inc(PS, SizeOf(TUString_Data_Header));
        if (Siz > 0) and (ValidUTF8(PS, Siz) >= 0) then
        begin
          SetLength(Result, Siz);
          Move(PS^, Result[1], Siz);
        end;
      end;
    end;
  end;
end;

function TZMEntryReader.GetDateTime: Cardinal;
begin
  Result := Header.ModifDateTime;
end;

{
 Encoded as OEM for
 DOS (default)                       FS_FAT
 OS/2                                FS_HPFS
 Win95/NT with Nico Mak's WinZip     FS_NTFS && MyFile = 5.0
 UTF8 is flag is set
 except (someone always has to be different)
 PKZIP (Win) 2.5, 2.6, 4.0 - mark as FS_FAT but local is Windows ANSI (1252)
 PKZIP (Unix) 2.51 - mark as FS_FAT but are current code page
}
function TZMEntryReader.GetEncoded: TZMEncodingOpts;
const
  WZIP = $0B32; // (FS_NTFS * 256) + 50;
  OS_HPFS = FS_HPFS * 256;
  OS_FAT = FS_FAT * 256;
begin
  Result := ZeoNone;

  if (Flag and FLAG_UTF8_BIT) <> 0 then
    Result := ZeoUTF8
  else
    if (GetDataString(False) <> '') or (GetDataString(True) <> '') then
      Result := ZeoUPath
    else
      if ((VersionMadeBy and OSMask) = OS_FAT) or
        ((VersionMadeBy and OSMask) = OS_HPFS) or (VersionMadeBy = WZIP) then
        Result := ZeoOEM;
end;

// returns the 'data' without the tag
function TZMEntryReader.GetExtFileAttrib: Longword;
begin
  Result := Header.ExtFileAtt;
end;

function TZMEntryReader.GetExtraData(Tag: Word): TZMRawBytes;
var
  I: Integer;
  Sz: Integer;
  X: TZMRawBytes;
begin
  Result := '';
  X := ExtraField;
  if XData(X, Tag, I, Sz) then
    Result := Copy(X, I + 4, Sz - 4);
end;

function TZMEntryReader.GetExtraField: TZMRawBytes;
begin
  Result := Copy(FVariables, Header.FileNameLen + 1, Header.ExtraLen);
end;

function TZMEntryReader.GetExtraFieldLength: Word;
begin
  Result := Header.ExtraLen;
end;

function TZMEntryReader.GetFileComment: string;
begin
  Result := Int2UTF(ZrsComment, False);
end;

function TZMEntryReader.GetFileCommentLen: Word;
begin
  Result := Header.FileComLen;
end;

// returns the external filename interpretting the internal name by Encoding
// still in internal form
function TZMEntryReader.GetFileName: string;
begin
  if FFileName = '' then
    FFileName := Int2UTF(ZrsName, False);
  Result := FFileName;
end;

function TZMEntryReader.GetFileNameLen: Word;
begin
  Result := Header.FileNameLen;
end;

function TZMEntryReader.GetFlag: Word;
begin
  Result := Header.Flag;
end;

function TZMEntryReader.GetIntFileAttrib: Word;
begin
  Result := Header.IntFileAtt;
end;

function TZMEntryReader.GetModifDateTime: Longword;
begin
  Result := Header.ModifDateTime;
end;

function TZMEntryReader.GetRelOffLocalHdr: Int64;
begin
  if Header.RelOffLocalHdr = MAX_UNSIGNED then
    Result := _RelOfs
  else
    Result := Header.RelOffLocalHdr;
end;

function TZMEntryReader.GetStartOnDisk: Cardinal;
begin
  if Header.DiskStart = MAX_WORD then
    Result := _DiskStt
  else
    Result := Header.DiskStart;
end;

function TZMEntryReader.GetUncompressedSize: Int64;
begin
  if Header.UncomprSize = MAX_UNSIGNED then
    Result := _USize
  else
    Result := Header.UncomprSize;
end;

function TZMEntryReader.GetVersionMadeBy: Word;
begin
  Result := Header.VersionMadeBy;
end;

function TZMEntryReader.GetVersionNeeded: Word;
begin
  Result := Header.VersionNeeded;
end;

function TZMEntryReader.Get_CompressedSize: Cardinal;
begin
  Result := Header.ComprSize;
end;

function TZMEntryReader.Get_FileComment: TZMRawBytes;
begin
  Result := Copy(FVariables, Header.FileNameLen + Header.ExtraLen + 1,
    Header.FileComLen);
end;

function TZMEntryReader.Get_FileName: TZMRawBytes;
begin
  Result := Copy(FVariables, 1, Header.FileNameLen);
end;

function TZMEntryReader.Get_RelOffLocalHdr: Cardinal;
begin
  Result := Header.RelOffLocalHdr;
end;

function TZMEntryReader.Get_StartOnDisk: Word;
begin
  Result := Header.DiskStart;
end;

function TZMEntryReader.Get_UncompressedSize: Cardinal;
begin
  Result := Header.UncomprSize;
end;

function TZMEntryReader.Int2UTF(Field: TZMRecStrings;
  NoUD: Boolean = False): string;
var
  Enc: TZMEncodingOpts;
  Fld: TZMRawBytes;
begin
  if Field = ZrsComment then
    Fld := _FileComment
  else
    Fld := _FileName;
  Result := '';
  Enc := Body.Encoding;
  if Enc = ZeoAuto then
  begin
    Enc := IsEncoded; // how entry is encoded
    if NoUD and (Enc = ZeoUPath) then
      Enc := ZeoOEM; // use header Field
  end;
  if (Enc = ZeoUPath) or StrHasExt(Fld) then
  begin
{$IFDEF UNICODE}
    case Enc of
      // use UTF8 extra data string if available
      ZeoUPath:
        Result := UTF8ToWide(GetDataString(Field = ZrsComment));
      ZeoNone: // treat as Ansi (from somewhere)
        Result := StrToUTFEx(Fld, Body.Encoding_CP, -1);
      ZeoUTF8: // treat Field as being UTF8
        Result := PUTF8ToWideStr(PAnsiChar(Fld), Length(Fld));
      ZeoOEM: // convert to OEM
        Result := StrToUTFEx(Fld, CP_OEMCP, -1);
    end;
{$ELSE}
    if Body.IsUtf8 then
    begin
      case Enc of
        // use UTF8 extra data string if available
        ZeoUPath:
          Result := GetDataString(Field = ZrsComment);
        ZeoNone: // treat as Ansi (from somewhere)
          Result := StrToUTFEx(Fld, Body.Encoding_CP, -1);
        ZeoUTF8: // treat Field as being UTF8
          Result := Fld;
        ZeoOEM: // convert to OEM
          Result := StrToUTFEx(Fld, CP_OEMCP, -1);
      end;
    end
    else
    begin
      case Enc of
        // use UTF8 extra data string if available
        ZeoUPath:
          Result := UTF8ToSafe(GetDataString(Field = ZrsComment), False);
        ZeoNone: // treat as Ansi (from somewhere)
          Result := StrToWideEx(Fld, Body.Encoding_CP, -1);
        // will be converted
        ZeoUTF8: // treat Field as being UTF8
          Result := UTF8ToSafe(Fld, False);
        ZeoOEM: // convert to OEM
          Result := StrToWideEx(Fld, CP_OEMCP, -1); // will be converted
      end;
    end;
{$ENDIF}
  end;
  if Length(Result) = 0 then
    Result := string(Fld); // better than nothing
  if Field = ZrsName then
    Result := SetSlash(Result, PsdExternal);
end;

(* ? TZMIRec.Read
  Reads directory entry
 returns
 >=0 = ok   (1 = Zip64)
 <0 = -error
*)
function TZMEntryReader.Read: Integer;
var
  R: Integer;
  V: Integer;
  Wf: TZMZipEOC;
begin
  Wf := MyFile;
  ASSERT(Assigned(Wf), 'no WorkFile');
  Result := -1;
  if not Wf.IsOpen then
    Exit;
  StatusBits := ZsbInvalid;
  R := Wf.Read(Header, SizeOf(TZipCentralHeader));
  if R <> SizeOf(TZipCentralHeader) then
  begin
    Result := ZM_Error({_LINE_}620, ZE_CEHBadRead);
    Exit;
  end;
  if Header.HeaderSig <> CentralFileHeaderSig then
  begin
    Result := ZM_Error({_LINE_}625, ZE_CEHWrongSig);
    Exit;
  end;
  // read variable length fields
  V := Header.FileNameLen + Header.ExtraLen + Header.FileComLen;
  SetLength(FVariables, V);
  R := Wf.Read(FVariables[1], V);
  if R <> V then
  begin
    Body.Inform('Error reading variable fields', {_LINE}634, __UNIT__);
    Result := ZM_Error({_LINE_}634, ZE_CEHBadRead);
//    Result := ZM_Error({_LINE_}634, ZE_CECommentLen);
//    if R < Header.FileNameLen then
//      Result := ZM_Error({_LINE_}636, ZE_CENameLen)
//    else
//      if R < (Header.FileNameLen + Header.ExtraLen) then
//        Result := ZM_Error({_LINE_}639, ZE_ReadZipError);
    Exit;
  end;
  ClearStatusBit(ZsbInvalid); // record is valid
  Status[ZsbExtName] := StrHasExt_(True);
  Status[ZsbExtCmnt] := StrHasExt_(False);
  if (Header.FileNameLen <> 0) and
    (FVariables[Header.FileNameLen] = PathDelimAlt) then
    SetStatusBit(ZsbDirOnly); // dir only entry
  Result := FixXData64;
end;

function TZMEntryReader.StrHasExt_(IsName: Boolean): Boolean;
var
  Ch: AnsiChar;
  Ends: Integer;
  Starts: Integer;
begin
  Result := False;
  Ends := GetFileNameLen;
  if IsName then
    Starts := 0
  else
  begin
    Starts := Ends;
    Ends := Starts + GetExtraFieldLength - 1;
  end;
  if Ends > Length(FVariables) then
    Ends := Length(FVariables);
  while Starts < Ends do
  begin
    Inc(Starts);
    Ch := FVariables[Starts];
    if (Ch < #32) or (Ch > #126) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// Return true if found
// if found return SttP --> tag, size = tag + data
function TZMEntryReader._XData(Tag: Word; var SttP: PByte;
  var Size: Integer): Boolean;
var
  I: Integer;
  L: Integer;
  Stt: PByte;
  TagP: PExtra_Head;
  Tmp: PByte;
  Wsz: Word;
begin
  Result := False;
  SttP := nil;
  Size := 0;
  I := Header.FileNameLen; // + 1;
  Stt := PByte(@(FVariables[1]));
  L := Header.ExtraLen + Header.FileNameLen;
  while I <= L - (Sizeof(TExtra_Head)) do
  begin
    Tmp := Stt;
    Inc(Tmp, I);
    TagP := PExtra_Head(Tmp);
    Wsz := TagP^.Size + Sizeof(TExtra_Head);
    if TagP^.Tag = Tag then
    begin
      Result := (I + Wsz) <= L + 1;
      if Result then
      begin
        SttP := Tmp;
        Size := Wsz;
      end;
      Break;
    end;
    I := I + Wsz;
  end;
end;

function IsOnlyDOS(const Hstr: TZMRawBytes): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Hstr) do
    if (Hstr[I] > #126) or (Hstr[I] < #32) then
    begin
      Result := False;
      Break;
    end;
end;

end.



