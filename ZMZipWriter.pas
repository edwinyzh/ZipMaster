unit ZMZipWriter;

// ZMZipWriter.pas - Handles basic writing of a Zip file

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
// modified 2013-12-05

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMZipDirectory, ZMZipReader, ZMZipBase, ZMStructs;

type
  TZipWrites = (ZwDefault, ZwSingle, ZwMultiple);

type
  THowToEnc = (HteOEM, HteAnsi, HteUTF8);

const
  ZsbRenamed = $80000; // write to different name
  ZsbVChanged = $100000; // variable length field details have changed
  // zsbExtName = $200000;  // filename has extended characters
  // zsbExtCmnt = $400000;  // filename has extended characters
  ZsbDuplicate = $800000; // kept duplicate
  ZsbResolved = $1000000; // duplicate resolved
  ZsbIsDup = $2000000; // duplicate added to list

  { .$DEFINE DEBUG_LISTS }
type
  TZMEntryWriter = class;

  TZMZipWriter = class(TZMZipReader)
  private
    ExtEntries: TZMEntryBase;
    FDuplicates: TList;
    FEntryCount: Integer;
    FFirst: Integer;
    FNewEntries: TZMEntryWriter;
    FNewEntryCount: Integer;
    FNewEntryTail: TZMEntryWriter;
    FNoAppend: Boolean;
    FOpenRet: Integer;
    FOpenRet1: Integer;
    FShowAll: Boolean;
    FToDoList: TList;
    RefTime: string;
    WBuf: array of Byte;
    function CommitEntries: Int64;
    function CommitPreamble: Integer;
    function CommitSum(var TotalProcess: Int64; var Latest: Cardinal;
      MarkLatest: Boolean): Cardinal;
    function DefaultDupAction: TZMDupResolutions;
    function MakeDupedName(const Name: string; TryIndex: Integer): string;
    function ResolveDuplicates: Integer;
    function StrToHeader(const AString: string; How: THowToEnc): TZMRawBytes;
    function StrTo_UTF8(const AString: string): UTF8String;
  protected
    procedure AddExtEntry(Entry: TZMEntryWriter);
    procedure AppendNewEntry(AnEntry: TZMEntryWriter);
    function BeforeCommit: Integer; virtual;
    function CalcSizes(var NoEntries: Integer; var ToProcess: Int64;
      var CenSize: Cardinal): Integer;
    function CommitCentral: Int64;
    function CommitRec(Rec: TZMEntryWriter): Int64; virtual;
    function EOCSize(Is64: Boolean): Cardinal;
    function FixHeaderNames: Integer; virtual;
    function FixVariableFields(Entry: TZMEntryWriter; DupCnt: Integer): Integer;
    procedure HailSuccesses;
    procedure MarkDirty;
    function Preprocess: Integer;
    procedure RemoveExtEntry(Entry: TZMEntryBase);
    function ResolveDupEntry(Entry, Dup: TZMEntryWriter; var NewName: string)
      : TZMDupResolutions;
    function SafeHeaderName(const IntName: string): string;
    function UpdateCentralEntries: Integer;
    function WriteCentral: Integer;
    property EntryCount: Integer read FEntryCount write FEntryCount;
    property NoAppend: Boolean read FNoAppend write FNoAppend;
  public
    function AffixEntry(AnEntry: TZMEntryWriter; NoDup: Boolean): Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Commit(MarkLatest: Boolean): Integer;
    function CommitAppend(Last: Integer; MarkLatest: Boolean): Integer;
    function FindDuplicates: Integer;
    function PrepareWrite(Typ: TZipWrites): Boolean;
    function WBuffer(Size: Integer): PByte;
    property Duplicates: TList read FDuplicates;
    property First: Integer read FFirst;
    property OpenRet: Integer read FOpenRet write FOpenRet;
    property OpenRet1: Integer read FOpenRet1 write FOpenRet1;
    property ShowAll: Boolean read FShowAll write FShowAll;
    property ToDoList: TList read FToDoList;
  end;

  TZMEntryWriter = class(TZMEntryBase)
  private
    FAuxInfo: TObject;
    FCrypt: Boolean;
    FExtraField: TZMRawBytes;
    FFileComment: string;
    FFileName: string;
    FLink: TZMEntryBase;
    FLocalData: TZMRawBytes;
    FMyExtraInfo: TObject;
    FStartOnDisk: Cardinal;
    FTempVar: Integer;
    FVariables: TZMRawBytes;
    F_FileComment: TZMRawBytes;
    F_FileName: TZMRawBytes;
    Header: TZipCentralHeader;
    _CSize: Int64;
    _DiskStt: Cardinal;
    _RelOfs: Int64;
    _USize: Int64;
    procedure SetCompressedSize(const Value: Int64);
    procedure SetCompressionMethod(const Value: Word);
    procedure SetCRC32(const Value: Cardinal);
    procedure SetDateTime(const Value: Cardinal);
    procedure SetEncrypted(const Value: Boolean);
    procedure SetExtFileAttrib(const Value: Longword);
    procedure SetExtraData(Tag: Word; const Data: TZMRawBytes);
    procedure SetExtraField(const Value: TZMRawBytes);
    procedure SetFileComment(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetFlag(const Value: Word);
    procedure SetIntFileAttrib(const Value: Word);
    procedure SetModifDateTime(const Value: Longword);
    procedure SetRelOffLocalHdr(const Value: Int64);
    procedure SetStartOnDisk(const Value: Cardinal);
    procedure SetUncompressedSize(const Value: Int64);
    procedure SetVersionMadeBy(const Value: Word);
    procedure SetVersionNeeded(const Value: Word);
    function StripDrive(const FName: string; NoPath: Boolean): string;
  protected
    procedure FixMinimumVers(IsZ64: Boolean);
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetEncrypted: Boolean; override;
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
    function GetIsEncoded: TZMEncodingOpts; override;
    function GetModifDateTime: Longword; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Cardinal; override;
    function GetTitle: string; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
    function Get_CompressedSize: Cardinal; override;
    function Get_FileComment: TZMRawBytes; override;
    function Get_FileName: TZMRawBytes; override;
    function Get_RelOffLocalHdr: Cardinal; override;
    function Get_StartOnDisk: Word; override;
    function Get_UncompressedSize: Cardinal; override;
    procedure PrepareLocalData;
    // 1 returns bytes written, <0 _ error
    function Process: Int64; virtual; abstract;
    function ProcessSize: Int64; virtual; abstract;
    function WriteAsLocal1(Stamp, Crc: Cardinal): Integer;
    function WriteDataDesc: Integer;
    // 1 Auxillary information not owned by record
    property AuxInfo: TObject read FAuxInfo write FAuxInfo;
    property LocalData: TZMRawBytes read FLocalData write FLocalData;
    // 1 Extra info owned by record
    property MyExtraInfo: TObject read FMyExtraInfo write FMyExtraInfo;
  public
    constructor Create(TheOwner: TZMZipWriter);
    procedure AssignFrom(const ZRec: TZMEntryBase); override;
    procedure BeforeDestruction; override;
    function ChangeName(const NewName: string): Integer;
    procedure ClearCachedName; override;
    function LocalSize: Cardinal;
    function ToIntForm(const ExtName: string; var IntName: string): Integer;
    function Write: Integer;
    property CompressedSize: Int64 read GetCompressedSize
      write SetCompressedSize;
    property CompressionMethod: Word read GetCompressionMethod
      write SetCompressionMethod;
    property CRC32: Cardinal read GetCRC32 write SetCRC32;
    property DateTime: Cardinal read GetDateTime write SetDateTime;
    property Encoded: TZMEncodingOpts read GetEncoded;
    property Encrypted: Boolean read GetEncrypted write SetEncrypted;
    property ExtFileAttrib: Longword read GetExtFileAttrib
      write SetExtFileAttrib;
    property ExtraData[Tag: Word]: TZMRawBytes read GetExtraData
      write SetExtraData;
    property ExtraField: TZMRawBytes read GetExtraField write SetExtraField;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComment: string read GetFileComment write SetFileComment;
    property FileCommentLen: Word read GetFileCommentLen;
    property FileName: string read GetFileName write SetFileName;
    property FileNameLen: Word read GetFileNameLen;
    property Flag: Word read GetFlag write SetFlag;
    property IntFileAttrib: Word read GetIntFileAttrib write SetIntFileAttrib;
    property IsEncoded: TZMEncodingOpts read GetIsEncoded write SetIsEncoded;
    property Link: TZMEntryBase read FLink write FLink;
    property ModifDateTime: Longword read GetModifDateTime
      write SetModifDateTime;
    property RelOffLocalHdr: Int64 read GetRelOffLocalHdr
      write SetRelOffLocalHdr;
    property StartOnDisk: Cardinal read GetStartOnDisk write SetStartOnDisk;
    property TempVar: Integer read FTempVar write FTempVar;
    property UncompressedSize: Int64 read GetUncompressedSize
      write SetUncompressedSize;
    property VersionMadeBy: Word read GetVersionMadeBy write SetVersionMadeBy;
    property VersionNeeded: Word read GetVersionNeeded write SetVersionNeeded;
    property _CompressedSize: Cardinal read Get_CompressedSize;
    property _FileComment: TZMRawBytes read F_FileComment write F_FileComment;
    property _FileName: TZMRawBytes read F_FileName write F_FileName;
    property _RelOffLocalHdr: Cardinal read Get_RelOffLocalHdr;
    property _StartOnDisk: Word read Get_StartOnDisk;
    property _UncompressedSize: Cardinal read Get_UncompressedSize;
  end;

type
  TZMEntryCopier = class(TZMEntryWriter)
  public
    procedure AssignFrom(const ARec: TZMEntryBase); override;
    function Process: Int64; override;
    function ProcessSize: Int64; override;
  end;

type
  TZMZipCopier = class(TZMZipWriter)
  public
    function AffixZippedFile(Rec: TZMEntryBase): TZMEntryCopier;
    function AffixZippedFiles(Src: TZMZipReader; All: Boolean): Integer;
    function WriteFile(InZip: TZMZipReader; All: Boolean): Int64;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, System.TypInfo,
{$ELSE}
  SysUtils, TypInfo,
{$ENDIF}
  ZMBody, ZMMsg, ZMUtils, ZMUTF8, ZMMisc, ZMZipEOC, ZMHandler, ZMCRC, ZMCore;

const
  __UNIT__ = 47;

const
  AllSpec: string = '*.*';
  AnySpec: string = '*';

const
  MAX_BYTE = 255;
  MaxDupAsk = 5; // maximum number of tries to manually rename duplicate

type
  Txdat64 = packed record
    Tag: Word;
    Siz: Word;
    Vals: array [0 .. 4] of Int64; // last only cardinal
  end;

type
  TZMEntryExt = class(TZMEntryCopier)
  protected
    function GetHTFileName: string; override;
  public
    function HTIsSame(const AnEntry: TZMEntryBase): Boolean; override;
    function HTIsSameStr(StrHash: Cardinal; const Str: string)
      : Boolean; override;
    function Process: Int64; override;
    function ProcessSize: Int64; override;
  end;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
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

constructor TZMEntryWriter.Create(TheOwner: TZMZipWriter);
begin
  inherited Create(TheOwner);
end;

procedure TZMEntryWriter.AssignFrom(const ZRec: TZMEntryBase);
var
  OldFile: TZMZipDirectory;
  OldHTNext: TZMEntryBase;
  OldNext: TZMEntryBase;
begin
  OldHTNext := HTNext;
  OldNext := Next;
  OldFile := MyFile;
  inherited;
  if ZRec <> Self then
  begin
    CompressedSize := ZRec.CompressedSize;
    CompressionMethod := ZRec.CompressionMethod;
    CRC32 := ZRec.CRC32;
    ExtFileAttrib := ZRec.ExtFileAttrib;
    ExtraField := ZRec.ExtraField;
    FileComment := ZRec.FileComment;
    FileName := ZRec.FileName;
    Flag := ZRec.Flag;
    _FileComment := ZRec._FileComment;
    _FileName := ZRec._FileName;
    IntFileAttrib := ZRec.IntFileAttrib;
    ModifDateTime := ZRec.ModifDateTime;
    RelOffLocalHdr := ZRec.RelOffLocalHdr;
    StartOnDisk := ZRec.StartOnDisk;
    UncompressedSize := ZRec.UncompressedSize;
    VersionNeeded := ZRec.VersionNeeded;
    VersionMadeBy := ZRec.VersionMadeBy;
    HTNext := OldHTNext; // restore original values
    Next := OldNext;
    MyFile := OldFile;
    ClearStatusBit(ZsbHashed or ZsbRenamed or ZsbVChanged);
  end;
end;

procedure TZMEntryWriter.BeforeDestruction;
begin
  inherited;
end;

function TZMEntryWriter.ChangeName(const NewName: string): Integer;
var
  Iname: string;
begin
  Result := ToIntForm(NewName, Iname);
  if Result = 0 then
  begin
    if IsFolder(Iname) <> IsFolder(FileName) then
    begin
      Result := ZM_Error(413, ZE_NoChangeDir);
      Exit; // dirOnly status must be same
    end;
    if CompareStr(Iname, FileName) <> 0 then
    begin
      SetStatusBit(ZsbDirty or ZsbVChanged or ZsbRenamed);
      MyFile.HTRemove(Self);
      FileName := Iname;
      _FileName := '';
    end;
  end;
end;

procedure TZMEntryWriter.ClearCachedName;
begin
  // does nothing
end;

procedure TZMEntryWriter.FixMinimumVers(IsZ64: Boolean);
const
  OS_FAT: Word = (FS_FAT * 256);
  WZIP = (FS_NTFS * 256) + 50;
var
  NewNeed: Word;
begin
  if ((VersionMadeBy and VerMask) <= ZIP64_VER) and
    ((VersionNeeded and VerMask) <= ZIP64_VER) then
  begin
    if IsZ64 then
      VersionMadeBy := (VersionMadeBy and OSMask) or ZIP64_VER
    else
      if (VersionMadeBy and VerMask) = ZIP64_VER then
      begin
        // zip64 no longer needed
        VersionMadeBy := (VersionMadeBy and OSMask) or OUR_VEM;
      end;
    // correct bad encodings - marked ntfs should be fat
    if VersionMadeBy = WZIP then
      VersionMadeBy := OS_FAT or OUR_VEM;

    case CompressionMethod of
      0:
        NewNeed := 10; // stored
      1 .. 8:
        NewNeed := 20;
      9:
        NewNeed := 21; // enhanced deflate
      10:
        NewNeed := 25; // DCL
      12:
        NewNeed := 46; // BZip2
    else
      NewNeed := ZIP64_VER;
    end;
    if ((Flag and 32) <> 0) and (NewNeed < 27) then
      NewNeed := 27;
    if IsZ64 and (NewNeed < ZIP64_VER) then
      NewNeed := ZIP64_VER;
    // keep needed os
    VersionNeeded := (VersionNeeded and OSMask) + NewNeed;
  end;
end;

function TZMEntryWriter.GetCompressedSize: Int64;
begin
  Result := Header.ComprSize;
  if Result = MAX_UNSIGNED then
    Result := _CSize;
end;

function TZMEntryWriter.GetCompressionMethod: Word;
begin
  Result := Header.ComprMethod;
end;

function TZMEntryWriter.GetCRC32: Cardinal;
begin
  Result := Header.CRC32;
end;

function TZMEntryWriter.GetDateTime: Cardinal;
begin
  Result := Header.ModifDateTime;
end;

function TZMEntryWriter.GetEncrypted: Boolean;
begin
  Result := (Flag and FLAG_CRYPT_BIT) <> 0;
  if Result <> FCrypt then
    Encrypted := FCrypt;
end;

function TZMEntryWriter.GetExtFileAttrib: Longword;
begin
  Result := Header.ExtFileAtt;
end;

// returns the 'data' without the tag
function TZMEntryWriter.GetExtraData(Tag: Word): TZMRawBytes;
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

function TZMEntryWriter.GetExtraField: TZMRawBytes;
begin
  Result := FExtraField;
end;

function TZMEntryWriter.GetExtraFieldLength: Word;
begin
  Result := Length(FExtraField);
end;

function TZMEntryWriter.GetFileComment: string;
begin
  Result := FFileComment;
end;

function TZMEntryWriter.GetFileCommentLen: Word;
begin
  Result := Length(F_FileComment);
end;

function TZMEntryWriter.GetFileName: string;
begin
  Result := FFileName;
end;

function TZMEntryWriter.GetFileNameLen: Word;
begin
  Result := Length(_FileName);
end;

function TZMEntryWriter.GetFlag: Word;
begin
  Result := Header.Flag;
end;

function TZMEntryWriter.GetIntFileAttrib: Word;
begin
  Result := Header.IntFileAtt;
end;

function TZMEntryWriter.GetIsEncoded: TZMEncodingOpts;
begin
  Result := inherited GetIsEncoded;
end;

function TZMEntryWriter.GetModifDateTime: Longword;
begin
  Result := Header.ModifDateTime;
end;

function TZMEntryWriter.GetRelOffLocalHdr: Int64;
begin
  Result := Header.RelOffLocalHdr;
  if Result = MAX_UNSIGNED then
    Result := _RelOfs;
end;

function TZMEntryWriter.GetStartOnDisk: Cardinal;
begin
  Result := Header.DiskStart;
  if Result = MAX_WORD then
    Result := _DiskStt;
end;

function TZMEntryWriter.GetTitle: string;
begin
  Result := FileName;
  if Status[ZsbRenamed] and (Link <> nil) then
    Result := Link.Title + ' :: ' + Result;
end;

function TZMEntryWriter.GetUncompressedSize: Int64;
begin
  Result := Header.UncomprSize;
  if Result = MAX_UNSIGNED then
    Result := _USize;
end;

function TZMEntryWriter.GetVersionMadeBy: Word;
begin
  Result := Header.VersionMadeBy;
end;

function TZMEntryWriter.GetVersionNeeded: Word;
begin
  Result := Header.VersionNeeded;
end;

function TZMEntryWriter.Get_CompressedSize: Cardinal;
begin
  Result := Header.ComprSize;
end;

function TZMEntryWriter.Get_FileComment: TZMRawBytes;
begin
  Result := F_FileComment;
end;

function TZMEntryWriter.Get_FileName: TZMRawBytes;
begin
  Result := F_FileName;
end;

function TZMEntryWriter.Get_RelOffLocalHdr: Cardinal;
begin
  Result := Header.RelOffLocalHdr;
end;

function TZMEntryWriter.Get_StartOnDisk: Word;
begin
  Result := Header.DiskStart;
end;

function TZMEntryWriter.Get_UncompressedSize: Cardinal;
begin
  Result := Header.UncomprSize;
end;

// also calculate required version and create extra data
function TZMEntryWriter.LocalSize: Cardinal;
begin
  Result := Sizeof(TZipLocalHeader);
  PrepareLocalData; // form local extra data
  Inc(Result, FileNameLen + Length(LocalData));
end;

procedure TZMEntryWriter.PrepareLocalData;
var
  Xd: Txdat64;
  Need64: Boolean;
begin
  LocalData := ''; // empty
  ClearStatusBit(ZsbLocal64);
  // check for Zip64
  Need64 := (UncompressedSize >= MAX_UNSIGNED) or
    (CompressedSize >= MAX_UNSIGNED);
  FixMinimumVers(Need64);
  if Need64 then
  begin
    SetStatusBit(ZsbLocal64);
    Xd.Tag := Zip64_data_tag;
    Xd.Siz := 16;
    Xd.Vals[0] := UncompressedSize;
    Xd.Vals[1] := CompressedSize;
    SetLength(FLocalData, 20);
    Move(Xd.Tag, PAnsiChar(LocalData)^, 20);
  end;
  // remove unwanted 'old' tags
  if ExtraFieldLength > 0 then
    LocalData := LocalData + XDataRemove(ExtraField,
      [Zip64_data_tag, NTFS_data_tag, UCmnt_Data_Tag]);
  SetStatusBit(ZsbLocalDone);
end;

procedure TZMEntryWriter.SetCompressedSize(const Value: Int64);
begin
  if Value >= MAX_UNSIGNED then
  begin
    _CSize := Value;
    Header.ComprSize := MAX_UNSIGNED;
  end
  else
  begin
    Header.ComprSize := Value;
    _CSize := 0;
  end;
end;

procedure TZMEntryWriter.SetCompressionMethod(const Value: Word);
begin
  Header.ComprMethod := Value;
end;

procedure TZMEntryWriter.SetCRC32(const Value: Cardinal);
begin
  Header.CRC32 := Value;
end;

procedure TZMEntryWriter.SetDateTime(const Value: Cardinal);
begin
  Header.ModifDateTime := Value;
end;

procedure TZMEntryWriter.SetEncrypted(const Value: Boolean);
begin
  FCrypt := Value;
  if Value then
    Flag := Flag or FLAG_CRYPT_BIT
  else
    Flag := Flag and (not FLAG_CRYPT_BIT);
end;

procedure TZMEntryWriter.SetExtFileAttrib(const Value: Longword);
begin
  Header.ExtFileAtt := Value;
end;

// assumes data contains the data with no header
procedure TZMEntryWriter.SetExtraData(Tag: Word; const Data: TZMRawBytes);
var
  After: Integer;
  AfterLen: Integer;
  Nidx: Integer;
  Ix: Integer;
  NewXData: TZMRawBytes;
  DataSize: Word;
  Sz: Integer;
  V: Integer;
  X: TZMRawBytes;
begin
  X := ExtraField;
  XData(X, Tag, Ix, Sz); // find existing Tag
  V := Length(X) - Sz; // size after old tag removed
  if Length(Data) > 0 then
    V := V + Length(Data) + 4;
  if V > MAX_WORD then // new length too big?
    Exit; // maybe give error
  DataSize := Length(Data);
  SetLength(NewXData, V);
  Nidx := 1; // next Index into newXData
  if (DataSize > 0) then
  begin
    // prefix required tag
    NewXData[1] := AnsiChar(Tag and MAX_BYTE);
    NewXData[2] := AnsiChar(Tag shr 8);
    NewXData[3] := AnsiChar(DataSize and MAX_BYTE);
    NewXData[4] := AnsiChar(DataSize shr 8);
    // add the data
    Move(Data[1], NewXData[5], DataSize);
    Inc(Nidx, DataSize + 4);
  end;
  if Ix >= 1 then
  begin
    // had existing data
    if Ix > 1 then
    begin
      // append data from before existing tag
      Move(X[1], NewXData[Nidx], Ix - 1);
      Inc(Nidx, Ix);
    end;
    After := Ix + Sz; // Index after replaced tag
    if After < Length(X) then
    begin
      // append data from after existing
      AfterLen := Length(X) + 1 - After;
      Move(X[After], NewXData[Nidx], AfterLen);
    end;
  end
  else
  begin
    // did not exist
    if Length(X) > 0 then
      Move(X[1], NewXData[Nidx], Length(X)); // append old extra data
  end;
  ExtraField := NewXData;
end;

procedure TZMEntryWriter.SetExtraField(const Value: TZMRawBytes);
begin
  FExtraField := Value;
  FVariables := ''; // will rebuild later
end;

procedure TZMEntryWriter.SetFileComment(const Value: string);
begin
  FFileComment := Value;
  FVariables := ''; // will rebuild later
  Status[ZsbExtCmnt] := StrHasExt(Value);
  SetStatusBit(ZsbVChanged);
end;

procedure TZMEntryWriter.SetFileName(const Value: string);
begin
  FFileName := Value;
  FVariables := ''; // will rebuild later
  Status[ZsbExtName] := StrHasExt(Value);
  SetStatusBit(ZsbVChanged);
  ClearStatusBit(ZsbHashed);
end;

procedure TZMEntryWriter.SetFlag(const Value: Word);
begin
  Header.Flag := Value;
end;

procedure TZMEntryWriter.SetIntFileAttrib(const Value: Word);
begin
  Header.IntFileAtt := Value;
end;

procedure TZMEntryWriter.SetModifDateTime(const Value: Longword);
begin
  Header.ModifDateTime := Value;
end;

procedure TZMEntryWriter.SetRelOffLocalHdr(const Value: Int64);
begin
  if Value >= MAX_UNSIGNED then
  begin
    _RelOfs := Value;
    Header.RelOffLocalHdr := MAX_UNSIGNED;
  end
  else
  begin
    Header.RelOffLocalHdr := Value;
    _RelOfs := 0;
  end;
end;

procedure TZMEntryWriter.SetStartOnDisk(const Value: Cardinal);
begin
  FStartOnDisk := Value;
  if Value >= MAX_WORD then
  begin
    _DiskStt := Value;
    Header.DiskStart := MAX_WORD;
  end
  else
  begin
    Header.DiskStart := Value;
    _DiskStt := 0;
  end;
end;

procedure TZMEntryWriter.SetUncompressedSize(const Value: Int64);
begin
  if Value >= MAX_UNSIGNED then
  begin
    _USize := Value;
    Header.UncomprSize := MAX_UNSIGNED;
  end
  else
  begin
    Header.UncomprSize := Value;
    _USize := 0;
  end;
end;

procedure TZMEntryWriter.SetVersionMadeBy(const Value: Word);
begin
  Header.VersionMadeBy := Value;
end;

procedure TZMEntryWriter.SetVersionNeeded(const Value: Word);
begin
  Header.VersionNeeded := Value;
end;

// converts to internal delimiter
function TZMEntryWriter.StripDrive(const FName: string;
  NoPath: Boolean): string;
var
  Nam: Integer;
  Posn: Integer;
begin
  Result := SetSlash(FName, PsdExternal);
  // Remove drive: or //host/share
  Posn := 0;
  if Length(Result) > 1 then
  begin
    if Result[2] = ':' then
    begin
      Posn := 2;
      if (Length(Result) > 2) and (Result[3] = PathDelim) then
        Posn := 3;
    end
    else
      if (Result[1] = PathDelim) and (Result[2] = PathDelim) then
      begin
        Posn := 3;
        while (Posn < Length(Result)) and (Result[Posn] <> PathDelim) do
          Inc(Posn);
        Inc(Posn);
        while (Posn < Length(Result)) and (Result[Posn] <> PathDelim) do
          Inc(Posn);
        if Posn >= Length(Result) then
        begin
          // error - invalid host/share
          Body.Inform('Invalid filespec: ' + Result, {_LINE_}909, __UNIT__);
          Result := '';
          Exit;
        end;
      end;
  end;
  Inc(Posn);
  // remove leading ./
  if ((Posn + 1) < Length(Result)) and (Result[Posn] = '.') and
    (Result[Posn + 1] = PathDelim) then
    Posn := Posn + 2;
  // remove path if not wanted
  if NoPath then
  begin
    Nam := LastPos(Result, PathDelim);
    if Nam > Posn then
      Posn := Nam + 1;
  end;
  Result := Copy(Result, Posn, MAX_PATH);
end;

function TZMEntryWriter.ToIntForm(const ExtName: string;
  var IntName: string): Integer;
var
  Temp: string;
begin
  Result := 0;
  IntName := StripDrive(ExtName, False);
  // truncate if too long
  if Length(IntName) > MAX_PATH then
  begin
    Temp := IntName;
    SetLength(IntName, MAX_PATH);
    Body.Inform('Truncated ' + Temp + ' to ' + IntName, {_LINE_}942, __UNIT__);
  end;
  if IsInvalidIntName(IntName) then
    Result := Body.PrepareErrMsg(ZE_BadFileName, [IntName], {_LINE_}945,
      __UNIT__);
end;

// write the central entry on it's MyFile
// return bytes written (< 0 = -Error)
function TZMEntryWriter.Write: Integer;
var
  Ch: PZipCentralHeader;
  L: Integer;
  Need64: Boolean;
  Ni: TZMRawBytes;
  P: PByte;
  Pb: PByte;
  R: Integer;
  Siz: Word;
  Vals: array [0 .. 4] of Int64;
  Wf: TZMZipWriter;
  X: TZMRawBytes;
begin
  Wf := MyFile as TZMZipWriter;
  // Diag('Write central');
  Result := -1;
  if not Wf.IsOpen then
    Exit;
  Pb := Wf.WBuffer(Sizeof(TZipCentralHeader));
  Ch := PZipCentralHeader(Pb);
  Ni := _FileName;
  Ch^.HeaderSig := CentralFileHeaderSig;
  Ch^.VersionMadeBy := VersionMadeBy;
  Ch^.VersionNeeded := VersionNeeded;
  // assumes local was written - may be updated
  Ch^.Flag := Flag;
  Ch^.ComprMethod := CompressionMethod;
  Ch^.ModifDateTime := ModifDateTime;
  Ch^.CRC32 := CRC32;
  Ch^.FileNameLen := Length(Ni);
  Ch^.FileComLen := Length(_FileComment);
  Ch^.IntFileAtt := IntFileAttrib;
  Ch^.ExtFileAtt := ExtFileAttrib;

  Siz := 0;
  if (UncompressedSize >= MAX_UNSIGNED) then
  begin
    Vals[0] := UncompressedSize;
    Siz := 8;
    Ch^.UncomprSize := MAX_UNSIGNED;
  end
  else
    Ch^.UncomprSize := Cardinal(UncompressedSize);

  if (CompressedSize >= MAX_UNSIGNED) then
  begin
    Vals[Siz div 8] := CompressedSize;
    Inc(Siz, 8);
    Ch^.ComprSize := MAX_UNSIGNED;
  end
  else
    Ch^.ComprSize := Cardinal(CompressedSize);

  if (RelOffLocalHdr >= MAX_UNSIGNED) then
  begin
    Vals[Siz div 8] := RelOffLocalHdr;
    Inc(Siz, 8);
    Ch^.RelOffLocalHdr := MAX_UNSIGNED;
  end
  else
    Ch^.RelOffLocalHdr := Cardinal(RelOffLocalHdr);

  if (StartOnDisk >= MAX_WORD) then
  begin
    Vals[Siz div 8] := StartOnDisk;
    Inc(Siz, 4);
    Ch^.DiskStart := MAX_WORD;
  end
  else
    Ch^.DiskStart := Word(StartOnDisk);
  Need64 := False;
  if Siz > 0 then
  begin
    SetLength(X, Siz);
    Move(Vals[0], X[1], Siz);
    Need64 := True;
    if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
    begin
      FixMinimumVers(True);
      Ch^.VersionNeeded := VersionNeeded;
      Ch^.VersionMadeBy := VersionMadeBy;
    end;
    ExtraData[Zip64_data_tag] := X;
  end
  else
    ExtraData[Zip64_data_tag] := ''; // remove old 64 data
  if (StatusBit[ZsbLocalDone] = 0) or (Need64) then
    FixMinimumVers(Need64);
  Ch^.VersionMadeBy := VersionMadeBy;
  Ch^.VersionNeeded := VersionNeeded;
  X := '';
  Ch^.ExtraLen := ExtraFieldLength;
  Result := ZM_Error(1033, ZE_CEHBadWrite);
  L := Sizeof(TZipCentralHeader) + Ch^.FileNameLen + Ch^.ExtraLen +
    Ch^.FileComLen;
  Pb := Wf.WBuffer(L);
  P := Pb;
  Inc(P, Sizeof(TZipCentralHeader));
  Move(Ni[1], P^, Ch^.FileNameLen);
  Inc(P, Ch^.FileNameLen);
  if Ch^.ExtraLen > 0 then
  begin
    Move(ExtraField[1], P^, Ch^.ExtraLen);
    Inc(P, Ch^.ExtraLen);
  end;
  if Ch^.FileComLen > 0 then
    Move(_FileComment[1], P^, Ch^.FileComLen);
  R := Wf.WriteContiguous(Pb^, L, False);
  if R = L then
  begin
    // Diag('  Write central ok');
    Result := R;
    ClearStatusBit(ZsbDirty);
  end // ;
  else
    if R < 0 then
      Result := R;
end;

// write local header using specified stamp and crc
// return bytes written (< 0 = -Error)
function TZMEntryWriter.WriteAsLocal1(Stamp, Crc: Cardinal): Integer;
var
  Cd: TZMRawBytes;
  Fnlen: Integer;
  I: Integer;
  LOH: PZipLocalHeader;
  Need64: Boolean;
  Ni: TZMRawBytes;
  P: PByte;
  Pb: PByte;
  T: Integer;
  Wf: TZMZipWriter;
begin
  Wf := MyFile as TZMZipWriter;
  if StatusBit[ZsbLocalDone] = 0 then
    PrepareLocalData;
  LOH := PZipLocalHeader(Wf.WBuffer(Sizeof(TZipLocalHeader)));
  if ((Flag and 9) = 8) then
    Flag := Flag and $FFF7; // remove extended local data if not encrypted
  Ni := _FileName;
  Fnlen := Length(Ni);
  LOH^.HeaderSig := LocalFileHeaderSig;
  LOH^.VersionNeeded := VersionNeeded; // may be updated
  LOH^.Flag := Flag;
  LOH^.ComprMethod := CompressionMethod;
  LOH^.ModifDateTime := Stamp;
  LOH^.CRC32 := Crc;
  LOH^.FileNameLen := Fnlen;
  Cd := LocalData;
  LOH^.ExtraLen := Length(Cd); // created by LocalSize
  Need64 := (LOH^.ExtraLen > 0) and (StatusBit[ZsbLocal64] <> 0);
  if Need64 then
  begin
    LOH^.UncomprSize := MAX_UNSIGNED;
    LOH^.ComprSize := MAX_UNSIGNED;
  end
  else
  begin
    if (Flag and 8) <> 0 then
    begin
      LOH^.UncomprSize := 0;
      LOH^.ComprSize := 0;
      if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
      begin
        FixMinimumVers(True);
        LOH^.VersionNeeded := VersionNeeded;
      end;
    end
    else
    begin
      LOH^.UncomprSize := Cardinal(UncompressedSize);
      LOH^.ComprSize := Cardinal(CompressedSize);
    end;
  end;
  T := Fnlen + Length(Cd);
  Pb := Wf.WBuffer(Sizeof(TZipLocalHeader) + T);
  P := Pb;
  Inc(P, Sizeof(TZipLocalHeader));
  I := Sizeof(TZipLocalHeader); // i = destination Index
  Move(Ni[1], P^, Fnlen);
  I := I + Fnlen;
  Inc(P, Fnlen);
  // copy any extra data
  if Length(Cd) > 0 then
  begin
    Move(Cd[1], P^, Length(Cd));
    Inc(I, Length(Cd));
  end;
  Result := Wf.WriteContiguous(Pb^, I, False);
  if Result = I then
    ClearStatusBit(ZsbDirty)
  else
    Result := Body.PrepareErrMsg(ZE_LOHBadWrite, [FileName],
      {_LINE_}1148, __UNIT__);
end;

// return bytes written (< 0 = -Error)
function TZMEntryWriter.WriteDataDesc: Integer;
var
  D: TZipDataDescriptor;
  D64: TZipDataDescriptor64;
  R: Integer;
  Wf: TZMZipDirectory;//TZMZipBase;
begin
  Wf := MyFile;
  ASSERT(Assigned(Wf), 'no WorkFile');
  Result := -1;
  if not Wf.IsOpen then
    Exit;
  if (Flag and 8) <> 0 then
  begin
    Result := 0;
    Exit;
  end;
  if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
  begin
    D.DataDescSig := ExtLocalSig;
    D.CRC32 := CRC32;
    D.ComprSize := Cardinal(CompressedSize);
    D.UncomprSize := Cardinal(UncompressedSize);
    R := Wf.WriteContiguous(D, Sizeof(TZipDataDescriptor), False);
    if R = Sizeof(TZipDataDescriptor) then
      Result := R;
  end
  else
  begin
    D64.DataDescSig := ExtLocalSig;
    D64.CRC32 := CRC32;
    D64.ComprSize := CompressedSize;
    D64.UncomprSize := UncompressedSize;
    R := Wf.WriteContiguous(D64, Sizeof(TZipDataDescriptor64), False);
    if R = Sizeof(TZipDataDescriptor64) then
      Result := R;
  end;
  if Result <= 0 then
    Result := Body.PrepareErrMsg(ZE_DataDesc, [Wf.Name],
      {_LINE_}1191, __UNIT__);
end;

procedure TZMZipWriter.AddExtEntry(Entry: TZMEntryWriter);
var
  ExtEntry: TZMEntryExt;
begin
  ExtEntry := TZMEntryExt.Create(Self);
  ExtEntry.AssignFrom(Entry);
  ExtEntry.Link := Entry;
  // add to list for clean up
  ExtEntry.Next := ExtEntries;
  ExtEntries := ExtEntry;
  HTAdd(ExtEntry, True);
end;

function TZMZipWriter.AffixEntry(AnEntry: TZMEntryWriter; NoDup: Boolean):
    Boolean;
var
  Dup: TZMEntryBase;
begin
  Dup := HTAdd(AnEntry, False);
  Result := Dup <> nil;
  if Result then
  begin
    if NoDup then
      Exit;
    FDuplicates.Add(AnEntry); // we have a duplicate, append to dup list
  end;
  Add(AnEntry);
  EntryCount := EntryCount + 1;
end;

procedure TZMZipWriter.AfterConstruction;
begin
  inherited;
  FDuplicates := TList.Create;
  FToDoList := TList.Create;
  ExtEntries := nil;
end;

procedure TZMZipWriter.AppendNewEntry(AnEntry: TZMEntryWriter);
begin
  if FNewEntries = nil then
    FNewEntries := AnEntry
  else
    FNewEntryTail.Next := AnEntry;
  FNewEntryTail := AnEntry;
  AnEntry.Next := nil;
  Inc(FNewEntryCount);
end;

function TZMZipWriter.BeforeCommit: Integer;
begin
  Result := 0;
end;

procedure TZMZipWriter.BeforeDestruction;
var
  Entries: TZMEntryBase;
  Tmp: TZMEntryBase;
begin
  WBuf := nil;
  FDuplicates.Free;
  FToDoList.Free;
  // free all entries being held
  Entries := ExtEntries;
  while Entries <> nil do
  begin
    Tmp := Entries;
    Entries := Entries.Next;
    Tmp.Free;
  end;
  inherited;
end;

function TZMZipWriter.CalcSizes(var NoEntries: Integer; var ToProcess: Int64;
  var CenSize: Cardinal): Integer;
var
  Rec: TZMEntryWriter;
begin
  Result := 0;
  Rec := TZMEntryWriter(FirstRec);
  while Rec <> nil do
  begin
    ToProcess := ToProcess + Rec.ProcessSize;
    CenSize := CenSize + Rec.CentralSize;
    Inc(NoEntries);
    Rec := TZMEntryWriter(Rec.Next);
  end;
end;

function TZMZipWriter.Commit(MarkLatest: Boolean): Integer;
var
  Latest: Cardinal;
  NoEntries: Cardinal;
  TotalProcess: Int64;
  Written: Int64;
  Wrote: Int64;
begin
  Body.Trace('Commit file', {_LINE_}1296, __UNIT__);
  Latest := 0;
  Wrote := 0;
  // check and fix duplicate and incomplete entries
  Result := Preprocess;
  if Result < 0 then
    Exit;
  Result := BeforeCommit;
  if Result < 0 then
    Exit;
  HailSuccesses; // add required entries to IncludeSpecs
  // calculate sizes
  TotalProcess := 0;
  NoEntries := CommitSum(TotalProcess, Latest, MarkLatest);
  Guage.SetCount(NoEntries + 1);
  Guage.SetSize(TotalProcess);
  Body.TraceFmt(' to process %d entries. size = %d', [NoEntries, TotalProcess],
    {_LINE_}1313, __UNIT__);
  Result := 0;
  if MarkLatest then
    StampDate := Latest;
  try
    Written := CommitPreamble; // 'span' mark or stub
    if Written >= 0 then
      Wrote := Written;
    if Written >= 0 then
    begin
      // write central
      Written := CommitEntries;
      if Written >= 0 then
        Wrote := Wrote + Written;
    end;
    // finished locals and data
    if Written >= 0 then
    begin
      // write central
      Written := CommitCentral;
      if Written >= 0 then
      begin
        Result := NoEntries;
        File_Size := Written + Wrote;
      end;
    end;
    if Written < 0 then
      Result := Integer(Written);
  finally
    Guage.EndBatch;
  end;
end;

// TODO -c : make Last: TZMEntryWriter
// Last = index of last existing rec to use
function TZMZipWriter.CommitAppend(Last: Integer; MarkLatest: Boolean): Integer;
var
  FirstAppendRec: TZMEntryWriter;
  I: Integer;
  Latest: Cardinal;
  NoEntries: Integer;
  R: Integer;
  Rec: TZMEntryWriter;
  ToProcess: Int64;
  TotalProcess: Int64;
  W64: Int64;
  Wrote: Int64;
begin
  Body.Trace('CommitAppend file', {_LINE_}1361, __UNIT__);
  Latest := 0;
  Wrote := 0;
  // calculate sizes
  NoEntries := 0;
  ToProcess := 0;
  I := -1;
  FirstAppendRec := nil;
  Rec := TZMEntryWriter(FirstRec);
  while Rec <> nil do
  begin
    CheckCancel;
    Inc(I);
    ASSERT(Assigned(Rec), ' no Rec');
    if I >= Last then
    begin
      if FirstAppendRec = nil then
        FirstAppendRec := Rec;
      ToProcess := ToProcess + Rec.ProcessSize;
      Inc(NoEntries);
    end;
    if MarkLatest and (Rec.ModifDateTime > Latest) then
      Latest := Rec.ModifDateTime;
    Rec := TZMEntryWriter(Rec.Next);
  end;
  // mostly right ToProcess = total compressed sizes
  TotalProcess := ToProcess;
  if UseSFX and Assigned(Stub) and (Stub.Size > 0) and (First < 0) then
    TotalProcess := TotalProcess + Stub.Size;
  Guage.SetCount(NoEntries + 1);
  Guage.SetSize(TotalProcess);
  if Verbosity > ZvVerbose then
  begin
    Body.TraceFmt(' to process %d entries. size = %d',
      [NoEntries, TotalProcess],
      {_LINE_}1396, __UNIT__);
  end;
  Result := 0;
  if MarkLatest then
    StampDate := Latest;
  try
    // write stub if required
    if UseSFX and Assigned(Stub) and (Stub.Size > 0) and (First < 0) then
    begin
      // write the sfx stub
      Guage.NewItem(ZP_SFX, Stub.Size);
      Stub.Position := 0;
      Result := WriteFrom(Stub, Stub.Size);
      if Result > 0 then
      begin
        Wrote := Stub.Size;
        Guage.Advance(Stub.Size);
        if ShowProgress = ZspFull then
          Progress.Written(Wrote);
        Sig := ZfsDOS; // assume correct
      end;
    end
    else
      Sig := ZfsLocal;
    if (Result >= 0) and (ToProcess > 0) then
    begin
      Rec := FirstAppendRec;
      while Rec <> nil do
      begin
        CheckCancel;
        W64 := CommitRec(Rec);
        if W64 < 0 then
        begin
          Result := W64;
          Break;
        end;
        Wrote := Wrote + W64;
        if ShowProgress = ZspFull then
          Body.TotalWritten := Wrote;
        Rec := TZMEntryWriter(Rec.Next);
      end;
    end;
    // finished locals and data
    if Result >= 0 then
    begin
      // write central
      if Verbosity >= ZvVerbose then
        ReportMsg(ZE_Copying, [ZipLoadStr(ZS_CopyCentral)]);
      R := WriteCentral; // uses XProgress
      if R >= 0 then
        Wrote := Wrote + R;
      Body.TraceFmt(' wrote = %u', [Wrote], {_LINE_}1447, __UNIT__);
      if R > 0 then
      begin
        Result := 0;
        File_Size := Wrote;
        Body.Trace('  finished ok', {_LINE_}1452, __UNIT__);
      end;
    end;
  finally
    Guage.EndBatch;
  end;
end;

function TZMZipWriter.CommitCentral: Int64;
var
  Wrote: Int64;
begin
  // write central
  if Verbosity > ZvVerbose then
    ReportMsg(ZE_Copying, [ZipLoadStr(ZS_CopyCentral)]);
  Result := WriteCentral; // uses XProgress
  if Result >= 0 then
  begin
    Wrote := Result;
    Result := FinishWrite;
    if Result >= 0 then
    begin
      Result := Result + Wrote;
      Body.Trace('  finished committing central', {_LINE_}1475, __UNIT__);
    end;
  end;
end;

function TZMZipWriter.CommitEntries: Int64;
var
  Rec: TZMEntryWriter;
  Written: Int64;
begin
  Result := 0;
  Rec := TZMEntryWriter(FirstRec);
  while Rec <> nil do
  begin
    CheckCancel;
    if Rec.StatusBit[ZsbError or ZsbDiscard] = 0 then
    begin
      Written := CommitRec(Rec);
      if Written < 0 then
      begin
        Result := Written; // error
        Break;
      end;
      Result := Result + Written;
      if ShowProgress = ZspFull then
        Body.TotalWritten := Result;
    end;
    Rec := TZMEntryWriter(Rec.Next);
  end;
end;

function TZMZipWriter.CommitPreamble: Integer;
var
  S: Cardinal;
begin
  Result := 0;
  // if out is going to split should write proper signal
  if IsMultiPart then
  begin
    S := ExtLocalSig;
    Result := WriteContiguous(S, 4, False);
    if (Result <> 4) and (Result > 0) then
      Result := ZM_Error(1500, ZE_NoWrite);
    Sig := ZfsMulti;
  end
  else // write stub if required
    if UseSFX and Assigned(Stub) and (Stub.Size > 0) then
    begin
      // write the sfx stub
      Guage.NewItem(ZP_SFX, Stub.Size);
      Stub.Position := 0;
      Result := WriteFrom(Stub, Stub.Size);
      if Result > 0 then
      begin
        Result := Stub.Size;
        Guage.Advance(Stub.Size);
        if ShowProgress = ZspFull then
          Progress.Written(Result);
        Sig := ZfsDOS; // assume correct
      end;
      Guage.EndItem;
    end
    else
      Sig := ZfsLocal;
end;

function TZMZipWriter.CommitRec(Rec: TZMEntryWriter): Int64;
begin
  Result := Rec.ProcessSize;
  if Result > 0 then
    Result := Rec.Process;
end;

function TZMZipWriter.CommitSum(var TotalProcess: Int64; var Latest: Cardinal;
  MarkLatest: Boolean): Cardinal;
var
  Rec: TZMEntryWriter;
begin
  Result := 0;
  TotalProcess := 0;
  Rec := TZMEntryWriter(FirstRec);
  while Rec <> nil do
  begin
    CheckCancel;
    if Rec.StatusBit[ZsbError or ZsbDiscard] = 0 then
    begin
      TotalProcess := TotalProcess + Rec.ProcessSize;
      Inc(Result);
      if MarkLatest and (Rec.ModifDateTime > Latest) then
        Latest := Rec.ModifDateTime;
    end;
    Rec := TZMEntryWriter(Rec.Next);
  end;
  // mostly right ToProcess = total compressed sizes
  if UseSFX and Assigned(Stub) and (Stub.Size > 0) then
    TotalProcess := TotalProcess + Stub.Size;
end;

function TZMZipWriter.DefaultDupAction: TZMDupResolutions;
begin
  Result := ZdrAbort;
  if ZwoAllowDup in Body.WriteOptions then
    Result := ZdrBoth
  else
    if ZwoAutoDup in Body.WriteOptions then
      Result := ZdrAuto
    else
      if ZwoSkipDup in Body.WriteOptions then
        Result := ZdrSkip;
end;

// Zip64 size aproximate only
function TZMZipWriter.EOCSize(Is64: Boolean): Cardinal;
begin
  Result := Cardinal(Sizeof(TZipEndOfCentral) + Length(ZipComment));
  if Is64 then
    Result := Result + Sizeof(TZip64EOCLocator) + Sizeof(TZipEOC64) +
      (3 * Sizeof(Int64));
end;

function TZMZipWriter.FindDuplicates: Integer;
var
  Dup: TZMEntryBase;
  Rec: TZMEntryBase;
begin
  Result := 0;
  HTAutoSize(Count); // resets
  Rec := FirstRec;
  while Rec <> nil do
  begin
    Dup := HTAdd(Rec, True);
    if Dup <> nil then
    begin
      Duplicates.Add(Rec);
      Inc(Result);
    end;
    Rec := Rec.Next;
  end;
end;

function TZMZipWriter.FixHeaderNames: Integer;
begin
  Result := 0;
end;

function TZMZipWriter.FixVariableFields(Entry: TZMEntryWriter;
  DupCnt: Integer): Integer;
var
  WDup: TZMEntryWriter;
  Dup: TZMEntryBase;
  Enc: TZMEncodingOpts;
  FComment: string;
  FName: string;
  HasXComment: Boolean;
  HasXName: Boolean;
  Hcomment: TZMRawBytes;
  I: Integer;
  Need64: Boolean;
  NeedU8Bit: Boolean;
  NewData: Boolean;
  SafeHName: TZMRawBytes;
  SafeIntName: string;
  NewMadeFS: Word;
  SHN: string;
  UComment: UTF8String;
  UData: TZMRawBytes;
  Uheader: TUString_Data_Header;
  UName: UTF8String;
  Xlen: Integer;
begin
  Enc := EncodeAs;
  NewMadeFS := (FS_FAT * 256) or OUR_VEM;
  UName := '';
  UComment := '';
  NeedU8Bit := False;
  FComment := Entry.FileComment;
  FName := Entry.FileName;
  SafeIntName := SafeHeaderName(FName); // encode unsafe codes <#255
  // default convert new name and comment to OEM
  SafeHName := StrToHeader(SafeIntName, HteOEM);
  Hcomment := StrToHeader(FComment, HteOEM);
  // make entry name
  HasXName := Entry.Status[ZsbExtName];
  HasXComment := Entry.Status[ZsbExtCmnt];
  // form required strings
  if HasXName or HasXComment then
  begin
    if Enc = ZeoAuto then
    begin
      Enc := ZeoUPath; // unless both extended
      if HasXName and HasXComment then
        Enc := ZeoUTF8;
    end;
    // convert strings
    if Enc = ZeoUTF8 then
    begin
      SafeHName := StrToHeader(SafeIntName, HteUTF8);
      Hcomment := StrToHeader(FComment, HteUTF8);
      NeedU8Bit := True;
    end
    else
    begin
      if Enc = ZeoUPath then
      begin
        // we want UPATH or/and UCOMMENT
        if HasXName then
        begin
          UName := StrTo_UTF8(SafeIntName);
          if DupCnt > 0 then
            SafeHName := StrToHeader(AppendToName(SafeIntName,
              IntToStr(DupCnt)), HteOEM)
          else
            SafeHName := StrToHeader(SafeIntName, HteOEM);
        end;
        if HasXComment then
        begin
          UComment := StrTo_UTF8(FComment);
          Hcomment := StrToHeader(FComment, HteOEM);
        end;
      end
      else
        if Enc = ZeoNone then
        begin
          // we want Ansi name and comment - NTFS
          SafeHName := StrToHeader(SafeIntName, HteAnsi);
          Hcomment := StrToHeader(FComment, HteAnsi);
          if StrHasExt(SafeHName) or StrHasExt(Hcomment) then
            NewMadeFS := (FS_NTFS * 256) or OUR_VEM; // wasn't made safe FAT
        end;
    end;
  end;
  // we now have the required strings
  // SafeHName is proposed header name
  if (ExtEntries <> nil) and not(ZwoAllowDup in Body.WriteOptions) then
  begin
    SetLength(SHN, Length(SafeHName));
    for I := 1 to Length(SafeHName) do
      SHN[I] := Char(Ord(SafeHName[I]));
    Dup := HTFind(SHN);
    while (Dup <> nil) and ((Dup = Entry) or (Dup.Status[ZsbDuplicate])) do
      Dup := HTNextDup(Dup);
    if Dup <> nil then
    begin
      Body.Inform('Duplicate header names: ' + Entry.FileName + '  ' +
        Dup.FileName, {_LINE_}1720, __UNIT__);
      if (not HasXName) or (Enc <> ZeoUPath) or (DupCnt > 255) then
      begin
        if not Dup.Status[ZsbExtName] then
        begin
          Result := Body.PrepareErrMsg(ZE_DuplFileName, [Entry.FileName],
            {_LINE_}1726, __UNIT__);
          Exit;
        end;
        if (Dup is TZMEntryExt) then
        begin
          // remove its ExtEntry and try again
          WDup := TZMEntryExt(Dup).Link as TZMEntryWriter;
          HTRemove(Dup);
          RemoveExtEntry(Dup);
        end
        else
          WDup := Dup as TZMEntryWriter;
        Duplicates.Add(WDup);
      end
      else
        Duplicates.Add(Entry);
    end;
  end;
  // remove old extra strings
  UData := XDataRemove(Entry.ExtraField, [UPath_Data_Tag, UCmnt_Data_Tag]);
  NewData := Length(UData) <> Entry.ExtraFieldLength;
  // add new extra strings
  if UName <> '' then
  begin
    Uheader.Tag := UPath_Data_Tag;
    Uheader.Totsiz := Sizeof(TUString_Data_Header) + Length(UName) -
      (2 * Sizeof(Word));
    Uheader.Version := 1;
    Uheader.Origcrc := ZCRC32(0, SafeHName[1], Length(SafeHName));
    XDataAppend(UData, Uheader, Sizeof(Uheader), UName[1], Length(UName));
    NewData := True;
  end;

  if UComment <> '' then
  begin
    // append UComment
    Uheader.Tag := UCmnt_Data_Tag;
    Uheader.Totsiz := Sizeof(TUString_Data_Header) + Length(UComment) -
      (2 * Sizeof(Word));
    Uheader.Version := 1;
    Uheader.Origcrc := ZCRC32(0, Hcomment[1], Length(Hcomment));
    XDataAppend(UData, Uheader, Sizeof(Uheader), UComment[1], Length(UComment));
    NewData := True;
  end;
  // will it fit?
  Result := ZM_Error({_LINE}1751, ZE_CEHDataSize);
  Xlen := Length(Entry._FileComment) + Length(SafeHName) + Length(UData);
  if Xlen < MAX_WORD then
  begin
    // ok - make change
    Entry._FileName := SafeHName;
    Entry._FileComment := Hcomment;

    if NewData then
      Entry.ExtraField := UData;

    if NeedU8Bit then
      Entry.Flag := Entry.Flag or FLAG_UTF8_BIT
    else
      Entry.Flag := Entry.Flag and (not FLAG_UTF8_BIT);
    Entry.ClearCachedName;
    Entry.IsEncoded := ZeoAuto; // unknown
    Need64 := (Entry.UncompressedSize >= MAX_UNSIGNED) or
      (Entry.CompressedSize >= MAX_UNSIGNED);
    // set versions to minimum required
    Entry.VersionMadeBy := NewMadeFS;
    Entry.FixMinimumVers(Need64);
    MarkDirty;
    Result := 0;
    if HasXName then
      AddExtEntry(Entry);
  end;
end;

procedure TZMZipWriter.HailSuccesses;
var
  Rec: TZMEntryWriter;
begin
  Rec := TZMEntryWriter(FirstRec);
  while Rec <> nil do
  begin
    if Rec.Status[ZsbHail] then
    begin
      IncludeSpecs.Add(Rec.Title);
      Rec.Status[ZsbHail] := False;
    end;
    Rec := TZMEntryWriter(Rec.Next);
  end;
end;

function TZMZipWriter.MakeDupedName(const Name: string;
  TryIndex: Integer): string;
var
  Stmp: Cardinal;
  Suffix: string;
begin
  if ZwoExtDup in Body.WriteOptions then
  begin
    if RefTime = '' then
    begin
      Stmp := Cardinal(DateTimeToFileDate(Now));
      RefTime := '{' + IntToBase36(Stmp);
    end;
    Suffix := RefTime;
    if TryIndex > 0 then
      Suffix := Suffix + IntToStr(TryIndex);
  end
  else
    Suffix := '{' + IntToStr(TryIndex + 1);
  Suffix := Suffix + '}';
  Result := AppendToName(Name, Suffix);
end;

procedure TZMZipWriter.MarkDirty;
begin
  Info := Info or Zfi_Dirty;
end;

function TZMZipWriter.PrepareWrite(Typ: TZipWrites): Boolean;
begin
  case Typ of
    ZwSingle:
      Result := False;
    ZwMultiple:
      Result := True;
  else
    Result := ZwoDiskSpan in WriteOptions;
  end;
  IsMultiPart := Result;
  if Result then
  begin
    DiskNr := 0;
    File_Close;
  end
  else
    DiskNr := -1;
end;

// check and fix duplicate and incomplete Entry
function TZMZipWriter.Preprocess: Integer;
begin
  // locate Duplicates
  Result := FindDuplicates;
  // resolve any duplicates
  if Result > 0 then
    Result := ResolveDuplicates;
  if Result = 0 then
    Result := UpdateCentralEntries;
end;

procedure TZMZipWriter.RemoveExtEntry(Entry: TZMEntryBase);
var
  Prev: TZMEntryBase;
  Tmp: TZMEntryBase;
begin
  if (Entry = nil) or (ExtEntries = nil) then
    Exit;
  Prev := nil;
  Tmp := ExtEntries;
  while (Tmp <> nil) and (Tmp <> Entry) do
  begin
    Prev := Tmp;
    Tmp := Tmp.Next;
  end;
  if Tmp = Entry then
  begin
    // found
    if Prev = nil then
      ExtEntries := Entry.Next // was first
    else
      Prev.Next := Entry.Next;
  end;
  Entry.Free;
end;

function TZMZipWriter.ResolveDupEntry(Entry, Dup: TZMEntryWriter;
  var NewName: string): TZMDupResolutions;
var
  ConflictEntry: TZMDupConflict;
  ExistEntry: TZMDupConflict;
  TmpWriteDupName: TZMWriteDupNameEvent;
begin
  // Do we have a event assigned for this then don't ask.
  TmpWriteDupName := Master.OnWriteDupName;
  if Assigned(TmpWriteDupName) then
  begin
    ConflictEntry := nil;
    ExistEntry := TZMDupConflict.Create(Master, Dup);
    try
      ExistEntry.ZipName := Dup.MyFile.ArchiveName;
      ExistEntry.OriginalName := Dup.Link.FileName;
      ConflictEntry := TZMDupConflict.Create(Master, Entry);
      ConflictEntry.ZipName := Entry.MyFile.ArchiveName;
      ConflictEntry.OriginalName := Entry.Link.FileName;
      Result := ZdrAuto;
      TmpWriteDupName(Master, ExistEntry, ConflictEntry, NewName, Result);
    finally
      ExistEntry.Free;
      ConflictEntry.Free;
    end;
    Exit;
  end;
  Result := DefaultDupAction;
end;

function TZMZipWriter.ResolveDuplicates: Integer;
var
  BadTries: Integer;
  NewName: string;
  Dup: TZMEntryWriter;
  DupCount: Integer;
  Entry: TZMEntryWriter;
  LastCount: Integer;
  Names: string;
  Resolved: TZMDupResolutions;
  Tmp: string;
  TryIndex: Integer;
begin
  Result := 0;
  LastCount := high(Integer);
  BadTries := 0;
  TryIndex := -1;
  while FDuplicates.Count > 0 do
  begin
    Inc(TryIndex);
    DupCount := FDuplicates.Count;
    if DupCount < LastCount then
    begin
      LastCount := DupCount;
      BadTries := 0;
    end
    else
      Inc(BadTries);
    Body.TraceFmt('Resolving %u duplicate names', [DupCount], {_LINE}1947,
      __UNIT__);
    while DupCount > 0 do
    begin
      Entry := FDuplicates[0];
      // remove Entry from duplicates list
      FDuplicates.Delete(0);
      Dec(DupCount);
      Dup := TZMEntryWriter(FindName(Entry.FileName, nil));
      // need to ignore zsbDuplicate
      while (Dup <> nil) and (Dup.Status[ZsbDuplicate]) do
        Dup := TZMEntryWriter(HTNextDup(Dup));
      if Dup = nil then
        Continue; // already resolved
      if Verbosity > ZvVerbose then
        Body.TraceFmt('Resolving duplicate entry: %d:%s',
          [Entry.ExtIndex, Entry.FileName], {_LINE_}1975, __UNIT__);
      NewName := MakeDupedName(Entry.FileName, TryIndex); // propose a name
      if BadTries < MaxDupAsk then
        Resolved := ResolveDupEntry(Entry, Dup, NewName)
      else
      begin
        if BadTries > 10 then
          Resolved := ZdrAbort // give up
        else
          Resolved := DefaultDupAction;
      end;
      if Resolved = ZdrAbort then
      begin
        Result := ZM_Error({_LINE}1968, ZS_Canceled);
        Exit;
      end;
      // do required action
      case Resolved of
        ZdrAuto, ZdrRename:
          begin
            if FindName(NewName, nil) = nil then
            begin
              Tmp := Entry.Title;
              if Entry.ChangeName(NewName) = 0 then
              begin
                HTAdd(Entry, True);
                Entry.Status[ZsbResolved] := True; // successfully resolved
                Body.InformFmt('Resolved duplicate entry: %d:%s ::: %s',
                  [Entry.ExtIndex, Tmp, NewName], {_LINE_}2003, __UNIT__);
              end;
            end;
            if not Entry.Status[ZsbResolved] then
            begin
              FDuplicates.Add(Entry);
              Body.TraceFmt('Could not rename: %s to %s',
                [Entry.FileName, NewName], {_LINE_}2010, __UNIT__);
            end;
          end;
        ZdrSkip: // remove, delete
          begin
            // add to 'skipped' list
            Names := Entry.Title + ' :=: ' + Dup.Title;
            Result := Body.PrepareErrMsg(ZE_DuplFileName, [Entry.Title],
              {_LINE_}2018, __UNIT__);
            RemoveEntry(Entry);
            if Skipping(Names, StDupName, Result) then
              Exit; // fatal
            Result := 0; // skip
          end;
        ZdrBoth: // move to versioned
          begin
            // nothing to do
            Entry.Status[ZsbDuplicate] := True;
          end;
        ZdrDefer:
          FDuplicates.Add(Entry); // try again next outer loop
      end;
    end;
  end;
end;

function TZMZipWriter.SafeHeaderName(const IntName: string): string;
const
  BadChars: TSysCharSet = [#0 .. #31, ':', '<', '>', '|', '*', '?', #39, '\'];
var
  C: Char;
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(IntName) do
  begin
    C := IntName[I];
    if (C <= #255) and (AnsiChar(C) in BadChars) then
    begin
      if C = '\' then
        Result := Result + PathDelimAlt
      else
        Result := Result + '#$' + IntToHex(Ord(C), 2);
    end
    else
      Result := Result + C;
  end;
end;

function TZMZipWriter.StrToHeader(const AString: string; How: THowToEnc)
  : TZMRawBytes;
begin
{$IFDEF UNICODE}
  if How = HteUTF8 then
    Result := TZMRawBytes(WideToUTF8(AString, -1))
  else
    Result := TZMRawBytes(WideToSafe(AString, How = HteOEM));
{$ELSE}
  if Body.IsUtf8 then
  begin
    if How = HteUTF8 then
      Result := TZMRawBytes(AString)
    else
      Result := TZMRawBytes(WideToSafe(UTF8ToWide(AString), How = HteOEM));
  end
  else
  begin
    case How of
      HteOEM:
        Result := TZMRawBytes(StrToOEM(AString));
      HteAnsi:
        Result := TZMRawBytes(AString);
      HteUTF8:
        Result := TZMRawBytes(StrToUTF8(AString));
    end;
  end;
{$ENDIF}
end;

function TZMZipWriter.StrTo_UTF8(const AString: string): UTF8String;
begin
{$IFDEF UNICODE}
  Result := UTF8String(AString);
{$ELSE}
  if Body.IsUtf8 then
    Result := AsUTF8Str(AString) // make sure UTF8
  else
    Result := StrToUTF8(AString);
{$ENDIF}
end;

// prepare central fields for writing
function TZMZipWriter.UpdateCentralEntries: Integer;
var
  DupCnt: Integer;
  Entry: TZMEntryWriter;
  I: Integer;
begin
  Result := 0;
  ToDoList.Clear; // for changed entries
  Entry := TZMEntryWriter(FirstRec);
  while Entry <> nil do
  begin
    if Entry.StatusBit[ZsbVChanged] <> 0 then
      ToDoList.Add(Entry)
    else
      if Entry.Status[ZsbExtName] then
        AddExtEntry(Entry); // add HT entry for _FileName
    Entry := TZMEntryWriter(Entry.Next);
  end;
  // fix entries
  if ToDoList.Count > 0 then
  begin
    DupCnt := -1;
    repeat
      Duplicates.Clear; // for fixable duplicate header names
      Inc(DupCnt);
      for I := 0 to ToDoList.Count - 1 do
      begin
        Entry := TZMEntryWriter(ToDoList[I]);
        if Verbosity >= ZvTrace then
          Body.TraceFmt('Updating fields %s', [Entry.FileName], {_LINE_}2131,
            __UNIT__);
        Result := FixVariableFields(Entry, DupCnt);
        if Result < 0 then
          Break
        else
          if (Result = 0) and (Entry.Status[ZsbExtName]) then
            AddExtEntry(Entry);
      end;
      if Duplicates.Count > 0 then
      begin
        ToDoList.Clear;
        for I := 0 to Duplicates.Count - 1 do
        begin
          ToDoList.Add(Duplicates[I]);
        end;
      end;
    until (Duplicates.Count = 0) or (DupCnt > 255);
  end;
end;

function TZMZipWriter.WBuffer(Size: Integer): PByte;
begin
  if Size < 1 then
    WBuf := nil
  else
    if high(WBuf) < Size then
    begin
      Size := Size or $3FF;
      SetLength(WBuf, Size + 1); // reallocate
    end;
  Result := @WBuf[0];
end;

// returns bytes written or <0 _ error
function TZMZipWriter.WriteCentral: Integer;
var
  I: Integer;
  Rec: TZMEntryWriter;
  Wrote: Integer;
begin
  Result := 0;
  Wrote := 0;
  CentralOffset := Position;
  CentralDiskNo := DiskNr;
  TotalEntries := 0;
  CentralEntries := 0;
  CentralSize := 0;
  Guage.NewXtraItem(ZxCentral, Count);
  Rec := TZMEntryWriter(FirstRec);
  I := 0; // counter
  while Rec <> nil do
  begin
    if Rec.StatusBit[ZsbError or ZsbDiscard] = 0 then
    begin
      // no processing error
      if Verbosity >= ZvTrace then
        Body.TraceFmt('Writing central [%u] %s', [I, Rec.FileName],
          {_LINE_}2189, __UNIT__);
      // check for deleted?
      Result := Rec.Write;
      if Result < 0 then
        Break; // error
      if Position <= Result then // started new part
        CentralEntries := 0;
      Wrote := Wrote + Result;
      CentralSize := CentralSize + Cardinal(Result);
      TotalEntries := TotalEntries + 1;
      CentralEntries := CentralEntries + 1;
      Guage.AdvanceXtra(1);
    end
    else
      if Verbosity > ZvVerbose then
        Body.TraceFmt('skipped Writing central [%u] %s', [I, Rec.FileName],
          {_LINE_}2205, __UNIT__);
    Rec := TZMEntryWriter(Rec.Next);
    Inc(I);
  end;
  // finished Central
  if Result >= 0 then
  begin
    Result := WriteEOC;
    if Result >= 0 then
    begin
      Guage.AdvanceXtra(1);
      Result := Wrote + Result;
      if Result > 0 then
        Body.Trace('  finished ok', {_LINE_}2218, __UNIT__);
    end;
  end;
end;

procedure TZMEntryCopier.AssignFrom(const ARec: TZMEntryBase);
begin
  inherited;
end;

// process record, return bytes written; <0 = -error
function TZMEntryCopier.Process: Int64;
var
  Did: Int64;
  InRec: TZMEntryBase;
  InWorkFile: TZMZipBase;
  LOH: TZipLocalHeader;
  StNr: Integer;
  Stt: Int64;
  ToWrite: Int64;
  Wrt: Int64;
begin
  InRec := Link;
  InWorkFile := InRec.MyFile;
  if MyFile.Verbosity >= ZvVerbose then
  begin
    Body.ReportMsg(ZE_Copying, [Title]);
    Body.Trace('Copying local', {_LINE_}2245, __UNIT__);
  end;
  Result := InRec.SeekLocalData(LOH, InRec._FileName);
  if Result < 0 then
    Exit; // error
  StNr := MyFile.DiskNr;
  Stt := MyFile.Position;
  Result := WriteAsLocal1(ModifDateTime, CRC32);
  if Result < 0 then
    Exit; // error
  Wrt := Result;
  MyFile.Guage.Advance(Wrt);
  // ok so update positions
  RelOffLocalHdr := Stt;
  StartOnDisk := StNr;
  ToWrite := CompressedSize;
  MyFile.Guage.NewItem(ZP_Copying, ToWrite);
  Did := MyFile.CopyFrom(InWorkFile, ToWrite);
  if Did <> ToWrite then
  begin
    if Did < 0 then
      Result := Did // write error
    else
      Result := Body.PrepareErrMsg(ZE_DataCopy, [InWorkFile.Name(True)],
        {_LINE_}2269, __UNIT__);
    Exit;
  end;
  Wrt := Wrt + Did;
  if (Flag and FLAG_DATADESC_BIT) <> 0 then
  begin
    Did := WriteDataDesc;
    if Did < 0 then
    begin
      Result := Did; // error
      Exit;
    end;
    Wrt := Wrt + Did;
    MyFile.Guage.Advance(Did);
  end;
  MyFile.Guage.EndItem;
  Result := Wrt;
end;

// return bytes to be processed
function TZMEntryCopier.ProcessSize: Int64;
begin
  Result := CompressedSize + LocalSize;
  if HasDataDesc then
    Result := Result + Sizeof(TZipDataDescriptor);
end;

{ TZMZipCopier }

// Add a copy of source record if name is unique
function TZMZipCopier.AffixZippedFile(Rec: TZMEntryBase): TZMEntryCopier;
var
  Nrec: TZMEntryCopier;
begin
  Result := nil;
  if HasDupName(Rec) = nil then
  begin
    // accept it
    Nrec := TZMEntryCopier.Create(Self); // make a copy
    Nrec.AssignFrom(Rec);
    Nrec.Link := Rec; // link to original
    Add(Nrec);
    Result := Nrec;
  end;
end;

// return >=0 number added <0 error
function TZMZipCopier.AffixZippedFiles(Src: TZMZipReader; All: Boolean)
  : Integer;
var
  Node: TZMEntryBase;
  Nrec: TZMEntryWriter;
  Rec: TZMEntryBase;
begin
  Result := 0;
  Node := Src.FirstRec;
  while Node <> nil do
  begin
    Rec := Node;
    Node := Node.Next;
    if not Assigned(Rec) then
      Continue;
    if All or Rec.TestStatusBit(ZsbSelected) then
    begin
      if Verbosity > ZvVerbose then
        Body.TraceFmt('including: %s', [Rec.FileName], {_LINE_}2334, __UNIT__);
      Nrec := TZMEntryCopier.Create(Self); // make a copy
      Nrec.AssignFrom(Rec);
      Nrec.Link := Rec; // link to original
      AffixEntry(Nrec, True);
      Inc(Result);
    end
    else
      if Verbosity > ZvVerbose then
        Body.TraceFmt('ignoring: %s', [Rec.FileName], {_LINE_}2343, __UNIT__);
  end;
end;

function TZMZipCopier.WriteFile(InZip: TZMZipReader; All: Boolean): Int64;
begin
  ASSERT(Assigned(InZip), 'no input');
  Body.Trace('Write file', {_LINE_}2350, __UNIT__);
  Result := InZip.VerifyOpen; // verify unchanged and open
  if Result >= 0 then
  begin
    ZipComment := InZip.ZipComment;
    Result := AffixZippedFiles(InZip, All);
    if Result >= 0 then
      Result := Commit(ZwoZipTime in WriteOptions);
  end;
end;

function TZMEntryExt.GetHTFileName: string;
var
  I: Integer;
begin
  SetLength(Result, Length(_FileName));
  for I := 1 to Length(_FileName) do
    Result[I] := Char(Ord(_FileName[I]));
end;

function TZMEntryExt.HTIsSame(const AnEntry: TZMEntryBase): Boolean;
var
  FN: string;
  FnMe: string;
begin
  Result := False;
  if TestStatusBit(ZsbDiscard) then
    Exit;
  FN := AnEntry.HTFileName;
  FnMe := HTFileName;
  Result := (AnEntry.Hash = Hash) and (Length(FN) = Length(FnMe)) and
    (CompareText(FN, FnMe) = 0);
end;

function TZMEntryExt.HTIsSameStr(StrHash: Cardinal; const Str: string): Boolean;
var
  FN: string;
begin
  Result := False;
  if TestStatusBit(ZsbDiscard) then
    Exit;
  FN := HTFileName;
  Result := (StrHash = Hash) and (Length(Str) = Length(FN)) and
    (CompareText(Str, FN) = 0);
end;

function TZMEntryExt.Process: Int64;
begin
  Result := -1;
end;

// return bytes to be processed
function TZMEntryExt.ProcessSize: Int64;
begin
  Result := 0;
end;

end.
