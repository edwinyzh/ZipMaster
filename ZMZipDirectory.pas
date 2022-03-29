unit ZMZipDirectory;

// ZMZipReader.pas - Represents the readable Directory of a Zip file

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
// modified 2014-01-02

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZipMstr, ZMBody, ZMZipEOC, ZMStructs;

type
  TZMRecStrings = (ZrsName, ZrsComment, ZrsIName);
  TZMSelects = (ZzsClear, ZzsSet, ZzsToggle);
  TZMStrEncOpts = (ZseDOS, ZseXName, ZseXComment);
  TZMStrEncodes = set of TZMStrEncOpts;

  // ZipDirEntry status bit constants
const
  ZsbHashed = $100; // hash calculated
  ZsbLocalDone = $200; // local data prepared
  ZsbLocal64 = $400; // local header required zip64

  ZsbEncMask = $70000; // mask for bits holding how entry is encoded
  ZsbExtName = $200000; // filename has extended characters
  ZsbExtCmnt = $400000; // filename has extended characters

type
  TZCChanges = (ZccBegin, ZccAdd, ZccEnd);
  TZCChangeEvent = procedure(Sender: TObject; Idx: Integer; Change: TZCChanges)
    of object;

type
  TZMZipDirectory = class;
  TZMSelectArgs = class;

  TZMEntryBase = class
  private
    FBody: TZMBody;
    FExtIndex: Integer;
    FHash: Cardinal;
    FHTNext: TZMEntryBase;
    FMyFile: TZMZipDirectory;
    FNext: TZMEntryBase;
    FSelectArgs: TZMSelectArgs;
    FStatusBits: Cardinal;
    FXName: string;
    function GetDataString(Cmnt: Boolean): UTF8String;
    function GetHash: Cardinal;
    function GetIsDirOnly: Boolean;
    function GetSelected: Boolean;
    function GetStatus(Bit: Cardinal): Boolean;
    function GetStatusBit(Mask: Cardinal): Cardinal;
    function GetXName: string;
    procedure SetSelected(const Value: Boolean);
    procedure SetStatus(Bit: Cardinal; const Value: Boolean);
    function VerifyLocalName(const LocalHeader: TZipLocalHeader; const HName:
        TZMRawBytes): Integer;
  protected
    function FindDataTag(Tag: Word; var Idx, Siz: Integer): Boolean;
    function GetCompressedSize: Int64; virtual; abstract;
    function GetCompressionMethod: Word; virtual; abstract;
    function GetCRC32: Cardinal; virtual; abstract;
    function GetDateStamp: TDateTime;
    function GetDateTime: Cardinal; virtual; abstract;
    function GetDirty: Boolean;
    function GetEncoded: TZMEncodingOpts; virtual;
    function GetEncrypted: Boolean; virtual;
    function GetExtFileAttrib: Longword; virtual; abstract;
    function GetExtraData(Tag: Word): TZMRawBytes; virtual; abstract;
    function GetExtraField: TZMRawBytes; virtual; abstract;
    function GetExtraFieldLength: Word; virtual; abstract;
    function GetFileComment: string; virtual; abstract;
    function GetFileCommentLen: Word; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetFileNameLen: Word; virtual; abstract;
    function GetFlag: Word; virtual; abstract;
    function GetHTFileName: string; virtual;
    function GetIntFileAttrib: Word; virtual; abstract;
    function GetIsEncoded: TZMEncodingOpts; virtual;
    function GetMaster: TCustomZipMaster;
    function GetModifDateTime: Longword; virtual; abstract;
    function GetRelOffLocalHdr: Int64; virtual; abstract;
    function GetStartOnDisk: Cardinal; virtual; abstract;
    function GetStatusBits: Cardinal;
    function GetTitle: string; virtual;
    function GetUncompressedSize: Int64; virtual; abstract;
    function GetVersionMadeBy: Word; virtual; abstract;
    function GetVersionNeeded: Word; virtual; abstract;
    function Get_CompressedSize: Cardinal; virtual; abstract;
    function Get_FileComment: TZMRawBytes; virtual; abstract;
    function Get_FileName: TZMRawBytes; virtual; abstract;
    function Get_RelOffLocalHdr: Cardinal; virtual; abstract;
    function Get_StartOnDisk: Word; virtual; abstract;
    function Get_UncompressedSize: Cardinal; virtual; abstract;
    function IsZip64: Boolean;
    procedure MarkDirty;
    procedure SetIsEncoded(const Value: TZMEncodingOpts);
    property Master: TCustomZipMaster read GetMaster;
  public
    constructor Create(TheOwner: TZMZipDirectory);
    procedure AfterConstruction; override;
    procedure AssignFrom(const Zr: TZMEntryBase); virtual;
    procedure BeforeDestruction; override;
    function CentralSize: Cardinal;
    procedure ClearCachedName; virtual; abstract;
    function ClearStatusBit(const Values: Cardinal): Cardinal;
    function FetchNTFSTimes(var Times: TNTFS_Times): Integer;
    function HasChanges: Boolean;
    function HasDataDesc: Boolean;
    function HTIsSame(const AnEntry: TZMEntryBase): Boolean; virtual;
    function HTIsSameStr(StrHash: Cardinal; const Str: string)
      : Boolean; virtual;
    function SeekLocalData(var LocalHeader: TZipLocalHeader;
      const HName: TZMRawBytes): Integer;
    function Select(How: TZMSelects): Boolean;
    function SetStatusBit(const Value: Cardinal): Cardinal;
    function TestStatusBit(const Mask: Cardinal): Boolean;
    property Body: TZMBody read FBody;
    property CompressedSize: Int64 read GetCompressedSize;
    property CompressionMethod: Word read GetCompressionMethod;
    property CRC32: Cardinal read GetCRC32;
    property DateStamp: TDateTime read GetDateStamp;
    property DateTime: Cardinal read GetDateTime;
    property Encoded: TZMEncodingOpts read GetEncoded;
    property Encrypted: Boolean read GetEncrypted;
    property ExtFileAttrib: Longword read GetExtFileAttrib;
    property ExtIndex: Integer read FExtIndex write FExtIndex;
    property ExtraField: TZMRawBytes read GetExtraField;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComment: string read GetFileComment;
    property FileCommentLen: Word read GetFileCommentLen;
    property FileName: string read GetFileName;
    property FileNameLen: Word read GetFileNameLen;
    property Flag: Word read GetFlag;
    property Hash: Cardinal read GetHash;
    property HTFileName: string read GetHTFileName;
    property HTNext: TZMEntryBase read FHTNext write FHTNext;
    property IntFileAttrib: Word read GetIntFileAttrib;
    property IsDirOnly: Boolean read GetIsDirOnly;
    property IsEncoded: TZMEncodingOpts read GetIsEncoded write SetIsEncoded;
    property ModifDateTime: Longword read GetModifDateTime;
    property MyFile: TZMZipDirectory read FMyFile write FMyFile;
    property Next: TZMEntryBase read FNext write FNext;
    property RelOffLocalHdr: Int64 read GetRelOffLocalHdr;
    property SelectArgs: TZMSelectArgs read FSelectArgs write FSelectArgs;
    property Selected: Boolean read GetSelected write SetSelected;
    property StartOnDisk: Cardinal read GetStartOnDisk;
    property Status[Bit: Cardinal]: Boolean read GetStatus write SetStatus;
    property StatusBit[Mask: Cardinal]: Cardinal read GetStatusBit;
    property StatusBits: Cardinal read FStatusBits write FStatusBits;
    property Title: string read GetTitle;
    property UncompressedSize: Int64 read GetUncompressedSize;
    property VersionMadeBy: Word read GetVersionMadeBy;
    property VersionNeeded: Word read GetVersionNeeded;
    property XName: string read GetXName write FXName;
    property _CompressedSize: Cardinal read Get_CompressedSize;
    // prefix '_' header properties that are overriden
    property _FileComment: TZMRawBytes read Get_FileComment;
    property _FileName: TZMRawBytes read Get_FileName;
    property _RelOffLocalHdr: Cardinal read Get_RelOffLocalHdr;
    property _StartOnDisk: Word read Get_StartOnDisk;
    property _UncompressedSize: Cardinal read Get_UncompressedSize;
  end;

  TZMSelectArgs = class
  private
    Cnts: Integer;
    FNext: TZMSelectArgs;
  public
    function Accept(Rec: TZMEntryBase): Boolean; virtual; abstract;
    procedure AfterConstruction; override;
    procedure Assign(Other: TZMSelectArgs); virtual;
    property Next: TZMSelectArgs read FNext write FNext;
  end;

  TZMZipDirectory = class(TZMZipEOC)
  private
    FArgsList: TZMSelectArgs;
    FCount: Integer;
    FFirstRec: TZMEntryBase;
    FHTCount: Integer;
    FHTSize: Integer;
    FLastRec: TZMEntryBase;
    FOnChange: TZCChangeEvent;
    FOpenRet: Integer;
    FSelCount: Integer;
    FSFXOfs: Cardinal;
    FSOCOfs: Int64;
    FStub: TMemoryStream;
    FUseSFX: Boolean;
    function GetFirstSelected: TZMEntryBase;
    procedure LoadEntries;
    procedure SetHTSize(Value: Integer);
    procedure SetStub(const Value: TMemoryStream);
  protected
    FHashTable: array of TZMEntryBase;
    procedure ClearEntries;
    function HTFind(const FileName: string): TZMEntryBase;
    procedure InferNumbering;
    procedure MarkDirty;
    procedure RemoveEntry(Entry: TZMEntryBase);
    function SelectEntry(Rec: TZMEntryBase; How: TZMSelects): Boolean;
    property HTSize: Integer read FHTSize write SetHTSize;
  public
    function Add(Rec: TZMEntryBase): Integer;
    function AddSelectArgs(Args: TZMSelectArgs): TZMSelectArgs;
    procedure AfterConstruction; override;
    procedure AssignStub(From: TZMZipDirectory);
    procedure BeforeDestruction; override;
    procedure ClearArgsList;
    procedure ClearCachedNames;
    procedure ClearSelection;
    function FindName(const Name: string; const NotMe: TZMEntryBase = nil)
      : TZMEntryBase;
    procedure FreeSelectArgs(Args: TZMSelectArgs);
    function HasDupName(const Rec: TZMEntryBase): TZMEntryBase;
    function HTAdd(AnEntry: TZMEntryBase; AllowDuplicate: Boolean)
      : TZMEntryBase;
    procedure HTAutoSize(Req: Cardinal);
    procedure HTClear;
    function HTFirstDup(const AnEntry: TZMEntryBase): TZMEntryBase;
    function HTNextDup(const AnEntry: TZMEntryBase): TZMEntryBase;
    procedure HTRemove(AnEntry: TZMEntryBase);
    function InsertAfter(NewRec, ARec: TZMEntryBase): Integer;
    // 1 Mark as Contents Invalid
    procedure Invalidate;
    function LoadZip: Integer;
    function NextSelected(CurRec: TZMEntryBase): TZMEntryBase;
    function RecAtN(N: Integer): TZMEntryBase;
    function SearchName(const Pattern: string; IsWild: Boolean; After: Integer)
      : TZMEntryBase;
    function SearchNameEx(const Pattern: string; IsWild: Boolean;
      After: TZMEntryBase): TZMEntryBase;
    function Select(const Pattern: string; How: TZMSelects): Integer;
    function SelectFile(const Pattern, Reject: string; How: TZMSelects)
      : Integer;
    function SelectFiles(const Want, Reject: TStrings): Integer;
    function SelectRec(const Pattern, Reject: string; How: TZMSelects;
      SelArgs: TZMSelectArgs): Integer;
    property ArgsList: TZMSelectArgs read FArgsList;
    property Count: Integer read FCount;
    property FirstRec: TZMEntryBase read FFirstRec write FFirstRec;
    property FirstSelected: TZMEntryBase read GetFirstSelected;
    property HTCount: Integer read FHTCount;
    property LastRec: TZMEntryBase read FLastRec;
    property OpenRet: Integer read FOpenRet write FOpenRet;
    property SelCount: Integer read FSelCount write FSelCount;
    property SFXOfs: Cardinal read FSFXOfs write FSFXOfs;
    property SOCOfs: Int64 read FSOCOfs write FSOCOfs;
    property Stub: TMemoryStream read FStub write SetStub;
    property UseSFX: Boolean read FUseSFX write FUseSFX;
    property OnChange: TZCChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils, {$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMCore, ZMZipBase, ZMMsg, ZMXcpt, ZMUtils, ZMMatch, ZMWinFuncs,
  ZMEntryReader, ZMUTF8, ZMCRC;

const
  __UNIT__ = 43;

const
  HTChainsMax = 65537;
  HTChainsMin = 61;

const
  AllSpec: string = '*.*';
  AnySpec: string = '*';

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

type
  Txdat64 = packed record
    Tag: Word;
    Siz: Word;
    Vals: array [0 .. 4] of Int64; // last only cardinal
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

// returns index
function TZMZipDirectory.Add(Rec: TZMEntryBase): Integer;
begin
  if FirstRec = nil then
  begin
    // will be first
    FFirstRec := Rec;
    Rec.Next := nil;
    Rec.ExtIndex := 0;
    FCount := 1;
    FLastRec := Rec;
    Result := 0;
  end
  else
  begin
    Assert(LastRec <> nil, 'invalid last pointer');
    LastRec.Next := Rec;
    FLastRec := Rec;
    Rec.ExtIndex := FCount;
    Result := FCount;
    Inc(FCount);
  end;
end;

function TZMZipDirectory.AddSelectArgs(Args: TZMSelectArgs): TZMSelectArgs;
begin
  Result := Args;
  if Args = nil then
    Exit;
  Args.Next := FArgsList;
  FArgsList := Args; // chain it
end;

procedure TZMZipDirectory.AfterConstruction;
begin
  inherited;
  FCount := 0;
  FFirstRec := nil;
  FLastRec := nil;
end;

procedure TZMZipDirectory.AssignStub(From: TZMZipDirectory);
begin
  FreeAndNil(FStub);
  FStub := From.Stub;
  From.FStub := nil;
end;

procedure TZMZipDirectory.BeforeDestruction;
begin
  ClearEntries;
  ClearArgsList;
  FreeAndNil(FStub);
  inherited;
end;

procedure TZMZipDirectory.ClearArgsList;
var
  Node: TZMEntryBase;
  Tmp: TZMSelectArgs;
begin
  Node := FirstRec;
  while Node <> nil do
  begin
    Node.SelectArgs := nil; // clear all
    Node := Node.Next;
  end;
  // delete list
  while FArgsList <> nil do
  begin
    Tmp := FArgsList;
    FArgsList := Tmp.Next;
    Tmp.Free;
  end;
end;

procedure TZMZipDirectory.ClearCachedNames;
var
  Node: TZMEntryBase;
begin
  HTClear;
  Node := FirstRec;
  while Node <> nil do
  begin
    Node.ClearCachedName;
    Node := Node.Next;
  end;
end;

procedure TZMZipDirectory.ClearEntries;
var
  Node: TZMEntryBase;
  Tmp: TObject;
begin
  HTClear;
  Node := FirstRec;
  while Node <> nil do
  begin
    Tmp := Node;
    Node := Node.Next;
    Tmp.Free;
  end;
  FFirstRec := nil;
  FLastRec := nil;
  FCount := 0;
  FSelCount := 0;
end;

procedure TZMZipDirectory.ClearSelection;
var
  Node: TZMEntryBase;
begin
  Node := FirstRec;
  while Node <> nil do
  begin
    Node.Selected := False;
    Node := Node.Next;
  end;
  FSelCount := 0;
end;

function TZMZipDirectory.FindName(const Name: string;
  const NotMe: TZMEntryBase = nil): TZMEntryBase;
var
  Hash: Cardinal;
begin
  if HTSize > 0 then
  begin
    Result := HTFind(Name);
    while (Result <> nil) do
    begin
      if (Result <> NotMe) and (Result.StatusBit[ZsbDiscard] = 0) then
        Break;
      Result := HTNextDup(Result);
    end;
  end
  else
  begin
    Hash := HashFuncNoCase(Name);

    Result := FirstRec; // from beginning
    while Result <> nil do
    begin
      if (Result <> NotMe) and (Result.StatusBit[ZsbDiscard] = 0) and
        (Result.Hash = Hash) and FileNameMatch(Name, Result.FileName) then
        Break;
      Result := Result.Next;
    end;
  end;
end;

procedure TZMZipDirectory.FreeSelectArgs(Args: TZMSelectArgs);
var
  Arg: TZMSelectArgs;
begin
  if Args = nil then
    Exit;
  Arg := ArgsList;
  if Arg = Args then
  begin
    // first entry
    FArgsList := Arg.Next; // remove from list
    Arg.Free;
  end
  else
  begin
    // find it
    while (Arg <> nil) and (Arg.Next <> Args) do
      Arg := Arg.Next;
    if Arg <> nil then
    begin
      // found
      Arg.Next := Args.Next; // remove from list
      Arg.Free;
    end;
  end;
end;

function TZMZipDirectory.GetFirstSelected: TZMEntryBase;
begin
  Result := FirstRec;
  if (Result <> nil) and (Result.StatusBit[ZsbSkipped or ZsbSelected] <>
    ZsbSelected) then
    Result := NextSelected(Result);
end;

// searches for record with same Name
function TZMZipDirectory.HasDupName(const Rec: TZMEntryBase): TZMEntryBase;
var
  Name: string;
  Hash: Cardinal;
begin
  Result := nil;
  if Rec <> nil then
  begin
    Name := Rec.FileName;
    Hash := HashFuncNoCase(Name);
    Result := FirstRec; // from beginning
    while Result <> nil do
    begin
      if (Result <> Rec) and (Result.Hash = Hash) and
        FileNameMatch(Name, Result.FileName) then
        Break;
      Result := Result.Next;
    end;
  end;
end;

// returns existing entry (if any) or nil
function TZMZipDirectory.HTAdd(AnEntry: TZMEntryBase; AllowDuplicate: Boolean)
  : TZMEntryBase;
var
  Entry: TZMEntryBase;
  Hash: Cardinal;
  Idx: Integer;
  Smaller: TZMEntryBase;
begin
  Assert(AnEntry <> nil, 'nil ZipDirEntry');
  if FHashTable = nil then
    HTSize := 1283;
  Result := nil;
  Hash := AnEntry.Hash;
  Idx := Hash mod Cardinal(Length(FHashTable));
  Entry := FHashTable[Idx];
  if Entry = nil then
  begin
    AnEntry.HTNext := nil; // only one in linked-list
    FHashTable[Idx] := AnEntry;
    Inc(FHTCount);
  end
  else
  begin
    // search linked-list
    Smaller := nil;
    // Parent := Entry; // keep compiler happy
    while Entry <> nil do
    begin
      if Entry.HTIsSame(AnEntry) then
      begin
        if not AllowDuplicate then
        begin
          Result := Entry;
          Exit;
        end;
        if Result = nil then
          Result := Entry; // catch first
        if Entry = AnEntry then
        begin
          Result := nil;
          Exit; // already added
        end;
      end;
      // step to next
      if Entry.ExtIndex < AnEntry.ExtIndex then
        Smaller := Entry; // remember last smaller index
      Entry := Entry.HTNext;
    end;
    if Smaller = nil then
    begin
      // new first node
      AnEntry.HTNext := FHashTable[Idx];
      FHashTable[Idx] := AnEntry;
    end
    else
    begin
      // insert into list after smaller
      AnEntry.HTNext := Smaller.HTNext;
      Smaller.HTNext := AnEntry;
    end;
    Inc(FHTCount);
  end;
end;

// set size to a reasonable prime number
procedure TZMZipDirectory.HTAutoSize(Req: Cardinal);
const
  PrimeSizes: array [0 .. 29] of Cardinal = (61, 131, 257, 389, 521, 641, 769,
    1031, 1283, 1543, 2053, 2579, 3593, 4099, 5147, 6151, 7177, 8209, 10243,
    12289, 14341, 16411, 18433, 20483, 22521, 24593, 28687, 32771,
    40961, 65537);
var
  I: Integer;
begin
  if Req < 12000 then
  begin
    // use next higher size
    for I := 0 to high(PrimeSizes) do
      if PrimeSizes[I] >= Req then
      begin
        Req := PrimeSizes[I];
        Break;
      end;
  end
  else
  begin
    // use highest smaller size
    for I := high(PrimeSizes) downto 0 do
      if PrimeSizes[I] < Req then
      begin
        Req := PrimeSizes[I];
        Break;
      end;
  end;
  SetHTSize(Req);
end;

procedure TZMZipDirectory.HTClear;
begin
  FHashTable := nil;
  FHTSize := 0;
  FHTCount := 0;
end;

// not needed for HeaderName ?
function TZMZipDirectory.HTFind(const FileName: string): TZMEntryBase;
var
  Hash: Cardinal;
  Idx: Cardinal;
begin
  if FHashTable = nil then
    HTAutoSize(Count);
  Hash := HashFuncNoCase(FileName);
  Idx := Hash mod Cardinal(Length(FHashTable));
  Result := FHashTable[Idx];
  // check entries in this chain
  while Result <> nil do
  begin
    if Result.HTIsSameStr(Hash, FileName) then
      Break; // exists
    Result := Result.HTNext;
  end;
end;

// finds the first duplicate in chain
// assumes HashTable filled and links are valid
function TZMZipDirectory.HTFirstDup(const AnEntry: TZMEntryBase): TZMEntryBase;
var
  Hash: Cardinal;
  Idx: Cardinal;
begin
  Hash := AnEntry.Hash;
  Idx := Hash mod Cardinal(Length(FHashTable));
  Result := FHashTable[Idx]; // first entry in linked-list
  // check entries in this chain
  while Result <> nil do
  begin
    if Result.HTIsSame(AnEntry) then
      Break; // exists
    Result := Result.HTNext;
  end;
end;

// find next duplicate in linked-list
// assumes HashTable filled and links are valid
function TZMZipDirectory.HTNextDup(const AnEntry: TZMEntryBase): TZMEntryBase;
begin
  Result := AnEntry.HTNext; // next entry in linked-list
  // check entries in this chain
  while Result <> nil do
  begin
    if Result.HTIsSame(AnEntry) then
      Break; // exists
    Result := Result.HTNext;
  end;
end;

procedure TZMZipDirectory.HTRemove(AnEntry: TZMEntryBase);
var
  Entry: TZMEntryBase;
  Hash: Cardinal;
  Idx: Cardinal;
  Prev: TZMEntryBase;
begin
  Prev := nil;
  if FHashTable = nil then
    Exit; // nothing to delete from
  Hash := AnEntry.Hash;
  Idx := Hash mod Cardinal(Length(FHashTable));
  Entry := FHashTable[Idx]; // first entry in linked-list
  // check entries in this chain
  while (Entry <> nil) and (Entry <> AnEntry) do
  begin
    Prev := Entry;
    Entry := Entry.HTNext;
  end;
  if Entry <> nil then
  begin
    // found
    if Prev = nil then
      FHashTable[Idx] := Entry.HTNext // is first
    else
      Prev.HTNext := Entry.HTNext; // remove from chain
    Entry.HTNext := nil;
    Dec(FHTCount);
  end;
end;

// Use after EOC found and FileName is last part
// if removable has proper numbered volume name we assume it is numbered volume
procedure TZMZipDirectory.InferNumbering;
var
  Fname: string;
  Num: Integer;
  NumStr: string;
begin
  if IsExtStream then
    Numbering := ZnsNone
  else
    if (Numbering = ZnsNone) and (TotalDisks > 1) then
    begin
      // only if unknown
      WorkDrive.DriveStr := ArchiveName;
      if WorkDrive.DriveIsFloppy and SameText(WorkDrive.DiskName,
        VolName(DiskNr)) then
        Numbering := ZnsVolume
      else
      begin
        NumStr := '';
        Fname := ExtractNameOfFile(ArchiveName);
        Numbering := ZnsExt;
        if Length(Fname) > 3 then
        begin
          NumStr := Copy(Fname, Length(Fname) - 2, 3);
          Num := StrToIntDef(NumStr, -1);
          if Num = (DiskNr + 1) then
          begin
            // ambiguous conflict
            if not HasSpanSig(ExtractFilePath(ArchiveName) + Fname + '.z01')
            then
              Numbering := ZnsName;
          end;
        end;
      end;
    end;
end;

function TZMZipDirectory.InsertAfter(NewRec, ARec: TZMEntryBase): Integer;
begin
  Assert(NewRec <> nil, 'Tried to add empty TZMEntryBase');
  Assert(NewRec.MyFile <> nil, 'Owned by different list');
  if ARec = nil then
    ARec := FirstRec;
  if ARec = nil then
  begin
    FFirstRec := NewRec;
    FLastRec := FirstRec;
    LastRec.Next := nil;
    FCount := 1;
  end
  else
  begin
    Assert(ARec.MyFile = Self, 'Tried to add to different list');
    NewRec.Next := ARec.Next;
    if LastRec = ARec then
      FLastRec := NewRec; // new end
    ARec.Next := NewRec;
    Inc(FCount);
  end;
  NewRec.MyFile := Self;
  Result := Count;
end;

procedure TZMZipDirectory.Invalidate;
begin
  Info := Info or Zfi_Invalid;
end;

function TZMZipDirectory.LoadZip: Integer;
var
  LiE: Integer;
  OffsetDiff: Int64;
  Sgn: Cardinal;
begin
  if not IsOpen then
  begin
    Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
      {_LINE_}884, __UNIT__);
    Exit;
  end;
  Result := ZM_Error({_LINE_}887, ZE_UnknownError);
  if (Info and Zfi_EOC) = 0 then
    Exit; // should not get here if eoc has not been read
  LiE := 1;
  OffsetDiff := 0;
  ClearEntries;
  if Assigned(OnChange) then
    OnChange(Self, 0, ZccBegin);
  SOCOfs := CentralOffset;
  try
    OffsetDiff := CentralOffset;
    // Do we have to request for a previous disk first?
    if DiskNr <> CentralDiskNo then
    begin
      SeekDisk(CentralDiskNo, False);
      File_Size := Seek(0, SoEnd);
    end
    else
      if not Z64 then
      begin
        // Due to the fact that v1.3 and v1.4x programs do not change the archives
        // EOC and CEH records in case of a SFX conversion (and back) we have to
        // make this extra check.
        OffsetDiff := File_Size -
          (Integer(CentralSize) + SizeOf(TZipEndOfCentral) + ZipCommentLen);
      end;
    SOCOfs := OffsetDiff;
    // save the location of the Start Of Central dir
    SFXOfs := Cardinal(OffsetDiff);
    if SFXOfs <> SOCOfs then
      SFXOfs := 0;
    // initialize this - we will reduce it later
    if File_Size = 22 then
      SFXOfs := 0;

    if CentralOffset <> OffsetDiff then
    begin
      // We need this in the ConvertXxx functions.
      ShowErrorEx(ZW_WrongZipStruct, {_LINE_}925, __UNIT__);
      CheckSeek(CentralOffset, SoBeginning,
        ZM_Error({_LINE_}927, ZE_ReadZipError));
      CheckRead(Sgn, 4, ZM_Error({_LINE_}928, ZE_CEHBadRead));
      if Sgn = CentralFileHeaderSig then
      begin
        SOCOfs := CentralOffset;
        // TODO warn - central size error
      end;
    end;

    // Now we can go to the start of the Central directory.
    CheckSeek(SOCOfs, SoBeginning, ZM_Error({_LINE_}937, ZE_ReadZipError));
    Guage.NewItem(ZP_Loading, TotalEntries);
    // Read every entry: The central header and save the information.
{$IFDEF DEBUG}
    Body.TraceFmt('List - expecting %d files', [TotalEntries],
      {_LINE_}942, __UNIT__);
{$ENDIF}
    LoadEntries;
    LiE := 0; // finished ok
    Result := 0;
    Info := (Info and not(Zfi_MakeMask)) or Zfi_Loaded;
  finally
    Guage.EndBatch;
    if LiE = 1 then
    begin
      ArchiveName := '';
      SFXOfs := 0;
      File_Close;
    end
    else
    begin
      CentralOffset := SOCOfs; // corrected
      // Correct the offset for v1.3 and 1.4x
      SFXOfs := SFXOfs + Cardinal(OffsetDiff - CentralOffset);
    end;

    // Let the user's program know we just refreshed the zip dir contents.
    if Assigned(OnChange) then
      OnChange(Self, Count, ZccEnd);
  end;
end;

procedure TZMZipDirectory.LoadEntries;
var
  I: Integer;
  R: Integer;
begin
  for I := 0 to (TotalEntries - 1) do
  begin
    if LastRec = nil then
    begin
      FFirstRec := TZMEntryReader.Create(Self); // is first
      FLastRec := FirstRec;
    end
    else
    begin
      LastRec.Next := TZMEntryReader.Create(Self); // append
      FLastRec := LastRec.Next;
    end;

    R := TZMEntryReader(LastRec).Read;
    if R < 0 then
      raise EZipMaster.CreateMsg(Body, R, 0, 0);
    if R > 0 then
      Z64 := True;
{$IFDEF DEBUG}
    if Verbosity >= ZvTrace then
      Body.TraceFmt('List - [%d] "%s"', [I, LastRec.FileName], {_LINE}995,
        __UNIT__);
{$ENDIF}
    Inc(FCount);
    // Notify user, when needed, of the NextSelected entry in the ZipDir.
    if Assigned(OnChange) then
      OnChange(Self, I, ZccAdd); // change event to give TZipDirEntry

    // Calculate the earliest Local Header start
    if SFXOfs > LastRec.RelOffLocalHdr then
      SFXOfs := LastRec.RelOffLocalHdr;
    Guage.Advance(1);
    CheckCancel;
  end; // for;
end;

procedure TZMZipDirectory.MarkDirty;
begin
  Info := Info or Zfi_Dirty;
end;

// return BadIndex when no more
function TZMZipDirectory.NextSelected(CurRec: TZMEntryBase): TZMEntryBase;
begin
  Result := CurRec.Next;
  while Result <> nil do
  begin
    if Result.StatusBit[ZsbSkipped or ZsbSelected] = ZsbSelected then
      Break;
    Result := Result.Next;
  end;
end;

function TZMZipDirectory.RecAtN(N: Integer): TZMEntryBase;
begin
  Result := FirstRec;
  while (Result <> nil) and (N > 0) do
  begin
    Result := Result.Next;
    Dec(N);
  end;
end;

procedure TZMZipDirectory.RemoveEntry(Entry: TZMEntryBase);
var
  N: Integer;
  Parent: TZMEntryBase;
  Rec: TZMEntryBase;
begin
  Parent := nil;
  Rec := FirstRec;
  while (Rec <> Entry) and (Rec <> nil) do
  begin
    Parent := Rec;
    Rec := Rec.Next;
  end;
  if Rec <> nil then
  begin
    // found
    HTRemove(Entry);
    if Parent <> nil then
      Parent.Next := Entry.Next
    else
      FirstRec := Entry.Next;
    Entry.Next := nil;
    Entry.Free;
    // renumber
    N := 0;
    if Parent <> nil then
    begin
      Rec := Parent.Next;
      N := Parent.ExtIndex + 1;
    end
    else
      Rec := FirstRec;
    while Rec <> nil do
    begin
      Rec.ExtIndex := N;
      Inc(N);
      Rec := Rec.Next;
    end;
    FCount := N;
  end;
end;

function TZMZipDirectory.SearchName(const Pattern: string; IsWild: Boolean;
  After: Integer): TZMEntryBase;
var
  Hash: Cardinal;
begin
  Result := nil;
  Hash := 0; // keep compiler happy
  if (Pattern <> '') then
  begin
    if not IsWild then
    begin
      if HTSize > 0 then
      begin
        Result := HTFind(Pattern);
        while (Result <> nil) do
        begin
          if (not Result.Status[ZsbDiscard]) and (Result.ExtIndex > After) then
            Break;
          Result := HTNextDup(Result);
        end;
        Exit;
      end;
      Hash := HashFuncNoCase(Pattern);
    end;
    Result := FirstRec; // from beginning
    while Result <> nil do
    begin
      if (Result.ExtIndex > After) and (Result.StatusBit[ZsbDiscard] = 0) and
        (IsWild or (Result.Hash = Hash)) and
        FileNameMatch(Pattern, Result.FileName) then
        Break;
      Result := Result.Next;
    end;
  end;
end;

function TZMZipDirectory.SearchNameEx(const Pattern: string; IsWild: Boolean;
  After: TZMEntryBase): TZMEntryBase;
var
  Hash: Cardinal;
begin
  Result := nil;
  Hash := 0; // keep compiler happy
  if (Pattern <> '') then
  begin
    if not IsWild then
    begin
      if HTSize > 0 then
      begin
        if After <> nil then
          Result := HTNextDup(After)
        else
          Result := HTFind(Pattern);
        while (Result <> nil) do
        begin
          if (Result.StatusBit[ZsbDiscard] = 0) then
            Break;
          Result := HTNextDup(Result);
        end;
        Exit;
      end;
      Hash := HashFuncNoCase(Pattern);
    end;
    if After = nil then
      Result := FirstRec // from beginning
    else
      Result := After.Next; // start at next
    while Result <> nil do
    begin
      if (Result.StatusBit[ZsbDiscard] = 0) and (IsWild or (Result.Hash = Hash))
        and FileNameMatch(Pattern, Result.FileName) then
        Break;
      Result := Result.Next;
    end;
  end;
end;

// select entries matching external Pattern - return number of selected entries
function TZMZipDirectory.Select(const Pattern: string; How: TZMSelects): Integer;
var
  Srch: Integer;
  ARec: TZMEntryBase;
  Wild: Boolean;
begin
  Result := 0;
  // if it wild or multiple we must try to match - else only if same hash
  Wild := not CanHash(Pattern);
  if (Pattern = '') or (Wild and ((Pattern = AllSpec) or (Pattern = AnySpec)))
  then
  begin
    ARec := FirstRec;
    while ARec <> nil do
    begin
      if SelectEntry(ARec, How) then
        Inc(Result);
      ARec := ARec.Next;
    end;
  end
  else
  begin
    // select specific Pattern
    Srch := 1;
    ARec := nil; // from beginning
    while Srch <> 0 do
    begin
      ARec := SearchNameEx(Pattern, Wild, ARec);
      if ARec = nil then
        Break;
      if SelectEntry(ARec, How) then
        Inc(Result);
      if Srch > 0 then
      begin
        if Wild then
          Srch := -1 // search all
        else
          Srch := 0; // done
      end;
    end;
  end;
end;

function TZMZipDirectory.SelectEntry(Rec: TZMEntryBase; How: TZMSelects): Boolean;
begin
  Result := Rec.Select(How);
  if Result then
    Inc(FSelCount)
  else
    Dec(FSelCount);
end;

// SelectFile entries matching external Pattern
function TZMZipDirectory.SelectFile(const Pattern, Reject: string;
  How: TZMSelects): Integer;
var
  Exc: string;
  Ptn: string;
  ARec: TZMEntryBase;
  Wild: Boolean;
begin
  Result := 0;
  Exc := Reject; // default excludes
  Ptn := Pattern; // need to remove switches
  // split Pattern into Pattern and switches
  // if it wild or multiple we must try to match - else only if same hash
  Wild := not CanHash(Ptn);
  if (Ptn = '') or (Wild and ((Ptn = AllSpec) or (Ptn = AnySpec))) then
  begin
    // do all
    ARec := FirstRec;
    while ARec <> nil do
    begin
      if (Exc = '') or not FileNameMatch(Exc, ARec.FileName) then
      begin
        SelectEntry(ARec, How);
        Inc(Result);
      end;
      ARec := ARec.Next;
    end;
  end
  else
  begin
    // SelectFile specific Pattern
    ARec := nil; // from beginning
    while True do
    begin
      ARec := SearchNameEx(Ptn, Wild, ARec);
      if ARec = nil then
        Break; // no matches
      if (Exc = '') or not FileNameMatch(Exc, ARec.FileName) then
      begin
        SelectEntry(ARec, How);
        Inc(Result);
      end;
      if not Wild then
        Break; // old find first
    end;
  end;
end;

function TZMZipDirectory.SelectFiles(const Want, Reject: TStrings): Integer;
var
  A: Integer;
  MissedSpecs: string;
  Excludes: string;
  I: Integer;
  NoSelected: Integer;
  Spec: string;
begin
  Result := 0;
  ClearSelection; // clear all
  if (Want.Count < 1) or (Count < 1) then
    Exit;
  MissedSpecs := '';
  Excludes := '';
  // combine rejects into a string
  if (Reject <> nil) and (Reject.Count > 0) then
  begin
    Excludes := Reject[0];
    for I := 1 to Reject.Count - 1 do
      Excludes := Excludes + ZSwitchFollows + Reject[I];
  end;
  // attempt to select each wanted spec
  for A := 0 to Want.Count - 1 do
  begin
    Spec := Want[A];
    NoSelected := SelectFile(Spec, Excludes, ZzsSet);
    if NoSelected < 1 then
    begin
      // none found
      if MissedSpecs <> '' then
        MissedSpecs := MissedSpecs + '|';
      MissedSpecs := MissedSpecs + Spec;
      if IsTrace then
        Body.Inform('Skipped filespec ' + Spec, {_LINE_}1718, __UNIT__);
      if Skipping(Spec, StNotFound, ZE_NoneSelected) then
      begin
        Result := -1;
        Break; // error
      end;
    end;
    if NoSelected > 0 then
      Result := Result + NoSelected;
    if NoSelected >= Count then
      Break; // all have been done
  end;
  if Result <= 0 then
  begin
    if Excludes <> '' then
      MissedSpecs := MissedSpecs + ' /E:' + Excludes;
    Result := Body.PrepareErrMsg(ZE_NoneSelected, [MissedSpecs], {_LINE_}1734,
      __UNIT__);
  end;
end;

// Func returns True to accept file
function TZMZipDirectory.SelectRec(const Pattern, Reject: string; How: TZMSelects;
  SelArgs: TZMSelectArgs): Integer;
var
  Exc: string;
  Ptn: string;
  ARec: TZMEntryBase;
  IsSelected: Boolean;
  Wild: Boolean;
begin
  Assert(Assigned(SelArgs), 'SelArgs not set');
  Result := 0;
  Exc := Reject; // default excludes
  Ptn := Pattern; // need to remove switches
  // split Pattern into Pattern and switches
  // if it wild or multiple we must try to match - else only if same hash
  Wild := not CanHash(Ptn);
  if (Ptn = '') or (Wild and ((Ptn = AllSpec) or (Ptn = AnySpec))) then
  begin
    // do all
    ARec := FirstRec;
    while ARec <> nil do
    begin
      if (Exc = '') or not FileNameMatch(Exc, ARec.FileName) then
      begin
        // do extra checking or processing
        if (How <> ZzsSet) or SelArgs.Accept(ARec) then
        begin
          IsSelected := SelectEntry(ARec, How);
          if IsSelected then
            ARec.SelectArgs := SelArgs
          else
            ARec.SelectArgs := nil;
          Inc(Result);
        end;
      end;
      ARec := ARec.Next;
    end;
  end
  else
  begin
    // Select specific Pattern
    ARec := nil; // from beginning
    while True do
    begin
      ARec := SearchNameEx(Ptn, Wild, ARec);
      if ARec = nil then
        Break; // no matches
      if (Exc = '') or not FileNameMatch(Exc, ARec.FileName) then
      begin
        // do extra checking or processing
        if (How <> ZzsSet) or SelArgs.Accept(ARec) then
        begin
          IsSelected := SelectEntry(ARec, How);
          if IsSelected then
            ARec.SelectArgs := SelArgs
          else
            ARec.SelectArgs := nil;
          Inc(Result);
        end;
      end;
      if not Wild then
        Break; // old find first
    end;
  end;
end;

procedure TZMZipDirectory.SetHTSize(Value: Integer);
var
  TableSize: Integer;
begin
  HTClear;
  TableSize := Value;
  // keep within reasonable limits
  if TableSize < HTChainsMin then
    TableSize := HTChainsMin
  else
    if TableSize > HTChainsMax then
      TableSize := HTChainsMax;
  SetLength(FHashTable, TableSize);
  ZeroMemory(FHashTable, TableSize * Sizeof(TZMEntryBase));

  FHTSize := Value;
  FHTCount := 0;
  Body.TraceFmt('Made Hash table size = %d', [TableSize], {_LINE_}1823,
    __UNIT__);
end;

procedure TZMZipDirectory.SetStub(const Value: TMemoryStream);
begin
  if FStub <> Value then
  begin
    if Assigned(FStub) then
      FStub.Free;
    FStub := Value;
  end;
end;

{ TZMEntryBase }
constructor TZMEntryBase.Create(TheOwner: TZMZipDirectory);
begin
  inherited Create;
  FMyFile := TheOwner;
  FBody := TheOwner.Body;
end;

procedure TZMEntryBase.AfterConstruction;
begin
  inherited;
  FStatusBits := 0;
  FSelectArgs := nil; // never owned by Entry
  FXName := '';
end;

procedure TZMEntryBase.AssignFrom(const Zr: TZMEntryBase);
begin
  if (Zr <> Self) and (Zr is TZMEntryBase) then
  begin
    FHash := Zr.FHash;
    FSelectArgs := Zr.SelectArgs;
    FStatusBits := Zr.StatusBits;
    FXName := Zr.FXName;
    FMyFile := Zr.MyFile;
    FHTNext := nil;
    FNext := nil;
  end;
end;

procedure TZMEntryBase.BeforeDestruction;
begin
  FSelectArgs := nil; // release if was used
  inherited;
end;

function TZMEntryBase.CentralSize: Cardinal;
begin
  Result := SizeOf(TZipCentralHeader);
  Inc(Result, FileNameLen + ExtraFieldLength + FileCommentLen);
end;

function TZMEntryBase.ClearStatusBit(const Values: Cardinal): Cardinal;
begin
  Result := StatusBits and Values;
  StatusBits := StatusBits and not Values;
end;

function TZMEntryBase.FetchNTFSTimes(var Times: TNTFS_Times): Integer;
var
  Subtags: PByte;
  Stag: PByte;
  Sz: Integer;
  X: TZMRawBytes;
begin
  Result := 0;
  if ExtraFieldLength >= SizeOf(TNTFS_Header) then
  begin
    X := ExtraField;
    if X = '' then
    begin
      Result := ZM_Error({_LINE_}1934, ZE_Zip64FieldError);
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

function TZMEntryBase.FindDataTag(Tag: Word; var Idx, Siz: Integer): Boolean;
begin
  Result := False;
  if XData(ExtraField, Tag, Idx, Siz) then
    Result := True;
end;

// will return empty if not exists or invalid
function TZMEntryBase.GetDataString(Cmnt: Boolean): UTF8String;
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
    PS := @ExtraField[Idx];
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

function TZMEntryBase.GetDateStamp: TDateTime;
begin
  Result := FileDateToLocalDateTime(GetDateTime);
end;

function TZMEntryBase.GetDirty: Boolean;
begin
  Result := TestStatusBit(ZsbDirty);
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
function TZMEntryBase.GetEncoded: TZMEncodingOpts;
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

function TZMEntryBase.GetEncrypted: Boolean;
begin
  Result := (Flag and FLAG_CRYPT_BIT) <> 0;
end;

function TZMEntryBase.GetHash: Cardinal;
begin
  if not TestStatusBit(ZsbHashed) then
  begin
    FHash := HashFuncNoCase(HTFileName);
    SetStatusBit(ZsbHashed);
  end;
  Result := FHash;
end;

function TZMEntryBase.GetHTFileName: string;
begin
  Result := FileName;
end;

function TZMEntryBase.GetIsDirOnly: Boolean;
begin
  Result := CharInSet(FileName[Length(FileName)], ['/', '\']);
end;

function TZMEntryBase.GetIsEncoded: TZMEncodingOpts;
var
  N: Integer;
begin
  N := StatusBit[ZsbEncMask] shr 16;
  if N > Ord(ZeoUPath) then
    N := 0;
  if N = 0 then
  begin
    // unknown - work it out and cache result
    Result := Encoded;
    SetIsEncoded(Result);
  end
  else
    Result := TZMEncodingOpts(N);
end;

function TZMEntryBase.GetMaster: TCustomZipMaster;
begin
  Result := FBody.Master;
end;

function TZMEntryBase.GetSelected: Boolean;
begin
  Result := TestStatusBit(ZsbSelected);
end;

function TZMEntryBase.GetStatus(Bit: Cardinal): Boolean;
begin
  Result := (FStatusBits and Bit) <> 0;
end;

function TZMEntryBase.GetStatusBit(Mask: Cardinal): Cardinal;
begin
  Result := FStatusBits and Mask;
end;

function TZMEntryBase.GetStatusBits: Cardinal;
begin
  Result := FStatusBits;
end;

function TZMEntryBase.GetTitle: string;
begin
  Result := FileName;
end;

function TZMEntryBase.GetXName: string;
begin
  Result := FXName;
  if Result = '' then
    Result := FileName;
end;

function TZMEntryBase.HasChanges: Boolean;
begin
  Result := (StatusBits and ZsbDirty) <> 0;
end;

function TZMEntryBase.HasDataDesc: Boolean;
begin
  Result := (Flag and FLAG_DATADESC_BIT) <> 0;
end;

function TZMentryBase.HTIsSame(const AnEntry: TZMEntryBase): Boolean;
var
  FN: string;
  FnMe: string;
begin
  Result := False;
  if TestStatusBit(ZsbDiscard) then
    Exit;
  FN := AnEntry.FileName;
  FnMe := FileName;
  Result := (AnEntry.Hash = Hash) and FileNamesSame(FN, FnMe);
end;

function TZMEntryBase.HTIsSameStr(StrHash: Cardinal; const Str: string)
  : Boolean;
var
  FN: string;
begin
  Result := False;
  if TestStatusBit(ZsbDiscard) then
    Exit;
  FN := FileName;
  Result := (StrHash = Hash) and FileNamesSame(Str, FN);
end;

function TZMEntryBase.IsZip64: Boolean;
begin
  Result := (UncompressedSize >= MAX_UNSIGNED) or
    (CompressedSize >= MAX_UNSIGNED) or (RelOffLocalHdr >= MAX_UNSIGNED) or
    (StartOnDisk >= MAX_WORD);
end;

procedure TZMEntryBase.MarkDirty;
begin
  SetStatusBit(ZsbDirty);
end;

// returns the new value
function TZMEntryBase.SeekLocalData(var LocalHeader: TZipLocalHeader;
  const HName: TZMRawBytes): Integer;
var
  Did: Int64;
begin
  ASSERT(Assigned(MyFile), 'no MyFile');
  if Body.Verbosity >= ZvTrace then
    Body.Trace(Format('Seeking local header, FileName=%s, Disk=%d, Offset=%d',
      [FileName, StartOnDisk, RelOffLocalHdr]), {_LINE_}2180, __UNIT__);
  if not MyFile.IsOpen then
  begin
    Result := Body.PrepareErrMsg(ZE_FileOpen, [MyFile.ArchiveName],
      {_LINE_}2184, __UNIT__);
    Exit;
  end;
  Result := ZM_Error({_LINE_}2187, ZE_LOHBadRead);
  try
    MyFile.SeekDisk(StartOnDisk, False);
    MyFile.Position := RelOffLocalHdr;
    Did := MyFile.Read(LocalHeader, SizeOf(DWORD));
    if (Did = SizeOf(DWORD)) and (LocalHeader.HeaderSig = LocalFileHeaderSig)
    then
    begin // was local header
      Did := MyFile.Read(LocalHeader.VersionNeeded,
        (SizeOf(TZipLocalHeader) - SizeOf(DWORD)));
      if Did = (SizeOf(TZipLocalHeader) - SizeOf(DWORD)) then
        Result := VerifyLocalName(LocalHeader, HName);
    end;
    if AbsErr(Result) = ZE_LOHBadRead then
      Body.Inform('could not read local header: ' + FileName,
        {_LINE_}2202, __UNIT__);
    if AbsErr(Result) = ZE_LOHWrongName then
    begin
      Body.Inform('local header name different: ' + FileName,
        {_LINE_}2205, __UNIT__);
      Result := 0;
    end;
  except
    on E: EZipMaster do
    begin
      Result := E.ExtErr;
      Exit;
    end;
    on E: EZMAbort do
      raise;
    on E: Exception do
    begin
      Result := ZM_Error({_LINE_}2216, ZE_UnknownError);
      Exit;
    end;
  end;
end;

function TZMEntryBase.Select(How: TZMSelects): Boolean;
begin
  case How of
    ZzsClear:
      Result := False;
    ZzsSet:
      Result := True;
    // zzsToggle:
  else
    Result := not TestStatusBit(ZsbSelected);
  end;
  SetSelected(Result);
end;

procedure TZMEntryBase.SetIsEncoded(const Value: TZMEncodingOpts);
var
  N: Integer;
begin
  N := Ord(Value) shl 16;
  ClearStatusBit(ZsbEncMask); // clear all
  SetStatusBit(N); // set new value
end;

procedure TZMEntryBase.SetSelected(const Value: Boolean);
begin
  if Selected <> Value then
  begin
    if Value then
      SetStatusBit(ZsbSelected)
    else
    begin
      ClearStatusBit(ZsbSelected);
      SelectArgs := nil;
    end;
  end;
end;

procedure TZMEntryBase.SetStatus(Bit: Cardinal; const Value: Boolean);
begin
  if Value then
    FStatusBits := FStatusBits or Bit
  else
    FStatusBits := FStatusBits and (not Bit);
end;

// returns previous values
function TZMEntryBase.SetStatusBit(const Value: Cardinal): Cardinal;
begin
  Result := FStatusBits and Value;
  FStatusBits := FStatusBits or Value;
end;

function TZMEntryBase.TestStatusBit(const Mask: Cardinal): Boolean;
begin
  Result := (StatusBits and Mask) <> 0;
end;

// read and verify local header (including extra data)
function TZMEntryBase.VerifyLocalName(const LocalHeader: TZipLocalHeader; const
    HName: TZMRawBytes): Integer;
var
  Did: Int64;
  I: Integer;
  T: Integer;
  V: TZMRawBytes;
begin
  if LocalHeader.FileNameLen = Length(HName) then
  begin
    T := LocalHeader.FileNameLen + LocalHeader.ExtraLen;
    SetLength(V, T);
    Did := MyFile.Read(V[1], T);
    if (Did = T) then
    begin
      Result := 0;
      for I := 1 to LocalHeader.FileNameLen do
      begin
        if V[I] <> HName[I] then
        begin
          Result := ZE_LOHWrongName;
          Break;
        end;
      end;
    end
    else
      Result := ZE_LOHBadRead;
  end
  else
    Result := ZE_LOHWrongName;
  if Result <> 0 then
    Result := ZM_Error({_LINE_}2311, Result);
end;

procedure TZMSelectArgs.AfterConstruction;
begin
  inherited;
  FNext := nil;
  Cnts := 0;
end;

procedure TZMSelectArgs.Assign(Other: TZMSelectArgs);
begin
  // ???
end;

end.

