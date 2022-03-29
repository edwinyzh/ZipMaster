unit ZMLister;

// ZMLister.pas - Loads and 'lists' zips

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

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils, VCL.Graphics,
{$ELSE}
  Windows, SysUtils, Classes, Graphics, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMStructs, ZMBody, ZMZipReader, ZMZipDirectory;

type
  // None = no reload, Reload = reload non-detached, Any = reload any, Clear = clear
  TZMReloads = (ZlrNone, ZlrClear, ZlrLoaded, ZlrReload, ZlrAny);

type
  TZMExtEntry = class;

  TZMLister = class(TZMBody)
  private
    FCurrent: TZMZipReader;
    FDirOnlyCount: Integer;
    FExtStream: TStream;
    FLockKey: Integer;
    FRecList: TList;
    FReload: TZMReloads;
    FStatusBits: Cardinal;
    FUseDirOnlyEntries: Boolean;
    FZipComment: AnsiString;
    FZipFileName: string;
    function AddRecord(Rec: TZMEntryBase): Boolean;
    procedure ClearEntries;
    function FindFirstIcon(var Rec: TImageResourceDataEntry;
      const ILevel: Integer; const PointerToRawData: Cardinal;
      Str: TStream): Boolean;
    function GetCount: Integer;
    function GetCurrent: TZMZipReader;
    function GetCurrentIsValid: Boolean;
    function GetDirEntry(Idx: Integer): TZMDirEntry;
    function GetDirOnlyCnt: Integer;
    function GetFirstIcon(Str: TMemoryStream): TIcon;
    function GetIsSpanned: Boolean;
    function GetSFXOffset: Integer;
    function GetTotalDisks: Integer;
    function GetUseDirOnlyEntries: Boolean;
    function GetZipComment: AnsiString;
    function GetZipEOC: Int64;
    function GetZipFileSize: Int64;
    function GetZipSOC: Int64;
    function MapSFXSettings17(Pheder: PByte; Stub: TMemoryStream): Integer;
    function MapSFXSettings19(Pheder: PByte; Stub: TMemoryStream): Integer;
    function ReadSFXStr17(var P: PByte; Len: Byte): AnsiString;
    procedure SetCurrent(const Value: TZMZipReader);
    procedure SetUseDirOnlyEntries(const Value: Boolean);
    procedure SetZipFileName(const Value: string);
    procedure ZipChange(Sender: TObject; Idx: Integer; Chng: TZCChanges);
  protected
    function IsDetachSFX(Zfile: TZMZipReader): Boolean;
    function LoadSFXStr(Ptbl: PByte; Ident: Byte): string;
    function MapOptionsFrom17(Opts: Word): TZMSFXOpts;
    function MapOptionsFromStub(Opts: Word): TZMSFXOpts;
    function MapOverwriteModeFromStub(Ovr: Word): TZMOvrOpts;
    // 1 return true if it was there
    function MapSFXSettings(Stub: TMemoryStream): Integer;
    procedure Started; override;
    function TrimDetached(Stub: TMemoryStream): Boolean;
    procedure UpdateAuxProperties;
    procedure UpdateCurrent(WasGood: Boolean);
    property StatusBits: Cardinal read FStatusBits write FStatusBits;
    procedure SetEncoding(const Value: TZMEncodingOpts); override;
    procedure SetEncoding_CP(const Value: Cardinal); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    procedure ClearCachedNames;
    function CurrentZip(MustExist: Boolean; SafePart: Boolean = False)
      : TZMZipReader;
    function Find(const Fspec: string; var Idx: Integer): TZMDirEntry;
    function ForEach(Func: TZMForEachFunction; var Data): Integer;
    function IsDetachedSFX(const FileName: string): Boolean;
    function List: Integer;
    function LoadZip(const ZipName: string; NoEvent: Boolean): Integer;
    procedure LocateFirstIconHeader(Str: TStream;
      var HdrSection: TImageSectionHeader;
      var RecIcon: TImageResourceDataEntry);
    procedure OnDirUpdate;
    procedure OnNewName(Idx: Integer);
    procedure Refresh;
    procedure Set_ExtStream(const Value: TStream);
    procedure Set_ZipFileName(const Zname: string; Load: TZMLoadOpts);
    property Count: Integer read GetCount;
    property Current: TZMZipReader read GetCurrent write SetCurrent;
    property CurrentIsValid: Boolean read GetCurrentIsValid;
    property DirEntry[Idx: Integer]: TZMDirEntry read GetDirEntry; default;
    property DirOnlyCnt: Integer read GetDirOnlyCnt;
    property ExtStream: TStream read FExtStream;
    property IsSpanned: Boolean read GetIsSpanned;
    property LockKey: Integer read FLockKey;
    property Reload: TZMReloads read FReload write FReload;
    property SFXOffset: Integer read GetSFXOffset;
    property TotalDisks: Integer read GetTotalDisks;
    property UseDirOnlyEntries: Boolean read GetUseDirOnlyEntries
      write SetUseDirOnlyEntries;
    property ZipComment: AnsiString read GetZipComment write FZipComment;
    property ZipEOC: Int64 read GetZipEOC;
    property ZipFileName: string read FZipFileName write SetZipFileName;
    property ZipFileSize: Int64 read GetZipFileSize;
    property ZipSOC: Int64 read GetZipSOC;
  end;

  TZMExtEntry = class(TZMDirEntry)
  private
    FMyKey: Integer;
    FMyLister: TZMLister;
    FMyRec: TZMEntryBase;
    function Fetch(var Rec: TZMEntryBase): Boolean;
  protected
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetEncoded: TZMEncodingOpts; override;
    function GetEncrypted: Boolean; override;
    function GetExtFileAttrib: Longword; override;
    function GetExtraField: TZMRawBytes; override;
    function GetExtraFieldLength: Word; override;
    function GetFileComment: string; override;
    function GetFileCommentLen: Word; override;
    function GetFileName: string; override;
    function GetFileNameLength: Word; override;
    function GetFlag: Word; override;
    function GetHeaderName: TZMRawBytes; override;
    function GetIntFileAttrib: Word; override;
    function GetMaster: TComponent; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Word; override;
    function GetStatusBits: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
  public
    constructor Create(Owner: TZMLister; TheRec: TZMEntryBase); overload;
    property MyRec: TZMEntryBase read FMyRec;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  VCL.Dialogs,
{$ELSE}
  Dialogs,
{$ENDIF}
  ZMMsg, ZMUtils, ZMXcpt, ZMHandler, ZMUTF8, ZMZipBase, ZMEngine,
  ZMCore, ZMSFXInt;

const
  __UNIT__ = 20;

const
  MinStubSize = 12000;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{ TZMLister }
function TZMLister.AddRecord(Rec: TZMEntryBase): Boolean;
var
  Idx: Integer;
  XRec: TZMExtEntry;
begin
  Result := False;
  if (not UseDirOnlyEntries) and Rec.TestStatusBit(ZsbDirOnly) then
  begin
    Inc(FDirOnlyCount);
    Rec.ExtIndex := -1; // not indexed
  end
  else
  begin
    XRec := TZMExtEntry.Create(Self, Rec);
    Idx := FRecList.Add(XRec);
    Rec.ExtIndex := Idx;
    FCurrent.HTAdd(Rec, True);
    Result := True;
  end;
  StatusBits := StatusBits or Rec.StatusBits;
end;

procedure TZMLister.AfterConstruction;
begin
  inherited;
  FRecList := TList.Create;
  FUseDirOnlyEntries := UseDirOnlyEntries;
end;

procedure TZMLister.BeforeDestruction;
begin
  Clear;
  FRecList.Free;
  FreeAndNil(FCurrent);
  inherited;
end;

procedure TZMLister.Clear;
begin
  inherited;
  ClearEntries;
  FExtStream := nil;
end;

procedure TZMLister.ClearCachedNames;
var
  Cz: TZMZipReader;
begin
  Cz := FCurrent;
  if Cz <> nil then
    Cz.ClearCachedNames; // update for any changed settings
end;

procedure TZMLister.ClearEntries;
var
  I: Integer;
  Tmp: TObject;
begin
  FDirOnlyCount := 0;
  for I := 0 to Pred(FRecList.Count) do
  begin
    Tmp := FRecList.Items[I];
    if Tmp <> nil then
    begin
      FRecList.Items[I] := nil;
      Tmp.Free;
    end;
  end;
  FRecList.Clear;
  StatusBits := 0;
end;

function TZMLister.CurrentZip(MustExist: Boolean; SafePart: Boolean = False)
  : TZMZipReader;
begin
  if ZipFileName = '' then
    raise EZipMaster.CreateMsg(Self, ZE_NoZipSpecified, {_LINE_}294, __UNIT__);
  Result := Current;
  if MustExist and ((Zfi_Loaded and Result.Info) = 0) then
    raise EZipMaster.CreateMsg(Self, ZE_NoValidZip, {_LINE_}297, __UNIT__);
  if SafePart and ((Zfi_Cancelled and Result.Info) <> 0) then
  begin
    if Result.AskAnotherDisk(ZipFileName) = IdCancel then
      raise EZipMaster.CreateMsg(Self, ZS_Abort, {_LINE_}301, __UNIT__);
    Result.Info := 0; // clear error
  end;

  if Result.ArchiveName = '' then
  begin
    // creating new file
    Result.ArchiveName := ZipFileName;
    Result.ReqFileName := ZipFileName;
  end;
end;

function TZMLister.Find(const Fspec: string; var Idx: Integer): TZMDirEntry;
var
  Rec: TZMEntryBase;
begin
  if Idx < -1 then
    Idx := -1;
  Result := nil;
  if (Idx + 1) < Count then
  begin
    Rec := FCurrent.SearchName(Fspec, IsWild(Fspec), Idx);
    if Rec <> nil then
    begin
      Idx := Rec.ExtIndex;
      Result := TZMDirEntry(FRecList[Idx]);
    end;
  end;
  if Result = nil then
    Idx := -1;
end;

function TZMLister.FindFirstIcon(var Rec: TImageResourceDataEntry;
  const ILevel: Integer; const PointerToRawData: Cardinal;
  Str: TStream): Boolean;
var
  I: Integer;
  IPos: Integer;
  RecDir: TImageResourceDirectory;
  RecEnt: TImageResourceDirectoryEntry;
begin
  // position must be correct
  Result := False;
  if (Str.Read(RecDir, Sizeof(RecDir)) <> Sizeof(RecDir)) then
    raise EZipMaster.CreateMsg(Self, ZE_BrowseError, {_LINE_}345, __UNIT__);

  for I := 0 to Pred(RecDir.NumberOfNamedEntries + RecDir.NumberOfIdEntries) do
  begin
    if (Str.Read(RecEnt, Sizeof(RecEnt)) <> Sizeof(RecEnt)) then
      raise EZipMaster.CreateMsg(Self, ZE_BrowseError, {_LINE_}350, __UNIT__);

    // check if a directory or a resource
    IPos := Str.Position;
    try
      if (RecEnt.Un2.DataIsDirectory and IMAGE_RESOURCE_DATA_IS_DIRECTORY) = IMAGE_RESOURCE_DATA_IS_DIRECTORY
      then
      begin
        if ((ILevel = 0) and (MakeIntResource(RecEnt.Un1.Name) <> RT_ICON)) or
          ((ILevel = 1) and (RecEnt.Un1.Id <> 1)) then
          Continue; // not an icon of id 1

        Str.Seek(RecEnt.Un2.OffsetToDirectory and
          (not IMAGE_RESOURCE_DATA_IS_DIRECTORY) + PointerToRawData,
{$IFDEF VERPre6}Word{$ENDIF}(TSeekOrigin(SoFromBeginning)));
        Result := FindFirstIcon(Rec, ILevel + 1, PointerToRawData, Str);
        if Result then
          Break;
      end
      else
      begin
        // is resource bin data
        Str.Seek(RecEnt.Un2.OffsetToData + PointerToRawData,
{$IFDEF VERPre6}Word{$ENDIF}(TSeekOrigin(SoFromBeginning)));
        if Str.Read(Rec, Sizeof(Rec)) <> Sizeof(Rec) then
          raise EZipMaster.CreateMsg(Self, ZE_BrowseError, {_LINE_}375,
            __UNIT__);
        Result := True;
        Break;
      end;
    finally
      Str.Position := IPos;
    end;
  end;
end;

function TZMLister.ForEach(Func: TZMForEachFunction; var Data): Integer;
var
  CurZip: TZMZipReader;
  Good: Integer;
  Idx: Integer;
  // i: Integer;
  Rec: TZMEntryBase;
  SelCnt: Integer;
  XRec: TZMDirEntry;
begin
  Result := 0;
  Good := 0;
  if Logging then
    LogSpecs('');
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsg(Self, ZE_InvalidArguments, {_LINE_}401,
      __UNIT__);
  Trace('StartUp ForEach', {_LINE_}403, __UNIT__);
  CurZip := CurrentZip(True);
  SelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs);
  if SelCnt < 0 then
  begin
    Result := SelCnt;
    ShowError(Result);
    Exit;
  end;
  ClearIncludeSpecs; // will contain files processed
  ShowProgress := ZspFull;
  CurZip.Guage.SetCount(SelCnt);
  CurZip.Guage.SetSize(SelCnt);
  Rec := CurZip.FirstSelected;
  while Rec <> nil do
  begin
    if (not UseDirOnlyEntries) and Rec.IsDirOnly then
    begin
      // ignore DirOnly entries
      Rec := CurZip.NextSelected(Rec);
      Continue;
    end;
    if Verbosity > ZvVerbose then
      TraceFmt('Processing: %s', [Rec.FileName], {_LINE_}426, __UNIT__);
    CurZip.Guage.NewItem(Rec.FileName, 1);
    Idx := Rec.ExtIndex;
    if Idx >= 0 then
    begin
      XRec := TZMDirEntry(FRecList[Idx]);
      Result := Func(XRec, Data);
      if Result <> 0 then
      begin
        ProblemList.Add(Rec.FileName);
        Break;
      end;
      Inc(Good);
      IncludeSpecs.Add(Rec.FileName);
      CurZip.Guage.Advance(1);
    end;
    CheckCancel;
    Rec := CurZip.NextSelected(Rec);
  end;
  CurZip.Guage.EndBatch;
  SuccessCnt := Good;
  Trace('finished ForEach', {_LINE_}447, __UNIT__);
  if Logging then
    LogSpecs('');
end;

function TZMLister.GetCount: Integer;
begin
  Result := FRecList.Count;
end;

function TZMLister.GetCurrent: TZMZipReader;
begin
  if Assigned(FCurrent) then
  begin
    if (FCurrent.Info and Zfi_Invalid) <> 0 then
      Current := TZMZipReader.Create(Self); // force reload
  end
  else
    Current := TZMZipReader.Create(Self);
  Result := FCurrent;
end;

function TZMLister.GetCurrentIsValid: Boolean;
begin
  Result := (ZipFileName <> '') and (Current <> nil) and
    ((Current.Info and Zfi_Loaded) <> 0);
end;

function TZMLister.GetDirEntry(Idx: Integer): TZMDirEntry;
begin
  if (Idx >= 0) and (Idx < FRecList.Count) then
    Result := TZMDirEntry(FRecList[Idx])
  else
  begin
    ReportMsg(ZE_RangeError, [Idx, FRecList.Count - 1]);
    Result := nil;
  end;
end;

function TZMLister.GetDirOnlyCnt: Integer;
begin
  Result := FDirOnlyCount;
end;

// replaces an icon in an executable file (stream)
function TZMLister.GetFirstIcon(Str: TMemoryStream): TIcon;
var
  Bad: Boolean;
  Delta: Cardinal;
  Handle: HIcon;
  HdrSection: TImageSectionHeader;
  IcoData: PByte;
  IcoSize: Cardinal;
  RecIcon: TImageResourceDataEntry;
begin
  Bad := True;
  Result := nil;
  LocateFirstIconHeader(Str, HdrSection, RecIcon);
  Delta := Integer(HdrSection.PointerToRawData) -
    Integer(HdrSection.VirtualAddress) + Integer(RecIcon.OffsetToData);
  IcoData := PByte(Str.Memory);
  Inc(IcoData, Delta);
  IcoSize := HdrSection.SizeOfRawData;
  Handle := CreateIconFromResource(IcoData, IcoSize, True, $30000);
  if Handle <> 0 then
  begin
    Result := TIcon.Create;
    Result.Handle := Handle;
    Bad := False;
  end;
  if Bad then
    // no icon copied, so none of matching size found
    raise EZipMaster.CreateMsg(Self, ZE_NoIconFound, {_LINE_}519, __UNIT__);
end;

function TZMLister.GetIsSpanned: Boolean;
begin
  if FCurrent <> nil then
    Result := FCurrent.MultiDisk
  else
    Result := False;
end;

function TZMLister.GetSFXOffset: Integer;
begin
  if FCurrent <> nil then
    Result := FCurrent.CentralOffset
  else
    Result := 0;
end;

function TZMLister.GetTotalDisks: Integer;
begin
  Result := 0;
  if Assigned(FCurrent) then
    Result := FCurrent.TotalDisks;
end;

function TZMLister.GetUseDirOnlyEntries: Boolean;
begin
  Result := FUseDirOnlyEntries;
end;

function TZMLister.GetZipComment: AnsiString;
begin
  if CurrentIsValid then
    Result := Current.ZipComment
  else
    Result := FZipComment;
end;

function TZMLister.GetZipEOC: Int64;
begin
  if FCurrent <> nil then
    Result := FCurrent.EOCOffset
  else
    Result := 0;
end;

function TZMLister.GetZipFileSize: Int64;
begin
  if FCurrent <> nil then
    Result := FCurrent.File_Size
  else
    Result := 0;
end;

function TZMLister.GetZipSOC: Int64;
begin
  if FCurrent <> nil then
    Result := FCurrent.CentralOffset
  else
    Result := 0;
end;

function TZMLister.IsDetachedSFX(const FileName: string): Boolean;
var
  Ext: string;
  FN: string;
  Wz: TZMZipReader;
begin
  Result := False;
  FN := FileName;
  if FN = '' then
    FN := ZipFileName;
  Ext := ExtractFileExt(FN);
  if AnsiSameText(Ext, '.exe') then
  begin
    Wz := TZMZipReader.Create(Self);
    try
      Wz.ArchiveName := FN;
      if (Wz.OpenEOC(FN, True) >= 0) and IsDetachSFX(Wz) then
        Result := True;
    finally
      Wz.Free;
    end;
  end;
end;

// if is detached sfx - set stub excluding the detached header
function TZMLister.IsDetachSFX(Zfile: TZMZipReader): Boolean;
const
  MaxStubSize = 80000;
var
  Cstt: Integer;
  Ms: TMemoryStream;
begin
  Result := False;
  try
    Zfile.Stub := nil; // remove old
    Ms := nil;
    if (Zfile.IsOpen) and (Zfile.DiskNr = 0) and (Zfile.Sig = ZfsDOS) then
    begin
      // check invalid values
      if (Zfile.EOCOffset <= Zfile.CentralSize) or
        (Zfile.CentralSize < Sizeof(TZipCentralHeader)) then
        Exit;
      Cstt := Zfile.EOCOffset - Zfile.CentralSize;
      // must have SFX stub but we only check for biggest practical header
      if (Cstt < MinStubSize) or (Cstt > MaxStubSize) then
        Exit;
      if Zfile.Seek(0, SoBeginning) <> 0 then
        Exit;
      Ms := TMemoryStream.Create;
      try
        if Zfile.ReadTo(Ms, Cstt + 4) = (Cstt + 4) then
        begin
          Result := TrimDetached(Ms);
        end;
      finally
        Ms.Free;
      end;
    end;
  except
    Result := False;
    FreeAndNil(Ms);
  end;
end;

function TZMLister.List: Integer;
begin
  Result := LoadZip(ZipFileName, False);
end;

// table format - ident: byte, strng[]: byte, 0: byte; ...;0
function TZMLister.LoadSFXStr(Ptbl: PByte; Ident: Byte): string;
var
  Id: Byte;
begin
  Result := '';
  if (Ptbl = nil) or (Ident = 0) then
    Exit;
  Id := Ptbl^;
  while (Id <> 0) and (Id <> Ident) do
  begin
    while Ptbl^ <> 0 do
      Inc(Ptbl);
    Inc(Ptbl);
    Id := Ptbl^;
  end;
  if Id = Ident then
  begin
    Inc(Ptbl);
{$IFDEF UNICODE}
    Result := PUTF8ToStr(PAnsiChar(Ptbl), -1);
{$ELSE}
    if IsUtf8 then
      Result := UTF8String(PAnsiChar(Ptbl))
    else
      Result := PUTF8ToStr(PAnsiChar(Ptbl), -1);
{$ENDIF}
  end;
end;

function TZMLister.LoadZip(const ZipName: string; NoEvent: Boolean): Integer;
var
  TmpDirUpdate: TNotifyEvent;
begin
  Result := 0;
  Current := nil; // close and remove any old file
  if (ZipName <> '') or (FExtStream <> nil) then
  begin
    // make new ZipFile
    if FExtStream <> nil then
      Current.Stream := ExtStream
    else
      Current.ArchiveName := ZipName;
    Result := Current.OpenZip(False, False);
    if Result >= 0 then
    begin
      Current.File_Close;
      ZipComment := '';
      // FZipComment := Current.ZipComment;
      Reload := ZlrLoaded; // update Aux properties
    end
    else
    begin
      if AbsErr(Result) = ZE_NoInFile then
      begin
        // just report no file - may be intentional
        Errors.ExtCode := ZM_Error({_LINE_}707, ZE_NoInFile);
        Errors.ErrMessage := ZipLoadStr(ZE_NoInFile);
        Result := 0;
      end;
    end;
  end;
  if not NoEvent then
  begin
    TmpDirUpdate := Master.OnDirUpdate;
    if Assigned(TmpDirUpdate) then
      TmpDirUpdate(Master);
  end;
end;

procedure TZMLister.LocateFirstIconHeader(Str: TStream;
  var HdrSection: TImageSectionHeader; var RecIcon: TImageResourceDataEntry);
var
  BFound: Boolean;
  CAddress: Cardinal;
  DataDir: PImageDataDirectory;
  HdrDos: TImageDosHeader;
  HdrNT: TImageNTHeaders;
  I: Integer;
begin
  BFound := False;
  // check if we have an executable
  Str.Seek(0, SoFromBeginning);
  if (Str.Read(HdrDos, Sizeof(HdrDos)) <> Sizeof(HdrDos)) or
    (HdrDos.E_magic <> IMAGE_DOS_SIGNATURE) then
    raise EZipMaster.CreateMsg(Self, ZE_InputNotExe, {_LINE_}736, __UNIT__);

  Str.Seek(HdrDos._lfanew, SoFromBeginning);
  if (Str.Read(HdrNT, Sizeof(HdrNT)) <> Sizeof(HdrNT)) or
    (HdrNT.Signature <> IMAGE_NT_SIGNATURE) then
    raise EZipMaster.CreateMsg(Self, ZE_InputNotExe, {_LINE_}741, __UNIT__);

  // check if we have a resource section
  DataDir := @(HdrNT.OptionalHeader.DataDirectory
    [IMAGE_DIRECTORY_ENTRY_RESOURCE]);
  if (DataDir^.VirtualAddress = 0) or (DataDir^.Size = 0) then
    raise EZipMaster.CreateMsg(Self, ZE_NoExeResource, {_LINE_}747, __UNIT__)
  else
    CAddress := DataDir^.VirtualAddress; // store address

  // iterate over sections
  for I := 0 to Pred(HdrNT.FileHeader.NumberOfSections) do
  begin
    if (Str.Read(HdrSection, Sizeof(HdrSection)) <> Sizeof(HdrSection)) then
      raise EZipMaster.CreateMsg(Self, ZE_ExeSections, {_LINE_}755, __UNIT__);

    // with hdrSection do
    if HdrSection.VirtualAddress = CAddress then
    begin
      BFound := True;
      Break;
    end;
  end;

  if not BFound then
    raise EZipMaster.CreateMsg(Self, ZE_NoExeResource, {_LINE_}766, __UNIT__);

  // go to resource data
  Str.Seek(HdrSection.PointerToRawData,
{$IFDEF VERPre6}Word{$ENDIF}(TSeekOrigin(SoFromBeginning)));

  // recurse through the resource dirs to find an icon
  if not FindFirstIcon(RecIcon, 0, HdrSection.PointerToRawData, Str) then
    raise EZipMaster.CreateMsg(Self, ZE_NoExeIcon, {_LINE_}774, __UNIT__);
end;

function TZMLister.MapOptionsFrom17(Opts: Word): TZMSFXOpts;
begin
  Result := [];
  if (So_AskCmdLine_17 and Opts) <> 0 then
    Result := Result + [SoAskCmdLine];
  if (So_AskFiles_17 and Opts) <> 0 then
    Result := Result + [SoAskFiles];
  if (So_HideOverWriteBox_17 and Opts) <> 0 then
    Result := Result + [SoHideOverWriteBox];
  if (So_AutoRun_17 and Opts) <> 0 then
    Result := Result + [SoAutoRun];
  if (So_NoSuccessMsg_17 and Opts) <> 0 then
    Result := Result + [SoNoSuccessMsg];
  if (So_ExpandVariables_17 and Opts) <> 0 then
    Result := Result + [SoExpandVariables];
  if (So_InitiallyHideFiles_17 and Opts) <> 0 then
    Result := Result + [SoInitiallyHideFiles];
  if (So_ForceHideFiles_17 and Opts) <> 0 then
    Result := Result + [SoForceHideFiles];
  if (So_CheckAutoRunFileName_17 and Opts) <> 0 then
    Result := Result + [SoCheckAutoRunFileName];
  if (So_CanBeCancelled_17 and Opts) <> 0 then
    Result := Result + [SoCanBeCancelled];
  if (So_CreateEmptyDirs_17 and Opts) <> 0 then
    Result := Result + [SoCreateEmptyDirs];
end;

function TZMLister.MapOptionsFromStub(Opts: Word): TZMSFXOpts;
begin
  Result := [];
  if (So_AskCmdLine and Opts) <> 0 then
    Result := Result + [SoAskCmdLine];
  if (So_AskFiles and Opts) <> 0 then
    Result := Result + [SoAskFiles];
  if (So_HideOverWriteBox and Opts) <> 0 then
    Result := Result + [SoHideOverWriteBox];
  if (So_AutoRun and Opts) <> 0 then
    Result := Result + [SoAutoRun];
  if (So_NoSuccessMsg and Opts) <> 0 then
    Result := Result + [SoNoSuccessMsg];
  if (So_ExpandVariables and Opts) <> 0 then
    Result := Result + [SoExpandVariables];
  if (So_InitiallyHideFiles and Opts) <> 0 then
    Result := Result + [SoInitiallyHideFiles];
  if (So_ForceHideFiles and Opts) <> 0 then
    Result := Result + [SoForceHideFiles];
  if (So_CheckAutoRunFileName and Opts) <> 0 then
    Result := Result + [SoCheckAutoRunFileName];
  if (So_CanBeCancelled and Opts) <> 0 then
    Result := Result + [SoCanBeCancelled];
  if (So_CreateEmptyDirs and Opts) <> 0 then
    Result := Result + [SoCreateEmptyDirs];
  if (So_SuccessAlways and Opts) <> 0 then
    Result := Result + [SoSuccessAlways];
end;

function TZMLister.MapOverwriteModeFromStub(Ovr: Word): TZMOvrOpts;
begin
  case Ovr of
    Som_Overwrite:
      Result := OvrAlways;
    Som_Skip:
      Result := OvrNever;
  else
    Result := OvrConfirm;
  end;
end;

function TZMLister.MapSFXSettings(Stub: TMemoryStream): Integer;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    X: Word;
  end;

  P_header = ^T_header;
var
  I: Integer;
  NumSections: Integer;
  P: PByte;
  Phed: P_header;
  Sz: Cardinal;
  Tmp: Cardinal;
begin
  Result := 0;
  if (Stub <> nil) and (Stub.Size > MinStubSize) then
  begin
    Sz := 0;
    P := Stub.Memory;
    if (PImageDosHeader(P).E_magic <> IMAGE_DOS_SIGNATURE) then
      Exit;
    Result := ZM_Error({_LINE_}869, ZE_SFXBadRead); // 'unknown sfx'
    Inc(P, PImageDosHeader(P)._lfanew);
    if PCardinal(P)^ <> IMAGE_PE_SIGNATURE then
      Exit; // not exe
    Inc(P, Sizeof(Cardinal));
    NumSections := PImageFileHeader(P).NumberOfSections;
    Inc(P, Sizeof(TImageFileHeader) + Sizeof(TImageOptionalHeader));
    for I := 1 to NumSections do
    begin
      Tmp := PImageSectionHeader(P)^.PointerToRawData + PImageSectionHeader(P)
        ^.SizeOfRawData;
      if Tmp > Sz then
        Sz := Tmp;
      Inc(P, Sizeof(TImageSectionHeader));
    end;
    // sz = end of stub
    P := Stub.Memory;
    Inc(P, Sz);
    Phed := P_header(P);
    if Phed.Sig = SFX_HEADER_SIG then
      Result := MapSFXSettings19(P, Stub)
    else
      if Phed.Sig = SFX_HEADER_SIG_17 then
        Result := MapSFXSettings17(P, Stub);
  end;
end;

function TZMLister.MapSFXSettings17(Pheder: PByte; Stub: TMemoryStream)
  : Integer;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    X: Word;
  end;

  P_header = ^T_header;
var
  Ico: TIcon;
  P: PByte;
  PSFXHeader: PSFXFileHeader_17;
  X_Caption, X_Path, X_CmdLine, X_RegFailPath, X_StartMsg: AnsiString;
begin
  Result := ZM_Error({_LINE_}912, ZE_SFXBadRead);
  PSFXHeader := PSFXFileHeader_17(Pheder);
  P := Pheder;
  Inc(P, Sizeof(TSFXFileHeader_17)); // point to strings
  X_Caption := ReadSFXStr17(P, PSFXHeader^.CaptionSize);
  X_Path := ReadSFXStr17(P, PSFXHeader^.PathSize);
  X_CmdLine := ReadSFXStr17(P, PSFXHeader^.CmdLineSize);
  X_RegFailPath := ReadSFXStr17(P, PSFXHeader^.RegFailPathSize);
  X_StartMsg := ReadSFXStr17(P, PSFXHeader^.StartMsgSize);

  // read icon
  try
    Ico := GetFirstIcon(Stub);
    // should test valid
    SFX.Icon := Ico;
    Ico.Free;
  except
    on E: EZipMaster do
    begin
      Result := E.ExtErr;
      Exit;
    end
    else
      Exit;
  end;
  SFX.Options := MapOptionsFrom17(PSFXHeader^.Options);
  SFX.OverwriteMode := MapOverwriteModeFromStub(PSFXHeader^.DefOVW);
  if (PSFXHeader^.StartMsgType and (MB_OKCANCEL or MB_YESNO)) <> 0 then
  begin
    if (PSFXHeader^.StartMsgType and MB_OKCANCEL) <> 0 then
      X_StartMsg := '1|' + X_StartMsg
    else
      if (PSFXHeader^.StartMsgType and MB_YESNO) <> 0 then
        X_StartMsg := '2|' + X_StartMsg;
  end;
  SFX.Message := string(X_StartMsg);
  SFX.Caption := string(X_Caption);
  SFX.DefaultDir := string(X_Path);
  SFX.CommandLine := string(X_CmdLine);
  SFX.RegFailPath := string(X_RegFailPath);
  Result := 0; // all is well
end;

function TZMLister.MapSFXSettings19(Pheder: PByte; Stub: TMemoryStream)
  : Integer;
var
  Cmnds: PByte;
  CStream: TMemoryStream;
  Ico: TIcon;
  Msg: string;
  Delta: Integer;
  P: PByte;
  PHed: PSFXFileHeader;
  Psdat: PSFXStringsData;
begin
  Result := ZM_Error({_LINE_}967, ZE_SFXBadRead);
  PHed := PSFXFileHeader(Pheder);
  CStream := nil;
  Cmnds := PByte(@PHed^.StartMsgType);
  Inc(Cmnds, Sizeof(Word));
  try
    // get command strings
    if (So_CompressedCmd and PHed^.Options) <> 0 then
    begin
      P := Cmnds;
      Cmnds := nil;
      Psdat := PSFXStringsData(P);
      Inc(P, Sizeof(TSFXStringsData)); // point to compressed data
      Delta := Cardinal(P) - Cardinal(Stub.Memory);
      if Stub.Seek(Delta, Word(SoFromBeginning)) = Delta then
      begin
        CStream := TMemoryStream.Create;
        if (UndeflateQ(CStream, Stub, Psdat.CSize, METHOD_DEFLATED, Psdat.CRC)
          = 0) and (CStream.Size = Psdat.USize) then
          Cmnds := CStream.Memory // ok
        else
        begin
          Trace('Undeflate Error: sfx strings', {_LINE_}989, __UNIT__);
{$IFDEF _DEBUG}
          ShowMessage(ZE_InternalError, 'Undeflate error');
{$ENDIF}
        end;
      end;
    end;
    if Cmnds <> nil then
    begin
      // read icon
      try
        Ico := GetFirstIcon(Stub);
        // should test valid
        SFX.Icon := Ico;
        Ico.Free;
      except
        on E: EZipMaster do
        begin
          Result := E.ExtErr;
          Exit;
        end
        else
          Exit;
      end;
      // we have strings
      SFX.Caption := LoadSFXStr(Cmnds, Sc_Caption);
      SFX.DefaultDir := LoadSFXStr(Cmnds, Sc_Path);
      SFX.CommandLine := LoadSFXStr(Cmnds, Sc_CmdLine);
      SFX.RegFailPath := LoadSFXStr(Cmnds, Sc_RegFailPath);
      Msg := LoadSFXStr(Cmnds, Sc_StartMsg);
      SFX.Options := MapOptionsFromStub(PHed^.Options);
      SFX.OverwriteMode := MapOverwriteModeFromStub(PHed^.DefOVW);
      if (PHed^.StartMsgType and (MB_OKCANCEL or MB_YESNO)) <> 0 then
      begin
        if (PHed^.StartMsgType and MB_OKCANCEL) <> 0 then
          Msg := '1|' + Msg
        else
          if (PHed^.StartMsgType and MB_YESNO) <> 0 then
            Msg := '2|' + Msg;
      end;
      SFX.Message := Msg;
      Result := 0; // all is well
    end;
  finally
    if CStream <> nil then
      CStream.Free;
  end;
end;

procedure TZMLister.OnDirUpdate;
begin
  if Assigned(Master.OnDirUpdate) then
    Master.OnDirUpdate(Master);
end;

procedure TZMLister.OnNewName(Idx: Integer);
begin
  if Assigned(Master.OnNewName) then
    Master.OnNewName(Master, Idx);
end;

function TZMLister.ReadSFXStr17(var P: PByte; Len: Byte): AnsiString;
var
  I: Integer;
begin
  Result := '';
  if Len > 0 then
  begin
    SetLength(Result, Len);
    for I := 1 to Len do
    begin
      Result[I] := AnsiChar(P^);
      Inc(P);
    end;
  end;
end;

procedure TZMLister.Refresh;
var
  Cnt: Integer;
  Rec: TZMEntryBase;
begin
  ClearEntries;
  OnDirUpdate;
  if FCurrent <> nil then
  begin
    FCurrent.HTClear;
    Cnt := FCurrent.Count;
    if Cnt > 0 then
    begin
      // load entries
      FRecList.Capacity := Cnt; // make room
      Rec := FCurrent.FirstRec;
      while Rec <> nil do
      begin
        if AddRecord(Rec) then
          OnNewName(Pred(Count));
        Rec := Rec.Next;
      end;
    end;
  end;
  OnDirUpdate;
end;

procedure TZMLister.SetCurrent(const Value: TZMZipReader);
begin
  if FCurrent <> Value then
  begin
    if FCurrent <> nil then
      FCurrent.File_Close;
    ClearEntries;
    FreeAndNil(FCurrent);
    FCurrent := Value;
    if Assigned(Value) then
    begin
      FCurrent.OnChange := ZipChange;
      Inc(FLockKey); // invalidate all external entries
    end;
    Refresh;
  end;
end;

procedure TZMLister.SetEncoding(const Value: TZMEncodingOpts);
begin
  inherited;
  ClearCachedNames; // force update names
  Refresh; // reload with updated names
end;

procedure TZMLister.SetEncoding_CP(const Value: Cardinal);
begin
  inherited;
  ClearCachedNames; // force update names
  Refresh; // reload with updated names
end;

procedure TZMLister.SetUseDirOnlyEntries(const Value: Boolean);
begin
  if Value <> UseDirOnlyEntries then
  begin
    FUseDirOnlyEntries := Value;
    if (StatusBits and ZsbDirOnly) <> 0 then
      Refresh;
  end;
end;

procedure TZMLister.SetZipFileName(const Value: string);
begin
  FExtStream := nil;
  Set_ZipFileName(Value, ZloFull);
end;

procedure TZMLister.Set_ExtStream(const Value: TStream);
var
  Res: Integer;
begin
  if FExtStream <> Value then
  begin
    ClearEntries;
    FExtStream := Value;
    if Value <> nil then
    begin
      FZipFileName := '<ExtStream>';
      Res := LoadZip('', False); // automatically load the file
      if Res < 0 then
        ShowError(Res);
    end
    else
      FZipFileName := '';
  end;
end;

procedure TZMLister.Set_ZipFileName(const Zname: string; Load: TZMLoadOpts);
var
  Res: Integer;
begin
  ClearEntries;
  FExtStream := nil;
  FZipFileName := Zname;
  if Load <> ZloNoLoad then
  begin
    Res := LoadZip(Zname, Load = ZloSilent); // automatically load the file
    if Res < 0 then
      ShowError(Res);
  end;
end;

procedure TZMLister.Started;
begin
  Reload := ZlrNone;
  Current.ClearSelection; //2015/08/09
  inherited;
end;

function TZMLister.TrimDetached(Stub: TMemoryStream): Boolean;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    X: Word;
  end;

  P_header = ^T_header;
var
  I: Integer;
  NumSections: Integer;
  P: PByte;
  Phed: P_header;
  Sz: Cardinal;
begin
  Result := False;
  if (Stub <> nil) and (Stub.Size > MinStubSize) then
  begin
    Sz := 0;
    P := Stub.Memory;
    if (PImageDosHeader(P).E_magic <> IMAGE_DOS_SIGNATURE) then
      Exit;
    Inc(P, PImageDosHeader(P)._lfanew);
    if PCardinal(P)^ <> IMAGE_PE_SIGNATURE then
      Exit; // not exe
    Inc(P, Sizeof(Cardinal));
    NumSections := PImageFileHeader(P).NumberOfSections;
    Inc(P, Sizeof(TImageFileHeader) + Sizeof(TImageOptionalHeader));
    for I := 1 to NumSections do
    begin
      with PImageSectionHeader(P)^ do
        if PointerToRawData + SizeOfRawData > Sz then
          Sz := PointerToRawData + SizeOfRawData;
      Inc(P, Sizeof(TImageSectionHeader));
    end;
    // sz = end of stub
    P := Stub.Memory;
    Inc(P, Sz);
    Phed := P_header(P);
    if Phed.Sig <> SFX_HEADER_SIG then
      Exit; // bad
    Sz := Sz + Phed.Size;
    // posn := sz;
    Inc(P, Phed.Size);
    Phed := P_header(P);
    if (Phed.Sig = CentralFileHeaderSig) then
    begin
      Stub.Size := Sz; // remove file header
      Result := True;
    end;
  end;
end;

// was GetAuxProperties
procedure TZMLister.UpdateAuxProperties;
var
  R: Integer;
  Czip: TZMZipReader;
begin
  if not NoReadAux then
  begin
    Czip := FCurrent;
    // only if it exists
    if Czip <> nil then
    begin
      if (Czip.Info and Zfi_DidLoad) <> 0 then
      begin
        if Czip.Stub <> nil then
        begin
          // read Aux Settings from stub into component
          R := MapSFXSettings(Czip.Stub);
          if R <> 0 then
            Exit; // not easy to show warning
        end;
        if Czip.MultiDisk then
        begin
          Span.Options := Czip.MapNumbering(Span.Options);
          // set multi-disk
          WriteOptions := WriteOptions + [ZwoDiskSpan];
        end
        else
          WriteOptions := WriteOptions - [ZwoDiskSpan];
        Czip.Info := Czip.Info and (not Zfi_DidLoad); // don't clear again
        AuxChanged := False; // clear AuxChanged
      end;
    end;
  end;
end;

procedure TZMLister.UpdateCurrent(WasGood: Boolean);
var
  Czip: TZMZipReader;
  TmpDirUpdate: TNotifyEvent;
begin
  Current.File_Close;
  if WasGood then
  begin
    if Cancel <> 0 then
      Reload := ZlrClear;
    if Reload = ZlrClear then
    begin
      Current := nil; // close and remove any old file
      TmpDirUpdate := Master.OnDirUpdate;
      if Assigned(TmpDirUpdate) then
        TmpDirUpdate(Master);
    end;

    if (Reload = ZlrAny) or
      ((Reload = ZlrReload) and not IsDetachedSFX(ZipFileName)) then
    begin
      LoadZip(ZipFileName, False);
      Current.File_Close;
    end;

    if Reload >= ZlrClear then
    begin
      FZipComment := '';
      if Reload > ZlrClear then
        UpdateAuxProperties; // update Aux properties from current
    end;
  end
  else
  begin
    Czip := Current;
    if Czip.Info <> 0 then
      Czip.Info := (Czip.Info and Zfi_Cancelled) or Zfi_Error;
  end;
end;

procedure TZMLister.ZipChange(Sender: TObject; Idx: Integer; Chng: TZCChanges);
begin
  case Chng of
    ZccBegin:
      ClearEntries;
    ZccAdd:
      if AddRecord(FCurrent.LastRec) then
        OnNewName(Pred(Count));
    ZccEnd:
      OnDirUpdate;
  end;
end;

{TZMExtEntry}
constructor TZMExtEntry.Create(Owner: TZMLister; TheRec: TZMEntryBase);
begin
  inherited Create;
  FMyLister := Owner;
  FMyKey := Owner.LockKey;
  FMyRec := TheRec;
end;

// return pointer to internal data
function TZMExtEntry.Fetch(var Rec: TZMEntryBase): Boolean;
begin
  Assert(FMyLister <> nil, 'Lister must be set');
  if FMyLister.LockKey = FMyKey then
    Rec := FMyRec;
  Result := Assigned(Rec);
end;

function TZMExtEntry.GetCompressedSize: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CompressedSize;
end;

function TZMExtEntry.GetCompressionMethod: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CompressionMethod;
end;

function TZMExtEntry.GetCRC32: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CRC32;
end;

function TZMExtEntry.GetDateTime: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ModifDateTime;
end;

function TZMExtEntry.GetEncoded: TZMEncodingOpts;
var
  R: TZMEntryBase;
begin
  Result := ZeoOEM;
  if Fetch(R) then
    Result := R.IsEncoded;
end;

function TZMExtEntry.GetEncrypted: Boolean;
var
  R: TZMEntryBase;
begin
  Result := False;
  if Fetch(R) then
    Result := R.Encrypted;
end;

function TZMExtEntry.GetExtFileAttrib: Longword;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ExtFileAttrib;
end;

function TZMExtEntry.GetExtraField: TZMRawBytes;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.ExtraField;
end;

function TZMExtEntry.GetExtraFieldLength: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ExtraFieldLength;
end;

function TZMExtEntry.GetFileComment: string;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.FileComment;
end;

function TZMExtEntry.GetFileCommentLen: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.FileCommentLen;
end;

function TZMExtEntry.GetFileName: string;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.FileName;
end;

function TZMExtEntry.GetFileNameLength: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.FileNameLen;
end;

function TZMExtEntry.GetFlag: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.Flag;
end;

function TZMExtEntry.GetHeaderName: TZMRawBytes;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R._FileName;
end;

function TZMExtEntry.GetIntFileAttrib: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.IntFileAttrib;
end;

function TZMExtEntry.GetMaster: TComponent;
begin
  Result := FMyLister.Master;
end;

function TZMExtEntry.GetRelOffLocalHdr: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.RelOffLocalHdr;
end;

function TZMExtEntry.GetStartOnDisk: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.StartOnDisk;
end;

function TZMExtEntry.GetStatusBits: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.StatusBits;
end;

function TZMExtEntry.GetUncompressedSize: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.UncompressedSize;
end;

function TZMExtEntry.GetVersionMadeBy: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.VersionMadeBy;
end;

function TZMExtEntry.GetVersionNeeded: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.VersionNeeded;
end;

end.
