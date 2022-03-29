unit ZMMisc;

// ZMMisc.pas -  support classes

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
// modified 2013-09-30

interface

{$I   '.\ZipVers.inc'}

uses
{$IFDEF VERDXE2up}
  System.Classes, Winapi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZipMstr, ZMZipDirectory, ZMZipReader, ZMCore, ZMBody;

type
  TZM_ConflictEntry = class(TZMConflictEntry)
  private
    FMyMaster: TCustomZipMaster;
    FMyRec: TZMEntryBase;
    FZipName: string;
    function Fetch(var Rec: TZMEntryBase): Boolean;
    procedure SetZipName(const Value: string);
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
    function GetOriginalName: string; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Word; override;
    function GetStatusBits: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
    function GetZipName: string; override;
  public
    constructor Create(TheMaster: TCustomZipMaster; TheRec: TZMEntryBase);
    property ZipName: string read GetZipName write SetZipName;
  end;

type
  TZMZipList = class
  private
    FProtectZero: Boolean;
    FZips: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TZMZipReader;
  public
    function Add(AZip: TZMZipReader): Integer;
    procedure BeforeDestruction; override;
    procedure Clear;
    procedure CloseAll;
    function Find(const AZipFile: string): Integer;
    property Count: Integer read GetCount;
    property Items[index: Integer]: TZMZipReader read GetItems; default;
    property ProtectZero: Boolean read FProtectZero write FProtectZero;
  end;

type
  TZMDupConflict = class(TZM_ConflictEntry)
  private
    FOriginalName: string;
  protected
    function GetOriginalName: string; override;
  public
    property OriginalName: string read GetOriginalName write FOriginalName;
  end;

const
  Z_SHORT_PATH = 8;

type
  TZMDriveFolders = class
  private
    FFolders: TStringList;
    FReportTo: TZMBody;
    procedure GetCurrentDrives;
    procedure GetCurrentFolders;
    function GetDriveStrings: TStrings;
  public
    constructor Create(TheBody: TZMBody);
    function AddToPath(var Joined: string;
      const BasePath, Extra: string): Integer;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CurrentFolder(DriveLetter: Char): string;
    function ExpandPath(var FullPath: string; const FileSpec: string): Integer;
    procedure FreezeFolders;
    property DriveStrings: TStrings read GetDriveStrings;
  end;

type
  TZMCryData = packed record
    Size: DWORD;
    Ticks: DWord;
    Flags: DWord;
    SniffNo: DWORD;
    Err: DWORD;
    MBuf: Char;
  end;

  PZMCryData = ^TZMCryData;

type
  TZMCrier = class
  private
    DatSize: Integer;
    FBuffer: array of AnsiChar;
    FBufSize: Integer;
    FHandle: HWND;
    PData: PZMCryData;
    function GetErr: Cardinal;
    function GetFlags: DWord;
    function GetMsg: string;
    function GetWidth: Integer;
    procedure SetErr(const Value: Cardinal);
    procedure SetFlags(const Value: DWord);
    procedure SetMsg(const Value: string);
    procedure SetWidth(const Value: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Send(SnifferHndl: HWND; SniffNo: Integer): DWord;
    property BufSize: Integer read FBufSize;
    property Err: Cardinal read GetErr write SetErr;
    property Flags: DWord read GetFlags write SetFlags;
    property Handle: HWND read FHandle write FHandle;
    property Msg: string read GetMsg write SetMsg;
    property Width: Integer read GetWidth write SetWidth;
  end;

type
  TZMLogger = class(TZMLog)
  private
    FBody: TZMBody;
    FCrier: TZMCrier;
    FLevel: Integer;
    FSniffer: HWND;
    FSniffNo: Integer;
    function SendLine(Err: Cardinal; const Msg: string): Boolean;
    function SendMultiLine(const Text: string): Boolean;
    procedure SetCharTyp;
  protected
    function SendStrings(const Strs: TStrings): Boolean;
  public
    constructor Create(Internals: TZMBody);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CloseLog; override;
    // return true if successful
    function Log(Err: Cardinal; const Msg: string): Boolean; override;
    // return true if successful
    function LogStrings(const Desc: string; const Strs: TStrings)
      : Boolean; override;
    function StartLog: Integer; override;
    property Body: TZMBody read FBody;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, WinApi.Messages, System.TypInfo, VCL.Forms,
{$ELSE}
  SysUtils, Messages, TypInfo, Forms,
{$ENDIF}
  ZMMsg, ZMMatch, ZMUtils;

const
  __UNIT__ = 23;

const
  SZipMasterSniffer = 'ZipMaster Sniffer V2';
  STZipSniffer = 'TZipSniffer';
  WM_SNIFF_START = WM_APP + $3F42;
  SNIFF_MASK = $FFFFFF;

const
  CurDirIndex: Integer = 0;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

constructor TZM_ConflictEntry.Create(TheMaster: TCustomZipMaster;
  TheRec: TZMEntryBase);
begin
  inherited Create;
  FMyMaster := TheMaster;
  FMyRec := TheRec;
end;

// return pointer to internal data
function TZM_ConflictEntry.Fetch(var Rec: TZMEntryBase): Boolean;
begin
  Rec := FMyRec;
  Result := Assigned(Rec);
end;

function TZM_ConflictEntry.GetCompressedSize: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CompressedSize;
end;

function TZM_ConflictEntry.GetCompressionMethod: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CompressionMethod;
end;

function TZM_ConflictEntry.GetCRC32: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.CRC32;
end;

function TZM_ConflictEntry.GetDateTime: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ModifDateTime;
end;

function TZM_ConflictEntry.GetEncoded: TZMEncodingOpts;
var
  R: TZMEntryBase;
begin
  Result := ZeoOEM;
  if Fetch(R) then
    Result := R.IsEncoded;
end;

function TZM_ConflictEntry.GetEncrypted: Boolean;
var
  R: TZMEntryBase;
begin
  Result := False;
  if Fetch(R) then
    Result := R.Encrypted;
end;

function TZM_ConflictEntry.GetExtFileAttrib: Longword;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ExtFileAttrib;
end;

function TZM_ConflictEntry.GetExtraField: TZMRawBytes;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.ExtraField;
end;

function TZM_ConflictEntry.GetExtraFieldLength: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.ExtraFieldLength;
end;

function TZM_ConflictEntry.GetFileComment: string;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.FileComment;
end;

function TZM_ConflictEntry.GetFileCommentLen: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.FileCommentLen;
end;

function TZM_ConflictEntry.GetFileName: string;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R.FileName;
end;

function TZM_ConflictEntry.GetFileNameLength: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.FileNameLen;
end;

function TZM_ConflictEntry.GetFlag: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.Flag;
end;

function TZM_ConflictEntry.GetHeaderName: TZMRawBytes;
var
  R: TZMEntryBase;
begin
  Result := '';
  if Fetch(R) then
    Result := R._FileName;
end;

function TZM_ConflictEntry.GetIntFileAttrib: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.IntFileAttrib;
end;

function TZM_ConflictEntry.GetMaster: TComponent;
begin
  Result := FMyMaster;
end;

function TZM_ConflictEntry.GetOriginalName: string;
begin
  Result := '';
end;

function TZM_ConflictEntry.GetRelOffLocalHdr: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.RelOffLocalHdr;
end;

function TZM_ConflictEntry.GetStartOnDisk: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.StartOnDisk;
end;

function TZM_ConflictEntry.GetStatusBits: Cardinal;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.StatusBits;
end;

function TZM_ConflictEntry.GetUncompressedSize: Int64;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.UncompressedSize;
end;

function TZM_ConflictEntry.GetVersionMadeBy: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.VersionMadeBy;
end;

function TZM_ConflictEntry.GetVersionNeeded: Word;
var
  R: TZMEntryBase;
begin
  Result := 0;
  if Fetch(R) then
    Result := R.VersionNeeded;
end;

{ TZM_ConflictEntry }
function TZM_ConflictEntry.GetZipName: string;
begin
  Result := FZipName;
end;

procedure TZM_ConflictEntry.SetZipName(const Value: string);
begin
  FZipName := Value;
end;

function TZMZipList.Add(AZip: TZMZipReader): Integer;
begin
  if FZips = nil then
    FZips := TList.Create;
  Result := FZips.Add(AZip) // add to list and return Index
end;

procedure TZMZipList.BeforeDestruction;
begin
  Clear;
  FZips.Free;
  inherited;
end;

procedure TZMZipList.Clear;
var
  F: Integer;
  I: Integer;
  Z: Pointer;
begin
  if FZips <> nil then
  begin
    if FProtectZero then
      F := 1 // do not destroy first object
    else
      F := 0;
    for I := F to FZips.Count - 1 do
    begin
      Z := FZips.Items[I];
      if Z <> nil then
        TObject(Z).Free;
    end;
    FZips.Clear;
  end;
end;

// close all source files
procedure TZMZipList.CloseAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].File_Close;
end;

// find already open zip file
// return Index or <0 = error
function TZMZipList.Find(const AZipFile: string): Integer;
var
  I: Integer;
  Z: TZMZipReader;
begin
  if FZips = nil then
    Result := ZM_Error({_LINE_}531, ZE_NoInFile)
  else
  begin
    Result := -1;
    // does it exist
    for I := 0 to FZips.Count - 1 do
    begin
      if FZips[I] = nil then
        Continue;
      Z := Items[I];
      if FileNameMatch(Z.Name, AZipFile) then
      begin
        Result := I; // found
        Break;
      end;
    end;
    if Result = -1 then
      Result := ZM_Error({_LINE_}548, ZE_NoInFile);
  end;
end;

function TZMZipList.GetCount: Integer;
begin
  Result := FZips.Count;
end;

function TZMZipList.GetItems(Index: Integer): TZMZipReader;
begin
  Result := nil;
  if (Index >= 0) and (Index < FZips.Count) and (FZips[Index] <> nil) then
    Result := TObject(FZips[Index]) as TZMZipReader;
end;

function TZMDupConflict.GetOriginalName: string;
begin
  Result := FOriginalName;
end;

constructor TZMDriveFolders.Create(TheBody: TZMBody);
begin
  inherited Create;
  FReportTo := TheBody;
end;

function TZMDriveFolders.AddToPath(var Joined: string;
  const BasePath, Extra: string): Integer;
var
  Temp: string;
begin
  Result := 0;
  Temp := PathConcat(BasePath, Extra);
  if IsExtPath(Temp) then
    Joined := Temp
  else
    Result := CleanPath(Joined, Temp, True);
end;

procedure TZMDriveFolders.AfterConstruction;
begin
  inherited;
  FFolders := TStringList.Create;
  FreezeFolders; // 'read' current
end;

procedure TZMDriveFolders.BeforeDestruction;
begin
  FFolders.Free;
  inherited;
end;

function TZMDriveFolders.CurrentFolder(DriveLetter: Char): string;
var
  Drv: Integer;
begin
  Result := '';
  case DriveLetter of
    'A' .. 'Z':
      Drv := 1 + Ord(DriveLetter) - Ord('A');
    'a' .. 'z':
      Drv := 1 + Ord(DriveLetter) - Ord('a');
  else
    Drv := -1;
  end;
  if (Drv >= 0) and (Drv < FFolders.Count) then
    Result := FFolders[Drv];
end;

function TZMDriveFolders.ExpandPath(var FullPath: string; const FileSpec:
    string): Integer;
var
  Path: string;
  PthTyp: TZPathTypes;
  Res: string;
  Spec: string;
  Trail: string;
begin
  Spec := FileSpec;
  Trail := '';
  if Pos('>>', Spec) > 0 then
    Spec := ZSplitString('>>', Spec, Trail);
  Path := '';
  Res := '';
  Result := 0;
  PthTyp := PathType(Spec);
  case PthTyp of
    ZptNone:
      ;
    ZptError:
      Result := FReportTo.PrepareErrMsg(ZE_BadFileName, [Spec],
        {_LINE_}640, __UNIT__);
    ZptAbsolute:
      Path := FileSpec;
    ZptRelDrive:
      begin
        Path := CurrentFolder(Spec[1]);
        if Path <> '' then
        begin
          Result := AddToPath(Res, Path, Copy(Spec, 3, MAXINT)); // remove drive
          Path := '';
        end;
      end;
    ZptRelCurDrive:
      Path := Copy(FFolders[CurDirIndex], 1, 2) + Spec;
    ZptRelCurDir:
      Result := AddToPath(Res, FFolders[CurDirIndex], Spec);
    ZptExt:
      Res := Spec;
    ZptUNC:
      Res := ExpandUNCFileName(Spec);
  end;
  if Path <> '' then
    Result := CleanPath(Res, Path, False);
  if Result >= 0 then
    FullPath := Res + Trail
  else
  begin
    if FReportTo <> nil then
      FReportTo.InformFmt('Invalid FileSpec %d "%s"', [Result, Spec],
        {_LINE_}669, __UNIT__);
    Result := ZM_Error(687, ZE_BadFileName);
  end;
end;

procedure TZMDriveFolders.FreezeFolders;
begin
  FFolders.Clear;
  GetCurrentDrives;
  GetCurrentFolders;
end;

procedure TZMDriveFolders.GetCurrentDrives;
var
  Bits: set of 0 .. 25;
  I: Integer;
  S: string;
begin
  Integer(Bits) := GetLogicalDrives();
  FFolders.Clear;
  S := GetCurrentDir;
  FFolders.Add(S);
  for I := 0 to 25 do
  begin
    if I in Bits then
      S := 'x'
    else
      S := '';
    FFolders.Add(S);
  end;
end;

procedure TZMDriveFolders.GetCurrentFolders;
var
  I: Integer;
  S: string;
begin
  FFolders[CurDirIndex] := DelimitPath(GetCurrentDir, True);
  for I := 1 to 26 do
  begin
    if FFolders[I] <> '' then
    begin
      // drive exists
      S := Char(Ord('A') + I - 1) + ':.\';
      S := DelimitPath(ExpandFileName(S), True);
      FFolders[I] := S;
    end;
  end;
end;

function TZMDriveFolders.GetDriveStrings: TStrings;
begin
  Result := FFolders;
end;

{ TZMLogger }
constructor TZMLogger.Create(Internals: TZMBody);
begin
  inherited Create;
  FBody := Internals;
end;

procedure TZMLogger.AfterConstruction;
begin
  inherited;
  FLevel := 0;
  FSniffer := 0;
  FCrier := TZMCrier.Create;
end;

procedure TZMLogger.BeforeDestruction;
begin
  FCrier.Free;
  inherited;
end;

procedure TZMLogger.CloseLog;
begin
  FSniffer := 0; // closed
end;

function TZMLogger.Log(Err: Cardinal; const Msg: string): Boolean;
begin
  Result := FSniffer <> 0;
  if Result then
    SendLine(Err, Msg);
end;

function TZMLogger.LogStrings(const Desc: string; const Strs: TStrings)
  : Boolean;
var
  M: string;
begin
  Result := FSniffer <> 0;
  if Result then
  begin
    M := ' -- ' + Desc + '  (' + IntToStr(Strs.Count) + ')' + #13#10;
    Result := SendLine(0, M);
    if Result then
    begin
      if Strs is TZMStringList then
        Result := SendMultiLine(Strs.Text)
      else
        Result := SendStrings(Strs);
    end;
  end;
end;

function TZMLogger.SendLine(Err: Cardinal; const Msg: string): Boolean;
begin
  FCrier.Err := Err;
  FCrier.Msg := Msg;
  Result := FCrier.Send(FSniffer, FSniffNo) = 2;
end;

function TZMLogger.SendMultiLine(const Text: string): Boolean;
var
  Len: Integer;
  Next: Integer;
  Nxt: Integer;
  Present: Integer;
  S: string;
begin
  Result := True;
  Present := 1;
  while Result and (Present < Length(Text)) do
  begin
    Next := Present + FCrier.BufSize - 1;
    if Next > Length(Text) then
      Next := Length(Text);
    Nxt := Next;
    while (Nxt > Present) and (Text[Nxt] <> #10) do
      Dec(Nxt); // find end of a line
    Len := Nxt - Present;
    if Len < 1 then
    begin
      // line too long - split it
      Len := (Next - Present) - 10;
      S := Copy(Text, Present, Len) + #13#10;
    end
    else
      S := Copy(Text, Present, Len);
    Result := SendLine(0, S);
    Present := Present + Len;
  end;
end;

function TZMLogger.SendStrings(const Strs: TStrings): Boolean;
var
  I: Integer;
  Len: Integer;
  M: string;
  Obj: TObject;
  S: string;
begin
  Result := True;
  M := '';
  for I := 0 to Strs.Count - 1 do
  begin
    S := ' - ' + Strs[I];
    Obj := Strs.Objects[I];
    if Obj is TZMProblem then
    begin
      S := S + ' [' + IntToStr(TZMProblem(Obj).ErrCode) + ', ' +
        IntToStr(Ord(TZMProblem(Obj).SkipCode)) + ']';
    end;
    S := S + #13#10;
    Len := Length(S);
    if (Length(M) + Len) > FCrier.BufSize then
    begin
      if Length(M) > 0 then
      begin
        Result := SendLine(0, M);
        if not Result then
          Break; // error
        M := '';
      end;
    end;
    M := M + S;
  end;
  if Result and (Length(M) > 0) then
    Result := SendLine(0, M);
end;

procedure TZMLogger.SetCharTyp;
begin
{$IFDEF UNICODE}
  FCrier.Width := 2;
{$ELSE}
  if FBody.Master.UseUTF8 then
    FCrier.Width := 1
  else
    FCrier.Width := 0;
{$ENDIF}
end;

// returns <0 _ error, 0 _ not wanted, >0 _ requested level
function TZMLogger.StartLog: Integer;
var
  Noise: Integer;
  Res: Integer;
  WHandle: HWND;
begin
  Result := 0; // not available/wanted
  WHandle := FindWindow(PChar(STZipSniffer), PChar(SZipMasterSniffer));
  if WHandle = 0 then
    Exit; // not available

  FSniffer := WHandle;
  Noise := Ord(Body.Verbosity) and 7;
  if FSniffer <> 0 then
  begin
    Res := SendMessage(FSniffer, WM_SNIFF_START,
      Longint(Body.Master.Handle), Noise);
    if Res < 0 then
      FSniffer := 0 // invalid
    else
    begin
      // in range so hopefully valid response
      Noise := (Cardinal(Res) shr 24) and 7;
      FSniffNo := Res and SNIFF_MASK; // operation number
      Result := 1;
      SetCharTyp;
      if (Result > 0) and (Noise > 3) then
        Result := Noise;
    end;
  end;
end;

function SetVal(Someset: Pointer): Integer;
begin
  Result := PByte(Someset)^;
end;

procedure TZMCrier.AfterConstruction;
begin
  inherited;
  SetLength(FBuffer, 1024);
  FBufSize := (1024 div SizeOf(Char)) - 1;
  PData := PZMCryData(@FBuffer[0]);
  DatSize := SizeOf(TZMCryData);
end;

procedure TZMCrier.BeforeDestruction;
begin
  FBuffer := nil;
  inherited;
end;

function TZMCrier.GetErr: Cardinal;
begin
  Result := PData^.Err;
end;

function TZMCrier.GetFlags: DWord;
begin
  Result := PData^.Flags;
end;

function TZMCrier.GetMsg: string;
begin
  Result := string(@PData^.MBuf);
end;

function TZMCrier.GetWidth: Integer;
begin
  Result := PData^.Flags and 3;
end;

function TZMCrier.Send(SnifferHndl: HWND; SniffNo: Integer): DWord;
var
  ACopyData: TCopyDataStruct;
begin
  Result := 0;
  if SnifferHndl <> 0 then
  begin
    PData := PZMCryData(@FBuffer[0]);
    PData^.Ticks := GetTickCount;
    PData^.Size := DatSize;
    PData^.SniffNo := SniffNo;
    ACopyData.DwData := DatSize;
    ACopyData.CbData := DatSize;
    ACopyData.LpData := PData;
    Result := SendMessage(SnifferHndl, WM_COPYDATA, Handle,
      Longint(@ACopyData));
  end;
  Msg := '';
end;

procedure TZMCrier.SetErr(const Value: Cardinal);
begin
  PData^.Err := Value;
end;

procedure TZMCrier.SetFlags(const Value: DWord);
begin
{$IFDEF UNICODE}
  PData^.Flags := (Value and $FFFC) or 2;
{$ELSE}
  PData^.Flags := (Value and $FFFC) or DWord(Width);
{$ENDIF}
end;

procedure TZMCrier.SetMsg(const Value: string);
var
  BSize: Integer;
  PEnd: PChar;
  ReqSize: Integer;
begin
  ReqSize := SizeOf(TZMCryData) + (Length(Value) * SizeOf(Char));
  if ReqSize >= Length(FBuffer) then
  begin
    BSize := (ReqSize or 511) + 1;
    SetLength(FBuffer, BSize);
    FBufSize := (BSize div SizeOf(Char)) - 1;
  end;
  PData := PZMCryData(@FBuffer[0]);
  if Length(Value) > 0 then
    Move(PChar(@Value[1])^, PData^.MBuf, Length(Value) * Sizeof(Char));
  PEnd := PChar(@PData^.MBuf);
  Inc(PEnd, Length(Value));
  PEnd^ := #0;
  DatSize := ReqSize;
end;

procedure TZMCrier.SetWidth(const Value: Integer);
begin
{$IFDEF UNICODE}
  PData^.Flags := (PData^.Flags and $FFFC) or 2;
{$ELSE}
  PData^.Flags := (PData^.Flags and $FFFC) or DWord(Value and 1);
{$ENDIF}
end;

end.
