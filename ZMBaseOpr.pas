unit ZMBaseOpr;

(*
 Derived from
 * SFX for DelZip v1.7
 * Copyright 2002-2005
 * written by Markus Stephany
*)
// ZMBaseOpr.pas - base of operation classes
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

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  WinApi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Graphics,
  Vcl.Dialogs, Vcl.Controls,
{$ELSE}
  Windows, SysUtils, Classes, Forms, Graphics, Dialogs, Controls,
{$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMBody, ZMLister, ZMMisc, ZMZipReader, ZMZipWriter;

type
  TZMBaseOpr = class(TZMBase)
  private
    FDriveFolders: TZMDriveFolders;
    FInterimZip: TZMZipWriter;
    FLister: TZMLister;
    FSFXBinStream: TMemoryStream;
    function CreateStubStream: Boolean;
    function GetCurrent: TZMZipReader;
    function GetReload: TZMReloads;
    function GetSuccessCnt: Integer;
    function GetZipFileName: string;
    function LoadFromBinFile(var Stub: TStream; var Specified: Boolean)
      : Integer;
    function LoadFromBinFile1(var Stub: TStream; var Specified: Boolean;
      const DefaultName: string): Integer;
    function LoadFromResource(var Stub: TStream; const Sfxtyp: string): Integer;
    function MapOptionsToStub(Opts: TZMSFXOpts): Word;
    function MapOverwriteModeToStub(Mode: TZMOvrOpts): Word;
    function NewSFXStub: TMemoryStream;
    function RecreateSingle(Intermed, TheZip: TZMZipReader): Integer;
    procedure ReplaceIcon(Str: TMemoryStream; OIcon: TIcon);
    procedure SetCurrent(const Value: TZMZipReader);
    procedure SetInterimZip(const Value: TZMZipWriter);
    procedure SetReload(const Value: TZMReloads);
    procedure SetSuccessCnt(const Value: Integer);
    function WriteIconToStream(Stream: TStream; Icon: HICON;
      Width, Height, Depth: Integer): Integer;
  protected
    procedure CreateInterimZip; virtual;
    function CurrentZip(MustExist: Boolean; SafePart: Boolean = False)
      : TZMZipReader;
    function DetachedSize(Zf: TZMZipReader): Integer;
    function FinalizeInterimZip(OrigZip: TZMZipReader): Integer; virtual;
    procedure PrepareInterimZip;
    function PrepareStub: Integer;
    function PrepareZip(Zip: TZMZipReader): Integer;
    function Recreate(Intermed, TheZip: TZMZipReader): Integer;
    function ReleaseSFXBin: TMemoryStream;
    // 1 Rewrite via an intermediate
    function Remake(CurZip: TZMZipReader; ReqCnt: Integer;
      All: Boolean): Integer;
    procedure VerifySource(SrcZip: TZMZipReader);
    function WriteDetached(Zf: TZMZipReader): Integer;
    function ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
      Btns: TMsgDlgButtons): TModalResult;
    property Current: TZMZipReader read GetCurrent write SetCurrent;
    property DriveFolders: TZMDriveFolders read FDriveFolders;
    property Lister: TZMLister read FLister;
    property Reload: TZMReloads read GetReload write SetReload;
    property SFXBinStream: TMemoryStream read FSFXBinStream write FSFXBinStream;
    property SuccessCnt: Integer read GetSuccessCnt write SetSuccessCnt;
    property ZipFileName: string read GetZipFileName;
  public
    constructor Create(TheLister: TZMLister); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property InterimZip: TZMZipWriter read FInterimZip write SetInterimZip;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.UITypes, Winapi.ShellAPI,
{$ELSE}
  ShellAPI,
{$ENDIF}
  ZMMsg, ZMDrv, ZMUtils, ZMZipBase, ZMZipDirectory, ZMUTF8, ZMWinFuncs,
  ZMEngine, ZMStructs, ZMSFXInt, ZMXcpt;

const
  __UNIT__ = 3;

const
  SFXBinDefault: string = 'ZMSFX192.bin';
  SFXBinUDefault: string = 'ZMSFXU192.bin';
  SFXBufSize: Word = $2000;
  ExtExe = 'exe';
  DotExtExe = '.' + ExtExe;

const
  MinStubSize = 12000;
  MaxStubSize = 80000;
  BufSize = 10240;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

type
  TZMLoader = class(TZMZipWriter)
  private
    FForZip: TZMZipReader;
    FOpr: TZMBaseOpr;
    procedure SetForZip(const Value: TZMZipReader);
  protected
    function AddStripped(const Rec: TZMEntryBase): Integer;
    function BeforeCommit: Integer; override;
    function FixHeaderNames: Integer; override;
    function PrepareDetached: Integer;
    function StripEntries: Integer;
    property Opr: TZMBaseOpr read FOpr;
  public
    constructor Create(TheLister: TZMLister; TheOpr: TZMBaseOpr);
    procedure AfterConstruction; override;
    property ForZip: TZMZipReader read FForZip write SetForZip;
  end;

type
  TZMEntryDetached = class(TZMEntryWriter)
  public
    function Process: Int64; override;
    function ProcessSize: Int64; override;
  end;

function WriteCommand(Dest: TMemoryStream; const Cmd: string; Ident: Integer)
  : Integer; forward;

function WriteCommand(Dest: TMemoryStream; const Cmd: string;
  Ident: Integer): Integer;
var
  Ucmd: UTF8String;
  Z: Byte;
begin
  Result := 0;
  if Length(Cmd) > 0 then
  begin
    Ucmd := AsUTF8Str(Cmd);
    Dest.Write(Ident, 1);
    Result := Dest.Write(PAnsiChar(Ucmd)^, Length(Ucmd)) + 2;
    Z := 0;
    Dest.Write(Z, 1);
  end;
end;

{ TZMBaseOpr }
constructor TZMBaseOpr.Create(TheLister: TZMLister);
begin
  inherited Create(TZMBody(TheLister));
  FLister := TheLister;
end;

procedure TZMBaseOpr.AfterConstruction;
begin
  inherited;
  FDriveFolders := TZMDriveFolders.Create(Body);
  FSFXBinStream := nil;
end;

procedure TZMBaseOpr.BeforeDestruction;
begin
  FDriveFolders.Free;
  if FInterimZip <> nil then
    FInterimZip.Free;
  FreeAndNil(FSFXBinStream);
  inherited;
end;

procedure TZMBaseOpr.CreateInterimZip;
begin
  InterimZip := nil;
end;

function TZMBaseOpr.CreateStubStream: Boolean;
const
  MinVers = 1900000;
var
  BinStub: TStream;
  BinVers: Integer;
  Err: Boolean;
  ResStub: TStream;
  ResVers: Integer;
  Stub: TStream;
  Stubname: string;
  UseBin: Boolean;
  XPath: string;
begin
  // what type of bin will be used
  Stub := nil;
  ResStub := nil;
  BinStub := nil;
  BinVers := -1;
  FreeAndNil(FSFXBinStream); // dispose of existing (if any)
  try
    // load it either from resource (if bcsfx##.res has been linked to the executable)
    // or by loading from file in SFXPath and check both versions if available
    Stubname := DZRES_SFX;
    Err := False; // resource stub not found
    XPath := Lister.SFX.Path;
    if (Length(XPath) > 1) and (XPath[1] = '>') and (XPath[Length(XPath)] = '<')
    then
    begin
      // must use from resource
      Stubname := Copy(XPath, 2, Length(XPath) - 2);
      if Stubname = '' then
        Stubname := DZRES_SFX;
      ResVers := LoadFromResource(ResStub, Stubname);
      if ResVers < MinVers then
        Err := True;
    end
    else
    begin
      // get from resource if it exists
      ResVers := LoadFromResource(ResStub, DZRES_SFX);
      // load if exists from file
      BinVers := LoadFromBinFile(BinStub, UseBin);
      if UseBin then
        ResVers := 0;
    end;
    if not Err then
    begin
      // decide which will be used
      if (BinVers >= MinVers) and (BinVers >= ResVers) then
        Stub := BinStub
      else
      begin
        if ResVers >= MinVers then
          Stub := ResStub
        else
          Err := True;
      end;
    end;
    if Stub <> nil then
    begin
      FSFXBinStream := TMemoryStream.Create();
      try
        if FSFXBinStream.CopyFrom(Stub, Stub.Size - Sizeof(Integer)) <>
          (Stub.Size - Sizeof(Integer)) then
          raise EZipMaster.CreateMsg(Body, ZE_CopyError, {_LINE_}301, __UNIT__);
        FSFXBinStream.Position := 0;
        if Assigned(Lister.SFX.Icon) then
          ReplaceIcon(FSFXBinStream, SFX.Icon);
        FSFXBinStream.Position := 0;
      except
        FreeAndNil(FSFXBinStream);
      end;
    end;
  finally
    FreeAndNil(ResStub);
    FreeAndNil(BinStub);
  end;
  if Err then
    raise EZipMaster.CreateMsgFmt(Body, ZE_NoZipSFXBin, [Stubname], {_LINE_}315,
      __UNIT__);
  Result := FSFXBinStream <> nil;
end;

function TZMBaseOpr.CurrentZip(MustExist: Boolean; SafePart: Boolean = False)
  : TZMZipReader;
begin
  Result := Lister.CurrentZip(MustExist, SafePart);
end;

function TZMBaseOpr.DetachedSize(Zf: TZMZipReader): Integer;
var
  Data: TZMRawBytes;
  Has64: Boolean;
  Ix: Integer;
  Rec: TZMEntryBase;
  Sz: Integer;
begin
  Result := -1;
  ASSERT(Assigned(Zf), 'no input');
  // Diag('Write file');
  if not Assigned(Zf) then
    Exit;
  if FSFXBinStream = nil then
  begin
    Result := PrepareStub;
    if Result < 0 then
      Exit;
  end;
  Result := FSFXBinStream.Size;

  Has64 := False;
  // add approximate central directory size
  Rec := Zf.FirstRec;
  while Rec <> nil do
  begin
    Result := Result + Sizeof(TZipCentralHeader) + Rec.FileNameLen;
    if Rec.ExtraFieldLength > 4 then
    begin
      Ix := 0;
      Sz := 0;
      Data := Rec.ExtraField;
      if XData(Data, Zip64_data_tag, Ix, Sz) then
      begin
        Result := Result + Sz;
        Has64 := True;
      end;
      if XData(Data, UPath_Data_Tag, Ix, Sz) then
        Result := Result + Sz;
      if XData(Data, NTFS_data_tag, Ix, Sz) and (Sz >= 36) then
        Result := Result + Sz;
    end;
    Rec := Rec.Next;
  end;
  Result := Result + Sizeof(TZipEndOfCentral);
  if Has64 then
  begin
    // also has EOC64
    Inc(Result, Sizeof(TZip64EOCLocator));
    Inc(Result, Zf.Z64VSize);
  end;
end;

function TZMBaseOpr.FinalizeInterimZip(OrigZip: TZMZipReader): Integer;
begin
  if OrigZip = nil then
    OrigZip := Lister.Current;
  OrigZip.File_Reopen(FmOpenRead or FmShareDenyWrite);
  Result := InterimZip.Commit(ZwoZipTime in WriteOptions);
  OrigZip.File_Close;
  InterimZip.File_Close;
  if Result < 0 then
    raise EZipMaster.CreateMsg(Body, Result, {_LINE_}388, __UNIT__);
  PrepareZip(OrigZip);
  // Recreate like orig
  Result := Recreate(InterimZip, OrigZip);
  if Result < 0 then
    raise EZipMaster.CreateMsg(Body, Result, {_LINE_}393, __UNIT__);
end;

function TZMBaseOpr.GetCurrent: TZMZipReader;
begin
  Result := Lister.Current;
end;

function TZMBaseOpr.GetReload: TZMReloads;
begin
  Result := Lister.Reload;
end;

function TZMBaseOpr.GetSuccessCnt: Integer;
begin
  Result := Lister.SuccessCnt;
end;

function TZMBaseOpr.GetZipFileName: string;
begin
  Result := Lister.ZipFileName;
end;

function TZMBaseOpr.LoadFromBinFile(var Stub: TStream;
  var Specified: Boolean): Integer;
begin
{$IFDEF UNICODE}
  Result := LoadFromBinFile1(Stub, Specified, SFXBinUDefault);
  if Result <= 0 then
    Result := LoadFromBinFile1(Stub, Specified, SFXBinDefault);
{$ELSE}
  Result := LoadFromBinFile1(Stub, Specified, SFXBinDefault);
  if Result <= 0 then
    Result := LoadFromBinFile1(Stub, Specified, SFXBinUDefault);
{$ENDIF}
end;

function TZMBaseOpr.LoadFromBinFile1(var Stub: TStream; var Specified: Boolean;
  const DefaultName: string): Integer;
var
  BinExists: Boolean;
  Binpath: string;
  Path: string;
  XPath: string;
begin
  Result := -1;
  Specified := False;
  XPath := SFX.Path;
  Path := XPath;
  // if no name specified use default
  if ExtractFileName(XPath) = '' then
    Path := Path + DefaultName;
  Binpath := Path;
  if (Length(XPath) > 1) and ((XPath[1] = '.') or (ExtractFilePath(XPath) <> ''))
  then
  begin
    // use specified
    Specified := True;
    if XPath[1] = '.' then // relative to program
      Binpath := PathConcat(ExtractFilePath(ParamStr(0)), Path);
    BinExists := FileExists(Binpath);
  end
  else
  begin
    // Try the application directory.
    Binpath := DelimitPath(ExtractFilePath(ParamStr(0)), True) + Path;
    BinExists := FileExists(Binpath);
    if not BinExists then
    begin
      // Try the current directory.
      Binpath := Path;
      BinExists := FileExists(Binpath);
    end;
  end;
  if BinExists then
  begin
    try
      Stub := TFileStream.Create(Binpath, FmOpenRead or FmShareDenyWrite);
      if (Stub.Size > MinStubSize) and (Stub.Size < MaxStubSize) then
      begin
        Stub.ReadBuffer(Result, Sizeof(Integer));
      end;
      Body.TraceFmt('found stub: %s %s', [XPath, VersStr(Result)], {_LINE_}475,
        __UNIT__);
    except
      Result := -5;
    end;
  end;
end;

function TZMBaseOpr.LoadFromResource(var Stub: TStream;
  const Sfxtyp: string): Integer;
var
  Rname: string;
begin
  Result := -2;
  Rname := Sfxtyp;
  Stub := OpenResStream(Rname, RT_RCDATA);
  if (Stub <> nil) and (Stub.Size > MinStubSize) and (Stub.Size < MaxStubSize)
  then
  begin
    Stub.ReadBuffer(Result, Sizeof(Integer));
    Body.TraceFmt('resource stub: %s', [VersStr(Result)], {_LINE_}495,
      __UNIT__);
  end;
end;

function TZMBaseOpr.MapOptionsToStub(Opts: TZMSFXOpts): Word;
begin
  Result := 0;
  if SoAskCmdLine in Opts then
    Result := Result or So_AskCmdLine;
  if SoAskFiles in Opts then
    Result := Result or So_AskFiles;
  if SoHideOverWriteBox in Opts then
    Result := Result or So_HideOverWriteBox;
  if SoAutoRun in Opts then
    Result := Result or So_AutoRun;
  if SoNoSuccessMsg in Opts then
    Result := Result or So_NoSuccessMsg;
  if SoExpandVariables in Opts then
    Result := Result or So_ExpandVariables;
  if SoInitiallyHideFiles in Opts then
    Result := Result or So_InitiallyHideFiles;
  if SoForceHideFiles in Opts then
    Result := Result or So_ForceHideFiles;
  if SoCheckAutoRunFileName in Opts then
    Result := Result or So_CheckAutoRunFileName;
  if SoCanBeCancelled in Opts then
    Result := Result or So_CanBeCancelled;
  if SoCreateEmptyDirs in Opts then
    Result := Result or So_CreateEmptyDirs;
  if SoSuccessAlways in Opts then
    Result := Result or So_SuccessAlways;
end;

function TZMBaseOpr.MapOverwriteModeToStub(Mode: TZMOvrOpts): Word;
begin
  case Mode of
    OvrAlways:
      Result := Som_Overwrite;
    OvrNever:
      Result := Som_Skip;
  else
    Result := Som_Ask;
  end;
end;

function TZMBaseOpr.NewSFXStub: TMemoryStream;
begin
  Result := nil;
  if PrepareStub = 0 then
    Result := ReleaseSFXBin;
end;

procedure TZMBaseOpr.PrepareInterimZip;
var
  CurZip: TZMZipReader;
  Err: Integer;
begin
  CurZip := Lister.Current;
  CreateInterimZip;
  ShowProgress := ZspFull;
  InterimZip.ZipComment := CurZip.ZipComment;
  if (CurZip.OpenRet >= 0) and CurZip.MultiDisk then
  begin
    Err := InterimZip.RefuseWriteSplit;
    if Err <> 0 then
      raise EZipMaster.CreateMsg(Body, Err, {_LINE_}561, __UNIT__);
  end;
  if (not(ZwoDiskSpan in Body.WriteOptions)) and
    (UpperCase(ExtractFileExt(CurZip.ReqFileName)) = EXT_EXE) then
  begin
    InterimZip.UseSFX := True;
    InterimZip.Stub := NewSFXStub;
  end;
end;

function TZMBaseOpr.PrepareStub: Integer;
var
  Cdata: TSFXStringsData;
  Deflater: TZMCompressor;
  Ds: TMemoryStream;
  Err: Integer;
  I: Integer;
  L: Integer;
  Ms: TMemoryStream;
  SFXBlkSize: Integer;
  SFXHead: TSFXFileHeader;
  SFXMsg: string;
  SFXMsgFlags: Word;
  Want: Integer;
begin
  Result := -ZE_Unknown;
  if not CreateStubStream then
  begin
    Result := ZM_Error({_LINE_}589, ZE_Unknown);
    Exit;
  end;
  SFXMsg := SFX.Message;
  SFXMsgFlags := MB_OK;
  if (Length(SFXMsg) >= 1) then
  begin
    Want := 1; // want the lot
    if (Length(SFXMsg) > 1) and (SFXMsg[2] = '|') then
    begin
      case SFXMsg[1] of
        '1':
          SFXMsgFlags := MB_OKCANCEL or MB_ICONINFORMATION;
        '2':
          SFXMsgFlags := MB_YESNO or MB_ICONQUESTION;
        '|':
          Want := 2;
      end;
      if SFXMsgFlags <> MB_OK then
        Want := 3;
    end;
    if Want > 1 then
      SFXMsg := Copy(SFXMsg, Want, 2048);
  end;
  try
    // create header
    SFXHead.Signature := SFX_HEADER_SIG;
    SFXHead.Options := MapOptionsToStub(SFX.Options);
    SFXHead.DefOVW := MapOverwriteModeToStub(SFX.OverwriteMode);
    SFXHead.StartMsgType := SFXMsgFlags;
    Ds := nil;
    Ms := TMemoryStream.Create;
    try
      WriteCommand(Ms, SFX.Caption, Sc_Caption);
      WriteCommand(Ms, SFX.CommandLine, Sc_CmdLine);
      WriteCommand(Ms, SFX.DefaultDir, Sc_Path);
      WriteCommand(Ms, SFX.Message, Sc_StartMsg);
      WriteCommand(Ms, SFX.RegFailPath, Sc_RegFailPath);
      L := 0;
      Ms.WriteBuffer(L, 1);
      // check string lengths
      if Ms.Size > 4000 then
        raise EZipMaster.CreateMsg(Body, ZE_StringTooLong, {_LINE_}631,
          __UNIT__);

      if Ms.Size > 100 then
      begin
        Cdata.USize := Ms.Size;
        Ms.Position := 0;
        Ds := TMemoryStream.Create;
        Deflater := TZMCompressor.Create;
        Deflater.OutStream := Ds;
        Deflater.InStream := Ms;
        Deflater.InSize := Ms.Size;
        Err := Deflater.Prepare(METHOD_DEFLATED);
        if Err = 0 then
          Err := Deflater.Compress; // (8);
        if Err = 0 then
        begin
          Cdata.CSize := Ds.Size;
          if (Ms.Size > (Cdata.CSize + Sizeof(Cdata))) then
          begin
            // use compressed
            Ms.Size := 0;
            Ds.Position := 0;
            Cdata.CRC := Deflater.CRC;
            Ms.WriteBuffer(Cdata, Sizeof(Cdata));
            Ms.CopyFrom(Ds, Ds.Size);
            SFXHead.Options := SFXHead.Options or So_CompressedCmd;
          end;
        end;
      end;
      // DWord Alignment.
      I := Ms.Size and 3;
      if I <> 0 then
        Ms.WriteBuffer(L, 4 - I); // dword align
      SFXBlkSize := Sizeof(TSFXFileHeader) + Ms.Size;
      // // create header
      SFXHead.Size := Word(SFXBlkSize);

      FSFXBinStream.Seek(0, SoFromEnd);
      FSFXBinStream.WriteBuffer(SFXHead, Sizeof(SFXHead));
      L := SFXBlkSize - Sizeof(SFXHead);
      I := Ms.Size;
      if I > 0 then
      begin
        Ms.Position := 0;
        FSFXBinStream.CopyFrom(Ms, I);
        Dec(L, I);
      end;
      // check DWORD align
      if L <> 0 then
        raise EZipMaster.CreateMsg(Body, ZE_InternalError, {_LINE_}681,
          __UNIT__);

      Result := 0;
    finally
      Ms.Free;
      Ds.Free;
    end;
  except
    on E: EZipMaster do
    begin
      FreeAndNil(FSFXBinStream);
      ShowExceptionError(E);
      Result := E.ExtErr;
    end
    else
    begin
      FreeAndNil(FSFXBinStream);
      Result := ZM_Error({_LINE_}699, ZE_Unknown);
    end;
  end;
end;

function TZMBaseOpr.PrepareZip(Zip: TZMZipReader): Integer;
begin
  Result := Zip.RefuseWriteSplit;
  if Result <> 0 then
  begin
    Result := ZM_Error({_LINE_}709, Result);
    Exit;
  end;
  if (UpperCase(ExtractFileExt(Zip.ReqFileName)) = EXT_EXE) then
  begin
    Zip.UseSFX := True;
    Zip.Stub := NewSFXStub;
  end;
  Result := 0;
end;

(* ? TZMBaseOpr.Recreate
  recreate the 'theZip' file from the intermediate result
 to make as SFX
 - theZip.UseSFX is set
 - theZip.Stub must hold the stub to use
*)
function TZMBaseOpr.Recreate(Intermed, TheZip: TZMZipReader): Integer;
var
  Czip: TZMZipReader;
  DestZip: TZMZipCopier;
  DetchSFX: Boolean;
  Detchsz: Integer;
  Existed: Boolean;
  OrigKeepFreeDisk1: Cardinal;
  R: Integer;
  Tmp: string;
  WantNewDisk: Boolean;
begin
  Detchsz := 0;
  DetchSFX := False;
  Existed := (Zfi_Loaded and TheZip.Info) <> 0;
  if TheZip.MultiDisk or ((not Existed) and (ZwoDiskSpan in Lister.WriteOptions))
  then
  begin
    Body.TraceFmt('Recreate multi-part: %s', [TheZip.ReqFileName], {_LINE_}744,
      __UNIT__);
    if TheZip.UseSFX then
      DetchSFX := True;
    Result := ZM_Error({_LINE_}748, ZE_Unknown);
    Intermed.File_Close;
    Czip := TheZip;
    // theZip must have proper stub
    if DetchSFX and not Assigned(Czip.Stub) then
    begin
      Result := ZM_Error({_LINE_}754, ZE_SFXCopyError);
      // no stub available - cannot convert
      Exit;
    end;
    WantNewDisk := True; // assume Require to ask for new disk
    if Existed then
    begin
      Czip.SeekDisk(0, True); // ask to enter the first disk again
      Czip.File_Close;     ////****
      WantNewDisk := False;
    end;
    Tmp := TheZip.ReqFileName;
    if DetchSFX then
    begin
      Body.Trace('Recreate detached SFX', {_LINE_}768, __UNIT__);
      // allow room detchSFX stub
      Detchsz := DetachedSize(Intermed);
      Tmp := ChangeFileExt(Tmp, EXT_ZIP); // name of the zip files
    end;
    // now create the spanned archive similar to theZip from Intermed
    OrigKeepFreeDisk1 := Span.KeepFreeOnDisk1;
    DestZip := TZMZipCopier.Create(Lister);
    try
      DestZip.ArchiveName := Tmp;
      DestZip.WorkDrive.DriveStr := Tmp;
      DestZip.ReqFileName := TheZip.ReqFileName;
      Span.KeepFreeOnDisk1 := Span.KeepFreeOnDisk1 + Cardinal(Detchsz);
      ShowProgress := ZspExtra;
      DestZip.TotalDisks := 0;
      if DetchSFX and (DestZip.Numbering = ZnsExt) then
        DestZip.Numbering := ZnsName
      else
        DestZip.Numbering := TheZip.Numbering; // number same as source
      DestZip.PrepareWrite(ZwMultiple);
      DestZip.NewDisk := WantNewDisk;
      DestZip.File_Size := Intermed.File_Size; // to calc TotalDisks
      Intermed.File_Open('', FmOpenRead or FmShareDenyWrite);
      DestZip.StampDate := Intermed.FileDate;
      AnswerAll := True;
      R := DestZip.WriteFile(Intermed, True);
      DestZip.File_Close;
      if R < 0 then
        raise EZipMaster.CreateMsg(Body, R, {_LINE_}796, __UNIT__);
      if DetchSFX then
      begin
        DestZip.DiskNr := 0;
        if DestZip.WorkDrive.DriveIsFloppy then
          DestZip.ArchiveName := Tmp
        else
          DestZip.ArchiveName := DestZip.CreateMVFileNameEx(Tmp, False, False);
        DestZip.SeekDisk(0, False);
        DestZip.AssignStub(Czip);
        DestZip.ArchiveName := Tmp; // restore base name
        if WriteDetached(DestZip) >= 0 then
          Result := 0;
      end
      else
        Result := 0;
    finally
      Intermed.File_Close;
      Span.KeepFreeOnDisk1 := OrigKeepFreeDisk1;
      DestZip.Free;
    end;
    TheZip.Invalidate; // must reload
  end
  else
    // not split
    Result := RecreateSingle(Intermed, TheZip); // just copy it
end;

(* ? TZMBaseOpr.RecreateSingle
  Recreate the 'current' file from the intermediate result
 to make as SFX
 - Current.UseSFX is set
 - Current.Stub must hold the stub to use
*)
function TZMBaseOpr.RecreateSingle(Intermed, TheZip: TZMZipReader): Integer;
var
  DestZip: TZMZipCopier;
begin
  TheZip.File_Close;
  Body.TraceFmt('Replacing: %s', [TheZip.ReqFileName], {_LINE_}835, __UNIT__);
  Result := _Z_EraseFile(TheZip.ReqFileName, HowToDelete = HtdAllowUndo);
  if Result > 0 then // ignore file does not exist
  begin
    Body.InformFmt('EraseFile failed for: %s', [TheZip.ReqFileName],
      {_LINE_}840, __UNIT__);
    raise EZipMaster.CreateMsgFmt(Body, ZE_FileError, [TheZip.ReqFileName],
      {_LINE_}842, __UNIT__);
  end;
  // rename/copy Intermed
  AnswerAll := True;
  if Assigned(TheZip.Stub) and TheZip.UseSFX and (Intermed.Sig <> ZfsDOS) then
  begin // rebuild with sfx
    Body.Trace('Rebuild with SFX', {_LINE_}848, __UNIT__);
    Intermed.File_Close;
    Intermed.File_Open('', FmOpenRead or FmShareDenyWrite);
    Result := Intermed.OpenZip(False, False);
    if Result < 0 then
      Exit;
    DestZip := TZMZipCopier.Create(Lister);
    try
      DestZip.AssignStub(TheZip);
      DestZip.UseSFX := True;
      DestZip.StampDate := Intermed.StampDate; // will be 'orig' or now
      DestZip.DiskNr := 0;
      DestZip.ZipComment := TheZip.ZipComment; // keep orig
      ShowProgress := ZspExtra;
      DestZip.File_Create(TheZip.ReqFileName);
      Result := DestZip.WriteFile(Intermed, True);
      Intermed.File_Close;
      DestZip.File_Close;
      if Result < 0 then
        raise EZipMaster.CreateMsg(Body, Result, {_LINE_}867, __UNIT__)
    finally
      DestZip.Free;
    end;
  end
  else
  begin
    TheZip.File_Close;
    Result := Body.PrepareErrMsg(ZE_FileError, [TheZip.ReqFileName],
      {_LINE_}876, __UNIT__);
    if Intermed.File_Rename(TheZip.ReqFileName) then
      Result := 0;
  end;
  TheZip.Invalidate; // changed - must reload
end;

function TZMBaseOpr.ReleaseSFXBin: TMemoryStream;
begin
  Result := FSFXBinStream;
  FSFXBinStream := nil;
end;

// write to intermediate then recreate as original
function TZMBaseOpr.Remake(CurZip: TZMZipReader; ReqCnt: Integer;
  All: Boolean): Integer;
var
  Intermed: TZMZipCopier;
  Res: Integer;
begin
  Result := 0;
  Intermed := TZMZipCopier.Create(Lister);
  try
    if not Intermed.File_CreateTemp(PRE_INTER, '') then
      raise EZipMaster.CreateMsg(Body, ZE_NoOutFile, {_LINE_}900, __UNIT__);
    ShowProgress := ZspFull;
    Intermed.ZipComment := CurZip.ZipComment;
    CurZip.File_Reopen(FmOpenRead or FmShareDenyWrite);
    Res := Intermed.WriteFile(CurZip, All);
    CurZip.File_Close;
    Intermed.File_Close;
    if Res < 0 then
      raise EZipMaster.CreateMsg(Body, Res, {_LINE_}908, __UNIT__);
    Result := Intermed.Count; // number of remaining files
    if (ReqCnt >= 0) and (Result <> ReqCnt) then
      raise EZipMaster.CreateMsg(Body, ZE_InternalError, {_LINE_}911, __UNIT__);
    // Recreate like orig
    Res := Recreate(Intermed, CurZip);
    if Res < 0 then
      raise EZipMaster.CreateMsg(Body, Res, {_LINE_}915, __UNIT__);
  finally
    Intermed.Free; // also delete temp file
  end;
end;

// replaces an icon in an executable file (stream)
procedure TZMBaseOpr.ReplaceIcon(Str: TMemoryStream; OIcon: TIcon);
var
  Bad: Boolean;
  HdrSection: TImageSectionHeader;
  I: Integer;
  OriInfo: BitmapInfoHeader;
  PIDE: PIconDirEntry;
  RecIcon: TImageResourceDataEntry;
  StrIco: TMemoryStream;
begin
  Bad := True;
  Lister.LocateFirstIconHeader(Str, HdrSection, RecIcon);
  Str.Seek(Integer(HdrSection.PointerToRawData) -
    Integer(HdrSection.VirtualAddress) + Integer(RecIcon.OffsetToData),
    SoFromBeginning);
  if (Str.Read(OriInfo, Sizeof(BitmapInfoHeader)) <> Sizeof(BitmapInfoHeader))
  then
    raise EZipMaster.CreateMsg(Body, ZE_NoCopyIcon, {_LINE_}939, __UNIT__);

  // now check the icon
  StrIco := TMemoryStream.Create;
  try
    if WriteIconToStream(StrIco, OIcon.Handle, OriInfo.BiWidth,
      OriInfo.BiHeight div 2, OriInfo.BiBitCount) <= 0 then
      raise EZipMaster.CreateMsg(Body, ZE_NoIcon, {_LINE_}946, __UNIT__);

    // now search for matching icon
    with PIconDir(StrIco.Memory)^ do
    begin
      if (ResType <> RES_ICON) or (ResCount < 1) or (Reserved <> 0) then
        raise EZipMaster.CreateMsg(Body, ZE_NoIcon, {_LINE_}952, __UNIT__);

      for I := 0 to Pred(ResCount) do
      begin
        PIDE := PIconDirEntry(PAnsiChar(StrIco.Memory) + Sizeof(TIconDir) +
          (I * Sizeof(TIconDirEntry)));
        if (PIDE^.DwBytesInRes = RecIcon.Size) and (PIDE^.BReserved = 0) then
        begin
          // matching icon found, replace
          StrIco.Seek(PIDE^.DwImageOffset,
{$IFDEF VERPre6}Word{$ENDIF}(TSeekOrigin(SoFromBeginning)));
          Str.Seek(Integer(HdrSection.PointerToRawData) -
            Integer(HdrSection.VirtualAddress) + Integer(RecIcon.OffsetToData),
            SoFromBeginning);
          if Str.CopyFrom(StrIco, RecIcon.Size) <> Integer(RecIcon.Size) then
            raise EZipMaster.CreateMsg(Body, ZE_NoCopyIcon, {_LINE_}967,
              __UNIT__);

          // ok and out
          Bad := False;
        end;
      end;
    end;
  finally
    StrIco.Free;
  end;
  if Bad then
    // no icon copied, so none of matching size found
    raise EZipMaster.CreateMsg(Body, ZE_NoIconFound, {_LINE_}980, __UNIT__);
end;

procedure TZMBaseOpr.SetCurrent(const Value: TZMZipReader);
begin
  Lister.Current := Value;
end;

procedure TZMBaseOpr.SetInterimZip(const Value: TZMZipWriter);
begin
  if Value <> FInterimZip then
  begin
    FInterimZip.Free;
    FInterimZip := Value;
  end;
end;

procedure TZMBaseOpr.SetReload(const Value: TZMReloads);
begin
  Lister.Reload := Value;
end;

procedure TZMBaseOpr.SetSuccessCnt(const Value: Integer);
begin
  Lister.SuccessCnt := Value;
end;

procedure TZMBaseOpr.VerifySource(SrcZip: TZMZipReader);
begin
  if not Assigned(SrcZip) then
    raise EZipMaster.CreateMsg(Body, ZE_NothingToDo, {_LINE_}1010, __UNIT__);
  if (SrcZip.Info and Zfi_Cancelled) <> 0 then
    raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}1012, __UNIT__);
  if (SrcZip.Info and Zfi_Loaded) = 0 then
    raise EZipMaster.CreateMsg(Body, ZE_InvalidZip, {_LINE_}1014, __UNIT__);
end;

function TZMBaseOpr.WriteDetached(Zf: TZMZipReader): Integer;
var
  Xf: TZMLoader;
begin
  Body.Trace('Write detached SFX stub', {_LINE_}1021, __UNIT__);
  Xf := TZMLoader.Create(Lister, Self);
  try
    Xf.ForZip := Zf;
    if Xf.File_Create(ChangeFileExt(Zf.ArchiveName, DotExtExe)) then
      Result := Xf.Commit(False)
    else
      Result := Body.PrepareErrMsg(ZE_FileCreate, [Xf.ArchiveName],
        {_LINE_}1029, __UNIT__);
  finally
    Xf.Free;
  end;
end;

// returns size or 0 on error or wrong dimensions
function TZMBaseOpr.WriteIconToStream(Stream: TStream; Icon: HICON;
  Width, Height, Depth: Integer): Integer;
type
  PIconRec = ^TIconRec;

  TIconRec = packed record
    IDir: TIconDir;
    IEntry: TIconDirEntry;
  end;
const
  RC3_ICON = 1;
var
  BI: PBITMAPINFO;
  BIsize: Integer;
  CBits: PByte;
  Cbm: Bitmap;
  Cofs: Integer;
  Colors: Integer;
  Dc: HDC;
  Ico: TIconRec;
  IconInfo: TIconInfo;
  MBI: BitMapInfo;
  MBits: PByte;
  Mofs: Integer;
begin
  Result := 0;

  if (Depth <= 4) then
    Depth := 4
  else
    if (Depth <= 8) then
      Depth := 8
    else
      if (Depth <= 16) then
        Depth := 16
      else
        if (Depth <= 24) then
          Depth := 24
        else
          Exit;
  Colors := 1 shl Depth;

  BI := nil;
  Dc := 0;
  if GetIconInfo(Icon, IconInfo) then
  begin
    try
      ZeroMemory(@Ico, Sizeof(TIconRec));
      if GetObject(IconInfo.HbmColor, Sizeof(Bitmap), @Cbm) = 0 then
        Exit;
      if (Width <> Cbm.BmWidth) or (Height <> Cbm.BmHeight) then
        Exit;

      // ok should be acceptable
      BIsize := Sizeof(BitmapInfoHeader);
      if (Depth <> 24) then
        Inc(BIsize, Colors * Sizeof(RGBQUAD)); // pallet

      Cofs := BIsize; // offset to colorbits
      Inc(BIsize, (Width * Height * Depth) div 8); // bits
      Mofs := BIsize; // offset to maskbits
      Inc(BIsize, (Width * Height) div 8);

      // allocate memory for it
      GetMem(BI, BIsize);

      ZeroMemory(BI, BIsize);
      // set required attributes for colour bitmap
      BI^.BmiHeader.BIsize := Sizeof(BitmapInfoHeader);
      BI^.BmiHeader.BiWidth := Width;
      BI^.BmiHeader.BiHeight := Height;
      BI^.BmiHeader.BiPlanes := 1;
      BI^.BmiHeader.BiBitCount := Depth;
      BI^.BmiHeader.BiCompression := BI_RGB;

      CBits := PByte(BI);
      Inc(CBits, Cofs);

      // prepare for mono mask bits
      ZeroMemory(@MBI, Sizeof(BitMapInfo));
      MBI.BmiHeader.BIsize := Sizeof(BitmapInfoHeader);
      MBI.BmiHeader.BiWidth := Width;
      MBI.BmiHeader.BiHeight := Height;
      MBI.BmiHeader.BiPlanes := 1;
      MBI.BmiHeader.BiBitCount := 1;

      MBits := PByte(BI);
      Inc(MBits, Mofs);

      Dc := CreateCompatibleDC(0);
      if Dc <> 0 then
      begin
        if GetDIBits(Dc, IconInfo.HbmColor, 0, Height, CBits, BI^,
          DIB_RGB_COLORS) > 0 then
        begin
          // ok get mask bits
          if GetDIBits(Dc, IconInfo.HbmMask, 0, Height, MBits, MBI,
            DIB_RGB_COLORS) > 0 then
          begin
            // good we have both
            DeleteDC(Dc); // release it quick before anything can go wrong
            Dc := 0;
            Ico.IDir.ResType := RC3_ICON;
            Ico.IDir.ResCount := 1;
            Ico.IEntry.BWidth := Width;
            Ico.IEntry.BHeight := Height;
            Ico.IEntry.BColorCount := Depth;
            Ico.IEntry.DwBytesInRes := BIsize;
            Ico.IEntry.DwImageOffset := Sizeof(TIconRec);
            BI^.BmiHeader.BiHeight := Height * 2;
            // color height includes mask bits
            Inc(BI^.BmiHeader.BiSizeImage, MBI.BmiHeader.BiSizeImage);
            if (Stream <> nil) then
            begin
              Stream.Write(Ico, Sizeof(TIconRec));
              Stream.Write(BI^, BIsize);
            end;
            Result := BIsize + Sizeof(TIconRec);
          end;
        end;
      end;
    finally
      if Dc <> 0 then
        DeleteDC(Dc);
      DeleteObject(IconInfo.HbmColor);
      DeleteObject(IconInfo.HbmMask);
      if BI <> nil then
        FreeMem(BI);
    end;
  end
  else
{$IFNDEF VERD6up}
    RaiseLastWin32Error;
{$ELSE}
    RaiseLastOSError;
{$ENDIF}
end;

function TZMBaseOpr.ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
  Btns: TMsgDlgButtons): TModalResult;
begin
  Result := Lister.ZipMessageDlgEx(Title, Msg, Context, Btns);
end;

constructor TZMLoader.Create(TheLister: TZMLister; TheOpr: TZMBaseOpr);
begin
  inherited Create(TheLister);
  FOpr := TheOpr;
end;

function TZMLoader.AddStripped(const Rec: TZMEntryBase): Integer;
var
  Data: TZMRawBytes;
  Idx: Integer;
  IxN: Integer;
  IxU: Integer;
  IxZ: Integer;
  NData: TZMRawBytes;
  Ni: TZMRawBytes;
  NRec: TZMEntryDetached;
  Siz: Integer;
  SzN: Integer;
  SzU: Integer;
  SzZ: Integer;
begin
  IxZ := 0;
  SzZ := 0;
  IxU := 0;
  SzU := 0;
  IxN := 0;
  SzN := 0;
  NRec := TZMEntryDetached.Create(Self);
  NRec.VersionMadeBy := Rec.VersionMadeBy;
  NRec.VersionNeeded := Rec.VersionNeeded;
  NRec.Flag := Rec.Flag;
  NRec.CompressionMethod := Rec.CompressionMethod;
  NRec.ModifDateTime := Rec.ModifDateTime;
  NRec.CRC32 := Rec.CRC32;
  NRec.CompressedSize := Rec.CompressedSize;
  NRec.UncompressedSize := Rec.UncompressedSize;
  NRec.StartOnDisk := Rec.StartOnDisk;
  NRec.IntFileAttrib := Rec.IntFileAttrib;
  NRec.ExtFileAttrib := Rec.ExtFileAttrib;
  NRec.RelOffLocalHdr := Rec.RelOffLocalHdr;
  NRec.StatusBits := Rec.StatusBits;
  NData := '';
  Siz := 0;
  Ni := Rec._FileName;
  if Rec.ExtraFieldLength > 4 then
  begin
    Data := Rec.ExtraField;
    if XData(Data, Zip64_data_tag, IxZ, SzZ) then
      Siz := Siz + SzZ;
    if XData(Data, UPath_Data_Tag, IxU, SzU) then
      Siz := Siz + SzU;
    if XData(Data, NTFS_data_tag, IxN, SzN) and (SzN >= 36) then
      Siz := Siz + SzN;
  end;
  NRec._FileName := Ni;
  NRec.FileName := Rec.FileName;
  if Siz > 0 then
  begin
    // copy required extra data fields
    SetLength(NData, Siz);
    Idx := 1;
    if SzZ > 0 then
      Move(Data[IxZ], NData[Idx], SzZ); // Zip64
    Inc(Idx, SzZ);
    if SzU > 0 then
      Move(Data[IxU], NData[Idx], SzU); // UPath
    Inc(Idx, SzU);
    if SzN >= 36 then
      Move(Data[IxN], NData[Idx], SzN); // NTFS
    NRec.ExtraField := NData;
    NData := '';
  end;
  NRec.Status[ZsbVChanged] := False;
  Result := Add(NRec);
  if Result < 0 then
  begin
    NRec.Free; // could not add it
    Result := ZM_Error({_LINE_}1257, ZE_InternalError);
  end;
end;

procedure TZMLoader.AfterConstruction;
begin
  inherited;
  ForZip := nil;
//  Fname := '';
  DiskNr := MAX_WORD - 1;
end;

function TZMLoader.BeforeCommit: Integer;
begin
  Result := inherited BeforeCommit;
  // Prepare detached header
  if Result = 0 then
  begin
    if Count < 0 then
      raise EZipMaster.CreateMsg(Body, ZE_NothingToDo, {_LINE_}1276, __UNIT__);
    StampDate := ForZip.StampDate;
    Result := PrepareDetached;
  end;
end;

function TZMLoader.FixHeaderNames: Integer;
begin
  Result := 0;
end;

function TZMLoader.PrepareDetached: Integer;
begin
  if not Assigned(Stub) then
  begin
    Result := Opr.PrepareStub;
    if Result < 0 then
      Exit; // something went wrong
    Stub := Opr.ReleaseSFXBin; // we now own it
  end;
  UseSFX := True;
  Result := 0;
end;

procedure TZMLoader.SetForZip(const Value: TZMZipReader);
begin
  if ForZip <> Value then
  begin
    FForZip := Value;
    ClearEntries;
    StripEntries;
    DiskNr := ForZip.DiskNr + 1;
  end;
end;

function TZMLoader.StripEntries: Integer;
var
  Rec: TZMEntryBase;
begin
  Result := ZM_Error({_LINE_}1315, ZE_NothingToDo);
  // fill list from ForFile
  Rec := ForZip.FirstRec;
  while Rec <> nil do
  begin
    Result := AddStripped(Rec);
    if Result < 0 then
      Break;
    Rec := Rec.Next;
  end;
end;

{ TZMEntryDetached }
function TZMEntryDetached.Process: Int64;
begin
  Result := 0;
end;

function TZMEntryDetached.ProcessSize: Int64;
begin
  Result := 0;
end;

end.



