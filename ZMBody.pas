unit ZMBody;

// ZMBody.pas -  Properties and methods used by all 'operations'
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

interface

{$INCLUDE   '.\ZipVers.inc'}
{.$DEFINE DEBUG_PROGRESS }

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.Graphics,
{$ELSE}
  Classes, Windows, SysUtils, Controls, Forms, Dialogs, Graphics,
  IniFiles, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMHandler, ZMCore;

type
  TZXtraProgress = (ZxArchive, ZxCopyZip, ZxSFX, ZxHeader, ZxFinalise,
    ZxCopying, ZxCentral, ZxChecking, ZxLoading, ZxJoining, ZxSplitting,
    ZxWriting, ZxPreCalc, ZxProcessing, ZxMerging);

type
  TZMLoadOpts = (ZloNoLoad, ZloFull, ZloSilent);

type
  TZMEncodingDir = (ZedFromInt, ZedToInt);
  TZipShowProgress = (ZspNone, ZspFull, ZspExtra);

const
  EXT_EXE = '.EXE';
  EXT_EXEL = '.exe';
  EXT_ZIP = '.ZIP';
  EXT_ZIPL = '.zip';
  PRE_INTER = 'ZI$';
  PRE_SFX = 'ZX$';

const
  ZPasswordFollows = '<';
  ZSwitchFollows = '|';
  ZForceNoRecurse = '|'; // leading
  ZForceRecurse = '>'; // leading

const
  OUR_VEM = 30;
  Def_VER = 20;

type
  TZipNameType = (ZntExternal, ZntInternal);

type
  TZipNumberScheme = (ZnsNone, ZnsVolume, ZnsName, ZnsExt);

type
  TZCentralValues = (ZcvDirty, ZcvEmpty, ZcvError, ZcvBadStruct, ZcvBusy);
  TZCentralStatus = set of TZCentralValues;

type
  TZMSpanParameters = class
  private
    FKeepFreeOnAllDisks: Cardinal;
    FKeepFreeOnDisk1: Cardinal;
    FMaxVolumeSize: Int64;
    FMaxVolumeSizeKb: Integer;
    FMinFreeVolumeSize: Cardinal;
    FOptions: TZMSpanOpts;
  public
    property KeepFreeOnAllDisks: Cardinal read FKeepFreeOnAllDisks
      write FKeepFreeOnAllDisks;
    property KeepFreeOnDisk1: Cardinal read FKeepFreeOnDisk1
      write FKeepFreeOnDisk1;
    property MaxVolumeSize: Int64 read FMaxVolumeSize write FMaxVolumeSize;
    property MaxVolumeSizeKb: Integer read FMaxVolumeSizeKb
      write FMaxVolumeSizeKb;
    property MinFreeVolumeSize: Cardinal read FMinFreeVolumeSize
      write FMinFreeVolumeSize;
    property Options: TZMSpanOpts read FOptions write FOptions;
  end;

type
  TZMSFXParameters = class
  private
    FCaption: string;
    FCommandLine: string;
    FDefaultDir: string;
    FIcon: TIcon;
    FMessage: string;
    FOptions: TZMSFXOpts;
    FOverwriteMode: TZMOvrOpts;
    FPath: string;
    FRegFailPath: string;
    procedure SetIcon(const Value: TIcon);
  public
    procedure BeforeDestruction; override;
    property Caption: string read FCaption write FCaption;
    property CommandLine: string read FCommandLine write FCommandLine;
    property DefaultDir: string read FDefaultDir write FDefaultDir;
    property Icon: TIcon read FIcon write SetIcon;
    property Message: string read FMessage write FMessage;
    property Options: TZMSFXOpts read FOptions write FOptions;
    property OverwriteMode: TZMOvrOpts read FOverwriteMode write FOverwriteMode;
    property Path: string read FPath write FPath;
    property RegFailPath: string read FRegFailPath write FRegFailPath;
  end;

type
  TZMProgress = class(TZMProgressDetails)
  private
    FCore: TZMCore;
    FDelta: Int64;
    FInBatch: Boolean;
    FItemName: string;
    FItemNumber: Integer;
    FItemPosition: Int64;
    FItemSize: Int64;
    FOnChange: TNotifyEvent;
    FProgType: TZMProgressType;
    FStop: Boolean;
    FTotalCount: Int64;
    FTotalPosition: Int64;
    FTotalSize: Int64;
    FWritten: Int64;
    procedure GiveProgress(DoEvent: Boolean);
    procedure SetTotalCount(const Value: Int64);
    procedure SetTotalSize(const Value: Int64);
  protected
    function GetBytesWritten: Int64; override;
    function GetDelta: Int64; override;
    function GetItemName: string; override;
    function GetItemNumber: Integer; override;
    function GetItemPosition: Int64; override;
    function GetItemSize: Int64; override;
    function GetOrder: TZMProgressType; override;
    function GetStop: Boolean; override;
    function GetTotalCount: Int64; override;
    function GetTotalPosition: Int64; override;
    function GetTotalSize: Int64; override;
    procedure SetStop(const Value: Boolean); override;
  public
    constructor Create(TheCore: TZMCore);
    procedure Advance(Adv: Int64);
    procedure AdvanceXtra(Adv: Cardinal);
    procedure Clear;
    procedure EndBatch;
    procedure EndItem;
    procedure MoreWritten(More: Int64);
    procedure NewItem(const FName: string; FSize: Int64);
    procedure NewXtraItem(const Xmsg: string; FSize: Int64); overload;
    procedure NewXtraItem(Xtra: TZXtraProgress; XSize: Integer); overload;
    procedure Written(Bytes: Int64);
    property BytesWritten: Int64 read GetBytesWritten write FWritten;
    property InBatch: Boolean read FInBatch;
    property ItemName: string read GetItemName write FItemName;
    property ItemNumber: Integer read GetItemNumber write FItemNumber;
    property ItemPosition: Int64 read GetItemPosition write FItemPosition;
    property ItemSize: Int64 read GetItemSize write FItemSize;
    property Order: TZMProgressType read GetOrder write FProgType;
    property TotalCount: Int64 read GetTotalCount write SetTotalCount;
    property TotalPosition: Int64 read GetTotalPosition write FTotalPosition;
    property TotalSize: Int64 read GetTotalSize write SetTotalSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TZMBody = class(TZMCore)
  private
    FAddCompLevel: Integer;
    FAddFrom: TDateTime;
    FAddOptions: TZMAddOpts;
    FAddStoreSuffixes: TZMAddStoreExts;
    FAnswerAll: Boolean;
    FAuxChanged: Boolean;
    FConfirmErase: Boolean;
    FEncodeAs: TZMEncodingOpts;
    FEncoding: TZMEncodingOpts;
    FEncoding_CP: Cardinal;
    FExcludeSpecs: TStrings;
    FExtAddStoreSuffixes: string;
    FExtrBaseDir: string;
    FExtrOptions: TZMExtrOpts;
    FHowToDelete: TZMDeleteOpts;
    FIncludeSpecs: TStrings;
    FIsDestructing: Boolean;
    FNoReadAux: Boolean;
    FPassword: string;
    FPasswordReqCount: Longword;
    FProgress: TZMProgress;
    FRootDir: string;
    FSFX: TZMSFXParameters;
    FShowProgress: TZipShowProgress;
    FSpan: TZMSpanParameters;
    FSuccessCnt: Integer;
    FTempDir: string;
    FTotalSizeToProcess: Int64;
    FWriteOptions: TZMWriteOpts;
    FZipStream: TMemoryStream;
    function GetPassword: string;
    function GetTotalWritten: Int64;
    function GetZipStream: TMemoryStream;
    procedure SetAddStoreSuffixes(const Value: TZMAddStoreExts);
    procedure SetExcludeSpecs(const Value: TStrings);
    procedure SetExtAddStoreSuffixes(const Value: string);
    procedure SetIncludeSpecs(const Value: TStrings);
    procedure SetNoReadAux(const Value: Boolean);
    procedure SetPassword(const Value: string);
    procedure SetTotalWritten(const Value: Int64);
    procedure SetZipStream(const Value: TMemoryStream);
  protected
    procedure CancelSet(Value: Integer);
    procedure ProgressChanged(Sender: TObject);
    procedure SetEncoding(const Value: TZMEncodingOpts); virtual;
    procedure SetEncoding_CP(const Value: Cardinal); virtual;
    procedure Started; virtual;
  public
    procedure AfterConstruction; override;
    procedure AuxWasChanged;
    procedure BeforeDestruction; override;
    procedure CheckCancel;
    procedure Clear; virtual;
    procedure ClearExcludeSpecs;
    procedure ClearIncludeSpecs;
    function KeepAlive: Boolean;
    procedure Log(Err: Cardinal; const Msg: string); override;
    procedure LogSpecs(const Desc: string);
    function Skipping(const Spec: string; Reason: TZMSkipTypes;
      Err: Integer): Boolean;
    function ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
      Btns: TMsgDlgButtons): TModalResult;
    function _GetAddPassword(var Response: TMsgDlgBtn): string;
    function _GetExtrPassword(var Response: TMsgDlgBtn): string;
    function _GetPassword(const DialogCaption, MsgTxt: string; Ctx: Integer;
      Pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
    property AddCompLevel: Integer read FAddCompLevel write FAddCompLevel;
    property AddFrom: TDateTime read FAddFrom write FAddFrom;
    property AddOptions: TZMAddOpts read FAddOptions write FAddOptions;
    property AddStoreSuffixes: TZMAddStoreExts read FAddStoreSuffixes
      write SetAddStoreSuffixes;
    property AnswerAll: Boolean read FAnswerAll write FAnswerAll;
    property AuxChanged: Boolean read FAuxChanged write FAuxChanged;
    property ConfirmErase: Boolean read FConfirmErase write FConfirmErase;
    property EncodeAs: TZMEncodingOpts read FEncodeAs write FEncodeAs;
    // 1 Filename and comment character encoding
    property Encoding: TZMEncodingOpts read FEncoding write SetEncoding;
    // 1 codepage to use to decode filename
    property Encoding_CP: Cardinal read FEncoding_CP write SetEncoding_CP;
    property ExcludeSpecs: TStrings read FExcludeSpecs write SetExcludeSpecs;
    property ExtAddStoreSuffixes: string read FExtAddStoreSuffixes
      write SetExtAddStoreSuffixes;
    property ExtrBaseDir: string read FExtrBaseDir write FExtrBaseDir;
    property ExtrOptions: TZMExtrOpts read FExtrOptions write FExtrOptions;
    property HowToDelete: TZMDeleteOpts read FHowToDelete write FHowToDelete;
    property IncludeSpecs: TStrings read FIncludeSpecs write SetIncludeSpecs;
    property IsDestructing: Boolean read FIsDestructing write FIsDestructing;
    property NoReadAux: Boolean read FNoReadAux write SetNoReadAux;
    property Password: string read GetPassword write SetPassword;
    property PasswordReqCount: Longword read FPasswordReqCount
      write FPasswordReqCount;
    property Progress: TZMProgress read FProgress;
    property RootDir: string read FRootDir write FRootDir;
    property SFX: TZMSFXParameters read FSFX;
    property ShowProgress: TZipShowProgress read FShowProgress
      write FShowProgress;
    property Span: TZMSpanParameters read FSpan;
    property SuccessCnt: Integer read FSuccessCnt write FSuccessCnt;
    property TempDir: string read FTempDir write FTempDir;
    property TotalSizeToProcess: Int64 read FTotalSizeToProcess;
    property TotalWritten: Int64 read GetTotalWritten write SetTotalWritten;
    property WriteOptions: TZMWriteOpts read FWriteOptions write FWriteOptions;
    property ZipStream: TMemoryStream read GetZipStream write SetZipStream;
  end;

type
  TZMBase = class
  private
    FBody: TZMBody;
    FErrors: TZMErrors;
    FExcludeSpecs: TStrings;
    FHowToDelete: TZMDeleteOpts;
    FIncludeSpecs: TStrings;
    FIsTrace: Boolean;
    FIsVerbose: Boolean;
    FMaster: TCustomZipMaster;
    FProblemList: TStrings;
    FProgress: TZMProgress;
    FSFX: TZMSFXParameters;
    FSpan: TZMSpanParameters;
    FTempDir: string;
    FUnattended: Boolean;
    FVerbosity: TZMVerbosity;
    FWriteOptions: TZMWriteOpts;
    function GetAnswerAll: Boolean;
    function GetCancel: Integer;
    function GetShowProgress: TZipShowProgress;
    function GetSuccessCnt: Integer;
    procedure SetAnswerAll(const Value: Boolean);
    procedure SetCancel(const Value: Integer);
    procedure SetShowProgress(const Value: TZipShowProgress);
    procedure SetSuccessCnt(const Value: Integer);
  protected
    function Skipping(const Spec: string; Reason: TZMSkipTypes;
      Err: Integer): Boolean;
    procedure ReportMessage(Err: Integer; const Msg: string);
    procedure ReportMsg(Id: Integer; const Args: array of const);
    // Show* optionally show ZipDialog before 'Report'
    procedure ShowError(Error: Integer);
    function ShowErrorEx(Ident: Integer; LineNo, UnitNo: Integer): Integer;
    procedure ShowExceptionError(const ZMExcept: Exception);
    procedure ShowMessage(Ident: Integer; const UserStr: string);
  public
    constructor Create(TheBody: TZMBody);
    procedure AfterConstruction; override;
    procedure CheckCancel;
    function KeepAlive: Boolean;
    function ZipFmtLoadStr(Id: Integer; const Args: array of const): string;
    function ZipLoadStr(Id: Integer): string;
    function ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
      Btns: TMsgDlgButtons): TModalResult;
    property AnswerAll: Boolean read GetAnswerAll write SetAnswerAll;
    property Body: TZMBody read FBody;
    property Cancel: Integer read GetCancel write SetCancel;
    property Errors: TZMErrors read FErrors;
    property ExcludeSpecs: TStrings read FExcludeSpecs;
    property HowToDelete: TZMDeleteOpts read FHowToDelete;
    property IncludeSpecs: TStrings read FIncludeSpecs;
    property IsTrace: Boolean read FIsTrace;
    property IsVerbose: Boolean read FIsVerbose;
    property Master: TCustomZipMaster read FMaster;
    property ProblemList: TStrings read FProblemList;
    property Progress: TZMProgress read FProgress;
    property SFX: TZMSFXParameters read FSFX;
    property ShowProgress: TZipShowProgress read GetShowProgress
      write SetShowProgress;
    property Span: TZMSpanParameters read FSpan;
    property SuccessCnt: Integer read GetSuccessCnt write SetSuccessCnt;
    property TempDir: string read FTempDir;
    property Unattended: Boolean read FUnattended;
    property Verbosity: TZMVerbosity read FVerbosity;
    property WriteOptions: TZMWriteOpts read FWriteOptions;
  end;

implementation

uses
  ZMUtils, ZMDlg, ZMCtx, ZMXcpt, ZMStructs, ZMMsg;

const
  __UNIT__ = 4;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

constructor TZMProgress.Create(TheCore: TZMCore);
begin
  inherited Create;
  FCore := TheCore;
end;

{ TZMProgress }
procedure TZMProgress.Advance(Adv: Int64);
begin
  FDelta := Adv;
  FTotalPosition := FTotalPosition + Adv;
  FItemPosition := FItemPosition + Adv;
  FProgType := ProgressUpdate;
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#Progress - [inc:%d] ipos:%d isiz:%d, tpos:%d tsiz:%d',
    [Adv, ItemPosition, ItemSize, TotalPosition, TotalSize], {_LINE_}432,
    __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

procedure TZMProgress.AdvanceXtra(Adv: Cardinal);
begin
  FDelta := Adv;
  Inc(FItemPosition, Adv);
  FProgType := ExtraUpdate;
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#XProgress - [inc:%d] pos:%d siz:%d',
    [Adv, ItemPosition, ItemSize], {_LINE_}445, __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

procedure TZMProgress.Clear;
begin
  FProgType := EndOfBatch;
  FDelta := 0;
  FWritten := 0;
  FTotalCount := 0;
  FTotalSize := 0;
  FTotalPosition := 0;
  FItemSize := 0;
  FItemPosition := 0;
  FItemName := '';
  FItemNumber := 0;
  FStop := False;
end;

procedure TZMProgress.EndBatch;
begin
{$IFDEF DEBUG_PROGRESS}
  if FCore.Verbosity >= ZvVerbose then
  begin
    if FInBatch then
      FCore.Trace('#End Of Batch', {_LINE_}471, __UNIT__);
    else
      FCore.Trace('#End Of Batch with no batch', {_LINE_}473, __UNIT__);
  end;
{$ENDIF}
  FItemName := '';
  FItemSize := 0;
  FInBatch := False;
  FProgType := EndOfBatch;
  FStop := False;
  GiveProgress(True);
end;

procedure TZMProgress.EndItem;
begin
  FProgType := EndOfItem;
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#End of Item - "%s"', [ItemName], {_LINE_}488, __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

function TZMProgress.GetBytesWritten: Int64;
begin
  Result := FWritten;
end;

function TZMProgress.GetDelta: Int64;
begin
  Result := FDelta;
end;

function TZMProgress.GetItemName: string;
begin
  Result := FItemName;
end;

function TZMProgress.GetItemNumber: Integer;
begin
  Result := FItemNumber;
end;

function TZMProgress.GetItemPosition: Int64;
begin
  Result := FItemPosition;
end;

function TZMProgress.GetItemSize: Int64;
begin
  Result := FItemSize;
end;

function TZMProgress.GetOrder: TZMProgressType;
begin
  Result := FProgType;
end;

function TZMProgress.GetStop: Boolean;
begin
  Result := FStop;
end;

function TZMProgress.GetTotalCount: Int64;
begin
  Result := FTotalCount;
end;

function TZMProgress.GetTotalPosition: Int64;
begin
  Result := FTotalPosition;
end;

function TZMProgress.GetTotalSize: Int64;
begin
  Result := FTotalSize;
end;

procedure TZMProgress.GiveProgress(DoEvent: Boolean);
begin
  if DoEvent and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TZMProgress.MoreWritten(More: Int64);
begin
  FWritten := FWritten + More;
end;

procedure TZMProgress.NewItem(const FName: string; FSize: Int64);
begin
  if FSize >= 0 then
  begin
    Inc(FItemNumber);
    FItemName := FName;
    FItemSize := FSize;
    FItemPosition := 0;
    FProgType := NewFile;
{$IFDEF DEBUG_PROGRESS}
    FCore.TraceFmt('#Item - "%s" %d', [ItemName, ItemSize], {_LINE_}569,
      __UNIT__);
{$ENDIF}
    GiveProgress(True);
  end
  else
    EndItem;
end;

procedure TZMProgress.NewXtraItem(const Xmsg: string; FSize: Int64);
begin
  FItemName := Xmsg;
  FItemSize := FSize;
  FItemPosition := 0;
  FProgType := NewExtra;
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#XItem - %s size = %d', [ItemName, FSize], {_LINE_}585,
    __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

procedure TZMProgress.NewXtraItem(Xtra: TZXtraProgress; XSize: Integer);
begin
  NewXtraItem(FCore.ZipLoadStr(ZP_Archive + Ord(Xtra) {-1}), XSize);
end;

procedure TZMProgress.SetStop(const Value: Boolean);
begin
  FStop := Value;
end;

procedure TZMProgress.SetTotalCount(const Value: Int64);
begin
  Clear;
  FTotalCount := Value;
  FItemNumber := 0;
  FProgType := TotalFiles2Process;
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#Count - %d', [TotalCount], {_LINE_}608, __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

procedure TZMProgress.SetTotalSize(const Value: Int64);
begin
  FStop := False;
  FTotalSize := Value;
  FTotalPosition := 0;
  FItemName := '';
  FItemSize := 0;
  FItemPosition := 0;
  FProgType := TotalSize2Process;
  FWritten := 0;
  FInBatch := True; // start of batch
{$IFDEF DEBUG_PROGRESS}
  FCore.TraceFmt('#Size - %d', [TotalSize], {_LINE_}625, __UNIT__);
{$ENDIF}
  GiveProgress(True);
end;

procedure TZMProgress.Written(Bytes: Int64);
begin
  FWritten := Bytes;
end;

procedure TZMBody.AfterConstruction;
begin
  inherited;
  FProgress := TZMProgress.Create(Self);
  FProgress.OnChange := ProgressChanged;
  FIncludeSpecs := TZMStringList.Create;
  FExcludeSpecs := TZMStringList.Create;
  FSpan := TZMSpanParameters.Create;
  FSFX := TZMSFXParameters.Create;
  FWriteOptions := [];
  FNoReadAux := False;
  FAuxChanged := False;
  FAddCompLevel := 9; // default to tightest compression
  FAddStoreSuffixes := ZMDefAddStoreSuffixes;
  FEncoding := ZeoAuto;
  FAddFrom := 0;
  FHowToDelete := HtdAllowUndo;
  FPassword := '';
  FPasswordReqCount := 1;
  Span.MinFreeVolumeSize := 65536;
  Span.MaxVolumeSize := 0;
  Span.KeepFreeOnAllDisks := 0;
  Span.KeepFreeOnDisk1 := 0;
  FConfirmErase := False;
end;

procedure TZMBody.AuxWasChanged;
begin
  if (not NoReadAux) or (CsDesigning in Master.ComponentState) or
    (CsLoading in Master.ComponentState) then
    FAuxChanged := True;
end;

procedure TZMBody.BeforeDestruction;
begin
  IsDestructing := True;
  Cancel := ZS_Canceled;
  FreeAndNil(FProgress);
  FreeAndNil(FZipStream);
  FIncludeSpecs.Free;
  FExcludeSpecs.Free;
  FSpan.Free;
  FSFX.Free;
  inherited;
end;

procedure TZMBody.CancelSet(Value: Integer);
begin
  Cancel := Value;
end;

procedure TZMBody.CheckCancel;
begin
  KeepAlive;
  if Cancel <> 0 then
    raise EZMAbort.Create;
end;

procedure TZMBody.Clear;
begin
  ClearExcludeSpecs;
  ClearIncludeSpecs;
  FPasswordReqCount := 1;
  if FZipStream <> nil then
    FreeAndNil(FZipStream);
  KillLogger;
  Progress.Clear;
end;

procedure TZMBody.ClearExcludeSpecs;
begin
  ExcludeSpecs.Clear;
end;

procedure TZMBody.ClearIncludeSpecs;
begin
  IncludeSpecs.Clear;
end;

function TZMBody.GetPassword: string;
begin
  Result := FPassword;
end;

function TZMBody.GetTotalWritten: Int64;
begin
  Result := Progress.BytesWritten;
end;

function TZMBody.GetZipStream: TMemoryStream;
begin
  if FZipStream = nil then
    FZipStream := TMemoryStream.Create;
  Result := FZipStream;
end;

function TZMBody.KeepAlive: Boolean;
var
  DoStop: Boolean;
  TmpCheckTerminate: TZMCheckTerminateEvent;
  TmpTick: TZMTickEvent;
begin
  Result := Cancel <> 0;
  TmpTick := Master.OnTick;
  if Assigned(TmpTick) then
    TmpTick(Self);
  TmpCheckTerminate := Master.OnCheckTerminate;
  if Assigned(TmpCheckTerminate) then
  begin
    DoStop := Cancel <> 0;
    TmpCheckTerminate(Self, DoStop);
    if DoStop then
      Cancel := ZS_Canceled;
  end
  else
    if not NotMainThread then
      Application.ProcessMessages;
end;

procedure TZMBody.Log(Err: Cardinal; const Msg: string);
begin
  if Logger <> nil then
    Logging := Logger.Log(Err, Msg)
  else
    Logging := False;
end;

procedure TZMBody.LogSpecs(const Desc: string);
begin
  if Logging then
    Logging := Logger.LogStrings(Desc + ' -- FSpecArgs', IncludeSpecs);
  if Logging then
    Logging := Logger.LogStrings(Desc + ' -- FSpecArgsExcl', ExcludeSpecs);
end;

// does not get fired for 'Clear'
procedure TZMBody.ProgressChanged(Sender: TObject);
var
  TmpProgress: TZMProgressEvent;
begin
  TmpProgress := Master.OnProgress;
  if Assigned(TmpProgress) then
    TmpProgress(Master, Progress);
  if Progress.Order = TotalSize2Process then
    FTotalSizeToProcess := Progress.TotalSize; // catch it
  KeepAlive;
end;

procedure TZMBody.SetAddStoreSuffixes(const Value: TZMAddStoreExts);
begin
  FAddStoreSuffixes := Value;
end;

procedure TZMBody.SetEncoding(const Value: TZMEncodingOpts);
begin
  FEncoding := Value;
end;

procedure TZMBody.SetEncoding_CP(const Value: Cardinal);
begin
  FEncoding_CP := Value;
end;

procedure TZMBody.SetExcludeSpecs(const Value: TStrings);
begin
  // never change
end;

procedure TZMBody.SetExtAddStoreSuffixes(const Value: string);
begin
  FExtAddStoreSuffixes := Value;
end;

procedure TZMBody.SetIncludeSpecs(const Value: TStrings);
begin
  // never change
end;

procedure TZMBody.SetNoReadAux(const Value: Boolean);
begin
  if FNoReadAux <> Value then
  begin
    FNoReadAux := Value;
    AuxChanged := False;
  end;
end;

procedure TZMBody.SetPassword(const Value: string);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    AuxWasChanged;
  end;
end;

procedure TZMBody.SetTotalWritten(const Value: Int64);
begin
  Progress.Written(Value);
end;

procedure TZMBody.SetZipStream(const Value: TMemoryStream);
begin
  if FZipStream <> Value then
  begin
    if FZipStream <> nil then
      FZipStream.Free;
    FZipStream := Value;
  end;
end;

function TZMBody.Skipping(const Spec: string; Reason: TZMSkipTypes;
  Err: Integer): Boolean;
begin
  Inform('Skipped filespec ' + Spec, {_LINE_}849, __UNIT__);
  ProblemList.AddObject(Spec, TZMProblemItem.Create(AbsErr(Err), Reason));
  Result := ReportSkipping(Spec, Err, Reason);
end;

procedure TZMBody.Started;
begin
  Cancel := 0;
  FTotalSizeToProcess := 0;
end;

function TZMBody.ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
  Btns: TMsgDlgButtons): TModalResult;
var
  M: string;
begin
  M := Msg;
  Result := ZipMessageDialog(Title, M, Context, Btns);
end;

function TZMBody._GetAddPassword(var Response: TMsgDlgBtn): string;
var
  P1: string;
  P2: string;
begin
  P2 := '';
  if Unattended then
    ShowErrorEx(ZE_UnatAddPWMiss, {_LINE_}876, __UNIT__)
  else
  begin
    Response := _GetPassword(ZipLoadStr(ZC_Caption),
      ZipLoadStr(ZC_MessageEnter), DHC_AddPwrd1, MbOkCancel, P1);
    if (Response = MbOK) and (P1 <> '') then
    begin
      Response := _GetPassword(ZipLoadStr(ZC_Caption),
        ZipLoadStr(ZC_MessageConfirm), DHC_AddPwrd2, MbOkCancel, P2);
      if (Response = MbOK) and (P2 <> '') then
        if AnsiCompareStr(P1, P2) <> 0 then
        begin
          ShowErrorEx(ZE_WrongPassword, {_LINE_}888, __UNIT__);
          P2 := '';
        end;
    end;
  end;
  Result := P2;
end;

function TZMBody._GetExtrPassword(var Response: TMsgDlgBtn): string;
begin
  Result := '';
  if Unattended then
    ShowErrorEx(ZE_UnatAddPWMiss, {_LINE_}900, __UNIT__)
  else
    Response := _GetPassword(ZipLoadStr(ZC_Caption),
      ZipLoadStr(ZC_MessageEnter), DHC_ExtrPwrd,
      [MbOK, MbCancel, MbAll], Result);
end;

function TZMBody._GetPassword(const DialogCaption, MsgTxt: string; Ctx: Integer;
  Pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
var
  GModalResult: TModalResult;
  Msg: string;
begin
  Msg := MsgTxt;
  ResultStr := '';
  GModalResult := ZipMessageDialog(DialogCaption, Msg,
    ZmtPassword + (Ctx and MAX_WORD), Pwb);
  case GModalResult of
    MrOk:
      begin
        ResultStr := Msg;
        Result := MbOK;
      end;
    MrCancel:
      Result := MbCancel;
    MrAll:
      Result := MbNoToAll;
  else
    Result := MbAbort;
  end;
end;

procedure TZMSFXParameters.BeforeDestruction;
begin
  if Assigned(FIcon) then
    FIcon.Free;
  inherited;
end;

procedure TZMSFXParameters.SetIcon(const Value: TIcon);
begin
  if Value <> FIcon then
  begin
    if Assigned(Value) and not Value.Empty then
    begin
      if not Assigned(FIcon) then
        FIcon := TIcon.Create;
      FIcon.Assign(Value);
    end
    else
      FreeAndNil(FIcon);
  end;
end;

{ TZMBase }
constructor TZMBase.Create(TheBody: TZMBody);
begin
  inherited Create;
  FBody := TheBody;
end;

procedure TZMBase.AfterConstruction;
begin
  inherited;
  FMaster := Body.Master;
  FProblemList := Body.ProblemList;
  FIncludeSpecs := Body.IncludeSpecs;
  FExcludeSpecs := Body.ExcludeSpecs;
  FHowToDelete := Body.HowToDelete;
  FWriteOptions := Body.WriteOptions;
  FProgress := Body.Progress;
  FSpan := Body.Span;
  FSFX := Body.SFX;
  FErrors := Body.Errors;
  FUnattended := Body.Unattended;
  FVerbosity := Body.Verbosity;
  FIsVerbose := Body.Verbosity >= ZvVerbose;
  FIsTrace := Body.Verbosity >= ZvTrace;
  FTempDir := Body.TempDir;
end;

procedure TZMBase.CheckCancel;
begin
  KeepAlive;
  if Body.Cancel <> 0 then
    raise EZipMaster.CreateMsg(Body, Cancel, {_LINE_}985, __UNIT__);
end;

function TZMBase.GetAnswerAll: Boolean;
begin
  Result := Body.AnswerAll;
end;

function TZMBase.GetCancel: Integer;
begin
  Result := Body.Cancel;
end;

function TZMBase.GetShowProgress: TZipShowProgress;
begin
  Result := Body.ShowProgress;
end;

function TZMBase.GetSuccessCnt: Integer;
begin
  Result := Body.SuccessCnt;
end;

function TZMBase.KeepAlive: Boolean;
begin
  Result := Body.KeepAlive;
end;

function TZMBase.Skipping(const Spec: string; Reason: TZMSkipTypes;
  Err: Integer): Boolean;
begin
  Result := Body.Skipping(Spec, Reason, Err);
end;

procedure TZMBase.ReportMessage(Err: Integer; const Msg: string);
begin
  Body.ReportMessage(Err, Msg);
end;

procedure TZMBase.ReportMsg(Id: Integer; const Args: array of const);
begin
  Body.ReportMsg(Id, Args);
end;

procedure TZMBase.SetAnswerAll(const Value: Boolean);
begin
  Body.AnswerAll := Value;
end;

procedure TZMBase.SetCancel(const Value: Integer);
begin
  Body.Cancel := Value;
end;

procedure TZMBase.SetShowProgress(const Value: TZipShowProgress);
begin
  Body.ShowProgress := Value;
end;

procedure TZMBase.SetSuccessCnt(const Value: Integer);
begin
  Body.SuccessCnt := Value;
end;

procedure TZMBase.ShowError(Error: Integer);
begin
  Body.ShowError(Error);
end;

function TZMBase.ShowErrorEx(Ident: Integer; LineNo, UnitNo: Integer): Integer;
begin
  Result := Body.ShowErrorEx(Ident, LineNo, UnitNo);
end;

procedure TZMBase.ShowExceptionError(const ZMExcept: Exception);
begin
  Body.ShowExceptionError(ZMExcept);
end;

procedure TZMBase.ShowMessage(Ident: Integer; const UserStr: string);
begin
  Body.ShowMessage(Ident, UserStr);
end;

function TZMBase.ZipFmtLoadStr(Id: Integer; const Args: array of const): string;
begin
  Result := Body.ZipFmtLoadStr(Id, Args);
end;

function TZMBase.ZipLoadStr(Id: Integer): string;
begin
  Result := Body.ZipLoadStr(Id);
end;

function TZMBase.ZipMessageDlgEx(const Title, Msg: string; Context: Integer;
  Btns: TMsgDlgButtons): TModalResult;
begin
  Result := Body.ZipMessageDlgEx(Title, Msg, Context, Btns);
end;

end.
