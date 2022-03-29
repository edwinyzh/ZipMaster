unit ZMCore;

// ZMCore.pas -  Properties and methods used by all 'operations'
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
  ZipMstr, ZMHandler;

type
  TZMVerbosity = (ZvOff, ZvVerbose, ZvTrace, ZvNoisy);

type
  TZMLog = class
  public
    procedure CloseLog; virtual; abstract;
    // return true if successful
    function Log(Err: Cardinal; const Msg: string): Boolean; virtual; abstract;
    // return true if successful
    function LogStrings(const Desc: string; const Strs: TStrings): Boolean;
      virtual; abstract;
    // 1 returns <0 _ error, 0 _ off, >0 _ available (>=8 = force verbosity)
    function StartLog: Integer; virtual; abstract;
  end;

type
  TZMStringList = class(TStringList)
  private
  protected
    function GetTextStr: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

type
  TZMProblemItem = class(TZMProblem)
  private
    FErrCode: Integer;
    FSkipCode: TZMSkipTypes;
  protected
    function GetErrCode: Integer; override;
    function GetSkipCode: TZMSkipTypes; override;
  public
    constructor Create(Err: Integer; Skip: TZMSkipTypes);
    function AsString: string;
  end;

type
  TZMCore = class(TZMRoot)
  private
    FCancel: Integer;
    FIsTrace: Boolean;
    FIsVerbose: Boolean;
    FLogger: TZMLog;
    FLogging: Boolean;
    FMaster: TCustomZipMaster;
    FNoSkipping: TZMSkipAborts;
    FNotMainThread: Boolean;
    FProblemList: TStrings;
    FUnattended: Boolean;
    FVerbosity: TZMVerbosity;
    procedure ScribeEx(LineNo, UnitNo: Integer; const Msg: string);
    procedure SetLogger(const Value: TZMLog);
    procedure ShowMsg(const Msg: string; ResID: Integer; Display: Boolean);
  protected
    procedure KillLogger;
    function ReportSkipping(const FName: string; Err: Integer; Typ: TZMSkipTypes):
        Boolean;
    procedure Scribe(Err: Integer; const M: string);
    property Logger: TZMLog read FLogger write SetLogger;
  public
    constructor Create(TheMaster: TCustomZipMaster);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Inform(const Msg: string; LineNo, UnitNo: Integer);
    procedure InformFmt(const Msg: string; const Args: array of const;
      LineNo, UnitNo: Integer);
    procedure InformSys(const Msg: string; LineNo, UnitNo: Integer);
    procedure InformSysFmt(const Msg: string; const Args: array of const;
      LineNo, UnitNo: Integer);
    function KeepAlive: Boolean;
    procedure Log(Err: Cardinal; const Msg: string); virtual; abstract;
    procedure ReportMessage(Err: Integer; const Msg: string);
    procedure ReportMsg(Id: Integer; const Args: array of const);
    procedure ShowError(Error: Integer);
    function ShowErrorEx(Ident: Integer; LineNo, UnitNo: Integer): Integer;
    // Show* optionally show ZipDialog before 'Report'
    procedure ShowExceptionError(const ZMExcept: Exception);
    procedure ShowFmtMessage(Id: Integer; const Args: array of const;
      Display: Boolean);
    procedure ShowMessage(Ident: Integer; const UserStr: string);
    procedure Trace(const Msg: string; LineNo, UnitNo: Integer);
    procedure TraceFmt(const Msg: string; const Args: array of const;
      LineNo, UnitNo: Integer);
    function ZipMessageDialog(const Title: string; var Msg: string;
      Context: Integer; Btns: TMsgDlgButtons): TModalResult;
    property Cancel: Integer read FCancel write FCancel;
    property IsTrace: Boolean read FIsTrace write FIsTrace;
    property IsVerbose: Boolean read FIsVerbose write FIsVerbose;
    property Logging: Boolean read FLogging write FLogging;
    property Master: TCustomZipMaster read FMaster;
    property NoSkipping: TZMSkipAborts read FNoSkipping write FNoSkipping;
    property NotMainThread: Boolean read FNotMainThread write FNotMainThread;
    property ProblemList: TStrings read FProblemList;
    property Unattended: Boolean read FUnattended write FUnattended;
    property Verbosity: TZMVerbosity read FVerbosity write FVerbosity;
  end;

type
  TZMCanal = class(TStream)
  private
    FMyStream: TStream;
    FOwned: Boolean;
    FOwnsStream: Boolean;
  public
    constructor Create(AStream: TStream; OwnsTheStream: Boolean = False;
      FreeMe: Boolean = True);
    procedure BeforeDestruction; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload;
{$IFDEF VERD7up} override {$ELSE} virtual{$ENDIF};
    function Write(const Buffer; Count: Longint): Longint; override;
    property MyStream: TStream read FMyStream;
    property Owned: Boolean read FOwned;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Messages, System.TypInfo,
{$ELSE}
  Messages, TypInfo,
{$ENDIF}
  ZMUtils, ZMDlg, ZMCtx, ZMXcpt, ZMStructs, ZMMsg;

const
  __UNIT__ = 8;

const
  SResourceMissingFor = 'Resource missing for ';
  SNoLanguageStrings = 'Could not load language files';

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

constructor TZMCore.Create(TheMaster: TCustomZipMaster);
begin
  inherited Create(TheMaster);
  FMaster := TheMaster;
end;

procedure TZMCore.AfterConstruction;
begin
  inherited;
  FNotMainThread := False;
  FLogger := nil;
  FUnattended := False;
  FNoSkipping := DefNoSkips;
  FProblemList := TZMStringList.Create;
end;

procedure TZMCore.BeforeDestruction;
begin
  FProblemList.Free;
  FLogger.Free;
  inherited;
end;

procedure TZMCore.Inform(const Msg: string; LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvVerbose then
    ScribeEx(LineNo, UnitNo, Msg);
end;

procedure TZMCore.InformFmt(const Msg: string; const Args: array of const;
  LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvVerbose then
    ScribeEx(LineNo, UnitNo, Format(Msg, Args));
end;

procedure TZMCore.InformSys(const Msg: string; LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvVerbose then
    ScribeEx(LineNo, UnitNo, Msg + '  ' + SysErrorMsg(GetLastError));
end;

procedure TZMCore.InformSysFmt(const Msg: string; const Args: array of const;
  LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvVerbose then
    ScribeEx(LineNo, UnitNo, Format(Msg, Args) + '  ' +
      SysErrorMsg(GetLastError));
end;

function TZMCore.KeepAlive: Boolean;
var
  DoStop: Boolean;
  TmpCheckTerminate: TZMCheckTerminateEvent;
  TmpTick: TZMTickEvent;
begin
  Result := FCancel <> 0;
  TmpTick := Master.OnTick;
  if Assigned(TmpTick) then
    TmpTick(Self);
  TmpCheckTerminate := Master.OnCheckTerminate;
  if Assigned(TmpCheckTerminate) then
  begin
    DoStop := FCancel <> 0;
    TmpCheckTerminate(Self, DoStop);
    if DoStop then
      FCancel := ZS_Canceled;
  end
  else
    if not NotMainThread then
      Application.ProcessMessages;
end;

procedure TZMCore.KillLogger;
begin
  FLogging := False;
  FreeAndNil(FLogger);
end;

procedure TZMCore.ReportMessage(Err: Integer; const Msg: string);
var
  ECode: Integer;
  M: string;
begin
  if Err < 0 then
    Err := -Err;
  ECode := AbsErr(Err);
  M := Msg;
  if ECode <> 0 then
  begin
    if M = '' then
      M := ZipLoadStr(ECode);
    if Errors.Code = 0 then // only catch first
    begin
      Errors.Code := ECode;
      Errors.ExtCode := Err;
      Errors.ErrMessage := M;
    end;
  end;
  Scribe(Err, M);
end;

// for non-error reporting only
procedure TZMCore.ReportMsg(Id: Integer; const Args: array of const);
var
  Msg: string;
begin
  Msg := ZipLoadStr(Id);

  Msg := Format(Msg, Args);
  Scribe(0, Msg);
end;

// return false if skipping allowed
function TZMCore.ReportSkipping(const FName: string; Err: Integer;
  Typ: TZMSkipTypes): Boolean;
var
  Ti: Integer;
  TmpMessage: TZMMessageEvent;
  TmpSkipped: TZMSkippedEvent;
begin
  Result := False;
  if Typ in NoSkipping then
  begin
    if Err = 0 then
      Err := ZE_NoSkipping;
  end;
  Ti := Err;
  if Ti < 0 then
    Ti := -Ti;
  if (Ti <> 0) and (Typ in NoSkipping) then
    Ti := -Ti; // default to abort
  TmpSkipped := Master.OnSkipped;
  if Assigned(TmpSkipped) then
    TmpSkipped(Self, FName, Typ, Ti)
  else
    if Verbosity > ZvOff then
    begin
      TmpMessage := Master.OnMessage;
      if Assigned(TmpMessage) then
        TmpMessage(Self, Err, ZipFmtLoadStr(ZS_Skipped,
          [FName, Ord(Typ)]));
    end;
  if Ti < 0 then
    Result := True; // Skipping not allowed
  if Logging then
    Logger.Log(ZM_Error({_LINE_}332, 0), Format('[Skipped] IN=%d,%d OUT=%d',
      [Err, Ord(Typ), Ord(Result)]));
end;

procedure TZMCore.Scribe(Err: Integer; const M: string);
var
  TmpMessage: TZMMessageEvent;
begin
  if Logging and (Logger <> nil) then
    Logger.Log(Err, M);
  TmpMessage := Master.OnMessage;
  if Assigned(TmpMessage) then
    TmpMessage(Self, AbsErr(Err), M);
  KeepAlive; // process messages or check terminate;
end;

procedure TZMCore.ScribeEx(LineNo, UnitNo: Integer; const Msg: string);
var
  L: Integer;
begin
  if UnitNo > 0 then
    L := (UnitNo shl ZERR_UNIT_SHIFTS) + (LineNo shl ZERR_LINE_SHIFTS)
  else
    L := LineNo; // assume error with extended information
  Scribe(L, Msg);
end;

procedure TZMCore.SetLogger(const Value: TZMLog);
begin
  if FLogger <> Value then
  begin
    FLogger.Free;
    FLogger := Value;
    Logging := False; // until activated
  end;
  if FLogger = nil then
    Logging := False;
end;

procedure TZMCore.ShowError(Error: Integer);
begin
  ShowMsg(ErrMsg(Error), Error, True);
end;

function TZMCore.ShowErrorEx(Ident: Integer; LineNo, UnitNo: Integer): Integer;
begin
  if UnitNo > 0 then
    Result := -((UnitNo shl ZERR_UNIT_SHIFTS) + (LineNo shl ZERR_LINE_SHIFTS) or
      AbsErr(Ident))
  else
    Result := Ident; // assume error with extended information
  ShowError(Result);
end;

procedure TZMCore.ShowExceptionError(const ZMExcept: Exception);
var
  Msg: string;
  ResID: Integer;
begin
  if ZMExcept is EZMException then
  begin
    ResID := EZMException(ZMExcept).ExtErr;
{$IFDEF UNICODE}
    Msg := ZMExcept.Message;
{$ELSE}
    if IsUtf8 then
      Msg := EZMException(ZMExcept).UMessage
    else
      Msg := ZMExcept.Message;
{$ENDIF}
    if AbsErr(ResID) = ZS_Abort then
      Msg := ZipLoadStr(ZS_Abort);
  end
  else
  begin
    ResID := ZM_Error({_LINE_}407, ZE_ExceptErr);
    Msg := ZMExcept.Message;
  end;
  ShowMsg(Msg, ResID, True);
end;

procedure TZMCore.ShowMsg(const Msg: string; ResID: Integer; Display: Boolean);
var
  M: string;
  MsgID: Integer;
begin
  if Display and not Unattended then
  begin
    MsgID := AbsErr(ResID);
    if (MsgID <> ZS_Abort) and (MsgID <> ZS_Canceled) then
    begin
      M := Msg;
      ZipMessageDialog('', M, ZmtInformation + DHC_ZipMessage, [MbOK]);
    end;
  end;

  ReportMessage(ResID, Msg);
end;

procedure TZMCore.ShowFmtMessage(Id: Integer; const Args: array of const;
  Display: Boolean);
var
  S: string;
begin
  S := ZipLoadStr(Id);

  S := Format(S, Args);
  ShowMsg(S, Id, Display);
end;

procedure TZMCore.ShowMessage(Ident: Integer; const UserStr: string);
var
  Msg: string;
begin
  Msg := ZipLoadStr(Ident);
  if UserStr <> '' then
    Msg := Msg + ': ' + UserStr;
  ShowMsg(Msg, Ident, True);
end;

procedure TZMCore.Trace(const Msg: string; LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvTrace then
    ScribeEx(LineNo, UnitNo, 'Trace: ' + Msg);
end;

procedure TZMCore.TraceFmt(const Msg: string; const Args: array of const;
  LineNo, UnitNo: Integer);
begin
  if Verbosity >= ZvTrace then
    ScribeEx(LineNo, UnitNo, 'Trace: ' + Format(Msg, Args));
end;

function TZMCore.ZipMessageDialog(const Title: string; var Msg: string;
  Context: Integer; Btns: TMsgDlgButtons): TModalResult;
var
  Ctx: Integer;
  Dlg: TZipDialogBox;
  S: string;
  T: string;
  TmpZipDialog: TZMDialogEvent;
begin
  T := Title;
  if Title = '' then
    T := Application.Title;
  if Verbosity > ZvOff then
    T := Format('%s   (%d)', [T, Context and MAX_WORD]);
  TmpZipDialog := Master.OnZipDialog;
  if Assigned(TmpZipDialog) then
  begin
    S := Msg;
    Ctx := Context;
    TmpZipDialog(Self, T, S, Ctx, Btns);
    if (Ctx > 0) and (Ctx <= Ord(MrYesToAll)) then
    begin
      Msg := S;
      Result := TModalResult(Ctx);
      Exit;
    end;
  end;
  Dlg := TZipDialogBox.CreateNew2(Application, Context);
  try
    Dlg.Build(T, Msg, Btns, Self);
    Dlg.ShowModal();
    Result := Dlg.ModalResult;
    if Dlg.DlgType = ZmtPassword then
    begin
      if (Result = MrOk) then
        Msg := Dlg.PWrd
      else
        Msg := '';
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TZMStringList }

procedure TZMStringList.Assign(Source: TPersistent);
var
  I: Integer;
  Src: TStrings;
begin
  Clear;
  inherited;
  // remove duplicate reference to object
  if Source is TStrings then
  begin
    Src := TStrings(Source);
    for I := 0 to Src.Count - 1 do
      Src.Objects[I] := nil;
  end;
end;

procedure TZMStringList.BeforeDestruction;
begin
  Clear; // delete any attached objects
  inherited;
end;

// delete any attached objects
procedure TZMStringList.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Count - 1 do
  begin
    if Objects[I] <> nil then
    begin
      Obj := TObject(Objects[I]);
      Objects[I] := nil;
      if (Obj is TZMCanal) then
      begin
        if (TZMCanal(Obj).Owned) then
          Obj.Free;
      end
      else
        if (Obj is TZMProblem) then
          Obj.Free;
    end;
  end;
  inherited;
end;

procedure TZMStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Obj := Objects[Index];
    if Obj <> nil then
    begin
      Objects[Index] := nil;
      if (Obj is TZMCanal) then
      begin
        if (TZMCanal(Obj).Owned) then
          Obj.Free;
      end
      else
        if (Obj is TZMProblem) then
          Obj.Free;
    end;
  end;
  inherited;
end;

function TZMStringList.GetTextStr: string;
var
  I: Integer;
  O: TObject;
  Obj: TObject;
  S: string;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Result := Result + Strings[I] + #13#10;
    Obj := Objects[I];
    while Obj <> nil do
    begin
      O := Obj;
      Obj := nil;
      S := '<UNKNOWN>';
      if O is TZMCanal then
      begin
        S := '<CANAL>';
        if TZMCanal(O).Owned then
          S := S + ', Owned';
      end
      else
        if O is TStream then
          S := '<STREAM>'
        else
          if O is TZMProblemItem then
          begin
            S := '<PROBLEM>, ' + TZMProblemItem(O).AsString;
          end;
      Result := Result + #9 + S + #13#10;
    end;
  end;
end;

constructor TZMProblemItem.Create(Err: Integer; Skip: TZMSkipTypes);
begin
  inherited Create;
  FErrCode := Err;
  FSkipCode := Skip;
end;

function TZMProblemItem.GetErrCode: Integer;
begin
  Result := FErrCode;
end;

function TZMProblemItem.GetSkipCode: TZMSkipTypes;
begin
  Result := FSkipCode;
end;

function TZMProblemItem.AsString: string;
begin
  Result := Format('%d, %d', [Ord(SkipCode), ErrCode]);
end;

constructor TZMCanal.Create(AStream: TStream; OwnsTheStream: Boolean = False;
  FreeMe: Boolean = True);
begin
  inherited Create;
  FMyStream := AStream;
  FOwnsStream := OwnsTheStream;
  FOwned := FreeMe;
end;

procedure TZMCanal.BeforeDestruction;
begin
  inherited;
  if FOwnsStream then
    MyStream.Free;
end;

function TZMCanal.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FMyStream.Read(Buffer, Count);
end;

function TZMCanal.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
{$IFNDEF VERD7up}
  if (Offset < low(Longint)) or (Offset > high(Longint)) then
    raise ERangeError.Create('>2G not supported');
  Result := Seek(Longint(Offset), Ord(Origin));
{$ELSE}
  Result := FMyStream.Seek(Offset, Origin);
{$ENDIF}
end;

function TZMCanal.Seek(Offset: Longint; Origin: Word): Longint;
begin
{$IFNDEF VERD7up}
  Result := FMyStream.Seek(Offset, Origin);
{$ELSE}
  Result := FMyStream.Seek(Int64(Offset), TSeekOrigin(Origin));
{$ENDIF}
end;

function TZMCanal.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FMyStream.Write(Buffer, Count);
end;

end.
