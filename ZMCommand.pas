unit ZMCommand;

// ZMCommand.pas -  Interface to run 'operations'
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
  System.Classes, WinApi.Windows, Vcl.Controls, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils, Controls,
{$ENDIF}
  ZipMstr, ZMHandler, ZMLister, ZMBaseOpr;

type
  TProcMethod = procedure of object;

type
  TZMCommand = class(TZMLister)
  private
    BusyFlag: Integer;
    FActive: Integer;
    FBlockedCnt: Integer;
    FCurWaitCount: Integer;
    FDelayedComment: AnsiString;
    FDelayExtStream: TStream;
    FDelaying: Integer;
    FDLLLoad: Boolean;
    FDLLWorking: TObject;
    FFileName: string;
    FLanguage: string;
    FRes: Integer;
    FSaveCursor: TCursor;
    FState: TZMStates;
{$IFNDEF UNICODE}
    FUseUTF8: Boolean;
{$ENDIF}
    FZipFileName: string;
    Opr: TZMBaseOpr;
    procedure EndRunning;
    procedure ForceStrings;
    function GetActive: Boolean;
    function GetDLL_Load: Boolean;
    procedure LogGlobals;
    function Permitted: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetDLL_Load(const Value: Boolean);
    procedure SetRes(const Value: Integer);
    procedure SetState(const Value: TZMStates);
    procedure StartWaitCursor;
    procedure StopWaitCursor;
    procedure UpdateChanged(TheOperation: TZMOperationRoot);
  protected
    procedure Start(TheOperation: TZMOperationRoot);
    property Res: Integer read FRes write SetRes;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure DoDelays;
    function GetDLL_Version1(ForceLoad: Boolean): string;
    function IsActive: Boolean;
    procedure ReEntered;
    procedure SetLanguage(const Value: string);
{$IFNDEF UNICODE}
    procedure SetUseUTF8(const Value: Boolean);
{$ENDIF}
    function Run(TheOperation: TZMOperationRoot): Integer; override;
    procedure SetExtStream(AStream: TStream);
    procedure SetTheLanguage(const Lang: string);
    procedure SetZipComment(const Value: AnsiString);
    procedure SetZipFileName(const Value: string);
    procedure StateChanged(NewState: TZMStates);
    property Active: Boolean read GetActive write SetActive;
    property BlockedCnt: Integer read FBlockedCnt;
    property DLLWorking: TObject read FDLLWorking write FDLLWorking;
    property DLL_Load: Boolean read GetDLL_Load write SetDLL_Load;
    property Language: string read FLanguage write SetLanguage;
    property State: TZMStates read FState write SetState;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  Vcl.Forms,
{$ELSE}
  Forms, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMOprMod, ZMBody,
  ZMMsg, ZMXcpt, ZMOprMsgStr, ZMDLLLoad, ZMUtils, ZMMisc, ZMCore,
  ZMOprCore;

const
  __UNIT__ = 6;

const
  DelayingLanguage = 1;
  DelayingFileName = 4;
  DelayingComment = 8;
  DelayingDLL = 16;
  DelayingExtStream = 32;
{$IFNDEF UNICODE}
  DelayingUseUTF8 = 64;
{$ENDIF}

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{$IFNDEF UNICODE}
type
  TZMOpSetUseUTF8 = class(TZMOperationRoot)
  private
    FUseUTF8: Boolean;
  public
    constructor Create(const Value: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

  { TZMOpSetUseUTF8 }

constructor TZMOpSetUseUTF8.Create(const Value: Boolean);
begin
  inherited Create;
  FUseUTF8 := Value;
end;

function TZMOpSetUseUTF8.Execute(TheBody: TZMHandler): Integer;
var
  Commander: TZMCommand;
  OpLang: TZMOpLanguage;
begin
  Commander := TheBody as TZMCommand;
  Result := 0;
  SetZipUseUTF8(FUseUTF8);
  if Commander.Language <> '' then
  begin
    OpLang := TZMOpLanguage.Create(Commander.Language);
    AnOperation := OpLang;
    Result := OpLang.Execute(Commander);
    if Result < 0 then
      Commander.Language := '';
  end;
  Commander.ClearCachedNames; // force update names
  Commander.Refresh; // reload with updated names
end;

function TZMOpSetUseUTF8.Name: string;
begin
  Result := 'SetUseUTF8';
end;
{$ENDIF}

procedure TZMCommand.AfterConstruction;
begin
  inherited;
  FDelaying := 0;
  BusyFlag := 0;
  FCurWaitCount := 0;
  FDLLWorking := nil;
  FActive := 2;
end;

procedure TZMCommand.DoDelays;
var
  Delay: Integer;
begin
  Delay := FDelaying;
  FDelaying := 0;
{$IFNDEF UNICODE}
  if (Delay and (DelayingUseUTF8)) <> 0 then
  begin
    if Opr <> nil then
      FreeAndNil(Opr);
    if Logger <> nil then
      Logger.Log(0, 'Starting --- SetUseUTF8');
    FUseUTF8 := Master.UseUTF8;
    Run(TZMOpLanguage.Create(FLanguage));
    Delay := Delay and not DelayingLanguage; // already set
  end;
{$ENDIF}
  if (Delay and DelayingLanguage) <> 0 then
    Run(TZMOpLanguage.Create(FLanguage));
  if (Delay and DelayingExtStream) <> 0 then
    Run(TZMOpSetExtStream.Create(FDelayExtStream));
  if (Delay and DelayingFileName) <> 0 then
    Run(TZMOpSetZipFileName.Create(FFileName));
  if (Errors.Code = 0) and ((Delay and DelayingComment) <> 0) then
    Run(TZMOpSetZipComment.Create(FDelayedComment));
  if (Errors.Code = 0) and ((Delay and DelayingDLL) <> 0) then
    Run(TZMOpSetDLLLoad.Create(FDLLLoad));
end;

function TZMCommand.GetActive: Boolean;
begin
  Result := (FActive <> 0);
end;

function TZMCommand.GetDLL_Load: Boolean;
begin
  if not((CsDesigning in Master.ComponentState) or
    (CsLoading in Master.ComponentState)) then
    FDLLLoad := _DLL_Loaded(Master);
  Result := FDLLLoad;
end;

function TZMCommand.GetDLL_Version1(ForceLoad: Boolean): string;
var
  Ver: Integer;
begin
  Result := '';
  if (CsDesigning in Master.ComponentState) or
    (CsLoading in Master.ComponentState) then
    Exit;
  Ver := _DLL_Build;
  if (Ver < (DELZIPVERSION * 1000)) and ForceLoad and IsActive then
  begin
    // shouldn't matter if busy or not
    _DLL_Load(Self);
    Ver := _DLL_Build;
    _DLL_Unload(Self);
  end;
  if Ver <= 0 then
    Result := '?.?.?.????'
  else
    Result := VersStr(Ver, False);
end;

function TZMCommand.IsActive: Boolean;
begin
  Result := (FActive <> 0);
  if Result and ((CsDesigning in Master.ComponentState) or
    (CsLoading in Master.ComponentState)) then
    Result := False; // never Active while loading or designing
end;

function SetVal(Someset: Pointer): Integer;
begin
  Result := PByte(Someset)^;
end;

procedure TZMCommand.BeforeDestruction;
begin
  IsDestructing := True;
  inherited;
end;

procedure TZMCommand.EndRunning;
begin
  Dec(BusyFlag);
  if BusyFlag = 0 then
  begin
    if State <> ZsReentry then
      StateChanged(ZsIdle)
    else
      StateChanged(ZsReentered);
    // Are we waiting to go inactive?
    if FActive < 0 then
    begin
      _DLL_Unload(Self);
      FActive := 0;
      StateChanged(ZsDisabled);
    end;
  end;
end;

procedure TZMCommand.ForceStrings;
var
  Missing: Boolean;
begin
  Missing := MessageStrs.Count < MAX_ID;
  if Missing then
    Missing := LoadDefStrings < 0;
  if Missing then
    raise EZipMaster.CreateMsg(Self, ZS_NoLanguage, {_LINE_}343, __UNIT__);
end;

procedure TZMCommand.LogGlobals;
var
  Msg: string;
  S: string;
  SpnOpts: TZMSpanOpts;
  SkpOpts: TZMSkipAborts;
begin
  if Master.Owner <> nil then
    S := Master.Owner.Name
  else
    S := '<none>';
  Logger.Log(ZM_Error({_LINE_}357, 0), Format('Start logging %u.%u:%s.<%p>(%s)',
    [MainThreadID, GetCurrentThreadID, S, Pointer(Master), Master.Name]));
  Msg := Application.ExeName + ' -- ' + DateTimeToStr(Now) + ' -- '#13#10;
  SpnOpts := Master.SpanOptions;
  SkpOpts := Master.NoSkipping;
  Msg := Msg + Format('Add:%x Extr:%x Skip:%x Span:%x Write:%x'#13#10,
    [SetVal(@AddOptions), SetVal(@ExtrOptions), SetVal(@SkpOpts),
    SetVal(@SpnOpts), SetVal(@WriteOptions)]);
  Msg := Msg + Format('Platform:%d, Windows:%d.%d, Delphi:%g, ZipMaster:%d',
    [Win32Platform, Win32MajorVersion, Win32MinorVersion,
{$IFDEF VERD7up}CompilerVersion{$ELSE}0.0{$ENDIF}, Master.Build]);
  Logger.Log(0, Msg);
end;

// if Permitted returns true Finish must be called
function TZMCommand.Permitted: Boolean;
begin
  Result := False;
  if IsActive then
  begin
    Inc(BusyFlag);
    if BusyFlag <> 1 then
    begin
      Dec(BusyFlag);
      ReEntered;
    end
    else
      Result := True;
  end;
  if Result then
  begin
    Opr := nil;
    StateChanged(ZsBusy);
  end;
end;

procedure TZMCommand.ReEntered;
begin
  Inc(FBlockedCnt);
  StateChanged(ZsReentry);
  Inform('Re-entry', {_LINE_}397, __UNIT__);
end;

function TZMCommand.Run(TheOperation: TZMOperationRoot): Integer;
var
  OpName: string;
  RetVal: Integer;
  S: string;
begin
  if Permitted then
  begin
    S := '';
    try
      try
        ForceStrings;
        Start(TheOperation);
        RetVal := TheOperation.Execute(Self);
        if (Errors.Code = 0) and (RetVal < 0) then
          ShowError(RetVal);
        UpdateChanged(TheOperation);
      except
        on E: Exception do
        begin
          UpdateCurrent(False);
          S := 'Exception!';
          if Progress.InBatch then
            Progress.EndBatch;
          ZipStream := nil;
          if E is EZMException then // Catch all Zip specific errors.
            ShowExceptionError(EZMException(E))
          else
            // the error ErrMessage of an unknown error is displayed ...
            ShowMessage(ZM_Error({_LINE_}429, ZE_UnknownError), E.Message);
        end;
      end;
    finally
      DLLWorking := nil;
      OpName := TheOperation.Name;
      TheOperation.Free;
      EndRunning;
      Result := Errors.Code;
      if (Result = 0) and (Errors.ErrMessage = '') then
        Errors.ErrMessage := ZipLoadStr(ZS_Success);
      if Logger <> nil then
      begin
        S := Format(' - Finished %s (%d) "%s" %s - %s',
          [OpName, Errors.Code, Errors.ErrMessage, S, DateTimeToStr(Now)]);

        Logger.Log(0, S);
        Logger.CloseLog;
      end;
    end;
  end
  else
  begin
    TheOperation.Free;
    Result := ZE_Blocked;
  end;
end;

(* SetActive
  sets the following values
 0 - not active
 1 - active
 -1 - active in design/loading state (no Active functions allowed)
*)
procedure TZMCommand.SetActive(const Value: Boolean);
var
  Was: Integer;
begin
  if (CsDesigning in Master.ComponentState) or
    (CsLoading in Master.ComponentState) then
  begin
    if Value then
      FActive := 1 // set but ignored
    else
      FActive := 0;
    Exit;
  end;
  if Value <> (FActive > 0) then
  begin
    Was := FActive;
    if Value then
    begin
      FActive := 1;
      // reject change active to inactive to active while busy
      if Was = 0 then
      begin
        // changed to 'active'
        StateChanged(ZsIdle);
        if (FDelaying <> 0) and (BusyFlag = 0) then
          DoDelays;
      end;
    end
    else
    begin
      if BusyFlag <> 0 then
        FActive := -3 // clear when 'done'
      else
      begin
        FActive := 0; // now inactive
        StateChanged(ZsDisabled);
      end;
    end;
  end;
end;

procedure TZMCommand.SetDLL_Load(const Value: Boolean);
var
  Err: Integer;
begin
  if Value <> DLL_Load then
  begin
    Err := Run(TZMOpSetDLLLoad.Create(Value));
    if (Err = ZE_Blocked) and not IsActive then
    begin
      FDLLLoad := Value;
      FDelaying := FDelaying or DelayingDLL; // delay until Active
    end
    else
      FDLLLoad := _DLL_Loaded(Master);
  end;
end;

procedure TZMCommand.SetExtStream(AStream: TStream);
begin
  if (Run(TZMOpSetExtStream.Create(AStream)) = ZE_Blocked) and (not IsActive)
    and (AStream <> ExtStream) then // not Active
  begin
    FDelayExtStream := AStream;
    FDelaying := FDelaying or DelayingExtStream;
    if AStream <> nil then
      FDelaying := FDelaying and not DelayingFileName;
  end;
end;

procedure TZMCommand.SetLanguage(const Value: string);
begin
  FLanguage := Value;
  if (Run(TZMOpLanguage.Create(Value)) = ZE_Blocked) and not IsActive then
    FDelaying := FDelaying or DelayingLanguage; // delay until Active
end;

procedure TZMCommand.SetRes(const Value: Integer);
begin
  FRes := Value;
  if (Errors.Code = 0) and (Value < 0) then
    ShowError(Value);
end;

procedure TZMCommand.SetState(const Value: TZMStates);
begin
  if (Value >= ZsDisabled) and (Value <= ZsReentry) and (Value <> FState) then
    StateChanged(Value);
end;

procedure TZMCommand.SetTheLanguage(const Lang: string);
begin
  FLanguage := Lang;
end;

{$IFNDEF UNICODE}
{called by Zipmstr}
procedure TZMCommand.SetUseUTF8(const Value: Boolean);
begin
  if (Value <> IsUtf8) and (Win32PlatForm = Ver_Platform_Win32_NT) then
  begin
    FUseUTF8 := Value;
    if (Run(TZMOpSetUseUTF8.Create(Value)) = ZE_Blocked) and not IsActive then
      FDelaying := FDelaying or DelayingUseUTF8; // delay until Active
  end;
end;
{$ENDIF}

{called by Zipmstr}
procedure TZMCommand.SetZipComment(const Value: AnsiString);
begin
  if Value <> ZipComment then
  begin
    FDelayedComment := '';
    if (Run(TZMOpSetZipComment.Create(Value)) = ZE_Blocked) and not IsActive
    then
    begin
      FDelayedComment := Value;
      FDelaying := FDelaying or DelayingComment;
    end;
  end;
end;

{called by Zipmstr}
procedure TZMCommand.SetZipFileName(const Value: string);
begin
  if (Run(TZMOpSetZipFileName.Create(Value)) = ZE_Blocked) and (not IsActive)
    and (Value <> ZipFileName) then // not Active
  begin
    FZipFileName := Value;
    FFileName := Value;
    FDelaying := FDelaying or DelayingFileName;
    if Value <> '' then
      FDelaying := FDelaying and not DelayingExtStream;
  end;
end;

procedure TZMCommand.Start(TheOperation: TZMOperationRoot);
var
  LogLevel: Integer;
begin
  FBlockedCnt := 0;
  DLLWorking := nil;
  if GetCurrentThreadID <> MainThreadID then
    NotMainThread := True;
  if ZorFSpecArgs in TheOperation.Needs then
  begin
    IncludeSpecs.Assign(Master.FSpecArgs); // lock contents
    ExcludeSpecs.Assign(Master.FSpecArgsExcl); // lock contents
  end;
  ProblemList.Clear;
  Errors.Clear;
  AnswerAll := False;
  SuccessCnt := 0;
  // 'lock' spec lists
  if Master.Trace then
    Verbosity := ZvTrace
  else
    if Master.Verbose then
      Verbosity := ZvVerbose
    else
      Verbosity := ZvOff;
  Logging := False;
  if Logger = nil then
    Logger := TZMLogger.Create(Self);
  LogLevel := Logger.StartLog;
  if LogLevel > 0 then
  begin
    Logging := True;
    if LogLevel > 3 then
      Verbosity := TZMVerbosity(LogLevel and 3); // force required level
    LogGlobals;
  end;
  if (Verbosity > ZvTrace) and not Logging then
    Verbosity := ZvTrace;
  IsTrace := Verbosity >= ZvTrace;
  IsVerbose := Verbosity >= ZvVerbose;
  Inform('- operation = ' + TheOperation.Name, {_LINE_}641, __UNIT__);
  Started;
end;

procedure TZMCommand.StartWaitCursor;
begin
  if FCurWaitCount = 0 then
  begin
    FSaveCursor := Screen.Cursor;
    Screen.Cursor := CrHourGlass;
  end;
  Inc(FCurWaitCount);
end;

procedure TZMCommand.StateChanged(NewState: TZMStates);
var
  NoCursor: Boolean;
begin
  if GetCurrentThreadID <> MainThreadID then
    NotMainThread := True;
  NoCursor := NotMainThread;
  if not NoCursor then
  begin
    if (NewState < ZsBusy) <> (State < ZsBusy) then
    begin
      // 'busy' state has changed
      if State < ZsBusy then
        StartWaitCursor // now 'busy'
      else
        StopWaitCursor; // now 'idle'
    end;
  end;
  if Assigned(Master.OnStateChange) and not IsDestructing then
    Master.OnStateChange(Master, NewState, NoCursor);
  FState := NewState;
end;

procedure TZMCommand.StopWaitCursor;
begin
  if FCurWaitCount > 0 then
  begin
    Dec(FCurWaitCount);
    if FCurWaitCount < 1 then
      Screen.Cursor := FSaveCursor;
  end;
end;

procedure TZMCommand.UpdateChanged(TheOperation: TZMOperationRoot);
begin
  if ZorFSpecArgs in TheOperation.Changes then
  begin
    // restore the 'locked' spec lists
    Master.FSpecArgs.Assign(IncludeSpecs);
    Master.FSpecArgsExcl.Assign(ProblemList);
  end;
  if ZorZip in TheOperation.Changes then
    UpdateCurrent(True);
end;

end.
