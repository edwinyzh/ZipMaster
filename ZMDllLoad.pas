unit ZMDllLoad;

// ZMDllLoad.pas - Dynamically load the DLL

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
// modified 2014-03-29

{$I   '.\ZipVers.inc'}
{$I   '.\ZMConfig192.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZipMstr, ZMDelZip, ZMBody;

procedure _DLL_Abort(Master: TCustomZipMaster; Key: Cardinal);
function _DLL_Banner: string;
function _DLL_Build: Integer;
function _DLL_Exec(Worker: TZMBody; const Rec: PDLLCommands;
  var Key: Cardinal): Integer;
function _DLL_Load(Worker: TZMBody): Integer;
function _DLL_Loaded(Master: TCustomZipMaster): Boolean;
function _DLL_Path: string;
procedure _DLL_Remove(Master: TCustomZipMaster);
procedure _DLL_Unload(Worker: TZMBody);

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMXcpt,
{$IFNDEF STATIC_LOAD_DELZIP_DLL}
  ZMEngine, {$IFNDEF VERpre6}SyncObjs, {$ENDIF}
{$ENDIF}
  ZMUtils, ZMMsg, ZMWinFuncs;

const
  __UNIT__ = 13;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

procedure CheckExec(RetVal: Integer; Worker: TZMBody);
var
  X: Integer;
begin
  if RetVal < 0 then
  begin
    X := -RetVal;
    if X > _DZ_ERR_MAX then
      raise EZipMaster.CreateMsgFmt(Worker, ZE_DLLCritical, [X], {_LINE_}102,
        __UNIT__);
    if (X = _DZ_ERR_CANCELLED) or (X = _DZ_ERR_ABORT) then
      X := ZS_Canceled
    else
      X := X + ZD_GOOD;
    raise EZipMaster.CreateMsg(Worker, -X, {_LINE_}108, __UNIT__);
  end;
end;
{$IFDEF STATIC_LOAD_DELZIP_DLL}
// 'static' loaded dll functions
function DZ_Abort(C: Cardinal): Integer; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Path: PChar; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_PrivVersion: Integer; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Exec(C: PDLLCommands): Integer; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Version: Integer; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Banner: PChar; stdcall; external DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Name(var Buf; Bufsiz: Integer; Wide: Boolean): Integer; stdcall;
  external DelZipDLL_Name {$IFDEF VERD2010up} Delayed {$ENDIF};
{$ELSE}

type
  TZMCount = record
    Master: TCustomZipMaster;
    Count: Integer;
  end;

type
  TZMDLLLoader = class
  private
    AbortFunc: TAbortOperationFunc;
    BannerFunc: TDLLBannerFunc;
    Counts: array of TZMCount;
    ExecFunc: TDLLExecFunc;
    FBanner: string;
    FHasResDLL: Integer;
    FKillTemp: Boolean;
    FLoadErr: Integer;
    FLoadingError: Integer;
    FLoadingErrMsg: string;
    FLoading: Integer;
    FLoadPath: string;
    FPath: string;
    FVer: Integer;
    // guard data for access by several threads
{$IFDEF VERpre6}
    CSection: TRTLCriticalSection;
{$ELSE}
    Guard: TCriticalSection;
{$ENDIF}
    Hndl: HWND;
    NameFunc: TDLLNameFunc;
    PathFunc: TDLLPathFunc;
    Priv: Integer;
    PrivFunc: TDLLPrivVersionFunc;
    TmpFileName: string;
    VersFunc: TDLLVersionFunc;
    function GetIsLoaded: Boolean;
    function LoadLib(Worker: TZMBody; FullPath: string;
      MustExist, Complain: Boolean): Integer;
    procedure ReleaseLib;
    procedure RemoveTempDLL;
  protected
    function Counts_Dec(Worker: TZMBody; AMaster: TCustomZipMaster): Integer;
    function Counts_Find(Master: TCustomZipMaster): Integer;
    function Counts_Inc(Worker: TZMBody): Integer;
    procedure Empty;
    function ExtractResDLL(Worker: TZMBody; OnlyVersion: Boolean): Integer;
    function LoadDLL(Worker: TZMBody): Integer;
    function UnloadDLL: Integer;
    property IsLoaded: Boolean read GetIsLoaded;
  public
    procedure Abort(Master: TCustomZipMaster; Key: Cardinal);
    procedure AfterConstruction; override;
    function Banner: string;
    procedure BeforeDestruction; override;
    function Build: Integer;
    function Exec(Worker: TZMBody; const Rec: PDLLCommands;
      var Key: Cardinal): Integer;
    function Load(Worker: TZMBody): Integer;
    function Loaded(Master: TCustomZipMaster): Boolean;
    function Path: string;
    procedure Remove(Master: TCustomZipMaster);
    procedure Unload(Worker: TZMBody);
    property Ver: Integer read FVer;
  end;

const
  RESVER_UNTRIED = -99; // have not looked for resdll yet
  RESVER_NONE = -1; // not available
  RESVER_BAD = 0; // was bad copy/version
  MIN_RESDLL_SIZE = 50000;
  MAX_RESDLL_SIZE = 600000;

var
  G_LoadedDLL: TZMDLLLoader = nil;

procedure TZMDLLLoader.Abort(Master: TCustomZipMaster; Key: Cardinal);
begin
  if Loaded(Master) and (Hndl <> 0) then
    AbortFunc(Key);
end;

procedure TZMDLLLoader.AfterConstruction;
begin
  inherited;
{$IFDEF VERpre6}
  InitializeCriticalSection(CSection);
{$ELSE}
  Guard := TCriticalSection.Create;
{$ENDIF}
  FKillTemp := False;
  Empty;
  FPath := DelZipDLL_Name;
  TmpFileName := '';
  FLoading := 0;
  FBanner := '';
  FHasResDLL := RESVER_UNTRIED; // unknown
end;

function TZMDLLLoader.Banner: string;
var
  Tmp: AnsiString;
begin
  Result := '';
  if IsLoaded then
  begin
    Tmp := BannerFunc;
    Result := string(Tmp);
  end;
end;

procedure TZMDLLLoader.BeforeDestruction;
begin
  if Hndl <> 0 then
    FreeLibrary(Hndl);
  Counts := nil;
{$IFDEF VERpre6}
  DeleteCriticalSection(CSection);
{$ELSE}
  FreeAndNil(Guard);
{$ENDIF}
  Hndl := 0;
  RemoveTempDLL;
  inherited;
end;

function TZMDLLLoader.Build: Integer;
begin
  Result := 0;
  if IsLoaded then
    Result := Priv;
end;

{ TZMDLLLoader }

function TZMDLLLoader.Counts_Dec(Worker: TZMBody;
  AMaster: TCustomZipMaster): Integer;
var
  I: Integer;
  KeepLoaded: Boolean;
  P: string;
  Zm: TCustomZipMaster;
begin
  Result := -1;
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    if Worker <> nil then
      Zm := Worker.Master
    else
      Zm := AMaster;
    // find master
    I := Counts_Find(Zm);
    if I >= 0 then
    begin
      // found
      Dec(Counts[I].Count);
      Result := Counts[I].Count;
      if Result < 1 then
      begin
        // not wanted - remove from list
        Counts[I].Master := nil;
        Counts[I].Count := 0;
      end;
    end;
    // ignore unload if loading
    if FLoading = 0 then
    begin
      KeepLoaded := False;
      for I := 0 to high(Counts) do
        if (Counts[I].Master <> nil) and (Counts[I].Count > 0) then
        begin
          KeepLoaded := True;
          Break;
        end;

      if not KeepLoaded then
      begin
        P := FPath;
        UnloadDLL;
        if Worker <> nil then
          Worker.ReportMsg(ZM_Error({_LINE_}313, ZS_DllUnloaded), [P]);
      end;
    end;
  finally
{$IFDEF VERpre6}
    LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

function TZMDLLLoader.Counts_Find(Master: TCustomZipMaster): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Counts <> nil then
  begin
    for I := 0 to high(Counts) do
    begin
      if Counts[I].Master = Master then
      begin
        // found
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TZMDLLLoader.Counts_Inc(Worker: TZMBody): Integer;
var
  I: Integer;
  Len: Integer;
  LoadVer: Integer;
  Zm: TCustomZipMaster;
begin
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    Zm := Worker.Master;
    // find master
    I := Counts_Find(Zm);
    if I >= 0 then
    begin
      // found
      Inc(Counts[I].Count);
      Result := Counts[I].Count;
    end
    else
    begin
      // need new one - any empty
      I := Counts_Find(nil);
      if I >= 0 then
      begin
        // have empty position - use it
        Counts[I].Master := Zm;
        Counts[I].Count := 1;
        Result := 1;
      end
      else
      begin
        // need to extend
        Len := high(Counts);
        if Len > 0 then
          Inc(Len)
        else
          Len := 0;
        SetLength(Counts, Len + 4);
        // clear the rest
        for I := Len + 3 downto Len + 1 do
        begin
          Counts[I].Master := nil;
          Counts[I].Count := 0;
        end;
        I := Len;
        Counts[I].Master := Zm;
        Counts[I].Count := 1;
        Result := 1;
      end;
    end;
    if not IsLoaded then
    begin
      // avoid re-entry
      Inc(FLoading);
      try
        if FLoading = 1 then
        begin
          try
            LoadVer := LoadDLL(Worker);
          except
            on Ers: EZipMaster do
            begin
              LoadVer := -1;
              Worker.ShowExceptionError(Ers);
            end;
            on E: Exception do
            begin
              LoadVer := -1;
              Worker.ShowExceptionError(E);
            end;
          end;
          if LoadVer < DELZIPVERSION then
          begin // could not load it - empty it (i is Index for this worker)
            Counts[I].Master := nil;
            Counts[I].Count := 0;
            Result := -1;
          end
          else
            Worker.ReportMsg(ZM_Error({_LINE_}426, ZS_DllLoaded), [FPath]);
        end;
      finally
        Dec(FLoading);
      end;
    end;
  finally
{$IFDEF VERpre6}
    LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

procedure TZMDLLLoader.Empty;
begin
  Hndl := 0;
  ExecFunc := nil;
  VersFunc := nil;
  PrivFunc := nil;
  AbortFunc := nil;
  NameFunc := nil;
  PathFunc := nil;
  BannerFunc := nil;
  FVer := 0;
  Priv := 0;
  FBanner := '';
end;

function TZMDLLLoader.Exec(Worker: TZMBody; const Rec: PDLLCommands;
  var Key: Cardinal): Integer;
begin
  Result := -1; // what error
  if Counts_Inc(Worker) > 0 then
  begin
    try
      Result := ExecFunc(Rec);
    finally
      Counts_Dec(Worker, nil);
      Key := 0;
    end;
  end;
end;

function TZMDLLLoader.ExtractResDLL(Worker: TZMBody;
  OnlyVersion: Boolean): Integer;
var
  Done: Boolean;
  Fs: TFileStream;
  Len: Integer;
  Rs: TResourceStream;
  Temppath: string;
  W: Word;
begin
  Done := False;
  Result := -1;
  Fs := nil;
  Rs := nil;
  try
    // only check if unknown or know exists
    if (FHasResDLL = RESVER_UNTRIED) or (FHasResDLL >= MIN_DLL_BUILD) then
      Rs := OpenResStream(DZRES_Dll, RT_RCDATA);
    if FHasResDLL = RESVER_UNTRIED then
      FHasResDLL := RESVER_NONE; // in case of exception
    // read the dll version if it exists
    if (Rs <> nil) and (Rs.Size >= MIN_RESDLL_SIZE) and
      (Rs.Size < MAX_RESDLL_SIZE) then
    begin
      Rs.Position := 0;
      Rs.ReadBuffer(Result, Sizeof(Integer));
      FHasResDLL := Result; // the dll version
      if (Result >= MIN_DLL_BUILD) and not OnlyVersion then
      begin
        Rs.ReadBuffer(W, Sizeof(Word));
        Rs.Position := Sizeof(Integer);
        Temppath := Worker.TempDir;
        if Length(Temppath) = 0 then // Get the system temp dir
        begin
          SetLength(Temppath, MAX_PATH + 2);
          Len := GetTempPath(MAX_PATH, PChar(Temppath));
          if (Len > MAX_PATH) or (Len = 0) then
            raise EZipMaster.CreateMsg(Worker, ZE_NoTempFile, {_LINE_}508,
              __UNIT__);
          Temppath := PChar(Temppath);
        end
        else // Use Temp dir provided by ZipMaster
          Temppath := DelimitPath(Worker.TempDir, True);
        TmpFileName := FormTempName(Temppath + '*.dll');
        if TmpFileName = '' then
          raise EZipMaster.CreateMsg(Worker, ZE_NoTempFile, {_LINE_}516,
            __UNIT__);
        Fs := TFileStream.Create(TmpFileName, FmCreate);
        Done := ExtractZStream(Fs, Rs, Rs.Size - Sizeof(Integer)) = 0;

        if not Done then
          FHasResDLL := RESVER_BAD; // could not extract
      end;
    end;
  finally
    FreeAndNil(Fs);
    FreeAndNil(Rs);
    if not OnlyVersion then
    begin
      if not Done then
        File_Delete(TmpFileName);
      if not _Z_FileExists(TmpFileName) then
        TmpFileName := '';
    end;
  end;
end;

function TZMDLLLoader.GetIsLoaded: Boolean;
begin
  Result := Hndl <> 0;
end;

function TZMDLLLoader.Load(Worker: TZMBody): Integer;
begin
  Result := 0;
  if Counts_Inc(Worker) > 0 then
    Result := G_LoadedDLL.Ver;
end;

function TZMDLLLoader.LoadDLL(Worker: TZMBody): Integer;
var
  AllowResDLL: Boolean;
  DBuild: Integer;
  DLLDirectory: string;
  Dpth: string;
  FullPath: string;
begin
  if Hndl = 0 then
  begin
    FVer := 0;
    FullPath := '';
    DLLDirectory := DelimitPath(Worker.Master.DLLDirectory, False);
    if DLLDirectory = '><' then
    begin
      // use res dll (or else)
      if (TmpFileName = '') and (ExtractResDLL(Worker, False) < MIN_DLL_BUILD)
      then
        raise EZipMaster.CreateMsgFmt(Worker, ZE_NoDll, [DelZipDLL_Name, ''],
          {_LINE_}569, __UNIT__);
      LoadLib(Worker, TmpFileName, True, True);
      Result := FVer;
      Exit;
    end;
    if DLLDirectory <> '' then
    begin
      // check relative?
      if DLLDirectory[1] = '.' then
        FullPath := PathConcat(ExtractFilePath(ParamStr(0)), DLLDirectory)
      else
        FullPath := DLLDirectory;
      if (ExtractNameOfFile(DLLDirectory) <> '') and
        (CompareText(ExtractFileExt(DLLDirectory), '.DLL') = 0) then
      begin
        // must load the named dll
        LoadLib(Worker, FullPath, True, True);
        Result := FVer;
        Exit;
      end;
      Dpth := ExtractFilePath(FullPath);
      if (Dpth <> '') and not _Z_DirExists(Dpth) then
        FullPath := '';
    end;
    AllowResDLL := DLLDirectory = ''; // only if no path specified
    if AllowResDLL then
    begin
      // check for res dll once only
      if FHasResDLL = RESVER_UNTRIED then
        ExtractResDLL(Worker, True); // read the res dll version if it exists
      if FHasResDLL < MIN_DLL_BUILD then
        AllowResDLL := False; // none or bad version
    end;
    DBuild := LoadLib(Worker, PathConcat(FullPath, DelZipDLL_Name),
      not AllowResDLL, False);
    // if not loaded we only get here if allowResDLL is true;
    if DBuild < MIN_DLL_BUILD then
    begin
      // use resdll if no other available
      if (TmpFileName <> '') or (ExtractResDLL(Worker, False) > 0) then
      begin
        if LoadLib(Worker, TmpFileName, False, False) < MIN_DLL_BUILD then
        begin
          // could not load the res dll
          FHasResDLL := RESVER_BAD; // is bad version
        end;
      end;
    end;
    if (FVer < MIN_DLL_BUILD) and (FLoadingError <> 0) then
      Worker.ReportMessage(FLoadingError, FLoadingErrMsg);
  end;
  Result := FVer;
end;

function TZMDLLLoader.Loaded(Master: TCustomZipMaster): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := Counts_Find(Master);
  if (I >= 0) and (Counts[I].Count > 0) then
    Result := True;
end;

// returns build
function TZMDLLLoader.LoadLib(Worker: TZMBody; FullPath: string;
  MustExist, Complain: Boolean): Integer;
var
  OldMode: Cardinal;
  S: string;
  Tmp: AnsiString;
begin
  if Hndl > 0 then
    FreeLibrary(Hndl);
  Empty;
  FLoadingError := 0;
  FLoadingErrMsg := '';
  FLoadErr := 0;
  FLoadPath := FullPath;
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    Hndl := LoadLibrary(PChar(FullPath));
    if Hndl > HInstance_Error then
    begin
      @ExecFunc := GetProcAddress(Hndl, DelZipDLL_Execfunc);
      if (@ExecFunc <> nil) then
        @VersFunc := GetProcAddress(Hndl, DelZipDLL_Versfunc);
      if (@VersFunc <> nil) then
        @PrivFunc := GetProcAddress(Hndl, DelZipDLL_Privfunc);
      if (@PrivFunc <> nil) then
        @AbortFunc := GetProcAddress(Hndl, DelZipDLL_Abortfunc);
      if (@AbortFunc <> nil) then
        @NameFunc := GetProcAddress(Hndl, DelZipDLL_Namefunc);
      if (@NameFunc <> nil) then
        @BannerFunc := GetProcAddress(Hndl, DelZipDLL_Bannerfunc);
      if (@BannerFunc <> nil) then
        @PathFunc := GetProcAddress(Hndl, DelZipDLL_Pathfunc);
    end
    else
      FLoadErr := GetLastError;
  finally
    SetErrorMode(OldMode);
  end;
  if Hndl <= HInstance_Error then
  begin
    Empty;
    FLoadingError := ZM_Error({_LINE_}675, ZE_LoadErr);
    FLoadingErrMsg := Worker.ZipFmtLoadStr(ZE_LoadErr,
      [FLoadErr, SysErrorMessage(FLoadErr), FLoadPath]);
    if Complain or MustExist then
      Worker.ReportMessage(FLoadingError, FLoadingErrMsg)
    else
      Worker.Trace(FLoadingErrMsg, {_LINE_}681, __UNIT__);
    if MustExist then
    begin
      raise EZipMaster.CreateMsgFmt(Worker, ZE_NoDll,
        [FullPath, #13#10 + SysErrorMessage(FLoadErr)], {_LINE_}685, __UNIT__);
    end;
    Result := 0;
    Exit;
  end;
  if (@BannerFunc <> nil) then
  begin
    Priv := PrivFunc;
    FVer := VersFunc;
    SetLength(FPath, MAX_PATH + 1);
    NameFunc(FPath[1], MAX_PATH, {$IFDEF UNICODE}True{$ELSE}False{$ENDIF});
    FPath := string(PChar(FPath));
    Tmp := BannerFunc;
    FBanner := string(Tmp);
  end;
  if (FVer <> DELZIPVERSION) or (Priv < MIN_DLL_BUILD) then
  begin
    FLoadingError := ZM_Error({_LINE_}702, ZE_BadDll);
    S := FLoadPath + ' (' + VersStr(Priv, False) + ')';
    FLoadingErrMsg := Worker.ZipFmtLoadStr(ZE_BadDll, [S]);
    if Complain or MustExist then
      Worker.ReportMessage(FLoadingError, FLoadingErrMsg)
    else
      Worker.Trace(FLoadingErrMsg, {_LINE_}708, __UNIT__);
    FullPath := FPath;
    FreeLibrary(Hndl);
    Empty;
    if MustExist then
    begin
      raise EZipMaster.CreateMsgFmt(Worker, ZE_BadDll, [S], {_LINE_}714,
        __UNIT__);
    end;
  end;
  Result := Priv;
end;

function TZMDLLLoader.Path: string;
begin
  Result := '';
  if IsLoaded then
    Result := FPath;
end;

procedure TZMDLLLoader.ReleaseLib;
begin
  if Hndl <> 0 then
  begin
    FreeLibrary(Hndl);
    Hndl := 0;
  end;
  if Hndl = 0 then
  begin
    Empty;
    FPath := '';
    if FKillTemp then
      RemoveTempDLL;
  end;
end;

procedure TZMDLLLoader.Remove(Master: TCustomZipMaster);
var
  I: Integer;
begin
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    I := Counts_Find(Master);
    if I >= 0 then
    begin
      // found - remove it
      Counts[I].Master := nil;
      Counts[I].Count := 0;
    end;
  finally
{$IFDEF VERpre6}
    LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

procedure TZMDLLLoader.RemoveTempDLL;
var
  T: string;
begin
  T := TmpFileName;
  TmpFileName := '';
  FKillTemp := False;
  if T <> '' then
    File_Delete(T);
end;

procedure TZMDLLLoader.Unload(Worker: TZMBody);
begin
  Counts_Dec(Worker, nil);
end;

function TZMDLLLoader.UnloadDLL: Integer;
begin
  ReleaseLib;
  Result := FVer;
end;
{$ENDIF}
{ public functions }

procedure _DLL_Abort(Master: TCustomZipMaster; Key: Cardinal);
begin
  if Key <> 0 then
{$IFDEF STATIC_LOAD_DELZIP_DLL}
    DZ_Abort(Key);
{$ELSE}
    G_LoadedDLL.Abort(Master, Key);
{$ENDIF}
end;

function _DLL_Banner: string;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Banner;
{$ELSE}
  Result := G_LoadedDLL.Banner;
{$ENDIF}
end;

function _DLL_Build: Integer;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_PrivVersion;
{$ELSE}
  Result := G_LoadedDLL.Build;
{$ENDIF}
end;

function _DLL_Exec(Worker: TZMBody; const Rec: PDLLCommands;
  var Key: Cardinal): Integer;
begin
  try
{$IFDEF STATIC_LOAD_DELZIP_DLL}
    Result := DZ_Exec(Rec);
{$ELSE}
    Result := G_LoadedDLL.Exec(Worker, Rec, Key);
{$ENDIF}
    Key := 0;
  except
    Result := -6;
    Key := 0;
  end;
  CheckExec(Result, Worker);
end;

function _DLL_Load(Worker: TZMBody): Integer;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Version;
{$ELSE}
  Result := G_LoadedDLL.Load(Worker);
{$ENDIF}
end;

function _DLL_Loaded(Master: TCustomZipMaster): Boolean;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := True;
{$ELSE}
  Result := G_LoadedDLL.Loaded(Master);
{$ENDIF}
end;

function _DLL_Path: string;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Path;
{$ELSE}
  Result := G_LoadedDLL.Path;
{$ENDIF}
end;

// remove from list
procedure _DLL_Remove(Master: TCustomZipMaster);
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  // nothing to do
{$ELSE}
  if G_LoadedDLL <> nil then
  begin
    G_LoadedDLL.Counts_Dec(nil, Master); // unload if only loaded by Master
    G_LoadedDLL.Remove(Master); // remove from list
  end;
{$ENDIF}
end;

procedure _DLL_Unload(Worker: TZMBody);
begin
{$IFNDEF STATIC_LOAD_DELZIP_DLL}
  G_LoadedDLL.Unload(Worker);
{$ENDIF}
end;

{$IFNDEF STATIC_LOAD_DELZIP_DLL}
initialization

G_LoadedDLL := TZMDLLLoader.Create;

finalization

FreeAndNil(G_LoadedDLL);
{$ENDIF}

end.
