unit ZMMFStream;


// ZMMFStream.pas - basic in/out for multi-part zip files

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
// modified 2014-03-28

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils,
{$ENDIF}
  ZipMstr, ZMBody, ZMFStream, ZMDrv, ZMZipBase;

type
  TZMMultiFileStream = class(TZMFileStream)
  private
    FAllowedSize: Int64;
    FCurrentName: string;
    FCurrentPart: TZMSingleFileStream;
    FDiskWritten: Cardinal;
    FIsTemp: Boolean;
    FMaster: TCustomZipMaster;
    FNewDisk: Boolean;
    FOpenMode: Integer;
    FSig: TZipFileSigs;
    FSpan: TZMSpanParameters;
    FTheZip: TZMZipBase;
    FWorkDrive: TZMWorkDrive;
    FZipDiskAction: TZMDiskAction;
    FZipDiskStatus: TZMZipDiskStatus;
    function AskOverwriteSegment(const DiskFile: string;
      DiskSeq: Integer): Integer;
    function ChangeNumberedName(const FName: string; NewNbr: Cardinal;
      Remove: Boolean): string;
    procedure ClearFloppy(const Dir: string);
    function GetArchiveName: string;
    function GetDiskNr: Integer;
    function GetNumbering: TZipNumberScheme;
    function GetReqFileName: string;
    function GetTotalDisks: Integer;
    function IsRightDisk: Boolean;
    function NewSegment: Boolean;
    function OldVolName(Part: Integer): string;
    procedure SetArchiveName(const Value: string);
    procedure SetCurrentPart(const Value: TZMSingleFileStream);
    procedure SetDiskNr(const Value: Integer);
    procedure SetNumbering(const Value: TZipNumberScheme);
    procedure SetTotalDisks(const Value: Integer);
    function VolName(Part: Integer): string;
    function ZipFormat(const NewName: string): Integer;
  protected
    function GetIsOpen: Boolean; override;
    function GetIsTemp: Boolean; override;
    function GetLastWritten: Cardinal; override;
    function GetOpenMode: Integer; override;
    function GetPosition: Int64; override;
    function GetRealFileName: string; override;
    procedure SetIsTemp(const Value: Boolean); override;
    procedure SetOpenMode(const Value: Integer); override;
    procedure SetPosition(const Value: Int64); override;
    property CurrentPart: TZMSingleFileStream read FCurrentPart
      write SetCurrentPart;
    property Master: TCustomZipMaster read FMaster;
    property ReqFileName: string read GetReqFileName;
    property Span: TZMSpanParameters read FSpan;
    property TheZip: TZMZipBase read FTheZip;
    property WorkDrive: TZMWorkDrive read FWorkDrive;
  public
    constructor Create(MyZip: TZMZipBase; const FileName: string;
      Mode: Word); overload;
    constructor Create(MyZip: TZMZipBase; Part: TZMSingleFileStream); overload;
    constructor Create(MyZip: TZMZipBase; const FileName: string); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CheckForDisk(Writing, UnformOk: Boolean);
    function CreateMVFileNameEx(const FileName: string;
      StripPartNbr, Compat: Boolean): string;
    function FileDate: Cardinal; override;
    procedure File_Close; override;
    procedure File_Create(const TheName: string); override;
    function File_GetDate: Cardinal; override;
    procedure File_Open(const FileName: string; Mode: Word); override;
    function GetFileInformation(var FileInfo: _BY_HANDLE_FILE_INFORMATION)
      : Boolean; override;
    function IsEndOfFile: Boolean; override;
    function LastWriteTime(var Last_write: TFileTime): Boolean; override;
    function Read(var Buffer; Len: Integer): Integer; override;
    function ReadSplit(var Buffer; Len: Integer): Integer;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      overload; override;
    function SeekDisk(DiskSeq: Integer; AllowEmpty: Boolean): Integer;
    function SetEndOfFile: Boolean; override;
    function Write(const Buffer; Len: Integer): Integer; override;
    function WriteSplit(const Buffer; ToWrite, Contiguous: Integer): Integer;
    property ArchiveName: string read GetArchiveName write SetArchiveName;
    property DiskNr: Integer read GetDiskNr write SetDiskNr;
    property NewDisk: Boolean read FNewDisk write FNewDisk;
    property Numbering: TZipNumberScheme read GetNumbering write SetNumbering;
    property Sig: TZipFileSigs read FSig write FSig;
    property TotalDisks: Integer read GetTotalDisks write SetTotalDisks;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  VCL.Forms, VCL.Controls, VCL.Dialogs, WinApi.ShlObj,
{$ELSE}
  Forms, Controls, Dialogs, ShlObj, {$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMXcpt, ZMMsg, ZMWinFuncs, ZMUtils, ZMCtx, ZMDlg, ZMCore, ZMZipMulti;

const
  __UNIT__ = 22;

const
  MAX_PARTS = 999;
  MaxDiskBufferSize = (4 * 1024 * 1024); // floppies only

const
  SZipSet = 'ZipSet_';
  SPKBACK = 'PKBACK#';

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{$IFNDEF VERDXEup}
{
  The SHFormatDrive in Shell32.dll
}

const
  SHFMT_DRV_A = 0;
  SHFMT_DRV_B = 1;
  SHFMT_ID_DEFAULT = $FFFF;
  SHFMT_OPT_QUICKFORMAT = 0;
  SHFMT_OPT_FULL = 1;
  SHFMT_OPT_SYSONLY = 2;
  SHFMT_ERROR = -1;
  SHFMT_CANCEL = -2;
  SHFMT_NOFORMAT = -3;

function SHFormatDrive(hWnd: HWND;
  Drive: Word;
  fmtID: Word;
  Options: Word): Longint
  stdcall; external 'Shell32.dll' Name 'SHFormatDrive';
{$ENDIF}


function FormatFloppy(WND: HWND; const Drive: string): Integer;
//const
//  SHFMT_ID_DEFAULT = $FFFF;
//  { options }
//  SHFMT_OPT_FULL = $0001;
//  // SHFMT_OPT_SYSONLY = $0002;
//  { return values }
//  // SHFMT_ERROR = $FFFFFFFF;
//  // -1 Error on last format, drive may be formatable
//  // SHFMT_CANCEL = $FFFFFFFE;    // -2 last format cancelled
//  // SHFMT_NOFORMAT = $FFFFFFFD;    // -3 drive is not formatable
//type
//  TSHFormatDrive = function(WND: HWND; Drive, FmtID, Options: DWORD)
//    : DWORD; stdcall;
//{$IFNDEF VERDXE2up}
//var
//  HLib: THandle;
//  SHFormatDrive: TSHFormatDrive;
//{$ENDIF}
var
  Drv: Integer;
  OldErrMode: Integer;
begin
  Result := -3; // error
  if not((Length(Drive) > 1) and (Drive[2] = ':') and CharInSet(Drive[1],
    ['A' .. 'Z', 'a' .. 'z'])) then
    Exit;
  if GetDriveType(PChar(Drive)) <> DRIVE_REMOVABLE then
    Exit;
  Drv := Ord(Upcase(Drive[1])) - Ord('A');
  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
//{$IFDEF VERDXE2up}
    Result := SHFormatDrive(WND, Drv, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
//{$ELSE}
//    HLib := LoadLibrary('Shell32');
//    if HLib <> 0 then
//    begin
//      @SHFormatDrive := GetProcAddress(HLib, 'SHFormatDrive');
//      if @SHFormatDrive <> nil then
//        try
//          Result := SHFormatDrive(WND, Drv, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
//        finally
//          FreeLibrary(HLib);
//        end;
//    end;
//{$ENDIF}
  finally
    SetErrorMode(OldErrMode);
  end;
end;

{ TZMMultiFileStream }

constructor TZMMultiFileStream.Create(MyZip: TZMZipBase; const FileName: string;
  Mode: Word);
begin
  inherited Create(MyZip.Body);
  FTheZip := MyZip;
end;

// open EOC opens last part, if multi-part must convert to MultiFileStream
constructor TZMMultiFileStream.Create(MyZip: TZMZipBase; Part:
    TZMSingleFileStream);
begin
  inherited Create(MyZip.Body);
  FTheZip := MyZip;
  FCurrentPart := Part;
  FCurrentName := Part.RealFileName;
end;

constructor TZMMultiFileStream.Create(MyZip: TZMZipBase;
  const FileName: string);
begin
  inherited Create(MyZip.Body);
  FTheZip := MyZip;
  FCurrentPart := nil;
end;

procedure TZMMultiFileStream.AfterConstruction;
begin
  inherited;
  FMaster := Body.Master;
  FSpan := Body.Span;
  FWorkDrive := TheZip.WorkDrive;
end;

function TZMMultiFileStream.AskOverwriteSegment(const DiskFile: string;
  DiskSeq: Integer): Integer;
var
  MsgQ: string;
  TmpStatusDisk: TZMStatusDiskEvent;
begin
  // Do we want to overwrite an existing file?
  if FileExists(DiskFile) then
    if (File_Age(DiskFile) = TheZip.StampDate) and (Pred(DiskSeq) < DiskNr) then
    begin
      MsgQ := Body.ZipFmtLoadStr(ZS_AskPrevFile, [DiskSeq]);
      FZipDiskStatus := FZipDiskStatus + [ZdsPreviousDisk];
    end
    else
    begin
      MsgQ := Body.ZipFmtLoadStr(ZS_AskDeleteFile, [DiskFile]);
      FZipDiskStatus := FZipDiskStatus + [ZdsSameFileName];
    end
  else
    if not WorkDrive.DriveIsFixed then
      if (WorkDrive.VolumeSize <> WorkDrive.VolumeSpace) then
        FZipDiskStatus := FZipDiskStatus + [ZdsHasFiles]
        // But not the same name
      else
        FZipDiskStatus := FZipDiskStatus + [ZdsEmpty];
  TmpStatusDisk := Master.OnStatusDisk;
  if Assigned(TmpStatusDisk) and not Body.AnswerAll then
  begin
    FZipDiskAction := ZdaOk; // The default action
    TmpStatusDisk(Master, DiskSeq, DiskFile, FZipDiskStatus, FZipDiskAction);
    case FZipDiskAction of
      ZdaCancel:
        Result := IdCancel;
      ZdaReject:
        Result := IdNo;
      ZdaErase:
        Result := IdOk;
      ZdaYesToAll:
        begin
          Result := IdOk;
          Body.AnswerAll := True;
        end;
      ZdaOk:
        Result := IdOk;
    else
      Result := IdOk;
    end;
  end
  else
    if ((FZipDiskStatus * [ZdsPreviousDisk, ZdsSameFileName]) <> []) and
      not(Body.AnswerAll or Body.Unattended) then
    begin
      Result := Body.ZipMessageDlgEx(Body.ZipLoadStr(ZS_Confirm), MsgQ,
        ZmtWarning + DHC_SpanOvr, [MbYes, MbNo, MbCancel, MbYesToAll]);
      if Result = MrYesToAll then
      begin
        Body.AnswerAll := True;
        Result := IdOk;
      end;
    end
    else
      Result := IdOk;
end;

procedure TZMMultiFileStream.BeforeDestruction;
begin
  FCurrentPart.Free;
  inherited;
end;

// uses 'real' number
function TZMMultiFileStream.ChangeNumberedName(const FName: string;
  NewNbr: Cardinal; Remove: Boolean): string;
var
  Ext: string;
  StripLen: Integer;
begin
  if DiskNr > 999 then
    raise EZipMaster.CreateMsg(Body, ZE_TooManyParts, {_LINE_}359, __UNIT__);
  Ext := ExtractFileExt(FName);
  StripLen := 0;
  if Remove then
    StripLen := 3;
  Result := Copy(FName, 1, Length(FName) - Length(Ext) - StripLen) +
    Copy(IntToStr(1000 + NewNbr), 2, 3) + Ext;
end;

procedure TZMMultiFileStream.CheckForDisk(Writing, UnformOk: Boolean);
var
  AbortAction: Boolean;
  MsgFlag: Integer;
  MsgStr: string;
  OnGetNextDisktmp: TZMGetNextDiskEvent;
  Res: Integer;
  SizeOfDisk: Int64;
  TotDisks: Integer;
begin
  if WorkDrive.DriveIsFixed then
  begin
    // If it is a fixed disk we don't want a new one.
    NewDisk := False;
    Body.CheckCancel;
    Exit;
  end;
  Body.KeepAlive; // just ProcessMessages
  // First check if we want a new one or if there is a disk (still) present.
  while (NewDisk or (not WorkDrive.HasMedia(UnformOk))) do
  begin
    if Body.Unattended then
      raise EZipMaster.CreateMsg(Body, ZE_NoUnattSpan, {_LINE_}392, __UNIT__);

    MsgFlag := ZmtWarning + DHC_SpanNxtW; // or error?
    if DiskNr < 0 then // want last disk
    begin
      MsgStr := Body.ZipLoadStr(ZS_InsertDisk);
      MsgFlag := ZmtError + DHC_SpanNxtR;
    end
    else
      if Writing then
      begin
        // This is an estimate, we can't know if every future disk has the same space available and
        // if there is no disk present we can't determine the size unless it's set by MaxVolumeSize.
        SizeOfDisk := WorkDrive.VolumeSize - Span.KeepFreeOnAllDisks;
        if (Span.MaxVolumeSize <> 0) and
          (Span.MaxVolumeSize < WorkDrive.VolumeSize) then
          SizeOfDisk := Span.MaxVolumeSize;

        TotalDisks := DiskNr + 1;
        if TotalDisks > MAX_PARTS then
          raise EZipMaster.CreateMsg(Body, ZE_TooManyParts, {_LINE_}412,
            __UNIT__);
        if SizeOfDisk > 0 then
        begin
          TotDisks := Trunc((TheZip.File_Size + 4 + Span.KeepFreeOnDisk1) /
            SizeOfDisk);
          if TotalDisks < TotDisks then
            TotalDisks := TotDisks;
          MsgStr := Body.ZipFmtLoadStr(ZS_InsertVolume,
            [DiskNr + 1, TotalDisks]);
        end
        else
          MsgStr := Body.ZipFmtLoadStr(ZS_InsertAVolume, [DiskNr + 1]);
      end
      else
      begin // reading - want specific disk
        if TotalDisks = 0 then
          MsgStr := Body.ZipFmtLoadStr(ZS_InsertAVolume, [DiskNr + 1])
        else
          MsgStr := Body.ZipFmtLoadStr(ZS_InsertVolume,
            [DiskNr + 1, TotalDisks]);
      end;

    MsgStr := MsgStr + Body.ZipFmtLoadStr(ZS_InDrive, [WorkDrive.DriveStr]);
    OnGetNextDisktmp := Master.OnGetNextDisk;
    if Assigned(OnGetNextDisktmp) then
    begin
      AbortAction := False;
      OnGetNextDisktmp(Master, DiskNr + 1, TotalDisks,
        Copy(WorkDrive.DriveStr, 1, 1), AbortAction);
      if AbortAction then
        Res := IdAbort
      else
        Res := IdOk;
    end
    else
      Res := Body.ZipMessageDlgEx('', MsgStr, MsgFlag, MbOkCancel);

    if Res <> IdOk then
    begin
      Body.Cancel := ZM_Error({_LINE_}452, ZS_Abort);
      TheZip.Info := TheZip.Info or Zfi_Cancelled;
      raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}454, __UNIT__);
    end;
    NewDisk := False;
    Body.KeepAlive;
  end;
end;

procedure TZMMultiFileStream.ClearFloppy(const Dir: string);
var
  FName: string;
  SRec: _Z_TSearchRec;
begin
  if _Z_FindFirst(Dir + '*.*'{WILD_ALL}, FaAnyFile, SRec) = 0 then
    repeat
      FName := Dir + SRec.Name;
      if ((SRec.Attr and FaDirectory) <> 0) and (SRec.Name <> '.'{DIR_THIS}) and
        (SRec.Name <> '..'{DIR_PARENT}) then
      begin
        FName := FName + PathDelim;
        ClearFloppy(FName);
        Body.ReportMsg(ZS_Deleting, [FName]);
        // allow time for OS to delete last file
        _Z_RemoveDir(FName);
      end
      else
      begin
        Body.ReportMsg(ZS_Deleting, [FName]);
        File_Delete(FName);
      end;
    until _Z_FindNext(SRec) <> 0;
  _Z_FindClose(SRec);
end;

function TZMMultiFileStream.CreateMVFileNameEx(const FileName: string;
  StripPartNbr, Compat: Boolean): string;
var
  Ext: string;
begin // changes FileName into multi volume FileName
  if Compat then
  begin
    if DiskNr <> (TotalDisks - 1) then
    begin
      if DiskNr < 9 then
        Ext := '.z0'
      else
        Ext := '.z';
      Ext := Ext + IntToStr(Succ(DiskNr));
    end
    else
      Ext := EXT_ZIP;
    Result := ChangeFileExt(FileName, Ext);
  end
  else
    Result := ChangeNumberedName(FileName, DiskNr + 1, StripPartNbr);
end;

function TZMMultiFileStream.FileDate: Cardinal;
begin
  Result := CurrentPart.FileDate;
end;

procedure TZMMultiFileStream.File_Close;
begin
  CurrentPart.File_Close;
end;

procedure TZMMultiFileStream.File_Create(const TheName: string);
begin
  if CurrentPart = nil then
    CurrentPart := TZMSingleFileStream.Create(Body, TheName, FmCreate)
  else
    CurrentPart.File_Create(TheName);
end;

function TZMMultiFileStream.File_GetDate: Cardinal;
begin
  if CurrentPart <> nil then
    Result := CurrentPart.File_GetDate
  else
    Result := 0;
end;

procedure TZMMultiFileStream.File_Open(const FileName: string; Mode: Word);
begin
  if CurrentPart = nil then
    CurrentPart := TZMSingleFileStream.Create(Body, FileName, Mode)
  else
    CurrentPart.File_Open(FileName, Mode);
end;

function TZMMultiFileStream.GetArchiveName: string;
begin
  Result := TheZip.ArchiveName;
end;

function TZMMultiFileStream.GetDiskNr: Integer;
begin
  Result := TheZip.DiskNr;
end;

function TZMMultiFileStream.GetFileInformation(var FileInfo
  : _BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  if CurrentPart <> nil then
    Result := CurrentPart.GetFileInformation(FileInfo)
  else
    Result := False;
end;

function TZMMultiFileStream.GetIsOpen: Boolean;
begin
  Result := (CurrentPart <> nil) and CurrentPart.IsOpen;
end;

function TZMMultiFileStream.GetIsTemp: Boolean;
begin
  Result := CurrentPart.IsTemp;
end;

function TZMMultiFileStream.GetLastWritten: Cardinal;
begin
  if CurrentPart <> nil then
    Result := CurrentPart.LastWritten
  else
    Result := 0;
end;

function TZMMultiFileStream.GetNumbering: TZipNumberScheme;
begin
  Result := TheZip.Numbering;
end;

function TZMMultiFileStream.GetOpenMode: Integer;
begin
  Result := FOpenMode;
end;

function TZMMultiFileStream.GetPosition: Int64;
begin
  Result := CurrentPart.Position;
end;

function TZMMultiFileStream.GetRealFileName: string;
begin
  if CurrentPart <> nil then
    Result := CurrentPart.RealFileName
  else
    Result := '';
end;

function TZMMultiFileStream.GetReqFileName: string;
begin
  Result := TheZip.ReqFileName;
end;

function TZMMultiFileStream.GetTotalDisks: Integer;
begin
  Result := TheZip.TotalDisks;
end;

function TZMMultiFileStream.IsEndOfFile: Boolean;
begin
  Result := (CurrentPart <> nil) and CurrentPart.IsEndOfFile;
end;

function TZMMultiFileStream.IsRightDisk: Boolean;
var
  Fn: string;
  VName: string;
begin
  Result := True;
  if (Numbering < ZnsName) and (not WorkDrive.DriveIsFixed) then
  begin
    VName := WorkDrive.DiskName;
    if Body.Verbosity <= ZvTrace then
      Body.TraceFmt('Checking disk %s need %s', [VName, VolName(DiskNr)],
        {_LINE_}635, __UNIT__);
    if (AnsiSameText(VName, VolName(DiskNr)) or AnsiSameText(VName,
      OldVolName(DiskNr))) and FileExists(RealFileName) then
    begin
      Numbering := ZnsVolume;
      Body.TraceFmt('found volume %s', [VName], {_LINE_}640, __UNIT__);
      Exit;
    end;
  end;
  Fn := FCurrentName;
  if Numbering = ZnsNone then // not known yet
  begin
    FCurrentName := CreateMVFileNameEx(FCurrentName, True, True);
    // make compat name
    if FileExists(FCurrentName) then
    begin
      Numbering := ZnsExt;
      Exit;
    end;
    FCurrentName := Fn;
    FCurrentName := CreateMVFileNameEx(FCurrentName, True, False);
    // make numbered name
    if FileExists(FCurrentName) then
    begin
      Numbering := ZnsName;
      Exit;
    end;
    if WorkDrive.DriveIsFixed then
      Exit; // always true - only needed name
    FCurrentName := Fn; // restore
    Result := False;
    Exit;
  end;
  // numbering scheme already known
  if Numbering = ZnsVolume then
  begin
    Result := False;
    Exit;
  end;
  {ArchiveName}FCurrentName := CreateMVFileNameEx(FCurrentName, True, Numbering = ZnsExt);
  // fixed drive always true only needed new filename
  if (not WorkDrive.DriveIsFixed) and (not FileExists(FCurrentName)) then
  begin
    FCurrentName := Fn; // restore
    Result := False;
  end;
end;

function TZMMultiFileStream.LastWriteTime(var Last_write: TFileTime): Boolean;
begin
  Result := (CurrentPart <> nil) and CurrentPart.LastWriteTime(Last_write);
end;

function TZMMultiFileStream.NewSegment: Boolean;
var
  DiskFile: string;
  DiskSeq: Integer;
  MsgQ: string;
  OnGetNextDisk: TZMGetNextDiskEvent;
  OnStatusDisk: TZMStatusDiskEvent;
  Res: Integer;
  SegName: string;
begin
  Result := False;
  // If we write on a fixed disk the filename must change.
  // We will get something like: FileNamexxx.zip where xxx is 001,002 etc.
  // if CompatNames are used we get FileName.zxx where xx is 01, 02 etc.. last .zip
  if Numbering = ZnsNone then
  begin
    if SpCompatName in Span.Options then
      Numbering := ZnsExt
    else
      if WorkDrive.DriveIsFixed or (SpNoVolumeName in Span.Options) then
        Numbering := ZnsName
      else
        Numbering := ZnsVolume;
  end;
  DiskFile := ArchiveName;
  if Numbering <> ZnsVolume then
    DiskFile := CreateMVFileNameEx(DiskFile, False, Numbering = ZnsExt);
  CheckForDisk(True, SpWipeFiles in Span.Options);

  OnGetNextDisk := Master.OnGetNextDisk;
  // Allow clearing of removeable media even if no volume names
  if (not WorkDrive.DriveIsFixed) and (SpWipeFiles in Span.Options) and
    ((FZipDiskAction = ZdaErase) or not Assigned(OnGetNextDisk)) then
  begin
    // Do we want a format first?
    if Numbering = ZnsVolume then
      SegName := VolName(DiskNr)
      // default name
    else
      SegName := SZipSet + IntToStr(Succ(DiskNr));
    // Ok=6 NoFormat=-3, Cancel=-2, Error=-1
    case ZipFormat(SegName) of
      // Start formating and wait until BeforeClose...
      - 1:
        raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}732, __UNIT__);
      -2:
        raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}734, __UNIT__);
    end;
  end;
  if WorkDrive.DriveIsFixed or (Numbering <> ZnsVolume) then
    DiskSeq := DiskNr + 1
  else
  begin
    DiskSeq := StrToIntDef(Copy(TheZip.WorkDrive.DiskName, 9, 3), 1);
    if DiskSeq < 0 then
      DiskSeq := 1;
  end;
  FZipDiskStatus := [];
  Res := AskOverwriteSegment(DiskFile, DiskSeq);
  if (Res = IdYes) and (TheZip.WorkDrive.DriveIsFixed) and
    (SpCompatName in TheZip.Span.Options) and _Z_FileExists(ReqFileName) then
  begin
    Res := AskOverwriteSegment(ReqFileName, DiskSeq);
    if (Res = IdYes) then
      _Z_EraseFile(ReqFileName, Body.HowToDelete = HtdFinal);
  end;
  if (Res = 0) or (Res = IdCancel) or ((Res = IdNo) and WorkDrive.DriveIsFixed)
  then
    raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}756, __UNIT__);

  if Res = IdNo then
  begin // we will try again...
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    Exit;
  end;
  // Create the output file.
  File_Create(DiskFile);
//  if not File_Create(DiskFile) then
  if not IsOpen then
  begin // change proposed by Pedro Araujo
    MsgQ := Body.ZipLoadStr(ZE_NoOutFile);
    Res := Body.ZipMessageDlgEx('', MsgQ, ZmtError + DHC_SpanNoOut,
      [MbRetry, MbCancel]);
    if Res <> IdRetry then
      raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}774, __UNIT__);
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    Exit;
  end;

  // Get the free space on this disk, correct later if neccessary.
  WorkDrive.VolumeRefresh;

  // Set the maximum number of bytes that can be written to this disk(file).
  // Reserve space on/in all the disk/file.
  if (DiskNr = 0) and (Span.KeepFreeOnDisk1 > 0) or (Span.KeepFreeOnAllDisks > 0)
  then
  begin
    if (Span.KeepFreeOnDisk1 mod WorkDrive.VolumeSecSize) <> 0 then
      Span.KeepFreeOnDisk1 :=
        Succ(Span.KeepFreeOnDisk1 div WorkDrive.VolumeSecSize) *
        WorkDrive.VolumeSecSize;
    if (Span.KeepFreeOnAllDisks mod WorkDrive.VolumeSecSize) <> 0 then
      Span.KeepFreeOnAllDisks :=
        Succ(Span.KeepFreeOnAllDisks div WorkDrive.VolumeSecSize) *
        WorkDrive.VolumeSecSize;
  end;
  FAllowedSize := WorkDrive.VolumeSpace - Span.KeepFreeOnAllDisks;
  if (Span.MaxVolumeSize > 0) and (Span.MaxVolumeSize < FAllowedSize) then
    FAllowedSize := Span.MaxVolumeSize;
  // Reserve space on/in the first disk(file).
  if DiskNr = 0 then
    FAllowedSize := FAllowedSize - Span.KeepFreeOnDisk1;

  // Do we still have enough free space on this disk.
  if FAllowedSize < Span.MinFreeVolumeSize then // No, too bad...
  begin
    OnStatusDisk := Master.OnStatusDisk;
    File_Close;
    File_Delete(DiskFile);
    if Assigned(OnStatusDisk) then // v1.60L
    begin
      if Numbering <> ZnsVolume then
        DiskSeq := DiskNr + 1
      else
      begin
        DiskSeq := StrToIntDef(Copy(WorkDrive.DiskName, 9, 3), 1);
        if DiskSeq < 0 then
          DiskSeq := 1;
      end;
      FZipDiskAction := ZdaOk; // The default action
      FZipDiskStatus := [ZdsNotEnoughSpace];
      OnStatusDisk(Master, DiskSeq, DiskFile, FZipDiskStatus, FZipDiskAction);
      if FZipDiskAction = ZdaCancel then
        Res := IdCancel
      else
        Res := IdRetry;
    end
    else
    begin
      MsgQ := Body.ZipLoadStr(ZE_NoDiskSpace);
      Res := Body.ZipMessageDlgEx('', MsgQ, ZmtError + DHC_SpanSpace,
        [MbRetry, MbCancel]);
    end;
    if Res <> IdRetry then
      raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}836, __UNIT__);
    FDiskWritten := 0;

    NewDisk := True;
    // If all this was on a HD then this wouldn't be useful but...
    Result := True;
  end
  else
  begin
    // ok. it fits and the file is open
    // Set the volume label of this disk if it is not a fixed one.
    if not(WorkDrive.DriveIsFixed or (Numbering <> ZnsVolume)) then
    begin
      if not WorkDrive.RenameDisk(VolName(DiskNr)) then
        raise EZipMaster.CreateMsg(Body, ZE_NoVolume, {_LINE_}850, __UNIT__);
    end;
  end;
end;

function TZMMultiFileStream.OldVolName(Part: Integer): string;
begin
  Result := SPKBACK + ' ' + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMMultiFileStream.Read(var Buffer; Len: Integer): Integer;
begin
  Result := ReadSplit(Buffer, Len);
end;

function TZMMultiFileStream.ReadSplit(var Buffer; Len: Integer): Integer;
var
  Bp: PAnsiChar;
  SizeR: Integer;
  ToRead: Integer;
begin
  try
    ToRead := Len;
    if Len < 0 then
      ToRead := -Len;
    Bp := @Buffer;
    Result := 0;
    while ToRead > 0 do
    begin
      SizeR := CurrentPart.read(Bp^, ToRead);
      if SizeR <> ToRead then
      begin
        // Check if we are at the end of a input disk.
        if SizeR < 0 then
        begin
          Result := SizeR;
          Exit;
        end;
        // if  error or (len <0 and read some) or (end segment)
        if ((Len < 0) and (SizeR <> 0)) or not IsEndOfFile then
        begin
          Result := ZM_Error({_LINE_}892, ZE_ReadError);
          Exit;
        end;
        // It seems we are at the end, so get a next disk.
        SeekDisk(DiskNr + 1, False);
      end;
      if SizeR > 0 then
      begin
        Inc(Bp, SizeR);
        ToRead := ToRead - SizeR;
        Result := Result + SizeR;
      end;
    end;
  except
    on EZMException do
      raise;
    on E: Exception do
      Result := ZM_Error({_LINE_}909, ZE_ReadError);
  end;
end;

function TZMMultiFileStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := CurrentPart.Seek(Offset, Origin);
end;

function TZMMultiFileStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := CurrentPart.Seek(Offset, Origin);
end;

function TZMMultiFileStream.SeekDisk(DiskSeq: Integer;
  AllowEmpty: Boolean): Integer;
begin
  Result := DiskSeq; // hopefully
  if DiskNr = DiskSeq then
    Exit;
  // Close the file on the old disk first.
  DiskNr := DiskSeq;
  while True do
  begin
    if CurrentPart <> nil then
    begin
      Body.InformFmt('Closing current part %s', [CurrentPart.RealFileName], {_LINE_}940, __UNIT__);
      CurrentPart.File_Close;
    end;
    repeat
      NewDisk := True;
      CheckForDisk(False, SpTryFormat in Span.Options);
      if AllowEmpty and WorkDrive.HasMedia(SpTryFormat in Span.Options) then
      begin
        if WorkDrive.VolumeSpace = -1 then
          Exit; // unformatted
        if WorkDrive.VolumeSpace = WorkDrive.VolumeSize then
          Exit; // empty
      end;
    until IsRightDisk;

    if Body.Verbosity > ZvVerbose then
      Body.TraceFmt('Seeking part %d', [DiskSeq], {_LINE_}958, __UNIT__);
    CurrentPart.File_Open({ArchiveName}FCurrentName, FmShareDenyWrite or FmOpenRead);
    if CurrentPart.IsOpen then
      Break; // found
    Body.TraceFmt('could not open part: %d', [DiskSeq], {_LINE_}963, __UNIT__);
    if WorkDrive.DriveIsFixed then
      raise EZipMaster.CreateMsg(Body, ZE_NoInFile, {_LINE_}965, __UNIT__);
  end;
end;

procedure TZMMultiFileStream.SetArchiveName(const Value: string);
begin
  TheZip.ArchiveName := Value;
end;

procedure TZMMultiFileStream.SetCurrentPart(const Value: TZMSingleFileStream);
begin
  if FCurrentPart <> Value then
  begin
    FCurrentPart.Free;
    FCurrentPart := Value;
  end;
end;

procedure TZMMultiFileStream.SetDiskNr(const Value: Integer);
begin
  TheZip.DiskNr := Value;
end;

function TZMMultiFileStream.SetEndOfFile: Boolean;
begin
  Result := (CurrentPart <> nil) and CurrentPart.SetEndOfFile;
end;

procedure TZMMultiFileStream.SetIsTemp(const Value: Boolean);
begin
  FIsTemp := Value;
  if CurrentPart <> nil then
    CurrentPart.IsTemp := Value;
end;

procedure TZMMultiFileStream.SetNumbering(const Value: TZipNumberScheme);
begin
  TheZip.Numbering := Value;
end;

procedure TZMMultiFileStream.SetOpenMode(const Value: Integer);
begin
  FOpenMode := Value;
end;

procedure TZMMultiFileStream.SetPosition(const Value: Int64);
begin
  if CurrentPart <> nil then
    CurrentPart.Position := Value;
end;

procedure TZMMultiFileStream.SetTotalDisks(const Value: Integer);
begin
  TheZip.TotalDisks := Value;
end;

function TZMMultiFileStream.VolName(Part: Integer): string;
begin
  Result := SPKBACK + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMMultiFileStream.Write(const Buffer; Len: Integer): Integer;
begin
  Result := WriteSplit(Buffer, Len, 0);//False);
end;

// Contiguous 0 _ can split, <0 _ must fit current part, >0 _ don't split
function TZMMultiFileStream.WriteSplit(const Buffer; ToWrite, Contiguous:
    Integer): Integer;
var
  Buf: PAnsiChar;
  Len: Integer;
  MaxLen: Integer;
  Res: Integer;
begin { WriteSplit }
  Result := 0;
  try
    Len := ToWrite;
    Buf := @Buffer;  // at start of buffer
    Body.KeepAlive;
    Body.CheckCancel;

    // Keep writing until error or Buffer is empty.
    while Len > 0 do
    begin
      // Check if we have an output file already opened, if not: create one,
      // do checks, gather info.
      if (not IsOpen) then
      begin
        Body.TraceFmt('Starting to write new segment: %d', [DiskNr], 1058, __UNIT__);
        NewDisk := DiskNr <> 0; // allow first disk in drive
        if NewSegment then
        begin
          NewDisk := True;
          Continue; // try again
        end;
      end;

      // If contiguous check if we have at least ToWrite available on this disk,
      // headers are not allowed to cross disk boundaries.
      if (Contiguous <> 0) and (ToWrite > FAllowedSize) then
      begin // finish this part
        Body.TraceFmt('Cannot fit contiguous %d bytes in segment: %d',
          [ToWrite, DiskNr], 1072, __UNIT__);
        // all parts must be same stamp
        if TheZip.StampDate = 0 then
          TheZip.StampDate := LastWritten;
        TheZip.FinaliseWrite;  // close file, update stamp;

        FDiskWritten := 0;
        NewDisk := True;
        DiskNr := DiskNr + 1;
//        Continue;
        if Contiguous > 0 then
          continue; // write to next part
        Result := MustFitError;
        break;
      end;

      // Don't try to write more bytes than allowed on this disk.
      MaxLen := high(Integer);
      if FAllowedSize < MaxLen then
        MaxLen := Integer(FAllowedSize);
      if Len < MaxLen then
        MaxLen := Len;
      Res := CurrentPart.Write(Buf^, MaxLen);
      if (Res < 0) or ((Contiguous <> 0) and (Res < MaxLen)) then
      begin
        // A write error (disk removed?)
        if Res < 0 then
          Result := Res
        else
          Result := ZM_Error(1092, ZE_NoWrite);
        Break;
      end;
      Inc(FDiskWritten, Res);
      Inc(Result, Res);
      FAllowedSize := FAllowedSize - MaxLen;
      Dec(Len, MaxLen);
      if Len > 0 then
      begin
        // We still have some data left, we need a new disk.
        if TheZip.StampDate = 0 then
          TheZip.StampDate := LastWritten;
        TheZip.FinaliseWrite;  // close file, update stamp;
        // File_Close;
        FAllowedSize := 0;
        FDiskWritten := 0;
        DiskNr := DiskNr + 1;
        NewDisk := True;
        Inc(Buf, MaxLen);
      end;
    end; { while(Len > 0) }
  except
    on E: EZipMaster do
      Result := E.ExtErr;
  end;
end;

function TZMMultiFileStream.ZipFormat(const NewName: string): Integer;
var
  Msg: string;
  Res: Integer;
  Vol: string;
begin
  if NewName <> '' then
    Vol := NewName
  else
    Vol := WorkDrive.DiskName;
  if Length(Vol) > 11 then
    Vol := Copy(Vol, 1, 11);
  Result := -3;
  if WorkDrive.DriveIsFloppy then
  begin
    if (SpTryFormat in Span.Options) then
      Result := FormatFloppy(Application.Handle, WorkDrive.DriveStr);
    if Result = -3 then
    begin
      if Body.ConfirmErase then
      begin
        Msg := Body.ZipFmtLoadStr(ZS_Erase, [WorkDrive.DriveStr]);
        Res := Body.ZipMessageDlgEx(Body.ZipLoadStr(ZS_Confirm), Msg,
          ZmtWarning + DHC_FormErase, [MbYes, MbNo]);
        if Res <> IdYes then
        begin
          Result := -3; // cancel
          Exit;
        end;
      end;
      ClearFloppy(WorkDrive.DriveStr);
      Result := 0;
    end;
    WorkDrive.HasMedia(False);
    if (Result = 0) and (Numbering = ZnsVolume) then
      WorkDrive.RenameDisk(Vol);
  end;
end;

end.

