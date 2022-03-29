unit ZMZipMulti;

// ZMZipMulti.pas - basic in/out for multi-part zip files

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

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils,
{$ENDIF}
  ZipMstr, ZMBody, ZMZipBase,  ZMMFStream;

type
  TZMZipMulti = class(TZMZipBase)
  private
    FIsMultiPart: Boolean;
    FMultiFile: TZMMultiFileStream;
    FNewDisk: Boolean;
    FSig: TZipFileSigs;
    FZipDiskAction: TZMDiskAction;
    FZipDiskStatus: TZMZipDiskStatus;
    procedure SetIsMultiPart(const Value: Boolean);
  protected
    procedure ForceMultiPart;
    procedure SetPosition(const Value: Int64); override;
    procedure SetStream(const Value: TStream); override;
    function VolName(Part: Integer): string;
    property MultiFile: TZMMultiFileStream read FMultiFile;
  public
    procedure AfterConstruction; override;
    function AskAnotherDisk(const DiskFile: string): Integer;
    procedure CheckForDisk(Writing, UnformOk: Boolean);
    function CreateMVFileNameEx(const FileName: string;
      StripPartNbr, Compat: Boolean): string;
    function FinishWrite: Integer;
    function MapNumbering(Opts: TZMSpanOpts): TZMSpanOpts;
    function Name(Expanded: Boolean = False): string; override;
    function RefuseWriteSplit: Integer;
    function SeekDisk(DiskSeq: Integer; AllowEmpty: Boolean): Integer;
    function WriteContiguous(const Buffer; Len: Integer; MustFit: Boolean): Integer;
    property IsMultiPart: Boolean read FIsMultiPart write SetIsMultiPart;
    property NewDisk: Boolean read FNewDisk write FNewDisk;
    property Sig: TZipFileSigs read FSig write FSig;
  end;

const
  // zfi_None: Cardinal = 0;
  // zfi_Open: Cardinal = 1;
  // zfi_Create: Cardinal = 2;
  Zfi_Dirty: Cardinal = 4;
  Zfi_MakeMask: Cardinal = $07;
  Zfi_Error: Cardinal = 8;
  // zfi_NotFound: cardinal = $10;     // named file not found
  // zfi_NoLast: cardinal = $20;       // last file not found
  Zfi_Loading: Cardinal = $40;
  Zfi_Cancelled: Cardinal = $80; // loading was cancelled
  // zfi_FileMask: cardinal = $F0;

const
  MustFitError = -300; // returned by WriteContiguous when won't fit in part

implementation

uses
{$IFDEF VERDXE2up}
  VCL.Forms, VCL.Controls, VCL.Dialogs, WinApi.ShlObj,
{$ELSE}
  Forms, Controls, Dialogs, ShlObj, {$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMCtx, ZMDlg,
  ZMStructs, ZMUtils, ZMMsg, ZMWinFuncs, ZMFStream, ZMCore, ZMDrv, ZMXcpt;

const
  __UNIT__ = 45;

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

procedure TZMZipMulti.AfterConstruction;
begin
  inherited;
  FIsMultiPart := False;
end;

function TZMZipMulti.AskAnotherDisk(const DiskFile: string): Integer;
var
  MsgQ: string;
  TmpStatusDisk: TZMStatusDiskEvent;
begin
  MsgQ := ZipLoadStr(ZS_AnotherDisk);
  FZipDiskStatus := FZipDiskStatus + [ZdsSameFileName];
  TmpStatusDisk := Master.OnStatusDisk;
  if Assigned(TmpStatusDisk) and not AnswerAll then
  begin
    FZipDiskAction := ZdaOk; // The default action
    TmpStatusDisk(Master, 0, DiskFile, FZipDiskStatus, FZipDiskAction);
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
        end;
      ZdaOk:
        Result := IdOk;
    else
      Result := IdOk;
    end;
  end
  else
    Result := ZipMessageDlgEx(ZipLoadStr(ZS_Confirm), MsgQ,
      ZmtWarning + DHC_SpanOvr, [MbOk, MbCancel]);
end;

procedure TZMZipMulti.CheckForDisk(Writing, UnformOk: Boolean);
var
  AbortAction: Boolean;
  MsgFlag: Integer;
  MsgStr: string;
  OnGetNextDisktmp: TZMGetNextDiskEvent;
  Res: Integer;
  SizeOfDisk: Int64;
  TotDisks: Integer;
begin
  if TotalDisks <> 1 then // check
    IsMultiPart := True;
  if WorkDrive.DriveIsFixed then
  begin
    // If it is a fixed disk we don't want a new one.
    NewDisk := False;
    CheckCancel;
    Exit;
  end;
  KeepAlive; // just ProcessMessages
  // First check if we want a new one or if there is a disk (still) present.
  while (NewDisk or (not WorkDrive.HasMedia(UnformOk))) do
  begin
    if Unattended then
      raise EZipMaster.CreateMsg(Body, ZE_NoUnattSpan, {_LINE_}197, __UNIT__);

    MsgFlag := ZmtWarning + DHC_SpanNxtW; // or error?
    if DiskNr < 0 then // want last disk
    begin
      MsgStr := ZipLoadStr(ZS_InsertDisk);
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
          raise EZipMaster.CreateMsg(Body, ZE_TooManyParts, {_LINE_}217,
            __UNIT__);
        if SizeOfDisk > 0 then
        begin
          TotDisks := Trunc((File_Size + 4 + Span.KeepFreeOnDisk1) /
            SizeOfDisk);
          if TotalDisks < TotDisks then
            TotalDisks := TotDisks;
          MsgStr := ZipFmtLoadStr(ZS_InsertVolume, [DiskNr + 1, TotalDisks]);
        end
        else
          MsgStr := ZipFmtLoadStr(ZS_InsertAVolume, [DiskNr + 1]);
      end
      else
      begin // reading - want specific disk
        if TotalDisks = 0 then
          MsgStr := ZipFmtLoadStr(ZS_InsertAVolume, [DiskNr + 1])
        else
          MsgStr := ZipFmtLoadStr(ZS_InsertVolume, [DiskNr + 1, TotalDisks]);
      end;

    MsgStr := MsgStr + ZipFmtLoadStr(ZS_InDrive, [WorkDrive.DriveStr]);
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
      Res := ZipMessageDlgEx('', MsgStr, MsgFlag, MbOkCancel);

    if Res <> IdOk then
    begin
      Cancel := ZM_Error({_LINE_}255, ZS_Abort);
      Info := Info or Zfi_Cancelled;
      raise EZipMaster.CreateMsg(Body, ZS_Canceled, {_LINE_}257, __UNIT__);
    end;
    NewDisk := False;
    KeepAlive;
  end;
end;

// changes FileName into multi volume FileName
function TZMZipMulti.CreateMVFileNameEx(const FileName: string;
  StripPartNbr, Compat: Boolean): string;
begin
  Result := MultiFile.CreateMVFileNameEx(FileName, StripPartNbr, Compat);
end;

// rename last part after Write
function TZMZipMulti.FinishWrite: Integer;
var
  Fn: string;
  LastName: string;
  MsgStr: string;
  OnStatusDisk: TZMStatusDiskEvent;
  Res: Integer;
begin
  // change extn of last file
  LastName := RealFileName; // name of last file written
  FinaliseWrite; // set date and attrs if required
  File_Close;
  Result := 0;

  if IsMultiPart then
  begin
    if ((Numbering = ZnsExt) and not AnsiSameText(ExtractFileExt(LastName),
      EXT_ZIP)) or ((Numbering = ZnsName) and (DiskNr = 0)) then
    begin
      Result := -1;
      Fn := ArchiveName;
      if (FileExists(Fn)) then
      begin
        MsgStr := ZipFmtLoadStr(ZS_AskDeleteFile, [Fn]);
        FZipDiskStatus := FZipDiskStatus + [ZdsSameFileName];
        Res := IdYes;
        if not AnswerAll then
        begin
          OnStatusDisk := Master.OnStatusDisk;
          if Assigned(OnStatusDisk) then // 1.77
          begin
            FZipDiskAction := ZdaOk; // The default action
            OnStatusDisk(Master, DiskNr, Fn, FZipDiskStatus, FZipDiskAction);
            if FZipDiskAction = ZdaYesToAll then
            begin
              AnswerAll := True;
              FZipDiskAction := ZdaOk;
            end;
            if FZipDiskAction = ZdaOk then
              Res := IdYes
            else
              Res := IdNo;
          end
          else
            Res := ZipMessageDlgEx(MsgStr, ZipLoadStr(ZS_Confirm),
              ZmtWarning + DHC_WrtSpnDel, [MbYes, MbNo]);
        end;
        if (Res = IdNo) then
          Body.ReportMsg(ZM_Error({_LINE_}320, ZE_NoRenamePart),
            [LastName]);
        if (Res = IdYes) then
          File_Delete(Fn); // if it exists delete old one
      end;
      if _Z_FileExists(LastName) then // should be there but ...
      begin
        _Z_RenameFile(LastName, Fn);
        Result := 0;
        Body.TraceFmt('renamed %s to %s', [LastName, Fn], {_LINE_}329,
          __UNIT__);
      end;
    end;
  end;
end;

procedure TZMZipMulti.ForceMultiPart;
var
  SStream: TStream;
begin
  if (MultiFile = nil) and (MyStream <> nil) then
  begin
    Body.Trace('Forcing multi-part processing', {_LINE}396, __UNIT__);
    // detach MyStream
    SStream := ReleaseStream;
    Stream := TZMMultiFileStream.Create(Self, SStream as TZMSingleFileStream);
  end;
end;

function TZMZipMulti.MapNumbering(Opts: TZMSpanOpts): TZMSpanOpts;
begin
  Result := Opts;
  if Numbering <> ZnsNone then
  begin
    // map numbering type only if known
    Result := Result - [SpCompatName] + [SpNoVolumeName];
    case Numbering of
      ZnsVolume:
        Result := Result - [SpNoVolumeName];
      ZnsExt:
        Result := Result + [SpCompatName];
    end;
  end;
end;

function TZMZipMulti.Name(Expanded: Boolean = False): string;
begin
  if ReqFileName = '' then
    Result := inherited name(Expanded)
  else
  begin
    Result := ReqFileName;
    if Alias <> '' then
    begin
      if Expanded then
        Result := Alias + '<' + Result + '>'
      else
        Result := Alias;
    end;
  end;
end;

function TZMZipMulti.RefuseWriteSplit: Integer;
begin
  Result := 0;
  if ZwoDiskSpan in WriteOptions then
  begin
    if IsExtStream then
      Result := ZE_NoStreamSpan
    else
      if Unattended and WorkDrive.DriveIsFloppy and
        not Assigned(Master.OnGetNextDisk) then
        Result := ZE_NoUnattSpan;
  end;
end;

function TZMZipMulti.SeekDisk(DiskSeq: Integer; AllowEmpty: Boolean): Integer;
begin
  if (TotalDisks > 1) or (DiskSeq <> 0) then
  begin
    IsMultiPart := True;
    if MultiFile = nil then
      ForceMultiPart;
  end;
  if MultiFile <> nil then
    Result := MultiFile.SeekDisk(DiskSeq, AllowEmpty)
  else
    Result := ZM_Error(951, ZE_InternalError);
end;

procedure TZMZipMulti.SetIsMultiPart(const Value: Boolean);
begin
  if FIsMultiPart <> Value then
  begin
    FIsMultiPart := Value;
    if Value then
      ForceMultiPart;
  end;
end;

procedure TZMZipMulti.SetPosition(const Value: Int64);
begin
  Seek(Value, SoBeginning);
end;

procedure TZMZipMulti.SetStream(const Value: TStream);
begin
  inherited;  // will set MyStream if applicable
  if MyStream is TZMMultiFileStream then
    FMultiFile := TZMMultiFileStream(MyStream)
  else
    FMultiFile := nil;
end;

function TZMZipMulti.VolName(Part: Integer): string;
begin
  Result := SPKBACK + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMZipMulti.WriteContiguous(const Buffer; Len: Integer; MustFit:
    Boolean): Integer;
var
  Flag: Integer;
begin
  if IsMultiPart then
  begin
    if not (Stream is TZMMultiFileStream) then
    begin
      // have not made it yet
      Stream := TZMMultiFileStream.Create(Self, RealFileName);
    end;
    if MustFit then
      Flag := -1
    else
      Flag := 1;
    Result := MultiFile.WriteSplit(Buffer, Len, Flag);//True);
  end
  else
    Result := inherited Write(Buffer, Len);
end;

end.

