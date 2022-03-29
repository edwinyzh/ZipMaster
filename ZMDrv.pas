unit ZMDrv;

// ZMDrv.pas - drive details

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
{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZMBody;

type
  // 1 Provides details of drive
  TZMWorkDrive = class
  private
    FBody: TZMBody;
    FDiskName: string;
    FDiskReadOnly: Boolean;
    FDiskSerial: Cardinal;
    FDriveIsFloppy: Boolean;
    FDriveLetter: Char;
    FDriveStr: string;
    FDriveType: Integer;
    FLastDrive: string;
    FVolumeFreeClusters: DWORD;
    FVolumeSecSize: Cardinal;
    FVolumeSectorsPerCluster: DWORD;
    FVolumeSize: Int64;
    FVolumeSpace: Int64;
    FVolumeTotalClusters: DWORD;
    function GetDiskName: string;
    function GetDriveIsFixed: Boolean;
    procedure SetDrive(const Path: string);
    procedure SetDriveStr(const Value: string);
    procedure SetExSizes(Fields: Integer);
  public
    constructor Create(TheBody: TZMBody);
    procedure AfterConstruction; override;
    procedure AssignFrom(const Src: TZMWorkDrive);
    procedure Clear;
    function HasMedia(UnformOk: Boolean): Boolean;
    function RenameDisk(const NewName: string): Boolean;
    procedure VolumeRefresh;
    property DiskName: string read GetDiskName;
    property DiskReadOnly: Boolean read FDiskReadOnly;
    property DiskSerial: Cardinal read FDiskSerial;
    property DriveIsFixed: Boolean read GetDriveIsFixed;
    property DriveIsFloppy: Boolean read FDriveIsFloppy;
    property DriveLetter: Char read FDriveLetter;
    property DriveStr: string read FDriveStr write SetDriveStr;
    property DriveType: Integer read FDriveType;
    property VolumeFreeClusters: DWORD read FVolumeFreeClusters;
    property VolumeSecSize: Cardinal read FVolumeSecSize;
    property VolumeSectorsPerCluster: DWORD read FVolumeSectorsPerCluster;
    property VolumeSize: Int64 read FVolumeSize;
    property VolumeSpace: Int64 read FVolumeSpace;
    property VolumeTotalClusters: DWORD read FVolumeTotalClusters;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  ZMXcpt, ZMMsg;

const
  __UNIT__ = 14;

const
  MAX_REMOVABLE = 10 * 1024 * 1024;

constructor TZMWorkDrive.Create(TheBody: TZMBody);
begin
  inherited Create;
  FBody := TheBody;
end;

procedure TZMWorkDrive.AfterConstruction;
begin
  inherited;
  Clear;
end;

procedure TZMWorkDrive.AssignFrom(const Src: TZMWorkDrive);
begin
  if (Self <> Src) then
  begin
    FDiskName := Src.DiskName;
    FDiskReadOnly := Src.DiskReadOnly;
    FDiskSerial := Src.DiskSerial;
    FDriveIsFloppy := Src.DriveIsFloppy;
    FDriveLetter := Src.DriveLetter;
    FDriveStr := Src.DriveStr;
    FDriveType := Src.DriveType;
    FVolumeFreeClusters := Src.VolumeFreeClusters;
    FVolumeSecSize := Src.VolumeSecSize;
    FVolumeSectorsPerCluster := Src.VolumeSectorsPerCluster;
    FVolumeSize := Src.VolumeSize;
    FVolumeSpace := Src.VolumeSpace;
    FVolumeTotalClusters := Src.VolumeTotalClusters;
  end;
end;

procedure TZMWorkDrive.Clear;
begin
  FDiskName := '';
  FLastDrive := '';
  FDiskReadOnly := False;
  FDiskSerial := 0;
  FDriveIsFloppy := False;
  FDriveLetter := #0;
  FDriveStr := '';
  FDriveType := 0;
  FVolumeSecSize := 512;
  FVolumeSectorsPerCluster := 4;
  FVolumeSize := 0;
  FVolumeSpace := 0;
  FVolumeTotalClusters := 0;
end;

function TZMWorkDrive.GetDiskName: string;
begin
  Result := '';
  if DriveIsFloppy then
  begin
    if (FDiskName = '') then
      HasMedia(False);
    Result := FDiskName;
  end;
end;

function TZMWorkDrive.GetDriveIsFixed: Boolean;
begin
  Result := not(DriveIsFloppy or (DriveType = DRIVE_CDROM));
end;

function TZMWorkDrive.HasMedia(UnformOk: Boolean): Boolean;
var
  Err: Cardinal;
  NamLen: Cardinal;
  OldErrMode: DWord;
  SysFlags: DWord;
  SysLen: DWord;
  VolNameAry: array [0 .. 255] of Char;
begin
  NamLen := 255;
  SysLen := 255;
  FVolumeSize := 0;
  FVolumeSpace := 0;
  FDiskName := '';
  FDiskSerial := 0;
  VolNameAry[0] := #0;

  Result := (DriveLetter = #0);
  if Result then
    Exit; // assume connected to media - net or stream

  if DriveType = DRIVE_NO_ROOT_DIR then
    raise EZipMaster.CreateMsgFmt(FBody, ZE_DriveNoMount, [DriveStr],
      {_LINE_}210, __UNIT__);

  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  // Turn off critical errors:
  // Since v1.52c no exception will be raised here; moved to List() itself.
  // 1.72 only get Volume label for removable drives
  if (not GetVolumeInformation(Pchar(DriveStr), VolNameAry, NamLen,
    PDWORD(@FDiskSerial), SysLen, SysFlags, nil, 0)) then
  begin
    // W'll get this if there is a disk but it is not or wrong formatted
    // so this disk can only be used when we also want formatting.
    Err := GetLastError();
    if (Err = 31) and (UnformOk) then
      Result := True;
  end
  else
  begin
    FDiskName := VolNameAry;
    FDiskReadOnly := False;
    { get free disk space and size. }
    SetExSizes(7); // RCV150199
  end;

  SetErrorMode(OldErrMode); // Restore critical errors:

  // -1 is not very likely to happen since GetVolumeInformation catches errors.
  // But on W95(+OSR1) and a UNC filename w'll get also -1, this would prevent
  // opening the file. !!!Potential error while using spanning with a UNC filename!!!
  if ((DriveLetter <> #0) and (VolumeSize <> -1)) then
    Result := True;
end;

function TZMWorkDrive.RenameDisk(const NewName: string): Boolean;
begin
  Result := False;
  if DriveIsFloppy and HasMedia(False) and not DiskReadOnly and
    SetVolumeLabel(PChar(DriveStr), PChar(NewName)) then
  begin
    HasMedia(False); // get new name
    Result := True;
  end;
end;

procedure TZMWorkDrive.SetDrive(const Path: string);
var
  S: string;
begin
  Clear;
  if (Path <> '') and (Path[1] <> '<') then
  begin
    S := '';
    if Length(Path) > 1 then
      S := Copy(Path, 1, 2);
    if (S <> '//') and (S <> '\\') then
    begin
      S := Uppercase(ExtractFileDrive(ExpandFileName(Path)) + '\');
      if (Length(S) = 3) and (S[2] = ':') then
      begin
        // a local drive
        FDriveStr := S;
        FDriveLetter := S[1];
        FDriveType := GetDriveType(Pchar(S));
        if DriveType = DRIVE_REMOVABLE then
        begin
          if (DriveLetter = 'A') or (DriveLetter = 'B') then
            FDriveIsFloppy := True;
        end;
      end
    end;
  end;
end;

procedure TZMWorkDrive.SetDriveStr(const Value: string);
begin
  if Value <> FDriveStr then
    SetDrive(Value);
end;

procedure TZMWorkDrive.SetExSizes(Fields: Integer);
var
  BytesPSector: DWORD;
  LDiskFree: Int64;
  LSizeOfDisk: Int64;
  SSize: Cardinal;
begin
  LDiskFree := -1;
  LSizeOfDisk := -1;
  SSize := 0;
  if GetDiskFreeSpace(Pchar(DriveStr), FVolumeSectorsPerCluster, BytesPSector,
    FVolumeFreeClusters, FVolumeTotalClusters) then
  begin
    SSize := BytesPSector;
  end;
  if not GetDiskFreeSpaceEx(Pchar(DriveStr), LDiskFree, LSizeOfDisk, nil) then
  begin
    LDiskFree := -1;
    LSizeOfDisk := -1;
    if SSize <> 0 then
    begin
      LDiskFree := Int64(BytesPSector) * VolumeSectorsPerCluster *
        VolumeFreeClusters;
      LSizeOfDisk := Int64(BytesPSector) * VolumeSectorsPerCluster *
        VolumeTotalClusters;
    end;
  end;
  if (Fields and 1) <> 0 then
    FVolumeSpace := LDiskFree;
  if (Fields and 2) <> 0 then
    FVolumeSize := LSizeOfDisk;
  if (Fields and 4) <> 0 then
    FVolumeSecSize := SSize;
end;

procedure TZMWorkDrive.VolumeRefresh;
begin
  SetExSizes(7);
end;

end.
