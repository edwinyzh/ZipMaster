unit ZMFileOpr;

// ZMFileOpr.pas - file operations
(*
 Derived from
 * SFX for DelZip v1.7
 * Copyright 2002-2005
 * written by Markus Stephany
*)

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
  WinApi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Graphics,
  Vcl.Dialogs, Vcl.Controls,
{$ELSE}
  Windows, SysUtils, Classes, Forms, Graphics, Dialogs, Controls,
{$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMBaseOpr, ZMStructs, ZMZipReader, ZMZipWriter;

type
  TZMFileOpr = class(TZMBaseOpr)
  private
    Detached: Boolean;
    function BrowseResDir(ResStart, Dir: PIRD; Depth: Integer): PIRDatE;
    function SearchResDirEntry(ResStart: PIRD; Entry: PIRDirE;
      Depth: Integer): PIRDatE;
    function WriteMulti(Src: TZMZipReader; Dest: TZMZipCopier;
      UseXProgress: Boolean): Integer;
  protected
    function NewSFXFile(const ExeName: string): Integer;
  public
    function ConvertToSFX(const OutName: string; TheZip: TZMZipReader): Integer;
    function ConvertToSpanSFX(const OutFileName: string;
      TheZip: TZMZipReader): Integer;
    function ConvertToZIP: Integer;
    function ReadSpan(const InFileName: string; var OutFilePath: string;
      UseXProgress: Boolean): Integer;
    function WriteSpan(const InFileName, OutFileName: string;
      UseXProgress: Boolean): Integer;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.UITypes, Winapi.ShellAPI,
{$ELSE}
  ShellAPI,
{$ENDIF}
  ZipMstr, ZMMsg, ZMDrv, ZMUtils, ZMZipBase, ZMWinFuncs, ZMLister,
  ZMBody, ZMMisc, ZMXcpt;

const
  __UNIT__ = 17;

const
  SPKBACK001 = 'PKBACK#001';
  { File Extensions }
  ExtZip = 'zip';
  DotExtZip = '.' + ExtZip;
  ExtExe = 'exe';
  DotExtExe = '.' + ExtExe;
  ExtBin = 'bin';
  ExtZSX = 'zsx';

const
  MinStubSize = 12000;
  MaxStubSize = 80000;
  BufSize = 10240;
  // 8192;   // Keep under 12K to avoid Winsock problems on Win95.
  // If chunks are too large, the Winsock stack can
  // lose bytes being sent or received.

type
  TFileNameIs = (FiExe, FiZip, FiOther, FiEmpty);

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

// get the kind of filename
function GetFileNameKind(const SFile: TFileName): TFileNameIs;
var
  SExt: string;
begin
  if SFile = '' then
    Result := FiEmpty
  else
  begin
    SExt := LowerCase(ExtractFileExt(SFile));
    if SExt = DotExtZip then
      Result := FiZip
    else
      if SExt = DotExtExe then
        Result := FiExe
      else
        Result := FiOther;
  end;
end;

function TZMFileOpr.BrowseResDir(ResStart, Dir: PIRD; Depth: Integer): PIRDatE;
var
  I: Integer;
  SingleRes: PIRDirE;
  X: PByte;
begin
  Result := nil;
  X := PByte(Dir);
  Inc(X, Sizeof(IMAGE_RESOURCE_DIRECTORY));
  SingleRes := PIRDirE(X);

  for I := 1 to Dir.NumberOfNamedEntries + Dir.NumberOfIdEntries do
  begin
    Result := SearchResDirEntry(ResStart, SingleRes, Depth);
    if Result <> nil then
      Break; // Found the one w're looking for.
  end;
end;

function TZMFileOpr.ConvertToSFX(const OutName: string;
  TheZip: TZMZipReader): Integer;
var
  Nn: string;
  Oz: TZMZipCopier;
  UseTemp: Boolean;
begin
  Body.Trace('ConvertToSFX', {_LINE_}181, __UNIT__);
  if TheZip = nil then
    TheZip := CurrentZip(True); // use Current
  if TheZip.IsExtStream then
    raise EZipMaster.CreateMsg(Body, ZE_StreamNoSupport, {_LINE_}185, __UNIT__);
  Detached := False;
  Result := PrepareStub;
  if (Result < 0) or not Assigned(SFXBinStream) then
  begin
    // result:= some error;
    Exit;
  end;
  if OutName = '' then
    Nn := ChangeFileExt(TheZip.ArchiveName, DotExtExe)
  else
  begin
    Result := DriveFolders.ExpandPath(Nn, OutName);
    if Result < 0 then
      Exit;
  end;
  UseTemp := _Z_FileExists(Nn);
  Oz := TZMZipCopier.Create(Lister);
  try
    if UseTemp then
      Oz.File_CreateTemp(ExtZSX, '')
    else
      Oz.File_Create(Nn);
    Oz.Stub := SFXBinStream;
    SFXBinStream := nil;
    Oz.UseSFX := True;
    Result := Oz.WriteFile(TheZip, True);
    TheZip.File_Close;
    if (Result >= 0) then
    begin
      if UseTemp and not Oz.File_Rename(Nn, HowToDelete <> HtdFinal) then
        raise EZipMaster.CreateMsgFmt(Body, ZE_CopyFailed, [Oz.ArchiveName, Nn],
          {_LINE_}217, __UNIT__);
      Result := 0;
      Lister.Set_ZipFileName(Nn, ZloFull);
    end;
  finally
    Oz.Free;
  end;
end;

function TZMFileOpr.ConvertToSpanSFX(const OutFileName: string;
  TheZip: TZMZipReader): Integer;
var
  DiskFile: string;
  DiskSerial: Cardinal;
  Dummy1: Cardinal;
  Dummy2: Cardinal;
  FileListSize: Cardinal;
  FreeOnDisk1: Cardinal;
  KeepFree: Cardinal;
  LDiskFree: Cardinal;
  MsgStr: string;
  OrgKeepFree: Cardinal;
  OutDrv: TZMWorkDrive;
  OutName: string;
  PartFileName: string;
  RightDiskInserted: Boolean;
  SFXName: string;
  SplitZip: TZMZipCopier;
  VolName: array [0 .. MAX_PATH - 1] of Char;
begin
  Detached := True;
  Result := DriveFolders.ExpandPath(OutName, OutFileName);
  if Result >= 0 then
    // prepare stub
    Result := PrepareStub;
  if (Result >= 0) and Assigned(SFXBinStream) then
  begin
    SplitZip := nil;
    if TheZip = nil then
      TheZip := Current; // use Current
    PartFileName := ChangeFileExt(OutName, DotExtZip);
    // delete the existing sfx stub
    File_Delete(OutName);
    SFXName := ExtractFileName(PartFileName);
    FileListSize := DetachedSize(TheZip);
    OrgKeepFree := Span.KeepFreeOnDisk1;
    OutDrv := TZMWorkDrive.Create(Body);
    try
      // get output parameters
      OutDrv.DriveStr := OutName;
      OutDrv.HasMedia(True); // set media details

      // calulate the size of the sfx stub
      Result := 0; // is good (at least until it goes bad)

      if (not OutDrv.DriveIsFixed) and (Span.MaxVolumeSize = 0) then
        Span.MaxVolumeSize := OutDrv.VolumeSize;
      // first test if multiple parts are really needed
      if (Span.MaxVolumeSize <= 0) or
        ((TheZip.File_Size + SFXBinStream.Size) < Span.MaxVolumeSize) then
      begin
        Body.Trace('Too small for span sfx', {_LINE_}278, __UNIT__);
        Detached := False;
        Result := ConvertToSFX(OutName, TheZip);
      end
      else
      begin
        FileListSize := FileListSize + Sizeof(Integer) +
          Sizeof(TZipEndOfCentral);
        if Span.KeepFreeOnDisk1 <= 0 then
          KeepFree := 0
        else
          KeepFree := Span.KeepFreeOnDisk1;
        KeepFree := KeepFree + FileListSize;
        if OutDrv.VolumeSize > MAXINT then
          LDiskFree := MAXINT
        else
          LDiskFree := Cardinal(OutDrv.VolumeSize);
        { only one set of ' span' params }
        if (Span.MaxVolumeSize > 0) and (Span.MaxVolumeSize < LDiskFree) then
          LDiskFree := Span.MaxVolumeSize;
        if (FileListSize > LDiskFree) then
          Result := ZM_Error({_LINE_}299, ZE_DetachedHeaderTooBig);

        if Result = 0 then // << moved
        begin
          if (KeepFree mod OutDrv.VolumeSecSize) <> 0 then
            FreeOnDisk1 := ((KeepFree div OutDrv.VolumeSecSize) + 1) *
              OutDrv.VolumeSecSize
          else
            FreeOnDisk1 := KeepFree;

          Span.KeepFreeOnDisk1 := FreeOnDisk1;
          SplitZip := TZMZipCopier.Create(Lister);
          SplitZip.ArchiveName := PartFileName;
          SplitZip.WorkDrive.DriveStr := OutName;
          Result := WriteMulti(TheZip, SplitZip, True);
          // if all went well - rewrite the loader correctly
          if (Result >= 0) and not OutDrv.DriveIsFixed then
          begin
            // for removable disk we need to insert the first again
            RightDiskInserted := False;
            while not RightDiskInserted do
            begin // ask to insert the first disk
              MsgStr := ZipFmtLoadStr(ZS_InsertAVolume, [1]) +
                ZipFmtLoadStr(ZS_InDrive, [OutDrv.DriveStr]);

              MessageDlg(MsgStr, MtInformation, [MbOK], 0);
              // check if right disk is inserted
              if SplitZip.Numbering = ZnsVolume then
              begin
                GetVolumeInformation(PChar(@OutDrv.DriveStr), VolName, MAX_PATH,
                  PDWORD(@DiskSerial), Dummy1, Dummy2, nil, 0);
                if (StrComp(VolName, SPKBACK001) = 0) then
                  RightDiskInserted := True;
              end
              else
              begin
                DiskFile := Copy(PartFileName, 1, Length(PartFileName) -
                  Length(ExtractFileExt(PartFileName))) + '001.zip';
                if FileExists(DiskFile) then
                  RightDiskInserted := True;
              end;
            end;
          end;
          // write the loader
          if Result >= 0 then
            Result := WriteDetached(SplitZip);
        end;
      end;
    finally
      FreeAndNil(SplitZip);
      FreeAndNil(OutDrv);
      // restore original value
      Span.KeepFreeOnDisk1 := OrgKeepFree;
    end;
  end;
end;

function TZMFileOpr.ConvertToZIP: Integer;
var
  Cz: TZMZipReader;
  Nn: string;
  Oz: TZMZipCopier;
  UseTemp: Boolean;
begin
  Body.Trace('ConvertToZip', {_LINE_}363, __UNIT__);
  Cz := CurrentZip(True);
  if Cz.IsExtStream then
    raise EZipMaster.CreateMsg(Body, ZE_StreamNoSupport, {_LINE_}366, __UNIT__);
  Nn := ChangeFileExt(Cz.ArchiveName, DotExtZip);
  UseTemp := _Z_FileExists(Nn);
  Oz := TZMZipCopier.Create(Lister);
  try
    if UseTemp then
      Oz.File_CreateTemp(ExtZSX, '')
    else
      Oz.File_Create(Nn);
    Result := Oz.WriteFile(Cz, True);
    Cz.File_Close;
    if (Result >= 0) then
    begin
      if UseTemp and not Oz.File_Rename(Nn, HowToDelete <> HtdFinal) then
        raise EZipMaster.CreateMsgFmt(Body, ZE_CopyFailed, [Oz.ArchiveName, Nn],
          {_LINE_}381, __UNIT__);
      Result := 0;
      Lister.Set_ZipFileName(Nn, ZloFull);
    end;
  finally
    Oz.Free;
  end;
end;

function TZMFileOpr.NewSFXFile(const ExeName: string): Integer;
var
  Eoc: TZipEndOfCentral;
  Fs: TFileStream;
begin
  Body.Trace('Write empty SFX', {_LINE_}395, __UNIT__);
  Fs := nil;
  Result := PrepareStub;
  if Result <> 0 then
    Exit;
  try
    Eoc.HeaderSig := EndCentralDirSig;
    Eoc.ThisDiskNo := 0;
    Eoc.CentralDiskNo := 0;
    Eoc.CentralEntries := 0;
    Eoc.TotalEntries := 0;
    Eoc.CentralSize := 0;
    Eoc.CentralOffset := 0;
    Eoc.ZipCommentLen := 0;
    SFXBinStream.WriteBuffer(Eoc, Sizeof(Eoc));
    Result := 0;
    SFXBinStream.Position := 0;
    Fs := TFileStream.Create(ExeName, FmCreate);
    Result := Fs.CopyFrom(SFXBinStream, SFXBinStream.Size);
    if Result <> SFXBinStream.Size then
      Result := ZM_Error({_LINE_}415, ZE_WriteError)
    else
      Result := 0;
    Body.Trace('finished write empty SFX', {_LINE_}418, __UNIT__);
  finally
    FreeAndNil(Fs);
    SFXBinStream.Free;
    SFXBinStream := nil;
  end;
end;

function TZMFileOpr.ReadSpan(const InFileName: string; var OutFilePath: string;
  UseXProgress: Boolean): Integer;
var
  Fd: TZMZipCopier;
  Fs: TZMZipReader;
  InName: string;
begin
  Fd := nil;
  Fs := nil;
  // use absolute paths
  Result := DriveFolders.ExpandPath(InName, InFileName);
  if Result >= 0 then
    Result := DriveFolders.ExpandPath(OutFilePath, OutFilePath);
  if Result < 0 then
    Exit;
  try
    // If we don't have a filename we make one first.
    if ExtractFileName(OutFilePath) = '' then
    begin
      OutFilePath := Master.MakeTempFileName('', '');
      // TODO: wrong need temp name on OutFilePath
      if OutFilePath = '' then
        Result := ZM_Error({_LINE_}448, ZE_NoTempFile);
    end
    else
    begin
      _Z_EraseFile(OutFilePath, HowToDelete = HtdFinal);
      OutFilePath := ChangeFileExt(OutFilePath, EXT_ZIP);
    end;

    if Result = 0 then
    begin
      Fs := TZMZipReader.Create(Lister);
      // Try to get the last disk from the user if part of Volume numbered set
      Fs.ArchiveName := InName;
      Result := Fs.OpenZip(False, False);
    end;
    if Result >= 0 then
    begin
      // InFileName opened successfully
      Fd := TZMZipCopier.Create(Lister);
      if Fd.File_Create(OutFilePath) then
      begin
        Fd.IsTemp := True;
        ShowProgress := ZspFull;
        Result := Fd.WriteFile(Fs, True);
        if Result >= 0 then
        begin
          Fd.IsTemp := False;
          Result := 0; // good
        end;
      end
      else
        Result := ZM_Error({_LINE_}479, ZE_NoOutFile);
    end;
  finally
    FreeAndNil(Fs);
    FreeAndNil(Fd);
  end;
end;

function TZMFileOpr.SearchResDirEntry(ResStart: PIRD; Entry: PIRDirE;
  Depth: Integer): PIRDatE;
var
  X: PByte;
begin
  Result := nil;
  if Entry.Un1.NameIsString <> 0 then
    Exit; // No named resources.
  if (Depth = 0) and (Entry.Un1.Id <> 3) then
    Exit; // Only icon resources.
  if (Depth = 1) and (Entry.Un1.Id <> 1) then
    Exit; // Only icon with ID 0x1.
  if Entry.Un2.DataIsDirectory = 0 then
  begin
    X := PByte(ResStart);
    Inc(X, Entry.Un2.OffsetToData);
    Result := PIRDatE(X);
  end
  else
  begin
    X := PByte(ResStart);
    Inc(X, Entry.Un2.OffsetToDirectory);
    Result := BrowseResDir(ResStart, PIRD(X), Depth + 1);
  end;
end;

function TZMFileOpr.WriteMulti(Src: TZMZipReader; Dest: TZMZipCopier;
  UseXProgress: Boolean): Integer;
begin
  try
    if (ExtractFileName(Src.ArchiveName) = '') and (Src.Stream = nil) then
      raise EZipMaster.CreateMsg(Body, ZE_NoInFile, {_LINE_}586, __UNIT__);
    if ExtractFileName(Dest.ArchiveName) = '' then
      raise EZipMaster.CreateMsg(Body, ZE_NoOutFile, {_LINE_}588, __UNIT__);
    Result := Src.OpenZip(False, False);
    if Result < 0 then
      raise EZipMaster.CreateMsg(Body, Result, 0, 0);
    Dest.StampDate := Src.StampDate;
    if UseXProgress then
      ShowProgress := ZspExtra
    else
      ShowProgress := ZspFull;
    Dest.TotalDisks := 0;
    Dest.PrepareWrite(ZwMultiple);
    Dest.File_Size := Src.File_Size; // to calc TotalDisks
    Result := Dest.WriteFile(Src, True);
    Dest.File_Close;
    Src.File_Close;
    if Result < 0 then
      raise EZipMaster.CreateMsg(Body, Result, {_LINE_}604, __UNIT__);
  except
    on E: EZMAbort do
      raise;
    on Ews: EZipMaster do // All WriteSpan specific errors.
    begin
      ShowExceptionError(Ews);
      Result := Ews.ExtErr;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      Result := ZM_Error({_LINE_}616, ZE_ErrorUnknown);
      ShowMessage(Result, E.Message);
    end;
  end;
end;

function TZMFileOpr.WriteSpan(const InFileName, OutFileName: string;
  UseXProgress: Boolean): Integer;
var
  Err: Integer;
  InArcName: string;
  OutArcName: string;
  Fd: TZMZipCopier;
  Fs: TZMZipReader;
begin
  Result := DriveFolders.ExpandPath(InArcName, InFileName);
  if Result >= 0 then
    Result := DriveFolders.ExpandPath(OutArcName, OutFileName);
  if Result >= 0 then
  begin
    Result := -1;
    Fd := nil;
    Fs := TZMZipReader.Create(Lister);
    try
      Fs.ArchiveName := InArcName; // will load it
      Fd := TZMZipCopier.Create(Lister);
      Fd.ArchiveName := OutArcName;
      Fd.WorkDrive.DriveStr := Fd.ArchiveName; // get drive params
      Err := Fd.RefuseWriteSplit;
      if Err <> 0 then
        raise EZipMaster.CreateMsg(Body, Err, 0, 0);
      Result := WriteMulti(Fs, Fd, UseXProgress);
    finally
      Fs.Free;
      if Fd <> nil then
        Fd.Free;
    end;
  end;
end;

end.
