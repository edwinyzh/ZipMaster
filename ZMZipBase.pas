unit ZMZipBase;

// ZMZipBase.pas - basic in/out for zip files

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
  System.Classes, WinApi.Windows, System.SysUtils, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.Graphics,
{$ELSE}
  Classes, Windows, SysUtils, Controls, Forms, Dialogs, Graphics,
{$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMBody, ZMDrv, ZMFStream;

// file signitures read by OpenEOC
type
  TZipFileSigs = (ZfsNone, ZfsLocal, ZfsMulti, ZfsDOS);

const
  FmCreateTemp = $FFF0;

type
  TByteBuffer = array of Byte;

type
  TZMGuage = class;

  TZMZipBase = class(TZMBase)
  private
    FAlias: string;
    FArchiveName: string;
    FBytesRead: Int64;
    FBytesWritten: Int64;
    FDiskNr: Integer;
    FEncodeAs: TZMEncodingOpts;
    FFileAttrs: Cardinal;
    FFile_Size: Int64;
    FGuage: TZMGuage;
    FInfo: Cardinal;
    FIsTemp: Boolean;
    FMyStream: TZMFileStream;
    FNumbering: TZipNumberScheme;
    FOpenMode: Integer;
    FReqFileName: string;
    FSavedFileInfo: _BY_HANDLE_FILE_INFORMATION;
    FStampDate: Cardinal;
    FStream: TStream;
    FStreamTime: TFileTime;
    FTotalDisks: Integer;
    FWorkDrive: TZMWorkDrive;
    function DoWrite(const Buffer; Len: Integer): Integer;
    function GetFileInformation(var FileInfo
      : _BY_HANDLE_FILE_INFORMATION): Boolean;
    function GetIsExtStream: Boolean;
    function GetIsOpen: Boolean;
    function GetLastWritten: Cardinal;
    function GetPosition: Int64;
    function GetRealFileName: string;
    procedure SetIsTemp(const Value: Boolean);
    function VerifyFileInformation(IgnoreWriteTime: Boolean = False): Boolean;
  protected
    procedure ClearFileInformation;
    function IsEndOfFile: Boolean;
    function SaveFileInformation: Boolean;
    procedure SetArchiveName(const Value: string);
    procedure SetPosition(const Value: Int64); virtual;
    procedure SetStream(const Value: TStream); virtual;
    property MyStream: TZMFileStream read FMyStream;
    property OpenMode: Integer read FOpenMode write FOpenMode;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CheckRead(var Buffer; Len: Integer): Boolean; overload;
    procedure CheckRead(var Buffer; Len, ErrId: Integer); overload;
    function CheckSeek(Offset: Int64; From: TSeekOrigin; ErrId: Integer): Int64;
    function CheckWrite(const Buffer; Len: Integer): Boolean; overload;
    procedure CheckWrite(const Buffer; Len, ErrId: Integer); overload;
    function CopyFrom(Source: TZMZipBase; Len: Int64): Int64;
    function FileDate: Cardinal;
    procedure File_Close;
    function File_Create(const TheName: string): Boolean; virtual;
    function File_CreateTemp(const Prefix, Where: string): Boolean;
    function File_Open(const FileName: string; Mode: Cardinal)
      : Boolean; virtual;
    function File_Rename(const NewName: string; const Safe: Boolean = False):
        Boolean;
    function File_Reopen(ReqMode: Cardinal): Integer;
    function File_SetAttrs(Attrs: Cardinal): Integer;
    function File_SetDate(DOSdate: Cardinal): Boolean;
    function File_SetTime(CreateTime, AccessTime, WriteTime: PFileTime)
      : Boolean;
    function FinaliseWrite: Integer;
    function FixFileAttrs: Integer;
    function FixFileDate: Integer;
    function LastWriteTime(var Last_write: TFileTime): Boolean;
    function Name(Expanded: Boolean = False): string; virtual;
    function Read(var Buffer; Len: Integer): Integer;
    function ReadTo(Strm: TStream; Count: Integer): Integer;
    function ReleaseStream: TStream;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
    function SetEndOfFile: Boolean;
    function Write(const Buffer; Len: Integer): Integer;
// TODO: WriteContiguous
//  function WriteContiguous(const Buffer; Len: Integer; MustFit: Boolean):
//      Integer; virtual;
//    function WriteContiguous(const Buffer; Len: Integer): Integer;
    function WriteFrom(Strm: TStream; Count: Integer): Int64;
    property Alias: string read FAlias write FAlias;
    property ArchiveName: string read FArchiveName write SetArchiveName;
    property BytesRead: Int64 read FBytesRead write FBytesRead;
    property BytesWritten: Int64 read FBytesWritten write FBytesWritten;
    property DiskNr: Integer read FDiskNr write FDiskNr;
    property EncodeAs: TZMEncodingOpts read FEncodeAs write FEncodeAs;
    property FileAttrs: Cardinal read FFileAttrs write FFileAttrs;
    property File_Size: Int64 read FFile_Size write FFile_Size;
    property Guage: TZMGuage read FGuage;
    property Info: Cardinal read FInfo write FInfo;
    property IsExtStream: Boolean read GetIsExtStream;
    property IsOpen: Boolean read GetIsOpen;
    property IsTemp: Boolean read FIsTemp write SetIsTemp;
    property LastWritten: Cardinal read GetLastWritten;
    property Numbering: TZipNumberScheme read FNumbering write FNumbering;
    property Position: Int64 read GetPosition write SetPosition;
    property RealFileName: string read GetRealFileName;
    property ReqFileName: string read FReqFileName write FReqFileName;
    property StampDate: Cardinal read FStampDate write FStampDate;
    property Stream: TStream read FStream write SetStream;
    property TotalDisks: Integer read FTotalDisks write FTotalDisks;
    property WorkDrive: TZMWorkDrive read FWorkDrive;
  end;

  TZMGuage = class
  private
    FBody: TZMBody;
    FProgress: TZMProgress;
    function GetShowProgress: TZipShowProgress;
    procedure SetBody(const Value: TZMBody);
  public
    procedure Advance(Adv: Int64);
    procedure AdvanceXtra(Adv: Cardinal);
    procedure Clear;
    procedure EndBatch;
    procedure EndItem;
    procedure MoreWritten(More: Int64);
    procedure NewItem(ResID: Integer; FSize: Int64); overload;
    procedure NewItem(const FName: string; FSize: Int64); overload;
    procedure NewXtraItem(Xtra: TZXtraProgress; XSize: Integer); overload;
    procedure SetCount(const Value: Int64);
    procedure SetSize(const Value: Int64);
    procedure Tick;
    procedure Written(Bytes: Int64);
    property Body: TZMBody read FBody write SetBody;
    property Progress: TZMProgress read FProgress;
    property ShowProgress: TZipShowProgress read GetShowProgress;
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
  Zfi_Loaded: Cardinal = $1000; // central loaded
  Zfi_DidLoad: Cardinal = $2000; // central loaded
  Zfi_Invalid: Cardinal = $8000; // needs reload

implementation

uses
  ZMMsg, ZMWinFuncs, ZMUtils, ZMCore, ZMXcpt;

const
  __UNIT__ = 42;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

procedure TZMZipBase.AfterConstruction;
begin
  inherited;
  FBytesWritten := 0;
  FBytesRead := 0;
  FOpenMode := 0;
  FInfo := 0;
  FMyStream := nil;
  FStream := nil;
  FNumbering := ZnsNone;
  FGuage := TZMGuage.Create;
  Guage.Body := Body;
  FEncodeAs := Body.EncodeAs; // default
  FWorkDrive := TZMWorkDrive.Create(Body);
  Stream := TZMSingleFileStream.Create(Body);
  ClearFileInformation;
end;

procedure TZMZipBase.BeforeDestruction;
var
  TmpStream: TStream;
begin
  if MyStream <> nil then
  begin
    TmpStream := MyStream;
    FMyStream := nil;
    FStream := nil;
    TmpStream.Free;
  end;
  FGuage.Free;
  FreeAndNil(FWorkDrive);
  inherited;
end;

function TZMZipBase.CheckRead(var Buffer; Len: Integer): Boolean;
begin
  Assert(Len >= 0, 'Read len < 0');
  Result := Read(Buffer, Len) = Len;
end;

procedure TZMZipBase.CheckRead(var Buffer; Len, ErrId: Integer);
begin
  Assert(Len >= 0, 'Read len < 0');
  if not CheckRead(Buffer, Len) then
  begin
    if ErrId = 0 then
      ErrId := ZM_Error({_LINE_}278, ZE_ReadError);
    raise EZipMaster.CreateMsg(Body, ErrId, 0, 0);
  end;
end;

function TZMZipBase.CheckSeek(Offset: Int64; From: TSeekOrigin;
  ErrId: Integer): Int64;
begin
  Result := Seek(Offset, From);
  if Result < 0 then
  begin
    if ErrId = 0 then
      raise EZipMaster.CreateMsg(Body, ZE_SeekError, {_LINE_}290, __UNIT__);
    if ErrId = -1 then
      ErrId := ZM_Error({_LINE_}292, ZE_FailedSeek);
    raise EZipMaster.CreateMsg(Body, ErrId, 0, 0);
  end;
end;

function TZMZipBase.CheckWrite(const Buffer; Len: Integer): Boolean;
begin
  Assert(Len >= 0, 'Read len < 0');
  Result := Write(Buffer, Len) = Len;
end;

procedure TZMZipBase.CheckWrite(const Buffer; Len, ErrId: Integer);
begin
  if not CheckWrite(Buffer, Len) then
  begin
    if ErrId = 0 then
      ErrId := ZM_Error({_LINE_}308, ZE_WriteError);
    raise EZipMaster.CreateMsg(Body, ErrId, 0, 0);
  end;
end;

procedure TZMZipBase.ClearFileInformation;
begin
  ZeroMemory(@FSavedFileInfo, Sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

function TZMZipBase.CopyFrom(Source: TZMZipBase; Len: Int64): Int64;
const
  BufSize = 10 * 1024; // constant is somewhere
var
  Buffer: array of Byte;
  SizeR: Integer;
  ToRead: Integer;
  Wb: PByte;
begin
  SetLength(Buffer, BufSize);
  Wb := PByte(Buffer);
  Result := 0;

  while Len > 0 do
  begin
    ToRead := BufSize;
    if Len < BufSize then
      ToRead := Len;
    SizeR := Source.Read(Wb^, ToRead);
    if SizeR <> ToRead then
    begin
      if SizeR < 0 then
        Result := SizeR
      else
        Result := ZM_Error({_LINE_}342, ZE_ReadError);
      Exit;
    end;
    if SizeR > 0 then
    begin
      ToRead := Write(Wb^, SizeR);
      if SizeR <> ToRead then
      begin
        if ToRead < 0 then
          Result := ToRead
        else
          Result := ZM_Error({_LINE_}353, ZE_WriteError);
        Exit;
      end;
      Len := Len - SizeR;
      Result := Result + SizeR;
      Guage.Advance(SizeR);
    end;
  end;
end;

function TZMZipBase.DoWrite(const Buffer; Len: Integer): Integer;
begin
  Result := -1;
  Assert(Len >= 0, 'Write len < 0');
  if Stream <> nil then
    Result := Stream.Write(Buffer, Len);
  if Result = Len then
    BytesWritten := BytesWritten + Len
  else
    if IsExtStream and (Result < 0) and (AbsErr(Result) <> ZS_Canceled) then
      Result := ZM_Error({_LINE_}373, ZE_WriteError);
end;

function TZMZipBase.FileDate: Cardinal;
begin
  Result := Cardinal(-1);
  if MyStream <> nil then
    Result := MyStream.File_GetDate;
end;

procedure TZMZipBase.File_Close;
begin
  if MyStream <> nil then
    MyStream.File_Close;
end;

function TZMZipBase.File_Create(const TheName: string): Boolean;
begin
  Result := False;
  if TheName = '' then
    Exit;
  ArchiveName := TheName;
  Body.TraceFmt('Creating %s', [TheName], {_LINE_}396, __UNIT__);
  FBytesWritten := 0;
  FBytesRead := 0;
  Result := False;
  if TheName <> '' then
  begin
    if MyStream = nil then
      Stream := TZMSingleFileStream.Create(Body);
    MyStream.File_Create(TheName);
    // only create single stream
    Result := MyStream.IsOpen;
    FOpenMode := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FmOpenReadWrite;
  end;
end;

function TZMZipBase.File_CreateTemp(const Prefix, Where: string): Boolean;
var
  Len: DWORD;
  TmpDir: string;
begin
  if MyStream <> nil then
    Stream := nil; // I made it
  TmpDir := '';
  if Length(Where) <> 0 then
  begin
    TmpDir := ExtractFilePath(Where);
    if (Length(TmpDir) > 1) and ((TmpDir[1] = '\') and (TmpDir[2] = '\') or
      (TmpDir[1] = '/') and (TmpDir[2] = '/')) then
      TmpDir := '' // use system instead of lan
    else
      TmpDir := ExpandFileName(TmpDir);
  end;
  if TmpDir = '' then
  begin
    if Length(TempDir) = 0 then
    begin
      // 1. The path specified by the TMP environment variable.
      // 2. The path specified by the TEMP environment variable, if TMP is not defined.
      // 3. The current directory, if both TMP and TEMP are not defined.
      Len := GetTempPath(0, PChar(TmpDir));
      SetLength(TmpDir, Len);
      GetTempPath(Len, PChar(TmpDir));
      TmpDir := string(PChar(TmpDir));
    end
    else // Use Temp dir provided by ZipMaster
      TmpDir := TempDir;
  end;
  TmpDir := DelimitPath(TmpDir, True);
  File_Create(FormTempName(TmpDir));
  IsTemp := True;
  Result := IsOpen;
  if MyStream <> nil then
  begin
    if Result then
      MyStream.IsTemp := True
    else
      Body.InformSysFmt('Creating temporary name failed %s', [RealFileName],
        {_LINE_}457, __UNIT__);
  end;
end;

function TZMZipBase.File_Open(const FileName: string; Mode: Cardinal): Boolean;
// const
// AccessMode: array[0..2] of LongWord = (
// GENERIC_READ,
// GENERIC_WRITE,
// GENERIC_READ or GENERIC_WRITE);
// ShareMode: array[0..4] of LongWord = (
// 0,
// 0,
// FILE_SHARE_READ,
// FILE_SHARE_WRITE,
// FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  Fn: string;
begin
  if IsExtStream then
  begin
    // assume stream can handle mode
    FOpenMode := Mode;
    Result := True;
    Exit;
  end;
  Fn := FileName;
  if Fn = '' then
    Fn := ArchiveName;
  Body.TraceFmt('Opening %s', [Fn], {_LINE_}486, __UNIT__);
  MyStream.File_Open(Fn, Mode);
  Result := MyStream.IsOpen;
  FOpenMode := Mode;
end;

function TZMZipBase.File_Rename(const NewName: string; const Safe: Boolean =
    False): Boolean;
begin
  if IsExtStream then
  begin
    // assume it could happen
    Result := True;
    Exit;
  end;
  Body.TraceFmt('Rename %s to %s', [Name(True), NewName], {_LINE_}504,
    __UNIT__);
  IsTemp := False;
  if MyStream <> nil then
    MyStream.File_Close; // close it
  if _Z_FileExists(RealFileName) then
  begin
    if _Z_FileExists(NewName) then
    begin
      Body.TraceFmt('Erasing %s', [NewName], {_LINE_}513, __UNIT__);
      if (_Z_EraseFile(NewName, not Safe) <> 0) then
        Body.InformSysFmt('Erase failed %s', [NewName], {_LINE_}515, __UNIT__);
    end;
  end;
  Result := _Z_RenameFile(RealFileName, NewName);
end;

// return <0 _error >= 0 _ open
function TZMZipBase.File_Reopen(ReqMode: Cardinal): Integer;
var
  OldMode: Cardinal;
  WasWriting: Boolean;
begin
  Result := 0;
  if IsExtStream then
  begin
    // just rewind
    Stream.Position := 0;
    Exit;
  end;
  if MyStream <> nil then
    OldMode := MyStream.OpenMode
  else
    OldMode := OpenMode;
  if (MyStream = nil) or (OldMode <> ReqMode) or not MyStream.IsOpen then
  begin
    Body.TraceFmt('Reopening: %s', [RealFileName], {_LINE_}540, __UNIT__);
    if not File_Open(RealFileName, ReqMode) then
    begin
      Body.InformSysFmt('Could not File_Reopen: %s', [RealFileName], {_LINE}580,
        __UNIT__);
      Result := Body.PrepareErrMsg(ZE_FileOpen, [RealFileName],
        {_LINE_}546, __UNIT__);
    end;
  end;
  if Result >= 0 then
  begin
    WasWriting := (BytesWritten > 0) and
      ((OldMode and ({$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.
      FmOpenReadWrite or
{$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FmOpenWrite)) <> 0);
    if ((Info and Zfi_Loaded) <> 0) and not VerifyFileInformation(WasWriting)
    then
    begin
      Body.InformFmt('File has changed! %s', [RealFileName], {_LINE_}558,
        __UNIT__);
      // close it?
      Result := Body.PrepareErrMsg(ZE_FileChanged, [RealFileName],
        {_LINE_}562, __UNIT__);
    end;
  end;
  if Result >= 0 then
    Result := Seek(0, SoBeginning);
end;

function TZMZipBase.File_SetAttrs(Attrs: Cardinal): Integer;
begin
  File_Close;
  Result := 0;
  if not _Z_SetFileAttributes(RealFileName, Attrs and $7F) then
  begin
    Body.InformSysFmt('Failed to set file attributes: %s', [RealFileName],
      {_LINE_}576, __UNIT__);
    Result := Body.PrepareErrMsg(ZE_SetFileAttributes, [RealFileName],
      {_LINE_}578, __UNIT__);
  end;
end;

function TZMZipBase.File_SetDate(DOSdate: Cardinal): Boolean;
var
  FileTime: TFileTime;
  LocalFileTime: TFileTime;
begin
  Result := DOSdate = 0;
  if MyStream <> nil then
  begin
    if not Result then
    begin
      Result := False;
      if MyStream <> nil then
      begin
        Result := DosDateTimeToFileTime(LongRec(DOSdate).Hi,
          LongRec(DOSdate).Lo, LocalFileTime) and
          LocalFileTimeToFileTime(LocalFileTime, FileTime) and
          File_SetTime(nil, nil, @FileTime);
      end;
    end;
  end;
end;

// return true on success  _ does not adjust
function TZMZipBase.File_SetTime(CreateTime, AccessTime,
  WriteTime: PFileTime): Boolean;
var
  FT: PFileTime;
  WriteHandle: THandle;
begin
  Result := False;
  FT := nil;
  if WriteTime <> nil then
    FT := WriteTime
  else
    if AccessTime <> nil then
      FT := AccessTime
    else
      if CreateTime <> nil then
        FT := CreateTime;
  if FT <> nil then
  begin
    if MyStream <> nil then
    begin
      File_Close;
      WriteHandle := _Z_FileOpen(RealFileName, FmOpenWrite);
      if WriteHandle <> INVALID_HANDLE_VALUE then
      begin
        try
          if SetFileTime(WriteHandle, CreateTime, AccessTime, WriteTime) then
          begin
            Result := True;
            if IsTrace then
              Body.TraceFmt('Set file Date: %s to %s',
                [RealFileName, DateTimeToStr(FileTimeToLocalDateTime(FT^))],
                {_LINE_} 636, __UNIT__);
          end
          else
          begin
            // failed
            Body.InformSysFmt('Warning: Set file Date: %s to %s failed',
              [RealFileName, DateTimeToStr(FileTimeToLocalDateTime(FT^))],
              {_LINE_} 643, __UNIT__);
          end;
        finally
          FileClose(WriteHandle);
        end;
      end
      else
        Body.InformSysFmt('Failed to open: %s', [RealFileName], {_LINE_} 650,
          __UNIT__);
    end;
  end;
end;

function TZMZipBase.FinaliseWrite: Integer;
var
  Err: Integer;
begin
  File_Close;
  Result := 0;
  Err := 0;
  if StampDate <> 0 then
    Result := FixFileDate;
  if FileAttrs <> 0 then
    Err := FixFileAttrs;
  if Result = 0 then
    Result := Err
  else
    if Err <> 0 then
      Result := Body.PrepareErrMsg(ZE_SetFileInformation, [RealFileName],
        {_LINE_} 672, __UNIT__);
end;

function TZMZipBase.FixFileAttrs: Integer;
begin
  // set attributes
  File_Close;
  Result := File_SetAttrs(FileAttrs);
end;

function TZMZipBase.FixFileDate: Integer;
begin
  Result := 0;
  if MyStream <> nil then
  begin
    if not File_SetDate(StampDate) then
      Result := Body.PrepareErrMsg(ZE_SetFileTimes, [RealFileName],
        {_LINE_} 689, __UNIT__); // failed
  end;
end;

function TZMZipBase.GetFileInformation(var FileInfo
  : _BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  Result := False;
  if MyStream <> nil then
    Result := MyStream.GetFileInformation(FileInfo);
  if not Result then
    ZeroMemory(@FileInfo, Sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

function TZMZipBase.GetIsExtStream: Boolean;
begin
  Result := (Stream <> nil) and (MyStream = nil);
end;

function TZMZipBase.GetIsOpen: Boolean;
begin
  if MyStream <> nil then
    Result := MyStream.IsOpen
  else
    Result := (Stream <> nil);
end;

function TZMZipBase.GetLastWritten: Cardinal;
var
  Ft: TFileTime;
begin
  Result := 0;
  if (MyStream <> nil) and MyStream.LastWriteTime(Ft) then
    Result := FileTimeToLocalDOSTime(Ft);
end;

function TZMZipBase.GetPosition: Int64;
begin
  Result := -1;
  if Stream <> nil then
    Result := Stream.Position;
end;

function TZMZipBase.GetRealFileName: string;
begin
  Result := '';
  if MyStream <> nil then
    Result := MyStream.RealFileName
  else
    if Stream <> nil then
      Result := '<stream>';
end;

// return true if end of segment
function TZMZipBase.IsEndOfFile: Boolean;
begin
  Result := True;
  if MyStream <> nil then
    Result := MyStream.IsEndOfFile
  else
    if Stream <> nil then
      Result := Stream.Position >= Stream.Size;
end;

function TZMZipBase.LastWriteTime(var Last_write: TFileTime): Boolean;
begin
  Result := False;
  Last_write.DwLowDateTime := 0;
  Last_write.DwHighDateTime := 0;
  if MyStream <> nil then
    Result := MyStream.LastWriteTime(Last_write)
  else
    if Stream <> nil then
    begin
      Last_write := FStreamTime; // time it was attached
      Result := True;
    end;
end;

function TZMZipBase.Name(Expanded: Boolean = False): string;
begin
  if Alias <> '' then
  begin
    Result := Alias;
    if Expanded then
      Result := Result + '<' + RealFileName + '>';
  end // ;
  else
    Result := RealFileName;
end;

function TZMZipBase.Read(var Buffer; Len: Integer): Integer;
begin
  Assert(Len >= 0, 'Read len < 0');
  if Stream <> nil then
  begin
    Result := Stream.Read(Buffer, Len);
    if Result = Len then
      BytesRead := BytesRead + Len;
  end
  else
    Result := ZM_Error({_LINE_}790, ZE_ReadError);
end;

function TZMZipBase.ReadTo(Strm: TStream; Count: Integer): Integer;
const
  Bsize = 20 * 1024;
var
  Done: Integer;
  Sz: Integer;
  Wbufr: array of Byte;
  Wrote: Integer;
begin
  Result := 0;
  SetLength(Wbufr, Bsize);
  while Count > 0 do
  begin
    Sz := Bsize;
    if Sz > Count then
      Sz := Count;
    Done := Read(Wbufr[0], Sz);
    if Done <= 0 then
    begin
      if Done = 0 then
        Done := ZM_Error({_LINE_}813, ZE_ReadError);
      Result := Done;
      Break;
    end;
    if Done > 0 then
    begin
      Wrote := Strm.Write(Wbufr[0], Done);
      if Wrote <> Done then
      begin
        if Wrote >= 0 then
          Wrote := ZM_Error({_LINE_}823, ZE_WriteError);
        Result := Wrote;
        Break;
      end;
    end;
    Count := Count - Sz;
    Result := Result + Sz;
  end;
end;

function TZMZipBase.ReleaseStream: TStream;
begin
  Result := Stream;
  FStream := nil; // just remove references
  FMyStream := nil;
end;

function TZMZipBase.SaveFileInformation: Boolean;
begin
  Result := False;
  if MyStream <> nil then
    Result := MyStream.GetFileInformation(FSavedFileInfo);
end;

function TZMZipBase.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := -1;
  if Stream <> nil then
{$IFDEF VERD7up}
    Result := Stream.Seek(Offset, Origin);
{$ELSE}
    begin
      if (Offset < low(Longint)) or (Offset > high(Longint)) then
        raise ERangeError.Create('>2G not supported');
      Result := Stream.Seek(Longint(Offset), Ord(Origin));
    end;
{$ENDIF}
  if (Result < 0) and IsExtStream then
    Result := ZM_Error({_LINE_}861, ZE_SeekError);
end;

procedure TZMZipBase.SetArchiveName(const Value: string);
begin
  if FArchiveName <> Value then
  begin
    FArchiveName := Value;
    WorkDrive.DriveStr := Value;
  end;
end;

function TZMZipBase.SetEndOfFile: Boolean;
begin
  Result := False;
  if MyStream <> nil then
    Result := MyStream.SetEndOfFile
  else
    if Stream <> nil then
    begin
      Stream.Size := Stream.Position;
      Result := Stream.Size = Stream.Position;
    end;
end;

procedure TZMZipBase.SetIsTemp(const Value: Boolean);
begin
  FIsTemp := Value;
  if MyStream <> nil then
    MyStream.IsTemp := Value;
end;

procedure TZMZipBase.SetPosition(const Value: Int64);
begin
  if Stream <> nil then
{$IFDEF VERD7up}
    Stream.Seek(Value, SoBeginning);
{$ELSE}
    begin
      if (Value < low(Longint)) or (Value > high(Longint)) then
        raise ERangeError.Create('>2G not supported');
      Stream.Seek(Longint(Value), Ord(SoBeginning));
    end;
{$ENDIF}
end;

procedure TZMZipBase.SetStream(const Value: TStream);
var
  St: TSystemTime;
begin
  if FStream <> Value then
  begin
    if FMyStream <> nil then
      FreeAndNil(FMyStream); // delete old
    FStream := Value;
    if Value <> nil then
    begin
      if Value is TZMFileStream then
      begin
        FMyStream := TZMFileStream(Value);
      end
      else
      begin
        GetLocalTime(St);
        SystemTimeToFileTime(St, FStreamTime);
      end;
    end;
  end;
end;

// return true unchanged
function TZMZipBase.VerifyFileInformation(IgnoreWriteTime
  : Boolean = False): Boolean;
var
  Info: _BY_HANDLE_FILE_INFORMATION;
begin
  GetFileInformation(Info);
  if IgnoreWriteTime then
    Result := True
  else
    Result := (Info.FtLastWriteTime.DwLowDateTime = FSavedFileInfo.
      FtLastWriteTime.DwLowDateTime) and
      (Info.FtLastWriteTime.DwHighDateTime = FSavedFileInfo.FtLastWriteTime.
      DwHighDateTime);
  if Result then
    Result := (Info.FtCreationTime.DwLowDateTime = FSavedFileInfo.
      FtCreationTime.DwLowDateTime) and
      (Info.FtCreationTime.DwHighDateTime = FSavedFileInfo.FtCreationTime.
      DwHighDateTime) and (Info.NFileSizeLow = FSavedFileInfo.NFileSizeLow) and
      (Info.NFileSizeHigh = FSavedFileInfo.NFileSizeHigh) and
      (Info.DwFileAttributes = FSavedFileInfo.DwFileAttributes) and
      (Info.DwVolumeSerialNumber = FSavedFileInfo.DwVolumeSerialNumber);
  // FileIndex not reliable via network
  if Result and (WorkDrive.DriveType <> DRIVE_REMOTE) then
    Result := (Info.NFileIndexLow = FSavedFileInfo.NFileIndexLow) and
      (Info.NFileIndexHigh = FSavedFileInfo.NFileIndexHigh);
end;

function TZMZipBase.Write(const Buffer; Len: Integer): Integer;
begin
  Result := DoWrite(Buffer, Len);
end;

// TODO: WriteContiguous
//function TZMZipBase.WriteContiguous(const Buffer; Len: Integer; MustFit:
//  Boolean): Integer;
//begin
//Result := DoWrite(Buffer, Len);
//end;

function TZMZipBase.WriteFrom(Strm: TStream; Count: Integer): Int64;
const
  Bsize = 20 * 1024;
var
  Done: Integer;
  Maxsize: Integer;
  Sz: Integer;
  Wbufr: array of Byte;
  Wrote: Integer;
begin
  Result := 0;
  SetLength(Wbufr, Bsize);
  Maxsize := Strm.Size - Strm.Position;
  if Count > Maxsize then
    Count := Maxsize;
  while Count > 0 do
  begin
    Sz := Bsize;
    if Sz > Count then
      Sz := Count;
    Done := Strm.Read(Wbufr[0], Sz);
    if Done < 0 then
    begin
      Result := Done;
      Break;
    end;
    if Done > 0 then
    begin
      Wrote := Write(Wbufr[0], Done);
      if Wrote <> Done then
      begin
        if Wrote >= 0 then
          Wrote := ZM_Error({_LINE_}1001, ZE_WriteError);
        Result := Wrote;
        Break;
      end;
    end;
    Count := Count - Sz;
    Result := Result + Sz;
  end;
end;

{ TZMGuage }
procedure TZMGuage.Advance(Adv: Int64);
begin
  case ShowProgress of
    ZspNone:
      Tick;
    ZspExtra:
      Progress.AdvanceXtra(Adv);
    ZspFull:
      Progress.Advance(Adv);
  end;
end;

procedure TZMGuage.AdvanceXtra(Adv: Cardinal);
begin
  if ShowProgress = ZspFull then
    Progress.AdvanceXtra(Adv)
  else
    Tick;
end;

procedure TZMGuage.Clear;
begin
  Progress.Clear;
end;

procedure TZMGuage.EndBatch;
begin
  if ShowProgress = ZspFull then
    Progress.EndBatch
  else
    Tick;
end;

procedure TZMGuage.EndItem;
begin
  if ShowProgress = ZspFull then
    Progress.EndItem
  else
    Tick;
end;

function TZMGuage.GetShowProgress: TZipShowProgress;
begin
  Result := Body.ShowProgress;
end;

procedure TZMGuage.MoreWritten(More: Int64);
begin
  if ShowProgress = ZspFull then
    Progress.MoreWritten(More);
end;

procedure TZMGuage.NewItem(ResID: Integer; FSize: Int64);
begin
  NewItem(Body.ZipLoadStr(ResID), FSize);
end;

procedure TZMGuage.NewItem(const FName: string; FSize: Int64);
begin
  case ShowProgress of
    ZspNone:
      Tick;
    ZspExtra:
      Progress.NewXtraItem(FName, FSize);
    ZspFull:
      Progress.NewItem(FName, FSize);
  end;
end;

procedure TZMGuage.NewXtraItem(Xtra: TZXtraProgress; XSize: Integer);
begin
  if ShowProgress = ZspFull then
    Progress.NewXtraItem(Xtra, XSize)
  else
    Tick;
end;

procedure TZMGuage.SetBody(const Value: TZMBody);
begin
  FBody := Value;
  FProgress := FBody.Progress;
end;

procedure TZMGuage.SetCount(const Value: Int64);
begin
  if ShowProgress = ZspFull then
    Progress.TotalCount := Value;
end;

procedure TZMGuage.SetSize(const Value: Int64);
begin
  if ShowProgress = ZspFull then
    Progress.TotalSize := Value
  else
    Tick;
end;

procedure TZMGuage.Tick;
begin
  Body.KeepAlive;
end;

procedure TZMGuage.Written(Bytes: Int64);
begin
  Progress.Written(Bytes);
end;

end.
