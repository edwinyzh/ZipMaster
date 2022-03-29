unit ZMFStream;

// ZMFStream.pas - basic in/out for zip files

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
// modified 2013-12-16

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils,
{$ENDIF}
  ZMBody;

type
  TZMFileStream = class(TStream)
  private
    FBody: TZMBody;
  protected
    function GetIsOpen: Boolean; virtual; abstract;
    function GetLastWritten: Cardinal; virtual; abstract;
    function GetPosition: Int64; virtual; abstract;
    procedure SetPosition(const Value: Int64); virtual; abstract;
    function GetRealFileName: string; virtual; abstract;
    function GetIsTemp: Boolean; virtual; abstract;
    procedure SetIsTemp(const Value: Boolean); virtual; abstract;
    function GetOpenMode: Integer; virtual; abstract;
    procedure SetOpenMode(const Value: integer); virtual; abstract;
  public
    constructor Create(MyOwner: TZMBody);
    function FileDate: Cardinal; virtual; abstract;
    procedure File_Close; virtual; abstract;
    procedure File_Create(const TheName: string); virtual; abstract;
    function File_GetDate: Cardinal; virtual; abstract;
    procedure File_Open(const FileName: string; Mode: Word); virtual; abstract;
    function GetFileInformation(var FileInfo
      : _BY_HANDLE_FILE_INFORMATION): Boolean;  virtual; abstract;
    function IsEndOfFile: Boolean;  virtual; abstract;
    function LastWriteTime(var Last_write: TFileTime): Boolean;  virtual; abstract;
{$IFNDEF VERD7up}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload;
       virtual; abstract;
{$ENDIF}
    function SetEndOfFile: Boolean; virtual; abstract;
    property Body: TZMBody read FBody;
    property LastWritten: Cardinal read GetLastWritten;
    property Position: Int64 read GetPosition write SetPosition;
    property RealFileName: string read GetRealFileName;
    property IsOpen: Boolean read GetIsOpen;
    property IsTemp: Boolean read GetIsTemp write SetIsTemp;
    property OpenMode: Integer read GetOpenMode write SetOpenMode;
  end;

type
  TZMSingleFileStream = class(TZMFileStream)
  private
    FHandle: Integer;
    FIsTemp: Boolean;
    FOpenMode: Integer;
    FRealFileName: string;
  protected
    function GetIsOpen: Boolean;  override;
    function GetLastWritten: Cardinal; override;
    function GetPosition: Int64; override;
    procedure SetPosition(const Value: Int64); override;
    function GetRealFileName: string;  override;
    function GetIsTemp: Boolean;  override;
    procedure SetIsTemp(const Value: Boolean);  override;
    function GetOpenMode: Integer;  override;
    procedure SetOpenMode(const Value: integer);  override;
    property Handle: Integer read FHandle;
  public
    constructor Create(MyOwner: TZMBody; const FileName: string; Mode: Word);
        overload;
    constructor Create(MyOwner: TZMBody); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function FileDate: Cardinal; override;
    procedure File_Close; override;
    procedure File_Create(const TheName: string); override;
    function File_GetDate: Cardinal; override;
    procedure File_Open(const FileName: string; Mode: Word); override;
    function GetFileInformation(var FileInfo
      : _BY_HANDLE_FILE_INFORMATION): Boolean; override;
    function IsEndOfFile: Boolean; override;
    function LastWriteTime(var Last_write: TFileTime): Boolean; override;
    function Read(var Buffer; Len: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function SetEndOfFile: Boolean; override;
    function Write(const Buffer; Len: Integer): Integer; override;
  end;

implementation

uses
  ZMCore, ZMMsg, ZMWinFuncs, ZMUtils;

const
  __UNIT__ = 18;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{ TZMSingleFileStream }
constructor TZMSingleFileStream.Create(MyOwner: TZMBody; const FileName: string;
  Mode: Word);
begin
  inherited Create(MyOwner);
  FOpenMode := Mode;
  FRealFileName := FileName;
end;

constructor TZMSingleFileStream.Create(MyOwner: TZMBody);
begin
  inherited;
  FOpenMode := -1;
  FRealFileName := '';
end;

procedure TZMSingleFileStream.AfterConstruction;
begin
  inherited;
  FHandle := -1;
  if OpenMode <> -1 then
  begin
    if OpenMode = FmCreate then
      File_Create(FRealFileName)
    else
      File_Open(FRealFileName, OpenMode);
  end;
end;

procedure TZMSingleFileStream.BeforeDestruction;
begin
  File_Close;
  if IsTemp and _Z_FileExists(RealFileName) then
  begin
    Body.TraceFmt('Deleting %s', [RealFileName], {_LINE_}201, __UNIT__);
    File_Delete(RealFileName);
  end;
  inherited;
end;

function TZMSingleFileStream.FileDate: Cardinal;
begin
  Result := Cardinal(-1);
  if FHandle <> -1 then
    Result := FileGetDate(Handle);
end;

procedure TZMSingleFileStream.File_Close;
var
  Th: Integer;
begin
  if FHandle <> -1 then
  begin
    Th := FHandle;
    FHandle := -1;
    FileClose(Th);
    Body.TraceFmt('Closed %s', [RealFileName], {_LINE_}223, __UNIT__);
  end;
end;

procedure TZMSingleFileStream.File_Create(const TheName: string);
begin
  if Handle <> -1 then
    File_Close;
  FRealFileName := TheName;
  if TheName <> '' then
  begin
    Body.TraceFmt('File_Create : %s', [TheName], {_LINE_}234, __UNIT__);
    FHandle := _Z_FileCreate(TheName);
    if FHandle = -1 then
      Body.InformSysFmt('FileCreate Failed: %s', [TheName], {_LINE_}237,
        __UNIT__);
    FOpenMode := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FmOpenReadWrite;
  end;
end;

function TZMSingleFileStream.File_GetDate: Cardinal;
begin
  Result := FileGetDate(Handle);
end;

procedure TZMSingleFileStream.File_Open(const FileName: string; Mode: Word);
begin
  if Handle <> -1 then
    File_Close;
  FRealFileName := FileName;
  FHandle := _Z_FileOpen(RealFileName, Mode);
  if Handle < 0 then
    Body.InformSysFmt('FileOpen Failed: %s', [RealFileName],
      {_LINE_}256, __UNIT__)
  else
    Body.TraceFmt('Opened %s', [RealFileName], {_LINE_}258, __UNIT__);
  FOpenMode := Mode;
end;

function TZMSingleFileStream.GetFileInformation(var FileInfo
  : _BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  Result := False;
  if Handle <> -1 then
    Result := GetFileInformationByHandle(Handle, FileInfo);
  if not Result then
    ZeroMemory(@FileInfo, Sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

function TZMSingleFileStream.GetIsOpen: Boolean;
begin
  Result := (Handle <> -1);
end;

function TZMSingleFileStream.GetIsTemp: Boolean;
begin
  Result := FIsTemp;
end;

function TZMSingleFileStream.GetLastWritten: Cardinal;
var
  Ft: TFileTime;
begin
  Result := 0;
  if (Handle <> -1) and LastWriteTime(Ft) then
    Result := FileTimeToLocalDOSTime(Ft);
end;

function TZMSingleFileStream.GetOpenMode: Integer;
begin
  Result := FOpenMode;
end;

function TZMSingleFileStream.GetPosition: Int64;
begin
  if Handle <> -1 then
    Result := FileSeek(FHandle, 0, SoFromCurrent)
  else
    Result := -1;
end;

function TZMSingleFileStream.GetRealFileName: string;
begin
  Result := FRealFileName;
end;

// return true if end of segment
function TZMSingleFileStream.IsEndOfFile: Boolean;
var
  Cposn: Int64;
begin
  Result := True;
  if Handle <> -1 then
  begin
    Cposn := Position;
    Result := Cposn = FileSeek(Handle, 0, SoFromEnd);
    if not Result then
      Position := Cposn;
  end;
end;

function TZMSingleFileStream.LastWriteTime(var Last_write: TFileTime): Boolean;
var
  BHFInfo: TByHandleFileInformation;
begin
  Result := False;
  Last_write.DwLowDateTime := 0;
  Last_write.DwHighDateTime := 0;
  if Handle <> -1 then
  begin
    Result := GetFileInformationByHandle(Handle, BHFInfo);
    if Result then
      Last_write := BHFInfo.FtLastWriteTime;
  end;
end;

function TZMSingleFileStream.Read(var Buffer; Len: Integer): Integer;
begin
  Assert(Len >= 0, 'TZMSingleFileStream read len < 0');
  Result := -1;
  if Handle <> -1 then
    Result := FileRead(Handle, Buffer, Len);
end;

function TZMSingleFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := -1;
  if Handle <> -1 then
    Result := FileSeek(Handle, Offset, WORD(Origin));
end;

function TZMSingleFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;

function TZMSingleFileStream.SetEndOfFile: Boolean;
begin
  Result := False;
  if Handle <> -1 then
    Result := {$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.SetEndOfFile(Handle);
end;

procedure TZMSingleFileStream.SetIsTemp(const Value: Boolean);
begin
  FIsTemp := Value;
end;

procedure TZMSingleFileStream.SetOpenMode(const Value: integer);
begin
  FOpenMode := Value;
end;

procedure TZMSingleFileStream.SetPosition(const Value: Int64);
begin
  if Handle <> -1 then
    Seek(Value, TSeekOrigin(SoFromBeginning));
end;

function TZMSingleFileStream.Write(const Buffer; Len: Integer): Integer;
begin
  Result := -1;
  Assert(Len >= 0, 'TZMSingleFileStream write Len < 0');
  if Handle <> -1 then
    Result := FileWrite(Handle, Buffer, Len);
end;

{ TZMFileStream }

constructor TZMFileStream.Create(MyOwner: TZMBody);
begin
  inherited Create;
  FBody := MyOwner;
end;

end.
