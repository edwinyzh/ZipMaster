unit ZMWinFuncs;

//  ZMWinFuncs.pas - Functions supporting UTF8/16 file names
                   
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
//Modified 2014-05-19

interface

{$INCLUDE   '.\ZipVers.inc'}

uses
  {$IFDEF VERDXE2up}
    WinApi.Windows, System.SysUtils;
  {$ELSE}
    Windows, SysUtils;
  {$ENDIF}

type
{$IFDEF UNICODE}
  _Z_TSearchRec = TSearchRec;
{$ELSE}
  _Z_TSearchRec = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    name: String;
    ExcludeAttr: Integer;
    FindHandle: THandle {$IFNDEF VERpre6} platform{$ENDIF};
    FindData: TWin32FindDataW {$IFNDEF VERpre6} platform{$ENDIF};
  end;
{$ENDIF}

function _Z_CreateFile(const FileName: String; dwDesiredAccess, dwShareMode:
    DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition,
    dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;
function _Z_DeleteFile(const FileName: String): Boolean;
function _Z_DirExists(const FileName: String): Boolean;

// returns <0 _ file does not exist, 0 _ success, >0 _ error
function _Z_EraseFile(const FileName: String; permanent: Boolean): Integer;
function _Z_FileCreate(const FileName: String): cardinal;
function _Z_FileExists(const FileName: String): Boolean;
function _Z_FileOpen(const FileName: String; Mode: LongWord): Integer;
function _Z_FindFirst(const Path: String; Attr: Integer;
  var F: _Z_TSearchRec): Integer;
function _Z_FindNext(var F: _Z_TSearchRec): Integer;
procedure _Z_FindClose(var F: _Z_TSearchRec);
function _Z_ForceDirectory(const Dir: String): Boolean;
function _Z_GetExeVersion(const FileName: String; var MS, LS: DWORD): Boolean;
function _Z_RemoveDir(const FileName: String): Boolean;
function _Z_CreateDir(const FileName: String): Boolean;
function _Z_RenameFile(const OldName, NewName: String): Boolean;
function _Z_SetFileAttributes(const FileName: string; Attrs: DWORD): Boolean;
function _Z_GetFileAttributes(const FileName: string): DWORD;
procedure _Z_ChangeNotify(Op: Cardinal; const FileName: string);

implementation

uses
  {$IFDEF VERDXE2up}
    Vcl.Forms, WinApi.ShlObj, WinApi.ShellAPI,
  {$ELSE}
    Forms, ShlObj, ShellAPI,
  {$ENDIF}
   ZMStructs, ZMUtils{$IFNDEF UNICODE}, ZMUTF8, ZMHandler{$ENDIF};

type
  TZMWString = {$IFDEF UNICODE}String{$ELSE}WideString{$ENDIF};

{$IFDEF UNICODE}
function PrefixLongPath(const FN: string): string;
begin
  Result := FN;
  if (Length(FN) >= MAX_PATH) and
    ((FN[1] <> '\') or (FN[2] <> '\') or (FN[3] <> '?') or (FN[4] <> '\')) then
    Result := '\\?\' + FN;
//  else
//    Result := FN;
end;
{$ELSE}
function PrefixLongPath(const FN: string): WideString;
begin
  if not UsingUTF8 then
    Result := WideString(FN)
  else
    Result := UTF8ToWide(FN, -1);
  if (Length(Result) >= MAX_PATH) and
    ((Result[1] <> '\') or (Result[2] <> '\') or (Result[3] <> '?') or (Result[4] <> '\')) then
    Result := '\\?\' + Result;
end;
{$ENDIF}


function _Z_CreateDir(const FileName: String): Boolean;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Result := CreateDirectoryW(PWideChar(XPath), nil);
end;

function _Z_CreateFile(const FileName: String; dwDesiredAccess, dwShareMode:
    DWORD; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition,
    dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;
var
  XFileName: TZMWString;
begin
  XFileName := PrefixLongPath(FileName);
  Result := CreateFileW(PWideChar(XFileName), dwDesiredAccess, dwShareMode,
  lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

function _Z_FileCreate(const FileName: String): cardinal;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Result := CreateFileW(PWideChar(XPath), GENERIC_READ or GENERIC_WRITE,
              0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

function _Z_DeleteFile(const FileName: String): Boolean;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Result := DeleteFileW(PWideChar(XPath));
end;

function _Z_DirExists(const FileName: String): Boolean;
var
  Code: DWORD;
  Dir: string;
  XPath: TZMWString;
begin
  Result := True; // current directory exists
  Dir := DelimitPath(FileName, False);
  if Dir <> '' then
  begin
    XPath := PrefixLongPath(Dir);
    Code := GetFileAttributesW(PWideChar(XPath));
    Result := (Code <> MAX_UNSIGNED) and
      ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;
end;

function ExistsLockedOrSharedW(const FileName: WideString): Boolean;
var
  FindData: TWin32FindDataW;
  LHandle: THandle;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  // Either the file is locked/share_exclusive or we got an access denied
  LHandle := FindFirstFileW(PWideChar(XPath), FindData);
  if LHandle <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.FindClose(LHandle);
    Result := FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
  end
  else
    Result := False;
end;

function _Z_FileExists(const FileName: String): Boolean;
var
  Code: Integer;
  LastError: cardinal;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Code := GetFileAttributesW(PWideChar(XPath));
  if Code <> -1 then
    Result := (FILE_ATTRIBUTE_DIRECTORY and Code = 0)
  else
  begin
    LastError := GetLastError;
    Result := (LastError <> ERROR_FILE_NOT_FOUND) and
      (LastError <> ERROR_PATH_NOT_FOUND) and
      (LastError <> ERROR_INVALID_NAME) and ExistsLockedOrSharedW(FileName);
  end;
end;


function _Z_SetFileAttributes(const FileName: string; Attrs: DWORD): Boolean;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Result := SetFileAttributesW(PWideChar(XPath), Attrs);
end;

function _Z_GetFileAttributes(const FileName: string): DWORD;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  Result := GetFileAttributesW(PWideChar(XPath));
end;

procedure _Z_ChangeNotify(Op: Cardinal; const FileName: string);
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(FileName);
  SHChangeNotify(Op, SHCNF_PATHW, PWideChar(XPath), nil);
end;

// returns <0 _ file does not exist, 0 _ success, >0 _ error
function _Z_EraseFile(const FileName: String; permanent: Boolean): Integer;
var
  DelFileName: String;
  SHF: TSHFileOpStructW;
  WName: TZMWString;
begin
  // If we do not have a full path then FOF_ALLOWUNDO does not work!?
  DelFileName := FileName;
  if ExtractFilePath(FileName) = '' then
    DelFileName := GetCurrentDir() + PathDelim + FileName;

  Result := -1;
  // We need to be able to 'Delete' without getting an error
  // if the file does not exists as in ReadSpan() can occur.
  if not _Z_FileExists(DelFileName) then
    Exit;

  WName := PrefixLongPath(DelFileName) + #0 + #0;
  SHF.Wnd := Application.Handle;
  SHF.wFunc := FO_DELETE;
  SHF.pFrom := PWideChar(WName);
  SHF.pTo := nil;
  SHF.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  if not permanent then
    SHF.fFlags := SHF.fFlags or FOF_ALLOWUNDO;

  Result := SHFileOperationW(SHF);
end;

function _Z_FileOpen(const FileName: String; Mode: LongWord): Integer;
const
  AccessMode: array [0 .. 2] of LongWord = (GENERIC_READ, GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array [0 .. 4] of LongWord = (0, 0, FILE_SHARE_READ,
    FILE_SHARE_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  XPath: TZMWString;
begin
  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    begin
      XPath := PrefixLongPath(FileName);
      Result := Integer(CreateFileW(PWideChar(XPath), AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
    end;
end;

function _FindMatchingFileW(var F: _Z_TSearchRec): Integer;
var
  LocalFileTime: TFileTime;
begin
//  with F do
//  begin
    while F.FindData.dwFileAttributes and F.ExcludeAttr <> 0 do
      if not FindNextFileW(F.FindHandle, F.FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(F.FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(F.Time).Hi, LongRec(F.Time).Lo);
    F.Size := F.FindData.nFileSizeLow or Int64(F.FindData.nFileSizeHigh) shl 32;
    F.Attr := F.FindData.dwFileAttributes;
{$IFDEF UNICODE}
    F.Name := F.FindData.cFileName;
{$ELSE}
    if UsingUTF8 then
      F.Name := PWideToUTF8(PWideChar(@F.FindData.cFileName), -1)
    else
      F.Name := PWideToSafe(PWideChar(@F.FindData.cFileName), False);
{$ENDIF}
//  end;
  Result := 0;
end;

function _Z_FindFirst(const Path: String; Attr: Integer;
  var F: _Z_TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faDirectory;
var
  XPath: TZMWString;
begin
  XPath := PrefixLongPath(Path);
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileW(PWideChar(XPath), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := _FindMatchingFileW(F);
    if Result <> 0 then
      _Z_FindClose(F);
  end
  else
    Result := GetLastError;
end;

function _Z_FindNext(var F: _Z_TSearchRec): Integer;
begin
  if FindNextFileW(F.FindHandle, F.FindData) then
    Result := _FindMatchingFileW(F)
  else
    Result := GetLastError;
end;

procedure _Z_FindClose(var F: _Z_TSearchRec);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    {$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function DelimitPathW(const Path: TZMWString; Sep: Boolean): TZMWString;
begin
  Result := Path;
  if Length(Path) = 0 then
  begin
    if Sep then
      Result := '\';
    exit;
  end;
  if (Path[Length(Path)] = '\') <> Sep then
  begin
    if Sep then
      Result := Path + '\'
    else
      Result := Copy(Path, 1, pred(Length(Path)));
  end;
end;

function FindLastW(const ws: TZMWString; const wc: widechar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(ws) - 1 downto 1 do
    if ws[i] = wc then
    begin
      Result := i;
    end;
end;

function FindFirstW(const ws: TZMWString; const wc: widechar): Integer;
begin
  for Result := 1 to Length(ws) - 1 do
    if ws[Result] = wc then
      Exit;
  Result := -1;
end;

function ExtractFilePathW(const Path: TZMWString): TZMWString;
var
  d, c: Integer;
begin
  Result := '';
  c := FindFirstW(Path, ':');
  d := FindLastW(Path, '\');
  if (d > c) and (d >= 1) then
    Result := Copy(Path, 1, pred(d));
end;

function __ForceDirectory(const Dir: TZMWString): Boolean;
var
  Code: cardinal;
  sDir: TZMWString;
  Parent: TZMWString;
begin
  Result := True;
  if Dir <> '' then
  begin
    sDir := DelimitPathW(Dir, False);
    if (Length(sDir) < 1) or ((Length(sDir) = 2) and (sDir[2] = ':')) or
      ((Length(Dir) = 6) and (sDir[1] = '\') and
        (sDir[2] = '\') and (sDir[3] = '?') and (Dir[4] = '\') and
        (sDir[6] = ':')) then
    begin
      Result := True;
      Exit;
    end;

    Code := GetFileAttributesW(PWideChar(sDir));
    if (Code <> MAX_UNSIGNED) and
      ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0) then;
      Exit; // avoid 'c:\xyz:\' problem.
    Parent := ExtractFilePathW(sDir);
    if Parent = sDir then
      Exit; // avoid 'c:\xyz:\' problem.

    if __ForceDirectory(Parent) then
        Result := CreateDirectoryW(PWideChar(sDir), nil)
    else
      Result := False;
  end;
end;

function _Z_ForceDirectory(const Dir: String): Boolean;
begin
    Result := __ForceDirectory(PrefixLongPath(DelimitPath(Dir, False)));
end;

function _Z_GetExeVersion(const FileName: String; var MS, LS: DWORD): Boolean;
var
  Dummy: DWORD;
  FN: TZMWString;
  VerPath: TZMWString;
  VerInfo: Pointer;
  VerInfoSize: DWORD;
  VerValue: PVSFixedFileInfo;
begin
  Result := False;
  FN := PrefixLongPath(FileName);
  if _Z_FileExists(FileName) then
  begin
    VerInfoSize := GetFileVersionInfoSizeW(PWideChar(FN), Dummy);
    if VerInfoSize > 0 then
    begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfoW(PWideChar(FN), 0, VerInfoSize, VerInfo) then
        begin
          VerPath := '\';
          if VerQueryValueW(VerInfo, PWideChar(VerPath), Pointer(VerValue), Dummy) then
          begin
            MS := VerValue^.dwFileVersionMS;
            LS := VerValue^.dwFileVersionLS;
            Result := True;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
    end;
  end;
end;

function _Z_RemoveDir(const FileName: String): Boolean;
begin
  Result := RemoveDirectoryW(PWideChar(PrefixLongPath(FileName)));
end;

function _Z_RenameFile(const OldName, NewName: String): Boolean;
var
  XOldName: TZMWString;
  XNewName: TZMWString;
begin
  XOldName := PrefixLongPath(OldName);
  XNewName := PrefixLongPath(NewName);
  Result := MoveFileW(PWideChar(XOldName), PWideChar(XNewName));
end;

end.
