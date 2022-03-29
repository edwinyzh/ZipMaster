unit ZMUnzipOpr;

// ZMUnzip.pas - unzip operations

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
// modified 2014-09-10

{$I   '.\ZipVers.inc'}

(*
 FSpecArgs --
 [global switches]
 spec [local switches]
 //  file.zip>>filespec [local switches]
 switches
 /D:"[< or >]date"        [< _ before or > _ after (default)] date
 /D:"[< or >]-days"       [< _ before or > _ after (default)] days ago
 /J[+ or -]              Junk dirs
 /O[A or N or O or -]     overwrite always, newer, older, never
 /N[+ or -]  flags not to use AddNewName (default N- _ use AddNewName)
 /X:[old]::[new]  replace 'old' with 'new' - must result in valid internal name
 spec  select files in current zip according to spec
 /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to
 globals otherwise use spec
 /F: folder  change ExtrBaseDir
 /S or /S-  turns on or off recurse into sub-folders
 changes to excludes occur at current line and continue until changed

 <password  use password (to eol)
 - does not change already included files.
 local switches only applies to that line and modifies the 'current' excludes.
*)
interface

uses
{$IFDEF VERDXE2up}
  System.Classes, System.SysUtils, WinApi.Windows, VCL.Graphics,
{$ELSE}
  Classes, SysUtils, Windows, Graphics, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr, ZMZipReader, ZMZipDirectory, ZMEngine,
  ZMArgSplit, ZMZipBase, ZMStructs, ZMBaseOpr;

type
  TZMUnzOpts = class(TZMSelectArgs)
  private
    FBefore: Boolean;
    FDOSDate: Cardinal;
    FExcludes: string;
    FFolder: string;
    FJunkDir: Boolean;
    FNFlag: Boolean;
    FOvrOpt: TZMMergeOpts;
    FPassword: string;
    FTFlag: Boolean;
    FXArg: string;
  public
    function Accept(Rec: TZMEntryBase): Boolean; override;
    procedure Assign(Other: TZMSelectArgs); override;
    function Cloned: TZMUnzOpts;
    property Before: Boolean read FBefore write FBefore;
    property DOSDate: Cardinal read FDOSDate write FDOSDate;
    property Excludes: string read FExcludes write FExcludes;
    property Folder: string read FFolder write FFolder;
    property JunkDir: Boolean read FJunkDir write FJunkDir;
    property NFlag: Boolean read FNFlag write FNFlag;
    property OvrOpt: TZMMergeOpts read FOvrOpt write FOvrOpt;
    property Password: string read FPassword write FPassword;
    property TFlag: Boolean read FTFlag write FTFlag;
    property XArg: string read FXArg write FXArg;
  end;

type
  TZMUnzipOpr = class(TZMBaseOpr)
  private
    FExtractor: TZMDecompressor;
    FSingleFile: Boolean;
    FSplitter: TZMArgSplitter;
    FZName: string;
    FZReader: TZMZipReader;
    FZRec: TZMEntryBase;
    function AsStream(Obj: Pointer): TStream;
    function CheckCRC(CRC, ReqCRC: DWORD; const FileName: string): Integer;
    function CheckEncryption(const FName: string): Integer;
    function CheckEncryptionEx(const FName, Pw: string): Integer;
    function CheckExistsOrReplacable(var Exists: Boolean;
      const DestFileName: string; const Options: TZMUnzOpts): Integer;
    function CleanFileName(var DestFileName: string): Integer;
    procedure DeflateProgress(Sender: TObject; const Count: Integer;
      IsRead: Boolean);
    function DoExtractStreamStream(InStream: TMemoryStream;
      InitOutSize: Longword; HeaderType: TZMZHeader): Integer;
    function DoUndeflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function FinaliseExtracted(Writer: TZMZipBase; const DestName: string;
      UseNTFS: Boolean): Integer;
    function ForceBaseDir(const BasePath: string): Integer;
    function ProcessInclude(SrcZip: TZMZipReader; const Spec: string; const Args:
        TZMUnzOpts): Integer;
    procedure RemoveExistingFile(const DestFileName: string);
    procedure ReportSkipped(const Spec: string; Reason: TZMSkipTypes; Error:
        Integer);
    function SelectUnzFiles(SrcZip: TZMZipReader): Integer;
    procedure SetZReader(const Value: TZMZipReader);
    procedure SetZRec(const Value: TZMEntryBase);
    function TestEntry(Rec: TZMEntryBase): Integer;
    function TestPhrase(const Key: string): Integer;
    function UnzipEntry(const BasePath, DestFileName: string;
      const Exists: Boolean; const Options: TZMUnzOpts): Integer;
    function UnZipSelected(CurZip: TZMZipReader;
      SelectedCount: Integer): Integer;
    function UnzipTheEntry(const DestFileName: string;
      Options: TZMUnzOpts): Integer;
    function UpdateName(var DestFileName: string; const BasePath: string;
      const Options: TZMUnzOpts): Integer;
    function UpdateOptionsFromSplitter(var Args: TZMUnzOpts;
      const ParentExcludes: string): Boolean;
  protected
    AnswerNoAll: Boolean;
    function AskOverwrite(const FName: string; Older: Boolean;
      Idx: Integer): Boolean;
    function BuildPath(const Dir, PathBase: string;
      ZFile: TZMZipReader): Boolean;
    function CanSkip(const FileName: string; Error: Integer): TZMSkipTypes;
    procedure DefaultOptions(Options: TZMUnzOpts);
    function DoSetExtNameEvent(const ZBasePath: string;
      var OverName: string): Integer;
    function FlattenExcludes: string;
    function UnzipAFile(var DestName: string; const Rec: TZMEntryBase;
      const Options: TZMUnzOpts): Integer;
    function UnzipAStream(DestStream: TStream; const Rec: TZMEntryBase)
      : Integer;
    property SingleFile: Boolean read FSingleFile write FSingleFile;
    property ZName: string read FZName;
    property ZReader: TZMZipReader read FZReader write SetZReader;
    property ZRec: TZMEntryBase read FZRec write SetZRec;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ExtractFileToStream(const EntryName: string): Integer;
    function ExtractStreamToStream(InStream: TMemoryStream; OutSize: Longword;
      HeaderType: TZMZHeader): Integer;
    function Undeflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function UnzipFiles: Integer;
    // extract single file
    function UnzipToFile(const DestName: string;
      const ExtRec: TZMDirEntry): Integer;
    // extract single file
    function UnzipToStream(DestStream: TStream;
      const ExtRec: TZMDirEntry): Integer;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  Vcl.Dialogs, WinApi.ShlObj,
{$ELSE}
  Dialogs, ShlObj,
{$ENDIF}
  ZMLister, ZMBody, ZMUtils, ZMMsg, ZMWinFuncs, ZMCore, ZMMisc, ZMXcpt;

const
  __UNIT__ = 37;

type
  TZMUnzStreamArg = class(TZMSelectArgs)
  private
    FTheStream: TStream;
  public
    constructor Create(AStream: TStream);
    function Accept(Rec: TZMEntryBase): Boolean; override;
    procedure BeforeDestruction; override;
    property TheStream: TStream read FTheStream write FTheStream;
  end;

const
  BadStatus = ZsbInvalid or ZsbError or ZsbDiscard;

const
  UnzIncludeListThreshold = 10;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{ TZMUnzipOpr }
procedure TZMUnzipOpr.AfterConstruction;
begin
  inherited;
  FSplitter := TZMArgSplitter.Create;
  FExtractor := TZMDecompressor.Create(-32 * 1024);
end;

(*   Actioncode = 10, zacOverwrite,
  *      Extract(UnZip) Overwrite ask.
 *   (O) Arg3 = 'older'
 *   (O) Arg2 = Index
 *   (O) Arg1 = Overwrite_All
 *   (O) MsgP = filename
 *   (I) ActionCode -1 = overwrite
 *          -2 = don't overwrite
*)
function TZMUnzipOpr.AskOverwrite(const FName: string; Older: Boolean;
  Idx: Integer): Boolean;
var
  DoOverwrite: Boolean;
  TmpExtractOverwrite: TZMExtractOverwriteEvent;
begin
  Result := False;
  TmpExtractOverwrite := Master.OnExtractOverwrite;
  if Assigned(TmpExtractOverwrite) then
  begin
    DoOverwrite := AnswerAll;
    TmpExtractOverwrite(Master, FName, Older, DoOverwrite, Idx);
    if DoOverwrite then
      Result := True;
    Body.TraceFmt('[Overwrite] IN=%s,%d OUT=%s', [BoolStr(AnswerAll), Idx,
      BoolStr(Result)], {_LINE_}272, __UNIT__);
  end;
end;

function TZMUnzipOpr.AsStream(Obj: Pointer): TStream;
var
  AnObj: TObject;
begin
  Result := nil;
  if Obj <> nil then
  begin
    AnObj := TObject(Obj);
    if AnObj is TStream then
      Result := TStream(AnObj);
  end;
end;

procedure TZMUnzipOpr.BeforeDestruction;
begin
  FSplitter.Free;
  FExtractor.Free;
  inherited;
end;

function TZMUnzipOpr.BuildPath(const Dir, PathBase: string;
  ZFile: TZMZipReader): Boolean;
var
  FolderRec: TZMEntryBase;
  NTFSTimes: TNTFS_Times;
  OFileHandle: THandle;
  Parent: string;
  RelDir: string;
  SDir: string;
begin
  Result := True;
  if Dir <> '' then
  begin
    Body.Trace('-- build path: ' + Dir, {_LINE_}309, __UNIT__);
    SDir := DelimitPath(Dir, False);
    if _Z_DirExists(SDir) then
      Exit;
    if (Length(SDir) = 2) and (SDir[2] = ':') then
      Exit;
    Parent := ExtractFilePath(SDir);
    if Parent = SDir then
      Exit; // avoid 'c:\xyz:\' problem.

    if BuildPath(Parent, PathBase, ZFile) then
    begin
      Result := _Z_CreateDir(SDir);
      if Result and (ExtrNTFS in Body.ExtrOptions) and
        (Length(SDir) > Length(PathBase)) then
      begin
        // find folder name entry
        RelDir := Copy(SDir, Length(PathBase) + 1, MAX_PATH);
        RelDir := DelimitPath(RelDir, True);
        FolderRec := ZFile.FindName(RelDir, nil);
        if (FolderRec <> nil) and (FolderRec.FetchNTFSTimes(NTFSTimes) > 0) then
        begin
          // set times to NTFS times
          OFileHandle := _Z_CreateFile(PChar(SDir),
            GENERIC_READ + GENERIC_WRITE, 0, nil, OPEN_EXISTING,
            FILE_FLAG_BACKUP_SEMANTICS, 0);
          if (OFileHandle <> INVALID_HANDLE_VALUE) then
          begin
            try
              Result := SetFileTime(OFileHandle, @NTFSTimes.CTime,
                @NTFSTimes.ATime, @NTFSTimes.MTime);
            finally
              CloseHandle(OFileHandle);
            end
          end;
        end;
      end;
    end;
  end;
end;

function TZMUnzipOpr.CanSkip(const FileName: string; Error: Integer)
  : TZMSkipTypes;
var
  Err: Integer;
begin
  Result := StNoSkip;
  Err := AbsErr(Error);
  if Error <> 0 then
  begin
    // What isn't fatal
    case Err of
      ZE_BadFileName, ZE_FileCreate, ZE_LOHBadRead, ZE_LOHWrongName,
        ZE_NoExtrDir, ZE_NoOutFile, ZE_PasswordCancel, ZE_ReadZipError,
        ZE_SeekError, ZE_CryptError, ZE_NoChangeDir, ZE_WrongLength,
        ZE_ZipDataError:
        Result := StGeneralExtractError;
      ZE_PasswordFail, ZE_WrongPassword, ZE_UnatExtPWMiss:
        Result := StBadPassword;
      ZE_NotFound:
        Result := StOnFreshen;
      ZE_NoOverwrite:
        Result := StNoOverwrite;
      ZE_BadCRC:
        Result := StCRCError;
      ZE_BuildPathError:
        Result := StPathError;
      ZE_EntryCancelled:
        Result := StUser;
      ZE_SetDateError, ZE_SetFileAttributes, ZE_SetFileTimes,
        ZE_SetFileInformation:
        Result := StWarning;
      ZE_Unsupported:
        Result := StCompressionUnknown;
    end;
  end;
  if (Result <> StNoSkip) and Skipping(FileName, Result, Error) then
    Result := StNoSkip;
end;

function TZMUnzipOpr.CheckCRC(CRC, ReqCRC: DWORD; const FileName: string):
    Integer;
var
  DoExtract: Boolean;
  TmpCRCError: TZMCRC32ErrorEvent;
begin
  Result := 0;
  if CRC <> ReqCRC then
  begin
    Body.InformFmt(' >>> crc error:  %4x should be %4x', [CRC, ReqCRC],
      {_LINE_}399, __UNIT__);
    DoExtract := False;
    TmpCRCError := Master.OnCRC32Error;
    if Assigned(TmpCRCError) then
      TmpCRCError(Master, FileName, CRC, ReqCRC, DoExtract);
    if DoExtract then
      Result := 0
    else
      Result := ZE_BadCRC;
    Result := ZM_Error({_LINE_}408, Result);
  end;
end;

// return 0 _ password match, <0 _ error or cancel, >0 _ no match
function TZMUnzipOpr.CheckEncryption(const FName: string): Integer;
var
  AllowedReqs: Integer;
  HasKey: Boolean;
  Key: string;
  PWErr: Integer;
  ReqsLeft: Integer;
  Response: TMsgDlgBtn;
  TmpPasswordError: TZMPasswordErrorEvent;
begin
  Key := Body.Password; // test global first
  HasKey := Key <> '';
  if HasKey then
    PWErr := ZE_PasswordFail
  else
    PWErr := ZE_UnatExtPWMiss;
  if HasKey then
  begin
    Result := TestPhrase(Key);
    if Result <= 0 then
      Exit; // matched or error
  end;
  // Ask for password
  ReqsLeft := Body.PasswordReqCount;
  AllowedReqs := 15;
  Result := -1;
  while (ReqsLeft > 0) and (AllowedReqs > 0) and not AnswerNoAll do
  begin
    CheckCancel;
    Key := '';
    Response := MbOK;
    TmpPasswordError := Body.Master.OnPasswordError;
    if Assigned(TmpPasswordError) then
    begin
      TmpPasswordError(Body.Master, False, Key, FName, LongWord(ReqsLeft),
        Response);
      ReqsLeft := ReqsLeft and 15;
    end
    else
    begin
      if not Body.Unattended then
        Key := Body._GetExtrPassword(Response)
      else
      begin
        Result := ZM_Error({_LINE_}457, PWErr);
        Body.ShowError(Result);
        Exit;
      end;
    end;
    if (Response = MbCancel) or (Response = MbAbort) or (Response = MbNoToAll)
    then
    begin
      if Response = MbNoToAll then
        AnswerNoAll := True;
      Result := ZM_Error({_LINE_}467, ZS_Canceled);
      Break;
    end;
    if Response <> MbOk then
      Key := ''; // ignore
    Body.Password := Key; // save key for later entries too
    if Key <> '' then
    begin
      if TestPhrase(Key) = 0 then
      begin
        Result := 0; // matched
        Break;
      end;
      Result := ZM_Error({_LINE_}480, ZE_PasswordFail);
    end;
    if ReqsLeft > AllowedReqs then
      ReqsLeft := AllowedReqs;
    Dec(ReqsLeft);
    Dec(AllowedReqs);
  end;
  if Result = -1 then
    Result := ZM_Error({_LINE_}488, PWErr);
end;

// return 0 _ password match, <0 _ error or cancel, >0 _ no match
function TZMUnzipOpr.CheckEncryptionEx(const FName, Pw: string): Integer;
begin
  if Pw <> '' then
    Result := TestPhrase(Pw)
  else
    Result := CheckEncryption(FName);
end;

function TZMUnzipOpr.CheckExistsOrReplacable(var Exists: Boolean;
  const DestFileName: string; const Options: TZMUnzOpts): Integer;
var
  DoOverWrite: Boolean;
  ExistDate: TDateTime;
  TmpOnOverWrite: TZMExtractOverwriteEvent;
begin
  Result := 0;
  Exists := FileLastModified(DestFileName, ExistDate);
  if (not Exists) and (ExtrFreshen in Body.ExtrOptions) then
  begin
    Result := ZM_Error({_LINE_}511, ZE_NotFound);
    Exit; // skip entry
  end;

  if Exists then
  begin
    if Verbosity >= ZvVerbose then
    begin
      Body.InformFmt('"%s" exists = %s file = %s',
        [DestFileName, DateTimeToStr(ExistDate), DateTimeToStr(ZRec.DateStamp)],
        {_LINE_}521, __UNIT__);
    end;
    TmpOnOverWrite := Master.OnExtractOverwrite;
    DoOverWrite := True;
    if ExistDate < ZRec.DateStamp then
    begin
      // exists older
      DoOverWrite := (Options.OvrOpt = ZmoAlways) or
        (Options.OvrOpt = ZmoNewer);
      if Assigned(TmpOnOverWrite) then
        TmpOnOverWrite(Master, DestFileName, True, DoOverWrite, ZRec.ExtIndex);
    end
    else
    begin
      // exists newer
      DoOverWrite := ((Options.OvrOpt = ZmoAlways) or
        (Options.OvrOpt = ZmoOlder)) and
        not((ExtrFreshen in Body.ExtrOptions) or
        (ExtrUpdate in Body.ExtrOptions));
      if Assigned(TmpOnOverWrite) then
        TmpOnOverWrite(Master, DestFileName, False, DoOverWrite, ZRec.ExtIndex);
    end;
    if not DoOverWrite then
      Result := Body.PrepareErrMsg(ZE_NoOverwrite, [DestFileName], {_LINE_}544,
        __UNIT__); // skip entry
  end;
end;

function TZMUnzipOpr.CleanFileName(var DestFileName: string): Integer;
var
  Cleaned: string;
begin
  Result := 0;
  if not IsExtPath(DestFileName) then
  begin
    Result := CleanPath(Cleaned, DestFileName, True);
    if Result <> 0 then
    begin
      Body.InformFmt('Invalid filename [%d]: "%s"',
        [AbsErr(Result), DestFileName], {_LINE_}560, __UNIT__);
      Result := Body.PrepareErrMsg(ZE_BadFileName, [DestFileName],
        {_LINE_}562, __UNIT__);
    end
    else
      DestFileName := Cleaned;
  end;
end;

procedure TZMUnzipOpr.DefaultOptions(Options: TZMUnzOpts);
begin
  Options.Before := False;
  Options.DOSDate := 0;
  Options.Excludes := FlattenExcludes;
  Options.Folder := Body.ExtrBaseDir;
  Options.JunkDir := not(ExtrDirNames in Body.ExtrOptions);
  Options.NFlag := not Assigned(Body.Master.OnSetExtName);
  Options.Password := ''; // use global or ask
  Options.TFlag := ExtrNTFS in Body.ExtrOptions;
  Options.XArg := '';
  if ExtrOverWrite in Body.ExtrOptions then
    Options.OvrOpt := ZmoAlways
  else
    Options.OvrOpt := ZmoConfirm;
end;

procedure TZMUnzipOpr.DeflateProgress(Sender: TObject; const Count: Integer;
  IsRead: Boolean);
begin
  if IsRead then
    Progress.Advance(Count)
  else
    Progress.MoreWritten(Count);
  if Body.Cancel <> 0 then
    raise EZMAbort.Create;
end;

function TZMUnzipOpr.DoExtractStreamStream(InStream: TMemoryStream;
  InitOutSize: Longword; HeaderType: TZMZHeader): Integer;
var
  Done: Integer;
  Header: TZM_StreamHeader;
  Method: TZMDeflates;
  Mthd: Integer;
  Realsize: Integer;
begin
  Result := 0;
  Realsize := Integer(InStream.Size - InStream.Position) -
    SizeOf(TZM_StreamHeader);
  Method := ZmDeflate;
  Body.ZipStream.SetSize(InitOutSize);
  if (HeaderType = ZzAuto) and (Realsize < 0) then
    HeaderType := ZzCompat; // assume a few bytes of data only
  if (HeaderType <> ZzCompat) and (Realsize >= 0) then
  begin
    InStream.ReadBuffer(Header, SizeOf(TZM_StreamHeader));
    case Header.Method of
      METHOD_DEFLATED:
        Method := ZmDeflate;
      METHOD_STORED:
        Method := ZmStore;
    else
      if HeaderType = ZzAuto then
      begin
        HeaderType := ZzCompat;
        Realsize := Realsize + SizeOf(TZM_StreamHeader);
        if InStream.Seek(-Sizeof(TZM_StreamHeader), SoCurrent) < 0 then
        begin
          Result := ZM_Error({_LINE_}628, ZE_SeekError);
          Body.ZipStream.Size := 0;
          Exit;
        end;
      end
      else
      begin
        Result := ZM_Error({_LINE_}635, ZE_Unsupported);
        Body.ZipStream.Size := 0;
        Exit;
      end;
    end;
  end;
  if Realsize > 0 then
  begin
    Progress.NewItem('<stream>', InitOutSize);
    try
      FExtractor.OutStream := Body.ZipStream;
      FExtractor.InStream := InStream;
      FExtractor.InSize := -1;
      FExtractor.OutSize := -1; // no limit
      if Method = ZmDeflate then
        Mthd := METHOD_DEFLATED
      else
        Mthd := METHOD_STORED;
      Result := FExtractor.Prepare(Mthd);
      if Result = 0 then
      begin
        Done := FExtractor.Decompress;
        if Done < 0 then
          Result := ZM_Error({_LINE_}658, ZE_InvalidZip)
        else
        begin
          if (HeaderType <> ZzCompat) and (Header.CRC <> FExtractor.CRC) then
          begin
            Body.InformFmt(' >>> crc error: %4x should be %4x',
              [FExtractor.CRC, Header.CRC], {_LINE_}664, __UNIT__);
            Result := ZM_Error({_LINE_}665, ZE_BadCRC);
          end;
        end;
      end;
      Body.TraceFmt('Inflate returns: %s', [Errors.ErrorStr(Result)],
        {_LINE_}670, __UNIT__);
    finally
      Progress.EndItem;
    end;
  end;
end;

// return <0 _ skipped, 0 _ did nothing, >0 _ changed
function TZMUnzipOpr.DoSetExtNameEvent(const ZBasePath: string;
  var OverName: string): Integer;
var
  IsChanged: Boolean;
  TempName: string;
  TmpSetExtName: TZMSetExtNameEvent;
begin
  Result := 0;
  TmpSetExtName := Master.OnSetExtName;
  if Assigned(TmpSetExtName) then
  begin
    TempName := ZName;
    IsChanged := False;
    TmpSetExtName(Master, TempName, ZBasePath, IsChanged);
    if IsChanged then
    begin
      Body.InformFmt('%s changed to: "%s"', [ZName, TempName], {_LINE_}694,
        __UNIT__);
      TempName := Unquote(TempName);
      if TempName = '' then
      begin
        Result := ZM_Error({_LINE_}699, ZE_EntryCancelled);
        Body.InformFmt('user cancelled: %s', [ZName], Result, 0);
      end
      else
      begin
        OverName := TempName;
        Result := 1; // changed
      end;
    end;
  end;
end;

// return <0 _ error, 0 _ ok, >0 _ error not extracted (skipped)
function TZMUnzipOpr.DoUndeflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  Done: Integer;
  Mthd: Integer;
begin
  if Length < 0 then
    Length := InStream.Size;
  if Length = 0 then
  begin
    Length := InStream.Size;
    InStream.Position := 0;
  end;

  Progress.TotalCount := 1;
  Progress.TotalSize := Length; // to read

  FExtractor.InStream := InStream;
  FExtractor.InSize := Length;
  FExtractor.OutStream := OutStream;
  FExtractor.OutSize := -1; // no limit
  FExtractor.OnProgress := DeflateProgress;
  if Method = ZmDeflate then
    Mthd := METHOD_DEFLATED
  else
    Mthd := METHOD_STORED;
  Result := FExtractor.Prepare(Mthd);
  if Result = 0 then
  begin
    Progress.NewItem('<stream>', Length);
    try
      Done := FExtractor.Decompress;
      if Done < 0 then
      begin
        Body.TraceFmt('Undeflate returns: %s', [Errors.ErrorStr(Integer(Done))],
          {_LINE_}747, __UNIT__);
        Result := ZM_Error({_LINE_}748, ZE_InvalidZip);
      end
      else
        CRC := FExtractor.CRC;
      Body.TraceFmt('Inflate returns: %s', [Errors.ErrorStr(Result)],
        {_LINE_}753, __UNIT__);
    finally
      Progress.EndItem;
    end;
  end;
  if Result = 0 then
    SuccessCnt := 1;
end;

function TZMUnzipOpr.ExtractFileToStream(const EntryName: string): Integer;
var
  CZip: TZMZipReader;
  Fn: string;
  Rec: TZMEntryBase;
begin
  Body.ZipStream.Clear;
  Result := 0;
  Fn := Trim(EntryName);
  if (Length(Fn) = 0) and (IncludeSpecs.Count > 0) then
    Fn := Trim(IncludeSpecs[0]);
  if IsWild(Fn) then
    Result := Body.PrepareErrMsg(ZE_WildName, [Fn], {_LINE_}774, __UNIT__)
  else
    if Fn = '' then
      Result := ZM_Error({_LINE_}777, ZE_NothingToDo);
  if Result >= 0 then
  begin
    Body.ClearIncludeSpecs;
    CZip := Lister.CurrentZip(True, False);
    Rec := CZip.FindName(Fn, nil);
    if Rec <> nil then
    begin
      Result := UnzipAStream(Body.ZipStream, Rec);
      if Result = 0 then
      begin
        SuccessCnt := 1;
        ReportMessage(ZM_Error({_LINE_}789, 0),
          Format('Unzipped file %s of size %d',
          [Rec.FileName, Rec.UncompressedSize]));
      end;
    end
    else
      Result := ZM_Error({_LINE_}795, ZE_NothingToDo);
  end;
  if Result <> 0 then
  begin
    // error
    Body.ZipStream.Clear;
  end;
end;

function TZMUnzipOpr.ExtractStreamToStream(InStream: TMemoryStream;
  OutSize: Longword; HeaderType: TZMZHeader): Integer;
begin
  Result := 0;
  Body.ZipStream.Clear();
  if not Assigned(InStream) then
    Result := ZM_Error({_LINE_}810, ZE_NothingToDo)
  else
    if InStream = Body.ZipStream then
      Result := ZM_Error({_LINE_}813, ZE_InIsOutStream);
  if Result = 0 then
    Result := DoExtractStreamStream(InStream, OutSize, HeaderType);
  if Result = 0 then
    SuccessCnt := 1
  else
    Body.ZipStream.Size := 0;
end;

function TZMUnzipOpr.FinaliseExtracted(Writer: TZMZipBase;
  const DestName: string; UseNTFS: Boolean): Integer;
var
  Err: Integer;
  NTFSTimes: TNTFS_Times;
begin
  Result := 0;
  // set times
  if UseNTFS and (ZRec.FetchNTFSTimes(NTFSTimes) > 0) then
  begin
    if not Writer.File_SetTime(@NTFSTimes.CTime, @NTFSTimes.ATime,
      @NTFSTimes.MTime) then
      Result := Body.PrepareErrMsg(ZE_SetFileTimes, [Writer.RealFileName],
        {_LINE_} 835, __UNIT__);
  end
  else
    Result := Writer.FixFileDate;
  if Result = 0 then
    _Z_ChangeNotify(SHCNE_UPDATEITEM, DestName);
  // set attributes
  Err := Writer.FixFileAttrs;
  if Err = 0 then
    _Z_ChangeNotify(SHCNE_ATTRIBUTES, DestName);
  if Result = 0 then
    Result := Err
  else
    if Err <> 0 then
      Result := Body.PrepareErrMsg(ZE_SetFileInformation, [DestName],
        {_LINE_} 850, __UNIT__);
end;

function TZMUnzipOpr.FlattenExcludes: string;
var
  I: Integer;
  S: string;
begin
  Result := '';
  // flatten the list
  for I := 0 to ExcludeSpecs.Count - 1 do
  begin
    S := ExcludeSpecs[I];
    if S = '' then
      Continue;
    if Result <> '' then
      Result := Result + SPEC_SEP;;
    Result := Result + S;
  end;
end;

function TZMUnzipOpr.ForceBaseDir(const BasePath: string): Integer;
begin
  Result := 0;
  if BasePath <> '' then
  begin
    if ExtrForceDirs in Body.ExtrOptions then
    begin
      if not ForceDirectory(BasePath) then
      begin
        Body.InformFmt('ForceDirectory failed: %s', [BasePath], {_LINE_}880,
          __UNIT__);
        Result := Body.PrepareErrMsg(ZE_BuildBaseError, [BasePath], {_LINE_}882,
          __UNIT__);
      end;
    end
    else
      if not DirExists(BasePath) then
      begin
        Body.InformFmt('path must exist: %s', [BasePath], {_LINE_}889,
          __UNIT__);
        Result := Body.PrepareErrMsg(ZE_BuildBaseError, [BasePath], {_LINE_}891,
          __UNIT__);
      end;
  end;
end;

// select entries
// returns <0 _ error, 0 _ ok, >0 _ number of entries selected
function TZMUnzipOpr.ProcessInclude(SrcZip: TZMZipReader; const Spec: string;
    const Args: TZMUnzOpts): Integer;
begin
  Result := SrcZip.SelectRec(Spec, Args.Excludes, ZzsSet, Args);
  if Result < 1 then
  begin
    // none found
    ReportSkipped(Spec, StNotFound, ZM_Error({_LINE_}906, ZE_NothingToDo));
  end;
end;

procedure TZMUnzipOpr.RemoveExistingFile(const DestFileName: string);
var
  Attrs: Cardinal;
begin
  // is read-only?
  Attrs := _Z_GetFileAttributes(DestFileName);
  if (Attrs and FILE_ATTRIBUTE_READONLY) <> 0 then
  begin
    Body.InformFmt('Existing file is Read-Only: %s', [DestFileName],
      {_LINE_}919, __UNIT__);
    // clear read-only
    if not _Z_SetFileAttributes(DestFileName, Attrs xor FILE_ATTRIBUTE_READONLY)
    then
      Body.InformFmt('Failed to clear Read-Only status: %s', [DestFileName],
        {_LINE_}924, __UNIT__);
  end;
  if _Z_EraseFile(DestFileName, not(ExtrSafe in Body.ExtrOptions)) = 0 then
  begin
    Body.TraceFmt('Deleted pre-existing file %s', [DestFileName],
      {_LINE_}929, __UNIT__);
    _Z_ChangeNotify(SHCNE_DELETE, DestFileName);
  end
  else
    Body.InformSysFmt('Deleting pre-existing file %s failed', [DestFileName],
      {_LINE_}934, __UNIT__)
end;

procedure TZMUnzipOpr.ReportSkipped(const Spec: string; Reason: TZMSkipTypes;
    Error: Integer);
begin
  if Skipping(Spec, Reason, Error) then
    raise EZipMaster.CreateMsg(Body, Error, 0, 0);
end;

function TZMUnzipOpr.SelectUnzFiles(SrcZip: TZMZipReader): Integer;
var
  AStream: TStream;
  DefaultExcludes: string;
  Effectives: TZMUnzOpts;
  I: Integer;
  Locals: TZMUnzOpts;
  SelectCount: Integer;
  ShowXProgress: Boolean;
  Spec: string;
  StreamArg: TZMUnzStreamArg;
begin
  Result := 0;
  SelectCount := 0;
  Effectives := TZMUnzOpts(SrcZip.AddSelectArgs(TZMUnzOpts.Create));
  DefaultOptions(Effectives); // set up defaults
  DefaultExcludes := Effectives.Excludes;
  Locals := nil;

  FSplitter.Allow := '><DEFJNOSTX';
  FSplitter.Options := [ZaoWildSpec];
  ShowXProgress := IncludeSpecs.Count > UnzIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(ZxProcessing, IncludeSpecs.Count);
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if Result < 0 then
      Break;
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Result := 0;
    AStream := AsStream(IncludeSpecs.Objects[I]);
    if AStream <> nil then
    begin
      StreamArg := TZMUnzStreamArg
        (SrcZip.AddSelectArgs(TZMUnzStreamArg.Create(AStream)));
      Result := SrcZip.SelectRec(IncludeSpecs[I], '', ZzsSet, StreamArg);
      if Result < 1 then
      begin
        // none found
        SrcZip.FreeSelectArgs(StreamArg);
        ReportSkipped(IncludeSpecs[I], StNotFound,
          ZM_Error({_LINE_}987, ZE_NothingToDo));
      end
      else
        if Result > 0 then
          IncludeSpecs.Objects[I] := nil;
      // now controlled by list of select args
      Continue;
    end;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> ZasNone then
      raise EZipMaster.CreateMsgFmt(Body, ZE_InvalidParameter, [FSplitter.Raw],
        {_LINE_}998, __UNIT__);

    Spec := FSplitter.Main;
    if (Spec = '') and FSplitter.Has('>') then
      Spec := FSplitter.Arg('>');
    if Spec = '' then
    begin
      // no spec _ set new globals
      // ignore empty lines
      if FSplitter.Found <> '' then
      begin
        // no spec, set defaults
        if UpdateOptionsFromSplitter(Effectives, DefaultExcludes) then
          SrcZip.AddSelectArgs(Effectives); // add new set to list
      end;
      Continue;
    end;
    // process spec and/or switches
    if FSplitter.Found = '' then
      Result := ProcessInclude(SrcZip, Spec, Effectives)
    else
    begin
      // using local settings from splitter
      Locals := Effectives;
      if UpdateOptionsFromSplitter(Locals, Effectives.Excludes) then
      begin
        Result := ProcessInclude(SrcZip, Spec, Locals);
        if Result > 0 then
          SrcZip.AddSelectArgs(Locals) // add new set to list
        else
          Locals.Free;
      end
      else // just use Effectives
        Result := ProcessInclude(SrcZip, Spec, Effectives);
    end;
    if Result > 0 then
      SelectCount := SelectCount + Result;
  end;
  if SelectCount > 0 then
    Result := SelectCount;
end;

procedure TZMUnzipOpr.SetZReader(const Value: TZMZipReader);
begin
  //
end;

procedure TZMUnzipOpr.SetZRec(const Value: TZMEntryBase);
begin
  FZRec := Value;
  Assert(ZRec <> nil, 'ZRec is nil');
  FZReader := ZRec.MyFile as TZMZipReader;
  FZName := ZRec.FileName;
end;

function TZMUnzipOpr.TestEntry(Rec: TZMEntryBase): Integer;
begin
  // if testing unzip to nil stream
  Result := 0;
  Body.TraceFmt('Testing: %s', [Rec.FileName], {_LINE_}1057, __UNIT__);
  if not Rec.IsDirOnly then
  begin
    Result := UnzipAStream(nil, Rec);
    if Result = 0 then
      ReportMessage(0, Format('Tested file %s of size %d',
        [Rec.FileName, ZRec.UncompressedSize]));
  end;
end;

// return 0 _ password match, <0 _ error or cancel, >0 _ no match
function TZMUnzipOpr.TestPhrase(const Key: string): Integer;
begin
  Result := 1; // no match
  if Key <> '' then
  begin
    Result := FExtractor.Decryptor.Unlock(AnsiString(Key));
    if Result < 0 then
    begin
      Result := ZM_Error({_LINE_}1076, ZE_LOHBadRead);
      Body.Inform('could not read encryption header', {_LINE_}1077, __UNIT__);
    end;
  end;
end;

function TZMUnzipOpr.Undeflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
begin
  Result := 0;
  if not Assigned(InStream) then
    Result := ZM_Error({_LINE_}1087, ZE_NoInStream)
  else
    if not Assigned(OutStream) then
      Result := ZM_Error({_LINE_}1090, ZE_NoOutStream)
    else
      if InStream = OutStream then
        Result := ZM_Error({_LINE_}1093, ZE_InIsOutStream);
  if Result = 0 then
    Result := DoUndeflate(OutStream, InStream, Length, Method, CRC);
end;

function TZMUnzipOpr.UnzipAFile(var DestName: string; const Rec: TZMEntryBase;
  const Options: TZMUnzOpts): Integer;
var
  BasePath: string;
  DefaultName: string;
  DestFileName: string;
  Exists: Boolean;
  NameChanged: Boolean;
begin
  if Rec <> nil then
    ZRec := Rec;
  if (ZRec = nil) or (ZRec.StatusBit[BadStatus] <> 0) then
  begin
    Result := ZM_Error({_LINE_}1111, ZE_InvalidEntry);
    Body.Inform('Invalid zip entry', {_LINE_}1112, __UNIT__);
    Exit;
  end;
  if ((ZRec.VersionNeeded and VerMask) > 45) or (ZRec.CompressionMethod > 8) then
  begin
    Result := ZM_Error({_LINE_}1107, ZE_Unsupported);
    Body.InformFmt('Unsupported zip entry: need version %d.%d or compressed %d',
      [(ZRec.VersionNeeded and VerMask) div 10, (ZRec.VersionNeeded and VerMask) mod 10,
      ZRec.CompressionMethod], {_LINE_}1111, __UNIT__);
    Exit;
  end;
  ZReader.Guage.NewItem(ZName, ZRec.CompressedSize);
  if not Options.JunkDir then
    DefaultName := ZName
  else
    DefaultName := ExtractFileName(ZName); // drop stored folders

  if (Options.JunkDir) and (ZRec.IsDirOnly) then
  begin
    // nothing to do
    Body.Inform('nothing to extract', {_LINE_}1124, __UNIT__);
    Result := 0;
    Exit;
  end;

  if ExtrTest in Body.ExtrOptions then
  begin
    Result := TestEntry(ZRec);
    Exit;
  end;

  BasePath := Options.Folder;
  if BasePath = '' then
    BasePath := '.\'; // current dir
  // BasePath is the default path
  Result := DriveFolders.ExpandPath(BasePath, BasePath);
  if Result = Z_WILD then
    Result := ZM_Error(1072, ZS_InvalidPath);
  if Result < 0 then
    Exit;
  BasePath := DelimitPath(BasePath, True);

  // prepare dest possibly overriding name and/or path
  if DestName = '' then
  begin
    DestFileName := DefaultName;
    Result := UpdateName(DestFileName, BasePath, Options);
    if Result < 0 then
      Exit;
  end
  else
  begin
    if DestName[1] = '.' then
    begin
      // relative to BasePath
      Result := DriveFolders.AddToPath(DestFileName, BasePath, DestName);
      if Result < 0 then
        Exit;
    end
    else
    begin
      Result := DriveFolders.ExpandPath(DestFileName, DestName);
      if Result < 0 then
        Exit;
    end;
    if (not ZRec.IsDirOnly) and (LastChar(DestFileName) = '\') then
    begin
      // just change base path
      BasePath := DestFileName;
      DestFileName := DefaultName;
    end;
  end;
  Result := CleanFileName(DestFileName);
  if Result < 0 then
    Exit; // invalid

  DestName := ZName; // the actual dest
  NameChanged := CompareStr(DestFileName, DefaultName) <> 0;
  if NameChanged then
  begin
    DestName := DestName + ' :: ' + DestFileName; // expand if changed
    // check not changing folders to/from name
    if ZRec.IsDirOnly <> (LastChar(DestFileName) = '\') then
    begin
      Result := ZM_Error({_LINE_}1188, ZE_NoChangeDir);
      Body.InformFmt('changed file to/from folder: %s %s',
        [ZName, DestFileName],
        {_LINE_}1191, __UNIT__);
      Exit; // always fatal
    end;
  end;

  // override default path and/or name
  if PathIsAbsolute(DestFileName) then
    BasePath := '' // do not check existence
  else
  begin
    Result := DriveFolders.AddToPath(DestFileName, BasePath, DestFileName);
    if Result < 0 then
    begin
      Result := Body.PrepareErrMsg(ZE_BadFileName, [DestFileName],
        {_LINE_}1205, __UNIT__);
      Exit;
    end;
    if Result = Z_SHORT_PATH then
    begin
      BasePath := ShortPathName(BasePath);
      Body.InformFmt('Shortened path to: %s', [BasePath], {_LINE_}1211,
        __UNIT__);
    end;
  end;

  Exists := False;
  if ZRec.IsDirOnly then
    Exists := _Z_DirExists(DestFileName)
  else
  begin
    Result := CheckExistsOrReplacable(Exists, DestFileName, Options);
    if Result <> 0 then
      Exit;
  end;

  // if we get here we definitely want to extract or test the entry
  if not ZReader.IsOpen then
  begin
    Result := ZReader.File_Reopen(FmOpenRead or fmShareDenyWrite);
//    Result := ZReader.File_Reopen(FmOpenRead);
    if Result < 0 then
      Exit;
  end;
  Result := UnzipEntry(BasePath, DestFileName, Exists, Options);
  if Result = 0 then
  begin
    ReportMessage(0, Format('Unzipped file %s of size %d',
      [DestName, ZRec.UncompressedSize]));
  end;
end;

function TZMUnzipOpr.UnzipAStream(DestStream: TStream;
  const Rec: TZMEntryBase): Integer;
var
  Err: Integer;
  LOH: TZipLocalHeader;
  RefValue: DWORD;
begin
  if Rec <> nil then
    ZRec := Rec;
  if (ZRec = nil) or (ZRec.StatusBit[BadStatus] <> 0) then
  begin
    Result := ZM_Error({_LINE_}1252, ZE_InvalidEntry);
    Body.Inform('Invalid zip entry', {_LINE_}1253, __UNIT__);
    Exit;
  end;
  if ((ZRec.VersionNeeded and VerMask) > 45) or (ZRec.CompressionMethod > 8) then
  begin
    Result := ZM_Error({_LINE_}1107, ZE_Unsupported);
    Body.InformFmt('Unsupported zip entry: need version %d.%d or compressed %d',
      [(ZRec.VersionNeeded and VerMask) div 10, (ZRec.VersionNeeded and VerMask) mod 10,
      ZRec.CompressionMethod], {_LINE_}1260, __UNIT__);
    Exit;
  end;

  // Test not ExtStream
  if @DestStream = @ZReader.Stream then
  begin
    Result := ZM_Error({_LINE_}1260, ZE_InIsOutStream);
    Exit;
  end;

  if ZRec.IsDirOnly then
  begin
    // extract zero length
    Result := 0;
    Exit;
  end;

  if not ZReader.IsOpen then
  begin
    Result := ZReader.File_Reopen(FmOpenRead or fmShareDenyWrite);
//    Result := ZReader.File_Reopen(FmOpenRead);
    if Result < 0 then
      Exit;
  end;

  // position to and check local header
  Result := ZRec.SeekLocalData(LOH, ZRec._FileName);
  if Result < 0 then
    Exit;

  FExtractor.OutStream := DestStream;
  FExtractor.InStream := ZReader.Stream;
  FExtractor.InSize := ZRec.CompressedSize;
  FExtractor.OutSize := ZRec.UncompressedSize;
  Result := FExtractor.Prepare(LOH.ComprMethod);
  if Result < 0 then
    Exit;
  FExtractor.OnProgress := DeflateProgress;
  // is it encrypted
  if ZRec.Encrypted then
  begin
    if ((LOH.Flag and FLAG_DATADESC_BIT) = 0) then
      RefValue := (LOH.CRC32 shr 24)
    else
      RefValue := ((LOH.ModifDateTime shr 8) and $FF);
    FExtractor.PrepareCrypt(ZcZip);
    if FExtractor.Decryptor.DecodeInit(RefValue) < 0 then
      Result := ZM_Error({_LINE_}1300, ZE_ReadZipError)
    else
    begin
      Result := CheckEncryption(ZRec.FileName);
      if AbsErr(Result) = ZE_PasswordFail then
        Result := ZM_Error({_LINE_}1305, ZE_PasswordFail);
    end;
  end;

  if Result >= 0 then
  begin
    ZReader.Guage.NewItem(ZName, ZRec.CompressedSize);
    try
      Result := FExtractor.Decompress;
      Body.TraceFmt('Inflate returns: %s', [Body.Errors.ErrorStr(Result)],
        {_LINE_}1315, __UNIT__);
      if Result >= 0 then
        Result := CheckCRC(FExtractor.CRC, ZRec.CRC32, ZRec.FileName);
      Err := AbsErr(Result);
      if (DestStream <> nil) and ((Err = ZE_BadCRC) or (Err = ZE_EntryCancelled))
      then
        DestStream.Size := 0;
    finally
      ZReader.Guage.EndItem;
    end;
  end;
end;

function TZMUnzipOpr.UnzipEntry(const BasePath, DestFileName: string;
  const Exists: Boolean; const Options: TZMUnzOpts): Integer;
var
  LOH: TZipLocalHeader;
  RefValue: DWORD;
begin
  // position to and read and check local header
  Result := ZRec.SeekLocalData(LOH, ZRec._FileName);
  if Result < 0 then
    Exit;

  // Prepare for extract
  FExtractor.InStream := ZReader.Stream;
  FExtractor.InSize := ZRec.CompressedSize;
  FExtractor.OutSize := ZRec.UncompressedSize;
  Result := FExtractor.Prepare(LOH.ComprMethod);
  if Result < 0 then
    Exit;
  FExtractor.OnProgress := DeflateProgress;
  // is it encrypted
  if ZRec.Encrypted then
  begin
    if ((LOH.Flag and 8) = 0) then
      RefValue := (LOH.CRC32 shr 24)
    else
      RefValue := ((LOH.ModifDateTime shr 8) and $FF);
    FExtractor.PrepareCrypt(ZcZip);
    if FExtractor.Decryptor.DecodeInit(RefValue) < 0 then
      Result := ZM_Error({_LINE_}1356, ZE_ReadZipError)
    else
      Result := CheckEncryptionEx(ZRec.FileName, Options.Password);
  end;

  if Result = 0 then
  begin
    if not Exists then
    begin
      // File not found - build path possibly with times
      Result := ForceBaseDir(BasePath);
      if (Result = 0) then
      begin
        if not BuildPath(ExtractFilePath(DestFileName), BasePath, ZReader) then
          // error _ could not make path
          Result := Body.PrepareErrMsg(ZE_BuildPathError, [DestFileName],
            {_LINE_}1372, __UNIT__);
      end;
      if Result <> 0 then
        Exit; // error
    end;

    if ZRec.IsDirOnly then
    begin
      Result := 0;
      Exit;
    end;

    if Exists then
    begin
      RemoveExistingFile(DestFileName);
    end;

    // unzip or test file
    Result := UnzipTheEntry(DestFileName, Options);
  end;
end;

function TZMUnzipOpr.UnzipFiles: Integer;
var
  CurZip: TZMZipReader;
begin
  Body.Trace('StartUp Extract', {_LINE_}1398, __UNIT__);
  if Body.Logging then
    Body.LogSpecs('');
  if Lister.ZipFileName = '' then
    raise EZipMaster.CreateMsg(Body, ZE_NoZipSpecified, {_LINE_}1402, __UNIT__);
  ShowProgress := ZspFull;
  if Lister.Count = 0 then
  begin
    Result := Lister.List; // try again
    if Result < 0 then
    begin
      Body.Inform('Failed to load zip', {_LINE_}1409, __UNIT__);
      ShowError(Result);
      Exit;
    end;

    if Lister.Count = 0 then
    begin
      Body.Inform('no files to extract', {_LINE_}1416, __UNIT__);
      Result := ZM_Error({_LINE_}1417, Errors.Code);
      ShowError(Result);
      Exit;
    end;
  end;

  CurZip := Lister.Current;
  if IncludeSpecs.Count = 0 then
    IncludeSpecs.Add('*.*'); // default to all
  Result := SelectUnzFiles(CurZip);
  Body.ClearIncludeSpecs; // will contain files processed
  if Result < 0 then
  begin
    ShowError(Result);
    Exit;
  end;
  if Result = 0 then
  begin
    Body.Inform('nothing selected', {_LINE_}1435, __UNIT__);
    Result := ZM_Error({_LINE_}1436, ZE_NothingToDo);
    ShowError(Result);
    Exit;
  end;
  Body.InformFmt('Expects to Extract %d files', [Result], {_LINE_}1440,
    __UNIT__);
  Result := UnZipSelected(CurZip, Result);
  if Result >= 0 then
  begin
    // success
    Body.InformFmt('Files acted on = %d', [SuccessCnt], {_LINE_}1446, __UNIT__);
    Result := 0;
  end;
  if Result < 0 then
    ShowError(Result);
end;

function TZMUnzipOpr.UnZipSelected(CurZip: TZMZipReader;
  SelectedCount: Integer): Integer;
var
  Count: Integer;
  DestName: string;
  Rec: TZMEntryBase;
  Skip: TZMSkipTypes;
  Stopped: Boolean;
  Strm: TStream;
  Testing: Boolean;
  USize: Int64;
begin
  Result := 0;
  USize := 0;
  Count := 0;
  Rec := CurZip.FirstSelected;
  while Rec <> nil do
  begin
    Inc(Count);
    USize := USize + Rec.CompressedSize;
    Rec := CurZip.NextSelected(Rec);
  end;
  ShowProgress := ZspFull;
  CurZip.Guage.SetCount(Count);
  CurZip.Guage.SetSize(USize);
  if not CurZip.IsOpen then
  begin
//    if CurZip.File_Reopen(FmOpenRead) < 0 then
    if CurZip.File_Reopen(FmOpenRead or fmShareDenyWrite) < 0 then
      Exit;
  end;
  SuccessCnt := 0;
  Stopped := False;
  Rec := CurZip.FirstSelected;
  while Rec <> nil do
  begin
    CheckCancel;
    if Progress.Stop then
    begin
      Body.TraceFmt('User stopped Unzip, %d remain',
        [SelectedCount - SuccessCnt],
        {_LINE_}1493, __UNIT__);
      Stopped := True;
    end;
    if Stopped then
    begin
      Result := ZM_Error({_LINE_}1498, ZE_EntryCancelled);
      ReportSkipped(Rec.FileName, StUser, Result);
      Rec := CurZip.NextSelected(Rec);
      Continue;
    end;

    DestName := '';
    Testing := ExtrTest in Body.ExtrOptions;
    if Testing then
      Result := TestEntry(Rec)
    else
    begin
      if Rec.SelectArgs is TZMUnzStreamArg then
      begin
        Strm := TZMUnzStreamArg(Rec.SelectArgs).TheStream;
        Result := UnzipAStream(Strm, Rec);
        if Strm is TZMCanal then
        begin
          TZMUnzStreamArg(Rec.SelectArgs).TheStream := nil;
          if TZMCanal(Strm).Owned then
            Strm.Free;
        end;
      end
      else
        Result := UnzipAFile(DestName, Rec, Rec.SelectArgs as TZMUnzOpts);
    end;
    if Result = 0 then
    begin
      // success
      SuccessCnt := SuccessCnt + 1;
      if Testing then
        IncludeSpecs.Add(Rec.FileName)
      else
        IncludeSpecs.Add(DestName);
    end
    else
    begin
      Body.InformFmt('Extraction of %s failed [%d]', [ZRec.FileName, Result],
        {_LINE_}1536, __UNIT__);
      Skip := CanSkip(Rec.FileName, Result);
      if Skip <> StNoSkip then
        Result := 0 // skipping permitted
      else
        Break;
    end;
    Rec := CurZip.NextSelected(Rec);
  end;
  CurZip.File_Close;
  CurZip.Guage.EndBatch;
end;

function TZMUnzipOpr.UnzipTheEntry(const DestFileName: string;
  Options: TZMUnzOpts): Integer;
var
  Err: Integer;
  Writer: TZMZipBase;
begin
  // create out file
  Writer := TZMZipBase.Create(Body);
  try
    if not Writer.File_Create(DestFileName) then
      Result := Body.PrepareErrMsg(ZE_FileCreate, [DestFileName],
        {_LINE_}1560, __UNIT__)
    else
    begin
      Writer.StampDate := ZRec.DateTime;
      Writer.FileAttrs := ZRec.ExtFileAttrib;

      // undeflate
      FExtractor.OutStream := Writer.Stream;
      Result := FExtractor.Decompress;
      Writer.File_Close;
      if Result >= 0 then
        Result := CheckCRC(FExtractor.CRC, ZRec.CRC32, ZRec.FileName);
      if Result = 0 then
        Result := FinaliseExtracted(Writer, DestFileName, Options.TFlag)
      else
      begin
        Err := AbsErr(Result);
        if (Err = ZE_BadCRC) or (Err = ZE_EntryCancelled) then
          Writer.IsTemp := True; // cause it to be deleted
      end;
    end;
  finally
    Writer.Free;
    ZReader.Guage.EndItem;
  end;
end;

function TZMUnzipOpr.UnzipToFile(const DestName: string;
  const ExtRec: TZMDirEntry): Integer;
var
  DName: string;
  Options: TZMUnzOpts;
  WasOpen: Boolean;
begin
  Body.Trace('UnzipToFile', {_LINE_}1594, __UNIT__);
  SuccessCnt := 0;
  if not(ExtRec is TZMExtEntry) then
  begin
    Result := ZM_Error({_LINE_}1598, ZE_UnknownError);
    Exit;
  end;

  ShowProgress := ZspFull;
  WasOpen := False;
  ZRec := TZMExtEntry(ExtRec).MyRec;
  Options := TZMUnzOpts.Create;
  try
    DefaultOptions(Options);
    ZReader.Guage.SetCount(1);
    ZReader.Guage.SetSize(ZRec.CompressedSize);
    WasOpen := ZReader.IsOpen;
    DName := DestName;
    Result := UnzipAFile(DName, nil, Options);
  finally
    Options.Free;
    if not WasOpen then
      ZReader.File_Close;
    ZReader.Guage.EndItem;
    ZReader.Guage.EndBatch;
  end;
  if Result = 0 then
    SuccessCnt := 1;
  if Result <> 0 then
    ShowError(Result);
end;

function TZMUnzipOpr.UnzipToStream(DestStream: TStream;
  const ExtRec: TZMDirEntry): Integer;
var
  WasOpen: Boolean;
begin
  Body.Trace('UnzipToStream', {_LINE_}1631, __UNIT__);
  SuccessCnt := 0;
  if not(ExtRec is TZMExtEntry) then
  begin
    Result := ZM_Error({_LINE_}1635, ZE_UnknownError);
    Exit;
  end;

  ZRec := TZMExtEntry(ExtRec).MyRec;

  if DestStream = nil then
    DestStream := Body.ZipStream;

  ShowProgress := ZspFull;
  ZReader.Guage.SetCount(1);
  ZReader.Guage.SetSize(ZRec.UncompressedSize);
  WasOpen := ZReader.IsOpen;
  Result := UnzipAStream(DestStream, nil);
  ZReader.Guage.EndBatch;
  if not WasOpen then
    ZReader.File_Close;
  if Result = 0 then
    SuccessCnt := 1;
end;

// return <0 _ user skip, 0 _ no change, >0 _ changed
function TZMUnzipOpr.UpdateName(var DestFileName: string;
  const BasePath: string; const Options: TZMUnzOpts): Integer;
var
  Old: string;
  Sep: Integer;
  Subst: string;
  Tmp: string;
begin
  Result := 0; // no change
  if not Options.NFlag then
  begin
    Tmp := DestFileName;
    Result := DoSetExtNameEvent(BasePath, Tmp);
    if Result < 0 then
      Exit; // user skip entry
    DestFileName := Tmp;
  end;
  if Options.XArg <> '' then
  begin
    Old := Options.XArg;
    Sep := Pos('::', Old);
    Subst := Copy(Old, Sep + 2, 2048);
    Subst := SetSlash(Subst, PsdExternal);
    Old := Copy(Old, 1, Sep - 1);
    if Old = '' then
    begin
      DestFileName := Subst + DestFileName;
      Result := 1; // changed
    end
    else
    begin
      Old := SetSlash(Old, PsdExternal);
      Tmp := StringReplace(DestFileName, Old, Subst,
        [RfReplaceAll, RfIgnoreCase]);
      if CompareStr(Tmp, Old) <> 0 then
      begin
        DestFileName := Tmp;
        Result := 1; // changed
      end;
    end;
  end;
end;

// if changed makes new options set, returns true
function TZMUnzipOpr.UpdateOptionsFromSplitter(var Args: TZMUnzOpts;
  const ParentExcludes: string): Boolean;
var
  DOSDate: Cardinal;
  Flag: Boolean;
  OvrOpt: TZMMergeOpts;
  S: string;
  Xc: string;
begin
  Result := False;
  // Result is true when Args have been cloned (only clone once)
  if FSplitter.Has('<') then
  begin
    S := FSplitter.Arg('<');
    if CompareStr(S, Args.Password) <> 0 then
    begin
      Args := Args.Cloned;
      Result := True;
      Args.Password := S;
    end;
  end;

  if FSplitter.Has('E') then
  begin
    Xc := FSplitter.Arg('E');
    if (Xc <> '') and (Xc[1] = SPEC_SEP) then
    begin
      S := ParentExcludes;
      if Length(Xc) <> 1 then
        S := S + Xc; // only if not '|' and not empty append to globals
    end
    else
      S := Xc; // new (even if empty)
    if CompareStr(S, Args.Excludes) <> 0 then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.Excludes := S;
    end;
  end;

  if FSplitter.Has('F') then
  begin
    S := FSplitter.Arg('F');
    if CompareStr(S, Args.Folder) <> 0 then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.Folder := S;
    end;
  end;

  if FSplitter.Has('O') then
  begin
    Xc := FSplitter.Arg('O');
    case Xc[1] of
      'A':
        OvrOpt := ZmoAlways;
      'N':
        OvrOpt := ZmoNever;
      'Y':
        OvrOpt := ZmoNewer;
      'O':
        OvrOpt := ZmoOlder;
    else
      OvrOpt := ZmoConfirm;
    end;
    if OvrOpt <> Args.OvrOpt then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.OvrOpt := OvrOpt;
    end;
  end;

  if FSplitter.Has('N') then
  begin
    Flag := FSplitter.Arg('N') = '+';
    if Flag <> Args.NFlag then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.NFlag := Flag;
    end;
  end;

  if FSplitter.Has('T') then
  begin
    Flag := FSplitter.Arg('T') = '+';
    if Flag <> Args.TFlag then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.TFlag := Flag;
    end;
  end;

  if FSplitter.Has('J') then
  begin
    Flag := FSplitter.Arg('J') = '+';
    if Flag <> Args.JunkDir then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.JunkDir := Flag;
    end;
  end;

  if FSplitter.Has('D') then
  begin
    DOSDate := Args.DOSDate;
    Flag := Args.Before;
    Xc := FSplitter.Arg('D');
    if Xc <> '' then
    begin
      Flag := Xc[1] = '<';
      DOSDate := Cardinal(StrToInt(Copy(Xc, 2, 64)));
    end;
    if (Flag <> Args.Before) or (DOSDate <> Args.DOSDate) then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.Before := Flag;
      Args.DOSDate := DOSDate;
    end;
  end;

  if FSplitter.Has('X') then
  begin
    S := FSplitter.Arg('X');
    if CompareStr(S, Args.XArg) <> 0 then
    begin
      if not Result then
        Args := Args.Cloned;
      Result := True;
      Args.XArg := S;
    end;
  end;
end;

function TZMUnzOpts.Accept(Rec: TZMEntryBase): Boolean;
begin
  Result := True;
  if DOSDate <> 0 then
  begin
    if Before then
      Result := DOSDate > Rec.ModifDateTime
    else
      Result := DOSDate <= Rec.ModifDateTime;
  end;
end;

procedure TZMUnzOpts.Assign(Other: TZMSelectArgs);
var
  Tmp: TZMUnzOpts;
begin
  inherited;
  if Other is TZMUnzOpts then
  begin
    Tmp := TZMUnzOpts(Other);
    FBefore := Tmp.FBefore;
    FDOSDate := Tmp.FDOSDate;
    FExcludes := Tmp.FExcludes;
    FFolder := Tmp.Folder;
    FJunkDir := Tmp.FJunkDir;
    FNFlag := Tmp.FNFlag;
    FOvrOpt := Tmp.FOvrOpt;
    FPassword := Tmp.Password;
    FTFlag := Tmp.FTFlag;
    FXArg := Tmp.FXArg;
  end;
end;

function TZMUnzOpts.Cloned: TZMUnzOpts;
begin
  Result := TZMUnzOpts.Create;
  Result.Assign(Self);
end;

constructor TZMUnzStreamArg.Create(AStream: TStream);
begin
  inherited Create;
  FTheStream := AStream;
end;

function TZMUnzStreamArg.Accept(Rec: TZMEntryBase): Boolean;
begin
  Result := TheStream <> nil;
end;

procedure TZMUnzStreamArg.BeforeDestruction;
begin
  if (TheStream is TZMCanal) and (TZMCanal(TheStream).Owned) then
    FTheStream.Free;
  inherited;
end;

end.
