unit ZMOprDLL;

// ZMDLLOpr.pas - DLL operations and functions

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

interface

{$INCLUDE    '.\ZipVers.inc'}

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZMHandler;
// {$DEFINE ZDEBUG}

type
  TZMDLL = class(TZMOperationRoot)
  public
    procedure AbortDLL;
  end;

type
  TZMOpAddStreamToFile = class(TZMDLL)
  private
    FFileAttr: Dword;
    FFileDate: Dword;
    FFileName: string;
  public
    constructor Create(const FileName: string; FileDate, FileAttr: Dword);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpAdd = class(TZMDLL)
  public
    constructor Create;
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, VCL.Controls, VCL.Graphics, VCL.Dialogs,
{$ELSE}
  SysUtils, Controls, Graphics, Dialogs,
{$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  Forms, ZipMstr, ZMDelZip, ZMBody, ZMBaseOpr, ZMXcpt, ZMFileOpr, ZMLister,
  ZMMsg, ZMUtils, ZMDrv, ZMStructs, ZMUTF8, ZMZipReader, ZMDLLLoad, ZMWinFuncs,
  ZMZipBase, ZMZipWriter, ZMCore, ZMCommand;

const
  __UNIT__ = 28;

type
  TZMDLLOpr = class;

  TDZCallback = class
  private
    FHoldSize: Integer;
    PCB: PZCallBackStruct;
    function GetActionCode: Integer;
    function GetArg1: Cardinal;
    function GetArg2: Cardinal;
    function GetArg3: Integer;
    function GetFile_Size: Int64;
    function GetIsZip: Boolean;
    function GetMsg: string;
    function GetMsg2: string;
    function GetOwner: TZMDLLOpr;
    function GetWritten: Int64;
    procedure SetArg1(const Value: Cardinal);
    procedure SetArg2(const Value: Cardinal);
    procedure SetArg3(const Value: Integer);
    procedure SetFile_Size(const Value: Int64);
    procedure SetMsg(const Value: string);
  protected
    FHeldData: PByte;
    function Assign(ZCallBackRec: PZCallBackStruct): Integer;
    function CopyData(Dst: PByte; MaxSize: Integer): Boolean;
    function GetMsgStr(const Msg: PByte): string;
    function HoldData(const Src: PByte; Size: Cardinal): PByte;
    function HoldString(const Src: string): PByte;
    procedure SetComment(const AStr: AnsiString);
    procedure SetData(Src: PByte; Size: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear;
    property ActionCode: Integer read GetActionCode;
    property Arg1: Cardinal read GetArg1 write SetArg1;
    property Arg2: Cardinal read GetArg2 write SetArg2;
    property Arg3: Integer read GetArg3 write SetArg3;
    property File_Size: Int64 read GetFile_Size write SetFile_Size;
    property IsZip: Boolean read GetIsZip;
    property Msg: string read GetMsg write SetMsg;
    property Msg2: string read GetMsg2;
    property Owner: TZMDLLOpr read GetOwner;
    property Written: Int64 read GetWritten;
  end;

  TZMDLLOpr = class(TZMFileOpr)
  private
    FAutoAttr: Cardinal;
    FAutoDate: Cardinal;
    FCB: TDZCallback;
    FDLLOperKey: Cardinal;
    FDLLTargetName: string;
    FEventErr: string;
    // 1 data for dll held until next callback or fini
    FHeldData: Pointer;
    FIsDestructing: Boolean;
    Warnings: Integer;
    function DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
    function DLLToErrCode(DLL_error: Integer): Integer;
    procedure DLL_Comment(var Result: Integer);
    procedure DLL_Data(var Result: Integer);
    procedure DLL_ExtName(var Result: Integer);
    procedure DLL_Message(var Result: Integer);
    procedure DLL_Password(var Result: Integer);
    procedure DLL_Progress(Action: TActionCodes; var Result: Integer);
    procedure DLL_SetAddName(var Result: Integer);
    procedure DLL_Skipped(var Result: Integer);
    function GetAddCompLevel: Integer;
    function GetAddFrom: TDateTime;
    function GetAddOptions: TZMAddOpts;
    function GetAddStoreSuffixes: TZMAddStoreExts;
    function GetDLL_Load: Boolean;
    function GetExtAddStoreSuffixes: string;
    function GetPassword: string;
    function GetPasswordReqCount: Integer;
    function GetRootDir: string;
    function GetZipStream: TMemoryStream;
    function IsDestWritable(const Fname: string; AllowEmpty: Boolean): Boolean;
    function JoinMVArchive(var TmpZipName: string): Integer;
    function RecreateMVArchive(const TmpZipName: string; Recreate: Boolean):
        Boolean;
    procedure SetAddOptions(const Value: TZMAddOpts);
    procedure SetCB(const Value: TDZCallback);
    procedure SetDLL_Load(const Value: Boolean);
    procedure SetPasswordReqCount(const Value: Integer);
    procedure SetRootDir(const Value: string);
  protected
    FAutoStream: TStream;
    function Add: Integer;
    function AddStoreExtStr(Options: TZMAddStoreExts): string;
    function AddStreamToFile(const FileName: string;
      FileDate, FileAttr: Dword): Integer;
    function AllocDLLCommand(const FileName: string): PDLLCommands;
    procedure CancelSet(Value: Integer);
    procedure DestroyDLLCmd(var Rec: PDLLCommands);
    function DLLCallback(ZCallBackRec: PZCallBackStruct): Integer;
    function DLLStreamOp(Op: TZStreamActions; ZStreamRec: PZStreamRec): Integer;
    procedure DLL_Arg(var Result: Integer);
    procedure ExtAdd;
    function SetupZipCmd(const Value: string): PDLLCommands;
    property AddCompLevel: Integer read GetAddCompLevel;
    property AddFrom: TDateTime read GetAddFrom;
    property AddStoreSuffixes: TZMAddStoreExts read GetAddStoreSuffixes;
    property CB: TDZCallback read FCB write SetCB;
    property DLLTargetName: string read FDLLTargetName write FDLLTargetName;
    property ExtAddStoreSuffixes: string read GetExtAddStoreSuffixes;
    property Password: string read GetPassword;
    property PasswordReqCount: Integer read GetPasswordReqCount
      write SetPasswordReqCount;
    property RootDir: string read GetRootDir write SetRootDir;
  public
    procedure AbortDLL;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property AddOptions: TZMAddOpts read GetAddOptions write SetAddOptions;
    property DLL_Load: Boolean read GetDLL_Load write SetDLL_Load;
    property ZipStream: TMemoryStream read GetZipStream;
  end;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

(* ? ZCallback
  1.76 01 May 2004 RP change return type and value to return flag for exception
 1.76 24 April 2004 RP use DLLCallback
 1.73 ( 1 June 2003) changed for new callback
 { Dennis Passmore (Compuserve: 71640,2464) contributed the idea of passing an
 instance handle to the DLL, and, in turn, getting it back from the callback.
 This lets us referance variables in the TZMDLLOpr class from within the
 callback function.  Way to go Dennis!
 Modified by Russell Peters }
*)
function ZCallback(ZCallBackRec: PZCallBackStruct): Longint; stdcall;
begin
  Result := CALLBACK_ERROR;
  if ZCallBackRec^.Check = ZCallBack_Check then
  begin
    with TObject(ZCallBackRec^.Caller) as TZMDLLOpr do
      Result := DLLCallback(ZCallBackRec);
  end;
end;

function ZStreamCallback(ZStreamRec: PZStreamRec): Longint; stdcall;
var
  Cnt: Integer;
  Op: TZStreamActions;
  Strm: TStream;
begin
  Result := CALLBACK_ERROR;
  try
    if ZStreamRec^.Check = ZStream_Check then
    begin
      with ZStreamRec^ do
      begin
        Op := TZStreamActions(OpCode);
        Result := 0;
        case Op of
          ZsaIdentify .. ZsaClose:
            with TObject(ZStreamRec^.Caller) as TZMDLLOpr do
              Result := DLLStreamOp(Op, ZStreamRec);
          ZsaPosition: // reposition
            begin
{$IFNDEF VERD6up}
              if Integer(ArgLL) <> ArgLL then
              begin
                Strm := TObject(StrmP) as TStream;
                ArgLL := Strm.Seek(ArgLL, Word(TSeekOrigin(ArgI)));
                if ArgLL >= 0 then
                  Result := CALLBACK_TRUE;
              end;
{$ELSE}
              Strm := TObject(StrmP) as TStream;
              ArgLL := Strm.Seek(ArgLL, TSeekOrigin(ArgI));
              if ArgLL >= 0 then
                Result := CALLBACK_TRUE;
{$ENDIF}
            end;
          ZsaRead: // read
            begin
              Strm := TObject(StrmP) as TStream;
              Cnt := ArgI;
              if (Strm.Position + Cnt) > Strm.Size then
                Cnt := Integer(Strm.Size - Strm.Position);
              ArgI := Strm.Read(BufP^, Cnt);
              if ArgI = Cnt then
                Result := CALLBACK_TRUE;
            end;
          ZsaWrite: // Write
            begin
              Strm := TObject(StrmP) as TStream;
              Cnt := ArgI;
              ArgI := Strm.Write(BufP^, Cnt);
              if ArgI = Cnt then
                Result := CALLBACK_TRUE;
            end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      // clear any exceptions
      Result := CALLBACK_ERROR;
    end;
  end;
end;

procedure TZMDLLOpr.AbortDLL;
begin
  if FDLLOperKey <> 0 then
    _DLL_Abort(Master, FDLLOperKey);
end;

function TZMDLLOpr.Add: Integer;
begin
  if Body.Logging then
    Body.LogSpecs('');
  FAutoStream := nil;
  ExtAdd;
  Result := Errors.Code;
end;

function TZMDLLOpr.AddStoreExtStr(Options: TZMAddStoreExts): string;
const
  SuffixStrings: array [TZMAddStoreSuffixEnum] of PChar = ('gif', 'png', 'z',
    'zip', 'zoo', 'arc', 'lzh', 'arj', 'taz', 'tgz', 'lha', 'rar', 'ace', 'cab',
    'gz', 'gzip', 'jar', 'exe', '', 'jpg', 'jpeg', '7zp', 'mp3', 'wmv', 'wma',
    'dvr-ms', 'avi');
var
  O: TZMAddStoreSuffixEnum;
begin
  Result := '';
  for O := low(TZMAddStoreSuffixEnum) to high(TZMAddStoreSuffixEnum) do
    if (O <> AssEXT) and (O in Options) then
      Result := Result + '.' + string(SuffixStrings[O]) + ':';
  if AssEXT in Options then
    Result := Result + ExtAddStoreSuffixes;
end;

(* ? TZMDLLOpr.AddStreamToFile
  // 'FileName' is the name you want to use in the zip file to
 // store the contents of the stream under.
*)
function TZMDLLOpr.AddStreamToFile(const FileName: string;
  FileDate, FileAttr: Dword): Integer;
var
  FatDate: Word;
  FatTime: Word;
  Fn: string;
  Ft: TFileTime;
  St: TSystemTime;
begin
  Fn := Trim(FileName);
  if (Length(Fn) = 0) and (IncludeSpecs.Count > 0) then
    Fn := Trim(IncludeSpecs[0]);

  // Edwin:
  // - Fixed: Allow empty stream (Size = 0) to be added to the target .zip archive
  // if (Fn = '') or (ZipStream.Size = 0) then
  if Fn = '' then
  // Edwin end
  begin
    Result := ZM_Error({_LINE_}376, ZE_NothingToZip);
    Exit;
  end;

  // Edwin:
  // - Fixed: AddStreamToFile should not call DriveFolders.ExpandPath, otherwise you won't be able to
  //   compress a file into the target zip archive into a relative path such as
  //   `MyFolder1\MyFile1.txt' (relative to the root of the zip file).
  //  Result := DriveFolders.ExpandPath(Fn, Fn);
  //  if Result < 0 then
  //    Exit;
  Result := 0;
  // Edwin end

  // strip drive etc like 1.79
  if ExtractFileDrive(Fn) <> '' then
    Fn := Copy(Fn, 3, Length(Fn) - 2);
  if (Fn <> '') and ((Fn[1] = '/') or (Fn[1] = '\')) then
    Fn := Copy(Fn, 2, Length(Fn) - 1);
  if NameIsBad(Fn, False) then
  begin
    Result := Body.PrepareErrMsg(ZE_BadFileName, [Fn], {_LINE_}385, __UNIT__);
    ShowError(Result);
  end;
  if Result = 0 then
  begin
    Body.ClearIncludeSpecs;

    IncludeSpecs.Add('0:' + Fn);
    if FileDate = 0 then
    begin
      GetLocalTime(St);
      SystemTimeToFileTime(St, Ft);
      FileTimeToDosDateTime(Ft, FatDate, FatTime);
      FileDate := (Dword(FatDate) shl 16) + FatTime;
    end;
    FAutoStream := ZipStream;
    FAutoDate := FileDate;
    FAutoAttr := FileAttr;
    ExtAdd;
    Result := -Errors.ExtCode; // ????
  end;
end;

procedure TZMDLLOpr.AfterConstruction;
begin
  inherited;
  FDLLOperKey := 0;
  FHeldData := nil;
  FCB := TDZCallback.Create;
end;

function TZMDLLOpr.AllocDLLCommand(const FileName: string): PDLLCommands;
var
  Opts: Cardinal;
begin
  Result := AllocMem(SizeOf(TDLLCommands));
  DLLTargetName := FileName;
  ZeroMemory(Result, SizeOf(TDLLCommands));
  Result^.FVersion := DELZIPVERSION; // version we expect the DLL to be
  Result^.FCaller := Self; // point to our VCL instance; returned in Report

  Result^.ZCallbackFunc := ZCallback;
  // pass addr of function to be called from DLL
  Result^.ZStreamFunc := ZStreamCallback;
  Result^.FEncodedAs := Ord(Lister.Encoding); // how to interpret existing names
  Result^.FFromPage := Lister.Encoding_CP;

  if Verbosity >= ZvTrace then
    Result^.FVerbosity := -1
  else
    if Verbosity >= ZvVerbose then
      Result^.FVerbosity := 1
    else
      Result^.FVerbosity := 0;
  { if tracing, we want verbose also }

  // used for dialogs (like the pwd dialogs)
  if Unattended then
    Result^.FHandle := 0
  else
    Result^.FHandle := Master.Handle;
  Result^.FSS := nil;

  Opts := DLL_OPT_Quiet; // no DLL error reporting

  Result^.FOptions := Opts;
end;

procedure TZMDLLOpr.BeforeDestruction;
begin
  FIsDestructing := True; // stop callbacks
  AbortDLL;
  if FHeldData <> nil then
  begin
    FreeMem(FHeldData); // release held data
    FHeldData := nil;
  end;
  FreeAndNil(FCB);
  inherited;
end;

procedure TZMDLLOpr.CancelSet(Value: Integer);
begin
  AbortDLL; // is this too soon
end;

procedure TZMDLLOpr.DestroyDLLCmd(var Rec: PDLLCommands);
begin
  if Rec <> nil then
  begin
    FreeMem(Rec);
    Rec := nil;
  end;
end;

(* ? TZMDLLOpr.DLLCallback
*)
function TZMDLLOpr.DLLCallback(ZCallBackRec: PZCallBackStruct): Integer;
var
  Action: TActionCodes;
begin
  Result := CALLBACK_UNHANDLED;
  if FIsDestructing then // in destructor return
  begin
    Exit;
  end;
  CB.Assign(ZCallBackRec);
  Action := TActionCodes(CB.ActionCode and 63);
  try
    case Action of
      ZacMessage:
        DLL_Message(Result);
      ZacItem .. ZacXProgress:
        DLL_Progress(Action, Result);
      ZacNewName:
        // request for a new path+name just before zipping or extracting
        DLL_SetAddName(Result);
      ZacPassword:
        // New or other password needed during Extract()
        DLL_Password(Result);
      ZacCRCError:
        ;
      ZacOverwrite:
        ;
      ZacSkipped:
        // Extract(UnZip) and Skipped
        DLL_Skipped(Result);
      ZacComment:
        // Add(Zip) FileComments.
        DLL_Comment(Result);
      ZacData:
        // Set Extra Data
        DLL_Data(Result);
      ZacExtName:
        // request for a new path+name just before zipping or extracting
        DLL_ExtName(Result);
      ZacKey:
        begin
          FDLLOperKey := CB.Arg1;
          Result := 0;
        end;
      ZacArg:
        DLL_Arg(Result);
    else
      Result := CALLBACK_IGNORED; // unknown
    end; { end case }
    if (Action < ZacKey) and (Action > ZacMessage) then
    begin
      KeepAlive;
    end;
    if Cancel <> 0 then
    begin
      Result := CALLBACK_CANCEL;
      if Body.Logging then
        Body.Log(ZM_Error({_LINE_}539, 0), '[CANCEL sent]');
    end;
  except
    on E: Exception do
    begin
      if FEventErr = '' then
        // catch first exception only
        FEventErr := ' #' + IntToStr(Ord(Action)) + ' "' + E.Message + '"';
      Cancel := ZE_Except;
      Result := CALLBACK_EXCEPTION;
      if Body.Logging then
        Body.Log(ZM_Error({_LINE_}550, 0), '[CALLBACK Exception sent] ' +
          FEventErr);
    end;
  end;
end;

function TZMDLLOpr.DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
var
  Strm: TStream;
begin
  Result := CALLBACK_UNHANDLED;
  if TObject(ZStreamRec^.StrmP) is TStream then
  begin
    Strm := TStream(ZStreamRec^.StrmP);
    if Strm = ZipStream then
    begin
      FAutoStream := nil;
      ZStreamRec^.StrmP := nil;
      Result := CALLBACK_TRUE;
    end;
  end;
end;

function TZMDLLOpr.DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
begin
  Result := CALLBACK_UNHANDLED;
  ZStreamRec^.StrmP := nil;
  if Assigned(FAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.StrmP := FAutoStream;
    FAutoStream.Position := 0;
  end;
end;

function TZMDLLOpr.DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
begin
  Result := CALLBACK_UNHANDLED;
  if Assigned(FAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.ArgLL := FAutoStream.Size;
    ZStreamRec^.ArgD := FAutoDate;
    ZStreamRec^.ArgA := FAutoAttr;
  end;
end;

// ALL interface structures BYTE ALIGNED
(* stream operation arg usage
  zacStIdentify,
 //      IN BufP = name
 IN Number = number
 OUT ArgLL = Size, ArgD = Date, ArgA = Attrs
 zacStCreate,
 //      IN BufP = name
 IN Number = number
 OUT StrmP = stream
 zacStClose,
 IN Number = number
 IN StrmP = stream
 OUT StrmP = stream (= NULL)
 zacStPosition,
 IN Number = number
 IN StrmP = stream, ArgLL = offset, ArgI = from
 OUT ArgLL = position
 zacStRead,
 IN Number = number
 IN StrmP = stream, BufP = buf, ArgI = count
 OUT ArgI = bytes read
 zacStWrite
 IN Number = number
 IN StrmP = stream, BufP = buf, ArgI = count
 OUT ArgI = bytes written
*)
function TZMDLLOpr.DLLStreamOp(Op: TZStreamActions;
  ZStreamRec: PZStreamRec): Integer;
begin
  Result := CALLBACK_UNHANDLED;
  case Op of
    ZsaIdentify: // get details for named stream
      Result := DLLStreamIdentify(ZStreamRec);
    ZsaCreate: // Assign a stream
      Result := DLLStreamCreate(ZStreamRec);
    ZsaClose: // defaults to freeing stream if not ZipStream
      Result := DLLStreamClose(ZStreamRec);
  end;
  Body.TraceFmt('Stream operation %d on %d returns %d',
    [Ord(Op), ZStreamRec^.Number, Result], {_LINE_}637, __UNIT__);
end;

// return proper ErrCode for dll error
function TZMDLLOpr.DLLToErrCode(DLL_error: Integer): Integer;
begin
  Result := DLL_error and $3F;
  if Result <> 0 then
    Result := ZD_GOOD + Result;
  if Result > ZD_SKIPPED then
    Result := ZD_ERROR;
end;

(* Arg1 = argument
  0 = filename
 1 = password
 2 = RootDir
 3 = ExtractDir
 4 = Zip comment
 5 = FSpecArgs      Arg3 = Index
 6 = FSpecArgsExcl  Arg3 = Index
*)
procedure TZMDLLOpr.DLL_Arg(var Result: Integer);
var
  Arg: TCBArgs;
  Idx: Integer;
  Sr: string;
begin
  if CB.Arg1 <= Cardinal(Ord(high(TCBArgs))) then
  begin
    Arg := TCBArgs(CB.Arg1);
    Idx := CB.Arg3;
    Sr := '';
    if (Arg in [ZcbFSpecArgs, ZcbFSpecArgsExcl]) and (Idx < 0) then
      Result := CALLBACK_ERROR
    else
      if Arg = ZcbComment then
      begin // always Ansi
        CB.SetComment(Lister.ZipComment);
        Result := CALLBACK_TRUE;
      end
      else
      begin
        Result := CALLBACK_TRUE;
        case Arg of
          ZcbFilename:
            Sr := DLLTargetName;
          ZcbPassword:
            Sr := Password;
          ZcbRootDir:
            Sr := RootDir;
          ZcbExtractDir:
            ; // sr := ExtrBaseDir;
          ZcbFSpecArgs:
            begin
              if Idx >= IncludeSpecs.Count then
                Result := CALLBACK_UNHANDLED
              else
                Sr := IncludeSpecs[Idx];
              CB.Arg3 := IncludeSpecs.Count;
            end;
          ZcbFSpecArgsExcl:
            begin
              if Idx >= ExcludeSpecs.Count then
                Result := CALLBACK_UNHANDLED
              else
                Sr := ExcludeSpecs[Idx];
              CB.Arg3 := ExcludeSpecs.Count;
            end;
          ZcbSpecials:
            Sr := AddStoreExtStr(AddStoreSuffixes);
          ZcbTempPath:
            Sr := Lister.TempDir;
        end;
        CB.Msg := Sr;
      end;
  end
  else
    Result := CALLBACK_ERROR;
end;

procedure TZMDLLOpr.DLL_Comment(var Result: Integer);
var
  FileComment: string;
  IsChanged: Boolean;
  Ti: Integer;
  TmpFileComment: TZMFileCommentEvent;
begin
  TmpFileComment := Master.OnFileComment;
  if Assigned(TmpFileComment) then
  begin
    FileComment := CB.Msg2;
    IsChanged := False;
    TmpFileComment(Master, CB.Msg, FileComment, IsChanged);
    if IsChanged then
    begin
      Result := CALLBACK_TRUE;
      Ti := Length(FileComment);
      if Ti > 255 then
      begin
        Ti := 255;
        FileComment := Copy(FileComment, 1, 255);
      end;
      CB.Msg := FileComment;
      CB.Arg1 := Ti;
    end;
  end;
  if (Cancel <> 0) and (Result >= CALLBACK_IGNORED) then
    Result := CALLBACK_CANCEL;
end;

procedure TZMDLLOpr.DLL_Data(var Result: Integer);
var
  Dat: TZMRawBytes;
  DataChanged: Boolean;
  DatSize: Int64;
  IsChanged: Boolean;
  LevelChanged: Boolean;
  Lvl: Integer;
  TmpFileExtra: TZMFileExtraEvent;
  TmpSetCompLevel: TZMSetCompLevel;
  Xlen: Integer;
begin
  TmpFileExtra := Master.OnFileExtra;
  TmpSetCompLevel := Master.OnSetCompLevel;
  LevelChanged := False;
  DataChanged := False;
  if Assigned(TmpSetCompLevel) then
  begin
    IsChanged := False;
    Lvl := Integer(CB.Arg2);
    TmpSetCompLevel(Master, CB.Msg, Lvl, IsChanged);
    if IsChanged and (Lvl in [0 .. 9]) then
    begin
      CB.Arg2 := Lvl;
      LevelChanged := True;
    end;
  end;
  if Assigned(TmpFileExtra) then
  begin
    DatSize := CB.Arg1; // old Size
    SetLength(Dat, DatSize);
    if DatSize > 0 then
      CB.CopyData(PByte(@Dat[1]), DatSize);
    IsChanged := False;
    TmpFileExtra(Master, CB.Msg, Dat, IsChanged);
    if IsChanged then
    begin
      DataChanged := True;
      Xlen := Length(Dat);
      if Xlen > 2047 then // limit
        Xlen := 2047;
      CB.SetData(PByte(@Dat[1]), Xlen);
    end;
  end;
  if DataChanged then
  begin
    if LevelChanged then
      Result := CALLBACK_3
    else
      Result := CALLBACK_TRUE;
  end
  else
  begin
    if LevelChanged then
      Result := CALLBACK_2;
  end;
end;

procedure TZMDLLOpr.DLL_ExtName(var Result: Integer);
var
  BaseDir: string;
  IsChanged: Boolean;
  Msg: string;
  OldFileName: string;
  TmpSetExtName: TZMSetExtNameEvent;

  function IsPathOnly(const F: string): Boolean;
  var
    C: Char;
  begin
    Result := False;
    if F <> '' then
    begin
      C := F[Length(F)];
      if (C = PathDelim) or (C = PathDelimAlt) then
        Result := True;
    end;
  end;

begin
  TmpSetExtName := Master.OnSetExtName;
  if Assigned(TmpSetExtName) then
  begin
    Msg := CB.Msg2;
    BaseDir := SetSlashW(Msg, PsdExternal);
    Msg := CB.Msg;
    OldFileName := Msg;
    IsChanged := False;
    TmpSetExtName(Master, OldFileName, BaseDir, IsChanged);
    if IsChanged and (OldFileName <> Msg) and
      (IsPathOnly(OldFileName) = IsPathOnly(Msg)) then
    begin
      CB.Msg := OldFileName;
      Result := CALLBACK_TRUE;
    end;
  end;
end;

procedure TZMDLLOpr.DLL_Message(var Result: Integer);
var
  ECode: Integer;
  Erm: string;
  ErrorCode: Integer;
  EType: Integer;
  ExtCode: Integer;
  Show: Boolean;
  TmpMessage: TZMMessageEvent;
begin
  Erm := CB.Msg;
  ErrorCode := CB.Arg1;
  ExtCode := 0;
  ECode := 0;
  EType := 0;
  if ErrorCode <> 0 then
  begin
    EType := ErrorCode and DZM_Type_Mask;
    if EType = DZM_Warning then
      Inc(Warnings);
    ExtCode := ErrorCode or $40000000;
    if ((ErrorCode and $FF) <> 0) and (EType >= DZM_Warning) and
      (EType > (Errors.ExtCode and DZM_Type_Mask)) then
      Errors.ExtCode := ExtCode; // remember last error

    ECode := DLLToErrCode(ErrorCode);
    if (EType >= DZM_Message) and ((ErrorCode and DZM_MessageBit) <> 0) then
      Erm := ZipLoadStr(ECode) + Erm;
    // W'll always keep the last ErrorCode
    if (ECode <> 0) and (Errors.Code = 0) then
    begin
      if (FEventErr <> '') and (ECode = _DZ_ERR_ABORT) then
        Erm := ZipFmtLoadStr(ZE_EventEx, [FEventErr]);
    end;
    Errors.ErrMessage := Erm;
  end;
  if Body.Logging then
    Body.Log(ExtCode, Erm);
  TmpMessage := Master.OnMessage;
  if Assigned(TmpMessage) then
  begin
    Show := False;
    case EType of
      DZM_General, DZM_Error, DZM_Warning, DZM_Message:
        Show := True;
      DZM_Verbose:
        if Verbosity >= ZvVerbose then
          Show := True;
      DZM_Trace:
        if Verbosity >= ZvTrace then
          Show := True;
    end;
    if Show then
    begin
      if ECode <> 0 then
        ECode := ZM_Error({_LINE_}901, ECode);
      TmpMessage(Master, ECode, Erm);
    end;
  end;
  KeepAlive; // process messages or check terminate
end;

procedure TZMDLLOpr.DLL_Password(var Result: Integer);
var
  IsZip: Boolean;
  Pwd: string;
  Response: TmsgDlgBtn;
  RptCount: Longword;
  TmpPasswordError: TZMPasswordErrorEvent;
begin
  Pwd := '';
  RptCount := CB.Arg1;
  Response := MbOK;
  IsZip := CB.IsZip;
  TmpPasswordError := Master.OnPasswordError;
  if Assigned(TmpPasswordError) then
  begin
    TmpPasswordError(Master, IsZip, Pwd, CB.Msg, RptCount, Response);
    if Response <> MbOK then
      Pwd := '';
  end
  else
    if IsZip then
      Pwd := Master.GetAddPassword(Response)
    else
      Pwd := Master.GetExtrPassword(Response);

  if Pwd <> '' then
  begin
    CB.Msg := Pwd;
    Result := CALLBACK_TRUE;
  end
  else
  begin // no password
    RptCount := 0;
    Result := CALLBACK_2;
  end;
  if RptCount > 15 then
    RptCount := 15;
  CB.Arg1 := RptCount;
  if Response = MbCancel then // Cancel
  begin
    Result := CALLBACK_2;
  end
  else
    if Response = MbNoToAll then // Cancel all
    begin
      Result := CALLBACK_3;
    end
    else
      if Response = MbAbort then // Abort
      begin
        Cancel := ZS_Abort;
        Result := CALLBACK_ABORT;
      end;
end;

procedure TZMDLLOpr.DLL_Progress(Action: TActionCodes; var Result: Integer);
begin
  case Action of
    ZacItem .. ZacEndOfBatch:
      Progress.Written(CB.Written);
  end;
  case Action of
    ZacTick:
      KeepAlive;
    ZacItem:
      Progress.NewItem(CB.Msg, CB.File_Size);
    ZacProgress:
      Progress.Advance(CB.File_Size);
    ZacEndOfBatch:
      Progress.EndBatch;
    ZacCount:
      Progress.TotalCount := CB.Arg1;
    ZacSize:
      Progress.TotalSize := CB.File_Size;
    ZacXItem:
      Progress.NewXtraItem(CB.Msg, CB.File_Size);
    ZacXProgress:
      Progress.AdvanceXtra(CB.File_Size);
  end;
  Result := 0;
  if (Action = ZacItem) and (CB.File_Size = -1) and (Progress.Stop) then
    Result := CALLBACK_TRUE;
end;

procedure TZMDLLOpr.DLL_SetAddName(var Result: Integer);
var
  IsChanged: Boolean;
  M: string;
  M2: string;
  OldFileName: string;
  OrigName: string;
  TmpSetAddName: TZMSetAddNameEvent;
begin
  TmpSetAddName := Master.OnSetAddName;
  if Assigned(TmpSetAddName) then
  begin
    M := CB.Msg; // saves OldFileName
    M2 := CB.Msg2;
    if Assigned(TmpSetAddName) then
    begin
      OrigName := SetSlashW(M2, PsdExternal);
      OldFileName := M;
      IsChanged := False;

      TmpSetAddName(Master, OldFileName, OrigName, IsChanged);
      if IsChanged then
      begin
        CB.Msg := OldFileName;
        Result := CALLBACK_TRUE;
      end;
    end;
  end;
end;

procedure TZMDLLOpr.DLL_Skipped(var Result: Integer);
var
  ErrorCode: Integer;
  Ti: Integer;
begin
  ErrorCode := CB.Arg1; // error
  if ErrorCode <> 0 then
    Errors.ExtCode := DLLToErrCode(ErrorCode);
  Ti := CB.Arg2; // type
  if Skipping(CB.Msg, TZMSkipTypes(Ti and 31), DLLToErrCode(ErrorCode)) then
    Result := CALLBACK_TRUE;
end;

procedure TZMDLLOpr.ExtAdd;
var
  CmdRecP: PDLLCommands;
  Curz: TZMZipReader;
  DestPath: string;
  MultiDisk: Boolean;
  Ret: Integer;
  TmpZipName: string;
begin
  Warnings := 0;
  // { Make sure we can't get back in here while work is going on }
  CmdRecP := nil;
  MultiDisk := ZwoDiskSpan in WriteOptions;
  // We can not do an Unattended Add if we don't have a password.
  if Unattended and (AddEncrypt in AddOptions) and (Password = '') then
    raise EZipMaster.CreateMsg(Body, ZE_UnattPassword, {_LINE_}1050, __UNIT__);
  try
    if ZipFileName = '' then // make sure we have a zip filename
      raise EZipMaster.CreateMsg(Body, ZE_NoZipSpecified, {_LINE_}1053,
        __UNIT__);
    if (IncludeSpecs.Count = 0) then
    begin
      if not((AddFreshen in AddOptions) or (AddUpdate in AddOptions)) then
        raise EZipMaster.CreateMsg(Body, ZE_NothingToZip, {_LINE_}1058,
          __UNIT__);
      AddOptions := (AddOptions - [AddUpdate]) + [AddFreshen];
      IncludeSpecs.Add(WILD_ALL); // do freshen all
    end;

    Curz := Current;
    if Curz.ArchiveName = '' then
      Curz.ArchiveName := ZipFileName;
    Curz.WorkDrive.HasMedia(False);
    // drive must exist and be changeable
    Ret := Curz.RefuseWriteSplit;
    if Ret <> 0 then
      raise EZipMaster.CreateMsg(Body, Ret, {_LINE_}1071, __UNIT__); // ****

    if (Curz.Count = 0) and ((AddFreshen in AddOptions)) then
      raise EZipMaster.CreateMsg(Body, ZE_NothingToZip, {_LINE_}1074, __UNIT__);

    // make certain destination can exist
    { We must allow a zipfile to be specified that doesn't already exist,
      so don't check here for existance. }
    if (Curz.WorkDrive.DriveIsFixed or not MultiDisk) then
    begin
      DestPath := ExtractFilePath(ZipFileName);
      if ZwoForceDest in WriteOptions then
        _Z_ForceDirectory(DestPath);
      if not _Z_DirExists(DestPath) then
        raise EZipMaster.CreateMsgFmt(Body, ZE_NoDestDir, [DestPath],
          {_LINE_}1086, __UNIT__);
    end;

    if not IsDestWritable(ZipFileName, MultiDisk) then
      raise EZipMaster.CreateMsgFmt(Body, ZE_NotChangeable, [ZipFileName],
        {_LINE_}1091, __UNIT__);

    if _DLL_Load(Lister) <= 0 then
      Exit; // could not load valid dll
    TmpZipName := ZipFileName; // default
    // If we are using disk spanning, first create a temporary file
    if (MultiDisk) then
    begin
      Ret := JoinMVArchive(TmpZipName);
      if Ret <> 0 then
      begin
        _DLL_Unload(Lister);
        raise EZipMaster.CreateMsg(Body, Errors.Code, {_LINE_}1103, __UNIT__);
        // ****
      end;
    end;
    if not MultiDisk and AnsiSameText(EXT_EXE, ExtractFileExt(ZipFileName)) and
      not FileExists(ZipFileName) then
    begin
      { This is the first "add" operation following creation of a new
        .EXE archive.  We need to add the SFX code now, before we add
       the files. }
      Ret := NewSFXFile(ZipFileName);
      if Ret <> 0 then
        raise EZipMaster.CreateMsgFmt(Body, ZE_AutoSFXWrong, [Ret],
          {_LINE_}1116, __UNIT__);
    end;
  except
    on Ews: EZipMaster do
    begin
      ShowExceptionError(Ews);
      Exit;
    end;
    else
      Exit;
  end;
  Cancel := 0;

  try
    try
      CmdRecP := SetupZipCmd(TmpZipName);
      FEventErr := ''; // added
      { pass in a ptr to parms }
      SuccessCnt := _DLL_Exec(Lister, CmdRecP, FDLLOperKey);
      FEventErr := ''; // added
      if MultiDisk then
      begin
        if (SuccessCnt < 0) or RecreateMVArchive(TmpZipName,
          (Lister.Count > 0) and ((AddFreshen in AddOptions) or
          (AddUpdate in AddOptions))) then
          File_Delete(TmpZipName);
      end;
    except
      on Ews: EZipMaster do
      begin
        if FEventErr <> '' then
          Ews.Message := Ews.Message + FEventErr;
        ShowExceptionError(Ews);
      end
      else
        ShowMessage(ZM_Error({_LINE_}1151, ZE_FatalZip), '');
    end;
  finally
    Body.ClearIncludeSpecs;
    Body.ClearExcludeSpecs;
    DestroyDLLCmd(CmdRecP);
  end; { end try finally }

  _DLL_Unload(Lister);
  Cancel := 0;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  if SuccessCnt > 0 then
    Reload := ZlrReload // force reload
  else
    Reload := ZlrClear;
  if (Errors.Code = 0) and (Warnings > 0) then
  begin
    Errors.ExtCode := ZM_Error({_LINE_}1169, ZD_WARNING);
    Errors.ErrMessage := 'Finished with ' + IntToStr(Warnings) + ' warnings';
    Body.Inform(Errors.ErrMessage, {_LINE_}1171, __UNIT__);
  end;
end;

function TZMDLLOpr.GetAddCompLevel: Integer;
begin
  Result := Lister.AddCompLevel
end;

function TZMDLLOpr.GetAddFrom: TDateTime;
begin
  Result := Lister.AddFrom
end;

function TZMDLLOpr.GetAddOptions: TZMAddOpts;
begin
  Result := Body.AddOptions;
end;

function TZMDLLOpr.GetAddStoreSuffixes: TZMAddStoreExts;
begin
  Result := Lister.AddStoreSuffixes
end;

function TZMDLLOpr.GetDLL_Load: Boolean;
begin
  Result := _DLL_Loaded(Master);
{$IFDEF ZDEBUG}
  Reporter.TraceFmt('DLL_Load = %d', [Ord(Result)], {_LINE_}1199, __UNIT__);
{$ENDIF}
end;

function TZMDLLOpr.GetExtAddStoreSuffixes: string;
begin
  Result := Lister.ExtAddStoreSuffixes
end;

function TZMDLLOpr.GetPassword: string;
begin
  Result := Lister.Password
end;

function TZMDLLOpr.GetPasswordReqCount: Integer;
begin
  Result := Lister.PasswordReqCount
end;

function TZMDLLOpr.GetRootDir: string;
begin
  Result := Lister.RootDir
end;

function TZMDLLOpr.GetZipStream: TMemoryStream;
begin
  Result := Body.ZipStream;
end;

function TZMDLLOpr.IsDestWritable(const Fname: string;
  AllowEmpty: Boolean): Boolean;
var
  HFile: Integer;
  Sr: _Z_TSearchRec;
  Wd: TZMWorkDrive;
  Xname: string;
begin
  Result := False;
  Wd := TZMWorkDrive.Create(Body);
  try
    Xname := ExpandUNCFileName(Fname);
    // test if destination can be written
    Wd.DriveStr := Xname;
    if not Wd.HasMedia(False) then
    begin
      Result := AllowEmpty and (Wd.DriveType = DRIVE_REMOVABLE);
      // assume can put in writable disk
      Exit;
    end;
    if {$IFNDEF UNICODE}IsWinXP or {$ENDIF}(Wd.DriveType <> DRIVE_CDROM) then
    begin
      if _Z_FindFirst(Xname, FaAnyFile, Sr) = 0 then
      begin
        Result := (Sr.Attr and FaReadOnly) = 0;
        _Z_FindClose(Sr);
        if Result then
        begin
          // exists and is not read-only - test locked
          HFile := _Z_FileOpen(Xname, FmOpenWrite);
          Result := HFile > -1;
          if Result then
{$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FileClose(HFile);
        end;
        Exit;
      end;
      // file did not exist - try to create it
      HFile := _Z_FileCreate(Xname);
      if HFile > -1 then
      begin
        Result := True;
{$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FileClose(HFile);
        File_Delete(Xname);
      end;
    end;
  finally
    Wd.Free;
  end;
end;

function TZMDLLOpr.JoinMVArchive(var TmpZipName: string): Integer;
var
  Attrs: Integer;
  Curz: TZMZipReader;
  Drt: Integer;
  Tempzip: TZMZipCopier;
  TmpMessage: TZMMessageEvent;
  Zname: string;
begin
  Zname := ZipFileName;
  TmpZipName := Master.MakeTempFileName('', '');
  if Verbosity >= ZvVerbose then
  begin
    TmpMessage := Master.OnMessage;
    if Assigned(TmpMessage) then
      TmpMessage(Master, 0, ZipFmtLoadStr(ZS_TempZip, [TmpZipName]));
  end;
  Result := 0;
  if Current.TotalEntries > 0 then
  begin
    if (AddFreshen in AddOptions) or (AddUpdate in AddOptions) then
    begin
      // is it detached SFX
      if Current.MultiDisk and (Current.Sig = ZfsDOS) then
        // load the actual zip instead of the loader (without events)
        Lister.LoadZip(ChangeFileExt(Zname, EXT_ZIPL), True);

      Curz := Current;
      // test if output can eventually be produced
      Drt := Curz.WorkDrive.DriveType;
      // we can't re-write on a CD-ROM

      if (Drt = DRIVE_CDROM) then
      begin
        Attrs := FileGetAttr(Zname);
        if Attrs and FaReadOnly <> 0 then
        begin
          Result := ZM_Error({_LINE_}1315, ZE_NotChangeable);
          Body.ShowFmtMessage(Result, [Zname], True);
          Exit;
        end;
      end;
      // rebuild a temp archive
      Tempzip := TZMZipCopier.Create(Lister);
      try
        if Tempzip.File_Create(TmpZipName) then
        begin
          ShowProgress := ZspExtra;
          if Curz.File_Open(Curz.RealFileName{''}, FmOpenRead) then
          begin
            Tempzip.EncodeAs := ZeoUTF8; // change for creating this zip only
            Result := Tempzip.WriteFile(Curz, True);
          end // ;
          else
            Result := Body.PrepareErrMsg(ZE_FileOpen, [Curz.ArchiveName],
              {_LINE_}1334, __UNIT__);
        end // ;
        else
          Result := Body.PrepareErrMsg(ZE_FileCreate, [Curz.ArchiveName],
            {_LINE_}1338, __UNIT__);
      finally
        Tempzip.Free;
        Curz.File_Close;
      end;
    end;
    if Result < 0 then
    begin
      Errors.ExtCode := Result;
      Exit;
    end;
    AnswerAll := True;
  end;
  Result := 0;
end;

// called from ExtAdd
// recreate main file (ZipFileName) from temporary file (TmpZipName)
function TZMDLLOpr.RecreateMVArchive(const TmpZipName: string; Recreate:
    Boolean): Boolean;
var
  OutPath: string;
  R: Integer;
  Tmp: string;
  TmpZip: TZMZipCopier;
begin
  Result := False;
  if Current.IsExtStream then
    raise EZipMaster.CreateMsg(Body, ZE_StreamNoSupport, {_LINE_}1366, __UNIT__);
  if Recreate then
  begin
    try
      Current.SeekDisk(0, True); // ask to enter the first disk again
      Current.File_Close;
    except
      on E: Exception do
      begin
        File_Delete(TmpZipName); // delete the temp file
        raise; // throw last exception again
      end;
    end;
  end;

  try
    TmpZip := TZMZipCopier.Create(Lister);

    TmpZip.ArchiveName := Current.ArchiveName;
    TmpZip.DiskNr := -1;
    TmpZip.IsMultiPart := True;
    TmpZip.Numbering := Current.Numbering;

    if AnsiSameText('.exe', ExtractFileExt(ZipFileName)) then
    begin // make 'detached' SFX
      OutPath := ZipFileName; // remember it
      Lister.Set_ZipFileName(TmpZipName, ZloFull); // reload
      // create an header first to now its size
      Tmp := ExtractFileName(OutPath);
      R := ConvertToSpanSFX(OutPath, Current);
      if R >= 0 then
      begin
        File_Delete(TmpZipName);
        Lister.Set_ZipFileName(OutPath, ZloNoLoad); // restore it
      end
      else
      begin
        SuccessCnt := 0; // failed
        if AbsErr(R) <> ZS_Canceled then
          ShowMessage(ZM_Error({_LINE_}1407, ZE_NoOutFile),
            'Error ' + IntToStr(R));
      end;
    end { if SameText(...) }
    else
    begin
      if Recreate then
        // reproduce orig numbering
        Span.Options := Current.MapNumbering(Span.Options);
      if WriteSpan(TmpZipName, Current.ReqFileName, True) < 0 then
        SuccessCnt := 0;
      File_Delete(TmpZipName);
    end;
  finally
    FreeAndNil(TmpZip);
  end;
end;

procedure TZMDLLOpr.SetAddOptions(const Value: TZMAddOpts);
begin
  Body.AddOptions := Value;
end;

procedure TZMDLLOpr.SetCB(const Value: TDZCallback);
begin
  if FCB <> Value then
  begin
    FCB := Value;
  end;
end;

procedure TZMDLLOpr.SetDLL_Load(const Value: Boolean);
begin
{$IFDEF ZDEBUG}
  Body.TraceFmt('set DLL_Load to %d', [Ord(Value)], {_LINE_}1442, __UNIT__);
{$ENDIF}
  if Value <> _DLL_Loaded(Master) then
  begin
    if Value then
      _DLL_Load(Lister)
    else
      _DLL_Unload(Lister);
{$IFDEF ZDEBUG}
    Body.TraceFmt('changed DLL_Load to %d', [Ord(Value)], {_LINE_}1451,
      __UNIT__);
{$ENDIF}
  end;
end;

procedure TZMDLLOpr.SetPasswordReqCount(const Value: Integer);
begin
  Lister.PasswordReqCount := Value;
end;

procedure TZMDLLOpr.SetRootDir(const Value: string);
begin
  Lister.RootDir := Value;
end;

function TZMDLLOpr.SetupZipCmd(const Value: string): PDLLCommands;
var
  Opts: Cardinal;
  AddOpts: TZMAddOpts;
begin
  Result := AllocDLLCommand(Value);
  if Result <> nil then
  begin
    AddOpts := AddOptions;
    Opts := Result^.FOptions;
    Result^.FEncodedAs := 0; // how to interpret existing names
    if Lister.Encoding = ZeoOEM then
      Result^.FEncodedAs := Ord(ZeoOEM)
    else
      if Lister.Encoding = ZeoUTF8 then
        Result^.FEncodedAs := Ord(ZeoUTF8);
    Result^.FEncodeAs := Ord(Lister.EncodeAs); // how to encode new names

    if AddArchiveOnly in AddOpts then
      Opts := Opts or DLL_OPT_ArchiveFilesOnly;
    if AddResetArchive in AddOpts then
      Opts := Opts or DLL_OPT_ResetArchiveBit;

    if HowToDelete = HtdAllowUndo then
      Opts := Opts or DLL_OPT_HowToMove;
    if AddVersion in AddOpts then
      Opts := Opts or DLL_OPT_Versioning;
    if AddVolume in AddOpts then
      Opts := Opts or DLL_OPT_Volume;

    { if True, exclude files earlier than specified date }
    { Date to include files after; only used if fDate=TRUE }
    if AddFromDate in AddOpts then
      Result^.FDate := DateTimeToFileDate(AddFrom);
    // Compression level (0 - 9, 0=none and 9=best)
    Result^.FLevel := AddCompLevel;
    if not(ZwoSafe in Lister.WriteOptions) then
      Opts := Opts or DLL_OPT_Grow;
    { if True, Allow appending to a zip file (-g) }
    if AddNTFS in AddOpts then
      Opts := Opts or DLL_OPT_NTFSStamps;

    // distinguish bet. Add and Delete
    Opts := Opts or DLL_OPT_OpIsZip;

    // make zipfile's timestamp same as newest file
    if ZwoZipTime in WriteOptions then
      Opts := Opts or DLL_OPT_LatestTime;

    if AddMove in AddOpts then
      Opts := Opts or DLL_OPT_Move; // dangerous, beware!

    if AddUpdate in AddOpts then
      Opts := Opts or DLL_OPT_Update
    else
      if AddFreshen in AddOpts then
        Opts := Opts or DLL_OPT_Freshen;
    // { Update has precedence over freshen }

    { DLL will prompt for password }
    if AddEncrypt in AddOpts then
      Opts := Opts or DLL_OPT_Encrypt;
    { NOTE: if user wants recursion, then he probably also wants
      AddDirNames, but we won't demand it. }
    if AddRecurseDirs in AddOpts then
      Opts := Opts or DLL_OPT_Recurse;
    if AddHiddenFiles in AddOpts then
      Opts := Opts or DLL_OPT_System;
    if not(AddEmptyDirs in AddOpts) then
      Opts := Opts or DLL_OPT_NoDirEntries;
    { don't store dirnames with filenames }
    if not(AddDirNames in AddOpts) then
      Opts := Opts or DLL_OPT_JunkDir;

    Result^.FOptions := Opts;
    Result^.FCheck := DLLCOMMANDCHECK;
  end;
end;

procedure TDZCallback.AfterConstruction;
begin
  inherited;
  PCB := nil;
  FHeldData := nil;
  FHoldSize := 0;
end;

function TDZCallback.Assign(ZCallBackRec: PZCallBackStruct): Integer;
begin
  PCB := ZCallBackRec;
  if PCB = nil then
    Result := 1
  else
    Result := 0;
end;

procedure TDZCallback.BeforeDestruction;
begin
  if FHeldData <> nil then
    FreeMem(FHeldData);
  FHeldData := nil;
  inherited;
end;

procedure TDZCallback.Clear;
begin
  if FHeldData <> nil then
    FreeMem(FHeldData);
  FHeldData := nil;
  FHoldSize := 0;
  PCB := nil; // ??
end;

function TDZCallback.CopyData(Dst: PByte; MaxSize: Integer): Boolean;
var
  Sz: Integer;
begin
  Result := False;
  Sz := Arg1;
  if Sz > MaxSize then
    Sz := MaxSize;
  if Sz > 0 then
  begin
    Move(PCB^.Msg2P^, Dst^, Sz);
    Result := True;
  end;
end;

function TDZCallback.GetActionCode: Integer;
begin
  Result := PCB^.ActionCode;
end;

function TDZCallback.GetArg1: Cardinal;
begin
  Result := PCB^.Arg1;
end;

function TDZCallback.GetArg2: Cardinal;
begin
  Result := PCB^.Arg2;
end;

function TDZCallback.GetArg3: Integer;
begin
  Result := PCB^.Arg3;
end;

function TDZCallback.GetFile_Size: Int64;
begin
  Result := PCB^.File_Size;
end;

function TDZCallback.GetIsZip: Boolean;
begin
  Result := PCB^.IsOperationZip;
end;

function TDZCallback.GetMsg: string;
begin
  Result := GetMsgStr(PCB^.MsgP);
end;

function TDZCallback.GetMsg2: string;
begin
  Result := GetMsgStr(PCB^.Msg2P);
end;

function TDZCallback.GetMsgStr(const Msg: PByte): string;
{$IFNDEF UNICODE}
var
  Utemp: UTF8String;
{$ENDIF}
begin
  Result := '';
  if Msg <> nil then
  begin
{$IFDEF UNICODE}
    if PCB^.HaveWide <> 0 then
      Result := PWideChar(Msg)
    else
      Result := PUTF8ToWideStr(PAnsiChar(Msg), -1);
{$ELSE}
    if UsingUtf8 then
    begin
      if PCB^.HaveWide <> 0 then
        Result := PWideToUTF8(PWideChar(Msg), -1)
      else
      begin
        Utemp := PAnsiChar(Msg);
        Result := StrToUTF8(Utemp);
      end;
    end
    else
    begin
      if PCB^.HaveWide <> 0 then
        Result := PWideChar(Msg) // will convert wide -> ansi
      else
        Result := PAnsiChar(Msg);
    end;
{$ENDIF}
  end;
end;

function TDZCallback.GetOwner: TZMDLLOpr;
begin
  Result := TObject(PCB^.Caller) as TZMDLLOpr;
end;

function TDZCallback.GetWritten: Int64;
begin
  Result := PCB^.Written;
end;

function TDZCallback.HoldData(const Src: PByte; Size: Cardinal): PByte;
var
  Len: Integer;
  P: PByte;
begin
  if Src = nil then
  begin
    // free buffer
    FreeMem(FHeldData);
    FHeldData := nil;
    FHoldSize := 0;
    Result := FHeldData;
    Exit;
  end;
  if FHeldData = nil then
    FHoldSize := 0;
  Len := Size + Sizeof(Integer);
  if (FHeldData = nil) or (Len >= FHoldSize) then
  begin
    if FHeldData <> nil then
      FreeMem(FHeldData);
    FHeldData := nil;
    Len := (Len or 511) + 1; // increments of 512
    GetMem(FHeldData, Len);
    FHoldSize := Len;
  end;
  P := FHeldData;
  if Size > 0 then
  begin
    Move(Src^, FHeldData^, Size);
    Inc(P, Size);
  end;
  PCardinal(P)^ := 0; // mark end
  Result := FHeldData;
end;

function TDZCallback.HoldString(const Src: string): PByte;
var
  Len: Integer;
begin
  Len := Length(Src) * Sizeof(Char);
  if Len > 0 then
    Result := HoldData(PByte(PChar(Src)), Len)
  else
    Result := HoldData(PByte(@Len), 0); // avoid freeing hold area
end;

procedure TDZCallback.SetArg1(const Value: Cardinal);
begin
  PCB^.Arg1 := Value;
end;

procedure TDZCallback.SetArg2(const Value: Cardinal);
begin
  PCB^.Arg2 := Value;
end;

procedure TDZCallback.SetArg3(const Value: Integer);
begin
  PCB^.Arg3 := Value;
end;

procedure TDZCallback.SetComment(const AStr: AnsiString);
begin
  PCB^.HaveWide := 0;
  PCB^.MsgP := HoldData(PByte(PAnsiChar(AStr)), Length(AStr));
  PCB^.Arg1 := Cardinal(Length(AStr));
end;

procedure TDZCallback.SetData(Src: PByte; Size: Integer);
begin
  if Size > 2048 then
    Size := 2048;
  PCB^.MsgP := HoldData(Src, Size);
  PCB^.Arg1 := Cardinal(Size);
end;

procedure TDZCallback.SetFile_Size(const Value: Int64);
begin
  PCB^.File_Size := Value;
end;

procedure TDZCallback.SetMsg(const Value: string);
begin
{$IFDEF UNICODE}
  PCB^.HaveWide := 1; // Unicode
{$ELSE}
  if UsingUtf8 and (ValidUTF8(Value, -1) > 0) then
    PCB^.HaveWide := 2 // UTF8
  else
    PCB^.HaveWide := 0; // Ansi
{$ENDIF}
  PCB^.MsgP := HoldString(Value);
end;

constructor TZMOpAddStreamToFile.Create(const FileName: string;
  FileDate, FileAttr: Dword);
begin
  inherited Create;
  FFileName := FileName;
  FFileDate := FileDate;
  FFileAttr := FileAttr;
end;

{ TZMOpAddStreamToFile }

function TZMOpAddStreamToFile.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpAddStreamToFile.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMDLLOpr;
begin
  FOper := TZMDLLOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  TZMCommand(TheBody).DLLWorking := Self;
  Result := FOper.AddStreamToFile(FFileName, FFileDate, FFileAttr);
end;

function TZMOpAddStreamToFile.Name: string;
begin
  Result := 'AddStreamToFile';
end;

function TZMOpAddStreamToFile.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

constructor TZMOpAdd.Create;
begin
  inherited Create;
end;

{ TZMOpAdd }

function TZMOpAdd.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpAdd.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMDLLOpr;
begin
  FOper := TZMDLLOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  TZMCommand(TheBody).DLLWorking := Self;
  Result := FOper.Add;
end;

function TZMOpAdd.Name: string;
begin
  Result := 'Add';
end;

function TZMOpAdd.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

{ TZMDLL }

procedure TZMDLL.AbortDLL;
begin
  (AnOperation as TZMDLLOpr).AbortDLL;
end;

end.
