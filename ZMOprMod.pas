unit ZMOprMod;

// ZMModOpr.pas - Operations modifying existing entries

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
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
  ZMHandler, ZipMstr;

type
  TZMOpChangeFileDetails = class(TZMOperationRoot)
  private
    FCFunc: TZMChangeFunction;
    FData: Pointer;
  public
    constructor Create(Func: TZMChangeFunction; var Data);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpRename = class(TZMOperationRoot)
  private
    FHow: TZMRenameOpts;
    FNewDateTime: Integer;
    FRenameList: TList;
  public
    constructor Create(RenameList: TList; NewDateTime: Integer;
      How: TZMRenameOpts = HtrDefault);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpSetZipComment = class(TZMOperationRoot)
  private
    FComment: AnsiString;
  public
    constructor Create(const ZComment: AnsiString);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Windows, System.SysUtils, VCL.Dialogs, VCL.Graphics,
{$ELSE}
  Windows, SysUtils, Dialogs, Graphics, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMLister, ZMBody, ZMBaseOpr, ZMZipReader, ZMZipWriter, ZMZipDirectory,
  ZMXcpt, ZMStructs, ZMUtils, ZMMsg, ZMZipBase, ZMMatch, ZMWinFuncs, ZMZipMulti,
  ZMCore;

const
  __UNIT__ = 31;

type
  TSFXOps = (SfoNew, SfoZip, SfoExe);

type
  TZMChangeOpr = class(TZMBaseOpr)
  private
    procedure ChangeEOCComment(Wz: TZMZipWriter; const ZComment: AnsiString);
  protected
    procedure CreateInterimZip; override;
  public
    function ChangeFileDetails(Func: TZMChangeFunction; var Data): Integer;
    function Rename(RenameList: TList; NewDateTime: Integer;
      How: TZMRenameOpts = HtrDefault): Integer;
    procedure Set_StreamZipComment(const ZComment: AnsiString);
    procedure Set_ZipComment(const ZComment: AnsiString);
  end;

type
  TZMRecChangables = (ZrcAttributes, ZrcComment, ZrcDOSDate, ZrcEncoding,
    ZrcExtraField, ZrcFileName);
  TZMRecChanges = set of TZMRecChangables;

type
  TZMChangeRec = class(TZMDirRec)
  private
    FChanged: TZMRecChanges;
    FExtFileAttrib: Cardinal;
    FExtraField: TZMRawBytes;
    FFileComment: string;
    FFileName: string;
    FIsEncoded: TZMEncodingOpts;
    FModifDateTime: Cardinal;
    FMyMaster: TCustomZipMaster;
    FMyRec: TZMEntryCopier;
    function GetChanges(Index: TZMRecChangables): Boolean;
    function SameExtra(const ExtraField, ExtraField1: TZMRawBytes): Boolean;
    procedure SetChanged(const Value: TZMRecChanges);
    procedure SetChanges(Index: TZMRecChangables; const Value: Boolean);
    property Changes[index: TZMRecChangables]: Boolean read GetChanges
      write SetChanges;
  protected
    procedure AssignEntry(Entry: TZMEntryCopier);
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetEncoded: TZMEncodingOpts; override;
    function GetEncrypted: Boolean; override;
    function GetExtFileAttrib: Longword; override;
    function GetExtraField: TZMRawBytes; override;
    function GetExtraFieldLength: Word; override;
    function GetFileComment: string; override;
    function GetFileCommentLen: Word; override;
    function GetFileName: string; override;
    function GetFileNameLength: Word; override;
    function GetFlag: Word; override;
    function GetHeaderName: TZMRawBytes; override;
    function GetIntFileAttrib: Word; override;
    function GetMaster: TComponent; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Word; override;
    function GetStatusBits: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
  public
    constructor Create(TheMaster: TCustomZipMaster;
      TheRec: TZMEntryCopier); overload;
    function ChangeAttrs(NAttr: Cardinal): Integer; override;
    function ChangeComment(const Ncomment: string): Integer; override;
    function ChangeData(Ndata: TZMRawBytes): Integer; override;
    function ChangeDate(Ndosdate: Cardinal): Integer; override;
    function ChangeEncoding: Integer; override;
    function ChangeName(const NewName: string; NoCheck: Boolean = False): Integer;
        override;
    property Changed: TZMRecChanges read FChanged write SetChanged;
  end;

type
  PRenData = ^TRenData;

  TRenData = record
    Owner: TZMBaseOpr;
    RenList: TList;
    DTime: Integer;
    How: TZMRenameOpts;
    Cnt: Integer;
  end;

type
  TZMZipChanger = class(TZMZipCopier)
  public
    function ChangeDetails(Func: TZMChangeFunction; var Data): Integer;
    function PrepareEntries(Src: TZMZipReader): Integer;
  end;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

// 'ForEach' function to rename files
function RenFunc(Rec: TZMDirRec; var Data): Integer;
var
  ChangingName: Boolean;
  FileName: string;
  How: TZMRenameOpts;
  I: Integer;
  K: Integer;
  Ncomment: string;
  Newname: string;
  NewStamp: Integer;
  PData: PRenData;
  PRenRec: PZMRenameRec;
  RenSource: string;
begin
  FileName := Rec.FileName;
  PData := @Data;
  How := PData.How;
  Result := 0;
  for I := 0 to PData^.RenList.Count - 1 do
  begin
    PRenRec := PZMRenameRec(PData^.RenList[I]);
    RenSource := PRenRec.Source;
    Newname := PRenRec.Dest;
    Ncomment := PRenRec.Comment;
    NewStamp := PRenRec.DateTime;
    ChangingName := (Newname <> SPEC_SEP) and
      (CompareStr(FileName, Newname) <> 0);
    if How = HtrFull then
    begin
      if FileNameMatch(PRenRec.Source, FileName) then
        K := -1
      else
        K := 0;
    end
    else
    begin
      K := Pos(UpperCase(RenSource), UpperCase(FileName));
    end;
    if K <> 0 then
    begin
      Inc(PData^.Cnt); // I am selected
      if not ChangingName then
        Result := 0
      else
      begin
        if K > 0 then
        begin
          Newname := FileName;
          System.Delete(Newname, K, Length(RenSource));
          Insert(PRenRec.Dest, Newname, K);
        end;
        Result := Rec.ChangeName(Newname);
        if Result = 0 then
          FileName := Rec.FileName;
      end;
      if Result = 0 then
      begin
        if Ncomment <> '' then
        begin
          if Ncomment[1] = #0 then
            Ncomment := '';
          Result := Rec.ChangeComment(Ncomment);
        end;
      end;
      if Result = 0 then
      begin
        if NewStamp = 0 then
          NewStamp := PData^.DTime;
        if NewStamp <> 0 then
          Result := Rec.ChangeDate(NewStamp);
      end;
      if How <> HtrDefault then
        Break;
    end;
  end;
end;

function TZMZipChanger.ChangeDetails(Func: TZMChangeFunction; var Data)
  : Integer;
var
  Changes: Integer;
  ChangeRec: TZMChangeRec;
  CRec: TZMEntryCopier;
  I: Integer;
  Reason: TZMSkipTypes;
  RecName: string;
begin
  Result := 0;
  Changes := 0;
  try
    ChangeRec := TZMChangeRec.Create(Master, nil);
    try
      for I := 0 to ToDoList.Count - 1 do
      begin
        CRec := TZMEntryCopier(ToDoList[I]);
        ChangeRec.AssignEntry(CRec);
        RecName := CRec.FileName;
        Result := Func(ChangeRec, Data);
        CheckCancel;
        if Result <> 0 then
        begin
          Body.InformFmt('error [%d] for: %s', [AbsErr(Result), CRec.FileName],
            {_LINE_}319, __UNIT__);

          Reason := StCannotDo;
          if AbsErr(Result) = ZE_DuplFileName then
            Reason := StDupName;
          if Skipping(RecName, Reason, Result) then
            Break; // fatal
          Result := 0; // ignore error
          Continue;
        end;
        if ChangeRec.Changed <> [] then
        begin
          if ZrcFileName in ChangeRec.Changed then
          begin
            if FHashTable <> nil then
              HTRemove(CRec); // remove old name
            CRec._FileName := '';
            CRec.FileName := ChangeRec.FileName;
            CRec.Status[ZsbRenamed] := True;
            if FHashTable <> nil then
              HTAdd(CRec, True); // add new name
            RecName := RecName + ' :: ' + CRec.FileName;
          end;
          if ZrcComment in ChangeRec.Changed then
          begin
            CRec._FileComment := '';
            CRec.FileComment := ChangeRec.FileComment;
            CRec.SetStatusBit(ZsbVChanged);
          end;
          if ZrcAttributes in ChangeRec.Changed then
          begin
            CRec.ExtFileAttrib := ChangeRec.ExtFileAttrib;
          end;
          if ZrcDOSDate in ChangeRec.Changed then
          begin
            CRec.ModifDateTime := ChangeRec.DateTime;
            CRec.SetStatusBit(ZsbVChanged);
          end;
          if ZrcEncoding in ChangeRec.Changed then
          begin

            CRec.SetStatusBit(ZsbVChanged);
          end;
          if ZrcExtraField in ChangeRec.Changed then
          begin
            CRec.ExtraField := ChangeRec.ExtraField;
            CRec.SetStatusBit(ZsbVChanged);
          end;
          CRec.SetStatusBit(ZsbDirty);
          Inc(Changes);
          Body.TraceFmt('Changed: %s', [RecName], {_LINE_}369, __UNIT__);
          CRec.Status[ZsbHail] := True;
        end
        else
          ProblemList.Add(RecName);
      end;
    finally
      ChangeRec.Free;
    end;
  except
    on E: EZipMaster do
      Result := E.ExtErr;
    on E: EZMAbort do
      raise;
    on E: Exception do
      Result := Body.PrepareErrMsg(ZE_ExceptErr, [E.Message],
        {_LINE_}385, __UNIT__);
  end;
  if Result = 0 then
    Result := Changes;
end;

// returns count of selected entries
function TZMZipChanger.PrepareEntries(Src: TZMZipReader): Integer;
var
  CopyRec: TZMEntryWriter;
  Rec: TZMEntryBase;
begin
  Result := 0;
  HTAutoSize(Src.Count);
  Rec := Src.FirstRec;
  while Rec <> nil do
  begin
    if Rec.StatusBit[ZsbError or ZsbDiscard] = 0 then
    begin
      if Verbosity > ZvVerbose then
        Body.TraceFmt('including: %s', [Rec.FileName], {_LINE_}405, __UNIT__);
      CopyRec := TZMEntryCopier.Create(Self); // make a copy
      CopyRec.AssignFrom(Rec);
      CopyRec.Link := Rec; // link to original
      Add(CopyRec);
      HTAdd(CopyRec, True); // add all names
      if Rec.TestStatusBit(ZsbSelected) then
      begin
        ToDoList.Add(CopyRec);
        Inc(Result);
      end;
    end;
    Rec := Rec.Next;
  end;
end;

procedure TZMChangeOpr.ChangeEOCComment(Wz: TZMZipWriter;
  const ZComment: AnsiString);
var
  EOC: TZipEndOfCentral;
  Len: Integer;
  Zcom: AnsiString;
begin
  Zcom := ZComment;
  Len := Length(Zcom);
  Wz.CheckSeek(Wz.EOCOffset, SoBeginning, ZM_Error({_LINE_}430, ZE_FailedSeek));
  Wz.CheckRead(EOC, SizeOf(EOC), ZM_Error({_LINE_}431, ZE_EOCBadRead));
  if (EOC.HeaderSig <> EndCentralDirSig) then
    raise EZipMaster.CreateMsg(Body, ZE_EOCBadRead, {_LINE_}433, __UNIT__);
  EOC.ZipCommentLen := Len;
  Wz.StampDate := File_Age(Wz.ArchiveName);
  Wz.CheckSeek(Wz.EOCOffset, SoBeginning, ZM_Error({_LINE_}436, ZE_FailedSeek));
  Wz.CheckWrite(EOC, SizeOf(EOC), ZM_Error({_LINE_}437, ZE_EOCBadWrite));
  if Len > 0 then
    Wz.CheckWrite(Zcom[1], Len, ZM_Error({_LINE_}439, ZE_EOCBadWrite));
  // if SetEOF fails we get garbage at the end of the file, not nice but
  // also not important.
  Wz.SetEndOfFile;
end;

(* TZMChangeOpr.ChangeFileDetails
  Add zipped files from source ZipMaster selected from source FSpecArgs
 When finished
 FSpecArgs will contain source files copied
 FSpecArgsExcl will contain source files skipped  (data = error code)
*)
function TZMChangeOpr.ChangeFileDetails(Func: TZMChangeFunction;
  var Data): Integer;
var
  CurZip: TZMZipReader;
  Did: Integer;
  MyChanger: TZMZipChanger;
  SelCnt: Integer;
begin
  Body.Trace('StartUp ChangeFileDetails', {_LINE_}459, __UNIT__);
  if Body.Logging then
    Body.LogSpecs('');
  if not Assigned(Func) then
    raise EZipMaster.CreateMsg(Body, ZE_InvalidArguments, {_LINE_}463,
      __UNIT__);
  if IncludeSpecs.Count < 1 then
    IncludeSpecs.Add('*.*');
  // copy all entries to InterimZip
  CurZip := CurrentZip(True, False);
  SelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs);
  Body.ClearIncludeSpecs; // will contain files processed
  if SelCnt < 0 then
  begin
    Result := SelCnt;
    ShowError(Result);
    Exit;
  end;
  // Body.ClearIncludeSpecs; // will contain files processed
  PrepareInterimZip;
  MyChanger := InterimZip as TZMZipChanger;
  MyChanger.PrepareEntries(CurZip);
  // process selected files and copy rest
  Result := MyChanger.ChangeDetails(Func, Data);
  Did := Result;
  if Result > 0 then
    Result := FinalizeInterimZip(CurZip); // write results

  if Result < 0 then
    ShowError(Result)
  else
    SuccessCnt := Did;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  Reload := ZlrReload;
  Body.Trace('finished ChangeFileDetails', {_LINE_}494, __UNIT__);
  if Body.Logging then
    Body.LogSpecs('');
end;

procedure TZMChangeOpr.CreateInterimZip;
begin
  InterimZip := TZMZipChanger.Create(Lister);
  if not InterimZip.File_CreateTemp(PRE_INTER, '') then
    raise EZipMaster.CreateMsg(Body, ZE_NoOutFile, {_LINE_}503, __UNIT__);
end;

(* ? TZMChangeOpr.Rename
  Function to read a Zip archive and change one or more file specifications.
 Source and Destination should be of the same type. (path or file)
 If NewDateTime is 0 then no change is made in the date/time fields.
*)
function TZMChangeOpr.Rename(RenameList: TList; NewDateTime: Integer;
  How: TZMRenameOpts = HtrDefault): Integer;
var
  I: Integer;
  RenDat: TRenData;
  RenRec: PZMRenameRec;
begin
  for I := 0 to RenameList.Count - 1 do
  begin
    RenRec := RenameList.Items[I];
    if IsWild(RenRec.Source) then
      raise EZipMaster.CreateMsgFmt(Body, ZE_WildName, [RenRec.Source],
        {_LINE_}523, __UNIT__);
    RenRec^.Source := SetSlash(RenRec^.Source, PsdExternal);
    RenRec^.Dest := SetSlash(RenRec^.Dest, PsdExternal);
  end;
  RenDat.Owner := Self;
  RenDat.RenList := RenameList;
  RenDat.DTime := NewDateTime;
  RenDat.How := How;
  RenDat.Cnt := 0;
  if IncludeSpecs.Count < 1 then
    IncludeSpecs.Add('*.*');
  Result := ChangeFileDetails(RenFunc, RenDat);
  if Result >= 0 then
    SuccessCnt := RenDat.Cnt;
end;

procedure TZMChangeOpr.Set_StreamZipComment(const ZComment: AnsiString);
var
  Wz: TZMZipWriter;
begin
  Wz := TZMZipWriter.Create(Lister);
  try
    try
      Wz.Stream := TZMLister(Body).ExtStream;
      if Wz.OpenZip(True, True) < 0 then
        raise EZipMaster.CreateMsg(Body, ZE_NoValidZip, { _LINE_ } 518,
          __UNIT__);
      Reload := ZlrReload; // force reload
      ChangeEOCComment(Wz, ZComment);
    except
      on Ews: EZipMaster do
        ShowExceptionError(Ews)
      else
        raise;
    end;
  finally
    Wz.Free;
  end;
end;

procedure TZMChangeOpr.Set_ZipComment(const ZComment: AnsiString);
var
  Wz: TZMZipWriter;
begin
  Wz := TZMZipWriter.Create(Lister);
  try
    try
      if Length(ZipFileName) <> 0 then
      begin
        Span.Options := Span.Options - [SpExactName];
        Wz.ArchiveName := ZipFileName;
        Wz.OpenZip(True, True); // ignore Errors
      end
      else
        raise EZipMaster.CreateMsg(Body, ZE_NoZipSpecified, {_LINE_}577,
          __UNIT__);
      Reload := ZlrReload; // force reload
      // opened by OpenEOC() only for Read
      if Wz.IsOpen then // file exists
      begin
        Wz.File_Close;
        // must reopen for read/write
        Wz.File_Open(Wz.ArchiveName{''}, FmShareDenyWrite or FmOpenReadWrite);
        if not Wz.IsOpen then
          raise EZipMaster.CreateMsgFmt(Body, ZE_FileOpen, [ZipFileName],
            {_LINE_}588, __UNIT__);
        if Wz.MultiDisk and (Wz.StampDate = 0) then
          Wz.StampDate := Wz.LastWritten; // keep date of set
        ChangeEOCComment(Wz, ZComment);
        Wz.FixFileDate; // TODO: warning, skip?
      end;
    except
      on Ews: EZipMaster do
        ShowExceptionError(Ews)
      else
        raise;
    end;
  finally
    Wz.Free;
  end;
end;

constructor TZMChangeRec.Create(TheMaster: TCustomZipMaster;
  TheRec: TZMEntryCopier);
begin
  inherited Create;
  FMyMaster := TheMaster;
  AssignEntry(TheRec);
end;

procedure TZMChangeRec.AssignEntry(Entry: TZMEntryCopier);
begin
  FMyRec := Entry;
  if FMyRec <> nil then
  begin
    FExtFileAttrib := Entry.ExtFileAttrib;
    FExtraField := Entry.ExtraField;
    FFileComment := Entry.FileComment;
    FFileName := Entry.FileName;
    FIsEncoded := Entry.IsEncoded;
    FModifDateTime := Entry.ModifDateTime;
  end
  else
  begin
    FExtFileAttrib := 0;
    FExtraField := '';
    FFileComment := '';
    FFileName := '';
    FIsEncoded := ZeoAuto;
    FModifDateTime := 0;
  end;
  FChanged := [];
end;

{ TZMChangeRec }
function TZMChangeRec.ChangeAttrs(NAttr: Cardinal): Integer;
begin
  if NAttr <> ExtFileAttrib then
  begin
    FExtFileAttrib := NAttr;
    Changes[ZrcAttributes] := NAttr <> FMyRec.ExtFileAttrib;
  end;
  Result := 0;
end;

function TZMChangeRec.ChangeComment(const Ncomment: string): Integer;
begin
  if CompareStr(Ncomment, FFileComment) <> 0 then
  begin
    FFileComment := Ncomment;
    Changes[ZrcComment] := CompareStr(FFileComment, FMyRec.FileComment) <> 0;
  end;
  Result := 0;
end;

function TZMChangeRec.ChangeData(Ndata: TZMRawBytes): Integer;
var
  NowData: TZMRawBytes;
  OldData: TZMRawBytes;
begin
  Result := 0; // always allowed
  if not SameExtra(Ndata, FExtraField) then
  begin
    // preserve required tags
    OldData := XDataKeep(FExtraField, [Zip64_data_tag, UPath_Data_Tag,
      UCmnt_Data_Tag]);
    // do not allow changing fields
    NowData := XDataRemove(Ndata, [Zip64_data_tag, UPath_Data_Tag,
      UCmnt_Data_Tag]);
    // will it fit?
    if (Length(OldData) + Length(NowData) + Length(GetFileComment) +
      Length(GetFileName)) < MAX_WORD then
    begin
      FExtraField := OldData + NowData;
      Changes[ZrcExtraField] := not SameExtra(FExtraField, FMyRec.ExtraField);
    end
    else
      Result := ZM_Error({_LINE_}680, ZE_CEHDataSize);
  end;
end;

function TZMChangeRec.ChangeDate(Ndosdate: Cardinal): Integer;
begin
  if Encrypted then
  begin
    Result := ZM_Error({_LINE_}688, ZE_NoProtected);
    Exit;
  end;
  try
    // test if valid date/time will throw error if not
    FileDateToDateTime(Ndosdate);
  except
    on E: EZMAbort do
      raise
    else
    begin
      Result := ZM_Error({_LINE_}699, ZE_InvalidDateTime);
      FMyRec.Body.InformFmt('Invalid date %s', [GetFileName], {_LINE_}700,
        __UNIT__);
      Exit;
    end;
  end;
  Result := 0;
  if Ndosdate <> GetDateTime then
  begin
    FModifDateTime := Ndosdate;
    Changes[ZrcDOSDate] := FModifDateTime <> FMyRec.ModifDateTime;
  end;
end;

function TZMChangeRec.ChangeEncoding: Integer;
begin
  Changes[ZrcEncoding] := True;
  Result := 0;
end;

function TZMChangeRec.ChangeName(const NewName: string; NoCheck: Boolean =
    False): Integer;
var
  Dup: TZMEntryBase;
  IntName: string;
begin
  Result := FMyRec.ToIntForm(NewName, IntName);
  if Result = 0 then
  begin
    if IsFolder(IntName) <> FMyRec.IsDirOnly then
    begin
      Result := ZM_Error({_LINE_}730, ZE_NoChangeDir);
      Exit; // dirOnly status must be same
    end;
    if CompareStr(IntName, FFileName) <> 0 then
    begin
      if not NoCheck then
      begin
        Dup := FMyRec.MyFile.FindName(IntName, FMyRec);
        if Dup <> nil then
        begin
          Result := FMyRec.Body.PrepareErrMsg(ZE_DuplFileName, [IntName],
            {_LINE_}741, __UNIT__);
          FMyRec.Body.InformFmt('Name already exists: "%s"', [IntName],
            {_LINE_}743, __UNIT__);
          Exit;
        end;
      end;
      FFileName := IntName;
      Changes[ZrcFileName] := True;
    end;
  end;
end;

function TZMChangeRec.GetChanges(Index: TZMRecChangables): Boolean;
begin
  Result := Index in FChanged;
end;

function TZMChangeRec.GetCompressedSize: Int64;
begin
  Result := FMyRec.CompressedSize;
end;

function TZMChangeRec.GetCompressionMethod: Word;
begin
  Result := FMyRec.CompressionMethod;
end;

function TZMChangeRec.GetCRC32: Cardinal;
begin
  Result := FMyRec.CRC32;
end;

function TZMChangeRec.GetDateTime: Cardinal;
begin
  Result := FModifDateTime;
end;

function TZMChangeRec.GetEncoded: TZMEncodingOpts;
begin
  Result := FIsEncoded;
end;

function TZMChangeRec.GetEncrypted: Boolean;
begin
  Result := FMyRec.Encrypted;
end;

function TZMChangeRec.GetExtFileAttrib: Longword;
begin
  Result := FExtFileAttrib;
end;

function TZMChangeRec.GetExtraField: TZMRawBytes;
begin
  Result := FExtraField;
end;

function TZMChangeRec.GetExtraFieldLength: Word;
begin
  Result := Length(FExtraField);
end;

function TZMChangeRec.GetFileComment: string;
begin
  Result := FFileComment;
end;

function TZMChangeRec.GetFileCommentLen: Word;
begin
  Result := Length(FFileComment);
end;

function TZMChangeRec.GetFileName: string;
begin
  Result := FFileName;
end;

function TZMChangeRec.GetFileNameLength: Word;
begin
  Result := Length(FFileName);
end;

function TZMChangeRec.GetFlag: Word;
begin
  Result := FMyRec.Flag;
end;

function TZMChangeRec.GetHeaderName: TZMRawBytes;
begin
  Result := FMyRec._FileName;
end;

function TZMChangeRec.GetIntFileAttrib: Word;
begin
  Result := FMyRec.IntFileAttrib;
end;

function TZMChangeRec.GetMaster: TComponent;
begin
  Result := FMyMaster;
end;

function TZMChangeRec.GetRelOffLocalHdr: Int64;
begin
  Result := FMyRec.RelOffLocalHdr;
end;

function TZMChangeRec.GetStartOnDisk: Word;
begin
  Result := FMyRec.StartOnDisk;
end;

function TZMChangeRec.GetStatusBits: Cardinal;
begin
  Result := FMyRec.StatusBits;
end;

function TZMChangeRec.GetUncompressedSize: Int64;
begin
  Result := FMyRec.UncompressedSize;
end;

function TZMChangeRec.GetVersionMadeBy: Word;
begin
  Result := FMyRec.VersionMadeBy;
end;

function TZMChangeRec.GetVersionNeeded: Word;
begin
  Result := FMyRec.VersionNeeded;
end;

function TZMChangeRec.SameExtra(const ExtraField, ExtraField1: TZMRawBytes):
    Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(ExtraField) <> Length(ExtraField1) then
    Exit;
  for I := 1 to Length(ExtraField) do
    if ExtraField[I] <> ExtraField1[I] then
      Exit;
  Result := True;
end;

procedure TZMChangeRec.SetChanged(const Value: TZMRecChanges);
var
  C: TZMRecChangables;
  V: TZMRecChanges;
begin
  if FChanged <> Value then
  begin
    V := Value;
    // don't allow setting changes that have not happened
    for C := ZrcAttributes to ZrcFileName do
      if (C in V) and not(C in FChanged) then
        V := V - [C];
    // allow clearing changes
    if (ZrcAttributes in FChanged) and not(ZrcAttributes in V) then
      FExtFileAttrib := FMyRec.ExtFileAttrib;
    if (ZrcComment in FChanged) and not(ZrcComment in V) then
      FFileComment := FMyRec.FileComment;
    if (ZrcDOSDate in FChanged) and not(ZrcDOSDate in V) then
      FModifDateTime := FMyRec.ModifDateTime;
    if (ZrcEncoding in FChanged) and not(ZrcEncoding in V) then
      FIsEncoded := FMyRec.IsEncoded;
    if (ZrcExtraField in FChanged) and not(ZrcExtraField in V) then
      FExtraField := FMyRec.ExtraField;
    if (ZrcFileName in FChanged) and not(ZrcFileName in V) then
      FFileName := FMyRec.FileName;
    FChanged := V;
  end;
end;

procedure TZMChangeRec.SetChanges(Index: TZMRecChangables;
  const Value: Boolean);
begin
  if Changes[Index] <> Value then
  begin
    if Value then
      FChanged := FChanged + [Index]
    else
      FChanged := FChanged - [Index];
  end;
end;

constructor TZMOpChangeFileDetails.Create(Func: TZMChangeFunction; var Data);
begin
  inherited Create;
  FCFunc := Func;
  FData := @Data;
end;

{ TZMOpChangeFileDetails }

function TZMOpChangeFileDetails.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpChangeFileDetails.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMChangeOpr;
begin
  FOper := TZMChangeOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.ChangeFileDetails(FCFunc, FData^);
end;

function TZMOpChangeFileDetails.Name: string;
begin
  Result := 'ChangeFileDetails';
end;

function TZMOpChangeFileDetails.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

constructor TZMOpRename.Create(RenameList: TList; NewDateTime: Integer;
  How: TZMRenameOpts);
begin
  inherited Create;
  FRenameList := RenameList;
  FNewDateTime := NewDateTime;
  FHow := How;
end;

{ TZMOpRename }

function TZMOpRename.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpRename.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMChangeOpr;
begin
  FOper := TZMChangeOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.Rename(FRenameList, FNewDateTime, FHow);
end;

function TZMOpRename.Name: string;
begin
  Result := 'Rename';
end;

function TZMOpRename.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

constructor TZMOpSetZipComment.Create(const ZComment: AnsiString);
begin
  inherited Create;
  FComment := ZComment;
end;

{ TZMOpSetZipComment }

function TZMOpSetZipComment.Changes: TZMOperRes;
begin
  Result := [ZorZip];
end;

function TZMOpSetZipComment.Execute(TheBody: TZMHandler): Integer;
var
  Body: TZMLister;
  FOper: TZMChangeOpr;
begin
  Body := TheBody as TZMLister;
  FOper := TZMChangeOpr.Create(Body);
  AnOperation := FOper;
  if Body.IsSpanned or FileExists(Body.Master.ZipFileName) or
    (Body.ExtStream <> nil) then
  begin
    if not Body.CurrentIsValid then
      raise EZipMaster.CreateMsg(Body, ZE_NoProcess, {_LINE_}1021, __UNIT__);
    if Body.ExtStream <> nil then
      FOper.Set_StreamZipComment(FComment)
    else
      FOper.Set_ZipComment(FComment);
  end
  else
    Body.ZipComment := FComment;
  Result := 0;
end;

function TZMOpSetZipComment.Name: string;
begin
  Result := 'SetZipComment';
end;

end.
