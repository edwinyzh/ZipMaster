unit ZMOprMerge;

// ZMMergeOpr.pas - MergeZipped operation

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
  TZMOpMerge = class(TZMOperationRoot)
  private
    FOpts: TZMMergeOpts;
  public
    constructor Create(Opts: TZMMergeOpts);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Windows, System.SysUtils, VCL.Graphics, VCL.Dialogs, VCL.Controls,
{$ELSE}
  Windows, SysUtils, Graphics, Dialogs, Controls,
{$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMLister, ZMBody, ZMMisc, ZMArgSplit, ZMZipBase, ZMZipReader, ZMZipWriter,
  ZMZipDirectory, ZMUnzipOpr, ZMXcpt, ZMStructs, ZMUtils, ZMDlg, ZMCtx,
  ZMMsg, ZMDrv, ZMZipMulti, ZMCore;

const
  __UNIT__ = 30;

type
  TSFXOps = (SfoNew, SfoZip, SfoExe);

type
  TMOArgOptions = record
    Arg: string;
    Excludes: string;
    NFlag: Boolean;
    XArg: string;
  end;

type
  // provides support for auto-open of source zips
  TZMZipMerger = class(TZMZipCopier)
  private
    FLastOpened: TZMZipBase;
    procedure SetLastOpened(const Value: TZMZipBase);
  protected
    function CommitRec(Rec: TZMEntryWriter): Int64; override;
    property LastOpened: TZMZipBase read FLastOpened write SetLastOpened;
  public
    procedure AfterConstruction; override;
  end;

  TZMEntryMerger = class(TZMEntryCopier)
  private
    FKeep: Boolean;
  protected
    function GetTitle: string; override;
  public
    procedure AfterConstruction; override;
    function Process: Int64; override;
    property Keep: Boolean read FKeep write FKeep;
  end;

type
  TZMMergeOpr = class(TZMUnzipOpr)
  private
    procedure AssignArgOptions(var Locals: TMOArgOptions;
      const Args: TMOArgOptions);
    procedure ClearAppend;
    function CommitAppend: Integer;
    function ExtractChildZip(ParentIndex: Integer;
      const MyName: string): Integer;
    function FlattenExcludes(Excludes: TStrings): string;
    function IncludeAZip(const SourceName: string): Integer;
    function MergeAnotherZip(const SourceName: string): Integer;
    function MergeAnotherZip1(const SourceName: string): Integer;
    function MergeAnotherZipInZip(const SourceName: string): Integer;
    function MergeIntermediate(SelectedCount: Integer;
      const Opts: TZMMergeOpts): Integer;
    function MergeIntermedZip(RefZip: TZMZipReader; ZipIndex: Integer;
      Opts: TZMMergeOpts): Integer;
    function MergePrepareName(var ZName: string; ZRec: TZMEntryBase): Integer;
    function MergeResolveConflict(RefRec, NRec: TZMEntryCopier;
      const Opts: TZMMergeOpts): Integer;
    function MergeSafeName(ZipIndex: Integer; const Name: string): string;
    function Prepare(MustExist: Boolean; SafePart: Boolean = False)
      : TZMZipReader;
    function PrepareAppend: Boolean;
    function ProcessInclude(SrcZip: TZMZipReader;
      const Args: TMOArgOptions): Integer;
    function ProcessIncludeList(var SelectCount: Integer): Integer;
    function ResolveConfirm(const ExistName: string; ExistRec: TZMEntryBase;
      const ConflictName: string; ConflictRec: TZMEntryBase;
      const NewName: string): TZMResolutions;
    procedure SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
      const ParentExcludes: string);
    procedure SetupAppendRecs(Allow: Boolean);
  protected
    FOutZip: TZMZipCopier;
    FSkippedFiles: TStringList;
    FSplitter: TZMArgSplitter;
    FZipList: TZMZipList;
    procedure CreateInterimZip; override;
    function FinalizeInterimZip(OrigZip: TZMZipReader): Integer; override;
    function MergeWrite: Integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CleanZipName(const FileName: string): string;
    function MergeZippedFiles(Opts: TZMMergeOpts;
      TheDest: TZMZipWriter): Integer;
  end;

type
  TZMMergeArgs = class(TZMSelectArgs)
  private
    FFromDate: Cardinal;
    Flist: TStrings;
    FNFlag: Boolean;
    FXArg: string;
    FZipName: string;
  public
    function Accept(Rec: TZMEntryBase): Boolean; override;
    procedure AfterConstruction; override;
    procedure Assign(Other: TZMSelectArgs); override;
    property FromDate: Cardinal read FFromDate write FFromDate;
    property List: TStrings read Flist write Flist;
    property NFlag: Boolean read FNFlag write FNFlag;
    property XArg: string read FXArg write FXArg;
    property ZipName: string read FZipName write FZipName;
  end;

const
  DeleteIncludeListThreshold = 20;
  MergeIncludeListThreshold = 10;
  PrepareAppendThreshold = 10;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

procedure TZMMergeOpr.AfterConstruction;
begin
  inherited;
  FSkippedFiles := nil;
  FZipList := TZMZipList.Create;
  FSkippedFiles := TStringList.Create;
  FSplitter := TZMArgSplitter.Create;
end;

procedure TZMMergeOpr.AssignArgOptions(var Locals: TMOArgOptions;
  const Args: TMOArgOptions);
begin
  Locals.Arg := Args.Arg;
  Locals.Excludes := Args.Excludes;
  Locals.NFlag := Args.NFlag;
  Locals.XArg := Args.XArg;
end;

procedure TZMMergeOpr.BeforeDestruction;
begin
  FSkippedFiles.Free;
  FZipList.Free;
  FSplitter.Free;
  inherited;
end;

// rebuild file name without spaces
function TZMMergeOpr.CleanZipName(const FileName: string): string;
var
  Ancestors: string;
  Done: Boolean;
  Generation: string;
begin
  Result := '';
  Ancestors := FileName;
  repeat
    SplitQualifiedName(Ancestors, Ancestors, Generation);
    Done := Generation = '';
    if Done then
      Generation := Ancestors;
    if Result <> '' then
      Result := Generation + ZFILE_SEPARATOR + Result
    else
      Result := Generation;
  until Done;
end;

procedure TZMMergeOpr.ClearAppend;
begin
  SetupAppendRecs(False); // clear flags
end;

function TZMMergeOpr.CommitAppend: Integer;
var
  DstZip: TZMZipReader;
begin
  DstZip := FZipList[0];
  DstZip.File_Close; // don't aquire handle
  FOutZip.Stream := DstZip.ReleaseStream;
  FOutZip.ArchiveName := DstZip.ArchiveName;
  Result := FOutZip.File_Reopen(FmOpenReadWrite);
  if Result >= 0 then
    Result := FOutZip.Commit(ZwoZipTime in WriteOptions);
end;

// does not create a temperary file
procedure TZMMergeOpr.CreateInterimZip;
begin
  InterimZip := TZMZipMerger.Create(Lister);
end;

// Extract MyName from FZipList[ParentIndex] and add to FZipList if successful
function TZMMergeOpr.ExtractChildZip(ParentIndex: Integer;
  const MyName: string): Integer;
var
  Entry: TZMEntryBase;
  Idx: Integer;
  MyFile: TZMZipReader;
  Parent: TZMZipReader;
  ParentName: string;
  TheZip: TZMZipReader;
begin
  // be safe
  if (ParentIndex <= 0) or (MyName = '') then
  begin
    Result := ZM_Error({_LINE_}288, ZE_NoInFile);
    Exit;
  end;
  Parent := FZipList[ParentIndex];
  ParentName := Parent.ArchiveName;
  if ParentName = '' then
  begin
    Result := ZM_Error({_LINE_}295, ZE_NoInFile);
    Exit;
  end;
  TheZip := TZMZipReader.Create(Lister);
  try
    TheZip.ArchiveName := ParentName;
    Body.Trace('Loading parent zip', {_LINE_}301, __UNIT__);
    Result := TheZip.OpenZip(False, False);
    if Result >= 0 then
    begin
      // loaded ok - see if file exists
      Idx := -1;
      Entry := TheZip.SearchName(MyName, True, Idx);
      // find first matching pattern
      if Entry <> nil then
      begin
        // create a temporary file
        MyFile := TZMZipReader.Create(Lister);
        try
          MyFile.Alias :=
            QualifiedName(FZipList[ParentIndex].Name(False), MyName);
          MyFile.File_CreateTemp('Zcy', '');
          Result := UnzipAStream(MyFile.Stream, Entry);
          if Result = 0 then
          begin
            // good
            MyFile.File_Close;
            Result := MyFile.OpenZip(False, False);
            MyFile.File_Close;
            if Result >= 0 then
            begin
              Result := FZipList.Add(MyFile); // add to list and return Index
              MyFile.File_Close;
              if Result > 0 then
                MyFile := nil; // do not free
            end;
          end
          else
          begin
            // extracting zip failed
            MyFile.Position := 0;
            MyFile.SetEndOfFile;
            MyFile.File_Close;
            Result := ZM_Error({_LINE_}338, ZE_NoWrite);
            // error does not matter
          end;
        finally
          MyFile.Free;
        end;
      end;
    end;
  finally
    TheZip.Free;
  end;
end;

function TZMMergeOpr.FinalizeInterimZip(OrigZip: TZMZipReader): Integer;
begin
  if (InterimZip.ArchiveName = '') and not InterimZip.File_CreateTemp
    (PRE_INTER, '') then
    raise EZipMaster.CreateMsg(Body, ZE_NoOutFile, {_LINE_}355, __UNIT__);
  Result := inherited FinalizeInterimZip(OrigZip);
end;

function TZMMergeOpr.FlattenExcludes(Excludes: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  // flatten the list
  for I := 0 to Excludes.Count - 1 do
  begin
    if Excludes[I] = '' then
      Continue;
    if Result <> '' then
      Result := Result + SPEC_SEP;;
    Result := Result + Excludes[I];
  end;
end;

function TZMMergeOpr.IncludeAZip(const SourceName: string): Integer;
var
  Skip: TZMSkipTypes;
begin
  Result := FZipList.Find(SourceName); // already in list?
  if Result = 0 then
  begin
    Result := ZM_Error({_LINE_}382, ZE_SourceIsDest);
    Exit;
  end;
  if Result < 0 then
  begin
    // have not opened file yet _ not in list
    Result := MergeAnotherZip(SourceName);
    if AbsErr(Result) = ZE_SameAsSource then
      Exit;
    if Result = 0 then
    begin
      Result := ZM_Error({_LINE_}393, ZE_SourceIsDest);
      Exit;
    end;
    if Result < 0 then
    begin
      // file does not exist or could not be opened
      Body.InformFmt('Skipped missing or bad zip [%d] %s',
        [AbsErr(Result), SourceName], {_LINE_}400, __UNIT__);
      case AbsErr(Result) of
        ZE_NoInFile:
          Skip := StNotFound;
        ZE_FileOpen:
          Skip := StNoOpen;
      else
        Skip := StReadError;
      end;
      if not Skipping(SourceName, Skip, Result) then
        Result := 0; // we can ignore it
    end;
  end;
end;

// opens a zip file for merging and adds to FZipList
// return <0 _ error, >= 0 _ Index in FZipList
function TZMMergeOpr.MergeAnotherZip(const SourceName: string): Integer;
begin
  if Pos(ZFILE_SEPARATOR, SourceName) > 0 then
    Result := MergeAnotherZipInZip(SourceName) // handle Zip in Zip
  else
    Result := MergeAnotherZip1(SourceName);
end;

// opens a zip file for merging and adds to FZipList
// return <0 _ error, >= 0 _ Index in FZipList
function TZMMergeOpr.MergeAnotherZip1(const SourceName: string): Integer;
var
  Dzip: TZMZipReader;
  SrcZip: TZMZipReader;
begin
  // have not opened file yet
  SrcZip := TZMZipReader.Create(Lister);
  try
    begin
      SrcZip.ArchiveName := SourceName;
      Result := SrcZip.OpenZip(False, False);
      if Result >= 0 then
      begin
        Dzip := FZipList[0];
        if (SrcZip.WorkDrive.DriveLetter = Dzip.WorkDrive.DriveLetter) and
          (not Dzip.WorkDrive.DriveIsFixed) and
          (Dzip.MultiDisk or SrcZip.MultiDisk or (ZwoDiskSpan in WriteOptions))
        then
          Result := ZM_Error({_LINE_}445, ZE_SameAsSource)
        else
        begin
          Result := FZipList.Add(SrcZip); // add to list and return Index
          SrcZip.File_Close;
          if Result > 0 then
            SrcZip := nil; // accepted so do not free
        end;
      end;
    end;
  finally
    SrcZip.Free;
  end;
end;

// handle Zip in Zip
function TZMMergeOpr.MergeAnotherZipInZip(const SourceName: string): Integer;
var
  MyName: string;
  MyOwner: string;
begin
  SplitQualifiedName(SourceName, MyOwner, MyName);
  // find MyOwner
  Result := FZipList.Find(MyOwner);
  if Result < 0 then
  begin
    // not found _ maybe MyOwner not added yet
    Result := MergeAnotherZip(MyOwner); // recursively add parents
  end;
  if Result > 0 then
  begin
    // we have owner in FZipList
    // open MyOwner and extract MyName to temporary
    Result := ExtractChildZip(Result, MyName);
  end;
end;

// create the intermediate from DstZip and selected entries
function TZMMergeOpr.MergeIntermediate(SelectedCount: Integer;
  const Opts: TZMMergeOpts): Integer;
var
  MaxCount: Integer;
  RefZip: TZMZipReader;
  RefZipIndex: Integer;
  SelCount: Integer;
begin
  Result := 0;
  PrepareInterimZip;
  FOutZip := InterimZip as TZMZipMerger;
  // replicate all records and settings
  FOutZip.Select('*', ZzsSet); // want all (initially)
  Progress.NewXtraItem(ZxMerging, FZipList.Count);
  // add the possibly altered selected entries
  // [0] is 'DstZip'
  MaxCount := 0;
  for RefZipIndex := 0 to FZipList.Count - 1 do
    MaxCount := MaxCount + FZipList[RefZipIndex].SelCount;
  FOutZip.HTAutoSize(MaxCount);
  for RefZipIndex := 0 to FZipList.Count - 1 do
  begin
    Progress.AdvanceXtra(1);
    CheckCancel;
    RefZip := FZipList[RefZipIndex];
    SelCount := RefZip.SelCount;
    Body.TraceFmt('Processing %d files: %s', [SelCount, RefZip.Name],
      {_LINE_}510, __UNIT__);
    if SelCount < 1 then
      Continue;
    Result := MergeIntermedZip(RefZip, RefZipIndex, Opts);
  end;
end;

// Merge selected entries from RefZip into FOutZip
function TZMMergeOpr.MergeIntermedZip(RefZip: TZMZipReader; ZipIndex: Integer;
  Opts: TZMMergeOpts): Integer;
var
  Cnt: Integer;
  NewRec: TZMEntryMerger;
  Rec: TZMEntryBase;
  RefRec: TZMEntryCopier;
  Skp: TZMSkipTypes;
  ZName: string;
  ZRec: TZMEntryBase;
begin
  Result := 0; // keep compiler happy
  // process each selected entry
  Rec := RefZip.FirstSelected;
  Cnt := 0;
  while (Rec <> nil) do
  begin
    Inc(Cnt);
    if (Cnt and 63) = 0 then
      CheckCancel;
    Result := 0;
    ZRec := Rec;
    Rec := RefZip.NextSelected(Rec);
    ZName := ZRec.FileName;
    if ZipIndex > 0 then
    begin
      Result := MergePrepareName(ZName, ZRec);
      if Result < 0 then
        Break; // error
      if ZName = '' then
        Continue; // entry ignored by user
    end;
    // make new CopyRec
    NewRec := TZMEntryMerger.Create(FOutZip); // make a entry
    NewRec.AssignFrom(ZRec);
    NewRec.Link := ZRec; // link to original
    if Result = 1 then
    begin
      NewRec.SetStatusBit(ZsbRenamed);
      Result := NewRec.ChangeName(ZName);
      if Result < 0 then
      begin
        NewRec.Free; // stop leak
        Body.InformFmt('Failed rename [%d] %s to %s',
          [-Result, ZRec.FileName, ZName], {_LINE_}562, __UNIT__);
        Skp := StUser;
        case AbsErr(Result) of
          ZE_BadFileName:
            Skp := StBadName;
          ZE_DuplFileName:
            Skp := StDupName;
        end;
        if Skp = StUser then
          Break; // unknown error - fatal
        if Skipping(QualifiedName(RefZip.Name, ZName), Skp, Result) then
          Break; // fatal
        Continue; // ignore
      end;
      Body.InformFmt('Renamed %s to %s', [ZRec.FileName, NewRec.FileName],
        {_LINE_}577, __UNIT__);
    end;
    // does it exist
    RefRec := TZMEntryCopier(FOutZip.FindName(ZName, nil));
    Result := 0;
    if RefRec <> nil then
    begin
      // duplicate found - resolve it
      Result := MergeResolveConflict(RefRec, NewRec, Opts);
    end;
    if Result = 0 then
    begin
      Result := FOutZip.Add(NewRec); // use NewRec
      FOutZip.HTAdd(NewRec, False);
    end
    else
      NewRec.Free; // stop leak
    if Result < 0 then
      Break;
  end;
end;

// returns <0 = error, 0 = no change, 1 = changed, 2 = ignored by user
function TZMMergeOpr.MergePrepareName(var ZName: string;
  ZRec: TZMEntryBase): Integer;
var
  Args: TZMMergeArgs;
  Changed: Boolean;
  FileName: string;
  Old: string;
  Sep: Integer;
  Subst: string;
  TmpOnSetAddName: TZMSetAddNameEvent;
  ZipName: string;
begin
  Result := 0;
  ZName := ZRec.FileName;
  Args := TZMMergeArgs(ZRec.SelectArgs);

  ZipName := ZRec.MyFile.Name;
  if (Args = nil) or not Args.NFlag then
  begin
    TmpOnSetAddName := Master.OnSetAddName;
    if Assigned(TmpOnSetAddName) then
    begin
      Changed := False;
      FileName := ZName;
      TmpOnSetAddName(Master, FileName, QualifiedName(ZipName,
        FileName), Changed);
      if Changed then
      begin
        Result := 1; // changed
        // verify valid
        if FileName = '' then
        begin
          Result := ZM_Error({_LINE_}632, ZE_EntryCancelled);
          if not Skipping(QualifiedName(ZipName, ZRec.FileName), StUser, Result)
          then
            Result := 2; // ignore entry
          ZName := FileName;
          Exit;
        end;
        FileName := SetSlash(FileName, PsdExternal);
        ZName := FileName;
      end;
    end;
  end;
  if (Args <> nil) and (Args.XArg <> '') then
  begin
    Old := Args.XArg;
    Sep := Pos('::', Old);
    Subst := Copy(Old, Sep + 2, 2048);
    Old := Copy(Old, 1, Sep - 1);
    if Old = '' then
      FileName := Subst + ZName
    else
    begin
      Old := SetSlash(Old, PsdExternal);
      if Pos(Old, ZName) <= 0 then
      begin
        // no change
        Result := 0;
        Exit;
      end;
      FileName := StringReplace(ZName, Old, Subst,
        [RfReplaceAll, RfIgnoreCase]);
    end;
    FileName := SetSlash(FileName, PsdExternal);
    Result := 1; // changed
    // verify valid
    ZName := FileName;
  end;
end;

// return <0 = error, 0 = use NRec, 1 = discard NRec
function TZMMergeOpr.MergeResolveConflict(RefRec, NRec: TZMEntryCopier;
  const Opts: TZMMergeOpts): Integer;
var
  ConflictZipName: string;
  NewName: string;
  RefZipName: string;
  Resolve: TZMResolutions;
begin
  RefZipName := RefRec.Link.MyFile.Name;
  ConflictZipName := NRec.Link.MyFile.Name;
  if Verbosity >= ZvTrace then
    Body.TraceFmt('Found conflict for %s in %s',
      [QualifiedName(RefZipName, RefRec.FileName), ConflictZipName],
      {_LINE_}685, __UNIT__);
  Resolve := ZmrConflicting;
  NewName := MergeSafeName(1, RefRec.FileName);
  case Opts of
    ZmoConfirm:
      Resolve := ResolveConfirm(RefZipName, RefRec, ConflictZipName,
        NRec, NewName);
    ZmoAlways:
      ;
    ZmoNewer:
      if RefRec.ModifDateTime >= NRec.ModifDateTime then
        Resolve := ZmrExisting;
    ZmoOlder:
      if RefRec.ModifDateTime <= NRec.ModifDateTime then
        Resolve := ZmrExisting;
    ZmoNever:
      Resolve := ZmrExisting;
    ZmoRename:
      Resolve := ZmrRename;
  end;
  Result := 0;
  case Resolve of
    ZmrExisting:
      begin
        Result := Body.PrepareErrMsg(ZE_Existing, [NRec.FileName],
          {_LINE_}710, __UNIT__);
        if not Skipping(QualifiedName(ConflictZipName, NRec.FileName),
          StFileExists, Result) then
        begin
          NRec.Selected := False;
          NRec.SetStatusBit(ZsbDiscard);
          Result := 1; // discard NRec
        end;
      end;
    ZmrConflicting:
      begin
        Result := Body.PrepareErrMsg(ZE_Existing, [RefRec.FileName],
          {_LINE_}722, __UNIT__);
        if not Skipping(QualifiedName(RefZipName, RefRec.FileName),
          StFileExists, Result) then
        begin
          RefRec.Selected := False;
          RefRec.SetStatusBit(ZsbDiscard);
          Result := 0;
        end;
      end;
    ZmrRename:
      begin
        // zmrRename _ keep RefRec, rename and keep NRec
        NRec.SetStatusBit(ZsbRenamed);
        Result := NRec.ChangeName(NewName);
        if (Result < 0) then
        begin
          Result := Body.PrepareErrMsg(ZE_Existing, [RefRec.FileName],
            {_LINE_}739, __UNIT__);
          if not Skipping(QualifiedName(RefZipName, RefRec.FileName),
            StFileExists, Result) then
            Result := 1; // ignore
        end
        else
        begin
          NRec.Selected := True;
          NRec.SetStatusBit(ZsbRenamed);
          Result := 0;
        end;
      end;
  end;
end;

function TZMMergeOpr.MergeSafeName(ZipIndex: Integer;
  const Name: string): string;
var
  EName: string;
  Extn: string;
  N: Cardinal;
begin
  EName := ExtractFilePath(Name) + ExtractNameOfFile(Name);
  Extn := ExtractFileExt(Name);
  N := 0;
  repeat
    if N = 0 then
      Result := Format('%s[%d]%s', [EName, ZipIndex, Extn])
    else
      Result := Format('%s[%d.%x]%s', [EName, ZipIndex, N, Extn]);
    Inc(N);
  until (FOutZip.FindName(Result, nil) = nil);
end;

function TZMMergeOpr.MergeWrite: Integer;
var
  DstZip: TZMZipReader; // orig dest zip (read only)
  Existed: Boolean;
  Includes: Integer;
  Rec: TZMEntryCopier;
  RenamedCnt: Integer;
  ReplaceCnt: Integer;
  Rubbish: Integer;
  SourceZip: TZMZipReader;
  TotalCnt: Integer;
  WillSplit: Boolean;
  ZRec: TZMEntryBase;
begin
  // find total files in dest and list of files added/replaced
  DstZip := FZipList[0];
  Body.ClearIncludeSpecs; // add names of files to be added
  Includes := 0;
  TotalCnt := 0;
  ReplaceCnt := 0;
  Rubbish := 0;
  RenamedCnt := 0;
  ZRec := FOutZip.FirstRec;
  while ZRec <> nil do
  begin
    CheckCancel;
    Rec := TZMEntryCopier(ZRec);
    ZRec := ZRec.Next;
    if Rec.StatusBit[ZsbError or ZsbDiscard or ZsbSelected] <> ZsbSelected then
    begin
      Inc(Rubbish);
      Continue;
    end;
    SourceZip := TZMZipReader(Rec.Link.MyFile);
    if Rec.StatusBit[ZsbDiscard] <> 0 then
    begin
      if SourceZip = DstZip then
        Inc(ReplaceCnt);
      Inc(Rubbish);
      Continue;
    end;
    Inc(TotalCnt);
    if SourceZip = DstZip then
      Continue;
    // count and mark for later adding to FSpecArgs
    if Rec.StatusBit[ZsbRenamed] <> 0 then
      Inc(RenamedCnt);
    Rec.Status[ZsbHail] := True;
    Inc(Includes);
  end;
  Body.TraceFmt('Total=%d, Replaced=%d, New=%d, Discarded=%d, Renamed=%d',
    [TotalCnt, ReplaceCnt, Includes, Rubbish, RenamedCnt], {_LINE_}824,
    __UNIT__);
  if (TotalCnt < 1) or (Includes < 1) then
  begin
    Body.Inform('nothing to do', {_LINE_}828, __UNIT__);
    Result := ZM_Error({_LINE_}829, ZE_NothingToDo);
    if not Skipping(DstZip.Name, StNothingToDo, Result) then
      Result := 0;
    Exit;
  end;
  // can we just append to original
  Existed := (Zfi_Loaded and DstZip.Info) <> 0;
  WillSplit := DstZip.MultiDisk or
    ((not Existed) and (ZwoDiskSpan in WriteOptions));

  if (not WillSplit) or (not(ZwoSafe in WriteOptions)) then
  begin
    if not Existed then
    begin // need to create the new file
      // write new file
      FOutZip.ArchiveName := DstZip.ArchiveName;
      FOutZip.ZipComment := Lister.ZipComment; // keep orig
      ShowProgress := ZspFull;
      if Assigned(DstZip.Stub) and DstZip.UseSFX then
      begin
        FOutZip.AssignStub(DstZip);
        FOutZip.UseSFX := True;
      end;
      Result := ZM_Error({_LINE_}852, ZE_NoOutFile);
      FOutZip.File_Create(DstZip.ArchiveName);
      if FOutZip.IsOpen then
      begin
        Result := FOutZip.Commit(ZwoZipTime in WriteOptions);
        FOutZip.File_Close;
        FZipList.CloseAll; // close all source files
      end;
      if Result < 0 then
        Body.InformFmt('Merging new file failed: %d', [AbsErr(Result)],
          {_LINE_}862, __UNIT__);
      Exit;
    end
    else
    begin
      // try to append to existing zip
      if PrepareAppend then
      begin
        // commit it
        Result := CommitAppend;
        if Result >= 0 then
          Body.Trace('Merging append successful', {_LINE_}873, __UNIT__);
        if AbsErr(Result) <> ZE_NoAppend then
          Exit;
        // fix to allow safe write
        ClearAppend;
        Body.InformFmt('Merging append failed: %d', [-Result],
          {_LINE_}879, __UNIT__);
      end;
    end;
  end;

  // write to intermediate
  if WillSplit then
    FOutZip.File_CreateTemp(PRE_INTER, '')
  else
    FOutZip.File_CreateTemp(PRE_INTER, DstZip.ArchiveName);
  if not FOutZip.IsOpen then
  begin
    Result := ZM_Error({_LINE_}891, ZE_NoOutFile);
    Exit;
  end;
  if not WillSplit then
  begin
    // initial temporary destination
    if Assigned(DstZip.Stub) and DstZip.UseSFX then
    begin
      FOutZip.AssignStub(DstZip);
      FOutZip.UseSFX := True;
    end;
    FOutZip.DiskNr := 0;
  end;
  FOutZip.ZipComment := DstZip.ZipComment; // keep orig
  ShowProgress := ZspFull;
  // now we write it
  Result := FOutZip.Commit(ZwoZipTime in WriteOptions);
  FOutZip.File_Close;
  FZipList.CloseAll; // close all source files
  if Result >= 0 then
  begin
    if (FOutZip.Count - Rubbish) <> TotalCnt then
      Result := ZM_Error({_LINE_}913, ZE_InternalError)
    else
      Result := Recreate(FOutZip, DstZip); // all correct so Recreate source
  end;
end;

// IncludeSpecs = Zips and files to include
// zipname
// ExcludeSpecs = files to exclude
function TZMMergeOpr.MergeZippedFiles(Opts: TZMMergeOpts;
  TheDest: TZMZipWriter): Integer;
var
  DstZip: TZMZipReader;
  SelectCount: Integer;
begin
  if Body.Logging then
    Body.LogSpecs('');
  ShowProgress := ZspFull;
  FOutZip := nil; // will be used to write the output
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsg(Body, ZE_InvalidArguments, {_LINE_}933,
      __UNIT__);
  if ZipFileName = '' then
    raise EZipMaster.CreateMsg(Body, ZE_NoZipSpecified, {_LINE_}936, __UNIT__);
  FZipList.Clear;
  FSkippedFiles.Clear;
  if TheDest <> nil then
    DstZip := TheDest
  else
    DstZip := Prepare(False, True);
  DstZip.Select('*', ZzsSet); // initial want all
  FZipList.ProtectZero := True; // do not destroy DstZip
  FZipList.Add(DstZip);
  // add source zips to list and select their files
  Result := ProcessIncludeList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsg(Body, Result, 0, 0);
  Body.ClearIncludeSpecs; // for new/updated files
  if SelectCount >= 1 then
  begin
    // add all selected to OutZip
    Result := MergeIntermediate(SelectCount, Opts);
    if Result < 0 then
      raise EZipMaster.CreateMsg(Body, Result, 0, 0);
    // we have processed list now resolve merge conflicts
    // write the results
    if Result >= 0 then
      Result := MergeWrite;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    Reload := ZlrReload; // force reload
  end;
  if Result < 0 then
    raise EZipMaster.CreateMsg(Body, Result, 0, 0);
  // it was successful
  SuccessCnt := IncludeSpecs.Count;
  FZipList.Clear;
  FSkippedFiles.Clear;
  if Body.Logging then
    Body.LogSpecs('');
end;

(* TZMMergeOpr.Prepare
  Prepare destination and get SFX stub as needed
*)
function TZMMergeOpr.Prepare(MustExist: Boolean; SafePart: Boolean = False)
  : TZMZipReader;
var
  Err: Integer;
begin
  Result := CurrentZip(MustExist, SafePart);
  Err := PrepareZip(Result);
  if Err < 0 then
    raise EZipMaster.CreateMsg(Body, Err, 0, 0);
end;

// prepare to commit by appending to orig file, return false if not possible
function TZMMergeOpr.PrepareAppend: Boolean;
var
  DstZip: TZMZipReader;
  HighKept: Int64;
  I: Integer;
  LocalOfs: Int64;
  LowDiscard: Int64;
  OrigCnt: Integer;
  Rec: TZMEntryMerger;
  ShowXProgress: Boolean;
  ZRec: TZMEntryBase;
begin
  Result := False;
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    Exit;
  // check can append
  ShowXProgress := OrigCnt > PrepareAppendThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(ZxProcessing, IncludeSpecs.Count);
  LowDiscard := DstZip.SOCOfs;
  HighKept := -1;
  ZRec := FOutZip.FirstRec;
  I := 0;
  while (ZRec <> nil) and (I < OrigCnt) do
  begin
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Rec := TZMEntryMerger(ZRec);
    ZRec := ZRec.Next;
    Inc(I);
    if (Rec = nil) or (Rec.StatusBit[ZsbError] <> 0) then
      Continue;
    LocalOfs := Rec.RelOffLocalHdr;
    if Rec.StatusBit[ZsbDiscard] <> 0 then
    begin
      if LocalOfs < LowDiscard then
      begin
        LowDiscard := LocalOfs;
        if HighKept > LowDiscard then
          Exit; // would produce a hole
      end;
    end
    else
    begin
      if LocalOfs > HighKept then
      begin
        HighKept := LocalOfs;
        if HighKept > LowDiscard then
          Exit; // would produce a hole
      end;
    end;
  end;
  Body.Trace('Should be able to append', {_LINE_}1045, __UNIT__);
  SetupAppendRecs(True);
  Result := True;
end;

// select files in Source, return files selected or <0 = error
function TZMMergeOpr.ProcessInclude(SrcZip: TZMZipReader;
  const Args: TMOArgOptions): Integer;
var
  MergeArgs: TZMMergeArgs;
begin
  if Verbosity > ZvVerbose then
    Body.TraceFmt('Including: %s', [QualifiedName(SrcZip.Name, Args.Arg)],
      {_LINE_}1058, __UNIT__);
  MergeArgs := TZMMergeArgs.Create;
  MergeArgs.ZipName := SrcZip.ArchiveName;
  MergeArgs.FromDate := DateTimeToFileDate(Lister.AddFrom);
  MergeArgs.List := FSkippedFiles;
  MergeArgs.XArg := Args.XArg;
  MergeArgs.NFlag := Args.NFlag;
  Result := SrcZip.SelectRec(Args.Arg, Args.Excludes, ZzsSet, MergeArgs);
  if Result < 1 then
  begin
    // none found
    MergeArgs.Free;
    Result := ZM_Error({_LINE_}1070, ZE_NotFound);
    if not Skipping(QualifiedName(SrcZip.Name(), Args.Arg), StNotFound, Result)
    then
      Result := 0;
  end
  else
    SrcZip.AddSelectArgs(MergeArgs);
end;

// returns <0 _ error
(*
 [zipname] [switch] [[switch] ...]
 switches
 /N[+ or -]  flags not to use AddNewName (default N- _ use AddNewName)
 /X:[old]::[new]  replace 'old' with 'new' - must result in valid internal name
 >>spec  before any source zip is specified sets the default select spec
 >>spec  select files in current zip according to spec
 /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to
 globals otherwise use spec
 changes to excludes occur at current line and continue until changed
 - does not change already included files.
 when used on same line as '>>' it only applies to that line
 and modifies the 'current' excludes.
*)
function TZMMergeOpr.ProcessIncludeList(var SelectCount: Integer): Integer;
var
  DefaultExcludes: string;
  Effectives: TMOArgOptions;
  I: Integer;
  Locals: TMOArgOptions;
  ShowXProgress: Boolean;
  ZipsIndex: Integer;
begin
  Result := 0;
  SelectCount := 0;
  DefaultExcludes := FlattenExcludes(ExcludeSpecs);
  Effectives.Excludes := DefaultExcludes;
  Effectives.Arg := '*.*';
  Effectives.NFlag := False; // N- or none
  Effectives.XArg := '';
  FSplitter.Allow := '>ENX';
  FSplitter.Options := [ZaoLastSpec, ZaoWildSpec, ZaoMultiSpec];
  // locate source zips and their files
  ZipsIndex := ZM_Error({_LINE_}1113, ZE_NoInFile); // no current yet
  ShowXProgress := IncludeSpecs.Count > MergeIncludeListThreshold;
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
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> ZasNone then
      raise EZipMaster.CreateMsgFmt(Body, ZE_InvalidParameter, [FSplitter.Raw],
        {_LINE_}1128, __UNIT__);
    if FSplitter.Main <> '' then
    begin
      // we are specifying a zip to process
      ZipsIndex := IncludeAZip(FSplitter.Main);
      Result := ZipsIndex;
      if Result < 0 then
        Break; // error
      if Result = 0 then
        Continue; // skipped it
      // process spec and/or switches
    end
    else // no zip specified
    begin
      // ignore empty lines
      if FSplitter.Found = '' then
        Continue;
      // any zips specified yet
      if ZipsIndex < 0 then
      begin
        // none yet, set defaults
        SetArgsOptionsFromSplitter(Effectives, DefaultExcludes);
        Continue;
      end;
    end;
    if (Result < 0) or (ZipsIndex < 0) then
      Break; // must have open file
    if FSplitter.Has(ZSPECARG) or (FSplitter.Main <> '') then
    begin
      // using local settings from splitter
      AssignArgOptions(Locals, Effectives);
      SetArgsOptionsFromSplitter(Locals, Effectives.Excludes);
      // include a spec in the current zip
      Result := ProcessInclude(FZipList[ZipsIndex], Locals);
      if Result > 0 then
        SelectCount := SelectCount + Result;
      Continue;
    end;
    // Only have switches _ Set effectives
    SetArgsOptionsFromSplitter(Effectives, DefaultExcludes);
  end;
end;

function TZMMergeOpr.ResolveConfirm(const ExistName: string;
  ExistRec: TZMEntryBase; const ConflictName: string; ConflictRec: TZMEntryBase;
  const NewName: string): TZMResolutions;
var
  ConflictEntry: TZM_ConflictEntry;
  ExistEntry: TZM_ConflictEntry;
  Response: Integer;
  TmpZippedConflict: TZMMergeZippedConflictEvent;
begin
  // Do we have a event assigned for this then don't ask.
  TmpZippedConflict := Master.OnMergeZippedConflict;
  if Assigned(TmpZippedConflict) then
  begin
    ConflictEntry := nil;
    ExistEntry := TZM_ConflictEntry.Create(Master, ExistRec);
    try
      ExistEntry.ZipName := ExistName;
      ConflictEntry := TZM_ConflictEntry.Create(Master, ConflictRec);
      ConflictEntry.ZipName := ConflictName;
      Result := ZmrRename;
      TmpZippedConflict(Master, ExistEntry, ConflictEntry, Result);
    finally
      ExistEntry.Free;
      ConflictEntry.Free;
    end;
    Exit;
  end;
  Response := ZipMessageDlgEx(ZipLoadStr(ZC_FileConflict),
    Format(ZipLoadStr(ZC_Merge), [QualifiedName(ExistName, ExistRec.XName),
    QualifiedName(ConflictName, ConflictRec.XName), NewName]),
    ZmtConfirmation + DHC_CpyZipOvr, [MbYes, MbNo, MbIgnore]);
  if Response = MrOk then
    Result := ZmrRename
  else
    if Response = MrNo then
      Result := ZmrExisting
    else
      Result := ZmrConflicting;
end;

procedure TZMMergeOpr.SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
  const ParentExcludes: string);
var
  Xc: string;
begin
  if FSplitter.Has(ZSPECARG) then
  begin
    Args.Arg := FSplitter.Arg(ZSPECARG);
    if Args.Arg = '' then
      Args.Arg := '*.*';
  end;
  if FSplitter.Has('E') then
  begin
    Xc := FSplitter.Arg('E');
    if (Xc <> '') and (Xc[1] = SPEC_SEP) then
    begin
      if Xc <> SPEC_SEP then
        Args.Excludes := ParentExcludes + Xc;
    end
    else
      Args.Excludes := Xc;
  end;
  if FSplitter.Has('N') then
    Args.NFlag := FSplitter.Arg('N') = '+';
  if FSplitter.Has('X') then
    Args.XArg := FSplitter.Arg('X');
end;

procedure TZMMergeOpr.SetupAppendRecs(Allow: Boolean);
var
  DstZip: TZMZipReader;
  I: Integer;
  OrigCnt: Integer;
  Rec: TZMEntryMerger;
  ZRec: TZMEntryBase;
begin
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    Exit;
  I := 0;
  ZRec := FOutZip.FirstRec;
  while (ZRec <> nil) and (I < OrigCnt) do
  begin
    CheckCancel;
    Rec := TZMEntryMerger(ZRec);
    ZRec := ZRec.Next;
    Inc(I);
    if (Rec = nil) or (Rec.StatusBit[ZsbError or ZsbDiscard or ZsbSelected] <>
      ZsbSelected) then
      Continue;
    Rec.Keep := Allow;
  end;
end;

{ TZMZipMerger }

procedure TZMZipMerger.AfterConstruction;
begin
  inherited;
  LastOpened := nil;
end;

function TZMZipMerger.CommitRec(Rec: TZMEntryWriter): Int64;
var
  Err: Integer;
  InFile: TZMZipBase;
  MergeRec: TZMEntryMerger;
begin
  if (not(Rec is TZMEntryMerger)) or (TZMEntryMerger(Rec).Link = nil) then
  begin
    Result := inherited CommitRec(Rec);
    Exit;
  end;
  MergeRec := TZMEntryMerger(Rec);
  if MergeRec.Keep then
  begin
    Result := MergeRec.Process;
  end
  else
  begin
    Result := MergeRec.ProcessSize;
    if Result > 0 then
    begin
      InFile := MergeRec.Link.MyFile;
      if (not InFile.IsOpen) and (InFile <> LastOpened) then
      begin
        if IsTrace then // Verbosity >= zvTrace then
          Body.TraceFmt('Opening %s', [InFile.Name(True)], {_LINE_}1299,
            __UNIT__);
        if FLastOpened <> nil then
          FLastOpened.File_Close;
        LastOpened := InFile;
        Err := InFile.File_Reopen(FmOpenRead or FmShareDenyWrite); // open it
        if Err < 0 then
        begin
          Result := Err;
          Exit;
        end;
      end;
      Result := MergeRec.Process;
    end;
  end;
end;

procedure TZMZipMerger.SetLastOpened(const Value: TZMZipBase);
begin
  if FLastOpened <> Value then
  begin
    if FLastOpened <> nil then
      FLastOpened.File_Close;
    FLastOpened := Value;
  end;
end;

procedure TZMEntryMerger.AfterConstruction;
begin
  inherited;
  Keep := False;
end;

function TZMEntryMerger.GetTitle: string;
begin
  Result := QualifiedName(Link.MyFile.Name, Link.FileName) + ' :: ' + FileName;
end;

function TZMEntryMerger.Process: Int64;
var
  LOH: TZipLocalHeader;
  Nxt: Int64;
begin
  if not Keep then
  begin
    Result := inherited Process;
    Exit;
  end;
  // verify at correct header
  Result := SeekLocalData(LOH, _FileName);
  if Result >= 0 then
  begin
    Nxt := RelOffLocalHdr + ProcessSize;
    // Good - Position to next entry
    if MyFile.Seek(Nxt, SoBeginning) = Nxt then
      Result := 0
    else
      Result := ZM_Error({_LINE_}1356, ZE_SeekError);
  end
  else
    if Result < 0 then
      Result := ZM_Error({_LINE_}1360, ZE_NoAppend);
end;

// return false = reject, too old
function TZMMergeArgs.Accept(Rec: TZMEntryBase): Boolean;
begin
  Result := True;
  if FromDate <> 0 then
    Result := FromDate <= Rec.ModifDateTime;
end;

procedure TZMMergeArgs.AfterConstruction;
begin
  inherited;
end;

procedure TZMMergeArgs.Assign(Other: TZMSelectArgs);
var
  Src: TZMMergeArgs;
begin
  inherited;
  if Other is TZMMergeArgs then
  begin
    Src := TZMMergeArgs(Other);
    FFromDate := Src.FromDate;
    Flist := Src.List;
    FNFlag := Src.NFlag;
    FXArg := Src.XArg;
    FZipName := Src.ZipName;
  end;
end;

constructor TZMOpMerge.Create(Opts: TZMMergeOpts);
begin
  inherited Create;
  FOpts := Opts;
end;

{ TZMOpMerge }

function TZMOpMerge.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpMerge.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMMergeOpr;
begin
  FOper := TZMMergeOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.MergeZippedFiles(FOpts, nil);
end;

function TZMOpMerge.Name: string;
begin
  Result := 'MergeZippedFiles';
end;

function TZMOpMerge.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

end.





