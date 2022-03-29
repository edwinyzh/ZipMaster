unit ZMOprDel;

// ZMDelOpr.pas -  Delete operations

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
  ZMHandler;

type
  TZMOpDelete = class(TZMOperationRoot)
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
  System.SysUtils, WinApi.Windows, VCL.Graphics, VCL.Dialogs, VCL.Controls,
{$ELSE}
  Dialogs, Controls, SysUtils, Windows, Graphics,
{$ENDIF}
  ZipMstr, ZMLister, ZMBody, ZMBaseOpr, ZMMisc,
  ZMArgSplit, ZMZipBase, ZMZipReader, ZMZipWriter, ZMZipDirectory, ZMXcpt,
  ZMStructs, ZMUtils, ZMMsg, ZMCore;

const
  __UNIT__ = 27;

const
  DeleteIncludeListThreshold = 20;
  MergeIncludeListThreshold = 10;
  PrepareAppendThreshold = 10;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

type
  TZMDelOpr = class(TZMBaseOpr)
  private
    FSkippedFiles: TStringList;
    FSplitter: TZMArgSplitter;
    FZipList: TZMZipList;
    function Delete1: Integer;
    function DeleteByList: Integer;
    function DeleteSelectedFiles(Zip: TZMZipReader): Integer;
    function FlattenExcludes(Excludes: TStrings): string;
    function ProcessDelete(SrcZip: TZMZipReader;
      const Arg, Excludes: string): Integer;
    function ProcessDeleteList(var SelectCount: Integer): Integer;
  protected
    procedure CreateInterimZip; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Delete: Integer;
  end;

procedure TZMDelOpr.AfterConstruction;
begin
  inherited;
  FSkippedFiles := nil;
  FZipList := TZMZipList.Create;
  FSkippedFiles := TStringList.Create;
  FSplitter := TZMArgSplitter.Create;
end;

procedure TZMDelOpr.BeforeDestruction;
begin
  FSkippedFiles.Free;
  FZipList.Free;
  FSplitter.Free;
  inherited;
end;

procedure TZMDelOpr.CreateInterimZip;
begin
  InterimZip := TZMZipCopier.Create(Lister);
  if not InterimZip.File_CreateTemp(PRE_INTER, '') then
    raise EZipMaster.CreateMsg(Body, ZE_NoOutFile, {_LINE_}138, __UNIT__);
end;

function TZMDelOpr.Delete: Integer;
begin
  if Body.Logging then
    Body.LogSpecs('');
  if ZipFileName <> '' then
  begin
    // do it the old way
    if (Current.Count < 1) or (IncludeSpecs.Count = 0) then
      Result := ZM_Error({_LINE_}149, ZE_NothingToDel)
    else
      Result := Delete1;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    if AbsErr(Result) <> ZE_NothingToDel then
      Reload := ZlrReload;
  end
  else
    Result := DeleteByList;
  if Result < 0 then
    ShowError(Result)
    // ShowZipMessage(Result, '')
  else
    SuccessCnt := Result;
  if Body.Logging then
    Body.LogSpecs('');
end;

(* ? TZMModOpr.Delete1
  Deletes files specified in FSpecArgs from current Zip
 exit: FSpecArgs = files deleted,
 FSpecArgsExcl = files skipped
 Result = >=0 number of files deleted, <0 error
*)
function TZMDelOpr.Delete1: Integer;
var
  CurZip: TZMZipReader;
  DelCnt: Integer;
  DropCount: Integer;
  Rec: TZMEntryBase;
begin
  CurZip := CurrentZip(True, False); // prepare the Current zip
  PrepareZip(CurZip);
  Result := 0;
  DelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs);
  Body.ClearIncludeSpecs; // will contain files deleted
  if DelCnt < 0 then
  begin
    Result := DelCnt;
    Exit;
  end;
  ASSERT(DelCnt = CurZip.SelCount, 'selcount wrong 1');
  if (CurZip.Count - DelCnt) < 1 then
  begin
    // no files left
    CurZip.File_Close;
    File_Delete(CurZip.ArchiveName);
    Result := DelCnt; // number of files deleted
  end
  else
  begin
    PrepareInterimZip;
    DropCount := 0;
    Rec := CurZip.FirstRec;
    while Rec <> nil do
    begin
      // Copy nonselected entries to IntermedZip
      if Rec.StatusBit[ZsbSelected] <> ZsbSelected then
      begin
        if TZMZipCopier(InterimZip).AffixZippedFile(Rec) = nil then
        begin
          Result := Body.PrepareErrMsg(ZE_DuplFileName, [Rec.FileName],
            {_LINE_}212, __UNIT__);
          Break; // fatal
        end;
      end
      else
      begin
        IncludeSpecs.Add(Rec.FileName);
        Inc(DropCount);
      end;
      Rec := Rec.Next;
      CheckCancel;
    end;
    if (Result = 0) and (DropCount > 0) then
    begin
      // write results
      FinalizeInterimZip(CurZip);
      Result := DropCount;
    end;
  end;
  CurZip.Invalidate;
  Current := nil; // force reload
end;

function TZMDelOpr.DeleteByList: Integer;
var
  I: Integer;
  SelectCount: Integer;
  ShowXProgress: Boolean;
  Zip: TZMZipReader;
begin
  ShowProgress := ZspFull;
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsg(Body, ZE_NothingToDo, {_LINE_}244, __UNIT__);
  FZipList.Clear;
  FSkippedFiles.Clear;
  FZipList.ProtectZero := False;
  // add zips to list and select their files
  Result := ProcessDeleteList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsg(Body, Result, {_LINE_}251, __UNIT__);
  if SelectCount < 1 then
  begin
    Body.Inform('nothing selected', {_LINE_}254, __UNIT__);
    Result := ZM_Error({_LINE_}255, ZE_NothingToDo);
    ShowError(Result);
    Exit;
  end;
  Body.ClearIncludeSpecs; // will contain files deleted
  ShowXProgress := FZipList.Count > MergeIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(ZxProcessing, FZipList.Count);
  for I := 0 to FZipList.Count - 1 do
  begin
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Zip := FZipList[I];
    Result := DeleteSelectedFiles(Zip);
  end;
end;

function TZMDelOpr.DeleteSelectedFiles(Zip: TZMZipReader): Integer;
var
  BeforeCnt: Integer;
  DelCnt: Integer;
  Rec: TZMEntryBase;
begin
  Result := PrepareZip(Zip); // prepare the Current zip
  DelCnt := Zip.SelCount;
  if (Result = 0) and (DelCnt <= 0) then
    Result := ZM_Error({_LINE_}282, ZE_NothingToDel);
  if Result = 0 then
  begin
    if (Zip.Count - Zip.SelCount) < 1 then
    begin
      // no files left
      Zip.File_Close;
      File_Delete(Zip.ArchiveName);
      Result := DelCnt; // number of files deleted
    end
    else
    begin
      Rec := Zip.FirstSelected; // from beginning
      while Rec <> nil do
      begin
        IncludeSpecs.Add(Rec.FileName); // we are deleting it
        Rec := Zip.NextSelected(Rec);
      end;
      BeforeCnt := Zip.Count;
      Zip.Select('*', ZzsToggle); // write entries not selected for deletion
      ASSERT((Zip.Count - DelCnt) = Zip.SelCount, 'selcount wrong 2');
      Result := Remake(Zip, Zip.Count - DelCnt, False); // write the result
      if Result >= 0 then
        Result := BeforeCnt - Result; // if no error
    end;
  end;
end;

function TZMDelOpr.FlattenExcludes(Excludes: TStrings): string;
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

function TZMDelOpr.ProcessDelete(SrcZip: TZMZipReader;
  const Arg, Excludes: string): Integer;
begin
  if Verbosity > ZvVerbose then
    Body.TraceFmt('Selecting: %s in: %s', [Arg, SrcZip.ArchiveName],
      {_LINE_}331, __UNIT__);
  Result := SrcZip.SelectFile(Arg, Excludes, ZzsSet);
  if Result < 1 then
  begin
    // none found
    Result := ZM_Error({_LINE_}336, ZE_NotFound);
    if not Skipping(QualifiedName(SrcZip.ArchiveName, Arg), StNotFound, Result)
    then
      Result := 0;
  end;
end;

// returns <0 _ error
(*
 switches
 >>spec  before any source zip is specified sets the default select spec
 >>spec  select files in current zip according to spec
 /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to globals otherwise use spec
 /E  same as /E:|    (use globals)
 changes to excludes occur at current line and continue until changed - does not change already selected files.
 when used on same line as '>>' it only applies to that line and modifies the 'current' excludes.
*)
function TZMDelOpr.ProcessDeleteList(var SelectCount: Integer): Integer;
var
  DefaultArg: string;
  EffectiveExcludes: string;
  GlobalExcludes: string;
  I: Integer;
  LocalArg: string;
  LocalExcludes: string;
  ShowXProgress: Boolean;
  Skip: TZMSkipTypes;
  SourceName: string;
  Xc: string;
  Zip: TZMZipReader;
  ZipsIndex: Integer;
begin
  Result := 0;
  SelectCount := 0;
  GlobalExcludes := FlattenExcludes(ExcludeSpecs);
  EffectiveExcludes := GlobalExcludes;
  FSplitter.Allow := '>E';
  FSplitter.Options := [ZaoWildSpec];
  DefaultArg := '';
  // locate source zips and their files
  SourceName := '';
  ShowXProgress := IncludeSpecs.Count > DeleteIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(ZxProcessing, IncludeSpecs.Count);
  ZipsIndex := ZM_Error({_LINE_}380, ZE_NoInFile); // no current yet
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if Result < 0 then
      Break; // error
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Result := 0;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> ZasNone then
      raise EZipMaster.CreateMsgFmt(Body, ZE_InvalidParameter, [FSplitter.Raw],
        {_LINE_}392, __UNIT__);
    if FSplitter.Main <> '' then
    begin
      Result := DriveFolders.ExpandPath(SourceName, FSplitter.Main);
      if Result < 0 then
        Break;
      ZipsIndex := FZipList.Find(SourceName);
      if ZipsIndex < 0 then
      begin
        // have not opened file yet
        if FileExists(SourceName) then
        begin
          Zip := TZMZipReader.Create(Lister);
          Zip.ArchiveName := SourceName;
          ZipsIndex := Zip.OpenZip(False, False);
          if ZipsIndex >= 0 then
            ZipsIndex := FZipList.Add(Zip) // add to list and return Index
          else
            Zip.Free; // dispose of it (will happen anyway)
        end;
        if ZipsIndex < 0 then
        begin
          // file does not exist or could not be opened
          Body.Inform(Format('Skipped missing or bad zip [%d] %s',
            [-ZipsIndex, SourceName]), {_LINE_}416, __UNIT__);
          case AbsErr(ZipsIndex) of
            ZE_NoInFile:
              Skip := StNotFound;
            ZE_FileOpen:
              Skip := StNoOpen;
          else
            Skip := StReadError;
          end;
          if Skipping(SourceName, Skip, ZipsIndex) then
          begin
            Result := ZipsIndex; // error
            Break;
          end;
          Result := 0; // we can ignore it
          Continue;
        end;
      end;
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
        if FSplitter.Has('E') then
        begin
          Xc := FSplitter.Arg('E');
          if (Xc <> '') and (Xc[1] = SPEC_SEP) then
            EffectiveExcludes := GlobalExcludes + Xc
          else
            EffectiveExcludes := Xc;
          GlobalExcludes := EffectiveExcludes;
        end;
        if FSplitter.Has(ZSPECARG) then
          DefaultArg := FSplitter.Arg(ZSPECARG);
        Continue;
      end;
    end;
    if (Result < 0) or (ZipsIndex < 0) then
      Break; // must have open file
    if FSplitter.Has(ZSPECARG) or (FSplitter.Main <> '') then
    begin
      // include a spec in the current zip
      Zip := FZipList[ZipsIndex];
      LocalExcludes := EffectiveExcludes;
      // check local overrides
      if FSplitter.Has('E') then
      begin
        Xc := FSplitter.Arg('E');
        if (Xc <> '') and (Xc[1] = SPEC_SEP) then
          LocalExcludes := EffectiveExcludes + Xc
        else
          LocalExcludes := Xc;
      end;
      if FSplitter.Has(ZSPECARG) then
      begin
        // include a spec in the current zip
        LocalArg := FSplitter.Arg(ZSPECARG);
        Result := ProcessDelete(Zip, LocalArg, LocalExcludes);
        if Result > 0 then
          SelectCount := SelectCount + Result;
      end;
      Continue;
    end;
    // Set effective
    if FSplitter.Has('E') then
    begin
      Xc := FSplitter.Arg('E');
      if (Xc <> '') and (Xc[1] = SPEC_SEP) then
        EffectiveExcludes := GlobalExcludes + Xc
      else
        EffectiveExcludes := Xc;
    end;
  end;
end;

{ TZMOpDelete }

constructor TZMOpDelete.Create;
begin
  inherited;
end;

function TZMOpDelete.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

function TZMOpDelete.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMDelOpr;
begin
  FOper := TZMDelOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.Delete;
end;

function TZMOpDelete.Name: string;
begin
  Result := 'Delete';
end;

function TZMOpDelete.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs, ZorZip];
end;

end.
