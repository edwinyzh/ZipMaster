Unit Mergeunit;

(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

   This file is part of TZipMaster Version 1.9.2
   
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

{$INCLUDE ZipVers.inc}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, Menus, ShlObj, ComCtrls, ImgList, ZipMstr;

Type
  TMergeForm = Class(TForm)
    AddFileBut: TButton;
    AllFiles1: TMenuItem;
    CancelBut: TButton;
    Clearrest1: TMenuItem;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    FileListBox1: TFileListBox;
    FilterBtn: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    Nofiles1: TMenuItem;
    OKBut: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PopupMenu1: TPopupMenu;
    SelectAllBut: TButton;
    Setrest1: TMenuItem;
    TreeView1: TTreeView;
    Default1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    RemoveAllZips1: TMenuItem;
    RemoveZip1: TMenuItem;
    Procedure AddFileButClick(Sender: TObject);
    procedure AllFiles1Click(Sender: TObject);
    Procedure CancelButClick(Sender: TObject);
    procedure Clearrest1Click(Sender: TObject);
    procedure Default1Click(Sender: TObject);
    procedure FilterBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Nofiles1Click(Sender: TObject);
    Procedure OKButClick(Sender: TObject);
    procedure RemoveAllZips1Click(Sender: TObject);
    procedure RemoveZip1Click(Sender: TObject);
    Procedure SelectAllButClick(Sender: TObject);
    procedure Setrest1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
        Boolean);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure ApplyFilters(const tn: TTreeNode);
    procedure ApplyGlobalExclude(const Exclude: string);
    procedure ApplyLocalFilter(ExcludeNode: TTreeNode; const Filters: string);
//  function ExtractExclude(const Filter: string): string;
//  function ExtractFilters(const Text: string; var Select, Exclude: string):
//      Boolean;
    procedure SplitFilters(const Filters: string; var Select, Exclude: string; var
        HasExclude: boolean);
    function FilterNode(tn: TTreeNode): PInteger;
    procedure FilterZip(ZipNode: TTreeNode);
//  function MakeExcludeCmd(const Exclude: string; Local: boolean): string;
    procedure SetManualSelection(Node: TTreeNode);
    procedure UpdateZipCounts(ZipNode: TTreeNode);
    procedure ShowGlobalFilter(const Filters: string);
    procedure ToggleTreeViewCheckBoxes(Node: TTreeNode);
  protected
    function AddZipToList(FullName: String): TTreeNode;
    function CleanFilters(const Filters: string): string;
    function ZipInList(FullName: String): TTreeNode;
  public
    { Public declarations }
    ZipAction: Integer;
    procedure ApplyFilter(ZipNode: TTreeNode; const Filter: string);
    function CheckEntry(ZipNode: TTreeNode; const EntryName: string; Select:
        boolean): Integer;
    function LoadFromFile(const FileName: String): Integer;
    procedure PrepareScript(List: TStrings);
  End;

Var
  MergeForm: TMergeForm;
  InMouseClick: Boolean;

const
  // ImageList.StateIndex=0 has some bugs, so we add one dummy image to position 0
  State_None: Integer = 0;
  State_GlobalExclude: integer = 1;
  State_OverrideExclude: integer = 2;
  State_LocalExclude: integer = 3;
  State_NotSelected: integer  = 4;
  State_Selected: integer     = 5;
  State_DefaultSelected: integer = 6;

const
  FiltersString = ':Filters=';
//  GlobalExcludeString = ':Global = ';
//  GlobalExcludeString = ':Global excludes = ';
//  LocalExcludeString = ':Local = ';
//  LocalExcludeString = ':Local excludes = ';

Implementation

Uses
  mainunit, FilterUnit, ZMMatch, ZMUtils;
{$R *.DFM}

Procedure TMergeForm.AddFileButClick(Sender: TObject);
Var
  i: Integer;
  FullName: String;
Begin
  mainunit.Canceled := True; // default
  For i := 0 To FileListBox1.Items.Count - 1 Do
  Begin
    If FileListBox1.Selected[i] Then
    Begin
      // Add this file if it isn't already in listbox
      FullName := MainForm.ZipMaster1.AppendSlash(DirectoryListBox1.Directory) +
        FileListBox1.Items[i];
      if ZipInList(FullName) = nil then
        AddZipToList(FullName);
      FileListBox1.Selected[i] := false;
    End;
  End;
End;

(*
  Nodes -
    First node is global excludes
    First child node of zip is
      a. error message
      b. local excludes
*)
function TMergeForm.AddZipToList(FullName: String): TTreeNode;
var
  i: Integer;
  tn: TTreeNode;
  ZipMaster2: TZipMaster;
begin
  // add last
  Result := TreeView1.Items.Item[0];
  // add the zip
  Result := TreeView1.Items.AddObject(Result, FullName, Pointer(0));
  if Result = nil then
    Exit;
//   add files as child nodes
  ZipMaster2 := TZipMaster.Create(nil);
  try
    ZipMaster2.ZipFileName := FullName;
    TreeView1.Items.AddChild(Result, FiltersString);//LocalExcludeString);

    for i := 0 to ZipMaster2.Count - 1 do
    begin
      tn := TreeView1.Items.AddChildObject(Result, ZipMaster2[i].FileName, @State_NotSelected);
      ApplyFilters(tn);
    end;
    UpdateZipCounts(Result);
  finally
    ZipMaster2.Free;
  end;
end;

procedure TMergeForm.AllFiles1Click(Sender: TObject);
var
  tn: TTreeNode;
begin
  tn := TreeView1.Selected;
  if tn <> nil then
  begin
    tn := tn.getFirstChild;
   if tn <> nil then
    tn := tn.getNextSibling;
    while tn <> nil do
    begin
      tn.StateIndex := State_Selected;
      tn := tn.getNextSibling;
    end;
    UpdateZipCounts(TreeView1.Selected);
  end;
end;

procedure TMergeForm.ApplyFilter(ZipNode: TTreeNode; const Filter: string);
begin
  if ZipNode = nil then
    ShowGlobalFilter(Filter)    // set global
  else
    ApplyLocalFilter(ZipNode.getFirstChild, Filter);  // set local filter
end;

procedure TMergeForm.ApplyFilters(const tn: TTreeNode);
var
  State: Integer;
  StateP: PInteger;
begin
  tn.StateIndex := State_DefaultSelected;
  StateP := FilterNode(tn);
  State := StateP^;
  if  (State > 0) and (State < State_DefaultSelected) then
  begin
    tn.StateIndex := State;
    tn.Data := StateP;
  end;
end;

procedure TMergeForm.ApplyGlobalExclude(const Exclude: string);
var
  ZipNode: TTreeNode;
begin
  ShowGlobalFilter(Exclude);
  ZipNode := TreeView1.Items[0];
  if ZipNode = nil then
    Exit;  // should not happen
  ZipNode := ZipNode.getNextSibling;
  while ZipNode <> nil do
  begin
    FilterZip(ZipNode);
    ZipNode := ZipNode.getNextSibling;
  end;
end;

procedure TMergeForm.ApplyLocalFilter(ExcludeNode: TTreeNode; const Filters:
    string);
begin
//  ExcludeNode.Text := MakeExcludeCmd(Select, Filters, True);
  ExcludeNode.Text := FiltersString + Filters;
  FilterZip(ExcludeNode.Parent);
end;

Procedure TMergeForm.CancelButClick(Sender: TObject);
Begin
  mainunit.Canceled := True;
  Close;
End;

function TMergeForm.CheckEntry(ZipNode: TTreeNode; const EntryName: string;
    Select: boolean): Integer;
var
  FoundNode: TTreeNode;
  Node: TTreeNode;
begin
  Result := 0;
  if (ZipNode = nil) or (EntryName = '') then
  begin
    Result := -1;
    Exit;
  end;
  // find the entry
  FoundNode := Nil;
  Node := ZipNode.getFirstChild;
  if Node <> nil then
    Node := Node.getNextSibling;
  while Node <> nil do
  begin
    if FileNameMatch(EntryName, Node.Text) then
    begin
      FoundNode := Node;
      if (Node.StateIndex <> State_Selected) and (Node.StateIndex <> State_NotSelected) then
        SetManualSelection(ZipNode.getFirstChild);
      if Select then
        Node.StateIndex := State_Selected
      else
        Node.StateIndex := State_NotSelected;
      if not IsWild(EntryName) then
        Break;  // can only find one
    end;
    Node := Node.getNextSibling;
  end;
  if FoundNode <> nil then
    UpdateZipCounts(FoundNode);
  // ignore unmatched nodes
end;

function TMergeForm.CleanFilters(const Filters: string): string;
begin
  Result := Trim(Filters);
  // remove FiltersString if it is there
  if CompareText(Copy(Result, 1, Length(FiltersString)), FiltersString) = 0  then
    Result := Copy(Filters, Length(FiltersString) + 1, 2048);
//    Result := Trim(Copy(Filters, Length(FiltersString) + 1, 2048));
end;

procedure TMergeForm.Clearrest1Click(Sender: TObject);
var
  State: Integer;
  tn: TTreeNode;
begin
  tn := TreeView1.Selected;
  SetManualSelection(tn);
  if tn <> nil then
  begin
    repeat
      tn := tn.getNextSibling;
      if tn = nil then
        Break;
      State := PInteger(tn.Data)^;
      if State > State_None then
        tn.StateIndex := State
      else
        tn.StateIndex := State_NotSelected;
    until False;
  end;
  UpdateZipCounts(TreeView1.Selected);
end;

procedure TMergeForm.Default1Click(Sender: TObject);
var
  tn: TTreeNode;
begin
  tn := TreeView1.Selected;
  if tn <> nil then
    FilterZip(tn);
end;

procedure TMergeForm.SplitFilters(const Filters: string; var Select, Exclude:
    string; var HasExclude: boolean);
var
  Posn: Integer;
  S: string;
begin
  HasExclude := False;
  Select := '';
  Exclude := '';
  S := CleanFilters(Filters);
  if S = '' then
    Exit;
  // find excludes if it exists
  Posn := Pos('/e:', S);
  if Posn < 1 then
    Posn := Pos('/E:', S);
  HasExclude := Posn >= 1;
  if HasExclude then
  begin
    Exclude := Trim(Copy(S, Posn + 3, 4096));
    S := Trim(Copy(S, 1, Posn -1));
  end;
  Select := S;
end;

procedure TMergeForm.FilterBtnClick(Sender: TObject);
var
  tn: TTreeNode;
  Filters: string;
begin
  // doing local or global?
  FilterForm.IsGlobal := True;  // assume global
  tn := TreeView1.Selected;
  if tn = nil then
    Exit;
  if tn.AbsoluteIndex > 0 then
  begin
    // local
    FilterForm.IsGlobal := False;
    // find 'first' node
    if tn.Parent <> nil then
      tn := tn.Parent;  // should only be 2 levels
    tn := tn.getFirstChild;
  end;
  // get 'old' filters
  Filters := CleanFilters(tn.Text);
  FilterForm.Filters := Filters;
  if FilterForm.ShowModal = mrOk then
  begin
    Filters := FilterForm.Filters;
    if FilterForm.IsGlobal then
      ApplyGlobalExclude(Filters) // apply global filter
    else
      ApplyLocalFilter(tn, Filters);  // app
  end;
end;

// Global excludes at Items[0], local excludes at first sibling
function TMergeForm.FilterNode(tn: TTreeNode): PInteger;
var
  EntryName: string;
  GlobalExclude: string;
//  GlobalFilter: string;
  GlobalSelect: string;
  HasGlobalExclude: Boolean;
  HasLocalExclude: Boolean;
  LocalExclude: string;
  LocalSelect: string;
  Parent: TTreeNode;
  Select: string;
  WantIt: boolean;
begin
  Result := @State_None;
  SplitFilters(TreeView1.Items[0].Text, GlobalSelect, GlobalExclude, HasGlobalExclude);
  if GlobalSelect = '' then
    GlobalSelect := '*.*';
  if GlobalExclude = '' then
    HasGlobalExclude := False;
  EntryName := tn.Text;
  Parent := tn.Parent;
  SplitFilters(Parent.getFirstChild.Text, LocalSelect, LocalExclude, HasLocalExclude);
  // is the node 'wanted'
  if LocalSelect <> '' then
    Select := LocalSelect
  else
    Select := GlobalSelect;
  WantIt := (CompareStr(Select, '*.*') = 0) or (CompareStr(Select, '*') = 0);
  if not WantIt then
    WantIt := FileNameMatch(Select, EntryName);
  if not WantIt then
  begin
    Result := @State_NotSelected;
    Exit;
  end;
  // we have a wanted entry
  if HasLocalExclude then
  begin
    if LocalExclude = '' then
    begin
      // ignore global
      Result := @State_Selected;
      Exit;
    end;
    if LocalExclude[1] <> '|' then
    begin
      // local overrides global
      if FileNameMatch(LocalExclude, EntryName) then
        Result := @State_OverrideExclude;
      Exit;
    end;
  end;
  // check global first
  if HasGlobalExclude and FileNameMatch(GlobalExclude, EntryName) then
  begin
    Result := @State_GlobalExclude;
    Exit;
  end;
  // check local
  if HasLocalExclude and FileNameMatch(LocalExclude, EntryName) then
  begin
    Result := @State_LocalExclude;
//    Exit;
  end;
end;

procedure TMergeForm.FilterZip(ZipNode: TTreeNode);
var
  Node: TTreeNode;
begin
  if ZipNode.Parent <> nil then
    ZipNode := ZipNode.Parent; // make sure we are on zip
  Node := ZipNode.getFirstChild;
  if Node <> nil then
  begin
    Node := Node.getNextSibling;
    while Node <> nil do
    begin
      ApplyFilters(Node);
      Node := Node.getNextSibling;
    end;
  end;
  UpdateZipCounts(ZipNode);
end;

Procedure TMergeForm.FormCreate(Sender: TObject);
Var
  SpecFolder: String;
Begin
  SpecFolder := '';
  MainForm.GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, SpecFolder);
  DriveComboBox1.Drive := ExtractFileDrive(SpecFolder)[1];
  DirectoryListBox1.Directory := ExtractFilePath(SpecFolder);
  InMouseClick := false;
{$IFNDEF VERpre6}
  TreeView1.OnContextPopUp := TreeView1ContextPopup;
{$ENDIF}
End;

procedure TMergeForm.FormShow(Sender: TObject);
var
  Node: TTreeNode;
begin
  if TreeView1.Items.Count < 1 then
    ShowGlobalFilter('');
  Node := TreeView1.Selected;
//  RemoveBut.Enabled := (Node <> nil) and (Node.Parent = nil)
//    and (Node.AbsoluteIndex > 0);
  FilterBtn.Enabled := Node <> nil;
end;

(*
file format-
  # means rem
  |<space>Global excludes - only before first ZipName
  ZipName always starts line
  |<space>Local excludes - must be first non-rem line after ZipName
  +<space>Entry name  - forces manual inclusion
  -<space>Entry name   - forces manual exclusion

All other entries in a zip are selected/deselected according to the filters
  Restrictions -
     no Zip/Entry name can have '#' or start with '+ ' or '- '
*)
function TMergeForm.LoadFromFile(const FileName: String): Integer;
var
  Cmnd: string;
  CmndList: TStringList;
  CurrentZipNode: TTreeNode;
  LNo: Integer;
  Op: Char;
  Posn: Integer;
begin
  Result := 0;
  ShowGlobalFilter(''); // make sure exists
  CurrentZipNode := nil;
  CmndList := TStringList.Create;
  try
    CmndList.LoadFromFile(FileName);
    LNo := 0;
    while LNo < CmndList.Count do
    begin
      if Result < 0 then
        Break; // fatal
      Cmnd := Trim(CmndList[LNo]);
      Inc(LNo);
      Posn := Pos('#', Cmnd);
      if Posn > 0 then
        Cmnd := Copy(Cmnd, 1, Posn - 1);
      Cmnd := Trim(Cmnd);
      if Cmnd = '' then
        Continue;
      Op := Cmnd[1];
      case Op of
        '|':
          begin
            // we have a filter
            Cmnd := Trim(Copy(Cmnd, 2, 1024));
            ApplyFilter(CurrentZipNode, Cmnd);
          end;
        '+', '-':
          begin
            // we have a manual selection
            if CurrentZipNode = nil then
              Result := -1 // error
            else
            begin
              Cmnd := Trim(Copy(Cmnd, 2, 1024));
              Result := CheckEntry(CurrentZipNode, Cmnd, Op = '+');
            end;
          end;
      else
        begin
          // we have a zip
          CurrentZipNode := AddZipToList(Cmnd);
          if CurrentZipNode = nil then
            Result := -1; // error
        end;
      end;
    end;
  finally
    CmndList.Free;
  end;
end;

//(*
//Global - Select empty give '*.*'
//Global - Exclude empty give nothing
//Local  - Select empty give nothing
//Local  - Exclude empty give ?
//*)
//function TMergeForm.MakeExcludeCmd(const Exclude: string; Local: boolean):
//  string;
//begin
////  if Local then
////    Result := LocalExcludeString
////  else
//  Result := FiltersString;//GlobalExcludeString;
//if Exclude <> '' then   // TODO -c : wrong - does not allow empty local
//  Result := Result + '/e:' + Exclude;
//end;

procedure TMergeForm.Nofiles1Click(Sender: TObject);
var
  tn: TTreeNode;
begin
  tn := TreeView1.Selected;
  if tn <> nil then
  begin
    tn := tn.getFirstChild;
    if tn <> nil then
      tn := tn.getNextSibling;
    while tn <> nil do
    begin
      tn.StateIndex := State_NotSelected;
      tn := tn.getNextSibling;
    end;
    UpdateZipCounts(TreeView1.Selected);
  end;
end;

Procedure TMergeForm.OKButClick(Sender: TObject);
Begin
  mainunit.Canceled := false;
  Close;
End;

procedure TMergeForm.PrepareScript(List: TStrings);
var
  cn: TTreeNode;
//  FilterExclude: string;
//  FilterSelect: string;
//  HasExclude: Boolean;
  Posn: Integer;
  tn: TTreeNode;
  txt: string;
  ZipName: string;
begin
  List.Clear;
  if TreeView1.Items.Count < 1 then
    Exit;
  tn := TreeView1.Items.Item[0];   // global filters
  if tn <> nil then
  begin
    txt := CleanFilters(tn.Text);
    if txt <> '' then
    begin
      if txt[1] = '/' then
        List.Add(txt)
      else
        List.Add('>> ' + txt);
    end;
    tn := tn.getNextSibling;
  end;
  while tn <> nil do
  begin
    ZipName := tn.Text;
    Posn := Pos(' <<', ZipName);
    if Posn > 0 then
      ZipName := Trim(Copy(ZipName, 1, Posn));
    cn := tn.getFirstChild;
    // at filters
    if cn <> nil then
    begin
      txt := CleanFilters(cn.Text);
      cn := cn.getNextSibling;
    end;
    while cn <> nil do
    begin
      if cn.StateIndex = State_Selected then
      begin
        if ZipName <> '' then
        begin
          List.Add(ZipName + ' >> ' + cn.Text);
          ZipName := '';  // used it
        end
        else
          List.Add(' >> ' + cn.Text);
      end;
      // next entry
      cn := cn.getNextSibling;
    end;
    if ZipName <> '' then
    begin
      // entries were selected by filters
      if txt <> '' then
      begin
      if txt[1] = '/' then
        List.Add(ZipName + txt)
      else
        List.Add(ZipName + ' >> ' + txt);
      end
      else
        List.Add(ZipName);  // no filters (use globals)
    end;
    // next zip
    tn := tn.getNextSibling;
  end;
end;

procedure TMergeForm.RemoveAllZips1Click(Sender: TObject);
begin
  TreeView1.Items.Clear;
  ShowGlobalFilter('');
end;

procedure TMergeForm.RemoveZip1Click(Sender: TObject);
var
  tn: TTreeNode;
begin
  if TreeView1.Items.Count < 1 then
    Exit;
  tn := TreeView1.Selected;
  // only delete zip
  if (tn <> nil) and (tn.Data = nil) then
  begin
    tn.DeleteChildren;
    tn.Delete;
  end;
end;

Procedure TMergeForm.SelectAllButClick(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To FileListBox1.Items.Count - 1 Do
    FileListBox1.Selected[i] := True;
End;

// change from auto to manual selecting
procedure TMergeForm.SetManualSelection(Node: TTreeNode);
var
  Sibling: TTreeNode;
  State: Integer;
begin
  Sibling := Node.Parent.getFirstChild; // local exclude
  if Sibling <> nil then
    Sibling := Sibling.getNextSibling;    // first entry
  while Sibling <> nil do
  begin
    State := Sibling.StateIndex;
    if State = State_DefaultSelected then
      Sibling.StateIndex := State_Selected
    else
    if (State = State_NotSelected) or (State = State_Selected) then
      Break;
    Sibling := Sibling.getNextSibling;
  end;
end;

procedure TMergeForm.Setrest1Click(Sender: TObject);
var
  tn: TTreeNode;
begin
  tn := TreeView1.Selected;
  SetManualSelection(tn);
  if tn <> nil then
  begin
    repeat
      tn := tn.getNextSibling;
      if tn = nil then
        Break;
      tn.StateIndex := State_Selected;
    until False;
  end;
  UpdateZipCounts(TreeView1.Selected);
end;

procedure TMergeForm.UpdateZipCounts(ZipNode: TTreeNode);
var
  Entries: Integer;
  NewCnts: string;
  Node: TTreeNode;
  Posn: Integer;
  Selected: Integer;
  ZCnts: string;
  ZName: string;
begin
  if (ZipNode = nil) or (ZipNode.AbsoluteIndex = 0) then
    Exit;
  if ZipNode.Parent <> nil then
    ZipNode := ZipNode.Parent; // was on child
  // Get any old counts and name
  ZCnts := '';
  ZName := ZipNode.Text;
  Posn := Pos(' <<', ZName);
  if Posn > 0 then
  begin
    ZCnts := Copy(ZName, Posn, 255);
    ZName := Trim(Copy(ZName, 1, Posn));
  end;
  // count entry nodes
  Entries := 0;
  Selected := 0;
  Node := ZipNode.getFirstChild;
  if Node <> nil then
    Node := Node.getNextSibling;
  while Node <> nil do
  begin
    if Node.StateIndex >= State_Selected then
      Inc(Selected);
    Inc(Entries);
    Node := Node.getNextSibling;
  end;
  // form the 'counts'
  NewCnts := ' << ';
  if Entries = 0 then
    NewCnts := NewCnts + 'ERROR >>'
  else
    NewCnts := NewCnts + IntToStr(Selected) + ' of ' + IntToStr(Entries) + ' >>';
  // only update if changed
  if CompareText(NewCnts, ZCnts) <> 0 then
    ZipNode.Text := ZName + NewCnts;
end;

procedure TMergeForm.ShowGlobalFilter(const Filters: string);
var
  tmp: string;
begin
//  tmp := MakeExcludeCmd(Filters, False);
  tmp := FiltersString + Filters;
  if TreeView1.Items.Count < 1 then
    TreeView1.Items.AddFirst(nil, tmp)
  else
    TreeView1.Items[0].Text := tmp;
end;

(*
This would mean 'checkmark' handling would be :-
  (A) StateIndex = (1..3), {ignore} 5
  (B) StateIndex = 4, change to 5
  (C) StateIndex = 5, change to {4}  Filter 1..4
  (D) StateIndex = 6, convert all other non-excluded to 5, this one to 4
*)
procedure TMergeForm.ToggleTreeViewCheckBoxes(Node: TTreeNode);
var
  State: Integer;
begin
  if Assigned(Node) and (Node.Data <> nil) then
  begin
    SetManualSelection(Node);

    if Node.StateIndex <= State_NotSelected then
      Node.StateIndex := State_Selected
    else
    if Node.StateIndex >= State_Selected then
    begin
      State := PInteger(Node.Data)^;
      if State > State_None then
        Node.StateIndex := State
      else
        Node.StateIndex := State_NotSelected;
    end;
  end; // if Assigned(Node)
  UpdateZipCounts(Node);
end; // ToggleTreeViewCheckBoxes

procedure TMergeForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
//  RemoveBut.Enabled := (Node <> nil) and (Node.Parent = nil)
//    and (Node.AbsoluteIndex > 0);
  FilterBtn.Enabled := TreeView1.Selected <> nil;
end;

procedure TMergeForm.TreeView1Click(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := TreeView1.ScreenToClient(P);
  if (htOnStateIcon in TreeView1.GetHitTestInfoAt(P.X, P.Y)) then
    ToggleTreeViewCheckBoxes(TreeView1.Selected);
end;

procedure TMergeForm.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  IsZip: Boolean;
  AllowRest: Boolean;
  treeNode: TTreeNode;
  TreeView: TTreeView;
begin
  TreeView := TTreeView(Sender);
  treeNode := TreeView.GetNodeAt(MousePos.X, MousePos.Y);

  if Assigned(treeNode) and (treeNode.Index > 0) then
  begin
    TreeView.Selected := treeNode;
    // modify memu
    AllowRest := false;
    IsZip := False;
    if treeNode.Data <> nil then
      AllowRest := treeNode.getNextSibling <> nil    // on entry
    else
      IsZip := True;
    Clearrest1.Enabled := AllowRest;
    Setrest1.Enabled := AllowRest;
    AllFiles1.Enabled := IsZip;
    Nofiles1.Enabled := IsZip;
    RemoveAllZips1.Enabled := IsZip;
    RemoveZip1.Enabled := IsZip;
    // popup will display automatically
  end
  else
    Handled := True; // no node under mouse - do not display popup
end;

procedure TMergeForm.TreeView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(TreeView1.Selected) then
    ToggleTreeViewCheckBoxes(TreeView1.Selected);
end; // TreeView1KeyDown

// if has children toggle expanded
function TMergeForm.ZipInList(FullName: String): TTreeNode;
begin
  Result := nil;
  if TreeView1.Items.Count < 1 then
    Exit;
  Result := TreeView1.Items.Item[0];
  while (Result <> nil) and (CompareText(Result.Text, FullName) <> 0) do
    Result := Result.getNextSibling;
end;

End.
