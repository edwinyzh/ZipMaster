unit FilterUnit;


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
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Tabs, AppEvnts;

type
  TFilterForm = class(TForm)
    FiltersBox: TCheckListBox;
    edCustom: TEdit;
    cbIncGlobal: TCheckBox;
    edFilter: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    lblCustom: TLabel;
    TabSet1: TTabSet;
    cbIgnoreGlobal: TCheckBox;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edCustomChange(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure FiltersBoxClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbIncGlobalClick(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    FExcludeFilter: string;
    FExcludes: TStrings;
    FIsGlobal: Boolean;
    FSelectFilter: string;
    FSelects: TStrings;
    procedure ComposeFilter;
    procedure Decompose(const Value: string; IsSelect: Boolean);
    function ExtractSpec(const AString: String): string;    function GetCurrentFilter: string;
    function GetFilters: string;
    function GetIsGlobal: Boolean;
    function GetOverrideGlobal: Boolean;    function GetSelectIsActive: Boolean;
    function HasUseGlobal(const Exclude: string): Boolean;
    function HaveCustom: Boolean;
    function RemovePrefix(const Filter: string): string;
    procedure SetExcludes(const Value: TStrings);
    function SetUseGlobal(const Exclude: string; Use: Boolean): string;
    procedure SetCurrentFilter(const Value: string);
    procedure SetFilters(const Value: string);
    procedure SetIsGlobal(const Value: Boolean);
    procedure SetSelectIsActive(const Value: Boolean);
    procedure SetSelects(const Value: TStrings);
    procedure SplitFilters(const Filters: string; var Select, Exclude: string;
      var HasExclude: Boolean);
    procedure UpdateForm;
  protected
    procedure SplitList(List: TStrings; const Value: string);
    property CurrentFilter: string read GetCurrentFilter write SetCurrentFilter;
    property SelectIsActive: Boolean read GetSelectIsActive
      write SetSelectIsActive;
  public
    property Excludes: TStrings read FExcludes write SetExcludes;
    property Filters: string read GetFilters write SetFilters;
    property IsGlobal: Boolean read GetIsGlobal write SetIsGlobal;
    property OverrideGlobal: Boolean read GetOverrideGlobal;
    property Selects: TStrings read FSelects write SetSelects;
  end;

var
  FilterForm: TFilterForm;

implementation

uses
  ZMUtils;

{$R *.DFM}

const
  ExcludePrefix = '/e:';
  CustomStr     = 'Custom';
  DEFAULT_SELECTS = 'Any (*.*)|Custom|Delphi (*.pas)|C++ (*.cpp)|Headers (*.h)';
  DEFAULT_EXCLUDES =
    'Text files (*.txt)|Backup files (*.bak)|Zip files (*.zip)|' +
    'Custom|HTM files (*.htm)|HTML files (*.html)|Bak (*.bak)';

procedure TFilterForm.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  Txt: string;
begin
  cbIgnoreGlobal.Visible := (not(SelectIsActive or IsGlobal)) and
    (RemovePrefix(edFilter.Text) = '');
  if cbIgnoreGlobal.Visible then
  begin
    if cbIgnoreGlobal.Checked then
      Txt := ExcludePrefix
    else
      Txt := '';
    if CompareStr(Txt, edFilter.Text) <> 0 then
      edFilter.Text := Txt;
  end;
  edCustom.Visible := HaveCustom;
  lblCustom.Visible := HaveCustom;
  cbIncGlobal.Visible := not(SelectIsActive or IsGlobal);
end;

procedure TFilterForm.FormDestroy(Sender: TObject);
begin
  FSelects.Free;
  FExcludes.Free;
end;

procedure TFilterForm.FormCreate(Sender: TObject);
begin
  FSelects := TStringList.Create;
  SplitList(FSelects, DEFAULT_SELECTS);
  FiltersBox.Items.Clear;
  FiltersBox.Items.AddStrings(FSelects);
  FExcludes := TStringList.Create;
  SplitList(FExcludes, DEFAULT_EXCLUDES);
end;

procedure TFilterForm.ComposeFilter;
var
  I: Integer;
  S: string;
  Txt: string;
begin
  S := '';
  for I := 0 to FiltersBox.Items.Count - 1 do
  begin
    if not FiltersBox.Checked[I] then
      Continue;
    if CompareText(FiltersBox.Items[I], CustomStr) = 0 then
    begin
      if edCustom.Text <> '' then
      begin
        if S <> '' then
          S := S + '|';
        S := S + edCustom.Text;
      end;
      Continue;
    end;
    Txt := FiltersBox.Items[I];
    Txt := ExtractSpec(Txt);
    if Txt <> '' then
    begin
      if S <> '' then
        S := S + '|';
      S := S + Txt;
    end;
  end;
  if not SelectIsActive then
  begin
    if cbIncGlobal.Checked and (S <> '') then
      S := '|' + S;
//  end
//  else
//  begin
    if S = '' then
    begin
      if cbIgnoreGlobal.Checked then
        S := ExcludePrefix;
    end
    else
      S := ExcludePrefix + S;
  end;
  if CompareStr(S, edFilter.Text) <> 0 then
    edFilter.Text := S;
end;

procedure TFilterForm.Decompose(const Value: string; IsSelect: Boolean);
var
  Custom: string;
  I: Integer;
  Posn: Integer;
  Spec: string;
  Strng: string;
  Tmp: string;
begin
  edCustom.Text := '';
  Strng := RemovePrefix(Value);
  cbIncGlobal.Checked := (not IsSelect) and HasUseGlobal(Value);
  cbIncGlobal.Visible := not IsSelect;
  // clear selections
  for I := 0 to FiltersBox.Items.Count - 1 do
    FiltersBox.Checked[I] := False;
  Strng := SetUseGlobal(Strng, False);
  Custom := '';
  repeat
    Posn := Pos('|', Strng);
    if Posn > 0 then
    begin
      Spec := Trim(Copy(Strng, 1, Posn - 1)); // spec
      Strng := Trim(Copy(Strng, Posn + 1, 256)); // rest of string
    end
    else
    begin
      Spec := Strng;
      Strng := '';
    end;
    if Spec <> '' then
    begin
      // search for corresponding entry
      for I := 0 to FiltersBox.Items.Count - 1 do
      begin
        Tmp := ExtractSpec(FiltersBox.Items[I]);
        if CompareText(Spec, Tmp) = 0 then
        begin
          FiltersBox.Checked[I] := True;
          Spec := ''; // we used it
          Break;
        end;
      end;
      if Spec <> '' then
      begin
        if Custom <> '' then
          Custom := Custom + '|';
        Custom := Custom + Spec;
        // set custom
        for I := 0 to FiltersBox.Items.Count - 1 do
        begin
          Tmp := ExtractSpec(FiltersBox.Items[I]);
          if CompareText(FiltersBox.Items[I], CustomStr) = 0 then
          begin
            FiltersBox.Checked[I] := True;
            Break;
          end;
        end;
      end;
    end;
  until Strng = '';
  if Custom <> '' then
    edCustom.Text := Custom;
end;

procedure TFilterForm.edCustomChange(Sender: TObject);
begin
  ComposeFilter;
end;

procedure TFilterForm.edFilterChange(Sender: TObject);
var
  Has: Boolean;
begin
  Has := HasUseGlobal(edFilter.Text);
  if Has <> cbIncGlobal.Checked then
    cbIncGlobal.Checked := Has;
end;

function TFilterForm.ExtractSpec(const AString: String): string;
var
  Posn: Integer;
  Txt: string;
begin
  Result := '';
  Posn := Pos('(', AString);
  if Posn > 0 then
  begin
    Txt := Copy(AString, Posn + 1, 4096);
    Posn := Pos(')', Txt);
    if Posn > 0 then
      Result := Copy(Txt, 1, Posn - 1);
  end;
end;

procedure TFilterForm.FiltersBoxClickCheck(Sender: TObject);
begin
  edCustom.Visible := HaveCustom;
  lblCustom.Visible := HaveCustom;
  ComposeFilter;
end;

procedure TFilterForm.FormShow(Sender: TObject);
begin
  UpdateForm;
end;

function TFilterForm.GetCurrentFilter: string;
begin
  Result := edFilter.Text;
end;

function TFilterForm.GetFilters: string;
begin
  if SelectIsActive then
    Result := GetCurrentFilter + ' ' + FExcludeFilter
  else
    Result := FSelectFilter + ' ' + GetCurrentFilter;
  Result := Trim(Result);
end;

function TFilterForm.GetIsGlobal: Boolean;
begin
  Result := FIsGlobal;
end;

function TFilterForm.GetOverrideGlobal: Boolean;
begin
  Result := (not IsGlobal) and cbIncGlobal.Checked;
end;

function TFilterForm.GetSelectIsActive: Boolean;
begin
  Result := TabSet1.TabIndex = 0;
end;

function TFilterForm.HasUseGlobal(const Exclude: string): Boolean;
var
  Strng: string;
begin
  Strng := RemovePrefix(Exclude);
  Result := (Strng <> '') and (Strng[1] = '|');
end;

function TFilterForm.HaveCustom: Boolean;
var
  CustomIndex: Integer;
begin
  // check 'Manual' set
  CustomIndex := FiltersBox.Items.Count;
  while CustomIndex > 0 do
  begin
    Dec(CustomIndex);
    if CompareText(FiltersBox.Items[CustomIndex], CustomStr) = 0 then
      Break;
  end;
  Result := (CustomIndex >= 0) and (FiltersBox.Checked[CustomIndex]);
end;

procedure TFilterForm.cbIncGlobalClick(Sender: TObject);
var
  Has: Boolean;
begin
  if not(IsGlobal or SelectIsActive) then
  begin
    Has := HasUseGlobal(edFilter.Text);
    if Has <> cbIncGlobal.Checked then
      edFilter.Text := SetUseGlobal(edFilter.Text, cbIncGlobal.Checked);
  end;
end;

function TFilterForm.RemovePrefix(const Filter: string): string;
begin
  Result := Trim(Filter);
  // remove FiltersString if it is there
  if CompareText(Copy(Result, 1, Length(ExcludePrefix)), ExcludePrefix) = 0 then
    Result := Trim(Copy(Filter, Length(ExcludePrefix) + 1, 2048));
end;

procedure TFilterForm.SetExcludes(const Value: TStrings);
begin
  FExcludes.Clear;
  FExcludes.AddStrings(Value);
end;

function TFilterForm.SetUseGlobal(const Exclude: string; Use: Boolean): string;
var
  Has: Boolean;
  S: string;
begin
  Result := Trim(Exclude);
  if Result <> '' then
  begin
    Has := HasUseGlobal(Result);
    if Has <> Use then
    begin
      S := RemovePrefix(Exclude);
      if Use then
        Result := '|' + S // Result
      else
        Result := Trim(Copy(S, 2, 4096));
      Result := ExcludePrefix + Result;
    end;
  end;
end;

procedure TFilterForm.SetCurrentFilter(const Value: string);
begin
  if CurrentFilter <> Value then
  begin
    // 'decompose' CurrentFilter into FilterBox checks
    Decompose(Value, SelectIsActive);
    edFilter.Text := Trim(Value);
  end;
end;

procedure TFilterForm.SetFilters(const Value: string);
var
  Exclude: string;
  HasExclude: Boolean;
  Select: string;
begin
  if Value <> Filters then
  begin
    SplitFilters(Value, Select, Exclude, HasExclude);
    if SelectIsActive then
    begin
      SetCurrentFilter(Select);
      FExcludeFilter := Exclude;
    end
    else
    begin
      SetCurrentFilter(Exclude);
      FExcludeFilter := Select;
    end;
  end;
end;

procedure TFilterForm.SetIsGlobal(const Value: Boolean);
begin
  if IsGlobal <> Value then
  begin
    FIsGlobal := Value;
    cbIncGlobal.Visible := not Value;
  end;
end;

procedure TFilterForm.SetSelectIsActive(const Value: Boolean);
begin
  if Value <> SelectIsActive then
  begin
    if Value then
      TabSet1.TabIndex := 0
    else
      TabSet1.TabIndex := 1;
  end;
end;

procedure TFilterForm.SetSelects(const Value: TStrings);
begin
  FSelects.Clear;
  FSelects.AddStrings(Value);
end;

procedure TFilterForm.SplitFilters(const Filters: string;
  var Select, Exclude: string; var HasExclude: Boolean);
var
  Posn: Integer;
  S: string;
begin
  HasExclude := False;
  Select := '';
  Exclude := '';
  S := Trim(Filters);
  if S = '' then
    Exit;
  // find excludes if it exists
  Posn := Pos(' /e:', S);
  if Posn < 1 then
    Posn := Pos(' /E:', S);
  HasExclude := Posn >= 1;
  if HasExclude then
  begin
    Exclude := Trim(Copy(S, Posn, 4096));
    S := Trim(Copy(S, 1, Posn - 1));
  end;
  Select := S;
end;

procedure TFilterForm.SplitList(List: TStrings; const Value: string);
var
  Rest: string;
  S: string;
begin
  S := Value;
  while S <> '' do
  begin
    S := ZSplitString('|', S, Rest);
    if S <> '' then
      List.Add(S);
    S := Rest;
  end;
end;

procedure TFilterForm.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if NewTab = 1 then
  begin
    // switch to Excludes
    FSelectFilter := edFilter.Text;
    FiltersBox.Items.Clear;
    FiltersBox.Items.AddStrings(FExcludes);
    edFilter.Text := FExcludeFilter;
    Decompose(FExcludeFilter, False);
  end
  else
  begin
    // switch to Selects
    FExcludeFilter := edFilter.Text;
    FiltersBox.Items.Clear;
    FiltersBox.Items.AddStrings(FSelects);
    edFilter.Text := FSelectFilter;
    Decompose(FSelectFilter, True);
  end
end;

procedure TFilterForm.UpdateForm;
begin
  ComposeFilter;
end;

end.
