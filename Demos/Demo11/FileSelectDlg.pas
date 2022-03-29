unit FileSelectDlg;
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

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FileCtrl;

type
  TDialogOptions = (tdSelFiles, tdIncludeRec, tdUseFilters);
  TDialogOpts    = set of TDialogOptions;

type
  TTFileSelDialog = class(TForm)
    BrowseBtn: TButton;
    CancelBtn: TBitBtn;
    CheckBox: TCheckBox;
    DLabel2: TLabel;
    DLabel5: TLabel;
    DLabel6: TLabel;
    Edit: TEdit;
    FilterComboBox: TFilterComboBox;
    OkBtn: TBitBtn;
    procedure BrowseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
  private
    { Private declarations }
    DirPath: string;
    FFileSelected: boolean;
    FTDOptions: TDialogOpts;
    procedure SetTDOptions(const Value: TDialogOpts);
  public
    { Public declarations }
    property FileSelected: boolean read FFileSelected write FFileSelected;
    property TDOptions: TDialogOpts read FTDOptions write SetTDOptions;
  end;

var
  TFileSelDialog: TTFileSelDialog;

implementation

uses
  ShlObj;

{$R *.dfm}

procedure TTFileSelDialog.BrowseBtnClick(Sender: TObject);
const
  BIF_NONEWFOLDERBUTTON = $200;
var
  BrowseInfo: TBROWSEINFO;
  DialogTitle: string;
  DisplayBuf: array [0 .. MAX_PATH] of Char;
  Dir: array [0 .. MAX_PATH] of Char;
  ItemID: PItemIDList;
begin
  if tdSelFiles in FTDOptions then
    DialogTitle := 'Select Files'
  else
    DialogTitle := 'Select Folder';
  ZeroMemory(@BrowseInfo, SizeOf(BrowseInfo)); 
  BrowseInfo.hwndOwner := Handle;
  BrowseInfo.pszDisplayName := DisplayBuf;
  BrowseInfo.lpszTitle := PChar(DialogTitle); 
  if (tdSelFiles in FTDOptions) then
    BrowseInfo.ulFlags := BIF_USENEWUI or BIF_NONEWFOLDERBUTTON or
      BIF_BROWSEINCLUDEFILES
  else
    BrowseInfo.ulFlags := BIF_USENEWUI or BIF_NONEWFOLDERBUTTON or
      BIF_RETURNONLYFSDIRS;
  ItemID := SHBrowseForFolder(BrowseInfo);
  if ((ItemID ) <> nil) then
  begin
    Dir[0] := #0;
    SHGetPathFromIDList(ItemID, Dir);
    FileSelected := (GetFileAttributes(Dir) and FILE_ATTRIBUTE_DIRECTORY) = 0;
    DirPath := Dir;
    OkBtn.Enabled := true;
    SelectionChange(nil);
  end;
end;

procedure TTFileSelDialog.FormShow(Sender: TObject);
begin
  OkBtn.Enabled := false;
  FileSelected := false;
  DirPath := '';
  SelectionChange(nil);
end;

procedure TTFileSelDialog.SelectionChange(Sender: TObject);
var
  RecurseStr: string;
begin
  if (tdUseFilters in FTDOptions) and not FileSelected then
  begin
    Edit.Text := DirPath + '\' + FilterComboBox.Mask;
    FilterComboBox.Enabled := true;
  end
  else
  begin
    Edit.Text := DirPath;
    FilterComboBox.Enabled := false;
  end;
  if (tdIncludeRec in FTDOptions) then
  begin
    if CheckBox.Checked then
      RecurseStr := '>'
    else
      RecurseStr := '|';

    Edit.Text := RecurseStr + Edit.Text;
  end;
  if (CheckBox.Checked) then
    DLabel6.Show
  else
    DLabel6.Hide;
end;

procedure TTFileSelDialog.SetTDOptions(const Value: TDialogOpts);
begin
  FTDOptions := Value;
  if tdSelFiles in Value then
    BrowseBtn.Caption := 'Select Files'
  else
    BrowseBtn.Caption := 'Select Folder';
end;

end.
