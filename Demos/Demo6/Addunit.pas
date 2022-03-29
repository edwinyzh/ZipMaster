unit AddUnit;
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
   
{$INCLUDE '..\..\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, Menus, ShlObj;

type
  TAddFile = class( TForm )
    Panel1:            TPanel;
    Panel2:            TPanel;
    Panel3:            TPanel;
    Panel4:            TPanel;
    Panel5:            TPanel;
    Panel6:            TPanel;
    Panel7:            TPanel;
    Panel8:            TPanel;
    Panel9:            TPanel;
    Bevel1:            TBevel;
    Label1:            TLabel;
    Label2:            TLabel;
    Label3:            TLabel;
    AddFileBut:        TButton;
    RemoveBut:         TButton;
    OKBut:             TButton;
    CancelBut:         TButton;
    SelectAllBut:      TButton;
    SortBut:           TButton;
    AddDirBut:         TButton;
    AddWildBttn:       TButton;
    AddWildPathBttn:   TButton;
    DirNameCB:         TCheckBox;
    RecurseCB:         TCheckBox;
    EncryptCB:         TCheckBox;
    SelectedList:      TListBox;
    WildEdit:          TEdit;
    DriveComboBox1:    TDriveComboBox;
    FileListBox1:      TFileListBox;
    DirectoryListBox1: TDirectoryListBox;

    procedure OKButClick( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure AddFileButClick( Sender: TObject );
    procedure SortButClick( Sender: TObject );
    procedure RemoveButClick( Sender: TObject );
    procedure SelectAllButClick( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure AddDirButClick( Sender: TObject );
    procedure AddWildBttnClick( Sender: TObject );
    procedure AddWildPathBttnClick( Sender: TObject );

  public
    { Public declarations }
  end;

var
  AddFile: TAddFile;
  InMouseClick: Boolean;

implementation

uses main;

{$R *.DFM}

procedure TAddFile.OKButClick( Sender: TObject );
begin
   Main.Canceled := False;
   Close;
end;

procedure TAddFile.CancelButClick( Sender: TObject );
begin
  Main.Canceled := True;
  Close;
end;

procedure TAddFile.SortButClick( Sender: TObject );
begin
  SelectedList.Sorted := True;
  SortBut.Enabled := False;  { list will remain sorted }
end;

procedure TAddFile.RemoveButClick( Sender: TObject );
var
   i: Integer;
begin
   for i := SelectedList.Items.Count - 1 downto 0 do
   begin
      if SelectedList.Selected[i] then
         SelectedList.Items.Delete(i);
   end;
end;

procedure TAddFile.SelectAllButClick( Sender: TObject );
var
   i: Integer;
begin
   for i := 0 to FileListBox1.Items.Count - 1 do
      FileListBox1.Selected[i] := True;
end;

procedure TAddFile.FormCreate( Sender: TObject );
var
   SpecFolder: String;
begin
   SpecFolder := '';

   MainForm.GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, SpecFolder );
   DriveComboBox1.Drive := ExtractFileDrive( SpecFolder )[1];
   DirectoryListBox1.Directory := ExtractFilePath( SpecFolder );
   InMouseClick := False;
end;

procedure TAddFile.AddDirButClick( Sender: TObject );
var
   i:        Integer;
   FullName: String;
begin
   Main.Canceled := True;  // default
   for i := 0 to DirectoryListBox1.Items.Count - 1 do
   begin
      if DirectoryListBox1.Selected[i] then
      begin
         // Add this file if it isn't already in listbox
         FullName := Mainform.ZipMaster1.AppendSlash( DirectoryListBox1.Directory ) + '*.*';

         if SelectedList.Items.IndexOf( FullName ) < 0 then
            SelectedList.Items.Add( FullName );
      { Never de-select dirnames from the DirectoryList! }
      {  DirectoryListBox1.Selected[i]:=False; }
      end;
   end;
   { Position the "SelectedList" listbox at the bottom }
   with SelectedList do
   begin
      Selected[Items.Count - 1] := True;
      Selected[Items.Count - 1] := False;
   end;
end;

procedure TAddFile.AddFileButClick( Sender: TObject );
var
   i:        Integer;
   FullName: String;
begin
   Main.Canceled := True;  // default
   for i := 0 to FileListBox1.Items.Count - 1 do
   begin
      if FileListBox1.Selected[i] then
      begin
         // Add this file if it isn't already in listbox
         FullName := Mainform.ZipMaster1.AppendSlash( DirectoryListBox1.Directory ) + FileListBox1.Items[i];
         if SelectedList.Items.IndexOf( FullName ) < 0 then
            SelectedList.Items.Add( FullName );
         FileListBox1.Selected[i] := False;
      end;
   end;
   { Position the "SelectedList" listbox at the bottom }
   with SelectedList do
   begin
      Selected[Items.Count - 1] := True;
      Selected[Items.Count - 1] := False;
   end;
end;

procedure TAddFile.AddWildBttnClick( Sender: TObject );
begin
   with SelectedList, SelectedList.Items do
   begin
      if (Length( WildEdit.Text ) > 0) and (IndexOf( WildEdit.Text ) < 0) then
      begin
         Add( WildEdit.Text );
         // Position the "SelectedList" listbox at the bottom.
         Selected[Count - 1] := True;
         Selected[Count - 1] := False;
      end;
   end;
end;

procedure TAddFile.AddWildPathBttnClick( Sender: TObject );
var
   FullName: String;
begin
   if Length( WildEdit.Text ) > 0 then
   begin
      FullName := MainForm.ZipMaster1.AppendSlash( DirectoryListBox1.Directory ) + WildEdit.Text;

      with SelectedList, SelectedList.Items do
      begin
         if IndexOf( FullName ) < 0 then
         begin
            Add( FullName );
            // Position the "SelectedList" listbox at the bottom.
            Selected[Count - 1] := True;
            Selected[Count - 1] := False;
         end;
      end;
   end;
end;

end.
