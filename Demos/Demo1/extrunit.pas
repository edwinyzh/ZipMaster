Unit extrunit;
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

{$INCLUDE 'ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, ShlObj;

Type
  TExtract = Class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    OKBut: TButton;
    CancelBut: TButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;

    Procedure OKButClick(Sender: TObject);
    Procedure CancelButClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  End;

Var
  Extract: TExtract;

Implementation

Uses MainUnit;
{$R *.DFM}

Procedure TExtract.OKButClick(Sender: TObject);
Begin
  MainUnit.Canceled := False;
  MainUnit.ExtractDir := DirectoryListBox1.Directory;
  If RadioGroup1.ItemIndex = 0 Then
    MainUnit.ExpandDirs := False
  Else
    MainUnit.ExpandDirs := True;
  If RadioGroup2.ItemIndex = 0 Then
    MainUnit.Overwr := False
  Else
    MainUnit.Overwr := True;
  If RadioGroup3.ItemIndex = 0 Then
    MainUnit.AllFiles := True
  Else
    MainUnit.AllFiles := False;
  Close;
End;

Procedure TExtract.CancelButClick(Sender: TObject);
Begin
  MainUnit.ExtractDir := '';
  Close;
End;

Procedure TExtract.FormActivate(Sender: TObject);
Begin
  MainUnit.Canceled := True; { default }
End;

Procedure TExtract.FormCreate(Sender: TObject);
Var
  SpecFolder: String;
Begin
  SpecFolder := '';

  MainForm.GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, SpecFolder);
  DriveComboBox1.Drive := ExtractFileDrive(SpecFolder)[1];
  DirectoryListBox1.Directory := ExtractFilePath(SpecFolder);
End;

End.
