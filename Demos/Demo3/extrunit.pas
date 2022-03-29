unit extrunit;
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
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl;

type
  TExtract = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBut: TButton;
    CancelBut: TButton;
    DirectoryListBox1: TDirectoryListBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Panel3: TPanel;
    DriveComboBox1: TDriveComboBox;
    procedure OKButClick(Sender: TObject);
    procedure CancelButClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Extract: TExtract;

implementation

uses Unit1;

{$R *.DFM}

procedure TExtract.OKButClick(Sender: TObject);
begin
   Form1.ExtractDir:=DirectoryListBox1.Directory;
   if RadioGroup1.ItemIndex = 0 then
      Form1.ExpandDirs:=False
   else
      Form1.ExpandDirs:=True;
   if RadioGroup2.ItemIndex = 0 then
      Form1.Overwrite:=False
   else
      Form1.Overwrite:=True;
   Close;
end;

procedure TExtract.CancelButClick(Sender: TObject);
begin
   Form1.ExtractDir:='';
   Close;
end;

procedure TExtract.FormCreate(Sender: TObject);
begin
   RadioGroup1.ItemIndex := 0;  // dflt: do not expand dirs
   RadioGroup2.ItemIndex := 1;  // dflt: overwrite existing files
end;

end.
