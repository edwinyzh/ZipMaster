Unit msgunit;

(* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011 Russell Peters and Roger Aelbrecht

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

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

Type
  TMsgform = Class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    FileBeingZipped: TLabel;
    StatusBar1: TStatusBar;
    Button1: TButton;
    Button2: TButton;
    MsgMemo: TMemo;
    PageControl1: TPageControl;
    MsgSheet: TTabSheet;
    IncSheet: TTabSheet;
    ExcSheet: TTabSheet;
    ExcMemo: TMemo;
    IncMemo: TMemo;
    Script: TTabSheet;
    ScriptMemo: TMemo;

    Procedure DismissButClick(Sender: TObject);
    Procedure CancelButClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);

  PUBLIC
    { Public declarations }
    FormInitialWidth: Integer;

    ProgressBar2: TProgressBar;
  End;

Var
  Msgform: TMsgform;

Implementation

Uses
  mainunit, ZipMstr;
{$R *.DFM}

Procedure TMsgform.DismissButClick(Sender: TObject);
Begin
  Hide;
End;

Procedure TMsgform.CancelButClick(Sender: TObject);
Begin
  If (MainForm.ZipMaster1.State >= zsBusy) Then
    MainForm.ZipMaster1.Cancel := True
  Else
    Hide; { nothing to cancel - assume user wants to close msg window }
End;

Procedure TMsgform.FormCreate(Sender: TObject);
Begin
  ProgressBar2 := TProgressBar.Create(StatusBar1); // Parent will delete it.
  ProgressBar1.Smooth := True;
//  With ProgressBar2 Do
//  Begin
    ProgressBar2.Parent := StatusBar1;
    ProgressBar2.Top := 2;
    ProgressBar2.Left := StatusBar1.Left + StatusBar1.Panels.Items[0]
      .Width + StatusBar1.Panels.Items[1].Width + 2;
    ProgressBar2.Height := StatusBar1.Height - 2;
    ProgressBar2.Min := 1;
    ProgressBar2.Max := 100;
    ProgressBar2.Smooth := True;
//  End;
  FormInitialWidth := Msgform.Width;
End;

Procedure TMsgform.FormResize(Sender: TObject);
Begin
  ProgressBar2.Width := StatusBar1.Width - StatusBar1.Panels.Items[0]
    .Width - StatusBar1.Panels.Items[1].Width - 18;
  ProgressBar1.Width := 177 + (Msgform.Width - FormInitialWidth);
End;

End.
