unit MsgUnit;
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
  StdCtrls, ExtCtrls, ComCtrls;

type
  TMsgform = class( TForm )
    Panel1:          TPanel;
    Panel2:          TPanel;
    DismissBut:      TButton;
    CancelBut:       TButton;
    FileBeingZipped: TLabel;
    ProgressBar1:    TProgressBar;
    StatusBar1:      TStatusBar;
    Memo1: TMemo;

    procedure DismissButClick( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FormResize( Sender: TObject );

  public
    { Public declarations }
    FormInitialWidth: Integer;

    ProgressBar2: TProgressBar;
  end;

var
  Msgform: TMsgform;

implementation

uses main, ZipMstr;

{$R *.DFM}

procedure TMsgform.DismissButClick( Sender: TObject );
begin
   Hide;
end;

procedure TMsgform.CancelButClick( Sender: TObject );
begin
//   if (MainForm.ZipMaster1.ZipBusy or MainForm.ZipMaster1.UnzBusy) then
   if MainForm.ZipMaster1.State >= zsBusy then
      MainForm.ZipMaster1.Cancel := True
   else
      Hide; { nothing to cancel - assume user wants to close msg window }
end;

procedure TMsgform.FormCreate( Sender: TObject );
begin
   ProgressBar2 := TProgressBar.Create( StatusBar1 );	// Parent will delete it.

   {$IfDef VER120}
   ProgressBar1.Smooth := True;
   {$EndIf}
   with ProgressBar2 do
   begin
      Parent  := StatusBar1;
      Top     := 2;
      Left    := StatusBar1.Left + StatusBar1.Panels.Items[0].Width +
                 StatusBar1.Panels.Items[1].Width + 2;
      Height  := StatusBar1.Height - 2;
      Min     := 1;
      Max     := 10001;
      {$IfDef VER120}
      Smooth  := True;
      {$EndIf}
   end;
   FormInitialWidth := MsgForm.Width;
end;

procedure TMsgform.FormResize( Sender: TObject );
begin
   ProgressBar2.Width := StatusBar1.Width - StatusBar1.Panels.Items[0].Width -
                         StatusBar1.Panels.Items[1].Width - 18;
   ProgressBar1.Width := 177 + (MsgForm.Width - FormInitialWidth);
end;

end.

