unit Unit1;   { DEMO 2 for Delphi Zip by Eric W. Engler }
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
  StdCtrls, ZipMstr;

type
  TForm1 = class(TForm)
    ZipBut: TButton;
    UnzipBut: TButton;
    ExitBut: TButton;
    DelBut: TButton;
    VersionBut: TButton;
    ZipMaster1: TZipMaster;
    procedure ZipButClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExitButClick(Sender: TObject);
    procedure ZipMaster1Message(Sender: TObject; ErrCode: Integer;
      Message: string);
    procedure UnzipButClick(Sender: TObject);
    procedure DelButClick(Sender: TObject);
    procedure VersionButClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   { SetCurrentDir('C:\ZIP\DEMO2'); }
   Caption:='ZIP Demo 2 - ' + GetCurrentDir;
   ZipMaster1.ZipFileName:='test.zip';
//   ZipMaster1.Dll_Load := true;
end;

{ Add one file to the zipfile }
procedure TForm1.ZipButClick(Sender: TObject);
begin
   if not FileExists('TEST.DAT') then
   begin
      ShowMessage('Error - test.dat not found');
      Exit;
   end;                         
   ZipMaster1.Dll_Load := true;
   ZipMaster1.FSpecArgs.Add('TEST.DAT');
   ZipMaster1.Add;
   ShowMessage('Files added = ' + IntToStr(ZipMaster1.SuccessCnt));
end;

{ expand all files from the zipfile }
procedure TForm1.UnzipButClick(Sender: TObject);
begin
  with ZipMaster1 do
  begin
     if Count = 0 then
     begin
        ShowMessage('Error - no files in the Zip file');
        Exit;
     end;
     { If we don't specify filenames, we will extract them all. }
     { Of course, in this little demo there is only 1 file in the ZIP. }
     FSpecArgs.Add('*.*');
     ExtrBaseDir:=GetCurrentDir;
     { if the file to be extracted already exists, overwrite it }
     ExtrOptions:=ExtrOptions+[ExtrOverwrite];
     Extract;
     ShowMessage('Files extracted = ' + IntToStr(SuccessCnt));
  end;
end;

{ delete one file from the zipfile }
procedure TForm1.DelButClick(Sender: TObject);
begin
   ZipMaster1.FSpecArgs.Add('TEST.DAT');
   ZipMaster1.Delete;
   ShowMessage('Files deleted = ' + IntToStr(ZipMaster1.SuccessCnt));
end;

procedure TForm1.VersionButClick(Sender: TObject);
begin                                                                 
   ShowMessage('DelZip191.DLL version: ' + ZipMaster1.Dll_Version
      + #13#10 + 'Loaded from: ' + ZipMaster1.Dll_Path
   + #13#10 + 'ZipMaster version: ' + ZipMaster1.Version);//Info);
end;

procedure TForm1.ExitButClick(Sender: TObject);
begin
   Close;
end;

{ This procedure displays messages received from the DLLs.  If you really
  want to minimize the amount of messages you show the user, you don't
  even need to assign this event handler.  However, I'd still recommend
  that you assign this to catch errors.  You can test the ErrCode
  before you display the message - if ErrCode is non-zero, make sure you
  display the message.  If it's 0, then you can ignore the message.
    Also, if ZipMaster1's "Verbose" property is true, you'll get more
  informational message callbacks here. By default, it's false to
  minimize user messages. }
procedure TForm1.ZipMaster1Message(Sender: TObject; ErrCode: Integer;
  Message: string);
begin
   { if ErrCode <> 0 then }   { uncomment this line to show errors ONLY }
   ShowMessage(Message);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ZipMaster1.Dll_Load := false;
end;

end.
 