unit Unit1;
// Demo for reading from Zip Stream by Nikolaj Olsson (nikse@e-mail.dk)
// amended April 11, 2005 R.Peters
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
  ExtCtrls, StdCtrls, Buttons, ZipMstr;

type
  TForm1 = class(TForm)
    Image1: TImage;
    StaticText1: TStaticText;
    BtnPic1: TButton;
    BtnPic2: TButton;
    BtnPic3: TButton;
    ZipMaster1: TZipMaster;
    procedure UnzipToImage(filename : string);
    procedure BtnPic3Click(Sender: TObject);
    procedure BtnPic1Click(Sender: TObject);
    procedure BtnPic2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
//  Stream1 : TZipstream;

implementation

{$R *.DFM}

procedure TForm1.UnzipToImage(filename : string);
var
  Stream1: TMemoryStream;
begin
  if ZipMaster1.State < zsBusy then
  begin
    ZipMaster1.ZipFileName:= 'resource.zip';
    if ZipMaster1.Count > 0 then
    begin
      ZipMaster1.Password:='password';
      Stream1 := ZipMaster1.ExtractFileToStream(filename);
      if Stream1 <> nil then
      begin
        Stream1.Position := 0;      // reset to the beginning of the stream
        StaticText1.Caption:='File Size: '+ IntToStr(Stream1.Size);
        Image1.Picture.Bitmap.LoadFromStream(Stream1);
      end
      else
        StaticText1.Caption:='extraction error';
    end
    else
      StaticText1.Caption:='resource.zip not found or empty';
  end;
end;

procedure TForm1.BtnPic3Click(Sender: TObject);
begin
  UnzipToImage('eye.bmp');
end;

procedure TForm1.BtnPic1Click(Sender: TObject);
begin
  UnzipToImage('paintbrush.bmp');
end;

procedure TForm1.BtnPic2Click(Sender: TObject);
begin
  UnzipToImage('Spectrum.bmp');
end;

end.
