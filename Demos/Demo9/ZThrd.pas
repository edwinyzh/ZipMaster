unit ZThrd;
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
  Classes, {ZipWrkr,} ZipMstr, StdCtrls, SysUtils, Messages, Main9_1;


type
  TZipThread = class(TThread)
  private
	{ Private declarations }
	zip: TZipMaster;
	ftext: string;
	fSpecs: TStrings;
	fName: String;
	fMemo: TMemo;
  err: integer;
  protected
	procedure Execute; override;
	procedure ShowText;
  procedure ZipMessage(Sender: TObject; ErrCode: integer; const Message:
      String);
	procedure CheckTerminate(Sender: TObject; var stop: boolean);
	procedure Finished(Sender: TObject);
  public
	constructor Create(Filename: string; Specs: TStrings; Memo: TMemo; suspended: boolean);
	destructor Destroy; override;      
	procedure AfterConstruction; override;
  end;

implementation

uses
  ZMMsg;

{ Important: Methods and properties of objects in VCL or CLX can only be used
  in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TZipThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TZipThread }

// runs own thread
procedure TZipThread.Execute;
begin
  zip.Active := true;
	zip.Add;
  err := zip.ErrCode;
end;

// must run main thread ie via Synchronize
procedure TZipThread.ShowText;
begin
	fMemo.Lines.Add(fText);
end;

procedure TZipThread.ZipMessage(Sender: TObject; ErrCode: integer; const
    Message: String);
begin
	if (ErrCode <> 0) and (ErrCode <> ZD_MISS{767}) then   // ignore missing
	begin
		fText := '('+IntToStr(ErrCode)+') '+Message;
		Synchronize(ShowText);		// in main thread add string to memo
	end  {
	else 	// showing all messages will slow it down
	begin
		fText := Message;
		Synchronize(ShowText);		// in main thread add string to memo
	end }
	;
end;

destructor TZipThread.Destroy;
begin
	zip.Free;
  inherited Destroy;
end;

// stops calls to ProcessMessages & checks for Terminated
procedure TZipThread.CheckTerminate(Sender: TObject; var stop: boolean);
begin
	if Terminated then
		stop := true;
end;

constructor TZipThread.Create(Filename: string; Specs: TStrings; Memo: TMemo; suspended: boolean);
begin
	inherited Create(suspended);
	fName := Filename;
	fSpecs := Specs;
	fMemo := Memo;
	zip := TZipMaster.Create(nil);	// no owner, so must free
  zip.Active := false;
  zip.NotMainThread := true;
	OnTerminate := Finished;
end;

procedure TZipThread.AfterConstruction;
begin
	zip.OnCheckTerminate := CheckTerminate;
	zip.OnMessage := ZipMessage;
	zip.Unattended := true;  
	zip.ZipFileName := fName;
	zip.FSpecArgs.Assign(fSpecs);
  zip.DLLDirectory := '..\..\dll';
	inherited; 					   
end;

// runs main thread
procedure TZipThread.Finished(Sender: TObject);
begin
	Form1.ZipThread := nil;			// don't allow Cancel
	fMemo.Lines.Add( 'Added '+IntToStr(zip.SuccessCnt)+' files');
	if zip.ErrCode <> 0 then
		fMemo.Lines.Add( 'Error '+IntToStr(zip.ErrCode and $FFFF)+'  '+zip.ErrMessage);
end;


end.

