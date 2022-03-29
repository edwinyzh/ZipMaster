unit ZMXcpt;

// ZMXcpt.pas - Exception class for ZipMaster

(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

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
// modified 2013-12-06

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  ZMHandler;

type
  EZMException = class(Exception)
  private
{$IFNDEF UNICODE}
    FUMessage: string; // only used if UsingUTF8 is set
{$ENDIF}
    FExtErr: Integer;
    procedure SetExtErr(const Value: Integer);
  public
    property ExtErr: Integer read FExtErr write SetExtErr;
{$IFNDEF UNICODE}
    property UMessage: string read FUMessage write FUMessage;
{$ENDIF}
  end;

type
  EZipMaster = class(EZMException)
    constructor CreateMsg(Core: TZMRoot; Ident, LineNo, UnitNo: Integer);
    constructor CreateMsgFmt(Core: TZMHandler; Ident: Integer;
      const Args: array of const; LineNo, UnitNo: Integer);
  public
  end;

type
  EZMAbort = class(EZMException)
  public
    constructor Create;
  end;

implementation

uses
  ZMMsg, ZMUtils;

procedure EZMException.SetExtErr(const Value: Integer);
begin
  if Value > 0 then
    FExtErr := -Value
  else
    FExtErr := Value;
end;

constructor EZMAbort.Create;
begin
  inherited Create('User Abort');
  ExtErr := -ZS_Abort;
end;

{ EZipMaster }

constructor EZipMaster.CreateMsg(Core: TZMRoot; Ident, LineNo, UnitNo: Integer);
var
  Msg: string;
begin
  Msg := Core.ErrMsg(Ident);
{$IFDEF UNICODE}
  inherited Create(Msg);
{$ELSE}
  inherited Create(Core.AsSysStr(Msg)); // create with ansi message
  UMessage := Msg;
{$ENDIF}
  if Ident < 0 then
    ExtErr := Ident
  else
    ExtErr := -((UnitNo shl ZERR_UNIT_SHIFTS) + (LineNo shl ZERR_LINE_SHIFTS) or
      AbsErr(Ident));
end;

constructor EZipMaster.CreateMsgFmt(Core: TZMHandler; Ident: Integer;
  const Args: array of const; LineNo, UnitNo: Integer);
var
  Msg: string;
begin
  Msg := Core.ZipFmtLoadStr(Ident, Args);
{$IFDEF UNICODE}
  inherited Create(Msg);
{$ELSE}
  inherited Create(Core.AsSysStr(Msg)); // create with ansi message
  UMessage := Msg;
{$ENDIF}
  if Ident < 0 then
    ExtErr := Ident
  else
    ExtErr := -((UnitNo shl ZERR_UNIT_SHIFTS) + (LineNo shl ZERR_LINE_SHIFTS) or
      AbsErr(Ident));
end;

end.
