unit ZMOprFile;

// ZMFileOpr.pas - file operations
(*
 Derived from
 * SFX for DelZip v1.7
 * Copyright 2002-2005
 * written by Markus Stephany
*)

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
// modified 2013-12-05

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
  ZMHandler;

type
  TZMOpConvertToSFX = class(TZMOperationRoot)
  private
    FOutName: string;
    FSpan: Boolean;
  public
    constructor Create(const OutName: string; Span: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpConvertToZip = class(TZMOperationRoot)
  public
    constructor Create;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpReadSpan = class(TZMOperationRoot)
  private
    FInFileName: string;
    FOutFilePath: string;
    FUseXProgress: Boolean;
  public
    constructor Create(const InFileName: string; var OutFilePath: string;
      UseXProgress: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpWriteSpan = class(TZMOperationRoot)
  private
    FInFileName: string;
    FOutFileName: string;
    FUseXProgress: Boolean;
  public
    constructor Create(const InFileName, OutFileName: string;
      UseXProgress: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
  ZMLister, ZMFileOpr;

{ TZMOpConvertToSFX }

constructor TZMOpConvertToSFX.Create(const OutName: string; Span: Boolean);
begin
  inherited Create;
  FOutName := OutName;
  FSpan := Span;
end;

function TZMOpConvertToSFX.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMFileOpr;
begin
  FOper := TZMFileOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  if FSpan then
    Result := FOper.ConvertToSpanSFX(FOutName, nil)
  else
    Result := FOper.ConvertToSFX(FOutName, nil);
end;

function TZMOpConvertToSFX.Name: string;
begin
  if FSpan then
    Result := 'ConvertToSpanSFX'
  else
    Result := 'ConvertToSFX';
end;

{ TZMOpConvertToZip }

constructor TZMOpConvertToZip.Create;
begin
  inherited;
end;

function TZMOpConvertToZip.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMFileOpr;
begin
  FOper := TZMFileOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.ConvertToZIP;
end;

function TZMOpConvertToZip.Name: string;
begin
  Result := 'ConvertToZip';
end;

{ TZMOpReadSpan }

constructor TZMOpReadSpan.Create(const InFileName: string;
  var OutFilePath: string; UseXProgress: Boolean);
begin
  inherited Create;
  FInFileName := InFileName;
  FOutFilePath := OutFilePath;
  FUseXProgress := UseXProgress;
end;

function TZMOpReadSpan.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMFileOpr;
begin
  FOper := TZMFileOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.ReadSpan(FInFileName, FOutFilePath, FUseXProgress);
end;

function TZMOpReadSpan.Name: string;
begin
  Result := 'ReadSpan';
end;

{ TZMOpWriteSpan }

constructor TZMOpWriteSpan.Create(const InFileName, OutFileName: string;
  UseXProgress: Boolean);
begin
  inherited Create;
  FInFileName := InFileName;
  FOutFileName := OutFileName;
  FUseXProgress := UseXProgress;
end;

function TZMOpWriteSpan.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMFileOpr;
begin
  FOper := TZMFileOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.WriteSpan(FInFileName, FOutFileName, FUseXProgress);
end;

function TZMOpWriteSpan.Name: string;
begin
  Result := 'WriteSpan';
end;

end.
