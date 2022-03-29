unit ZMOprCore;

// ZMOprCore.pas -  Core operations

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
// modified 2014-09-11

{$I '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
  ZMHandler, ZipMstr;

type
  TZMOpSetExtStream = class(TZMOperationRoot)
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpSetZipFileName = class(TZMOperationRoot)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpSetDLLLoad = class(TZMOperationRoot)
  private
    FDoLoad: Boolean;
  public
    constructor Create(DoLoad: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpClear = class(TZMOperationRoot)
  public
    constructor Create;
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpList = class(TZMOperationRoot)
  public
    constructor Create;
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpForEach = class(TZMOperationRoot)
  private
    FFEFunc: TZMForEachFunction;
    FPData: Pointer;
  public
    constructor Create(Func: TZMForEachFunction; var Data);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpSetUseDirOnly = class(TZMOperationRoot)
  private
    FDirOnly: Boolean;
  public
    constructor Create(DirOnly: Boolean);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
  ZMLister, ZMDllLoad, ZMBody;

constructor TZMOpSetExtStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

{ TZMOpSetExtStream }

function TZMOpSetExtStream.Changes: TZMOperRes;
begin
  Result := [ZorZip];
end;

function TZMOpSetExtStream.Execute(TheBody: TZMHandler): Integer;
begin
  Result := 0;
  (TheBody as TZMLister).Set_ExtStream(FStream);
end;

function TZMOpSetExtStream.Name: string;
begin
  Result := 'SetExtStream';
end;

constructor TZMOpSetZipFileName.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

{ TZMOpSetZipFileName }

function TZMOpSetZipFileName.Changes: TZMOperRes;
begin
  Result := [ZorZip];
end;

function TZMOpSetZipFileName.Execute(TheBody: TZMHandler): Integer;
var
  Lister: TZMLister;
begin
  Result := 0;
  Lister := TheBody as TZMLister;
  Lister.Set_ZipFileName(FFileName, ZloFull);
end;

function TZMOpSetZipFileName.Name: string;
begin
  Result := 'SetZipFileName';
end;

{ TZMOpSetDLLLoad }

constructor TZMOpSetDLLLoad.Create(DoLoad: Boolean);
begin
  inherited Create;
  FDoLoad := DoLoad;
end;

function TZMOpSetDLLLoad.Execute(TheBody: TZMHandler): Integer;
var
  Lister: TZMLister;
begin
  Result := 0;
  Lister := TheBody as TZMLister;
{$IFDEF ZDEBUG}
  Lister.TraceFmt('set DLL_Load to %d', [Ord(Value)], {_LINE_}1382, __UNIT__);
{$ENDIF}
  if FDoLoad <> _DLL_Loaded(Lister.Master) then
  begin
    if FDoLoad then
      _DLL_Load(Lister)
    else
      _DLL_Unload(Lister);
{$IFDEF ZDEBUG}
    Lister.TraceFmt('changed DLL_Load to %d', [Ord(FValue)], {_LINE_}1391,
      __UNIT__);
{$ENDIF}
  end;
end;

function TZMOpSetDLLLoad.Name: string;
begin
  Result := 'SetDllLoad';
end;

{ TZMOpClear }

constructor TZMOpClear.Create;
begin
  inherited;
end;

function TZMOpClear.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

function TZMOpClear.Execute(TheBody: TZMHandler): Integer;
begin
  (TheBody as TZMLister).Clear;
  Result := 0;
end;

function TZMOpClear.Name: string;
begin
  Result := 'Clear';
end;

constructor TZMOpForEach.Create(Func: TZMForEachFunction; var Data);
begin
  inherited Create;
  FFEFunc := Func;
  FPData := @Data;
end;

{ TZMOpForEach }

function TZMOpForEach.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

function TZMOpForEach.Execute(TheBody: TZMHandler): Integer;
begin
  Result := (TheBody as TZMLister).ForEach(FFEFunc, FPData^)
end;

function TZMOpForEach.Name: string;
begin
  Result := 'ForEach';
end;

function TZMOpForEach.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

constructor TZMOpList.Create;
begin
  inherited;
end;

{ TZMOpList }

function TZMOpList.Changes: TZMOperRes;
begin
  Result := [ZorZip];
end;

function TZMOpList.Execute(TheBody: TZMHandler): Integer;
begin
  Result := (TheBody as TZMLister).List;
end;

function TZMOpList.Name: string;
begin
  Result := 'List';
end;

{ TZMOpSetUseDirOnly }

constructor TZMOpSetUseDirOnly.Create(DirOnly: Boolean);
begin
  inherited Create;
  FDirOnly := DirOnly;
end;

function TZMOpSetUseDirOnly.Execute(TheBody: TZMHandler): Integer;
begin
  (TheBody as TZMLister).UseDirOnlyEntries := FDirOnly;
  Result := 0;
end;

function TZMOpSetUseDirOnly.Name: string;
begin
  Result := 'SetUseDirOnly';
end;

end.
