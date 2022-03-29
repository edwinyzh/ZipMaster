unit ZMOprUnzip;

// ZMUnzipOpr.pas - unzip operations

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

(*
 FSpecArgs --
 [global switches]
 spec [local switches]
 //  file.zip>>filespec [local switches]
 switches
 /D:"[< or >]date"        [< _ before or > _ after (default)] date
 /D:"[< or >]-days"       [< _ before or > _ after (default)] days ago
 /J[+ or -]              Junk dirs
 /O[A or N or O or -]     overwrite always, newer, older, never
 /N[+ or -]  flags not to use AddNewName (default N- _ use AddNewName)
 /X:[old]::[new]  replace 'old' with 'new' - must result in valid internal name
 spec  select files in current zip according to spec
 /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to
 globals otherwise use spec
 /F: folder  change ExtrBaseDir
 /S or /S-  turns on or off recurse into sub-folders
 changes to excludes occur at current line and continue until changed

 <password  use password (to eol)
 - does not change already included files.
 local switches only applies to that line and modifies the 'current' excludes.
*)
interface

uses
{$IFDEF VERDXE2up}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
  ZMHandler, ZipMstr;

type
  TZMOpExtractFileToStream = class(TZMOperationRoot)
  private
    FEntryName: string;
  public
    constructor Create(const EntryName: string);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpExtractStreamToStream = class(TZMOperationRoot)
  private
    FHeaderType: TZMZHeader;
    FInstream: TMemoryStream;
    FOutSize: Longword;
  public
    constructor Create(InStream: TMemoryStream; OutSize: Longword;
      HeaderType: TZMZHeader);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

type
  TZMOpUnzipFiles = class(TZMOperationRoot)
  public
    constructor Create;
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpUndeflate = class(TZMOperationRoot)
  private
    FInStream: TStream;
    FLength: Int64;
    FOutStream: TStream;
    FPCRC: PCardinal;
    FPMethod: PZMDeflates;
  public
    constructor Create(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal);
    procedure BeforeDestruction; override;
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

  // extract single file
type
  TZMOpUnzipToFile = class(TZMOperationRoot)
  private
    FDestName: string;
    FExtRec: TZMDirEntry;
  public
    constructor Create(const DestName: string; const ExtRec: TZMDirEntry);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

  // extract single file
type
  TZMOpUnzipToStream = class(TZMOperationRoot)
  private
    FDestStream: TStream;
    FExtRec: TZMDirEntry;
  public
    constructor Create(DestStream: TStream; const ExtRec: TZMDirEntry);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Windows,
{$ELSE}
  Windows,
{$ENDIF}
  ZMLister,
  ZMUnzipOpr;

const
  __UNIT__ = 33;

  { TZMOpExtractFileToStream }

constructor TZMOpExtractFileToStream.Create(const EntryName: string);
begin
  inherited Create;
  FEntryName := EntryName;
end;

function TZMOpExtractFileToStream.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpExtractFileToStream.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMUnzipOpr;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.ExtractFileToStream(FEntryName);
end;

function TZMOpExtractFileToStream.Name: string;
begin
  Result := 'ExtractFileToStream';
end;

{ TZMOpExtractStreamToStream }

constructor TZMOpExtractStreamToStream.Create(InStream: TMemoryStream;
  OutSize: Longword; HeaderType: TZMZHeader);
begin
  inherited Create;
  FInstream := InStream;
  FOutSize := OutSize;
  FHeaderType := HeaderType;
end;

function TZMOpExtractStreamToStream.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpExtractStreamToStream.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMUnzipOpr;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.ExtractStreamToStream(FInstream, FOutSize, FHeaderType);
end;

function TZMOpExtractStreamToStream.Name: string;
begin
  Result := 'ExtractFileToStream';
end;

constructor TZMOpUnzipFiles.Create;
begin
  inherited Create;
end;

{ TZMOpUnzipFiles }

function TZMOpUnzipFiles.Changes: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

function TZMOpUnzipFiles.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMUnzipOpr;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.UnzipFiles;
end;

function TZMOpUnzipFiles.Name: string;
begin
  Result := 'Extract';
end;

function TZMOpUnzipFiles.Needs: TZMOperRes;
begin
  Result := [ZorFSpecArgs];
end;

constructor TZMOpUndeflate.Create(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal);
begin
  inherited Create;
  FOutStream := OutStream;
  FInStream := InStream;
  FLength := Length;
  FPMethod := @Method;
  FPCRC := @CRC;
end;

procedure TZMOpUndeflate.BeforeDestruction;
begin
  inherited;
end;

{ TZMOpUndeflate }

function TZMOpUndeflate.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpUndeflate.Execute(TheBody: TZMHandler): Integer;
var
  CRC: Dword;
  FOper: TZMUnzipOpr;
  Mthd: TZMDeflates;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Mthd := FPMethod^;
  Result := FOper.Undeflate(FOutStream, FInStream, FLength, Mthd, CRC);
  if Result = 0 then
  begin
    FPMethod^ := Mthd;
    FPCRC^ := CRC;
  end;
end;

function TZMOpUndeflate.Name: string;
begin
  Result := 'Undeflate';
end;

constructor TZMOpUnzipToFile.Create(const DestName: string;
  const ExtRec: TZMDirEntry);
begin
  inherited Create;
  FDestName := DestName;
  FExtRec := ExtRec;
end;

{ TZMOpUnzipToFile }

function TZMOpUnzipToFile.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpUnzipToFile.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMUnzipOpr;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.UnzipToFile(FDestName, FExtRec);
end;

function TZMOpUnzipToFile.Name: string;
begin
  Result := 'UnzipToFile';
end;

constructor TZMOpUnzipToStream.Create(DestStream: TStream;
  const ExtRec: TZMDirEntry);
begin
  inherited Create;
  FDestStream := DestStream;
  FExtRec := ExtRec;
end;

{ TZMOpUnzipToStream }

function TZMOpUnzipToStream.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpUnzipToStream.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMUnzipOpr;
begin
  FOper := TZMUnzipOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.UnzipToStream(FDestStream, FExtRec);
end;

function TZMOpUnzipToStream.Name: string;
begin
  Result := 'UnzipToStream';
end;

end.
