unit ZMOprDeflate;

// ZMZipOpr.pas - Zip operations (limited only this version)

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

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
  ZMHandler, ZipMstr;

type
  TZMOpAddStreamToStream = class(TZMOperationRoot)
  private
    FInstream: TMemoryStream;
  public
    constructor Create(InStream: TMemoryStream);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
    function Needs: TZMOperRes; override;
  end;

type
  TZMOpDeflate = class(TZMOperationRoot)
  private
    FInStream: TStream;
    FLength: Int64;
    FOutStream: TStream;
    FPCRC: PCardinal;
    FPMethod: PZMDeflates;
  public
    constructor Create(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal);
    function Changes: TZMOperRes; override;
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, WinApi.Windows, Vcl.Dialogs,
{$ELSE}
  SysUtils, Windows, Dialogs, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMBaseOpr, ZMBody, ZMLister,
  ZMEngine, ZMMsg, ZMStructs, ZMXcpt, ZMUtils, ZMCRC;

const
  __UNIT__ = 26;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

type
  TZMDeflateOpr = class(TZMBaseOpr)
  private
    procedure DeflateProgress(Sender: TObject; const Count: Integer;
      IsRead: Boolean);
    function DoDeflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
  public
    function AddStreamToStream(InStream: TMemoryStream): Integer;
    function Deflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function PreCalcCRC(InStream: TStream; Count: Int64;
      var CRC: DWORD): Integer;
  end;

function TZMDeflateOpr.AddStreamToStream(InStream: TMemoryStream): Integer;
var
  CRC: Dword;
  Header: TZM_StreamHeader;
  Method: TZMDeflates;
  ZipStream: TStream;
begin
  Body.ZipStream.Size := 0; // clear ready
  if InStream = Body.ZipStream then
    Result := ZM_Error({_LINE_}131, ZE_InIsOutStream)
  else
    if Assigned(InStream) and (InStream.Size > 0) then
    begin
      ZipStream := Body.ZipStream;
      ZipStream.Size := 0;
      Method := ZmDeflate;
      CRC := 0;
      Body.ZipStream.Size := 0;
      Header.Method := METHOD_DEFLATED;
      Header.CRC := 0;
      ZipStream.WriteBuffer(Header, SizeOf(Header));
      Result := Deflate(ZipStream, InStream, -1, Method, CRC);
      if SuccessCnt = 1 then
      begin
        ZipStream.Position := 0;
        case Method of
          ZmStore:
            Header.Method := METHOD_STORED;
          ZmDeflate:
            Header.Method := METHOD_DEFLATED;
        end;
        Header.CRC := CRC;
        ZipStream.WriteBuffer(Header, SizeOf(Header)); // position will be 6
      end
      else
        ZipStream.Size := 0;
    end
    else
      Result := ZM_Error({_LINE_}160, ZE_NothingToZip);
end;

function TZMDeflateOpr.Deflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
begin
  Result := 0;
  if not Assigned(InStream) then
    Result := ZM_Error({_LINE_}168, ZE_NoInStream)
  else
    if not Assigned(OutStream) then
      Result := ZM_Error({_LINE_}171, ZE_NoOutStream)
    else
      if InStream = OutStream then
        Result := ZM_Error({_LINE_}174, ZE_InIsOutStream);
  if Result = 0 then
    Result := DoDeflate(OutStream, InStream, Length, Method, CRC);
end;

procedure TZMDeflateOpr.DeflateProgress(Sender: TObject; const Count: Integer;
  IsRead: Boolean);
begin
  if IsRead then
    Progress.Advance(Count)
  else
    Progress.MoreWritten(Count);
  if Body.Cancel <> 0 then
    raise EZMAbort.Create;
end;

function TZMDeflateOpr.DoDeflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  Deflater: TZMCompressor;
  Mthd: Integer;
begin
  if (Method = ZmDeflate) and (Master.AddCompLevel > 0) then
    Mthd := METHOD_DEFLATED
  else
    Mthd := METHOD_STORED;
  if Length < 0 then
    Length := InStream.Size;
  if Length = 0 then
  begin
    Length := InStream.Size;
    InStream.Position := 0;
  end;
  Progress.TotalCount := 1;
  Progress.TotalSize := Length;
  Deflater := TZMCompressor.Create(Length);
  try
    Deflater.InStream := InStream;
    Deflater.OutStream := OutStream;
    Deflater.CompLevel := Master.AddCompLevel;
    Deflater.OnProgress := DeflateProgress;
    Progress.NewItem('<stream>', Length);
    Result := Deflater.Prepare(Mthd);
    if Result >= 0 then
    begin
      Result := Deflater.Compress;
      if Result >= 0 then
      begin
        CRC := Deflater.CRC; // always give result crc
      end;
    end;
  finally
    Deflater.Free;
    Progress.EndItem;
  end;

  if Result >= 0 then
  begin // success
    Result := 0;
    SuccessCnt := 1;
    if Mthd <> 0 then
      Method := ZmDeflate
    else
      Method := ZmStore;
  end;
end;

function TZMDeflateOpr.PreCalcCRC(InStream: TStream; Count: Int64;
  var CRC: DWORD): Integer;
const
  BufSizeMax = $F000;
var
  Buf: PByte;
  BufSize: Integer;
  Err: Integer;
  InPosition: Int64;
begin
  CRC := 0;
  InPosition := InStream.Position;
  BufSize := BufSizeMax;
  if Count < BufSize then
    BufSize := (Count or $3FF) + 1;
  GetMem(Buf, BufSize);
  try
    Progress.NewXtraItem('<stream>', Count);
    repeat
      Result := InStream.Read(Buf^, BufSize);
      if Result < BufSize then
        Break; // return error
      CRC := ZCRC32(CRC, Buf^, Result);
      Progress.AdvanceXtra(Result);
      Dec(Count, Result);
      if Count < BufSize then
        BufSize := Count;
    until Count <= 0;
  finally
    FreeMem(Buf);
    Err := InStream.Seek(InPosition,
{$IFDEF VERPre6}Word{$ENDIF}(TSeekOrigin(SoBeginning)));
    if Err < 0 then
      Result := Err;
  end;
end;

constructor TZMOpAddStreamToStream.Create(InStream: TMemoryStream);
begin
  inherited Create;
  FInstream := InStream;
end;

{ TZMOpAddStreamToStream }

function TZMOpAddStreamToStream.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpAddStreamToStream.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMDeflateOpr;
begin
  FOper := TZMDeflateOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.AddStreamToStream(FInstream);
end;

function TZMOpAddStreamToStream.Name: string;
begin
  Result := 'AddStreamToStream';
end;

function TZMOpAddStreamToStream.Needs: TZMOperRes;
begin
  Result := [];
end;

constructor TZMOpDeflate.Create(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal);
begin
  inherited Create;
  FOutStream := OutStream;
  FInStream := InStream;
  FLength := Length;
  FPMethod := PZMDeflates(@Method);
  FPCRC := PCardinal(@CRC);
end;

{ TZMOpDeflate }

function TZMOpDeflate.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOpDeflate.Execute(TheBody: TZMHandler): Integer;
var
  CRC: Dword;
  FOper: TZMDeflateOpr;
  Mthd: TZMDeflates;
begin
  FOper := TZMDeflateOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Mthd := FPMethod^;
  Result := FOper.Deflate(FOutStream, FInStream, FLength, Mthd, CRC);
  if Result = 0 then
  begin
    FPMethod^ := Mthd;
    FPCRC^ := CRC;
  end;
end;

function TZMOpDeflate.Name: string;
begin
  Result := 'Deflate';
end;

end.
