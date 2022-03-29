unit ZMEngine;

// ZMEngine.pas - Compress/decompress and crypt engines interface

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
{$Q-}  // range check off

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils;
{$ELSE}
  Windows, Classes, SysUtils;
{$ENDIF}

type
  TZM_ProgressEvent = procedure(Sender: TObject; const Count: Integer;
    IsRead: Boolean) of object;

type
  TZMCryptTypes = (ZcNone, ZcZip);

type
  TZMBuffer = class
  private
    FActualSize: Integer;
    FBufPtr: PByte;
    FMyBuffer: array of Byte;
    FSize: Integer;
    procedure Resize(const Value: Integer);
    procedure SetSize(const Value: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CalcBufSize(Value: Int64): Integer;
    procedure Clean;
    property BufPtr: PByte read FBufPtr;
    property Size: Integer read FSize write SetSize;
  end;

type
  TZMCryptorBase = class
  protected
    function GetState: Integer; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    function Supports(CryptType: TZMCryptTypes): Boolean; virtual; abstract;
    property State: Integer read GetState;
  end;

type
  TZMDecryptorBase = class(TZMCryptorBase)
  public
    function DecodeInit(RefValue: Cardinal): Integer; virtual; abstract;
    function ReadIn(Buf: PByte; const Count: Integer): Integer;
      virtual; abstract;
    // 1 Return <0 _ error, 0 _ match, >0 _ no match
    function Unlock(const PassPhrase: AnsiString): Integer; virtual; abstract;
  end;

type
  TZMEncryptorBase = class(TZMCryptorBase)
  public
    function EncodeInit(const PassPhrase: AnsiString; RefValue: DWORD): Integer;
      virtual; abstract;
    function WriteOut(const Buf: PByte; const Count: Integer): Integer;
      virtual; abstract;
  end;

type
  TZMCoDecBase = class
  public
    function Execute: Integer; virtual; abstract;
    function Initial(CompressionType: Integer): Integer; virtual; abstract;
    function Supports(CompressionType: Integer): Boolean; virtual; abstract;
  end;

type
  TZMEngine = class
  private
    FCompLevel: Integer;
    FCRC: Cardinal;
    FEncrypted: Boolean;
    FInBuffer: TZMBuffer;
    FInSize: Int64;
    FInStrm: TStream;
    FOnProgress: TZM_ProgressEvent;
    FOutBuffer: TZMBuffer;
    FOutSize: Int64;
    FOutStrm: TStream;
    FState: Integer;
    FTheCryptor: TZMCryptorBase;
    FToBeWritten: Int64;
    FWrote: Int64;
    FZBufferSize: Cardinal;
    function CalcBufSize(Value: Int64): Integer;
    procedure MakeBuffers;
  private
    FCoDec: TZMCoDecBase;
    procedure SetTheCryptor(const Value: TZMCryptorBase);
  protected
    procedure Finish; virtual;
    function InitialCoDec(CompressionType: Integer): Integer; virtual; abstract;
    procedure SetCoDec(ACoDec: TZMCoDecBase);
    function SetCryptor(CryptType: TZMCryptTypes): Integer; virtual; abstract;
    property State: Integer read FState;
    property TheCryptor: TZMCryptorBase read FTheCryptor write SetTheCryptor;
  public
    constructor Create; overload;
    constructor Create(BufSize: Int64); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function DoReadIn(Buf: PByte; const Count: Integer): Integer;
    function DoWriteOut(const Buf: PByte; const Count: Integer): Integer;
    function Prepare(CompressionType: Integer): Integer;
    function PrepareCrypt(CryptType: TZMCryptTypes): Integer;
    function ReadIn(Buf: PByte; const Count: Integer): Integer; virtual;
    function WriteOut(const Buf: PByte; const Count: Integer): Integer; virtual;
    property CompLevel: Integer read FCompLevel write FCompLevel;
    property CRC: Cardinal read FCRC write FCRC;
    property Encrypted: Boolean read FEncrypted;
    property InBuffer: TZMBuffer read FInBuffer;
    property InSize: Int64 read FInSize write FInSize;
    // max to read, amount read
    property InStream: TStream read FInStrm write FInStrm;
    property OutBuffer: TZMBuffer read FOutBuffer;
    property OutSize: Int64 read FOutSize write FOutSize; // max to write
    property OutStream: TStream read FOutStrm write FOutStrm;
    property ToBeWritten: Int64 read FToBeWritten write FToBeWritten;
    property Wrote: Int64 read FWrote write FWrote;
    property OnProgress: TZM_ProgressEvent read FOnProgress write FOnProgress;
  end;

type
  TZMCompressor = class(TZMEngine)
  private
    FEncryptor: TZMEncryptorBase;
  protected
    procedure Finish; override;
    function InitialCoDec(CompressionType: Integer): Integer; override;
    function SetCryptor(CryptType: TZMCryptTypes): Integer; override;
  public
    function Compress: Integer;
    function WriteOut(const Buf: PByte; const Count: Integer): Integer;
      override;
    property Encryptor: TZMEncryptorBase read FEncryptor;
  end;

type
  TZMDecompressor = class(TZMEngine)
  private
    FDecryptor: TZMDecryptorBase;
  protected
    procedure Finish; override;
    function InitialCoDec(CompressionType: Integer): Integer; override;
    function SetCryptor(CryptType: TZMCryptTypes): Integer; override;
  public
    function Decompress: Integer;
    function ReadIn(Buf: PByte; const Count: Integer): Integer; override;
    property Decryptor: TZMDecryptorBase read FDecryptor;
  end;

function DeflateZStream(OutStream, InStream: TStream; Length: Int64): Integer;

function ExtractZStream(OutStream, InStream: TStream; Length: Int64): Integer;

function UndeflateQ(OutStream, InStream: TStream; Length: Int64; Method: Word;
  var CRC: Cardinal): Integer;

implementation

uses
  ZMMsg, ZMUtils, ZMStructs, ZMCoDec;

const
  __UNIT__ = 15;

{$IFDEF VERPre6}
type
  UInt64 = Int64;
{$ENDIF}

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

function UndeflateQ(OutStream, InStream: TStream; Length: Int64; Method: Word;
  var CRC: Cardinal): Integer;
var
  Inflater: TZMDecompressor;
begin
  if (Method = METHOD_DEFLATED) or (Method = METHOD_STORED) then
  begin
    Inflater := TZMDecompressor.Create(InStream.Size);
    try
      Inflater.InStream := InStream;
      Inflater.OutStream := OutStream;
      Inflater.InSize := Length;
      Result := Inflater.Prepare(Method);
      if Result = 0 then
        Result := Inflater.Decompress;
      if (Result = 0) and (Inflater.CRC <> CRC) then
      begin
        Result := ZM_Error({_LINE_}251, ZE_BadCRC);
        CRC := Inflater.CRC;
      end;
    finally
      Inflater.Free;
    end;
  end
  else
    Result := ZM_Error({_LINE_}259, ZE_Unsupported);
end;

function ExtractZStream(OutStream, InStream: TStream; Length: Int64): Integer;
var
  Heder: TZM_StreamHeader;
begin
  if InStream.Read(Heder, SizeOf(TZM_StreamHeader)) = SizeOf(TZM_StreamHeader)
  then
    Result := UndeflateQ(OutStream, InStream, Length - SizeOf(TZM_StreamHeader),
      Heder.Method, Heder.CRC)
  else
    Result := ZM_Error({_LINE_}271, ZE_ReadError);
end;

function DeflateZStream(OutStream, InStream: TStream; Length: Int64): Integer;
var
  Deflater: TZMCompressor;
  Heder: TZM_StreamHeader;
  Position1: Int64;
  Position2: Int64;
begin
  Heder.Method := 8;
  Heder.CRC := 0;
  Position1 := OutStream.Position;
  if OutStream.Write(Heder, Sizeof(Heder)) = SizeOf(Heder) then
  begin
    Deflater := TZMCompressor.Create(InStream.Size);
    try
      Deflater.InStream := InStream;
      if Length > 0 then
        Deflater.InSize := Length
      else
        Deflater.InSize := -1; // assume want the lot
      Deflater.OutStream := OutStream;
      Deflater.CompLevel := 9;
      Result := Deflater.Prepare(METHOD_DEFLATED);
      if Result = 0 then
        Result := Deflater.Compress;
      if Result >= 0 then
        Heder.CRC := Deflater.CRC; // good
    finally
      Deflater.Free;
    end;
    if Result >= 0 then
    begin
      // rewrite Heder
      Position2 := OutStream.Position;
      OutStream.Position := Position1; // reposition to heder
      if OutStream.Write(Heder, Sizeof(Heder)) = SizeOf(Heder) then
        OutStream.Position := Position2; // back at end
    end;
  end
  else
    Result := ZM_Error({_LINE_}313, ZE_WriteError);
end;

procedure TZMBuffer.AfterConstruction;
begin
  inherited;
  FActualSize := 0;
  FSize := 0;
  FBufPtr := nil;
end;

procedure TZMBuffer.BeforeDestruction;
begin
  FMyBuffer := nil;
  inherited;
end;

function TZMBuffer.CalcBufSize(Value: Int64): Integer;
begin
  if Value < 0 then
    Result := -Integer(Value)
  else
  begin
    Result := Value div 1024;
    if Result > 63 then
      Result := 63
    else
      if Result < 4 then
        Result := 3;
    Result := (1 + Result) * 1024;
  end;
end;

// fill with zeros
procedure TZMBuffer.Clean;
begin
  ZeroMemory(BufPtr, Size);
end;

procedure TZMBuffer.Resize(const Value: Integer);
begin
  SetLength(FMyBuffer, Value);
  FActualSize := Value;
  FSize := Value;
  FBufPtr := PByte(@FMyBuffer[0]);
end;

procedure TZMBuffer.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    if FActualSize < Value then
      Resize(Value)
    else
      FSize := Value;
  end;
end;

constructor TZMEngine.Create;
begin
  inherited;
  FZBufferSize := 8192;
end;

constructor TZMEngine.Create(BufSize: Int64);
begin
  inherited Create;
  FZBufferSize := CalcBufSize(BufSize);
end;

procedure TZMEngine.AfterConstruction;
begin
  inherited;
  MakeBuffers;
  FInSize := -1;
  FOutSize := -1;
  FCompLevel := 9;
end;

procedure TZMEngine.BeforeDestruction;
begin
  FInBuffer.Free;
  OutBuffer.Free;
  FTheCryptor.Free;
  FCoDec.Free;
  inherited;
end;

function TZMEngine.CalcBufSize(Value: Int64): Integer;
begin
  if Value < 0 then
    Result := {-}Integer(-Value)
  else
  begin
    Result := Value div 1024;
    if Result > 63 then
      Result := 63
    else
      if Result < 4 then
        Result := 3;
    Result := (1 + Result) * 1024;
  end;
end;

function TZMEngine.DoReadIn(Buf: PByte; const Count: Integer): Integer;
var
  ToRead: Integer;
begin
  Result := 0;
  ToRead := Count;
  if (InSize >= 0) and (ToRead > InSize) then
    ToRead := InSize;
  if ToRead > 0 then
    Result := InStream.Read(Buf^, ToRead);
  if Result > 0 then
  begin
    if InSize >= 0 then
      InSize := InSize - Result;
    if Assigned(OnProgress) then
      OnProgress(Self, Result, True); // amount read
  end;
end;

function TZMEngine.DoWriteOut(const Buf: PByte; const Count: Integer): Integer;
var
  DidWrite: Integer;
begin
  Result := 0;
  if Count > 0 then
  begin
    if ToBeWritten > 0 then
    begin
      OutSize := OutSize + Count;
      if OutSize > ToBeWritten then
      begin
        Result := ZM_Error({_LINE_}448, ZE_WrongLength);
        Exit;
      end;
    end;
    if Assigned(OutStream) then
    begin
      DidWrite := OutStream.Write(Buf^, Count);
      if DidWrite <> Count then
      begin
        if DidWrite < 0 then
          Result := DidWrite
        else
          Result := ZM_Error({_LINE_}460, ZE_WriteError);
        Exit;
      end;
    end
    else
      DidWrite := Count;
    if DidWrite > 0 then
    begin
      Wrote := Wrote + DidWrite;
      if Assigned(OnProgress) then
        OnProgress(Self, DidWrite, False); // amount written
    end;
  end;
end;

procedure TZMEngine.Finish;
begin
  FEncrypted := False;
  FState := 1; // finished
end;

procedure TZMEngine.MakeBuffers;
begin
  FInBuffer := TZMBuffer.Create;
  FOutBuffer := TZMBuffer.Create;
  FInBuffer.Size := FZBufferSize;
  OutBuffer.Size := FZBufferSize;
end;

function TZMEngine.Prepare(CompressionType: Integer): Integer;
begin
  FState := -1; // not prepared
  FEncrypted := False;
  ToBeWritten := -1;
  FCRC := 0;
  FWrote := 0;
  Result := InitialCoDec(CompressionType);
  if Result >= 0 then
    FState := 0;
end;

function TZMEngine.PrepareCrypt(CryptType: TZMCryptTypes): Integer;
begin
  if State = 0 then
  begin
    Result := SetCryptor(CryptType);
    FEncrypted := Result = 1;
  end
  else
    Result := ZM_Error({_LINE_}509, ZE_LogicError);
end;

function TZMEngine.ReadIn(Buf: PByte; const Count: Integer): Integer;
begin
  Result := DoReadIn(Buf, Count);
end;

procedure TZMEngine.SetCoDec(ACoDec: TZMCoDecBase);
begin
  if FCoDec <> ACoDec then
  begin
    FCoDec.Free;
    FCoDec := ACoDec;
  end;
end;

procedure TZMEngine.SetTheCryptor(const Value: TZMCryptorBase);
begin
  if FTheCryptor <> Value then
  begin
    FTheCryptor.Free;
    FTheCryptor := Value;
  end;
end;

function TZMEngine.WriteOut(const Buf: PByte; const Count: Integer): Integer;
begin
  Result := DoWriteOut(Buf, Count);
end;

{ TZMCompressor }

function TZMCompressor.Compress: Integer;
begin
  if @InStream = @OutStream then
  begin
    Result := ZM_Error({_LINE_}546, ZE_InIsOutStream);
    Exit;
  end;
  if State = 0 then
  begin
    Result := FCoDec.Execute;
    Finish;
  end
  else
    Result := ZM_Error({_LINE_}555, ZE_LogicError);
end;

function TZMCompressor.InitialCoDec(CompressionType: Integer): Integer;
begin
  Result := 0;
  FEncryptor := nil;
  if CompLevel < 1 then
    CompressionType := METHOD_STORED;
  if not(Assigned(FCoDec) and FCoDec.Supports(CompressionType)) then
  begin
    // need new CoDec
    case CompressionType of
      METHOD_STORED:
        SetCoDec(TZMCoDecStore.Create(Self));
      METHOD_DEFLATED:
        SetCoDec(TZMCoDecDeflate.Create(Self));
    else
      Result := -ZE_Unsupported;
    end;
  end;
  if Result = 0 then
    Result := FCoDec.Initial(CompressionType);
end;

procedure TZMCompressor.Finish;
begin
  if Assigned(Encryptor) then
    Encryptor.Clear;
  FEncryptor := nil;
  inherited;
end;

function TZMCompressor.SetCryptor(CryptType: TZMCryptTypes): Integer;
begin
  Result := 0;
  if CryptType = ZcNone then
    FEncryptor := nil
  else
  begin
    if not Assigned(TheCryptor) then
      TheCryptor := TZMZipEncryptor.Create(Self);
    FEncryptor := TZMZipEncryptor(TheCryptor);
    Result := 1;
  end;
end;

function TZMCompressor.WriteOut(const Buf: PByte; const Count: Integer)
  : Integer;
begin
  if Encrypted then
    Result := Encryptor.WriteOut(Buf, Count) // encode and write
  else
    Result := DoWriteOut(Buf, Count);
end;

{ TZMDecompressor }

function TZMDecompressor.Decompress: Integer;
begin
  if @InStream = @OutStream then
  begin
    Result := ZM_Error({_LINE_}617, ZE_InIsOutStream);
    Exit;
  end;
  if State = 0 then
  begin
    Result := FCoDec.Execute;
    Finish;
  end
  else
    Result := ZM_Error({_LINE_}626, ZE_LogicError);
end;

procedure TZMDecompressor.Finish;
begin
  if Assigned(Decryptor) then
    Decryptor.Clear;
  FDecryptor := nil;
  inherited;
end;

function TZMDecompressor.InitialCoDec(CompressionType: Integer): Integer;
begin
  Result := 0;
  FDecryptor := nil;
  if not(Assigned(FCoDec) and FCoDec.Supports(CompressionType)) then
  begin
    // need new CoDec
    case CompressionType of
      METHOD_STORED:
        SetCoDec(TZMCoDecStore.Create(Self));
      METHOD_DEFLATED:
        SetCoDec(TZMCoDecInflate.Create(Self));
    else
      Result := -ZE_Unsupported;
    end;
  end;
  if Result = 0 then
    Result := FCoDec.Initial(CompressionType);
end;

function TZMDecompressor.ReadIn(Buf: PByte; const Count: Integer): Integer;
begin
  if Encrypted then
    Result := Decryptor.ReadIn(Buf, Count) // readin and decode it
  else
    Result := DoReadIn(Buf, Count);
end;

function TZMDecompressor.SetCryptor(CryptType: TZMCryptTypes): Integer;
begin
  Result := 0;
  if CryptType = ZcNone then
    FDecryptor := nil
  else
  begin
    if not Assigned(TheCryptor) then
      TheCryptor := TZMZipDecryptor.Create(Self);
    FDecryptor := TZMZipDecryptor(TheCryptor);
    Result := 1;
  end;
end;

end.
