unit ZMCoDec;

// ZMCoDec.pas - 'zip' Compress/decompress and crypt engine

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
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Windows, Classes, SysUtils,
{$ENDIF}
  {ZipMstr,} ZMEngine, ZMCRC;

const
  DEFLATE_MAX = 9;

const
  RAND_HEAD_LEN = 12; // Length of encryption random header.

type
  TZMCryptKeys = array [0 .. 2] of DWORD;
  PZMCryptKeys = ^TZMCryptKeys;
  TZMCryptHeader = packed array [0 .. RAND_HEAD_LEN - 1] of Byte;
  PZMCryptHeader = ^TZMCryptHeader;

type
  TZMZipCryptor = class
  private
    FCRC32Table: PZCRC32Table;
    FCryptHeader: TZMCryptHeader;
    FEngine: TZMEngine;
    FMyReference: Cardinal;
    FReady: Integer;
    Keys: TZMCryptKeys;
    function DecodeByte(C: Byte): Byte;
    function EncodeByte(C: Byte): Byte;
    procedure Init_keys(const Passwd: AnsiString);
    function MakeHeader(const PassPhrase: AnsiString; RefValue: DWORD): Integer;
    procedure Update_keys(C: Byte);
  protected
    function DecodeBuffer(Buf: PByte; Count: Longint): Integer;
    function EncodeBuffer(Buf: PByte; Count: Longint): Integer;
    function ReadHeader: Integer;
    function WriteHeader: Integer;
    property CRC32Table: PZCRC32Table read FCRC32Table;
    property Ready: Integer read FReady write FReady;
  public
    constructor Create(MyBase: TZMEngine);
    procedure AfterConstruction; override;
    procedure Clear;
    function DecodeInit(RefValue: Cardinal): Integer;
    function EncodeInit(const PassPhrase: AnsiString; RefValue: DWORD): Integer;
    function Supports(CryptType: TZMCryptTypes): Boolean;
    function Unlock(const PassPhrase: AnsiString): Integer;
  end;

type
  TZMZipDecryptor = class(TZMDecryptorBase)
  private
    FCryptor: TZMZipCryptor;
    FEngine: TZMEngine;
  protected
    function GetState: Integer; override;
  public
    constructor Create(MyBase: TZMEngine);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function DecodeInit(RefValue: Cardinal): Integer; override;
    function ReadIn(Buf: PByte; const Count: Integer): Integer; override;
    function Supports(CryptType: TZMCryptTypes): Boolean; override;
    function Unlock(const PassPhrase: AnsiString): Integer; override;
  end;

type
  TZMZipEncryptor = class(TZMEncryptorBase)
  private
    FCryptor: TZMZipCryptor;
    FEngine: TZMEngine;
  protected
    function GetState: Integer; override;
  public
    constructor Create(MyBase: TZMEngine);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function EncodeInit(const PassPhrase: AnsiString; RefValue: DWORD)
      : Integer; override;
    function Supports(CryptType: TZMCryptTypes): Boolean; override;
    function WriteOut(const Buf: PByte; const Count: Integer): Integer;
      override;
  end;

type
  TZMCoDecs = class(TZMCoDecBase)
  private
    FMyEngine: TZMEngine;
  protected
    property MyEngine: TZMEngine read FMyEngine;
  public
    constructor Create(MyBase: TZMEngine);
    function Initial(CompressionType: Integer): Integer; override;
  end;

type
  TZMCoDecStore = class(TZMCoDecs)
  public
    function Execute: Integer; override;
    function Supports(CompressionType: Integer): Boolean; override;
  end;

type
  TZMCoDecInflate = class(TZMCoDecs)
  public
    function Supports(CompressionType: Integer): Boolean; override;
    function Execute: Integer; override;
  end;

type
  TZMCoDecDeflate = class(TZMCoDecs)
  private
    FCompLevel: Integer;
  protected
    property CompLevel: Integer read FCompLevel write FCompLevel;
  public
    function Supports(CompressionType: Integer): Boolean; override;
    function Initial(CompressionType: Integer): Integer; override;
    function Execute: Integer; override;
  end;

implementation

uses
  ZMMsg, ZMStructs, ZMZLibExApi, ZMUtils;

const
  __UNIT__ = 5;

const
  CRYPT_BUFFER_SIZE = 8 * 1024;
  ZipMagic = 134775813;
  MinInterval = 8096;

{$IFDEF VERPre6}

type
  UInt64 = Int64;
{$ENDIF}

var
  XSSeed: UInt64 = 0;
  XSInitialed: Boolean = False;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

function ZLibToZMError(Code: Integer): Integer;
begin
  Result := 0;
  if Code < 0 then
  begin
    Result := (ZZ_ZLibFile - 1) + (-Code);
    if Result > ZZ_ZLibUnknown then
      Result := ZZ_ZLibUnknown;
  end;
end;

{ ** zlib deflate routines *********************************************************************** }

function ZDeflateInit2(var Stream: TZStreamRec; Level: Integer): Integer;
begin
  Result := deflateInit2(Stream, Level, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY);
end;

function ZDeflate(var Stream: TZStreamRec; Flush: Integer): Integer;
begin
  Result := deflate(Stream, Flush);
end;

function ZDeflateEnd(var Stream: TZStreamRec): Integer;
begin
  Result := deflateEnd(Stream);
end;

{ ** zlib inflate routines *********************************************************************** }
function ZInflateInit2(var Stream: TZStreamRec; WindowBits: Integer): Integer;
begin
  Result := inflateInit2_(Stream, WindowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZInflate(var Stream: TZStreamRec; Flush: Integer): Integer;
begin
  Result := inflate(Stream, Flush);
end;

function ZInflateEnd(var Stream: TZStreamRec): Integer;
begin
  Result := inflateEnd(Stream);
end;

// 2463534242;
function XSRandom: Cardinal;
// Marsaglia, George (July 2003). "Xorshift RNGs".
// Journal of Statistical Software Vol. 8 (Issue  14).
var
  Temp: UInt64;
begin
  Temp := XSSeed xor (XSSeed shl 13);
  Temp := Temp xor (Temp shr 17);
  Temp := Temp xor (Temp shl 5);
  XSSeed := Temp;
  Result := (Temp shr 1) and $FFFFFFFF;
end;

procedure InitXSSeed;
var
  Temp: FILETIME;
  SysTime: SYSTEMTIME;
begin
  GetSystemTime(SysTime);
  SystemTimeToFileTime(SysTime, Temp);
  XSSeed := (GetTickCount shr 1) xor UInt64(Temp);
  XSInitialed := True;
end;

{ TZMZipCryptor }

constructor TZMZipCryptor.Create(MyBase: TZMEngine);
begin
  inherited Create;
  FEngine := MyBase;
end;

procedure TZMZipCryptor.AfterConstruction;
begin
  inherited;
  FCRC32Table := PZCRC32Table(GetCRCTable);
  Clear;
  if not XSInitialed then
    InitXSSeed;
end;

procedure TZMZipCryptor.Clear;
begin
  FReady := 0; // not verified
end;

// returns >0 _ output count, <0 _ error
// decrypts inplace
function TZMZipCryptor.DecodeBuffer(Buf: PByte; Count: Longint): Integer;
var
  B: Byte;
  I: Integer;
  T: Byte;
  Temp: Cardinal;
begin
  for I := 0 to Count - 1 do
  begin
    // decode byte
    Temp := (Keys[2] and $FFFF) or 2;
    T := (((Temp * (Temp xor 1)) shr 8) and $FF);
    B := Byte(Buf^ xor T);
    Buf^ := B;
    // update keys
    Keys[0] := CRC32Table^[DWORD((Keys[0] xor B) and $0FF)
      ] xor DWORD((Keys[0] shr 8) and $0FFFFFF);
    Keys[1] := Keys[1] + (Keys[0] and $FF);
    Keys[1] := (Keys[1] * ZipMagic) + 1;
    Keys[2] := CRC32Table^[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
      xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
    Inc(Buf);
  end;
  Result := Count;
end;

function TZMZipCryptor.DecodeByte(C: Byte): Byte;
var
  Temp: Cardinal;
  T: Byte;
begin
  // decode byte
  Temp := (Keys[2] and $FFFF) or 2;
  T := (((Temp * (Temp xor 1)) shr 8) and $0FF);
  Result := Byte(C xor T);
  // update keys
  Keys[0] := CRC32Table^[DWORD((Keys[0] xor Result) and $0FF)
    ] xor DWORD((Keys[0] shr 8) and $0FFFFFF);
  Keys[1] := Keys[1] + (Keys[0] and $FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table^[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.DecodeInit(RefValue: Cardinal): Integer;
begin
  FMyReference := RefValue;
  if ReadHeader <> RAND_HEAD_LEN then
    FReady := -1; // bad

  Result := Ready;
end;

// returns >0 _ output count, <0 _ error
// encrypts inplace
function TZMZipCryptor.EncodeBuffer(Buf: PByte; Count: Longint): Integer;
var
  B: Byte;
  I: Integer;
  T: Byte;
  Temp: Cardinal;
begin
  for I := 0 to Count - 1 do
  begin
    B := Buf^;
    Temp := (Keys[2] and $FFFF) or 2;
    T := ((Temp * (Temp xor 1)) shr 8) and $FF;
    Buf^ := Byte(B xor T);
    // update keys
    Keys[0] := CRC32Table[DWORD((Keys[0] xor B) and $0FF)
      ] xor DWORD((Keys[0] shr 8) and $0FFFFFF);
    Keys[1] := Keys[1] + (Keys[0] and $FF);
    Keys[1] := (Keys[1] * ZipMagic) + 1;
    Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
      xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
    Inc(Buf);
  end;
  Result := Count;
end;

function TZMZipCryptor.EncodeByte(C: Byte): Byte;
var
  T: Byte;
  Temp: DWORD;
begin
  // decrypt byte
  Temp := (Keys[2] and $FFFF) or 2;
  T := (((Temp * (Temp xor 1)) shr 8) and $FF);
  Result := Byte(C xor T);
  // update keys
  Keys[0] := CRC32Table[DWORD((Keys[0] xor C) and $0FF)
    ] xor DWORD((Keys[0] shr 8) and $0FFFFFF);
  Keys[1] := Keys[1] + (Keys[0] and $FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.EncodeInit(const PassPhrase: AnsiString;
  RefValue: DWORD): Integer;
begin
  Result := MakeHeader(PassPhrase, RefValue);
  if Result < 0 then
    FReady := -1
  else
    FReady := 2;
end;

procedure TZMZipCryptor.Init_keys(const Passwd: AnsiString);
var
  I: Integer;
begin
  Keys[0] := 305419896;
  Keys[1] := 591751049;
  Keys[2] := 878082192;
  for I := 1 to Length(Passwd) do
    Update_keys(Ord(Passwd[I]));
end;

function TZMZipCryptor.MakeHeader(const PassPhrase: AnsiString;
  RefValue: DWORD): Integer;
var
  Header: array [0 .. RAND_HEAD_LEN - 3] of Byte;
  I: Integer;
begin
  // First generate RAND_HEAD_LEN - 2 random bytes.
  // We encrypt the output of rand() to get less predictability,
  // since rand() is often poorly implemented.
  Init_keys(PassPhrase);
  for I := 0 to RAND_HEAD_LEN - 3 do
  begin
    Header[I] := EncodeByte(XSRandom and $0FF);
  end;
  Init_keys(PassPhrase);
  // Encrypt random header (last two bytes is high word of crc)
  for I := 0 to RAND_HEAD_LEN - 3 do
    FCryptHeader[I] := EncodeByte(Header[I]);
  FCryptHeader[RAND_HEAD_LEN - 2] := EncodeByte((RefValue shr 16) and $FF);
  FCryptHeader[RAND_HEAD_LEN - 1] := EncodeByte((RefValue shr 24) and $FF);
  Result := RAND_HEAD_LEN;
end;

function TZMZipCryptor.ReadHeader: Integer;
begin
  Result := FEngine.DoReadIn(PByte(@FCryptHeader), RAND_HEAD_LEN);
end;

function TZMZipCryptor.Supports(CryptType: TZMCryptTypes): Boolean;
begin
  Result := CryptType = ZcZip;
end;

// Return <0 _ error, 0 _ match, >0 _ no match
function TZMZipCryptor.Unlock(const PassPhrase: AnsiString): Integer;
var
  Hh: array [0 .. RAND_HEAD_LEN - 1] of Byte; // decrypted header copy
  B: Word;
  N: Integer;
begin
  if FReady < 0 then
  begin
    Result := ZM_Error({_LINE_}461, ZE_LogicError); // not initialised
    Exit;
  end;
  if (FReady = 0) and (PassPhrase <> '') then
  begin
    // still locked
    // set DecryptKeys and save the encrypted header
    Init_keys(PassPhrase);
    Move(FCryptHeader, Hh, RAND_HEAD_LEN);

    // check password
    for N := 0 to RAND_HEAD_LEN - 1 do
      Hh[N] := Byte(DecodeByte(Hh[N]));
    B := Hh[RAND_HEAD_LEN - 1];

    if B = (FMyReference and $0FF) then
      FReady := 1; // unlocked
  end;
  if FReady = 1 then
    Result := 0 // matched
  else
    Result := 1; // failed
end;

procedure TZMZipCryptor.Update_keys(C: Byte);
begin
  Keys[0] := CRC32Table[(C xor Keys[0]) and $0FF] xor (Keys[0] shr 8);
  Keys[1] := Keys[1] + (Keys[0] and $0FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.WriteHeader: Integer;
begin
  Result := 0;
  if FReady = 2 then
  begin
    Result := FEngine.DoWriteOut(PByte(@FCryptHeader), RAND_HEAD_LEN);
    if Result <> RAND_HEAD_LEN then
      FReady := -1
    else
      FReady := 1;
  end
  else
    FReady := -1;
  if (Result >= 0) and (Result <> RAND_HEAD_LEN) then
    Result := ZM_Error({_LINE_}508, ZE_CryptError);
end;

function TZMCoDecStore.Execute: Integer;
var
  CRC: DWORD;
  InBuf: PByte;
  InBufSize: Integer;
begin
  FMyEngine.CRC := 0;
  CRC := 0;
  InBuf := FMyEngine.InBuffer.BufPtr;
  InBufSize := FMyEngine.InBuffer.Size;
  FMyEngine.ToBeWritten := FMyEngine.OutSize; // max to write
  FMyEngine.OutSize := 0; // nothing written yet

  repeat
    Result := FMyEngine.ReadIn(InBuf, InBufSize);
    if Result <= 0 then
      Break; // error

    CRC := ZCRC32(CRC, InBuf^, Result);

    Result := FMyEngine.WriteOut(InBuf, Result);
    if Result < 0 then
      Break;
  until (Result < 0);
  FMyEngine.CRC := CRC;
end;

function TZMCoDecStore.Supports(CompressionType: Integer): Boolean;
begin
  Result := CompressionType = METHOD_STORED;
end;

{ TZMCoDecInflate }

function TZMCoDecInflate.Execute: Integer;
var
  CRC: DWORD;
  Have: Integer;
  InBuf: PByte;
  InBufSize: Integer;
  OutBuf: PByte;
  OutBufSize: Integer;
  ZStream: TZStreamRec;
begin
  MyEngine.CRC := 0;
  CRC := 0;
  InBuf := MyEngine.InBuffer.BufPtr;
  InBufSize := MyEngine.InBuffer.Size;
  OutBuf := MyEngine.OutBuffer.BufPtr;
  OutBufSize := MyEngine.OutBuffer.Size;
  MyEngine.ToBeWritten := MyEngine.OutSize; // max to write
  MyEngine.OutSize := 0; // nothing written yet

  // allocate deflate state
  ZStream.Zalloc := nil;
  ZStream.Zfree := nil;
  ZStream.Opaque := nil;
  ZStream.Avail_in := 0;
  ZStream.Next_in := nil;

  Result := ZInflateInit2(ZStream, -15);
  if (Result < 0) and (Result <> Z_BUF_ERROR) then
  begin
    Result := ZM_Error({_LINE_}574, ZLibToZMError(Result));
    Exit;
  end;
  try
    // Inflate until deflate stream ends or end of file
    repeat
      Have := MyEngine.ReadIn(InBuf, InBufSize);
      if Have <= 0 then
      begin
        Result := Have;
        Break;
      end;

      ZStream.Avail_in := Have;
      ZStream.Next_in := InBuf;
      // run inflate() on input until output buffer not full
      repeat
        ZStream.Avail_out := OutBufSize;
        ZStream.Next_out := OutBuf;
        Result := ZInflate(ZStream, Z_NO_FLUSH);
        if (Result < 0) and (Result <> Z_BUF_ERROR) then
        begin
          Result := ZM_Error({_LINE_}596, ZLibToZMError(Result));
          Break;
        end;
        Result := 0;
        Have := OutBufSize - Integer(ZStream.Avail_out);
        if Have > 0 then
        begin
          CRC := ZCRC32(CRC, OutBuf^, Have);
          Result := MyEngine.WriteOut(OutBuf, Have);
          if Result < 0 then
            Break;
        end;
      until (ZStream.Avail_out <> 0);

      // done when inflate() says it's done or error
    until (Result < 0) or (Result = Z_STREAM_END);
  finally
    // clean up
    ZInflateEnd(ZStream);
    MyEngine.CRC := CRC;
  end;
end;

function TZMCoDecInflate.Supports(CompressionType: Integer): Boolean;
begin
  Result := CompressionType = METHOD_DEFLATED;
end;

{ TZMCoDecDeflate }

function TZMCoDecDeflate.Execute: Integer;
var
  CRC: DWORD;
  Flush: Integer;
  Have: Integer;
  InBuf: PByte;
  InBufSize: Integer;
  OutBuf: PByte;
  OutBufSize: Integer;
  ZStream: TZStreamRec;
begin
  MyEngine.CRC := 0;
  CRC := 0;
  InBuf := MyEngine.InBuffer.BufPtr;
  InBufSize := MyEngine.InBuffer.Size;
  OutBuf := MyEngine.OutBuffer.BufPtr;
  OutBufSize := MyEngine.OutBuffer.Size;

  MyEngine.ToBeWritten := MyEngine.OutSize; // max to write
  MyEngine.OutSize := 0; // nothing written yet

  // allocate deflate state
  ZStream.Zalloc := nil;
  ZStream.Zfree := nil;
  ZStream.Opaque := nil;

  Result := ZDeflateInit2(ZStream, MyEngine.CompLevel);
  if Result < 0 then
  begin
    Result := ZM_Error({_LINE_}655, ZLibToZMError(Result));
    Exit;
  end;
  try
    Result := 0;
    Flush := Z_NO_FLUSH;
    while (Flush <> Z_FINISH) and (Result = 0) do
    begin
      Result := MyEngine.ReadIn(InBuf, InBufSize);
      if Result < 0 then
        Break; // error

      if Result < InBufSize{1} then
        Flush := Z_FINISH // end of input
      else
        Flush := Z_NO_FLUSH;
      if Result > 0 then
        CRC := ZCRC32(CRC, InBuf^, Result);

      ZStream.Avail_in := Result;
      ZStream.Next_in := InBuf;
      { run deflate() on input until output buffer not full, finish
        compression if all of source has been read in }
      repeat
        ZStream.Avail_out := OutBufSize;
        ZStream.Next_out := OutBuf;
        Result := ZDeflate(ZStream, Flush);
        if Result < 0 then
        begin
          Result := ZM_Error({_LINE_}684, ZLibToZMError(Result));
          Break;
        end;
        Have := OutBufSize - Integer(ZStream.Avail_out);
        if Have > 0 then
        begin
          Result := MyEngine.WriteOut(OutBuf, Have);
          if Result < 0 then
            Break;
        end;
      until ZStream.Avail_out <> 0;
    end;
  finally
    ZDeflateEnd(ZStream);
    MyEngine.CRC := CRC;
  end;
  if Result > 0 then
    Result := 0;
end;

function TZMCoDecDeflate.Initial(CompressionType: Integer): Integer;
begin
  Result := 0;
  if Supports(CompressionType) then
    FCompLevel := CompressionType
  else
    Result := ZM_Error(735, ZE_LogicError);
end;

function TZMCoDecDeflate.Supports(CompressionType: Integer): Boolean;
begin
  Result := (CompressionType > 0) and (CompressionType <= DEFLATE_MAX);
end;

constructor TZMCoDecs.Create(MyBase: TZMEngine);
begin
  inherited Create;
  FMyEngine := MyBase;
end;

function TZMCoDecs.Initial(CompressionType: Integer): Integer;
begin
  if Supports(CompressionType) then
    Result := 0
  else
    Result := ZM_Error(648, ZE_LogicError);
end;

constructor TZMZipDecryptor.Create(MyBase: TZMEngine);
begin
  inherited Create;
  FEngine := MyBase;
end;

{ TZMZipDecryptor }

procedure TZMZipDecryptor.AfterConstruction;
begin
  inherited;
  FCryptor := TZMZipCryptor.Create(FEngine);
end;

procedure TZMZipDecryptor.BeforeDestruction;
begin
  FCryptor.Free;
  inherited;
end;

procedure TZMZipDecryptor.Clear;
begin
  FCryptor.Clear;
end;

function TZMZipDecryptor.DecodeInit(RefValue: Cardinal): Integer;
begin
  Result := FCryptor.DecodeInit(RefValue);
end;

function TZMZipDecryptor.GetState: Integer;
begin
  Result := FCryptor.Ready;
end;

function TZMZipDecryptor.ReadIn(Buf: PByte; const Count: Integer): Integer;
begin
  Result := FEngine.DoReadIn(Buf, Count);
  if Result > 0 then
    Result := FCryptor.DecodeBuffer(Buf, Result);
end;

function TZMZipDecryptor.Supports(CryptType: TZMCryptTypes): Boolean;
begin
  Result := FCryptor.Supports(CryptType);
end;

function TZMZipDecryptor.Unlock(const PassPhrase: AnsiString): Integer;
begin
  Result := FCryptor.Unlock(PassPhrase);
end;

{ TZMZipEncryptor }

procedure TZMZipEncryptor.AfterConstruction;
begin
  inherited;
  FCryptor := TZMZipCryptor.Create(FEngine);
end;

procedure TZMZipEncryptor.BeforeDestruction;
begin
  FCryptor.Free;
  inherited;
end;

procedure TZMZipEncryptor.Clear;
begin
  FCryptor.Clear;
end;

constructor TZMZipEncryptor.Create(MyBase: TZMEngine);
begin
  inherited Create;
  FEngine := MyBase;
end;

function TZMZipEncryptor.EncodeInit(const PassPhrase: AnsiString;
  RefValue: DWORD): Integer;
begin
  Result := FCryptor.EncodeInit(PassPhrase, RefValue);
end;

function TZMZipEncryptor.GetState: Integer;
begin
  Result := FCryptor.Ready;
end;

function TZMZipEncryptor.Supports(CryptType: TZMCryptTypes): Boolean;
begin
  Result := FCryptor.Supports(CryptType);
end;

function TZMZipEncryptor.WriteOut(const Buf: PByte;
  const Count: Integer): Integer;
begin
  Result := FCryptor.EncodeBuffer(Buf, Count);
  if Result >= 0 then
    Result := FEngine.DoWriteOut(Buf, Count);
end;

end.
