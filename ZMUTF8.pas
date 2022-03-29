unit ZMUTF8;
                      
// ZMUTF8.pas - Some UTF8/16 utility functions

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
// modified 2014-09-06

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZipMstr;

// convert to/from UTF8 characters
function UTF8ToStr(const Astr: UTF8String): string;
function StrToUTF8(const Ustr: string): UTF8String;
function StrToUTFEx(const Astr: AnsiString; Cp: Cardinal = 0;
  Len: Integer = -1): string;
function UTF8ToWide(const Astr: UTF8String; Len: Integer = -1): TZMWideString;
function PWideToUTF8(const Pwstr: PWideChar; Len: Integer = -1): UTF8String;
function WideToUTF8(const Astr: TZMWideString; Len: Integer = -1): UTF8String;

// test for valid UTF8 character(s)  > 0 _ some, 0 _ none, < 0 _ invalid
function ValidUTF8(Pstr: PAnsiChar; Len: Integer): Integer; overload;
function ValidUTF8(const Str: AnsiString; Len: Integer = -1): Integer; overload;
{$IFDEF UNICODE}
function ValidUTF8(const Str: UTF8String; Len: Integer = -1): Integer; overload;
{$ENDIF}
// convert to UTF8 (if needed)
function AsUTF8Str(const Zstr: string): UTF8String;

function IsUTF8Trail(C: AnsiChar): Boolean;

function PUTF8ToStr(const Raw: PAnsiChar; Len: Integer): string;
function PUTF8ToWideStr(const Raw: PAnsiChar; Len: Integer): TZMWideString;

function StrToWideEx(const Astr: AnsiString; Cp: Cardinal; Len: Integer)
  : TZMWideString;

// convert to Ansi/OEM escaping unsupported characters
function PWideToSafe(const PWstr: PWideChar; ToOEM: Boolean): AnsiString;

// test all characters are supported
function WideIsSafe(const Wstr: TZMWideString; ToOEM: Boolean): Boolean;

// convert to Ansi/OEM escaping unsupported characters
function WideToSafe(const Wstr: TZMWideString; ToOEM: Boolean): AnsiString;

{$IFNDEF UNICODE}
function UTF8ToSafe(const Ustr: AnsiString; ToOEM: Boolean): AnsiString;
function PUTF8ToSafe(const Raw: PAnsiChar; Len: Integer): AnsiString;
function UTF8IsSafe(const Ustr: AnsiString; ToOEM: Boolean): Boolean;
// only applicable when converting to OEM
function AnsiIsSafe(const Ustr: AnsiString; ToOEM: Boolean): Boolean;

{$ENDIF}

// -------------------------- ------------ -------------------------
implementation

const
  __UNIT__ = 38;

function PUTF8ToWideStr(const Raw: PAnsiChar; Len: Integer): TZMWideString;
const
  MB_ERR_INVALID_CHARS = $00000008; // error for invalid chars
var
  Wcnt: Integer;
  Flg: Cardinal;
  P: PAnsiChar;
  Rlen: Integer;
begin
  Result := '';
  if (Raw = nil) or (Len = 0) then
    Exit;
  Rlen := Len;
  if Len < 0 then
  begin
    Len := -1;
    P := Raw;
    Rlen := 0;
    while P^ <> #0 do
    begin
      Inc(P);
      Inc(Rlen);
    end;
  end;
  Rlen := Rlen * 2;
{$IFDEF UNICODE}
  Flg := MB_ERR_INVALID_CHARS;
{$ELSE}
  if Win32MajorVersion > 4 then
    Flg := MB_ERR_INVALID_CHARS
  else
    Flg := 0;
{$ENDIF}
  SetLength(Result, Rlen); // plenty of room
  Wcnt := MultiByteToWideChar(CP_UTF8, Flg, Raw, Len, PWideChar(Result), Rlen);
  if Wcnt = 0 then // try again assuming Ansi
    Wcnt := MultiByteToWideChar(0, Flg, Raw, Len, PWideChar(Result), Rlen);
  if (Wcnt > 0) and (Len = -1) then
    Dec(Wcnt); // don't want end null
  SetLength(Result, Wcnt);
end;

function PUTF8ToStr(const Raw: PAnsiChar; Len: Integer): string;
begin
  Result := PUTF8ToWideStr(Raw, Len);
end;

function PWideToUTF8(const Pwstr: PWideChar; Len: Integer = -1): UTF8String;
var
  Cnt: Integer;
begin
  Result := '';
  if Len < 0 then
    Len := -1;
  if Len = 0 then
    Exit;
  Cnt := WideCharToMultiByte(CP_UTF8, 0, Pwstr, Len, nil, 0, nil, nil);
  if Cnt > 0 then
  begin
    SetLength(Result, Cnt);
    Cnt := WideCharToMultiByte(CP_UTF8, 0, Pwstr, Len, PAnsiChar(Result), Cnt,
      nil, nil);
    if Cnt < 1 then
      Result := ''; // oops - something went wrong
    if Len = -1 then
      SetLength(Result, Cnt - 1); // remove end nul
  end // ;
  else
    RaiseLastOSError;
end;

function WideToUTF8(const Astr: TZMWideString; Len: Integer = -1): UTF8String;
begin
  if Len < 0 then
    Len := Length(Astr);
  Result := PWideToUTF8(@Astr[1], Len);
end;

function UTF8ToWide(const Astr: UTF8String; Len: Integer = -1): TZMWideString;
begin
  Result := '';
  if Len < 0 then
    Len := Length(Astr);
  Result := PUTF8ToWideStr(PAnsiChar(Astr), Len);
end;

function UTF8ToStr(const Astr: UTF8String): string;
begin
  Result := PUTF8ToStr(PAnsiChar(Astr), Length(Astr));
end;

function StrToUTF8(const Ustr: string): UTF8String;
var
  Wtemp: TZMWideString;
begin
  Wtemp := Ustr;
  Result := WideToUTF8(Wtemp, -1);
end;

function StrToUTFEx(const Astr: AnsiString; Cp: Cardinal = 0;
  Len: Integer = -1): string;
var
  Ws: TZMWideString;
begin
  Ws := StrToWideEx(Astr, Cp, Len);
{$IFDEF UNICODE}
  Result := Ws;
{$ELSE}
  Result := StrToUTF8(Ws);
{$ENDIF}
end;

function IsUTF8Trail(C: AnsiChar): Boolean;
begin
  Result := (Ord(C) and $C0) = $80;
end;

function UTF8SeqLen(C: AnsiChar): Integer;
var
  U8: Cardinal;
begin
  Result := 1;
  U8 := Ord(C);
  if U8 >= $80 then
  begin
    if (U8 and $FE) = $FC then
      Result := 6
    else
      if (U8 and $FC) = $F8 then
        Result := 5
      else
        if (U8 and $F8) = $F0 then
          Result := 4
        else
          if (U8 and $F0) = $E0 then
            Result := 3
          else
            if (U8 and $E0) = $C0 then
              Result := 2
            else
              Result := -1; // trailing byte - invalid
  end;
end;

// test for valid UTF8 character(s)  > 0 _ some, 0 _ none, < 0 _ invalid
function ValidUTF8(const Str: AnsiString; Len: Integer): Integer;
var
  I, J, Ul: Integer;
begin
  if Len < 0 then
    Len := Length(Str);
  Result := 0;
  I := 1;
  while (I <= Len) do
  begin
    Ul := UTF8SeqLen(Str[I]);
    Inc(I);
    if Ul <> 1 then
    begin
      if (Ul < 1) or ((I + Ul - 2) > Len) then
      begin
        Result := -1; // invalid
        Break;
      end;
      // first in seq
      for J := 0 to Ul - 2 do
      begin
        if (Ord(Str[I]) and $C0) <> $80 then
        begin
          Result := -1;
          Break;
        end;
        Inc(I);
      end;
      if Result >= 0 then
        Inc(Result) // was valid so count it
      else
        Break;
    end;
  end;
end;

// test for valid UTF8 character(s)  > 0 _ some, 0 _ none, < 0 _ invalid
function ValidUTF8(Pstr: PAnsiChar; Len: Integer): Integer;
var
  J, Ul: Integer;
begin
  Result := 0;
  // i := 1;
  while (Len > 0) do
  begin
    Ul := UTF8SeqLen(Pstr^);
    Inc(Pstr);
    Dec(Len);
    if Ul <> 1 then
    begin
      if (Ul < 1) or ((Ul - 1) > Len) then
      begin
        Result := -1; // invalid
        Break;
      end;
      // first in seq
      for J := 0 to Ul - 2 do
      begin
        if (Ord(Pstr^) and $C0) <> $80 then
        begin
          Result := -1;
          Break;
        end;
        Inc(Pstr);
        Dec(Len);
      end;
      if Result >= 0 then
        Inc(Result) // was valid so count it
      else
        Break;
    end;
  end;
end;

{$IFDEF UNICODE}
function ValidUTF8(const Str: UTF8String; Len: Integer = -1): Integer;
var
  I, J, Ul: Integer;
begin
  if Len < 0 then
    Len := Length(Str);
  Result := 0;
  I := 1;
  while (I <= Len) do
  begin
    Ul := UTF8SeqLen(Str[I]);
    Inc(I);
    if Ul <> 1 then
    begin
      if (Ul < 1) or ((I + Ul - 2) > Len) then
      begin
        Result := -1; // invalid
        Break;
      end;
      // first in seq
      for J := 0 to Ul - 2 do
      begin
        if (Ord(Str[I]) and $C0) <> $80 then
        begin
          Result := -1;
          Break;
        end;
        Inc(I);
      end;
      if Result >= 0 then
        Inc(Result) // was valid so count it
      else
        Break;
    end;
  end;
end;
{$ENDIF}

function AsUTF8Str(const Zstr: string): UTF8String;
begin
{$IFDEF UNICODE}
  Result := UTF8String(Zstr);
{$ELSE}
  if ValidUTF8(Zstr, -1) < 0 then
    Result := StrToUTF8(Zstr)
  else
    Result := Zstr;
{$ENDIF}
end;

function StrToWideEx(const Astr: AnsiString; Cp: Cardinal; Len: Integer)
  : TZMWideString;
var
  Cnt: Integer;
  S: AnsiString;
  Wcnt: Integer;
begin
  Result := '';
  if Len < 0 then
    Len := Length(Astr);
  if Len = 0 then
    Exit;
  Wcnt := MultiByteToWideChar(Cp, 0, PAnsiChar(Astr), Len, nil, 0);
  if Wcnt > 0 then
  begin
    SetLength(Result, Wcnt);
    Cnt := MultiByteToWideChar(Cp, 0, PAnsiChar(Astr), Len,
      PWideChar(Result), Wcnt);
    if Cnt < 1 then
      Result := ''; // oops - something went wrong
  end
  else
  begin
    S := Astr; // assume it is Ansi
    if (Len > 0) and (Len < Length(Astr)) then
      SetLength(S, Len);
    Result := string(S);
  end;
end;

// convert to MultiByte escaping unsupported characters
function PWideToSafe(const PWstr: PWideChar; ToOEM: Boolean): AnsiString;
{$IFNDEF VERDXE2up}
const
  WC_NO_BEST_FIT_CHARS = $00000400;
{$ENDIF}
var
  Bad: Bool;
  C: AnsiChar;
  Cnt: Integer;
//  I: Integer;
  Pa: PAnsiChar;
  Pw: PWideChar;
  Tmp: AnsiString;
  Subst: array [0 .. 1] of AnsiChar;
  ToCP: Cardinal;
  Wc: WideChar;
//  Wlen: Integer;
begin
  Result := '';
  if (PWstr <> nil) and (PWstr^ <> #0) then
  begin
    if ToOEM then
      ToCP := CP_OEMCP
    else
      ToCP := CP_ACP;
    Subst[0] := #$1B; // substitute char - escape
    Subst[1] := #0;
    Cnt := WideCharToMultiByte(ToCP, WC_NO_BEST_FIT_CHARS, PWstr,
      -1, nil, 0, PAnsiChar(@Subst), @Bad);
    if Cnt > 0 then
    begin
      SetLength(Result, Cnt);
      Cnt := WideCharToMultiByte(ToCP, WC_NO_BEST_FIT_CHARS, PWstr,
        -1, PAnsiChar(Result), Cnt, PAnsiChar(@Subst), @Bad);
      if Cnt < 1 then
        Result := ''//; // oops - something went wrong
      else
        SetLength(Result, Cnt - 1); // remove end nul
    end;
    if Bad then
    begin
      Tmp := Result;
      Result := '';
      Pa := PAnsiChar(Tmp);
      Pw := PWstr;
//      I := 1;
//      Wc := #0;
//      Wlen := Length(Wstr);
      while (Pa^ <> #0) do
      begin
        C := Pa^;
        Wc := Pw^;
        if Wc <> #0 then
          Inc(Pw);
//        if I < Wlen then
//        begin
//          Wc := PWstr^[I];
//          Inc(I);
//        end;
        if C = #$1B then
          Result := Result + '#$' + AnsiString(IntToHex(Ord(Wc), 4))
        else
          Result := Result + C;
        Pa := CharNextExA(ToCP, Pa, 0);
      end;
    end;
  end;
end;

{$IFNDEF UNICODE}
function UTF8ToSafe(const Ustr: AnsiString; ToOEM: Boolean): AnsiString;
begin
  Result := WideToSafe(UTF8ToWide(Ustr), ToOEM);
end;
{$ENDIF}

{$IFNDEF UNICODE}
function PUTF8ToSafe(const Raw: PAnsiChar; Len: Integer): AnsiString;
begin
  Result := WideToSafe(PUTF8ToWideStr(Raw, Len), False);
end;
{$ENDIF}

// test all characters are supported
function WideIsSafe(const Wstr: TZMWideString; ToOEM: Boolean): Boolean;
var
  Bad: Bool;
  Cnt: Integer;
  ToCP: Cardinal;
begin
  Result := True;
  if ToOEM then
    ToCP := CP_OEMCP
  else
    ToCP := CP_ACP;
  if Wstr <> '' then
  begin
    Cnt := WideCharToMultiByte(ToCP, 0, PWideChar(Wstr), Length(Wstr), nil, 0,
      nil, @Bad);
    Result := (not Bad) and (Cnt > 1);//0);
  end;
end;

{$IFNDEF UNICODE}
function UTF8IsSafe(const Ustr: AnsiString; ToOEM: Boolean): Boolean;
begin
  Result := WideIsSafe(UTF8ToWide(Ustr), ToOEM);
end;
{$ENDIF}


// convert to MultiByte escaping unsupported characters
function WideToSafe(const Wstr: TZMWideString; ToOEM: Boolean): AnsiString;
begin
  Result := PWideToSafe(PWideChar(Wstr), ToOEM);
end;

{$IFNDEF UNICODE}
// only applicable when converting to OEM
function AnsiIsSafe(const Ustr: AnsiString; ToOEM: Boolean): Boolean;
begin
  Result := True;
  if ToOEM then
    Result := WideIsSafe(WideString(Ustr), ToOEM);
end;
{$ENDIF}

end.
