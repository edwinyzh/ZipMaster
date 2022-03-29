unit ZMMatch;

// ZMMatch.pas - Wild filename matching

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
// modified 2011-12-05

{$I   '.\ZipVers.inc'}

interface

uses
  SysUtils;

function FileNameMatch(const UPattern, USpec: string): Boolean;
function FileNameComp(const S1, S2: string): Integer;
function FileNamesSame(const FN1, FN2: string): Boolean;

implementation

uses
  Windows, {$IFNDEF UNICODE} ZMUTF8, ZMHandler,{$ENDIF} ZMStructs;

const
  __UNIT__ = 21;

type
{$IFDEF UNICODE}
  TZMWideString = string;
{$ELSE}
  TZMWideString = WideString;
{$ENDIF}

type
  TBounds = record
    Start: PWideChar;
    Finish: PWideChar;
  end;

  TParts = record
    Main: TBounds;
    Extn: TBounds;
    MainLen: Integer;
    ExtnLen: Integer;
  end;

  // return <0 _ match to *, 0 _ match to end, >0 _ no match
function Wild(var Bp, Bs: TBounds): Integer;
var
  Cp: Widechar;
  Cs: Widechar;
begin
  Result := -1; // matches so far
  // handle matching characters before wild
  while (Bs.Start <= Bs.Finish) and (Bp.Start <= Bp.Finish) do
  begin
    Cp := Bp.Start^;
    Cs := Bs.Start^;
    if Cp <> Cs then
    begin
      if Cp = '*' then
        Break; // matched to *
      // would match anything except path sep
      if (Cp <> '?') or (Cs = '\') then
      begin
        Result := 1; // no match
        Exit;
      end;
    end;
    // they match
    Inc(Bp.Start);
    Inc(Bs.Start);
  end;
  // we have * or eos
  if Bp.Start > Bp.Finish then
  begin
    if Bs.Start > Bs.Finish then
      Result := 0; // matched to end
  end;
  if Result < 0 then
  begin
    // handle matching characters from wild to end
    while Bs.Start <= Bs.Finish do
    begin
      Cp := Bp.Finish^;
      Cs := Bs.Finish^;
      if Cp <> Cs then
      begin
        if Cp = '*' then
          Break;
        // must not match path sep
        if (Cp <> '?') or (Cs = '\') then
        begin
          Result := 1; // no match
          Break;
        end;
      end;
      // they match
      Dec(Bp.Finish);
      Dec(Bs.Finish);
    end;
  end;
end;

function WildCmp(Bp, Bs: TBounds): Integer;
var
  Bpt: TBounds;
  Bst: TBounds;
  Sm: Integer;
  Pidx: PWideChar;
  Sidx: PWideChar;
begin
  // quick check for '*'
  if (Bp.Start = Bp.Finish) and (Bp.Start <> nil) and (Bp.Start^ = '*') then
  begin
    Result := 0; // matches any/none
    Exit;
  end;
  // no more Spec?
  if Bs.Finish < Bs.Start then
  begin
    if Bp.Finish < Bp.Start then
      Result := 0 // empty matches empty
    else
      Result := 3; // no match
    Exit;
  end;
  // handle matching characters before wild
  Result := Wild(Bp, Bs);
  if Result < 0 then
  begin
    Pidx := Bp.Start;
    Sidx := Bs.Start;
    if Bp.Start > Bp.Finish then
    begin
      if Sidx <= Bs.Finish then
        Result := 123
      else
        Result := 0;
      Exit;
    end;
    // handle wild
    if (Sidx <= Bs.Finish) and (Pidx^ = '*') then
    begin
      // skip multiple *
      while (Pidx < Bp.Finish) and ((Pidx + 1)^ = '*') and (Pidx^ = '*') do
        Inc(Pidx);
      // end of Pattern?
      if Pidx = Bp.Finish then
        Result := 0 // match
      else
      begin
        Inc(Pidx);
        Bpt.Start := Pidx;
        Bpt.Finish := Bp.Finish;
        Bst.Start := Sidx;
        Bst.Finish := Bs.Finish;
        while (Bst.Start <= Bst.Finish) do
        begin
          // recursively compare sub patterns
          Sm := WildCmp(Bpt, Bst);
          if Sm = 0 then
          begin
            Result := 0; // match
            Break;
          end;
          Inc(Bst.Start);
        end;
        if Result <> 0 then
          Result := 1; // no match
      end;
    end;
    // end of Spec - Pattern must only have *
    if Result < 0 then
    begin
      while (Pidx <= Bp.Finish) and (Pidx^ = '*') do
        Inc(Pidx);
      if Pidx > Bp.Finish then
        Result := 0; // matched
    end;
  end;
end;

// returned bit values
const
  MAIN = $01; // not empty
  MAIN_WILDALL = $02; // is *
  MAIN_HASWILD = $04;
  EXTN = $10;
  EXTN_WILDALL = $20;
  EXTN_HASWILD = $40;
  HAD_DOT = $08;

function Decompose(var Idx: PWideChar; var Parts: TParts): Integer;
var
  C: Widechar;
  ExtnFinish: PwideChar;
  ExtnStart: PwideChar;
  MainFinish: PwideChar;
  MainStart: PwideChar;
  Mwildall: Integer;
  Tmp: PWideChar;
  Xwildall: Integer;
begin
  Result := 0;
  Mwildall := 0;
  Xwildall := 0;
  Parts.ExtnLen := 0;
  ExtnStart := nil;
  ExtnFinish := nil;
  // at start of text or spec
  MainStart := Idx;
{$IFNDEF VERDXE464up}
  MainFinish := nil; // keep compiler happy
{$ENDIF}
  while True do
  begin
    C := Idx^;
    case C of
      '.':
        if Idx > MainStart then
        begin
          // we probably have extn
          if ExtnStart <> nil then
            Inc(Mwildall, Xwildall); // count all * in main
          ExtnStart := Idx + 1;
          Xwildall := 0;
        end;
      '\', '/', ':':
        begin
          if C = '/' then
            Idx^ := '\'; // normalise path seps
          if ExtnStart <> nil then
          begin
            // was false start of extn
            ExtnStart := nil;
            Inc(Mwildall, Xwildall); // count all * in main
            Xwildall := 0;
          end;
        end;
      ' ':
        begin
          // space can be embedded but cannot trail
          Tmp := Idx;
          Inc(Idx);
          while Idx^ = ' ' do
            Inc(Idx);
          if Idx^ < ' ' then
          begin
            // terminate
            MainFinish := Tmp - 1;
            Break;
          end;
          if Idx^ = SPEC_SEP then
          begin
            // terminate
            MainFinish := Tmp - 1;
            Inc(Idx);
            Break;
          end;
          Continue;
        end;
      #0 .. #31:
        begin
          // control terminates
          MainFinish := Idx - 1;
          Break;
        end;
      SPEC_SEP: // '|':
        begin
          // at the end
          MainFinish := Idx - 1;
          Inc(Idx);
          Break;
        end;
      '*':
        begin
          if ExtnStart <> nil then
            Inc(Xwildall)
          else
            Inc(Mwildall);
        end;
    end;
    Inc(Idx);
  end;
  // was there an extension?
  if ExtnStart <> nil then
  begin
    Result := Result or HAD_DOT;
    if ExtnStart <= MainFinish then
    begin
      // we have extn
      ExtnFinish := MainFinish;
      MainFinish := ExtnStart - 2;
      Parts.ExtnLen := 1 + (ExtnFinish - ExtnStart);
      Result := Result or EXTN;
      if Xwildall <> 0 then
      begin
        if Xwildall = Parts.ExtnLen then
          Result := Result or EXTN_WILDALL;
        Result := Result or EXTN_HASWILD;
      end;
    end
    else
    begin
      // dot but no extn
      ExtnStart := nil;
      Dec(MainFinish); // before dot
    end;
  end;

  Parts.MainLen := 1 + (MainFinish - MainStart);
  if Parts.MainLen > 0 then
  begin
    Result := Result or MAIN;
    if Mwildall <> 0 then
    begin
      if Mwildall = Parts.MainLen then
        Result := Result or MAIN_WILDALL;
      Result := Result or MAIN_HASWILD;
    end;
  end;
  // set resulting pointers
  Parts.Main.Start := MainStart;
  Parts.Main.Finish := MainFinish;
  Parts.Extn.Start := ExtnStart;
  Parts.Extn.Finish := ExtnFinish;
end;

// only gets called to compare same length names
function FileRCmp(var Bp, Bs: TBounds): Integer;
var
  Cp: Widechar;
  Cs: Widechar;
begin
  Result := 1; // no match
  if (Bs.Start > Bs.Finish) then
    Exit;
  if (Bp.Start^ <> Bs.Start^) and ((Bp.Start^ = '\') or (Bp.Start^ <> '?')) then
    Exit; // cannot match
  Inc(Bs.Start);
  Inc(Bp.Start);
  while (Bs.Start <= Bs.Finish) and (Bp.Start <= Bp.Finish) do
  begin
    Cp := Bp.Finish^;
    Cs := Bs.Finish^;
    Dec(Bp.Finish);
    Dec(Bs.Finish);
    if Cp <> Cs then
    begin
      // must not match path sep
      if (Cp <> '?') or (Cs = '\') then
        Exit; // no match
    end;
  end;
  Result := 0; // match
end;

procedure ToUpperCase(var Fspec: WideString);
{$IFNDEF UNICODE}
var
  Pw: PWideChar;
  Wc: WideChar;
{$ENDIF}
begin
{$IFDEF UNICODE}
  CharUpperW(PWideChar(Fspec));
{$ELSE}
  Pw := PWideChar(Fspec);
  if Win32MajorVersion > 4 then
    CharUpperW(Pw) // not implemented for earlier versions
  else
  begin
    Wc := Pw^;
    while Wc <> #0 do
    begin
      if (Wc <= 'z') and (Wc >= 'a') then
        Pw^ := WideChar(Ord(Wc) and $DF);
      Inc(Pw);
      Wc := Pw^;
    end;
  end;
{$ENDIF}
end;
(*
 function PUTF8ToWideStr(const raw: PAnsiChar; len: integer): TZMWideString;
 const
 MB_ERR_INVALID_CHARS = $00000008; // error for invalid chars
 var
 len: Integer;
 wcnt: Integer;
 flg: Cardinal;
 begin
 Result := '';
 len := Length(ustr);
 if len = 0 then
 exit;
 {$IFDEF UNICODE}
 flg := MB_ERR_INVALID_CHARS;
 {$ELSE}
 if Win32MajorVersion > 4 then
 flg := MB_ERR_INVALID_CHARS
 else
 flg := 0;
 {$ENDIF}
 SetLength(Result, len * 2); // plenty of room
 wcnt := MultiByteToWideChar(CP_UTF8, flg, PAnsiChar(ustr), -1,
 PWideChar(Result), len * 2);
 if wcnt = 0 then    // try again assuming Ansi
 wcnt := MultiByteToWideChar(0, flg, PAnsiChar(ustr), -1,
 PWideChar(Result), len * 2);
 if wcnt > 0 then
 dec(wcnt);  // don't want end null
 SetLength(Result, wcnt);
 end;
*)

function UpperFileNameMatch(const Pattern, Spec: TZMWideString): Boolean;
const
  FULL_WILD = MAIN_WILDALL or EXTN_WILDALL;
var
  Ch: WideChar;
  PFlag: Integer;
  Pidx: PWideChar;
  Ptn: TParts;
  SFlag: Integer;
  Sidx: PWideChar;
  Spc: TParts;
  Spc1: TParts;
  SpecStt: PWideChar;
  Xres: Integer;
begin
  Result := False;
  // check the spec if has extension
  SpecStt := PWideChar(Spec);
  Sidx := SpecStt;
  while Sidx^ <= ' ' do
  begin
    if Sidx^ = #0 then
      Exit;
    Inc(Sidx);
  end;
  SFlag := Decompose(Sidx, Spc);
  // now start processing each pattern
  Pidx := PWideChar(Pattern);
  repeat
    Ch := Pidx^;
    // skip garbage or separator
    while (Ch <= ' ') or (Ch = SPEC_SEP{'|'}) do
    begin
      if Ch = #0 then
        Exit;
      Inc(Pidx);
      Ch := Pidx^;
    end;
    PFlag := Decompose(Pidx, Ptn);
    // work out what we must test
    if ((PFlag and FULL_WILD) = FULL_WILD) or
      ((PFlag and (FULL_WILD or EXTN or HAD_DOT)) = MAIN_WILDALL) then
    begin
      Result := True;
      Break;
    end;
    if ((PFlag and (EXTN_HASWILD or EXTN)) = EXTN) and
      (Spc.ExtnLen <> Ptn.ExtnLen) then
      Continue; // cannot match
    if ((PFlag and MAIN_HASWILD) = 0) and (Spc.MainLen <> Ptn.MainLen) then
      Continue; // cannot match
    Xres := -1; // not tried to match
    // make copy of spc
    Move(Spc, Spc1, SizeOf(TParts));
    if (PFlag and EXTN_WILDALL) <> 0 then
      Xres := 0 // ignore extn as matched
    else
    begin
      // if pattern has extn, we must 'split' spec
      if (PFlag and HAD_DOT) <> 0 then
      begin
        // check special cases
        if (PFlag and EXTN) = 0 then
        begin
          // pattern ended in dot - spec must not have extn
          if (SFlag and EXTN) <> 0 then
            Continue; // spec has extn - cannot match
          Xres := 0; // no extn to check
        end
        else
        begin
          // spec must have extn
          if (SFlag and EXTN) = 0 then
            Continue; // no spec extn - cannot match
        end;
      end
      else
      begin
        // no Pattern dot _ test full spec
        if ((SFlag and EXTN) <> 0) then
          Spc1.Main.Finish := Spc.Extn.Finish; // full spec
        Xres := 0; // only test spec
      end;

      // test extn first (if required)
      if Xres < 0 then
        Xres := WildCmp(Ptn.Extn, Spc1.Extn);
    end;
    // if extn matched test main part
    if Xres = 0 then
    begin
      if (PFlag and MAIN_WILDALL) = 0 then
      begin
        if (PFlag and MAIN_HASWILD) <> 0 then
          Xres := WildCmp(Ptn.Main, Spc1.Main)
        else
          Xres := FileRCmp(Ptn.Main, Spc1.Main);
      end;
    end;
    // equate
    Result := Xres = 0;
    // at next pattern
  until Result;
end;

function FileNameMatch(const UPattern, USpec: string): Boolean;
var
  Pattern: TZMWideString;
  Spec: TZMWideString;
begin
  Result := False;
  if (UPattern = '') <> (USpec = '') then
    Exit;
{$IFDEF UNICODE}
  Pattern := AnsiUpperCase(UPattern);
  Spec := AnsiUpperCase(USpec);
{$ELSE}
  if UsingUtf8 then
  begin
    Pattern := PUTF8ToWideStr(PAnsiChar(UPattern), Length(UPattern));
    Spec := PUTF8ToWideStr(PAnsiChar(USpec), Length(USpec));
  end
  else
  begin
    Pattern := UPattern;
    Spec := USpec;
  end;
  ToUpperCase(Pattern);
  ToUpperCase(Spec);
{$ENDIF}
  Result := UpperFileNameMatch(Pattern, Spec);
end;

function UpperFileNameComp(const Ws1, Ws2: TZMWideString): Integer;
var
  Idx: Integer;
  Len: Integer;
  Len1: Integer;
  Len2: Integer;
  Wc1: WideChar;
  Wc2: WideChar;
begin
  Result := 0;
  Len1 := Length(Ws1);
  Len2 := Length(Ws2);
  Len := Len1;
  if Len2 < Len then
    Len1 := Len2;
  Idx := 1;
  // handle matching characters while they do
  while Idx <= Len do
  begin
    Wc1 := Ws1[Idx];
    if Wc1 = '/' then
      Wc1 := '\';
    Wc2 := Ws2[Idx];
    if Wc2 = '/' then
      Wc2 := '\';
    Result := Ord(Wc1) - Ord(Wc2);
    if Result <> 0 then
      Break;
    // they match
    Inc(Idx);
  end;
  if Result = 0 then
    Result := Len1 - Len2;
end;

function FileNameComp(const S1, S2: string): Integer;
var
  Ws1: TZMWideString;
  Ws2: TZMWideString;
begin
{$IFDEF UNICODE}
  Ws1 := AnsiUpperCase(S1);
  Ws2 := AnsiUpperCase(S2);
{$ELSE}
  if UsingUtf8 then
  begin
    Ws1 := PUTF8ToWideStr(PAnsiChar(S1), Length(S1));
    Ws2 := PUTF8ToWideStr(PAnsiChar(S2), Length(S2));
  end
  else
  begin
    Ws1 := S1;
    Ws2 := S2;
  end;
  ToUpperCase(Ws1);
  ToUpperCase(Ws2);
{$ENDIF}
  Result := UpperFileNameComp(Ws1, Ws2);
end;

function FileNamesSame(const FN1, FN2: string): Boolean;
var
  I: Integer;
  V1: Integer;
  V2: Integer;
begin
  Result := False;
  if Length(FN1) <> Length(FN2) then
    Exit;
  for I := Length(FN1) downto 1 do
    if FN1[I] <> FN2[I] then
    begin
      V1 := Ord(FN1[I]);
      if (V1 >= Ord('a')) and (V1 <= Ord('z')) then
        V1 := V1 - $20;
      V2 := Ord(FN2[I]);
      if (V2 >= Ord('a')) and (V2 <= Ord('z')) then
        V2 := V2 - $20;
      if V1 <> V2 then
      begin
        if (V1 = Ord('/')) and (V2 = Ord('\')) then
          Continue;
        if (V2 <> Ord('/')) or (V2 <> Ord('\')) then
          Exit;
      end;
    end;
  Result := True;
end;

end.
