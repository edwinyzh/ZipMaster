unit ZMArgSplit;

// ZMArgSplit.pas - Split command strings

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
// modified 2014-01-4

{$I   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes;
{$ELSE}
  Classes;
{$ENDIF}

// const
// ZPASSWORDARG: Char = '<';
// ZSwitch: Char      = '/';
// ZSpecArg: Char     = '>';
// MAX_ARG_LENGTH     = 2048;
// ZFILE_SEPARATOR    = '>>';

type
  TZASErrors = (ZasNone, ZasIgnored, ZasInvalid, ZasDuplicate, ZasUnknown,
    ZasDisallowed, ZasBad);
  TZArgOpts = (ZaoLastSpec, ZaoWildSpec, ZaoMultiSpec);
  TZArgOptions = set of TZArgOpts;

type
  TZMArgSplitter = class
  private
    FAllow: string;
    FArgs: TStringList;
    FError: TZASErrors;
    FErrorMsg: string;
    FFound: string;
    FMain: string;
    FOptions: TZArgOptions;
    FRaw: string;
    procedure AnotherArg(Option: Char; const Arg: string);
    function CheckCompression(var Arg: string): TZASErrors;
    function CheckDate(var Arg: string): TZASErrors;
    function CheckExcludes(var Arg: string): TZASErrors;
    function CheckFolder(var Arg: string): TZASErrors;
    function CheckInclude(var Arg: string): TZASErrors;
    function CheckOverWrite(var Arg: string): TZASErrors;
    function CheckXlatePath(var Arg: string): TZASErrors;
    procedure SetAllow(const Value: string);
    procedure SetRaw(const Value: string);
  protected
    function GetArg(var Idx: Integer; AllowPW: Boolean): string;
    function HandleMain(S: string): Boolean;
    function HandleSwitch(TheSwitch: string): Boolean;
    function Index(Option: Char): Integer;
    procedure SplitRaw;
    function UpperOp(Option: Char): Char;
  public
    procedure AfterConstruction; override;
    function Arg(Option: Char): string; overload;
    function Arg(Index: Integer): string; overload;
    procedure BeforeDestruction; override;
    procedure Clear;
    function Has(Option: Char): Boolean;
    property Allow: string read FAllow write SetAllow;
    property Error: TZASErrors read FError write FError;
    property ErrorMsg: string read FErrorMsg;
    property Found: string read FFound;
    property Main: string read FMain;
    property Options: TZArgOptions read FOptions write FOptions;
    property Raw: string read FRaw write SetRaw;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, WinApi.Windows,
{$ELSE}
  SysUtils, Windows,
{$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMUtils, ZMStructs;

const
  __UNIT__ = 2;

const
  KnownArgs = ['>', '<', 'C', 'D', 'E', 'F', 'J', 'N', 'O', 'S', 'T', 'X'];
  DupAllowed = [];
  {
   switches
   >>spec                  select spec
   <password               use password (must be last)
   C:n                     set compression to n
   D:"[< or >]date"        [< _ before or > _ after (default)] date
   D:"[< or >]-days"       [< _ before or > _ after (default)] days ago
   E:[|][spec[|spec...]]   exclude spec if starts | added to global
   F:folder                set folder
   J[+ or -]               junk folders
   N[+ or -]               no rename with AddNewName (default +)
   O:[A or N or O or Y]     overwrite always, never, older, younger
   S[+ or -]               sub-directories (default+)
   T[+ or -]               NTFS times (default+)
   X:[old]::[new]          translate old to new
   '::'
   ':: this line is a rem'
   'something ::'
   'something :: this is a rem'
  }
  MAX_ARG_LENGTH = 2048;

procedure TZMArgSplitter.AfterConstruction;
begin
  inherited;
  FArgs := TStringList.Create;
end;

procedure TZMArgSplitter.AnotherArg(Option: Char; const Arg: string);
begin
  FFound := FFound + Option;
  FArgs.Add(Arg);
end;

function TZMArgSplitter.Arg(Option: Char): string;
var
  Idx: Integer;
begin
  Result := '';
  Idx := Index(Option);
  if Idx < 0 then
    Exit; // not valid - how was it found?
  Result := FArgs[Idx];
end;

function TZMArgSplitter.Arg(Index: Integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < FArgs.Count) then
    Result := FArgs[Index];
end;

procedure TZMArgSplitter.BeforeDestruction;
begin
  FArgs.Free;
  inherited;
end;

// must be 0..9
function TZMArgSplitter.CheckCompression(var Arg: string): TZASErrors;
begin
  if (Length(Arg) = 1) and (Arg[1] >= '0') and (Arg[1] <= '9') then
    Result := ZasNone
  else
    Result := ZasBad;
end;

function TZMArgSplitter.CheckDate(var Arg: string): TZASErrors;
var
  Ch: Char;
  Darg: string;
  Days: Integer;
  DosDate: Cardinal;
  Dt: TDateTime;
  Narg: string;
  Tmp: string;
begin
  Result := ZasBad;
  Darg := Arg;
  Narg := '>'; // default 'after'
  if Arg <> '' then
  begin
    Ch := Arg[1];
    if CharInSet(Ch, ['<', '>']) then
    begin
      Narg := Ch;
      Darg := Copy(Arg, 2, Length(Arg) - 1);
    end;
    if (Length(Darg) > 2) and (Darg[1] = '-') then
    begin
      // need number
      Tmp := Copy(Darg, 2, 6);
      if TryStrToInt(Tmp, Days) then
      begin
        Dt := Date - Days;
        Result := ZasNone;
        DosDate := DateTimeToFileDate(Dt);
        Arg := Narg + '$' + IntToHex(DosDate, 8);
      end;
    end
    else
      if TryStrToDateTime(Darg, Dt) then
      begin
        Result := ZasNone;
        DosDate := DateTimeToFileDate(Dt);
        Arg := Narg + '$' + IntToHex(DosDate, 8);
      end;
  end;
end;

// allowed empty
function TZMArgSplitter.CheckExcludes(var Arg: string): TZASErrors;
begin
  Result := ZasNone;
  Arg := WinPathDelimiters(Arg);
end;

// empty returns current dir
function TZMArgSplitter.CheckFolder(var Arg: string): TZASErrors;
var
  Cleaned: string;
  Ret: Integer;
  Tmp: string;
begin
  Result := ZasNone;
  if (Arg = '') or (Arg = '.') then
    Tmp := GetCurrentDir
  else
  begin
    Tmp := WinPathDelimiters(Arg);
    Ret := CleanPath(Cleaned, Tmp, False);
    if Ret <> 0 then
      Result := ZasBad
    else
      Arg := Cleaned;
  end;
end;

// must have file spec
function TZMArgSplitter.CheckInclude(var Arg: string): TZASErrors;
var
  Cleaned: string;
  First: string;
  Rest: string;
  Ret: Integer;
  Tmp: string;
begin
  Result := ZasNone;
  Tmp := WinPathDelimiters(Arg);
  First := Tmp;
  if (ZaoMultiSpec in Options) and (Pos('|', First) > 0) then
  begin
    while First <> '' do
    begin
      First := ZSplitString('|', First, Rest);
      Ret := CleanPath(Cleaned, First, False);
      if (Ret <> 0) and not((ZaoWildSpec in Options) and (Ret = Z_WILD)) then
      begin
        Result := ZasBad;
        Exit;
      end;
      First := Rest;
    end;
    Arg := Tmp;
  end
  else
  begin
    Ret := CleanPath(Cleaned, Tmp, False);
    if (Ret <> 0) and not((ZaoWildSpec in Options) and (Ret = Z_WILD)) then
      Result := ZasBad
    else
      Arg := Cleaned;
  end;
end;

function TZMArgSplitter.CheckOverWrite(var Arg: string): TZASErrors;
var
  C: Char;
begin
  Result := ZasInvalid;
  if Length(Arg) < 1 then
    C := 'A'
  else
  begin
    C := Arg[1];
    C := UpperOp(C);
  end;
  if C = '+' then
    C := 'A'; // default Always
  if CharInSet(C, ['N', 'A', 'Y', 'O']) then
  begin
    Arg := C;
    Result := ZasNone;
  end;
end;

// format :[orig_path]::[new_path]
// paths must not include drive
function TZMArgSplitter.CheckXlatePath(var Arg: string): TZASErrors;
var
  New_path: string;
  Nposn: Integer;
  Orig_path: string;
  Tmp: string;
begin
  if Length(Arg) < 2 then
  begin
    Result := ZasInvalid;
    Exit;
  end;
  Tmp := Arg;
  Nposn := Pos('::', Tmp);
  if Nposn < 1 then
  begin
    Result := ZasInvalid;
    Exit;
  end;
  Tmp := SetSlash(Tmp, PsdInternal);
  Orig_path := Trim(Copy(Tmp, 1, Nposn - 1));
  New_path := Trim(Copy(Tmp, Nposn + 2, MAX_ARG_LENGTH));
  if (Orig_path = '') and (New_path = '') then
  begin
    Result := ZasIgnored;
    Exit;
  end;
  // TODO check paths valid if not empty
  Arg := Orig_path + '::' + New_path; // rebuild it ??
  Result := ZasNone; // good
end;

procedure TZMArgSplitter.Clear;
begin
  FAllow := '';
  FArgs.Clear;
  FError := ZasNone;
  FErrorMsg := '';
  FFound := '';
  FMain := '';
  FRaw := '';
end;

function TZMArgSplitter.GetArg(var Idx: Integer; AllowPW: Boolean): string;
var
  Ch: Char;
  Spaces: Integer;
  Lastchar: Integer;
  Start: Integer;
  Len: Integer;
  Nxt: Integer;
begin
  Result := '';
  if Idx < 1 then
    Idx := 1;
  Len := Length(FRaw);
  if Idx > Len then
  begin
    Idx := -1; // passed end
    Exit;
  end;
  Spaces := 0;
  // skip leading
  while Idx <= Len do
  begin
    if FRaw[Idx] > ' ' then
      Break;
    Inc(Idx);
  end;
  if Idx > Len then
  begin
    Idx := -1;
    Exit; // nothing valid found
  end;
  if (AllowPW and (FRaw[Idx] = ZPASSWORDARG)) then
  begin
    // pass to end of line
    Result := Copy(FRaw, Idx, Len - Pred(Idx));
    Idx := -1;
    Exit;
  end;

  if (FRaw[Idx] = ':') and (Idx < Len) and (FRaw[Idx + 1] = ':') then
  begin
    if ((Idx + 1) >= Len) or (FRaw[Idx + 2] = ' ') then
    begin
      Idx := -1;
      Exit; // nothing valid found
    end;
  end;

  // advance to next, find the length ignoring trailing space
  Nxt := Idx;
  Start := Idx;
  while Nxt <= Len do
  begin
    Ch := FRaw[Nxt];
    if (Ch = ZSWITCH) and (Spaces > 0) then
      Break; // at next switch
    if AllowPW and (Ch = ZPASSWORDARG) then
      Break; // at next (Comment)
    if (Ch = ':') and (Spaces > 0) and (Nxt < Len) and (FRaw[Nxt + 1] = ':')
    then
    begin
      if ((Nxt + 1) >= Len) or (FRaw[Nxt + 2] = ' ') then
        Break; // at 'rem'
    end;
    if (Ch <= ' ') then
    begin
      // skip but count space
      Inc(Nxt);
      Inc(Spaces);
      Continue;
    end;

    if (Ch = '"') then
    begin
      // copy previous
      Result := Result + Copy(FRaw, Start, Nxt - Start);
      Start := Nxt;
      Inc(Start); // past leading quote
      // find end of quote
      while Nxt <= Len do
      begin
        Inc(Nxt);
        if FRaw[Nxt] = '"' then
          Break;
      end;
      Result := Result + Copy(FRaw, Start, Nxt - Start);
      Inc(Nxt);
      Start := Nxt; // end quote
      Spaces := 0;
      Continue;
    end;

    // just a character
    Inc(Nxt);
    Spaces := 0;
  end;
  // copy previous
  Lastchar := Nxt - Spaces;
  if (Lastchar > Start) then
    Result := Result + Copy(FRaw, Start, Lastchar - Start);
  Idx := Idx + (Nxt - Idx);

  if Idx > Len then
    Idx := -1;
end;

function TZMArgSplitter.HandleMain(S: string): Boolean;
var
  Delimpos: Integer;
  I: Integer;
  Tmp: string;
begin
  Result := False;
  FMain := S;
  if Pos(ZSPECARG, FAllow) < 1 then
    Exit; // don't check
  if Pos(ZFILE_SEPARATOR, S) < 1 then
    Exit; // no sub spec
  // trim unwanted spaces
  while Pos(' >>', S) > 0 do
  begin
    I := Pos(' >>', S);
    S := Copy(S, 1, I - 1) + Copy(S, I + 1, 1024);
  end;
  while Pos('>> ', S) > 0 do
  begin
    I := Pos('>> ', S);
    S := Copy(S, 1, I + 1) + Copy(S, I + 3, 1024);
  end;
  if ZaoLastSpec in Options then
  begin
    // Split at last
    SplitQualifiedName(S, S, Tmp);
    FMain := S;
  end
  else
  begin
    // split at first
    Delimpos := Pos(ZFILE_SEPARATOR, S);
    Tmp := S;
    I := Delimpos - 1;
    while (I > 0) and (S[I] <= ' ') do
      Dec(I); // trim
    FMain := Copy(S, 1, I);
    I := Delimpos + Length(ZFILE_SEPARATOR);
    while (I < Length(Tmp)) and (S[I] <= ' ') do
      Inc(I); // trim
    Tmp := Copy(Tmp, I, MAX_ARG_LENGTH);
  end;
  Error := CheckInclude(Tmp);
  Result := Error <> ZasNone;
  if not Result then
    AnotherArg('>', Tmp)
  else
    FErrorMsg := Tmp;
end;

function TZMArgSplitter.HandleSwitch(TheSwitch: string): Boolean;
const
  Flags: string = 'JNST';
var
  Arg: string;
  C: Char;
  Opt: Char;
  Sw: Integer;
begin
  Result := True;
  Error := ZasDisallowed;
  FErrorMsg := TheSwitch;
  Opt := UpperOp(TheSwitch[2]); // option
  Sw := Pos(Opt, FAllow) - 1;
  if Sw < 0 then
    Exit;
  if Pos(Opt, FFound) > 0 then
  begin
    // duplicate
    Error := ZasDuplicate;
    Exit; // fatal
  end;
  Error := ZasInvalid;
  // we have wanted switch
  // if (Opt = 'S') or (Opt = 'N') or (Opt = 'T') then
  if Pos(Opt, Flags) > 0 then
  begin
    // can be /s or /s+ or /s-
    if Length(TheSwitch) = 2 then
    begin
      TheSwitch := TheSwitch + '+'
    end;
    C := TheSwitch[3];
    if (Length(TheSwitch) = 3) and ((C = '+') or (C = '-')) then
    begin
      Arg := C;
      Error := ZasNone;
    end;
  end
  else
  begin
    // have form /?:   ?
    if (Length(TheSwitch) < 4) or (TheSwitch[3] <> ':') then
    begin
      // no
      if Opt = 'E' then
      begin
        Error := ZasNone;
        Arg := SPEC_SEP; // allow /E as short for /E:| (use default);
      end
      else
        if Opt = 'F' then
        begin
          Error := ZasNone;
          Arg := ''; // allow /F or /F: as short for use current;
        end;
    end
    else
    begin
      Arg := Trim(Copy(TheSwitch, 4, MAX_ARG_LENGTH));
      case Opt of
        'C':
          Error := CheckCompression(Arg);
        'D':
          Error := CheckDate(Arg);
        'E':
          Error := CheckExcludes(Arg);
        'F':
          Error := CheckFolder(Arg);
        'O':
          Error := CheckOverWrite(Arg);
        'X':
          Error := CheckXlatePath(Arg);
      end;
    end;
  end;
  if Error <= ZasIgnored then
  begin
    if Error <> ZasIgnored then
      AnotherArg(Opt, Arg);
    Result := False;
    FErrorMsg := '';
    Error := ZasNone;
  end;
end;

function TZMArgSplitter.Has(Option: Char): Boolean;
begin
  Result := Pos(UpperOp(Option), FFound) > 0;
end;

function TZMArgSplitter.Index(Option: Char): Integer;
begin
  Option := UpperOp(Option);
  Result := Pos(Option, FFound) - 1;
end;

procedure TZMArgSplitter.SetAllow(const Value: string);
var
  Ch: Char;
  Up: string;
  I: Integer;
begin
  Up := UpperCase(Value);
  if FAllow <> Up then
  begin
    for I := 1 to Length(Up) do
    begin
      Ch := Up[I];
{$IFDEF UNICODE}
      if not CharInSet(Ch, KnownArgs) then
{$ELSE}
      if not(Ch in KnownArgs) then
{$ENDIF}
      begin
        Error := ZasInvalid;
        FErrorMsg := Up[I];
        Exit;
      end;
    end;
    FAllow := Up;
    SplitRaw;
  end;
end;

procedure TZMArgSplitter.SetRaw(const Value: string);
begin
  if FRaw <> Value then
  begin
    FRaw := Value;
    SplitRaw;
  end;
end;

procedure TZMArgSplitter.SplitRaw;
var
  AllowPW: Boolean;
  C: Char;
  Idx: Integer;
  S: string;
  Stt: Integer;
begin
  FArgs.Clear;
  FFound := '';
  FMain := '';
  FErrorMsg := '';
  FError := ZasNone;
  AllowPW := Pos(ZPASSWORDARG, FAllow) > 0;
  // split raw
  Idx := 1;
  while Idx > 0 do
  begin
    Stt := Idx;
    S := GetArg(Idx, AllowPW);
    if S <> '' then
    begin
      C := S[1];
      if C = ZSWITCH then
      begin
        if Length(S) < 2 then
        begin
          // invalid
          Error := ZasInvalid;
          FErrorMsg := Copy(FRaw, Stt, MAX_ARG_LENGTH);
          Break; // fatal
        end;
        if HandleSwitch(S) then
          Break; // fatal
      end
      else
        if C = ZPASSWORDARG then
          AnotherArg(ZPASSWORDARG, Copy(S, 2, MAX_ARG_LENGTH))
        else
          if HandleMain(S) then
            Break; // fatal
    end;
  end;
end;

function TZMArgSplitter.UpperOp(Option: Char): Char;
begin
  if (Option >= 'a') and (Option <= 'z') then
    Result := Char(Ord(Option) - 32)
  else
    Result := Option;
end;

end.
