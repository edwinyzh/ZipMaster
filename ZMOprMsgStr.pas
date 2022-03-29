unit ZMOprMsgStr;

// ZMMsgStr.pas - global string language handler
// This file is part of TZipMaster Version 1.9.2.x

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

// [zip>>]'0';  // uses user default language or primary [in zip]
// [zip>>]'*';  // uses user default language or primary [in zip]
// [zip>>]'1023';  // uses language 1023 or primary [in zip]
// [zip>>]'$XXXX';  // uses language XXXX or primary [in zip]
// [zip>>]'LL';  // uses named language [in zip]
// [zip>>]'pattern';  // uses first entry matching 'pattern' [in zip]
// 'file';  // file 'file'
// returns >0 strings loaded or <=0 error
type
  TZMOpLanguage = class(TZMOperationRoot)
  private
    FReqLanguage: string;
  public
    constructor Create(const ReqLanguage: string);
    function Execute(TheBody: TZMHandler): Integer; override;
    function Name: string; override;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Windows, System.SysUtils, System.SyncObjs,
{$ELSE}
  Windows, SysUtils, {$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$IFNDEF UNICODE}
  {$IFNDEF VERpre6}SyncObjs, {$ENDIF}
{$ENDIF}
{$ENDIF}
  ZMBody, ZMlister, ZMZipReader, ZMZipDirectory, ZMUnzipOpr,
  ZMUtils, ZMCore, ZMMsg;

const
  __UNIT__ = 32;

type
  TZMLangOpr = class(TZMUnzipOpr)
  private
    function FindById(ZM: TZMZipReader; const HexId: string): TZMEntryBase;
    function LoadFromZip(DstStrs: TStrings;
      const ById, Req, LangsZip: string): Integer;
    function LoadLang(DstStrs: TStrings; const Lang, LangsZip: string): Integer;
    function LoadZipEntry(DstStrs: TStrings; Entry: TZMEntryBase): Integer;
    function SetMsgLanguage(const Zl: string): Integer;
    function MergeLanguage(DstStrs: TStrings; const ReqName: string): Integer;
    function SetZipMsgLanguage(const Zl: string): Integer;
  public
    function SetLanguage(const Zl: string): Integer;
  end;

const
  SNoLanguageStrings = 'Could not Load language files';

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

function TZMLangOpr.FindById(ZM: TZMZipReader; const HexId: string):
    TZMEntryBase;
const
  Hexit = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
var
  Ch: Char;
  Entry: TZMEntryBase;
  Full: TZMEntryBase;
  Almost: TZMEntryBase;
  Num: string;
  PrimaryHigh: Integer;
  ToFind: string;
begin
  Result := nil;
  if Length(HexId) <> 4 then
    Exit;
  PrimaryHigh := Ord(HexId[2]) and 3; // high 2 bits of primary language
  Almost := nil;
  Full := nil;
  Entry := nil;
  repeat
    // find any likely match of primary language
    ToFind := '??' + Copy(HexId, 3, 2) + '-*.txt';
    Entry := ZM.SearchNameEx(ToFind, True, Entry);
    if Entry = nil then
      Break;
    Num := Copy(Entry.FileName, 1, 4); // get found ident as string
    // verify first 2 characters are hex
    if not(CharInSet(Num[1], Hexit) and CharInSet(Num[2], Hexit)) then
      Continue;
    if CompareText(Num, HexId) = 0 then
    begin
      // found full match
      Full := Entry;
      Break;
    end;
    // test high bits of primary
    Ch := Num[2];
    if (Ord(Ch) and 3) = PrimaryHigh then
    begin
      // Found primary match
      if Almost = nil then
        Almost := Entry;
    end;
  until False;
  if Full <> nil then
    Result := Full
  else
    if Almost <> nil then
      Result := Almost;
end;

function TZMLangOpr.LoadFromZip(DstStrs: TStrings;
  const ById, Req, LangsZip: string): Integer;
var
  Entry: TZMEntryBase;
  Idx: Integer;
  ResZip: TStream;
  TheZip: TZMZipReader;
begin
  Result := 0;
  ResZip := nil;
  TheZip := TZMZipReader.Create(Lister);
  try
    if LangsZip = '' then
    begin
      ResZip := OpenResStream(DZRES_Lng, RT_RCData);
      if ResZip = nil then
      begin
        Body.Inform(SNoLanguageStrings, {_LINE_}196, __UNIT__);
        Result := ZM_Error({_LINE_}197, ZS_NoLanguage);
        // resource zip not found
      end
      else
      begin
        ResZip.Position := 0;
        TheZip.Stream := ResZip;
      end;
    end
    else
      TheZip.ArchiveName := LangsZip;
    if Result = 0 then
    begin
      Body.Trace('Loading Language zip', {_LINE_}210, __UNIT__);
      Result := TheZip.OpenZip(False, False);
    end;
    if (Result >= 0) and (TheZip.Count > 0) then
    begin
      Idx := -1;
      if ById <> '' then
        Entry := FindById(TheZip, ById)
      else
        Entry := TheZip.SearchName(Req, True, Idx);
      // find first matching pattern
      if Entry <> nil then
        Result := LoadZipEntry(DstStrs, Entry)
      else
        Body.InformFmt('Could not find language: %s', [Req], {_LINE_}224,
          __UNIT__);
    end
    else
      if Body <> nil then
      begin
        if Result < 0 then
          Body.InformFmt('Error: %d loading zipped langauge files',
            [AbsErr(Result)],
            {_LINE_}233, __UNIT__);
      end;
  finally
    TheZip.Free;
    if ResZip <> nil then
      ResZip.Free;
  end;
end;

// filename is 'xxxx-lang.txt'
function TZMLangOpr.LoadLang(DstStrs: TStrings; const Lang, LangsZip: string):
    Integer;
var
  ById: string;
  IdToFind: Integer;
  Posn: Integer;
  Req: string;
begin
  Result := -1;
  if Lang = '' then
    Exit;
  Req := Trim(Lang);
  Posn := Pos(':', Req);
  if Posn > 2 then
    Req := Trim(Copy(Req, 1, Posn - 1));
  if Req = '*' then
    Req := '0';
  if (not IsWild(Req)) and (Pos('-', Req) < 5) and (ExtractFileExt(Req) = '')
  then
  begin
    if TryStrToInt(Req, IdToFind) then
    begin
      if IdToFind <= 0 then
      begin
        IdToFind := GetUserDefaultLCID;
        if (IdToFind >= $FFFF) or ((LangsZip = '') and (IdToFind = $0409)) then
          Exit; // use default
      end;
      ById := IntToHex(IdToFind, 4);
    end
    else
      Req := '????-' + Req + '.txt';
  end;
  Result := LoadFromZip(DstStrs, ById, Req, LangsZip);
end;

function TZMLangOpr.LoadZipEntry(DstStrs: TStrings;
  Entry: TZMEntryBase): Integer;
var
  Ls: TMemoryStream;
begin
  Body.Trace('Loading selected language', {_LINE_}284, __UNIT__);
  Ls := TMemoryStream.Create;
  try
    Result := UnzipAStream(Ls, Entry);
    if (Result = 0) and (Ls.Size > 10) then
    begin
      Ls.Position := 0;
      Result := Body.LoadMessageStrs(DstStrs, Ls);
      if Result < 0 then
      begin
        Body.Inform('Invalid language file', {_LINE_}294, __UNIT__);
        Result := ZM_Error({_LINE_}295, ZS_NoLanguage);
      end;
    end
    else
      Body.Inform('Error unzipping language strings', {_LINE_}299, __UNIT__);
  finally
    Ls.Free;
  end;
end;

function TZMLangOpr.MergeLanguage(DstStrs: TStrings;
  const ReqName: string): Integer;
var
  AStream: TStream;
  EntryName: string;
  ZipName: string;
begin
  Result := 0;
  if ReqName = '' then
    Exit; // use default
  if (Pos('>>', ReqName) < 1) and (ExtractFileExt(ReqName) <> '') then
  begin
    // load specified file
    if FileExists(ReqName) then
    begin
      AStream := TFileStream.Create(ReqName, FmOpenRead or FmShareDenyWrite);
      try
        AStream.Position := 0;
        Result := Body.LoadMessageStrs(DstStrs, AStream);
        if Result < 0 then
        begin
          Body.Inform('Invalid language file', {_LINE_}326, __UNIT__);
          Result := ZM_Error({_LINE_}327, ZS_NoLanguage);
          // TODO: needs new error
        end;
      finally
        AStream.Free;
      end;
    end
    else
      Result := LoadLang(DstStrs, ReqName, ''); // from resources
  end
  else
  begin
    if (Pos('>>', ReqName) > 1) then
    begin
      ZipName := ZSplitString('>>', ReqName, EntryName);
      Result := LoadLang(DstStrs, EntryName, ZipName);
    end
    else
      Result := LoadLang(DstStrs, ReqName, ''); // from resources
  end;
end;

function TZMLangOpr.SetLanguage(const Zl: string): Integer;
begin
  Result := SetZipMsgLanguage(Zl);
  if Result <= 0 then
    Body.InformFmt('Error %d setting language: %s', [Result, Zl], {_LINE_}353,
      __UNIT__);
end;

function TZMLangOpr.SetMsgLanguage(const Zl: string): Integer;
var
  DefStrs: TStrings;
begin
  Result := -1;
  DefStrs := TStringList.Create;
  try
    DefStrs.Capacity := MAX_ID + 1;
    Result := Body.LoadDefaultStrings(DefStrs); // , Body);
    if (Result > 0) and (Zl <> '') then
      Result := MergeLanguage(DefStrs, Zl);
  finally
    if Result >= 0 then
      Body.MessageStrs := DefStrs; // set new strings
    DefStrs.Free; // the previous or 'bad' strings
  end;
  if Result >= 0 then
  begin
    Result := 0;
  end
  else
    Body.Inform('Error unzipping language strings', {_LINE_}378, __UNIT__);
end;

function TZMLangOpr.SetZipMsgLanguage(const Zl: string): Integer;
const
  DefLang: array [0 .. 2] of string = ('US', '$0409', '1033');
var
  UseDefault: Boolean;
  I: Integer;
  Req: string;
begin
  Req := Trim(Unquote(Zl));
  if Req <> '' then
  begin
    if (Pos('>>', Req) = 0) and (Pos(':', Req) > 2) then
    begin
      Req := Copy(Req, 1, Pos(':', Req) - 1);
    end;
    UseDefault := False;
    for I := 0 to 2 do
      if SameText(Req, DefLang[I]) then
      begin
        UseDefault := True;
        Break;
      end;
    if UseDefault then
      Req := '';
  end;
  Result := SetMsgLanguage(Req);
end;

{ TZMOpLanguage }

constructor TZMOpLanguage.Create(const ReqLanguage: string);
begin
  inherited Create;
  FReqLanguage := ReqLanguage;
end;

function TZMOpLanguage.Execute(TheBody: TZMHandler): Integer;
var
  FOper: TZMLangOpr;
begin
  FOper := TZMLangOpr.Create(TheBody as TZMLister);
  AnOperation := FOper;
  Result := FOper.SetLanguage(FReqLanguage);
end;

function TZMOpLanguage.Name: string;
begin
  Result := '_SetLanguage';
end;

end.
