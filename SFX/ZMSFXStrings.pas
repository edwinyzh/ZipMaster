(******************************************************************)
(* SFX for DelZip v1.8                                            *)
(* Copyright 2002-2004, 2008                                      *)
(*                                                                *)
(* written by Markus Stephany                                     *)
(* modified by Russell Peters, Roger Aelbrecht
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

   This file is part of TZipMaster Version 1.9.

    TZipMaster is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TZipMaster is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

    contact: problems@delphizip.org (include ZipMaster in the subject).
    updates: http://www.delphizip.org
    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip

  modified 30-Jan-2008
---------------------------------------------------------------------------*)
unit ZMSFXStrings;

{
this unit contains localized strings

}

interface

uses Windows;
        
const
  SFX_LVC_Filename = 1;//1024;   // 'Filename';
  SFX_LVC_Filesize = 2;//1025;   // 'Size';

  // error messages
  SFX_Err_CannotCloseFile = 3;//1040;   // 'Cannot close ><';
  SFX_Err_Archive = 5;//1041;           // 'Error reading archive ><';
  SFX_Err_InvalidFileName = 6;//1042;   // 'Invalid filename.';
  SFX_Err_Directory = 7;//1043;         // 'Error in directory ><';
  SFX_Err_ZipUnknownComp = 8;//1044;    // 'Unknown compression type';
  SFX_Err_ArchiveCorrupted = 9;//1045;  // 'Archive corrupted, please try to download this file again.';
  SFX_Err_CannotOpenFile = 10;//1046;    // 'Cannot open ><';
  SFX_Err_CannotWriteFile = 11;//1047;   // 'Cannot write to ><';
  SFX_Err_Run_Run = 12;//1048;           // 'Couldn''t run >< ><';
  SFX_Err_Run_Inst = 13;//1049;          // 'Couldn''t install >< ><';
  SFX_Err_CRC32 = 14;//1050;             // 'CRC32 Error in ><';
//  SFX_Err_BadFilename = 15;

  // messages
  SFX_Msg_RunCheckBox_Run = 16;//1072;   // 'After extraction, run: >< ><';
  SFX_Msg_RunCheckBox_Inst = 17;//1073;  // 'After extraction, install: >< ><';
  SFX_Msg_FileExists = 18;//1074;        // '>< already exists, overwrite ?';
  SFX_Msg_AllExtracted = 19;//1075;      // 'All files have been extracted.';
  SFX_Msg_SelNotExtracted = 20;//1076;   // 'The selected file(s) couldn''t get extracted.';
  SFX_Msg_SomeNotExtracted = 21;//1077;  // 'Some file(s) couldn''t get extracted.';
  SFX_Msg_QueryCancel = 22;//1078;       // 'Do you want to cancel extraction?';
  SFX_Msg_InsertDiskVolume = 23;//1079;  // 'Please insert disk volume >< in drive ><';

  // about text
//  SFX_Msg_About = 1088;
  SFX_MSG_ABOUT0 = 60;
  SFX_MSG_ABOUT1 = 61;
  SFX_MSG_AUTHORS = 62;//1089;//'The authors';
  SFX_MSG_ABOUT2 = 63;
  SFX_MSG_CREDITS = 64;//1090;//'Credits to';
  SFX_MSG_ABOUT3 = 65;
  SFX_MSG_TRANSLATION = 66;//1091;//' ';

  // message / dialog box titles
  SFX_Cap_Err = 28;//1104;               // 'Error...';
  SFX_Cap_App = 29;//1105;               // 'DelZip Self-Extractor';
  SFX_Cap_Browse = 30;//1106;            // 'Please choose the destination directory';
  SFX_Cap_About = 31;//1107;             // 'About DelZip Self-Extractor...';
  SFX_Cap_Finished = 32;//1108;          // 'Finished.';
  SFX_Cap_Password = 33;//1109;          // 'Enter password for ><:';
  // Main Dialog buttons
  SFX_Btn_Start = 34;//1120;     // 1
  SFX_Btn_Close = 35;//1121;     // 2
  SFX_Btn_About = 36;//1122;     // 103
  SFX_Btn_Files = 37;//1123;     // 501
  SFX_Btn_Existing = 38;//1124;  // 602
  SFX_Btn_ShowFiles = 39;//1125; // 105
  SFX_Btn_ExtractTo = 40;//1126; // 601
  SFX_Btn_OvrAsk = 41;//1127;    // 703
  SFX_Btn_OvrSkip = 42;//1128;   // 702
  SFX_Btn_OvrAll = 43;//1129;    // 701
  // FileExists Dialog
  SFX_Btn_Yes = 44;//1136;       // 1
  SFX_Btn_No = 46;//1137;        // 2
  SFX_Btn_DontAsk = 47;//1138;   // 301
  // Password Dialog
  SFX_Btn_Ok = 48;//1152;        // 1
  SFX_Btn_Cancel = 49;//1153;    // 2
  // Dialog titles
  SFX_Ttl_Main = 50;//1068;
  SFX_Ttl_File = 51;//1069;
  SFX_Ttl_PWrd = 52;//1070;
  // Language Dialog
  SFX_Btn_Select = 53;//1085;


Var
  VRec_DefStrings: PByte = nil;   // pointer to table of strings
  VRec_Strings: PByte = nil;   // pointer to table of strings

const
  MaxStringLen = MAX_PATH * 2;

procedure SetDlgStrings(Dlg: hWnd; val: cardinal);
function SFXString(id: integer): string;             

function To_Str(CP: Integer; Source: pAnsiChar; len: integer; IsName: Boolean;
    var Bad: boolean): string;

// table format - ident: byte, strng[]: byte, 0: byte; ...;0
function LoadSFXStr(ptbl: pByte; ident: Byte): String;

implementation

uses
  ZMSFXDefs;

type
  IdTbleRec2 = packed record
    DlgId: byte;
    Ctrl: word;
    StrId: byte;
  end;

const
  SOFF = 0;//1024;
  IdTbl1: array [0..22] of IdTbleRec2 = (
    (DlgID: SFX_DLG_MAIN; Ctrl: 0; StrId: SFX_Ttl_Main - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 1; StrId: SFX_Btn_Start - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 2; StrId: SFX_Btn_Close - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 103; StrId: SFX_Btn_About - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 501; StrId: SFX_Btn_Files - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 602; StrId: SFX_Btn_Existing - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 601; StrId: SFX_Btn_ExtractTo - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 105; StrId: SFX_Btn_ShowFiles - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 703; StrId: SFX_Btn_OVRAsk - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 702; StrId: SFX_Btn_OVRSkip - SOFF),
    (DlgID: SFX_DLG_MAIN; Ctrl: 701; StrId: SFX_Btn_OVRAll - SOFF),
    (DlgID: SFX_DLG_FILE; Ctrl: 0; StrId: SFX_Ttl_File - SOFF),
    (DlgID: SFX_DLG_FILE; Ctrl: 1; StrId: SFX_Btn_Yes - SOFF),
    (DlgID: SFX_DLG_FILE; Ctrl: 2; StrId: SFX_Btn_No - SOFF),
    (DlgID: SFX_DLG_FILE; Ctrl: 301; StrId: SFX_Btn_DontAsk - SOFF),
    (DlgID: SFX_DLG_PWRD; Ctrl: 0; StrId: SFX_Ttl_PWrd - SOFF),
    (DlgID: SFX_DLG_PWRD; Ctrl: 1; StrId: SFX_Btn_Ok - SOFF),
    (DlgID: SFX_DLG_PWRD; Ctrl: 2; StrId: SFX_Btn_Cancel - SOFF),
    (DlgID: SFX_DLG_LANG; Ctrl: 0; StrId: SFX_Ttl_Main - SOFF),
    (DlgID: SFX_DLG_LANG; Ctrl: 1; StrId: SFX_Btn_Ok - SOFF),
    (DlgID: SFX_DLG_LANG; Ctrl: 2; StrId: SFX_Btn_Close - SOFF),
    (DlgID: SFX_DLG_LANG; Ctrl: ID_LANG_SELECT; StrId: SFX_Btn_Select - SOFF),
    (DlgID: 0; Ctrl: 0; StrId: 0)
    );

procedure SetDlgStrings(Dlg: hWnd; val: cardinal);
var
  d: cardinal;
  i: cardinal;
  s: string;
  x: cardinal;
begin
  i := 0;
  repeat
    d := IdTbl1[i].DlgId;
    if d = val then
    begin
      s := SFXString(IdTbl1[i].StrId + SOFF);
      if s <> '' then
      begin
        x := IdTbl1[i].Ctrl;
        if x = 0 then
          SetWindowText(Dlg, pChar(s))
        else
          SetDlgItemText(Dlg, x, PChar(s));
      end;
    end;
    Inc(i);
  until (d = 0) or (d > val);
end;

function StrHasExt(Source: pAnsiChar; len: integer): Boolean;
var
  c: AnsiChar;
  i: integer;
begin
  Result := false;
  if len < 0 then
    len := 4096;
  for i := 1 to Len do
  begin
    c := Source^;
    if c = #0 then
      break;
    if (c > #126) {or (c < #31)} then
    begin
      Result := True;
      break;
    end;
    inc(Source);
  end;
end;


function To_Str(CP: Integer; Source: pAnsiChar; len: integer; IsName: Boolean;
    var Bad: boolean): string;
const
  WC_NO_BEST_FIT_CHARS = $00000400; // do not use best fit chars
var
{$IFNDEF UNICODE}
  buffer: array [0..(MaxStringLen + 3)] of widechar;
  cnt:   integer;
  flg: cardinal;
  notsup: longBool;
{$ENDIF}
  wcnt:  integer;
begin
  Result := '';
  Bad := false;   // hopefully
  if Source^ = #0 then
    exit;
{$IFNDEF UNICODE}
  if (CP = 0) and (len > 0) then
  begin
    SetLength(Result, len);
    Move(Source^, PChar(Result)^, len);
    exit;
  end;
  wcnt := MultiByteToWideChar(CP, 0, Source, len, nil, 0);
  if (wcnt > 0) and (wcnt < MaxStringLen) then
  begin
    wcnt := MultiByteToWideChar(CP, 0, Source, len,
      pWideChar(@buffer[0]), MaxStringLen);
    if wcnt > 0 then
    begin
      buffer[wcnt] := #0;
      if IsName then
        flg := WC_NO_BEST_FIT_CHARS
      else
        flg := 0;
      cnt := WideCharToMultiByte(0, flg, pWideChar(@buffer[0]),
               wcnt + 1, nil, 0, nil, @notsup);
      Bad := IsName and notsup;
      if (cnt > 0) then
      begin
        SetLength(Result, cnt);
        cnt := WideCharToMultiByte(0, flg, pWideChar(@buffer[0]),
                wcnt + 1, PAnsiChar(Result), cnt, nil, nil);
        if cnt = 0 then
          Bad := True
        else
          Result := PAnsiChar(Result);
      end;
    end;
  end;
{$ELSE}
  wcnt := MultiByteToWideChar(CP, 0, Source, len, nil, 0);
  if (wcnt > 0) then
  begin
    SetLength(Result, wcnt);
    wcnt := MultiByteToWideChar(CP, 0, Source, len, PChar(Result), wcnt);
    if (wcnt > 0) then
      Result := PWideChar(Result);   // don't want end 0
  end
  else
    Bad := True;
{$ENDIF}
end;


// table format - ident: byte, strng[]: byte, 0: byte; ...;0
function LoadSFXStr(ptbl: pByte; ident: Byte): String;
var
  bad: Boolean;
  id: Byte;
begin
  Result := '';
  if (ptbl = nil) or (ident = 0) then
    exit;
  id := ptbl^;
  while (id <> 0) and (id <> ident) do
  begin
    while ptbl^ <> 0 do
      inc(ptbl);
    inc(ptbl);
    id := ptbl^;
  end;
  if id = ident then
  begin
    inc(ptbl);
    Result := To_Str(CP_UTF8, pAnsiChar(ptbl), -1, false, bad);
  end;
end;

function SFXString(id: integer): string;
begin
  Result := '';
  if VRec_Strings <> nil then
    Result := LoadSFXStr(VRec_Strings, id);
  if Result = '' then
//    Result := LoadSFXStr(@StrBlok, {true,} id);
  begin
    if VRec_DefStrings <> nil then
      Result := LoadSFXStr(VRec_DefStrings, id)
    else
    begin
      MessageBox(0, 'Missing resource!', 'DelphiZip SFX', MB_ICONSTOP or MB_TASKMODAL);
      Halt(2);
    end;
  end;
end;

end.
