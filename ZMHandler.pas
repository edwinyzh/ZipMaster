unit ZMHandler;

// ZMHandler.pas - ZipMaster message string handler

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

{$INCLUDE   '.\ZipVers.inc'}
{$I   '.\ZMConfig192.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes;
{$ELSE}
  Classes;
{$ENDIF}

type
  TZMHandler = class
  private
    FMessageStrs: TStrings;
    FMasterObj: TObject;
{$IFNDEF UNICODE}
    function GetIsUTF8: Boolean;
{$ENDIF}
    function FixExpandedStrings(DstStrs: TStrings; Updated: Integer): Integer;
    function GetLanguageID(const AStr: AnsiString): Integer;
    procedure SetMessageStrs(const Value: TStrings);
  protected
{$IFDEF UNICODE}
    function MergeStrs(DstStrs: TStrings; const Ws: string): Integer;
{$ELSE}
    function MergeStrs(DstStrs: TStrings; const Ws: WideString): Integer;
{$ENDIF}
  public
    constructor Create(TheMaster: TObject);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
{$IFNDEF UNICODE}
    function AsSysStr(const AString: string): string;
{$ENDIF}
    procedure FillEmptyStrs(DstStrs: TStrings);
    function LoadDefaultStrings(DefStrs: TStrings): Integer;
    function LoadDefStrings: Integer;
    function LoadMessageStrs(DstStrs: TStrings; Ss: TStream): Integer;
    function ZipFmtLoadStr(Id: Integer; const Args: array of const): string;
    function ZipLoadStr(Id: Integer): string;
    property MessageStrs: TStrings read FMessageStrs write SetMessageStrs;
{$IFNDEF UNICODE}
    property IsUTF8: Boolean read GetIsUTF8;
{$ENDIF}
  end;

type
  TZMErrors = class
  private
    FCode: Integer;
    FErrLine: Integer;
    FErrUnit: Integer;
    FExtCode: Integer;
    FErrMessage: string;
    function GetIsDll: Boolean;
    procedure SetCode(Value: Integer);
    procedure SetExtCode(Value: Integer);
  public
    procedure AfterConstruction; override;
    procedure Clear;
    function ErrorStr(ExtErr: Integer): string;
    property Code: Integer read FCode write SetCode;
    property ErrLine: Integer read FErrLine;
    property ErrUnit: Integer read FErrUnit;
    property ExtCode: Integer read FExtCode write SetExtCode;
    property IsDll: Boolean read GetIsDll;
    property ErrMessage: string read FErrMessage write FErrMessage;
  end;

type
  TZMOperOpts = (ZorFSpecArgs, ZorZip);
  TZMOperRes = set of TZMOperOpts;

type
  TZMOperationRoot = class
  private
    FAnOperation: TObject; // for cleanup
  public
    procedure BeforeDestruction; override;
    function Execute(TheBody: TZMHandler): Integer; virtual; abstract;
    function Needs: TZMOperRes; virtual;
    function Changes: TZMOperRes; virtual;
    function Name: string; virtual; abstract;
    property AnOperation: TObject read FAnOperation write FAnOperation;
  end;

type
  TZMRoot = class(TZMHandler)
  private
    FErrors: TZMErrors;
    FStoredErrMsg: string;
    FStoredError: Integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ErrMsg(ExtErr: Integer): string;
    function PrepareErrMsg(Ident: Integer; const Args: array of const;
      LineNo, UnitNo: Integer): Integer;
    function Run(TheOperation: TZMOperationRoot): Integer; virtual; abstract;
    property Errors: TZMErrors read FErrors;
  end;

{$IFNDEF UNICODE}

function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
{$J+}
// writeable
const
  UsingUTF8: Boolean = False; // global
{$J-}
{$ENDIF}

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Windows, System.SysUtils,
{$ELSE}
  Windows, SysUtils,
{$ENDIF}
  ZipMstr, ZMUtils, ZMMsg, ZMUTF8, ZMEngine;

{ TZMHandler }

const
  SResourceMissingFor = 'Resource missing for ';
  SNoLanguageStrings = 'Could not load language files';

procedure TZMHandler.AfterConstruction;
begin
  inherited;
  FMessageStrs := TStringList.Create;
end;

procedure TZMHandler.BeforeDestruction;
begin
  FMessageStrs.Free;
  inherited;
end;

constructor TZMHandler.Create(TheMaster: TObject);
begin
  inherited Create;
  FMasterObj := TheMaster;
end;

{$IFNDEF UNICODE}
function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
begin
  // Win 95, 98, ME do not have UTF8 support
  UsingUtf8 := UseUTF8 and (Win32PlatForm = Ver_Platform_Win32_NT);
  Result := UsingUtf8;
end;

function TZMHandler.AsSysStr(const AString: string): string;
begin
  if IsUTF8 then
    Result := UTF8ToStr(AString)
  else
    Result := AString;
end;
{$ENDIF}

procedure TZMHandler.FillEmptyStrs(DstStrs: TStrings);
var
  I: Integer;
begin
  DstStrs.Clear;
  DstStrs.Capacity := MAX_ID + 1;
  for I := 0 to MAX_ID do
    DstStrs.Add('');
end;

function TZMHandler.FixExpandedStrings(DstStrs: TStrings;
  Updated: Integer): Integer;
const
  Expanded: array [0 .. 14] of Integer = (ZE_BadFileName, ZE_DataCopy,
    ZE_DataDesc, ZE_DuplFileName, ZE_ExceptErr, ZE_Existing, ZE_FileChanged,
    ZE_FileError, ZE_FileOpen, ZE_LOHBadWrite, ZE_NoOverwrite,
    ZE_SetFileAttributes, ZE_SetFileInformation, ZE_SetFileTimes, ZE_WildName);
var
  I: Integer;
begin
  Result := Updated;
  if Pos('%', DstStrs[ZE_EventEx]) < 1 then
    DstStrs[ZE_EventEx] := DstStrs[ZE_EventEx] + ' %s';
  for I := 0 to high(Expanded) do
  begin
    if Pos('%', DstStrs[Expanded[I]]) < 1 then
      DstStrs[Expanded[I]] := DstStrs[Expanded[I]] + ': ''%s''';
  end;
end;

{$IFNDEF UNICODE}
function TZMHandler.GetIsUTF8: Boolean;
begin
  Result := UsingUTF8;
end;
{$ENDIF}

function TZMHandler.GetLanguageID(const AStr: AnsiString): Integer;
const
  NeededU: AnsiString = 'ZA_ID';
  NeededL: AnsiString = 'za_id';
var
  ACh: AnsiChar;
  L: string;
  Posn: Integer;
  Matching: Integer;
  MaxPosn: Integer;
begin
  Result := 0;
  Matching := 1;
  Posn := 0;
  if (Length(AStr) > 1) and (AStr[1] = '1') then
  begin
    Result := $0409;
    Exit;
  end;
  MaxPosn := Length(AStr) - (5 + Length(NeededU));
  while Posn <= MaxPosn do
  begin
    Inc(Posn);
    ACh := AStr[Posn];
    if (ACh = NeededU[Matching]) or (ACh = NeededL[Matching]) then
    begin
      Inc(Matching);
      if Matching > Length(NeededU) then
        Break; // found
      Continue;
    end;
    Matching := 1;
  end;
  if Matching > Length(NeededU) then
  begin
    while (Posn < Length(AStr)) and (AStr[Posn] <> '"') do
      Inc(Posn);
    Inc(Posn);
    L := '';
    while (Posn < Length(AStr)) and (AStr[Posn] <> '"') do
    begin
      L := L + Char(AStr[Posn]);
      Inc(Posn);
    end;
    if Length(L) > 3 then
    begin
      if (L[1] = '0') and ((L[2] = 'x') or (L[2] = 'X')) then
        L := '$' + Copy(L, 3, 4);
      Result := 0;
      if not TryStrToInt(L, Result) then
        Result := -1;
    end;
  end;
end;

function TZMHandler.LoadDefaultStrings(DefStrs: TStrings): Integer;
var
  CRC: Dword;
  Si: TMemoryStream;
  So: TMemoryStream;
begin
  Si := TMemoryStream.Create;
  try
    // load 'static' compressed default strings
    ReadCompressedStrings(Si); // read array
    Si.Position := 0;
    Si.ReadBuffer(CRC, Sizeof(DWord));
    So := TMemoryStream.Create;
    try
      Result := UndeflateQ(So, Si, Si.Size - Sizeof(DWORD), 8, CRC);
      if ((Result = 0) or (AbsErr(Result) = ZE_BadCRC)) and (So.Size < 20000)
      then
      begin
        // success
        So.Position := 0;
        FillEmptyStrs(DefStrs);
        Result := LoadMessageStrs(DefStrs, So);
      end
      else
        Result := -1;
    finally
      FreeAndNil(So);
    end;
  finally
    Si.Free;
  end;
end;

function TZMHandler.LoadDefStrings: Integer;
var
  DefStrs: TStrings;
begin
  DefStrs := TStringList.Create;
  try
    DefStrs.Capacity := MAX_ID + 1;
    Result := LoadDefaultStrings(DefStrs);
    if Result >= 0 then
      MessageStrs := DefStrs; // set new strings
  finally
    DefStrs.Free; // the previous or 'bad' strings
  end;
end;

{$IFDEF UNICODE}
function TZMHandler.MergeStrs(DstStrs: TStrings; const Ws: string): Integer;
{$ELSE}
function TZMHandler.MergeStrs(DstStrs: TStrings; const Ws: WideString): Integer;
{$ENDIF}
var
  Sl: TStringList;
  I: Integer;
  Id: Integer;
  IdStr: string;
  Psn: Integer;
  Qs: string;
  Qsp: PChar;
  S: string;
begin
  Result := 0;
  Sl := TStringList.Create;
  try
{$IFNDEF UNICODE}
    if IsUTF8 then
      Sl.Text := WideToUTF8(Ws)
    else
{$ENDIF}
      Sl.Text := Ws; // will convert if needed
    for I := 0 to Sl.Count - 1 do
    begin
      S := Sl[I];
      Psn := Pos(',', S);
      if Psn < 2 then
        Continue;
      IdStr := Trim(Copy(S, 1, Psn - 1));
      if IdStr = '' then
        Continue;
      Id := -1;
      if IdStr[1] = 'Z' then
        Id := FindIndentifier(IdStr);
      if Id < 0 then
      begin
        if not TryStrToInt(IdStr, Id) then
          Continue; // not found or invalid
      end;
      if Id > MAX_ID then
        Continue;
      Qs := Trim(Copy(S, Psn + 1, Length(S)));
      S := Qs;
      if (Qs <> '') and (Qs[1] = '"') then
      begin
        Qsp := PChar(Qs);
        S := AnsiExtractQuotedStr(Qsp, '"');
      end;
      S := StringReplace(S, '\n', #13#10, [RfReplaceAll]);
      DstStrs[Id] := S;
      Inc(Result);
    end;
  finally
    Sl.Free;
  end;
end;

function TZMHandler.LoadMessageStrs(DstStrs: TStrings; Ss: TStream): Integer;
var
  AStr: AnsiString;
  Ws: TZMWideString;
  LID: Integer;
  Pw: PWideChar;
begin
  Result := -1;
  if (Ss.Size > 20000) or (Ss.Size < 30) then
    Exit;
  // read
  SetLength(AStr, Ss.Size);
  Ss.Position := 0;
  if Ss.Read(PAnsiChar(AStr)^, Ss.Size) <> Ss.Size then
  begin
    Result := -21;
    Exit;
  end;
  // make guess at encoding
  if (AStr[2] = #0) or ((AStr[1] = #$FF) and (AStr[2] = #$FE)) then
  begin
    // Unicode
    Pw := PWideChar(PAnsiChar(AStr));
    if AStr[2] = #$FE then
      Inc(Pw);
    Ws := Pw;
  end
  else
  begin
    // Ansii
    // find ZA_ID
    LID := GetLanguageID(AStr);
    if LID <= 0 then
    begin
      // assume default
      Ws := TZMWideString(AStr);
    end
    else
      Ws := StrToWideEx(AStr, LID, Length(AStr));
  end;
  Result := MergeStrs(DstStrs, Ws);
  // expand strings
  if Result >= 0 then
    Result := FixExpandedStrings(DstStrs, Result);
end;

procedure TZMHandler.SetMessageStrs(const Value: TStrings);
begin
  if FMessageStrs <> Value then
  begin
    FMessageStrs.Clear;
    FMessageStrs.Assign(Value);
  end;
end;

function TZMHandler.ZipFmtLoadStr(Id: Integer;
  const Args: array of const): string;
begin
  Result := ZipLoadStr(Id);

  if Result <> '' then
    Result := Format(Result, Args);
end;

function TZMHandler.ZipLoadStr(Id: Integer): string;
var
  D: string;
  MsgId: Integer;
  TmpOnZipStr: TZMLoadStrEvent;
begin
  Result := '';
  MsgId := AbsErr(Id); // remove possible extended info
  if MessageStrs.Count < MAX_ID then
    LoadDefStrings;
  if (MsgId >= 0) and (MsgId <= MAX_ID) then
  begin
    if MsgId < MessageStrs.Count then
      Result := MessageStrs[MsgId]
    else
      Result := SNoLanguageStrings;
  end
  else
    if MsgId = ZS_NoLanguage then
      Result := SNoLanguageStrings;

  TmpOnZipStr := TCustomZipMaster(FMasterObj).OnLoadStr;
  if Assigned(TmpOnZipStr) then
  begin
    D := Result;
    TmpOnZipStr(MsgId, D);
    if D <> '' then
      Result := D;
  end;
  if (Result = '') and (MsgId >= 0) then
    Result := SResourceMissingFor + IntToStr(MsgId);
end;

procedure TZMErrors.AfterConstruction;
begin
  inherited;
  Clear;
end;

procedure TZMErrors.Clear;
begin
  FCode := 0;
  FExtCode := 0;
  FErrMessage := '';
  FErrUnit := 0;
  FErrLine := 0;
end;

function TZMErrors.ErrorStr(ExtErr: Integer): string;
var
  ErrC: Integer;
  ErrL: Integer;
  ErrU: Integer;
  ErrX: Integer;
begin
  ErrX := ExtErr;
  if ErrX < 0 then
    ErrX := -ErrX;
  ErrU := (ErrX shr ZERR_UNIT_SHIFTS) and ZERR_UNIT_MASK_SHIFTED;
  ErrL := (ErrX shr ZERR_LINE_SHIFTS) and ZERR_LINE_MASK_SHIFTED;
  ErrC := ErrX and $1FF;
  Result := Format(' %d {%d, %d, %d}', [ExtErr, ErrU, ErrL, ErrC]);
end;

function TZMErrors.GetIsDll: Boolean;
begin
  Result := (FExtCode and $40000000) <> 0;
end;

procedure TZMErrors.SetCode(Value: Integer);
begin
  if Value < 0 then
    Value := -Value;
  FCode := Value and ZERR_ERROR_MASK;
end;

procedure TZMErrors.SetExtCode(Value: Integer);
var
  Tcode: Integer;
begin
  if Value < 0 then
    Value := -Value;
  if FExtCode <> Value then
  begin
    FExtCode := Value;
    if (Value and $40000000) = 0 then
    begin
      // component error
      FErrUnit := (Value shr ZERR_UNIT_SHIFTS) and ZERR_UNIT_MASK_SHIFTED;
      FErrLine := (Value shr ZERR_LINE_SHIFTS) and ZERR_LINE_MASK_SHIFTED;
      Code := Value;
    end
    else
    begin
      // dll error
      // 01FF FFFF  LLLL LLLL   LLLL MTTT  EEEE EEEE  {31 .. 0}
      // F = file number (6 bits = 63 files)
      // L = line number (12 bits=4096 lines)
      // M = ErrMessage instead of error string
      // T = type  (3 bits=8)
      // E = error/string code (8 bits = 256 errors)
      FErrUnit := (Value shr 24) and $3F;
      FErrLine := (Value shr 12) and $FFF;
      Tcode := (Value and $FF);
      if Tcode <> 0 then
      begin
        Tcode := Tcode + ZD_GOOD;
        if Tcode > ZD_SKIPPED then
          Tcode := ZD_ERROR;
        Code := Tcode;
      end;
    end;
  end;
end;

{ TZMRoot }
procedure TZMRoot.AfterConstruction;
begin
  inherited;
  FErrors := TZMErrors.Create;
end;

procedure TZMRoot.BeforeDestruction;
begin
  FErrors.Free;
  inherited;
end;

function TZMRoot.ErrMsg(ExtErr: Integer): string;
begin
  if (ExtErr = FStoredError) and (FStoredErrMsg <> '') then
    Result := FStoredErrMsg
  else
    Result := ZipLoadStr(ExtErr);
  FStoredErrMsg := '';
  FStoredError := -1;
end;

function TZMRoot.PrepareErrMsg(Ident: Integer; const Args: array of const;
  LineNo, UnitNo: Integer): Integer;
begin
  if UnitNo > 0 then
    Result := -((UnitNo shl ZERR_UNIT_SHIFTS) + (LineNo shl ZERR_LINE_SHIFTS) or
      AbsErr(Ident))
  else
    Result := Ident; // assume error with extended information
  FStoredError := Result;
  FStoredErrMsg := ZipLoadStr(Ident);
  FStoredErrMsg := Format(FStoredErrMsg, Args);
end;

//function TZMRoot.Run(TheOperation: TZMOperationRoot): Integer;
//begin
//  raise Exception.Create('Should not happen');
//end;

procedure TZMOperationRoot.BeforeDestruction;
begin
  FAnOperation.Free;
  inherited;
end;

function TZMOperationRoot.Changes: TZMOperRes;
begin
  Result := [];
end;

function TZMOperationRoot.Needs: TZMOperRes;
begin
  Result := [];
end;

end.
