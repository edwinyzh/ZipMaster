unit ZMDlg;

// ZMDlg.pas - DialogBox with buttons from language strings

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

interface

{$INCLUDE   '.\ZipVers.inc'}

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, VCL.Forms, VCL.Dialogs, VCL.StdCtrls,
{$ELSE}
  Classes, Windows, Forms, Dialogs, StdCtrls,
{$ENDIF}
  ZMCore;

// High word = $10 or TMsgDlgType, low word = context
const
  ZmtWarning = $100000;
  ZmtError = $110000;
  ZmtInformation = $120000;
  ZmtConfirmation = $130000;
  ZmtPassword = $140000;

type
  TZipDialogBox = class(TForm)
  private
    AvDlgUnits: TPoint;
    BeepId: Integer;
    Ctx: Integer;
    IconID: PChar;
    PwdEdit: TEdit;
    function GetDlgType: Integer;
    function GetPWrd: string;
    procedure SetPwrd(const Value: string);
  public
    constructor CreateNew2(Owner: TComponent; Context: Integer); virtual;
    procedure Build(const Title, Msg: string; Btns: TMsgDlgButtons;
      const Core: TZMCore);
    function ShowModal: Integer; override;
    property DlgType: Integer read GetDlgType;
    property PWrd: string read GetPWrd write SetPwrd;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils, VCL.Graphics, VCL.ExtCtrls, VCL.Controls,
{$ELSE}
  SysUtils, Graphics, ExtCtrls, Controls, {$IFNDEF UNICODE}ZMUTF8, {$ENDIF}
{$ENDIF}
  ZMMsg, ZMHandler;

const
  SZmdText = 'zmdText';
  SImage = 'Image';
  SZmdEdit = 'zmdEdit';
  SZMDlg19 = 'ZMDlg_A%d';
  { Maximum no. of characters in a password; Do not change! }
  PWLEN = 80;

  { TMsgDlgBtn = (
    mbYes,
   mbNo,
   mbOK,
   mbCancel,
   mbAbort,
   mbRetry,
   mbIgnore,
   mbAll,
   mbNoToAll,
   mbYesToAll,
   mbHelp,
   mbClose
   ); }

type
{$IFDEF UNICODE}
  TZWideLabel = TLabel;
{$ELSE}

  TZWideLabel = class(TLabel)
  private
    WideText: WideString;
    procedure SetCaption(Value: WideString);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    property Caption: WideString read WideText write SetCaption;
  end;

procedure TZWideLabel.DoDrawText(var Rect: TRect; Flags: Longint);
begin
  Canvas.Font := Font;

  DrawTextW(Canvas.Handle, PWideChar(WideText), Length(WideText), Rect, Flags);
end;

procedure TZWideLabel.SetCaption(Value: WideString);
begin
  WideText := Value;
  Invalidate; // repaint
end;
{$ENDIF}

constructor TZipDialogBox.CreateNew2(Owner: TComponent; Context: Integer);
const
  IconIDs: array [0 .. 4] of PChar = (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK,
    IDI_QUESTION, nil);
  BeepIDs: array [0 .. 4] of Integer = (MB_ICONEXCLAMATION, MB_ICONHAND,
    MB_ICONASTERISK, MB_ICONQUESTION, 0);
var
  Buf: array [0 .. 65] of Char;
  I: Integer;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(Owner, 0);
  NonClientMetrics.CbSize := Sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.LfMessageFont);
  Ctx := Context;
  if DlgType = 0 then
    Ctx := Ctx or ZmtWarning;
  for I := 0 to 25 do
  begin
    Buf[I] := Char(Ord('A') + I);
    Buf[I + 27] := Char(Ord('a') + I);
  end;
  Buf[26] := ' ';
  Buf[52] := ' ';
  for I := 53 to 63 do
    Buf[I] := Char(Ord('0') + I - 53);
  Buf[64] := #0;
  GetTextExtentPoint(Canvas.Handle, Buf, 64, TSize(AvDlgUnits));
  AvDlgUnits.X := AvDlgUnits.X div 64;
  I := (DlgType shr 16) and 7;
  if I > 4 then
    I := 4;
  IconID := IconIDs[I];
  BeepId := BeepIDs[I];
end;

procedure TZipDialogBox.Build(const Title, Msg: string; Btns: TMsgDlgButtons;
  const Core: TZMCore);
const
  KHMargin = 8;
  KVMargin = 8;
  KHSpacing = 10;
  KVSpacing = 10;
  KBWidth = 50;
  KBHeight = 14;
  KBSpacing = 4;
  ModalResults: array [TMsgDlgBtn] of Integer = (MrYes, MrNo, MrOk, MrCancel,
    MrAbort, MrRetry, MrIgnore, MrAll, MrNoToAll, MrYesToAll, 0
{$IFDEF UNICODE}, 0 {$ENDIF});
  MsgDlgBtnIds: array [TMsgDlgBtn] of Integer = (ZC_Yes, ZC_No, ZC_OK,
    ZC_Cancel, ZC_Abort, ZC_Retry, ZC_Ignore, ZC_CancelAll, ZC_NoToAll,
    ZC_YesToAll, 0
{$IFDEF UNICODE}, 0 {$ENDIF});
var
  ALeft: Integer;
  B: TMsgDlgBtn;
  BHeight: Integer;
  BSpacing: Integer;
  ButtonCount: Integer;
  ButtonGroupWidth: Integer;
  BWidth: Integer;
  CancelButton: TMsgDlgBtn;
  CHeight: Integer;
  CWidth: Integer;
  DefaultButton: TMsgDlgBtn;
  DxText: TZWideLabel;
  HMargin: Integer;
  HSpacing: Integer;
  I: Integer;
  IconTextHeight: Integer;
  IconTextWidth: Integer;
  N: TButton;
  TabOrdr: Integer;
  TextRect: TRect;
  Tx: Integer;
  VMargin: Integer;
  VSpacing: Integer;
  Wdth: Integer;
{$IFDEF UNICODE}
  Wmsg: string;
{$ELSE}
  Wmsg: WideString;
{$ENDIF}
  X: Integer;
  Y: Integer;
begin
  BiDiMode := Application.BiDiMode;
  BorderStyle := BsDialog;
  Canvas.Font := Font;
  if Title = '' then
    Caption := Application.Title
  else
    Caption := Title;
{$IFNDEF UNICODE}
  if UsingUtf8 then
    Wmsg := UTF8ToWide(Msg, -1)
  else
{$ENDIF}
    Wmsg := Msg;
  HMargin := MulDiv(KHMargin, AvDlgUnits.X, 4);
  VMargin := MulDiv(KVMargin, AvDlgUnits.Y, 8);
  HSpacing := MulDiv(KHSpacing, AvDlgUnits.X, 4);
  VSpacing := MulDiv(KVSpacing, AvDlgUnits.Y, 8);
  BWidth := MulDiv(KBWidth, AvDlgUnits.X, 4);
  if MbOK in Btns then
    DefaultButton := MbOK
  else
    if MbYes in Btns then
      DefaultButton := MbYes
    else
      DefaultButton := MbRetry;
  if MbCancel in Btns then
    CancelButton := MbCancel
  else
    if MbNo in Btns then
      CancelButton := MbNo
    else
      CancelButton := MbOK;
  ButtonCount := 0;
  TabOrdr := 1;
  if DlgType = ZmtPassword then
    TabOrdr := 2;
  for B := low(TMsgDlgBtn) to high(TMsgDlgBtn) do
    if (B < {>} MbHelp) and (B in Btns) then
    begin
      Inc(ButtonCount);
      N := TButton.Create(Self);
      // with N do
      begin
        N.Name := Format(SZMDlg19, [ButtonCount]);
        N.Parent := Self;
        N.Caption := Core.ZipLoadStr(MsgDlgBtnIds[B]);
        N.ModalResult := ModalResults[B];
        if B = DefaultButton then
          N.Default := True;
        if B = CancelButton then
          N.Cancel := True;
        N.TabStop := True;
        N.TabOrder := TabOrdr;
        Inc(TabOrdr);
      end;
      TextRect := Rect(0, 0, 0, 0);
{$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.DrawText(Canvas.Handle,
        PChar(N.Caption), -1, TextRect, DT_CALCRECT or DT_LEFT or
        DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
      Wdth := TextRect.Right - TextRect.Left + 8;
      if Wdth > BWidth then
        BWidth := Wdth;
    end;
  BHeight := MulDiv(KBHeight, AvDlgUnits.Y, 8);
  BSpacing := MulDiv(KBSpacing, AvDlgUnits.X, 4);
  SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
  DrawTextW(Canvas.Handle, PWideChar(Wmsg), Length(Wmsg) + 1, TextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
    DrawTextBiDiModeFlagsReadingOnly);
  IconTextWidth := TextRect.Right;
  IconTextHeight := TextRect.Bottom;
  if IconID <> nil then
  begin
    Inc(IconTextWidth, 32 + HSpacing);
    if IconTextHeight < 32 then
      IconTextHeight := 32;
  end;
  ButtonGroupWidth := 0;
  if ButtonCount <> 0 then
    ButtonGroupWidth := BWidth * ButtonCount + BSpacing * (ButtonCount - 1);
  if IconTextWidth > ButtonGroupWidth then
    CWidth := IconTextWidth
  else
    CWidth := ButtonGroupWidth;
  CHeight := IconTextHeight + BHeight;
  if DlgType = ZmtPassword then
  begin
    if CWidth < (PWLEN * AvDlgUnits.X) then
      CWidth := PWLEN * AvDlgUnits.X;
    PwdEdit := TEdit.Create(Self);
    with PwdEdit do
    begin
      Name := SZmdEdit;
      Text := '';
      Parent := Self;
      PasswordChar := '*';
      MaxLength := PWLEN;
      TabOrder := 1;
      TabStop := True;
      BiDiMode := Self.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HMargin;
      if UseRightToLeftAlignment then
        ALeft := CWidth - ALeft - Width;
      Tx := PWLEN * AvDlgUnits.X;
      if Tx < TextRect.Right then
        Tx := TextRect.Right;
      SetBounds(ALeft, IconTextHeight + VMargin + VSpacing, Tx, 15);
    end;
    ActiveControl := PwdEdit;
    CHeight := CHeight + PwdEdit.Height + VMargin;
  end;
  ClientWidth := CWidth + (HMargin * 2);
  ClientHeight := CHeight + VSpacing + VMargin * 2;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  if IconID <> nil then
    with TImage.Create(Self) do
    begin
      Name := SImage;
      Parent := Self;
      Picture.Icon.Handle := LoadIcon(0, IconID);
      SetBounds(HMargin, VMargin, 32, 32);
    end;
  DxText := TZWideLabel.Create(Self);
  with DxText do
  begin
    Name := SZmdText;
    Parent := Self;
    WordWrap := True;
    Caption := Wmsg;
    BoundsRect := TextRect;
    BiDiMode := Self.BiDiMode;
    ALeft := IconTextWidth - TextRect.Right + HMargin;
    if UseRightToLeftAlignment then
      ALeft := Self.ClientWidth - ALeft - Width;
    SetBounds(ALeft, VMargin, TextRect.Right, TextRect.Bottom);
  end;
  X := (ClientWidth - ButtonGroupWidth) div 2;
  Y := IconTextHeight + VMargin + VSpacing;
  if DlgType = ZmtPassword then
    Inc(Y, PwdEdit.Height + VSpacing);
  for I := 0 to Pred(ComponentCount) do
    if Components[I] is TButton then
      with Components[I] as TButton do
      begin
        SetBounds(X, Y, BWidth, BHeight);
        Inc(X, BWidth + BSpacing);
      end;
end;

function TZipDialogBox.GetDlgType: Integer;
begin
  Result := Ctx and $1F0000;
end;

function TZipDialogBox.GetPWrd: string;
begin
  if Assigned(PwdEdit) then
    Result := PwdEdit.Text
  else
    Result := '';
end;

procedure TZipDialogBox.SetPwrd(const Value: string);
begin
  if Assigned(PwdEdit) and (Value <> PwdEdit.Text) then
    PwdEdit.Text := Value;
end;

function TZipDialogBox.ShowModal: Integer;
begin
  if BeepId <> 0 then
    MessageBeep(BeepId);
  Result := inherited ShowModal;
end;

end.
