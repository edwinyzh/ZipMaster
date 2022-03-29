Unit sfxunit;
(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

   This file is part of TZipMaster Version 1.9.2
   
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

{$INCLUDE 'ZipVers.inc'}

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ZipMstr, Buttons, ComCtrls, ShlObj, ShellAPI, ImgList;

Type
  TMakeSFX = Class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    CmdLineCB: TCheckBox;
    FileListCB: TCheckBox;
    HideOverWriteCB: TCheckBox;
    AutoRunCB: TCheckBox;
    NoMsgShowCB: TCheckBox;
    DfltOverWriteGrp: TRadioGroup;
    OkCancelRB: TRadioButton;
    YesNoRB: TRadioButton;
    OkBttnRB: TRadioButton;
    DefIconRB: TRadioButton;
    AutoIconRB: TRadioButton;
    OrigIconRB: TRadioButton;
    ExecBut: TButton;
    CancelBut: TButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    SFXPage: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label9: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    IconIndexLabel: TLabel;
    MsgEdit: TEdit;
    CaptionEdit: TEdit;
    CommandEdit: TEdit;
    IconEdit: TEdit;
    IconIndexEdit: TEdit;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    IconIndexUD: TUpDown;
    DirectoryEdit: TComboBox;

    Procedure FormShow(Sender: TObject);
    Procedure ExecButClick(Sender: TObject);
    Procedure BitBtn1Click(Sender: TObject);
    Procedure BitBtn2Click(Sender: TObject);
    Procedure CancelButClick(Sender: TObject);
    Procedure AutoRunCBClick(Sender: TObject);
    Procedure DefIconRBClick(Sender: TObject);
    Procedure IconEditKeyPress(Sender: TObject; Var Key: Char);
    Procedure IconIndexUDClick(Sender: TObject; Button: TUDBtnType);

  PUBLIC
    Procedure LoadCustomIcon(IconPath: String; IconIndex: Integer);
  End;

Var
  MakeSFX: TMakeSFX;
  IconDir: String;

Implementation

Uses mainunit;
{$R *.DFM}

Procedure TMakeSFX.ExecButClick(Sender: TObject);
Begin
  With Mainform.ZipMaster1 Do
  Begin
    If CmdLineCB.Checked Then
      SFXOptions := SFXOptions + [soAskCmdLine]
    Else
      SFXOptions := SFXOptions - [soAskCmdLine];
    If FileListCB.Checked Then
      SFXOptions := SFXOptions + [soAskFiles]
    Else
      SFXOptions := SFXOptions - [soAskFiles];
    If HideOverWriteCB.Checked Then
      SFXOptions := SFXOptions + [soHideOverWriteBox]
    Else
      SFXOptions := SFXOptions - [soHideOverWriteBox];
    If AutoRunCB.Checked Then
      SFXOptions := SFXOptions + [soAutoRun]
    Else
      SFXOptions := SFXOptions - [soAutoRun];
    If NoMsgShowCB.Checked Then
      SFXOptions := SFXOptions + [soNoSuccessMsg]
    Else
      SFXOptions := SFXOptions - [soNoSuccessMsg];

    If DfltOverWriteGrp.ItemIndex = 0 Then
      SFXOverWriteMode := ovrConfirm;
    If DfltOverWriteGrp.ItemIndex = 1 Then
      SFXOverWriteMode := ovrAlways;
    If DfltOverWriteGrp.ItemIndex = 2 Then
      SFXOverWriteMode := ovrNever;

    SFXCaption := CaptionEdit.Text;
    SFXDefaultDir := DirectoryEdit.Text;
    SFXCommandLine := CommandEdit.Text;
    SFXMessage := '';
    If OkCancelRB.Checked Then
      SFXMessage := #1;
    If YesNoRB.Checked Then
      SFXMessage := #2;
    SFXMessage := SFXMessage + MsgEdit.Text;
  End;
  Mainform.DoIt := True;
  Close;
End;

Procedure TMakeSFX.CancelButClick(Sender: TObject);
Begin
  Mainform.DoIt := False;
  Close;
End;

Procedure TMakeSFX.BitBtn1Click(Sender: TObject);
Var
  TempStr: String;
Begin
  TempStr := DirectoryEdit.Text;
  Mainform.AskDirDialog(MakeSFX.Handle, TempStr);
  DirectoryEdit.Text := TempStr;
End;

Procedure TMakeSFX.FormShow(Sender: TObject);
Begin
  With Mainform.ZipMaster1 Do
  Begin
    If soAskCmdLine In SFXOptions Then
      CmdLineCB.Checked := True
    Else
      CmdLineCB.Checked := False;
    If soAskFiles In SFXOptions Then
      FileListCB.Checked := True
    Else
      FileListCB.Checked := False;
    If soHideOverWriteBox In SFXOptions Then
      HideOverWriteCB.Checked := True
    Else
      HideOverWriteCB.Checked := False;
    If soAutoRun In SFXOptions Then
      AutoRunCB.Checked := True
    Else
      AutoRunCB.Checked := False;
    If soNoSuccessMsg In SFXOptions Then
      NoMsgShowCB.Checked := True
    Else
      NoMsgShowCB.Checked := False;

    Case SFXOverWriteMode Of
      ovrConfirm:
        DfltOverWriteGrp.ItemIndex := 0;
      ovrAlways:
        DfltOverWriteGrp.ItemIndex := 1;
      ovrNever:
        DfltOverWriteGrp.ItemIndex := 2;
    End;

    CaptionEdit.Text := SFXCaption;
    DirectoryEdit.Text := SFXDefaultDir;
    CommandEdit.Text := SFXCommandLine;
    If (Length(SFXMessage) > 0) And (SFXMessage[2] = '|') Then
//      ((SFXMessage[1] = #1) Or (SFXMessage[1] = #2)) Then
    Begin
      If SFXMessage[1] = '1'{#1} Then
        OkCancelRB.Checked := True
      Else
        YesNoRB.Checked := True;
      MsgEdit.Text := Copy(SFXMessage, 3{2}, Length(SFXMessage) - 2{1});
    End
    Else
    Begin
      MsgEdit.Text := SFXMessage;
      OkBttnRB.Checked := True;
    End;
  End;

  If ImageList1.Count = 3 Then
    ImageList1.Delete(2);
  If IconEdit.Text = '' Then
  Begin
    If (Mainform.ZipMaster1.SFXIcon = nil) or Mainform.ZipMaster1.SFXIcon.Empty Then
    Begin
      ImageList1.GetIcon(Integer(AutoRunCB.Checked), Image1.Picture.Icon);
      Mainform.ZipMaster1.SFXIcon := Image1.Picture.Icon;
    End
    Else
    Begin
      ImageList1.AddIcon(Mainform.ZipMaster1.SFXIcon);
      OrigIconRB.Checked := True;
    End;
  End;
  If ImageList1.Count = 3 Then
    OrigIconRB.Enabled := True
  Else
    OrigIconRB.Enabled := False
End;

Procedure TMakeSFX.BitBtn2Click(Sender: TObject);
Begin
  If IconDir = '' Then
    Mainform.GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, IconDir);
  With OpenDialog1 Do
  Begin
    InitialDir := IconDir;
    If Execute Then
    Begin
      IconDir := ExtractFilePath(FileName);
      IconEdit.Text := FileName;
      DefIconRB.Checked := False;
      AutoIconRB.Checked := False;
      If UpperCase(ExtractFileExt(FileName)) <> '.ICO' Then
      Begin
        LoadCustomIcon(FileName, StrToIntDef(IconIndexEdit.Text, 0));
        IconIndexLabel.Enabled := True;
        IconIndexEdit.Enabled := True;
        IconIndexUD.Enabled := True;
        IconIndexEdit.Color := clWindow;
      End
      Else
      Begin
        Image1.Picture.Icon.LoadFromFile(FileName);
        IconIndexEdit.Text := '0';
        IconIndexLabel.Enabled := False;
        IconIndexEdit.Enabled := False;
        IconIndexUD.Enabled := False;
        IconIndexEdit.Color := clBtnFace;
        Mainform.ZipMaster1.SFXIcon := Image1.Picture.Icon;
      End;
    End;
  End;
End;

Procedure TMakeSFX.AutoRunCBClick(Sender: TObject);
Begin
  If IconEdit.Text = '' Then
  Begin
    ImageList1.GetIcon(Integer(AutoRunCB.Checked), Image1.Picture.Icon);
    Mainform.ZipMaster1.SFXIcon := Image1.Picture.Icon;
  End;
End;

Procedure TMakeSFX.DefIconRBClick(Sender: TObject);
Begin
  ImageList1.GetIcon(TRadioButton(Sender).Tag, Image1.Picture.Icon);
  Mainform.ZipMaster1.SFXIcon := Image1.Picture.Icon;
  IconEdit.Text := '';
  IconIndexEdit.Text := '0';
  IconIndexLabel.Enabled := False;
  IconIndexEdit.Enabled := False;
  IconIndexUD.Enabled := False;
  IconIndexEdit.Color := clBtnFace;
End;

Procedure TMakeSFX.LoadCustomIcon(IconPath: String; IconIndex: Integer);
Var
  IconHandle: THandle;
Begin
  Image1.Picture.Icon.ReleaseHandle();
  Mainform.ZipMaster1.SFXIcon.ReleaseHandle();
  IconHandle := ExtractIcon(hInstance, PChar(IconPath), IconIndex);
  If (IconHandle <> 0) And (IconHandle <> 1) Then
  Begin
    Image1.Picture.Icon.Handle := IconHandle;
    Mainform.ZipMaster1.SFXIcon.Handle := IconHandle;
  End;
End;

Procedure TMakeSFX.IconIndexUDClick(Sender: TObject; Button: TUDBtnType);
Begin
  LoadCustomIcon(IconEdit.Text, StrToIntDef(IconIndexEdit.Text, 0));
End;

Procedure TMakeSFX.IconEditKeyPress(Sender: TObject; Var Key: Char);
Begin
  If FileExists(IconEdit.Text) Then
    LoadCustomIcon(IconEdit.Text, StrToIntDef(IconIndexEdit.Text, 0));
End;

End.
