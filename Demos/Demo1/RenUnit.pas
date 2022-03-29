Unit renunit;
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

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ZipMstr, Grids, SortGrid;

Type
  TRenForm = Class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    OldCombo: TComboBox;
    NewEdit: TEdit;
    DTEdit: TEdit;
    AddBtn: TButton;
    RemoveBtn: TButton;
    DTAllBtn: TButton;

    Procedure FormShow(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure AddBtnClick(Sender: TObject);
    Procedure DTAllBtnClick(Sender: TObject);
    Procedure OkBitBtnClick(Sender: TObject);
    Procedure OldComboClick(Sender: TObject);
    Procedure RemoveBtnClick(Sender: TObject);
    Procedure CancelBitBtnClick(Sender: TObject);
    Procedure SelectedGridGetCellFormat(Sender: TObject; Col, Row: Integer;
      State: TGridDrawState; Var FormatOptions: TFormatOptions);

  PRIVATE { Private declarations }
    Procedure ClearZipRenList;

  PUBLIC { Public declarations }
    ZipRenameList: TList;
    SelectedGrid: TSortGrid;
    GDateTime: Integer;
  End;

Var
  RenForm: TRenForm;

Implementation

Uses mainunit, msgunit;
{$R *.DFM}

Procedure TRenForm.FormShow(Sender: TObject);
Var
  i: Integer;
Begin
  GDateTime := 0;
  SelectedGrid.RowCount := 2;
  SelectedGrid.Rows[1].Clear();
  RenForm.Caption := 'Rename items in zip archive: ' + ExtractFileName
    (MainForm.ZipMaster1.ZipFileName);
  OldCombo.Clear();
  ClearZipRenList();
  For i := 1 To MainForm.ZipMaster1.Count Do
  Begin
    With MainForm.ZipMaster1[i - 1] Do
    Begin
      OldCombo.Items.Add(FileName);
    End;
  End;
End;

Procedure TRenForm.AddBtnClick(Sender: TObject);
Var
  AddItem: String;
  RenRec: PZMRenameRec;
Begin
  AddItem := OldCombo.Text;
  If (AddItem <> NewEdit.Text) Or (DTEdit.Text <> '') Then
  Begin
    If (Length(AddItem) > 0) And (Length(NewEdit.Text) > 0) Then
    Begin
      If (SelectedGrid.RowCount > 2) Or ((SelectedGrid.RowCount = 2) And
          (SelectedGrid.Cells[0, 1] <> '')) Then
        SelectedGrid.RowCount := SelectedGrid.RowCount + 1;
      New(RenRec);
      RenRec^.Source := AddItem;
      RenRec^.Dest := NewEdit.Text;
      Try
        If DTEdit.Text <> '' Then
          RenRec^.DateTime := DateTimeToFileDate(StrToDateTime(DTEdit.Text));
      Except
        Else
          Dispose(RenRec);
          Raise ;
        End;
        ZipRenameList.Add(RenRec);
        SelectedGrid.Cells[0, SelectedGrid.RowCount - 1] := AddItem;
        SelectedGrid.Cells[1, SelectedGrid.RowCount - 1] := NewEdit.Text;
        SelectedGrid.Cells[2, SelectedGrid.RowCount - 1] := DTEdit.Text;
      End;
      OldCombo.Text := '';
      NewEdit.Text := '';
      DTEdit.Text := '';
    End;
  End;

  Procedure TRenForm.FormCreate(Sender: TObject);
  Begin
    ZipRenameList := TList.Create();
    SelectedGrid := TSortGrid.create(self);
    SelectedGrid.Parent := self;      
    SelectedGrid.Left := 16;
    SelectedGrid.Top := 16;
    SelectedGrid.Width := 417;
    SelectedGrid.Height := 93;
    SelectedGrid.Color := clSilver;
    SelectedGrid.ColCount := 3;
    SelectedGrid.DefaultColWidth := 198;
    SelectedGrid.DefaultRowHeight := 16;
    SelectedGrid.FixedCols := 0;
    SelectedGrid.RowCount := 2;
    SelectedGrid.Font.Color := clWindowText;
    SelectedGrid.Font.Height := -11;
    SelectedGrid.Font.Name := 'Arial';
    SelectedGrid.Font.Style := [];
    SelectedGrid.Options := [goFixedVertLine, goFixedHorzLine, goRangeSelect, goColSizing, goRowSelect];
    SelectedGrid.ParentFont := False;
    SelectedGrid.ParentShowHint := False;
    SelectedGrid.ShowHint := False;
    SelectedGrid.TabOrder := 0;
    SelectedGrid.CaseSensitive := False;
    SelectedGrid.AlignmentHorz := taLeftJustify;
    SelectedGrid.AlignmentVert := taTopJustify;
    SelectedGrid.ProportionalScrollBars := True;
    SelectedGrid.ExtendedKeys := False;
    SelectedGrid.SortOnClick := True;
    SelectedGrid.FooterFont.Color := clWindowText;
    SelectedGrid.FooterFont.Height := -11;
    SelectedGrid.FooterFont.Name := 'MS Sans Serif';
    SelectedGrid.FooterFont.Style := [];
    SelectedGrid.ColWidths[0] := 154;
    SelectedGrid.ColWidths[1] := 160;
    SelectedGrid.ColWidths[2] := 96;
    SelectedGrid.Cells[0, 0] := 'Old name';
    SelectedGrid.Cells[1, 0] := 'New name';
    SelectedGrid.Cells[2, 0] := 'Date/Time';
  End;

  Procedure TRenForm.FormDestroy(Sender: TObject);
  Begin
    ClearZipRenList();
    ZipRenameList.Free();
  End;

  Procedure TRenForm.ClearZipRenList();
  Var
    i: Integer;
    RenRec: PZMRenameRec;
  Begin
    For i := 0 To ZipRenameList.Count - 1 Do
    Begin
      RenRec := ZipRenameList.Items[i];
      Dispose(RenRec);
    End;
    ZipRenameList.Clear();
  End;

  Procedure TRenForm.RemoveBtnClick(Sender: TObject);
  Var
    i, j: Integer;
    RenRec: PZMRenameRec;
  Begin
    j := SelectedGrid.Selection.Top;
    For i := SelectedGrid.Selection.Bottom Downto j Do
    Begin
      If SelectedGrid.Cells[0, i] <> '' Then
      Begin
        RenRec := ZipRenameList.Items[i - 1];
        ZipRenameList.Delete(i - 1);
        Dispose(RenRec);
        SelectedGrid.Rows[i].Clear();
        If i <> 1 Then
          SelectedGrid.DeleteRow(i);
      End;
    End;
  End;

  Procedure TRenForm.CancelBitBtnClick(Sender: TObject);
  Begin
    Hide();
  End;

  Procedure TRenForm.OkBitBtnClick(Sender: TObject);
  Var
    RenameErr: Integer;
  Begin
    AddBtnClick(Sender);

    MsgForm.Memo1.Clear();
    MsgForm.Show();
    { Put this message into the message form's memo }
    MainForm.ZipMaster1Message(Self, 0,
      'Begin renaming entries in: ' + MainForm.ZipMaster1.ZipFileName);

    RenameErr := MainForm.ZipMaster1.Rename(ZipRenameList, GDateTime);
    If RenameErr <> 0 Then
      ShowMessage('Error ' + IntToStr(RenameErr) +
          ' occured in rename zip specification(s)');
    MsgForm.Hide();
    Hide();
  End;

  Procedure TRenForm.OldComboClick(Sender: TObject);
  Begin
    NewEdit.Text := OldCombo.Items.Strings[OldCombo.ItemIndex];
    NewEdit.SetFocus();
    NewEdit.SelStart := Length(NewEdit.Text);
  End;

  Procedure TRenForm.SelectedGridGetCellFormat
    (Sender: TObject; Col, Row: Integer; State: TGridDrawState;
    Var FormatOptions: TFormatOptions);
  Begin
    If Row = 0 Then
    Begin
      FormatOptions.AlignmentHorz := taCenter;
      FormatOptions.Font.Style := FormatOptions.Font.Style + [fsBold];
      FormatOptions.Font.Color := clBlue;
    End;
  End;

  Procedure TRenForm.DTAllBtnClick(Sender: TObject);
  Begin
    If DTEdit.Text <> '' Then
      GDateTime := DateTimeToFileDate(StrToDateTime(DTEdit.Text))
    Else
      GDateTime := 0;
  End;

End.
