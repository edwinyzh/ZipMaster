Unit mainunit;

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
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, SortGrid, ZipMstr, Menus, ShlObj, FileCtrl,
  ImgList, printers;

Type
  TMainform = Class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    ZipFName: TLabel;
    TimeLabel: TLabel;
    FilesLabel: TLabel;
    MsgBut: TButton;
    CloseBut: TButton;
    NewZipBut: TButton;
    ZipOpenBut: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Project1: TMenuItem;
    Zipcomment1: TMenuItem;
    Showlasterror1: TMenuItem;
    ImageList1: TImageList;
    Options1: TMenuItem;
    MergeOptions1: TMenuItem;
    OptConfirm: TMenuItem;
    OptAlways: TMenuItem;
    OptNewer: TMenuItem;
    OptOlder: TMenuItem;
    OptNever: TMenuItem;
    OptRename: TMenuItem;
    Unattended1: TMenuItem;
    N1: TMenuItem;
    Verbose1: TMenuItem;
    Trace1: TMenuItem;
    MergeBut: TButton;
    N2: TMenuItem;
    Latesttime1: TMenuItem;
    ForceDestination1: TMenuItem;
    Writesafe1: TMenuItem;

    Procedure ZipOpenButClick(Sender: TObject);
    Procedure CloseButClick(Sender: TObject);
    Procedure NewZipButClick(Sender: TObject);
    Procedure ZipMaster1DirUpdate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FillGrid;
    Procedure MsgButClick(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure Zipcomment1Click(Sender: TObject);
    Procedure Showlasterror1Click(Sender: TObject);
    Procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure MergeButClick(Sender: TObject);
  Procedure StringGrid1GetCellFormat(Sender: TObject; Col, Row: LongInt;
    State: TGridDrawState; Var FormatOptions: TFormatOptions);
    Procedure StringGrid1EndSort(Sender: TObject; Col: LongInt);
{$IFNDEF VERD6up}
  procedure CheckClick(Sender: TObject);
  procedure RadioClick(Sender: TObject);
{$ENDIF}
  private
    AutoRun: Integer;
    procedure DoAutoLoad;
    procedure FixForm;
    procedure ShowMsgForm(Sheet: Integer; Erase: Boolean);
  PUBLIC
    { Public declarations }
    DoIt: Boolean;
    TotUncomp, TotComp: Cardinal;
    StringGrid1: TSortGrid;
    ZipMaster1: TZipMaster;

    Function ShowLTime(s, f: LongInt): String;
    Procedure SetZipFName(aCaption: String; AssignName: Boolean);
    Function GetSpecialFolder(aFolder: Integer; Var Location: String): LongWord;
    Procedure SetZipTotals;
    Function AskDirDialog(Const FormHandle: HWND; Var DirPath: String): Boolean;
    procedure ZipMaster1Message(Sender: TObject; ErrCode: Integer;
      const Message: String);
    procedure ZipMaster1Progress(Sender: TObject; details: TZMProgressDetails);
    procedure ZipMaster1Tick(Sender: TObject);
  PROTECTED
    FNewCount: Integer;
    function DoTheMerge: Integer;
  End;

Var
  Mainform: TMainform;
  ExtractDir: String;
  ExpandDirs: Boolean;
  OverWr: Boolean;
  AllFiles: Boolean;
  Canceled: Boolean;

Implementation

Uses msgunit, Mergeunit, FilterUnit;
{$R *.DFM}

Procedure TMainform.FormCreate(Sender: TObject);
Begin                                  
  ZipMaster1 := TZipMaster.Create(Self);
  StringGrid1 := TSortGrid.Create(Self);
  StringGrid1.Parent := Self;
  StringGrid1.Left := 0;
  StringGrid1.Top := 125;
  StringGrid1.Width := 612;
  StringGrid1.Height := 247;
  StringGrid1.Align := alClient;
  StringGrid1.ColCount := 6;
  StringGrid1.DefaultRowHeight := 22;
  StringGrid1.FixedCols := 0;
  StringGrid1.RowCount := 8;
  StringGrid1.Font.Charset := DEFAULT_CHARSET;
  StringGrid1.Font.Color := clBlack;
  StringGrid1.Font.Height := -12;
  StringGrid1.Font.Name := 'Arial';
  StringGrid1.Font.Style := [];
  StringGrid1.Options := [goFixedVertLine, goFixedHorzLine, goHorzLine, goRangeSelect,
    goColSizing, goRowSelect, goThumbTracking];
  StringGrid1.ColWidths[0] := 178;
  StringGrid1.ColWidths[1] := 91;
  StringGrid1.ColWidths[2] := 105;
  StringGrid1.ColWidths[3] := 108;
  StringGrid1.ColWidths[4] := 53;
  StringGrid1.ColWidths[5] := 251;
  StringGrid1.ParentFont := False;
  StringGrid1.TabOrder := 1;
  StringGrid1.CaseSensitive := False;
  StringGrid1.AlignmentHorz := taLeftJustify;
  StringGrid1.AlignmentVert := taTopJustify;
  StringGrid1.ProportionalScrollBars := True;
  StringGrid1.ExtendedKeys := False;
  StringGrid1.SortSymbol := sgGlyph;
  StringGrid1.SortColumn := 0;
  StringGrid1.SortOnClick := True;
  StringGrid1.FooterRows := 1;
  StringGrid1.FooterFont.Charset := DEFAULT_CHARSET;
  StringGrid1.FooterFont.Color := clWindowText;
  StringGrid1.FooterFont.Height := -11;
  StringGrid1.FooterFont.Name := 'MS Sans Serif';
  StringGrid1.FooterFont.Style := [fsBold];
  StringGrid1.PrintOptions.Orientation := poPortrait;
  StringGrid1.PrintOptions.PageTitleMargin := 0;
  StringGrid1.PrintOptions.PageFooter := 'date|time|page';
  StringGrid1.PrintOptions.HeaderSize := 10;
  StringGrid1.PrintOptions.FooterSize := 7;
  StringGrid1.PrintOptions.DateFormat := 'd-mmm-yyyy';
  StringGrid1.PrintOptions.TimeFormat := 'h:nn';
  StringGrid1.PrintOptions.FromRow := 0;
  StringGrid1.PrintOptions.ToRow := 0;
  StringGrid1.PrintOptions.BorderStyle := bsNone;
  StringGrid1.PrintOptions.MarginBottom := 0;
  StringGrid1.PrintOptions.MarginLeft := 0;
  StringGrid1.PrintOptions.MarginTop := 0;
  StringGrid1.PrintOptions.MarginRight := 0;
  StringGrid1.WordWrap := False;
  StringGrid1.OnGetCellFormat := StringGrid1GetCellFormat;
  StringGrid1.OnEndSort := StringGrid1EndSort;

  { Make sure "goColMoving" is false in object inspector. This lets the
    TSortGrid use Mouse Clicks on the col headers. }
  StringGrid1.RowCount := 2; { first row is fixed, and used for column headers }
  StringGrid1.Cells[0, 0] := 'File Name';
  StringGrid1.Cells[1, 0] := 'Compr. Size';
  StringGrid1.Cells[2, 0] := 'Uncmpr. Size';
  StringGrid1.Cells[3, 0] := 'Date/Time';
  StringGrid1.Cells[4, 0] := 'Ratio';
  StringGrid1.Cells[5, 0] := 'Path';

  FixForm; // adjust form for different compilers
  // Set up component
//  ZipMaster1 := TZipMaster_A.Create(Self);
  ZipMaster1.Active := True;
  ZipMaster1.OnMessage := ZipMaster1Message;
  ZipMaster1.OnProgress := ZipMaster1Progress;
  ZipMaster1.OnTick := ZipMaster1Tick;
  ZipMaster1.OnDirUpdate := ZipMaster1DirUpdate;

  { If we had args on the cmd line, then try to open the first one
    as a zip/exe file.  This is most useful in case user has an association
    to ".zip" that causes this program to run when user dble clicks on a zip
    file in Explorer. }

    AutoRun := -1;
//  If ParamCount > 0 Then
//    StartZipName := ParamStr(1); // load it when first shows
End;

Procedure TMainform.FormResize(Sender: TObject);
Begin
  If Width - 291 > 0 Then
    ZipFName.Width := Width - 291
  Else
    ZipFName.Width := 0;
  SetZipFName(ZipMaster1.ZipFilename, False);
End;

Procedure TMainform.CloseButClick(Sender: TObject);
Begin
  Close;
End;

//Procedure TMainform.FormDestroy(Sender: TObject);
//Begin
//// TODO -c : Don't close if busy _ cancel first
////  ZipMaster1.Dll_Load := False;
//End;

Procedure TMainform.ZipOpenButClick(Sender: TObject);
Var
  FirstDir: String;
Begin
  If FirstDir = '' Then
    GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, FirstDir);
  // With OpenDialog1 Do
  // Begin
  OpenDialog1.InitialDir := FirstDir;
  OpenDialog1.Title := 'Open Existing ZIP File';
  OpenDialog1.Filter := 'ZIP Files (*.ZIP, *.EXE)|*.zip;*.exe';
  OpenDialog1.FileName := '';
  OpenDialog1.Options := OpenDialog1.Options + [ofHideReadOnly, ofShareAware,
    ofPathMustExist, ofFileMustExist];
  If OpenDialog1.Execute Then
  Begin
    FirstDir := ExtractFilePath(OpenDialog1.FileName);
    { Set the caption after assigning the filename. This
      way, the filename will be null if the open failed. }
    SetZipFName(OpenDialog1.FileName, True);
  End;
  // End;
End;

Procedure TMainform.NewZipButClick(Sender: TObject);
Var
  ans: Boolean;
  FirstDir: String;
Begin
  If FirstDir = '' Then
    GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, FirstDir);
//  With OpenDialog1 Do
//  Begin
  OpenDialog1.InitialDir := FirstDir;
  OpenDialog1.FileName := '';
  OpenDialog1.Filter := 'ZIP Files (*.ZIP)|*.zip';
  OpenDialog1.DefaultExt := 'Zip';
  OpenDialog1.Title := 'Create New ZIP File';
  OpenDialog1.Options := OpenDialog1.Options + [ofHideReadOnly, ofShareAware];
  OpenDialog1.Options := OpenDialog1.Options - [ofPathMustExist, ofFileMustExist];
  If OpenDialog1.Execute Then
  Begin
    FirstDir := ExtractFilePath(OpenDialog1.FileName);
    If FileExists(OpenDialog1.FileName) Then
    Begin
      ans := MessageDlg('Overwrite Existing File: ' + OpenDialog1.FileName + '?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes;
      If ans Then
        DeleteFile(OpenDialog1.FileName)
      Else
        Exit; { Don't use the new name }
    End;
    SetZipFName(OpenDialog1.FileName, True);
  End;
//  End;
End;

Procedure TMainform.MsgButClick(Sender: TObject);
Begin
//  MsgForm.Show;
  ShowMsgForm(0, False);
End;

Procedure TMainform.Showlasterror1Click(Sender: TObject);
Begin
  If ZipMaster1.ErrCode <> 0 Then
    ShowMessage(IntToStr(ZipMaster1.ErrCode) + ' ' + ZipMaster1.ErrMessage)
  Else
    ShowMessage('No last error present');
End;

Procedure TMainform.Exit1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TMainform.Zipcomment1Click(Sender: TObject);
Begin
  If ZipMaster1.ZipComment <> '' Then
  Begin
    MsgForm.MsgMemo.Clear;
    MsgForm.MsgMemo.Lines.Add(string(ZipMaster1.ZipComment));
    Msgform.PageControl1.ActivePage := Msgform.MsgSheet;
    MsgForm.Show;
  End
  Else
    ShowMessage('No Zip comment in this zip file');
End;

// ***********************ZipMaster Event handling***************************
// ---------------------------------------------------------------------------

// This is the "OnMessage" event handler
procedure TMainform.ZipMaster1Message(Sender: TObject; ErrCode: Integer;
  const Message: String);
Begin
  MsgForm.MsgMemo.Lines.Append(Message);
  PostMessage(MsgForm.MsgMemo.Handle, EM_SCROLLCARET, 0, 0);
  If (ErrCode > 0) And Not ZipMaster1.Unattended Then
  begin
    if MessageDlg('Error Msg: ' + Message, mtError, [mbOK, mbAbort], 0) <> mrOk then
      ZipMaster1.Cancel := True;
  end;
End;

Procedure TMainform.ZipMaster1DirUpdate(Sender: TObject);
Begin
  FillGrid;
  FilesLabel.Caption := IntToStr(ZipMaster1.Count);
End;

procedure TMainform.ZipMaster1Progress(Sender: TObject; details:
    TZMProgressDetails);
begin
  Case details.Order Of
    TotalSize2Process:
      Begin
        MsgForm.StatusBar1.Panels.Items[0].Text := 'Total size: ' + IntToStr
          (details.TotalSize Div 1024) + ' Kb';
        MsgForm.ProgressBar2.Position := 1;
        MsgForm.ProgressBar1.Max := 100;
        MsgForm.ProgressBar2.Min := 1;
      End;
    TotalFiles2Process:
      Begin
        MsgForm.StatusBar1.Panels.Items[1].Text := IntToStr(details.TotalCount)
          + ' files';
      End;
    NewExtra,
    NewFile:
      Begin
        MsgForm.FileBeingZipped.Caption := details.ItemName;
        MsgForm.ProgressBar1.Position := 1;
      End;
    ExtraUpdate,
    ProgressUpdate:
      Begin
        MsgForm.ProgressBar1.Position := details.ItemPerCent;
        MsgForm.ProgressBar2.Position := details.TotalPerCent;
      End;
    EndOfBatch: // Reset the progress bar and filename.
      Begin
        MsgForm.FileBeingZipped.Caption := '';
        MsgForm.ProgressBar1.Position := 1;
        MsgForm.StatusBar1.Panels[0].Text := '';
        MsgForm.StatusBar1.Panels[1].Text := '';
        MsgForm.ProgressBar2.Position := 1;
      End;
  End;
end;

// ***********************User defined functions *****************************
// ---------------------------------------------------------------------------

Function TMainform.ShowLTime(s, f: LongInt): String;
Var
  min, sec, st: Integer;
  smin, ssec : String;
Begin
  st := f - s;
  sec := st Div 1000;
  min := sec Div 60;
  sec := sec Mod 60;
  If sec > 9 Then
    ssec := IntToStr(sec)
  Else
    ssec := '0' + IntToStr(sec);
  If min > 9 Then
    smin := IntToStr(min)
  Else
    smin := '0' + IntToStr(min);
  Result := smin + ':' + ssec;
End;

Procedure TMainform.SetZipFName(aCaption: String; AssignName: Boolean);
Begin
  // Assigning the filename will cause the table of contents to be read.
  // and possibly reset it to an empty string (If error found).
  If AssignName Then
    ZipMaster1.ZipFilename := aCaption;

  If ZipMaster1.ZipFilename = '' Then
    ZipFName.Caption := AnsiString('<none>')
  Else
    ZipFName.Caption := MinimizeName(ZipMaster1.ZipFilename, ZipFName.Canvas,
      ZipFName.Width);

  If ZipFName.Canvas.TextWidth(ZipMaster1.ZipFilename) > ZipFName.Width Then
  Begin
    ZipFName.Hint := ZipMaster1.ZipFilename;
    ZipFName.ShowHint := True;
  End
  Else
    ZipFName.ShowHint := False;
End;

// ---------------------------------------------------------------------------

Procedure TMainform.SetZipTotals;
Begin
  StringGrid1.Cells[0, StringGrid1.RowCount - 1] := 'Total';
  StringGrid1.Cells[1, StringGrid1.RowCount - 1] := IntToStr(TotComp);
  StringGrid1.Cells[2, StringGrid1.RowCount - 1] := IntToStr(TotUncomp);
  If TotUncomp <> 0 Then
    StringGrid1.Cells[4, StringGrid1.RowCount - 1] := IntToStr
      (Round((1 - (TotComp / TotUncomp)) * 100)) + '% '
  Else
    StringGrid1.Cells[4, StringGrid1.RowCount - 1] := '0 % ';
  StringGrid1.Cells[5, StringGrid1.RowCount - 1] := '';
End;

// ---------------------------------------------------------------------------

Function TMainform.AskDirDialog(Const FormHandle: HWND; Var DirPath: String)
  : Boolean;
Var
  pidl: PItemIDList;
  FBrowseInfo: TBrowseInfo;
  Success: Boolean;
  TitleName: String;
  Buffer: Array [0 .. MAX_PATH] Of Char;
Begin
  Result := False;
  ZeroMemory(@FBrowseInfo, SizeOf(FBrowseInfo));
  Try
    GetMem(FBrowseInfo.pszDisplayName, MAX_PATH);
    FBrowseInfo.hwndOwner := FormHandle;
    TitleName := 'Please specify a directory';
    FBrowseInfo.lpszTitle := PChar(TitleName);
    pidl := ShBrowseForFolder(FBrowseInfo);
    If pidl <> Nil Then
    Begin
      Success := SHGetPathFromIDList(pidl, Buffer);
      // if False then pidl not part of namespace
      If Success Then
      Begin
        DirPath := Buffer;
        If DirPath[Length(DirPath)] <> '\' Then
          DirPath := DirPath + '\';
        Result := True;
      End;
      GlobalFreePtr(pidl);
    End;
  Finally
    If Assigned(FBrowseInfo.pszDisplayName) Then
      FreeMem(FBrowseInfo.pszDisplayName, MAX_PATH);
  End;
End;

// ---------------------------------------------------------------------------
{ * Folder types are a.o.
  *	CSIDL_DESKTOPDIRECTORY, CSIDL_STARTMENU, CSIDL_SENDTO,
  * CSIDL_PROGRAMS, CSIDL_STARTUP etc.
  * }

Function TMainform.GetSpecialFolder(aFolder: Integer; Var Location: String)
  : LongWord;
Var
  pidl: PItemIDList;
  hRes: HRESULT;
  RealPath: Array [0 .. MAX_PATH] Of Char;
  Success: Boolean;
Begin
  Result := 0;
  hRes := SHGetSpecialFolderLocation(Handle, aFolder, pidl);
  If hRes = NO_ERROR Then
  Begin
    Success := SHGetPathFromIDList(pidl, RealPath);
    If Success Then
      Location := String(RealPath) + '\'
    Else
      Result := LongWord(E_UNEXPECTED);
  End
  Else
    Result := hRes;
End;

// **************************Grid functions **********************************
// ---------------------------------------------------------------------------

Procedure TMainform.FillGrid;
Const
  sorts: Array [1 .. 6] Of TSortStyle = (ssAutomatic, ssAlphabetic, ssNumeric,
    ssDateTime, ssTime, ssCustom);
Var
  i: Integer;
  so: TSortOptions;
Begin
  With StringGrid1 Do
  Begin
    { remove everything from grid except col titles }
    RowCount := 2;
    Rows[1].Clear;
    If ZipMaster1.Count = 0 Then
      Exit;

    StringGrid1.RowCount := ZipMaster1.Count + 2;
    TotUncomp := 0;
    TotComp := 0;
    For i := 1 To ZipMaster1.Count Do
    Begin
      With ZipMaster1.DirEntry[i - 1] Do
      Begin
        Cells[0, i] := ExtractFileName(FileName);
        Cells[1, i] := IntToStr(CompressedSize);
        Cells[2, i] := IntToStr(UncompressedSize);
        Cells[3, i] := FormatDateTime('ddddd  t', FileDateToDateTime(DateTime));
        If UncompressedSize <> 0 Then
          Cells[4, i] := IntToStr
            (Round((1 - (CompressedSize / UncompressedSize)) * 100)) + '% '
        Else
          Cells[4, i] := '0% ';
        Cells[5, i] := ExtractFilePath(FileName);
        TotUncomp := TotUncomp + Cardinal(UncompressedSize);
        Inc(TotComp, CompressedSize);
      End; // end with
    End; // end for

    so.SortDirection := sdAscending;
    so.SortStyle := ssAutomatic;
    so.SortCaseSensitive := False;
    SortByColumn(SortColumn, so);
    Row := 1;
  End; // end with
End;

procedure TMainform.FixForm;
begin
{$IFDEF VERD6up}
  Verbose1.AutoCheck := True;
  Trace1.AutoCheck := True;
  Unattended1.AutoCheck := True;
  OptConfirm.AutoCheck := True;
  OptAlways.AutoCheck := True;
  OptNewer.AutoCheck := True;
  OptOlder.AutoCheck := True;
  OptNever.AutoCheck := True;
  OptRename.AutoCheck := True;
  ForceDestination1.AutoCheck := True;
  Writesafe1.AutoCheck := True;
  Latesttime1.AutoCheck := True;
{$ELSE}
  Verbose1.OnClick := CheckClick;
  Trace1.OnClick := CheckClick;
  Unattended1.OnClick := CheckClick;
  OptConfirm.OnClick := RadioClick;
  OptAlways.OnClick := RadioClick;
  OptNewer.OnClick := RadioClick;
  OptOlder.OnClick := RadioClick;
  OptNever.OnClick := RadioClick;
  OptRename.OnClick := RadioClick;
  ForceDestination1.OnClick := CheckClick;
  Writesafe1.OnClick := CheckClick;
  Latesttime1.OnClick := CheckClick;
{$ENDIF}
end;

procedure TMainform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  while ZipMaster1.State >= zsBusy do
  begin
    ZipMaster1.Cancel := True;
    Sleep(2000);
  end;
  CanClose := True;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
  if AutoRun < 0 then
  begin
    AutoRun := 0; // stop checking again
    if ParamCount > 0 then
      DoAutoLoad;
  end;
end;

procedure TMainform.MergeButClick(Sender: TObject);
Var
  IsOne: String;
Begin
  If ZipMaster1.ZipFilename = '' Then
  Begin
    ShowMessage('Error - open a zip file first');
    Exit;
  End;
  MergeForm.Left := Left;
  MergeForm.Top := Top;
  MergeForm.Width := Width;
  MergeForm.Height := Height;
  Canceled := False;
  MergeForm.ShowModal; { let user pick filenames to add }
  If Canceled Then
    Exit;
  // make 'script'
  ZipMaster1.FSpecArgs.Clear;
  ZipMaster1.FSpecArgsExcl.Clear;
  MergeForm.PrepareScript(ZipMaster1.FSpecArgs);
  DoTheMerge;
  If ZipMaster1.SuccessCnt = 1 Then
    IsOne := ' was'
  Else
    IsOne := 's were';
  ShowMessage(IntToStr(ZipMaster1.SuccessCnt) + ' file' + IsOne + ' merged');
end;

Procedure TMainform.StringGrid1EndSort(Sender: TObject; Col: LongInt);
Begin
  SetZipTotals;
End;

Procedure TMainform.StringGrid1GetCellFormat(Sender: TObject; Col, Row: LongInt;
  State: TGridDrawState; Var FormatOptions: TFormatOptions);
Begin
  If (Row <> 0) And (Col <> 0) And (Col <> 5) Then
    FormatOptions.AlignmentHorz := taRightJustify;
End;

(*
Command line
    load file list
  Demo11.exe dest.zip -lfile.txt
    Autorun script (as FSpecArgs)
  Demo11.exe dest.zip -afile.txt
   restrictions - uses 'standard' paramStr and ParamCnt so no embedded spaces
*)
procedure TMainform.DoAutoLoad;
var
  Cmnd: string;
  CmndList: TStringList;
  Err: Integer;
  FileName: string;
begin
  FileName := ParamStr(1);
  if FileName <> '' then
    ZipMaster1.ZipFileName := FileName; // load the command line zip
  if ParamCount = 1 then
    Exit;
  Cmnd := ParamStr(2);
  // is it '-lfilename' ?
  if (Length(Cmnd) < 5) or (Cmnd[1] <> '-') then
    Exit;
  FileName := Copy(Cmnd, 3, 1024);
  if not FileExists(FileName) then
    Exit;
  if (Cmnd[2] = 'l') or (Cmnd[2] = 'L') then
  begin
    if MergeForm.LoadFromFile(FileName) = 0 then
      MergeButClick(Self)
    else
      Exit;
  end;
  if (Cmnd[2] = 'a') or (Cmnd[2] = 'A') then
  begin
//    Err := -1;
    ZipMaster1.FSpecArgs.Clear;
    ZipMaster1.FSpecArgsExcl.Clear;
    CmndList := TStringList.Create;
    try
      CmndList.LoadFromFile(FileName);
      ZipMaster1.FSpecArgs.AddStrings(CmndList);
      Err := DoTheMerge;
    finally
      CmndList.Free;
    end;
    if Err >= 0 then
      CloseButClick(Self); // was good
  end;
end;

function TMainform.DoTheMerge: Integer;
var
  s: LongInt;
  f: LongInt;
  MergeOpts: TZMMergeOpts;
begin
  Result := -1;
  If ZipMaster1.FSpecArgs.Count = 0 Then
  Begin
    ShowMessage('No files selected');
    Exit;
  End;

  FNewCount := 0;
  ShowMsgForm(0, True);  // clear it
//  Msgform.ScriptMemo.AddStrings(ZipMaster1.FSpecArgs);
//  MsgForm.MsgMemo.Clear;
//  Msgform.PageControl1.ActivePage := Msgform.MsgSheet;
//  MsgForm.Show;
  { Put this message into the message form's memo }
  ZipMaster1Message(Self, 0, 'Beginning Merge zipped files to ' + ZipMaster1.ZipFilename);

  ZipMaster1.AddOptions := [];
  ZipMaster1.WriteOptions := [];
  if ForceDestination1.Checked then
    ZipMaster1.WriteOptions := ZipMaster1.WriteOptions + [zwoForceDest];
  if Writesafe1.Checked then
    ZipMaster1.WriteOptions := ZipMaster1.WriteOptions + [zwoSafe];
  if Latesttime1.Checked then
    ZipMaster1.WriteOptions := ZipMaster1.WriteOptions + [zwoZipTime];
  ZipMaster1.Unattended := Unattended1.Checked;
  ZipMaster1.Verbose := Verbose1.Checked;
  ZipMaster1.Trace := Trace1.Checked;
  // get merge option
  MergeOpts := zmoConfirm;
  while True do
  begin
    if OptConfirm.Checked then
      Break;
    if OptAlways.Checked then
    begin
      MergeOpts := zmoAlways;
      Break;
    end;
    if OptNewer.Checked then
    begin
      MergeOpts := zmoNewer;
      Break;
    end;
    if OptOlder.Checked then
    begin
      MergeOpts := zmoOlder;
      Break;
    end;
    if OptNever.Checked then
    begin
      MergeOpts := zmoNever;
      Break;
    end;
    MergeOpts := zmoRename;
    Break;
  end;

  s := GetTickCount;
  Try
    Result := ZipMaster1.MergeZippedFiles(MergeOpts);
  Except
    ShowMessage('Exception in Merge');
    Result := -1;
  End;
  f := GetTickCount;
  TimeLabel.Caption := ShowLTime(s, f);
//  If ZipMaster1.SuccessCnt = 1 Then
//    IsOne := ' was'
//  Else
//    IsOne := 's were';
  ShowMsgForm(0, False); // update lists
//  ShowMessage(IntToStr(ZipMaster1.SuccessCnt) + ' file' + IsOne + ' merged');
end;

{$IFNDEF VERD6up}
procedure TMainform.CheckClick(Sender: TObject);
var
  m1: TMenuItem;
begin
  m1 := Sender as TMenuItem;
  m1.Checked := not m1.Checked;
end;

procedure TMainform.RadioClick(Sender: TObject);
var
  m1: TMenuItem;
begin
  m1 := Sender as TMenuItem;
  m1.Checked := True;
end;
{$ENDIF}

procedure TMainform.ShowMsgForm(Sheet: Integer; Erase: Boolean);
begin
  if Erase then
    MsgForm.MsgMemo.Clear;
  MsgForm.IncMemo.Clear;
  MsgForm.ExcMemo.Clear;
  if Erase then
    Msgform.ScriptMemo.Lines.AddStrings(ZipMaster1.FSpecArgs);
//  else
  MsgForm.IncMemo.Lines.AddStrings(ZipMaster1.FSpecArgs);
  MsgForm.ExcMemo.Lines.AddStrings(ZipMaster1.FSpecArgsExcl);
  Msgform.PageControl1.ActivePage := Msgform.MsgSheet;
  MsgForm.Show;
end;

procedure TMainform.ZipMaster1Tick(Sender: TObject);
Begin
  FNewCount := succ(FNewCount);
  if (FNewCount and 7) = 1 then
  begin
    FNewCount := FNewCount and 127;
    MsgForm.StatusBar1.Panels[0].Text := IntToStr(FNewCount);
  end;
End;

End.
