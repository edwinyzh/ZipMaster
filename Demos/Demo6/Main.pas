unit Main;
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

{$INCLUDE '..\..\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ZipMstr, Menus, Grids, SortGrid, StdCtrls, ExtCtrls, ShlObj, FileCtrl, ImgList;

type
  TMainForm = class( TForm )
    Panel2:          TPanel;
    Panel3:          TPanel;
    Panel4:          TPanel;
    Label1:          TLabel;
    Label2:          TLabel;
    Label4:          TLabel;
    Label5:          TLabel;
    Label6:          TLabel;
    ZipFName:        TLabel;
    FilesLabel:      TLabel;
    MaxVolSizeEdit:  TEdit;
    FreeDisk1Edit:   TEdit;
    MinFreeVolEdit:  TEdit;
    WipeCB: TCheckBox;
    Bevel1:          TBevel;
    AddBut:          TButton;
    ExtractBut:      TButton;
    WriteBttn:       TButton;
    ReadBttn:        TButton;
    NewZipBut:       TButton;
//    StringGrid1:     TSortGrid;
    OpenDialog1:     TOpenDialog;
    ImageList1:      TImageList;
    MainMenu1:       TMainMenu;
    File1:           TMenuItem;
    Exit1:           TMenuItem;
    Project1:        TMenuItem;
    Showlasterror1:  TMenuItem;
    DLLversioninfo1: TMenuItem;
    Messages1:       TMenuItem;
    ZipMaster1: TZipMaster;

    procedure StringGrid1BeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
    procedure StringGrid1ClickSort( Sender: TObject; Col, Row: LongInt; var SortOptions: TSortOptions );
    procedure StringGrid1DrawCell( Sender: TObject; Col, Row: LongInt; Rect: TRect; State: TGridDrawState );
    procedure StringGrid1EndSort( Sender: TObject; Col: LongInt );
    procedure StringGrid1GetCellFormat( Sender: TObject; Col, Row: LongInt; State: TGridDrawState; var FormatOptions: TFormatOptions );
    procedure NewZipButClick( Sender: TObject );
    procedure AddButClick( Sender: TObject );
    procedure WriteBttnClick( Sender: TObject );
    procedure ExtractButClick( Sender: TObject );
    procedure ReadBttnClick( Sender: TObject );
    procedure Exit1Click( Sender: TObject );
    procedure Showlasterror1Click( Sender: TObject );
    procedure DLLversioninfo1Click( Sender: TObject );
    procedure Messages1Click( Sender: TObject );
    procedure FreeDisk1EditChange( Sender: TObject );
    procedure MinFreeVolEditChange( Sender: TObject );
    procedure MaxVolSizeEditChange( Sender: TObject );
    procedure ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: String );
// TODO: ZipMaster1Progress
//  procedure ZipMaster1Progress( Sender: TObject; ProgrType: ProgressType; Filename: String; FileSize: Int64);//Integer );
    procedure ZipMaster1DirUpdate( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure FormResize(Sender: TObject);
    procedure ZipMaster1Progress(Sender: TObject; details: TZMProgressDetails);

  public    { Public declarations }
     FirstDir1, FirstDir2: String;
     FirstDir3, FirstDir4: String;
     FirstDir5:            String;       
    StringGrid1:     TSortGrid;
     GSortOptions:         TSortOptions;
     TotUncomp, TotComp:   Cardinal;
//     TotalSize1, TotalProgress1, TotalSize2, TotalProgress2: Int64;
     DoIt:                 Boolean;
     GSortCol:             LongInt;

     procedure AddSpan;
     procedure FillGrid;
     procedure SetZipTotals;
     procedure SetZipFName( aCaption: String; AssignName: Boolean );
     function  ZipOpenArchive: Boolean;
     function  AskDirDialog( const FormHandle: HWND; var DirPath: String ): Boolean;
     function  GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
  end;

var
  MainForm:   TMainForm;
  ExtractDir: String;
  ExpandDirs, OverWr, AllFiles, Canceled: Boolean;

implementation

uses MsgUnit, ExtrUnit, AddUnit, printers;

{$R *.DFM}

procedure TMainForm.FormCreate( Sender: TObject );
begin
  StringGrid1 := TSortGrid.Create(self);
  StringGrid1.Parent := Self;
   { Make sure "goColMoving" is false in object inspector. This lets the
     TSortGrid use Mouse Clicks on the col headers. }
  with StringGrid1 do
  begin
    Left := 0;
    Top := 109;
    Width := 617;
    Height := 283;
    Align := alClient;
    ColCount := 6;
    DefaultRowHeight := 22;
    FixedCols := 0;
    RowCount := 8;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -12;
    Font.Name := 'Arial';
    Font.Style := [];
    Options := [goFixedVertLine, goFixedHorzLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect];
    ParentFont := False;
    TabOrder := 1;
    OnDrawCell := StringGrid1DrawCell;
    CaseSensitive := False;
    AlignmentHorz := taLeftJustify;
    AlignmentVert := taTopJustify;
    ProportionalScrollBars := True;
    ExtendedKeys := False;
    SortOnClick := True;
    FooterFont.Charset := DEFAULT_CHARSET;
    FooterFont.Color := clWindowText;
    FooterFont.Height := -11;
    FooterFont.Name := 'MS Sans Serif';
    FooterFont.Style := [];
    PrintOptions.Orientation := poPortrait;
    PrintOptions.PageTitleMargin := 0;
    PrintOptions.PageFooter := 'date|time|page';
    PrintOptions.HeaderSize := 10;
    PrintOptions.FooterSize := 7;
    PrintOptions.DateFormat := 'd-mmm-yyyy';
    PrintOptions.TimeFormat := 'h:nn';
    PrintOptions.FromRow := 0;
    PrintOptions.ToRow := 0;
    PrintOptions.BorderStyle := bsNone;
    PrintOptions.MarginBottom := 0;
    PrintOptions.MarginLeft := 0;
    PrintOptions.MarginTop := 0;
    PrintOptions.MarginRight := 0;
    WordWrap := False;
    OnGetCellFormat := StringGrid1GetCellFormat;
    OnClickSort := StringGrid1ClickSort;
    OnBeginSort := StringGrid1BeginSort;
    OnEndSort := StringGrid1EndSort;
    ColWidths[0] := 123;
    ColWidths[1] := 89;
    ColWidths[2] := 100;
    ColWidths[3] := 101;
    ColWidths[4] := 52;
    ColWidths[5] := 143;
    RowCount     := 2;  // First row is fixed, and used for column headers.
    Cells[0, 0]  := 'File Name';
    Cells[1, 0]  := 'Compr. Size';
    Cells[2, 0]  := 'Uncompr. Size';
    Cells[3, 0]  := 'Date Time';
    Cells[4, 0]  := 'Ratio';
    Cells[5, 0]  := 'Path';
  end;

//  ZipMaster1.DLL_Load := true;
      { If we had args on the cmd line, then try to open the first one
        as a zip/exe file.  This is most useful in case user has an association
        to ".zip" that causes this program to run when user double clicks on a zip
        file in Explorer. }
   ZipMaster1.OnDirUpdate := ZipMaster1DirUpdate;
   if ParamCount > 0 then
     ZipMaster1.ZipFilename := ParamStr( 1 );
   with GSortOptions do
   begin
      SortDirection     := sdAscending;
      SortStyle         := ssAutomatic;
      SortCaseSensitive := False;
   end;
end;

procedure TMainForm.FormDestroy( Sender: TObject );
begin
  ZipMaster1.DLL_Load := false;
end;

procedure TMainForm.FormResize( Sender: TObject );
begin
   if Width - 291 > 0 then
      ZipFName.Width := Width - 291
   else
      ZipFName.Width := 0;
   SetZipFName( ZipMaster1.ZipFilename, False );
end;


procedure TMainForm.NewZipButClick( Sender: TObject );
var
   Ans: Word;
begin
   if FirstDir1 = '' then
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, FirstDir1 );
   with OpenDialog1 do
   begin
      InitialDir := FirstDir1;
      Title      := 'Create New ZIP File';
      FileName   := '';
      Filter     := 'ZIP Files (*.ZIP)|*.zip';
      DefaultExt := 'Zip';
      Options := Options + [ofHideReadOnly, ofShareAware];
      Options := Options - [ofPathMustExist, ofFileMustExist];
      if Execute then
      begin
         FirstDir1 := ExtractFilePath( FileName );
         if UpperCase( ExtractFileExt( FileName ) ) <> '.ZIP' then
         begin
            ShowMessage( 'Error: your new archive must end in .ZIP' );
            Exit;
         end;
         if FileExists( FileName ) then
         begin
            Ans := MessageDlg( 'Overwrite Existing File: ' + FileName + '?', mtConfirmation, [mbYes, mbNo], 0 );
            if Ans = mrYes then
               DeleteFile( FileName )
            else
               Exit;  // Don't use the new name.
         end;
         SetZipFName( Filename, True );
      end else
         Exit;
      if ZipMaster1.ZipFilename <> '' then
         AddSpan;
   end;
end;

procedure TMainForm.AddButClick( Sender: TObject );
begin
   FirstDir2 := FirstDir3;
   if NOT ZipOpenArchive then
      Exit;
   FirstDir3 := FirstDir2;
   if ZipMaster1.ZipFilename = '' then
      Exit;
   AddSpan;
end;

procedure TMainForm.AddSpan();
var
   IsOne: String;
begin
   Canceled := False;
   AddFile.ShowModal;  // Let user pick filenames to add.
   if Canceled then
      Exit;

   if AddFile.SelectedList.Items.Count = 0 then
   begin
      ShowMessage( 'No files selected' );
      Exit;
   end;
//   MsgForm.RichEdit1.Clear;
   MsgForm.Memo1.Clear;
   MsgForm.Show;
   // Put this message into the message form.
  ZipMaster1Message( self, 0, 'Beginning Add to ' + ZipMaster1.ZipFilename );

  ZipMaster1.AddOptions := [];
  if AddFile.RecurseCB.Checked then   // We want recursion.
     ZipMaster1.AddOptions := ZipMaster1.AddOptions + [AddRecurseDirs];
  if AddFile.DirNameCB.Checked then   // We want dirnames.
     ZipMaster1.AddOptions := ZipMaster1.AddOptions + [AddDirNames];
  ZipMaster1.WriteOptions := [];
  if WipeCB.Checked then    // We want disk spanning with formatting
     ZipMaster1.SpanOptions := ZipMaster1.SpanOptions + [spWipeFiles{, spTryFormat}] ;
//         AddOptions := AddOptions + [AddDiskSpanErase]
//      else															// We want normal disk spanning
     ZipMaster1.WriteOptions := ZipMaster1.WriteOptions + [zwoDiskSpan];
//         AddOptions := AddOptions + [AddDiskSpan];
  if AddFile.EncryptCB.Checked then   // We want a password.
     ZipMaster1.AddOptions := ZipMaster1.AddOptions + [AddEncrypt];

  ZipMaster1.FSpecArgs.Clear;
  ZipMaster1.FSpecArgs.Assign(AddFile.SelectedList.Items );   // Specify filenames.
  AddFile.SelectedList.Clear;
  try
     ZipMaster1.Add;
  except
     ShowMessage( 'Error in Add; Fatal Exception in Main' );
     Exit;
  end;
  if ZipMaster1.SuccessCnt = 1 then
     IsOne := ' was'
  else
     IsOne := 's were';
  ShowMessage( IntToStr(ZipMaster1.SuccessCnt ) + ' file' + IsOne + ' added' );
end;

procedure TMainForm.WriteBttnClick( Sender: TObject );
var
   InFile, OutFile: String;
begin
   FirstDir2 := FirstDir4;
   if NOT ZipOpenArchive then
      Exit;
   FirstDir4 := FirstDir2;
   InFile := ZipMaster1.ZipFilename;
   if InFile = '' then
     Exit;

   if AskDirDialog( MainForm.Handle, OutFile ) then
   begin
      OutFile := OutFile + ExtractFileName( InFile );
//      MsgForm.RichEdit1.Clear;
      MsgForm.Memo1.Clear;
      MsgForm.Show;
      ZipMaster1.WriteSpan( InFile, OutFile );
      MsgForm.Hide;
   end;
end;

procedure TMainForm.ExtractButClick( Sender: TObject );
var
   i:     Integer;
   IsOne: String;
begin
   FirstDir2 := FirstDir5;
   if NOT ZipOpenArchive or (ZipMaster1.ZipFilename = '') then
      Exit;
   FirstDir5 := FirstDir2;

   Extract.ShowModal;
   if (ExtractDir = '') or (Canceled = True) then
      Exit;

   if ZipMaster1.Count < 1 then
   begin
      ShowMessage( 'Error - no files to extract' );
      Exit;
   end;
   with ZipMaster1, StringGrid1 do
   begin
      FSpecArgs.Clear;
      // Get fspecs of selected files, unless user wants all files extracted.
      if NOT AllFiles then
      begin
         for i := Selection.Top to Selection.Bottom do
         begin
            if i <> RowCount - 1 then
            begin
               FSpecArgs.Add( Cells[5, i] + Cells[0, i] );
            end;
         end;
         if FSpecArgs.Count < 1 then
         begin
            ShowMessage( 'Error - no files selected' );
            Exit;
         end;
      end;
//      MsgForm.RichEdit1.Clear;
      MsgForm.Memo1.Clear;
      MsgForm.Show;
      // Put this message into the message form.
      ZipMaster1Message( self, 0, 'Beginning Extract from ' + ZipFilename );

      ExtrBaseDir := ExtractDir;
      ExtrOptions := [];
      if ExpandDirs then
         ExtrOptions := ExtrOptions + [ExtrDirNames];
      if OverWr then
         ExtrOptions := ExtrOptions + [ExtrOverWrite];
      try
         Extract;
      except
         ShowMessage( 'Error in Extract; Fatal DLL Exception in Main' );
         Exit;
      end;
      if SuccessCnt = 1 then
         IsOne := ' was'
      else
         IsOne := 's were';
      ShowMessage( IntToStr( SuccessCnt ) + ' file' + IsOne + ' extracted' );
   end;
end;

procedure TMainForm.ReadBttnClick( Sender: TObject );
var
   InFile, OutPath, ext: String;
   fd:                   String;
   len :                 LongInt;
   drivetype:            LongWord;
begin
   with OpenDialog1 do
   begin
      Options    := Options + [ofHideReadOnly, ofShareAware, ofPathMustExist, ofFileMustExist];
      Title      := 'Open spanned ZIP archive on last disk';
      Filter     := 'ZIP Files (*.ZIP)|*.zip';
      FileName   := '';
      InitialDir := 'A:\';
      DefaultExt := 'zip';
      if OpenDialog1.Execute then
      begin
         InFile    := FileName;
         fd        := ExtractFileDrive ( InFile ) + '\';
         drivetype := GetDriveType( PChar( fd ) );
         len       := 3;

         if (drivetype = DRIVE_FIXED) or (drivetype = DRIVE_REMOTE) then
         begin
            ext := ExtractFileExt( InFile );
            len := Length( InFile ) - Length( ext );
            if StrToIntDef( Copy( InFile, len - 2, 3 ), -1 ) = -1 then
            begin
               ShowMessage( 'This is not a valid (last)part of a spanned archive' );
               Exit;
            end;
         end;
         if AskDirDialog( MainForm.Handle, OutPath ) then
         begin
            if (drivetype = DRIVE_FIXED) or (drivetype = DRIVE_REMOTE) then
               OutPath := OutPath + ExtractFileName( Copy( InFile, 1, len - 3 ) + ext )
            else
               OutPath := OutPath + ExtractFileName( InFile );
//            MsgForm.RichEdit1.Clear;
            MsgForm.Memo1.Clear;
            MsgForm.Show;
            if ZipMaster1.ReadSpan( InFile, OutPath ) = 0 then
               SetZipFName( OutPath, True );
            MsgForm.Hide;
         end;
      end;
   end;
end;

procedure TMainForm.Exit1Click( Sender: TObject );
begin
   Close;
end;

procedure TMainForm.Showlasterror1Click( Sender: TObject );
begin
   if ZipMaster1.ErrCode <> 0 then
      ShowMessage( IntToStr( ZipMaster1.ErrCode ) + ' ' + ZipMaster1.ErrMessage )
   else
      ShowMessage( 'No last error present' );
end;

procedure TMainForm.DLLversioninfo1Click( Sender: TObject );
begin
  ShowMessage('DelZip179.dll ' + ZipMaster1.DLL_Version + #13#10
    + 'at ' + ZipMaster1.DLL_Path);
//   ShowMessage( 'UnZip Dll version: ' + IntToStr( ZipMaster1.UnzVers ) + #10 +
//					 '  Zip Dll version: ' + IntToStr( ZipMaster1.ZipVers ) );
end;

procedure TMainForm.Messages1Click( Sender: TObject );
begin
   MsgForm.Show;
end;

procedure TMainForm.FreeDisk1EditChange( Sender: TObject );
begin
   ZipMaster1.KeepFreeOnDisk1 := StrToIntDef( FreeDisk1Edit.Text, 0 );
end;

procedure TMainForm.MinFreeVolEditChange( Sender: TObject );
begin
   ZipMaster1.MinFreeVolumeSize := StrToIntDef( MinFreeVolEdit.Text, 65536 );
end;

procedure TMainForm.MaxVolSizeEditChange( Sender: TObject );
begin
   ZipMaster1.MaxVolumeSize := StrToIntDef(  MaxVolSizeEdit.Text, 0 );
end;

procedure TMainform.SetZipTotals();
begin
   with StringGrid1 do
   begin
      RowCount := RowCount + 1;
      Cells[0, RowCount - 1] := 'Total';
      Cells[1, RowCount - 1] := IntToStr( TotComp );
      Cells[2, RowCount - 1] := IntToStr( TotUncomp );
      if TotUnComp <> 0 then
         Cells[4, RowCount - 1] := IntToStr( Round( (1- (TotComp / TotUnComp) )* 100) ) + '% '
      else
         Cells[4, RowCount - 1] := '0 % ';
      Cells[5, RowCount - 1]    := '';
   end;
end;

//---------------------------------------------------------------------------
function TMainform.AskDirDialog( const FormHandle: HWND; var DirPath: String ): Boolean;
var
   pidl:        PItemIDList;
   FBrowseInfo: TBrowseInfo;
   Success:     Boolean;
   TitleName:   String;
   Buffer:      Array[0..MAX_PATH] of Char;
begin
   Result := False;
   ZeroMemory( @FBrowseInfo, SizeOf( FBrowseInfo ) );
   try
      GetMem( FBrowseInfo.pszDisplayName, MAX_PATH );
      FBrowseInfo.hwndOwner := FormHandle;
      TitleName             := 'Please specify a directory';
      FBrowseInfo.lpszTitle := PChar( TitleName );
      pidl := ShBrowseForFolder( FBrowseInfo );
      if pidl <> nil then
      begin
         Success := SHGetPathFromIDList( pidl, Buffer );
         // if False then pidl not part of namespace
         if Success then
         begin
            DirPath := Buffer;
            if DirPath[Length( DirPath )] <> '\' then
               DirPath := DirPath + '\';
            Result := True;
         end;
         GlobalFreePtr( pidl );
      end;
   finally
      if Assigned( FBrowseInfo.pszDisplayName ) then
         FreeMem( FBrowseInfo.pszDisplayName, Max_Path );
   end;
end;

{* Folder types are a.o.
 *	CSIDL_DESKTOPDIRECTORY, CSIDL_STARTMENU, CSIDL_SENDTO,
 * CSIDL_PROGRAMS, CSIDL_STARTUP etc.
 *}
function TMainform.GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
var
   pidl:      PItemIDList;
   hRes:      HRESULT;
   RealPath:  Array[0..MAX_PATH] of Char;
   Success:   Boolean;
begin
   Result := 0;
   hRes   := SHGetSpecialFolderLocation( Handle, aFolder, pidl );
   if hRes = NO_ERROR then
   begin
      Success := SHGetPathFromIDList( pidl, RealPath );
      if Success then
         Location := String( RealPath ) + '\'
      else
         Result := LongWord( E_UNEXPECTED );
   end else
      Result := hRes;
end;

procedure TMainForm.ZipMaster1DirUpdate( Sender: TObject );
begin
   FillGrid;
   FilesLabel.Caption := IntToStr( ZipMaster1.Count );
   SetZipFName( ZipMaster1.ZipFilename, False );
end;

procedure TMainForm.ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: String );
begin
//   MsgForm.RichEdit1.Lines.Append( Message );
   MsgForm.Memo1.Lines.Append( Message );
//   PostMessage( MsgForm.RichEdit1.Handle, EM_SCROLLCARET, 0, 0 );
   PostMessage( MsgForm.Memo1.Handle, EM_SCROLLCARET, 0, 0 );
   Application.ProcessMessages;
   if ErrCode > 0 then
      ShowMessage( 'Error Msg: ' + Message );
end;

// TODO: ZipMaster1Progress
//procedure TMainForm.ZipMaster1Progress( Sender: TObject; ProgrType: ProgressType; Filename: String; FileSize: Int64);//Integer );
//var
// Step: Integer;
//begin
// case ProgrType of
//    TotalSize2Process:
//       begin
//          // ZipMaster1Message( self, 0, 'in OnProgress type TotalBytes, size= ' + IntToStr( FileSize ) );
//          MsgForm.StatusBar1.Panels.Items[0].Text := 'Total size: ' + IntToStr( FileSize div 1024 ) + ' Kb';
//          MsgForm.ProgressBar2.Position := 1;
//          TotalSize2                    := FileSize;
//          TotalProgress2                := 0;
//       end;
//    TotalFiles2Process:
//       begin
//          // ZipMaster1Message( self, 0, 'in OnProgress type TotalFiles, files= ' + IntToStr( FileSize ) );
//          MsgForm.StatusBar1.Panels.Items[1].Text := IntToStr( FileSize ) + ' files';
//       end;
//    NewFile:
//       begin
//          // ZipMaster1Message( self, 0, 'in OnProgress type NewFile, size= ' + IntToStr( FileSize ) );
//          MsgForm.FileBeingZipped.Caption := Filename;
//          MsgForm.ProgressBar1.Position   := 1;         // Current position of bar.
//          TotalSize1                      := FileSize;
//          TotalProgress1                  := 0;
//       end;
//    ProgressUpdate:
//       begin
//          // ZipMaster1Message( self, 0, 'in OnProgress type Update, size= ' + IntToStr( FileSize ) );
//          // FileSize gives now the bytes processed since the last call.
//          TotalProgress1 := TotalProgress1 + FileSize;
//          TotalProgress2 := TotalProgress2 + FileSize;
//          if TotalSize1 <> 0 then
//          begin
//             {$IFDEF VERD6up}
//             Step := Integer( Int64(TotalProgress1) * Int64(10000) div Int64(TotalSize1) );
//             {$ELSE}
//             // D4+   (D5 gives a compiler error when using Int64 conversion!?)
//             Step := MulDiv(TotalProgress1, 10000, TotalSize1);
//             {$ENDIF}
//             // ZipMaster1Message( self, 0, 'Step = ' + IntToStr( Step ) );
//             MsgForm.ProgressBar1.Position := 1 + Step;
//          end else
//             MsgForm.ProgressBar1.Position := 10001;
//          if TotalSize2 <> 0 then
//          begin
//             {$IFDEF VERD6up}
//             Step := Integer( Int64(TotalProgress2) * Int64(10000) div Int64(TotalSize2) );
//             {$ELSE}
//             Step := MulDiv(TotalProgress2, 10000, TotalSize2);
//             {$EndIf}
//             MsgForm.ProgressBar2.Position := 1 + Step;
//          end;
//       end;
//    EndOfBatch:    // Reset the progress bar and filename.
//       begin
//          // ZipMaster1Message( self, 0, 'in OnProgress type EndOfBatch' );
//          MsgForm.FileBeingZipped.Caption   := '';
//          MsgForm.ProgressBar1.Position     := 1;
//          MsgForm.StatusBar1.Panels[0].Text := '';
//          MsgForm.StatusBar1.Panels[1].Text := '';
//          MsgForm.ProgressBar2.Position     := 1;
//       end;
// end;   // EOF Case
//end;

procedure TMainform.SetZipFName( aCaption: String; AssignName: Boolean );
begin
   with ZipFName, ZipMaster1 do
   begin
      // Assigning the filename will cause the table of contents to be read.
      // and possibly reset it to an empty string (If error found).
      if AssignName then
         ZipFilename := aCaption;

      if ZipFilename = '' then
         Caption := AnsiString( '<none>' )
      else
         Caption := MinimizeName( ZipFilename, Canvas, Width );

      if Canvas.TextWidth( ZipFilename ) > Width then
      begin
         Hint     := ZipFilename;
         ShowHint := True;
      end else
         ShowHint := False;
   end;
end;

function TMainForm.ZipOpenArchive(): Boolean;
begin
   Result := False;
   if FirstDir2 = '' then
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, FirstDir2 );
   with OpenDialog1 do
   begin
      InitialDir := FirstDir2;
      Title      := 'Open Existing ZIP File';
      Filter     := 'ZIP Files (*.ZIP)|*.zip';
      FileName   := '';
      Options    := Options + [ofHideReadOnly, ofShareAware, ofPathMustExist, ofFileMustExist];
      if Execute then
      begin
         FirstDir2 := ExtractFilePath( FileName );
         // Assigning the filename will cause the table of contents to be read.
         SetZipFName( Filename, True );
         Result := True;
      end;
   end;
end;

procedure TMainForm.StringGrid1BeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
begin
   StringGrid1.RowCount := StringGrid1.RowCount - 1;
end;

procedure TMainForm.StringGrid1ClickSort( Sender: TObject; Col, Row: LongInt; var SortOptions: TSortOptions );
begin
   with GSortOptions do
   begin
      if SortDirection = sdAscending then
         SortDirection := sdDescending
      else
         SortDirection := sdAscending;
   end;
   GSortCol    := Col;
   SortOptions := GSortOptions;
end;

procedure TMainForm.StringGrid1DrawCell( Sender: TObject; Col, Row: LongInt; Rect: TRect; State: TGridDrawState );
var
   i: Integer;
begin
   if (Row = 0) and (Col = GSortCol) then
   begin
      i := 1;
      if GSortOptions.SortDirection = sdAscending then
         i := 0;
      ImageList1.Draw( StringGrid1.Canvas, Rect.Right - 18, 1, i );
   end
end;

procedure TMainForm.StringGrid1EndSort( Sender: TObject; Col: LongInt );
begin
   SetZipTotals;
end;

procedure TMainForm.StringGrid1GetCellFormat( Sender: TObject; Col, Row: LongInt; State: TGridDrawState; var FormatOptions: TFormatOptions );
begin
   with FormatOptions do
   begin
      if (Row <> 0) and (Col <> 0) and (Col <> 5) then
         AlignmentHorz := taRightJustify;
      if Row = StringGrid1.RowCount - 1 then
      begin
         Font.Style := Font.Style + [fsBold];
         Font.Color := clRed;
      end;
   end;
end;

procedure TMainForm.FillGrid;
var
  i: Integer;
begin
  with StringGrid1 do
  begin
    { remove everything from grid except col titles }
    RowCount := 2;
    Rows[1].Clear;
    if ZipMaster1.Count = 0 then
       Exit;

    StringGrid1.RowCount := ZipMaster1.Count + 2;
    TotUnComp := 0;
    TotComp   := 0;
    for i := 1 to ZipMaster1.Count do
    begin
//       with ZipDirEntry( ZipMaster1.ZipContents[i - 1]^ ) do
       with ZipMaster1[i - 1] do
       begin
          Cells[0, i] := ExtractFileName( FileName );
          Cells[1, i] := IntToStr( CompressedSize );
          Cells[2, i] := IntToStr( UncompressedSize );
          Cells[3, i] := FormatDateTime( 'ddddd  t', FileDateToDateTime( DateTime ) );
          if UncompressedSize <> 0 then
             Cells[4, i] := IntToStr( Round( (1- (CompressedSize / UnCompressedSize) )* 100) ) + '% '
          else
             Cells[4, i] := '0% ';
          Cells[5, i] := ExtractFilePath( FileName );
          TotUncomp   := TotUnComp + Cardinal(UncompressedSize);
          Inc( TotComp, CompressedSize );
       end; // end with
    end; // end for
    SortByColumn( GSortCol, GSortOptions );
    Row := 1;
  end; // end with
end;

procedure TMainForm.ZipMaster1Progress(Sender: TObject; details:
    TZMProgressDetails);
begin
   case Details.Order of
      TotalSize2Process:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type TotalBytes, size= ' + IntToStr( FileSize ) );
            MsgForm.StatusBar1.Panels.Items[0].Text := 'Total size: ' + IntToStr(Details.TotalSize div 1024 ) + ' Kb';
            MsgForm.ProgressBar2.Position := 1;
            MsgForm.ProgressBar1.Max := 100;
            MsgForm.ProgressBar2.Max := 100;
//            TotalSize2                    := Details.TotalSize;
//            TotalProgress2                := 0;
         end;
      TotalFiles2Process:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type TotalFiles, files= ' + IntToStr( FileSize ) );
            MsgForm.StatusBar1.Panels.Items[1].Text := IntToStr(details.TotalCount) + ' files';
         end;
      NewFile:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type NewFile, size= ' + IntToStr( FileSize ) );
            MsgForm.FileBeingZipped.Caption := details.ItemName;
            MsgForm.ProgressBar1.Position   := 1;         // Current position of bar.
//            TotalSize1                      := details.ItemSize;
//            TotalProgress1                  := 0;
         end;
      ProgressUpdate:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type Update, size= ' + IntToStr( FileSize ) );
            // FileSize gives now the bytes processed since the last call.
//            TotalProgress1 := TotalProgress1 + FileSize;
//            TotalProgress2 := TotalProgress2 + FileSize;
//            if TotalSize1 <> 0 then
//            begin
//               {$IFDEF VERD6up}
//               Step := Integer( Int64(TotalProgress1) * Int64(10000) div Int64(TotalSize1) );
//               {$ELSE}
//               // D4+   (D5 gives a compiler error when using Int64 conversion!?)
//               Step := MulDiv(TotalProgress1, 10000, TotalSize1);
//               {$ENDIF}
               // ZipMaster1Message( self, 0, 'Step = ' + IntToStr( Step ) );
               MsgForm.ProgressBar1.Position := details.ItemPerCent;
//            end else
//               MsgForm.ProgressBar1.Position := 10001;
//            if TotalSize2 <> 0 then
//            begin
//               {$IFDEF VERD6up}
//               Step := Integer( Int64(TotalProgress2) * Int64(10000) div Int64(TotalSize2) );
//               {$ELSE}
//               Step := MulDiv(TotalProgress2, 10000, TotalSize2);
//               {$EndIf}
               MsgForm.ProgressBar2.Position := details.TotalPerCent;
//            end;
         end;
      EndOfBatch:    // Reset the progress bar and filename.
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type EndOfBatch' );
            MsgForm.FileBeingZipped.Caption   := '';
            MsgForm.ProgressBar1.Position     := 1;
            MsgForm.StatusBar1.Panels[0].Text := '';
            MsgForm.StatusBar1.Panels[1].Text := '';
            MsgForm.ProgressBar2.Position     := 1;
         end;
   end;   // EOF Case
end;

end.
 
