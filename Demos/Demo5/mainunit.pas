unit mainunit;
// demo5 of Delphi Zip - this simple program makes an .EXE archive
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
  StdCtrls, Grids, ExtCtrls, {SortGrid,} ZipMstr, ShlObj;

type
  TMainform = class( TForm )
    Panel1:       TPanel;
    Panel2:       TPanel;
    Panel3:       TPanel;
    Panel4:       TPanel;
    Bevel1:       TBevel;
    Bevel2:       TBevel;
    Label1:       TLabel;
    Label2:       TLabel;
    ZipFName:     TLabel;
    FilesLabel:   TLabel;
    DeleteZipBut: TButton;
    NewZipBut:    TButton;
    CloseBut:     TButton;
    DeleteBut:    TButton;
    AddBut:       TButton;
    StringGrid1:  TStringGrid;
    OpenDialog1:  TOpenDialog;
    ZipMaster1: TZipMaster;

    procedure CloseButClick( Sender: TObject );
    procedure NewZipButClick( Sender: TObject );
    procedure DeleteZipButClick( Sender: TObject );
    procedure ZipMaster1DirUpdate( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FillGrid;
    procedure AddButClick( Sender: TObject );
    procedure ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: string );
    procedure DeleteButClick( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
    DoIt:     Boolean;
    FirstDir: String;

    procedure SetZipFName( aCaption: String );
    function  GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
  end;

var
  Mainform: TMainform;
  Cancelled: Boolean;

implementation

uses addunit;

{$R *.DFM}
//{$R 'Res\dzsfx_all.res'}  // bin files

procedure TMainform.CloseButClick( Sender: TObject );
begin
   Close;
end;

procedure TMainform.NewZipButClick( Sender: TObject );
begin
   if FirstDir = '' then
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, FirstDir );
   with OpenDialog1 do
   begin
      Title      := 'Create New EXE File';
      Options    := Options + [ofHideReadOnly,ofShareAware];
      Options    := Options - [ofPathMustExist,ofFileMustExist];
      Filter     := 'EXE Files (*.EXE)|*.exe';
      InitialDir := FirstDir;
      FileName   := '';
      if NOT Execute then
         Exit;

      FirstDir := ExtractFilePath( FileName );
      if Pos( '.', Filename ) = 0 then
         // user did not specify extension
         FileName := FileName + '.exe'
      else
         if Uppercase( ExtractFileExt( Filename ) ) <> '.EXE' then
         begin
            ShowMessage( 'Error: your new archive must end in .EXE' );
            Exit;
         end;
      if FileExists( FileName ) then
      begin
         if MessageDlg( 'Overwrite Existing File: ' + FileName + '?',
                       mtConfirmation, [mbYes,mbNo], 0 ) = mrYes then
            DeleteFile( FileName )
         else
            Exit;  { Don't use the new name }
      end;
      SetZipFName( Filename );
   end; { end with }

  if MessageDlg( 'Protect the archive with a password?', mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
	  ZipMaster1.Password := ZipMaster1.GetAddPassword;
      if ZipMaster1.Password = '' then
      begin
         { The 2 password's entered by user didn't match. }
         { We'll give him one more try }
         if MessageDlg( 'Do you want another chance to enter the password?',
            mtConfirmation, [mbYes,mbNo], 0 ) = mrYes then
		 begin
			ZipMaster1.Password := ZipMaster1.GetAddPassword;
            if ZipMaster1.Password = '' then
               Close;
         end
         else
            Close;
      end;
  end;
  ZipMaster1.ZipFileName := ZipFName.Caption;
end;

procedure TMainform.DeleteZipButClick( Sender: TObject );
var
   ans: Boolean;
begin
   if FileExists( ZipFName.Caption ) then
   begin
      Ans:=MessageDlg('Are you sure you want to delete: ' + ZipFName.Caption
            + '?', mtConfirmation, [mbYes,mbNo], 0) = mrYes;
      if Ans then
      begin
         DeleteFile( ZipFName.Caption );
         SetZipFName( '<none>' );
         StringGrid1.RowCount := 2; { empty }
         StringGrid1.Rows[1].Clear;
         ZipMaster1.ZipFilename := '';
         FilesLabel.Caption := '0';
      end
      else
         Exit;  { Don't use the new name }
   end
   else
      ShowMessage( 'File not found: ' + ZipFName.Caption );
end;

procedure TMainform.ZipMaster1DirUpdate( Sender: TObject );
begin
   FillGrid;
   FilesLabel.Caption:=IntToStr( ZipMaster1.Count );
   SetZipFName( ZipMaster1.ZipFilename );
end;

procedure TMainform.FormCreate( Sender: TObject );
begin
  chdir( 'C:\' );

  with StringGrid1 do
  begin
    { Make sure "goColMoving" is false in object inspector. This lets the
      TSortGrid use Mouse Clicks on the col headers. }
    RowCount     :=   2;  { first row is fixed, and used for column headers }
    Cells[0,0]   := 'File Name';
    Cells[1,0]   := 'Compr Size';
    Cells[2,0]   := 'Uncmpr Size';
    Cells[3,0]   := 'Date/Time';
    ColWidths[0] := 316;
    ColWidths[1] :=  84;
    ColWidths[2] :=  94;
    ColWidths[3] := 120;
  end;
  ZipMaster1.Dll_Load := true;
//  ZipMaster1.Load_Zip_Dll;
end;

procedure TMainform.FormDestroy( Sender: TObject );
begin
  ZipMaster1.Dll_Load := false;
//  ZipMaster1.Unload_Zip_Dll;
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

    StringGrid1.RowCount := ZipMaster1.Count + 1;
    for i := 1 to ZipMaster1.Count do
    begin
//       with ZipDirEntry( ZipMaster1.ZipContents[i - 1]^ ) do	// old
	   with ZipMaster1.DirEntry[i - 1]{^} do  	// new
       begin
          Cells[0, i] := FileName;
          Cells[1, i] := IntToStr( CompressedSize );
          Cells[2, i] := IntToStr( UncompressedSize );
          Cells[3, i] := FormatDateTime( 'ddddd  t', FileDateToDateTime( DateTime ) );
       end; // end with
    end; // end for
  end; // end with
end;

procedure TMainform.AddButClick( Sender: TObject );
var
   IsOne: String;
begin
   { In this program, the first Add will cause the SFX code to be added,
     and will make the SFX control block.  So, we need to set the SFX
     properties now! }
   ZipMaster1.SFXDefaultDir := 'C:\'; // default extract directory

   if ZipMaster1.ZipFileName = '' then
   begin
      ShowMessage( 'Error - create a new archive first' );
      Exit;
   end;
   AddForm.Left   := Left;
   AddForm.Top    := Top;
   AddForm.Width  := Width;
   AddForm.Height := Height;
   Cancelled      := False;

   AddForm.ShowModal;  { let user pick filenames to add }
   if Cancelled then
      Exit;

   if AddForm.SelectedList.Items.Count = 0 then
   begin
      ShowMessage( 'No files selected' );
      Exit;
   end;

   with ZipMaster1 do
   begin
      Verbose    := False;
      Trace      := False;
      AddOptions := [];
      if Password > '' then
         AddOptions := AddOptions + [AddEncrypt]  { we want a password }
      else
         AddOptions := AddOptions - [AddEncrypt]; // don't forget to turn this off!
      FSpecArgs.Clear;
      FSpecArgs.Assign( AddForm.SelectedList.Items ); { specify filenames }
      AddForm.SelectedList.Clear;
      Screen.Cursor := crHourGlass;
      try
         Add;
      except
         Screen.Cursor := crDefault;
         ShowMessage( 'Error in Add; Fatal DLL Exception in mainunit' );
      end;
      Screen.Cursor := crDefault;
      if SuccessCnt = 1 then
         IsOne := ' was'
      else
         IsOne := 's were';
      ShowMessage( IntToStr( SuccessCnt ) + ' file' + IsOne + ' added' );
   end; { end with }
end;

// This is the "OnMessage" event handler
procedure TMainform.ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: string );
begin
   if ErrCode > 0 then
      ShowMessage( 'Error Msg from DLL: ' + Message );
end;

procedure TMainform.DeleteButClick( Sender: TObject );
var
   i:     Integer;
   Ans:   Boolean;
   IsOne: String;
begin
   with StringGrid1 do
   begin
      if ZipMaster1.Count < 1 then
      begin
         ShowMessage( 'Error - no files to delete' );
         Exit;
      end;
      Ans:=MessageDlg( 'Delete selected files from: ' + ZipMaster1.ZipFileName + '?',
               mtConfirmation, [mbYes, mbNo], 0 ) = mrYes;
      if not Ans then
         Exit;

      ZipMaster1.FSpecArgs.Clear;
      for i := Selection.Top to Selection.Bottom do
      begin
         ZipMaster1.FSpecArgs.Add( Cells[0, i] );
      end; { end for }

      if ZipMaster1.FSpecArgs.Count < 1 then
      begin
         ShowMessage( 'Error - no files selected' );
         Exit;
      end;
   end; { end with }

   ZipMaster1.Verbose := False;
   ZipMaster1.Trace   := False;
   Screen.Cursor := crHourGlass;
   try
      ZipMaster1.Delete;
   except
      Screen.Cursor := crDefault;
      ShowMessage( 'Fatal error trying to delete' );
   end;
   Screen.Cursor := crDefault;
   if ZipMaster1.SuccessCnt = 1 then
      IsOne := ' was'
   else
      IsOne := 's were';
   ShowMessage( IntToStr( ZipMaster1.SuccessCnt ) + ' file' + IsOne + ' deleted' );
end;

procedure TMainform.SetZipFName( aCaption: String );
begin
   ZipFName.Caption := aCaption;
   Font.Assign( ZipFName.Font );
   if Canvas.TextWidth( aCaption ) > ZipFName.Width then
   begin
      ZipFName.Hint     := aCaption;
      ZipFName.ShowHint := True;
   end
   else
      ZipFName.ShowHint := False;
end;

//---------------------------------------------------------------------------
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
      GlobalFreePtr( pidl );
   end else
      Result := hRes;
end;

end.
