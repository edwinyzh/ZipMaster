unit Unit1;    { ViewZip - Demo4 of Delphi Zip }
{ This is a Delphi example of how a small self-installing program
  might be written.  If it runs with an argument of /INSTALL, it automatically
  brings up the install menu.  If it runs with an argument of /UNINSTALL
  (such as when running from the Control Panel Uninstall option), it
  does the uninstall and exits.  If the argument is anything else, then
  it assumes it's a zip file and tries to open it.

  IMPORTANT!!!  The "InstUnit" is designed for Win95 Registry keys.  It
  should work on Win98, but it will likely require some tweaks for WinNT.
  YOU HAVE BEEN WARNED!
}
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, SortGrid, InstUnit, ZipMstr, ImgList;

//{$IfDef VER90} // Delphi 2 is a special case
//   type LPCTSTR = PChar;
//{$EndIf}

// Prototypes for functions that we explicitly import from Kernel32.DLL
type PROCFREELIBRARY     = function( hInst: THandle ): Boolean; stdcall;
type PROCDELETEFILE      = function ( aFile: LPCTSTR ): Boolean; stdcall;
type PROCREMOVEDIRECTORY = function( aDir: LPCTSTR ): Boolean; stdcall;
type PROCEXITPROCESS     = procedure( aVal: DWORD ); stdcall;

// Data structure containing all the information we need to delete ourself,
// remove our containing directory, and terminate ourself.
type DELEXEINFO = packed record
   hInstExe:           THandle;
   pfnFreeLibrary:     PROCFREELIBRARY;
   pfnDeleteFile:      PROCDELETEFILE;
   FileName:           Array [0..MAX_PATH] of Char;
   pfnRemoveDirectory: PROCREMOVEDIRECTORY;
   Dir:                Array [0..MAX_PATH] of Char;
   pfnExitProcess:     PROCEXITPROCESS;
   ExitCode:           DWORD;
 end;
type pDELEXEINFO = ^DELEXEINFO;

type PROCDELEXE = procedure( pDEI: pDELEXEINFO ); stdcall;

type
  TForm1 = class( TForm )
    Panel1:      TPanel;
    OpenBut:     TButton;
    CancelBut:   TButton;
    InstBut:     TButton;
    Label1:      TLabel;
    Label2:      TLabel;
    ZipFName:    TLabel;
    Label4:      TLabel;
    OpenDialog1: TOpenDialog;
    ImageList1:  TImageList;
    ZipDir1: TZipMaster;

    procedure FormCreate( Sender: TObject );
    procedure FormActivate( Sender: TObject );
    procedure OpenButClick( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure InstButClick( Sender: TObject );
    procedure SortGrid1DrawCell( Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState );
    procedure SortGrid1ClickSort( Sender: TObject; Col, Row: Longint; var SortOptions: TSortOptions );

  private
    { Private declarations }

  public
    { Public declarations }     
    SortGrid1:   TSortGrid;
    GSortOptions:  TSortOptions;
    GSortCol:      Integer;
    AutoUninstall: Boolean;

    procedure FillGrid;
  end;

const
  HEAP_ZERO_MEMORY = $00000008;   

var
  Form1: TForm1;

//  procedure DelExeInjCode( pdei: PDELEXEINFO ); stdcall;
//  procedure AfterDelExeInjCode; stdcall;
//  procedure KillMySelf( exitcode: Integer; fRemoveDir: Boolean ); stdcall;

implementation

{$R *.DFM}
uses
  printers;

procedure TForm1.FormCreate( Sender: TObject );
begin
   SortGrid1 := TSortGrid.Create(self);
   SortGrid1.Parent := Self;
   with SortGrid1 do
   begin
    Left := 0;
    Top := 89;
    Width := 572;
    Height := 224;
    Align := alClient;
    ColCount := 4;
    DefaultRowHeight := 18;
    FixedCols := 0;
    RowCount := 2;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect];
    TabOrder := 1;
    OnDrawCell := SortGrid1DrawCell;
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
    OnClickSort := SortGrid1ClickSort;
    ColWidths[0] := 306; 
    ColWidths[1] := 94;
    ColWidths[2] := 100;
    ColWidths[3] := 120;
      Cells[0, 0]  := 'File Name';
      Cells[1, 0]  := 'Compr Size';
      Cells[2, 0]  := 'Uncmpr Size';
      Cells[3, 0]  := 'Date/Time';
   end;

   { Allowable Command Line parameters:
       a zip filename = display it's contents
       /install = bring up install menu automatically
       /uninstall = do the uninstall and quit (no menu)
   }
   if ParamCount > 0 then
   begin
      if UpperCase( ParamStr( 1 ) ) = '/INSTALL' then
      begin
         AutoUnInstall := False;
         InstButClick( Self );   { show install menu }
      end
      else if UpperCase( ParamStr( 1 ) ) = '/UNINSTALL' then
      begin
         AutoUnInstall := True;
         InstButClick( Self );  { do the un-install }
      end
      else
      begin
         { someone passed us an argument that is most likely
         the name of a zip file }
         if FileExists( ParamStr( 1 ) ) then
         begin
            ZipFName.Caption := ParamStr( 1 );
            { This assignment causes zipfile to be read: }
            ZipDir1.ZipFileName := ZipFName.Caption;
            FillGrid;
         end
         else
            ShowMessage( 'File Not Found: ' + ParamStr( 1 ) );
      end;
   end;
end;

procedure TForm1.FormActivate( Sender: TObject );
begin
   if AutoUnInstall then
      { The user just un-installed us: either from the Control Panel, or
        from our Install Menu.  Either way, he obviously doesn't want
        us to continue running now. }
      Close;
end;

procedure TForm1.OpenButClick( Sender: TObject );
begin
   if OpenDialog1.Execute then
   begin
      ZipFName.Caption := OpenDialog1.Filename;
      { This assignment causes zipfile to be read: }
      ZipDir1.ZipFileName := ZipFName.Caption;
      FillGrid;
   end;
end;

procedure TForm1.CancelButClick( Sender: TObject );
begin
   Close;
end;

procedure TForm1.InstButClick( Sender: TObject );
var
   InstForm: TInstForm;
begin
   InstForm := TInstForm.Create( Self );
   InstForm.ShowModal;
   InstForm.Destroy;
end;


//---------------------------------------------------------------------------
procedure TForm1.FillGrid;
var
  i: Integer;
begin
  with SortGrid1 do
  begin
    { Empty data from string grid }
    RowCount  := 2; { remove everything from grid except col titles }
    Rows[1].Clear();
    if ZipDir1.Count = 0 then
       Exit;

    RowCount := ZipDir1.Count + 1;
    for i := 1 to ZipDir1.Count do
    begin
  //     with ZipDirEntry( ZipDir1.ZipContents[i - 1]^ ) do  // old way
	   with ZipDir1.DirEntry[i - 1]{^} do    // new
       begin
          { The "-1" below is an offset for the row titles }
          Cells[0, i] := FileName;
          Cells[1, i] := IntToStr( CompressedSize );
          Cells[2, i] := IntToStr( UncompressedSize );
          Cells[3, i] := FormatDateTime( 'ddddd  t', FileDateToDateTime( DateTime ) );
       end; // end with
    end; // end for
    SortByColumn( GSortCol, GSortOptions );
  end; // end with
end;

procedure TForm1.SortGrid1ClickSort( Sender: TObject; Col, Row: LongInt; var SortOptions: TSortOptions );
begin
   if GSortOptions.SortDirection = sdAscending then
      GSortOptions.SortDirection := sdDescending
   else
      GSortOptions.SortDirection := sdAscending;
   GSortCol    := Col;
   SortOptions := GSortOptions;
end;

procedure TForm1.SortGrid1DrawCell( Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState );
var
  direction: Integer;
begin
   if (ARow = 0) and (ACol = GSortCol) then
   begin
      if GSortOptions.SortDirection = sdAscending then
         direction := 0
      else
         direction := 1;
      ImageList1.Draw( SortGrid1.Canvas, Rect.Right - 18, 0, direction );
   end;
end;

{$ifdef NEVER}
//---------------------------------------------------------------------------
// Code to be injected into our own address space.
procedure DelExeInjCode( pdei: pDELEXEINFO ); stdcall;
begin
  // Remove the EXE file from our address space
  pdei.pfnFreeLibrary( pdei.hinstExe );

  // Delete the EXE file now that it is no longer in use
  pdei.pfnDeleteFile( pdei.FileName );

  if @pdei.pfnRemoveDirectory <> nil then // Remove the directory (which is now empty)
     pdei.pfnRemoveDirectory( pdei.Dir );

  // Terminate our process
  pdei.pfnExitProcess( pdei.ExitCode );
end;

// This function just marks the end of the previous function.
procedure AfterDelExeInjCode; stdcall
begin
end;

// I'm showing you here how to delete an .exe file from within itself.
// This is not protable accross Windows versions.
// This is just "For your info...".
procedure KillMySelf( exitcode: Integer; fRemoveDir: Boolean ); stdcall
var
  dei:       DELEXEINFO;
  hinstKrnl: THandle;
  hheap:     THandle;
  FuncSize:  Integer;
  pfnDelExe: PROCDELEXE;
  P:         PChar;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
     Exit;

  hinstKrnl := GetModuleHandle( 'KERNEL32' );
  hheap	    := GetProcessHeap();

  // Calculate the number of bytes in the DelExeInjCode function.
  FuncSize := Integer(DWord(@AfterDelExeInjCode) - DWord(@DelExeInjCode));

  // From our process's default heap, allocate memory where we can inject our own function.
  @pfnDelExe := HeapAlloc( hheap, HEAP_ZERO_MEMORY, FuncSize );

  // Inject the DelExeInjCode function into the memory block
  CopyMemory( @pfnDelExe, @DelExeInjCode, FuncSize );

  // Initialize the DELEXEINFO structure.
  dei.hinstExe := GetModuleHandle( nil );
  @dei.pfnFreeLibrary := GetProcAddress( hinstKrnl, 'FreeLibrary' );

  // Assume that the subdirectory is NOT to be removed.
  dei.pfnRemoveDirectory := nil;
  @dei.pfnDeleteFile := GetProcAddress( hinstKrnl, 'DeleteFileA' );
  GetModuleFileName( dei.hinstExe, dei.FileName, MAX_PATH );

  if fRemoveDir then
  begin	// The subdirectory should be removed.
    @dei.pfnRemoveDirectory := GetProcAddress( hinstKrnl, 'RemoveDirectoryA' );
    StrCopy( dei.Dir, dei.FileName );
    P := StrRScan( dei.Dir, '\' );
    if P <> nil then
       P^ := #0;
  end;

  @dei.pfnExitProcess := GetProcAddress( hinstKrnl, 'ExitProcess' );
  dei.ExitCode := exitcode;

  pfnDelExe( @dei );
  // We never get here because pfnDelExe never returns.
end;
{$endif}

end.
