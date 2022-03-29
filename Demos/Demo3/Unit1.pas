{ unit1.pas   a demo of freeware ZIP/UNZIP DLLs for Delphi.
  This is the main unit of the advanced Zip/Unzip Demo projoect, demo3.
  The DLL is required to run this program: DelZip190.DLL.
  Also, both of these VCL's must be installed before opening this
  project in Delphi: ZipMaster and SortGrid. }
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

unit Unit1;

interface
{$include '..\..\ZipVers.inc'}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TZipList, Extrunit, ComCtrls, ZipMstr;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    RadioTraceOpt: TRadioGroup;
    Panel2: TPanel;
    RadioVerboseOpt: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    RadioRecurse: TRadioGroup;
    Panel3: TPanel;
    Panel4: TPanel;
    ProgressBar1: TProgressBar;
    FileBeingZipped: TLabel;
    NewBut: TButton;
    OpenBut: TButton;
    ZipFName: TLabel;
    OpenDialog: TOpenDialog;
    VersionBut: TButton;
    ListBut: TButton;
    AddBut: TButton;
    DeleteBut: TButton;
    ExtractBut: TButton;
    AbortBut: TButton;
    ExitBut: TButton;
    RichEdit1: TRichEdit;
    RadioDirNames: TRadioGroup;
    ZipMaster1: TZipMaster;
    procedure FormCreate(Sender: TObject);
    procedure VersionButClick(Sender: TObject);
    procedure ExitButClick(Sender: TObject);
    procedure ListButClick(Sender: TObject);
    procedure AddButClick(Sender: TObject);
    procedure DeleteButClick(Sender: TObject);
    procedure AbortButClick(Sender: TObject);
    procedure ResetProgressBar;
    procedure ExtractButClick(Sender: TObject);
    procedure OpenButClick(Sender: TObject);
    procedure NewButClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ZipMaster1Progress(Sender: TObject; details: TZMProgressDetails);
    procedure ZipMaster1Message(Sender: TObject; ErrCode: Integer;
      Message: String);
  private
    { Private declarations }
    procedure SetNewZipFile(FName: String; NewFile: Boolean);
  public
    { Public declarations }
    ExtractDir: String;
    ExpandDirs: Boolean;
    Overwrite: Boolean;
    TotalSize1, TotalProgress1: Int64;
end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{----------------------------------------------------------------}

procedure TForm1.ResetProgressBar;
begin
   FileBeingZipped.Caption:='';
   with ProgressBar1 do
   begin   { reset the bar: make it empty }
      min:=1;
      max:=10;
      step:=1;
      position:=min;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   RadioTraceOpt.ItemIndex:=0;  { default to no tracing }
   RadioVerboseOpt.ItemIndex:=1;{ default to show verbose msgs }
   RadioRecurse.ItemIndex:=0;   { dflt to no recursive adds of files }
   RadioDirNames.ItemIndex:=1;  { dflt to save dir names }
   AbortBut.Enabled:=False;
   ResetProgressBar;
   { IMPORTANT!  Either make sure you're in the same dir as all your
     files, or else use full pathnames on all your files. }
   Caption:='ZIP Demo3 - ' + GetCurrentDir;
//   ZipMaster1.DLL_Load := true;
end;

procedure TForm1.VersionButClick(Sender: TObject);
begin    
  ZipMaster1.DLL_Load := true;
  ShowMessage('DelZip192.dll is ' + ZipMaster1.Dll_Version + #13#10#13#10
   + ' at ' + ZipMaster1.Dll_Path);
  ZipMaster1.DLL_Load := false;
end;

procedure TForm1.ExitButClick(Sender: TObject);
begin
   Close;
end;

procedure TForm1.ListButClick(Sender: TObject);
begin
   { I'm making this modal bec. I don't want the zipfile to be
     changed while it's contents are being viewed. }
   if not FileExists(ZipFName.Caption) then
   begin
      ShowMessage('Error: file not found: ' + ZipFName.Caption);
      exit;
   end;
   VersionBut.Enabled:=False;
   DeleteBut.Enabled:=False;
   AddBut.Enabled:=False;
   ExitBut.Enabled:=False;
   ListBut.Enabled:=False;
   ExtractBut.Enabled:=False;
   ZipForm.ShowModal;  { we're using a separate form for the List function }
   VersionBut.Enabled:=True;
   DeleteBut.Enabled:=True;
   AddBut.Enabled:=True;
   ExitBut.Enabled:=True;
   ListBut.Enabled:=True;
   ExtractBut.Enabled:=True;
end;

procedure TForm1.AddButClick(Sender: TObject);
begin
   if ((Length(ZipFName.Caption) = 0) or (Length(Edit1.Text) = 0)) then
   begin
      ShowMessage('Error: you need at least name of zip, and 1st filespec to add');
      exit;
   end;
   VersionBut.Enabled:=False;
   DeleteBut.Enabled:=False;
   AddBut.Enabled:=False;
   ExitBut.Enabled:=False;
   ListBut.Enabled:=False;
   ExtractBut.Enabled:=False;

   ZipMaster1.FSpecArgs.Add(Edit1.Text);
   if Edit2.Text > '' then
      ZipMaster1.FSpecArgs.Add(Edit2.Text);  
   ZipMaster1.DLL_Load := true;

   with ZipMaster1 do
   begin
      if RadioVerboseOpt.ItemIndex = 0 then
         Verbose := False
      else
         Verbose := True;

      if RadioTraceOpt.ItemIndex = 0 then
         Trace := False
      else
         Trace := True;

      AddOptions:=[];
      if RadioDirNames.ItemIndex = 1 then
         AddOptions := AddOptions + [AddDirNames];

      if RadioRecurse.ItemIndex = 1 then
         AddOptions := AddOptions + [AddRecurseDirs];
   end;

   Cursor:=crHourGlass;
   AbortBut.Enabled:=True;
   ZipMaster1.Add;
   Cursor:=crDefault;
   if ZipMaster1.SuccessCnt < 1 then
      ShowMessage('Error adding files')
   else
      ShowMessage('DONE: Number of files zipped up: ' + IntToStr(ZipMaster1.SuccessCnt));

   ResetProgressBar;          
   ZipMaster1.DLL_Load := false;
   VersionBut.Enabled:=True;
   DeleteBut.Enabled:=True;
   AddBut.Enabled:=True;
   ExitBut.Enabled:=True;
   ListBut.Enabled:=True;
   AbortBut.Enabled:=False;
   ExtractBut.Enabled:=True;
end;

procedure TForm1.DeleteButClick(Sender: TObject);
begin
   VersionBut.Enabled:=False;
   DeleteBut.Enabled:=False;
   AddBut.Enabled:=False;
   ExitBut.Enabled:=False;
   ListBut.Enabled:=False;
   ExtractBut.Enabled:=False;

   if ((Length(ZipFName.Caption) = 0) or (Length(Edit1.Text) = 0)) then
   begin
      ShowMessage('Error: you need at least name of zip, and 1st filespec to add');
      exit;
   end;
   if not FileExists(ZipFName.Caption) then
   begin
      ShowMessage('Error: file not found: ' + ZipFName.Caption);
      exit;
   end;

   ZipMaster1.FSpecArgs.Add(Edit1.Text);
   if Edit2.Text > '' then
      ZipMaster1.FSpecArgs.Add(Edit2.Text);

   with ZipMaster1 do
   begin
      if RadioVerboseOpt.ItemIndex = 0 then
         Verbose := False
      else
         Verbose := True;

      if RadioTraceOpt.ItemIndex = 0 then
         Trace := False
      else
         Trace := True;
   end;
   
   Cursor:=crHourGlass;
   AbortBut.Enabled:=False;  { The DELETE option doesn't support abort }
   ZipMaster1.Delete;
   Cursor:=crDefault;
   if ZipMaster1.SuccessCnt < 1 then
      ShowMessage('Error deleting files')
   else
      ShowMessage('DONE: Number of files deleted: ' + IntToStr(ZipMaster1.SuccessCnt));

   ResetProgressBar;
   VersionBut.Enabled:=True;
   DeleteBut.Enabled:=True;
   AddBut.Enabled:=True;
   ExitBut.Enabled:=True;
   ListBut.Enabled:=True;
   AbortBut.Enabled:=False;
   ExtractBut.Enabled:=True;
end;

procedure TForm1.AbortButClick(Sender: TObject);
begin
   { This will be passed back to the DLL upon finishing processing
     of the next callback }
   ZipMaster1.Cancel:=True;
   AbortBut.Enabled:=False;
end;

procedure TForm1.ExtractButClick(Sender: TObject);
var
   SaveDir: String;
begin
   if (Length(ZipFName.Caption) = 0) then
   begin
      ShowMessage('Error: you need name of zipfile');
      exit;
   end;
   ExtractDir:='';
   SaveDir:=GetCurrentDir;
   { let user select extract directory,
     whether to expand the zip file's dir's,
     and whether to overwrite existing files }
   Extract.ShowModal;
   if Length(ExtractDir) = 0 then
      exit;

   SetCurrentDir(ExtractDir);
   if (GetCurrentDir <> ExtractDir) then
   begin
      ShowMessage('Error selecting dir: ' + ExtractDir);
      Exit;
   end;

   VersionBut.Enabled:=False;
   DeleteBut.Enabled:=False;
   AddBut.Enabled:=False;
   ExitBut.Enabled:=False;
   ListBut.Enabled:=False;
   ExtractBut.Enabled:=False;

   ZipMaster1.FSpecArgs.Clear;
   if Edit1.Text > '' then
      ZipMaster1.FSpecArgs.Add(Edit1.Text);
   if Edit2.Text > '' then
      ZipMaster1.FSpecArgs.Add(Edit2.Text);

   with ZipMaster1 do
   begin
      if RadioVerboseOpt.ItemIndex = 0 then
         Verbose := False
      else
         Verbose := True;

      if RadioTraceOpt.ItemIndex = 0 then
         Trace := False
      else
         Trace := True;

      ExtrOptions:=[];
      if ExpandDirs then
         ExtrOptions := ExtrOptions + [ExtrDirNames];

      if OverWrite then
         ExtrOptions := ExtrOptions + [ExtrOverWrite];
   end;
   Cursor:=crHourGlass;
   AbortBut.Enabled:=True;
   RichEdit1.Lines.Add('Unzip base directory: ' + ExtractDir);
   ZipMaster1.Extract;
   Cursor:=crDefault;
   ShowMessage('DONE: Number of files Unzipped: ' + IntToStr(ZipMaster1.SuccessCnt));

   VersionBut.Enabled:=True;
   DeleteBut.Enabled:=True;
   AddBut.Enabled:=True;
   ExitBut.Enabled:=True;
   ListBut.Enabled:=True;
   AbortBut.Enabled:=False;
   ExtractBut.Enabled:=True;

   SetCurrentDir(SaveDir);
   if (GetCurrentDir <> SaveDir) then
      ShowMessage('Error re-selecting dir: ' + SaveDir);
end;

procedure TForm1.OpenButClick(Sender: TObject);
begin
   with OpenDialog do
   begin
      Title:='Open Existing ZIP File';
      Options:=Options+[ofHideReadOnly,ofShareAware,ofPathMustExist,ofFileMustExist];
      Filter :='ZIP Files (*.ZIP)|*.zip';
      if Execute then
         SetNewZipFile(Filename, False);
   end;
end;

procedure TForm1.SetNewZipFile(FName: String; NewFile: Boolean);
var
   Ans: Boolean;
   i: Integer;
   Extension: String;
begin
   { get the extension of the filename }
   for i:=Length(FName)-1 downto 0 do
      if FName[i] = '.' then
      begin
         Extension:=Copy(FName, i, Length(FName) - i + 1);
         break;
      end;
   { if the extension isn't ZIP, then append a .zip extension onto it }
   if (CompareText(Extension,'.zip') <> 0) then
      FName:=FName+'.zip';

   { see if user wants a new zipfile, and if it already exists }
   if NewFile and FileExists(FName) then
   begin
      Ans:=MessageDlg('Overwrite Existing File: ' + FName + '?',
                          mtConfirmation,[mbYes,mbNo],0)=mrYes;
      if Ans then
         DeleteFile(FName)
      else
         Exit;  { Don't use the new name }
   end;

   ZipFName.Caption:=FName;
   ZipMaster1.ZipFilename:=FName;
   { Change to the new drive/directory, so all filespecs will
     be relative to the directory of the ZIP file. This is
     very important for most ZIP application programs!  A
     failure to do this will cause files and directories to
     become all mixed up. }
   SetCurrentDir(ExtractFileDir(FName));
   Caption:='ZIP Demo3 - ' + GetCurrentDir;

   VersionBut.Enabled:=True;
   DeleteBut.Enabled:=True;
   AddBut.Enabled:=True;
   ExitBut.Enabled:=True;
   ListBut.Enabled:=True;
   AbortBut.Enabled:=False;
   ExtractBut.Enabled:=True;
   RichEdit1.Lines.Clear;
end;

procedure TForm1.NewButClick(Sender: TObject);
begin
   with OpenDialog do
   begin
      Title:='Create New ZIP File';
      Options:=Options+[ofHideReadOnly,ofShareAware];
      Options:=Options-[ofPathMustExist,ofFileMustExist];
      Filter :='ZIP Files (*.ZIP)|*.zip';
      if Execute then
         SetNewZipFile(Filename, True);
   end; { end with }
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ZipMaster1.Dll_Load := false;
end;

procedure TForm1.ZipMaster1Progress(Sender: TObject; details: TZMProgressDetails);
begin
  Case details.Order Of
    TotalSize2Process:
      Begin
            RichEdit1.Lines.Add('Total uncompressed size: ' + IntToStr(details.TotalSize div 1024 ) + ' Kb');
            with ProgressBar1 do
            begin
               Max        := 100;
               Position   := 1;      // Current position of bar.
               Step       := 1;
            end;
            TotalSize1     := details.TotalSize;
            TotalProgress1 := 0;
      End;
    TotalFiles2Process:
      Begin
            RichEdit1.Lines.Add(IntToStr(details.TotalCount) + ' files to add');
      End;
    NewFile:
      Begin
            FileBeingZipped.Caption := details.ItemName;
      End;
    ProgressUpdate:
      Begin
            ProgressBar1.Position := details.TotalPerCent;
      End;
    EndOfBatch: // Reset the progress bar and filename.
      Begin
            FileBeingZipped.Caption   := '';
            ProgressBar1.Position     := 1;
      End;
  End;
   Application.ProcessMessages;
end;
 
procedure TForm1.ZipMaster1Message(Sender: TObject; ErrCode: Integer;
  Message: String);
begin
   RichEdit1.Lines.Add(Message);
   PostMessage(RichEdit1.Handle, EM_SCROLLCARET, 0, 0);
   Application.ProcessMessages;
   if (ErrCode > 0) and not ZipMaster1.Unattended then
      ShowMessage( 'Error Msg: ' + Message );
end;

end.

