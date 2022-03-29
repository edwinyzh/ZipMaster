unit InstUnit;
{ InstUnit - part of DELZIP demo #4.  Freeware by Eric W. Engler and Chris Vleghert}
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ShlObj
  {$IfDef VER90}     // Delphi2
     ,Ole2;
  {$Else}            // Delphi3+
     ,ComObj, ActiveX;
  {$EndIf}

{$IfDef VER90}       // Delphi2
   type LongWord = Cardinal;
   type WideString = Array[0..MAX_PATH] of WideChar;

   type pShLinkType  = ^IShellLink;
   type ppShLinkType = ^pShLinkType;
   type pFileType    = ^IPersistFile;
   type ppFileType   = ^pFileType;
{$EndIf}

type
  TInstForm = class( TForm )
    GroupBox1:        TGroupBox;
    StartMenuCB:      TCheckBox;
    DesktopCB:        TCheckBox;
    RegistryCB:       TCheckBox;
    AssocCB:          TCheckBox;
    SendToCB:         TCheckBox;
    KillCB:           TCheckBox;
    UninstBut:        TButton;
    CancelBut:        TButton;
    InstBut:          TButton;
    Label1:           TLabel;
    Label2:           TLabel;
    Label3:           TLabel;
    ProgramNameLabel: TLabel;
    StartMenuRB:      TRadioButton;
    ProgramRB:        TRadioButton;

    procedure FormCreate( Sender: TObject) ;
    procedure InstButClick( Sender: TObject );
    procedure SetValInReg( RKey:HKey; KeyPath: String; ValName: String; NewVal: String );
    procedure MakeAssociation( Ext: String; PgmToLinkTo: String );
    procedure MakeLink( PgmPath, PgmArgs, LinkPath, Descr: String );
    procedure CancelButClick( Sender: TObject );
    procedure UninstButClick( Sender: TObject );
    procedure RegDeleteKey( RKey:HKey; KeyPath: String );
    procedure RemoveAssociation( Ext: String );
    procedure FormActivate( Sender: TObject );
    function  AddBackslash( str_in: string ): string;
    procedure StartMenuCBClick( Sender: TObject );

  private
    { Private declarations }

  public
    { Public declarations }
    EXEName, EXETitle: String;

    function  GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
  end;

var
  InstForm: TInstForm;

implementation

uses unit1;

{$R *.DFM}

procedure TInstForm.FormCreate( Sender: TObject );
begin
   EXEName  := ExtractFileName( ParamStr( 0 ) );
   EXETitle := 'ViewZip - Delphi ZIP Auto Install Application Example';
   ProgramNameLabel.Caption := ParamStr( 0 );

   if Form1.AutoUninstall then
   begin
      ShowMessage( 'Now beginning ViewZip auto uninstall' );
      UnInstButClick( Self );
   end;
end;

procedure TInstForm.InstButClick( Sender: TObject );
var
   path: String;
   MenuDir: Integer;
begin
   Screen.Cursor := crHourGlass;

   if StartMenuCB.Checked then
   begin
      if StartMenuRB.Checked then
         MenuDir := CSIDL_STARTMENU
      else
         MenuDir := CSIDL_PROGRAMS;
      GetSpecialFolder( MenuDir, path );
      MakeLink( ParamStr( 0 ),   // the full pathname of this executable program
            '',                  // no arguments
            path + EXETitle + '.lnk',
            'Sample Self-install Program' );
   end;

   if DesktopCB.Checked then
   begin
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, path );
      MakeLink( ParamStr( 0 ),   // the full pathname of this executable program
            '',                  // no arguments
            path + EXETitle + '.lnk',
            'Sample Install Program' );
   end;

   if SendToCB.Checked then
   begin
      GetSpecialFolder( CSIDL_SENDTO, path );
      MakeLink( ParamStr( 0 ),   // the full pathname of this executable program
            '',                  // no arguments
            path + EXETitle + '.lnk',
            'Sample Install Program' );
   end;

   if RegistryCB.Checked then
   begin
      { define the application path }
      SetValInReg( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + EXEName,
               '',                    { specify the default data item }
               ParamStr( 0 ) );       { Full pathname with program name }
      path:=ExtractFilePath(ParamStr(0));
      SetValInReg( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + EXEName,
               'Path',                { specify the Path data item }
               Copy( path, 1, Length( path )- 1 ) ); { Full pathname without end slash }

      { define the un-install command line }
      SetValInReg( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + EXEName,
               'DisplayName',
               EXETitle ); { show user this name in control panel }
      SetValInReg( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + EXEName,
               'UninstallString',
               ParamStr( 0 ) + ' /UNINSTALL' ); { pgm name and parameter }


      { define the main application program settings key }
      SetValInReg( HKEY_LOCAL_MACHINE,
               'SOFTWARE\' + EXEName,
               'InstalledVersion',
               '1.51' );

      { these are settings that only apply to the current logged-in user }
      SetValInReg( HKEY_CURRENT_USER,
               'SOFTWARE\' + EXEName,
               'InstalledVersion',
               '1.51' );
      SetValInReg( HKEY_CURRENT_USER,
               'SOFTWARE\' + EXEName,
               'Setting1',
               'y' );
      SetValInReg( HKEY_CURRENT_USER,
               'SOFTWARE\' + EXEName,
               'Setting2',
               'n' );
   end;

   if AssocCB.Checked then
      MakeAssociation( 'zip', ParamStr( 0 ) );

   Screen.Cursor := crDefault;

{$ifdef NEVER}
   if KillCB.Checked then
      KillMySelf( 0, False );
{$endif}

   Close;
end;

{ Create a Win95 file association in the registry.  This uses the Quick-and-
  Dirty method used by Explorer when you right click on a file and choose
  "Open With...".  Basically, the file extension is created as a class, and
  a dummy file type is created for that class to tell Win95 which program to
  run.  Once this is done, you can easily test it from a DOS Shell by typing:
  START FILENAME.EXT
    Be advised: This is where I expected file associations to be (because
  there are already some associations there), but they seem to have no effect:
    HKEY_CURRENT_USER,'Software\Microsoft\Windows\CurrentVersion\Extensions'
}
procedure TInstForm.MakeAssociation( Ext: String; PgmToLinkTo: String );
begin
   { ALL extensions must be in lowercase to avoid trouble! }
   Ext := LowerCase( Ext );
   if FileExists( PgmToLinkTo ) then
   begin
      SetValInReg( HKEY_CLASSES_ROOT,
             '.' + ext,            { extension we want to define }
             '',                   { specify the default data item }
             ext + '_auto_file' ); { This is the value of the default data item -
                                     this referances our new type to be defined  }
      SetValInReg( HKEY_CLASSES_ROOT,
            ext + '_auto_file',    { this is the type we want to define }
            '',                    { specify the default data item }
            ext + ' Files');       { This is the value of the default data item -
                                     this is the English description of the file type }

      SetValInReg( HKEY_CLASSES_ROOT,
            Ext + '_auto_file\DefaultIcon', { Create a file...DefaultIcon.}
            '',	                            { Specify the default data item.}
            PgmToLinkTo + ',0' );            { Executable where icon is in and it's Sequence number.}
      SHChangeNotify( SHCNE_ASSOCCHANGED, SHCNF_FLUSH, nil, nil );

// un-comment this if your file type can be viewed by Quick View
//    SetValInReg( HKEY_CLASSES_ROOT,
//          ext + '_auto_file\QuickView', { create a key for QuickView compat. }
//          '',                    { specify the default data item }
//          '*' );                 { flag to tell Explorer that QuickView is OK }

      SetValInReg( HKEY_CLASSES_ROOT,
            ext + '_auto_file\shell\open\command', { create a file...open key }
            '',                    { specify the default data item }
            PgmToLinkTo + ' %1' ); { command line to open file with }
   end
   else
      ShowMessage( 'Error: Program not found: ' + PgmToLinkTo );
end;

procedure TInstForm.RemoveAssociation( Ext: String );
begin
   Ext := LowerCase( Ext );
   RegDeleteKey( HKEY_CLASSES_ROOT,
                '.' + ext );     { extension we want to undefine }
   RegDeleteKey( HKEY_CLASSES_ROOT,
                Ext + '_auto_file\DefaultIcon' );
   RegDeleteKey( HKEY_CLASSES_ROOT,
                ext + '_auto_file\shell\open\command' );
   RegDeleteKey( HKEY_CLASSES_ROOT,
                ext + '_auto_file' );
   SHChangeNotify( SHCNE_ASSOCCHANGED, SHCNF_FLUSH, nil, nil );
end;

procedure TInstForm.RegDeleteKey( RKey: HKey; KeyPath: String );
begin
   with TRegistry.Create do
   try
      RootKey := RKey;
      // Under Win95, all keys under this one are auto. deleted also.
      // But, under WinNT, the keys under this one will be left alone.
      DeleteKey( KeyPath );
   finally
      Free;
   end;
end;

{ Set a value in the registry. This is NOT related to the .LNK code.
  This will create a new registry key if it doesn't already exist. }
procedure TInstForm.SetValInReg( RKey: HKey; KeyPath: String; ValName: String; NewVal: String );
begin
   with TRegistry.Create do
   try
      RootKey := RKey;
      OpenKey( KeyPath, True );
      WriteString( ValName, NewVal );
   finally
      Free;
   end;
end;

{$IfNDef VER90}    // Delphi 3+
{* Make a Shell Link, also called a "shortcut".
 * MakeLink - uses the shell's IShellLink and IPersistFile interfaces
 * to create and store a shortcut to the specified object.
 *
 * PgmPath  - address of a buffer containing the path of the object.
 * LinkPath - address of a buffer containing the path where the shell link is to be stored.
 * Descr    - address of a buffer containing the description of the shell link.
 * PgmArgs  - address of a buffer containing the arguments for the shell link.
 *}
procedure TInstForm.MakeLink( PgmPath, PgmArgs, LinkPath, Descr: String );
var
   AnObj:     IUnknown;
   ShLink:    IShellLink;
   PFile:     IPersistFile;
   WFileName: WideString;
begin
   if UpperCase( ExtractFileExt( LinkPath ) ) <> '.LNK' then
   begin
      ShowMessage( 'Error: link path extension must be .LNK' );
      Exit;
   end;

   // access to the two interfaces of the object
   AnObj  := CreateComObject( CLSID_ShellLink );
   ShLink := AnObj as IShellLink;
   PFile  := AnObj as IPersistFile;

   // NOTE: We're using a COM Object, so all string args must be PChar

   // set the link properties
   ShLink.SetPath( PChar( PgmPath ) );   // also called the link target
   ShLink.SetArguments( PChar( PgmArgs ) );
   ShLink.SetWorkingDirectory( PChar( ExtractFilePath( PgmPath ) ) );
   ShLink.SetDescription( PChar( Descr ) );

   // Save with a WideString filename
   WFileName := LinkPath;
   PFile.Save( PWChar( WFileName ), False );
end;

{$Else}
// Delphi 2
procedure TInstForm.MakeLink( PgmPath, PgmArgs, LinkPath, Descr: String );
var
   ShLink:     pShLinkType;
   pShLink:    ppShLinkType;
   hRes:       HRESULT;
   pFile:      pFileType;
   ppFile:     ppFileType;
   WFileName:  Array[0..MAX_PATH] of WideChar;
begin
   if UpperCase( ExtractFileExt( LinkPath ) ) <> '.LNK' then
   begin
      ShowMessage( 'Error: link path extension must be .LNK' );
      Exit;
   end;
   hRes := CoInitialize( nil );
   if (hRes = S_OK) or (hRes = S_FALSE) then
   begin
      if hRes = S_OK then
      begin
         // Get a pointer to the IShellLink interface.
         hRes := CoCreateInstance( CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLink, pShLink );
         if SUCCEEDED( hRes ) then
         begin
            // Set the path to the shortcut target, and add the description.
            ShLink := @pShLink;
            ShLink.SetPath( PChar( PgmPath ) );
            ShLink.SetDescription( PChar( Descr ) );
            ShLink.SetArguments( PChar( PgmArgs ) );
            ShLink.SetIconLocation( PChar( PgmPath ), 0 );
            ShLink.SetWorkingDirectory( PChar( ExtractFilePath( PgmPath ) ) );

            // Query IShellLink for the IPersistFile interface for saving the
            // shortcut in persistent storage.
            hRes := ShLink.QueryInterface( IID_IPersistFile, ppFile );
            if SUCCEEDED( hRes ) then
            begin
               pFile := @ppFile;
               // Ensure that the string is ANSI.
               MultiByteToWideChar( CP_ACP, 0, PChar( LinkPath ), -1, WFileName, MAX_PATH );

               // Save the link by calling IPersistFile::Save.
               pFile.Save( WFileName, False );
               PFile.SaveCompleted( WFileName );
               pFile.Release;
            end;
            ShLink.Release;
         end;
      end else
         ShowMessage( 'COM already initialized' );
      CoUninitialize;
   end else   // E_INVALIDARG, E_OUTOFMEMORY, o E_UNEXPECTED.
      ShowMessage( 'COM library could not initialize' );
end;
{$EndIf}

procedure TInstForm.CancelButClick( Sender: TObject );
begin
   Close;
end;

procedure TInstForm.UninstButClick( Sender: TObject );
var
   path:    String;
   MenuDir: Integer;
begin
   Screen.Cursor := crHourGlass;
   if StartMenuCB.Checked then
   begin
      if StartMenuRB.Checked then
         MenuDir := CSIDL_STARTMENU
      else
         MenuDir := CSIDL_PROGRAMS;
      GetSpecialFolder( MenuDir, path );
      DeleteFile( path + EXETitle + '.lnk' );
   end;

   if DesktopCB.Checked then
   begin
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, path );
      DeleteFile( path + EXETitle + '.lnk' );
   end;

   if SendToCB.Checked then
   begin
      GetSpecialFolder( CSIDL_SENDTO, path );
      DeleteFile( path + EXETitle + '.lnk' );
   end;

   if RegistryCB.Checked then
   begin
      RegDeleteKey( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + EXEName );
      RegDeleteKey( HKEY_LOCAL_MACHINE,
               'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + EXEName );
      RegDeleteKey( HKEY_LOCAL_MACHINE,
               'SOFTWARE\' + EXEName );
      RegDeleteKey( HKEY_CURRENT_USER,
               'SOFTWARE\' + EXEName );
   end;

   if AssocCB.Checked then
      RemoveAssociation( 'zip' );

   Screen.Cursor := crDefault;

   if NOT Form1.AutoUnInstall then
      { if we are auto-uninstalling, then we are still in OnCreate,
        so we can't close this form yet. }
      Close;
end;

procedure TInstForm.FormActivate( Sender: TObject );
begin
   if Form1.AutoUnInstall then
      PostMessage( Handle, WM_CLOSE, 0, 0 );
end;

// Add a backslash to a string if it doesn't already end in one,
// AND if the string has a non-zero length.
function TInstForm.AddBackslash( str_in: string ): string;
begin
   Result := str_in;
   if Result = '' then
      Exit;
   if Result[Length( Result )] <> '\' then
      Result := Result + '\';
end;

procedure TInstForm.StartMenuCBClick( Sender: TObject );
begin
  StartMenuRB.Enabled := StartMenuCB.Checked;
  ProgramRB.Enabled   := StartMenuCB.Checked;
end;

//---------------------------------------------------------------------------
{* Folder types are a.o.
 *	CSIDL_DESKTOPDIRECTORY, CSIDL_STARTMENU, CSIDL_SENDTO,
 * CSIDL_PROGRAMS, CSIDL_STARTUP etc.
 *}
function TInstForm.GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
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
