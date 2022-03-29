(******************************************************************)
(* SFX for DelZip v1.9                                            *)
(* Copyright 2002-2004, 2008                                      *)
(*                                                                *)
(* written by Markus Stephany                                     *)
(* modified by Russell Peters, Roger Aelbrecht
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License (licence.txt) for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  contact: problems AT delphizip DOT org
  updates: http://www.delphizip.org

  modified 30-Jan-2008
---------------------------------------------------------------------------*)
unit ZMSFXDefs;

{
this unit contains most definitions, constants and types used
by the sfx binary of delzip.

}

interface

uses
  Messages, Windows;

//{$I DELVER.INC}
{$I ..\ZipVers.inc}

{ Dialog Control-IDs }
const
  // buttons !! don't change the order of the first two buttons (SFXDialogs.pas, FileExistsDialogProc)
  ID_BTN_YES = 1; // yes/ok-buttons
  ID_BTN_NO = 2; // no/cancel-buttons

  ID_BTN_ABOUT = 103; // about-button
  ID_BTN_BROWSE = 104; // browse for extraction path
  ID_BTN_SHOWFILES = 105; // enlarge dialog to show files list box

  // edit box
  ID_EDITBOX = 201; // path/password edit

  // check boxes
  ID_CB_NOASK = 301; // file already exist : don't ask again checkbox
  ID_CB_RUN = 302; // after extraction, run/install...

  // files list view
  ID_LV_FILES = 401; // file list view

  // lines/edges (gui enhancements)
  ID_EDGE_BOTTOM = 502; // lower

  // other STATICs
  ID_ST_EXTRACTTO = 601; // "extract to"
  ID_ST_OVWGROUP = 602; // "existing files:..."
  ID_ST_FILES = 501; // "Files:"

  // radio buttons  !! don't change the order!
  ID_RB_OVERWRITE = 701; // overwrite existing files
  ID_RB_SKIP = 702; // do not overwrite existing files
  ID_RB_CONFIRM = 703; // ask before overwriting

  // progress bar
  ID_PRG_EXTRACT = 801; // extraction progress

  // language combo
  ID_LANG_COMBO = 901;
  ID_LANG_SELECT = 903;

const
  // max password length
  MAX_PASSWORD = 80;
    
const
  // request value for Dialogs for setting strings
  SFX_DLG_MAIN = 1;//$400;
  SFX_DLG_FILE = 2;//$800;
  SFX_DLG_PWRD = 3;//$C00; 
  SFX_DLG_LANG = 4;

//  SFX_DLG_MASK = $C00;

type
  // crc table type
  TCRC32Table = array[0..256] of Cardinal;

const
  // window size--must be a power OF two, and at least 32k
  WSIZE = 32768;
  RAND_HEAD_LEN = 12;
  CRC_MASK = HIGH(Cardinal);
//  CRC_MASK = {$IFNDEF DELPHI3UP}Cardinal(-1){$ELSE}$FFFFFFFF{$ENDIF};
  SFXSpanTypeNone = 0;
  SFXSpanTypeSpanned = 1;
  SFXSpanTypeMultiVol = 2;
  SFXSpanTypeUnknown = -1;

const
  { DEFLATE stuff }
  // Tables for deflate from PKZIP's appnote.txt.

  // Copy lengths FOR literal codes 257..285
  cplens: array[0..30] of word = (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17,
    19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

  // Copy offsets FOR distance codes 0..29
  cpdist: array[0..29] of word = (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49,
    65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577);

  // Extra bits FOR literal codes 257..285
  cplext: array[0..30] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2,
    2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99); { 99==invalid }

  // Extra bits FOR distance codes
  cpdext: array[0..29] of byte = (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5,
    5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13);

  // AND'ing with mask[n] masks the lower n bits
  maskr: array[0..16] of word = ($0000, $0001, $0003, $0007, $000F, $001F,
    $003F, $007F, $00FF, $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF);

  lbits = 9;
  dbits = 6;
  N_MAX = 288;

type
  PT = ^Thuft;
  Thuft = packed record
    e,
    b: shortint;
    n: word;
    Next: PT;
  end;

  BufPtr = ^BufType;
  BufType = array[0..WSIZE] of byte;

const
  { SHBrowseForFolder definitions and functions }
  BFFM_INITIALIZED = 1;
{$IFDEF UNICODE}
  BFFM_SETSELECTION = WM_USER + 103;
{$ELSE}
  BFFM_SETSELECTION = WM_USER + 102;
{$ENDIF}
  BFFM_SELCHANGED = 2;
  BIF_RETURNONLYFSDIRS = 1;
  BIF_NEWDIALOGSTYLE = $40;
  BIF_EDITBOX = $10;
  BIF_USENEWUI = (BIF_NEWDIALOGSTYLE or BIF_EDITBOX);

type
  // browsecallback
  TFNBFFCallBack = function(Wnd: HWND; uMsg: UINT;
    lParam, lpData: LPARAM): integer; stdcall;

  // TSHItemID -- Item ID
  TSHItemID = packed record { mkid }
    cb: word; { Size of the ID (including cb itself) }
    abID: array[0..0] of byte; { The item ID (variable length) }
  end;

  // TItemIDList -- List if item IDs (combined with 0-terminator)
  pItemIDList = ^TItemIDList;
  TItemIDList = packed record { idl }
    mkid: TSHItemID;
  end;

  TBrowseInfo = packed record
    hwndOwner: HWND;
    pidlRoot: pItemIDList;
//    pszDisplayName: pAnsiChar; { Return display name of item selected. }
//    lpszTitle: pAnsiChar; { text to go in the banner over the tree. }
    pszDisplayName: pChar; { Return display name of item selected. }
    lpszTitle: pChar; { text to go in the banner over the tree. }
    ulFlags: UINT; { Flags that control the return stuff }
    lpfn: TFNBFFCallBack;
    lParam: LPARAM; { extra info that's passed back in callbacks }
    iImage: integer; { output var: where to return the Image index. }
  end;


//  {$IFDEF DELPHI3UP}
  // Lucjan Lukasik
  { IMalloc interface }

  IMalloc = interface(IUnknown)
    ['{00000002-0000-0000-C000-000000000046}']
    function Alloc(cb: longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): longint; stdcall;
    function DidAlloc(pv: Pointer): integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;
//  {$ENDIF}

  PSHFileInfo = ^TSHFileInfo;
  TSHFileInfo = record
    hIcon: HICON;
    iIcon: Integer;
    dwAttributes: Cardinal;
    szDisplayName: array [0..MAX_PATH-1] of Char;
    szTypeName: array [0..79] of Char;
  end;
const
  SHGFI_SYSICONINDEX      = $000004000;
  SHGFI_SELECTED          = $000010000;
  SHGFI_SMALLICON         = $000000001;
  SHGFI_SHELLICONSIZE     = $000000004;
  SHGFI_USEFILEATTRIBUTES = $000000010;

//{$ENDIF}
const
  // progressbar defs
  PBM_SETRANGE = WM_USER + 1;
  PBM_SETPOS = WM_USER + 2;

  { strings }
  Chr_DirSep = '\';

  {! do not localize dialog resource names below !}
  { dialog resource names }
  Str_Dlg_Main = 'MAINDIALOG'; // main dialog res
  Str_Dlg_Password = 'PASSWD'; // password input box
  Str_Dlg_FileExists = 'FILEEXIST'; // overwrite file confirmation dialog
  Str_Dlg_Language = 'LANGS'; // languages dialog res
  {! do not localize dialog resource names above !}

// stuff different between delphi versions
type
//{$IFDEF DELPHI4UP} // may be incorrect (DELPHI3UP?)
//{$IFDEF VERD4up}
  TWriteFileWritten = Cardinal;
//{$ELSE}
//  TWriteFileWritten = Integer;
//{$ENDIF}

// stuff not defined in delphi 2
//{$I missing_types.inc}


// list view definitions
const
  LVFI_STRING             = $0002;

  LVCFMT_LEFT             = $0000;
  LVCFMT_RIGHT            = $0001;
  LVCFMT_CENTER           = $0002;
  LVCFMT_JUSTIFYMASK      = $0003;

  LVCF_FMT                = $0001;
  LVCF_WIDTH              = $0002;
  LVCF_TEXT               = $0004;
  LVCF_SUBITEM            = $0008;
  LVCF_IMAGE              = $0010;
  LVCF_ORDER              = $0020;

  LVSIL_SMALL             = 1;

  LVS_EX_FULLROWSELECT    = $00000020;

  LVN_FIRST               = 0-100;
  LVN_ITEMCHANGING        = LVN_FIRST-0;
  LVN_ITEMCHANGED         = LVN_FIRST-1;

  LVM_FIRST               = $1000;
  LVM_SETIMAGELIST        = LVM_FIRST + 3;
  LVM_GETITEMCOUNT        = LVM_FIRST + 4;
  LVM_GETITEMA            = LVM_FIRST + 5;
  LVM_GETITEMW            = LVM_FIRST + 75;
  LVM_SETITEMA            = LVM_FIRST + 6;
  LVM_SETITEMW            = LVM_FIRST + 76;
  LVM_INSERTITEMA         = LVM_FIRST + 7;
  LVM_INSERTITEMW         = LVM_FIRST + 77;
  LVM_FINDITEMA           = LVM_FIRST + 13;
  LVM_FINDITEMW           = LVM_FIRST + 83;
  LVM_ENSUREVISIBLE       = LVM_FIRST + 19;
  LVM_SETCOLUMNA          = LVM_FIRST + 26;
  LVM_SETCOLUMNW          = LVM_FIRST + 96;
  LVM_INSERTCOLUMNA       = LVM_FIRST + 27;
  LVM_INSERTCOLUMNW       = LVM_FIRST + 97;
  LVM_SETITEMSTATE        = LVM_FIRST + 43;
  LVM_GETITEMSTATE        = LVM_FIRST + 44;
  LVM_GETITEMTEXTA        = LVM_FIRST + 45;
  LVM_GETITEMTEXTW        = LVM_FIRST + 115;
  LVM_GETSELECTEDCOUNT    = LVM_FIRST + 50;
  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54;
{$IFDEF UNICODE}
  LVM_GETITEM  		        = LVM_GETITEMW;
  LVM_SETITEM  		        = LVM_SETITEMW;
  LVM_INSERTITEM  		    = LVM_INSERTITEMW;
  LVM_FINDITEM  		      = LVM_FINDITEMW;
  LVM_SETCOLUMN  		      = LVM_SETCOLUMNW;
  LVM_INSERTCOLUMN  		  = LVM_INSERTCOLUMNW;
  LVM_GETITEMTEXT  		    = LVM_GETITEMTEXTW;
{$ELSE}
  LVM_GETITEM  		        = LVM_GETITEMA;
  LVM_SETITEM  		        = LVM_SETITEMA;
  LVM_INSERTITEM  		    = LVM_INSERTITEMA;
  LVM_FINDITEM  		      = LVM_FINDITEMA;
  LVM_SETCOLUMN  		      = LVM_SETCOLUMNA;
  LVM_INSERTCOLUMN  		  = LVM_INSERTCOLUMNA;
  LVM_GETITEMTEXT  		    = LVM_GETITEMTEXTA;
{$ENDIF}

  CCM_FIRST               = $2000;      { Common control shared messages }
  CCM_SETUNICODEFORMAT    = CCM_FIRST + 5;
  CCM_GETUNICODEFORMAT    = CCM_FIRST + 6;

type
  PLVColumn = ^TLVColumn;
  TLVCOLUMN = packed record
    mask: Cardinal;
    fmt: Integer;
    cx: Integer;
    pszText: PChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: Integer;
    iOrder: Integer;
  end;

  PLVFindInfo = ^TLVFindInfo;
  TLVFindInfo = packed record
    flags: UINT;
    psz: PChar;
    lParam: LPARAM;
    pt: TPoint;
    vkDirection: UINT;
  end;


const
  LVIF_TEXT               = $0001;
  LVIF_IMAGE              = $0002;
  LVIF_PARAM              = $0004;
  LVIF_STATE              = $0008;

  LVIS_SELECTED           = $0002;

  ICC_LISTVIEW_CLASSES    = $00000001; // listview, header
  ICC_TREEVIEW_CLASSES    = $00000002; // treeview, tooltips
  ICC_BAR_CLASSES         = $00000004; // toolbar, statusbar, trackbar, tooltips
  ICC_TAB_CLASSES         = $00000008; // tab, tooltips
  ICC_UPDOWN_CLASS        = $00000010; // updown
  ICC_PROGRESS_CLASS      = $00000020; // progress
  ICC_HOTKEY_CLASS        = $00000040; // hotkey
  ICC_ANIMATE_CLASS       = $00000080; // animate
  ICC_WIN95_CLASSES       = $000000FF;
  ICC_DATE_CLASSES        = $00000100; // month picker, date picker, time picker, updown
  ICC_USEREX_CLASSES      = $00000200; // comboex
  ICC_COOL_CLASSES        = $00000400; // rebar (coolbar) control
  ICC_STANDARD_CLASSES    = $00004000;

type
  PLVItem = ^TLVItem;
  TLVITEM = packed record
    mask: Cardinal;
    iItem: Integer;
    iSubItem: Integer;
    state: Cardinal;
    stateMask: Cardinal;
    pszText: PChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;

//    iGroupId: Integer;
//    cColumns: Integer;{ tile view columns }
//    puColumns: PUINT;
  end;

  PNMListView = ^TNMListView;
  TNMListView = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
  end;

  PCCInitCommonControlsEx = ^TCCInitCommonControlsEx;
  TCCInitCommonControlsEx = packed record
    dwSize: DWORD;
    dwICC: DWORD;
  end;

  { api definitions }
  // initialize common controls (progress bar)
//procedure InitCommonControls; stdcall; external 'comctl32.dll' Name 'InitCommonControls';
function InitCommonControlsEx(const IntCtrls : PCCINITCOMMONCONTROLSEX): LongBool; stdcall;
  external 'comctl32.dll' Name 'InitCommonControlsEx';

{$IFDEF UNICODE}
// needed by ShBrowseForFolder
function SHBrowseForFolder(var lpbi: TBrowseInfo): pItemIDList; stdcall;
  external 'shell32.dll' Name 'SHBrowseForFolderW';

function SHGetPathFromIDList(pidl: pItemIDList; pszPath: PChar): BOOL;
  stdcall; external 'shell32.dll' Name 'SHGetPathFromIDListW';
{$ELSE}
function SHBrowseForFolder(var lpbi: TBrowseInfo): pItemIDList; stdcall;
  external 'shell32.dll' Name 'SHBrowseForFolderA';

function SHGetPathFromIDList(pidl: pItemIDList; pszPath: PAnsiChar): BOOL;
  stdcall; external 'shell32.dll' Name 'SHGetPathFromIDListA';
{$ENDIF}

//{$IFDEF DELPHI3UP}
function SHGetMalloc(var ppMalloc: IMalloc): HResult; stdcall;
  external 'shell32.dll' Name 'SHGetMalloc'; // Lucjan Lukasik
//{$ENDIF}

{$IFDEF UNICODE}
function SHGetFileInfo(pszPath: PChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfo; cbFileInfo, uFlags: Cardinal): DWORD; stdcall;
  external 'shell32.dll' name 'SHGetFileInfoW';
{$ELSE}
function SHGetFileInfo(pszPath: PAnsiChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfo; cbFileInfo, uFlags: Cardinal): DWORD; stdcall;
  external 'shell32.dll' name 'SHGetFileInfoA';
{$ENDIF}

function ShellExecute(hWnd: HWND; Operation, FileName, Parameters, Directory: PChar;
  ShowCmd: integer): HINST; stdcall; external 'shell32.dll' Name 'ShellExecuteA';

implementation

end.
