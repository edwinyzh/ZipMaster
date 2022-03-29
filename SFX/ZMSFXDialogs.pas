 (******************************************************************)
 (* SFX for DelZip v1.8                                            *)
 (* Copyright 1997, Microchip Systems / Carl Bunton                *)
 (* Email: Twojags@cris.com                                        *)
 (* Web-page: http://www.concentric.net/~twojags                   *)
 (*                                                                *)
 (* modified by Markus Stephany                                    *)
(* modified by Russell Peters, Roger Aelbrecht                     *)

(* ***************************************************************************
  ZMSFXDialogs.pas - dialog forms SFX for ZipMaster
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

	This file is part of TZipMaster Version 1.9.1.x

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
//modified 2012-04-04
unit ZMSFXDialogs;

{
this unit contains the dialog procedures of all sfx dialogs
and the select extract directory dialog wrapper

}

interface

uses Messages, Windows, ZMSFXDefs, ZMSFXInt, ZMSFXStrings;

const
  SFX_Version ='1.9.0.0030';

{$I '..\ZipVers.inc'}

// main dialog window procedure
function MainDialogProc(DlgWin: hWnd; DlgMessage: UINT; DlgWParam: WPARAM;
  DlgLParam: LPARAM): BOOL; stdcall;

// confirm overwriting
function FileExistsDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;

// ask for password
function PasswordQueryDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;
                       
// ask for locale
function LanguageQueryDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;

implementation

uses
  ZMSFXVars, ZMSFXProcs;

var
  // needed by browse for folder proc
  BrowseInfo: TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  idBrowse: pItemIDList;

// callback proc for shbrowseforfolder

function BrowseCBProc(HWindow: HWND; uMsg: integer; lParameter: LPARAM;
  lpBrowseFolder: LPARAM): integer; stdcall;
begin
  Result := 0;
  case uMsg of
    BFFM_INITIALIZED:
    begin
      // set the current path
      SendMessage(HWindow, BFFM_SETSELECTION, 1, lpBrowseFolder);
      SetWindowText(HWindow, pChar(SFXString(SFX_Cap_Browse)));
    end;
    BFFM_SELCHANGED:
    begin
      // test if the currently selected path is a valid file-system path
      if not SHGetPathFromIDList(PItemIDList(lParameter), DisplayName) then
        EnableWindow(GetDlgItem(HWindow, idOk), False);
    end;
  end;
end;

// browse for a directory using shell32 api
function BrowseDirectory(Parent: HWND; var sPath: string): boolean;
//{$IFDEF DELPHI3UP}
var
  ppMalloc: IMalloc; // Lucjan Lukasik
//{$ENDIF}
begin
  Result := False;

  with BrowseInfo do
  begin
    hWndOwner := Parent;
    pidlRoot := nil;
    pszDisplayName := DisplayName;
    lpszTitle := pChar(SFXString(SFX_Cap_Browse));
    ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI;
    lpfn := TFNBFFCallBack(@BrowseCBProc);
    lParam := integer(PChar(sPath));
  end;

  idBrowse := SHBrowseForFolder(BrowseInfo);

  if Assigned(idBrowse) then
  begin
    // convert pidl to folder name
    SetLength(sPath, MAX_PATH * 2);
    SHGetPathFromIDList(idBrowse, PChar(sPath));
    sPath  := PChar(sPath); // match length
//    {$IFDEF DELPHI3UP}
    if (SHGetMalloc(ppMalloc) and $80000000) <> 0 then // Lucjan Lukasik
      // If error, then Exit;
      Exit; // Result = False
    ppMalloc.Free(idBrowse);
//    {$ENDIF}
    Result := True;
    ChDir(sPath);
  end;
end;

// FileExistsDialogProc --- Handle messages for file exists dialog.
function FileExistsDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;
begin
  Result := True;
  case DlgMessage of
    WM_INITDIALOG:
    begin                      
      SetDlgStrings(DlgWin, SFX_DLG_FILE);
      SetDlgItemText(DlgWin, ID_EDITBOX,
        PChar(FmtStrID1(SFX_Msg_FileExists, VStr_CurrentFile)));
      SetFocus(GetDlgItem(DlgWin, ID_BTN_YES));
    end;

    WM_COMMAND:
      case LOWORD(DlgWParam) of
        ID_BTN_YES, ID_BTN_NO:
        begin
          (* No ask Overwrite again checked, show results in affected option buttons *)
          if IsDlgButtonChecked(DlgWin, ID_CB_NOASK) = BST_CHECKED then
            //SendMessage(GetDlgItem(DlgWin, ID_CB_NOASK), BM_GETCHECK, 0, 0) = 1 then
          begin
            CheckRadioButton(VH_MainWnd, ID_RB_OVERWRITE, ID_RB_CONFIRM,
              ID_RB_OVERWRITE + integer(LOWORD(DlgWParam) = ID_BTN_NO));    
            if LOWORD(DlgWParam) = ID_BTN_NO then
              VRec_SFXHeader.DefOVW := WORD(som_Skip)
            else
              VRec_SFXHeader.DefOVW := WORD(som_Overwrite);
//            VRec_SFXHeader.DefOVW := {TSFXOverwriteMode(}LOWORD(DlgWParam) = ID_BTN_NO{)};
              {(* Set confirm radiobutton to Unchecked *)
              SendMessage(GetDlgItem(VH_MainWnd, ID_RB_CONFIRM), BM_SETCHECK, 0, 0);
              if LOWORD(DlgWParam) = ID_BTN_YES then
              begin
                (* Set overwrite rb to checked *)
                SendMessage(GetDlgItem(VH_MainWnd, ID_RB_OVERWRITE), BM_SETCHECK, 1, 0);
                (* Don't ask for OverWrites *)
                VRec_SFXHeader.DefOVW := somOverwrite;
                //VBool_OverwriteFile := True;
              end
              else
              begin
                (* Set skip rb to checked *)
                SendMessage(GetDlgItem(VH_MainWnd, ID_RB_SKIP), BM_SETCHECK, 1, 0);
                (* Don't ask... skip *)
                VRec_SFXHeader.DefOVW := somSkip;
                //VBool_OverwriteFile := False;
              end;}
          end
            (*else if VRec_SFXHeader.DefOVW > somOverwrite then
              VBool_OverwriteFile := LOWORD(DlgWParam) = ID_BTN_YES*);
          EndDialog(DlgWin, LOWORD(DlgWParam));
          Exit;
        end;
      end;
  end;
  Result := False;
end;

// PasswordQueryDialogProc --- Handle messages for password dialog.
function PasswordQueryDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;
begin
  case DlgMessage of
    WM_INITDIALOG:
    begin
        SetDlgStrings(DlgWin, SFX_DLG_PWRD);
      // set the caption to reflect the current file
      SetWindowText(DlgWin, PChar(
        FmtStr1(SFXString(SFX_Cap_Password), ExtractFileName(VStr_CurrentFile))));
      (* Set input focus to the first   *)
      (* edit field.                    *)
      SetFocus(GetDlgItem(DlgWin, ID_EDITBOX));
    end;

    WM_CLOSE:
    begin
      VStr_Password := '';
      EndDialog(DlgWin, idCancel);
    end;
    WM_COMMAND:
      case LOWORD(DlgWParam) of
        ID_EDITBOX:
          if HIWORD(DlgwParam) = EN_CHANGE then
          begin
            EnableWindow(GetDlgItem(DlgWin, ID_BTN_YES),
              GetWindowTextLength(DlgLParam) > 0);
          end;
        ID_BTN_YES:
        begin
          VStr_Password := Copy(AnsiString(StrGetEditText(DlgWin)), 1, MAX_PASSWORD);
          EndDialog(DlgWin, idOk);
        end;
        ID_BTN_NO:
        begin
          VStr_Password := '';
          EndDialog(DlgWin, idCancel);
        end;
      end;
  end;
  Result := False;
end;

// LanguageQueryDialogProc --- Handle messages for language dialog.
function LanguageQueryDialogProc(DlgWin: hWnd; DlgMessage: UINT;
  DlgWParam: WPARAM; DlgLParam: LPARAM): BOOL; stdcall;
var
  tmp: pChar;
begin
  case DlgMessage of
    WM_INITDIALOG:
    begin                             
      SetDlgStrings(DlgWin, SFX_DLG_LANG);
      SetWindowText(DlgWin, pChar(SFXString(SFX_Cap_App)));
//      SetDlgItemText(DlgWin, ID_LANG_SELECT, PChar(SFXString(SFX_Btn_Select)));
      // set the language names
      SetLangStrings(GetDlgItem(DlgWin, ID_LANG_COMBO));
    end;

    WM_CLOSE:
    begin
//      VStr_Lang := '';
      VInt_CurLang := 0;
      EndDialog(DlgWin, idCancel);
    end;
    WM_COMMAND:
      case LOWORD(DlgWParam) of
        ID_BTN_YES:
        begin
          tmp := pChar(GetXBuf(512));
          GetDlgItemText(DlgWin, ID_LANG_COMBO, tmp, 200);
          VInt_CurLang := SendMessage(GetDlgItem(DlgWin, ID_LANG_COMBO),
              CB_FINDSTRINGEXACT, 0, Integer(tmp));
          EndDialog(DlgWin, idOk);
        end;
        ID_BTN_NO:
        begin
//          VStr_Lang := '';
          VInt_CurLang := 0;
//          EndDialog(DlgWin, idCancel);
          EndDialog(DlgWin, LOWORD(DlgWParam));
        end;
      end;
  end;
  Result := False;
end;

// MainDialogProc --- Handle messages for main window dialog.
var
  // we are in extraction, so we maybe can cancel
  bInExtraction: boolean;

const
  // show/hide
  SHOW_CMD: array[False..True] of integer = (SW_SHOW, SW_HIDE);

  // controls to show on "show files" button press
  SHOW_CTRLS: array[0..1] of integer = (ID_ST_FILES, ID_LV_FILES);

  // controls to reposition on hiding overwrite/run controls
  POS_CTRLS_HIDE: array[0..5] of integer = (ID_BTN_SHOWFILES, ID_CB_RUN, ID_BTN_ABOUT,
    ID_BTN_YES, ID_BTN_NO, ID_EDGE_BOTTOM);

  // controls to reposition on pressing "Show files" button
  POS_CTRLS: array[0..9] of integer = (ID_ST_OVWGROUP, ID_RB_OVERWRITE,
    ID_RB_SKIP, ID_RB_CONFIRM, ID_BTN_SHOWFILES, ID_CB_RUN, ID_BTN_ABOUT,
    ID_BTN_YES, ID_BTN_NO, ID_EDGE_BOTTOM);

  // controls to show/hide on progress bar display/hiding
  SWAP_CTRLS: array[0..3] of integer = (ID_PRG_EXTRACT, ID_ST_EXTRACTTO,
    ID_EDITBOX, ID_BTN_BROWSE);

function MainDialogProc(DlgWin: hWnd; DlgMessage: UINT; DlgWParam: WPARAM;
  DlgLParam: LPARAM): BOOL; stdcall;
var
  cm1: integer;
  i: integer;
  bOK: boolean;            
  LIntSmaller: integer;
  tmp: string;
begin (* MainDialogProc *)
  Result := True;
  with VRec_SFXHeader do
    case DlgMessage of
      WM_INITDIALOG:
      begin
        (* Assign to a global *)
        VH_MainWnd := DlgWin;
        bInExtraction := False;
        VBool_Cancelled := False;

        SetDlgStrings(DlgWin, SFX_DLG_MAIN);
        (* Set the program's icon *)
        // may 11,2002: changed
        SendMessage(DlgWin, WM_SETICON, ICON_BIG, LoadIcon(hInstance, 'MNICON'));

        // add columns to the listview (name,path,size)
        AddFilesListViewCol(DlgWin, 0, SFXString(SFX_LVC_Filename), LVCFMT_LEFT, 200);
        AddFilesListViewCol(DlgWin, 1, SFXString(SFX_LVC_Filesize), LVCFMT_RIGHT, 70);

        // set image list
        SendDlgItemMessage(DlgWin, ID_LV_FILES, LVM_SETEXTENDEDLISTVIEWSTYLE,
          LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT);
        SendDlgItemMessage(DlgWin, ID_LV_FILES, LVM_SETIMAGELIST,
          LVSIL_SMALL, VH_ShellImageList);

        cm1 := GetDlgItem(DlgWin, ID_CB_RUN); // the run... checkbox
        if (VStr_SFX_CmdLine = '') or ((so_AskCmdLine and Options) = 0) then
          // Command Line
          DestroyWindow(cm1) //## no cmd-line, so hide the run... checkbox
        else
        begin
          //## give the run... checkbox a title
          SetWindowText(cm1, PChar(GetRunCheckBoxText));

          //## check it by default
          SendMessage(cm1, BM_SETCHECK, 1, 0);
        end;
        SetDlgItemText(DlgWin, ID_EDITBOX, PChar(VStr_ExtractPath));
        (* Hilite string in Edit1 control *)
        SendMessage(GetDlgItem(DlgWin, ID_EDITBOX), EM_SETSEL, 0, $7FFF);

        (* Set overwrite mode *)
        SendMessage(GetDlgItem(DlgWin, integer(DefOVW) + ID_RB_OVERWRITE),
          BM_SETCHECK, 1, 0);
        //## and calculate the command for the overwrite-mode

        LIntSmaller := 0;
        if (so_HideOverWriteBox and Options) <> 0 then
          //## if we do not want to select another overwrite-mode, destroy the controls
        begin
          LIntSmaller := 23;
          DestroyWindow(GetDlgItem(DlgWin, ID_RB_OVERWRITE));
          DestroyWindow(GetDlgItem(DlgWin, ID_RB_SKIP));
          DestroyWindow(GetDlgItem(DlgWin, ID_RB_CONFIRM));
          DestroyWindow(GetDlgItem(DlgWin, ID_ST_OVWGROUP));
        end;

        if (so_AskCmdLine and Options) = 0 then
        begin
          if LIntSmaller <> 0 then
          begin
            //ShowWindow(GetDlgItem(DlgWin, ID_EDGE_BOTTOM), SW_HIDE);
            Inc(LIntSmaller, 5);
          end;
          Inc(LIntSmaller, 10);
        end;

        if LIntSmaller <> 0 then
        begin
          ResizeControl(DlgWin, False, -LIntSmaller);
          for i := Low(POS_CTRLS_HIDE) to High(POS_CTRLS_HIDE) do
            ResizeControl(GetDlgItem(DlgWin, POS_CTRLS_HIDE[i]), True, -LIntSmaller);
        end;

        Windows.SetWindowText(DlgWin, PChar(VStr_SFX_Caption));
//        SendDlgItemMessage(DlgWin, ID_LV_FILES, WM_SETFONT, 0, 0);

        (* Fill the list view and select all items *)
        FillListView(DlgWin);
        if (so_InitiallyHideFiles and Options) = 0 then
          // show files listview
          SendMessage(DlgWin, WM_COMMAND, ID_BTN_SHOWFILES, 0)
        else
        if (so_ForceHideFiles and Options) <> 0 then
          // no "show files" button
          DestroyWindow(GetDlgItem(DlgWin, ID_BTN_SHOWFILES));

        if (so_AutoRun and Options) <> 0 then
//        if soAutoRun in Options then
          // do the extraction automatically, if autorun = true
          SetTimer(DlgWin, 0, 20, nil);
        //PostMessage(DlgWin, WM_COMMAND, ID_BTN_YES, 0);
      end;

      WM_TIMER:
      begin
        KillTimer(DlgWin, 0);
        PostMessage(DlgWin, WM_COMMAND, ID_BTN_YES, 0);
      end;

      (* Handle button presses, etc. *)
      WM_NOTIFY:
        case DlgWParam of
          ID_LV_FILES:
          begin
            case PNMHDR(DlgLParam)^.code of
              LVN_ITEMCHANGED:
              begin
                if ((so_AskFiles and Options) <> 0) and (not bInExtraction) then
                  EnableWindow(GetDlgItem(DlgWin, ID_BTN_YES),
                    (SendDlgItemMessage(DlgWin, ID_LV_FILES,
                    LVM_GETSELECTEDCOUNT, 0, 0) > 0));
              end;
            end;
          end;
        end;
      WM_COMMAND:
        case LOWORD(DlgWParam) of
          ID_ST_FILES:
            // 09/18/2002 -> select the files list by pressing the "Files" shortcut key
            SetFocus(GetDlgItem(DlgWin, ID_LV_FILES));
          ID_BTN_SHOWFILES:
          begin
            // show files
            for i := Low(SHOW_CTRLS) to High(SHOW_CTRLS) do
              ShowWindow(GetDlgItem(DlgWin, SHOW_CTRLS[i]), SW_SHOW);
            ResizeControl(DlgWin, False, 66);
            for i := Low(POS_CTRLS) to High(POS_CTRLS) do
              ResizeControl(GetDlgItem(DlgWin, POS_CTRLS[i]), True, 66);
            SetFocus(GetDlgItem(DlgWin, ID_LV_FILES));
            DestroyWindow(GetDlgItem(DlgWin, ID_BTN_SHOWFILES));
          end;

          //## added the ability to select a extract directory
          // Modified by Deepu Chandy Thomas //
          ID_BTN_BROWSE:
          begin
            FillChar(VStr_ExtractPath, sizeof(VStr_ExtractPath), 0);
            VStr_ExtractPath := StrGetEditText(DlgWin);
            if BrowseDirectory(DlgWin, VStr_ExtractPath) then
              SetDlgItemText(DlgWin, ID_EDITBOX, PChar(VStr_ExtractPath));
          end;

          // show the copyright information
          ID_BTN_ABOUT:
          begin
            tmp := SFXString(-SFX_Cap_App) + ' version ' + SFX_Version;
{$IFDEF UNICODE}
            i := SFX_MSG_ABOUT0;
{$ELSE}
            i := SFX_MSG_ABOUT1;
{$ENDIF}
            while i <= SFX_MSG_TRANSLATION do
            begin
              tmp := tmp + SFXString(i);
              inc(i);
            end;
            MsgBox(DlgWin, tmp, SFXString(SFX_Cap_About), MB_OK);
          end;

          ID_RB_OVERWRITE:
            VRec_SFXHeader.DefOVW := WORD(som_Overwrite);

          ID_RB_SKIP:
            VRec_SFXHeader.DefOVW := WORD(som_Skip);

          ID_RB_CONFIRM:
            VRec_SFXHeader.DefOVW := WORD(som_Ask);

          ID_BTN_YES:
          begin
            //## if the user is not allowed to (de)select files from the archive, then
            //   select them all
            VBool_Cancelled := False;
            if (so_AskFiles and Options) = 0 then
              SelectAllInFilesListView(DlgWin);

            VStr_ExtractPath := RemoveDirSeparator(StrGetEditText(DlgWin));

            if VStr_ExtractPath = '' then
            begin
              VStr_ExtractPath := GetCurDir;
              SetDlgItemText(DlgWin, ID_EDITBOX, PChar(VStr_ExtractPath));
              Exit;
            end;

            EnableChildren(DlgWin, False);
            // re-enable close-button
            if (so_CanBeCancelled and Options) <> 0 then
              EnableWindow(GetDlgItem(DlgWin, ID_BTN_NO), True);

            // show progress bar
            for i := Low(SWAP_CTRLS) to High(SWAP_CTRLS) do
              ShowWindow(GetDlgItem(DlgWin, SWAP_CTRLS[i]),
                SHOW_CMD[i <> Low(SWAP_CTRLS)]);

            // do extraction
            bInExtraction := True;
            bOK := Extract(DlgWin);
            bInExtraction := False;

            // hide progress bar
            for i := Low(SWAP_CTRLS) to High(SWAP_CTRLS) do
              ShowWindow(GetDlgItem(DlgWin, SWAP_CTRLS[i]),
                SHOW_CMD[i = Low(SWAP_CTRLS)]);

            // enable controls
            EnableChildren(DlgWin, True);

            if not bOK then
            begin
              if (so_HideOverwriteBox and Options) <> 0 then
                DefOVW := som_Ask;

              if not VBool_Cancelled then
              begin
                if IsWindow(GetDlgItem(DlgWin, ID_BTN_SHOWFILES)) then
                  // show files if something went wrong
                  SendMessage(DlgWin, WM_COMMAND, ID_BTN_SHOWFILES, 0);
                if (so_ForceHideFiles and Options) <> 0 then
                  ErrorMsgBoxFmt1(DlgWin, SFX_Msg_SomeNotExtracted, '')
                else
                  ErrorMsgBoxFmt1(DlgWin, SFX_Msg_SelNotExtracted, '');
              end;
            end
            else
            begin
              if ((Options and (so_SuccessAlways)) <> 0) or
                  ((Options and (so_AutoRun or so_NoSuccessMsg)) = 0) then
                MsgBox(DlgWin, SFXString(SFX_Msg_AllExtracted),
                    SFXString(SFX_Cap_Finished), MB_OK);
              if ((so_AskCmdLine and Options) = 0) or
                (SendMessage(GetDlgItem(DlgWin, ID_CB_RUN), BM_GETCHECK, 0, 0) = 1) then
                if (VStr_SFX_CmdLine <> '') and (ExecuteCMD < 32) then
                  ErrorMsgBox(DlgWin, GetRunErrorMessage);
              EndDialog(DlgWin, idOk);
            end;

            if (so_AutoRun and Options) <> 0 then
              PostMessage(DlgWin, WM_COMMAND, ID_BTN_NO, 0);
          end;

          ID_BTN_NO:
          begin
            if not bInExtraction then
            begin
              VStr_ExtractPath := StrGetEditText(DlgWin);
              if DirectoryExists(VStr_ExtractPath) then
                ChDir(VStr_ExtractPath);
              EndDialog(DlgWin, LOWORD(DlgWParam));
              Exit;
            end
            else
            begin
              if ((so_CanBeCancelled and Options) <> 0) and
                (VBool_Cancelled or (MsgBox(DlgWin,
                    SFXString(SFX_Msg_QueryCancel),
                VStr_SFX_Caption, MB_YESNO or MB_ICONWARNING) = idYes)) then
              begin
                VBool_Cancelled := True;
                PostMessage(DlgWin, WM_CLOSE, DlgWParam, DlgLParam);
                bInExtraction := False;
              end;
            end;
          end;
        end;
    end;
  Result := False;
end;

end.
