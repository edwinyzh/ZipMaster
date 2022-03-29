unit ZMSFXWinTrust;
(************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

   This file is part of TZipMaster Version 1.9.

    TZipMaster is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TZipMaster is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

    contact: problems@delphizip.org (include ZipMaster in the subject).
    updates: http://www.delphizip.org
    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip 
************************************************************************)
(*
  An adaption of nvWinTrust.pas
  for use with ZMSFX
*)

interface

uses Windows;


function nvVerifyTrust(const FileName: string; WTD_FLAGS: DWORD = $FFFFFFFF):
    DWORD;
 { Returns 0 if successful, otherwise result may be passed to SysErrorMessage. }
 { Returns 0 if not supported by Windows.                                      }
 { This is intended for use verifying file integrity.                          }

implementation


{ Sample return codes - others may be returned : only zero indicates success }
const
  CRYPT_E_SECURITY_SETTINGS = $80092026;
  { The cryptographic operation failed due to a local security option setting. }
  TRUST_E_PROVIDER_UNKNOWN = $800B0001;
  { The trust provider is not recognized on this system.                       }
  TRUST_E_ACTIONUNKNOWN = $800B0002;
  { The trust provider does not support the specified action.                  }
  TRUST_E_SUBJECT_FORM_UNKNOWN = $800B0003;
  { The trust provider does not support the form specified for the subject.    }
  TRUST_E_SUBJECT_NOT_TRUSTED = $800B0004;
  { The subject is not trusted for the specified action.                       }
  TRUST_E_NOSIGNATURE = $800B0100;
  { No signature was present in the subject.                                   }
  TRUST_E_EXPLICIT_DISTRUST = $800B0111;
{ The certificate was explicitly marked as untrusted by the user.            }

const
  WTD_UI_ALL  = 1;
  WTD_UI_NONE = 2;
  WTD_UI_NOBAD = 3;
  WTD_UI_NOGOOD = 4;

  WTD_REVOKE_NONE = 0;
  WTD_REVOKE_WHOLECHAIN = 1;

  WTD_CHOICE_FILE = 1;
  WTD_CHOICE_CATALOG = 2;
  WTD_CHOICE_BLOB = 3;
  WTD_CHOICE_SIGNER = 4;
  WTD_CHOICE_CERT = 5;

  WTD_STATEACTION_IGNORE = 0;
  WTD_STATEACTION_VERIFY = 1;
  WTD_STATEACTION_CLOSE  = 2;
  WTD_STATEACTION_AUTO_CACHE = 3;
  WTD_STATEACTION_AUTO_CACHE_FLUSH = 4;

  WTD_PROV_FLAGS_MASK = $0000FFFF;
  WTD_USE_IE4_TRUST_FLAG = $00000001;
  WTD_NO_IE4_CHAIN_FLAG = $00000002;
  WTD_NO_POLICY_USAGE_FLAG = $00000004;
  WTD_REVOCATION_CHECK_NONE = $00000010;
  WTD_REVOCATION_CHECK_END_CERT = $00000020;
  WTD_REVOCATION_CHECK_CHAIN = $00000040;
  WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $00000080;
  WTD_SAFER_FLAG = $00000100;
  WTD_HASH_ONLY_FLAG = $00000200;
  WTD_USE_DEFAULT_OSVER_CHECK = $00000400;
  WTD_LIFETIME_SIGNING_FLAG = $00000800;
  WTD_CACHE_ONLY_URL_RETRIEVAL = $00001000;

  WTD_UICONTEXT_EXECUTE = 0;
  WTD_UICONTEXT_INSTALL = 1;

  WINTRUST_ACTION_GENERIC_VERIFY: TGUID =
    '{189A3842-3041-11D1-85E1-00C04FC295EE}';
  { Verify certificate chain only }
(*
  WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID =
    '{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}';
{ Verify a file or object using the Authenticode policy provider }
*)
type
  TWinTrustFileInfo = packed record
    cbStruct: DWORD;   // required, size of structure
    pcwszFilePath: pWChar;  // required, name of file name to be verified
    hFile: THANDLE; // optional
    pgKnownSubject: pGUID;   // optional
  end;

type
  TWinTrustData = packed record
    cbStruct: DWORD;    // required, size of structure
    pPolicyCallbackData: pointer;  // optional
    pSIPClientData: pointer;  // optional
    dwUIChoice: DWORD;    // required
    fdwRevocationChecks: DWORD;    // required (but zero is normally used)
    dwChoice: DWORD;
    // required : identifies which structure is being passed through pChoiceData
    pChoiceData: pointer;  // required
    dwStateAction: DWORD;    // optional
    hWVTStateData: THandle;  // optional
    pwszURLReference: pWChar;   // optional
    dwProvFlags: DWORD;
    // optional : WTD_REVOCATION_CHECK_NONE is used to avoid connecting to the internet
    dwUIContext: DWORD;    // optional
  end;

//var
//  hWinTrust: HMODULE;
//  didLoad: Boolean;
//  pWinTrustFunc: function(WND: HWND; const ActionID: TGUID; const ActionData: TWinTrustData): DWORD;
//    stdcall;

function WinVerifyTrust(WND: HWND; const ActionID: TGUID;
  const ActionData: TWinTrustData): DWORD;
const
  dll = 'Wintrust.dll';
var
  hWinTrust: HMODULE;
//  didLoad: Boolean;
  pWinTrustFunc: function(WND: HWND; const ActionID: TGUID; const ActionData: TWinTrustData): DWORD;
    stdcall;
begin
//  if not didLoad then
//  begin
    Result := DWORD(E_NOTIMPL);
    @pWinTrustFunc := nil;
//    didLoad := True;
//    hWinTrust := GetModuleHandle(dll);
//    if hWinTrust = 0 then
    hWinTrust := LoadLibrary(dll);

    if hWinTrust <> 0 then
    begin
      try
        pWinTrustFunc := GetProcAddress(hWinTrust, 'WinVerifyTrust');
        if @pWinTrustFunc <> nil then
          Result := pWinTrustFunc(WND, ActionID, ActionData);
      finally
        FreeLibrary(hWinTrust);
      end;
    end;
//      pWinTrustFunc := GetProcAddress(hWinTrust, 'WinVerifyTrust');
//  end;

//  if (hWinTrust = 0) or (@pWinTrustFunc = nil) then
//    Result := DWORD(E_NOTIMPL)
//  else
//    Result := pWinTrustFunc(WND, ActionID, ActionData);
end;

function nvVerifyTrust(const FileName: string; WTD_FLAGS: DWORD = $FFFFFFFF):
    DWORD;
  { Returns 0 if successful, otherwise result may be passed to SysErrorMessage. }
  { Returns 0 if not supported by Windows.                                      }
  { This is intended for use verifying file integrity.                          }
var
{$IFNDEF UNICODE}
  buff: array[0..MAX_PATH] of Widechar;
{$ENDIF}
  td: TWinTrustData;
  fi: TWinTrustFileInfo;
begin
//  if (FileName = nil) or (FileName^ = #0) then
  if (FileName = '') then
  begin
    Result := ERROR_INVALID_PARAMETER;
    exit;
  end;

  if WTD_FLAGS = $FFFFFFFF then
    WTD_FLAGS := WTD_REVOCATION_CHECK_NONE or WTD_HASH_ONLY_FLAG;

  ZeroMemory(@fi, SizeOf(fi));
  ZeroMemory(@td, SizeOf(td));
{$IFDEF UNICODE}
  fi.pcwszFilePath := PChar(FileName);
{$ELSE}
  MultiByteToWideChar(0, 0, PChar(FileName), -1, Buff, Length(Buff));
  fi.pcwszFilePath := buff;
{$ENDIF}

  fi.cbStruct := SizeOf(fi);
//  fi.pcwszFilePath := buff;

  td.cbStruct := SizeOf(td);
  td.dwProvFlags := WTD_FLAGS;
  td.dwUIChoice := WTD_UI_NONE;
  { No user interaction                              }
  td.dwChoice := WTD_CHOICE_FILE;
  { pChoice identifies a TWinTrustFileInfo structure }
  td.pChoiceData := @fi;

  Result := WinVerifyTrust(INVALID_HANDLE_VALUE,
    WINTRUST_ACTION_GENERIC_VERIFY, td);

  if Result = DWORD(E_NOTIMPL) then
    Result := 0;     { Report success on old versions of Windows }
end;

{ NOTE : Use of the API functions CertGetCertificateChain, CertVerifyCertificateChainPolicy and CertFreeCertificateChain }
{      : is recommended by Microsoft to perform certificate verification, however, the method above seems to work fine.  }

//initialization
//
//  didLoad := False;
//  hWinTrust:= 0;//LoadLibrary(WINTRUST_LIB);
////  gdwError:=GetLastError;
//
//finalization
//
//  if didLoad and (hWinTrust <> 0) then
//    FreeLibrary(hWinTrust);

end.

