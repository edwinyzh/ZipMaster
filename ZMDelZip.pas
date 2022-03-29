unit ZMDelZip;

// ZMDelZip.pas - port of DelZip.h (dll interface)

(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

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
// modified 2013-01-23

interface

uses
{$IFDEF VERDXE2up}
  WinApi.Windows,
{$ELSE}
  Windows;
{$ENDIF}
(* zacArg - return string
  Arg1 = argument
 0 = filename
 1 = password
 2 = RootDir
 3 = ExtractDir
 4 = FSpecArgs      Arg3 = Index
 5 = FSpecArgsExcl  Arg3 = Index
*)

type
  TActionCodes = (ZacTick, ZacItem, ZacProgress, ZacEndOfBatch, ZacCount,
    ZacSize, ZacXItem, ZacXProgress, ZacMessage, ZacNewName, ZacPassword,
    ZacCRCError, ZacOverwrite, ZacSkipped, ZacComment, ZacData, ZacExtName,
    ZacNone, ZacKey, ZacArg, ZacWinErr);

  TCBArgs = (ZcbFilename, ZcbPassword, ZcbRootDir, ZcbExtractDir, ZcbComment,
    ZcbFSpecArgs, ZcbFSpecArgsExcl, ZcbSpecials, ZcbTempPath);

  TZStreamActions = (ZsaIdentify, ZsaCreate, ZsaClose, ZsaPosition, ZsaRead,
    ZsaWrite);

const
  ZCallBack_Check = $0707;
  ZStream_Check = $070B;

  { All the items in the CallBackStruct are passed to the Delphi
    program from the DLL.  Note that the "Caller" value returned
   here is the same one specified earlier in ZipParms by the
   Delphi pgm. }
type
  TZCallBackStruct = packed record
    Caller: Pointer; // "self" reference of the Delphi form }
    Version: Longint; // version no. of DLL }
    IsOperationZip: Longbool; // True=zip, False=unzip }
    ActionCode: Integer;
    HaveWide: Integer; // wide string passed
    MsgP: PByte; // pointer to text/data or stream src/dst
    Msg2P: PByte; // orig file comment
    File_Size: Int64; // file size or stream position offset
    Written: Int64;
    Arg1: Cardinal;
    Arg2: Cardinal;
    Arg3: Integer; // 'older', stream cnt or from
    Check: Cardinal;
  end;

  PZCallBackStruct = ^TZCallBackStruct;

  // ALL interface structures BYTE ALIGNED
  (* stream operation arg usage
    zacStIdentify,
   //      IN BufP = name
   IN Number = number
   OUT ArgLL = size, ArgD = Date, ArgA = Attrs
   zacStCreate,
   //      IN BufP = name
   IN Number = number
   OUT StrmP = stream
   zacStClose,
   IN Number = number
   IN StrmP = stream
   OUT StrmP = stream (= NULL)
   zacStPosition,
   IN Number = number
   IN StrmP = stream, ArgLL = offset, ArgI = from
   OUT ArgLL = position
   zacStRead,
   IN Number = number
   IN StrmP = stream, BufP = buf, ArgI = count
   OUT ArgI = bytes read
   zacStWrite
   IN Number = number
   IN StrmP = stream, BufP = buf, ArgI = count
   OUT ArgI = bytes written
  *)
type
  TZStreamRec = packed record
    Caller: Pointer; // "self" reference of the Delphi form }
    Version: Longint; // version no. of DLL }
    StrmP: Pointer; // pointer to 'tstream'
    Number: Integer;
    OpCode: Integer; // TZStreamActions
    BufP: PByte; // pointer to stream src/dst or identifier
    ArgLL: Int64; // file size or stream position offset
    ArgI: Integer; // stream cnt or from
    ArgD: Cardinal; // date
    ArgA: Cardinal; // attribs
    Check: Cardinal; // ZStream_Check;
  end;

  PZStreamRec = ^TZStreamRec;

  (* Declare a function pointer type for the BCB/Delphi callback function, to
    * be called by the DLL to pass updated status info back to BCB/Delphi.*)
type
  TZFunctionPtrType = function(ZCallbackRec: PZCallBackStruct)
    : Longint; stdcall;

  TZStreamFunctionPtrType = function(ZStreamRec: PZStreamRec): Longint; stdcall;

type
  PZSSArgs = ^TZSSArgs;

  TZSSArgs = packed record // used stream-stream
    Method: Cardinal; // low word = method, hi word nz=encrypt
    CRC: Cardinal; // IN init encrypt crc OUT crc
    Size: Int64;
    FSSInput: Pointer;
    FSSOutput: Pointer;
  end;

  (* These records are very critical.  Any changes in the order of items, the
    size of items, or modifying the number of items, may have disasterous
   results.  You have been warned! *)
const
  DLLCOMMANDCHECK = $03070505;
  DLL_OPT_OpIsZip = $0000001;
  // DLL_OPT_OpIsDelete        = $0000002; // delete - not used?
  // DLL_OPT_OpIsUnz           = $0000004;
  // DLL_OPT_OpIsTest          = $0000008;
  DLL_OPT_CanWide = $0000010;
  DLL_OPT_Quiet = $0000020;
  // DLL_OPT_NoSkip            = $0000040; // skipping is fatal
  DLL_OPT_Update = $0000080;
  DLL_OPT_Freshen = $0000100;
  DLL_OPT_Directories = $0000200; // extract directories
  DLL_OPT_Overwrite = $0000400; // overwrite all
  DLL_OPT_NoDirEntries = $0000800;
  DLL_OPT_JunkDir = $0001000;
  DLL_OPT_Recurse = $0002000;
  DLL_OPT_Grow = $0004000;
  // DLL_OPT_Force             = $0008000; // Force to DOS 8.3
  DLL_OPT_Move = $0010000;
  DLL_OPT_System = $0020000;
  DLL_OPT_JunkSFX = $0040000; // remove sfx stub
  DLL_OPT_LatestTime = $0080000; // set zip to latest file
  DLL_OPT_ArchiveFilesOnly = $0100000; // zip when archive bit set
  DLL_OPT_ResetArchiveBit = $0200000;
  // reset the archive bit after successfull zip
  DLL_OPT_Versioning = $0400000; // rename old version instead of replace
  DLL_OPT_HowToMove = $0800000;
  DLL_OPT_NoPrecalc = $1000000; // don't precalc crc when encrypt
  DLL_OPT_Encrypt = $2000000; // General encrypt, if not superseded
  DLL_OPT_Volume = $4000000;
  DLL_OPT_NTFSStamps = $8000000; // Generate or use NTFS time stamps

type
  TDLLCommands = packed record
    FHandle: HWND;
    FCaller: Pointer;
    FVersion: Longint;
    ZCallbackFunc: TZFunctionPtrType;
    ZStreamFunc: TZStreamFunctionPtrType;
    FVerbosity: Integer;
    FEncodedAs: Cardinal; // Assume name encoded as (auto, raw, utf8, oem)
    FSS: PZSSArgs; // used stream-stream
    FFromPage: Cardinal; // country to use
    FOptions: Cardinal; // DLL_OPT_?
    FPwdReqCount: Cardinal;
    FEncodeAs: Cardinal; // encode names as
    FLevel: Integer;
    // General Date, if not superseded by FileData.fDate
    FDate: Cardinal;
    FNotUsed: array [0 .. 3] of Cardinal;
    FCheck: Cardinal;
  end;

  PDLLCommands = ^TDLLCommands;

type
  TDLLExecFunc = function(Rec: PDLLCommands): Integer; stdcall;
  TDLLVersionFunc = function: Integer; stdcall;
  TDLLPrivVersionFunc = function: Integer; stdcall;
  TAbortOperationFunc = function(Rec: Cardinal): Integer; stdcall;
  TDLLPathFunc = function: PAnsiChar; stdcall;
  TDLLBannerFunc = function: PAnsiChar; stdcall;
  TDLLNameFunc = function(var Buf; Bufsiz: Integer; Wide: Boolean)
    : Integer; stdcall;

const
  _DZ_ERR_GOOD = 0; // ZEN_OK
  _DZ_ERR_CANCELLED = 1;
  _DZ_ERR_ABORT = 2; (*
   _DZ_ERR_CALLBACK  = 3;
   _DZ_ERR_MEMORY   = 4;
   _DZ_ERR_STRUCT   = 5;
   _DZ_ERR_ERROR   = 6;
   _DZ_ERR_PASSWORD_FAIL  = 7;
   _DZ_ERR_PASSWORD_CANCEL = 8;
   _DZ_ERR_INVAL_ZIP      = 9 ; // ZEN_FORM
   _DZ_ERR_NO_CENTRAL     = 10;  // UEN_EOF01
   _DZ_ERR_ZIP_EOF        = 11;  // ZEN_EOF
   _DZ_ERR_ZIP_END        = 12;  // UEN_EOF02
   _DZ_ERR_ZIP_NOOPEN     = 13;
   _DZ_ERR_ZIP_MULTI      = 14;
   _DZ_ERR_NOT_FOUND      = 15;
   _DZ_ERR_LOGIC_ERROR    = 16;  // ZEN_LOGIC
   _DZ_ERR_NOTHING_TO_DO  = 17;  // ZEN_NONE
   _DZ_ERR_BAD_OPTIONS    = 18;  // ZEN_PARM
   _DZ_ERR_TEMP_FAILED    = 19;  // ZEN_TEMP
   _DZ_ERR_NO_FILE_OPEN   = 20;  // ZEN_OPEN
   _DZ_ERR_ERROR_READ     = 21;  // ZEN_READ
   _DZ_ERR_ERROR_CREATE   = 22;  // ZEN_CREAT
   _DZ_ERR_ERROR_WRITE    = 23;  // ZEN_WRITE
   _DZ_ERR_ERROR_SEEK     = 24;
   _DZ_ERR_EMPTY_ZIP      = 25;
   _DZ_ERR_INVAL_NAME     = 26;
   _DZ_ERR_GENERAL        = 27;
   _DZ_ERR_MISS           = 28;  // ZEN_MISS UEN_MISC03
   _DZ_ERR_WARNING        = 29;  // PK_WARN
   _ZD_ERR_ERROR_DELETE   = 30;  // PK_NODEL
   _ZD_ERR_FATAL_IMPORT   = 31;
   _ZD_ERR_SKIPPING       = 32;
   _ZD_ERR_LOCKED         = 33;
   _ZD_ERR_DENIED         = 34;
   _ZD_ERR_DUPNAME        = 35;
   _DZ_ERR_NOSKIP         = 36;
  *)
  _DZ_ERR_MAX = 36;

  // ZD_SKIPPING       = 37;
  // DLLPARAMCHECK      = $07070505;
  (*    Message code format
    0FFF FFFF  LLLL LLLL   LLLL MTTT  EEEE EEEE  {31 .. 0}
   F = file number (7 bits = 128 files)
   L = line number (12 bits=4096 lines)
   M = message instead of error string
   T = type  (3 bits=8)
   E = error/string code (8 bits = 256 errors)
  *)
  DZM_Type_Mask = $700;
  DZM_General = $000;
  DZM_Error = $600; // 1 1 x (E... is identifier)
  DZM_Warning = $400; // 1 0 x
  DZM_Trace = $300; // 0 1 1
  DZM_Verbose = $100; // 0 0 1
  DZM_Message = $200; // 0 1 0 (E... is identifier)

  DZM_MessageBit = $800; // mask for message bit

  // callback return values
  // CALLBACK_FALSE     =      0;
  CALLBACK_UNHANDLED = 0;
  CALLBACK_TRUE = 1;
  CALLBACK_2 = 2;
  CALLBACK_3 = 3;
  CALLBACK_4 = 4;
  CALLBACK_IGNORED = -1; // invalid ActionCode
  CALLBACK_CANCEL = -2; // user cancel
  CALLBACK_ABORT = -3;
  CALLBACK_EXCEPTION = -4; // handled exception
  CALLBACK_ERROR = -5; // unknown error

const
{$IFDEF WIN64}
  DelZipDLL_Name = 'DelZip192x64.dll';
{$ELSE}
  DelZipDLL_Name = 'DelZip192.dll';
{$ENDIF}
  DelZipDLL_Execfunc = 'DZ_Exec';
  DelZipDLL_Abortfunc = 'DZ_Abort';
  DelZipDLL_Versfunc = 'DZ_Version';
  DelZipDLL_Privfunc = 'DZ_PrivVersion';
  DelZipDLL_Pathfunc = 'DZ_Path';
  DelZipDLL_Bannerfunc = 'DZ_Banner';
  DelZipDLL_Namefunc = 'DZ_Name';
  (*
   // 'static' loaded dll functions
   function DZ_Exec(C: pDLLCommands): Integer; STDCALL; EXTERNAL DelZipDLL_Name;
   function DZ_Abort(C: Cardinal): Integer; STDCALL; EXTERNAL DelZipDLL_Name;
   function DZ_Version: Integer; STDCALL; EXTERNAL DelZipDLL_Name;
   function DZ_PrivVersion: Integer; STDCALL; EXTERNAL DelZipDLL_Name;
   function DZ_Path: pChar; STDCALL; EXTERNAL DelZipDLL_Name;
   function DZ_Banner: pChar; STDCALL; EXTERNAL DelZipDLL_Name;
  *)

implementation

end.
