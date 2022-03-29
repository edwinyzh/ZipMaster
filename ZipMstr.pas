unit ZipMstr;

// ZipMstr.pas - main component

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
// modified 2014-02-23
{$I   '.\ZipVers.inc'}
{$I   '.\ZMConfig192.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, VCL.Forms, System.SysUtils, VCL.Graphics, VCL.Dialogs,
  WinApi.Windows, VCL.Controls,
{$ELSE}
  Classes, Forms, SysUtils, Graphics, Dialogs, Windows, Controls,
{$IFDEF VERPre6} ZMCompat, {$ENDIF}
{$ENDIF}
  ZMHandler;

const
  ZIPMASTERBUILD: string =  '1.9.2.0021';
  ZIPMASTERDATE: string =  '29/03/2016';
  ZIPMASTERPRIV: Integer = 1920021;
  DELZIPVERSION = 192;
  MIN_DLL_BUILD = 1920002;

const
  ZMPWLEN = 80;

type
{$IFDEF UNICODE}
  TZMWideString = string;
  TZMRawBytes = RawByteString;
{$ELSE}
  TZMWideString = WideString;
  TZMRawBytes = AnsiString;
{$ENDIF}

type
  TZMStates = (ZsDisabled, ZsIdle, ZsReentered, ZsBusy, ZsReentry);

  // options when editing a zip
  TZMAddOptsEnum = (AddDirNames, AddRecurseDirs, AddMove, AddFreshen, AddUpdate,
    AddHiddenFiles, AddArchiveOnly, AddResetArchive, AddEncrypt, AddEmptyDirs,
    AddVolume, AddFromDate, AddVersion, AddNTFS);
  TZMAddOpts = set of TZMAddOptsEnum;

  // the EncodeAs values (writing) -
  // zeoUPATH - convert to Ansi but have UTF8 proper name in data
  // zeoUTF  - convert to UTF8
  // zeoOEM  - convert to OEM
  // zeoNone - store 'as is' (Ansi on Windows)
  // 'default' (zeoAuto) - [in order of preference]
  // is Ansi - use zeoNone
  // can be converted to Ansi - use zeoUPath (unless comment also extended)
  // use zeoUTF8

  // Encoded (reading)
  // zeoUPATH- use UPATH if available
  // zeoUTF  - assume name is UTF8 - convert to Ansi/Unicode
  // zeoOEM  - assume name is OEM - convert to Ansi/Unicode
  // zeoNone - assume name is Ansi - convert to Ansi/Unicode
  // zeoAuto - unless flags/versions say otherwise, or it has UTF8 name in data,
  // treat it as OEM (FAT) / Ansi (NTFS)
  TZMEncodingOpts = (ZeoAuto, ZeoNone, ZeoOEM, ZeoUTF8, ZeoUPath);

  // When changing this enum also change the pointer array in the function AddSuffix,
  // and the initialisation of ZipMaster.
  TZMAddStoreSuffixEnum = (AssGIF, AssPNG, AssZ, AssZIP, AssZOO, AssARC, AssLZH,
    AssARJ, AssTAZ, AssTGZ, AssLHA, AssRAR, AssACE, AssCAB, AssGZ, AssGZIP,
    AssJAR, AssEXE, AssEXT, AssJPG, AssJPEG, Ass7Zp, AssMP3, AssWMV, AssWMA,
    AssDVR, AssAVI);

  TZMAddStoreExts = set of TZMAddStoreSuffixEnum;

  TZMSpanOptsEnum = (SpNoVolumeName, SpCompatName, SpWipeFiles, SpTryFormat,
    SpAnyTime, SpExactName);
  TZMSpanOpts = set of TZMSpanOptsEnum;

  // options for when reading a zip file
  TZMExtrOptsEnum = (ExtrDirNames, ExtrOverWrite, ExtrFreshen, ExtrUpdate,
    ExtrTest, ExtrForceDirs, ExtrNTFS, ExtrSafe);
  TZMExtrOpts = set of TZMExtrOptsEnum;

  // options for when writing a zip file
  TZMWriteOptsEnum = (ZwoDiskSpan, // write multipart
    ZwoZipTime, // set zip time to latest entry time
    ZwoForceDest, // force destination folder
    ZwoSafe, // write to temporary before replacing existing zip
    ZwoSkipDup, // do not write duplicate names
    ZwoAutoDup, // rename duplicates via OnWriteDupName
    ZwoAllowDup, // write duplicate names
    ZwoExtDup); // rename by appending {xxxx} xxxx is encoded time
  TZMWriteOpts = set of TZMWriteOptsEnum;

  // other options
  TZMMergeOpts = (ZmoConfirm, ZmoAlways, ZmoNewer, ZmoOlder, ZmoNever,
    ZmoRename);
  TZMOvrOpts = (OvrAlways, OvrNever, OvrConfirm);

  TZMReplaceOpts = (RplConfirm, RplAlways, RplNewer, RplNever);

  TZMDeleteOpts = (HtdFinal, HtdAllowUndo);

  TZMRenameOpts = (HtrDefault, HtrOnce, HtrFull);

  TZMSkipTypes = (StNoSkip, StOnFreshen, StNoOverwrite, StFileExists,
    StBadPassword, StBadName, StCompressionUnknown, StUnknownZipHost,
    StZipFileFormatWrong, StGeneralExtractError, StUser, StCannotDo, StNotFound,
    StNoShare, StNoAccess, StNoOpen, StDupName, StReadError, StSizeChange,
    StNothingToDo, StCRCError, StPathError, StWarning);
  TZMSkipAborts = set of TZMSkipTypes;

type
  TZMProblem = class
  private
  protected
    function GetErrCode: Integer; virtual; abstract;
    function GetSkipCode: TZMSkipTypes; virtual; abstract;
  public
    property ErrCode: Integer read GetErrCode;
    property SkipCode: TZMSkipTypes read GetSkipCode;
  end;

type
  TZMZipDiskStatusEnum = (ZdsEmpty, ZdsHasFiles, ZdsPreviousDisk,
    ZdsSameFileName, ZdsNotEnoughSpace);
  TZMZipDiskStatus = set of TZMZipDiskStatusEnum;
  TZMDiskAction = (ZdaYesToAll, ZdaOk, ZdaErase, ZdaReject, ZdaCancel);

  TZMDeflates = (ZmStore, ZmDeflate);
  PZMDeflates = ^TZMDeflates;

  TZMZHeader = (ZzNormal, ZzCompat, ZzAuto);

type
  TZMSFXOpt = (SoAskCmdLine,
    // allow user to prevent execution of the command line
    SoAskFiles, // allow user to prevent certain files from extraction
    SoHideOverWriteBox, // do not allow user to choose the overwrite mode
    SoAutoRun, // start extraction + evtl. command line automatically
    // only if sfx filename starts with "!" or is "setup.exe"
    SoNoSuccessMsg, // don't show success message after extraction
    SoExpandVariables, // expand environment variables in path/cmd line...
    SoInitiallyHideFiles, // dont show file listview on startup
    SoForceHideFiles, // do not allow user to show files list
    // (no effect if shfInitiallyShowFiles is set)
    SoCheckAutoRunFileName, // can only autorun if !... or setup.exe
    SoCanBeCancelled, // extraction can be cancelled
    SoCreateEmptyDirs, // recreate empty directories
    SoSuccessAlways
    // always give success message even if soAutoRun or soNoSuccessMsg
    );

  // set of TSFXOption
  TZMSFXOpts = set of TZMSFXOpt;

type
  TZMProgressType = (NewFile, ProgressUpdate, EndOfItem, EndOfBatch,
    TotalFiles2Process, TotalSize2Process, NewExtra, ExtraUpdate);

type
  TZMProgressDetails = class(TObject)
  protected
    function GetBytesWritten: Int64; virtual; abstract;
    function GetDelta: Int64; virtual; abstract;
    function GetItemName: string; virtual; abstract;
    function GetItemNumber: Integer; virtual; abstract;
    function GetItemPerCent: Integer;
    function GetItemPosition: Int64; virtual; abstract;
    function GetItemSize: Int64; virtual; abstract;
    function GetOrder: TZMProgressType; virtual; abstract;
    function GetStop: Boolean; virtual; abstract;
    function GetTotalCount: Int64; virtual; abstract;
    function GetTotalPerCent: Integer;
    function GetTotalPosition: Int64; virtual; abstract;
    function GetTotalSize: Int64; virtual; abstract;
    procedure SetStop(const Value: Boolean); virtual; abstract;
  public
    property BytesWritten: Int64 read GetBytesWritten;
    property Delta: Int64 read GetDelta;
    property ItemName: string read GetItemName;
    property ItemNumber: Integer read GetItemNumber;
    property ItemPerCent: Integer read GetItemPerCent;
    property ItemPosition: Int64 read GetItemPosition;
    property ItemSize: Int64 read GetItemSize;
    property Order: TZMProgressType read GetOrder;
    property Stop: Boolean read GetStop write SetStop;
    property TotalCount: Int64 read GetTotalCount;
    property TotalPerCent: Integer read GetTotalPerCent;
    property TotalPosition: Int64 read GetTotalPosition;
    property TotalSize: Int64 read GetTotalSize;
  end;

  // ZipDirEntry status bit constants
const
  ZsbDirty = $1;
  ZsbSelected = $2;
  ZsbSkipped = $4;
  ZsbHail = $8;
  // zsbIgnore = $8;
  ZsbDirOnly = $10;
  ZsbInvalid = $20;
  ZsbError = $40; // processing error
  ZsbDiscard = $80;

const
  DefNoSkips = [StDupName, StReadError];

type
  // abstract class representing a zip central record
  TZMDirEntry = class
  private
    function GetExtraData(Tag: Word): TZMRawBytes;
    function GetIsDirOnly: Boolean;
  protected
    function GetCompressedSize: Int64; virtual; abstract;
    function GetCompressionMethod: Word; virtual; abstract;
    function GetCRC32: Cardinal; virtual; abstract;
    function GetDateStamp: TDateTime;
    function GetDateTime: Cardinal; virtual; abstract;
    function GetEncoded: TZMEncodingOpts; virtual; abstract;
    function GetEncrypted: Boolean; virtual; abstract;
    function GetExtFileAttrib: Longword; virtual; abstract;
    function GetExtraField: TZMRawBytes; virtual; abstract;
    function GetExtraFieldLength: Word; virtual; abstract;
    function GetFileComment: string; virtual; abstract;
    function GetFileCommentLen: Word; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetFileNameLength: Word; virtual; abstract;
    function GetFlag: Word; virtual; abstract;
    function GetHeaderName: TZMRawBytes; virtual; abstract;
    function GetIntFileAttrib: Word; virtual; abstract;
    function GetMaster: TComponent; virtual; abstract;
    function GetRelOffLocalHdr: Int64; virtual; abstract;
    function GetStartOnDisk: Word; virtual; abstract;
    function GetStatusBits: Cardinal; virtual; abstract;
    function GetUncompressedSize: Int64; virtual; abstract;
    function GetVersionMadeBy: Word; virtual; abstract;
    function GetVersionNeeded: Word; virtual; abstract;
    function XData(const X: TZMRawBytes; Tag: Word;
      var Idx, Size: Integer): Boolean;
    property Master: TComponent read GetMaster;
  public
    function UnzipToFile(const DestName: string): Integer;
    function UnzipToStream(DestStream: TStream): Integer;
    property CompressedSize: Int64 read GetCompressedSize;
    property CompressionMethod: Word read GetCompressionMethod;
    property CRC32: Cardinal read GetCRC32;
    property DateStamp: TDateTime read GetDateStamp;
    property DateTime: Cardinal read GetDateTime;
    property Encoded: TZMEncodingOpts read GetEncoded;
    property Encrypted: Boolean read GetEncrypted;
    property ExtFileAttrib: Longword read GetExtFileAttrib;
    property ExtraData[Tag: Word]: TZMRawBytes read GetExtraData;
    property ExtraField: TZMRawBytes read GetExtraField;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComment: string read GetFileComment;
    property FileCommentLen: Word read GetFileCommentLen;
    property FileName: string read GetFileName;
    property FileNameLength: Word read GetFileNameLength;
    property Flag: Word read GetFlag;
    property HeaderName: TZMRawBytes read GetHeaderName;
    property IntFileAttrib: Word read GetIntFileAttrib;
    property IsDirOnly: Boolean read GetIsDirOnly;
    property RelOffLocalHdr: Int64 read GetRelOffLocalHdr;
    property StartOnDisk: Word read GetStartOnDisk;
    property StatusBits: Cardinal read GetStatusBits;
    property UncompressedSize: Int64 read GetUncompressedSize;
    property VersionMadeBy: Word read GetVersionMadeBy;
    property VersionNeeded: Word read GetVersionNeeded;
  end;

  TZMDirRec = class(TZMDirEntry)
  public
    function ChangeAttrs(NAttr: Cardinal): Integer; virtual; abstract;
    function ChangeComment(const Ncomment: string): Integer; virtual; abstract;
    function ChangeData(Ndata: TZMRawBytes): Integer; virtual; abstract;
    function ChangeDate(Ndosdate: Cardinal): Integer; virtual; abstract;
    function ChangeEncoding: Integer; virtual; abstract;
    function ChangeName(const Nname: string; NoCheck: Boolean = False): Integer;
      virtual; abstract;
    function ChangeStamp(Ndate: TDateTime): Integer;
  end;

type
  TZMForEachFunction = function(Rec: TZMDirEntry; var Data): Integer;
  TZMChangeFunction = function(Rec: TZMDirRec; var Data): Integer;

type
  TZMRenameRec = record
    Source: string;
    Dest: string;
    Comment: string;
    DateTime: Integer;
  end;
  PZMRenameRec = ^TZMRenameRec;

type
  TZMConflictEntry = class(TZMDirEntry)
  protected
    function GetOriginalName: string; virtual; abstract;
    function GetZipName: string; virtual; abstract;
  public
    property OriginalName: string read GetOriginalName;
    property ZipName: string read GetZipName;
  end;

type
  TZMResolutions = (ZmrRename, ZmrExisting, ZmrConflicting);
  TZMDupResolutions = (ZdrAuto, ZdrSkip, ZdrRename, ZdrBoth, ZdrAbort,
    ZdrDefer);

type
  TZMCheckTerminateEvent = procedure(Sender: TObject; var Abort: Boolean)
    of object;
  TZMCRC32ErrorEvent = procedure(Sender: TObject; const ForFile: string;
    FoundCRC, ExpectedCRC: Longword; var DoExtract: Boolean) of object;
  TZMExtractOverwriteEvent = procedure(Sender: TObject; const ForFile: string;
    IsOlder: Boolean; var DoOverwrite: Boolean; DirIndex: Integer) of object;
  TZMMergeZippedConflictEvent = procedure(Sender: TObject;
    Existing, Conflicting: TZMConflictEntry; var Resolve: TZMResolutions)
    of object;
  TZMSkippedEvent = procedure(Sender: TObject; const ForFile: string;
    SkipType: TZMSkipTypes; var ExtError: Integer) of object;
  TZMFileCommentEvent = procedure(Sender: TObject; const ForFile: string;
    var FileComment: string; var IsChanged: Boolean) of object;
  TZMFileExtraEvent = procedure(Sender: TObject; const ForFile: string;
    var Data: TZMRawBytes; var IsChanged: Boolean) of object;
  TZMGetNextDiskEvent = procedure(Sender: TObject;
    DiskSeqNo, DiskTotal: Integer; Drive: string; var AbortAction: Boolean)
    of object;
  TZMLoadStrEvent = procedure(Ident: Integer; var DefStr: string) of object;
  TZMMessageEvent = procedure(Sender: TObject; ErrCode: Integer;
    const ErrMsg: string) of object;
  TZMNewNameEvent = procedure(Sender: TObject; SeqNo: Integer) of object;
  TZMPasswordErrorEvent = procedure(Sender: TObject; IsZipAction: Boolean;
    var NewPassword: string; const ForFile: string; var RepeatCount: Longword;
    var Action: TMsgDlgBtn) of object;
  TZMProgressEvent = procedure(Sender: TObject; Details: TZMProgressDetails)
    of object;
  TZMSetAddNameEvent = procedure(Sender: TObject; var FileName: string;
    const ExtName: string; var IsChanged: Boolean) of object;
  TZMSetExtNameEvent = procedure(Sender: TObject; var FileName: string;
    const BaseDir: string; var IsChanged: Boolean) of object;
  TZMStatusDiskEvent = procedure(Sender: TObject; PreviousDisk: Integer;
    PreviousFile: string; Status: TZMZipDiskStatus; var Action: TZMDiskAction)
    of object;
  TZMTickEvent = procedure(Sender: TObject) of object;
  TZMDialogEvent = procedure(Sender: TObject; const Title: string;
    var Msg: string; var Result: Integer; Btns: TMsgDlgButtons) of object;
  TZMSetCompLevel = procedure(Sender: TObject; const ForFile: string;
    var Level: Integer; var IsChanged: Boolean) of object;
  TZMStateChange = procedure(Sender: TObject; State: TZMStates;
    var NoCursor: Boolean) of object;
  TZMWriteDupNameEvent = procedure(Sender: TObject;
    const Existing, Conflicting: TZMConflictEntry; var NewName: string;
    var Resolve: TZMDupResolutions) of object;

  // default file extensions that are best 'stored'
const
  ZMDefAddStoreSuffixes = [AssGIF .. AssJAR, AssJPG .. Ass7Zp,
    AssMP3 .. AssAVI];

type
{$IFDEF VERD2005up}
  TCustomZipMaster = class;

  TZipMasterEnumerator = class
  private
    FIndex: Integer;
    FOwner: TCustomZipMaster;
  public
    constructor Create(AMaster: TCustomZipMaster);
    function GetCurrent: TZMDirEntry;
    function MoveNext: Boolean;
    property Current: TZMDirEntry read GetCurrent;
  end;
{$ENDIF}

  // the main component
  TCustomZipMaster = class(TComponent)
  private
    { Private versions of property variables }
    FDLLDirectory: string;
    FFSpecArgs: TStrings;
    FFSpecArgsExcl: TStrings;
    FHandle: HWND;
    FOnCheckTerminate: TZMCheckTerminateEvent;
    FOnCRC32Error: TZMCRC32ErrorEvent;
    FOnDirUpdate: TNotifyEvent;
    FOnExtractOverwrite: TZMExtractOverwriteEvent;
    FOnFileComment: TZMFileCommentEvent;
    FOnFileExtra: TZMFileExtraEvent;
    FOnGetNextDisk: TZMGetNextDiskEvent;
    FOnLoadStr: TZMLoadStrEvent;
    FOnMergeZippedConflict: TZMMergeZippedConflictEvent;
    FOnMessage: TZMMessageEvent;
    FOnNewName: TZMNewNameEvent;
    FOnPasswordError: TZMPasswordErrorEvent;
    FOnProgress: TZMProgressEvent;
    FOnSetAddName: TZMSetAddNameEvent;
    FOnSetCompLevel: TZMSetCompLevel;
    FOnSetExtName: TZMSetExtNameEvent;
    FOnSkipped: TZMSkippedEvent;
    FOnStateChange: TZMStateChange;
    FOnStatusDisk: TZMStatusDiskEvent;
    FOnTick: TZMTickEvent;
    FOnWriteDupName: TZMWriteDupNameEvent;
    FOnZipDialog: TZMDialogEvent;
    FTrace: Boolean;
    FVerbose: Boolean;
    procedure AuxWasChanged;
    function CopyBuffer(InFile, OutFile: Integer): Integer;
    function GetActive: Boolean;
    function GetAddCompLevel: Integer;
    function GetAddFrom: TDateTime;
    function GetAddOptions: TZMAddOpts;
    function GetAddStoreSuffixes: TZMAddStoreExts;
    function GetBlockedCnt: Integer;
    function GetBuild: Integer;
    // { Property get/set functions }
    function GetCancel: Boolean;
    function GetConfirmErase: Boolean;
    function GetCount: Integer;
    function GetDirEntry(Idx: Integer): TZMDirEntry;
    function GetDirOnlyCnt: Integer;
    function GetDLL_Build: Integer;
    function GetDLL_Load: Boolean;
    function GetDLL_Path: string;
    function GetDLL_Version: string;
    function GetDLL_Version1(ForceLoad: Boolean): string;
    function GetEncodeAs: TZMEncodingOpts;
    function GetEncoding: TZMEncodingOpts;
    function GetEncoding_CP: Cardinal;
    function GetErrCode: Integer;
    function GetErrMessage: string;
    function GetExtAddStoreSuffixes: string;
    function GetExtErrCode: Cardinal;
    function GetExtrBaseDir: string;
    function GetExtrOptions: TZMExtrOpts;
    function GetExtStream: TStream;
    function GetHowToDelete: TZMDeleteOpts;
    function GetIsSpanned: Boolean;
    function GetKeepFreeOnAllDisks: Cardinal;
    function GetKeepFreeOnDisk1: Cardinal;
    function GetLanguage: string;
    function GetMaxVolumeSize: Int64;
    function GetMaxVolumeSizeKb: Integer;
    function GetMinFreeVolumeSize: Cardinal;
    function GetNoReadAux: Boolean;
    function GetNoSkipping: TZMSkipAborts;
    function GetNotMainThread: Boolean;
    function Get_Password: string;
    function GetPasswordReqCount: Longword;
    function GetProgressDetail: TZMProgressDetails;
    function GetRootDir: string;
    function GetSFXCaption: string;
    function GetSFXCommandLine: string;
    function GetSFXDefaultDir: string;
    function GetSFXIcon: TIcon;
    function GetSFXMessage: string;
    function GetSFXOffset: Integer;
    function GetSFXOptions: TZMSFXOpts;
    function GetSFXOverwriteMode: TZMOvrOpts;
    function GetSFXPath: string;
    function GetSFXRegFailPath: string;
    function GetSpanOptions: TZMSpanOpts;
    function Getstate: TZMStates;
    function GetSuccessCnt: Integer;
    function GetTempDir: string;
    function GetTotalSizeToProcess: Int64;
    function GetUnattended: Boolean;
    function GetUseDirOnlyEntries: Boolean;
{$IFNDEF UNICODE}
    function GetUseUTF8: Boolean;
{$ENDIF}
    function GetVersion: string;
    function GetWriteOptions: TZMWriteOpts;
    function GetZipComment: AnsiString;
    function GetZipEOC: Int64;
    function GetZipFileName: string;
    function GetZipFileSize: Int64;
    function GetZipSOC: Int64;
    function GetZipStream: TMemoryStream;
    function IsActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetAddCompLevel(const Value: Integer);
    procedure SetAddFrom(const Value: TDateTime);
    procedure SetAddOptions(const Value: TZMAddOpts);
    procedure SetAddStoreSuffixes(const Value: TZMAddStoreExts);
    procedure SetCancel(Value: Boolean);
    procedure SetConfirmErase(const Value: Boolean);
    procedure SetDLLDirectory(const Value: string);
    procedure SetDLL_Load(const Value: Boolean);
    procedure SetEncodeAs(const Value: TZMEncodingOpts);
    procedure SetEncoding(const Value: TZMEncodingOpts);
    procedure SetEncoding_CP(Value: Cardinal);
    procedure SetErrCode(Value: Integer);
    procedure SetExtAddStoreSuffixes(const Value: string);
    procedure SetExtErrCode(const Value: Cardinal);
    procedure SetExtrBaseDir(const Value: string);
    procedure SetExtrOptions(const Value: TZMExtrOpts);
    procedure SetExtStream(const Value: TStream);
    procedure SetFSpecArgs(const Value: TStrings);
    procedure SetFSpecArgsExcl(const Value: TStrings);
    procedure SetHowToDelete(const Value: TZMDeleteOpts);
    procedure SetKeepFreeOnAllDisks(const Value: Cardinal);
    procedure SetKeepFreeOnDisk1(const Value: Cardinal);
    procedure SetLanguage(const Value: string);
    procedure SetMaxVolumeSize(const Value: Int64);
    procedure SetMaxVolumeSizeKb(const Value: Integer);
    procedure SetMinFreeVolumeSize(const Value: Cardinal);
    procedure SetNoReadAux(const Value: Boolean);
    procedure SetNoSkipping(const Value: TZMSkipAborts);
    procedure SetNotMainThread(const Value: Boolean);
    procedure Set_Password(const Value: string);
    procedure SetPasswordReqCount(Value: Longword);
    procedure SetRootDir(const Value: string);
    procedure SetSFXCaption(const Value: string);
    procedure SetSFXCommandLine(const Value: string);
    procedure SetSFXDefaultDir(const Value: string);
    procedure SetSFXIcon(Value: TIcon);
    procedure SetSFXMessage(const Value: string);
    procedure SetSFXOptions(const Value: TZMSFXOpts);
    procedure SetSFXOverwriteMode(const Value: TZMOvrOpts);
    procedure SetSFXPath(const Value: string);
    procedure SetSFXRegFailPath(const Value: string);
    procedure SetSpanOptions(const Value: TZMSpanOpts);
    procedure SetTempDir(const Value: string);
    procedure SetTrace(const Value: Boolean);
    procedure SetUnattended(const Value: Boolean);
    procedure SetUseDirOnlyEntries(const Value: Boolean);
{$IFNDEF UNICODE}
    procedure SetUseUTF8(const Value: Boolean);
{$ENDIF}
    procedure SetVerbose(const Value: Boolean);
    procedure SetVersion(const Value: string);
    procedure SetWriteOptions(const Value: TZMWriteOpts);
    procedure SetZipComment(const Value: AnsiString);
    procedure SetZipFileName(const Value: string);
  protected
    FBody: TZMRoot;
    procedure Loaded; override;
    function ReEntry: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AbortDLL;
    function Add: Integer;
    function AddStreamToFile(const FileName: string;
      FileDate, FileAttr: Dword): Integer;
    function AddStreamToStream(InStream: TMemoryStream): TMemoryStream;
//    procedure AfterConstruction; override;
    function AppendSlash(const SDir: string): string;
    procedure BeforeDestruction; override;
    function ChangeFileDetails(Func: TZMChangeFunction; var Data): Integer;
    procedure Clear;
    function ConvertToSFX: Integer;
    function ConvertToSpanSFX(const OutFile: string): Integer;
    function ConvertToZIP: Integer;
    function Copy_File(const InFileName, OutFileName: string): Integer;
    function Deflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function Delete: Integer;
    function EraseFile(const FName: string; How: TZMDeleteOpts): Integer;
    function Extract: Integer;
    function ExtractFileToStream(const FileName: string): TMemoryStream;
    function ExtractStreamToStream(InStream: TMemoryStream; OutSize: Longword;
      HeaderType: TZMZHeader = ZzNormal): TMemoryStream;
    function Find(const Fspec: string; var Idx: Integer): TZMDirEntry;
    function ForEach(Func: TZMForEachFunction; var Data): Integer;
    function FullVersionString: string;
    function GetAddPassword: string; overload;
    function GetAddPassword(var Response: TMsgDlgBtn): string; overload;
{$IFDEF VERD2005up}
    function GetEnumerator: TZipMasterEnumerator;
{$ENDIF}
    function GetExtrPassword: string; overload;
    function GetExtrPassword(var Response: TMsgDlgBtn): string; overload;
    function GetPassword(const DialogCaption, MsgTxt: string;
      Pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
    function IndexOf(const FName: string): Integer;
    function IsZipSFX(const SFXExeName: string): Integer;
    function List: Integer;
    function MakeTempFileName(const Prefix, Extension: string): string;
    function MergeZippedFiles(Opts: TZMMergeOpts): Integer;
    function QueryZip(const FName: TFileName): Integer;
    function ReadSpan(const InFileName: string;
      var OutFilePath: string): Integer;
    function Rename(RenameList: TList; DateTime: Integer;
      How: TZMRenameOpts = HtrDefault): Integer;
    procedure ShowExceptionError(const ZMExcept: Exception);
    procedure ShowZipFmtMessage(Id: Integer; const Args: array of const;
      Display: Boolean);
    procedure ShowZipMessage(Ident: Integer; const UserStr: string);
    function Undeflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function UnzipToFile(const Entry: TZMDirEntry; const DestName: string): Integer;
    function UnzipToStream(const Entry: TZMDirEntry; DestStream: TStream): Integer;
    function WriteSpan(const InFileName, OutFileName: string): Integer;
    function ZipLoadStr(Id: Integer): string;

    property Active: Boolean read GetActive write SetActive;
    property AddCompLevel: Integer read GetAddCompLevel write SetAddCompLevel;
    property AddFrom: TDateTime read GetAddFrom write SetAddFrom;
    property AddOptions: TZMAddOpts read GetAddOptions write SetAddOptions;
    property AddStoreSuffixes: TZMAddStoreExts read GetAddStoreSuffixes
      write SetAddStoreSuffixes;
    property BlockedCnt: Integer read GetBlockedCnt;
    property Build: Integer read GetBuild;
    property Cancel: Boolean read GetCancel write SetCancel;
    property ConfirmErase: Boolean read GetConfirmErase write SetConfirmErase
      default True;
    property Count: Integer read GetCount;
    property DirEntry[Idx: Integer]: TZMDirEntry read GetDirEntry; default;
    property DirOnlyCnt: Integer read GetDirOnlyCnt;
    property DLLDirectory: string read FDLLDirectory write SetDLLDirectory;
    property DLL_Build: Integer read GetDLL_Build;
    property DLL_Load: Boolean read GetDLL_Load write SetDLL_Load;
    property DLL_Path: string read GetDLL_Path;
    property DLL_Version: string read GetDLL_Version;
    property EncodeAs: TZMEncodingOpts read GetEncodeAs write SetEncodeAs;
    // 1 Filename and comment character encoding
    property Encoding: TZMEncodingOpts read GetEncoding write SetEncoding;
    // 1 codepage to use to decode filename
    property Encoding_CP: Cardinal read GetEncoding_CP write SetEncoding_CP;
    property ErrCode: Integer read GetErrCode write SetErrCode;
    property ErrMessage: string read GetErrMessage;
    property ExtAddStoreSuffixes: string read GetExtAddStoreSuffixes
      write SetExtAddStoreSuffixes;
    property ExtErrCode: Cardinal read GetExtErrCode write SetExtErrCode;
    property ExtrBaseDir: string read GetExtrBaseDir write SetExtrBaseDir;
    property ExtrOptions: TZMExtrOpts read GetExtrOptions write SetExtrOptions;
    property ExtStream: TStream read GetExtStream write SetExtStream;
    property FSpecArgs: TStrings read FFSpecArgs write SetFSpecArgs;
    property FSpecArgsExcl: TStrings read FFSpecArgsExcl write SetFSpecArgsExcl;
    property Handle: HWND read FHandle;
    property HowToDelete: TZMDeleteOpts read GetHowToDelete write SetHowToDelete;
    property IsSpanned: Boolean read GetIsSpanned;
    property KeepFreeOnAllDisks: Cardinal read GetKeepFreeOnAllDisks
      write SetKeepFreeOnAllDisks;
    property KeepFreeOnDisk1: Cardinal read GetKeepFreeOnDisk1
      write SetKeepFreeOnDisk1;
    property Language: string read GetLanguage write SetLanguage;
    property MaxVolumeSize: Int64 read GetMaxVolumeSize write SetMaxVolumeSize;
    property MaxVolumeSizeKb: Integer read GetMaxVolumeSizeKb
      write SetMaxVolumeSizeKb;
    property MinFreeVolumeSize: Cardinal read GetMinFreeVolumeSize
      write SetMinFreeVolumeSize;
    property NoReadAux: Boolean read GetNoReadAux write SetNoReadAux;
    property NoSkipping: TZMSkipAborts read GetNoSkipping write SetNoSkipping
      default DefNoSkips;
    property NotMainThread: Boolean read GetNotMainThread
      write SetNotMainThread;
    property Password: string read Get_Password write Set_Password;
    property PasswordReqCount: Longword read GetPasswordReqCount
      write SetPasswordReqCount;
    property ProgressDetail: TZMProgressDetails read GetProgressDetail;
    property RootDir: string read GetRootDir write SetRootDir;
    property SFXCaption: string read GetSFXCaption write SetSFXCaption;
    property SFXCommandLine: string read GetSFXCommandLine
      write SetSFXCommandLine;
    property SFXDefaultDir: string read GetSFXDefaultDir write SetSFXDefaultDir;
    property SFXIcon: TIcon read GetSFXIcon write SetSFXIcon;
    property SFXMessage: string read GetSFXMessage write SetSFXMessage;
    property SFXOffset: Integer read GetSFXOffset;
    property SFXOptions: TZMSFXOpts read GetSFXOptions write SetSFXOptions;
    property SFXOverwriteMode: TZMOvrOpts read GetSFXOverwriteMode
      write SetSFXOverwriteMode default OvrConfirm;
    property SFXPath: string read GetSFXPath write SetSFXPath;
    property SFXRegFailPath: string read GetSFXRegFailPath
      write SetSFXRegFailPath;
    property SpanOptions: TZMSpanOpts read GetSpanOptions write SetSpanOptions;
    property State: TZMStates read Getstate;
    property SuccessCnt: Integer read GetSuccessCnt;
    property TempDir: string read GetTempDir write SetTempDir;
    property TotalSizeToProcess: Int64 read GetTotalSizeToProcess;
    property Trace: Boolean read FTrace write SetTrace;
    property Unattended: Boolean read GetUnattended write SetUnattended;
    property UseDirOnlyEntries: Boolean read GetUseDirOnlyEntries
      write SetUseDirOnlyEntries;// default False;
{$IFNDEF UNICODE}
    property UseUTF8: Boolean read GetUseUTF8 write SetUseUTF8;
{$ENDIF}
    property Verbose: Boolean read FVerbose write SetVerbose;
    property Version: string read GetVersion write SetVersion;
    property WriteOptions: TZMWriteOpts read GetWriteOptions
      write SetWriteOptions;
    property ZipComment: AnsiString read GetZipComment write SetZipComment;
    property ZipEOC: Int64 read GetZipEOC;
    property ZipFileName: string read GetZipFileName write SetZipFileName;
    property ZipFileSize: Int64 read GetZipFileSize;
    property ZipSOC: Int64 read GetZipSOC;
    property ZipStream: TMemoryStream read GetZipStream;
    { Events }
    property OnCheckTerminate: TZMCheckTerminateEvent read FOnCheckTerminate
      write FOnCheckTerminate;
    property OnCRC32Error: TZMCRC32ErrorEvent read FOnCRC32Error
      write FOnCRC32Error;
    property OnDirUpdate: TNotifyEvent read FOnDirUpdate write FOnDirUpdate;
    property OnExtractOverwrite: TZMExtractOverwriteEvent
      read FOnExtractOverwrite write FOnExtractOverwrite;
    property OnFileComment: TZMFileCommentEvent read FOnFileComment
      write FOnFileComment;
    property OnFileExtra: TZMFileExtraEvent read FOnFileExtra
      write FOnFileExtra;
    property OnGetNextDisk: TZMGetNextDiskEvent read FOnGetNextDisk
      write FOnGetNextDisk;
    property OnLoadStr: TZMLoadStrEvent read FOnLoadStr write FOnLoadStr;
    property OnMergeZippedConflict: TZMMergeZippedConflictEvent
      read FOnMergeZippedConflict write FOnMergeZippedConflict;
    property OnMessage: TZMMessageEvent read FOnMessage write FOnMessage;
    property OnNewName: TZMNewNameEvent read FOnNewName write FOnNewName;
    property OnPasswordError: TZMPasswordErrorEvent read FOnPasswordError
      write FOnPasswordError;
    property OnProgress: TZMProgressEvent read FOnProgress write FOnProgress;
    property OnSetAddName: TZMSetAddNameEvent read FOnSetAddName
      write FOnSetAddName;
    property OnSetCompLevel: TZMSetCompLevel read FOnSetCompLevel
      write FOnSetCompLevel;
    property OnSetExtName: TZMSetExtNameEvent read FOnSetExtName
      write FOnSetExtName;
    property OnSkipped: TZMSkippedEvent read FOnSkipped write FOnSkipped;
    property OnStateChange: TZMStateChange read FOnStateChange
      write FOnStateChange;
    property OnStatusDisk: TZMStatusDiskEvent read FOnStatusDisk
      write FOnStatusDisk;
    property OnTick: TZMTickEvent read FOnTick write FOnTick;
    property OnWriteDupName: TZMWriteDupNameEvent read FOnWriteDupName
      write FOnWriteDupName;
    property OnZipDialog: TZMDialogEvent read FOnZipDialog write FOnZipDialog;
  end;

type
  TZipMaster = class(TCustomZipMaster)
  published
    property Active default True;
    property AddCompLevel default 9;
    property AddFrom;
    property AddOptions default [];
    property AddStoreSuffixes default ZMDefAddStoreSuffixes;
    property ConfirmErase default True;
    property DLLDirectory;
    property DLL_Load default False;
    property EncodeAs default ZeoAuto;
    // 1 Filename and comment character encoding
    property Encoding default ZeoAuto;
    property Encoding_CP default 0;
    property ExtAddStoreSuffixes;
    property ExtrBaseDir;
    property ExtrOptions default [];
    property FSpecArgs;
    property FSpecArgsExcl;
    property HowToDelete default HtdAllowUndo;
    property KeepFreeOnAllDisks default 0;
    property KeepFreeOnDisk1 default 0;
    property Language;
    property MaxVolumeSize default 0;
    property MaxVolumeSizeKb default 0;
    property MinFreeVolumeSize default 65536;
    property NoReadAux default False;
    property NoSkipping default DefNoSkips;
    { Events }
    property OnCheckTerminate;
    property OnCRC32Error;
    property OnDirUpdate;
    property OnExtractOverwrite;
    property OnFileComment;
    property OnFileExtra;
    property OnGetNextDisk;
    property OnLoadStr;
    property OnMergeZippedConflict;
    property OnMessage;
    property OnNewName;
    property OnPasswordError;
    property OnProgress;
    property OnSetAddName;
    property OnSetCompLevel;
    property OnSetExtName;
    property OnSkipped;
    property OnStatusDisk;
    property OnTick;
    property OnWriteDupName;
    property OnZipDialog;
    property Password;
    property PasswordReqCount default 1;
    // SFX
    property RootDir;
    property SFXCaption;
    property SFXCommandLine;
    property SFXDefaultDir;
    property SFXIcon;
    property SFXMessage;
    property SFXOptions default [];
    property SFXOverwriteMode default OvrAlways;
    property SFXPath;
    property SFXRegFailPath;
    property SpanOptions default [];
    property TempDir;
    property Trace default False;
    property Unattended default False;
    property UseDirOnlyEntries default False;
{$IFNDEF UNICODE}
    property UseUTF8 default False;
{$ENDIF}
    property Verbose default False;
    property Version;
    property WriteOptions default [];
    property ZipComment;
    property ZipFileName;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  WinApi.Messages,
{$ELSE}
  Messages,
{$ENDIF}
  ZMUtils, ZMBody, ZMMsg, ZMCtx, ZMOprMsgStr, ZMWinFuncs, ZMMatch,
  ZMDllLoad, ZMLister, ZMOprDll, ZMCommand, ZMCore, ZMOprUnzip,
  ZMOprDeflate, ZMOprDel, ZMOprCore, ZMOprMod, ZMOprMerge, ZMOprFile;

const
  __UNIT__ = 1;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

{ TZMProgressDetails }
function TZMProgressDetails.GetItemPerCent: Integer;
begin
  if (ItemSize > 0) and (ItemPosition > 0) then
    Result := (100 * ItemPosition) div ItemSize
  else
    Result := 0;
end;

function TZMProgressDetails.GetTotalPerCent: Integer;
begin
  if (TotalSize > 0) and (TotalPosition > 0) then
    Result := (100 * TotalPosition) div TotalSize
  else
    Result := 0;
end;

{ TZMDirEntry }
function TZMDirEntry.GetDateStamp: TDateTime;
begin
  Result := FileDateToLocalDateTime(GetDateTime);
end;

// return first data for Tag
function TZMDirEntry.GetExtraData(Tag: Word): TZMRawBytes;
var
  I: Integer;
  Sz: Integer;
begin
  Result := ExtraField;
  if (ExtraFieldLength >= 4) and XData(Result, Word(Tag), I, Sz) then
    Result := Copy(Result, 5, Sz - 4)
  else
    Result := '';
end;

function TZMDirEntry.GetIsDirOnly: Boolean;
begin
  Result := (StatusBits and ZsbDirOnly) <> 0;
end;

function TZMDirEntry.UnzipToFile(const DestName: string): Integer;
begin
  Result := (Master as TCustomZipMaster).UnzipToFile(Self, DestName);
end;

function TZMDirEntry.UnzipToStream(DestStream: TStream): Integer;
begin
  Result := (Master as TCustomZipMaster).UnzipToStream(Self, DestStream);
end;

function TZMDirEntry.XData(const X: TZMRawBytes; Tag: Word;
  var Idx, Size: Integer): Boolean;
var
  I: Integer;
  L: Integer;
  Wsz: Word;
  Wtg: Word;
begin
  Result := False;
  Idx := 0;
  Size := 0;
  I := 1;
  L := Length(X);
  while I <= L - 4 do
  begin
    Wtg := PWord(@X[I])^;
    Wsz := PWord(@X[I + 2])^;
    if Wtg = Tag then
    begin
      Result := (I + Wsz + 4) <= L + 1;
      if Result then
      begin
        Idx := I;
        Size := Wsz + 4;
      end;
      Break;
    end;
    I := I + Wsz + 4;
  end;
end;

{ TZMDirRec }
function TZMDirRec.ChangeStamp(Ndate: TDateTime): Integer;
begin
  Result := ChangeDate(DateTimeToFileDate(Ndate));
end;

{$IFDEF VERD2005up}
{ TZipMasterEnumerator }
constructor TZipMasterEnumerator.Create(AMaster: TCustomZipMaster);
begin
  inherited Create;
  FIndex := -1;
  FOwner := AMaster;
end;

function TZipMasterEnumerator.GetCurrent: TZMDirEntry;
begin
  Result := FOwner[FIndex];
end;

function TCustomZipMaster.GetEnumerator: TZipMasterEnumerator;
begin
  Result := TZipMasterEnumerator.Create(Self);
end;

function TZipMasterEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < (FOwner.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$ENDIF}

constructor TCustomZipMaster.Create(AOwner: TComponent);
begin
  inherited;
  FBody := TZMCommand.Create(Self);
  FFSpecArgs := TZMStringList.Create;
  FFSpecArgsExcl := TZMStringList.Create;
  FHandle := Application.Handle;
end;

procedure TCustomZipMaster.AbortDLL;
begin
  if TZMCommand(FBody).DLLWorking is TZMDLL then
    TZMDLL(TZMCommand(FBody).DLLWorking).AbortDLL;
end;

function TCustomZipMaster.Add: Integer;
begin
  Result := FBody.Run(TZMOpAdd.Create);
end;

function TCustomZipMaster.AddStreamToFile(const FileName: string;
  FileDate, FileAttr: Dword): Integer;
begin
  Result := FBody.Run(TZMOpAddStreamToFile.Create(FileName, FileDate,
    FileAttr));
end;

function TCustomZipMaster.AddStreamToStream(InStream: TMemoryStream)
  : TMemoryStream;
var
  Res: Integer;
begin
  Result := nil;
  Res := FBody.Run(TZMOpAddStreamToStream.Create(InStream));
  if (Res = 0) and (SuccessCnt = 1) then
    Result := ZipStream;
end;

//procedure TCustomZipMaster.AfterConstruction;
//begin
//  inherited;
//  FBody := TZMCommand.Create(Self);
//  FFSpecArgs := TZMStringList.Create;
//  FFSpecArgsExcl := TZMStringList.Create;
//  FHandle := Application.Handle;
//end;

function TCustomZipMaster.AppendSlash(const SDir: string): string;
begin
  Result := DelimitPath(SDir, True);
end;

procedure TCustomZipMaster.BeforeDestruction;
begin
  FOnMessage := nil; // stop any messages being sent
  FOnStateChange := nil;
  FOnTick := nil;
  FOnZipDialog := nil;
  TZMBody(FBody).IsDestructing := True;
  Cancel := True; // stop any activity
  TZMCommand(FBody).Active := False;
  FFSpecArgs.Free;
  FFSpecArgsExcl.Free;
  _DLL_Remove(Self); // remove from list
  FBody.Free;
  inherited;
end;

function TCustomZipMaster.ChangeFileDetails(Func: TZMChangeFunction;
  var Data): Integer;
begin
  Result := FBody.Run(TZMOpChangeFileDetails.Create({@}Func, Data));
end;

procedure TCustomZipMaster.Clear;
begin
  FBody.Run(TZMOpClear.Create);
end;

function TCustomZipMaster.ConvertToSFX: Integer;
var
  S: string;
begin
  S := '';
  Result := FBody.Run(TZMOpConvertToSFX.Create(S, False));
end;

function TCustomZipMaster.ConvertToSpanSFX(const OutFile: string): Integer;
begin
  Result := FBody.Run(TZMOpConvertToSFX.Create(OutFile, True));
end;

function TCustomZipMaster.ConvertToZIP: Integer;
begin
  Result := FBody.Run(TZMOpConvertToZip.Create);
end;

function TCustomZipMaster.CopyBuffer(InFile, OutFile: Integer): Integer;
const
  BufSize = 10 * 1024;
var
  Buffer: array of Byte;
  SizeR: Integer;
  ToRead: Cardinal;
begin
  // both files are already open
  Result := 0;
  ToRead := BufSize;
  try
    SetLength(Buffer, BufSize);
    repeat
      SizeR := FileRead(InFile, Buffer[0], ToRead);
      if (SizeR < 0) or (FileWrite(OutFile, Buffer[0], SizeR) <> SizeR) then
      begin
        Result := ZM_Error({_LINE_}1137, ZE_CopyError);
        Break;
      end;
    until (SizeR <> Integer(ToRead));
  except
    Result := ZM_Error({_LINE_}1142, ZE_CopyError);
  end;
  // leave both files open
end;

// maybe move to utils
function TCustomZipMaster.Copy_File(const InFileName,
  OutFileName: string): Integer;
var
  InFile: Integer;
  In_Size: Int64;
  OutFile: Integer;
  Out_Size: Int64;
begin
  In_Size := -1;
  Out_Size := -1;
  Result := ZM_Error({_LINE_}1158, ZE_FileOpen);

  if not _Z_FileExists(InFileName) then
    Exit;
  InFile := _Z_FileOpen(InFileName, FmOpenRead or FmShareDenyWrite);
  if InFile <> -1 then
  begin
    if _Z_FileExists(OutFileName) then
    begin
      OutFile := _Z_FileOpen(OutFileName, FmOpenWrite or FmShareExclusive);
      if OutFile = -1 then
      begin
        Result := ZM_Error({_LINE_}1170, ZE_NoOutFile);
        // might be read-only or source
        File_Close(InFile);
        Exit;
      end;
      File_Close(OutFile);
      _Z_EraseFile(OutFileName, HowToDelete = HtdFinal);
    end;
    OutFile := _Z_FileCreate(OutFileName);
    if OutFile <> -1 then
    begin
      Result := CopyBuffer(InFile, OutFile);
      if (Result = 0) and (FileSetDate(OutFile, FileGetDate(InFile)) <> 0) then
        Result := ZM_Error({_LINE_}1183, ZE_SetDateError);
      Out_Size := FileSeek(OutFile, Int64(0), SoFromEnd);
      File_Close(OutFile);
    end
    else
      Result := ZM_Error({_LINE_}1188, ZE_NoOutFile);
    In_Size := FileSeek(InFile, Int64(0), SoFromEnd);
    File_Close(InFile);
  end;
  // An extra check if the filesizes are the same.
  if (Result = 0) and ((In_Size = -1) or (Out_Size = -1) or
    (In_Size <> Out_Size)) then
    Result := ZM_Error({_LINE_}1195, ZE_UnknownError);
  // Don't leave a corrupted outfile lying around. (SetDateError is not fatal!)
  if (Result <> 0) and (AbsErr(Result) <> ZE_SetDateError) then
    File_Delete(OutFileName);
end;

function TCustomZipMaster.Deflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
begin
  Result := FBody.Run(TZMOpDeflate.Create(OutStream, InStream, Length,
    Method, CRC));
end;

function TCustomZipMaster.Delete: Integer;
begin
  Result := FBody.Run(TZMOpDelete.Create);
end;

function TCustomZipMaster.EraseFile(const FName: string;
  How: TZMDeleteOpts): Integer;
begin
  Result := _Z_EraseFile(FName, How = HtdFinal);
end;

function TCustomZipMaster.Extract: Integer;
begin
  Result := FBody.Run(TZMOpUnzipFiles.Create);
end;

function TCustomZipMaster.ExtractFileToStream(const FileName: string)
  : TMemoryStream;
begin
  Result := nil;
  if (FBody.Run(TZMOpExtractFileToStream.Create(FileName)) = 0) and
    (SuccessCnt = 1) then
    Result := ZipStream;
end;

function TCustomZipMaster.ExtractStreamToStream(InStream: TMemoryStream;
  OutSize: Longword; HeaderType: TZMZHeader = ZzNormal): TMemoryStream;
begin
  Result := nil;
  if (FBody.Run(TZMOpExtractStreamToStream.Create(InStream, OutSize, HeaderType)
    ) = 0) and (SuccessCnt = 1) then
    Result := ZipStream;
end;

function TCustomZipMaster.Find(const Fspec: string; var Idx: Integer)
  : TZMDirEntry;
begin
  Result := TZMLister(FBody).Find(Unquote(Fspec), Idx);
end;

function TCustomZipMaster.ForEach(Func: TZMForEachFunction; var Data): Integer;
begin
  Result := FBody.Run(TZMOpForEach.Create({@}Func, Data));
end;

function TCustomZipMaster.FullVersionString: string;
begin
  Result := 'ZipMaster ' + Version;
  Result := Result + ', DLL ' + GetDLL_Version1(True);
end;

function TCustomZipMaster.GetActive: Boolean;
begin
  Result := TZMCommand(FBody).Active;
end;

function TCustomZipMaster.GetAddPassword: string;
var
  Resp: TMsgDlgBtn;
begin
  Result := (FBody as TZMBody)._GetAddPassword(Resp);
  if State < ZsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetAddPassword(var Response: TMsgDlgBtn): string;
begin
  Result := (FBody as TZMBody)._GetAddPassword(Response);
  if State < ZsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetBuild: Integer;
begin
  Result := ZIPMASTERPRIV;
end;

function TCustomZipMaster.GetCancel: Boolean;
begin
  Result := TZMBody(FBody).Cancel <> 0;
end;

function TCustomZipMaster.GetCount: Integer;
begin
  Result := TZMLister(FBody).Count;
end;

function TCustomZipMaster.GetDirEntry(Idx: Integer): TZMDirEntry;
begin
  Result := nil;
  if IsActive then
    Result := TZMLister(FBody).DirEntry[Idx];
end;

function TCustomZipMaster.GetDirOnlyCnt: Integer;
begin
  Result := 0;
  if IsActive then
    Result := TZMLister(FBody).DirOnlyCnt;
end;

function TCustomZipMaster.GetDLL_Build: Integer;
begin
  if (CsDesigning in ComponentState) or (CsLoading in ComponentState) then
    Result := 0
  else
    Result := _DLL_Build;
end;

function TCustomZipMaster.GetDLL_Load: Boolean;
begin
  Result := TZMCommand(FBody).DLL_Load;
end;

function TCustomZipMaster.GetDLL_Path: string;
begin
  if (CsDesigning in ComponentState) or (CsLoading in ComponentState) then
    Result := ''
  else
    Result := _DLL_Path;
end;

function TCustomZipMaster.GetDLL_Version: string;
begin
  Result := GetDLL_Version1(False);
end;

function TCustomZipMaster.GetDLL_Version1(ForceLoad: Boolean): string;
begin
  Result := TZMCommand(FBody).GetDLL_Version1(ForceLoad);
end;

function TCustomZipMaster.GetErrCode: Integer;
begin
  if IsActive then
    Result := TZMBody(FBody).Errors.Code
  else
    Result := ZE_Inactive;
end;

function TCustomZipMaster.GetErrMessage: string;
begin
  if IsActive then
    Result := TZMBody(FBody).Errors.ErrMessage
  else
    Result := ZipLoadStr(ZE_Inactive);
end;

function TCustomZipMaster.GetExtrPassword: string;
var
  Resp: TMsgDlgBtn;
begin
  Result := TZMBody(FBody)._GetExtrPassword(Resp);
  if State < ZsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetExtrPassword(var Response: TMsgDlgBtn): string;
begin
  Result := TZMBody(FBody)._GetExtrPassword(Response);
  if State < ZsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetIsSpanned: Boolean;
begin
  if IsActive then
    Result := TZMLister(FBody).IsSpanned
  else
    Result := False; // unknown
end;

function TCustomZipMaster.GetLanguage: string;
begin
  Result := TZMCommand(FBody).Language;
end;

function TCustomZipMaster.GetNoReadAux: Boolean;
begin
  Result := TZMBody(FBody).NoReadAux;
  if not((CsDesigning in ComponentState) or (CsLoading in ComponentState)) then
    Result := Result or TZMBody(FBody).AuxChanged;
end;

function TCustomZipMaster.GetPassword(const DialogCaption, MsgTxt: string;
  Pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
begin
  Result := TZMBody(FBody)._GetPassword(DialogCaption, MsgTxt, DHC_Password,
    Pwb, ResultStr);
end;

function TCustomZipMaster.GetSFXOffset: Integer;
begin
  Result := TZMLister(FBody).SFXOffset;
end;

function TCustomZipMaster.GetSuccessCnt: Integer;
begin
  Result := TZMBody(FBody).SuccessCnt;
end;

function TCustomZipMaster.GetTotalSizeToProcess: Int64;
begin
  Result := TZMBody(FBody).TotalSizeToProcess;
end;

{$IFNDEF UNICODE}
function TCustomZipMaster.GetUseUTF8: Boolean;
begin
  Result := TZMBody(FBody).IsUtf8;
end;
{$ENDIF}

function TCustomZipMaster.GetVersion: string;
begin
  Result := ZIPMASTERBUILD;
end;

function TCustomZipMaster.GetZipEOC: Int64;
begin
  Result := TZMLister(FBody).ZipEOC;
end;

function TCustomZipMaster.GetZipFileSize: Int64;
begin
  Result := TZMLister(FBody).ZipFileSize;
end;

function TCustomZipMaster.GetZipSOC: Int64;
begin
  Result := TZMLister(FBody).ZipSOC;
end;

function TCustomZipMaster.GetZipStream: TMemoryStream;
begin
  Result := TZMLister(FBody).ZipStream;
end;

function TCustomZipMaster.IndexOf(const FName: string): Integer;
var
  Fn: string;
begin
  Fn := FName;
  for Result := 0 to Pred(Count) do
    if FileNameMatch(Fn, GetDirEntry(Result).FileName) then
      Exit;
  Result := -1;
end;

function TCustomZipMaster.IsZipSFX(const SFXExeName: string): Integer;
begin
  Result := ZMUtils.IsZipSFX(SFXExeName);
end;

function TCustomZipMaster.List: Integer;
begin
  Result := FBody.Run(TZMOpList.Create);
end;

procedure TCustomZipMaster.Loaded;
begin
  inherited;
  if IsActive then
    TZMCommand(FBody).DoDelays;
end;

(* ? MakeTempFileName
  Make a temporary filename like: C:\...\zipxxxx.zip
 Prefix and extension are default: 'zip' and '.zip'
*)
function TCustomZipMaster.MakeTempFileName(const Prefix,
  Extension: string): string;
var
  Buf: string;
  Ext: string;
  Len: Dword;
  Pre: string;
  TmpDir: string;
begin
  Result := '';
  if Prefix = '' then
    Pre := 'zip'
  else
    Pre := Prefix;
  if Extension = '' then
    Ext := EXT_ZIPL
  else
    Ext := Extension;
  if Length(TempDir) = 0 then // Get the system temp dir
  begin
    // 1. The path specified by the TMP environment variable.
    // 2. The path specified by the TEMP environment variable, if TMP is not defined.
    // 3. The current directory, if both TMP and TEMP are not defined.
    Len := GetTempPath(0, PChar(TmpDir));
    SetLength(TmpDir, Len);
    GetTempPath(Len, PChar(TmpDir));
  end
  else // Use Temp dir provided by ZipMaster
    TmpDir := DelimitPath(TempDir, True);
  SetLength(Buf, MAX_PATH + 12);
  if GetTempFileName(PChar(TmpDir), PChar(Pre), 0, PChar(Buf)) <> 0 then
  begin
    Buf := PChar(Buf);
    // Needed because GetTempFileName creates the file also.
{$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.DeleteFile(Buf);
    Result := ChangeFileExt(Buf, Ext); // And finally change the extension.
  end;
end;

function TCustomZipMaster.MergeZippedFiles(Opts: TZMMergeOpts): Integer;
begin
  Result := FBody.Run(TZMOpMerge.Create(Opts));
end;

function TCustomZipMaster.QueryZip(const FName: TFileName): Integer;
begin
  Result := ZMUtils.QueryZip(FName);
end;

function TCustomZipMaster.ReadSpan(const InFileName: string;
  var OutFilePath: string): Integer;
begin
  Result := FBody.Run(TZMOpReadSpan.Create(InFileName, OutFilePath, False));
end;

function TCustomZipMaster.ReEntry: Boolean;
begin
  Result := State >= ZsBusy;
  if Result then
    TZMCommand(FBody).ReEntered;
end;

function TCustomZipMaster.Rename(RenameList: TList; DateTime: Integer;
  How: TZMRenameOpts = HtrDefault): Integer;
begin
  Result := FBody.Run(TZMOpRename.Create(RenameList, DateTime, How));
end;

procedure TCustomZipMaster.AuxWasChanged;
begin
  if (not NoReadAux) or (CsDesigning in ComponentState) or
    (CsLoading in ComponentState) then
    TZMBody(FBody).AuxWasChanged;
end;

function TCustomZipMaster.GetAddCompLevel: Integer;
begin
  Result := TZMBody(FBody).AddCompLevel;
end;

function TCustomZipMaster.GetAddFrom: TDateTime;
begin
  Result := TZMBody(FBody).AddFrom;
end;

function TCustomZipMaster.GetAddOptions: TZMAddOpts;
begin
  Result := TZMBody(FBody).AddOptions;
end;

function TCustomZipMaster.GetAddStoreSuffixes: TZMAddStoreExts;
begin
  Result := TZMBody(FBody).AddStoreSuffixes;
end;

function TCustomZipMaster.GetBlockedCnt: Integer;
begin
  Result := TZMCommand(FBody).BlockedCnt;
end;

function TCustomZipMaster.GetConfirmErase: Boolean;
begin
  Result := TZMBody(FBody).ConfirmErase;
end;

function TCustomZipMaster.GetEncodeAs: TZMEncodingOpts;
begin
  Result := TZMBody(FBody).EncodeAs;
end;

function TCustomZipMaster.GetEncoding: TZMEncodingOpts;
begin
  Result := TZMBody(FBody).Encoding;
end;

function TCustomZipMaster.GetEncoding_CP: Cardinal;
begin
  Result := TZMBody(FBody).Encoding_CP;
end;

function TCustomZipMaster.GetExtAddStoreSuffixes: string;
begin
  Result := TZMBody(FBody).ExtAddStoreSuffixes;
end;

function TCustomZipMaster.GetExtErrCode: Cardinal;
begin
  if not IsActive then
    Result := ZM_Error(ZE_Inactive, {_LINE_}1608)
  else
    Result := TZMBody(FBody).Errors.ExtCode;
end;

function TCustomZipMaster.GetExtrBaseDir: string;
begin
  Result := TZMBody(FBody).ExtrBaseDir;
end;

function TCustomZipMaster.GetExtrOptions: TZMExtrOpts;
begin
  Result := TZMBody(FBody).ExtrOptions;
end;

function TCustomZipMaster.GetExtStream: TStream;
begin
  Result := TZMLister(FBody).ExtStream;
end;

function TCustomZipMaster.GetHowToDelete: TZMDeleteOpts;
begin
  Result := TZMBody(FBody).HowToDelete;
end;

function TCustomZipMaster.GetKeepFreeOnAllDisks: Cardinal;
begin
  Result := TZMBody(FBody).Span.KeepFreeOnAllDisks;
end;

function TCustomZipMaster.GetKeepFreeOnDisk1: Cardinal;
begin
  Result := TZMBody(FBody).Span.KeepFreeOnDisk1;
end;

function TCustomZipMaster.GetMaxVolumeSize: Int64;
begin
  Result := TZMBody(FBody).Span.MaxVolumeSize;
end;

function TCustomZipMaster.GetMaxVolumeSizeKb: Integer;
begin
  Result := TZMBody(FBody).Span.MaxVolumeSize div 1024;
end;

function TCustomZipMaster.GetMinFreeVolumeSize: Cardinal;
begin
  Result := TZMBody(FBody).Span.MinFreeVolumeSize;
end;

function TCustomZipMaster.GetNoSkipping: TZMSkipAborts;
begin
  Result := TZMBody(FBody).NoSkipping;
end;

function TCustomZipMaster.GetNotMainThread: Boolean;
begin
  Result := TZMBody(FBody).NotMainThread;
end;

function TCustomZipMaster.Get_Password: string;
begin
  Result := TZMBody(FBody).Password;
end;

function TCustomZipMaster.GetPasswordReqCount: Longword;
begin
  Result := TZMBody(FBody).PasswordReqCount;
end;

function TCustomZipMaster.GetProgressDetail: TZMProgressDetails;
begin
  Result := TZMBody(FBody).Progress;
end;

function TCustomZipMaster.GetRootDir: string;
begin
  Result := TZMBody(FBody).RootDir;
end;

function TCustomZipMaster.GetSFXCaption: string;
begin
  Result := TZMBody(FBody).SFX.Caption;
end;

function TCustomZipMaster.GetSFXCommandLine: string;
begin
  Result := TZMBody(FBody).SFX.CommandLine;
end;

function TCustomZipMaster.GetSFXDefaultDir: string;
begin
  Result := TZMBody(FBody).SFX.DefaultDir;
end;

function TCustomZipMaster.GetSFXIcon: TIcon;
begin
  Result := TZMBody(FBody).SFX.Icon;
end;

function TCustomZipMaster.GetSFXMessage: string;
begin
  Result := TZMBody(FBody).SFX.Message;
end;

function TCustomZipMaster.GetSFXOptions: TZMSFXOpts;
begin
  Result := TZMBody(FBody).SFX.Options;
end;

function TCustomZipMaster.GetSFXOverwriteMode: TZMOvrOpts;
begin
  Result := TZMBody(FBody).SFX.OverwriteMode;
end;

function TCustomZipMaster.GetSFXPath: string;
begin
  Result := TZMBody(FBody).SFX.Path;
end;

function TCustomZipMaster.GetSFXRegFailPath: string;
begin
  Result := TZMBody(FBody).SFX.RegFailPath;
end;

function TCustomZipMaster.GetSpanOptions: TZMSpanOpts;
begin
  Result := TZMBody(FBody).Span.Options;
end;

function TCustomZipMaster.GetState: TZMStates;
begin
  Result := TZMCommand(FBody).State;
end;

function TCustomZipMaster.GetTempDir: string;
begin
  Result := TZMBody(FBody).TempDir;
end;

function TCustomZipMaster.GetUnattended: Boolean;
begin
  Result := TZMBody(FBody).Unattended;
end;

function TCustomZipMaster.GetUseDirOnlyEntries: Boolean;
begin
  Result := TZMLister(FBody).UseDirOnlyEntries;
end;

function TCustomZipMaster.GetWriteOptions: TZMWriteOpts;
begin
  Result := TZMBody(FBody).WriteOptions;
end;

function TCustomZipMaster.GetZipComment: AnsiString;
begin
  Result := TZMLister(FBody).ZipComment;
end;

function TCustomZipMaster.GetZipFileName: string;
begin
  Result := TZMLister(FBody).ZipFileName;
end;

function TCustomZipMaster.IsActive: Boolean;
begin
  Result := TZMCommand(FBody).IsActive;
end;

(* SetActive
  sets the following values
 0 - not active
 1 - active
 -1 - active in design/loading state (no Active functions allowed)
*)
procedure TCustomZipMaster.SetActive(Value: Boolean);
begin
  TZMCommand(FBody).Active := Value;
end;

procedure TCustomZipMaster.SetAddCompLevel(const Value: Integer);
begin
  if not ReEntry then
    TZMBody(FBody).AddCompLevel := Value;
end;

procedure TCustomZipMaster.SetAddFrom(const Value: TDateTime);
begin
  if not ReEntry then
    TZMBody(FBody).AddFrom := Value;
end;

procedure TCustomZipMaster.SetAddOptions(const Value: TZMAddOpts);
begin
  if not ReEntry then
    TZMBody(FBody).AddOptions := Value;
end;

procedure TCustomZipMaster.SetAddStoreSuffixes(const Value: TZMAddStoreExts);
begin
  if not ReEntry then
    TZMBody(FBody).AddStoreSuffixes := Value;
end;

procedure TCustomZipMaster.SetCancel(Value: Boolean);
begin
  TZMBody(FBody).Cancel := ZS_Canceled;
end;

procedure TCustomZipMaster.SetConfirmErase(const Value: Boolean);
begin
  if not ReEntry then
    TZMBody(FBody).ConfirmErase := Value;
end;

procedure TCustomZipMaster.SetDLLDirectory(const Value: string);
begin
  if not ReEntry then
    FDLLDirectory := Value;
end;

procedure TCustomZipMaster.SetDLL_Load(const Value: Boolean);
begin
  TZMCommand(FBody).DLL_Load := Value;
end;

procedure TCustomZipMaster.SetEncodeAs(const Value: TZMEncodingOpts);
begin
  if not ReEntry then
    TZMBody(FBody).EncodeAs := Value;
end;

procedure TCustomZipMaster.SetEncoding(const Value: TZMEncodingOpts);
var
  Lister: TZMLister;
begin
  if not ReEntry then
  begin
    Lister := FBody as TZMLister;
    if Lister.Encoding <> Value then
      Lister.Encoding := Value;
  end;
end;

procedure TCustomZipMaster.SetEncoding_CP(Value: Cardinal);
var
  Info: TCPInfo;
  Lister: TZMLister;
begin
  if not ReEntry then
  begin
    if not GetCPInfo(Value, Info) then
      Value := 0;
    Lister := FBody as TZMLister;
    if Lister.Encoding_CP <> Value then
      Lister.Encoding_CP := Value;
  end;
end;

procedure TCustomZipMaster.SetErrCode(Value: Integer);
begin
  if not ReEntry then
    TZMBody(FBody).Errors.Code := Value;
end;

procedure TCustomZipMaster.SetExtAddStoreSuffixes(const Value: string);
var
  C: Char;
  I: Integer;
  TempStr: string;
begin
  if not ReEntry then
  begin
    if Value <> '' then
    begin
      C := ':';
      I := 1;
      while I <= Length(Value) do
      begin
        C := Value[I];
        if C <> '.' then
          TempStr := TempStr + '.';
        while (C <> ':') and (I <= Length(Value)) do
        begin
          C := Value[I];
          if (C = ';') or (C = ':') or (C = ',') then
            C := ':';
          TempStr := TempStr + C;
          Inc(I);
        end;
      end;
      if C <> ':' then
        TempStr := TempStr + ':';
      TZMBody(FBody).AddStoreSuffixes := TZMBody(FBody).AddStoreSuffixes
        + [AssEXT];
      TZMBody(FBody).ExtAddStoreSuffixes := Lowercase(TempStr);
    end
    else
    begin
      TZMBody(FBody).AddStoreSuffixes := TZMBody(FBody).AddStoreSuffixes
        - [AssEXT];
      TZMBody(FBody).ExtAddStoreSuffixes := '';
    end;
  end;
end;

procedure TCustomZipMaster.SetExtErrCode(const Value: Cardinal);
begin
  if not ReEntry then
    TZMBody(FBody).Errors.ExtCode := Value;
end;

procedure TCustomZipMaster.SetExtrBaseDir(const Value: string);
begin
  if not ReEntry then
    TZMBody(FBody).ExtrBaseDir := Value;
end;

procedure TCustomZipMaster.SetExtrOptions(const Value: TZMExtrOpts);
begin
  if not ReEntry then
    TZMBody(FBody).ExtrOptions := Value;
end;

procedure TCustomZipMaster.SetExtStream(const Value: TStream);
begin
  TZMCommand(FBody).SetExtStream(Value);
end;

procedure TCustomZipMaster.SetFSpecArgs(const Value: TStrings);
begin
  // never set
end;

procedure TCustomZipMaster.SetFSpecArgsExcl(const Value: TStrings);
begin
  // never set
end;

procedure TCustomZipMaster.SetHowToDelete(const Value: TZMDeleteOpts);
begin
  if not ReEntry then
    TZMBody(FBody).HowToDelete := Value;
end;

procedure TCustomZipMaster.SetKeepFreeOnAllDisks(const Value: Cardinal);
begin
  if not ReEntry then
    TZMBody(FBody).Span.KeepFreeOnAllDisks := Value;
end;

procedure TCustomZipMaster.SetKeepFreeOnDisk1(const Value: Cardinal);
begin
  if not ReEntry then
    TZMBody(FBody).Span.KeepFreeOnDisk1 := Value;
end;

procedure TCustomZipMaster.SetLanguage(const Value: string);
begin
  TZMCommand(FBody).Language := Value;
end;

procedure TCustomZipMaster.SetMaxVolumeSize(const Value: Int64);
begin
  if not ReEntry then
  begin
    if Value > 0 then
      TZMBody(FBody).Span.MaxVolumeSize := Value
    else
      TZMBody(FBody).Span.MaxVolumeSize := 0;
  end;
end;

procedure TCustomZipMaster.SetMaxVolumeSizeKb(const Value: Integer);
begin
  if not ReEntry then
  begin
    if Value > 0 then
      TZMBody(FBody).Span.MaxVolumeSize := Value * 1024
    else
      TZMBody(FBody).Span.MaxVolumeSize := 0;
  end;
end;

procedure TCustomZipMaster.SetMinFreeVolumeSize(const Value: Cardinal);
begin
  if not ReEntry then
    TZMBody(FBody).Span.MinFreeVolumeSize := Value;
end;

procedure TCustomZipMaster.SetNoReadAux(const Value: Boolean);
begin
  if not ReEntry then
    TZMBody(FBody).NoReadAux := Value;
end;

procedure TCustomZipMaster.SetNoSkipping(const Value: TZMSkipAborts);
begin
  if not ReEntry then
    TZMBody(FBody).NoSkipping := Value;
end;

procedure TCustomZipMaster.SetNotMainThread(const Value: Boolean);
begin
  TZMBody(FBody).NotMainThread := Value;
end;

procedure TCustomZipMaster.Set_Password(const Value: string);
begin
  if Password <> Value then
  begin
    if not ReEntry then
      TZMBody(FBody).Password := Value;
  end;
end;

procedure TCustomZipMaster.SetPasswordReqCount(Value: Longword);
begin
  if Value > 15 then
    Value := 15;
  if Value <> PasswordReqCount then
  begin
    if not ReEntry then
      TZMBody(FBody).PasswordReqCount := Value;
  end;
end;

procedure TCustomZipMaster.SetRootDir(const Value: string);
begin
  if not ReEntry then
    TZMBody(FBody).RootDir := Value;
end;

procedure TCustomZipMaster.SetSFXCaption(const Value: string);
begin
  if (not ReEntry) and (SFXCaption <> Value) then
  begin
    TZMBody(FBody).SFX.Caption := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXCommandLine(const Value: string);
begin
  if (not ReEntry) and (SFXCommandLine <> Value) then
  begin
    TZMBody(FBody).SFX.CommandLine := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXDefaultDir(const Value: string);
begin
  if (not ReEntry) and (SFXDefaultDir <> Value) then
  begin
    TZMBody(FBody).SFX.DefaultDir := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXIcon(Value: TIcon);
begin
  if (not ReEntry) and (SFXIcon <> Value) then
  begin
    TZMBody(FBody).SFX.Icon := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXMessage(const Value: string);
begin
  if (not ReEntry) and (SFXMessage <> Value) then
  begin
    TZMBody(FBody).SFX.Message := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXOptions(const Value: TZMSFXOpts);
begin
  if (not ReEntry) and (SFXOptions <> Value) then
  begin
    TZMBody(FBody).SFX.Options := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXOverwriteMode(const Value: TZMOvrOpts);
begin
  if (not ReEntry) and (SFXOverwriteMode <> Value) then
  begin
    TZMBody(FBody).SFX.OverwriteMode := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXPath(const Value: string);
begin
  if (not ReEntry) and (SFXPath <> Value) then
  begin
    TZMBody(FBody).SFX.Path := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSFXRegFailPath(const Value: string);
begin
  if (not ReEntry) and (SFXRegFailPath <> Value) then
  begin
    TZMBody(FBody).SFX.RegFailPath := Value;
    AuxWasChanged;
  end;
end;

procedure TCustomZipMaster.SetSpanOptions(const Value: TZMSpanOpts);
begin
  if (not ReEntry) and (SpanOptions <> Value) then
  begin
    if (Value * [SpNoVolumeName, SpCompatName]) <>
      (SpanOptions * [SpNoVolumeName, SpCompatName]) then
      AuxWasChanged;
    TZMBody(FBody).Span.Options := Value;
  end;
end;

procedure TCustomZipMaster.SetTempDir(const Value: string);
begin
  if not ReEntry then
  begin
    if Value <> '' then
      TZMBody(FBody).TempDir := DelimitPath(Value, True)
    else
      TZMBody(FBody).TempDir := '';
  end;
end;

procedure TCustomZipMaster.SetTrace(const Value: Boolean);
begin
  if not ReEntry then
    FTrace := Value;
end;

procedure TCustomZipMaster.SetUnattended(const Value: Boolean);
begin
  if not ReEntry then
    TZMBody(FBody).Unattended := Value;
end;

procedure TCustomZipMaster.SetUseDirOnlyEntries(const Value: Boolean);
begin
  FBody.Run(TZMOpSetUseDirOnly.Create(Value));
end;

{$IFNDEF UNICODE}
procedure TCustomZipMaster.SetUseUTF8(const Value: Boolean);
begin
  TZMCommand(FBody).SetUseUTF8(Value);
end;
{$ENDIF}

procedure TCustomZipMaster.SetVerbose(const Value: Boolean);
begin
  if not ReEntry then
    FVerbose := Value;
end;

procedure TCustomZipMaster.SetVersion(const Value: string);
begin
  // Read only
end;

procedure TCustomZipMaster.SetWriteOptions(const Value: TZMWriteOpts);
begin
  if (not ReEntry) and (WriteOptions <> Value) then
  begin
    if (ZwoDiskSpan in Value) <> (ZwoDiskSpan in WriteOptions) then
      AuxWasChanged;
    TZMBody(FBody).WriteOptions := Value;
  end;
end;

procedure TCustomZipMaster.SetZipComment(const Value: AnsiString);
begin
  TZMCommand(FBody).SetZipComment(Value);
end;

procedure TCustomZipMaster.SetZipFileName(const Value: string);
begin
  TZMCommand(FBody).SetZipFileName(ExpandFileName(Value));
end;

procedure TCustomZipMaster.ShowExceptionError(const ZMExcept: Exception);
begin
  TZMBody(FBody).ShowExceptionError(ZMExcept);
end;

procedure TCustomZipMaster.ShowZipFmtMessage(Id: Integer;
  const Args: array of const; Display: Boolean);
begin
  TZMBody(FBody).ShowFmtMessage(Id, Args, Display);
end;

procedure TCustomZipMaster.ShowZipMessage(Ident: Integer;
  const UserStr: string);
begin
  TZMBody(FBody).ShowMessage(Ident, UserStr);
end;

function TCustomZipMaster.Undeflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
begin
  Result := FBody.Run(TZMOpUndeflate.Create(OutStream, InStream, Length,
    Method, CRC));
end;

function TCustomZipMaster.UnzipToFile(const Entry: TZMDirEntry;
  const DestName: string): Integer;
begin
  Result := FBody.Run(TZMOpUnzipToFile.Create(DestName, Entry));
end;

function TCustomZipMaster.UnzipToStream(const Entry: TZMDirEntry;
  DestStream: TStream): Integer;
begin
  Result := FBody.Run(TZMOpUnzipToStream.Create(DestStream, Entry));
end;

function TCustomZipMaster.WriteSpan(const InFileName,
  OutFileName: string): Integer;
begin
  Result := FBody.Run(TZMOpWriteSpan.Create(InFileName, OutFileName, False));
end;

function TCustomZipMaster.ZipLoadStr(Id: Integer): string;
begin
  Result := TZMBody(FBody).ZipLoadStr(Id);
end;

end.
