Unit ZMMsg;
 
// Built by ZipResMaker
//   DO NOT MODIFY
//  ZMMsg.pas - default messages and compressed tables
 
(* **************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009-2013 Russell Peters and Roger Aelbrecht
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
************************************************** *)
//Generated 2014-02-19
 
Interface
 
Uses
  {$IFDEF VERDXE2up}
    System.Classes;
  {$ELSE}
    Classes;
  {$ENDIF}
 
Const
  ZS_Success              = 0;   // "Success"
  ZE_UnknownError         = 1;   // "Unknown Error"
  ZA_Author               = 2;   // "R.Peters"
  ZA_Desc                 = 3;   // "Language Neutral"
  ZA_ID                   = 4;   // "$0409"
  ZA_Language             = 5;   // "US: default"
  ZC_Abort                = 6;   // "&Abort"
  ZC_Cancel               = 7;   // "&Cancel"
  ZC_CancelAll            = 8;   // "CancelAll"
  ZC_Caption              = 9;   // "Password"
  ZC_FileConflict         = 10;  // "File conflict!"
  ZC_Ignore               = 11;  // "&Ignore"
  ZC_Merge                = 12;  // "'%s'\nwill overwrite\n '%s'\nRename to '%s'?"
  ZC_MessageConfirm       = 13;  // "Confirm Password "
  ZC_MessageEnter         = 14;  // "Enter Password "
  ZC_No                   = 15;  // "&No"
  ZC_NoToAll              = 16;  // "NoToAll"
  ZC_OK                   = 17;  // "&OK"
  ZC_Retry                = 18;  // "&Retry"
  ZC_Yes                  = 19;  // "&Yes"
  ZC_YesToAll             = 20;  // "YesToAll"
  ZE_AutoSFXWrong         = 23;  // "Error %.1d occurred during Auto SFX creation."
  ZE_BadCRC               = 24;  // "CRC error"
  ZE_BadDll               = 25;  // "Unable to load %s - It is old or corrupt"
  ZE_BadFileName          = 26;  // "Invalid Filename: '%s'"
  ZE_Blocked              = 27;  // "Busy or not Active"
  ZE_BrowseError          = 28;  // "Error while browsing resources."
  ZE_BuildBaseError       = 29;  // "Error building extract base path '%s'"
  ZE_BuildPathError       = 30;  // "Error building path '%s'"
  ZE_CEHBadRead           = 31;  // "Error while reading a central header"
  ZE_CEHBadWrite          = 32;  // "Error while writing a central header"
  ZE_CEHDataSize          = 33;  // "The combined length of CEH + FileName + FileComment + ExtraData exceeds 65535"
  ZE_CEHWrongSig          = 34;  // "A central header signature is wrong"
  ZE_CopyError            = 35;  // "File copy error"
  ZE_CopyFailed           = 36;  // "Copying a file from '%s' to '%s' failed"
  ZE_Copying              = 37;  // "Copying: %s"
  ZE_CryptError           = 38;  // "Crypt error"
  ZE_DataCopy             = 39;  // "Error copying compressed data: '%s'"
  ZE_DataDesc             = 40;  // "Error while reading/writing a data descriptor area: '%s'"
  ZE_DetachedHeaderTooBig = 41;  // "Detached SFX Header too large"
  ZE_DLLCritical          = 42;  // "critical DLL Error %d"
  ZE_DriveNoMount         = 43;  // "Drive %s is NOT defined"
  ZE_DuplFileName         = 44;  // "Duplicate Filename: '%s'"
  ZE_EntryCancelled       = 45;  // "Entry cancelled"
  ZE_EOCBadRead           = 46;  // "Error while reading the End Of Central Directory"
  ZE_EOCBadWrite          = 47;  // "Error while writing the End Of Central Directory"
  ZE_ErrorUnknown         = 48;  // "UnKnown error in function"
  ZE_EventEx              = 49;  // "Exception in Event '%s'"
  ZE_Except               = 50;  // "Exception in Event handler %s"
  ZE_ExceptErr            = 51;  // "Error Exception: "
  ZE_ExeSections          = 52;  // "Error while reading executable sections."
  ZE_Existing             = 53;  // "File exists: '%s'"
  ZE_FailedSeek           = 54;  // "Seek error in input file"
  ZE_FatalZip             = 55;  // "Fatal Error in DLL: abort exception"
  ZE_FileChanged          = 56;  // "File changed"
  ZE_FileCreate           = 57;  // "Error: Could not create file '%s'"
  ZE_FileError            = 58;  // "File Error: '%s'"
  ZE_FileOpen             = 59;  // "Zip file could not be opened"
  ZE_Inactive             = 60;  // "not Active"
  ZE_InIsOutStream        = 61;  // "Input stream may not be set to the output stream"
  ZE_InputNotExe          = 62;  // "Error: input file is not an .EXE file"
  ZE_InternalError        = 63;  // "Internal error"
  ZE_InvalidArguments     = 64;  // "Invalid Arguments"
  ZE_InvalidDateTime      = 65;  // "Invalid date/time argument for file: "
  ZE_InvalidEntry         = 66;  // "Invalid zip entry!"
  ZE_InvalidParameter     = 67;  // "Invalid Parameter! '%s'"
  ZE_InvalidZip           = 68;  // "Invalid zip file"
  ZE_LoadErr              = 69;  // "Error [%d %s] loading %s"
  ZE_LogicError           = 70;  // "Internal logic error!"
  ZE_LOHBadRead           = 71;  // "Error while reading a local header"
  ZE_LOHBadWrite          = 72;  // "Error while writing a local header"
  ZE_LOHWrongName         = 73;  // "Local and Central names different : %s"
  ZE_NoAppend             = 74;  // "Append failed"
  ZE_NoChangeDir          = 75;  // "Cannot change path"
  ZE_NoCopyIcon           = 76;  // "Cannot copy icon."
  ZE_NoDestDir            = 77;  // "Destination directory '%s' must exist!"
  ZE_NoDiskSpace          = 78;  // "This disk has not enough free space available"
  ZE_NoDll                = 79;  // "Failed to load %s%s"
  ZE_NoEncrypt            = 80;  // "encryption not supported"
  ZE_NoExeIcon            = 81;  // "No icon resources found in executable."
  ZE_NoExeResource        = 82;  // "No resources found in executable."
  ZE_NoExtrDir            = 83;  // "Extract directory '%s' must exist"
  ZE_NoIcon               = 84;  // "No icon found."
  ZE_NoIconFound          = 85;  // "No matching icon found."
  ZE_NoInFile             = 86;  // "Input file does not exist"
  ZE_NoInStream           = 87;  // "No input stream"
  ZE_NoMem                = 88;  // "Requested memory not available"
  ZE_NoneSelected         = 89;  // "No files selected for '%s'"
  ZE_NoOutFile            = 90;  // "Creation of output file failed"
  ZE_NoOutStream          = 91;  // "No output stream"
  ZE_NoOverwrite          = 92;  // "Cannot overwrite existing file"
  ZE_NoProcess            = 93;  // "Cannot process invalid zip"
  ZE_NoProtected          = 94;  // "Cannot change details of Encrypted file"
  ZE_NoRenamePart         = 95;  // "Last part left as : %s"
  ZE_NoSkipping           = 96;  // "Skipping not allowed"
  ZE_NoStreamSpan         = 97;  // "Multi-parts not supported on streams!"
  ZE_NotChangeable        = 98;  // "Cannot write to %s"
  ZE_NoTempFile           = 99;  // "Temporary file could not be created"
  ZE_NotFound             = 100; // "File not found!"
  ZE_NothingToDel         = 101; // "Error - no files selected for deletion"
  ZE_NothingToDo          = 102; // "Nothing to do"
  ZE_NothingToZip         = 103; // "Error - no files to zip!"
  ZE_NoUnattSpan          = 104; // "Unattended disk spanning not implemented"
  ZE_NoValidZip           = 105; // "This archive is not a valid Zip archive"
  ZE_NoVolume             = 106; // "Volume label could not be set"
  ZE_NoWrite              = 107; // "Write error in output file"
  ZE_NoZipSFXBin          = 108; // "Error: SFX stub '%s' not found!"
  ZE_NoZipSpecified       = 109; // "Error - no zip file specified!"
  ZE_PasswordCancel       = 110; // "Password cancelled!"
  ZE_PasswordFail         = 111; // "Password failed!"
  ZE_RangeError           = 112; // "Index (%d) outside range 0..%d"
  ZE_ReadError            = 113; // "Error reading file"
  ZE_ReadZipError         = 114; // "Seek error reading Zip archive!"
  ZE_SameAsSource         = 115; // "source and destination on same removable drive"
  ZE_SeekError            = 116; // "File seek error"
  ZE_SetDateError         = 117; // "Error setting file date"
  ZE_SetFileAttributes    = 118; // "Error setting file attributes"
  ZE_SetFileInformation   = 119; // "Error setting file information"
  ZE_SetFileTimes         = 120; // "Error setting file times"
  ZE_SFXBadRead           = 121; // "Error reading SFX"
  ZE_SFXCopyError         = 122; // "Error while copying the SFX data"
  ZE_SourceIsDest         = 123; // "Source archive is the same as the destination archive!"
  ZE_StreamNoSupport      = 124; // "Operation not supported on streams"
  ZE_StringTooLong        = 125; // "Error: Combined SFX strings unreasonably long!"
  ZE_TooManyParts         = 126; // "More than 999 parts in multi volume archive"
  ZE_UnatAddPWMiss        = 127; // "Error - no add password given"
  ZE_UnatExtPWMiss        = 128; // "Error - no extract password given"
  ZE_UnattPassword        = 129; // "Unattended action not possible without a password"
  ZE_Unknown              = 130; // " Unknown error %d"
  ZE_Unsupported          = 131; // "Unsupported zip version"
  ZE_WildName             = 132; // "Wildcards are not allowed in Filename or file specification"
  ZE_WriteError           = 133; // "Error writing file"
  ZE_WrongLength          = 134; // "Wrong length"
  ZE_WrongPassword        = 135; // "Error - passwords do NOT match\nPassword ignored"
  ZE_Zip64FieldError      = 136; // "Error reading Zip64 field"
  ZE_ZipDataError         = 137; // "Zip data error"
  ZE_ZLib                 = 138; // "ZLib error: %d %s"
  ZS_Abort                = 139; // "User Abort"
  ZS_AnotherDisk          = 140; // "This disk is part of a backup set,\nplease insert another disk"
  ZS_AskDeleteFile        = 141; // "There is already a file %s\nDo you want to overwrite this file"
  ZS_AskPrevFile          = 142; // "ATTENTION: This is previous disk no %d!!!\nAre you sure you want to overwrite the contents"
  ZS_Canceled             = 143; // "User canceled operation"
  ZS_Confirm              = 144; // "Confirm"
  ZS_CopyCentral          = 145; // "Central directory"
  ZS_Deleting             = 146; // "EraseFloppy - Deleting %s"
  ZS_DllLoaded            = 147; // "Loaded %s"
  ZS_DllUnloaded          = 148; // "Unloaded %s"
  ZS_Erase                = 149; // "Erase %s"
  ZS_Erasing              = 150; // "EraseFloppy - Removing %s"
  ZS_GetNewDisk           = 151; // "GetNewDisk Opening: %s"
  ZS_InDrive              = 152; // "\nin drive: %s"
  ZS_InsertAVolume        = 153; // "Please insert disk volume %.1d"
  ZS_InsertDisk           = 154; // "Please insert last disk"
  ZS_InsertVolume         = 155; // "Please insert disk volume %.1d of %.1d"
  ZS_InvalidPath          = 156; // "Invalid or illegal path"
  ZS_Skipped              = 157; // "Skipped %s %d"
  ZS_TempZip              = 158; // "Temporary zipfile: %s"
  ZW_EOCCommentLen        = 159; // "EOC comment length error"
  ZW_WrongZipStruct       = 160; // "Warning - Error in zip structure!"
  ZZ_ZLibData             = 161; // "ZLib Error: Data error"
  ZZ_ZLibFile             = 162; // "ZLib Error: File error"
  ZZ_ZLibIncompatible     = 163; // "ZLib Error: Incompatible version"
  ZZ_ZLibNoMem            = 164; // "ZLib Error: Insufficient memory"
  ZZ_ZLibStream           = 165; // "ZLib Error: Stream error"
  ZZ_ZLibUnknown          = 166; // "ZLib Error: Unknown error"
  ZP_Archive              = 172; // "*Resetting Archive bit"
  ZP_CopyZipFile          = 173; // "*Copying Zip File"
  ZP_SFX                  = 174; // "*SFX"
  ZP_Header               = 175; // "*??"
  ZP_Finish               = 176; // "*Finalising"
  ZP_Copying              = 177; // "*Copying"
  ZP_CentrlDir            = 178; // "*Writing Central Directory"
  ZP_Checking             = 179; // "*Checking"
  ZP_Loading              = 180; // "*Loading Directory"
  ZP_Joining              = 181; // "*Joining split zip file"
  ZP_Splitting            = 182; // "*Splitting zip file"
  ZP_Writing              = 183; // "*Writing zip file"
  ZP_PreCalc              = 184; // "*Precalculating CRC"
  ZP_Processing           = 185; // "*Processing"
  ZP_Merging              = 186; // "*Merging"
  ZD_GOOD                 = 187; // "Good"
  ZD_CANCELLED            = 188; // "Cancelled"
  ZD_ABORT                = 189; // "Aborted by User!"
  ZD_CALLBACK             = 190; // "Callback exception"
  ZD_MEMORY               = 191; // "No memory"
  ZD_STRUCT               = 192; // "Invalid structure"
  ZD_ERROR                = 193; // "Fatal error"
  ZD_PASSWORD_FAIL        = 194; // "Password failed!"
  ZD_PASSWORD_CANCEL      = 195; // "Password cancelled!"
  ZD_INVAL_ZIP            = 196; // "Invalid zip structure!"
  ZD_NO_CENTRAL           = 197; // "No Central directory!"
  ZD_ZIP_EOF              = 198; // "Unexpected end of Zip file!"
  ZD_ZIP_END              = 199; // "Premature end of file!"
  ZD_ZIP_NOOPEN           = 200; // "Error opening Zip file!"
  ZD_ZIP_MULTI            = 201; // "Multi-part Zips not supported!"
  ZD_NOT_FOUND            = 202; // "File not found!"
  ZD_LOGIC_ERROR          = 203; // "Internal logic error!"
  ZD_NOTHING_TO_DO        = 204; // "Nothing to do!"
  ZD_BAD_OPTIONS          = 205; // "Bad Options specified!"
  ZD_TEMP_FAILED          = 206; // "Temporary file failure!"
  ZD_NO_FILE_OPEN         = 207; // "File not found or no permission!"
  ZD_ERROR_READ           = 208; // "Error reading file!"
  ZD_ERROR_CREATE         = 209; // "Error creating file!"
  ZD_ERROR_WRITE          = 210; // "Error writing file!"
  ZD_ERROR_SEEK           = 211; // "Error seeking in file!"
  ZD_EMPTY_ZIP            = 212; // "Missing or empty zip file!"
  ZD_INVAL_NAME           = 213; // "Invalid characters in filename!"
  ZD_GENERAL              = 214; // "Error "
  ZD_MISS                 = 215; // "Nothing found"
  ZD_WARNING              = 216; // "Warning: "
  ZD_ERROR_DELETE         = 217; // "Delete failed"
  ZD_FATAL_IMPORT         = 218; // "Fatal Error - could not import symbol!"
  ZD_SKIPPING             = 219; // "Skipping: "
  ZD_LOCKED               = 220; // "File locked"
  ZD_DENIED               = 221; // "Access denied"
  ZD_DUPNAME              = 222; // "Duplicate internal name"
  ZD_SKIPPED              = 223; // "Skipped files"
 
Const
 MSG_ID_MASK   = $1FF;
 MAX_ID        = 223;
 ZS_NoLanguage = 224;
 
// name of compressed resource data
const 
  DZRES_Lng = 'DZResLng';  // compressed language strings
  DZRES_SFX = 'DZResSFX';  // stored UPX Dll version as string
  DZRES_Dll = 'DZResDll';  // stored UPX Dll
 
procedure ReadCompressedStrings(si: TMemoryStream);
 
function FindIndentifier(const Ident: string): integer;
 
// Extended error codes
// 00FF FFFF  LLLL LLLL  LLLL mmmE  EEEE EEEE  {31 .. 0}
// F _ file number    [ 6 bits  = 0..63]
// L _ line number    [12 bits = 0..4095]
// m _ unused         [ 3 bits = 0..7]
// E _ error          [ 9 bits = 0..511]
const
  ZERR_UNIT_BITS = 6;
  ZERR_LINE_BITS = 12;
  ZERR_MISC_BITS = 3;
  ZERR_ERROR_BITS = 9;
  ZERR_UNIT_MASK_SHIFTED = (1 shl ZERR_UNIT_BITS) - 1;
  ZERR_LINE_MASK_SHIFTED = (1 shl ZERR_LINE_BITS) - 1;
//  ZERR_MISC_MASK_SHIFTED = (1 shl ZERR_MISC_BITS) - 1;
  ZERR_UNIT_SHIFTS = ZERR_LINE_BITS + ZERR_MISC_BITS + ZERR_ERROR_BITS;
  ZERR_LINE_SHIFTS = ZERR_MISC_BITS + ZERR_ERROR_BITS;
  ZERR_MISC_SHIFTS = ZERR_ERROR_BITS;
  ZERR_UNIT_MASK = ZERR_UNIT_MASK_SHIFTED shl ZERR_UNIT_SHIFTS;
  ZERR_LINE_MASK = ZERR_LINE_MASK_SHIFTED shl ZERR_LINE_SHIFTS;
//  ZERR_MISC_MASK = ZERR_MISC_MASK_SHIFTED shl ZERR_MISC_SHIFTS;
  ZERR_ERROR_MASK = (1 shl ZERR_ERROR_BITS) - 1;
 
implementation
 
uses
  ZMUtils;
 
const
 CompBlok: packed array [0..681] of Cardinal = (
  $9F4BEDE6, $576B5985, $FD11B8E4, $341FF305, $426CC27D, $DFBB6D80, 
  $02C3D97C, $81964BB3, $3666C039, $DC72721B, $C1D1BAB6, $C07E3B6D, 
  $EE7D7E74, $A6AFC92D, $B689FB27, $6EAA9254, $89552ADD, $BEE0D1E1, 
  $145D430A, $D1DE7783, $39E4D7E0, $75135F49, $3E69E799, $0D1FE778, 
  $EB3E4EEE, $1E98E752, $83AE0D1D, $6B055D64, $55746EAD, $783C41E6, 
  $BFD9FBF7, $FE8E2EAF, $170F1C34, $7F5826EF, $BD22AA7F, $1CB8AA0A, 
  $0D1E9BBC, $D32D9C3E, $F8673F1C, $A1241E79, $D1F992C6, $8B3CFEC0, 
  $1A38B5F1, $E28A0E7C, $6823CD35, $7C1A3C30, $6156B134, $1362AC9A, 
  $0631EF96, $AD5C3EB5, $F8D73493, $1DDF4680, $263DDF14, $558E26AF, 
  $D7F3A2FA, $31FA94DC, $4EF87651, $AB46C127, $1FEF9532, $9F6A02B0, 
  $6F933763, $C2907D54, $7068F8F0, $DEB0C099, $70F82C20, $5407E293, 
  $521F49BF, $F2A3D0D1, $07E0BFB7, $A770FF34, $DF8B7CCB, $3FC3FBD0, 
  $A1F16034, $61387E33, $7809C71F, $8BC4E0EA, $55861A54, $4548EB9E, 
  $B5926E55, $D9A0AB3A, $54DFC7FD, $34A0EB98, $25827269, $EEFCE550, 
  $3987695C, $65824C61, $69C4562C, $4283A910, $52ABAB1D, $31A54299, 
  $4F81CD36, $8019579E, $55E9A1F5, $26C412F2, $D68E4452, $98ED8A9F, 
  $563FE283, $920B96C5, $616754B4, $A2005E69, $7C75AF3F, $CBD8227D, 
  $A82D7D3C, $B48BAE5D, $6A0641CA, $9685A8B5, $4E238995, $79F0AFEB, 
  $5065AA58, $E5059568, $34673B93, $9DDA237C, $C1A7FAF5, $54091170, 
  $489213A8, $C344613D, $97DF91F2, $21BF67A4, $9E1F9409, $D2CD9348, 
  $86178024, $BA6271AF, $3F97E752, $140ABFAB, $FD822437, $366E9E79, 
  $975F02D8, $2822E1B4, $6A135803, $3A6A151D, $D8268C99, $DB385E12, 
  $66154839, $9565049D, $9AFF426B, $821AC9A7, $D9A38693, $68DA76B6, 
  $5B65964A, $D38AEEAB, $08DD3CAB, $D5193504, $5838C02A, $C2359A3E, 
  $062DF0A7, $36F91218, $1AED9B2B, $29B743F0, $9EE0CD2C, $F6825482, 
  $C3C78BB5, $DF7EE0BD, $84694742, $26E61164, $4017212B, $D70D59A4, 
  $0832E85C, $592DB09F, $BB35B3F9, $20BC414C, $632B835F, $66E424F8, 
  $B8B95021, $DC8E56BE, $270C6330, $1460E45C, $6E7C09F9, $FB181F6E, 
  $012607F4, $219545E7, $BF528386, $2278E661, $556F97D1, $60409928, 
  $13F74F19, $2FA384A5, $C2DD4893, $173DCEB9, $AC21D726, $678E2041, 
  $07F349FB, $B20CE6AB, $09D7245F, $6A24CAE6, $63212555, $1FC4D312, 
  $C9F8C8FC, $D217CBA9, $193F2AC7, $0A7C9DEE, $78062892, $E1A4E874, 
  $8A78237C, $F7F132FC, $EABFA85B, $E4252AB0, $302A2D0B, $47232702, 
  $14D5FD34, $37635165, $5AF7B201, $9B7AB73F, $214AAB24, $4EC9661D, 
  $8E763CB8, $76BC34C2, $61E602AA, $4D667CE1, $1A6B4C9C, $F135F143, 
  $9E48D49F, $42B4F3AA, $90CE619E, $A32DA53C, $B773F9DD, $51BAC9D0, 
  $C9BBF620, $2CD860AC, $66956A5F, $0531185A, $342CDD6C, $A8B998F5, 
  $90E25179, $DB04DA8D, $4BA14D7A, $987D0D46, $832B6556, $64A37E55, 
  $C7D9366B, $4EA24195, $74BB7F2E, $3C474C50, $013E2900, $A6B02E1A, 
  $9E5D36E3, $598AEBE5, $4E9AFE01, $8C0461DA, $C834BEFE, $5013811D, 
  $3DC9002B, $2769C3E9, $0617FEE1, $5C5B5B93, $3B59D353, $41C839FE, 
  $DEFC9C76, $F79D33A1, $9B053917, $01E7FF28, $24BFFC93, $E41093DB, 
  $51D86CD0, $26D74E3F, $ACE046B4, $6A5337D9, $349D869C, $4266FB39, 
  $D5E391DD, $70E0232E, $D80C2E13, $B5664542, $B05139D2, $3CC66AB9, 
  $51A7C199, $1366CF93, $2E85D1B9, $FF492642, $E3B69F18, $942695CC, 
  $63366EFB, $89A029D6, $C13880DC, $92CDA585, $2D28AA9B, $9CE969E5, 
  $8D50DE89, $662119E2, $B493ACFD, $C3213F5A, $0B25506A, $05EAB842, 
  $B08030BA, $A008C164, $93FB9D66, $A801CF9A, $59E6424E, $8A8DCB1E, 
  $1710CB2A, $F1EE6234, $DB1D17CA, $AF9E107B, $A8822860, $3058510D, 
  $0FF645F7, $B80B58C5, $469BF33B, $9EDC6C42, $E9711B2C, $4136C644, 
  $63D13E19, $1D69C9FD, $28FE4239, $B7160ED5, $97EECCDF, $0E66E374, 
  $7FF4EF20, $600EE02A, $55437A36, $E45A6624, $C8590BE6, $60840B4D, 
  $4B21850C, $2F165BF2, $6CA6BC86, $D6ABC2DD, $EDBDA685, $1616171C, 
  $5DA89DC6, $9A130DF8, $4BAACD92, $2C346CE3, $964C8D46, $6148ACA7, 
  $98321342, $2CB2EF1F, $E265D5C2, $5A5CAA82, $BB5D52BF, $2AC584D9, 
  $A402CE30, $60AF58BD, $BC18E168, $CFF77C00, $B678CB26, $E9C71040, 
  $E40F28AB, $3574A13E, $F4565CC7, $B460A229, $3100A7B5, $1B46F317, 
  $26B80D6B, $07C3039B, $0092C1BD, $86CDBF00, $A94A9F36, $9CBAD75B, 
  $D8B2D711, $7DA1361D, $BE7D898C, $5361F088, $1E1437B8, $9C10C296, 
  $1D72B11F, $806CF68E, $FB286049, $2CBAA58F, $6B30044B, $42158609, 
  $306A4928, $358B26CC, $72559EB3, $C82FE2E2, $3697C441, $19EB2AD3, 
  $45377CA6, $F7FB2069, $44D24634, $71D4B0F9, $752474DF, $2FE300CA, 
  $05F5FFD6, $0511E1D5, $67864DE6, $D5945249, $1F4CC6D2, $CCCF45A7, 
  $16C2A93A, $D1959A1D, $A276C422, $4A9BEF29, $CEBC6643, $0CAEE58C, 
  $48AC8CFB, $3E7F557F, $C542FE88, $BF21130A, $2786DD0A, $3C7B2C27, 
  $CEB841AF, $AF734EBB, $B3D7337F, $0CE48C1D, $86F36A40, $93A8DB64, 
  $1630C909, $F3C239AD, $2881CA45, $F57AC8B7, $FB345D6D, $7DA9B473, 
  $4A136D80, $76718372, $BE1677BE, $2AB2CDCF, $DE78BBD9, $498CAF62, 
  $E04DB4C0, $F70FE668, $E48EF10A, $8BB7BE1E, $F1C6FC07, $2EA7DB9D, 
  $D3AB7965, $294CACB3, $1DEF3806, $28C92D1E, $F6045824, $0EA61777, 
  $6E9DEC98, $6F079D33, $5E277A53, $14E93829, $B069AE37, $565F62F4, 
  $64812AA8, $DAEFB291, $D648D2E2, $FF503BB2, $1A13BE84, $2C58B4A0, 
  $05E6CA16, $34C37670, $D92C5EA1, $67F008DB, $51065A3D, $668E2584, 
  $9C08886B, $DD4C9EF3, $5B95BD55, $083542F4, $B4B31B1B, $F1E90C28, 
  $9027CA6A, $B598E114, $31DB343D, $BAF23755, $91BDD729, $3E2DF7F4, 
  $E324198C, $CD993AC2, $6F7417D7, $0C479061, $8D4DDD77, $E1AEB8B4, 
  $EA862B95, $9A6B0B50, $A8B89A8C, $17170CEB, $B0BEAD6C, $1A6D7279, 
  $062D2687, $E4B8EAB5, $E4226953, $9A4C7BD6, $FCD234F8, $5DD3098B, 
  $A6B821AA, $63A41C63, $2AD99F99, $3A9E9A54, $FD5DD84A, $76966D7E, 
  $24407700, $9363DA99, $8A816BF5, $7851BDDD, $62DAA66C, $F2E57F03, 
  $A012EDC0, $AB9F0DE5, $7A3FFC8C, $D920904C, $8B049B01, $03227459, 
  $E22E1763, $A6B49E26, $620C65B6, $5BADB9EA, $31E283C2, $36D548B9, 
  $906BD4AD, $5EDB8948, $764F2587, $3AEBB188, $BCB8787B, $BDBAB879, 
  $14A25539, $8BF5C9F5, $20A72B49, $BD1073B8, $31FEFF7F, $DC69C339, 
  $8FDCA8B4, $A3CBBB7D, $5DEB694A, $D89BBD8F, $A322B36E, $CE228EA5, 
  $13379B8E, $73BCB17E, $B6D46135, $D7ED9E50, $38C7C480, $1DDE50CD, 
  $9AB90BAB, $B1EAFD5C, $F946BB33, $B6DE1DA5, $23BB8AE7, $FD83B70B, 
  $B7AAC694, $61131DE1, $AABB21B3, $A379749F, $3F682F5F, $5A693EE2, 
  $D5BD8F68, $CA6DBE23, $E66107AC, $04B79EE7, $7CB61739, $78D0A1A1, 
  $E5662857, $86C9FCE0, $416FFFEA, $755BD486, $6DDE039A, $40D66EB8, 
  $1EC396CD, $3C292C1B, $941B7EC4, $7ADAA6EC, $FB692101, $E13DAB0C, 
  $EF673DB9, $7B886D2F, $CD79A8E4, $904BEA86, $DC7DCDCB, $6B0C7BB6, 
  $E42AB564, $18B305BB, $BFE12D1B, $DD177FCB, $D53F60E0, $670DB49D, 
  $4FEA393D, $867C255E, $7B984B00, $A6C1D4DA, $54562FAA, $4306242B, 
  $49A96DA5, $EA5F49A9, $CD7D76DE, $74FED359, $11F1812F, $DE1E8A0E, 
  $3A06FAE9, $D2C55773, $C0D97230, $2FEFD61C, $6D1F004C, $3959B11C, 
  $9B11B7B4, $F81FC394, $E4B6C820, $932E23F0, $8BD5F0D8, $3976B323, 
  $C393E13F, $ED872E2F, $D8F03C79, $3F9E1C62, $2ED9F0E9, $78716760, 
  $AEB77AED, $C38A9B20, $7C46A5BF, $29B16454, $C79FED3B, $9EF0E236, 
  $775272A3, $A0D1D466, $3E7B4E3B, $0E20FF63, $0AB038AB, $66739DDE, 
  $37523322, $E668A53B, $DDAF9D27, $29FC6C27, $79B9C94D, $252B02F3, 
  $5FBAC7BB, $B9782072, $05E62ADC, $7DCA46F2, $7DCC005C, $CF0397DD, 
  $678DF8D5, $FACB7E17, $1F1C7986, $B5116FD5, $EFC35913, $AD58BB29, 
  $16D3AFEF, $8F76DD3B, $E8DB0BD8, $324DC9BC, $7F46C927, $CCE16CCD, 
  $5A611576, $048850BF, $69519E5F, $273DF7D9, $61FD26E0, $36A4F253, 
  $FA9774CB, $3EDBD22C, $BD3B5381, $7BF90882, $387F7BEA, $EF5766FA, 
  $E53BC70F, $9B020CBC, $DEC3D11F, $AF45FA0A, $7059F5C0, $441B74EF, 
  $B3E3E2D5, $606BEED4, $5215825F, $8C14C6FD, $EF9B115E, $A66567AB, 
  $B694867D, $0DE533AF, $32F65DF7, $A2D5B5E3, $35E6967E, $CA012769, 
  $A5142327, $DCBD502E, $81170436, $B470EB51, $908CAC79, $582DEA67, 
  $B8D4887F, $931F833E, $92B13616, $3726D363, $BC8C8A9E, $6A76E039, 
  $F59F553B, $ECA9F771, $18AED3B8, $6E870A62, $9634CB37, $D36D1713, 
  $75FA3B28, $E1A716FB, $3AC7ECB3, $95FE4CF8, $31356A82, $3EEFCC76, 
  $F1DA9A83, $A3B383B4, $469042F6, $7FEEF06D);
 
const
 IdentHashes: packed array [0..223] of Cardinal = (
  $089283C3, $0088C942, $036AD332, $0E738A23, $005E73D4, $0672E1B5, 
  $09357194, $0376790C, $0791336C, $0760650E, $01855A74, $03DC8AF5, 
  $0941A355, $0EFB61ED, $0F84E162, $005E942F, $034EDD4C, $005E943B, 
  $0946AC99, $05E94DA3, $0AD7A2DC, $00000000, $00000000, $08B69AE7, 
  $0365D6D3, $0365D7BC, $07407955, $071694A4, $0CAD0862, $0E044452, 
  $01545252, $08EA10C4, $0EABF705, $0DB1E785, $0E155FF7, $00314F02, 
  $03166494, $08403717, $0B39F2F2, $06DBD7D9, $06DB39F3, $05A2A167, 
  $0DD1C23C, $02B8FE04, $0F940C05, $01B17594, $08EACA74, $0EA65C05, 
  $0B897E7E, $0AADD398, $03AC24E4, $024EB0B2, $02B22DA3, $0CB63F87, 
  $0E2ADC0B, $06DCAD40, $0D00CEE4, $073538D5, $05714DA2, $0E58F73E, 
  $02063D45, $07A2C6CD, $042ABA85, $0C373952, $00400603, $09416CA5, 
  $0E6BC5C9, $0AFFDF32, $0CBE7D10, $01306132, $03CE66A2, $08ED7AC4, 
  $0EDD5705, $0839A8F5, $033BFE14, $0F68AAC2, $0AE9E50E, $0D43CBD2, 
  $0D4B7875, $0B433EEC, $0DC33D64, $0F22950E, $095B8755, $0F322BD2, 
  $04338A8E, $096A7284, $039C1A35, $0D3D5ACD, $0B43447D, $0E3DFC54, 
  $0143EF45, $02CA281D, $013F2FC5, $009839C3, $0FCAC804, $022E7EE4, 
  $01517A07, $0D76538E, $01F4C225, $08909AD5, $04D5FE14, $06FDEAEC, 
  $076FDEAF, $06FD9C90, $0DDAC99E, $0CB9AA10, $050EAE25, $034985C5, 
  $0E388DDE, $0DCD41E4, $0EA437BC, $002E73CC, $052E1132, $006131D2, 
  $0A237212, $0C12E1A5, $0C1132D2, $0CE0A192, $01996C73, $03FE730E, 
  $04D215D3, $08ED12C4, $01A1EDB2, $0D637244, $0BDA3FA4, $01B15BA7, 
  $07701863, $03797043, $03797F13, $0CABD664, $0A2BDFFE, $0E1E45D4, 
  $0E5792A5, $062EB1F2, $01465B48, $0AFF8364, $06F996C2, $0CB46192, 
  $0EB4F082, $09357184, $0590BF0B, $0CF3DD35, $0F9C29A5, $067813E4, 
  $0847575D, $0EBBD67C, $0A551FA7, $070A6184, $0C4A4CE4, $093A6385, 
  $0A638517, $0D38349B, $0E2C9795, $08A248A5, $05EB6D5B, $0A513DF5, 
  $09E77938, $07FBADD4, $08A7A7A0, $0BB12FAE, $02F463A4, $00B68271, 
  $00B6AAF5, $028E4395, $0B738E9D, $078D99ED, $0A613FCE, $00000000, 
  $00000000, $00000000, $00000000, $00000000, $06623895, $08DCEE15, 
  $05F647B8, $03C907C2, $03AE7118, $08402C17, $06613F42, $0CC89F57, 
  $01307C67, $0F39DC17, $051FC677, $0C6B7C67, $056C8443, $08CC2B47, 
  $01A34C67, $0EA3C464, $079447F4, $0A357194, $066EC60B, $041A5AD9, 
  $04892724, $0A3A74A2, $030EC7DC, $0E18249C, $0FCC70D0, $0947A33C, 
  $0EE1D476, $0EE1D464, $0E98AA8E, $01E8EF69, $0DD10B64, $037B4972, 
  $00067BFF, $0E6E9FD3, $09ED8754, $04A7201E, $0AE7CE14, $0021DC65, 
  $0E761A25, $0AE7DEDB, $007574C0, $0CC1C7E5, $0BA74C6C, $0EA41ED3, 
  $0B64C867, $002F4065, $0084A8B4, $0FBBEA57, $04132134, $038A7334, 
  $09A2D025, $07FBBED4);
 
procedure ReadCompressedStrings(si: TMemoryStream);
begin
  si.WriteBuffer(CompBlok, SizeOf(CompBlok));
end;
 
function FindIndentifier(const Ident: string): integer;
var
  Hash: Cardinal;
begin
  Hash := HashFuncNoCase(Ident);
  for Result := 0 to High(IdentHashes) do
    if IdentHashes[Result] = Hash then
      Exit;
  Result := -1;
end;
 
end.
