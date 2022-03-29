#ifndef ZipMsgH
  #define ZipMsgH
/* **************************************************
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
************************************************** */
//Generated 2014-02-19
 
// note special comments //! shared, //- discard, {//= moved}
 
  #define ZS_Success               0
  #define ZE_UnknownError          1
  #define ZA_Author                2
  #define ZA_Desc                  3
  #define ZA_ID                    4
  #define ZA_Language              5
  #define ZC_Abort                 6 // =PW_Abort
  #define ZC_Cancel                7 // =PW_Cancel
  #define ZC_CancelAll             8 // =PW_CancelAll
  #define ZC_Caption               9
  #define ZC_FileConflict          10
  #define ZC_Ignore                11
  #define ZC_Merge                 12
  #define ZC_MessageConfirm        13
  #define ZC_MessageEnter          14
  #define ZC_No                    15
  #define ZC_NoToAll               16
  #define ZC_OK                    17 // =PW_Ok
  #define ZC_Retry                 18
  #define ZC_Yes                   19
  #define ZC_YesToAll              20
  #define ZE_AutoSFXWrong          23
  #define ZE_BadCRC                24
  #define ZE_BadDll                25
  #define ZE_BadFileName           26
  #define ZE_Blocked               27
  #define ZE_BrowseError           28
  #define ZE_BuildBaseError        29 //+
  #define ZE_BuildPathError        30
  #define ZE_CEHBadRead            31
  #define ZE_CEHBadWrite           32
  #define ZE_CEHDataSize           33
  #define ZE_CEHWrongSig           34
  #define ZE_CopyError             35
  #define ZE_CopyFailed            36
  #define ZE_Copying               37
  #define ZE_CryptError            38
  #define ZE_DataCopy              39
  #define ZE_DataDesc              40
  #define ZE_DetachedHeaderTooBig  41
  #define ZE_DLLCritical           42
  #define ZE_DriveNoMount          43
  #define ZE_DuplFileName          44
  #define ZE_EntryCancelled        45 //+
  #define ZE_EOCBadRead            46
  #define ZE_EOCBadWrite           47
  #define ZE_ErrorUnknown          48
  #define ZE_EventEx               49
  #define ZE_Except                50
  #define ZE_ExceptErr             51
  #define ZE_ExeSections           52
  #define ZE_Existing              53 //+
  #define ZE_FailedSeek            54
  #define ZE_FatalZip              55
  #define ZE_FileChanged           56
  #define ZE_FileCreate            57
  #define ZE_FileError             58
  #define ZE_FileOpen              59
  #define ZE_Inactive              60
  #define ZE_InIsOutStream         61
  #define ZE_InputNotExe           62
  #define ZE_InternalError         63
  #define ZE_InvalidArguments      64
  #define ZE_InvalidDateTime       65
  #define ZE_InvalidEntry          66
  #define ZE_InvalidParameter      67
  #define ZE_InvalidZip            68
  #define ZE_LoadErr               69
  #define ZE_LogicError            70
  #define ZE_LOHBadRead            71
  #define ZE_LOHBadWrite           72
  #define ZE_LOHWrongName          73
  #define ZE_NoAppend              74
  #define ZE_NoChangeDir           75
  #define ZE_NoCopyIcon            76
  #define ZE_NoDestDir             77
  #define ZE_NoDiskSpace           78
  #define ZE_NoDll                 79
  #define ZE_NoEncrypt             80
  #define ZE_NoExeIcon             81
  #define ZE_NoExeResource         82
  #define ZE_NoExtrDir             83
  #define ZE_NoIcon                84
  #define ZE_NoIconFound           85
  #define ZE_NoInFile              86
  #define ZE_NoInStream            87
  #define ZE_NoMem                 88
  #define ZE_NoneSelected          89
  #define ZE_NoOutFile             90
  #define ZE_NoOutStream           91
  #define ZE_NoOverwrite           92 //+
  #define ZE_NoProcess             93
  #define ZE_NoProtected           94
  #define ZE_NoRenamePart          95
  #define ZE_NoSkipping            96
  #define ZE_NoStreamSpan          97
  #define ZE_NotChangeable         98
  #define ZE_NoTempFile            99
  #define ZE_NotFound              100 //+
  #define ZE_NothingToDel          101
  #define ZE_NothingToDo           102
  #define ZE_NothingToZip          103
  #define ZE_NoUnattSpan           104
  #define ZE_NoValidZip            105
  #define ZE_NoVolume              106
  #define ZE_NoWrite               107
  #define ZE_NoZipSFXBin           108
  #define ZE_NoZipSpecified        109
  #define ZE_PasswordCancel        110
  #define ZE_PasswordFail          111
  #define ZE_RangeError            112
  #define ZE_ReadError             113
  #define ZE_ReadZipError          114
  #define ZE_SameAsSource          115
  #define ZE_SeekError             116
  #define ZE_SetDateError          117
  #define ZE_SetFileAttributes     118
  #define ZE_SetFileInformation    119 //+
  #define ZE_SetFileTimes          120
  #define ZE_SFXBadRead            121
  #define ZE_SFXCopyError          122
  #define ZE_SourceIsDest          123
  #define ZE_StreamNoSupport       124
  #define ZE_StringTooLong         125
  #define ZE_TooManyParts          126
  #define ZE_UnatAddPWMiss         127 // =ZS_UnatAddPWMiss
  #define ZE_UnatExtPWMiss         128 // =ZS_UnatExtPWMiss
  #define ZE_UnattPassword         129
  #define ZE_Unknown               130
  #define ZE_Unsupported           131
  #define ZE_WildName              132
  #define ZE_WriteError            133
  #define ZE_WrongLength           134
  #define ZE_WrongPassword         135
  #define ZE_Zip64FieldError       136
  #define ZE_ZipDataError          137
  #define ZE_ZLib                  138
  #define ZS_Abort                 139
  #define ZS_AnotherDisk           140
  #define ZS_AskDeleteFile         141
  #define ZS_AskPrevFile           142
  #define ZS_Canceled              143
  #define ZS_Confirm               144
  #define ZS_CopyCentral           145
  #define ZS_Deleting              146
  #define ZS_DllLoaded             147
  #define ZS_DllUnloaded           148
  #define ZS_Erase                 149
  #define ZS_Erasing               150
  #define ZS_GetNewDisk            151
  #define ZS_InDrive               152
  #define ZS_InsertAVolume         153
  #define ZS_InsertDisk            154
  #define ZS_InsertVolume          155
  #define ZS_InvalidPath           156
  #define ZS_Skipped               157
  #define ZS_TempZip               158
  #define ZW_EOCCommentLen         159
  #define ZW_WrongZipStruct        160
  #define ZZ_ZLibData              161
  #define ZZ_ZLibFile              162
  #define ZZ_ZLibIncompatible      163
  #define ZZ_ZLibNoMem             164
  #define ZZ_ZLibStream            165
  #define ZZ_ZLibUnknown           166
  #define ZP_Archive               172
  #define ZP_CopyZipFile           173
  #define ZP_SFX                   174
  #define ZP_Header                175
  #define ZP_Finish                176
  #define ZP_Copying               177
  #define ZP_CentrlDir             178
  #define ZP_Checking              179
  #define ZP_Loading               180
  #define ZP_Joining               181
  #define ZP_Splitting             182
  #define ZP_Writing               183
  #define ZP_PreCalc               184
  #define ZP_Processing            185
  #define ZP_Merging               186
  #define ZD_GOOD                  187 // ZEN_OK
  #define ZD_CANCELLED             188
  #define ZD_ABORT                 189
  #define ZD_CALLBACK              190
  #define ZD_MEMORY                191
  #define ZD_STRUCT                192
  #define ZD_ERROR                 193
  #define ZD_PASSWORD_FAIL         194
  #define ZD_PASSWORD_CANCEL       195
  #define ZD_INVAL_ZIP             196 // ZEN_FORM
  #define ZD_NO_CENTRAL            197 // UEN_EOF01
  #define ZD_ZIP_EOF               198 // ZEN_EOF
  #define ZD_ZIP_END               199 // UEN_EOF02
  #define ZD_ZIP_NOOPEN            200
  #define ZD_ZIP_MULTI             201
  #define ZD_NOT_FOUND             202
  #define ZD_LOGIC_ERROR           203 // ZEN_LOGIC
  #define ZD_NOTHING_TO_DO         204 // ZEN_NONE
  #define ZD_BAD_OPTIONS           205 // ZEN_PARM
  #define ZD_TEMP_FAILED           206 // ZEN_TEMP
  #define ZD_NO_FILE_OPEN          207 // ZEN_OPEN
  #define ZD_ERROR_READ            208 // ZEN_READ
  #define ZD_ERROR_CREATE          209 // ZEN_CREAT
  #define ZD_ERROR_WRITE           210 // ZEN_WRITE
  #define ZD_ERROR_SEEK            211
  #define ZD_EMPTY_ZIP             212
  #define ZD_INVAL_NAME            213
  #define ZD_GENERAL               214
  #define ZD_MISS                  215 // ZEN_MISS UEN_MISC03
  #define ZD_WARNING               216 // PK_WARN
  #define ZD_ERROR_DELETE          217 // PK_NODEL
  #define ZD_FATAL_IMPORT          218
  #define ZD_SKIPPING              219
  #define ZD_LOCKED                220
  #define ZD_DENIED                221
  #define ZD_DUPNAME               222
  #define ZD_SKIPPED               223
 
  #define ZX_Last                  223
 
#endif
