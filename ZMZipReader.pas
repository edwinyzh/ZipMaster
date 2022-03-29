unit ZMZipReader;

// ZMZipReader.pas - Loads a Zip file

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
// modified 2014-01-03

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Classes, Windows,
{$ENDIF}
  ZipMstr, ZMBody, ZMZipDirectory, ZMZipEOC, ZMStructs;

type
  TZMZipReader = class(TZMZipDirectory)
  private
    FEOCFileTime: TFileTime;
    function LoadTheZip: Integer;
    function LoadSFXStub(const ZipName: string): Integer;
    function Open1(EOConly: Boolean): Integer;
    function OpenLast(EOConly: Boolean; OpenRes: Integer): Integer;
    function OpenLastExt(FName: string; EOConly: Boolean): Integer;
    function OpenLastFind(var FMVolume: Boolean; const Fname, Path: string;
      var TmpNumbering: TZipNumberScheme; EOConly: Boolean): Integer;
    function OpenLastFixed(const BaseName: string; var PartNbr: Integer; EOConly:
        Boolean): Integer;
    function OpenLastPart(const BaseName, Path: string): Integer;
    function OpenLastVerify(PartNbr: Integer): Integer;
    function VerifyPossible(const FN: string): Integer;
  public
    function OpenZip(EOConly, NoLoad: Boolean): Integer;
    function PossibleBase(var FName: string; const RequestedName: string): Integer;
    function VerifyOpen: Integer;
    property EOCFileTime: TFileTime read FEOCFileTime write FEOCFileTime;
  end;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils, {$IFNDEF UNICODE}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMCore, ZMZipBase, ZMMsg, ZMXcpt, ZMUtils, ZMMatch, ZMWinFuncs,
  ZMEntryReader, ZMUTF8, ZMCRC;

const
  __UNIT__ = 46;

const
  HTChainsMax = 65537;
  HTChainsMin = 61;

const
  AllSpec: string = '*.*';
  AnySpec: string = '*';

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

function FindAnyPart(const BaseName: string; Compat: Boolean): string;
var
  Fs: string;
  N: Integer;
  NN: string;
  R: Integer;
  SRec: _Z_TSearchRec;
begin
  Result := '';
  if Compat then
    Fs := BaseName + '.z??*'
  else
    Fs := BaseName + '???.zip';
  R := _Z_FindFirst(Fs, FaAnyFile, SRec);
  while R = 0 do
  begin
    if Compat then
    begin
      Fs := UpperCase(Copy(ExtractFileExt(SRec.Name), 3, 20));
      if Fs = 'IP' then
        NN := '99999'
      else
        NN := Fs;
    end
    else
      NN := Copy(SRec.Name, Length(SRec.Name) - 6, 3);
    N := StrToIntDef(NN, 0);
    if N > 0 then
    begin
      Result := SRec.Name; // possible name
      Break;
    end;
    R := _Z_FindNext(SRec);
  end;
  _Z_FindClose(SRec);
end;

function TZMZipReader.LoadTheZip: Integer;
begin
  LastWriteTime(FEOCFileTime);
  InferNumbering;
  Info := Info or Zfi_EOC;
  Result := LoadZip;
  if Result = 0 then
  begin
    Info := Info or Zfi_Loaded or Zfi_DidLoad;
    SaveFileInformation; // get details
  end;
end;

function TZMZipReader.LoadSFXStub(const ZipName: string): Integer;
var
  FN: string;
  Size: Integer;
begin
  Stub := nil;
  FN := ZipName;
  Result := CheckSFXType(Stream, FN, Size);
  if Result >= CstSFX17 then
  begin
    if Seek(0, SoBeginning) <> 0 then
      Exit;
    Stub := TMemoryStream.Create;
    try
      if ReadTo(Stub, Size) <> Size then
        Stub := nil;
    except
      Stub := nil;
    end;
  end;
end;

function TZMZipReader.OpenZip(EOConly, NoLoad: Boolean): Integer;
var
  FN: string;
  Possible: Integer;
  R: Integer;
begin
    // verify disk loaded
  ClearFileInformation;
  Possible := 0;
  Info := (Info and Zfi_MakeMask) or Zfi_Loading;
  if IsExtStream or WorkDrive.DriveIsFixed or WorkDrive.HasMedia(False) then
  begin
    if IsExtStream then
      FN := '<stream>'
    else
      FN := ArchiveName;
    Body.TraceFmt('Opening zip %s', [FN], {_LINE}1050, __UNIT__);
    Result := Open1(EOConly);
    if (Result >= 0) and (TotalDisks > 1) and not EOConly then
    begin
      MultiDisk := True;
      if Unattended and WorkDrive.DriveIsFloppy and
        not Assigned(Master.OnGetNextDisk) then
        Result := ZM_Error({_LINE_}281, ZE_NoUnattSpan)
      else
      if not IsExtStream then
      begin
        IsMultiPart := True;
        Body.TraceFmt('Opened multi-part zip %s', [FN], {_LINE}1065, __UNIT__);
        if Numbering = ZnsNone then
        begin
          // numbering is not known yet
          FN := ChangeFileExt(ArchiveName, '');
          Possible := PossibleBase(FN, ArchiveName);
          if (Possible > 0) and (Possible <> (DiskNr + 1)) then
            Possible := 0;
          if Possible > 0 then
            Possible := VerifyPossible(FN);
        end;
      end;
    end;
    if (Result >= 0) and not(EOConly or NoLoad) then
    begin
      if (Result and EOCBadComment) <> 0 then
        ShowErrorEx(ZW_EOCCommentLen, {_LINE_}225, __UNIT__);
      if (Result and EOCBadStruct) <> 0 then
        ShowErrorEx(ZW_WrongZipStruct, {_LINE_}227, __UNIT__);
      if IsExtStream and (TotalDisks > 1) then
        raise EZipMaster.CreateMsg(Body, ZE_StreamNoSupport, {_LINE_}231, __UNIT__);
      R := LoadTheZip;
      if R < 0 then
        Result := R;
    end;
    // adjust name
    if (Result >= 0) and (Possible <> 0) then
    begin
      ArchiveName := FN + '.ZIP';
      ReqFileName := ArchiveName;
      Body.TraceFmt('Using base name of multi-part zip %s', [ArchiveName],
        {_LINE}1112, __UNIT__);
    end;
  end
  else
    Result := -ZE_NoInFile;
  OpenRet := Result;
  if IsTrace then
  begin
    if Result < 0 then
      Body.TraceFmt('Open = %s', [ZipLoadStr(Result)], {_LINE_}321, __UNIT__)
    else
      Body.TraceFmt('Open = %d', [Result], {_LINE}1076, __UNIT__);
  end;
end;

function TZMZipReader.Open1(EOConly: Boolean): Integer;
var
  FN: string;
  Res: Integer;
  SfxType: Integer;
begin
  SfxType := 0; // keep compiler happy
  ReqFileName := ArchiveName;
  FN := ArchiveName;
  Result := OpenEOC(FN, EOConly);
  if (Result >= 0) and (Sig = ZfsDOS) then
  begin
    // found sfx eoc
    SfxType := LoadSFXStub(FN);
  end;
  if IsExtStream then
    Exit; // must have eoc
  // wasn't found or looking for 'detached' last part
  if not(SpExactName in Span.Options) then
  begin
    if (Result >= 0) and (SfxType >= CstDetached) then
    begin // it is last part of detached sfx
      File_Close;
      // Get proper path and name
      ArchiveName := ChangeFileExt(FN, '.zip'); // find last part
      Result := -ZE_NoInFile;
    end;
    if Result < 0 then
    begin
      Res{ult} := OpenLast(EOConly, Result);
      if Res >= 0 then
        Result := Res; // ignore OpenLast errors
    end;
  end;
end;

// GetLastVolume
function TZMZipReader.OpenLast(EOConly: Boolean; OpenRes: Integer): Integer;
var
  BaseName: string;
  Ext: string;
  FMVolume: Boolean;
  OrigName: string;
  PartNbr: Integer;
  Path: string;
  Possible: Integer;
  TmpNumbering: TZipNumberScheme;
  WasNoFile: Boolean;
begin
  Result := AbsErr(OpenRes);
  WasNoFile := Result = ZE_NoInFile;
  PartNbr := -1;
  Result := 0;
  Possible := 0;
  FMVolume := False;
  OrigName := ArchiveName; // save it
  WorkDrive.DriveStr := ArchiveName;
  Path := ExtractFilePath(ArchiveName);
  Numbering := ZnsNone; // unknown as yet
  TmpNumbering := ZnsNone;
  try
    WorkDrive.HasMedia(False); // check valid drive
    if WasNoFile then
      BaseName := ChangeFileExt(ArchiveName, '') // remove extension
    else
    begin
      // file exists but is not last part of zip
      Possible := PossibleBase(BaseName, ArchiveName);
      if Possible < 0 then
        Result := OpenLastExt(BaseName + '.zip', EOConly)
      else
      if Possible > 0 then
        WasNoFile := True;
    end;
    if WasNoFile then
    begin
      // file did not exist maybe it is a multi volume
      FMVolume := True;
      Ext := ExtractFileExt(ArchiveName);
      if CompareText(Ext, EXT_ZIP) = 0 then
      begin
        // get the 'base' name for numbered names
        // remove extension
        // if no file exists with exact name on harddisk then only Multi volume parts are possible
        if WorkDrive.DriveIsFixed then
          // filename is of type ArchiveXXX.zip
          // MV files are series with consecutive partnbrs in filename,
          // highest number has EOC
          Result := OpenLastFixed(BaseName, PartNbr, EOConly)
        else
          // do we have an MV archive copied to a removable disk
          // accept any MV filename on disk - then we ask for last part
          Result := OpenLastPart(BaseName, Path);
      end//;
      else
        Result := OpenRes;
    end; // if not exists
    if Result >= 0 then
    begin
      // zip file exists or we got an acceptable part in multivolume or split
      // archive
      // use class variable for other functions
      Result := OpenLastFind(FMVolume, BaseName, Path, TmpNumbering, EOConly);
      if FMVolume then
        // got a multi volume part so we need more checks
        Result := OpenLastVerify(PartNbr);
    end;
  finally
    if Result < 0 then
    begin
      File_Close; // close filehandle if OpenLast
      ArchiveName := ''; // don't use the file
    end
    else
      if (Numbering <> ZnsVolume) and (TmpNumbering <> ZnsNone) then
        Numbering := TmpNumbering;
  end;
  if (Result >= 0) and (Possible <> 0) then
  begin
    ArchiveName := BaseName + '.ZIP';
    ReqFileName := ArchiveName;
    Body.TraceFmt('Using base name of multi-part zip %s', [ArchiveName],
      {_LINE}1307, __UNIT__);
  end;
end;

function TZMZipReader.OpenLastExt(FName: string; EOConly: Boolean): Integer;
var
  StampLast: Integer;
  StampPart: Integer;
begin
  StampPart := Integer(File_Age(ArchiveName));
  StampLast := Integer(File_Age(FName));
  if (StampPart = -1) or (StampLast = -1) or
    ((StampPart <> StampLast) and not(SpAnyTime in Span.Options)) then
  begin
    // not found or stamp does not match
    Result := ZM_Error({_LINE_}459, ZE_NoInFile);
    Exit;
  end;
  Result := OpenEOC(FName, EOConly);
  if Result >= 0 then
  begin // found possible last part
    Numbering := ZnsExt;
  end;
end;

function TZMZipReader.OpenLastFind(var FMVolume: Boolean;
  const Fname, Path: string; var TmpNumbering: TZipNumberScheme;
  EOConly: Boolean): Integer;
var
  SName: string;
begin
  Result := 0;
  // zip file exists or we got an acceptable part in multivolume or split
  // archive
  // use class variable for other functions
  while not IsOpen do // only open if found last part on hd
  begin
    // does this part contains the central dir
    Result := OpenEOC(ArchiveName, EOConly); // don't LoadZip on success
    if Result >= 0 then
      Break; // found a 'last' disk
    if WorkDrive.DriveIsFixed then
    begin
      if (not FMVolume) and (AbsErr(Result) <> ZE_FileOpen) then
        Result := ZM_Error({_LINE_}488, ZE_NoValidZip);
      Break; // file with EOC is not on fixed disk
    end;
    // it is not the disk with central dir so ask for the last disk
    NewDisk := True; // new last disk
    DiskNr := -1; // read operation
    CheckForDisk(False, False); // does the request for new disk
    if FMVolume and not FileExists(ArchiveName) then
    begin // we have removable disks with multi volume archives
      // get the file name on this disk
      TmpNumbering := ZnsName; // only if part and last part inserted
      SName := FindAnyPart(Fname, False);
      if SName = '' then
      begin
        SName := FindAnyPart(Fname, True);
        TmpNumbering := ZnsExt; // only if last part inserted
      end;
      if SName = '' then // none
      begin
        Result := ZM_Error({_LINE_}507, ZE_NoInFile);
        // no file with likely name
        FMVolume := False;
        Break;
      end;
      ArchiveName := Path + SName;
    end;
  end; // while;
end;

function TZMZipReader.OpenLastFixed(const BaseName: string; var PartNbr:
    Integer; EOConly: Boolean): Integer;
var
  Finding: Boolean;
  S: string;
  Stamp: Integer;
  StampTmp: Integer;
begin
  Result := 0; // default failure
  // filename is of type ArchiveXXX.zip
  // MV files are series with consecutive partnbrs in filename,
  // highest number has EOC
  Finding := True;
  Stamp := -1;
  while Finding and (PartNbr < 1000) do
  begin
    if KeepAlive then
    begin
      Body.Trace('OpenLast - user abort', {_LINE_}535, __UNIT__);
      Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
        {_LINE_}537, __UNIT__);
      Exit; // cancelled
    end;
    // add part number and extension to base name
    S := BaseName + Copy(IntToStr(1002 + PartNbr), 2, 3) + EXT_ZIPL;
    StampTmp := Integer(File_Age(S));
    if (StampTmp = -1) or ((Stamp <> -1) and (StampTmp <> Stamp)) then
    begin
      // not found or stamp does not match
      Result := ZM_Error({_LINE_}546, ZE_NoInFile);
      Exit;
    end;
    if (PartNbr = -1) and not(SpAnyTime in Span.Options) then
      Stamp := StampTmp;
    Inc(PartNbr);
    ArchiveName := S;
    Result := OpenEOC(ArchiveName, EOConly);
    if Result >= 0 then
    begin // found possible last part
      Finding := False;
      if (TotalDisks - 1) <> PartNbr then
      begin
        // was not last disk
        File_Close; // should happen in 'finally'
        Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
          {_LINE_}562, __UNIT__);
        Exit;
      end;
      Numbering := ZnsName;
    end;
  end; // while
  if not IsOpen then
    Result := ZM_Error({_LINE_}569, ZE_NoInFile) // not found
  else
  begin
    // should be the same as s
    ArchiveName := BaseName + Copy(IntToStr(1001 + PartNbr), 2, 3) + EXT_ZIPL;
    // check if filename.z01 exists then it is part of MV with compat names
    // and cannot be used
    if (FileExists(ChangeFileExt(ArchiveName, '.z01'))) then
    begin
      // ambiguous - cannot be used
      File_Close; // should happen in 'finally'
      Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
        {_LINE_}581, __UNIT__);
    end;
  end;
end;

function TZMZipReader.OpenLastPart(const BaseName, Path: string): Integer;
var
  SName: string;
begin
  Result := 0;
  // do we have an MV archive copied to a removable disk
  // accept any MV filename on disk - then we ask for last part
  SName := FindAnyPart(BaseName, False);  // try numbered name
  if SName = '' then
    SName := FindAnyPart(BaseName, True); // try numbered extension
  if SName = '' then // none
    Result := ZM_Error({_LINE_}597, ZE_NoInFile) // no file with likely name
  else
    ArchiveName := Path + SName;
end;

function TZMZipReader.OpenLastVerify(PartNbr: Integer): Integer;
begin // is this first file of a multi-part
  Result := 0;
  if (Sig <> ZfsMulti) and ((TotalDisks = 1) and (PartNbr >= 0)) then
    Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
      {_LINE_}607, __UNIT__)
  else
    // part and EOC equal?
    if WorkDrive.DriveIsFixed and (TotalDisks <> (PartNbr + 1)) then
    begin
      File_Close; // should happen in 'finally'
      Result := ZM_Error({_LINE_}613, ZE_NoValidZip);
    end;
end;

// returns 0 _ not valid, <0 _ numbered extension, >0 _ numbered name
function TZMZipReader.PossibleBase(var FName: string; const RequestedName:
    string): Integer;
const
  Digits = ['0'..'9'];
var
  Extn: string;
  FileName: string;
  I: Integer;
  Len: Integer;
  N: Integer;
  NNN: string;
begin
  Result := 0;
  Extn := UpperCase(ExtractFileExt(Name));
  if (Length(Extn) <> 4) or (Extn[2] <> 'Z') then
    Exit;
  if CharInSet(Extn[3], Digits) and CharInSet(Extn[4], Digits) then
    Result := -1
  else
  if (Extn[3] = 'I') and (Extn[4] = 'P') then
  begin
    for I := Length(RequestedName) - 4 downto Length(RequestedName) - 6 do
      if (I < 2) or not CharInSet(RequestedName[I], Digits) then
        Exit;
    FileName := ExtractNameOfFile(RequestedName);

    if Length(FileName) > 3 then
    begin
      // needs 3 digits
      NNN := Copy(FileName, Length(FileName) - 2, 3);
      if TryStrToInt(NNN, N) then
        Result := N;
    end;
  end;
  if Result <> 0 then
  begin
    Len := Length(RequestedName) - Length(Extn);
    if Result > 0 then
      Len := Len - 3;
    FName := Copy(RequestedName, 1, Len);
  end;
end;

function TZMZipReader.VerifyOpen: Integer;
var
  Ft: TFileTime;
begin
  Result := 0;
  if not IsOpen and not File_Open('', FmOpenRead or FmShareDenyWrite) then
  begin
    Result := Body.PrepareErrMsg(ZE_FileOpen, [ArchiveName],
      {_LINE_}669, __UNIT__);
    Exit;
  end;
  if LastWriteTime(Ft) then
  begin
    LastWriteTime(FEOCFileTime);
    if CompareFileTime(EOCFileTime, Ft) <> 0 then
      Result := Body.PrepareErrMsg(ZE_FileChanged, [Name],
        {_LINE_}677, __UNIT__);
  end;
end;

function TZMZipReader.VerifyPossible(const FN: string): Integer;
var
  Stamp: Cardinal;
begin
  Result := 0;
  Stamp := File_Age(ChangeFileExt(ArchiveName, '.z01'));
  if Stamp <> Cardinal(-1) then
    Exit;  // ambiguous - named extension exists
  Stamp := File_Age(FN + '001.zip');
  if Stamp = Cardinal(-1) then
    Exit;  // first part not found
  if (not(SpAnyTime in Span.Options)) or (File_Age(ArchiveName) = Stamp) then
      Result := 1;
end;

end.
