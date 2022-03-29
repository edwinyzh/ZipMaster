unit ZMZipEOC;

// ZMZipEOC.pas - EOC handling

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
// modified 2013-12-27

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VER180}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows, System.SysUtils,
{$ELSE}
  Classes, Windows, SysUtils, {$IFNDEF VERD7up}ZMCompat, {$ENDIF}
{$ENDIF}
  ZMZipBase, ZMZipMulti, ZMStructs;

type
  TZMZipEOC = class(TZMZipMulti)
  private
    FCentralDiskNo: Integer;
    FCentralEntries: Cardinal;
    FCentralOffset: Int64;
    FCentralSize: Int64;
    FEOCOffset: Int64;
    FMultiDisk: Boolean;
    FOffsetDelta: Int64;
    FTotalEntries: Cardinal;
    FVersionMadeBy: Word;
    FVersionNeeded: Word;
    FZ64: Boolean;
    FZ64VSize: Int64;
    FZipComment: AnsiString;
    function CheckSignature: Integer;
    function GetEOC64(Ret: Integer): Integer;
    function GetZipCommentLen: Integer;
    function FindEOC: Integer;
    function IsLocator(Locp: PZip64EOCLocator): Boolean;
    procedure SetZipComment(const Value: AnsiString);
    procedure SetZipCommentLen(const Value: Integer);
  public
    procedure AfterConstruction; override;
    function OpenEOC(const FileName: string; EOConly: Boolean): Integer;
    function WriteEOC: Integer;
    property CentralDiskNo: Integer read FCentralDiskNo write FCentralDiskNo;
    property CentralEntries: Cardinal read FCentralEntries
      write FCentralEntries;
    property CentralOffset: Int64 read FCentralOffset write FCentralOffset;
    property CentralSize: Int64 read FCentralSize write FCentralSize;
    property EOCOffset: Int64 read FEOCOffset write FEOCOffset;
    property MultiDisk: Boolean read FMultiDisk write FMultiDisk;
    property OffsetDelta: Int64 read FOffsetDelta write FOffsetDelta;
    property TotalEntries: Cardinal read FTotalEntries write FTotalEntries;
    property VersionMadeBy: Word read FVersionMadeBy write FVersionMadeBy;
    property VersionNeeded: Word read FVersionNeeded write FVersionNeeded;
    property Z64: Boolean read FZ64 write FZ64;
    property Z64VSize: Int64 read FZ64VSize write FZ64VSize;
    property ZipComment: AnsiString read FZipComment write SetZipComment;
    property ZipCommentLen: Integer read GetZipCommentLen
      write SetZipCommentLen;
  end;

const
  Zfi_EOC: Cardinal = $800; // valid EOC found

const
  EOCBadStruct = 2;
  EOCBadComment = 1;
  EOCWant64 = 64;

implementation

uses
  ZipMstr, ZMBody, ZMXcpt, ZMMsg, ZMUtils;

const
  __UNIT__ = 44;

const
  EOCSigFound = 8;

function ZM_Error(Line, Error: Integer): Integer;
begin
  Result := -((__UNIT__ shl ZERR_UNIT_SHIFTS) + (Line shl ZERR_LINE_SHIFTS) or
    AbsErr(Error));
end;

procedure TZMZipEOC.AfterConstruction;
begin
  inherited;
  FMultiDisk := False;
  FEOCOffset := 0;
  FZipComment := '';
end;

function TZMZipEOC.CheckSignature: Integer;
var
  Sg: Cardinal;
begin
//  Result := 0;
  // First a check for the first disk of a spanned archive,
  // could also be the last so we don't issue a warning yet.
  Result := Seek(0, SoBeginning);
  if Result < 0 then
  begin
    Result := ZM_Error({_LINE_}153, ZE_FailedSeek);
    Exit;
  end;
  Sig := ZfsNone;
  if read(Sg, 4) <> 4 then
  begin
    Result := ZM_Error({_LINE_}159, ZE_NoValidZip);
    Exit;
  end;
  if (Sg and $FFFF) = IMAGE_DOS_SIGNATURE then
    Sig := ZfsDOS
  else
  if (Sg = LocalFileHeaderSig) then
    Sig := ZfsLocal
  else
  if (Sg = ExtLocalSig) and (read(Sg, 4) = 4) and (Sg = LocalFileHeaderSig)
  then
    Sig := ZfsMulti;
end;

function TZMZipEOC.IsLocator(Locp: PZip64EOCLocator): Boolean;
begin
  Result := False;
  if (Locp^.LocSig <> EOC64LocatorSig) then
    Exit;
  if (DiskNr = MAX_WORD) and (Locp^.NumberDisks < MAX_WORD) then
    Exit;
  Result := True;
end;

function TZMZipEOC.GetEOC64(Ret: Integer): Integer;
var
  CEnd: Int64;
  Eoc64: TZipEOC64;
  Loc: TZip64EOCLocator;
  Posn: Int64;
begin
  Result := Ret;
  if (Result <= 0) or ((Result and EOCWant64) = 0) then
    Exit;
  CEnd := EOCOffset;
  Posn := EOCOffset - Sizeof(TZip64EOCLocator);
  if Posn >= 0 then
  begin
    if Seek(Posn, SoBeginning) < 0 then
    begin
      Result := ZM_Error({_LINE_}213, ZE_FailedSeek);
      Exit;
    end;
    if Read(Loc, Sizeof(TZip64EOCLocator)) <> Sizeof(TZip64EOCLocator) then
    begin
      Result := ZM_Error({_LINE_}218, ZE_EOCBadRead);
      Exit;
    end;
    if (IsLocator(@Loc)) then
    begin
      // locator found
      FZ64 := True; // in theory anyway - if it has locator it must be Z64
      TotalDisks := Loc.NumberDisks;
      DiskNr := Loc.NumberDisks - 1; // is last disk
      if Integer(Loc.EOC64DiskStt) <> DiskNr then
      begin
        Result := ZM_Error({_LINE_}229, ZE_EOCBadRead);
        Exit;
      end;
      if Seek(Loc.EOC64RelOfs, SoBeginning) < 0 then
      begin
        Result := ZM_Error({_LINE_}234, ZE_FailedSeek);
        Exit;
      end;
      if Read(Eoc64, Sizeof(TZipEOC64)) <> Sizeof(TZipEOC64) then
      begin
        Result := ZM_Error({_LINE_}239, ZE_EOCBadRead);
        Exit;
      end;
      if (Eoc64.EOC64Sig = EndCentral64Sig) then
      begin
        // read EOC64
        FVersionNeeded := Eoc64.VersionNeed;
        if ((VersionNeeded and VerMask) > ZIP64_VER) or
          (Eoc64.Vsize < (Sizeof(TZipEOC64) - 12)) then
        begin
          Result := ZM_Error({_LINE_}249, ZE_Unsupported);
          Exit;
        end;
        CEnd := Loc.EOC64RelOfs;
        FVersionMadeBy := Eoc64.VersionMade;
        FZ64VSize := Eoc64.Vsize + 12;
        if CentralDiskNo = MAX_WORD then
        begin
          CentralDiskNo := Eoc64.CentralDiskNo;
          FZ64 := True;
        end;
        if TotalEntries = MAX_WORD then
        begin
          TotalEntries := Cardinal(Eoc64.TotalEntries);
          FZ64 := True;
        end;
        if CentralEntries = MAX_WORD then
        begin
          CentralEntries := Cardinal(Eoc64.CentralEntries);
          FZ64 := True;
        end;
        if CentralSize = MAX_UNSIGNED then
        begin
          CentralSize := Eoc64.CentralSize;
          FZ64 := True;
        end;
        if CentralOffset = MAX_UNSIGNED then
        begin
          CentralOffset := Eoc64.CentralOffset;
          FZ64 := True;
        end;
      end;
    end;
    // check structure
    OffsetDelta := CEnd - CentralSize - CentralOffset;
    if OffsetDelta <> 0 then
      Result := Result or EOCBadStruct;
  end;
end;

function TZMZipEOC.GetZipCommentLen: Integer;
begin
  Result := Length(ZipComment);
end;

(*? TZMZipEOC.OpenEOC
 // Function to find the EOC record at the end of the archive (on the last disk.)
 // We can get a return value or an exception if not found.
 1.73 28 June 2003 RP change handling split files
 return
 <0 - -reason for not finding
 >=0 - found
 Warning values (ored)
 1 - bad comment
 2 - bad structure (Central offset wrong)
*)
function TZMZipEOC.OpenEOC(const FileName: string; EOConly: Boolean): Integer;
var
  Res: Integer;
begin
  try
    if not IsOpen then
      File_Open(FileName, FmOpenRead or FmShareDenyWrite);
    if not IsOpen then
    begin
      if IsExtStream or FileExists(FileName) then
        Result := Body.PrepareErrMsg(ZE_FileOpen, [FileName],
          {_LINE_}316, __UNIT__)
      else
        Result := ZM_Error({_LINE_}318, ZE_NoInFile);
      Exit;
    end;
    Res := 0;
    Result := FindEOC;
    if (Result >= EOCWant64) and not EOConly then
      Result := GetEOC64(Result);
    if Result > 0 then
      Res{ult} := Result and (EOCBadComment or EOCBadStruct);
    if Result >= 0 then
      Result := CheckSignature;
    if Result < 0 then
      File_Close//;
    else
      Result := Res;
  except
    on E: EZipMaster do
    begin
      File_Close;
      Result := E.ExtErr;
    end;
    else
    begin
      File_Close;
      raise;
    end;
  end;
end;

function TZMZipEOC.FindEOC: Integer;
var
  AfterEOC: Integer;
  Clen: Integer;
  FEOC: TZipEndOfCentral;
  I: Integer;
  J: Integer;
  PEOC: PZipEndOfCentral;
  Size: Integer;
  ZipBuf: array of AnsiChar;
begin
  FZipComment := '';
  MultiDisk := False;
  DiskNr := 0;
  TotalDisks := 0;
  TotalEntries := 0;
  CentralEntries := 0;
  CentralDiskNo := 0;
  CentralOffset := 0;
  CentralSize := 0;
  FEOCOffset := 0;
  FVersionMadeBy := 0;
  FVersionNeeded := 0;
  OffsetDelta := 0;
  PEOC := nil;

  Result := 0;
  try
    // Next we do a check at the end of the file to speed things up if
    // there isn't a Zip archive ZipComment.
    File_Size := Seek(-SizeOf(TZipEndOfCentral), SoEnd);
    if File_Size < 0 then
      Result := ZM_Error({_LINE_}380, ZE_NoValidZip)
    else
    begin
      File_Size := File_Size + SizeOf(TZipEndOfCentral);
      // There could follow a correction on FFileSize.
      if Read(FEOC, SizeOf(TZipEndOfCentral)) <> Sizeof(TZipEndOfCentral) then
        Result := ZM_Error({_LINE_}388, ZE_EOCBadRead)
      else
        if (FEOC.HeaderSig = EndCentralDirSig) then
        begin
          FEOCOffset := File_Size - SizeOf(TZipEndOfCentral);
          Result := EOCSigFound{8}; // something found
          if FEOC.ZipCommentLen <> 0 then
          begin
            FEOC.ZipCommentLen := 0; // ??? make safe
            Result := EOCBadComment; // return bad comment
          end;
          PEOC := @FEOC;
        end;
    end;

    if Result = 0 then // did not find it - must have ZipComment
    begin
      Size := 65535 + SizeOf(TZipEndOfCentral);
      if File_Size < Size then
        Size := Integer(File_Size);
      SetLength(ZipBuf, Size);
      if Seek(-Size, SoEnd) < 0 then
        Result := ZM_Error({_LINE_}410, ZE_FailedSeek)
      else
        if Read(PByte(ZipBuf)^, Size) <> Size then
          Result := ZM_Error({_LINE_}413, ZE_EOCBadRead);
      // end;
      if Result = 0 then
      begin
        for I := Size - SizeOf(TZipEndOfCentral) - 1 downto 0 do
          if PZipEndOfCentral(PAnsiChar(ZipBuf) + I)^.HeaderSig = EndCentralDirSig
          then
          begin
            FEOCOffset := File_Size - (Size - I);
            PEOC := PZipEndOfCentral(@ZipBuf[I]);
            Result := EOCSigFound; // something found
            // If we have ZipComment: Save it
            AfterEOC := Size - (I + SizeOf(TZipEndOfCentral));
            Clen := PEOC^.ZipCommentLen;
            if AfterEOC < Clen then
              Clen := AfterEOC;
            if Clen > 0 then
            begin
              SetLength(FZipComment, Clen);
              for J := 1 to Clen do
                FZipComment[J] := ZipBuf[I + Sizeof(TZipEndOfCentral) + J - 1];
            end;
            // Check if we really are at the end of the file, if not correct the File_Size
            // and give a warning. (It should be an error but we are nice.)
            if I + SizeOf(TZipEndOfCentral) + Clen <> Size then
            begin
              File_Size := File_Size +
                ((I + SizeOf(TZipEndOfCentral) + Clen) - Size);
              // Now we need a check for WinZip Self Extractor which makes SFX files which
              // almost always have garbage at the end (Zero filled at 512 byte boundary!)
              // In this special case 'we' don't give a warning.
              // Unfortunately later versions use a different boundary so the test is invalid
              Result := EOCBadComment; // has 'garbage'
            end;
            Break;
          end; // for
      end;
    end;
    if Result > 0 then
    begin
      Result := Result and (EOCBadComment or EOCBadStruct); // remove 'found' flag
      DiskNr := PEOC^.ThisDiskNo;
      TotalDisks := PEOC^.ThisDiskNo + 1; // check
      TotalEntries := PEOC^.TotalEntries;
      CentralEntries := PEOC^.CentralEntries;
      CentralDiskNo := PEOC^.CentralDiskNo;
      CentralOffset := PEOC^.CentralOffset;
      CentralSize := PEOC^.CentralSize;
      if (PEOC^.TotalEntries = MAX_WORD) or (PEOC^.CentralOffset = MAX_UNSIGNED)
        or (PEOC^.CentralEntries = MAX_WORD) or
        (PEOC^.CentralSize = MAX_UNSIGNED) or (PEOC^.ThisDiskNo = MAX_WORD) or
        (PEOC^.CentralDiskNo = MAX_WORD) then
      begin
        Result := Result or EOCWant64;
      end;
    end
    else
    if Result = 0 then
      Result := ZM_Error({_LINE_}471, ZE_NoValidZip);
  finally
    ZipBuf := nil;
  end;
end;

procedure TZMZipEOC.SetZipComment(const Value: AnsiString);
begin
  FZipComment := Value;
end;

procedure TZMZipEOC.SetZipCommentLen(const Value: Integer);
var
  C: AnsiString;
begin
  if (Value <> ZipCommentLen) and (Value < Length(ZipComment)) then
  begin
    C := ZipComment;
    SetLength(C, Value);
    ZipComment := C;
  end;
end;

// returns >0 ok = bytes written, <0 -ErrNo
function TZMZipEOC.WriteEOC: Integer;
type
  TEOCrecs = packed record
    Loc: TZip64EOCLocator;
    Eoc: TZipEndOfCentral;
  end;
  PEOCrecs = ^TEOCrecs;
var
  Clen: Integer;
  Eoc64: TZipEOC64;
  Er: array of Byte;
  Erz: Integer;
  Need64: Boolean;
  Peoc: PEOCrecs;
  T: Integer;
begin
  Result := -1; // keeps compiler happy
  TotalDisks := DiskNr + 1; // check
  Need64 := False;
  ZeroMemory(@Eoc64, Sizeof(Eoc64));
  Clen := Length(ZipComment);
  ASSERT(Clen = ZipCommentLen, ' ZipComment length error');
  Erz := Sizeof(TEOCrecs) + Clen;
  SetLength(Er, Erz + 1);
  Peoc := @Er[0];
  Peoc^.Eoc.HeaderSig := EndCentralDirSig;
  Peoc^.Loc.LocSig := EOC64LocatorSig;
  if Clen > 0 then
    Move(ZipComment[1], Er[Sizeof(TEOCrecs)], Clen);
  Peoc^.Eoc.ZipCommentLen := Clen;
  // check Zip64 needed
  if TotalDisks > MAX_WORD then
  begin
    Peoc^.Eoc.ThisDiskNo := MAX_WORD;//Word(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.ThisDiskNo := Word(TotalDisks - 1); // check

  if CentralDiskNo >= MAX_WORD then
  begin
    Peoc^.Eoc.CentralDiskNo := MAX_WORD;//Word(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.CentralDiskNo := CentralDiskNo;

  if TotalEntries >= MAX_WORD then
  begin
    Peoc^.Eoc.TotalEntries := MAX_WORD;//Word(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.TotalEntries := Word(TotalEntries);

  if CentralEntries >= MAX_WORD then
  begin
    Peoc^.Eoc.CentralEntries := MAX_WORD;//Word(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.CentralEntries := Word(CentralEntries);

  if CentralSize >= MAX_UNSIGNED then
  begin
    Peoc^.Eoc.CentralSize := MAX_UNSIGNED;//Cardinal(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.CentralSize := CentralSize;

  if (CentralOffset >= MAX_UNSIGNED) then
  begin
    Peoc^.Eoc.CentralOffset := MAX_UNSIGNED;//Cardinal(-1);
    Need64 := True;
  end
  else
    Peoc^.Eoc.CentralOffset := CentralOffset;

  if not Need64 then
  begin
    // write 'normal' EOC
    Erz := Erz - Sizeof(TZip64EOCLocator); // must not split
    Result := WriteContiguous(Er[Sizeof(TZip64EOCLocator)], Erz, True);
    if Result <> Erz then
    begin
      if Result = MustFitError then
      begin
        if DiskNr >= MAX_WORD then
          Need64 := true
        else
        begin
          peoc^.eoc.ThisDiskNo := Word(DiskNr); // update
          Result := WriteContiguous(er[sizeof(TZip64EOCLocator)], erz, True);
          if Result <> erz then
            Result := ZM_Error({_LINE_}574, ZE_EOCBadWrite);
        end;
      end
      else
        Result := ZM_Error({_LINE_}578, ZE_EOCBadWrite);
    end;
  end;
  if Need64 then
  begin
    Z64 := True;
    Eoc64.EOC64Sig := EndCentral64Sig;
    Eoc64.Vsize := Sizeof(Eoc64) - 12;
    Eoc64.VersionMade := ZIP64_VER;
    Eoc64.VersionNeed := ZIP64_VER;
    Eoc64.ThisDiskNo := DiskNr;
    Eoc64.CentralDiskNo := CentralDiskNo;
    Eoc64.TotalEntries := TotalEntries;
    Eoc64.CentralEntries := CentralEntries;
    Eoc64.CentralSize := CentralSize;
    Eoc64.CentralOffset := CentralOffset;
    Peoc^.Loc.EOC64RelOfs := Position;
    Peoc^.Loc.EOC64DiskStt := DiskNr;
    Result := WriteContiguous(Eoc64, Sizeof(TZipEOC64), False); // can be on next part
    if Result = Sizeof(TZipEOC64) then
    begin
      Peoc^.Loc.NumberDisks := DiskNr + 1; // may be new part
      if DiskNr >= MAX_WORD then
        Peoc^.Eoc.ThisDiskNo := MAX_WORD
      else
        Peoc^.Eoc.ThisDiskNo := Word(DiskNr);
      Result := Sizeof(TZipEndOfCentral) + Peoc^.Eoc.ZipCommentLen; // if it fits
      T := WriteContiguous(Er[0], Erz, True);
      if T <> Erz then
      begin
        if T = MustFitError then
        begin
          // didn't fit, update and write to next part
          Peoc^.Loc.NumberDisks := DiskNr + 1;
          if DiskNr >= MAX_WORD then
            Peoc^.Eoc.ThisDiskNo := MAX_WORD
          else
            Peoc^.Eoc.ThisDiskNo := Word(DiskNr);
          T := WriteContiguous(Er[0], Erz, True);
          if T <> Erz then
            Result := ZM_Error({_LINE_}642, ZE_EOCBadWrite);
        end
        else
          Result := ZM_Error({_LINE_}642, ZE_EOCBadWrite);  // return effective error
      end;
    end;
  end;
end;

end.

