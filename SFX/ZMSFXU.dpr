
(******************************************************************)
(* SFX for DelZip v1.8                                            *)
(* ZipSFX                                                         *)
(* Copyright 1997, Carl Bunton  Twojags@cris.com                  *)
(*                                                                *)
(* 1998-2001 maintained by Chris Vleghert                         *)
(*                                                                *)
(* 2002-? maintained again by Markus Stephany                     *)
(* mailto:delphizip@mirkes.de                                     *)
(* http://delphizip.mirkes.de                                     *)
(*                                                                *)
(* Credits: see CREDITS.TXT                                       *)
(*                                                                *)
(* last changed: 09/19/2005                                       *)
(*                                                                *)
(* In memory of Chris Vleghert                                    *)
(*                                                                *)
(* modified by Russell Peters, Roger Aelbrecht*)

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

// Changes RCV:
// Jan. 10, 1999  Adapted for D4 beta v0.99f=now v1.60
// Feb. 10, 1999  Changed the Initialization and Finalization sections
//                to include file close and CRC table.
//                ( The Crc table was not freed after an Halt. )
// Jun. 15, 2000  Added code to Dialog.pas to free a pidl, bug found by
//                Lucjan Lukasik
// Sep. 01, 2000  Added version Checked for Delphi 5 and BCB 4 and 5
// Oct. 09, 2000  Added DirExists to the function FileExists because
//                FindFirstFile does not work when there is no file on
//                a drive (e.g. an empty 'A' drive) extract to that drive
//                would not work, found by Clyde England clyde@conres.com.au

// changes mst:
// apr 2002       almost completely rewritten (or better re-cut'n'pasted...)
//                - moved routines/types/variables to appropriate units
//                - added progress bar, ability to (initially) hide the files listview
//                - removed "new directory" handling (this is handled by newer windows' themselves)
//                - added the ability to expand environment variables
//                - uses a new sfxheader instead of the MPV header; it's now a pascal record
//                - added the ability to interrupt extraction (by pressing the "X" in the caption bar)
// may 01, 2002   added the GetExeSize function from Angus Johnson's TZip-SFX to get rid of
//                caring about the executable's size
// further changes: see history.txt in the ..\doc\ directory

{ Notes:

the initial release of zipsfx comes from Carl Bunton (see above).

the first modifications came from Eric W. Engler, the author of the great freeware
delphi-vcl delzip that can handle zip-archives and -sfx's. (EEngler@zcsterling.com)

original zip-code comes from the infozip-group, they developped a free implementation
of the zip/unzip-code for unix and later for other platforms.
  Info-Zip home page:
  http://freesoftware.com/pub/infozip/Info-ZIP.html

regards, Markus Stephany
saarbrücken, saarland, germany, january 2004/september 2005

please read SFXInterface.pas for further details.


}

  (* the structure of a zipsfx-file :

  - zipsfx-executable code (0-xxxxx)
  - TSFXFileHeader record (see above)
  - possibly one or more strings (depending on the headers' properties) (not #0 terminated)
  Caption                             sfx dialog caption
  Path                                sfx default extraction path
  CmdLine                             command line to execute after extraction
  RegFailPath                         default extract path if Path could not be read from registry
  StartMsg                            startup message

  NOTE: the complete header (including the strings) must be DWORD-aligned!

  if not detached from the archive:
  - the zip archive

  if detached from the archive:
  - TSFXDetachedHeader + filename + TSFXDetachedHeader
  - centraldirectories[] + endofcentraldir of the zip archive


  *)

program ZMsfxU;

{$IFNDEF UNICODE}
  Delphi 2009 or later needed
{$ENDIF}

{$R 'ZMSFXDLG.res' 'ZMSFXDLG.rc'}
{$R 'ZMSFXU_ver.res' 'ZMSFXU_ver.rc'}

uses
  Windows,
  ZMSFXDialogs in 'ZMSFXDialogs.pas',
  ZMSFXProcs in 'ZMSFXProcs.pas',
  ZMSFXInflate in 'ZMSFXInflate.pas',
  ZMSFXDefs in 'ZMSFXDefs.pas',
  ZMSFXStructs in 'ZMSFXStructs.pas',
  ZMSFXVars in 'ZMSFXVars.pas',
  ZMSFXStrings in 'ZMSFXStrings.pas',
  ZMSFXWinTrust in 'ZMSFXWinTrust.pas',
  ZMSFXInt in 'ZMSFXInt.pas';

begin
  Run;
end.
