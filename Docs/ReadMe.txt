
			Welcome to the Delphi Zip v1.92
                   This is the Delphi Edition for version 7 and later only
					September 04, 2015
						 
Major Changes
	Supports Windows NT and later

Delphi versions supported
	not before 5

						Directory Structure
		ZipMaster files
		Delphi			 - design- and run-time page files
		Demos 			 - Demos and SortGrid
		Dll				 - dll
		DLL\Source		 - Source code for dll (Available separately
		Docs 			 - where this file resides
		Res				 - resource files for connecting to the application or SFX
		Res\Lang		 - Language files
		Tools			 - 

The required Delphi source files (files marked with '+' are written by ZipResMaker.exe)
    ...\
		ZipMstr.pas			 - main component source
		ZipMstr.res			 - components version resource 
		ZipVers.inc			 - required defines for Delphi versions (only 5..XE5 supported)
		ZMArgSplit.pas		 - used by MergeZippedFiles to 'read' command arguments
		ZMBaseOpr.pas		 - basic support for operations
		ZMBody.pas			 - support functions
		ZMCoDec.pas			 - compression and decompression classes
		ZMConfig192.inc		 - configuration settings
		ZMCommand.pas		 - runs various operations
		ZMCompat.pas		 - support for older compilers
		ZMCore.pas			 - basic support functions and event triggering
		ZMCRC.pas			 - CRC32 table
		ZMCtx.pas			 - Dialog box help context values
		ZMDelZip.pas		 - dll interface definitions
		ZMDlg.pas			 - dialog box support
		ZMDllLoad.pas		 - dynamically loads and binds the dll 
		ZMDrv.pas			 - Handles drive level parameters and methods
		ZMEngine.pas		 - compression/decompression engine
		ZMEntryReader.pas	 - representation and read-only functions for zip central directory
		ZMFileOpr.pas		 - basic operations on zip file
		ZMFStream.pas		 - stream to handle files
		ZMHandler.pas		 - message string and error handlers
		ZMLister.pas		 - loads and controls the zip file specified
		ZMMatch.pas			 - wildcard file spec matching
		ZMMFStream.pas		 - stream to handle multi-part files
		ZMMisc.pas			 - miscellaneous internal support classes
		ZMMsg.pas			 - message values
		ZMOprCore.pas		 - basic support for operations
		ZMOprDeflate.pas	 - stream deflate operations
		ZMOprDel.pas		 - delete entries from zip
		ZMOprDll.pas		 - operations that require the dll
		ZMOprFile.pas		 - operations on zip file
		ZMOprMerge.pas		 - operations copying or merging zipped files
		ZMOprMod.pas		 - operations that modify the zip file
		ZMOprMsgStr.pas		 - language operations
		ZMOprUnzip.pas		 - extract operations
		ZMSFXInt.pas		 - SFX stub interface structures and definitions
		ZMStructs.pas		 - definition of internal zip structures 
		ZMUnzipOpr.pas		 - basic unzip functions
		ZMUTF8.pas			 - functions for handling UTF8/UTF16
		ZMUtils.pas			 - some functions to make life easier
		ZMWinFuncs.pas		 - support 'wide' file names and paths for non-Unicode Delphi
		ZMXcpt.pas			 - EZipMaster definitions
		ZMZipBase.pas		 - primitive support for files
		ZMZipDirectory.pas	 - represents the 'Central Directory' of a zip file
		ZMZipEOC.pas		 - represents and finds, reads, writes the zip end of central structures
		ZMZipMulti.pas		 - low level support for handling multi-part zips
		ZMZipReader.pas		 - handles reading a zip file
		ZMZipWriter.pas		 - support for writing a zip file
		ZMZLibExApi.pas		 - ZLIB interface
	...\zlib\				 - zlib files from Base2 Technologies http://www.base2ti.com
	...\zlib\win32\			 - Win32 precompiled object files (compiled without zstream support)
	...\zlib\win64\			 - Win64 precompiled object files (compiled without zstream support)

	...\RES\
+		ZMRes192_lng.rc		 - resource script for compressed languages and dll
+		ZMRes192_lng.res	 - compiled resource for applications using ZipMaster (link to application)
		ZMRes192_sfx.rc		 - resource script for including sfx stub
		ZMRes192_sfx.res	 - compiled resource for including sfx stub (link to application) 
		ZMRes192_sfxu.rc	 - resource script for including Unicode sfx stub
		ZMRes192_sfxu.res	 - compiled resource for including Unicode sfx stub (link to application) 
		ZMRes192_dll.rc		 - resource script for including compressed dll
		ZMRes192_dll.res	 - compiled resource for including compressed dll (link to application)(optional)
		ZMRes192_all.rc		 - resource script for languages, dll and sfx combined
		ZMRes192_all.res	 - compiled resource for languages, dll and sfx combined (link to application)
		ZMSFX192.bin		 - Ansi sfx stub
		ZMSFXU192.bin		 - Unicode sfx stub (requires XP or later)
		DZ_Langs.zip		 - language text files for component
	...\RES\LANGS\
		ZipMsg.h			 - master message identifier header file
		DefaultStrs.txt		 - default language neutral messages
		Countries.txt		 - country information
		ZipMsg??.res		 - compiled language resource file
		SFXstr_??.txt		 - language files for sfx

	...\Packages\ 
		ZMstr192D*.bpk		 - design and run-time package (? is compiler version)
		ZMstr192D*.res		 - package resources
	...\Packages\XE*\
		ZipMaster.groupproj	 - group file for run-time and design packages
		ZipMasterD.dpk		 - design package
		ZipMasterD.dproj	 - design package
		ZipMasterD.res		 - design package resources
		ZipMasterR.dpk		 - run-time package
		ZipMasterR.dproj	 - run-time package
		ZipMasterR.res		 - run-time package resources


	...\DLL\
		DelZip192.dll		 - required dll for Add only (x32)
		Delzip192x64.dll	 - required dll for Add only (x64)

	...\DOCS\
		licence.txt			 - a copy of the licence
		ReadMe.txt			 - this file
		Install.txt			 - instructions to install component in I.D.E
		Debug.txt			 - some debugging tips
		ZipMaster.chm		 - compiled html file
		dzsfx.chm			 - SFX help file

	...\HELP\SOURCE\		 - source files for help

    ...\DEMOS\DEMO1\		 - zip adder/extractor

    ...\DEMOS\DEMO2\		 - quick add/extract and dll test

    ...\DEMOS\DEMO3\		 - another add/extract example

    ...\DEMOS\DEMO4\		 - simple self installer

    ...\DEMOS\DEMO5\		 - make exe file (sfx)

    ...\DEMOS\DEMO6\		 - span multiple disks

    ...\DEMOS\DEMO7\		 - extract from stream

    ...\DEMOS\DEMO9\		 - use in thread

    ...\DEMOS\DEMO11\		 - merge zipped files from multiple zips

    ...\DEMOS\SortGrid\		 - (optional) sort grid component (used in some Demos)
            SortGrid.pas	 - 
            SortGrid.res	 - 
            SortGrid.dcr	 - 
            SortGridreg.pas	 - 
            SortGridPreview.pas	 - 
            SortGridPreview.dfm	 - 



					License

	This component is subject to a variation of the BSD 3-Clause license
	 (http://www.opensource.org/licenses/BSD-3-Clause)
     as explained in full in the Help file and in licence.txt.

 
					DLL Source Code in C 

		The DLL source code is distributed separately due to it's
	size, and the fact that most Delphi users of this package
	probably don't want the C source for the DLL's.  The DLL 
	source is also freeware, and will remain that way. 
	The DLL source code needs Borland C++ Builder v5 or later.


					Problem Reports or Suggestions

	We DO want to hear your ideas!  If you find a problem with
	any part of this project, or if you just have an idea for
	us to consider, send us e-mail!

	But, please make sure that your bug has not already been
	reported.  Check the "official" web site often:

	Latest Versions and changes available at
	http://www.delphizip.org/index.html

		Problems
	please report any to 
	problems@delphizip.org

	Amended and updated by
	R.Peters 

