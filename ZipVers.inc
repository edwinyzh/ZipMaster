{$IfDef CLR}
   {$DEFINE BADCOMPILER}
    YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT DESIGNED!

    YOU HAVE BEEN WARNED !!!
{$EndIf}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$IF (CompilerVersion >= 14)} // Delphi 6.0 or newer
    {$DEFINE VERD6up}
  {$IFEND}
  {$IF (CompilerVersion >= 15)} // Delphi 7.0 or newer
    {$WARN UNSAFE_CODE OFF} 
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$DEFINE VERD7up}
  {$IFEND}
  {$IF (CompilerVersion >= 17)} // Delphi 2005 or newer
    {$DEFINE VERD2005up}
    {$DEFINE ZM_INLINE}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$IFEND}
  {$IF (CompilerVersion >= 18.5)} // Delphi 2007 or newer
    {$DEFINE VERD2007up}
  {$IFEND}
  {$IF (CompilerVersion >= 20)} // Delphi 2009 or newer   - UNICODE
    {$DEFINE VERD2009up}
  {$IFEND}
  {$IF (CompilerVersion >= 21)} // Delphi 2010 or newer   - UNICODE
    {$DEFINE VERD2010up}
  {$IFEND}
  {$IF (CompilerVersion >= 22)} // Delphi XE or newer   - UNICODE
    {$DEFINE VERDXEup}
  {$IFEND}
  {$IF (CompilerVersion >= 23)} // Delphi XE2 or newer   - UNICODE
    {$DEFINE VERDXE2up}
  {$IFEND}
  {$IF (CompilerVersion >= 24)} // Delphi XE3 or newer   - UNICODE
    {$DEFINE VERDXE3up}
  {$IFEND}
  {$IF (CompilerVersion >= 25)} // Delphi XE4 or newer   - UNICODE
    {$DEFINE VERDXE4up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE464up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 26)} // Delphi XE5 or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 27)} // Delphi XE6 or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 28)} // Delphi XE7 or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 29)} // Delphi XE8 or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 30)} // Delphi DX Seattle or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}
  {$IF (CompilerVersion >= 31)} // Delphi DX London or newer   - UNICODE
    {$DEFINE VERDXE5up}
	{$IFDEF WIN64}
    {$DEFINE VERDXE564up}
	{$ENDIF}
  {$IFEND}


{$ENDIF}   // $IFDEF CONDITIONALEXPRESSIONS

{$IfDef BADCOMPILER}
  YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT DESIGNED!

  YOU HAVE BEEN WARNED !!!
{$EndIf}

{$IfDef BCB}
  {$ObjExportAll On}
{$Endif}

{$ALIGN ON}
{$BOOLEVAL OFF} // important: must be off!
{$LONGSTRINGS ON}
{$EXTENDEDSYNTAX ON}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$TYPEDADDRESS ON}
{$WRITEABLECONST OFF}


