// the code in the FHIR. namespace compiles for the following targets:

// WINDOWS - Microsoft Windows 32 or 64 bit, either delphi or FPC
// OSX - OSX 10 64 - FPC only
// LINUX - 64 bit only, FPC only

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDRECORDS}
  {$MODESWITCH TYPEHELPERS}
  {$IFDEF LINUX}
    {$IFNDEF CPU64}
    Linux only compiles for 64bit
    {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
    {$IFNDEF CPU64}
    OSX only compiles for 64bit (OSX only)
    {$ENDIF}
    {$DEFINE OSX}      
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}  
  {$ENDIF}
  {$IFNDEF LINUX}
    {$IFNDEF DARWIN}
      {$IFNDEF MSWINDOWS}
       This compile target is not supported
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF WINDOWS}
    {$DEFINE NO_JS} // still working on getting this compiling
  {$ENDIF}
{$ELSE}
  {$IFNDEF MSWINDOWS}
    only windows is supported under delphi
  {$ENDIF}
  {$DEFINE WINDOWS}
  {$DEFINE DELPHI}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}
  {$ELSE}
    {$DEFINE CPU32}
  {$ENDIF}

{$ENDIF}

{
The base class TFslObject can track all instantiated objects.
Doing so it useful for leak hunting in production, but is also
costly. Enable it with this define
}
{$IFOPT D+}
{$DEFINE OBJECT_TRACKING}
{$ENDIF}

