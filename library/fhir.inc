// the code in the FHIR. namespace compiles for the following targets:

// WINDOWS - Microsoft Windows 32 or 64 bit, either delphi or FPC
// OSX - OSX 10 64 - FPC only
// LINUX - 64 bit only, FPC only

{$IFDEF FPC}
  {$DEFINE STATICLOAD_OPENSSL}
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
  {$warn 04122 error} // creating an abstract class
  {.$.warn 04046 error} // creating a class with an abstract method

  // mostly these are because of generics
  {$WARN 5033 off : Function result does not seem to be set}
  {$warn 05093 off}   // function result variable of a managed type does not seem to be initialized
  {$warn 05071 off}   // Note: Private type X never used
  {$WARN 3057 off : An inherited method is hidden by "$1"}
  {$WARN 6058 off : Marked as inline is not inlined}       
  {$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"} 
  {$WARN 3018 off : Constructor should be public}   
  {$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}  
  {$WARN 5089 off : Local variable "$1" of a managed type does not seem to be initialized}
  {$WARN 5093 off : fsl_base.pas(2354,19) Warning: function result variable of a managed type does not seem to be initialized}
  {$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
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
OpenSSL is always statically loaded.
Or in the case of FPC compiled applications, statically bound
}
{$DEFINE STATICLOAD_OPENSSL}
                    
// there's a bug in the lazarus/FPC compiler where global settings
// such as assertions are not consistently the same across all units.
// this sets the global compile settings for development consistently based
// on the presence of the define 'PRODUCTION' in the project set up

{$IFDEF PRODUCTION}

{$C-} // assertions off
{$I-} // IO checking off
{$Q-} // overflow checking off
{$R-} // range checking off
{$OPTIMIZATION LEVEL3} // FPC level 3 optimizations for production
{$D-} // debugging info off for production

{$ELSE}

{$C+} // assertions on (needed to pass the tests)
{$I+} // IO checking on - though this is probably useless?
{$Q-} // overflow checking off - these are always off; doesn't run with them on
{$R-} // range checking off - these are always off; doesn't run with them on
{$OPTIMIZATION OFF} // all optimizations off for development
{$D+} // debugging info on for development

{
The base class TFslObject can track all instantiated objects.
Doing so is useful for leak hunting in production, but is also a little costly.
}
{$DEFINE OBJECT_TRACKING}

{$ENDIF}


