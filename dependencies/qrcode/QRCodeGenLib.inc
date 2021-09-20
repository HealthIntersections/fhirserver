{ *********************************************************************************** }
{ *                              QRCodeGenLib Library                               * }
{ *                 Copyright (c) 2018 - 2019 Ugochukwu Mmaduekwe                   * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

 {$DEFINE DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)
{$IFDEF FPC}
{$I QRCodeGenLibHelper.inc} // Had to Include this Since Delphi Does not allow "FPC_FULLVERSION" to Compile.
{$UNDEF DELPHI}
{$MODE delphi}

{$DEFINE USE_UNROLLED_VARIANT}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

// Enable Pointer Math
{$POINTERMATH ON}

// Disable Warnings and Hints.
{$WARNINGS OFF}
{$HINTS OFF}
{$NOTES OFF}

// Optimizations
{$OPTIMIZATION LEVEL3}
{$OPTIMIZATION PEEPHOLE}
{$OPTIMIZATION REGVAR}
{$OPTIMIZATION LOOPUNROLL}
{$OPTIMIZATION STRENGTH}
{$OPTIMIZATION CSE}
{$OPTIMIZATION DFA}

{$IFDEF CPUI386}
   {$OPTIMIZATION USEEBP}
{$ENDIF}

{$IFDEF CPUX86_64}
   {$OPTIMIZATION USERBP}
{$ENDIF}

{$DEFINE Framework_FCL} // enable to force FCL mode
{$IFDEF Framework_FCL}
   {$UNDEF LCL}
   {$DEFINE FCL}
{$ELSE}
   {$DEFINE LCL}
{$ENDIF}

{$IFDEF LCL}
   {$DEFINE VCL_OR_LCL}
{$ENDIF LCL}

{$ENDIF FPC}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

{$IFDEF DELPHI}

{$DEFINE USE_UNROLLED_VARIANT}

// This option is needed to enable code browsing (aka Ctrl+Click)
// It does not affect the binary size or generated code
{$DEFINITIONINFO ON}

// Disable Overflow and RangeChecks.
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

 // Enable Pointer Math
{$POINTERMATH ON}

// Disable String Checks
{$STRINGCHECKS OFF}

// Disable Duplicate Constructor Warnings
{$WARN DUPLICATE_CTOR_DTOR OFF}

 // XE3 and Above
{$IF CompilerVersion >= 24.0}
   {$DEFINE DELPHIXE3_UP}
   {$LEGACYIFEND ON}
   {$ZEROBASEDSTRINGS OFF}
{$IFEND}

  // XE and Above
{$IF CompilerVersion >= 22.0}
   {$DEFINE DELPHIXE_UP}
{$IFEND}

  // XE3 and Above
{$IFNDEF DELPHIXE3_UP}
   {$MESSAGE ERROR 'This Library requires Delphi XE3 or higher.'}
{$ENDIF}

{.$DEFINE Framework_FMX} // enable to force FMX mode
{$IFDEF Framework_FMX}
   {$UNDEF VCL}
   {$DEFINE FMX}
{$ELSE}
   {$DEFINE VCL}
{$ENDIF}

{$IFDEF VCL}
   {$DEFINE VCL_OR_LCL}
{$ENDIF VCL}

{$ENDIF DELPHI}

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

