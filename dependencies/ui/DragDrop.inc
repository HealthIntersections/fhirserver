// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDrop.inc
// Description:     This include file contains common defines used by the
//                  library.
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
// Copyright        © 1997-99 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//			Detect compiler version
// -----------------------------------------------------------------------------

// Delphi 1.x
{$IFDEF VER80}
  {$DEFINE VER8_PLUS}
{$ENDIF}

// Delphi 2.x
{$IFDEF VER90}
  {$DEFINE VER9x}
  {$DEFINE VER9_PLUS}
{$ENDIF}

// C++ Builder 1.x
{$IFDEF VER93}
  {$DEFINE VER9x}
  {$DEFINE VER93_PLUS}
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
  {$DEFINE VER10_PLUS}
{$ENDIF}

// C++ Builder 3.x
{$IFDEF VER110}
  {$DEFINE VER11_PLUS}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
  {$DEFINE VER12_PLUS}
{$ENDIF}

// C++ Builder 4.x
{$IFDEF VER125}
  {$DEFINE VER125_PLUS}
{$ENDIF}

// Delphi 5.x
{$IFDEF VER130}
  {$DEFINE VER13_PLUS}
{$ENDIF}


{$IFDEF VER13_PLUS}
  {$DEFINE VER125_PLUS}
{$ENDIF}
{$IFDEF VER125_PLUS}
  {$DEFINE VER12_PLUS}
{$ENDIF}
{$IFDEF VER12_PLUS}
  {$DEFINE VER11_PLUS}
{$ENDIF}
{$IFDEF VER11_PLUS}
  {$DEFINE VER10_PLUS}
{$ENDIF}
{$IFDEF VER10_PLUS}
  {$DEFINE VER93_PLUS}
{$ENDIF}
{$IFDEF VER93_PLUS}
  {$DEFINE VER9_PLUS}
{$ENDIF}
{$IFDEF VER9_PLUS}
  {$DEFINE VER8_PLUS}
{$ENDIF}

// Unknown compiler version - assume D5 compatible
{$IFNDEF VER8_PLUS}
  {$DEFINE VER13_PLUS}
{$ENDIF}


// -----------------------------------------------------------------------------
//			Compiler directives
// -----------------------------------------------------------------------------

// @ operator must return an untyped pointer - the delphi default
{$T-}
