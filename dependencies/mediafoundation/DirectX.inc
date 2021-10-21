{******************************************************************************
                Copyright (c) PilotLogic Software House
                     
 Package pl_Win_DirectX
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)      
                                                                               
   ****** BEGIN LICENSE BLOCK *****     
                                                              
   The contents of this file are used with permission, subject to the Mozilla   
   Public License Version 2.0 (the "License"); you may not use this file except 
   in compliance with the License. You may obtain a copy of the License at      
   https://www.mozilla.org/en-US/MPL/2.0/                                     
                                                                               
   Software distributed under the License is distributed on an "AS IS" basis,   
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for 
   the specific language governing rights and limitations under the License. 
   
   ****** END LICENSE BLOCK *****  
  
**********************************************************************************}

  {$ASMMODE Intel}

  // Additional settings
  {$H+} // Long Strings
  {$BOOLEVAL OFF}
  {$MINENUMSIZE 4}
  {$ALIGN ON}
  {$PACKRECORDS 8}
  {$INLINE ON}

  {$UNDEF TYPE_IDENTITY}
  {$DEFINE SUPPORTS_EXCEPTIONS}
  {$DEFINE SUPPORTS_INLINE}
  {$DEFINE COMPILER5_UP} // Specially for DirectDraw.pas

// By default use most recent DirectX sub-version

{$IFNDEF DX81}
  {$IFNDEF DX80}
    {$DEFINE DX81}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DX92}
  {$IFNDEF DX91}
    {$IFNDEF DX90}
      {$DEFINE DX92}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}




