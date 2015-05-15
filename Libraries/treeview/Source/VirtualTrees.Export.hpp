// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.Export.pas' rev: 29.00 (Windows)

#ifndef Virtualtrees_ExportHPP
#define Virtualtrees_ExportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <System.StrUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <VirtualTrees.hpp>
#include <VirtualTrees.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Export
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::AnsiString __fastcall ContentToText(Virtualtrees::TCustomVirtualStringTree* Tree, Virtualtrees::TVSTTextSourceType Source, const System::AnsiString Separator);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ContentToHTML(Virtualtrees::TCustomVirtualStringTree* Tree, Virtualtrees::TVSTTextSourceType Source, const System::UnicodeString Caption = System::UnicodeString());
extern DELPHI_PACKAGE System::RawByteString __fastcall ContentToRTF(Virtualtrees::TCustomVirtualStringTree* Tree, Virtualtrees::TVSTTextSourceType Source);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ContentToUnicode(Virtualtrees::TCustomVirtualStringTree* Tree, Virtualtrees::TVSTTextSourceType Source, const System::UnicodeString Separator);
extern DELPHI_PACKAGE NativeUInt __fastcall ContentToClipboard(Virtualtrees::TCustomVirtualStringTree* Tree, System::Word Format, Virtualtrees::TVSTTextSourceType Source);
extern DELPHI_PACKAGE void __fastcall ContentToCustom(Virtualtrees::TCustomVirtualStringTree* Tree, Virtualtrees::TVSTTextSourceType Source);
}	/* namespace Export */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_EXPORT)
using namespace Virtualtrees::Export;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_ExportHPP
