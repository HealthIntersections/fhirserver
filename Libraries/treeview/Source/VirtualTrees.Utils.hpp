// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.Utils.pas' rev: 29.00 (Windows)

#ifndef Virtualtrees_UtilsHPP
#define Virtualtrees_UtilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Types.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ImgList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Utils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBlendMode : unsigned char { bmConstantAlpha, bmPerPixelAlpha, bmMasterAlpha, bmConstantAlphaAndColor };

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Types::TRect __fastcall OrderRect(const System::Types::TRect &R);
extern DELPHI_PACKAGE void __fastcall SetBrushOrigin(Vcl::Graphics::TCanvas* Canvas, int X, int Y);
extern DELPHI_PACKAGE void __fastcall SetCanvasOrigin(Vcl::Graphics::TCanvas* Canvas, int X, int Y);
extern DELPHI_PACKAGE void __fastcall ClipCanvas(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &ClipRect, HRGN VisibleRegion = (HRGN)(0x0));
extern DELPHI_PACKAGE void __fastcall GetStringDrawRect(HDC DC, const System::UnicodeString S, System::Types::TRect &Bounds, unsigned DrawFormat);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ShortenString(HDC DC, const System::UnicodeString S, int Width, int EllipsisWidth = 0x0);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WrapString(HDC DC, const System::UnicodeString S, const System::Types::TRect &Bounds, bool RTL, unsigned DrawFormat);
extern DELPHI_PACKAGE void __fastcall AlphaBlend(HDC Source, HDC Destination, const System::Types::TRect &R, const System::Types::TPoint &Target, TBlendMode Mode, int ConstantAlpha, int Bias);
extern DELPHI_PACKAGE unsigned __fastcall GetRGBColor(System::Uitypes::TColor Value);
extern DELPHI_PACKAGE void __fastcall PrtStretchDrawDIB(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &DestRect, Vcl::Graphics::TBitmap* ABitmap);
extern DELPHI_PACKAGE bool __fastcall HasMMX(void);
extern DELPHI_PACKAGE void __fastcall FillDragRectangles(int DragWidth, int DragHeight, int DeltaX, int DeltaY, System::Types::TRect &RClip, System::Types::TRect &RScroll, System::Types::TRect &RSamp1, System::Types::TRect &RSamp2, System::Types::TRect &RDraw1, System::Types::TRect &RDraw2);
extern DELPHI_PACKAGE void __fastcall DrawImage(Vcl::Imglist::TCustomImageList* ImageList, int Index, Vcl::Graphics::TCanvas* Canvas, int X, int Y, unsigned Style, bool Enabled);
}	/* namespace Utils */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_UTILS)
using namespace Virtualtrees::Utils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_UtilsHPP
