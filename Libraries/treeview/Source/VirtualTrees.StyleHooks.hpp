// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.StyleHooks.pas' rev: 32.00 (Windows)

#ifndef Virtualtrees_StylehooksHPP
#define Virtualtrees_StylehooksHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.UxTheme.hpp>
#include <System.Classes.hpp>
#include <Vcl.Themes.hpp>
#include <Vcl.Controls.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Stylehooks
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVclStyleScrollBarsHook;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVclStyleScrollBarsHook : public Vcl::Themes::TMouseTrackControlStyleHook
{
	typedef Vcl::Themes::TMouseTrackControlStyleHook inherited;
	
	
private:
	class DELPHICLASS TVclStyleScrollBarWindow;
	#pragma pack(push,8)
	class PASCALIMPLEMENTATION TVclStyleScrollBarWindow : public Vcl::Controls::TWinControl
	{
		typedef Vcl::Controls::TWinControl inherited;
		
	private:
		TVclStyleScrollBarsHook* FScrollBarWindowOwner;
		bool FScrollBarVertical;
		bool FScrollBarVisible;
		bool FScrollBarEnabled;
		HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Msg);
		HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TMessage &Msg);
		HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
		
	protected:
		virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
		
	public:
		__fastcall virtual TVclStyleScrollBarWindow(System::Classes::TComponent* AOwner);
		__property TVclStyleScrollBarsHook* ScrollBarWindowOwner = {read=FScrollBarWindowOwner, write=FScrollBarWindowOwner};
		__property bool ScrollBarVertical = {read=FScrollBarVertical, write=FScrollBarVertical, nodefault};
		__property bool ScrollBarVisible = {read=FScrollBarVisible, write=FScrollBarVisible, nodefault};
		__property bool ScrollBarEnabled = {read=FScrollBarEnabled, write=FScrollBarEnabled, nodefault};
	public:
		/* TWinControl.CreateParented */ inline __fastcall TVclStyleScrollBarWindow(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
		/* TWinControl.Destroy */ inline __fastcall virtual ~TVclStyleScrollBarWindow(void) { }
		
	};
	
	#pragma pack(pop)
	
	
private:
	System::Types::TRect FHorzScrollBarDownButtonRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarDownButtonState;
	System::Types::TRect FHorzScrollBarRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarSliderState;
	System::Types::TRect FHorzScrollBarSliderTrackRect;
	System::Types::TRect FHorzScrollBarUpButtonRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarUpButtonState;
	TVclStyleScrollBarWindow* FHorzScrollBarWindow;
	bool FLeftMouseButtonDown;
	int FPrevScrollPos;
	float FScrollPos;
	System::Types::TRect FVertScrollBarDownButtonRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarDownButtonState;
	System::Types::TRect FVertScrollBarRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarSliderState;
	System::Types::TRect FVertScrollBarSliderTrackRect;
	System::Types::TRect FVertScrollBarUpButtonRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarUpButtonState;
	TVclStyleScrollBarWindow* FVertScrollBarWindow;
	MESSAGE void __fastcall CMUpdateVclStyleScrollbars(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMNCLButtonDown(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCMouseMove(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMNCLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMVScroll(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMHScroll(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMCaptureChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMNCLButtonDblClk(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMSize(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMPosChanged(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall CalcScrollBarsRect(void);
	virtual void __fastcall DrawHorzScrollBar(HDC DC);
	virtual void __fastcall DrawVertScrollBar(HDC DC);
	System::Types::TRect __fastcall GetHorzScrollBarSliderRect(void);
	System::Types::TRect __fastcall GetVertScrollBarSliderRect(void);
	virtual void __fastcall MouseLeave(void);
	virtual void __fastcall PaintScrollBars(void);
	bool __fastcall PointInTreeHeader(const System::Types::TPoint &P);
	void __fastcall UpdateScrollBarWindow(void);
	
public:
	__fastcall virtual TVclStyleScrollBarsHook(Vcl::Controls::TWinControl* AControl);
	__fastcall virtual ~TVclStyleScrollBarsHook(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word CM_UPDATE_VCLSTYLE_SCROLLBARS = System::Word(0xb802);
}	/* namespace Stylehooks */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_STYLEHOOKS)
using namespace Virtualtrees::Stylehooks;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_StylehooksHPP
