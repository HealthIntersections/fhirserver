// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.Classes.pas' rev: 32.00 (Windows)

#ifndef Virtualtrees_ClassesHPP
#define Virtualtrees_ClassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBufferedAnsiString;
class DELPHICLASS TWideBufferedString;
class DELPHICLASS TCriticalSection;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBufferedAnsiString : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	char *FStart;
	char *FPosition;
	char *FEnd;
	System::RawByteString __fastcall GetAsString(void);
	
public:
	__fastcall virtual ~TBufferedAnsiString(void);
	void __fastcall Add(const System::RawByteString S);
	void __fastcall AddNewLine(void);
	__property System::RawByteString AsString = {read=GetAsString};
public:
	/* TObject.Create */ inline __fastcall TBufferedAnsiString(void) : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TWideBufferedString : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::WideChar *FStart;
	System::WideChar *FPosition;
	System::WideChar *FEnd;
	System::UnicodeString __fastcall GetAsString(void);
	
public:
	__fastcall virtual ~TWideBufferedString(void);
	void __fastcall Add(const System::UnicodeString S);
	void __fastcall AddNewLine(void);
	__property System::UnicodeString AsString = {read=GetAsString};
public:
	/* TObject.Create */ inline __fastcall TWideBufferedString(void) : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCriticalSection : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	_RTL_CRITICAL_SECTION FSection;
	
public:
	__fastcall TCriticalSection(void);
	__fastcall virtual ~TCriticalSection(void);
	void __fastcall Enter(void);
	void __fastcall Leave(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Classes */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_CLASSES)
using namespace Virtualtrees::Classes;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_ClassesHPP
