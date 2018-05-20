// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.Actions.pas' rev: 32.00 (Windows)

#ifndef Virtualtrees_ActionsHPP
#define Virtualtrees_ActionsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <VirtualTrees.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Actions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualTreeAction;
class DELPHICLASS TVirtualTreePerItemAction;
class DELPHICLASS TVirtualTreeCheckAll;
class DELPHICLASS TVirtualTreeUncheckAll;
class DELPHICLASS TVirtualTreeSelectAll;
class DELPHICLASS TVirtualTreeForSelectedAction;
class DELPHICLASS TVirtualTreeCopy;
class DELPHICLASS TVirtualTreeCut;
class DELPHICLASS TVirtualTreePaste;
class DELPHICLASS TVirtualTreeDelete;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVirtualTreeAction : public Vcl::Actnlist::TCustomAction
{
	typedef Vcl::Actnlist::TCustomAction inherited;
	
private:
	Virtualtrees::TBaseVirtualTree* fTree;
	bool fTreeAutoDetect;
	System::Classes::TNotifyEvent fOnAfterExecute;
	bool __fastcall GetSelectedOnly(void);
	void __fastcall SetSelectedOnly(const bool Value);
	
protected:
	Virtualtrees::TVirtualNodeStates fFilter;
	void __fastcall SetControl(Virtualtrees::TBaseVirtualTree* Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall DoAfterExecute(void);
	__property bool SelectedOnly = {read=GetSelectedOnly, write=SetSelectedOnly, default=0};
	
public:
	virtual bool __fastcall HandlesTarget(System::TObject* Target);
	virtual void __fastcall UpdateTarget(System::TObject* Target);
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
	
__published:
	__fastcall virtual TVirtualTreeAction(System::Classes::TComponent* AOwner);
	__property Virtualtrees::TBaseVirtualTree* Control = {read=fTree, write=SetControl};
	__property System::Classes::TNotifyEvent OnAfterExecute = {read=fOnAfterExecute, write=fOnAfterExecute};
	__property Caption = {default=0};
	__property Enabled = {default=1};
	__property HelpContext = {default=0};
	__property HelpType = {default=0};
	__property Hint = {default=0};
	__property ImageIndex = {default=-1};
	__property ShortCut = {default=0};
	__property SecondaryShortCuts;
	__property Visible = {default=1};
	__property OnHint;
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeAction(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreePerItemAction : public TVirtualTreeAction
{
	typedef TVirtualTreeAction inherited;
	
private:
	System::Classes::TNotifyEvent fOnBeforeExecute;
	
protected:
	Virtualtrees::_di_TVTGetNodeProc fToExecute;
	void __fastcall DoBeforeExecute(void);
	
public:
	__fastcall virtual TVirtualTreePerItemAction(System::Classes::TComponent* AOwner);
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
	
__published:
	__property System::Classes::TNotifyEvent OnBeforeExecute = {read=fOnBeforeExecute, write=fOnBeforeExecute};
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreePerItemAction(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeCheckAll : public TVirtualTreePerItemAction
{
	typedef TVirtualTreePerItemAction inherited;
	
protected:
	Virtualtrees::TCheckState fDesiredCheckState;
	
public:
	__fastcall virtual TVirtualTreeCheckAll(System::Classes::TComponent* AOwner);
	
__published:
	__property SelectedOnly = {default=0};
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeCheckAll(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeUncheckAll : public TVirtualTreeCheckAll
{
	typedef TVirtualTreeCheckAll inherited;
	
public:
	__fastcall virtual TVirtualTreeUncheckAll(System::Classes::TComponent* AOwner);
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeUncheckAll(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeSelectAll : public TVirtualTreeAction
{
	typedef TVirtualTreeAction inherited;
	
public:
	virtual void __fastcall UpdateTarget(System::TObject* Target);
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
__published:
	/* TVirtualTreeAction.Create */ inline __fastcall virtual TVirtualTreeSelectAll(System::Classes::TComponent* AOwner) : TVirtualTreeAction(AOwner) { }
	
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeSelectAll(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeForSelectedAction : public TVirtualTreeAction
{
	typedef TVirtualTreeAction inherited;
	
public:
	__fastcall virtual TVirtualTreeForSelectedAction(System::Classes::TComponent* AOwner);
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeForSelectedAction(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeCopy : public TVirtualTreeForSelectedAction
{
	typedef TVirtualTreeForSelectedAction inherited;
	
public:
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
public:
	/* TVirtualTreeForSelectedAction.Create */ inline __fastcall virtual TVirtualTreeCopy(System::Classes::TComponent* AOwner) : TVirtualTreeForSelectedAction(AOwner) { }
	
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeCopy(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeCut : public TVirtualTreeForSelectedAction
{
	typedef TVirtualTreeForSelectedAction inherited;
	
public:
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
public:
	/* TVirtualTreeForSelectedAction.Create */ inline __fastcall virtual TVirtualTreeCut(System::Classes::TComponent* AOwner) : TVirtualTreeForSelectedAction(AOwner) { }
	
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeCut(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreePaste : public TVirtualTreeForSelectedAction
{
	typedef TVirtualTreeForSelectedAction inherited;
	
public:
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
public:
	/* TVirtualTreeForSelectedAction.Create */ inline __fastcall virtual TVirtualTreePaste(System::Classes::TComponent* AOwner) : TVirtualTreeForSelectedAction(AOwner) { }
	
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreePaste(void) { }
	
};


class PASCALIMPLEMENTATION TVirtualTreeDelete : public TVirtualTreeForSelectedAction
{
	typedef TVirtualTreeForSelectedAction inherited;
	
public:
	virtual void __fastcall ExecuteTarget(System::TObject* Target);
public:
	/* TVirtualTreeForSelectedAction.Create */ inline __fastcall virtual TVirtualTreeDelete(System::Classes::TComponent* AOwner) : TVirtualTreeForSelectedAction(AOwner) { }
	
public:
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TVirtualTreeDelete(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Actions */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_ACTIONS)
using namespace Virtualtrees::Actions;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_ActionsHPP
