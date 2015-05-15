// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.WorkerThread.pas' rev: 29.00 (Windows)

#ifndef Virtualtrees_WorkerthreadHPP
#define Virtualtrees_WorkerthreadHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <VirtualTrees.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtrees
{
namespace Workerthread
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TWorkerThread;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TWorkerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	Virtualtrees::TBaseVirtualTree* FCurrentTree;
	System::Classes::TThreadList* FWaiterList;
	unsigned FRefCount;
	
protected:
	void __fastcall CancelValidation(Virtualtrees::TBaseVirtualTree* Tree);
	virtual void __fastcall Execute(void);
	
public:
	__fastcall TWorkerThread(bool CreateSuspended);
	__fastcall virtual ~TWorkerThread(void);
	void __fastcall AddTree(Virtualtrees::TBaseVirtualTree* Tree);
	void __fastcall RemoveTree(Virtualtrees::TBaseVirtualTree* Tree);
	__property Virtualtrees::TBaseVirtualTree* CurrentTree = {read=FCurrentTree};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TWorkerThread* WorkerThread;
extern DELPHI_PACKAGE NativeUInt WorkEvent;
extern DELPHI_PACKAGE void __fastcall AddThreadReference(void);
extern DELPHI_PACKAGE void __fastcall ReleaseThreadReference(Virtualtrees::TBaseVirtualTree* Tree);
}	/* namespace Workerthread */
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES_WORKERTHREAD)
using namespace Virtualtrees::Workerthread;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Virtualtrees_WorkerthreadHPP
