// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - Unknwn.pas
// Kind: Pascal Unit
// Release date: 08-07-2012
// Language: ENU
//
// Version: 1.0.0.2
// Description: Requires Windows 2000 Pro or later. 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// 
// LastEdited by: Tony
// EditDate: updt 080712a, updt 290712b
//
// Remarks:   Delphi : The IUnknown entries of functions should be casted like this:
//            IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: Overall Check
// =============================================================================
// Source: unknwn.h
//
// Microsoft Windows Media Foundation
// Copyright (c) 1997-2012 Microsoft Corporation. All rights reserved
//==============================================================================
// The contents of this file are subject to the Mozilla Public
// License Version 1.1 (the "License"). you may not use this file
// except in compliance with the License. You may obtain a copy of
// the License at http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
//==============================================================================

unit Unknwn;


  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}


interface

uses
	Windows, ComObj, ActiveX, WTypes, MFPTypes;


const
  IID_IUnknown                         : TGUID = '{00000000-0000-0000-C000-000000000046}';
  IID_AsyncIUnknown                    : TGUID = '{000E0000-0000-0000-C000-000000000046}';
  IID_IClassFactory                    : TGUID = '{00000001-0000-0000-C000-000000000046}';

type
  PIUnknown = ^TIUnknown;
  IUnknown = interface;
  TIUnknown = IUnknown;

  PAsyncIUnknown = ^TAsyncIUnknown;
  AsyncIUnknown = interface;
  TAsyncIUnknown = AsyncIUnknown;

  PIClassFactory = ^TIClassFactory;
  IClassFactory = interface;
  TIClassFactory = IClassFactory;


  //Interface IUnknown
  {$EXTERNALSYM LPUNKNOWN}
  LPUNKNOWN = ^IUnknown;


//////////////////////////////////////////////////////////////////
// IID_IUnknown and all other system IIDs are provided in UUID.LIB
// Link that library in with your proxies, clients and servers
//////////////////////////////////////////////////////////////////


  //Interface IUnknown
  //Enables clients to get pointers to other interfaces on a given object through the QueryInterface method,
  //and manage the existence of the object through the AddRef and Release methods.
  //All other COM interfaces are inherited, directly or indirectly, from IUnknown.
  //Therefore, the three methods in IUnknown are the first entries in the VTable for every interface.
  //
  //You must implement IUnknown as part of every interface.
  //If you are using C++ multiple inheritance to implement multiple interfaces,
  //the various interfaces can share one implementation of IUnknown.
  //If you are using nested classes to implement multiple interfaces,
  //you must implement IUnknown once for each interface you implement.
  //
  //Delphi specific: The IUnknown entries of functions should be casted like this:
  //IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
  
  IUnknown = interface   //updt 290712b
  ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const riid: REFIID; ppvObject: PPointer): HResult; stdcall;
    function AddRef(): ULONG; stdcall;
    function Release(): ULONG; stdcall;
  end;

  //Interface AsyncIUnknown
  //Undocumented
  AsyncIUnknown = interface(IUnknown)
  ['{000e0000-0000-0000-C000-000000000046}']
    function Begin_QueryInterface(const riid: REFIID): HResult; stdcall;
    function Finish_QueryInterface(out pvObject: PPointer): HResult; stdcall;
    function Begin_AddRef(): HResult; stdcall;
    function Finish_AddRef(): ULONG; stdcall;
    function Begin_Release(): HResult; stdcall;
    function Finish_Release(): ULONG; stdcall;
  end;

  //Interface IClassFactory
  //Enables a class of objects to be created.
  //You must implement this interface for every class that you register in the system registry and
  //to which you assign a CLSID, so objects of that class can be created.
  //
  //After calling the CoGetClassObject function to get an IClassFactory interface pointer to the class object,
  //call the CreateInstance method of this interface to create an object.
  //It is not, however, always necessary to go through this process to create an object.
  //To create a single object, you can, instead, just call CoCreateInstance.
  //OLE also provides numerous helper functions (with names of the form OleCreateXxx) to create compound document objects.
  //
  //Call the LockServer method to keep the object server in memory and enhance performance only if you
  //intend to create more than one object of the specified class.
  
  IClassFactory = interface(IUnknown)
  ['{00000001-0000-0000-C000-000000000046}']
    function CreateInstance(const pUnkOuter: PIUnknown; const riid: REFIID; out ppvObject: PPointer): HResult; stdcall;
    function LockServer(const fLock: BOOL): HResult; stdcall;
  end;


//Additional Prototypes for ALL interfaces

//end of Additional Prototypes

implementation

end.
