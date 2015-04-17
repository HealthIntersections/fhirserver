{
IndySOAP: Indy Soap Global Defines
}
{$I IdCompilerDefines.inc}


{$B-} // always use short circuit boolean logic

{=============================================================================
  Indy Defines
 =============================================================================}

{
  About Indy 9 and 10. IndySoap supports both Indy 9 and Indy 10.
  All tests pass for both versions. However you cannot use both
  versions at once - they have clashing unit names.

  Indy 9 is the default. To support Indy 10, enable this define.

  In addition to choosing the define, you must make sure that the
  matching indy version is on the path or in the project uses clause.
}
{$IFNDEF VER130}
  {$DEFINE INDY_V10}
{$ENDIF}

{$IFNDEF INDY_V10}
  {$IFDEF UNICODE}
     Only Indy v10 is supported for Unicode Versions of Delphi
  {$ENDIF}
{$ENDIF}

{=============================================================================
  Indy Soap Defines
 =============================================================================}

{$IFNDEF LINUX}
 {.$.DEFINE ID_SOAP_COMPRESSION}
{$ENDIF}

{ requires a working copy of ZLib.pas. We do not distribute this }

{
IndySoap generates dynamic machine code. The generated machine code is highly
quality checked, and not a risk. Windows defined a defense machanism called
DEP which is intended to stop accidental buffer overflows, and which prevents
the IndySoap machanism from working.

To resolve this, IndySoap uses specially allocated memory that is marked
as executable. however there's an necessary overhead in managing this which
has a measureable impact on peformance (up to 50% on the infrastructure). If
you'd rather turn DEP off for the process, and get the speed boost, define
this define

This only affects the server. The client will always use DEP safe code (doesn't
have the same performance hit there)
}
{$DEFINE NEED_DEP_TURNED_OFF}

{=============================================================================
  Functional Defines
 =============================================================================}

{$DEFINE ID_SOAP_SORT_PROPERTIES}

// indySoap performs faster if the properties of an object are sorted. This
// will increase the speed at which the XML is loaded. The more properties an
// object has, the faster this will work
//
// however some clients may need the order to be preserved. So you can turn
// sorting using this define. Default is to sort. IndySoap doesn't care about
// order itself.
//
// this is a define since this happens so early - before any application host
// code is executed
//

{=============================================================================
  Defines Relating to OpenXML
 =============================================================================}

{.$.DEFINE IDSOAP_USE_RENAMED_OPENXML}

// This define has been removed - too much is changing in OpenXML to
// make it relevent - the only version supported by indysoap is the one provided
// as part of indysoap

{=============================================================================
  Defines Relating to D6+/K2+ Enterprise features
 =============================================================================}

{ The general define VER140ENTERPRISE has been replaced by 2 defines,
ID_SOAP_WEBBROKER and ID_SOAP_RTTI_ITI }

{.$.DEFINE ID_SOAP_WEBBROKER}

{ This includes the TIdSoapWebBroker component which you can use to make
a web broker based indysoap server. You need to have a web broker enabled version
of Delphi to compile indysoap with this version enabled. (D6 Enterprise,
D7 Enterprise, Architect, or Studio, K2/K3 Enterprise)}

{.$.DEFINE ID_SOAP_ITI_RTTI}

{ this includes the functionality to build the ITI on the fly from the
 RTTI that delphi builds for interfaces. This requires D6+ and also the
 unit IntfInfo.pas. You need one of the following versions of Delphi for
 this: D6 Enterprise, D7 Enterprise, Architect, or Studio, K2/K3 Enterprise

 We recommend against using this approach. Reasons:
   1/ there is various bugs and incompatibilities in the various versions
      of intfinfo.pas.

   2/ This approach gives little control over the SOAP behaviour

   3/ We've never been able to install the indysoap package in D6 with
      this compile enabled. This package needs DclSoap. It needs CoreIde,
      that can't be built.... etc, etc.
}

{=============================================================================
  Defines Relating to Object Tracking
 =============================================================================}

{.$.C+}
  { if this is disabled, then IndySOAP will run with the project assertion
    setting. You can use this setting to make IndySOAP run with a different
    assertion checking policy to the project (would usually be, project on,
    IndySOAP off)}

{.$.DEFINE OBJECT_TRACKING}
  { Complete Object Tracking. A full list of Objects is maintained in
    a separate list. Objects are cross-compared with the list. Not only
    does this provide leak tracking on a class per class basis, but it
    allows for authoritative checking that the object still points to
    an actual live instant that it is meant to. In addition, a watch can
    be requested on an object so that an break point will be called when
    the object is freed. }

{$IFOPT C+}
  // you can have assertions without having object tracking
  {$DEFINE OBJECT_TRACKING}
{$ELSE}
  {$UNDEF OBJECT_TRACKING}
{$ENDIF}

{$IFNDEF LINUX}
{.$.DEFINE USE_ADV}
  { Internal to Kestral only }
{$ENDIF}



