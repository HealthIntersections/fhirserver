Delphi/Pascal FHIR Reference Implementation
===========================================

This Delphi/Pascal reference implementation is maintained by Grahame Grieve 
(grahame@healthintersections.com.au) and used in the following tools:
* FHIR Reference Server
* Notepad++ Plug-in for FHIR
* FHIR Tookit

There are a number of other commercial uses of the reference implementation.

For bug reports concerning the reference implementation, use the 
GitHub issues page at https://github.com/grahamegrieve/fhirserver/issues.

License
-------

The license is standard BSD-3:

Copyright (c) 2011+, HL7, Inc and Health Intersections Pty Ltd
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of HL7 nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.


Delphi / Lazarus Versions supported
-----------------------------------

As released, this code supports all unicode versions of delphi, though
the project files may need to rebuilt for versions older than Delphi 10.2

Free Pascal / Lazarus: this code doesn't compile under Lazarus at this; it's
work in progress. 

FHIR Versions
-------------
This reference implementation supports 3 FHIR versions:
* FHIR DSTU 2 - stable supported release 
* FHIR DSTU 3 - stable supported release
* FHIR DSTU 4 - current trunk. This is unstable and may not work at times

Using the library 
-----------------

This library provides an object model for all FHIR resources, and 
parsers and serialisers for the XML and JSON formats, a client, 
and various utilities including validation.

There are multiple ways to work with resources, and there are examples
of all them through out the library. 

Understanding the library
-------------------------

In general, these are the kind of things you do with the library:
* Create a worker context
* read a resource from a stream 
* create resources
* write a resource to a stream
* read and write resources from a server

You can choose to do these in several different ways; which way 
depends on whether you want to support multiple versions or not. 

Context
-------

Almost all the units in the library depend on a context. The context
keeps all the useful information that the library needs in the background,
including a factory that is capable of creating the various objects
used through out the library 

Some uses of the library (validation, some cross-version use) requires
the context to have content loaded from one (or more) of the FHIR 
distribution packages. 

Note that reading and writing resources as documented here does not require
any packages to be loaded.

Using the library for a single version
--------------------------------------

Choose a version, and include the units. Typically
 
   uses
     FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Factory;

Note that there's a long tail of unit dependencies to deal with, and 
other units than these may be required.

Next, create a context, with a factory

   var
     context : TFHIRWorkerContext;
     
   context := TFHIRWorkerContext.create(TFHIRFactory4.create)

Reading a resource from a stream:

  var
    r : Resource;
    p : TFHIRParser;
  
  p := context.factory.makeParser(context.link, ffJson, 'en');
  try
    r := p.parseResource(stream); 
  finally
    p.free;
  end;
  
See below for doco about the .link. ffJson is the format to read - ffXml and ffTurtle 
are also supported. 'en' is the language - an http Accept-Language string is supported. 
If you don't know the format of the stream, see FHIR.Base.Utilities.DetectFormat

Creating a resource:

  var
    r : Resource;
  
  r := TFHIRPatient.create;
  
Then just use the properties to access to object model of the resource. 
  
Writing a resource to a stream is similar to reading it:

  var
    r : Resource;
    c : TFHIRComposer;
  
  c := context.factory.makeComposer(context.link, ffJson, 'en', OutputStylePretty);
  try
    c.compose(r, stream); 
  finally
    c.free;
  end;
  
The one addition parameter specifies whether to produce a human readable resource or not. 

Reading and writing resources from the server:

  var
    r : TFHIRPatient;
    client : TFHIRClient;
  
  client := factory.makeClient(context.link, 'http://test/fhir.org/r4', ffJson) as TFHIRClient;
  try
    r := client.readResource(frtPatient, 'example') as TFHIRPatient;
    try
      // change R....
      client.updateResource(r).free;  // remember to .free the return if you're not interested in it.      
    finally
      r.free;
    end;
  finally
    client.free;
  end;


Working across versions
-----------------------

todo....


Object life-cycle management
----------------------------

The library uses it's own approach to managing object life 
cycles: manual reference counting.

Each object keeps it's own internal reference count,
and when a new pointer to the object is created, the programmer
must increment the count. When all references to the object 
are released, the object is automatically freed.

Question: Why not use interfaces?
Answers: 
* well, the outcome is kind of like how interfaces work, but without their polymorphism problems (can't cast an interface to a descendent interface)
* legacy, really. This approach was implemented prior to interfaces being available

The way it works is all implemented in TAdvObject

Using an object is easy: 

    var
      obj : TAdvObject; { or any descendent }
    begin
      obj := TAdvObject.create;
      try
        // do things with the object
      finally
        obj.free;
      end;
    end;

Note: you *never* call FreeAndNil(obj) - because FreeAndNil
calls TObject(obj).free, and it can't be overridden. So 
you can't ever call FreeAndNil when working with these objects.
It's painful, but if you look at the existing code, 
you just don't ever need to do it very often - use smaller
procedures, mostly.  (or use the := nil trick below)

The technique starts to get useful when you have to add the object to
a list:

    Procedure addToList(list : TAdvObjectList; params...);
    var
      obj : TAdvObject; { or any descendent }
    begin
      obj := TAdvObject.create;
      try
        // do things with the object and params
        list.add(obj.link);  // add to the reference count
      finally
        obj.free; // delete from the reference count, and free if no more references are left
      end;
    end;

This code won't leak - if the object can't be created, an exception
will be raised, and the object will be released. If it works, it'll be 
added to the list and the list will be the only owner - the .link
routine adds to the reference count. 

So once the object is in the list, and you want to work with it:

    procedure UseObject(list : TAdvObjectList; i : integer);
    var
      obj : TAdvObject;
    begin
      obj := list[i].link; // get a copy, and reference count it
      try
        // do stuff with the object. It's ok to keep 
        // doing stuff, even if it is removed from the list
        // while this is happening 
      finally
        obj.free; // release our reference to obj; free if this is the last reference
      end; 
    end;

The list automatically owns the object, and manages it's own reference to it
correctly. You don't have to increment the reference count when you use an
object:

    procedure UseObject(list : TAdvObjectList; i : integer);
    var
      obj : TAdvObject;
    begin
      obj := list[i]; 
      // do stuff with the object, but be quick:
      // if it gets removed from the list while you're using 
      // it (e.g. in a routine you call), then boom!
    end;


If you have properties that have this type, then you code them like this:

    Property Object_ : TAdvObject read FObject write SetObject;
  
and SetObject looks like this:

    Procedure TSomeObject.SetObject(value : TAdvObject);
    begin
      FObject.free; // release the reference we have (or just do nothing if FObject is nil(
      FObject := value;  
    end;

Then you use it like this:

    Procedure doSomething(obj : TAdvObject);
    var
      s : TSomeObject;
    begin
      s := TSomeObject.create;
      try
        s.object_ := obj.link; // as long as s lives, it keeps a copy of s.
        // do stuff with s
      finally
        s.free;
      end;
    end;

And in the TSomeObject Destructor:

    Destructor TSomeObject.destroy;
    begin
      // other stuff
      Object_ := nil; // equivalent to freeandNil(Object);
      inherited;
    end;

Note: this pattern arose experimentally seeking a reproducible
pattern that would make it easy to keep objects alive when you
need them, and not have any leaks, in a large programming team.
Once you're used to it, it's hard to think any other way.

These are the alias strings for the project options:

* 2: FHIRAuthMap=FHIRAuthMap2;FHIRConstants=FHIRConstants2;FHIRContext=FHIRContext2;FHIRIndexInformation=FHIRIndexInformation2;FHIRJavascriptReg=FHIRJavascriptReg2;FHIRMetaModel=FHIRMetaModel2;FHIRNarrativeGenerator=FHIRNarrativeGenerator2;FHIROperations=FHIROperations2;FHIRParserJson=FHIRParserJson2;FHIRParserTurtle=FHIRParserTurtle2;FHIRParserXml=FHIRParserXml2;FHIRPathNode=FHIRPathNode2;FHIRProfileUtilities=FHIRProfileUtilities2;FHIRResources=FHIRResources2;FHIRStructureMapUtilities=FHIRStructureMapUtilities2;FHIRTags=FHIRTags2;FHIRTestWorker=FHIRTestWorker2;FHIRTypes=FHIRTypes2;FHIRUtilities=FHIRUtilities2;FHIRValidator=FHIRValidator2;FhirOpBase=FhirOpBase2;FhirPath=FhirPath2;NarrativeGenerator=NarrativeGenerator2;QuestionnaireBuilder=QuestionnaireBuilder2;FHIRBaseX=FHIRBase2
* 3: FHIRAuthMap=FHIRAuthMap3;FHIRConstants=FHIRConstants3;FHIRContext=FHIRContext3;FHIRIndexInformation=FHIRIndexInformation3;FHIRJavascriptReg=FHIRJavascriptReg3;FHIRMetaModel=FHIRMetaModel3;FHIRNarrativeGenerator=FHIRNarrativeGenerator3;FHIROperations=FHIROperations3;FHIRParserJson=FHIRParserJson3;FHIRParserTurtle=FHIRParserTurtle3;FHIRParserXml=FHIRParserXml3;FHIRPathNode=FHIRPathNode3;FHIRProfileUtilities=FHIRProfileUtilities3;FHIRResources=FHIRResources3;FHIRStructureMapUtilities=FHIRStructureMapUtilities3;FHIRTags=FHIRTags3;FHIRTestWorker=FHIRTestWorker3;FHIRTypes=FHIRTypes3;FHIRUtilities=FHIRUtilities3;FHIRValidator=FHIRValidator3;FhirOpBase=FhirOpBase3;FhirPath=FhirPath3;NarrativeGenerator=NarrativeGenerator3;QuestionnaireBuilder=QuestionnaireBuilder3;FHIRBaseX=FHIRBase3
* 4: FHIRAuthMap=FHIRAuthMap4;FHIRConstants=FHIRConstants4;FHIRContext=FHIRContext4;FHIRIndexInformation=FHIRIndexInformation4;FHIRJavascriptReg=FHIRJavascriptReg4;FHIRMetaModel=FHIRMetaModel4;FHIRNarrativeGenerator=FHIRNarrativeGenerator4;FHIROperations=FHIROperations4;FHIRParserJson=FHIRParserJson4;FHIRParserTurtle=FHIRParserTurtle4;FHIRParserXml=FHIRParserXml4;FHIRPathNode=FHIRPathNode4;FHIRProfileUtilities=FHIRProfileUtilities4;FHIRResources=FHIRResources4;FHIRStructureMapUtilities=FHIRStructureMapUtilities4;FHIRTags=FHIRTags4;FHIRTestWorker=FHIRTestWorker4;FHIRTypes=FHIRTypes4;FHIRUtilities=FHIRUtilities4;FHIRValidator=FHIRValidator4;FhirOpBase=FhirOpBase4;FhirPath=FhirPath4;NarrativeGenerator=NarrativeGenerator4;QuestionnaireBuilder=QuestionnaireBuilder4;FHIRBaseX=FHIRBase4
