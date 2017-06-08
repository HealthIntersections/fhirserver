Delphi/Pascal FHIR Reference Implementation
===========================================

This Delphi/Pascal reference implementation is maintained by Grahame Grieve 
(grahame@healthintersections.com.au) and used in the following tools:
* FHIR Reference Server
* Notepad++ Plug-in for FHIR
* FHIR Value Set Editor 

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
the project files may need to rebuilt for versions older than Delphi
Seattle

The library depends on msxml, so won't work on anything but windows

Free Pascal / Lazarus: this code doesn't compile under FPC because I can't
figure out how to maintain dual source around the Unicode issue. If anyone
wants to help, assistance will be welcome. 

FHIR Versions
-------------
This reference implementation supports 3 FHIR versions:
* FHIR DSTU 1 - no longer maintained actively (no longer compiling - needs week)
* FHIR DSTU 2 - stable supported release 
* FHIR DSTU 3 - stable supported release
* FHIR DSTU 4 - current trunk. This is unstable and may not work at times

To save implementers from confusion, you must add one of the following
defines to your project options: FHIR1, FHIR2, FHIR3, or FHIR4,
depending on which version you want to support (as well as including the 
correct files)

Note: this does mean you can't support more than one version of FHIR 
in the same executable, and is under further consideration.

Using the library 
-----------------

This library provides an object model for all FHIR resources, and 
parsers and serialisers for the XML and JSON formats, a client, 
and various utilities including validation.

To create an object model: resources or bundles, or supporting classes
can be created directly (TFhirPatient.create), or using appropriate
methods from TFhirFactory (FhirSupport.pas). See below for notes about
managing object life cycles.

To read a stream into an object model, create a TFHIRXmlParser, or
a TFHIRJsonParser (FhirParser.pas) (you must know which in advance - 
this should be known by the mime type). Then assign it's source 
property (a stream of any type is required), call "parse", and 
then check to see whether a resource or a bundle (or a taglist, 
in some circumstances) was decoded.

To write a stream, create an object model, and then create 
either a TFHIRXmlComposer, or a TFHIRJsonComposer, and call 
the compose methods with appropriate parameters.

There's some useful example code in FHIRTest.dpr


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

