# Pascal FHIR Reference Implementation

This Pascal reference implementation is maintained by Grahame Grieve 
(grahame@healthintersections.com.au) and used in the following tools:
* FHIR Reference Server
* FHIR Tookit

There are a number of other commercial uses of the reference implementation.

For bug reports concerning the reference implementation, use the 
GitHub issues page at https://github.com/grahamegrieve/fhirserver/issues.

## License

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

## Building this 

See [building the fhirserver](../build/readme.md)

## FHIR Versions

This reference implementation supports 4 FHIR versions:
* FHIR DSTU 2 - stable supported release 
* FHIR STU 3 - stable supported release
* FHIR R4 - stable supported release 
* FHIR R5 - current trunk. This is unstable and may not work at times

## Using the library

This library provides an object model for all FHIR resources, and 
parsers and serialisers for the XML and JSON formats, a client, 
and various utilities including validation.

There are multiple ways to work with resources, and there are examples
of all them through out the library. 

In general, these are the kind of things you do with the library:
* Create a worker context
* read a resource from source (stream | bytes | string) 
* create resources
* write a resource to source
* read and write resources from a server

You can choose to do these in several different ways; which way 
depends on whether you want to support multiple versions or not. 

### Context

Almost all the units in the library depend on a context. The context
keeps all the useful information that the library needs in the background,
including a factory that is capable of creating the various objects
used through out the library 

Some uses of the library (validation, some cross-version use) requires
the context to have content loaded from one (or more) of the FHIR 
distribution packages. 

Note that reading and writing resources as documented here does not require
any packages to be loaded.

### Using the libary for anything

Almost all the classes in the library inherit from TFslObject 
and make heavy use of manual reference counting using .link. See  [reference counting](fsl/readme.md) for doco about this. This is important - you don't always have to 
do manual reference counting yourself, but you have to know 
that the library always does. Used carefully, this will make 
it easy to prevent memory leaks. 

### Using the library for a single version

Choose a version, and include the units. Typically

```pas 
   uses
     fhir4_types, fhir4_resources, fhir4_context, fhir4_factory;
```

Other units than these may be required.

Next, create a context, with a factory

```pas 
   var
     context : TFHIRWorkerContext;
     
   context := TFHIRWorkerContext.Create(TFHIRFactory.create)
```

Reading a resource from a stream:

```pas 
  var
    r : TFHIRResource;
    p : TFHIRParser;
  
  p := context.factory.makeParser(context.link, ffJson, 'en');
  try
    r := p.parseResource(stream); 
  finally
    p.free;
  end;
```
  
See [reference counting](fsl/readme.md) for doco about the .link. 

ffJson is the format to read - ffXml and ffTurtle are also supported. 'en' is the language - an http Accept-Language string is supported, or use defLang()

If you don't know the format of the stream, see fhir_utilities.DetectFormat

Creating a resource:

```pas
  var
    r : TFHIRPatient;
  
  r := TFHIRPatient.Create;
```

Then just use the properties to access to object model of the resource. 
  
Writing a resource to a stream is similar to reading it:

```pas
  var
    r : TFHIRPatient;
    c : TFHIRComposer;
  
  c := context.factory.makeComposer(context.link, ffJson, 'en', OutputStylePretty);
  try
    c.compose(r, stream); 
  finally
    c.free;
  end;
```

The one addition parameter specifies whether to produce a human readable resource or not. 

Reading and writing resources from the server:

```pas
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
```

Working across versions
-----------------------

If you don't have a fixed version to target, there's several different ways 
to write code that works across versions. The first is to $IFDEF the units:

```pas
uses
  {$IFDEF USE_R5}
  fhir5_types, fhir5_resources, fhir5_context, fhir5_factory;
  {$ELSE}
  fhir4_types, fhir4_resources, fhir4_context, fhir4_factory;
  {$ENDIF}
```

Then for code like this:

```pas
var
  r : TFHIRResource;
  p : TFHIRParser;
begin  
  p := context.factory.makeParser(context.link, ffJson, 'en');
  try
    r := p.parseResource(stream); 
  finally
    p.free;
  end;
end;
```

This code will work with either R4 or R5, depending on which units 
are included. Note that the properties of the resources and data types 
changes across versions, so you might have to $IFDEF quite a lot of the 
code that works with the objects. 

Alternatively, just work with the fhir_ units - these are version independent. 

```pas
uses
  fhir_objects, fhir_context, fhir_factory;

var
  r : TFHIRResourceV;
  p : TFHIRParserV;
begin  
  p := context.factory.makeParser(context.link, ffJson, 'en');
  try
    r := p.parseResource(stream); 
  finally
    p.free;
  end;
end;
```

The code looks the same, but TFHIRResourceV is the abstract ancestor of all 
resources in all versions - you don't have direct access to it's properties 
unless you type cast to a specific descendent. This is a common way to work 
with resources in a generic way, but pretty painful as a way to deal with 
the content in the resources.

Finally, there's a version independent wrapper framework 

```pas
uses
  fhir_objects, fhir_common, fhir_factory;

var
  r : TFHIRResourceV;
  p : TFHIRPatientW; // W stands for wrapper
begin  
  r := loadResourceFromSomewhere; 
  try
    p := factory.wrapPatient(r.link);
    try
      showmessage(p.name.asText);
    finally
      p.free;
    end;
  finally
    r.free;
  end;
end;
```

The TFHIRResourceW classes are implemented in all the versions, and whatever
you do with them just 'works' for which version the underlying resource has. 
Note that adding the to TFHIRResourceW is tiresome, so only a veru limited 
subset of the resource model is supported. Contributions are welcome.

When writing code that crosses versions, it's not unusual to mix all these 
different techniques. 
