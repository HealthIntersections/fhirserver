{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}
program FHIRTest;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\reference-platform\support\FastMM4.pas',
  FastMM4Messages in '..\reference-platform\support\FastMM4Messages.pas',
  SysUtils,
  Classes,
  ActiveX,
  IdSSLOpenSSLHeaders,
  StringSupport in '..\reference-platform\support\StringSupport.pas',
  MathSupport in '..\reference-platform\support\MathSupport.pas',
  DecimalSupport in '..\reference-platform\support\DecimalSupport.pas',
  GUIDSupport in '..\reference-platform\support\GUIDSupport.pas',
  AdvFactories in '..\reference-platform\support\AdvFactories.pas',
  FileSupport in '..\reference-platform\support\FileSupport.pas',
  MemorySupport in '..\reference-platform\support\MemorySupport.pas',
  DateSupport in '..\reference-platform\support\DateSupport.pas',
  ErrorSupport in '..\reference-platform\support\ErrorSupport.pas',
  SystemSupport in '..\reference-platform\support\SystemSupport.pas',
  ThreadSupport in '..\reference-platform\support\ThreadSupport.pas',
  EncodeSupport in '..\reference-platform\support\EncodeSupport.pas',
  AdvControllers in '..\reference-platform\support\AdvControllers.pas',
  AdvPersistents in '..\reference-platform\support\AdvPersistents.pas',
  AdvObjects in '..\reference-platform\support\AdvObjects.pas',
  AdvExceptions in '..\reference-platform\support\AdvExceptions.pas',
  AdvFilers in '..\reference-platform\support\AdvFilers.pas',
  ColourSupport in '..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\support\CurrencySupport.pas',
  AdvPersistentLists in '..\reference-platform\support\AdvPersistentLists.pas',
  AdvObjectLists in '..\reference-platform\support\AdvObjectLists.pas',
  AdvItems in '..\reference-platform\support\AdvItems.pas',
  AdvCollections in '..\reference-platform\support\AdvCollections.pas',
  AdvIterators in '..\reference-platform\support\AdvIterators.pas',
  AdvClassHashes in '..\reference-platform\support\AdvClassHashes.pas',
  AdvHashes in '..\reference-platform\support\AdvHashes.pas',
  HashSupport in '..\reference-platform\support\HashSupport.pas',
  AdvStringHashes in '..\reference-platform\support\AdvStringHashes.pas',
  AdvProfilers in '..\reference-platform\support\AdvProfilers.pas',
  AdvStringIntegerMatches in '..\reference-platform\support\AdvStringIntegerMatches.pas',
  AdvStreams in '..\reference-platform\support\AdvStreams.pas',
  AdvParameters in '..\reference-platform\support\AdvParameters.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\reference-platform\support\AdvThreads.pas',
  AdvSignals in '..\reference-platform\support\AdvSignals.pas',
  AdvSynchronizationRegistries in '..\reference-platform\support\AdvSynchronizationRegistries.pas',
  AdvTimeControllers in '..\reference-platform\support\AdvTimeControllers.pas',
  AdvIntegerMatches in '..\reference-platform\support\AdvIntegerMatches.pas',
  AdvBuffers in '..\reference-platform\support\AdvBuffers.pas',
  BytesSupport in '..\reference-platform\support\BytesSupport.pas',
  AdvStringBuilders in '..\reference-platform\support\AdvStringBuilders.pas',
  AdvFiles in '..\reference-platform\support\AdvFiles.pas',
  AdvLargeIntegerMatches in '..\reference-platform\support\AdvLargeIntegerMatches.pas',
  AdvStringLargeIntegerMatches in '..\reference-platform\support\AdvStringLargeIntegerMatches.pas',
  AdvStringLists in '..\reference-platform\support\AdvStringLists.pas',
  AdvCSVFormatters in '..\reference-platform\support\AdvCSVFormatters.pas',
  AdvTextFormatters in '..\reference-platform\support\AdvTextFormatters.pas',
  AdvFormatters in '..\reference-platform\support\AdvFormatters.pas',
  AdvCSVExtractors in '..\reference-platform\support\AdvCSVExtractors.pas',
  AdvTextExtractors in '..\reference-platform\support\AdvTextExtractors.pas',
  AdvExtractors in '..\reference-platform\support\AdvExtractors.pas',
  AdvCharacterSets in '..\reference-platform\support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\reference-platform\support\AdvOrdinalSets.pas',
  AdvStreamReaders in '..\reference-platform\support\AdvStreamReaders.pas',
  AdvStringStreams in '..\reference-platform\support\AdvStringStreams.pas',
  DateAndTime in '..\reference-platform\support\DateAndTime.pas',
  KDate in '..\reference-platform\support\KDate.pas',
  HL7V2DateSupport in '..\reference-platform\support\HL7V2DateSupport.pas',
  AdvStringMatches in '..\reference-platform\support\AdvStringMatches.pas',
  ParseMap in '..\reference-platform\support\ParseMap.pas',
  MsXmlParser in '..\reference-platform\support\MsXmlParser.pas',
  AdvMemories in '..\reference-platform\support\AdvMemories.pas',
  XMLBuilder in '..\reference-platform\support\XMLBuilder.pas',
  AdvWinInetClients in '..\reference-platform\support\AdvWinInetClients.pas',
  MsXmlBuilder in '..\reference-platform\support\MsXmlBuilder.pas',
  TextUtilities in '..\reference-platform\support\TextUtilities.pas',
  AdvVCLStreams in '..\reference-platform\support\AdvVCLStreams.pas',
  AdvXmlBuilders in '..\reference-platform\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\reference-platform\support\AdvXMLEntities.pas',
  AdvJSON in '..\reference-platform\support\AdvJSON.pas',
  AfsResourceVolumes in '..\reference-platform\support\AfsResourceVolumes.pas',
  AfsVolumes in '..\reference-platform\support\AfsVolumes.pas',
  AfsStreamManagers in '..\reference-platform\support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\reference-platform\support\AdvObjectMatches.pas',
  AdvStringObjectMatches in '..\reference-platform\support\AdvStringObjectMatches.pas',
  JWT in '..\reference-platform\support\JWT.pas',
  HMAC in '..\reference-platform\support\HMAC.pas',
  libeay32 in '..\reference-platform\support\libeay32.pas',
  DigitalSignatures in '..\reference-platform\support\DigitalSignatures.pas',
  XMLSupport in '..\reference-platform\support\XMLSupport.pas',
  InternetFetcher in '..\reference-platform\support\InternetFetcher.pas',
  SCIMObjects in '..\reference-platform\SCIMObjects.pas',
  FHIRLang in '..\reference-platform\FHIRLang.pas',
  FHIRBase in '..\reference-platform\FHIRBase.pas',
  FHIRParserBase in '..\reference-platform\FHIRParserBase.pas',
  FHIRConstants in '..\reference-platform\gen21\FHIRConstants.pas',
  FHIRTypes in '..\reference-platform\gen21\FHIRTypes.pas',
  FHIRResources in '..\reference-platform\gen21\FHIRResources.pas',
  FHIRParser in '..\reference-platform\gen21\FHIRParser.pas',
  FHIRSupport in '..\reference-platform\FHIRSupport.pas',
  FHIRUtilities in '..\reference-platform\FHIRUtilities.pas',
  FHIRDigitalSignatures in '..\reference-platform\FHIRDigitalSignatures.pas',
  AdvNames in '..\reference-platform\support\AdvNames.pas',
  OIDSupport in '..\reference-platform\support\OIDSupport.pas',
  FHIRValidator in '..\reference-platform\FHIRValidator.pas',
  AltovaXMLLib_TLB in '..\reference-platform\support\AltovaXMLLib_TLB.pas',
  AdvZipReaders in '..\reference-platform\support\AdvZipReaders.pas',
  AdvNameBuffers in '..\reference-platform\support\AdvNameBuffers.pas',
  AdvZipDeclarations in '..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\support\AdvZipParts.pas',
  AdvZipUtilities in '..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\support\AdvZipWorkers.pas',
  MimeMessage in '..\reference-platform\support\MimeMessage.pas',
  kCritSct in '..\reference-platform\support\kCritSct.pas',
  AdvGenericsTests in '..\reference-platform\support\AdvGenericsTests.pas',
  AdvGenerics in '..\reference-platform\support\AdvGenerics.pas',
  FHIRSecurity in '..\reference-platform\FHIRSecurity.pas',
  FHIRTags in '..\reference-platform\FHIRTags.pas',
  FhirPath in '..\reference-platform\FhirPath.pas',
  FHIRProfileUtilities in '..\reference-platform\FHIRProfileUtilities.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas';

procedure SaveStringToFile(s : AnsiString; fn : String);
var
  f : TFileStream;
begin
  f := TFileStream.Create(fn, fmCreate);
  try
    f.Write(s[1], length(s));
  finally
    f.free;
  end;
end;


procedure Roundtrip(Source, Dest : String);
var
  f : TFileStream;
  m : TMemoryStream;
  p : TFHIRParser;
  c : TFHIRComposer;
  r : TFhirResource;
begin
  r := nil;
  try
    p := TFHIRXmlParser.Create('en');
    try
      p.ParserPolicy := xppDrop;
      f := TFileStream.Create(source, fmopenRead,+ fmShareDenyWrite);
      try
        p.source := f;
        p.Parse;
        r := p.resource.Link;
      finally
        f.Free;
      end;
    finally
      p.free;
    end;
    m := TMemoryStream.Create;
    try

      c := TFHIRJsonComposer.Create('en');
      try
        TFHIRJsonComposer(c).Comments := true;
        c.Compose(m, r, true, nil);
      finally
        c.free;
      end;
      m.Position := 0;
      m.SaveToFile(ChangeFileExt(dest, '.json'));
      m.Position := 0;
      r.Free;
      r := nil;
      p := TFHIRJsonParser.Create('en');
      try
        p.source := m;
        p.Parse;
        r := p.resource.Link;
      finally
        p.Free;
      end;
    finally
      m.Free;
    end;

    f := TFileStream.Create(dest, fmCreate);
    try
      c := TFHIRXMLComposer.Create('en');
      try
        c.Compose(f, r, true, nil);
      finally
        c.free;
      end;
    finally
      f.free;
    end;
    f := TFileStream.Create(dest+'.rdf', fmCreate);
    try
      c := TFHIRRDFComposer.Create('en');
      try
        TFHIRRDFComposer(c).URL := 'http://hl7.org/fhir/Condition/f202';
        c.Compose(f, r, true, nil);
      finally
        c.free;
      end;
    finally
      f.free;
    end;
    f := TFileStream.Create(dest+'.rdf1', fmCreate);
    try
      c := TFHIRRDFComposer.Create('en');
      try
        TFHIRRDFComposer(c).RDFFormat := rdfNTriple;
        TFHIRRDFComposer(c).URL := 'http://hl7.org/fhir/Condition/f202';
        c.Compose(f, r, true, nil);
      finally
        c.free;
      end;
    finally
      f.free;
    end;
  finally
    r.Free;
  end;

//  IdSoapXmlCheckDifferent(source, dest);
end;

procedure roundTripDirectory(pathSource, pathDest : String);
var
  SR: TSearchRec;
  s : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(PathSource) + '*.xml', faAnyFile, SR) = 0 then
  begin
    repeat
      s := copy(SR.Name, 1, pos('.', SR.Name)-1);
      if (SR.Attr <> faDirectory) and (copy(s, length(s)-1, 2) <> '-d') then
      begin
        Writeln(SR.name);
        Roundtrip(IncludeTrailingPathDelimiter(pathSource)+SR.Name, IncludeTrailingPathDelimiter(pathDest)+s+'-d'+ExtractFileExt((SR.Name)));
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function ConfigureDigSig(dsig : TDigitalSigner; certpath, certtype : String) : TSignatureMethod;
begin
  if certtype = 'rsa' then
  begin
    dsig.KeyFile := IncludeTrailingPathDelimiter(certpath)+'rsa_2048.pem';
    dsig.KeyPassword := 'fhir';
    result := sdXmlRSASha256;
  end
  else if certtype = 'dsa' then
  begin
    dsig.KeyFile := IncludeTrailingPathDelimiter(certpath)+'dsa_1024.pem';
    dsig.KeyPassword := 'fhir';
    result := sdXmlDSASha256;
  end
  else if certtype = 'ecdsa' then
  begin
    dsig.KeyFile := IncludeTrailingPathDelimiter(certpath)+'ecdsa_priv.pem';
//    dsig.CertFile := IncludeTrailingPathDelimiter(certpath)+'ecdsa_pub.pem';
    result := sdXmlRSASha256;
  end;
end;

procedure signProvenance(filename, certpath, certtype : String);
begin
  raise Exception.Create('Not Done Yet');
{  // ok, first we look at the filename, and see what it is.
  p := TFHIRXmlParser.Create('en');
  try
    p.ParserPolicy := xppDrop;
    f := TFileStream.Create(filename, fmopenRead,+ fmShareDenyWrite);
    try
      p.source := f;
      p.Parse;
      if p.feed <> nil then
        sigtype := 0
      else if p.resource is TFhirProvenance then
        sigtype := 1
      else
        raise Exception.Create('Do not know how to sign a '+CODES_TFhirResourceType[p.resource.ResourceType]);
    finally
      f.Free;
    end;
  finally
    p.free;
  end;
}
end;

procedure signAtom(filename, certpath, certtype : String);
var
  dsig : TDigitalSigner;
  method : TSignatureMethod;
begin
  dsig := TDigitalSigner.Create;
  try
    BytesToFile(dsig.signEnveloped(FileToBytes(filename), ConfigureDigSig(dsig, certPath, certType), true), filename);
  finally
    dsig.Free;
  end;
end;

procedure verify(filename, certificate, password : String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure dotest(src, dst : String; names : TStringList);
var
  s : String;
begin
  for s in names do
    try
      roundtrip(src+s+'.xml', dst+s.Replace('\', '_')+'.pascal.xml');
    except
      on e : exception do
        raise Exception.Create('Error Processing '+s+': '+e.message);
    end;
end;

procedure DoBuildEntry;
var
  certpath, certtype, password : String;
  f : System.Text;
  st : TStringList;
  src, dst, s : String;
begin
  try
    CoInitialize(nil);
    IdSSLOpenSSLHeaders.load;
//    LoadEAYExtensions;
//    ERR_load_crypto_strings;
//    OpenSSL_add_all_algorithms;
    try
      if (paramstr(1) = '-signatom') then
      begin
        if not FindCmdLineSwitch('certpath', certpath) then
          raise Exception.Create('No certificate provided');
        if not FindCmdLineSwitch('certtype', certtype) then
          raise Exception.Create('No certificate provided');
//        writeln('-signatom '+paramstr(2)+' -certpath '+certpath+' -certtype '+certtype);
        signAtom(paramstr(2), certpath, certtype);
      end
      else if (paramstr(1) = '-signprovenance') then
      begin
        raise Exception.Create('Not Done Yet');
      end
      else if (paramstr(1) = '-tests') then
      begin
        try
          AssignFile(f, paramstr(2));
          reset(f);
          st :=  TStringList.Create;
          try
            readln(f, src);
            readln(f, dst);
            while not (eof(f)) do
            begin
              readln(f, s);
              st.Add(s);
            end;
            dotest(src, dst, st);
          finally
            st.Free;
          end;
          CloseFile(f);
          AssignFile(f, paramstr(3));
          Rewrite(f);
          write(f, 'ok');
          closeFile(f);
        except
          on e:exception do
          begin
            AssignFile(f, paramstr(3));
            Rewrite(f);
            write(f, e.Message);
            closeFile(f);
          end;
        end;
      end
      else if (paramstr(1) = '-verify') then
      begin
  //      FindCmdLineSwitch('cert', cert);
  //      FindCmdLineSwitch('password', password);
  //      verify(paramstr(2), cert, password);
      end
      else if DirectoryExists(ParamStr(2)) and DirectoryExists(Paramstr(1)) then
        roundTripDirectory(Paramstr(1), ParamStr(2))
      else
      begin
        if (ParamStr(1) = '') or (ParamStr(2) = '') or not FileExists(paramstr(1)) then
          raise Exception.Create('Provide input and output file names');
        roundTrip(paramStr(1), paramStr(2));
      end;
    except
      on e:exception do
        SaveStringToFile(AnsiString(e.Message), ParamStr(2)+'.err');
    end;
  finally
    UnloadEAYExtensions;
    CoUninitialize;
  end;
end;

begin
  IdOpenSSLSetLibPath(ExtractFilePath(paramstr(0)));
  if paramstr(1) = '-test' then
    TAdvGenericsTests.execute
  else
    DoBuildEntry;
end.
