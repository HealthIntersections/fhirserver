unit FHIRValueSetChecker;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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


interface

uses
  SysUtils, Classes,
  AdvObjects, AdvStringObjectMatches,
  FHIRTypes, FHIRResources, FHIRUtilities, FHIRBase,
  TerminologyServices, TerminologyServerStore;

Type
  TValueSetChecker = class (TAdvObject)
  private
    FStore : TTerminologyServerStore;
    FOthers : TAdvStringObjectMatch; // checkers or code system providers
    fvs : TFHIRValueSet;
    FId: String;
    FProfile : TFhirExpansionProfile;
    function check(system, version, code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean; overload;
    function findCode(cs : TFhirCodeSystem; code: String; list : TFhirCodeSystemConceptList; displays : TStringList; out isabstract : boolean): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean;
//    function rule(op : TFhirOperationOutcome; severity : TFhirIssueSeverityEnum; test : boolean; code : TFhirIssueTypeEnum; msg : string):boolean;
    function getName: String;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeInclude; var cs: TCodeSystemProvider);
  public
    constructor Create(store : TTerminologyServerStore; id : String); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    procedure prepare(vs : TFHIRValueSet; profile : TFhirExpansionProfile);

    function check(system, version, code : String; abstractOk : boolean) : boolean; overload;
    function check(coding : TFhirCoding; abstractOk : boolean): TFhirParameters; overload;
    function check(code: TFhirCodeableConcept; abstractOk : boolean) : TFhirParameters; overload;
  end;

implementation

{ TValueSetChecker }

constructor TValueSetChecker.create(store : TTerminologyServerStore; id : string);
begin
  Create;
  FStore := store;
  FId := id;
  FOthers := TAdvStringObjectMatch.create;
  FOthers.PreventDuplicates;
end;

destructor TValueSetChecker.destroy;
begin
  FVs.Free;
  FOthers.Free;
  FStore.Free;
  Fprofile.Free;
  inherited;
end;

procedure TValueSetChecker.prepare(vs: TFHIRValueSet; profile : TFhirExpansionProfile);
var
  cs : TCodeSystemProvider;
  cc : TFhirValueSetComposeInclude;
  {$IFDEF FHIR2}
  i, j : integer;
  other : TFHIRValueSet;
  checker : TValueSetChecker;
  {$ENDIF}
begin
  FProfile := profile.Link;

  vs.checkNoImplicitRules('ValueSetChecker.prepare', 'ValueSet');
  vs.checkNoModifiers('ValueSetChecker.prepare', 'ValueSet');
  if (vs = nil) then

  else
  begin
    FVs := vs.link;
    {$IFDEF FHIR2}
    if fvs.codeSystem <> nil then
    begin
      fvs.codeSystem.checkNoModifiers('ValueSetChecker.prepare', 'CodeSystem');
      FOthers.Add(fvs.codeSystem.system, TFhirCodeSystemProvider.create(TFHIRCodeSystemEntry.Create(FVs.Link)));
    end;
    {$ENDIF}
    if (fvs.compose <> nil) then
    begin
      fvs.compose.checkNoModifiers('ValueSetChecker.prepare', 'compose', []);
    {$IFDEF FHIR2}
      for i := 0 to fvs.compose.importList.Count - 1 do
      begin
        other := FStore.getValueSetByUrl(fvs.compose.importList[i].value);
        try
          if other = nil then
            raise ETerminologyError.create('Unable to find value set '+fvs.compose.importList[i].value);
          checker := TValueSetChecker.create(Fstore.link, other.url);
          try
            checker.prepare(other, profile);
            FOthers.Add(fvs.compose.importList[i].value, checker.Link);
          finally
            checker.free;
          end;
        finally
          other.free;
        end;
      end;
    {$ENDIF}
      for cc in fvs.compose.includeList do
        prepareConceptSet('include', cc, cs);
      for cc in fvs.compose.excludeList do
        prepareConceptSet('exclude', cc, cs);
    end;
  end;
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeInclude; var cs: TCodeSystemProvider);
var
  {$IFNDEF FHIR2}
  ivs: TFhirUri;
  other: TFhirValueSet;
  {$ENDIF}
  checker: TValueSetChecker;
  ccf: TFhirValueSetComposeIncludeFilter;
begin
  cc.checkNoModifiers('ValueSetChecker.prepare', desc, []);
  {$IFNDEF FHIR2}
  for ivs in cc.valueSetList do
  begin
    other := FStore.getValueSetByUrl(ivs.value);
    try
      if other = nil then
        raise ETerminologyError.create('Unable to find value set ' + ivs.value);
      checker := TValueSetChecker.create(Fstore.link, other.url);
      try
        checker.prepare(other, FProfile);
        FOthers.Add(ivs.value, checker.Link);
      finally
        checker.free;
      end;
    finally
      other.free;
    end;
  end;
  {$ENDIF}
  if not FOthers.ExistsByKey(cc.system) then
    FOthers.Add(cc.system, FStore.getProvider(cc.system, cc.version, Fprofile));
  cs := TCodeSystemProvider(FOthers.matches[cc.system]);
  for ccf in cc.filterList do
  begin
    ccf.checkNoModifiers('ValueSetChecker.prepare', desc + '.filter', []);
    if not (('concept' = ccf.property_) and (ccf.Op = FilterOperatorIsA)) then
      if not cs.doesFilter(ccf.property_, ccf.Op, ccf.value) then
        raise ETerminologyError.create('The filter "' + ccf.property_ + ' ' + CODES_TFhirFilterOperatorEnum[ccf.Op] + ' ' + ccf.value + '" was not understood in the context of ' + cs.system(nil));
  end;
end;

function TValueSetChecker.findCode(cs : TFhirCodeSystem; code: String; list : TFhirCodeSystemConceptList; displays : TStringList; out isabstract : boolean): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].code) then
    begin
      result := true;
      {$IFNDEF FHIR2}
      isabstract := cs.isAbstract(list[i]);
      {$ELSE}
      isabstract := list[i].abstract;
      {$ENDIF}
      displays.Add(list[i].display);
      exit;
    end;
    if findCode(cs, code, list[i].conceptList, displays, isabstract) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TValueSetChecker.getName: String;
begin
  if (fvs <> nil) then
    result := fvs.name
  else
    result := '??';
end;

function TValueSetChecker.check(system, version, code: String; abstractOk : boolean): boolean;
var
  list : TStringList;
  msg : string;
begin
  list := TStringList.Create;
  try
    list.Duplicates := dupIgnore;
    list.CaseSensitive := false;
    result := check(system, version, code, abstractOk, list, msg);
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(system, version, code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean;
var
//  checker : TValueSetChecker;
  cs : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  cc : TFhirValueSetComposeInclude;
//  uri : TFhirUri;
  excluded : boolean;
  {$IFDEF FHIR2}
  i : integer;
  isabstract : boolean;
  {$ENDIF}
  checker : TValueSetChecker;
  uri : TFhirUri;
begin
  result := false;
  {special case:}
  if (fvs.url = ANY_CODE_VS) then
  begin
    cs := FStore.getProvider(system, version, FProfile, true);
    try
      if cs = nil then
        result := false
      else
      begin
        ctxt := cs.locate(code);
        if (ctxt = nil) then
          result := false
        else
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FProfile = nil) or not FProfile.activeOnly or not cs.isInactive(ctxt));
            cs.Displays(ctxt, displays, Fprofile.displayLanguage);
          finally
            cs.Close(ctxt);
          end;
      end;
    finally
      cs.Free;
    end;
  end
  else
  begin
    {$IFDEF FHIR2}
    if (fvs.codeSystem <> nil) and ((system = fvs.codeSystem.system) or (system = SYSTEM_NOT_APPLICABLE)) then
    begin
      result := FindCode(fvs, code, fvs.codeSystem.conceptList, displays, isabstract);
      if result then
      begin
        result := abstractOk or not isabstract;
        exit;
      end;
    end;
    {$ENDIF}
    if (fvs.compose <> nil) then
    begin
      result := false;
      {$IFDEF FHIR2}
      for i := 0 to fvs.compose.importList.Count - 1 do
      begin
        if not result then
        begin
          checker := TValueSetChecker(FOthers.matches[fvs.compose.importList[i].value]);
          result := checker.check(system, version, code, abstractOk, displays, message);
        end;
      end;
      {$ENDIF}
      for cc in fvs.compose.includeList do
      begin
        if cc.system = '' then
          result := true
        else
        begin
          cs := TCodeSystemProvider(FOthers.matches[cc.system]);
          result := ((system = SYSTEM_NOT_APPLICABLE) or (cs.system(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
        end;
        {$IFNDEF FHIR2}
        for uri in cc.valueSetList do
        begin
          checker := TValueSetChecker(FOthers.matches[uri.value]);
          result := result and checker.check(system, version, code, abstractOk, displays, message);
        end;
        {$ENDIF}
        if result then
          break;
      end;
      if result then
        for cc in fvs.compose.excludeList do
        begin
          if cc.system = '' then
            excluded := true
          else
          begin
            cs := TCodeSystemProvider(FOthers.matches[cc.system]);
            excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.system(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
          end;
          {$IFDEF FHIR3}
          for uri in cc.valueSetList do
          begin
            checker := TValueSetChecker(FOthers.matches[uri.value]);
            excluded := excluded and checker.check(system, version, code, abstractOk, displays, message);
          end;
          {$ENDIF}
          if excluded then
            exit(false);
        end;
    end;
  end;
end;


function TValueSetChecker.check(coding: TFhirCoding; abstractOk : boolean) : TFhirParameters;
var
  list : TStringList;
  message : String;
begin
  result := TFhirParameters.create;
  try
    list := TStringList.Create;
    try
      list.Duplicates := dupIgnore;
      list.CaseSensitive := false;
      if check(coding.system, coding.version, coding.code, abstractOk, list, message) then
      begin
        result.AddParameter('result', TFhirBoolean.Create(true));
        if (coding.display <> '') and (list.IndexOf(coding.display) < 0) then
          result.AddParameter('message', 'The display "'+coding.display+'" is not a valid display for the code '+coding.code);
        if list.Count > 0 then
          result.AddParameter('display', list[0]);
      end
      else
      begin
        result.AddParameter('result', TFhirBoolean.Create(false));
        result.AddParameter('message', 'The system/code "'+coding.system+'"/"'+coding.code+'" is not in the value set '+fvs.name);
        if (message <> '') then
          result.AddParameter('message', message);
      end;
    finally
      list.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function hasMessage(params : TFhirParameters; msg : String) : boolean;
var
  p : TFhirParametersParameter;
begin
  result := false;
  for p in params.parameterList do
    if (p.name = 'message') and (p.value is TFHIRString) and (TFHIRString(p.value).value = msg) then
      exit(true);
end;

function TValueSetChecker.check(code: TFhirCodeableConcept; abstractOk : boolean) : TFhirParameters;
  function Summary(code: TFhirCodeableConcept) : String;
  begin
    if (code.codingList.Count = 1) then
      result := 'The code provided ('+summarise(code)+') is not '
    else
      result := 'None of the codes provided ('+summarise(code)+') are ';
  end;
var
  list : TStringList;
  i : integer;
  v : boolean;
  ok : TFhirBoolean;
  cc, codelist, message : String;
  prov : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
begin
  if FVs = nil then
    raise Exception.Create('Error: cannot validate a CodeableConcept without a nominated valueset');
  result := TFhirParameters.Create;
  try
    list := TStringList.Create;
    try
      list.Duplicates := dupIgnore;
      list.CaseSensitive := false;
      ok := TFhirBoolean.Create(false);
      result.AddParameter('result', ok);
      codelist := '';
      for i := 0 to code.codingList.Count - 1 do
      begin
        list.Clear;
        cc := ',{'+code.codingList[i].system+'}'+code.codingList[i].code;
        codelist := codelist + cc;
        v := check(code.codingList[i].system, code.codingList[i].version, code.codingList[i].code, abstractOk, list, message);
        if not v and (message <> '') then
          result.AddParameter('message', message);
        ok.value := ok.value or v;

        if (v) then
        begin
          message := '';
          if (code.codingList[i].display <> '') and (list.IndexOf(code.codingList[i].display) < 0) then
            result.AddParameter('message', 'The display "'+code.codingList[i].display+'" is not a valid display for the code '+cc.substring(1));
          if list.Count > 0 then
            result.AddParameter('display', list[0]);
        end
        else
        begin
          prov := FStore.getProvider(code.codingList[i].system, code.codingList[i].version, FProfile, true);
          try
           if (prov = nil) then
           begin
             result.AddParameter('message', 'The system "'+code.codingList[i].system+'" is not known');
             result.AddParameter('cause', 'unknown');
           end
           else
           begin
             ctxt := prov.locate(code.codingList[i].code, message);
             try
               if ctxt = nil then
               begin
                 result.AddParameter('message', 'The code "'+code.codingList[i].code+'" is not valid in the system '+code.codingList[i].system);
                 result.AddParameter('cause', 'invalid');
                 if (message <> '') and not hasMessage(result, message) then
                   result.AddParameter('message', message);
               end
               else
               begin
                 prov.Displays(ctxt, list, '');
                 if (code.codingList[i].display <> '') and (list.IndexOf(code.codingList[i].display) = -1) then
                   result.AddParameter('message', 'The display "'+code.codingList[i].display+'" is not a valid display for the code '+cc)
               end;
             finally
               prov.Close(ctxt);
             end;
           end;
          finally
            prov.Free;
          end;
        end;
      end;
      if (not ok.value) then
        if fvs.name = '' then
          result.AddParameter('message', Summary(code) +' valid')
        else
          result.AddParameter('message', Summary(code) +' valid in the value set '+fvs.name);
    finally
      list.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderFilterContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

function TValueSetChecker.checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code: String; abstractOk : boolean; displays : TStringList; var message : String): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  ctxt : TCodeSystemProviderFilterContext;
  loc :  TCodeSystemProviderContext;
  prep : TCodeSystemProviderFilterPreparationContext;
  filters : Array of TCodeSystemProviderFilterContext;
  msg : String;
begin
  result := false;
  if (cset.conceptList.count = 0) and (cset.filterList.count = 0) then
  begin
    loc := cs.locate(code, message);
    try
      result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
      if result then
      begin
        if FProfile = nil then
          cs.displays(loc, displays, '')
        else
          cs.displays(loc, displays, FProfile.displayLanguage);
        exit;
      end
      else

    finally
      cs.Close(loc);
    end;
  end;

  for i := 0 to cset.conceptList.count - 1 do
    if (code = cset.conceptList[i].code) then
    begin
      loc := cs.locate(code);
      if Loc <> nil then
      begin
        cs.close(loc);
        cs.displays(code, displays, '');
        result := (abstractOk or not cs.IsAbstract(loc));
        exit;
      end;
    end;

  if cset.filterList.count > 0 then
  begin
    SetLength(filters, cset.filterList.count);
    prep := cs.getPrepContext;
    try
      for i := 0 to cset.filterList.count - 1 do
      begin
        fc := cset.filterList[i];
        // gg - why? if ('concept' = fc.property_) and (fc.Op = FilterOperatorIsA) then
        filters[i] := cs.filter(fc.property_, fc.Op, fc.value, prep);
      end;
      if cs.prepare(prep) then // all are together, just query the first filter
      begin
        ctxt := filters[0];
        loc := cs.filterLocate(ctxt, code);
        try
          result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
          if result then
            cs.displays(loc, displays, FProfile.displayLanguage);
        finally
          cs.Close(loc);
        end;
      end
      else
      begin
        result := true;
        for i := 0 to cset.filterList.count - 1 do
        begin
          fc := cset.filterList[i];
          if ('concept' = fc.property_) and (fc.Op = FilterOperatorIsA) then
          begin
            loc := cs.locateIsA(code, fc.value);
            try
              result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
              if loc <> nil then
                cs.displays(loc, displays, FProfile.displayLanguage);
            finally
              cs.Close(loc);
            end;
          end
          else
          begin
            ctxt := filters[i];
            loc := cs.filterLocate(ctxt, code, msg);
            try
              if (loc = nil) and (message = '') then
                message := msg;
              result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
              if loc <> nil then
                cs.displays(loc, displays, FProfile.displayLanguage);
            finally
              cs.Close(loc);
            end;
          end;
          if not result then
            break;
        end;
      end;
    finally
      for i := 0 to cset.filterList.count - 1 do
        cs.Close(filters[i]);
      cs.Close(prep);
    end;
  end;
end;

end.
