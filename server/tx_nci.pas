unit tx_nci;


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
  fsl_utilities, fsl_base, AdvObjectLists,
  YuStemmer,
  fdb_manager,
  FHIR.Version.Types, FHIRComponents, FHIR.Version.Resources, ftx_service, DateAndTime;

type
  TNciMetaConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
    FOthers : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNciMetaFilter = class (TCodeSystemProviderFilterContext)
  private
    sql : String;
    text : boolean;
    qry : TFDBConnection;
  public
    destructor Destroy; Override;
  end;

  TNciMetaPrep = class (TCodeSystemProviderFilterPreparationContext)
  private
    filters : TFslObjectList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  TNciMetaServices = class (TCodeSystemProvider)
  private
    db : TFDBManager;

  public
    constructor Create(db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TNciMetaServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

procedure generateNciStems(db : TFDBManager);

implementation

procedure MakeStems(stemmer : TFslWordStemmer; stems : TStringList; desc : String; cui : string);
var
  s : String;
  i : integer;
  list : TStringList;
begin
  while (desc <> '') do
  begin
    StringSplit(desc, [' ', '.', ',', '-', ')', '(', '#', '/', '%', '[', ']', '{', '}', ':'], s, desc);
    s := StringTrimSet(s, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).ToLower;
    if (s <> '') and not StringIsInteger16(s) and (s[1] >= 'a') then
    begin
      s := stemmer.calc(s);
      if stems.Find(s, i) then
        TStringList(stems.Objects[i]).add(cui)
      else
      begin
        list := TStringList.Create;
        list.Sorted := true;
        list.Add(cui);
        stems.AddObject(s, list);
      end;
    end;
  end;
end;

procedure generateNciStems(db : TFDBManager);
var
  stems, list : TStringList;
  qry : TFDBConnection;
  stemmer : TFslWordStemmer;
  i, j : integer;
begin
  stemmer := TFslWordStemmer.create('english');
  stems := TStringList.Create;
  try
    stems.Sorted := true;
    qry := db.GetConnection('stems');
    try
      qry.ExecSQL('delete from rxnstems');

      qry.SQL := 'select RXCUI, STR from rxnconso where SAB = ''RXNORM'''; // allow SY
      qry.Prepare;
      qry.Execute;
      i := 0;
      write('Stemming');
      while qry.FetchNext do
      begin
        makeStems(stemmer, stems, qry.ColString[2], qry.ColString[1]);
        inc(i);
        if (i mod 10000 = 0) then
          write('.');
      end;
      writeln('done');
      writeln(inttostr(stems.Count)+' stems');
      qry.Terminate;

      qry.SQL := 'insert into rxnstems (stem, cui) values (:stem, :cui)';
      qry.Prepare;
      write('Storing');
      for i := 0 to stems.count - 1 do
      begin
        list := stems.objects[i] as TStringList;
        for j := 0 to list.count-1 do
        begin
          qry.BindString('stem', copy(stems[i], 1, 20));
          qry.BindString('cui', list[j]);
          qry.Execute;
        end;
        if (i mod 100 = 0) then
          write('.');
      end;
      writeln('done');
      qry.Terminate;
      qry.Release;
    except
      on e : Exception do
      begin
        qry.Error(e);
        raise;
      end;
    end;
  finally
    stems.Free;
    stemmer.Free;
    db.free;
  end;
end;

{ TNciMetaServices }

Constructor TNciMetaServices.create(db : TFDBManager);
begin
  inherited Create;

  self.db := db;
  if (TotalCount = 0) then
    raise EDBException.create('Error Connecting to NciMeta');
end;



function TNciMetaServices.TotalCount : integer;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection('NciMeta.Count');
  try
    qry.SQL := 'Select Count(*) from rxnconso where SAB = ''RXNORM'' and TTY <> ''SY''';
    qry.prepare;
    qry.execute;
    qry.FetchNext;
    result := qry.ColInteger[1];
    qry.Terminate;
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      raise;
    end;
  end;
end;


function TNciMetaServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://ncimeta.nci.nih.gov';
end;

function TNciMetaServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TNciMetaServices.getDisplay(code : String):String;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection('NciMeta.display');
  try
    qry.SQL := 'Select STR from rxnconso where RXCUI = :code and SAB = ''RXNORM'' and TTY <> ''SY''';
    qry.prepare;
    qry.execute;
    qry.FetchNext;
    result := qry.colString[1];
    qry.Terminate;
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      raise;
    end;
  end;
end;

function TNciMetaServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := TNciMetaPrep.Create;
end;

procedure TNciMetaServices.Displays(code : String; list : TStringList);
begin
  list.Add(getDisplay(code));
end;


{
 TCodeSystemProviderContext methods

 a TCodeSystemProviderContext is a reference to a code (A CUI in RXNrom) that is
 used to get information about the code
}

function TNciMetaServices.locate(code : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
  res : TNciMetaConcept;
begin
  qry := db.GetConnection('NciMeta.display');
  try
    qry.SQL := 'Select STR, TTY from rxnconso where RXCUI = :code and SAB = ''RXNORM''';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TNciMetaConcept.Create;
      try
        res.FCode := code;
        repeat
          if qry.ColString[2] = 'SY' then
            res.FOthers.Add(qry.ColString[1])
          else
            res.FDisplay := qry.ColString[1];
        until (not qry.FetchNext);
        result := res.Link;
      finally
        res.Free;
      end;
    end;
    qry.Terminate;
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      raise;
    end;
  end;
end;


function TNciMetaServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TNciMetaConcept(context).FCode;
end;

function TNciMetaServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TNciMetaServices.Destroy;
begin
  DB.Free;
  inherited;
end;

function TNciMetaServices.Display(context : TCodeSystemProviderContext) : string;
begin
  result := TNciMetaConcept(context).FDisplay;
end;

procedure TNciMetaServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  list.Add(Display(context));
  list.AddStrings(TNciMetaConcept(context).FOthers);
end;

function TNciMetaServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // NciMeta doesn't do abstract?
end;

function TNciMetaServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TNciMetaServices.Link: TNciMetaServices;
begin
  result := TNciMetaServices(Inherited Link);
end;

function TNciMetaServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  raise ETerminologyError.create('ChildCount not supported by RXNorm'); // only used when iterating the entire code system. and NciMeta is too big
end;

function TNciMetaServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('getcontext not supported by RXNorm'); // only used when iterating the entire code system. and NciMeta is too big
end;

function TNciMetaServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  result := nil; // todo: no sumbsumption?
end;


function TNciMetaServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
var
  sql1 : string;
  sql2 : String;
  i : integer;
  filter : TNciMetaFilter;
begin
  result := false;
  if TNciMetaPrep(prep).filters.Count = 0 then
    exit; // not being used

  sql1 := '';
  sql2 := 'from rxnconso';

  for i := 0 to TNciMetaPrep(prep).filters.Count - 1 do
    if not TNciMetaFilter(TNciMetaPrep(prep).filters[i]).text then
      sql1 := sql1 + ' '+TNciMetaFilter(TNciMetaPrep(prep).filters[i]).sql;
  for i := 0 to TNciMetaPrep(prep).filters.Count - 1 do
  begin
    if TNciMetaFilter(TNciMetaPrep(prep).filters[i]).text then
    begin
      sql2 := sql2 + ', rxnstems as s'+inttostr(i);
      sql1 := sql1 + ' '+TNciMetaFilter(TNciMetaPrep(prep).filters[i]).sql.replace('%%', inttostr(i));
    end;
  end;

  filter := TNciMetaFilter(TNciMetaPrep(prep).filters[0]);
  filter.sql := sql1;
  result := true;
  filter.qry := db.GetConnection('NciMeta.prepare');
  filter.qry.SQL := 'Select RXCUI, STR '+sql2+' where SAB = ''RXNORM'' and TTY <> ''SY'' '+filter.sql;
  filter.qry.Prepare;
  filter.qry.Execute;
end;

function TNciMetaServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  s : String;
  i : integer;
  res : TNciMetaFilter;
begin
  if prep = nil then
  begin
    // in this case, a search through the entire rxnorm. nothing to do to speed it up, and it won't be prepped
    s := '';
    for i := 0 to filter.stems.Count - 1 do
      s := s +' and RXCUI in (select CUI from rxnstems where stem like '''+SQLWrapString(filter.stems[i])+'%'')';

    res := TNciMetaFilter.Create;
    try
      res.sql := s;
      result := res.link;
    finally
      res.Free;
    end;
  end
  else
  begin
    result := nil;
    for i := 0 to filter.stems.Count - 1 do
    begin
      res := TNciMetaFilter.Create;
      try
        res.sql := ' and (RXCUI = s%%.CUI and s%%.stem like '''+SQLWrapString(filter.stems[i])+'%'')';
        res.text := true;
        if result = nil then
          result := res.link;
        TNciMetaPrep(prep).filters.Add(res.Link);
      finally
        res.Free;
      end;
    end;
  end;
end;

const
  RELATIONSHIPS : Array[0..6] of String = ('SY', 'SIB', 'RN', 'PAR', 'CHD', 'RB', 'RO');
  RELATIONSHIP_TYPES : Array[0..77] of String = (
    'active_ingredient_of', 'active_metabolites_of', 'chemical_structure_of', 'consists_of', 'constitutes', 'contained_in', 'contains', 'contraindicated_with_disease', 'contraindicating_class_of', 'contraindicating_mechanism_of_action_of', 'contraindicating_physiologic_effect_of', 'doseformgroup_of', 'dose_form_of', 'effect_may_be_inhibited_by', 'entry_version_of',
    'form_of', 'has_active_ingredient', 'has_active_metabolites', 'has_chemical_structure', 'has_contraindicated_drug', 'has_contraindicating_class', 'has_contraindicating_mechanism_of_action', 'has_contraindicating_physiologic_effect', 'has_doseformgroup', 'has_dose_form', 'has_entry_version',
    'has_form', 'has_ingredient', 'has_ingredients', 'has_mechanism_of_action', 'has_member', 'has_part', 'has_participant', 'has_permuted_term', 'has_pharmacokinetics', 'has_physiologic_effect', 'has_precise_ingredient', 'has_print_name', 'has_product_component', 'has_quantified_form', 'has_sort_version', 'has_therapeutic_class', 'has_tradename', 'included_in', 'includes', 'induced_by', 'induces', 'ingredients_of', 'ingredient_of',
    'inverse_isa', 'isa', 'mapped_from', 'mapped_to', 'may_be_diagnosed_by', 'may_be_prevented_by', 'may_be_treated_by', 'may_diagnose', 'may_inhibit_effect_of', 'may_prevent', 'may_treat', 'mechanism_of_action_of', 'member_of', 'metabolic_site_of', 'participates_in', 'part_of', 'permuted_term_of', 'pharmacokinetics_of', 'physiologic_effect_of', 'precise_ingredient_of', 'print_name_of', 'product_component_of', 'quantified_form_of', 'reformulated_to', 'reformulation_of', 'site_of_metabolism', 'sort_version_of', 'therapeutic_class_of', 'tradename_of'
  );

function TNciMetaServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TNciMetaFilter;
  ok : boolean;
begin
  result := nil;

  res := TNciMetaFilter.Create;
  try
    ok := true;
    if (op <> FilterOperatorEqual) then
      ok := false
    else if prop = 'STY' then
      res.sql := 'and RXCUI in (select RXCUI from rxnsty where TUI = '''+SQLWrapString(value)+''')'
    else if prop = 'SAB' then
      res.sql := 'and RXCUI in (select RXCUI from rxnconso where SAB = '''+SQLWrapString(value)+'''))'
    else if prop = 'TTY' then
      res.sql := 'and TTY =  '''+SQLWrapString(value)+''''
    else if StringArrayExistsSensitive(RELATIONSHIPS, prop) and value.StartsWith('CUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where RXCUI in (select RXCUI1 from rxnrel where '+
      'REL = '''+SQLWrapString(prop)+''' and RXCUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if StringArrayExistsSensitive(RELATIONSHIPS, prop) and value.StartsWith('AUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXAUI in (select RXAUI1 from rxnrel where REL = '''+SQLWrapString(prop)+''' and RXAUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if StringArrayExistsSensitive(RELATIONSHIP_TYPES, prop) and value.StartsWith('CUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXCUI in (select RXCUI1 from rxnrel where '+
      'RELA = '''+SQLWrapString(prop)+''' and RXCUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if StringArrayExistsSensitive(RELATIONSHIP_TYPES, prop) and value.StartsWith('AUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXAUI in (select RXAUI1 from rxnrel where RELA = '''+SQLWrapString(prop)+''' and RXAUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else
      ok := false;
    if ok then
    begin
      result := res.link;
      if prep <> nil then
        TNciMetaPrep(prep).filters.Add(res.Link);
    end
    else
      raise ETerminologyError.create('Unknown ');
  finally
    res.Free;
  end;
end;

function TNciMetaServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
  res : TNciMetaConcept;
begin
  qry := db.GetConnection('NciMeta.display');
  try
    qry.SQL := 'Select RXCUI, STR from rxnconso where SAB = ''RXNORM''  and TTY <> ''SY'' and RXCUI = :code '+TNciMetaFilter(ctxt).sql;
    qry.prepare;
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TNciMetaConcept.Create;
      try
        res.FCode := code;
        res.FDisplay := qry.ColString[2];
        result := res.Link;
      finally
        res.Free;
      end;
    end;
    qry.Terminate;
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      raise;
    end;
  end;
end;

function TNciMetaServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
var
  filter : TNciMetaFilter;
begin
  filter := TNciMetaFilter(ctxt);
  if (filter.qry = nil) then
  begin
    // search on full rxnorm
    filter.qry := db.GetConnection('NciMeta.filter');
    filter.qry.SQL := 'Select RXCUI, STR from rxnconso where SAB = ''RXNORM'' and TTY <> ''SY'' '+filter.sql;
    filter.qry.prepare;
    filter.qry.Execute;
  end;
  result := filter.qry.FetchNext;
end;

function TNciMetaServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  filter : TNciMetaFilter;
  res : TNciMetaConcept;
begin
  filter := TNciMetaFilter(ctxt);
  res := TNciMetaConcept.Create;
  try
    res.FCode := filter.qry.ColString[1];
    res.FDisplay := filter.qry.ColString[2];
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TNciMetaServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyError.create('Error in internal logic - filter not prepped?');
end;

procedure TNciMetaServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TNciMetaServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;



procedure TNciMetaServices.Close(
  ctxt: TCodeSystemProviderFilterPreparationContext);
begin

end;

{ TNciMetaPrep }

constructor TNciMetaPrep.Create;
begin
  inherited;
  filters := TFslObjectList.Create;
end;

destructor TNciMetaPrep.Destroy;
begin
  filters.Free;
  inherited;
end;

{ TNciMetaFilter }

destructor TNciMetaFilter.Destroy;
begin
  qry.terminate;
  qry.Release;
  inherited;
end;

{ TNciMetaConcept }

constructor TNciMetaConcept.create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TNciMetaConcept.destroy;
begin
  FOthers.free;
  inherited;
end;

end.
