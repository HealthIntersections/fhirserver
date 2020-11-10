program FHIRToolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF WINDOWS}
  FastMM4,
  {$ENDIF}
  Windows,
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, frm_npm_manager, frm_npm_browser,
  { you can add units after this }
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Logging,
  FHIR.Support.Utilities, FHIR.Support.Collections, FHIR.Support.MXml,
  FHIR.Support.Json, FHIR.LCL.Managers, FHIR.Base.Lang, FHIR.Base.Objects,
  FHIR.Base.Xhtml, FHIR.Web.Parsers, frm_progress, SynHighlighterHL7,
  FHIR.LCL.Synchroniser, FHIR.v2.Objects, FHIR.v2.Base, FHIR.v2.Dictionary,
  FHIR.Ucum.IFace, FHIR.Client.Base, FHIR.R4.Parser, FHIR.R4.Types,
  FHIR.R4.Base, FHIR.R4.Resources, FHIR.R4.Xml, FHIR.R4.Json, FHIR.R4.Turtle,
  FHIR.R4.Context, FHIR.Cda.Narrative, FHIR.Cda.Base, FHIR.Cda.Types,
  FHIR.Smart.Utilities, frm_file_format, FHIR.Toolkit.Context, FrameViewer09,
  FHIR.Toolkit.Console, FHIR.Toolkit.FileStore, FHIR.Toolkit.IniEditor,
  FHIR.Toolkit.TextEditor, FHIR.Toolkit.XmlEditor, FHIR.Toolkit.Factory,
  FHIR.Toolkit.JsonEditor, FHIR.Toolkit.BaseEditor, FHIR.Toolkit.HtmlEditor,
  FHIR.Toolkit.JavascriptEditor, frm_settings, FHIR.Toolkit.HL7Editor,
  FHIR.Toolkit.Search;

{$R *.res}

begin
  AllocConsole;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainToolkitForm, MainToolkitForm);
  Application.Run;
end.

