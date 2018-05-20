program FhirPackageManager;

uses
  Vcl.Forms,
  FHIR.Cache.PackageManagerDialog in '..\..\reference-platform\cache\FHIR.Cache.PackageManagerDialog.pas' {PackageCacheForm},
  VirtualTrees in '..\..\..\Components\treeview\Source\VirtualTrees.pas',
  VTAccessibilityFactory in '..\..\..\Components\treeview\Source\VTAccessibilityFactory.pas',
  VirtualTrees.StyleHooks in '..\..\..\Components\treeview\Source\VirtualTrees.StyleHooks.pas',
  VirtualTrees.Classes in '..\..\..\Components\treeview\Source\VirtualTrees.Classes.pas',
  VirtualTrees.WorkerThread in '..\..\..\Components\treeview\Source\VirtualTrees.WorkerThread.pas',
  VirtualTrees.ClipBoard in '..\..\..\Components\treeview\Source\VirtualTrees.ClipBoard.pas',
  VirtualTrees.Utils in '..\..\..\Components\treeview\Source\VirtualTrees.Utils.pas',
  VirtualTrees.Export in '..\..\..\Components\treeview\Source\VirtualTrees.Export.pas',
  VTHeaderPopup in '..\..\..\Components\treeview\Source\VTHeaderPopup.pas',
  FHIR.Cache.PackageManager in '..\..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Support.Objects in '..\..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Strings in '..\..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\..\reference-platform\support\FHIR.Support.Math.pas',
  FHIR.Support.Generics in '..\..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.System in '..\..\reference-platform\support\FHIR.Support.System.pas',
  FHIR.Support.DateTime in '..\..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\..\reference-platform\support\FHIR.Support.Decimal.pas',
  FHIR.Support.Json in '..\..\reference-platform\support\FHIR.Support.Json.pas',
  FHIR.Support.Binary in '..\..\reference-platform\support\FHIR.Support.Binary.pas',
  FHIR.Support.Stream in '..\..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Collections in '..\..\reference-platform\support\FHIR.Support.Collections.pas',
  FHIR.Support.Factory in '..\..\reference-platform\support\FHIR.Support.Factory.pas',
  FHIR.Support.Controllers in '..\..\reference-platform\support\FHIR.Support.Controllers.pas',
  FHIR.Support.Text in '..\..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Tarball in '..\..\reference-platform\support\FHIR.Support.Tarball.pas',
  FHIR.Web.Fetcher in '..\..\reference-platform\support\FHIR.Web.Fetcher.pas',
  FHIR.Support.Shell in '..\..\reference-platform\support\FHIR.Support.Shell.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPackageCacheForm, PackageCacheForm);
  Application.Run;
end.
