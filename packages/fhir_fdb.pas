{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_fdb;

{$warn 5023 off : no warning about unused units}
interface

uses
  fdb_dialects, fdb_fpc, fdb_logging, fdb_manager, fdb_odbc_headers, 
  fdb_odbc_objects, fdb_odbc, fdb_settings, fdb_sqlite3_objects, 
  fdb_sqlite3_utilities, fdb_sqlite3_wrapper, fdb_sqlite3, fdb_fts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
   RegisterPackage('fhir_fdb', @Register);
end.
