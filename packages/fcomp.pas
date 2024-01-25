{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcomp;

{$warn 5023 off : no warning about unused units}
interface

uses
  fcomp_register, fcomp_graph, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fcomp_register', @fcomp_register.Register);
end;

initialization
  RegisterPackage('fcomp', @Register);
end.
