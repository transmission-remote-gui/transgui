{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit trcomp; 

interface

uses
  VarGrid, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('VarGrid', @VarGrid.Register); 
end; 

initialization
  RegisterPackage('trcomp', @Register); 
end.
