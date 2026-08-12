program RunFileTemplatesTests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, FileTemplates_Tests;

var
  TestRunner: TTestRunner;
begin
  TestRunner:=TTestRunner.Create(nil);
  try
    TestRunner.Initialize;
    TestRunner.Run;
  finally
    TestRunner.Free;
  end;
end.
