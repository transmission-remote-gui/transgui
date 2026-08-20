program run_rpctable_tests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner, rpctable_tests;

var
  Application: TTestRunner;

begin
  Application:=TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='rpctable unit tests';
  Application.Run;
  Application.Free;
end.
