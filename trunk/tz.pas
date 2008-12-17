unit tz;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
{$ifdef windows}
  Windows
{$endif}
{$ifdef unix}
  baseunix
{$endif}
  ;

function GetTimeZoneDelta: TDateTime;

implementation

function GetTimeZoneDelta: TDateTime;
{$ifdef windows}
var
  t: TIME_ZONE_INFORMATION;
{$endif}
begin
  Result:=0;
{$ifdef windows}
  if GetTimeZoneInformation(t) <> TIME_ZONE_ID_INVALID then
    Result:=-t.Bias/MinsPerDay;
{$endif}
end;

end.

