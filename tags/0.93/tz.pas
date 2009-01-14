unit tz;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
{$ifdef windows}
  Windows
{$endif}
{$ifdef unix}
  baseunix, unix
{$endif}
  ;

function GetTimeZoneDelta: TDateTime;

implementation

function GetTimeZoneDelta: TDateTime;
{$ifdef windows}
var
  t: TIME_ZONE_INFORMATION;
{$endif}
{$ifdef unix}
var
   timeval: TTimeVal;
   timezone: TTimeZone;
{$endif}
begin
  Result:=0;
{$ifdef windows}
  if GetTimeZoneInformation(t) <> TIME_ZONE_ID_INVALID then
    Result:=-t.Bias/MinsPerDay;
{$endif}
{$ifdef unix}
  timezone.tz_minuteswest:=0;
  fpgettimeofday(@timeval, @timezone);
  Result:=-timezone.tz_minuteswest/MinsPerDay;
{$endif}
end;

end.

