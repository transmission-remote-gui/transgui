{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2009 by Yury Sidorov.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*************************************************************************************}

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

