{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

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

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}
unit IpResolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GeoIP, syncobjs;

type
  PHostEntry = ^THostEntry;
  THostEntry = record
    IP: string;
    HostName: string;
    CountryName: string;
    CountryCode: string;
    ImageIndex: integer;
  end;

  TResolverOption = (roResolveIP, roResolveCountry);
  TResolverOptions = set of TResolverOption;

  { TIpResolverThread }

  TIpResolver = class(TThread)
  private
    FLock: TCriticalSection;
    FResolveEvent: TEvent;
    // Objects[] stores PHostEntry pointers. TStringList does not own them;
    // Destroy disposes each cached entry exactly once.
    FCache: TStringList;
    FResolveIp: TStringList;
    FGeoIp: TGeoIP;
    FOptions: TResolverOptions;
    FGeoIpCounryDB: string;
  protected
    procedure Execute; override;
    function GetOrCreateEntry(const IpAddress: string; out IsNew: boolean): PHostEntry;
    function FindEntry(const IpAddress: string): PHostEntry;
  public
    constructor Create(const GeoIpCounryDB: string; AOptions: TResolverOptions); reintroduce;
    destructor Destroy; override;
    function Resolve(const IpAddress: string): PHostEntry;
  end;

implementation

uses synsock;

{ TIpResolver }

procedure TIpResolver.Execute;
var
  ip, s: string;
  c: PHostEntry;
begin
  try
    while not Terminated do begin
      if FResolveEvent.WaitFor(200) = wrSignaled then begin
        FResolveEvent.ResetEvent;

        while True do begin
          FLock.Enter;
          try
            ip:='';
            if not Terminated and (FResolveIp.Count > 0) then begin
              ip:=FResolveIp[0];
              FResolveIp.Delete(0);
            end;
            UniqueString(ip);
          finally
            FLock.Leave;
          end;

          if ip = '' then
            break;

          if roResolveIP in FOptions then begin
            s:=synsock.ResolveIPToName(ip, AF_INET, IPPROTO_IP, 0);
            c:=FindEntry(ip);
            FLock.Enter;
            try
              c^.HostName:=s;
              UniqueString(c^.HostName);
            finally
              FLock.Leave;
            end;
          end;
        end;

      end;
    end;
  except
  end;
  Sleep(20);
end;

function TIpResolver.GetOrCreateEntry(const IpAddress: string; out IsNew: boolean): PHostEntry;
var
  i: integer;
begin
  FLock.Enter;
  try
    if FCache.Find(IpAddress, i) then begin
      Result:=PHostEntry(FCache.Objects[i]);
      IsNew:=False;
    end
    else begin
      New(Result);
      try
        Result^.ImageIndex:=0;
        Result^.IP:=IpAddress;
        UniqueString(Result^.IP);
        Result^.HostName:=IpAddress;
        UniqueString(Result^.HostName);
        FCache.AddObject(Result^.IP, TObject(Result));
      except
        Dispose(Result);
        raise;
      end;
      IsNew:=True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TIpResolver.FindEntry(const IpAddress: string): PHostEntry;
var
  i: integer;
begin
  FLock.Enter;
  try
    if FCache.Find(IpAddress, i) then
      Result:=PHostEntry(FCache.Objects[i])
    else
      Result:=nil;
  finally
    FLock.Leave;
  end;
end;

constructor TIpResolver.Create(const GeoIpCounryDB: string; AOptions: TResolverOptions);
begin
  FOptions:=AOptions;
  FLock:=TCriticalSection.Create;
  FResolveEvent:=TEvent.Create(nil, True, False, '');
  FCache:=TStringList.Create;
  FCache.CaseSensitive:=True;
  FCache.UseLocale:=False;
  FCache.Duplicates:=dupIgnore;
  FCache.Sorted:=True;
  FResolveIp:=TStringList.Create;
  FGeoIpCounryDB:=GeoIpCounryDB;
  if (roResolveCountry in FOptions) and (FGeoIpCounryDB <> '') then
    FGeoIp:=TGeoIP.Create(GeoIpCounryDB);
  inherited Create(not (roResolveIP in FOptions));
end;

destructor TIpResolver.Destroy;
var
  i: integer;
begin
  FLock.Enter;
  try
    Terminate;
  finally
    FLock.Leave;
  end;
  if not Suspended then begin
    FResolveEvent.SetEvent;
    WaitFor;
  end;
  FResolveIp.Free;
  FResolveEvent.Free;
  FLock.Free;
  FGeoIp.Free;
  for i:=0 to FCache.Count - 1 do
    Dispose(PHostEntry(FCache.Objects[i]));
  FCache.Free;
  inherited Destroy;
end;

function TIpResolver.Resolve(const IpAddress: string): PHostEntry;
var
  GeoCountry: TGeoIPCountry;
  IsNew: boolean;
begin
  Result:=GetOrCreateEntry(IpAddress, IsNew);
  if not IsNew then
    exit;

  if roResolveIP in FOptions then begin
    FLock.Enter;
    try
      if FResolveIp.IndexOf(IpAddress) < 0 then begin
        FResolveIp.Add(IpAddress);
        FResolveEvent.SetEvent;
      end;
    finally
      FLock.Leave;
    end;
  end;

  if FGeoIp <> nil then
  try
    if FGeoIp.GetCountry(IpAddress, GeoCountry) = GEOIP_SUCCESS then begin
      FLock.Enter;
      try
        Result^.CountryName:=GeoCountry.CountryName;
        UniqueString(Result^.CountryName);
        Result^.CountryCode:=AnsiLowerCase(GeoCountry.CountryCode);
        UniqueString(Result^.CountryCode);
      finally
        FLock.Leave;
      end;
    end;
  except
    FreeAndNil(FGeoIp);
    DeleteFile(FGeoIpCounryDB);
    Result:=nil;
  end;
end;

end.

