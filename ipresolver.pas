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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, GeoIP, syncobjs;

type
  PHostEntry = ^THostEntry;
  THostEntry = record
  private
    FSharedState: Pointer;
    function GetImageIndex: integer;
    procedure SetImageIndex(AValue: integer);
  public
    IP: string;
    HostName: string;
    CountryName: string;
    CountryCode: string;
    // UI-owned metadata is shared by all snapshots for the same IP and is
    // synchronized internally, so existing callers can keep using this property.
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
  end;

  TResolverOption = (roResolveIP, roResolveCountry);
  TResolverOptions = set of TResolverOption;

  { TIpResolverThread }

  TIpResolver = class(TThread)
  private
    FLock: TCriticalSection;
    FGeoIpLock: TCriticalSection;
    FResolveEvent: TEvent;
    // Objects[] stores private cache-entry pointers. Published PHostEntry
    // snapshots stay valid until Destroy, even after a newer snapshot replaces
    // them in the cache.
    FCache: TStringList;
    FRetiredEntries: TList;
    FResolveIp: TStringList;
    FGeoIp: TGeoIP;
    FOptions: TResolverOptions;
    FGeoIpCounryDB: string;
  protected
    procedure Execute; override;
    function GetOrCreateEntry(const IpAddress: string; out IsNew: boolean): PHostEntry;
  public
    constructor Create(const GeoIpCounryDB: string; AOptions: TResolverOptions); reintroduce;
    destructor Destroy; override;
    // Country initialization is serialized for concurrent Resolve calls.
    // Reverse-DNS results are published as replacement snapshots; the resolver
    // never mutates resolver-owned string fields of a PHostEntry after returning
    // it to a caller. ImageIndex remains safely shared through its property.
    // Callers must ensure no Resolve call overlaps object destruction.
    function Resolve(const IpAddress: string): PHostEntry;
  end;

implementation

uses synsock, LazFileUtils;

type
  PHostCacheEntry = ^THostCacheEntry;
  THostCacheEntry = record
    ResolverLock: TCriticalSection;
    CurrentEntry: PHostEntry;
    ImageIndex: integer;
    PendingHostName: string;
    HasPendingHostName: boolean;
  end;

{ THostEntry }

function THostEntry.GetImageIndex: integer;
var
  CacheEntry: PHostCacheEntry;
begin
  CacheEntry:=PHostCacheEntry(FSharedState);
  if CacheEntry = nil then begin
    Result:=0;
    exit;
  end;

  CacheEntry^.ResolverLock.Enter;
  try
    Result:=CacheEntry^.ImageIndex;
  finally
    CacheEntry^.ResolverLock.Leave;
  end;
end;

procedure THostEntry.SetImageIndex(AValue: integer);
var
  CacheEntry: PHostCacheEntry;
begin
  CacheEntry:=PHostCacheEntry(FSharedState);
  if CacheEntry = nil then
    exit;

  CacheEntry^.ResolverLock.Enter;
  try
    CacheEntry^.ImageIndex:=AValue;
  finally
    CacheEntry^.ResolverLock.Leave;
  end;
end;

{ TIpResolver }

procedure TIpResolver.Execute;
var
  ip, s: string;
  i: integer;
  CacheEntry: PHostCacheEntry;
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
            FLock.Enter;
            try
              if FCache.Find(ip, i) then begin
                CacheEntry:=PHostCacheEntry(FCache.Objects[i]);
                CacheEntry^.PendingHostName:=s;
                UniqueString(CacheEntry^.PendingHostName);
                CacheEntry^.HasPendingHostName:=True;
              end;
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
  CacheEntry: PHostCacheEntry;
  NewEntry, OldEntry: PHostEntry;
begin
  FLock.Enter;
  try
    if FCache.Find(IpAddress, i) then begin
      CacheEntry:=PHostCacheEntry(FCache.Objects[i]);
      IsNew:=False;

      if CacheEntry^.HasPendingHostName then begin
        OldEntry:=CacheEntry^.CurrentEntry;
        New(NewEntry);
        try
          // Build the replacement completely before publishing it. UI-owned
          // ImageIndex state lives in CacheEntry, so no caller update can be
          // lost while this snapshot is being prepared.
          NewEntry^.FSharedState:=CacheEntry;
          NewEntry^.IP:=OldEntry^.IP;
          UniqueString(NewEntry^.IP);
          NewEntry^.HostName:=CacheEntry^.PendingHostName;
          UniqueString(NewEntry^.HostName);
          NewEntry^.CountryName:=OldEntry^.CountryName;
          UniqueString(NewEntry^.CountryName);
          NewEntry^.CountryCode:=OldEntry^.CountryCode;
          UniqueString(NewEntry^.CountryCode);

          // Reverse DNS is queued only when an entry is first created, so at
          // most one retired resolver snapshot is retained per cached IP.
          FRetiredEntries.Add(OldEntry);
        except
          Dispose(NewEntry);
          raise;
        end;

        CacheEntry^.CurrentEntry:=NewEntry;
        CacheEntry^.PendingHostName:='';
        CacheEntry^.HasPendingHostName:=False;
      end;

      Result:=CacheEntry^.CurrentEntry;
    end
    else begin
      New(CacheEntry);
      CacheEntry^.ResolverLock:=FLock;
      CacheEntry^.CurrentEntry:=nil;
      CacheEntry^.ImageIndex:=0;
      CacheEntry^.HasPendingHostName:=False;
      try
        New(CacheEntry^.CurrentEntry);
        CacheEntry^.CurrentEntry^.FSharedState:=CacheEntry;
        CacheEntry^.CurrentEntry^.IP:=IpAddress;
        UniqueString(CacheEntry^.CurrentEntry^.IP);
        CacheEntry^.CurrentEntry^.HostName:=IpAddress;
        UniqueString(CacheEntry^.CurrentEntry^.HostName);
        FCache.AddObject(CacheEntry^.CurrentEntry^.IP, TObject(CacheEntry));
      except
        if CacheEntry^.CurrentEntry <> nil then
          Dispose(CacheEntry^.CurrentEntry);
        Dispose(CacheEntry);
        raise;
      end;
      Result:=CacheEntry^.CurrentEntry;
      IsNew:=True;
    end;
  finally
    FLock.Leave;
  end;
end;

constructor TIpResolver.Create(const GeoIpCounryDB: string; AOptions: TResolverOptions);
begin
  // Keep the worker suspended until every field used by Execute is ready. This
  // also makes constructor unwinding safe if any allocation below fails.
  inherited Create(True);
  FOptions:=AOptions;
  FLock:=TCriticalSection.Create;
  FGeoIpLock:=TCriticalSection.Create;
  FResolveEvent:=TEvent.Create(nil, True, False, '');
  FCache:=TStringList.Create;
  FCache.CaseSensitive:=True;
  FCache.UseLocale:=False;
  FCache.Duplicates:=dupIgnore;
  FCache.Sorted:=True;
  FRetiredEntries:=TList.Create;
  FResolveIp:=TStringList.Create;
  FGeoIpCounryDB:=GeoIpCounryDB;
  if (roResolveCountry in FOptions) and (FGeoIpCounryDB <> '') then
    FGeoIp:=TGeoIP.Create(GeoIpCounryDB);
  if roResolveIP in FOptions then
    Start;
end;

destructor TIpResolver.Destroy;
var
  i: integer;
  CacheEntry: PHostCacheEntry;
begin
  if FLock <> nil then begin
    FLock.Enter;
    try
      Terminate;
    finally
      FLock.Leave;
    end;
  end
  else
    Terminate;

  if not Suspended then begin
    if FResolveEvent <> nil then
      FResolveEvent.SetEvent;
    WaitFor;
  end;
  FResolveIp.Free;
  FResolveEvent.Free;
  FGeoIp.Free;
  FGeoIpLock.Free;

  // Dispose every snapshot before its private cache state. Callers must already
  // have stopped using entries before Destroy begins.
  if FCache <> nil then
    for i:=0 to FCache.Count - 1 do begin
      CacheEntry:=PHostCacheEntry(FCache.Objects[i]);
      Dispose(CacheEntry^.CurrentEntry);
    end;
  if FRetiredEntries <> nil then begin
    for i:=0 to FRetiredEntries.Count - 1 do
      Dispose(PHostEntry(FRetiredEntries[i]));
    FRetiredEntries.Free;
  end;

  if FCache <> nil then
    for i:=0 to FCache.Count - 1 do begin
      CacheEntry:=PHostCacheEntry(FCache.Objects[i]);
      Dispose(CacheEntry);
    end;
  FCache.Free;
  FLock.Free;
  inherited Destroy;
end;

function TIpResolver.Resolve(const IpAddress: string): PHostEntry;
var
  GeoCountry: TGeoIPCountry;
  GeoIpResult: TGeoIPResult;
  GeoIpFailed: boolean;
  DeleteGeoIp: boolean;
  IsNew: boolean;
  LockGeoIp: boolean;
begin
  GeoIpResult:=GEOIP_NODATA;
  GeoIpFailed:=False;
  LockGeoIp:=roResolveCountry in FOptions;
  if LockGeoIp then
    FGeoIpLock.Enter;
  try
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
      GeoIpResult:=FGeoIp.GetCountry(IpAddress, GeoCountry);
      if GeoIpResult = GEOIP_ERROR_IO then begin
        DeleteGeoIp:=FGeoIp.DatabaseCorrupt;
        FreeAndNil(FGeoIp);
        if DeleteGeoIp then
          DeleteFileUTF8(FGeoIpCounryDB);
        GeoIpFailed:=True;
      end;
    except
      DeleteGeoIp:=False;
      if FGeoIp <> nil then
        DeleteGeoIp:=FGeoIp.DatabaseCorrupt;
      FreeAndNil(FGeoIp);
      if DeleteGeoIp then
        DeleteFileUTF8(FGeoIpCounryDB);
      GeoIpFailed:=True;
    end;

    if (not GeoIpFailed) and (GeoIpResult = GEOIP_SUCCESS) then begin
      // This is a newly created snapshot that has not escaped Resolve yet.
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
  finally
    if LockGeoIp then
      FGeoIpLock.Leave;
  end;

  if GeoIpFailed then
    Result:=nil;
end;

end.

