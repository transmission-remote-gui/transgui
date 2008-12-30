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
    FCache: TList;
    FResolveIp: TStringList;
    FGeoIp: TGeoIP;
    FOptions: TResolverOptions;
    FGeoIpCounryDB: string;
  protected
    procedure Execute; override;
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
  GeoCountry: TGeoIPCountry;
begin
  try
    while not Terminated do begin
      if FResolveEvent.WaitFor(200) = wrSignaled then begin
        FResolveEvent.ResetEvent;

        while True do begin
          FLock.Enter;
          try
            ip:='';
            if FResolveIp.Count > 0 then begin
              ip:=FResolveIp[0];
              FResolveIp.Delete(0);
            end;
            UniqueString(ip);
          finally
            FLock.Leave;
          end;

          if ip = '' then
            break;

          FLock.Enter;
          try
            New(c);
            c^.ImageIndex:=0;
            c^.IP:=ip;
            UniqueString(c^.IP);
            c^.HostName:=c^.IP;
            FCache.Add(c);
          finally
            FLock.Leave;
          end;

          if roResolveIP in FOptions then begin
            s:=synsock.ResolveIPToName(ip, AF_INET, IPPROTO_IP, 0);
            FLock.Enter;
            try
              c^.HostName:=s;
              UniqueString(c^.HostName);
            finally
              FLock.Leave;
            end;
          end;

          if FGeoIp <> nil then
          try
            if FGeoIp.GetCountry(ip, GeoCountry) = GEOIP_SUCCESS then begin
              FLock.Enter;
              try
                c^.CountryName:=GeoCountry.CountryName;
                UniqueString(c^.CountryName);
                c^.CountryCode:=AnsiLowerCase(GeoCountry.CountryCode);
                UniqueString(c^.CountryCode);
              finally
                FLock.Leave;
              end;
            end;
          except
            FreeAndNil(FGeoIp);
            DeleteFile(FGeoIpCounryDB);
          end;
        end;

      end;
    end;
  except
  end;
  Sleep(20);
end;

constructor TIpResolver.Create(const GeoIpCounryDB: string; AOptions: TResolverOptions);
begin
  FOptions:=AOptions;
  FLock:=TCriticalSection.Create;
  FResolveEvent:=TEvent.Create(nil, True, False, '');
  FCache:=TList.Create;
  FResolveIp:=TStringList.Create;
  FGeoIpCounryDB:=GeoIpCounryDB;
  if (roResolveCountry in FOptions) and (FGeoIpCounryDB <> '') then
    FGeoIp:=TGeoIP.Create(GeoIpCounryDB);
  inherited Create(False);
end;

destructor TIpResolver.Destroy;
var
  i: integer;
begin
  Terminate;
  WaitFor;
  FResolveIp.Free;
  FResolveEvent.Free;
  FLock.Free;
  FGeoIp.Free;
  for i:=0 to FCache.Count - 1 do
    Dispose(PHostEntry(FCache[i]));
  FCache.Free;
  inherited Destroy;
end;

function TIpResolver.Resolve(const IpAddress: string): PHostEntry;
var
  i: integer;
begin
  FLock.Enter;
  try
    for i:=0 to FCache.Count - 1 do begin
      Result:=FCache[i];
      if Result^.IP = IpAddress then
        exit;
    end;
    if FResolveIp.IndexOf(IpAddress) < 0 then begin
      FResolveIp.Add(IpAddress);
      FResolveEvent.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
  Result:=nil;
end;

end.

