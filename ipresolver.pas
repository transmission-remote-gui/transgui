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

  { TIpResolverThread }

  TIpResolver = class(TThread)
  private
    FLock: TCriticalSection;
    FResolveEvent: TEvent;
    FCache: TList;
    FResolveIp: TStringList;
    FGeoIp: TGeoIP;
  protected
    procedure Execute; override;
  public
    constructor Create(const GeoIpCounryDB: string); reintroduce;
    destructor Destroy; override;
    function Resolve(const IpAddress: string): PHostEntry;
  end;

implementation

uses dnssend, synamisc;

{ TIpResolver }

procedure TIpResolver.Execute;
var
  i: integer;
  ip: string;
  dns: TDNSSend;
  DnsServers, reply: TStringList;
  c: PHostEntry;
  GeoCountry: TGeoIPCountry;
begin
  try
    DnsServers:=TStringList.Create;
    DnsServers.CommaText:=GetDNS;
    dns:=TDNSSend.Create;
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
              c^.ImageIndex:=-1;
              c^.IP:=ip;
              UniqueString(c^.IP);
              c^.HostName:=c^.IP;
              FCache.Add(c);
            finally
              FLock.Leave;
            end;
            reply:=TStringList.Create;
            try
              for i:=0 to DnsServers.Count - 1 do begin
                dns.TargetHost:=DnsServers[i];
                if dns.DNSQuery(ip, QTYPE_PTR, reply) and (reply.Count > 0) then begin
                  FLock.Enter;
                  try
                    c^.HostName:=reply[0];
                    UniqueString(c^.HostName);
                  finally
                    FLock.Leave;
                  end;
                  break;
                end;
              end;
            finally
              reply.Free;
            end;

            if FGeoIp <> nil then begin
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
            end;
          end;

        end;
      end;
    finally
      dns.Free;
      DnsServers.Free;
    end;
  except
  end;
  Sleep(20);
end;

constructor TIpResolver.Create(const GeoIpCounryDB: string);
begin
  FLock:=TCriticalSection.Create;
  FResolveEvent:=TEvent.Create(nil, True, False, '');
  FCache:=TList.Create;
  FResolveIp:=TStringList.Create;
  if GeoIpCounryDB <> '' then
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

