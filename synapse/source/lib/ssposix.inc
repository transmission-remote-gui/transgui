{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.004 |
|==============================================================================|
| Content: Socket Independent Platform Layer - Delphi Posix definition include |
|==============================================================================|
| Copyright (c)2006-2013, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2006-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|  Radek Cervinka                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@exclude}

{$IFDEF POSIX}
{for delphi XE2+}

//{$DEFINE FORCEOLDAPI}
{Note about define FORCEOLDAPI:
If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then the new API
is used, when running system allows it.

For IPv6 support you must have new API!
}

{
note RC:
partially compatible with NextGen Delphi compiler - iOS


}


interface

uses
  SyncObjs, SysUtils, Classes,
  Posix.SysSocket, Posix.SysSelect, Posix.SysTime, Posix.NetinetIn,
  Posix.StrOpts, Posix.Errno;

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

const
  DLLStackName = '';
  WinsockLevel = $0202;

  cLocalHost = '127.0.0.1';
  cBroadcast = '255.255.255.255';
  cAnyHost = '0.0.0.0';
  c6AnyHost = '::0';
  c6Localhost = '::1';
  cLocalHostStr = 'localhost';

type
  TSocket = longint;
  TAddrFamily = integer;

  TMemory = pointer;


type
  TFDSet = fd_set;
  PFDSet = Pfd_set;
  Ptimeval = Posix.SysTime.ptimeval;
  Ttimeval = Posix.SysTime.timeval;

const
  //
  FIONREAD = $4004667F; // oSX FIONREAD        = Posix.StrOpts.FIONREAD;

  FIONBIO	 = $8004667E; //OSX FIONBIO         = Posix.StrOpts.FIONBIO;
  FIOASYNC = $8004667D; //OSX  FIOASYNC        = Posix.StrOpts.FIOASYNC;  // not defined in XE2

  {  FIONREAD        = $541B; // LINUX?
  FIONBIO         = $5421;
  FIOASYNC        = $5452;   }

const
  IPPROTO_IP     =   Posix.NetinetIn.IPPROTO_IP;		{ Dummy					}
  IPPROTO_ICMP   =   Posix.NetinetIn.IPPROTO_ICMP;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   Posix.NetinetIn.IPPROTO_IGMP;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   Posix.NetinetIn.IPPROTO_TCP;		{ TCP           			}
  IPPROTO_UDP    =   Posix.NetinetIn.IPPROTO_UDP;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   Posix.NetinetIn.IPPROTO_IPV6;
  IPPROTO_ICMPV6 =   58;
  IPPROTO_RM     =  113;

  IPPROTO_RAW    =   Posix.NetinetIn.IPPROTO_RAW;
  IPPROTO_MAX    =   Posix.NetinetIn.IPPROTO_MAX;

type
  PInAddr = ^TInAddr;
  TInAddr = Posix.NetinetIn.in_addr;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = Posix.NetinetIn.sockaddr_in;


  TIP_mreq =  record
    imr_multiaddr: TInAddr;     // IP multicast address of group
    imr_interface: TInAddr;     // local IP address of interface
  end;


  PInAddr6 = ^TInAddr6;
  TInAddr6 = Posix.NetinetIn.in6_addr;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = Posix.NetinetIn.sockaddr_in6;


  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
  end;

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

Const
  IP_TOS             = Posix.NetinetIn.IP_TOS;             { int; IP type of service and precedence.  }
  IP_TTL             = Posix.NetinetIn.IP_TTL;             { int; IP time to live.  }
  IP_HDRINCL         = Posix.NetinetIn.IP_HDRINCL;         { int; Header is included with data.  }
  IP_OPTIONS         = Posix.NetinetIn.IP_OPTIONS;         { ip_opts; IP per-packet options.  }
//  IP_ROUTER_ALERT    = sockets.IP_ROUTER_ALERT;    { bool }
  IP_RECVOPTS        = Posix.NetinetIn.IP_RECVOPTS;        { bool }
  IP_RETOPTS         = Posix.NetinetIn.IP_RETOPTS;         { bool }
//  IP_PKTINFO         = sockets.IP_PKTINFO;         { bool }
//  IP_PKTOPTIONS      = sockets.IP_PKTOPTIONS;
//  IP_PMTUDISC        = sockets.IP_PMTUDISC;        { obsolete name? }
//  IP_MTU_DISCOVER    = sockets.IP_MTU_DISCOVER;    { int; see below }
//  IP_RECVERR         = sockets.IP_RECVERR;         { bool }
//  IP_RECVTTL         = sockets.IP_RECVTTL;         { bool }
//  IP_RECVTOS         = sockets.IP_RECVTOS;         { bool }
  IP_MULTICAST_IF    = Posix.NetinetIn.IP_MULTICAST_IF;    { in_addr; set/get IP multicast i/f }
  IP_MULTICAST_TTL   = Posix.NetinetIn.IP_MULTICAST_TTL;   { u_char; set/get IP multicast ttl }
  IP_MULTICAST_LOOP  = Posix.NetinetIn.IP_MULTICAST_LOOP;  { i_char; set/get IP multicast loopback }
  IP_ADD_MEMBERSHIP  = Posix.NetinetIn.IP_ADD_MEMBERSHIP;  { ip_mreq; add an IP group membership }
  IP_DROP_MEMBERSHIP = Posix.NetinetIn.IP_DROP_MEMBERSHIP; { ip_mreq; drop an IP group membership }

  SOL_SOCKET    = Posix.SysSocket.SOL_SOCKET;

  SO_DEBUG      = Posix.SysSocket.SO_DEBUG;
  SO_REUSEADDR  = Posix.SysSocket.SO_REUSEADDR;
  SO_TYPE       = Posix.SysSocket.SO_TYPE;
  SO_ERROR      = Posix.SysSocket.SO_ERROR;
  SO_DONTROUTE  = Posix.SysSocket.SO_DONTROUTE;
  SO_BROADCAST  = Posix.SysSocket.SO_BROADCAST;
  SO_SNDBUF     = Posix.SysSocket.SO_SNDBUF;
  SO_RCVBUF     = Posix.SysSocket.SO_RCVBUF;
  SO_KEEPALIVE  = Posix.SysSocket.SO_KEEPALIVE;
  SO_OOBINLINE  = Posix.SysSocket.SO_OOBINLINE;
//  SO_NO_CHECK   = SysSocket.SO_NO_CHECK;
//  SO_PRIORITY   = SysSocket.SO_PRIORITY;
  SO_LINGER     = Posix.SysSocket.SO_LINGER;
//  SO_BSDCOMPAT  = SysSocket.SO_BSDCOMPAT;
//  SO_REUSEPORT  = SysSocket.SO_REUSEPORT;
//  SO_PASSCRED   = SysSocket.SO_PASSCRED;
//  SO_PEERCRED   = SysSocket.SO_PEERCRED;
  SO_RCVLOWAT   = Posix.SysSocket.SO_RCVLOWAT;
  SO_SNDLOWAT   = Posix.SysSocket.SO_SNDLOWAT;
  SO_RCVTIMEO   = Posix.SysSocket.SO_RCVTIMEO;
  SO_SNDTIMEO   = Posix.SysSocket.SO_SNDTIMEO;
{ Security levels - as per NRL IPv6 - don't actually do anything }
//  SO_SECURITY_AUTHENTICATION       = SysSocket.SO_SECURITY_AUTHENTICATION;
//  SO_SECURITY_ENCRYPTION_TRANSPORT = SysSocket.SO_SECURITY_ENCRYPTION_TRANSPORT;
//  SO_SECURITY_ENCRYPTION_NETWORK   = SysSocket.SO_SECURITY_ENCRYPTION_NETWORK;
//  SO_BINDTODEVICE                  = SysSocket.SO_BINDTODEVICE;
{ Socket filtering }
//  SO_ATTACH_FILTER = SysSocket.SO_ATTACH_FILTER;
//  SO_DETACH_FILTER = SysSocket.SO_DETACH_FILTER;

  SOMAXCONN       = 1024;

  IPV6_UNICAST_HOPS     = Posix.NetinetIn.IPV6_UNICAST_HOPS;
  IPV6_MULTICAST_IF     = Posix.NetinetIn.IPV6_MULTICAST_IF;
  IPV6_MULTICAST_HOPS   = Posix.NetinetIn.IPV6_MULTICAST_HOPS;
  IPV6_MULTICAST_LOOP   = Posix.NetinetIn.IPV6_MULTICAST_LOOP;
  IPV6_JOIN_GROUP       = Posix.NetinetIn.IPV6_JOIN_GROUP;
  IPV6_LEAVE_GROUP      = Posix.NetinetIn.IPV6_LEAVE_GROUP;

const
  SOCK_STREAM     = Posix.SysSocket.SOCK_STREAM;// 1;               { stream socket }
  SOCK_DGRAM      = Posix.SysSocket.SOCK_DGRAM;// 2;               { datagram socket }
  SOCK_RAW        = Posix.SysSocket.SOCK_RAW;// 3;               { raw-protocol interface }
  SOCK_RDM        = Posix.SysSocket.SOCK_RDM;// 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = Posix.SysSocket.SOCK_SEQPACKET;// 5;               { sequenced packet stream }

{ TCP options. }
  TCP_NODELAY     = $0001; //netinettcp.pas

{ Address families. }

  AF_UNSPEC       = Posix.SysSocket.AF_UNSPEC;// 0;               { unspecified }
  AF_INET         = Posix.SysSocket.AF_INET;  // 2;               { internetwork: UDP, TCP, etc. }
  AF_INET6        = Posix.SysSocket.AF_INET6; // !! 30            { Internetwork Version 6 }
  AF_MAX          = Posix.SysSocket.AF_MAX;   // !! - variable by OS

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = Posix.SysSocket.linger;

const

  MSG_OOB       = Posix.SysSocket.MSG_OOB;      // Process out-of-band data.
  MSG_PEEK      = Posix.SysSocket.MSG_PEEK;     // Peek at incoming messages.
  {$IFDEF MACOS}
  MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
                           // Works under MAC OS X, but is undocumented,
                           // So FPC doesn't include it
  {$ELSE}
   MSG_NOSIGNAL  = $4000; // Do not generate SIGPIPE.
  {$ENDIF}

const
  WSAEINTR = EINTR;
  WSAEBADF = EBADF;
  WSAEACCES = EACCES;
  WSAEFAULT = EFAULT;
  WSAEINVAL = EINVAL;
  WSAEMFILE = EMFILE;
  WSAEWOULDBLOCK = EWOULDBLOCK;
  WSAEINPROGRESS = EINPROGRESS;
  WSAEALREADY = EALREADY;
  WSAENOTSOCK = ENOTSOCK;
  WSAEDESTADDRREQ = EDESTADDRREQ;
  WSAEMSGSIZE = EMSGSIZE;
  WSAEPROTOTYPE = EPROTOTYPE;
  WSAENOPROTOOPT = ENOPROTOOPT;
  WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = EOPNOTSUPP;
  WSAEPFNOSUPPORT = EPFNOSUPPORT;
  WSAEAFNOSUPPORT = EAFNOSUPPORT;
  WSAEADDRINUSE = EADDRINUSE;
  WSAEADDRNOTAVAIL = EADDRNOTAVAIL;
  WSAENETDOWN = ENETDOWN;
  WSAENETUNREACH = ENETUNREACH;
  WSAENETRESET = ENETRESET;
  WSAECONNABORTED = ECONNABORTED;
  WSAECONNRESET = ECONNRESET;
  WSAENOBUFS = ENOBUFS;
  WSAEISCONN = EISCONN;
  WSAENOTCONN = ENOTCONN;
  WSAESHUTDOWN = ESHUTDOWN;
  WSAETOOMANYREFS = ETOOMANYREFS;
  WSAETIMEDOUT = ETIMEDOUT;
  WSAECONNREFUSED = ECONNREFUSED;
  WSAELOOP = ELOOP;
  WSAENAMETOOLONG = ENAMETOOLONG;
  WSAEHOSTDOWN = EHOSTDOWN;
  WSAEHOSTUNREACH = EHOSTUNREACH;
  WSAENOTEMPTY = ENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = EUSERS;
  WSAEDQUOT = EDQUOT;
  WSAESTALE = ESTALE;
  WSAEREMOTE = EREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = 1;
  WSATRY_AGAIN = 2;
  WSANO_RECOVERY = 3;
  WSANO_DATA = -6;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;

  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);

var
  in6addr_any, in6addr_loopback : TInAddr6;

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{=============================================================================}

var
  SynSockCS: SyncObjs.TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

  {$DEFINE SOCK_HAS_SINLEN} // OSX

type
  TVarSin = packed record
  {$ifdef SOCK_HAS_SINLEN}
     sin_len     : UInt8;
  {$endif}

    case integer of
      0: (AddressFamily: sa_family_t);
      1: (
        case sin_family: sa_family_t of
          AF_INET: (sin_port: word;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of Char);
          AF_INET6: (sin6_port:     word;
                		sin6_flowinfo: longword;
      	    	      sin6_addr:     TInAddr6;
      		          sin6_scope_id: longword);
          );
  end;

function SizeOfVarSin(sin: TVarSin): integer;

  function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
  function WSACleanup: Integer;
  function WSAGetLastError: Integer;
  function GetHostName: string;
  function Shutdown(s: TSocket; how: Integer): Integer;
  function SetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    optlen: Integer): Integer;
  function GetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    var optlen: Integer): Integer;
  function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
  function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
  function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
  function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
  function ntohs(netshort: word): word;
  function ntohl(netlong: longword): longword;
  function Listen(s: TSocket; backlog: Integer): Integer;
  function IoctlSocket(s: TSocket; cmd: Integer; var arg: integer): Integer;
  function htons(hostshort: word): word;
  function htonl(hostlong: longword): longword;
  function GetSockName(s: TSocket; var name: TVarSin): Integer;
  function GetPeerName(s: TSocket; var name: TVarSin): Integer;
  function Connect(s: TSocket; const name: TVarSin): Integer;
  function CloseSocket(s: TSocket): Integer;
  function Bind(s: TSocket; const addr: TVarSin): Integer;
  function Accept(s: TSocket; var addr: TVarSin): TSocket;
  function Socket(af, Struc, Protocol: Integer): TSocket;
  function Select(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint;

function IsNewApi(Family: integer): Boolean;
function SetVarSin(var Sin: TVarSin; IP, Port: string; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(Sin: TVarSin): string;
function GetSinPort(Sin: TVarSin): Integer;
procedure ResolveNameToIP(Name: string;  Family, SockProtocol, SockType: integer; const IPList: TStrings);
function ResolveIPToName(IP: string; Family, SockProtocol, SockType: integer): string;
function ResolvePort(Port: string; Family, SockProtocol, SockType: integer): Word;


{==============================================================================}
implementation
uses
  Posix.Base, Posix.Unistd, Posix.ArpaInet, Posix.NetDB;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := Posix.NetinetIn.IN6_IS_ADDR_UNSPECIFIED(a^);
{  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));}
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := Posix.NetinetIn.IN6_IS_ADDR_LOOPBACK(a^);
{  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));}
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := Posix.NetinetIn.IN6_IS_ADDR_LINKLOCAL(a^);
{  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));}
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := Posix.NetinetIn.IN6_IS_ADDR_SITELOCAL(a^);
//  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := Posix.NetinetIn.IN6_IS_ADDR_MULTICAST(a^);
//  Result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.__s6_addr8[15] := 1;
end;

{$IFDEF NEXTGEN}
function GetHostByName(const name: string):Phostent;
var
  h: Phostent;
begin
  h := Posix.NetDB.gethostbyname(MarshaledAString(TMarshal.AsAnsi(name)));
end;
{$ENDIF}
{=============================================================================}

function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synsock - Synapse Platform Independent Socket Layer';
    szSystemStatus := 'Running on Posix by Delphi';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function WSACleanup: Integer;
begin
  Result := 0;
end;

function WSAGetLastError: Integer;
begin
  Result := Posix.Errno.errno;
end;

function FD_ISSET(Socket: TSocket; var fdset: TFDSet): Boolean;
begin
  Result := __FD_ISSET(socket, fdset);
end;

procedure FD_SET(Socket: TSocket; var fdset: TFDSet);
begin
  __FD_SET(Socket, fdset);
end;

procedure FD_CLR(Socket: TSocket; var fdset: TFDSet);
begin
  __FD_CLR(Socket, fdset);
end;

procedure FD_ZERO(var fdset: TFDSet);
begin
  __FD_ZERO(fdset);
end;

{=============================================================================}

function SizeOfVarSin(sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:
            Result := SizeOf(TSockAddrIn);
    AF_INET6:
            Result := SizeOf(TSockAddrIn6);
  else
    Result := 0;
  end;
end;

{=============================================================================}

function Bind(s: TSocket; const addr: TVarSin): Integer;
var
  sa: sockaddr absolute addr;
begin
  Result :=  Posix.SysSocket.Bind(s, sa, SizeOfVarSin(addr));
end;

function Connect(s: TSocket; const name: TVarSin): Integer;
var
  sa: sockaddr absolute name;
begin
  Result := Posix.SysSocket.Connect(s,  sa, SizeOfVarSin(name));
end;

function GetSockName(s: TSocket; var name: TVarSin): Integer;
var
  len: socklen_t;
  address : sockaddr absolute name;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := Posix.SysSocket.GetSockName(s, address, Len);
end;

function GetPeerName(s: TSocket; var name: TVarSin): Integer;
var
  len: socklen_t;
  address : sockaddr absolute name;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := Posix.SysSocket.GetPeerName(s, address, Len);
end;

function GetHostName: string;
{$IFDEF NEXTGEN}
var
  name: TArray<Byte>;
const
  cMaxHostLength = 255;
begin
  SetLength(name, cMaxHostLength);
  if Posix.Unistd.GetHostName(MarshaledAString(name), cMaxHostLength) = 0 then
    Result := TEncoding.UTF8.GetString(name).ToUpper;
{$ELSE}
var
  s: AnsiString;
begin
  Result := '';
  setlength(s, 255);
  Posix.Unistd.GetHostName(PAnsiChar(s), Length(s) - 1);
  Result := PChar(string(s));
{$ENDIF}
end;

function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
begin
  Result := Posix.SysSocket.Send(s, Buf^, len, flags);
end;

function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
begin
  Result := Posix.SysSocket.Recv(s, Buf^, len, flags);
end;

function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
var
  sa: sockaddr absolute addrto;
begin
  Result := Posix.SysSocket.SendTo(s, Buf^, len, flags, sa, SizeOfVarSin(addrto));
end;

function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
var
  x: socklen_t;
  address : sockaddr absolute from;
begin
  x := SizeOf(from);
  Result := Posix.SysSocket.RecvFrom(s, Buf^, len, flags, address, x);
end;

function Accept(s: TSocket; var addr: TVarSin): TSocket;
var
  x: socklen_t;
  address : sockaddr absolute addr;
begin
  x := SizeOf(addr);
  Result := Posix.SysSocket.Accept(s, address, x);
end;

function Shutdown(s: TSocket; how: Integer): Integer;
begin
  Result := Posix.SysSocket.Shutdown(s, how);
end;

function SetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  optlen: Integer): Integer;
begin
  Result := Posix.SysSocket.setsockopt(s, level, optname, pointer(optval), optlen);
end;

function GetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  var optlen: Integer): Integer;
var
  x: socklen_t;
begin
  x := optlen;
  Result := Posix.SysSocket.getsockopt(s, level, optname, pointer(optval), x);
  optlen := x;
end;

function  ntohs(netshort: word): word;
begin
  Result := Posix.ArpaInet.ntohs(NetShort);
end;

function  ntohl(netlong: longword): longword;
begin
  Result := Posix.ArpaInet.ntohl(NetLong);
end;

function  Listen(s: TSocket; backlog: Integer): Integer;
begin
  if Posix.SysSocket.Listen(s, backlog) = 0 then
    Result := 0
  else
    Result := SOCKET_ERROR;
end;

function  IoctlSocket(s: TSocket; cmd: Integer; var arg: integer): Integer;
begin
  Result := Posix.StrOpts.Ioctl(s, cmd, @arg);
end;

function  htons(hostshort: word): word;
begin
  Result := Posix.ArpaInet.htons(Hostshort);
end;

function  htonl(hostlong: longword): longword;
begin
  Result := Posix.ArpaInet.htonl(HostLong);
end;

function CloseSocket(s: TSocket): Integer;
begin
  Result := Posix.Unistd.__close(s);
end;

function Socket(af, Struc, Protocol: Integer): TSocket;
begin
  Result := Posix.SysSocket.Socket(af, struc, protocol);
end;

function Select(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): Longint;
begin
  Result := Posix.SysSelect.Select(nfds, readfds, writefds, exceptfds, timeout);
end;

{=============================================================================}
function IsNewApi(Family: integer): Boolean;
begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (Family = AF_INET6) and SockWship6Api;
end;

{$IFDEF NEXTGEN}
function GetAddrInfo(hostname, servname: string;
  const hints: addrinfo; out res: Paddrinfo): Integer;
begin

end;
{$ENDIF}

function gethostbyname(const name: PAnsiChar): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyname';
function gethostbyaddr(var addr; len: socklen_t; atype: integer): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyaddr';

function SetVarSin(var Sin: TVarSin; IP, Port: string; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  r: integer;
  Hints1, Hints2: AddrInfo;
  Sin1, Sin2: TVarSin;
  TwoPass: boolean;

  function GetAddr(const IP, port: string; Hints: AddrInfo; var Sin: TVarSin): integer;
  var
    Addr: PAddrInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then
      begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        Result := GetAddrInfo(PAnsiChar(AnsiString(IP)), nil, Hints, Addr);
      end
      else
      begin
        if (IP = cAnyHost) or (IP = c6AnyHost) then
        begin
          Hints.ai_flags := AI_PASSIVE;
          Result := GetAddrInfo(nil, PAnsiChar(AnsiString(Port)), Hints, Addr);
        end
        else
          if (IP = cLocalhost) or (IP = c6Localhost) then
          begin
            Result := GetAddrInfo(nil, PAnsiChar(AnsiString(Port)), Hints, Addr);
          end
          else
          begin
            Result := GetAddrInfo(PAnsiChar(AnsiString(IP)), PAnsiChar(AnsiString(Port)), Hints, Addr);
          end;
      end;
      if Result = 0 then
        if (Addr <> nil) then
          Move(Addr^.ai_addr^, Sin, Addr^.ai_addrlen);
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr^);
    end;
  end;

begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  if not IsNewApi(family) then
  begin
    SynSockCS.Enter;
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
      {$IFDEF NEXTGEN}
        ServEnt := GetServByName(MarshaledAString(TMarshal.AsAnsi(Port)), ProtoEnt^.p_name);
      {$ELSE}
        ServEnt := GetServByName(PAnsiChar(AnsiString(Port)), ProtoEnt^.p_name);
      {$ENDIF}
      if ServEnt = nil then
        Sin.sin_port := htons(StrToIntDef(Port, 0))
      else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := UInt32(INADDR_BROADCAST)
      else
      begin
      {$IFDEF NEXTGEN}
        Sin.sin_addr.s_addr := inet_addr(MarshaledAString(TMarshal.AsAnsi(IP)));
      {$ELSE}
        Sin.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(IP)));
      {$ENDIF}
        if Sin.sin_addr.s_addr = UInt32(INADDR_NONE) then
        begin
          {$IFDEF NEXTGEN}
          HostEnt := GetHostByName(MarshaledAString(TMarshal.AsAnsi(IP)));
          {$ELSE}
          HostEnt := GetHostByName(PAnsiChar(AnsiString(IP)));
          {$ENDIF}
          Result := WSAGetLastError;
          if HostEnt <> nil then
            Sin.sin_addr.S_addr := UInt32(HostEnt.h_addr_list);
        end;
      end;
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    FillChar(Hints1, Sizeof(Hints1), 0);
    FillChar(Hints2, Sizeof(Hints2), 0);
    TwoPass := False;
    if Family = AF_UNSPEC then
    begin
      if PreferIP4 then
      begin
        Hints1.ai_family := AF_INET;
        Hints2.ai_family := AF_INET6;
        TwoPass := True;
      end
      else
      begin
        Hints2.ai_family := AF_INET;
        Hints1.ai_family := AF_INET6;
        TwoPass := True;
      end;
    end
    else
      Hints1.ai_family := Family;

    Hints1.ai_socktype := SockType;
    Hints1.ai_protocol := SockProtocol;
    Hints2.ai_socktype := Hints1.ai_socktype;
    Hints2.ai_protocol := Hints1.ai_protocol;

    r := GetAddr(IP, Port, Hints1, Sin1);
    Result := r;
    sin := sin1;
    if r <> 0 then
      if TwoPass then
      begin
        r := GetAddr(IP, Port, Hints2, Sin2);
        Result := r;
        if r = 0 then
          sin := sin2;
      end;
  end;
end;

function GetSinIP(Sin: TVarSin): string;
var
  p: PAnsiChar;
  hostlen, servlen: integer;
  r: integer;
  sa:sockaddr absolute Sin;
  byHost, byServ: TBytes;
  HostWrapper, ServWrapper: TPtrWrapper;
begin
  Result := '';
  if not IsNewApi(Sin.AddressFamily) then
  begin
    p := inet_ntoa(Sin.sin_addr);
    if p <> nil then
      Result := string(p);
  end
  else
  begin
    // NEXTGEN compatible
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    Setlength(byHost, hostLen);
    Setlength(byServ, hostLen);
    HostWrapper := TPtrWrapper.Create(@byHost[0]);
    ServWrapper := TPtrWrapper.Create(@byServ[0]);
    r := getnameinfo(sa, SizeOfVarSin(sin), HostWrapper.ToPointer, hostlen,
      ServWrapper.ToPointer, servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := TMarshal.ReadStringAsAnsi(HostWrapper{, NI_MAXHOST});
  end;
end;

function GetSinPort(Sin: TVarSin): Integer;
begin
  if (Sin.sin_family = AF_INET6) then
    Result := synsock.ntohs(Sin.sin6_port)
  else
    Result := synsock.ntohs(Sin.sin_port);
end;

procedure ResolveNameToIP(Name: string; Family, SockProtocol, SockType: integer; const IPList: TStrings);
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  Hints: AddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IP: UINT32;
  PAdrPtr: PaPInAddr;
  i: Integer;
  s: string;
  InAddr: TInAddr;
  aby:TArray<Byte>;
begin
  IPList.Clear;
  if not IsNewApi(Family) then
  begin
  {$IFDEF NEXTGEN}
    IP := inet_addr(MarshaledAString(TMarshal.AsAnsi(Name)));
  {$ELSE}
    IP := inet_addr(PAnsiChar(AnsiString(Name)));
  {$ENDIF}
    if IP = UINT32(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
  {$IFDEF NEXTGEN}
        RemoteHost := GetHostByName(MarshaledAString(TMarshal.AsAnsi(Name)));
  {$ELSE}
        RemoteHost := GetHostByName(PAnsiChar(AnsiString(Name)));
  {$ENDIF}
        if RemoteHost <> nil then
        begin
          PAdrPtr := PAPInAddr(RemoteHost^.h_addr_list);
          i := 0;
          while PAdrPtr^[i] <> nil do
          begin
            InAddr := PAdrPtr^[i]^;
            aby := TArray<byte>(InAddr);
            s := Format('%d.%d.%d.%d', [aby[0], aby[1],
              aby[2], aby[3]]);
            IPList.Add(s);
            Inc(i);
          end;
        end;
      finally
        SynSockCS.Leave;
      end;
    end
    else
      IPList.Add(Name);
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      Hints.ai_flags := 0;
      r := GetAddrInfo(PAnsiChar(AnsiString(Name)), nil, Hints, Addr);
      if r = 0 then
      begin
        AddrNext := Addr;
        while not(AddrNext = nil) do
        begin
          if not(((Family = AF_INET6) and (AddrNext^.ai_family = AF_INET))
            or ((Family = AF_INET) and (AddrNext^.ai_family = AF_INET6))) then
          begin
            hostlen := NI_MAXHOST;
            servlen := NI_MAXSERV;
            setlength(host, hostlen);
            setlength(serv, servlen);
{$IFDEF NEXTGEN}
            r := getnameinfo(AddrNext^.ai_addr^, AddrNext^.ai_addrlen,
              MarshaledAString(TMarshal.AsAnsi(host)), hostlen, MarshaledAString(TMarshal.AsAnsi(serv)), servlen,
              NI_NUMERICHOST + NI_NUMERICSERV);
{$ELSE}
            r := getnameinfo(AddrNext^.ai_addr^, AddrNext^.ai_addrlen,
              PAnsiChar(AnsiString(host)), hostlen, PAnsiChar(AnsiString(serv)), servlen,
              NI_NUMERICHOST + NI_NUMERICSERV);
{$ENDIF}
            if r = 0 then
            begin
              host := PChar(host);
              IPList.Add(host);
            end;
          end;
          AddrNext := AddrNext^.ai_next;
        end;
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr^);
    end;
  end;
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
end;

function ResolvePort(Port: string; Family, SockProtocol, SockType: integer): Word;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  Hints: AddrInfo;
  Addr: PAddrInfo;
  _Addr: AddrInfo;
  r: integer;
begin
  Result := 0;
  if not IsNewApi(Family) then
  begin
    SynSockCS.Enter;
    try
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := GetServByName(PAnsiChar(AnsiString(Port)), ProtoEnt^.p_name);
      if ServEnt = nil then
        Result := StrToIntDef(Port, 0)
      else
        Result := htons(ServEnt^.s_port);
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := Sockprotocol;
      Hints.ai_flags := AI_PASSIVE;
      r := GetAddrInfo(nil, PAnsiChar(AnsiString(Port)), Hints, Addr);
      if (r = 0) and Assigned(Addr) then
      begin
        if Addr^.ai_family = AF_INET then
          Result := htons(Addr^.ai_addr^.sa_data[0]); // port
        if Addr^.ai_family = AF_INET6 then
          Result := htons(PSockAddrIn6(Addr^.ai_addr)^.sin6_port);
      end;
    finally
      if Assigned(Addr) then
      begin
        _Addr := Addr^;
        FreeAddrInfo(_Addr);
      end;
    end;
  end;
end;

function ResolveIPToName(IP: string; Family, SockProtocol, SockType: integer): string;
var
  Hints: AddrInfo;
  Addr: PAddrInfo;
  _Addr: AddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IPn: UINT32;
begin
  Result := IP;
  if not IsNewApi(Family) then
  begin
    IPn := inet_addr(PAnsiChar(AnsiString(IP)));
    if IPn <> UINT32(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
        RemoteHost := GetHostByAddr(IPn, SizeOf(IPn), AF_INET);
        if RemoteHost <> nil then
          Result := string(RemoteHost^.hname);
      finally
        SynSockCS.Leave;
      end;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      Hints.ai_flags := 0;
      r := GetAddrInfo(PAnsiChar(AnsiString(IP)), nil, Hints, Addr);
      if (r = 0) and Assigned(Addr)then
      begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        setlength(host, hostlen);
        setlength(serv, servlen);
        r := getnameinfo(Addr^.ai_addr^, Addr^.ai_addrlen,
          PAnsiChar(AnsiString(host)), hostlen, PAnsiChar(AnsiString(serv)), servlen,
          NI_NUMERICSERV);
        if r = 0 then
          Result := PChar(host);
      end;
    finally
      if Assigned(Addr) then
      begin
        _Addr := Addr^;
        FreeAddrInfo(_Addr);
      end;
    end;
  end;
end;

{=============================================================================}

function InitSocketInterface(stack: string): Boolean;
begin
  SockEnhancedApi := True;
  SockWship6Api := False;
//  Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
  Result := True;
end;

function DestroySocketInterface: Boolean;
begin
  Result := True;
end;

initialization
begin
  SynSockCS := SyncObjs.TCriticalSection.Create;
  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
end;

finalization
begin
  SynSockCS.Free;
end;

{$ENDIF}
