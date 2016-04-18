unit ufilesmac;

interface

{$mode objfpc}{$h+}

uses
  Unix, ufilescore, ufilesutf8, MacOSAll, osxattr;

implementation

function MacCompareFileName(const name1, name2: UnicodeString; CaseSensitive: Boolean): Integer;
var
  F1,F2: CFStringRef;
const
  CompareFlags : array [Boolean] of CFStringCompareFlags = (
    kCFCompareNonliteral,
    kCFCompareNonliteral or kCFCompareCaseInsensitive
  );
begin
  if name1=name2 then exit(0);
  if name1='' then Exit(1);
  if name2='' then Exit(-1);
  F1:=CFStringCreateWithCharacters(nil,@name1[1], length(name1));
  F2:=CFStringCreateWithCharacters(nil,@name2[1], length(name2));
  Result:=CFStringCompare(F1,F2,CompareFlags[CaseSensitive]);
  CFRelease(F1);
  CFRelease(F2);
end;




function MacPathCaseSensitive(const APath: UnicodeString; var CaseSensitive: Boolean): Boolean;
type
  vol_caps_buf_t = packed record
    size : LongWord;
    caps : vol_capabilities_attr_t;
  end;

var
  info  : tstatfs;
  s     : string;

  alist   : attrlist;
  buffer  : vol_caps_buf_t;
begin

  FillChar(info, sizeof(info), 0);
  s:=UTF8Encode(APath);
  Result:=fpStatFS( PAnsiChar(s), @info) = 0;
  if not Result then Exit;

  FillChar(alist, sizeof(alist), 0);
  alist.bitmapcount:=ATTR_BIT_MAP_COUNT;
  alist.volattr:=ATTR_VOL_CAPABILITIES;
  getattrlist(PAnsiChar(s), nil, nil, 0, 0);

  // getattrlist requires the path to the actual mount point.
  Result:=getattrlist(info.mountpoint, @alist, @buffer, sizeof(buffer), 0)=0;
  if not Result then Exit;

  Result:= (alist.volattr and ATTR_VOL_CAPABILITIES) > 0;
  if Result then begin
    Result:=(buffer.caps.valid[VOL_CAPABILITIES_FORMAT] and VOL_CAP_FMT_CASE_SENSITIVE) > 0;
    CaseSensitive:=(buffer.caps.capabilities[VOL_CAPABILITIES_FORMAT] and VOL_CAP_FMT_CASE_SENSITIVE) > 0;
  end;
end;

initialization
  DoCompareFileName:=@MacCompareFileName;
  DoPathCaseSensitive:=@MacPathCaseSensitive;
  RegisterUtf8AsAnsiNames;

end.
