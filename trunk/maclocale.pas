{
    This file is part of the Free Pascal packages library.
    Copyright (c) 2013 by Yury Sidorov, member of the
    Free Pascal development team

    This unit is used to get format settings of the current
    Mac OS UI locale. Works on Mac OS X 10.3 or later.

    Function CFStringToStr() has been taken from CarbonProc LCL unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit MacLocale;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, MacOSAll;

procedure GetMacFormatSettings(var ASettings: TFormatSettings);

implementation

{------------------------------------------------------------------------------
  Name:    CFStringToStr
  Params:  AString  - Core Foundation string ref
           Encoding - Result data encoding format
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = kCFStringEncodingUTF8): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

function StrOfChar(c: AnsiChar; Count: integer): ansistring;
begin
  SetLength(Result, Count);
  FillChar(PAnsiChar(Result)^, Count, c);
end;

function ConvertFormatStr(const fmt: ansistring): ansistring;
var
  cnt: integer;
  c, q: AnsiChar;
  p: PAnsiChar;
  s: ansistring;
begin
  Result:='';
  q:=#0;
  cnt:=1;
  p:=PAnsiChar(fmt);
  while p^ <> #0 do begin
    s:='';
    c:=p^;
    if c in ['''', '"'] then begin
      if q = #0 then
        q:=c
      else
        if c = q then begin
          q:=#0;
          cnt:=1;
        end;
      s:=c;
    end
    else
      if q <> #0 then
        s:=c
      else begin
        if (p + 1)^ = c then
          Inc(cnt)
        else begin
          case c of
            'y', 'Y':
              begin
                c:='y';
                if cnt > 2 then
                  cnt:=4
                else
                  cnt:=2;
              end;
            'M', 'L':
              begin
                c:='m';
                if cnt > 4 then
                  cnt:=3;
              end;
            'd':
              if cnt > 2 then
                cnt:=2;
            'E', 'e', 'c':
              begin
                c:='d';
                if (cnt < 3) or (cnt > 4) then
                  cnt:=3;
              end;
            'a':
              begin
                cnt:=0;
                s:='ampm';
              end;
            'h', 'H', 'k', 'K':
              begin
                c:='h';
                if cnt > 2 then
                  cnt:=2;
              end;
            'm':
              begin
                c:='n';
                if cnt > 2 then
                  cnt:=2;
              end;
            's':
              if cnt > 2 then
                cnt:=2;
            'S':
              begin
                c:='z';
                cnt:=1;
              end;
            'G','u','Q','q','w','W','D','F','g','A','z','Z','v':
              cnt:=0;
          end;
          if cnt > 0 then
            s:=StrOfChar(c, cnt);
          cnt:=1;
        end;
      end;
    Inc(p);
    if s <> '' then
      Result:=Result + s;
  end;
end;

procedure GetMacFormatSettings(var ASettings: TFormatSettings);
var
  loc: CFLocaleRef;

  function _GetFormat(dateStyle: CFDateFormatterStyle; timeStyle: CFDateFormatterStyle; const DefFormat: string): string;
  var
    fmt: CFDateFormatterRef;
  begin
    Result:='';
    fmt:=CFDateFormatterCreate(nil, loc, dateStyle, timeStyle);
    if fmt <> nil then begin
      Result:=ConvertFormatStr(CFStringToStr(CFDateFormatterGetFormat(fmt)));
      CFRelease(fmt);
    end;
    if Result = '' then
      Result:=DefFormat;
  end;

  function _DateToStr(fmt: CFDateFormatterRef; const AFormat: ansistring; AYear: integer; AMonth, ADay, AHour: byte;
                      const ADefault: ansistring): ansistring;
  var
    cs: CFStringRef;
    gd: CFGregorianDate;
    at: CFAbsoluteTime;
    tz: CFTimeZoneRef;
  begin
    cs:=CFStringCreateWithCString(nil, Pointer(PAnsiChar(AFormat)), kCFStringEncodingUTF8);
    CFDateFormatterSetFormat(fmt, cs);
    CFRelease(cs);
    FillChar(gd, SIzeOf(gd), 0);
    gd.year:=AYear;
    gd.month:=AMonth;
    gd.day:=ADay;
    gd.hour:=AHour;
    tz:=CFTimeZoneCopyDefault;
    at:=CFGregorianDateGetAbsoluteTime(gd, tz);
    CFRelease(tz);
    cs:=CFDateFormatterCreateStringWithAbsoluteTime(nil, fmt, at);
    Result:=CFStringToStr(cs);
    CFRelease(cs);
    if Result = '' then
      Result:=ADefault;
  end;

  function _GetSeparator(dateStyle: CFDateFormatterStyle; timeStyle: CFDateFormatterStyle; DefSep: char): char;
  var
    fmt: CFDateFormatterRef;
    s: ansistring;
    p: PAnsiChar;
  begin
    Result:=DefSep;
    fmt:=CFDateFormatterCreate(nil, loc, dateStyle, timeStyle);
    if fmt <> nil then begin
      s:=_DateToStr(fmt, CFStringToStr(CFDateFormatterGetFormat(fmt)), 2000, 1, 1, 0, DefSep);
      s:=Trim(s);
      p:=PAnsiChar(s);
      while p^ <> #0 do
        if (p^ > ' ') and (p^ < 'A') and not (p^ in ['0'..'9']) then begin
          Result:=p^;
          break;
        end
        else
          Inc(p);
      CFRelease(fmt);
    end;
  end;

var
  s: string;
  fmt: CFDateFormatterRef;
  i: integer;
begin
  with ASettings do begin
    loc:=CFLocaleCopyCurrent;
    if loc = nil then
      exit;
    s:=CFStringToStr(CFLocaleGetValue(loc, kCFLocaleDecimalSeparator));
    if Length(s) = 1 then
      DecimalSeparator:=s[1];
    s:=CFStringToStr(CFLocaleGetValue(loc, kCFLocaleGroupingSeparator));
    if Length(s) = 1 then
      ThousandSeparator:=s[1]
    else
      ThousandSeparator:=' ';  // Unicode char has been returned. Probably it is a whitespace
    CurrencyString:=CFStringToStr(CFLocaleGetValue(loc, kCFLocaleCurrencySymbol));

    DateSeparator:=_GetSeparator(kCFDateFormatterShortStyle, kCFDateFormatterNoStyle, DateSeparator);
    TimeSeparator:=_GetSeparator(kCFDateFormatterNoStyle, kCFDateFormatterShortStyle, TimeSeparator);

    LongDateFormat:=_GetFormat(kCFDateFormatterLongStyle, kCFDateFormatterNoStyle, LongDateFormat);
    ShortDateFormat:=_GetFormat(kCFDateFormatterShortStyle, kCFDateFormatterNoStyle, ShortDateFormat);
    LongTimeFormat:=_GetFormat(kCFDateFormatterNoStyle, kCFDateFormatterLongStyle, LongTimeFormat);
    ShortTimeFormat:=_GetFormat(kCFDateFormatterNoStyle, kCFDateFormatterShortStyle, ShortTimeFormat);

    fmt:=CFDateFormatterCreate(nil, loc, kCFDateFormatterNoStyle, kCFDateFormatterNoStyle);
    if fmt <> nil then begin
      for i:=1 to 12 do begin
        LongMonthNames[i]:=_DateToStr(fmt, 'LLLL', 2006, i, 1, 0, LongMonthNames[i]);
        ShortMonthNames[i]:=_DateToStr(fmt, 'LLL', 2006, i, 1, 0, ShortMonthNames[i]);
      end;
      for i:=1 to 7 do begin
        LongDayNames[i]:=_DateToStr(fmt, 'cccc', 2006, 1, i, 0, LongDayNames[i]);
        ShortDayNames[i]:=_DateToStr(fmt, 'ccc', 2006, 1, i, 0, ShortDayNames[i]);
      end;
      TimeAMString:=_DateToStr(fmt, 'a', 2006, 1, 1, 1, TimeAMString);
      TimePMString:=_DateToStr(fmt, 'a', 2006, 1, 1, 13, TimePMString);
      CFRelease(fmt);
    end;
    CFRelease(loc);
  end;
end;

initialization
  GetMacFormatSettings(DefaultFormatSettings);

end.

