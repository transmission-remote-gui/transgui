//Unit with timezone support for some Freepascal platforms.
//Tomas Hajny

unit tzutil;


interface

type
 DSTSpecType = (DSTMonthWeekDay, DSTMonthDay, DSTJulian, DSTJulianX);

(* Initialized to default values *)
const
  TZName: string = '';
  TZDSTName: string = '';
  TZOffset: longint = 0;
  DSTOffset: longint = 0;
  DSTStartMonth: byte = 4;
  DSTStartWeek: shortint = 1;
  DSTStartDay: word = 0;
  DSTStartSec: cardinal = 7200;
  DSTEndMonth: byte = 10;
  DSTEndWeek: shortint = -1;
  DSTEndDay: word = 0;
  DSTEndSec: cardinal = 10800;
  DSTStartSpecType: DSTSpecType = DSTMonthWeekDay;
  DSTEndSpecType: DSTSpecType = DSTMonthWeekDay;

function TZSeconds: longint;
(* Return current offset from UTC in seconds while respecting DST *)

implementation

uses
  Dos;

function TZSeconds: longint;
const
  MonthDays: array [1..12] of byte =
                              (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  MonthEnds: array [1..12] of word =
                     (31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365);
var
  Y, Mo, D, WD, H, Mi, S, S100: word;
  MS, DS, ME, DE: byte;
  L: longint;
  Second: cardinal;
  AfterDSTStart, BeforeDSTEnd: boolean;

function LeapDay: byte;
begin
 if (Y mod 400 = 0) or (Y mod 100 <> 0) and (Y mod 4 = 0) then
  LeapDay := 1
 else
  LeapDay := 0;
end;

function FirstDay (MM: byte): byte;
(* What day of week (0-6) is the first day of month MM? *)
var
 DD: longint;
begin
 if MM < Mo then
  begin
   DD := D + MonthEnds [Pred (Mo)];
   if MM > 1 then
    Dec (DD, MonthEnds [Pred (MM)]);
   if (MM <= 2) and (Mo > 2) then
    Inc (DD, LeapDay);
  end
 else
  if MM > Mo then
   begin
    DD := - MonthDays [Mo] + D - MonthEnds [Pred (MM)] + MonthEnds [Mo];
    if (Mo <= 2) and (MM > 2) then
     Dec (DD, LeapDay);
   end
  else
(* M = MM *)
   DD := D;
 DD := WD - DD mod 7 + 1;
 if DD < 0 then
  FirstDay := DD + 7
 else
  FirstDay := DD mod 7;
end;

begin
 TZSeconds := TZOffset;
 if DSTOffset <> TZOffset then
  begin
   GetDate (Y, Mo, D, WD);
   GetTime (H, Mi, S, S100);
   Second := cardinal (H) * 3600 + Mi * 60 + S;

   if (DSTStartSpecType = DSTMonthWeekDay) or (DSTStartSpecType = DSTMonthDay)
                                                                           then
    begin
     MS := DSTStartMonth;
     if DSTStartSpecType = DSTMonthDay then
      DS := DSTStartDay
     else
      begin
       DS := FirstDay (DSTStartMonth);
       if (DSTStartWeek >= 1) and (DSTStartWeek <= 4) then
        if DSTStartDay < DS then
         DS := DSTStartWeek * 7 + DSTStartDay - DS + 1
        else
         DS := Pred (DSTStartWeek) * 7 + DSTStartDay - DS + 1
       else
(* Last week in month *)
        begin
         DS := DS + MonthDays [MS] - 1;
         if MS = 2 then
          Inc (DS, LeapDay);
         DS := DS mod 7;
         if DS < DSTStartDay then
          DS := DS + 7 - DSTStartDay
         else
          DS := DS - DSTStartDay;
         DS := MonthDays [MS] - DS;
        end;
      end;
    end
   else
    begin
(* Julian day *)
     L := DSTStartDay;
     if (DSTStartSpecType = DSTJulian) then
(* 0-based *)
      if (L + LeapDay <= 59) then
       Inc (L)
      else
       L := L + 1 - LeapDay;
     if L <= 31 then
      begin
       MS := 1;
       DS := L;
      end
     else
      if (L <= 59) or
                    (DSTStartSpecType = DSTJulian) and (L - LeapDay <= 59) then
       begin
        MS := 2;
        DS := DSTStartDay - 31;
       end
      else
       begin
        MS := 3;
        while (MS < 12) and (MonthEnds [MS] > L) do
         Inc (MS);
        DS := L - MonthEnds [Pred (MS)];
       end;
    end;

   if (DSTEndSpecType = DSTMonthWeekDay) or (DSTEndSpecType = DSTMonthDay) then
    begin
     ME := DSTEndMonth;
     if DSTEndSpecType = DSTMonthDay then
      DE := DSTEndDay
     else
      begin
       DE := FirstDay (DSTEndMonth);
       if (DSTEndWeek >= 1) and (DSTEndWeek <= 4) then
        if DSTEndDay < DE then
         DE := DSTEndWeek * 7 + DSTEndDay - DE + 1
        else
         DE := Pred (DSTEndWeek) * 7 + DSTEndDay - DE + 1
       else
(* Last week in month *)
        begin
         DE := DE + MonthDays [ME] - 1;
         if ME = 2 then
          Inc (DE, LeapDay);
         DE := DE mod 7;
         if DE < DSTEndDay then
          DE := DE + 7 - DSTEndDay
         else
          DE := DE - DSTEndDay;
         DE := MonthDays [ME] - DE;
        end;
      end;
    end
   else
    begin
(* Julian day *)
     L := DSTEndDay;
     if (DSTEndSpecType = DSTJulian) then
(* 0-based *)
      if (L + LeapDay <= 59) then
       Inc (L)
      else
       L := L + 1 - LeapDay;
     if L <= 31 then
      begin
       ME := 1;
       DE := L;
      end
     else
      if (L <= 59) or
                      (DSTEndSpecType = DSTJulian) and (L - LeapDay <= 59) then
       begin
        ME := 2;
        DE := DSTEndDay - 31;
       end
      else
       begin
        ME := 3;
        while (ME < 12) and (MonthEnds [ME] > L) do
         Inc (ME);
        DE := L - MonthEnds [Pred (ME)];
       end;
    end;

   if Mo < MS then
    AfterDSTStart := false
   else
    if Mo > MS then
     AfterDSTStart := true
    else
     if D < DS then
      AfterDSTStart := false
     else
      if D > DS then
       AfterDSTStart := true
      else
       AfterDSTStart := Second > DSTStartSec;
   if Mo > ME then
    BeforeDSTEnd := false
   else
    if Mo < ME then
     BeforeDSTEnd := true
    else
     if D > DE then
      BeforeDSTEnd := false
     else
      if D < DE then
       BeforeDSTEnd := true
      else
       BeforeDSTEnd := Second < DSTEndSec;
   if AfterDSTStart and BeforeDSTEnd then
    TZSeconds := DSTOffset;
  end;
end;

procedure InitTZ;
const
  TZEnvName = 'TZ';
  EMXTZEnvName = 'EMXTZ';
var
  TZ, S: string;
  I, J: byte;
  Err: longint;
  GnuFmt: boolean;
  ADSTStartMonth: byte;
  ADSTStartWeek: shortint;
  ADSTStartDay: word;
  ADSTStartSec: cardinal;
  ADSTEndMonth: byte;
  ADSTEndWeek: shortint;
  ADSTEndDay: word;
  ADSTEndSec: cardinal;
  ADSTStartSpecType: DSTSpecType;
  ADSTEndSpecType: DSTSpecType;
  ADSTChangeSec: cardinal;

  function ParseOffset (OffStr: string): longint;
  (* Parse time offset given as [-|+]HH[:MI[:SS]] and return in seconds *)
  var
    TZShiftHH, TZShiftDir: shortint;
    TZShiftMI, TZShiftSS: byte;
    N1, N2: byte;
  begin
    TZShiftHH := 0;
    TZShiftMI := 0;
    TZShiftSS := 0;
    TZShiftDir := 1;
    N1 := 1;
    while (N1 <= Length (OffStr)) and (OffStr [N1] <> ':') do
     Inc (N1);
    Val (Copy (OffStr, 1, Pred (N1)), TZShiftHH, Err);
    if (Err = 0) and (TZShiftHH >= -24) and (TZShiftHH <= 23) then
     begin
(* Normalize the hour offset to -12..11 if necessary *)
      if TZShiftHH > 11 then
       Dec (TZShiftHH, 24) else
      if TZShiftHH < -12 then
       Inc (TZShiftHH, 24);
      if TZShiftHH < 0 then
       TZShiftDir := -1;
      if (N1 <= Length (OffStr)) then
       begin
        N2 := Succ (N1);
        while (N2 <= Length (OffStr)) and (OffStr [N2] <> ':') do
         Inc (N2);
        Val (Copy (OffStr, Succ (N1), N2 - N1), TZShiftMI, Err);
         if (Err = 0) and (TZShiftMI <= 59) then
          begin
           if (N2 <= Length (OffStr)) then
            begin
             Val (Copy (OffStr, Succ (N2), Length (OffStr) - N2), TZShiftSS, Err);
             if (Err <> 0) or (TZShiftSS > 59) then
              TZShiftSS := 0;
            end
          end
         else
          TZShiftMI := 0;
       end;
     end
    else
     TZShiftHH := 0;
    ParseOffset := longint (TZShiftHH) * 3600 +
                           TZShiftDir * (longint (TZShiftMI) * 60 + TZShiftSS);
  end;

begin
  TZ := GetEnv (TZEnvName);
  if TZ = '' then
   TZ := GetEnv (EMXTZEnvName);
  if TZ <> '' then
   begin
    TZ := Upcase (TZ);
(* Timezone name *)
    I := 1;
    while (I <= Length (TZ)) and (TZ [I] in ['A'..'Z']) do
     Inc (I);
    TZName := Copy (TZ, 1, Pred (I));
    if I <= Length (TZ) then
     begin
(* Timezone shift *)
      J := Succ (I);
      while (J <= Length (TZ)) and not (TZ [J] in ['A'..'Z']) do
       Inc (J);
      TZOffset := ParseOffset (Copy (TZ, I, J - I));
(* DST timezone name *)
      I := J;
      while (J <= Length (TZ)) and (TZ [J] in ['A'..'Z']) do
       Inc (J);
      if J > I then
       begin
        TZDSTName := Copy (TZ, I, J - I);
(* DST timezone name provided; if equal to the standard timezone  *)
(* name then DSTOffset is set to be equal to TZOffset by default, *)
(* otherwise it is set to TZOffset - 3600 seconds.                *)
        if TZDSTName <> TZName then
         DSTOffset := -3600 + TZOffset
        else
         DSTOffset := TZOffset;
       end
      else
       begin
        TZDSTName := TZName;
(* No DST timezone name provided => DSTOffset is equal to TZOffset *)
        DSTOffset := TZOffset;
       end;
      if J <= Length (TZ) then
       begin
(* Check if DST offset is specified here;   *)
(* if not, default value set above is used. *)
        if TZ [J] <> ',' then
         begin
          I := J;
          Inc (J);
          while (J <= Length (TZ)) and (TZ [J] <> ',') do
           Inc (J);
          DSTOffset := ParseOffset (Copy (TZ, I, J - I));
         end;
        if J < Length (TZ) then
         begin
          Inc (J);
(* DST switching details *)
          case TZ [J] of
           'M':
            begin
(* Mmonth.week.dayofweek[/StartHour] *)
             ADSTStartSpecType := DSTMonthWeekDay;
             if J >= Length (TZ) then
              Exit;
             Inc (J);
             I := J;
             while (J <= Length (TZ)) and not (TZ [J] in ['.', ',', '/']) do
              Inc (J);
             if (J >= Length (TZ)) or (TZ [J] <> '.') then
              Exit;
             Val (Copy (TZ, I, J - I), ADSTStartMonth, Err);
             if (Err > 0) or (ADSTStartMonth > 12) then
              Exit;
             Inc (J);
             I := J;
             while (J <= Length (TZ)) and not (TZ [J] in ['.', ',', '/']) do
              Inc (J);
             if (J >= Length (TZ)) or (TZ [J] <> '.') then
              Exit;
             Val (Copy (TZ, I, J - I), ADSTStartWeek, Err);
             if (Err > 0) or (ADSTStartWeek < 1) or (ADSTStartWeek > 5) then
              Exit;
             Inc (J);
             I := J;
             while (J <= Length (TZ)) and not (TZ [J] in [',', '/']) do
              Inc (J);
             Val (Copy (TZ, I, J - I), ADSTStartDay, Err);
             if (Err > 0) or (ADSTStartDay < 0) or (ADSTStartDay > 6)
                                                     or (J >= Length (TZ)) then
              Exit;
             if TZ [J] = '/' then
              begin
               Inc (J);
               I := J;
               while (J <= Length (TZ)) and (TZ [J] <> ',') do
                Inc (J);
               Val (Copy (TZ, I, J - I), ADSTStartSec, Err);
               if (Err > 0) or (ADSTStartSec > 86399) or (J >= Length (TZ))
                                                                           then
                Exit
               else
                ADSTStartSec := ADSTStartSec * 3600;
              end
             else
              (* Use the preset default *)
              ADSTStartSec := DSTStartSec;
             Inc (J);
            end;
           'J':
            begin
(* Jjulianday[/StartHour] *)
             ADSTStartSpecType := DSTJulianX;
             if J >= Length (TZ) then
              Exit;
             Inc (J);
             I := J;
             while (J <= Length (TZ)) and not (TZ [J] in [',', '/']) do
              Inc (J);
             Val (Copy (TZ, I, J - I), ADSTStartDay, Err);
             if (Err > 0) or (ADSTStartDay = 0) or (ADSTStartDay > 365)
                                                     or (J >= Length (TZ)) then
              Exit;
             if TZ [J] = '/' then
              begin
               Inc (J);
               I := J;
               while (J <= Length (TZ)) and (TZ [J] <> ',') do
                Inc (J);
               Val (Copy (TZ, I, J - I), ADSTStartSec, Err);
               if (Err > 0) or (ADSTStartSec > 86399) or (J >= Length (TZ))
                                                                           then
                Exit
               else
                ADSTStartSec := ADSTStartSec * 3600;
              end
             else
              (* Use the preset default *)
              ADSTStartSec := DSTStartSec;
             Inc (J);
            end
          else
           begin
(* Check the used format first - GNU libc / GCC / EMX expect                 *)
(* "NameOffsetDstname[Dstoffset],Start[/StartHour],End[/EndHour]";           *)
(* if more than one comma (',') is found, the following format is assumed:   *)
(* "NameOffsetDstname[Dstoffset],StartMonth,StartWeek,StartDay,StartSecond,  *)
(*                         EndMonth,EndWeek,EndDay,EndSecond,DSTDifference". *)
            I := J;
            while (J <= Length (TZ)) and (TZ [J] <> ',') do
             Inc (J);
            S := Copy (TZ, I, J - I);
            if J < Length (TZ) then
             begin
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              GnuFmt := J > Length (TZ);
             end
            else
             Exit;
            if GnuFmt then
             begin
              ADSTStartSpecType := DSTJulian;
              J := Pos ('/', S);
              if J = 0 then
               begin
                Val (S, ADSTStartDay, Err);
                if (Err > 0) or (ADSTStartDay > 365) then
                 Exit;
                (* Use the preset default *)
                ADSTStartSec := DSTStartSec;
               end
              else
               begin
                if J = Length (S) then
                 Exit;
                Val (Copy (S, 1, Pred (J)), ADSTStartDay, Err);
                if (Err > 0) or (ADSTStartDay > 365) then
                 Exit;
                Val (Copy (S, Succ (J), Length (S) - J), ADSTStartSec, Err);
                if (Err > 0) or (ADSTStartSec > 86399) then
                 Exit
                else
                 ADSTStartSec := ADSTStartSec * 3600;
               end;
              J := I;
             end
            else
             begin
              Val (S, ADSTStartMonth, Err);
              if (Err > 0) or (ADSTStartMonth > 12) then
               Exit;
              Val (Copy (TZ, I, J - I), ADSTStartWeek, Err);
              if (Err > 0) or (ADSTStartWeek < -1) or (ADSTStartWeek > 5) or
                                                        (J >= Length (TZ)) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTStartDay, Err);
              if (DSTStartWeek = 0) then
               begin
                if (Err > 0) or (ADSTStartDay < 1) or (ADSTStartDay > 31)
                  or (ADSTStartDay > 30) and (ADSTStartMonth in [4, 6, 9, 11])
                           or (ADSTStartMonth = 2) and (ADSTStartDay > 29) then
                 Exit;
                ADSTStartSpecType := DSTMonthDay;
               end
              else
               begin
                if (Err > 0) or (ADSTStartDay < 0) or (ADSTStartDay > 6) then
                 Exit;
                ADSTStartSpecType := DSTMonthWeekDay;
               end;
              if J >= Length (TZ) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTStartSec, Err);
              if (Err > 0) or (ADSTStartSec > 86399) or (J >= Length (TZ)) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTEndMonth, Err);
              if (Err > 0) or (ADSTEndMonth > 12) or (J >= Length (TZ)) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTEndWeek, Err);
              if (Err > 0) or (ADSTEndWeek < -1) or (ADSTEndWeek > 5)
                                                     or (J >= Length (TZ)) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTEndDay, Err);
              if (DSTEndWeek = 0) then
               begin
                if (Err > 0) or (ADSTEndDay < 1) or (ADSTEndDay > 31)
                   or (ADSTEndDay > 30) and (ADSTEndMonth in [4, 6, 9, 11])
                               or (ADSTEndMonth = 2) and (ADSTEndDay > 29) then
                 Exit;
                ADSTEndSpecType := DSTMonthDay;
               end
              else
               begin
                if (Err > 0) or (ADSTEndDay < 0) or (ADSTEndDay > 6) then
                 Exit;
                ADSTEndSpecType := DSTMonthWeekDay;
               end;
              if J >= Length (TZ) then
               Exit;
              Inc (J);
              I := J;
              while (J <= Length (TZ)) and (TZ [J] <> ',') do
               Inc (J);
              Val (Copy (TZ, I, J - I), ADSTEndSec, Err);
              if (Err > 0) or (ADSTEndSec > 86399) or (J >= Length (TZ)) then
               Exit;
              Val (Copy (TZ, Succ (J), Length (TZ) - J), ADSTChangeSec, Err);
              if (Err = 0) and (ADSTChangeSec < 86400) then
               begin
(* Format complete, all checks successful => accept the parsed values. *)
                DSTStartMonth := ADSTStartMonth;
                DSTStartWeek := ADSTStartWeek;
                DSTStartDay := ADSTStartDay;
                DSTStartSec := ADSTStartSec;
                DSTEndMonth := ADSTEndMonth;
                DSTEndWeek := ADSTEndWeek;
                DSTEndDay := ADSTEndDay;
                DSTEndSec := ADSTEndSec;
                DSTStartSpecType := ADSTStartSpecType;
                DSTEndSpecType := ADSTEndSpecType;
                DSTOffset := TZOffset - ADSTChangeSec;
               end;
(* Parsing finished *)
              Exit;
             end;
           end;
          end;
(* GnuFmt - DST end specification *)
          if TZ [J] = 'M' then
           begin
(* Mmonth.week.dayofweek *)
            ADSTEndSpecType := DSTMonthWeekDay;
            if J >= Length (TZ) then
             Exit;
            Inc (J);
            I := J;
            while (J <= Length (TZ)) and not (TZ [J] in ['.', ',', '/']) do
             Inc (J);
            if (J >= Length (TZ)) or (TZ [J] <> '.') then
             Exit;
            Val (Copy (TZ, I, J - I), ADSTEndMonth, Err);
            if (Err > 0) or (ADSTEndMonth > 12) then
             Exit;
            Inc (J);
            I := J;
            while (J <= Length (TZ)) and not (TZ [J] in ['.', ',', '/']) do
             Inc (J);
            if (J >= Length (TZ)) or (TZ [J] <> '.') then
             Exit;
            Val (Copy (TZ, I, J - I), ADSTEndWeek, Err);
            if (Err > 0) or (ADSTEndWeek < 1) or (ADSTEndWeek > 5) then
             Exit;
            Inc (J);
            I := J;
            while (J <= Length (TZ)) and (TZ [J] <> '/') do
             Inc (J);
            Val (Copy (TZ, I, J - I), ADSTEndDay, Err);
            if (Err > 0) or (ADSTEndDay < 0) or (ADSTEndDay > 6) then
             Exit;
           end
          else
           begin
            if TZ [J] = 'J' then
             begin
(* Jjulianday *)
              if J = Length (TZ) then
               Exit;
              Inc (J);
              ADSTEndSpecType := DSTJulianX
             end
            else
             ADSTEndSpecType := DSTJulian;
            if J >= Length (TZ) then
             Exit;
            Inc (J);
            I := J;
            while (J <= Length (TZ)) and (TZ [J] <> '/') do
             Inc (J);
            Val (Copy (TZ, I, J - I), ADSTEndDay, Err);
            if (Err > 0) or (ADSTEndDay = 0) and (ADSTEndSpecType = DSTJulianX)
                                                     or (ADSTEndDay > 365) then
             Exit;
           end;
          if (J <= Length (TZ)) and (TZ [J] = '/') then
           begin
            if J = Length (TZ) then
             Exit;
            Val (Copy (TZ, Succ (J), Length (TZ) - J), ADSTEndSec, Err);
            if (Err > 0) or (ADSTEndSec > 86399) then
             Exit
            else
             ADSTEndSec := ADSTEndSec * 3600;
           end
          else
           (* Use the preset default *)
           ADSTEndSec := DSTEndSec;

(* Format complete, all checks successful => accept the parsed values. *)
         if ADSTStartSpecType = DSTMonthWeekDay then
          begin
           DSTStartMonth := ADSTStartMonth;
           DSTStartWeek := ADSTStartWeek;
          end;
         DSTStartDay := ADSTStartDay;
         DSTStartSec := ADSTStartSec;
         if ADSTStartSpecType = DSTMonthWeekDay then
          begin
           DSTEndMonth := ADSTEndMonth;
           DSTEndWeek := ADSTEndWeek;
          end;
          DSTEndDay := ADSTEndDay;
          DSTEndSec := ADSTEndSec;
          DSTStartSpecType := ADSTStartSpecType;
          DSTEndSpecType := ADSTEndSpecType;
         end;
       end
      else
       DSTOffset := -3600 + TZOffset;
     end;
   end;
end;


begin
  InitTZ;
end.
