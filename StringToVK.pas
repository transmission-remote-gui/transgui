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
unit StringToVK;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function VKStringToWord(const aVKstring: string): word;

implementation

function VKStringToWord(const aVKstring: string): word;
begin
  if aVKstring='' then
    Exit(0);
  case UpperCase(aVKstring) of
    'VK_0'                   :Result:= $30;
    'VK_1'                   :Result:= $31;
    'VK_2'                   :Result:= $32;
    'VK_3'                   :Result:= $33;
    'VK_4'                   :Result:= $34;
    'VK_5'                   :Result:= $35;
    'VK_6'                   :Result:= $36;
    'VK_7'                   :Result:= $37;
    'VK_8'                   :Result:= $38;
    'VK_9'                   :Result:= $39;
    'VK_A'                   :Result:= $41;
    'VK_ACCEPT'              :Result:= 30;
    'VK_ADD'                 :Result:= 107;
    'VK_APPS'                :Result:= $5D;
    'VK_ATTN'                :Result:= $F6;
    'VK_B'                   :Result:= $42;
    'VK_BACK'                :Result:= 8;
    'VK_BROWSER_BACK'        :Result:= $A6;
    'VK_BROWSER_FAVORITES'   :Result:= $AB;
    'VK_BROWSER_FORWARD'     :Result:= $A7;
    'VK_BROWSER_HOME'        :Result:= $AC;
    'VK_BROWSER_REFRESH'     :Result:= $A8;
    'VK_BROWSER_SEARCH'      :Result:= $AA;
    'VK_BROWSER_STOP'        :Result:= $A9;
    'VK_C'                   :Result:= $43;
    'VK_CANCEL'              :Result:= 3;
    'VK_CAPITAL'             :Result:= 20;
    'VK_CLEAR'               :Result:= 12;
    'VK_CONTROL'             :Result:= 17;
    'VK_CONVERT'             :Result:= 28;
    'VK_CRSEL'               :Result:= $F7;
    'VK_D'                   :Result:= $44;
    'VK_DECIMAL'             :Result:= 110;
    'VK_DELETE'              :Result:= 46;
    'VK_DIVIDE'              :Result:= 111;
    'VK_DOWN'                :Result:= 40;
    'VK_E'                   :Result:= $45;
    'VK_END'                 :Result:= 35;
    'VK_EREOF'               :Result:= $F9;
    'VK_ESCAPE'              :Result:= 27;
    'VK_EXECUTE'             :Result:= 43;
    'VK_EXSEL'               :Result:= $F8;
    'VK_F'                   :Result:= $46;
    'VK_F1'                  :Result:= 112;
    'VK_F10'                 :Result:= 121;
    'VK_F11'                 :Result:= 122;
    'VK_F12'                 :Result:= 123;
    'VK_F13'                 :Result:= 124;
    'VK_F14'                 :Result:= 125;
    'VK_F15'                 :Result:= 126;
    'VK_F16'                 :Result:= 127;
    'VK_F17'                 :Result:= 128;
    'VK_F18'                 :Result:= 129;
    'VK_F19'                 :Result:= 130;
    'VK_F2'                  :Result:= 113;
    'VK_F20'                 :Result:= 131;
    'VK_F21'                 :Result:= 132;
    'VK_F22'                 :Result:= 133;
    'VK_F23'                 :Result:= 134;
    'VK_F24'                 :Result:= 135;
    'VK_F3'                  :Result:= 114;
    'VK_F4'                  :Result:= 115;
    'VK_F5'                  :Result:= 116;
    'VK_F6'                  :Result:= 117;
    'VK_F7'                  :Result:= 118;
    'VK_F8'                  :Result:= 119;
    'VK_F9'                  :Result:= 120;
    'VK_FINAL'               :Result:= 24;
    'VK_G'                   :Result:= $47;
    'VK_H'                   :Result:= $48;
    'VK_HANGUL'              :Result:= 21;
    'VK_HANJA'               :Result:= 25;
    'VK_HELP'                :Result:= 47;
    'VK_HIGHESTVALUE'        :Result:= $FFFF;
    'VK_HOME'                :Result:= 36;
    'VK_I'                   :Result:= $49;
    'VK_INSERT'              :Result:= 45;
    'VK_J'                   :Result:= $4A;
    'VK_JUNJA'               :Result:= 23;
    'VK_K'                   :Result:= $4B;
    'VK_KANA'                :Result:= 21;
    'VK_KANJI'               :Result:= 25;
    'VK_L'                   :Result:= $4C;
    'VK_LAUNCH_APP1'         :Result:= $B6;
    'VK_LAUNCH_APP2'         :Result:= $B7;
    'VK_LAUNCH_MAIL'         :Result:= $B4;
    'VK_LAUNCH_MEDIA_SELECT' :Result:= $B5;
    'VK_LBUTTON'             :Result:= 1;
    'VK_LCL_ALT'             :Result:= 18;
    'VK_LCL_AT'              :Result:= $103;
    'VK_LCL_BACKSLASH'       :Result:= $DC;
    'VK_LCL_CALL'            :Result:= $101;
    'VK_LCL_CAPSLOCK'        :Result:= 20;
    'VK_LCL_CLOSE_BRAKET'    :Result:= $DD;
    'VK_LCL_COMMA'           :Result:= $BC;
    'VK_LCL_ENDCALL'         :Result:= $102;
    'VK_LCL_EQUAL'           :Result:= $BB;
    'VK_LCL_LALT'            :Result:= $A4;
    'VK_LCL_MINUS'           :Result:= $BD;
    'VK_LCL_OPEN_BRAKET'     :Result:= $DB;
    'VK_LCL_POINT'           :Result:= $BE;
    'VK_LCL_POWER'           :Result:= $100;
    'VK_LCL_QUOTE'           :Result:= $DE;
    'VK_LCL_RALT'            :Result:= $A5;
    'VK_LCL_SEMI_COMMA'      :Result:= $BA;
    'VK_LCL_SLASH'           :Result:= $BF;
    'VK_LCL_TILDE'           :Result:= $C0;
    'VK_LCONTROL'            :Result:= $A2;
    'VK_LEFT'                :Result:= 37;
    'VK_LMENU'               :Result:= $A4;
    'VK_LSHIFT'              :Result:= $A0;
    'VK_LWIN'                :Result:= $5B;
    'VK_M'                   :Result:= $4D;
    'VK_MBUTTON'             :Result:= 4;
    'VK_MEDIA_NEXT_TRACK'    :Result:= $B0;
    'VK_MEDIA_PLAY_PAUSE'    :Result:= $B3;
    'VK_MEDIA_PREV_TRACK'    :Result:= $B1;
    'VK_MEDIA_STOP'          :Result:= $B2;
    'VK_MENU'                :Result:= 18;
    'VK_MODECHANGE'          :Result:= 31;
    'VK_MULTIPLY'            :Result:= 106;
    'VK_N'                   :Result:= $4E;
    'VK_NEXT'                :Result:= 34;
    'VK_NONAME'              :Result:= $FC;
    'VK_NONCONVERT'          :Result:= 29;
    'VK_NUMLOCK'             :Result:= $90;
    'VK_NUMPAD0'             :Result:= 96;
    'VK_NUMPAD1'             :Result:= 97;
    'VK_NUMPAD2'             :Result:= 98;
    'VK_NUMPAD3'             :Result:= 99;
    'VK_NUMPAD4'             :Result:= 100;
    'VK_NUMPAD5'             :Result:= 101;
    'VK_NUMPAD6'             :Result:= 102;
    'VK_NUMPAD7'             :Result:= 103;
    'VK_NUMPAD8'             :Result:= 104;
    'VK_NUMPAD9'             :Result:= 105;
    'VK_O'                   :Result:= $4F;
    'VK_OEM_1'               :Result:= $BA;
    'VK_OEM_102'             :Result:= $E2;
    'VK_OEM_2'               :Result:= $BF;
    'VK_OEM_3'               :Result:= $C0;
    'VK_OEM_4'               :Result:= $DB;
    'VK_OEM_5'               :Result:= $DC;
    'VK_OEM_6'               :Result:= $DD;
    'VK_OEM_7'               :Result:= $DE;
    'VK_OEM_8'               :Result:= $DF;
    'VK_OEM_CLEAR'           :Result:= $FE;
    'VK_OEM_COMMA'           :Result:= $BC;
    'VK_OEM_MINUS'           :Result:= $BD;
    'VK_OEM_PERIOD'          :Result:= $BE;
    'VK_OEM_PLUS'            :Result:= $BB;
    'VK_P'                   :Result:= $50;
    'VK_PA1'                 :Result:= $FD;
    'VK_PAUSE'               :Result:= 19;
    'VK_PLAY'                :Result:= $FA;
    'VK_PRINT'               :Result:= 42;
    'VK_PRIOR'               :Result:= 33;
    'VK_PROCESSKEY'          :Result:= $E7;
    'VK_Q'                   :Result:= $51;
    'VK_R'                   :Result:= $52;
    'VK_RBUTTON'             :Result:= 2;
    'VK_RCONTROL'            :Result:= $A3;
    'VK_RETURN'              :Result:= 13;
    'VK_RIGHT'               :Result:= 39;
    'VK_RMENU'               :Result:= $A5;
    'VK_RSHIFT'              :Result:= $A1;
    'VK_RWIN'                :Result:= $5C;
    'VK_S'                   :Result:= $53;
    'VK_SCROLL'              :Result:= $91;
    'VK_SELECT'              :Result:= 41;
    'VK_SEPARATOR'           :Result:= 108;
    'VK_SHIFT'               :Result:= 16;
    'VK_SLEEP'               :Result:= $5F;
    'VK_SNAPSHOT'            :Result:= 44;
    'VK_SPACE'               :Result:= 32;
    'VK_SUBTRACT'            :Result:= 109;
    'VK_T'                   :Result:= $54;
    'VK_TAB'                 :Result:= 9;
    'VK_U'                   :Result:= $55;
    'VK_UNDEFINED'           :Result:= $FF;
    'VK_UNKNOWN'             :Result:= 0;
    'VK_UP'                  :Result:= 38;
    'VK_V'                   :Result:= $56;
    'VK_VOLUME_DOWN'         :Result:= $AE;
    'VK_VOLUME_MUTE'         :Result:= $AD;
    'VK_VOLUME_UP'           :Result:= $AF;
    'VK_W'                   :Result:= $57;
    'VK_X'                   :Result:= $58;
    'VK_XBUTTON1'            :Result:= 5;
    'VK_XBUTTON2'            :Result:= 6;
    'VK_Y'                   :Result:= $59;
    'VK_Z'                   :Result:= $5A;
    'VK_ZOOM'                :Result:= $FB;
    else Result:=0;
  end;
  if Result=0 then
    begin
      if Pos('MOD_ALT',aVKstring)     > 0 then Result := Result + 1;
      if Pos('MOD_CONTROL',aVKstring) > 0 then Result := Result + 2;
      if Pos('MOD_SHIFT',aVKstring)   > 0 then Result := Result + 4;
      if Pos('MOD_WIN',aVKstring)     > 0 then Result := Result + 8;
    end;
{  if Result=0 then
    begin
      if Pos('ssAlt',aVKstring)     > 0 then Result := Result + $8000;
      if Pos('ssCtrl',aVKstring)    > 0 then Result := Result + $4000;
      if Pos('ssShift',aVKstring)   > 0 then Result := Result + $2000;
      if Pos('ssWin',aVKstring)     > 0 then Result := Result + $1000;
    end; }
end;

end.

