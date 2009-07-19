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

unit varlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants;

type

  { TVarList }

  TVarList = class(TList)
  private
    FColCnt: integer;
    function GetItems(ACol, ARow: integer): variant;
    function GetRowCnt: integer;
    function GetRows(ARow: integer): variant;
    function GetRow(ARow: integer): PVariant;
    procedure SetItems(ACol, ARow: integer; const AValue: variant);
    procedure SetRowCnt(const AValue: integer);

  public
    constructor Create(AColCnt, ARowCnt: integer);
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Sort(ACol: integer; Descending: boolean = False);
    function IndexOf(ACol: integer; const Value: variant): integer;
    property Items[ACol, ARow: integer]: variant read GetItems write SetItems; default;
    property Rows[ARow: integer]: variant read GetRows;
    property ColCnt: integer read FColCnt;
    property RowCnt: integer read GetRowCnt write SetRowCnt;
    property Count: integer read GetRowCnt;
  end;

implementation

uses Math;

{ TVarList }

function TVarList.GetItems(ACol, ARow: integer): variant;
begin
  Result:=GetRow(ARow)^[ACol];
end;

function TVarList.GetRowCnt: integer;
begin
  Result:=inherited GetCount;
end;

function TVarList.GetRows(ARow: integer): variant;
begin
  Result:=GetRow(ARow)^;
end;

function TVarList.GetRow(ARow: integer): PVariant;
var
  v: PVariant;
begin
  if ARow >= Count then
    SetCount(ARow + 1);
  v:=Get(ARow);
  if v = nil then begin
    v:=GetMem(SizeOf(variant));
    FillChar(v^, SizeOf(variant), 0);
    v^:=VarArrayCreate([0, FColCnt - 1], varVariant);
    Put(ARow, v);
  end;
  Result:=v;
end;

procedure TVarList.SetItems(ACol, ARow: integer; const AValue: variant);
begin
  GetRow(ARow)^[ACol]:=AValue;
end;

procedure TVarList.SetRowCnt(const AValue: integer);
begin
  while Count > AValue do
    Delete(Count - 1);
  SetCount(AValue);
end;

constructor TVarList.Create(AColCnt, ARowCnt: integer);
begin
  inherited Create;
  FColCnt:=AColCnt;
  RowCnt:=ARowCnt;
end;

procedure TVarList.Clear;
var
  i: integer;
  v: PVariant;
begin
  for i:=0 to Count - 1 do begin
    v:=inherited Get(i);
    if v <> nil then begin
      VarClear(v^);
      FreeMem(v);
    end;
  end;
  inherited Clear;
end;

procedure TVarList.Delete(Index: Integer);
var
  v: PVariant;
begin
  v:=inherited Get(Index);
  if v <> nil then begin
    VarClear(v^);
    FreeMem(v);
  end;
  inherited Delete(Index);
end;

var
  FSortColumn: integer;
  FSortDesc: boolean;

function CompareVariants(const v1, v2: variant): integer;
begin
  if VarIsNull(v1) or VarIsEmpty(v1) or VarIsNull(v2) or VarIsEmpty(v2) then
    Result:=0
  else
    case VarType(v1) of
    varInteger:
      Result:=integer(v1) - integer(v2);
    varDouble,varSingle,varDate:
      Result:=Sign(double(v1) - double(v2));
    else
      Result:=AnsiCompareText(v1, v2);
    end;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  v1, v2: PVariant;
  i: integer;
begin
  if Item1 = Item2 then begin
    Result:=0;
    exit;
  end;
  v1:=Item1;
  v2:=Item2;
  Result:=CompareVariants(v1^[FSortColumn], v2^[FSortColumn]);
  i:=0;
  while (Result = 0) and (i <= VarArrayHighBound(v1^, 1)) do begin
    if i <> FSortColumn then
      Result:=CompareVariants(v1^[i], v2^[i]);
    Inc(i);
  end;
  if FSortDesc then
    Result:=-Result;
end;

procedure TVarList.Sort(ACol: integer; Descending: boolean);
begin
  FSortColumn:=ACol;
  FSortDesc:=Descending;
  inherited Sort(@CompareItems);
end;

function TVarList.IndexOf(ACol: integer; const Value: variant): integer;
var
  i: integer;
begin
  for i:=0 to RowCnt - 1 do
    if Items[ACol, i] = Value then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

end.

