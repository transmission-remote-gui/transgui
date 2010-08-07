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
    FExtraColumns: integer;
    FOnDataChanged: TNotifyEvent;
    FUpdateLockCnt: integer;
    function GetItems(ACol, ARow: integer): variant;
    function GetRowCnt: integer;
    function GetRowOptions(ARow: integer): integer;
    function GetRows(ARow: integer): variant;
    function GetRow(ARow: integer): PVariant;
    procedure SetColCnt(const AValue: integer);
    procedure SetExtraColumns(const AValue: integer);
    procedure SetItems(ACol, ARow: integer; const AValue: variant);
    procedure SetRowCnt(const AValue: integer);
    procedure SetRowOptions(ARow: integer; const AValue: integer);
    function IntCols: integer;

  protected
    procedure DoDataChanged; virtual;

  public
    constructor Create(AColCnt, ARowCnt: integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Sort(ACol: integer; Descending: boolean = False);
    function IndexOf(ACol: integer; const Value: variant): integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Items[ACol, ARow: integer]: variant read GetItems write SetItems; default;
    property Rows[ARow: integer]: variant read GetRows;
    property RowOptions[ARow: integer]: integer read GetRowOptions write SetRowOptions;
    property ColCnt: integer read FColCnt write SetColCnt;
    property RowCnt: integer read GetRowCnt write SetRowCnt;
    property Count: integer read GetRowCnt;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property ExtraColumns: integer read FExtraColumns write SetExtraColumns;
  end;

implementation

uses Math;

{ TVarList }

function TVarList.GetItems(ACol, ARow: integer): variant;
begin
  Result:=GetRow(ARow)^[ACol + IntCols];
end;

function TVarList.GetRowCnt: integer;
begin
  Result:=inherited GetCount;
end;

function TVarList.GetRowOptions(ARow: integer): integer;
begin
  Result:=GetRow(ARow)^[0];
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
    SetRowCnt(ARow + 1);
  v:=Get(ARow);
  if v = nil then begin
    v:=GetMem(SizeOf(variant));
    FillChar(v^, SizeOf(variant), 0);
    v^:=VarArrayCreate([0, FColCnt + IntCols - 1], varVariant);
    v^[0]:=0;
    Put(ARow, v);
  end;
  Result:=v;
end;

procedure TVarList.SetColCnt(const AValue: integer);
var
  i: integer;
begin
  if FColCnt = AValue then exit;
  FColCnt:=AValue;
  for i:=0 to Count - 1 do
    VarArrayRedim(GetRow(i)^, FColCnt + IntCols - 1);
end;

procedure TVarList.SetExtraColumns(const AValue: integer);
begin
  if FExtraColumns=AValue then exit;
  if RowCnt <> 0 then
    raise Exception.Create('Unable to set extra columns.');
  FExtraColumns:=AValue;
end;

procedure TVarList.SetItems(ACol, ARow: integer; const AValue: variant);
begin
  GetRow(ARow)^[ACol + IntCols]:=AValue;
  DoDataChanged;
end;

procedure TVarList.SetRowCnt(const AValue: integer);
begin
  BeginUpdate;
  try
    while Count > AValue do
      Delete(Count - 1);
    SetCount(AValue);
  finally
    EndUpdate;
  end;
end;

procedure TVarList.SetRowOptions(ARow: integer; const AValue: integer);
begin
  GetRow(ARow)^[0]:=AValue;
end;

function TVarList.IntCols: integer;
begin
  Result:=FExtraColumns + 1;
end;

procedure TVarList.DoDataChanged;
begin
  if Assigned(FOnDataChanged) and (FUpdateLockCnt = 0) then
    FOnDataChanged(Self);
end;

constructor TVarList.Create(AColCnt, ARowCnt: integer);
begin
  inherited Create;
  FColCnt:=AColCnt;
  RowCnt:=ARowCnt;
end;

destructor TVarList.Destroy;
begin
  FOnDataChanged:=nil;
  inherited Destroy;
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
  DoDataChanged;
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
  DoDataChanged;
end;

function CompareVariants(const v1, v2: variant): integer;
var
  v1e, v2e: boolean;
begin
  v1e:=VarIsNull(v1) or VarIsEmpty(v1);
  v2e:=VarIsNull(v2) or VarIsEmpty(v2);
  if v1e and v2e then
    Result:=0
  else
  if v1e and not v2e then
    Result:=-1
  else
  if not v1e and v2e then
    Result:=1
  else
    case VarType(v1) of
    varInteger,varsmallint,varshortint,varbyte,varword,varlongword,varint64,varqword:
      Result:=Int64(v1) - Int64(v2);
    varDouble,varSingle,varDate:
      Result:=Sign(double(v1) - double(v2));
    else
      Result:=AnsiCompareText(v1, v2);
    end;
end;

var
  _SortColumn: integer;
  _SortDesc: boolean;
  _IntCols: integer;

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
  Result:=CompareVariants(v1^[_SortColumn], v2^[_SortColumn]);
  i:=_IntCols;
  while (Result = 0) and (i <= VarArrayHighBound(v1^, 1)) do begin
    if i <> _SortColumn then
      Result:=CompareVariants(v1^[i], v2^[i]);
    Inc(i);
  end;
  if _SortDesc then
    Result:=-Result;
end;

procedure TVarList.Sort(ACol: integer; Descending: boolean);
begin
  _SortColumn:=ACol + IntCols;
  _SortDesc:=Descending;
  _IntCols:=IntCols;
  inherited Sort(@CompareItems);
  DoDataChanged;
end;

function TVarList.IndexOf(ACol: integer; const Value: variant): integer;
var
  i: integer;
begin
  for i:=0 to RowCnt - 1 do
    if CompareVariants(Items[ACol, i], Value) = 0 then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

procedure TVarList.BeginUpdate;
begin
  Inc(FUpdateLockCnt);
end;

procedure TVarList.EndUpdate;
begin
  Dec(FUpdateLockCnt);
  if FUpdateLockCnt = 0 then
    DoDataChanged;
end;

end.

