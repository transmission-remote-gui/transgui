{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2013 by Yury Sidorov.

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
  TVarList = class;

  TCompareVarRowsEvent = function(Sender: TVarList; Row1, Row2: PVariant; DescendingSort: boolean): integer of object;

  { TVarList }

  TVarList = class(TList)
  private
    FColCnt: integer;
    FExtraColumns: integer;
    FOnCompareVarRows: TCompareVarRowsEvent;
    FOnDataChanged: TNotifyEvent;
    FUpdateLockCnt: integer;
    function GetItemPtr(ACol, ARow: integer): PVariant;
    function GetItems(ACol, ARow: integer): variant;
    function GetRowCnt: integer;
    function GetRowOptions(ARow: integer): integer;
    function GetRows(ARow: integer): PVariant;
    function GetRow(ARow: integer): PVariant;
    procedure SetColCnt(const AValue: integer);
    procedure SetExtraColumns(const AValue: integer);
    procedure SetItems(ACol, ARow: integer; const AValue: variant);
    procedure SetRowCnt(const AValue: integer);
    procedure SetRowOptions(ARow: integer; const AValue: integer);
    function IntCols: integer;
    procedure CheckColIndex(ColIndex: integer);

  protected
    procedure DoDataChanged; virtual;

  public
    constructor Create(AColCnt, ARowCnt: integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Sort(ACol: integer; Descending: boolean = False); reintroduce;
    function IndexOf(ACol: integer; const Value: variant): integer;
    function SortedIndexOf(ACol: integer; const Value: variant): integer;
    function Find(ACol: integer; const Value: variant; var Index: Integer): Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure InsertRow(ARow: integer);
    function IsUpdating: boolean;
    function GetRowItem(ARow: PVariant; ACol: integer): variant;
    property Items[ACol, ARow: integer]: variant read GetItems write SetItems; default;
    property ItemPtrs[ACol, ARow: integer]: PVariant read GetItemPtr;
    property Rows[ARow: integer]: PVariant read GetRows;
    property RowOptions[ARow: integer]: integer read GetRowOptions write SetRowOptions;
    property ColCnt: integer read FColCnt write SetColCnt;
    property RowCnt: integer read GetRowCnt write SetRowCnt;
    property Count: integer read GetRowCnt;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnCompareVarRows: TCompareVarRowsEvent read FOnCompareVarRows write FOnCompareVarRows;
    property ExtraColumns: integer read FExtraColumns write SetExtraColumns;
  end;

function CompareVariants(const v1, v2: variant): integer;

implementation

uses Math;

{ TVarList }

function TVarList.GetItems(ACol, ARow: integer): variant;
begin
  CheckColIndex(ACol);
  Result:=GetRow(ARow)[ACol + IntCols];
end;

function TVarList.GetItemPtr(ACol, ARow: integer): PVariant;
begin
  CheckColIndex(ACol);
  Result:=GetRow(ARow) + (ACol + IntCols);
end;

function TVarList.GetRowCnt: integer;
begin
  Result:=inherited GetCount;
end;

function TVarList.GetRowOptions(ARow: integer): integer;
begin
  Result:=GetRow(ARow)[0];
end;

function TVarList.GetRows(ARow: integer): PVariant;
begin
  Result:=GetRow(ARow);
end;

function TVarList.GetRow(ARow: integer): PVariant;
var
  v: PVariant;
  sz: integer;
begin
  if ARow >= Count then
    SetRowCnt(ARow + 1);
  v:=Get(ARow);
  if v = nil then begin
    sz:=SizeOf(variant)*(FColCnt + IntCols);
    v:=GetMem(sz);
    FillChar(v^, sz, 0);
    v[0]:=0;
    Put(ARow, v);
  end;
  Result:=v;
end;

procedure TVarList.SetColCnt(const AValue: integer);
var
  i, j, ocnt, ncnt: integer;
  p: PVariant;
begin
  if FColCnt = AValue then exit;
  ocnt:=FColCnt + IntCols;
  FColCnt:=AValue;
  ncnt:=FColCnt + IntCols;
  for i:=0 to Count - 1 do begin
    p:=GetRow(i);
    for j:=ncnt to ocnt - 1 do
      VarClear(p[j]);
    ReAllocMem(p, ncnt*SizeOf(variant));
    if ncnt > ocnt then
      FillChar(p[ocnt], (ncnt - ocnt)*SizeOf(variant), 0);
  end;
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
  GetRow(ARow)[ACol + IntCols]:=AValue;
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
  GetRow(ARow)[0]:=AValue;
end;

function TVarList.IntCols: integer;
begin
  Result:=FExtraColumns + 1;
end;

procedure TVarList.CheckColIndex(ColIndex: integer);
begin
  if (ColIndex + IntCols < 0) or (ColIndex >= ColCnt) then
    raise Exception.CreateFmt('Invalid column index (%d).', [ColIndex]);
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
  i: integer;
begin
  v:=inherited Get(Index);
  if v <> nil then begin
    for i:=0 to ColCnt + IntCols - 1 do
      VarClear(v[i]);
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
  _List: TVarList;

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
  if Assigned(_List.OnCompareVarRows) then
    Result:=_List.OnCompareVarRows(_List, v1, v2, _SortDesc)
  else
    Result:=0;
  if Result = 0 then begin
    Result:=CompareVariants(v1[_SortColumn], v2[_SortColumn]);
    i:=_IntCols;
    while (Result = 0) and (i < _List.ColCnt) do begin
      if i <> _SortColumn then
        Result:=CompareVariants(v1[i], v2[i]);
      Inc(i);
    end;
    if _SortDesc then
      Result:=-Result;
  end;
end;

procedure TVarList.Sort(ACol: integer; Descending: boolean);
begin
  _SortColumn:=ACol + IntCols;
  _SortDesc:=Descending;
  _IntCols:=IntCols;
  _List:=Self;
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

function TVarList.SortedIndexOf(ACol: integer; const Value: variant): integer;
begin
  Result:=-1;
  if not Find(ACol, Value, Result) then
    Result:=-1;
end;

function TVarList.Find(ACol: integer; const Value: variant; var Index: Integer): Boolean;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result := false;
  L := 0;
  R := Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := CompareVariants(Value, Items[ACol, I]);
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then begin
         Result := true;
         L := I; // forces end of while loop
      end;
    end;
  end;
  Index := L;
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

procedure TVarList.InsertRow(ARow: integer);
begin
  inherited Insert(ARow, nil);
end;

function TVarList.IsUpdating: boolean;
begin
  Result:=FUpdateLockCnt > 0;
end;

function TVarList.GetRowItem(ARow: PVariant; ACol: integer): variant;
begin
  CheckColIndex(ACol);
  Result:=ARow[ACol + IntCols];
end;

end.

