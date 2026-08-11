unit FolderHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes;

const
  MinFolderHistoryItems = 10;
  MaxFolderHistoryItems = 99;
  // Legacy writers also touched one terminator key at the count index.
  MaxFolderHistoryCleanupIndex = MaxFolderHistoryItems;

function NormalizeFolderHistoryLimit(Value: Integer): Integer;
function NormalizeFolderHistoryCount(Value: Integer): Integer;
function NormalizeFolderHistoryHit(Value: Integer): Integer;
function IncrementFolderHistoryHit(Value: Integer): Integer;
procedure FreeFolderHistoryItem(AItems: TStrings; AIndex: Integer);
procedure ClearFolderHistoryItems(AItems: TStrings);

implementation

function NormalizeFolderHistoryLimit(Value: Integer): Integer;
begin
  if Value < MinFolderHistoryItems then
    Result:=MinFolderHistoryItems
  else if Value > MaxFolderHistoryItems then
    Result:=MaxFolderHistoryItems
  else
    Result:=Value;
end;

function NormalizeFolderHistoryCount(Value: Integer): Integer;
begin
  if Value < 0 then
    Result:=0
  else if Value > MaxFolderHistoryItems then
    Result:=MaxFolderHistoryItems
  else
    Result:=Value;
end;

function NormalizeFolderHistoryHit(Value: Integer): Integer;
begin
  // Positive values are cumulative usage counts and have no specified cap.
  if Value < 0 then
    Result:=0
  else
    Result:=Value;
end;

function IncrementFolderHistoryHit(Value: Integer): Integer;
begin
  Result:=NormalizeFolderHistoryHit(Value);
  if Result < High(Integer) then
    Inc(Result);
end;

procedure FreeFolderHistoryItem(AItems: TStrings; AIndex: Integer);
var
  Item: TObject;
begin
  if (AItems = nil) or (AIndex < 0) or (AIndex >= AItems.Count) then
    exit;
  Item:=AItems.Objects[AIndex];
  // Clear the slot before Delete so owning and non-owning lists are both safe.
  AItems.Objects[AIndex]:=nil;
  AItems.Delete(AIndex);
  Item.Free;
end;

procedure ClearFolderHistoryItems(AItems: TStrings);
var
  i: Integer;
  Items: array of TObject;
begin
  if AItems = nil then
    exit;
  SetLength(Items, AItems.Count);
  for i:=0 to AItems.Count - 1 do begin
    Items[i]:=AItems.Objects[i];
    AItems.Objects[i]:=nil;
  end;
  AItems.Clear;
  for i:=0 to High(Items) do
    Items[i].Free;
end;

end.
