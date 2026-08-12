unit FolderHistory;

{$mode objfpc}{$H+}

interface

const
  MinFolderHistoryItems = 10;
  MaxFolderHistoryItems = 99;
  // Legacy writers also touched one terminator key at the count index.
  MaxFolderHistoryCleanupIndex = MaxFolderHistoryItems;

function NormalizeFolderHistoryLimit(Value: Integer): Integer;
function NormalizeFolderHistoryCount(Value: Integer): Integer;
function NormalizeFolderHistoryHit(Value: Integer): Integer;
function IncrementFolderHistoryHit(Value: Integer): Integer;

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

end.
