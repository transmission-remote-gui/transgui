#!/usr/bin/env python3
from pathlib import Path


def replace_once(path: str, old: str, new: str) -> None:
    file_path = Path(path)
    text = file_path.read_text(encoding="utf-8")
    count = text.count(old)
    if count != 1:
        raise RuntimeError(
            f"{path}: expected one replacement target, found {count}"
        )
    file_path.write_text(text.replace(old, new, 1), encoding="utf-8")


replace_once(
    "folderhistory.pas",
    "interface\n\nconst\n",
    "interface\n\nuses\n  Classes;\n\nconst\n",
)
replace_once(
    "folderhistory.pas",
    "function IncrementFolderHistoryHit(Value: Integer): Integer;\n\nimplementation\n",
    """function IncrementFolderHistoryHit(Value: Integer): Integer;
procedure FreeFolderHistoryItem(AItems: TStrings; AIndex: Integer);
procedure ClearFolderHistoryItems(AItems: TStrings);

implementation
""",
)
replace_once(
    "folderhistory.pas",
    """function IncrementFolderHistoryHit(Value: Integer): Integer;
begin
  Result:=NormalizeFolderHistoryHit(Value);
  if Result < High(Integer) then
    Inc(Result);
end;

end.
""",
    """function IncrementFolderHistoryHit(Value: Integer): Integer;
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
  AItems.BeginUpdate;
  try
    // Clear the slot before Delete so owning and non-owning lists are both safe.
    AItems.Objects[AIndex]:=nil;
    try
      AItems.Delete(AIndex);
    finally
      Item.Free;
    end;
  finally
    AItems.EndUpdate;
  end;
end;

procedure ClearFolderHistoryItems(AItems: TStrings);
var
  i, DetachedCount: Integer;
  Item: TObject;
  Items: array of TObject;
begin
  if AItems = nil then
    exit;
  SetLength(Items, AItems.Count);
  DetachedCount:=0;
  AItems.BeginUpdate;
  try
    try
      for i:=0 to AItems.Count - 1 do begin
        Item:=AItems.Objects[i];
        // Detach before Clear so owning and non-owning lists are both safe.
        AItems.Objects[i]:=nil;
        Items[DetachedCount]:=Item;
        Inc(DetachedCount);
      end;
      AItems.Clear;
    finally
      for i:=0 to DetachedCount - 1 do
        Items[i].Free;
    end;
  finally
    AItems.EndUpdate;
  end;
end;

end.
""",
)
