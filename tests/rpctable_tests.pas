unit rpctable_tests;

{$mode objfpc}{$H+}

interface

uses
  fpjson, fpcunit, testregistry, rpctable;

type

  TRpcTableTests = class(TTestCase)
  private
    function ConvertFrom(const AJSON: string): TJSONObject;
  published
    procedure TestNormalConversion;
    procedure TestHeaderOnlyTable;
    procedure TestMissingTorrents;
    procedure TestEmptyTorrents;
    procedure TestNonArrayHeader;
    procedure TestTorrentsNotArray;
    procedure TestNonStringHeaderField;
    procedure TestNonArrayDataRow;
    procedure TestShortDataRow;
    procedure TestLongDataRow;
    procedure TestDuplicateHeaderField;
  end;

implementation

uses
  jsonparser;

function TRpcTableTests.ConvertFrom(const AJSON: string): TJSONObject;
begin
  Result:=GetJSON(AJSON, False) as TJSONObject;
end;

procedure TRpcTableTests.TestNormalConversion;
var
  reply, out_reply: TJSONObject;
  torrents: TJSONArray;
begin
  reply:=ConvertFrom('{"torrents": [["id", "name", "sizeWhenDone"], ' +
                     '[1, "foo", 100], [2, "bar", 200]]}');
  out_reply:=TranslateTableToObjects(reply);
  try
    AssertNotNull(out_reply);
    torrents:=out_reply.Arrays['torrents'];
    AssertEquals(2, torrents.Count);
    AssertEquals('foo', torrents.Objects[0].Strings['name']);
    AssertEquals(1, torrents.Objects[0].Integers['id']);
    AssertEquals(100, torrents.Objects[0].Integers['sizeWhenDone']);
    AssertEquals('bar', torrents.Objects[1].Strings['name']);
    AssertEquals(2, torrents.Objects[1].Integers['id']);
    AssertEquals(200, torrents.Objects[1].Integers['sizeWhenDone']);
  finally
    out_reply.Free;
  end;
end;

procedure TRpcTableTests.TestHeaderOnlyTable;
var
  reply, out_reply: TJSONObject;
  torrents: TJSONArray;
begin
  reply:=ConvertFrom('{"torrents": [["id", "name"]]}');
  out_reply:=TranslateTableToObjects(reply);
  try
    AssertNotNull(out_reply);
    torrents:=out_reply.Arrays['torrents'];
    AssertEquals(0, torrents.Count);
  finally
    out_reply.Free;
  end;
end;

procedure TRpcTableTests.TestMissingTorrents;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"result": "success"}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestEmptyTorrents;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": []}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestNonArrayHeader;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": ["id", "name"]}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestTorrentsNotArray;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": {}}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestNonStringHeaderField;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": [[1, "name"], [7, "foo"]]}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestNonArrayDataRow;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": [["id"], "foo"]}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestShortDataRow;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": [["id", "name"], [1]]}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestLongDataRow;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": [["id"], [1, 2]]}');
  AssertNull(TranslateTableToObjects(reply));
end;

procedure TRpcTableTests.TestDuplicateHeaderField;
var
  reply: TJSONObject;
begin
  reply:=ConvertFrom('{"torrents": [["id", "id"], [1, 2]]}');
  AssertNull(TranslateTableToObjects(reply));
end;

initialization
  RegisterTest(TRpcTableTests);

end.
