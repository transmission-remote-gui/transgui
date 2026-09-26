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
unit rpctable;

{$mode objfpc}{$H+}

interface

uses
  fpjson;

function TranslateTableToObjects(reply: TJSONObject) : TJSONObject;

implementation

function TranslateTableToObjects(reply: TJSONObject) : TJSONObject;
var
  torrents_data : TJSONData;
  rows, fields, array_tor, out_torrents : TJSONArray;
  object_tor : TJSONObject;
  row_added : boolean;
  i, j : integer;
begin
  Result:=nil;
  if not reply.Find('torrents', torrents_data) then begin
    reply.Free;
    exit;
  end;
  if torrents_data.JSONType <> jtArray then begin
    reply.Free;
    exit;
  end;
  rows:=torrents_data as TJSONArray;
  if rows.Count = 0 then begin
    reply.Free;
    exit;
  end;
  if rows.Items[0].JSONType <> jtArray then begin
    reply.Free;
    exit;
  end;
  fields:=rows.Arrays[0];
  for j:=0 to fields.Count - 1 do
    if fields.Items[j].JSONType <> jtString then begin
      reply.Free;
      exit;
    end;
  for i:=0 to fields.Count - 1 do
    for j:=i + 1 to fields.Count - 1 do
      if fields.Items[i].AsString = fields.Items[j].AsString then begin
        reply.Free;
        exit;
      end;
  out_torrents:=TJSONArray.Create;
  try
    try
      for i:=1 to rows.Count - 1 do begin
        if rows.Items[i].JSONType <> jtArray then
          exit;
        array_tor:=rows.Arrays[i];
        if array_tor.Count <> fields.Count then
          exit;
        object_tor:=TJSONObject.Create;
        row_added:=False;
        try
          for j:=0 to fields.Count - 1 do
            object_tor.Add(fields.Items[j].AsString, array_tor.Items[j].Clone);
          out_torrents.Add(object_tor);
          row_added:=True;
        finally
          if not row_added then
            object_tor.Free;
        end;
      end;
    finally
      reply.Free;
    end;
    Result:=TJSONObject.Create(['torrents', out_torrents]);
  finally
    if Result = nil then
      out_torrents.Free;
  end;
end;

end.
