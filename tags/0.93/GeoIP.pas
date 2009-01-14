{
 * Copyright (C) 2005 MaxMind LLC  All Rights Reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 * ChangeLog
 * 2003-04-09 Translation of C# class to Pascal provided by W. Tracz
 * 2005-07-20 Added support for GeoIP Region, City, ISP and Organization (Yvan Schwab/esoftys)
}

{ Thanks to W. Tracz/Yvan Schwab for contributing this class }

{$mode delphi}
unit GeoIP;

interface

uses Classes, SysUtils, {$ifdef FPC} sockets {$else} WinSock {$endif};

type
  TGeoIPResult = (
    GEOIP_SUCCESS                 = 0,
    GEOIP_NODATA                  = 1,
    GEOIP_ERROR_IPADDR            = 2,
    GEOIP_ERROR_DBTYPE            = 3,
    GEOIP_ERROR_IO                = 4
  );

  TGeoIPDBTypes = (
    GEOIP_COUNTRY_EDITION     = 1,
    GEOIP_CITY_EDITION_REV1   = 2,
    GEOIP_REGION_EDITION_REV1 = 3,
    GEOIP_ISP_EDITION         = 4,
    GEOIP_ORG_EDITION         = 5,
    GEOIP_CITY_EDITION_REV0   = 6,
    GEOIP_REGION_EDITION_REV0 = 7,
    GEOIP_PROXY_EDITION       = 8,
    GEOIP_ASNUM_EDITION       = 9
  );

  TGeoIPCountry = record
    CountryCode: string;
    CountryName: string;
  end;

  TGeoIPRegion = record
    CountryCode: string;
    Region: string;
  end;

  TGeoIPCity = record
    CountryCode: string;
    CountryName: string;
    Region: string;
    City: string;
    PostalCode: string;
    Latitude: Double;
    Longitude: Double;
    DmaCode: Integer;
    AreaCode: Integer;
  end;

  TGeoIPOrg = record
    Name: string;
  end;

  TGeoIP = class
  private
    FInputFile: TFileStream;
    FDatabaseType: TGeoIPDBTypes;
    FDatabaseSegments: array of Cardinal;
    FDatabaseInfo: string;
    FRecordLength: Cardinal;
    function _GetCity(IPNum: Cardinal; var GeoIPCity: TGeoIPCity): TGeoIPResult;
    function _GetCountry(IPNum: Cardinal; var GeoIPCountry: TGeoIPCountry): TGeoIPResult;
    function _GetOrg(IPNum: Cardinal; var GeoIPOrg: TGeoIPOrg): TGeoIPResult;
    function _GetRegion(IPNum: Cardinal; var GeoIPRegion: TGeoIPRegion): TGeoIPResult;
    function AddrToNum(const IPAddr: string): Cardinal;
    procedure InitDBFile;
    function SeekRecord(IPNum: Cardinal): Cardinal;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function GetCity(const IPAddr: string; var GeoIPCity: TGeoIPCity): TGeoIPResult;
    function GetCountry(const IPAddr: string; var GeoIPCountry: TGeoIPCountry): TGeoIPResult;
    function GetDatabaseInfo: string;
    function GetOrg(const IPAddr: string; var GeoIPOrg: TGeoIPOrg): TGeoIPResult;
    function GetRegion(const IPAddr: string; var GeoIPRegion: TGeoIPRegion): TGeoIPResult;
  end;

const
  CountryCodes:array [0..252] of string = ('--','AP','EU','AD','AE','AF','AG','AI','AL','AM','AN','AO','AQ','AR','AS','AT','AU','AW','AZ','BA','BB','BD','BE','BF','BG','BH','BI','BJ','BM','BN','BO','BR','BS','BT','BV','BW','BY','BZ','CA','CC','CD','CF','CG','CH','CI','CK','CL','CM','CN','CO','CR','CU','CV','CX','CY','CZ','DE','DJ','DK','DM','DO','DZ','EC','EE','EG','EH','ER','ES','ET','FI','FJ','FK','FM','FO','FR','FX','GA','GB','GD','GE','GF','GH','GI','GL','GM','GN','GP','GQ','GR','GS','GT','GU','GW',
                                           'GY','HK','HM','HN','HR','HT','HU','ID','IE','IL','IN','IO','IQ','IR','IS','IT','JM','JO','JP','KE','KG','KH','KI','KM','KN','KP','KR','KW','KY','KZ','LA','LB','LC','LI','LK','LR','LS','LT','LU','LV','LY','MA','MC','MD','MG','MH','MK','ML','MM','MN','MO','MP','MQ','MR','MS','MT','MU','MV','MW','MX','MY','MZ','NA','NC','NE','NF','NG','NI','NL','NO','NP','NR','NU','NZ','OM','PA','PE','PF','PG','PH','PK','PL','PM','PN','PR','PS','PT','PW','PY','QA','RE','RO','RU',
                                           'RW','SA','SB','SC','SD','SE','SG','SH','SI','SJ','SK','SL','SM','SN','SO','SR','ST','SV','SY','SZ','TC','TD','TF','TG','TH','TJ','TK','TM','TN','TO','TL','TR','TT','TV','TW','TZ','UA','UG','UM','US','UY','UZ','VA','VC','VE','VG','VI','VN','VU','WF','WS','YE','YT','RS','ZA','ZM','ME','ZW','A1','A2','O1','AX','GG','IM','JE','BL','MF');

  CountryNames:array [0..252] of string = ('N/A','Asia/Pacific Region','Europe','Andorra','United Arab Emirates','Afghanistan','Antigua and Barbuda','Anguilla','Albania','Armenia','Netherlands Antilles','Angola','Antarctica','Argentina','American Samoa','Austria','Australia','Aruba','Azerbaijan','Bosnia and Herzegovina','Barbados','Bangladesh','Belgium','Burkina Faso','Bulgaria','Bahrain','Burundi','Benin','Bermuda','Brunei Darussalam','Bolivia','Brazil','Bahamas','Bhutan','Bouvet Island','Botswana',
                                           'Belarus','Belize','Canada','Cocos (Keeling) Islands','Congo, The Democratic Republic of the','Central African Republic','Congo','Switzerland','Cote D''Ivoire','Cook Islands','Chile','Cameroon','China','Colombia','Costa Rica','Cuba','Cape Verde','Christmas Island','Cyprus','Czech Republic','Germany','Djibouti','Denmark','Dominica','Dominican Republic','Algeria','Ecuador','Estonia','Egypt','Western Sahara','Eritrea','Spain','Ethiopia','Finland','Fiji',
                                           'Falkland Islands (Malvinas)','Micronesia, Federated States of','Faroe Islands','France','France, Metropolitan','Gabon','United Kingdom','Grenada','Georgia','French Guiana','Ghana','Gibraltar','Greenland','Gambia','Guinea','Guadeloupe','Equatorial Guinea','Greece','South Georgia and the South Sandwich Islands','Guatemala','Guam','Guinea-Bissau','Guyana','Hong Kong','Heard Island and McDonald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','Ireland',
                                           'Israel','India','British Indian Ocean Territory','Iraq','Iran, Islamic Republic of','Iceland','Italy','Jamaica','Jordan','Japan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Comoros','Saint Kitts and Nevis','Korea, Democratic People''s Republic of','Korea, Republic of','Kuwait','Cayman Islands','Kazakstan','Lao People''s Democratic Republic','Lebanon','Saint Lucia','Liechtenstein','Sri Lanka','Liberia','Lesotho','Lithuania','Luxembourg','Latvia',
                                           'Libyan Arab Jamahiriya','Morocco','Monaco','Moldova, Republic of','Madagascar','Marshall Islands','Macedonia, the Former Yugoslav Republic of','Mali','Myanmar','Mongolia','Macao','Northern Mariana Islands','Martinique','Mauritania','Montserrat','Malta','Mauritius','Maldives','Malawi','Mexico','Malaysia','Mozambique','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Netherlands','Norway','Nepal','Nauru','Niue','New Zealand','Oman',
                                           'Panama','Peru','French Polynesia','Papua New Guinea','Philippines','Pakistan','Poland','Saint Pierre and Miquelon','Pitcairn','Puerto Rico','Palestinian Territory, Occupied','Portugal','Palau','Paraguay','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Solomon Islands','Seychelles','Sudan','Sweden','Singapore','Saint Helena','Slovenia','Svalbard and Jan Mayen','Slovakia','Sierra Leone','San Marino','Senegal','Somalia','Suriname',
                                           'Sao Tome and Principe','El Salvador','Syrian Arab Republic','Swaziland','Turks and Caicos Islands','Chad','French Southern Territories','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','Tunisia','Tonga','Timor-Leste','Turkey','Trinidad and Tobago','Tuvalu','Taiwan','Tanzania, United Republic of','Ukraine','Uganda','United States Minor Outlying Islands','United States','Uruguay','Uzbekistan','Holy See (Vatican City State)',
                                           'Saint Vincent and the Grenadines','Venezuela','Virgin Islands, British','Virgin Islands, U.S.','Vietnam','Vanuatu','Wallis and Futuna','Samoa','Yemen','Mayotte','Serbia','South Africa','Zambia','Montenegro','Zimbabwe','Anonymous Proxy','Satellite Provider','Other','Aland Islands','Guernsey','Isle of Man','Jersey','Saint Barthelemy','Saint Martin');


implementation

const
  COUNTRY_BEGIN = 16776960;
  STATE_BEGIN_REV0 = 16700000;
  STATE_BEGIN_REV1  = 16000000;
  STRUCTURE_INFO_MAX_SIZE = 20;
  DATABASE_INFO_MAX_SIZE = 100;
  SEGMENT_RECORD_LENGTH = 3;
  STANDARD_RECORD_LENGTH = 3;
  ORG_RECORD_LENGTH = 4;
  MAX_RECORD_LENGTH = 4;
  MAX_ORG_RECORD_LENGTH = 300;
  FULL_RECORD_LENGTH = 50;
  US_OFFSET = 1;
  CANADA_OFFSET = 677;
  WORLD_OFFSET = 1353;
  FIPS_RANGE = 360;

{ TGeoIP }

constructor TGeoIP.Create(const FileName: string);
begin
  inherited Create;
  FInputFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  InitDBFile;
end;

destructor TGeoIP.Destroy;
begin
  if Assigned(FInputFile) then
    FInputFile.Free;
  inherited Destroy;
end;

function TGeoIP._GetCity(IPNum: Cardinal; var GeoIPCity: TGeoIPCity): TGeoIPResult;
var
   SeekCity: Cardinal;
   RecordPointer: Cardinal;
   StrLen: Cardinal;
   buf: array[0..FULL_RECORD_LENGTH-1] of Byte;
   p: PChar;
   i: Integer;
   DmaAreaCombo: Integer;
begin
  if (FDatabaseType <> GEOIP_CITY_EDITION_REV0) and (FDatabaseType <> GEOIP_CITY_EDITION_REV1) then
  begin
    Result := GEOIP_ERROR_DBTYPE;
    Exit;
  end;
  SeekCity := SeekRecord(IPNum);
  if SeekCity = FDatabaseSegments[0] then
  begin
    Result := GEOIP_NODATA;
    Exit;
  end;
  RecordPointer := SeekCity + (2 * FRecordLength - 1) * FDatabaseSegments[0];
  FInputFile.Seek(RecordPointer, soFromBeginning);
  FInputFile.Read(buf, FULL_RECORD_LENGTH);

  // get country
  GeoIPCity.CountryCode := CountryCodes[buf[0]];
  GeoIPCity.CountryName := CountryNames[buf[0]];

  // get region
  p := @buf[1];
  StrLen := 0;
  while (p[StrLen] <> #0) do
    Inc(StrLen);
  GeoIPCity.Region := Copy(p, 0, StrLen);

  // get city
  Inc(p, StrLen + 1);
  StrLen := 0;
  while (p[StrLen] <> #0) do
    Inc(StrLen);
  GeoIPCity.City := Copy(p, 0, StrLen);

  // get postal code
  Inc(p, StrLen + 1);
  StrLen := 0;
  while (p[StrLen] <> #0) do
    Inc(StrLen);
  GeoIPCity.PostalCode := Copy(p, 0, StrLen);

  // get latitude
  Inc(p, StrLen + 1);
  GeoIPCity.Latitude := 0.0;
  for i:=0 to 2 do
  begin
    GeoIPCity.Latitude := GeoIPCity.Latitude + (Integer(p[i]) shl (i*8));
  end;
  GeoIPCity.Latitude := GeoIPCity.Latitude/10000 - 180;

  // get longitude
  Inc(p, 3);
  GeoIPCity.Longitude := 0.0;
  for i:=0 to 2 do
  begin
    GeoIPCity.Longitude := GeoIPCity.Longitude + (Integer(p[i]) shl (i*8));
  end;
  GeoIPCity.Longitude := GeoIPCity.Longitude/10000 - 180;

  // get area code and dma code for post April 2002 databases and for US locations
  GeoIPCity.DmaCode := 0;
  GeoIPCity.AreaCode := 0;
  if FDatabaseType = GEOIP_CITY_EDITION_REV1 then
  begin
    if GeoIPCity.CountryCode = 'US' then
    begin
      Inc(p, 3);
      DmaAreaCombo := 0;
      for i:=0 to 2 do
      begin
        DmaAreaCombo := DmaAreaCombo + (Integer(p[i]) shl (i*8));
      end;
      GeoIPCity.DmaCode := DmaAreaCombo div 1000;
      GeoIPCity.AreaCode := DmaAreaCombo mod 1000;
    end;
  end;
  Result := GEOIP_SUCCESS;
end;

function TGeoIP._GetCountry(IPNum: Cardinal; var GeoIPCountry: TGeoIPCountry): TGeoIPResult;
var
   ret: Cardinal;
begin
  if (FDatabaseType <> GEOIP_COUNTRY_EDITION) and (FDatabaseType <> GEOIP_PROXY_EDITION) then
  begin
    Result := GEOIP_ERROR_DBTYPE;
    Exit;
  end;
  ret := SeekRecord(IPNum) - COUNTRY_BEGIN;
  if ret > 0 then
  begin
    GeoIPCountry.CountryCode := CountryCodes[ret];
    GeoIPCountry.CountryName := CountryNames[ret];
    Result := GEOIP_SUCCESS;
  end
  else
  begin
    Result := GEOIP_NODATA;
  end;
end;

function TGeoIP._GetOrg(IPNum: Cardinal; var GeoIPOrg: TGeoIPOrg): TGeoIPResult;
var
   SeekOrg: Cardinal;
   RecordPointer: Cardinal;
   StrLen: Cardinal;
   buf: array[0..MAX_ORG_RECORD_LENGTH-1] of Byte;
   p: PChar;
begin
  if (FDatabaseType <> GEOIP_ORG_EDITION) and (FDatabaseType <> GEOIP_ISP_EDITION) and (FDatabaseType <> GEOIP_ASNUM_EDITION) then
  begin
    Result := GEOIP_ERROR_DBTYPE;
    Exit;
  end;
  SeekOrg := SeekRecord(IPNum);
  if SeekOrg = FDatabaseSegments[0] then
  begin
    Result := GEOIP_NODATA;
    Exit;
  end;
  RecordPointer := SeekOrg + (2 * FRecordLength - 1) * FDatabaseSegments[0];
  FInputFile.Seek(RecordPointer, soFromBeginning);
  FInputFile.Read(buf, FULL_RECORD_LENGTH);

  p := @buf[0];
  StrLen := 0;
  while (p[StrLen] <> #0) do
    Inc(StrLen);
  GeoIPOrg.Name := Copy(p, 0, StrLen);
  Result := GEOIP_SUCCESS;
end;

function TGeoIP._GetRegion(IPNum: Cardinal; var GeoIPRegion: TGeoIPRegion): TGeoIPResult;
var
   SeekRegion: Cardinal;
begin
  if (FDatabaseType <> GEOIP_REGION_EDITION_REV0) and (FDatabaseType <> GEOIP_REGION_EDITION_REV1) then
  begin
    Result := GEOIP_ERROR_DBTYPE;
    Exit;
  end;
  SeekRegion := SeekRecord(IPNum);
  if FDatabaseType = GEOIP_REGION_EDITION_REV0 then
  begin
    // Region Edition, pre June 2003
    Dec(SeekRegion, STATE_BEGIN_REV0);
    if SeekRegion >= 1000 then
    begin
      GeoIPRegion.CountryCode := 'US';
      GeoIPRegion.Region := Chr((SeekRegion - 1000) div 26 + 65) + Chr((SeekRegion - 1000) mod 26 + 65);
    end
    else
    begin
      GeoIPRegion.CountryCode := CountryCodes[SeekRegion];
      GeoIPRegion.Region := '';
    end;
  end
  else if FDatabaseType = GEOIP_REGION_EDITION_REV1 then
  begin
    // Region Edition, post June 2003
    Dec(SeekRegion, STATE_BEGIN_REV1);
    if SeekRegion < US_OFFSET then
    begin
      // Unknown
      GeoIPRegion.CountryCode := '';
      GeoIPRegion.Region := '';
    end
    else if SeekRegion < CANADA_OFFSET then
    begin
      // USA State
      GeoIPRegion.CountryCode := 'US';
      GeoIPRegion.Region := Chr((SeekRegion - US_OFFSET) div 26 + 65) + Chr((SeekRegion - US_OFFSET) mod 26 + 65);
    end
    else if SeekRegion < WORLD_OFFSET then
    begin
      // Canada Province
      GeoIPRegion.CountryCode := 'CA';
      GeoIPRegion.Region := Chr((SeekRegion - CANADA_OFFSET) div 26 + 65) + Chr((SeekRegion - CANADA_OFFSET) mod 26 + 65);
    end
    else
    begin
      // Not US or Canada
      GeoIPRegion.CountryCode := CountryCodes[(SeekRegion - WORLD_OFFSET) div FIPS_RANGE];
      GeoIPRegion.Region := '';
    end;
  end;
  Result := GEOIP_SUCCESS;
end;

function TGeoIP.AddrToNum(const IPAddr: string): Cardinal;
{$ifdef FPC}
begin
  Result:=StrToHostAddr(IPAddr).s_addr;
end;
{$else}
var
   netlong: LongInt;
begin
  netlong := inet_addr(PChar(IPAddr));
  if netlong <> INADDR_NONE then
    Result := ntohl(netlong)
  else
    Result := 0;
end;
{$endif}

function TGeoIP.GetCity(const IPAddr: string; var GeoIPCity: TGeoIPCity): TGeoIPResult;
var
   IPNum: Cardinal;
begin
  IPNum := AddrToNum(IPAddr);
  if IPNum = 0 then
  begin
    Result := GEOIP_ERROR_IPADDR;
    Exit;
  end;
  Result := _GetCity(IPNum, GeoIPCity);
end;

function TGeoIP.GetCountry(const IPAddr: string; var GeoIPCountry: TGeoIPCountry): TGeoIPResult;
var
   IPNum: Cardinal;
begin
  IPNum := AddrToNum(IPAddr);
  if IPNum = 0 then
  begin
    Result := GEOIP_ERROR_IPADDR;
    Exit;
  end;
  Result := _GetCountry(IPNum, GeoIPCountry);
end;

function TGeoIP.GetDatabaseInfo: string;
var
   i: Integer;
   delim: array[0..2] of Byte;
   HasStructureInfo: Boolean;
begin
  FDatabaseInfo := '';
  HasStructureInfo := False;
  FInputFile.Seek(-3, soFromEnd);
  for i:=0 to STRUCTURE_INFO_MAX_SIZE-1 do
  begin
    FInputFile.Read(delim, 3);
    if (delim[0] = 255) and (delim[1] = 255) and (delim[2] = 255) then
    begin
      HasStructureInfo := True;
      Break;
    end;
    FInputFile.Seek(-4, soFromCurrent);
  end;
  if HasStructureInfo then
    FInputFile.Seek(-3, soFromCurrent)
  else
    // no structure info, must be pre Sep 2002 database, go back to end
    FInputFile.Seek(-3, soFromEnd);
  for i:=0 to DATABASE_INFO_MAX_SIZE-1 do
  begin
    FInputFile.Read(delim, 3);
    if (delim[0] = 0) and (delim[1] = 0) and (delim[2] = 0) then
    begin
      SetLength(FDatabaseInfo, i);
      FInputFile.Read(PChar(FDatabaseInfo)^, i);
      Break;
    end;
    FInputFile.Seek(-4, soFromCurrent);
  end;
  Result := FDatabaseInfo;
end;

function TGeoIP.GetOrg(const IPAddr: string; var GeoIPOrg: TGeoIPOrg): TGeoIPResult;
var
   IPNum: Cardinal;
begin
  IPNum := AddrToNum(IPAddr);
  if IPNum = 0 then
  begin
    Result := GEOIP_ERROR_IPADDR;
    Exit;
  end;
  Result := _GetOrg(IPNum, GeoIPOrg);
end;

function TGeoIP.GetRegion(const IPAddr: string; var GeoIPRegion: TGeoIPRegion): TGeoIPResult;
var
   IPNum: Cardinal;
begin
  IPNum := AddrToNum(IPAddr);
  if IPNum = 0 then
  begin
    Result := GEOIP_ERROR_IPADDR;
    Exit;
  end;
  Result := _GetRegion(IPNum, GeoIPRegion);
end;

procedure TGeoIP.InitDBFile;
var
   i,j: Integer;
   delim: array[0..2] of Byte;
   buf: array[0..SEGMENT_RECORD_LENGTH-1] of Byte;
begin
  // default to GeoIP Country Edition
  FDatabaseType := GEOIP_COUNTRY_EDITION;
  FRecordLength := STANDARD_RECORD_LENGTH;
  FInputFile.Seek(-3, soFromEnd);
  for i:=0 to STRUCTURE_INFO_MAX_SIZE-1 do
  begin
    FInputFile.Read(delim, 3);
    if (delim[0] = 255) and (delim[1] = 255) and (delim[2] = 255) then
    begin
      FInputFile.Read(FDatabaseType, 1);
      if Byte(FDatabaseType) >= 106 then
      begin
        // Backward compatibility with databases from April 2003 and earlier
        Dec(FDatabaseType, 105);
      end;
      if FDatabaseType = GEOIP_REGION_EDITION_REV0 then
      begin
        // Region Edition, pre June 2003
        SetLength(FDatabaseSegments, 1);
        FDatabaseSegments[0] := STATE_BEGIN_REV0;
      end
      else if FDatabaseType = GEOIP_REGION_EDITION_REV1 then
      begin
        // Region Edition, post June 2003
        SetLength(FDatabaseSegments, 1);
        FDatabaseSegments[0] := STATE_BEGIN_REV1;
      end
      else if (FDatabaseType = GEOIP_CITY_EDITION_REV0) or
              (FDatabaseType = GEOIP_CITY_EDITION_REV1) or
              (FDatabaseType = GEOIP_ORG_EDITION) or
              (FDatabaseType = GEOIP_ISP_EDITION) or
              (FDatabaseType = GEOIP_ASNUM_EDITION) then
      begin
        // City/Org Editions have two segments, read offset of second segment
        SetLength(FDatabaseSegments, 1);
        FDatabaseSegments[0] := 0;
        FInputFile.Read(buf, SEGMENT_RECORD_LENGTH);
        for j:=0 to SEGMENT_RECORD_LENGTH-1 do
        begin
          Inc(FDatabaseSegments[0], Integer(buf[j]) shl (j*8));
        end;
        if (FDatabaseType = GEOIP_ORG_EDITION) or
           (FDatabaseType = GEOIP_ISP_EDITION) then
             FRecordLength := ORG_RECORD_LENGTH;
      end;
      Break;
    end
    else
    begin
      FInputFile.Seek(-4, soFromCurrent);
    end;
  end;
  if (FDatabaseType = GEOIP_COUNTRY_EDITION) or
     (FDatabaseType = GEOIP_PROXY_EDITION) then
  begin
    SetLength(FDatabaseSegments, 1);
    FDatabaseSegments[0] := COUNTRY_BEGIN;
  end;
end;

function TGeoIP.SeekRecord(IPNum: Cardinal): Cardinal;
var
   depth: Cardinal;
   offset: Cardinal;
   i,j: Cardinal;
   x: array[0..1] of Cardinal;
   y: Cardinal;
   buf: array[0..2*MAX_RECORD_LENGTH-1] of Byte;
begin
  offset := 0;
  for depth:=31 downto 0 do
  begin
    FInputFile.Seek(2 * FRecordLength * offset, soFromBeginning);
    FInputFile.Read(buf, 2 * FRecordLength);
    for i:=0 to 1 do
    begin
      x[i] := 0;
      for j:=0 to FRecordLength-1 do
      begin
        y := buf[i*FRecordLength+j];
        x[i] := x[i] + (y shl (j*8));
      end;
    end;
    if (IPNum and (1 shl depth)) <> 0 then
    begin
      if x[1] >= FDatabaseSegments[0] then
      begin
        Result := x[1];
        Exit;
      end
      else
      begin
        Offset := x[1];
      end;
    end
    else
    begin
      if x[0] >= FDatabaseSegments[0] then
      begin
        Result := x[0];
        Exit;
      end
      else
      begin
        Offset := x[0];
      end;
    end;
  end;
  Result := 0;
end;

end.
