{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.000 |
|==============================================================================|
| Content: Encryption support                                                  |
|==============================================================================|
| Copyright (c)2007, Lukas Gebauer                                             |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2007.                     |
| All Rights Reserved.                                                         |
| Based on work of David Barton and Eric Young                                 |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Encryption support)

Implemented are DES and 3DES encryption/decryption by ECB, CBC, CFB-8bit,
 CFB-block, OFB and CTR methods.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

unit synacrypt;

interface

uses
  SysUtils, Classes, synautil;

type
  {:@abstract(Implementation of common routines for 64-bit block ciphers)

   Do not use this class directly, use descendants only!}
  TSynaBlockCipher= class(TObject)
  protected
    procedure InitKey(Key: AnsiString); virtual;
  private
    IV, CV: AnsiString;
    procedure IncCounter;
  public
    {:Sets the IV to Value and performs a reset}
    procedure SetIV(const Value: AnsiString); virtual;
    {:Returns the current chaining information, not the actual IV}
    function GetIV: AnsiString; virtual;
    {:Reset any stored chaining information}
    procedure Reset; virtual;
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; virtual;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CBC method of encryption}
    function EncryptCBC(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CBC method of decryption}
    function DecryptCBC(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CFB (8 bit) method of encryption}
    function EncryptCFB8bit(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CFB (8 bit) method of decryption}
    function DecryptCFB8bit(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CFB (block) method of encryption}
    function EncryptCFBblock(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CFB (block) method of decryption}
    function DecryptCFBblock(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the OFB method of encryption}
    function EncryptOFB(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the OFB method of decryption}
    function DecryptOFB(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CTR method of encryption}
    function EncryptCTR(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CTR method of decryption}
    function DecryptCTR(const Indata: AnsiString): AnsiString; virtual;
    {:Create a encryptor/decryptor instance and initialize it by the Key.}
    constructor Create(Key: AnsiString);
  end;

  {:@abstract(Datatype for holding one DES key data)

    This data type is used internally.}
  TDesKeyData = array[0..31] of integer;

  {:@abstract(Implementation of common routines for DES encryption)

   Do not use this class directly, use descendants only!}
  TSynaCustomDes = class(TSynaBlockcipher)
  protected
    procedure DoInit(KeyB: AnsiString; var KeyData: TDesKeyData);
    function EncryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
    function DecryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
  end;

  {:@abstract(Implementation of DES encryption)}
  TSynaDes= class(TSynaCustomDes)
  protected
    KeyData: TDesKeyData;
    procedure InitKey(Key: AnsiString); override;
  public
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; override;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; override;
  end;

  {:@abstract(Implementation of 3DES encryption)}
  TSyna3Des= class(TSynaCustomDes)
  protected
    KeyData: array[0..2] of TDesKeyData;
    procedure InitKey(Key: AnsiString); override;
  public
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; override;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; override;
  end;

{:Call internal test of all DES encryptions. Returns @true if all is OK.}
function TestDes: boolean;
{:Call internal test of all 3DES encryptions. Returns @true if all is OK.}
function Test3Des: boolean;

{==============================================================================}
implementation

const
  shifts2: array[0..15]of byte=
    (0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,0);

  des_skb: array[0..7,0..63]of integer=(
    (
    (* for C bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    integer($00000000),integer($00000010),integer($20000000),integer($20000010),
    integer($00010000),integer($00010010),integer($20010000),integer($20010010),
    integer($00000800),integer($00000810),integer($20000800),integer($20000810),
    integer($00010800),integer($00010810),integer($20010800),integer($20010810),
    integer($00000020),integer($00000030),integer($20000020),integer($20000030),
    integer($00010020),integer($00010030),integer($20010020),integer($20010030),
    integer($00000820),integer($00000830),integer($20000820),integer($20000830),
    integer($00010820),integer($00010830),integer($20010820),integer($20010830),
    integer($00080000),integer($00080010),integer($20080000),integer($20080010),
    integer($00090000),integer($00090010),integer($20090000),integer($20090010),
    integer($00080800),integer($00080810),integer($20080800),integer($20080810),
    integer($00090800),integer($00090810),integer($20090800),integer($20090810),
    integer($00080020),integer($00080030),integer($20080020),integer($20080030),
    integer($00090020),integer($00090030),integer($20090020),integer($20090030),
    integer($00080820),integer($00080830),integer($20080820),integer($20080830),
    integer($00090820),integer($00090830),integer($20090820),integer($20090830)
    ),(
    (* for C bits (numbered as per FIPS 46) 7 8 10 11 12 13 *)
    integer($00000000),integer($02000000),integer($00002000),integer($02002000),
    integer($00200000),integer($02200000),integer($00202000),integer($02202000),
    integer($00000004),integer($02000004),integer($00002004),integer($02002004),
    integer($00200004),integer($02200004),integer($00202004),integer($02202004),
    integer($00000400),integer($02000400),integer($00002400),integer($02002400),
    integer($00200400),integer($02200400),integer($00202400),integer($02202400),
    integer($00000404),integer($02000404),integer($00002404),integer($02002404),
    integer($00200404),integer($02200404),integer($00202404),integer($02202404),
    integer($10000000),integer($12000000),integer($10002000),integer($12002000),
    integer($10200000),integer($12200000),integer($10202000),integer($12202000),
    integer($10000004),integer($12000004),integer($10002004),integer($12002004),
    integer($10200004),integer($12200004),integer($10202004),integer($12202004),
    integer($10000400),integer($12000400),integer($10002400),integer($12002400),
    integer($10200400),integer($12200400),integer($10202400),integer($12202400),
    integer($10000404),integer($12000404),integer($10002404),integer($12002404),
    integer($10200404),integer($12200404),integer($10202404),integer($12202404)
    ),(
    (* for C bits (numbered as per FIPS 46) 14 15 16 17 19 20 *)
    integer($00000000),integer($00000001),integer($00040000),integer($00040001),
    integer($01000000),integer($01000001),integer($01040000),integer($01040001),
    integer($00000002),integer($00000003),integer($00040002),integer($00040003),
    integer($01000002),integer($01000003),integer($01040002),integer($01040003),
    integer($00000200),integer($00000201),integer($00040200),integer($00040201),
    integer($01000200),integer($01000201),integer($01040200),integer($01040201),
    integer($00000202),integer($00000203),integer($00040202),integer($00040203),
    integer($01000202),integer($01000203),integer($01040202),integer($01040203),
    integer($08000000),integer($08000001),integer($08040000),integer($08040001),
    integer($09000000),integer($09000001),integer($09040000),integer($09040001),
    integer($08000002),integer($08000003),integer($08040002),integer($08040003),
    integer($09000002),integer($09000003),integer($09040002),integer($09040003),
    integer($08000200),integer($08000201),integer($08040200),integer($08040201),
    integer($09000200),integer($09000201),integer($09040200),integer($09040201),
    integer($08000202),integer($08000203),integer($08040202),integer($08040203),
    integer($09000202),integer($09000203),integer($09040202),integer($09040203)
    ),(
    (* for C bits (numbered as per FIPS 46) 21 23 24 26 27 28 *)
    integer($00000000),integer($00100000),integer($00000100),integer($00100100),
    integer($00000008),integer($00100008),integer($00000108),integer($00100108),
    integer($00001000),integer($00101000),integer($00001100),integer($00101100),
    integer($00001008),integer($00101008),integer($00001108),integer($00101108),
    integer($04000000),integer($04100000),integer($04000100),integer($04100100),
    integer($04000008),integer($04100008),integer($04000108),integer($04100108),
    integer($04001000),integer($04101000),integer($04001100),integer($04101100),
    integer($04001008),integer($04101008),integer($04001108),integer($04101108),
    integer($00020000),integer($00120000),integer($00020100),integer($00120100),
    integer($00020008),integer($00120008),integer($00020108),integer($00120108),
    integer($00021000),integer($00121000),integer($00021100),integer($00121100),
    integer($00021008),integer($00121008),integer($00021108),integer($00121108),
    integer($04020000),integer($04120000),integer($04020100),integer($04120100),
    integer($04020008),integer($04120008),integer($04020108),integer($04120108),
    integer($04021000),integer($04121000),integer($04021100),integer($04121100),
    integer($04021008),integer($04121008),integer($04021108),integer($04121108)
    ),(
    (* for D bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    integer($00000000),integer($10000000),integer($00010000),integer($10010000),
    integer($00000004),integer($10000004),integer($00010004),integer($10010004),
    integer($20000000),integer($30000000),integer($20010000),integer($30010000),
    integer($20000004),integer($30000004),integer($20010004),integer($30010004),
    integer($00100000),integer($10100000),integer($00110000),integer($10110000),
    integer($00100004),integer($10100004),integer($00110004),integer($10110004),
    integer($20100000),integer($30100000),integer($20110000),integer($30110000),
    integer($20100004),integer($30100004),integer($20110004),integer($30110004),
    integer($00001000),integer($10001000),integer($00011000),integer($10011000),
    integer($00001004),integer($10001004),integer($00011004),integer($10011004),
    integer($20001000),integer($30001000),integer($20011000),integer($30011000),
    integer($20001004),integer($30001004),integer($20011004),integer($30011004),
    integer($00101000),integer($10101000),integer($00111000),integer($10111000),
    integer($00101004),integer($10101004),integer($00111004),integer($10111004),
    integer($20101000),integer($30101000),integer($20111000),integer($30111000),
    integer($20101004),integer($30101004),integer($20111004),integer($30111004)
    ),(
    (* for D bits (numbered as per FIPS 46) 8 9 11 12 13 14 *)
    integer($00000000),integer($08000000),integer($00000008),integer($08000008),
    integer($00000400),integer($08000400),integer($00000408),integer($08000408),
    integer($00020000),integer($08020000),integer($00020008),integer($08020008),
    integer($00020400),integer($08020400),integer($00020408),integer($08020408),
    integer($00000001),integer($08000001),integer($00000009),integer($08000009),
    integer($00000401),integer($08000401),integer($00000409),integer($08000409),
    integer($00020001),integer($08020001),integer($00020009),integer($08020009),
    integer($00020401),integer($08020401),integer($00020409),integer($08020409),
    integer($02000000),integer($0A000000),integer($02000008),integer($0A000008),
    integer($02000400),integer($0A000400),integer($02000408),integer($0A000408),
    integer($02020000),integer($0A020000),integer($02020008),integer($0A020008),
    integer($02020400),integer($0A020400),integer($02020408),integer($0A020408),
    integer($02000001),integer($0A000001),integer($02000009),integer($0A000009),
    integer($02000401),integer($0A000401),integer($02000409),integer($0A000409),
    integer($02020001),integer($0A020001),integer($02020009),integer($0A020009),
    integer($02020401),integer($0A020401),integer($02020409),integer($0A020409)
    ),(
    (* for D bits (numbered as per FIPS 46) 16 17 18 19 20 21 *)
    integer($00000000),integer($00000100),integer($00080000),integer($00080100),
    integer($01000000),integer($01000100),integer($01080000),integer($01080100),
    integer($00000010),integer($00000110),integer($00080010),integer($00080110),
    integer($01000010),integer($01000110),integer($01080010),integer($01080110),
    integer($00200000),integer($00200100),integer($00280000),integer($00280100),
    integer($01200000),integer($01200100),integer($01280000),integer($01280100),
    integer($00200010),integer($00200110),integer($00280010),integer($00280110),
    integer($01200010),integer($01200110),integer($01280010),integer($01280110),
    integer($00000200),integer($00000300),integer($00080200),integer($00080300),
    integer($01000200),integer($01000300),integer($01080200),integer($01080300),
    integer($00000210),integer($00000310),integer($00080210),integer($00080310),
    integer($01000210),integer($01000310),integer($01080210),integer($01080310),
    integer($00200200),integer($00200300),integer($00280200),integer($00280300),
    integer($01200200),integer($01200300),integer($01280200),integer($01280300),
    integer($00200210),integer($00200310),integer($00280210),integer($00280310),
    integer($01200210),integer($01200310),integer($01280210),integer($01280310)
    ),(
    (* for D bits (numbered as per FIPS 46) 22 23 24 25 27 28 *)
    integer($00000000),integer($04000000),integer($00040000),integer($04040000),
    integer($00000002),integer($04000002),integer($00040002),integer($04040002),
    integer($00002000),integer($04002000),integer($00042000),integer($04042000),
    integer($00002002),integer($04002002),integer($00042002),integer($04042002),
    integer($00000020),integer($04000020),integer($00040020),integer($04040020),
    integer($00000022),integer($04000022),integer($00040022),integer($04040022),
    integer($00002020),integer($04002020),integer($00042020),integer($04042020),
    integer($00002022),integer($04002022),integer($00042022),integer($04042022),
    integer($00000800),integer($04000800),integer($00040800),integer($04040800),
    integer($00000802),integer($04000802),integer($00040802),integer($04040802),
    integer($00002800),integer($04002800),integer($00042800),integer($04042800),
    integer($00002802),integer($04002802),integer($00042802),integer($04042802),
    integer($00000820),integer($04000820),integer($00040820),integer($04040820),
    integer($00000822),integer($04000822),integer($00040822),integer($04040822),
    integer($00002820),integer($04002820),integer($00042820),integer($04042820),
    integer($00002822),integer($04002822),integer($00042822),integer($04042822)
    ));

  des_sptrans: array[0..7,0..63] of integer=(
    (
    (* nibble 0 *)
    integer($02080800), integer($00080000), integer($02000002), integer($02080802),
    integer($02000000), integer($00080802), integer($00080002), integer($02000002),
    integer($00080802), integer($02080800), integer($02080000), integer($00000802),
    integer($02000802), integer($02000000), integer($00000000), integer($00080002),
    integer($00080000), integer($00000002), integer($02000800), integer($00080800),
    integer($02080802), integer($02080000), integer($00000802), integer($02000800),
    integer($00000002), integer($00000800), integer($00080800), integer($02080002),
    integer($00000800), integer($02000802), integer($02080002), integer($00000000),
    integer($00000000), integer($02080802), integer($02000800), integer($00080002),
    integer($02080800), integer($00080000), integer($00000802), integer($02000800),
    integer($02080002), integer($00000800), integer($00080800), integer($02000002),
    integer($00080802), integer($00000002), integer($02000002), integer($02080000),
    integer($02080802), integer($00080800), integer($02080000), integer($02000802),
    integer($02000000), integer($00000802), integer($00080002), integer($00000000),
    integer($00080000), integer($02000000), integer($02000802), integer($02080800),
    integer($00000002), integer($02080002), integer($00000800), integer($00080802)
    ),(
    (* nibble 1 *)
    integer($40108010), integer($00000000), integer($00108000), integer($40100000),
    integer($40000010), integer($00008010), integer($40008000), integer($00108000),
    integer($00008000), integer($40100010), integer($00000010), integer($40008000),
    integer($00100010), integer($40108000), integer($40100000), integer($00000010),
    integer($00100000), integer($40008010), integer($40100010), integer($00008000),
    integer($00108010), integer($40000000), integer($00000000), integer($00100010),
    integer($40008010), integer($00108010), integer($40108000), integer($40000010),
    integer($40000000), integer($00100000), integer($00008010), integer($40108010),
    integer($00100010), integer($40108000), integer($40008000), integer($00108010),
    integer($40108010), integer($00100010), integer($40000010), integer($00000000),
    integer($40000000), integer($00008010), integer($00100000), integer($40100010),
    integer($00008000), integer($40000000), integer($00108010), integer($40008010),
    integer($40108000), integer($00008000), integer($00000000), integer($40000010),
    integer($00000010), integer($40108010), integer($00108000), integer($40100000),
    integer($40100010), integer($00100000), integer($00008010), integer($40008000),
    integer($40008010), integer($00000010), integer($40100000), integer($00108000)
    ),(
    (* nibble 2 *)
    integer($04000001), integer($04040100), integer($00000100), integer($04000101),
    integer($00040001), integer($04000000), integer($04000101), integer($00040100),
    integer($04000100), integer($00040000), integer($04040000), integer($00000001),
    integer($04040101), integer($00000101), integer($00000001), integer($04040001),
    integer($00000000), integer($00040001), integer($04040100), integer($00000100),
    integer($00000101), integer($04040101), integer($00040000), integer($04000001),
    integer($04040001), integer($04000100), integer($00040101), integer($04040000),
    integer($00040100), integer($00000000), integer($04000000), integer($00040101),
    integer($04040100), integer($00000100), integer($00000001), integer($00040000),
    integer($00000101), integer($00040001), integer($04040000), integer($04000101),
    integer($00000000), integer($04040100), integer($00040100), integer($04040001),
    integer($00040001), integer($04000000), integer($04040101), integer($00000001),
    integer($00040101), integer($04000001), integer($04000000), integer($04040101),
    integer($00040000), integer($04000100), integer($04000101), integer($00040100),
    integer($04000100), integer($00000000), integer($04040001), integer($00000101),
    integer($04000001), integer($00040101), integer($00000100), integer($04040000)
    ),(
    (* nibble 3 *)
    integer($00401008), integer($10001000), integer($00000008), integer($10401008),
    integer($00000000), integer($10400000), integer($10001008), integer($00400008),
    integer($10401000), integer($10000008), integer($10000000), integer($00001008),
    integer($10000008), integer($00401008), integer($00400000), integer($10000000),
    integer($10400008), integer($00401000), integer($00001000), integer($00000008),
    integer($00401000), integer($10001008), integer($10400000), integer($00001000),
    integer($00001008), integer($00000000), integer($00400008), integer($10401000),
    integer($10001000), integer($10400008), integer($10401008), integer($00400000),
    integer($10400008), integer($00001008), integer($00400000), integer($10000008),
    integer($00401000), integer($10001000), integer($00000008), integer($10400000),
    integer($10001008), integer($00000000), integer($00001000), integer($00400008),
    integer($00000000), integer($10400008), integer($10401000), integer($00001000),
    integer($10000000), integer($10401008), integer($00401008), integer($00400000),
    integer($10401008), integer($00000008), integer($10001000), integer($00401008),
    integer($00400008), integer($00401000), integer($10400000), integer($10001008),
    integer($00001008), integer($10000000), integer($10000008), integer($10401000)
    ),(
    (* nibble 4 *)
    integer($08000000), integer($00010000), integer($00000400), integer($08010420),
    integer($08010020), integer($08000400), integer($00010420), integer($08010000),
    integer($00010000), integer($00000020), integer($08000020), integer($00010400),
    integer($08000420), integer($08010020), integer($08010400), integer($00000000),
    integer($00010400), integer($08000000), integer($00010020), integer($00000420),
    integer($08000400), integer($00010420), integer($00000000), integer($08000020),
    integer($00000020), integer($08000420), integer($08010420), integer($00010020),
    integer($08010000), integer($00000400), integer($00000420), integer($08010400),
    integer($08010400), integer($08000420), integer($00010020), integer($08010000),
    integer($00010000), integer($00000020), integer($08000020), integer($08000400),
    integer($08000000), integer($00010400), integer($08010420), integer($00000000),
    integer($00010420), integer($08000000), integer($00000400), integer($00010020),
    integer($08000420), integer($00000400), integer($00000000), integer($08010420),
    integer($08010020), integer($08010400), integer($00000420), integer($00010000),
    integer($00010400), integer($08010020), integer($08000400), integer($00000420),
    integer($00000020), integer($00010420), integer($08010000), integer($08000020)
    ),(
    (* nibble 5 *)
    integer($80000040), integer($00200040), integer($00000000), integer($80202000),
    integer($00200040), integer($00002000), integer($80002040), integer($00200000),
    integer($00002040), integer($80202040), integer($00202000), integer($80000000),
    integer($80002000), integer($80000040), integer($80200000), integer($00202040),
    integer($00200000), integer($80002040), integer($80200040), integer($00000000),
    integer($00002000), integer($00000040), integer($80202000), integer($80200040),
    integer($80202040), integer($80200000), integer($80000000), integer($00002040),
    integer($00000040), integer($00202000), integer($00202040), integer($80002000),
    integer($00002040), integer($80000000), integer($80002000), integer($00202040),
    integer($80202000), integer($00200040), integer($00000000), integer($80002000),
    integer($80000000), integer($00002000), integer($80200040), integer($00200000),
    integer($00200040), integer($80202040), integer($00202000), integer($00000040),
    integer($80202040), integer($00202000), integer($00200000), integer($80002040),
    integer($80000040), integer($80200000), integer($00202040), integer($00000000),
    integer($00002000), integer($80000040), integer($80002040), integer($80202000),
    integer($80200000), integer($00002040), integer($00000040), integer($80200040)
    ),(
    (* nibble 6 *)
    integer($00004000), integer($00000200), integer($01000200), integer($01000004),
    integer($01004204), integer($00004004), integer($00004200), integer($00000000),
    integer($01000000), integer($01000204), integer($00000204), integer($01004000),
    integer($00000004), integer($01004200), integer($01004000), integer($00000204),
    integer($01000204), integer($00004000), integer($00004004), integer($01004204),
    integer($00000000), integer($01000200), integer($01000004), integer($00004200),
    integer($01004004), integer($00004204), integer($01004200), integer($00000004),
    integer($00004204), integer($01004004), integer($00000200), integer($01000000),
    integer($00004204), integer($01004000), integer($01004004), integer($00000204),
    integer($00004000), integer($00000200), integer($01000000), integer($01004004),
    integer($01000204), integer($00004204), integer($00004200), integer($00000000),
    integer($00000200), integer($01000004), integer($00000004), integer($01000200),
    integer($00000000), integer($01000204), integer($01000200), integer($00004200),
    integer($00000204), integer($00004000), integer($01004204), integer($01000000),
    integer($01004200), integer($00000004), integer($00004004), integer($01004204),
    integer($01000004), integer($01004200), integer($01004000), integer($00004004)
    ),(
    (* nibble 7 *)
    integer($20800080), integer($20820000), integer($00020080), integer($00000000),
    integer($20020000), integer($00800080), integer($20800000), integer($20820080),
    integer($00000080), integer($20000000), integer($00820000), integer($00020080),
    integer($00820080), integer($20020080), integer($20000080), integer($20800000),
    integer($00020000), integer($00820080), integer($00800080), integer($20020000),
    integer($20820080), integer($20000080), integer($00000000), integer($00820000),
    integer($20000000), integer($00800000), integer($20020080), integer($20800080),
    integer($00800000), integer($00020000), integer($20820000), integer($00000080),
    integer($00800000), integer($00020000), integer($20000080), integer($20820080),
    integer($00020080), integer($20000000), integer($00000000), integer($00820000),
    integer($20800080), integer($20020080), integer($20020000), integer($00800080),
    integer($20820000), integer($00000080), integer($00800080), integer($20020000),
    integer($20820080), integer($00800000), integer($20800000), integer($20000080),
    integer($00820000), integer($00020080), integer($20020080), integer($20800000),
    integer($00000080), integer($20820000), integer($00820080), integer($00000000),
    integer($20000000), integer($20800080), integer($00020000), integer($00820080)
    ));

{==============================================================================}

function XorString(Indata1, Indata2: AnsiString): AnsiString;
var
  i: integer;
begin
  Indata2 := PadString(Indata2, length(Indata1), #0);
  Result := '';
  for i := 1 to length(Indata1) do
    Result := Result + AnsiChar(ord(Indata1[i]) xor ord(Indata2[i]));
end;

procedure hperm_op(var a, t: integer; n, m: integer);
begin
  t:= ((a shl (16 - n)) xor a) and m;
  a:= a xor t xor (t shr (16 - n));
end;

procedure perm_op(var a, b, t: integer; n, m: integer);
begin
  t:= ((a shr n) xor b) and m;
  b:= b xor t;
  a:= a xor (t shl n);
end;

{==============================================================================}
procedure TSynaBlockCipher.IncCounter;
var
  i: integer;
begin
  Inc(CV[8]);
  i:= 7;
  while (i> 0) and (CV[i + 1] = #0) do
  begin
    Inc(CV[i]);
    Dec(i);
  end;
end;

procedure TSynaBlockCipher.Reset;
begin
  CV := IV;
end;

procedure TSynaBlockCipher.InitKey(Key: AnsiString);
begin
end;

procedure TSynaBlockCipher.SetIV(const Value: AnsiString);
begin
  IV := PadString(Value, 8, #0);
  Reset;
end;

function TSynaBlockCipher.GetIV: AnsiString;
begin
  Result := CV;
end;

function TSynaBlockCipher.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := InData;
end;

function TSynaBlockCipher.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := InData;
end;

function TSynaBlockCipher.EncryptCBC(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: ansistring;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, CV);
    s := EncryptECB(s);
    CV := s;
    Result := Result + s;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCBC(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s, temp: ansistring;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    temp := s;
    s := DecryptECB(s);
    s := XorString(s, CV);
    Result := Result + s;
    CV := Temp;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptCFB8bit(const Indata: AnsiString): AnsiString;
var
  i: integer;
  Temp: AnsiString;
  c: AnsiChar;
begin
  Result := '';
  for i:= 1 to Length(Indata) do
  begin
    Temp := EncryptECB(CV);
    c := AnsiChar(ord(InData[i]) xor ord(temp[1]));
    Result := Result + c;
    Delete(CV, 1, 1);
    CV := CV + c;
  end;
end;

function TSynaBlockCipher.DecryptCFB8bit(const Indata: AnsiString): AnsiString;
var
  i: integer;
  Temp: AnsiString;
  c: AnsiChar;
begin
  Result := '';
  for i:= 1 to length(Indata) do
  begin
    c:= Indata[i];
    Temp := EncryptECB(CV);
    Result := Result + AnsiChar(ord(InData[i]) xor ord(temp[1]));
    Delete(CV, 1, 1);
    CV := CV + c;
  end;
end;

function TSynaBlockCipher.EncryptCFBblock(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, CV);
    Result := Result + s;
    CV := s;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCFBblock(const Indata: AnsiString): AnsiString;
var
  i: integer;
  S, Temp: AnsiString;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    Temp := s;
    CV := EncryptECB(CV);
    s := XorString(s, CV);
    Result := result + s;
    CV := temp;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptOFB(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptOFB(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    Cv := EncryptECB(CV);
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
  if (l mod 8)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptCTR(const Indata: AnsiString): AnsiString;
var
  temp: AnsiString;
  i: integer;
  s: AnsiString;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, temp);
    Result := Result + s;
  end;
  if (l mod 8)<> 0 then
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, temp);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCTR(const Indata: AnsiString): AnsiString;
var
  temp: AnsiString;
  s: AnsiString;
  i: integer;
  l: integer;
begin
  Result := '';
  l := Length(InData);
  for i:= 1 to (l div 8) do
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (i - 1) * 8 + 1, 8);
    s := XorString(s, temp);
    Result := Result + s;
  end;
  if (l mod 8)<> 0 then
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (l div 8) * 8 + 1, l mod 8);
    s := XorString(s, temp);
    Result := Result + s;
  end;
end;

constructor TSynaBlockCipher.Create(Key: AnsiString);
begin
  inherited Create;
  InitKey(Key);
  IV := StringOfChar(#0, 8);
  IV := EncryptECB(IV);
  Reset;
end;

{==============================================================================}

procedure TSynaCustomDes.DoInit(KeyB: AnsiString; var KeyData: TDesKeyData);
var
  c, d, t, s, t2, i: integer;
begin
  KeyB := PadString(KeyB, 8, #0);
  c:= ord(KeyB[1]) or (ord(KeyB[2]) shl 8) or (ord(KeyB[3]) shl 16) or (ord(KeyB[4]) shl 24);
  d:= ord(KeyB[5]) or (ord(KeyB[6]) shl 8) or (ord(KeyB[7]) shl 16) or (ord(KeyB[8]) shl 24);
  perm_op(d,c,t,4,integer($0f0f0f0f));
  hperm_op(c,t,integer(-2),integer($cccc0000));
  hperm_op(d,t,integer(-2),integer($cccc0000));
  perm_op(d,c,t,1,integer($55555555));
  perm_op(c,d,t,8,integer($00ff00ff));
  perm_op(d,c,t,1,integer($55555555));
  d:= ((d and $ff) shl 16) or (d and $ff00) or ((d and $ff0000) shr 16) or
        ((c and integer($f0000000)) shr 4);
  c:= c and $fffffff;
  for i:= 0 to 15 do
  begin
    if shifts2[i]<> 0 then
    begin
      c:= ((c shr 2) or (c shl 26));
      d:= ((d shr 2) or (d shl 26));
    end
    else
    begin
      c:= ((c shr 1) or (c shl 27));
      d:= ((d shr 1) or (d shl 27));
    end;
    c:= c and $fffffff;
    d:= d and $fffffff;
    s:= des_skb[0,c and $3f] or
        des_skb[1,((c shr  6) and $03) or ((c shr  7) and $3c)] or
        des_skb[2,((c shr 13) and $0f) or ((c shr 14) and $30)] or
        des_skb[3,((c shr 20) and $01) or ((c shr 21) and $06) or ((c shr 22) and $38)];
    t:= des_skb[4,d and $3f] or
        des_skb[5,((d shr  7) and $03) or ((d shr  8) and $3c)] or
        des_skb[6, (d shr 15) and $3f                         ] or
        des_skb[7,((d shr 21) and $0f) or ((d shr 22) and $30)];
    t2:= ((t shl 16) or (s and $ffff));
    KeyData[(i shl 1)]:= ((t2 shl 2) or (t2 shr 30));
    t2:= ((s shr 16) or (t and integer($ffff0000)));
    KeyData[(i shl 1)+1]:= ((t2 shl 6) or (t2 shr 26));
  end;
end;

function TSynaCustomDes.EncryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
var
  l, r, t, u: integer;
  i: longint;
begin
  r := Swapbytes(DecodeLongint(Indata, 1));
  l := swapbytes(DecodeLongint(Indata, 5));
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 0;
  while i< 32 do
  begin
    u:= r xor KeyData[i  ];
    t:= r xor KeyData[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i+2];
    t:= l xor KeyData[i+3];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData[i+4];
    t:= r xor KeyData[i+5];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i+6];
    t:= l xor KeyData[i+7];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Inc(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  Result := CodeLongInt(Swapbytes(l)) + CodeLongInt(Swapbytes(r));
end;

function TSynaCustomDes.DecryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
var
  l, r, t, u: integer;
  i: longint;
begin
  r := Swapbytes(DecodeLongint(Indata, 1));
  l := Swapbytes(DecodeLongint(Indata, 5));
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 30;
  while i> 0 do
  begin
    u:= r xor KeyData[i  ];
    t:= r xor KeyData[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i-2];
    t:= l xor KeyData[i-1];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData[i-4];
    t:= r xor KeyData[i-3];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i-6];
    t:= l xor KeyData[i-5];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Dec(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  Result := CodeLongInt(Swapbytes(l)) + CodeLongInt(Swapbytes(r));
end;

{==============================================================================}

procedure TSynaDes.InitKey(Key: AnsiString);
begin
  Key := PadString(Key, 8, #0);
  DoInit(Key,KeyData);
end;

function TSynaDes.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := EncryptBlock(InData,KeyData);
end;

function TSynaDes.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := DecryptBlock(Indata,KeyData);
end;

{==============================================================================}

procedure TSyna3Des.InitKey(Key: AnsiString);
var
  Size: integer;
  n: integer;
begin
  Size := length(Key);
  key := PadString(key, 3 * 8, #0);
  DoInit(Copy(key, 1, 8),KeyData[0]);
  DoInit(Copy(key, 9, 8),KeyData[1]);
  if Size > 16 then
    DoInit(Copy(key, 17, 8),KeyData[2])
  else
    for n := 0 to high(KeyData[0]) do
      KeyData[2][n] := Keydata[0][n];
end;

function TSyna3Des.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := EncryptBlock(Indata,KeyData[0]);
  Result := DecryptBlock(Result,KeyData[1]);
  Result := EncryptBlock(Result,KeyData[2]);
end;

function TSyna3Des.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := DecryptBlock(InData,KeyData[2]);
  Result := EncryptBlock(Result,KeyData[1]);
  Result := DecryptBlock(Result,KeyData[0]);
end;

{==============================================================================}

function TestDes: boolean;
var
  des: TSynaDes;
  s, t: string;
const
  key = '01234567';
  data1= '01234567';
  data2= '0123456789abcdefghij';
begin
  //ECB
  des := TSynaDes.Create(key);
  try
    s := des.EncryptECB(data1);
    t := strtohex(s);
    result := t = 'c50ad028c6da9800';
    s := des.DecryptECB(s);
    result := result and (data1 = s);
  finally
    des.free;
  end;
  //CBC
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCBC(data2);
    t := strtohex(s);
    result := result and (t = 'eec50f6353115ad6dee90a22ed1b6a88a0926e35');
    des.Reset;
    s := des.DecryptCBC(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-8bit
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCFB8bit(data2);
    t := strtohex(s);
    result := result and (t = 'eb6aa12c2f0ff634b4dfb6da6cb2af8f9c5c1452');
    des.Reset;
    s := des.DecryptCFB8bit(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-block
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCFBblock(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cdec28605e07f9b7f3be1053257');
    des.Reset;
    s := des.DecryptCFBblock(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //OFB
  des := TSynaDes.Create(key);
  try
    s := des.EncryptOFB(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cdee0b8b3798c4c34baac87dbdc');
    des.Reset;
    s := des.DecryptOFB(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CTR
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCTR(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cde0dd20b45f3afd9aa1b91b87e');
    des.Reset;
    s := des.DecryptCTR(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
end;

function Test3Des: boolean;
var
  des: TSyna3Des;
  s, t: string;
const
  key = '0123456789abcdefghijklmn';
  data1= '01234567';
  data2= '0123456789abcdefghij';
begin
  //ECB
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptECB(data1);
    t := strtohex(s);
    result := t = 'e0dee91008dc460c';
    s := des.DecryptECB(s);
    result := result and (data1 = s);
  finally
    des.free;
  end;
  //CBC
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCBC(data2);
    t := strtohex(s);
    result := result and (t = 'ee844a2a4f49c01b91a1599b8eba29128c1ad87a');
    des.Reset;
    s := des.DecryptCBC(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-8bit
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCFB8bit(data2);
    t := strtohex(s);
    result := result and (t = '935bbf5210c32cfa1faf61f91e8dc02dfa0ff1e8');
    des.Reset;
    s := des.DecryptCFB8bit(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-block
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCFBblock(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf4bd81f1739419e8d2cfe1671');
    des.Reset;
    s := des.DecryptCFBblock(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //OFB
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptOFB(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf04ef0a5efc926ebdf2d95f20');
    des.Reset;
    s := des.DecryptOFB(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CTR
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCTR(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf1c51a121d2c93f989e70b3ad');
    des.Reset;
    s := des.DecryptCTR(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
end;

{==============================================================================}

end.
