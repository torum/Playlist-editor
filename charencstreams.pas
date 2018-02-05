{

Copyright (c) 2006-2007 Jan Behrens, FlexiGuided GmbH, Berlin

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.


This software distribution contains derived data from a modified version of
the Unicode data files. The following license applies to that data:

COPYRIGHT AND PERMISSION NOTICE

Copyright (c) 1991-2007 Unicode, Inc. All rights reserved. Distributed
under the Terms of Use in http://www.unicode.org/copyright.html.

Permission is hereby granted, free of charge, to any person obtaining a
copy of the Unicode data files and any associated documentation (the "Data
Files") or Unicode software and any associated documentation (the
"Software") to deal in the Data Files or Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, and/or sell copies of the Data Files or Software, and
to permit persons to whom the Data Files or Software are furnished to do
so, provided that (a) the above copyright notice(s) and this permission
notice appear with all copies of the Data Files or Software, (b) both the
above copyright notice(s) and this permission notice appear in associated
documentation, and (c) there is clear notice in each modified Data File or
in the Software as well as in the documentation associated with the Data
File(s) or Software that the data or software has been modified.

THE DATA FILES AND SOFTWARE ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF
THIRD PARTY RIGHTS. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR HOLDERS
INCLUDED IN THIS NOTICE BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR
CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THE DATA FILES OR SOFTWARE.

Except as contained in this notice, the name of a copyright holder shall
not be used in advertising or otherwise to promote the sale, use or other
dealings in these Data Files or Software without prior written
authorization of the copyright holder.


Unicode and the Unicode logo are trademarks of Unicode, Inc., and may be
registered in some jurisdictions. All other trademarks and registered
trademarks mentioned herein are the property of their respective owners.


}

unit charencstreams;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TUniStreamTypes = (ufUtf8, ufANSI, ufUtf16be, ufUtf16le, ufUtf32be, ufUtf32le);

  TUArr = array of DWord;

const
  UniStreamTypesStrings: array[0..5] of ShortString = ('UTF8', 'ANSI', 'UTF16BE', 'UTF16LE', 'UTF32BE', 'UTF32LE');

  UTF8BOM: string = #$EF#$BB#$BF;
  UTF16BEBOM: string = #$FE#$FF;
  UTF16LEBOM: string = #$FF#$FE;
  UTF32BEBOM: string = #$00#$00#$FE#$FF;
  UTF32LEBOM: string = #$FF#$FE#$00#$00;

type

{ TUniStream }

  TUniStream = class(TMemoryStream)
  private
    fForceType: Boolean;
    fHasBOM: Boolean;
    fHaveType: Boolean;
    fUniStreamType: TUniStreamTypes;
    function GetOffset: int64;
    function GetUniStreamType: TUniStreamTypes;
    procedure SetUniStreamType(const AValue: TUniStreamTypes);
  protected
    procedure CheckFileType;
    function GetUTF8Text: AnsiString; virtual;
    procedure SetUTF8Text(AString: AnsiString); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    property HasBOM: Boolean read fHasBOM write fHasBOM;
    property UniStreamType: TUniStreamTypes read GetUniStreamType write SetUniStreamType;
    property UTF8Text: AnsiString read GetUTF8Text write SetUTF8Text;
    property ForceType: Boolean read fForceType write fForceType;
    property HaveType: Boolean read fHaveType write fHaveType;
    property Offset: int64 read GetOffset;
  end;


{ TCharEncStream }

  TCharEncStream = class(TUniStream)
  private
    fANSIEnc: string;
    function GetANSIEnc: string;
    procedure SetANSIEnc(const AValue: string);
  protected
    function GetUTF8Text: AnsiString; override;
    procedure SetUTF8Text(AString: AnsiString); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ANSIEnc: string read GetANSIEnc write SetANSIEnc;
  end;

procedure GetSupportedANSIEncodings(List: TStrings);
procedure GetSupportedUniStreamTypes(List: TStrings);
function GetSystemEncoding: string;


implementation

uses LazUTF8, LConvEncoding, Math;   

procedure GetSupportedANSIEncodings(List: TStrings);
begin
  LConvEncoding.GetSupportedEncodings(List);
end;

procedure GetSupportedUniStreamTypes(List: TStrings);
var i:integer;
begin
  for i := 0 to High(UniStreamTypesStrings) do List.Add(UniStreamTypesStrings[i]);
end;

function GetSystemEncoding: string;
begin

     Result := LConvEncoding.GetDefaultTextEncoding;

  if Result='ansi' then Result:='ANSI';
  if Result='utf8' then Result:='UTF-8';
end;

procedure WideSwapEndian(PWC: PWideChar;size:integer);
begin
  while size >= sizeof(widechar) do
  begin
    PWC^ := WideChar(SwapEndian(Word(PWC^)));
    inc(PWC);
    dec(size,sizeof(widechar));
  end;
end;

procedure UASwapEndian(var UC: TUArr);
var i: integer;
begin
  for i := 0 to High(UC) do UC[i] := SwapEndian(DWord(UC[i]));
end;


{ TUniStream }

function TUniStream.GetUniStreamType: TUniStreamTypes;
begin
  if not fHaveType then CheckFileType;
  Result := fUniStreamType;
end;

procedure TUniStream.CheckFileType;
var ASt: string[5];
  Str: AnsiString;
  Posi, rd: integer;
begin
  Ast := #0#0#0#0#0;
  if GetSystemEncoding = EncodingUTF8 then fUniStreamType := ufUTF8 else fUniStreamType := ufANSI;
  fHasBOM := False;
  Position := 0;
  rd := Read(ASt[1], 4);
  begin
    if (rd > 2) and (Copy(Ast, 1, 3) = UTF8BOM) then begin fUniStreamType := ufUtf8; fHasBOM := True; end else
      if (rd > 3) and (Copy(Ast, 1, 4) = UTF32LEBOM) then begin fUniStreamType := ufUtf32le; fHasBOM := True; end else
        if (rd > 3) and (Copy(Ast, 1, 4) = UTF32BEBOM) then begin fUniStreamType := ufUtf32be; fHasBOM := True; end else
          if (rd > 1) and (Copy(Ast, 1, 2) = UTF16LEBOM) then begin fUniStreamType := ufUtf16le; fHasBOM := True; end else
            if (rd > 1) and (Copy(Ast, 1, 2) = UTF16BEBOM) then begin fUniStreamType := ufUtf16be; fHasBOM := True; end;
    Position := 0;
    fHaveType := True;
  end;
  if not fHasBom then
  begin
    SetLength(Str, Min(2048, Size));
    if Length(Str) = 0 then exit;
    Read(Str[1], Length(Str));
    Posi := Pos(#0#0, Str);
    if Posi > 0 then
    begin
      if odd(Posi div 2) then fUniStreamType := ufUtf32le else fUniStreamType := ufUtf32be;
    end else
    begin
      Posi := Pos(#0, Str);
      if Posi > 0 then if odd(Posi) then fUniStreamType := ufUtf16be else fUniStreamType := ufUtf16le;
    end;
  end;
end;


function TUniStream.GetOffset: int64;
begin
  if not fHaveType then CheckFileType;
  if not HasBom then
  begin
    Result := 0;
    exit;
  end;
  case fUniStreamType of
    ufUtf8: Result := 3;
    ufUtf16be, ufUtf16le: Result := 2;
    ufUtf32be, ufUtf32le: Result := 4;
  end;
end;

function TUniStream.GetUTF8Text: AnsiString;
var
  PWC: PWideChar;
  PC: PChar;
  aPtr: PChar;
  UArr: TUArr;
begin
  if (not fHaveType) and (not fForceType) then CheckFileType;
  Position := 0;

  case fUniStreamtype of
    ufANSI:
      begin
        PC := Memory;
        Result := Copy(PC, 1, (Size));
      end;
    ufUtf8:
      begin
        PC := Memory;
        if fHasBom then
          Result := Copy(PC, 4, (Size - 3)) else
          Result := Copy(PC, 1, (Size));
      end;
    ufUtf16be, ufUtf16le:
      begin
        PWC := Memory;
        if fUniStreamType = ufUtf16be then WideSwapEndian(PWC,size);
        if fHasBom then
          Result := UTF16ToUTF8(Copy(PWC, 2, (Size - 1) div 2)) else
          Result := UTF16ToUTF8(Copy(PWC, 1, (Size) div 2))
      end;
    ufUtf32be, ufUtf32le:
      begin
        aPtr := Memory;
        if fHasBom then
        begin
          inc(aPtr, 4);
          SetLength(UArr, ((Size - 4) div 4));
          Move(aPtr^, UArr[0], Size - 4);
        end else
        begin
          SetLength(UArr, ((Size) div 4));
          Move(aPtr^, UArr[0], Size);
        end;
        if fUniStreamType = ufUtf32be then UASwapEndian(UArr);
        Result := UTF16ToUTF8(UCS4StringToWideString(UCS4String(UArr)));
        SetLength(UArr, 0);
      end;
  end;
end;

procedure TUniStream.SetUniStreamType(const AValue: TUniStreamTypes);
begin
  fUniStreamType := AValue;
  fHaveType := true;
  if AValue = ufANSI then fHasBOM := false;
end;

procedure TUniStream.SetUTF8Text(AString: AnsiString);
var
  WS: WideString;
  UArr: TUArr;
begin
  Position := 0;
  case fUniStreamType of
    ufANSI:
      begin
        Size := Length(AString);
        Write(PChar(AString)^, Length(AString));
      end;
    ufUtf8:
      begin
        Size := Length(AString) + GetOffset;
        if fHasBom then Write(PChar(UTF8BOM)^, GetOffset);
        if Size > GetOffset then Write(PChar(AString)^, Length(AString));
      end;
    ufUtf16be, ufUtf16le:
      begin
        WS := UTF8ToUTF16(AString);
        Size := Length(WS) * 2 + GetOffset;
        if fHasBom then
          if fUniStreamType = ufUtf16be then
            Write(PChar(UTF16BEBOM)^, GetOffset) else
            Write(PChar(UTF16LEBOM)^, GetOffset);
        if Size > GetOffset then
        begin
          if fUniStreamType = ufUtf16be then WideSwapEndian(@WS[1],size);
          Write(PWideChar(WS)^, Length(WS) * 2);
        end;
      end;
    ufUtf32be, ufUtf32le:
      begin
        UArr := TUArr(WideStringToUCS4String(UTF8ToUTF16(AString)));
        Size := (Length(UArr) - 1) * 4 + GetOffset;
        if fHasBom then
          if fUniStreamType = ufUtf32be then
            Write(PChar(UTF32BEBOM)^, GetOffset) else
            Write(PChar(UTF32LEBOM)^, GetOffset);
        if Size > GetOffset then
        begin
          if fUniStreamType = ufUtf32be then UASwapEndian(UArr);
          Write(UArr[0], (Length(UArr) - 1) * 4);
        end;
        SetLength(UArr, 0);
      end;
  end;
end;


constructor TUniStream.Create;
begin
  Reset;
end;

destructor TUniStream.Destroy;
begin
  inherited Destroy;
end;

procedure TUniStream.Reset;
begin
  fHaveType := false;
  fHasBOM := true;
  fForceType := false;
  if GetSystemEncoding = EncodingUTF8 then fUniStreamType := ufUTF8 else fUniStreamType := ufANSI;
  Position := 0;
end;

{ TCharEncStream }

function TCharEncStream.GetANSIEnc: string;
begin
  Result := fANSIEnc;
end;

procedure TCharEncStream.SetANSIEnc(const AValue: string);
begin
  fANSIEnc := AValue;
end;

function TCharEncStream.GetUTF8Text: AnsiString;
begin
  Result := inherited GetUTF8Text;
  if (UniStreamType = ufANSI) or ((UniStreamType = ufUtf8) and (not HasBom)) then
  begin
    if not ForceType then ANSIEnc := LConvencoding.GuessEncoding(Result);
    if ANSIEnc <> EncodingUTF8 then
    begin
      UniStreamType := ufANSI;
      Result := ConvertEncoding(Result, ANSIEnc, EncodingUTF8);
    end;
  end;
end;

procedure TCharEncStream.SetUTF8Text(AString: AnsiString);
begin
  if UniStreamType = ufANSI then
  begin
    AString := ConvertEncoding(AString, EncodingUTF8, ANSIEnc);
    HasBom := false;
    HaveType := True;
  end;
  inherited SetUTF8Text(AString);
end;

constructor TCharEncStream.Create;
begin
  inherited Create;
end;

destructor TCharEncStream.Destroy;
begin
  inherited Destroy;
end;

end.
