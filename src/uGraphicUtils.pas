unit uGraphicUtils;

interface

uses
  Graphics, Classes;

function RGB(r, g, b: longint) : LongWord;
function CalcBrightColor(const AColor: TColor; const ALightPercent: Integer): TColor;


implementation

uses
  SysUtils;

function GetBValue(rgb: longint) : BYTE;
begin
  GetBValue := BYTE(rgb shr 16);
end;

function GetGValue(rgb: longint) : BYTE;
begin
  GetGValue := BYTE((WORD(rgb)) shr 8);
end;

function GetRValue(rgb: longint) : BYTE;
begin
  GetRValue := BYTE(rgb);
end;

function RGB(r, g, b: longint) : LongWord;
begin
  RGB := LongWord(((WORD(BYTE(r))) or ((WORD(WORD(g))) shl 8)) or ((WORD(BYTE(b))) shl 16));
end;

function CalcBrightColor(const AColor: TColor; const ALightPercent: Integer): TColor;
var
  vRed, vGreen, vBlue: Integer;
begin
  vRed := GetRValue(AColor);
  vGreen := GetGValue(AColor);
  vBlue := GetBValue(AColor);
  if ALightPercent > 0 then
  begin
    vRed := vRed + Trunc((255 - vRed) * (ALightPercent / 100));
    vGreen := vGreen + Trunc((255 - vGreen) * (ALightPercent / 100));
    vBlue := vBlue + Trunc((255 - vBlue) * (ALightPercent / 100));
  end
  else
  begin
    vRed := vRed + Trunc(vRed * (ALightPercent / 100));
    vGreen := vGreen + Trunc(vGreen * (ALightPercent / 100));
    vBlue := vBlue + Trunc(vBlue * (ALightPercent / 100));
  end;
  Result := TColor(RGB(vRed, vGreen, vBlue));
end;

end.

