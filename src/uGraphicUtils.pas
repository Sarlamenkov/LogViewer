unit uGraphicUtils;

interface

uses
  Windows, Graphics, Classes;

function CalcBrightColor(const AColor: TColor; const ALightPercent: Integer): TColor;


implementation

uses
  SysUtils;

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

