unit uGraphicUtils;

interface

uses
  Windows, Graphics, Classes;

type
  TTriVertex = packed record
    x: Longint;
    y: Longint;
    Red: Word;
    Green: Word;
    Blue: Word;
    Alpha: Word;
  end;

  TGradientOrientation = (goHorizontal, goVertical);

  TVertAlignment = (vaTop, vaCenter, vaBottom);
  THorzAlignment = (haLeft, haRight, haCenter);

  TPercent = 0..100;

  TScaleResult = record
    MaxValue: Double;
    MinValue: Double;
    StepCounts: Integer;
  end;

function GradientFill(DC: HDC; var Vertex: TTriVertex; NumVertex: ULONG;
  Mesh: Pointer; NumMesh, Mode: ULONG): BOOL; stdcall;
function CalcBrightColor(const AColor: TColor; const ALightPercent: Integer): TColor;
procedure FillGradientRect(ACanvas: TCanvas; ARect: TRect; AStartColor,
  AEndColor: TColor; AGradOrient: TGradientOrientation = goVertical);
procedure FillGradientRoundRect(ACanvas: TCanvas; ARect: TRect; AStartColor,
  AEndColor, AFrameColor: TColor; AGradOrient: TGradientOrientation = goVertical);

procedure DrawSimpleText(ACanvas: TCanvas; const ARect: TRect;
  const AText: string; const AFont: TFont = nil;
  const AHorzAlignment: THorzAlignment = haCenter;
  const AVertAlignment: TVertAlignment = vaCenter);

procedure DrawHighlightedText(ACanvas: TCanvas; const ARect: TRect;
  const AText, APattern: string; const ABaseFont, AHighlightFont: TFont;
  const AHighlightColor: TColor; const AHorzAlignment: THorzAlignment = haLeft);

function CalculateMaxTextWidth(const ACanvas: TCanvas; const AFont: TFont;
  arrString: TStringList): Integer;
function CalculateTextHeight(const ACanvas: TCanvas; const AFont: TFont;
  const AText: string = ''): Integer;
function CalculateTextWidth(const ACanvas: TCanvas; const AFont: TFont;
  const AText: string): Integer;
procedure ComplexLinearGradient(ACanvas: TCanvas; const ARect: TRect;
  AColors: array of TColor; APercentage: array of TPercent;
  AGradOrient: TGradientOrientation = goVertical);

procedure DrawGradientParallelogramm(ACanvas: TCanvas;
  const APoints: array of TPoint;
  AColors: array of TColor; APercentage: array of TPercent;
  const AFrameColor: TColor;
  AGradOrient: TGradientOrientation = goVertical);

procedure RichGradient(ACanvas: TCanvas; const ARect: TRect);

function CalculateScale(const AMin, AMax: Double; const AMaxStepCount:
  Integer = 10): TScaleResult;

function CreateRotatedBitmap(const ACanvas: TCanvas;
  const ARect: TRect; const AnAngle: Integer): TBitmap;

implementation

uses
  SysUtils, Types, Math;

function CreateRotatedBitmap(const ACanvas: TCanvas;
  const ARect: TRect; const AnAngle: Integer): TBitmap;
var
  i: Integer; // loop counter
  j: Integer; // loop counter
  vSin: Extended; // sine used in rotation
  vCos: Extended; // cosine used in rotation
  x1: Integer; // used in calculating new
  //   bitmap dimensions
  x2: Integer; // used in calculating new
  //     bitmap dimensions
  x3: Integer; // used in calculating new
  //     bitmap dimensions
  y1: Integer; // used in calculating new
  // bitmap dimensions
  y2: Integer; // used in calculating new
  // bitmap dimensions
  y3: Integer; // used in calculating new
  // bitmap dimensions
  vMinX: Integer; // used in calculating new
  // bitmap dimensions
  vMaxX: Integer; // used in calculating new
  // bitmap dimensions
  vMinY: Integer; // used in calculating new
  // bitmap dimensions
  vMaxY: Integer; // used in calculating new
  // bitmap dimensions
  vNewWidth: Integer; // width of new bitmap
  vNewHeight: Integer; // height of new bitmap
  vSourceX: Integer; // x pixel coord we are blitting
  // from the source  image
  vSourceY: Integer; // y pixel coord we are blitting
  // from the source image
  vRadians: Double;
  vHeight: Integer;
  vWidth: Integer;

  dx, dy: Integer;
begin
  vRadians := Pi * AnAngle / 180;

  vSin := Sin(vRadians);
  vCos := Cos(vRadians);

  vHeight := ARect.Bottom - ARect.Top;
  vWidth := ARect.Right - ARect.Left;

  // compute the size of the new bitmap being created
  x1 := Round(-vHeight * vSin);
  y1 := Round(vHeight * vCos);
  x2 := Round(vWidth * vCos - vHeight * vSin);
  y2 := Round(vHeight * vCos + vWidth * vSin);
  x3 := Round(vWidth * vCos);
  y3 := Round(vWidth * vSin);

  // figure out the max/min size of the new bitmap
  vMinX := Min(0, Min(x1, Min(x2, x3)));
  vMinY := Min(0, Min(y1, Min(y2, y3)));
  vMaxX := Max(x1, Max(x2, x3));
  vMaxY := Max(y1, Max(y2, y3));

  // set the new bitmap width/height
  vNewWidth := vMaxX - vMinX;
  vNewHeight := vMaxY - vMinY;

  // create a new bitmap based upon the new width/height of the
  // rotated bitmap
  Result := TBitmap.Create;
  Result.Width := vNewWidth;
  Result.Height := vNewHeight;

  dx := 0;
  dy := 0;
  if AnAngle = 90 then
  begin
    dy := -1;
  end
  else if AnAngle = 180 then
  begin
    dx := -1;
    dy := -1;
  end
  else if AnAngle = 270 then
  begin
    dx := -1;
  end;

  // loop through and translate each pixel to its new location.
  // this is using a standard rotation algorithm
  for i := 0 to vNewHeight do
  begin
    for j := 0 to vNewWidth do
    begin
      vSourceX := Round((j + vMinX) * vCos + (i + vMinY) * vSin);
      vSourceY := Round((i + vMinY) * vCos - (j + vMinX) * vSin);
      //       if (vSourceX >= 0) and (vSourceX <= vWidth)
      //         and (vSourceY >= 0) and (vSourceY <= vHeight)
      //       then
      BitBlt(Result.Canvas.Handle, j, i, 1, 1, ACanvas.Handle,
        vSourceX + ARect.Left + dx, vSourceY + ARect.Top + dy, SRCCOPY);
    end;
  end;
end;

function CalculateScale(const AMin, AMax: Double; const AMaxStepCount:
  Integer = 10): TScaleResult;
var
  vMin, vMax, vSum: Double;
  vKoef: Double;
  vLog2: Integer;
  vLog10: Integer;
  vTmpSteps: Integer;
  vRoundingMode: TFPURoundingMode;
begin
  if AMax < 0 then
    vMax := 0
  else
    vMax := AMax;
  if AMin > 0 then
    vMin := 0
  else
    vMin := AMin;

  vSum := vMax - vMin;

  vRoundingMode := GetRoundMode;

  try
    SetRoundMode(rmNearest);
    vLog10 := Trunc(Ln(vSum) / Ln(10));
    vKoef := 1 / Power(10, vLog10);

    vLog2 := Trunc(Ln(vSum * vKoef / AMaxStepCount) / Ln(2));
    vKoef := vKoef / Power(2, vLog2);

    SetRoundMode(rmUp);

    Result.StepCounts := 0;

    if vMax = 0 then
    begin
      Result.MaxValue := 0
    end
    else
    begin
      vTmpSteps := Round(vMax * vKoef);
      Result.MaxValue := vTmpSteps / vKoef;
      Result.StepCounts := vTmpSteps;
    end;

    SetRoundMode(rmDown);
    if vMin = 0 then
    begin
      Result.MinValue := 0
    end
    else
    begin
      vTmpSteps := Round(vMin * vKoef);
      Result.MinValue := vTmpSteps / vKoef;
      Result.StepCounts := Result.StepCounts - vTmpSteps;
    end;
  finally
    SetRoundMode(vRoundingMode);
  end;
end;

function DegreesToRadian(ADegree: Double): Double;
begin
  Result := ADegree / 180 * Pi;
end;

function GradientFill; external msimg32 name 'GradientFill';

procedure DrawHighlightedText(ACanvas: TCanvas; const ARect: TRect;
  const AText, APattern: string; const ABaseFont, AHighlightFont: TFont;
  const AHighlightColor: TColor; const AHorzAlignment: THorzAlignment = haLeft);
var
  vPos: Integer;
  vBottomRect: TRect;
  vRect: TRect;
  vPattern: string;
  vText: string;
  vPaintingText: string;
  vWidth: Integer;
  vLength: Integer;
  vPatternWidth: Integer;
  vOldBrushColor: TColor;
begin
  vPattern := Trim(APattern);
  vText := AText;
  vRect := ARect;
  vLength := Length(vPattern);

  vPos := Pos(AnsiUpperCase(vPattern), AnsiUpperCase(vText));
  if vPos > 0 then
  begin
    repeat
      vPaintingText := Copy(vText, 1, vPos - 1);
      vWidth := CalculateTextWidth(ACanvas, ABaseFont, vPaintingText);
      DrawSimpleText(ACanvas, vRect, vPaintingText, ABaseFont, AHorzAlignment);
      vRect.Left := vRect.Left + vWidth;

      vPaintingText := Copy(vText, vPos, vLength);
      vPatternWidth := CalculateTextWidth(ACanvas, AHighlightFont, vPaintingText);

      vBottomRect := vRect;
      vBottomRect.Right := vRect.Left + vPatternWidth;
      vOldBrushColor := ACanvas.Brush.Color;
      try
        ACanvas.Brush.Color := AHighlightColor;
        ACanvas.FillRect(vBottomRect);
      finally
        ACanvas.Brush.Color := vOldBrushColor;
      end;

      DrawSimpleText(ACanvas, vRect, vPaintingText, AHighlightFont, AHorzAlignment);
      vRect.Left := vBottomRect.Right;

      Delete(vText, 1, vPos + vLength - 1);

      vPos := Pos(AnsiUpperCase(vPattern), AnsiUpperCase(vText));
    until vPos = 0;
  end;

  DrawSimpleText(ACanvas, vRect, vText, ABaseFont, AHorzAlignment);
end;

procedure DrawSimpleText(ACanvas: TCanvas; const ARect: TRect;
  const AText: string; const AFont: TFont = nil;
  const AHorzAlignment: THorzAlignment = haCenter;
  const AVertAlignment: TVertAlignment = vaCenter);
var
  vFontRecall: TFontRecall;
  vBrushRecall: TBrushRecall;
  vDrawFormat: Cardinal;
  vRect: TRect;
begin
  if (AText = '') or not Assigned(AFont) then
    Exit;

  vFontRecall := TFontRecall.Create(ACanvas.Font);
  vBrushRecall := TBrushRecall.Create(ACanvas.Brush);

  ACanvas.Font.Assign(AFont);
  ACanvas.Font.Charset := DEFAULT_CHARSET;

  try
    ACanvas.Brush.Style := bsClear;

    vDrawFormat := DT_NOPREFIX or DT_WORDBREAK or DT_END_ELLIPSIS or
      DT_VCENTER;
    case AHorzAlignment of
      haLeft: vDrawFormat := vDrawFormat or DT_LEFT;
      haCenter: vDrawFormat := vDrawFormat or DT_CENTER;
      haRight: vDrawFormat := vDrawFormat or DT_RIGHT;
    end;
    vRect := ARect;
    DrawTextA(ACanvas.Handle, PAnsiChar(AText), Length(AText), vRect, vDrawFormat);
  finally
    vFontRecall.Free;
    vBrushRecall.Free;
  end;
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

procedure FillGradientRect(ACanvas: TCanvas; ARect: TRect; AStartColor,
  AEndColor: TColor; AGradOrient: TGradientOrientation = goVertical);
var
  vVertexes: array[0..1] of TTriVertex;
  vGradRect: TGradientRect;
begin
  vVertexes[0].x := ARect.Left;
  vVertexes[0].y := ARect.Top;
  vVertexes[0].Red := GetRValue(AStartColor) shl 8;
  vVertexes[0].Green := GetGValue(AStartColor) shl 8;
  vVertexes[0].Blue := GetBValue(AStartColor) shl 8;
  vVertexes[0].Alpha := 0;//Byte(DWORD(AStartColor) shr 24) shl 8;
  vVertexes[1].x := ARect.Right;
  vVertexes[1].y := ARect.Bottom;
  vVertexes[1].Red := GetRValue(AEndColor) shl 8;
  vVertexes[1].Green := GetGValue(AEndColor) shl 8;
  vVertexes[1].Blue := GetBValue(AEndColor) shl 8;
  vVertexes[1].Alpha := 0;//Byte(DWORD(AStartColor) shr 24) shl 8;

  vGradRect.UpperLeft := 0;
  vGradRect.LowerRight := 1;

  if AGradOrient = goHorizontal then
    GradientFill(ACanvas.Handle, vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_H)
  else
    GradientFill(ACanvas.Handle, vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_V);
end;

procedure FillGradientRoundRect(ACanvas: TCanvas; ARect: TRect; AStartColor,
  AEndColor, AFrameColor: TColor; AGradOrient: TGradientOrientation = goVertical);
var
  vClipRgn: HRGN;
  vBrushRecall: TBrushRecall;
begin
  vBrushRecall := TBrushRecall.Create(ACanvas.Brush);

  try
    ACanvas.Brush.Color := AFrameColor;

    vClipRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
    SelectClipRgn(ACanvas.Handle, vClipRgn);

    try
      FillGradientRect(ACanvas, ARect, AStartColor, AEndColor, AGradOrient);
      FrameRgn(ACanvas.Handle, vClipRgn, ACanvas.Brush.Handle, 1, 1);
    finally
      SelectClipRgn(ACanvas.Handle, 0);
      DeleteObject(vClipRgn);
    end;

  finally
    vBrushRecall.Free;
  end;
end;

procedure RichGradient(ACanvas: TCanvas; const ARect: TRect);
type
  TGradientTriangle = record
    v1: Integer;
    v2: Integer;
    v3: Integer;
  end;
var
  grTri: array[0..3] of TGradientTriangle;
  vVertexes: array[0..4] of TTriVertex;
  vPoint: TPoint;
const
  c1 = clBlack;
  c2 = clRed;
  c3 = $00D56A00;
  c4 = clGreen;
  c5 = clWhite;
begin
  vPoint.X := ARect.Left + (ARect.Right - ARect.Left) div 2;
  vPoint.Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2;

  vVertexes[0].x := ARect.Left;
  vVertexes[0].y := ARect.Top;
  vVertexes[0].Red := GetRValue(c1) shl 8;
  vVertexes[0].Green := GetGValue(c1) shl 8;
  vVertexes[0].Blue := GetBValue(c1) shl 8;
  vVertexes[0].Alpha := 0;
  vVertexes[1].x := ARect.Right;
  vVertexes[1].y := ARect.Bottom;
  vVertexes[1].Red := GetRValue(c2) shl 8;
  vVertexes[1].Green := GetGValue(c2) shl 8;
  vVertexes[1].Blue := GetBValue(c2) shl 8;
  vVertexes[1].Alpha := 0;
  vVertexes[2].x := ARect.Left;
  vVertexes[2].y := ARect.Bottom;
  vVertexes[2].Red := GetRValue(c3) shl 8;
  vVertexes[2].Green := GetGValue(c3) shl 8;
  vVertexes[2].Blue := GetBValue(c3) shl 8;
  vVertexes[2].Alpha := 0;
  vVertexes[3].x := ARect.Right;
  vVertexes[3].y := ARect.Top;
  vVertexes[3].Red := GetRValue(c4) shl 8;
  vVertexes[3].Green := GetGValue(c4) shl 8;
  vVertexes[3].Blue := GetBValue(c4) shl 8;
  vVertexes[3].Alpha := 0;
  vVertexes[4].x := vPoint.X;
  vVertexes[4].y := vPoint.Y;
  vVertexes[4].Red := GetRValue(c5) shl 8;
  vVertexes[4].Green := GetGValue(c5) shl 8;
  vVertexes[4].Blue := GetBValue(c5) shl 8;
  vVertexes[4].Alpha := 0;

  grTri[0].v1 := 0;
  grTri[0].v2 := 2;
  grTri[0].v3 := 4;

  grTri[1].v1 := 0;
  grTri[1].v2 := 3;
  grTri[1].v3 := 4;

  grTri[2].v1 := 3;
  grTri[2].v2 := 1;
  grTri[2].v3 := 4;

  grTri[3].v1 := 1;
  grTri[3].v2 := 2;
  grTri[3].v3 := 4;

  GradientFill(ACanvas.Handle, vVertexes[0], 5, @grTri, 4, GRADIENT_FILL_TRIANGLE);
end;

procedure ComplexLinearGradient(ACanvas: TCanvas; const ARect: TRect;
  AColors: array of TColor; APercentage: array of TPercent;
  AGradOrient: TGradientOrientation = goVertical);
var
  vRect: TRect;
  vStartColor: TColor;
  vEndColor: TColor;
  vStartPos: Integer;
  vEndPos: Integer;
  i: Integer;

  function CalcPercentageRect(ARect: TRect; AGradOrient: TGradientOrientation;
    AStartPos, AEndPos: Integer): TRect;
  begin
    Result := ARect;
    if AGradOrient = goHorizontal then
    begin
      Result.Left := ARect.Left + (ARect.Right - ARect.Left) * AStartPos div 100;
      Result.Right := ARect.Left + (ARect.Right - ARect.Left) * AEndPos div 100;
    end
    else if AGradOrient = goVertical then
    begin
      Result.Top := ARect.Top + (ARect.Bottom - ARect.Top) * AStartPos div 100;
      Result.Bottom := ARect.Top + (ARect.Bottom - ARect.Top) * AEndPos div 100;
    end;
  end;

begin
  if High(AColors) < 0 then
    Exit;

  vRect := ARect;
  i := Low(AColors);
  vStartColor := AColors[i];
  vStartPos := 0;
  vEndPos := 0;
  Inc(i);
  while (i <= High(AColors)) and (vEndPos < 100) do
  begin
    if i > High(APercentage) then
      vEndPos := 100
    else
      vEndPos := APercentage[i];

    if vEndPos < vStartPos then
      vEndPos := vStartPos;

    vEndColor := AColors[i];

    vRect := CalcPercentageRect(ARect, AGradOrient, vStartPos, vEndPos);
    FillGradientRect(ACanvas, vRect, vStartColor, vEndColor, AGradOrient);

    vStartColor := vEndColor;
    vStartPos := vEndPos;
    Inc(i);
  end;

  if vEndPos < 100 then
  begin
    vEndPos := 100;
    vEndColor := vStartColor;
    vRect := CalcPercentageRect(ARect, AGradOrient, vStartPos, vEndPos);
    FillGradientRect(ACanvas, vRect, vStartColor, vEndColor, AGradOrient);
  end;
end;

procedure DrawGradientParallelogramm(ACanvas: TCanvas;
  const APoints: array of TPoint;
  AColors: array of TColor; APercentage: array of TPercent;
  const AFrameColor: TColor;
  AGradOrient: TGradientOrientation = goVertical);
var
  vRgn: HRGN;
  vRect: TRect;
  vBrushRecall: TBrushRecall;
begin
  vRgn := CreatePolygonRgn(APoints, SizeOf(APoints) div SizeOf(TPoint), WINDING);
  GetRgnBox(vRgn, vRect);

  vBrushRecall := TBrushRecall.Create(ACanvas.Brush);

  ACanvas.Brush.Color := AFrameColor;
  SelectClipRgn(ACanvas.Handle, vRgn);

  try
    ComplexLinearGradient(ACanvas, vRect, AColors, APercentage, AGradOrient);
    FrameRgn(ACanvas.Handle, vRgn, ACanvas.Brush.Handle, 1, 1);
  finally
    SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(vRgn);
    vBrushRecall.Free;
  end;
end;

{ procedure DrawGradientColumn(ACanvas: TCanvas; const ARect: TRect;
  AColor, AFrameColor: TColor; ARadius: TPercent);
var
  vRect: TRect;
  vRadius: Integer;
  vTopEllipticRgn: HRGN;
  vBottomEllipticRgn: HRGN;
  vComplexRgn: HRGN;
  vBrushRecall: TBrushRecall;
begin

 vRadius := (ARect.Right - ARect.Left) * ARadius div 200;
  vRect := ARect;
  vRect.Bottom := ARect.Bottom + vRadius;
  vComplexRgn := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right,
    ARect.Bottom);
  vTopEllipticRgn := CreateEllipticRgn(ARect.Left, ARect.Top - vRadius,
    ARect.Right, ARect.Top + vRadius);
  vBottomEllipticRgn := CreateEllipticRgn(ARect.Left, ARect.Bottom - vRadius,
    ARect.Right, ARect.Bottom + vRadius);

  vBrushRecall := TBrushRecall.Create(ACanvas.Brush);

  try
    SelectClipRgn(ACanvas.Handle, vTopEllipticRgn);

    ComplexLinearGradient(ACanvas, Rect(ARect.Left, ARect.Top - vRadius, ARect.Right,
      ARect.Top + vRadius), [AFrameColor, CalcBrightColor(AFrameColor, 20), AFrameColor],
      [0, 50, 100], goVertical);

    ACanvas.Brush.Color := AFrameColor;
    FrameRgn(ACanvas.Handle, vTopEllipticRgn, ACanvas.Brush.Handle, 1, 1);

    CombineRgn(vComplexRgn, vComplexRgn, vBottomEllipticRgn, RGN_OR);
    CombineRgn(vComplexRgn, vComplexRgn, vTopEllipticRgn, RGN_DIFF);

    SelectClipRgn(ACanvas.Handle, 0);
    SelectClipRgn(ACanvas.Handle, vComplexRgn);

    ComplexLinearGradient(ACanvas, Rect(ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom + vRadius), [CalcBrightColor(AColor, -8),
      CalcBrightColor(AColor, 15), CalcBrightColor(AColor, -8)],
      [0, 60, 100], goHorizontal);

    FrameRgn(ACanvas.Handle, vComplexRgn, ACanvas.Brush.Handle, 1, 1);
  finally
    SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(vComplexRgn);
    DeleteObject(vBottomEllipticRgn);
    DeleteObject(vTopEllipticRgn);
    vBrushRecall.Free;
  end;
end;   }

function CalculateMaxTextWidth(const ACanvas: TCanvas; const AFont: TFont;
  arrString: TStringList): Integer;
var
  i: Integer;
  vTextWidth: Integer;
  vFontRecall: TFontRecall;
begin
  Result := 0;

  vFontRecall := TFontRecall.Create(ACanvas.Font);

  try
    ACanvas.Font.Assign(AFont);
    ACanvas.Font.Charset := DEFAULT_CHARSET;
    for i := 0 to arrString.Count - 1 do
    begin
      vTextWidth := ACanvas.TextWidth(arrString.Strings[i]);
      if vTextWidth > Result then
        Result := vTextWidth;
    end;
  finally
    vFontRecall.Free;
  end;
end;

function CalculateTextHeight(const ACanvas: TCanvas; const AFont: TFont;
  const AText: string = ''): Integer;
var
  vFontRecall: TFontRecall;
  vText: string;
begin
  if AText = '' then
    vText := 'fg'
  else
    vText := AText;

  vFontRecall := TFontRecall.Create(ACanvas.Font);
  try
    ACanvas.Font.Assign(AFont);
    ACanvas.Font.Charset := DEFAULT_CHARSET;
    Result := ACanvas.TextHeight(vText);
  finally
    vFontRecall.Free;
  end;
end;

function CalculateTextWidth(const ACanvas: TCanvas; const AFont: TFont;
  const AText: string): Integer;
var
  vFontRecall: TFontRecall;
begin
  vFontRecall := TFontRecall.Create(ACanvas.Font);

  try
    ACanvas.Font.Assign(AFont);
    ACanvas.Font.Charset := DEFAULT_CHARSET;
    Result := ACanvas.TextWidth(AText);
  finally
    vFontRecall.Free;
  end;
end;

end.

