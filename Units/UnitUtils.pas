unit UnitUtils;

interface uses System.Classes, System.SysUtils,  WinApi.Windows, Vcl.Graphics;

const
  InternalFontName: string = 'Fontin SmallCaps';

type
TDropDrawFlags = set of (ddfCenter);

function SplitString(const S: string; const Delimiter: string; const AllowEmpty: boolean = false; const NilOnEmpty: boolean = false): TStrings;
function TrimString(Str: string; Symbol: char): string;
procedure DrawOpacityImage(Where: TCanvas; X, Y: Integer; Image: TBitmap; Opacity: Byte);
function DesireBackgroundColor(const Color: TColor): TColor;
function DrawDrop(const Where: TCanvas; const CanvasSize: TSize; const Text: string; const FontSize: integer; const TextColor, BorderColor, BackgroundColor: TColor; const AlphaText, AlphaBorder, AlphaBackground: Byte; Flags: TDropDrawFlags = []): TRect;
implementation

function SplitString(const S: string; const Delimiter: string; const AllowEmpty: boolean = false; const NilOnEmpty: boolean = false): TStrings;
var
  parts: TArray<string>;
  part: string;
begin
  Result := TStringList.Create();
  parts := S.Split([Delimiter]);
  for part in parts do
  begin
    if AllowEmpty or (Trim(part) <> '') then
    begin
      Result.Add(Trim(part));
    end;
  end;

  if not NilOnEmpty then
  begin
    exit;
  end;

  if Result.Count = 0 then
  begin
    Result.Free();
    Result := nil;
  end;
end;

function TrimString(Str: string; Symbol: char): string;
begin
  Result := Str;
  if Str = '' then exit;
  while (length(Str) > 0) and (AnsiUpperCase(Str[1]) = AnsiUpperCase(Symbol)) do
  begin
    Delete(Str, 1, 1);
  end;

  while (length(Str) > 0) and (AnsiUpperCase(Str[length(Str)]) = AnsiUpperCase(Symbol)) do
  begin
    Delete(Str, length(Str), 1);
  end;

  Result := Str;
end;

procedure DrawOpacityImage(Where: TCanvas; X, Y: Integer; Image: TBitmap; Opacity: Byte);
var
  I, J: Integer;
  Pixels: PRGBQuad;
  ColorRgb: Integer;
  ColorR, ColorG, ColorB: Byte;
begin
  ColorRgb :=  ColorToRGB(Image.TransparentColor);
  ColorR := GetRValue(ColorRgb);
  ColorG := GetGValue(ColorRgb);
  ColorB := GetBValue(ColorRgb);

  for I := 0 to Image.Height - 1 do
  begin
    Pixels := PRGBQuad(Image.ScanLine[I]);
    for J := 0 to Image.Width - 1 do
    begin
      with Pixels^ do
      begin
        if (rgbRed = ColorR) and (rgbGreen = ColorG) and (rgbBlue = ColorB) then
        begin
          rgbReserved := 0
        end else begin
          rgbReserved := Opacity;
        end;
        rgbRed := (rgbRed * rgbReserved) div $ff;
        rgbGreen := (rgbGreen * rgbReserved) div $ff;
        rgbBlue := (rgbBlue * rgbReserved) div $ff;
      end;
      Inc(Pixels);
    end;
  end;

  Where.Draw(X, Y, Image, 255);
end;

function DesireBackgroundColor(const Color: TColor): TColor;
var
  i: integer;
begin
  i := GetRValue(ColorToRGB(Color)) + GetGValue(ColorToRGB(Color)) + GetBValue(ColorToRGB(Color));
  if i div 3 > 128 then
  begin
    Result := clBlack;
  end else begin
    result := clWhite;
  end;
end;

function DrawDrop(const Where: TCanvas; const CanvasSize: TSize; const Text: string; const FontSize: integer; const TextColor, BorderColor, BackgroundColor: TColor; const AlphaText, AlphaBorder, AlphaBackground: Byte; Flags: TDropDrawFlags = []): TRect;
var
  tb: TBitmap;

  {$IFDEF 3STEPDRAW}
  procedure CleanBitmap();
  begin
    tb.Canvas.Brush.Color := tb.TransparentColor;
    tb.Canvas.FillRect(TRect.Create(0, 0, tb.Width, tb.Height));
  end;

  function CreateTransparentColor(const Color: TColor): TColor;
  var
    r, g, b: Byte;
  begin
    r := GetRValue(ColorToRGB(Color));
    g := GetGValue(ColorToRGB(Color));
    b := GetBValue(ColorToRGB(Color));
    Result := RGB(
      r + 120,
      g + 60,
      b + 30
    );
  end;
  {$ENDIF}
var
  c: TCanvas;
  sz: TSize;
begin
  tb := TBitmap.Create();
  tb.PixelFormat := pf32Bit;
  tb.TransparentMode := tmFixed;
  c := tb.Canvas;
  c.Font.Size := FontSize;
  c.Font.Name := InternalFontName;
  sz := c.TextExtent(Text);

  tb.SetSize(sz.cx + 10, sz.cy + 10);

  if ddfCenter in Flags then
  begin
    Result.Left := CanvasSize.cx div 2 - tb.Width div 2;
    Result.Top := CanvasSize.cy div 2 - tb.Height div 2;
  end else begin
    Result.Left := CanvasSize.cx;
    Result.Top := CanvasSize.cy;
  end;

  Result.Width := tb.Width;
  Result.Height := tb.Height;

  {$IFDEF 3STEPDRAW}
  // Background
  tb.TransparentColor := CreateTransparentColor(BackgroundColor);
  c.Brush.Color := BackgroundColor;
  c.FillRect(TRect.Create(0, 0, tb.Width, tb.Height));
  DrawOpacityImage(Where, Result.Left, Result.Top, tb, AlphaText);

  // Border
  tb.TransparentColor := CreateTransparentColor(BorderColor);
  CleanBitmap();
  c.Brush.Color := tb.TransparentColor;
  c.Pen.Color := BorderColor;
  c.Rectangle(0, 0, tb.Width, tb.Height);
  DrawOpacityImage(Where, Result.Left, Result.Top, tb, AlphaBorder);

  // Text
  tb.TransparentColor := CreateTransparentColor(TextColor);
  CleanBitmap();
  c.Pen.Color := TextColor;
  c.Font.Color := TextColor;
  c.TextOut(5, 5, Text);
  Where.Draw(0, tb.Height - 15, tb);
  DrawOpacityImage(Where, Result.Left, Result.Top, tb, AlphaText);
  {$ENDIF}
  {$IFNDEF 3STEPDRAW}
  c.Brush.Color := BackgroundColor;
  c.Pen.Color := BorderColor;
  c.Font.Color := TextColor;

  c.Rectangle(0, 0, tb.Width, tb.Height);
  c.TextOut(5, 5, Text);
  DrawOpacityImage(Where, Result.Left, Result.Top, tb, AlphaText);
  {$ENDIF}

  tb.Free();
end;
end.

