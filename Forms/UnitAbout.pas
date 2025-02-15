unit UnitAbout;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, UnitUtils, ShellApi, System.Math;

type
TAboutItem = record
  X, Y: integer;
  Size: integer;
  Text: string;
  Color: TColor;
  Locked: boolean;
  Rect: TRect;
  procedure Update(X, Y: integer; Color: TColor; Text: string; Locked: boolean = false);
end;

TfrmAbout = class(TForm)
  iLogo: TImage;
  bvlHeading: TBevel;
  bClose: TButton;
  llText: TLinkLabel;
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure iLogoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure llTextLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
private
  FFloor: TBitmap;
  FItems: array of TAboutItem;
  procedure DrawLogo();
end;

var
  frmAbout: TfrmAbout;

implementation
{$R *.dfm}

procedure TAboutItem.Update(X, Y: integer; Color: TColor; Text: string; Locked: boolean = false);
begin
  self.X := X;
  self.Y := Y;
  self.Size := 16;
  self.Color := Color;
  self.Text := Text;
  self.Locked := Locked;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
const
  Names: array[0..2] of string = ('Mirror of Kalandra', 'Divine Orb', 'Exalted Orb');
begin
  FFloor := TBitmap.Create();
  FFloor.LoadFromResourceName(HInstance, 'FLOOR');

  SetLength(FItems, 5);
  FItems[0].Update(15, 5, clWhite, 'Path of Exile Filter Editor', true);
  FItems[1].Update(38, 41, clRed, 'Chaos Orb');
  FItems[2].Update(150, 40, clFuchsia, Names[RandomRange(0, 2)]);
  FItems[3].Update(52, 76, clGray, 'Scroll of Wisdom');
  FItems[3].Size := 14;
  FItems[4].Update(238, 76, clFuchsia, Names[RandomRange(0, 2)]);

  DrawLogo();
end;

procedure TfrmAbout.FormDestroy(Sender: TObject);
begin
  FFloor.Free();
end;

procedure TfrmAbout.iLogoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, remove: integer;
begin
  //OutputDebugString(PChar(Format('Click: X=%d; Y=%d;', [X, Y])));
  remove := -1;
  for i := Low(FItems) to High(FItems) do
  begin
    if FItems[i].Rect.Contains(TPoint.Create(X, Y)) then
    begin
      if FItems[i].Locked then
      begin
        Close();
        exit;
      end;

      remove := i;
      break;
    end;
  end;

  if remove = -1 then
  begin
    exit;
  end;

  for i := remove to High(FItems) - 1 do
  begin
    FItems[i] := FItems[i + 1];
  end;
  SetLength(FItems, Length(FItems) - 1);

  DrawLogo();
end;

procedure TfrmAbout.llTextLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  if LinkType <> sltURL then exit;
  ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_SHOWDEFAULT);
end;

procedure TfrmAbout.DrawLogo();
var
  i: integer;
begin
  iLogo.Canvas.Draw(0, 0, FFloor);
  for i := Low(FItems) to High(FItems) do
  begin
    FItems[i].Rect := DrawDrop(
      iLogo.Canvas,
      TSize.Create(FItems[i].X, FItems[i].Y),
      FItems[i].Text,
      FItems[i].Size,
      FItems[i].Color,
      FItems[i].Color,
      RGB(4, 4, 4),
      220,
      220,
      220
    );
  end;

end;

end.

