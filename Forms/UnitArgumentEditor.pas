unit UnitArgumentEditor;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFilter,
  Vcl.StdCtrls, UnitWinUtils, Vcl.ComCtrls, System.Math, Vcl.ExtCtrls, UnitUtils;

type
TfrmArgumentEditor = class(TForm)
  gbIntValue: TGroupBox;
  bOK: TButton;
  bCancel: TButton;
  tIntEditor: TEdit;
  lblIntEditor: TLabel;
  udIntEditor: TUpDown;
  gbListValue: TGroupBox;
  lblListEditor: TLabel;
  cbListEditor: TComboBox;
  gbVariadic: TGroupBox;
  lbVariadicEditor: TListBox;
  lblVariadicEditor: TLabel;
  gbColor: TGroupBox;
  tColorRed: TEdit;
  tColorBlue: TEdit;
  tColorGreen: TEdit;
  tColorAlpha: TEdit;
  cdColorSelector: TColorDialog;
  lblColorRed: TLabel;
  lblColorBlue: TLabel;
  lblColorGreen: TLabel;
  lblColorAlpha: TLabel;
  iColorDrawer: TImage;
  bclColorSelector: TBevel;
  gbSockets: TGroupBox;
  tSocketsValue: TEdit;
  lblSocketsValue: TLabel;
  bSelectColor: TButton;
  procedure FormCreate(Sender: TObject);
  procedure bOKClick(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure ColorChange(Sender: TObject);
  procedure bSelectColorClick(Sender: TObject);
private
  FArgument: TActionArgument;
  FFloor: TBitmap;
  FColorText: string;
  FGroupBoxes: array of TGroupBox;
  procedure DrawColor();
public
  function Edit(const Argument: TActionArgument; const Value: TStrings): TStrings;
end;

var
  frmArgumentEditor: TfrmArgumentEditor;

implementation
{$R *.dfm}

procedure TfrmArgumentEditor.bOKClick(Sender: TObject);
const
  ErrTitle: string = 'Error';
const
  AllowedChars: array[0..5] of string = ('R', 'G', 'B', 'A', 'D', 'W');
  function IsCharAllowed(const V: string): boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := Low(AllowedChars) to High(AllowedChars) do
    begin
      if AllowedChars[i] = V then
      begin
        Result := true;
      end;
    end;
  end;
var
  iTemp: integer;
  i: Integer;
  s: string;
begin
  case FArgument.Kind of
    akInt: begin
      iTemp := StrToIntDef(tIntEditor.Text, -1);
      if (FArgument.Minimum <> -1) and (FArgument.Maximum <> -1) and ((iTemp < FArgument.Minimum) or (iTemp > FArgument.Maximum)) then
      begin
        wuShowBalloon(tIntEditor.Handle, ErrTitle, Format('Value must be between "%d" and "%d"', [FArgument.Minimum, FArgument.Maximum]), biError);
        exit;
      end;

      if (FArgument.Minimum <> -1) and (iTemp < FArgument.Minimum) then
      begin
        wuShowBalloon(tIntEditor.Handle, ErrTitle, Format('Value must be greater than "%d"', [FArgument.Minimum]), biError);
        exit;
      end;

      if (FArgument.Maximum <> -1) and (iTemp > FArgument.Maximum) then
      begin
        wuShowBalloon(tIntEditor.Handle, ErrTitle, Format('Value must be less than "%d"', [FArgument.Maximum]), biError);
        exit;
      end;
    end;
    akVariadic: begin
      if lbVariadicEditor.Items.Count = 0 then
      begin
        MessageBox(Handle, PChar(Format('%s cannot be empty', [FArgument.Name])), PChar(ErrTitle), MB_ICONERROR);
        exit;
      end;
    end;
    akSockets: begin
      for i := 1 to Length(tSocketsValue.Text) do
      begin
        s := Copy(tSocketsValue.Text, i, 1);
        if not (IsCharAllowed(s) or ((StrToIntDef(s, -1) <> -1) and (i = 1))) then
        begin
          wuShowBalloon(tSocketsValue.Handle, 'Error', 'Socket group can contain only numbers and socket colors (R, G, B, A, D, W), example: 2RGG', biError);
          exit;
        end;
      end;

      i := StrToIntDef(Copy(tSocketsValue.Text, 0, 1), -1);
      if (i <> -1) and (i < 0) and (i > 6) then
      begin
        wuShowBalloon(tSocketsValue.Handle, 'Error', 'Socket count in group must be between "1" and "6"', biError);
        exit;
      end;

      if Length(tSocketsValue.Text) > 6 + Ord(i <> -1) then
      begin
        wuShowBalloon(tSocketsValue.Handle, 'Error', 'Socket group mask is too big', biError);
        exit;
      end;
    end;
  end;

  ModalResult := mrOk;
  CloseModal();
end;

function TfrmArgumentEditor.Edit(const Argument: TActionArgument; const Value: TStrings): TStrings;
const
 Texts: array[0..2] of string = ('Divine Orb', 'Chaos Orb', 'Mirror of Kalandra');
 function GetArgDef(const Index: integer; Default: string = '0'): string;
 begin
   if Index >= Value.Count then
   begin
     Result := Default;
     exit;
   end;

   Result := Value[Index];
 end;
var
  i, iBaseSize: integer;
begin
  Result := nil;

  Caption := Argument.Name;
  FArgument := Argument;

  iBaseSize := gbVariadic.Top + (ClientHeight - (bOK.Top + bOK.Height)) * 2 + bOK.Height;

  for i := Low(FGroupBoxes) to High(FGroupBoxes) do
  begin
    FGroupBoxes[i].Visible := i = Ord(Argument.Kind);
    if FGroupBoxes[i].Visible then
    begin
      ClientHeight := iBaseSize + FGroupBoxes[i].Height;
      FGroupBoxes[i].Top := gbVariadic.Top;
    end;
    FGroupBoxes[i].Caption := '';
  end;

  case Argument.Kind of
    akInt: begin
      udIntEditor.Min := Max(0, Argument.Minimum);
      i := Argument.Maximum;
      if Argument.Maximum = -1 then
      begin
        i := 65535;
      end;
      udIntEditor.Max := i;
      if Value.Count > 0 then
      begin
        udIntEditor.Position := StrToIntDef(Value[0], 0);
      end else begin
        udIntEditor.Position := udIntEditor.Min;
      end;
      lblIntEditor.Caption := Format('Enter value for %s:', [Argument.Name]);
    end;
    akList: begin
      lblListEditor.Caption := Format('Select value for %s:', [Argument.Name]);
      cbListEditor.Items.AddStrings(Argument.GetDisplayNames());

      i := -1;
      if Value.Count > 0 then
      begin
        i := cbListEditor.Items.IndexOf(Argument.GetNameByValue(Value[0])); // Argument.Values.IndexOf(Value[0]);
      end;

      cbListEditor.ItemIndex := Max(0, i);
    end;
    akColor: begin
      FFloor := TBitmap.Create();
      FFloor.LoadFromResourceName(HInstance, 'FLOOR');
      FFloor.SetSize(iColorDrawer.Width, iColorDrawer.Height);

      tColorRed.Text := GetArgDef(0);
      tColorGreen.Text := GetArgDef(1);
      tColorBlue.Text := GetArgDef(2);
      tColorAlpha.Text := GetArgDef(3, '');

      FColorText := Texts[RandomRange(0, 2)];

      DrawColor();
    end;
    akSockets: begin
      lblSocketsValue.Caption := Format('Enter %s:', [Argument.Name]);
      tSocketsValue.Text := Value[0];
    end;
    akVariadic: begin
      lblVariadicEditor.Caption := Format('Values for %s:', [Argument.Name]);
      lbVariadicEditor.Items.AddStrings(Value);
    end;
  end;

  if ShowModal() = mrOk then
  begin
    Result := TStringList.Create();
    case Argument.Kind of
      akInt: begin
        Result.Add(tIntEditor.Text);
      end;
      akList: begin
        Result.Add(Argument.GetValueByName(cbListEditor.Text));
      end;
      akVariadic: begin
        Result.AddStrings(lbVariadicEditor.Items);
      end;
      akColor: begin
        Result.Add(tColorRed.Text);
        Result.Add(tColorGreen.Text);
        Result.Add(tColorBlue.Text);

        if tColorAlpha.Text <> '' then
        begin
          Result.Add(tColorAlpha.Text);
        end;
      end;
      akSockets: begin
        Result.Add(tSocketsValue.Text);
      end;
    end;
  end;

end;
procedure TfrmArgumentEditor.FormCreate(Sender: TObject);
begin
  SetLength(FGroupBoxes, 5);
  FGroupBoxes[0] := gbIntValue;
  FGroupBoxes[1] := gbListValue;
  FGroupBoxes[2] := gbVariadic;
  FGroupBoxes[3] := gbColor;
  FGroupBoxes[4] := gbSockets;
end;

procedure TfrmArgumentEditor.DrawColor();
var
  c: TCanvas;
  color: TColor;
begin
  c := iColorDrawer.Canvas;
  c.Draw(0, 0, FFloor);

  color := RGB(
    StrToIntDef(tColorRed.Text, 0),
    StrToIntDef(tColorGreen.Text, 0),
    StrToIntDef(tColorBlue.Text, 0)
  );

  DrawDrop(
    iColorDrawer.Canvas,
    TSize.Create(iColorDrawer.Width, iColorDrawer.Height),
    FColorText,
    16,
    color,
    color,
    DesireBackgroundColor(color),
    StrToIntDef(tColorAlpha.Text, 240),
    StrToIntDef(tColorAlpha.Text, 240),
    240,
    [ddfCenter]
  );
end;

procedure TfrmArgumentEditor.FormDestroy(Sender: TObject);
begin
  if FFloor <> nil then
  begin
    FFloor.Free();
  end;
end;

procedure TfrmArgumentEditor.bSelectColorClick(Sender: TObject);
begin
  cdColorSelector.Color := RGB(
    StrToIntDef(tColorRed.Text, 0),
    StrToIntDef(tColorGreen.Text, 0),
    StrToIntDef(tColorBlue.Text, 0)
  );

  //cdColorSelector.CustomColors.

  if cdColorSelector.Execute(Handle) then
  begin
    tColorRed.Text := IntToStr(GetRValue(cdColorSelector.Color));
    tColorGreen.Text := IntToStr(GetGValue(cdColorSelector.Color));
    tColorBlue.Text := IntToStr(GetBValue(cdColorSelector.Color));
  end;
end;

procedure TfrmArgumentEditor.ColorChange(Sender: TObject);
var
  v: integer;
begin
  v := StrToIntDef(TEdit(Sender).Text, 0);

  if (v < 0) or (v > 255) then
  begin
    wuShowBalloon(TEdit(Sender).Handle, 'Error', 'Value must be between "0" and "255"', biError);
  end;

  DrawColor();
end;

end.

