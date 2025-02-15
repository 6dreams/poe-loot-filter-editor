unit UnitItemEditor;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFilter,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.RegularExpressions, UnitArgumentEditor;

type

TfrmItemEditor = class(TForm)
  cbType: TComboBox;
  lblType: TLabel;
  gbEditor: TGroupBox;
  bOK: TButton;
  bCancel: TButton;
  llText: TLinkLabel;
  procedure cbTypeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  procedure FormDestroy(Sender: TObject);
  procedure cbTypeClick(Sender: TObject);
  procedure bOKClick(Sender: TObject);
  procedure llTextLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
private
  FActions: TActions;
  FItem: TFilterItem;
  FArguments: array of TStrings;
  procedure InsertTypes();
  procedure UpdateLinkText();
  function GetActionById(const Index: integer): TAction;
public
  function Edit(const Actions: TActions; const Item: TFilterItem): TFilterItem;
  function Add(const Actions: TActions): TFilterItem;
end;

var
  frmItemEditor: TfrmItemEditor;
implementation
{$R *.dfm}

procedure TfrmItemEditor.InsertTypes();
var
  i: integer;
begin
  SetLength(FArguments, Length(FActions.Actions));
  for i := Low(FArguments) to High(FArguments) do
  begin
    FArguments[i] := TStringList.Create();
  end;

  for i := 0 to High(FActions.Actions) do
  begin
    cbType.Items.Add(FActions.Actions[i].Name);
  end;
end;

procedure TfrmItemEditor.llTextLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
var
  i, ArgIndex: integer;
  action: TAction;
  frmEditor: TfrmArgumentEditor;
  updated, value: TStrings;
  hasMultipleArgs: boolean;
begin
  if LinkType <> sltID then
  begin
    exit;
  end;

  action := GetActionById(cbType.ItemIndex);
  ArgIndex := -1;
  for i := Low(action.Arguments) to High(action.Arguments) do
  begin
    if action.Arguments[i].InternalName = Link then
    begin
      ArgIndex := i;
      break;
    end;
  end;

  if ArgIndex = -1 then
  begin
    exit;
  end;

  hasMultipleArgs := (action.Arguments[ArgIndex].Kind = akVariadic) or (action.Arguments[ArgIndex].Kind = akColor);

  frmEditor := TfrmArgumentEditor.Create(self);
  value := TStringList.Create();
  if hasMultipleArgs then
  begin
    for i := ArgIndex to FArguments[cbType.ItemIndex].Count - 1 do
    begin
      value.Add(FArguments[cbType.ItemIndex][i]);
    end;
  end else begin
    value.Add(FArguments[cbType.ItemIndex][ArgIndex]);
  end;

  updated := frmEditor.Edit(action.Arguments[ArgIndex], value);
  frmEditor.Free();
  value.Free();

  if hasMultipleArgs then
  begin
    for i := FArguments[cbType.ItemIndex].Count - 1 downto ArgIndex do
    begin
      FArguments[cbType.ItemIndex].Delete(i);
    end;
  end;

  if updated <> nil then
  begin
    if hasMultipleArgs then
    begin
      FArguments[cbType.ItemIndex].AddStrings(updated);
    end else begin
      FArguments[cbType.ItemIndex][ArgIndex] := updated.Text;
    end;
  end else begin
    if not hasMultipleArgs then
    begin
      if ArgIndex > action.Required then
      begin
        FArguments[cbType.ItemIndex].Delete(ArgIndex);
      end;
    end;
  end;

  UpdateLinkText();
end;

function TfrmItemEditor.GetActionById(const Index: integer): TAction;
begin
  Result := FActions.Actions[Index];
end;

procedure TfrmItemEditor.UpdateLinkText();
var
  action: TAction;
  i, Index: integer;
  text, argument, pattern: string;
  match: TMatch;
begin
  Index := cbType.ItemIndex;
  action := GetActionById(Index);

  text := action.Text;

  for i := Low(action.Arguments) to High(action.Arguments) do
  begin
    argument := '';
    if (FArguments[Index].Count - 1 >= i) and (FArguments[Index][i] <> '') then
    begin
      argument := FArguments[Index][i];
    end;

    match := TRegEx.Match(text, Format('{%d\|(.*)}', [i + 1]));
    if match.Success then
    begin
      pattern := match.Value;
      argument := Trim(argument + ' ' + match.Groups.Item[1].Value);
    end else begin
      pattern := Format('{%d}', [i + 1]);
    end;

    if argument = '' then
    begin
      argument := action.Arguments[i].Name;
    end;

    text := StringReplace(text, pattern, argument, [rfReplaceAll]);
  end;

  llText.Caption := text;
end;

procedure TfrmItemEditor.bOKClick(Sender: TObject);
begin
  // todo: validate
  ModalResult := mrOk;
  CloseModal();
end;

procedure TfrmItemEditor.cbTypeClick(Sender: TObject);
begin
  gbEditor.Caption := Format(' %s ', [FActions.Actions[cbType.ItemIndex].Name]);

  UpdateLinkText();
end;

procedure TfrmItemEditor.cbTypeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  c: TCanvas;
  action: TAction;
begin
  c := cbType.Canvas;
  c.FillRect(rect);

  action := GetActionById(Index);

  c.TextOut(Rect.Left + 2, Rect.Top + 2, action.Name);

  c.Font.Size := 8;
  c.TextOut(Rect.Right - 2 - c.TextWidth(action.Kind), Rect.Top + 4, action.Kind);
end;

function TfrmItemEditor.Edit(const Actions: TActions; const Item: TFilterItem): TFilterItem;
var
  i: integer;
begin
  Result := nil;

  FActions := Actions;
  FItem := Item;

  InsertTypes();
  for i := 0 to cbType.Items.Count - 1 do
  begin
    if item.Action.Name = FActions.Actions[i].Name then
    begin
      cbType.ItemIndex := i;
      break;
    end;
  end;

  FArguments[i].AddStrings(FItem.Arguments);
  cbTypeClick(nil);

  if ShowModal() = mrOk then
  begin
    FItem.Arguments.Clear();
    FItem.Arguments.AddStrings(FArguments[cbType.ItemIndex]);
    Result := FItem;
  end;
end;

procedure TfrmItemEditor.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cbType.Items.Count - 1 do
  begin
    FArguments[i].Free();
  end;
end;

function TfrmItemEditor.Add(const Actions: TActions): TFilterItem;
begin
  Result := nil;

  FActions := Actions;

  InsertTypes();
  cbType.ItemIndex := 0;
  cbTypeClick(nil);

  if ShowModal() = mrOk then
  begin
    Result := FItem;
  end;
end;

end.

