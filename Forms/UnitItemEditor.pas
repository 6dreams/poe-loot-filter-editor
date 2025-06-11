unit UnitItemEditor;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFilter,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.RegularExpressions, UnitArgumentEditor, UnitWinUtils
  {$IFDEF DEBUG}, Clipbrd{$ENDIF};

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
  {$IFDEF DEBUG}procedure gbEditorDblClick(Sender: TObject);{$ENDIF}
  procedure FormCreate(Sender: TObject);
  procedure OnSearch(Text: string);
private
  FActions: TActions;
  FArguments: array of TStrings;
  FSearch: TTimedSearch;
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
  hasMultipleArgs, isArgumentExists: boolean;
begin
  if LinkType = sltURL then
  begin
    wuOpenUrl(Handle, Link);
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
  isArgumentExists := FArguments[cbType.ItemIndex].Count > ArgIndex;

  frmEditor := TfrmArgumentEditor.Create(self);
  value := TStringList.Create();
  if hasMultipleArgs then
  begin
    for i := ArgIndex to FArguments[cbType.ItemIndex].Count - 1 do
    begin
      value.Add(FArguments[cbType.ItemIndex][i]);
    end;
  end else begin
    if isArgumentExists then
    begin
      value.Add(FArguments[cbType.ItemIndex][ArgIndex]);
    end else begin
      value.Add(action.Arguments[ArgIndex].GetDefaultValue());
    end;
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
      if isArgumentExists then
      begin
        FArguments[cbType.ItemIndex][ArgIndex] := updated[0];
      end else begin
        for i := FArguments[cbType.ItemIndex].Count to ArgIndex - 1 do
        begin
          FArguments[cbType.ItemIndex].Add(FActions.Actions[cbType.ItemIndex].Arguments[i].GetDefaultValue());
        end;

        FArguments[cbType.ItemIndex].Insert(ArgIndex, updated[0]);
      end;
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

  updated.Free();

  UpdateLinkText();
end;

function TfrmItemEditor.GetActionById(const Index: integer): TAction;
begin
  Result := FActions.Actions[Index];
end;

procedure TfrmItemEditor.UpdateLinkText();
var
  action: TAction;
  i, j, Index: integer;
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
      case action.Arguments[i].Kind of
        akVariadic: begin
          argument := FArguments[Index][i] + ', ...';
        end;
        akColor: begin
          argument := '';

          for j := i to FArguments[Index].Count - 1 do
          begin
            argument := argument + FArguments[Index][j];

            if j < FArguments[Index].Count - 1 then
            begin
              argument := argument + ', ';
            end;
          end;
        end;
        akList: begin
          argument := action.Arguments[i].GetNameByValue(FArguments[Index][i]);
        end
        else begin
          argument := FArguments[Index][i];
        end;
      end;
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
      argument := Format('[%s]', [action.Arguments[i].Name]);
    end;

    text := StringReplace(text, pattern, argument, [rfReplaceAll]);
  end;

  llText.Caption := StringReplace(text, '\n', #10, [rfReplaceAll]);
end;

procedure TfrmItemEditor.bOKClick(Sender: TObject);
const
  csError = 'Error';
  csFormat = 'Please enter value for %s.';
var
  action: TAction;
  args: TStrings;
  i: Integer;
begin
  action := GetActionById(cbType.ItemIndex);
  args   := FArguments[cbType.ItemIndex];

  for i := Low(action.Arguments) to High(action.Arguments) do
  begin
    if i >= args.Count then
    begin
      MessageBox(Handle, PChar(Format(csFormat, [action.Arguments[i].Name])), csError, MB_ICONERROR);
      exit;
    end;

    if args[i] = '' then
    begin
      MessageBox(Handle, PChar(Format(csFormat, [action.Arguments[i].Name])), csError, MB_ICONERROR);
      exit;
    end;

  end;

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

  InsertTypes();
  for i := 0 to cbType.Items.Count - 1 do
  begin
    if item.Action.Name = FActions.Actions[i].Name then
    begin
      cbType.ItemIndex := i;
      break;
    end;
  end;

  FArguments[i].AddStrings(Item.Arguments);
  cbTypeClick(nil);

  if ShowModal() = mrOk then
  begin
    Result := TFilterItem.Create(FActions.Actions[cbType.ItemIndex], TStringList.Create());
    Result.Arguments.AddStrings(FArguments[cbType.ItemIndex]);
  end;
end;

procedure TfrmItemEditor.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}gbEditor.OnDblClick := gbEditorDblClick;{$ENDIF}
  FSearch := TTimedSearch.Create();
  FSearch.OnSearch := OnSearch;

  cbType.OnKeyPress := FSearch.Handler;
  OnKeyPress := FSearch.Handler;
  bOK.OnKeyPress := FSearch.Handler;
  bCancel.OnKeyPress := FSearch.Handler;
end;

procedure TfrmItemEditor.OnSearch(Text: string);
var
  i: integer;
  sl: TStringList;
begin
  {$IFDEF DEBUG}OutputDebugString(PChar(Format('Search: `%s`', [Text])));{$ENDIF}

  sl := TStringList.Create();
  for i := Low(FActions.Actions) to High(FActions.Actions) do
  begin
    sl.Add(FActions.Actions[i].Name);
  end;

  i := FSearch.GetMatched(Text, sl);
  sl.Free();

  if i = -1 then
  begin
    exit;
  end;

  cbType.ItemIndex := i;
end;

procedure TfrmItemEditor.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cbType.Items.Count - 1 do
  begin
    FArguments[i].Free();
  end;
  FSearch.Free();
end;

{$IFDEF DEBUG}
procedure TfrmItemEditor.gbEditorDblClick(Sender: TObject);
begin
  Clipboard.AsText := llText.Caption;
end;
{$ENDIF}

function TfrmItemEditor.Add(const Actions: TActions): TFilterItem;
begin
  Result := nil;

  FActions := Actions;

  InsertTypes();
  cbType.ItemIndex := 0;
  cbTypeClick(nil);

  if ShowModal() = mrOk then
  begin
    Result := TFilterItem.Create(FActions.Actions[cbType.ItemIndex], TStringList.Create());
    Result.Arguments.AddStrings(FArguments[cbType.ItemIndex]);
  end;
end;

end.

