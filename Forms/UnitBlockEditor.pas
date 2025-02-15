unit UnitBlockEditor;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFilter,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls, UnitItemEditor;

type
TfrmBlockEditor = class(TForm)
  rgOptions: TRadioGroup;
  lblDescription: TLabel;
  tDescription: TMemo;
  lblContent: TLabel;
  lbContent: TListBox;
  bOK: TButton;
  bCancel: TButton;
  cbDisabled: TCheckBox;
  pmEditor: TPopupMenu;
  pmiAdd: TMenuItem;
  pmiEdit: TMenuItem;
  pmiDelete: TMenuItem;
  procedure bOKClick(Sender: TObject);
  procedure lbContentDblClick(Sender: TObject);
  procedure pmEditorPopup(Sender: TObject);
  procedure pmiDeleteClick(Sender: TObject);
  procedure pmiAddClick(Sender: TObject);
  procedure lbContentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
private
  FActions: TActions;
  FBlock: TFilterBlock;
  function BuildItemText(const Name: string; const Arguments: TStrings): string;
public
  function Edit(const Actions: TActions; const Block: TFilterBlock): TFilterBlock;
  function Add(const Actions: TActions): TFilterBlock;
end;

var
  frmBlockEditor: TfrmBlockEditor;

implementation
{$R *.dfm}

procedure TfrmBlockEditor.bOKClick(Sender: TObject);
begin
  if Length(FBlock.Items) = 0 then
  begin
    MessageBox(Handle, 'Filter Block cannot be empty, add some actions or conditions', 'Error', MB_ICONERROR);
    exit;
  end;

  ModalResult := mrOk;
  CloseModal();
end;

function TfrmBlockEditor.BuildItemText(const Name: string; const Arguments: TStrings): string;
var
  i: integer;
begin
  Result := Name + ' ';

  for i := 0 to Arguments.Count - 1 do
  begin
    Result := Result + Arguments[i];
    if i < Arguments.Count - 1 then
    begin
      if TFilterUtils.IsOperator(Arguments[i], i) then
      begin
        Result := Result + ' ';
      end else begin
        Result := Result + ', ';
      end;
    end;
  end;
end;

function TfrmBlockEditor.Add(const Actions: TActions): TFilterBlock;
begin
  FBlock := TFilterBlock.Create();

  FActions := Actions;

  if ShowModal() <> mrOk then
  begin
    FBlock.Free();
    FBlock := nil;
  end else begin
    FBlock.Comment.Clear();
    FBlock.Comment.AddStrings(tDescription.Lines);
  end;

  Result := FBlock;
end;

function TfrmBlockEditor.Edit(const Actions: TActions; const Block: TFilterBlock): TFilterBlock;
var
  i: integer;
begin
  FBlock := Block.Clone();

  tDescription.Lines.AddStrings(FBlock.Comment);
  cbDisabled.Checked := Block.HasMarker(Block.mDisabled);

  FActions := Actions;

  lbContent.Items.BeginUpdate();
  for i := Low(Block.Items) to High(Block.Items) do
  begin
    lbContent.Items.Add(BuildItemText(Block.Items[i].Name, Block.Items[i].Arguments));
  end;
  lbContent.Items.EndUpdate();

  if ShowModal() <> mrOk then
  begin
    FBlock.Free();
    FBlock := nil;
  end else begin
    FBlock.Comment.Clear();
    FBlock.Comment.AddStrings(tDescription.Lines);
  end;

  Result := FBlock;
end;

procedure TfrmBlockEditor.lbContentDblClick(Sender: TObject);
var
  frmEditor: TfrmItemEditor;
  item: TFilterItem;
  index: integer;
begin
  index := lbContent.ItemIndex;
  if index = -1 then
  begin
    exit;
  end;

  frmEditor := TfrmItemEditor.Create(self);
  item := frmEditor.Edit(FActions, FBlock.Items[index]);
  frmEditor.Free();

  if item <> nil then
  begin
    FBlock.Items[index].Free();
    FBlock.Items[index] := item;
    lbContent.Items[index] := BuildItemText(FBlock.Items[index].Name, FBlock.Items[index].Arguments);
  end;
end;

procedure TfrmBlockEditor.lbContentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    pmiDeleteClick(Sender);
  end;
end;

procedure TfrmBlockEditor.pmEditorPopup(Sender: TObject);
begin
  pmiEdit.Enabled := lbContent.ItemIndex <> -1;
  pmiDelete.Enabled := pmiEdit.Enabled;
end;

procedure TfrmBlockEditor.pmiAddClick(Sender: TObject);
var
  target: integer;
  frmEditor: TfrmItemEditor;
  item: TFilterItem;
begin
  if lbContent.ItemIndex = -1 then
  begin
    target := lbContent.Items.Count - 1;
  end else begin
    target := lbContent.ItemIndex + 1;
  end;

  frmEditor := TfrmItemEditor.Create(self);
  item := frmEditor.Add(FActions);
  frmEditor.Free();

  if item = nil then
  begin
    exit;
  end;

  FBlock.Insert(target, item);
  lbContent.Items.Insert(target, BuildItemText(item.Name, item.Arguments));
end;

procedure TfrmBlockEditor.pmiDeleteClick(Sender: TObject);
begin
  if lbContent.ItemIndex = -1 then
  begin
    exit;
  end;

  FBlock.Delete(lbContent.ItemIndex);
  lbContent.DeleteSelected();
end;

end.

