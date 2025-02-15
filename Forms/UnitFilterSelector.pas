unit UnitFilterSelector;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, System.Types, Vcl.StdCtrls, UnitFilter;

type
TfrmFilterSelector = class(TForm)
  lbFilters: TListBox;
  bReload: TButton;
  bSelect: TButton;
  bCancel: TButton;
  cbGameVersion: TComboBox;
  procedure bReloadClick(Sender: TObject);
  procedure lbFiltersDblClick(Sender: TObject);
  procedure bSelectClick(Sender: TObject);
    procedure cbGameVersionChange(Sender: TObject);
private
  function GetGamePath(): string;
  procedure GetFilters();
public
  function GetFilter(): string;
end;

implementation
{$R *.dfm}

function TfrmFilterSelector.GetGamePath(): string;
begin
  Result := Format('\My Games\%s\', [cbGameVersion.Text]);
end;

procedure TfrmFilterSelector.GetFilters();
var
  FileName: string;
begin
  lbFilters.Clear();
  for FileName in TDirectory.GetFiles(TPath.GetDocumentsPath() + GetGamePath()) do
  begin
    if TFilterUtils.IsFilter(FileName) then
    begin
      lbFilters.Items.Add(TFilterUtils.GetSimpleName(FileName));
    end
  end;
end;

procedure TfrmFilterSelector.lbFiltersDblClick(Sender: TObject);
begin
  bSelectClick(Sender);
end;

procedure TfrmFilterSelector.bReloadClick(Sender: TObject);
begin
  GetFilters();
end;

procedure TfrmFilterSelector.bSelectClick(Sender: TObject);
begin
  if lbFilters.ItemIndex <> -1 then
  begin
    ModalResult := mrOk;
    CloseModal();
  end;
end;

procedure TfrmFilterSelector.cbGameVersionChange(Sender: TObject);
begin
  GetFilters();
end;

function TfrmFilterSelector.GetFilter(): string;
begin
  GetFilters();
  if ShowModal() = mrOk then
  begin
    Result := TPath.GetDocumentsPath() + GetGamePath() + TFilterUtils.GetFileName(lbFilters.Items[lbFilters.ItemIndex]);
  end;
end;

end.

