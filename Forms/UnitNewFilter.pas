unit UnitNewFilter;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, System.IOUtils, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UnitWinUtils, UnitFilter;

type
TfrmNewFilter = class(TForm)
  cbGameVersion: TComboBox;
  gbNameContainer: TGroupBox;
  tFilterName: TEdit;
  bOk: TButton;
  bCancel: TButton;
  cbRuthless: TCheckBox;
  procedure bOkClick(Sender: TObject);
private
  function GetFilterName(): string;
public
  function NewFilter(): string;
end;

var
  frmNewFilter: TfrmNewFilter;

implementation
{$R *.dfm}

function TfrmNewFilter.GetFilterName(): string;
begin
  if Trim(tFilterName.Text) = '' then
  begin
    exit;
  end;

  Result := Format('%s\My Games\%s\%s%s', [TPath.GetDocumentsPath(), cbGameVersion.Text, Trim(tFilterName.Text), caFormat[Ord(cbRuthless.Checked)]]);

  if not TPath.HasValidFileNameChars(ExtractFileName(Result), false) then
  begin
    Result := '';
  end;
end;

procedure TfrmNewFilter.bOkClick(Sender: TObject);
const
  csError = 'Error';
var
  name: string;
begin
  name := GetFilterName();
  if name = '' then
  begin
    wuShowBalloon(tFilterName.Handle, csError, 'Invalid filter name', biError);
    exit;
  end;


  if FileExists(name) then
  begin
    wuShowBalloon(tFilterName.Handle, csError, 'Filter already exists!', biError);
    exit;
  end;

  ModalResult := mrOk;
  CloseModal();
end;

function TfrmNewFilter.NewFilter(): string;
begin
  ShowModal();
  if ModalResult = mrOk then
  begin
    Result := GetFilterName();
  end;
end;
end.

