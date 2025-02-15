unit UnitSectionEditor;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UnitWinUtils;

type
TfrmSectionEditor = class(TForm)
  tSectionName: TEdit;
  tSectionDescription: TMemo;
  lblSection: TLabel;
  lblDescription: TLabel;
  bOK: TButton;
  bCancel: TButton;
    procedure bOKClick(Sender: TObject);
public
  function NewSection(): TStrings;
  function EditSection(const Section: TStrings): TStrings;
end;

var
  frmSectionEditor: TfrmSectionEditor;

implementation
{$R *.dfm}
function TfrmSectionEditor.NewSection(): TStrings;
begin
  if ShowModal() <> mrOk then
  begin
    Result := nil;
    exit;
  end;

  Result := TStringList.Create();
  Result.Add(tSectionName.Text);
  Result.AddStrings(tSectionDescription.Lines);
end;

procedure TfrmSectionEditor.bOKClick(Sender: TObject);
begin
  if Trim(tSectionName.Text) = '' then
  begin
    wuShowBalloon(tSectionName.Handle, 'Error', 'Section name cannot be empty!', biError);
    exit;
  end;

  ModalResult := mrOk;
  CloseModal();
end;

function TfrmSectionEditor.EditSection(const Section: TStrings): TStrings;
var
  i: integer;
begin
  if Section.Count > 0 then
  begin
    tSectionName.Text := Section[0];
  end;

  tSectionDescription.Lines.BeginUpdate();
  for i := 1 to Section.Count - 1 do
  begin
    tSectionDescription.Lines.Add(Section[i]);
  end;
  tSectionDescription.Lines.EndUpdate();

  Result := NewSection();
end;
end.

