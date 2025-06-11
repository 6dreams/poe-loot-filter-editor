program LootFilterEditor;

{$R *.dres}

uses
  Vcl.Forms,
  UnitMain in 'Forms\UnitMain.pas' {frmMain},
  UnitFilter in 'Units\UnitFilter.pas',
  UnitFilterSelector in 'Forms\UnitFilterSelector.pas' {frmFilterSelector},
  UnitUtils in 'Units\UnitUtils.pas',
  UnitSectionEditor in 'Forms\UnitSectionEditor.pas' {frmSectionEditor},
  UnitWinUtils in 'Units\UnitWinUtils.pas',
  UnitBlockEditor in 'Forms\UnitBlockEditor.pas' {frmBlockEditor},
  UnitItemEditor in 'Forms\UnitItemEditor.pas' {frmItemEditor},
  UnitArgumentEditor in 'Forms\UnitArgumentEditor.pas' {frmArgumentEditor},
  UnitAbout in 'Forms\UnitAbout.pas' {frmAbout},
  UnitNewFilter in 'Forms\UnitNewFilter.pas' {frmNewFilter};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  wuLoadInternalFont();
  Application.Run;
  wuUnloadInternalFont();
end.

