object frmFilterSelector: TfrmFilterSelector
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Path of Exile Loot filter...'
  ClientHeight = 327
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbFilters: TListBox
    Left = 24
    Top = 35
    Width = 417
    Height = 254
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 19
    ParentFont = False
    TabOrder = 1
    OnDblClick = lbFiltersDblClick
  end
  object bReload: TButton
    Left = 32
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 2
    OnClick = bReloadClick
  end
  object bSelect: TButton
    Left = 279
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Select'
    Default = True
    TabOrder = 3
    OnClick = bSelectClick
  end
  object bCancel: TButton
    Left = 360
    Top = 295
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbGameVersion: TComboBox
    Left = 24
    Top = 8
    Width = 417
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Path of Exile'
    OnChange = cbGameVersionChange
    Items.Strings = (
      'Path of Exile'
      'Path of Exile 2')
  end
end
