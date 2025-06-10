object frmNewFilter: TfrmNewFilter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New filter...'
  ClientHeight = 152
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
  object cbGameVersion: TComboBox
    Left = 24
    Top = 8
    Width = 417
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Path of Exile'
    Items.Strings = (
      'Path of Exile'
      'Path of Exile 2')
  end
  object gbNameContainer: TGroupBox
    Left = 24
    Top = 35
    Width = 417
    Height = 78
    Caption = 'Filter'
    TabOrder = 1
    object tFilterName: TEdit
      Left = 16
      Top = 24
      Width = 385
      Height = 21
      TabOrder = 0
    end
    object cbRuthless: TCheckBox
      Left = 16
      Top = 51
      Width = 97
      Height = 17
      Caption = 'Ruthless'
      TabOrder = 1
    end
  end
  object bOk: TButton
    Left = 279
    Top = 119
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = bOkClick
  end
  object bCancel: TButton
    Left = 360
    Top = 119
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
