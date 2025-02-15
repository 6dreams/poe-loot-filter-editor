object frmSectionEditor: TfrmSectionEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Section Editor'
  ClientHeight = 207
  ClientWidth = 361
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
  object lblSection: TLabel
    Left = 16
    Top = 13
    Width = 68
    Height = 13
    Caption = 'Section name:'
  end
  object lblDescription: TLabel
    Left = 16
    Top = 61
    Width = 57
    Height = 13
    Caption = 'Description:'
  end
  object tSectionName: TEdit
    Left = 16
    Top = 32
    Width = 329
    Height = 21
    TabOrder = 0
  end
  object tSectionDescription: TMemo
    Left = 16
    Top = 80
    Width = 329
    Height = 89
    TabOrder = 1
  end
  object bOK: TButton
    Left = 183
    Top = 175
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 264
    Top = 175
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
