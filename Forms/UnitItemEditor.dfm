object frmItemEditor: TfrmItemEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Item Editor'
  ClientHeight = 209
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblType: TLabel
    Left = 150
    Top = 23
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object cbType: TComboBox
    Left = 184
    Top = 16
    Width = 249
    Height = 28
    Style = csOwnerDrawFixed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 22
    ParentFont = False
    TabOrder = 0
    OnClick = cbTypeClick
    OnDrawItem = cbTypeDrawItem
  end
  object gbEditor: TGroupBox
    Left = 8
    Top = 50
    Width = 441
    Height = 111
    Caption = ' title '
    TabOrder = 1
    object llText: TLinkLabel
      Left = 19
      Top = 24
      Width = 398
      Height = 73
      AutoSize = False
      Caption = 'some <a id="stuff">title</a> or <a id="aaa">another</a> one'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnLinkClick = llTextLinkClick
    end
  end
  object bOK: TButton
    Left = 277
    Top = 175
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 358
    Top = 175
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
