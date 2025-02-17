object frmItemEditor: TfrmItemEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Item Editor'
  ClientHeight = 209
  ClientWidth = 457
  Color = clBtnFace
  Constraints.MinHeight = 248
  Constraints.MinWidth = 473
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    457
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object lblType: TLabel
    Left = 150
    Top = 23
    Width = 28
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Type:'
  end
  object cbType: TComboBox
    Left = 184
    Top = 16
    Width = 249
    Height = 28
    Style = csOwnerDrawFixed
    Anchors = [akTop, akRight]
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
    Height = 122
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' title '
    TabOrder = 1
    DesignSize = (
      441
      122)
    object llText: TLinkLabel
      Left = 11
      Top = 16
      Width = 421
      Height = 92
      Anchors = [akLeft, akTop, akRight, akBottom]
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
    Left = 285
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 366
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
