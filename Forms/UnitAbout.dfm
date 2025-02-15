object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About Path of Exile Lootfiler Editor...'
  ClientHeight = 184
  ClientWidth = 505
  Color = clBtnFace
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
    505
    184)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlHeading: TBevel
    Left = 8
    Top = 8
    Width = 489
    Height = 113
  end
  object iLogo: TImage
    Left = 9
    Top = 9
    Width = 487
    Height = 111
    AutoSize = True
    OnMouseUp = iLogoMouseUp
  end
  object bClose: TButton
    Left = 416
    Top = 150
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object llText: TLinkLabel
    Left = 7
    Top = 127
    Width = 489
    Height = 17
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'designed by <a href="https://6dreams.net/">6dreams</a> member .f' +
      'ry and hosted on <a href="https://github.com/6dreams/poe-loot-fi' +
      'lter-editor">github</a>'
    TabOrder = 1
    OnLinkClick = llTextLinkClick
  end
end
