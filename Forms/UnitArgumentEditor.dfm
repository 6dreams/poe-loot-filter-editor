object frmArgumentEditor: TfrmArgumentEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 648
  ClientWidth = 609
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
    609
    648)
  PixelsPerInch = 96
  TextHeight = 13
  object gbVariadic: TGroupBox
    Left = 16
    Top = 16
    Width = 577
    Height = 185
    Caption = 'gbVariadic'
    TabOrder = 4
    object lblVariadicEditor: TLabel
      Left = 16
      Top = 15
      Width = 75
      Height = 13
      Caption = 'lblVariadicEditor'
    end
    object lbVariadicEditor: TListBox
      Left = 16
      Top = 33
      Width = 545
      Height = 134
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object gbIntValue: TGroupBox
    Left = 16
    Top = 302
    Width = 577
    Height = 89
    Caption = 'Number'
    TabOrder = 0
    object lblIntEditor: TLabel
      Left = 16
      Top = 21
      Width = 52
      Height = 13
      Caption = 'lblIntEditor'
    end
    object tIntEditor: TEdit
      Left = 16
      Top = 40
      Width = 529
      Height = 21
      NumbersOnly = True
      TabOrder = 0
      Text = '0'
    end
    object udIntEditor: TUpDown
      Left = 545
      Top = 40
      Width = 16
      Height = 21
      Associate = tIntEditor
      TabOrder = 1
    end
  end
  object gbListValue: TGroupBox
    Left = 16
    Top = 207
    Width = 577
    Height = 89
    Caption = 'List'
    TabOrder = 3
    object lblListEditor: TLabel
      Left = 16
      Top = 21
      Width = 54
      Height = 13
      Caption = 'lblListEditor'
    end
    object cbListEditor: TComboBox
      Left = 16
      Top = 40
      Width = 545
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object bOK: TButton
    Left = 423
    Top = 615
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 504
    Top = 615
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object gbColor: TGroupBox
    Left = 16
    Top = 397
    Width = 577
    Height = 84
    Caption = 'gbColor'
    TabOrder = 5
    object lblColorRed: TLabel
      Left = 18
      Top = 21
      Width = 23
      Height = 13
      Caption = 'Red:'
    end
    object lblColorBlue: TLabel
      Left = 87
      Top = 21
      Width = 24
      Height = 13
      Caption = 'Blue:'
    end
    object lblColorGreen: TLabel
      Left = 156
      Top = 21
      Width = 33
      Height = 13
      Caption = 'Green:'
    end
    object lblColorAlpha: TLabel
      Left = 225
      Top = 21
      Width = 31
      Height = 13
      Caption = 'Alpha:'
    end
    object iColorDrawer: TImage
      Left = 344
      Top = 20
      Width = 217
      Height = 49
    end
    object bclColorSelector: TBevel
      Left = 343
      Top = 19
      Width = 217
      Height = 51
    end
    object tColorRed: TEdit
      Left = 18
      Top = 40
      Width = 63
      Height = 21
      NumbersOnly = True
      TabOrder = 0
      Text = 'tColorRed'
      OnChange = ColorChange
    end
    object tColorBlue: TEdit
      Left = 87
      Top = 40
      Width = 63
      Height = 21
      NumbersOnly = True
      TabOrder = 1
      Text = 'tColorBlue'
      OnChange = ColorChange
    end
    object tColorGreen: TEdit
      Left = 156
      Top = 40
      Width = 63
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = 'tColorGreen'
      OnChange = ColorChange
    end
    object tColorAlpha: TEdit
      Left = 225
      Top = 40
      Width = 63
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = 'tColorAlpha'
      OnChange = ColorChange
    end
    object bSelectColor: TButton
      Left = 294
      Top = 38
      Width = 43
      Height = 25
      Caption = '...'
      TabOrder = 4
      OnClick = bSelectColorClick
    end
  end
  object gbSockets: TGroupBox
    Left = 16
    Top = 488
    Width = 577
    Height = 89
    Caption = 'gbSockets'
    TabOrder = 6
    object lblSocketsValue: TLabel
      Left = 18
      Top = 24
      Width = 73
      Height = 13
      Caption = 'lblSocketsValue'
    end
    object tSocketsValue: TEdit
      Left = 18
      Top = 43
      Width = 543
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 7
      TabOrder = 0
    end
  end
  object cdColorSelector: TColorDialog
    CustomColors.Strings = (
      'ColorA=000000'
      'ColorB=FFFFFFFF'
      'ColorC=FFFFFFFF'
      'ColorD=FFFFFFFF'
      'ColorE=FFFFFFFF'
      'ColorF=FFFFFFFF'
      'ColorG=FFFFFFFF'
      'ColorH=FFFFFFFF'
      'ColorI=72EC53'
      'ColorJ=FFFFFFFF'
      'ColorK=FFFFFFFF'
      'ColorL=FFFFFFFF'
      'ColorM=FFFFFFFF'
      'ColorN=FFFFFFFF'
      'ColorO=FFFFFFFF'
      'ColorP=FFFFFFFF')
    Left = 392
    Top = 365
  end
end
