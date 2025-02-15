object frmBlockEditor: TfrmBlockEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Block Editor'
  ClientHeight = 368
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescription: TLabel
    Left = 8
    Top = 16
    Width = 57
    Height = 13
    Caption = 'Description:'
  end
  object lblContent: TLabel
    Left = 8
    Top = 87
    Width = 102
    Height = 13
    Caption = 'Actions && Conditions:'
  end
  object rgOptions: TRadioGroup
    Left = 8
    Top = 248
    Width = 617
    Height = 81
    Caption = 'Options'
    TabOrder = 0
  end
  object tDescription: TMemo
    Left = 8
    Top = 32
    Width = 617
    Height = 49
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object lbContent: TListBox
    Left = 8
    Top = 103
    Width = 617
    Height = 139
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PopupMenu = pmEditor
    TabOrder = 2
    OnDblClick = lbContentDblClick
    OnKeyDown = lbContentKeyDown
  end
  object bOK: TButton
    Left = 455
    Top = 335
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 536
    Top = 335
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbDisabled: TCheckBox
    Left = 24
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Disabled'
    TabOrder = 5
  end
  object pmEditor: TPopupMenu
    OnPopup = pmEditorPopup
    Left = 24
    Top = 112
    object pmiAdd: TMenuItem
      Caption = 'Add'
      OnClick = pmiAddClick
    end
    object pmiEdit: TMenuItem
      Caption = 'Edit'
      Default = True
      OnClick = lbContentDblClick
    end
    object pmiDelete: TMenuItem
      Caption = 'Delete'
      OnClick = pmiDeleteClick
    end
  end
end
