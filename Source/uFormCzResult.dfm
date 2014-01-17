inherited FormCzResult: TFormCzResult
  Caption = 'Result form'
  ClientHeight = 313
  ClientWidth = 735
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbResult: TListBox
    Left = 0
    Top = 0
    Width = 735
    Height = 272
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 24
    ParentFont = False
    PopupMenu = pmResult
    TabOrder = 0
    OnDblClick = lbResultDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 272
    Width = 735
    Height = 41
    Align = alBottom
    TabOrder = 1
    object bOK: TButton
      Left = 634
      Top = 8
      Width = 89
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = bOKClick
    end
  end
  object pmResult: TPopupMenu
    Left = 272
    Top = 112
    object miDeleteItem: TMenuItem
      Caption = 'Delete item'
      OnClick = miDeleteItemClick
    end
    object Addtodictionary1: TMenuItem
      Caption = 'Add to dictionary'
    end
    object Addcomponenttypetoignorelist1: TMenuItem
      Caption = 'Add component type to ignore list'
    end
    object Addpropertytoignorelist1: TMenuItem
      Caption = 'Add property to ignore list'
    end
  end
end
