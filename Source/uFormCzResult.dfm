inherited FormCzResult: TFormCzResult
  Caption = 'Result form'
  ClientHeight = 313
  ClientWidth = 735
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
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
    object lbResultCount: TLabel
      Left = 6
      Top = 14
      Width = 66
      Height = 13
      Caption = 'lbResultCount'
    end
    object bClose: TButton
      Left = 638
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = bCloseClick
    end
    object bOpenSpellChecker: TButton
      Left = 482
      Top = 8
      Width = 138
      Height = 25
      Caption = 'Open spell checker'
      TabOrder = 1
      OnClick = bCloseClick
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
      OnClick = Addtodictionary1Click
    end
    object Addcomponenttypetoignorelist1: TMenuItem
      Caption = 'Add component type to ignore list'
      OnClick = Addcomponenttypetoignorelist1Click
    end
    object Addpropertytoignorelist1: TMenuItem
      Caption = 'Add property to ignore list'
      OnClick = Addpropertytoignorelist1Click
    end
  end
end
