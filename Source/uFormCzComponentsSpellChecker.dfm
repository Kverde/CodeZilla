object FormCzComponentsSpellChecker: TFormCzComponentsSpellChecker
  Left = 0
  Top = 0
  Caption = 'Components spell checker'
  ClientHeight = 310
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 5
      Top = 135
      Width = 66
      Height = 16
      Caption = 'Ignore list'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rgKindFind: TRadioGroup
      AlignWithMargins = True
      Left = 7
      Top = 7
      Width = 254
      Height = 122
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 10
      Caption = 'Where'
      ItemIndex = 3
      Items.Strings = (
        'Current file'
        'Open project files'
        'All files in project'
        'All files in project group')
      TabOrder = 0
    end
    object bDictionary: TButton
      Left = 271
      Top = 78
      Width = 106
      Height = 41
      Caption = 'Edit custom word'
      ImageIndex = 1
      ImageMargins.Left = 3
      TabOrder = 1
      OnClick = bDictionaryClick
    end
    object bRefresh: TButton
      Left = 383
      Top = 78
      Width = 75
      Height = 41
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = bRefreshClick
    end
    object bSelectDictionary: TButton
      Left = 271
      Top = 31
      Width = 106
      Height = 41
      Caption = 'Select dictionary'
      TabOrder = 3
      OnClick = bSelectDictionaryClick
    end
    object lbCurrentDictionary: TLinkLabel
      Left = 277
      Top = 10
      Width = 95
      Height = 17
      Caption = 'Current dictionary:'
      TabOrder = 4
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 280
    Width = 552
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      552
      30)
    object bOk: TButton
      Left = 291
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 0
      OnClick = bOkClick
    end
    object bCancel: TButton
      Left = 378
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = bCancelClick
    end
    object bHelp: TButton
      Left = 466
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      Enabled = False
      TabOrder = 2
    end
  end
  object mFilters: TMemo
    Left = 0
    Top = 153
    Width = 552
    Height = 127
    Align = alClient
    Lines.Strings = (
      ''
      '.Name')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
