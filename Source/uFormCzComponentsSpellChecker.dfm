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
  object mIgnoreList: TMemo
    Left = 0
    Top = 153
    Width = 552
    Height = 127
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'TDataSet'
      'TOraQuery'
      'TOraSession'
      ''
      '.KeyField'
      '.ListField'
      '.Name'
      '')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object alEdit: TActionList
    Left = 470
    Top = 187
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
  end
end
