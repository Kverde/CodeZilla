object CzFormDictionaryEdit: TCzFormDictionaryEdit
  Left = 0
  Top = 0
  Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1083#1086#1074#1072
  ClientHeight = 393
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object LBDopWordsForDic: TListBox
    Left = 0
    Top = 0
    Width = 303
    Height = 393
    Align = alClient
    Sorted = True
    TabOrder = 0
    OnClick = LBDopWordsForDicClick
  end
  object PanelButtons: TPanel
    Left = 303
    Top = 0
    Width = 172
    Height = 393
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object bAdd: TButton
      AlignWithMargins = True
      Left = 15
      Top = 39
      Width = 142
      Height = 32
      Margins.Left = 15
      Margins.Top = 5
      Margins.Right = 15
      Align = alTop
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 0
      OnClick = bAddClick
    end
    object eWord: TEdit
      AlignWithMargins = True
      Left = 7
      Top = 7
      Width = 158
      Height = 24
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Align = alTop
      TabOrder = 1
    end
    object bEdit: TButton
      AlignWithMargins = True
      Left = 15
      Top = 79
      Width = 142
      Height = 32
      Margins.Left = 15
      Margins.Top = 5
      Margins.Right = 15
      Align = alTop
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      TabOrder = 2
      OnClick = bEditClick
    end
    object bDelete: TButton
      AlignWithMargins = True
      Left = 15
      Top = 119
      Width = 142
      Height = 32
      Margins.Left = 15
      Margins.Top = 5
      Margins.Right = 15
      Align = alTop
      Caption = #1059#1076#1072#1083#1080#1090#1100
      TabOrder = 3
      OnClick = bDeleteClick
    end
    object bClose: TButton
      AlignWithMargins = True
      Left = 15
      Top = 351
      Width = 142
      Height = 32
      Margins.Left = 15
      Margins.Right = 15
      Margins.Bottom = 10
      Align = alBottom
      Caption = #1042#1099#1093#1086#1076
      TabOrder = 4
      OnClick = bCloseClick
    end
    object bSelectDict: TButton
      AlignWithMargins = True
      Left = 15
      Top = 174
      Width = 142
      Height = 32
      Margins.Left = 15
      Margins.Top = 20
      Margins.Right = 15
      Align = alTop
      Caption = #1054#1089#1085#1086#1074#1085#1086#1081' '#1089#1083#1086#1074#1072#1088#1100
      TabOrder = 5
      OnClick = bSelectDictClick
    end
    object bOptions: TButton
      Left = 15
      Top = 212
      Width = 142
      Height = 32
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      TabOrder = 6
      OnClick = bOptionsClick
    end
  end
end
