object OptionsFm: TOptionsFm
  Left = 626
  Top = 249
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 229
  ClientWidth = 242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    242
    229)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 33
    AutoSize = False
    Caption = 'lbl1'
    Color = clWindow
    ParentColor = False
  end
  object btnOk: TButton
    Left = 160
    Top = 199
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 4
  end
  object chkCaseSensitive: TCheckBox
    Left = 8
    Top = 48
    Width = 128
    Height = 17
    Caption = 'Case Sensitive'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object chkTwoWindow: TCheckBox
    Left = 8
    Top = 72
    Width = 129
    Height = 17
    Caption = 'Two Window'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object btnSelect: TButton
    Left = 160
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 0
    OnClick = btnSelectClick
  end
  object chkSaveOnExit: TCheckBox
    Left = 8
    Top = 96
    Width = 177
    Height = 17
    Caption = 'Save On Exit (always true)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object dlgFont1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 104
  end
end
