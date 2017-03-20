object OptionsFm: TOptionsFm
  Left = 626
  Top = 249
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 152
  ClientWidth = 242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    242
    152)
  PixelsPerInch = 96
  TextHeight = 16
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
    Left = 62
    Top = 99
    Width = 107
    Height = 37
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    ExplicitTop = 155
  end
  object chkTwoWindow: TCheckBox
    Left = 8
    Top = 56
    Width = 129
    Height = 17
    Caption = 'Two Window'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
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
  object dlgFont1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 104
  end
end
