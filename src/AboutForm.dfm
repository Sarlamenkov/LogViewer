object AboutFm: TAboutFm
  Left = 461
  Top = 236
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 227
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object Version: TLabel
      Left = 6
      Top = 48
      Width = 35
      Height = 13
      Caption = 'Version'
      IsControl = True
    end
    object lbl1: TLabel
      Left = 6
      Top = 8
      Width = 119
      Height = 34
      Caption = 'Log Viewer'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Impact'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object lblVersion: TLabel
      Left = 103
      Top = 48
      Width = 35
      Height = 13
      Caption = 'Version'
      IsControl = True
    end
    object Memo1: TMemo
      Left = 6
      Top = 86
      Width = 281
      Height = 108
      BevelInner = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'Created by Sergey Arlamenkov (sergey999@yandex.ru)'
        ''
        'Special thanks to Alexander Zenkin, Gena Minchuk, '
        'Tatyana Gutorova for great ideas, help in developing and '
        'optimization.')
      ReadOnly = True
      TabOrder = 0
    end
  end
  object OKButton: TButton
    Left = 103
    Top = 184
    Width = 90
    Height = 32
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
  end
end
