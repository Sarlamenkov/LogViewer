object AboutFm: TAboutFm
  Left = 461
  Top = 236
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 213
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
    TabOrder = 0
    object ProductName: TLabel
      Left = 8
      Top = 16
      Width = 68
      Height = 13
      Caption = 'Product Name'
      IsControl = True
    end
    object Version: TLabel
      Left = 8
      Top = 40
      Width = 35
      Height = 13
      Caption = 'Version'
      IsControl = True
    end
    object lbl1: TLabel
      Left = 107
      Top = 16
      Width = 53
      Height = 13
      Caption = 'Log Viewer'
      IsControl = True
    end
    object lblVersion: TLabel
      Left = 107
      Top = 40
      Width = 35
      Height = 13
      Caption = 'Version'
      IsControl = True
    end
    object lbl2: TLabel
      Left = 6
      Top = 88
      Width = 261
      Height = 26
      Caption = 
        'Created by Sergey Arlamenkov (sergey999@yandex.ru)'#13#10'and Alexande' +
        'r Zenkin'
    end
  end
  object OKButton: TButton
    Left = 111
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
