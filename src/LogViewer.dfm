object Form1: TForm1
  Left = 226
  Top = 103
  Width = 778
  Height = 513
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 486
    Align = alLeft
    Caption = 'pnlLeft'
    TabOrder = 0
    object VirtualStringTree2: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 183
      Height = 484
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      TabOrder = 0
      Columns = <>
    end
  end
  object pnlWork: TPanel
    Left = 185
    Top = 0
    Width = 585
    Height = 486
    Align = alClient
    Caption = 'pnlWork'
    TabOrder = 1
    object VirtualStringTree1: TVirtualStringTree
      Left = 1
      Top = 101
      Width = 583
      Height = 384
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      TabOrder = 0
      Columns = <>
    end
    object VirtualStringTree3: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 583
      Height = 100
      Align = alTop
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      TabOrder = 1
      Columns = <>
    end
  end
end
