object HelpFm: THelpFm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Hints'
  ClientHeight = 264
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object btnClose: TButton
    Left = 232
    Top = 224
    Width = 89
    Height = 30
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 541
    Height = 210
    Lines.Strings = (
      'Alt + left mouse button - select word in Log.'
      
        'To remove this selection click (Alt + left mouse button) on word' +
        ' again.'
      ''
      'Alt + T - add first selected tag to tag list'
      ''
      'F5 - refresh current log')
    ReadOnly = True
    TabOrder = 1
  end
end
