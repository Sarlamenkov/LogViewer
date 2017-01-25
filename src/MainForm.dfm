object MainFm: TMainFm
  Left = 449
  Top = 218
  Caption = 'Log Viewer'
  ClientHeight = 620
  ClientWidth = 932
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 932
    Height = 620
    Align = alClient
    TabOrder = 0
    OnMouseUp = PageControl1MouseUp
  end
  object dlgOpen1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 107
    Top = 97
  end
  object MainMenu: TMainMenu
    Left = 219
    Top = 44
    object File1: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Caption = 'Open'
        OnClick = btn3Click
      end
      object miReopen: TMenuItem
        Caption = 'Reopen'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Closecurrent1: TMenuItem
        Caption = 'Close current'
        OnClick = actCloseTabExecute
      end
      object Closeall1: TMenuItem
        Caption = 'Close all'
        OnClick = Closeall1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      OnClick = Options1Click
    end
    object DefaultTags1: TMenuItem
      Caption = 'Default Tags'
      OnClick = DefaultTags1Click
    end
    object About1: TMenuItem
      Caption = 'About'
      OnClick = About1Click
    end
  end
end
