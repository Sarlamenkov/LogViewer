object MainFm: TMainFm
  Left = 362
  Top = 164
  Width = 948
  Height = 678
  Caption = 'Log Viewer'
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
  object Splitter1: TSplitter
    Left = 249
    Top = 0
    Width = 8
    Height = 632
  end
  object PageControl1: TPageControl
    Left = 257
    Top = 0
    Width = 683
    Height = 632
    Align = alClient
    TabOrder = 0
    OnMouseUp = PageControl1MouseUp
  end
  inline tlTags: TTagListFrm
    Left = 0
    Top = 0
    Width = 249
    Height = 632
    Align = alLeft
    TabOrder = 1
    inherited pnl1: TPanel
      inherited lbl1: TLabel
        Width = 45
      end
    end
    inherited pgc1: TPageControl
      Height = 572
      inherited ts1: TTabSheet
        inherited vtTags: TVirtualStringTree
          Height = 544
        end
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 107
    Top = 97
  end
  object MainMenu: TMainMenu
    Left = 218
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
    object About1: TMenuItem
      Caption = 'About'
      OnClick = About1Click
    end
  end
end
