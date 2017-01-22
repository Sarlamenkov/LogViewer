object View2Frm: TView2Frm
  Left = 0
  Top = 0
  Width = 662
  Height = 481
  TabOrder = 0
  object pnlWork: TPanel
    Left = 0
    Top = 0
    Width = 662
    Height = 459
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlWork'
    TabOrder = 0
    object pnl1: TPanel
      Left = 0
      Top = 0
      Width = 662
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lbl1: TLabel
        Left = 11
        Top = 10
        Width = 33
        Height = 13
        Caption = 'Search'
        Layout = tlCenter
      end
      object edtSearch: TEdit
        Left = 54
        Top = 6
        Width = 114
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object btnFindNext: TButton
        Left = 172
        Top = 5
        Width = 61
        Height = 25
        Caption = 'Next'
        TabOrder = 1
      end
      object chkFiltered: TCheckBox
        Left = 320
        Top = 9
        Width = 65
        Height = 17
        TabStop = False
        Caption = 'Filtered'
        TabOrder = 2
      end
      object btnFindPrev: TButton
        Left = 236
        Top = 5
        Width = 61
        Height = 25
        Caption = 'Prev'
        TabOrder = 3
      end
      object chkTwoWindows: TCheckBox
        Left = 395
        Top = 9
        Width = 97
        Height = 17
        Caption = '2 windows'
        TabOrder = 4
      end
    end
    object pnl2: TPanel
      Left = 0
      Top = 33
      Width = 648
      Height = 426
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object spl1: TSplitter
        Left = 0
        Top = 221
        Width = 648
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object pnl3: TPanel
        Left = 0
        Top = 227
        Width = 648
        Height = 199
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnl3'
        TabOrder = 1
        object pnl8: TPanel
          Left = 0
          Top = 0
          Width = 648
          Height = 199
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnl8'
          TabOrder = 0
          object spl2: TSplitter
            Left = 641
            Top = 0
            Width = 6
            Height = 199
            Align = alRight
            MinSize = 1
          end
          object vtFilteredLog2: TVirtualStringTree
            Left = 647
            Top = 0
            Width = 1
            Height = 199
            Align = alRight
            ClipboardFormats.Strings = (
              'Plain text')
            Colors.UnfocusedSelectionBorderColor = clHighlight
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            Font.Style = []
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'MS Sans Serif'
            Header.Font.Style = []
            Header.Options = [hoDblClickResize, hoDrag, hoFullRepaintOnResize]
            LineStyle = lsSolid
            ParentFont = False
            TabOrder = 0
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnEdit]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick]
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection, toShowFilteredNodes]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect]
            Columns = <
              item
                Color = clBtnFace
                Options = [coParentBidiMode, coShowDropMark, coVisible, coFixed, coAllowFocus]
                Position = 0
                Width = 20
                WideText = 'Line'
              end
              item
                Position = 1
                Width = 500
                WideText = 'Text'
              end>
          end
          object vtFilteredLog: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 641
            Height = 199
            Align = alClient
            ClipboardFormats.Strings = (
              'Plain text')
            Colors.UnfocusedSelectionBorderColor = clHighlight
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            Font.Style = []
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'MS Sans Serif'
            Header.Font.Style = []
            Header.Options = [hoDblClickResize, hoDrag, hoFullRepaintOnResize]
            LineStyle = lsSolid
            ParentFont = False
            TabOrder = 1
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnEdit]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick]
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection, toShowFilteredNodes]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect]
            OnBeforeCellPaint = vtLogBeforeCellPaint
            OnGetText = vtLogGetText
            Columns = <
              item
                Color = clBtnFace
                Options = [coParentBidiMode, coShowDropMark, coVisible, coFixed, coAllowFocus]
                Position = 0
                Width = 20
                WideText = 'Line'
              end
              item
                Position = 1
                Width = 500
                WideText = 'Text'
              end>
          end
        end
      end
      object vtLog: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 648
        Height = 221
        Align = alTop
        ClipboardFormats.Strings = (
          'Plain text')
        Colors.UnfocusedSelectionBorderColor = clHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier'
        Font.Style = []
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Options = [hoDblClickResize, hoDrag, hoFullRepaintOnResize]
        LineStyle = lsSolid
        ParentFont = False
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnEdit]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection, toShowFilteredNodes]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect]
        OnBeforeCellPaint = vtLogBeforeCellPaint
        OnGetText = vtLogGetText
        Columns = <
          item
            Color = clBtnFace
            Options = [coParentBidiMode, coShowDropMark, coVisible, coFixed, coAllowFocus]
            Position = 0
            Width = 20
            WideText = 'Line'
          end
          item
            Position = 1
            Width = 500
            WideText = 'Text'
          end>
      end
    end
    object pnl4: TPanel
      Left = 648
      Top = 33
      Width = 14
      Height = 426
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object pb1: TPaintBox
        Left = 0
        Top = 0
        Width = 14
        Height = 410
        Align = alClient
      end
      object pnl6: TPanel
        Left = 0
        Top = 410
        Width = 14
        Height = 16
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object pnl7: TPanel
    Left = 0
    Top = 459
    Width = 662
    Height = 22
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object lblCount: TLabel
      Left = 15
      Top = 5
      Width = 39
      Height = 13
      Caption = 'lblCount'
    end
  end
end
