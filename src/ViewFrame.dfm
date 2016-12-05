object ViewFrm: TViewFrm
  Left = 0
  Top = 0
  Width = 822
  Height = 528
  TabOrder = 0
  object pnlWork: TPanel
    Left = 0
    Top = 0
    Width = 822
    Height = 506
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlWork'
    TabOrder = 0
    object pnl1: TPanel
      Left = 0
      Top = 0
      Width = 822
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
        OnChange = edtSearchChange
        OnKeyUp = edtSearchKeyUp
      end
      object btnFindNext: TButton
        Left = 172
        Top = 5
        Width = 61
        Height = 25
        Caption = 'Next'
        TabOrder = 1
        OnClick = btnFindNextClick
      end
      object chkFiltered: TCheckBox
        Left = 320
        Top = 9
        Width = 65
        Height = 17
        TabStop = False
        Caption = 'Filtered'
        TabOrder = 2
        OnClick = chkFilteredClick
      end
      object btn1: TButton
        Left = 637
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Cut text'
        TabOrder = 3
        OnClick = btn1Click
      end
      object btnFindPrev: TButton
        Left = 236
        Top = 5
        Width = 61
        Height = 25
        Caption = 'Prev'
        TabOrder = 4
      end
      object chkTwoWindows: TCheckBox
        Left = 395
        Top = 9
        Width = 97
        Height = 17
        Caption = '2 windows'
        TabOrder = 5
        OnClick = chkTwoWindowsClick
      end
    end
    object pnl2: TPanel
      Left = 0
      Top = 33
      Width = 822
      Height = 473
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object spl1: TSplitter
        Left = 0
        Top = 221
        Width = 822
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object pnl3: TPanel
        Left = 0
        Top = 227
        Width = 822
        Height = 246
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnl3'
        TabOrder = 1
        object pnl4: TPanel
          Left = 808
          Top = 0
          Width = 14
          Height = 246
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object pb1: TPaintBox
            Left = 0
            Top = 17
            Width = 14
            Height = 195
            Align = alClient
            OnPaint = pb1Paint
          end
          object pnl5: TPanel
            Left = 0
            Top = 0
            Width = 14
            Height = 17
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
          end
          object pnl6: TPanel
            Left = 0
            Top = 212
            Width = 14
            Height = 34
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
          end
        end
        object pnl8: TPanel
          Left = 0
          Top = 0
          Width = 808
          Height = 246
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnl8'
          TabOrder = 1
          object spl2: TSplitter
            Left = 801
            Top = 0
            Width = 6
            Height = 246
            Align = alRight
            MinSize = 1
          end
          object vrtlstrngtrFullLog2: TVirtualStringTree
            Left = 807
            Top = 0
            Width = 1
            Height = 246
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
            OnBeforeCellPaint = vtFullLogBeforeCellPaint
            OnDblClick = vtFullLogDblClick
            OnEditing = vtFullLogEditing
            OnEnter = vtLogEnter
            OnGetText = vtFullLogGetText
            OnKeyDown = vtFullLogKeyDown
            OnMouseUp = vtFullLogMouseDown
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
          object vtFullLog: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 801
            Height = 246
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
            OnBeforeCellPaint = vtFullLogBeforeCellPaint
            OnClick = vtLogClick
            OnDblClick = vtFullLogDblClick
            OnEditing = vtFullLogEditing
            OnEnter = vtLogEnter
            OnGetText = vtFullLogGetText
            OnKeyDown = vtFullLogKeyDown
            OnMouseUp = vtFullLogMouseDown
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
        Width = 822
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
        Visible = False
        OnBeforeCellPaint = vtFullLogBeforeCellPaint
        OnClick = vtLogClick
        OnEditing = vtFullLogEditing
        OnEnter = vtLogEnter
        OnGetText = vtFullLogGetText
        OnKeyDown = vtFullLogKeyDown
        OnMouseUp = vtFullLogMouseDown
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
  object pnl7: TPanel
    Left = 0
    Top = 506
    Width = 822
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
