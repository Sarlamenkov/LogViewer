object ViewFrm: TViewFrm
  Left = 0
  Top = 0
  Width = 863
  Height = 591
  TabOrder = 0
  object pnlWork: TPanel
    Left = 0
    Top = 0
    Width = 863
    Height = 569
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlWork'
    TabOrder = 0
    object pnl1: TPanel
      Left = 0
      Top = 0
      Width = 863
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
        Left = 535
        Top = 5
        Width = 59
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
      Width = 849
      Height = 536
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object spl1: TSplitter
        Left = 0
        Top = 221
        Width = 849
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object pnl3: TPanel
        Left = 0
        Top = 227
        Width = 849
        Height = 309
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnl3'
        TabOrder = 1
        object pnl8: TPanel
          Left = 0
          Top = 0
          Width = 849
          Height = 309
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pnl8'
          TabOrder = 0
          object spl2: TSplitter
            Left = 842
            Top = 0
            Width = 6
            Height = 309
            Align = alRight
            MinSize = 1
          end
          object vrtlstrngtrFullLog2: TVirtualStringTree
            Left = 848
            Top = 0
            Width = 1
            Height = 309
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
            Width = 842
            Height = 309
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
        Width = 849
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
      inline View2Frm1: TView2Frm
        Left = 127
        Top = 30
        Width = 662
        Height = 481
        TabOrder = 2
        ExplicitLeft = 127
        ExplicitTop = 30
        ExplicitWidth = 662
        ExplicitHeight = 481
        inherited spl3: TSplitter
          Height = 459
        end
        inherited pnlWork: TPanel
          Width = 407
          Height = 459
        end
        inherited pnl7: TPanel
          Top = 459
          Width = 662
        end
        inherited tl1: TTagListFrm
          Height = 459
          ExplicitHeight = 459
          inherited pgc1: TPageControl
            Height = 429
            ExplicitHeight = 429
          end
        end
      end
    end
    object pnl4: TPanel
      Left = 849
      Top = 33
      Width = 14
      Height = 536
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object pb1: TPaintBox
        Left = 0
        Top = 0
        Width = 14
        Height = 520
        Align = alClient
        OnMouseUp = pb1MouseUp
        OnPaint = pb1Paint
      end
      object pnl6: TPanel
        Left = 0
        Top = 520
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
    Top = 569
    Width = 863
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
