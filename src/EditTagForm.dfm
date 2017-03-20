object EditTagFm: TEditTagFm
  Left = 618
  Top = 337
  BorderStyle = bsDialog
  Caption = 'Edit Tag'
  ClientHeight = 189
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 24
    Top = 14
    Width = 37
    Height = 16
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 24
    Top = 43
    Width = 77
    Height = 16
    Caption = 'Group Name'
  end
  object lbl1: TLabel
    Left = 24
    Top = 101
    Width = 32
    Height = 16
    Caption = 'Color'
  end
  object edtName: TEdit
    Left = 114
    Top = 8
    Width = 193
    Height = 24
    TabOrder = 0
    OnChange = edtNameChange
  end
  object btnOk: TButton
    Left = 80
    Top = 141
    Width = 75
    Height = 28
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 184
    Top = 141
    Width = 75
    Height = 28
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlColor: TPanel
    Left = 114
    Top = 99
    Width = 193
    Height = 22
    Caption = 'Assign Color...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = pnlColorClick
  end
  object edGroupName: TEdit
    Left = 114
    Top = 37
    Width = 193
    Height = 24
    TabOrder = 1
  end
  object chkCaseSensitive: TCheckBox
    Left = 114
    Top = 72
    Width = 128
    Height = 17
    Caption = 'Case Sensitive'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object ColorDialog1: TColorDialog
    Left = 264
    Top = 64
  end
end
