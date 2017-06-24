object EditTagFm: TEditTagFm
  Left = 618
  Top = 337
  BorderStyle = bsDialog
  Caption = 'Edit Tag'
  ClientHeight = 189
  ClientWidth = 645
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
    Top = 20
    Width = 37
    Height = 16
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 328
    Top = 20
    Width = 77
    Height = 16
    Caption = 'Group Name'
  end
  object lbl1: TLabel
    Left = 24
    Top = 117
    Width = 32
    Height = 16
    Caption = 'Color'
  end
  object Label3: TLabel
    Left = 24
    Top = 54
    Width = 68
    Height = 16
    Caption = 'Description'
  end
  object edtName: TEdit
    Left = 114
    Top = 17
    Width = 193
    Height = 24
    TabOrder = 0
    OnChange = edtNameChange
  end
  object btnOk: TButton
    Left = 432
    Top = 153
    Width = 75
    Height = 28
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 536
    Top = 153
    Width = 75
    Height = 28
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlColor: TPanel
    Left = 114
    Top = 115
    Width = 193
    Height = 22
    Caption = 'Assign Color...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
    OnClick = pnlColorClick
  end
  object edGroupName: TEdit
    Left = 418
    Top = 17
    Width = 193
    Height = 24
    TabOrder = 1
  end
  object chkCaseSensitive: TCheckBox
    Left = 114
    Top = 86
    Width = 128
    Height = 17
    Caption = 'Case Sensitive'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 114
    Top = 51
    Width = 497
    Height = 24
    ReadOnly = True
    TabOrder = 6
  end
  object chbRegExp: TCheckBox
    Left = 306
    Top = 86
    Width = 143
    Height = 17
    Caption = 'Regular Expression'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object ColorDialog1: TColorDialog
    Left = 128
    Top = 118
  end
end
