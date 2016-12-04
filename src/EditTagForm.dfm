object EditTagFm: TEditTagFm
  Left = 497
  Top = 293
  BorderStyle = bsDialog
  Caption = 'Edit Tag'
  ClientHeight = 136
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 14
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 24
    Top = 38
    Width = 60
    Height = 13
    Caption = 'Group Name'
  end
  object lbl1: TLabel
    Left = 24
    Top = 64
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object edtName: TEdit
    Left = 104
    Top = 8
    Width = 193
    Height = 21
    TabOrder = 0
    OnChange = edtNameChange
  end
  object btnOk: TButton
    Left = 72
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 160
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlColor: TPanel
    Left = 104
    Top = 62
    Width = 106
    Height = 22
    Caption = 'Assign Color...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = pnlColorClick
  end
  object edGroupName: TEdit
    Left = 104
    Top = 32
    Width = 193
    Height = 21
    TabOrder = 1
  end
  object ColorDialog1: TColorDialog
    Left = 264
    Top = 64
  end
end
