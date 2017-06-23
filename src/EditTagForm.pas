unit EditTagForm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  uStructs;

type
  TEditTagFm = class(TForm)
    edtName: TEdit;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlColor: TPanel;
    Label2: TLabel;
    edGroupName: TEdit;
    lbl1: TLabel;
    chkCaseSensitive: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtNameChange(Sender: TObject);
    procedure pnlColorClick(Sender: TObject);
  private
    procedure UpdateView;
    procedure SetTagColor(cl: TColor);
  public
    function Edit(const ATag: TTagInfo): Boolean;

  end;

var
  EditTagFm: TEditTagFm;

implementation

{$R *.dfm}

procedure TEditTagFm.SetTagColor(cl: TColor);
begin
  pnlColor.Color := cl;
  if (GetRValue(cl)+GetGValue(cl)+GetBValue(cl))/3 > 128 then
    pnlColor.Font.Color := clBlack
  else
    pnlColor.Font.Color := clWhite;
end;

function TEditTagFm.Edit(const ATag: TTagInfo): Boolean;
begin
  edtName.Text := ATag.Name;
  SetTagColor(ATag.Color);
  edGroupName.Text := ATag.GroupName;
  chkCaseSensitive.Checked := ATag.CaseSens;
  Result := ShowModal = mrOk;
  if not Result then Exit;
  ATag.Name := edtName.Text ;
  ATag.Color := pnlColor.Color;
  ATag.GroupName := edGroupName.Text;
  ATag.CaseSens := chkCaseSensitive.Checked;
end;

procedure TEditTagFm.FormShow(Sender: TObject);
begin
  ActiveControl := edtName;
  UpdateView;
end;

procedure TEditTagFm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TEditTagFm.UpdateView;
begin
  btnOk.Enabled := Length(Trim(edtName.Text)) > 0;
end;

procedure TEditTagFm.edtNameChange(Sender: TObject);
begin
  UpdateView;
end;

procedure TEditTagFm.pnlColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
    SetTagColor(ColorDialog1.Color);
end;

end.
