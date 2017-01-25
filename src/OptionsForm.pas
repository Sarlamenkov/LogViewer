unit OptionsForm;

interface

uses
  Classes, Controls, Forms,
  StdCtrls,

  Structs2Unit, Dialogs, Graphics;

type
  TOptionsFm = class(TForm)
    btnOk: TButton;
    chkCaseSensitive: TCheckBox;
    chkTwoWindow: TCheckBox;
    dlgFont1: TFontDialog;
    lbl1: TLabel;
    btnSelect: TButton;
    chkSaveOnExit: TCheckBox;
    procedure btnSelectClick(Sender: TObject);
  private
    procedure SetFont(AFont: TFont);
    { Private declarations }
  public
    function Edit: Boolean;
  end;

var
  OptionsFm: TOptionsFm;

implementation

uses SysUtils;

{$R *.dfm}

{ TOptionsFm }

function TOptionsFm.Edit: Boolean;
begin
  dlgFont1.Font.Name := Options.FontName;
  dlgFont1.Font.Size := Options.FontSize;
  SetFont(dlgFont1.Font);
  chkCaseSensitive.Checked := Options.CaseSens;
  chkTwoWindow.Checked := Options.TwoWindow;
  chkSaveOnExit.Checked := Options.SaveOnExit;
  Result := ShowModal = mrOk;
  if Result then
  begin
    Options.FontName := dlgFont1.Font.Name;
    Options.FontSize := dlgFont1.Font.Size;
    Options.CaseSens := chkCaseSensitive.Checked;
    Options.TwoWindow := chkTwoWindow.Checked;
    Options.SaveOnExit := chkSaveOnExit.Checked;
 //   ShowMessage('The changes will be applied after program restart');
  end;
end;

procedure TOptionsFm.SetFont(AFont: TFont);
begin
  lbl1.Caption := AFont.Name + ', ' + IntToStr(AFont.Size);
  lbl1.Font.Name := AFont.Name;
  lbl1.Font.Size := AFont.Size;
end;

procedure TOptionsFm.btnSelectClick(Sender: TObject);
begin
  if dlgFont1.Execute then
    SetFont(dlgFont1.Font);
end;

end.
