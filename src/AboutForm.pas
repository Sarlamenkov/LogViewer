unit AboutForm;

interface

uses Classes, Forms, Controls, StdCtrls,
  ExtCtrls;

type
  TAboutFm = class(TForm)
    Panel1: TPanel;
    ProductName: TLabel;
    Version: TLabel;
    OKButton: TButton;
    lbl1: TLabel;
    lblVersion: TLabel;
    lbl2: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutFm: TAboutFm;

implementation

uses
  uConsts;

{$R *.dfm}

procedure TAboutFm.FormShow(Sender: TObject);
begin
  lblVersion.Caption := GetFileVersion;
end;

end.
 
