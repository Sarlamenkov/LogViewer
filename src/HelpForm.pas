unit HelpForm;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  THelpFm = class(TForm)
    btnClose: TButton;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HelpFm: THelpFm;

implementation

{$R *.dfm}

end.
