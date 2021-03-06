unit TagListDefaultForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TagListFrame, StdCtrls, ExtCtrls;

type
  TTagListDefaultFm = class(TForm)
    pnl1: TPanel;
    btn1: TButton;
    tl1: TTagListFrm;
  private
    { Private declarations }
  public
    procedure Edit;
  end;

var
  TagListDefaultFm: TTagListDefaultFm;

implementation

{$R *.dfm}

uses
  uStructs;

{ TTagListDefaultFm }

procedure TTagListDefaultFm.Edit;
var
  vTagList: TTagList;
begin
  vTagList := TTagList.Create;
  vTagList.Load('tags');
  tl1.Init2(vTagList);
  if ShowModal <> mrOk then Exit;
  vTagList.Save;
end;

end.
