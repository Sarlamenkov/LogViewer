unit LogViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees;

type
  TForm1 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    VirtualStringTree2: TVirtualStringTree;
    VirtualStringTree3: TVirtualStringTree;
    pnlLeft: TPanel;
    pnlWork: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
