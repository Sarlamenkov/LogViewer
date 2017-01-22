unit View2Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, VirtualTrees, StdCtrls,
  
  Structs2Unit;

type
  TView2Frm = class(TFrame)
    pnlWork: TPanel;
    pnl1: TPanel;
    lbl1: TLabel;
    edtSearch: TEdit;
    btnFindNext: TButton;
    chkFiltered: TCheckBox;
    btnFindPrev: TButton;
    chkTwoWindows: TCheckBox;
    pnl2: TPanel;
    spl1: TSplitter;
    pnl3: TPanel;
    pnl8: TPanel;
    spl2: TSplitter;
    vtFilteredLog2: TVirtualStringTree;
    vtFilteredLog: TVirtualStringTree;
    vtLog: TVirtualStringTree;
    pnl4: TPanel;
    pb1: TPaintBox;
    pnl6: TPanel;
    pnl7: TPanel;
    lblCount: TLabel;
    procedure vtLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vtLogBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
  private
    FFileName: string;
    FDataList: TDataList;
    function GetText(Sender: TBaseVirtualTree; Column: TColumnIndex; NodeIndex: Integer): string;
  public
    procedure Init(const AFileName: string);
    procedure Deinit;
    
    property FileName: string read FFileName;
  end;

implementation

{$R *.dfm}

uses
  StrUtils, StructsUnit, uGraphicUtils;
    
{ TView2Frm }

procedure TView2Frm.Deinit;
begin
  FreeAndNil(FDataList);
end;

function TView2Frm.GetText(Sender: TBaseVirtualTree; Column: TColumnIndex;
  NodeIndex: Integer): string;
var
  w: Integer;
begin
  if Sender = vtLog then
  begin
    if Column = 0 then
      Result := IntToStr(NodeIndex + 1)
    else
      Result := FDataList.Rows[NodeIndex];
  end
  else
  begin
    if Column = 0 then
      Result := IntToStr(FDataList.GetFilteredRowNumber(NodeIndex) + 1)
    else
      Result := FDataList.FilteredRows[NodeIndex];
  end;
  w := TVirtualStringTree(Sender).Canvas.TextWidth(Result) + 20;
  if TVirtualStringTree(Sender).Header.Columns.Items[Column].Width < w then
    TVirtualStringTree(Sender).Header.Columns.Items[Column].Width := w;
end;

procedure TView2Frm.Init(const AFileName: string);
begin
  Deinit;
  FDataList := TDataList.Create;
  FDataList.LoadFromFile(AFileName);
  vtLog.RootNodeCount := 0;
  vtLog.RootNodeCount := FDataList.RowCount;
  vtFilteredLog.RootNodeCount := 0;
  vtFilteredLog.RootNodeCount := FDataList.FilteredRowCount;
  
  vtLog.Font.Name := Options.FontName;
  vtLog.Font.Size := Options.FontSize;
  vtFilteredLog.Font.Name := Options.FontName;
  vtFilteredLog.Font.Size := Options.FontSize;
  vtFilteredLog2.Font.Name := Options.FontName;
  vtFilteredLog2.Font.Size := Options.FontSize;
  chkTwoWindows.Checked := Options.TwoWindow;
end;

procedure TView2Frm.vtLogGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  CellText := GetText(Sender, Column, Node.Index);
end;

procedure TView2Frm.vtLogBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  i, vPos, vMargin: Integer;
  vTag: TTagInfo2;
  vText, vBeforeTag: string;
  vRect: TRect;
  procedure DrawSelection(const AColor: TColor; const ASelText: string);
  begin
    if Length(ASelText) > 0 then
    begin
      if Options.CaseSens then
        vPos := Pos(ASelText, vText)
      else
        vPos := Pos(UpperCase(ASelText), UpperCase(vText));
      while vPos > 0 do
      begin
        vBeforeTag := Copy(vText, 0, vPos - 1);
        vRect.Left := vMargin + CellRect.Left + TargetCanvas.TextWidth(vBeforeTag);
        vRect.Right := vRect.Left + TargetCanvas.TextWidth(Copy(vText, Length(vBeforeTag)+1, Length(ASelText))) + 1;
        TargetCanvas.Brush.Color := CalcBrightColor(AColor, 80);
        TargetCanvas.Pen.Color := AColor;
        TargetCanvas.RoundRect(vRect.Left, vRect.Top + 1, vRect.Right, vRect.Bottom - 1, 5, 5);
//        FillGradientRoundRect(TargetCanvas, vRect, CalcBrightColor(AColor, 90), CalcBrightColor(AColor, 70), AColor);
        if Options.CaseSens then
          vPos := PosEx(ASelText, vText, vPos + 1)
        else
          vPos := PosEx(UpperCase(ASelText), UpperCase(vText), vPos + 1);
      end;
    end;
  end;
begin
  vText := GetText(Sender, Column, Node.Index);
  vRect := CellRect;
  vMargin := TVirtualStringTree(Sender).Margin + TVirtualStringTree(Sender).TextMargin;
  for i := 0 to FDataList.TagCount - 1 do
  begin
    vTag := FDataList.Tags[i];
    if (vTag.Enabled) then
      DrawSelection(vTag.Color, vTag.Name);
  end;

  DrawSelection(clGray, edtSearch.Text);
 // DrawSelection(clMaroon, FSelectedWord);
end;

end.
