unit View2Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, VirtualTrees, StdCtrls,
  
  Structs2Unit, ComCtrls, ToolWin, CheckLst, TagListFrame;

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
    spl3: TSplitter;
    tl1: TTagListFrm;
    pb2: TProgressBar;
    procedure vtLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vtLogBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure vtFilteredLogDblClick(Sender: TObject);
    procedure pb1Paint(Sender: TObject);
    procedure vtLogClick(Sender: TObject);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFileName: string;
    FDataList: TDataList;
    FSelectedWord: string;
    function GetText(Sender: TBaseVirtualTree; Column: TColumnIndex; NodeIndex: Integer): string;
    procedure OnChangeTags(Sender: TObject);
    procedure OnLoaded(Sender: TObject);
    procedure OnLoading(const APercent: Byte);
    function GetNodeByIndex(Sender: TVirtualStringTree;
      ind: Integer): PVirtualNode;
    procedure UpdateMarks;
    procedure UpdateCountLabel;
  public
    procedure Init(const AFileName: string);
    procedure Deinit;
    
    property FileName: string read FFileName;
  end;

implementation

{$R *.dfm}

uses
  StrUtils, StructsUnit, uGraphicUtils;

const
  cSelectableSymbols = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_0123456789';
    
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
  FFileName := AFileName;
  FDataList := TDataList.Create;
  FDataList.OnChanged := OnChangeTags;
  FDataList.OnLoading := OnLoading;
  FDataList.OnLoaded := OnLoaded;
  FDataList.LoadFromFile(AFileName);
  tl1.Init(FDataList.TagList);

  
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
  try
    vText := GetText(Sender, Column, Node.Index);
  except
    ShowMessage(IntToStr(Column) + ' ' + IntToStr(Node.Index));
  end;
  vRect := CellRect;
  if (Node.Index mod 2 = 0) and (Column <> 0) then
  begin
    TargetCanvas.Brush.Color := CalcBrightColor(clSilver, 80);
    TargetCanvas.FillRect(vRect);
  end;
  vMargin := TVirtualStringTree(Sender).Margin + TVirtualStringTree(Sender).TextMargin;
  for i := 0 to FDataList.TagCount - 1 do
  begin
    vTag := FDataList.Tags[i];
    if (vTag.Enabled) then
      DrawSelection(vTag.Color, vTag.Name);
  end;

  DrawSelection(clGray, edtSearch.Text);
  DrawSelection(clMaroon, FSelectedWord);
end;

procedure TView2Frm.OnChangeTags(Sender: TObject);
begin
  vtLog.RootNodeCount := 0;
  vtLog.RootNodeCount := FDataList.RowCount;
  vtFilteredLog.RootNodeCount := 0;
  vtFilteredLog.RootNodeCount := FDataList.FilteredRowCount;
  UpdateMarks;
  UpdateCountLabel;
end;

procedure TView2Frm.OnLoading(const APercent: Byte);
begin
  pb2.Position := APercent;
end;

function TView2Frm.GetNodeByIndex(Sender: TVirtualStringTree; ind: Integer): PVirtualNode;
var
  node: PVirtualNode;
  i: Integer;
begin
  node := Sender.GetFirst();
  for i := 1 to ind do
  begin
    if node = nil then Break;
    node := node.NextSibling;
  end;
  Result := node;
end;

procedure TView2Frm.vtFilteredLogDblClick(Sender: TObject);
var
  vVST: TVirtualStringTree;
begin
  if not (Sender is TVirtualStringTree) then Exit;
  
  vVST := TVirtualStringTree(Sender);
  if vtLog.Visible and (vVST.FocusedNode <> nil) then
  begin
    vtLog.ClearSelection;
    vtLog.FocusedNode := GetNodeByIndex(vtLog, FDataList.GetFilteredRowNumber(vVST.FocusedNode.Index));
    vtLog.Selected[vtLog.FocusedNode] := True;
    vtLog.ScrollIntoView(vtLog.FocusedNode, true);
    if vtLog.CanFocus then
      vtLog.SetFocus;
    vtLog.Invalidate;
  end;
end;

procedure TView2Frm.UpdateMarks;
var
  i, pbWidth, pbHeight, rc, j, y: Integer;
  vTag: TTagInfo2;
begin
  pb1.Canvas.Lock;
  try
    pb1.Canvas.FillRect(pb1.ClientRect);
    pbHeight := pb1.Height;
    pbWidth := pb1.Width;
    rc := FDataList.RowCount;
    for i := 0 to FDataList.TagCount - 1 do
    begin
      vTag := FDataList.Tags[i];
      if vTag.Enabled then
      begin
        for j := 0 to vTag.MatchCount - 1 do
        begin
          y := Round(vTag.MatchRows[j]/rc*pbHeight);
          pb1.Canvas.Pen.Color := vTag.Color;
          pb1.Canvas.MoveTo(0, y);
          pb1.Canvas.LineTo(pbWidth, y);
        end;
      end;
    end;
  finally
    pb1.Canvas.Unlock;
  end;
end;

procedure TView2Frm.UpdateCountLabel;
begin
  lblCount.Caption := 'Rows: ' +
    IntToStr(FDataList.FilteredRowCount) + ' / ' + IntToStr(FDataList.RowCount);
end;

procedure TView2Frm.pb1Paint(Sender: TObject);
begin
  UpdateMarks;
end;

procedure TView2Frm.OnLoaded(Sender: TObject);
begin
  UpdateMarks;
  UpdateCountLabel;
end;

procedure TView2Frm.vtLogClick(Sender: TObject);
var
  vVST: TVirtualStringTree;
  vNode: PVirtualNode;
  vRowText: WideString;
  vPos: TPoint;
  vCellRect: TRect;
//  vMargin: Integer;
  function TextAt(const AX: Integer): string;
  var
    c, i: Integer;
    vStr: string;
  begin
    c := 0; i := 1; vStr := '';
    while c < AX do
    begin
      vStr := vStr + vRowText[i];
      c := vVST.Canvas.TextWidth(vStr);
      Inc(i);
    end;
    Dec(i);
    while (i > 1) and (Pos(vRowText[i], cSelectableSymbols) > 0) do
      Dec(i);
    Result := Copy(vRowText, i + 1, Length(vRowText)); // cut from left
    i := 1;
    while (i < (Length(Result) + 1)) and (Pos(Result[i], cSelectableSymbols) > 0) do
      Inc(i);
    Result := Copy(Result, 0, i - 1); // cut from right
  end;
begin
  if not (Sender is TVirtualStringTree) then Exit;
  
  vVST := TVirtualStringTree(Sender);
  vPos := vVST.ScreenToClient(Mouse.CursorPos);
  vNode := vVST.GetNodeAt(vPos.X, vPos.Y);
  try
    if vNode = nil then Exit;
    vVST.GetTextInfo(vNode, 1, vVST.Font, vCellRect, vRowText);
  //  vMargin := vVST.Margin + vVST.TextMargin;
    if Length(vRowText) = 0 then Exit;
    FSelectedWord := TextAt(vPos.X - vCellRect.Left);
  finally
    vtLog.Invalidate;
    vtFilteredLog.Invalidate;
  end;
end;  

procedure TView2Frm.pb1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vCurNode: PVirtualNode;
begin
  vCurNode := GetNodeByIndex(vtLog, Trunc(FDataList.RowCount * (Y/pb1.Height)));
  vtLog.FocusedNode := vCurNode;
  vtLog.ScrollIntoView(vCurNode, True);
end;

end.
