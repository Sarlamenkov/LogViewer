unit ViewFrame;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  VirtualTrees, ExtCtrls, StdCtrls, Buttons, Clipbrd,

  StructsUnit, Structs2Unit, View2Frame;

type
  TViewFrm = class(TFrame)
    pnlWork: TPanel;
    vtFullLog: TVirtualStringTree;
    pnl1: TPanel;
    lbl1: TLabel;
    edtSearch: TEdit;
    btnFindNext: TButton;
    pnl2: TPanel;
    pb1: TPaintBox;
    chkFiltered: TCheckBox;
    btn1: TButton;
    pnl3: TPanel;
    spl1: TSplitter;
    vtLog: TVirtualStringTree;
    pnl4: TPanel;
    pnl6: TPanel;
    btnFindPrev: TButton;
    chkTwoWindows: TCheckBox;
    pnl7: TPanel;
    lblCount: TLabel;
    vrtlstrngtrFullLog2: TVirtualStringTree;
    spl2: TSplitter;
    pnl8: TPanel;
    View2Frm1: TView2Frm;
    procedure vtFullLogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vtFullLogEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vtFullLogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtSearchChange(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkFilteredClick(Sender: TObject);
    procedure vtFullLogGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure btn1Click(Sender: TObject);
    procedure vtFullLogBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure vtFullLogDblClick(Sender: TObject);
    procedure pb1Paint(Sender: TObject);
    procedure vtLogEnter(Sender: TObject);
    procedure chkTwoWindowsClick(Sender: TObject);
    procedure vtLogClick(Sender: TObject);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOriginRows: TMyStringList;
    FFiltered: Boolean;
    FFilteredRowCount: Integer;
    FFilteredRows: array of Integer;
    FTags: TTagList;
    FFileName: string;
    FFindNextNode: PVirtualNode;
    FFindWindow: TVirtualStringTree;
    FSelectedWord: string;
    FDataList: TDataList;
    function FindOriginNodeByText(const AFromNode: PVirtualNode; const AText: string): PVirtualNode;
    procedure DoFilter;
    function GetText(Sender: TBaseVirtualTree; Column: TColumnIndex; NodeIndex: Integer): string;
    function GetRowNumber(index: Integer): Integer;
    function GetNodeByIndex(Sender: TVirtualStringTree; ind: Integer): PVirtualNode;
    procedure UpdateMarks;
    procedure UpdateCountLabel;
  public
    procedure Init(const AFileName: string; const ATags: TTagList);
    procedure Deinit;
    procedure Actualize;
    procedure SwitchFilter;
    property FileName: string read FFileName;
  end;

implementation

{$R *.dfm}

uses
  StrUtils, uGraphicUtils;

const
  cSelectableSymbols = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_0123456789';

{ TViewFrm }

procedure TViewFrm.Deinit;
begin
  if FOriginRows = nil then Exit;
  vtFullLog.RootNodeCount := 0;
  vrtlstrngtrFullLog2.RootNodeCount := 0;
  FreeAndNil(FOriginRows);
end;

procedure TViewFrm.Init(const AFileName: string; const ATags: TTagList);
var
  vTime: Cardinal;
begin
  Deinit;

  FFileName := AFileName;
  FTags := ATags;
  FOriginRows := TMyStringList.Create;
  FOriginRows.LoadFromFile(AFileName);

  FFindNextNode := nil;
  vtLog.Font.Name := Options.FontName;
  vtLog.Font.Size := Options.FontSize;
  vtFullLog.Font.Name := Options.FontName;
  vtFullLog.Font.Size := Options.FontSize;
  vrtlstrngtrFullLog2.Font.Name := Options.FontName;
  vrtlstrngtrFullLog2.Font.Size := Options.FontSize;
  chkTwoWindows.Checked := Options.TwoWindow;

  Actualize;

  View2Frm1.Init(AFileName);
 { vTime := GetTickCount;
  FDataList := TDataList.Create;
  FDataList.LoadFromFile(AFileName);
  Application.MessageBox(PAnsiChar(IntToStr(GetTickCount - vTime)), '');
  Assert(FDataList.RowCount = FOriginRows.Count);   }
//  Assert(FDataList.FilteredRowCount = FFilteredRowCount, IntToStr(FDataList.FilteredRowCount) + ' ' + IntToStr(FFilteredRowCount));
end;

procedure TViewFrm.Actualize;
var
  vPrevCursor: TCursor;
begin
  vPrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    DoFilter;
  finally
    Screen.Cursor := vPrevCursor;
  end;
end;

function TViewFrm.GetText(Sender: TBaseVirtualTree; Column: TColumnIndex; NodeIndex: Integer): string;
var
  row, w: Integer;
begin
  if Sender = vtLog then
    row := NodeIndex
  else
    row := GetRowNumber(NodeIndex);
  if Column = 0 then
    Result := IntToStr(row + 1)
  else
    Result := FOriginRows[row];
  w := TVirtualStringTree(Sender).Canvas.TextWidth(Result) + 20;
  if TVirtualStringTree(Sender).Header.Columns.Items[Column].Width < w then
    TVirtualStringTree(Sender).Header.Columns.Items[Column].Width := w;
end;

procedure TViewFrm.vtFullLogGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  CellText := GetText(Sender, Column, Node.Index);
end;

procedure TViewFrm.vtFullLogBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  i, vPos, vMargin: Integer;
  vTag: TTagInfo;
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
  for i := 0 to FTags.Count - 1 do
  begin
    vTag := FTags[i];
    if (vTag.Enabled) then
      DrawSelection(vTag.Color, vTag.Name);
  end;

  DrawSelection(clGray, edtSearch.Text);
  DrawSelection(clMaroon, FSelectedWord);
end;

procedure TViewFrm.vtFullLogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  vLog: TVirtualStringTree;
  vSelection: TVTVirtualNodeEnumerator;
  vText: TStrings;
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    vLog := TVirtualStringTree(Sender);
    vSelection := vLog.SelectedNodes.GetEnumerator;
    vText := TStringList.Create;
    try
      while vSelection.MoveNext do
        vText.Add(GetText(TBaseVirtualTree(Sender), 0, vSelection.Current.Index)+#9+GetText(TBaseVirtualTree(Sender), 1, vSelection.Current.Index));
      Clipboard.Open;
      try
        Clipboard.AsText := vText.Text;
      finally
        Clipboard.Close;
      end;
    finally
      vSelection.Free;
      vText.Free;
    end;
  end
  else if Key = VK_F2 then
    if Assigned(TVirtualStringTree(Sender).FocusedNode) then
      TVirtualStringTree(Sender).EditNode(TVirtualStringTree(Sender).FocusedNode, 1);
end;

procedure TViewFrm.vtFullLogEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  if Column = 0 then
    Allowed := False;
end;

procedure TViewFrm.vtFullLogMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    if Assigned(TVirtualStringTree(Sender).FocusedNode) then
      TVirtualStringTree(Sender).EditNode(TVirtualStringTree(Sender).FocusedNode, 1);
end;

procedure TViewFrm.edtSearchChange(Sender: TObject);
begin
  vtFullLog.Invalidate;
  vrtlstrngtrFullLog2.Invalidate;
end;

function TViewFrm.FindOriginNodeByText(const AFromNode: PVirtualNode;
  const AText: string): PVirtualNode;
var
  vNode: PVirtualNode;
  vPos, row: Integer;
begin
  Result := nil;
  vNode := AFromNode;
  while Assigned(vNode) do
  begin
    if FFindWindow = vtLog then
      row := vNode.Index
    else
      row := GetRowNumber(vNode.Index);
    if Options.CaseSens then
      vPos := Pos(AText, FOriginRows[row])
    else
      vPos := Pos(UpperCase(AText), UpperCase(FOriginRows[row]));

    if vPos > 0 then
    begin
      Result := vNode;
      Break;
    end;
    vNode := vNode.NextSibling;
  end;
end;

procedure TViewFrm.btnFindNextClick(Sender: TObject);
begin
  if Length(Trim(edtSearch.Text)) = 0 then Exit;
  if (FFindWindow = nil) or (FFindWindow.Visible = False) then
    FFindWindow := vtFullLog;
  FFindNextNode := FFindWindow.FocusedNode;
  if FFindNextNode = nil then
    FFindNextNode := FFindWindow.GetFirst
  else
    FFindNextNode := FFindWindow.GetNext(FFindNextNode);

  FFindWindow.FocusedNode := FindOriginNodeByText(FFindNextNode, edtSearch.Text);
  FFindWindow.ClearSelection;
  if FFindWindow.FocusedNode <> nil then
  begin
    FFindWindow.ScrollIntoView(FFindWindow.FocusedNode, true);
    FFindWindow.Selected[FFindWindow.FocusedNode] := true;
    FFindWindow.Invalidate;
    if FFindWindow.CanFocus then
      FFindWindow.SetFocus;
  end;
end;

procedure TViewFrm.edtSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_F3) then
    btnFindNextClick(nil);
end;

procedure TViewFrm.DoFilter;
var
  i, rowNumber: Integer;
  curNode: PVirtualNode;
  tagInfo: TTagInfo;
begin
// Save focused row number
  rowNumber := 0;
  if Assigned(vtFullLog.FocusedNode) then
    rowNumber := GetRowNumber(vtFullLog.FocusedNode.Index);

  FFilteredRowCount := 0;
  FTags.Actualize;
  if chkFiltered.Checked then
  begin
    SetLength(FFilteredRows, FOriginRows.Count);
    for i := 0 to FOriginRows.Count - 1 do
    begin
      tagInfo := FTags.IsMatch(FOriginRows[i]);
      if tagInfo <> nil then
      begin
        tagInfo.Matched;      
        FOriginRows.Objects[i] := Pointer(ColorToRGB(tagInfo.Color) or $FF000000);
        FFilteredRows[FFilteredRowCount] := i;
        Inc(FFilteredRowCount);
      end
      else
        FOriginRows.Objects[i] := Pointer($00000000);
    end;
    vtFullLog.RootNodeCount := FFilteredRowCount;
    vrtlstrngtrFullLog2.RootNodeCount := FFilteredRowCount;
  end
  else
  begin
    vtFullLog.RootNodeCount := FOriginRows.Count;
    vrtlstrngtrFullLog2.RootNodeCount := FFilteredRowCount;
  end;
    
  SetLength(FFilteredRows, FFilteredRowCount);
  FFiltered := chkFiltered.Checked;

// Restore focus to row number
  curNode := vtFullLog.GetFirst;
  for i := 0 to vtFullLog.RootNodeCount - 1 do
  begin
    if GetRowNumber(curNode.Index) = rowNumber then Break;
    curNode := curNode.NextSibling;
  end;
  vtFullLog.FocusedNode := curNode;
  vtFullLog.ClearSelection;
  vtFullLog.Selected[curNode] := True;
  vtFullLog.ScrollIntoView(curNode, True);
  vtFullLog.Invalidate;
  if vtFullLog.CanFocus then
    vtFullLog.SetFocus;

  if vtLog.Visible then
    vtLog.Invalidate;

  UpdateMarks;
  UpdateCountLabel;
end;

procedure TViewFrm.chkFilteredClick(Sender: TObject);
begin
  DoFilter;
end;

function TViewFrm.GetRowNumber(index: Integer): Integer;
begin
  if FFiltered then
    Result := FFilteredRows[index]
  else
    Result := index;
end;

procedure TViewFrm.btn1Click(Sender: TObject);
var
  selection: TVTVirtualNodeEnumerator;
  startRow, endRow, i: Integer;
  node: PVirtualNode;
begin
  if vtFullLog.SelectedCount >= 2 then
  begin
    selection := vtFullLog.SelectedNodes.GetEnumerator;
    if selection.MoveNext then
    begin
      startRow := GetRowNumber(selection.Current.Index);
      while selection.MoveNext do
        node := selection.Current;
      endRow := GetRowNumber(node.Index);
      for i := FOriginRows.Count-1 downto endRow + 1 do
        FOriginRows.Delete(i);
      for i := startRow - 1 downto 0 do
        FOriginRows.Delete(i);
    end;
  end;
  DoFilter;
end;

procedure TViewFrm.UpdateMarks;
var
  i, pbWidth, pbHeight, rc, c, y: Integer;
begin
  pb1.Canvas.Lock;
  try
    pb1.Canvas.FillRect(pb1.ClientRect);
    pbHeight := pb1.Height;
    pbWidth := pb1.Width;
    rc := FOriginRows.Count;
    for i := 0 to rc - 1 do
    begin
      c := Integer(FOriginRows.Objects[i]);
      if c and $FF000000 > 0 then
      begin
        y := Round(i/rc*pbHeight);
        pb1.Canvas.Pen.Color := c and $00FFFFFF;
        pb1.Canvas.MoveTo(0, y);
        pb1.Canvas.LineTo(pbWidth, y);
      end;
    end;
  finally
    pb1.Canvas.Unlock;
  end;
end;

procedure TViewFrm.pb1Paint(Sender: TObject);
begin
  UpdateMarks;
end;

procedure TViewFrm.SwitchFilter;
begin
  chkTwoWindows.Checked := not chkTwoWindows.Checked;
end;

function TViewFrm.GetNodeByIndex(Sender: TVirtualStringTree; ind: Integer): PVirtualNode;
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

procedure TViewFrm.vtFullLogDblClick(Sender: TObject);
var
  vVST: TVirtualStringTree;
begin
  if not (Sender is TVirtualStringTree) then Exit;
  
  vVST := TVirtualStringTree(Sender);
  if vtLog.Visible and (vVST.FocusedNode <> nil) then
  begin
    vtLog.ClearSelection;
    vtLog.FocusedNode := GetNodeByIndex(vtLog, GetRowNumber(vVST.FocusedNode.Index));
    vtLog.Selected[vtLog.FocusedNode] := True;
    vtLog.ScrollIntoView(vtLog.FocusedNode, true);
    if vtLog.CanFocus then
      vtLog.SetFocus;
    vtLog.Invalidate;
  end;
end;

procedure TViewFrm.vtLogEnter(Sender: TObject);
begin
  FFindWindow := TVirtualStringTree(Sender);
end;

procedure TViewFrm.chkTwoWindowsClick(Sender: TObject);
begin
  Options.TwoWindow := chkTwoWindows.Checked;
  vtLog.Visible := Options.TwoWindow;
  spl1.Visible := Options.TwoWindow;
  spl1.Top := vtLog.Top + vtLog.Height + 2;
  if vtLog.Visible then
  begin
    vtLog.RootNodeCount := FOriginRows.Count;
    vtLog.Invalidate;
  end
  else
    vtLog.RootNodeCount := 0;
end;

procedure TViewFrm.UpdateCountLabel;
begin
  lblCount.Caption := 'Rows: ';
  if FFiltered then
    lblCount.Caption := lblCount.Caption + IntToStr(FFilteredRowCount) + ' / ' + IntToStr(FOriginRows.Count)
  else
    lblCount.Caption := lblCount.Caption + IntToStr(FOriginRows.Count);
end;

procedure TViewFrm.vtLogClick(Sender: TObject);
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
    vtFullLog.Invalidate;
  end;   
end;

procedure TViewFrm.pb1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vCount: Integer;
  vCurNode: PVirtualNode;
begin
  vCount := FOriginRows.Count;
  if FFiltered then
    vCount := FFilteredRowCount;
  vCurNode := GetNodeByIndex(vtFullLog, Trunc(vCount * (Y/pb1.Height)));
 // vtFullLog.Selected[vCurNode] := True;
  vtFullLog.FocusedNode := vCurNode;
  vtFullLog.ScrollIntoView(vCurNode, True);
  vtFullLogDblClick(vtFullLog);
end;

end.
