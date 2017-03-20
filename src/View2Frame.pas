unit View2Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, VirtualTrees, StdCtrls,
  
  uStructs, ComCtrls, ToolWin, CheckLst, TagListFrame, System.Actions,
  Vcl.ActnList;

type
  TView2Frm = class(TFrame)
    pnlWork: TPanel;
    pnlTools: TPanel;
    lbl1: TLabel;
    edtSearch: TEdit;
    btnFindNext: TButton;
    chkFiltered: TCheckBox;
    btnFindPrev: TButton;
    chkTwoWindows: TCheckBox;
    splVerticalLogs: TSplitter;
    pnlLog: TPanel;
    pnlFiltered: TPanel;
    splFiltered: TSplitter;
    vtFilteredLog2: TVirtualStringTree;
    vtFilteredLog: TVirtualStringTree;
    vtLog: TVirtualStringTree;
    pnlMarks: TPanel;
    pb1: TPaintBox;
    pnlBottom: TPanel;
    lblCount: TLabel;
    spl3: TSplitter;
    tl1: TTagListFrm;
    pb2: TProgressBar;
    pnlFull: TPanel;
    vtLog2: TVirtualStringTree;
    splFullLog: TSplitter;
    ActionList1: TActionList;
    act2windows: TAction;
    actFiltered: TAction;
    pnlBase: TPanel;
    procedure vtLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtLogBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure vtFilteredLogDblClick(Sender: TObject);
    procedure pb1Paint(Sender: TObject);
    procedure GetSelectedWord(VST: TObject; const ANew: Boolean);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure act2windowsExecute(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure vtLogEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vtLogEnter(Sender: TObject);
    procedure vtFilteredLogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vtLogMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actFilteredExecute(Sender: TObject);
    procedure vtLogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFileName: string;
    FDataList: TDataList;
    FSortedTags: TList;
    FSelectedWords: TStrings;
    FFindWindow: TVirtualStringTree;
    FFindNextNode: PVirtualNode;
    function GetText(Sender: TBaseVirtualTree; Column: TColumnIndex; NodeIndex: Integer): string;
    procedure OnChangeTags(Sender: TObject);
    procedure OnLoaded(Sender: TObject);
    procedure OnLoading(const APercent: Byte);
    function GetNodeByIndex(Sender: TVirtualStringTree; ind: Integer): PVirtualNode;
    procedure UpdateMarks;
    procedure UpdateCountLabel;
    function FindOriginNodeByText(const AFromNode: PVirtualNode;
      const AText: string): PVirtualNode;
    procedure GoToNode(const AVST: TVirtualStringTree; const ARowNum: Integer);
  public
    procedure Init(const AFileName: string);
    procedure Deinit;

    procedure AddTagFromSelection;
    property FileName: string read FFileName;
  end;

implementation

{$R *.dfm}

uses
  Clipbrd, StrUtils, Math,

  uGraphicUtils, uConsts;

const
  cSelectableSymbols = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_0123456789';
    
{ TView2Frm }

procedure TView2Frm.act2windowsExecute(Sender: TObject);
begin
  LockControl(pnlWork, True);
  try
    chkFiltered.Visible := not act2windows.Checked;
    splVerticalLogs.Visible := act2windows.Checked;
    pnlFiltered.Visible := act2windows.Checked;
    pnlFull.Visible := True;
    if pnlFiltered.Visible then
    begin
      pnlFull.Align := alTop;
      pnlFull.Height := 300;
      splVerticalLogs.Top := pnlFull.Height + 1;
    end
    else
      pnlFull.Align := alClient;
  finally
    LockControl(pnlWork, False);
  end;
end;

procedure TView2Frm.actFilteredExecute(Sender: TObject);
begin
  LockControl(pnlWork, True);
  try
    pnlFiltered.Visible := chkFiltered.Checked;
    pnlFull.Visible := not chkFiltered.Checked;
  finally
    LockControl(pnlWork, False);
  end;
end;

procedure TView2Frm.AddTagFromSelection;
var
  vTI: TTagInfo;
begin
  if FSelectedWords.Count = 0 then Exit;
  vTI := TTagInfo.Create(FSelectedWords[0], True, clSkyBlue, 'Temp');
  FDataList.TagList.Add(vTI);
  tl1.UpdateLists;
end;

procedure TView2Frm.btnFindNextClick(Sender: TObject);
begin
  if Length(Trim(edtSearch.Text)) = 0 then Exit;

  if (FFindWindow = nil) or (FFindWindow.Visible = False) then
    FFindWindow := vtLog;
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

function TView2Frm.FindOriginNodeByText(const AFromNode: PVirtualNode;
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
      row := FDataList.GetFilteredRowNumber(vNode.Index);
 //   if Options.CaseSens then
 //     vPos := Pos(AText, FDataList.Rows[row])
 //   else
      vPos := Pos(UpperCase(AText), UpperCase(FDataList.Rows[row]));

    if vPos > 0 then
    begin
      Result := vNode;
      Break;
    end;
    vNode := vNode.NextSibling;
  end;
end;

procedure TView2Frm.Deinit;
begin
  vtLog.RootNodeCount := 0;
  vtLog2.RootNodeCount := 0;
  vtFilteredLog.RootNodeCount := 0;
  vtFilteredLog2.RootNodeCount := 0;
  tl1.Deinit;
  FreeAndNil(FDataList);
  FreeAndNil(FSelectedWords);
  FreeAndNil(FSortedTags);
end;

function TView2Frm.GetText(Sender: TBaseVirtualTree; Column: TColumnIndex;
  NodeIndex: Integer): string;
var
  w: Integer;
begin
  if Column < 0 then Exit;

  if (Sender = vtLog) or (Sender = vtLog2) then
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
  FSortedTags := TList.Create;
  FDataList.OnChanged := OnChangeTags;
  FDataList.OnLoading := OnLoading;
  FDataList.OnLoaded := OnLoaded;
  FDataList.LoadFromFile(AFileName);
  tl1.Init(FDataList);
  FSelectedWords := TStringList.Create;

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
  var CellText: string);
begin
  CellText := GetText(Sender, Column, Node.Index);
end;

procedure TView2Frm.vtLogMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    GetSelectedWord(Sender, not (ssAlt in Shift));
end;

procedure TView2Frm.vtLogMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    if Assigned(TVirtualStringTree(Sender).FocusedNode) then
      TVirtualStringTree(Sender).EditNode(TVirtualStringTree(Sender).FocusedNode, 1);
end;

procedure TView2Frm.vtLogBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  i, vPos, vMargin: Integer;
  vTag: TTagInfo;
  vRowText, vBeforeTag: string;
  vRect: TRect;
  procedure DrawSelection(const AColor: TColor; const ASelText: string;
    const AExactMatch: Boolean = False; const ACaseSens: Boolean = False);
  var
    vvNeedDrawSelection: Boolean;
    vvAfterEndChar: Integer;
  begin
    if Length(ASelText) > 0 then
    begin
      if ACaseSens then
        vPos := Pos(ASelText, vRowText)
      else
        vPos := Pos(UpperCase(ASelText), UpperCase(vRowText));
      while vPos > 0 do
      begin
        vvNeedDrawSelection := True;
        if AExactMatch then
        begin // проверяем, что перед и после подсвечиваемого слова есть невыбираемые символы
          vvAfterEndChar := vPos + Length(ASelText);
          if ((vPos > 1) and (Pos(vRowText[vPos-1], cSelectableSymbols) > 0)) or  // char before word
             ((Length(vRowText) > vvAfterEndChar) and (Pos(vRowText[vvAfterEndChar], cSelectableSymbols) > 0)) // char after word
          then
            vvNeedDrawSelection := False;
        end;

        if vvNeedDrawSelection then
        begin
          vBeforeTag := Copy(vRowText, 0, vPos - 1);
          vRect.Left := vMargin + CellRect.Left + TargetCanvas.TextWidth(vBeforeTag);
          vRect.Right := vRect.Left + TargetCanvas.TextWidth(Copy(vRowText, Length(vBeforeTag)+1, Length(ASelText))) + 1;
          TargetCanvas.Brush.Color := CalcBrightColor(AColor, 90);
          TargetCanvas.Pen.Color := AColor;
          TargetCanvas.RoundRect(vRect.Left, vRect.Top + 1, vRect.Right, vRect.Bottom - 1, 4, 4);
        end;

        if ACaseSens then
          vPos := PosEx(ASelText, vRowText, vPos + 1)
        else
          vPos := PosEx(UpperCase(ASelText), UpperCase(vRowText), vPos + 1);
      end;
    end;
  end;
begin
  if Column = 0 then Exit;

  try
    vRowText := GetText(Sender, Column, Node.Index);
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
    if (vTag.Enabled) and (vTag.MatchCount > 0) then
      DrawSelection(vTag.Color, vTag.Name, False, vTag.CaseSens);
  end;

  DrawSelection(clGray, edtSearch.Text);
  for i := 0 to FSelectedWords.Count - 1 do
    DrawSelection(clMaroon - 50*i, FSelectedWords[i], True);
end;

procedure TView2Frm.vtLogEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  if Column = 0 then
    Allowed := False;
end;

procedure TView2Frm.vtLogEnter(Sender: TObject);
begin
  FFindWindow := TVirtualStringTree(Sender);
end;

procedure TView2Frm.OnChangeTags(Sender: TObject);
  procedure RefreshLog(const AVST: TVirtualStringTree; const AFiltered: Boolean);
  var
    vRowNum: Integer;
    vNode: PVirtualNode;
    i, vIndex: Integer;
  begin
    vRowNum := -1;
    if Assigned(AVST.FocusedNode) then
      if AFiltered then
        vRowNum := FDataList.GetFilteredRowNumber(AVST.FocusedNode.Index)
      else
        vRowNum := AVST.FocusedNode.Index;

    AVST.RootNodeCount := 0;
    AVST.RootNodeCount := IfThen(AFiltered, FDataList.FilteredRowCount, FDataList.RowCount);

    if vRowNum < 0 then Exit;

    vNode := AVST.GetFirst;
    for i := 0 to AVST.RootNodeCount - 1 do
    begin
      if AFiltered then
        vIndex := FDataList.GetFilteredRowNumber(vNode.Index)
      else
        vIndex := vNode.Index;
      if vIndex = vRowNum then Break;
      vNode := vNode.NextSibling;
    end;
    AVST.FocusedNode := vNode;
    AVST.ClearSelection;
    AVST.Selected[vNode] := True;
    AVST.ScrollIntoView(vNode, True);
  end;
begin
  RefreshLog(vtLog, False);
  RefreshLog(vtLog2, False);
  RefreshLog(vtFilteredLog, True);
  RefreshLog(vtFilteredLog2, True);
  UpdateMarks;
  UpdateCountLabel;
end;

procedure TView2Frm.OnLoading(const APercent: Byte);
begin
  if not pb2.Visible then
    pb2.Visible := True;
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

procedure TView2Frm.GoToNode(const AVST: TVirtualStringTree; const ARowNum: Integer);
begin
  if AVST.Visible then
  begin
    AVST.ClearSelection;
    AVST.FocusedNode := GetNodeByIndex(AVST, ARowNum);
    AVST.Selected[AVST.FocusedNode] := True;
    AVST.ScrollIntoView(AVST.FocusedNode, True);
    if AVST.CanFocus then
      AVST.SetFocus;
    AVST.Invalidate;
  end;
end;

procedure TView2Frm.vtFilteredLogDblClick(Sender: TObject);
var
  vVST: TVirtualStringTree;
  vRowNum: Integer;
begin
  if not (Sender is TVirtualStringTree) then Exit;

  vVST := TVirtualStringTree(Sender);
  vRowNum := FDataList.GetFilteredRowNumber(vVST.FocusedNode.Index);

  GoToNode(vtLog2, vRowNum);
  GoToNode(vtLog, vRowNum);

end;

procedure TView2Frm.vtFilteredLogKeyDown(Sender: TObject; var Key: Word;
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
      vText.Free;
    end;
  end
  else if Key = VK_F2 then
    if Assigned(TVirtualStringTree(Sender).FocusedNode) then
      TVirtualStringTree(Sender).EditNode(TVirtualStringTree(Sender).FocusedNode, 1);
end;

procedure TView2Frm.UpdateMarks;
var
  i, pbWidth, pbHeight, vRowCount, j, y, vStep: Integer;
  vTag: TTagInfo;
begin
  if (FDataList = nil) or (FSortedTags = nil) then Exit;

  pb1.Canvas.Lock;
  try
    pb1.Canvas.FillRect(pb1.ClientRect);
    pbHeight := pb1.Height;
    pbWidth := pb1.Width;
    vRowCount := FDataList.RowCount;
    FDataList.TagList.Sort(stCheckedSort, FSortedTags);
    for i := 0 to FSortedTags.Count - 1 do
    begin
      vTag := TTagInfo(FSortedTags[i]);
      if not vTag.Enabled then Exit;

      vStep := 1; j := 0;
      if vTag.MatchCount > pbHeight then
        vStep := vTag.MatchCount div pbHeight;
      while j < vTag.MatchCount - 1 do
      begin
        y := Round(vTag.MatchRows[j] / vRowCount * pbHeight);
        pb1.Canvas.Pen.Color := vTag.Color;
        pb1.Canvas.MoveTo(0, y);
        pb1.Canvas.LineTo(pbWidth, y);
        j := j + vStep;
      end;
    end;
  finally
    pb1.Canvas.Unlock;
  end;
end;

procedure TView2Frm.UpdateCountLabel;
begin
  lblCount.Caption := IntToStr(FDataList.FilteredRowCount) + ' / ' + IntToStr(FDataList.RowCount);
end;

procedure TView2Frm.pb1Paint(Sender: TObject);
begin
  UpdateMarks;
end;

procedure TView2Frm.OnLoaded(Sender: TObject);
begin
  UpdateCountLabel;
  pb2.Visible := False;
end;

procedure TView2Frm.GetSelectedWord(VST: TObject; const ANew: Boolean);
var
  vVST: TVirtualStringTree;
  vNode: PVirtualNode;
  vRowText: string;
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
  if not (VST is TVirtualStringTree) then Exit;
  
  vVST := TVirtualStringTree(VST);
  vPos := vVST.ScreenToClient(Mouse.CursorPos);
  vNode := vVST.GetNodeAt(vPos.X, vPos.Y);
  try
    if vNode = nil then Exit;
    vVST.GetTextInfo(vNode, 1, vVST.Font, vCellRect, vRowText);
  //  vMargin := vVST.Margin + vVST.TextMargin;
    if Length(vRowText) = 0 then Exit;
    if ANew then
      FSelectedWords.Clear;
    FSelectedWords.Append(TextAt(vPos.X - vCellRect.Left));
  finally
    vtLog.Invalidate;
    vtFilteredLog.Invalidate;
    vtLog2.Invalidate;
    vtFilteredLog2.Invalidate;
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

  vCurNode := GetNodeByIndex(vtLog2, Trunc(FDataList.RowCount * (Y/pb1.Height)));
  vtLog2.FocusedNode := vCurNode;
  vtLog2.ScrollIntoView(vCurNode, True);

  vCurNode := GetNodeByIndex(vtFilteredLog, Trunc(FDataList.FilteredRowCount * (Y/pb1.Height)));
  vtFilteredLog.FocusedNode := vCurNode;
  vtFilteredLog.ScrollIntoView(vCurNode, True);

  vCurNode := GetNodeByIndex(vtFilteredLog2, Trunc(FDataList.FilteredRowCount * (Y/pb1.Height)));
  vtFilteredLog2.FocusedNode := vCurNode;
  vtFilteredLog2.ScrollIntoView(vCurNode, True);
end;

end.
