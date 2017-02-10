unit TagListFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, ComCtrls, ToolWin, ActnList, ImgList,

  uStructs, StdCtrls, ExtCtrls, Menus, CheckLst, System.Actions,
  System.ImageList;

type
  TTagChangeEvent = procedure() of object;
  TSortType = (stAlphaSort, stCheckedSort, stNoSort);

  TTagListFrm = class(TFrame)
    ImageList: TImageList;
    alEdit: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actCloseTab: TAction;
    tlb1: TToolBar;
    btnAdd: TToolButton;
    btnEdit: TToolButton;
    btnDelete: TToolButton;
    vtTags: TVirtualStringTree;
    pm1: TPopupMenu;
    miCheckAll: TMenuItem;
    miUncheckAll: TMenuItem;
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    chklst1: TCheckListBox;
    tlb2: TToolBar;
    alSort: TActionList;
    actAlphabeticalSort: TAction;
    actCheckedSort: TAction;
    actNoSort: TAction;
    btnAlphabeticalSort: TToolButton;
    btnCheckedSort: TToolButton;
    btnNoSort: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure vtTagsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtTagsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure actEditExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure vtTagsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure vtTagsResize(Sender: TObject);
    procedure miCheckAllClick(Sender: TObject);
    procedure miUncheckAllClick(Sender: TObject);
    procedure edSkipTextChange(Sender: TObject);
    procedure actAlphabeticalSortExecute(Sender: TObject);
    procedure actCheckedSortExecute(Sender: TObject);
    procedure chklst1ClickCheck(Sender: TObject);
    procedure chklst1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure vtTagsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtTagsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
  private
    FTags: TTagList2;
    FSortedTags: TList;//array of Integer;
    FSort: TSortType;
    FOnChangeTag: TTagChangeEvent;
    procedure FillTreeTags;
    procedure FillListTags;
    procedure Sort;
    procedure ReallocSortedTags;
    procedure SyncTreeChecks;
    procedure SyncListChecks;
    function GetSelectedTag: TTagInfo2;
    procedure CheckAll(const ACheck: Boolean);
    procedure DoOnChangeTag;
  public
    procedure Init(const ATagList: TTagList2);
    procedure Deinit;

    procedure UpdateLists;
    property Tags: TTagList2 read FTags;
    property OnChangeTag: TTagChangeEvent read FOnChangeTag write FOnChangeTag;
  end;

implementation

uses
  EditTagForm,
  
  uGraphicUtils, Types;

{$R *.dfm}

function CheckState(const AValue: Boolean): TCheckState;
begin
  if AValue then
    Result := csCheckedNormal
  else
    Result := csUncheckedNormal;
end;

procedure TTagListFrm.FillTreeTags;
var
  i: Integer;
  vTag: TTagInfo2;
  vNode: PVirtualNode;
  vNodeData: ^TNodeData;
  function GetNodeByGroup: PVirtualNode;
  var
    vN, vGroupNode: PVirtualNode;
  begin
    vGroupNode := nil;
    if vTag.GroupName = '' then
      Result := vtTags.AddChild(nil)
    else
    begin
      vN := vtTags.GetFirst;
      while Assigned(vN) do  //find group node
      begin
        vNodeData := vtTags.GetNodeData(vN);
        if vNodeData.GroupName = vTag.GroupName then
        begin
          vGroupNode := vN;
          Break;
        end;
        vN := vtTags.GetNext(vN);
      end;
      if vN = nil then //group node not found
      begin
        vGroupNode := vtTags.AddChild(nil);
        vGroupNode.CheckType := ctTriStateCheckBox;
        vNodeData := vtTags.GetNodeData(vGroupNode);
        vNodeData.GroupName := vTag.GroupName;
      end;
      Result := vtTags.AddChild(vGroupNode);
    end;
  end;
begin
  vtTags.Clear;
  chklst1.Clear;
  for i := 0 to FTags.Count - 1 do
  begin
    vTag := FTags[i];
    vNode := GetNodeByGroup;
    vNode.CheckType := ctTriStateCheckBox;
    vNode.CheckState := CheckState(vTag.Enabled);
    vNodeData := vtTags.GetNodeData(vNode);
    vNodeData.Data := vTag;

    chklst1.AddItem(vTag.FullName, vTag);
    chklst1.Checked[i] := vTag.Enabled;
  end;
  vtTags.FullExpand;

  FillListTags;
end;

procedure TTagListFrm.Init(const ATagList: TTagList2);
begin
  Deinit;
  FTags := ATagList;
  FSortedTags := TList.Create;
  ReallocSortedTags;  
  FillTreeTags;
end;

procedure TTagListFrm.actAddExecute(Sender: TObject);
var
  vTag: TTagInfo2;
begin
  vTag := TTagInfo2.Create('', True, clHighlight, '');
  if not EditTagFm.Edit(vTag) then
  begin
    vTag.Free;
    Exit;
  end;

  FTags.Add(vTag);
  try
    FTags.Save; // могли добавить некорректный тэг, должна быть возможность его удалить потом
  finally
    ReallocSortedTags;  
    FillTreeTags;
    DoOnChangeTag;
  end;
end;

procedure TTagListFrm.vtTagsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TTagListFrm.vtTagsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  vNodeData: ^TNodeData;
  vTag: TTagInfo2;
begin
  vNodeData := vtTags.GetNodeData(Node);
  vTag := vNodeData.Data;
  if vTag = nil then
    CellText := vNodeData.GroupName
  else
  begin
    CellText := vTag.FullName;
  end;
end;

procedure TTagListFrm.vtTagsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  vNodeData: ^TNodeData;
  vTag: TTagInfo2;
begin
  vNodeData := vtTags.GetNodeData(Node);
  vTag := vNodeData.Data;
  if Assigned(vTag) then
  begin
    vTag.Enabled := not(Node.CheckState = csUncheckedNormal);
    SyncListChecks;
    DoOnChangeTag;
    vtTags.Invalidate;
  end
  else
    FTags.Owner.EndUpdate;

end;

procedure TTagListFrm.vtTagsChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  vNodeData: ^TNodeData;
begin
  vNodeData := vtTags.GetNodeData(Node);
  if vNodeData.Data = nil then //click on group
    FTags.Owner.BeginUpdate;

end;

procedure TTagListFrm.actEditExecute(Sender: TObject);
var
  vTag: TTagInfo2;
begin
  vTag := GetSelectedTag;
  if vTag = nil then Exit;

  if EditTagFm.Edit(vTag) then
  begin
    FTags.Save;
    FillTreeTags;
    DoOnChangeTag;
  end;
end;

function TTagListFrm.GetSelectedTag: TTagInfo2;
var
  vNodeData: ^TNodeData;
  vNode: PVirtualNode;
begin
  Result := nil;
  vNode := vtTags.FocusedNode;
  if vNode = nil then Exit;

  vNodeData := vtTags.GetNodeData(vtTags.FocusedNode);
  Result := vNodeData.data;
end;

procedure TTagListFrm.actDeleteExecute(Sender: TObject);
begin
  if Application.MessageBox(PChar('Delete tag?'), PChar('Confirm'), MB_YESNO) = ID_YES then
  begin
    FTags.Remove(GetSelectedTag);
    FTags.Save;
    ReallocSortedTags;
    FillTreeTags;
    DoOnChangeTag;
  end;
end;

procedure TTagListFrm.Deinit;
begin
  FreeAndNil(FSortedTags);
end;

procedure TTagListFrm.vtTagsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  vNodeData: ^TNodeData;
  vRect: TRect;
begin
  vNodeData := vtTags.GetNodeData(Node);
  if Assigned(vNodeData.Data) and vNodeData.Data.Enabled then
  begin
    vRect := vtTags.GetDisplayRect(Node, Column, True);
    CellRect.Left := vRect.Left;
    CellRect.Right := vRect.Right;
    FillGradientRoundRect(TargetCanvas, CellRect,
      CalcBrightColor(vNodeData.Data.Color, 85),//RGB(207, 221, 204),
      CalcBrightColor(vNodeData.Data.Color, 70), //RGB(207, 221, 204),
      vNodeData.Data.Color);
  end;
end;

procedure TTagListFrm.vtTagsResize(Sender: TObject);
begin
  vtTags.Header.Columns[0].Width := vtTags.Width - 24;
end;

procedure TTagListFrm.CheckAll(const ACheck: Boolean);
var
  vNode: PVirtualNode;
begin
  vNode := vtTags.GetFirst;
  while Assigned(vNode) do
  begin
    if ACheck then
      vNode.CheckState := csCheckedNormal
    else
      vNode.CheckState := csUncheckedNormal;
    vNode := vtTags.GetNext(vNode);
  end;
  FTags.CheckAll(ACheck);
  vtTags.Invalidate;
end;

procedure TTagListFrm.miCheckAllClick(Sender: TObject);
begin
  CheckAll(True);
end;

procedure TTagListFrm.miUncheckAllClick(Sender: TObject);
begin
  CheckAll(False);
end;

procedure TTagListFrm.DoOnChangeTag;
begin
  if Assigned(FOnChangeTag) then
    FOnChangeTag;
end;

procedure TTagListFrm.edSkipTextChange(Sender: TObject);
begin
  DoOnChangeTag;
end;

procedure TTagListFrm.FillListTags;
var
  i: Integer;
  vText: string;
  vTag: TTagInfo2;
begin
  chklst1.Clear;
  for i := 0 to FSortedTags.Count - 1 do
  begin
    vTag := TTagInfo2(FSortedTags[i]);
    vText := vTag.FullName;
    chklst1.AddItem(vText, vTag);
    chklst1.Checked[i] := vTag.Enabled;
  end;
end;

procedure TTagListFrm.ReallocSortedTags;
begin
  FTags.CopyTo(FSortedTags);
 // FSort
  Sort;
end;

function SortAlpha(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TTagInfo2(Item1).Name, TTagInfo2(Item2).Name);
end;

function SortChecked(Item1, Item2: Pointer): Integer;
begin
  if TTagInfo2(Item1).Enabled = TTagInfo2(Item2).Enabled then
  begin
    if TTagInfo2(Item1).Enabled then
      Result := TTagInfo2(Item2).MatchCount - TTagInfo2(Item1).MatchCount
    else
      Result := CompareText(TTagInfo2(Item1).Name, TTagInfo2(Item2).Name);
  end
  else  if TTagInfo2(Item1).Enabled then
    Result := -1
  else
    Result := 1;
end;

procedure TTagListFrm.Sort;
begin
  case FSort of
    stAlphaSort: FSortedTags.Sort(SortAlpha);
    stCheckedSort: FSortedTags.Sort(SortChecked);
    stNoSort:;
  end;  
end;

procedure TTagListFrm.actAlphabeticalSortExecute(Sender: TObject);
begin
  FSort := stAlphaSort;
  Sort;
  FillListTags;
end;

procedure TTagListFrm.actCheckedSortExecute(Sender: TObject);
begin
  FSort := stCheckedSort;
  Sort;
  FillListTags;
end;

procedure TTagListFrm.chklst1ClickCheck(Sender: TObject);
begin
  TTagInfo2(chklst1.Items.Objects[chklst1.ItemIndex]).Enabled := chklst1.Checked[chklst1.ItemIndex];
  SyncTreeChecks;
  DoOnChangeTag;
end;

procedure TTagListFrm.SyncTreeChecks;
var
  vNode: PVirtualNode;
  vNodeData: ^TNodeData;
  vTag: TTagInfo2;
begin
  vNode := vtTags.GetFirst;
  while Assigned(vNode) do
  begin
    vNodeData := vtTags.GetNodeData(vNode);
    vTag := vNodeData.Data;
    if Assigned(vTag) then
    begin
      if vTag.Enabled then
        vNode.CheckState := csCheckedNormal
      else
        vNode.CheckState := csUncheckedNormal;
    end;    
    vNode := vtTags.GetNext(vNode);
  end;
  vtTags.Invalidate;
end;

procedure TTagListFrm.SyncListChecks;
var
  i: Integer;
begin
  for i := 0 to chklst1.Count - 1 do
    if TTagInfo2(chklst1.Items.Objects[i]).Enabled <> chklst1.Checked[i] then
      chklst1.Checked[i] := TTagInfo2(chklst1.Items.Objects[i]).Enabled;
end;

procedure TTagListFrm.chklst1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  vList: TCheckListBox;
  vColor: TColor;
begin
  vList := Control as TCheckListBox;
  vColor := TTagInfo2(vList.Items.Objects[Index]).Color;
  with vList.Canvas, Rect do
  begin
    if odSelected in State then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else
    begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end;
   // FillRect(Rect);

    if odFocused in State then Windows.DrawFocusRect(vList.Canvas.Handle, Rect);
    TextOut(Left + 14, Top, vList.Items[Index]);
    if TTagInfo2(vList.Items.Objects[Index]).Enabled then
    begin
      Brush.Color := vColor;
      RoundRect(Rect.Left + 10, Rect.Top, 14, Rect.Bottom, 3, 3);
    end;  
  end;
///  TCheckListBox(Control).Canvas.TextRect(Rect, 20, 0, chklst1.Items[Index]);
end;

procedure TTagListFrm.UpdateLists;
begin
  FillListTags;
  FillTreeTags;
end;

end.
