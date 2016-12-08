unit TagListFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, ComCtrls, ToolWin, ActnList, ImgList,

  StructsUnit, StdCtrls, ExtCtrls, Menus, CheckLst;

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
    pnl1: TPanel;
    lbl1: TLabel;
    edSkipText: TEdit;
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
      var CellText: WideString);
    procedure vtTagsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
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
  private
    FTags: TTagList;
    FSortedTags: TList;//array of Integer;
    FSort: TSortType;
    FOnChangeTag: TTagChangeEvent;
    procedure FillTreeTags;
    procedure FillListTags;
    procedure Sort;
    procedure ReallocSortedTags;
    procedure SyncTreeChecks;
    procedure SyncListChecks;    
    function GetSelectedTag: TTagInfo;
    procedure CheckAll(const ACheck: Boolean);
    procedure DoOnChangeTag;
  public
    procedure Init(const ATagsSection: string);
    procedure Deinit;

    property Tags: TTagList read FTags;
    property OnChangeTag: TTagChangeEvent read FOnChangeTag write FOnChangeTag;
  end;

implementation

uses
  EditTagForm,
  
  uGraphicUtils;

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
  vTag: TTagInfo;
  vNode: PVirtualNode;
  vNodeData: ^TNodeData;
  function GetNodeByGroup: PVirtualNode;
  var
    vN, vGroupNode: PVirtualNode;
  begin
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

    chklst1.AddItem(vTag.Name, vTag);
    chklst1.Checked[i] := vTag.Enabled;
  end;
  vtTags.FullExpand;

  FillListTags;
end;

procedure TTagListFrm.Init(const ATagsSection: string);
begin
  Deinit;
  FTags := TTagList.Create;
  FTags.Load(ATagsSection);
  FSortedTags := TList.Create;
  edSkipText.Text := FTags.SkipText;
  ReallocSortedTags;  
  FillTreeTags;
end;

procedure TTagListFrm.actAddExecute(Sender: TObject);
var
  vTag: TTagInfo;
begin
  vTag := TTagInfo.Create('', True, clHighlight, '');
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
  var CellText: WideString);
var
  vNodeData: ^TNodeData;
  vTag: TTagInfo;
begin
  vNodeData := vtTags.GetNodeData(Node);
  vTag := vNodeData.Data;
  if vTag = nil then
    CellText := vNodeData.GroupName
  else
    CellText := vTag.Name;
end;

procedure TTagListFrm.vtTagsChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  vNodeData: ^TNodeData;
  vTag: TTagInfo;
begin
  vNodeData := vtTags.GetNodeData(Node);
  vTag := vNodeData.Data;
  if Assigned(vTag) then
  begin
    vTag.Enabled := not(NewState = csUncheckedNormal);
    SyncListChecks;
    DoOnChangeTag;
  end;
end;

procedure TTagListFrm.actEditExecute(Sender: TObject);
var
  vTag: TTagInfo;
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

function TTagListFrm.GetSelectedTag: TTagInfo;
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
  FreeAndNil(FTags);
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
  vNodeData: ^TNodeData;
  vTag: TTagInfo;
begin
  vNode := vtTags.GetFirst;
  while Assigned(vNode) do
  begin
    vNodeData := vtTags.GetNodeData(vNode);
    vTag := vNodeData.Data;
    if Assigned(vTag) then
      vTag.Enabled := ACheck;
    if ACheck then
      vNode.CheckState := csCheckedNormal
    else
      vNode.CheckState := csUncheckedNormal;
    vNode := vtTags.GetNext(vNode);
  end;
  vtTags.Invalidate;
  DoOnChangeTag;
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
  FTags.SkipText := edSkipText.Text;
  DoOnChangeTag;
end;

procedure TTagListFrm.FillListTags;
var
  i: Integer;
begin
  chklst1.Clear;
  for i := 0 to FSortedTags.Count - 1 do
  begin
    chklst1.AddItem(TTagInfo(FSortedTags[i]).Name, FSortedTags[i]);
    chklst1.Checked[i] := TTagInfo(FSortedTags[i]).Enabled;
  end;
end;

procedure TTagListFrm.ReallocSortedTags;
begin
  FTags.CopyTo(FSortedTags);
  Sort;
end;

function SortAlpha(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TTagInfo(Item1).Name, TTagInfo(Item2).Name);
end;

function SortChecked(Item1, Item2: Pointer): Integer;
begin
  if TTagInfo(Item1).Enabled = TTagInfo(Item2).Enabled then
    Result := 0
  else if TTagInfo(Item1).Enabled then
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
  TTagInfo(chklst1.Items.Objects[chklst1.ItemIndex]).Enabled := chklst1.Checked[chklst1.ItemIndex];
  SyncTreeChecks;
  DoOnChangeTag;
end;

procedure TTagListFrm.SyncTreeChecks;
var
  vNode: PVirtualNode;
  vNodeData: ^TNodeData;
  vTag: TTagInfo;
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
    if TTagInfo(chklst1.Items.Objects[i]).Enabled <> chklst1.Checked[i] then
      chklst1.Checked[i] := TTagInfo(chklst1.Items.Objects[i]).Enabled;
end;

end.
