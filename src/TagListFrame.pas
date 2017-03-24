unit TagListFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, ComCtrls, ToolWin, ActnList, ImgList,

  uStructs, StdCtrls, ExtCtrls, Menus, CheckLst, System.Actions,
  System.ImageList, Vcl.Buttons;

type
  TTagChangeEvent = procedure() of object;

  TTagListFrm = class(TFrame)
    ImageList: TImageList;
    alEdit: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    tlb1: TToolBar;
    btnAdd: TToolButton;
    btnEdit: TToolButton;
    btnDelete: TToolButton;
    vtTags: TVirtualStringTree;
    pm1: TPopupMenu;
    miCheckAll: TMenuItem;
    miUncheckAll: TMenuItem;
    actGroup: TAction;
    actCheckedInTop: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    pnlFilter: TPanel;
    edFilter: TEdit;
    btnClearFilter: TSpeedButton;
    btnAddFromFilter: TSpeedButton;
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
    procedure chklst1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure vtTagsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtTagsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure actGroupExecute(Sender: TObject);
    procedure actCheckedInTopExecute(Sender: TObject);
    procedure btnClearFilterClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure btnAddFromFilterClick(Sender: TObject);
  private
    FTags: TTagList;
    FDataList: TDataList;
    FSortedTags: TList;//array of Integer;
    FSort: TSortType;
    FOnChangeTag: TTagChangeEvent;
    FNeedSave: Boolean;
    procedure FillTreeTags;
    procedure Sort;
    function GetSelectedTag: TTagInfo;
    procedure CheckAll(const ACheck: Boolean);
    procedure DoOnChangeTag;
    procedure UpdateColumns;
  public
    procedure Init(const ADataList: TDataList);
    procedure Init2(const ATagList: TTagList);
    procedure Deinit;

    procedure UpdateLists;
    property Tags: TTagList read FTags;
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
  vTag: TTagInfo;
  vNode: PVirtualNode;
  vNodeData: ^TNodeData;
  vCurrTag: TTagInfo;
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
  if vtTags.FocusedNode = nil then
    vtTags.FocusedNode := vtTags.GetFirst;
  if vtTags.FocusedNode <> nil then
  begin
    vNodeData := vtTags.GetNodeData(vtTags.FocusedNode);
    vCurrTag := vNodeData.Data;
  end;
  vtTags.Clear;
  Sort;
  for i := 0 to FSortedTags.Count - 1 do
  begin
    vTag := TTagInfo(FSortedTags[i]);

    if Length(Trim(edFilter.Text)) > 0 then
      if Pos(UpperCase(Trim(edFilter.Text)), UpperCase(vTag.Name)) <= 0 then Continue;

    if actGroup.Checked then
      vNode := GetNodeByGroup
    else
      vNode := vtTags.AddChild(nil);
    vNode.CheckType := ctTriStateCheckBox;
    vNode.CheckState := CheckState(vTag.Enabled);
    vNodeData := vtTags.GetNodeData(vNode);
    vNodeData.Data := vTag;
    if vTag = vCurrTag then
      vtTags.FocusedNode := vNode;
  end;
  if (vtTags.RootNodeCount > 0) and (vtTags.FocusedNode = nil) then
    vtTags.FocusedNode := vtTags.GetFirst;

  vtTags.FullExpand;
end;

procedure TTagListFrm.Init(const ADataList: TDataList);
begin
  FDataList := ADataList;
  UpdateColumns;
  Init2(ADataList.TagList);
  FNeedSave := False;
end;

procedure TTagListFrm.Init2(const ATagList: TTagList);
begin
  Deinit;
  FTags := ATagList;
  FNeedSave := True;
  FSortedTags := TList.Create;
  FSort := stAlphaSort;
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
    if FNeedSave then
      FTags.Save; // могли добавить некорректный тэг, должна быть возможность его удалить потом
  finally
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
  vTag: TTagInfo;
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
  vTag: TTagInfo;
begin
  vNodeData := vtTags.GetNodeData(Node);
  vTag := vNodeData.Data;
  if Assigned(vTag) then
  begin
    vTag.Enabled := not(Node.CheckState = csUncheckedNormal);
    DoOnChangeTag;
    vtTags.Invalidate;
  end
  else if Assigned(FTags.Owner) then
    FTags.Owner.EndUpdate;
end;

procedure TTagListFrm.vtTagsChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  vNodeData: ^TNodeData;
begin
  vNodeData := vtTags.GetNodeData(Node);
  if (vNodeData.Data = nil) and Assigned(FTags.Owner) then //click on group
    FTags.Owner.BeginUpdate;
end;

procedure TTagListFrm.actEditExecute(Sender: TObject);
var
  vTag: TTagInfo;
begin
  vTag := GetSelectedTag;
  if vTag = nil then Exit;

  if EditTagFm.Edit(vTag) then
  begin
    if FNeedSave then
      FTags.Save;
    FillTreeTags;
    DoOnChangeTag;
  end;
end;

procedure TTagListFrm.actGroupExecute(Sender: TObject);
begin
  FillTreeTags;
end;

procedure TTagListFrm.btnAddFromFilterClick(Sender: TObject);
begin
  FTags.AddTag(Trim(edFilter.Text), 'Temporary added');
  FillTreeTags;
  DoOnChangeTag;
end;

procedure TTagListFrm.btnClearFilterClick(Sender: TObject);
begin
  edFilter.Clear;
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

procedure TTagListFrm.actCheckedInTopExecute(Sender: TObject);
begin
  if actCheckedInTop.Checked then
    FSort := stCheckedSort
  else
    FSort := stAlphaSort;
  FillTreeTags;
end;

procedure TTagListFrm.actDeleteExecute(Sender: TObject);
begin
  if Application.MessageBox(PChar('Delete tag?'), PChar('Confirm'), MB_YESNO) = ID_YES then
  begin
    FTags.Remove(GetSelectedTag);
    if FNeedSave then
      FTags.Save;
    FillTreeTags;
    DoOnChangeTag;
  end;
end;

procedure TTagListFrm.Deinit;
begin
  vtTags.RootNodeCount := 0;
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
   { FillGradientRoundRect(TargetCanvas, CellRect,
      CalcBrightColor(vNodeData.Data.Color, 85),//RGB(207, 221, 204),
      CalcBrightColor(vNodeData.Data.Color, 70), //RGB(207, 221, 204),
      vNodeData.Data.Color);}

    TargetCanvas.Brush.Color := CalcBrightColor(vNodeData.Data.Color, 75);
    TargetCanvas.Pen.Color := vNodeData.Data.Color;
    TargetCanvas.RoundRect(CellRect.Left, CellRect.Top + 1, CellRect.Right, CellRect.Bottom - 1, 4, 4);
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

procedure TTagListFrm.edFilterChange(Sender: TObject);
begin
  FillTreeTags;
  btnAddFromFilter.Visible := Length(Trim(edFilter.Text)) > 0;
  btnClearFilter.Visible := Length(Trim(edFilter.Text)) > 0;
end;

procedure TTagListFrm.edSkipTextChange(Sender: TObject);
begin
  DoOnChangeTag;
end;

procedure TTagListFrm.Sort;
begin
  FTags.Sort(FSort, FSortedTags);
end;

procedure TTagListFrm.chklst1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  vList: TCheckListBox;
  vColor: TColor;
begin
  vList := Control as TCheckListBox;
  vColor := TTagInfo(vList.Items.Objects[Index]).Color;
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
    if TTagInfo(vList.Items.Objects[Index]).Enabled then
    begin
      Brush.Color := vColor;
      RoundRect(Rect.Left + 10, Rect.Top, 14, Rect.Bottom, 3, 3);
    end;  
  end;
///  TCheckListBox(Control).Canvas.TextRect(Rect, 20, 0, chklst1.Items[Index]);
end;

procedure TTagListFrm.UpdateColumns;
begin

end;

procedure TTagListFrm.UpdateLists;
begin
  Sort;
  FillTreeTags;
end;

end.
