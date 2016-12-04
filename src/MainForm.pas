unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees, ComCtrls, ActnList,
  ViewFrame, StructsUnit, ShellAPI, StdCtrls, Menus, ImgList, ToolWin;

const
  WM_CommandArrived = WM_USER + 1;  

type
  TEventWaitThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TMainFm = class(TForm)
    pnlLeft: TPanel;
    vtTags: TVirtualStringTree;
    ToolBar2: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    Splitter1: TSplitter;
    ActionList2: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actCloseTab: TAction;
    edSkipText: TEdit;
    Label1: TLabel;
    dlgOpen1: TOpenDialog;
    mm1: TMainMenu;
    File1: TMenuItem;
    Options1: TMenuItem;
    About1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Closecurrent1: TMenuItem;
    Closeall1: TMenuItem;
    pnl1: TPanel;
    il1: TImageList;
    pm1: TPopupMenu;
    miUncheckAll: TMenuItem;
    miCheckAll: TMenuItem;
    PageControl1: TPageControl;

    procedure vtTagsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtTagsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure actAddExecute(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);

    procedure edSkipTextChange(Sender: TObject);
    procedure vtTagsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btn3Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Closeall1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure vtTagsResize(Sender: TObject);
    procedure miCheckAllClick(Sender: TObject);
    procedure miUncheckAllClick(Sender: TObject);
    procedure vtTagsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FTags: TTagList;
    procedure FillTags;
    function GetSelectedTag: TTagInfo;
    procedure ActivateTab(const AFileName: string);
    procedure CloseCurrentTab;
    procedure ActualizeCurrentView;
    procedure WMCommandArrived(var AMessage: TMessage); message WM_CommandArrived;
    procedure GoToForeground;
    function ReadStringFromMailslot: string;
    procedure UpdateCaption(const AFileName: string);
    procedure SaveOptions;
    procedure LoadOptions;
    procedure CheckAll(const ACheck: Boolean);
  protected
    procedure WMDropFiles(var Msg: TMessage); message wm_DropFiles;
  public
  end;

  TNodeData = record
    Data: TTagInfo;
    GroupName: string;
  end;

var
  MainFm: TMainFm;
  CommandEvent, ServerMailslotHandle: THandle;

implementation

{$R *.dfm}

uses
  uConsts, EditTagForm, uGraphicUtils, AboutForm, IniFiles, OptionsForm;

{ TEventWaitThread }

procedure TEventWaitThread.Execute;
begin
  while True do
  begin            
    if WaitForSingleObject(CommandEvent, INFINITE) <> WAIT_OBJECT_0 then
      Exit;
    PostMessage(MainFm.Handle, WM_CommandArrived, 0, 0);
  end;
end;

function CheckState(const AValue: Boolean): TCheckState;
begin
  if AValue then
    Result := csCheckedNormal
  else
    Result := csUncheckedNormal;  
end;

procedure TMainFm.FillTags;
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
  for i := 0 to FTags.Count - 1 do
  begin
    vTag := FTags[i];
    vNode := GetNodeByGroup;
    vNode.CheckType := ctTriStateCheckBox;
    vNode.CheckState := CheckState(vTag.Enabled);
    vNodeData := vtTags.GetNodeData(vNode);
    vNodeData.Data := vTag;
  end;
  vtTags.FullExpand;
end;

procedure TMainFm.FormShow(Sender: TObject);
begin
  UpdateCaption('');
  gSettingsFileName := ExtractFilePath(Application.ExeName) + 'settings.ini';
  FTags := TTagList.Create;
  FTags.Load;
  edSkipText.Text := FTags.SkipText;
  FillTags;
  if ParamCount > 0 then
    ActivateTab(ParamStr(1));
  TEventWaitThread.Create(False);
  LoadOptions;
end;

procedure TMainFm.actAddExecute(Sender: TObject);
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
    FillTags;
    ActualizeCurrentView;
  end;
end;

procedure TMainFm.vtTagsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TMainFm.vtTagsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
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

procedure TMainFm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    if (FTags.SaveOnExit) then
      FTags.Save;
  except;
  end;  
end;

procedure TMainFm.ActivateTab(const AFileName: string);
var
  vTabSheet: TTabSheet;
  vView: TViewFrm;
  vPrevCursor: TCursor;
  function GetTab: TTabSheet;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to PageControl1.PageCount - 1 do
      if TViewFrm(PageControl1.Pages[i].Tag).FileName = AFileName then
      begin
        Result := PageControl1.Pages[i];
        Break;
      end;
  end;
begin
  if not FileExists(AFileName) then Exit;
    
  vTabSheet := GetTab;
  if vTabSheet = nil then
  begin
    vTabSheet := TTabSheet.Create(nil);
    vTabSheet.PageControl := PageControl1;
    vTabSheet.Caption := ExtractFileName(AFileName);
    vView := TViewFrm.Create(nil);
    vView.Parent := vTabSheet;
    vView.Align := alClient;
    vTabSheet.Tag := Integer(vView);
  end
  else
    vView := TViewFrm(vTabSheet.Tag);

  vPrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    vView.Init(AFileName, FTags);
  finally
    Screen.Cursor := vPrevCursor;
  end;
  PageControl1.ActivePage := vTabSheet;
  
  UpdateCaption(vView.FileName);
end;

procedure TMainFm.vtTagsChecking(Sender: TBaseVirtualTree;
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
    ActualizeCurrentView;
  end;
end;

procedure TMainFm.actEditExecute(Sender: TObject);
var
  vTag: TTagInfo;
begin
  vTag := GetSelectedTag;
  if vTag = nil then Exit;

  if EditTagFm.Edit(vTag) then
  begin
    FTags.Save;
    FillTags;
    ActualizeCurrentView;
  end;  
end;

function TMainFm.GetSelectedTag: TTagInfo;
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

procedure TMainFm.WMDropFiles(var Msg: TMessage);
var
  vFilename: array[0 .. 256] of Char;
  vFile: string;
  vFileCount: Cardinal;
  i: Integer;
begin
  vFileCount := DragQueryFile(THandle(Msg.WParam), 4294967295, { номер файла } vFilename, SizeOf(vFilename));
  for i := 0 to vFileCount - 1 do
  begin
    DragQueryFile(THandle(Msg.WParam), i, { номер файла } vFilename, SizeOf(vFilename));
    vFile := vFilename;
    ActivateTab(vFile);
  end;
  DragFinish(THandle(Msg.WParam));
  SaveOptions;
end;

procedure TMainFm.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True); // разрешаем форме принимать файлы
end;

procedure TMainFm.CloseCurrentTab;
begin
  if PageControl1.PageCount = 0 then Exit;

  TViewFrm(PageControl1.ActivePage.Tag).Deinit;
  TViewFrm(PageControl1.ActivePage.Tag).Free;
  PageControl1.ActivePage.Free;
  ActualizeCurrentView;
  SaveOptions;
end;

procedure TMainFm.actCloseTabExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

procedure TMainFm.actDeleteExecute(Sender: TObject);
begin
  if Application.MessageBox(PChar('Delete tag?'), PChar('Confirm'), MB_YESNO) = ID_YES then
  begin
    FTags.Remove(GetSelectedTag);
    FTags.Save;
    FillTags;
    ActualizeCurrentView;  
  end;
end;

procedure TMainFm.ActualizeCurrentView;
var
  vView: TViewFrm;
begin
  if PageControl1.ActivePage = nil then
  begin
    UpdateCaption('');  
    Exit;
  end;
  vView := TViewFrm(PageControl1.ActivePage.Tag);
  vView.Actualize;
  UpdateCaption(vView.FileName);
end;

procedure TMainFm.edSkipTextChange(Sender: TObject);
begin
  FTags.SkipText := edSkipText.Text;
  ActualizeCurrentView;
end;

procedure TMainFm.vtTagsBeforeCellPaint(Sender: TBaseVirtualTree;
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

procedure TMainFm.WMCommandArrived(var AMessage: TMessage);
var
  Letter, vFileName: string;
begin
  GoToForeground;
  Letter := ReadStringFromMailslot;
  while Letter <> '' do
  begin
    case Letter[1] of
      'e':
        begin
          vFileName := Copy(Letter, 2, MaxInt);
          ActivateTab(vFileName);
        end;
//      'v': OpenFile(Copy(Letter, 2, MaxInt), True);
    end;
    Letter := ReadStringFromMailslot;
  end;
  SaveOptions;
end;

procedure TMainFm.GoToForeground;
var
  Info: TAnimationInfo;
  Animation: Boolean;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  Animation := SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) and
    (Info.iMinAnimate <> 0);
  if Animation then
  begin
    Info.iMinAnimate := 0;
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
  end;
  if not IsIconic(Application.Handle) then
    Application.Minimize;
  Application.Restore;
  if Animation then
  begin
    Info.iMinAnimate := 1;
    SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
  end;
end;

function TMainFm.ReadStringFromMailslot: string;
var
  MessageSize: DWORD;
begin
  GetMailslotInfo(ServerMailslotHandle, nil, MessageSize, nil, nil);
  if MessageSize = MAILSLOT_NO_MESSAGE then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, MessageSize);
  ReadFile(ServerMailslotHandle, Result[1], MessageSize, MessageSize, nil);
end;

procedure TMainFm.PageControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    CloseCurrentTab;
end;

procedure TMainFm.PageControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ActualizeCurrentView
  else
    CloseCurrentTab;
end;

procedure TMainFm.btn3Click(Sender: TObject);
var
  i: Integer;
begin
  if not dlgOpen1.Execute then Exit;

  for i := 0 to dlgOpen1.Files.Count - 1 do
    ActivateTab(dlgOpen1.Files[i]);

  SaveOptions;  
end;

procedure TMainFm.UpdateCaption(const AFileName: string);
begin
  Caption := AFileName;
  if Length(Caption) > 0 then
    Caption := Caption + ' - ';
  Caption := Caption + 'Log Viewer ' + GetFileVersion;
end;

procedure TMainFm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainFm.Closeall1Click(Sender: TObject);
begin
  while PageControl1.PageCount > 0 do
    CloseCurrentTab;
end;

procedure TMainFm.About1Click(Sender: TObject);
begin
  AboutFm.ShowModal;
end;

procedure TMainFm.LoadOptions;
var
  vIni: TIniFile;
  vNames: TStringList;
  i: Integer;
begin
  vIni := TIniFile.Create(gSettingsFileName);
  vNames := TStringList.Create;
  vIni.ReadSection('files', vNames);
  for i := 0 to vNames.Count - 1 do
    ActivateTab(vIni.ReadString('files', vNames[i], ''));
  vNames.Free;
  vIni.Free;
end;

procedure TMainFm.SaveOptions;
var
  vIni: TIniFile;
  i: Integer;
begin
  vIni := TIniFile.Create(gSettingsFileName);
  vIni.EraseSection('files');
  for i := 0 to PageControl1.PageCount - 1 do
    vIni.WriteString('files', 'file' + IntToStr(i), TViewFrm(PageControl1.Pages[i].Tag).FileName);
  vIni.Free;
end;

procedure TMainFm.Options1Click(Sender: TObject);
begin
  if OptionsFm.Edit(FTags) then
  begin
    FTags.Save;
    ActualizeCurrentView;
  end;
end;

procedure TMainFm.vtTagsResize(Sender: TObject);
begin
  vtTags.Header.Columns[0].Width := vtTags.Width - 4;
end;

procedure TMainFm.CheckAll(const ACheck: Boolean);
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
  ActualizeCurrentView;
end;

procedure TMainFm.miCheckAllClick(Sender: TObject);
begin
  CheckAll(True);
end;

procedure TMainFm.miUncheckAllClick(Sender: TObject);
begin
  CheckAll(False);
end;

procedure TMainFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('F')) and (Shift = [ssCtrl]) then
  begin
    TViewFrm(PageControl1.ActivePage.Tag).SwitchFilter;
  end;
end;

initialization

end.
