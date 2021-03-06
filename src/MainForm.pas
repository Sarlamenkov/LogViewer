unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees, ComCtrls, ActnList,
  ShellAPI, StdCtrls, Menus, ImgList, ToolWin, ImageList, Actions,

  View2Frame, uStructs, TagListFrame,
  uPageControlWithCloseButtons;

const
  WM_CommandArrived = WM_USER + 1;  

type
  TEventWaitThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TMainFm = class(TForm)
    dlgOpen1: TOpenDialog;

    Timer1: TTimer;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    actOpen: TAction;
    actCloseCurrent: TAction;
    actCloseAll: TAction;
    actOptions: TAction;
    actDefaultTags: TAction;
    actAbout: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ImageList1: TImageList;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    tbHistory: TToolButton;
    pmReopen: TPopupMenu;
    actHelp: TAction;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    pmClose: TPopupMenu;
    est1: TMenuItem;
    est21: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCloseTabExecute(Sender: TObject);

    procedure btn3Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Closeall1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DefaultTags1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actCloseCurrentExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actDefaultTagsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure tbHistoryClick(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);

  private
    PageControlWithCB: TPageControlWithCloseButtons;
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabSheetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function CreateTab(const AFileName: string): TTabSheet;
    procedure CloseCurrentTab(const AQuick: Boolean = False);
    procedure CloseTab(const AIndex: Integer; const AQuick: Boolean = False);
    procedure ActualizeCurrentView;
    procedure WMCommandArrived(var AMessage: TMessage); message WM_CommandArrived;
    procedure GoToForeground;
    function ReadStringFromMailslot: string;
    procedure UpdateCaption(const AFileName: string);
    procedure RefillOpenedFileNames;
    procedure RefillHistoryFileNames;
    procedure OnSelectHistoryFile(Sender: TObject);
    procedure FirstActivate;
    procedure OnCloseTab(const ATabIndex: Integer);
    procedure OnPageChange(ASender: TObject);
  protected
    procedure WMDropFiles(var Msg: TMessage); message wm_DropFiles;
  public
  end;


var
  MainFm: TMainFm;
  CommandEvent, ServerMailslotHandle: THandle;


implementation

{$R *.dfm}

uses
  IniFiles, Types,

  uConsts, EditTagForm, AboutForm,  OptionsForm,
  TagListDefaultForm, HelpForm;

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

procedure TMainFm.FormShow(Sender: TObject); 
begin
  gSettingsFileName := ExtractFilePath(Application.ExeName) + 'logviewer.ini';
  UpdateCaption('');
  TEventWaitThread.Create(False);
  Options.LoadOptions;
  RefillHistoryFileNames;
  WindowState := TWindowState(Options.MainWindowState);
  PageControlWithCB.OnChange := OnPageChange;
end;

procedure TMainFm.RefillOpenedFileNames;
var
  i: Integer;
begin
  Options.OpenedFileNames.Clear;
  for i := 0 to PageControlWithCB.PageCount - 1 do
    Options.OpenedFileNames.Add(TView2Frm(PageControlWithCB.Pages[i].Tag).FileName);
end;

procedure TMainFm.TabSheetContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  //pmClose.Popup(MousePos.X , MousePos.Y);
end;

procedure TMainFm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  FirstActivate;
end;

procedure TMainFm.tbHistoryClick(Sender: TObject);
var
  vPoint: TPoint;
begin
  vPoint := tbHistory.ClientToScreen(Point(0, tbHistory.Height + 2));
  pmReopen.Popup(vPoint.X, vPoint.Y);
end;

procedure TMainFm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LockControl(Self, True);
  try
    Options.MainWindowState := Integer(WindowState);
    RefillOpenedFileNames;
    Options.SaveOptions;
    while PageControlWithCB.PageCount > 0 do
      CloseCurrentTab(True);
  finally
    LockControl(Self, False);
  end;
end;

function TMainFm.CreateTab(const AFileName: string): TTabSheet;
var
  vView: TView2Frm;
  function GetTab: TTabSheet;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to PageControlWithCB.PageCount - 1 do
      if TView2Frm(PageControlWithCB.Pages[i].Tag).FileName = AFileName then
      begin
        Result := PageControlWithCB.Pages[i];
        Break;
      end;
  end;
begin
  Result := nil;

  if not FileExists(AFileName) then Exit;
    
  Result := GetTab;
  if Result = nil then
  begin
    Result := TTabSheet.Create(nil);
    Result.PageControl := PageControlWithCB;
    PageControlWithCB.PageCountChanged;
    Result.Caption := ExtractFileName(AFileName) + '      ';
    Result.Width := PageControlWithCB.Canvas.TextWidth(Result.Caption) + 120;
    vView := TView2Frm.Create(nil);
    vView.Parent := Result;
    vView.Align := alClient;
    Result.Tag := Integer(vView);
  end
  else
    vView := TView2Frm(Result.Tag);

  PageControlWithCB.OnContextPopup := TabSheetContextPopup;

  vView.Init(AFileName);

  UpdateCaption(vView.FileName);
end;

procedure TMainFm.actOpenExecute(Sender: TObject);
var
  i: Integer;
  vLastTab: TTabSheet;
begin
  if not dlgOpen1.Execute then Exit;

  vLastTab := nil;
  for i := 0 to dlgOpen1.Files.Count - 1 do
    vLastTab := CreateTab(dlgOpen1.Files[i]);

  RefillOpenedFileNames;
  Options.SaveOptions;

  PageControlWithCB.ActivePage := vLastTab;
  ActualizeCurrentView;
end;

procedure TMainFm.actOptionsExecute(Sender: TObject);
begin
  if OptionsFm.Edit then
  begin
    Options.SaveOptions;
    ActualizeCurrentView;
  end;
end;

procedure TMainFm.WMDropFiles(var Msg: TMessage);
var
  vFilename: array[0 .. 256] of Char;
  vFile: string;
  vFileCount: Cardinal;
  i: Integer;
begin
  vFileCount := DragQueryFile(THandle(Msg.WParam), 4294967295, { ����� ����� } vFilename, SizeOf(vFilename));
  for i := 0 to vFileCount - 1 do
  begin
    DragQueryFile(THandle(Msg.WParam), i, { ����� ����� } vFilename, SizeOf(vFilename));
    vFile := vFilename;
    CreateTab(vFile);
  end;
  DragFinish(THandle(Msg.WParam));
//  SaveOptions;
end;

procedure TMainFm.FirstActivate;
var
  i: Integer;
begin
  for i := 0 to Options.OpenedFileNames.Count - 1 do
    CreateTab(Options.OpenedFileNames[i]);
  if ParamCount > 0 then
    CreateTab(ParamStr(1));

  ActualizeCurrentView;
end;

procedure TMainFm.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True); // ��������� ����� ��������� �����
  PageControlWithCB := TPageControlWithCloseButtons.Create(Self);
  PageControlWithCB.Align := alClient;
  PageControlWithCB.Parent := Self;
  PageControlWithCB.OnMouseUp2 := PageControl1MouseUp;
  PageControlWithCB.OnMouseDown2 := PageControl1MouseDown;
  PageControlWithCB.OnCloseTab := OnCloseTab;
end;

procedure TMainFm.CloseCurrentTab(const AQuick: Boolean = False);
begin
  if PageControlWithCB.ActivePage <> nil then
    CloseTab(PageControlWithCB.ActivePage.TabIndex, AQuick);
end;

procedure TMainFm.CloseTab(const AIndex: Integer; const AQuick: Boolean);
var
  vView: TView2Frm;
begin
  if PageControlWithCB.PageCount = 0 then Exit;

  vView := TView2Frm(PageControlWithCB.Pages[AIndex].Tag);

  Options.AddToHistory(vView.FileName);

  vView.Deinit;
  vView.Free;
  PageControlWithCB.Pages[AIndex].Free;
  PageControlWithCB.PageCountChanged;
  if not AQuick then
  begin
    RefillHistoryFileNames;
    ActualizeCurrentView;
    Options.SaveOptions;
  end;
end;

procedure TMainFm.actAboutExecute(Sender: TObject);
begin
  AboutFm.ShowModal;
end;

procedure TMainFm.actCloseAllExecute(Sender: TObject);
begin
  while PageControlWithCB.PageCount > 0 do
    CloseCurrentTab(True);
  RefillOpenedFileNames;
  Options.SaveOptions;

  RefillHistoryFileNames;
end;

procedure TMainFm.actCloseCurrentExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

procedure TMainFm.actCloseTabExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

procedure TMainFm.actDefaultTagsExecute(Sender: TObject);
begin
  TagListDefaultFm.Edit;
end;

procedure TMainFm.actHelpExecute(Sender: TObject);
begin
  HelpFm.ShowModal;
end;

procedure TMainFm.ActualizeCurrentView;
var
  vView: TView2Frm;
begin
  if PageControlWithCB.ActivePage = nil then
  begin
    UpdateCaption('');
    Exit;
  end;
  vView := TView2Frm(PageControlWithCB.ActivePage.Tag);
  vView.Actualize;
  UpdateCaption(vView.FileName);
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
          PageControlWithCB.ActivePage := CreateTab(vFileName);
          TView2Frm(PageControlWithCB.ActivePage.Tag).Actualize;
        end;
//      'v': OpenFile(Copy(Letter, 2, MaxInt), True);
    end;
    Letter := ReadStringFromMailslot;
  end;
//  SaveOptions;
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
//  if not IsIconic(Application.Handle) then
//    Application.Minimize;
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
  a: AnsiString;
begin
  GetMailslotInfo(ServerMailslotHandle, nil, MessageSize, nil, nil);
  if MessageSize = MAILSLOT_NO_MESSAGE then
  begin
    Result := '';
    Exit;
  end;
  SetLength(a, MessageSize);
  ReadFile(ServerMailslotHandle, a[1] , MessageSize, MessageSize, nil);
  Result := a;
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
    ActualizeCurrentView;
end;

procedure TMainFm.btn3Click(Sender: TObject);
var
  i: Integer;
begin
  if not dlgOpen1.Execute then Exit;

  for i := 0 to dlgOpen1.Files.Count - 1 do
    CreateTab(dlgOpen1.Files[i]);

  RefillOpenedFileNames;
  Options.SaveOptions;
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
  while PageControlWithCB.PageCount > 0 do
    CloseCurrentTab(True);
  RefillOpenedFileNames;
  RefillHistoryFileNames;
  Options.SaveOptions;  
end;

procedure TMainFm.About1Click(Sender: TObject);
begin
  AboutFm.ShowModal;
end;

procedure TMainFm.Options1Click(Sender: TObject);
begin
  if OptionsFm.Edit then
  begin
    Options.SaveOptions;
    ActualizeCurrentView;
  end;
end;

procedure TMainFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  vView: TView2Frm;
begin
  if (Key = Ord('F')) then
  begin
    if Shift = [ssCtrl] then begin
      if TView2Frm(PageControlWithCB.ActivePage.Tag).edtSearch.CanFocus then
        TView2Frm(PageControlWithCB.ActivePage.Tag).edtSearch.SetFocus;
    end
    else if Shift = [] then
       // TView2Frm(PageControl1.ActivePage.Tag).SwitchFilter;
  end
  else if (Key = Ord('T')) and (Shift = [ssCtrl]) then
  begin
    if PageControlWithCB.PageCount = 0 then Exit;

    vView := TView2Frm(PageControlWithCB.ActivePage.Tag);
    vView.AddTagFromSelection;
  end
  else if (Key = VK_F5) then
  begin
    TView2Frm(PageControlWithCB.ActivePage.Tag).Reload;
  end;
end;

procedure TMainFm.RefillHistoryFileNames;
var
  i: Integer;
  vMI: TMenuItem;
begin
//  for i := 0 to miReopen.Count - 1 do
 //   miReopen.Items[i].Free;
  pmReopen.Items.Clear;

  for i := 0 to Options.HistoryFileNames.Count - 1 do
  begin
    vMI := TMenuItem.Create(nil);
    vMI.OnClick := OnSelectHistoryFile;
    vMI.Caption := Options.HistoryFileNames[i];
    vMI.Hint := Options.HistoryFileNames[i];
    pmReopen.Items.Add(vMI);
  end;
end;

procedure TMainFm.OnCloseTab(const ATabIndex: Integer);
begin
  CloseTab(ATabIndex);
end;

procedure TMainFm.OnPageChange(ASender: TObject);
begin
  ActualizeCurrentView;
end;

procedure TMainFm.OnSelectHistoryFile(Sender: TObject);
var
  i: Integer;
begin
  PageControlWithCB.ActivePage := CreateTab(TMenuItem(Sender).Hint);
  i := Options.HistoryFileNames.IndexOf(TMenuItem(Sender).Hint);
  if i > -1 then
  begin
    Options.HistoryFileNames.Delete(i);
    RefillHistoryFileNames;
    Options.SaveOptions;
  end;

  ActualizeCurrentView;
end;

procedure TMainFm.DefaultTags1Click(Sender: TObject);
begin
  TagListDefaultFm.Edit;
end;

initialization

end.
