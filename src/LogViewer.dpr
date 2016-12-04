program LogViewer;

uses
  Forms,
  Windows,
  SysUtils,
  MainForm in 'MainForm.pas' {MainFm},
  ViewFrame in 'ViewFrame.pas' {ViewFrm: TFrame},
  EditTagForm in 'EditTagForm.pas' {EditTagFm},
  StructsUnit in 'StructsUnit.pas',
  uGraphicUtils in 'uGraphicUtils.pas',
  AboutForm in 'AboutForm.pas' {AboutFm},
  uConsts in 'uConsts.pas',
  OptionsForm in 'OptionsForm.pas' {OptionsFm};

{$R *.res}

const
  MailslotName = '\\.\mailslot\LogViewer_FileCommand';
  EventName = 'LogViewer_Command_Event';

var
  ClientMailslotHandle: THandle;
  Letter: string;
  OpenForView: Boolean;
  BytesWritten: DWORD;


begin
  ServerMailslotHandle := CreateMailSlot(MailslotName, 0, MAILSLOT_WAIT_FOREVER, nil);
  
  if ServerMailslotHandle = INVALID_HANDLE_VALUE then
  begin
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
       ClientMailslotHandle := CreateFile(MailslotName, GENERIC_WRITE,
        FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

      if ParamCount > 0 then
      begin
        OpenForView := (ParamCount > 1) and (CompareText(ParamStr(2), '/v') = 0);
        if OpenForView then
          Letter := 'v' + ParamStr(1)
        else
          Letter := 'e' + ParamStr(1);
      end
      else
        Letter := 's';
      WriteFile(ClientMailslotHandle, Letter[1], Length(Letter),
        BytesWritten, nil);
      CommandEvent := OpenEvent(EVENT_MODIFY_STATE, False, EventName);
      SetEvent(CommandEvent);
      CloseHandle(CommandEvent);
      CloseHandle(ClientMailslotHandle);
    end
  end
  else
  begin
    CommandEvent := CreateEvent(nil, False, False, EventName);
    Application.Initialize;
    Application.Title := 'Log Viewer';
    Application.CreateForm(TMainFm, MainFm);
    Application.CreateForm(TEditTagFm, EditTagFm);
    Application.CreateForm(TAboutFm, AboutFm);
    Application.CreateForm(TOptionsFm, OptionsFm);
    Application.Run;
    CloseHandle(ServerMailslotHandle);
    CloseHandle(CommandEvent);
  end;
end.
