program logviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  uConsts,
  uStructs,
  AboutForm,
  HelpForm,
  OptionsForm,
  EditTagForm,
  TagListFrame,
  TagListDefaultForm,
  MainForm

  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.CreateForm(TEditTagFm, EditTagFm);
  Application.CreateForm(TAboutFm, AboutFm);
  Application.CreateForm(TOptionsFm, OptionsFm);
  Application.CreateForm(TTagListDefaultFm, TagListDefaultFm);
  Application.CreateForm(THelpFm, HelpFm);
  Application.Run;
end.

