unit uStructs;

interface

uses
  Graphics, Classes, IniFiles;

type
  TDataList = class;
  TTagList2 = class;
  
  TTagInfo2 = class
  private
    FOwner: TTagList2;
    FName: string;
    FUpperCaseName: string;
    FColor: TColor;
    FEnabled: Boolean;
    FGroupName: string;
    FIndexRows: TList;
    FIndexed: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetMatchCount: Integer;
    function GetFullName: string;
    procedure SetName(const Value: string);
    function GetMatchRow(const AIndex: Integer): Integer;
  public
    constructor Create(const AName: string = ''; const AEnabled: Boolean = True;
      const AColor: TColor = clHighlightText; const AGroupName: string = '');
    destructor Destroy; override;

    procedure DoIndex(const AText: string; const AUpperText: string; const ARowIndex: Integer);

    property MatchRows[const AIndex: Integer]: Integer read GetMatchRow;
    property MatchCount: Integer read GetMatchCount;
    property Name: string read FName write SetName;
    property UpperCaseName: string read FUpperCaseName;
    property FullName: string read GetFullName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Color: TColor read FColor write FColor;
    property GroupName: string read FGroupName write FGroupName;
  end;

  TTagList2 = class
  private
    FOwner: TDataList;
    FSectionName: string;
    FList: TList;
    function GetItem(const AIndex: Integer): TTagInfo2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const ASectionName: string);
    procedure Save;

    procedure Add(const ATagInfo: TTagInfo2);
    procedure Remove(const ATagInfo: TTagInfo2);

    procedure CheckAll(const ACheck: Boolean);

    procedure CopyTo(const AList: TList);

    function Count: Integer;
    property Items[const AIndex: Integer]: TTagInfo2 read GetItem; default;
    property Owner: TDataList read FOwner;
  end;

  TMyStringList = class (TStringList)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    Data: TTagInfo2;
    GroupName: string;
  end;

  TOnLoadingEvent = procedure(const APercent: Byte) of object;

  TDataList = class
  private
    FTagList: TTagList2;
    FData: TMyStringList;
    FFilteredInds: TList;
    FOnChanged: TNotifyEvent;
    FOnLoading: TOnLoadingEvent;
    FOnLoaded: TNotifyEvent;
    FBuildInProgress: Boolean;
    FUpdateCount: Integer;
    function GetFilteredRowCount: Integer;
    function GetRowCount: Integer;
    function GetFilteredRow(const AIndex: Integer): string;
    function GetRow(const AIndex: Integer): string;
    procedure BuildIndexForTag(const ATag: TTagInfo2);
    procedure BuildIndexForTags();
    procedure BuildFilteredIndex;
    function GetTag(const AIndex: Integer): TTagInfo2;
    function GetTagCount: Integer;
    procedure DoOnChange;
    procedure DoOnLoading(const APercent: Byte);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
//    function In
    function GetFilteredRowNumber(const ACurrentRow: Integer): Integer;

    property Rows[const AIndex: Integer]: string read GetRow;
    property RowCount: Integer read GetRowCount;
    property FilteredRows[const AIndex: Integer]: string read GetFilteredRow;
    property FilteredRowCount: Integer read GetFilteredRowCount;
    property Tags[const AIndex: Integer]: TTagInfo2 read GetTag;
    property TagCount: Integer read GetTagCount;
    property TagList: TTagList2 read FTagList;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnLoading: TOnLoadingEvent read FOnLoading write FOnLoading;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
  end;

  TOptions = class
  private
    FOpenedFileNames, FHistoryFileNames: TStrings;
  public
    CaseSens: Boolean;
    FontName: string;
    FontSize: Integer;
    TwoWindow: Boolean;
    SaveOnExit: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure SaveOptions;
    procedure LoadOptions;

    procedure AddToHistory(const AFileName: string);

    property OpenedFileNames: TStrings read FOpenedFileNames;
    property HistoryFileNames: TStrings read FHistoryFileNames;
  end;

function Options: TOptions;

implementation

uses
  SysUtils, StrUtils, Forms, Types,

  uConsts;

const
  cLeftBrace = '(_';
  cRightBrace = '_)';

var
  gOptions: TOptions;

function Options: TOptions;
begin
  if gOptions = nil then
    gOptions := TOptions.Create;
  Result := gOptions;
end;
{ TTagInfo }

constructor TTagInfo2.Create(const AName: string = ''; const AEnabled: Boolean = True; const AColor: TColor = clHighlightText;
  const AGroupName: string = '');
begin
  Name := AName;
  FEnabled := AEnabled;
  FColor := AColor;
  FGroupName := AGroupName;
  FIndexRows := TList.Create;
  FIndexed := False;
end;

destructor TTagInfo2.Destroy;
begin
  FreeAndNil(FIndexRows);
  inherited;
end;

procedure TTagInfo2.DoIndex(const AText: string; const AUpperText: string; const ARowIndex: Integer);
begin
  if (Options.CaseSens and (Pos(Name, AText) > 0)) or ((not Options.CaseSens) and (Pos(FUpperCaseName, AUpperText) > 0))  then
    FIndexRows.Add(TObject(ARowIndex));
end;

function TTagInfo2.GetFullName: string;
begin
  Result := Name;
  if MatchCount > 0 then
    Result := Result + '  (' + IntToStr(MatchCount) + ')'; 
end;

function TTagInfo2.GetMatchCount: Integer;
begin
  Result := FIndexRows.Count;
end;

function TTagInfo2.GetMatchRow(const AIndex: Integer): Integer;
begin
  Result := Integer(FIndexRows[AIndex]); 
end;

procedure TTagInfo2.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if Assigned(FOwner.FOwner) then
    FOwner.FOwner.BuildFilteredIndex;
end;

procedure TTagInfo2.SetName(const Value: string);
begin
  FName := Value;
  FUpperCaseName := UpperCase(FName);
end;

{ TTagList }

procedure TTagList2.Add(const ATagInfo: TTagInfo2);
begin
  FList.Add(ATagInfo);
  ATagInfo.FOwner := Self;
end;

procedure TTagList2.CheckAll(const ACheck: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FEnabled := ACheck;
  FOwner.BuildFilteredIndex;  
end;

procedure TTagList2.CopyTo(const AList: TList);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to FList.Count - 1 do
    AList.Add(FList[i]);
end;

function TTagList2.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TTagList2.Create;
begin
  FList := TList.Create;
end;

destructor TTagList2.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TTagInfo2(FList[i]).Free;
  FreeAndNil(FList);
  inherited;
end;

function TTagList2.GetItem(const AIndex: Integer): TTagInfo2;
begin
  Result := TTagInfo2(FList[AIndex]);
end;

procedure TTagList2.Load(const ASectionName: string);
var
  i: Integer;
  vIni: TMemIniFile;
  vValues, vSplit: TStringList;
  vName: string;
begin
  if not FileExists(gSettingsFileName) then Exit;

  FSectionName := ASectionName;

  FList.Clear;

  vIni := TMemIniFile.Create(gSettingsFileName);
  vValues := TStringList.Create;
  vSplit := TStringList.Create;
  vSplit.Delimiter := ';';
  vIni.ReadSection(FSectionName, vValues);
  for i := 0 to vValues.Count - 1 do
  begin
    vName := vValues[i];
    vSplit.DelimitedText := vIni.ReadString(FSectionName, vName, '0;0;[dt]');
    if (Pos(cLeftBrace, vName) > 0) or (Pos(cRightBrace, vName) > 0) then
    begin
      vName := AnsiReplaceText(vName, cLeftBrace, '[');
      vName := AnsiReplaceText(vName, cRightBrace, ']');
    end;
    Add(TTagInfo2.Create(vName, vSplit[0] = '1', StrToIntDef(vSplit[1], 0), vSplit[2]));
  end;

  vIni.Free;
  vValues.Free;
  vSplit.Free;
end;

procedure TTagList2.Remove(const ATagInfo: TTagInfo2);
var
  i: Integer;
begin
  i := FList.IndexOf(ATagInfo);
  if i > -1 then
  begin
    FList.Remove(ATagInfo);
    ATagInfo.Free;
  end;
end;

procedure TTagList2.Save;
var
  vIni: TIniFile;
  i: Integer;
  vName: string;
begin
  vIni := TIniFile.Create(gSettingsFileName);

  vIni.EraseSection(FSectionName);

  for i := 0 to FList.Count - 1 do
  begin
    vName := Items[i].Name;
    if (Pos('[', vName) > 0) or (Pos(']', vName) > 0) then
    begin
      vName := AnsiReplaceText(vName, '[', cLeftBrace);
      vName := AnsiReplaceText(vName, ']', cRightBrace);
    end;
    vIni.WriteString(FSectionName, vName, IfThen(Items[i].Enabled, '1', '0') + ';' + IntToStr(Items[i].Color) + ';' + Items[i].GroupName);
  end;

  vIni.Free;
end;

{ TMyStringList }

procedure TMyStringList.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
  P, Start: PAnsiChar;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);

    P := PAnsiChar(S);
    if P <> nil then
    begin
      Start := P;
      while P - Start < Size do
      begin
        if P^ = #0 then
          P^ := #42;
        Inc(P);
      end;
    end;
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

{ TDataList }

function SortAsc(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2);
end;

procedure TDataList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDataList.BuildFilteredIndex;
var
  i, j: Integer;
  tempList: TList;
  rows: TList;
begin
  if FUpdateCount > 0 then Exit;

  FBuildInProgress := True;
  tempList := TList.Create;
  tempList.Count := FData.Count;
  try
    BuildIndexForTags;
    FFilteredInds.Clear;
    for i := 0 to FTagList.Count - 1 do
    begin
      if FTagList.Items[i].Enabled then
      begin
        rows := FTagList.Items[i].FIndexRows;
        for j := 0 to rows.Count - 1 do
          tempList[Integer(rows[j])] := Pointer(1);      
        //BuildIndexForTag(FTagList.Items[i]);
        //FFilteredInds.Assign(FTagList.Items[i].FIndexRows, laOr);
      end;
      //DoOnLoading(10 + Trunc(i/FTagList.Count * 90));
    end;
    //FFilteredInds.Sort(SortAsc);

    for j := 0 to tempList.Count - 1 do
      if Assigned(tempList[j]) then
        FFilteredInds.Add(Pointer(j));

  finally
    FBuildInProgress := False;
    tempList.Free;

    DoOnChange;
    if Assigned(FOnLoaded) then
      FOnLoaded(Self);
  end;
end;

procedure TDataList.BuildIndexForTags();
var
  i, j: Integer;
  tag: TTagInfo2;
  tags: TList;
  upperText: String;
begin
  tags := TList.Create;
  try
    for i := 0 to FTagList.Count - 1 do
    begin
      tag := FTagList.Items[i];
      if tag.Enabled and not tag.FIndexed then
      begin
        tags.Add(FTagList.Items[i]);
        tag.FIndexed := True;
      end;
    end;

    if tags.Count = 0 then
      Exit;

    for i := 0 to FData.Count - 1 do
    begin
      upperText := UpperCase(FData[i]);
      for j := 0 to tags.Count - 1 do
        TTagInfo2(tags[j]).DoIndex(FData[i], upperText, i);
    end;

  finally
    tags.Free;
  end;
end;


procedure TDataList.BuildIndexForTag(const ATag: TTagInfo2);
var
  i: Integer;
begin
  if ATag.FIndexed then
    Exit; // index already builded

  for i := 0 to FData.Count - 1 do
    ATag.DoIndex(FData[i], UpperCase(FData[i]), i);

  ATag.FIndexed := True;
end;

constructor TDataList.Create;
begin
  FData := TMyStringList.Create;
  FTagList := TTagList2.Create;
  FTagList.FOwner := Self;
  FFilteredInds := TList.Create;
  FBuildInProgress := False;
  FUpdateCount := 0;
end;

destructor TDataList.Destroy;
begin
  FreeAndNil(FFilteredInds);
  FreeAndNil(FTagList);
  FreeAndNil(FData);
  inherited;
end;

procedure TDataList.DoOnChange;
begin
  if Assigned(FOnChanged) and (FUpdateCount = 0) then
    FOnChanged(Self);
end;

procedure TDataList.DoOnLoading(const APercent: Byte);
begin
  if not Assigned(FOnLoading) then Exit;
  Application.ProcessMessages;
  FOnLoading(APercent);
end;

procedure TDataList.EndUpdate;
begin
  if FUpdateCount = 0 then Exit;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    BuildFilteredIndex;
end;

function TDataList.GetFilteredRow(const AIndex: Integer): string;
begin
  if FBuildInProgress then
    Result := ''
  else
    Result := FData[Integer(FFilteredInds[AIndex])];
end;

function TDataList.GetFilteredRowCount: Integer;
begin
  Result := FFilteredInds.Count;
end;

function TDataList.GetFilteredRowNumber(const ACurrentRow: Integer): Integer;
begin
  if FBuildInProgress or (ACurrentRow - 1 > FFilteredInds.Count) then
    Result := -1
  else
    Result := Integer(FFilteredInds[ACurrentRow]);
end;

function TDataList.GetRow(const AIndex: Integer): string;
begin
  Result := FData[AIndex];
end;

function TDataList.GetRowCount: Integer;
begin
  Result := FData.Count;
end;

function TDataList.GetTag(const AIndex: Integer): TTagInfo2;
begin
  Result := FTagList.Items[AIndex];
end;

function TDataList.GetTagCount: Integer;
begin
  Result := FTagList.Count;
end;

procedure TDataList.LoadFromFile(const AFileName: string);
begin
  Application.ProcessMessages;
  FData.LoadFromFile(AFileName);
  DoOnLoading(5);
  FTagList.Load('tags');
  DoOnLoading(10);
  BuildFilteredIndex;

end;

{ TOptions }

procedure TOptions.AddToHistory(const AFileName: string);
begin
  if HistoryFileNames.IndexOf(AFileName) < 0 then
    HistoryFileNames.Add(AFileName);
  if HistoryFileNames.Count > 10 then
    HistoryFileNames.Delete(0);
end;

constructor TOptions.Create;
begin
  FOpenedFileNames := TStringList.Create;
  FHistoryFileNames := TStringList.Create;
end;

destructor TOptions.Destroy;
begin
  FreeAndNil(FHistoryFileNames);
  FreeAndNil(FOpenedFileNames);
  inherited;
end;

procedure TOptions.LoadOptions;
var
  vIni: TIniFile;
  vFiles: TStrings;
  i: Integer;
begin
  vIni := TIniFile.Create(gSettingsFileName);

  FontName := vIni.ReadString('options', 'font', 'Courier');
  FontSize := vIni.ReadInteger('options', 'font_size', 8);
  CaseSens := vIni.ReadBool('options', 'case_sens', False);
  TwoWindow := vIni.ReadBool('options', 'two_window', False);
  SaveOnExit := True;
  vFiles := TStringList.Create;
  vIni.ReadSectionValues('files', vFiles);
  FOpenedFileNames.Clear;
  for i := 0 to vFiles.Count - 1 do
    FOpenedFileNames.Add(vFiles.ValueFromIndex[i]);

  vIni.ReadSectionValues('history', vFiles);
  FHistoryFileNames.Clear;
  for i := 0 to vFiles.Count - 1 do
    FHistoryFileNames.Add(vFiles.ValueFromIndex[i]);

  vIni.Free;
  vFiles.Free;
end;

procedure TOptions.SaveOptions;
var
  vIni: TIniFile;
  i: Integer;
begin
  vIni := TIniFile.Create(gSettingsFileName);
  vIni.WriteString('options', 'font', FontName);
  vIni.WriteInteger('options', 'font_size', FontSize);
  vIni.WriteBool('options', 'case_sens', CaseSens);
  vIni.WriteBool('options', 'two_window', TwoWindow);

  vIni.EraseSection('files');
  for i := 0 to FOpenedFileNames.Count - 1 do
    vIni.WriteString('files', 'file' + IntToStr(i), FOpenedFileNames[i]);

  vIni.EraseSection('history');
  for i := 0 to FHistoryFileNames.Count - 1 do
    vIni.WriteString('history', 'file' + IntToStr(i), FHistoryFileNames[i]);

  vIni.Free;
end;

end.
