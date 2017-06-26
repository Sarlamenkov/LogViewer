unit uStructs;

interface

uses
  Graphics, Classes, IniFiles;

type
  TDataList = class;
  TTagList = class;
  
  TTagInfo = class
  private
    FOwner: TTagList;
    FName: string;
    FUpperCaseName: string;
    FColor: TColor;
    FEnabled: Boolean;
    FGroupName: string;
    FIndexRows: TList;
    FIndexed: Boolean;
    FCaseSens: Boolean;
    FOrder: Integer;
    FRegExp: Boolean;
    FDesc: string;
    procedure SetEnabled(const Value: Boolean);
    function GetMatchCount: Integer;
    function GetFullName: string;
    procedure SetName(const Value: string);
    function GetMatchRow(const AIndex: Integer): Integer;
    procedure SetCaseSens(const Value: Boolean);
    procedure DoIndex(const AText: string; const AUpperText: string; const ARowIndex: Integer);
    procedure SetRegExp(const Value: Boolean);
    procedure RebuildIndex;
  public
    constructor Create(const AName: string = ''; const AEnabled: Boolean = True;
      const AColor: TColor = clHighlightText; const AGroupName: string = '');
    destructor Destroy; override;

    function IsMatchToRow(const ARowIndex): Boolean;

    property MatchRows[const AIndex: Integer]: Integer read GetMatchRow;
    property MatchCount: Integer read GetMatchCount;
    property Name: string read FName write SetName;
    property Description: string read FDesc write FDesc;
    property UpperCaseName: string read FUpperCaseName;
    property FullName: string read GetFullName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property CaseSens: Boolean read FCaseSens write SetCaseSens;
    property RegExp: Boolean read FRegExp write SetRegExp;
    property Color: TColor read FColor write FColor;
    property GroupName: string read FGroupName write FGroupName;
    property Order: Integer read FOrder write FOrder;
  end;

  TSortType = (stAlphaSort, stCheckedSort, stNoSort);

  TTagList = class
  private
    FOwner: TDataList;
    FSectionName: string;
    FList: TList;
    function GetItem(const AIndex: Integer): TTagInfo;
    function Find(const AName: string): TTagInfo;
    procedure InternalAdd(const ATagInfo: TTagInfo);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const ASectionName: string);
    procedure Save;

    procedure Add(const ATagInfo: TTagInfo);
    procedure Remove(const ATagInfo: TTagInfo);
    procedure AddTag(const AName: string; const AGroupName: string = '');

    procedure CheckAll(const ACheck: Boolean);

    procedure CopyTo(const AList: TList);

    procedure Sort(const ASortType: TSortType; const AIndices: TList);

    function Count: Integer;
    property Items[const AIndex: Integer]: TTagInfo read GetItem; default;
    property Owner: TDataList read FOwner;
  end;

  {TProcessView = class
  public
    property StartPattern: string;
    property EndPattern: string;
  end;           }

  TMyStringList = class (TStringList)
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromFile(const FileName: string); override;
  end;

  PNodeData = ^TNodeData;
  TNodeData = record
    Data: TTagInfo;
    GroupName: string;
  end;

  TOnLoadingEvent = procedure(const APercent: Byte) of object;

  TDataList = class
  private
    FTagList: TTagList;
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
    procedure BuildIndexForTags;
    procedure BuildFilteredIndex;
    function GetTag(const AIndex: Integer): TTagInfo;
    function GetTagCount: Integer;
    procedure DoOnChange;
    procedure DoOnLoading(const APercent: Byte);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;

    function GetFilteredRowNumber(const ACurrentRow: Integer): Integer;
    function RowInFilter(const ACurrentRow: Integer): Boolean;

    property Rows[const AIndex: Integer]: string read GetRow;
    property RowCount: Integer read GetRowCount;
    property FilteredRows[const AIndex: Integer]: string read GetFilteredRow;
    property FilteredRowCount: Integer read GetFilteredRowCount;
    property Tags[const AIndex: Integer]: TTagInfo read GetTag;
    property TagCount: Integer read GetTagCount;
    property TagList: TTagList read FTagList;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnLoading: TOnLoadingEvent read FOnLoading write FOnLoading;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
  end;

  TOptions = class
  private
    FOpenedFileNames, FHistoryFileNames: TStrings;
  public
//    CaseSens: Boolean;
    FontName: string;
    FontSize: Integer;
    TwoWindow: Boolean;
    SaveOnExit: Boolean;
    InterlacedBacklight: Boolean;
    MainWindowState: Integer;

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
  SysUtils, StrUtils, Forms,

  uConsts, RegExpr, uGraphicUtils;

const
  cLeftBrace = '(_';
  cRightBrace = '_)';
  cDelim = '_!_';

var
  gOptions: TOptions;

function Options: TOptions;
begin
  if gOptions = nil then
    gOptions := TOptions.Create;
  Result := gOptions;
end;

{ TTagInfo }

constructor TTagInfo.Create(const AName: string = ''; const AEnabled: Boolean = True; const AColor: TColor = clHighlightText;
  const AGroupName: string = '');
begin
  FIndexRows := TList.Create;
  FIndexed := False;
  FCaseSens := False;
  FEnabled := AEnabled;
  FColor := AColor;
  Name := AName;
  if Length(AGroupName) = 0 then
    FGroupName := 'No Group'
  else
    FGroupName := AGroupName;
end;

destructor TTagInfo.Destroy;
begin
  FreeAndNil(FIndexRows);
  inherited;
end;

procedure TTagInfo.RebuildIndex;
begin
  if Assigned(FOwner) and Assigned(FOwner.FOwner) then
    FOwner.FOwner.BuildFilteredIndex;
end;

procedure TTagInfo.DoIndex(const AText: string; const AUpperText: string; const ARowIndex: Integer);
begin
  if ((not RegExp) and
     ((CaseSens and (Pos(Name, AText) > 0)) or
     ((not CaseSens) and (Pos(FUpperCaseName, AUpperText) > 0)))) or
     (RegExp and ExecRegExpr(Name, AText)) then
    FIndexRows.Add(Pointer(ARowIndex));
end;

function TTagInfo.GetFullName: string;
begin
  Result := Name;
  if MatchCount > 0 then
    Result := Result + '  (' + IntToStr(MatchCount) + ')'; 
end;

function TTagInfo.GetMatchCount: Integer;
begin
  Result := FIndexRows.Count;
end;

function TTagInfo.GetMatchRow(const AIndex: Integer): Integer;
begin
  Result := Integer(FIndexRows[AIndex]); 
end;

function TTagInfo.IsMatchToRow(const ARowIndex): Boolean;
begin
  Result := FIndexRows.IndexOf(Pointer(ARowIndex)) > -1;
end;

procedure TTagInfo.SetCaseSens(const Value: Boolean);
begin
  if FCaseSens = Value then Exit;

  FCaseSens := Value;
  FIndexed := False;
  FIndexRows.Clear;
  RebuildIndex;
end;

procedure TTagInfo.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  RebuildIndex;
end;

procedure TTagInfo.SetName(const Value: string);
begin
  FName := Value;
  FUpperCaseName := UpperCase(FName);
  FIndexed := False;
  FIndexRows.Clear;
  RebuildIndex;
end;

procedure TTagInfo.SetRegExp(const Value: Boolean);
begin
  if FRegExp = Value then Exit;
  FRegExp := Value;
  FIndexed := False;
  FIndexRows.Clear;
  RebuildIndex;
end;

{ TTagList }

procedure TTagList.Add(const ATagInfo: TTagInfo);
begin
  InternalAdd(ATagInfo);
  if Assigned(FOwner) then
    FOwner.BuildFilteredIndex;
end;

procedure TTagList.AddTag(const AName, AGroupName: string);
var
  vTag: TTagInfo;
begin
  vTag := Find(AName);
  if vTag = nil then
  begin
    vTag := TTagInfo.Create(AName, True, RGB(Random(256), Random(256), Random(256)), AGroupName);
    Add(vTag);
  end;
end;

procedure TTagList.CheckAll(const ACheck: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FEnabled := ACheck;
  if Assigned(FOwner) then
    FOwner.BuildFilteredIndex;
end;

procedure TTagList.CopyTo(const AList: TList);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to FList.Count - 1 do
    AList.Add(FList[i]);
end;

function TTagList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TTagList.Create;
begin
  FList := TList.Create;
end;

destructor TTagList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TTagInfo(FList[i]).Free;
  FreeAndNil(FList);
  inherited;
end;

function TTagList.Find(const AName: string): TTagInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TTagList.GetItem(const AIndex: Integer): TTagInfo;
begin
  Result := TTagInfo(FList[AIndex]);
end;

procedure TTagList.InternalAdd(const ATagInfo: TTagInfo);
begin
  FList.Add(ATagInfo);
  ATagInfo.FOwner := Self;
end;

procedure TTagList.Load(const ASectionName: string);
var
  i: Integer;
  vIni: TMemIniFile;
  vValues, vSplit: TStringList;
  vName, vDesc: string;
  vTag: TTagInfo;
begin
  FSectionName := ASectionName;
  if not FileExists(gSettingsFileName) then Exit;

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
//    if (Pos(cLeftBrace, vName) > 0) or (Pos(cRightBrace, vName) > 0) then
//    begin
    vName := AnsiReplaceText(vName, cLeftBrace, '[');
    vName := AnsiReplaceText(vName, cRightBrace, ']');
    vName := AnsiReplaceText(vName, cDelim, ';');

//    end;

    vTag := TTagInfo.Create(vName, vSplit[0] = '1', StrToIntDef(vSplit[1], 0), vSplit[2]);
    if vSplit.Count > 3 then
      vTag.FCaseSens := vSplit[3] = '1';
    if vSplit.Count > 4 then
      vTag.FRegExp := vSplit[4] = '1';
    if vSplit.Count > 5 then
    begin
      vDesc := vSplit[5];
      vDesc := AnsiReplaceText(vDesc, cLeftBrace, '[');
      vDesc := AnsiReplaceText(vDesc, cRightBrace, ']');
      vDesc := AnsiReplaceText(vDesc, cDelim, ';');
      vTag.Description := vDesc;
    end;

    InternalAdd(vTag);
  end;

  vIni.Free;
  vValues.Free;
  vSplit.Free;
end;

procedure TTagList.Remove(const ATagInfo: TTagInfo);
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

procedure TTagList.Save;
var
  vIni: TIniFile;
  i: Integer;
  vName, vDesc: string;
begin
  vIni := TIniFile.Create(gSettingsFileName);

  vIni.EraseSection(FSectionName);

  for i := 0 to FList.Count - 1 do
  begin
   // if (Pos('[', vName) > 0) or (Pos(']', vName) > 0) then
   // begin
    vName := AnsiReplaceText(Items[i].Name, '[', cLeftBrace);
    vName := AnsiReplaceText(vName, ']', cRightBrace);
    vName := AnsiReplaceText(vName, ';', cDelim);
    vDesc := AnsiReplaceText(Items[i].Description, '[', cLeftBrace);
    vDesc := AnsiReplaceText(vDesc, ']', cRightBrace);
    vDesc := AnsiReplaceText(vDesc, ';', cDelim);
   // end;
    vIni.WriteString(FSectionName, vName,
      IfThen(Items[i].Enabled, '1', '0') + ';' +
      IntToStr(Items[i].Color) + ';' + Items[i].GroupName + ';' +
      IfThen(Items[i].CaseSens, '1', '0') + ';' +
      IfThen(Items[i].RegExp, '1', '0') + ';' +
      vDesc);
  end;

  vIni.Free;
end;

function SortAlpha(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TTagInfo(Item1).Name, TTagInfo(Item2).Name);
end;

function SortChecked(Item1, Item2: Pointer): Integer;
begin
  if TTagInfo(Item1).Enabled = TTagInfo(Item2).Enabled then
  begin
    if TTagInfo(Item1).Enabled then
      Result := TTagInfo(Item2).MatchCount - TTagInfo(Item1).MatchCount
    else
      Result := CompareText(TTagInfo(Item1).Name, TTagInfo(Item2).Name);
  end
  else  if TTagInfo(Item1).Enabled then
    Result := -1
  else
    Result := 1;
end;

procedure TTagList.Sort(const ASortType: TSortType; const AIndices: TList);
begin
  CopyTo(AIndices);
  case ASortType of
    stAlphaSort: AIndices.Sort(SortAlpha);
    stCheckedSort: AIndices.Sort(SortChecked);
    stNoSort:;
  end;
end;

{ TMyStringList }

procedure TMyStringList.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmShareDenyRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

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
          P^ := #42
        else if P^ = #9 then
          P^ := #95;
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
  vTempList: TList;
  vRows: TList;
begin
  if FUpdateCount > 0 then Exit;

  FBuildInProgress := True;
  vTempList := TList.Create;
  vTempList.Count := FData.Count;
  try
    BuildIndexForTags;

    for i := 0 to FTagList.Count - 1 do
      if FTagList.Items[i].Enabled then
      begin
        vRows := FTagList.Items[i].FIndexRows;
        for j := 0 to vRows.Count - 1 do
          vTempList[Integer(vRows[j])] := Pointer(1);
      end;

    FFilteredInds.Clear;
    for j := 0 to vTempList.Count - 1 do
      if Assigned(vTempList[j]) then
        FFilteredInds.Add(Pointer(j));

  finally
    vTempList.Free;
    FBuildInProgress := False;
    DoOnChange;
    if Assigned(FOnLoaded) then
      FOnLoaded(Self);
  end;
end;

procedure TDataList.BuildIndexForTags;
var
  i, j: Integer;
  vTag: TTagInfo;
  vTagList: TList;
  vUpperText: String;
begin
  vTagList := TList.Create;
  try
    for i := 0 to FTagList.Count - 1 do
    begin
      vTag := FTagList.Items[i];
      if vTag.Enabled and not vTag.FIndexed then
      begin
        vTagList.Add(FTagList.Items[i]);
        vTag.FIndexed := True;
      end;
    end;

    if vTagList.Count = 0 then
      Exit;

    for i := 0 to FData.Count - 1 do
    begin
      vUpperText := UpperCase(FData[i]);
      for j := 0 to vTagList.Count - 1 do
        TTagInfo(vTagList[j]).DoIndex(FData[i], vUpperText, i);
    end;
  finally
    vTagList.Free;
  end;
end;

constructor TDataList.Create;
begin
  FData := TMyStringList.Create;
  FTagList := TTagList.Create;
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
  if FBuildInProgress or (ACurrentRow > FFilteredInds.Count - 1) then
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

function TDataList.GetTag(const AIndex: Integer): TTagInfo;
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

function TDataList.RowInFilter(const ACurrentRow: Integer): Boolean;
begin
  Result := FFilteredInds.IndexOf(Pointer(ACurrentRow)) > -1;
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
 // CaseSens := vIni.ReadBool('options', 'case_sens', False);
  TwoWindow := vIni.ReadBool('options', 'two_window', False);
  InterlacedBacklight := vIni.ReadBool('options', 'interlaced_backlight', True);

  MainWindowState := vIni.ReadInteger('options', 'main_window_state', 0);
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
 // vIni.WriteBool('options', 'case_sens', CaseSens);
  vIni.WriteBool('options', 'two_window', TwoWindow);
  vIni.WriteBool('options', 'interlaced_backlight', InterlacedBacklight);
  vIni.WriteInteger('options', 'main_window_state', Integer(MainWindowState));

  vIni.EraseSection('files');
  for i := 0 to FOpenedFileNames.Count - 1 do
    vIni.WriteString('files', 'file' + IntToStr(i), FOpenedFileNames[i]);

  vIni.EraseSection('history');
  for i := 0 to FHistoryFileNames.Count - 1 do
    vIni.WriteString('history', 'file' + IntToStr(i), FHistoryFileNames[i]);

  vIni.Free;
end;

end.
