unit Structs2Unit;

interface

uses
  Graphics, Classes, IniFiles;

type
  TTagInfo2 = class
  private
    FName: string;
    FColor: TColor;
    FEnabled: Boolean;
    FGroupName: string;
    FMatchCount: Integer;
    FIndexRows: TList;
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(const AName: string = ''; const AEnabled: Boolean = True;
      const AColor: TColor = clHighlightText; const AGroupName: string = '');
    destructor Destroy; override;

    procedure DoIndex(const AText: string; const ARowIndex: Integer);

    property MatchCount: Integer read FMatchCount;
    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Color: TColor read FColor write SetColor;
    property GroupName: string read FGroupName write FGroupName;
  end;

  TTagList2 = class
  private
    FSectionName: string;
    FList: TList;
    FActiveTags: TList;
    FSkip: TStringList;

    function GetItem(const AIndex: Integer): TTagInfo2;
    function GetSkipText: string;
    procedure SetSkipText(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const ASectionName: string);
    procedure Save;

    procedure Add(const ATagInfo: TTagInfo2);
    procedure Remove(const ATagInfo: TTagInfo2);

    procedure CopyTo(const AList: TList);
    procedure Actualize;
    function DoSkip(const AText: string): string;

    function IsMatch(ATextRow: string): TTagInfo2;
    function Count: Integer;
    property Items[const AIndex: Integer]: TTagInfo2 read GetItem; default;
    property SkipText: string read GetSkipText write SetSkipText;
    property Skip: TStringList read FSkip;
  end;

  TMyStringList = class (TStringList)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TNodeData = record
    Data: TTagInfo2;
    GroupName: string;
  end;

  TDataList = class
  private
    FTagList: TTagList2;
    FData: TMyStringList;
    FFilteredInds: TList;
    function GetFilteredRowCount: Integer;
    function GetRowCount: Integer;
    function GetFilteredRow(const AIndex: Integer): string;
    function GetRow(const AIndex: Integer): string;
    procedure BuildIndexForTag(const ATag: TTagInfo2);
    procedure BuildFilteredIndex;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);

    property Rows[const AIndex: Integer]: string read GetRow;
    property RowCount: Integer read GetRowCount;
    property FilteredRows[const AIndex: Integer]: string read GetFilteredRow;
    property FilteredRowCount: Integer read GetFilteredRowCount;
  end;

implementation

uses
  SysUtils, StrUtils, uConsts, StructsUnit;

const
  cLeftBrace = '(_';
  cRightBrace = '_)';

{ TTagInfo }

constructor TTagInfo2.Create(const AName: string = ''; const AEnabled: Boolean = True; const AColor: TColor = clHighlightText;
  const AGroupName: string = '');
begin
  FName := AName;
  FEnabled := AEnabled;
  FColor := AColor;
  FGroupName := AGroupName;
  FIndexRows := TList.Create;  
end;

destructor TTagInfo2.Destroy;
begin
  FreeAndNil(FIndexRows);
  inherited;
end;

procedure TTagInfo2.DoIndex(const AText: string; const ARowIndex: Integer);
begin
  if (Options.CaseSens and (Pos(Name, AText) > 0))
    or ((not Options.CaseSens) and (Pos(UpperCase(Name), UpperCase(AText)) > 0))  then
    FIndexRows.Add(TObject(ARowIndex));
end;

procedure TTagInfo2.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TTagInfo2.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

{ TTagList }

procedure TTagList2.Actualize;
var
  i: Integer;
  vMatchedTag: TTagInfo2;
begin
  FActiveTags.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    vMatchedTag := GetItem(i);
    vMatchedTag.FMatchCount := 0;
    if vMatchedTag.Enabled then
      FActiveTags.Add(vMatchedTag);
  end;
end;

procedure TTagList2.Add(const ATagInfo: TTagInfo2);
begin
  FList.Add(ATagInfo);
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
  FActiveTags := TList.Create;
  FSkip := TStringList.Create;
  FSkip.Delimiter := ';';

end;

destructor TTagList2.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TTagInfo2(FList[i]).Free;
  FreeAndNil(FList);
  FreeAndNil(FActiveTags);
  FreeAndNil(FSkip);
  inherited;
end;

function TTagList2.DoSkip(const AText: string): string;
var
  i: Integer; 
begin
  Result := AText;  
  for i := 0 to FSkip.Count - 1 do
    Result := AnsiReplaceText(Result, FSkip[i], '');
end;

function TTagList2.GetItem(const AIndex: Integer): TTagInfo2;
begin
  Result := TTagInfo2(FList[AIndex]);
end;

function TTagList2.GetSkipText: string;
begin
  Result := FSkip.DelimitedText;
end;

function TTagList2.IsMatch(ATextRow: string): TTagInfo2;
var
  i: Integer;
  tagName: string;
begin
  Result := nil;
  if not Options.CaseSens then
    ATextRow := UpperCase(ATextRow);
  for i := 0 to FActiveTags.Count - 1 do
  begin
    tagName := TTagInfo2(FActiveTags[i]).Name;
    if not Options.CaseSens then
      tagName := UpperCase(tagName);
    if Pos(tagName, ATextRow) > 0 then
    begin
      Result := FActiveTags[i];
      Exit;
    end;
  end;
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

procedure TTagList2.SetSkipText(const Value: string);
begin
  FSkip.DelimitedText := Value;
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

procedure TDataList.BuildFilteredIndex;
var
  i: Integer;
begin
  FFilteredInds.Clear;
  for i := 0 to FTagList.Count - 1 do
    if FTagList.Items[i].Enabled then
    begin
      BuildIndexForTag(FTagList.Items[i]);
      FFilteredInds.Assign(FTagList.Items[i].FIndexRows, laOr);
    end;
  FFilteredInds.Sort(SortAsc);
end;

procedure TDataList.BuildIndexForTag(const ATag: TTagInfo2);
var
  i: Integer;
begin
  if ATag.FIndexRows.Count > 0 then Exit; // index already builded
  for i := 0 to FData.Count - 1 do
    ATag.DoIndex(FData[i], i);
end;

constructor TDataList.Create;
begin
  FData := TMyStringList.Create;
  FTagList := TTagList2.Create;
  FFilteredInds := TList.Create;
end;

destructor TDataList.Destroy;
begin
  FreeAndNil(FFilteredInds);
  FreeAndNil(FTagList);
  FreeAndNil(FData);
  inherited;
end;

function TDataList.GetFilteredRow(const AIndex: Integer): string;
begin
  Result := FData[Integer(FFilteredInds[AIndex])];
end;

function TDataList.GetFilteredRowCount: Integer;
begin
  Result := FFilteredInds.Count;
end;

function TDataList.GetRow(const AIndex: Integer): string;
begin
  Result := FData[AIndex];
end;

function TDataList.GetRowCount: Integer;
begin
  Result := FData.Count;
end;

procedure TDataList.LoadFromFile(const AFileName: string);
begin
  FData.LoadFromFile(AFileName);
  FTagList.Load('tags');
  BuildFilteredIndex;
end;

end.
