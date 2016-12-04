unit StructsUnit;

interface

uses
  Graphics, Classes, IniFiles;

type
  TTagInfo = class
  private
    FName: string;
    FColor: TColor;
    FEnabled: Boolean;
    FGroupName: string;
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(const AName: string = ''; const AEnabled: Boolean = True;
      const AColor: TColor = clHighlightText; const AGroupName: string = '');

    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Color: TColor read FColor write SetColor;
    property GroupName: string read FGroupName write FGroupName;
  end;

  TTagList = class
  private
    FList: TList;
    FActiveTags: TList;
    FSkip: TStringList;
    FCaseSens: Boolean;
    FFontName: String;
    FFontSize: Integer;
    FTwoWindow: Boolean;
    FSaveOnExit: Boolean;
    function GetItem(const AIndex: Integer): TTagInfo;
    function GetSkipText: string;
    procedure SetSkipText(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    procedure Add(const ATagInfo: TTagInfo);
    procedure Remove(const ATagInfo: TTagInfo);

    procedure Actualize;
    function DoSkip(const AText: string): string;

    function IsMatch(ATextRow: string): TTagInfo;
    function Count: Integer;
    property Items[const AIndex: Integer]: TTagInfo read GetItem; default;
    property SkipText: string read GetSkipText write SetSkipText;
    property Skip: TStringList read FSkip;
    property FontName: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property CaseSens: Boolean read FCaseSens write FCaseSens;
    property TwoWindow: Boolean read FTwoWindow write FTwoWindow;
    property SaveOnExit: Boolean read FSaveOnExit write FSaveOnExit;
  end;

  TMyStringList = class (TStringList)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

var
  gSettingsFileName: string;

implementation

uses
  SysUtils, StrUtils;

const
  cLeftBrace = '(_';
  cRightBrace = '_)';

{ TTagInfo }

constructor TTagInfo.Create(const AName: string = ''; const AEnabled: Boolean = True; const AColor: TColor = clHighlightText;
  const AGroupName: string = '');
begin
  FName := AName;
  FEnabled := AEnabled;
  FColor := AColor;
  FGroupName := AGroupName;
end;

procedure TTagInfo.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TTagInfo.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

{ TTagList }

procedure TTagList.Actualize;
var
  i: Integer;
  vMatchedTag: TTagInfo;
begin
  FActiveTags.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    vMatchedTag := GetItem(i);
    if vMatchedTag.Enabled then
      FActiveTags.Add(vMatchedTag);
  end;
end;

procedure TTagList.Add(const ATagInfo: TTagInfo);
begin
  FList.Add(ATagInfo);
end;

function TTagList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TTagList.Create;
begin
  FList := TList.Create;
  FActiveTags := TList.Create;
  FSkip := TStringList.Create;
  FSkip.Delimiter := ';';
end;

destructor TTagList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TTagInfo(FList[i]).Free;
  FreeAndNil(FList);
  FreeAndNil(FActiveTags);
  FreeAndNil(FSkip);
  inherited;
end;

function TTagList.DoSkip(const AText: string): string;
var
  i: Integer; 
begin
  Result := AText;  
  for i := 0 to FSkip.Count - 1 do
    Result := AnsiReplaceText(Result, FSkip[i], '');
end;

function TTagList.GetItem(const AIndex: Integer): TTagInfo;
begin
  Result := TTagInfo(FList[AIndex]);
end;

function TTagList.GetSkipText: string;
begin
  Result := FSkip.DelimitedText;
end;

function TTagList.IsMatch(ATextRow: string): TTagInfo;
var
  i: Integer;
  tagName: string;
begin
  Result := nil;
  if not FCaseSens then
    ATextRow := UpperCase(ATextRow);
  for i := 0 to FActiveTags.Count - 1 do
  begin
    tagName := TTagInfo(FActiveTags[i]).Name;
    if not FCaseSens then
      tagName := UpperCase(tagName);
    if Pos(tagName, ATextRow) > 0 then
    begin
      Result := FActiveTags[i];
      Exit;
    end;
  end;
end;

procedure TTagList.Load;
var
  i: Integer;
  vIni: TMemIniFile;
  vValues, vSplit: TStringList;
  vName: string;
begin
  if not FileExists(gSettingsFileName) then Exit;

  FList.Clear;

  vIni := TMemIniFile.Create(gSettingsFileName);
  vValues := TStringList.Create;
  vSplit := TStringList.Create;
  vSplit.Delimiter := ';';
  vIni.ReadSection('tags', vValues);
  for i := 0 to vValues.Count - 1 do
  begin
    vName := vValues[i];
    vSplit.DelimitedText := vIni.ReadString('tags', vName, '0;0;[dt]');
    if (Pos(cLeftBrace, vName) > 0) or (Pos(cRightBrace, vName) > 0) then
    begin
      vName := AnsiReplaceText(vName, cLeftBrace, '[');
      vName := AnsiReplaceText(vName, cRightBrace, ']');
    end;
    Add(TTagInfo.Create(vName, vSplit[0] = '1', StrToIntDef(vSplit[1], 0), vSplit[2]));
  end;

  FFontName := vIni.ReadString('options', 'font', 'Courier');
  FFontSize := vIni.ReadInteger('options', 'font_size', 8);
  FCaseSens := vIni.ReadBool('options', 'case_sens', False);
  FTwoWindow := vIni.ReadBool('options', 'two_window', False);
  FSaveOnExit := True;

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
  vName: string;
begin
  vIni := TIniFile.Create(gSettingsFileName);

  vIni.EraseSection('tags');

  for i := 0 to FList.Count - 1 do
  begin
    vName := Items[i].Name;
    if (Pos('[', vName) > 0) or (Pos(']', vName) > 0) then
    begin
      vName := AnsiReplaceText(vName, '[', cLeftBrace);
      vName := AnsiReplaceText(vName, ']', cRightBrace);
    end;
    vIni.WriteString('tags', vName, IfThen(Items[i].Enabled, '1', '0') + ';' + IntToStr(Items[i].Color) + ';' + Items[i].GroupName);
  end;

  vIni.WriteString('options', 'font', FFontName);
  vIni.WriteInteger('options', 'font_size', FFontSize);
  vIni.WriteBool('options', 'case_sens', FCaseSens);
  vIni.WriteBool('options', 'two_window', FTwoWindow);

  vIni.Free;
end;

procedure TTagList.SetSkipText(const Value: string);
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

end.
