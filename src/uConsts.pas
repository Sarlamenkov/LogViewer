unit uConsts;

interface

function GetFileVersion: string;

implementation

uses
  Windows, SysUtils;

function GetFileVersion: string;
var
  szName: array[0..255] of Char;
  P: Pointer;
  Value: Pointer;
  Len: UINT;
  GetTranslationString: string;
  FFileName: PChar;
  FValid: boolean;
  FSize: DWORD;
  FHandle: DWORD;
  FBuffer: PChar;
  AFileName: string;
begin
  FSize := 0;
  FFileName := '';
  FBuffer := nil;
  try
    AFileName := ParamStr(0);
    FFileName := StrPCopy(StrAlloc(Length(AFileName) + 1), AFileName);
    FValid := False;
    FSize := GetFileVersionInfoSize(FFileName, FHandle);
    if FSize > 0 then
    try
      GetMem(FBuffer, FSize);
      FValid := GetFileVersionInfo(FFileName, FHandle, FSize, FBuffer);
    except
      raise;
    end;
    Result := '';
    if FValid then
      VerQueryValue(FBuffer, '\VarFileInfo\Translation', p, Len)
    else
      p := nil;
    if P <> nil then
      GetTranslationString := IntToHex(MakeLong(HiWord(Longint(P^)),
        LoWord(Longint(P^))), 8);
    if FValid then
    begin
      StrPCopy(szName, '\StringFileInfo\' + GetTranslationString +
        '\FileVersion');
      if VerQueryValue(FBuffer, szName, Value, Len) then
        Result := StrPas(PChar(Value));
    end;
  finally
    try
      if FBuffer <> nil then
        FreeMem(FBuffer, FSize);
    except
    end;
    try
      StrDispose(FFileName);
    except
    end;
  end;
end;

end.
