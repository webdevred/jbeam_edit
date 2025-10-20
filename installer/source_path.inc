(* -*- mode: pascal -*- *)

function SendMessageTimeoutA(hWnd: Integer; Msg: Integer;
  wParam: Longint; lParam: Longint; fuFlags: Longint;
  uTimeout: Longint; var lpdwResult: Longint): Longint;
  external 'SendMessageTimeoutA@user32.dll stdcall';

procedure BroadcastEnvChange;
var
  resultValue: Longint;
begin
  SendMessageTimeoutA($FFFF, $1A, 0, 0, $0002, 5000, resultValue);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[Length(Result)] = '\') then
    Delete(Result, Length(Result), 1);
end;

procedure AddPathEntry(const Entry: string);
var
  Key, PathValue, UpperPath, NormalizedEntry: string;
begin
  Key := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  if not RegQueryStringValue(HKLM, Key, 'Path', PathValue) then
    PathValue := '';
  NormalizedEntry := UpperCase(ExcludeTrailingPathDelimiter(Entry));
  UpperPath := UpperCase(';' + PathValue + ';');
  if (Pos(';' + NormalizedEntry + ';', UpperPath) = 0) and
     (Pos(';' + NormalizedEntry + '\;', UpperPath) = 0) then
  begin
    if PathValue = '' then
      PathValue := Entry
    else
      PathValue := PathValue + ';' + Entry;
    RegWriteExpandStringValue(HKLM, Key, 'Path', PathValue);
    BroadcastEnvChange;
  end;
end;

function ReplaceText(const S, OldPattern, NewPattern: string): string;
var
  P: Integer;
  Temp: string;
begin
  Temp := S;
  P := Pos(OldPattern, Temp);
  while P > 0 do
  begin
    Delete(Temp, P, Length(OldPattern));
    Insert(NewPattern, Temp, P);
    P := Pos(OldPattern, Temp);
  end;
  Result := Temp;
end;

procedure RemovePathEntry(const Entry: string);
var
  Key, PathValue, Cleaned, NormalizedEntry, UpperPath: string;
begin
  Key := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  if not RegQueryStringValue(HKLM, Key, 'Path', PathValue) then Exit;
  NormalizedEntry := UpperCase(ExcludeTrailingPathDelimiter(Entry));
  UpperPath := UpperCase(';' + PathValue + ';');
  Cleaned := ReplaceText(UpperPath, ';' + NormalizedEntry + ';', ';');
  Cleaned := ReplaceText(Cleaned, ';' + NormalizedEntry + '\;', ';');
  if (Length(Cleaned) > 0) and (Cleaned[1] = ';') then Delete(Cleaned, 1, 1);
  if (Length(Cleaned) > 0) and (Cleaned[Length(Cleaned)] = ';') then Delete(Cleaned, Length(Cleaned), 1);
  RegWriteExpandStringValue(HKLM, Key, 'Path', Cleaned);
  BroadcastEnvChange;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
    AddPathEntry(ExpandConstant('{app}'));
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usPostUninstall then
    RemovePathEntry(ExpandConstant('{app}'));
end;
