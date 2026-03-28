(* -*- mode: pascal -*- *)

const
  HWND_BROADCAST = $FFFF;
  WM_SETTINGCHANGE = $1A;
  SMTO_ABORTIFHUNG = $0002;
  ENV_CHANGE_TIMEOUT = 5000;

function SendMessageTimeoutA(hWnd: Integer; Msg: Integer;
  wParam: Longint; lParam: Longint; fuFlags: Longint;
  uTimeout: Longint; var lpdwResult: Longint): Longint;
  external 'SendMessageTimeoutA@user32.dll stdcall';

procedure BroadcastEnvChange;
var
  resultValue: Longint;
begin
  SendMessageTimeoutA(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0,
    SMTO_ABORTIFHUNG, ENV_CHANGE_TIMEOUT, resultValue);
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

procedure RemovePathEntry(const Entry: string);
var
  Key, PathValue, NormalizedEntry, Current, NewPath: string;
  P: Integer;
begin
  Key := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  if not RegQueryStringValue(HKLM, Key, 'Path', PathValue) then Exit;
  NormalizedEntry := UpperCase(ExcludeTrailingPathDelimiter(Entry));
  NewPath := '';
  while Length(PathValue) > 0 do
  begin
    P := Pos(';', PathValue);
    if P > 0 then
    begin
      Current := Copy(PathValue, 1, P - 1);
      PathValue := Copy(PathValue, P + 1, Length(PathValue));
    end
    else
    begin
      Current := PathValue;
      PathValue := '';
    end;
    if (Current <> '') and
       (UpperCase(ExcludeTrailingPathDelimiter(Current)) <> NormalizedEntry) then
    begin
      if NewPath = '' then
        NewPath := Current
      else
        NewPath := NewPath + ';' + Current;
    end;
  end;
  RegWriteExpandStringValue(HKLM, Key, 'Path', NewPath);
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
