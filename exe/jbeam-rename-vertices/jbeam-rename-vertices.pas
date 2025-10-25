program JbeamRenameVertices;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Classes, IniFiles;

function GetAppDataPath: string;
var
  buf: array[0..1023] of Char;
begin
  if Windows.GetEnvironmentVariable('APPDATA', buf, Length(buf)) <> 0 then
    Result := StrPas(buf) + '\jbeam-edit\'
  else
    Result := '';
end;

procedure RunJbeamEdit(const args: string);
var
  si: TStartupInfo;
  pi: TProcessInformation;
  cmd: string;
begin
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;

  ZeroMemory(@pi, SizeOf(pi));

  cmd := 'jbeam-edit ' + args;

  if not CreateProcess(nil, PChar(cmd), nil, nil, False, 0, nil, nil, si, pi) then
    Writeln('Failed to run jbeam-edit.')
  else
  begin
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  end;
end;

var
  filePath, src, dst, renameArgs, answer, argStr: string;
  replacements: TStringList;
  ini: TIniFile;
  alwaysBackup: Boolean;
  iniPath: string;

begin
  SetConsoleOutputCP(CP_UTF8);

  iniPath := GetAppDataPath;
  if iniPath = '' then
  begin
    Writeln('Could not find APPDATA path.');
    Exit;
  end;
  ForceDirectories(iniPath);

  ini := TIniFile.Create(iniPath + 'settings.ini');
  alwaysBackup := ini.ReadBool('Settings', 'AlwaysBackup', True);
  ini.Free;

  Write('Enter full path to .jbeam file: ');
  ReadLn(filePath);
  if filePath = '' then Exit;

  replacements := TStringList.Create;
  try
    renameArgs := '';
    repeat
      Write('Source (leave empty to finish): ');
      ReadLn(src);
      if src = '' then Break;
      Write('Replacement: ');
      ReadLn(dst);
      if renameArgs <> '' then renameArgs := renameArgs + ',';
      renameArgs := renameArgs + src + ':' + dst;
    until False;

    if renameArgs = '' then
    begin
      Writeln('No replacements entered.');
      Exit;
    end;

    if alwaysBackup then
      argStr := '-n "' + renameArgs + '" "' + filePath + '"'
    else
    begin
      Write('Backup recommended. y=Backup, n=No Backup, c=Always No Backup: ');
      ReadLn(answer);
      case LowerCase(answer) of
        'y': argStr := '-n "' + renameArgs + '" "' + filePath + '"';
        'n': argStr := '-i -n "' + renameArgs + '" "' + filePath + '"';
        'c':
          begin
            ini := TIniFile.Create(iniPath + 'settings.ini');
            ini.WriteBool('Settings', 'AlwaysBackup', False);
            ini.Free;
            argStr := '-i -n "' + renameArgs + '" "' + filePath + '"';
          end;
      else
        Writeln('Invalid option, using backup by default.');
        argStr := '-n "' + renameArgs + '" "' + filePath + '"';
      end;
    end;

    RunJbeamEdit(argStr);
    Writeln('Done.');
  finally
    replacements.Free;
  end;
end.
