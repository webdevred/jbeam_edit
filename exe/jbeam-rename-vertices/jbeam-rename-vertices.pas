program JbeamRenameVertices;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Classes;

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
  filePath, src, dst, renameArgs: string;

begin
  SetConsoleOutputCP(CP_UTF8);

  Write('Enter full path to .jbeam file: ');
  ReadLn(filePath);

  if filePath = '' then
  begin
    Writeln('No file selected.');
    Exit;
  end;

  renameArgs := '';

  repeat
    Write('Source (empty to finish): ');
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

  RunJbeamEdit('-n "' + renameArgs + '" "' + filePath + '"');
  Writeln('Done.');
end.
