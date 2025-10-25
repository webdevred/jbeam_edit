program JbeamRenameVertices;

uses
  Windows, SysUtils, ShellAPI;

var
  hConsole: THandle;
  filePath, src, dst, answer, renameArgs: string;
  row: Integer;

procedure ClrScr;
var
  csbi: TConsoleScreenBufferInfo;
  charsWritten: DWORD;
begin
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(hConsole, csbi);

  FillConsoleOutputCharacter(hConsole, ' ', csbi.dwSize.X * csbi.dwSize.Y, COORD(0,0), charsWritten);
  FillConsoleOutputAttribute(hConsole, csbi.wAttributes, csbi.dwSize.X * csbi.dwSize.Y, COORD(0,0), charsWritten);
  SetConsoleCursorPosition(hConsole, COORD(0,0));
end;

procedure SetTextColor(color: Word);
begin
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  SetConsoleTextAttribute(hConsole, color);
end;

procedure GotoXY(x, y: Integer);
var
  coord: TCoord;
begin
  coord.X := x-1;
  coord.Y := y-1;
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), coord);
end;

begin
  SetConsoleOutputCP(CP_UTF8);
  renameArgs := '';
  row := 2;

  ClrScr;
  SetTextColor(14);
  GotoXY(10,1); WriteLn('==== JBeam Rename Vertices ====');
  SetTextColor(7); 

  GotoXY(5,row); Write('Enter file: '); ReadLn(filePath);
  Inc(row,2);

  repeat
    GotoXY(5,row); Write('Source (what to replace): '); ReadLn(src); Inc(row);
    GotoXY(5,row); Write('Replacement (to what): '); ReadLn(dst); Inc(row);

    if renameArgs <> '' then renameArgs := renameArgs + ',';
    renameArgs := renameArgs + src + ':' + dst;

    GotoXY(5,row); Write('Add more replacements? (y/n): '); ReadLn(answer); Inc(row,2);
  until LowerCase(answer) <> 'y';

  SetTextColor(11);
  GotoXY(5,row); WriteLn('Command: jbeam-edit -i -n "', renameArgs, '" "', filePath, '"');
  SetTextColor(7);
  Inc(row,2);

  if ShellExecute(0, 'open', PChar('jbeam-edit'),
                  PChar('-i -n "' + renameArgs + '" "' + filePath + '"'),
                  nil, SW_SHOWNORMAL) <= 32 then
  begin
    SetTextColor(12);
    GotoXY(5,row); WriteLn('Could not run jbeam-edit. Is it in PATH?');
    SetTextColor(7);
    Inc(row,2);
  end;

  GotoXY(5,row); WriteLn('Done!');
  ReadLn;
end.
