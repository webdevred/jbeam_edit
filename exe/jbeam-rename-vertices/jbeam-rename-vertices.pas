program JbeamRenameVertices;

uses
crt, SysUtils, Windows;

var
   filePath, src, dst, answer, renameArgs: string;
   row: integer;

procedure PrintHeader;
begin
   textcolor(yellow);
   clrscr;
   gotoxy(10,1);
   writeln('===============================');
   gotoxy(10,2);
   writeln('    JBeam Rename Vertices      ');
   gotoxy(10,3);
   writeln('===============================');
   textcolor(white);
end;

begin
   SetConsoleOutputCP(CP_UTF8);
   renameArgs := '';
   row := 5;

   PrintHeader;

   gotoxy(5,row);
   write('Enter file: ');
   readln(filePath);
   inc(row,2);

   repeat
      gotoxy(5,row);
      write('Source (what to replace): ');
      readln(src);
      inc(row);

      gotoxy(5,row);
      write('Replacement (to what): ');
      readln(dst);
      inc(row);

      if renameArgs <> '' then
         renameArgs := renameArgs + ',';
      renameArgs := renameArgs + src + ':' + dst;

      gotoxy(5,row);
      write('Add more replacements? (y/n): ');
      readln(answer);
      inc(row,2);
   until LowerCase(answer) <> 'y';

   textcolor(lightcyan);
   gotoxy(5,row);
   writeln('Command: jbeam-edit -i -n "', renameArgs, '" "', filePath, '"');
   textcolor(white);
   inc(row,2);

   if ShellExecute(0, 'open', PChar('jbeam-edit'),
                   PChar('-i -n "' + renameArgs + '" "' + filePath + '"'),
                   nil, SW_SHOWNORMAL) <= 32 then
   begin
      textcolor(red);
      gotoxy(5,row);
      writeln('Could not run jbeam-edit. Is it in PATH?');
      textcolor(white);
      inc(row,2);
   end;

   gotoxy(5,row);
   writeln('Done!');
   readln;
end.
