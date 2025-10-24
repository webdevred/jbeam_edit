program JbeamRenameVertices;

uses
SysUtils, Windows;

var
   filePath, src, dst, answer: string;
   renameArgs: string;

begin
   SetConsoleOutputCP(CP_UTF8);

   Write('Enter file: ');
   ReadLn(filePath);

   renameArgs := '';

   repeat
      Write('Source (what to replace): ');
      ReadLn(src);

      Write('Replacement (to what): ');
      ReadLn(dst);

      if renameArgs <> '' then
         renameArgs := renameArgs + ',';
      renameArgs := renameArgs + src + ':' + dst;

      Write('Do you want to add more? (y/n): ');
      ReadLn(answer);

   until LowerCase(answer) <> 'y';

   Writeln;
   Writeln('> jbeam-edit -i -n "', renameArgs, '" "', filePath, '"');

   if ShellExecute(0, 'open',
                   PChar('jbeam-edit'),
                   PChar('-i -n "' + renameArgs + '" "' + filePath + '"'),
                   nil, SW_SHOWNORMAL) <= 32 then
   begin
      Writeln('Could not run jbeam-edit. Is it in PATH?');
   end;

   Writeln;
   Writeln('Done!');
end.
