; -- Basic Inno Setup Script for jbeam-edit --

[Setup]
AppName=jbeam-edit
AppVersion=0.0.4.0
DefaultDirName={commonpf}\jbeam-edit
OutputBaseFilename=jbeam-edit-setup
Compression=lzma
SolidCompression=yes
PrivilegesRequired=admin
UninstallDisplayName=jbeam-edit

[Files]
Source: "..\dist\release\jbeam-edit.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\dist\release\jbeam-lsp-server.exe"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "..\dist\release\examples\jbfl\*"; DestDir: "{app}\examples\jbfl"; Flags: recursesubdirs createallsubdirs ignoreversion

[UninstallDelete]
Name: "{app}\*"; Type: files
Name: "{app}"; Type: dirifempty


[Code]
#include "source_path.inc"
