; -- Basic Inno Setup Script for jbeam-edit --

[Setup]
AppName=jbeam-edit
AppVersion=1.0
DefaultDirName={pf}\jbeam-edit
DefaultGroupName=jbeam-edit
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes

[Files]
Source: "..\dist\release\jbeam-edit.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\dist\release\examples\*"; DestDir: "{app}\examples"; Flags: recursesubdirs createallsubdirs
Source: "..\dist\release\README.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\dist\release\JBFL_DOCS.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\dist\release\LICENSE"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\jbeam-edit"; Filename: "{app}\jbeam-edit.exe"

[Run]
Filename: "{app}\jbeam-edit.exe"; Description: "Launch jbeam-edit"; Flags: nowait postinstall skipifsilent
