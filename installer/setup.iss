; -- Basic Inno Setup Script for jbeam-edit --

[Setup]
AppName=jbeam-edit
AppVersion=0.0.3.0
DefaultDirName={pf}\jbeam-edit
DefaultGroupName=jbeam-edit
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes

[Files]
Source: "..\dist\release\jbeam-edit.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\dist\release\examples\jbfl\*"; DestDir: "{app}\examples\jbfl"; Flags: recursesubdirs createallsubdirs ignoreversion

[Registry]
  Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "Path"; \
    ValueData: "{olddata};{app}"; Flags: preservestringtype uninsdeletevalue
