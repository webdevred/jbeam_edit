; -- Basic Inno Setup Script for jbeam-edit --

#include "constants.inc"

[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
DefaultDirName={commonpf}\{#AppName}
OutputBaseFilename={#AppName}-setup
Compression=lzma
SolidCompression=yes
PrivilegesRequired=admin
UninstallDisplayName={#AppName}

[UninstallDelete]
Name: "{app}\*"; Type: files
Name: "{app}"; Type: dirifempty


[Code]
#include "source_path.inc"
