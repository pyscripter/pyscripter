{-----------------------------------------------------------------------------
 Unit Name: cAppPaths
 Author:    PyScripter
 Date:      30-Dec-2026
 Purpose:   Holds the App paths
 History:
-----------------------------------------------------------------------------}

unit cAppPaths;

interface

function GetEXEPath: String;
function GetAppRootPath: String;
function GetAppName: String;


implementation

uses
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms;

var
  AppName: string;

  // paths including trailing path delimiter
  EXEPath: string;
  AppRootPath: string;

function GetAppName: String;
begin
  Result := AppName;
end;

function GetEXEPath: String;
begin
  Result := EXEPath;
end;

function GetAppRootPath: String;
begin
  Result := AppRootPath;
end;

procedure InitPaths;
var
  DirName: String;
begin
  AppName := TPath.GetFileNameWithoutExtension(Application.ExeName);
  EXEPath := ExtractFilePath(Application.ExeName);
  DirName := LowerCase(ExtractFileName(ExcludeTrailingPathDelimiter(EXEPath)));
  {$IFDEF WIN64}
  if (DirName = 'x64') or (DirName = 'bin64') then
  {$ELSE}
  if (DirName = 'x86') or (DirName = 'bin') then
  {$ENDIF}
    AppRootPath := ExtractFilePath(ExcludeTrailingPathDelimiter(EXEPath))
  else
    AppRootPath := EXEPath;
end;

initialization
  InitPaths;

end.
