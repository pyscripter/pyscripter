{-----------------------------------------------------------------------------
 Unit Name: cPyBaseDebugger
 Author:    uDpiAware
 Date:      23-Apr-2007
 Purpose:
 History:   Call SetProcessDPIAware at initialization.
-----------------------------------------------------------------------------}
unit uDpiAware;

interface

implementation
Uses
  Windows, SysUtils, uCmdLine;
type
  TSetProcessDPIAware = function(): BOOL; stdcall;
var
  SetDPI: TSetProcessDPIAware;
  User32Dll: THandle;

initialization
if CmdLineReader.readFlag('DPIAWARE') and (Win32MajorVersion >= 6) then
begin
  User32Dll := GetModuleHandle('user32.dll');
  SetDPI := GetProcAddress(User32Dll, 'SetProcessDPIAware');
  if Assigned(SetDPI) then SetDPI;
end;

end.
