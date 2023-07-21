{
  Syn
  Copyright Γ‚Β© 2003-2004, Danail Traichev. All rights reserved.
  neum@developer.bg,

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is uParams.pas, released Thu, 27 Mar 2003 10:22:27 UTC.

  The Initial Developer of the Original Code is Danail Traichev.
  Portions created by Danail Traichev are Copyright Γ‚Β© 2003-2004 Danail Traichev.
  All Rights Reserved.

  Contributor(s): .

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  You may retrieve the latest version of this file at the  home page,
  located at http://syn.sourceforge.net

 $Id: uParams.pas,v 1.12 2004/06/07 23:03:48 neum Exp $
 }
unit uParams;
interface

Uses
  System.UITypes,
  System.Classes;


(* parameters, valid for current Windows configuration *)
procedure RegisterStandardParametersAndModifiers;
procedure UnRegisterStandardParametersAndModifiers;

procedure RegisterCustomParams;
procedure UnRegisterCustomParams;

Var
  CustomParams : TStrings;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.Win.Registry,
  System.RegularExpressions,
  System.IOUtils,
  Vcl.Clipbrd,
  Vcl.Dialogs,
  Vcl.FileCtrl,
  JclSysInfo,
  MPShellUtilities,
  PythonVersions,
  JvGnugettext,
  StringResources,
  dmResources,
  uEditAppIntfs,
  uCommonFunctions,
  cPyScripterSettings,
  cParameters,
  cPyBaseDebugger,
  cProjectClasses,
  cPyControl,
  dlgCommandLine;

function GetActiveDoc: string;
Var
  Editor : IEditor;
begin
  Result:= '';
  Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then
    Result:= Editor.FileId;
end;

function GetActiveScript: string;
begin
  Result:= PyControl.RunConfig.ScriptName;
end;

function GetProjectFile: string;
begin
  Result:= ActiveProject.FileName;
end;

function GetModFiles: string;
var
  i: integer;
begin
  Result:= '';
  if Assigned(GI_EditorFactory) then begin
    with GI_EditorFactory do
      for i := 0 to GetEditorCount - 1 do
        with Editor[i] do
          if Modified and (FileName <> '') then
            Result := Result + ' ' + ExtractShortPathName(FileName);
    Delete(Result, 1, 1);
  end;
end;

function GetOpenFiles: string;
var
  i: integer;
begin
  Result:= '';
  if Assigned(GI_EditorFactory) then begin
    with GI_EditorFactory do
      for i := 0 to GetEditorCount - 1 do
        with Editor[i] do
          if GetFileName <> '' then
            Result := Result + ' ' + ExtractShortPathName(FileName);
    Delete(Result, 1, 1);
  end;
end;

function GetCurWord(const AFileName: string): string;
var
  AEditor: IEditor;
begin
  Result:= '';
  if (AFileName = '') or SameText('ActiveDoc', AFileName) then
    AEditor:= GI_ActiveEditor
  else AEditor := GI_EditorFactory.GetEditorByName(AFileName);
  if Assigned(AEditor) then
    Result:= AEditor.GetSynEdit.WordAtCursor;
end;

function GetCurLine(const AFileName: string): string;
var
  AEditor: IEditor;
begin
  Result:= '';
  if (AFileName = '') or SameText('ActiveDoc', AFileName) then
    AEditor:= GI_ActiveEditor
  else AEditor:= GI_EditorFactory.GetEditorByFileId(AFileName);
  if Assigned(AEditor) then
    Result:= AEditor.GetSynEdit.LineText;
end;

function GetCurLineNumber(const AFileName: string): string;
var
  AEditor: IEditor;
begin
  Result:= '0';
  if (AFileName = '') or SameText('ActiveDoc', AFileName) then
    AEditor:= GI_ActiveEditor
  else AEditor:= GI_EditorFactory.GetEditorByFileId(AFileName);
  if Assigned(AEditor) then
    Result:= AEditor.GetSynEdit.CaretY.ToString;
end;

function GetSelText(const AFileName: string): string;
var
  AEditor: IEditor;
begin
  Result:= '';
  if (AFileName = '') or SameText('ActiveDoc', AFileName) then
    AEditor:= GI_ActiveEditor
  else AEditor:= GI_EditorFactory.GetEditorByName(AFileName);
  if Assigned(AEditor) then
    Result:= AEditor.GetSynEdit.SelText;
end;

function SelectFile(const ATitle: string): string;
var
  SaveTitle: string;
begin
  with ResourcesDataModule.dlgFileOpen do begin
    Filter := _(SFilterAllFiles);
    FileName := '';
    SaveTitle:= Title;
    if ATitle <> '' then
      Title:= ATitle
    else
      Title := 'Select File';
    if Execute then begin
      Result:= FileName;
      Parameters.ChangeParameter('SelectedFile', FileName);
    end;
    Title:= SaveTitle;
  end;
end;

function SelectDir(const ATitle: string): string;
var
  Directories : TArray<string>;
begin
  if SelectDirectory('', Directories, [], ATitle) then
  begin
    Result := Directories[0];
    Parameters.ChangeParameter('SelectedDir', Result);
  end;
end;

function StrDefQuote(const AText: string): string;
begin
  Result:= AText.QuotedString('"');
end;

function GetDateTime: string;
begin
  Result:= DateTimeToStr(Now);
end;

function GetDate(const AText: string): string;
Var
  V : Variant;
begin
  try
    V := AText;
    VarCast(V, V, varDate);
//    Result:= DateToStr(StrToDateTime(AText));
    Result:= DateToStr(V);
  except
    Result := '';
  end;
end;

function GetTime(const AText: string): string;
Var
  V : Variant;
begin
  try
    V := AText;
    VarCast(V, V, varDate);
//    Result:= TimeToStr(StrToDateTime(AText));
    Result:= TimeToStr(V);
  except
    Result := '';
  end;
end;

function GetFileDate(const AFileName: string): string;
  Var
    DateTime : TDateTime;
begin
  Result:= '';
  if FileExists(AFileName) then begin
    FileAge(AFileName, DateTime);
    Result:= DateTimeToStr(DateTime);
  end;
end;

function GetFileDateCreate(const AFileName: string): string;
Var
  NameSpace : TNameSpace;
begin
  Result:= '';
  if FileExists(AFileName) then begin
    NameSpace := TNameSpace.CreateFromFileName(AFileName);
    try
      Result := NameSpace.CreationTime;
    finally
      NameSpace.Free;
    end;
  end;
end;

function GetFileDateWrite(const AFileName: string): string;
Var
  NameSpace : TNameSpace;
begin
  Result:= '';
  if FileExists(AFileName) then begin
    NameSpace := TNameSpace.CreateFromFileName(AFileName);
    try
      Result := NameSpace.LastWriteTime;
    finally
      NameSpace.Free;
    end;
  end;
end;

function GetFileDateAccess(const AFileName: string): string;
Var
  NameSpace : TNameSpace;
begin
  Result:= '';
  if FileExists(AFileName) then begin
    NameSpace := TNameSpace.CreateFromFileName(AFileName);
    try
      Result := NameSpace.LastAccessTime;
    finally
      NameSpace.Free;
    end;
  end;
end;

function GetFileType(const AFileName: string): string;
Var
  NameSpace : TNameSpace;
begin
  Result:= '';
  if FileExists(AFileName) then begin
    NameSpace := TNameSpace.CreateFromFileName(AFileName);
    try
      Result := NameSpace.FileType;
    finally
      NameSpace.Free;
    end;
  end;
end;

function GetDateFormated(const AText: string): string;
// Delphi's string to date conversion fails when the date contains month names
// so use variant conversion instead
var
  V : Variant;
begin
  with TRegEx.Match(AText, '([^'']+)-''([^'']+)''') do
    if Success then
      try
        V := GroupValue(1);
        VarCast(V, V, varDate);
        Exit(FormatDateTime(GroupValue(2),  V));
      except
        Exit('');
      end
    else begin
      StyledMessageDlg(Format(_(SInvalidParameterFormat),
        [Concat(AText, '-', 'DateFormat')]), mtError, [mbOK], 0);
      Abort;
    end;
end;

function GetExe: string;
begin
  Result:= ParamStr(0);
end;

function GetParam(const AIndex: string): string;
(* Returns the commandline argument *)
var
  ix: integer;
begin
  Result := '';
  if TryStrToInt(AIndex, ix) then begin
    if ix <= ParamCount then
      Result := ParamStr(ix);
  end;
end;

function GetClipboard: string;
(* returns clipboard as text *)
begin
  Result:= Clipboard.AsText;
end;

function GetFileExt(const AFileName: string): string;
(* returns extension without . *)
begin
  Result:= ExtractFileExt(AFileName);
  if Result <> '' then
    Delete(Result, 1, 1);
end;

function GetReg(const ARegKey: string): string;
(* returns registry key value *)
var
  Info: TRegDataInfo;
  AName: string;
  i: Integer;
  Buff: Pointer;
  S : string;
begin
  with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do try
    Result:= '';
    if ARegKey = '' then Exit;
    i:= Pos('\', ARegKey);
    (* read root key *)
    if i > 1 then begin
      AName:= Copy(ARegKey, 1, i-1);
      if (AName = 'HKCU') or (AName = 'HKEY_CURRENT_USER') then
        RootKey:= HKEY_CURRENT_USER
      else if (AName = 'HKLM') or (AName = 'HKEY_LOCAL_MACHINE') then
        RootKey:= HKEY_LOCAL_MACHINE
      else if (AName = 'HKCR') or (AName = 'HKEY_CLASSES_ROOT') then
        RootKey:= HKEY_CLASSES_ROOT
      else if (AName = 'HKU') or (AName = 'HKEY_USERS') then
        RootKey:= HKEY_USERS
      else if (AName = 'HKPD') or (AName = 'HKEY_PERFORMANCE_DATA') then
        RootKey:= HKEY_PERFORMANCE_DATA
      else if (AName = 'HKCC') or (AName = 'HKEY_CURRENT_CONFIG') then
        RootKey:= HKEY_CURRENT_CONFIG
      else if (AName = 'HKDD') or (AName = 'HKEY_DYN_DATA') then
        RootKey:= HKEY_DYN_DATA;
      AName:= Copy(ARegKey, i, MaxInt);
    end
    else AName:= ARegKey;
    (* if key exists, read key data *)
    if OpenKeyReadOnly(ExtractFilePath(AName)) then begin
      AName:= TPath.GetFileName(ARegKey);
      if not GetDataInfo(AName, Info) then
        Info.RegData:= rdUnknown;
      (* convert value to string *)
      case Info.RegData of
      rdString,
      rdExpandString:
        Result:= ReadString(AName);
      rdInteger:
        Result:= IntToStr(ReadInteger(AName));
      rdUnknown,
      rdBinary:
        begin
          GetMem(Buff, Info.DataSize);
          try
            ReadBinaryData(AName, Buff^, Info.DataSize);
            SetLength(S, 2 * Info.DataSize);
            BinToHex(Buff, PChar(S), Info.DataSize);
            Result := S;
          finally
            FreeMem(Buff);
          end;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

function GetFileText(const AFileName: string): string;
(* returns file text (searches editor and project enviroment too) *)
var
  AEditor: IEditor;
begin
  Result:= '';
  // look in open files
  AEditor:= GI_EditorFactory.GetEditorByFileId(AFileName);
  if Assigned(AEditor) then
    Result:= AEditor.GetSynEdit.Text
  else begin
    if FileExists(AFileName) then
      Result:= FileToStr(AFileName);
  end;
end;

function GetShortFileName(const APath: string): string;
(* returns short file name even for nonexisting files *)
begin
  if APath = '' then Result:= ''
  else begin
    Result:= ExtractShortPathName(APath);
    // if different - function is working
    if (Result = '') or
       (Result = APath) and not FileExists(ExcludeTrailingPathDelimiter(APath)) then
    begin
      Result:= ExtractFilePath(APath);
      // we are up to top level
      if (Result = '') or (Result[Length(Result)] = ':') then
        Result:= APath
      else Result:= GetShortFileName(ExcludeTrailingPathDelimiter(Result)) +
                           PathDelim + TPath.GetFileName(APath);
    end;
  end;
end;

function GetActivePythonDir : string;
begin
  Result := '';
  if GI_PyControl.PythonLoaded then
    Result := IncludeTrailingPathDelimiter(PyControl.PythonVersion.InstallPath);
end;

function GetActivePythonExe : string;
begin
  if GI_PyControl.PythonLoaded then
    Result := PyControl.PythonVersion.PythonExecutable
  else
    Result := 'python.exe';
end;

function GetActivePythonwExe : string;
begin
  Result :=  GetActivePythonExe;
  Result := Copy(Result, 1, Length(Result)- 4) + 'w.exe';
end;

function GetPythonDir (VersionString : string) : string;
var
  PythonVersion : TPythonVersion;
begin
  for PythonVersion in PyControl.RegPythonVersions do
    if PythonVersion.SysVersion = VersionString then
    begin
      Result := PythonVersion.InstallPath;
      break;
    end;

  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function GetPythonVersion: string;
begin
  Result := '';
  if GI_PyControl.PythonLoaded then
    Result := PyControl.PythonVersion.SysVersion;
end;

function GetCmdLineArgs: string;
begin
  Result := Parameters.ReplaceInText(CommandLineParams);
end;

function GetEnvironmentVariable(const Name: string): string;
const
  BufSize = 1024;
var
  Len: Integer;
  Buffer: array[0..BufSize - 1] of Char;
begin
  Result := '';
  Len := WinApi.Windows.GetEnvironmentVariable(PChar(Name), @Buffer, BufSize);
  if Len < BufSize then
    SetString(Result, PChar(@Buffer), Len)
  else
  begin
    SetLength(Result, Len - 1);
    WinApi.Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Len);
  end;
end;

function ExpandUNCFileName(const FileName: string): string;
begin
  { In the absense of a better solution use the ANSI function }
  Result := ExpandUNCFileName(FileName);
end;

function StripExtension(const AFileName: string): string;
begin
  Result := ChangeFileExt(AFileName, '');
end;

function GetUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(Result, Count);
  if WinApi.Windows.GetUserName(PChar(Result), Count) then
    SetLength(Result, StrLen(PChar(Result)))
  else
    Result := '';
end;

function GetPhysicalDesktopFolder : string;
begin
  Result := PhysicalDesktopFolder.NameForParsing;
end;

function GetProgramFilesFolder : string;
begin
  Result := GetReg('HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\ProgramFilesDir');
end;

function GetPersonalFolder : string;
begin
  Result := MyDocumentsFolder.NameForParsing;
end;

function GetCommonFilesFolder : string;
begin
  Result := GetReg('HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\CommonFilesDir');
end;

function WindowsDirectory : string;
begin
  Result := GetWindowsFolder;
end;

function SystemDirectory : string;
begin
  Result := GetWindowsSystemFolder;
end;

function GetTempDir : string;
begin
  Result := GetWindowsTempFolder;
end;

procedure RegisterStandardParametersAndModifiers;
begin
  with Parameters do begin
    Sorted := False;

    (* parameters, valid for current Windows configuration *)
    // Python Paths etc.
    RegisterParameter('Python37Dir', GetPythonDir('3.7'), nil);
    RegisterParameter('Python38Dir', GetPythonDir('3.8'), nil);
    RegisterParameter('Python39Dir', GetPythonDir('3.9'), nil);
    RegisterParameter('Python310Dir', GetPythonDir('3.10'), nil);
    RegisterParameter('Python311Dir', GetPythonDir('3.11'), nil);
    RegisterParameter('Python312Dir', GetPythonDir('3.12'), nil);
    RegisterParameter('Python37Exe', '$[PYTHON37DIR]python.exe', nil);
    RegisterParameter('Python38Exe', '$[PYTHON38DIR]python.exe', nil);
    RegisterParameter('Python39Exe', '$[PYTHON39DIR]python.exe', nil);
    RegisterParameter('Python310Exe', '$[PYTHON310DIR]python.exe', nil);
    RegisterParameter('Python311Exe', '$[PYTHON311DIR]python.exe', nil);
    RegisterParameter('Python312Exe', '$[PYTHON312DIR]python.exe', nil);
    RegisterParameter('PythonDir', _('Directory of active Python version'), GetActivePythonDir);
    RegisterParameter('PythonExe', _('Executable of active Python'), GetActivePythonExe);
    RegisterParameter('PythonwExe', _('Executable of active Python'), GetActivePythonwExe);
    RegisterParameter('PythonVersion', _('Version of active Python'), GetPythonVersion);

    // register system paths and parameters
    RegisterParameter('ProgramFiles', _('Program Files directory'), GetProgramFilesFolder);
    RegisterParameter('CommonFiles', _('Common Files directory'), GetCommonFilesFolder);
    RegisterParameter('Windows', _('Windows installation directory'), WindowsDirectory);
    RegisterParameter('WindowsSystem', _('Windows System directory'), SystemDirectory);
    RegisterParameter('WindowsTemp', _('Windows Temp directory'), GetTempDir);
    RegisterParameter('MyDocuments', _('MyDocuments directory'), GetPersonalFolder);
    RegisterParameter('Desktop', _('Desktop directory'), GetPhysicalDesktopFolder);

    // register parameters
    RegisterParameter('Paste', _('Clipboard as text'), GetClipboard);
    RegisterParameter('UserName', _('User name'), GetUserName);
    RegisterParameter('CurrentDir', _('Current directory'), GetCurrentDir);
    RegisterParameter('Exe', _('Executable name'), GetExe);
    RegisterParameter('CmdLineArgs', _('Python Command Line Arguments'), GetCmdLineArgs);

    // register parameter modifiers
    RegisterModifier('Path', _('Path of file'), ExtractFilePath);
    RegisterModifier('Dir', _('Path without delimeter'), ExtractFileDir);
    RegisterModifier('Name', _('File name'), TPath.GetFileName);
    RegisterModifier('Ext', _('File extension'), ExtractFileExt);
    RegisterModifier('ExtOnly', _('File extension without "."'), GetFileExt);
    RegisterModifier('NoExt', _('File name without extension'), StripExtension);
    RegisterModifier('Drive', _('File drive'), ExtractFileDrive);
    RegisterModifier('Full', _('Expanded file name'), ExpandFileName);
    RegisterModifier('UNC', _('Expanded UNC file name'), ExpandUNCFileName);
    RegisterModifier('Long', _('Long file name'), GetLongFileName);
    RegisterModifier('Short', _('Short file name'), GetShortFileName);
    RegisterModifier('Sep', _('Path with final path delimiter added'), IncludeTrailingPathDelimiter);
    RegisterModifier('NoSep', _('Path with final path delimiter removed'), ExcludeTrailingPathDelimiter);
    RegisterModifier('Type', _('File type'), GetFileType);
    RegisterModifier('Text', _('Contents of text file'), FileToStr);
    RegisterModifier('Param', _('Command line parameter'), GetParam);
    RegisterModifier('Reg', _('Value of registry key'), GetReg);
    RegisterModifier('Env', _('Value of environment variable'), GetEnvironmentVariable);
    RegisterModifier('UpperCase', _('Upper case of string'), System.SysUtils.AnsiUpperCase);
    RegisterModifier('LowerCase', _('Lower case of string'), System.SysUtils.AnsiLowerCase);
    RegisterModifier('Quote', _('Quoted string'), StrDefQuote);
    RegisterModifier('UnQuote', _('Unquoted string'), StrUnquote);

   (* parameters, specific for PyScripter *)
    RegisterParameter('SelectFile', '$[-SelectFile]', nil);
    RegisterParameter('SelectedFile', '', nil);
    RegisterParameter('SelectDir', '$[-SelectDir]', nil);
    RegisterParameter('SelectedDir', '', nil);
    RegisterParameter('DateTime', _('Current date and time'), GetDateTime);
    RegisterModifier('SelectFile', _('Select file'), SelectFile);
    RegisterModifier('SelectDir', _('Select directory'), SelectDir);
    RegisterModifier('Date', _('Date of a date-time value'), GetDate);
    RegisterModifier('Time', _('Time of a date-time value'), GetTime);
    RegisterModifier('FileDate', _('Date of a file'), GetFileDate);
    RegisterModifier('DateCreate', _('Creation date of a file'), GetFileDateCreate);
    RegisterModifier('DateWrite',  _('Last modification date of a file'), GetFileDateWrite);
    RegisterModifier('DateAccess', _('Last access date of a file'), GetFileDateAccess);
    RegisterModifier('DateFormat', _('Formatted date'), GetDateFormated);

    (* parameters, that change often in one PyScripter session *)
    (* editor related *)
    RegisterParameter('ActiveDoc', _('Active document name'), GetActiveDoc);
    RegisterParameter('ActiveScript', _('The name or last run script'), GetActiveScript);
    RegisterParameter('Project', _('Project file name'), GetProjectFile);
    RegisterParameter('ModFiles', _('Modified files'), GetModFiles);
    RegisterParameter('OpenFiles', _('Open files'), GetOpenFiles);
    RegisterParameter('CurWord', '$[-CurWord]', nil);
    RegisterParameter('CurLine', '$[-CurLine]', nil);
    RegisterParameter('CurLineNumber', '$[-CurLineNumber]', nil);
    RegisterParameter('SelText', '$[-SelText]', nil);
    RegisterModifier('EdText', _('Text of the active document or a file'), GetFileText);
    RegisterModifier('CurWord', _('Current word in the active document'), GetCurWord);
    RegisterModifier('CurLine', _('Current line in the active document'), GetCurLine);
    RegisterModifier('CurLineNumber', _('Current line number in the active document'), GetCurLineNumber);
    RegisterModifier('SelText', _('Selected text in the active document'), GetSelText);

    Sorted := True;
  end;
end;

procedure UnRegisterStandardParametersAndModifiers;
begin
  // unregister parameter modifiers
  with Parameters do begin
    (* parameters, valid for current Windows configuration *)
    // Python Paths etc.
    UnRegisterParameter('Python37Dir');
    UnRegisterParameter('Python38Dir');
    UnRegisterParameter('Python39Dir');
    UnRegisterParameter('Python310Dir');
    UnRegisterParameter('Python311Dir');
    UnRegisterParameter('Python312Dir');
    UnRegisterParameter('Python37Exe');
    UnRegisterParameter('Python38Exe');
    UnRegisterParameter('Python39Exe');
    UnRegisterParameter('Python310Exe');
    UnRegisterParameter('Python311Exe');
    UnRegisterParameter('Python312Exe');
    UnRegisterParameter('PythonDir');
    UnRegisterParameter('PythonExe');
    UnRegisterParameter('PythonwExe');
    UnRegisterParameter('PythonVersion');

    // unregister system paths and parameters
    UnRegisterParameter('ProgramFiles');
    UnRegisterParameter('CommonFiles');
    UnRegisterParameter('Windows');
    UnRegisterParameter('WindowsSystem');
    UnRegisterParameter('WindowsTemp');
    UnRegisterParameter('MyDocuments');
    UnRegisterParameter('Desktop');

    // unregister parameters
    UnRegisterParameter('Paste');
    UnRegisterParameter('UserName');
    UnRegisterParameter('CurrentDir');
    UnRegisterParameter('Exe');
    UnRegisterParameter('CmdLineArgs');

    // unregister modifiers
    UnRegisterModifier('Path');
    UnRegisterModifier('Dir');
    UnRegisterModifier('Name');
    UnRegisterModifier('Ext');
    UnRegisterModifier('ExtOnly');
    UnRegisterModifier('NoExt');
    UnRegisterModifier('Drive');
    UnRegisterModifier('Full');
    UnRegisterModifier('UNC');
    UnRegisterModifier('Long');
    UnRegisterModifier('Short');
    UnRegisterModifier('Sep');
    UnRegisterModifier('NoSep');
    UnRegisterModifier('Type');
    UnRegisterModifier('Text');
    UnRegisterModifier('Param');
    UnRegisterModifier('Reg');
    UnRegisterModifier('Env');
    UnRegisterModifier('UpperCase');
    UnRegisterModifier('LowerCase');
    UnRegisterModifier('Quote');
    UnRegisterModifier('UnQuote');

   (* parameters, specific for PyScripter *)
    UnRegisterParameter('SelectFile');
    UnRegisterParameter('SelectedFile');
    UnRegisterParameter('SelectDir');
    UnRegisterParameter('SelectedDir');
    UnRegisterParameter('DateTime');
    UnRegisterModifier('SelectFile');
    UnRegisterModifier('SelectDir');
    UnRegisterModifier('Date');
    UnRegisterModifier('Time');
    UnRegisterModifier('FileDate');
    UnRegisterModifier('DateCreate');
    UnRegisterModifier('DateWrite');
    UnRegisterModifier('DateAccess');
    UnRegisterModifier('DateFormat');

    (* parameters, that change often in one syn session *)
    (* editor related *)
    UnRegisterParameter('ActiveDoc');
    UnRegisterParameter('ActiveScript');
    UnRegisterParameter('Project');
    UnRegisterParameter('ModFiles');
    UnRegisterParameter('OpenFiles');
    UnRegisterParameter('CurWord');
    UnRegisterParameter('CurLine');
    UnRegisterParameter('CurLineNumber');
    UnRegisterParameter('SelText');
    UnRegisterModifier('EdText');
    UnRegisterModifier('CurWord');
    UnRegisterModifier('CurLine');
    UnRegisterModifier('CurLineNumber');
    UnRegisterModifier('SelText');
  end;
end;

procedure RegisterCustomParams;
Var
  i : integer;
  ParamName : string;
begin
  with Parameters do begin
    Clear;
    Modifiers.Clear;
    RegisterStandardParametersAndModifiers;
    for i := 0 to CustomParams.Count - 1 do begin
      ParamName := CustomParams.Names[i];
      if ParamName <> '' then
        RegisterParameter(ParamName, CustomParams.Values[ParamName], nil);
    end;
  end;
end;

procedure UnRegisterCustomParams;
begin
  Parameters.Clear;
  Parameters.Modifiers.Clear;
  RegisterStandardParametersAndModifiers;
end;


initialization
  CustomParams := TStringList.Create;
  RegisterStandardParametersAndModifiers;

finalization
  FreeAndNil(CustomParams);
end.
