{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ExceptDlg.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Sample Application exception dialog replacement with sending report by the default mail client   }
{ functionality                                                                                    }
{                                                                                                  }
{ Last modified: $Date: 2005/10/26 03:29:44 $                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit dlgExceptionMail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JclMapi, JclDebug;

const
  UM_CREATEDETAILS = WM_USER + $100;

  ReportToLogEnabled   = $00000001; // TExceptionDialog.Tag property
  DisableTextScrollbar = $00000002; // TExceptionDialog.Tag property

type
  TSimpleExceptionLog = class (TObject)
  private
    FLogFileHandle: THandle;
    FLogFileName: string;
    FLogWasEmpty: Boolean;
    function GetLogOpen: Boolean;
  protected
    function CreateDefaultFileName: string;
  public
    constructor Create(const ALogFileName: string = '');
    destructor Destroy; override;
    procedure CloseLog;
    procedure OpenLog;
    procedure Write(const Text: string; Indent: Integer = 0); overload;
    procedure Write(Strings: TStrings; Indent: Integer = 0); overload;
    procedure WriteStamp(SeparatorLen: Integer = 0);
    property LogFileName: string read FLogFileName;
    property LogOpen: Boolean read GetLogOpen;
  end;

  TExcDialogSystemInfo = (siStackList, siOsInfo, siModuleList, siActiveControls);
  TExcDialogSystemInfos = set of TExcDialogSystemInfo;

  TExceptionDialogMail = class(TForm)
    SendBtn: TButton;
    TextLabel: TMemo;
    OkBtn: TButton;
    DetailsBtn: TButton;
    Bevel1: TBevel;
    DetailsMemo: TMemo;
    procedure SendBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    private
    FDetailsVisible: Boolean;
    FIsMainThead: Boolean;
    FLastActiveControl: TWinControl;
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
    FSimpleLog: TSimpleExceptionLog;
    procedure CreateDetails;
    function GetReportAsText: string;
    procedure ReportToLog;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetailInfo; dynamic;
    procedure CreateReport(const SystemInfo: TExcDialogSystemInfos);
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: Char; virtual;
    procedure NextDetailBlock;
    procedure UpdateTextLabelScrollbars;
  public
    procedure CopyReportToClipboard;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: Exception; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;
    property SimpleLog: TSimpleExceptionLog read FSimpleLog;
  end;

  TExceptionDialogMailClass = class of TExceptionDialogMail;

var
  ExceptionDialogMailClass: TExceptionDialogMailClass = TExceptionDialogMail;

implementation

{$R *.dfm}

uses
  ClipBrd, Math,
  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclStrings, JclSysInfo, JclSysUtils,
  uCommonFunctions;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'System   : %s %s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s, %s, %d MHz %s%s';
  RsScreenRes = 'Display  : %dx%d pixels, %d bpp';
  RsActiveControl = 'Active Controls hierarchy:';
  RsThread = 'Thread: %s';
  RsMissingVersionInfo = '(no version info)';

  RsSendBugReportAddress = 'pyscripter@gmail.com';
  RsSendBugReportSubject = 'PyScripter Bug Report';

var
  ExceptionDialogMail: TExceptionDialogMail;

//==================================================================================================
// Helper routines
//==================================================================================================

function GetBPP: Integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
  ReleaseDC(0, DC);
end;

//--------------------------------------------------------------------------------------------------

function SortModulesListByAddressCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;

//==================================================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//==================================================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end else
//    SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  if JclValidateModuleAddress(ExceptAddr) and (ExceptObject.InstanceSize >= Exception.InstanceSize) then
    TExceptionDialogMail.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//--------------------------------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;
type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;
var
  TApplicationHandleExceptionAddr, SysUtilsShowExceptionAddr: Pointer;
  CALLInstruction: TCALLInstruction;
  CallAddress: Pointer;
  NW: DWORD;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(Cardinal(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(Integer(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) - Integer(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;    
  end;

begin
  TApplicationHandleExceptionAddr := PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr := PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
  if Result then
  begin
    CALLInstruction.Address := Integer(@HookShowException) - Integer(CallAddress) - SizeOf(CALLInstruction);
    Result := WriteProcessMemory(GetCurrentProcess, CallAddress, @CALLInstruction, SizeOf(CALLInstruction), NW);
    if Result then
      FlushInstructionCache(GetCurrentProcess, CallAddress, SizeOf(CALLInstruction));
  end;
end;

//==================================================================================================
// TSimpleExceptionLog
//==================================================================================================

procedure TSimpleExceptionLog.CloseLog;
begin
  if LogOpen then
  begin
    CloseHandle(FLogFileHandle);
    FLogFileHandle := INVALID_HANDLE_VALUE;
    FLogWasEmpty := False;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TSimpleExceptionLog.Create(const ALogFileName: string);
begin
  if ALogFileName = '' then
    FLogFileName := CreateDefaultFileName
  else
    FLogFileName := ALogFileName;
  FLogFileHandle := INVALID_HANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

function TSimpleExceptionLog.CreateDefaultFileName: string;
begin
  Result := PathExtractFileDirFixed(ParamStr(0)) + PathExtractFileNameNoExt(ParamStr(0)) + '_Err.log';
end;

//--------------------------------------------------------------------------------------------------

destructor TSimpleExceptionLog.Destroy;
begin
  CloseLog;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TSimpleExceptionLog.GetLogOpen: Boolean;
begin
  Result := FLogFileHandle <> INVALID_HANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.OpenLog;
begin
  if not LogOpen then
  begin
    FLogFileHandle := CreateFile(PChar(FLogFileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
      OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if LogOpen then
      FLogWasEmpty := SetFilePointer(FLogFileHandle, 0, nil, FILE_END) = 0;
  end
  else
    FLogWasEmpty := False;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.Write(const Text: string; Indent: Integer);
var
  S: string;
  SL: TStringList;
  I: Integer;
begin
  if LogOpen then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Text;
      for I := 0 to SL.Count - 1 do
      begin
        S := StringOfChar(' ', Indent) + StrEnsureSuffix(AnsiCrLf, TrimRight(SL[I]));
        FileWrite(Integer(FLogFileHandle), Pointer(S)^, Length(S));
      end;
    finally
      SL.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.Write(Strings: TStrings; Indent: Integer);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Write(Strings[I], Indent);
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.WriteStamp(SeparatorLen: Integer);
begin
  if SeparatorLen = 0 then
    SeparatorLen := 100;
  SeparatorLen := Max(SeparatorLen, 20);  
  OpenLog;
  if not FLogWasEmpty then
    Write(AnsiCrLf);
  Write(StrRepeat('=', SeparatorLen));
  Write(Format('= %-*s =', [SeparatorLen - 4, DateTimeToStr(Now)]));
  Write(StrRepeat('=', SeparatorLen));
end;

//==================================================================================================
// Exception dialog with Send
//==================================================================================================

var
  ExceptionShowing: Boolean;

{ TExceptionDialogMail }

procedure TExceptionDialogMail.AfterCreateDetails;
begin
  SendBtn.Enabled := True;
end;

procedure TExceptionDialogMail.BeforeCreateDetails;
begin
  SendBtn.Enabled := False;
end;

function TExceptionDialogMail.ReportMaxColumns: Integer;
begin
  Result := 78;
end;

procedure TExceptionDialogMail.SendBtnClick(Sender: TObject);
begin
  with TJclEmail.Create do
  try
    ParentWnd := Application.Handle;
    Recipients.Add(RsSendBugReportAddress);
    Subject := RsSendBugReportSubject;
    Body := ReportAsText;
    SaveTaskWindows;
    try
      Send(True);
    finally
      RestoreTaskWindows;
    end;    
  finally
    Free;
  end;
end;

procedure TExceptionDialogMail.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.CreateDetailInfo;
begin
  CreateReport([siStackList, siOsInfo, siModuleList, siActiveControls]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  try
    CreateDetailInfo;
    ReportToLog;
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.CreateReport(const SystemInfo: TExcDialogSystemInfos);
const
  MMXText: array[Boolean] of PChar = ('', 'MMX');
  FDIVText: array[Boolean] of PChar = (' [FDIV Bug]', '');
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  CpuInfo: TCpuInfo;
  C: TWinControl;
  NtHeaders: PImageNtHeaders;
  ModuleBase: Cardinal;
  ImageBaseStr: string;
  StackList: TJclStackInfoList;
begin
  SL := TStringList.Create;
  try
    // Version Infor
    DetailsMemo.Lines.Add(Format('%s version : %s', [Application.Title, ApplicationVersion]));
    NextDetailBlock;
    // Stack list
    if siStackList in SystemInfo then
    begin
      StackList := JclLastExceptStackList;
      if Assigned(StackList) then
      begin
        DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
        StackList.AddToStrings(DetailsMemo.Lines, False, True, True);
        NextDetailBlock;
      end;
    end;
    // System and OS information
    if siOsInfo in SystemInfo then
    begin
      DetailsMemo.Lines.Add(Format(RsOSVersion, [GetWindowsVersionString, NtProductTypeString,
        Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
      GetCpuInfo(CpuInfo);
      with CpuInfo do
        DetailsMemo.Lines.Add(Format(RsProcessor, [Manufacturer, CpuName,
          RoundFrequency(FrequencyInfo.NormFreq),
          MMXText[MMX], FDIVText[IsFDIVOK]]));
      DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
      NextDetailBlock;
    end;
    // Modules list
    if (siModuleList in SystemInfo) and LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      DetailsMemo.Lines.Add(RsModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := Cardinal(SL.Objects[I]);
        DetailsMemo.Lines.Add(Format('[%.8x] %s', [ModuleBase, ModuleName]));
        NtHeaders := PeMapImgNtHeaders(Pointer(ModuleBase));
        if (NtHeaders <> nil) and (NtHeaders^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders^.OptionalHeader.ImageBase])
        else
          ImageBaseStr := StrRepeat(' ', 11);
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            DetailsMemo.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end
        else
          DetailsMemo.Lines.Add(ImageBaseStr + RsMissingVersionInfo);
      end;
      NextDetailBlock;
    end;
    // Active controls
    if (siActiveControls in SystemInfo) and (FLastActiveControl <> nil) then
    begin
      DetailsMemo.Lines.Add(RsActiveControl);
      C := FLastActiveControl;
      while C <> nil do
      begin
        DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
        C := C.Parent;
      end;
      NextDetailBlock;
    end;
  finally
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if ExceptionShowing then
    Application.ShowException(E)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E, nil);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ExceptionThreadHandler(Thread: TJclDebugThread);
begin
  if ExceptionShowing then
    Application.ShowException(Thread.SyncException)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(Thread.SyncException, Thread);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormCreate(Sender: TObject);
begin
  FSimpleLog := TSimpleExceptionLog.Create;
  FFullHeight := ClientHeight;
  DetailsVisible := False;
  Caption := Format(RsAppError, [Application.Title]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSimpleLog);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, LoadIcon(0, IDI_ERROR));
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormShow(Sender: TObject);
begin
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateDetails;
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialogMail.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + AnsiCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialogMail.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.ReportToLog;
begin
  if Tag and ReportToLogEnabled <> 0 then
  begin
    FSimpleLog.WriteStamp(ReportMaxColumns);
    try
      FSimpleLog.Write(ReportAsText);
    finally
      FSimpleLog.CloseLog;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    Constraints.MinHeight := FNonDetailsHeight + 100;
    Constraints.MaxHeight := Screen.Height;
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
    DetailsMemo.Height := FFullHeight - DetailsMemo.Top - 3;
  end
  else
  begin
    FFullHeight := ClientHeight;
    DetailsCaption := DetailsCaption + ' >>';
    if FNonDetailsHeight = 0 then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
    Constraints.MinHeight := FNonDetailsHeight;
    Constraints.MaxHeight := FNonDetailsHeight
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ShowException(E: Exception; Thread: TJclDebugThread);
begin
  if ExceptionDialogMail = nil then
    ExceptionDialogMail := TExceptionDialogMailClass.Create(Application);
  try
    with ExceptionDialogMail do
    begin
      FIsMainThead := (GetCurrentThreadId = MainThreadID);
      FLastActiveControl := Screen.ActiveControl;
      TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.Message));
      UpdateTextLabelScrollbars;
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      if Thread = nil then
        DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]))
      else
        DetailsMemo.Lines.Add(Format(RsThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      ShowModal;
    end;
  finally
    FreeAndNil(ExceptionDialogMail);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.UpdateTextLabelScrollbars;
begin
  if Tag and DisableTextScrollbar = 0 then
  begin
    Canvas.Font := TextLabel.Font;
    if TextLabel.Lines.Count * Canvas.TextHeight('Wg') > TextLabel.ClientHeight then
      TextLabel.ScrollBars := ssVertical
    else
      TextLabel.ScrollBars := ssNone;
   end;   
end;

//==================================================================================================
// Exception handler initialization code
//==================================================================================================

procedure InitializeHandler;
begin
  JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode];
  {$IFNDEF HOOK_DLL_EXCEPTIONS}
  JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  JclDebugThreadList.OnSyncException := TExceptionDialogMail.ExceptionThreadHandler;
  JclStartExceptionTracking;
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  if HookTApplicationHandleException then
    JclTrackExceptionsFromLibraries;
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  Application.OnException := TExceptionDialogMail.ExceptionHandler;
end;

//--------------------------------------------------------------------------------------------------

procedure UnInitializeHandler;
begin
  Application.OnException := nil;
  JclDebugThreadList.OnSyncException := nil;
  JclUnhookExceptions;
  JclStopExceptionTracking;
end;

//------------------------------------------------------------------------------------------------

initialization
  InitializeHandler;

finalization
  UnInitializeHandler;

// History:

// $Log: ExceptDlgMail.pas,v $
// Revision 1.2  2005/10/26 03:29:44  rrossmair
// - improved header information, added $Date$ and $Log$ CVS tags.
//

end.
