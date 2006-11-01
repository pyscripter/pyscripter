{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppInst.pas, released on 2003-10-07.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvAppInst.pas,v 1.26 2005/03/18 19:04:10 ahuser Exp $

unit JvAppInst;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Forms,
  Classes, { Classes must be after Forms for Delphi 5 compatibility. }
  JclAppInst;

type
  TJvAppInstDataKind = TJclAppInstDataKind; // = Integer

  TInstanceChangeEvent = procedure(Sender: TObject; ProcessId: Cardinal) of object;
  TUserNotifyEvent = procedure(Sender: TObject; Param: Integer) of object;
  TDataAvailableEvent = procedure(Sender: TObject; Kind: TJvAppInstDataKind;
    Data: Pointer; Size: Integer) of object;
    { Data contains the sent data and is released when the function returns }
  TCmdLineReceivedEvent = procedure(Sender: TObject; CmdLine: TStrings) of object;

  { TJvAppInstance encapsulates the TJclAppInstance class. To set a
    UniqueAppIdGuidStr you must call JclAppInst.JclAppInstances in the
    initialization section of a unit or before the forms are created (OnCreate
    is too late).
    This class is not thread safe. }
  TJvAppInstances = class(TComponent)
  private
    FHandle: THandle;
    FOnInstanceCreated: TInstanceChangeEvent;
    FOnInstanceDestroyed: TInstanceChangeEvent;
    FOnUserNotify: TUserNotifyEvent;
    FOnDataAvailable: TDataAvailableEvent;
    FOnCmdLineReceived: TCmdLineReceivedEvent;
    FOnRejected: TNotifyEvent;
    FAutoActivate: Boolean;
    FMaxInstances: Integer;
    FActive: Boolean;
    FSendCmdLine: Boolean;
    function GetAppInstances: TJclAppInstances;
  protected
    procedure Loaded; override;
    procedure WndProc(var Msg: TMessage); virtual;
    function GetIsRemoteInstanceActive: Boolean;
    procedure DoInstanceCreated(ProcessId: Cardinal); virtual;
    procedure DoInstanceDestroyed(ProcessId: Cardinal); virtual;
    procedure DoUserNotify(Param: Integer); virtual;
    procedure DoDataAvailable(Kind: TJvAppInstDataKind; Data: Pointer; Size: Integer); virtual;
    procedure DoCmdLineReceived(CmdLine: TStrings); virtual;
    procedure DoRejected; virtual;
    property Handle: THandle read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Check;
    procedure UserNotify(Param: Integer);
    function SendData(DataKind: TJclAppInstDataKind; Data: Pointer; Size: Integer): Boolean;
    property AppInstances: TJclAppInstances read GetAppInstances;
  published
    property Active: Boolean read FActive write FActive default True;
    property AutoActivate: Boolean read FAutoActivate write FAutoActivate default True;
     { AutoActivate: True means that the first instance is brought to front
       by the second process instance. }
    property MaxInstances: Integer read FMaxInstances write FMaxInstances default 1;
     { MaxInstances: 0 means no restriction }
    property SendCmdLine: Boolean read FSendCmdLine write FSendCmdLine default True;
     { SendCmdLine: True means that the second process instance sends it's
       CmdLine to the first instance before it terminates. }
    property OnInstanceCreated: TInstanceChangeEvent read FOnInstanceCreated write FOnInstanceCreated;
    property OnInstanceDestroyed: TInstanceChangeEvent read FOnInstanceDestroyed write FOnInstanceDestroyed;
    property OnUserNotify: TUserNotifyEvent read FOnUserNotify write FOnUserNotify;
    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnCmdLineReceived: TCmdLineReceivedEvent read FOnCmdLineReceived write FOnCmdLineReceived;
    property OnRejected: TNotifyEvent read FOnRejected write FOnRejected;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile: JvAppInst.pas,v $';
    Revision: '$Revision: 1.26 $';
    Date: '$Date: 2005/03/18 19:04:10 $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JvJVCLUtils;

const
  sAppInstancesWindowClassName = 'JvAppInstances_WindowClass'; // do not localize
  AI_GETACTIVE = $0004;
  AI_SETACTIVE = $0005;

{$IFDEF VisualCLX}
type
  TPrivateComponent = class(TPersistent, IInterface, IInterfaceComponentReference)
  protected
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
  public
    function _AddRef: Integer; virtual; stdcall; abstract;
    function _Release: Integer; virtual; stdcall; abstract;
    function GetComponent: TComponent; virtual; abstract;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall; abstract;
  end;
{$ENDIF VisualCLX}

var
  FirstJvAppInstance: Boolean = True;

constructor TJvAppInstances.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    FHandle := AllocateHWndEx(WndProc, sAppInstancesWindowClassName);
    if FirstJvAppInstance then
    begin
      FirstJvAppInstance := False;
      AppInstances.CheckInstance($FFFF); // increase shared instance count
    end;
  end;

  FActive := True;
  FMaxInstances := 1;
  FAutoActivate := True;
  FSendCmdLine := True;
end;

destructor TJvAppInstances.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DeallocateHWndEx(FHandle);
  inherited Destroy;
end;

procedure TJvAppInstances.Check;
begin
  if Active and not (csDesigning in ComponentState) then
    if MaxInstances > 0 then
//      if AppInstances.InstanceCount > MaxInstances then
      if AppInstances.InstanceIndex[GetCurrentProcessId] >= MaxInstances then
      begin
        if GetIsRemoteInstanceActive then
        begin
          DoRejected;
          if AutoActivate then
            AppInstances.SwitchTo(0);
          if SendCmdLine then
            AppInstances.SendCmdLineParams(sAppInstancesWindowClassName, Handle);

          AppInstances.Free;  // Calls protected method RemoveInstance
          TerminateProcess(GetCurrentProcess, 0);
        end;
      end;
end;

procedure TJvAppInstances.DoCmdLineReceived(CmdLine: TStrings);
begin
  if Assigned(FOnCmdLineReceived) then
    FOnCmdLineReceived(Self, CmdLine);
end;

procedure TJvAppInstances.DoDataAvailable(Kind: TJvAppInstDataKind;
  Data: Pointer; Size: Integer);
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(Self, Kind, Data, Size);
end;

procedure TJvAppInstances.DoInstanceCreated(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceCreated) then
    FOnInstanceCreated(Self, ProcessId);
end;

procedure TJvAppInstances.DoInstanceDestroyed(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceDestroyed) then
    FOnInstanceDestroyed(Self, ProcessId);
end;

procedure TJvAppInstances.DoUserNotify(Param: Integer);
begin
  if Assigned(FOnUserNotify) then
    FOnUserNotify(Self, Param);
end;

procedure TJvAppInstances.DoRejected;
begin
  if Assigned(FOnRejected) then
    FOnRejected(Self);
end;

function TJvAppInstances.GetAppInstances: TJclAppInstances;
begin
  if csDesigning in ComponentState then
    Result := nil
  else
    Result := JclAppInstances; // create AppInstance
end;

procedure TJvAppInstances.Loaded;
begin
  inherited Loaded;
  Check;
end;

procedure TJvAppInstances.WndProc(var Msg: TMessage);
var
  Kind: TJvAppInstDataKind;
  Data: Pointer;
  Size: Integer;
  CmdLine: TStrings;
begin
  try
    if Msg.Msg = AppInstances.MessageID then
    begin
      case Msg.WParam of
        AI_INSTANCECREATED:
          if Cardinal(Msg.LParam) <> GetCurrentProcessId then
            DoInstanceCreated(Cardinal(Msg.LParam));
        AI_INSTANCEDESTROYED:
          DoInstanceDestroyed(Cardinal(Msg.LParam));
        AI_USERMSG:
          DoUserNotify(Msg.LParam);
        AI_GETACTIVE:
          SendMessage(HWND(Msg.LParam), AppInstances.MessageID,
            AI_SETACTIVE, Ord(Active));
        AI_SETACTIVE:
          Active := Msg.LParam <> 0;
      end;
    end
    else
    begin
      Kind := ReadMessageCheck(Msg, Handle);
      case Kind of
        AppInstDataKindNoData:
          ; // do nothing
        AppInstCmdLineDataKind:
          begin
            if Assigned(FOnCmdLineReceived) then
            begin
              CmdLine := TStringList.Create;
              try
                ReadMessageStrings(Msg, CmdLine);
                DoCmdLineReceived(CmdLine);
              finally
                CmdLine.Free;
              end;
            end;
            Exit;
          end;
      else
        if Assigned(FOnDataAvailable) then
        begin
          ReadMessageData(Msg, Data, Size);
          try
            DoDataAvailable(Kind, Data, Size);
          finally
            FreeMem(Data);
          end;
        end;
        Exit;
      end;
    end;
  except
    on E: Exception do
      Application.ShowException(E);
  end;

  with Msg do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TJvAppInstances.UserNotify(Param: Integer);
begin
  AppInstances.UserNotify(Param);
end;

function TJvAppInstances.SendData(DataKind: TJclAppInstDataKind;
  Data: Pointer; Size: Integer): Boolean;
begin
  Result := AppInstances.SendData(sAppInstancesWindowClassName, DataKind, Data,
    Size, Handle);
end;


function TJvAppInstances.GetIsRemoteInstanceActive: Boolean;
type
  PData = ^TData;
  TData = record
    Instance: TJvAppInstances;
    Message: TMessage;
  end;

var
  I: Integer;
  Wnd: HWND;
  TID: DWORD;
  Data: TData;

  function EnumWinProc(Wnd: HWND; Data: PData): BOOL; stdcall;
  begin
    with Data^.Message do
      SendMessage(Wnd, Msg, WParam, LParam);
    Result := Data^.Instance.Active;
  end;

begin
  for I := 0 to AppInstances.InstanceCount - 1 do
  begin
    if AppInstances.ProcessIDs[I] = GetCurrentProcessId then
      Continue;
    Wnd := AppInstances.AppWnds[I];
    TID := GetWindowThreadProcessId(Wnd, nil);
    Data.Instance := Self;
    Data.Message.Msg := AppInstances.MessageID;
    Data.Message.WParam := AI_GETACTIVE;
    Data.Message.LParam := Handle;
    EnumThreadWindows(TID, @EnumWinProc, LPARAM(@Data));
    if not Active then
      Break;
  end;
  Result := Active;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

