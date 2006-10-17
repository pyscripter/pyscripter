{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChangeNotify.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thrnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thrnqvist are Copyright (C) 2002 Peter Thrnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A wrapper for the Find[First/Next]ChangeNotification API calls.

Changes:
  //dierk schmid 2004-4-28
  -- TJvChangeNotify: Put property "active" from public to published section
     (cause I always forget to set this property in runtime to true)
  -- TJvChangeNotify.SetActive: Exit if csDesigning in ComponentState (Active is now published)
  -- TJvChangeItem.SetDir: Exception not when csDesigning in ComponentState
     (cause, it was impossible to reset in designtime the directory property)
  -- Same TJvChangeNotify.CheckActive: Exception not when csDesigning+csloading in ComponentState
  -- added procedure TJvChangeNotify.Loaded; override;

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvChangeNotify.pas,v 1.41 2005/11/27 10:57:46 ahuser Exp $

unit JvChangeNotify;

interface

{$I jvcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Text,
  {$ENDIF CLR}
  Windows, Classes,
  JvComponentBase, JvTypes;

type
  TJvNotifyArray = array [0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
  TJvChangeAction = (caChangeFileName, caChangeDirName, caChangeAttributes, caChangeSize,
    caChangeLastWrite, caChangeSecurity);
  TJvChangeActions = set of TJvChangeAction;
  TJvNotifyEvent = procedure(Sender: TObject; Dir: string; Actions: TJvChangeActions) of object;
  TJvThreadNotifyEvent = procedure(Sender: TObject; Index: Integer) of object;
  TJvNotifyError = procedure(Sender: TObject; const Msg: string) of object;

  TJvChangeItems = class;
  TJvChangeNotify = class;

//  Added by KV
  EJVCLChangeNotifyException =  class(EJVCLException)
  private
    fErrorDirectory : string;
  public
    property ErrorDirectory : string read fErrorDirectory;
  end;
//  End KV

  TJvChangeItem = class(TCollectionItem)
  private
    FParent: TJvChangeItems;
    FActions: TJvChangeActions;
    FSubTrees: Boolean;
    FDir: string;
    FOnChange: TNotifyEvent;
    procedure SetSubTrees(const Value: Boolean);
    procedure SetDir(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure Change; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Directory: string read FDir write SetDir;
    property Actions: TJvChangeActions read FActions write FActions default [caChangeFileName, caChangeDirName];
    property IncludeSubTrees: Boolean read FSubTrees write SetSubTrees default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvChangeItems = class(TCollection)
  protected
    FOwner: TJvChangeNotify;
    function GetItem(Index: Integer): TJvChangeItem;
    procedure SetItem(Index: Integer; Value: TJvChangeItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TJvChangeNotify);
    function Add: TJvChangeItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvChangeItem read GetItem write SetItem; default;
  end;

  { WARNING: Do not call Thread.Terminate from user code. This will leave a
    dangling TJvChangeNotify.FThread reference which will cause an access
    violation at the next TJvChangeNotify.SetActive call. }
  TJvChangeThread = class(TThread)
  private
    FNotifyArray: TJvNotifyArray;
    FCount: Integer;
    FIndex: Integer;
    FInterval: Integer;
    FNotify: TJvThreadNotifyEvent;
    procedure SynchChange;
  protected
    procedure Execute; override;
  public
    constructor Create(NotifyArray: TJvNotifyArray; Count, Interval: Integer; AFreeOnTerminate: Boolean);
    property OnChangeNotify: TJvThreadNotifyEvent read FNotify write FNotify;
  end;

  TJvChangeNotify = class(TJvComponent)
  private
    FThread: TJvChangeThread;
    FActive: Boolean;
    FInterval: Integer;
    FCollection: TJvChangeItems;
    FNotify: TJvNotifyEvent;
    FNotifyArray: TJvNotifyArray;
    FFreeOnTerminate: Boolean;
    procedure SetCollection(const Value: TJvChangeItems);
    procedure SetInterval(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    procedure CheckActive(const Name: string);
    procedure NotifyError(const Msg: string);
    procedure DoThreadChangeNotify(Sender: TObject; Index: Integer);
    procedure SetFreeOnTerminate(const Value: Boolean);
  protected
    procedure Change(Item: TJvChangeItem); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Notifications: TJvChangeItems read FCollection write SetCollection;
    property CheckInterval: Integer read FInterval write SetInterval default 100;
    // Set FreeOnTerminate to True if you want to be able to change the Active property
    // in the OnChangeNotify event.
    property FreeOnTerminate: Boolean read FFreeOnTerminate write SetFreeOnTerminate default True;
    property OnChangeNotify: TJvNotifyEvent read FNotify write FNotify;
  end;

function ActionsToString(Actions: TJvChangeActions): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile: JvChangeNotify.pas,v $';
    Revision: '$Revision: 1.41 $';
    Date: '$Date: 2005/11/27 10:57:46 $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JvVCL5Utils, JvJCLUtils, JvResources;
  // JvJCLUtils for DirectoryExists

function ActionsToString(Actions: TJvChangeActions): string;
const
  ActionStrings: array [TJvChangeAction] of string =
    (RsFileNameChange, RsDirectoryNameChange, RsAttributesChange,
     RsSizeChange, RsWriteChange, RsSecurityChange);
var
  I: TJvChangeAction;
begin
  Result := '';
  for I := Low(TJvChangeAction) to High(TJvChangeAction) do
    if I in Actions then
      if Result = '' then
        Result := ActionStrings[I]
      else
        Result := Result + ',' + ActionStrings[I];
end;

//=== { TJvChangeItem } ======================================================

constructor TJvChangeItem.Create(Collection: Classes.TCollection);  // TCollection redefined in JvVCL5Utils
begin
  inherited Create(Collection);
  FParent := TJvChangeItems(Collection);
  FSubTrees := False;
  FActions := [caChangeFileName, caChangeDirName];
end;

procedure TJvChangeItem.Assign(Source: TPersistent);
begin
  if Source is TJvChangeItem then
  begin
    Directory := TJvChangeItem(Source).Directory;
    Actions := TJvChangeItem(Source).Actions;
    IncludeSubTrees := TJvChangeItem(Source).IncludeSubTrees;
  end
  else
    inherited Assign(Source);
end;

procedure TJvChangeItem.SetSubTrees(const Value: Boolean);
begin
  if FSubTrees <> Value then
  begin
    if csDesigning in FParent.FOwner.ComponentState then
      FSubTrees := Value
    else
    if Value then
      FSubTrees := Value and (Win32Platform = VER_PLATFORM_WIN32_NT)
    else
      FSubTrees := False;
  end;
end;

procedure TJvChangeItem.SetDir(const Value: string);
begin
  if FDir <> Value then
  begin
    if not (csDesigning in FParent.FOwner.ComponentState) and
      ((Length(Value) = 0) or not DirectoryExists(Value)) then
      {$IFDEF CLR}
      raise EJVCLException.CreateFmt(RsEFmtInvalidPath, [Value]);
      {$ELSE}
      raise EJVCLException.CreateResFmt(@RsEFmtInvalidPath, [Value]);
      {$ENDIF CLR}
    FDir := Value;
  end;
end;

function TJvChangeItem.GetDisplayName: string;
begin
  if FDir <> '' then
    Result := FDir
  else
    Result := inherited GetDisplayName;
end;

procedure TJvChangeItem.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== { TJvChangeItems } =====================================================

constructor TJvChangeItems.Create(AOwner: TJvChangeNotify);
begin
  inherited Create(TJvChangeItem);
  FOwner := AOwner;
end;

function TJvChangeItems.Add: TJvChangeItem;
begin
  if Count < MAXIMUM_WAIT_OBJECTS then
    Result := TJvChangeItem(inherited Add)
  else
    {$IFDEF CLR}
    raise EJVCLException.CreateFmt(RsEFmtMaxCountExceeded, [MAXIMUM_WAIT_OBJECTS]);
    {$ELSE}
    raise EJVCLException.CreateResFmt(@RsEFmtMaxCountExceeded, [MAXIMUM_WAIT_OBJECTS]);
    {$ENDIF CLR}
end;

function TJvChangeItems.GetItem(Index: Integer): TJvChangeItem;
begin
  Result := TJvChangeItem(inherited GetItem(Index));
end;

procedure TJvChangeItems.SetItem(Index: Integer; Value: TJvChangeItem);
begin
  inherited SetItem(Index, Value);
end;

function TJvChangeItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJvChangeItems.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvChangeItems then
  begin
    Clear;
    for I := 0 to TJvChangeItems(Source).Count - 1 do
      Add.Assign(TJvChangeItems(Source)[I]);
  end
  else
    inherited Assign(Source);
end;

//=== { TJvChangeNotify } ====================================================

constructor TJvChangeNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollection := TJvChangeItems.Create(Self);
  FActive := False;
  FInterval := 100;
  FFreeOnTerminate := True;
end;

destructor TJvChangeNotify.Destroy;
begin
  if Assigned(FThread) then
    FThread.FreeOnTerminate := False;
  FFreeOnTerminate := False; // do not call SetFreeOnTerminate here
  Active := False;
  FCollection.Free;
  inherited Destroy;
end;

procedure TJvChangeNotify.CheckActive(const Name: string);
begin
  if Active and
     not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then   //active is now published
    {$IFDEF CLR}
    raise EJVCLException.CreateFmt(RsEFmtCannotChangeName, [Name]);
    {$ELSE}
    raise EJVCLException.CreateResFmt(@RsEFmtCannotChangeName, [Name]);
    {$ENDIF CLR}
end;

procedure TJvChangeNotify.SetCollection(const Value: TJvChangeItems);
begin
  FCollection.Assign(Value);
end;

procedure TJvChangeNotify.Change(Item: TJvChangeItem);
begin
  if Assigned(Item) then
  begin
    Item.Change;
    if Assigned(FNotify) then
      FNotify(Self, Item.Directory, Item.Actions);
  end;
end;

procedure TJvChangeNotify.SetInterval(const Value: Integer);
begin
  CheckActive('Interval');
  if Value <= 0 then
    Exit;
  if FInterval <> Value then
    FInterval := Value;
end;

procedure TJvChangeNotify.NotifyError(const Msg: string);
var
  ErrorMsg: string;
{$IFDEF CLR}
  sb: StringBuilder;
{$ENDIF CLR}
//  Added by KV
 E : EJVCLChangeNotifyException;
begin
  {$IFDEF CLR}
  sb := StringBuilder.Create(256);
  sb.Length := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    GetLastError, 0, sb, sb.Length, nil);
  ErrorMsg := sb.ToString();
  {$ELSE}
  SetLength(ErrorMsg, 256);
  SetLength(ErrorMsg, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    GetLastError, 0, PChar(ErrorMsg), Length(ErrorMsg), nil));
  {$ENDIF CLR}
  //raise EJVCLException.CreateFmt(RsENotifyErrorFmt, [ErrorMsg, Msg]);
  //Added by KV
  E := EJVCLChangeNotifyException.CreateFmt(RsENotifyErrorFmt, [ErrorMsg, Msg]);
  E.fErrorDirectory := Msg;
  raise E;
end;

procedure TJvChangeNotify.DoThreadChangeNotify(Sender: TObject; Index: Integer);
begin
  Change(Notifications[Index]);
end;

procedure TJvChangeNotify.SetActive(const Value: Boolean);
const
  cActions: array [TJvChangeAction] of Cardinal =
   (FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
    FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY);
var
  cA: TJvChangeAction;
  Flags: Cardinal;
  I: Integer;
  J: Integer;  //KV
  S: string;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if csDesigning in ComponentState then
      Exit;   //active is now published

    if FActive then
    begin
      if FCollection.Count > MAXIMUM_WAIT_OBJECTS then
        {$IFDEF CLR}
        raise EJVCLException.CreateFmt(RsEFmtMaxCountExceeded,[MAXIMUM_WAIT_OBJECTS]);
        {$ELSE}
        raise EJVCLException.CreateResFmt(@RsEFmtMaxCountExceeded,[MAXIMUM_WAIT_OBJECTS]);
        {$ENDIF CLR}
      {$IFDEF CLR}
      for I := 0 to High(FNotifyArray) do
        FNotifyArray[I] := INVALID_HANDLE_VALUE;
      {$ELSE}
      FillChar(FNotifyArray, SizeOf(TJvNotifyArray), INVALID_HANDLE_VALUE);
      {$ENDIF CLR}
      for I := 0 to FCollection.Count - 1 do
      begin
        Flags := 0;
        { convert TJvChangeActions to bitfields }
        for cA := Low(TJvChangeAction) to High(TJvChangeAction) do
          if cA in FCollection[I].Actions then
            Flags := Flags or (cActions[cA]);
        S := FCollection[I].Directory;
        if (S = '') or not DirectoryExists(S) then
          {$IFDEF CLR}
          raise EJVCLException.CreateFmt(RsEFmtInvalidPathAtIndex, [S, I]);
          {$ELSE}
          raise EJVCLException.CreateResFmt(@RsEFmtInvalidPathAtIndex, [S, I]);
          {$ENDIF CLR}
        FNotifyArray[I] := FindFirstChangeNotification(
          {$IFDEF CLR} S {$ELSE} PChar(S) {$ENDIF},
          BOOL(FCollection[I].IncludeSubTrees), Flags);
//        if FNotifyArray[I] = INVALID_HANDLE_VALUE then
//          NotifyError(FCollection[I].Directory);
//      Added by KV
        if FNotifyArray[I] = INVALID_HANDLE_VALUE then begin
          // Clean up before raising the exception
          for J := 0 to I - 1 do begin
            FindCloseChangeNotification(FNotifyArray[J]);
            FNotifyArray[J] := INVALID_HANDLE_VALUE;
          end;
          FActive := False;
          // Now raise the exception
          NotifyError(FCollection[I].Directory);
        end;
      end;
//    End  Added by KV
      if FThread <> nil then
      begin
        FThread.OnChangeNotify := nil;
        FThread.Terminate;
        if FThread.Suspended then
          FThread.Resume;
        if FreeOnTerminate then
          FThread := nil
        else
        begin
          FThread.WaitFor;
          FreeAndNil(FThread);
        end;
      end;
      FThread := TJvChangeThread.Create(FNotifyArray, FCollection.Count, FInterval, FFreeOnTerminate);
      FThread.OnChangeNotify := DoThreadChangeNotify;
      FThread.Resume;
    end
    else
    if FThread <> nil then
    begin
      FThread.OnChangeNotify := nil;
      FThread.Terminate;
      if FThread.Suspended then
        FThread.Resume;
      if FreeOnTerminate then
        FThread := nil
      else
      begin
        FThread.WaitFor;
        FreeAndNil(FThread);
      end;
    end;

    {
      while FActive do
      begin
        I := WaitForMultipleObjects(FCollection.Count, @FNotifyArray, False, FInterval);
        if (I >= 0) and (I < FCollection.Count) then
        begin
          try
            Change(FCollection.Items[I]);
          finally
            Assert(FindNextChangeNotification(FNotifyArray[I]));
          end;
        end
        else
          Application.ProcessMessages;
      end;
    for I := 0 to FCollection.Count - 1 do // Iterate
      FindCloseChangeNotification(FNotifyArray[I]);
      }
  end;
end;

procedure TJvChangeNotify.Loaded;
begin
  inherited Loaded;
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TJvChangeNotify.SetFreeOnTerminate(const Value: Boolean);
var
  State: Boolean;
begin
  if csLoading in ComponentState then
    FFreeOnTerminate := Value
  else
  begin
    State := Active;
    try
      Active := False;
      FFreeOnTerminate := Value;
    finally
      Active := State;
    end;
  end;
end;

//=== { TJvChangeThread } ====================================================

constructor TJvChangeThread.Create(NotifyArray: TJvNotifyArray; Count, Interval: Integer; AFreeOnTerminate:Boolean);
var
  I: Integer;
begin
  inherited Create(True);
  FCount := Count;
  FInterval := Interval;
  {$IFDEF CLR}
  for I := 0 to High(FNotifyArray) do
    FNotifyArray[I] := INVALID_HANDLE_VALUE;
  {$ELSE}
  FillChar(FNotifyArray, SizeOf(TJvNotifyArray), INVALID_HANDLE_VALUE);
  {$ENDIF CLR}
  for I := 0 to FCount - 1 do
    FNotifyArray[I] := NotifyArray[I];
  FreeOnTerminate := AFreeOnTerminate;
end;

procedure TJvChangeThread.Execute;
var
  I: Integer;
begin
  // (rom) secure thread against exceptions (Delphi 5 needs it)
  try
    while not Terminated do
    begin
      I := WaitForMultipleObjects(FCount,
        {$IFDEF CLR}
        FNotifyArray,
        {$ELSE}
        @FNotifyArray[0],
        {$ENDIF CLR}
        False, FInterval);
      if (I >= 0) and (I < FCount) and not Terminated then
      begin
        try
          FIndex := I;
          Synchronize(SynchChange);
        finally
          // (rom) raising an exception in a thread is not a good idea
          // (rom) Assert removed
          //Assert(FindNextChangeNotification(FNotifyArray[I]));
          FindNextChangeNotification(FNotifyArray[I]);
        end;
      end;
    end;
    if Terminated then
      for I := 0 to FCount - 1 do
        if FNotifyArray[I] <> INVALID_HANDLE_VALUE then
        begin
          FindCloseChangeNotification(FNotifyArray[I]);
          FNotifyArray[I] := INVALID_HANDLE_VALUE;
        end;
  except
  end;
end;

procedure TJvChangeThread.SynchChange;
begin
  if Assigned(FNotify) then
    FNotify(Self, FIndex);
end;

{$IFDEF UNITVERSIONING}

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

