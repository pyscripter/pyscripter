{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDSADialogs.PAS, released on 2002-08-23.

The Initial Developer of the Original Code is Marcel Bestebroer [marcelb att zeelandnet dott nl]
Portions created by Marcel Bestebroer are Copyright (C) 2002 Marcel Bestebroer.
All Rights Reserved.

Contributor(s):
  Steve Magruder

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDSADialogs;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Contnrs, Graphics, Controls, Forms, StdCtrls, Dialogs,
  ExtCtrls, JvComponent,
  JvComponentBase, JvDynControlEngine, JvTypes, JvAppStorage,
  JvDynControlEngineIntf;

type
  TDlgCenterKind = (dckScreen, dckMainForm, dckActiveForm);

  TDSAMessageForm = class(TJvForm)
  private
    FTimeout: Integer;
    FTimer: TTimer;
    FCountdown: IJvDynControlCaption;
    FMsg: string;
    FDefaultButton: {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF};
  protected
    property DefaultButton: {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF} read FDefaultButton write FDefaultButton ;
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CustomMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CustomShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure WriteToClipboard(const Text: string);
    function GetFormText: string;
    class function TimeoutUnit(Count: Integer; Seconds: Boolean = True): string;
    procedure CancelAutoClose;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    function IsDSAChecked: Boolean;
    property Msg: string read FMsg write FMsg;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

//----------------------------------------------------------------------------
// DSA storage and registration classes, types, constants and exceptions
//----------------------------------------------------------------------------

type
  TDSACheckTextKind = type Integer;

const
  ctkShow = 0;
  ctkAsk = 1;
  ctkWarn = 2;

type
  TDSAStorage = class;

  TDSARegItem = record
    ID: Integer;
    Name: string;
    Description: string;
    Storage: TDSAStorage;
    ChkTextKind: TDSACheckTextKind;
  end;

  TDSACustomData = procedure(const Storage: TDSAStorage; const DSAInfo: TDSARegItem) of object;

  TDSAStorage = class(TObject)
  private
    FStates: TStack;
  protected
    procedure BeginCustomRead(const DSAInfo: TDSARegItem); virtual;
    procedure BeginCustomWrite(const DSAInfo: TDSARegItem); virtual;
    procedure BeginRead(const DSAInfo: TDSARegItem); virtual;
    procedure BeginWrite(const DSAInfo: TDSARegItem); virtual;
    procedure EndCustomRead(const DSAInfo: TDSARegItem); virtual;
    procedure EndCustomWrite(const DSAInfo: TDSARegItem); virtual;
    procedure EndRead(const DSAInfo: TDSARegItem); virtual;
    procedure EndWrite(const DSAInfo: TDSARegItem); virtual;
    function IsKeyNameAllowed(const Key: string): Boolean;
    function GetCheckMarkTextSuffix: string; virtual; abstract;
    procedure SetCheckMarkTextSuffix(const Value: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function GetState(const DSAInfo: TDSARegItem; out LastResult: Integer;
      const OnCustomData: TDSACustomData = nil): Boolean; virtual;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; virtual; abstract;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Boolean): Boolean; virtual; abstract;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; virtual; abstract;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Extended): Extended; virtual; abstract;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; virtual; abstract;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Int64): Int64; virtual; abstract;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; virtual; abstract;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Integer): Integer; virtual; abstract;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; virtual; abstract;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: string): string; virtual; abstract;
    procedure SetState(const DSAInfo: TDSARegItem; const DontShowAgain: Boolean;
      const LastResult: Integer; const OnCustomData: TDSACustomData = nil); virtual;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: Boolean); virtual; abstract;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: Extended); virtual; abstract;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: Int64); virtual; abstract;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: Integer); virtual; abstract;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); virtual; abstract;
    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix;
  end;

  {$IFDEF MSWINDOWS}
  TDSARegStorage = class(TDSAStorage)
  private
    FRootKey: HKEY;
    FKey: string;
  protected
    procedure CreateKey(const DSAInfo: TDSARegItem); virtual;
    function GetCheckMarkTextSuffix: string; override;
    procedure SetCheckMarkTextSuffix(const Value: string); override;
  public
    constructor Create(const ARootKey: HKEY; const AKey: string);
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;
    property RootKey: HKEY read FRootKey write FRootKey;
    property Key: string read FKey write FKey;
  end;
  {$ENDIF MSWINDOWS}

  TDSAQueueStorage = class(TDSAStorage)
  private
    FList: TStringList;
    FCheckMarkSuffix: string;
  protected
    procedure AddDSA(const DSAInfo: TDSARegItem);
    procedure DeleteDSA(const Index: Integer);
    function FindDSA(const DSAInfo: TDSARegItem): Integer;
    function GetCheckMarkTextSuffix: string; override;
    function GetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: Integer): string;
    function HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): Boolean;
    procedure SetCheckMarkTextSuffix(const Value: string); override;
    procedure SetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: Integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;
    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix write SetCheckMarkTextSuffix;
  end;

const
  ssCustomRead: Pointer = @TDSAStorage.BeginCustomRead;
  ssCustomWrite: Pointer = @TDSAStorage.BeginCustomWrite;
  ssRead: Pointer = @TDSAStorage.BeginRead;
  ssWrite: Pointer = @TDSAStorage.BeginWrite;

//--------------------------------------------------------------------------------------------------
// MessageDlg replacements and extensions
//--------------------------------------------------------------------------------------------------

// Additional values for DefaultButton, CancelButton and HelpButton parameters

const
  mbNone = TMsgDlgBtn(-1);
  mbDefault = TMsgDlgBtn(-2);

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const ADynControlEngine: TJvDynControlEngine = nil);
procedure ShowMessageFmt(const Msg: string; const Params: array of const; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string;
  const Results: array of Integer; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string;
  const Results: array of Integer; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic; const Buttons: array of string;
  const Results: array of Integer; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;

//--------------------------------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//--------------------------------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: Integer; const Msg: string; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);
procedure DSAShowMessageFmt(const DlgID: Integer; const Msg: string; const Params: array of const;
  const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const ADynControlEngine: TJvDynControlEngine = nil);
function DSAMessageDlg(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault;
  const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault;
  const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const Timeout: Integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault;
  const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlgEx(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1; const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): Integer; overload;
function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): Integer; overload;

//----------------------------------------------------------------------------
// Generic DSA dialog
//----------------------------------------------------------------------------

function CreateDSAMessageForm(const ACaption, Msg: string; const APicture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Integer;
  const CheckCaption: string; const Center: TDlgCenterKind = dckScreen;
  const ATimeout: Integer = 0; const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  HelpButton: Integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TDSAMessageForm;

//----------------------------------------------------------------------------
// DSA registration
//----------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
procedure UnregisterDSA(const DlgID: Integer);
function LocateDSAReg(const DlgID: Integer): TDSARegItem;

//----------------------------------------------------------------------------
// DSA state setting/retrieving
//----------------------------------------------------------------------------

function GetDSAState(const DlgID: Integer): Boolean; overload;
function GetDSAState(const DlgID: Integer; out ResCode: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean; overload;
procedure SetDSAState(const DlgID: Integer; const DontShowAgain: Boolean;
  const LastResult: Integer = mrNone; const OnCustomData: TDSACustomData = nil);

//----------------------------------------------------------------------------
// Iterating the DSA registration
//----------------------------------------------------------------------------

function DSACount: Integer;
function DSAItem(const Index: Integer): TDSARegItem;

//----------------------------------------------------------------------------
// DSA check box text registration
//----------------------------------------------------------------------------

procedure RegisterDSACheckMarkText(const ID: TDSACheckTextKind; const Text: string);
procedure UnregisterDSACheckMarkText(const ID: TDSACheckTextKind);
function GetDSACheckMarkText(const ID: TDSACheckTextKind): string;

//----------------------------------------------------------------------------
// Standard DSA storage devices
//----------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function DSARegStore: TDSARegStorage;
{$ENDIF MSWINDOWS}
function DSAQueueStore: TDSAQueueStorage;

//----------------------------------------------------------------------------
// DSA time formatting function.
// Returns a string representing the number of seconds.
// Standard function returns this:
// "(Secs) sec" if Secs is lower than 60
// "(Secs div 60) min (Secs mod 60) sec" if Secs is greater or equal to 60
// The min and sec constants are taken from resource strings in JvResources.
//----------------------------------------------------------------------------
type
  TJvDSATimeFormatter = function(Secs: Integer) : string;

procedure SetDSATimeFormatter(const ATimeFormatter: TJvDSATimeFormatter);
function StandardDSATimeFormatter(Secs: Integer) : string;

//----------------------------------------------------------------------------
// VCL component
//----------------------------------------------------------------------------

type
  EJvDSADialog = class(EJVCLException);

  TJvDSADataEvent = procedure(Sender: TObject; const DSAInfo: TDSARegItem; const Storage: TDSAStorage) of object;
  TJvDSAAutoCloseEvent = procedure(Sender: TObject; var Handled: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDSADialog = class(TJvComponent)
  private
    FCheckControl: TWinControl;
    FDialogID: Integer;
    FIgnoreDSAChkMrkTxt: Boolean;
    FOnUpdateKeys: TJvDSADataEvent;
    FOnApplyKeys: TJvDSADataEvent;
    FOrgOwner: TComponent;
    FOrgShowModalPtr: Pointer;
    FTimeout: Integer;
    FTimer: TTimer;
    FTimerCount: Integer;
    FOnCountdown: TNotifyEvent;
    FOnAutoClose: TJvDSAAutoCloseEvent;
  protected
    procedure AutoClose;
    procedure AfterShow; virtual;
    procedure ApplySavedState; virtual;
    procedure BeforeShow; virtual;
    procedure DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    function DoAutoClose: Boolean;
    procedure DoCountDown;
    procedure DoUpdateKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    function GetDSAStateInternal(out ModalResult: Integer): Boolean;
    function GetOrgOwner: TComponent;
    function GetOrgShowModalPtr: Pointer;
    function GetStorage: TDSAStorage;
    procedure FormPatch;
    procedure FormUnPatch;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCheckControl(Value: TWinControl); virtual;
    procedure SetDialogID(Value: Integer); virtual;
    procedure SetOrgOwner(Value: TComponent);
    procedure SetOrgShowModalPtr(Value: Pointer);
    procedure TimerEvent(Sender: TObject);
    procedure UpdateDSAState; virtual;
    property OrgOwner: TComponent read GetOrgOwner write SetOrgOwner;
    property OrgShowModalPtr: Pointer read GetOrgShowModalPtr write SetOrgShowModalPtr;
    property Storage: TDSAStorage read GetStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetModalResult: Integer; virtual;
    function IsDSAChecked: Boolean; virtual;
    procedure Loaded; override;
    procedure CancelCountdown; virtual;
    function SecondsLeft: Integer;
  published
    property Timeout: Integer read FTimeout write FTimeout;
    property CheckControl: TWinControl read FCheckControl write SetCheckControl;
    property DialogID: Integer read FDialogID write SetDialogID;
    property IgnoreDSAChkMrkTxt: Boolean read FIgnoreDSAChkMrkTxt write FIgnoreDSAChkMrkTxt;
    property OnApplyKeys: TJvDSADataEvent read FOnApplyKeys write FOnApplyKeys;
    property OnUpdateKeys: TJvDSADataEvent read FOnUpdateKeys write FOnUpdateKeys;
    property OnCountdown: TNotifyEvent read FOnCountdown write FOnCountdown;
    property OnAutoClose: TJvDSAAutoCloseEvent read FOnAutoClose write FOnAutoClose;
  end;

type
  TDSAAppStorage = class(TDSAStorage)
  private
    FAppStorage : TJvCustomAppStorage;
    FAppStoragePath: string;
  protected
    function GetCheckMarkTextSuffix: string; override;
    procedure SetCheckMarkTextSuffix(const Value: string); override;
  public
    constructor Create(AAppStorage : TJvCustomAppStorage; const APath : string);
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Types, Consts, Math, TypInfo,
  {$IFDEF MSWINDOWS}
  JclRegistry,
  {$ENDIF MSWINDOWS}
  JclBase, JclSysUtils,
  JvConsts, JvResources, JvJVCLUtils;

const
  cDSAStateValueName = 'DSA_State'; // do not localize
  cDSAStateLastResultName = 'LastResult'; // do not localize

  sPathStr = '%s\%s\%s';


type
  PBoolean = ^Boolean;

var
  TimeFormatter: TJvDSATimeFormatter = StandardDSATimeFormatter;

procedure SetDSATimeFormatter(const ATimeFormatter: TJvDSATimeFormatter);
begin
  TimeFormatter := ATimeFormatter
end;

function StandardDSATimeFormatter(Secs: Integer) : string;
var
  Mins: Integer;
  TimeStr: string;
begin
  Mins := Secs div 60;
  Secs := Secs mod 60;
  if Mins <> 0 then
    TimeStr := Format('%d %s %d %s', [Mins, TDSAMessageForm.TimeoutUnit(Mins, False),
                                      Secs, TDSAMessageForm.TimeoutUnit(Secs)])
  else
    TimeStr := Format('%d %s', [Secs, TDSAMessageForm.TimeoutUnit(Secs)]);

  Result := Format(RsCntdownText, [TimeStr]);
end;

//=== CheckMarkTexts =========================================================

var
  GlobalCheckMarkTexts: TStringList = nil;

function CheckMarkTexts: TStrings;
begin
  if GlobalCheckMarkTexts = nil then
    GlobalCheckMarkTexts := TStringList.Create;
  Result := GlobalCheckMarkTexts;
end;

function GetCheckMarkText(const ID: TDSACheckTextKind): string;
var
  Idx: Integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    Result := CheckMarkTexts[Idx]
  else
    Result := '';
end;

//=== { TDSAMessageForm } ====================================================

constructor TDSAMessageForm.CreateNew(AOwner: TComponent; Dummy: Integer);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner, Dummy);
  {$IFDEF RTL210_UP}
  NonClientMetrics.cbSize := TNonClientMetrics.SizeOf;
  {$ELSE}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  {$ENDIF RTL210_UP}
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, NonClientMetrics.cbSize, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.OnTimer := TimerEvent;
end;

procedure TDSAMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  CancelAutoClose;
  if (Shift = [ssCtrl]) and (Key = Word('C')) then
  begin
    WriteToClipboard(GetFormText);
  end;
  if Key = VK_RETURN then
    if ActiveControl is {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF} then
      {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF}(ActiveControl).Click
    else if Assigned(DefaultButton) then
      DefaultButton.Click;
end;

procedure TDSAMessageForm.CustomMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CancelAutoClose;
end;

procedure TDSAMessageForm.CustomShow(Sender: TObject);
var
  I: Integer;
  First : Boolean;
  Btn : {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF};
begin
  if Timeout <> 0 then
    FTimer.Enabled := True;
  First := True;
  for I := 0 to ComponentCount - 1 do
  begin
    if (Components[i] is {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF}) then
    begin
      Btn := (Components[i] as {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF});
      if First then
      begin
        First := False;
        Btn.SetFocus;
      end
      else if Btn.Default then
      begin
        Btn.SetFocus;
        Break;
      end;
    end;
  end;
  Supports(FindComponent('CountDown'),IJvDynControlCaption, FCountDown);
end;

procedure TDSAMessageForm.HelpButtonClick(Sender: TObject);
begin
  CancelAutoClose;
  Application.HelpContext(HelpContext);
end;

procedure TDSAMessageForm.TimerEvent(Sender: TObject);
var
  I: Integer;
begin
  if FTimer.Enabled then
  begin
    Dec(FTimeout);
    if FTimeout = 0 then
    begin
      FTimer.Enabled := False;
      for I := 0 to ComponentCount - 1 do
      begin
        if (Components[i] is {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF}) and (Components[i] as {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF}).Default then
        begin
          (Components[i] as {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF}).Click;
          Exit;
        end;
      end;
      // No default button found; just close the form
      Close;
    end
    else
    if Assigned(FCountdown) then
      FCountdown.ControlCaption := TimeFormatter(Timeout);
  end;
end;


procedure TDSAMessageForm.WriteToClipboard(const Text: string);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if OpenClipboard(0) then
  begin
    try
      Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Length(Text) + 1) * SizeOf(Char));
      try
        DataPtr := GlobalLock(Data);
        try
          Move(PChar(Text)^, DataPtr^, (Length(Text) + 1) * SizeOf(Char));
          EmptyClipboard;
          SetClipboardData({$IFDEF UNICODE}CF_UNICODETEXT{$ELSE}CF_TEXT{$ENDIF UNICODE}, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    finally
      CloseClipboard;
    end;
  end
  else
    raise EJVCLException.CreateRes(@SCannotOpenClipboard);
end;




function TDSAMessageForm.GetFormText: string;
var
  DividerLine, ButtonCaptions: string;
  I: Integer;
begin
  DividerLine := StringOfChar('-', 27) + CrLf;
  for I := 0 to ComponentCount - 1 do
    if Components[i] is {$IFDEF RTL200_UP}TCustomButton{$ELSE}TButton{$ENDIF} then
      ButtonCaptions := ButtonCaptions + TButton(Components[i]).Caption + StringOfChar(' ', 3);
  ButtonCaptions := StringReplace(ButtonCaptions, '&', '', [rfReplaceAll]);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, CrLf, DividerLine,
    Msg, CrLf, DividerLine, ButtonCaptions, CrLf, DividerLine]);
end;

class function TDSAMessageForm.TimeoutUnit(Count: Integer; Seconds: Boolean): string;
begin
  if Seconds then
    if Count <> 1 then
      Result := RsCntdownSecsText
    else
      Result := RsCntdownSecText
  else
    if Count <> 1 then
      Result := RsCntdownMinsText
    else
      Result := RsCntdownMinText;
end;

procedure TDSAMessageForm.CancelAutoClose;
begin
  FTimer.Enabled := False;
end;

function TDSAMessageForm.IsDSAChecked: Boolean;
var
  I: Integer;
begin
  I := ComponentCount - 1;
  while (I > -1) and not (Components[i] is TCustomCheckBox) do
    Dec(I);
  if (I > -1) then
    Result := TCheckBox(Components[i]).Checked
  else
    Result := False;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array [0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint32(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function CreateDSAMessageForm(const ACaption, Msg: string; const APicture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Integer;
  const CheckCaption: string; const Center: TDlgCenterKind = dckScreen;
  const ATimeout: Integer = 0; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; HelpButton: Integer = -1;
  const ADynControlEngine: TJvDynControlEngine = nil): TDSAMessageForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 4;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth: Integer;
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth: Integer;
  IconWidth, IconHeight,
  TextWidth, TextHeight,
  X: Integer;
  ChkTextWidth: Integer;
  TimeoutTextWidth: Integer;
  TempRect, TextRect: TRect;
  I: Integer;
  CenterParent: TComponent;
  CenterParLeft, CenterParTop, CenterParWidth, CenterParHeight: Integer;
  DynControlEngine: TJvDynControlEngine;
  CountDownlabel, MessageLabel: TControl;
  Image: TWinControl;
  DynControlImage: IJvDynControlImage;
  DynControlLabel: IJvDynControlLabel;
  MessagePanel: TWinControl;
  BottomPanel: TWinControl;
  ResultForm : TDSAMessageForm;
  Button : TButton;
  CheckBox : TWinControl;
  ImagePanel: TWinControl;
  CheckPanel : TWinControl;
  MainPanel : TWinControl;
  DynControlAutoSize: IJvDynControlAutoSize;

  procedure CalcTextRect(iSingle: Boolean; lpString: PChar; nCount: Integer; var lpRect: TRect);
  begin
    if iSingle then
      DrawText(ResultForm.Canvas.Handle, lpString, nCount, lpRect,
          DT_CALCRECT or DT_LEFT or DT_SINGLELINE or ResultForm.DrawTextBiDiModeFlagsReadingOnly)
    else
      DrawText(ResultForm.Canvas.Handle, lpString, nCount, lpRect,
          DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or ResultForm.DrawTextBiDiModeFlagsReadingOnly);
  end;

  procedure ResizeResultForm;
  begin
    ResultForm.ClientWidth := Max(TimeoutTextWidth,
                                  Max( MulDiv(17, DialogUnits.X, 4) + ChkTextWidth,
                                      Max(IconWidth+TextWidth, ButtonGroupWidth)))
                              + HorzMargin * 2;
    ResultForm.ClientHeight := Max(IconHeight, TextHeight) + ButtonHeight + VertSpacing * 2 + VertMargin * 2;

    if CheckCaption <> '' then
      ResultForm.ClientHeight := ResultForm.ClientHeight + VertSpacing + MulDiv(17, DialogUnits.Y, 8);
    if ATimeout > 0 then
      ResultForm.ClientHeight := ResultForm.ClientHeight + VertSpacing + MulDiv(13, DialogUnits.Y, 8);

    if ResultForm.ClientWidth > Screen.Width-100 then
      ResultForm.ClientWidth := Screen.Width-100;
    if ResultForm.ClientHeight > Screen.Height-100 then
      ResultForm.ClientHeight := Screen.Height-100;

    ResultForm.Left := (CenterParWidth div 2) - (ResultForm.Width div 2) + CenterParLeft;
    ResultForm.Top := (CenterParHeight div 2) - (ResultForm.Height div 2) + CenterParTop;
  end;

begin
  ResultForm := nil;
  if Assigned(ADynControlEngine) then
    DynControlEngine := ADynControlEngine
  else
    DynControlEngine := DefaultDynControlEngine;
  case Center of
    dckScreen:
      CenterParent := Screen;
    dckMainForm:
      CenterParent := Application.MainForm;
    dckActiveForm:
      CenterParent := Screen.ActiveCustomForm;
  else
    CenterParent := nil;
  end;
  if CenterParent = nil then
    CenterParent := Screen;
  if CenterParent is TScreen then
  begin
    CenterParLeft := 0;
    CenterParTop := 0;
    CenterParWidth := TScreen(CenterParent).Width;
    CenterParHeight := TScreen(CenterParent).Height;
  end
  else
  begin
    CenterParLeft := TWinControl(CenterParent).Left;
    CenterParTop := TWinControl(CenterParent).Top;
    CenterParWidth := TWinControl(CenterParent).Width;
    CenterParHeight := TWinControl(CenterParent).Height;
  end;
  if HelpButton = High(Integer) then
    HelpButton := High(Buttons);
  ResultForm := TDSAMessageForm.CreateNew(Screen.ActiveCustomForm);
  try
    ResultForm.Msg := Msg;
    ResultForm.Position := poDesigned; // Delphi 2005 has a new default
    ResultForm.BiDiMode := Application.BiDiMode;
    ResultForm.BorderStyle := bsDialog;
    ResultForm.Canvas.Font := ResultForm.Font;
    ResultForm.KeyPreview := True;
    ResultForm.HelpContext := HelpCtx;
    ResultForm.OnKeyDown := ResultForm.CustomKeyDown;
    ResultForm.OnShow := ResultForm.CustomShow;
    ResultForm.OnMouseDown := ResultForm.CustomMouseDown;
    DialogUnits := GetAveCharSize(ResultForm.Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    ResultForm.Timeout := Abs(ATimeout);
    for I := Low(Buttons) to High(Buttons) do
    begin
      TextRect := Rect(0, 0, 0, 0);
      CalcTextRect (true, PChar(Buttons[I]), -1, TextRect);
      if (TextRect.Right - TextRect.Left + MulDiv(8, DialogUnits.X, 4)) > ButtonWidth then
        ButtonWidth := (TextRect.Right - TextRect.Left + MulDiv(8, DialogUnits.X, 4));
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);

    if (Screen.Width div 2) > (CenterParWidth + (2 * (CenterParLeft - Screen.DesktopLeft))) then
      SetRect(TextRect, 0, 0, CenterParWidth + (2 * (CenterParLeft - Screen.DesktopLeft)), 0)
    else
      SetRect(TextRect, 0, 0, Screen.Width div 2, 0);


    CalcTextRect (False, PChar(Msg), Length(Msg) + 1, TextRect);
    TextWidth := TextRect.Right;
    TextHeight := TextRect.Bottom;
    if CheckCaption <> '' then
    begin
      SetRect(TempRect, 0, 0, Screen.Width div 2, 0);
      CalcTextRect (False, PChar(CheckCaption), Length(CheckCaption) + 1, TempRect);
      ChkTextWidth := TempRect.Right;
    end
    else
      ChkTextWidth := 0;
    if ATimeout > 0 then
    begin
      SetRect(TempRect, 0, 0, Screen.Width div 2, 0);
      CalcTextRect (False, PChar(TimeFormatter(ResultForm.Timeout)),
        Length(TimeFormatter(ResultForm.Timeout)) + 1, TempRect);
      TimeoutTextWidth := TempRect.Right;
    end
    else
      TimeoutTextWidth := 0;
    if APicture <> nil then
    begin
      IconWidth := APicture.Width + HorzSpacing;
      IconHeight := APicture.Height;
    end
    else
    begin
      IconWidth := 0;
      IconHeight := 0;
    end;

    ButtonCount := Length(Buttons);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount + ButtonSpacing * (ButtonCount - 1);

    ResizeResultForm;

    if ACaption <> '' then
      ResultForm.Caption := ACaption
    else
      ResultForm.Caption := Application.Title;

    BottomPanel := DynControlEngine.CreatePanelControl(ResultForm, ResultForm, 'BottomPanel', '', alBottom);
    BottomPanel.Height := VertSpacing+VertMargin+ButtonHeight;

    MainPanel := DynControlEngine.CreatePanelControl(ResultForm, ResultForm, 'MainPanel', '', alClient);

    CheckPanel := DynControlEngine.CreatePanelControl(ResultForm, MainPanel, 'CheckPanel', '', alBottom);
    CheckPanel.Visible := (CheckCaption <> '') or (ATimeout > 0);

    ImagePanel := DynControlEngine.CreatePanelControl(ResultForm, MainPanel, 'ImagePanel', '', alLeft);
    ImagePanel.Visible := Assigned(APicture);
    if Assigned(APicture) then
    begin
      ImagePanel.Width := APicture.Width + MulDiv(4, DialogUnits.X, 4) + HorzMargin - MulDiv(2, DialogUnits.X, 4);
      Image := DynControlEngine.CreateImageControl(ResultForm, ImagePanel, 'Image');
      if Supports(Image, IJvDynControlImage, DynControlImage) then
      begin
        DynControlImage.ControlSetGraphic(APicture);
        DynControlImage.ControlSetCenter(True);
      end;
      Image.SetBounds(HorzMargin - MulDiv(2, DialogUnits.X, 4), VertMargin -  MulDiv(2, DialogUnits.Y, 8), APicture.Width +  MulDiv(2, DialogUnits.Y, 8), APicture.Height + 2);
      Image.Enabled := False;
    end;

    MessagePanel := DynControlEngine.CreatePanelControl(ResultForm, MainPanel, 'Panel', '', alClient);
    if ImagePanel.Visible then
      MessagePanel.Width := MainPanel.Width-ImagePanel.Width
    else
      MessagePanel.Width := MainPanel.Width;
    MessageLabel := DynControlEngine.CreateLabelControl(ResultForm, MessagePanel, 'Message', Msg, nil);

    if Assigned(APicture) then
      MessageLabel.Left := HorzSpacing
    else
      MessageLabel.Left := HorzMargin;
    MessageLabel.Top := VertMargin;
    MessageLabel.Width := MessagePanel.Width-MessageLabel.Left;
    MessageLabel.Height := MessagePanel.Height - MessageLabel.Top;

    if Supports(MessageLabel, IJvDynControlLabel, DynControlLabel) then
      DynControlLabel.ControlSetWordWrap(True);
    if Supports(MessageLabel, IJvDynControlAutoSize, DynControlAutoSize) then
    begin
      DynControlAutoSize.ControlSetAutoSize(True);
      TextWidth := MessageLabel.Width;
      TextHeight := MessageLabel.Height;
    end;

    MessageLabel.BiDiMode := ResultForm.BiDiMode;

    ResultForm.DefaultButton := nil;
    X := (ResultForm.ClientWidth - ButtonGroupWidth) div 2;
    for I := Low(Buttons) to High(Buttons) do
    begin
      Button := DynControlEngine.CreateButton(ResultForm, BottomPanel, 'Button' + IntToStr(I), Buttons[I], '', nil, False, False);
      Button.ModalResult := Results[I];
      if I = DefaultButton then
      begin
        ResultForm.DefaultButton := Button;
        Button.Default := True;
      end;
      if I = CancelButton then
        Button.Cancel := True;
      Button.TabStop := True;
      Button.SetBounds(X, VertSpacing, ButtonWidth, ButtonHeight);
      Inc(X, ButtonWidth + ButtonSpacing);
      if I = HelpButton then
        Button.OnClick := ResultForm.HelpButtonClick;
    end;
    CheckBox := nil; // to avoid warnings
    if CheckCaption <> '' then
    begin
      CheckBox := DynControlEngine.CreateCheckboxControl(ResultForm, CheckPanel, 'DontShowAgain', CheckCaption);
      CheckBox.BiDiMode := ResultForm.BiDiMode;
      CheckBox.Height := MulDiv(CheckBox.Height, DialogUnits.Y, 8);
      CheckBox.SetBounds(HorzMargin, 0,
        ResultForm.ClientWidth - 2 * HorzMargin, CheckBox.Height);
      CheckPanel.Height := CheckBox.Height;
    end;
    if ATimeout > 0 then
    begin
      CountDownlabel := DynControlEngine.CreateLabelControl(ResultForm, CheckPanel, 'Countdown',
        TimeFormatter(ResultForm.Timeout), nil);

      CountDownlabel.BiDiMode := ResultForm.BiDiMode;
      if CheckCaption <> '' then
      begin
        CheckPanel.Height := CheckBox.Height+CountDownlabel.Height+VertSpacing;;
        CountDownlabel.SetBounds (HorzMargin, CheckBox.Height+VertSpacing,
          ResultForm.ClientWidth - 2 * HorzMargin, CountDownlabel.Height)
      end
      else
      begin
        CheckPanel.Height := CountDownlabel.Height;;
        CountDownlabel.SetBounds(HorzMargin, 0,
          ResultForm.ClientWidth - 2 * HorzMargin, CountDownlabel.Height);
      end;
    end;

    ResizeResultForm;

  except
    FreeAndNil(ResultForm);
    raise;
  end;
  Result := ResultForm;
end;

//=== { TDSARegister } =======================================================

type
  TAddResult = (arAdded, arExists, arDuplicateID, arDuplicateName);

  TDSARegister = class
  private
    FList: array of TDSARegItem;
  protected
    function AddNew: Integer;
    procedure Remove(const Index: Integer);
    function IndexOf(const ID: Integer): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function IndexOf(const Item: TDSARegItem): Integer; overload;
  public
    destructor Destroy; override;
    function Add(const Item: TDSARegItem): TAddResult; overload;
    function Add(const ID: Integer; const Name, Description: string;
      const Storage: TDSAStorage; const CheckTextKind:
      TDSACheckTextKind = ctkShow): TAddResult; overload;
    procedure Clear;
//    procedure Delete(const Item: TDSARegItem); overload;
    procedure Delete(const ID: Integer); overload;
//    procedure Delete(const Name: string); overload;
    function Locate(const ID: Integer): TDSARegItem; overload;
//    function Locate(const Name: string): TDSARegItem; overload;
  end;

const
  EmptyItem: TDSARegItem = (ID: High(Integer); Name: ''; Storage: nil);

var
  GlobalDSARegister: TDSARegister = nil;

function DSARegister: TDSARegister;
begin
  if not Assigned(GlobalDSARegister) then
  begin
    GlobalDSARegister := TDSARegister.Create;
   // register
    RegisterDSACheckMarkText(ctkShow, RsDSActkShowText);
    RegisterDSACheckMarkText(ctkAsk, RsDSActkAskText);
    RegisterDSACheckMarkText(ctkWarn, RsDSActkWarnText);
  end;
  Result := GlobalDSARegister;
end;

destructor TDSARegister.Destroy;
begin
  inherited Destroy;
  Clear;
end;

function TDSARegister.AddNew: Integer;
begin
  Result := Length(FList);
  SetLength(FList, Result + 1);
end;

procedure TDSARegister.Remove(const Index: Integer);
var
  I: Integer;
begin
  for I := Index + 1 to High(FList) do
  begin
    FList[I-1].ID := FList[I].ID;
    FList[I-1].Name := FList[I].Name;
    FList[I-1].Description := FList[I].Description;
    FList[I-1].ChkTextKind := FList[I].ChkTextKind;
    FList[I-1].Storage := FList[I].Storage;
  end;
  SetLength(FList, High(FList));
end;

function TDSARegister.IndexOf(const ID: Integer): Integer;
begin
  Result := High(FList);
  while (Result > -1) and (FList[Result].ID <> ID) do
    Dec(Result);
end;

function TDSARegister.IndexOf(const Name: string): Integer;
begin
  Result := High(FList);
  while (Result > -1) and not AnsiSameText(FList[Result].Name, Name) do
    Dec(Result);
end;

function TDSARegister.IndexOf(const Item: TDSARegItem): Integer;
begin
  Result := IndexOf(Item.ID);
  if (Result > -1) and not AnsiSameText(FList[Result].Name, Item.Name) then
    Result := -1;
end;

function TDSARegister.Add(const Item: TDSARegItem): TAddResult;
var
  Idx: Integer;
begin
  if IndexOf(Item) > -1 then
    Result := arExists
  else
  if IndexOf(Item.ID) > -1 then
  begin
    Idx := IndexOf(Item.ID);
    if AnsiSameText(FList[Idx].Name, Item.Name) then
      Result := arExists
    else
      Result := arDuplicateID;
  end
  else
  if IndexOf(Item.Name) > -1 then
    Result := arDuplicateName
  else
  begin
    Idx := AddNew;
    FList[Idx].ID := Item.ID;
    FList[Idx].Name := Item.Name;
    FList[Idx].Description := Item.Description;
    FList[Idx].Storage := Item.Storage;
    FList[Idx].ChkTextKind := Item.ChkTextKind;
    Result := arAdded;
  end;
end;

function TDSARegister.Add(const ID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow): TAddResult;
var
  TmpItem: TDSARegItem;
begin
  TmpItem.ID := ID;
  TmpItem.Name := Name;
  TmpItem.Description := Description;
  TmpItem.Storage := Storage;
  TmpItem.ChkTextKind := CheckTextKind;
  Result := Add(TmpItem);
end;

procedure TDSARegister.Clear;
begin
  SetLength(FList, 0);
end;

(* make Delphi 5 compiler happy // andreas
procedure TDSARegister.Delete(const Item: TDSARegItem);
var
  Idx: Integer;
begin
  Idx := IndexOf(Item.ID);
  if (Idx > -1) and AnsiSameText(FList[Idx].Name, Item.Name) then
    Remove(Idx);
end;
*)

procedure TDSARegister.Delete(const ID: Integer);
var
  Idx: Integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Remove(Idx);
end;

(* make Delphi 5 compiler happy // andreas
procedure TDSARegister.Delete(const Name: string);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Remove(Idx);
end;
*)

function TDSARegister.Locate(const ID: Integer): TDSARegItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Result := FList[Idx]
  else
    Result := EmptyItem;
end;

(* make Delphi 5 compiler happy // andreas
function TDSARegister.Locate(const Name: string): TDSARegItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Result := FList[Idx]
  else
    Result := EmptyItem;
end;
*)

//=== { TDSAStorage } ========================================================

constructor TDSAStorage.Create;
begin
  inherited Create;
  FStates := TStack.Create;
end;

destructor TDSAStorage.Destroy;
begin
  FStates.Free;
  inherited Create;
end;

procedure TDSAStorage.BeginCustomRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomRead);
end;

procedure TDSAStorage.BeginCustomWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomWrite);
end;

procedure TDSAStorage.BeginRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssRead);
end;

procedure TDSAStorage.BeginWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssWrite);
end;

procedure TDSAStorage.EndCustomRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomRead then
    raise EJvDSADialog.CreateRes(@RsECannotEndCustomReadIfNotInCustomRea);
  FStates.Pop;
end;

procedure TDSAStorage.EndCustomWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomWrite then
    raise EJvDSADialog.CreateRes(@RsECannotEndCustomWriteIfNotInCustomWr);
  FStates.Pop;
end;

procedure TDSAStorage.EndRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssRead then
    raise EJvDSADialog.CreateRes(@RsECannotEndReadIfNotInReadMode);
  FStates.Pop;
end;

procedure TDSAStorage.EndWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssWrite then
    raise EJvDSADialog.CreateRes(@RsECannotEndWriteIfNotInWriteMode);
  FStates.Pop;
end;

function TDSAStorage.IsKeyNameAllowed(const Key: string): Boolean;
begin
  if AnsiSameText(Key, cDSAStateValueName) or AnsiSameText(Key, cDSAStateLastResultName) then
    Result := Integer(FStates.Peek) in [Integer(ssRead), Integer(ssWrite)]
  else
    Result := Integer(FStates.Peek) in [Integer(ssCustomRead), Integer(ssCustomWrite)];
end;

function TDSAStorage.GetState(const DSAInfo: TDSARegItem; out LastResult: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean;
begin
  BeginRead(DSAInfo);
  try
    LastResult := 0;
    Result := ReadBoolDef(DSAInfo, cDSAStateValueName, False);
    if Result then
    begin
      LastResult := ReadIntegerDef(DSAInfo, cDSAStateLastResultName, 0);
      if Assigned(OnCustomData) then
      begin
        BeginCustomRead(DSAInfo);
        try
          OnCustomData(Self, DSAInfo);
        finally
          EndCustomRead(DSAInfo);
        end;
      end;
    end;
  finally
    EndRead(DSAInfo);
  end;
end;

procedure TDSAStorage.SetState(const DSAInfo: TDSARegItem; const DontShowAgain: Boolean;
  const LastResult: Integer; const OnCustomData: TDSACustomData = nil);
begin
  BeginWrite(DSAInfo);
  try
    WriteBool(DSAInfo, cDSAStateValueName, DontShowAgain);
    if DontShowAgain then
    begin
      WriteInteger(DSAInfo, cDSAStateLastResultName, LastResult);
      if Assigned(OnCustomData) then
      begin
        BeginCustomWrite(DSAInfo);
        try
          OnCustomData(Self, DSAInfo);
        finally
          EndCustomWrite(DSAInfo);
        end;
      end;
    end;
  finally
    EndWrite(DSAInfo);
  end;
end;

//=== { TDSARegStorage } =====================================================

{$IFDEF MSWINDOWS}

constructor TDSARegStorage.Create(const ARootKey: HKEY; const AKey: string);
begin
  inherited Create;
  FRootKey := ARootKey;
  FKey := AKey;
end;

procedure TDSARegStorage.CreateKey(const DSAInfo: TDSARegItem);
begin
  if not (RegKeyExists(RootKey, Key + '\' + DSAInfo.Name) or
    (RegCreateKey(RootKey, Key + '\' + DSAInfo.Name, '') = ERROR_SUCCESS)) then
    raise EJvDSADialog.CreateResFmt(@RsEDSARegKeyCreateError, [Key + '\' + DSAInfo.Name]);
end;

function TDSARegStorage.GetCheckMarkTextSuffix: string;
begin
  Result := '';
end;

procedure TDSARegStorage.SetCheckMarkTextSuffix(const Value: string);
begin
end;

function TDSARegStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
begin
  Result := RegReadBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  Result := RegReadBoolDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  RegReadBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(Extended));
end;

function TDSARegStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  if RegReadBinaryDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(Extended), 0) = 0 then
    Result := Default;
end;

function TDSARegStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := RegReadInt64(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64;
begin
  Result := RegReadInt64Def(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := RegReadInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  Result := RegReadIntegerDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := RegReadString(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  Result := RegReadStringDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

procedure TDSARegStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  CreateKey(DSAInfo);
  RegWriteBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
var
  Temp: Extended;
begin
  CreateKey(DSAInfo);
  Temp := Value;
  RegWriteBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Temp, SizeOf(Extended));
end;

procedure TDSARegStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  CreateKey(DSAInfo);
  RegWriteInt64(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  CreateKey(DSAInfo);
  RegWriteInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  CreateKey(DSAInfo);
  RegWriteString(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

{$ENDIF MSWINDOWS}

//=== { TDSAValues } =========================================================

const
  DSABool = 1;
  DSAFloat = 2;
  DSAInt64 = 3;
  DSAInt = 4;
  DSAString = 5;

  DSAKindTexts: array [DSABool..DSAString] of string =
    (RsEDSAAccessBool, RsEDSAAccessFloat, RsEDSAAccessInt64, RsEDSAAccessInt, RsEDSAAccessString);

type
  TDSAValues = class(TStringList)
  public
    constructor Create;
  end;

constructor TDSAValues.Create;
begin
  inherited Create;
  Sorted := True;
end;

//=== { TDSAQueueStorage } ===================================================

constructor TDSAQueueStorage.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
  FCheckMarkSuffix := RsInTheCurrentQueue;
end;

destructor TDSAQueueStorage.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDSAQueueStorage.AddDSA(const DSAInfo: TDSARegItem);
begin
  if FindDSA(DSAInfo) < 0 then
    FList.AddObject(DSAInfo.Name, TDSAValues.Create);
end;

procedure TDSAQueueStorage.DeleteDSA(const Index: Integer);
begin
  FList.Objects[Index].Free;
  FList.Delete(Index);
end;

function TDSAQueueStorage.FindDSA(const DSAInfo: TDSARegItem): Integer;
begin
  Result := FList.IndexOf(DSAInfo.Name);
end;

function TDSAQueueStorage.GetCheckMarkTextSuffix: string;
begin
  Result := FCheckMarkSuffix;
end;

function TDSAQueueStorage.GetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
  const Kind: Integer): string;
var
  I: Integer;
  DSAKeys: TStrings;
begin
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateResFmt(@RsEDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    raise EJvDSADialog.CreateResFmt(@RsEDSAKeyNotFound, [Key]);
  if Integer(DSAKeys.Objects[I]) <> Kind then
    raise EJvDSADialog.CreateResFmt(@RsEDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
  Result := DSAKeys.Values[Key];
end;

function TDSAQueueStorage.HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): Boolean;
var
  I: Integer;
  DSAKeys: TStrings;
begin
  I := FindDSA(DSAInfo);
  Result := I > -1;
  if Result then
  begin
    DSAKeys := TStrings(FList.Objects[I]);
    Result := DSAKeys.IndexOfName(Key) > -1;
  end;
end;

procedure TDSAQueueStorage.SetCheckMarkTextSuffix(const Value: string);
begin
  if Value <> CheckMarkTextSuffix then
    FCheckMarkSuffix := Value;
end;

procedure TDSAQueueStorage.SetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
  const Kind: Integer; const Value: string);
var
  I: Integer;
  DSAKeys: TStrings;
begin
  AddDSA(DSAInfo);
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateResFmt(@RsEDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    DSAKeys.AddObject(Key + '=' + Value, TObject(Kind))
  else
  begin
    if Integer(DSAKeys.Objects[I]) <> Kind then
      raise EJvDSADialog.CreateResFmt(@RsEDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
    DSAKeys.Values[Key] := Value;
  end;
end;

procedure TDSAQueueStorage.Clear;
begin
  while FList.Count > 0 do
    DeleteDSA(FList.Count - 1);
end;

function TDSAQueueStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
var
  S: string;
begin
  S := GetDSAValue(DSAInfo, Key, DSABool);
  Result := AnsiSameText(S, 'True') or AnsiSameText(S, '1');
end;

function TDSAQueueStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadBool(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  Result := StrToFloat(StringReplace(GetDSAValue(DSAInfo, Key, DSAFloat),
    JclFormatSettings.ThousandSeparator, JclFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
end;

function TDSAQueueStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadFloat(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := StrToInt64(GetDSAValue(DSAInfo, Key, DSAInt64));
end;

function TDSAQueueStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Int64): Int64;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInt64(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := StrToInt(GetDSAValue(DSAInfo, Key, DSAInt));
end;

function TDSAQueueStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInteger(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := GetDSAValue(DSAInfo, Key, DSAString);
end;

function TDSAQueueStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadString(DSAInfo, Key)
  else
    Result := Default;
end;

procedure TDSAQueueStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  if Value then
    SetDSAValue(DSAInfo, Key, DSABool, '1')
  else
    SetDSAValue(DSAInfo, Key, DSABool, '0');
end;

procedure TDSAQueueStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
begin
  SetDSAValue(DSAInfo, Key, DSAFloat, FloatToStr(Value));
end;

procedure TDSAQueueStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  SetDSAValue(DSAInfo, Key, DSAInt64, IntToStr(Value));
end;

procedure TDSAQueueStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  SetDSAValue(DSAInfo, Key, DSAInt, IntToStr(Value));
end;

procedure TDSAQueueStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  SetDSAValue(DSAInfo, Key, DSAString, Value);
end;

//--------------------------------------------------------------------------------------------------
// Helpers
//--------------------------------------------------------------------------------------------------

const
  Captions: array [TMsgDlgType] of string =
    (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');
  IconIDs: array [TMsgDlgType] of PChar =
    (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil);

  ButtonCaptions: array [TMsgDlgBtn] of string =
   (SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp{$IFDEF COMPILER12_UP}, SMsgDlgClose{$ENDIF COMPILER12_UP});
  ModalResults: array [TMsgDlgBtn] of Integer =
   (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0{$IFDEF COMPILER12_UP}, mrClose{$ENDIF COMPILER12_UP});

function DlgCaption(const DlgType: TMsgDlgType): string;
begin
  Result := Captions[DlgType];
end;

function DlgPic(const DlgType: TMsgDlgType): TGraphic;
begin
  if IconIDs[DlgType] <> nil then
  begin
    Result := TIcon.Create;
    try
      TIcon(Result).Handle := LoadIcon(0, IconIDs[DlgType]);
    except
      Result.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

function DlgButtonCaptions(const Buttons: TMsgDlgButtons): TDynStringArray;
var
  I: Integer;
  B: TMsgDlgBtn;
begin
  SetLength(Result, Ord(High(TMsgDlgBtn)) + 1);
  I := 0;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if B in Buttons then
    begin
      Result[I] := ButtonCaptions[B];
      Inc(I);
    end;
  SetLength(Result, I);
end;

function DlgButtonResults(const Buttons: TMsgDlgButtons): TDynIntegerArray;
var
  I: Integer;
  B: TMsgDlgBtn;
begin
  SetLength(Result, Ord(High(TMsgDlgBtn)) + 1);
  I := 0;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if B in Buttons then
    begin
      Result[I] := ModalResults[B];
      Inc(I);
    end;
  SetLength(Result, I);
end;

function ButtonIndex(const Results: array of Integer; const ResCode: Integer): Integer; overload;
begin
  Result := High(Results);
  while (Result > -1) and (Results[Result] <> ResCode) do
    Dec(Result);
end;

function ButtonIndex(const Results: array of Integer; const Button: TMsgDlgBtn): Integer; overload;
begin
  Result := ButtonIndex(Results, ModalResults[Button]);
end;

//----------------------------------------------------------------------------
// MessageDlg replacements and extensions
//----------------------------------------------------------------------------

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind; const Timeout: Integer;
  const ADynControlEngine: TJvDynControlEngine);
begin
  MessageDlg(Msg, mtCustom, [mbOK], 0, Center, Timeout);
end;

procedure ShowMessageFmt(const Msg: string; const Params: array of const; const Center: TDlgCenterKind;
  const Timeout: Integer; const ADynControlEngine: TJvDynControlEngine);
begin
  MessageDlg(Format(Msg, Params), mtCustom, [mbOK], 0, Center, Timeout);
end;

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind; const Timeout: Integer;
  const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx, Center, Timeout, DefaultButton,
      CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind; const Timeout: Integer;
  const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(Caption, Msg, TmpPic, Buttons, HelpCtx, Center,
      Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlg(const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind; const Timeout: Integer;
  const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  DefBtn: TMsgDlgBtn;
  CanBtn: TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOK in Buttons then
      DefBtn := mbOK
    else
    if mbYes in Buttons then
      DefBtn := mbYes
    else
      DefBtn := mbRetry;
  end
  else
    DefBtn := DefaultButton;
  if CancelButton = mbDefault then
  begin
    if mbCancel in Buttons then
      CanBtn := mbCancel
    else
    if mbNo in Buttons then
      CanBtn := mbNo
    else
      CanBtn := mbOK;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := MessageDlgEx(Caption, Msg, Picture, DlgButtonCaptions(Buttons),
    BtnResults, HelpCtx, Center, Timeout, ButtonIndex(BtnResults, DefBtn),
    ButtonIndex(BtnResults, CanBtn), ButtonIndex(BtnResults, HelpButton),
    ADynControlEngine);
end;

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string;
  const Results: array of Integer; const HelpCtx: Longint; const Center: TDlgCenterKind;
  const Timeout: Integer; const DefaultButton: Integer; const CancelButton: Integer;
  const HelpButton: Integer; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(DlgCaption(DlgType), Msg, TmpPic, Buttons, Results, HelpCtx, Center, Timeout, DefaultButton,
      CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout: Integer; const DefaultButton: Integer;
  const CancelButton: Integer; const HelpButton: Integer;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(Caption, Msg, TmpPic, Buttons, Results, HelpCtx,
      Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout: Integer; const DefaultButton: Integer;
  const CancelButton: Integer; const HelpButton: Integer;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
begin
  with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx, '',
    Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine) do
  try
    Result := ShowModal;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//----------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: Integer; const Msg: string;
  const Center: TDlgCenterKind; const Timeout: Integer;
  const ADynControlEngine: TJvDynControlEngine);
begin
  DSAMessageDlg(DlgID, Msg, mtCustom, [mbOK], 0, Center, Timeout, mbDefault,
    mbDefault, mbHelp, ADynControlEngine);
end;

procedure DSAShowMessageFmt(const DlgID: Integer; const Msg: string;
  const Params: array of const; const Center: TDlgCenterKind;
  const Timeout: Integer; const ADynControlEngine: TJvDynControlEngine);
begin
  DSAMessageDlg(DlgID, Format(Msg, Params), mtCustom, [mbOK], 0, Center, Timeout,
    mbDefault, mbDefault, mbHelp, ADynControlEngine);
end;

function DSAMessageDlg(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind;
  const Timeout: Integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn;
  const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx,
      Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string;
  const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout: Integer; const DefaultButton: TMsgDlgBtn;
  const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, Caption, Msg, TmpPic, Buttons, HelpCtx, Center,
      Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string;
  const Picture: TGraphic; const Buttons: TMsgDlgButtons; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout: Integer; const DefaultButton: TMsgDlgBtn;
  const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  DefBtn: TMsgDlgBtn;
  CanBtn: TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOK in Buttons then
      DefBtn := mbOK
    else
    if mbYes in Buttons then
      DefBtn := mbYes
    else
      DefBtn := mbRetry;
  end
  else
    DefBtn := DefaultButton;
  if CancelButton = mbDefault then
  begin
    if mbCancel in Buttons then
      CanBtn := mbCancel
    else
    if mbNo in Buttons then
      CanBtn := mbNo
    else
      CanBtn := mbOK;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := DSAMessageDlgEx(DlgID, Caption, Msg, Picture, DlgButtonCaptions(Buttons),
    BtnResults, HelpCtx, Center, Timeout, ButtonIndex(BtnResults, DefBtn),
    ButtonIndex(BtnResults, CanBtn), ButtonIndex(BtnResults, HelpButton), ADynControlEngine);
end;

function DSAMessageDlgEx(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout: Integer; const DefaultButton: Integer;
  const CancelButton: Integer; const HelpButton: Integer;
  const ADynControlEngine: TJvDynControlEngine): Integer;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons,
      Results, HelpCtx, Center, Timeout, DefaultButton, CancelButton,
      HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout, DefaultButton, CancelButton, HelpButton: Integer;
  const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, Caption, Msg, TmpPic, Buttons, Results, HelpCtx,
      Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind; const Timeout, DefaultButton, CancelButton, HelpButton: Integer;
  const ADynControlEngine: TJvDynControlEngine): Integer;
var
  DSAItem: TDSARegItem;
  CheckCaption: string;
  Temp: string;
begin
  if not GetDSAState(DlgID, Result) then
  begin
    Result := High(Integer);
    DSAItem := LocateDSAReg(DlgID);
    CheckCaption := GetCheckMarkText(DSAItem.ChkTextKind);
    if CheckCaption = '' then
      CheckCaption := GetCheckMarkText(ctkShow);
    Temp := DSAItem.Storage.CheckMarkTextSuffix;
    if Temp <> '' then
      CheckCaption := CheckCaption + ' ' + Temp + '.'
    else
      CheckCaption := CheckCaption + '.';
    // Create and show dialog
    with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx, CheckCaption,
      Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine) do
    try
      Result := ShowModal;
      if IsDSAChecked then
        SetDSAState(DlgID, True, Result);
    finally
      Free;
    end;
  end;
end;

//----------------------------------------------------------------------------
// DSA registration
//----------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
begin
  case DSARegister.Add(DlgID, Name, Description, Storage, CheckTextKind) of
    arDuplicateID:
      raise EJvDSADialog.CreateResFmt(@RsEDSADuplicateID, [DlgID]);
    arDuplicateName:
      raise EJvDSADialog.CreateResFmt(@RsEDSADuplicateName, [Name]);
  end;
end;

procedure UnregisterDSA(const DlgID: Integer);
begin
  DSARegister.Delete(DlgID);
end;

function LocateDSAReg(const DlgID: Integer): TDSARegItem;
begin
  Result := DSARegister.Locate(DlgID);
end;

//----------------------------------------------------------------------------
// DSA state setting/retrieving
//----------------------------------------------------------------------------

function GetDSAState(const DlgID: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := GetDSAState(DlgID, Dummy);
end;

function GetDSAState(const DlgID: Integer; out ResCode: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean;
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    Result := RegItem.Storage.GetState(RegItem, ResCode, OnCustomData)
  else
    raise EJvDSADialog.CreateResFmt(@RsEDSADialogIDNotFound, [DlgID]);
end;

procedure SetDSAState(const DlgID: Integer; const DontShowAgain: Boolean;
  const LastResult: Integer = mrNone; const OnCustomData: TDSACustomData = nil);
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    RegItem.Storage.SetState(RegItem, DontShowAgain, LastResult, OnCustomData)
  else
    raise EJvDSADialog.CreateResFmt(@RsEDSADialogIDNotFound, [DlgID]);
end;

//----------------------------------------------------------------------------
// Iterating the DSA registration
//----------------------------------------------------------------------------

function DSACount: Integer;
begin
  Result := Length(DSARegister.FList);
end;

function DSAItem(const Index: Integer): TDSARegItem;
begin
  Result := DSARegister.FList[Index];
end;

//----------------------------------------------------------------------------
// DSA check box text registration
//----------------------------------------------------------------------------

procedure RegisterDSACheckMarkText(const ID: TDSACheckTextKind; const Text: string);
begin
  if CheckMarkTexts.IndexOfObject(TObject(ID)) < 0 then
    CheckMarkTexts.AddObject(Text, TObject(ID))
  else
    raise EJvDSADialog.CreateResFmt(@RsEDSADuplicateCTK_ID, [ID]);
end;

procedure UnregisterDSACheckMarkText(const ID: TDSACheckTextKind);
var
  Idx: Integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    CheckMarkTexts.Delete(Idx);
end;

function GetDSACheckMarkText(const ID: TDSACheckTextKind): string;
begin
  Result := GetCheckMarkText(ID);
end;

//----------------------------------------------------------------------------
// Standard DSA storage devices
//----------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

var
  GlobalRegStore: TDSAStorage = nil;

function DSARegStore: TDSARegStorage;
begin
  if GlobalRegStore = nil then
  begin
    GlobalRegStore :=
      TDSARegStorage.Create(HKEY_CURRENT_USER, 'Software\' + Application.Title + '\DSA');
  end;
  Result := TDSARegStorage(GlobalRegStore);
end;

{$ENDIF MSWINDOWS}

var
  GlobalQueueStore: TDSAStorage = nil;

function DSAQueueStore: TDSAQueueStorage;
begin
  if GlobalQueueStore = nil then
    GlobalQueueStore := TDSAQueueStorage.Create;
  Result := TDSAQueueStorage(GlobalQueueStore);
end;

{ ShowModal patch }

function GetShowModalVMTOffset: Integer;
asm
  MOV EAX, VMTOFFSET TCustomForm.ShowModal
end;

function GetShowModalVMTIndex: Integer; //  Locate the VMT index of ShowModal
begin
  Result := GetShowModalVMTOffset div SizeOf(Pointer);
end;

//=== { TPatchedForm } =======================================================

type
  TShowModalMethod = function: Integer of object; // So we can call the original ShowModal method.

  TPatchedForm = class(TCustomForm) // To replace the orignal ShowModal method.
  public
    function ShowModal: Integer; override;
  end;

function TPatchedForm.ShowModal: Integer;
var
  I: Integer;
  JvDSADialog: TJvDSADialog;
  DSAItem: TDSARegItem;
  CheckCaption: string;
  Temp: string;
  ShowModalMethod: TShowModalMethod;
begin
  // retrieve the TJvDSADialog instance.
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TJvDSADialog) do
    Dec(I);
  if I = -1 then
    raise EJvDSADialog.CreateRes(@RsEJvDSADialogPatchErrorJvDSADialogCom);
  JvDSADialog := Components[I] as TJvDSADialog;

  // Check the DSA state
  if not JvDSADialog.GetDSAStateInternal(Result) then
  begin
    if (JvDSADialog.CheckControl <> nil) and not JvDSADialog.IgnoreDSAChkMrkTxt then
    begin
      // Get DSA checkmark caption
      DSAItem := LocateDSAReg(JvDSADialog.DialogID);
      CheckCaption := GetDSACheckMarkText(DSAItem.ChkTextKind);
      if CheckCaption = '' then
        CheckCaption := GetDSACheckMarkText(ctkShow);
      Temp := DSAItem.Storage.CheckMarkTextSuffix;
      if Temp <> '' then
        CheckCaption := CheckCaption + ' ' + Temp + '.'
      else
        CheckCaption := CheckCaption + '.';
      SetStrProp(JvDSADialog.CheckControl, 'Caption', CheckCaption);
    end;

    { Notify the JvDSADialog component that we are about to show the form (may initialize the
      auto-close timer) }
    JvDSADialog.BeforeShow;
    // Show the dialog by calling the original ShowModal method: setting up the method pointers.
    TMethod(ShowModalMethod).Data := Self;
    TMethod(ShowModalMethod).Code := JvDSADialog.GetOrgShowModalPtr;
    // Show the dialog by calling the original ShowModal method: make the actual call.
    Result := ShowModalMethod;
    { Notify the JvDSADialog component that we the form has closed (may clean up the
      auto-close timer) }
    JvDSADialog.AfterShow;
    // Update the DSA state in storage.
    JvDSADialog.UpdateDSAState;
  end
  else
    // The dialog is suppressed. Apply the saved state.
    JvDSADialog.ApplySavedState;
end;

//=== { TJvDSADialog } =======================================================

constructor TJvDSADialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  if AOwner is TCustomForm then
  begin
    I := AOwner.ComponentCount - 1;
    while (I > -1) and not (AOwner.Components[I] is TJvDSADialog) do
      Dec(I);
    if I > -1 then
      raise EJvDSADialog.CreateRes(@RsEAlreadyDSADialog);
    inherited Create(AOwner);
  end
  else
    raise EJvDSADialog.CreateRes(@RsEOnlyAllowedOnForms);
end;

destructor TJvDSADialog.Destroy;
begin
  FormUnPatch;
  inherited Destroy;
end;

procedure TJvDSADialog.AutoClose;
begin
  CancelCountdown;
  if not DoAutoClose then
    (Owner as TCustomForm).Close;
end;

procedure TJvDSADialog.AfterShow;
begin
  if FTimer <> nil then
    FreeAndNil(FTimer);
end;

procedure TJvDSADialog.ApplySavedState;
var
  ResCode: Integer;
begin
  GetDSAState(DialogID, ResCode, DoApplyKeys);
  TCustomForm(Owner).ModalResult := ResCode;
end;

procedure TJvDSADialog.BeforeShow;
begin
  if FTimeout > 0 then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Enabled := False;
    FTimer.Interval := 1000;
    FTimer.OnTimer := TimerEvent;
    FTimerCount := FTimeout;
  end;
end;

procedure TJvDSADialog.DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem);
begin
  if Assigned(FOnApplyKeys) then
    OnApplyKeys(Self, DSAInfo, Storage);
end;

function TJvDSADialog.DoAutoClose: Boolean;
begin
  Result := False;
  if Assigned(FOnAutoClose) then
    FOnAutoClose(Self, Result);
end;

procedure TJvDSADialog.DoCountDown;
begin
  if Assigned(FOnCountdown) then
    OnCountdown(Self);
end;

procedure TJvDSADialog.DoUpdateKeys;
begin
  if Assigned(FOnUpdateKeys) then
    OnUpdateKeys(Self, DSAInfo, Storage);
end;

function TJvDSADialog.GetDSAStateInternal(out ModalResult: Integer): Boolean;
begin
  Result := GetDSAState(DialogID, ModalResult);
end;

function TJvDSADialog.GetOrgOwner: TComponent;
begin
  Result := FOrgOwner;
end;

function TJvDSADialog.GetOrgShowModalPtr: Pointer;
begin
  Result := FOrgShowModalPtr;
end;

function TJvDSADialog.GetStorage: TDSAStorage;
begin
  Result := LocateDSAReg(DialogID).Storage;
end;

procedure TJvDSADialog.FormPatch;
var
  VMTIdx: Integer;
begin
  VMTIdx := GetShowModalVMTIndex;
  SetOrgShowModalPtr(GetVirtualMethod(Owner.ClassType, VMTIdx));
  SetOrgOwner(Owner);
  SetVirtualMethod(Owner.ClassType, VMTIdx, @TPatchedForm.ShowModal);
end;

procedure TJvDSADialog.FormUnPatch;
var
  VMTIdx: Integer;
begin
  if GetOrgShowModalPtr <> nil then
  begin
    VMTIdx := GetShowModalVMTIndex;
    SetVirtualMethod(GetOrgOwner.ClassType, VMTIdx, GetOrgShowModalPtr);
    SetOrgShowModalPtr(nil);
  end;
end;

procedure TJvDSADialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = CheckControl) then
    CheckControl := nil;
end;

procedure TJvDSADialog.SetCheckControl(Value: TWinControl);
begin
  if ReplaceComponentReference(Self, Value, TComponent(FCheckControl)) then
    if Value <> nil then
    begin
      if GetPropInfo(Value, 'Checked') = nil then
        raise EJvDSADialog.CreateRes(@RsECtrlHasNoCheckedProp);
      if GetPropInfo(Value, 'Caption') = nil then
        raise EJvDSADialog.CreateRes(@RsECtrlHasNoCaptionProp);
    end;
end;

procedure TJvDSADialog.SetDialogID(Value: Integer);
begin
  if Value <> DialogID then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in Owner.ComponentState) then
      raise EJvDSADialog.CreateRes(@RsEDialogIDChangeOnlyInDesign);
    FDialogID := Value;
  end;
end;

procedure TJvDSADialog.SetOrgOwner(Value: TComponent);
begin
  FOrgOwner := Value;
end;

procedure TJvDSADialog.SetOrgShowModalPtr(Value: Pointer);
begin
  FOrgShowModalPtr := Value;
end;

procedure TJvDSADialog.TimerEvent(Sender: TObject);
begin
  Dec(FTimerCount);
  if FTimerCount = 0 then
    AutoClose
  else
    DoCountDown;
end;

procedure TJvDSADialog.UpdateDSAState;
begin
  SetDSAState(DialogID, IsDSAChecked, TCustomForm(Owner).ModalResult, DoUpdateKeys);
end;

function TJvDSADialog.GetModalResult: Integer;
begin
  Result := TCustomForm(Owner).ModalResult;
end;

function TJvDSADialog.IsDSAChecked: Boolean;
begin
  if CheckControl <> nil then
    Result := GetOrdProp(CheckControl, 'Checked') <> 0
  else
    Result := False;
end;

procedure TJvDSADialog.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    FormPatch;
end;

procedure TJvDSADialog.CancelCountdown;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;
end;

function TJvDSADialog.SecondsLeft: Integer;
begin
  if Timeout <> 0 then
    Result := FTimerCount
  else
    Result := 0;
end;

constructor TDSAAppStorage.Create(AAppStorage : TJvCustomAppStorage; const APath : string);
begin
  inherited Create;
  FAppStorage := AAppStorage;
  FAppStoragePath := APath;
end;

function TDSAAppStorage.GetCheckMarkTextSuffix: string;
begin
  Result := '';
end;

procedure TDSAAppStorage.SetCheckMarkTextSuffix(const Value: string);
begin
end;

function TDSAAppStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
begin
  Result := FAppStorage.ReadBoolean(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]));
end;

function TDSAAppStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  Result := FAppStorage.ReadBoolean(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Default);
end;

function TDSAAppStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  Result := FAppStorage.ReadFloat(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]));
end;

function TDSAAppStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  Result := FAppStorage.ReadFloat(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Default);
end;

function TDSAAppStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]));
end;

function TDSAAppStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Default);
end;

function TDSAAppStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]));
end;

function TDSAAppStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Default);
end;

function TDSAAppStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := FAppStorage.ReadString(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]));
end;

function TDSAAppStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  Result := FAppStorage.ReadString(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Default);
end;

procedure TDSAAppStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  FAppStorage.WriteBoolean(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Value);
end;

procedure TDSAAppStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
begin
  FAppStorage.WriteFloat(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Value);
end;

procedure TDSAAppStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  FAppStorage.WriteInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Value);
end;

procedure TDSAAppStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  FAppStorage.WriteInteger(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Value);
end;

procedure TDSAAppStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  FAppStorage.WriteString(Format(sPathStr, [FAppStoragePath, DSAInfo.Name, Key]), Value);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalCheckMarkTexts);
  FreeAndNil(GlobalDSARegister);
  FreeAndNil(GlobalQueueStore);
  {$IFDEF MSWINDOWS}
  FreeAndNil(GlobalRegStore);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
