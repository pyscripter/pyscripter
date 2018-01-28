{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvParameterList;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils, Windows, Messages,
  StdCtrls, ExtCtrls, Graphics, Forms, Controls, Dialogs,
  Variants,
  JvConsts, JvTypes, JvDynControlEngine, JvDynControlEngineIntf,
  JvComponentBase, JvPanel, JvPropertyStore, JvAppStorage, JvAppStorageSelectList;

type
  TJvParameterList = class;
  TJvParameterListPropertyStore = class;
  TJvParameterPropertyValues = class;
  TJvParameterListSelectList = class;
  TJvBaseParameter = class;

  TJvBaseParameterClass = class of TJvBaseParameter;
  TJvParameterListClass = class of TJvParameterList;

  TJvParameterListEvent = procedure(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) of object;
  TJvParameterListAfterParameterWincontrolPropertiesChangedEvent = procedure(const Parameter: TJvBaseParameter; const WinControl:
    TWinControl) of object;

  TJvParameterOnValidateData = procedure (const Data : Variant; var Msg : String; var Valid : Boolean) of Object;

  TJvParameterListEnableDisableReason = class(TPersistent)
  private
    FRemoteParameterName: string;
    FValue: Variant;
    FIsEmpty: Boolean;
    FIsNotEmpty: Boolean;
  protected
    procedure SetAsString(Value: string);
    function GetAsString: string;
    procedure SetAsDouble(Value: Double);
    function GetAsDouble: Double;
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
    procedure SetAsBoolean(Value: Boolean);
    function GetAsBoolean: Boolean;
    procedure SetAsDate(Value: TDateTime);
    function GetAsDate: TDateTime;
    procedure SetAsVariant(Value: Variant);
    function GetAsVariant: Variant;
    procedure SetIsEmpty(Value: Boolean);
    procedure SetIsNotEmpty(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property IsEmpty: Boolean read FIsEmpty write SetIsEmpty;
    property IsNotEmpty: Boolean read FIsNotEmpty write SetIsNotEmpty;
    property RemoteParameterName: string read FRemoteParameterName write FRemoteParameterName;
  end;

  TJvParameterListEnableDisableReasonList = class(TStringList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddReasonVariant(const RemoteParameterName: string; Value: Variant);
    procedure AddReason(const RemoteParameterName: string; Value: Boolean); overload;
    procedure AddReason(const RemoteParameterName: string; Value: Integer); overload;
    procedure AddReason(const RemoteParameterName: string; Value: Double); overload;
    procedure AddReason(const RemoteParameterName: string; const Value: string); overload;
    procedure AddReason(const RemoteParameterName: string; Value: TDateTime); overload;
    procedure AddReasonIsEmpty(const RemoteParameterName: string);
    procedure AddReasonIsNotEmpty(const RemoteParameterName: string);
  end;

  TJvParameterPropertyValue = class(TPersistent)
  private
    FPropertyName: string;
    FPropertyValue: Variant;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
    property PropertyValue: Variant read FPropertyValue write FPropertyValue;
  end;

  TJvParameterPropertyValues = class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure AddValue(const AName: string; AValue: Variant);
  end;

  TJvBaseParameter = class(TJvComponent)
  private
    FCaption: string;
    FValue: Variant;
    FWidth: Integer;
    FHeight: Integer;
    FSearchName: string;
    FReadOnly: Boolean;
    FStoreValueToAppStorage: Boolean;
    FStoreValueCrypted: Boolean;
    FParentParameterName: string;
    FTabOrder: Integer;
    FParameterList: TJvParameterList;
    FWinControl: TWinControl;
    FJvDynControl: IJvDynControl;
    FJvDynControlData: IJvDynControlData;
    FHint: string;
    FTag: Integer;
    FColor: TColor;
    FAdditionalData: Pointer;
    FAfterWincontrolPropertiesChanged: TJvParameterListAfterParameterWincontrolPropertiesChangedEvent;
    FAfterWincontrolPropertiesChangedDisabledCnt: Integer;
    FEnabled: Boolean;
    FHelpContext: THelpContext;
    FDisableReasons: TJvParameterListEnableDisableReasonList;
    FEnableReasons: TJvParameterListEnableDisableReasonList;
    FJvDynControlReadOnly: IJvDynControlReadOnly;
    FJvDynControlCaption: IJvDynControlCaption;
    FVisible: Boolean;
    FOnEnterParameter: TJvParameterListEvent;
    FOnExitParameter: TJvParameterListEvent;
    FOnValidateData: TJvParameterOnValidateData;
    FRequired: Boolean;
    procedure DisableAfterWincontrolPropertiesChanged;
    procedure EnableAfterWincontrolPropertiesChanged;
    procedure HandleAfterWincontrolPropertiesChanged;
    function IsAfterWincontrolPropertiesChangedDisabled: Boolean;
    procedure SetAfterWincontrolPropertiesChangedDisabled(Updating: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetParameterList(const Value: TJvParameterList);
  protected
    procedure SetAsString(const Value: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsDouble(Value: Double); virtual;
    function GetAsDouble: Double; virtual;
    procedure SetAsInteger(Value: Integer); virtual;
    function GetAsInteger: Integer; virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsDate(Value: TDateTime); virtual;
    function GetAsDate: TDateTime; virtual;
    procedure SetAsVariant(Value: Variant); virtual;
    function GetAsVariant: Variant; virtual;
    function GetParameterNameExt: string; virtual;
    function GetParameterNameBase: string;
    function GetParameterName: string;
    procedure SetWinControl(const Value: TWinControl);
    function GetWinControlData: Variant; virtual;
    procedure SetWinControlData(Value: Variant); virtual;
    procedure SetSearchName(Value: string);

    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    function GetHeight: Integer; virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetReadOnly(const Value: Boolean);
    function GetWidth: Integer; virtual;
    procedure SetWidth(Value: Integer); virtual;
    procedure SetTabOrder(Value: Integer); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDynControlEngine: TJvDynControlEngine;
    property Color: TColor read FColor write FColor;
    property JvDynControl: IJvDynControl read FJvDynControl;
    property JvDynControlCaption: IJvDynControlCaption read FJvDynControlCaption;
    property JvDynControlData: IJvDynControlData read FJvDynControlData;
    property JvDynControlReadOnly: IJvDynControlReadOnly read FJvDynControlReadOnly;
    property Value: Variant read FValue write FValue;
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; virtual;
    procedure SetWinControlProperties; virtual;
  public
    constructor Create(AParameterList: TJvParameterList); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; virtual;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); virtual; abstract;
    property WinControlData: Variant read GetWinControlData write SetWinControlData;
    //1 Creates a new instance of the same objecttype and assigns the property contents to the new instance
    function Clone(AOwner: TJvParameterlist): TJvBaseParameter;
    procedure GetData; virtual;
    function IsValid(const AData: Variant): Boolean; virtual;
    procedure SetData; virtual;
    property AdditionalData: Pointer read FAdditionalData write FAdditionalData;
    property ParameterList: TJvParameterList read FParameterList write SetParameterList;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine;
    property WinControl: TWinControl read FWinControl;
  published
    {the next properties implements the possibilities to read and write the AdditionalData }
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    {this name is used to identify the parameter in the parameterlist,
     this value must be defined before inserting into the parameterlist }
    property SearchName: string read FSearchName write SetSearchName;
    {should this value be saved by the parameterlist }
    property StoreValueToAppStorage: Boolean read FStoreValueToAppStorage write FStoreValueToAppStorage;
    {should this value be crypted before save }
    property StoreValueCrypted: Boolean read FStoreValueCrypted write FStoreValueCrypted;
    {the searchname of the parentparameter. The parentparameter must be a
     descent of TJvGroupBoxParameter, TJvPanelParameter or TJvPageControlParameter. If the
     parent parameter is a TJvPageControlParameter, then the ParentParameterName must be
     "searchname.tabname" of the TJvPageControlParameter}
    property ParentParameterName: string read FParentParameterName write FParentParameterName;
    {Is the value required, will be checked in the validate function}
    property Required: Boolean read FRequired write FRequired;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    {the next properties find their expressions in the same properties of TWinControl }
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: string read FHint write FHint;
    property Tag: Integer read FTag write FTag;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property TabOrder: Integer read FTabOrder write SetTabOrder;
    property DisableReasons: TJvParameterListEnableDisableReasonList read FDisableReasons;
    property EnableReasons: TJvParameterListEnableDisableReasonList read FEnableReasons;
    property AfterWincontrolPropertiesChanged: TJvParameterListAfterParameterWincontrolPropertiesChangedEvent read
      FAfterWincontrolPropertiesChanged write FAfterWincontrolPropertiesChanged;
    /// Use this event to implement a custom logic to validate the parameter contents
    property OnValidateData: TJvParameterOnValidateData read FOnValidateData write FOnValidateData;
    property OnEnterParameter: TJvParameterListEvent read FOnEnterParameter write FOnEnterParameter;
    property OnExitParameter: TJvParameterListEvent read FOnExitParameter write FOnExitParameter;
  end;

  TJvParameterListMessages = class(TPersistent)
  private
    FCaption: string;
    FOkButton: string;
    FCancelButton: string;
    FHistoryLoadButton: string;
    FHistorySaveButton: string;
    FHistoryClearButton: string;
    FHistoryLoadCaption: string;
    FHistorySaveCaption: string;
    FHistoryClearCaption: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write FCaption;
    property OkButton: string read FOkButton write FOkButton;
    property CancelButton: string read FCancelButton write FCancelButton;
    property HistoryLoadButton: string read FHistoryLoadButton write FHistoryLoadButton;
    property HistorySaveButton: string read FHistorySaveButton write FHistorySaveButton;
    property HistoryClearButton: string read FHistoryClearButton write FHistoryClearButton;
    property HistoryLoadCaption: string read FHistoryLoadCaption write FHistoryLoadCaption;
    property HistorySaveCaption: string read FHistorySaveCaption write FHistorySaveCaption;
    property HistoryClearCaption: string read FHistoryClearCaption write FHistoryClearCaption;
  end;

  TJvParameterList = class(TJvComponent)
  private
    FHandleParameterEnabledCnt: Integer;
    FMessages: TJvParameterListMessages;
    FIntParameterList: TStringList;
    FArrangeSettings: TJvArrangeSettings;
    FDynControlEngine: TJvDynControlEngine;
    FParameterDialog: TCustomForm;
    FWidth: Integer;
    FHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FDefaultParameterHeight: Integer;
    FDefaultParameterWidth: Integer;
    FDefaultParameterLabelWidth: Integer;
    FOkButtonVisible: Boolean;
    FCancelButtonVisible: Boolean;
    FParameterListPropertyStore: TJvParameterListPropertyStore;
    FHistoryEnabled: Boolean;
    FLastHistoryName: string;
    FParameterListSelectList: TJvParameterListSelectList;
    FOkButtonDisableReasons: TJvParameterListEnableDisableReasonList;
    FOkButtonEnableReasons: TJvParameterListEnableDisableReasonList;
    FOnChangeParameter: TNotifyEvent;
    FOnEnterParameter: TNotifyEvent;
    FOnExitParameter: TNotifyEvent;
    FShowParameterValidState: Boolean;
    function GetIntParameterList: TStrings;
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure DisableHandleParameterEnabled;
    procedure EnableHandleParameterEnabled;
    function GetVisibleCount: Integer;
    function IsHandleParameterEnabledDisabled: Boolean;
    procedure OnOkButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
    procedure ShowParameterDialogThread;
  protected
    OkButton: TButton;
    ArrangePanel: TJvPanel;
    ScrollBox: TScrollBox;
    RightPanel: TJvPanel;
    MainPanel: TWinControl;
    HistoryPanel: TWinControl;
    BottomPanel: TWinControl;
    ButtonPanel: TWinControl;
    OrgButtonPanelWidth: Integer;
    OrgHistoryPanelWidth: Integer;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetAppStoragePath(const Value: string);
    function GetAppStoragePath: string;
    function GetAppStorage: TJvCustomAppStorage;
    procedure SetAppStorage(Value: TJvCustomAppStorage);

    procedure ResizeDialogAfterArrange(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetParentByName(MainParent: TWinControl; const ASearchName: string): TWinControl;
    function GetCount: Integer;

    procedure SetParameters(Index: Integer; const Value: TJvBaseParameter);
    function GetParameters(Index: Integer): TJvBaseParameter;

    function GetCurrentWidth: Integer;
    function GetCurrentHeight: Integer;

    procedure HistoryLoadClick(Sender: TObject);
    procedure HistorySaveClick(Sender: TObject);
    procedure HistoryClearClick(Sender: TObject);
    function GetEnableDisableReasonState(ADisableReasons: TJvParameterListEnableDisableReasonList; AEnableReasons:
        TJvParameterListEnableDisableReasonList): Integer;
    procedure DialogShow(Sender: TObject);
    {this procedure checks the autoscroll-property of the internal
     scrollbox. This function should only be called, after the size of
     the parent-panel has changed}
    procedure CheckScrollBoxAutoScroll;
    property IntParameterList: TStrings read GetIntParameterList;
    property ParameterDialog: TCustomForm read FParameterDialog;
    property ParameterListSelectList: TJvParameterListSelectList read FParameterListSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Saves the data of all allowed parameters to the AppStorage }
    procedure StoreData;
    {Adds a new Parameter to the parameterlist }
    procedure AddParameter(AParameter: TJvBaseParameter);
    {returns the parameter identified by the Searchname}
    function ParameterByName(const ASearchName: string): TJvBaseParameter;
    {returns True id the parameter identified by the Searchname exists}
    function ExistsParameter(const ASearchName: string): Boolean;
    {returns the parameter identified by index-position}
    function ParameterByIndex(AIndex: Integer): TJvBaseParameter;
    {executes a dialog to enter all Parameter-Data,
     returns True when ok-button pressed}
    function ShowParameterDialog: Boolean; overload;
    {executes a dialog to enter all Parameter-Data,
     returns True when ok-button pressed
     This function can be called inside a running thread. It will synchromized
     with the main thread using SynchronizeThread.Synchronize}
    function ShowParameterDialog(SynchronizeThread: TThread): Boolean; overload;
    { Creates the ParameterDialog }
    procedure CreateParameterDialog;
    { Checks the Disable/Enable-Reason of all Parameters }
    procedure HandleParameterEnabled;
    {creates the components of all parameters on any WinControl}
    procedure CreateWinControlsOnParent(ParameterParent: TWinControl);
    {Destroy the WinControls of all parameters}
    procedure DestroyWinControls;
    { reads the data of all parameters from the WinControls}
    procedure GetDataFromWinControls;
    procedure SetDataToWinControls;
    { validates the data of all parameters without filling the data into
     the parameters }
    function ValidateDataAtWinControls: Boolean;
    {deletes all Parameters from the ParameterList}
    procedure Clear;
    { count of parameters }
    property Count: Integer read GetCount;
    {returns the current height of the created main-parameter-panel}
    property CurrentWidth: Integer read GetCurrentWidth;
    {returns the current height of the created main-parameter-panel}
    property CurrentHeight: Integer read GetCurrentHeight;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write FDynControlEngine;
    { Property to get access to the parameters }
    property Parameters[Index: Integer]: TJvBaseParameter read GetParameters write SetParameters; default;
    // Enable/DisableReason for the OkButton
    property OkButtonDisableReasons: TJvParameterListEnableDisableReasonList
        read FOkButtonDisableReasons write FOkButtonDisableReasons;
    property OkButtonEnableReasons: TJvParameterListEnableDisableReasonList
        read FOkButtonEnableReasons write FOkButtonEnableReasons;
    //1 Number of visible parameters
    property VisibleCount: Integer read GetVisibleCount;
    procedure AssignValues(iSourceList: TJvParameterList);
    procedure AssignValuesTo(iDestinationList: TJvParameterList);
    //1 Creates a new instance of the same objecttype and assigns the property contents to the new instance
    function Clone(AOwner: TComponent): TJvParameterList;
    {creates the components of all parameters on any TWInControl}
    procedure CreateWinControlsOnWinControl(ParameterParent: TWinControl);
    {
    Checks the IsDataValid of each Parameter, When the ShowParameterValidStatus is
    activated the labels invalid parameters will be shown italic
    }
    procedure HandleShowValidState;
    function IndexOfParameter(AParameter: TJvBaseParameter): Integer;
    { load the data of all allowed parameters from the AppStorage }
    procedure LoadData;
    { load the data of all allowed parameters from the AppStorage }
    procedure LoadDataFrom(const iTempAppStoragePath: string);
    procedure OnEnterParameterControl(Sender: TObject);
    procedure OnExitParameterControl(Sender: TObject);
    procedure OnChangeParameterControl(Sender: TObject);
    { Saves the data of all allowed parameters to the AppStorage }
    procedure StoreDataTo(const iTempAppStoragePath: string);
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Messages: TJvParameterListMessages read FMessages;
    {AppStoragePath for the Parameter-Storage using AppStorage}
    property AppStoragePath: string read GetAppStoragePath write SetAppStoragePath;
    {Width of the dialog. When width = 0, then the width will be calculated }
    property Width: Integer read FWidth write FWidth;
    {Height of the dialog. When height = 0, then the Height will be calculated }
    property Height: Integer read FHeight write FHeight;
    {Maximum ClientWidth of the Dialog}
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 400;
    {Maximum ClientHeight of the Dialog}
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 600;
    property DefaultParameterHeight: Integer read FDefaultParameterHeight write FDefaultParameterHeight default 0;
    property DefaultParameterWidth: Integer read FDefaultParameterWidth write FDefaultParameterWidth default 0;
    property DefaultParameterLabelWidth: Integer read FDefaultParameterLabelWidth write FDefaultParameterLabelWidth default
        0;
    property OkButtonVisible: Boolean read FOkButtonVisible write FOkButtonVisible;
    property CancelButtonVisible: Boolean read FCancelButtonVisible write FCancelButtonVisible;
    property HistoryEnabled: Boolean read FHistoryEnabled write FHistoryEnabled;
    property LastHistoryName: string read FLastHistoryName write FLastHistoryName;
    property AppStorage: TJvCustomAppStorage read GetAppStorage write SetAppStorage;
    /// Show the state of each invalid parameter by drawing the label italic
    property ShowParameterValidState: Boolean read FShowParameterValidState write FShowParameterValidState default False;
    property OnChangeParameter: TNotifyEvent read FOnChangeParameter write FOnChangeParameter;
    property OnEnterParameter: TNotifyEvent read FOnEnterParameter write FOnEnterParameter;
    property OnExitParameter: TNotifyEvent read FOnExitParameter write FOnExitParameter;
  end;

  TJvParameterListSelectList = class(TJvBaseAppStorageSelectList)
  private
    FParameterList: TJvParameterList;
  protected
    function CreateSelectListDialogInstance(AOwner: TComponent;AOperation: TJvAppStorageSelectListOperation; ACaption:
        string = ''): TJvBaseAppStorageSelectListDialogInstance; override;
    procedure SetParameterList(Value: TJvParameterList); virtual;
    function GetAppStorage: TJvCustomAppStorage; override;
    function GetStoragePath: string; override;
    procedure SetAppStorage(Value: TJvCustomAppStorage); override;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RestoreParameterList(const ACaption: string = '');
    procedure SaveParameterList(const ACaption: string = '');
  published
    property CheckEntries;
    property ParameterList: TJvParameterList read FParameterList write SetParameterList;
    property SelectListDialog;
    property SelectPath;
  end;

  TJvParameterListPropertyStore = class(TJvCustomPropertyStore)
  private
    FParameterList: TJvParameterList;
    procedure SetParameterList(const Value: TJvParameterList);
  protected
    procedure LoadData; override;
    procedure StoreData; override;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ParameterList: TJvParameterList read FParameterList write SetParameterList;
  end;

  {$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
      '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
  {$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  JclStrings,
  JvParameterListParameter, JvResources, JvJVCLUtils, JclSysUtils;

const
  cFalse = 'FALSE';
  cTrue = 'TRUE';
  cAllowedChars: array [0..62] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_');


//=== { Support function for DPI Aware apps } ================================

function PPIScale(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

//=== { TJvParameterListMessages } ===========================================

constructor TJvParameterListMessages.Create;
begin
  inherited Create;
  Caption := RsDialogCaption;
  OkButton := RsButtonOKCaption;
  CancelButton := RsCancelButton;
  HistoryLoadButton := RsHistoryLoadButton;
  HistorySaveButton := RsHistorySaveButton;
  HistoryClearButton := RsHistoryClearButton;
  HistoryLoadCaption := RsHistoryLoadCaption;
  HistorySaveCaption := RsHistorySaveCaption;
  HistoryClearCaption := RsHistoryClearCaption;
end;

procedure TJvParameterListMessages.Assign(Source: TPersistent);
begin
  if Source is TJvParameterListMessages then
  begin
    Caption := TJvParameterListMessages(Source).Caption;
    OkButton := TJvParameterListMessages(Source).OkButton;
    CancelButton := TJvParameterListMessages(Source).CancelButton;
    HistoryLoadButton := TJvParameterListMessages(Source).HistoryLoadButton;
    HistorySaveButton := TJvParameterListMessages(Source).HistorySaveButton;
    HistoryClearButton := TJvParameterListMessages(Source).HistoryClearButton;
    HistoryLoadCaption := TJvParameterListMessages(Source).HistoryLoadCaption;
    HistorySaveCaption := TJvParameterListMessages(Source).HistorySaveCaption;
    HistoryClearCaption := TJvParameterListMessages(Source).HistoryClearCaption;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvParameterListEnableDisableReason } ================================

procedure TJvParameterListEnableDisableReason.SetAsString(Value: string);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsString: string;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsDouble(Value: Double);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsDouble: Double;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsInteger(Value: Integer);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsInteger: Integer;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsBoolean(Value: Boolean);
begin
  if Value then
    AsVariant := cTrue
  else
    AsVariant := cFalse;
end;

function TJvParameterListEnableDisableReason.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := FValue;
  Result := S = cTrue;
end;

procedure TJvParameterListEnableDisableReason.SetAsDate(Value: TDateTime);
begin
  AsVariant := VarFromDateTime(Value);
end;

function TJvParameterListEnableDisableReason.GetAsDate: TDateTime;
begin
  Result := VarToDateTime(FValue);
end;

procedure TJvParameterListEnableDisableReason.SetAsVariant(Value: Variant);
begin
  FValue := Value;
end;

function TJvParameterListEnableDisableReason.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetIsEmpty(Value: Boolean);
begin
  // IsEmpty and NotIsEmtpy can both be False, in this case the Reason looks
  // for the value to activate/deactivate
  FIsEmpty := Value;
  if Value then
    IsNotEmpty := False;
end;

procedure TJvParameterListEnableDisableReason.SetIsNotEmpty(Value: Boolean);
begin
  // IsEmpty and NotIsEmtpy can both be False, in this case the Reason looks
  // for the value to activate/deactivate
  FIsNotEmpty := Value;
  if Value then
    IsEmpty := False;
end;

procedure TJvParameterListEnableDisableReason.Assign(Source: TPersistent);
begin
  if Source is TJvParameterListEnableDisableReason then
  begin
    AsVariant := TJvParameterListEnableDisableReason(Source).AsVariant;
    IsEmpty := TJvParameterListEnableDisableReason(Source).IsEmpty;
    IsNotEmpty := TJvParameterListEnableDisableReason(Source).IsNotEmpty;
    RemoteParameterName := TJvParameterListEnableDisableReason(Source).RemoteParameterName;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvParameterListEnableDisableReasonList } ============================

destructor TJvParameterListEnableDisableReasonList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvParameterListEnableDisableReasonList.Delete(Index: Integer);
begin
  Objects[Index].Free;
  inherited Delete(Index);
end;

procedure TJvParameterListEnableDisableReasonList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonVariant(const RemoteParameterName: string; Value: Variant);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsVariant := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(const RemoteParameterName: string; Value: Boolean);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsBoolean := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(const RemoteParameterName: string; Value: Integer);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsInteger := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(const RemoteParameterName: string; Value: Double);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDouble := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(const RemoteParameterName: string; const Value: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsString := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(const RemoteParameterName: string; Value: TDateTime);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDate := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonIsEmpty(const RemoteParameterName: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.IsEmpty := True;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonIsNotEmpty(const RemoteParameterName: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.IsNotEmpty := True;
  AddObject(RemoteParameterName, Reason);
end;

//=== { TJvParameterPropertyValues } =========================================

constructor TJvParameterPropertyValues.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupIgnore;
end;

destructor TJvParameterPropertyValues.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvParameterPropertyValues.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

procedure TJvParameterPropertyValues.Delete(Index: Integer);
begin
  Objects[Index].Free;
  inherited Delete(Index);
end;

procedure TJvParameterPropertyValues.AddValue(const AName: string; AValue: Variant);
var
  Value: TJvParameterPropertyValue;
begin
  Value := TJvParameterPropertyValue.Create;
  Value.PropertyName := AName;
  Value.PropertyValue := AValue;
  AddObject(AName, Value);
end;

//=== { TJvBaseParameter } ===================================================

constructor TJvBaseParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FStoreValueToAppStorage := True;
  FStoreValueCrypted := False;
  FTabOrder := -1;
  SetParameterList (AParameterList);
  FWinControl := nil;
  FJvDynControl := nil;
  FJvDynControlCaption := nil;
  FJvDynControlData := nil;
  FJvDynControlReadOnly := nil;
  Color := clBtnFace;
  FEnabled := True;
  FVisible := True;
  FEnableReasons := TJvParameterListEnableDisableReasonList.Create;
  FDisableReasons := TJvParameterListEnableDisableReasonList.Create;
  FValue := null;
  FAfterWincontrolPropertiesChangedDisabledCnt := 0;
end;

destructor TJvBaseParameter.Destroy;
begin
  FreeAndNil(FEnableReasons);
  FreeAndNil(FDisableReasons);
  inherited Destroy;
end;

procedure TJvBaseParameter.SetAsString(const Value: string);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsString: string;
begin
  if VarIsNullEmpty(AsVariant) then
    Result := ''
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsDouble(Value: Double);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsDouble: Double;
begin
  if AsString = '' then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsInteger(Value: Integer);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsInteger: Integer;
begin
  if VarIsNullEmpty(AsVariant) then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsBoolean(Value: Boolean);
begin
  if Value then
    AsVariant := cTrue
  else
    AsVariant := cFalse;
end;

function TJvBaseParameter.GetAsBoolean: Boolean;
var
  S: string;
begin
  if VarIsNullEmpty(FValue) then
    Result := False
  else
  begin
    S := AsVariant;
    Result := UpperCase(S) = cTrue;
  end;
end;

procedure TJvBaseParameter.SetAsDate(Value: TDateTime);
begin
  AsVariant := VarFromDateTime(Value);
end;

function TJvBaseParameter.GetAsDate: TDateTime;
begin
  if VarIsNullEmpty(FValue) then
    Result := 0
  else
    Result := VarToDateTime(FValue);
end;

procedure TJvBaseParameter.SetAsVariant(Value: Variant);
begin
  FValue := Value;
end;

function TJvBaseParameter.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TJvBaseParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)  then
    if (AComponent = FWinControl) then
    begin
      FWinControl := nil;
      FJvDynControl := nil;
      FJvDynControlCaption := nil;
      FJvDynControlData := nil;
      FJvDynControlReadOnly := nil;
    end
  else if (AComponent = FParameterList) then
    fParameterList := nil;
end;

function TJvBaseParameter.GetWinControlData: Variant;
begin
  if Assigned(JvDynControlData) then
    Result := JvDynControlData.ControlValue
  else
    Result := Null;
end;

procedure TJvBaseParameter.SetWinControlData(Value: Variant);
begin
  if Assigned(JvDynControlData) then
    try
      JvDynControlData.ControlValue := Value;
    except
      on E: EConvertError do
        ;
      on E: EVariantTypeCastError do
        ;
    end;
end;

procedure TJvBaseParameter.SetSearchName(Value: string);
begin
  FSearchName := Trim(Value);
end;

function TJvBaseParameter.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := nil;
  if Assigned(ParameterList) then
    Result := ParameterList.DynControlEngine;
end;

//type
//  TWinControlAccessProtected = class(TWinControl);

procedure TJvBaseParameter.SetWinControl(const Value: TWinControl);
begin
  FJvDynControl := nil;
  FJvDynControlCaption := nil;
  FJvDynControlData := nil;
  FJvDynControlReadOnly := nil;
  ReplaceComponentReference(Self, Value, TComponent(FWinControl));
  if not Assigned(Value) then
    Exit;
  Supports(FWinControl, IJvDynControl, FJvDynControl);
  Supports(FWinControl, IJvDynControlCaption, FJvDynControlCaption);
  Supports(FWinControl, IJvDynControlData, FJvDynControlData);
  Supports(FWinControl, IJvDynControlReadOnly, FJvDynControlReadOnly);

  DisableAfterWincontrolPropertiesChanged;
  try
    SetWinControlProperties;
  finally
    EnableAfterWincontrolPropertiesChanged;
  end;
  HandleAfterWincontrolPropertiesChanged;
end;

procedure TJvBaseParameter.SetWinControlProperties;
begin
  if Assigned(WinControl) then
  begin
    SetCaption(Caption);
    if Assigned(JvDynControlReadOnly) then
    begin
      JvDynControlReadOnly.ControlSetReadOnly(ReadOnly);
      SetEnabled(FEnabled);
    end
    else
      SetEnabled(FEnabled and not ReadOnly);
    SetVisible(FVisible);
    if FTabOrder >= 0 then
      SetTabOrder(FTabOrder);
    if FWidth > 0 then
      SetWidth(FWidth);
    if FHeight > 0 then
      SetHeight(FHeight);
    WinControl.Hint := Hint;
    WinControl.Tag := Tag;
    WinControl.HelpContext := HelpContext;
    if Assigned(JvDynControl) then
    begin
      JvDynControl.ControlSetOnEnter(ParameterList.OnEnterParameterControl);
      JvDynControl.ControlSetOnExit(ParameterList.OnExitParameterControl);
    end;
    if Assigned(JvDynControlData) then
      JvDynControlData.ControlSetOnChange(ParameterList.OnChangeParameterControl);
  end;
end;

procedure TJvBaseParameter.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  if Assigned(WinControl) then
    WinControl.Enabled := Value;
  HandleAfterWincontrolPropertiesChanged;
end;

procedure TJvBaseParameter.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  if Assigned(WinControl) then
    WinControl.Visible := Value;
  HandleAfterWincontrolPropertiesChanged;
end;

function TJvBaseParameter.GetHeight: Integer;
begin
  if Assigned(ParameterList) and (FHeight <= 0) then
    Result := ParameterList.DefaultParameterHeight
  else
    Result := FHeight;
end;

procedure TJvBaseParameter.SetHeight(Value: Integer);
begin
  FHeight := Value;
  if Assigned(WinControl) then
    WinControl.Height := Value;
end;

function TJvBaseParameter.GetWidth: Integer;
begin
  if Assigned(ParameterList) and (FWidth <= 0) then
    Result := ParameterList.DefaultParameterWidth
  else
    Result := FWidth;
end;

procedure TJvBaseParameter.SetWidth(Value: Integer);
begin
  FWidth := Value;
  if Assigned(WinControl) then
    WinControl.Width := Value;
  HandleAfterWincontrolPropertiesChanged;
end;

procedure TJvBaseParameter.SetTabOrder(Value: Integer);
begin
  FTabOrder := Value;
  if Assigned(WinControl) then
    WinControl.TabOrder := Value;
  HandleAfterWincontrolPropertiesChanged;
end;

procedure TJvBaseParameter.GetData;
begin
  if Assigned(WinControl) then
    AsVariant := WinControlData;
end;

procedure TJvBaseParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := AsVariant;
end;

procedure TJvBaseParameter.Assign(Source: TPersistent);
begin
  if Source is TJvBaseParameter then
  begin
    DisableAfterWincontrolPropertiesChanged;
    try
      AsVariant := TJvBaseParameter(Source).AsVariant;
      Caption := TJvBaseParameter(Source).Caption;
      SearchName := TJvBaseParameter(Source).SearchName;
      Width := TJvBaseParameter(Source).Width;
      Height := TJvBaseParameter(Source).Height;
      Required := TJvBaseParameter(Source).Required;
      ParentParameterName := TJvBaseParameter(Source).ParentParameterName;
      StoreValueToAppStorage := TJvBaseParameter(Source).StoreValueToAppStorage;
      StoreValueCrypted := TJvBaseParameter(Source).StoreValueCrypted;
      TabOrder := TJvBaseParameter(Source).TabOrder;
      FParameterList := TJvBaseParameter(Source).ParameterList;
      Color := TJvBaseParameter(Source).Color;
      ReadOnly := TJvBaseParameter(Source).ReadOnly;
      Enabled := TJvBaseParameter(Source).Enabled;
      FEnableReasons.Assign(TJvBaseParameter(Source).FEnableReasons);
      FDisableReasons.Assign(TJvBaseParameter(Source).FDisableReasons);
    finally
      EnableAfterWincontrolPropertiesChanged;
      HandleAfterWincontrolPropertiesChanged;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvBaseParameter.Clone(AOwner: TJvParameterlist): TJvBaseParameter;
begin
  Result := TJvBaseParameterClass(ClassType).Create(AOwner);
  Result.Assign(Self);
end;

procedure TJvBaseParameter.DisableAfterWincontrolPropertiesChanged;
begin
  Inc(FAfterWincontrolPropertiesChangedDisabledCnt);
  if FAfterWincontrolPropertiesChangedDisabledCnt = 1 then
    SetAfterWincontrolPropertiesChangedDisabled(False);
end;

procedure TJvBaseParameter.EnableAfterWincontrolPropertiesChanged;
begin
  Dec(FAfterWincontrolPropertiesChangedDisabledCnt);
  if FAfterWincontrolPropertiesChangedDisabledCnt = 0 then
    SetAfterWincontrolPropertiesChangedDisabled(True);
end;

function TJvBaseParameter.Validate(var AData: Variant): Boolean;
var Msg : String;
begin
  Result := IsDataValid(AData, Msg);
  if not Result then
    DSADialogsMessageDlg(Msg, mtError, [mbOK], 0);
end;

function TJvBaseParameter.GetParameterNameExt: string;
begin
  Result := '';
end;

function TJvBaseParameter.GetParameterNameBase: string;
begin
  Result := 'ParameterItem' + StrReplaceButChars(SearchName, cAllowedChars, '_');
end;

function TJvBaseParameter.GetParameterName: string;
begin
  Result := GetParameterNameBase + GetParameterNameExt;
end;

procedure TJvBaseParameter.HandleAfterWincontrolPropertiesChanged;
begin
  if Assigned(FAfterWincontrolPropertiesChanged) and not IsAfterWincontrolPropertiesChangedDisabled then
    AfterWincontrolPropertiesChanged(Self, WinControl);
end;

function TJvBaseParameter.IsAfterWincontrolPropertiesChangedDisabled: Boolean;
begin
  Result := FAfterWincontrolPropertiesChangedDisabledCnt > 0;
end;

function TJvBaseParameter.IsValid(const AData: Variant): Boolean;
var Msg : String;
begin
  if Assigned(OnValidateData) then
    OnValidateData(AData, Msg, Result)
  else
    Result := IsDataValid(AData, Msg);
end;

function TJvBaseParameter.IsDataValid(const AData: Variant; var vMsg: String): Boolean;
begin
  if not Required or not Enabled then
    Result := True
  else
    Result := VarToStr(AData) <> '';
  if not Result then
    vMsg := Format(RsErrParameterMustBeEntered, [Caption]);
end;

procedure TJvBaseParameter.SetAfterWincontrolPropertiesChangedDisabled(Updating: Boolean);
begin
end;

procedure TJvBaseParameter.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(JvDynControlCaption) then
    JvDynControlCaption.ControlSetCaption(FCaption);
  HandleAfterWincontrolPropertiesChanged;
end;

procedure TJvBaseParameter.SetParameterList(const Value: TJvParameterList);
begin
  ReplaceComponentReference(Self, Value, TComponent(FParameterList));
end;

procedure TJvBaseParameter.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  if Assigned(WinControl) then
    if Assigned(JvDynControlReadOnly) then
      JvDynControlReadOnly.ControlSetReadOnly(ReadOnly)
    else
      SetEnabled(FEnabled and not ReadOnly);
  HandleAfterWincontrolPropertiesChanged;
end;

//=== { TJvParameterList } ===================================================

constructor TJvParameterList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessages := TJvParameterListMessages.Create;
  FParameterListPropertyStore := TJvParameterListPropertyStore.Create(nil);
  FParameterListPropertyStore.ParameterList := Self;
  FIntParameterList := TStringList.Create;
  FDynControlEngine := DefaultDynControlEngine;
  FArrangeSettings := TJvArrangeSettings.Create(Self);
  FArrangeSettings.AutoArrange := True;
  FArrangeSettings.WrapControls := True;
  FArrangeSettings.AutoSize := asBoth;
  FArrangeSettings.DistanceVertical := PPIScale(3);
  FArrangeSettings.DistanceHorizontal := PPIScale(3);
  FArrangeSettings.BorderLeft := PPIScale(5);
  FArrangeSettings.BorderTop := PPIScale(5);
  ScrollBox := nil;
  RightPanel := nil;
  ArrangePanel := nil;
  FMaxWidth := PPIScale(600);
  FMaxHeight := PPIScale(400);
  FDefaultParameterHeight := 0;
  FDefaultParameterWidth := 0;
  FDefaultParameterLabelWidth := 0;
  FOkButtonVisible := True;
  FCancelButtonVisible := True;
  FHistoryEnabled := False;
  FLastHistoryName := '';
  FParameterListSelectList := TJvParameterListSelectList.Create(Self);
  FParameterListSelectList.ParameterList := Self;
  FOkButtonDisableReasons := TJvParameterListEnableDisableReasonList.Create;
  FOkButtonEnableReasons := TJvParameterListEnableDisableReasonList.Create;
  FShowParameterValidState := False;
  FHandleParameterEnabledCnt := 0;
end;

destructor TJvParameterList.Destroy;
begin
  DestroyWinControls;
  FreeAndNil(FParameterListSelectList);
  FreeAndNil(FIntParameterList);
  FreeAndNil(FParameterListPropertyStore);
  FreeAndNil(FArrangeSettings);
  FreeAndNil(FMessages);
  FreeAndNil(FOkButtonDisableReasons);
  FreeAndNil(FOkButtonEnableReasons);
  inherited Destroy;
end;

function TJvParameterList.GetIntParameterList: TStrings;
begin
  Result := FIntParameterList;
end;

procedure TJvParameterList.AddParameter(AParameter: TJvBaseParameter);
begin
  AddObject(AParameter.SearchName, AParameter);
end;

function TJvParameterList.ExistsParameter(const ASearchName: string): Boolean;
begin
  Result := Assigned(ParameterByName(ASearchName));
end;

function TJvParameterList.ParameterByName(const ASearchName: string): TJvBaseParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if UpperCase(Parameters[I].SearchName) = UpperCase(Trim(ASearchName)) then
    begin
      Result := Parameters[I];
      Break;
    end;
end;

function TJvParameterList.ParameterByIndex(AIndex: Integer): TJvBaseParameter;
begin
  Result := Parameters[AIndex];
end;

procedure TJvParameterList.Assign(Source: TPersistent);
begin
  if Source is TJvParameterList then
  begin
    Messages.Assign(TJvParameterList(Source).Messages);
    ArrangeSettings := TJvParameterList(Source).ArrangeSettings;
    AppStorage := TJvParameterList(Source).AppStorage;
    Width := TJvParameterList(Source).Width;
    Height := TJvParameterList(Source).Height;
    MaxWidth := TJvParameterList(Source).MaxWidth;
    MaxHeight := TJvParameterList(Source).MaxHeight;
    OkButtonVisible := TJvParameterList(Source).OkButtonVisible;
    CancelButtonVisible := TJvParameterList(Source).CancelButtonVisible;
    FIntParameterList.Assign(TJvParameterList(Source).IntParameterList);
    HistoryEnabled := TJvParameterList(Source).HistoryEnabled;
    AppStoragePath := TJvParameterList(Source).AppStoragePath;
    ShowParameterValidState := TJvParameterList(Source).ShowParameterValidState;
  end
  else
    inherited Assign(Source);
end;

procedure TJvParameterList.SetAppStoragePath(const Value: string);
begin
  FParameterListPropertyStore.AppStoragePath := Value;
  if Assigned(AppStorage) then
    FParameterListSelectList.SelectPath := AppStorage.ConcatPaths([Value, RsHistorySelectPath])
end;

function TJvParameterList.GetAppStoragePath: string;
begin
  Result := FParameterListPropertyStore.AppStoragePath;
end;

function TJvParameterList.GetAppStorage: TJvCustomAppStorage;
begin
  Result := FParameterListPropertyStore.AppStorage;
end;

procedure TJvParameterList.SetAppStorage(Value: TJvCustomAppStorage);
begin
  FParameterListPropertyStore.AppStorage := Value;
  if Assigned(Value) then
    FParameterListSelectList.SelectPath :=
      Value.ConcatPaths([FParameterListPropertyStore.AppStoragePath, RsHistorySelectPath])
end;

procedure TJvParameterList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ScrollBox then
      ScrollBox := nil;
    if AComponent = RightPanel then
      RightPanel := nil;
    if AComponent = ArrangePanel then
      ArrangePanel := nil;
    if AComponent = FParameterListPropertyStore then
      FParameterListPropertyStore := nil;
    if AComponent = OkButton then
      OkButton := nil;
  end;
end;

procedure TJvParameterList.LoadData;
begin
  if (AppStoragePath <> '') and Assigned(AppStorage) then
    FParameterListPropertyStore.LoadData;
end;

procedure TJvParameterList.LoadDataFrom(const iTempAppStoragePath: string);
var SaveAppStoragePath : string;
begin
  if (iTempAppStoragePath <> '') and Assigned(AppStorage) then
  begin
    try
      SaveAppStoragePath := AppStoragePath;
      AppStoragePath := iTempAppStoragePath;
      FParameterListPropertyStore.LoadData;
    finally
      AppStoragePath := SaveAppStoragePath;
    end;
  end;
end;

procedure TJvParameterList.StoreData;
begin
  if (AppStoragePath <> '') and Assigned(AppStorage) then
    FParameterListPropertyStore.StoreData;
end;

procedure TJvParameterList.StoreDataTo(const iTempAppStoragePath: string);
var SaveAppStoragePath : string;
begin
  if (iTempAppStoragePath <> '') and Assigned(AppStorage) then
  begin
    try
      SaveAppStoragePath := AppStoragePath;
      AppStoragePath := iTempAppStoragePath;
      FParameterListPropertyStore.StoreData;
    finally
      AppStoragePath := SaveAppStoragePath;
    end;
  end;
end;

procedure TJvParameterList.OnOkButtonClick(Sender: TObject);
begin
  if ValidateDataAtWinControls then
    ParameterDialog.ModalResult := mrOk;
end;

procedure TJvParameterList.OnCancelButtonClick(Sender: TObject);
begin
  ParameterDialog.ModalResult := mrCancel;
end;

procedure TJvParameterList.OnEnterParameterControl(Sender: TObject);
var
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(Sender) then
    for I := 0 to Count - 1 do
      if Parameters[I].WinControl = Sender then
      begin
        if Assigned(Parameters[I].OnEnterParameter) then
          Parameters[I].OnEnterParameter(Self, Parameters[I]);
        if Assigned(OnEnterParameter) then
          OnEnterParameter(Parameters[I]);
        Break;
      end;
end;

procedure TJvParameterList.OnExitParameterControl(Sender: TObject);
var
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(Sender) then
    for I := 0 to Count - 1 do
      if Parameters[I].WinControl = Sender then
      begin
        if Assigned(Parameters[I].OnExitParameter) then
          Parameters[I].OnExitParameter(Self, Parameters[I]);
        if Assigned(OnExitParameter) then
          OnExitParameter(Parameters[I]);
        Break;
      end;
  HandleParameterEnabled;
  HandleShowValidState;
end;

procedure TJvParameterList.OnChangeParameterControl(Sender: TObject);
var
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if Assigned(Sender) then
    for I := 0 to Count - 1 do
      if Parameters[I].WinControl = Sender then
      begin
        if Assigned(OnChangeParameter) then
          OnChangeParameter(Parameters[I]);
        Break;
      end;
  HandleParameterEnabled;
  HandleShowValidState;
end;

type
  TCustomControlAccessProtected = class(TCustomControl);

procedure TJvParameterList.CreateParameterDialog;
var
  CancelButton: TWinControl;
  LoadButton, SaveButton, ClearButton: TWinControl;
  ButtonLeft: Integer;
  ITmpPanel: IJvDynControlPanel;
begin
  FreeAndNil(FParameterDialog);

  FParameterDialog := DynControlEngine.CreateForm(Messages.Caption, '');

  TForm(ParameterDialog).BorderIcons := [];
  TForm(ParameterDialog).DefaultMonitor := dmActiveForm;
  TForm(ParameterDialog).BorderStyle := bsDialog;
  TForm(ParameterDialog).FormStyle := fsNormal;
  {$IFDEF COMPILER7_UP}
  TForm(ParameterDialog).Position := poOwnerFormCenter;
  {$ELSE}
  TForm(ParameterDialog).Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  TForm(ParameterDialog).ShowHint := True;
  TForm(ParameterDialog).OnShow := DialogShow;

  if Height > 0 then
    ParameterDialog.Height := Height;
  if Width > 0 then
    ParameterDialog.Width := Width;

  BottomPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'BottomPanel', '', alBottom);
  if not Supports(BottomPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvRaised, 1, bsNone, 0);

  MainPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvRaised, 1, bsNone, PPIScale(3));

  ButtonPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'BottonPanel', '',
    alRight);
  if not Supports(ButtonPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton', Messages.OkButton, '',
    OnOkButtonClick, True, False);
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton',
    Messages.CancelButton, '',
    OnCancelButtonClick, False, True);

  BottomPanel.Height := PPIScale(22) + PPIScale(6 + 2);

  OkButton.Top := PPIScale(3);
  OkButton.Left := PPIScale(3);
  OkButton.Visible := OkButtonVisible;
  OkButton.Enabled := OkButtonVisible;
  OkButton.Height := PPIScale(22);
  if OkButton.Visible then
    ButtonLeft := OkButton.Left + OkButton.Width + PPIScale(3)
  else
    ButtonLeft := 0;

  CancelButton.Top := PPIScale(3);
  CancelButton.Left := ButtonLeft + PPIScale(3);
  CancelButton.Visible := CancelButtonVisible;
  CancelButton.Enabled := CancelButtonVisible;
  CancelButton.Height := PPIScale(22);
  if CancelButton.Visible then
    ButtonLeft := ButtonLeft + PPIScale(3) + CancelButton.Width + PPIScale(3);

  ButtonPanel.Width := ButtonLeft + PPIScale(3);
  OrgButtonPanelWidth := ButtonLeft + PPIScale(3);

  OkButton.Anchors := [akTop, akRight];
  CancelButton.Anchors := [akTop, akRight];

  if HistoryEnabled and (AppStoragePath <> '') then
  begin
    HistoryPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'HistoryPanel', '',
      alLeft);
    if not Supports(HistoryPanel, IJvDynControlPanel, ITmpPanel) then
      raise EIntfCastError.CreateRes(@RsEIntfCastError);
    ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);
    HistoryPanel.Height := PPIScale(25);
    LoadButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'LoadButton',
      Messages.HistoryLoadButton, '',
      HistoryLoadClick, False, False);
    LoadButton.Left := PPIScale(6);
    LoadButton.Top := PPIScale(5);
    LoadButton.Height := PPIScale(20);
    LoadButton.Width :=
        TCustomControlAccessProtected(HistoryPanel).Canvas.TextWidth(Messages.HistoryLoadButton) + 5;
    ButtonLeft := LoadButton.Left + LoadButton.Width + 5;
    SaveButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'SaveButton',
    Messages.HistorySaveButton, '',
    HistorySaveClick, False, False);
    SaveButton.Left := ButtonLeft;
    SaveButton.Top := PPIScale(5);
    SaveButton.Height := PPIScale(20);
    SaveButton.Width :=
      TCustomControlAccessProtected(HistoryPanel).Canvas.TextWidth(Messages.HistorySaveButton) + 5;
    ButtonLeft := SaveButton.Left + SaveButton.Width + 5;
    ClearButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'ClearButton',
      Messages.HistoryClearButton, '',
      HistoryClearClick, False, False);
    ClearButton.Left := ButtonLeft;
    ClearButton.Top := PPIScale(5);
    ClearButton.Height := PPIScale(20);
    ClearButton.Width :=
      TCustomControlAccessProtected(HistoryPanel).Canvas.TextWidth(Messages.HistoryClearButton) +
      PPIScale(5);
    ButtonLeft := ClearButton.Left + ClearButton.Width + PPIScale(5);
    HistoryPanel.Width := ButtonLeft;
    OrgHistoryPanelWidth := ButtonLeft;
  end
  else
    HistoryPanel := nil;

//  if Screen.PixelsPerInch <> 96 then begin
//    ParameterDialog.ScaleBy(Screen.PixelsPerInch, 96);
//    ParameterDialog.Font := Application.DefaultFont;
//  end;

  CreateWinControlsOnParent(MainPanel);

  ResizeDialogAfterArrange(nil, MainPanel.Left, MainPanel.Top, MainPanel.Width, MainPanel.Height);
end;

procedure TJvParameterList.ResizeDialogAfterArrange(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);
begin
  if Assigned(ParameterDialog) then
  begin
    if (Width <= 0) or (ArrangeSettings.AutoSize in [asWidth, asBoth]) then
      if ArrangePanel.Width > TForm(ParameterDialog).ClientWidth then
        if ArrangePanel.Width > MaxWidth then
          TForm(ParameterDialog).ClientWidth := MaxWidth
        else
          TForm(ParameterDialog).ClientWidth := ArrangePanel.Width + PPIScale(5)
      else
        TForm(ParameterDialog).ClientWidth := ArrangePanel.Width + PPIScale(5);
    if Assigned(HistoryPanel) and
      (TForm(ParameterDialog).ClientWidth < HistoryPanel.Width) then
      TForm(ParameterDialog).ClientWidth := HistoryPanel.Width
    else if TForm(ParameterDialog).ClientWidth < ButtonPanel.Width then
      TForm(ParameterDialog).ClientWidth := ButtonPanel.Width;
    if (Height <= 0) or (ArrangeSettings.AutoSize in [asHeight, asBoth]) then
      if ArrangePanel.Height + BottomPanel.Height > TForm(ParameterDialog).ClientHeight then
        if ArrangePanel.Height + BottomPanel.Height > MaxHeight then
          TForm(ParameterDialog).ClientHeight := MaxHeight + 10
        else
          TForm(ParameterDialog).ClientHeight := ArrangePanel.Height + BottomPanel.Height + PPIScale(10)
      else
        TForm(ParameterDialog).ClientHeight := ArrangePanel.Height + BottomPanel.Height + PPIScale(10);
  end;

  if Assigned(HistoryPanel) then
    if (OrgButtonPanelWidth + OrgHistoryPanelWidth) > BottomPanel.Width then
    begin
      ButtonPanel.Align := alBottom;
      ButtonPanel.Height := OkButton.Height + PPIScale(6 + 2);
      BottomPanel.Height := ButtonPanel.Height * 2 + PPIScale(1);
      HistoryPanel.Align := alClient;
    end
    else
    begin
      ButtonPanel.Align := alRight;
      ButtonPanel.Width := OrgButtonPanelWidth;
      HistoryPanel.Align := alLeft;
      HistoryPanel.Width := OrgHistoryPanelWidth;
      BottomPanel.Height := OkButton.Height + PPIScale(6 + 2);
    end;
  CheckScrollBoxAutoScroll;
end;

procedure TJvParameterList.CheckScrollBoxAutoScroll;
begin
  if not Assigned(ScrollBox) then
    Exit;
  if not Assigned(ArrangePanel) then
    Exit;
  RightPanel.Visible := False;
  ScrollBox.AutoScroll := False;
  if Assigned(ParameterDialog) then
    if (ArrangePanel.Width >= (TForm(ParameterDialog).ClientWidth)) or
      (ArrangePanel.Height > (TForm(ParameterDialog).ClientHeight - BottomPanel.Height)) then
    begin
      RightPanel.Visible := True;
      TForm(ParameterDialog).ClientWidth := TForm(ParameterDialog).ClientWidth + RightPanel.Width +
        4;
      ScrollBox.AutoScroll := True;
    end;
end;

function TJvParameterList.ShowParameterDialog: Boolean;
begin
  if Count = 0 then
    EJVCLException.CreateRes(@RsENoParametersDefined);
  CreateParameterDialog;
  try
    SetDataToWinControls;
    ParameterDialog.ShowModal;
    Result := ParameterDialog.ModalResult = mrOk;
    if Result then
      GetDataFromWinControls;
  finally
    FreeAndNil(FParameterDialog);
  end;
end;

procedure TJvParameterList.ShowParameterDialogThread;
begin
  ParameterDialog.ShowModal;
end;

type
  TAccessThread = class(TThread);

function TJvParameterList.ShowParameterDialog(SynchronizeThread: TThread): Boolean;
begin
  if Count = 0 then
    EJVCLException.CreateRes(@RsENoParametersDefined);
  CreateParameterDialog;
  try
    SetDataToWinControls;
    if Assigned(SynchronizeThread) then
      TAccessThread(SynchronizeThread).Synchronize(ShowParameterDialogThread)
    else
      ParameterDialog.ShowModal;
    Result := ParameterDialog.ModalResult = mrOk;
    if Result then
      GetDataFromWinControls;
  finally
    FreeAndNil(FParameterDialog);
  end;
end;

function TJvParameterList.GetParentByName(MainParent: TWinControl; const ASearchName: string): TWinControl;
var
  Parameter: TJvBaseParameter;
  I: Integer;
  J: Integer;
begin
  Result := MainParent;
  if (Trim(ASearchName) = '') or not Assigned(MainParent) then
    Exit;
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      if UpperCase(Parameters[I].SearchName) = UpperCase(Trim(ASearchName)) then
      begin
        Parameter := Parameters[I];
        if Parameter is TJvArrangeParameter then
        begin
          Result := TJvArrangeParameter(Parameter).ParentControl;
          Break;
        end;
      end
      else
        if Parameters[I] is TJvPageControlParameter then
          for J := 0 to TJvPageControlParameter(Parameters[I]).Pages.Count - 1 do
             if Uppercase(Parameters[I].SearchName + '.' + TJvPageControlParameter(Parameters[I]).Pages[J]) = UpperCase(Trim(ASearchName)) then
             begin
               Result := TJvPageControlParameter(Parameters[I]).PageWinControl(J);
               break;
             end
end;

procedure TJvParameterList.HistoryLoadClick(Sender: TObject);
begin
  ParameterListSelectList.RestoreParameterList(Messages.HistoryLoadCaption);
end;

procedure TJvParameterList.HistorySaveClick(Sender: TObject);
begin
  ParameterListSelectList.SaveParameterList(Messages.HistorySaveCaption);
end;

procedure TJvParameterList.HistoryClearClick(Sender: TObject);
begin
  ParameterListSelectList.ManageSelectList(Messages.HistoryClearCaption);
end;

procedure TJvParameterList.DialogShow(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      if Assigned(Parameters[I].WinControl) then
        if not Parameters[i].ReadOnly and
           Parameters[I].WinControl.CanFocus then
        begin
          Parameters[I].WinControl.SetFocus;
          Break;
        end;
end;

function TJvParameterList.GetEnableDisableReasonState(ADisableReasons: TJvParameterListEnableDisableReasonList;
    AEnableReasons: TJvParameterListEnableDisableReasonList): Integer;
var
  J: Integer;
  IEnable: Integer;
  Reason: TJvParameterListEnableDisableReason;
  SearchParameter: TJvBaseParameter;
  Data: Variant;
begin
  IEnable := 0;
  if AEnableReasons.Count > 0 then
  begin
    for J := 0 to AEnableReasons.Count - 1 do
    begin
      Reason := TJvParameterListEnableDisableReason(AEnableReasons.Objects[J]);
      if not Assigned(Reason) then
        Continue;
      if VarIsNullEmpty(Reason.AsVariant) then
        Continue;
      SearchParameter := ParameterByName(Reason.RemoteParameterName);
      if not Assigned(SearchParameter) then
        Continue;
      if not Assigned(SearchParameter.WinControl) then
        Continue;
      Data := SearchParameter.GetWinControlData;
      if VarIsEmpty(Data) and Reason.IsEmpty and (IEnable <> -1) then
        IEnable := 1;
      if (not VarIsEmpty(Data)) and Reason.IsNotEmpty and (IEnable <> -1) then
        IEnable := 1;
      try
        if (VarCompareValue(Reason.AsVariant, Data) = vrEqual) and (IEnable <> -1) then
          IEnable := 1;
      except
      end;
    end;
    if IEnable = 0 then
      IEnable := -1;
  end;
  if ADisableReasons.Count > 0 then
  begin
    for J := 0 to ADisableReasons.Count - 1 do
    begin
      Reason := TJvParameterListEnableDisableReason(ADisableReasons.Objects[J]);
      if not Assigned(Reason) then
        Continue;
      if VarIsNullEmpty(Reason.AsVariant) then
        Continue;
      SearchParameter := ParameterByName(Reason.RemoteParameterName);
      if not Assigned(SearchParameter) then
        Continue;
      if not Assigned(SearchParameter.WinControl) then
        Continue;
      Data := SearchParameter.GetWinControlData;
      if VarIsNullEmptyBlank(Data) and Reason.IsEmpty then
        IEnable := -1;
      if not VarIsNullEmptyBlank(Data) and Reason.IsNotEmpty then
        IEnable := -1;
      try
        if VarCompareValue(Reason.AsVariant, Data) = vrEqual then
          IEnable := -1;
      except
      end;
    end;
    if IEnable = 0 then
      IEnable := 1;
  end;
  Result := IEnable;
end;

procedure TJvParameterList.HandleParameterEnabled;
var
  I: Integer;
  Parameter: TJvBaseParameter;
  IEnable: Integer;
begin
  if IsHandleParameterEnabledDisabled then
    Exit;
  try
    DisableHandleParameterEnabled;
    for I := 0 to Count - 1 do
    begin
      Parameter := ParameterByIndex(I);
      if Assigned(Parameter.WinControl) then
      begin
        IEnable := GetEnableDisableReasonState(Parameter.DisableReasons, Parameter.EnableReasons);
        case IEnable of
          -1:
            Parameter.Enabled := False;
          1:
            Parameter.Enabled := True;
        end;
      end;
    end;
  finally
    EnableHandleParameterEnabled;
  end;
end;

procedure TJvParameterList.CreateWinControlsOnParent(ParameterParent: TWinControl);
begin
  FreeAndNil(ScrollBox);
  ScrollBox := TScrollBox.Create(Self);
  ScrollBox.Parent := ParameterParent;
  ScrollBox.Name := 'ParameterList_Scrollbox';
  ScrollBox.AutoScroll := False;
  ScrollBox.BorderStyle := bsNone;
  {$IFDEF COMPILER10_UP}
  ScrollBox.ParentBackground := True;
  {$ENDIF COMPILER10_UP}
  ScrollBox.BevelInner := bvNone;
  ScrollBox.BevelOuter := bvNone;
  ScrollBox.Align := alClient;
  ScrollBox.Width := ParameterParent.Width;
  RightPanel := TJvPanel.Create(Self);
  RightPanel.Name := 'ParameterList_RightPanel';
  RightPanel.Parent := ScrollBox;
  RightPanel.Align := alRight;
  RightPanel.BorderStyle := bsNone;
  RightPanel.BevelInner := bvNone;
  RightPanel.BevelOuter := bvNone;
  RightPanel.Width := 22; // asn: need to check this
  RightPanel.Visible := False;
  FreeAndNil(ArrangePanel);
  ArrangePanel := TJvPanel.Create(Self);
  ArrangePanel.Parent := ScrollBox;
  ArrangePanel.Name := 'ParameterList_MainArrangePanel';
  ArrangePanel.Align := alNone;
  ArrangePanel.BorderStyle := bsNone;
  ArrangePanel.BevelInner := bvNone;
  ArrangePanel.BevelOuter := bvNone;
  ArrangePanel.Caption := '';
  ArrangePanel.Left := 0;
  ArrangePanel.Top := 0;
  ArrangePanel.OnResizeParent := ResizeDialogAfterArrange;
  ArrangePanel.ArrangeSettings := ArrangeSettings;
  case ArrangePanel.ArrangeSettings.AutoSize of
    asNone:
      ArrangePanel.ArrangeSettings.AutoSize := asHeight;
    asWidth:
      ArrangePanel.ArrangeSettings.AutoSize := asBoth;
  end;
  if (Width > 0) and (ArrangePanel.ArrangeSettings.AutoSize = asHeight) then
    ArrangePanel.Width := ScrollBox.Width - RightPanel.Width;
  if MaxWidth > 0 then
    ArrangePanel.ArrangeSettings.MaxWidth := MaxWidth - RightPanel.Width - 2;
  CreateWinControlsOnWinControl(ArrangePanel);
end;

procedure TJvParameterList.CreateWinControlsOnWinControl(ParameterParent: TWinControl);
var
  I: Integer;
  BeforeAfterParameterNames : TStringList;
begin
  if ParameterParent is TJvCustomArrangePanel then
    TJvCustomArrangePanel(ParameterParent).DisableArrange;
  BeforeAfterParameterNames := TStringList.Create;
  BeforeAfterParameterNames.Sorted := True;
  BeforeAfterParameterNames.Duplicates := dupError;
  try
    DisableHandleParameterEnabled;
    for I := 0 to Count - 1 do
      if (Parameters[I] is TJvBasePanelEditParameter) then
      begin
        try
          if TJvBasePanelEditParameter(Parameters[I]).BeforeParameterName <> '' then
            BeforeAfterParameterNames.Add(TJvBasePanelEditParameter(Parameters[I]).BeforeParameterName);
        except
          on e:exception do
            raise Exception.CreateResFmt(@RsECreateWinControlsOnWinControlDuplicateBeforeAfterNotAllowed, ['BeforeParameterName', TJvBasePanelEditParameter(Parameters[I]).BeforeParameterName]);
        end;
        try
          if TJvBasePanelEditParameter(Parameters[I]).AfterParameterName <> '' then
            BeforeAfterParameterNames.Add(TJvBasePanelEditParameter(Parameters[I]).AfterParameterName);
        except
          on e:exception do
            raise Exception.CreateResFmt(@RsECreateWinControlsOnWinControlDuplicateBeforeAfterNotAllowed, ['AfterParameterName', TJvBasePanelEditParameter(Parameters[I]).AfterParameterName]);
        end;
      end;
    if ParameterParent is TJvCustomArrangePanel then
      TJvCustomArrangePanel(ParameterParent).DisableArrange;
    for I := 0 to Count - 1 do
      if (BeforeAfterParameterNames.IndexOf(Parameters[I].SearchName) < 0)then
      begin
        Parameters[I].CreateWinControlOnParent(
          GetParentByName(ParameterParent, Parameters[I].ParentParameterName));
        if (Parameters[I] is TJvArrangeParameter) then
          TJvArrangeParameter(Parameters[I]).DisableArrange;
      end;

    // Splitted in a Separate Loop because the order could be changed when Before/AfterParameterName is Used
    for I := 0 to Count - 1 do
      if Assigned(Parameters[I].WinControl) then
        Parameters[I].WinControlData := Parameters[I].AsVariant;

    for I := 0 to Count - 1 do
      if (Parameters[I] is TJvArrangeParameter) and Assigned(Parameters[I].WinControl) then
      begin
        TJvArrangeParameter(Parameters[I]).EnableArrange;
        TJvArrangeParameter(Parameters[I]).ArrangeControls;
      end;
  finally
    if ParameterParent is TJvCustomArrangePanel then
      TJvCustomArrangePanel(ParameterParent).EnableArrange;
    BeforeAfterParameterNames.Free;
    EnableHandleParameterEnabled;
  end;
  HandleParameterEnabled;
  HandleShowValidState;
  if ParameterParent is TJvCustomArrangePanel then
  begin
    TJvCustomArrangePanel(ParameterParent).EnableArrange;
    TJvCustomArrangePanel(ParameterParent).ArrangeControls;
  end;
end;



procedure TJvParameterList.DestroyWinControls;
begin
  FreeAndNil(ArrangePanel);
  FreeAndNil(ScrollBox);
end;

procedure TJvParameterList.GetDataFromWinControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Parameters[I].GetData;
end;

procedure TJvParameterList.SetDataToWinControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Parameters[I].SetData;
end;

function TJvParameterList.ValidateDataAtWinControls: Boolean;
var
  I: Integer;
  V: Variant;
  B: Boolean;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Parameters[I].Visible and Parameters[I].Enabled then
    begin
      V := Parameters[I].WinControlData;
      B := Parameters[I].Validate(V);
      Parameters[I].WinControlData := V;
      if not B then
      begin
        if Assigned(Parameters[I].WinControl) then
          Parameters[I].WinControl.SetFocus;
        Exit;
      end;
    end;
  Result := True;
end;

function TJvParameterList.GetCount: Integer;
begin
  Result := IntParameterList.Count;
end;

function TJvParameterList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not (AObject is TJvBaseParameter) then
    raise EJVCLException.CreateRes(@RsEAddObjectWrongObjectType);
  if TJvBaseParameter(AObject).SearchName = '' then
    raise EJVCLException.CreateRes(@RsEAddObjectSearchNameNotDefined);
  if IntParameterList.IndexOf(S) >= 0 then
    raise Exception.CreateResFmt(@RsEAddObjectDuplicateSearchNamesNotAllowed, [S]);
  TJvBaseParameter(AObject).ParameterList := Self;
  Result := IntParameterList.AddObject(S, AObject);
end;

procedure TJvParameterList.AssignValues(iSourceList: TJvParameterList);
var
  i: Integer;
  SourceParam : TJvBaseParameter;
begin
  if not assigned(iSourceList) then
    Exit;
  for i := 0 to Count - 1 do
  begin
    SourceParam := iSourceList.ParameterByName(Parameters[i].SearchName);
    if Assigned(SourceParam) then
      Parameters[i].AsVariant := SourceParam.AsVariant
    else
      Parameters[i].AsVariant := null;
  end;
end;

procedure TJvParameterList.AssignValuesTo(iDestinationList: TJvParameterList);
begin
  if not assigned(iDestinationList) then
    Exit;
  iDestinationList.AssignValues(Self);
end;

procedure TJvParameterList.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
  if Assigned(ArrangePanel) then
    ArrangePanel.ArrangeSettings := ArrangeSettings;
end;

procedure TJvParameterList.SetParameters(Index: Integer; const Value: TJvBaseParameter);
begin
  if (Index >= 0) and (Index < IntParameterList.Count) then
    IntParameterList.Objects[Index] := Value;
end;

function TJvParameterList.GetParameters(Index: Integer): TJvBaseParameter;
begin
  if (Index >= 0) and (Index < IntParameterList.Count) then
    Result := TJvBaseParameter(IntParameterList.Objects[Index])
  else
    Result := nil;
end;

function TJvParameterList.GetCurrentWidth: Integer;
begin
  if Width > 0 then
    Result := Width
  else if Assigned(ArrangePanel) then
    if ArrangePanel.Align in [alTop, alBottom, alClient] then
      Result := ArrangePanel.ArrangeWidth
    else
      Result := ArrangePanel.Width
  else
    Result := 0;
  if Result > MaxWidth then
    Result := MaxWidth;
end;

function TJvParameterList.GetCurrentHeight: Integer;
begin
  if Height > 0 then
    Result := Height
  else if Assigned(ArrangePanel) then
  begin
    if ArrangePanel.Align in [alLeft, alRight, alClient] then
      Result := ArrangePanel.ArrangeHeight
    else
      Result := ArrangePanel.Height;
  end
  else
    Result := 0;
  if Result > MaxHeight then
    Result := MaxHeight;
end;

procedure TJvParameterList.Clear;
var
  I: Integer;
begin
  for I := 0 to IntParameterList.Count - 1 do
    IntParameterList.Objects[I].Free;
  IntParameterList.Clear;
end;

function TJvParameterList.Clone(AOwner: TComponent): TJvParameterList;
var
  i: Integer;
  List : TJvParameterList;
begin
  List := TJvParameterListClass(ClassType).Create(AOwner);
  List.Assign(Self);
  while List.IntParameterList.Count > 0 do
    List.IntParameterList.Delete(0);
  for i := 0 to Count - 1 do
    List.AddParameter(Parameters[i].Clone(List));
  Result := List;
end;

function TJvParameterList.GetVisibleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Parameters[i].Visible then
      Inc(Result);
end;

type
  TAccessControl = class(TControl);

procedure TJvParameterList.DisableHandleParameterEnabled;
begin
  Inc(FHandleParameterEnabledCnt);
end;

procedure TJvParameterList.EnableHandleParameterEnabled;
begin
  Dec(FHandleParameterEnabledCnt);
end;

procedure TJvParameterList.HandleShowValidState;
var
  I: Integer;
  Parameter: TJvBaseParameter;
  Control : TControl;
  Valid: Boolean;
  ParValid: Boolean;
  IEnable: Integer;

  procedure SetControlFont(AControl: TControl; AValid: Boolean);
  begin
    if Assigned(AControl) then
    begin
      if AValid then
        TAccessControl(AControl).Font.Style := tAccessControl(AControl).Font.Style - [fsItalic]
      else
        TAccessControl(AControl).Font.Style := tAccessControl(AControl).Font.Style + [fsItalic];
    end;
  end;

begin
  Valid := True;
  if ShowParameterValidState then
    for I := 0 to Count - 1 do
    begin
      Parameter := ParameterByIndex(I);
      if Parameter is TJvBasePanelEditParameter then
        if Assigned(TJvBasePanelEditParameter(Parameter).LabelControl) then
          Control := TJvBasePanelEditParameter(Parameter).LabelControl
        else
          Control := TJvBasePanelEditParameter(Parameter).WinControl
      else if Parameter is TJvCheckBoxParameter then
        Control := TJvCheckBoxParameter(Parameter).WinControl
      else
        Control := nil;
      ParValid := Parameter.IsValid(Parameter.GetWinControlData);
      Valid := Valid And ParValid;
      SetControlFont (Control, ParValid);
    end;
  if Assigned(OkButton) then
  begin
    IEnable := GetEnableDisableReasonState(OkButtonDisableReasons, OkButtonEnableReasons);
    if IEnable = -1 then
      OkButton.Enabled := False
    else
      OkButton.Enabled := Valid;
  end;
end;

function TJvParameterList.IndexOfParameter(AParameter: TJvBaseParameter): Integer;
begin
  Result := IntParameterList.IndexOfObject(AParameter);
end;

function TJvParameterList.IsHandleParameterEnabledDisabled: Boolean;
begin
  Result := FHandleParameterEnabledCnt > 0;
end;

//=== { TJvParameterListPropertyStore } ======================================

procedure TJvParameterListPropertyStore.LoadData;
var
  I: Integer;
begin
  if Assigned(AppStorage) And Assigned(ParameterList) then
  begin
    AppStorage.BeginUpdate;
    try
      for I := 0 to ParameterList.Count - 1 do
        if not (ParameterList.Parameters[I] is TJvNoDataParameter) then
          if ParameterList.Parameters[I].StoreValueToAppStorage then
          begin
            if ParameterList.Parameters[I].StoreValueCrypted then
              AppStorage.EnablePropertyValueCrypt;
            if (ParameterList.Parameters[I] is TJvListParameter)
                and (TJvListParameter(ParameterList.Parameters[I]).VariantAsItemIndex) then
              TJvListParameter(ParameterList.Parameters[I]).ItemIndex :=
                AppStorage.ReadInteger(AppStorage.ConcatPaths([AppStoragePath,
                    ParameterList.Parameters[I].SearchName]),
                    TJvListParameter(ParameterList.Parameters[I]).ItemIndex)
            else
              ParameterList.Parameters[I].AsString := AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath,
                ParameterList.Parameters[I].SearchName]),
                ParameterList.Parameters[I].AsString);
            if ParameterList.Parameters[I].StoreValueCrypted then
              AppStorage.DisablePropertyValueCrypt;
          end;
    finally
      AppStorage.EndUpdate;
    end;
  end;
end;

procedure TJvParameterListPropertyStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FParameterList) then
    FParameterList := nil;
end;

procedure TJvParameterListPropertyStore.SetParameterList(const Value: TJvParameterList);
begin
  ReplaceComponentReference(Self, Value, TComponent(FParameterList));
end;

procedure TJvParameterListPropertyStore.StoreData;
var
  I: Integer;
begin
  if Assigned(AppStorage) And Assigned(ParameterList) then
  begin
    AppStorage.BeginUpdate;
    try
      for I := 0 to ParameterList.Count - 1 do
        if not (ParameterList.Parameters[I] is TJvNoDataParameter) then
          if ParameterList.Parameters[I].StoreValueToAppStorage then
          begin
            if ParameterList.Parameters[I].StoreValueCrypted then
              AppStorage.EnablePropertyValueCrypt;
            if (ParameterList.Parameters[I] is TJvListParameter)
               and (TJvListParameter(ParameterList.Parameters[I]).VariantAsItemIndex) then
              AppStorage.WriteInteger(AppStorage.ConcatPaths([AppStoragePath, ParameterList.Parameters[I].SearchName]),
                TJvListParameter(ParameterList.Parameters[I]).ItemIndex)
            else
              AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, ParameterList.Parameters[I].SearchName]),
                ParameterList.Parameters[I].AsString);
            if ParameterList.Parameters[I].StoreValueCrypted then
              AppStorage.DisablePropertyValueCrypt;
          end;
    finally
      AppStorage.EndUpdate;
    end;
  end;
end;

function TJvParameterListSelectList.CreateSelectListDialogInstance(AOwner: TComponent;AOperation:
    TJvAppStorageSelectListOperation; ACaption: string = ''): TJvBaseAppStorageSelectListDialogInstance;
begin
  Result := inherited CreateSelectListDialogInstance(AOwner, AOperation, ACaption);
  if not Assigned(SelectListDialog) then
    Result.DynControlEngine := FParameterList.DynControlEngine;
end;

procedure TJvParameterListSelectList.SetParameterList(Value: TJvParameterList);
begin
  ReplaceComponentReference(Self, Value, TComponent(FParameterList));
end;

function TJvParameterListSelectList.GetAppStorage: TJvCustomAppStorage;
begin
  if Assigned(FParameterList) then
    Result := FParameterList.AppStorage
  else
    Result := nil;
end;

function TJvParameterListSelectList.GetStoragePath: string;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ConcatPaths([ParameterList.AppStoragePath, SelectPath])
  else
    Result := ParameterList.AppStoragePath + PathDelim + SelectPath;
end;

procedure TJvParameterListSelectList.SetAppStorage(Value: TJvCustomAppStorage);
begin
  if Assigned(FParameterList) then
    FParameterList.AppStorage := Value;
end;

procedure TJvParameterListSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FParameterList) then
    FParameterList := nil;
end;

procedure TJvParameterListSelectList.RestoreParameterList(const ACaption: string = '');
var
  SelectPath: string;
begin
  if not Assigned(ParameterList) then
    Exit;
  SelectPath := GetSelectListPath(sloStore, ACaption);
  if SelectPath <> '' then
  begin
    ParameterList.LoadDataFrom(SelectPath);
    ParameterList.SetDataToWinControls;
  end;
end;

procedure TJvParameterListSelectList.SaveParameterList(const ACaption: string = '');
var
  SelectPath: string;
begin
  if not Assigned(ParameterList) then
    Exit;
  SelectPath := GetSelectListPath(sloStore, ACaption);
  if SelectPath <> '' then
  begin
    ParameterList.GetDataFromWinControls;
    ParameterList.StoreDataTo(SelectPath);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

