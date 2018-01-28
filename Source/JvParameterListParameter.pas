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

unit JvParameterListParameter;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  Windows,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms,
  Controls, FileCtrl, Dialogs, ComCtrls, Buttons, Variants,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvPanel, JvParameterList, JvDynControlEngine, JvDSADialogs,
  JvDynControlEngineIntf, ActnList;

type
  TJvNoDataParameter = class(TJvBaseParameter)
  protected
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; override;
    property AsString;
    property AsDouble;
    property AsInteger;
    property AsBoolean;
    property AsDate;
    property Required;
    property StoreValueToAppStorage;
    property ReadOnly;
  public
    constructor Create(AParameterList: TJvParameterList); override;
  end;

  TJvButtonParameter = class(TJvNoDataParameter)
  private
    FAction: TCustomAction;
    FGlyph: TBitmap;
    FNumGlyphs: Integer;
    FLayout: TButtonLayout;
    FOnClick: TJvParameterListEvent;
  protected
    procedure SetGlyph(Value: TBitmap);
    function GetParameterNameExt: string; override;
    procedure Click(Sender: TObject);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Action: TCustomAction read FAction write FAction;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: Integer read FNumGlyphs write FNumGlyphs;
    property Layout: TButtonLayout read FLayout write FLayout;
    property OnClick: TJvParameterListEvent read FOnClick write FOnClick;
  end;

  TJvRadioButtonParameter = class(TJvNoDataParameter)
  private
    FOnClick: TJvParameterListEvent;
  protected
    function GetParameterNameExt: string; override;
    procedure Click(Sender: TObject);
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property OnClick: TJvParameterListEvent read FOnClick write FOnClick;
  end;

  TJvParameterLabelArrangeMode = (lamBefore, lamAbove, lamGroupBox, lamNone);

  TJvBasePanelEditParameter = class(TJvBaseParameter)
  private
    FAfterParameterControl: TControl;
    FAfterParameterName: string;
    FArrangeLabelAndWinControlDisabled: Boolean;
    FBeforeParameterControl: TControl;
    FBeforeParameterName: string;
    FEditWidth: Integer;
    FFrameControl: TWinControl;
    FLabelArrangeMode: TJvParameterLabelArrangeMode;
    FLabelControl: TControl;
    FLabelWidth: Integer;
    FOrgWinControlHeight: Integer;
    FOrgWinControlWidth: Integer;
    procedure ArrangeLabelAndWinControlOnPanelAbove;
    procedure ArrangeLabelAndWinControlOnPanelBefore;
    procedure ArrangeLabelAndWinControlOnPanelGroupBox;
    procedure ArrangeLabelAndWinControlOnPanelNone;
    procedure ArrangeWinControlsonPanel(iLeft, iTop: Integer; var iWidth: Integer;
        iHeight: Integer);
  protected
    procedure ArrangeLabelAndWinControlOnPanel; virtual;
    procedure CreateAfterParameterControl(AParameterParent: TWinControl); virtual;
    procedure CreateBeforeParameterControl(AParameterParent: TWinControl); virtual;
    procedure CreateFramePanel(AParameterParent: TWinControl); virtual;
    procedure CreateLabelControl(AParameterParent: TWinControl); virtual;
    procedure CreateWinControl(AParameterParent: TWinControl); virtual; abstract;
    function GetLabelWidth: Integer; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetLabelWidth(Value: Integer); virtual;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetLabelArrangeMode(Value: TJvParameterLabelArrangeMode); virtual;
    procedure SetTabOrder(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    property FrameControl: TWinControl read FFrameControl;
    property LabelControl: TControl read FLabelControl;
  published
    /// The searchname of the afterparameter.
    /// The afterparameter will positioned behind the edit control.
    property AfterParameterName: string read FAfterParameterName write FAfterParameterName;
    /// The searchname of the beforeparameter.
    /// The beforeparameter will positioned before the edit control.
    property BeforeParameterName: string read FBeforeParameterName write FBeforeParameterName;
    /// Width of the edit control
    property EditWidth: Integer read FEditWidth write FEditWidth;
    /// Mode how the label and the edit control will be arranged :
    /// - lamBefire : The label is before the edit control
    /// - lamAbove : The label is positioned on top of the edit control
    /// - lamGroupBox : A Groupbox is created arround the edit control
    /// - lamNone : No Label is shown
    property LabelArrangeMode: TJvParameterLabelArrangeMode read FLabelArrangeMode
      write SetLabelArrangeMode;
    /// Width of the label, only valid when LabelArrangeMode = lamBefore
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth;
  end;

  TJvArrangeParameter = class(TJvNoDataParameter)
  private
    FArrangeSettings: TJvArrangeSettings;
    FParentControl: TWinControl;
    procedure SetParentControl(const Value: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    function GetParentControl: TWinControl;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure ArrangeControls; virtual;
    procedure DisableArrange; virtual;
    procedure EnableArrange; virtual;
    property ParentControl: TWinControl read GetParentControl write SetParentControl;
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Color;
  end;

  TJvPanelParameter = class(TJvArrangeParameter)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: Integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: Integer;
  protected
    function GetParameterNameExt: string; override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property BevelInner: TPanelBevel read FBevelInner write FBevelInner;
    property BevelOuter: TPanelBevel read FBevelOuter write FBevelOuter;
    property BevelWidth: Integer read FBevelWidth write FBevelWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
  end;

  TJvGroupBoxParameter = class(TJvArrangeParameter)
  protected
    function GetParameterNameExt: string; override;
    procedure ReArrangeGroupbox(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvImageParameter = class(TJvBasePanelEditParameter)
  private
    FAutoSize: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FStretch: Boolean;
    FPicture: TPicture;
  protected
    procedure SetPicture(Value: TPicture);
    procedure SetAutoSize(Value: Boolean); virtual;
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    //    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Center: Boolean read FCenter write FCenter;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay;
    property Transparent: Boolean read FTransparent write FTransparent;
    property Stretch: Boolean read FStretch write FStretch;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TJvLabelParameter = class(TJvNoDataParameter)
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvCheckBoxParameter = class(TJvBaseParameter)
  private
    FOnChange: TNotifyEvent;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvEditParameter = class(TJvBasePanelEditParameter)
  private
    FEditMask: string;
    FPasswordChar: Char;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EditMask: string read FEditMask write FEditMask;
    property PasswordChar: Char read FPasswordChar write FPasswordChar;
  end;

  TJvButtonEditParameter = class(TJvEditParameter)
  private
    FOnClick: TNotifyEvent;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TJvNumberEditorType = (netEdit, netSpin, netCalculate);

  TJvNumberEditParameter = class(TJvEditParameter)
  private
    FEditorType: TJvNumberEditorType;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property EditorType: TJvNumberEditorType read FEditorType write FEditorType;
  end;

  TJvIntegerEditParameter = class(TJvNumberEditParameter)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FIncrement: Integer;
  protected
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Increment: Integer read FIncrement write FIncrement;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

  TJvDoubleEditParameter = class(TJvNumberEditParameter)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FIncrement: Integer;
  protected
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Increment: Integer read FIncrement write FIncrement;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
  end;

  TJvFileNameParameter = class(TJvBasePanelEditParameter)
  private
    FDefaultExt: string;
    FFilter: string;
    FFilterIndex: Integer;
    FInitialDir: string;
    FDialogOptions: TOpenOptions;
    FDialogTitle: string;
    FDialogKind: TJvDynControlFileNameDialogKind;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property FileName: string read GetAsString write SetAsString;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogKind: TJvDynControlFileNameDialogKind read FDialogKind write FDialogKind;
  end;

  TJvDirectoryParameter = class(TJvBasePanelEditParameter)
  private
    FInitialDir: string;
    FDialogTitle: string;
    FDialogOptions: TSelectDirOpts;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    function IsDataValid(const AData: Variant; var vMsg: String): Boolean; override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Directory: string read GetAsString write SetAsString;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogOptions: TSelectDirOpts read FDialogOptions write FDialogOptions;
  end;

  TJvListParameter = class(TJvBasePanelEditParameter)
  private
    FItemList: TStringList;
    FItemIndex: Integer;
    FSorted: Boolean;
    FVariantAsItemIndex: Boolean;
  protected
    function GetItemList: TStringList; virtual;
    procedure SetItemList(Value: TStringList); virtual;
    procedure SetItemIndex(Value: Integer); virtual;
    procedure SetAsString(const Value: string); override;
    function GetAsString: string; override;
    procedure SetAsInteger(Value: Integer); override;
    function GetAsInteger: Integer; override;
    procedure SetAsVariant(Value: Variant); override;
    function GetAsVariant: Variant; override;
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SearchItemIndex(const Search: string);
  published
    property ItemList: TStringList read GetItemList write SetItemList;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Sorted: Boolean read FSorted write FSorted;
    property VariantAsItemIndex: Boolean read FVariantAsItemIndex write FVariantAsItemIndex default False;
  end;

  TJvRadioGroupParameter = class(TJvListParameter)
  private
    FColumns: Integer;
  protected
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  published
    property Columns: Integer read FColumns write FColumns;
  end;

  TJvComboBoxParameterStyle = (cpsListEdit, cpsListFixed);

  TJvComboBoxParameter = class(TJvListParameter)
  private
    FSorted: Boolean;
    FNewEntriesAllowed: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: Boolean read FSorted write FSorted;
    property NewEntriesAllowed: Boolean read FNewEntriesAllowed write FNewEntriesAllowed;
  end;

  TJvListBoxParameter = class(TJvListParameter)
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TJvCheckListItemDataWrapper = class(TObject)
  private
    FState: TCheckBoxState;
    FItemEnabled: Boolean;
    FHeader: Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property ItemEnabled: Boolean read FItemEnabled write FItemEnabled;
    property Header: Boolean read FHeader write FHeader;
  end;

  TJvCheckListBoxParameter = class(TJvListParameter)
  private
    FAllowGrayed: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
    function GetItemData(Index: Integer): TJvCheckListItemDataWrapper;
    procedure SetItemData(Index: Integer; Value: TJvCheckListItemDataWrapper);
    procedure SetItemList(Value: TStringList); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure AddCheckListBoxItem(const AText: string; AState: TCheckBoxState = cbChecked;
      AItemEnabled: Boolean = True; AHeader: Boolean = False);
    property ItemData[Index: Integer]: TJvCheckListItemDataWrapper read GetItemData write SetItemData;
  published
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed;
  end;

  TJvTimeParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
  end;

  TJvDateTimeParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property MinDate: TDate read FMinDate write FMinDate;
  end;

  TJvDateParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property MinDate: TDate read FMinDate write FMinDate;
  end;

  TJvMemoParameter = class(TJvBasePanelEditParameter)
  private
    FWordWrap: Boolean;
    FWantTabs: Boolean;
    FWantReturns: Boolean;
    FScrollBars: TScrollStyle;
    FFontName: string;
    procedure SetFontName(const Value: string);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetWantReturns(const Value: Boolean);
    procedure SetWantTabs(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
  published
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property WantTabs: Boolean read FWantTabs write SetWantTabs;
    property WantReturns: Boolean read FWantReturns write SetWantReturns;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property FontName: string read FFontName write SetFontName;
  end;

  TJvRichEditParameter = class(TJvBasePanelEditParameter)
  private
    FWordWrap: Boolean;
    FWantTabs: Boolean;
    FWantReturns: Boolean;
    FScrollBars: TScrollStyle;
    FFontName: string;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
  published
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property WantTabs: Boolean read FWantTabs write FWantTabs;
    property WantReturns: Boolean read FWantReturns write FWantReturns;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;
    property FontName: string read FFontName write FFontName;
  end;

  TJvPageControlParameter = class(TJvArrangeParameter)
  private
    fHotTrack: Boolean;
    fMultiline: Boolean;
    fScrollOpposite: Boolean;
    fTabIndex: Integer;
    fTabPosition: TTabPosition;
    FPages: TStringList;
    FRaggedRight: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure RearrangePageControl(Sender: TObject; nLeft, nTop, nWidth, nHeight:
      Integer);
    procedure SetPages(Value: TStringList);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure ArrangeControls; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
    procedure DisableArrange; override;
    procedure EnableArrange; override;
    function PageWinControl(Index: Integer): TWinControl;
  published
    property HotTrack: Boolean read fHotTrack write fHotTrack;
    property Multiline: Boolean read fMultiline write fMultiline;
    property ScrollOpposite: Boolean read fScrollOpposite write fScrollOpposite;
    property TabIndex: Integer read fTabIndex write fTabIndex;
    property TabPosition: TTabPosition read fTabPosition write fTabPosition;
    property Pages: TStringList read FPages write SetPages;
    property RaggedRight: Boolean read FRaggedRight write FRaggedRight;
  end;

  TJvCheckComboBoxParameter = class(TJvListParameter)
  private
    FDelimiter: string;
    FSorted: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Delimiter: string read FDelimiter write FDelimiter;
    property Sorted: Boolean read FSorted write FSorted;
  end;

function DSADialogsMessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult;

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
  JvResources, JvJVCLUtils, JclSysUtils;

//=== { Support function for DPI Aware apps } ================================

function PPIScale(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

function DSADialogsMessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult;
begin
  Result := JvDSADialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, Center, Timeout, DefaultButton,
    CancelButton, HelpButton, ADynControlEngine);
end;

//=== { TJvNoDataParameter } =================================================

constructor TJvNoDataParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  StoreValueToAppStorage := False;
end;

function TJvNoDataParameter.IsDataValid(const AData: Variant; var vMsg:
    String): Boolean;
begin
  Result := True;
end;

//=== { TJvButtonParameter } =================================================

function TJvButtonParameter.GetParameterNameExt: string;
begin
  Result := 'Button';
end;

procedure TJvButtonParameter.Click(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(ParameterList, Self);
end;

procedure TJvButtonParameter.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TJvButtonParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvButtonParameter then
  begin
    Glyph := TJvButtonParameter(Source).Glyph;
    Layout := TJvButtonParameter(Source).Layout;
    NumGlyphs := TJvButtonParameter(Source).NumGlyphs;
    OnClick := TJvButtonParameter(Source).OnClick;
    Action := TJvButtonParameter(Source).Action;
  end;
end;

constructor TJvButtonParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FGlyph := TBitmap.Create;
end;

procedure TJvButtonParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  Button: TButton;
begin
  Button := DynControlEngine.CreateButton(Self, ParameterParent,
    GetParameterName, Caption, Hint, Click, False, False);
  Button.Action := Action;
  SetWinControl (Button);
  WinControl.Height := PPIScale(WinControl.Height);
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

destructor TJvButtonParameter.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvButtonParameter.SetWinControlProperties;
var
  IJvButton: IJvDynControlButton;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlButton, IJvButton) then
  begin
    IJvButton.ControlSetGlyph(Glyph);
    IJvButton.ControlSetNumGlyphs(NumGlyphs);
    IJvButton.ControlSetLayout(Layout);
  end;
end;

//=== { TJvRadioButtonParameter } ============================================

function TJvRadioButtonParameter.GetParameterNameExt: string;
begin
  Result := 'RadioButton';
end;

procedure TJvRadioButtonParameter.Click(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(ParameterList, Self);
end;

procedure TJvRadioButtonParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TJvRadioButtonParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateRadioButton(Self, ParameterParent,
    GetParameterName, Caption));
  WinControl.Hint := Hint;
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvRadioButtonParameter.SetWinControlProperties;
begin
  inherited SetWinControlProperties;
end;

//=== { TJvBasePanelEditParameter } ==========================================

constructor TJvBasePanelEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FLabelArrangeMode := lamAbove;
  FLabelWidth := 0;
  FEditWidth := 0;
  FArrangeLabelAndWinControlDisabled := False;
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanel;
var
  TmpLabelArrangeMode: TJvParameterLabelArrangeMode;
begin
  if not Assigned(FrameControl) or not Assigned(WinControl) or FArrangeLabelAndWinControlDisabled then
    Exit;
  if not Assigned(LabelControl) and (LabelArrangeMode in [lamBefore, lamAbove]) then
    TmpLabelArrangeMode := lamNone
  else
    TmpLabelArrangeMode := LabelArrangeMode;

  case TmpLabelArrangeMode of
    lamBefore:
      ArrangeLabelAndWinControlOnPanelBefore;
    lamAbove:
      ArrangeLabelAndWinControlOnPanelAbove;
    lamNone:
      ArrangeLabelAndWinControlOnPanelNone;
    lamGroupBox:
      ArrangeLabelAndWinControlOnPanelGroupBox;
  end;
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanelAbove;
var
  l, t, w, h: Integer;
begin
  t := LabelControl.Height;
  l := 0;

  if Height > 0 then
    h := Height - t
  else
    h := fOrgWinControlHeight;

  if EditWidth > 0 then
    w := EditWidth
  else if Width > 0 then
    w := Width
  else
    w := 0;

  ArrangeWinControlsonPanel(l, t, w, h);

  FrameControl.Height := t + h;
  FrameControl.Width := l + w;
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanelBefore;
var
  DynCtrlFont: IJvDynControlFont;
  l, t, w, h: Integer;
begin
  if LabelWidth > 0 then
    LabelControl.Width := LabelWidth
  else
    if Supports(LabelControl, IJvDynControlFont, DynCtrlFont) then
      LabelControl.Width :=
         DynControlEngine.GetControlTextWidth(LabelControl,
                                              DynCtrlFont.ControlFont, Caption+'X');

  t := LabelControl.Top;
  l := LabelControl.Left + LabelControl.Width + PPIScale(4);

  if Height > 0 then
    h := Height
  else
    h := fOrgWinControlHeight;

  if EditWidth > 0 then
    w := EditWidth
  else if Width > 0 then
    w := Width - l
  else
    w := 0;

  ArrangeWinControlsonPanel(l, t, w, h);

  LabelControl.Top := t + Round((h - LabelControl.Height) / 2);

  FrameControl.Height := t + h;
  FrameControl.Width := l + w;

end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanelGroupBox;
var
  l, t, w, h: Integer;
begin
  t := PPIScale(16);
  l := PPIScale(5);

  if Height > 0 then
    h := Height - PPIScale(20)
  else
    h := fOrgWinControlHeight;

  if EditWidth > 0 then
    w := EditWidth
  else if Width > 0 then
    w := Width - PPIScale(9)
  else
    w := 0;

  ArrangeWinControlsonPanel(l, t, w, h);

  FrameControl.Height := h + PPIScale(20);
  FrameControl.Width := w + PPIScale(9);
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanelNone;
var
  l, t, w, h: Integer;
begin
  t := 0;
  l := 0;

  if Height > 0 then
    h := Height
  else
    h := fOrgWinControlHeight;

  if EditWidth > 0 then
    w := EditWidth
  else if Width > 0 then
    w := Width
  else
    w := 0;

  ArrangeWinControlsonPanel(l, t, w, h);

  FrameControl.Height := h;
  FrameControl.Width := w;
end;

procedure TJvBasePanelEditParameter.ArrangeWinControlsonPanel(iLeft, iTop:
    Integer; var iWidth: Integer; iHeight: Integer);
const
  Space = 2;
var
  l, w: Integer;
begin
  l := iLeft;
  w := 0;
  if Assigned(FBeforeParameterControl) then
  begin
    FBeforeParameterControl.Left := l;
    FBeforeParameterControl.Top := iTop;
    FBeforeParameterControl.Height := iHeight;
    l := FBeforeParameterControl.Left + FBeforeParameterControl.Width+ PPIScale(Space);
    w := w + FBeforeParameterControl.Width+ PPIScale(Space);
  end;
  WinControl.Left := l;
  WinControl.Top := iTop;
  WinControl.Height := iHeight;
  if iWidth > 0 then
  begin
    WinControl.Width := iWidth-l+iLeft;
    if Assigned (FAfterParameterControl) then
      WinControl.Width := WinControl.Width - (FAfterParameterControl.Width + PPIScale(Space));
  end
  else
    WinControl.Width := FOrgWinControlWidth;
  w := w + WinControl.Width;
  if Assigned(FAfterParameterControl) then
  begin
    l := WinControl.Left + WinControl.Width + PPIScale(Space);
    FAfterParameterControl.Left := l;
    FAfterParameterControl.Top := iTop;
    FAfterParameterControl.Height := iHeight;
    w := w + FAfterParameterControl.Width+ PPIScale(Space);
  end;
  iWidth := w;
end;

procedure TJvBasePanelEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvBasePanelEditParameter then
  begin
    LabelArrangeMode := TJvBasePanelEditParameter(Source).LabelArrangeMode;
    LabelWidth := TJvBasePanelEditParameter(Source).LabelWidth;
    EditWidth := TJvBasePanelEditParameter(Source).EditWidth;
    AfterParameterName := TJvBasePanelEditParameter(Source).AfterParameterName ;
    BeforeParameterName := TJvBasePanelEditParameter(Source).BeforeParameterName ;
  end;
end;

procedure TJvBasePanelEditParameter.CreateAfterParameterControl(
  AParameterParent: TWinControl);
var
  AfterParameter: TJvBaseParameter;
begin
  AfterParameter := ParameterList.ParameterByName(AfterParameterName);
  if Assigned(AfterParameter) and AfterParameter.Visible then
  begin
    AfterParameter.CreateWinControlOnParent(AParameterParent);
    if AfterParameter is TJvBasePanelEditParameter then
      FAfterParameterControl := TJvBasePanelEditParameter(AfterParameter).FrameControl
    else
      FAfterParameterControl := AfterParameter.WinControl;
    FAfterParameterControl.Parent := AParameterParent;
  end
  else
    FAfterParameterControl := nil;
end;

procedure TJvBasePanelEditParameter.CreateBeforeParameterControl(AParameterParent: TWinControl);
var
  BeforeParameter: TJvBaseParameter;
begin
  BeforeParameter := ParameterList.ParameterByName(BeforeParameterName);
  if Assigned(BeforeParameter) and BeforeParameter.Visible then
  begin
    BeforeParameter.CreateWinControlOnParent(AParameterParent);
    if BeforeParameter is TJvBasePanelEditParameter then
      FBeforeParameterControl := TJvBasePanelEditParameter(BeforeParameter).FrameControl
    else
      FBeforeParameterControl := BeforeParameter.WinControl;
    FBeforeParameterControl.Parent := AParameterParent;
  end
  else
    FBeforeParameterControl := nil;
end;

procedure TJvBasePanelEditParameter.CreateFramePanel(AParameterParent: TWinControl);
var
  DynBevel: IJvDynControlBevelBorder;
begin
  if LabelArrangeMode = lamGroupBox then
    FFrameControl := DynControlEngine.CreateGroupBoxControl(Self, AParameterParent,
      GetParameterName + 'GroupBox', Caption)
  else
    FFrameControl := DynControlEngine.CreatePanelControl(Self, AParameterParent,
      GetParameterName + 'Panel', '', alNone);
  FrameControl.Height := Height;
  FrameControl.Width := Width;
  if Supports(FrameControl, IJvDynControlBevelBorder, DynBevel) then
  begin
    DynBevel.ControlSetBevelInner(bvNone);
    DynBevel.ControlSetBevelOuter(bvNone);
  end
  else if FrameControl is TPanel then
  begin
    TPanel(FrameControl).BevelInner := bvNone;
    TPanel(FrameControl).BevelOuter := bvNone;
  end;
end;

procedure TJvBasePanelEditParameter.CreateLabelControl(AParameterParent: TWinControl);
var
  IDynAutoSize: IJvDynControlAutoSize;
begin
  if (Caption = '') or (LabelArrangeMode in [lamGroupBox, lamNone]) then
    Exit;
  fLabelControl := DynControlEngine.CreateLabelControl(Self, AParameterParent,
    GetParameterName + 'Label', Caption, WinControl);
  LabelControl.Visible := True;
  LabelControl.Enabled := Enabled;
  LabelControl.Parent := AParameterParent;
  if Supports (LabelControl, IJvDynControlAutoSize, IDynAutoSize) then
  begin
    IDynAutoSize.ControlSetAutosize(True);
    IDynAutoSize.ControlSetAutosize(False);
  end
  else
    LabelControl.Height := PPIScale(16);

end;

procedure TJvBasePanelEditParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  CreateFramePanel(ParameterParent);
  CreateBeforeParameterControl(FrameControl);
  CreateWinControl(FrameControl);
  CreateAfterParameterControl(FrameControl);
  CreateLabelControl(FrameControl);
  fOrgWinControlHeight := WinControl.Height;
  fOrgWinControlWidth := WinControl.Width;
  ArrangeLabelAndWinControlOnPanel;
end;

function TJvBasePanelEditParameter.GetLabelWidth: Integer;
begin
  if Assigned(ParameterList) and (FLabelWidth <= 0) then
    Result := ParameterList.DefaultParameterLabelWidth
  else
    Result := FLabelWidth;
end;

procedure TJvBasePanelEditParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)  then
  begin
    if (AComponent = FFrameControl) then
      FFrameControl := nil;
    if (AComponent = FLabelControl) then
      FLabelControl := nil;
    if (AComponent = FAfterParameterControl) then
      FAfterParameterControl := nil;
    if (AComponent = FBeforeParameterControl) then
      FBeforeParameterControl := nil;
  end;
end;

procedure TJvBasePanelEditParameter.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(FrameControl) then
    FrameControl.Enabled := Value;
  if Assigned(LabelControl) then
    LabelControl.Enabled := Value;
  if Assigned(FAfterParameterControl) then
    FAfterParameterControl.Enabled := Value;
  if Assigned(FBeforeParameterControl) then
    FBeforeParameterControl.Enabled := Value;
end;

procedure TJvBasePanelEditParameter.SetHeight(Value: Integer);
begin
  inherited SetHeight(Value);
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetLabelArrangeMode(Value:
  TJvParameterLabelArrangeMode);
begin
  FLabelArrangeMode := Value;
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetLabelWidth(Value: Integer);
begin
  FLabelWidth := Value;
  if Assigned(WinControl) then
    ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetTabOrder(Value: Integer);
begin
  if Assigned(FrameControl) then
    FrameControl.TabOrder := Value;
end;

procedure TJvBasePanelEditParameter.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Assigned(FrameControl) then
    FrameControl.Visible := Value;
end;

procedure TJvBasePanelEditParameter.SetWidth(Value: Integer);
begin
  inherited SetWidth(Value);
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetWinControlProperties;
begin
  try
    FArrangeLabelAndWinControlDisabled := True;
    inherited SetWinControlProperties;
  finally
    FArrangeLabelAndWinControlDisabled := False;
  end;
end;

type
  TAccessCustomControl = class(TCustomControl);

  //=== { TJvLabelParameter } ==================================================

procedure TJvLabelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateStaticTextControl(Self, ParameterParent,
    GetParameterName, Caption));
end;

//=== { TJvImageParameter } ==================================================

constructor TJvImageParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPicture := TPicture.Create;
  FAutoSize := False;
  FCenter := False;
  FIncrementalDisplay := False;
  FStretch := False;
  FTransparent := False;
end;

destructor TJvImageParameter.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvImageParameter.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvImageParameter.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
  end;
end;

procedure TJvImageParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvImageParameter then
  begin
    Picture := TJvImageParameter(Source).Picture;
    //  AutoSize := TJvImageParameter(Source).AutoSize;
    Center := TJvImageParameter(Source).Center;
    IncrementalDisplay := TJvImageParameter(Source).IncrementalDisplay;
    Stretch := TJvImageParameter(Source).Stretch;
    Transparent := TJvImageParameter(Source).Transparent;
  end;
end;

function TJvImageParameter.GetParameterNameExt: string;
begin
  Result := 'Image';
end;

procedure TJvImageParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpImage: IJvDynControlImage;
begin
  SetWinControl (DynControlEngine.CreateImageControl(Self, AParameterParent, GetParameterName));
  if Supports(WinControl, IJvDynControlImage, ITmpImage) then
  begin
    ITmpImage.ControlSetPicture(Picture);
    //      ITmpImage.ControlSetAutoSize(AutoSize);
    ITmpImage.ControlSetIncrementalDisplay(IncrementalDisplay);
    ITmpImage.ControlSetCenter(Center);
    ITmpImage.ControlSetStretch(Stretch);
    ITmpImage.ControlSetTransparent(Transparent);
  end;
end;

//=== { TJvArrangeParameter } ================================================

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FArrangeSettings := TJvArrangeSettings.Create(Self);
  FArrangeSettings.BorderLeft := PPIScale(2);
  FArrangeSettings.BorderTop := PPIScale(2);
  FArrangeSettings.DistanceVertical := PPIScale(2);
  FArrangeSettings.DistanceHorizontal := PPIScale(2);
  FArrangeSettings.AutoArrange := True;
end;

destructor TJvArrangeParameter.Destroy;
begin
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvArrangeParameter.ArrangeControls;
begin
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).ArrangeControls;
end;

procedure TJvArrangeParameter.DisableArrange;
begin
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).DisableArrange;
end;

procedure TJvArrangeParameter.EnableArrange;
begin
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).EnableArrange;
end;

procedure TJvArrangeParameter.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;

procedure TJvArrangeParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FParentControl) and (Operation = opRemove) then
    FParentControl := nil;
end;

function TJvArrangeParameter.GetParentControl: TWinControl;
begin
  if Assigned(FParentControl) then
    Result := FParentControl
  else
    Result := WinControl;
end;

procedure TJvArrangeParameter.SetParentControl(const Value: TWinControl);
begin
  ReplaceComponentReference(Self, Value, TComponent(FParentControl));
end;

//=== { TJvPanelParameter } ==================================================

constructor TJvPanelParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelWidth := 1;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;

procedure TJvPanelParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvPanelParameter then
  begin
    BevelInner := TJvPanelParameter(Source).BevelInner;
    BevelOuter := TJvPanelParameter(Source).BevelOuter;
  end;
end;

function TJvPanelParameter.GetParameterNameExt: string;
begin
  Result := 'Panel';
end;

procedure TJvPanelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreatePanelControl(Self, ParameterParent,
    GetParameterName, Caption, alNone));
  ParentControl := WinControl;
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvPanelParameter.SetWinControlProperties;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpArrangePanel: IJvArrangePanel;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlPanel, ITmpPanel) then
    ITmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
  if Supports(WinControl, IJvArrangePanel, ITmpArrangePanel) then
    ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;

//=== { TJvGroupBoxParameter } ===============================================

constructor TJvGroupBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ArrangeSettings.AutoSize := asHeight;
end;

function TJvGroupBoxParameter.GetParameterNameExt: string;
begin
  Result := 'GroupBoxPanel';
end;

procedure TJvGroupBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  Panel: TJvPanel;
begin
  SetWinControl (DynControlEngine.CreateGroupBoxControl(Self, ParameterParent,
    GetParameterName, Caption));
  Panel := TJvPanel.Create(ParameterParent.Owner);
  ParentControl := Panel;
  Panel.Name := GetParameterName;
  Panel.ArrangeSettings := ArrangeSettings;
  Panel.BevelInner := bvNone;
  Panel.BevelOuter := bvNone;
  Panel.Parent := WinControl;
  Panel.Align := alClient;
  Panel.Visible := True;
  Panel.Caption := '';
  Panel.Color := Color;
  Panel.OnResizeParent := ReArrangeGroupbox;
  //  Panel.Transparent := True;
end;

procedure TJvGroupBoxParameter.ReArrangeGroupbox(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);
begin
  if ArrangeSettings.AutoSize in [asWidth, asBoth] then
    WinControl.Width := nWidth + PPIScale(5);
  if ArrangeSettings.AutoSize in [asHeight, asBoth] then
    WinControl.Height := nHeight + PPIScale(22);
end;

procedure TJvGroupBoxParameter.SetWinControlProperties;
var
  ITmpArrangePanel: IJvArrangePanel;
begin
  inherited SetWinControlProperties;
  if Supports(ParentControl, IJvArrangePanel, ITmpArrangePanel) then
    ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;

//=== { TJvListParameter } ===================================================

constructor TJvListParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FItemList := TStringList.Create;
  Sorted := False;
  FItemIndex := -1;
  FVariantAsItemIndex := False;
end;

destructor TJvListParameter.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

procedure TJvListParameter.SetAsString(const Value: string);
var
  I: Integer;
begin
  I := ItemList.IndexOf(VarToStr(Value));
  if (I >= 0) and (I < ItemList.Count) then
    ItemIndex := I
  else
    ItemIndex := -1;
  if not VariantAsItemIndex then
    inherited SetAsVariant(Value);
end;

function TJvListParameter.GetAsString: string;
begin
  if VariantAsItemIndex then
    if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
      Result := ItemList[ItemIndex]
    else
      Result := ''
  else
    Result := inherited GetAsString;
end;

procedure TJvListParameter.SetAsInteger(Value: Integer);
begin
  ItemIndex := Value
end;

function TJvListParameter.GetAsInteger: Integer;
begin
  Result := ItemIndex;
end;

procedure TJvListParameter.SetAsVariant(Value: Variant);
begin
  if VarIsNullEmpty(Value) then
    ItemIndex := -1
  else if VariantAsItemIndex then
    if VarType(Value) in [varSmallInt, varInteger, varByte, varShortInt, varWord, varLongWord] then
      ItemIndex := Value
    else
      SetAsString(Value)
  else
    SetAsString(Value);
end;

function TJvListParameter.GetAsVariant: Variant;
begin
  Result := inherited GetAsVariant;
  if VariantAsItemIndex then
    if VarToStr(Result) = '-1' then
      Result := Null;
end;

function TJvListParameter.GetItemList: TStringList;
begin
  Result := FItemList;
end;

procedure TJvListParameter.SetItemList(Value: TStringList);
begin
  FItemList.Assign(Value);
  if Assigned(Value) then
    SetItemIndex(FItemIndex);
  if Assigned(WinControl) then
    SetWinControlProperties;
end;

procedure TJvListParameter.SetItemIndex(Value: Integer);
begin
  if Assigned(ItemList) then
  begin
    if Value >= ItemList.Count then
      FItemIndex := ItemList.Count - 1
    else
      FItemIndex := Value;
    if VariantAsItemIndex then
      inherited SetAsVariant(FItemIndex)
    else if (FItemIndex >= 0) and (FItemIndex < ItemList.Count) then
      inherited SetAsVariant(ItemList[FItemIndex])
    else
      inherited SetAsVariant('');
  end
  else
  begin
    FItemIndex := -1;
    if VariantAsItemIndex then
      inherited SetAsVariant(FItemIndex)
    else
      inherited SetAsVariant('');
  end;
end;

function TJvListParameter.GetWinControlData: Variant;
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
    Index := JvDynControlData.ControlValue
  else
    Index := -1;
  if VariantAsItemIndex then
    Result := Index
  else if (Index >= 0) and (Index < ItemList.Count) then
    Result := ItemList[Index]
  else
    Result := JvDynControlData.ControlValue;
end;

procedure TJvListParameter.SetWinControlData(Value: Variant);
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
    if VariantAsItemIndex then
      JvDynControlData.ControlValue := Value
    else
    begin
      Index := ItemList.IndexOf(VarToStr(Value));
      if (Index >= 0) and (Index < ItemList.Count) then
        JvDynControlData.ControlValue := ItemList[Index]
      else
        JvDynControlData.ControlValue := '';
    end;
end;

procedure TJvListParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvListParameter then
  begin
    ItemList.Assign(TJvListParameter(Source).ItemList);
    ItemIndex := TJvListParameter(Source).ItemIndex;
    Sorted := TJvListParameter(Source).Sorted;
  end;
end;

procedure TJvListParameter.SearchItemIndex(const Search: string);
var
  I: Integer;
begin
  FItemIndex := -1;
  for I := 0 to ItemList.Count - 1 do
    if Search = ItemList.Strings[I] then
    begin
      FItemIndex := I;
      Break;
    end;
end;

procedure TJvListParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlItems := ItemList;
end;

//=== { TJvRadioGroupParameter } =============================================

procedure TJvRadioGroupParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvRadioGroupParameter then
    Columns := TJvRadioGroupParameter(Source).Columns;
end;

procedure TJvRadioGroupParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateRadioGroupControl(Self, ParameterParent,
    GetParameterName, Caption, ItemList));
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvRadioGroupParameter.CreateWinControl(AParameterParent: TWinControl);
begin
end;

procedure TJvRadioGroupParameter.SetWinControlProperties;
var
  ITmpRadioGroup: IJvDynControlRadioGroup;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlRadioGroup, ITmpRadioGroup) then
    ITmpRadioGroup.ControlSetColumns(Columns);
end;

procedure TJvCheckBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvCheckBoxParameter then
    OnChange := TJvCheckBoxParameter(Source).OnChange;
end;

//=== { TJvCheckBoxParameter } ===============================================

procedure TJvCheckBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  DynCtrlData: IJvDynControlData;
begin
  SetWinControl (DynControlEngine.CreateCheckBoxControl(Self, ParameterParent,
    GetParameterName, Caption));
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
  if Supports(WinControl, IJvDynControlData, DynCtrlData) and Assigned(OnChange) then
    DynCtrlData.ControlSetOnChange(OnChange);
end;

//=== { TJvComboBoxParameter } ===============================================

constructor TJvComboBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
  FSorted := False;
  FNewEntriesAllowed := False;
end;

procedure TJvComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvComboBoxParameter then
  begin
    Sorted := TJvComboBoxParameter(Source).Sorted;
    NewEntriesAllowed := TJvComboBoxParameter(Source).NewEntriesAllowed;
  end;
end;

function TJvComboBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ComboBox';
end;

procedure TJvComboBoxParameter.GetData;
begin
  if Assigned(WinControl) then
    Value := WinControlData;
end;

procedure TJvComboBoxParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := Value;
end;

procedure TJvComboBoxParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateComboBoxControl(Self, AParameterParent,
    GetParameterName, ItemList));
end;

procedure TJvComboBoxParameter.SetWinControlProperties;
var
  ITmpComboBox: IJvDynControlComboBox;
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlComboBox, ITmpComboBox) then
    ITmpComboBox.ControlSetNewEntriesAllowed(NewEntriesAllowed);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

function TJvComboBoxParameter.GetWinControlData: Variant;
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
  begin
    Index := ItemList.IndexOf(JvDynControlData.ControlValue);
    if VariantAsItemIndex then
      Result := Index
    else if (Index >= 0) and (Index < ItemList.Count) then
      Result := ItemList[Index]
    else
      Result := JvDynControlData.ControlValue;
  end
  else
    if VariantAsItemIndex then
      Result := -1
    else
      Result := null;
end;

procedure TJvComboBoxParameter.SetWinControlData(Value: Variant);
begin
  if Assigned(JvDynControlData) then
    if VariantAsItemIndex then
      JvDynControlData.ControlValue := ItemList[Value]
    else
      JvDynControlData.ControlValue := Value;
end;

//=== { TJvListBoxParameter } ================================================

procedure TJvListBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvListBoxParameter then
    Sorted := TJvListBoxParameter(Source).Sorted;
end;

function TJvListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ListBox';
end;

procedure TJvListBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  SetWinControl (DynControlEngine.CreateListBoxControl(Self, AParameterParent,
    GetParameterName, ItemList));
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvListBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//==== TJvCheckListItemDataWrapper ===========================================

procedure TJvCheckListItemDataWrapper.SetChecked(Check: Boolean);
begin
  if Check then
    FState := cbChecked
  else
    FState := cbUnchecked;
end;

function TJvCheckListItemDataWrapper.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

//=== { TJvCheckListBoxParameter } ===========================================

constructor TJvCheckListBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FSorted := False;
  FAllowGrayed := False;
end;

destructor TJvCheckListBoxParameter.Destroy;
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    ItemList.Objects[I].Free;
  inherited Destroy;
end;

procedure TJvCheckListBoxParameter.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TJvCheckListBoxParameter then
  begin
    Sorted := TJvCheckListBoxParameter(Source).Sorted;
    AllowGrayed := TJvCheckListBoxParameter(Source).AllowGrayed;
    for I := 0 to ItemList.Count do
      ItemData[I] := TJvCheckListBoxParameter(Source).ItemData[I];
  end;
end;

procedure TJvCheckListBoxParameter.GetData;
var
  ITmpCheckListBox: IJvDynControlCheckListBox;
  I: Integer;
begin
  inherited GetData;
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    for I := 0 to ItemList.Count - 1 do
    begin
      ItemData[I].ItemEnabled := ITmpCheckListBox.ControlGetItemEnabled(I);
      ItemData[I].State := ITmpCheckListBox.ControlGetState(I);
      ItemData[I].Header := ITmpCheckListBox.ControlGetHeader(I);
    end;
end;

procedure TJvCheckListBoxParameter.SetData;
var
  ITmpCheckListBox: IJvDynControlCheckListBox;
  I: Integer;
begin
  inherited SetData;
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    for I := 0 to ItemList.Count - 1 do
    begin
      ITmpCheckListBox.ControlSetItemEnabled(I, ItemData[I].ItemEnabled);
      ITmpCheckListBox.ControlSetState(I, ItemData[I].State);
      ITmpCheckListBox.ControlSetHeader(I, ItemData[I].Header);
    end;
end;

procedure TJvCheckListBoxParameter.AddCheckListBoxItem(const AText: string;
  AState: TCheckBoxState = cbChecked; AItemEnabled: Boolean = True;
  AHeader: Boolean = False);
begin
  ItemList.Add(AText);
  ItemData[ItemList.Count - 1].Header := AHeader;
  ItemData[ItemList.Count - 1].State := AState;
  ItemData[ItemList.Count - 1].ItemEnabled := AItemEnabled;
end;

function TJvCheckListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'CheckListBox';
end;

procedure TJvCheckListBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  SetWinControl (DynControlEngine.CreateCheckListBoxControl(Self, AParameterParent,
    GetParameterName, ItemList));
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvCheckListBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
  ITmpCheckListBox: IJvDynControlCheckListBox;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    ITmpCheckListBox.ControlSetAllowGrayed(AllowGrayed);
end;

function TJvCheckListBoxParameter.GetItemData(Index: Integer): TJvCheckListItemDataWrapper;
begin
  if (Index >= 0) and (Index < ItemList.Count) then
  begin
    if not Assigned(ItemList.Objects[Index]) then
    begin
      ItemList.Objects[Index] := TJvCheckListItemDataWrapper.Create;
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).State := cbChecked;
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).Header := False;
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).ItemEnabled := True;
    end;
    Result := TJvCheckListItemDataWrapper(ItemList.Objects[Index]);
  end
  else
    Result := nil;
end;

procedure TJvCheckListBoxParameter.SetItemData(Index: Integer; Value: TJvCheckListItemDataWrapper);
var
  Data: TJvCheckListItemDataWrapper;
begin
  Data := GetItemData(Index);
  if Assigned(Data) then
  begin
    Data.State := Value.State;
    Data.ItemEnabled := Value.ItemEnabled;
    Data.Header := Value.Header;
  end;
end;

procedure TJvCheckListBoxParameter.SetItemList(Value: TStringList);
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    if Assigned(ItemList.Objects[I]) then
      ItemList.Objects[I].Free;
  inherited SetItemList(Value);
end;

//=== { TJvTimeParameter } ===================================================

constructor TJvTimeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

procedure TJvTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvTimeParameter then
    Format := TJvTimeParameter(Source).Format;
end;

function TJvTimeParameter.GetParameterNameExt: string;
begin
  Result := 'Time';
end;

procedure TJvTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateTimeControl(Self, AParameterParent, GetParameterName));
end;

procedure TJvTimeParameter.SetWinControlProperties;
var
  DynControlTime: IJvDynControlTime;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlTime, DynControlTime) then
    DynControlTime.ControlSetFormat(Format);
end;

//=== { TJvDateTimeParameter } ===============================================

constructor TJvDateTimeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

procedure TJvDateTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDateTimeParameter then
  begin
    Format := TJvDateTimeParameter(Source).Format;
    MaxDate := TJvDateTimeParameter(Source).MaxDate;
    MinDate := TJvDateTimeParameter(Source).MinDate;
  end;
end;

function TJvDateTimeParameter.GetParameterNameExt: string;
begin
  Result := 'DateTime';
end;

procedure TJvDateTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateDateTimeControl(Self, AParameterParent, GetParameterName));
end;

procedure TJvDateTimeParameter.SetWinControlProperties;
var
  DynControlDate: IJvDynControlDate;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDate, DynControlDate) then
  begin
    DynControlDate.ControlSetFormat(Format);
    DynControlDate.ControlSetMinDate(MinDate);
    DynControlDate.ControlSetMaxDate(MaxDate);
  end;
end;

//=== { TJvDateParameter } ===================================================

constructor TJvDateParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

procedure TJvDateParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDateParameter then
  begin
    Format := TJvDateParameter(Source).Format;
    MinDate := TJvDateParameter(Source).MinDate;
    MaxDate := TJvDateParameter(Source).MaxDate;
  end;
end;

function TJvDateParameter.GetParameterNameExt: string;
begin
  Result := 'Date';
end;

procedure TJvDateParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateDateControl(Self, AParameterParent, GetParameterName));
end;

procedure TJvDateParameter.SetWinControlProperties;
var
  DynControlDate: IJvDynControlDate;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDate, DynControlDate) then
  begin
    DynControlDate.ControlSetFormat(Format);
    DynControlDate.ControlSetMinDate(MinDate);
    DynControlDate.ControlSetMaxDate(MaxDate);
  end;
end;

//=== { TJvEditParameter } ===================================================

constructor TJvEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPasswordChar := #0;
  FEditMask := '';
  FLabelWidth := 0;
  FEditWidth := 0;
  LabelArrangeMode := lamBefore;
end;

procedure TJvEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvEditParameter then
  begin
    EditMask := TJvEditParameter(Source).EditMask;
    PasswordChar := TJvEditParameter(Source).PasswordChar;
  end;
end;

function TJvEditParameter.GetParameterNameExt: string;
begin
  Result := 'MaskEdit';
end;

procedure TJvEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  SetWinControl (DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName));
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

//=== { TJvButtonEditParameter } =============================================

function TJvButtonEditParameter.GetParameterNameExt: string;
begin
  Result := 'ButtonEdit';
end;

procedure TJvButtonEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  SetWinControl (DynControlEngine.CreateButtonEditControl(Self, AParameterParent, GetParameterName, FOnClick));
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvButtonEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvButtonEditParameter then
    OnClick := TJvButtonEditParameter(Source).OnClick;
end;

//=== { TJvNumberEditParameter } =============================================

procedure TJvNumberEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvNumberEditParameter then
    EditorType := TJvNumberEditParameter(Source).EditorType;
end;

//=== { TJvIntegerEditParameter } ============================================

constructor TJvIntegerEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  MinValue := Low(Integer);
  MaxValue := High(Integer);
  Increment := 10;
end;

procedure TJvIntegerEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    SetWinControl (DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName))
  else if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    SetWinControl (DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName))
  else
    SetWinControl (DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName));
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvIntegerEditParameter.SetWinControlProperties;
var
  ITmpSpin: IJvDynControlSpin;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
  begin
    ITmpSpin.ControlSetIncrement(Increment);
    ITmpSpin.ControlSetMinValue(MinValue);
    ITmpSpin.ControlSetMaxValue(MaxValue);
    ITmpSpin.ControlSetUseForInteger(True);
  end;
end;

procedure TJvIntegerEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvIntegerEditParameter then
  begin
    MinValue := TJvIntegerEditParameter(Source).MinValue;
    MaxValue := TJvIntegerEditParameter(Source).MaxValue;
  end;
end;

function TJvIntegerEditParameter.IsDataValid(const AData: Variant; var vMsg:
    String): Boolean;
var
  I: Integer;
begin
  Result := Inherited IsDataValid(AData, vMsg);
  if Result and (VarToStr(AData) <> '') then
  begin
    try
      I := AData;
      if (I < MinValue) or (I > MaxValue) then
      begin
        vMsg:= Format(RsErrParameterMustBeBetween, [Caption, AData, IntToStr(MinValue), IntToStr(MaxValue)]);
        Result := False;
      end;
    except
      Result := False;
      vMsg := Format(RsErrParameterIsNotAValidNumber, [Caption, AData]);
    end;
  end;
end;

//=== { TJvDoubleEditParameter } =============================================

constructor TJvDoubleEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  // (rom) please use better values here (see JclMath)
  MinValue := -1E38;
  MaxValue := 1E38;
  Increment := 100;
end;

procedure TJvDoubleEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    SetWinControl (DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName))
  else if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    SetWinControl (DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName))
  else
    SetWinControl (DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName));
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvDoubleEditParameter.SetWinControlProperties;
var
  ITmpSpin: IJvDynControlSpin;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
  begin
    ITmpSpin.ControlSetIncrement(Increment);
    ITmpSpin.ControlSetMinValue(MinValue);
    ITmpSpin.ControlSetMaxValue(MaxValue);
    ITmpSpin.ControlSetUseForInteger(True);
  end;
end;

procedure TJvDoubleEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDoubleEditParameter then
  begin
    MinValue := TJvDoubleEditParameter(Source).MinValue;
    MaxValue := TJvDoubleEditParameter(Source).MaxValue;
  end;
end;

function TJvDoubleEditParameter.IsDataValid(const AData: Variant; var vMsg:
    String): Boolean;
var
  I: Integer;
begin
  Result := Inherited IsDataValid(AData, vMsg);
  if Result and (VarToStr(AData) <> '') then
  begin
    try
      I := AData;
      if (I < MinValue) or (I > MaxValue) then
      begin
        vMsg:= Format(RsErrParameterMustBeBetween,
                      [Caption, AData, FloatToStr(MinValue), FloatToStr(MaxValue)]);
        Result := False;
      end;
    except
      Result := False;
      vMsg := Format(RsErrParameterIsNotAValidNumber, [Caption, AData]);
    end;
  end;
end;

//=== { TJvFileNameParameter } ===============================================

constructor TJvFileNameParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
  FDialogOptions := [ofHideReadOnly, ofEnableSizing];
end;

procedure TJvFileNameParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvFileNameParameter then
  begin
    DialogKind := TJvFileNameParameter(Source).DialogKind;
    DefaultExt := TJvFileNameParameter(Source).DefaultExt;
    Filter := TJvFileNameParameter(Source).Filter;
    FilterIndex := TJvFileNameParameter(Source).FilterIndex;
    InitialDir := TJvFileNameParameter(Source).InitialDir;
    DialogOptions := TJvFileNameParameter(Source).DialogOptions;
    DialogTitle := TJvFileNameParameter(Source).DialogTitle;
  end;
end;

function TJvFileNameParameter.GetParameterNameExt: string;
begin
  Result := 'FileNameEdit';
end;

procedure TJvFileNameParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateFileNameControl(Self, AParameterParent, GetParameterName));
end;

function TJvFileNameParameter.IsDataValid(const AData: Variant; var vMsg:
    String): Boolean;
var
  Data : Variant;
begin
  Data := Trim(AData);
  if Data = DefaultExt then
    Data := '';
  Result := Inherited IsDataValid(Data, vMsg);
  if Result then
  begin
    if Data <> '' then
      if ExtractFileExt(Data) = '' then
        if DefaultExt <> '' then
          if DefaultExt[1] = '.' then
            Data := Data + DefaultExt
          else
            Data := Data + '.' + DefaultExt;
    if (ofFileMustExist in DialogOptions) and not FileExists(Data) then
      begin
        vMsg := Format(RsErrParameterFileDoesNotExist, [Caption, Data]);
        Result := False;
      end
    else if (ofPathMustExist in DialogOptions) and
            (ExtractFilePath(Data) <> '') and
            not {$IFDEF RTL220_UP}SysUtils.{$ENDIF RTL220_UP}DirectoryExists(ExtractFilePath(Data)) then
      begin
        vMsg := Format(RsErrParameterDirectoryNotExist, [Caption, ExtractFilePath(Data)]);
        Result:= False;
      end;
  end;
end;

procedure TJvFileNameParameter.SetWinControlProperties;
var
  ITmpControlFileName: IJvDynControlFileName;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlFileName, ITmpControlFileName) then
  begin
    ITmpControlFileName.ControlSetDialogKind(DialogKind);
    ITmpControlFileName.ControlSetDefaultExt(DefaultExt);
    ITmpControlFileName.ControlSetFilter(Filter);
    ITmpControlFileName.ControlSetFilterIndex(FilterIndex);
    ITmpControlFileName.ControlSetInitialDir(InitialDir);
    ITmpControlFileName.ControlSetDialogOptions(DialogOptions);
    ITmpControlFileName.ControlSetDialogTitle(DialogTitle);
  end;
end;

function TJvFileNameParameter.Validate(var AData: Variant): Boolean;
begin
  Result := Inherited Validate(AData);
  if Result then
    if (ofOverwritePrompt in DialogOptions) and FileExists(AData) then
      if DSADialogsMessageDlg(Format(RsErrParameterFileExistOverwrite, [Caption, AData]), mtConfirmation, [mbYes,
        mbNo], 0) = mrNo then
        Result := False;
end;

//=== { TJvDirectoryParameter } ==============================================

constructor TJvDirectoryParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

procedure TJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDirectoryParameter then
  begin
    InitialDir := TJvDirectoryParameter(Source).InitialDir;
    DialogOptions := TJvDirectoryParameter(Source).DialogOptions;
    DialogTitle := TJvDirectoryParameter(Source).DialogTitle;
  end;
end;

function TJvDirectoryParameter.GetParameterNameExt: string;
begin
  Result := 'DirectoryEdit';
end;

procedure TJvDirectoryParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateDirectoryControl(Self, AParameterParent, GetParameterName));
end;

function TJvDirectoryParameter.IsDataValid(const AData: Variant; var vMsg:
    String): Boolean;
var
  Data : Variant;
begin
  Data := Trim(AData);
  Result := Inherited IsDataValid(Data, vMsg);
  if Result then
    if not {$IFDEF RTL220_UP}SysUtils.{$ENDIF RTL220_UP}DirectoryExists(AData) and not (sdAllowCreate in DialogOptions) then
      begin
        vMsg := Format(RsErrParameterDirectoryNotExist, [Caption, AData]);
        Result := False;
      end;
end;

procedure TJvDirectoryParameter.SetWinControlProperties;
var
  ITmpControlDirectory: IJvDynControlDirectory;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDirectory, ITmpControlDirectory) then
  begin
    ITmpControlDirectory.ControlSetDialogTitle(DialogTitle);
    ITmpControlDirectory.ControlSetDialogOptions(DialogOptions);
    ITmpControlDirectory.ControlSetInitialDir(InitialDir);
  end;
end;

///=== { TJvMemoParameter } ==================================================

constructor TJvMemoParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars := ssNone;
  WantTabs := False;
  WantReturns := True;
  WordWrap := False;
end;

function TJvMemoParameter.GetParameterNameExt: string;
begin
  Result := 'Memo';
end;

procedure TJvMemoParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateMemoControl(Self, AParameterParent, GetParameterName));
end;

procedure TJvMemoParameter.SetFontName(const Value: string);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    SetWinControlProperties;
  end;
end;

procedure TJvMemoParameter.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    SetWinControlProperties;
  end;
end;

procedure TJvMemoParameter.SetWantReturns(const Value: Boolean);
begin
  if FWantReturns <> Value then
  begin
    FWantReturns := Value;
    SetWinControlProperties;
  end;
end;

procedure TJvMemoParameter.SetWantTabs(const Value: Boolean);
begin
  if FWantTabs <> Value then
  begin
    FWantTabs := Value;
    SetWinControlProperties;
  end;
end;

procedure TJvMemoParameter.SetWinControlProperties;
var
  ITmpMemo: IJvDynControlMemo;
  ITmpFont: IJvDynControlFont;
begin
  inherited SetWinControlProperties;
  if FontName <> '' then
    if Supports(WinControl, IJvDynControlFont, ITmpFont) then
      ITmpFont.ControlFont.Name := FontName;
  if Supports(WinControl, IJvDynControlMemo, ITmpMemo) then
  begin
    ITmpMemo.ControlSetWantTabs(WantTabs);
    ITmpMemo.ControlSetWantReturns(WantReturns);
    ITmpMemo.ControlSetWordWrap(WordWrap);
    ITmpMemo.ControlSetScrollbars(ScrollBars);
  end;
end;

procedure TJvMemoParameter.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    SetWinControlProperties;
  end;
end;

///=== { TJvRichEditParameter } ==============================================

constructor TJvRichEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars := ssNone;
  WantTabs := False;
  WantReturns := True;
  WordWrap := False;
end;

function TJvRichEditParameter.GetParameterNameExt: string;
begin
  Result := 'RichEdit';
end;

procedure TJvRichEditParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateRichEditControl(Self, AParameterParent, GetParameterName));
end;

procedure TJvRichEditParameter.SetWinControlProperties;
var
  ITmpMemo: IJvDynControlMemo;
  ITmpFont: IJvDynControlFont;
begin
  inherited SetWinControlProperties;
  if FontName <> '' then
    if Supports(WinControl, IJvDynControlFont, ITmpFont) then
      ITmpFont.ControlFont.Name := FontName;
  if Supports(WinControl, IJvDynControlMemo, ITmpMemo) then
  begin
    ITmpMemo.ControlSetWantTabs(WantTabs);
    ITmpMemo.ControlSetWantReturns(WantReturns);
    ITmpMemo.ControlSetWordWrap(WordWrap);
    ITmpMemo.ControlSetScrollbars(ScrollBars);
  end;
end;

///=== { TJvPageControlParameter } ==============================================

constructor TJvPageControlParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fHotTrack := True;
  fMultiline := True;
  fScrollOpposite := True;
  fTabIndex := 0;
  FRaggedRight := False;
  FPages := TStringList.Create;
end;

destructor TJvPageControlParameter.Destroy;
begin
  FreeAndNil(FPages);
  inherited Destroy;
end;

procedure TJvPageControlParameter.ArrangeControls;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
  w, h: Integer;
  c: TWinControl;
begin
  w := 0;
  h := 0;
  for i := 0 to Pages.Count - 1 do
  begin
    c := PageWinControl(i);
    if Supports(c, IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.ArrangeControls;
    if (ArrangeSettings.AutoSize in [asWidth, asBoth]) then
      if c.Width > w then
        w := c.Width;
    if (ArrangeSettings.AutoSize in [asHeight, asBoth]) then
      if c.Height > h then
        h := c.Height;
  end;
  if (ArrangeSettings.AutoSize in [asWidth, asBoth])
    and (w <> WinControl.Width) then
    WinControl.Width := w;
  if (ArrangeSettings.AutoSize in [asHeight, asBoth])
    and (h <> WinControl.Height) then
    WinControl.Height := h;
end;

procedure TJvPageControlParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvPageControlParameter then
  begin
    HotTrack := TJvPageControlParameter(Source).HotTrack;
    Multiline := TJvPageControlParameter(Source).Multiline;
    ScrollOpposite := TJvPageControlParameter(Source).Scrollopposite;
    TabIndex := TJvPageControlParameter(Source).TabIndex;
    RaggedRight := TJvPageControlParameter(Source).RaggedRight;
    Pages.Assign(TJvPageControlParameter(Source).Pages);
  end;
end;

procedure TJvPageControlParameter.CreateWinControlOnParent(ParameterParent:
  TWinControl);
var
  i: Integer;
  ITmpPageControl: IJvDynControlPageControl;
  Scrollbox: TScrollBox;
  Panel: TJvPanel;
begin
  SetWinControl (DynControlEngine.CreatePageControlControl(Self, ParameterParent, GetParameterName, Pages));
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
  Supports(WinControl, IJvDynControlPageControl, ITmpPageControl);
  for i := 0 to Pages.Count - 1 do
  begin
    Scrollbox := TScrollbox.Create(ParameterParent.Owner);
    Scrollbox.Parent := ITmpPageControl.ControlGetPage(Pages[i]);
    Scrollbox.Align := alClient;
    ScrollBox.AutoScroll := False;
    ScrollBox.BorderStyle := bsNone;
    {$IFDEF COMPILER10_UP}
    ScrollBox.ParentBackground := True;
    {$ENDIF COMPILER10_UP}
    Panel := TJvPanel.Create(ParameterParent.Owner);
    Panel.Name := GenerateUniqueComponentName(ParameterParent.Owner, Panel, GetParameterName + '_' + Pages[i]);
    Panel.ArrangeSettings := ArrangeSettings;
    Panel.BevelInner := bvNone;
    Panel.BevelOuter := bvNone;
    Panel.Parent := Scrollbox;
    Panel.Align := alTop;
    Panel.Visible := True;
    Panel.Caption := '';
    Panel.Color := Color;
    Panel.OnResizeParent := RearrangePageControl;
    Panel.Parent := Scrollbox;
    Pages.Objects[i] := Panel;
  end;
end;

procedure TJvPageControlParameter.DisableArrange;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
begin
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.DisableArrange;
end;

procedure TJvPageControlParameter.EnableArrange;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
begin
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.EnableArrange;
end;

function TJvPageControlParameter.GetParameterNameExt: string;
begin
  Result := 'PageControl';
end;

function TJvPageControlParameter.PageWinControl(Index: Integer): TWinControl;
begin
  if Assigned(Pages.Objects[Index]) and (Pages.Objects[Index] is TWinControl) then
    Result := TWinControl(Pages.Objects[Index])
  else
    Result := nil;
end;

procedure TJvPageControlParameter.RearrangePageControl(Sender: TObject; nLeft,
  nTop, nWidth, nHeight: Integer);
begin
  if Assigned(Sender) and (Sender is TWinControl) then
  begin
    if (ArrangeSettings.AutoSize in [asWidth, asBoth])
      and (TWinControl(Sender).Width <> nWidth + PPIScale(5)) then
      TWinControl(Sender).Width := nWidth + PPIScale(5);
    if (ArrangeSettings.AutoSize in [asHeight, asBoth])
      and (TWinControl(Sender).Height <> nHeight + PPIScale(45)) then
      TWinControl(Sender).Height := nHeight + PPIScale(45);
  end;
end;

procedure TJvPageControlParameter.SetPages(Value: TStringList);
begin
  FPages.Assign(Value);
end;

procedure TJvPageControlParameter.SetWinControlProperties;
var
  ITmpTabControl: IJvDynControlTabControl;
  ITmpArrangePanel: IJvArrangePanel;
  i: Integer;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlTabControl, ITmpTabControl) then
  begin
    ITmpTabControl.ControlSetRaggedRight(RaggedRight);
    ITmpTabControl.ControlSetMultiline(Multiline);
    ITmpTabControl.ControlSetScrollOpposite(ScrollOpposite);
    ITmpTabControl.ControlSetHotTrack(HotTrack);
  end;
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;

//=== { TJvCheckComboBoxParameter } ===============================================

constructor TJvCheckComboBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
  FSorted := False;
  FDelimiter := ';';
end;

procedure TJvCheckComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvCheckComboBoxParameter then
  begin
    Sorted := TJvCheckComboBoxParameter(Source).Sorted;
    Delimiter := TJvCheckComboBoxParameter(Source).Delimiter;
  end;
end;

function TJvCheckComboBoxParameter.GetParameterNameExt: string;
begin
  Result := 'CheckComboBox';
end;

procedure TJvCheckComboBoxParameter.GetData;
begin
  if Assigned(WinControl) then
    Value := WinControlData;
end;

procedure TJvCheckComboBoxParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := Value;
end;

procedure TJvCheckComboBoxParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  SetWinControl (DynControlEngine.CreateCheckComboBoxControl(Self, AParameterParent,
    GetParameterName, ItemList, Delimiter));
end;

procedure TJvCheckComboBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
  ITmpCheckComboBox: IJvDynControlCheckComboBox;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
  if Supports(WinControl, IJvDynControlCheckComboBox, ITmpCheckComboBox) then
    ITmpCheckComboBox.ControlSetDelimiter(Delimiter);
end;

function TJvCheckComboBoxParameter.GetWinControlData: Variant;
begin
  if Assigned(JvDynControlData) then
    Result := JvDynControlData.ControlValue
  else
    Result := null;
end;

procedure TJvCheckComboBoxParameter.SetWinControlData(Value: Variant);
begin
  if Assigned(JvDynControlData) then
    JvDynControlData.ControlValue := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
