{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}

unit SynCompletionProposal;

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  SynEditTypes,
  SynEditKeyCmds,
  SynEdit,
  SynUnicode;

type
  SynCompletionType = (ctCode, ctHint, ctParams);

  TSynBaseCompletionProposalPaintItem = procedure(Sender: TObject;
    Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
    var CustomDraw: Boolean) of object;

  TSynBaseCompletionProposalMeasureItem = procedure(Sender: TObject;
    Index: Integer; TargetCanvas: TCanvas; var ItemWidth: Integer) of object;

  TCodeCompletionEvent = procedure(Sender: TObject; var Value: string;
    Shift: TShiftState; Index: Integer; EndToken: WideChar) of object;

  TAfterCodeCompletionEvent = procedure(Sender: TObject; const Value: string;
    Shift: TShiftState; Index: Integer; EndToken: WideChar) of object;

  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState;
    EndToken: WideChar) of object;

  TCompletionParameter = procedure(Sender: TObject; Key: WideChar;
    var CurrentIndex, Level: Integer) of object;

  TCompletionExecute = procedure(Kind: SynCompletionType; Sender: TObject;
    var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean) of object;

  TCompletionChange = procedure(Sender: TObject; AIndex: Integer) of object;

  TCodeItemInfo = procedure(Sender: TObject; AIndex: Integer; var Info : string) of object;

  TSynCompletionOption = (scoCaseSensitive,         //Use case sensitivity to do matches
                          scoLimitToMatchedText,    //Limit the matched text to only what they have typed in
                          scoTitleIsCentered,       //Center the title in the box if you choose to use titles
                          scoUseInsertList,         //Use the InsertList to insert text instead of the ItemList (which will be displayed)
                          scoUsePrettyText,         //Use the PrettyText function to output the words
                          scoUseBuiltInTimer,       //Use the built in timer and the trigger keys to execute the proposal as well as the shortcut
                          scoEndCharCompletion,     //When an end char is pressed, it triggers completion to occur (like the Delphi IDE)
                          scoConsiderWordBreakChars,//Use word break characters as additional end characters
                          scoCompleteWithTab,       //Use the tab character for completion
                          scoCompleteWithEnter);    //Use the Enter character for completion

  TSynCompletionOptions = set of TSynCompletionOption;


const
  DefaultProposalOptions = [scoLimitToMatchedText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  DefaultEndOfTokenChr = '()[]. ';

type
  TProposalColumns = class;

  TSynBaseCompletionProposalForm = class(TCustomForm)
  private
    FCurrentString: string;
    FOnPaintItem: TSynBaseCompletionProposalPaintItem;
    FOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
    FOnChangePosition: TCompletionChange;
    FOnCodeItemInfo: TCodeItemInfo;
    FItemList: TStrings;
    FInsertList: TStrings;
    FAssignedList: TStrings;
    FPosition: Integer;
    FLinesInWindow: Integer;
    FFontHeight: integer;
    FScrollbar: TScrollBar;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FClSelect: TColor;
    fClSelectText: TColor;
    FClTitleBackground: TColor;
    fClBackGround: TColor;
    FPaintBitmap: TBitmap; // used for drawing
    FTitleBitmap: TBitmap; // used for title-drawing
    FCurrentEditor: TCustomSynEdit;
    FTitle: string;
    FTitleFont: TFont;
    FFont: TFont;
    FResizeable: Boolean;
    FItemHeight: Integer;
    FMargin: Integer;
    FEffectiveItemHeight: Integer;
    FImages: TCustomImageList;

    //These are the reflections of the Options property of the CompletionProposal
    FCase: boolean;
    FMatchText: Boolean;
    FFormattedText: Boolean;
    FCenterTitle: Boolean;
    FUseInsertList: boolean;
    FCompleteWithTab: Boolean;
    FCompleteWithEnter: Boolean;

    FMouseWheelAccumulator: integer;
    FDisplayKind: SynCompletionType;
    FOnParameterToken: TCompletionParameter;
    FCurrentIndex: Integer;
    FCurrentLevel: Integer;
    FDefaultKind: SynCompletionType;
    FEndOfTokenChr: string;
    FTriggerChars: string;
    OldShowCaret: Boolean;
    FTitleHeight: Integer;
    FColumns: TProposalColumns;
    FGripperHeight : integer;
    FGripperText : string;
    FGripperFont : TFont;

    FScaledMargin : integer;
    procedure SetCurrentString(const Value: string);
    procedure MoveLine(cnt: Integer);
    procedure ScrollbarOnChange(Sender: TObject);
    procedure ScrollbarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollbarOnEnter(Sender: TObject);

    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetResizeable(const Value: Boolean);
    procedure SetItemHeight(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure StringListChange(Sender: TObject);
    procedure DoDoubleClick(Sender : TObject);
    procedure DoFormShow(Sender: TObject);
    procedure DoFormHide(Sender: TObject);
    procedure AdjustScrollBarPosition;
    procedure AdjustMetrics;
    procedure SetTitle(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetColumns(Value: TProposalColumns);
    procedure RecalcItemHeight;
    function IsWordBreakChar(AChar: WideChar): Boolean;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure ResetCanvas(const Canvas: TCanvas);
    procedure SetGripperText(const Value: string);
    procedure SetGripperFont(const Value: TFont);
  protected
    const cGripperBarHeight = 16;
    var FCodeItemInfoWindow : THintWindow;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure PaintTitle;
    procedure PaintCodeItems;
    procedure PaintHint;
    procedure PaintGripper;
    procedure PaintParams;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMActivate (var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ShowCodeItemInfo(const Info: string);
    function GetCurrentPPI: Integer;{$IF CompilerVersion >= 32}override;{$ENDIF}
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;

    function LogicalToPhysicalIndex(Index: Integer): Integer;
    function PhysicalToLogicalIndex(Index: Integer): Integer;

    property DisplayType: SynCompletionType read FDisplayKind write FDisplayKind;
    property DefaultType: SynCompletionType read FDefaultKind write FDefaultKind default ctCode;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
    property CurrentLevel: Integer read FCurrentLevel write FCurrentLevel;
    property OnParameterToken: TCompletionParameter read FOnParameterToken write FOnParameterToken;
    property OnKeyPress;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem read FOnPaintItem write FOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionProposalMeasureItem read FOnMeasureItem write FOnMeasureItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList: TStrings read FItemList write SetItemList;
    property InsertList: TStrings read FInsertList write SetInsertList;
    property AssignedList: TStrings read FAssignedList write FAssignedList;
    property Position: Integer read FPosition write SetPosition;
    property Title: string read fTitle write SetTitle;
    property GripperText : string read FGripperText write SetGripperText;
    property ClSelect: TColor read FClSelect write FClSelect default clHighlight;
    property ClSelectedText: TColor read FClSelectText write FClSelectText default clHighlightText;
    property ClBackground: TColor read FClBackGround write FClBackGround default clWindow;
    property ClTitleBackground: TColor read FClTitleBackground write FClTitleBackground default clBtnFace;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property Margin: Integer read FMargin write FMargin default 2;

    property UsePrettyText: boolean read FFormattedText write FFormattedText default False;
    property UseInsertList: boolean read FUseInsertList write FUseInsertList default False;
    property CenterTitle: boolean read FCenterTitle write FCenterTitle   default True;
    property CaseSensitive: Boolean read fCase write fCase default False;
    property CurrentEditor: TCustomSynEdit read fCurrentEditor write fCurrentEditor;
    property MatchText: Boolean read fMatchText write fMatchText;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property TriggerChars: string read FTriggerChars write FTriggerChars;
    property CompleteWithTab: Boolean read FCompleteWithTab write FCompleteWithTab;
    property CompleteWithEnter: Boolean read FCompleteWithEnter write FCompleteWithEnter;

    property TitleFont: TFont read fTitleFont write SetTitleFont;
    property Font: TFont read fFont write SetFont;  // hides inherited property - not auto scaled
    property GripperFont : TFont read FGripperFont write SetGripperFont;
    property Columns: TProposalColumns read FColumns write SetColumns;
    property Resizeable: Boolean read FResizeable write SetResizeable;
    property Images: TCustomImageList read FImages write SetImages;
  end;

  TSynBaseCompletionProposal = class(TComponent)
  private
    FForm: TSynBaseCompletionProposalForm;
    FOnExecute: TCompletionExecute;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FWidth: Integer;
    FPreviousToken: string;
    FDotOffset: Integer;
    FOptions: TSynCompletionOptions;
    FNbLinesInWindow: Integer;
    FFontsAreScaled: Boolean;
    FPaintFormShadow: Boolean;
    FShowGripper: Boolean;
    FFormatParams : Boolean;

    FCanExecute: Boolean;
    function GetClSelect: TColor;
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;
    function GetItemList: TStrings;
    function GetInsertList: TStrings;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
    function GetOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionProposalPaintItem);
    procedure SetOnMeasureItem(const Value: TSynBaseCompletionProposalMeasureItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    procedure SetWidth(Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    function GetDisplayKind: SynCompletionType;
    procedure SetDisplayKind(const Value: SynCompletionType);
    function GetParameterToken: TCompletionParameter;
    procedure SetParameterToken(const Value: TCompletionParameter);
    function GetDefaultKind: SynCompletionType;
    procedure SetDefaultKind(const Value: SynCompletionType);
    function GetClBack: TColor;
    procedure SetClBack(const Value: TColor);
    function GetClSelectedText: TColor;
    procedure SetClSelectedText(const Value: TColor);
    function GetEndOfTokenChar: string;
    procedure SetEndOfTokenChar(const Value: string);
    function GetClTitleBackground: TColor;
    procedure SetClTitleBackground(const Value: TColor);
    procedure SetTitle(const Value: string);
    function GetTitle: string;
    function GetFont: TFont;
    function GetTitleFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetGripperFont(const Value : TFont);
    function GetOptions: TSynCompletionOptions;
    function GetTriggerChars: string;
    procedure SetTriggerChars(const Value: string);
    function GetOnChange: TCompletionChange;
    procedure SetOnChange(const Value: TCompletionChange);
    function GetOnCodeItemInfo: TCodeItemInfo;
    procedure SetOnCodeItemInfo(const Value: TCodeItemInfo);
    procedure SetColumns(const Value: TProposalColumns);
    function GetColumns: TProposalColumns;
    function GetResizeable: Boolean;
    procedure SetResizeable(const Value: Boolean);
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    function GetMargin: Integer;
    procedure SetMargin(const Value: Integer);
    function GetImages: TCustomImageList;
    function IsWordBreakChar(AChar: WideChar): Boolean;
    procedure SetPaintFormShadow(const Value: Boolean);
    procedure SetGripperText(const Value: string);
    function GetGripperFont: TFont;
    function GetGripperText: string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetOptions(const Value: TSynCompletionOptions); virtual;
    procedure EditorCancelMode(Sender: TObject); virtual;
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: Pointer; HandlerData: Pointer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(s: string; x, y: Integer);
    procedure ExecuteEx(s: string; x, y: Integer; Kind: SynCompletionType = ctCode); virtual;
    procedure Activate;
    procedure Deactivate;

    procedure ClearList;
    function DisplayItem(AIndex: Integer): string;
    function InsertItem(AIndex: Integer): string;
    procedure AddItemAt(Where: Integer; ADisplayText, AInsertText: string);
    procedure AddItem(ADisplayText, AInsertText: string);
    procedure ResetAssignedList;

    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    property DotOffset: Integer read FDotOffset write FDotOffset;
    property DisplayType: SynCompletionType read GetDisplayKind write SetDisplayKind;
    property Form: TSynBaseCompletionProposalForm read FForm;
    property PreviousToken: string read FPreviousToken;
    property Position: Integer read GetPosition write SetPosition;
    property FontsAreScaled: Boolean read fFontsAreScaled write fFontsAreScaled;
    property FormatParams : boolean read fFormatParams write fFormatParams;
  published
    property DefaultType: SynCompletionType read GetDefaultKind write SetDefaultKind default ctCode;
    property Options: TSynCompletionOptions read GetOptions write SetOptions default DefaultProposalOptions;

    property ItemList: TStrings read GetItemList write SetItemList;
    property InsertList: TStrings read GetInsertList write SetInsertList;
    property NbLinesInWindow: Integer read FNbLinesInWindow write SetNbLinesInWindow default 8;
    property ClSelect: TColor read GetClSelect write SetClSelect default clHighlight;
    property ClSelectedText: TColor read GetClSelectedText write SetClSelectedText default clHighlightText;
    property ClBackground: TColor read GetClBack write SetClBack default clWindow;
    property ClTitleBackground: TColor read GetClTitleBackground write SetClTitleBackground default clBtnFace;
    property Width: Integer read FWidth write SetWidth default 260;
    property EndOfTokenChr: string read GetEndOfTokenChar write SetEndOfTokenChar;
    property TriggerChars: string read GetTriggerChars write SetTriggerChars;
    property Title: string read GetTitle write SetTitle;
    property Font: TFont read GetFont write SetFont;
    property TitleFont: TFont read GetTitleFont write SetTitleFont;
    property GripperFont : TFont read GetGripperFont write SetGripperFont;
    property Columns: TProposalColumns read GetColumns write SetColumns;
    property Resizeable: Boolean read GetResizeable write SetResizeable default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight default 0;
    property Images: TCustomImageList read GetImages write SetImages default nil;
    property Margin: Integer read GetMargin write SetMargin default 2;
    property PaintFormShadow: Boolean read FPaintFormShadow write SetPaintFormShadow default True;
    property ShowGripper: Boolean read FShowGripper write FShowGripper default False;
    property GripperText : string read GetGripperText write SetGripperText;

    property OnChange: TCompletionChange read GetOnChange write SetOnChange;
    property OnCodeItemInfo: TCodeItemInfo read GetOnCodeItemInfo write SetOnCodeItemInfo;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnExecute: TCompletionExecute read FOnExecute write FOnExecute;
    property OnMeasureItem: TSynBaseCompletionProposalMeasureItem read GetOnMeasureItem write SetOnMeasureItem;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem read GetOnPaintItem write SetOnPaintItem;
    property OnParameterToken: TCompletionParameter read GetParameterToken write SetParameterToken;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TSynCompletionProposal = class(TSynBaseCompletionProposal)
  private
    fEditors: TList;
    FShortCut: TShortCut;
    FNoNextKey: Boolean;
    FCompletionStart: Integer;
    FAdjustCompletionStart: Boolean;
    FOnCodeCompletion: TCodeCompletionEvent;
    FTimer: TTimer;
    FTimerInterval: Integer;
    FEditor: TCustomSynEdit;
    FOnAfterCodeCompletion: TAfterCodeCompletionEvent;
    FOnCancelled: TNotifyEvent;
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure HandleOnCancel(Sender: TObject);
    procedure HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: WideChar);
    procedure HandleOnKeyPress(Sender: TObject; var Key: WideChar);
    procedure HandleDblClick(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: WideChar);
    procedure TimerExecute(Sender: TObject);
    function GetPreviousToken(AEditor: TCustomSynEdit): string;
    function GetCurrentInput(AEditor: TCustomSynEdit): string;
    function GetTimerInterval: Integer;
    procedure SetTimerInterval(const Value: Integer);
    function GetEditor(i: Integer): TCustomSynEdit;
    procedure InternalCancelCompletion;
  protected
    procedure DoExecute(AEditor: TCustomSynEdit); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetShortCut(Value: TShortCut);
    procedure SetOptions(const Value: TSynCompletionOptions); override;
    procedure EditorCancelMode(Sender: TObject); override;
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: Pointer; HandlerData: Pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEditor(AEditor: TCustomSynEdit);
    function RemoveEditor(AEditor: TCustomSynEdit): boolean;
    function EditorsCount: integer;
    procedure ExecuteEx(s: string; x, y: Integer; Kind : SynCompletionType = ctCode); override;
    procedure ActivateCompletion;
    procedure CancelCompletion;
    procedure ActivateTimer(ACurrentEditor: TCustomSynEdit);
    procedure DeactivateTimer;
    property Editors[i: Integer]: TCustomSynEdit read GetEditor;
    property CompletionStart: Integer read FCompletionStart write FCompletionStart; // ET 04/02/2003
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval default 1000;

    property OnAfterCodeCompletion: TAfterCodeCompletionEvent read FOnAfterCodeCompletion write FOnAfterCodeCompletion;
    property OnCancelled: TNotifyEvent read FOnCancelled write FOnCancelled;
    property OnCodeCompletion: TCodeCompletionEvent read FOnCodeCompletion write FOnCodeCompletion;
  end;

  TSynAutoComplete = class(TComponent)
  private
    FShortCut: TShortCut;
    fEditor: TCustomSynEdit;
    fAutoCompleteList: TStrings;
    fNoNextKey : Boolean;
    FEndOfTokenChr: string;
    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;
    FInternalCompletion: TSynCompletionProposal;
    FDoLookup: Boolean;
    FOptions: TSynCompletionOptions;
    procedure SetAutoCompleteList(List: TStrings);
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetDoLookup(const Value: Boolean);
    procedure CreateInternalCompletion;
    function GetOptions: TSynCompletionOptions;
    procedure SetOptions(const Value: TSynCompletionOptions);
    procedure DoInternalAutoCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    function GetExecuting: Boolean;
  protected
    procedure SetShortCut(Value: TShortCut);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditorKeyPress(Sender: TObject; var Key: WideChar); virtual;
  public
    function GetCompletionProposal : TSynCompletionProposal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(Token: string; Editor: TCustomSynEdit);
    procedure ExecuteEx(Token: string; Editor: TCustomSynEdit; LookupIfNotExact: Boolean);
    function GetTokenList: string;
    function GetTokenValue(Token: string): string;
    function GetPreviousToken(Editor: TCustomSynEdit): string;
    procedure CancelCompletion;
    property Executing: Boolean read GetExecuting;
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property DoLookupWhenNotExact: Boolean read FDoLookup write SetDoLookup default true;
    property Options: TSynCompletionOptions read GetOptions write SetOptions default DefaultProposalOptions;
  end;

  TProposalColumn = class(TCollectionItem)
  private
    FColumnWidth: Integer;
    FInternalWidth: Integer;
    FFontStyle: TFontStyles;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ColumnWidth: Integer read FColumnWidth write FColumnWidth;
    property DefaultFontStyle: TFontStyles read FFontStyle write FFontStyle default [];
  end;

  TProposalColumns = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TProposalColumn;
    procedure SetItem(Index: Integer; Value: TProposalColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    function Add: TProposalColumn;
    function FindItemID(ID: Integer): TProposalColumn;
    function Insert(Index: Integer): TProposalColumn;
    property Items[Index: Integer]: TProposalColumn read GetItem write SetItem; default;
  end;


procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect;  PPI: Integer;
  const Text: string; Selected: Boolean; Columns: TProposalColumns; Images: TCustomImageList);
function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string;  PPI: Integer;
  Columns: TProposalColumns; Images: TCustomImageList): Integer;
function PrettyTextToFormattedString(const APrettyText: string;
  AlternateBoldStyle: Boolean = False): string;
(*
   GetParameter extracts a parameter from a formatted function parameter string
   Parameter lists can contain default values which may  be strings
   Ignores commas in strings and brackets
   Python parameter lists can contain type hints which are arbitrary expressions
*)
function GetParameter(var S: string): string;

implementation

uses
  System.Math,
  Vcl.Themes,
  System.Types,
  System.UITypes,
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynEditKeyConst,
  JvGnugettext;

const
  TextHeightString = 'CompletionProposal';

//------------------------- Formatted painting stuff ---------------------------

type
  TFormatCommand = (fcNoCommand, fcColor, fcStyle, fcColumn, fcHSpace, fcImage);
  TFormatCommands = set of TFormatCommand;

  PFormatChunk = ^TFormatChunk;
  TFormatChunk = record
    Str: string;
    Command: TFormatCommand;
    Data: Pointer;
  end;

  PFormatStyleData = ^TFormatStyleData;
  TFormatStyleData = record
    Style: WideChar;
    Action: Integer;    // -1 = Reset, +1 = Set, 0 = Toggle
  end;

  TFormatChunkList = class
  private
    FChunks: TList;
    function GetCount: Integer;
    function GetChunk(Index: Integer): PFormatChunk;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AChunk: PFormatChunk);
    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: PFormatChunk read GetChunk; default;
  end;


const
  AllCommands = [fcColor..High(TFormatCommand)];


function TFormatChunkList.GetCount: Integer;
begin
  Result := FChunks.Count;
end;

function TFormatChunkList.GetChunk(Index: Integer): PFormatChunk;
begin
  Result := FChunks[Index];
end;

procedure TFormatChunkList.Clear;
var
  C: PFormatChunk;
  StyleFormatData: PFormatStyleData;
begin
  while FChunks.Count > 0 do
  begin
    C := FChunks.Last;
    FChunks.Delete(FChunks.Count-1);

    case C^.Command of
    fcStyle:
      begin
        StyleFormatData := C^.Data;
        Dispose(StyleFormatData);
      end;
    end;

    Dispose(C);
  end;
end;

constructor TFormatChunkList.Create;
begin
  inherited Create;
  FChunks := TList.Create;
end;

destructor TFormatChunkList.Destroy;
begin
  Clear;
  FChunks.Free;
  inherited Destroy;
end;

procedure TFormatChunkList.Add(AChunk: PFormatChunk);
begin
  FChunks.Add(AChunk);
end;


function ParseFormatChunks(const FormattedString: string; ChunkList: TFormatChunkList;
  const StripCommands: TFormatCommands): Boolean;
var
  CurChar: WideChar;
  CurPos: Integer;
  CurrentChunk: string;
  PossibleErrorPos: Integer;
  ErrorFound: Boolean;

  procedure NextChar;
  begin
    inc(CurPos);
    {$IFOPT R+}
    // Work-around Delphi's annoying behaviour of failing the RangeCheck when
    // reading the final #0 char
    if CurPos = Length(FormattedString) +1 then
      CurChar := #0
    else
    {$ENDIF}
    CurChar := FormattedString[CurPos];
  end;

  procedure AddStringChunk;
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := CurrentChunk;
    C^.Command := fcNoCommand;
    C^.Data := nil;
    ChunkList.Add(C);

    CurrentChunk := '';
  end;

  procedure AddCommandChunk(ACommand: TFormatCommand; Data: Pointer);
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := '';
    C^.Command := ACommand;
    C^.Data := Data;
    ChunkList.Add(C);
  end;

  procedure ParseEscapeSequence;
  var
    Command: string;
    Parameter: string;
    CommandType: TFormatCommand;
    Data: Pointer;
  begin
    Assert(CurChar = '\');
    NextChar;
    if CurChar = '\' then
    begin
      CurrentChunk := CurrentChunk  + '\';
      NextChar;
      exit;
    end;

    if CurrentChunk <> '' then
      AddStringChunk;

    Command := '';
    while (CurChar <> '{') and (CurPos <= Length(FormattedString)) do
    begin
      Command := Command +CurChar;
      NextChar;
    end;

    if CurChar = '{' then
    begin
      PossibleErrorPos := CurPos;
      NextChar;
      Parameter := '';
      while (CurChar <> '}') and (CurPos <= Length(FormattedString)) do
      begin
        Parameter := Parameter + CurChar;
        NextChar;
      end;

      if CurChar = '}' then
      begin
        Command := System.SysUtils.AnsiUpperCase(Command);

        Data := nil;
        CommandType := fcNoCommand;

        if Command = 'COLOR' then
        begin
          try
            Data := Pointer(StringToColor(Parameter));
            CommandType := fcColor;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'COLUMN' then
        begin
          if Parameter <> '' then
          begin
            CommandType := fcNoCommand;
            ErrorFound := True;
          end else
            CommandType := fcColumn;
        end else
        if Command = 'HSPACE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcHSpace;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'IMAGE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcImage;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'STYLE' then
        begin
          if (Length(Parameter) = 2)
            and CharInSet(Parameter[1], ['+', '-', '~'])
            and CharInSet(System.SysUtils.AnsiUpperCase(Parameter[2])[1],
              ['B', 'I', 'U', 'S']) then
          begin
            CommandType := fcStyle;
            if not (fcStyle in StripCommands) then
            begin
              Data := New(PFormatStyleData);
              PFormatStyleData(Data)^.Style := System.SysUtils.AnsiUpperCase(Parameter[2])[1];
              case Parameter[1] of
              '+': PFormatStyleData(Data)^.Action := 1;
              '-': PFormatStyleData(Data)^.Action := -1;
              '~': PFormatStyleData(Data)^.Action := 0;
              end;
            end;
          end else
          begin
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
          ErrorFound := True;

        if (CommandType <> fcNoCommand) and (not (CommandType in StripCommands)) then
          AddCommandChunk(CommandType, Data);

        NextChar;
      end;
    end;
    Result := not ErrorFound;
  end;

  procedure ParseString;
  begin
    Assert(CurChar <> '\');
    while (CurChar <> '\') and (CurPos <= Length(FormattedString)) do
    begin
      CurrentChunk := CurrentChunk +CurChar;
      NextChar;
    end;
  end;

begin
  Assert(Assigned(ChunkList));

  if FormattedString = '' then
    exit;

  ErrorFound := False;
  CurrentChunk := '';
  CurPos := 1;
  CurChar := FormattedString[1];

  while CurPos <= Length(FormattedString) do
  begin
    if CurChar = '\' then
      ParseEscapeSequence
    else
      ParseString;
  end;

  if CurrentChunk <> '' then
    AddStringChunk;
end;


function StripFormatCommands(const FormattedString: string): string;
var
  Chunks: TFormatChunkList;
  i: Integer;
begin
  Chunks := TFormatChunkList.Create;
  try
    ParseFormatChunks(FormattedString, Chunks, AllCommands);

    Result := '';
    for i := 0 to Chunks.Count -1 do
      Result := Result + Chunks[i]^.Str;

  finally
    Chunks.Free;
  end;
end;


function PaintChunks(TargetCanvas: TCanvas; const Rect: TRect; PPI : integer;
  ChunkList: TFormatChunkList; Columns: TProposalColumns; Images: TCustomImageList;
  Invisible: Boolean): Integer;
var
  i: Integer;
  X: Integer;
  C: PFormatChunk;
  CurrentColumn: TProposalColumn;
  CurrentColumnIndex: Integer;
  LastColumnStart: Integer;
  Style: TFontStyles;
  OldFont: TFont;
begin
  OldFont := TFont.Create;
  try
    OldFont.Assign(TargetCanvas.Font);

    if Assigned(Columns) and (Columns.Count > 0) then
    begin
      CurrentColumnIndex := 0;
      CurrentColumn := TProposalColumn(Columns.Items[0]);
      TargetCanvas.Font.Style := CurrentColumn.FFontStyle;
    end else
    begin
      CurrentColumnIndex := -1;
      CurrentColumn := nil;
    end;

    LastColumnStart := Rect.Left;
    X := Rect.Left;

    TargetCanvas.Brush.Style := bsClear;

    for i := 0 to ChunkList.Count -1 do
    begin
      C := ChunkList[i];

      case C^.Command of
      fcNoCommand:
        begin
          if not Invisible then
            TargetCanvas.TextOut(X, Rect.Top, C^.Str);

          inc(X, TargetCanvas.TextWidth(C^.Str));
          if X > Rect.Right then
            break;
        end;
      fcColor:
        if not Invisible then
          TargetCanvas.Font.Color := StyleServices.GetSystemColor(TColor(C^.Data));
      fcStyle:
        begin
          case PFormatStyleData(C^.Data)^.Style of
          'I': Style := [fsItalic];
          'B': Style := [fsBold];
          'U': Style := [fsUnderline];
          'S': Style := [fsStrikeout];
          else Assert(False);
          end;


          case PFormatStyleData(C^.Data)^.Action of
          -1: TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          0: if TargetCanvas.Font.Style * Style = [] then
               TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style
             else
               TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          1: TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style;
          else Assert(False);
          end;
        end;
      fcColumn:
        if Assigned(Columns) and (Columns.Count > 0) then
        begin
          if CurrentColumnIndex <= Columns.Count -1 then
          begin
            inc(LastColumnStart, MulDiv(CurrentColumn.FColumnWidth, PPI, 96));
            X := LastColumnStart;

            inc(CurrentColumnIndex);
            if CurrentColumnIndex <= Columns.Count -1 then
            begin
              CurrentColumn := TProposalColumn(Columns.Items[CurrentColumnIndex]);
              TargetCanvas.Font.Style := CurrentColumn.FFontStyle;
            end else
              CurrentColumn := nil;
          end;
        end;
      fcHSpace:
        begin
          inc(X, MulDiv(Integer(C^.Data), PPI, 96));
          if X > Rect.Right then
            break;
        end;
      fcImage:
        begin
          Assert(Assigned(Images));

          Images.Draw(TargetCanvas, X, Rect.Top, Integer(C^.Data));

          inc(X, Images.Width);
          if X > Rect.Right then
            break;
        end;
      end;
    end;

    Result := X;
    TargetCanvas.Font.Assign(OldFont);
  finally
    OldFont.Free;
    TargetCanvas.Brush.Style := bsSolid;
  end;
end;

procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect; PPI: Integer;
  const Text: string; Selected: Boolean; Columns: TProposalColumns; Images: TCustomImageList);
var
  Chunks: TFormatChunkList;
  StripCommands: TFormatCommands;
begin
  Chunks := TFormatChunkList.Create;
  try
    if Selected then
      StripCommands := [fcColor]
    else
      StripCommands := [];

    ParseFormatChunks(Text, Chunks, StripCommands);
    PaintChunks(TargetCanvas, Rect, PPI, Chunks, Columns, Images, False);
  finally
    Chunks.Free;
  end;
end;

function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string; PPI: Integer;
  Columns: TProposalColumns; Images: TCustomImageList): Integer;
var
  Chunks: TFormatChunkList;
  TmpRect: TRect;
begin
  Chunks := TFormatChunkList.Create;
  try
    TmpRect := Rect(0, 0, MaxInt, MaxInt);

    ParseFormatChunks(Text, Chunks, [fcColor]);
    Result := PaintChunks(TargetCanvas, TmpRect, PPI, Chunks, Columns, Images, True);
  finally
    Chunks.Free;
  end;
end;

function PrettyTextToFormattedString(const APrettyText: string;
  AlternateBoldStyle: Boolean = False): string;
var
  i: Integer;
  Color: TColor;
Begin
  Result := '';
  i := 1;
  while i <= Length(APrettyText) do
    case APrettyText[i] of
      #1, #2:
        begin
          Color := (Ord(APrettyText[i + 3]) shl 8
            +Ord(APrettyText[i + 2])) shl 8
            +Ord(APrettyText[i + 1]);

          Result := Result+'\color{'+ColorToString(Color)+'}';

          inc(i, 4);
        end;
      #3:
        begin
          if CharInSet(System.SysUtils.AnsiUpperCase(APrettyText[i + 1])[1], ['B', 'I', 'U']) then
          begin
            Result := Result + '\style{';

            case APrettyText[i + 1] of
            'B': Result := Result + '+B';
            'b': Result := Result + '-B';
            'I': Result := Result + '+I';
            'i': Result := Result + '-I';
            'U': Result := Result + '+U';
            'u': Result := Result + '-U';
            end;

            Result := Result + '}';
          end;
          inc(i, 2);
        end;
      #9:
        begin
          Result := Result + '\column{}';
          if AlternateBoldStyle then
            Result := Result + '\style{~B}';
          inc(i);
        end;
      else
        Result := Result + APrettyText[i];
        inc(i);
    end;
end;


// TProposalColumn

constructor TProposalColumn.Create(Collection: TCollection);
begin
  inherited;
  FColumnWidth := 100;
  FInternalWidth := -1;
  FFontStyle := [];
end;

destructor TProposalColumn.Destroy;
begin
  inherited;
end;

procedure TProposalColumn.Assign(Source: TPersistent);
begin
  if Source is TProposalColumn then
  begin
    FColumnWidth := TProposalColumn(Source).FColumnWidth;
    FInternalWidth := TProposalColumn(Source).FInternalWidth;
    FFontStyle := TProposalColumn(Source).FFontStyle;
  end
  else
    inherited Assign(Source);
end;

procedure TProposalColumn.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

constructor TProposalColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TProposalColumns.GetItem(Index: Integer): TProposalColumn;
begin
  Result := inherited GetItem(Index) as TProposalColumn;
end;

procedure TProposalColumns.SetItem(Index: Integer; Value: TProposalColumn);
begin
  inherited SetItem(Index, Value);
end;

function TProposalColumns.Add: TProposalColumn;
begin
  Result := inherited Add as TProposalColumn;
end;

function TProposalColumns.FindItemID(ID: Integer): TProposalColumn;
begin
  Result := inherited FindItemID(ID) as TProposalColumn;
end;

function TProposalColumns.Insert(Index: Integer): TProposalColumn;
begin
  Result := inherited Insert(Index) as TProposalColumn;
end;



//============================================================================
function GetParameter(var S: string): string;
var
  Idx : integer;
  BracketCounter : integer;
  StringOpener : WideChar;
begin
  BracketCounter := 0;
  StringOpener := WideChar(0);
  for Idx := 1 to Length(S) do begin
    if StringOpener <> WideChar(0) then begin
      if S[Idx] = StringOpener then
        StringOpener := WideChar(0);
    end else if  Ord(S[Idx]) < 128 then begin
      if AnsiChar(S[Idx]) in ['''','"'] then
        StringOpener := S[Idx]
      else if AnsiChar(S[Idx]) in ['(','{','['] then
        Inc(BracketCounter)
      else if AnsiChar(S[Idx]) in [')','}',']'] then
        Dec(BracketCounter)
      else if (BracketCounter = 0) and (S[Idx] = ',') then begin
        Result := Copy (S, 1, Idx - 1);
        Delete(S, 1, Idx);
        Exit;
      end;
    end;
  end;
  Result := S;
  S := '';
end;

function FormatParamList(const Params: string; CurrentIndex: Integer): string;
var
  S, Param, TrimmedParam: string;
  i: Integer;
begin
  Result := '';
  i := 0;
  S := Params.Trim;
  Repeat
    Param := GetParameter(S);
    TrimmedParam := Param.Trim;
    if (TrimmedParam = '*') or (TrimmedParam = '/') then
    begin
      // Deal with keyword only or positional only python arguments
      Result := Result + Param + ',';
      Continue;
    end
    else
    if i = CurrentIndex then
      Result := Result + '\style{~B}' + Param + '\style{~B}'
    else
      Result := Result + Param;
    if S.Length > 0 then
      Result := Result + ',';
    Inc(i);
  Until S.Length = 0;
end;

{ TSynBaseCompletionProposalForm }

constructor TSynBaseCompletionProposalForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  BorderStyle := bsSingle;
  BorderWidth := 1;

  FPaintBitmap := TBitmap.Create;
  FTitleBitmap := TBitmap.Create;
  FItemList := TStringList.Create;
  FInsertList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FMatchText := False;

  FScrollbar := TScrollBar.Create(Self);
  FScrollbar.Kind := sbVertical;
  FScrollbar.ParentCtl3D := False;
  FScrollbar.OnChange := ScrollbarOnChange;
  FScrollbar.OnScroll := ScrollbarOnScroll;
  FScrollbar.OnEnter := ScrollbarOnEnter;
  FScrollbar.TabStop := False;
  FScrollbar.Parent := Self;

  FTitleFont := TFont.Create;
  FTitleFont.Style := [fsBold];
  FTitleFont.Color := clBtnText;

  FFont := TFont.Create;

  FGripperFont := TFont.Create;
  FGripperFont.Color := clBtnText;

  ClSelect := clHighlight;
  ClSelectedText := clHighlightText;
  ClBackground := clWindow;
  ClTitleBackground := clBtnFace;

  (FItemList as TStringList).OnChange := StringListChange;  // Really necessary? It seems to work
  FTitle := '';                                             // fine without it
  FUseInsertList := False;
  FFormattedText := False;
  FCenterTitle := True;
  FCase := False;

  FColumns := TProposalColumns.Create(AOwner, TProposalColumn);

  FGripperHeight := 0;
  FItemHeight := 0;
  FMargin := 2;
  FEffectiveItemHeight := 0;

  OnDblClick := DoDoubleClick;
  OnShow := DoFormShow;
  OnHide := DoFormHide;

  StyleElements := [seClient];
  Resizeable := False;
  Visible := False;
end;

procedure TSynBaseCompletionProposalForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    {
      WS_THICKFRAME causes Windows 10 to display a 6 pixel title bar
      Also with VCL Styles and WS_BORDER the window is not resizable
      So we use WS_BORDER and make the window sizeable by handling WM_NCHITTEST
    }
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;

    // Only affects the first time you create the handle
    // https://stackoverflow.com/questions/44521877/window-class-style-cs-noclose-does-not-work-after-calling-to-recreatewnd
    if (Owner as TSynBaseCompletionProposal).PaintFormShadow then
      WindowClass.style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TSynBaseCompletionProposalForm.Activate;
begin
  Visible := True;
  if (DisplayType = ctCode) and Assigned(CurrentEditor) then
    (CurrentEditor as TCustomSynEdit).AddFocusControl(Self);
end;

procedure TSynBaseCompletionProposalForm.Deactivate;
begin
  if (DisplayType = ctCode) and Assigned(CurrentEditor) then begin
    (CurrentEditor as TCustomSynEdit).RemoveFocusControl(Self);
    Visible := False;
    if Assigned(FCodeItemInfoWindow) then
      FCodeItemInfoWindow.ReleaseHandle;
  end;
end;

destructor TSynBaseCompletionProposalForm.Destroy;
begin
  inherited Destroy;
  FColumns.Free;
  FPaintBitmap.Free;
  FTitleBitmap.Free;
  FItemList.Free;
  FInsertList.Free;
  FAssignedList.Free;
  FTitleFont.Free;
  FFont.Free;
  FGripperFont.Free;
end;

procedure TSynBaseCompletionProposalForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  C: WideChar;
  Cmd: TSynEditorCommand;
  i: integer;

  procedure ExecuteCmdAndCancel;
  begin
    if Cmd <> ecNone then begin
      if Assigned(CurrentEditor) then
        (CurrentEditor as TCustomSynEdit).CommandProcessor(Cmd, #0, nil);

      if Assigned(OnCancel) then
        OnCancel(Self);
    end;
  end;
begin
  if DisplayType = ctCode then
  begin
    i := (CurrentEditor as TCustomSynEdit).Keystrokes.FindKeycode(Key, Shift);
    if i >= 0 then
      Cmd := TCustomSynEdit(CurrentEditor).Keystrokes[i].Command
    else
      Cmd := ecNone;
    case Key of
      SYNEDIT_RETURN:
        if (FCompleteWithEnter) and Assigned(OnValidate) then
          OnValidate(Self, Shift, #0);
      SYNEDIT_TAB:
        if  (FCompleteWithTab) and Assigned(OnValidate) then
          OnValidate(Self, Shift, #0);
      SYNEDIT_ESCAPE:
      begin
        if Assigned(OnCancel) then
          OnCancel(Self);
      end;
      SYNEDIT_LEFT:
        if (Shift = []) then
        begin
          if Length(FCurrentString) > 0 then
          begin
            CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecLeft, #0, nil);
          end
          else
          begin
            //Since we have control, we need to re-send the key to
            //the editor so that the cursor behaves properly
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecLeft, #0, nil);

            if Assigned(OnCancel) then
              OnCancel(Self);
          end;
        end else
          ExecuteCmdAndCancel;
      SYNEDIT_RIGHT:
        if (Shift = []) then
        begin
          if Assigned(CurrentEditor) then
            with CurrentEditor as TCustomSynEdit do
            begin
              if CaretX <= Length(LineText) then
                C := LineText[CaretX]
              else
                C := #32;

              if Self.IsWordBreakChar(C) then
                if Assigned(OnCancel) then
                  OnCancel(Self)
                else
              else
                CurrentString := CurrentString + C;

              CommandProcessor(ecRight, #0, nil);
            end;
        end else
          ExecuteCmdAndCancel;
      SYNEDIT_PRIOR:
        MoveLine(-FLinesInWindow);
      SYNEDIT_NEXT:
        MoveLine(FLinesInWindow);
      SYNEDIT_END:
        Position := FAssignedList.Count - 1;
      SYNEDIT_HOME:
        Position := 0;
      SYNEDIT_UP:
        if ssCtrl in Shift then
          Position := 0
        else
          MoveLine(-1);
      SYNEDIT_DOWN:
        if ssCtrl in Shift then
          Position := FAssignedList.Count - 1
        else
          MoveLine(1);
      SYNEDIT_BACK:
        if (Shift = []) then
        begin
          if Length(FCurrentString) > 0 then
          begin
            CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);

            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);
          end
          else
          begin
            //Since we have control, we need to re-send the key to
            //the editor so that the cursor behaves properly
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);

            if Assigned(OnCancel) then
              OnCancel(Self);
          end;
        end else
          ExecuteCmdAndCancel;
      SYNEDIT_DELETE:
        if Assigned(CurrentEditor) then
          (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteChar, #0, nil);
    else
      ExecuteCmdAndCancel;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.KeyPress(var Key: Char);
begin
  if Key = #0 then Exit;

  if DisplayType = ctCode then
  begin
    case Key of
      #13, #27:; // These keys are already handled by KeyDown
      #32..High(WideChar):
        begin
          if IsWordBreakChar(Key) and Assigned(OnValidate) then
          begin
            //if Key = #32 then
            //  OnValidate(Self, [], #0)
            //else
              OnValidate(Self, [], Key);
          end;

          CurrentString := CurrentString + Key;

          if Assigned(OnKeyPress) then
            OnKeyPress(Self, Key);
        end;
      #8:
        if Assigned(OnKeyPress) then
          OnKeyPress(Self, Key);
      else
        with CurrentEditor as TCustomSynEdit do
          CommandProcessor(ecChar, Key, nil);

        if Assigned(OnCancel) then
          OnCancel(Self);
    end;
  end;
  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  y := (y - FTitleHeight) div FEffectiveItemHeight;
  Position := FScrollbar.Position + y;
//  (CurrentEditor as TCustomSynEdit).UpdateCaret;
end;

procedure TSynBaseCompletionProposalForm.Resize;
begin
  inherited;

  if Owner.ComponentState * [csDesigning, csLoading] <> [] then
    Exit;

  AdjustMetrics;

  if FEffectiveItemHeight <> 0 then
    FLinesInWindow := (ClientHeight - FTitleHeight - FGripperHeight) div FEffectiveItemHeight;

  if Visible then
    AdjustScrollBarPosition;

  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.ResetCanvas(const Canvas : TCanvas);
begin
  with Canvas do
  begin
    Pen.Color := StyleServices.GetSystemColor(FClBackGround);
    Brush.Color := Pen.Color;
    Brush.Style := bsSolid;
    Font.Assign(FFont);
    Font.Color := StyleServices.GetSystemColor(FFont.Color);
  end;
end;

procedure TSynBaseCompletionProposalForm.Paint;
begin
  case FDisplayKind of
    ctCode:
    begin
      PaintTitle;
      PaintCodeItems;
      PaintGripper;
    end;
    ctHint: PaintHint;
    ctParams: PaintParams;
  end;
end;

procedure TSynBaseCompletionProposalForm.PaintCodeItems;
var
  AlreadyDrawn: Boolean;
  i: Integer;
begin
  with FPaintBitmap do
  begin
    ResetCanvas(FPaintBitmap.Canvas);
    Canvas.FillRect(Rect(0, 0, FPaintBitmap.Width, FPaintBitmap.Height));
    for i := 0 to Min(FLinesInWindow - 1, FAssignedList.Count - 1) do
    begin
      if i + FScrollbar.Position = Position then
      begin
        Canvas.Brush.Color := StyleServices.GetSystemColor(FClSelect);
        Canvas.Pen.Color := StyleServices.GetSystemColor(FClSelect);
        Canvas.Rectangle(0, (FEffectiveItemHeight * i), ClientWidth - FScrollbar.Width, (FEffectiveItemHeight * (i + 1)));
        Canvas.Font.Assign(FFont);
        Canvas.Font.Color := StyleServices.GetSystemColor(fClSelectText);
      end;

      AlreadyDrawn := False;

      if Assigned(OnPaintItem) then
        OnPaintItem(Self, LogicalToPhysicalIndex(FScrollBar.Position + i),
          Canvas, Rect(0, (FEffectiveItemHeight * i), ClientWidth - FScrollbar.Width, (FEffectiveItemHeight * (i + 1))), AlreadyDrawn);

      if AlreadyDrawn then
        ResetCanvas(FPaintBitmap.Canvas)
      else
      begin
        if FFormattedText then
        begin
          FormattedTextOut(Canvas,
            Rect(FScaledMargin, (FEffectiveItemHeight * i)  + ((FEffectiveItemHeight - FFontHeight) div 2), FPaintBitmap.Width, FEffectiveItemHeight * (i + 1)),
            GetCurrentPPI, FAssignedList[FScrollbar.Position + i],
            (i + FScrollbar.Position = Position), FColumns, FImages);
        end
        else
        begin
          Canvas.TextOut(FScaledMargin, FEffectiveItemHeight * i, FAssignedList[FScrollbar.Position + i]);
        end;

        if i + FScrollbar.Position = Position then
          ResetCanvas(FPaintBitmap.Canvas);
      end;
    end;
  end;
  Canvas.Draw(0, FTitleHeight, FPaintBitmap);
end;

procedure TSynBaseCompletionProposalForm.PaintGripper;
var
  Details:  TThemedElementDetails;
  LStyle: TCustomStyleServices;
  GripperRect : TRect;
  GripperBarRect : TRect;
  ScaledGripSize: Integer;
  TextRect : TRect;
begin
  if FGripperHeight > 0 then
  begin
    GripperBarRect := TRect.Create(0, ClientHeight - FGripperHeight, ClientWidth, ClientHeight);
    ScaledGripSize := MulDiv(cGripperBarHeight, GetCurrentPPI, 96);
    GripperRect := TRect.Create(ClientWidth - ScaledGripSize, ClientHeight - ScaledGripSize, ClientWidth, ClientHeight);

    Canvas.Brush.Color := StyleServices.GetSystemColor(FClTitleBackground);
    Canvas.FillRect(GripperBarRect);
    Canvas.Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
    Canvas.PenPos := GripperBarRect.TopLeft;
    Canvas.LineTo(GripperBarRect.Right,GripperBarRect.Top);
    LStyle := StyleServices;

    if FGripperText <> '' then
    begin
      textRect := TRect.Create(0, ClientHeight - ScaledGripSize , ClientWidth - ScaledGripSize, ClientHeight);
      textRect.Inflate(-FScaledMargin, -FScaledMargin);
      Canvas.Font.Assign(FGripperFont);
      Canvas.TextRect(textRect, FGripperText);
    end;

    //Draw gripper.
    if StyleServices.Available then
    begin
      details := StyleServices(Self).GetElementDetails(tsGripper);
      LStyle.DrawElement(Canvas.Handle, Details, GripperRect, nil, GetCurrentPPI);
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.PaintHint;
begin
  PaintParams; //does the same right now so this is just to make it easier if they diverge.
end;

procedure TSynBaseCompletionProposalForm.PaintParams;
var
  TmpRect: TRect;
  AlreadyDrawn: Boolean;
  TmpString: string;
  i: Integer;
begin
  ResetCanvas(FPaintBitmap.Canvas);
  with FPaintBitmap do
  begin
    TmpRect := Rect(0, 0, ClientWidth, ClientHeight);
    Canvas.FillRect(TmpRect);

    for i := 0 to FAssignedList.Count - 1 do
    begin
      AlreadyDrawn := False;
      if Assigned(OnPaintItem) then
        OnPaintItem(Self, i, Canvas, Rect(0, FEffectiveItemHeight * i + FScaledMargin,
          ClientWidth, FEffectiveItemHeight * (i + 1) + FScaledMargin), AlreadyDrawn);

      if AlreadyDrawn then
        ResetCanvas(FPaintBitmap.Canvas)
      else
      begin
          if (FDisplayKind = ctParams) and (i = 0) and
            (Owner as TSynBaseCompletionProposal).FormatParams
          then
            TmpString := FormatParamList(FAssignedList[i], CurrentIndex)
          else
            TmpString := FAssignedList[i];

        FormattedTextOut(Canvas, Rect(FScaledMargin + 1,
          FEffectiveItemHeight * i + ((FEffectiveItemHeight-FFontHeight) div 2) + FScaledMargin,
          FPaintBitmap.Width - 1, FEffectiveItemHeight * (i + 1) + FScaledMargin), GetCurrentPPI, TmpString,
          False, nil, FImages);
      end;
    end;
  end;
  Canvas.Draw(0, 0, FPaintBitmap);
end;

procedure TSynBaseCompletionProposalForm.PaintTitle;
var
  TmpRect: TRect;
  TmpX: Integer;
  TitleRect: TRect;
begin
  if FTitle <> '' then
  begin
    TitleRect := TRect.Create(0,0,ClientWidth, FTitleHeight);
    ResetCanvas(FTitleBitmap.Canvas);
    with FTitleBitmap do
    begin
      Canvas.Brush.Color := StyleServices.GetSystemColor(FClTitleBackground);
      Canvas.FillRect(TitleRect);
      Canvas.Font.Assign(FTitleFont);
      Canvas.Font.Color := StyleServices.GetSystemColor(FTitleFont.Color);

      if CenterTitle then
      begin
        TmpX := (Width - Canvas.TextWidth(Title)) div 2;
        if TmpX < FScaledMargin then
          TmpX := FScaledMargin;  //We still want to be able to read it, even if it does go over the edge
      end else
      begin
        TmpX := FScaledMargin;
      end;
      Canvas.TextRect(TitleRect, TmpX, FScaledMargin - 1, FTitle); // -1 because TmpRect.Top is already 1


      TmpRect := TitleRect;
      dec(TmpRect.Bottom, 1);
      Canvas.Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
      Canvas.PenPos := TPoint.Create(TmpRect.Left, TmpRect.Bottom);
      Canvas.LineTo(TmpRect.Right,TmpRect.Bottom);

    end;
    Canvas.Draw(0, 0, FTitleBitmap);
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnChange(Sender: TObject);
begin
  if Position < FScrollbar.Position then
    Position := FScrollbar.Position
  else
    if Position > FScrollbar.Position + FLinesInWindow - 1 then
      Position := FScrollbar.Position + FLinesInWindow - 1
    else
    begin
      Invalidate;
      if Visible and Assigned(FCodeItemInfoWindow) then
        ShowCodeItemInfo(FCodeItemInfoWindow.Caption);
    end;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with CurrentEditor as TCustomSynEdit do
  begin
    SetFocus;
    //This tricks the caret into showing itself again.
    AlwaysShowCaret := False;
    AlwaysShowCaret := True;
//    UpdateCaret;
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnEnter(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionProposalForm.MoveLine(cnt: Integer);
var
  NewPos: Integer;
begin
  NewPos := EnsureRange(Position + cnt, 0, FAssignedList.Count - 1);
  if NewPos <> Position then
    Position := NewPos;
end;

function TSynBaseCompletionProposalForm.LogicalToPhysicalIndex(Index: Integer): Integer;
begin
  if FMatchText and (Index >= 0) and (Index < FAssignedList.Count) then
    Result := Integer(FAssignedList.Objects[Index])
  else
    Result := -1;
end;

function TSynBaseCompletionProposalForm.PhysicalToLogicalIndex(Index: Integer): Integer;
var i : Integer;
begin
  if FMatchText then
  begin
    Result := -1;
    for i := 0 to FAssignedList.Count - 1 do
      if Integer(FAssignedList.Objects[i]) = Index then
      begin
        Result := i;
        break;
      end;
  end else
    Result := Index;
end;

procedure TSynBaseCompletionProposalForm.SetCurrentString(const Value: string);

  function MatchItem(AIndex: Integer; UseItemList: Boolean): Boolean;
  var
    CompareString: string;
  begin
{    if UseInsertList then
      CompareString := FInsertList[AIndex]
    else
    begin
      CompareString := FItemList[AIndex];

      if UsePrettyText then
        CompareString := StripFormatCommands(CompareString);
    end;}

    if UseInsertList then
      CompareString := FInsertList[aIndex]
    else
    begin
      if (FMatchText) and (not UseItemList) then
        CompareString := FAssignedList[aIndex]
      else
        CompareString := FItemList[aIndex];   //GBN 29/08/2002 Fix for when match text is not active

      if UsePrettyText then
        CompareString := StripFormatCommands(CompareString);
    end;


    CompareString := Copy(CompareString, 1, Length(Value));

    if FCase then
      Result := AnsiCompareStr(CompareString, Value) = 0
    else
      Result := AnsiCompareText(CompareString, Value) = 0;
  end;

  procedure RecalcList;
  var
    i: Integer;
  begin
    FAssignedList.Clear;
    for i := 0 to FItemList.Count -1 do
    begin
      if MatchItem(i, True) then
        FAssignedList.AddObject(FItemList[i], TObject(i));
    end;
  end;

var
  i: Integer;
begin
  FCurrentString := Value;
  if DisplayType <> ctCode then
    exit;
  if FMatchText then
  begin
    RecalcList;
    AdjustScrollBarPosition;
    Position := 0;

    if Visible and Assigned(FOnChangePosition) and (DisplayType = ctCode) then
      FOnChangePosition(Owner as TSynBaseCompletionProposal,
        LogicalToPhysicalIndex(FPosition));

    Invalidate;
  end
  else
  begin
    i := 0;
    while (i < ItemList.Count) and (not MatchItem(i, True)) do
      inc(i);

    if i < ItemList.Count then
      Position := i
    else
      Position := 0;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  FAssignedList.Assign(Value);
  CurrentString := CurrentString;
end;

procedure TSynBaseCompletionProposalForm.SetInsertList(const Value: TStrings);
begin
  FInsertList.Assign(Value);
end;

procedure TSynBaseCompletionProposalForm.DoDoubleClick(Sender: TObject);
begin
//we need to do the same as the enter key;
  if DisplayType = ctCode then
    if Assigned(OnValidate) then OnValidate(Self, [], #0);                      //GBN 15/11/2001
end;

procedure TSynBaseCompletionProposalForm.SetPosition(const Value: Integer);
Var
  Info : string;
begin
//  if ((Value <= 0) and (FPosition = 0)) or (FPosition = Value) then
//    Exit;

  if (Value < 0) or (Value > AssignedList.Count - 1) then
  begin
    ShowCodeItemInfo('');  // Destroys the Info window
    Exit;
  end;

  if Value <= FAssignedList.Count - 1 then
  begin
    FPosition := Value;
    if Position < FScrollbar.Position then
      FScrollbar.Position := Position else
    if FScrollbar.Position < (Position - FLinesInWindow + 1) then
      FScrollbar.Position := Position - FLinesInWindow + 1;

    if Visible and (DisplayType = ctCode) then
    begin
      if Assigned(FOnChangePosition) then
        FOnChangePosition(Owner as TSynBaseCompletionProposal,
          LogicalToPhysicalIndex(FPosition));
      if Assigned(FOnCodeItemInfo) then begin
        FOnCodeItemInfo(Owner as TSynBaseCompletionProposal,
          LogicalToPhysicalIndex(FPosition), Info);
        ShowCodeItemInfo(Info);
      end;
    end;
    Invalidate;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetResizeable(const Value: Boolean);
begin
  FResizeable := Value;
  RecreateWnd;
end;

procedure TSynBaseCompletionProposalForm.SetItemHeight(const Value: Integer);
begin
  if Value <> FItemHeight then
    FItemHeight := Value;
end;

procedure TSynBaseCompletionProposalForm.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
      FImages.RemoveFreeNotification(Self);

    FImages := Value;
    if Assigned(FImages) then
      FImages.FreeNotification(Self);
  end;
end;

procedure TSynBaseCompletionProposalForm.RecalcItemHeight;
begin
  HandleNeeded;
  Canvas.Font.Assign(FFont);
  FFontHeight := Canvas.TextHeight(TextHeightString);
  if FItemHeight > 0 then
    FEffectiveItemHeight := MulDiv(FItemHeight, GetCurrentPPI, 96)
  else
  begin
    FEffectiveItemHeight := FFontHeight;
  end;
end;

procedure TSynBaseCompletionProposalForm.StringListChange(Sender: TObject);
begin
  FScrollbar.Position := Position;
end;

function TSynBaseCompletionProposalForm.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := (Owner as TSynBaseCompletionProposal).IsWordBreakChar(AChar);
end;

procedure TSynBaseCompletionProposalForm.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
begin
  if csDesigning in ComponentState then exit;

  if GetKeyState(VK_CONTROL) >= 0 then nDelta := Mouse.WheelScrollLines
    else nDelta := FLinesInWindow;

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > FLinesInWindow) then
    nDelta := FLinesInWindow;

  Position := Position - (nDelta * nWheelClicks);
//  (CurrentEditor as TCustomSynEdit).UpdateCaret;
end;

procedure TSynBaseCompletionProposalForm.WMNCHitTest(var Message: TWMNCHitTest);
//  Makes the form resizable
var
  D: Integer;
  P: TPoint;
begin
  if not (FResizeable and (DisplayType = ctCode)) then
  begin
    inherited;
    Exit;
  end;

  D := GetSystemMetrics(SM_CXSIZEFRAME);

  P := Self.ScreenToClient(Message.Pos);

  if P.Y < D then
  begin
    if P.X < D then
      Message.Result := HTTOPLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTTOPRIGHT
    else
      Message.Result := HTTOP;
  end
  else if P.Y > ClientHeight - D then
  begin
    if P.X < D then
      Message.Result := HTBOTTOMLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTBOTTOMRIGHT
    else
      Message.Result := HTBOTTOM;
  end
  else
  begin
    if P.X < D then
      Message.Result := HTLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTRIGHT
  end;

  if Message.Result = 0 then
    inherited;
end;

function GetMDIParent(const Form: TCustomForm): TCustomForm;
{ Returns the parent of the specified MDI child form. But, if Form isn't a
  MDI child, it simply returns Form. }
var
  I, J: Integer;
begin
  Result := Form;
  if Form = nil then
    exit;
  if (Form is TCustomForm) and
     ((Form as TForm).FormStyle = fsMDIChild) then
    for I := 0 to Screen.FormCount-1 do
      with Screen.Forms[I] do
      begin
        if FormStyle <> fsMDIForm then Continue;
        for J := 0 to MDIChildCount-1 do
          if MDIChildren[J] = Form then
          begin
            Result := Screen.Forms[I];
            exit;
          end;
      end;
end;

procedure TSynBaseCompletionProposalForm.WMActivate(var Message: TWMActivate);
var
  ParentForm: TCustomForm;
begin
  if csDesigning in ComponentState then begin
    inherited;
    Exit;
  end;

  if Assigned(CurrentEditor) then
  begin
    ParentForm := GetParentForm(CurrentEditor);
    if Assigned(ParentForm) then
      ParentForm := GetMDIParent(ParentForm);
  end
  else
    ParentForm := nil;

  if Assigned(ParentForm) and ParentForm.HandleAllocated then
    SendMessage(ParentForm.Handle, WM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);
end;

procedure TSynBaseCompletionProposalForm.DoFormHide(Sender: TObject);
begin
  if CurrentEditor <> nil then
  begin
    (CurrentEditor as TCustomSynEdit).AlwaysShowCaret := OldShowCaret;
//    (CurrentEditor as TCustomSynEdit).UpdateCaret;
    if (Owner as TSynBaseCompletionProposal).FontsAreScaled then
    begin
      TitleFont.Height := MulDiv(TitleFont.Height, TitleFont.PixelsPerInch, GetCurrentPPI);
      Font.Height := MulDiv(Font.Height, Font.PixelsPerInch, GetCurrentPPI);
      TSynBaseCompletionProposal(Owner).FontsAreScaled := False;
    end;
    if DisplayType = ctCode then
    begin
      // Save after removing the PPI scaling
      (Owner as TSynBaseCompletionProposal).FWidth := MulDiv(ClientWidth, 96, GetCurrentPPI);
      (Owner as TSynBaseCompletionProposal).FNbLinesInWindow := FLinesInWindow;
    end;
  end;
  if Assigned((Owner as TSynBaseCompletionProposal).OnClose) then
    TSynBaseCompletionProposal(Owner).OnClose(Self);
end;

procedure TSynBaseCompletionProposalForm.DoFormShow(Sender: TObject);
begin
  if Assigned(CurrentEditor) then
  begin
    with CurrentEditor as TCustomSynEdit do
    begin
      OldShowCaret := AlwaysShowCaret;
      AlwaysShowCaret := Focused;
//      UpdateCaret;
    end;
  end;
  if Assigned((Owner as TSynBaseCompletionProposal).OnShow) then
    (Owner as TSynBaseCompletionProposal).OnShow(Self);
end;

procedure TSynBaseCompletionProposalForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TSynBaseCompletionProposalForm.AdjustMetrics;
var
  TitleFontHeight: Integer;
begin
  FScaledMargin := MulDiv(FMargin, GetCurrentPPI, 96);

  if DisplayType = ctCode then
  begin
    Canvas.Font.Assign(FTitleFont);
    TitleFontHeight := Canvas.TextHeight(TextHeightString);

    if FTitle <> '' then
      FTitleHeight := TitleFontHeight + 2 * FScaledMargin
    else
      FTitleHeight := 0;

    if FResizeable and TSynBaseCompletionProposal(Owner).ShowGripper then
      FGripperHeight := MulDiv(cGripperBarHeight, GetCurrentPPI, 96)
    else
      FGripperHeight := 0;

    if (ClientWidth >= FScrollbar.Width) and (ClientHeight >= FTitleHeight) then
      FPaintBitmap.SetSize(ClientWidth - FScrollbar.Width, ClientHeight - FTitleHeight - FGripperHeight);

    if (ClientWidth > 0) and (FTitleHeight > 0) then
      FTitleBitmap.SetSize(ClientWidth, FTitleHeight);

  end else
  begin
    if (ClientWidth > 0) and (ClientHeight > 0) then
      FPaintBitmap.SetSize(ClientWidth, ClientHeight);
  end;
end;

procedure TSynBaseCompletionProposalForm.AdjustScrollBarPosition;
begin
  if FDisplayKind = ctCode then
  begin
    if Assigned(FScrollbar) then
    begin
      FScrollbar.Top := FTitleHeight;
      FScrollbar.Height := ClientHeight - FTitleHeight - FGripperHeight;
      FScrollbar.Left := ClientWidth - FScrollbar.Width;

      if FAssignedList.Count - FLinesInWindow < 0 then
      begin
        FScrollbar.PageSize := 0;
        FScrollbar.Max := 0;
        FScrollbar.Enabled := False;
      end else
      begin
        FScrollbar.PageSize := 0;
        FScrollbar.Max := FAssignedList.Count - FLinesInWindow;
        if FScrollbar.Max <> 0 then
        begin
          FScrollbar.LargeChange := FLinesInWindow;
          FScrollbar.PageSize := 1;
          FScrollbar.Enabled := True;
        end else
          FScrollbar.Enabled := False;
      end;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TSynBaseCompletionProposalForm.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if Assigned(FCodeItemInfoWindow) then
    FCodeItemInfoWindow.Canvas.Font.Assign(FFont);
end;

procedure TSynBaseCompletionProposalForm.SetGripperFont(const Value: TFont);
begin
  FGripperFont.Assign(Value);
end;

procedure TSynBaseCompletionProposalForm.SetGripperText(const Value: string);
begin
  FGripperText := value;
end;

procedure TSynBaseCompletionProposalForm.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TSynBaseCompletionProposalForm.ShowCodeItemInfo(const Info: string);
Var
  HintRect, WorkArea: TRect;
  Monitor: TMonitor;
begin
  if Info = '' then begin
    if Assigned(FCodeItemInfoWindow) then begin
       fCodeItemInfoWindow.ReleaseHandle;
       FCodeItemInfoWindow.Caption := '';
    end;
  end
  else
  begin
    if not Assigned(FCodeItemInfoWindow) then begin
      FCodeItemInfoWindow := THintWindow.Create(Self);
      FCodeItemInfoWindow.Canvas.Font.Assign(FFont);
    end;

    Monitor := Screen.MonitorFromPoint(ClientToScreen(Point(0,0)));
    WorkArea := Monitor.WorkareaRect;

    HintRect := FCodeItemInfoWindow.CalcHintRect(Monitor.Width div 2, Info, nil);

    // Calculate horizontal position
    if Left + Width + (HintRect.Right - HintRect.Left) < WorkArea.Right
    then
      OffsetRect(HintRect, Left + Width , 0)
    else
      OffsetRect(HintRect, Left - (HintRect.Right - HintRect.Left), 0);

    // Calculate vertical position
      OffsetRect(HintRect, 0, ClientToScreen(Point(0,0)).Y +
        (FPosition - FScrollbar.Position) * FEffectiveItemHeight);
      // No need to fit it to the workarea since ActivateHint does that

    FCodeItemInfoWindow.ActivateHint(HintRect, Info);
  end;
end;

procedure TSynBaseCompletionProposalForm.SetColumns(Value: TProposalColumns);
begin
  FColumns.Assign(Value);
end;


function TSynBaseCompletionProposalForm.GetCurrentPPI: Integer;
begin
  if Assigned(FCurrentEditor) then
    Result := FCurrentEditor.CurrentPPI
  else
    Result := Screen.PixelsPerInch;
end;

procedure TSynBaseCompletionProposalForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if AComponent = FImages then
      Images := nil;
  end;

  inherited Notification(AComponent, Operation);
end;


{ TSynBaseCompletionProposal }

constructor TSynBaseCompletionProposal.Create(AOwner: TComponent);
begin
  FWidth := 260;
  FNbLinesInWindow := 8;
  inherited Create(AOwner);
  FForm := TSynBaseCompletionProposalForm.Create(Self);
  EndOfTokenChr := DefaultEndOfTokenChr;
  FDotOffset := 0;
  FFormatParams := True;
  DefaultType := ctCode;
  FPaintFormShadow := True;
end;

procedure TSynBaseCompletionProposal.Execute(s: string; x, y: integer);
begin
  ExecuteEx(s, x, y, DefaultType);
end;

procedure TSynBaseCompletionProposal.ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType);
Var
  WorkArea : TRect;
  Monitor: TMonitor;

  function GetParamWidth(const S: string): Integer;
  var
    i: Integer;
    List: TStringList;
    NewWidth: Integer;
  begin
    List := TStringList.Create;
    try
      List.CommaText := S;

      Result := 0;
      for i := -1 to List.Count -1 do
      begin
        NewWidth := FormattedTextWidth(Form.Canvas,
        FormatParamList(S, i), Form.GetCurrentPPI, Columns, FForm.Images);

        if NewWidth > Result then
          Result := NewWidth;
      end;
    finally
      List.Free;
    end;
  end;

  procedure RecalcFormPlacement;
  var
    i: Integer;
    tmpWidth: Integer;
    tmpHeight: Integer;
    tmpX: Integer;
    tmpY: Integer;
    tmpStr: string;
    NewWidth: Integer;
    ActivePPI: integer;
  begin
    ActivePPI := FForm.GetCurrentPPI;
    // ScaleForPPI will scale Width and Height
    // Scaling at this point prevents further scaling when the Form is shown
    {$IF CompilerVersion >= 32}FForm.ScaleForPPI(ActivePPI);{$ENDIF}

    // Scrollbar needs to be properly scaled in case primary monitor is High-DPI
    // Check for Windows Anniversary Edition
    if (TOSVersion.Major >= 10) and (TOSVersion.Build >= 14393) then
      FForm.FScrollbar.Width := GetSystemMetricsForDPI(SM_CXVSCROLL, ActivePPI)
    else
      FForm.FScrollbar.Width := GetSystemMetrics(SM_CXVSCROLL);

    if not FFontsAreScaled then
    begin
      TitleFont.Height := MulDiv(TitleFont.Height, ActivePPI, TitleFont.PixelsPerInch);
      Font.Height := MulDiv(Font.Height, ActivePPI, Font.PixelsPerInch);
      FFontsAreScaled := True;
    end;

    // Now we can do the measurements
    FForm.RecalcItemHeight;
    FForm.AdjustMetrics;

    tmpX := x;
    tmpY := Y + FForm.FScaledMargin;
    tmpWidth := 0;
    tmpHeight := 0;
    case Kind of
    ctCode:
      begin
        tmpWidth := MulDiv(FWidth, ActivePPI, 96);
        tmpHeight := Form.FTitleHeight + Form.FGripperHeight + Form.FEffectiveItemHeight * FNbLinesInWindow;
      end;
    ctHint:
      begin
        tmpHeight := Form.FEffectiveItemHeight * ItemList.Count +  2 * FForm.FScaledMargin;

        Form.Canvas.Font.Assign(Font);
        for i := 0 to ItemList.Count -1 do
        begin
          tmpStr := ItemList[i];
          NewWidth := FormattedTextWidth(Form.Canvas, tmpStr,
            ActivePPI, nil, FForm.Images);
          if NewWidth > tmpWidth then
            tmpWidth := NewWidth;
        end;

        Inc(tmpWidth, 2 * FForm.FScaledMargin);
      end;
    ctParams:
      begin
        tmpHeight := Form.FEffectiveItemHeight * ItemList.Count + 2 * FForm.FScaledMargin;

        Form.Canvas.Font.Assign(Font);
        for i := 0 to ItemList.Count -1 do
        begin
          if (i = 0) and FFormatParams then
            NewWidth := GetParamWidth(StripFormatCommands(ItemList[i]))
          else
            NewWidth := FormattedTextWidth(Form.Canvas, ItemList[i],
              ActivePPI, nil, FForm.Images);

          if Assigned(Form.OnMeasureItem) then
            Form.OnMeasureItem(Self, i, Form.Canvas, NewWidth);

          if NewWidth > tmpWidth then
            tmpWidth := NewWidth;
        end;

        inc(tmpWidth, 2 * FForm.FScaledMargin);
      end;
    end;

    // Maximum size 3/4 of monitor width and 1/2 of monitor height
    tmpWidth := Min(tmpWidth, MulDiv(WorkArea.Width, 3, 4));
    tmpHeight := Min(tmpHeight, WorkArea.Height div 2);

    if tmpX + tmpWidth > WorkArea.Right then
    begin
      tmpX := WorkArea.Right - tmpWidth - 2 * Form.FScaledMargin;
      if tmpX < 0 then
        tmpX := 0;
    end;

    if tmpY + tmpHeight > WorkArea.Bottom then
    begin
      tmpY := tmpY - tmpHeight - (Form.CurrentEditor  as TCustomSynEdit).LineHeight -
        2 * FForm.FScaledMargin;
      if tmpY < 0 then
        tmpY := 0;
    end;

    Form.ClientWidth := tmpWidth;
    Form.ClientHeight := tmpHeight;
    Form.SetBounds(tmpX, tmpY, Form.Width, Form.Height)
  end;

var
  TmpOffset: Integer;
begin
  Monitor := Screen.MonitorFromPoint(Point(x, y));
  WorkArea := Monitor.WorkareaRect;

  DisplayType := Kind;

  FCanExecute := True;
  if Assigned(OnExecute) then
    OnExecute(Kind, Self, s, x, y, FCanExecute);

  if (not FCanExecute) or (ItemList.Count = 0) then
  begin
    if Form.Visible and (Kind = ctParams) then
      Form.Visible := False;
    Exit;
  end;

  // Set PopupMode, PopupParent, FormStyle and Constraints
  Form.PopupMode := pmExplicit;
  if (Kind =  ctCode) then
  begin
    Form.FormStyle := fsStayOnTop;
    //if the form is sized too small the metrics calcs fall over.
    Form.Constraints.MinHeight := 100;
    Form.Constraints.MinWidth := 100;  end
  else
  begin
    Form.FormStyle := fsNormal;
    Form.Constraints.MinHeight := 0;
    Form.Constraints.MinWidth := 0;
  end;

  if Assigned(Form.CurrentEditor) then
  begin
    TmpOffset := (Form.CurrentEditor as TCustomSynEdit).Canvas.TextWidth(Copy(s, 1, DotOffset));
    if DotOffset > 1 then
      TmpOffset := TmpOffset + (3 * (DotOffset -1));
    Form.PopupParent := GetParentForm(Form.CurrentEditor);
  end else
    TmpOffset := 0;
  x := x - tmpOffset;

  ResetAssignedList;

  RecalcFormPlacement;
  case Kind of
  ctCode:
    if Form.AssignedList.Count > 0 then
    begin
      //This may seem redundant, but it fixes scrolling bugs for the first time
      //That is the only time these occur
      Position := 0;
      Form.AdjustScrollBarPosition;
      Form.FScrollbar.Position := Form.Position;
      Form.FScrollbar.Visible := True;

      Form.Show;

      CurrentString := s;  // bug id 1496148
    end;
  ctParams, ctHint:
    begin
      Form.FScrollbar.Visible := False;

      if not Form.Visible then
      begin
        //ShowWindow(Form.Handle, SW_SHOWNOACTIVATE);
        ShowWindow(Form.Handle, SW_SHOWNA);
        Form.Visible := True;
      end;
      Form.Invalidate;
    end;
  end;
end;

function TSynBaseCompletionProposal.GetCurrentString: string;
begin
  Result := Form.CurrentString;
end;

function TSynBaseCompletionProposal.GetItemList: TStrings;
begin
  Result := Form.ItemList;
end;

function TSynBaseCompletionProposal.GetInsertList: TStrings;
begin
  Result := Form.InsertList;
end;

function TSynBaseCompletionProposal.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynBaseCompletionProposal.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynBaseCompletionProposal.GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynBaseCompletionProposal.GetOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
begin
  Result := Form.OnMeasureItem;
end;

function TSynBaseCompletionProposal.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynBaseCompletionProposal.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynBaseCompletionProposal.SetCurrentString(const Value: string);
begin
  Form.CurrentString := Value;
end;

procedure TSynBaseCompletionProposal.SetItemList(const Value: TStrings);
begin
  Form.ItemList := Value;
end;

procedure TSynBaseCompletionProposal.SetInsertList(const Value: TStrings);
begin
  Form.InsertList := Value;
end;

procedure TSynBaseCompletionProposal.SetNbLinesInWindow(const Value: Integer);
begin
  FNbLinesInWindow := Value;
end;

procedure TSynBaseCompletionProposal.SetOnCancel(const Value: TNotifyEvent);
begin
  Form.OnCancel := Value;
end;

procedure TSynBaseCompletionProposal.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  Form.OnKeyPress := Value;
end;

procedure TSynBaseCompletionProposal.SetOnPaintItem(const Value:
  TSynBaseCompletionProposalPaintItem);
begin
  Form.OnPaintItem := Value;
end;

procedure TSynBaseCompletionProposal.SetOnMeasureItem(const Value:
  TSynBaseCompletionProposalMeasureItem);
begin
  Form.OnMeasureItem := Value;
end;


procedure TSynBaseCompletionProposal.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynBaseCompletionProposal.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynBaseCompletionProposal.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

procedure TSynBaseCompletionProposal.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

procedure TSynBaseCompletionProposal.SetWidth(Value: Integer);
begin
  FWidth := Value;
end;

procedure TSynBaseCompletionProposal.Activate;
begin
  if Assigned(Form) then
    Form.Activate;
end;

procedure TSynBaseCompletionProposal.Deactivate;
begin
  if Assigned(Form) then
    Form.Deactivate;
end;

procedure TSynBaseCompletionProposal.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

function TSynBaseCompletionProposal.GetClBack: TColor;
begin
  Result := Form.ClBackground;
end;

procedure TSynBaseCompletionProposal.SetClBack(const Value: TColor);
begin
  Form.ClBackground := Value
end;

function TSynBaseCompletionProposal.GetClSelectedText: TColor;
begin
  Result := Form.ClSelectedText;
end;

procedure TSynBaseCompletionProposal.SetClSelectedText(const Value: TColor);
begin
  Form.ClSelectedText := Value;
end;

procedure TSynBaseCompletionProposal.AddItem(ADisplayText, AInsertText: string);
begin
  GetInsertList.Add(AInsertText);
  GetItemList.Add(ADisplayText);
end;

procedure TSynBaseCompletionProposal.AddItemAt(Where: Integer; ADisplayText, AInsertText: string);
begin
  try
    GetInsertList.Insert(Where, AInsertText);
    GetItemList.Insert(Where, ADisplayText);
  except
    raise Exception.Create('Cannot insert item at position ' + IntToStr(Where) + '.');
  end;
end;

procedure TSynBaseCompletionProposal.ClearList;
begin
  GetInsertList.Clear;
  GetItemList.Clear;
end;

function TSynBaseCompletionProposal.DisplayItem(AIndex : Integer): string;
begin
  Result := GetItemList[AIndex];
end;

function TSynBaseCompletionProposal.InsertItem(AIndex : Integer): string;
begin
  Result := GetInsertList[AIndex];
end;

function TSynBaseCompletionProposal.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := False;
  if (scoConsiderWordBreakChars in Options) and Assigned(Form) and
    Assigned(Form.CurrentEditor)
  then
    Result := Form.CurrentEditor.IsWordBreakChar(AChar);
  Result := Result or (Pos(AChar, EndOfTokenChr) > 0);
end;

function TSynBaseCompletionProposal.GetDisplayKind: SynCompletionType;
begin
  Result := Form.DisplayType;
end;

procedure TSynBaseCompletionProposal.SetDisplayKind(const Value: SynCompletionType);
begin
  Form.DisplayType := Value;
end;

function TSynBaseCompletionProposal.GetParameterToken: TCompletionParameter;
begin
  Result := Form.OnParameterToken;
end;

procedure TSynBaseCompletionProposal.SetPaintFormShadow(const Value: Boolean);
begin
  if FPaintFormShadow <> Value then
  begin
    FPaintFormShadow := Value;
    FForm.RecreateWnd;
  end;
end;

procedure TSynBaseCompletionProposal.SetParameterToken(
  const Value: TCompletionParameter);
begin
  Form.OnParameterToken := Value;
end;

procedure TSynBaseCompletionProposal.SetColumns(const Value: TProposalColumns);
begin
  FForm.Columns := Value;
end;

function TSynBaseCompletionProposal.GetColumns: TProposalColumns;
begin
  Result := FForm.Columns;
end;

function TSynBaseCompletionProposal.GetResizeable: Boolean;
begin
  Result := FForm.Resizeable;
end;

procedure TSynBaseCompletionProposal.SetResizeable(const Value: Boolean);
begin
  if FForm.Resizeable <> Value then
    FForm.Resizeable := Value;
end;

function TSynBaseCompletionProposal.GetItemHeight: Integer;
begin
  Result := FForm.ItemHeight;
end;

procedure TSynBaseCompletionProposal.SetItemHeight(const Value: Integer);
begin
  if FForm.ItemHeight <> Value then
    FForm.ItemHeight := Value;
end;

procedure TSynBaseCompletionProposal.SetImages(const Value: TCustomImageList);
begin
  FForm.Images := Value;
end;

function TSynBaseCompletionProposal.GetImages: TCustomImageList;
begin
  Result := FForm.Images;
end;

function TSynBaseCompletionProposal.GetMargin: Integer;
begin
  Result := FForm.Margin;
end;

procedure TSynBaseCompletionProposal.SetMargin(const Value: Integer);
begin
  if Value <> FForm.Margin then
    FForm.Margin := Value;
end;

function TSynBaseCompletionProposal.GetDefaultKind: SynCompletionType;
begin
  Result := Form.DefaultType;
end;

procedure TSynBaseCompletionProposal.SetDefaultKind(const Value: SynCompletionType);
begin
  Form.DefaultType := Value;
  Form.DisplayType := Value;
  Form.RecreateWnd;
end;

procedure TSynBaseCompletionProposal.SetEndOfTokenChar(
  const Value: string);
begin
  if Form.FEndOfTokenChr <> Value then
  begin
    Form.FEndOfTokenChr := Value;
  end;
end;

function TSynBaseCompletionProposal.GetClTitleBackground: TColor;
begin
  Result := Form.ClTitleBackground;
end;

procedure TSynBaseCompletionProposal.SetClTitleBackground(
  const Value: TColor);
begin
  Form.ClTitleBackground := Value;
end;

function TSynBaseCompletionProposal.GetTitle: string;
begin
  Result := Form.Title;
end;

procedure TSynBaseCompletionProposal.SetTitle(const Value: string);
begin
  Form.Title := Value;
end;

function TSynBaseCompletionProposal.GetFont: TFont;
begin
  Result := Form.Font;
end;

function TSynBaseCompletionProposal.GetGripperFont: TFont;
begin
  result := Form.GripperFont;
end;

function TSynBaseCompletionProposal.GetGripperText: string;
begin
  result := Form.GripperText;
end;

function TSynBaseCompletionProposal.GetTitleFont: TFont;
begin
  Result := Form.TitleFont;
end;

procedure TSynBaseCompletionProposal.SetFont(const Value: TFont);
begin
  Form.Font := Value;
end;

procedure TSynBaseCompletionProposal.SetGripperFont(const Value: TFont);
begin
  Form.GripperFont := Value;
end;

procedure TSynBaseCompletionProposal.SetGripperText(const Value: string);
begin
  Form.GripperText := Value;
end;

procedure TSynBaseCompletionProposal.SetTitleFont(const Value: TFont);
begin
  Form.TitleFont := Value;
end;

function TSynBaseCompletionProposal.GetEndOfTokenChar: string;
begin
  Result := Form.EndOfTokenChr;
end;

function TSynBaseCompletionProposal.GetOptions: TSynCompletionOptions;
begin
  Result := fOptions;
end;

procedure TSynBaseCompletionProposal.SetOptions(
  const Value: TSynCompletionOptions);
begin
  if fOptions <> Value then
  begin
    fOptions := Value;
    Form.CenterTitle := scoTitleIsCentered in Value;
    Form.CaseSensitive := scoCaseSensitive in Value;
    Form.UsePrettyText := scoUsePrettyText in Value;
    Form.UseInsertList := scoUseInsertList in Value;
    Form.MatchText := scoLimitToMatchedText in Value;
    Form.CompleteWithTab := scoCompleteWithTab in Value;
    Form.CompleteWithEnter := scoCompleteWithEnter in Value;
  end;
end;

function TSynBaseCompletionProposal.GetTriggerChars: string;
begin
  Result := Form.TriggerChars;
end;

procedure TSynBaseCompletionProposal.SetTriggerChars(const Value: string);
begin
  Form.TriggerChars := Value;
end;

procedure TSynBaseCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  //Do nothing here, used in TSynCompletionProposal
end;

procedure TSynBaseCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand;
  var AChar: WideChar; Data, HandlerData: Pointer);
begin
  // Do nothing here, used in TSynCompletionProposal
end;

function TSynBaseCompletionProposal.GetOnChange: TCompletionChange;
begin
  Result := Form.FOnChangePosition;
end;

function TSynBaseCompletionProposal.GetOnCodeItemInfo: TCodeItemInfo;
begin
  Result := Form.FOnCodeItemInfo;
end;

procedure TSynBaseCompletionProposal.SetOnChange(
  const Value: TCompletionChange);
begin
  Form.FOnChangePosition := Value;
end;

procedure TSynBaseCompletionProposal.SetOnCodeItemInfo(
  const Value: TCodeItemInfo);
begin
  Form.FOnCodeItemInfo := Value;
end;

procedure TSynBaseCompletionProposal.ResetAssignedList;
begin
  Form.AssignedList.Assign(ItemList);
end;

{ ----------------  TSynCompletionProposal -------------- }

procedure TSynCompletionProposal.HandleOnCancel(Sender: TObject);
var
  F: TSynBaseCompletionProposalForm;
  CurrentEditor : TCustomSynedit;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  FNoNextKey := False;
  CurrentEditor := F.CurrentEditor;
  if CurrentEditor <> nil then
  begin
    if Assigned(FTimer) then
      FTimer.Enabled := False;

    F.Hide;

    if ((CurrentEditor as TCustomSynEdit).Owner is TWinControl) and
       (((CurrentEditor as TCustomSynEdit).Owner as TWinControl).Visible) then
    begin
      ((CurrentEditor as TCustomSynEdit).Owner as TWinControl).SetFocus;
    end;

    if (CurrentEditor as TCustomSynEdit).CanFocus then
      (CurrentEditor as TCustomSynEdit).SetFocus;

    if Assigned(OnCancelled) then
      OnCancelled(Self);
  end;
end;

procedure TSynCompletionProposal.HandleOnValidate(Sender: TObject;
  Shift: TShiftState; EndToken: WideChar);
var
  F: TSynBaseCompletionProposalForm;
  Value: string;
  Index: Integer;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if Assigned(F.CurrentEditor) then
    with F.CurrentEditor as TCustomSynEdit do
    begin
      //Treat entire completion as a single undo operation
      BeginUpdate;
      BeginUndoBlock;
      try
        if FAdjustCompletionStart then
          FCompletionStart := BufferCoord(FCompletionStart, CaretY).Char;
        BlockBegin := BufferCoord(FCompletionStart, CaretY);
        if EndToken = #0 then
          BlockEnd := BufferCoord(WordEnd.Char, CaretY)
        else
          BlockEnd := BufferCoord(CaretX, CaretY);

        if scoUseInsertList in FOptions then
        begin
          if scoLimitToMatchedText in FOptions then
          begin
            if (Form.FAssignedList.Count > Position) then
              // Added check to make sure item is only used when no EndChar
              if (InsertList.Count > Integer(Form.FAssignedList.Objects[position])) and
                 ((scoEndCharCompletion in fOptions) or (EndToken = #0)) then
                Value := InsertList[Integer(Form.FAssignedList.Objects[position])]
              else
                Value := SelText
            else
              Value := SelText;
          end else
          begin
            // Added check to make sure item is only used when no EndChar
            if (InsertList.Count > Position) and
               ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
              Value := InsertList[position]
            else
              Value := SelText;
          end;
        end else
        begin
          // Added check to make sure item is only used when no EndChar
          if (Form.FAssignedList.Count > Position) and
             ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
            Value := Form.FAssignedList[Position]
          else
            Value := SelText;
        end;
        Index := Position; // Need to assign position to temp var since it changes later

        if Assigned(FOnCodeCompletion) then
          FOnCodeCompletion(Self, Value, Shift,
            F.LogicalToPhysicalIndex(Index), EndToken); //GBN 15/11/2001

        if SelText <> Value then
          SelText := Value;

        with (F.CurrentEditor as TCustomSynEdit) do
        begin
          //This replaces the previous way of cancelling the completion by
          //sending a WM_MOUSEDOWN message. The problem with the mouse down is
          //that the editor would bounce back to the left margin, very irritating
          InternalCancelCompletion;
          if CanFocus then
            SetFocus;
          EnsureCursorPosVisible;
          CaretXY := BlockEnd;
          BlockBegin := CaretXY;
        end;
        if Assigned(FOnAfterCodeCompletion) then
          FOnAfterCodeCompletion(Self, Value, Shift,
            F.LogicalToPhysicalIndex(Index), EndToken);
      finally
        EndUndoBlock;
        EndUpdate;
      end;
    end;
end;

procedure TSynCompletionProposal.HandleOnKeyPress(Sender: TObject; var Key: WideChar);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then
  begin
    with F.CurrentEditor as TCustomSynEdit do
      CommandProcessor(ecChar, Key, nil);
    //Daisy chain completions
    Application.ProcessMessages;
    if (System.Pos(Key, TriggerChars) > 0) and not F.Visible then
      begin
        if (Sender is TCustomSynEdit) then
          DoExecute(Sender as TCustomSynEdit)
        else
          if Assigned(Form.CurrentEditor) then
            DoExecute(Form.CurrentEditor as TCustomSynEdit);
      end;
  end;
end;

procedure TSynCompletionProposal.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if Assigned(Editor) then
      RemoveEditor(Editor);
    FEditor := Value;
    if Assigned(Value) then
      AddEditor(Value);
  end;
end;

procedure TSynCompletionProposal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if Editor = AComponent then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor(TCustomSynEdit(AComponent));
  end;

  inherited Notification(AComponent, Operation);
end;

constructor TSynCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnKeyPress := HandleOnKeyPress;
  Form.OnValidate := HandleOnValidate;
  Form.OnCancel := HandleOnCancel;
  Form.OnDblClick := HandleDblClick;
  EndOfTokenChr := DefaultEndOfTokenChr;
  TriggerChars := '.';
  fTimerInterval:= 1000;
  fNoNextKey := False;

  fShortCut := Vcl.Menus.ShortCut(Ord(' '), [ssCtrl]);
  Options := DefaultProposalOptions;
  fEditors := TList.Create;
end;

procedure TSynCompletionProposal.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletionProposal.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
  Editor: TCustomSynedit;
begin
  Editor := Sender as TCustomSynEdit;
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
    if ((DefaultType <> ctCode) or not Editor.ReadOnly) and
       (Shift = ShortCutShift) and (Key = ShortCutKey) then
    begin
      Form.CurrentEditor := Editor;
      Key := 0;
      DoExecute(Editor);
    end;
end;

function TSynCompletionProposal.GetCurrentInput(AEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if AEditor <> nil then
  begin
    s := AEditor.LineText;
    i := AEditor.CaretX - 1;
    if i <= Length(s) then
    begin
      FAdjustCompletionStart := False;
      while (i > 0) and (s[i] > #32) and not Self.IsWordBreakChar(s[i]) do
        dec(i);

      FCompletionStart := i + 1;
      Result := Copy(s, i + 1, AEditor.CaretX - i - 1);
    end
    else
      FAdjustCompletionStart := True;

    FCompletionStart := i + 1;
  end;
end;

function TSynCompletionProposal.GetPreviousToken(AEditor: TCustomSynEdit): string;
var
  Line: string;
  X: Integer;
begin
  Result := '';
  if not Assigned(AEditor) then
    exit;

  Line := AEditor.Lines[AEditor.CaretXY.Line - 1];
  X := AEditor.CaretXY.Char - 1;
  if (X = 0) or (X > Length(Line)) or (Length(Line) = 0) then
    exit;

  if Self.IsWordBreakChar(Line[X]) then
    dec(X);

  while (X > 0) and not(Self.IsWordBreakChar(Line[X])) do
  begin
    Result := Line[X] + Result;
    dec(x);
  end;
end;

procedure TSynCompletionProposal.EditorKeyPress(Sender: TObject; var Key: WideChar);
begin
  if fNoNextKey  then
  begin
    FNoNextKey := False;
    Key := #0;
  end
  else
  if Assigned(FTimer) then
  begin
    DeactivateTimer;
    if Pos(Key, TriggerChars) <> 0 then
      ActivateTimer(Sender as TCustomSynEdit);
  end;
end;

procedure TSynCompletionProposal.ActivateTimer(ACurrentEditor: TCustomSynEdit);
begin
  if Assigned(FTimer) then
  begin
    Form.CurrentEditor := ACurrentEditor;
    FTimer.Enabled := True;
  end;
end;

procedure TSynCompletionProposal.DeactivateTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
  end;
end;


procedure TSynCompletionProposal.HandleDblClick(Sender: TObject);
begin
  HandleOnValidate(Sender, [], #0);
end;

destructor TSynCompletionProposal.Destroy;
begin
  if Form.Visible then
    CancelCompletion;
  Editor := nil;
  while fEditors.Count <> 0 do
    RemoveEditor(TCustomSynEdit(FEditors.Last));

  inherited;

  fEditors.Free;
end;

procedure TSynCompletionProposal.TimerExecute(Sender: TObject);
begin
  if not Assigned(FTimer) then exit;
  FTimer.Enabled := False;
  if Application.Active then
  begin
    DoExecute(Form.CurrentEditor as TCustomSynEdit);
    FNoNextKey := False;
  end else if Form.Visible then begin
    Form.Hide;
    Form.PopupParent := nil;
  end;
end;

function TSynCompletionProposal.GetTimerInterval: Integer;
begin
  Result := FTimerInterval;
end;

procedure TSynCompletionProposal.SetTimerInterval(const Value: Integer);
begin
  FTimerInterval := Value;
  if Assigned(FTimer) then
    FTimer.Interval := Value;
end;

procedure TSynCompletionProposal.SetOptions(const Value: TSynCompletionOptions);
begin
  inherited;

  if scoUseBuiltInTimer in Value then
  begin
    if not(Assigned(FTimer)) then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := False;
      FTimer.Interval := FTimerInterval;
      FTimer.OnTimer := TimerExecute;
    end;
  end else begin
    if Assigned(FTimer) then
    begin
      FreeAndNil(FTimer);
    end;
  end;

end;

procedure TSynCompletionProposal.ExecuteEx(s: string; x, y: integer;
  Kind: SynCompletionType);
begin
  inherited;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TSynCompletionProposal.AddEditor(AEditor: TCustomSynEdit);
var
  i : integer;
begin
  i := fEditors.IndexOf(AEditor);
  if i = -1 then begin
    AEditor.FreeNotification(Self);
    fEditors.Add(AEditor);
    AEditor.AddKeyDownHandler(EditorKeyDown);
    AEditor.AddKeyPressHandler(EditorKeyPress);
    AEditor.RegisterCommandHandler(HookedEditorCommand, Self);
  end;
end;

function TSynCompletionProposal.EditorsCount: integer;
begin
  result := fEditors.count;
end;

function TSynCompletionProposal.GetEditor(i: integer): TCustomSynEdit;
begin
  if (i < 0) or (i >= EditorsCount) then
    Result := nil
  else
    Result := fEditors[i];
end;

function TSynCompletionProposal.RemoveEditor(AEditor: TCustomSynEdit): boolean;
var
  i: integer;
begin
  i := fEditors.Remove(AEditor);
  result := i <> -1;
  if result then begin
    if Form.CurrentEditor = AEditor then
    begin
      if Form.Visible then
        CancelCompletion;
      Form.CurrentEditor := nil;
    end;
    AEditor.RemoveKeyDownHandler(EditorKeyDown);
    AEditor.RemoveKeyPressHandler(EditorKeyPress);
    AEditor.UnregisterCommandHandler(HookedEditorCommand);
    RemoveFreeNotification( AEditor );
    if fEditor = AEditor then
      fEditor := nil;
  end;
end;

procedure TSynCompletionProposal.DoExecute(AEditor: TCustomSynEdit);
var
  p: TPoint;
  i: integer;
begin
  i := FEditors.IndexOf(AEditor);
  if i <> -1 then
    with AEditor do
    begin
      if (DefaultType <> ctCode) or not ReadOnly then
      begin
        if DefaultType = ctHint then
          GetCursorPos(P)
        else
        begin
          p := ClientToScreen(RowColumnToPixels(DisplayXY));
          Inc(p.y, LineHeight);
        end;

        Form.CurrentEditor := AEditor;

        FPreviousToken := GetPreviousToken(Form.CurrentEditor as TCustomSynEdit);
        ExecuteEx(GetCurrentInput(AEditor), p.x, p.y, DefaultType);
        FNoNextKey := (DefaultType = ctCode) and FCanExecute and Form.Visible;
      end;
    end;
end;

procedure TSynCompletionProposal.InternalCancelCompletion;
begin
  if Assigned(FTimer) then FTimer.Enabled := False;
  FNoNextKey := False;
  if (Form.Visible) then
  begin
    Deactivate;
    Form.Hide;
    Form.PopupParent := nil;
  end;
end;

procedure TSynCompletionProposal.CancelCompletion;
begin
  InternalCancelCompletion;
  if Assigned(OnCancelled) then OnCancelled(Self);
end;

procedure TSynCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  if (DisplayType = ctParams) then CancelCompletion;
end;

procedure TSynCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand;
  var AChar: WideChar; Data, HandlerData: Pointer);
begin
  inherited;

  if AfterProcessing and Form.Visible then
  begin
    case DisplayType of
    ctCode:
      begin

      end;
    ctHint:
      begin
          CancelCompletion
      end;
    ctParams:
      case Command of
        // So that param completion is not hidden when you display code completion
        //ecGotFocus, ecLostFocus:
        //  CancelCompletion;
        ecLineBreak:
          DoExecute(Sender as TCustomSynEdit);
        ecChar:
          if AChar = #27 then
            CancelCompletion
          else if Assigned(OnParameterToken) then
            OnParameterToken(Self, AChar, FForm.FCurrentIndex, FForm.FCurrentLevel)
          else if not ((Sender as TCustomSynEdit).IsIdentChar(AChar) or
            TCustomSynEdit(Sender).IsWhiteChar(AChar))
          then
            DoExecute(Sender as TCustomSynEdit);
      else
        DoExecute(Sender as TCustomSynEdit);
      end;
    end;
  end
  else
  if (not Form.Visible) and Assigned(FTimer) then
  begin
    if (Command = ecChar) then
      if (Pos(AChar, TriggerChars) = 0) then
        FTimer.Enabled := False
      else
    else
      FTimer.Enabled := False;
  end;

end;

procedure TSynCompletionProposal.ActivateCompletion;
begin
  DoExecute(Editor);
  fNoNextKey := False;   //  Synedit bug report 1496151
end;



{ TSynAutoComplete }

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;
  FDoLookup := True;
  CreateInternalCompletion;
  FEndOfTokenChr := DefaultEndOfTokenChr;
  fAutoCompleteList := TStringList.Create;
  fNoNextKey := false;
  fShortCut := Vcl.Menus.ShortCut(Ord(' '), [ssShift]);
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.Destroy;
begin
  Editor := nil;
  if Assigned(FInternalCompletion) then
  begin
    FInternalCompletion.Free;
    FInternalCompletion := nil;
  end;
  inherited;
  fAutoCompleteList.free;
end;

procedure TSynAutoComplete.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  if not (Sender as TCustomSynEdit).ReadOnly and
    (Shift = ShortCutShift) and (Key = ShortCutKey) then
  begin
    Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
    fNoNextKey := True;
    Key := 0;
  end;
end;

procedure TSynAutoComplete.EditorKeyPress(Sender: TObject; var Key: WideChar);
begin
  if fNoNextKey then
  begin
    fNoNextKey := False;
    Key := #0;
  end;
end;

procedure TSynAutoComplete.Execute(Token: string; Editor: TCustomSynEdit);
begin
  ExecuteEx(Token, Editor, FDoLookup);
end;

procedure TSynAutoComplete.ExecuteEx(Token: string; Editor: TCustomSynEdit;
  LookupIfNotExact: Boolean);
var
  Temp: string;
  i, j: integer;
  StartOfBlock: TBufferCoord;
  ChangedIndent: Boolean;
  ChangedTrailing: Boolean;
  TmpOptions: TSynEditorOptions;
  OrigOptions: TSynEditorOptions;
  BeginningSpaceCount : Integer;
  Spacing: string;
begin
  if Assigned(OnBeforeExecute) then OnBeforeExecute(Self);
  try
    i := AutoCompleteList.IndexOf(Token);
    if (Length(Token) > 0) and (i <> -1) then
    begin
      Editor.Lines.BeginUpdate;
      try
        TmpOptions := Editor.Options;
        OrigOptions := Editor.Options;
        ChangedIndent := eoAutoIndent in TmpOptions;
        ChangedTrailing := eoTrimTrailingSpaces in TmpOptions;

        if ChangedIndent then Exclude(TmpOptions, eoAutoIndent);
        if ChangedTrailing then Exclude(TmpOptions, eoTrimTrailingSpaces);

        if ChangedIndent or ChangedTrailing then
          Editor.Options := TmpOptions;

        Editor.BeginUndoBlock;
        try
          fNoNextKey := True;
          for j := 1 to Length(Token) do
            Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
          BeginningSpaceCount := Editor.DisplayX - 1;
          if not(eoTabsToSpaces in Editor.Options) and
            (BeginningSpaceCount >= Editor.TabWidth)
          then
            Spacing := StringofChar(#9, BeginningSpaceCount div Editor.TabWidth)
              + StringofChar(' ', BeginningSpaceCount mod Editor.TabWidth)
          else
            Spacing := StringofChar(' ', BeginningSpaceCount);

          inc(i);
          if (i < AutoCompleteList.Count) and
             (Length(AutoCompleteList[i]) > 0) and
             (AutoCompleteList[i][1] = '|') then
          begin
            inc(i);
          end;
          StartOfBlock.Char := -1;
          StartOfBlock.Line := -1;
          while (i < AutoCompleteList.Count) and
                (length(AutoCompleteList[i]) > 0) and
                (AutoCompleteList[i][1] = '=') do
          begin
      {      for j := 0 to PrevSpace - 1 do
              Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);}
            Temp := AutoCompleteList[i];
            for j := 2 to Length(Temp) do begin
              if (Temp[j] = #9) then
                Editor.CommandProcessor(ecTab, Temp[j], nil)
              else
                Editor.CommandProcessor(ecChar, Temp[j], nil);
              if (Temp[j] = '|') then
                StartOfBlock := Editor.CaretXY
            end;
            inc(i);
            if (i < AutoCompleteList.Count) and
               (length(AutoCompleteList[i]) > 0) and
               (AutoCompleteList[i][1] = '=') then
            begin
               Editor.CommandProcessor (ecLineBreak,' ',nil);
               for j := 1 to length(Spacing) do
                 if (Spacing[j] = #9) then
                   Editor.CommandProcessor(ecTab, #9, nil)
                 else
                   Editor.CommandProcessor (ecChar, ' ', nil);
            end;
          end;
          if (StartOfBlock.Char <> -1) and (StartOfBlock.Line <> -1) then begin
            Editor.CaretXY := StartOfBlock;
            Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
          end;

          if ChangedIndent or ChangedTrailing then Editor.Options := OrigOptions;
        finally
          Editor.EndUndoBlock;
        end;
        fNoNextKey := False;
      finally
        Editor.Lines.EndUpdate;
      end;
    end
    else if LookupIfNotExact and Assigned(FInternalCompletion) then
    begin
      FInternalCompletion.AddEditor(Editor);
      FInternalCompletion.ClearList;
      for i := 0 to AutoCompleteList.Count - 1 do
        if (Length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') and (AutoCompleteList[i][1] <> '|') then
        begin
          if (i + 1 < AutoCompleteList.Count) and (length(AutoCompleteList[i + 1]) > 0) and
            (AutoCompleteList[i + 1][1] = '|') then
          begin
            Temp := _(AutoCompleteList[i + 1]);
            Delete(Temp, 1, 1);
          end
          else
            Temp := AutoCompleteList[i];
          Temp := '\style{+B}' + AutoCompleteList[i] + '\style{-B}\column{}' + Temp;
          FInternalCompletion.ItemList.Add(Temp);
          FInternalCompletion.InsertList.Add(AutoCompleteList[i]);
        end;
      FInternalCompletion.DoExecute(Editor);
    end;
  finally
    if Assigned(OnAfterExecute) then OnAfterExecute(Self);
  end;
end;

procedure TSynAutoComplete.DoInternalAutoCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: WideChar);
begin
  ExecuteEx(GetPreviousToken(Editor), Editor, False);
  FInternalCompletion.Editor := nil;
end;

function TSynAutoComplete.GetPreviousToken(Editor: TCustomSynEdit): string;
var
  s: string;
  i: Integer;
begin
  Result := '';
  if Editor <> nil then
  begin
    s := Editor.LineText;
    i := Editor.CaretX - 1;
    if i <= Length (s) then
    begin
      while (i > 0) and (s[i] > ' ') and (Pos(s[i], FEndOfTokenChr) = 0) do
        Dec(i);
      Result := copy(s, i + 1, Editor.CaretX - i - 1);
    end;
  end
end;

procedure TSynAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (Editor = AComponent) then
    Editor := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if Editor <> nil then
    begin
      Editor.RemoveKeyDownHandler( EditorKeyDown );
      Editor.RemoveKeyPressHandler( EditorKeyPress );
      RemoveFreeNotification( Editor );
    end;
    fEditor := Value;
    if Editor <> nil then
    begin
      Editor.AddKeyDownHandler( EditorKeyDown );
      Editor.AddKeyPressHandler( EditorKeyPress );
      FreeNotification( Editor );
    end;
  end;
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then
  begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

procedure TSynAutoComplete.SetDoLookup(const Value: Boolean);
begin
  FDoLookup := Value;
  if FDoLookup and not Assigned(FInternalCompletion) then
    CreateInternalCompletion
  else if not FDoLookup and Assigned(FInternalCompletion) then begin
    FInternalCompletion.Free;
    FInternalCompletion := nil;
  end;
end;

procedure TSynAutoComplete.CreateInternalCompletion;
begin
  FInternalCompletion := TSynCompletionProposal.Create(Self);
  FInternalCompletion.Options := DefaultProposalOptions + [scoUsePrettyText] - [scoUseBuiltInTimer];
  FInternalCompletion.EndOfTokenChr := FEndOfTokenChr;
  FInternalCompletion.ShortCut := 0;
  FInternalCompletion.OnAfterCodeCompletion := DoInternalAutoCompletion;
  FInternalCompletion.Columns.Add;
  FInternalCompletion.Width := 350;
end;

function TSynAutoComplete.GetOptions: TSynCompletionOptions;
begin
  Result := FOptions;
end;

procedure TSynAutoComplete.SetOptions(const Value: TSynCompletionOptions);
begin
  FOptions := Value;
  if Assigned(FInternalCompletion) then
    FInternalCompletion.Options := FOptions + [scoUsePrettyText] - [scoUseBuiltInTimer];
end;

procedure TSynAutoComplete.CancelCompletion;
begin
  if Assigned(FInternalCompletion) then
    FInternalCompletion.CancelCompletion;
end;

function TSynAutoComplete.GetCompletionProposal: TSynCompletionProposal;
begin
  Result := FInternalCompletion;
end;

function TSynAutoComplete.GetExecuting: Boolean;
begin
  if Assigned(FInternalCompletion) then
    Result := FInternalCompletion.Form.Visible
  else Result := False;
end;

end.
