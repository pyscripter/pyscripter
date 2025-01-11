{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

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

unit dlgSynEditOptions;

interface

uses
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.WinXPanels,
  TB2Item,
  SpTBXEditors,
  SpTBXItem,
  SpTBXExtEditors,
  SpTBXTabs,
  SynEdit,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditKeyCmds,
  dlgPyIDEBase;

type
  TSynEditorOptionsUserCommand = procedure(AUserCommand: Integer;
                                           var ADescription: string) of object;

  //NOTE: in order for the user commands to be recorded correctly, you must
  //      put the command itself in the object property.
  //      you can do this like so:
  //
  //      StringList.AddObject('ecSomeCommand', TObject(ecSomeCommand))
  //
  //      where ecSomeCommand is the command that you want to add

  TSynEditorOptionsAllUserCommands = procedure(ACommands: TStrings) of object;

  TSynEditorOptionsContainer = class;

  TEditorOptionsDialog = class(TPyIDEDlgBase)
    gbBookmarks: TGroupBox;
    gbLineSpacing: TGroupBox;
    gbGutter: TGroupBox;
    gbRightEdge: TGroupBox;
    gbEditorFont: TGroupBox;
    gbOptions: TGroupBox;
    gbCaret: TGroupBox;
    Panel3: TPanel;
    FontDialog: TFontDialog;
    gbKeyStrokes: TGroupBox;
    pnlGutterFontDisplay: TPanel;
    GroupBox1: TGroupBox;
    SynSyntaxSample: TSynEdit;
    GroupBox2: TGroupBox;
    btnAddKey: TButton;
    btnRemKey: TButton;
    btnUpdateKey: TButton;
    cbxElementBold: TCheckBox;
    cbxElementItalic: TCheckBox;
    cbxElementUnderline: TCheckBox;
    cbxElementStrikeout: TCheckBox;
    Label3: TLabel;
    Label10: TLabel;
    Label1: TLabel;
    lblGutterFont: TLabel;
    labFont: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    cInsertCaret: TComboBox;
    cOverwriteCaret: TComboBox;
    cKeyCommand: TComboBox;
    cbHighlighters: TComboBox;
    eRightEdge: TEdit;
    eLineSpacing: TEdit;
    eTabWidth: TEdit;
    lbElements: TSpTBXListBox;
    cbRightEdgeColor: TSpTBXColorEdit;
    cbGutterColor: TSpTBXColorEdit;
    cbActiveLineColor: TSpTBXColorEdit;
    cbElementForeground: TSpTBXColorEdit;
    cbElementBackground: TSpTBXColorEdit;
    TabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    Display: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    Options: TSpTBXTabSheet;
    SpTBXTabItem3: TSpTBXTabItem;
    KeyStrokes: TSpTBXTabSheet;
    SpTBXTabItem4: TSpTBXTabItem;
    Color: TSpTBXTabSheet;
    KeyList: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    SpTBXTabItem5: TSpTBXTabItem;
    ColorThemes: TSpTBXTabSheet;
    SynThemeSample: TSynEdit;
    SpTBXLabel1: TLabel;
    lbColorThemes: TListBox;
    SpTBXLabel2: TLabel;
    btnApplyTheme: TButton;
    GridPanel1: TGridPanel;
    StackPanel1: TStackPanel;
    StackPanel2: TStackPanel;
    ckRightMouseMoves: TCheckBox;
    ckEnhanceEndKey: TCheckBox;
    ckEnhanceHomeKey: TCheckBox;
    ckTabsToSpaces: TCheckBox;
    ckSmartTabDelete: TCheckBox;
    ckSmartTabs: TCheckBox;
    ckTabIndent: TCheckBox;
    ckKeepCaretX: TCheckBox;
    ckWordWrap: TCheckBox;
    ckDragAndDropEditing: TCheckBox;
    ckAutoIndent: TCheckBox;
    ckTrimTrailingSpaces: TCheckBox;
    ckShowSpecialChars: TCheckBox;
    ckDisableScrollArrows: TCheckBox;
    ckGroupUndo: TCheckBox;
    ckHideShowScrollbars: TCheckBox;
    ckScrollHintFollows: TCheckBox;
    ckShowScrollHint: TCheckBox;
    ckScrollPastEOL: TCheckBox;
    ckScrollPastEOF: TCheckBox;
    ckScrollByOneLess: TCheckBox;
    ckHalfPageScroll: TCheckBox;
    ckGutterAutosize: TCheckBox;
    ckGutterShowLineNumbers: TCheckBox;
    ckGutterShowLeaderZeros: TCheckBox;
    ckGutterVisible: TCheckBox;
    ckGutterStartAtZero: TCheckBox;
    cbGutterFont: TCheckBox;
    ckGutterGradient: TCheckBox;
    ckBookmarkKeys: TCheckBox;
    ckBookmarkVisible: TCheckBox;
    cbApplyToAll: TCheckBox;
    btnGutterFont: TButton;
    btnFont: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    ckShowLigatures: TCheckBox;
    EDigits: TEdit;
    lDigits: TLabel;
    procedure SynSyntaxSampleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnUpdateKeyClick(Sender: TObject);
    procedure btnAddKeyClick(Sender: TObject);
    procedure btnRemKeyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnGutterFontClick(Sender: TObject);
    procedure cbGutterFontClick(Sender: TObject);
    procedure cbHighlightersChange(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure cbElementForegroundChange(Sender: TObject);
    procedure cbElementBackgroundChange(Sender: TObject);
    procedure cbxElementBoldClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure KeyListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lbColorThemesClick(Sender: TObject);
    procedure btnApplyThemeClick(Sender: TObject);
    procedure cKeyCommandChange(Sender: TObject);
    procedure ckGutterAutosizeClick(Sender: TObject);
  private
    FHotKeyEditor1: TSynHotKey;
    FHotKeyEditor2: TSynHotKey;

    FHandleChanges: Boolean;  //Normally true, can prevent unwanted execution of event handlers

    FSynEdit: TSynEditorOptionsContainer;
    FUserCommand: TSynEditorOptionsUserCommand;
    FAllUserCommands: TSynEditorOptionsAllUserCommands;
    FExtended: Boolean;
    FColorTheme: string;
    FBgColor: TColor;

    procedure GetData;
    procedure PutData;
    procedure EditStrCallback(const CmdName: string);
    procedure FillInKeystrokeInfo(AKey: TSynEditKeyStroke; AItem: TListItem);
    procedure UpdateKey(AKey: TSynEditKeyStroke);
    function SelectedHighlighter:TSynCustomHighlighter;
    procedure EnableColorItems(AEnable: Boolean);
    procedure UpdateColorFontStyle;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message CM_DIALOGCHAR;
  public
    function Execute(EditOptions: TSynEditorOptionsContainer): Boolean;
    property GetUserCommandNames: TSynEditorOptionsUserCommand read FUserCommand
      write FUserCommand;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read FAllUserCommands
      write FAllUserCommands;
    property UseExtendedStrings: Boolean read FExtended write FExtended;
    property ColorTheme: string read FColorTheme write FColorTheme;
  end;

  TSynHighlighterCountEvent = procedure(Sender:TObject; var Count:Integer) of object;
  TSynGetHighlighterEvent = procedure(Sender:TObject; Index:Integer;
    var SynHighlighter:TSynCustomHighlighter) of object;
  TSynSetHighlighterEvent = procedure(Sender:TObject; Index:Integer;
    SynHighlighter:TSynCustomHighlighter) of object;
  TSynOptionPage = (soDisplay, soOptions, soKeystrokes, soColor);
  TSynOptionPages = set of TSynOptionPage;

  TSynEditOptionsDialog = class(TComponent)
  private
    FForm: TEditorOptionsDialog;
    FPages: TSynOptionPages;
    FHighlighterCountEvent: TSynHighlighterCountEvent;
    FGetHighlighterEvent: TSynGetHighlighterEvent;
    FSetHighlighterEvent: TSynSetHighlighterEvent;
    FHighlighters: TList;
    FColorThemeHighlighter: TSynCustomHighlighter;
    function GetUserCommandNames: TSynEditorOptionsUserCommand;
    procedure SetUserCommandNames(
      const Value: TSynEditorOptionsUserCommand);
    function GetUserCommands: TSynEditorOptionsAllUserCommands;
    procedure SetUserCommands(
      const Value: TSynEditorOptionsAllUserCommands);
    function GetExtended: Boolean;
    procedure SetExtended(const Value: Boolean);
    function GetOptionPages: TSynOptionPages;
    procedure SetOptionPages(const Value: TSynOptionPages);
    procedure ClearHighlighters;
  public
    class var HighlighterFileDir: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(EditOptions: TSynEditorOptionsContainer): Boolean;
    procedure UpdateHighlighters;
    property Form: TEditorOptionsDialog read FForm;
  published
    property GetUserCommand: TSynEditorOptionsUserCommand
      read GetUserCommandNames
      write SetUserCommandNames;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read GetUserCommands
      write SetUserCommands;
    property UseExtendedStrings: Boolean read GetExtended write SetExtended;
    property VisiblePages: TSynOptionPages read GetOptionPages write SetOptionPages;
    property OnGetHighlighterCount: TSynHighlighterCountEvent read FHighlighterCountEvent write FHighlighterCountEvent;
    property OnGetHighlighter: TSynGetHighlighterEvent read FGetHighlighterEvent write FGetHighlighterEvent;
    property OnSetHighlighter: TSynSetHighlighterEvent read FSetHighlighterEvent write FSetHighlighterEvent;
  end;

  //This class is assignable to a SynEdit without modifying key properties that affect function
  TSynEditorOptionsContainer = class(TComponent)
  private
    FHideSelection: Boolean;
    FWantTabs: Boolean;
    FWordWrap: Boolean;
    FMaxUndo: Integer;
    FExtraLineSpacing: Integer;
    FTabWidth: Integer;
    FRightEdge: Integer;
    FSelectedColor: TSynSelectedColor;
    FIndentGuides: TSynIndentGuides;
    FDisplayFlowControl: TSynDisplayFlowControl;
    FRightEdgeColor: TColor;
    FFont: TFont;
    FBookmarks: TSynBookMarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FKeystrokes: TSynEditKeyStrokes;
    FOptions: TSynEditorOptions;
    FSynGutter: TSynGutter;
    FColor: TColor;
    FActiveLineColor: TColor;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    procedure SetBookMarks(const Value: TSynBookMarkOpt);
    procedure SetFont(const Value: TFont);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetSynGutter(const Value: TSynGutter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    property BookMarkOptions: TSynBookMarkOpt read FBookmarks write SetBookMarks;
  published
    property Options: TSynEditorOptions read FOptions write FOptions;
    property Color: TColor read FColor write FColor;
    property Font: TFont read FFont write SetFont;
    property ExtraLineSpacing: Integer read FExtraLineSpacing write FExtraLineSpacing;
    property Gutter: TSynGutter read FSynGutter write SetSynGutter;
    property RightEdge: Integer read FRightEdge write FRightEdge;
    property RightEdgeColor: TColor read FRightEdgeColor write FRightEdgeColor;
    property WantTabs: Boolean read FWantTabs write FWantTabs;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property InsertCaret: TSynEditCaretType read FInsertCaret write FInsertCaret;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret write FOverwriteCaret;
    property HideSelection: Boolean read FHideSelection write FHideSelection;
    property MaxUndo: Integer read FMaxUndo write FMaxUndo;
    property SelectedColor: TSynSelectedColor read FSelectedColor;
    property IndentGuides: TSynIndentGuides read FIndentGuides;
    property DisplayFlowControl: TSynDisplayFlowControl read FDisplayFlowControl;
    property TabWidth: Integer read FTabWidth write FTabWidth;
    property Keystrokes: TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
    property ActiveLineColor: TColor read FActiveLineColor write FActiveLineColor;
    property VisibleSpecialChars: TSynVisibleSpecialChars
      read FVisibleSpecialChars write FVisibleSpecialChars;
  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.Themes,
  uCommonFunctions,
  JvGnugettext,
  StringResources,
  JvAppStorage,
  JvAppIniStorage;

{ TSynEditOptionsDialog }

constructor TSynEditOptionsDialog.Create(AOwner: TComponent);
begin
  inherited;
  FForm:= TEditorOptionsDialog.Create(Self);
  FPages := [soDisplay, soOptions, soKeystrokes];
  FHighlighters := TList.Create;
end;

destructor TSynEditOptionsDialog.Destroy;
begin
  ClearHighlighters;
  FreeAndNil(FColorThemeHighlighter);
  FHighlighters.Free;
  FForm.Free;
  inherited;
end;

function TSynEditOptionsDialog.Execute(EditOptions: TSynEditorOptionsContainer): Boolean;
type
  TSynHClass = class of TSynCustomHighlighter;
var
   HCount: Integer;
   SynH: TSynCustomHighlighter;
   InternalSynH: TSynCustomHighlighter;
   SynHClass: TSynHClass;
   FileName: string;
begin
  if soDisplay in FPages then
     FForm.Display.TabVisible := True
  else
     FForm.Display.TabVisible := False;

  if soOptions in FPages then
     FForm.Options.TabVisible := True
  else
     FForm.Options.TabVisible := False;

  if soKeystrokes in FPages then
     FForm.KeyStrokes.TabVisible := True
  else
     FForm.KeyStrokes.TabVisible := False;

  if soColor in FPages then
  begin
     if Assigned(FHighlighterCountEvent) then
     begin
        HCount := 0;
        FHighlighterCountEvent(Self, HCount);
     end;

     if (HCount > 0) and Assigned(FGetHighlighterEvent) then
     begin
        if FForm.cbHighlighters.Items.Count <> HCount then
        begin
           FForm.cbHighlighters.Items.Clear;

           for var I := 0 to HCount - 1 do
           begin
              FGetHighlighterEvent(Self, I, SynH);
              if Assigned(SynH) then
              begin
                 SynHClass := TSynHClass(SynH.ClassType);
                 InternalSynH := SynHClass.Create(nil);
                 InternalSynH.Assign(SynH);
                 FHighlighters.Add(InternalSynH);
                 FForm.cbHighlighters.Items.AddObject(_(InternalSynH.FriendlyLanguageName), InternalSynH);

                 if (InternalSynH.FriendlyLanguageName = 'Python') or
                    ((InternalSynH.FriendlyLanguageName = 'Python Interpreter') and
                   not Assigned(FColorThemeHighlighter)) then
                 begin
                   FColorThemeHighlighter := SynHClass.Create(nil);
                   FColorThemeHighlighter.Assign(SynH);
                   FForm.SynThemeSample.Highlighter := FColorThemeHighlighter;
                   FForm.SynThemeSample.Lines.Text := FColorThemeHighlighter.SampleSource;
                   FForm.FBgColor := FColorThemeHighlighter.WhitespaceAttribute.Background;
                 end;
              end;
           end;
        end;
     end;

    if HighlighterFileDir <> '' then
      for FileName in TDirectory.GetFiles(HighlighterFileDir,'*.ini') do
        FForm.lbColorThemes.Items.Add(TPath.GetFileNameWithoutExtension(FileName));

    FForm.Color.TabVisible := True;
    FForm.ColorThemes.TabVisible := True;
  end
  else begin
    FForm.Color.TabVisible := False;
    FForm.ColorThemes.TabVisible := False;
  end;

  //Run the form
  Result:= FForm.Execute(EditOptions);
end;

function TSynEditOptionsDialog.GetUserCommands: TSynEditorOptionsAllUserCommands;
begin
  Result := FForm.GetAllUserCommands;
end;

function TSynEditOptionsDialog.GetUserCommandNames: TSynEditorOptionsUserCommand;
begin
  Result := FForm.GetUserCommandNames;
end;

procedure TSynEditOptionsDialog.SetUserCommands(
  const Value: TSynEditorOptionsAllUserCommands);
begin
  FForm.GetAllUserCommands := Value;
end;

procedure TSynEditOptionsDialog.SetUserCommandNames(
  const Value: TSynEditorOptionsUserCommand);
begin
  FForm.GetUserCommandNames := Value;
end;

function TSynEditOptionsDialog.GetExtended: Boolean;
begin
  Result := FForm.UseExtendedStrings;
end;

procedure TSynEditOptionsDialog.SetExtended(const Value: Boolean);
begin
  FForm.UseExtendedStrings := Value;
end;

function TSynEditOptionsDialog.GetOptionPages: TSynOptionPages;
begin
  Result := FPages;
end;

procedure TSynEditOptionsDialog.SetOptionPages(
  const Value: TSynOptionPages);
begin
  if not FForm.Visible then
    FPages := Value;
end;

procedure TSynEditOptionsDialog.ClearHighlighters;
var
  SynH: TSynCustomHighlighter;
begin
  for var I := 0 to FHighlighters.Count-1 do
  begin
     SynH := FHighlighters[I];
     FHighlighters[I] := nil;
     SynH.Free;
  end;
end;

/// Iterates through the highlighter list and
/// fires FSetHighlighterEvent for each highlighter.
procedure TSynEditOptionsDialog.UpdateHighlighters;
begin
  if Assigned(FSetHighlighterEvent) then
    for var I := 0 to FHighlighters.Count-1 do
       FSetHighlighterEvent(Self, I, FHighlighters[I]);
end;

{ TSynEditorOptionsContainer }

procedure TSynEditorOptionsContainer.Assign(Source: TPersistent);
var
  PPI: Integer;
begin
  if Assigned(Source) and (Source is TCustomSynEdit) then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.BookMarkOptions.Assign(TCustomSynEdit(Source).BookMarkOptions);
    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);
    Self.Keystrokes.Assign(TCustomSynEdit(Source).Keystrokes);
    Self.SelectedColor.Assign(TCustomSynEdit(Source).SelectedColor);
    Self.IndentGuides.Assign(TCustomSynEdit(Source).IndentGuides);
    Self.DisplayFlowControl.Assign(TCustomSynEdit(Source).DisplayFlowControl);

    Self.Color := TCustomSynEdit(Source).Color;
    Self.Options := TCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.HideSelection := TCustomSynEdit(Source).HideSelection;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
    Self.WantTabs := TCustomSynEdit(Source).WantTabs;
    Self.WordWrap := TCustomSynEdit(Source).WordWrap;
    Self.ActiveLineColor := TCustomSynEdit(Source).ActiveLineColor;
    Self.VisibleSpecialChars := TCustomSynEdit(Source).VisibleSpecialChars;
    // store unscaled
    PPI := TCustomSynEdit(Source).CurrentPPI;
    Self.BookMarkOptions.ChangeScale(96, PPI);
    Self.ExtraLineSpacing := MulDiv(Self.ExtraLineSpacing, 96, PPI);
  end else if Assigned(Source) and (Source is TSynEditorOptionsContainer) then
  begin
    Self.Font.Assign(TSynEditorOptionsContainer(Source).Font);
    Self.BookMarkOptions.Assign(TSynEditorOptionsContainer(Source).BookMarkOptions);
    Self.Gutter.Assign(TSynEditorOptionsContainer(Source).Gutter);
    Self.Keystrokes.Assign(TSynEditorOptionsContainer(Source).Keystrokes);
    Self.SelectedColor.Assign(TSynEditorOptionsContainer(Source).SelectedColor);
    Self.IndentGuides.Assign(TSynEditorOptionsContainer(Source).IndentGuides);
    Self.DisplayFlowControl.Assign(TSynEditorOptionsContainer(Source).DisplayFlowControl);
    Self.Color := TSynEditorOptionsContainer(Source).Color;
    Self.Options := TSynEditorOptionsContainer(Source).Options;
    Self.ExtraLineSpacing := TSynEditorOptionsContainer(Source).ExtraLineSpacing;
    Self.HideSelection := TSynEditorOptionsContainer(Source).HideSelection;
    Self.InsertCaret := TSynEditorOptionsContainer(Source).InsertCaret;
    Self.OverwriteCaret := TSynEditorOptionsContainer(Source).OverwriteCaret;
    Self.MaxUndo := TSynEditorOptionsContainer(Source).MaxUndo;
    Self.RightEdge := TSynEditorOptionsContainer(Source).RightEdge;
    Self.RightEdgeColor := TSynEditorOptionsContainer(Source).RightEdgeColor;
    Self.TabWidth := TSynEditorOptionsContainer(Source).TabWidth;
    Self.WantTabs := TSynEditorOptionsContainer(Source).WantTabs;
    Self.WordWrap := TSynEditorOptionsContainer(Source).WordWrap;
    Self.ActiveLineColor := TSynEditorOptionsContainer(Source).ActiveLineColor;
    Self.VisibleSpecialChars := TSynEditorOptionsContainer(Source).VisibleSpecialChars;
  end else
    inherited;
end;

procedure TSynEditorOptionsContainer.AssignTo(Dest: TPersistent);
var
  PPI: Integer;
begin
  if Assigned(Dest) and (Dest is TCustomSynEdit) then
  begin
    TCustomSynEdit(Dest).BeginUpdate;
    try
      TCustomSynEdit(Dest).Font := Self.Font;
      TCustomSynEdit(Dest).BookMarkOptions.Assign(Self.BookMarkOptions);
      TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
      TCustomSynEdit(Dest).Keystrokes.Assign(Self.Keystrokes);
      TCustomSynEdit(Dest).SelectedColor.Assign(Self.SelectedColor);
      TCustomSynEdit(Dest).IndentGuides.Assign(Self.IndentGuides);
      TCustomSynEdit(Dest).DisplayFlowControl.Assign(Self.DisplayFlowControl);
      TCustomSynEdit(Dest).Color := Self.Color;
      TCustomSynEdit(Dest).Options := Self.Options;
      TCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
      TCustomSynEdit(Dest).HideSelection := Self.HideSelection;
      TCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
      TCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
      TCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
      TCustomSynEdit(Dest).RightEdge := Self.RightEdge;
      TCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
      TCustomSynEdit(Dest).TabWidth := Self.TabWidth;
      TCustomSynEdit(Dest).WantTabs := Self.WantTabs;
      TCustomSynEdit(Dest).WordWrap := Self.WordWrap;
      TCustomSynEdit(Dest).ActiveLineColor := Self.ActiveLineColor;
      TCustomSynEdit(Dest).VisibleSpecialChars := Self.VisibleSpecialChars;
      // scale for editor PPI
      PPI := TCustomSynEdit(Dest).CurrentPPI;
      TCustomSynEdit(Dest).BookMarkOptions.ChangeScale(PPI, 96);
      TCustomSynEdit(Dest).ExtraLineSpacing :=
        MulDiv(TCustomSynEdit(Dest).ExtraLineSpacing, PPI, 96);
    finally
      TCustomSynEdit(Dest).EndUpdate;
    end;
  end else
    inherited;
end;

constructor TSynEditorOptionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FBookmarks := TSynBookMarkOpt.Create(Self);
  FKeystrokes := TSynEditKeyStrokes.Create(Self);
  FSynGutter := TSynGutter.Create;
  FSynGutter.AssignableBands := False;
  FSelectedColor := TSynSelectedColor.Create;
  FIndentGuides := TSynIndentGuides.Create;
  FSelectedColor.Foreground := clHighlightText;
  FSelectedColor.Background := clHighlight;
  FDisplayFlowControl := TSynDisplayFlowControl.Create;
  FActiveLineColor := clNone;
  FFont := TFont.Create;
  FFont.Name := DefaultCodeFontName;
  FFont.Size := 10;
  {$IF CompilerVersion >= 36}
  FFont.IsScreenFont := True;
  {$ENDIF}
  Color := clWindow;
  Keystrokes.ResetDefaults;
  Options := SYNEDIT_DEFAULT_OPTIONS;
  ExtraLineSpacing := 0;
  HideSelection := False;
  InsertCaret := ctVerticalLine;
  OverwriteCaret := ctBlock;
  MaxUndo := 0;
  RightEdge := 80;
  RightEdgeColor := clSilver;
  FActiveLineColor := clNone;
  TabWidth := 8;
  WantTabs := True;
  WordWrap := False;
end;

destructor TSynEditorOptionsContainer.Destroy;
begin
  FBookmarks.Free;
  FKeystrokes.Free;
  FSynGutter.Free;
  FSelectedColor.Free;
  FDisplayFlowControl.Free;
  FIndentGuides.Free;
  FFont.Free;
  inherited;
end;

procedure TSynEditorOptionsContainer.SetBookMarks(
  const Value: TSynBookMarkOpt);
begin
  FBookmarks.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetKeystrokes(
  const Value: TSynEditKeyStrokes);
begin
  FKeystrokes.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetSynGutter(const Value: TSynGutter);
begin
  FSynGutter.Assign(Value);
end;

{ TfmEditorOptionsDialog }

function TEditorOptionsDialog.Execute(EditOptions: TSynEditorOptionsContainer): Boolean;
begin
  if (EditOptions = nil) then
  begin
    Result:= False;
    Exit;
  end;
  //Assign the Containers
  FSynEdit:= EditOptions;
  //Get Data
  GetData;
  //Show the form
  Result:= ShowModal = mrOk;
  //PutData
  if Result then PutData;
end;

procedure TEditorOptionsDialog.GetData;
var I: Integer;
    Item: TListItem;
begin
  ckWordWrap.Checked := FSynEdit.WordWrap;
  //Gutter
  ckGutterVisible.Checked:= FSynEdit.Gutter.Visible;
  ckGutterAutosize.Checked:= FSynEdit.Gutter.AutoSize;  //fixed by KF Orig: FSynEdit.Gutter.Visible;
  ckGutterShowLineNumbers.Checked:= FSynEdit.Gutter.ShowLineNumbers;
  ckGutterShowLeaderZeros.Checked:= FSynEdit.Gutter.LeadingZeros;
  ckGutterStartAtZero.Checked:= FSynEdit.Gutter.ZeroStart;
  cbGutterFont.Checked := FSynEdit.Gutter.UseFontStyle;
  cbGutterColor.SelectedColor := FSynEdit.Gutter.Color;
  lblGutterFont.Font.Assign(FSynEdit.Gutter.Font);
  lblGutterFont.Caption:= lblGutterFont.Font.Name + ' ' + IntToStr(lblGutterFont.Font.Size) + 'pt';
  ckGutterGradient.Checked := FSynEdit.Gutter.Gradient;
  EDigits.Text:= IntToStr(FSynEdit.Gutter.DigitCount);
  EDigits.Enabled:= not ckGutterAutosize.Checked;
  //Right Edge
  eRightEdge.Text:= IntToStr(FSynEdit.RightEdge);
  cbRightEdgeColor.SelectedColor:= FSynEdit.RightEdgeColor;
  //ActiveLineColor
  cbActiveLineColor.SelectedColor := FSynEdit.ActiveLineColor;
  //Line Spacing
  eLineSpacing.Text:= IntToStr(FSynEdit.ExtraLineSpacing);
  eTabWidth.Text:= IntToStr(FSynEdit.TabWidth);
  //Bookmarks
  ckBookmarkKeys.Checked:= FSynEdit.BookMarkOptions.EnableKeys;
  ckBookmarkVisible.Checked:= FSynEdit.BookMarkOptions.GlyphsVisible;
  //Font
  labFont.Font.Assign(FSynEdit.Font);
  labFont.Caption:= labFont.Font.Name + ' ' + IntToStr(labFont.Font.Size) + 'pt';
  //Options
  ckAutoIndent.Checked:= eoAutoIndent in FSynEdit.Options;
  ckDragAndDropEditing.Checked:= eoDragDropEditing in FSynEdit.Options;
  ckTabIndent.Checked:= eoTabIndent in FSynEdit.Options;
  ckSmartTabs.Checked:= eoSmartTabs in FSynEdit.Options;
  ckHalfPageScroll.Checked:= eoHalfPageScroll in FSynEdit.Options;
  ckScrollByOneLess.Checked:= eoScrollByOneLess in FSynEdit.Options;
  ckScrollPastEOF.Checked:= eoScrollPastEof in FSynEdit.Options;
  ckScrollPastEOL.Checked:= eoScrollPastEol in FSynEdit.Options;
  ckShowScrollHint.Checked:= eoShowScrollHint in FSynEdit.Options;
  ckTabsToSpaces.Checked:= eoTabsToSpaces in FSynEdit.Options;
  ckTrimTrailingSpaces.Checked:= eoTrimTrailingSpaces in FSynEdit.Options;
  ckKeepCaretX.Checked:= eoKeepCaretX in FSynEdit.Options;
  ckSmartTabDelete.Checked := eoSmartTabDelete in FSynEdit.Options;
  ckRightMouseMoves.Checked := eoRightMouseMovesCursor in FSynEdit.Options;
  ckEnhanceHomeKey.Checked := eoEnhanceHomeKey in FSynEdit.Options;
  ckEnhanceEndKey.Checked := eoEnhanceEndKey in FSynEdit.Options;
  ckScrollHintFollows.Checked := eoScrollHintFollows in FSynEdit.Options;
  ckGroupUndo.Checked := eoGroupUndo in FSynEdit.Options;
  ckDisableScrollArrows.Checked := eoDisableScrollArrows in FSynEdit.Options;
  ckHideShowScrollbars.Checked := eoHideShowScrollbars in FSynEdit.Options;
  ckShowSpecialChars.Checked := FSynEdit.VisibleSpecialChars <> [];
  ckShowLigatures.Checked := eoShowLigatures in FSynEdit.Options;
  //Caret
  cInsertCaret.ItemIndex:= Ord(FSynEdit.InsertCaret);
  cOverwriteCaret.ItemIndex:= Ord(FSynEdit.OverwriteCaret);

  KeyList.Items.BeginUpdate;
  try
    KeyList.Items.Clear;
    for I:= 0 to FSynEdit.Keystrokes.Count-1 do
    begin
      Item:= KeyList.Items.Add;
      FillInKeystrokeInfo(FSynEdit.Keystrokes[I], Item);
      Item.Data:= FSynEdit.Keystrokes[I];
    end;
  finally
    KeyList.Items.EndUpdate;
  end;
  lbColorThemes.ItemIndex:= lbColorThemes.Items.IndexOf(FColorTheme);
end;

procedure TEditorOptionsDialog.PutData;
var
  EdOptions: TSynEditorOptions;
  Digits: Integer;

  procedure SetFlag(AOption: TSynEditorOption; AValue: Boolean);
  begin
    if AValue then
      Include(EdOptions, AOption)
    else
      Exclude(EdOptions, AOption);
  end;
begin
  FSynEdit.WordWrap := ckWordWrap.Checked;
  //Gutter
  FSynEdit.Gutter.Visible:= ckGutterVisible.Checked;
  FSynEdit.Gutter.AutoSize := ckGutterAutosize.Checked;
  FSynEdit.Gutter.ShowLineNumbers:= ckGutterShowLineNumbers.Checked;
  FSynEdit.Gutter.LeadingZeros:= ckGutterShowLeaderZeros.Checked;
  FSynEdit.Gutter.ZeroStart:= ckGutterStartAtZero.Checked;
  FSynEdit.Gutter.Color:= cbGutterColor.SelectedColor;
  FSynEdit.Gutter.UseFontStyle := cbGutterFont.Checked;
  FSynEdit.Gutter.Font.Assign(lblGutterFont.Font);
  FSynEdit.Gutter.Gradient := ckGutterGradient.Checked;
  if ckGutterAutosize.Checked then
    FSynEdit.Gutter.DigitCount:= 2
  else if TryStrToInt(EDigits.Text, Digits) and (2 <= Digits) and (Digits <= 12) then
    FSynEdit.Gutter.DigitCount:= Digits;
  //Right Edge
  FSynEdit.RightEdge:= StrToIntDef(eRightEdge.Text, 80);
  FSynEdit.RightEdgeColor:= cbRightEdgeColor.SelectedColor;
  //ActiveLineColor
  FSynEdit.ActiveLineColor := cbActiveLineColor.SelectedColor;
  //Line Spacing
  FSynEdit.ExtraLineSpacing:= StrToIntDef(eLineSpacing.Text, 0);
  FSynEdit.TabWidth:= StrToIntDef(eTabWidth.Text, 8);
  //Bookmarks
  FSynEdit.BookMarkOptions.EnableKeys:= ckBookmarkKeys.Checked;
  FSynEdit.BookMarkOptions.GlyphsVisible:= ckBookmarkVisible.Checked;
  //Font
  FSynEdit.Font.Assign(labFont.Font);
  //Options
  EdOptions := FSynEdit.Options; //Keep old values for unsupported options
  SetFlag(eoAutoIndent, ckAutoIndent.Checked);
  SetFlag(eoDragDropEditing, ckDragAndDropEditing.Checked);
  SetFlag(eoTabIndent, ckTabIndent.Checked);
  SetFlag(eoSmartTabs, ckSmartTabs.Checked);
  SetFlag(eoHalfPageScroll, ckHalfPageScroll.Checked);
  SetFlag(eoScrollByOneLess, ckScrollByOneLess.Checked);
  SetFlag(eoScrollPastEof, ckScrollPastEOF.Checked);
  SetFlag(eoScrollPastEol, ckScrollPastEOL.Checked);
  SetFlag(eoShowScrollHint, ckShowScrollHint.Checked);
  SetFlag(eoTabsToSpaces, ckTabsToSpaces.Checked);
  SetFlag(eoTrimTrailingSpaces, ckTrimTrailingSpaces.Checked);
  SetFlag(eoKeepCaretX, ckKeepCaretX.Checked);
  SetFlag(eoSmartTabDelete, ckSmartTabDelete.Checked);
  SetFlag(eoRightMouseMovesCursor, ckRightMouseMoves.Checked);
  SetFlag(eoEnhanceHomeKey, ckEnhanceHomeKey.Checked);
  SetFlag(eoEnhanceEndKey, ckEnhanceEndKey.Checked);
  SetFlag(eoGroupUndo, ckGroupUndo.Checked);
  SetFlag(eoScrollHintFollows, ckScrollHintFollows.Checked);
  SetFlag(eoDisableScrollArrows, ckDisableScrollArrows.Checked);
  SetFlag(eoHideShowScrollbars, ckHideShowScrollbars.Checked);
  SetFlag(eoShowLigatures, ckShowLigatures.Checked);
  FSynEdit.Options := EdOptions;
  if ckShowSpecialChars.Checked then
    FSynEdit.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL]
  else
    FSynEdit.VisibleSpecialChars := [];
  //Caret
  FSynEdit.InsertCaret:= TSynEditCaretType(cInsertCaret.ItemIndex);
  FSynEdit.OverwriteCaret:= TSynEditCaretType(cOverwriteCaret.ItemIndex);
end;

procedure TEditorOptionsDialog.FormCreate(Sender: TObject);
begin
  inherited;
  FHandleChanges := True;  //Normally true, can prevent unwanted execution of event handlers

  KeyList.OnSelectItem := KeyListSelectItem;

  FHotKeyEditor1:= TSynHotKey.Create(Self);
  with FHotKeyEditor1 do
  begin
    Parent := gbKeyStrokes;
    Left := PPIScale(185);
    Top := PPIScale(55);
    Width := PPIScale(185);
    Height := PPIScale(21);
    InvalidKeys := [];
    Modifiers := [];
    HotKey := 0;
    TabOrder := 1;
    Font.Color := StyleServices.GetSystemColor(clWindowText);
    Color := StyleServices.GetSystemColor(clWindow);
  end;

  FHotKeyEditor2:= TSynHotKey.Create(Self);
  with FHotKeyEditor2 do
  begin
    Parent := gbKeyStrokes;
    Left := PPIScale(185);
    Top := PPIScale(87);
    Width := PPIScale(185);
    Height := PPIScale(21);
    InvalidKeys := [];
    Modifiers := [];
    HotKey := 0;
    TabOrder := 2;
    Font.Color := StyleServices.GetSystemColor(clWindowText);
    Color := StyleServices.GetSystemColor(clWindow);
  end;
end;

procedure TEditorOptionsDialog.btnFontClick(Sender: TObject);
begin
  labFont.Font.PixelsPerInch := FCurrentPPI;
  FontDialog.Font := labFont.Font;
  if FontDialog.Execute then
  begin
    labFont.Font.Assign(FontDialog.Font);
    labFont.Caption:= labFont.Font.Name;
    labFont.Caption:= labFont.Font.Name + ' ' + IntToStr(labFont.Font.Size) + 'pt';
  end;
  {$IF CompilerVersion < 36}
  labFont.Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

procedure TEditorOptionsDialog.UpdateKey(AKey: TSynEditKeyStroke);
begin
  AKey.Command := NativeUInt(cKeyCommand.Items.Objects[cKeyCommand.ItemIndex]);

  AKey.ShortCut := FHotKeyEditor1.HotKey;
  AKey.ShortCut2:= FHotKeyEditor2.HotKey;
end;

procedure TEditorOptionsDialog.btnUpdateKeyClick(Sender: TObject);

var
  OldShortcut: TShortCut;
  OldShortcut2: TShortCut;
  Key: TSynEditKeyStroke;
begin
  if KeyList.Selected = nil then Exit;
  if cKeyCommand.ItemIndex < 0 then Exit;

  Key := TSynEditKeyStroke(KeyList.Selected.Data);
  OldShortcut  := Key.ShortCut;
  OldShortcut2 := Key.ShortCut2;
  try
    UpdateKey(Key);
  except
     on E: ESynKeyError do begin
       Key.ShortCut := OldShortcut;
       Key.ShortCut2 := OldShortcut2;
       StyledMessageDlg(_(SDuplicateKey), mtError, [TMsgDlgBtn.mbOK], 0);
     end;
  end;
  FillInKeystrokeInfo(TSynEditKeyStroke(KeyList.Selected.Data), KeyList.Selected);
end;

procedure TEditorOptionsDialog.btnApplyThemeClick(Sender: TObject);
var
  AppStorage: TJvAppIniFileStorage;
  FileName: string;
  LineColor: TColor;
begin
  if lbColorThemes.ItemIndex >= 0 then
  begin
    FileName := IncludeTrailingPathDelimiter(TSynEditOptionsDialog.HighlighterFileDir) +
                   lbColorThemes.Items[lbColorThemes.ItemIndex]+ '.ini';
    AppStorage := TJvAppIniFileStorage.Create(nil);
    try
      AppStorage.FlushOnDestroy := False;
      AppStorage.Location := flCustom;
      AppStorage.FileName := FileName;
      for var I := 0 to cbHighlighters.Items.Count - 1 do
      begin
        TSynCustomHighlighter(cbHighlighters.Items.Objects[I]).BeginUpdate;
        try
          AppStorage.ReadPersistent('Highlighters\'+
            TSynCustomHighlighter(cbHighlighters.Items.Objects[I]).FriendlyLanguageName,
            TPersistent(cbHighlighters.Items.Objects[I]));
        finally
          TSynCustomHighlighter(cbHighlighters.Items.Objects[I]).EndUpdate;
        end;
      end;
    finally
        AppStorage.Free;
    end;
    FColorTheme:= lbColorThemes.Items[lbColorThemes.ItemIndex];

    // Adjust active line color
    if (FSynEdit.ActiveLineColor <> clNone) and Assigned(SynThemeSample.Highlighter) then
    begin
      LineColor := SynThemeSample.Highlighter.WhitespaceAttribute.Background;
      // Only change if we swithcing from dart to light or vice versa.
      if IsColorDark(LineColor) xor IsColorDark(FBgColor) then
      begin
        if IsColorDark(LineColor) then
          cbActiveLineColor.SelectedColor := LightenColor(LineColor, 20)
        else
          cbActiveLineColor.SelectedColor := DarkenColor(LineColor, 20);
      end;
    end;
  end;
end;

procedure TEditorOptionsDialog.btnAddKeyClick(Sender: TObject);
var
  Item: TListItem;
begin
  if cKeyCommand.ItemIndex < 0 then Exit;
  Item:= KeyList.Items.Add;
  try
    Item.Data:= FSynEdit.Keystrokes.Add;
    UpdateKey(TSynEditKeyStroke(Item.Data));
    FillInKeystrokeInfo(TSynEditKeyStroke(Item.Data), Item);
    Item.Selected:= True;
  except
     on E: ESynKeyError do begin
       StyledMessageDlg(_(SDuplicateKey), mtError, [TMsgDlgBtn.mbOK], 0);
       TSynEditKeyStroke(Item.Data).Free;
       Item.Delete;
     end;
  end;
  Item.MakeVisible(True);
end;

procedure TEditorOptionsDialog.btnRemKeyClick(Sender: TObject);
begin
  if KeyList.Selected = nil then Exit;
  TSynEditKeyStroke(KeyList.Selected.Data).Free;
  KeyList.Selected.Delete;
end;

procedure TEditorOptionsDialog.EditStrCallback(const CmdName: string);
begin
  //Add the Item
  if FExtended then
    cKeyCommand.Items.AddObject(_(CmdName),
      TObject(NativeUInt(ConvertExtendedToCommand(CmdName))))
  else
    cKeyCommand.Items.AddObject(CmdName,
      TObject(NativeUInt(ConvertCodeStringToCommand(CmdName))));
end;

procedure TEditorOptionsDialog.FormShow(Sender: TObject);
var
 Commands: TStringList;
begin
  //We need to do this now because it will not have been assigned when
  //create occurs
  cKeyCommand.Items.Clear;
  //Start the callback to add the strings
  if FExtended then
    GetEditorCommandExtended(EditStrCallback)
  else
    GetEditorCommandValues(EditStrCallback);
  //Now add in the user defined ones if they have any
  if Assigned(FAllUserCommands) then
  begin
    Commands := TStringList.Create;
    try
      FAllUserCommands(Commands);
      for var I := 0 to Commands.Count - 1 do
        if Commands.Objects[I] <> nil then
          cKeyCommand.Items.AddObject(Commands[I], Commands.Objects[I]);
    finally
      Commands.Free;
    end;
  end;
  if (KeyList.Items.Count > 0) then KeyList.Items[0].Selected:= True;

  TabControl.ActivePage := Display;

  if Color.TabVisible then
  begin
    if cbHighlighters.Items.Count > 0 then
      cbHighlighters.ItemIndex := 0;

    if cbHighlighters.ItemIndex = -1 then
      EnableColorItems(False)  //If there is still no selected item then disable controls
    else
      cbHighlightersChange(cbHighlighters);  //run OnChange handler (it wont be fired on setting the itemindex prop)
  end;

  // DPI Scaling
  StackPanel1.Spacing := MulDiv(StackPanel1.Spacing, FCurrentPPI, 96);
  StackPanel2.Spacing := MulDiv(StackPanel2.Spacing, FCurrentPPI, 96);
end;

procedure TEditorOptionsDialog.KeyListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not (Selected and Assigned(Item)) then Exit;
  cKeyCommand.OnChange := nil;
  cKeyCommand.Text      := Item.Caption;
  cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf(Item.Caption);
  FHotKeyEditor1.HotKey     := TSynEditKeyStroke(Item.Data).ShortCut;
  FHotKeyEditor2.HotKey     := TSynEditKeyStroke(Item.Data).ShortCut2;
  btnUpdateKey.Enabled := True;
  btnRemKey.Enabled := True;
  cKeyCommand.OnChange := cKeyCommandChange;
end;

procedure TEditorOptionsDialog.btnOkClick(Sender: TObject);
begin
  // Bug fix of SpTBXColorEdit
  btnOk.SetFocus;
  Application.ProcessMessages;

  ModalResult:= mrOk;
end;

procedure TEditorOptionsDialog.btnGutterFontClick(Sender: TObject);
begin
  lblGutterFont.Font.PixelsPerInch := FCurrentPPI;
  FontDialog.Font := lblGutterFont.Font;
  if FontDialog.Execute then
  begin
    lblGutterFont.Font.Assign(FontDialog.Font);
    lblGutterFont.Caption:= lblGutterFont.Font.Name + ' ' + IntToStr(lblGutterFont.Font.Size) + 'pt';
  end;
  {$IF CompilerVersion < 36}
  lblGutterFont.Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

procedure TEditorOptionsDialog.cbGutterFontClick(Sender: TObject);
begin
  lblGutterFont.Enabled := cbGutterFont.Checked;
  btnGutterFont.Enabled := cbGutterFont.Checked;
end;

procedure TEditorOptionsDialog.FillInKeystrokeInfo(
  AKey: TSynEditKeyStroke; AItem: TListItem);
var TmpString: string;      begin
  with AKey do
  begin
    if Command >= ecUserFirst then
    begin
      TmpString := 'User Command';
      if Assigned(GetUserCommandNames) then
        GetUserCommandNames(Command, TmpString);
    end else begin
      if FExtended then
        TmpString := _(ConvertCodeStringToExtended(EditorCommandToCodeString(Command)))
      else
        TmpString := EditorCommandToCodeString(Command);
    end;

    AItem.Caption:= TmpString;
    AItem.SubItems.Clear;

    TmpString := '';
    if ShortCut <> 0 then
      TmpString := ShortCutToText(ShortCut);

    if (TmpString <> '') and (ShortCut2 <> 0) then
      TmpString := TmpString + ' ' + ShortCutToText(ShortCut2);

    AItem.SubItems.Add(TmpString);
  end;
end;

procedure TEditorOptionsDialog.ckGutterAutosizeClick(Sender: TObject);
begin
  EDigits.Enabled:= not ckGutterAutosize.Checked;
end;

procedure TEditorOptionsDialog.cbHighlightersChange(Sender: TObject);
var
  SynH: TSynCustomHighlighter;
begin
  lbElements.Items.BeginUpdate;
  SynSyntaxSample.Lines.BeginUpdate;
  try
    lbElements.ItemIndex := -1;
    lbElements.Items.Clear;
    SynSyntaxSample.Lines.Clear;
    if cbHighlighters.ItemIndex > -1 then
    begin
      SynH := SelectedHighlighter;
      for var I := 0 to SynH.AttrCount - 1 do
        lbElements.Items.Add(SynH.Attribute[I].FriendlyName);
      SynSyntaxSample.Highlighter := SynH;
      SynSyntaxSample.Lines.Text := SynH.SampleSource;
    end;

    //Select the first Element if avail to avoid exceptions
    if lbElements.Items.Count > 0 then  //Added by KF 2005_JUL_15
    begin
      lbElements.ItemIndex := 0;
      lbElementsClick(lbElements);  //We have to run it manually, as setting its Items prop won't fire the event.
      EnableColorItems(True);   //Controls can be enabled now because there is active highlighter and element.
    end
    else
      EnableColorItems(False);  //Else disable controls

  finally
    lbElements.Items.EndUpdate;
    SynSyntaxSample.Lines.EndUpdate;
  end;
end;

procedure TEditorOptionsDialog.lbColorThemesClick(Sender: TObject);
var
  AppStorage: TJvAppIniFileStorage;
  FileName: string;
begin
  if lbColorThemes.ItemIndex >= 0 then
  begin
    FileName := IncludeTrailingPathDelimiter(TSynEditOptionsDialog.HighlighterFileDir) +
                   lbColorThemes.Items[lbColorThemes.ItemIndex]+ '.ini';
    AppStorage := TJvAppIniFileStorage.Create(nil);
    try
      AppStorage.FlushOnDestroy := False;
      AppStorage.Location := flCustom;
      AppStorage.FileName := FileName;
      SynThemeSample.Highlighter.BeginUpdate;
      try
        AppStorage.ReadPersistent('Highlighters\'+SynThemeSample.Highlighter.FriendlyLanguageName,
            SynThemeSample.Highlighter);
      finally
        SynThemeSample.Highlighter.EndUpdate;
      end;
    finally
      AppStorage.Free;
    end;
  end;
end;

procedure TEditorOptionsDialog.lbElementsClick(Sender: TObject);
var
  SynH: TSynCustomHighlighter;
  SynAttr: TSynHighlighterAttributes;

begin
  if lbElements.ItemIndex <> -1 then
  begin
    EnableColorItems(True);
    SynH := SelectedHighlighter;
    SynAttr := SynH.Attribute[lbElements.ItemIndex];

    FHandleChanges := False;
    try
      cbxElementBold.Checked := (fsBold in SynAttr.Style);
      cbxElementItalic.Checked := (fsItalic in SynAttr.Style);
      cbxElementUnderline.Checked := (fsUnderline in SynAttr.Style);
      cbxElementStrikeout.Checked := (fsStrikeOut in SynAttr.Style);
      cbElementForeground.SelectedColor := SynAttr.Foreground;
      cbElementBackground.SelectedColor := SynAttr.Background;
    finally
      FHandleChanges := True;
    end;
  end
  else
    EnableColorItems(False);
end;

function TEditorOptionsDialog.SelectedHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
  if cbHighlighters.ItemIndex > -1 then
    Result := cbHighlighters.Items.Objects[cbHighlighters.ItemIndex] as TSynCustomHighlighter;
end;

procedure TEditorOptionsDialog.EnableColorItems(AEnable: Boolean);
begin
  cbElementForeground.Enabled := AEnable;
  cbElementBackground.Enabled := AEnable;
  cbxElementBold.Enabled := AEnable;
  cbxElementItalic.Enabled := AEnable;
  cbxElementUnderline.Enabled := AEnable;
  cbxElementStrikeout.Enabled := AEnable;
  if AEnable then begin
    cbElementForeground.HandleNeeded;
    cbElementBackground.HandleNeeded;
  end;
end;

procedure TEditorOptionsDialog.cbElementForegroundChange(
  Sender: TObject);
var
  SynH: TSynCustomHighlighter;
  SynAttr: TSynHighlighterAttributes;
begin
  SynH := SelectedHighlighter;
  SynAttr := SynH.Attribute[lbElements.ItemIndex];
  SynAttr.Foreground := cbElementForeground.SelectedColor;
end;

procedure TEditorOptionsDialog.cbElementBackgroundChange(
  Sender: TObject);
var
  SynH: TSynCustomHighlighter;
  SynAttr: TSynHighlighterAttributes;
begin
  SynH := SelectedHighlighter;
  SynAttr := SynH.Attribute[lbElements.ItemIndex];
  SynAttr.Background := cbElementBackground.SelectedColor;
end;

procedure TEditorOptionsDialog.UpdateColorFontStyle;
var
  FontStyles: TFontStyles;
  SynH: TSynCustomHighlighter;
  SynAttr: TSynHighlighterAttributes;
begin
  FontStyles := [];
  SynH := SelectedHighlighter;
  SynAttr := SynH.Attribute[lbElements.ItemIndex];

  if cbxElementBold.Checked then
    Include(FontStyles, fsBold);

  if cbxElementItalic.Checked then
    Include(FontStyles, fsItalic);

  if cbxElementUnderline.Checked then
    Include(FontStyles, fsUnderline);

  if cbxElementStrikeout.Checked then
    Include(FontStyles, fsStrikeOut);

  SynAttr.Style := FontStyles;
end;

procedure TEditorOptionsDialog.cbxElementBoldClick(Sender: TObject);
begin
  if FHandleChanges then
    UpdateColorFontStyle;
end;

procedure TEditorOptionsDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TEditorOptionsDialog.cKeyCommandChange(Sender: TObject);
var
  ListItem: TListItem;
begin
  for ListItem in KeyList.Items do
  begin
    if TSynEditKeyStroke(ListItem.Data).Command =
      Word(NativeUInt(cKeyCommand.Items.Objects[cKeyCommand.ItemIndex]))
    then
    begin
      ListItem.Selected := True;
      ListItem.MakeVisible(False);
      Exit;
    end;
  end;

  // If Command does not exist
  KeyList.Selected := nil;
  FHotKeyEditor1.HotKey := 0;
  FHotKeyEditor2.HotKey := 0;
  btnUpdateKey.Enabled := False;
  btnRemKey.Enabled := False;
end;

procedure TEditorOptionsDialog.SynSyntaxSampleClick(Sender: TObject);
var
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  SynSyntaxSample.GetHighlighterAttriAtRowColEx(SynSyntaxSample.CaretXY, Token,
            TokenType, Start, Attri);
  for var I := 0 to lbElements.Count - 1 do
    if Assigned(Attri) and (lbElements.Items[I] = Attri.FriendlyName) then begin
      lbElements.ItemIndex := I;
      lbElementsClick(Self);
      Break;
    end;
end;

procedure TEditorOptionsDialog.CMDialogChar(var Msg: TCMDialogChar );
//  To avoid invoking button accelerators without pressing the ALT button
begin
  if ((Msg.KeyData and $20000000) = 0) or FHotKeyEditor1.Focused or FHotKeyEditor2.Focused then
    Msg.Result := 1 // alt not down, eat key
  else
    inherited;
end;

end.


