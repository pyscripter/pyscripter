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

$Id: dlgSynEditOptions.pas,v 1.21 2004/06/26 20:55:33 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:

-------------------------------------------------------------------------------}

unit dlgSynEditOptions;

{$I SynEdit.inc}

interface

uses
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,

  Buttons,
  Menus,
  SynEdit,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditKeyCmds,
  Classes,
  SysUtils, SpTBXControls,
  SpTBXEditors, dlgPyIDEBase, SpTBXItem, SpTBXExtEditors,
  EasyListview, SpTBXTabs, TB2Item, MPCommonObjects;

type
  TSynEditorOptionsUserCommand = procedure(AUserCommand: Integer;
                                           var ADescription: String) of object;

  //NOTE: in order for the user commands to be recorded correctly, you must
  //      put the command itself in the object property.
  //      you can do this like so:
  //
  //      StringList.AddObject('ecSomeCommand', TObject(ecSomeCommand))
  //
  //      where ecSomeCommand is the command that you want to add

  TSynEditorOptionsAllUserCommands = procedure(ACommands: TStrings) of object;

  TSynEditorOptionsContainer = class;

  TfmEditorOptionsDialog = class(TPyIDEDlgBase)
    gbBookmarks: TSpTBXGroupBox;
    gbLineSpacing: TSpTBXGroupBox;
    gbGutter: TSpTBXGroupBox;
    gbRightEdge: TSpTBXGroupBox;
    gbEditorFont: TSpTBXGroupBox;
    gbOptions: TSpTBXGroupBox;
    gbCaret: TSpTBXGroupBox;
    Panel3: TSpTBXPanel;
    FontDialog: TFontDialog;
    gbKeyStrokes: TSpTBXGroupBox;
    pnlGutterFontDisplay: TSpTBXPanel;
    GroupBox1: TSpTBXGroupBox;
    SynEdit1: TSynEdit;
    GroupBox2: TSpTBXGroupBox;
    btnGutterFont: TSpTBXButton;
    btnFont: TSpTBXButton;
    btnAddKey: TSpTBXButton;
    btnRemKey: TSpTBXButton;
    btnUpdateKey: TSpTBXButton;
    btnOk: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnHelp: TSpTBXButton;
    ckGutterAutosize: TSpTBXCheckBox;
    ckGutterShowLineNumbers: TSpTBXCheckBox;
    ckGutterShowLeaderZeros: TSpTBXCheckBox;
    ckGutterVisible: TSpTBXCheckBox;
    cbGutterFont: TSpTBXCheckBox;
    ckGutterStartAtZero: TSpTBXCheckBox;
    ckGutterGradient: TSpTBXCheckBox;
    ckBookmarkKeys: TSpTBXCheckBox;
    ckBookmarkVisible: TSpTBXCheckBox;
    ckAutoIndent: TSpTBXCheckBox;
    ckDragAndDropEditing: TSpTBXCheckBox;
    ckWordWrap: TSpTBXCheckBox;
    ckHalfPageScroll: TSpTBXCheckBox;
    ckThemeSelection: TSpTBXCheckBox;
    ckScrollByOneLess: TSpTBXCheckBox;
    ckScrollPastEOF: TSpTBXCheckBox;
    ckScrollPastEOL: TSpTBXCheckBox;
    ckShowScrollHint: TSpTBXCheckBox;
    ckSmartTabs: TSpTBXCheckBox;
    ckTabsToSpaces: TSpTBXCheckBox;
    ckTrimTrailingSpaces: TSpTBXCheckBox;
    ckTabIndent: TSpTBXCheckBox;
    ckAltSetsColumnMode: TSpTBXCheckBox;
    ckKeepCaretX: TSpTBXCheckBox;
    ckScrollHintFollows: TSpTBXCheckBox;
    ckGroupUndo: TSpTBXCheckBox;
    ckSmartTabDelete: TSpTBXCheckBox;
    ckRightMouseMoves: TSpTBXCheckBox;
    ckEnhanceHomeKey: TSpTBXCheckBox;
    ckHideShowScrollbars: TSpTBXCheckBox;
    ckDisableScrollArrows: TSpTBXCheckBox;
    ckShowSpecialChars: TSpTBXCheckBox;
    ckEnhanceEndKey: TSpTBXCheckBox;
    cbxElementBold: TSpTBXCheckBox;
    cbxElementItalic: TSpTBXCheckBox;
    cbxElementUnderline: TSpTBXCheckBox;
    cbxElementStrikeout: TSpTBXCheckBox;
    cbApplyToAll: TSpTBXCheckBox;
    Label3: TSpTBXLabel;
    Label10: TSpTBXLabel;
    Label1: TSpTBXLabel;
    lblGutterFont: TSpTBXLabel;
    labFont: TSpTBXLabel;
    Label8: TSpTBXLabel;
    Label9: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label4: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    Label11: TSpTBXLabel;
    Label12: TSpTBXLabel;
    Label13: TSpTBXLabel;
    Label14: TSpTBXLabel;
    Label15: TSpTBXLabel;
    cInsertCaret: TSpTBXComboBox;
    cOverwriteCaret: TSpTBXComboBox;
    cKeyCommand: TSpTBXComboBox;
    cbHighlighters: TSpTBXComboBox;
    eRightEdge: TSpTBXEdit;
    eLineSpacing: TSpTBXEdit;
    eTabWidth: TSpTBXEdit;
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
    KeyList: TEasyListview;
    SpTBXPanel1: TSpTBXPanel;
    SpTBXPanel2: TSpTBXPanel;
    procedure SynEdit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnUpdateKeyClick(Sender: TObject);
    procedure btnAddKeyClick(Sender: TObject);
    procedure btnRemKeyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnGutterFontClick(Sender: TObject);
    procedure cbGutterFontClick(Sender: TObject);
    procedure cKeyCommandExit(Sender: TObject);
    procedure cKeyCommandKeyPress(Sender: TObject; var Key: Char);
    procedure cKeyCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbHighlightersChange(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure cbElementForegroundChange(Sender: TObject);
    procedure cbElementBackgroundChange(Sender: TObject);
    procedure cbxElementBoldClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure KeyListItemSelectionsChanged(Sender: TCustomEasyListview);
  private
    FHandleChanges : Boolean;  //Normally true, can prevent unwanted execution of event handlers

    FSynEdit: TSynEditorOptionsContainer;
    FUserCommand: TSynEditorOptionsUserCommand;
    FAllUserCommands: TSynEditorOptionsAllUserCommands;

//    OldSelected: TEasyItem;
    FExtended: Boolean;

    procedure GetData;
    procedure PutData;
    procedure EditStrCallback(const S: string);
    procedure FillInKeystrokeInfo(AKey: TSynEditKeystroke; AItem: TEasyItem);
    procedure UpdateKey(AKey: TSynEditKeystroke);
    function SelectedHighlighter:TSynCustomHighlighter;
    procedure EnableColorItems(aEnable:boolean);
    procedure UpdateColorFontStyle;
    procedure cmDialogChar( Var msg: TCMDialogChar ); message CM_DIALOGCHAR;
  public
    eKeyShort2: TSynHotKey;
    eKeyShort1: TSynHotKey;

    function Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
    property GetUserCommandNames: TSynEditorOptionsUserCommand read FUserCommand
      write FUserCommand;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read FAllUserCommands
      write FAllUserCommands;
    property UseExtendedStrings: Boolean read FExtended write FExtended;
  end;

  TSynHighlighterCountEvent = procedure(Sender:TObject; var Count:integer) of object;
  TSynGetHighlighterEvent = procedure(Sender:TObject; Index:integer; Var SynHighlighter:TSynCustomHighlighter) of object;
  TSynSetHighlighterEvent = procedure(Sender:TObject; Index:integer; SynHighlighter:TSynCustomHighlighter) of object;
  TSynOptionPage = (soDisplay, soOptions, soKeystrokes, soColor);
  TSynOptionPages = set of TSynOptionPage;

  TSynEditOptionsDialog = class(TComponent)
  private
    FForm: TfmEditorOptionsDialog;
    fPages : TSynOptionPages;
    fHighlighterCountEvent: TSynHighlighterCountEvent;
    fGetHighlighterEvent: TSynGetHighlighterEvent;
    fSetHighlighterEvent: TSynSetHighlighterEvent;
    fHighlighters : TList;
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
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
    property Form: TfmEditorOptionsDialog read FForm;
    procedure UpdateHighlighters;
  published
    property GetUserCommand: TSynEditorOptionsUserCommand
      read GetUserCommandNames
      write SetUserCommandNames;
    property GetAllUserCommands: TSynEditorOptionsAllUserCommands
      read GetUserCommands
      write SetUserCommands;
    property UseExtendedStrings: Boolean read GetExtended write SetExtended;
    property VisiblePages : TSynOptionPages read GetOptionPages write SetOptionPages;
    property OnGetHighlighterCount : TSynHighlighterCountEvent read fHighlighterCountEvent write FHighlighterCountEvent;
    property OnGetHighlighter : TSynGetHighlighterEvent read fGetHighlighterEvent write fGetHighlighterEvent;
    property OnSetHighlighter : TSynSetHighlighterEvent read fSetHighlighterEvent write fSetHighlighterEvent;
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
    FMaxScrollWidth: Integer;
    FRightEdge: Integer;
    FSelectedColor: TSynSelectedColor;
    FRightEdgeColor: TColor;
    FFont: TFont;
    FBookmarks: TSynBookMarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FKeystrokes: TSynEditKeyStrokes;
    FOptions: TSynEditorOptions;
    FSynGutter: TSynGutter;
    FWordBreakChars: String;
    FColor: TColor;
    FActiveLineColor : TColor;
    procedure SetBookMarks(const Value: TSynBookMarkOpt);
    procedure SetFont(const Value: TFont);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetOptions(const Value: TSynEditorOptions);
    procedure SetSynGutter(const Value: TSynGutter);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AssignTo(Dest : TPersistent); override;
  published
    property Options : TSynEditorOptions read FOptions write SetOptions;
    property BookMarkOptions : TSynBookMarkOpt read FBookmarks write SetBookMarks;
    property Color : TColor read FColor write FColor;
    property Font : TFont read FFont write SetFont;
    property ExtraLineSpacing : Integer read FExtraLineSpacing write FExtraLineSpacing;
    property Gutter : TSynGutter read FSynGutter write SetSynGutter;
    property RightEdge : Integer read FRightEdge write FRightEdge;
    property RightEdgeColor : TColor read FRightEdgeColor write FRightEdgeColor;
    property WantTabs : Boolean read FWantTabs write FWantTabs;
    property WordWrap : Boolean read FWordWrap write FWordWrap;
    property InsertCaret : TSynEditCaretType read FInsertCaret write FInsertCaret;
    property OverwriteCaret : TSynEditCaretType read FOverwriteCaret write FOverwriteCaret;
    property HideSelection : Boolean read FHideSelection write FHideSelection;
    property MaxScrollWidth : Integer read FMaxScrollWidth write FMaxScrollWidth;
    property MaxUndo : Integer read FMaxUndo write FMaxUndo;
    property SelectedColor : TSynSelectedColor read FSelectedColor write FSelectedColor;
    property TabWidth : Integer read FTabWidth write FTabWidth;
    property WordBreakChars : String read FWordBreakChars write FWordBreakChars;
    property Keystrokes : TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
    property ActiveLineColor : TColor read FActiveLineColor write FActiveLineColor;
  end;

implementation

{$R *.dfm}

uses
  SynEditKeyConst, uCommonFunctions, gnugettext, StringResources, SpTBXSkins;

//function SortByColumn(Item1, Item2: TEasyItem; Data: integer): integer; stdcall;
//begin
//  if Data = 0 then
//    Result := AnsiCompareText(Item1.Caption, Item2.Caption)
//  else
//    Result := AnsiCompareText(Item1.SubItems[Data-1],
//                              Item2.SubItems[Data-1]);
//end;

{ TSynEditOptionsDialog }

constructor TSynEditOptionsDialog.Create(AOwner: TComponent);
begin
  inherited;
  FForm:= TfmEditorOptionsDialog.Create(Self);
  fPages := [soDisplay, soOptions, soKeystrokes];
  fHighlighters := TList.create;
end;

destructor TSynEditOptionsDialog.Destroy;
begin
  ClearHighlighters;
  fHighlighters.free;
  FForm.Free;
  inherited;
end;

function TSynEditOptionsDialog.Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
type
  TSynHClass = class of TSynCustomHighlighter;
var
   wCount : integer;
   loop : integer;
   wHighlighter : TSynCustomHighlighter;
   wInternalSynH : TSynCustomHighlighter;
   wSynHClass : TSynHClass;
begin
  if soDisplay in fPages then
     FForm.Display.TabVisible := true
  else
     FForm.Display.TabVisible := false;

  if soOptions in fPages then
     FForm.Options.TabVisible := true
  else
     FForm.Options.TabVisible := false;

  if soKeyStrokes in fPages then
     FForm.Keystrokes.TabVisible := true
  else
     FForm.Keystrokes.TabVisible := false;

  if soColor in FPages then
  begin
     if assigned(fHighlighterCountEvent) then
     begin
        wCount := 0;
        fHighlighterCountEvent(self, wCount);
     end;

     if (wCount > 0) and assigned(fGetHighlighterEvent) then
     begin
        if fForm.cbHighlighters.Items.Count <> wCount then
        begin
           FForm.cbHighlighters.Items.Clear;
             
           for loop := 0 to wCount do
           begin
              fGetHighlighterEvent(self, loop, wHighlighter);
              if assigned(wHighlighter) then
              begin
                 wSynHClass := TSynHClass(wHighlighter.classtype);
                 wInternalSynH := wSynHClass.Create(nil);
                 wInternalSynH.assign(wHighlighter);
                 fHighlighters.add(wInternalSynH);
                 FForm.cbHighlighters.Items.AddObject(wInternalSynH.FriendlyLanguageName, wInternalSynH);
              end;
           end;
        end;
     end;

    FForm.Color.TabVisible := true;
  end
  else
    FForm.Color.TabVisible := false;

  //Run the form
  Result:= FForm.Execute(EditOptions);
end;

function TSynEditOptionsDialog.GetUserCommands: TSynEditorOptionsAllUserCommands;
begin
  Result := FForm.GetAllUserCommands;
end;

function TSynEditOptionsDialog.GetUserCommandNames: TSynEditorOptionsUserCommand;
begin
  Result := FForm.GetUserCommandNames
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
  Result := fPages;
end;

procedure TSynEditOptionsDialog.SetOptionPages(
  const Value: TSynOptionPages);
begin
  if not FForm.Visible then
    fPages := value;
end;

procedure TSynEditOptionsDialog.ClearHighlighters;
var
  loop : integer;
  wSynH : TSynCustomHighlighter;
begin
  for loop := 0 to fHighlighters.count-1 do
  begin
     wSynH := fHighlighters[loop];
     fHighlighters[loop] := nil;
     wSynH.free;
  end;
end;

/// Iterates through the highlighter list and
/// fires FSetHighlighterEvent for each highlighter.
procedure TSynEditOptionsDialog.UpdateHighlighters;
var
  loop : integer;
begin
   if assigned(fSetHighlighterEvent) then
      for loop := 0 to fHighlighters.Count-1 do
         fSetHighlighterEvent(self, loop, fHighlighters[loop]);
end;

{ TSynEditorOptionsContainer }

procedure TSynEditorOptionsContainer.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TCustomSynEdit) then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.BookmarkOptions.Assign(TCustomSynEdit(Source).BookmarkOptions);
    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);
    Self.Keystrokes.Assign(TCustomSynEdit(Source).Keystrokes);
    Self.SelectedColor.Assign(TCustomSynEdit(Source).SelectedColor);

    Self.Color := TCustomSynEdit(Source).Color;
    Self.Options := TCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.HideSelection := TCustomSynEdit(Source).HideSelection;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxScrollWidth := TCustomSynEdit(Source).MaxScrollWidth;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
    Self.WantTabs := TCustomSynEdit(Source).WantTabs;
    Self.WordWrap := TCustomSynEdit(Source).WordWrap;
    Self.ActiveLineColor := TCustomSynEdit(Source).ActiveLineColor;
//!!    Self.WordBreakChars := TSynEdit(Source).WordBreakChars;
  end else if Assigned(Source) and (Source is TSynEditorOptionsContainer) then
  begin
    Self.Font.Assign(TSynEditorOptionsContainer(Source).Font);
    Self.BookmarkOptions.Assign(TSynEditorOptionsContainer(Source).BookmarkOptions);
    Self.Gutter.Assign(TSynEditorOptionsContainer(Source).Gutter);
    Self.Keystrokes.Assign(TSynEditorOptionsContainer(Source).Keystrokes);
    Self.SelectedColor.Assign(TSynEditorOptionsContainer(Source).SelectedColor);

    Self.Color := TSynEditorOptionsContainer(Source).Color;
    Self.Options := TSynEditorOptionsContainer(Source).Options;
    Self.ExtraLineSpacing := TSynEditorOptionsContainer(Source).ExtraLineSpacing;
    Self.HideSelection := TSynEditorOptionsContainer(Source).HideSelection;
    Self.InsertCaret := TSynEditorOptionsContainer(Source).InsertCaret;
    Self.OverwriteCaret := TSynEditorOptionsContainer(Source).OverwriteCaret;
    Self.MaxScrollWidth := TSynEditorOptionsContainer(Source).MaxScrollWidth;
    Self.MaxUndo := TSynEditorOptionsContainer(Source).MaxUndo;
    Self.RightEdge := TSynEditorOptionsContainer(Source).RightEdge;
    Self.RightEdgeColor := TSynEditorOptionsContainer(Source).RightEdgeColor;
    Self.TabWidth := TSynEditorOptionsContainer(Source).TabWidth;
    Self.WantTabs := TSynEditorOptionsContainer(Source).WantTabs;
    Self.WordWrap := TSynEditorOptionsContainer(Source).WordWrap;
    Self.ActiveLineColor := TSynEditorOptionsContainer(Source).ActiveLineColor;
  end else
    inherited;
end;

procedure TSynEditorOptionsContainer.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) and (Dest is TCustomSynEdit) then
  begin
    TCustomSynEdit(Dest).Font.Assign(Self.Font);
    TCustomSynEdit(Dest).BookmarkOptions.Assign(Self.BookmarkOptions);
    TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
    TCustomSynEdit(Dest).Keystrokes.Assign(Self.Keystrokes);
    TCustomSynEdit(Dest).SelectedColor.Assign(Self.SelectedColor);

    TCustomSynEdit(Dest).Color := Self.Color;
    TCustomSynEdit(Dest).Options := Self.Options;
    TCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
    TCustomSynEdit(Dest).HideSelection := Self.HideSelection;
    TCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
    TCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
    TCustomSynEdit(Dest).MaxScrollWidth := Self.MaxScrollWidth;
    TCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
    TCustomSynEdit(Dest).RightEdge := Self.RightEdge;
    TCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
    TCustomSynEdit(Dest).TabWidth := Self.TabWidth;
    TCustomSynEdit(Dest).WantTabs := Self.WantTabs;
    TCustomSynEdit(Dest).WordWrap := Self.WordWrap;
    TCustomSynEdit(Dest).ActiveLineColor := Self.ActiveLineColor;
  end else
    inherited;
end;

constructor TSynEditorOptionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FBookmarks:= TSynBookMarkOpt.Create(Self);
  FKeystrokes:= TSynEditKeyStrokes.Create(Self);
  FSynGutter:= TSynGutter.Create;
  FSelectedColor:= TSynSelectedColor.Create;
  FSelectedColor.Foreground:= clHighlightText;
  FSelectedColor.Background:= clHighlight;
  fActiveLineColor := clNone;
  FFont:= TFont.Create;
  FFont.Name:= 'Courier New';
  FFont.Size:= 8;
  Color:= clWindow;
  Keystrokes.ResetDefaults;
  Options := [eoAutoIndent,eoDragDropEditing,eoDropFiles,eoScrollPastEol,
    eoShowScrollHint,eoSmartTabs,eoAltSetsColumnMode, eoTabsToSpaces,eoTrimTrailingSpaces, eoKeepCaretX];
  ExtraLineSpacing := 0;
  HideSelection := False;
  InsertCaret := ctVerticalLine;
  OverwriteCaret := ctBlock;
  MaxScrollWidth := 1024;
  MaxUndo := 1024;
  RightEdge := 80;
  RightEdgeColor := clSilver;
  fActiveLineColor := clNone;
  TabWidth := 8;
  WantTabs := True;
  WordWrap := False;
//!!  WordBreakChars:= '.,;:''"&!?$%#@<>[](){}^-=+-*/\|';
end;

destructor TSynEditorOptionsContainer.Destroy;
begin
  FBookMarks.Free;
  FKeyStrokes.Free;
  FSynGutter.Free;
  FSelectedColor.Free;
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

procedure TSynEditorOptionsContainer.SetOptions(
  const Value: TSynEditorOptions);
begin
  FOptions:= Value;
end;

procedure TSynEditorOptionsContainer.SetSynGutter(const Value: TSynGutter);
begin
  FSynGutter.Assign(Value);
end;

{ TfmEditorOptionsDialog }

function TfmEditorOptionsDialog.Execute(EditOptions : TSynEditorOptionsContainer) : Boolean;
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
  Result:= Showmodal = mrOk;
  //PutData
  if Result then PutData;
end;

procedure TfmEditorOptionsDialog.GetData;
var I : Integer;
    Item : TEasyItem;
begin
  ckWordWrap.Checked := FSynedit.WordWrap;
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
  //Right Edge
  eRightEdge.Text:= IntToStr(FSynEdit.RightEdge);
  cbRightEdgeColor.SelectedColor:= FSynEdit.RightEdgeColor;
  //ActiveLineColor;
  cbActiveLineColor.SelectedColor := FSynEdit.ActiveLineColor;
  //Line Spacing
  eLineSpacing.Text:= IntToStr(FSynEdit.ExtraLineSpacing);
  eTabWidth.Text:= IntToStr(FSynEdit.TabWidth);
  //Break Chars
//!!  eBreakchars.Text:= FSynEdit.WordBreakChars;
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
  ckAltSetsColumnMode.Checked:= eoAltSetsColumnMode in FSynEdit.Options;
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
  ckShowSpecialChars.Checked := eoShowSpecialChars in FSynEdit.Options;
  ckThemeSelection.Checked := FSynEdit.SelectedColor.Background <> clHighlight;

  //Caret
  cInsertCaret.ItemIndex:= ord(FSynEdit.InsertCaret);
  cOverwriteCaret.ItemIndex:= ord(FSynEdit.OverwriteCaret);


  KeyList.BeginUpdate;
  try
    KeyList.Items.Clear;
    for I:= 0 to FSynEdit.Keystrokes.Count-1 do
    begin
      Item:= KeyList.Items.Add;
      FillInKeystrokeInfo(FSynEdit.Keystrokes.Items[I], Item);
      Item.Data:= FSynEdit.Keystrokes.Items[I];
    end;
  finally
    KeyList.EndUpdate;
  end;
  KeyList.Sort.AutoSort := True;
end;

procedure TfmEditorOptionsDialog.PutData;
var
  vOptions: TSynEditorOptions;

  procedure SetFlag(aOption: TSynEditorOption; aValue: Boolean);
  begin
    if aValue then
      Include(vOptions, aOption)
    else
      Exclude(vOptions, aOption);
  end;
begin
  fSynEdit.WordWrap := ckWordWrap.Checked;
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
  //Right Edge
  FSynEdit.RightEdge:= StrToIntDef(eRightEdge.Text, 80);
  FSynEdit.RightEdgeColor:= cbRightEdgeColor.SelectedColor;
  //ActiveLineColor;
  FSynEdit.ActiveLineColor := cbActiveLineColor.SelectedColor;
  //Line Spacing
  FSynEdit.ExtraLineSpacing:= StrToIntDef(eLineSpacing.Text, 0);
  FSynEdit.TabWidth:= StrToIntDef(eTabWidth.Text, 8);
  //Break Chars
//!!  FSynEdit.WordBreakChars:= eBreakchars.Text;
  //Bookmarks
  FSynEdit.BookMarkOptions.EnableKeys:= ckBookmarkKeys.Checked;
  FSynEdit.BookMarkOptions.GlyphsVisible:= ckBookmarkVisible.Checked;
  //Font
  FSynEdit.Font.Assign(labFont.Font);
  //Options
  vOptions := FSynEdit.Options; //Keep old values for unsupported options
  SetFlag(eoAutoIndent, ckAutoIndent.Checked);
  SetFlag(eoDragDropEditing, ckDragAndDropEditing.Checked);
  SetFlag(eoTabIndent, ckTabIndent.Checked);
  SetFlag(eoSmartTabs, ckSmartTabs.Checked);
  SetFlag(eoAltSetsColumnMode, ckAltSetsColumnMode.Checked);
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
  SetFlag(eoShowSpecialChars, ckShowSpecialChars.Checked);
  FSynEdit.Options := vOptions;
  if ckThemeSelection.Checked then
    FSynEdit.SelectedColor.Background := SelectionBackgroundColor
  else
    FSynEdit.SelectedColor.Background := clHighlight;

  //Caret
  FSynEdit.InsertCaret:= TSynEditCaretType(cInsertCaret.ItemIndex);
  FSynEdit.OverwriteCaret:= TSynEditCaretType(cOverwriteCaret.ItemIndex);
end;


procedure TfmEditorOptionsDialog.FormCreate(Sender: TObject);
begin
  inherited;
  FHandleChanges := True;  //Normally true, can prevent unwanted execution of event handlers

  KeyList.OnItemSelectionsChanged := KeyListItemSelectionsChanged;

  eKeyShort1:= TSynHotKey.Create(Self);
  with eKeyShort1 do
  begin
    Parent := gbKeystrokes;
    Left := 154;
    Top := 55;
    Width := 185;
    Height := 21;
    InvalidKeys := [];
    Modifiers := [];
    HotKey := 0;
    TabOrder := 1;
  end;

  eKeyShort2:= TSynHotKey.Create(Self);
  with eKeyShort2 do
  begin
    Parent := gbKeystrokes;
    Left := 154;
    Top := 87;
    Width := 185;
    Height := 21;
    InvalidKeys := [];
    Modifiers := [];
    HotKey := 0;
    TabOrder := 2;
  end;
end;


procedure TfmEditorOptionsDialog.btnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(labFont.Font);
  if FontDialog.Execute then
  begin
    labFont.Font.Assign(FontDialog.Font);
    labFont.Caption:= labFont.Font.Name;
    labFont.Caption:= labFont.Font.Name + ' ' + IntToStr(labFont.Font.Size) + 'pt';    
  end;
end;

procedure TfmEditorOptionsDialog.UpdateKey(AKey: TSynEditKeystroke);
var
  Cmd          : Integer;
begin
  Cmd := Integer(cKeyCommand.Items.Objects[cKeyCommand.ItemIndex]);

  AKey.Command:= Cmd;

  //if eKeyShort1.HotKey <> 0 then  Issue 304
    AKey.ShortCut := eKeyShort1.HotKey;

  //if eKeyShort2.HotKey <> 0 then
    AKey.ShortCut2:= eKeyShort2.HotKey;

end;

procedure TfmEditorOptionsDialog.btnUpdateKeyClick(Sender: TObject);

var
//  Cmd          : Integer;
//  KeyLoc       : Integer;
//  TmpCommand   : String;
  OldShortcut  : TShortcut;
  OldShortcut2 : TShortcut;
  Key : TSynEditKeyStroke;
  S : string;
begin
  if not Assigned(KeyList.Selection.First()) then Exit;
  if cKeyCommand.ItemIndex < 0 then Exit;

  Key := TSynEditKeyStroke(KeyList.Selection.First.Data);
  OldShortcut  := Key.ShortCut;
  OldShortcut2 := Key.ShortCut2;
  try
    UpdateKey(Key);
  except
     on E: ESynKeyError do begin
       S := _(SDuplicateKey);
       Key.ShortCut := OldShortcut;
       Key.ShortCut2 := OldShortcut2;
       MessageBox(0, PChar(E.Message), PChar(S), MB_ICONERROR or MB_OK);
     end;
  end;
  FillInKeystrokeInfo(TSynEditKeyStroke(KeyList.Selection.First.Data), KeyList.Selection.First);
  KeyList.Sort.SortAll;
end;

procedure TfmEditorOptionsDialog.btnAddKeyClick(Sender: TObject);
var
  Item : TEasyItem;
  S : String;
begin
  Item:= KeyList.Items.Add;
  try
    Item.Data:= FSynEdit.Keystrokes.Add;
    UpdateKey(TSynEditKeystroke(Item.Data));
    FillInKeystrokeInfo(TSynEditKeystroke(Item.Data), Item);
    Item.Selected:= True;
  except
     on E: ESynKeyError do begin
       S := _(SDuplicateKey);
       MessageBox(0, PChar(E.Message), PChar(S), MB_ICONERROR or MB_OK);
       TSynEditKeyStroke(Item.Data).Free;
       KeyList.Items.Delete(Item.Index);
     end;
  end;
  KeyList.Sort.SortAll;
  Item.MakeVisible(emvAuto);
end;

procedure TfmEditorOptionsDialog.btnRemKeyClick(Sender: TObject);
var
  Item : TEasyItem;
begin
  Item := KeyList.Selection.First;
  if not Assigned(Item) then Exit;
  TSynEditKeyStroke(Item.Data).Free;
  KeyList.Items.Delete(Item.Index);
end;

procedure TfmEditorOptionsDialog.EditStrCallback(const S: string);
begin
  //Add the Item
  if FExtended then
    cKeyCommand.Items.AddObject(S, TObject(ConvertExtendedToCommand(S)))
  else cKeyCommand.Items.AddObject(S, TObject(ConvertCodeStringToCommand(S)));
end;

procedure TfmEditorOptionsDialog.FormShow(Sender: TObject);
var Commands: TStringList;
    i : Integer;
begin
//We need to do this now because it will not have been assigned when
//create occurs
  cKeyCommand.Items.Clear;
  //Start the callback to add the strings
  if FExtended then
    GetEditorCommandExtended(EditStrCallback)
  else
    GetEditorCommandValues(EditStrCallBack);
  //Now add in the user defined ones if they have any
  if Assigned(FAllUserCommands) then
  begin
    Commands := TStringList.Create;
    try
      FAllUserCommands(Commands);
      for i := 0 to Commands.Count - 1 do
        if Commands.Objects[i] <> nil then
          cKeyCommand.Items.AddObject(Commands[i], Commands.Objects[i]);
    finally
      Commands.Free;
    end;
  end;
//  KeyList.CustomSort(@SortByColumn, 0);
  if (KeyList.Items.Count > 0) then KeyList.Items[0].Selected:= True;

  TabControl.ActivePage := Display;

  //Added by KF 2005_JUL_15
  if Color.TabVisible then
  begin
    if cbHighlighters.Items.Count > 0 then
      cbHighlighters.ItemIndex := 0;

    if cbHighlighters.ItemIndex = -1 then
      EnableColorItems(False)  //If there is still no selected item then disable controls
    else
      cbHighlightersChange(cbHighlighters);  //run OnChange handler (it wont be fired on setting the itemindex prop)
  end;
end;

procedure TfmEditorOptionsDialog.KeyListItemSelectionsChanged(
  Sender: TCustomEasyListview);
begin
  if not Assigned(KeyList.Selection.First()) then Exit;
  with KeyList.Selection.First do begin
    cKeyCommand.Text      := Caption;
    cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf(Caption);
    eKeyShort1.HotKey     := TSynEditKeyStroke(Data).ShortCut;
    eKeyShort2.HotKey     := TSynEditKeyStroke(Data).ShortCut2;
  end;
//  OldSelected := Item;
end;

procedure TfmEditorOptionsDialog.btnOkClick(Sender: TObject);
begin
//  btnUpdateKey.Click;
  // Bug fix of SpTBXColorEdit
  btnOk.SetFocus;
  Application.ProcessMessages;

  ModalResult:= mrOk;
end;

procedure TfmEditorOptionsDialog.btnGutterFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(lblGutterFont.Font);
  if FontDialog.Execute then
  begin
    lblGutterFont.Font.Assign(FontDialog.Font);
    lblGutterFont.Caption:= lblGutterFont.Font.Name + ' ' + IntToStr(lblGutterFont.Font.Size) + 'pt';
  end;
end;

procedure TfmEditorOptionsDialog.cbGutterFontClick(Sender: TObject);
begin
  lblGutterFont.Enabled := cbGutterFont.Checked;
  btnGutterFont.Enabled := cbGutterFont.Checked;
end;


procedure TfmEditorOptionsDialog.FillInKeystrokeInfo(
  AKey: TSynEditKeystroke; AItem: TEasyItem);
var TmpString: String;      begin
  with AKey do
  begin
    if Command >= ecUserFirst then
    begin
      TmpString := 'User Command';
      if Assigned(GetUserCommandNames) then
        GetUserCommandNames(Command, TmpString);
    end else begin
      if FExtended then
        TmpString := ConvertCodeStringToExtended(EditorCommandToCodeString(Command))
      else TmpString := EditorCommandToCodeString(Command);
    end;

    AItem.Caption:= TmpString;
    AItem.Captions[1] := '';

    TmpString := '';
    if Shortcut <> 0 then
      TmpString := ShortCutToText(ShortCut);

    if (TmpString <> '') and (Shortcut2 <> 0) then
      TmpString := TmpString + ' ' + ShortCutToText(ShortCut2);

    AItem.Captions[1] := TmpString;
  end;
end;

procedure TfmEditorOptionsDialog.cKeyCommandExit(Sender: TObject);
var TmpIndex : Integer;
begin
  TmpIndex := cKeyCommand.Items.IndexOf(cKeyCommand.Text);
  if TmpIndex = -1 then
  begin
    if FExtended then
      cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf(ConvertCodeStringToExtended('ecNone'))
    else cKeyCommand.ItemIndex := cKeyCommand.Items.IndexOf('ecNone');
  end else cKeyCommand.ItemIndex := TmpIndex;  //need to force it incase they just typed something in

end;

procedure TfmEditorOptionsDialog.cKeyCommandKeyPress(Sender: TObject;
  var Key: Char);
var WorkStr : String;
    i       : Integer;
begin
//This would be better if componentized, but oh well...
  WorkStr := AnsiUppercase(Copy(cKeyCommand.Text, 1, cKeyCommand.SelStart) + Key);
  i := 0;
  While i < cKeyCommand.Items.Count do
  begin
    if pos(WorkStr, AnsiUppercase(cKeyCommand.Items[i])) = 1 then
    begin
      cKeyCommand.Text := cKeyCommand.Items[i];
      cKeyCommand.SelStart := length(WorkStr);
      cKeyCommand.SelLength := Length(cKeyCommand.Text) - cKeyCommand.SelStart;
      Key := #0;
      break;
    end else inc(i);
  end;

end;

procedure TfmEditorOptionsDialog.cKeyCommandKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = SYNEDIT_RETURN then btnUpdateKey.Click;
end;

procedure TfmEditorOptionsDialog.cbHighlightersChange(Sender : TObject);
var
  loop : integer;
  wSynH : TSynCustomHighlighter;
begin
  lbElements.items.BeginUpdate;
  synedit1.Lines.BeginUpdate;
  try
    lbElements.itemindex := -1;
    lbElements.items.Clear;
    synedit1.lines.clear;
    if cbHighlighters.itemindex > -1 then
    begin
      wSynH := SelectedHighlighter;
      for loop := 0 to wSynH.AttrCount - 1 do
        lbElements.Items.Add(wSynH.Attribute[loop].FriendlyName);
      synedit1.Highlighter := wSynH;
      SynEdit1.Lines.text := wSynH.SampleSource;
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
    lbElements.items.EndUpdate;
    synedit1.Lines.EndUpdate;
  end;
end;

procedure TfmEditorOptionsDialog.lbElementsClick(Sender : TObject);
var
  wSynH : TSynCustomHighlighter;
  wSynAttr : TSynHighlighterAttributes;

begin
  if lbElements.ItemIndex <> -1 then
  begin
    EnableColorItems(True);
    wSynH := SelectedHighlighter;
    wSynAttr := wSynH.Attribute[lbElements.ItemIndex];

    FHandleChanges := False;
    try
      cbxElementBold.Checked := (fsBold in wSynAttr.Style);
      cbxElementItalic.Checked := (fsItalic in wSynAttr.Style);
      cbxElementUnderline.Checked := (fsUnderline in wSynAttr.Style);
      cbxElementStrikeout.Checked := (fsStrikeOut in wSynAttr.Style);
      cbElementForeground.SelectedColor := wSynAttr.Foreground;
      cbElementBackground.SelectedColor := wSynAttr.Background;
    finally
      FHandleChanges := True;
    end;
  end
  else
    EnableColorItems(False);
end;

function TfmEditorOptionsDialog.SelectedHighlighter : TSynCustomHighlighter;
begin
  Result := nil;

  if cbHighlighters.ItemIndex > -1 then
    Result := cbHighlighters.Items.Objects[cbHighlighters.ItemIndex] as TSynCustomHighlighter;
end;

procedure TfmEditorOptionsDialog.EnableColorItems(aEnable : Boolean);
begin
  cbElementForeground.Enabled := aenable;
  cbElementBackground.Enabled := aenable;
  cbxElementBold.Enabled := aenable;
  cbxElementItalic.Enabled := aenable;
  cbxElementUnderline.Enabled := aenable;
  cbxElementStrikeout.Enabled := aenable;
  if aEnable then begin
    cbElementForeground.HandleNeeded;
    cbElementBackground.HandleNeeded;
  end;
end;

procedure TfmEditorOptionsDialog.cbElementForegroundChange(
  Sender: TObject);
var
  wSynH : TSynCustomHighlighter;
  wSynAttr : TSynHighlighterAttributes;
begin
  wSynH := SelectedHighlighter;
  wSynAttr := wSynH.Attribute[lbElements.ItemIndex];
  wSynAttr.Foreground := cbElementForeground.SelectedColor;
end;

procedure TfmEditorOptionsDialog.cbElementBackgroundChange(
  Sender: TObject);
var
  wSynH : TSynCustomHighlighter;
  wSynAttr : TSynHighlighterAttributes;
begin
  wSynH := SelectedHighlighter;
  wSynAttr := wSynH.Attribute[lbElements.ItemIndex];
  wSynAttr.Background := cbElementBackground.SelectedColor;
end;

procedure TfmEditorOptionsDialog.UpdateColorFontStyle;
var
  wfs : TFontStyles;
  wSynH : TSynCustomHighlighter;
  wSynAttr : TSynHighlighterAttributes;
begin
  wfs := [];
  wSynH := SelectedHighlighter;
  wSynAttr := wSynH.Attribute[lbElements.ItemIndex];

  if cbxElementBold.Checked then
    include(wfs, fsBold);

  if cbxElementItalic.Checked then
    include(wfs, fsItalic);

  if cbxElementUnderline.Checked then
    include(wfs, fsUnderline);

  if cbxElementStrikeout.Checked then
    include(wfs, fsStrikeOut);

  wSynAttr.style := wfs;
end;

procedure TfmEditorOptionsDialog.cbxElementBoldClick(Sender: TObject);
begin
  if FHandleChanges then
    UpdateColorFontStyle;
end;

procedure TfmEditorOptionsDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfmEditorOptionsDialog.SynEdit1Click(Sender: TObject);
var
  i, TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  SynEdit1.GetHighlighterAttriAtRowColEx(SynEdit1.CaretXY, Token,
            TokenType, Start, Attri);
  for i := 0 to lbElements.Count - 1 do
    if Assigned(Attri) and (lbElements.Items[i] = Attri.FriendlyName) then begin
      lbElements.ItemIndex := i;
      lbElementsClick(Self);
      break;
    end;

end;

procedure TfmEditorOptionsDialog.cmDialogChar( Var msg: TCMDialogChar );
//  To avoid invoking button accelerators without pressing the ALT button
begin
  If ((msg.keydata and $20000000) = 0) or eKeyShort1.Focused or eKeyShort2.Focused Then
    msg.result := 1 // alt not down, eat key
  Else
    inherited;
end;

end.


