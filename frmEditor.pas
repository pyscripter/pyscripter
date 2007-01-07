{-----------------------------------------------------------------------------
 Unit Name: frmEditor
 Author:    Kiriakos Vlahos
 Date:      23-Feb-2005
 Purpose:
 History:   Origianlly Based on SynEdit Demo
-----------------------------------------------------------------------------}

unit frmEditor;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Contnrs, Forms,
  Menus, uEditAppIntfs, SynEdit, SynEditTypes, SynEditMiscProcs,
  SynEditHighlighter, SynEditMiscClasses, SynEditSearch, SynEditRegexSearch,
  SynEditKeyCmds, ImgList, Dialogs, ExtCtrls, JvExExtCtrls, JvComponent, JvPanel,
  JvPageList, JvExControls, JvTabBar, TBX, TB2Item, uCommonFunctions,
  SynCompletionProposal, cPyBaseDebugger, SynUnicode;

type
  TEditor = class;

  THotIdentInfo = record
    HaveHotIdent : boolean;
    IdentArea : TRect;
    Ident : string;
    DottedIdent : string;
    StartCoord : TBufferCoord;
    SynToken: string;
    SynAttri: TSynHighlighterAttributes;
  end;

  TEditorForm = class(TForm)
    imglGutterGlyphs: TImageList;
    pmnuEditor: TTBXPopupMenu;
    pmnuPageList: TTBXPopupMenu;
    CloseTab: TTBXItem;
    FGPanel: TPanel;
    ViewsTabBar: TJvTabBar;
    EditorViews: TJvPageList;
    SourcePage: TJvStandardPage;
    SynEdit: TSynEdit;
    SynCodeCompletion: TSynCompletionProposal;
    SynParamCompletion: TSynCompletionProposal;
    procedure SynEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: Widestring; var x, y: Integer;
      var CanExecute: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure SynEditExit(Sender: TObject);
    procedure SynEditReplaceText(Sender: TObject; const ASearch,
      AReplace: WideString; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure SynEditStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure SynEditGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Line: Integer; Mark: TSynEditMark);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGPanelEnter(Sender: TObject);
    procedure FGPanelExit(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CloseTabClick(Sender: TObject);
    procedure ViewsTabBarTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure SynEditMouseCursor(Sender: TObject;
      const aLineCharPos: TBufferCoord; var aCursor: TCursor);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure SynEditMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    fEditor: TEditor;
    fAutoCompleteActive : Boolean;
    fHotIdentInfo : THotIdentInfo;
    fHintIdentInfo : THotIdentInfo;
    fNeedToCheckSyntax : Boolean;
    fSyntaxErrorPos : TEditorPos;
    fCloseBracketChar : WideChar;
    function DoAskSaveChanges: boolean;
    procedure DoAssignInterfacePointer(AActive: boolean);
    function DoSave: boolean;
    function DoSaveFile: boolean;
    function DoSaveAs: boolean;
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter(HighlighterName : string = '');
    procedure AutoCompleteBeforeExecute(Sender: TObject);
    procedure AutoCompleteAfterExecute(Sender: TObject);
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure CodeHintEventHandler(Sender : TObject; AArea : TRect; var CodeHint : string);
    procedure CodeHintLinkHandler(Sender: TObject; LinkName: string);
    procedure EditorZoom(theZoom: Integer);
    procedure EditorMouseWheel(theDirection: Integer; Shift: TShiftState );
  public
    BreakPoints : TObjectList;
    HasFocus : Boolean;
    FileTime : TFileTime;
    DefaultExtension : string;
    procedure DoActivate;
    procedure DoActivateEditor;
    function DoActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
    function GetEditor : IEditor;
    procedure doProcessUserCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: pointer);
    procedure PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer);
    procedure DoOnIdle;
    procedure AddWatchAtCursor;
  end;

  TEditor = class(TInterfacedObject, IUnknown, IEditor, IEditCommands, IFileCommands,
    ISearchCommands)
  private
    // IEditor implementation
    procedure Activate;
    function ActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
    function AskSaveChanges: boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetSynEdit : TSynEdit;
    function GetBreakPoints : TObjectList;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetFileNameOrTitle: string;
    function GetModified: boolean;
    function GetFileEncoding : TFileSaveFormat;
    procedure SetFileEncoding(FileEncoding : TFileSaveFormat);
    function GetEncodedText : string;
    procedure OpenFile(const AFileName: string; HighlighterName : string = '');
    function HasPythonFile : Boolean;
    function GetForm : TForm;
    // IEditCommands implementation
    function CanCopy: boolean;
    function CanCut: boolean;
    function IEditCommands.CanDelete = CanCut;
    function CanPaste: boolean;
    function CanRedo: boolean;
    function CanSelectAll: boolean;
    function CanUndo: boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
    // IFileCommands implementation
    function CanClose: boolean;
    function CanPrint: boolean;
    function CanSave: boolean;
    function CanSaveAs: boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecPrintPreview;
    procedure ExecSave;
    procedure ExecSaveAs;
    // ISearchCommands implementation
    function CanFind: boolean;
    function CanFindNext: boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  private
    fFileName: string;
    fForm: TEditorForm;
    fHasSelection: boolean;
    fIsReadOnly: boolean;
    fModified: boolean;
    fUntitledNumber: integer;
    TabBarItem : TJvTabBarItem;
    fFileEncoding : TFileSaveFormat;
    function IsEmpty : Boolean;
    constructor Create(AForm: TEditorForm);
    procedure DoSetFileName(AFileName: string);
    function GetEncodedTextEx(var EncodedText: string;
      InformationLossWarning: Boolean) : Boolean;
  end;


implementation

{$R *.DFM}

uses
  frmPyIDEMain, dlgSynPrintPreview, frmCodeExplorer,
  frmBreakPoints, Variants, dmCommands, JclFileUtils,
  TBXThemes, StringResources, JclStrings, VarPyth, cRefactoring,
  cPythonSourceScanner, cCodeHint, frmPythonII, dlgConfirmReplace, Math,
  JvTypes, frmWatches, JclSysUtils, PythonEngine, frmMessages,
  SynEditTextBuffer, cPyDebugger, dlgPickList;

const
  WM_DELETETHIS  =  WM_USER + 42;

resourcestring
  SInsert = 'Insert';
  SOverwrite = 'Overwrite';
  SReadOnly = 'Read Only';
  SNonameFileTitle = 'Untitled';
  SNonamePythonFileTitle = 'Module';

  SAskSaveChanges = 'The text in the "%s" file has changed.'#13#10#13#10 +
                    'Do you want to save the modifications?';

{ TGutterMarkDrawPlugin }

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    fForm: TEditorForm;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(AForm: TEditorForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TEditorForm);
begin
  inherited Create(AForm.SynEdit);
  fForm := AForm;
end;

Type
  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

procedure TDebugSupportPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
Var
  TP : TPoint;
  MaxX, LH : integer;

  procedure PaintUnderLine;
  Const
    UnderlineStyle : TUnderlineStyle = usMicrosoftWord;
  var
    NewPoint,
    NewY: Integer;

    procedure DrawPoint;
    begin
      // Do not draw on gutter.
      // This happens when a word is underlined and part of it is "hidden" under
      // the gutter.
      if TP.X <= Editor.Gutter.RealGutterWidth(Editor.CharWidth) then
        Exit;
      with ACanvas do begin
        if NewY = TP.Y - 1 then
          Pen.Color := fForm.SynEdit.Color
        else
          Pen.Color := clRed;
        Pixels[TP.X, NewY] := Pen.Color;
      end;
    end;

  const
    // Microsoft Word style
//  MW_POINTS: array[0..6] of ShortInt = (1, 2, 2, 1, 0, 0, 0);
    MW_POINTS: array[0..3] of ShortInt = (0, 1, 2, 1);
    // Corel Word Perfect style
//  WP_POINTS: array[0..4] of ShortInt = (3, 2, 1, -1, -1);
    WP_POINTS: array[0..3] of ShortInt = (2, 1, 0, -1);

  begin
    Inc(TP.Y, LH - 3);
    NewPoint := 0;
    if UnderlineStyle = usMicrosoftWord then
      NewY := TP.Y + MW_POINTS[NewPoint]
    else
      NewY := TP.Y + WP_POINTS[NewPoint];
    DrawPoint;
    while TP.X <= MaxX do begin
      DrawPoint;
      Inc(NewPoint);
      if UnderlineStyle = usMicrosoftWord then begin
        if NewPoint > High(MW_POINTS) then
          NewPoint := 0
      end else begin
        if NewPoint > High(WP_POINTS) then
          NewPoint := 0;
      end;
      DrawPoint;
      Inc(TP.X);
      if UnderlineStyle = usMicrosoftWord then
        NewY := TP.Y + MW_POINTS[NewPoint]
      else
        NewY := TP.Y + WP_POINTS[NewPoint];
    end;
  end;

begin
  if CommandsDataModule.PyIDEOptions.CheckSyntaxAsYouType then
  with fForm.fSyntaxErrorPos do
    if IsSyntax and (Editor = fForm.GetEditor) and Math.InRange(Line, FirstLine, LastLine) then begin
      LH := fForm.SynEdit.LineHeight;
      TP := fForm.SynEdit.RowColumnToPixels(fForm.SynEdit.BufferToDisplayPos(
        BufferCoord(1, Line)));
      if TP.X <= ACanvas.ClipRect.Right - ACanvas.ClipRect.Left then begin
        MaxX := fForm.SynEdit.RowColumnToPixels(fForm.SynEdit.BufferToDisplayPos(
          BufferCoord(Char, Line))).X;
        PaintUnderLine;
      end;
    end;

  if fForm.SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then
    fForm.PaintGutterGlyphs(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugSupportPlugin.LinesInserted(FirstLine, Count: integer);
var
  i: integer;
begin
  with fForm do begin
    for i := 0 to BreakPoints.Count - 1 do
      if TBreakPoint(BreakPoints[i]).LineNo >= FirstLine then begin
        TBreakPoint(BreakPoints[i]).LineNo :=
          TBreakPoint(BreakPoints[i]).LineNo + Count;
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end;
  end;
end;

procedure TDebugSupportPlugin.LinesDeleted(FirstLine, Count: integer);
var
  i: integer;
begin
  with fForm do begin
    for i := BreakPoints.Count - 1 downto 0 do
      if TBreakPoint(BreakPoints[i]).LineNo >= FirstLine + Count then begin
        TBreakPoint(BreakPoints[i]).LineNo :=
          TBreakPoint(BreakPoints[i]).LineNo - Count;
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end else if TBreakPoint(BreakPoints[i]).LineNo > FirstLine then begin
        BreakPoints.Delete(i);
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end;
  end;
end;

{ TEditor }

constructor TEditor.Create(AForm: TEditorForm);
begin
  Assert(AForm <> nil);
  inherited Create;
  fForm := AForm;
  fUntitledNumber := -1;
  fFileEncoding := sf_Ansi;
end;

procedure TEditor.Activate;
begin
  if fForm <> nil then
    fForm.DoActivateEditor;
end;

function TEditor.ActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
begin
  if fForm <> nil then
    Result := fForm.DoActivateView(ViewFactory);
end;

function TEditor.AskSaveChanges: boolean;
begin
  if fForm <> nil then
    Result := fForm.DoAskSaveChanges
  else
    Result := TRUE;
end;

procedure TEditor.Close;
// Closes without asking
Var
  TabSheet : TJvStandardPage;
begin
  if (fForm <> nil) then begin
    if (fFileName <> '') and (CommandsDataModule <> nil) then
      PyIDEMainForm.TBXMRUList.Add(fFileName);
    if fUntitledNumber <> -1 then
      CommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);

    if fForm.BreakPoints.Count > 0 then begin
      PyControl.BreakPointsChanged := True;
      BreakPointsWindow.UpdateWindow;
    end;

    TabSheet := (fForm.Parent as TJvStandardPage);
    fForm.Close;
    TabSheet.Free;
    FreeAndNil(TabBarItem);
    CodeExplorerWindow.UpdateWindow;
    CommandsDataModule.UpdateChangeNotify;
  end;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if AFileName <> fFileName then begin
    fFileName := AFileName;
    if fUntitledNumber <> -1 then begin
      CommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);
      fUntitledNumber := -1;
    end;
  end;
end;

function TEditor.GetSynEdit: TSynEdit;
begin
  Result := fForm.SynEdit;
end;

function TEditor.GetBreakPoints: TObjectList;
begin
  Result := fForm.BreakPoints;
end;

function TEditor.GetCaretPos: TPoint;
begin
  if fForm <> nil then
    Result := TPoint(fForm.SynEdit.CaretXY)
  else
    Result := Point(-1, -1);
end;

function TEditor.GetEditorState: string;
begin
  if fForm <> nil then begin
    if fForm.SynEdit.ReadOnly then
      Result := SReadOnly
    else if fForm.SynEdit.InsertMode then
      Result := SInsert
    else
      Result := SOverwrite;
  end else
    Result := '';
end;

function TEditor.GetEncodedText: string;
begin
  GetEncodedTextEx(Result, False);
end;

function TEditor.GetEncodedTextEx(var EncodedText: string;
  InformationLossWarning: Boolean) : Boolean;
var
  PyEncoding : string;
  UniPy, EncodeMethod, Args, EncodedString : PPyObject;
  wStr, LineBreak : WideString;
  SupressOutput : IInterface;
begin
  Result := True;

  case (fForm.SynEdit.Lines as TSynEditStringList).FileFormat of
    sffDos:
      LineBreak := WideCRLF;
    sffUnix:
      LineBreak := WideLF;
    sffMac:
      LineBreak := WideCR;
    sffUnicode:
      if fFileEncoding = sf_Ansi then
        // Ansi-file cannot contain Unicode LINE SEPARATOR,
        // so default to platform-specific Ansi-compatible LineBreak
        LineBreak := SynUnicode.SLineBreak
      else
        LineBreak := WideLineSeparator;
  end;

  wStr := fForm.SynEdit.Lines.GetSeparatedText(LineBreak);

  case fFileEncoding of
    sf_Ansi :
      if HasPythonFile then begin
        PyEncoding := '';
        if fForm.SynEdit.Lines.Count > 0 then
          PyEncoding := ParsePySourceEncoding(fForm.SynEdit.Lines[0]);
        if (PyEncoding = '') and (fForm.SynEdit.Lines.Count > 1) then
          PyEncoding := ParsePySourceEncoding(fForm.SynEdit.Lines[1]);

//        if PyEncoding = 'utf-8' then
//          EncodedText := UTF8BOMString + UTF8Encode(wStr)
//        else with GetPythonEngine do begin
        with GetPythonEngine do begin
          if PyEncoding = '' then
            PyEncoding := SysModule.getdefaultencoding();
          SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
          UniPy := nil;
          EncodeMethod := nil;
          Args := nil;
          EncodedString := nil;
          try
            try
              UniPy := PyUnicode_FromWideChar(PWideChar(wStr), Length(wStr));
              CheckError;
              EncodeMethod := PyObject_GetAttrString(UniPy, 'encode');
              CheckError;
              if InformationLossWarning then begin
                try
                  Args := ArrayToPyTuple([PyEncoding, 'strict']);
                  EncodedString := PyEval_CallObject(EncodeMethod, Args);
                  CheckError;
                  EncodedText := PyString_AsString(EncodedString);
                  CheckError;
                except
                  on UnicodeEncodeError do begin
                    Result :=
                      MessageDlg(Format('Saving file "%s" using "%s" encoding will ' +
                        'result in information loss.  Do you want to proceed?',
                        [GetFileNameOrTitle, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes;
                    if Result then begin
                      Py_XDECREF(Args);
                      Py_XDECREF(EncodedString);
                      Args := nil;
                      EncodedString := nil;
                      Args := ArrayToPyTuple([PyEncoding, 'replace']);
                      EncodedString := PyEval_CallObject(EncodeMethod, Args);
                      CheckError;
                      EncodedText := PyString_AsString(EncodedString);
                      CheckError;
                    end;
                  end;
                end;
              end else begin
                Args := ArrayToPyTuple([PyEncoding, 'replace']);
                EncodedString := PyEval_CallObject(EncodeMethod, Args);
                CheckError;
                EncodedText := PyString_AsString(EncodedString);
                CheckError;
              end;
            finally
              Py_XDECREF(UniPy);
              Py_XDECREF(EncodeMethod);
              Py_XDECREF(Args);
              Py_XDECREF(EncodedString);
            end;
          except
            PyErr_Clear;
            EncodedText := wStr;
            if InformationLossWarning then
              Result :=
                MessageDlg(Format('An error occurred while encoding file "%s" ' +
                 'using "%s" encoding. The default system encoding has been used instead. ' +
                 'Do you want to proceed?',
                  [GetFileNameOrTitle, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
          end;
        end;
      end else begin
        EncodedText := wStr;
        if InformationLossWarning and not IsAnsiOnly(wStr) then begin
          Result :=
            MessageDlg(Format('Saving file "%s" using ANSI encoding will result ' +
            'in information loss.  Do you want to proceed?',
            [GetFileNameOrTitle]), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
        end;
      end;
    sf_UTF8 : EncodedText := UTF8BOMString + UTF8Encode(wStr);
    sf_UTF8_NoBOM : EncodedText := UTF8Encode(wStr);
  end;
end;

function TEditor.GetFileName: string;
begin
  Result := fFileName;
end;

function TEditor.GetFileTitle: string;
begin
  if fFileName <> '' then
    Result := ExtractFileName(fFileName)
  else begin
    if fUntitledNumber = -1 then
      fUntitledNumber := CommandsDataModule.GetUntitledNumber;
    if fForm.SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then
      Result := SNonamePythonFileTitle + IntToStr(fUntitledNumber)
    else
      Result := SNonameFileTitle + IntToStr(fUntitledNumber);
  end;
end;

function TEditor.GetFileNameOrTitle: string;
begin
  if fFileName <> '' then
    Result := fFileName
  else
    Result := GetFileTitle;
end;

function TEditor.GetModified: boolean;
begin
  if fForm <> nil then
    Result := fForm.SynEdit.Modified
  else
    Result := FALSE;
end;

function TEditor.GetFileEncoding: TFileSaveFormat;
begin
  Result := fFileEncoding;
end;

procedure TEditor.SetFileEncoding(FileEncoding: TFileSaveFormat);
begin
  fFileEncoding := FileEncoding;
end;

procedure TEditor.OpenFile(const AFileName: string; HighlighterName : string = '');
begin
  DoSetFileName(AFileName);

  if fForm <> nil then begin
    if (AFileName <> '') and FileExists(AFileName) then begin
      //CommandsDataModule.JvChangeNotify.Active := False;
      if LoadFileIntoWideStrings(AFileName, fForm.SynEdit.Lines, fFileEncoding) then begin
        CommandsDataModule.UpdateChangeNotify;
        fForm.FileTime := GetFileLastWrite(AFileName);
      end else
        Abort;
    end else begin
      fForm.SynEdit.Lines.Clear;
      if AFileName = '' then begin
        // Default settings for new files
        if CommandsDataModule.PyIDEOptions.NewFileLineBreaks <> sffUnicode then
          (fForm.SynEdit.Lines as TSynEditStringList).FileFormat :=
            CommandsDataModule.PyIDEOptions.NewFileLineBreaks;

        fFileEncoding := CommandsDataModule.PyIDEOptions.NewFileEncoding;
      end;
    end;

    fForm.SynEdit.Modified := False;
    fForm.DoUpdateHighlighter(HighlighterName);
    fForm.DoUpdateCaption;
  end;
end;

function TEditor.HasPythonFile: Boolean;
begin
  Result := GetSynEdit.Highlighter = CommandsDataModule.SynPythonSyn;
end;

function TEditor.GetForm: TForm;
begin
  Result := fForm;
end;

// IEditCommands implementation

function TEditor.CanCopy: boolean;
begin
  Result := (fForm <> nil) and fHasSelection;
end;

function TEditor.CanCut: boolean;
begin
  Result := (fForm <> nil) and fHasSelection and not fIsReadOnly;
end;

function TEditor.CanPaste: boolean;
begin
  Result := (fForm <> nil) and fForm.SynEdit.CanPaste;
end;

function TEditor.CanRedo: boolean;
begin
  Result := (fForm <> nil) and fForm.SynEdit.CanRedo;
end;

function TEditor.CanSelectAll: boolean;
begin
  Result := fForm <> nil;
end;

function TEditor.CanUndo: boolean;
begin
  Result := (fForm <> nil) and fForm.SynEdit.CanUndo;
end;

procedure TEditor.ExecCopy;
begin
  if fForm <> nil then
    fForm.SynEdit.CopyToClipboard;
end;

procedure TEditor.ExecCut;
begin
  if fForm <> nil then
    fForm.SynEdit.CutToClipboard;
end;

procedure TEditor.ExecDelete;
begin
  if fForm <> nil then
    fForm.SynEdit.SelText := '';
end;

procedure TEditor.ExecPaste;
begin
  if fForm <> nil then
    fForm.SynEdit.PasteFromClipboard;
end;

procedure TEditor.ExecRedo;
begin
  if fForm <> nil then
    fForm.SynEdit.Redo;
end;

procedure TEditor.ExecSelectAll;
begin
  if fForm <> nil then
    fForm.SynEdit.SelectAll;
end;

procedure TEditor.ExecUndo;
begin
  if fForm <> nil then
    fForm.SynEdit.Undo;
end;

// IFileCommands implementation

function TEditor.CanPrint: boolean;
begin
  Result := True;
end;

function TEditor.CanSave: boolean;
begin
  Result := (fForm <> nil) and (fModified or (fFileName = ''));
end;

function TEditor.CanSaveAs: boolean;
begin
  Result := fForm <> nil;
end;

function TEditor.CanClose: boolean;
begin
  Result := fForm <> nil;
end;

procedure TEditor.ExecClose;
//  Close only after asking
begin
  if AskSaveChanges then Close;
end;

procedure TEditor.ExecPrint;
begin
  if fForm <> nil then with CommandsDataModule do
    if PrintDialog.Execute then begin
      SynEditPrint.SynEdit := fForm.SynEdit;
      SynEditPrint.Title := GetFileTitle;
      if PrintDialog.PrintRange = prAllPages then
        SynEditPrint.Print
      else
        SynEditPrint.PrintRange(PrintDialog.FromPage, PrintDialog.ToPage);
    end;
end;

procedure TEditor.ExecPrintPreview;
begin
  CommandsDataModule.SynEditPrint.SynEdit := fForm.SynEdit;
  CommandsDataModule.SynEditPrint.Title := GetFileTitle;
  with TPrintPreviewDlg.Create(PyIDEMainForm) do begin
    SynEditPrintPreview.SynEditPrint := CommandsDataModule.SynEditPrint;
    ShowModal;
    Release;
  end;
end;

procedure TEditor.ExecSave;
begin
  if fForm <> nil then begin
    if fFileName <> '' then
      fForm.DoSave
    else
      fForm.DoSaveAs
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if fForm <> nil then
    fForm.DoSaveAs;
end;

// ISearchCommands implementation

function TEditor.CanFind: boolean;
begin
  Result := (fForm <> nil) and not IsEmpty;
end;

function TEditor.CanFindNext: boolean;
begin
  Result := (fForm <> nil) and not IsEmpty and
    (CommandsDataModule.EditorSearchOptions.SearchText <> '');
end;

function TEditor.CanReplace: boolean;
begin
  Result := (fForm <> nil) and not fIsReadOnly and not IsEmpty;
end;

procedure TEditor.ExecFind;
begin
  if fForm <> nil then
    CommandsDataModule.ShowSearchReplaceDialog(fForm.SynEdit, FALSE);
end;

procedure TEditor.ExecFindNext;
begin
  if fForm <> nil then
    CommandsDataModule.DoSearchReplaceText(fForm.SynEdit, FALSE, FALSE);
end;

procedure TEditor.ExecFindPrev;
begin
  if fForm <> nil then
    CommandsDataModule.DoSearchReplaceText(fForm.SynEdit, FALSE, TRUE);
end;

procedure TEditor.ExecReplace;
begin
  if fForm <> nil then
    CommandsDataModule.ShowSearchReplaceDialog(fForm.SynEdit, TRUE);
end;

function TEditor.IsEmpty: Boolean;
begin
  Result := (fForm.SynEdit.Lines.Count  = 0) or
    ((fForm.SynEdit.Lines.Count  = 1) and (fForm.SynEdit.Lines[0] = ''));
end;

{ TEditorFactory }

type
  TEditorFactory = class(TInterfacedObject, IEditorFactory)
  private
    // IEditorFactory implementation
    function CanCloseAll: boolean;
    procedure CloseAll;
    function CreateTabSheet(AOwner: TJvPageList): IEditor;
    function GetEditorCount: integer;
    function GetEditorByName(Name : string): IEditor;
    function GetEditorByNameOrTitle(Name : string): IEditor;
    function GetEditor(Index: integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    procedure RegisterViewFactory(ViewFactory : IEditorViewFactory);
    procedure SetupEditorViewMenu;
  private
    fEditors: TInterfaceList;
    fEditorViewFactories : TInterfaceList;
    constructor Create;
    destructor Destroy; override;
    procedure OnEditorViewClick(Sender : TObject);
  end;

constructor TEditorFactory.Create;
begin
  inherited Create;
  fEditors := TInterfaceList.Create;
  fEditorViewFactories := TInterfaceList.Create;
end;

destructor TEditorFactory.Destroy;
begin
  fEditors.Free;
  fEditorViewFactories.Free;
  inherited Destroy;
end;

function TEditorFactory.CanCloseAll: boolean;
var
  i: integer;
begin
  Result := False;
  with TPickListDialog.Create(Application.MainForm) do begin
    Caption := 'Save modified files';
    lbMessage.Caption := 'The following files have been modified.'+
    ' Please select the files that you wish to save and press the OK button. '+
    ' Press Cancel to go back to PyScripter';
    for i := 0 to fEditors.Count - 1 do
      if IEditor(fEditors[i]).Modified then
        CheckListBox.Items.AddObject(IEditor(fEditors[i]).GetFileNameOrTitle,
          IEditor(fEditors[i]).Form);
    SetScrollWidth;      
    mnSelectAllClick(nil);
    if CheckListBox.Items.Count = 0 then
      Result := True
    else if CheckListBox.Items.Count = 1  then
      Result := TEditorForm(CheckListBox.Items.Objects[0]).DoAskSaveChanges 
    else if ShowModal = IdOK then begin
      Result := True;
      for i := CheckListBox.Count - 1 downto 0 do begin
        if CheckListBox.Checked[i] then begin
          if not TEditorForm(CheckListBox.Items.Objects[i]).DoSave then begin
            Result := False;
            break;
          end;
        end;
      end;
    end;
    Free;
  end;

//  i := fEditors.Count - 1;
//  while i >= 0 do begin
//    LEditor := IEditor(fEditors[i]);
//    if not LEditor.AskSaveChanges then begin
//      Result := FALSE;
//      exit;
//    end;
//    Dec(i);
//  end;
//  Result := TRUE;

end;

procedure TEditorFactory.CloseAll;
var
  i: integer;
begin
  i := fEditors.Count - 1;
  while i >= 0 do begin
    IEditor(fEditors[i]).Close;
    Dec(i);
  end;
end;

function TEditorFactory.CreateTabSheet(AOwner: TJvPageList): IEditor;
var
  Sheet: TJvStandardPage;
  LForm: TEditorForm;
  TabBarItem : TJvTabBarItem;
begin
  Sheet := TJvStandardPage.Create(AOwner);
  Sheet.ControlStyle :=  Sheet.ControlStyle  + [csOpaque];
  TabBarItem := PyIDEMainForm.TabBar.AddTab('');
  TabBarItem.Selected := True;
  TabBarItem.Data := Sheet;
  try
    Sheet.PageList := AOwner;
    LForm := TEditorForm.Create(Sheet);
    with LForm do begin
      fEditor := TEditor.Create(LForm);
      fEditor.TabBarItem := TabBarItem;
      Result := fEditor;
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := TRUE;
      AOwner.ActivePage := Sheet;
      LForm.SetFocus;
    end;
    // fix for Delphi 4 (???)
    LForm.Realign;
    if Result <> nil then
      fEditors.Add(Result);
  except
    Sheet.Free;
  end;
end;

function TEditorFactory.GetEditorCount: integer;
begin
  Result := fEditors.Count;
end;

function TEditorFactory.GetEditorByName(Name: string): IEditor;
Var
  i : integer;
begin
  Result := nil;
  for i := 0 to fEditors.Count - 1 do
    if AnsiCompareText(IEditor(fEditors[i]).GetFileName,
      GetLongFileName(ExpandFileName(Name))) = 0 then
    begin
      Result := IEditor(fEditors[i]);
      break;
    end;
end;

function TEditorFactory.GetEditorByNameOrTitle(Name: string): IEditor;
Var
  i : integer;
begin
  Result := GetEditorByName(Name);
  if not Assigned(Result) then
    for i := 0 to fEditors.Count - 1 do
      if (IEditor(fEditors[i]).GetFileName = '') and
         (CompareText(IEditor(fEditors[i]).GetFileTitle, Name) = 0) then begin
        Result := IEditor(fEditors[i]);
        break;
      end;
end;

function TEditorFactory.GetEditor(Index: integer): IEditor;
begin
  Result := IEditor(fEditors[Index]);
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
var
  i: integer;
begin
  i := fEditors.IndexOf(AEditor);
  if i > -1 then
    fEditors.Delete(i);
end;

procedure TEditorFactory.RegisterViewFactory(ViewFactory: IEditorViewFactory);
begin
  fEditorViewFactories.Add(ViewFactory);
end;

procedure TEditorFactory.OnEditorViewClick(Sender: TObject);
Var
  ViewFactory: IEditorViewFactory;
  EditorView : IEditorView;
  Editor : IEditor;
  Index : Integer;
begin
  Editor := GI_ActiveEditor;
  if not assigned(Editor) then Exit;
  Index := (Sender as TTBXItem).Tag;
  if (Index >= 0) and (Index < fEditorViewFactories.Count) then begin
    ViewFactory := fEditorViewFactories[Index] as IEditorViewFactory;
    EditorView := Editor.ActivateView(ViewFactory);
    if Assigned(EditorView) then
      EditorView.UpdateView(Editor);
  end;
end;

procedure TEditorFactory.SetupEditorViewMenu;
Var
  MenuItem : TTBXItem;
  i : integer;
  ViewFactory: IEditorViewFactory;
begin
  fEditorViewFactories.Lock;
  try
    PyIdeMainForm.EditorViewsMenu.Enabled := fEditorViewFactories.Count > 0;
    for i := 0 to fEditorViewFactories.Count - 1 do begin
      ViewFactory := fEditorViewFactories[i] as IEditorViewFactory;

      // Add MenuItem
      MenuItem := TTBXItem.Create(PyIDEMainForm);
      MenuItem.Hint := ViewFactory.Hint;
      MenuItem.ImageIndex := ViewFactory.ImageIndex;
      MenuItem.Caption := ViewFactory.MenuCaption;
      MenuItem.ShortCut := ViewFactory.ShortCut;
      MenuItem.OnClick := OnEditorViewClick;
      MenuItem.Tag := i;

      PyIdeMainForm.EditorViewsMenu.Add(MenuItem);
    end;
  finally
    fEditorViewFactories.UnLock;
  end;
end;

{ TEditorForm }

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  DoAssignInterfacePointer(TRUE);
end;

procedure TEditorForm.FormDeactivate(Sender: TObject);
begin
  DoAssignInterfacePointer(FALSE);
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //PostMessage(Parent.Handle, WM_DELETETHIS, 0, 0);
  Action := caNone;
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
var
  LEditor: IEditor;
begin
  LEditor := fEditor;
  Assert(fEditor <> nil);
  fEditor.fForm := nil;
  Assert(GI_EditorFactory <> nil);
  GI_EditorFactory.RemoveEditor(LEditor);
  Breakpoints.Free;
  fSyntaxErrorPos.Free;

  RemoveThemeNotification(Self);
end;

procedure TEditorForm.SynEditChange(Sender: TObject);
begin
  CodeExplorerWindow.UpdateWindow;
  with PyControl.ErrorPos do
    if  Editor = GetEditor then begin
      Clear;
      PyIDEMainForm.DebuggerErrorPosChange(Self);
    end;
  fSyntaxErrorPos.Clear;
  fNeedToCheckSyntax := True;
end;

Var
  fOldEditorForm : TEditorForm = nil;

procedure TEditorForm.SynEditEnter(Sender: TObject);
begin
  DoAssignInterfacePointer(TRUE);
  CommandsDataModule.ParameterCompletion.Editor := SynEdit;
  CommandsDataModule.ModifierCompletion.Editor := SynEdit;
  CommandsDataModule.CodeTemplatesCompletion.Editor := SynEdit;
  CommandsDataModule.CodeTemplatesCompletion.OnBeforeExecute := AutoCompleteBeforeExecute;
  CommandsDataModule.CodeTemplatesCompletion.OnAfterExecute := AutoCompleteAfterExecute;
  fEditor.TabBarItem.Selected := True;

  if fEditor.HasPythonFile then begin
    CodeHint.OnGetCodeHint := CodeHintEventHandler;
    CodeHint.OnHyperLinkClick := CodeHintLinkHandler;
  end else begin
    CodeHint.OnGetCodeHint := nil;
    CodeHint.OnHyperLinkClick := nil;
  end;

  if fOldEditorForm <> Self then
    CodeExplorerWindow.UpdateWindow;
  fOldEditorForm := Self;
end;

procedure TEditorForm.SynEditExit(Sender: TObject);
begin
  // The following create problems
  // CommandsDataModule.ParameterCompletion.Editor := nil;
  // CommandsDataModule.ModifierCompletion.Editor := nil;
  // CommandsDataModule.CodeTemplatesCompletion.Editor := nil;
  DoAssignInterfacePointer(FALSE);
  CodeHint.CancelHint;
  CodeHint.OnGetCodeHint := nil;
  CodeHint.OnHyperLinkClick := nil;
end;

procedure TEditorForm.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  Assert(fEditor <> nil);
  if Changes * [scAll, scSelection] <> [] then
    fEditor.fHasSelection := SynEdit.SelAvail;
  if Changes * [scAll, scSelection] <> [] then
    fEditor.fIsReadOnly := SynEdit.ReadOnly;
  if Changes * [scAll, scModified] <> [] then
    fEditor.fModified := SynEdit.Modified;
end;

procedure TEditorForm.DoActivate;
//var
//  Sheet: TTabSheet;
//  PCtrl: TPageList;
begin
  if FormStyle = fsMDIChild then
    BringToFront
  else if Parent is TJvStandardPage then begin
//    Sheet := Parent as TJvStandardPage;
//    PCtrl := Sheet.PageList;
//    if PCtrl <> nil then
//      PCtrl.ActivePage := Sheet;
    if not fEditor.TabBarItem.Selected then
      fEditor.TabBarItem.Selected := True
    else
      //  make sure TabBarTabSelected is called so that the focus
      //  goes to the form
      PyIDEMainForm.TabBarTabSelected(Self, fEditor.TabBarItem);
  end;
end;

procedure TEditorForm.DoActivateEditor;
begin
  DoActivate;
  ViewsTabBar.Tabs[0].Selected := True;
  SynEdit.SetFocus;
end;

function TEditorForm.DoActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
var
  i : integer;
  ViewTab : TJvStandardPage;
  Form : TCustomForm;
begin
  Result := nil;
  DoActivate;
  // Does the EditorView tab exist?
  ViewTab := nil;
  for i := 0 to ViewsTabBar.Tabs.Count - 1 do
    if ViewsTabBar.Tabs[i].Caption = ViewFactory.TabCaption then begin
      ViewTab := ViewsTabBar.Tabs[i].Data as TJvStandardPage;
      ViewsTabBar.Tabs[i].Selected := True;
      Result := ViewTab.Components[0] as IEditorView;
      break;
    end;
  if not Assigned(ViewTab) then begin
    //  Editor View does not exist - Create
    ViewTab := TJvStandardPage.Create(EditorViews);
    try
      ViewTab.PageList := EditorViews;
      ViewTab.Caption :=  ViewFactory.TabCaption;
      Form := ViewFactory.CreateForm(fEditor, ViewTab);
      with Form do begin
        BorderStyle := bsNone;
        Parent := ViewTab;
        Align := alClient;
        Visible := True;
        Form.SetFocus;
        Result := Form as IEditorView;
      end;
      // fix for Delphi 4 (???)
      Form.Realign;
      //  Add Tab to the ViewsTabbar
      with ViewsTabbar.AddTab(ViewTab.Caption) do begin
        Data := ViewTab;
        Selected := True;
        PopupMenu := pmnuPageList;
      end;
    except
      ViewTab.Free;
      raise;
    end;
  end;

  ViewsTabbar.Visible := True;
end;

function TEditorForm.DoAskSaveChanges: boolean;
const
  MBType = MB_YESNOCANCEL or MB_ICONQUESTION;
var
  s: string;
begin
  // this is necessary to prevent second confirmation when closing MDI childs
  if SynEdit.Modified then begin
    DoActivateEditor;
    MessageBeep(MB_ICONQUESTION);
    Assert(fEditor <> nil);
    s := Format(SAskSaveChanges, [ExtractFileName(fEditor.GetFileTitle)]);
    case Application.MessageBox(PChar(s), PChar(Application.Title), MBType) of
      IDYes: Result := DoSave;
      IDNo: Result := TRUE;
    else
      Result := FALSE;
    end;
  end else
    Result := TRUE;
end;

procedure TEditorForm.DoAssignInterfacePointer(AActive: boolean);
begin
  if AActive then begin
    GI_ActiveEditor := fEditor;
    GI_EditCmds := fEditor;
    GI_FileCmds := fEditor;
    GI_SearchCmds := fEditor;
  end else begin
    if GI_ActiveEditor = IEditor(fEditor) then
      GI_ActiveEditor := nil;
    if GI_EditCmds = IEditCommands(fEditor) then
      GI_EditCmds := nil;
    if GI_FileCmds = IFileCommands(fEditor) then
      GI_FileCmds := nil;
    if GI_SearchCmds = ISearchCommands(fEditor) then
      GI_SearchCmds := nil;
  end;
end;

function TEditorForm.DoSave: boolean;
begin
  Assert(fEditor <> nil);
  if fEditor.fFileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TEditorForm.DoSaveFile: boolean;
Var
  FileStream : TFileStream;
  S : string;
begin
  Assert(fEditor <> nil);
  try
    CommandsDataModule.JvChangeNotify.Active := False;
    // Create Backup
    if CommandsDataModule.PyIDEOptions.CreateBackupFiles and
      FileExists(fEditor.fFileName) then
    begin
      try
        FileBackup(fEditor.fFileName);
      except
        MessageDlg(Format('Failed to backup file "%s"', [fEditor.fFileName]),
          mtWarning, [mbOK], 0);
      end;
    end;

    Result := True;
    if fEditor.fFileEncoding = sf_Ansi then
      Result := fEditor.GetEncodedTextEx(S, True);

    if Result then begin
      FileStream := TFileStream.Create(fEditor.fFileName, fmCreate);
      try
        case fEditor.fFileEncoding of
          sf_Ansi : FileStream.WriteBuffer(S[1], Length(S));
          sf_UTF8 : SaveToStream(SynEdit.Lines, FileStream, seUTF8, True);
          sf_UTF8_NoBOM : SaveToStream(SynEdit.Lines, FileStream, seUTF8, False);
        end;
      finally
        FileStream.Free;
      end;

      FileTime := GetFileLastWrite(fEditor.fFileName);
      CommandsDataModule.UpdateChangeNotify;
      if not CommandsDataModule.PyIDEOptions.UndoAfterSave then
        SynEdit.ClearUndo;
      SynEdit.Modified := False;
    end;
  except
    on E: Exception do begin
      MessageBox(0, PChar(E.Message), PChar(Format('Error in saving file: "%s"', [fEditor.fFileName])),
        MB_ICONERROR or MB_OK);
      Result := False;
    end;
  end;
end;

function TEditorForm.DoSaveAs: boolean;
var
  NewName: string;
  Edit : IEditor;
begin
  Assert(fEditor <> nil);
  NewName := fEditor.GetFileNameOrTitle;
  if (fEditor.GetFileName = '') and (DefaultExtension <> '') and
     (ExtractFileExt(NewName) = '')
  then
    NewName := NewName + '.' + DefaultExtension;
  if CommandsDataModule.GetSaveFileName(NewName, SynEdit.Highlighter, DefaultExtension) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(NewName);
    if Assigned(Edit) and (Edit <> Self.fEditor as IEditor) then begin
      MessageDlg('Another editor with the same file is open. '+
        ' You can not have two editors with the same file.', mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    fEditor.DoSetFileName(NewName);
    DoUpdateHighlighter;
    DoUpdateCaption;
    Result := DoSaveFile;
  end else
    Result := FALSE;
end;

procedure TEditorForm.DoUpdateCaption;
begin
  Assert(fEditor <> nil);
  with fEditor.TabBarItem do begin
    Caption := fEditor.GetFileTitle;
    Hint := fEditor.GetFileName;
  end;
end;

procedure TEditorForm.DoUpdateHighlighter(HighlighterName : string = '');
var
  Index : integer;
begin
  Assert(fEditor <> nil);
  if fEditor.fFileName <> '' then
    SynEdit.Highlighter := CommandsDataModule.GetHighlighterForFile(
      fEditor.fFileName)
  else if HighlighterName <> '' then begin
    Index := CommandsDataModule.Highlighters.IndexOf(HighlighterName);
    if Index < 0 then
      SynEdit.Highlighter := nil
    else
      SynEdit.Highlighter :=
        CommandsDataModule.Highlighters.Objects[Index] as TSynCustomHighlighter;
  end else //No highlighter otherwise
    SynEdit.Highlighter := nil;
  SynEdit.RegisterCommandHandler( doProcessUserCommand, nil );
end;

procedure TEditorForm.EditorMouseWheel(theDirection: Integer;
  Shift: TShiftState);
  function OwnScroll(Shift: TShiftState; LinesInWindow: Integer): Integer;
  begin
    if ssShift in Shift
    then
      Result := LinesInWindow
    else
      Result := Mouse.WheelScrollLines;
  end;
//
begin
{*
  Manage Zoom in and out, Page up and down, Line scroll - with the Mouse Wheel
*}
  if ssCtrl in Shift then
    EditorZoom( theDirection )
  else
    SynEdit.TopLine := SynEdit.TopLine +
     (theDirection * OwnScroll( Shift, SynEdit.LinesInWindow ) );
end;

procedure TEditorForm.EditorZoom(theZoom: Integer);
begin
  if not ( (theZoom < 1) and (SynEdit.Font.Size <= 2) ) then begin
    SynEdit.Font.Size        := SynEdit.Font.Size        + theZoom;
    SynEdit.Gutter.Font.Size := Max(SynEdit.Font.Size -2, 1);
  end;
end;

procedure TEditorForm.doProcessUserCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: pointer);
var
  iPrevLine, Indent: string;
  Position, Len: integer;
  OldOptions : TSynEditorOptions;
  OpenBrackets, CloseBrackets : WideString;
  OpenBracketPos : integer;
  Line : WideString;
  CharRight, CharLeft : WideChar;
begin
  if (Command = ecCodeCompletion) and not AfterProcessing and
     (SynEdit.Highlighter = CommandsDataModule.SynPythonSyn) then
  begin
    if SynCodeCompletion.Form.Visible then
      SynCodeCompletion.CancelCompletion;
    //SynCodeCompletion.DefaultType := ctCode;
    SynCodeCompletion.ActivateCompletion;
    Command := ecNone;
  end else if (Command = ecParamCompletion) and not AfterProcessing and
              (SynEdit.Highlighter = CommandsDataModule.SynPythonSyn) then
  begin
    if SynParamCompletion.Form.Visible then
      SynParamCompletion.CancelCompletion;
    //SynCodeCompletion.DefaultType := ctParams;
    SynParamCompletion.ActivateCompletion;
    Command := ecNone;
  end else if (Command = ecLineBreak) and AfterProcessing and SynEdit.InsertMode and
    (SynEdit.Highlighter = CommandsDataModule.SynPythonSyn) and not fAutoCompleteActive
  then begin
    { CaretY should never be lesser than 2 right after ecLineBreak, so there's
    no need for a check }

    iPrevLine := TrimRight( SynEdit.Lines[ SynEdit.CaretY -2 ] );

    Position := 1;
    Indent := '';
    while (Length(iPrevLine)>=Position) and
         (iPrevLine[Position] in [#09, #32]) do begin
      Indent := Indent + iPrevLine[Position];
      Inc(Position);
    end;

    if CommandsDataModule.IsBlockOpener(iPrevLine) or (Indent <> '') then
    begin
      SynEdit.UndoList.BeginBlock;
      OldOptions := SynEdit.Options;
      SynEdit.Options := SynEdit.Options - [eoTrimTrailingSpaces];
      try
        if (eoAutoIndent in SynEdit.Options) and (iPrevLine <> '') then begin
          // undo the effect of autoindent
          Position := SynEdit.CaretX;
          SynEdit.BlockBegin := BufferCoord(1, SynEdit.CaretY);
          SynEdit.BlockEnd :=  BufferCoord(Position, SynEdit.CaretY);
          SynEdit.SelText := '';
        end;

        if CommandsDataModule.IsBlockOpener(iPrevLine) then begin
          if eoTabsToSpaces in SynEdit.Options then
            Indent := Indent + StringOfChar(' ', SynEdit.TabWidth)
          else
            Indent := indent + #9;
        end else if CommandsDataModule.IsBlockCloser(iPrevLine) then begin
          if (eoTabsToSpaces in SynEdit.Options) and (Length(Indent) > 0) and
            (Indent[Length(Indent)] <> #9)
          then
            Delete(Indent, Length(Indent) - SynEdit.TabWidth + 1, SynEdit.TabWidth)
          else
            Delete(Indent, Length(Indent), 1);
        end;
        // use ReplaceSel to ensure it goes at the cursor rather than end of buffer
        if Trim(iPrevLine) <> '' then
          SynEdit.SelText := indent;
      finally
        SynEdit.UndoList.EndBlock;
        SynEdit.Options := OldOptions;
      end;
    end;
    SynEdit.InvalidateGutterLine(SynEdit.CaretY - 1);
  end  else if (Command = ecChar) and AfterProcessing and not fAutoCompleteActive
    and CommandsDataModule.PyIDEOptions.AutoCompleteBrackets then
  with SynEdit do begin
    if SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then begin
      OpenBrackets := '([{"''';
      CloseBrackets := ')]}"''';
    end else if (SynEdit.Highlighter = CommandsDataModule.SynHTMLSyn) or
       (SynEdit.Highlighter = CommandsDataModule.SynXMLSyn) or
       (SynEdit.Highlighter = CommandsDataModule.SynCssSyn) then
    begin
      OpenBrackets := '<"''';
      CloseBrackets := '>"''';
    end else
      Exit;

    Line := LineText;
    Len := Length(LineText);

    if aChar = fCloseBracketChar then begin
      if InsertMode and (CaretX <= Len) and (Line[CaretX] = fCloseBracketChar) then
        ExecuteCommand(ecDeleteChar, WideChar(#0), nil);
      fCloseBracketChar := #0;
    end else begin
      fCloseBracketChar := #0;
      OpenBracketPos := Pos(aChar, OpenBrackets);

      if (OpenBracketPos > 0) then begin
        CharRight := WideNull;
        Position := CaretX;
        while (Position <= Len) and Highlighter.IsWhiteChar(LineText[Position]) do
          Inc(Position);
        if Position <= Len then
          CharRight := Line[Position];

        CharLeft := WideNull;
        Position := CaretX-2;
        while (Position >= 1) and Highlighter.IsWhiteChar(LineText[Position]) do
          Dec(Position);
        if Position >= 1 then
          CharLeft := Line[Position];

        if (CharRight <> aChar) and not Highlighter.IsIdentChar(CharRight) and
          not ((aChar in [WideChar('"'), WideChar('''')])
          and (Highlighter.IsIdentChar(CharLeft) or (CharLeft= aChar))) then
        begin
          SelText := CloseBrackets[OpenBracketPos];
          CaretX := CaretX - 1;
          fCloseBracketChar := CloseBrackets[OpenBracketPos];
        end;
      end;
    end;
  end;
end;

procedure TEditorForm.PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  LH, X, Y: integer;
  LI: TDebuggerLineInfos;
  ImgIndex: integer;
begin
  if (PyControl.ActiveDebugger <> nil) and SynEdit.Gutter.Visible then
  begin
    FirstLine := SynEdit.RowToLine(FirstLine);
    LastLine := SynEdit.RowToLine(LastLine);
    X := 14;
    LH := SynEdit.LineHeight;
    while FirstLine <= LastLine do
    begin
      Y := (LH - imglGutterGlyphs.Height) div 2
           + LH * (SynEdit.LineToRow(FirstLine) - SynEdit.TopLine);
      LI := PyControl.GetLineInfos(fEditor, FirstLine);
      if dlCurrentLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 2
        else
          ImgIndex := 1;
      end else if dlExecutableLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 3
        else if dlDisabledBreakpointLine in LI then
          ImgIndex := 5
        else if CommandsDataModule.PyIDEOptions.MarkExecutableLines then
          ImgIndex := 0
        else
          ImgIndex := -1
      end else begin
        if dlBreakpointLine in LI then
          ImgIndex := 4
        else if dlDisabledBreakpointLine in LI then
          ImgIndex := 5
        else
          ImgIndex := -1;
      end;
      if ImgIndex >= 0 then
        imglGutterGlyphs.Draw(ACanvas, X, Y, ImgIndex);
      Inc(FirstLine);
    end;
  end;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  FormResize(Self);
  FGPanelExit(Self);

  SynEdit.ControlStyle := Synedit.ControlStyle + [csOpaque];
  ViewsTabBar.ControlStyle := ViewsTabBar.ControlStyle + [csOpaque];
  FGPanel.ControlStyle := FGPanel.ControlStyle + [csOpaque];
  EditorViews.ControlStyle := EditorViews.ControlStyle + [csOpaque];
  SourcePage.ControlStyle := SourcePage.ControlStyle  + [csOpaque];

  fHotIdentInfo.HaveHotIdent := False;
  fSyntaxErrorPos := TEditorPos.Create;

  ViewsTabBar.Visible := False;
  ViewsTabBar.Tabs[0].Data := SourcePage;
  case CommandsDataModule.PyIDEOptions.EditorTabPosition of
    toTop:
      begin
        ViewsTabBar.Orientation := toBottom;
        ViewsTabBar.Align := alBottom;
      end;
    toBottom:
      begin
        ViewsTabBar.Orientation := toTop;
        ViewsTabBar.Align := alTop;
      end;
  end;

  BreakPoints := TObjectList.Create(True);
  TDebugSupportPlugin.Create(Self); // No need to free

  AddThemeNotification(Self);

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
end;

procedure TEditorForm.SynEditGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if (SynEdit.Highlighter = CommandsDataModule.SynPythonSyn) and
    (PyControl.ActiveDebugger <> nil) and not (sfGutterDragging in SynEdit.StateFlags)
  then
    PyControl.ToggleBreakpoint(fEditor, SynEdit.RowToLine(Line));
end;

procedure TEditorForm.SynEditSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  LI: TDebuggerLineInfos;
begin
  if PyControl.ActiveDebugger <> nil then begin
    LI := PyControl.GetLineInfos(fEditor, Line);
    if dlCurrentLine in LI then begin
      Special := TRUE;
      FG := clWhite;
      BG := clBlue;
    end else if (dlErrorLine in LI) then
    begin
      Special := TRUE;
      FG := clWhite;
      BG := clRed
    end;
  end;
end;

function TEditorForm.GetEditor: IEditor;
begin
  Result := fEditor;
end;

procedure TEditorForm.SynEditPaintTransient(Sender: TObject;
  Canvas: TCanvas; TransientType: TTransientType);
Var
  Pix: TPoint;
begin
  if (not Assigned(SynEdit.Highlighter)) then
    Exit;
  if fHotIdentInfo.HaveHotIdent and (TransientType = ttAfter) then begin
    Pix := SynEdit.RowColumnToPixels(SynEdit.BufferToDisplayPos(fHotIdentInfo.StartCoord));
    Canvas.Font.Assign(SynEdit.Font);
    Canvas.Font.Style := fHotIdentInfo.SynAttri.Style + [fsUnderline];
    Canvas.Font.Color:= clBlue;
    if fHotIdentInfo.SynAttri.Background <> clNone then
      Canvas.Brush.Color := fHotIdentInfo.SynAttri.Background
    else
      Canvas.Brush.Color := SynEdit.Color;
    Canvas.Brush.Style := bsSolid;
    SetTextCharacterExtra(Canvas.Handle, SynEdit.CharWidth - Canvas.TextWidth('W'));
    Canvas.TextOut(Pix.X, Pix.Y, fHotIdentInfo.SynToken);
  end;
  CommandsDataModule.PaintMatchingBrackets(Canvas, SynEdit, TransientType);
end;

procedure TEditorForm.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Cancel Code Hint
  CodeHint.CancelHint;
end;

procedure TEditorForm.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if fHotIdentInfo.HaveHotIdent then begin
    fHotIdentInfo.HaveHotIdent := False;
    SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
    SetCursor(Screen.Cursors[crIBeam])
  end;
end;

procedure TEditorForm.SynEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TokenType, Start, ExpStart: Integer;
  Token, LineTxt: WideString;
  Attri: TSynHighlighterAttributes;
  Pix : TPoint;
  aLineCharPos : TBufferCoord;
begin
  if X < Synedit.Gutter.Width then exit;
  aLineCharPos := SynEdit.DisplayToBufferPos(SynEdit.PixelsToRowColumn(X, Y));
  // Syntax error hints
  if CommandsDataModule.PyIDEOptions.CheckSyntaxAsYouType and
     fSyntaxErrorPos.IsSyntax and (fSyntaxErrorPos.Editor = GetEditor) and
     (aLineCharPos.Line = fSyntaxErrorPos.Line) and
     (aLineCharPos.Char <= fSyntaxErrorPos.Char) then
  with SynEdit do begin
    Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(BufferCoord(1, aLineCharpos.Line))));
    fHintIdentInfo.IdentArea.TopLeft := Pix;
    Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(BufferCoord(fSyntaxErrorPos.Char, aLineCharpos.Line))));
    fHintIdentInfo.IdentArea.Right := Pix.X;
    fHintIdentInfo.IdentArea.Bottom := Pix.Y + LineHeight + 3;
    Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(aLineCharPos)));
    Pix.Y := Pix.Y + LineHeight;
    fHintIdentInfo.SynToken := 'Syntax Error';
    CodeHint.ActivateHintAt(fHintIdentInfo.IdentArea, Pix);
  end else if SynEdit.Focused and fEditor.HasPythonFile and
    (HiWord(GetAsyncKeyState(VK_CONTROL)) = 0) and
    not SynEdit.IsPointInSelection(aLineCharPos) and
    //(FindVCLWindow(SynEdit.ClientToScreen(Point(X,Y))) = SynEdit) and
    (((PyControl.DebuggerState = dsPaused) and
    CommandsDataModule.PyIDEOptions.ShowDebuggerHints) or
    ((PyControl.DebuggerState = dsInactive) and
    CommandsDataModule.PyIDEOptions.ShowCodeHints))
  then with SynEdit do begin
    // Code and debugger hints
    GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start, Attri);
    if (Attri = CommandsDataModule.SynPythonSyn.IdentifierAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.NonKeyAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.SystemAttri) or
       ((PyControl.DebuggerState = dsPaused) and ((Token = ')')
        or (Token = ']'))) then
    begin
      with fHintIdentInfo do begin
        LineTxt := Lines[aLineCharPos.Line - 1];
        Ident := Token;
        DottedIdent :=
          GetWordAtPos(LineTxt, aLineCharPos.Char, IdentChars+['.'], True, False, True);
        ExpStart := aLineCharPos.Char - Length(DottedIdent) + 1;
        DottedIdent := DottedIdent +
          GetWordAtPos(LineTxt, aLineCharPos.Char + 1, IdentChars, False, True);
        //Determine the hint area
        StartCoord := BufferCoord(Start, aLineCharPos.Line);
        Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(StartCoord)));
        IdentArea.TopLeft := Pix;
        aLineCharPos := WordEndEx(aLineCharPos);
        if (Token = ']') or (Token = ')') then
          Inc(aLineCharPos.Char);
        Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(aLineCharPos)));
        IdentArea.Right := Pix.X;
        IdentArea.Bottom := Pix.Y + LineHeight + 3;
        // Determine where the hint should be shown (beginning of the expression)
        if PyControl.DebuggerState = dsPaused then
          aLineCharPos := BufferCoord(ExpStart, aLineCharPos.Line)
        else
          aLineCharPos := StartCoord;
        Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(aLineCharPos)));
        Pix.Y := Pix.Y + LineHeight;
        // Activate the hint
        CodeHint.ActivateHintAt(IdentArea, Pix);
      end;
    end;
  end;
end;

procedure TEditorForm.SynEditMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel( +1, Shift );
  Handled := True;
end;

procedure TEditorForm.SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel( -1, Shift );
  Handled := True;
end;

procedure TEditorForm.SynEditMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  TokenType, Start: Integer;
  Token: WideString;
  Attri: TSynHighlighterAttributes;
begin
  if fHotIdentInfo.HaveHotIdent then begin
    fHotIdentInfo.HaveHotIdent := False;
    SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
  end;
  if SynEdit.Focused and (HiWord(GetAsyncKeyState(VK_CONTROL)) > 0) and
     fEditor.HasPythonFile and not SynEdit.IsPointInSelection(aLineCharPos)
  then with SynEdit do begin
    GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start, Attri);
    if (Attri = CommandsDataModule.SynPythonSyn.IdentifierAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.NonKeyAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.SystemAttri) then
    begin
      aCursor := crHandPoint;
      with fHotIdentInfo do begin
        HaveHotIdent := True;
        SynAttri := Attri;
        SynToken := Token;
        StartCoord := BufferCoord(Start, aLineCharPos.Line);
      end;
      InvalidateLine(aLineCharPos.Line);
    end;
  end;
end;

procedure TEditorForm.SynEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with PyControl.ErrorPos do
    if Editor = GetEditor then begin
      Clear;
      PyIDEMainForm.DebuggerErrorPosChange(Self);
    end;

  if fHotIdentInfo.HaveHotIdent then begin
    fHotIdentInfo.HaveHotIdent := False;
    SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
    PostMessage(PyIDEMainForm.Handle, WM_FINDDEFINITION, fHotIdentInfo.StartCoord.Char,
      fHotIdentInfo.StartCoord.Line);
  end;
end;

procedure TEditorForm.FGPanelEnter(Sender: TObject);
begin
  HasFocus := True;
  Color := CurrentTheme.GetItemColor(GetItemInfo('hot'));
  //Color := GetBorderColor('active');
end;

procedure TEditorForm.FGPanelExit(Sender: TObject);
begin
  HasFocus := False;
//  Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
  Color := GetBorderColor('inactive');
end;

procedure TEditorForm.FormResize(Sender: TObject);
begin
  with FGPanel do begin
    if (Top <> 3) or (Left <> 3) or (Width <> Self.ClientWidth - 6)
      or (Height <> Self.ClientHeight - 6)
    then begin
      Anchors :=[];
      Top := 3;
      Left := 3;
      Width := Self.ClientWidth - 6;
      Height := Self.ClientHeight - 6;
      Anchors :=[akLeft, akRight, akTop, akBottom];
    end;
  end;
end;

procedure TEditorForm.CloseTabClick(Sender: TObject);
begin
  if ViewsTabBar.SelectedTab.Data <> SourcePage then begin
    TJvStandardPage(ViewsTabbar.SelectedTab.Data).Free;
    ViewsTabBar.SelectedTab.Free;
    if ViewsTabBar.Tabs.Count = 1 then begin
      ViewsTabBar.Visible := False;
    end;
  end;
end;

procedure TEditorForm.ViewsTabBarTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
begin
  EditorViews.ActivePage := TJvStandardPage(ViewsTabbar.SelectedTab.Data);
end;

procedure TEditorForm.AddWatchAtCursor;
var
  TokenType, Start: Integer;
  Token, LineTxt, DottedIdent: WideString;
  Attri: TSynHighlighterAttributes;
  aLineCharPos : TBufferCoord;
begin
 aLineCharPos := SynEdit.CaretXY;
 if fEditor.HasPythonFile then with SynEdit do begin
    GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start, Attri);
    if (Attri = CommandsDataModule.SynPythonSyn.IdentifierAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.NonKeyAttri) or
       (Attri = CommandsDataModule.SynPythonSyn.SystemAttri) or
       ((Token = ')') or (Token = ']')) then
    begin
      LineTxt := Lines[aLineCharPos.Line - 1];
      DottedIdent :=
        GetWordAtPos(LineTxt, aLineCharPos.Char, IdentChars+['.'], True, False, True);
      DottedIdent := DottedIdent +
        GetWordAtPos(LineTxt, aLineCharPos.Char + 1, IdentChars, False, True);
      if DottedIdent <> '' then
        WatchesWindow.AddWatch(DottedIdent);
    end;
  end;
end;

procedure TEditorForm.AutoCompleteAfterExecute(Sender: TObject);
begin
  fAutoCompleteActive := False;
  CommandsDataModule.actReplaceParametersExecute(nil);
end;

procedure TEditorForm.AutoCompleteBeforeExecute(Sender: TObject);
begin
  fAutoCompleteActive := True;
end;

procedure TEditorForm.TBMThemeChange(var Message: TMessage);

begin
  if Message.WParam = TSC_VIEWCHANGE then begin
    if HasFocus then
      Color := CurrentTheme.GetItemColor(GetItemInfo('hot'))
//      Color := GetBorderColor('active')
    else
//      Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
      Color := GetBorderColor('inactive');

    PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
    SynEdit.InvalidateGutter;
    Invalidate;
  end;
end;

procedure TEditorForm.SynEditReplaceText(Sender: TObject; const ASearch,
  AReplace: WideString; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := SynEdit.ClientToScreen(
      SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(
      BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + SynEdit.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

procedure TEditorForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
  var CanExecute: Boolean);
var
  locline, lookup: string;
  TmpX, Index, ImageIndex, i,
  TmpLocation    : Integer;
  FoundMatch     : Boolean;
  DisplayText, InsertText, FName, ErrMsg, S : string;
  NameSpace, SortedNameSpace : TStringList;
  Scope: TCodeElement;
  Def, CE : TBaseCodeElement;
  ParsedModule, ParsedBuiltInModule : TParsedModule;
  PythonPathAdder : IInterface;
begin
  if not fEditor.HasPythonFile or PyControl.IsRunning then begin
    CanExecute := False;
    Exit;
  end;

  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    TmpLocation := 0;

    lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False);
    Index := CharLastPos(lookup, '.');

    FName := GetEditor.GetFileNameOrTitle;
    // Add the file path to the Python path - Will be automatically removed
    PythonPathAdder := InternalInterpreter.AddPathToPythonPath(ExtractFilePath(FName));

    PyScripterRefactor.InitializeQuery;
    // GetParsedModule
    ParsedModule := PyScripterRefactor.GetParsedModule(
      FileNameToModuleName(FName), None);
    Scope := nil;
    if Assigned(ParsedModule) then
      Scope := ParsedModule.GetScopeForLine(CaretY);
    if Assigned(ParsedModule) and Assigned(Scope) then begin
      NameSpace := TStringList.Create;
      SortedNameSpace := TStringList.Create;

      try
        DisplayText := '';
        if Index > 0 then begin
          lookup := Copy(lookup, 1, Index-1);
          Def := PyScripterRefactor.FindDottedDefinition(lookup, ParsedModule,
            Scope, ErrMsg);
          if Assigned(Def) and(Def.ClassType = TVariable) then
            Def := PyScripterRefactor.GetType(TVariable(Def), ErrMsg);
          if Assigned(Def) then
            (Def as TCodeElement).GetNameSpace(NameSpace);
        end else begin
          // extract namespace from current scope and its parents
          while Assigned(Scope) do begin
            Scope.GetNameSpace(NameSpace);
            Scope := Scope.Parent as TCodeElement;
          end;

          //  builtins (could add keywords as well)
          ParsedBuiltInModule := PyScripterRefactor.GetParsedModule('__builtin__', None);
          ParsedBuiltInModule.GetNameSpace(NameSpace);
        end;

        SortedNameSpace.Duplicates := dupIgnore; // Remove duplicates
        SortedNameSpace.Sorted := True;
        SortedNameSpace.AddStrings(NameSpace);
        SortedNameSpace.CustomSort(ComparePythonIdents);
        InsertText := SortedNamespace.Text;
        for i := 0 to SortedNamespace.Count - 1 do begin
          S := SortedNamespace[i];
          CE := SortedNamespace.Objects[i] as TBaseCodeElement;
          if (CE is TParsedModule) or (CE is TModuleImport) then
            ImageIndex := 16
          else if CE is TParsedFunction then begin
            if CE.Parent is TParsedClass then
              ImageIndex := 14
            else
              ImageIndex := 17
          end else if CE is TParsedClass then
            ImageIndex := 13
          else begin  // TVariable or TParsedVariable
            if CE.Parent is TParsedClass then
              ImageIndex := 1
            else
              ImageIndex := 0
          end;
          DisplayText := DisplayText + Format('\Image{%d}\hspace{2}%s', [ImageIndex, S]);
          if i < SortedNamespace.Count - 1 then
            DisplayText := DisplayText + #10;
        end;
      finally
        NameSpace.Free;
        SortedNameSpace.Free;
      end;
    end;
    FoundMatch := DisplayText <> '';
  end;

  CanExecute := FoundMatch;

  if CanExecute then begin
    TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
    TSynCompletionProposal(Sender).ItemList.Text := DisplayText;
    TSynCompletionProposal(Sender).InsertList.Text := InsertText;
  end else begin
    TSynCompletionProposal(Sender).ItemList.Clear;
    TSynCompletionProposal(Sender).InsertList.Clear;
  end;
end;

procedure TEditorForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
  var CanExecute: Boolean);
var
  locline, lookup: String;
  TmpX, StartX,
  ParenCounter,
  TmpLocation : Integer;
  FoundMatch : Boolean;
  FName, DisplayText, ErrMsg, Doc : string;
  p : TPoint;
  Scope: TCodeElement;
  Def: TBaseCodeElement;
  ParsedModule : TParsedModule;
  PythonPathAdder : IInterface;
  TokenType, Start: Integer;
  Token: WideString;
  Attri: TSynHighlighterAttributes;
begin
  if not fEditor.HasPythonFile or PyControl.IsRunning then begin
    CanExecute := False;
    Exit;
  end;

  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    StartX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;

    while (TmpX > 0) and not(FoundMatch) do
    begin
      if LocLine[TmpX] = ',' then
      begin
        inc(TmpLocation);
        dec(TmpX);
      end else if LocLine[TmpX] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if LocLine[TmpX] = ')' then inc(ParenCounter)
          else if LocLine[TmpX] = '(' then dec(ParenCounter);
          dec(TmpX);
        end;
      end else if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not(locLine[TmpX] in IdentChars+['.']) do  // added [.]
          Dec(TmpX);
        if TmpX > 0 then
        begin
          DisplayText := '';

          GetHighlighterAttriAtRowColEx(BufferCoord(TmpX, CaretY), Token,
            TokenType, Start, Attri);
          if (Attri = CommandsDataModule.SynPythonSyn.IdentifierAttri) or
             (Attri = CommandsDataModule.SynPythonSyn.NonKeyAttri) or
             (Attri = CommandsDataModule.SynPythonSyn.SystemAttri) then
          begin
            lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False);

            FName := GetEditor.GetFileNameOrTitle;
            // Add the file path to the Python path - Will be automatically removed
            PythonPathAdder := InternalInterpreter.AddPathToPythonPath(ExtractFilePath(FName));

            PyScripterRefactor.InitializeQuery;
            // GetParsedModule
            ParsedModule := PyScripterRefactor.GetParsedModule(
              FileNameToModuleName(FName), None);
            Scope := nil;
            if Assigned(ParsedModule) then
              Scope := ParsedModule.GetScopeForLine(CaretY);
            if Assigned(ParsedModule) and Assigned(Scope) then begin
              Def := PyScripterRefactor.FindDottedDefinition(lookup, ParsedModule,
                Scope, ErrMsg);

              if Assigned(Def) and (Def is TParsedClass) then
                Def := TParsedClass(Def).GetConstructor;

              if Assigned(Def) and (Def is TParsedFunction) then begin
                DisplayText := TParsedFunction(Def).ArgumentsString;
                // Remove self arguments from methods
                if StrIsLeft(PChar(DisplayText), 'self') then
                  Delete(DisplayText, 1, 4);
                if StrIsLeft(PChar(DisplayText), ', ') then
                  Delete(DisplayText, 1, 2);
                Doc := TParsedFunction(Def).DocString;
                if Doc <> '' then
                  Doc := GetNthLine(Doc, 1);
                FoundMatch := True;
              end;
            end;
          end;

          if not(FoundMatch) then
          begin
            TmpX := StartX;
            dec(TmpX);
          end;
        end;
      end else dec(TmpX)
    end;
  end;

  if FoundMatch then begin
    //CanExecute := (DisplayText <> '') or (Doc <> '');
    CanExecute := True;
  end else
    CanExecute := False;

  if CanExecute then begin
    with TSynCompletionProposal(Sender) do begin
      if DisplayText = '' then begin
        FormatParams := False;
        DisplayText :=  '\style{~B}' + SNoParameters + '\style{~B}';
      end else begin
        FormatParams := True;
      end;
      if (Doc <> '') then
        DisplayText := DisplayText + sLineBreak;

      Form.CurrentIndex := TmpLocation;
      ItemList.Text := DisplayText + Doc;
    end;

    //  position the hint window at and just below the opening bracket
    p := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(BufferCoord(Succ(StartX), SynEdit.CaretY))));
    Inc(p.y, SynEdit.LineHeight);
    x := p.X;
    y := p.Y;
  end else begin
    TSynCompletionProposal(Sender).ItemList.Clear;
    TSynCompletionProposal(Sender).InsertList.Clear;
  end;
end;

procedure TEditorForm.CodeHintEventHandler(Sender: TObject; AArea: TRect;
  var CodeHint: string);
Var
  ObjectValue, ObjectType, ErrMsg : string;
  CE : TBaseCodeElement;
begin
  if CompareMem(@fHintIdentInfo.IdentArea, @AArea, SizeOf(TRect)) then begin
    if (fHintIdentInfo.SynToken = 'Syntax Error') and
      (fSyntaxErrorPos.Editor = GetEditor) then
    begin
    // Syntax hint
       CodeHint := 'Syntax Error: ' + fSyntaxErrorPos.ErrorMsg;
    end else if (PyControl.DebuggerState = dsPaused) and
      CommandsDataModule.PyIDEOptions.ShowDebuggerHints then
    begin
      // Debugger hints
      PyControl.ActiveDebugger.Evaluate(fHintIdentInfo.DottedIdent, ObjectType, ObjectValue);
      if ObjectValue <> SNotAvailable then begin
        ObjectValue := HTMLSafe(ObjectValue);
        ObjectType := HTMLSafe(ObjectType);
        CodeHint := Format(SDebuggerHintFormat,
          [fHintIdentInfo.DottedIdent, ObjectType, ObjectValue]);
      end else
        CodeHint := '';
    end else if (PyControl.DebuggerState = dsInactive) and
      CommandsDataModule.PyIDEOptions.ShowCodeHints then
    begin
      // Code hints
      CE := PyScripterRefactor.FindDefinitionByCoordinates(fEditor.GetFileNameOrTitle,
        fHintIdentInfo.StartCoord.Line, fHintIdentInfo.StartCoord.Char, ErrMsg);
      if Assigned(CE) then begin
        CodeHint := CE.CodeHint;
      end else
        CodeHint := '';
    end;
  end else
    CodeHint := '';
end;

procedure TEditorForm.CodeHintLinkHandler(Sender: TObject; LinkName: string);
begin
  CodeHint.CancelHint;
  PyIDEMainForm.JumpToFilePosInfo(LinkName);
  PyIDEMainForm.AdjustBrowserLists(fEditor.GetFileNameOrTitle,
    fHintIdentInfo.StartCoord.Line, fHintIdentInfo.StartCoord.Char, LinkName);
end;

procedure TEditorForm.DoOnIdle;
begin
  if GetEditor.HasPythonFile and fNeedToCheckSyntax and
    CommandsDataModule.PyIDEOptions.CheckSyntaxAsYouType
  then begin
    InternalInterpreter.SyntaxCheck(GetEditor, True);
    fSyntaxErrorPos.Assign(PyControl.ErrorPos);
    PyControl.ErrorPos.Clear;
    fNeedToCheckSyntax := False;
  end;
end;

initialization
  GI_EditorFactory := TEditorFactory.Create;

finalization
  GI_EditorFactory := nil;
end.















