{-----------------------------------------------------------------------------
 Unit Name: dmResources
 Author:    Kiriakos Vlahos
 Date:      03-Jul-2023
 Purpose:   Strores project resources and non-visual components
 History:
-----------------------------------------------------------------------------}

unit dmResources;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Vcl.BaseImageCollection,
  JclSysUtils,
  JvStringHolder,
  JvDockVIDStyle,
  JvDockVSNetStyleSpTBX,
  SVGIconImageCollection,
  SynEditHighlighter,
  SynCompletionProposal,
  SynHighlighterPython,
  SynHighlighterGeneral,
  SynHighlighterJSON,
  SynEditCodeFolding,
  SynHighlighterCpp,
  SynHighlighterIni,
  SynHighlighterWeb,
  SynHighlighterYAML,
  uHighlighterProcs;

type
  TResourcesDataModule = class(TDataModule)
    dlgFileOpen: TOpenDialog;
    dlgFileSave: TSaveDialog;
    PrintDialog: TPrintDialog;
    PythonScripts: TJvMultiStringHolder;
    ParameterCompletion: TSynCompletionProposal;
    ModifierCompletion: TSynCompletionProposal;
    CodeTemplatesCompletion: TSynAutoComplete;
    icGutterGlyphs: TSVGIconImageCollection;
    icCodeImages: TSVGIconImageCollection;
    icBrowserImages: TSVGIconImageCollection;
    icSVGImages: TSVGIconImageCollection;
    SynWebEngine: TSynWebEngine;
    SynWebEsSyn: TSynWebEsSyn;
    SynWebPhpPlainSyn: TSynWebPhpPlainSyn;
    SynWebCssSyn: TSynWebCssSyn;
    SynWebXmlSyn: TSynWebXmlSyn;
    SynYAMLSyn: TSynYAMLSyn;
    SynWebHtmlSyn: TSynWebHtmlSyn;
    SynIniSyn: TSynIniSyn;
    SynCppSyn: TSynCppSyn;
    SynJSONSyn: TSynJSONSyn;
    SynGeneralSyn: TSynGeneralSyn;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure ModifierCompletionCodeCompletion(Sender: TObject; var Value: string;
        Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure ModifierCompletionExecute(Kind: SynCompletionType; Sender: TObject;
        var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure ParameterCompletionCodeCompletion(Sender: TObject; var Value: string;
        Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure ParameterCompletionExecute(Kind: SynCompletionType; Sender: TObject;
        var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
  private
    FLogger: TJclSimpleLog;
    FHighlighters: THighlighterList;
    procedure SynPythonSynChanged(Sender: TObject);
    procedure PyIDEOptionsChanged;
  public
    SynPythonSyn: TSynPythonSyn;
    SynCythonSyn: TSynCythonSyn;
    DockStyle: TJvDockVSNetStyleSpTBX;
    function GetSaveFileName(var ANewName: string;
      AHighlighter: TSynCustomHighlighter; DefaultExtension : string): boolean;
    procedure UpdateImageCollections;
    procedure ExportHighlighters;
    procedure ImportHighlighters;
    class function IsHighlighterStored(Highlighter: TObject): Boolean;
    property Logger: TJclSimpleLog read FLogger;
    property Highlighters: THighlighterList read FHighlighters;
  end;

{$SCOPEDENUMS ON}
  TCodeImages =(
    Python,
    Variable,
    Field,
    Func,
    Method,
    Klass,
    Namespace,
    List,
    Module,
    Keyword
  );
{$SCOPEDENUMS OFF}

var
  ResourcesDataModule: TResourcesDataModule;

implementation

uses
  System.UITypes,
  System.IOUtils,
  Vcl.Themes,
  JvAppStorage,
  JvAppIniStorage,
  JvGnugettext,
  TB2Item,
  StringResources,
  uEditAppIntfs,
  uCommonFunctions,
  cPyScripterSettings,
  cParameters;


{$R *.dfm}

procedure TResourcesDataModule.DataModuleDestroy(Sender: TObject);
begin
  FLogger.Free;
  FHighlighters.Free;
  PyIDEOptions.OnChange.RemoveHandler(PyIDEOptionsChanged);
  ResourcesDataModule := nil;
end;

procedure TResourcesDataModule.ExportHighlighters;
begin
  with dlgFileSave do begin
    Title := _(SExportHighlighters);
    Filter := SynIniSyn.DefaultFilter;
    DefaultExt := 'ini';
    if Execute then begin
      var IP := TSmartPtr.Make(TStringList.Create)();
      IP.Add('Name'); IP.Add('DefaultFilter'); IP.Add('DefaultExtension');
      var AppStorage := TSmartPtr.Make(TJvAppIniFileStorage.Create(nil))();
      AppStorage.FlushOnDestroy := True;
      AppStorage.Location := flCustom;
      AppStorage.FileName := FileName;
      AppStorage.StorageOptions.SetAsString := True;
      AppStorage.WriteString('PyScripter\Version', ApplicationVersion);
      AppStorage.WriteBoolean('PyScripter\SetAsString', True);
      AppStorage.DeleteSubTree('Highlighters');
      for var Highlighter in FHighlighters do
        if IsHighlighterStored(Highlighter) then
          AppStorage.WritePersistent(
            'Highlighters\'+ Highlighter.FriendlyLanguageName,
            Highlighter, True, IP);
      AppStorage.WritePersistent('Highlighters\Python Interpreter',
        GI_PyInterpreter.Editor.Highlighter, True, IP);
    end;
  end;
end;

function TResourcesDataModule.GetSaveFileName(var ANewName: string;
  AHighlighter: TSynCustomHighlighter; DefaultExtension: string): boolean;
begin
  with dlgFileSave do begin
    if ANewName <> '' then begin
      InitialDir := TPath.GetDirectoryName(ANewName);
      FileName := TPath.GetFileName(ANewName);
      Title := Format(_(SSaveAs), [FileName]);
    end else begin
      InitialDir := '';
      FileName := '';
      Title := _(SSaveFileAs);
    end;
    if AHighlighter <> nil then
      Filter := _(AHighlighter.DefaultFilter)
    else
      Filter := _(SFilterAllFiles);

    DefaultExt := DefaultExtension;
    //  Make the current file extension the default extension
    if DefaultExt = '' then
      DefaultExt := ExtractFileExt(ANewName);

    if Execute then begin
      ANewName := FileName;
      Result := TRUE;
    end else
      Result := FALSE;
  end;
end;

procedure TResourcesDataModule.ImportHighlighters;
begin
  with ResourcesDataModule.dlgFileOpen do begin
    Title := _(SImportHighlighters);
    Filter := SynIniSyn.DefaultFilter;
    FileName := '';
    if Execute then begin
      var AppStorage := TSmartPtr.Make(TJvAppIniFileStorage.Create(nil))();
      AppStorage.FlushOnDestroy := False;
      AppStorage.Location := flCustom;
      AppStorage.FileName := FileName;
      AppStorage.StorageOptions.SetAsString :=
        AppStorage.ReadBoolean('PyScripter\SetAsString', False);
      for var Highlighter in FHighlighters do
      begin
        Highlighter.BeginUpdate;
        try
          AppStorage.ReadPersistent(
            'Highlighters\' + Highlighter.FriendlyLanguageName,
            Highlighter);
        finally
          Highlighter.EndUpdate;
        end;
      end;
      GI_PyInterpreter.Editor.Highlighter.Assign(SynPythonSyn);
      SynCythonSyn.Assign(SynPythonSyn);
      SynCythonSyn.DefaultFilter := PyIDEOptions.CythonFileFilter;
      if AppStorage.IniFile.SectionExists('Highlighters\Python Interpreter') then
      begin
        GI_PyInterpreter.Editor.Highlighter.BeginUpdate;
        try
          AppStorage.ReadPersistent('Highlighters\Python Interpreter',
            GI_PyInterpreter.Editor.Highlighter);
        finally
          GI_PyInterpreter.Editor.Highlighter.EndUpdate;
        end;
      end;
    end;
  end;
end;

class function TResourcesDataModule.IsHighlighterStored(
  Highlighter: TObject): Boolean;
begin
  Result :=  not (Highlighter is TSynCythonSyn) and
    (not (Highlighter is TSynWebBase) or (Highlighter is TSynWebHtmlSyn));
end;

procedure TResourcesDataModule.DataModuleCreate(Sender: TObject);
begin
  // Create logger
  try
    FLogger := TJclSimpleLog.Create(TPyScripterSettings.PyScripterLogFile);
    FLogger.LoggingActive := False;
  except
  end;

  // SpTBXLib Font
  ToolbarFont.Size := 10;

  // Create JvDockVSNetStyleSpTBX docking style
  DockStyle := TJvDockVSNetStyleSpTBX.Create(Self);
  DockStyle.Name := 'JvDockVSNetStyleSpTBX';
  DockStyle.AlwaysShowGrabber := False;
  // JvDocking Fonts
  with DockStyle.TabServerOption as TJvDockVIDTabServerOption do begin
    ActiveFont.Assign(ToolbarFont);
    InactiveFont.Assign(ToolbarFont);
  end;

  // Highlighters
  SynCythonSyn := TSynCythonSyn.Create(Self);
  SynCythonSyn.DefaultFilter := PyIDEOptions.CythonFileFilter;

  FHighlighters := THighlighterList.Create;
  FHighlighters.GetHighlighters(Self, False);

  SynPythonSyn := TSynPythonSyn.Create(Self);
  SynPythonSyn.HookAttrChangeEvent(SynPythonSynChanged);
  FHighlighters.Insert(0, SynPythonSyn);

  //  Place General highlighter last
  var Index := FHighlighters.IndexOf(SynGeneralSyn);
  if Index >= 0 then FHighlighters.Delete(Index);
  fHighlighters.Add(SynGeneralSyn);

  // SynWeb Highlighters do not provide default filters
  SynWebHTMLSyn.DefaultFilter := PyIDEOptions.HTMLFileFilter;
  SynWebXMLSyn.DefaultFilter := PyIDEOptions.XMLFileFilter;
  SynWebCssSyn.DefaultFilter := PyIDEOptions.CSSFileFilter;
  SynWebEsSyn.DefaultFilter := PyIDEOptions.JSFileFilter;
  SynWebPhpPlainSyn.DefaultFilter := PyIDEOptions.PHPFileFilter;

  PyIDEOptions.OnChange.AddHandler(PyIDEOptionsChanged);
end;

procedure TResourcesDataModule.ModifierCompletionCodeCompletion(Sender:
    TObject; var Value: string; Shift: TShiftState; Index: Integer; EndToken:
    Char);
var
  L: Integer;
begin
  if Assigned(ModifierCompletion.Form.CurrentEditor) then
    with ModifierCompletion.Form.CurrentEditor do begin
      SelText := '';
      L:= Length(Parameters.StopMask);
      if (CaretX > 0) and (Copy(LineText, CaretX-L, L) = Parameters.StopMask) then
      begin
        CaretX:= CaretX - L;
        Value := '-' + Value;
      end else if not ((CaretX > 1) and (Lines[CaretY-1][CaretX-1] = '-')) then
      begin
        L:= LineText.LastIndexOf(Parameters.StopMask);
        if L >= 0 then CaretX := L + 1;
        Value := '-' + Value;
      end;
    end;
end;

procedure TResourcesDataModule.ModifierCompletionExecute(Kind:
    SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y:
    Integer; var CanExecute: Boolean);
var
  ModName, ModComment : string;
begin
  with ModifierCompletion do
  begin
    Title := _('Modifiers');
    NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
    ItemList.Clear;
    InsertList.Clear;
    for var I := 0 to Parameters.Modifiers.Count - 1 do
    begin
      ModName := Parameters.Modifiers.Names[I];
      ModComment := Parameters.Modifiers.Values[ModName];
      ItemList.Add(Format('\color{$FF8844}%s\color{clWindowText}\column{}%s',
        [ModName, ModComment]));
      InsertList.Add(ModName);
    end;
  end;
end;

procedure TResourcesDataModule.ParameterCompletionCodeCompletion(Sender:
    TObject; var Value: string; Shift: TShiftState; Index: Integer; EndToken:
    Char);
begin
  if ssCtrl in Shift then
    Value := Parameters.Values[Value]
  else
    Value := Parameters.MakeParameter(Value);
end;

procedure TResourcesDataModule.ParameterCompletionExecute(Kind:
    SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y:
    Integer; var CanExecute: Boolean);
var
  ParamName, ParamValue : string;
begin
  with ParameterCompletion do
  begin
    Title := _('Parameters');
    NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
    ItemList.Clear;
    InsertList.Clear;
    for var I := 0 to Parameters.Count - 1 do
    begin
      Parameters.Split(I, ParamName, ParamValue, False);
      ItemList.Add(Format('\color{$FF8844}%s\color{clWindowText}\column{}%s',
         [ParamName, StringReplace(ParamValue, '\', '\\', [rfReplaceAll])]));
      InsertList.Add(ParamName);
    end;
  end;
end;

{ TResourcesDataModule }

procedure TResourcesDataModule.PyIDEOptionsChanged;
begin
  //  Dock animation parameters
  DockStyle.SetAnimationInterval(PyIDEOptions.DockAnimationInterval);
  DockStyle.SetAnimationMoveWidth(PyIDEOptions.DockAnimationMoveWidth);

  // Filters
  SynPythonSyn.DefaultFilter := PyIDEOptions.PythonFileFilter;
  SynCythonSyn.DefaultFilter := PyIDEOptions.CythonFileFilter;
  SynWebHTMLSyn.DefaultFilter := PyIDEOptions.HTMLFileFilter;
  SynWebXMLSyn.DefaultFilter := PyIDEOptions.XMLFileFilter;
  SynWebCssSyn.DefaultFilter := PyIDEOptions.CSSFileFilter;
  SynCppSyn.DefaultFilter := PyIDEOptions.CPPFileFilter;
  SynYAMLSyn.DefaultFilter := PyIDEOptions.YAMLFileFilter;
  SynJSONSyn.DefaultFilter := PyIDEOptions.JSONFileFilter;
  SynGeneralSyn.DefaultFilter := PyIDEOptions.GeneralFileFilter;

  // Logging
  with FLogger do
    if LoggingActive <> PyIDEOptions.LoggingEnabled then
    begin
      LoggingActive := PyIDEOptions.LoggingEnabled;
      if LoggingActive then
      begin
        ClearLog;
        WriteStamp(0, False);
      end;
    end;

  // Parameter Completion
  ParameterCompletion.Font.Assign(PyIDEOptions.AutoCompletionFont);
  ParameterCompletion.TitleFont.Assign(PyIDEOptions.AutoCompletionFont);
  ParameterCompletion.TitleFont.Style := [TFontStyle.fsBold];

  // Modifier completion
  ModifierCompletion.Font.Assign(PyIDEOptions.AutoCompletionFont);
  ModifierCompletion.TitleFont.Assign(PyIDEOptions.AutoCompletionFont);
  ModifierCompletion.TitleFont.Style := [TFontStyle.fsBold];

  // Code Templates
  CodeTemplatesCompletion.GetCompletionProposal().Font.Assign(PyIDEOptions.AutoCompletionFont);
end;

procedure TResourcesDataModule.SynPythonSynChanged(Sender: TObject);
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    Editor.SynEdit.BracketsHighlight.SetFontColorsAndStyle(
      SynPythonSyn.MatchingBraceAttri.Foreground,
      SynPythonSyn.UnbalancedBraceAttri.Foreground, [TFontStyle.fsBold]);
    Editor.SynEdit2.BracketsHighlight.SetFontColorsAndStyle(
      SynPythonSyn.MatchingBraceAttri.Foreground,
      SynPythonSyn.UnbalancedBraceAttri.Foreground, [TFontStyle.fsBold]);
  end);
  GI_PyInterpreter.Editor.BracketsHighlight.SetFontColorsAndStyle(
    SynPythonSyn.MatchingBraceAttri.Foreground,
    SynPythonSyn.UnbalancedBraceAttri.Foreground, [TFontStyle.fsBold]);

  SynCythonSyn.Assign(SynPythonSyn);
  SynCythonSyn.DefaultFilter := PyIDEOptions.CythonFileFilter;
end;

procedure TResourcesDataModule.UpdateImageCollections;

  procedure ProcessImageCollection(IC: TSVGIconImageCollection;
    FixedColor: TColor; AntiAliasColor: TColor = TColors.SysDefault);
  begin
    IC.SVGIconItems.BeginUpdate;
    try
      IC.FixedColor := SvgFixedColor(FixedColor);

      if AntiAliasColor <> TColors.SysDefault then
        IC.AntiAliasColor := StyleServices.GetSystemColor(AntiAliasColor);
    finally
      IC.SVGIconItems.EndUpdate;
    end;
  end;

var
  TextColor: TColor;
begin
  var Details := StyleServices.GetElementDetails(ttbButtonNormal);
  if not StyleServices.GetElementColor(Details, ecTextColor, TextColor) then
    TextColor := StyleServices.GetSystemColor(TColors.SysBtnText);

  ProcessImageCollection(icBrowserImages, TextColor);
  ProcessImageCollection(icCodeImages, TColors.SysWindowText, TColors.SysWindow);
  ProcessImageCollection(icGutterGlyphs, TextColor);
  ProcessImageCollection(icSVGImages, TextColor);
end;

end.
