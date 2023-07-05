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
  SVGIconImageCollection,
  SynEditHighlighter,
  SynCompletionProposal;

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
    procedure PyIDEOptionsChanged;
  public
    function GetSaveFileName(var ANewName: string;
      AHighlighter: TSynCustomHighlighter; DefaultExtension : string): boolean;
    procedure UpdateImageCollections;
    property Logger: TJclSimpleLog read FLogger;
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
  JvGnugettext,
  StringResources,
  uCommonFunctions,
  cPyScripterSettings,
  cParameters;


{$R *.dfm}

procedure TResourcesDataModule.DataModuleDestroy(Sender: TObject);
begin
  FLogger.Free;
  PyIDEOptions.OnChange.RemoveHandler(PyIDEOptionsChanged);
  ResourcesDataModule := nil;
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

procedure TResourcesDataModule.DataModuleCreate(Sender: TObject);
begin
  // Create logger
  try
    FLogger := TJclSimpleLog.Create(TPyScripterSettings.PyScripterLogFile);
    FLogger.LoggingActive := False;
  except
  end;

  // Completion
  ParameterCompletion.FontsAreScaled := True;
  ModifierCompletion.FontsAreScaled := True;
  CodeTemplatesCompletion.GetCompletionProposal().FontsAreScaled := True;

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
