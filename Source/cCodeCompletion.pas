{-----------------------------------------------------------------------------
 Unit Name: cCodeCompletion
 Author:    Kiriakos Vlahos
 Purpose:   Code completion support classes
 History:
-----------------------------------------------------------------------------}

unit cCodeCompletion;

interface
uses
  Winapi.Windows,
  System.Contnrs,
  System.SyncObjs,
  SynEdit,
  SynEditTypes,
  SynEditHighlighter;

type
  TBaseCodeCompletionSkipHandler = class
    function SkipCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean; virtual; abstract;
  end;

  TBaseCodeCompletionHandler = class
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function HandleCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes;
      out InsertText, DisplayText: string): Boolean; virtual; abstract;
    function GetInfo(CCItem: string): string; virtual; abstract;
  end;

  TCompletionInfo = record
    Id: Integer;  // Lsp Request Id
    Editor: TSynEdit;
    CaretXY: TBufferCoord;
    InsertText,
    DisplayText: string;
    CompletionHandler: TBaseCodeCompletionHandler;
  end;

  TCodeCompletion = class
  public
    SkipHandlers: TObjectList;
    CompletionHandlers: TObjectList;
    CompletionInfo: TCompletionInfo;
    Lock: TRTLCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure CleanUp;
    procedure RegisterSkipHandler(Handler: TBaseCodeCompletionSkipHandler);
    procedure RegisterCompletionHandler(Handler: TBaseCodeCompletionHandler);
  end;

  TBaseParamCompletion = class
  private type
    TCachedResults = record
      Editor: TSynEdit;
      StartX: Integer;
      Lookup, DisplayString, DocString: string;
    end;
  private
    FCachedResults: TCachedResults;
  protected
    FAllowFunctionCalls: Boolean;
    function CallTipFromExpression(const Expr, FileName: string;
      const Line: Integer; var DisplayString, DocString: string): Boolean; virtual; abstract;
  public
    function HandleParamCompletion(const FileName: string;
      Editor: TSynEdit; out DisplayString, DocString: string;
      out StartX: Integer): Boolean;
  end;

  TInterpreterParamCompletion = class(TBaseParamCompletion)
  protected
    function CallTipFromExpression(const Expr, FileName: string;
        const Line: Integer; var DisplayString, DocString: string): Boolean; override;
  public
    constructor Create;
  end;

  TIDECompletion = class
    class var EditorCodeCompletion: TCodeCompletion;
    class var InterpreterCodeCompletion: TCodeCompletion;
    class var InterpreterParamCompletion: TBaseParamCompletion;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.Threading,
  System.RegularExpressions,
  System.JSON,
  JvGnugettext,
  dmResources,
  SynHighlighterPython,
  StringResources,
  uEditAppIntfs,
  uCommonFunctions,
  cPyBaseDebugger,
  PythonEngine,
  cPyScripterSettings,
  cPySupportTypes,
  cPyControl,
  LspUtils,
  JediLspClient;

{ TRegExpressions }
type
TRegExpressions = class
  class var RE_Import: TRegEx;
  class var RE_From: TRegEx;
  class var RE_FromImport: TRegEx;
  class var RE_For: TRegEx;
  class var RE_ImportAs: TRegEx;
  class var RE_FromImportAs: TRegEx;
  class var RE_String: TRegEx;
  class constructor Create;
end;

class constructor TRegExpressions.Create;
begin
  RE_Import := CompiledRegEx(
    Format('^\s*import +(%s( +as +%s)? *, *)*(%s)?$',
    [DottedIdentRE, IdentRE, DottedIdentRE]));
  RE_From := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)?$', [DottedIdentRE]));
  RE_FromImport := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*(%s)?$',
      [DottedIdentRE, IdentRE, IdentRE, IdentRE]));
  RE_For := CompiledRegEx(
    Format('^\s*for +(%s *, *)*(%s)?$', [IdentRE, IdentRE]));
  RE_ImportAs := CompiledRegEx(
    Format('^\s*import +(%s( +as +%s)? *, *)*%s +as +(%s)?$',
    [DottedIdentRE, IdentRE, DottedIdentRE, IdentRE]));
  RE_FromImportAs := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*%s +as +(%s)?$',
      [DottedIdentRE, IdentRE, IdentRE, IdentRE, IdentRE]));
  RE_String := CompiledRegEx(
    Format('[''"]\.(%s)?$', [IdentRE]));
end;

{ TStringAndCommentSkipHandler }
type
  TStringAndCommentSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean; override;
  end;

function TStringAndCommentSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord;
  Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result :=  Assigned(HighlighterAttr) and
        ((HighlighterAttr = Highlighter.StringAttribute) or
        (HighlighterAttr = Highlighter.CommentAttribute) or
        ((Highlighter is TSynPythonSyn) and
        (HighlighterAttr = TSynPythonSyn(Highlighter).CodeCommentAttri) or
        (HighlighterAttr = TSynPythonSyn(Highlighter).MultiLineStringAttri) or
        (HighlighterAttr = TSynPythonSyn(Highlighter).DocStringAttri)));
end;

{ TForStatementSkipHandler }
type
  TForStatementSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean; override;
  end;

function TForStatementSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result := TRegExpressions.RE_For.IsMatch(Copy(Line, 1, Caret.Char - 1));
end;


{ TImportAsStatementSkipHandler }
type
  TImportAsStatementSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean; override;
  end;

function TImportAsStatementSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result := TRegExpressions.RE_ImportAs.IsMatch(Copy(Line, 1, Caret.Char - 1)) or
    TRegExpressions.RE_FromImportAs.IsMatch(Copy(Line, 1, Caret.Char - 1));
end;

{ TLiveNamespaceCompletionHandler }
type
  TLiveNamespaceCompletionHandler = class(TBaseCodeCompletionHandler)
  private
    FPyNameSpace: TBaseNameSpaceItem;
    FNameSpace: TStringList;
  public
    procedure Finalize; override;
    function HandleCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes;
      out InsertText, DisplayText: string): Boolean; override;
    function GetInfo(CCItem: string): string; override;
  end;

procedure TLiveNamespaceCompletionHandler.Finalize;
begin
  FreeAndNil(FNameSpace);
  if Assigned(FPyNameSpace) then
  begin
    var Py := SafePyEngine;
    FreeAndNil(FPyNameSpace);
  end;
end;

function TLiveNamespaceCompletionHandler.GetInfo(CCItem: string): string;
var
  Index: Integer;
  NameSpaceItem: TBaseNameSpaceItem;
begin
  Index := FNameSpace.IndexOf(CCItem);
  if Index >=0  then
  begin
    NameSpaceItem := FNameSpace.Objects[Index] as TBaseNameSpaceItem;
    if not Assigned(NameSpaceItem) then
      Result := _(SPythonKeyword)
    else
    begin
      var Py := SafePyEngine;
      Result := GetLineRange(NameSpaceItem.DocString, 1, 20);
    end;
  end;
end;

function TLiveNamespaceCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;

  function ToInsertItem(const Name: string; Item: TBaseNameSpaceItem): string;
  begin
    if Assigned(Item) and (Item.IsClass or Item.IsFunction or Item.IsMethod) then
      Result := Name + '()'
    else
      Result := Name;
  end;

var
  TmpX, Index, ImageIndex: Integer;
  Lookup: string;
  NameSpaceItem: TBaseNameSpaceItem;
begin
  // Clean-up of FNameSpace and FNameSpaceDict takes place in the Close event
  TmpX := Caret.Char;
  if TmpX > Length(Line) then
    TmpX := Length(Line)
  else Dec(TmpX);

  Lookup := GetWordAtPos(Line, TmpX, True, True, False, True);
  Index := Lookup.LastIndexOf('.');
  FPyNameSpace := nil;
  if Index >= 0 then begin
    Lookup := Lookup.Substring(0, Index);
    if Lookup.IndexOf(')') >= 0 then
      Lookup := ''  // Issue 422  Do not evaluate functions
    else if IsDigits(Lookup) then
      Lookup := '';  // Issue 478  User is typing a number
  end else
    Lookup := '';  // Completion from global namespace

  var Py := SafePyEngine;
  if (Index < 0) or (Lookup <> '') then begin
    if GI_PyControl.Inactive then
      FPyNameSpace := PyControl.ActiveInterpreter.NameSpaceFromExpression(Lookup)
    else if GI_PyControl.PythonLoaded and not GI_PyControl.Running then
      FPyNameSpace := PyControl.ActiveDebugger.NameSpaceFromExpression(Lookup);
  end;

  DisplayText := '';
  InsertText := '';

  FNameSpace := TStringList.Create;
  if Assigned(FPyNameSpace) then
  begin
    if Lookup <> '' then begin
      // FNameSpace corresponds to a Python object not a dict
      FPyNameSpace.ExpandCommonTypes := True;
      FPyNameSpace.ExpandSequences := False;
    end;
    for var I := 0 to FPyNameSpace.ChildCount - 1 do
    begin
      NameSpaceItem := FPyNameSpace.ChildNode[I];
      FNameSpace.AddObject(NameSpaceItem.Name, NameSpaceItem);
    end;
  end;
  if (Lookup = '') and PyIDEOptions.CompleteKeywords then begin
    // only add keywords to the completion of the global namespace
    for var I := 0 to (Highlighter as TSynPythonSyn).Keywords.Count - 1 do
    begin
      if TtkTokenKind(TSynPythonSyn(Highlighter).Keywords.Objects[I]) = tkKey then
        FNameSpace.Add(TSynPythonSyn(Highlighter).Keywords[I]);
    end;
  end;

  FNameSpace.CustomSort(ComparePythonIdents);

  for var I := 0 to FNameSpace.Count - 1 do
  begin
    NameSpaceItem := FNameSpace.Objects[I] as TBaseNameSpaceItem;
    InsertText := InsertText + ToInsertItem(FNameSpace[I], NameSpaceItem);

    if not Assigned(NameSpaceItem) then
       DisplayText := DisplayText + Format('\Image{%d}\hspace{8}\color{$FF8844}%s',
         [Integer(TCodeImages.Keyword), FNameSpace[I]])
    else
    begin
      if NameSpaceItem.IsModule then
        ImageIndex := Integer(TCodeImages.Module)
      else if NameSpaceItem.IsMethod
           {or NameSpaceItem.IsMethodDescriptor} then
        ImageIndex := Integer(TCodeImages.Method)
      else if NameSpaceItem.IsFunction
           {or NameSpaceItem.IsBuiltin} then
        ImageIndex := Integer(TCodeImages.Func)
      else if NameSpaceItem.IsClass then
        ImageIndex := Integer(TCodeImages.Klass)
      else begin
        if Index >= 0 then
          ImageIndex := Integer(TCodeImages.Field)
        else
          ImageIndex := Integer(TCodeImages.Variable);
      end;
      DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s', [ImageIndex, NameSpaceItem.Name]);
    end;
    if I < FNameSpace.Count - 1 then
    begin
      DisplayText := DisplayText + #10;
      InsertText := InsertText + #10;
    end;
  end;
  Result := InsertText <> '';
end;

{ TBaseLspCompletionHandler }
type
  TBaseLspCompletionHandler = class(TBaseCodeCompletionHandler)
  protected
    function GetWordStart(const Line: string;
      const Caret: TBufferCoord): Integer;
  public
    function GetInfo(CCItem: string): string; override;
  end;

function TBaseLspCompletionHandler.GetInfo(CCItem: string): string;
begin
  Result := TJedi.ResolveCompletionItem(CCItem);
end;

function TBaseLspCompletionHandler.GetWordStart(const Line: string;
  const Caret: TBufferCoord): Integer;
begin
  Result := Caret.Char;
  if Result > Length(Line) then
    Result := Length(Line) + 1;
  while (Result > 1) and ((Line[Result-1] = '_') or Line[Result-1].IsLetterOrDigit) do
    Dec(Result);
end;

{ TLspCompletionHandler }
type
  TLspCompletionHandler = class(TBaseLspCompletionHandler)
  public
    function HandleCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes;
      out InsertText, DisplayText: string): Boolean; override;
  end;

function TLspCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;

begin
  if not TJedi.Ready or (FileName = '') then Exit(False);

  // Get Completion for the current word
  var WordStart := GetWordStart(Line, Caret);

  Result := TJedi.HandleCodeCompletion(FileName, BufferCoord(WordStart, Caret.Line),
    InsertText, DisplayText);
end;

{ TBaseLspImportCompletionHandler }
type
  TBaseLspIICompletionHandler = class(TBaseLspCompletionHandler)
  private
    FDocContent: string;
  protected
    function CanHandle(const Line: string;
      const Caret: TBufferCoord): Boolean; virtual; abstract;
  public
    function HandleCodeCompletion(const Line, FileName: string; Caret: TBufferCoord;
      Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes;
      out InsertText, DisplayText: string): Boolean; override;
  end;

function TBaseLspIICompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord;
  Highlighter: TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes;
  out InsertText, DisplayText: string): Boolean;
var
  Param: TJSONObject;
const
  TempFileName = 'TempInterpreterFile';
begin
  if not (TJedi.Ready and CanHandle(Line, Caret)) then Exit(False);

  Param := TJSONObject.Create;
  Param.AddPair('textDocument', LspTextDocumentItem(TempFileName, 'python',
    FDocContent, 0));
  TJedi.LspClient.Notify('textDocument/didOpen', Param.ToJSON);
  Param.Free;

  Result := TJedi.HandleCodeCompletion(TempFileName, BufferCoord(FDocContent.Length + 1, 1),
    InsertText, DisplayText);

  Param := TJSONObject.Create;
  Param.AddPair('textDocument', LspTextDocumentIdentifier(TempFileName));
  TJedi.LspClient.Notify('textDocument/didClose', Param.ToJSON);
  Param.Free;
end;

{ TImportStatementHandler }
type
  TImportStatementHandler = class(TBaseLspIICompletionHandler)
  protected
    function CanHandle(const Line: string;
      const Caret: TBufferCoord): Boolean; override;
  end;

function TImportStatementHandler.CanHandle(const Line: string;
  const Caret: TBufferCoord): Boolean;
begin
  var LeftS := Copy(Line, 1, Caret.Char - 1);
  Result := TRegExpressions.RE_Import.IsMatch(LeftS) or
    TRegExpressions.RE_From.IsMatch(LeftS) or
    TRegExpressions.RE_FromImport.IsMatch(LeftS);
  if Result then
  begin
    var WordStart := GetWordStart(Line, Caret);
    FDocContent := TrimLeft(Copy(Line, 1, WordStart-1));
  end;
end;

{TStringCompletionHandler}
type
  TStringCompletionHandler = class(TBaseLspIICompletionHandler)
  protected
    function CanHandle(const Line: string;
      const Caret: TBufferCoord): Boolean; override;
  end;

function TStringCompletionHandler.CanHandle(const Line: string;
  const Caret: TBufferCoord): Boolean;
begin
  Result :=  TRegExpressions.RE_String.IsMatch(Copy(Line, 1, Caret.Char - 1));
  if Result then
    FDocContent := '"".';
end;

{ TCodeCompletion }

procedure TCodeCompletion.CleanUp;
begin
  Lock.Enter;
  try
    CompletionInfo.Id := -1;
    CompletionInfo.InsertText := '';
    CompletionInfo.DisplayText := '';
    CompletionInfo.Editor := nil;
    CompletionInfo.CaretXY := BufferCoord(0,9);
    if Assigned(CompletionInfo.CompletionHandler) then
    begin
      CompletionInfo.CompletionHandler.Finalize;
      CompletionInfo.CompletionHandler := nil;
    end;
  finally
    Lock.Leave;
  end;
end;

constructor TCodeCompletion.Create;
begin
  SkipHandlers := TObjectList.Create(True);
  CompletionHandlers := TObjectList.Create(True);
  Lock.Initialize;
end;

destructor TCodeCompletion.Destroy;
begin
  Lock.Enter;
  Lock.Leave;
  Lock.Free;
  SkipHandlers.Free;
  CompletionHandlers.Free;
  inherited;
end;

procedure TCodeCompletion.RegisterCompletionHandler(Handler: TBaseCodeCompletionHandler);
begin
  CompletionHandlers.Add(Handler);
end;

procedure TCodeCompletion.RegisterSkipHandler(Handler: TBaseCodeCompletionSkipHandler);
begin
  SkipHandlers.Add(Handler);
end;

{ TBaseCodeCompletionHandler }

procedure TBaseCodeCompletionHandler.Finalize;
begin
end;

procedure TBaseCodeCompletionHandler.Initialize;
begin
end;

{ TBaseParamCompletion }

function TBaseParamCompletion.HandleParamCompletion(const FileName: string;
  Editor: TSynEdit; out DisplayString, DocString: string;
  out StartX: Integer): Boolean;
const
  ParamCompletionWaitTime = 2000;  //ms
var
  LocLine, Lookup: string;
  TmpX, ParenCounter: Integer;
  FoundMatch: Boolean;
  DummyToken: string;
  Attr: TSynHighlighterAttributes;
  Caret: TBufferCoord;
  Highlighter: TSynPythonSyn;
begin
  DisplayString := '';
  DocString := '';
  Caret := Editor.CaretXY;
  Result := False;

  Highlighter := Editor.Highlighter as TSynPythonSyn;
  if Editor.GetHighlighterAttriAtRowCol(Caret, DummyToken, Attr) and
   ({(attr = Highlighter.StringAttribute) or} (Attr = Highlighter.CommentAttribute) or
    (Attr = Highlighter.CodeCommentAttri) or (Attr = Highlighter.MultiLineStringAttri) or
    (Attr = Highlighter.DocStringAttri))
   then
    // Do not code complete inside doc strings, code or multiline comments
    Exit;

  LocLine := Editor.LineText;

  //go back from the cursor and find the first open paren
  StartX := Caret.Char;
  TmpX := Caret.Char;
  if TmpX > Length(LocLine)
  then
    TmpX := Length(LocLine)
  else
    Dec(TmpX);

  FoundMatch := False;
  while (TmpX > 0) and not FoundMatch do
  begin
    Editor.GetHighlighterAttriAtRowCol(BufferCoord(TmpX, Caret.Line), DummyToken, Attr);
    if (Attr = Highlighter.StringAttri) or (Attr = Highlighter.SpaceAttri)
    then
      Dec(TmpX)
    else if LocLine[TmpX] = ')' then
    begin
      //We found a close, go till it's opening paren
      ParenCounter := 1;
      Dec(TmpX);
      while (TmpX > 0) and (ParenCounter > 0) do
      begin
        if LocLine[TmpX] = ')' then Inc(ParenCounter)
        else if LocLine[TmpX] = '(' then Dec(ParenCounter);
        Dec(TmpX);
      end;
    end else if LocLine[TmpX] = '(' then
    begin
      //we have a valid open paren, lets see what the word before it is
      StartX := TmpX;
      Dec(TmpX);
      while (TmpX > 0) and (LocLine[TmpX] = ' ') do
        Dec(TmpX);
      if TmpX > 0 then
      begin
        Lookup := GetWordAtPos(LocLine, TmpX, True, True, False, True);

        if (Lookup <> '') and (Lookup[1] = '.') and
           (TmpX > Length(Lookup)) and
           CharInSet(LocLine[TmpX - Length(Lookup)], ['''', '"'])
        then
          Lookup := 'str' + Lookup;

        FoundMatch :=
          (FAllowFunctionCalls or (Lookup.IndexOf(')') < 0)) // Issue 422  Do not evaluate functions
          and (Lookup <> '');

        if FoundMatch then
        begin
          // Use CachedResults
          if (Editor = FCachedResults.Editor) and (Lookup = FCachedResults.Lookup)
            and (StartX = FCachedResults.StartX)  then
          begin
            DisplayString := FCachedResults.DisplayString;
            DocString := FCachedResults.DocString;
          end
          else
          begin
            var DispS, DocS: string;
            var Task := TTask.Create(procedure
            begin
              FoundMatch := CallTipFromExpression(Lookup, FileName, Caret.Line, DispS, DocS);
            end).Start;
            if Task.Wait(ParamCompletionWaitTime) then
            begin
              if FoundMatch then
              begin
                DisplayString := DispS;
                DocString := DocS;
                // cache the results
                FCachedResults.Editor := Editor;
                FCachedResults.StartX := StartX;
                FCachedResults.Lookup := Lookup;
                FCachedResults.DisplayString := DisplayString;
                FCachedResults.DocString := DocString;
              end;
            end
            else
              Exit; // We got stuck!
          end;
        end;

        if not FoundMatch then
        begin
          TmpX := StartX;
          Dec(TmpX);
        end;
      end;
    end
    else
      Dec(TmpX);
  end;
  Result := FoundMatch;
end;

{ TInterpreterParamCompletion }

function TInterpreterParamCompletion.CallTipFromExpression(const Expr, FileName: string;
  const Line: Integer; var DisplayString, DocString: string): Boolean;
begin
  Result := PyControl.ActiveInterpreter.CallTipFromExpression(
    Expr, DisplayString, DocString);
end;

constructor TInterpreterParamCompletion.Create;
begin
  inherited;
  FAllowFunctionCalls := True;
end;

{ TIDECompletion }

class constructor TIDECompletion.Create;
begin
  InterpreterParamCompletion := TInterpreterParamCompletion.Create;

  EditorCodeCompletion := TCodeCompletion.Create;
  InterpreterCodeCompletion := TCodeCompletion.Create;

  //  Register handlers - Order may be important
  EditorCodeCompletion.RegisterSkipHandler(TStringAndCommentSkipHandler.Create);
  EditorCodeCompletion.RegisterSkipHandler(TForStatementSkipHandler.Create);
  EditorCodeCompletion.RegisterSkipHandler(TImportAsStatementSkipHandler.Create);

  EditorCodeCompletion.RegisterCompletionHandler(TLspCompletionHandler.Create);

  InterpreterCodeCompletion.RegisterSkipHandler(TStringAndCommentSkipHandler.Create);
  InterpreterCodeCompletion.RegisterSkipHandler(TForStatementSkipHandler.Create);
  InterpreterCodeCompletion.RegisterSkipHandler(TImportAsStatementSkipHandler.Create);

  InterpreterCodeCompletion.RegisterCompletionHandler(TImportStatementHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TStringCompletionHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TLiveNamespaceCompletionHandler.Create);
end;

class destructor TIDECompletion.Destroy;
begin
  EditorCodeCompletion.Free;
  InterpreterCodeCompletion.Free;
  InterpreterParamCompletion.Free;
end;

end.
