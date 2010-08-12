{-----------------------------------------------------------------------------
 Unit Name: frmToDo
 Author:    Kiriakos Vlahos
 Date:      19-May-2005
 Purpose:   Todo list Dock Form based on GExperts unit and covered by its license

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement only covers code written by GExperts, Inc and Erik Berry. You should contact the other authors concerning their respective copyrights and conditions.

The rules governing the use of GExperts and the GExperts source code are derived
from the official Open Source Definition, available at http://www.opensource.org.
The conditions and limitations are as follows:

    * Usage of GExperts binary distributions is permitted for all developers.
      You may not use the GExperts source code to develop proprietary or
      commercial products including plugins or libraries for those products.
      You may use the GExperts source code in an Open Source project, under the
      terms listed below.
    * You may not use the GExperts source code to create and distribute custom
      versions of GExperts under the "GExperts" name. If you do modify and
      distribute custom versions of GExperts, the binary distribution must be
      named differently and clearly marked so users can tell they are not using
      the official GExperts distribution. A visible and unmodified version of
      this license must appear in any modified distribution of GExperts.
    * Custom distributions of GExperts must include all of the custom changes
      as a patch file that can be applied to the original source code. This
      restriction is in place to protect the integrity of the original author's
      source code. No support for modified versions of GExperts will be provided
      by the original authors or on the GExperts mailing lists.
    * All works derived from GExperts must be distributed under a license
      compatible with this license and the official Open Source Definition,
      which can be obtained from http://www.opensource.org/.
    * Please note that GExperts, Inc. and the other contributing authors hereby
      state that this package is provided "as is" and without any express or
      implied warranties, including, but not without limitation, the implied
      warranties of merchantability and fitness for a particular purpose. In
      other words, we accept no liability for any damage that may result from
      using GExperts or programs that use the GExperts source code.

-----------------------------------------------------------------------------}
// TODO: consider showing the owner and the category

unit frmToDo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmIDEDockWin, JvDockControlForm, ExtCtrls, ActnList,
  Contnrs, ImgList, ComCtrls, Menus, JvAppStorage, TB2Item,
  TB2Dock, TB2Toolbar, VirtualTrees, JvComponentBase, SpTBXItem,
  SynUnicode, MPCommonObjects, TntActnList, SpTBXSkins;

type
  TToDoPriority = (tpHigh, tpMed, tpLow, tpDone);
  TToDoScanType = (tstOpenFiles, tstProjectFiles, tstDirectory);

  TTokenInfo = class(TObject)
  private
    FToken: WideString;
    FPriority: TToDoPriority;
  public
    property Token: WideString read FToken write FToken;
    property Priority: TToDoPriority read FPriority write FPriority;
  end;

  TToDoInfo = class(TObject)
  private
    NumericPriority: Cardinal;
    Owner: WideString;
    ToDoClass: WideString;
    //
    Priority: TToDoPriority;
    Raw: WideString;
    Display: WideString;
    FileName: WideString;
    LineNo: Integer;
  end;

  TTokenList = class(TUnicodeStringList)
  private
    procedure AddToken(const Token: WideString; Priority: TToDoPriority);
  public
    destructor Destroy; override;
  end;

  TToDoWindow = class(TIDEDockWindow)
    ilTodo: TImageList;
    TBXDock1: TSpTBXDock;
    Toolbar: TSpTBXToolbar;
    tbiGoTo: TSpTBXItem;
    tbiRefresh: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiPrint: TSpTBXItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    tbiOptions: TSpTBXItem;
    PopupMenu: TSpTBXPopupMenu;
    mnGoto: TSpTBXItem;
    mnRefresh: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    mnCopyAll: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    mnPrint: TSpTBXItem;
    mnOptions: TSpTBXItem;
    ToDoView: TVirtualStringTree;
    TBXSeparatorItem5: TSpTBXSeparatorItem;
    tbiHelp: TSpTBXItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    mnHelp: TSpTBXItem;
    tbiAbort: TSpTBXItem;
    Actions: TTntActionList;
    actFileAbort: TTntAction;
    actEditCopy: TTntAction;
    actHelpHelp: TTntAction;
    actOptionsConfigure: TTntAction;
    actFilePrint: TTntAction;
    actEditGoto: TTntAction;
    actFileRefresh: TTntAction;
    procedure actEditCopyExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFilePrintUpdate(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actEditGotoExecute(Sender: TObject);
    procedure actOptionsConfigureExecute(Sender: TObject);
    procedure TodoViewKeyPress(Sender: TObject; var Key: Char);
    procedure ToDoViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ToDoViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ToDoViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ToDoViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ToDoViewCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure ToDoViewShortenString(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const S: WideString; TextSpace: Integer; var Result: WideString;
      var Done: Boolean);
    procedure ToDoViewHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  private
    { Private declarations }
    FIsFirstActivation: Boolean;
    FDataList: TObjectList;
    FAbortSignalled: Boolean;
    function GetSelectedItem: TToDoInfo;
    procedure ClearDataListAndListView;
    procedure EnumerateFilesByDirectory;
    procedure EnumerateOpenFiles;
    procedure EnumerateProjectFiles;
    procedure LoadFile(const FileName: WideString);
    function ParseComment(const FileName, SComment, EComment,
     TokenString: WideString; LineNumber: Integer): TToDoInfo;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure VirtualFileSearchEnd(Sender: TObject; Results: TCommonPIDLList);
  public
    { Public declarations }
    procedure RefreshTodoList;
    property AbortSignalled: Boolean read FAbortSignalled write FAbortSignalled;
  end;

  TToDoExpert = class(TInterfacedPersistent, IJvAppStorageHandler)
  private
    FScanType: TToDoScanType;
    FDirsToScan: WideString;
    FRecurseDirScan: Boolean;
    FTokenList: TTokenList;
    FShowTokens: Boolean;
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure;
  end;

var
  ToDoWindow: TToDoWindow;
  ToDoExpert: TToDoExpert;

implementation

uses dmCommands, uEditAppIntfs, Math, frmPyIDEMain, dlgToDoOptions,
  uCommonFunctions, JvJVCLUtils, cProjectClasses, WideStrUtils,
  VirtualFileSearch, MPCommonUtilities, cParameters;

{$R *.dfm}

Type

  PToDoRec = ^TToDoRec;
  TToDoRec = record
    ToDoInfo : TToDoInfo;
  end;

function WideCaseInsensitivePos(const SubString, S: WideString): Integer;
begin
  Result := Pos(WideUpperCase(SubString), WideUpperCase(S));
end;

procedure TToDoWindow.actEditCopyExecute(Sender: TObject);
var
  ClipText: TUnicodeStrings;
  i: Integer;
begin
  inherited;

  ClipText := TUnicodeStringList.Create;
  try
    for i := 0 to fDataList.Count - 1 do
    with TToDoInfo(fDataList[i]) do begin
      ClipText.Add(IntToStr(Ord(Priority)) + #9 +
        Display + #9 +
        ExtractFileName(FileName) + #9 +
        IntToStr(LineNo));
    end;
    SetClipboardWideText(ClipText.Text);
  finally
    FreeAndNil(ClipText);
  end;
end;

procedure TToDoWindow.actOptionsConfigureExecute(Sender: TObject);
begin
  ToDoExpert.Configure;
end;

function TToDoWindow.GetSelectedItem: TToDoInfo;
Var
  Node : PVirtualNode;
begin
  Result := nil;
  Node := ToDoView.GetFirstSelected();
  if Assigned(Node) then
    Result := PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo;
end;

procedure TToDoWindow.actFileRefreshExecute(Sender: TObject);
begin
  RefreshTodoList;
end;

procedure TToDoWindow.actEditGotoExecute(Sender: TObject);
Var
  SelectedItem: TToDoInfo;
begin
  SelectedItem := GetSelectedItem;
  if SelectedItem = nil then Exit;

  PyIDEMainForm.ShowFilePosition(SelectedItem.FileName, SelectedItem.LineNo+1, 1);
end;

resourcestring
  SHigh = 'High';
  SNormal = 'Normal';
  SLow = 'Low';
  SDone = 'Done';

var
  PriorityText: array[Low(TToDoPriority)..High(TToDoPriority)] of string =
    (SHigh, SNormal, SLow, SDone);

procedure TToDoWindow.actFileAbortExecute(Sender: TObject);
begin
  AbortSignalled := True;
  actFileAbort.Enabled := False;
end;

procedure TToDoWindow.actFilePrintExecute(Sender: TObject);
resourcestring
  STodoItems = 'To Do Items';
var
  RichEdit: TRichEdit;
  i: Integer;
begin
  if fDataList.Count = 0 then
    Exit;

  RichEdit := TRichEdit.Create(Self);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Self;
    RichEdit.Clear;
    RichEdit.Lines.Add('');
    with RichEdit.SelAttributes do
    begin
      Style := [fsBold];
      Size := 14;
    end;

    with RichEdit.SelAttributes do begin
      Style := [fsBold];
      RichEdit.Lines.Add(STodoItems);
      Style := [];
    end;
    for i := 0 to FDataList.Count - 1 do
    begin
      with TToDoInfo(FDataList[i]) do
      begin
        with RichEdit.SelAttributes do
        begin
          case Priority of
            tpHigh   : Style := [fsBold];
            tpMed : Style := [];
            tpLow    : Style := [fsItalic];
          end;
          Size := 10;
        end;
        RichEdit.Lines.Add(ExtractFileName(FileName) + ' (' + IntToStr(LineNo) + ')' +
          #09 + PriorityText[Priority] + #9 + Display);
      end;
    end;
    RichEdit.Print(SToDoItems);
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure TToDoWindow.actFilePrintUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDataList.Count > 0);
end;

procedure TToDoWindow.FormCreate(Sender: TObject);
begin
  inherited;
  FIsFirstActivation := True;

  FDataList := TObjectList.Create(True); // Owned objects

  // Let the tree know how much data space we need.
  ToDoView.NodeDataSize := SizeOf(TToDoRec);
  ToDoView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  ToDoView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
  ToDoView.OnBeforeCellPaint :=
    CommandsDataModule.VirtualStringTreeBeforeCellPaint;
  ToDoView.OnPaintText :=
    CommandsDataModule.VirtualStringTreePaintText;
end;

procedure TToDoWindow.FormDestroy(Sender: TObject);
begin
  ClearDataListAndListView;

  FreeAndNil(FDataList);

  inherited;
end;

procedure TToDoWindow.ClearDataListAndListView;
begin
  ToDoView.Clear;
  FDataList.Clear;
end;

{ TTokenList }

procedure TTokenList.AddToken(const Token: WideString;
  Priority: TToDoPriority);
Var
  TokenInfo: TTokenInfo;
begin
  TokenInfo := TTokenInfo.Create;
  TokenInfo.Token := WideUpperCase(Token);
  TokenInfo.Priority := Priority;

  AddObject(Token, TokenInfo);
end;

destructor TTokenList.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Objects[i].Free;
  inherited;
end;

{ TToDoExpert }

procedure TToDoExpert.Configure;
var
  Dlg: TfmToDoOptions;
  TodoListOptionsUnchanged: Boolean;
  OldScanType: TToDoScanType;
begin
  TodoListOptionsUnchanged := True;

  Dlg := TfmToDoOptions.Create(nil);
  try
    Dlg.lstTokens.Items.Assign(FTokenList);
    Dlg.cbShowTokens.Checked := FShowTokens;
    case FScanType of
      tstOpenFiles:
        Dlg.radScanOpen.Checked := True;
      tstProjectFiles:
        Dlg.radScanProject.Checked := True;
      tstDirectory:
        Dlg.radScanDir.Checked := True;
    end;
    Dlg.chkInclude.Checked := FRecurseDirScan;
    Dlg.meDirectories.Text := FDirsToScan;

    if Dlg.ShowModal = mrOk then
    begin
      TodoListOptionsUnchanged := TodoListOptionsUnchanged and
        FTokenList.Equals(Dlg.lstTokens.Items);
      FTokenList.Assign(Dlg.lstTokens.Items);

      TodoListOptionsUnchanged := TodoListOptionsUnchanged and
        (FShowTokens = Dlg.cbShowTokens.Checked);
      FShowTokens := Dlg.cbShowTokens.Checked;

      OldScanType := FScanType;

      if Dlg.radScanOpen.Checked then
        FScanType := tstOpenFiles
      else if Dlg.radScanProject.Checked then
        FScanType := tstProjectFiles
      else if Dlg.radScanDir.Checked then
        FScanType := tstDirectory;

      TodoListOptionsUnchanged := TodoListOptionsUnchanged and
        (FScanType = OldScanType);

      TodoListOptionsUnchanged := TodoListOptionsUnchanged and
        (FRecurseDirScan = Dlg.chkInclude.Checked);
      FRecurseDirScan := Dlg.chkInclude.Checked;

      TodoListOptionsUnchanged := TodoListOptionsUnchanged and
        (AnsiCompareText(FDirsToScan, Dlg.meDirectories.Text) = 0);
      FDirsToScan := Dlg.meDirectories.Text;
    end;
  finally
    FreeAndNil(Dlg);
  end;

  if not TodoListOptionsUnchanged then
  begin
    if Assigned(ToDoWindow) then
      ToDoWindow.RefreshTodoList;
  end;
end;

constructor TToDoExpert.Create;
begin
  inherited;
  FTokenList := TTokenList.Create;
  FScanType := tstOpenFiles;
  // Initialize FTokenList
  with FTokenList do begin
    AddToken('TODO', tpMed);
    AddToken('FIXME', tpMed);
    AddToken('XXX', tpMed);
    AddToken('ToDo1', tpHigh);
    AddToken('ToDo2', tpMed);
    AddToken('ToDo3', tpLow);
  end;
end;

destructor TToDoExpert.Destroy;
begin
  FreeAndNil(FTokenList);

  ToDoExpert := nil;
  inherited;
end;

procedure TToDoExpert.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  i : integer;
  NTokens : integer;
  Priority : TToDoPriority;
  Token : WideString;
  SL : TUnicodeStringList;
begin
  with AppStorage do begin
    FShowTokens := ReadBoolean(BasePath+'\ShowTokens');
    ReadEnumeration(BasePath+'\ScanType', TypeInfo(TToDoScanType), FScanType, FScanType);
    SL := TUnicodeStringList.Create;
    try
      ReadWideStringList(BasePath+'\DirsToScan', SL);
      FDirsToScan := SL.Text;
    finally
      SL.Free;
    end;
    FRecurseDirScan := ReadBoolean(BasePath+'\RecurseDirScan');
    NTokens := ReadInteger(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Count']));
    if NTokens > 0 then begin
      FTokenList.Free;
      FTokenList := TTokenList.Create;
      for i := 0 to NTokens - 1 do begin
        Token := ReadWideString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Token']));
        Priority := tpMed;
        ReadEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Priority']),
          TypeInfo(TToDoPriority), Priority, Priority);
        FTokenList.AddToken(Token, Priority);
      end;
    end;
    ToDoWindow.ToDoView.Header.Columns[2].Width := AppStorage.ReadInteger(BasePath+'\FileName Width', 150);
    ToDoWindow.ToDoView.Header.Columns[3].Width := AppStorage.ReadInteger(BasePath+'\Line Width', 50);
  end;
end;

procedure TToDoExpert.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  i : integer;
  SL : TUnicodeStringList;
begin
  with AppStorage do begin
    DeleteSubTree(BasePath);
    WriteBoolean(BasePath+'\ShowTokens', FShowTokens);
    WriteEnumeration(BasePath+'\ScanType', TypeInfo(TToDoScanType), FScanType);
    SL := TUnicodeStringList.Create;
    try
      SL.Text := FDirsToScan;
      WriteWideStringList(BasePath+'\DirsToScan', SL);
    finally
      SL.Free;
    end;
    WriteBoolean(BasePath+'\RecurseDirScan', FRecurseDirScan);

    WriteInteger(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Count']), FTokenList.Count);
    for i := 0 to FTokenList.Count - 1 do begin
      WriteWideString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Token']),
        FTokenList[i]);
      WriteEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Priority']),
        TypeInfo(TToDoPriority), TTokenInfo(FTokenList.Objects[i]).Priority);
    end;
    AppStorage.WriteInteger(BasePath+'\FileName Width', TodoWindow.TodoView.Header.Columns[2].Width);
    AppStorage.WriteInteger(BasePath+'\Line Width', TodoWindow.TodoView.Header.Columns[3].Width);
  end;
end;

resourcestring
  SDoneTodoDesignation = 'Done';

function TToDoWindow.ParseComment(const FileName, SComment, EComment,
  TokenString: WideString; LineNumber: Integer) : TToDoInfo;
var
  i, j, k, n, m, TokenPos, NextCharPos: Integer;
  IsDoneTodoItem: Boolean;
  ParsingString: WideString;
  OptionChar: WideChar;
  TokenStringUpped : WideString;
begin
  // Token string is alread trimmed and without SComment
  Result := nil;
  TokenStringUpped := WideUpperCase(TokenString);
  for i := 0 to ToDoExpert.FTokenList.Count - 1 do
  begin
    if WideStrIsLeft(PWideChar(TokenStringUpped),
      PWideChar(TTokenInfo(ToDoExpert.FTokenList.Objects[i]).Token)) then
    begin
      // We found a token that looks like a TODO comment and is in the position 1.
      n := 1;
      // Remove comment characters
      ParsingString := TokenString;
      TokenPos := 1;

      // The TODO token should be followed by a non-alphanumeric character
      NextCharPos := TokenPos + Length(ToDoExpert.FTokenList[i]);
      if (NextCharPos <= Length(ParsingString)) and IsCharAlphaNumericW(ParsingString[NextCharPos]) then
        Continue;

      // Token found in comment line
      Result := TToDoInfo.Create;

      // Remove token from string
      Delete(ParsingString, 1, Length(ToDoExpert.FTokenList[i]));
      ParsingString := TrimRight(ParsingString);

      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(ParsingString) do
      begin
        if not (ParsingString[j + 1] in [WideChar('0')..WideChar('9')]) then
          Break;
        Inc(j);
      end;
      Result.NumericPriority := StrToIntDef(Copy(ParsingString, 1, j), 0);
      Delete(ParsingString, 1, j);
      ParsingString := TrimLeft(ParsingString);

      IsDoneTodoItem := (WideCaseInsensitivePos(SDoneTodoDesignation, ParsingString) = 1);

      { zTODO -oTestCase: -cIssue <-- test case for colon }
      // Delete everything being with a possible trailing colon:
      j := Pos(WideChar(':'), ParsingString);
      if j > 0 then
        Delete(ParsingString, j, Length(ParsingString))
      else
        ParsingString := '';

      { zTODO -cSomething -oTestCase: <-- test case for -o switch }
      { zTODO blah -cSomething -oTestCase: <-- this is a deliberately broken item }
      { zTODO -oTestCase -cTest -c switch: <-- test case for -c switch }
      { zTODO -oTestCase -cTest -c switch: <-- another test case for -c switch }
      { zTODO -oTestCase -cTest -zWhoops -c switch: <-- -z switch }
      { zTODO -oTestCase -cTest -z-z-z-z -------------- -c switch: <-- try to break it }
      { zTODO -oTestCase -cTe-st : <-- hyphen test }
      //zTOdo
      { zTODO -oTestCase
          -cMultiline
          <-- Multiline test }
      // Identify owner of TODO item (-o)
      // Identify class of TODO item (-c)
      OptionChar := WideChar(#0); // Initialize to make compiler happy - redundant
      while Pos(WideChar('-'), ParsingString) > 0 do
      begin
        if Length(ParsingString) > 1 then
        begin
          OptionChar := WideUpperCase(Copy(ParsingString, 2, 1))[1];
          Delete(ParsingString, 1, 2);
        end
        else
          Break;

        // Find char immediately preceding the next option switch
        j := Pos('-', ParsingString) - 1;
        if j > 0 then
          k := j
        else
          k := Length(ParsingString);

        case OptionChar of
          'O': Result.Owner := Trim(Copy(ParsingString, 1, k));
          'C': Result.ToDoClass := Trim(Copy(ParsingString, 1, k));
        end;

        // Delete everything up to, but not including, the
        // next option switch
        Delete(ParsingString, 1, j);
      end;

      Result.Raw := TokenString;

      if IsDoneTodoItem then
        Result.Priority := tpDone
      else
        Result.Priority := TTokenInfo(ToDoExpert.FTokenList.Objects[i]).Priority;

      if not ToDoExpert.FShowTokens then begin
        n := n + Length(ToDoExpert.FTokenList[i]);
        // Do not show anything up to ':'
        n := Max(n, Pos(':', TokenString)+1);
      end;
      if EComment <> '' then // Trim end-comment token.
        m := WideCaseInsensitivePos(EComment, TokenString) - 1
      else
        m := Length(TokenString);
      if m < 1 then
        m := Length(TokenString);
      // Prevent multi-line to do notes
      j := Pos(WideChar(#13), TokenString);
      k := Pos(WideChar(#10), TokenString);
      if j = 0 then j := 999999;
      if k = 0 then k := 999999;
      m := Min(m, Min(j, k));
      // The +1 is necessary to match IDE's line numbering
      Result.Display := Trim(Copy(TokenString, n, (m - n) + 1));
      Result.LineNo := LineNumber;
      Result.FileName := FileName;
      FDataList.Add(Result);

      Break; // Comment line parsed, stop searching for more To Do items in that line
    end;
  end;
end;

procedure TToDoWindow.LoadFile(const FileName: WideString);
Var
  SourceCode : TUnicodeStringList;
  Editor : IEditor;
  i, Index : integer;
  TokenString : WideString;
  Encoding : TFileSaveFormat;
  Info, OldInfo : TToDoInfo;
  PStart : PWideChar;
begin
  SourceCode := TUnicodeStringList.Create;
  try
    //  Read file into SourceCode
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    if Assigned(Editor) then
      SourceCode.Assign(Editor.SynEdit.Lines)
    else begin
      if not LoadFileIntoWideStrings(FileName, SourceCode, Encoding) then Exit;
    end;

    // scan source code
    i := 0;
    OldInfo := nil;
    while i < SourceCode.Count do begin
      TokenString := SourceCode[i];
      PStart := WStrScan(PWideChar(TokenString), '#');
      if Assigned(PStart) then begin
        if Assigned(OldInfo) then begin
          if TrimLeft(TokenString)[1] <> WideChar('#') then
            OldInfo := nil;  // it is not a multiline comments
        end;

        Index := PStart - PWideChar(TokenString) + 1;
        TokenString := Copy(TokenString, Index + 1, MaxInt);  // We delete the #
        TokenString := Trim(TokenString);

        Info := ParseComment(FileName, '#', '', Tokenstring, i);
        if Assigned(Info) then begin
          OldInfo := Info;
        end else if Assigned(OldInfo) then
          // Append new comment to Info.Display
          OldInfo.Display := OldInfo.Display + ' ' + TokenString;
      end else
        OldInfo := nil;
      Inc(i);
    end;
  finally
    SourceCode.Free;
  end;
end;

procedure TToDoWindow.RefreshTodoList;
var
  Cursor: IInterface;
begin
  Application.ProcessMessages;  // to repaint etc.
  Cursor := WaitCursor;
  FAbortSignalled := False;
  actFileAbort.Enabled := True;
  try
    ClearDataListAndListView;

    case ToDoExpert.FScanType of
      tstOpenFiles:
        EnumerateOpenFiles;
      tstProjectFiles:
        EnumerateProjectFiles;
      tstDirectory:
        begin
          if Trim(ToDoExpert.FDirsToScan) <> '' then
            EnumerateFilesByDirectory;
        end;
    end;
    ToDoView.RootNodeCount := FDataList.Count;

  finally
    if FDataList.Count > 0 then
    begin
      if Assigned(ToDoView.RootNode.FirstChild) then
        ToDoView.Selected[ToDoView.RootNode.FirstChild] := True;
      actEditGoto.Enabled := True;
    end
    else
      actEditGoto.Enabled := False;
    fAbortSignalled := True;
    actFileAbort.Enabled := False;
  end;
end;

procedure TToDoWindow.VirtualFileSearchEnd(Sender: TObject;
  Results: TCommonPIDLList);
Var
  i : integer;
begin
  for i := 0 to Results.Count - 1 do begin
    if FAbortSignalled then
        Exit;
    LoadFile(PIDLToPath(Results[i]));
    Application.ProcessMessages;
  end;
  Results.Clear;
end;

procedure TToDoWindow.EnumerateFilesByDirectory;
var
  FileSearch : TVirtualFileSearch;
begin
  FileSearch := TVirtualFileSearch.Create(nil);
  FileSearch.SearchAttribs := FileSearch.SearchAttribs- [vsaSystem, vsaHidden];
  FileSearch.SearchExcludeAttribs := [vsaSystem, vsaHidden];
  try
    FileSearch.SearchPaths.StrictDelimiter := True;
    FileSearch.SearchPaths.Delimiter := ';';
    FileSearch.SearchPaths.DelimitedText := ToDoExpert.FDirsToScan;

    FileSearch.SearchCriteriaFilename.StrictDelimiter := True;
    FileSearch.SearchCriteriaFilename.Delimiter := ';';
    FileSearch.SearchCriteriaFilename.DelimitedText :=
      CommandsDataModule.PyIDEOptions.PythonFileExtensions;

    FileSearch.SubFolders := ToDoExpert.FRecurseDirScan;

    FileSearch.UpdateRate := 200;

    FileSearch.OnSearchEnd := VirtualFileSearchEnd;

    FileSearch.Run;
    while not FileSearch.Finished do begin
      if FAbortSignalled then begin
        FileSearch.Stop;
        Exit;
      end else begin
        Application.ProcessMessages;
        Sleep(20);
      end;
    end;
  finally
    FileSearch.Free;
  end;
end;

procedure TToDoWindow.EnumerateOpenFiles;
var
  i : integer;
  FileName : WideString;
  Editor : IEditor;
begin
  for i := 0 to GI_EditorFactory.Count - 1 do begin
    Editor := GI_EditorFactory.Editor[i];
    FileName := Editor.GetFileNameOrTitle;
    if FAbortSignalled then
      Exit
    else
      Application.ProcessMessages;
    LoadFile(FileName);
  end;
end;

function ProcessProjectFile(Node: TAbstractProjectNode; Data : Pointer):boolean;
var
  FileName : WideString;
begin
   if TToDoWindow(Data).FAbortSignalled then begin
     Result := True;
     Exit;
   end else begin
     Application.ProcessMessages;
     Result := False;
   end;
   if (Node is TProjectFileNode) and (TProjectFileNode(Node).FileName <> '') then begin
     FileName := Parameters.ReplaceInText(TProjectFileNode(Node).FileName);
     if CommandsDataModule.FileIsPythonSource(FileName)
     then
       TToDoWindow(Data).LoadFile(FileName);
   end;
end;

procedure TToDoWindow.EnumerateProjectFiles;
begin
  ActiveProject.FirstThat(ProcessProjectFile, Self);
end;

procedure TToDoWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if FIsFirstActivation then
  begin
    FIsFirstActivation := False;
    RefreshTodoList;
  end;
  if CanActuallyFocus(ToDoView) then
    ToDoView.SetFocus;
  //PostMessage(ToDoView.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TToDoWindow.TodoViewKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then begin
    actEditGotoExecute(Self);
    Key := #0;
  end;
end;

procedure TToDoWindow.ToDoViewShortenString(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const S: WideString; TextSpace: Integer; var Result: WideString;
  var Done: Boolean);
begin
  if Column = 2 then begin
    Result := ShortenStringEx(TargetCanvas.Handle, S, TextSpace,
      Application.UseRightToLeftReading, sseFilePathMiddle);
    Done := True
  end;
end;

procedure TToDoWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  ToDoView.Header.Invalidate(nil, True);
  ToDoView.Invalidate;
  if SkinManager.IsDefaultSkin then
    ToDoView.TreeOptions.PaintOptions := ToDoView.TreeOptions.PaintOptions - [toAlwaysHideSelection]
  else
    ToDoView.TreeOptions.PaintOptions := ToDoView.TreeOptions.PaintOptions + [toAlwaysHideSelection];
end;

procedure TToDoWindow.ToDoViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(ToDoView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fDataList.Count);
  PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo :=
    fDataList[Node.Index] as TToDoInfo;
end;

procedure TToDoWindow.ToDoViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  Assert(ToDoView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fDataList.Count);
  with PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo do
    case Column of
      1:  CellText := Display;
      2:  CellText := FileName;
      3:  CellText := IntToStr(LineNo);
    end;
end;

procedure TToDoWindow.ToDoViewHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if ToDoView.Header.SortColumn = HitInfo.Column then begin
    if ToDoView.Header.SortDirection = sdAscending then
      ToDoView.Header.SortDirection := sdDescending
    else
      ToDoView.Header.SortDirection := sdAscending;
  end else
    ToDoView.Header.SortColumn := HitInfo.Column;
  if ToDoView.Header.SortColumn = 0 then
    ToDoView.Header.Columns[0].Alignment := taLeftJustify  // to make it fit
  else
    ToDoView.Header.Columns[0].Alignment := taCenter;
end;

procedure TToDoWindow.ToDoViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if (Kind in [ikNormal, ikSelected]) and (Column = 0) then begin
    Assert(ToDoView.GetNodeLevel(Node) = 0);
    Assert(Integer(Node.Index) < fDataList.Count);
    with PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo do
      ImageIndex := Ord(Priority);
  end;
end;

procedure TToDoWindow.ToDoViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  actEditGoto.Enabled := Assigned(Node);
end;

procedure TToDoWindow.ToDoViewCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
Var
  ToDoInfo1, ToDoInfo2 : TTodoInfo;
begin
  Assert(Assigned(Node1) and Assigned(Node2));
  ToDoInfo1 := PToDoRec(TodoView.GetNodeData(Node1))^.ToDoInfo;
  ToDoInfo2 := PToDoRec(TodoView.GetNodeData(Node2))^.ToDoInfo;
  case Column of
    -1: Result := 0;
    0: Result := Ord(ToDoInfo1.Priority) - Ord(ToDoInfo2.Priority);
    1: Result := WideCompareStr(ToDoInfo1.Display, ToDoInfo2.Display);
    2: Result := AnsiCompareStr(ExtractFileName(ToDoInfo1.FileName),
                    ExtractFileName(ToDoInfo2.FileName));
    3: Result := ToDoInfo1.LineNo - ToDoInfo2.LineNo;
  end;
end;

procedure TToDoWindow.actHelpHelpExecute(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

initialization
  ToDoExpert := TToDoExpert.Create;
finalization
  ToDoExpert.Free;
end.





