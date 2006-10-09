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
  Dialogs, frmIDEDockWin, JvComponent, JvDockControlForm, ExtCtrls, ActnList,
  Contnrs, ImgList, ComCtrls, Menus, JvAppStorage, JvSearchFiles, TB2Item,
  TBX, TBXThemes, TB2Dock, TB2Toolbar, VirtualTrees, JvComponentBase;

type
  TToDoPriority = (tpHigh, tpMed, tpLow, tpDone);
  TToDoScanType = (tstOpenFiles, tstDirectory);

  TTokenInfo = class(TObject)
  private
    FToken: string;
    FPriority: TToDoPriority;
  public                                       
    property Token: string read FToken write FToken;
    property Priority: TToDoPriority read FPriority write FPriority;
  end;

  TToDoInfo = class(TObject)
  private
    NumericPriority: Cardinal;
    Owner: string;
    ToDoClass: string;
    //
    Priority: TToDoPriority;
    Raw: string;
    Display: string;
    FileName: string;
    LineNo: Integer;
  end;

  TTokenList = class(TStringList)
  private
    procedure AddToken(const Token: string; Priority: TToDoPriority);
  public
    destructor Destroy; override;
  end;

  TToDoWindow = class(TIDEDockWindow)
    ilTodo: TImageList;
    Actions: TActionList;
    actFileRefresh: TAction;
    actEditGoto: TAction;
    actFilePrint: TAction;
    actOptionsConfigure: TAction;
    actHelpHelp: TAction;
    actEditCopy: TAction;
    JvSearchFiles: TJvSearchFiles;
    actViewStayOnTop: TAction;
    TBXDock1: TTBXDock;
    Toolbar: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem3: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem5: TTBXItem;
    PopupMenu: TTBXPopupMenu;
    Goto1: TTBXItem;
    Refresh1: TTBXItem;
    N1: TTBXSeparatorItem;
    CopyAll1: TTBXItem;
    N2: TTBXSeparatorItem;
    Print1: TTBXItem;
    Options1: TTBXItem;
    ToDoView: TVirtualStringTree;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    procedure actEditCopyExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFilePrintUpdate(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actEditGotoExecute(Sender: TObject);
    procedure actOptionsConfigureExecute(Sender: TObject);
    procedure actViewStayOnTopExecute(Sender: TObject);
    procedure actViewStayOnTopUpdate(Sender: TObject);
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
    procedure ToDoViewHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actHelpHelpExecute(Sender: TObject);
  private
    { Private declarations }
    FIsFirstActivation: Boolean;
    FDataList: TObjectList;
    FScannedFiles: TStringList;
    function GetSelectedItem: TToDoInfo;
    procedure ClearDataListAndListView;
    procedure EnumerateFilesByDirectory;
    procedure EnumerateOpenFiles;
    procedure LoadFile(const FileName: string);
    procedure ParseComment(const FileName: string; const SComment, EComment: string;
      const TokenString: string; LineNumber: Integer);
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  public
    { Public declarations }
    procedure RefreshTodoList;
  end;

  TToDoExpert = class(TInterfacedPersistent, IJvAppStorageHandler)
  private
    FScanType: TToDoScanType;
    FDirsToScan: string;
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

uses dmCommands, Clipbrd, uEditAppIntfs, Math, frmPyIDEMain, dlgToDoOptions,
  uCommonFunctions, JvJVCLUtils, JvDockGlobals;

{$R *.dfm}

Type

  PToDoRec = ^TToDoRec;
  TToDoRec = record
    ToDoInfo : TToDoInfo;
  end;

function AnsiCaseInsensitivePos(const SubString, S: string): Integer;
begin
  Result := AnsiPos(AnsiUpperCase(SubString), AnsiUpperCase(S));
end;

procedure TToDoWindow.actEditCopyExecute(Sender: TObject);
var
  ClipText: TStrings;
  i: Integer;
begin
  inherited;

  ClipText := TStringList.Create;
  try
    for i := 0 to fDataList.Count - 1 do
    with TToDoInfo(fDataList[i]) do begin
      ClipText.Add(IntToStr(Ord(Priority)) + #9 +
        Display + #9 +
        ExtractFileName(FileName) + #9 +
        IntToStr(LineNo));
    end;
    Clipboard.AsText := ClipText.Text;
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
  FScannedFiles := TStringList.Create;

  // Let the tree know how much data space we need.
  ToDoView.NodeDataSize := SizeOf(TToDoRec);
  ToDoView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  ToDoView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
end;

procedure TToDoWindow.FormDestroy(Sender: TObject);
begin
  ClearDataListAndListView;

  FreeAndNil(FDataList);
  FreeAndNil(FScannedFiles);

  inherited;
end;

procedure TToDoWindow.ClearDataListAndListView;
begin
  ToDoView.Clear;
  FDataList.Clear;
end;

{ TTokenList }

procedure TTokenList.AddToken(const Token: string;
  Priority: TToDoPriority);
Var
  TokenInfo: TTokenInfo;
begin
  TokenInfo := TTokenInfo.Create;
  TokenInfo.Token := Token;
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
  Token : string;
  SL : TStringList;
begin
  with AppStorage do begin
    FShowTokens := ReadBoolean(BasePath+'\ShowTokens');
    ReadEnumeration(BasePath+'\ScanType', TypeInfo(TToDoScanType), FScanType, FScanType);
    SL := TStringList.Create;
    try
      ReadStringList(BasePath+'\DirsToScan', SL);
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
        Token := ReadString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Token']));
        Priority := tpMed;
        ReadEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Priority']),
          TypeInfo(TToDoPriority), Priority, Priority);
        FTokenList.AddToken(Token, Priority);
      end;
    end;
  end;
end;

procedure TToDoExpert.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  i : integer;
  SL : TStringList;
begin
  with AppStorage do begin
    WriteBoolean(BasePath+'\ShowTokens', FShowTokens);
    WriteEnumeration(BasePath+'\ScanType', TypeInfo(TToDoScanType), FScanType);
    SL := TStringList.Create;
    try
      SL.Text := FDirsToScan;
      WriteStringList(BasePath+'\DirsToScan', SL);
    finally
      SL.Free;
    end;
    WriteBoolean(BasePath+'\RecurseDirScan', FRecurseDirScan);
    DeleteSubTree(ConcatPaths([BasePath, 'Todo Tokens']));
    WriteInteger(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Count']), FTokenList.Count);
    for i := 0 to FTokenList.Count - 1 do begin
      WriteString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Token']),
        FTokenList[i]);
      WriteEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(i), 'Priority']),
        TypeInfo(TToDoPriority), TTokenInfo(FTokenList.Objects[i]).Priority);
    end;
  end;
end;

resourcestring
  SDoneTodoDesignation = 'Done';

procedure TToDoWindow.ParseComment(const FileName, SComment, EComment,
  TokenString: string; LineNumber: Integer);
var
  i, j, k, n, m, TokenPos, NextCharPos: Integer;
  Info: TToDoInfo;
  IsDoneTodoItem: Boolean;
  ParsingString: string;
  OptionChar: Char;
begin
  for i := 0 to ToDoExpert.FTokenList.Count - 1 do
  begin
    n := AnsiCaseInsensitivePos(ToDoExpert.FTokenList[i], TokenString);
    if n > 1 then
    begin
      // We found a token that looks like a TODO comment. Now
      // verify that it *is* one: either a white-space or the
      // comment token need to be right in front of the TODO item

      // Remove comment characters
      ParsingString := TokenString;
      Delete(ParsingString, 1, Length(SComment));
      // Remove white-space left and right
      ParsingString := Trim(ParsingString);

      // The TODO token should be at the beginning of the comment
      TokenPos := AnsiCaseInsensitivePos(ToDoExpert.FTokenList[i], ParsingString);
      if TokenPos <> 1 then
        Continue;

      // The TODO token should be followed by a non-alphanumeric character
      NextCharPos := TokenPos + Length(ToDoExpert.FTokenList[i]);
      if (NextCharPos <= Length(ParsingString)) and IsCharAlphaNumeric(ParsingString[NextCharPos]) then
        Continue;

      // Token found in comment line
      Info := TToDoInfo.Create;

      // Remove token from string
      Delete(ParsingString, 1, Length(ToDoExpert.FTokenList[i]));
      ParsingString := TrimRight(ParsingString);

      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(ParsingString) do
      begin
        if not (ParsingString[j + 1] in ['0'..'9']) then
          Break;
        Inc(j);
      end;
      Info.NumericPriority := StrToIntDef(Copy(ParsingString, 1, j), 0);
      Delete(ParsingString, 1, j);
      ParsingString := TrimLeft(ParsingString);

      IsDoneTodoItem := (AnsiCaseInsensitivePos(SDoneTodoDesignation, ParsingString) = 1);

      { zTODO -oTestCase: -cIssue <-- test case for colon }
      // Delete everything being with a possible trailing colon:
      j := Pos(':', ParsingString);
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
      OptionChar := #0; // Initialize to make compiler happy - redundant
      while Pos('-', ParsingString) > 0 do
      begin
        if Length(ParsingString) > 1 then
        begin
          OptionChar := UpCase(ParsingString[2]);
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
          'O': Info.Owner := Trim(Copy(ParsingString, 1, k));
          'C': Info.ToDoClass := Trim(Copy(ParsingString, 1, k));
        end;

        // Delete everything up to, but not including, the
        // next option switch
        Delete(ParsingString, 1, j);
      end;

      Info.Raw := TokenString;

      if IsDoneTodoItem then
        Info.Priority := tpDone
      else
        Info.Priority := TTokenInfo(ToDoExpert.FTokenList.Objects[i]).Priority;

      if not ToDoExpert.FShowTokens then begin
        n := n + Length(ToDoExpert.FTokenList[i]);
        // Do not show anything up to ':'
        n := Max(n, Pos(':', TokenString)+1);
      end;
      if EComment <> '' then // Trim end-comment token.
        m := AnsiCaseInsensitivePos(EComment, TokenString) - 1
      else
        m := Length(TokenString);
      if m < 1 then
        m := Length(TokenString);
      // Prevent multi-line to do notes
      j := Pos(#13, TokenString);
      k := Pos(#10, TokenString);
      if j = 0 then j := 999999;
      if k = 0 then k := 999999;
      m := Min(m, Min(j, k));
      // The +1 is necessary to match IDE's line numbering
      Info.Display := Trim(Copy(TokenString, n, (m - n) + 1));
      Info.LineNo := LineNumber;
      Info.FileName := FileName;
      FDataList.Add(Info);

      Break; // Comment line parsed, stop searching for more To Do items in that line
    end;
  end;
end;

procedure TToDoWindow.LoadFile(const FileName: string);
Var
  SourceCode : TStringList;
  Editor : IEditor;
  i, Index : integer;
  TokenString : string;
begin
  SourceCode := TStringList.Create;
  try
    //  Read file into SourceCode
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    if Assigned(Editor) then
      SourceCode.Assign(Editor.SynEdit.Lines)
    else
      try
        SourceCode.LoadFromFile(FileName);
      except
        MessageDlg('Could not load FileName "' + FileName + '"', mtWarning, [mbOK], 0);
        Exit;
      end;
    // scan source code
    for i := 0 to SourceCode.Count - 1 do begin
      Index := Pos('#', SourceCode[i]);
      if Index > 0 then begin
        TokenString := Copy(SourceCode[i], Index, Length(SourceCode[i]) - Index + 1);
        ParseComment(FileName, '#', '', Tokenstring, i);
      end;
    end;
  finally
    SourceCode.Free;
  end;
end;

procedure TToDoWindow.RefreshTodoList;
var
  Cursor: IInterface;
begin
  Cursor := WaitCursor;
  try
    FScannedFiles.Clear;
    ClearDataListAndListView;

    case ToDoExpert.FScanType of
      tstOpenFiles:
        EnumerateOpenFiles;
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
  end;
end;

procedure TToDoWindow.EnumerateFilesByDirectory;
Var
  Dirs: TStringList;
  i, j : integer;
begin
  JvSearchFiles.FileParams.FileMaskSeperator := ';';
  JvSearchFiles.FileParams.FileMask := CommandsDataModule.PyIDEOptions.FileExplorerFilter;
  JvSearchFiles.FileParams.SearchTypes := [stFileMask];

  Dirs := TStringList.Create;
  Dirs.Text := ToDoExpert.FDirsToScan;
  try
    for i := 0 to Dirs.Count - 1 do begin
      jvSearchFiles.RootDirectory := Dirs[i];
      if ToDoExpert.FRecurseDirScan then
        JvSearchFiles.DirOption := doIncludeSubDirs
      else
        JvSearchFiles.DirOption := doExcludeSubDirs;
      if JvSearchFiles.Search then begin
        for j := 0 to JvSearchFiles.Files.Count - 1 do
          LoadFile(JvSearchFiles.Files[j]);
      end;
    end;
  finally
    Dirs.Free;
  end;
end;

procedure TToDoWindow.EnumerateOpenFiles;
var
  i : integer;
  FileName : string;
  Editor : IEditor;
begin
  for i := 0 to GI_EditorFactory.Count - 1 do begin
    Editor := GI_EditorFactory.Editor[i];
    FileName := Editor.GetFileNameOrTitle;
    LoadFile(FileName);
  end;
end;

procedure TToDoWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if FIsFirstActivation then
  begin
    FIsFirstActivation := False;
    RefreshTodoList;
  end;
  if not HasFocus then begin
    FGPanelEnter(Self);
    PostMessage(ToDoView.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TToDoWindow.actViewStayOnTopExecute(Sender: TObject);
begin
  if FormStyle = fsNormal then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TToDoWindow.actViewStayOnTopUpdate(Sender: TObject);
begin
  actViewStayOnTop.Checked := FormStyle = fsStayOnTop;
end;

procedure TToDoWindow.TodoViewKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then begin
    actEditGotoExecute(Self);
    Key := #0;
  end;
end;

procedure TToDoWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    ToDoView.Header.Invalidate(nil, True);
    ToDoView.Colors.HeaderHotColor :=
      CurrentTheme.GetItemTextColor(GetItemInfo('active'));
  end;
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
      2:  CellText := ExtractFileName(FileName);
      3:  CellText := IntToStr(LineNo);
    end;
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
    1: Result := AnsiCompareStr(ToDoInfo1.Display, ToDoInfo2.Display);
    2: Result := AnsiCompareStr(ExtractFileName(ToDoInfo1.FileName),
                    ExtractFileName(ToDoInfo2.FileName));
    3: Result := ToDoInfo1.LineNo - ToDoInfo2.LineNo;
  end;
end;

procedure TToDoWindow.ToDoViewHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if ToDoView.Header.SortColumn = Column then begin
    if ToDoView.Header.SortDirection = sdAscending then
      ToDoView.Header.SortDirection := sdDescending
    else
      ToDoView.Header.SortDirection := sdAscending;
  end else
    ToDoView.Header.SortColumn := Column;
  if ToDoView.Header.SortColumn = 0 then
    ToDoView.Header.Columns[0].Alignment := taLeftJustify  // to make it fit
  else
    ToDoView.Header.Columns[0].Alignment := taCenter;
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




