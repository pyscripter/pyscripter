{-----------------------------------------------------------------------------
 Unit Name: frmToDo
 Author:    Kiriakos Vlahos
 Date:      19-May-2005
 Purpose:   Todo list Dock Form based on GExperts unit and covered by its license

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement
only covers code written by GExperts, Inc and Erik Berry. You should contact
the other authors concerning their respective copyrights and conditions.

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
  Winapi.Messages,
  System.UITypes,
  System.Classes,
  System.ImageList,
  System.Contnrs,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXSkins,
  SpTBXItem,
  JvDockControlForm,
  JvAppStorage,
  JvComponentBase,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  uCommonFunctions,
  frmIDEDockWin;

type
  TToDoPriority = (tpHigh, tpMed, tpLow, tpDone);
  TToDoScanType = (tstOpenFiles, tstProjectFiles, tstDirectory);

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

  TToDoWindow = class(TIDEDockWindow)
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
    mnSeparator1: TSpTBXSeparatorItem;
    mnCopyAll: TSpTBXItem;
    mnSeparator2: TSpTBXSeparatorItem;
    mnPrint: TSpTBXItem;
    mnOptions: TSpTBXItem;
    ToDoView: TVirtualStringTree;
    TBXSeparatorItem5: TSpTBXSeparatorItem;
    tbiHelp: TSpTBXItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    mnHelp: TSpTBXItem;
    tbiAbort: TSpTBXItem;
    Actions: TActionList;
    actFileAbort: TAction;
    actEditCopy: TAction;
    actHelpHelp: TAction;
    actOptionsConfigure: TAction;
    actFilePrint: TAction;
    actEditGoto: TAction;
    actFileRefresh: TAction;
    ToDoImages: TVirtualImageList;
    vicImages: TVirtualImageList;
    icTodo: TSVGIconImageCollection;
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
      var CellText: string);
    procedure ToDoViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ToDoViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ToDoViewCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure ToDoViewShortenString(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Str: string; TextSpace: TDimension; var Result: string;
      var Done: Boolean);
    procedure ToDoViewHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  private
    FToDoExpert: TToDoExpert;
    FIsFirstActivation: Boolean;
    FDataList: TObjectList;
    FAbortSignalled: Boolean;
    function GetSelectedItem: TToDoInfo;
    procedure ClearDataListAndListView;
    procedure EnumerateFilesByDirectory;
    procedure EnumerateOpenFiles;
    procedure EnumerateProjectFiles;
    procedure LoadFile(const FileName: string);
    function ParseComment(const FileName, SComment, EComment,
     TokenString: string; LineNumber: Integer): TToDoInfo;
    function GetPreCallback: TDirectoryWalkProc;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure RefreshTodoList;
    property AbortSignalled: Boolean read FAbortSignalled write FAbortSignalled;
    class function CreateInstance: TIDEDockWindow; override;
end;

var
  ToDoWindow: TToDoWindow;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  System.Math,
  System.IOUtils,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.Clipbrd,
  Vcl.Themes,
  MPCommonUtilities,
  uEditAppIntfs,
  cProjectClasses,
  cPyScripterSettings,
  dmResources,
  dlgToDoOptions;

{$R *.dfm}

type

  PToDoRec = ^TToDoRec;
  TToDoRec = record
    ToDoInfo: TToDoInfo;
  end;

function WideCaseInsensitivePos(const SubString, Str: string): Integer;
begin
  Result := Pos(SubString.ToUpper, Str.ToUpper);
end;

procedure TToDoWindow.actEditCopyExecute(Sender: TObject);
begin
  var ClipText := TStringList.Create;
  try
    for var I := 0 to FDataList.Count - 1 do
      with TToDoInfo(FDataList[I]) do
      begin
        ClipText.Add(IntToStr(Ord(Priority)) + #9 +
          Display + #9 +
          TPath.GetFileName(FileName) + #9 +
          IntToStr(LineNo));
      end;
    Clipboard.AsText := ClipText.Text;
  finally
    ClipText.Free;
  end;
end;

procedure TToDoWindow.actOptionsConfigureExecute(Sender: TObject);
begin
  FToDoExpert.Configure;
end;

function TToDoWindow.GetPreCallback: TDirectoryWalkProc;
// Separate method to avoid memory leak
// See http://stackoverflow.com/questions/6273376/memory-leaks-happens-in-nested-anonymous-method
begin
  Result :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      Name: string;
    begin
      Result := not FAbortSignalled;

      if Result and (FileInfo.Attr and System.SysUtils.faDirectory = 0) and
        (FileInfo.Attr and System.SysUtils.faHidden = 0) then
      begin
        Name := Path + FileInfo.Name;
        LoadFile(Name);
        Application.ProcessMessages;
      end;
    end;
end;

function TToDoWindow.GetSelectedItem: TToDoInfo;
begin
  Result := nil;
  var Node := ToDoView.GetFirstSelected;
  if Assigned(Node) then
    Result := PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo;
end;

procedure TToDoWindow.actFileRefreshExecute(Sender: TObject);
begin
  RefreshTodoList;
end;

procedure TToDoWindow.actEditGotoExecute(Sender: TObject);
begin
  var SelectedItem := GetSelectedItem;
  if SelectedItem = nil then Exit;

  GI_PyIDEServices.ShowFilePosition(SelectedItem.FileName, SelectedItem.LineNo+1, 1);
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
begin
  if FDataList.Count = 0 then
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
    for var I := 0 to FDataList.Count - 1 do
    begin
      with TToDoInfo(FDataList[I]) do
      begin
        with RichEdit.SelAttributes do
        begin
          case Priority of
            tpHigh: Style := [fsBold];
            tpMed: Style := [];
            tpLow: Style := [fsItalic];
          end;
          Size := 10;
        end;
        RichEdit.Lines.Add(TPath.GetFileName(FileName) + ' (' + IntToStr(LineNo) + ')' +
          #09 + PriorityText[Priority] + #9 + Display);
      end;
    end;
    RichEdit.Print(STodoItems);
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
  ImageName := 'TodoWin';
  inherited;

  FToDoExpert := TToDoExpert.Create;
  FIsFirstActivation := True;
  FDataList := TObjectList.Create(True); // Owned objects

  // Let the tree know how much data space we need.
  ToDoView.NodeDataSize := SizeOf(TToDoRec);
end;

procedure TToDoWindow.FormDestroy(Sender: TObject);
begin
  ClearDataListAndListView;

  FreeAndNil(FDataList);
  FreeAndNil(FToDoExpert);

  inherited;
end;

procedure TToDoWindow.ClearDataListAndListView;
begin
  ToDoView.Clear;
  FDataList.Clear;
end;

class function TToDoWindow.CreateInstance: TIDEDockWindow;
begin
  ToDoWindow := TToDoWindow.Create(Application);
  Result := ToDoWindow;
end;

{ TTokenList }

procedure TTokenList.AddToken(const Token: string;
  Priority: TToDoPriority);
begin
  var TokenInfo := TTokenInfo.Create;
  TokenInfo.Token := Token.ToUpper;
  TokenInfo.Priority := Priority;

  AddObject(Token, TokenInfo);
end;

destructor TTokenList.Destroy;
begin
  for var I := Count - 1 downto 0 do
    Objects[I].Free;
  inherited;
end;

{ TToDoExpert }

procedure TToDoExpert.Configure;
var
  TodoListOptionsUnchanged: Boolean;
  OldScanType: TToDoScanType;
begin
  TodoListOptionsUnchanged := True;

  var Dlg := TToDoOptionsDlg.Create(nil);
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
  inherited;
end;

procedure TToDoExpert.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
var
  NTokens: Integer;
  Priority: TToDoPriority;
  Token: string;
  SL: TStringList;
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
      for var I := 0 to NTokens - 1 do begin
        Token := ReadString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(I), 'Token']));
        Priority := tpMed;
        ReadEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(I), 'Priority']),
          TypeInfo(TToDoPriority), Priority, Priority);
        FTokenList.AddToken(Token, Priority);
      end;
    end;
    ToDoWindow.ToDoView.Header.Columns[2].Width :=
      ToDoWindow.PPIScale(AppStorage.ReadInteger(BasePath+'\FileName Width', 150));
    ToDoWindow.ToDoView.Header.Columns[3].Width :=
      ToDoWindow.PPIScale(AppStorage.ReadInteger(BasePath+'\Line Width', 50));
  end;
end;

procedure TToDoExpert.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
var
  SL: TStringList;
begin
  with AppStorage do begin
    DeleteSubTree(BasePath);
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

    WriteInteger(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Count']), FTokenList.Count);
    for var I := 0 to FTokenList.Count - 1 do begin
      WriteString(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(I), 'Token']),
        FTokenList[I]);
      WriteEnumeration(AppStorage.ConcatPaths([BasePath, 'Todo Tokens', 'Token'+IntToStr(I), 'Priority']),
        TypeInfo(TToDoPriority), TTokenInfo(FTokenList.Objects[I]).Priority);
    end;
    AppStorage.WriteInteger(BasePath+'\FileName Width',
      ToDoWindow.PPIUnScale(ToDoWindow.TodoView.Header.Columns[2].Width));
    AppStorage.WriteInteger(BasePath+'\Line Width',
      ToDoWindow.PPIUnScale(ToDoWindow.TodoView.Header.Columns[3].Width));
  end;
end;

resourcestring
  SDoneTodoDesignation = 'Done';

function TToDoWindow.ParseComment(const FileName, SComment, EComment,
  TokenString: string; LineNumber: Integer): TToDoInfo;
var
  I, J, K, N, M, TokenPos, NextCharPos: Integer;
  IsDoneTodoItem: Boolean;
  ParsingString: string;
  OptionChar: WideChar;
begin
  // Token string is alread trimmed and without SComment
  Result := nil;
  for I := 0 to FToDoExpert.FTokenList.Count - 1 do
  begin
    if TokenString.StartsWith(
      TTokenInfo(FToDoExpert.FTokenList.Objects[I]).Token, True) then
    begin
      // We found a token that looks like a TODO comment and is in the position 1.
      N := 1;
      // Remove comment characters
      ParsingString := TokenString;
      TokenPos := 1;

      // The TODO token should be followed by a non-alphanumeric character
      NextCharPos := TokenPos + Length(FToDoExpert.FTokenList[I]);
      if (NextCharPos <= Length(ParsingString)) and IsCharAlphaNumericW(ParsingString[NextCharPos]) then
        Continue;

      // Token found in comment line
      Result := TToDoInfo.Create;

      // Remove token from string
      Delete(ParsingString, 1, Length(FToDoExpert.FTokenList[I]));
      ParsingString := TrimRight(ParsingString);

      // Identify numeric priority (if there is one)
      J := 0;
      while J < Length(ParsingString) do
      begin
        if not CharInSet(ParsingString[J + 1], ['0'..'9']) then
          Break;
        Inc(J);
      end;
      Result.NumericPriority := StrToIntDef(Copy(ParsingString, 1, J), 0);
      Delete(ParsingString, 1, J);
      ParsingString := TrimLeft(ParsingString);

      IsDoneTodoItem := (WideCaseInsensitivePos(SDoneTodoDesignation, ParsingString) = 1);

      { zTODO -oTestCase: -cIssue <-- test case for colon }
      // Delete everything being with a possible trailing colon:
      J := Pos(':', ParsingString);
      if J > 0 then
        Delete(ParsingString, J, Length(ParsingString))
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
      while Pos('-', ParsingString) > 0 do
      begin
        if Length(ParsingString) > 1 then
        begin
          OptionChar := Copy(ParsingString, 2, 1).ToUpper[1];
          Delete(ParsingString, 1, 2);
        end
        else
          Break;

        // Find char immediately preceding the next option switch
        J := Pos('-', ParsingString) - 1;
        if J > 0 then
          K := J
        else
          K := Length(ParsingString);

        case OptionChar of
          'O': Result.Owner := Trim(Copy(ParsingString, 1, K));
          'C': Result.ToDoClass := Trim(Copy(ParsingString, 1, K));
        end;

        // Delete everything up to, but not including, the
        // next option switch
        Delete(ParsingString, 1, J);
      end;

      Result.Raw := TokenString;

      if IsDoneTodoItem then
        Result.Priority := tpDone
      else
        Result.Priority := TTokenInfo(FToDoExpert.FTokenList.Objects[I]).Priority;

      if not FToDoExpert.FShowTokens then begin
        N := N + Length(FToDoExpert.FTokenList[I]);
        // Do not show anything up to ':'
        N := Max(N, Pos(':', TokenString)+1);
      end;
      if EComment <> '' then // Trim end-comment token.
        M := WideCaseInsensitivePos(EComment, TokenString) - 1
      else
        M := Length(TokenString);
      if M < 1 then
        M := Length(TokenString);
      // Prevent multi-line to do notes
      J := Pos(#$000D, TokenString);
      K := Pos(#$000A, TokenString);
      if J = 0 then J := 999999;
      if K = 0 then K := 999999;
      M := Min(M, Min(J, K));
      // The +1 is necessary to match IDE's line numbering
      Result.Display := Trim(Copy(TokenString, N, (M - N) + 1));
      Result.LineNo := LineNumber;
      Result.FileName := FileName;
      FDataList.Add(Result);

      Break; // Comment line parsed, stop searching for more To Do items in that line
    end;
  end;
end;

procedure TToDoWindow.LoadFile(const FileName: string);
var
  SourceCode: TStringList;
  Editor: IEditor;
  I, Index: Integer;
  TokenString: string;
  Info, OldInfo: TToDoInfo;
  PStart: PWideChar;
begin
  SourceCode := TStringList.Create;
  try
    //  Read file into SourceCode
    Editor := GI_EditorFactory.GetEditorByFileId(FileName);
    if Assigned(Editor) then
      SourceCode.Assign(Editor.SynEdit.Lines)
    else begin
      if not LoadFileIntoWideStrings(FileName, SourceCode) then Exit;
    end;

    // scan source code
    I := 0;
    OldInfo := nil;
    while I < SourceCode.Count do begin
      TokenString := SourceCode[I];
      PStart := StrScan(PWideChar(TokenString), '#');
      if Assigned(PStart) then begin
        if Assigned(OldInfo) then begin
          if TrimLeft(TokenString)[1] <> '#' then
            OldInfo := nil;  // it is not a multiline comments
        end;

        Index := PStart - PWideChar(TokenString) + 1;
        TokenString := Copy(TokenString, Index + 1, MaxInt);  // We delete the #
        TokenString := Trim(TokenString);

        Info := ParseComment(FileName, '#', '', TokenString, I);
        if Assigned(Info) then begin
          OldInfo := Info;
        end else if Assigned(OldInfo) then
          // Append new comment to Info.Display
          OldInfo.Display := OldInfo.Display + ' ' + TokenString;
      end else
        OldInfo := nil;
      Inc(I);
    end;
  finally
    SourceCode.Free;
  end;
end;

procedure TToDoWindow.RefreshTodoList;
begin
  FAbortSignalled := False;
  actFileAbort.Enabled := True;
  actFileRefresh.Enabled := False;
  Application.ProcessMessages;  // to repaint etc.
  try
    ClearDataListAndListView;

    case FToDoExpert.FScanType of
      tstOpenFiles:
        EnumerateOpenFiles;
      tstProjectFiles:
        EnumerateProjectFiles;
      tstDirectory:
        begin
          if Trim(FToDoExpert.FDirsToScan) <> '' then
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
    FAbortSignalled := True;
    actFileRefresh.Enabled := True;
    actFileAbort.Enabled := False;
  end;
end;

procedure TToDoWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.ReadPersistent('ToDo Options', FToDoExpert);
end;

procedure TToDoWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WritePersistent('ToDo Options', FToDoExpert);
end;

procedure TToDoWindow.EnumerateFilesByDirectory;
var
  PreCallback: TDirectoryWalkProc;
  Paths: string;
begin
  Paths := StringReplace(FToDoExpert.FDirsToScan, sLineBreak, ';', [rfReplaceAll]);
  if Paths[Length(Paths)] = ';' then
    Delete(Paths, Length(Paths), 1);
  PreCallback := GetPreCallback();
  WalkThroughDirectories(Paths,
    PyIDEOptions.PythonFileExtensions, PreCallback,
    FToDoExpert.FRecurseDirScan);
end;

procedure TToDoWindow.EnumerateOpenFiles;
begin
  GI_EditorFactory.FirstEditorCond(function(Editor: IEditor): Boolean
  begin
    if FAbortSignalled then
      Exit(True)
    else
      Result := False;
    var FileName := Editor.FileId;
    LoadFile(FileName);
  end);
end;

function ProcessProjectFile(Node: TAbstractProjectNode; Data: Pointer): Boolean;
var
  FileName: string;
begin
   if TToDoWindow(Data).FAbortSignalled then begin
     Result := True;
     Exit;
   end else begin
     Application.ProcessMessages;
     Result := False;
   end;
   if (Node is TProjectFileNode) and (TProjectFileNode(Node).FileName <> '') then
   begin
     FileName := GI_PyIDEServices.ReplaceParams(TProjectFileNode(Node).FileName);
     if GI_PyIDEServices.FileIsPythonSource(FileName) then
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
end;

procedure TToDoWindow.TodoViewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    actEditGotoExecute(Self);
    Key := #0;
  end;
end;

procedure TToDoWindow.ToDoViewShortenString(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Str: string; TextSpace: TDimension; var Result: string;
  var Done: Boolean);
begin
  if Column = 2 then begin
    Result := ShortenStringEx(TargetCanvas.Handle, Str, TextSpace,
      Application.UseRightToLeftReading, sseFilePathMiddle);
    Done := True;
  end;
end;

procedure TToDoWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  icTodo.SVGIconItems.BeginUpdate;
  try
    icTodo.FixedColor := SvgFixedColor(clWindowText);
    icTodo.AntiAliasColor := StyleServices.GetSystemColor(clWindow);
  finally
    icTodo.SVGIconItems.EndUpdate;
  end;
end;

procedure TToDoWindow.ToDoViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(ParentNode = nil, 'ToDoViewShortenString');
  Assert(Integer(Node.Index) < FDataList.Count, 'ToDoViewShortenString');
  PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo :=
    FDataList[Node.Index] as TToDoInfo;
end;

procedure TToDoWindow.ToDoViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  Assert(Integer(Node.Index) < FDataList.Count, 'ToDoViewGetText');
  with PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo do
    case Column of
      0:  CellText := '';
      1:  CellText := Display;
      2:  CellText := FileName;
      3:  CellText := IntToStr(LineNo);
    end;
end;

procedure TToDoWindow.ToDoViewHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
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
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind in [ikNormal, ikSelected]) and (Column = 0) then begin
    Assert(Integer(Node.Index) < FDataList.Count, 'ToDoViewGetImageIndex');
    with PToDoRec(ToDoView.GetNodeData(Node))^.ToDoInfo do
      if Priority = tpMed then
        ImageIndex := -1
      else
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
var
  ToDoInfo1, ToDoInfo2: TToDoInfo;
begin
  Assert(Assigned(Node1) and Assigned(Node2), 'ToDoViewCompareNodes');
  ToDoInfo1 := PToDoRec(ToDoView.GetNodeData(Node1))^.ToDoInfo;
  ToDoInfo2 := PToDoRec(ToDoView.GetNodeData(Node2))^.ToDoInfo;
  case Column of
    -1: Result := 0;
    0: Result := Ord(ToDoInfo1.Priority) - Ord(ToDoInfo2.Priority);
    1: Result := AnsiCompareStr(ToDoInfo1.Display, ToDoInfo2.Display);
    2: Result := AnsiCompareStr(TPath.GetFileName(ToDoInfo1.FileName),
                    TPath.GetFileName(ToDoInfo2.FileName));
    3: Result := ToDoInfo1.LineNo - ToDoInfo2.LineNo;
  end;
end;

procedure TToDoWindow.actHelpHelpExecute(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

initialization
  TIDEDockWindow.RegisterDockWinClass(ideTodo, TToDoWindow);
end.



