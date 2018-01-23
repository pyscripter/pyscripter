{-----------------------------------------------------------------------------
 Unit Name: frmCodeExplorer
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Code Explorer Window
            Code Based on SynEdit demo
 History:
-----------------------------------------------------------------------------}

unit frmCodeExplorer;

interface

uses
  Windows, Messages, System.UITypes, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, JvDockControlForm, JvAppStorage,
  Menus, Contnrs, VirtualTrees, frmIDEDockWin, TB2Item,
  cPythonSourceScanner, SpTBXItem, JvComponentBase, SpTBXControls;

type
  TCESortOrder = (soPosition, soAlpha);
  TCEExpandState = (esExpanded, esCollapsed, esUnknown);

  TAbstractCENode = class
  private
    fChildren : TObjectList;
    function GetChildCount: integer;
    function GetChildren(i : integer): TAbstractCENode;
  protected
    fCodeElement : TBaseCodeElement;
    fExpanded : TCEExpandState;
    fNode : PVirtualNode;
    function GetHint: string; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetImageIndex : integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(CENode : TAbstractCENode);
    procedure Sort(SortOrder : TCESortOrder);
    property CodeElement : TBaseCodeElement read fCodeElement;
    property Hint : string read GetHint;
    property Caption : string read GetCaption;
    property ImageIndex : integer read GetImageIndex;
    property ChildCount : integer read GetChildCount;
    property Children[i : integer] : TAbstractCENode read GetChildren;
    property Expanded : TCEExpandState read fExpanded write fExpanded;
  end;

  TCodeElementCENode = class(TAbstractCENode)
  private
    function GetCodeBlock : TCodeBlock;
  public
    function GetScopeForLine(LineNo: integer): TCodeElementCENode;
    property CodeBlock : TCodeBlock read GetCodeBlock;
  end;

  TModuleCENode = class(TCodeElementCENode)
  private
    fOffsetXY: TPoint;
    function GetParsedModule: TParsedModule;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromModule(AModule : TParsedModule);
    property Module : TParsedModule read GetParsedModule;
    property OffsetXY : TPoint read fOffsetXY write fOffsetXY;
  end;

  TImportsCENode = class(TAbstractCENode)
  private
    fModule : TParsedModule;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromModule(AModule : TParsedModule);
  end;

  TImportCENode = class(TAbstractCENode)
  private
    function GetModuleImport: TModuleImport;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromModuleImport(AModuleImport : TModuleImport);
    property ModuleImport : TModuleImport read GetModuleImport;
  end;

  TImportNameCENode = class(TAbstractCENode)
  private
    function GetVariable : TVariable;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromVariable(AVariable : TVariable);
    property Variable : TVariable read GetVariable;
  end;

  TGlobalsCENode = class(TAbstractCENode)
  private
    fModule : TParsedModule;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromModule(AModule : TParsedModule);
  end;

  TVariableCENode = class(TAbstractCENode)
  private
    function GetVariable: TVariable;
  protected
    function GetCaption: string; override;
  public
    constructor CreateFromVariable(AVariable : TVariable);
    property Variable : TVariable read GetVariable;
  end;

  TGlobalCENode = class(TVariableCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TClassCENode = class(TCodeElementCENode)
  private
    function GetParsedClass: TParsedClass;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromClass(AClass : TParsedClass);
    property ParsedClass : TParsedClass read GetParsedClass;
  end;

  TAtrributesCENode = class(TAbstractCENode)
  private
    fParsedClass : TParsedClass;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromClass(AClass : TParsedClass);
  end;

  TAttributeCENode = class(TVariableCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TFunctionCENode = class(TCodeElementCENode)
  private
    function GetParsedFunction: TParsedFunction;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromFunction(AFunction : TParsedFunction);
    property ParsedFunction : TParsedFunction read GetParsedFunction;
  end;

  TMethodCENode = class(TFunctionCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TCEUpdateReason = (ceuChange, ceuEnter, ceuExit);

  // for storing and restoring code explorer data
  ICodeExplorerData = interface
    function GetSourceScanner : IAsyncSourceScanner;
    procedure SetSourceScanner(SC : IAsyncSourceScanner);
    function GetNewSourceScanner : IAsyncSourceScanner;
    procedure SetNewSourceScanner(SC : IAsyncSourceScanner);
    function GetModuleNode : TModuleCENode;
    procedure SetModuleNode(ModuleNode : TModuleCENode);
    property SourceScanner : IAsyncSourceScanner
      read GetSourceScanner write SetSourceScanner;
    property NewSourceScanner : IAsyncSourceScanner
      read GetNewSourceScanner write SetNewSourceScanner;
    property ModuleNode : TModuleCENode
      read GetModuleNode write SetModuleNode;
  end;

  TCodeExplorerData = class(TInterfacedObject, ICodeExplorerData)
  private
    fModuleNode : TModuleCENode;
    fSourceScanner : IAsyncSourceScanner;
    fNewSourceScanner : IAsyncSourceScanner;
    function GetSourceScanner : IAsyncSourceScanner;
    procedure SetSourceScanner(SC : IAsyncSourceScanner);
    function GetModuleNode : TModuleCENode;
    procedure SetModuleNode(AModuleNode : TModuleCENode);
    function GetNewSourceScanner : IAsyncSourceScanner;
    procedure SetNewSourceScanner(SC : IAsyncSourceScanner);
  public
    destructor Destroy; override;
  end;

  TCodeExplorerWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    Panel1: TPanel;
    ExplorerTree: TVirtualStringTree;
    CEPopupMenu: TSpTBXPopupMenu;
    mnExpandAll: TSpTBXItem;
    nCollapseAll: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    mnShowSelection: TSpTBXItem;
    CENodePopUpMenu: TSpTBXPopupMenu;
    mnHighlight: TSpTBXItem;
    mnFindDefinition: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    mnFindReferences: TSpTBXItem;
    mnAlphaSort: TSpTBXItem;
    mnFollowEditor: TSpTBXItem;
    procedure ExplorerTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure FormCreate(Sender: TObject);
    procedure ExplorerTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ExplorerTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ExplorerTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FormDestroy(Sender: TObject);
    procedure ExplorerTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure ExplorerTreeDblClick(Sender: TObject);
    procedure mnExpandAllClick(Sender: TObject);
    procedure nCollapseAllClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ExplorerTreeKeyPress(Sender: TObject; var Key: Char);
    procedure ExplorerTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ExplorerTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mnHighlightClick(Sender: TObject);
    procedure mnFindDefinitionClick(Sender: TObject);
    procedure mnFindReferencesClick(Sender: TObject);
    procedure mnAlphaSortClick(Sender: TObject);
    procedure ExplorerTreeCollapsed(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ExplorerTreeExpanded(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ExplorerTreeScroll(Sender: TBaseVirtualTree; DeltaX,
      DeltaY: Integer);
    procedure mnFollowEditorClick(Sender: TObject);
  private
    procedure NavigateToNodeElement(Node: PVirtualNode;
      ForceToMiddle : Boolean = True; Activate : Boolean = True);
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
    WorkerThread: TThread;
    procedure ClearAll;
    procedure ShowEditorCodeElement;
    procedure UpdateWindow(UpdateReason : TCEUpdateReason);
    procedure ShutDownWorkerThread;
  end;

var
  CodeExplorerWindow: TCodeExplorerWindow;

implementation

uses frmPyIDEMain, dmCommands, uEditAppIntfs, SynEdit,
  SynEditTypes, uCommonFunctions, Math, cPyScripterSettings;

{$R *.dfm}

type
  TScanCodeThread = class(TThread)
  private
    fOldCEData : ICodeExplorerData;
    fNewCEData : ICodeExplorerData;
    fScanEventHandle: THandle;
    fSourceChanged: boolean;
    procedure GetEditorData;
    procedure SetResults;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModified;
    procedure Shutdown;
  end;

constructor TScanCodeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Priority := tpLowest;
  fScanEventHandle := CreateEvent(nil, FALSE, FALSE, nil);
  if (fScanEventHandle = 0) or (fScanEventHandle = INVALID_HANDLE_VALUE) then
    raise EOutOfResources.Create('Couldn''t create WIN32 event object');
  //Resume;  It is resumed from the main form
end;

destructor TScanCodeThread.Destroy;
begin
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(fScanEventHandle);
  inherited Destroy;
end;

procedure TScanCodeThread.Execute;
Var
  ParsedModule : TParsedModule;
begin
  while not Terminated do begin
    WaitForSingleObject(fScanEventHandle, INFINITE);
    repeat
      // make sure the event is reset when we are still in the repeat loop
      ParsedModule := nil;
      ResetEvent(fScanEventHandle);
      if Terminated then
        break;
      Synchronize(GetEditorData);

      fSourceChanged := False;

      Sleep(100);

      if fSourceChanged then
        continue;
      if True then

      if Terminated then
        break;
      if Assigned(fNewCEData) and Assigned(fNewCEData.NewSourceScanner) then
        ParsedModule := fNewCEData.NewSourceScanner.ParsedModule;  //Wait till scanning is finished;
    until not fSourceChanged or Terminated;

    if Terminated then
      break
    // if Assigned(fNewCEData) and not Assigned(ParsedModule) means that StopScanning was called
    else if  Assigned(ParsedModule)  or
      not (Assigned(fNewCEData) and Assigned(fNewCEData.NewSourceScanner))
    then
      Synchronize(SetResults);
    // and go to sleep again
  end;
end;

procedure TScanCodeThread.GetEditorData;
Var
  Editor : IEditor;
begin
  Editor := PyIDEMainForm.GetActiveEditor;
  if Assigned(Editor) then begin
    fNewCEData := Editor.CodeExplorerData;
    fNewCEData.NewSourceScanner := Editor.SourceScanner;
  end else begin
    fNewCEData := nil;
  end;
end;

procedure TScanCodeThread.SetModified;
{it is called in the main thread}
begin
  if Terminated or (csDestroying in CodeExplorerWindow.ComponentState) then Exit;
  fSourceChanged := True;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(fScanEventHandle);
end;

procedure TScanCodeThread.SetResults;
Var
  SameModule : Boolean;
  NewMod : TParsedModule;
  ModuleCENode : TModuleCENode;
begin
  if Terminated or (csDestroying in CodeExplorerWindow.ComponentState) then Exit;

  if Assigned(fNewCEData) and Assigned(fNewCEData.NewSourceScanner) then
    NewMod := fNewCEData.NewSourceScanner.ParsedModule
  else
    NewMod := nil;

  if not Assigned(NewMod) then with CodeExplorerWindow do begin
    ExplorerTree.Clear;
    if Assigned(fNewCEData) then begin
      fNewCEData.SourceScanner := fNewCEData.NewSourceScanner;
      fNewCEData.NewSourceScanner := nil;
      fNewCEData.ModuleNode := nil;
    end;
    fOldCEData := fNewCEData;
    fNewCEData := nil;
    Exit;
  end;

  SameModule := (fNewCEData = fOldCEData) and
               (CodeExplorerWindow.ExplorerTree.RootNodeCount > 0);

  with CodeExplorerWindow do begin
    // Turn off Animation to speed things up
    ExplorerTree.TreeOptions.AnimationOptions :=
      ExplorerTree.TreeOptions.AnimationOptions - [toAnimatedToggle];
    if SameModule then begin
      // The same module but changed
      // ReInit the tree with the new data to keep it as close as possible
      ModuleCENode := TModuleCENode.CreateFromModule(NewMod);
      if mnAlphaSort.Checked then
        ModuleCENode.Sort(soAlpha);
      fNewCEData.SourceScanner := fNewCEData.NewSourceScanner;
      fNewCEData.NewSourceScanner := nil;
      fOldCEData := fNewCEData;
      fOldCEData.ModuleNode := ModuleCENode;
      ExplorerTree.BeginUpdate;
      try
        ExplorerTree.ReinitNode(ExplorerTree.RootNode.FirstChild, True);
        ExplorerTree.InvalidateToBottom(ExplorerTree.GetFirstVisible);
      finally
        ExplorerTree.EndUpdate;
      end;
    end else begin
      ExplorerTree.Clear;
      if Assigned(fNewCEData.ModuleNode) and
        (fNewCEData.SourceScanner = fNewCEData.NewSourceScanner) then
      begin
        // Different module unchanged
        // Restore existing Tree
        fNewCEData.NewSourceScanner := nil;
        fOldCEData := fNewCEData;
        ExplorerTree.RootNodeCount := 1;
        ExplorerTree.Refresh;
        ExplorerTree.OffsetXY := fOldCEData.ModuleNode.OffsetXY;
      end else begin
        // All other cases
        ModuleCENode := TModuleCENode.CreateFromModule(NewMod);
        if mnAlphaSort.Checked then
          ModuleCENode.Sort(soAlpha);
        fNewCEData.SourceScanner := fNewCEData.NewSourceScanner;
        fNewCEData.NewSourceScanner := nil;
        fOldCEData := fNewCEData;
        fOldCEData.ModuleNode := ModuleCENode;
        ExplorerTree.RootNodeCount := 1;
      end;
      ExplorerTree.ValidateNode(ExplorerTree.RootNode.FirstChild, True);
    end;
    ExplorerTree.TreeOptions.AnimationOptions :=
      ExplorerTree.TreeOptions.AnimationOptions + [toAnimatedToggle];
    ShowEditorCodeElement;
  end;
  fNewCEData := nil;
end;

procedure TScanCodeThread.Shutdown;
begin
  Terminate;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(fScanEventHandle);
end;

Type
  PNodeDataRec = ^TNodeDataRec;
  TNodeDataRec = record
    CENode : TAbstractCENode;
  end;

procedure TCodeExplorerWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if ExplorerTree.CanFocus then
    ExplorerTree.SetFocus;
end;

procedure TCodeExplorerWindow.FormCreate(Sender: TObject);
begin
  inherited;
  // Let the tree know how much data space we need.
  ExplorerTree.NodeDataSize := SizeOf(TNodeDataRec);
  WorkerThread := TScanCodeThread.Create;
end;

procedure TCodeExplorerWindow.ExplorerTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  if ExplorerTree.GetNodeLevel(Node) = 0 then
    Data.CENode := TScanCodeThread(WorkerThread).fOldCEData.ModuleNode
  else begin
    ParentData := ExplorerTree.GetNodeData(ParentNode);
    Data.CENode :=
      ParentData.CENode.Children[Node.Index] as TAbstractCENode;
  end;
  if Data.CENode.ChildCount > 0 then
    if Data.CENode.Expanded = esExpanded then
      InitialStates := [ivsHasChildren, ivsExpanded]
    else
      InitialStates := [ivsHasChildren];
  // reverse link from CENode to Tree node
  Data.CENode.fNode := Node;
end;

procedure TCodeExplorerWindow.ExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    NavigateToNodeElement(ExplorerTree.GetFirstSelected);
end;

procedure TCodeExplorerWindow.ExplorerTreeScroll(Sender: TBaseVirtualTree;
  DeltaX, DeltaY: Integer);
Var
  ModuleCENode : TModuleCENode;
begin
  if Assigned(TScanCodeThread(WorkerThread).fOldCEData) then begin
     ModuleCENode := TScanCodeThread(WorkerThread).fOldCEData.ModuleNode;
     if Assigned(ModuleCENode) then
       ModuleCENode.OffsetXY := ExplorerTree.OffsetXY;
  end;
end;

procedure TCodeExplorerWindow.ExplorerTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  ChildCount := Data.CENode.ChildCount;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data : PNodeDataRec;
begin
  if Kind in [ikNormal, ikSelected] then begin
    Data := ExplorerTree.GetNodeData(Node);
    ImageIndex := Data.CENode.ImageIndex;
  end;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data.CENode.Caption;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  HintText := Data.CENode.Hint;
end;

procedure TCodeExplorerWindow.ExplorerTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and (vsSelected in Node.States) and
    mnShowSelection.Checked
  then
   NavigateToNodeElement(Node, True, False);
end;

procedure TCodeExplorerWindow.ExplorerTreeCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data : PNodeDataRec;
begin
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    Data.CENode.Expanded := esCollapsed;
  end;
end;

procedure TCodeExplorerWindow.ExplorerTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  Data : PNodeDataRec;
  Node : PVirtualNode;
  PopUpMenu : TPopupMenu;
  Pos : TPoint;
  HitInfo : THitInfo;
begin
  if (MousePos.X = -1) and (MousePos.Y = -1) then
    // Keyboard invocation
    Node := ExplorerTree.GetFirstSelected
  else begin
    ExplorerTree.GetHitTestInfoAt(MousePos.X, MousePos.Y, True, HitInfo);
    Node := HitInfo.HitNode;
    if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
      Node := nil;
    if Assigned(Node) and not (vsSelected in Node.States) then
      Node := nil;
  end;
  if Assigned(Node) then begin
    PopupMenu := CENodePopUpMenu;
    //UpdatePopupActions;
    Data := ExplorerTree.GetNodeData(Node);
    mnFindDefinition.Enabled := Assigned(Data.CENode.CodeElement) and
      not (Data.CENode is TModuleCENode);
    mnFindReferences.Enabled := mnFindDefinition.Enabled;
    mnHighlight.Enabled := mnFindDefinition.Enabled;
  end else
    PopUpMenu := CEPopupMenu;

  Pos := ExplorerTree.ClientToScreen(MousePos);
  PopUpMenu.Popup(Pos.X, Pos.Y);
  Handled := True;
end;

procedure TCodeExplorerWindow.ExplorerTreeDblClick(Sender: TObject);
begin
  NavigateToNodeElement(ExplorerTree.HotNode)
end;

procedure TCodeExplorerWindow.ExplorerTreeExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data : PNodeDataRec;
begin
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    Data.CENode.Expanded := esExpanded;
  end;
end;

procedure TCodeExplorerWindow.UpdateWindow(UpdateReason : TCEUpdateReason);
begin
  if Visible and Assigned(WorkerThread) then   // Issue 219
    TScanCodeThread(WorkerThread).SetModified;
end;

procedure TCodeExplorerWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteBoolean(BasePath+'\AlphaSort', mnAlphaSort.Checked);
  AppStorage.WriteBoolean(BasePath+'\Show Selection', mnShowSelection.Checked);
  AppStorage.WriteBoolean(BasePath+'\Follow Editor', mnFollowEditor.Checked);
end;

procedure TCodeExplorerWindow.ClearAll;
begin
  ExplorerTree.Clear;
end;

procedure TCodeExplorerWindow.FormDestroy(Sender: TObject);
begin
  inherited;
  ShutDownWorkerThread;  // Calls ClearAll;
end;

procedure TCodeExplorerWindow.ShowEditorCodeElement;
Var
  Editor : IEditor;
  ModuleCENode : TModuleCENode;
  CodeElement : TCodeElementCENode;
begin
  if not mnFollowEditor.Checked then Exit;

  Editor := PyIDEMainForm.GetActiveEditor;
  if not Assigned(Editor) then Exit;

  if (TScanCodeThread(WorkerThread).fOldCEData = Editor.CodeExplorerData) and
    Assigned(TScanCodeThread(WorkerThread).fOldCEData.ModuleNode) and
    (ExplorerTree.RootNodeCount > 0) then
  begin
    ModuleCENode := TScanCodeThread(WorkerThread).fOldCEData.ModuleNode;
    CodeElement := ModuleCENode.GetScopeForLine(Editor.ActiveSynEdit.CaretY);
    if Assigned(CodeElement) and Assigned(CodeElement.fNode) then begin
      ExplorerTree.TreeOptions.AnimationOptions :=
        ExplorerTree.TreeOptions.AnimationOptions - [toAnimatedToggle];
      ExplorerTree.OnChange := nil;
      ExplorerTree.FullyVisible[CodeElement.fNode] := True;
      ExplorerTree.Selected[CodeElement.fNode] := True;
      ExplorerTree.ScrollIntoView(CodeElement.fNode, False);
      ExplorerTree.OnChange := ExplorerTreeChange;
      ExplorerTree.TreeOptions.AnimationOptions :=
        ExplorerTree.TreeOptions.AnimationOptions + [toAnimatedToggle];
    end;
  end;
end;

procedure TCodeExplorerWindow.ShutDownWorkerThread;
begin
  //  Important to Clear here since the destruction of the Worker thread
  //  destroys fOldCodeExplorerData to which tree nodes have pointers.
  ClearAll;
  if WorkerThread <> nil then begin
    TScanCodeThread(WorkerThread).Shutdown;
    TScanCodeThread(WorkerThread).WaitFor;
    FreeAndNil(WorkerThread);
  end;
end;

procedure TCodeExplorerWindow.NavigateToNodeElement(Node: PVirtualNode;
      ForceToMiddle : Boolean = True; Activate : Boolean = True);
var
  Data: PNodeDataRec;
  CodePos : TCodePos;
  L : integer;
  Editor : IEditor;
begin
  CodePos.LineNo := - 1;
  L := 0;

  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Assigned(Data.CENode.CodeElement) then
    begin
      CodePos := Data.CENode.CodeElement.CodePos;
      if not (Data.CENode is TModuleCENode) then
        L := Length(Data.CENode.CodeElement.Name);
    end else if Assigned(ExplorerTree.GetFirstChild(Node)) then begin
        NavigateToNodeElement(Node.FirstChild, ForceToMiddle, Activate);
        Exit;
    end else
      Exit;

    Editor := PyIDEMainForm.GetActiveEditor;
    if Assigned(Editor) and (CodePos.LineNo >= 0) then begin
      with Editor.ActiveSynEdit do
      begin
        CaretXY := BufferCoord(1, CodePos.LineNo);
        EnsureCursorPosVisibleEx(ForceToMiddle);
        if CodePos.CharOffset > 0 then
        begin
          SelStart := RowColToCharIndex(CaretXY) + CodePos.CharOffset - 1;
          SelEnd := SelStart + L;
        end;
      end;
      if Activate then Editor.Activate(False);
    end;
  end;
end;

procedure TCodeExplorerWindow.mnAlphaSortClick(Sender: TObject);
Var
  ModuleCENode : TModuleCENode;
begin
  if Assigned(TScanCodeThread(WorkerThread).fOldCEData) then
     ModuleCENode := TScanCodeThread(WorkerThread).fOldCEData.ModuleNode
  else
    ModuleCENode := nil;

  if Assigned(ModuleCENode) then begin
    if mnAlphaSort.Checked then
      ModuleCENode.Sort(soAlpha)
    else
      ModuleCENode.Sort(soPosition);

    ExplorerTree.BeginUpdate;
    try
      ExplorerTree.ReinitNode(ExplorerTree.RootNode.FirstChild, True);
      ExplorerTree.InvalidateToBottom(ExplorerTree.GetFirstVisible);
    finally
      ExplorerTree.EndUpdate;
    end;
  end;
end;

procedure TCodeExplorerWindow.mnExpandAllClick(Sender: TObject);
begin
  ExplorerTree.FullExpand;
end;

procedure TCodeExplorerWindow.mnFindDefinitionClick(Sender: TObject);
Var
  Node : PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected();
  if Assigned(Node) then
    NavigateToNodeElement(Node);
end;

procedure TCodeExplorerWindow.mnFindReferencesClick(Sender: TObject);
Var
  Node : PVirtualNode;
  Data: PNodeDataRec;
begin
  Node := ExplorerTree.GetFirstSelected();
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Assigned(Data.CENode.CodeElement) and
      not (Data.CENode is TModuleCENode) then
    begin
      NavigateToNodeElement(Node);
      PyIDEMainForm.actFindReferencesExecute(Self);
    end;
  end;
end;

procedure TCodeExplorerWindow.mnFollowEditorClick(Sender: TObject);
begin
  if mnFollowEditor.Checked then
    ShowEditorCodeElement;
end;

procedure TCodeExplorerWindow.mnHighlightClick(Sender: TObject);
Var
  Node : PVirtualNode;
  Data: PNodeDataRec;
begin
  Node := ExplorerTree.GetFirstSelected();
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Assigned(Data.CENode.CodeElement) and not (Data.CENode is TModuleCENode) then
      CommandsDataModule.HighlightWordInActiveEditor(Data.CENode.CodeElement.Name);
  end;
end;

procedure TCodeExplorerWindow.nCollapseAllClick(Sender: TObject);
begin
  ExplorerTree.FullCollapse;
end;

procedure TCodeExplorerWindow.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  mnAlphaSort.Checked := AppStorage.ReadBoolean(BasePath+'\AlphaSort', False);
  mnShowSelection.Checked := AppStorage.ReadBoolean(BasePath+'\Show Selection', True);
  mnFollowEditor.Checked := AppStorage.ReadBoolean(BasePath+'\Follow Editor', True);
end;

{ TAbstractCENode }

procedure TAbstractCENode.AddChild(CENode: TAbstractCENode);
begin
  if fChildren = nil then
    fChildren := TObjectList.Create(True);

  fChildren.Add(CENode);
end;

constructor TAbstractCENode.Create;
begin
  fChildren := nil;
  fExpanded := esUnknown;
end;

function TAbstractCENode.GetChildren(i: integer): TAbstractCENode;
begin
  if Assigned(fChildren) then
    Result := TAbstractCENode(fChildren[i])
  else
    Result := nil;
end;

procedure TAbstractCENode.Sort(SortOrder: TCESortOrder);
Var
  Child : Pointer;
begin
  if not Assigned(fChildren) then Exit;

  for Child in fChildren do
    TAbstractCENode(Child).Sort(SortOrder);

  fChildren.SortList(
     function (Item1, Item2: Pointer): Integer
     Var
       Nd1, Nd2 : TAbstractCENode;
     begin
       Nd1 := TAbstractCENode(Item1);
       Nd2 := TAbstractCENode(Item2);
       if Nd1 is TImportsCENode then
         Result := -1
       else if Nd2 is TImportsCENode then
         Result := 1
       else if Nd1 is TGlobalsCENode then
         Result := -1
       else if Nd2 is TGlobalsCENode then
         Result := 1
       else if Nd1 is TAtrributesCENode then
         Result := -1
       else if Nd2 is TAtrributesCENode then
         Result := 1
       else if Assigned(Nd1.CodeElement) and Assigned(Nd2.CodeElement) then
         if SortOrder = soAlpha then
           Result := ComparePythonIdents(Nd1.CodeElement.Name, Nd2.CodeElement.Name)
         else begin
           Result := Sign(Nd1.CodeElement.CodePos.LineNo -
                            Nd2.CodeElement.CodePos.LineNo);
           if Result = 0 then
             Result := Sign(Nd1.CodeElement.CodePos.CharOffset -
                              Nd2.CodeElement.CodePos.CharOffset);
         end
       else
         Result := 0;
     end);
end;

destructor TAbstractCENode.Destroy;
begin
  FreeAndNil(fChildren);
  inherited;
end;

function TAbstractCENode.GetChildCount: integer;
begin
  if Assigned(fChildren) then
    Result := fChildren.Count
  else
    Result := 0;
end;

{ TModuleCENode }

function TModuleCENode.GetCaption: string;
begin
  Result := ExtractFileName(Module.Name);
end;

function TModuleCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Python);
end;

constructor TModuleCENode.CreateFromModule(AModule: TParsedModule);
Var
  i : integer;
  CE : TCodeElement;
  ClassNode : TClassCENode;
begin
  inherited Create;
  fCodeElement := AModule;
  fExpanded := esExpanded;
  if Module.ImportedModules.Count > 0 then
    AddChild(TImportsCENode.CreateFromModule(Module));
  if Module.Globals.Count > 0 then
    AddChild(TGlobalsCENode.CreateFromModule(Module));
  for i := 0 to Module.ChildCount - 1 do begin
    CE := Module.Children[i];
    if CE is TParsedClass then begin
      ClassNode := TClassCENode.CreateFromClass(TParsedClass(CE));
      if PyIDEOptions.ExporerInitiallyExpanded then
        ClassNode.fExpanded := esExpanded;
      AddChild(ClassNode);
    end else if CE is TParsedFunction then
      AddChild(TFunctionCENode.CreateFromFunction(TParsedFunction(CE)));
  end;
end;

function TModuleCENode.GetHint: string;
begin
  Result := Format('Python Module "%s"', [Module.Name]);
end;

function TModuleCENode.GetParsedModule: TParsedModule;
begin
  Result := fCodeElement as TParsedModule;
end;


{ TImportsCENode }

function TImportsCENode.GetCaption: string;
begin
  Result := 'Imports';
end;

function TImportsCENode.GetImageIndex: integer;
begin
  Result := -1;
end;

constructor TImportsCENode.CreateFromModule(AModule: TParsedModule);
Var
  i : integer;
  SortedImports : TObjectList;
begin
  inherited Create;
  fModule := AModule;
  SortedImports := TObjectList.Create(False);
  try
    fModule.GetSortedImports(SortedImports);
    for i := 0 to SortedImports.Count - 1 do
      AddChild(TImportCENode.CreateFromModuleImport(TModuleImport(SortedImports[i])));
  finally
    SortedImports.Free;
  end;
end;

function TImportsCENode.GetHint: string;
begin
  Result := 'Imported modules';
end;

{ TImportCENode }

function TImportCENode.GetCaption: string;
begin
  Result := ModuleImport.Name;
end;

constructor TImportCENode.CreateFromModuleImport(AModuleImport: TModuleImport);
Var
  i : integer;
begin
  inherited Create;
  fCodeElement := AModuleImport;
  if Assigned(AModuleImport.ImportedNames) then with AModuleImport do
    for i := 0 to ImportedNames.Count - 1 do
      AddChild(TImportNameCENode.CreateFromVariable(ImportedNames[i] as TVariable));
end;

function TImportCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Module);
end;

function TImportCENode.GetHint: string;
begin
  if ModuleImport.RealName <> ModuleImport.Name then
    Result := Format('Imported Module "%s" as %s at line %d',
                    [ModuleImport.RealName, ModuleImport.Name,
                     fCodeElement.CodePos.LineNo])
  else
    Result := Format('Imported Module "%s" at line %d',
                    [ModuleImport.Name, fCodeElement.CodePos.LineNo]);
  if ModuleImport.ImportAll then
    Result := Result + ' (* import)';
end;

function TImportCENode.GetModuleImport: TModuleImport;
begin
  Result := fCodeElement as TModuleImport;
end;

{ TImportNameCENode }

constructor TImportNameCENode.CreateFromVariable(AVariable : TVariable);
begin
  fCodeElement := AVariable;
end;

function TImportNameCENode.GetCaption: string;
begin
  Result := Variable.Name;
end;

function TImportNameCENode.GetHint: string;
begin
  if Variable.RealName = Variable.Name then
    Result := Format('Imported identifier "%s" from module "%s"',
                [Variable.Name,
                (Variable.Parent as TModuleImport).Name])
  else
    Result := Format('Imported identifier "%s" as "%s" from module "%s"',
                [Variable.RealName, Variable.Name,
                (Variable.Parent as TModuleImport).Name]);
end;

function TImportNameCENode.GetImageIndex: integer;
begin
  Result := -1;
end;

function TImportNameCENode.GetVariable: TVariable;
begin
  Result := fCodeElement as TVariable;
end;

{ TGlobalsCENode }

function TGlobalsCENode.GetCaption: string;
begin
  Result := 'Globals';
end;

function TGlobalsCENode.GetImageIndex: integer;
begin
  Result := -1;
end;

constructor TGlobalsCENode.CreateFromModule(AModule: TParsedModule);
Var
  i : integer;
  SortedGlobals : TObjectList;
begin
  inherited Create;
  fModule := AModule;
  SortedGlobals := TObjectList.Create(False);
  try
    fModule.GetUniqueSortedGlobals(SortedGlobals);
    for i := 0 to SortedGlobals.Count - 1 do
      AddChild(TGlobalCENode.CreateFromVariable(TVariable(SortedGlobals[i])));
  finally
    SortedGlobals.Free;
  end;
end;

function TGlobalsCENode.GetHint: string;
begin
  Result := 'Global variables';
end;

{ TVariableCENode }

function TVariableCENode.GetCaption: string;
begin
  Result := Variable.Name;
end;

constructor TVariableCENode.CreateFromVariable(AVariable: TVariable);
begin
  inherited Create;
  fCodeElement := AVariable;
end;

function TVariableCENode.GetVariable: TVariable;
begin
  Result := fCodeElement as TVariable;
end;

{ TGlobalCENode }

function TGlobalCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Variable);
end;

function TGlobalCENode.GetHint: string;
begin
  Result := Format('Global variable "%s" defined at line %d',
                    [Caption, fCodeElement.CodePos.LineNo]);
end;

{ TClassCENode }

function TClassCENode.GetCaption: string;
begin
  Result := ParsedClass.Name;
end;

function TClassCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Klass);
end;

constructor TClassCENode.CreateFromClass(AClass: TParsedClass);
Var
  i : integer;
  CE : TCodeElement;
begin
  inherited Create;
  fCodeElement := AClass;
  if ParsedClass.Attributes.Count > 0 then
    AddChild(TAtrributesCENode.CreateFromClass(ParsedClass));
  for i := 0 to ParsedClass.ChildCount - 1 do begin
    CE := ParsedClass.Children[i];
    if CE is TParsedClass then
      AddChild(TClassCENode.CreateFromClass(TParsedClass(CE)))
    else if CE is TParsedFunction then
      AddChild(TMethodCENode.CreateFromFunction(TParsedFunction(CE)));
  end;
end;

function TClassCENode.GetHint: string;
Var
  FormatString, Doc : string;
begin
  FormatString := 'Class "%s" defined in line %d';
  if ParsedClass.SuperClasses.CommaText <> '' then
    Result := Format(FormatString + #13#10'Inherits from: %s',
              [Caption, fCodeElement.CodePos.LineNo, ParsedClass.SuperClasses.CommaText])
  else
    Result := Format(FormatString , [Caption, fCodeElement.CodePos.LineNo]);
  Doc := ParsedClass.DocString;
  if Doc <> '' then
    Result := Result + #13#10#13#10 + Doc;
end;

function TClassCENode.GetParsedClass: TParsedClass;
begin
  Result := fCodeElement as TParsedClass;
end;

{ TAtrributesCENode }

function TAtrributesCENode.GetCaption: string;
begin
  Result := 'Attributes';
end;

function TAtrributesCENode.GetImageIndex: integer;
begin
  Result := -1;
end;

constructor TAtrributesCENode.CreateFromClass(AClass: TParsedClass);
Var
  i : integer;
  SortedAttributes : TObjectList;
begin
  inherited Create;
  fParsedClass := AClass;
  SortedAttributes := TObjectList.Create(False);
  try
    fParsedClass.GetUniqueSortedAttibutes(SortedAttributes);
    for i := 0 to SortedAttributes.Count - 1 do
      AddChild(TAttributeCENode.CreateFromVariable(TVariable(SortedAttributes[i])));
  finally
    SortedAttributes.Free;
  end;
end;

function TAtrributesCENode.GetHint: string;
begin
  Result := 'Class attributes';
end;

{ TAttributeCENode }

function TAttributeCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Field);
end;

function TAttributeCENode.GetHint: string;
begin
  Result := Format('Class attribute "%s" defined at line %d',
                    [Caption, fCodeElement.CodePos.LineNo]);
end;

{ TFunctionCENode }

constructor TFunctionCENode.CreateFromFunction(AFunction: TParsedFunction);
Var
  i : integer;
  CE : TCodeElement;
begin
  inherited Create;
  fCodeElement := AFunction;
  for i := 0 to ParsedFunction.ChildCount - 1 do begin
    CE := ParsedFunction.Children[i];
    if CE is TParsedClass then begin
      AddChild(TClassCENode.CreateFromClass(TParsedClass(CE)));
    end else if CE is TParsedFunction then
      AddChild(TFunctionCENode.CreateFromFunction(TParsedFunction(CE)));
  end;
end;

function TFunctionCENode.GetCaption: string;
begin
  Result := Format('%s(%s)', [ParsedFunction.Name, ParsedFunction.ArgumentsString]);
end;

function TFunctionCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Func);
end;

function TFunctionCENode.GetHint: string;
Var
  Doc : string;
begin
  Result := Format('Function "%s" defined at line %d'#13#10'Arguments: %s',
              [Caption, fCodeElement.CodePos.LineNo, ParsedFunction.ArgumentsString]);
  Doc := ParsedFunction.DocString;
  if Doc <> '' then
    Result := Result + #13#10#13#10 + Doc;
end;

function TFunctionCENode.GetParsedFunction: TParsedFunction;
begin
  Result := fCodeElement as TParsedFunction;
end;

{ TMethodCENode }

function TMethodCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Method);
end;

function TMethodCENode.GetHint: string;
Var
  Doc : string;
begin
  Result := Format('Method %s defined at line %d'#13#10'Arguments: %s',
              [Caption, fCodeElement.CodePos.LineNo, ParsedFunction.ArgumentsString]);
  Doc := ParsedFunction.DocString;
  if Doc <> '' then
    Result := Result + #13#10#13#10 + Doc;
end;

{ TCodeExplorerData }

destructor TCodeExplorerData.Destroy;
begin
  fSourceScanner := nil;
  FreeAndNil(fModuleNode);
  inherited;
end;

function TCodeExplorerData.GetModuleNode: TModuleCENode;
begin
  Result := fModuleNode;
end;

function TCodeExplorerData.GetNewSourceScanner: IAsyncSourceScanner;
begin
  Result := fNewSourceScanner;
end;

function TCodeExplorerData.GetSourceScanner: IAsyncSourceScanner;
begin
  Result := fSourceScanner;
end;

procedure TCodeExplorerData.SetModuleNode(AModuleNode: TModuleCENode);
begin
  FreeAndNil(fModuleNode);
  fModuleNode := AModuleNode;
end;

procedure TCodeExplorerData.SetNewSourceScanner(SC: IAsyncSourceScanner);
begin
  fNewSourceScanner := SC;
end;

procedure TCodeExplorerData.SetSourceScanner(SC: IAsyncSourceScanner);
begin
  fSourceScanner := SC;
end;

{ TCodeElementCENode }

function TCodeElementCENode.GetCodeBlock: TCodeBlock;
begin
  if Assigned(CodeElement) and (CodeElement is TCodeElement) then
    Result := TCodeElement(CodeElement).CodeBlock
  else
    Result := cPythonSourceScanner.CodeBlock(0, 0);
end;

function TCodeElementCENode.GetScopeForLine(LineNo: integer): TCodeElementCENode;
// similar to TCodeElement.GetScopeForLine in cPythonSourceScanner
Var
  i : integer;
  Node : TAbstractCENode;
begin
  if (LineNo >= CodeBlock.StartLine) and (LineNo <= CodeBlock.EndLine) then begin
    Result := Self;
    //  try to see whether the line belongs to a child
    if not Assigned(fChildren) then Exit;
    for i := 0 to fChildren.Count - 1 do begin
      Node := Children[i];
      if not (Node is TCodeElementCENode) then continue;

      if (LineNo >= TCodeElementCENode(Node).CodeBlock.StartLine) and
          (LineNo <= TCodeElementCENode(Node).CodeBlock.EndLine)
      then begin
        // recursive call
        Result := TCodeElementCENode(Node).GetScopeForLine(LineNo);
        break;
      end;
    end;
  end else
    Result := nil;
end;

end.

