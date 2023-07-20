{-----------------------------------------------------------------------------
 Unit Name: frmCodeExplorer
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Code Explorer Window
 History:
-----------------------------------------------------------------------------}

unit frmCodeExplorer;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Contnrs,
  System.ImageList,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees,
  TB2Item,
  SpTBXItem,
  SpTBXControls,
  frmIDEDockWin,
  JediLspClient,
  SynEditTypes,
  SynEditLsp;

type
  TCESortOrder = (soPosition, soAlpha);
  TCEExpandState = (esUnknown, esExpanded, esCollapsed);

  TCodeBlock = record
    StartLine : integer;
    EndLine : integer;
  end;

  TAbstractCENodeClass = class of TAbstractCENode;
  TAbstractCENode = class
  private
    FChildren : TObjectList<TAbstractCENode>;
    function GetChildCount: Integer;
    function GetChildren(I: Integer): TAbstractCENode;
    class var SortOrder: TCESortOrder;
    class var NodeComparer: IComparer<TAbstractCENode>;
    class var FileName: string;
  protected
    FName: string;
    FCodePos: TBufferCoord;
    FExpanded : TCEExpandState;
    FNode : PVirtualNode;
    function GetHint: string; virtual;
    function GetCaption: string; virtual;
    function GetImageIndex : integer; virtual;
  public
    class constructor Create;
    constructor CreateFromSymbol(Symbol: TJsonObject); virtual;
    destructor Destroy; override;
    function AddChild(CENode: TAbstractCENode): Integer;
    procedure Sort(ASortOrder: TCESortOrder);
    property Name: string read FName;
    property CodePos: TBufferCoord read FCodePos;
    property Hint : string read GetHint;
    property Caption : string read GetCaption;
    property ImageIndex : integer read GetImageIndex;
    property ChildCount : integer read GetChildCount;
    property Children[I : integer] : TAbstractCENode read GetChildren;
    property Expanded : TCEExpandState read fExpanded write fExpanded;
  end;

  TGroupCENode = class abstract(TAbstractCENode)
  end;

  TCodeElementCENode = class(TAbstractCENode)
  private
    FCodeBlock: TCodeBlock;
  protected
    function GetHint: string; override;
  public
    constructor CreateFromSymbol(Symbol: TJsonObject); override;
    function GetScopeForLine(LineNo: integer): TCodeElementCENode;
    property CodeBlock : TCodeBlock read FCodeBlock;
  end;

  TImportCENode = class(TAbstractCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TImportsCENode = class(TGroupCENode)
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
  end;

  TGlobalsCENode = class(TGroupCENode)
    function GetHint: string; override;
    function GetCaption: string; override;
  end;

  TModuleCENode = class(TCodeElementCENode)
  private
    FOffsetXY: TPoint;
    FImports: TImportsCENode;
    FGlobals: TGlobalsCENode;
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromSymbols(const AFileName: string; Symbols: TJsonArray);
    property OffsetXY : TPoint read FOffsetXY write FOffsetXY;
    property Imports: TImportsCENode read FImports;
    property Globals: TGlobalsCENode read FGlobals;
  end;

  TVariableCENode = class(TAbstractCENode)
  end;

  TGlobalCENode = class(TVariableCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TAttributesCENode = class(TGroupCENode)
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
  end;

  TClassCENode = class(TCodeElementCENode)
  private
    FAttributes: TAttributesCENode;
  protected
    //function GetHint: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromSymbol(Symbol: TJsonObject); override;
  end;

  TAttributeCENode = class(TVariableCENode)
  protected
    function GetHint: string; override;
    function GetImageIndex : integer; override;
  end;

  TFunctionCENode = class(TCodeElementCENode)
  protected
    function GetImageIndex : integer; override;
  public
    constructor CreateFromSymbol(Symbol: TJsonObject); override;
  end;

  TMethodCENode = class(TFunctionCENode)
  protected
    function GetImageIndex : integer; override;
  end;

  TCEUpdateReason = (ceuSymbolsChanged, ceuEditorEnter);

  TCodeExplorerWindow = class(TIDEDockWindow)
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
    vilCodeImages: TVirtualImageList;
    vilImages: TVirtualImageList;
    procedure ExplorerTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure FormCreate(Sender: TObject);
    procedure ExplorerTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ExplorerTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
    procedure ExplorerTreeGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
  private
    const FBasePath = 'Code Explorer Options'; // Used for storing settings
    var FFileId: string;
    FModuleNode: TModuleCENode;
    procedure NavigateToNodeElement(Node: PVirtualNode;
      ForceToMiddle : Boolean = True; Activate : Boolean = True);
    procedure UpdateModuleNode(const FileId: string; Symbols: TJsonArray);
    procedure UpdateTree(const FileId: string;
      UpdateReason: TCEUpdateReason; NewModuleNode: TModuleCENode);
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure ClearAll;
    procedure ShowEditorCodeElement;
    procedure UpdateWindow(DocSymbols: TDocSymbols; UpdateReason: TCEUpdateReason);
  end;

var
  CodeExplorerWindow: TCodeExplorerWindow;

implementation

uses
  System.Math,
  System.IOUtils,
  System.Threading,
  JvGNUGetText,
  SynEdit,
  dmResources,
  dmCommands,
  frmPyIDEMain,
  uEditAppIntfs,
  uCommonFunctions,
  LspUtils,
  cPyScripterSettings;

{$R *.dfm}

procedure TCodeExplorerWindow.UpdateTree(const FileId: string;
      UpdateReason: TCEUpdateReason; NewModuleNode: TModuleCENode);
begin
  if GI_PyIDEServices.IsClosing then Exit;

  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if ActiveEditor = nil then
  begin
    ExplorerTree.Clear;
    FFileId := '';
    FModuleNode := nil;
    Exit;
  end;

  var ShowingActiveEditor := SameFileName(ActiveEditor.FileId, FFileId);
  if not ShowingActiveEditor then
    FFileId := ActiveEditor.FileId;

  var Editor: IEditor;
  if SameFileName(FFileId, FileId) then
    Editor := ActiveEditor
  else
    Editor := GI_EditorFactory.GetEditorByFileId(FileId);

  var SameModule := ShowingActiveEditor and (Editor = ActiveEditor);

  if UpdateReason = ceuSymbolsChanged then
  begin
    if Assigned(Editor) then
    begin
      FreeAndNil(TDocSymbols(Editor.DocSymbols).ModuleNode);
      TDocSymbols(Editor.DocSymbols).ModuleNode := NewModuleNode;
    end
    else
    begin
      // Editor must have been destroyed
      NewModuleNode.Free;
      Exit;
    end;

    if ShowingActiveEditor and (ActiveEditor <> Editor) then
      // A non active editor's DocSymbols have been updated
      Exit;
  end;
  FModuleNode := TDocSymbols(ActiveEditor.DocSymbols).ModuleNode as TModuleCENode;

  // Turn off Animation to speed things up
  ExplorerTree.TreeOptions.AnimationOptions :=
    ExplorerTree.TreeOptions.AnimationOptions - [toAnimatedToggle];

  if SameModule then
  begin
    if FModuleNode = nil then
     ExplorerTree.Clear
    else
    begin
      ExplorerTree.RootNodeCount := 1;
      // The same module but changed
      // ReInit the tree with the new data to keep it as close as possible
      if mnAlphaSort.Checked then
        TModuleCENode(FModuleNode).Sort(soAlpha);
      ExplorerTree.BeginUpdate;
      try
        ExplorerTree.ReinitNode(nil, True, True);
        ExplorerTree.Invalidate;
      finally
        ExplorerTree.EndUpdate;
      end;
    end;
  end
  else
  begin
    ExplorerTree.Clear;
    if FModuleNode <> nil then
    begin
      ExplorerTree.RootNodeCount := 1;
      ExplorerTree.OffsetXY := FModuleNode.OffsetXY;
      ExplorerTree.ValidateNode(nil, True);
      //ExplorerTree.Refresh;
    end;
  end;
  ExplorerTree.TreeOptions.AnimationOptions :=
    ExplorerTree.TreeOptions.AnimationOptions + [toAnimatedToggle];
  ShowEditorCodeElement;
end;

procedure TCodeExplorerWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if ExplorerTree.CanFocus then
    ExplorerTree.SetFocus;
end;

procedure TCodeExplorerWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'CodeExplorer';
  inherited;
  // Let the tree know how much data space we need.
  ExplorerTree.NodeDataSize := SizeOf(Pointer);
end;

procedure TCodeExplorerWindow.ExplorerTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CENode: TAbstractCENode;
begin
  if ParentNode = nil then
    CENode := FModuleNode
  else begin
    var ParentCENode := ParentNode.GetData<TAbstractCENode>;
    CENode := ParentCENode.Children[Node.Index];
  end;
  Node.SetData<TAbstractCENode>(CENode);

  if CENode.ChildCount > 0 then
  begin
    Include(InitialStates, ivsHasChildren);
    if (ivsReinit in InitialStates) then
    begin
      if vsExpanded in Node.States then
        CENode.Expanded := esExpanded
      else
        CENode.Expanded := esCollapsed;
    end
    else if CENode.Expanded = esExpanded then
      Include(InitialStates, ivsExpanded);
  end;
  // reverse link from CENode to Tree node
  CENode.fNode := Node;
end;

procedure TCodeExplorerWindow.ExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    NavigateToNodeElement(ExplorerTree.GetFirstSelected);
end;

procedure TCodeExplorerWindow.ExplorerTreeScroll(Sender: TBaseVirtualTree;
  DeltaX, DeltaY: Integer);
begin
  if Assigned(FModuleNode) then
    TModuleCENode(FModuleNode).OffsetXY := ExplorerTree.OffsetXY;
end;

procedure TCodeExplorerWindow.ExplorerTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := Node.GetData<TAbstractCENode>.ChildCount;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if Kind in [ikNormal, ikSelected] then
    ImageIndex := Node.GetData<TAbstractCENode>.ImageIndex;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
begin
  E.CellText := E.Node.GetData<TAbstractCENode>.Caption;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  HintText := Node.GetData<TAbstractCENode>.Hint;
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
begin
  if Assigned(Node) then
    Node.GetData<TAbstractCENode>.Expanded := esCollapsed;
end;

procedure TCodeExplorerWindow.ExplorerTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  Node: PVirtualNode;
  CENode: TAbstractCENode;
  PopUpMenu: TPopupMenu;
  Pos: TPoint;
  HitInfo: THitInfo;
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
    CENode := Node.GetData<TAbstractCENode>;
    mnFindDefinition.Enabled :=
      (CENode is TCodeElementCENode) and not (CENode is TModuleCENode) or
      (CENode is TVariableCENode) or (CENode is TImportCENode);
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
begin
  if Assigned(Node) then
    Node.GetData<TAbstractCENode>.Expanded := esExpanded;
end;

procedure TCodeExplorerWindow.UpdateModuleNode(const FileId: string; Symbols: TJsonArray);
var
  ModuleNode: TModuleCENode;
begin
    if Assigned(Symbols) then
    begin
      ModuleNode := TModuleCENode.CreateFromSymbols(FileId, Symbols);
      Symbols.Free;
    end
    else
      ModuleNode := nil;
    TThread.ForceQueue(nil, procedure
      begin
        UpdateTree(FileId, ceuSymbolsChanged, ModuleNode);
      end);
end;

procedure TCodeExplorerWindow.UpdateWindow(DocSymbols: TDocSymbols;
  UpdateReason: TCEUpdateReason);
Var
  Symbols: TJsonArray;
  LFileId: string;
begin
  Assert(Assigned(DocSymbols));
  LFileId := DocSymbols.FileId;
  case UpdateReason of
    ceuSymbolsChanged:
      begin
        DocSymbols.Lock;
        try
          if DocSymbols.Symbols = nil then
          begin
            Assert(GetCurrentThreadId = MainThreadId);
            if FFileId = LFileId then
            begin
              ExplorerTree.Clear;
              FModuleNode := nil;
            end;
            FreeAndNil(DocSymbols.ModuleNode);
          end
          else
          begin
            // Called from a thread <> MainThread
            Symbols := DocSymbols.Symbols; // Will destroyed in UpdateModuleNode
            DocSymbols.Symbols := nil;
            var Task := TTask.Create(procedure
              begin
                UpdateModuleNode(LFileId, Symbols);
              end);
            Task.Start;
          end;
        finally
          DocSymbols.Unlock;
        end;
      end;
    ceuEditorEnter:
      begin
        TThread.ForceQueue(nil, procedure
          begin
            if not GI_PyIDEServices.IsClosing then
              UpdateTree(LFileId, ceuEditorEnter, nil);
          end);
      end;
  end;
end;

procedure TCodeExplorerWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteBoolean(FBasePath+'\AlphaSort', mnAlphaSort.Checked);
  AppStorage.WriteBoolean(FBasePath+'\Show Selection', mnShowSelection.Checked);
  AppStorage.WriteBoolean(FBasePath+'\Follow Editor', mnFollowEditor.Checked);
end;

procedure TCodeExplorerWindow.ClearAll;
begin
  ExplorerTree.Clear;
end;

procedure TCodeExplorerWindow.ShowEditorCodeElement;
Var
  Editor : IEditor;
  CodeElement : TCodeElementCENode;
begin
  if not mnFollowEditor.Checked then Exit;

  Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Editor) then Exit;

  if Assigned(FModuleNode) and (FFileId = Editor.FileId) and
    (ExplorerTree.RootNodeCount > 0)
  then
  begin
    CodeElement := TModuleCENode(FModuleNode).
      GetScopeForLine(Editor.ActiveSynEdit.CaretY);
    if Assigned(CodeElement) and Assigned(CodeElement.fNode) then
    begin
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

procedure TCodeExplorerWindow.NavigateToNodeElement(Node: PVirtualNode;
      ForceToMiddle : Boolean = True; Activate : Boolean = True);
var
  CENode: TAbstractCENode;
  CodePos : TBufferCoord;
  L : integer;
  Editor : IEditor;
begin
  CodePos.Line := - 1;
  L := 0;

  if Assigned(Node) then
  begin
    CENode := Node.GetData<TAbstractCENode>;
    if not (CENode is TGroupCENode) then
    begin
      CodePos := CENode.CodePos;
      if not (CENode is TModuleCENode) then
        L := Length(CENode.Name);
    end else if Assigned(ExplorerTree.GetFirstChild(Node)) then begin
        NavigateToNodeElement(Node.FirstChild, ForceToMiddle, Activate);
        Exit;
    end else
      Exit;

    Editor := GI_PyIDEServices.ActiveEditor;
    if Assigned(Editor) and (Editor.FileId = FFileId) and (CodePos.Line >= 0) then
    begin
      with Editor.ActiveSynEdit do
      begin
        CaretXY := CodePos;
        EnsureCursorPosVisibleEx(ForceToMiddle);
        if L > 0 then
          BlockEnd := BufferCoord(BlockBegin.Char + L, BlockBegin.Line);
      end;
      if Activate then Editor.Activate(False);
    end;
  end;
end;

procedure TCodeExplorerWindow.mnAlphaSortClick(Sender: TObject);
begin
  if Assigned(FModuleNode) then begin
    if mnAlphaSort.Checked then
      FModuleNode.Sort(soAlpha)
    else
      FModuleNode.Sort(soPosition);

    ExplorerTree.BeginUpdate;
    try
      ExplorerTree.ReinitNode(nil, True, True);
      ExplorerTree.Invalidate;
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
begin
  var Node := ExplorerTree.GetFirstSelected();
  if Assigned(Node) then begin
    var CENode := Node.GetData<TAbstractCENode>;
    if (CENode is TCodeElementCENode) and not (CENode is TModuleCENode) or
       (CENode is TVariableCENode) or (CENode is TImportCENode) then
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
begin
  var Node := ExplorerTree.GetFirstSelected();
  if Assigned(Node) then begin
    var CENode := Node.GetData<TAbstractCENode>;
    if (CENode is TCodeElementCENode) and not (CENode is TModuleCENode) or
       (CENode is TVariableCENode) or (CENode is TImportCENode)
    then
      CommandsDataModule.HighlightWordInActiveEditor(CENode.Name);
  end;
end;

procedure TCodeExplorerWindow.nCollapseAllClick(Sender: TObject);
begin
  ExplorerTree.FullCollapse;
end;

procedure TCodeExplorerWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  mnAlphaSort.Checked := AppStorage.ReadBoolean(FBasePath+'\AlphaSort', False);
  mnShowSelection.Checked := AppStorage.ReadBoolean(FBasePath+'\Show Selection', True);
  mnFollowEditor.Checked := AppStorage.ReadBoolean(FBasePath+'\Follow Editor', True);
end;

{ TAbstractCENode }

function TAbstractCENode.AddChild(CENode: TAbstractCENode): Integer;
begin
  if fChildren = nil then
    fChildren := TObjectList<TAbstractCENode>.Create(True);

  Result := fChildren.Add(CENode);
end;

class constructor TAbstractCENode.Create;
begin
  NodeComparer := TComparer<TAbstractCENode>.Construct(
     function(const Nd1, Nd2: TAbstractCENode): Integer
     begin
       if Nd1 is TImportsCENode then
         Result := -1
       else if Nd2 is TImportsCENode then
         Result := 1
       else if Nd1 is TGlobalsCENode then
         Result := -1
       else if Nd2 is TGlobalsCENode then
         Result := 1
       else if Nd1 is TAttributesCENode then
         Result := -1
       else if Nd2 is TAttributesCENode then
         Result := 1
       else
       begin
         if SortOrder = soAlpha then
           Result := ComparePythonIdents(Nd1.Name, Nd2.Name)
         else begin
           Result := Sign(Nd1.CodePos.Line -
                            Nd2.CodePos.Line);
           if Result = 0 then
             Result := Sign(Nd1.CodePos.Char -
                              Nd2.CodePos.Char);
         end;
       end;
     end);

end;

constructor TAbstractCENode.CreateFromSymbol(Symbol: TJsonObject);
begin
  inherited Create;
  FName := Symbol.GetValue<string>('name', '');
  FCodePos.Line := Symbol.GetValue<integer>('selectionRange.start.line', 0);
  Inc(FCodePos.Line);
  FCodePos.Char := Symbol.GetValue<integer>('selectionRange.start.character', 0);
  Inc(FCodePos.Char);
end;

function TAbstractCENode.GetChildren(i: integer): TAbstractCENode;
begin
  if Assigned(fChildren) then
    Result := fChildren[i]
  else
    Result := nil;
end;

function TAbstractCENode.GetHint: string;
begin
  Result := '';
end;

function TAbstractCENode.GetImageIndex: integer;
begin
  Result := -1;
end;

procedure TAbstractCENode.Sort(ASortOrder: TCESortOrder);
Var
  Child : Pointer;
begin
  SortOrder := ASortOrder;
  if not Assigned(fChildren) then Exit;

  for Child in fChildren do
    TAbstractCENode(Child).Sort(SortOrder);

  fChildren.Sort(NodeComparer);
end;

destructor TAbstractCENode.Destroy;
begin
  FreeAndNil(fChildren);
  inherited;
end;

function TAbstractCENode.GetCaption: string;
begin
  Result := FName;
end;

function TAbstractCENode.GetChildCount: integer;
begin
  if Assigned(fChildren) then
    Result := fChildren.Count
  else
    Result := 0;
end;

{ TModuleCENode }

function TModuleCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Python);
end;

constructor TModuleCENode.CreateFromSymbols(const AFileName: string; Symbols: TJsonArray);
var
  Kind: Integer;
  NodeClass: TAbstractCENodeClass;
  Node: TAbstractCENode;
begin
  inherited Create;
  FileName := AFileName;
  FName := TPath.GetFileName(AFileName);
  fExpanded := esExpanded;
  FCodePos := BufferCoord(1, 1);
  FCodeBlock.StartLine := 1;
  FCodeBlock.EndLine := MaxInt;

  for var Symbol in Symbols do
  begin
    if not Symbol.TryGetValue<integer>('kind', Kind) then
      Continue;

    case TSymbolKind(Kind) of
      TSymbolKind.Module: NodeClass := TImportCENode;
      TSymbolKind._Class: NodeClass := TClassCENode;
      TSymbolKind._Function: NodeClass := TFunctionCENode;
      TSymbolKind._Variable: NodeClass := TGlobalCENode;
    else
      Continue;
    end;
    try
      Node := NodeClass.CreateFromSymbol(Symbol as TJsonObject);
    except
      Continue;
    end;
    if NodeClass = TImportCENode then
    begin
      if FImports = nil then
      begin
        FImports := TImportsCENode.Create;
        var Index := AddChild(FImports);
        FChildren.Move(Index, 0);
      end;
      FImports.AddChild(Node);
    end
    else if NodeClass = TGlobalCENode then
    begin
      if FGlobals = nil then
      begin
        FGlobals := TGlobalsCENode.Create;
        var Index := AddChild(FGlobals);
        if FImports <> nil then
          FChildren.Move(Index, 1)
        else
          FChildren.Move(Index, 0);
      end;
      FGlobals.AddChild(Node);
    end
    else
      AddChild(Node);
  end;
end;

function TModuleCENode.GetHint: string;
begin
  Result := Format(_('Python Module "%s"'), [Name]);
end;

{ TImportsCENode }

function TImportsCENode.GetCaption: string;
begin
  Result := _('Imports');
end;

function TImportsCENode.GetHint: string;
begin
  Result := _('Imported modules');
end;

{ TImportCENode }

function TImportCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Module);
end;

function TImportCENode.GetHint: string;
begin
//  if ModuleImport.RealName <> ModuleImport.Name then
//    Result := Format(_('Imported Module "%s" as %s at line %d'),
//                    [ModuleImport.RealName, ModuleImport.Name,
//                     fCodeElement.CodePos.LineNo])
//  else
    Result := Format(_('Imported Module "%s" at line %d'),
                    [Name, CodePos.Line]);
end;

{ TGlobalsCENode }

function TGlobalsCENode.GetCaption: string;
begin
  Result := 'Globals';
end;

function TGlobalsCENode.GetHint: string;
begin
  Result := _('Global variables');
end;

{ TGlobalCENode }

function TGlobalCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Variable);
end;

function TGlobalCENode.GetHint: string;
begin
  Result := Format(_('Global variable "%s" defined at line %d'),
                    [Caption, CodePos.Line]);
end;

{ TClassCENode }

function TClassCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Klass);
end;

constructor TClassCENode.CreateFromSymbol(Symbol: TJsonObject);
var
  Kind: Integer;
  Symbols: TJsonValue;
  NodeClass: TAbstractCENodeClass;
  Node: TAbstractCENode;
begin
  inherited;

  Symbol.TryGetValue('children', Symbols);
  if not (Symbols is TJsonArray) then Exit;

  for var CE in TJsonArray(Symbols) do
  begin
    if not CE.TryGetValue<integer>('kind', Kind) then
      Continue;

    case TSymbolKind(Kind) of
      TSymbolKind._Class: NodeClass := TClassCENode;
      TSymbolKind.Method: NodeClass := TMethodCENode;
      TSymbolKind._Function: NodeClass := TFunctionCENode;
      TSymbolKind._Property,
      TSymbolKind._Variable: NodeClass := TAttributeCENode;
    else
      Continue;
    end;
    try
      Node := NodeClass.CreateFromSymbol(CE as TJsonObject);
    except
      Continue;
    end;
    if NodeClass = TAttributeCENode then
    begin
      if FAttributes = nil then
      begin
        FAttributes := TAttributesCENode.Create;
        var Index := AddChild(FAttributes);
        FChildren.Move(Index, 0);
      end;
      FAttributes.AddChild(Node);
    end
    else
      AddChild(Node);
  end;
  if PyIDEOptions.ExplorerInitiallyExpanded and (ChildCount > 0) then
   fExpanded := esExpanded;
end;

{ TAttributesCENode }

function TAttributesCENode.GetCaption: string;
begin
  Result := _('Attributes');
end;

function TAttributesCENode.GetHint: string;
begin
  Result := _('Class attributes');
end;

{ TAttributeCENode }

function TAttributeCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Field);
end;

function TAttributeCENode.GetHint: string;
begin
  Result := Format(_('Class attribute "%s" defined at line %d'),
                    [Caption, CodePos.Line]);
end;

{ TFunctionCENode }

constructor TFunctionCENode.CreateFromSymbol(Symbol: TJsonObject);
var
  Kind: Integer;
  Symbols: TJsonValue;
  NodeClass: TAbstractCENodeClass;
  Node: TAbstractCENode;
begin
  inherited;

  Symbol.TryGetValue('children', Symbols);
  if not (Symbols is TJsonArray) then Exit;

  for var CE in TJsonArray(Symbols) do
  begin
    if not CE.TryGetValue<integer>('kind', Kind) then
      Continue;

    case TSymbolKind(Kind) of
      TSymbolKind._Class: NodeClass := TClassCENode;
      TSymbolKind._Function: NodeClass := TFunctionCENode;
    else
      Continue;
    end;
    try
      Node := NodeClass.CreateFromSymbol(CE as TJsonObject);
    except
      Continue;
    end;
    AddChild(Node);
  end;
end;

function TFunctionCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Func);
end;

{ TMethodCENode }

function TMethodCENode.GetImageIndex: integer;
begin
  Result := Integer(TCodeImages.Method);
end;

{ TCodeElementCENode }

constructor TCodeElementCENode.CreateFromSymbol(Symbol: TJsonObject);
begin
  inherited CreateFromSymbol(Symbol);
  FCodeBlock.StartLine := Symbol.GetValue<integer>('range.start.line', 0);
  Inc(FCodeBlock.StartLine);
  FCodeBlock.EndLine := Symbol.GetValue<integer>('range.end.line', 0);
  Inc(FCodeBlock.EndLine);
end;

function TCodeElementCENode.GetScopeForLine(LineNo: integer): TCodeElementCENode;
begin
  if InRange(LineNo, CodeBlock.StartLine, CodeBlock.EndLine) then begin
    Result := Self;
    //  try to see whether the line belongs to a child
    if not Assigned(fChildren) then Exit;
    for var I := 0 to fChildren.Count - 1 do begin
      var Node := Children[I];
      if Node is TCodeElementCENode then
      begin
        var CENode := TCodeElementCENode(Node);
        // recursive call
        CENode := CENode.GetScopeForLine(LineNo);
        if Assigned(CENode) then
        begin
          Result := CENode;
          Break;
        end;
      end;
    end;
  end else
    Result := nil;
end;

function TCodeElementCENode.GetHint: string;
begin
  Result := TJedi.SimpleHintAtCoordinates(FileName, CodePos);
end;


end.

