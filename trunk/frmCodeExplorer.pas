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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, Buttons, JvDockControlForm,
  JvComponent, Menus, Contnrs, VirtualTrees, frmIDEDockWin, TB2Item,
  cPythonSourceScanner, JvComponentBase, SpTBXItem, SpTBXSkins;

type
  TAbstractCENode = class
  private
    fChildren : TObjectList;
    function GetChildCount: integer;
    function GetChildren(i : integer): TAbstractCENode;
  protected
    fCodeElement : TBaseCodeElement;
    fInitiallyExpanded : boolean;
    function GetHint: string; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetImageIndex : integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(CENode : TAbstractCENode);
    property CodeElement : TBaseCodeElement read fCodeElement;
    property Hint : string read GetHint;
    property Caption : string read GetCaption;
    property ImageIndex : integer read GetImageIndex;
    property ChildCount : integer read GetChildCount;
    property Children[i : integer] : TAbstractCENode read GetChildren;
    property InitiallyExpanded : boolean read fInitiallyExpanded;
  end;

  TModuleCENode = class(TAbstractCENode)
  private
    function GetParsedModule: TParsedModule;
  protected
    function GetHint: string; override;
    function GetCaption: string; override;
    function GetImageIndex : integer; override;
  public
    constructor CreateFromModule(AModule : TParsedModule);
    property Module : TParsedModule read GetParsedModule;
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

  TClassCENode = class(TAbstractCENode)
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

  TFunctionCENode = class(TAbstractCENode)
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

  TCodeExplorerWindow = class(TIDEDockWindow)
    Panel1: TPanel;
    ExplorerTree: TVirtualStringTree;
    TreePopupMenu: TSpTBXPopupMenu;
    mnExpandAll: TSpTBXItem;
    nCollapseAll: TSpTBXItem;
    procedure ExplorerTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure FormCreate(Sender: TObject);
    procedure ExplorerTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ExplorerTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ExplorerTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormDestroy(Sender: TObject);
    procedure ExplorerTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure ExplorerTreeDblClick(Sender: TObject);
    procedure mnExpandAllClick(Sender: TObject);
    procedure nCollapseAllClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ExplorerTreeKeyPress(Sender: TObject; var Key: Char);
  private
    procedure NavigateToNodeElement(Node: PVirtualNode);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    { Public declarations }
    ModuleCENode : TModuleCENode;
    WorkerThread: TThread;
    procedure ClearAll;
    procedure UpdateWindow;
    procedure ShutDownWorkerThread;
  end;

var
  CodeExplorerWindow: TCodeExplorerWindow;

implementation

uses frmPyIDEMain, dmCommands, uEditAppIntfs, SynEdit, SynRegExpr,
  SynEditTypes, JclFileUtils, JvDockGlobals;

{$R *.dfm}

type
  TScanCodeThread = class(TThread)
  private
    fOldModule : TParsedModule;
    fNewModule : TParsedModule;
    fScanEventHandle: THandle;
    fSource: string;
    fModuleName : string;
    fModuleFileName : string;
    fSourceChanged: boolean;
    fPythonScanner : TPythonScanner;
    procedure GetSource;
    procedure SetResults;
    procedure ScanProgress(CharNo, NoOfChars : integer; var Stop : Boolean);
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
  fOldModule := TParsedModule.Create;
  fNewModule := TParsedModule.Create;
  fPythonScanner := TPythonScanner.Create();
  fPythonScanner.OnScannerProgress := ScanProgress;
  Priority := tpLowest;
  fScanEventHandle := CreateEvent(nil, FALSE, FALSE, nil);
  if (fScanEventHandle = 0) or (fScanEventHandle = INVALID_HANDLE_VALUE) then
    raise EOutOfResources.Create('Couldn''t create WIN32 event object');
  //Resume;  It is resumed from the main form
end;

destructor TScanCodeThread.Destroy;
begin
  fOldModule.Free;
  fNewModule.Free;
  fPythonScanner.Free;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(fScanEventHandle);
  inherited Destroy;
end;

procedure TScanCodeThread.Execute;
begin
  while not Terminated do begin
    WaitForSingleObject(fScanEventHandle, INFINITE);
    repeat
      // make sure the event is reset when we are still in the repeat loop
      ResetEvent(fScanEventHandle);
      if Terminated then
        break;
      // get the modified source and set fSourceChanged to 0
      Synchronize(GetSource);
      if Terminated then
        break;
      // clear
      fNewModule.Clear;
      fNewModule.Name := fModuleName;
      fNewModule.FileName := fModuleFileName;
      // scan the source text for the keywords, cancel if the source in the
      // editor has been changed again
      if fSource <> '' then
        fPythonScanner.ScanModule(fSource, fNewModule);
    until not fSourceChanged or Terminated;

    if Terminated then
      break;
    // source was changed while scanning
    if fSourceChanged then begin
      //Sleep(100);
      if not Terminated then continue;
    end;

    if not Terminated then
      Synchronize(SetResults)
      // and go to sleep again
    else
      break;
  end;
end;

procedure TScanCodeThread.GetSource;
var
  Editor : IEditor;
begin
  fModuleName := '';
  fModuleFileName := '';
  fSource := '';
  Editor := PyIDEMainForm.GetActiveEditor;
  if Assigned(Editor) and Editor.HasPythonFile then
  begin
    fModuleName := PathRemoveExtension(Editor.FileTitle);
    fModuleFileName := Editor.FileName;
    fSource := Editor.SynEdit.Text;
  end;
  fSourceChanged := FALSE;
end;

procedure TScanCodeThread.ScanProgress(CharNo, NoOfChars : integer;
  var Stop: Boolean);
begin
  Stop := Terminated or fSourceChanged;
end;

procedure TScanCodeThread.SetModified;
begin
  if Terminated or (csDestroying in CodeExplorerWindow.ComponentState) then Exit;
  fSourceChanged := True;
  if (fScanEventHandle <> 0) and (fScanEventHandle <> INVALID_HANDLE_VALUE) then
    SetEvent(fScanEventHandle);
end;

procedure TScanCodeThread.SetResults;
Var
  SameModule : Boolean;
begin
  if Terminated or (csDestroying in CodeExplorerWindow.ComponentState) then Exit;

  SameModule := (fOldModule.Name = fNewModule.Name) and
               (fOldModule.FileName = fNewModule.FileName) and
               (CodeExplorerWindow.ExplorerTree.RootNodeCount > 0);

  fOldModule.Free;
  fOldModule := fNewModule;
  fNewModule := TParsedModule.Create;
  with CodeExplorerWindow do begin
    FreeAndNil(ModuleCENode);
    ModuleCENode := TModuleCENode.CreateFromModule(fOldModule);
    // Turn off Animation to speed things up
    ExplorerTree.TreeOptions.AnimationOptions :=
      ExplorerTree.TreeOptions.AnimationOptions - [toAnimatedToggle];
    if SameModule then begin
      ExplorerTree.BeginUpdate;
      try
        ExplorerTree.ReinitNode(ExplorerTree.RootNode.FirstChild, True);
        ExplorerTree.InvalidateToBottom(ExplorerTree.GetFirstVisible);
      finally
        ExplorerTree.EndUpdate;
      end;
    end else begin
      ExplorerTree.Clear;
      ExplorerTree.RootNodeCount := 1;
    end;
    ExplorerTree.TreeOptions.AnimationOptions :=
      ExplorerTree.TreeOptions.AnimationOptions + [toAnimatedToggle];
  end;
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
  ModuleCENode := nil;
  WorkerThread := TScanCodeThread.Create;
  ExplorerTree.OnBeforeCellPaint :=
    CommandsDataModule.VirtualStringTreeBeforeCellPaint;
  ExplorerTree.OnPaintText :=
    CommandsDataModule.VirtualStringTreePaintText;
end;

procedure TCodeExplorerWindow.ExplorerTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  if ExplorerTree.GetNodeLevel(Node) = 0 then
    Data.CENode := ModuleCENode
  else begin
    ParentData := ExplorerTree.GetNodeData(ParentNode);
    Data.CENode :=
      ParentData.CENode.Children[Node.Index] as TAbstractCENode;
  end;
  if Data.CENode.ChildCount > 0 then
    if Data.CENode.InitiallyExpanded then
      InitialStates := [ivsHasChildren, ivsExpanded]
    else
      InitialStates := [ivsHasChildren];
end;

procedure TCodeExplorerWindow.ExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    NavigateToNodeElement(ExplorerTree.GetFirstSelected);
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
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data : PNodeDataRec;
begin
  if Kind in [ikNormal, ikSelected] then begin
    Data := ExplorerTree.GetNodeData(Node);
    ImageIndex := Data.CENode.ImageIndex;
    if (Data.CENode.ClassType = TParsedClass) and
      (vsExpanded in Node.States) then
        ImageIndex := 12;
  end;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  CellText := Data.CENode.Caption;
end;

procedure TCodeExplorerWindow.ExplorerTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  HintText := Data.CENode.Hint;
end;

procedure TCodeExplorerWindow.ExplorerTreeDblClick(Sender: TObject);
begin
  NavigateToNodeElement(ExplorerTree.HotNode)
end;

procedure TCodeExplorerWindow.UpdateWindow;
begin
  if Visible and Assigned(WorkerThread) then  // Issue 219
    TScanCodeThread(WorkerThread).SetModified;
end;

procedure TCodeExplorerWindow.WMSpSkinChange(var Message: TMessage);
begin
  ExplorerTree.Invalidate;
  if SkinManager.IsDefaultSkin then
    ExplorerTree.TreeOptions.PaintOptions := ExplorerTree.TreeOptions.PaintOptions - [toAlwaysHideSelection]
  else
    ExplorerTree.TreeOptions.PaintOptions := ExplorerTree.TreeOptions.PaintOptions + [toAlwaysHideSelection];
end;

procedure TCodeExplorerWindow.ClearAll;
begin
  ExplorerTree.Clear;
  FreeAndNil(ModuleCENode);
end;

procedure TCodeExplorerWindow.FormDestroy(Sender: TObject);
begin
  inherited;
  ShutDownWorkerThread;  // Calls ClearAll;
end;

procedure TCodeExplorerWindow.ShutDownWorkerThread;
begin
  //  Important to Clear here since the destruction of the Worker thread
  //  destroys fOldModule to which ModuleCENode has pointers.
  ClearAll;
  if WorkerThread <> nil then begin
    TScanCodeThread(WorkerThread).Shutdown;
    TScanCodeThread(WorkerThread).WaitFor;
    FreeAndNil(WorkerThread);
  end;
end;

procedure TCodeExplorerWindow.NavigateToNodeElement(Node: PVirtualNode);
var
  Data: PNodeDataRec;
  CodePos : TCodePos;
  L : integer;
begin
  CodePos.LineNo := - 1;
  L := 0;

  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Assigned(Data.CENode.CodeElement) then
    begin
      CodePos := Data.CENode.CodeElement.CodePos;
      L := Length(Data.CENode.Caption);
    end else if Data.CENode is TImportsCENode then begin
      if Data.CENode.ChildCount > 0 then begin
        //  first import
        CodePos :=
          TBaseCodeElement(TImportsCENode(Data.CENode).fModule.ImportedModules[0]).CodePos;
        CodePos.CharOffset := 1;
      end;
    end;

    if CodePos.LineNo >= 0  then begin
      with PyIDEMainForm.GetActiveEditor.SynEdit do
      begin
        CaretXY := BufferCoord(1, CodePos.LineNo);
        EnsureCursorPosVisibleEx(True);
        if CodePos.CharOffset > 0 then
        begin
          SelStart := RowColToCharIndex(CaretXY) + CodePos.CharOffset - 1;
          SelEnd := SelStart + L;
        end;
      end;
    end;
    
  end;
end;

procedure TCodeExplorerWindow.mnExpandAllClick(Sender: TObject);
begin
  ExplorerTree.FullExpand;
end;

procedure TCodeExplorerWindow.nCollapseAllClick(Sender: TObject);
begin
  ExplorerTree.FullCollapse;
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
  fInitiallyExpanded := False;
end;

function TAbstractCENode.GetChildren(i: integer): TAbstractCENode;
begin
  if Assigned(fChildren) then
    Result := TAbstractCENode(fChildren[i])
  else
    Result := nil;
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
  Result := 18;
end;

constructor TModuleCENode.CreateFromModule(AModule: TParsedModule);
Var
  i : integer;
  CodeElement : TCodeElement;
  ClassNode : TClassCENode;
begin
  inherited Create;
  fCodeElement := AModule;
  fInitiallyExpanded := True;
  if Module.ImportedModules.Count > 0 then
    AddChild(TImportsCENode.CreateFromModule(Module));
  if Module.Globals.Count > 0 then
    AddChild(TGlobalsCENode.CreateFromModule(Module));
  for i := 0 to Module.ChildCount - 1 do begin
    CodeElement := Module.Children[i];
    if CodeElement is TParsedClass then begin
      ClassNode := TClassCENode.CreateFromClass(TParsedClass(CodeElement));
      ClassNode.fInitiallyExpanded :=
        CommandsDataModule.PyIDEOptions.ExporerInitiallyExpanded;
      AddChild(ClassNode);
    end else if CodeElement is TParsedFunction then
      AddChild(TFunctionCENode.CreateFromFunction(TParsedFunction(CodeElement)));
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
  if ModuleImport.ImportAll or (ChildCount > 0) then
    Result := 19
  else
    Result := 16;
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
  Result := 0;
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
  Result := 13;
end;

constructor TClassCENode.CreateFromClass(AClass: TParsedClass);
Var
  i : integer;
  CodeElement : TCodeElement;
begin
  inherited Create;
  fCodeElement := AClass;
  if ParsedClass.Attributes.Count > 0 then
    AddChild(TAtrributesCENode.CreateFromClass(ParsedClass));
  for i := 0 to ParsedClass.ChildCount - 1 do begin
    CodeElement := ParsedClass.Children[i];
    if CodeElement is TParsedClass then
      AddChild(TClassCENode.CreateFromClass(TParsedClass(CodeElement)))
    else if CodeElement is TParsedFunction then
      AddChild(TMethodCENode.CreateFromFunction(TParsedFunction(CodeElement)));
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
  Result := 1;
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
  CodeElement : TCodeElement;
begin
  inherited Create;
  fCodeElement := AFunction;
  for i := 0 to ParsedFunction.ChildCount - 1 do begin
    CodeElement := ParsedFunction.Children[i];
    if CodeElement is TParsedClass then begin
      AddChild(TClassCENode.CreateFromClass(TParsedClass(CodeElement)));
    end else if CodeElement is TParsedFunction then
      AddChild(TFunctionCENode.CreateFromFunction(TParsedFunction(CodeElement)));
  end;
end;

function TFunctionCENode.GetCaption: string;
begin
  Result := ParsedFunction.Name;
end;

function TFunctionCENode.GetImageIndex: integer;
begin
  Result := 17;
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
  Result := 14;
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

end.

