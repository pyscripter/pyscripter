{-----------------------------------------------------------------------------
 Unit Name: frmVariables
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Variables Window
 History:
-----------------------------------------------------------------------------}

unit frmVariables;

interface

uses
  Winapi.Messages,
  System.UITypes,
  System.Classes,
  System.ImageList,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SpTBXDkPanels,
  SpTBXSkins,
  SpTBXPageScroller,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees.HeaderPopup,
  VirtualTrees,
  SynEdit,
  uPythonItfs,
  frmIDEDockWin,
  cPySupportTypes,
  cPyBaseDebugger;

type
  TVariablesWindow = class(TIDEDockWindow, IVariablesWindow)
    VTHeaderPopupMenu: TVTHeaderPopupMenu;
    VariablesTree: TVirtualStringTree;
    DocPanel: TSpTBXPageScroller;
    SpTBXSplitter: TSpTBXSplitter;
    Panel1: TPanel;
    vilCodeImages: TVirtualImageList;
    synInfo: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure VariablesTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VariablesTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VariablesTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VariablesTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VariablesTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VariablesTreeAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VariablesTreeGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure VariablesTreeNodeClick(Sender: TBaseVirtualTree; const HitInfo:
        THitInfo);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  private
    { Private declarations }
    CurrentModule, CurrentFunction: string;
    GlobalsNameSpace, LocalsNameSpace: TBaseNameSpaceItem;
    class var DebugInspectorsRegister: TDictionary<string, string>;
    // IVariablesWindow implementation
    procedure ClearAll;
    procedure UpdateWindow;
  protected
    const FBasePath = 'Variables Window Options'; // Used for storing settings
    const FBoldIndicatorID: TGUID = '{10FBEC66-4210-49F5-9F7D-189B6252080B}';
    const FItalicIndicatorID: TGUID = '{6B5724CC-1B94-4328-92D1-38C34FC9D667}';
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    class constructor Create;
    class destructor Destroy;
    class function CreateInstance: TIDEDockWindow; override;
  end;

var
  VariablesWindow: TVariablesWindow = nil;

implementation

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Math,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Themes,
  JvGnugettext,
  SynEditMiscClasses,
  SynDWrite,
  StringResources,
  PythonEngine,
  uEditAppIntfs,
  uCommonFunctions,
  dmResources,
  cPyScripterSettings;

{$R *.dfm}
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Name: string;
    ObjectType: string;
    Value: string;
    ImageIndex: Integer;
    NameSpaceItem: TBaseNameSpaceItem;
  end;

procedure TVariablesWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'VariablesWin';
  inherited;
  // Let the tree know how much data space we need.
  VariablesTree.NodeDataSize := SizeOf(TNodeData);

  var FBoldIndicatorSpec := TSynIndicatorSpec.Create(sisTextDecoration,
    clNoneF, clNoneF, [fsBold]);
  synInfo.Indicators.RegisterSpec(FBoldIndicatorID, FBoldIndicatorSpec);
  var FItalicIndicatorSpec := TSynIndicatorSpec.Create(sisTextDecoration,
    clNoneF, clNoneF, [fsItalic]);
  synInfo.Indicators.RegisterSpec(FItalicIndicatorID, FItalicIndicatorSpec);

  GI_VariablesWindow := Self;
end;

procedure TVariablesWindow.VariablesTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data: PNodeData;
begin
  Data := Node.GetData;
  if Assigned(Data.NameSpaceItem) then
  begin
    var Py := SafePyEngine;
    ChildCount := Data.NameSpaceItem.ChildCount;
  end;
end;

procedure TVariablesWindow.VariablesTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Py: IPyEngineAndGIL;
  Data, ParentData: PNodeData;
begin
  Data := Node.GetData;
  if Assigned(ParentNode) then
    ParentData := ParentNode.GetData
  else
    ParentData := nil;

  if not VariablesTree.Enabled or
    ((ParentData <> nil) and (ParentData.NameSpaceItem = nil)) then
  begin
    Data.NameSpaceItem := nil;
    Exit;
  end;

  Py := SafePyEngine;
  if ParentNode = nil then begin
    // Top level
    Assert(Node.Index <= 1, 'VariablesTreeInitNode');
    if CurrentModule <> '' then begin
      if Node.Index = 0 then begin
        Assert(Assigned(GlobalsNameSpace), 'VariablesTreeInitNode');
        Data.NameSpaceItem := GlobalsNameSpace;
        InitialStates := [ivsHasChildren];
      end else if Node.Index = 1 then begin
        Assert(Assigned(LocalsNameSpace), 'VariablesTreeInitNode');
        Data.NameSpaceItem := LocalsNameSpace;
        InitialStates := [ivsExpanded, ivsHasChildren];
      end;
    end else begin
      Assert(Node.Index = 0, 'VariablesTreeInitNode');
      Assert(Assigned(GlobalsNameSpace), 'VariablesTreeInitNode');
      Data.NameSpaceItem := GlobalsNameSpace;
      InitialStates := [ivsExpanded, ivsHasChildren];
    end;
  end else begin
    Data.NameSpaceItem := ParentData.NameSpaceItem.ChildNode[Node.Index];
    if Data.NameSpaceItem.ChildCount > 0 then
      InitialStates := [ivsHasChildren]
    else
      InitialStates := [];
  end;
  // Node Text
  Data.Name := Data.NameSpaceItem.Name;
  Data.ObjectType := Data.NameSpaceItem.ObjectType;
  if not (Data.NameSpaceItem.IsClass or Data.NameSpaceItem.IsFunction
    or Data.NameSpaceItem.IsModule or Data.NameSpaceItem.IsMethod
    or Data.NameSpaceItem.IsDict)
  then
    try
      Data.Value := Data.NameSpaceItem.Value;
    except
      Data.Value := '';
    end
  else
    Data.Value := '';
  // ImageIndex
  if Data.NameSpaceItem.IsDict then
    Data.ImageIndex := Ord(TCodeImages.Namespace)
  else if Data.NameSpaceItem.IsModule then
    Data.ImageIndex := Ord(TCodeImages.Module)
  else if Data.NameSpaceItem.IsMethod then
    Data.ImageIndex := Ord(TCodeImages.Method)
  else if Data.NameSpaceItem.IsFunction then
    Data.ImageIndex := Ord(TCodeImages.Func)
  else if Data.NameSpaceItem.IsClass or Data.NameSpaceItem.Has__dict__ then
     Data.ImageIndex := Ord(TCodeImages.Klass)
  else if (Data.ObjectType = 'list') or (Data.ObjectType = 'tuple') then
    Data.ImageIndex := Ord(TCodeImages.List)
  else begin
    if Assigned(ParentData) and
      (ParentData.NameSpaceItem.IsDict or ParentData.NameSpaceItem.IsModule)
    then
      Data.ImageIndex := Ord(TCodeImages.Variable)
    else
      Data.ImageIndex := Ord(TCodeImages.Field);
  end;
end;

procedure TVariablesWindow.VariablesTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PNodeData;
begin
  Data := Node.GetData;
  if VariablesTree.Enabled and Assigned(Data) and Assigned(Data.NameSpaceItem) then
    if nsaChanged in Data.NameSpaceItem.Attributes then
      TargetCanvas.Font.Color := clRed
    else if nsaNew in Data.NameSpaceItem.Attributes then
      TargetCanvas.Font.Color := $FF8844;
end;

procedure TVariablesWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  var TempWidth := PPIScale(AppStorage.ReadInteger(FBasePath+'\DocPanelWidth', DocPanel.Width));
  DocPanel.Width := Min(TempWidth,  Max(Width-PPIScale(100), PPIScale(3)));
  if AppStorage.ReadBoolean(FBasePath+'\Types Visible') then
    VariablesTree.Header.Columns[1].Options := VariablesTree.Header.Columns[1].Options + [coVisible]
  else
    VariablesTree.Header.Columns[1].Options := VariablesTree.Header.Columns[1].Options - [coVisible];
  VariablesTree.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Names Width', 160));
  VariablesTree.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Types Width', 100));
end;

procedure TVariablesWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  synInfo.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synInfo.Color := StyleServices.GetSystemColor(clWindow);
end;

procedure TVariablesWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\DocPanelWidth', PPIUnScale(DocPanel.Width));
  AppStorage.WriteBoolean(FBasePath+'\Types Visible', coVisible in VariablesTree.Header.Columns[1].Options);
  AppStorage.WriteInteger(FBasePath+'\Names Width',
    PPIUnScale(VariablesTree.Header.Columns[0].Width));
  AppStorage.WriteInteger(FBasePath+'\Types Width',
    PPIUnScale(VariablesTree.Header.Columns[1].Width));
end;

procedure TVariablesWindow.VariablesTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PNodeData;
  Folder: string;
begin
  Data := Node.GetData;
  ImageIndex := -1;

  if not Assigned(Data.NameSpaceItem) then Exit;

  if (Column = 0) and (Kind in [ikNormal, ikSelected]) then
    ImageIndex := Data.ImageIndex
  else if (Column = 3) and (Kind in [ikNormal, ikSelected]) and
    DebugInspectorsRegister.TryGetValue(Data.NameSpaceItem.QualifiedObjectType, Folder)
  then
    ImageIndex := 10;
end;

procedure TVariablesWindow.VariablesTreeGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  Data: PNodeData;
begin
  Data := E.Node.GetData;
  if Assigned(Data) and Assigned(Data.NameSpaceItem) then
    case E.Column of
      0: E.CellText := Data.Name;
      1: E.CellText := Data.ObjectType;
      2: E.CellText := Data.Value;
      3: E.CellText := '';
    end
  else
    E.CellText := 'NA';
end;

procedure TVariablesWindow.UpdateWindow;
var
  Py: IPyEngineAndGIL;
  CurrentFrame: TBaseFrameInfo;
  SameFrame: Boolean;
  RootNodeCount: Cardinal;
  OldGlobalsNameSpace, OldLocalsNamespace: TBaseNameSpaceItem;
begin
  if ((GI_PyControl.PythonEngineType = peSSH) and PyIDEOptions.SSHDisableVariablesWin) or
     not (GI_PyControl.PythonLoaded and
          Assigned(GI_CallStackWindow) and
          Assigned(GI_PyControl.ActiveInterpreter) and
          Assigned(GI_PyControl.ActiveDebugger)) then
  begin
     ClearAll;
     Exit;
  end;

  if GI_PyControl.Running then begin
    // should not update
    VariablesTree.Enabled := False;
    Exit;
  end else
    VariablesTree.Enabled := True;

  Py := SafePyEngine;

  // Get the selected frame
  CurrentFrame := GI_CallStackWindow.GetSelectedStackFrame;

  SameFrame := (not Assigned(CurrentFrame) and
                (CurrentModule = '') and
                (CurrentFunction = '')) or
                (Assigned(CurrentFrame) and
                (CurrentModule = CurrentFrame.FileName) and
                (CurrentFunction = CurrentFrame.FunctionName));

  OldGlobalsNameSpace := GlobalsNameSpace;
  OldLocalsNamespace := LocalsNameSpace;
  GlobalsNameSpace := nil;
  LocalsNameSpace := nil;

  // Turn off Animation to speed things up
  VariablesTree.TreeOptions.AnimationOptions :=
    VariablesTree.TreeOptions.AnimationOptions - [toAnimatedToggle];

  if Assigned(CurrentFrame) then begin
    CurrentModule := CurrentFrame.FileName;
    CurrentFunction := CurrentFrame.FunctionName;
    // Set the initial number of nodes.
    GlobalsNameSpace := GI_PyControl.ActiveDebugger.GetFrameGlobals(CurrentFrame);
    LocalsNameSpace := GI_PyControl.ActiveDebugger.GetFrameLocals(CurrentFrame);
    if Assigned(GlobalsNameSpace) and Assigned(LocalsNameSpace) then
      RootNodeCount := 2
    else
      RootNodeCount := 0;
  end else begin
    CurrentModule := '';
    CurrentFunction := '';
    try
      GlobalsNameSpace := GI_PyControl.ActiveInterpreter.GetGlobals;
      RootNodeCount := 1;
    except
      RootNodeCount := 0;
    end;
  end;

  if (RootNodeCount > 0) and SameFrame and (RootNodeCount = VariablesTree.RootNodeCount) then begin
    if Assigned(GlobalsNameSpace) and Assigned(OldGlobalsNameSpace) then
      GlobalsNameSpace.CompareToOldItem(OldGlobalsNameSpace);
    if Assigned(LocalsNameSpace) and Assigned(OldLocalsNamespace) then
      LocalsNameSpace.CompareToOldItem(OldLocalsNamespace);
    VariablesTree.BeginUpdate;
    try
      // The following will Reinitialize only initialized nodes
      // No need to initialize other nodes they will be initialized as needed
      VariablesTree.ReinitChildren(nil, True);
      VariablesTree.Invalidate;
    finally
      VariablesTree.EndUpdate;
    end;
  end else begin
    VariablesTree.Clear;
    VariablesTree.RootNodeCount := RootNodeCount;
  end;
  FreeAndNil(OldGlobalsNameSpace);
  FreeAndNil(OldLocalsNamespace);

  VariablesTree.TreeOptions.AnimationOptions :=
    VariablesTree.TreeOptions.AnimationOptions + [toAnimatedToggle];
  VariablesTreeAddToSelection(VariablesTree, nil);
end;

procedure TVariablesWindow.ClearAll;
begin
  VariablesTree.Clear;
  if Assigned(GlobalsNameSpace) or Assigned(LocalsNameSpace) then
  begin
    var Py := SafePyEngine;
    FreeAndNil(GlobalsNameSpace);
    FreeAndNil(LocalsNameSpace);
  end;
end;

class constructor TVariablesWindow.Create;
// Registers Variable Inspectors

  procedure ProcessSubFolder(const SubFolder: string);
  begin
    var SL := TSmartPtr.Make(TStringList.Create)();
    try
      SL.LoadFromFile(TPath.Combine(SubFolder, 'handled_classes.txt'));
      for var Klass in SL do
        DebugInspectorsRegister.TryAdd(Klass, SubFolder);
    except
    end;
  end;

  procedure ProcessFolder(const Folder: string);
  begin
    if (Folder = '') or not TDirectory.Exists(Folder) then Exit;

    for var Subfolder in TDirectory.GetDirectories(Folder) do
      ProcessSubFolder(SubFolder);
  end;

begin
  DebugInspectorsRegister := TDictionary<string, string>.Create;
  ProcessFolder(TPyScripterSettings.UserDebugInspectorsDir);
  ProcessFolder(TPyScripterSettings.AppDebugInspectorsDir);
end;

class function TVariablesWindow.CreateInstance: TIDEDockWindow;
begin
  VariablesWindow := TVariablesWindow.Create(Application);
  Result := VariablesWindow;
end;

class destructor TVariablesWindow.Destroy;
begin
  DebugInspectorsRegister.Free;
end;

procedure TVariablesWindow.FormActivate(Sender: TObject);
begin
  inherited;

  if CanActuallyFocus(VariablesTree) then
    VariablesTree.SetFocus;
  //PostMessage(VariablesTree.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TVariablesWindow.FormDestroy(Sender: TObject);
begin
  GI_VariablesWindow := nil;
  VariablesWindow := nil;
  ClearAll;
  inherited;
end;

procedure TVariablesWindow.VariablesTreeAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

  procedure AddFormatText(ASynEdit: TSynEdit; const S: string;
    FontStyle: TFontStyles = []);
  var
    Indicator: TSynIndicator;
  begin
    var Lines := S.Split([sLineBreak]);
    for var I := 0 to Length(Lines) - 1 do
    begin
      if (I > 0) or (ASynEdit.Lines.Count = 0) then
        ASynEdit.Lines.Add('');
      var OldLine := ASynEdit.Lines[ASynEdit.Lines.Count - 1];
      if (Lines[I] <> '')  then begin
        // Save the old indicators and restore them after changing the line
        var OldIndicators := ASynEdit.Indicators.LineIndicators(ASynEdit.Lines.Count);
        ASynEdit.Lines[ASynEdit.Lines.Count - 1] := OldLine + Lines[I];
        for var OldIndicator in OldIndicators do
          ASynEdit.Indicators.Add(ASynEdit.Lines.Count, Indicator);
        if (FontStyle <> []) then
        begin
          if fsItalic in FontStyle then
            Indicator := TSynIndicator.Create(FItalicIndicatorID,
              OldLine.Length + 1, OldLine.Length + Lines[I].Length + 1)
          else
            Indicator := TSynIndicator.Create(FBoldIndicatorID,
              OldLine.Length + 1, OldLine.Length + Lines[I].Length + 1);
           ASynEdit.Indicators.Add(ASynEdit.Lines.Count, Indicator);
        end;
      end;
    end;
  end;

var
  NameSpace,
  ObjectName,
  ObjectType,
  ObjectValue,
  DocString: string;
  Data: PNodeData;
begin
  if not Enabled then Exit;

  // Get the selected frame
  if CurrentModule <> '' then
    NameSpace := Format(_(SNamespaceFormat), [CurrentFunction, CurrentModule])
  else
    NameSpace := 'Interpreter globals';

  synInfo.Clear;
  synInfo.BeginUpdate;
  try
    AddFormatText(synInfo, _('Namespace') + ': ', [fsBold]);
    AddFormatText(synInfo, NameSpace, [fsItalic]);
    if Assigned(Node) then begin
      var Py := SafePyEngine;
      Data := Node.GetData;
      ObjectName := Data.Name;
      ObjectType := Data.ObjectType;
      ObjectValue := Data.Value;
      DocString :=  Data.NameSpaceItem.DocString;

      AddFormatText(synInfo, sLineBreak+_('Name')+': ', [fsBold]);
      AddFormatText(synInfo, ObjectName, [fsItalic]);
      AddFormatText(synInfo, sLineBreak + _('Type') + ': ', [fsBold]);
      AddFormatText(synInfo, ObjectType);
      if ObjectValue <> '' then begin
        AddFormatText(synInfo, sLineBreak + _('Value') + ':' + sLineBreak, [fsBold]);
        AddFormatText(synInfo, ObjectValue);
      end;
      AddFormatText(synInfo, sLineBreak + _('DocString') + ':' + sLineBreak, [fsBold]);
      AddFormatText(synInfo, AdjustLineBreaks(DocString, System.tlbsCRLF));
    end;
  finally
    synInfo.EndUpdate;
  end;
  synInfo.ClientHeight := synInfo.DisplayRowCount * synInfo.LineHeight;
  synInfo.ClientHeight := Max(synInfo.ClientHeight, DocPanel.ClientHeight);
end;

procedure TVariablesWindow.VariablesTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PNodeData;
begin
  Data := Node.GetData;
  Finalize(Data^);
end;

procedure TVariablesWindow.VariablesTreeNodeClick(Sender: TBaseVirtualTree;
    const HitInfo: THitInfo);
var
  Data: PNodeData;
  Folder: string;
  Py: IPyEngineAndGIL;
begin
  if (HitInfo.HitColumn = 3) and Assigned(HitInfo.HitNode) then
  begin
    Data := HitInfo.HitNode.GetData;
    if DebugInspectorsRegister.TryGetValue(Data.NameSpaceItem.QualifiedObjectType, Folder) then
    begin
      // Pickle the value
      Py := SafePyEngine;
      var FileName := TPath.Combine(TPyScripterSettings.UserDebugInspectorsDir, 'value.pickle');
      try
        GI_PyControl.Pickle(Data.NameSpaceItem.PyObject, FileName);
      except
        on E: Exception do
        begin
           ShowMessage(E.Message);
           Exit;
        end;
      end;

      var Parameters := Format('"%s" "%s"', [TPath.Combine(Folder, 'main.py'), FileName]);

      ShellExecute(0, 'open', PChar(GI_PyControl.PythonVersion.PythonExecutable),
        PChar(Parameters), PChar(Folder), SW_HIDE);
    end;
  end;
end;

initialization
  TIDEDockWindow.RegisterDockWinClass(ideVariables, TVariablesWindow);
end.


