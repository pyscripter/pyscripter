unit dlgPythonVersions;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  TB2Item,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  dlgPyIDEBase;

type
  TPythonVersionsDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    vtPythonVersions: TVirtualStringTree;
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    actlPythonVersions: TActionList;
    actPVActivate: TAction;
    tbiActivate: TSpTBXItem;
    actPVAdd: TAction;
    actPVRemove: TAction;
    actPVTest: TAction;
    actPVShow: TAction;
    actPVCommandShell: TAction;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiPVAdd: TSpTBXItem;
    tbiPVRemove: TSpTBXItem;
    tbiPVTest: TSpTBXItem;
    tbiPVShow: TSpTBXItem;
    tbiPVCommandPrompt: TSpTBXItem;
    actPVHelp: TAction;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiPVHelp: TSpTBXItem;
    actPVRename: TAction;
    SpTBXItem1: TSpTBXItem;
    vilImages: TVirtualImageList;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    vilTreeImages: TVirtualImageList;
    procedure vtPythonVersionsGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure FormCreate(Sender: TObject);
    procedure vtPythonVersionsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtPythonVersionsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure actPVActivateExecute(Sender: TObject);
    procedure vtPythonVersionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure actlPythonVersionsUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actPVAddExecute(Sender: TObject);
    procedure actPVRemoveExecute(Sender: TObject);
    procedure actPVTestExecute(Sender: TObject);
    procedure actPVShowExecute(Sender: TObject);
    procedure actPVCommandShellExecute(Sender: TObject);
    procedure actPVHelpExecute(Sender: TObject);
    procedure actPVRenameExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PythonVersionsDialog: TPythonVersionsDialog;

implementation
{$R *.dfm}

Uses
  Winapi.ShellAPI,
  Vcl.FileCtrl,
  JvGnuGetText,
  StringResources,
  uEditAppIntfs,
  uCommonFunctions,
  cPyControl,
  PythonVersions,
  dmResources;

procedure TPythonVersionsDialog.actlPythonVersionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
Var
  Node : PVirtualNode;
  Level : integer;
begin
  Node := vtPythonVersions.GetFirstSelected;
  Level := -1;  // to avoid compiler warning
  if Assigned(Node) then
    Level := vtPythonVersions.GetNodeLevel(Node);
  actPVActivate.Enabled := Assigned(Node) and (Level = 1) and
   (not GI_PyControl.PythonLoaded or
    not (((Node.Parent.Index = 0) and (PyControl.PythonVersionIndex = integer(Node.Index))) or
         ((Node.Parent.Index = 1) and (PyControl.PythonVersionIndex = -(Node.Index + 1)))));

  actPVRemove.Enabled := Assigned(Node) and (Level = 1) and (Node.Parent.Index = 1) and
    not (PyControl.PythonVersionIndex = -(Node.Index + 1));
  actPVRename.Enabled := Assigned(Node) and (Level = 1) and (Node.Parent.Index = 1);
  actPVTest.Enabled :=Assigned(Node) and (Level = 1);
  actPVShow.Enabled :=Assigned(Node) and (Level = 1);
  actPVCommandShell.Enabled :=Assigned(Node) and (Level = 1);

  Handled := True;
end;

procedure TPythonVersionsDialog.actPVActivateExecute(Sender: TObject);
var
  Node : PVirtualNode;
  Level : integer;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
    Level := vtPythonVersions.GetNodeLevel(Node);
    if Level = 1 then
    begin
      if Node.Parent.Index = 0 then
        PyControl.PythonVersionIndex := Node.Index
      else if Node.Parent.Index = 1 then
        PyControl.PythonVersionIndex := - (Node.Index + 1);
      vtPythonVersions.InvalidateChildren(nil, True);
    end;
  end;
end;

procedure TPythonVersionsDialog.actPVAddExecute(Sender: TObject);
Var
  PythonVersion: TPythonVersion;
  Directories: TArray<string>;
begin
  if SelectDirectory('', Directories, [], _('Select folder with Python installation (inlcuding virtualenv and venv)'))
  then begin
    if PythonVersionFromPath(Directories[0], PythonVersion, True,
      PyControl.MinPyVersion, PyControl.MaxPyVersion)
    then
    begin
      SetLength(PyControl.CustomPythonVersions, Length(PyControl.CustomPythonVersions) + 1);
      PyControl.CustomPythonVersions[Length(PyControl.CustomPythonVersions)-1] := PythonVersion;
      vtPythonVersions.ReinitChildren(nil, True);
      vtPythonVersions.Selected[vtPythonVersions.GetLast] := True;
    end else
      StyledMessageDlg(_(SPythonFindError), mtError, [mbOK], 0);
  end;
end;

procedure TPythonVersionsDialog.actPVCommandShellExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Level: integer;
  Version: TPythonVersion;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
    Level := vtPythonVersions.GetNodeLevel(Node);
    if (Level = 1) then
    begin
      if Node.Parent.Index = 0 then
        Version := PyControl.RegPythonVersions[Node.Index]
      else
        Version := PyControl.CustomPythonVersions[Node.Index];
      ShellExecute(0, nil, 'cmd.exe', nil,
        PWideChar(Version.InstallPath), SW_SHOWNORMAL);
    end;
  end;
end;

procedure TPythonVersionsDialog.actPVHelpExecute(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TPythonVersionsDialog.actPVRemoveExecute(Sender: TObject);
var
  Node : PVirtualNode;
  Level : integer;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
    Level := vtPythonVersions.GetNodeLevel(Node);
    if (Level = 1) and (Node.Parent.Index = 1) and
      not (PyControl.PythonVersionIndex = -(Node.Index + 1)) then
    begin
      PyControl.RemoveCustomVersion(Node.Index);
      vtPythonVersions.ReinitNode(Node.Parent, True);
    end;
  end;
end;

procedure TPythonVersionsDialog.actPVRenameExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Level: integer;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
     Level := vtPythonVersions.GetNodeLevel(Node);
    if (Level = 1) then
    begin
      if Node.Parent.Index = 1 then begin
        PyControl.CustomPythonVersions[Node.Index].DisplayName :=
          InputBox(_('Rename Python Version'), _('New name:'),
          PyControl.CustomPythonVersions[Node.Index].DisplayName);
        vtPythonVersions.ReinitNode(Node.Parent, True);
      end;
    end;
  end;
end;

procedure TPythonVersionsDialog.actPVShowExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Level: integer;
  Version: TPythonVersion;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
    Level := vtPythonVersions.GetNodeLevel(Node);
    if (Level = 1) then
    begin
      if Node.Parent.Index = 0 then
        Version := PyControl.RegPythonVersions[Node.Index]
      else
        Version := PyControl.CustomPythonVersions[Node.Index];
      ShellExecute(0, nil, PWideChar(Version.InstallPath), nil,
        PWideChar(Version.InstallPath), SW_SHOWNORMAL);
    end;
  end;
end;

procedure TPythonVersionsDialog.actPVTestExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Level: integer;
  Version: TPythonVersion;
begin
  Node := vtPythonVersions.GetFirstSelected;
  if Assigned(Node) then begin
    Level := vtPythonVersions.GetNodeLevel(Node);
    if (Level = 1) then
    begin
      if Node.Parent.Index = 0 then
        Version := PyControl.RegPythonVersions[Node.Index]
      else
        Version := PyControl.CustomPythonVersions[Node.Index];
      ShellExecute(0, nil, PWideChar(Version.PythonExecutable), nil,
        PWideChar(Version.InstallPath), SW_SHOWNORMAL);
    end;
  end;
end;

procedure TPythonVersionsDialog.FormCreate(Sender: TObject);
begin
  inherited;
  vtPythonVersions.DefaultText := '';
  vtPythonVersions.RootNodeCount := 2;
end;

procedure TPythonVersionsDialog.vtPythonVersionsGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
Var
  Level : integer;
begin
  Level := vtPythonVersions.GetNodeLevel(E.Node);
  case Level of
    0:  if E.Column = 0 then
        begin
          if E.Node.Index = 0 then
            E.CellText := _(SRegisteredVersions)
          else
            E.CellText := _(SUnRegisteredVersions);
        end;
    1:  if E.Column = 0 then
        begin
          if E.Node.Parent.Index = 0 then
            E.CellText := PyControl.RegPythonVersions[E.Node.Index].DisplayName
          else
            E.CellText := PyControl.CustomPythonVersions[E.Node.Index].DisplayName;
        end
        else if E.Column = 1 then
        begin
          if E.Node.Parent.Index = 0 then
            E.CellText := PyControl.RegPythonVersions[E.Node.Index].InstallPath
          else
            E.CellText := PyControl.CustomPythonVersions[E.Node.Index].InstallPath;
        end;
  end;
end;

procedure TPythonVersionsDialog.vtPythonVersionsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
Var
  Level : integer;
begin
  ImageIndex := -1;
  if not (Kind in [ikNormal, ikSelected]) or (Column <> 0) then Exit;
  Level := vtPythonVersions.GetNodeLevel(Node);
  if (Level = 1) and GI_PyControl.PythonLoaded and
     (((Node.Parent.Index = 0) and (PyControl.PythonVersionIndex = integer(Node.Index))) or
      ((Node.Parent.Index = 1) and (PyControl.PythonVersionIndex = - (Node.Index + 1))))
  then
    ImageIndex := 0;
end;

procedure TPythonVersionsDialog.vtPythonVersionsInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
Var
  Level : integer;
begin
  Level := vtPythonVersions.GetNodeLevel(Node);
  if Level = 0 then begin
    if Node.Index = 0 then
      ChildCount := Length(PyControl.RegPythonVersions)
    else  if Node.Index = 1 then
      ChildCount := Length(PyControl.CustomPythonVersions);
  end;
end;

procedure TPythonVersionsDialog.vtPythonVersionsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
Var
  Level : integer;
begin
  Level := vtPythonVersions.GetNodeLevel(Node);
  if Level = 0 then begin
    if (Node.Index = 0) and (Length(PyControl.RegPythonVersions) > 0) then
      InitialStates := [ivsHasChildren, ivsExpanded]
    else if (Node.Index = 1) and (Length(PyControl.CustomPythonVersions) > 0) then
      InitialStates := [ivsHasChildren, ivsExpanded];
  end;
end;

end.
