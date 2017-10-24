{-----------------------------------------------------------------------------
 Unit Name: cVirtualStringTreeHelper
 Author:    Kiriakos
 Date:      06-March-2012
 Purpose:   SpTBXLib themeing of TVirtualStringTree
 History:
-----------------------------------------------------------------------------}

unit cVirtualStringTreeHelper;

interface
uses
  Types, Windows, Classes, Graphics, VirtualTrees, VirtualTrees.Utils,
  SpTBXSkins;

type

  TVirtualStringTreeHelper = class helper for TCustomVirtualStringTree
  public
    procedure ReinitInitializedNode(Node: PVirtualNode; Recursive: Boolean);
    procedure ReinitInitializedChildren(Node: PVirtualNode; Recursive: Boolean);
  end;

implementation

type
  // to help us access protected methods

  TCrackedVirtualStringTree = class(TCustomVirtualStringTree)
  end;


{ TThemedVirtualStringTree }

procedure TVirtualStringTreeHelper.ReinitInitializedChildren(Node: PVirtualNode;
  Recursive: Boolean);
// Forces all child nodes of Node to be reinitialized.
// If Recursive is True then also the grandchildren are reinitialized.
// Modified version to reinitialize only when the node is already initialized
var
  Run: PVirtualNode;
begin
  if Assigned(Node) then
  begin
    TCrackedVirtualStringTree(Self).InitChildren(Node);
    Run := Node.FirstChild;
  end
  else
  begin
    TCrackedVirtualStringTree(Self).InitChildren(RootNode);
    Run := RootNode.FirstChild;
  end;

  while Assigned(Run) do
  begin
    if vsInitialized in Run.States then
      ReinitInitializedNode(Run, Recursive);
    Run := Run.NextSibling;
  end;
end;

procedure TVirtualStringTreeHelper.ReinitInitializedNode(Node: PVirtualNode;
  Recursive: Boolean);

// Forces the given node and all its children (if recursive is True) to be initialized again without
// modifying any data in the nodes nor deleting children (unless the application requests a different amount).

begin
  if Assigned(Node) and (Node <> RootNode) and (vsInitialized in Node.States) then
  begin
    // Remove dynamic styles.
    Node.States := Node.States - [vsChecking, vsCutOrCopy, vsDeleting, vsHeightMeasured];
    TCrackedVirtualStringTree(self).InitNode(Node);
  end;

  if Recursive and (not Assigned(Node) or Assigned(Node) and (vsInitialized in Node.States)) then
    ReinitInitializedChildren(Node, True);
end;


end.
