{-----------------------------------------------------------------------------
 Unit Name: frmMessages
 Author:    Kiriakos Vlahos
 Purpose:   Messages Window
 History:
-----------------------------------------------------------------------------}

unit frmMessages;

interface

uses
  System.Classes,
  System.Contnrs,
  System.Actions,
  System.ImageList,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  TB2Item,
  SpTBXItem,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  PythonEngine,
  uEditAppIntfs,
  frmIDEDockWin;

type
  TMessagesWindow = class(TIDEDockWindow, IMessagesService)
    TBXPopupMenu: TSpTBXPopupMenu;
    mnClearall: TSpTBXItem;
    MessagesView: TVirtualStringTree;
    mnPreviousMessage: TSpTBXItem;
    mnNextMessage: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    mnCopy: TSpTBXItem;
    MsgsActionList: TActionList;
    actCopyToClipboard: TAction;
    actNextMsgs: TAction;
    actPreviousMsgs: TAction;
    actClearAll: TAction;
    vilImages: TVirtualImageList;
    Panel1: TPanel;
    BtnNextMsg: TButton;
    BtnPrevMsg: TButton;
    procedure TBXPopupMenuPopup(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure ClearAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MessagesViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MessagesViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure MessagesViewDblClick(Sender: TObject);
    procedure actNextMsgsExecute(Sender: TObject);
    procedure actPreviousMsgsExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    const FBasePath = 'Messages Window Options'; // Used for storing settings
    var FMessageHistory: TObjectList;
    FHistoryIndex: Integer;
    FHistorySize: Integer;
    // IMessageServices implementation
    procedure ShowWindow;
    procedure AddMessage(const Msg: string; const FileName: string = '';
       Line: Integer = 0; Offset: Integer = 0; SelLen: Integer = 0);
    procedure ClearMessages;
    procedure ShowPythonTraceback(Traceback: TPythonTraceback; SkipFrames: Integer = 1; ShowWindow: Boolean = False);
  protected
    procedure StoreTopNodeIndex;
    procedure RestoreTopNodeIndex;
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure ShowPythonSyntaxError(E: EPySyntaxError);
    procedure JumpToPosition(Node: PVirtualNode);
    procedure UpdateMsgActions;
    class function CreateInstance: TIDEDockWindow; override;
  end;

var
  MessagesWindow: TMessagesWindow;

implementation

uses
  System.SysUtils,
  Vcl.Forms,
  Vcl.Clipbrd,
  dmResources,
  uCommonFunctions,
  JvGnugettext,
  StringResources;

{$R *.dfm}
type
  TMsg = class
    Msg: string;
    FileName: string;
    Line: Integer;
    Offset: Integer;
    SelLen: Integer;
  end;

  PMsgRec = ^TMsgRec;
  TMsgRec = record
    Msg: TMsg;
  end;

  TMessageList = class(TObjectList)
  private
    FTopNodeIndex: Cardinal;
  public
    property TopNodeIndex: Cardinal read FTopNodeIndex;
  end;

{ TMessagesWindow }

procedure TMessagesWindow.AddMessage(const Msg, FileName: string;
   Line, Offset, SelLen: Integer);
var
  NewMsg: TMsg;
begin
  if FMessageHistory.Count = 0 then
    // Create New List
    FHistoryIndex := FMessageHistory.Add(TMessageList.Create(True))
  else if FHistoryIndex <> FMessageHistory.Count - 1 then begin
    StoreTopNodeIndex;
    FHistoryIndex := FMessageHistory.Count - 1;
    MessagesView.Clear;
  end;

   NewMsg := TMsg.Create;
   NewMsg.Msg := Msg;
   NewMsg.FileName := FileName;
   NewMsg.Line := Line;
   NewMsg.Offset := Offset;
   NewMsg.SelLen := SelLen;

   TObjectList(FMessageHistory[FHistoryIndex]).Add(NewMsg);
  // ReInitializes the list
  MessagesView.RootNodeCount := TObjectList(FMessageHistory[FHistoryIndex]).Count;
  RestoreTopNodeIndex;
  UpdateMsgActions;
end;

procedure TMessagesWindow.ClearMessages;
begin
  if FMessageHistory.Count = 0 then
    // Create New List
    FHistoryIndex := FMessageHistory.Add(TMessageList.Create(True))  // Onwns objects
  else if TObjectList(FMessageHistory.Last).Count = 0 then
  begin
    // Reuse last list
    StoreTopNodeIndex;
    FHistoryIndex := FMessageHistory.Count - 1;
  end else begin
    // Create new list
    StoreTopNodeIndex;
    FHistoryIndex := FMessageHistory.Add(TMessageList.Create(True));
    if FMessageHistory.Count > FHistorySize then
    begin
      FMessageHistory.Delete(0);
      FHistoryIndex := FMessageHistory.Count - 1;
    end;
  end;
  MessagesView.Clear;
  UpdateMsgActions;
end;

class function TMessagesWindow.CreateInstance: TIDEDockWindow;
begin
  MessagesWindow := TMessagesWindow.Create(Application);
  Result := MessagesWindow;
end;

procedure TMessagesWindow.ClearAllExecute(Sender: TObject);
begin
  FMessageHistory.Clear;
  ClearMessages;
end;

procedure TMessagesWindow.actCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := MessagesView.ContentToText(tstAll, #9);
end;

procedure TMessagesWindow.ShowPythonTraceback(Traceback: TPythonTraceback; SkipFrames: Integer; ShowWindow: Boolean);
begin
  with Traceback do begin
    if ItemCount > 0 then begin
      AddMessage('Traceback');
      for var I := SkipFrames {don't show base frame} to ItemCount-1 do
        with Items[I] do
          AddMessage('    '+Context, FileName, LineNo);
      end;
      if ShowWindow then ShowDockForm(Self);
    end;
end;

procedure TMessagesWindow.ShowPythonSyntaxError(E: EPySyntaxError);
begin
  AddMessage(_(SSyntaxError));
  with E do begin
    AddMessage('    ' + EValue, EFileName, ELineNumber, EOffset);
    ShowDockForm(Self);
  end;
end;

procedure TMessagesWindow.ShowWindow;
begin
  GI_PyIDEServices.ShowIDEDockForm(Self, False);
end;

procedure TMessagesWindow.StoreTopNodeIndex;
begin
  if (FHistoryIndex >= 0) and (FMessageHistory.Count > FHistoryIndex) then begin
    if Assigned(MessagesView.TopNode) then
      TMessageList(FMessageHistory[FHistoryIndex]).FTopNodeIndex :=
        MessagesView.TopNode.Index
    else
      TMessageList(FMessageHistory[FHistoryIndex]).FTopNodeIndex := 0;
  end;
end;

procedure TMessagesWindow.JumpToPosition(Node: PVirtualNode);
var
  Msg: TMsg;
begin
  if Assigned(Node) then begin
    MessagesView.Selected[Node] := True;
    Msg := PMsgRec(MessagesView.GetNodeData(Node))^.Msg;

    if (Msg.FileName ='') then Exit; // No FileName or LineNumber
    GI_PyIDEServices.ShowFilePosition(Msg.FileName, Msg.Line, Msg.Offset, Msg.SelLen);
  end;
end;

procedure TMessagesWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(MessagesView) then
    MessagesView.SetFocus;
end;

procedure TMessagesWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'MessagesWin';
  inherited;
  FMessageHistory := TObjectList.Create(True);  // Onwns objects
  FHistoryIndex := -1;
  // 10 sounds reasonable
  // Could become user defined
  FHistorySize := 10;
  // Let the tree know how much data space we need.
  MessagesView.NodeDataSize := SizeOf(TMsgRec);

  GI_MessagesService := Self;
end;

procedure TMessagesWindow.FormDestroy(Sender: TObject);
begin
  GI_MessagesService := nil;
  FMessageHistory.Free;
  inherited;
end;

procedure TMessagesWindow.MessagesViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Assert(ParentNode = nil, 'TMessagesWindow.MessagesViewInitNode');
  Assert(Integer(Node.Index) < TObjectList(FMessageHistory[FHistoryIndex]).Count,
     'TMessagesWindow.MessagesViewInitNode');
  PMsgRec(MessagesView.GetNodeData(Node))^.Msg :=
    TMsg(TObjectList(FMessageHistory[FHistoryIndex])[Node.Index]);
end;

procedure TMessagesWindow.RestoreTopNodeIndex;
var
  I, Index: Cardinal;
  Node: PVirtualNode;
begin
  if (FHistoryIndex >= 0) and (FMessageHistory.Count > FHistoryIndex) then begin
    Index := TMessageList(FMessageHistory[FHistoryIndex]).FTopNodeIndex;
    Node := MessagesView.RootNode.FirstChild;
    I := 0;
    while(Assigned(Node) and Assigned(Node.NextSibling) and (I < Index)) do
      Node := Node.NextSibling;
    if Assigned(Node) then MessagesView.TopNode := Node;
  end;
end;

procedure TMessagesWindow.MessagesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  Assert(Integer(Node.Index) < TObjectList(FMessageHistory[FHistoryIndex]).Count,
    'TMessagesWindow.MessagesViewGetText');
  with PMsgRec(MessagesView.GetNodeData(Node))^.Msg do
    case Column of
      0:  CellText := Msg;
      1:  CellText := FileName;
      2:  if Line > 0
            then CellText := IntToStr(Line)
          else
            CellText := '';
      3:  if Offset > 0 then
            CellText := IntToStr(Offset)
          else
            CellText := '';
      else
        CellText := '';
    end;
end;

procedure TMessagesWindow.MessagesViewDblClick(Sender: TObject);
begin
  if Assigned(MessagesView.GetFirstSelected()) then
    JumpToPosition(MessagesView.GetFirstSelected);
end;

procedure TMessagesWindow.actNextMsgsExecute(Sender: TObject);
begin
  if FHistoryIndex < FMessageHistory.Count - 1 then begin
    StoreTopNodeIndex;
    Inc(FHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(FMessageHistory[FHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
    RestoreTopNodeIndex;
    MessagesView.Invalidate;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.actPreviousMsgsExecute(Sender: TObject);
begin
  if FHistoryIndex > 0 then begin
    StoreTopNodeIndex;
    Dec(FHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(FMessageHistory[FHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
    RestoreTopNodeIndex;
    MessagesView.Invalidate;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.UpdateMsgActions;
begin
  actPreviousMsgs.Enabled := FHistoryIndex > 0 ;
  actNextMsgs.Enabled := FHistoryIndex < FMessageHistory.Count - 1;
  BtnPrevMsg.Enabled := actPreviousMsgs.Enabled;
  BtnNextMsg.Enabled := actNextMsgs.Enabled;
  actClearAll.Enabled := MessagesView.RootNodeCount <> 0;
  actCopyToClipboard.Enabled := MessagesView.RootNodeCount <> 0;
end;

procedure TMessagesWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  MessagesView.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\FileName Width', 200));
  MessagesView.Header.Columns[2].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Line Width', 50));
  MessagesView.Header.Columns[3].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Position Width', 60));
end;

procedure TMessagesWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\FileName Width',
    PPIUnScale(MessagesView.Header.Columns[1].Width));
  AppStorage.WriteInteger(FBasePath+'\Line Width',
    PPIUnScale(MessagesView.Header.Columns[2].Width));
  AppStorage.WriteInteger(FBasePath+'\Position Width',
    PPIUnScale(MessagesView.Header.Columns[3].Width));
end;

procedure TMessagesWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  UpdateMsgActions;
end;

initialization
  TIDEDockWindow.RegisterDockWinClass(ideMessages, TMessagesWindow);
end.
