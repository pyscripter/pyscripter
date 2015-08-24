{-----------------------------------------------------------------------------
 Unit Name: frmMessages
 Author:    Kiriakos Vlahos
 Purpose:   Messages Window
 History:
-----------------------------------------------------------------------------}

unit frmMessages;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvDockControlForm, PythonEngine,
  Contnrs, frmIDEDockWin, ExtCtrls, TB2Item, VirtualTrees,
  TB2Dock, TB2Toolbar, ActnList, JvComponentBase, SpTBXSkins, SpTBXItem, SpTBXControls,
  JvAppStorage, System.Actions;

type
  TMessagesWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    TBXPopupMenu: TSpTBXPopupMenu;
    mnClearall: TSpTBXItem;
    MessagesView: TVirtualStringTree;
    TBToolbar1: TTBToolbar;
    TBControlItem5: TTBControlItem;
    TBControlItem6: TTBControlItem;
    BtnPreviousMsgs: TSpTBXButton;
    BtnNextMsgs: TSpTBXButton;
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
    { Private declarations }
    fMessageHistory : TObjectList;
    fHistoryIndex : integer;
    fHistorySize : integer;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure StoreTopNodeIndex;
    procedure RestoreTopNodeIndex;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
    procedure ShowWindow;
    procedure AddMessage(Msg: string; FileName : string = '';
       Line : integer = 0; Offset : integer = 0; SelLen : integer = 0);
    procedure ClearMessages;
    procedure ShowPythonTraceback(SkipFrames : integer = 1);
    procedure ShowTraceback(Traceback : Variant; SkipFrames : integer = 0);
    procedure ShowPythonSyntaxError(E: EPySyntaxError); overload;
    procedure ShowPythonSyntaxError(ErrorClass : string; E: Variant); overload;
    procedure JumpToPosition(Node : PVirtualNode);
    procedure UpdateMsgActions;
  end;

var
  MessagesWindow: TMessagesWindow;


implementation

uses
  frmPyIDEMain, dmCommands, uCommonFunctions,
  Clipbrd, VarPyth, gnugettext, StringResources, cThemedVirtualStringTree;

{$R *.dfm}
Type
  TMsg = class
    Msg: string;
    FileName : string;
    Line : integer;
    Offset : integer;
    SelLen : integer;
  end;

  PMsgRec = ^TMsgRec;
  TMsgRec = record
    Msg : TMsg;
  end;

  TMessageList = class(TObjectList)
  private
    fTopNodeIndex : Cardinal;
  public
    property TopNodeIndex : Cardinal read fTopNodeIndex;
  end;

{ TMessagesWindow }

procedure TMessagesWindow.AddMessage(Msg, FileName: string;
   Line, Offset, SelLen : integer);
Var
  NewMsg : TMsg;
begin
  if fMessageHistory.Count = 0 then
    // Create New List
    fHistoryIndex := fMessageHistory.Add(TMessageList.Create(True))
  else if fHistoryIndex <> fMessageHistory.Count - 1 then begin
    StoreTopNodeIndex;
    fHistoryIndex := fMessageHistory.Count - 1;
    MessagesView.Clear;
  end;

   NewMsg := TMsg.Create;
   NewMsg.Msg := Msg;
   NewMsg.FileName := FileName;
   NewMsg.Line := Line;
   NewMsg.Offset := Offset;
   NewMsg.SelLen := SelLen;

   TObjectList(fMessageHistory[fHistoryIndex]).Add(NewMsg);
  // ReInitializes the list
  MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
  RestoreTopNodeIndex;
  UpdateMsgActions;
end;

procedure TMessagesWindow.ClearMessages;
begin
  if fMessageHistory.Count = 0 then
    // Create New List
    fHistoryIndex := fMessageHistory.Add(TMessageList.Create(True))  // Onwns objects
  else if TObjectList(fMessageHistory[fMessageHistory.Count-1]).Count = 0 then begin
    // Reuse last list
    StoreTopNodeIndex;
    fHistoryIndex := fMessageHistory.Count - 1;
  end else begin
    // Create new list
    StoreTopNodeIndex;
    fHistoryIndex := fMessageHistory.Add(TMessageList.Create(True));
    if fMessageHistory.Count > fHistorySize then begin
      fMessageHistory.Delete(0);
      fHistoryIndex := fMessageHistory.Count - 1;
    end;
  end;
  MessagesView.Clear;
  UpdateMsgActions;
end;

procedure TMessagesWindow.ClearAllExecute(Sender: TObject);
begin
  fMessageHistory.Clear;
  ClearMessages;
end;

procedure TMessagesWindow.actCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := string(MessagesView.ContentToText(tstAll, #9));
end;

procedure TMessagesWindow.ShowPythonTraceback(SkipFrames : integer = 1);
Var
  i : integer;
begin
  with GetPythonEngine.TraceBack do begin
    if ItemCount > 0 then begin
      AddMessage('Traceback');
      for i := SkipFrames {don't show base frame} to ItemCount-1 do
        with Items[i] do
          AddMessage('    '+Context, FileName, LineNo);
      end;
      ShowDockForm(Self);
    end;
end;

procedure TMessagesWindow.ShowTraceback(Traceback: Variant;
  SkipFrames: integer);
Var
  i : integer;
  CurrentTraceback : Variant;
  CodeObject : Variant;
  LineNo: Integer;
begin
  if VarIsPython(Traceback) and not VarIsNone(Traceback) then begin
    CurrentTraceback := Traceback;
    for i  := 1 to SkipFrames do begin
      CurrentTraceback := CurrentTraceback.tb_next;
      if VarIsNone(Traceback) then break;
    end;
    if not VarIsNone(CurrentTraceback) then begin
      AddMessage('Traceback');
      while not VarIsNone(CurrentTraceback) do begin
        try
          LineNo := CurrentTraceback.tb_lineno;
        except
          LineNo := 0;
        end;
        CodeObject := CurrentTraceback.tb_frame.f_code;
        AddMessage('    '+CodeObject.co_name,
          CodeObject.co_filename, LineNo);
        CurrentTraceback := CurrentTraceback.tb_next;
      end;
      ShowDockForm(Self);
    end;
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

procedure TMessagesWindow.ShowPythonSyntaxError(ErrorClass : string; E: Variant);
Var
  Msg, FileName : string;
  LineNo, Offset : integer;
begin
  try
    Msg := E;
  except
    Msg := '';
  end;
  ExtractPyErrorInfo(E, FileName, LineNo, Offset);
  AddMessage(ErrorClass);
  AddMessage('    ' + Msg, FileName, LineNo, Offset);
  ShowDockForm(Self);
end;

procedure TMessagesWindow.ShowWindow;
begin
    ShowDockForm(Self);
end;

procedure TMessagesWindow.StoreTopNodeIndex;
begin
  if (fHistoryIndex >= 0) and (fMessageHistory.Count > fHistoryIndex) then begin
    if Assigned(MessagesView.TopNode) then
      TMessageList(fMessageHistory[fHistoryIndex]).fTopNodeIndex :=
        MessagesView.TopNode.Index
    else
      TMessageList(fMessageHistory[fHistoryIndex]).fTopNodeIndex := 0;
  end;
end;

procedure TMessagesWindow.JumpToPosition(Node : PVirtualNode);
Var
  Msg : TMsg;
begin
  if Assigned(Node) then begin
    MessagesView.Selected[Node] := True;
    Msg := PMsgRec(MessagesView.GetNodeData(Node))^.Msg;

    if (Msg.FileName ='') then Exit; // No FileName or LineNumber
    PyIDEMainForm.ShowFilePosition(Msg.FileName, Msg.Line, Msg.Offset, Msg.SelLen);
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
  inherited;
  fMessageHistory := TObjectList.Create(True);  // Onwns objects
  fHistoryIndex := -1;
  // 10 sounds reasonable
  // Could become user defined
  fHistorySize := 10;
  // Let the tree know how much data space we need.
  MessagesView.NodeDataSize := SizeOf(TMsgRec);
  MessagesView.Header.Height :=
    MulDiv(MessagesView.Header.Height, Screen.PixelsPerInch, 96);
end;

procedure TMessagesWindow.FormDestroy(Sender: TObject);
begin
  fMessageHistory.Free;
  inherited;
end;

procedure TMessagesWindow.MessagesViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Assert(MessagesView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < TObjectList(fMessageHistory[fHistoryIndex]).Count);
  PMsgRec(MessagesView.GetNodeData(Node))^.Msg :=
    TMsg(TObjectList(fMessageHistory[fHistoryIndex])[Node.Index]);
end;

procedure TMessagesWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  MessagesView.Header.Columns[1].Width := AppStorage.ReadInteger(BasePath+'\FileName Width', 200);
  MessagesView.Header.Columns[2].Width := AppStorage.ReadInteger(BasePath+'\Line Width', 50);
  MessagesView.Header.Columns[3].Width := AppStorage.ReadInteger(BasePath+'\Position Width', 60);
end;

procedure TMessagesWindow.RestoreTopNodeIndex;
Var
  i, Index : Cardinal;
  Node : PVirtualNode;
begin
  if (fHistoryIndex >= 0) and (fMessageHistory.Count > fHistoryIndex) then begin
    Index := TMessageList(fMessageHistory[fHistoryIndex]).fTopNodeIndex;
    Node := MessagesView.RootNode.FirstChild;
    i := 0;
    while(Assigned(Node) and Assigned(Node.NextSibling) and (i<Index)) do
      Node := Node.NextSibling;
    if Assigned(Node) then MessagesView.TopNode := Node;
  end;
end;

procedure TMessagesWindow.MessagesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  Assert(MessagesView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < TObjectList(fMessageHistory[fHistoryIndex]).Count);
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
    JumpToPosition(MessagesView.GetFirstSelected)
end;

procedure TMessagesWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  MessagesView.SkinTree;
end;

procedure TMessagesWindow.actNextMsgsExecute(Sender: TObject);
begin
  if fHistoryIndex < fMessageHistory.Count - 1 then begin
    StoreTopNodeIndex;
    Inc(fHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
    RestoreTopNodeIndex;
    MessagesView.Invalidate;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.actPreviousMsgsExecute(Sender: TObject);
begin
  if fHistoryIndex > 0 then begin
    StoreTopNodeIndex;
    Dec(fHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
    RestoreTopNodeIndex;
    MessagesView.Invalidate;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.UpdateMsgActions;
begin
  actPreviousMsgs.Enabled := fHistoryIndex > 0 ;
  actNextMsgs.Enabled := fHistoryIndex < fMessageHistory.Count - 1;
  BtnPreviousMsgs.Enabled := actPreviousMsgs.Enabled;
  BtnNextMsgs.Enabled := actNextMsgs.Enabled;
  actClearAll.Enabled := MessagesView.RootNodeCount <> 0;
  actCopyToClipboard.Enabled := MessagesView.RootNodeCount <> 0;
end;

procedure TMessagesWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteInteger(BasePath+'\FileName Width', MessagesView.Header.Columns[1].Width);
  AppStorage.WriteInteger(BasePath+'\Line Width', MessagesView.Header.Columns[2].Width);
  AppStorage.WriteInteger(BasePath+'\Position Width', MessagesView.Header.Columns[3].Width);
end;

procedure TMessagesWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  UpdateMsgActions;
end;

end.
