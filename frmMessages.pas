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
  Dialogs, StdCtrls, ComCtrls, Menus, JvDockControlForm, JvComponent, PythonEngine,
  Contnrs, frmIDEDockWin, ExtCtrls, TB2Item, TBX, TBXThemes, TBXDkPanels, VirtualTrees,
  TB2Dock, TB2Toolbar, ActnList, JvComponentBase;

type
  TMessagesWindow = class(TIDEDockWindow)
    TBXPopupMenu: TTBXPopupMenu;
    mnClearall: TTBXItem;
    MessagesView: TVirtualStringTree;
    TBToolbar1: TTBToolbar;
    TBControlItem5: TTBControlItem;
    TBControlItem6: TTBControlItem;
    BtnPreviousMsgs: TTBXButton;
    BtnNextMsgs: TTBXButton;
    MsgsActionList: TActionList;
    actClearAll: TAction;
    actPreviousMsgs: TAction;
    actNextMsgs: TAction;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    actCopyToClipboard: TAction;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem3: TTBXItem;
    procedure TBXPopupMenuPopup(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure ClearAllExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MessagesViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MessagesViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MessagesViewDblClick(Sender: TObject);
    procedure actNextMsgsExecute(Sender: TObject);
    procedure actPreviousMsgsExecute(Sender: TObject);
  private
    { Private declarations }
    fMessageHistory : TObjectList;
    fHistoryIndex : integer;
    fHistorySize : integer;
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  public
    { Public declarations }
    procedure ShowWindow;
    procedure AddMessage(Msg: string; FileName : string = '';
                         Line : integer = 0; Offset : integer = 0);
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
  frmPyIDEMain, uEditAppIntfs, SynEditTypes, dmCommands, uCommonFunctions,
  Clipbrd, JvDockGlobals, VarPyth;

{$R *.dfm}
Type
  TMsg = class
    Msg: string;
    FileName : string;
    Line : integer;
    Offset : integer;
  end;

  PMsgRec = ^TMsgRec;
  TMsgRec = record
    Msg : TMsg;
  end;


{ TMessagesWindow }

procedure TMessagesWindow.AddMessage(Msg, FileName: string; Line, Offset : integer);
Var
  NewMsg : TMsg;
begin
  if fMessageHistory.Count = 0 then
    // Create New List
    fHistoryIndex := fMessageHistory.Add(TObjectList.Create(True))
  else if fHistoryIndex <> fMessageHistory.Count - 1 then begin
    fHistoryIndex := fMessageHistory.Count - 1;
    MessagesView.Clear;
  end;

   NewMsg := TMsg.Create;
   NewMsg.Msg := Msg;
   NewMsg.FileName := FileName;
   NewMsg.Line := Line;
   NewMsg.Offset := Offset;

   TObjectList(fMessageHistory[fHistoryIndex]).Add(NewMsg);
  // ReInitializes the list
  MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
  UpdateMsgActions;
end;

procedure TMessagesWindow.ClearMessages;
begin
  MessagesView.Clear;
  if fMessageHistory.Count = 0 then
    // Create New List
    fHistoryIndex := fMessageHistory.Add(TObjectList.Create(True))  // Onwns objects
  else if TObjectList(fMessageHistory[fMessageHistory.Count-1]).Count = 0 then
    // Reuse last list
    fHistoryIndex := fMessageHistory.Count - 1
  else begin
    // Create new list
    fHistoryIndex := fMessageHistory.Add(TObjectList.Create(True));
    if fMessageHistory.Count > fHistorySize then begin
      fMessageHistory.Delete(0);
      fHistoryIndex := fMessageHistory.Count - 1;
    end;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.ClearAllExecute(Sender: TObject);
begin
  fMessageHistory.Clear;
  ClearMessages;
end;

procedure TMessagesWindow.actCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := MessagesView.ContentToText(tstAll, #9);
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
  AddMessage('Syntax Error');
  with E do begin
    AddMessage('    ' + EValue, EFileName, ELineNumber, EOffset);
    ShowDockForm(Self);
  end;
end;

procedure TMessagesWindow.ShowPythonSyntaxError(ErrorClass : string; E: Variant);
begin
  AddMessage(ErrorClass);
  AddMessage('    ' + E, E.filename, E.lineno, E.offset);
  ShowDockForm(Self);
end;

procedure TMessagesWindow.ShowWindow;
begin
    ShowDockForm(Self);
end;

procedure TMessagesWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if not HasFocus then begin
    FGPanelEnter(Self);
    PostMessage(MessagesView.Handle, WM_SETFOCUS, 0, 0);
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
    PyIDEMainForm.ShowFilePosition(Msg.FileName, Msg.Line, Msg.Offset);
  end;
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
  MessagesView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  MessagesView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
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

procedure TMessagesWindow.MessagesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
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
    end;
end;

procedure TMessagesWindow.MessagesViewDblClick(Sender: TObject);
begin
  if Assigned(MessagesView.GetFirstSelected()) then
    JumpToPosition(MessagesView.GetFirstSelected)
end;

procedure TMessagesWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    MessagesView.Header.Invalidate(nil, True);
    MessagesView.Colors.HeaderHotColor :=
      CurrentTheme.GetItemTextColor(GetItemInfo('active'));
  end;
end;

procedure TMessagesWindow.actNextMsgsExecute(Sender: TObject);
begin
  if fHistoryIndex < fMessageHistory.Count - 1 then begin
    Inc(fHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
    MessagesView.Invalidate;
  end;
  UpdateMsgActions;
end;

procedure TMessagesWindow.actPreviousMsgsExecute(Sender: TObject);
begin
  if fHistoryIndex > 0 then begin
    Dec(fHistoryIndex);
    MessagesView.RootNodeCount := TObjectList(fMessageHistory[fHistoryIndex]).Count;
    MessagesView.ReinitNode(MessagesView.RootNode, True);
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

procedure TMessagesWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  UpdateMsgActions;
end;

end.


