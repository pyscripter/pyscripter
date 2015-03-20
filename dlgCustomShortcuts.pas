{-----------------------------------------------------------------------------
 Unit Name: frmCustomShortcuts
 Author:    Kiriakos Vlahos
 Purpose:   Dialog and code to support IDE shortcut customization
 History:   Based on Delhpi Magazine article
-----------------------------------------------------------------------------}

unit dlgCustomShortcuts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, System.Actions, ActnList, ComCtrls, SpTBXControls,
  Buttons, SpTBXEditors, dlgPyIDEBase, ExtCtrls,
  SpTBXItem;

type
  TActionProxyItem = class(TCollectionItem)
  private
    fSecondaryShortCuts: TShortCutList;
    FShortCut: TShortCut;
    fActionListName: string;
    fActionName: string;
    function IsSecondaryShortCutsStored: Boolean;
    procedure SetSecondaryShortCuts(const Value: TCustomShortCutList);
    function GetSecondaryShortCuts: TCustomShortCutList;
  public
    Category : string;
    Caption : string;
    Hint : string;
    destructor Destroy; override;
  published
    property ActionListName : string read fActionListName write fActionListName;
    property ActionName : string read fActionName write fActionName;
    property ShortCut: TShortCut read FShortCut write FShortCut default 0;
    property SecondaryShortCuts: TCustomShortCutList read GetSecondaryShortCuts
      write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
  end;

  TActionListArray = array of TActionList;

  TActionProxyCollection = class(TCollection)
    constructor Create(ActionListArray : TActionListArray);
    procedure ApplyShortCuts(ActionListArray : TActionListArray);
  end;

  TfrmCustomKeyboard = class(TPyIDEDlgBase)
    gbDescription: TSpTBXGroupBox;
    edNewShortcut: THotKey;
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnHelp: TSpTBXButton;
    btnAssign: TSpTBXButton;
    btnRemove: TSpTBXButton;
    lblNewShortcutKey: TSpTBXLabel;
    lblCategories: TSpTBXLabel;
    lblCommands: TSpTBXLabel;
    lblCurrent: TSpTBXLabel;
    lblAssignedTo: TSpTBXLabel;
    lblCurrentKeys: TSpTBXLabel;
    lblDescription: TSpTBXLabel;
    lbCategories: TSpTBXListBox;
    lbCommands: TSpTBXListBox;
    lbCurrentKeys: TSpTBXListBox;
    Bevel1: TBevel;
    SpTBXPanel1: TSpTBXPanel;
    procedure HelpButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCommandsClick(Sender: TObject);
    procedure btnAssignClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure edNewShortcutChange(Sender: TObject);
    procedure lbCurrentKeysClick(Sender: TObject);
  private
    procedure SetCategories;
    procedure DoneItems;
    procedure SelectItem(Idx: Integer);
    function GetCurrentAction: TActionProxyItem;
    procedure FillFunctionList;
    procedure AssignKeysToActionProxy(var CurAction: TActionProxyItem);
    { Private declarations }
  public
    GotKey       : Boolean;
    Categories   : TStringList;
    FunctionList : TStringList;
    KeyList      : TStringList;
    ActionProxyCollection   : TActionProxyCollection;

    procedure PrepActions(ActionListArray : TActionListArray);
    function Execute(ActionListArray : TActionListArray) : Boolean;

    property CurrentAction : TActionProxyItem
      read GetCurrentAction;

    { Public declarations }
  end;
var
  frmCustomKeyboard: TfrmCustomKeyboard;

implementation

{$R *.DFM}

{ TfrmCustomKeyboard }

function TfrmCustomKeyboard.Execute(ActionListArray: TActionListArray) : Boolean;
begin
  Result := False;
  DoneItems;

  PrepActions(ActionListArray);

  if ShowModal = mrOk then begin
    ActionProxyCollection.ApplyShortCuts(ActionListArray);
    Result := True;
  end;

  DoneItems;
end;

procedure TfrmCustomKeyboard.AssignKeysToActionProxy(var CurAction: TActionProxyItem);
var
  i: Integer;
begin
  if lbCurrentKeys.Count > 0 then
    CurAction.ShortCut := TShortCut(lbCurrentKeys.Items.Objects[0])
  else
    CurAction.ShortCut := 0;
  { Assign secondary shortcuts }
  CurAction.SecondaryShortCuts.Clear;
  for i := 1 to lbCurrentKeys.Count - 1 do
    CurAction.SecondaryShortCuts.AddObject(lbCurrentKeys.Items[i],
      lbCurrentKeys.Items.Objects[i]);
end;

procedure TfrmCustomKeyboard.FormCreate(Sender: TObject);
begin
  inherited;
  btnAssign.Enabled := False;
  btnRemove.Enabled := False;

  FunctionList            := TStringList.Create;
  FunctionList.Sorted     := True;
  FunctionList.Duplicates := dupIgnore;

  KeyList                 := TStringList.Create;
  KeyList.Sorted          := True;
  KeyList.Duplicates      := dupIgnore;
end;

procedure TfrmCustomKeyboard.FormDestroy(Sender: TObject);
begin
  KeyList.Free;
  FunctionList.Free;
  ActionProxyCollection.Free;
end;

procedure TfrmCustomKeyboard.FormShow(Sender: TObject);
begin
  lbCategories.Items.Clear;
  lbCommands.Items.Clear;
  lbCurrentKeys.Items.Clear;
  edNewShortCut.HotKey := 0;
  SetCategories;
  SelectItem(0);
  lbCategories.ItemIndex := 0;
  lbCategories.SetFocus;
end;

procedure TfrmCustomKeyboard.SetCategories;
begin
  lbCategories.Items.Clear;
  lbCategories.Items.AddStrings(FunctionList);
  SelectItem(0);
end;

procedure TfrmCustomKeyboard.DoneItems;
var
  i : Integer;
begin
  for i := Pred(FunctionList.Count) downto 0 do begin
    (FunctionList.Objects[i] as TStringList).Free;
    FunctionList.Delete(i);
  end;
end;

procedure TfrmCustomKeyboard.lbCategoriesClick(Sender: TObject);
begin
  SelectItem(lbCategories.ItemIndex);
end;

procedure TfrmCustomKeyboard.SelectItem(Idx: Integer);
begin
  edNewShortCut.HotKey := 0;
  edNewShortcutChange(Self);
  lbCurrentkeys.Items.Clear;
  lbCommands.Items.Clear;
  lblDescription.Caption := '';
  lbCommands.Items.AddStrings(FunctionList.Objects[Idx] as TStrings);
  btnRemove.Enabled := False;
end;

procedure TfrmCustomKeyboard.lbCommandsClick(Sender: TObject);
var
  A : TActionProxyItem;
begin
  if lbCommands.ItemIndex < 0 then Exit;

  A := CurrentAction;

  edNewShortCut.HotKey := 0;
  edNewShortcutChange(Self);
  lbCurrentKeys.Items.Clear;
  lblDescription.Caption := GetLongHint(A.Hint);

  if A.ShortCut <> 0 then
    lbCurrentKeys.Items.AddObject(ShortCutToText(A.ShortCut), TObject(A.ShortCut));

  lbCurrentKeys.Items.AddStrings(A.SecondaryShortCuts);

  btnRemove.Enabled := False;
end;

procedure TfrmCustomKeyboard.btnAssignClick(Sender: TObject);
var
  ShortCut : TShortCut;
  CurAction : TActionProxyItem;
begin
  if lbCommands.ItemIndex < 0 then Exit;
  if edNewShortcut.HotKey <> 0 then begin
    try
      ShortCut := edNewShortcut.HotKey;

      CurAction := CurrentAction;

      if lbCurrentKeys.Items.IndexOf(ShortCutToText(edNewShortcut.HotKey)) < 0 then begin
        { show the keystroke }
        lbCurrentKeys.Items.AddObject(ShortCutToText(edNewShortcut.HotKey), TObject(edNewShortcut.HotKey));

        AssignKeysToActionProxy(CurAction);

        { track the keystroke assignment }
        KeyList.Add(ShortCutToText(ShortCut) + '=' + CurAction.ActionName);

        { Update the lblAssignedTo}
        edNewShortcutChange(Self);
      end else begin
        MessageBeep(MB_ICONEXCLAMATION);
      end;
    except
      MessageBeep(MB_ICONEXCLAMATION);
      edNewShortcut.SetFocus;
    end;
  end;
end;

function TfrmCustomKeyboard.GetCurrentAction: TActionProxyItem;
var
  CatIdx, CmdIdx : Integer;
  SL : TStringList;
begin
  CatIdx := FunctionList.IndexOf(lbCategories.Items[lbCategories.ItemIndex]);
  SL     := FunctionList.Objects[CatIdx] as TStringList;
  CmdIdx := SL.IndexOf(lbCommands.Items[lbCommands.ItemIndex]);
  Result := (SL.Objects[CmdIdx] as TActionProxyItem);
end;

procedure TfrmCustomKeyboard.PrepActions(ActionListArray: TActionListArray);
begin
  ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
  FillFunctionList;
  SetCategories;
end;

procedure TfrmCustomKeyboard.FillFunctionList;
var
  i, j, Idx : Integer;
  A : TActionProxyItem;
begin
  for i := 0 to ActionProxyCollection.Count - 1 do begin
    A := TActionProxyItem(ActionProxyCollection.Items[i]);

    { get category index }
    Idx := FunctionList.IndexOf(A.Category);

    { if category doesn't already exist, add it }
    if Idx < 0 then
      Idx := FunctionList.AddObject(A.Category, TStringList.Create);

    { add keyboard function to list }
    (FunctionList.Objects[Idx] as TStringList).AddObject(A.ActionName, A);

    { shortcut value already assigned }
    if A.ShortCut <> 0 then begin
      { track the keystroke }
      KeyList.Add(ShortCutToText(A.ShortCut) + '=' + A.ActionName);
    end;
    { Deal with secondary shortcuts }
    if A.IsSecondaryShortCutsStored then
      for j := 0 to A.SecondaryShortCuts.Count - 1 do
        KeyList.Add(ShortCutToText(A.SecondaryShortCuts.ShortCuts[j]) + '=' + A.ActionName);
  end;
end;

procedure TfrmCustomKeyboard.btnRemoveClick(Sender: TObject);
var
  CurAction : TActionProxyItem;
  Index : integer;
begin
  if lbCurrentKeys.ItemIndex < 0 then Exit;
  CurAction := CurrentAction;
  { Remove shortcut from keylist }
  Index := KeyList.IndexOf(lbCurrentKeys.Items[lbCurrentKeys.ItemIndex]
    + '=' + CurrentAction.ActionName);
  if Index >= 0 then
    KeyList.Delete(Index);

  { Remove shortcut from lbCurrentKeys }
  lbCurrentKeys.Items.Delete(lbCurrentKeys.ItemIndex);

  AssignKeysToActionProxy(CurAction);

  { Update the lblAssignedTo}
  edNewShortcutChange(Self);
  btnRemove.Enabled := False;
end;

procedure TfrmCustomKeyboard.edNewShortcutChange(Sender: TObject);
begin
  if edNewShortCut.HotKey = 0 then begin
    btnAssign.Enabled := False;
    lblAssignedTo.Visible := False;
    lblCurrent.Visible := False;
  end
  else begin
    btnAssign.Enabled := True;
    lblAssignedTo.Visible := True;
    if KeyList.IndexOfName(ShortCutToText(edNewShortCut.HotKey)) > -1 then begin
      lblCurrent.Visible := True;
      lblAssignedTo.Caption := KeyList.Values[ShortCutToText(edNewShortCut.HotKey)];
    end else begin
      lblCurrent.Visible := False;
      lblAssignedTo.Caption := '[Unassigned]';
    end;
  end;
end;

procedure TfrmCustomKeyboard.lbCurrentKeysClick(Sender: TObject);
begin
  btnAssign.Enabled := False;
  btnRemove.Enabled := True;
end;

{ TActionProxyItem }

procedure TActionProxyItem.SetSecondaryShortCuts(const Value: TCustomShortCutList);
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  fSecondaryShortCuts.Assign(Value);
end;

function TActionProxyItem.IsSecondaryShortCutsStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
end;

destructor TActionProxyItem.Destroy;
begin
  if Assigned(FSecondaryShortCuts) then
    FreeAndNil(FSecondaryShortCuts);
  inherited;
end;

function TActionProxyItem.GetSecondaryShortCuts: TCustomShortCutList;
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  Result := FSecondaryShortCuts;
end;

{ TActionProxyCollection }

constructor TActionProxyCollection.Create(ActionListArray: TActionListArray);
var
  i, j : integer;
  Action : TCustomAction;
  ActionList : TActionList;
  ActionProxyItem : TActionProxyItem;
begin
  inherited Create(TActionProxyItem);
  for i := Low(ActionListArray) to High(ActionListArray) do begin
    ActionList := ActionListArray[i];
    for j := 0 to ActionList.ActionCount - 1 do begin
      Action := ActionList.Actions[j] as TCustomAction;
      ActionProxyItem := Add as TActionProxyItem;
      ActionProxyItem.fActionListName := ActionList.Name;
      ActionProxyItem.fActionName := Action.Name;
      ActionProxyItem.FShortCut := Action.ShortCut;
      ActionProxyItem.Category := Action.Category;
      ActionProxyItem.Caption := Action.Caption;
      ActionProxyItem.Hint := Action.Hint;
      if Action.SecondaryShortCuts.Count > 0 then
        ActionProxyItem.SecondaryShortCuts := Action.SecondaryShortCuts;
    end;
  end;
end;

function FindActionListByName(Name : string;
  ActionListArray: TActionListArray) : TActionList;
var
  i : integer;
begin
  Result := nil;
  for i := Low(ActionListArray) to High(ActionListArray) do
    if ActionListArray[i].Name = Name then begin
      Result := ActionListArray[i];
      break;
    end;
end;

function FindActionByName(Name : string; ActionList : TActionList): TCustomAction;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to ActionList.ActionCount - 1 do
    if ActionList.Actions[i].Name = Name then begin
      Result := ActionList.Actions[i] as TCustomAction;
      break;
    end;
end;

procedure TActionProxyCollection.ApplyShortCuts(
  ActionListArray: TActionListArray);
var
  i : integer;
  ActionProxyItem : TActionProxyItem;
  ActionList : TActionList;
  Action : TCustomAction;
begin
  for i := 0 to Count - 1 do begin
    ActionProxyItem := Items[i] as TActionProxyItem;
    ActionList := FindActionListByName(ActionProxyItem.ActionListName, ActionListArray);
    if Assigned(ActionList) then begin
      Action := FindActionByName(ActionProxyItem.ActionName, ActionList);
      if Assigned(Action) then begin
        Action.ShortCut := ActionProxyItem.ShortCut;
        Action.SecondaryShortCuts.Clear;
        if ActionProxyItem.IsSecondaryShortCutsStored then
          Action.SecondaryShortCuts.Assign(ActionProxyItem.SecondaryShortCuts);
      end;
    end;
  end;
end;

procedure TfrmCustomKeyboard.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
