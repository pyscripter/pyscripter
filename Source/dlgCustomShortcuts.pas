{-----------------------------------------------------------------------------
 Unit Name: frmCustomShortcuts
 Author:    Kiriakos Vlahos
 Purpose:   Dialog and code to support IDE shortcut customization
 History:   Based on Delhpi Magazine article
-----------------------------------------------------------------------------}

unit dlgCustomShortcuts;

interface

uses
  System.Actions,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  dlgPyIDEBase,
  SynEditMiscClasses;

type
  TActionProxyItem = class(TCollectionItem)
  private
    FSecondaryShortCuts: TShortCutList;
    FShortCut: TShortCut;
    FActionListName: string;
    FActionName: string;
    function IsSecondaryShortCutsStored: Boolean;
    procedure SetSecondaryShortCuts(const Value: TCustomShortCutList);
    function GetSecondaryShortCuts: TCustomShortCutList;
  protected
    function GetDisplayName: string;  override;
  public
    Category: string;
    Caption: string;
    Hint: string;
    destructor Destroy; override;
  published
    property ActionListName: string read FActionListName write FActionListName;
    property ActionName: string read FActionName write FActionName;
    property ShortCut: TShortCut read FShortCut write FShortCut default 0;
    property SecondaryShortCuts: TCustomShortCutList read GetSecondaryShortCuts
      write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
  end;

  TActionListArray = array of TActionList;

  TActionProxyCollectionCreateType = (apcctEmpty, apcctAll, apcctChanged);

  TActionProxyCollection = class(TCollection)
    constructor Create(CreateType: TActionProxyCollectionCreateType);
    procedure ApplyShortCuts;
  private
    class var FChangedActions: TList<TCustomAction>;
  public
    class var ActionLists: TActionListArray;
    class constructor Create;
    class destructor Destroy;
  end;

  TfrmCustomKeyboard = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Bevel1: TBevel;
    lblNewShortcutKey: TLabel;
    lblCategories: TLabel;
    lblCommands: TLabel;
    lblCurrent: TLabel;
    lblAssignedTo: TLabel;
    lblCurrentKeys: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    btnAssign: TButton;
    btnRemove: TButton;
    lbCategories: TListBox;
    lbCommands: TListBox;
    lbCurrentKeys: TListBox;
    gbDescription: TGroupBox;
    lblDescription: TLabel;
    ActionList1: TActionList;
    actAssignShortcut: TAction;
    actRemoveShortcut: TAction;
    procedure HelpButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCommandsClick(Sender: TObject);
    procedure btnAssignClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure actRemoveShortcutUpdate(Sender: TObject);
    procedure actAssignShortcutUpdate(Sender: TObject);
  private
    edNewShortcut: TSynHotKey;
    procedure SetCategories;
    procedure DoneItems;
    procedure SelectItem(Idx: Integer);
    function GetCurrentAction: TActionProxyItem;
    procedure FillFunctionList;
    procedure AssignKeysToActionProxy(var CurAction: TActionProxyItem);
    { Private declarations }
  public
    Categories: TStringList;
    FunctionList: TStringList;
    KeyList: TStringList;
    ActionProxyCollection: TActionProxyCollection;

    procedure PrepActions;
    function Execute: Boolean;

    property CurrentAction: TActionProxyItem
      read GetCurrentAction;

    { Public declarations }
  end;

implementation
uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.Themes,
  JvGnugettext,
  uCommonFunctions;

{$R *.DFM}

{ TfrmCustomKeyboard }

function TfrmCustomKeyboard.Execute: Boolean;
begin
  Result := False;
  DoneItems;

  PrepActions;

  if ShowModal = mrOk then begin
    ActionProxyCollection.ApplyShortCuts;
    Result := True;
  end;

  DoneItems;
end;

procedure TfrmCustomKeyboard.actAssignShortcutUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := False;
  if edNewShortcut.HotKey = 0 then begin
    lblAssignedTo.Visible := False;
    lblCurrent.Visible := False;
  end
  else begin
    lblAssignedTo.Visible := True;
    if KeyList.IndexOfName(ShortCutToText(edNewShortcut.HotKey)) > -1 then begin
      lblCurrent.Visible := True;
      lblAssignedTo.Caption := KeyList.Values[ShortCutToText(edNewShortcut.HotKey)];
    end else begin
      Enabled := lbCommands.ItemIndex >= 0;
      lblCurrent.Visible := False;
      lblAssignedTo.Caption := '['+_('Unassigned')+']';
    end;
  end;
  actAssignShortcut.Enabled := Enabled;
end;

procedure TfrmCustomKeyboard.actRemoveShortcutUpdate(Sender: TObject);
begin
  actRemoveShortcut.Enabled :=
    (lbCurrentKeys.ItemIndex >= 0) and (lbCommands.ItemIndex >= 0);
end;

procedure TfrmCustomKeyboard.AssignKeysToActionProxy(var CurAction: TActionProxyItem);
begin
  if lbCurrentKeys.Count > 0 then
    CurAction.ShortCut := TShortCut(NativeUInt(lbCurrentKeys.Items.Objects[0]))
  else
    CurAction.ShortCut := 0;
  { Assign secondary shortcuts }
  CurAction.SecondaryShortCuts.Clear;
  for var I := 1 to lbCurrentKeys.Count - 1 do
    CurAction.SecondaryShortCuts.AddObject(lbCurrentKeys.Items[I],
      lbCurrentKeys.Items.Objects[I]);
end;

procedure TfrmCustomKeyboard.FormCreate(Sender: TObject);
begin
  inherited;
  FunctionList            := TStringList.Create;
  FunctionList.Sorted     := True;
  FunctionList.Duplicates := dupIgnore;

  KeyList                 := TStringList.Create;
  KeyList.Sorted          := True;
  KeyList.Duplicates      := dupIgnore;


  edNewShortcut := TSynHotKey.Create(Self);
  with edNewShortcut do
  begin
    Name := 'edNewShortcut';
    Parent := Panel1;
    Left := PPIScale(8);
    Top := PPIScale(224);
    Width := PPIScale(169);
    Height := PPIScale(21);
    TabOrder := 4;
    InvalidKeys := [];
    Modifiers := [];
    HotKey := 0;
    Font.Color := StyleServices.GetSystemColor(clWindowText);
    Color := StyleServices.GetSystemColor(clWindow);
  end;
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
  edNewShortcut.HotKey := 0;
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
begin
  for var I := Pred(FunctionList.Count) downto 0 do
  begin
    FunctionList.Objects[I].Free;
    FunctionList.Delete(I);
  end;
end;

procedure TfrmCustomKeyboard.lbCategoriesClick(Sender: TObject);
begin
  SelectItem(lbCategories.ItemIndex);
end;

procedure TfrmCustomKeyboard.SelectItem(Idx: Integer);
begin
  edNewShortcut.HotKey := 0;
  lbCurrentKeys.Items.Clear;
  lbCommands.Items.Clear;
  lblDescription.Caption := '';
  lbCommands.Items.AddStrings(FunctionList.Objects[Idx] as TStrings);
end;

procedure TfrmCustomKeyboard.lbCommandsClick(Sender: TObject);
var
  ActionProxy: TActionProxyItem;
begin
  if lbCommands.ItemIndex < 0 then Exit;

  ActionProxy := CurrentAction;

  edNewShortcut.HotKey := 0;
  lbCurrentKeys.Items.Clear;
  lblDescription.Caption := GetLongHint(ActionProxy.Hint);

  if ActionProxy.ShortCut <> 0 then
    lbCurrentKeys.Items.AddObject(ShortCutToText(ActionProxy.ShortCut),
      TObject(NativeUInt(ActionProxy.ShortCut)));

  lbCurrentKeys.Items.AddStrings(ActionProxy.SecondaryShortCuts);
end;

procedure TfrmCustomKeyboard.btnAssignClick(Sender: TObject);
var
  ShortCut: TShortCut;
  CurAction: TActionProxyItem;
begin
  if lbCommands.ItemIndex < 0 then Exit;
  if edNewShortcut.HotKey <> 0 then begin
    try
      ShortCut := edNewShortcut.HotKey;

      CurAction := CurrentAction;

      if lbCurrentKeys.Items.IndexOf(ShortCutToText(edNewShortcut.HotKey)) < 0 then begin
        { show the keystroke }
        lbCurrentKeys.Items.AddObject(ShortCutToText(edNewShortcut.HotKey),
          TObject(NativeUInt(edNewShortcut.HotKey)));

        AssignKeysToActionProxy(CurAction);

        { track the keystroke assignment }
        KeyList.Add(ShortCutToText(ShortCut) + '=' + CurAction.DisplayName);

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
begin
  if lbCommands.ItemIndex < 0 then  Exit(nil);

  Result := lbCommands.Items.Objects[lbCommands.ItemIndex] as TActionProxyItem;
end;

procedure TfrmCustomKeyboard.PrepActions;
begin
  ActionProxyCollection := TActionProxyCollection.Create(apcctAll);
  FillFunctionList;
  SetCategories;
end;

procedure TfrmCustomKeyboard.FillFunctionList;
var
  I, J, Idx: Integer;
  ActionProxy: TActionProxyItem;
begin
  for I := 0 to ActionProxyCollection.Count - 1 do begin
    ActionProxy := TActionProxyItem(ActionProxyCollection.Items[I]);

    { get category index }
    Idx := FunctionList.IndexOf(_(ActionProxy.Category));

    { if category doesn't already exist, add it }
    if Idx < 0 then
      Idx := FunctionList.AddObject(_(ActionProxy.Category), TStringList.Create);

    { add keyboard function to list }
    (FunctionList.Objects[Idx] as TStringList).AddObject(ActionProxy.DisplayName, ActionProxy);

    { shortcut value already assigned }
    if ActionProxy.ShortCut <> 0 then begin
      { track the keystroke }
      KeyList.Add(ShortCutToText(ActionProxy.ShortCut) + '=' + ActionProxy.DisplayName);
    end;
    { Deal with secondary shortcuts }
    if ActionProxy.IsSecondaryShortCutsStored then
      for J := 0 to ActionProxy.SecondaryShortCuts.Count - 1 do
        KeyList.Add(ShortCutToText(ActionProxy.SecondaryShortCuts.ShortCuts[J]) + '=' + ActionProxy.DisplayName);
  end;
end;

procedure TfrmCustomKeyboard.btnRemoveClick(Sender: TObject);
var
  CurAction: TActionProxyItem;
  Index: Integer;
begin
  if (lbCurrentKeys.ItemIndex < 0) or (lbCommands.ItemIndex < 0) then Exit;
  CurAction := CurrentAction;
  { Remove shortcut from keylist }
  Index := KeyList.IndexOf(lbCurrentKeys.Items[lbCurrentKeys.ItemIndex]
    + '=' + CurrentAction.DisplayName);
  if Index >= 0 then
    KeyList.Delete(Index);

  { Remove shortcut from lbCurrentKeys }
  lbCurrentKeys.Items.Delete(lbCurrentKeys.ItemIndex);

  AssignKeysToActionProxy(CurAction);
end;

{ TActionProxyItem }

procedure TActionProxyItem.SetSecondaryShortCuts(const Value: TCustomShortCutList);
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  FSecondaryShortCuts.Assign(Value);
end;

function TActionProxyItem.IsSecondaryShortCutsStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
end;

destructor TActionProxyItem.Destroy;
begin
  FSecondaryShortCuts.Free;
  inherited;
end;

function TActionProxyItem.GetDisplayName: string;
begin
  Result := StripHotkey(Caption);
end;

function TActionProxyItem.GetSecondaryShortCuts: TCustomShortCutList;
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  Result := FSecondaryShortCuts;
end;

{ TActionProxyCollection }

constructor TActionProxyCollection.Create(CreateType: TActionProxyCollectionCreateType);
var
  i, j, Index: Integer;
  Action: TCustomAction;
  ActionList: TActionList;
  ActionProxyItem: TActionProxyItem;
begin
  inherited Create(TActionProxyItem);
  if CreateType = apcctEmpty then Exit;

  for i := Low(TActionProxyCollection.ActionLists) to High(TActionProxyCollection.ActionLists) do begin
    ActionList := TActionProxyCollection.ActionLists[i];
    for j := 0 to ActionList.ActionCount - 1 do begin
      Action := ActionList[j] as TCustomAction;
      if TActionProxyCollection.FChangedActions.BinarySearch(Action, Index) or
        (CreateType = apcctAll) then
      begin
        ActionProxyItem := Add as TActionProxyItem;
        ActionProxyItem.FActionListName := ActionList.Name;
        ActionProxyItem.FActionName := Action.Name;
        ActionProxyItem.FShortCut := Action.ShortCut;
        ActionProxyItem.Category := Action.Category;
        ActionProxyItem.Caption := Action.Caption;
        ActionProxyItem.Hint := Action.Hint;
        if Action.SecondaryShortCuts.Count > 0 then
          ActionProxyItem.SecondaryShortCuts := Action.SecondaryShortCuts;
      end;
    end;
  end;
end;

function FindActionListByName(Name: string;
  ActionListArray: TActionListArray): TActionList;
begin
  Result := nil;
  for var I := Low(TActionProxyCollection.ActionLists) to High(TActionProxyCollection.ActionLists) do
    if ActionListArray[I].Name = Name then begin
      Result := ActionListArray[I];
      Break;
    end;
end;

function FindActionByName(Name: string; ActionList: TActionList): TCustomAction;
begin
  Result := nil;
  for var I := 0 to ActionList.ActionCount - 1 do
    if ActionList.Actions[I].Name = Name then begin
      Result := ActionList.Actions[I] as TCustomAction;
      Break;
    end;
end;

function SameShortcuts(Action: TCustomAction; ActionProxy: TActionProxyItem): Boolean;
begin
  //  No PyScripter action has secondary shortcuts by default  
  Result := (Action.ShortCut = ActionProxy.ShortCut)
    and (not ActionProxy.IsSecondaryShortCutsStored and (Action.SecondaryShortCuts.Count = 0));
end;

procedure TActionProxyCollection.ApplyShortCuts();
var
  I: Integer;
  Index: Integer;
  ActionProxyItem: TActionProxyItem;
  ActionList: TActionList;
  Action: TCustomAction;
begin
  for I := 0 to Count - 1 do begin
    ActionProxyItem := Items[I] as TActionProxyItem;
    ActionList := FindActionListByName(ActionProxyItem.ActionListName, TActionProxyCollection.ActionLists);
    if Assigned(ActionList) then begin
      Action := FindActionByName(ActionProxyItem.ActionName, ActionList);
      if Assigned(Action) then begin
        if SameShortcuts(Action, ActionProxyItem) then Continue;

        Action.ShortCut := ActionProxyItem.ShortCut;
        Action.SecondaryShortCuts.Clear;
        if ActionProxyItem.IsSecondaryShortCutsStored then
          Action.SecondaryShortCuts.Assign(ActionProxyItem.SecondaryShortCuts);
        //  Keep ChangedActions sorted
        if not TActionProxyCollection.FChangedActions.BinarySearch(Action, Index) then
          TActionProxyCollection.FChangedActions.Insert(Index, Action);
      end;
    end;
  end;
end;

procedure TfrmCustomKeyboard.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

class constructor TActionProxyCollection.Create;
begin
  SetLength(TActionProxyCollection.ActionLists, 0);
  TActionProxyCollection.FChangedActions := TList<TCustomAction>.Create;
end;

class destructor TActionProxyCollection.Destroy;
begin
  SetLength(TActionProxyCollection.ActionLists, 0);
  TActionProxyCollection.FChangedActions.Free;
end;

end.
