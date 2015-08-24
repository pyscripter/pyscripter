{-----------------------------------------------------------------------------
 Unit Name: frmFunctionList
 Author:    Kiriakos Vlahos
 Date:      14-Jun-2005
 Purpose:   Function List window for code navigation
            Adapted from GExperts and covered by its license

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement only covers code written by GExperts, Inc and Erik Berry. You should contact the other authors concerning their respective copyrights and conditions.

The rules governing the use of GExperts and the GExperts source code are derived
from the official Open Source Definition, available at http://www.opensource.org.
The conditions and limitations are as follows:

    * Usage of GExperts binary distributions is permitted for all developers.
      You may not use the GExperts source code to develop proprietary or
      commercial products including plugins or libraries for those products.
      You may use the GExperts source code in an Open Source project, under the
      terms listed below.
    * You may not use the GExperts source code to create and distribute custom
      versions of GExperts under the "GExperts" name. If you do modify and
      distribute custom versions of GExperts, the binary distribution must be
      named differently and clearly marked so users can tell they are not using
      the official GExperts distribution. A visible and unmodified version of
      this license must appear in any modified distribution of GExperts.
    * Custom distributions of GExperts must include all of the custom changes
      as a patch file that can be applied to the original source code. This
      restriction is in place to protect the integrity of the original author's
      source code. No support for modified versions of GExperts will be provided
      by the original authors or on the GExperts mailing lists.
    * All works derived from GExperts must be distributed under a license
      compatible with this license and the official Open Source Definition,
      which can be obtained from http://www.opensource.org/.
    * Please note that GExperts, Inc. and the other contributing authors hereby
      state that this package is provided "as is" and without any express or
      implied warranties, including, but not without limitation, the implied
      warranties of merchantability and fitness for a particular purpose. In
      other words, we accept no liability for any damage that may result from
      using GExperts or programs that use the GExperts source code.

-----------------------------------------------------------------------------}
unit frmFunctionList;

interface

uses
  Messages, Windows, Classes, Forms, Dialogs, StdCtrls, ActnList,
  Controls, SpTBXControls, Graphics,
  SpTBXEditors, TB2Dock, TB2Toolbar, SpTBXItem, TB2Item,
  dlgPyIDEBase, MPCommonObjects, EasyListview, MPCommonUtilities, System.Actions;

type
  TProcInfo = class(TObject)
  private
    FLineNo: Integer;
    FDisplayName: string;
    FProcArgs: string;
    FProcClass: string;
    FProcName: string;
    FProcIndex: Integer;
  public
    property LineNo: Integer read FLineNo write FLineNo;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ProcArgs: string read FProcArgs write FProcArgs;
    property ProcName: string read FProcName write FProcName;
    property ProcClass: string read FProcClass write FProcClass;
    property ProcIndex: Integer read FProcIndex write FProcIndex;
  end;


type
  TFunctionListWindow = class(TPyIDEDlgBase)
    pnHolder: TSpTBXPanel;
    pnlHeader: TSpTBXPanel;
    dlgProcFont: TFontDialog;
    pnlHeaderLeft: TSpTBXPanel;
    pnlHeaderRight: TSpTBXPanel;
    Actions: TActionList;
    actHelpHelp: TAction;
    actViewGoto: TAction;
    actViewAny: TAction;
    actViewStart: TAction;
    actOptionsFont: TAction;
    actEditCopy: TAction;
    lblMethods: TSpTBXLabel;
    lblObjects: TSpTBXLabel;
    edtMethods: TSpTBXEdit;
    cbxObjects: TSpTBXComboBox;
    ToolBarDock: TSpTBXDock;
    Toolbar: TSpTBXToolbar;
    tbiHelp: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiViewGoto: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiViewAny: TSpTBXItem;
    tbiViewStart: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    tbiFont: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    tbiCopy: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    LeftStatusLabel: TSpTBXLabelItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    RightStatusLabel: TSpTBXLabelItem;
    lvProcs: TEasyListview;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtMethodsChange(Sender: TObject);
    procedure edtMethodsKeyPress(Sender: TObject; var Key: Char);
    procedure edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxObjectsChange(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actOptionsFontExecute(Sender: TObject);
    procedure actViewStartExecute(Sender: TObject);
    procedure actViewAnyExecute(Sender: TObject);
    procedure actViewGotoExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lvProcsItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure lvProcsItemDblClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; MousePos: TPoint; HitInfo: TEasyHitInfoItem);
    function lvProcsItemCompare(Sender: TCustomEasyListview;
      Column: TEasyColumn; Group: TEasyGroup; Item1, Item2: TEasyItem;
      var DoDefault: Boolean): Integer;
    procedure lvProcsSortEnd(Sender: TCustomEasyListview);
  private
    FSearchAll: Boolean;
    FProcList: TStringList;
    FFileName: string;
    FObjectStrings: TStringList;
    procedure LoadProcs;
    procedure FillListBox;
    procedure ResizeCols;
    procedure GotoCurrentlySelectedProcedure;
    procedure ClearObjectStrings;
    procedure LoadObjectCombobox;
    procedure InitializeForm;
    procedure AddProcedure(ProcedureInfo: TProcInfo);
  public
    constructor CreateWithFileName(AOwner: TComponent; const FileName: string);
    destructor Destroy; override;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

procedure JumpToFunction;

implementation

{$R *.dfm}

uses
  SysUtils, Clipbrd, frmPyIDEMain, StrUtils, Math,
  dmCommands, uEditAppIntfs, cPythonSourceScanner;

resourcestring
  SAllString = '<All>';
  SNoneString = '<None>';

constructor TFunctionListWindow.CreateWithFileName(AOwner: TComponent; const FileName: string);
resourcestring
  SParseStatistics = 'Module processed in %g seconds';
var
  LoadTime: DWORD;
begin
  inherited Create(AOwner);

  FFileName := FileName;

  LoadTime := GetTickCount;
  InitializeForm;
  LoadTime := GetTickCount - LoadTime;
  RightStatusLabel.Caption := Format(SParseStatistics, [LoadTime / 1000]);
  ResizeCols;
end;

procedure TFunctionListWindow.LoadProcs;

  procedure ProcessCodeElement(CodeElement : TCodeElement);
  Var
    ProcInfo: TProcInfo;
    i : integer;
  begin
    if CodeElement.ClassType = TParsedFunction then begin
      ProcInfo := TProcInfo.Create;
      ProcInfo.ProcName := CodeElement.Name;
      ProcInfo.LineNo := CodeElement.CodeBlock.StartLine;
      ProcInfo.ProcArgs := TParsedFunction(CodeElement).ArgumentsString;
      if Assigned(CodeElement.Parent) and (CodeElement.Parent.ClassType = TParsedClass) then begin
        ProcInfo.FProcClass := CodeElement.Parent.Name;
        ProcInfo.ProcIndex := Integer(TCodeImages.Method);
      end else
        ProcInfo.ProcIndex := Integer(TCodeImages.Func);

      AddProcedure(ProcInfo);
    end;
    for i := 0 to CodeElement.ChildCount - 1 do
      ProcessCodeElement(CodeElement.Children[i]);
  end;

  procedure ProcessParsedModule(Module : TParsedModule);
  var
    i : integer;
  begin
    if Module.ChildCount > 0 then
      for i := 0 to Module.ChildCount - 1 do
        ProcessCodeElement(Module.Children[i] as TCodeElement);
  end;

var
  Editor : IEditor;
  Module : TParsedModule;
  PythonScanner : TPythonScanner;
begin
  Caption := Caption + ' - ' + ExtractFileName(FFileName);
  Editor := GI_EditorFactory.GetEditorByNameOrTitle(Self.FFileName);
  if not Assigned(Editor) then Exit;
  Module := TParsedModule.Create(Editor.SynEdit.Lines.Text);
  PythonScanner := TPythonScanner.Create;
  try
    FProcList.Capacity := 200;
    ClearObjectStrings;
    FProcList.BeginUpdate;
    try
      PythonScanner.ScanModule(Module);
      ProcessParsedModule(Module);
    finally
      FProcList.EndUpdate;
      LoadObjectCombobox;
    end;
    RightStatusLabel.Caption := Trim(IntToStr(lvProcs.Items.Count));
  finally
    PythonScanner.Free;
    Module.Free;
  end;
end;

procedure TFunctionListWindow.AddProcedure(ProcedureInfo: TProcInfo);
begin
  if Length(ProcedureInfo.ProcClass) > 0 then
    ProcedureInfo.DisplayName := ProcedureInfo.ProcClass + '.';
  ProcedureInfo.DisplayName := ProcedureInfo.DisplayName + ProcedureInfo.ProcName;
  FProcList.AddObject(#9 + ProcedureInfo.DisplayName + #9 + IntToStr(ProcedureInfo.LineNo), ProcedureInfo);
  if Length(ProcedureInfo.ProcClass) = 0 then
    FObjectStrings.Add(SNoneString)
  else
    FObjectStrings.Add(ProcedureInfo.ProcClass);
end;

procedure TFunctionListWindow.FillListBox;
var
  i: Integer;
  ProcName: string;
  IsObject: Boolean;
  ProcInfo: TProcInfo;

  procedure AddListItem(ProcInfo: TProcInfo);
  var
    ListItem: TEasyItem;
  begin
    ListItem := lvProcs.Items.Add;
    ListItem.Caption := '';
    ListItem.ImageIndex := ProcInfo.ProcIndex;
    ListItem.Captions[1] := ProcInfo.DisplayName;
    ListItem.Captions[2] := IntToStr(ProcInfo.LineNo);
    ListItem.Data := ProcInfo;
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvProcs.Items.Count > 0 then
    begin
      lvProcs.Items[0].Selected := True;
      lvProcs.Items[0].Focused := True;
    end;
  end;

begin
  lvProcs.Sort.BeginUpdate;
  lvProcs.BeginUpdate;
  try
    lvProcs.Items.Clear;
    if (Length(edtMethods.Text) = 0) and (cbxObjects.Text = SAllString) then
    begin
      for i := 0 to FProcList.Count - 1 do
        AddListItem(TProcInfo(FProcList.Objects[i]));
    end else begin
      for i := 0 to FProcList.Count - 1 do
      begin
        ProcInfo := TProcInfo(FProcList.Objects[i]);
        IsObject := Length(ProcInfo.ProcClass) > 0;

        // Is it the object we want?
        if cbxObjects.Text <> SAllString then
        begin
          if cbxObjects.Text = SNoneString then
          begin
            if IsObject then // Does it have an object?
              Continue;
            if Length(edtMethods.Text) = 0 then // If no filter is active, add
            begin
              AddListItem(ProcInfo);
              Continue;
            end;
          end // if/then
          else if not SameText(cbxObjects.Text, ProcInfo.ProcClass) then
            Continue;
        end;

        ProcName := ProcInfo.ProcName;

        if Length(edtMethods.Text) = 0 then
          AddListItem(ProcInfo)
        else if not FSearchAll and SameText(edtMethods.Text, Copy(ProcName, 1, Length(edtMethods.Text))) then
          AddListItem(ProcInfo)
        else if FSearchAll and AnsiContainsText(ProcName, edtMethods.Text) then
          AddListItem(ProcInfo);
      end;
    end;
  finally
    lvProcs.EndUpdate;
    lvProcs.Sort.EndUpdate;
  end;
  FocusAndSelectFirstItem;
end;

procedure TFunctionListWindow.ResizeCols;
begin
  lvProcs.Header.Columns[1].Width := Max(0, lvProcs.ClientWidth - lvProcs.Header.Columns[2].Width
    - lvProcs.Header.Columns[0].Width- 30);
end;

procedure TFunctionListWindow.SaveSettings;
begin
  // Do not localize any of the following lines
  with PyIDEMainForm.AppStorage do begin
    WriteInteger('Function List\Left', Left);
    WriteInteger('Function List\Top', Top);
    WriteInteger('Function List\Width', Width);
    WriteInteger('Function List\Height', Height);

    WriteBoolean('Function List\SearchAll', FSearchAll);
    WriteInteger('Function List\SortColumn', lvProcs.Selection.FocusedColumn.Index);
    WritePersistent('Function List\Font', lvProcs.Font);
  end;
end;

procedure TFunctionListWindow.LoadSettings;
Var
    SortOnColumn: Integer;
begin
  // Do not localize any of the following lines
  with PyIDEMainForm.AppStorage do begin
    Left := ReadInteger('Function List\Left', Left);
    Top := ReadInteger('Function List\Top', Top);
    Width := ReadInteger('Function List\Width', Width);
    Height := ReadInteger('Function List\Height', Height);

    SortOnColumn := ReadInteger('Function List\SortColumn', 1);
    if SortOnColumn < 3 then
      lvProcs.Header.ClickColumn(lvProcs.Header.Columns[SortOnColumn]);
    ReadPersistent('Function List\Font', lvProcs.Font);
    FSearchAll := ReadBoolean('Function List\SearchAll', True);
  end;
end;

procedure TFunctionListWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

function TFunctionListWindow.lvProcsItemCompare(Sender: TCustomEasyListview;
  Column: TEasyColumn; Group: TEasyGroup; Item1, Item2: TEasyItem;
  var DoDefault: Boolean): Integer;
Var
  PI1, PI2: TProcInfo;
begin
  Result := 0;
  PI1 := TProcInfo(Item1.Data);
  PI2 := TProcInfo(Item2.Data);
  case Column.Index of
    0:   Result := Sign(PI1.ProcIndex - PI2.ProcIndex);
    1:   Result := StrIComp(PChar(PI1.DisplayName), PChar(PI2.DisplayName));
    2:   //Result := Sign(PI1.LineNo - PI2.LineNo);
      if PI1.LineNo > PI2.LineNo then
        Result := 1
      else if PI1.LineNo = PI2.LineNo then
        Result := 0
      else
        Result := -1;
  end;
  if Assigned(Column) and (Column.SortDirection = esdDescending) then
    Result := -Result;
  DoDefault := False;
end;

procedure TFunctionListWindow.lvProcsItemDblClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; MousePos: TPoint; HitInfo: TEasyHitInfoItem);
begin
  GotoCurrentlySelectedProcedure;
end;

procedure TFunctionListWindow.lvProcsItemSelectionsChanged(
  Sender: TCustomEasyListview);
var
  ProcInfo: TProcInfo;
begin
  ProcInfo := nil;
  if Assigned(lvProcs.Selection.First()) then
    ProcInfo := lvProcs.Selection.First.Data as TProcInfo;
  actViewGoto.Enabled := Assigned(ProcInfo);

  if ProcInfo <> nil then
  begin
    LeftStatusLabel.Caption := ProcInfo.DisplayName + '(' + ProcInfo.ProcArgs + ')';
    RightStatusLabel.Caption := Format('%d/%d', [lvProcs.Selection.First.Index + 1, lvProcs.Items.Count]);
  end;
end;

procedure TFunctionListWindow.lvProcsSortEnd(Sender: TCustomEasyListview);
Var
  Item : TEasyItem;
begin
  Item := lvProcs.Selection.First;
  if Assigned(Item) then
    Item.MakeVisible(emvAuto);
end;

procedure TFunctionListWindow.edtMethodsChange(Sender: TObject);
begin
  FillListBox;
end;

procedure TFunctionListWindow.edtMethodsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      begin
        GotoCurrentlySelectedProcedure;
        Key := #0;
      end;
    #27:
      begin
        Close;
        Key := #0;
      end;
  end;
end;

procedure TFunctionListWindow.edtMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not (((Key = VK_F4) and (ssAlt in Shift)) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT]) or
    ((Key in [VK_INSERT]) and ((ssShift in Shift) or (ssCtrl in Shift))) or
    ((Key in [VK_HOME, VK_END]) and (ssShift in Shift))) then
  begin
    SendMessage(lvProcs.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TFunctionListWindow.cbxObjectsChange(Sender: TObject);
begin
  FillListBox;
end;

procedure TFunctionListWindow.pnlHeaderResize(Sender: TObject);
begin
  pnlHeaderLeft.Width := (pnlHeader.ClientWidth div 2);
  edtMethods.Width := pnlHeaderLeft.ClientWidth - edtMethods.Left - 8;
  cbxObjects.Width := pnlHeaderRight.ClientWidth - cbxObjects.Left - 8;
end;

procedure TFunctionListWindow.ClearObjectStrings;
begin
  FObjectStrings.Clear;
  FObjectStrings.Add(SAllString);
end;

procedure TFunctionListWindow.LoadObjectCombobox;
begin
  cbxObjects.Items.Assign(FObjectStrings);
  cbxObjects.ItemIndex := cbxObjects.Items.IndexOf(SAllString);
end;

procedure TFunctionListWindow.actEditCopyExecute(Sender: TObject);
var
  i: Integer;
  Procs: TStringList;
  ProcInfo: TProcInfo;
begin
  Procs := TStringList.Create;
  try
    for i := 0 to lvProcs.Items.Count - 1 do
    begin
      ProcInfo := TProcInfo(lvProcs.Items[i].Data);
      if ProcInfo <> nil then
        Procs.Add(ProcInfo.ProcName + ProcInfo.ProcArgs);
    end;
  finally
    if Procs.Count > 0 then
      Clipboard.AsText := Procs.Text;
    FreeAndNil(Procs);
  end;
end;

procedure TFunctionListWindow.actHelpHelpExecute(Sender: TObject);
begin
    Application.HelpContext(HelpContext);
end;

procedure TFunctionListWindow.actOptionsFontExecute(Sender: TObject);
begin
  dlgProcFont.Font.Assign(lvProcs.Font);
  if dlgProcFont.Execute then
    lvProcs.Font.Assign(dlgProcFont.Font);
end;

procedure TFunctionListWindow.actViewStartExecute(Sender: TObject);
begin
  FSearchAll := False;

  FillListBox;
end;

procedure TFunctionListWindow.actViewAnyExecute(Sender: TObject);
begin
  FSearchAll := True;

  FillListBox;
end;

procedure TFunctionListWindow.actViewGotoExecute(Sender: TObject);
begin
  GotoCurrentlySelectedProcedure;
end;

procedure TFunctionListWindow.GotoCurrentlySelectedProcedure;
var
  ProcInfo: TProcInfo;
begin
  if Assigned(lvProcs.Selection.First()) then
  begin
    ProcInfo := lvProcs.Selection.First.Data as TProcInfo;
    if ProcInfo <> nil then
    begin
      PyIDEMainForm.ShowFilePosition(fFileName, ProcInfo.LineNo, 1);
      ModalResult := mrOk;
    end;
  end;
end;

destructor TFunctionListWindow.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FObjectStrings);

  if FProcList <> nil then
  begin
    for i := 0 to FProcList.Count - 1 do
      FProcList.Objects[i].Free;
    FreeAndNil(FProcList);
  end;

  inherited Destroy;
end;

procedure TFunctionListWindow.InitializeForm;
begin
  FObjectStrings := TStringList.Create;
  FObjectStrings.CaseSensitive := True;
  FObjectStrings.Sorted := True;
  FObjectStrings.Duplicates := dupIgnore;
  ClearObjectStrings;

  FProcList := TStringList.Create;

  LoadSettings;
  LoadProcs;

  FillListBox;
end;

procedure TFunctionListWindow.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actViewGoto.Enabled := Assigned(lvProcs.Selection.First());
  actViewStart.Checked := not FSearchAll;
  actViewAny.Checked := FSearchAll;
end;

procedure JumpToFunction;
var
  FileName: string;
  Dlg: TFunctionListWindow;

begin
  if Assigned(GI_ActiveEditor) then begin
    FileName := GI_ActiveEditor.GetFileNameOrTitle;

    Dlg := TFunctionListWindow.CreateWithFileName(nil, FileName);
    try
      Dlg.ShowModal;
    finally
      FreeAndNil(Dlg);
    end;
  end;
end;

end.
