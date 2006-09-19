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
  Messages, Classes, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList,
  ToolWin, Controls;

const
  UM_RESIZECOLS = WM_USER + 523;

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
  TFunctionListWindow = class(TForm)
    pnHolder: TPanel;
    lvProcs: TListView;
    StatusBar: TStatusBar;
    pnlHeader: TPanel;
    dlgProcFont: TFontDialog;
    pnlHeaderLeft: TPanel;
    lblMethods: TLabel;
    edtMethods: TEdit;
    pnlHeaderRight: TPanel;
    cbxObjects: TComboBox;
    lblObjects: TLabel;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnCopy: TToolButton;
    tbnSep1: TToolButton;
    tbnFont: TToolButton;
    tbnSep2: TToolButton;
    tbnStart: TToolButton;
    tbnAny: TToolButton;
    tbnSep3: TToolButton;
    tbnGoto: TToolButton;
    tbnSep4: TToolButton;
    actEditCopy: TAction;
    actOptionsFont: TAction;
    actViewStart: TAction;
    actViewAny: TAction;
    actViewGoto: TAction;
    actHelpHelp: TAction;
    ToolButton1: TToolButton;
    procedure lvProcsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvProcsColumnClick(Sender: TObject; Column: TListColumn);
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
  private
    FSortOnColumn: Integer;
    FSearchAll: Boolean;
    FProcList: TStringList;
    FFileName: string;
    FObjectStrings: TStringList;
    procedure QuickSort(L, R: Integer);
    procedure LoadProcs;
    procedure FillListBox;
    procedure ResizeCols;
    procedure GotoCurrentlySelectedProcedure;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
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
  SysUtils, Menus, Windows, Clipbrd, frmPyIDEMain, StrUtils, Math,
  dmCommands, uEditAppIntfs, cPythonSourceScanner, Contnrs,
  uCommonFunctions, JvJVCLUtils;

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
  StatusBar.Panels[0].Text := Format(SParseStatistics, [LoadTime / 1000]);
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
        ProcInfo.ProcIndex := 17;
      end else
        ProcInfo.ProcIndex := 14;

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
  Module := TParsedModule.Create;
  PythonScanner := TPythonScanner.Create;
  try
    FProcList.Capacity := 200;
    ClearObjectStrings;
    FProcList.BeginUpdate;
    try
      PythonScanner.ScanModule(Editor.SynEdit.Lines.Text, Module);
      ProcessParsedModule(Module);
    finally
      FProcList.EndUpdate;
      LoadObjectCombobox;
    end;
    QuickSort(0, FProcList.Count - 1);
    StatusBar.Panels[1].Text := Trim(IntToStr(lvProcs.Items.Count));
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

procedure TFunctionListWindow.lvProcsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  ProcInfo: TProcInfo;
begin
  ProcInfo := nil;
  if lvProcs.Selected <> nil then
    ProcInfo := lvProcs.Selected.Data;
  if ProcInfo <> nil then
  begin
    StatusBar.Panels[0].Text := ProcInfo.DisplayName + '(' + ProcInfo.ProcArgs + ')';
    StatusBar.Panels[1].Text := Format('%d/%d', [lvProcs.Selected.Index + 1, lvProcs.Items.Count]);
    actViewGoto.Enabled := (lvProcs.Selected <> nil);
  end;
end;

procedure TFunctionListWindow.FillListBox;
var
  i: Integer;
  ProcName: string;
  IsObject: Boolean;
  ProcInfo: TProcInfo;

  procedure AddListItem(ProcInfo: TProcInfo);
  var
    ListItem: TListItem;
  begin
    ListItem := lvProcs.Items.Add;
    ListItem.Caption := '';
    ListItem.ImageIndex := ProcInfo.ProcIndex;
    ListItem.SubItems.Add(ProcInfo.DisplayName);
    ListItem.SubItems.Add(IntToStr(ProcInfo.LineNo));
    ListItem.Data := ProcInfo;
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvProcs.Items.Count > 0 then
    begin
      lvProcs.Selected := lvProcs.Items[0];
      lvProcs.ItemFocused := lvProcs.Selected;
    end;
  end;

begin
  lvProcs.Items.BeginUpdate;
  try
    lvProcs.Items.Clear;
    if (Length(edtMethods.Text) = 0) and (cbxObjects.Text = SAllString) then
    begin
      for i := 0 to FProcList.Count - 1 do
        AddListItem(TProcInfo(FProcList.Objects[i]));
      FocusAndSelectFirstItem;
      Exit;
    end;

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
    FocusAndSelectFirstItem;
  finally
    lvProcs.Items.EndUpdate;
  end;
  ResizeCols;
end;

procedure TFunctionListWindow.FormResize(Sender: TObject);
begin
  with StatusBar do
  begin
    if Width > 80 then
    begin
      Panels[1].Width := 80;
      Panels[0].Width := Width - 80;
    end;
  end;

  ResizeCols;
end;

// This is just a nasty hack to be sure the scroll bar is set right
// before playing with the column widths. We should fix this somehow.
procedure TFunctionListWindow.ResizeCols;
begin
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

procedure TFunctionListWindow.UMResizeCols(var Msg: TMessage);
begin
  Application.ProcessMessages;
  lvProcs.Columns[1].Width := Max(0, lvProcs.ClientWidth - lvProcs.Columns[2].Width
    - lvProcs.Columns[0].Width);
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
    WriteInteger('Function List\SortColumn', FSortOnColumn);
    WritePersistent('Function List\Font', lvProcs.Font);
  end;
end;

procedure TFunctionListWindow.LoadSettings;
begin
  // Do not localize any of the following lines
  with PyIDEMainForm.AppStorage do begin
    Left := ReadInteger('Function List\Left', Left);
    Top := ReadInteger('Function List\Top', Top);
    Width := ReadInteger('Function List\Width', Width);
    Height := ReadInteger('Function List\Height', Height);

    FSortOnColumn := ReadInteger('Function List\SortColumn', FSortOnColumn);
    ReadPersistent('Function List\Font', lvProcs.Font);
    FSearchAll := ReadBoolean('Function List\SearchAll', True);
    ResizeCols;
  end;
end;

procedure TFunctionListWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TFunctionListWindow.QuickSort(L, R: Integer);
resourcestring
  SInvalidIndex = 'Invalid index number';

  function GetValue(idx: Integer): string;
  var
    i: Integer;
    TabPos: Integer;
  begin
    if idx >= FProcList.Count then
      raise Exception.Create(SInvalidIndex);
    Result := FProcList.Strings[idx];
    for i := 0 to FSortOnColumn - 1 do
    begin
      TabPos := Pos(#9, Result);
      if TabPos > 0 then
        Delete(Result, 1, TabPos)
      else
        Exit;
    end;
    if FSortOnColumn = 2 then
    begin
      for i := Length(Result) to 5 do
        Result := ' ' + Result;
    end;
  end;

var
  i, j: Integer;
  P: string;
begin
  if FProcList.Count = 0 then
    Exit;
  repeat
    i := L;
    j := R;
    P := GetValue((L + R) shr 1);
    repeat
      while AnsiCompareText(GetValue(i), P) < 0 do
        Inc(i);
      while AnsiCompareText(GetValue(j), P) > 0 do
        Dec(j);
      if i <= j then
      begin
        FProcList.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then
      QuickSort(L, j);
    L := i;
  until i >= R;
end;

procedure TFunctionListWindow.lvProcsColumnClick(Sender: TObject; Column: TListColumn);
var
  i: Integer;
  Cursor: IInterface;
begin
  i := Column.Index;
  if i <> 0 then
  begin
    Cursor := WaitCursor;
    FSortOnColumn := i;
    QuickSort(0, FProcList.Count - 1);
    FillListBox;
  end;
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
  ResizeCols;
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
  if lvProcs.Selected <> nil then
  begin
    ProcInfo := lvProcs.Selected.Data;
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
  FObjectStrings.Sorted := True;
  FObjectStrings.Duplicates := dupIgnore;
  ClearObjectStrings;

  FSortOnColumn := 1;

  FProcList := TStringList.Create;

  LoadSettings;
  LoadProcs;

  FillListBox;
end;

procedure TFunctionListWindow.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actViewGoto.Enabled := (lvProcs.Selected <> nil);
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


