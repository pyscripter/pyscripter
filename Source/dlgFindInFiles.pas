{-----------------------------------------------------------------------------
 Unit Name: dlgFindInFiles
 Author:    Kiriakos Vlahos
 Date:      20-May-2005
 Purpose:   Find in Files dialog
 History:   Based on code from GExperts and covered by its licence

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

unit dlgFindInFiles;

interface

uses
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  cFindInFiles,
  dlgPyIDEBase;

type
  TFindInFilesDialog = class(TPyIDEDlgBase)
    gbxOptions: TGroupBox;
    gbxWhere: TGroupBox;
    gbxDirectories: TGroupBox;
    btnBrowse: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    cbCaseSensitive: TCheckBox;
    cbNoComments: TCheckBox;
    cbWholeWord: TCheckBox;
    cbRegEx: TCheckBox;
    cbInclude: TCheckBox;
    rbOpenFiles: TRadioButton;
    rbProject: TRadioButton;
    rbCurrentOnly: TRadioButton;
    rbDirectories: TRadioButton;
    cbMasks: TComboBox;
    cbDirectory: TComboBox;
    cbText: TComboBox;
    lblFind: TLabel;
    lblMasks: TLabel;
    lblDirectory: TLabel;
    Panel1: TPanel;
    vilImages: TVirtualImageList;
    procedure btnBrowseClick(Sender: TObject);
    procedure rbProjectClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbDirectoryDropDown(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFindInFilesExpert: TFindInFilesExpert;
    procedure EnableDirectoryControls(New: Boolean);
    procedure LoadFormSettings;
    procedure SaveFormSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RetrieveSettings(var Value: TGrepSettings);
    property FindInFilesExpert: TFindInFilesExpert read FFindInFilesExpert;
  end;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Math,
  System.UITypes,
  System.SysUtils,
  System.StrUtils,
  Vcl.FileCtrl,
  Vcl.Dialogs,
  JvGnugettext,
  uEditAppIntfs,
  uCommonFunctions,
  dmResources,
  frmFindResults,
  StringResources,
  cPyScripterSettings,
  cPyControl;

function GetScrollbarWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TFindInFilesDialog.btnBrowseClick(Sender: TObject);
var
  NewDir : string;
  Directories : TArray<string>;
begin
  NewDir := cbDirectory.Text;
  if SelectDirectory(NewDir, Directories, [], _('Directory To Search:')) then
    cbDirectory.Text := Directories[0];
end;

procedure TFindInFilesDialog.EnableDirectoryControls(New: Boolean);
begin
  cbDirectory.Enabled := New;
  cbMasks.Enabled := New;
  cbInclude.Enabled := New;
  btnBrowse.Enabled := New;
end;

procedure TFindInFilesDialog.FormShow(Sender: TObject);
begin
  PostMessage(cbMasks.Handle, CB_SETEDITSEL, 0, 0);
  PostMessage(cbDirectory.Handle, CB_SETEDITSEL, 0, 0);
end;

procedure TFindInFilesDialog.rbProjectClick(Sender: TObject);
begin
  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TFindInFilesDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TFindInFilesDialog.cbDirectoryDropDown(Sender: TObject);
var
  i: Integer;
  MaxWidth: Integer;
  Bitmap: Vcl.Graphics.TBitmap;
begin
  MaxWidth := cbDirectory.Width;
  Bitmap := Vcl.Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(cbDirectory.Font);
    for i := 0 to cbDirectory.Items.Count - 1 do
      MaxWidth := Max(MaxWidth, Bitmap.Canvas.TextWidth(cbDirectory.Items[i]) + 10);
  finally;
    FreeAndNil(Bitmap);
  end;
  if cbDirectory.Items.Count > cbDirectory.DropDownCount then
    Inc(MaxWidth, GetScrollbarWidth);
  MaxWidth := Min(400, MaxWidth);
  if MaxWidth > cbDirectory.Width then
    SendMessage(cbDirectory.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0)
  else
    SendMessage(cbDirectory.Handle, CB_SETDROPPEDWIDTH, 0, 0)
end;

procedure TFindInFilesDialog.btnOKClick(Sender: TObject);
var
  Dirs: TStringDynArray;
  Dir, DirName : string;
begin
  if rbDirectories.Checked then
  begin
    if Trim(cbDirectory.Text) = '' then
      cbDirectory.Text := GetCurrentDir;
    Dirs := SplitString(cbDirectory.Text, ';');
    for Dir in Dirs do
    begin
      if Dir = '' then continue;
      DirName := ExpandFileName(GI_PyIDEServices.ReplaceParams(Dir));
      if not System.SysUtils.DirectoryExists(DirName) then begin
        StyledMessageDlg(Format(_(SSearchDirectoryDoesNotExist), [DirName]), mtError, [mbOK], 0);
        Abort;
      end;
    end;
  end;

  ModalResult := mrOk;
end;

constructor TFindInFilesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  LoadFormSettings;
end;

destructor TFindInFilesDialog.Destroy;
begin
  SaveFormSettings;

  inherited Destroy;
end;

procedure TFindInFilesDialog.SaveFormSettings;
begin
  AddMRUString(cbText.Text, FFindInFilesExpert.SearchList, False);
  AddMRUString(cbDirectory.Text, FFindInFilesExpert.DirList, True);
  AddMRUString(cbMasks.Text, FFindInFilesExpert.MaskList, False);

  FFindInFilesExpert.GrepCaseSensitive := cbCaseSensitive.Checked;
  FFindInFilesExpert.GrepNoComments := cbNoComments.Checked;
  FFindInFilesExpert.GrepSub := cbInclude.Checked;
  FFindInFilesExpert.GrepWholeWord := cbWholeWord.Checked;
  FFindInFilesExpert.GrepRegEx := cbRegEx.Checked;

  if rbCurrentOnly.Checked then
    FFindInFilesExpert.GrepSearch := 0
  else if rbOpenFiles.Checked then
    FFindInFilesExpert.GrepSearch := 1
  else if rbProject.Checked then
    FFindInFilesExpert.GrepSearch := 2
  else if rbDirectories.Checked then
    FFindInFilesExpert.GrepSearch := 3;
end;

procedure TFindInFilesDialog.LoadFormSettings;

  function RetrieveEditorBlockSelection: string;
  var
    Temp: string;
    i: Integer;
  begin
    if Assigned(GI_ActiveEditor) then
      Temp := GI_ActiveEditor.SynEdit.SelText
    else
      Temp := '';
    // Only use the currently selected text if the length is between 1 and 80
    if (Length(Trim(Temp)) >= 1) and (Length(Trim(Temp)) <= 80) then
    begin
      i := Min(Pos(#13, Temp), Pos(#10, Temp));
      if i > 0 then
        Temp := Copy(Temp, 1, i - 1);
      Temp := Temp;
    end else
      Temp := '';
    Result := Temp;
  end;

  procedure SetSearchPattern(Str: string);
  begin
    cbText.Text := Str;
    cbText.SelectAll;
  end;

  procedure SetupPattern;
  var
    Selection: string;
  begin
    Selection := RetrieveEditorBlockSelection;
    if (Trim(Selection) = '') and PyIDEOptions.SearchTextAtCaret then begin
      if Assigned(GI_ActiveEditor) then
        Selection := GI_ActiveEditor.SynEdit.WordAtCursor
      else
        Selection := '';
    end;
    if (Selection = '') and (cbText.Items.Count > 0) then
      Selection := cbText.Items[0];
    SetSearchPattern(Selection);
  end;

begin
  FFindInFilesExpert := FindResultsWindow.FindInFilesExpert;
  cbText.Items.Assign(FFindInFilesExpert.SearchList);
  cbDirectory.Items.Assign(FFindInFilesExpert.DirList);
  cbMasks.Items.Assign(FFindInFilesExpert.MaskList);

  if GI_PyControl.PythonLoaded and
    (cbDirectory.Items.IndexOf(PyControl.PythonVersion.InstallPath) < 0) then
    cbDirectory.Items. Add(PyControl.PythonVersion.InstallPath);

  if FFindInFilesExpert.GrepSave then
  begin
    cbCaseSensitive.Checked := FFindInFilesExpert.GrepCaseSensitive;
    cbNoComments.Checked := FFindInFilesExpert.GrepNoComments;
    cbInclude.Checked := FFindInFilesExpert.GrepSub;
    cbWholeWord.Checked := FFindInFilesExpert.GrepWholeWord;
    cbRegEx.Checked := FFindInFilesExpert.GrepRegEx;
    case FFindInFilesExpert.GrepSearch of
      0: rbCurrentOnly.Checked := True;
      1: rbOpenFiles.Checked := True;
      2: rbProject.Checked := True;
      3: rbDirectories.Checked := True;
    end;

    if cbText.Items.Count > 0 then
      cbText.Text := cbText.Items[0];
    if cbDirectory.Items.Count > 0 then
      cbDirectory.Text := cbDirectory.Items[0];
    if cbMasks.Items.Count > 0 then
      cbMasks.Text := cbMasks.Items[0];
  end;

  SetupPattern;

  rbOpenFiles.Enabled := GI_EditorFactory.Count > 0;
  rbCurrentOnly.Enabled := GI_EditorFactory.Count > 0;

  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TFindInFilesDialog.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.NoComments := cbNoComments.Checked;
  Value.CaseSensitive := cbCaseSensitive.Checked;
  Value.WholeWord := cbWholeWord.Checked;
  Value.RegEx := cbRegEx.Checked;
  Value.Pattern := cbText.Text;
  Value.IncludeSubdirs := cbInclude.Checked;
  Value.Mask := '';
  Value.Directories := '';

  if rbCurrentOnly.Checked then
    Value.FindInFilesAction := gaCurrentOnlyGrep
  else if rbOpenFiles.Checked then
    Value.FindInFilesAction := gaOpenFilesGrep
  else if rbProject.Checked then
    Value.FindInFilesAction := gaProjectGrep
  else
  begin
    Value.FindInFilesAction := gaDirGrep;
    Value.Mask := cbMasks.Text;
    Value.Directories := cbDirectory.Text;
  end;
end;

end.
