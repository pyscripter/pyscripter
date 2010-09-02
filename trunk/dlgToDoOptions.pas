{-----------------------------------------------------------------------------
 Unit Name: dlgToDoOptions
 Author:    Kiriakos Vlahos
 Date:      19-May-2005
 Purpose:   Form to configure Todo options
 History:   Based on GExperts unit and covered by its licence
            Original Author: AJ Banck <ajbanck@davilex.nl>

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
unit dlgToDoOptions;

interface


uses
  Classes, Controls, StdCtrls, Forms, SpTBXControls,
  SpTBXEditors, dlgPyIDEBase, SpTBXItem, ComCtrls;

type
  TfmToDoOptions = class(TPyIDEDlgBase)
    gbxTokens: TSpTBXGroupBox;
    gbxOptions: TSpTBXGroupBox;
    gbxSearchFiles: TSpTBXGroupBox;
    btnInsert: TSpTBXButton;
    btnApply: TSpTBXButton;
    btnRemove: TSpTBXButton;
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnBrowse: TSpTBXButton;
    btnHelp: TSpTBXButton;
    cbShowTokens: TSpTBXCheckBox;
    chkInclude: TSpTBXCheckBox;
    radScanOpen: TSpTBXRadioButton;
    radScanDir: TSpTBXRadioButton;
    radScanProject: TSpTBXRadioButton;
    lstTokens: TSpTBXListBox;
    edToken: TSpTBXEdit;
    cboPriority: TSpTBXComboBox;
    lblPriority: TSpTBXLabel;
    lblToken: TSpTBXLabel;
    pnlBG: TSpTBXPanel;
    meDirectories: TRichEdit;
    procedure btnInsertClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edTokenChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstTokensClick(Sender: TObject);
    procedure cboPriorityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure radScanDirClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    procedure UpdateButtonState;
    procedure DirEnable(New: Boolean);
  end;

implementation

{$R *.dfm}

uses
  Dialogs, Graphics, SysUtils, frmToDo, dlgDirectoryList, gnugettext,
  StringResources;

procedure TfmToDoOptions.UpdateButtonState;
var
  HasTokenText: Boolean;
  TokenTextInList: Boolean;
  IsListItemSelected: Boolean;
  TextIsCurrentListItem: Boolean;
begin
  HasTokenText := (edToken.Text <> '');
  TokenTextInList := (lstTokens.Items.IndexOf(edToken.Text) > -1);
  IsListItemSelected := (lstTokens.ItemIndex > -1);

  with lstTokens do
    TextIsCurrentListItem := IsListItemSelected and (edToken.Text = Items[ItemIndex]);

  btnInsert.Enabled := HasTokenText and not TokenTextInList;
  btnRemove.Enabled := IsListItemSelected;
  btnApply.Enabled := HasTokenText and IsListItemSelected and TokenTextInList;

  if TextIsCurrentListItem then
    with lstTokens do
    begin
      if (cboPriority.ItemIndex = Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority)) then
        btnApply.Enabled := False;
    end;
end;

procedure TfmToDoOptions.btnBrowseClick(Sender: TObject);
begin
  EditFolderList(meDirectories.Lines, 'To Do search path ')
end;

procedure TfmToDoOptions.DirEnable(New: Boolean);
begin
  meDirectories.Enabled := New;
  chkInclude.Enabled   := New;
  btnBrowse.Enabled    := New;
  if not New then
    meDirectories.Color := clBtnFace
  else
    meDirectories.Color := clWindow;
end;

procedure TfmToDoOptions.btnInsertClick(Sender: TObject);
var
  TokenInfo: TTokenInfo;
  TokenString: string;
begin
  TokenString := Trim(edToken.Text);
  if TokenString <> '' then
  begin
    TokenInfo := TTokenInfo.Create;
    TokenInfo.Token := WideUpperCase(TokenString);
    TokenInfo.Priority := TTodoPriority(cboPriority.ItemIndex);
    lstTokens.Items.AddObject(TokenInfo.Token, TokenInfo);
  end
  else
  begin
    // Warning message that an empty token is inserted
    Dialogs.MessageDlg(_(SEmptyTokenTextError), mtError, [mbOK], 0);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnRemoveClick(Sender: TObject);
begin
  with lstTokens do
    if ItemIndex <> -1 then
    begin
      Items.Objects[ItemIndex].Free;
      Items.Delete(ItemIndex);
    end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnApplyClick(Sender: TObject);
var
  TokenText: string;
begin
  with lstTokens do
  begin
    TokenText := edToken.Text;

    if (ItemIndex > -1) and (TokenText <> '') and
       Assigned(Items.Objects[ItemIndex]) then
    begin
      Items[ItemIndex] := TokenText;
      with TTokeninfo(Items.Objects[ItemIndex]) do
      begin
        Token := WideUpperCase(TokenText);
        Priority := TToDoPriority(cboPriority.ItemIndex);
      end;
    end;
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.edTokenChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormShow(Sender: TObject);
begin
  cboPriority.ItemIndex := 1;
  UpdateButtonState;
end;

procedure TfmToDoOptions.lstTokensClick(Sender: TObject);
begin
  UpdateButtonState;
  if lstTokens.ItemIndex > -1 then
  begin
    with lstTokens do
    begin
      cboPriority.ItemIndex := Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority);
      edToken.Text := Items[ItemIndex]
    end;
  end;
end;

procedure TfmToDoOptions.cboPriorityChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormCreate(Sender: TObject);
begin
  inherited;
  DirEnable(radScanDir.Checked);
end;

procedure TfmToDoOptions.radScanDirClick(Sender: TObject);
begin
  DirEnable(radScanDir.Checked);
end;

procedure TfmToDoOptions.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
