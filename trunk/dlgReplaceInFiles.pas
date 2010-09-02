{-----------------------------------------------------------------------------
 Unit Name: dlgReplaceInFiles     
 Author:    Kiriakos Vlahos
 Date:      29-May-2005
 Purpose:
 History:   Based on GExperts (www.gexperts.org) unit and covered by its licence

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

unit dlgReplaceInFiles;

interface

uses
  Classes, Controls, StdCtrls, Forms,
  cFindInFiles, SpTBXControls, SpTBXEditors,
  dlgPyIDEBase, SpTBXItem;

type
  TReplaceInFilesDialog = class(TPyIDEDlgBase)
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnHelp: TSpTBXButton;
    cbBackup: TSpTBXCheckBox;
    cbReplace: TSpTBXComboBox;
    lblInString: TSpTBXLabel;
    lblReplaceString: TSpTBXLabel;
    lblWith: TSpTBXLabel;
    lblIn: TSpTBXLabel;
    lblReplace: TSpTBXLabel;
    SpTBXPanel1: TSpTBXPanel;
    procedure btnHelpClick(Sender: TObject);
  private
    FFindInFilesExpert : TFindInFilesExpert;
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure SetSearchString(const Value: string);
    procedure SetReplaceInString(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RetrieveSettings(var Value: TGrepSettings);
    property FindInFilesExpert: TFindInFilesExpert read FFindInFilesExpert;
    property SearchString: string write SetSearchString;
    property ReplaceInString: string write SetReplaceInString;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, frmFindResults;

constructor TReplaceInFilesDialog.Create(AOwner: TComponent);
begin
  inherited;
  LoadFormSettings;
end;

destructor TReplaceInFilesDialog.Destroy;
begin
  SaveFormSettings;
  inherited;
end;

procedure TReplaceInFilesDialog.SaveFormSettings;
begin
  AddMRUString(cbReplace.Text, FFindInFilesExpert.ReplaceList, False);
  FFindInFilesExpert.BackupModified := cbBackup.Checked;
end;

procedure TReplaceInFilesDialog.LoadFormSettings;
resourcestring
  SGrepResultsNotActive = 'The Find in Files Results window is not active';
begin
  if not Assigned(FindResultsWindow) then
    raise Exception.Create(SGrepResultsNotActive);

  FFindInFilesExpert := FindResultsWindow.FindInFilesExpert;
  cbReplace.Items.Assign(FFindInFilesExpert.ReplaceList);

  if cbReplace.Items.Count > 0 then
  begin
    cbReplace.Text := cbReplace.Items[0];
    cbReplace.SelectAll;
  end;

  cbBackup.Checked := FFindInFilesExpert.BackupModified;
end;

procedure TReplaceInFilesDialog.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.Replace := cbReplace.Text;
  Value.BackupModified := cbBackup.Checked;
end;

procedure TReplaceInFilesDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TReplaceInFilesDialog.SetSearchString(const Value: string);
begin
  lblReplaceString.Caption := Value;
end;

procedure TReplaceInFilesDialog.SetReplaceInString(const Value: string);
begin
  lblInString.Caption := Value;
end;

end.

