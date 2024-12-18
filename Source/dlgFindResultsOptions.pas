{-----------------------------------------------------------------------------
 Unit Name: dlgFindResultsOptions
 Author:    Kiriakos Vlahos
 Date:      29-May-2005
 Purpose:
 History:   Based on GExperts (www.gexperts.org) unit and covered by its licence

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement
only covers code written by GExperts, Inc and Erik Berry. You should contact
the other authors concerning their respective copyrights and conditions.

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

unit dlgFindResultsOptions;

interface

uses
  System.Classes,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  dlgPyIDEBase;

type
  TFindResultsOptionsDialog = class(TPyIDEDlgBase)
    gbxMatchList: TGroupBox;
    gbxMatchContext: TGroupBox;
    dlgGrepListFont: TFontDialog;
    dlgGrepContextFont: TFontDialog;
    dlgContextFontColor: TColorDialog;
    pnlListFont: TPanel;
    pnlContextFont: TPanel;
    pnlMatchLineColor: TPanel;
    Panel1: TPanel;
    chkGrepMiddle: TCheckBox;
    chkGrepExpandAll: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    spnContextLines: TSpinEdit;
    lblContextLines: TLabel;
    procedure pnlContextFontClick(Sender: TObject);
    procedure pnlMatchLineColorClick(Sender: TObject);
    procedure pnlListFontClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  end;

implementation

uses
  Vcl.Forms;

{$R *.dfm}

procedure TFindResultsOptionsDialog.pnlContextFontClick(Sender: TObject);
begin
  pnlContextFont.Font.PixelsPerInch := FCurrentPPI;
  pnlMatchLineColor.Font.PixelsPerInch := FCurrentPPI;

  dlgGrepContextFont.Font := pnlContextFont.Font;
  if  dlgGrepContextFont.Execute then
  begin
    pnlContextFont.Font.Assign(dlgGrepContextFont.Font);
    var MatchCol := pnlMatchLineColor.Font.Color;
    pnlMatchLineColor.Font.Assign(dlgGrepContextFont.Font);
    pnlMatchLineColor.Font.Color := MatchCol;
  end;
  {$IF CompilerVersion < 36}
  pnlContextFont.Font.PixelsPerInch := Screen.PixelsPerInch;
  pnlMatchLineColor.Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

procedure TFindResultsOptionsDialog.pnlMatchLineColorClick(Sender: TObject);
begin
  dlgContextFontColor.Color := pnlMatchLineColor.Font.Color;
  if dlgContextFontColor.Execute then begin
    pnlMatchLineColor.Font.Color := dlgContextFontColor.Color;
    pnlMatchLineColor.Refresh;
  end;
end;

procedure TFindResultsOptionsDialog.pnlListFontClick(Sender: TObject);
begin
  pnlListFont.Font.PixelsPerInch := FCurrentPPI;
  dlgGrepListFont.Font := pnlListFont.Font;
  if dlgGrepListFont.Execute then
    pnlListFont.Font.Assign(dlgGrepListFont.Font);
  {$IF CompilerVersion < 36}
  pnlListFont.Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

procedure TFindResultsOptionsDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
