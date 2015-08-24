{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DTestPrintPreview.pas, released 2000-06-01.

The Original Code is part of the TestPP project, written by
Morten J. Skovrup for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: DTestPrintPreview.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit dlgSynPrintPreview;

{$I SynEdit.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls,
  Buttons, ActnList, ImgList, Dialogs,
  SynEditPrintPreview, Printers, SpTBXItem,
  TB2Item, TB2Dock, TB2Toolbar, dlgPyIDEBase, System.Actions;

type
  TPrintPreviewDlg = class(TPyIDEDlgBase)
    ImageList: TImageList;
    SynEditPrintPreview: TSynEditPrintPreview;
    ActionList: TActionList;
    CloseCmd: TAction;
    PrintCmd: TAction;
    ZoomCmd: TAction;
    LastCmd: TAction;
    NextCmd: TAction;
    PrevCmd: TAction;
    FirstCmd: TAction;
    ToolbarDock: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    tbiClose: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiPrint: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    tbiLast: TSpTBXItem;
    tbiNext: TSpTBXItem;
    tbiPrev: TSpTBXItem;
    tbiFirst: TSpTBXItem;
    tbiZoom: TSpTBXSubmenuItem;
    tbiWholePage: TSpTBXItem;
    tbiPageWidth: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    tbi25: TSpTBXItem;
    tbi50: TSpTBXItem;
    tbi100: TSpTBXItem;
    tbi200: TSpTBXItem;
    tbi400: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    RightStatusLabel: TSpTBXLabelItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    LeftStatusLabel: TSpTBXLabelItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;

    procedure FirstCmdExecute(Sender: TObject);
    procedure PrevCmdExecute(Sender: TObject);
    procedure NextCmdExecute(Sender: TObject);
    procedure LastCmdExecute(Sender: TObject);
    procedure ZoomCmdExecute(Sender: TObject);
    procedure PrintCmdExecute(Sender: TObject);
    procedure CloseCmdExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Fitto1Click(Sender: TObject);
    procedure SynEditPrintPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEditPrintPreviewPreviewPage(Sender: TObject;
      PageNumber: Integer);
    procedure ProcessHint(var HintStr: string; var CanShow: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrintPreviewDlg: TPrintPreviewDlg;

implementation

{$R *.DFM}

procedure TPrintPreviewDlg.FormShow(Sender: TObject);
begin
  SynEditPrintPreview.UpdatePreview;
  SynEditPrintPreview.FirstPage;
  if Printer.PrinterIndex >= 0 then
    PrintCmd.Hint := 'Print (' + Printer.Printers[Printer.PrinterIndex] +
      ')|Print the document on ' + Printer.Printers[Printer.PrinterIndex];
end;

procedure TPrintPreviewDlg.FirstCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.FirstPage;
end;

procedure TPrintPreviewDlg.PrevCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.PreviousPage;
end;

procedure TPrintPreviewDlg.NextCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.NextPage;
end;

procedure TPrintPreviewDlg.LastCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.LastPage;
end;

procedure TPrintPreviewDlg.ZoomCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.ScaleMode := pscWholePage;
end;

procedure TPrintPreviewDlg.PrintCmdExecute(Sender: TObject);
begin
  SynEditPrintPreview.Print;
end;

procedure TPrintPreviewDlg.ProcessHint(var HintStr: string;
  var CanShow: Boolean);
begin
  LeftStatusLabel.Caption := '  ' + HintStr;
  CanShow := True;
end;

procedure TPrintPreviewDlg.CloseCmdExecute(Sender: TObject);
begin
  Close;
end;

procedure TPrintPreviewDlg.Fitto1Click(Sender: TObject);
begin
  case (Sender as TSpTBXItem).Tag of
    -1: SynEditPrintPreview.ScaleMode := pscWholePage;
    -2: SynEditPrintPreview.ScaleMode := pscPageWidth;
  else
    SynEditPrintPreview.ScalePercent := (Sender as TSpTBXItem).Tag;
  end;
end;

procedure TPrintPreviewDlg.SynEditPrintPreviewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  FScale: Integer;
begin
  FScale := SynEditPrintPreview.ScalePercent;
  if Button = mbLeft then begin
    if SynEditPrintPreview.ScaleMode = pscWholePage then
      SynEditPrintPreview.ScalePercent := 100
    else begin
      FScale := FScale * 2;
      if FScale > 400 then
        FScale := 400;
      SynEditPrintPreview.ScalePercent := FScale;
    end;
  end
  else begin
    FScale := FScale div 2;
    if FScale < 25 then
      FScale := 25;
    SynEditPrintPreview.ScalePercent := FScale;
  end;
end;

procedure TPrintPreviewDlg.SynEditPrintPreviewPreviewPage(
  Sender: TObject; PageNumber: Integer);
begin
  RightStatusLabel.Caption := ' Page: ' + IntToStr(SynEditPrintPreview.PageNumber);
end;

end.
