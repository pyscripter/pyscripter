{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirFrm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvDirectoryListForm.pas,v 1.16 2004/08/24 13:33:08 asnepvangers Exp $

unit dlgDirectoryList;

interface

uses
  SysUtils, Classes, Windows, Controls, Forms, StdCtrls, TBXDkPanels,
  SpTBXControls;

type
  TJvDirectoryListDialog = class(TForm)
    DirectoryList: TListBox;
    AddBtn: TSpTBXButton;
    RemoveBtn: TSpTBXButton;
    ModifyBtn: TSpTBXButton;
    OKBtn: TSpTBXButton;
    CancelBtn: TSpTBXButton;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure DirectoryListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DirectoryListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    procedure CheckButtons;
  end;

function EditFolderList(Folders: TStrings; FormCaption : string = 'Directory List';
  HelpCntxt : integer = 0): Boolean;

implementation

uses
  JvJVCLUtils, JvJCLUtils, JvBrowseFolder, JvConsts, JVBoxProcs,
  dmCommands;

{$R *.dfm}

function EditFolderList(Folders: TStrings; FormCaption : string = 'Directory List';
  HelpCntxt : integer = 0): Boolean;
var
  I: Integer;
begin
  with TJvDirectoryListDialog.Create(Application) do
  try
    Caption := FormCaption;
    HelpContext := HelpCntxt;
    if Assigned(Folders) then
      for I := 0 to Folders.Count - 1 do
        DirectoryList.Items.Add(Folders[I]);
    Result := ShowModal = mrOK;
    if Result and Assigned(Folders) then
    begin
      Folders.Clear;
      for I := 0 to DirectoryList.Items.Count - 1 do
        Folders.Add(DirectoryList.Items[I]);
    end;
  finally
    Free;
  end;
end;

//=== { TJvDirectoryListDialog } =============================================

procedure TJvDirectoryListDialog.CheckButtons;
begin
  ModifyBtn.Enabled := (DirectoryList.Items.Count > 0) and
    (DirectoryList.ItemIndex >= 0);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
end;

procedure TJvDirectoryListDialog.AddBtnClick(Sender: TObject);
var
  S: string;
begin
  S := '';
  if BrowseDirectory(S, 'Select Directory to Add:', 0) then
  begin
    if DirectoryList.Items.IndexOf(S) < 0 then begin
      DirectoryList.AddItem(S, nil);
      DirectoryList.ItemIndex := DirectoryList.Count -1
    end;
    CheckButtons;
  end;
end;

procedure TJvDirectoryListDialog.ModifyBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if DirectoryList.ItemIndex < 0 then
    Exit;
  I := DirectoryList.ItemIndex;
  S := DirectoryList.Items[I];
  if BrowseDirectory(S, 'Select Directory:', 0) then
    DirectoryList.Items[I] := S;
end;

procedure TJvDirectoryListDialog.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  if DirectoryList.ItemIndex < 0 then
    Exit;
  I := DirectoryList.ItemIndex;
  DirectoryList.Items.Delete(I);
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvDirectoryListDialog.FormShow(Sender: TObject);
begin
  CheckButtons;
  CommandsDataModule.Images.GetIcon(84, Icon)
end;

procedure TJvDirectoryListDialog.DirectoryListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  BoxMoveFocusedItem(DirectoryList, DirectoryList.ItemAtPos(Point(X, Y), True));
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DirectoryList, Source, X, Y, State, Accept, DirectoryList.Sorted);
  CheckButtons;
end;

end.

