{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockInfo.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvDockInfo.pas 11252 2007-04-05 22:12:55Z remkobonte $

unit JvDockInfo;

{$I jvcl.inc}

interface

uses
  {$IFDEF USEJVCL}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF USEJVCL}
  Windows, IniFiles, Registry, Classes, Controls, Forms,
  {$IFDEF USEJVCL}
  JvAppStorage,
  {$ENDIF USEJVCL}
  JvDockControlForm, JvDockSupportClass, JvDockSupportProc;

type
  TJvDockInfoTree = class;

  TJvDockFormStyle = (dsNormal, dsConjoin, dsTab, dsDockPanel);

  TJvDockInfoZone = class(TJvDockBaseZone)
  private
    FDockFormName: string;
    FParentName: string;
    FDockRect: TRect;
    FLastDockSiteName: string;
    FUnDockLeft: Integer;
    FUnDockTop: Integer;
    FLRDockWidth: Integer;
    FTBDockHeight: Integer;
    FUnDockWidth: Integer;
    FUnDockHeight: Integer;
    FVSPaneWidth: Integer;
    FVisible: Boolean;
    FBorderStyle: TBorderStyle;
    FFormStyle: TFormStyle;
    FWindowState: TWindowState;
    FCanDocked: Boolean;
    FEachOtherDocked: Boolean;
    FLeftDocked: Boolean;
    FTopDocked: Boolean;
    FRightDocked: Boolean;
    FBottomDocked: Boolean;
    FCustomDocked: Boolean; {NEW! Contains custom dock panel! }
    FDockFormStyle: TJvDockFormStyle;
    FDockClientData: string;
    FDockControl: TWinControl;
    function GetChildControlCount: Integer;
  public
    procedure SetDockInfoFromControlToNode(Control: TControl); virtual;
    procedure SetDockInfoFromNodeToControl(Control: TControl); virtual;
    procedure SetDockInfoFromDockControlToNode(DockControl: TJvDockBaseControl); virtual;
    procedure SetDockInfoFromNodeToDockControl(DockControl: TJvDockBaseControl); virtual;
    property DockFormName: string read FDockFormName write FDockFormName;
    property ParentName: string read FParentName write FParentName;
    property DockRect: TRect read FDockRect write FDockRect;
    property LastDockSiteName: string read FLastDockSiteName write FLastDockSiteName;
    property UnDockLeft: Integer read FUnDockLeft write FUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write FUnDockTop;
    property LRDockWidth: Integer read FLRDockWidth write FLRDockWidth;
    property TBDockHeight: Integer read FTBDockHeight write FTBDockHeight;
    property UnDockWidth: Integer read FUnDockWidth write FUnDockWidth;
    property UnDockHeight: Integer read FUnDockHeight write FUnDockHeight;
    property VSPaneWidth: Integer read FVSPaneWidth write FVSPaneWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property Visible: Boolean read FVisible write FVisible;
    property CanDocked: Boolean read FCanDocked write FCanDocked;
    property EachOtherDocked: Boolean read FEachOtherDocked write FEachOtherDocked;
    property LeftDocked: Boolean read FLeftDocked write FLeftDocked;
    property TopDocked: Boolean read FTopDocked write FTopDocked;
    property RightDocked: Boolean read FRightDocked write FRightDocked;
    property BottomDocked: Boolean read FBottomDocked write FBottomDocked;
    property CustomDocked: Boolean read FCustomDocked write FCustomDocked; {NEW! Contains custom dock panel! }
    property DockFormStyle: TJvDockFormStyle read FDockFormStyle write FDockFormStyle;
    property DockClientData: string read FDockClientData write FDockClientData;
    property DockControl: TWinControl read FDockControl write FDockControl;
  end;

  // TJvDockInfoStyle enumerates the mode that is used when you call
  // TJvDockInfoTree.ScanTreeZone. This is a part of the code used to
  // implement persistence (loading and saving of docking layouts).
  //
  TJvDockInfoStyle =
    (isNone,  {  No mode set }
     {$IFDEF USEJVCL}
     isJVCLReadInfo,  { Mode for this scan is JVCL App Storage Load }
     isJVCLWriteInfo, { Mode for this scan is JVCL App Storage Save }
     {$ENDIF USEJVCL}
     isReadFileInfo,  { Mode for this scan is Text File Read.  Backwards compatible.  }
     isWriteFileInfo, { Mode for this scan is Text File Write. Backwards compatible.  }

     isReadRegInfo,   { Mode for this scan is registry Read }
     isWriteRegInfo); { Mode for this scan is registry Write }

  { JvDockInfoTree contains information about the docking tree.  It is created
    as part of the persistence framework for the JvDocking components. In order
    to save or load docking layout you must create one of these objects and use
    it to store the information about the set of docked forms being managed by
    JvDocking. }
  TJvDockInfoTree = class(TJvDockBaseTree)
  private
    {$IFDEF USEJVCL}
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    {$ENDIF USEJVCL}
    FDockInfoIni: TCustomIniFile;
    FDockInfoReg: TRegistry;
    FRegName: string;
    FJvDockInfoStyle: TJvDockInfoStyle; { Which action to do when doing a ScanTreeZone() recursive operation over the document tree. }
    FDataStream: TMemoryStream;
    function FindDockForm(const FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
  protected
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); override;
    {$IFDEF USEJVCL}
    procedure CreateZoneAndAddInfoFromAppStorage; virtual;
    {$ENDIF USEJVCL}
    procedure CreateZoneAndAddInfoFromIni; virtual;
    procedure CreateZoneAndAddInfoFromReg; virtual;
    procedure SetDockControlInfo(ATreeZone: TJvDockInfoZone); virtual;
  public
    constructor Create(TreeZone: TJvDockTreeZoneClass); override;
    destructor Destroy; override;

    // This is the most important function in this class, it basically
    // puts the important information from the application form into this
    // object.
    procedure CreateZoneAndAddInfoFromApp(Control: TControl); virtual;

    {$IFDEF USEJVCL}
    procedure ReadInfoFromAppStorage;
    procedure WriteInfoToAppStorage;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
    {$ENDIF USEJVCL}
    procedure ReadInfoFromIni;
    procedure ReadInfoFromReg(const RegName: string);
    procedure WriteInfoToIni;
    procedure WriteInfoToReg(const RegName: string);
    property DockInfoIni: TCustomIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
  end;

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/tags/JVCL3_32/run/JvDockInfo.pas $';
    Revision: '$Revision: 11252 $';
    Date: '$Date: 2007-04-06 00:12:55 +0200 (ven., 06 avr. 2007) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

implementation

uses
  SysUtils,
  JvDockGlobals, JvDockVSNetStyle;

//=== Local procedures =======================================================

function FindDockForm(const FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then
    Result := nil
  else
    Result := JvDockFindDockFormWithName(FormName);
end;

function FindDockPanel(const ControlName: string): TWinControl;
var
  Index: Word;
  DockServer: TJvDockServer;
begin
  Result := nil;
  Index := Pos(RsDockJvDockInfoSplitter, ControlName);
  if Index = 0 then
    Exit;
  Result := FindDockForm(Copy(ControlName, 1, Index - 1));
  if Result <> nil then
  begin
    DockServer := FindDockServer(Result);
    if DockServer <> nil then
      with DockServer do
      begin
        if Pos('TopDockPanel', ControlName) > Index then
          Result := TopDockPanel
        else
        if Pos('LeftDockPanel', ControlName) > Index then
          Result := LeftDockPanel
        else
        if Pos('BottomDockPanel', ControlName) > Index then
          Result := BottomDockPanel
        else
        if Pos('RightDockPanel', ControlName) > Index then
          Result := RightDockPanel
        else
        if Pos('CustomDockPanel', ControlName) > Index then
          Result := CustomDockPanel;

        // Mantis 3603: No more AV, Result may not always be a TJvDockVSNETPanel
        if (Result is TJvDockVSNETPanel) and (Pos('PopupPanel', ControlName) > 20) then
          Result := (Result as TJvDockVSNETPanel).VSChannel.VSPopupPanel;
      end;
  end;
end;

function FindDockHost(const ControlName: string): TWinControl;
begin
  Result := FindDockForm(ControlName);
  if Result = nil then
    Result := FindDockPanel(ControlName);
end;

//=== { TJvDockInfoTree } ====================================================

constructor TJvDockInfoTree.Create(TreeZone: TJvDockTreeZoneClass);
begin
  inherited Create(TreeZone);
  {$IFNDEF USEJVCL}
  FDockInfoIni := nil;
  FDockInfoReg := nil;
  {$ENDIF !USEJVCL}
  FJvDockInfoStyle := isNone;
  FDataStream := TMemoryStream.Create;
end;

destructor TJvDockInfoTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDataStream);
end;

{ Create an TJvDockConjoinHostForm or  TJvDockTabHostForm when restoring a docking layout }
function TJvDockInfoTree.CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
var
  Form: TForm;
  ADockClient: TJvDockClient;
begin
  { The dockinfo data that is saved, contains names of the values of ChildZone.DockControl
    Thus on loading it can be that no form with that DockControl name can be found;
    then DockControl will be nil
  }
  Result := nil;
  case ATreeZone.DockFormStyle of
    dsConjoin:
      if Assigned(TJvDockInfoZone(ATreeZone.ChildZone).DockControl) then
      begin
        Form := TJvDockConjoinHostForm.Create(Application);
        ADockClient := FindDockClient(TJvDockInfoZone(ATreeZone.ChildZone).DockControl);
        Result := ADockClient.CreateConjoinPanelClass(Form).Parent;
      end;
    dsTab:
      if Assigned(TJvDockInfoZone(ATreeZone.ChildZone).DockControl) then
      begin
        Form := TJvDockTabHostForm.Create(Application);
        ADockClient := FindDockClient(TJvDockInfoZone(ATreeZone.ChildZone).DockControl);
        Result := ADockClient.CreateTabDockClass(Form).Parent;
      end;
  end;
  if Result <> nil then
    Result.Name := ATreeZone.DockFormName;
end;

// CreateZoneAndAddInfoFromApp
//
// Control: TControl - note this is probably actually a TForm
//                    descendant, since this library only supports form docking.
//
// This is the most important function in this class, it basically
// puts the important information from the application form into this
// object.
//
// This is used to take a form that is docked somewhere and extract all the
// docking layout information contained inside it, and add it to this JvDockInfoTree
// object, which can then be iterated through, stored to disk, etc. }

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromApp(Control: TControl);
var
  I: TJvDockPosition; {was TAlign}
  J: Integer;
  TreeZone: TJvDockInfoZone;
  DockBaseControl: TJvDockBaseControl;
  TmpDockPanel: TJvDockPanel;
begin
  TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
  with TreeZone do
  begin
    ParentName := TJvDockInfoZone(CurrTreeZone).DockFormName;
    SetDockInfoFromControlToNode(Control);
    if Control is TJvDockPanel then
      DockFormName := TJvDockInfoZone(CurrTreeZone).DockFormName +
        RsDockJvDockInfoSplitter + Control.Name
    else
      DockFormName := Control.Name;
    FDataStream.Clear;
    if Control is TJvDockTabHostForm then
      TJvDockTabHostForm(Control).PageControl.SaveToStream(FDataStream)
    else
    if Control is TJvDockConjoinHostForm then
      TJvDockConjoinHostForm(Control).Panel.DockManager.SaveToStream(FDataStream)
    else
    if Control is TJvDockPanel then
      TJvDockPanel(Control).DockManager.SaveToStream(FDataStream);
    DockClientData := JvDockStreamDataToString(FDataStream);
    DockBaseControl := FindDockBaseControl(Control);
    if DockBaseControl <> nil then
    begin
      SetDockInfoFromDockControlToNode(DockBaseControl);
      if Control is TJvDockTabHostForm then
        DockFormStyle := dsTab
      else
      if Control is TJvDockConjoinHostForm then
        DockFormStyle := dsConjoin
      else
        DockFormStyle := dsNormal;
      if DockBaseControl is TJvDockClient then
      begin
        if Control is TJvDockableForm then
          with TJvDockableForm(Control).DockableControl do
            for J := 0 to DockClientCount - 1 do
            begin
              CurrTreeZone := TreeZone;
              CreateZoneAndAddInfoFromApp(DockClients[J]);
              CurrTreeZone := TreeZone.GetParentZone;
            end;
      end
      else
      begin
        // Changed to persist ALL DockPanels, not just Top,Left,Right,Bottom.
        // This is a hardcoded assumption throughout the component that is
        // proving hard to overcome.
        for I := Low(TJvDockPosition) to High(TJvDockPosition) do // There are 5 TJvDockPositions now ! {NEW!}
        begin
          CurrTreeZone := TreeZone;
          TmpDockPanel := TJvDockServer(DockBaseControl).DockPanel[I];
          if Assigned(TmpDockPanel) then
          begin
            CreateZoneAndAddInfoFromApp(TmpDockPanel);
            if TmpDockPanel is TJvDockVSNETPanel then // JvDockVSNetStyle specific:
              CreateZoneAndAddInfoFromApp(TJvDockVSNETPanel(TmpDockPanel).VSChannel.VSPopupPanel);
          end;
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;

    if Control is TJvDockPanel then
    begin
      DockFormStyle := dsDockPanel;
      if Control is TJvDockVSPopupPanel then
        with TJvDockVSPopupPanel(Control) do
          for J := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[J]));
            CurrTreeZone := TreeZone.GetParentZone;
          end
      else
        with TJvDockPanel(Control) do
          for J := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[J]));
            CurrTreeZone := TreeZone.GetParentZone;
          end;
    end;
  end;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromAppStorage;
var
  FormList: TStringList;
  CP, CP1: PChar;
  S: string;
  I: Integer;
  OldPath: string;
  OldDefaultIfValueNotExists : Boolean;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    if FAppStorage.PathExists(FormList[Index]) then
    begin
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, FAppStorage do
      begin
        { Move down into the folder of the form.. }
        Path := ConcatPaths([Path, FormList[Index]]);

        DockFormName := FormList[Index];
        ParentName := ReadString('ParentName');
        DockRect := Rect(ReadInteger('DockLeft'), ReadInteger('DockTop'),
          ReadInteger('DockRight'), ReadInteger('DockBottom'));
        LRDockWidth := ReadInteger('LRDockWidth');
        LastDockSiteName := ReadString('LastDockSiteName');
        UnDockLeft := ReadInteger('UnDockLeft');
        UnDockTop := ReadInteger('UnDockTop');
        TBDockHeight := ReadInteger('TBDockHeight');
        UnDockWidth := ReadInteger('UnDockWidth');
        UnDockHeight := ReadInteger('UnDockHeight');
        VSPaneWidth := ReadInteger('VSPaneWidth');
        Visible := ReadBoolean('Visible');
        BorderStyle := TBorderStyle(ReadInteger('BorderStyle'));
        FormStyle := TFormStyle(ReadInteger('FormStyle'));
        WindowState := TWindowState(ReadInteger('WindowState'));
        DockFormStyle := TJvDockFormStyle(ReadInteger('DockFormStyle'));
        CanDocked := ReadBoolean('CanDocked');
        EachOtherDocked := ReadBoolean('EachOtherDocked');
        LeftDocked := ReadBoolean('LeftDocked');
        TopDocked := ReadBoolean('TopDocked');
        RightDocked := ReadBoolean('RightDocked');
        BottomDocked := ReadBoolean('BottomDocked');
        CustomDocked := ReadBoolean('CustomDocked'); {NEW}
        DockClientData := ReadString('DockClientData');

        { ..and move up a level }
        Path := ConcatPaths([Path, '..']);
      end;
      for I := Index - 1 downto 0 do
      begin
        { Search for forms that have this form (FormList[I]) as parent }
        if FAppStorage.ReadString(FAppStorage.ConcatPaths([FormList[I], 'ParentName'])) = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(I);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;

begin
  FormList := TStringList.Create;
  FJvDockInfoStyle := isJVCLReadInfo; // set mode for Scan.
  try
    { Normally, we wouldn't find duplicate names, but if so ignore them otherwise havoc }
    FormList.Duplicates := dupIgnore;
    OldPath := FAppStorage.Path;
    OldDefaultIfValueNotExists := FAppStorage.StorageOptions.DefaultIfValueNotExists;
    FAppStorage.StorageOptions.DefaultIfValueNotExists := True;
    try
      FAppStorage.Path := FAppStorage.ConcatPaths([FAppStorage.Path, AppStoragePath, 'Forms']);
      if FAppStorage.ValueStored('FormNames') then
      begin
        S := FAppStorage.ReadString('FormNames');
        { UniqueString is used because we modify the contents of S after
          casting S to a PChar. S might point to an actual string in a storage,
          as is the case with TJvAppXMLFileStorage. Not using UniqueString would
          change the value in the storage too. }
        UniqueString(S);
        CP := PChar(S);
        CP1 := StrPos(CP, ';');
        while CP1 <> nil do
        begin
          CP1^ := #0;
          FormList.Add(string(CP));
          CP := CP1 + 1;
          CP1 := StrPos(CP, ';');
        end;
        for I := FormList.Count - 1 downto 0 do
          if FAppStorage.ReadString(FAppStorage.ConcatPaths([FormList[I], 'ParentName'])) = '' then
            CreateZoneAndAddInfo(I);
      end;
    finally
      FAppStorage.Path := OldPath;
      FAppStorage.StorageOptions.DefaultIfValueNotExists := OldDefaultIfValueNotExists;
    end;
  finally
    FormList.Free;
    FJvDockInfoStyle := isNone;
  end;
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromIni;
var
  I: Integer;
  Sections: TStringList;
  TempDockInfoZoneArray: array of TJvDockInfoZone;

  procedure CreateTempDockInfoZoneArray;
  var
    I: Integer;
  begin
    SetLength(TempDockInfoZoneArray, SizeOf(TJvDockInfoZone) * Sections.Count);
    for I := 0 to Sections.Count - 1 do
    begin
      TempDockInfoZoneArray[I] := TJvDockInfoZone.Create(nil);
      with TempDockInfoZoneArray[I], DockInfoIni do
      begin
        DockFormName := Sections[I];
        ParentName := ReadString(DockFormName, 'ParentName', 'ERROR');
        DockRect := Rect(ReadInteger(DockFormName, 'DockLeft', 0),
          ReadInteger(DockFormName, 'DockTop', 0),
          ReadInteger(DockFormName, 'DockRight', 100),
          ReadInteger(DockFormName, 'DockBottom', 100));
        LastDockSiteName := ReadString(DockFormName, 'LastDockSiteName', 'ERROR');
        UnDockLeft := ReadInteger(DockFormName, 'UnDockLeft', 100);
        UnDockTop := ReadInteger(DockFormName, 'UnDockTop', 100);
        LRDockWidth := ReadInteger(DockFormName, 'LRDockWidth', 100);
        TBDockHeight := ReadInteger(DockFormName, 'TBDockHeight', 100);
        UnDockWidth := ReadInteger(DockFormName, 'UnDockWidth', 100);
        UnDockHeight := ReadInteger(DockFormName, 'UnDockHeight', 100);
        VSPaneWidth := ReadInteger(DockFormName, 'VSPaneWidth', 100);
        Visible := ReadBool(DockFormName, 'Visible', True);
        BorderStyle := TBorderStyle(ReadInteger(DockFormName, 'BorderStyle', 0));
        FormStyle := TFormStyle(ReadInteger(DockFormName, 'FormStyle', 0));
        WindowState := TWindowState(ReadInteger(DockFormName, 'WindowState', 0));
        DockFormStyle := TJvDockFormStyle(ReadInteger(DockFormName, 'DockFormStyle', 0));
        CanDocked := ReadBool(DockFormName, 'CanDocked', True);
        EachOtherDocked := ReadBool(DockFormName, 'EachOtherDocked', True);
        LeftDocked := ReadBool(DockFormName, 'LeftDocked', LeftDocked);
        TopDocked := ReadBool(DockFormName, 'TopDocked', True);
        RightDocked := ReadBool(DockFormName, 'RightDocked', True);
        BottomDocked := ReadBool(DockFormName, 'BottomDocked', True);
        CustomDocked := ReadBool(DockFormName, 'CustomDocked', True);
        DockClientData := ReadString(DockFormName, 'DockClientData', '');
      end;
    end;
  end;

  procedure DestroyTempDockInfoZoneArray;
  var
    I: Integer;
  begin
    for I := Sections.Count - 1 downto 0 do
      TempDockInfoZoneArray[I].Free;
  end;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));

    with TempDockInfoZoneArray[Index] do
    begin
      TreeZone.DockFormName := DockFormName;
      TreeZone.ParentName := ParentName;
      TreeZone.DockRect := DockRect;
      TreeZone.LastDockSiteName := LastDockSiteName;
      TreeZone.UnDockLeft := UnDockLeft;
      TreeZone.UnDockTop := UnDockTop;
      TreeZone.LRDockWidth := LRDockWidth;
      TreeZone.TBDockHeight := TBDockHeight;
      TreeZone.UnDockWidth := UnDockWidth;
      TreeZone.UnDockHeight := UnDockHeight;
      TreeZone.VSPaneWidth := VSPaneWidth;
      TreeZone.Visible := Visible;
      TreeZone.BorderStyle := BorderStyle;
      TreeZone.FormStyle := FormStyle;
      TreeZone.WindowState := WindowState;
      TreeZone.DockFormStyle := DockFormStyle;
      TreeZone.CanDocked := CanDocked;
      TreeZone.EachOtherDocked := EachOtherDocked;
      TreeZone.LeftDocked := LeftDocked;
      TreeZone.TopDocked := TopDocked;
      TreeZone.RightDocked := RightDocked;
      TreeZone.BottomDocked := BottomDocked;
      TreeZone.CustomDocked := CustomDocked; {NEW!}
      TreeZone.DockClientData := DockClientData;
    end;

    for I := Index - 1 downto 0 do
      if TempDockInfoZoneArray[I].ParentName = Sections[Index] then
      begin
        CurrTreeZone := TreeZone;
        CreateZoneAndAddInfo(I);
        CurrTreeZone := TreeZone.GetParentZone;
      end;
  end;

begin
  Sections := TStringList.Create;
  try
    DockInfoIni.ReadSections(Sections);
    CreateTempDockInfoZoneArray;
    for I := Sections.Count - 1 downto 0 do
      if TempDockInfoZoneArray[I].ParentName = '' then
        CreateZoneAndAddInfo(I);
    FJvDockInfoStyle := isNone;
  finally
    DestroyTempDockInfoZoneArray;
    Sections.Free;
  end;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromReg;
var
  FormList: TStringList;
  CP, CP1: PChar;
  I: Integer;
  S: string;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    DockInfoReg.OpenKey(FRegName, False);
    if DockInfoReg.KeyExists(FormList[Index]) then
    begin
      DockInfoReg.OpenKey(FRegName + '\' + FormList[Index], False);
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, DockInfoReg do
      begin
        DockFormName := FormList[Index];
        ParentName := ReadString('ParentName');
        DockRect := Rect(ReadInteger('DockLeft'), ReadInteger('DockTop'),
          ReadInteger('DockRight'), ReadInteger('DockBottom'));
        LRDockWidth := ReadInteger('LRDockWidth');
        LastDockSiteName := ReadString('LastDockSiteName');
        UnDockLeft := ReadInteger('UnDockLeft');
        UnDockTop := ReadInteger('UnDockTop');
        TBDockHeight := ReadInteger('TBDockHeight');
        UnDockWidth := ReadInteger('UnDockWidth');
        UnDockHeight := ReadInteger('UnDockHeight');
        VSPaneWidth := ReadInteger('VSPaneWidth');
        Visible := ReadBool('Visible');
        BorderStyle := TBorderStyle(ReadInteger('BorderStyle'));
        FormStyle := TFormStyle(ReadInteger('FormStyle'));
        WindowState := TWindowState(ReadInteger('WindowState'));
        DockFormStyle := TJvDockFormStyle(ReadInteger('DockFormStyle'));
        CanDocked := ReadBool('CanDocked');
        EachOtherDocked := ReadBool('EachOtherDocked');
        LeftDocked := ReadBool('LeftDocked');
        TopDocked := ReadBool('TopDocked');
        RightDocked := ReadBool('RightDocked');
        BottomDocked := ReadBool('BottomDocked');
        CustomDocked := ReadBool('CustomDocked'); {NEW!}
        DockClientData := ReadString('DockClientData');
      end;
      for I := Index - 1 downto 0 do
      begin
        DockInfoReg.OpenKey(FRegName + '\' + FormList[I], False);
        if DockInfoReg.ReadString('ParentName') = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(I);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;

begin
  FormList := TStringList.Create;
  try
    if DockInfoReg.OpenKey(FRegName, False) then
    begin
      S := DockInfoReg.ReadString('FormNames');
      CP := PChar(S);
      CP1 := StrPos(CP, '\');
      while CP1 <> nil do
      begin
        CP1^ := #0;
        FormList.Add(CP);
        CP := CP1 + 1;
        CP1 := StrPos(CP, '\');
      end;
      FJvDockInfoStyle := isReadFileInfo;
      for I := FormList.Count - 1 downto 0 do
      begin
        DockInfoReg.OpenKey(FRegName + '\' + FormList[I], False);
        if DockInfoReg.ReadString('ParentName') = '' then
          CreateZoneAndAddInfo(I);
      end;
      FJvDockInfoStyle := isNone;
    end;
  finally
    DockInfoReg.CloseKey;
    FormList.Free;
  end;
end;

function TJvDockInfoTree.FindDockForm(const FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then
    Result := nil
  else
    Result := JvDockFindDockFormWithName(FormName);
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.ReadInfoFromAppStorage;
begin
  AppStorage.BeginUpdate;
  try
    CreateZoneAndAddInfoFromAppStorage;
    DoFloatAllForm;
    // (rom) this is disputable
    Application.ProcessMessages;
    try
      FJvDockInfoStyle := isJVCLReadInfo;
      MiddleScanTree(TopTreeZone);
    finally
      FJvDockInfoStyle := isNone;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  {$IFDEF USEJVCL}
  FJvDockInfoStyle := isJVCLReadInfo;
  {$ELSE}
  FJvDockInfoStyle := isReadFileInfo;
  {$ENDIF USEJVCL}
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.ReadInfoFromReg(const RegName: string);
begin
  FRegName := RegName;
  CreateZoneAndAddInfoFromReg;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  FJvDockInfoStyle := isReadRegInfo;
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
  OldPath: string;

  procedure WriteIntegerIfNonZero(const Path: string; Value: Integer);
  begin
    if Value <> 0 then
      fAppStorage.WriteInteger(Path, Value);
  end;

  procedure WriteBooleanIfFalse(const Path: string; Value: Boolean);
  begin
    if not Value then
      fAppStorage.WriteBoolean(Path, Value);
  end;


begin
  if FJvDockInfoStyle = isJVCLReadInfo then { JVCL Mode persistance : READ }
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else
  if FJvDockInfoStyle = isJVCLWriteInfo then { JVCL Mode persistance : WRITE }
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), FAppStorage do
      begin
        OldPath := Path;
        try
          Path := ConcatPaths([Path, AppStoragePath, 'Forms']);
          WriteString('FormNames', ReadString('FormNames') + DockFormName + ';');
          Path := ConcatPaths([Path, DockFormName]);
          WriteString('ParentName', ParentName);
          WriteIntegerIfNonZero('DockLeft', DockRect.Left);
          WriteIntegerIfNonZero('DockTop', DockRect.Top);
          WriteIntegerIfNonZero('DockRight', DockRect.Right);
          WriteIntegerIfNonZero('DockBottom', DockRect.Bottom);
          WriteString('LastDockSiteName', LastDockSiteName);
          WriteIntegerIfNonZero('UnDockLeft', UnDockLeft);
          WriteIntegerIfNonZero('UnDockTop', UnDockTop);
          WriteIntegerIfNonZero('LRDockWidth', LRDockWidth);
          WriteIntegerIfNonZero('TBDockHeight', TBDockHeight);
          WriteIntegerIfNonZero('UnDockWidth', UnDockWidth);
          WriteIntegerIfNonZero('UnDockHeight', UnDockHeight);
          WriteIntegerIfNonZero('VSPaneWidth', VSPaneWidth);
          WriteBooleanIfFalse('Visible', Visible);
          WriteIntegerIfNonZero('BorderStyle', Integer(BorderStyle));
          WriteIntegerIfNonZero('FormStyle', Integer(FormStyle));
          WriteIntegerIfNonZero('WindowState', Integer(WindowState));
          WriteIntegerIfNonZero('DockFormStyle', Integer(DockFormStyle));
          WriteBooleanIfFalse('CanDocked', CanDocked);
          WriteBooleanIfFalse('EachOtherDocked', EachOtherDocked);
          WriteBooleanIfFalse('LeftDocked', LeftDocked);
          WriteBooleanIfFalse('TopDocked', TopDocked);
          WriteBooleanIfFalse('RightDocked', RightDocked);
          WriteBooleanIfFalse('BottomDocked', BottomDocked);
          WriteBooleanIfFalse('CustomDocked', CustomDocked); {NEW!}
          WriteString('DockClientData', DockClientData);
        finally
          FAppStorage.Path := OldPath;
        end;
      end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{$ELSE}

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
begin
//  FJvDockInfoStyle := isReadFileInfo;
  if (FJvDockInfoStyle = isReadFileInfo) or (FJvDockInfoStyle = isReadRegInfo) then
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else
  if FJvDockInfoStyle = isWriteFileInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoIni do
      begin
        WriteString(DockFormName, 'ParentName', ParentName);
        WriteInteger(DockFormName, 'DockLeft', DockRect.Left);
        WriteInteger(DockFormName, 'DockTop', DockRect.Top);
        WriteInteger(DockFormName, 'DockRight', DockRect.Right);
        WriteInteger(DockFormName, 'DockBottom', DockRect.Bottom);
        WriteString(DockFormName, 'LastDockSiteName', LastDockSiteName);
        WriteInteger(DockFormName, 'UnDockLeft', UnDockLeft);
        WriteInteger(DockFormName, 'UnDockTop', UnDockTop);
        WriteInteger(DockFormName, 'LRDockWidth', LRDockWidth);
        WriteInteger(DockFormName, 'TBDockHeight', TBDockHeight);
        WriteInteger(DockFormName, 'UnDockWidth', UnDockWidth);
        WriteInteger(DockFormName, 'UnDockHeight', UnDockHeight);
        WriteInteger(DockFormName, 'VSPaneWidth', VSPaneWidth);
        WriteBool(DockFormName, 'Visible', Visible);
        WriteInteger(DockFormName, 'BorderStyle', Integer(BorderStyle));
        WriteInteger(DockFormName, 'WindowState', Integer(WindowState));
        WriteInteger(DockFormName, 'FormStyle', Integer(FormStyle));
        WriteInteger(DockFormName, 'DockFormStyle', Integer(DockFormStyle));
        WriteBool(DockFormName, 'CanDocked', CanDocked);
        WriteBool(DockFormName, 'EachOtherDocked', EachOtherDocked);
        WriteBool(DockFormName, 'LeftDocked', LeftDocked);
        WriteBool(DockFormName, 'TopDocked', TopDocked);
        WriteBool(DockFormName, 'RightDocked', RightDocked);
        WriteBool(DockFormName, 'BottomDocked', BottomDocked);
        WriteBool(DockFormName, 'CustomDocked', CustomDocked); {NEW!}
        WriteString(DockFormName, 'DockClientData', DockClientData);
      end;
  end
  else
  if FJvDockInfoStyle = isWriteRegInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoReg do
      begin
        OpenKey(FRegName, True);
        WriteString('FormNames', ReadString('FormNames') + DockFormName + '\');
        OpenKey(FRegName + '\' + DockFormName, True);
        WriteString('ParentName', ParentName);
        WriteInteger('DockLeft', DockRect.Left);
        WriteInteger('DockTop', DockRect.Top);
        WriteInteger('DockRight', DockRect.Right);
        WriteInteger('DockBottom', DockRect.Bottom);
        WriteString('LastDockSiteName', LastDockSiteName);
        WriteInteger('UnDockLeft', UnDockLeft);
        WriteInteger('UnDockTop', UnDockTop);
        WriteInteger('LRDockWidth', LRDockWidth);
        WriteInteger('TBDockHeight', TBDockHeight);
        WriteInteger('UnDockWidth', UnDockWidth);
        WriteInteger('UnDockHeight', UnDockHeight);
        WriteInteger('VSPaneWidth', VSPaneWidth);
        WriteBool('Visible', Visible);
        WriteInteger('BorderStyle', Integer(BorderStyle));
        WriteInteger('FormStyle', Integer(FormStyle));
        WriteInteger('WindowState', Integer(WindowState));
        WriteInteger('DockFormStyle', Integer(DockFormStyle));
        WriteBool('CanDocked', CanDocked);
        WriteBool('EachOtherDocked', EachOtherDocked);
        WriteBool('LeftDocked', LeftDocked);
        WriteBool('TopDocked', TopDocked);
        WriteBool('RightDocked', RightDocked);
        WriteBool('BottomDocked', BottomDocked);
        WriteBool('CustomDocked', CustomDocked); {NEW!}
        WriteString('DockClientData', DockClientData);
        CloseKey;
      end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.SetDockControlInfo(ATreeZone: TJvDockInfoZone);
var
  DockBaseControl: TJvDockBaseControl;
  Host: TWinControl;
begin
  with ATreeZone do
  begin
    if DockFormName = '' then
      Exit;
    Host := FindDockHost(DockFormName);
    if (Host = nil) and (ATreeZone.GetChildControlCount > 1) then
      Host := CreateHostControl(ATreeZone);
    if (Host <> nil) and (DockClientData <> '') and (FDataStream <> nil) then
    begin
      FDataStream.Clear;

      JvDockStringToStreamData(FDataStream, DockClientData);

      FDataStream.Position := 0;
      if Host is TJvDockTabHostForm then
      begin
        with TJvDockTabHostForm(Host).PageControl do
        begin
          DisableAlign;
          try
            LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end
      else
      if Host is TJvDockConjoinHostForm then
      begin
        with TJvDockConjoinHostForm(Host).Panel do
        begin
          DisableAlign;
          try
            DockManager.LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end
      else
      if Host is TJvDockPanel then
      begin
        with TJvDockPanel(Host) do
        begin
          DisableAlign;
          try
            DockManager.LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
    if Host <> nil then
    begin
      SetDockInfoFromNodeToControl(Host);
      DockBaseControl := FindDockBaseControl(Host);
      if DockBaseControl <> nil then
        SetDockInfoFromNodeToDockControl(DockBaseControl);
    end;
  end;
end;

{$IFDEF USEJVCL}
procedure TJvDockInfoTree.WriteInfoToAppStorage;
begin
  AppStorage.BeginUpdate;
  try
    AppStorage.DeleteSubTree(AppStoragePath);
    try
      FJvDockInfoStyle := isJVCLWriteInfo;
      MiddleScanTree(TopTreeZone);
    finally
      FJvDockInfoStyle := isNone;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;
{$ENDIF USEJVCL}

procedure TJvDockInfoTree.WriteInfoToIni;
var
  Sections: TStringList;
  I: Integer;
begin
  Sections := TStringList.Create;
  try
    DockInfoIni.ReadSections(Sections);

    for I := 0 to Sections.Count - 1 do
      DockInfoIni.EraseSection(Sections[I]);
  finally
    Sections.Free;
  end;
  {$IFDEF USEJVCL}
  FJvDockInfoStyle := isJVCLWriteInfo;
  {$ELSE}
  FJvDockInfoStyle := isWriteFileInfo;
  {$ENDIF USEJVCL}
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.WriteInfoToReg(const RegName: string);
begin
  try
    if DockInfoReg.OpenKey(RegName, False) then
      DockInfoReg.DeleteKey(RegName);

    DockInfoReg.CreateKey(RegName);
    DockInfoReg.CloseKey;
    FRegName := RegName;

    FJvDockInfoStyle := isWriteRegInfo;
    MiddleScanTree(TopTreeZone);
    FJvDockInfoStyle := isNone;
  finally
    DockInfoReg.CloseKey;
  end;
end;

//=== { TJvDockInfoZone } ====================================================

function TJvDockInfoZone.GetChildControlCount: Integer;
var
  Zone: TJvDockBaseZone;
begin
  Result := 0;
  if ChildZone <> nil then
  begin
    Inc(Result);
    Zone := ChildZone;
    while Zone.NextSibling <> nil do
    begin
      Zone := Zone.NextSibling;
      if TJvDockInfoZone(Zone).DockControl <> nil then
        Inc(Result);
    end;
  end;
end;

procedure TJvDockInfoZone.SetDockInfoFromControlToNode(Control: TControl);
begin
  DockRect := Control.BoundsRect;
  UnDockWidth := Control.UnDockWidth;
  UnDockHeight := Control.UnDockHeight;
  if Control is TJvDockVSPopupPanel then
    Control.Visible := False
  else
    Visible := Control.Visible;

  if Control is TForm then
  begin
    BorderStyle := TForm(Control).BorderStyle;
    FormStyle := TForm(Control).FormStyle;
    WindowState := TForm(Control).WindowState;
    LRDockWidth := Control.LRDockWidth;
    TBDockHeight := Control.TBDockHeight;
  end;
end;

procedure TJvDockInfoZone.SetDockInfoFromDockControlToNode(DockControl: TJvDockBaseControl);

  function GetLastDockSiteName(AControl: TControl): string;
  begin
    Result := RsDockCannotFindWindow;
    if AControl <> nil then
    begin
      if AControl.Parent is TJvDockableForm then
        Result := AControl.Parent.Name
      else
      if AControl is TJvDockPanel then
        Result := AControl.Parent.Name + RsDockJvDockInfoSplitter + AControl.Name;
    end;
  end;

begin
  CanDocked := DockControl.EnableDock;
  EachOtherDocked := DockControl.EachOtherDock;
  LeftDocked := DockControl.LeftDock;
  TopDocked := DockControl.TopDock;
  RightDocked := DockControl.RightDock;
  BottomDocked := DockControl.BottomDock;
  CustomDocked := DockControl.CustomDock; {NEW!}

  if DockControl is TJvDockClient then
  begin
    VSPaneWidth := TJvDockClient(DockControl).VSPaneWidth;
    UnDockLeft := TJvDockClient(DockControl).UnDockLeft;
    UnDockTop := TJvDockClient(DockControl).UnDockTop;
    LastDockSiteName := GetLastDockSiteName(TJvDockClient(DockControl).LastDockSite);
  end
  else
    VSPaneWidth := 0;
end;

{ When restoring a Control (form) properties when loading layout, this sets one form's properties.}

procedure TJvDockInfoZone.SetDockInfoFromNodeToControl(Control: TControl);
var
  DS: TJvDockServer;

  procedure SetPopupPanelSize(PopupPanel: TJvDockVSPopupPanel);
  begin
  end;

  procedure SetDockSiteSize(DockSite: TJvDockPanel);
  begin
    if DockSite.Align in [alTop, alBottom] then
      DockSite.JvDockManager.DockSiteSize := DockRect.Bottom - DockRect.Top
    else
      DockSite.JvDockManager.DockSiteSize := DockRect.Right - DockRect.Left;
  end;

begin
  if (ParentName = '') or ((Control is TJvDockPanel) and
    (TJvDockPanel(Control).VisibleDockClientCount > 0)) then
  begin
    TWinControl(Control).DisableAlign;
    try
      if Control is TForm then
      begin
        TForm(Control).BorderStyle := BorderStyle;
        TForm(Control).FormStyle := FormStyle;
        if WindowState = wsNormal then
          Control.BoundsRect := DockRect;
        TForm(Control).WindowState := WindowState;
      end
      else
      begin
        if Control is TJvDockVSPopupPanel then
          SetPopupPanelSize(Control as TJvDockVSPopupPanel)
        else
          SetDockSiteSize(Control as TJvDockPanel);
      end;
      DS := FindDockServer(Control);
      if DS <> nil then
      begin
        DS.GetClientAlignControl(alTop);
        DS.GetClientAlignControl(alBottom);
        DS.GetClientAlignControl(alLeft);
        DS.GetClientAlignControl(alRight);
      end;
    finally
      TWinControl(Control).EnableAlign;
    end;
  end;
//  //  KV to avoid flickering in Vista
//  if not ((Control is TForm) and (ParentName <> '')) then
  Control.Visible := Visible;
  Control.LRDockWidth := LRDockWidth;
  Control.TBDockHeight := TBDockHeight;
  Control.UnDockHeight := UnDockHeight;
  Control.UnDockWidth := UnDockWidth;
end;

{ Restores settings in the TjvDockClient inside the form, when loading docking layout. }

procedure TJvDockInfoZone.SetDockInfoFromNodeToDockControl(DockControl: TJvDockBaseControl);

  function GetLastDockSite(const AName: string): TWinControl;
  begin
    Result := FindDockPanel(AName);
    if Result = nil then
    begin
      Result := FindDockForm(AName);
      if Result is TJvDockableForm then
        Result := TJvDockableForm(Result).DockableControl;
    end;
  end;

begin
  if DockControl is TJvDockClient then
  begin
    TJvDockClient(DockControl).UnDockLeft := UnDockLeft;
    TJvDockClient(DockControl).UnDockTop := UnDockTop;
    TJvDockClient(DockControl).LastDockSite := GetLastDockSite(LastDockSiteName);
    if Visible then
    begin
      TJvDockClient(DockControl).ParentVisible := False;
      TJvDockClient(DockControl).MakeShowEvent;
    end
    else
      TJvDockClient(DockControl).MakeHideEvent;
    TJvDockClient(DockControl).VSPaneWidth := VSPaneWidth;
  end;
  DockControl.EnableDock := CanDocked;
  DockControl.LeftDock := LeftDocked;
  DockControl.TopDock := TopDocked;
  DockControl.BottomDock := BottomDocked;
  DockControl.CustomDock := CustomDocked; {NEW!}
  DockControl.RightDock := RightDocked;
end;

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

end.

