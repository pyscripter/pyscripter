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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockInfo;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, IniFiles, Registry, Classes, Controls, Forms,
  JvAppStorage, JvDockControlForm, JvDockSupportClass, JvDockSupportProc;

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
     isJVCLReadInfo,  { Mode for this scan is JVCL App Storage Load }
     isJVCLWriteInfo, { Mode for this scan is JVCL App Storage Save }

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
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FDockInfoIni: TCustomIniFile;
    FDockInfoReg: TRegistry;
    FRegName: string;
    FJvDockInfoStyle: TJvDockInfoStyle; { Which action to do when doing a ScanTreeZone() recursive operation over the document tree. }
    FDataStream: TMemoryStream;
    function FindDockForm(const FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
  protected
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); override;
    procedure CreateZoneAndAddInfoFromAppStorage; virtual;
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

    procedure ReadInfoFromAppStorage;
    procedure WriteInfoToAppStorage;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
    procedure ReadInfoFromIni;
    procedure ReadInfoFromReg(const RegName: string);
    procedure WriteInfoToIni;
    procedure WriteInfoToReg(const RegName: string);
    property DockInfoIni: TCustomIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Winapi.Messages,
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
        //TJvDockConjoinHostFormCreatedEvent:   Added by KV
        if Assigned(ADockClient.OnConjoinHostFormCreated) then
          ADockClient.OnConjoinHostFormCreated(ADockClient, TJvDockConjoinHostForm(Form));
      end;
    dsTab:
      if Assigned(TJvDockInfoZone(ATreeZone.ChildZone).DockControl) then
      begin
        Form := TJvDockTabHostForm.Create(Application);
        ADockClient := FindDockClient(TJvDockInfoZone(ATreeZone.ChildZone).DockControl);
        Result := ADockClient.CreateTabDockClass(Form).Parent;
        //TJvDockTabHostFormCreatedEvent:   Added by KV
        if Assigned(ADockClient.OnTabHostFormCreated) then
          ADockClient.OnTabHostFormCreated(ADockClient, TJvDockTabHostForm(Form));
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

var
  FormName: string;
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
        CP := PChar(S);
        CP1 := StrPos(CP, ';');
        while CP1 <> nil do
        begin
          SetString(FormName, CP, CP1 - CP);
          // (Mantis #4293) Avoid restoration of not instantiated forms => DockFormStyle == dsNormal
          if TJvDockFormStyle(FAppStorage.ReadInteger(
                              FAppStorage.ConcatPaths([FormName, 'DockFormStyle']))) = dsNormal then
          begin
            for I := 0 to Screen.FormCount - 1 do
            begin
              if Screen.Forms[I].Name = FormName then
              begin
                FormList.Add(FormName);
                Break;
              end;
            end;
          end
          else
            FormList.Add(FormName);
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

procedure TJvDockInfoTree.ReadInfoFromAppStorage;
begin
  AppStorage.BeginUpdate;
  try
    CreateZoneAndAddInfoFromAppStorage;
    DoFloatAllForm;
    // (rom) this is disputable
    //Application.ProcessMessages;
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

procedure TJvDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;

  DoFloatAllForm;

  // (rom) this is disputable
  //Application.ProcessMessages;

  FJvDockInfoStyle := isJVCLReadInfo;
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.ReadInfoFromReg(const RegName: string);
begin
  FRegName := RegName;
  CreateZoneAndAddInfoFromReg;

  DoFloatAllForm;

  // (rom) this is disputable
  //Application.ProcessMessages;

  FJvDockInfoStyle := isReadRegInfo;
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

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
  FJvDockInfoStyle := isJVCLWriteInfo;
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

type
  TControlAccessProtected = class(TControl);

procedure TJvDockInfoZone.SetDockInfoFromControlToNode(Control: TControl);
Var
  ControlPPI: integer;
begin
  ControlPPI := TControlAccessProtected(Control).FCurrentPPI;
  if (Control is TJvDockPanel) and (ControlPPI <> 96) and (ControlPPI <> 0) then
  begin
    // Get DockRect unscaled.  It will be scaled on loading back
    with Control.BoundsRect do
    begin
      FDockRect.Left := MulDiv(Left, 96, ControlPPI);
      FDockRect.Right := MulDiv(Right, 96, ControlPPI);
      FDockRect.Top := MulDiv(Top, 96, ControlPPI);
      FDockRect.Bottom := MulDiv(Bottom, 96, ControlPPI);
      LRDockWidth := MulDiv(Control.LRDockWidth, 96, ControlPPI);
      TBDockHeight := MulDiv(Control.TBDockHeight, 96, ControlPPI);
    end;
  end else
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
    // Save unscaled
    LRDockWidth := MulDiv(Control.LRDockWidth, 96, ControlPPI);
    TBDockHeight := MulDiv(Control.TBDockHeight, 96, ControlPPI);
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
  // DockPanel DockRect is unscaled
  var
    LCurrentPPI: Integer;
  begin
    LCurrentPPI := TControlAccessProtected(DockSite).FCurrentPPI;
    if DockSite.Align in [alTop, alBottom] then
      DockSite.JvDockManager.DockSiteSize := MulDiv(DockRect.Bottom - DockRect.Top, LCurrentPPI, 96)
    else
      DockSite.JvDockManager.DockSiteSize := MulDiv(DockRect.Right - DockRect.Left, LCurrentPPI, 96);
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
        //https://stackoverflow.com/questions/3118751/how-can-i-display-a-form-on-a-secondary-monitor
        // When Windows state is wsMaximized setting the BoundsRect would maximize the form in the correct Monitor

        // Set the BoundsRect twice.  The first one may cause PPI scaling and change the bounds
        TWinControl(Control).HandleNeeded;
        Control.BoundsRect := DockRect;  // is this useful for minimized forms?
        Control.BoundsRect := DockRect;

        Control.Visible := Visible;
        if WindowState <> wsMinimized then
          TForm(Control).WindowState := WindowState
        else
          // setting WindowState to wsMinimized leads to crashes
          PostMessage(TForm(Control).Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
      end
      else if Control is TJvDockVSPopupPanel then
        SetPopupPanelSize(Control as TJvDockVSPopupPanel)
      else
        SetDockSiteSize(Control as TJvDockPanel);
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
  Control.Visible := Visible;
  if (Control is TForm) or (Control is TJvDockPanel) then
  begin
    Control.LRDockWidth := MulDiv(LRDockWidth, TControlAccessProtected(Control).FCurrentPPI, 96);
    Control.TBDockHeight := MulDiv(TBDockHeight, TControlAccessProtected(Control).FCurrentPPI, 96);
  end
  else
  begin
    Control.LRDockWidth := LRDockWidth;
    Control.TBDockHeight := TBDockHeight;
  end;
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
