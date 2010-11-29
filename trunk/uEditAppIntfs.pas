{-----------------------------------------------------------------------------
 Unit Name: uEditAppIntfs
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Editor interfaces
            Based on SynEdit demo
 History:
-----------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uEditAppIntfs.pas, released 2000-09-08.

The Original Code is part of the EditAppDemos project, written by
Michael Hieke for the SynEdit component suite.
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

$Id: uEditAppIntfs.pas,v 1.2 2000/11/22 08:34:14 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uEditAppIntfs;

{$I SynEdit.inc}

interface

uses
  Windows, Classes, Forms, SynEdit, SpTBXTabs, Contnrs, cPythonSourceScanner,
  frmCodeExplorer;

type
  TBreakPoint = class(TPersistent)
  private
    fLineNo : integer;
    fDisabled : Boolean;
    fCondition : string;
  published
    property LineNo : integer read fLineNo write fLineNo;
    property Disabled : Boolean read fDisabled write fDisabled;
    property Condition : string read fCondition write fCondition;
  end;

  IEditor = interface;

  IEditorViewFactory = interface
  ['{680F6C4E-5EED-4684-A199-5A62E644D81B}']
    function CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
    function GetName : string;
    function GetTabCaption : string;
    function GetMenuCaption : string;
    function GetHint : string;
    function GetImageIndex : integer;
    function GetShortCut : TShortCut;
    procedure GetContextHighlighters(List : TList);
    property Name : string read GetName;
    property TabCaption : string read GetTabCaption;
    property MenuCaption : string read GetMenuCaption;
    property Hint : string read GetHint;
    property ImageIndex : integer read GetImageIndex;
    property ShortCut : TShortCut read GetShortCut;
  end;

  IEditorView = interface
  ['{E68438C1-CE7C-4831-A995-5E72F01AEFEC}']
    procedure UpdateView(Editor : IEditor);
  end;

  TFileSaveFormat = (sf_Ansi, sf_UTF8, sf_UTF8_NoBOM, sf_UTF16LE, sf_UTF16BE);

  IEditor = interface
  ['{15E8BD28-6E18-4D49-8499-1DB594AB88F7}']
    procedure Activate(Primary : Boolean = True);
    function ActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
    function AskSaveChanges: boolean;
    function CanClose: boolean;
    procedure Close;
    function GetSynEdit : TSynEdit;
    function GetSynEdit2 : TSynEdit;
    function GetActiveSynEdit : TSynEdit;
    function GetBreakPoints : TObjectList;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetFileNameOrTitle: string;
    function GetModified: boolean;
    function GetFileEncoding : TFileSaveFormat;
    function GetForm : TForm;
    function GetEncodedText : AnsiString;
    function GetSourceScanner : IAsyncSourceScanner;
    function GetCodeExplorerData : ICodeExplorerData;
    function GetTabControlIndex : integer;
    procedure SetFileEncoding(FileEncoding : TFileSaveFormat);
    procedure OpenFile(const AFileName: string; HighlighterName : string = '');
    function HasPythonFile : Boolean;
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure Retranslate;
    property FileName : string read GetFileName;
    property FileTitle : string read GetFileTitle;
    property Modified : boolean read GetModified;
    property SynEdit : TSynEdit read GetSynEdit;
    property SynEdit2 : TSynEdit read GetSynEdit2;
    property ActiveSynEdit : TSynEdit read GetActiveSynEdit;
    property BreakPoints : TObjectList read GetBreakPoints;
    property FileEncoding : TFileSaveFormat read GetFileEncoding write SetFileEncoding;
    property EncodedText : AnsiString read GetEncodedText;
    property Form : TForm read GetForm;
    property SourceScanner : IAsyncSourceScanner read GetSourceScanner;
    property CodeExplorerData : ICodeExplorerData read GetCodeExplorerdata;
    property TabControlIndex : integer read GetTabControlIndex;
  end;

  IEditorFactory = interface
  ['{FDAE7FBD-4B61-4D7C-BEE6-DB7740A225E8}']
    function CanCloseAll: boolean;
    procedure CloseAll;
    function CreateTabSheet(AOwner: TSpTBXCustomTabControl): IEditor;
    function GetEditorCount: integer;
    function GetEditor(Index: integer): IEditor;
    function GetEditorByName(const Name : string): IEditor;
    function GetEditorByNameOrTitle(const Name : string): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    procedure RegisterViewFactory(ViewFactory : IEditorViewFactory);
    function GetViewFactoryCount: integer;
    function GetViewFactory(Index: integer): IEditorViewFactory;
    procedure SetupEditorViewMenu;
    procedure UpdateEditorViewMenu;
    //procedure GetRegisteredViewFactory(ViewName : string):IEditorViewFactory;
    property Count : integer read GetEditorCount;
    property Editor[Index: integer]: IEditor read GetEditor;  default;
    property ViewFactoryCount : integer read GetViewFactoryCount;
    property ViewFactory[Index: integer]: IEditorViewFactory read GetViewFactory;
  end;

  IEditCommands = interface
  ['{64397AD0-BA45-4F4A-B72E-2E4647B8ACB9}']
    function CanCopy: boolean;
    function CanCut: boolean;
    function CanDelete: boolean;
    function CanPaste: boolean;
    function CanRedo: boolean;
    function CanSelectAll: boolean;
    function CanUndo: boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
  end;

  IFileCommands = interface
  ['{C10F67B6-BE8D-4A0D-8FDA-05BBF8DEA08A}']
    function CanClose: boolean;
    function CanPrint: boolean;
    function CanSave: boolean;
    function CanSaveAs: boolean;
    function CanReload: boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecPrintPreview;
    procedure ExecSave;
    procedure ExecSaveAs;
    procedure ExecReload(Quiet : Boolean = False);
  end;

  ISearchCommands = interface
    ['{490F145F-01EB-486F-A326-07281AA86BFD}']
    function CanFind: boolean;
    function CanFindNext: boolean;
    function CanFindPrev: boolean;
    function CanReplace: boolean;
    function GetSearchTarget : TSynEdit;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    property SearchTarget : TSynEdit read GetSearchTarget;
  end;

var
  GI_EditorFactory: IEditorFactory;

  GI_ActiveEditor: IEditor;

  GI_EditCmds: IEditCommands;
  GI_FileCmds: IFileCommands;
  GI_SearchCmds: ISearchCommands;

implementation

end.




