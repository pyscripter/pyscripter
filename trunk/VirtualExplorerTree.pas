unit VirtualExplorerTree;

// Version 2.4.0
//
// The contents of this file are subject to the Mozilla Public License               `
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimdk@mindspring.com>
//
//----------------------------------------------------------------------------
//
// History
//  July 12, 2010
//   - Fixed issue with race condition in unit initialization and MP Common Library on loading
//     dynamically linked Shell32 and NT only API functions
//
//  May 30 2010
//
//   - Added support to try to keep node selected if the file/folder name is renamed
//     outside of VirtualShellTools. If the Kernel Notifier is used it is not 100% guaranteed
//     but should work most of the time.
//
// Version 2.0.0 12.21.05
//  - Fixed bug in ForceIconCachRebuild.  Need ValueExists not KeyExists
//  - Fixed memory leak in VirtualShellHistory
//
// Version 1.4.0 - 12-4-05
//
//  - Support for D2006.
//
//  - Incorporated changes to make hard coded text in header dialog box and other
//    places to be changed at runtime.  Allows localization at runtime.
//  - Fixed several bugs in the Threaded Enumerations
//  - Fixed bug in resetting the column sort column and direction.
//  - Added new option, VETOptions.MiscOptions.toPersistListviewColumnSort
//  - Fixed incorrect use of compressed text color
//  - TVirtualExplorerListview.BrowseToByPIDL now checks if it's necessary to
//    change the rootfolder.
//  - Added ESC key handler to cancel cut or copy.
//
//-  Version 1.3.0 - 11-6-05
//
//  - Renamed OnAddCustomShellColumn to OnHeaderRebuild.
//    OnHeaderRebuild is fired after the header gets recreated.  Use this event to add,
//    delete or modify the columns.
//    WARNING:
//      This will break your app if you used the OnAddCustomShellColumn
//      event.  You will need to change over to this new event.  Sorry.
//  - Reworked the delayed shell notify handler for when a VET is dragging,
//    context menu is showing or editing.  Uses less resources and is more robust.
//  - Added background enumeration of objects in the Listview.  See the new
//    ThreadedEnum property and the EnumThreadLengthyOperation and OnEnumFinished
//    events.
//  - Fixed bug in IsDriveW when using "<Drive Letter>:' format.
//
//   Version 2.4.x April 20, 2010
//
//      - Fixed hangs in Vista and Win 7
//      - Added more FileObjects to include in enumeration.  Most only valid in Vista or Win 7 see the Microsoft documentation on IShellFolder.EnumObjects and the SHCONTF_xxxx constants
//
//
//
//  SEE "VirtualExplorerTree History.txt" FOR PREVIOUS VERSION HISTORY
//
//  Thanks to Kostas Yannakopoulos and Mike Lischke for tricks of the trade in
//      making the dropdown windows for the combobox and autocomplete a reality in
//      this lifetime.
//  Credits for invaluable help implementing the first *real* virtual namespace and
//     working out the kinks in the VNS interface: Gerard Slurink
//  Credits for valuable discussions on the Windows shell implementation and
//  donated code:  Kostas Yannakopoulos
//  Credits for general invaluable assistance:
//      Milan Vandrovec (Keeping this thing compilable in BCB), Mike Lischke,
//      Werner Lehmann, Adam Baba, Robert Lee (not OptimalCode Robert Lee) for
//      a lot of help debugging and improving TExplorerListview (and now
//      TExplorerCombobox), Aaron Chan for demos with Toolbar 2000/TBX, Gerald Koeder,
//      Keld R. Hansen, Bill Miller, Eberhard Meisel, Hideo Koiso,
//      and numerous others who's names I have forgotten but their contributions to
//      VET have not.
//  Credits for their openly available work:
//      James Holderness (Undocumented Shell goodies), Angus Johnson (GetDiskFreeSpaceFAT32
//        code), Gerald Nunn and John Topley (inspiration and techniques from
//        GXExplorer), Jeroen Mineur (Code to create a class method callback),
//        Troy Wolbrink - for his Unicode enabled VCL and the Unicode TEdit,
//        currently using code from 12.31.02
//

interface

  {$IFDEF TNTSUPPORT}
//   IMPORTANT - PLEASE READ then comment this line out.
//  If using TNT you MUST include the TNT package for your specific compiler in the
//  Requires section of this package.  It may be possible to compile without doing
//  this but you WILL eventually have strange crashes in your application that will
//  be difficult to understand.  The best way to do this in my opinion is to create
//  a new folder in the package install directory called "Delphi_TNT" (or CBuilder_TNT)
//  and copy all the files from the Delphi (or CBuilder) folder into it.  Now open the
//  VirtualShellToolsDx.dpk (or bpk) file in the "Delphi_TNT" (or CBuilder_TNT) based on your compiler
//  version in a text editor.  In the "Requires" section add "TNTUnicodeVcl_Rx0", where
//  the "x" is the version of Delphi you are using.  Open the dpk (bpk) file in your
//  IDE. Select the menu option Projects>Options>Directories/Conditionals>Conditional
//  and enter TNTSUPPORT. Compile the package, then open the VirtualShellToolsDxD.dpk (or bpk)
//  and compile and press the Install button.
//  Now when you update the packages you won't have to redo all this.  Just install
//  the update then compile the packages in the "Delphi_TNT" (or CBuilder_TNT) folders
//  an you are done.
  {$ENDIF}

{$include Compilers.inc}
{$include ..\Include\AddIns.inc}
{$B-}
{.$DEFINE GXDEBUG}
{.$DEFINE ALWAYS_SHOW_ALL_COLUMNS}  // If enabled all shell columns will be shown in report view by default
{.$DEFINE DELAY_THREADED_ENUM}
{.$DEFINE GXDEBUG_ADDRESSBAR}

{$ifdef COMPILER_12_UP}
  {$WARN IMPLICIT_STRING_CAST       OFF}
 {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
{$endif COMPILER_12_UP}

{$IFDEF GXDEBUG_ADDRESSBAR}
  {$DEFINE GXDEBUG}
{$ENDIF}

uses
  {$IFDEF COMPILER_9_UP}
  Types,
  {$ENDIF}
  {$IFDEF GXDEBUG}
  DbugIntf,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  ShellAPI,
  ActiveX,
  ShlObj,
  ComCtrls,
  Forms,
  CommCtrl,
  Menus,
  Buttons,
  ImgList,
  StdCtrls,
  ExtCtrls,
  {$IFDEF SpTBX}
  SpTBXSkins,
  SpTBXEditors,
  {$ENDIF SpTBX}
  {$IFDEF TNTSUPPORT}
  TntStdCtrls,
  TntClasses,
  TntSysUtils,
    {$IFDEF COMPILER_10_UP}
    WideStrings,
    {$ELSE}
    TntWideStrings,
    {$ENDIF}
  {$ENDIF}
  Registry,
  VirtualTrees,
  MPShellUtilities,
  VirtualScrollbars,
  VirtualShellNotifier,
  VirtualShellAutoComplete,
  VirtualResources,
  MPCommonObjects,
  MPCommonUtilities,
  MPThreadManager,
  MPResources,
  MPShellTypes,
  MPDataObject,
  {$IFDEF COMPILER_7_UP}
  Themes,
  {$ELSE}
  TMSchema, // Windows XP themes support for D5-D6. Get these units from www.delphi-gems.com.
  {$ENDIF}
  UxTheme,
  EasyListview;  // Windows XP themes support for D5-D6. Get these units from www.delphi-gems.com.

{*******************************************************************************}
{  VET Declarations                                                             }
{*******************************************************************************}

const
  DragBorderWidth = 5;

  VETDropDownMinWidth = 80;
  VETDropDownMinHeight = 20;

  VETStreamStorageVer = 6;     // Current Stream version

  VETStreamStorageVer_0 = 0;
  VETStreamStorageVer_1 = 1;
  VETStreamStorageVer_2 = 2;
  VETStreamStorageVer_3 = 3;
  VETStreamStorageVer_4 = 4;   // Adds Details and grouping info for folders
  VETStreamStorageVer_5 = 5;   // Fixed a mistake
  VETStreamStorageVer_6 = 6;   // Adds Thumbnail info for folders

  BKGNDLENTHYOPERATIONTIME = 20; // 50ms * N before a backgnd enumeration is concidered too long

  AFTEREDITDELAY = 100;  // ms; Forces a wait after enter is pressed to try to
                         // delay VET enough to let the ChangeNotifies pass
                         // such that the tree will keep the newly created node
                         // on the bottom of the list and not do a refresh and
                         // resort it

  BORDER = 1;  // Single line border

  INVALIDFILECHAR = ['/', '\', ':', '*', '?', '"', '<', '>', '|'];

  ID_HARDREFRESHTIMER = 204;

  TID_ICON = TID_START + 9990;
  TID_EXPANDMARK = TID_START + 9991;

  WM_VETCOMBO_SELECTALL = WM_APP + 56;

type
  TNodeSearchRec = packed record
    Node: PVirtualNode;
    NS: TNamespace;
  end;
  TNodeSearchArray = array of TNodeSearchRec;

{$ifndef COMPILER_12_UP}
type
  UnicodeString = WideString;
{$endif}

type

{ How VET displays the file size                                                }
  TFileSizeFormat = ( 
    fsfExplorer,
    fsfActual,
    fsfDiskUsage
  );

{ What special folder, or custom path is used as the root of the tree.          }
  TRootFolder = (
    rfAdminTools,
    rfAltStartup,
    rfAppData,
    rfBitBucket,
    rfCommonAdminTools,
    rfCommonAltStartup,
    rfCommonAppData,
    rfCommonDesktopDirectory,
    rfCommonDocuments,
    rfCommonFavorties,
    rfCommonPrograms,
    rfCommonStartMenu,
    rfCommonStartup,
    rfCommonTemplates,
    rfControlPanel,
    rfCookies,
    rfDesktop,
    rfDesktopDirectory,
    rfDrives,
    rfFavorites,
    rfFonts,
    rfHistory,
    rfInternet,
    rfInternetCache,
    rfLocalAppData,
    rfMyPictures,
    rfNetHood,
    rfNetwork,
    rfPersonal,
    rfPrinters,
    rfPrintHood,
    rfProfile,
    rfProgramFiles,
    rfCommonProgramFiles,
    rfPrograms,
    rfRecent,
    rfSendTo,
    rfStartMenu,
    rfStartUp,
    rfSystem,
    rfTemplate,
    rfWindows,
    rfCustom,
    rfCustomPIDL
  );

  TColumnDetailType = (
    cdUser,
    cdVETColumns,
    cdShellColumns
  );

  TColumnDetails = (
    cdFileName,
    cdSize,
    cdType,
    cdModified,
    cdAccessed,
    cdCreated,
    cdAttributes,
    cdPath,
    cdDOSName,
    cdCustom
  );

  TColumnWidthView = (
    cwv_Default,
    cwv_AutoFit,
    cwv_Minimize
  );

  TColumnWidthInfo = packed record
    Width: Longword;
    WidthView: TColumnWidthView;
  end;

  TButtonState = (
    bsDown,
    bsUp
  );

  TCoordType = (            // For coordinate translation
    ctClient,               // Want or are supplying coord in Client Coordinates
    ctScreen,               // Want or are supplying coord in Screen Coordinates
    ctWindow                // Want or are supplying coord in Window Coordinates
  );

  TForceRightDragType = (
    frdBegin,
    frdEnd
  );

  TVETFolderOption = (
    toFoldersExpandable,      // Force VET to show only one level of file/folder
    toHideRootFolder,         // Emulate a list view
    toForceHideRecycleBin,    // Never show the RecycleBin
    toForceShowMyDocuments,   // Always show the My Documents folder even if user has removed it from desktop
    toShowOpenIconOnSelect,   // Show the Open Folder icon on selection of the node, else the open icon is only shown if the folder is expanded
    toDisableGhostedFolders,  // does not allow blended images on folders for mimicing Explorer Treeview
    toNoUseVETColorsProp,     // Don't automaticlly use the VETColors property to color Folders, Files, and Compressed objects text like Explorer
    toThreadedExpandMark      // VET uses threads to determine if a folder really needs a [+] expand mark
  );
  TVETFolderOptions = set of TVETFolderOption;

  TVETShellOption = (
    toRightAlignSizeColumn,   // Well.. what it says in VETColumn or ShellColumn mode
    toContextMenus,           // Implement Shell style ContextMenus
    toDragDrop,               // Enable OLE File drag and drop
    toShellHints,             // Use the hints property to show hints associated with file (Win98, WinMe, Win2k only)
    toShellColumnMenu,         // Allows for hiding/showing of different columns like Explorer in Win2k/WinME
    toFullRowContextMenuActivate       // The shell context menu will be shown anywhere the mouse clicks in the node
  );
  TVETShellOptions = set of TVETShellOption;

  TVETMiscOption = (
    toBrowseExecuteFolder,            // Substitues the default ShellExecute action for a folder namespace to a VET navigation action
    toBrowseExecuteFolderShortcut,    // Substitues the default ShellExecute action for a shortcut to a folder namespace to a VET navigation action
    toBrowseExecuteZipFolder,         // Substitues the default ShellExecute action for a zip folder namespace to a VET navigation action
    toChangeNotifierThread,           // VET notified when files system changes
    toListviewLimitBrowseToRoot,      // Limits the Listview backbrowsing (through keyboard and and BrowseTo method) to the set RootFolder or any linked VET or Treeview window root
    toNoRebuildIconListOnAssocChange, // XP has a side effect of refreshing the icon list by changing the iconsize then back again.  It reorganizes the desktop icons.  Allow it to be turned off
    toTrackChangesInMappedDrives,     // If the toChangeNotifierThread is set then track changes in mapped drives.  This rather costly so use at your own discresion
    toPersistentColumns,               // Allows VET to store the state of the columns on a per node basis
    toExecuteOnDblClk,                 // Execute the default action on dbl clicked item
    toExecuteOnDblClkFullRow,          // Execute the default action on dbl click for the full row if using Full Row Selection
    toRightButtonSelect,               // The Right mouse button selects the hit node and stays unlike the Explorer treeview
    toRemoveContextMenuShortCut,       // Removes the Shortcut item from the context menu.  Used mainly when in the explorer Treeview to be consistent with Explorer
    toUserSort,                        // The application is responsible for sorting through the Sort method
    toAutoScrollHorz,                  // All ScrollIntoView calls scroll horzantally too.
    toVETReadOnly,                     // The user can not modify the contents of the folder viewed
    toRestoreTopNodeOnRefresh,         // Restores the original top node in the tree on a Refresh
    toPersistListviewColumnSort        // Keeps the current view column sort information in all views
  );
  TVETMiscOptions = set of TVETMiscOption;

  TVETImageOption = (
    toHideOverlay,            // Don't use the Overlay Interface for Icon Overlays
    toImages,                 // Use images associated with files
    toThreadedImages,         // Create a thread to retrieve the images if used
    toLargeImages,            // Use the 32x32 system images
    toMarkCutAndCopy         // Draw the imags differently if the items are cut or copied
  );
  TVETImageOptions = set of TVETImageOption;

  TVETSyncOption = (
    toCollapseTargetFirst,    // Collapse all expanded node before selecting target node
    toExpandTarget,           // Expands the node in the syncronized target VET
    toSelectTarget            // Select the node in the syncronized Target VET
  );
  TVETSyncOptions = set of TVETSyncOption;

    TComboBoxStyle = (
    cbsClassic,               // Works like the ExplorerComboBox
    cbsVETEnhanced            // Allows for VET enabled ComboBox dropdown with expandable folders
  );

  TExplorerComboboxText = (
    ecbtNameOnly,   // Only show the object name in the edit box
    ecbtFullPath    // Show the full path in the edit box if a file object
  );

  TVETComboState = (
    vcbsNotifyChanging,            // VET Combobox is in the process of a ChangeNotify
    vcbsOverDropDownButton,        // For drawing a higlighted drop down button when mouse over it
    vcbsDropDownButtonPressed,     // The button is pressed
    vcbsDropDownButtonPressPending // The button was press but the cursor is off the button with mouse release
  );
  TVETComboStates = set of TVETComboState;

  TVETComboOption = (
    vcboThreadedImages,             // ThreadedImages in the VET drop down
    vcboThemeAware,                 // Enabled themes if available
    vcboSelectPathOnDropDown        // The current path in the edit is selecting in the dropdown VET when it drops down
  );
  TVETComboOptions = set of TVETComboOption;

  TComboItemRect = (
    crBackGround,         // The usable background of the Combo (minus border but with DropDownButton)
    crClient,             // The full client rectangle
    crDropDownButton,     // The area of the DropDownButton
    crImage,              // The area of the Icon
    crComboEdit            // The area of the ComboEdit Control
  );

  TDropDown = (
    ddExplorer,           // Show the Explorer Dropdown in the ComboEdit
    ddAutoComplete        // Show the AutoCompleteDropDown in the ComboEdit
  );

  TShellComboStyle = (
    scsDropDown,          // The edit in the ShellComboBox is editable
    scsDropDownList       // The edit in the ShellComboBox is not editable
  );

  TChangeLinkListState = (
    clsDispatching      // The change link list is in the middle of a Dispatch
  );
  TChangLinkListStates = set of TChangeLinkListState;

  TUnRegisterType = (
    utServer,       // The server is requesting the Unregister
    utClient,       // The Client is requesting the Unregister
    utLink,         // The particular link between Client and Server requesting Unregister
    utAll           // All Server and Client links are broken
  );

  TVETState = (
    vsBrowsing,         // Set when VET is in the middle of a BrowseTo operation
    vsNotifyChanging,   // VET is in the process of a ChangeNotify
    vsLockChangeNotifier, // Stops Change Notifier Calls
    vsHeaderWasShown,        // True if the header was shown before the backgound item enumeration
    vsCreateNotificationLock
  );
  TVETStates = set of TVETState;

  TVETPersistentState = (
    vpsFullInit     // Set after the target tree has been fully initialized
  );
  TVETPersistentStates = set of TVETPersistentState;

  TNamespaceStructureChange = (
    nscDelete,     // A node/namespace are being removed from the tree
    nscAdd         // A node/namespace are being added to the tree
  );

  TStorageType = (
    stChecks,             // The VET Storage contains Check box data
    stColumns,            // The VET Storage contains Columns width data
    stGrouping,           // The VET Storage contains grouping info for the Listview
    stUser
  );

   // Rolldown Window types
  TPopupState = (
    psRolledDown,          // Set if the form is rolled down
    psFormHooked,          // Set if the parent forms Window Proc is hooked
    psAboveHostControl,    // When rolled down the window is below the host control (if assigned)
    psBelowHostControl,    // When rolled down the window is above the host control (if assigned)
    psDroppedOnce,         // The window has been dropped down once (used for persistent sizing)
    psScrollingUp,         // The window is in AutoScroll mode, Scrolling up
    psScrollingDown,       // The window is in AutoScroll mode, Scrolling Down
    psFastScroll,          // The window is scrolling fast (line at a time)
    psSlowScroll,          // The window is scrolling slow (pixel at a time)
    psLeftScrollbar        // The scrollbar is on the left side
  );
  TPopupStates = set of TPopupState;

  
  TPopupOption = (
    poAnimated,               // The popup uses animation to roll
    poEnabled,                // Enable or disable the popup
    poPersistentSizing,       // The popup remembers the last dimenstions of the window
    poSizeable,               // The Popup window has a sizable border
    poRespectSysAnimationFlag, // use SystemParameterInfo to see if the use want animation
    poThemeAware              // Shows it self using themes if available (XP only)
  );
  TPopupOptions = set of TPopupOption;

{-------------------------------------------------------------------------------}
{ Assorted type definitions                                                     }
{-------------------------------------------------------------------------------}

type
  PFindSpecialFolderByNameData = ^TFindSpecialFolderByNameData;
  TFindSpecialFolderByNameData = record
    Name: WideString;
    PIDL: PItemIDList;
  end;

{-------------------------------------------------------------------------------}

const
  DefaultVETPaintOptions = [toShowButtons, toUseBlendedImages, toShowTreeLines, toGhostedIfUnfocused];
  DefaultVETFolderOptions = [toFoldersExpandable];
  DefaultVETShellOptions = [toContextMenus, toRightAlignSizeColumn];
  DefaultVETMiscOptions = [toExecuteOnDblClk, toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder];
  DefaultVETImageOptions = [toImages, toMarkCutAndCopy];
  DefaultVETSyncOptions = [toCollapseTargetFirst, toSelectTarget, toExpandTarget];

  DefaultExplorerTreeFileObjects = [foFolders, foHidden];
  DefaultExplorerTreeAutoOptions = [toAutoScroll];
  DefaultExplorerTreeMiscOptions = [toEditable, toAcceptOLEDrop, toToggleOnDblClick];
  DefaultExplorerTreePaintOptions = [toShowButtons, toUseBlendedImages, toShowTreeLines, toGhostedIfUnfocused];
  DefaultExplorerTreeVETFolderOptions = [toFoldersExpandable];
  DefaultExplorerTreeVETShellOptions = [toContextMenus];
  DefaultExplorerTreeVETMiscOptions = [toChangeNotifierThread, toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder, toRemoveContextMenuShortCut];
  DefaultExplorerTreeVETImageOptions = [toImages, toThreadedImages, toMarkCutAndCopy];
  DefaultExplorerTreeVETSelectionOptions = [toLevelSelectConstraint];
  DefaultExplorerTreeVETSyncOptions = [toCollapseTargetFirst, toSelectTarget, toExpandTarget];

  DefaultExplorerListFileObjects = [foFolders, foHidden, foNonFolders, foEnableAsync];
  DefaultExplorerListPaintOptions = [toShowTreeLines, toUseBlendedImages, toGhostedIfUnfocused];
  DefaultExplorerListMiscOptions = [toAcceptOLEDrop, toToggleOnDblClick, toReportMode, toEditable];
  DefaultExplorerListAutoOptions = [toAutoScroll];
  DefaultExplorerListVETFolderOptions = [toHideRootFolder];
  DefaultExplorerListVETShellOptions = [toContextMenus, toShellColumnMenu, toRightAlignSizeColumn];
  DefaultExplorerListVETMiscOptions = [toExecuteOnDblClk, toChangeNotifierThread, toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder];
  DefaultExplorerListVETImageOptions = [toImages, toThreadedImages, toMarkCutAndCopy];
  DefaultExplorerListVETSelectionOptions = [toMultiSelect, toLevelSelectConstraint, toRightClickSelect];
  DefaultExplorerListVETSyncOptions = [toCollapseTargetFirst, toSelectTarget, toExpandTarget];
  DefaultExplorerListHeaderOptions = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoDblClickResize];

  DefaultExplorerComboFileObjects = [foFolders, foHidden];

  // These are all for the popup windows for the combobox
  DefaultPopupMiscOptions = [];

  DefaultPopupPaintOptions = [toUseBlendedImages, toPopupMode, toHideFocusRect, toShowBackground];
  DefaultPopupAutoOptions = [toAutoScroll, toAutoScrollOnExpand, toAutoScrollOnExpand];
  DefaultPopupSelectionOptions = [toDisableDrawSelection];
  DefaultExplorerComboVETFolderOptions = [toFoldersExpandable];
  DefaultExplorerComboVETShellOptions = [];
  DefaultExplorerComboVETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteZipFolder];
  DefaultExplorerComboVETImageOptions = [toImages];
  DefaultExplorerComboVETSyncOptions = [toCollapseTargetFirst, toSelectTarget];
  DefaultExplorerComboOptions = [vcboSelectPathOnDropDown, vcboThemeAware];
  DefaultPopupOptions = [poThemeAware, poRespectSysAnimationFlag, poEnabled];

type
  TCustomVirtualExplorerTree = class;           // Forward
  TVirtualExplorerListview = class;             // Forward
  TVirtualExplorerTreeview = class;             // Forward
  PVirtualBackGndEnumThread = ^TVirtualBackGndEnumThread;
  TVirtualBackGndEnumThread = class;


  TCustomVirtualExplorerCombobox = class;       // Forward
 // ComboBox classes
  TDropDownWnd = class;                         // forward
  TPopupExplorerDropDown = class;               // forward
  TPopupAutoCompleteDropDown = class;           // forward
  TPopupExplorerTree = class;                   // forward
  TPopupAutoCompleteTree = class;               // forward

  TVETChangeDispatch = class;                   // forward
  TVETChangeLink = class;                       // forward
  IVETChangeLink = interface;                   // forward
  TLeafNodeList = class;                        // forward
  TRootNodeStorage = class;                     // forward

  TVETChangeLinkEvent = procedure(Server: TObject; NewPIDL: PItemIDList) of object;
  TVETChangeLinkFreeEvent = procedure(ChangeLink: IVETChangeLink) of object;


    // ***************************************************************
  // A ThreadRequest that extracts the shell supplied Icon index of an object via a PIDL
  // ***************************************************************
  TExpandMarkThreadRequest = class(TPIDLThreadRequest)
  private
    FEnumFlags: DWORD;  // [IN] Set the folder enumeration flags cuurent for the VET
    FShowMark: Boolean; // [OUT] True if the PIDL shows an expand mark on a remote drive
  public
    function HandleRequest: Boolean; override; // Extracts the Icon from the PIDL
    procedure Assign(Source: TPersistent); override;
    property EnumFlags: DWORD read FEnumFlags write FEnumFlags;
    property ShowMark: Boolean read FShowMark;
  end;

  // Interface of an object that links two controls together (not necessarily VETs)
  // by designating one as the Server and one as the Client.  Object that registers
  // the change link can specify a method to call when a change in the server is
  // dispatched, OnChangeLink, and it may specify a method to be called when the
  // Link is broken or if the Client or Server is freed, OnChangeLinkFree.
  IVETChangeLink = interface
  ['{3C0AF30B-DA91-4F42-B02C-8A326704B368}']
    function GetOnChangeLink: TVETChangeLinkEvent;
    procedure SetOnChangeLink(const Value: TVETChangeLinkEvent);
    function GetChangeLinkServer: TObject;
    procedure SetChangeLinkServer(const Value: TObject);
    function GetChangeLinkClient: TObject;
    procedure SetChangeLinkClient(const Value: TObject);
    function GetOnChangeLinkFree: TVETChangeLinkFreeEvent;
    procedure SetOnChangeLinkFree(const Value: TVETChangeLinkFreeEvent);

    property ChangeLinkServer: TObject read GetChangeLinkServer write SetChangeLinkServer;
    property ChangeLinkClient: TObject read GetChangeLinkClient write SetChangeLinkClient;
    property OnChangeLink: TVETChangeLinkEvent read GetOnChangeLink write SetOnChangeLink;
    property OnChangeLinkFree: TVETChangeLinkFreeEvent read GetOnChangeLinkFree write SetOnChangeLinkFree;
  end;

  // The Object that implements the IVETChangeLink inteface
  TVETChangeLink = class(TInterfacedObject, IVETChangeLink)
  private
    FChangeLinkServer: TObject;
    FChangeLinkClient: TObject;
    FOnChangeLink: TVETChangeLinkEvent;
    FOnChangeLinkFree: TVETChangeLinkFreeEvent;
    function GetOnChangeLink: TVETChangeLinkEvent;
    procedure SetOnChangeLink(const Value: TVETChangeLinkEvent);
    function GetChangeLinkServer: TObject;
    procedure SetChangeLinkClient(const Value: TObject);
    function GetChangeLinkClient: TObject;
    procedure SetChangeLinkServer(const Value: TObject);
    function GetOnChangeLinkFree: TVETChangeLinkFreeEvent;
    procedure SetOnChangeLinkFree(const Value: TVETChangeLinkFreeEvent);
  public
    property OnChangeLink: TVETChangeLinkEvent read GetOnChangeLink write SetOnChangeLink;
    property OnChangeLinkFree: TVETChangeLinkFreeEvent read GetOnChangeLinkFree write SetOnChangeLinkFree;
    property ChangeLinkServer: TObject read GetChangeLinkServer write SetChangeLinkServer;
    property ChangeLinkClient: TObject read GetChangeLinkClient write SetChangeLinkClient;
  end;

  TVETChangeDispatch = class(TInterfaceList)
  private
    FInitialDispatcher: TObject;
    FChangeLinkCache: TInterfaceList;  // Counts the number of levels deep the DispatchChange has recursed
  protected
    procedure DispatchLinks(Server: TObject; NewPIDL: PItemIDList);
    function FindLink(Server, Client: TObject): integer;
    procedure ReduceServerSet(Server: TObject);

    property ChangeLinkCache: TInterfaceList read FChangeLinkCache write FChangeLinkCache;
    property InitialDispatcher: TObject read FInitialDispatcher write FInitialDispatcher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DispatchChange(Server: TObject; NewPIDL: PItemIDList);
    procedure RegisterChangeLink(Server, Client: TObject; ClientOnChangeEvent: TVETChangeLinkEvent;
      ChangeLinkFreeEvent: TVETChangeLinkFreeEvent);
    function UnRegisterChangeLink(Server, Client: TObject; UnRegisterType: TUnRegisterType): Boolean;
  end;

{*******************************************************************************}
{  Persistent Storing and Recreating VET                                        }
{*******************************************************************************}

  TStorageTypes = set of TStorageType;

  TVirtualWordArray = array of Word;
  TBooleanArray = array of Boolean;

  TColumnWidths = TVirtualWordArray;
  TColumnOrder = TVirtualWordArray;

  // Dummy record that is the same as the old TCheckStorage. Used for streaming
  // in older formats
  PVer1CheckStorage = ^TVer1CheckStorage;
  TVer1CheckStorage = packed record
    CheckState: TCheckState;
  end;

  // The latest and greatest checkstorage structure
  PCheckStorage = ^TCheckStorage;
  TCheckStorage = packed record
    CheckState: TCheckState;
    CheckType: TCheckType;     // Extra field added 8.11.02
  end;

  PColumnStorage = ^TColumnStorage;
  TColumnStorage = packed record
    Width: TColumnWidths;
    Position: TColumnOrder;
    Visible: TBooleanArray;
    SortColumn: Integer; // Added in Stream Version 4
    SortDir: Integer;    // Added in Stream Version 4
    DefaultSortDir: Integer; // Added in Stream Version 4  // Depreciated, this should not be saved on a per folder basis it is global to the Listview
  end;

  PGroupStorage = ^TGroupStorage;   // Added in Stream Version 4
  TGroupStorage = packed record
    Enabled: Boolean;
    GroupColumn: Integer;
    View: Integer // View of the control (for Listview = icon, small icon, etc.)
  end;

  PThumbnailStorage = ^TThumbnailStorage; // Added in Stream Version 5
  TThumbnailStorage = packed record
    Size: TSize
  end;

  // User Defined Per node Storage override this and use the Storage class in VET or the Global
  TUserDataStorage = class(TStreamableClass)
  public
    //You should override these 3 methods in your application:
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;
    procedure Assign(Source: TPersistent); override;
  end;

  TUserDataStorageClass = class of TUserDataStorage;

  PStorage = ^TStorage;
  TStorage = packed record
    Types: TStorageTypes;       // Defines what data is currently stored
    Check: TCheckStorage;       // Defines the node checkstate if other than unchecked
    Column: TColumnStorage;     // Defines the column state for a node if other than default
    UserData: TUserDataStorage; // Defines the Users Data based on TUserDataStorage
    Grouping: TGroupStorage;    // Defines the grouping state for the current folder (if it is a folder) // Added Stream Version 4
    Thumbnails: TThumbnailStorage;  // Defines the thumbnail state for the current folder (if it is a folder) // Added Stream Version 6
    Tag: Integer;
  end;

  TNodeStorageList = class;                 // forward

  TNodeStorage = class(TStreamableClass)
  private
    FChildNodeList: TNodeStorageList;   // ChildNodes of the node, implies this node is a folder
    FRelativePIDL: PItemIDList;         // RelativePIDL to parent node
    FAbsolutePIDL: PItemIDList;         // AbsolutePIDL from Desktop
    FParentNode: TNodeStorage;          // Parent of this node
    FShellFolder: IShellFolder;         // IShellFolder of this node. Should only be initialized if node is folder for CompareID
    function GetShellFolder: IShellFolder;
    function GetRootNode: TRootNodeStorage;
  public
    Storage: TStorage;
    procedure Assign(Source: TNodeStorage); reintroduce; virtual;
    procedure AssignTo(Destination: TNodeStorage); reintroduce; virtual;
    procedure Clear(FreeUserData: Boolean = False); virtual;
    constructor Create(AnAbsolutePIDL: PItemIDList; AnOwnerNode: TNodeStorage); virtual;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

    property AbsolutePIDL: PItemIDList read FAbsolutePIDL write FAbsolutePIDL;
    property ChildNodeList: TNodeStorageList read FChildNodeList write FChildNodeList;
    property ParentNode: TNodeStorage read FParentNode write FParentNode;
    property RelativePIDL: PItemIDList read FRelativePIDL write FRelativePIDL;
    property RootNode: TRootNodeStorage read GetRootNode;
    property ShellFolder: IShellFolder read GetShellFolder;
  end;

  TRootNodeStorage = class(TNodeStorage)
  private
    FCacheNode: TNodeStorage;
    {$IFDEF TNTSUPPORT}
    FCheckedFileNames: TTntStringList;
    FResolvedFileNames: TTntStringList;
    {$ELSE}
    FCheckedFileNames: TStringList;
    FResolvedFileNames: TStringList;
    {$ENDIF}
    FCheckedPIDLs: TCommonPIDLList;
    FStored: Boolean;     // True if the RootNode has something stored in it
    function GetCheckedPIDLs: TCommonPIDLList;
    procedure SetCheckedPIDLs(const Value: TCommonPIDLList);
    {$IFDEF TNTSUPPORT}
    function GetCheckedFileNames: TWideStrings;
    procedure SetCheckFileNames(const Value: TWideStrings);
    function GetResolvedFileNames: TWideStrings;
    {$ELSE}
    function GetCheckedFileNames: TStrings;
    procedure SetCheckFileNames(const Value: TStrings);
    function GetResolvedFileNames: TStrings;
    {$ENDIF}
  protected
    function ProcessNode(RelativePIDL: PItemIDList; CurrentNode: TNodeStorage; Force, MarkCheckMixed: Boolean): TNodeStorage;
    function WalkPIDLToStorageNode(PIDL: PItemIDList; Force: Boolean): TNodeStorage;

    property CacheNode: TNodeStorage read FCacheNode write FCacheNode;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Clear(FreeUserData: Boolean = False); override;
    procedure Delete(APIDL: PItemIDList; StorageTypes: TStorageTypes; Force: Boolean = False; FreeUserData: Boolean = False);
    function Find(APIDL: PItemIDList; StorageTypes: TStorageTypes): TNodeStorage; overload; virtual;
    function Find(APIDL: PItemIDList; StorageTypes: TStorageTypes; var StorageNode: TNodeStorage): Boolean; overload; virtual;
    function SetFileChecked(FileName: WideString; CheckBoxType: TCheckType): Boolean;
    function SetPIDLChecked(PIDL: PItemIDList; CheckBoxType: TCheckType): Boolean;
    function Store(APIDL: PItemIDList; StorageTypes: TStorageTypes): TNodeStorage; virtual;

    // Warning Setting the checked filenames assumes a checktype of ctTriStateCheckBox
    // To set a node checked for an arbitrary checkbox style use SetFileChecked and SetPIDLChecked;
    procedure LoadFromStream(S: TStream; Version: integer = 0; ReadVerFromStream: Boolean = False); override;
    {$IFDEF TNTSUPPORT}
    property CheckedFileNames: TWideStrings read GetCheckedFileNames write SetCheckFileNames;
    property ResolvedFileNames: TWideStrings read GetResolvedFileNames;
    {$ELSE}
    property CheckedFileNames: TStrings read GetCheckedFileNames write SetCheckFileNames;
    property ResolvedFileNames: TStrings read GetResolvedFileNames;
    {$ENDIF}
    property CheckedPIDLs: TCommonPIDLList read GetCheckedPIDLs write SetCheckedPIDLs;
    property Stored: Boolean read FStored write FStored;
  end;

  TNodeStorageList = class(TStreamableList)
  protected
    function GetItems(Index: integer): TNodeStorage;
    procedure SetItems(Index: integer; const Value: TNodeStorage);
  public
    procedure Clear; override;
    procedure LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

    property Items[Index: integer]: TNodeStorage read GetItems write SetItems; default;
  end;

  TLeafNode = class(TStreamableClass)
  private
    FExpanded: Boolean;
    FPIDL: PItemIDList;
    FOwner: TLeafNodeList;
  public
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

    property Expanded: Boolean read FExpanded write FExpanded;
    property Owner: TLeafNodeList read FOwner write FOwner;
    property PIDL: PItemIDList read FPIDL write FPIDL;
  end;

  TLeafNodeList = class(TStreamableList)
  private
    FShareNodes: Boolean;
    function GetItems(Index: Integer): TLeafNode;
    procedure SetItems(Index: Integer; const Value: TLeafNode);
  public
    procedure AddLeafNode(LeafPIDL: PItemIDList; IsExpanded: Boolean);
    procedure Clear; override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

    property ShareNodes: Boolean read FShareNodes write FShareNodes;
    property Items[Index: Integer]: TLeafNode read GetItems write SetItems; default;
  end;

  { Stores a VETs state so it may be recreated.  It stores the PIDLs associated }
  { with the the tree that can be used to recreate the tree.  These include the }
  { Nodes that are the the fartherest branches of the tree to be able to expand }
  { it back to original (assuming the folders still exist), the Nodes that are  }
  { selected, and the TopNode so it can be scrolled back into the same place.   }
  { It also includes stream capability to write the info to the disk or         }
  { to either create multiple VETs that look identical or saving the data to    }
  { disk to recreate the last state VET was left in.                            }
  TVETPersistent = class(TStreamableClass)
  private
    FLeafNodes: TLeafNodeList;
    FSelectedPIDLs: TCommonPIDLList;
    FTopNodePIDL: PItemIDList;
    FRootFolderCustomPath: WideString;
    FRootFolder: TRootFolder;
    FSelectedPIDLsParent: PItemIDList;
    FStorage: TRootNodeStorage;
    FStates: TVETPersistentStates;
    FRootFolderCustomPIDL: PItemIDList;
    FFocusPIDL: PItemIDList;
  protected
    procedure FullInitTree(VET: TCustomVirtualExplorerTree; DoInit: Boolean);
    procedure ReStoreLeafPIDLs(VET: TCustomVirtualExplorerTree; RootNode: PVirtualNode);
    procedure ReStoreSelectedPIDLs(VET: TCustomVirtualExplorerTree; RootNode: PVirtualNode);
    function StoreLeafPIDLs(VET: TCustomVirtualExplorerTree; RootNode: PVirtualNode): PVirtualNode;
    procedure StoreSelectedPIDLs(VET: TCustomVirtualExplorerTree; RootNode: PVirtualNode);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure RestoreTree(VET: TCustomVirtualExplorerTree; RestoreSelection, RestoreFocus: Boolean; ScrollToOldTopNode: Boolean = False);
    procedure RestoreTreeBranch(VET: TCustomVirtualExplorerTree; Node: PVirtualNode; RestoreSelection: Boolean);
    procedure SaveTree(VET: TCustomVirtualExplorerTree; SaveSelection, SaveFocus: Boolean);
    procedure SaveTreeBranch(VET: TCustomVirtualExplorerTree; Node: PVirtualNode; SaveSelection: Boolean);
    procedure SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

    property FocusPIDL: PItemIDList read FFocusPIDL write FFocusPIDL;
    property LeafNodes: TLeafNodeList read FLeafNodes;
    property RootFolder: TRootFolder read FRootFolder write FRootFolder;
    property RootFolderCustomPath: WideString read FRootFolderCustomPath write FRootFolderCustomPath;
    property RootFolderCustomPIDL: PItemIDList read FRootFolderCustomPIDL write FRootFolderCustomPIDL;
    property SelectedPIDLs: TCommonPIDLList read FSelectedPIDLs;
    property SelectedPIDLsParent: PItemIDList read FSelectedPIDLsParent;
    property Storage: TRootNodeStorage read FStorage write FStorage;
    property States: TVETPersistentStates read FStates write FStates;
    property TopNodePIDL: PItemIDList read FTopNodePIDL write FTopNodePIDL;
  end;

  { TView expands TVETPersistent by added a name property.  This way many saved }
  { views may be stored and retreived by VET or externally.                     }
  TView = class(TVETPersistent)
  private
    FViewName: WideString;
    public
      constructor Create(AViewName: WideString); reintroduce;
      procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
      procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;

      property ViewName: WideString read FViewName write FViewName;
  end;

  { Implements a TList that can handle TView Objects.                           }
  TViewList = class(TStreamableList)
  private
    function GetItems(Index: Integer): TView;
    procedure SetItems(Index: Integer; const Value: TView);
  public
    destructor Destroy; override;
    procedure LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;
    procedure SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;

    property Items[Index: Integer]: TView read GetItems write SetItems;
  end;

  { Implements a Manager that knows how to handle a TViewList.  It is the       }
  { interface from the program to the collection of Views it stores and         }
  { maintains.                                                                  }
  TViewManager = class(TStreamableClass)
  private
    FViews: TViewList;
    function GetView(ViewName: WideString): TView;
    function GetViewCount: Integer;
    function GetViewName(Index: integer): WideString;
    procedure SetViewName(Index: integer; NewViewName: WideString);
  protected
    property Views: TViewList read FViews write FViews;
  public
    procedure Clear; virtual;
    constructor Create;
    destructor Destroy; override;
    procedure DeleteView(ViewName: WideString);
    procedure LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure ShowView(ViewName: WideString; VET: TCustomVirtualExplorerTree); virtual;
    procedure Snapshot(NewViewName: WideString; VET: TCustomVirtualExplorerTree); virtual;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;
    procedure SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;

    property View[ViewName: WideString]: TView read GetView;
    property ViewCount: integer read GetViewCount;
    property ViewName[Index: integer]: WideString read GetViewName write SetViewName;
  end;

  TGlobalViewManager = class(TViewManager)
  public
    procedure LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False); override;
    procedure SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False); override;
    procedure ShowView(ViewName: WideString; VET: TCustomVirtualExplorerTree); override;
    procedure Snapshot(NewViewName: WideString; VET: TCustomVirtualExplorerTree); override;
  end;

{*******************************************************************************}
{  DragDrop                                                                     }
{*******************************************************************************}

  { Used to register the Shell clipboard formats instead of VT's standard formats }
  TClipboardFormats = array of TClipFormat;

  TVETDataObject = class(TVTDataObject)
  private
    FShellDataObject: IDataObject;
  public
    function DAdvise(const FormatEtc: TFormatEtc; advf: Integer; const advSink: IAdviseSink; out dwConnection: Integer): HResult; override; stdcall;
    function DUnadvise(dwConnection: Integer): HResult; override; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; override; stdcall;
    function EnumFormatEtc(Direction: Integer; out EnumFormatEtc: IEnumFormatEtc): HResult; override; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcOut: TFormatEtc): HResult; override; stdcall;
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HResult; override; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HResult; override; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HResult; override; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; DoRelease: BOOL): HResult; override; stdcall;

    property ShellDataObject: IDataObject read FShellDataObject write FShellDataObject;
  end;

{*******************************************************************************}
{  VET Colors                                                                   }
{*******************************************************************************}
  TVETColorArray = array [0..3] of TColor;

  TVETColors = class(TPersistent)
  private
    FVETColors: TVETColorArray;
    FOwner: TCustomVirtualExplorerTree;
    function GetVETColor(const Index: Integer): TColor;
    procedure SetVETColor(const Index: Integer; const Value: TColor);
  protected
    property Owner: TCustomVirtualExplorerTree read FOwner write FOwner;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(AnOwner: TCustomVirtualExplorerTree);
    destructor Destroy; override;
  published
    property CompressedTextColor: TColor index 0 read GetVETColor write SetVETColor default clExplorerCompressed;
    property FolderTextColor: TColor index 1 read GetVETColor write SetVETColor default clExplorerFolder;
    property FileTextColor: TColor index 2 read GetVETColor write SetVETColor default clExplorerFile;
    property EncryptedTextColor: TColor index 3 read GetVETColor write SetVETColor default clExplorerEncrypted;
  end;


  TColumnMenuItem = class(TMenuItem)
  private
    FColumnIndex: integer;
  public
    procedure Click; override;
    procedure LiveVETUpdate(Sender: TObject);
    procedure UpdateColumns(VET: TCustomVirtualExplorerTree; VST: TVirtualStringTree);

    { Defines what column is associated with the MenuItem instance }
    property ColumnIndex: integer read FColumnIndex write FColumnIndex;
  end;

  TColumnMenu = class(TPopupMenu)
  private
    FVET: TCustomVirtualExplorerTree;
  public
    constructor Create(AOwner: TCustomVirtualExplorerTree); reintroduce;
    procedure Popup(X, Y: Integer); override;

    property VET: TCustomVirtualExplorerTree read FVET;
  end;

  TVETHeader = class(TVTHeader)
  protected
    function CanWriteColumns: Boolean; override;
  end;


  TVETColumn = class(TVirtualTreeColumn)
  private
    FColumnDetails: TColumnDetails;
    procedure SetColumnDetails(const Value: TColumnDetails);
  public
    constructor Create(Collection: TCollection); override;
  published
    property ColumnDetails: TColumnDetails read FColumnDetails write SetColumnDetails;
  end;

  TVETColumnClass = class of TVETColumn;

{*******************************************************************************}
{  Column Width Managment                                                       }
{*******************************************************************************}

  { Types for the ColumnManager class                                           }
  TColumnsWidths = array of TColumnWidthInfo;

  { This class handles the columns of the VET.  It handles Autofitting          }
  { based on the width of the strings in the column.  Eventually it will be     }
  { expanded to allow each node to have a custom layout for the columns in VET  }
  TColumnManager = class
  private
    FColumnWidths: TColumnsWidths;
    FVET: TCustomVirtualExplorerTree;
  protected
    procedure StoreColumnWidth(Column: integer);
    procedure ValidateColumnWidths;
    property ColumnWidths: TColumnsWidths read FColumnWidths write FColumnWidths;
    property VET: TCustomVirtualExplorerTree read FVET write FVET;
  public
    constructor Create(AnOwner: TCustomVirtualExplorerTree);
    destructor Destroy; override;
    procedure ToggleWidthAutoFit(ColumnIndex: integer);
    procedure ToggleWidthMinimize(ColumnIndex: integer);
  end;


{*******************************************************************************}
{  Right Click ContextMenu Management                                           }
{*******************************************************************************}

  TContextMenuManager = class
  private
    FActiveNode: PVirtualNode;
    FPreviousSelectedNode: PVirtualNode;
    FOwner: TCustomVirtualExplorerTree;
    FPreviousFocusNode: PVirtualNode;
    FIsEditingNode: Boolean;
    FEnabled: boolean;
    FMenuPending: Boolean;
  protected
    FMenuShown: Boolean;

    procedure ContextMenuCmdCallback(Namespace: TNamespace; Verb: WideString;
    MenuItemID: Integer; var Handled: Boolean);
    procedure ContextMenuShowCallback(Namespace: TNamespace; Menu: hMenu;
      var Allow: Boolean);
    procedure ContextMenuAfterCmdCallback(Namespace: TNamespace; Verb: WideString;
      MenuItemID: Integer; Successful: Boolean);
    procedure ShowContextMenu(NS: TNamespace; Position: PPoint = nil);
  public
    function CalculatePopupPoint(Node: PVirtualNode): TPoint;
    constructor Create(AnOwner: TCustomVirtualExplorerTree);
    procedure HandleContextMenuMsg(Msg, wParam, lParam: integer; var Result: LRESULT);
    procedure MenuSelect(Msg, wParam, lParam: integer; var Result: LRESULT);
    procedure ResetState;
    procedure RightClick(XPos, YPos: Integer; ButtonState: TButtonState; Coordinates: TCoordType);
    function ShowContextMenuOfActiveNode(Point: TPoint): Boolean;
    function ShowContextMenuOfSelectedItem: Boolean;

    property ActiveNode: PVirtualNode read FActiveNode;
    property Enabled: boolean read FEnabled write FEnabled;
    property IsEditingNode: Boolean read FIsEditingNode write FIsEditingNode;
    property MenuPending: Boolean read FMenuPending write FMenuPending;
    property MenuShown: Boolean read FMenuShown;
    property Owner: TCustomVirtualExplorerTree read FOwner;
    property PreviousFocusNode: PVirtualNode read FPreviousFocusNode;
    property PreviousSelectedNode: PVirtualNode read FPreviousSelectedNode;
  end;

{*******************************************************************************}
{  Options Class                                                                }
{   Add VET options to the class                                                }
{*******************************************************************************}

  TCustomVirtualExplorerTreeOptions = class(TStringTreeOptions)
  private
    FForceMultiLevelMultiSelect: Boolean;  // BE VERY CAREFUL WITH THIS
    FVETFolderOptions: TVETFolderOptions;
    FVETImageOptions: TVETImageOptions;
    FVETMiscOptions: TVETMiscOptions;
    FVETShellOptions: TVETShellOptions;
    FVETSyncOptions: TVETSyncOptions;
    procedure SetVETFolderOptions(const Value: TVETFolderOptions);
    procedure SetVETImageOptions(const Value: TVETImageOptions);
    procedure SetVETMiscOptions(const Value: TVETMiscOptions);
    procedure SetVETShellOptions(const Value: TVETShellOptions);
    function GetOwner: TCustomVirtualExplorerTree; reintroduce;
    function GetAutoOptions: TVTAutoOptions;
    procedure SetAutoOptions(const Value: TVTAutoOptions);
    function GetSelectionOptions: TVTSelectionOptions;
    procedure SetSelectionOptions(const Value: TVTSelectionOptions);

  protected
    property AutoOptions: TVTAutoOptions read GetAutoOptions write SetAutoOptions
      default DefaultAutoOptions;
    property SelectionOptions: TVTSelectionOptions read GetSelectionOptions
      write SetSelectionOptions default DefaultSelectionOptions;
    property VETFolderOptions: TVETFolderOptions read FVETFolderOptions
      write SetVETFolderOptions  default DefaultExplorerTreeVETFolderOptions;
    property VETShellOptions: TVETShellOptions read FVETShellOptions write SetVETShellOptions;
    property VETMiscOptions: TVETMiscOptions read FVETMiscOptions write SetVETMiscOptions;
    property VETImageOptions: TVETImageOptions read FVETImageOptions write SetVETImageOptions;
    property VETSyncOptions: TVETSyncOptions read FVETSyncOptions write FVETSyncOptions;

  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    property ForceMultiLevelMultiSelect: Boolean read FForceMultiLevelMultiSelect write FForceMultiLevelMultiSelect;
    property Owner: TCustomVirtualExplorerTree read GetOwner;
  end;

  TVirtualExplorerTreeOptions = class(TCustomVirtualExplorerTreeOptions)
  published
    property AnimationOptions;
    property AutoOptions;
    property MiscOptions;
    property PaintOptions;
    property SelectionOptions;
    property StringOptions;
    property VETFolderOptions;
    property VETShellOptions;
    property VETSyncOptions;
    property VETMiscOptions;
    property VETImageOptions;
  end;

  TVirtualExplorerEditLink = class(TStringEditLink)
  end;


  TVirtualShellBackgroundContextMenu = class(TCommonShellBackgroundContextMenu)
  end;

  TVirtualShellMultiParentContextMenu = class(TCommonShellMultiParentContextMenu)
  end;

{*******************************************************************************}
{  Data that Virtual Treeview stores                                            }
{*******************************************************************************}
  PNodeData = ^TNodeData;
  TNodeData = packed record
    Namespace: TNamespace;
    ColumnManager: TColumnManager;
  end;

{*******************************************************************************}
{  Event and Callback definitions.                                              }
{*******************************************************************************}

  TVETOnCustomColumnCompare = procedure(Sender: TCustomVirtualExplorerTree;
    Column: TColumnIndex; Node1, Node2: PVirtualNode; var Result: integer) of object;
  TVETOnHeaderRebuild = procedure(Sender: TCustomVirtualExplorerTree; Header: TVTHeader) of object;
  TVETOnShellExecute = procedure(Sender: TCustomVirtualExplorerTree; Namespace: TNamespace;
    var WorkingDir: WideString; var CmdLineArgument: WideString; var Allow: Boolean) of object;
  TVETOnShellNotify = procedure(Sender: TCustomVirtualExplorerTree;
    ShellEvent: TVirtualShellEvent) of object;
  TVETOnEnumFolder = procedure(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; var AllowAsChild: Boolean) of object;
  TVETEnumFinishedEvent = procedure(Sender: TCustomVirtualExplorerTree) of object;
  TVETEnumLenghyOperaionEvent = procedure(Sender: TCustomVirtualExplorerTree; var ShowAnimation: Boolean) of object;
  TVETOnContextMenuAfterCmd = procedure(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; Verb: WideString; MenuItemID: Integer; Successful: Boolean) of object;
  TVETOnContextMenuCmd = procedure(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; Verb: WideString; MenuItemID: Integer; var Handled: Boolean) of object;
  TVETOnContextMenuShow = procedure(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; Menu: hMenu; var Allow: Boolean) of object;
  TVETContextMenuItemChange = procedure(Sender: TCustomVirtualExplorerTree;
    Namespace: TNamespace; MenuItemID : Integer; SubMenuID: hMenu;
      MouseSelect: Boolean) of object;
  TVETOnCustomNamespace = procedure(Sender: TCustomVirtualExplorerTree; AParentNode: PVirtualNode) of object;
  TVETOnDrawNodeText = procedure(Sender: TCustomVirtualExplorerTree; Column: TColumnIndex;
    Node: PVirtualNode; Namespace: TNamespace; var Text: UnicodeString) of object;
  TVETOnTreeDblClick = procedure(Sender: TCustomVirtualExplorerTree; Node: PVirtualNode;
    Button: TMouseButton; Point: TPoint) of object;
  TVETOnRootChange = procedure(Sender: TCustomVirtualExplorerTree) of object;
  TVETOnRootChanging = procedure(Sender: TCustomVirtualExplorerTree; const NewValue: TRootFolder;
    const CurrentNamespace, Namespace: TNamespace; var Allow: Boolean) of object;
  TVETOnRootRebuild = procedure(Sender: TCustomVirtualExplorerTree) of object;
  TVETOnClipboardCopy = procedure(Sender: TCustomVirtualExplorerTree; var Handled: Boolean) of object;
  TVETOnClipboardCut = procedure(Sender: TCustomVirtualExplorerTree; var MarkSelectedCut: Boolean; var Handled: Boolean) of object;
  TVETOnClipboardPaste = procedure(Sender: TCustomVirtualExplorerTree; var Handled: Boolean) of object;
  TNamespaceStructureChangeEvent = procedure(Sender: TCustomVirtualExplorerTree; Node: PVirtualNode; NS: TNamespace; ChangeType: TNamespaceStructureChange) of object;
  TOnPopupRollDown = procedure(Sender: TObject; var Allow: Boolean) of object;
  TOnPopupRollUp = procedure(Sender: TObject; Selected: Boolean) of object;
  TVETOnComboInvalidEntry = procedure(Sender: TCustomVirtualExplorerCombobox; EnteredText: WideString) of object;
  {$IFDEF TNTSUPPORT}
  TOnAutoCompleteUpdateList = procedure(Sender: TObject; const CurrentEditContents: WideString; EnumList: TTntStringList; var Handled: Boolean) of object;
  {$ELSE}
  TOnAutoCompleteUpdateList = procedure(Sender: TObject; const CurrentEditContents: WideString; EnumList: TStringList; var Handled: Boolean) of object;
  {$ENDIF}
  TVETComboBoxDecodeSpecialVariableEvent = procedure(Sender: TCustomVirtualExplorerCombobox; Variable: WideString; var NS: TNamespace) of object;
  TOnComboPathChange = procedure(Sender: TCustomVirtualExplorerCombobox; SelectedNamespace: TNamespace) of object;
  TOnComboPathChanging = procedure(Sender: TCustomVirtualExplorerCombobox; PrevNamespace: TNamespace; var Allow: Boolean) of object;
  TVETOnComboDraw = procedure(Sender: TCustomVirtualExplorerCombobox; ACanvas: TCanvas; AClientRect, AButtonRect: TRect;
    AComboState: TVETComboStates; Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}
{ VIRTUAL EXPLORER TREE                                                         }
{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}

{*******************************************************************************}
{  TCustomVirtualExplorerTree                                                   }
{*******************************************************************************}
  TCustomVirtualExplorerTree = class(TCustomVirtualStringTree, IVirtualShellNotify)
  private
    FActive: Boolean;
    FActivated: Boolean;
    FAltKeyDown: Boolean;              // Tracks the Alt key for special Action (like Alt - DblClick opens property sheet)

    FAnimateFolderEnum: TAnimate;
    FAutoFireChangeLink: Boolean;
    FBackGndMenu: TVirtualShellBackgroundContextMenu;
    FCurrentUpdateCount: Integer;
    { Drag Drop Support }
    FDragDataObject: IDataObject;       // Drag Drop support (the shell supplies its own dataobject)
    FEnumBkGndList: TCommonPIDLList;
    FEnumBkGndTime: Cardinal;
    FEnumLock: TRTLCriticalSection;
    FEnumThread: TVirtualBackGndEnumThread;
    FEnumTimer: THandle;
    FOnColumnUserChangedVisiblility: TNotifyEvent;
    FOnEnumFinished: TVETEnumFinishedEvent;
    FOnEnumThreadLengthyOperation: TVETEnumLenghyOperaionEvent;
    FDragMouseButton: integer;          // Tracks which mouse button is doing the drag
    FDropping: Boolean;                 // Bypasses VT when it calls DragOver from within DragDrop, confuses the Shell D&D (always opens Copy/Move/Link menu)
    FLastDropTargetNode: PVirtualNode;  // Drag Drop support
    FLastDragEffect: integer;           // OLE D&D does not send the effect of the DragOver in the DragDrop call so we must remember it
    FShellNotifySuspended: Boolean;
    FThreadedExpandMarkEnabled: Boolean;

    FThreadedEnum: Boolean;
    { Image Support }
    FUnknownFileIconIndex: integer;     // Index of the "unknown" file type icon
    FUnknownFolderIconIndex: integer;   // Index of a generic Folder icon

    { VET Root options }
    FFileObjects: TFileObjects;         // Shell objects shown: Folders, Files, Hidden
    FRebuildRootNamespaceCount: integer;// Used for reference counting the RootNamespace rebuilds during DFM loading especially
    FRootFolder: TRootFolder;           // What Namespace folder it the tree root node
    FRootFolderCustomPath: WideString;      // Root is a custom path
    FRootFolderCustomPIDL: PItemIDList; // Root is a custom PIDL
    FRootFolderNamespace: TNamespace;   // TNamespace of the Tree Root node
    FTempRootNamespace: TNamespace;     // Place holder to support the OnRootChanging event

    { Persistent VET helpers }
    FVETPersistent: TVETPersistent;     // Stores tree state during tree rebuilds and refreshes so it may be show in previous state
    FViewManager: TViewManager;         // Manages saving the treestate to storage

    { General VET options }
    FFileSizeFormat: TFileSizeFormat;   // Format of the file Size in VET type columns (can't use in ShellColumn Mode)
    FFileSort: TFileSort;               // Forces a sort by extension when the File Type column is chosen to sort the tree
    FVETColors: TVETColors;             // Colors for various VET text base on Namespace attribute
    FColumnDetails: TColumnDetailType;  // Defines User defined columns (through the Header > Columns properties, Standard VET Columns or Shell Based Columns

    FWaitCursorRef: integer;            // Reference count of WaitCursor calls

    { Necessary VT decendent support }
    FInternalDataOffset: Longword;      // How many bytes of internal storage per node VET needs

    { Right Click Menu support }
    FColumnMenu: TColumnMenu;           // Right click menu to select column options
    FColumnMenuItemCount: integer;      // Number of items to show in menu before showing the "More.." message to open dialog
    FContextMenuManager: TContextMenuManager;  // Manages the Shell Context menu actions

    { Header and UserDefined Column support }
    FCreatingHeaders: Boolean;          // True when the Tree in the middle of rebuilding the headers
    FShellBaseColumnCount: integer;     // Tracks how many columns are available for the shell columns defined by the root folder
                                        // Used in aid of custom columns expecially in ShellColumn mode where column count can change
  {$IFDEF EXPLORERTREE_L}
    { InterTree Link support }
    FVirtualExplorerTree: TCustomVirtualExplorerTree;   // Linked VET
  {$ENDIF}

  {$IFDEF EXPLORERCOMBOBOX_L}
    FExplorerComboBox: TCustomVirtualExplorerCombobox;         // Linked TExplorerCombobox
  {$ENDIF}

    { General Property Support }
    {$IFDEF TNTSUPPORT}
    FSelectedPaths: TWideStrings;
    FSelectedFiles: TWideStrings;
    {$ELSE}
    FSelectedPaths: TStrings;
    FSelectedFiles: TStrings;
    {$ENDIF}
    { Event Support }
    FOnClipboardCopy: TVETOnClipboardCopy;
    FOnClipboardCut: TVETOnClipboardCut;
    FOnClipboardPaste: TVETOnClipboardPaste;
    FOnContextMenuAfterCmd: TVETOnContextMenuAfterCmd;
    FOnContextMenuCmd: TVETOnContextMenuCmd;
    FOnContextMenuItemChange: TVETContextMenuItemChange;
    FOnContextMenuShow: TVETOnContextMenuShow;
    FOnCustomColumnCompare: TVETOnCustomColumnCompare;
    FOnCustomNamespace: TVETOnCustomNamespace;
    FOnDrawNodeText: TVETOnDrawNodeText;
    FOnEnumFilter: TVETOnEnumFolder;
    FOnHeaderRebuild: TVETOnHeaderRebuild;
    FOnRootChange: TVETOnRootChange;
    FOnRootChanging: TVETOnRootChanging;
    FOnShellExecute: TVETOnShellExecute;
    FOnAfterShellNotify: TVETOnShellNotify;
    FOnShellNotify: TVETOnShellNotify;
    FOnTreeDblClick: TVETOnTreeDblClick;
    FVETState: TVETStates;
    FDisableWaitCursors: Boolean;
    FOnRootRebuild: TVETOnRootRebuild;
    FShellNotifyTimerHandle: THandle;
    FShellNotifyQueue: TList;
    FExpandingByButtonClick: Boolean;
    FShellContextSubMenu: TPopupMenu;
    FShellContextSubMenuCaption: WideString;
    FChangeNotifierEnabled: Boolean;
    FThreadedImagesEnabled: Boolean;
    FChangeNotifierCount: integer;
    FSortHelper: TShellSortHelper;
    FMalloc: IMalloc;
    FOnNamespaceStructureChange: TNamespaceStructureChangeEvent;
    FOnInvalidRootNamespace: TNotifyEvent;
    function GetContextMenuShown: Boolean;
    function GetNodeDataSize: Integer;
    function GetOkToShellNotifyDispatch: Boolean;
    function GetOptions: TVirtualExplorerTreeOptions;
    function GetRecycleBinNode: PVirtualNode;
    function GetSelectedFile: WideString;
    {$IFDEF TNTSUPPORT}
    function GetSelectedFiles: TWideStrings;
    function GetSelectedPaths: TWideStrings;
    {$ELSE}
    function GetSelectedFiles: TStrings;
    function GetSelectedPaths: TStrings;
    {$ENDIF}
    function GetSelectedPath: WideString;
    function GetSelectedPIDL: PItemIDList;
    function GetThreadedExpandMarkEnabled: Boolean;
    function InternalWalkPIDLToNode(PIDL: PItemIDList): PVirtualNode;
    procedure SetChangeNotiferEnabled(const Value: Boolean);
    procedure SetDisableWaitCursors(const Value: Boolean);
    procedure SetFileObjects(const Value: TFileObjects);
    procedure SetFileSizeFormat(const Value: TFileSizeFormat);
    procedure SetFileSort(const Value: TFileSort);
    procedure SetNodeDataSize(const Value: Integer);
    procedure SetOptions(const Value: TVirtualExplorerTreeOptions);
    procedure SetRootFolder(const Value: TRootFolder);
    procedure SetRootFolderCustomPath(const Value: WideString);
    procedure SetRootFolderCustomPIDL(const Value: PItemIDList);
    procedure SetThreadedExpandMarkEnabled(Value: Boolean);
    procedure SetThreadedImagesEnabled(const Value: Boolean);
  {$IFDEF EXPLORERTREE_L}
    procedure SetVirtualExplorerTree(const Value: TCustomVirtualExplorerTree);
  {$ENDIF}
    procedure SetColumnDetails(const Value: TColumnDetailType);
  {$IFDEF EXPLORERCOMBOBOX_L}
    procedure SetExplorerComboBox(const Value: TCustomVirtualExplorerCombobox);
  {$ENDIF}
    function GetNewStorage: TRootNodeStorage;

    property TempRootNamespace: TNamespace read FTempRootNamespace write FTempRootNamespace;
  protected
    function BuildEnumFlags: DWORD;
    function CanShowDragImage: Boolean; override;
    function ShowBkGndContextMenu(Point: TPoint): Integer; virtual;
    procedure ActivateTree(Activate: Boolean);
    procedure AddMyDocumentsFolder(FolderNode: PVirtualNode; DesktopFolderOnly: Boolean);
    procedure AfterValidEnumIDList(Sender: TObject);
    procedure CollapseNamespaceFolder(Node: PVirtualNode);
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DeleteNodeByPIDL(PIDL: PItemIDList);
    procedure DestroyWnd; override;
    function DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoClipboardCopy(var Handled: Boolean); virtual;
    procedure DoClipboardCut(var MarkSelectedCut: Boolean; var Handled: Boolean); virtual;
    procedure DoClipboardPaste(var Handled: Boolean); virtual;
    procedure DoCollapsed(Node: PVirtualNode); override;
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
    procedure DoColumnResize(Column: TColumnIndex); override;
    procedure DoColumnUserChangedVisibility; virtual;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoContextMenuAfterCmd(Namespace: TNamespace; Verb: WideString; MenuItemID: Integer; Successful: Boolean);
    function DoContextMenuCmd(Namespace: TNamespace; Verb: WideString; MenuItemID: Integer): Boolean;
    procedure DoContextMenuSelect(Namespace: TNamespace; MenuItemID : Integer; SubMenuID: hMenu; MouseSelect: Boolean);
    function DoContextMenuShow(Namespace: TNamespace; Menu: hMenu): Boolean;
    function DoCreateDataObject: IDataObject; override;
    procedure DoCustomColumnCompare(Column: TColumnIndex; Node1, Node2: PVirtualNode; var Result: integer); virtual;
    procedure DoCustomNamespace(AParentNode: PVirtualNode); virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoEdit; override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean); virtual;
    function DoExpanding(Node: PVirtualNode): Boolean; override;
    procedure DoEnumThreadLengthyOperation(var ShowAnimation: Boolean);
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;
    function DoGetPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint): TPopupMenu; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString); override;
    procedure DoGetVETText(Column: TColumnIndex; Node: PVirtualNode; Namespace: TNamespace; var Text: UnicodeString);
    procedure DoInvalidRootNamespace; virtual;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure DoHeaderMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoHeaderRebuild; virtual;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    procedure DoNamespaceStructureChange(Node: PVirtualNode; NS: TNamespace; ChangeType: TNamespaceStructureChange); virtual;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position: TPoint); override;
    procedure DoRootChange; virtual;
    procedure DoRootChanging(const NewRoot: TRootFolder; Namespace: TNamespace; var Allow: Boolean); virtual;
    procedure DoRootRebuild; virtual;
    procedure DoShellExecute(Node: PVirtualNode); virtual;
    procedure DoAfterShellNotify(ShellEvent: TVirtualShellEvent); virtual;
    procedure DoShellNotify(ShellEvent: TVirtualShellEvent); virtual;
    procedure DoTreeDblClick(Button: TMouseButton; Position: TPoint); virtual;
    procedure DoUpdating(State: TVTUpdateState); override;
    function DragDrop(const DataObject: IDataObject; KeyState: Integer; Pt: TPoint;  var Effect: Integer): HResult; override;
    function DragEnter(KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult; override;
    procedure DragAndDrop(AllowedEffects: Integer; DataObject: IDataObject; DragEffect: Integer); override;
    procedure DragLeave; override;
    function DragOver(Source: TObject; KeyState: Integer; DragState: TDragState; Pt: TPoint; var Effect: Integer): HResult; override;
    procedure DummyOnDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure EnumThreadFinished; virtual;
    procedure EnumThreadStart; virtual;
    procedure EnumThreadTimer(Enable: Boolean);
    procedure ExecuteNamespace(Namespace: TNamespace; var WorkingDir: WideString; var CmdLineArgument: WideString); virtual;
    function EnumerateFolderCallback(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace; Data: pointer; var Terminate: Boolean): Boolean;
    function ExpandNamespaceFolder(Node: PVirtualNode): integer; virtual;
    function FindFolderByNameCallback(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace; Data: Pointer; var Terminate: Boolean): Boolean;
    procedure ForceIconCachRebuild;
    function GetAnimateWndParent: TWinControl; virtual;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetHeaderClass: TVTHeaderClass; override;
    function GetColumnMenu: TColumnMenu; // prop getter for BCB compatibilty
    function GetOptionsClass: TTreeOptionsClass; override;
    function HasPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Pos: TPoint): Boolean; override;
    function InternalCreateNewFolder(TargetPIDL: PItemIDList; SuggestedFolderName: WideString): WideString; virtual;
    function InternalData(Node: PVirtualNode): Pointer; reintroduce;
    procedure HideAnimateFolderWnd;
    procedure InvalidateChildNamespaces(Node: PVirtualNode; RefreshIcon: Boolean);
    procedure InvalidateImageByIndex(ImageIndex: integer);
    procedure InvalidateNodeByPIDL(PIDL: PItemIDList);
    function IsAnyEditing: Boolean; virtual;  // Allows custom IsEditing logic for descenedents
    function ItemHasChildren(NS: TNamespace; Node, ParentNode: PVirtualNode): Boolean;
    procedure LiveColumnUpdate(Sender: TObject);
    procedure LoadDefaultOptions; virtual;
  {$IFDEF EXPLORERCOMBOBOX_L}
    procedure LoadExplorerComboBox(Reader: TReader);
  {$ENDIF}
    function NextSelectedNode(Node: PVirtualNode; DoSelectNext: Boolean; var ResultIsParent: Boolean): PVirtualNode;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function OkToBrowseTo(PIDL: PItemIDList): Boolean; virtual;
    function OkToExpandNode(Node: PVirtualNode): Boolean;
    function PathofNameSpace(NS: TNameSpace): WideString;
    procedure Notify(var Msg: TMessage);
    procedure ReadChildNodes(Node: PVirtualNode; var ANodeArray: TNodeSearchArray; Sorted: Boolean; var ValidNodesRead: Integer);
    procedure RebuildRootNamespace; virtual;
    procedure RebuildRootNamespaceBeginUpdate;
    procedure RebuildRootNamespaceEndUpdate;
    procedure RebuildShellHeader(BasedOnNamespace: TNamespace);
    procedure RebuildVETHeader;
    procedure RefreshNodeByPIDL(aPIDL: PItemIDList; ForceExpand, SaveSelection: Boolean);
    procedure ReStoreColumnState; virtual;
    procedure ShowAnimateFolderWnd; virtual;
    procedure StoreColumnState; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure ShellExecuteFolderLink(NS: TNamespace; WorkingDir, CmdLineArgument: WideString); virtual;
    function SuggestDropEffect(Source: TObject; Shift: TShiftState; Pt: TPoint; AllowedEffects: Integer): Integer; override;
    procedure TerminateEnumThread;
    function ValidRootNamespace: Boolean;
    function WalkPIDLToNode(AnAbsolutePIDL: PItemIDList; SelectNode, ForceExpand, QuietExpand, ShowAllSiblings: Boolean): PVirtualNode;
    procedure WaitCursor(Show: Boolean);
    procedure WMCommonThreadCallback(var Msg: TWMThreadRequest); message WM_COMMONTHREADCALLBACK;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
    procedure WMDrawItem(var Msg: TMessage); message WM_DRAWITEM;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMInitMenuPopup(var Msg: TMessage); message WM_INITMENUPOPUP;
    procedure WMInvalidFileName(var Msg: TMessage); message WM_INVALIDFILENAME;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMeasureItem(var Msg: TMessage); message WM_MEASUREITEM;
    procedure WMMenuChar(var Msg: TMessage); message WM_MENUCHAR;
    procedure WMMenuSelect(var Msg: TMessage); message WM_MENUSELECT;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
    procedure WMSysChar(var Msg: TWMSysChar); message WM_SYSCHAR;
    procedure WMSysKeyDown(var Msg: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var Msg: TWMSysKeyUp); message WM_SYSKEYUP;
    procedure WMQueryEndSession(var Msg: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    property Active: Boolean read FActive write SetActive;
    property AltKeyDown: Boolean read FAltKeyDown write FAltKeyDown;
    property AnimateFolderEnum: TAnimate read FAnimateFolderEnum write FAnimateFolderEnum;
    property BackGndMenu: TVirtualShellBackgroundContextMenu read FBackGndMenu write FBackGndMenu;
    property ChangeNotifierCount: integer read FChangeNotifierCount write FChangeNotifierCount;
    property ChangeNotifierEnabled: Boolean read FChangeNotifierEnabled write SetChangeNotiferEnabled;
    property ColumnDetails: TColumnDetailType read FColumnDetails write SetColumnDetails;
    property ColumnMenu: TColumnMenu read GetColumnMenu write FColumnMenu;
    property ColumnMenuItemCount: integer read FColumnMenuItemCount write FColumnMenuItemCount;
    property CreatingHeaders: Boolean read FCreatingHeaders write FCreatingHeaders;
    property ContextMenuManager: TContextMenuManager read FContextMenuManager write FContextMenuManager;
    property CurrentUpdateCount: Integer read FCurrentUpdateCount write FCurrentUpdateCount;
    property DisableWaitCursors: Boolean read FDisableWaitCursors write SetDisableWaitCursors;
    property DragDataObject: IDataObject read FDragDataObject write FDragDataObject;
    property DragMouseButton: integer read FDragMouseButton write FDragMouseButton;
    property Dropping: Boolean read FDropping;
    property EnumBkGndList: TCommonPIDLList read FEnumBkGndList write FEnumBkGndList;
    property EnumLock: TRTLCriticalSection read FEnumLock write FEnumLock;
    property EnumThread: TVirtualBackGndEnumThread read FEnumThread write FEnumThread;
    property EnumTimer: THandle read FEnumTimer write FEnumTimer;
    property EnumBkGndTime: Cardinal read FEnumBkGndTime write FEnumBkGndTime;
    property ExpandingByButtonClick: Boolean read FExpandingByButtonClick;  
  {$IFDEF EXPLORERCOMBOBOX_L}
    property ExplorerComboBox: TCustomVirtualExplorerCombobox read FExplorerComboBox write SetExplorerComboBox;
  {$ENDIF}
    property FileObjects: TFileObjects read FFileObjects write SetFileObjects default [foFolders];
    property FileSizeFormat: TFileSizeFormat read FFileSizeFormat write SetFileSizeFormat;
    property FileSort: TFileSort read FFileSort write SetFileSort;
    property LastDragEffect: integer read FLastDragEffect write FLastDragEffect;
    property LastDropTargetNode: PVirtualNode read FLastDropTargetNode write FLastDropTargetNode;
    property Malloc: IMalloc read FMalloc write FMalloc;
    property NodeDataSize: Integer read GetNodeDataSize write SetNodeDataSize default -1;
    property OnClipboardCopy: TVETOnClipboardCopy read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardCut: TVETOnClipboardCut read FOnClipboardCut write FOnClipboardCut;
    property OnClipboardPaste: TVETOnClipboardPaste read FOnClipboardPaste write FOnClipboardPaste;
    property OnColumnUserChangedVisiblility: TNotifyEvent read FOnColumnUserChangedVisiblility write FOnColumnUserChangedVisiblility;
    property OnContextMenuAfterCmd: TVETOnContextMenuAfterCmd read FOnContextMenuAfterCmd write FOnContextMenuAfterCmd;
    property OnContextMenuItemChange: TVETContextMenuItemChange read FOnContextMenuItemChange write FOnContextMenuItemChange;
    property OnContextMenuCmd: TVETOnContextMenuCmd read FOnContextMenuCmd write FOnContextMenuCmd;
    property OnContextMenuShow: TVETOnContextMenuShow read FOnContextMenuShow write FOnContextMenuShow;
    property OnCustomColumnCompare: TVETOnCustomColumnCompare read FOnCustomColumnCompare write FOnCustomColumnCompare;
    property OnCustomNamespace: TVETOnCustomNamespace read FOnCustomNamespace write FOnCustomNamespace;
    property OnEnumThreadLengthyOperation: TVETEnumLenghyOperaionEvent read FOnEnumThreadLengthyOperation write FOnEnumThreadLengthyOperation;
    property OnGetVETText: TVETOnDrawNodeText read FOnDrawNodeText write FOnDrawNodeText;
    property OnEnumFolder: TVETOnEnumFolder read FOnEnumFilter write FOnEnumFilter;
    property OnEnumFinished: TVETEnumFinishedEvent read FOnEnumFinished write FOnEnumFinished;
    property OnHeaderRebuild: TVETOnHeaderRebuild read FOnHeaderRebuild write FOnHeaderRebuild;
    property OnInvalidRootNamespace: TNotifyEvent read FOnInvalidRootNamespace write FOnInvalidRootNamespace;
    property OnNamespaceStructureChange: TNamespaceStructureChangeEvent read FOnNamespaceStructureChange write FOnNamespaceStructureChange;
    property OnRootChange: TVETOnRootChange read FOnRootChange write FOnRootChange;
    property OnRootChanging: TVETOnRootChanging read FOnRootChanging write FOnRootChanging;
    property OnRootRebuild: TVETOnRootRebuild read FOnRootRebuild write FOnRootRebuild;
    property OnShellExecute: TVETOnShellExecute read FOnShellExecute write FOnShellExecute;
    property OnAfterShellNotify: TVETOnShellNotify read FOnAfterShellNotify write FOnAfterShellNotify;
    property OnShellNotify: TVETOnShellNotify read FOnShellNotify write FOnShellNotify;
    property OnTreeDblClick: TVETOnTreeDblClick read FOnTreeDblClick write FOnTreeDblClick;
    property RebuildRootNamespaceCount: integer read FRebuildRootNamespaceCount write FRebuildRootNamespaceCount;
    property RecycleBinNode: PVirtualNode read GetRecycleBinNode;
    property RootFolder: TRootFolder read FRootFolder write SetRootFolder;
    property ShellBaseColumnCount: integer read FShellBaseColumnCount write FShellBaseColumnCount;
    property ShellContextSubMenu: TPopupMenu read FShellContextSubMenu write FShellContextSubMenu;
    property ShellContextSubMenuCaption: WideString read FShellContextSubMenuCaption write FShellContextSubMenuCaption;
    property OkToShellNotifyDispatch: Boolean read GetOkToShellNotifyDispatch;   // IShellNotify
    property ShellNotifyQueue: TList read FShellNotifyQueue write FShellNotifyQueue;
    property ShellNotifyTimerHandle: THandle read FShellNotifyTimerHandle write FShellNotifyTimerHandle;
    property SortHelper: TShellSortHelper read FSortHelper write FSortHelper;
    property ThreadedEnum: Boolean read FThreadedEnum write FThreadedEnum default False;
    property ThreadedImagesEnabled: Boolean read FThreadedImagesEnabled write SetThreadedImagesEnabled;
    property TreeOptions: TVirtualExplorerTreeOptions read GetOptions write SetOptions;
    property UnknownFolderIconIndex: integer read FUnknownFolderIconIndex write FUnknownFolderIconIndex;
    property UnknownFileIconIndex: integer read FUnknownFileIconIndex write FUnknownFileIconIndex;
    property VETColors: TVETColors read FVETColors write FVETColors;
    property VETState: TVETStates read FVETState write FVETState;
  {$IFDEF EXPLORERTREE_L}
    property VirtualExplorerTree: TCustomVirtualExplorerTree read FVirtualExplorerTree write SetVirtualExplorerTree;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddCustomNode(ParentNode: PVirtualNode; CustomNamespace: TNamespace; UsesCheckBoxes: Boolean; CheckBoxType: TCheckType = ctTriStateCheckBox): PVirtualNode;
    function AddNodeToTree(ParentNode: PVirtualNode): PVirtualNode;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function BrowseTo(APath: WideString; ExpandTarget, SelectTarget, SetFocusToVET, CollapseAllFirst: Boolean): Boolean; overload;
    function BrowseTo(APath: WideString; SetFocusToVET: Boolean = True): Boolean; overload;
    function BrowseToByPIDL(APIDL: PItemIDList; ExpandTarget, SelectTarget, SetFocusToVET, CollapseAllFirst: Boolean; ShowAllSiblings: Boolean = True): Boolean; virtual;
    procedure ChangeLinkChanging(Server: TObject; NewPIDL: PItemIDList); dynamic; // ChangeLink method
    procedure ChangeLinkDispatch; virtual;
    procedure ChangeLinkFreeing(ChangeLink: IVETChangeLink); dynamic;
    procedure Clear; override;
    procedure CopyToClipBoard; override;
    function CreateNewFolder(TargetPath: WideString): Boolean; overload;
    function CreateNewFolder(TargetPath: WideString; var NewFolder: WideString): Boolean; overload;
    function CreateNewFolder(TargetPath, SuggestedFolderName: WideString; var NewFolder: WideString): Boolean; overload;
    function CreateNewFolderByNode(Node: PVirtualNode): Boolean;
    procedure CutToClipBoard; override;
    procedure DeleteNode(Node: PVirtualNode; Reindex: Boolean = True); reintroduce;
    procedure DeleteSelectedNodes(ShiftKeyState: TExecuteVerbShift = evsCurrent); reintroduce;
    function DoCancelEdit: Boolean; override;
    function DoEndEdit: Boolean; override;
    function FindDesktopFolderByName(AName: WideString; var Namespace: TNamespace): Boolean;
    function FindNode(APath: WideString): PVirtualNode;
    function FindNodeByPIDL(APIDL: PItemIDList): PVirtualNode;
    function FindFolderByName(AName: WideString; var Namespace: TNamespace): Boolean;
    function ForceNode(APath: WideString; Expand: Boolean): PVirtualNode;
    function ForceNodeByPIDL(APIDL: PItemIDList; Expand: Boolean; ShowAllSiblings: Boolean = True): PVirtualNode;
    procedure EnableChangeNotifier(Enable: Boolean);
    procedure FlushChangeNotifier;
    procedure InitAllChildren(Node: PVirtualNode);
    procedure InitAllNodes;
    procedure Loaded; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure MarkNodesCut;
    procedure MarkNodesCopied;
    function PasteFromClipboard: Boolean; override;
    procedure RebuildHeader(BasedOnNamespace: TNamespace);
    function RebuildTree: Boolean;
    function RefreshNode(Node: PVirtualNode): Boolean;
    function RefreshTree(RestoreTopNode: Boolean = False): Boolean;
    procedure ReReadAndRefreshNode(Node: PVirtualNode; SortNode: Boolean); virtual;
    procedure SaveToStream(Stream: TStream; Node: PVirtualNode = nil); override;
    procedure SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift = evsCurrent); virtual;
    procedure SelectedFilesPaste(AllowMultipleTargets: Boolean); virtual;
    procedure SelectedFilesShowProperties; virtual;
    function SelectedToDataObject: IDataObject; virtual;
    function SelectedToNamespaceArray: TNamespaceArray; virtual;
    function SelectedToPIDLArray: TRelativePIDLArray; virtual;
    procedure ShowColumnDialog;
    function ValidateColumnManager(Node: PVirtualNode; var ColumnManager: TColumnManager): Boolean;
    function ValidateNamespace(Node: PVirtualNode; var Namespace: TNamespace): Boolean;
    function ValidateParentNamespace(Node: PVirtualNode; var Namespace: TNamespace): Boolean;
    property AutoFireChangeLink: Boolean read FAutoFireChangeLink write FAutoFireChangeLink;
    property ContextMenuShown: Boolean read GetContextMenuShown;
    property RootFolderCustomPath: WideString read FRootFolderCustomPath write SetRootFolderCustomPath;
    property RootFolderCustomPIDL: PItemIDList read FRootFolderCustomPIDL write SetRootFolderCustomPIDL;
    property RootFolderNamespace: TNamespace read FRootFolderNamespace;
    property SelectedFile: WideString read GetSelectedFile;
    {$IFDEF TNTSUPPORT}
    property SelectedFiles: TWideStrings read GetSelectedFiles;
    property SelectedPaths: TWideStrings read GetSelectedPaths;
    {$ELSE}
    property SelectedFiles: TStrings read GetSelectedFiles;
    property SelectedPaths: TStrings read GetSelectedPaths;
    {$ENDIF}
    property SelectedPath: WideString read GetSelectedPath;
    property SelectedPIDL: PItemIDList read GetSelectedPIDL;
    property ShellNotifySuspended: Boolean read FShellNotifySuspended write FShellNotifySuspended default False;
    property ThreadedExpandMarkEnabled: Boolean read GetThreadedExpandMarkEnabled write SetThreadedExpandMarkEnabled;
    property VETPersistent: TVETPersistent read FVETPersistent;
    property ViewManager: TViewManager read FViewManager write FViewManager;
    property Storage: TRootNodeStorage read GetNewStorage;
  end;

{*******************************************************************************}
{  TVirtualExplorerTree                                                         }
{*******************************************************************************}
  TVirtualExplorerTree = class(TCustomVirtualExplorerTree)
  public
     property ColumnMenu;
    property SortHelper;
  published
    property Action;
    property Active;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property BackGndMenu;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ButtonFillMode;
    property ButtonStyle;
    property ChangeDelay;
    property CheckImageKind;
    property Color;
    property Colors;
    property ColumnDetails;
    property ColumnMenuItemCount;
    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DragCursor;
    property DragHeight;
    property DragImageKind;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
  {$IFDEF EXPLORERCOMBOBOX_L}
    property ExplorerComboBox;
  {$ENDIF}
    property FileObjects;
    property FileSizeFormat;
    property FileSort;
    property Font;
    property Header;
    property HintAnimation;
    property HintMode;
    property HotCursor;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootFolder;
    property RootFolderCustomPath;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ScrollBarOptions;
    property ShellContextSubMenu;
    property ShellContextSubMenuCaption;
    property ShowHint;
    property TabOrder;
    property TabStop NoDefault;
    property TextMargin;
    property TreeOptions;
    property VETColors;
  {$IFDEF EXPLORERTREE_L}
    property VirtualExplorerTree;
  {$ENDIF}
    property Visible;      
    property OnAdvancedHeaderDraw;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnClipboardCopy;
    property OnClipboardCut;
    property OnClipboardPaste;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnColumnUserChangedVisiblility;
    property OnCompareNodes;
    property OnContextMenuAfterCmd;
    property OnContextMenuCmd;
    property OnContextMenuItemChange;
    property OnContextMenuShow;
    property OnCreateDataObject;
    property OnCreateEditor;
    property OnCustomColumnCompare;
    property OnCustomNamespace;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEndDock;
    property OnEnter;
    property OnEnumFolder;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetHelpContext;
    property OnGetHint;
    property OnGetImageIndexEx;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetVETText;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderDragged;
    property OnHeaderDragging;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHeaderRebuild;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnInvalidRootNamespace;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNamespaceStructureChange;
    property OnPaintBackground;
    property OnPaintText;
    property OnResize;
    property OnRootChange;
    property OnRootChanging;
    property OnRootRebuild;
    property OnScroll;
    property OnShellExecute;
    property OnAfterShellNotify;
    property OnShellNotify;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
    property OnTreeDblClick;
    property OnUpdating;
  end;


  TVirtualExplorerViews = class(TCustomVirtualExplorerTree)
  public
   property ColumnMenu: TColumnMenu read GetColumnMenu;
   property SortHelper;
  published
    property Action;
    property Active;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ButtonFillMode;
    property ButtonStyle;
    property ChangeDelay;
    property CheckImageKind;
    property Color;
    property Colors;
    property ColumnDetails;
    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DragCursor;
    property DragHeight;
    property DragImageKind;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
 {$IFDEF EXPLORERCOMBOBOX_L}
    property ExplorerComboBox;
 {$ENDIF}
    property FileObjects default [foFolders, foHidden];
    property FileSizeFormat;
    property FileSort;
    property Font;
    property Header;
    property HintAnimation;
    property HintMode;
    property HotCursor;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootFolder;
    property RootFolderCustomPath;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ScrollBarOptions;
    property ShellContextSubMenu;
    property ShellContextSubMenuCaption;
    property ShowHint;
    property TabOrder;
    property TabStop NoDefault;
    property TextMargin;
    property TreeOptions;
    property Visible;
    property VETColors;

    property OnAdvancedHeaderDraw;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnClipboardCopy;
    property OnClipboardCut;
    property OnClipboardPaste;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnColumnUserChangedVisiblility;
    property OnCompareNodes;
    property OnContextMenuAfterCmd;
    property OnContextMenuCmd;
    property OnContextMenuItemChange;
    property OnContextMenuShow;
    property OnCreateDataObject;
    property OnCreateEditor;
    property OnCustomColumnCompare;
    property OnCustomNamespace;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnEnumFinished;
    property OnEnumFolder;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetHelpContext;
    property OnGetHint;
    property OnGetImageIndexEx;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetVETText;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnInvalidRootNamespace;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNamespaceStructureChange;
    property OnNewText;
    property OnPaintBackground;
    property OnPaintText;
    property OnResize;
    property OnRootChange;
    property OnRootChanging;
    property OnRootRebuild;
    property OnScroll;
    property OnShellExecute;
    property OnShellNotify;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
    property OnTreeDblClick;
    property OnUpdating;
  end;

{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}
{ EXPLORER TREEVIEW                                                             }
{       Just a VET set up to be aware of the ExplorerListView and look like     }
{       the Explorer Treeview                                                   }
{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}

{*******************************************************************************}
{  TCustomExplorerTreeview                                                      }
{*******************************************************************************}
  TVirtualExplorerTreeview = class(TVirtualExplorerViews)
  private
  {$IFDEF EXPLORERLISTVIEW_L}
    FVirtualExplorerListview: TVirtualExplorerListview;
  {$ENDIF}
    FRightButtonDown: Boolean;
  {$IFDEF EXPLORERLISTVIEW_L}
    procedure SetVirtualExplorerListview(const Value: TVirtualExplorerListview);
  {$ENDIF}
  protected
    procedure LoadDefaultOptions; override;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;

    property RightButtonDown: Boolean read FRightButtonDown write FRightButtonDown;
  public
    procedure ChangeLinkFreeing(ChangeLink: IVETChangeLink); override;
  published
  {$IFDEF EXPLORERLISTVIEW_L}
    property VirtualExplorerListview: TVirtualExplorerListview
      read FVirtualExplorerListview write SetVirtualExplorerListview;
  {$ENDIF}
  end;

{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}
{ EXPLORER LISTVIEW                                                             }
{       Just a VET set up to be aware of the ExplorerTreeview and look like     }
{       the Explorer Listview (minus the icon view modes)                       }
{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}

{*******************************************************************************}
{  TCustomExplorerListview                                                      }
{*******************************************************************************}
  TVirtualExplorerListview = class(TVirtualExplorerViews)
  private
  {$IFDEF EXPLORERTREEVIEW_L}
    FVirtualExplorerTreeview: TVirtualExplorerTreeview;
  {$ENDIF}
    FBackBrowseRoot: TNamespace;
  {$IFDEF EXPLORERTREEVIEW_L}
    procedure SetVirtualExplorerTreeview(const Value: TVirtualExplorerTreeview);
  {$ENDIF}
    procedure SetBackBrowseRoot(const Value: TNamespace);
  protected
    function ShowBkGndContextMenu(Point: TPoint): Integer; override;
    procedure CreateWnd; override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    procedure DoShellExecute(Node: PVirtualNode); override;
    procedure LoadDefaultOptions; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function OkToBrowseTo(PIDL: PItemIDList): Boolean; override;
    procedure RebuildRootNamespace; override;
    procedure ShellExecuteFolderLink(NS: TNamespace; WorkingDir, CmdLineArgument: WideString); override;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function BrowseToByPIDL(APIDL: PItemIDList; ExpandTarget, SelectTarget, SetFocusToVET, CollapseAllFirst: Boolean; ShowAllSiblings: Boolean = True): Boolean; override;
    procedure BrowseToPrevLevel;
    function BrowseToNextLevel: Boolean;
    procedure ChangeLinkDispatch; override;
    procedure ChangeLinkFreeing(ChangeLink: IVETChangeLink); override;
    function PasteFromClipboard: Boolean; override;
    procedure ReReadAndRefreshNode(Node: PVirtualNode; SortNode: Boolean); override;
    procedure SelectedFilesPaste(AllowMultipleTargets: Boolean); override;

    property BackBrowseRoot: TNamespace read FBackBrowseRoot write SetBackBrowseRoot;
  published
    property BackGndMenu;
    property ColumnMenuItemCount;
  {$IFDEF EXPLORERTREEVIEW_L}
    property VirtualExplorerTreeview: TVirtualExplorerTreeview read FVirtualExplorerTreeview write SetVirtualExplorerTreeview;
  {$ENDIF}
    property ThreadedEnum;
    property OnEnumThreadLengthyOperation;
    property OnHeaderRebuild;
  end;

{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}
{ EXPLORER COMBOBOX                                                             }
{            The various pieces of the Unicode aware Explorer like Combobox.    }
{-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------}

  TComboEdit = class(TVirtualCustomEdit)
  private
    FKeyPressed: Boolean;
    FIsEditing: Boolean;
    FExplorerComboBox: TCustomVirtualExplorerCombobox;
    FOwnerControl: TCustomVirtualExplorerCombobox;
    FOldNamespace: TNamespace;
    FStyle: TShellComboStyle;
    FColorOldFont: TColor;
    FColorOldWindow: TColor;
    FCursorOld: TCursor;
    FSelectingAll: Boolean;

    function GetText: WideString;
    procedure SetText(const Value: WideString);
    procedure SetOldNamespace(const Value: TNamespace);
    procedure SetStyle(const Value: TShellComboStyle);

  protected
    procedure CreateWnd; override;
    procedure DefaultOnInvalidEntry(InvalidText: WideString);
    procedure DoOnInvalidEntry(InvalidPath: WideString); virtual;
    procedure HandleDropDowns(DropDown: TDropDown);
    procedure PaintWindow(DC: HDC); override;
    procedure SelectEnteredPath;
    procedure UndoBufferSave;
    procedure UndoBufferRestore;

    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMComboSelectAll(var Msg: TMessage); message WM_VETCOMBO_SELECTALL;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMSysKeyDown(var Message: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure CMSysColorChange(var Message: TWMSysColorChange); message CM_SYSCOLORCHANGE;

    property ColorOldFont: TColor read FColorOldFont write FColorOldFont;
    property ColorOldWindow: TColor read FColorOldWindow write FColorOldWindow;
    property CursorOld: TCursor read FCursorOld write FCursorOld;
    property IsEditing: Boolean read FIsEditing write FIsEditing;
    property KeyPressed: Boolean read FKeyPressed write FKeyPressed;
    property OldNamespace: TNamespace read FOldNamespace write SetOldNamespace;
    property OwnerControl: TCustomVirtualExplorerCombobox read FOwnerControl write FOwnerControl;
    property SelectingAll: Boolean read FSelectingAll write FSelectingAll;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;

    property ExplorerComboBox: TCustomVirtualExplorerCombobox read FExplorerComboBox write FExplorerComboBox;
    property Style: TShellComboStyle read FStyle write SetStyle;
    property Text: WideString read GetText write SetText;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  // ---------------------------------------------------------------------------
  //  TSizeGrabber
  //   Necessary because I could not get the special scrollbar size window
  //   to stop forcing the focus to the window after sizing.  The Explorer autocomplete
  //   somehow does it but I could not figure it out.  Any sizing event seems to give
  //   the focus to the sized window without question.  Need to someday find out if
  //   this how it does it.  Anyway this will allow me to use Themes without a Manifest
  // ---------------------------------------------------------------------------
  TSizeGrabber = class(TCustomControl)
  private
    FCaptured: Boolean;               // True when the mouse is captured
    FDragStartPos: TPoint;            // Used to calculate the Delta move when sizing
    FOwnerDropDown: TDropDownWnd;     // The DropDown window we are controlling
    FThemeScrollbar: HTheme;          // Theme handle for a scrollbar
    FThemesActive: Boolean;           // Cached value to check if we should draw using Theme API
    FTransparent: Boolean;            // Paints the grabber transparently for when there is not scrollbar in the window
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure PaintGrabber(DC: hDC);

    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;

    property Captured: Boolean read FCaptured write FCaptured;
    property DragStartPos: TPoint read FDragStartPos write FDragStartPos;
    property OwnerDropDown: TDropDownWnd read FOwnerDropDown write FOwnerDropDown;
    property ThemesActive: Boolean read FThemesActive write FThemesActive;
    property ThemeScrollbar: HTheme read FThemeScrollbar write FThemeScrollbar;
    property Transparent: Boolean read FTransparent write FTransparent default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // ---------------------------------------------------------------------------
  //  TDropDownWnd
  //    The basis for the container window that drops down as in the Autocomplete
  //    and the Explorer view in the ExplorerCombobox.  It handles most of the
  //    details necessary on positioning and handling of the scrollbar and grabber
  //    window.  These are separate piece so the window will look like the M$
  //    implmenation in Explorer and IE.
  // ---------------------------------------------------------------------------
  TDropDownWnd = class(TCustomControl)
  private
    FPopupOptions: TPopupOptions;
    FAnimationSpeed: integer;
    FPopupStates: TPopupStates;
    FRemoteScrollbar: TOwnerDrawScrollbar;
    FGrabber: TSizeGrabber;

    FDropDownCount: Cardinal;         // How many "Row" based on RowHeight virtual method to be shown

    { Event Handlers }
    FOnRollDown: TOnPopupRollDown;
    FOnRollUp: TOnPopupRollUp;

    { Scrolling support }
    FAutoScrollTimerStub: Pointer;    // Stub for timer callback function (object method)
    FAutoScrollTimer: integer;        // Timer Handle
    FAutoScrollSlowTime: integer;     // Scroll time when mouse is down and dragged a few pixels out of window
    FAutoScrollFastTime: integer;    // Scroll time when mouse is down and dragged > 20 pixels out of window
    FAutoScrollWindow: TWinControl;  // The windows that recieves the WM_VScroll messages
    FOnRollDownInit: TNotifyEvent;   // Event fired just before the popup is shown
    FReFocusWindow: TWinControl;     // The window to set focus to on RollUp, if nil then HookedControl is focused (if Possible)
    FWheelMouseTarget: TWinControl;
    FOwnerControl: TWinControl;

    procedure SetPopupOptions(const Value: TPopupOptions);
    function GetScrolling: Boolean;
    procedure SetDropDownCount(const Value: Cardinal);
  protected

    FLastMousePos: TPoint;            // Used to keep track of the last Mouse position to keep issues between
                                      //   Keyboard and Mouse events that change the node focus
    function AllowClickInWindow(Window: HWnd; Point: TPoint): Boolean; virtual; // Override to allow a click in the popup with out closing it
    procedure AnimateRollDown;
    procedure AutoPositionPopup(AControl: TWinControl; InitialExtents: PPoint); // Override to customize the size/position of the popup
    procedure AutoScrollTimerCallback(Window: hWnd; Msg, idEvent: integer; dwTime: Longword); stdcall;
    procedure BitBltGrabber(Canvas: TCanvas; Host: TWinControl; BkGndColor: TColor);
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoRollDown(var Allow: Boolean); virtual; // Called first during a rolldown, popup size/position info is not set yet
    procedure DoRollDownInit; virtual; // called when dropdown is off the Screen but visible so initizations that need a visible window (many in VT <g>) can be done
    procedure DoRollUp(Selected: Boolean); virtual; // Called when the popup is hidden
    procedure DropDownMessageLoop;
    function GrabberPolyRgn(Grabber: TSizeGrabber; Host: TWinControl): HRgn;
    procedure KeyPressDispatch(var Message: TMessage; var Handled: Boolean); virtual; // Called when a keystroke occurs in the Form control that is the parent of the popup
    procedure RefreshScrollbar; virtual;    // Override to link your main window with the RemoteScrollbar property
    function RowHeight: Cardinal; virtual;  // The height of one "row" of the dropdown, should be overriden
    procedure RealignChildWindows(NewWidth, NewHeight: integer); virtual; // override to move/resize any new windows added to the popup

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMUpdateScrollbar(var Message: TMessage); message WM_UPDATESCROLLBAR;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AnimationSpeed: integer read FAnimationSpeed write FAnimationSpeed default 200;
    procedure RollDown(AnOwnerControl, AlignmentControl: TWinControl; Extents: PPoint);
    procedure RollUp(Select: Boolean);
    procedure ScrollTimerCreate(FastScroll: Boolean);  // Starts the autoscroll timer in fast/slow scroll speed
    procedure ScrollTimerDestroy(ClearFlags: Boolean); // Destroys the autoscroll timers

    property AutoScrollSlowTime: integer read FAutoScrollSlowTime write FAutoScrollSlowTime default 200;
    property AutoScrollFastTime: integer read FAutoScrollFastTime write FAutoScrollFastTime default 10;
    property AutoScrollWindow: TWinControl read FAutoScrollWindow write FAutoScrollWindow;
    property DropDownCount: Cardinal read FDropDownCount write SetDropDownCount;
    property ReFocusWindow: TWinControl read FReFocusWindow write FReFocusWindow;
    property Grabber: TSizeGrabber read FGrabber write FGrabber;
    property WheelMouseTarget: TWinControl read FWheelMouseTarget write FWheelMouseTarget;
    property OnRollDownInit: TNotifyEvent read FOnRollDownInit write FOnRollDownInit;
    property OnRollDown: TOnPopupRollDown read FOnRollDown write FOnRollDown;
    property OnRollUp: TOnPopupRollUp read FOnRollUp write FOnRollUp;
    property OwnerControl: TWinControl read FOwnerControl;
    property PopupOptions: TPopupOptions read FPopupOptions write SetPopupOptions default DefaultPopupOptions;
    property PopupStates: TPopupStates read FPopupStates;
    property RemoteScrollbar: TOwnerDrawScrollbar read FRemoteScrollbar write FRemoteScrollbar;
    property Scrolling: Boolean read GetScrolling;
  end;

  // ---------------------------------------------------------------------------
  //  TPopupExplorerTree
  //    Creates a specialized VET window that can be used as a contained window
  //    in a TDropDownWnd.  It deals with the autohotracking of the selected item
  //    and paint issues necessary for the Grabber if the Scrollbar is not visible
  // ---------------------------------------------------------------------------
  TPopupExplorerTree = class(TVirtualExplorerTree)
  private
    FPopupExplorerDropDown: TPopupExplorerDropDown;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCollapsed(Node: PVirtualNode); override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure LoadDefaultOptions; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BrowseToByPIDL(APIDL: PItemIDList; ExpandTarget, SelectTarget, SetFocusToVET,
      CollapseAllFirst: Boolean; ShowAllSiblings: Boolean = True): Boolean; override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean); override;

    property PopupExplorerDropDown: TPopupExplorerDropDown read FPopupExplorerDropDown write FPopupExplorerDropDown;
  end;

  // ---------------------------------------------------------------------------
  //   TComboDropOptions
  //     Proxy class to show in Object Inspector at designtime and interface
  //     with a TPopupExplorerTree
  // ---------------------------------------------------------------------------
  TPopupExplorerOptions = class(TPersistent)
  private
    FPopupExplorerDropDown: TPopupExplorerDropDown;
    function GetAnimationSpeed: integer;
    function GetAutoScrollTimeFast: integer;
    function GetAutoScrollTimeSlow: integer;
    function GetBackground: TPicture;
    function GetBackgroundOffsetX: integer;
    function GetBackgroundOffsetY: integer;
    function GetComboBoxStyle: TComboBoxStyle;
    function GetDefaultNodeHeight: Cardinal;
    function GetDropDownCount: integer;
    function GetIndent: integer;
    function GetOnRollDown: TOnPopupRollDown;
    function GetOnRollUp: TOnPopupRollUp;
    function GetOptions: TPopupOptions;
    procedure SetAnimationSpeed(const Value: integer);
    procedure SetAutoScrollTimeFast(const Value: integer);
    procedure SetAutoScrollTimeSlow(const Value: integer);
    procedure SetBackground(const Value: TPicture);
    procedure SetBackgroundOffsetX(const Value: integer);
    procedure SetBackgroundOffsetY(const Value: integer);
    procedure SetComboBoxStyle(const Value: TComboBoxStyle);
    procedure SetDefaultNodeHeight(const Value: Cardinal);
    procedure SetDropDownCount(const Value: integer);
    procedure SetIndent(const Value: integer);
    procedure SetOnRollDown(const Value: TOnPopupRollDown);
    procedure SetOnRollUp(const Value: TOnPopupRollUp);
    procedure SetOptions(const Value: TPopupOptions);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetOnEnumFolder: TVETOnEnumFolder;
    procedure SetOnEnumFolder(const Value: TVETOnEnumFolder);
  protected
    property PopupExplorerDropDown: TPopupExplorerDropDown read FPopupExplorerDropDown write FPopupExplorerDropDown;
  published
    property AnimationSpeed: integer read GetAnimationSpeed write SetAnimationSpeed default 200;
    property AutoScrollTimeFast: integer read GetAutoScrollTimeFast write SetAutoScrollTimeFast default 10;
    property AutoScrollTimeSlow: integer read GetAutoScrollTimeSlow write SetAutoScrollTimeSlow default 200;
    property Background: TPicture read GetBackground write SetBackground;
    property BackgroundOffsetX: integer read GetBackgroundOffsetX write SetBackgroundOffsetX default 0;
    property BackgroundOffsetY: integer read GetBackgroundOffsetY write SetBackgroundOffsetY default 0;
    property Color: TColor read GetColor write SetColor default clWindow;
    property ComboBoxStyle: TComboBoxStyle read GetComboBoxStyle write SetComboBoxStyle default cbsClassic;
    property DefaultNodeHeight: Cardinal read GetDefaultNodeHeight write SetDefaultNodeHeight default 17;
    property DropDownCount: integer read GetDropDownCount write SetDropDownCount default 8;
    property Indent: integer read GetIndent write SetIndent default 10;
    property OnEnumFolder: TVETOnEnumFolder read GetOnEnumFolder write SetOnEnumFolder;
    property OnRollDown: TOnPopupRollDown read GetOnRollDown write SetOnRollDown;
    property OnRollUp: TOnPopupRollUp read GetOnRollUp write SetOnRollUp;
    property Options: TPopupOptions read GetOptions write SetOptions default [poEnabled, poRespectSysAnimationFlag, poThemeAware];
  end;

  // ---------------------------------------------------------------------------
  //  TPopupExplorerDropDown
  //    A specalized TDropDownWnd that contains a TPopupExplorerTree. It handles
  //    the task of being the interface between the RemoteScrollbar in the
  //    TDropDownWnd and the VET window as well has handling the keystrokes of
  //    the UnicodeEdit which is hooked and still has the focus while the
  //    window is visible
  // ---------------------------------------------------------------------------
  TPopupExplorerDropDown = class(TDropDownWnd)
  private
    FComboBoxStyle: TComboBoxStyle;         // Either a classic style combo or an Enhanced with expandable nodes
    FPopupExplorerTree: TPopupExplorerTree; // The Child VET
    FTargetPIDL: PItemIDList;               // On drop down set the intitial PIDL to browse to, on rollup it points to the selected PIDL (or nil if canceled)
    FExplorerCombobox: TCustomVirtualExplorerCombobox;
    FSelectOnDropDown: Boolean;  // If true the target is selected on drop down

    procedure SetComboBoxStyle(const Value: TComboBoxStyle);
    function GetPopupOptions: TPopupOptions;
    procedure SetPopupOptions(const Value: TPopupOptions);
  protected
    function AllowClickInWindow(Window: HWnd; Point: TPoint): Boolean; override;
    function CreatePopupExplorerTree: TPopupExplorerTree; virtual;
    procedure DoRollDownInit; override;
    procedure DoRollUp(Selected: Boolean); override;
    procedure KeyPressDispatch(var Message: TMessage; var Handled: Boolean); override;
    procedure RealignChildWindows(NewWidth, NewHeight: integer); override;
    procedure RefreshScrollbar; override;
    function RowHeight: Cardinal; override;

    procedure WMVScroll(var message: TWMVScroll); message WM_VSCROLL;

    property ComboBoxStyle: TComboBoxStyle read FComboBoxStyle write SetComboBoxStyle default cbsClassic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ExplorerCombobox: TCustomVirtualExplorerCombobox read FExplorerCombobox write FExplorerCombobox;
    property PopupExplorerTree: TPopupExplorerTree read FPopupExplorerTree;
    property PopupOptions: TPopupOptions read GetPopupOptions write SetPopupOptions default DefaultPopupOptions;
    property TargetPIDL: PItemIDList read FTargetPIDL write FTargetPIDL;
    property SelectOnDropDown: Boolean read FSelectOnDropDown write FSelectOnDropDown;
  end;


  // ---------------------------------------------------------------------------
  //  TPopupAutoCompleteTree
  //    Creates a specialized VT window that can be used as a contained window
  //    in a TDropDownWnd.  It deals with the autohotracking of the selected item
  //    and paint issues necessary for the Grabber if the Scrollbar is not visible
  // ---------------------------------------------------------------------------
  TPopupAutoCompleteTree = class(TVirtualStringTree)
  private
    FPopupAutoCompleteDropDown: TPopupAutoCompleteDropDown;
    FAutoScrollLastMousePos: TPoint;
    FAutoComplete: TVirtualShellAutoComplete;
    {$IFDEF TNTSUPPORT}
    FStrings: TTntStringList;
    {$ELSE}
    FStrings: TStringList;
    {$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: UnicodeString); override;
    {$IFDEF TNTSUPPORT}
     procedure DoUpdateList(const CurrentEditContents: WideString;
      EnumList: TTntStringList; var Handled: Boolean);
    {$ELSE}
    procedure DoUpdateList(const CurrentEditContents: WideString;
      EnumList: TStringList; var Handled: Boolean);
    {$ENDIF}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateList(CurrentEditStr: WideString);

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;

    property AutoComplete: TVirtualShellAutoComplete read FAutoComplete write FAutoComplete;
    property AutoScrollLastMousePos: TPoint read FAutoScrollLastMousePos write FAutoScrollLastMousePos;
    {$IFDEF TNTSUPPORT}
    property Strings: TTntStringList read FStrings write FStrings;
    {$ELSE}
    property Strings: TStringList read FStrings write FStrings;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PopupAutoCompleteDropDown: TPopupAutoCompleteDropDown read FPopupAutoCompleteDropDown write FPopupAutoCompleteDropDown;
  end;

  // ---------------------------------------------------------------------------
  //   TPopupAutoCompleteOptions
  //     Proxy class to show in Object Inspector at designtime and interface
  //     with a TPopupAutoCompleteTree
  // ---------------------------------------------------------------------------
  TPopupAutoCompleteOptions = class(TPersistent)
  private
    FPopupAutoCompleteDropDown: TPopupAutoCompleteDropDown;
    function GetAnimationSpeed: integer;
    function GetAutoScrollTimeFast: integer;
    function GetAutoScrollTimeSlow: integer;
    function GetBackground: TPicture;
    function GetBackgroundOffsetX: integer;
    function GetBackgroundOffsetY: integer;
    function GetContents: TAutoCompleteContents;
    function GetDefaultNodeHeight: Cardinal;
    function GetDropDownCount: integer;
    function GetIndent: integer;
    function GetOnAutoCompleteUpdateList: TOnAutoCompleteUpdateList;
    function GetOnRollDown: TOnPopupRollDown;
    function GetOnRollUp: TOnPopupRollUp;
    function GetOptions: TPopupOptions;
    procedure SetAnimationSpeed(const Value: integer);
    procedure SetAutoScrollTimeFast(const Value: integer);
    procedure SetAutoScrollTimeSlow(const Value: integer);
    procedure SetBackground(const Value: TPicture);
    procedure SetBackgroundOffsetX(const Value: integer);
    procedure SetBackgroundOffsetY(const Value: integer);
    procedure SetContents(const Value: TAutoCompleteContents);
    procedure SetDefaultNodeHeight(const Value: Cardinal);
    procedure SetDropDownCount(const Value: integer);
    procedure SetIndent(const Value: integer);
    procedure SetOnAutoCompleteUpdateList(const Value: TOnAutoCompleteUpdateList);
    procedure SetOnRollDown(const Value: TOnPopupRollDown);
    procedure SetOnRollUp(const Value: TOnPopupRollUp);
    procedure SetOptions(const Value: TPopupOptions);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    procedure AlwaysShowReader(Reader: TReader);
    procedure DefineProperties(Filer: TFiler); override;
    property PopupAutoCompleteDropDown: TPopupAutoCompleteDropDown read FPopupAutoCompleteDropDown write FPopupAutoCompleteDropDown;
  published
    property AnimationSpeed: integer read GetAnimationSpeed write SetAnimationSpeed default 200;
    property AutoScrollTimeFast: integer read GetAutoScrollTimeFast write SetAutoScrollTimeFast default 10;
    property AutoScrollTimeSlow: integer read GetAutoScrollTimeSlow write SetAutoScrollTimeSlow default 200;
    property Background: TPicture read GetBackground write SetBackground;
    property BackgroundOffsetX: integer read GetBackgroundOffsetX write SetBackgroundOffsetX default 0;
    property BackgroundOffsetY: integer read GetBackgroundOffsetY write SetBackgroundOffsetY default 0;
    property Color: TColor read GetColor write SetColor default clWindow;
    property Contents: TAutoCompleteContents read GetContents write SetContents default [accCurrentDir, accMyComputer, accFileSysDirs, accFileSysFiles];
    property DefaultNodeHeight: Cardinal read GetDefaultNodeHeight write SetDefaultNodeHeight default 17;
    property DropDownCount: integer read GetDropDownCount write SetDropDownCount default 8;
    property Indent: integer read GetIndent write SetIndent default 0;
    property OnRollDown: TOnPopupRollDown read GetOnRollDown write SetOnRollDown;
    property OnRollUp: TOnPopupRollUp read GetOnRollUp write SetOnRollUp;
    property OnAutoCompleteUpdateList: TOnAutoCompleteUpdateList read GetOnAutoCompleteUpdateList write SetOnAutoCompleteUpdateList;
    property Options: TPopupOptions read GetOptions write SetOptions default [poEnabled, poRespectSysAnimationFlag, poThemeAware];
  end;

  // ---------------------------------------------------------------------------
  //  TPopupAutoCompleteDropDown
  //    A specalized TDropDownWnd that contains a TPopupAutoCompleteTree. It handles
  //    the task of being the interface between the RemoteScrollbar in the
  //    TDropDownWnd and the VT window as well has handling the keystrokes of
  //    the UnicodeEdit which is hooked and still has the focus while the
  //    window is visible
  // ---------------------------------------------------------------------------
  TPopupAutoCompleteDropDown = class(TDropDownWnd)
  private
    FPopupAutoCompleteTree: TPopupAutoCompleteTree; // The Child VET
    FExplorerCombobox: TCustomVirtualExplorerCombobox;
    FOnAutoCompleteUpdateList: TOnAutoCompleteUpdateList;

    function GetPopupOptions: TPopupOptions;
    procedure SetPopupOptions(const Value: TPopupOptions);
  protected
    function AllowClickInWindow(Window: HWnd; Point: TPoint): Boolean; override;
    function CreatePopupAutoCompleteTree: TPopupAutoCompleteTree; virtual;
    procedure DoRollDown(var Allow: Boolean); override;
    procedure DoRollDownInit; override;
    procedure DoRollUp(Selected: Boolean); override;
    procedure KeyPressDispatch(var Message: TMessage; var Handled: Boolean); override;
    procedure RealignChildWindows(NewWidth, NewHeight: integer); override;
    procedure RefreshScrollbar; override;
    function RowHeight: Cardinal; override;

    procedure WMVScroll(var message: TWMVScroll); message WM_VSCROLL;

    property OnAutoCompleteUpdateList: TOnAutoCompleteUpdateList read FOnAutoCompleteUpdateList write FOnAutoCompleteUpdateList;
    property PopupAutoCompleteTree: TPopupAutoCompleteTree read FPopupAutoCompleteTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ExplorerCombobox: TCustomVirtualExplorerCombobox read FExplorerCombobox write FExplorerCombobox;
    property PopupOptions: TPopupOptions read GetPopupOptions write SetPopupOptions default DefaultPopupOptions;
  end;

  // ---------------------------------------------------------------------------
  // TCustomVirtualExplorerCombobox
  //   The base class for the ExplorerCombobox that is the actual component
  //   that is dropped on the form.  It contains the Ownerdraw "Edit" frame,
  //   the ComboEdit, the Popup for the combobox, and the popup for the
  //   Autocomplete.
  // ---------------------------------------------------------------------------
  TCustomVirtualExplorerCombobox = class(TCustomControl)
  private

    FMouseTimer: TTimer;
    FOldFontOnChange: TNotifyEvent;                   // Hooked OnFontChange
    FOnDecodeSpecialVariable: TVETComboBoxDecodeSpecialVariableEvent;
    FOnPathChanging: TOnComboPathChanging;
    FVirtualExplorerTree: TCustomVirtualExplorerTree; // Change Linked VET

    { XP Theme support }
    FThemesActive: Boolean;
    FThemeCombo: HTHEME;
    FThemeEdit: HTHEME;
    FThemeButton: HTHEME;

    { Event support }
    FOnInvalidEntry: TVETOnComboInvalidEntry;   // Event called when invalid path typed in ComboEdit

    FEditNamespace: TNamespace;                 // Namespace associted with current edit path

    // Edit visual support
    FImageIndex: integer;                       // Index used when a non folder is selected in combobox
    FButtonRect: TRect;                         // Rectangle for dropdown button
    FVETComboState: TVETComboStates;            // Dynamic state of the combo box
    FTextType: TExplorerComboboxText;           // Show only filename or whole path

    // Visual objects that make up a TExplorerCombobox
    FOptions: TVETComboOptions;
    FPopupAutoCompleteDropDown: TPopupAutoCompleteDropDown;
    FPopupAutoCompleteOptions: TPopupAutoCompleteOptions;
    FPopupExplorerDropDown: TPopupExplorerDropDown;
    FPopupExplorerOptions: TPopupExplorerOptions;
    FComboEdit: TComboEdit;
    FOnDraw: TVETOnComboDraw;
    FOnPathChange: TOnComboPathChange;
    FStyle: TShellComboStyle;
    FFlat: Boolean;
    FBorderStyle: TBorderStyle;
    FActive: Boolean;
    FHotTracking: Boolean;

    function GetColor: TColor;
    function GetComboEdit: TComboEdit;
    function GetImageIndex: integer;
    function GetOnAutoCompleteRollDown: TOnPopupRollDown;
    function GetOnAutoCompleteRollUp: TOnPopupRollUp;
    function GetOnAutoCompleteUpdateList: TOnAutoCompleteUpdateList;
    function GetOnChange: TNotifyEvent;
    function GetOnComboRollDown: TOnPopupRollDown;
    function GetOnComboRollUp: TOnPopupRollUp;
    function GetOnEnter: TNotifyEvent;
    function GetOnEnumFolder: TVETOnEnumFolder;
    function GetOnExit: TNotifyEvent;
    function GetPath: WideString;
    function GetTabStop: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetEditNamespace(const Value: TNamespace);
    procedure SetOnEnumFolder(const Value: TVETOnEnumFolder);
    procedure SetOnAutoCompleteRollDown(const Value: TOnPopupRollDown);
    procedure SetOnAutoCompleteRollUp(const Value: TOnPopupRollUp);
    procedure SetOnAutoCompleteUpdateList(const Value: TOnAutoCompleteUpdateList);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnComboRollDown(const Value: TOnPopupRollDown);
    procedure SetOnComboRollUp(const Value: TOnPopupRollUp);
    procedure SetOnEnter(const Value: TNotifyEvent);
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetOptions(const Value: TVETComboOptions);
    procedure SetPath(const Value: WideString);
    procedure SetStyle(const Value: TShellComboStyle);
    procedure SetTabStop(const Value: Boolean);
    procedure SetTextType(const Value: TExplorerComboboxText);
    procedure SetVirtualExplorerTree(const Value: TCustomVirtualExplorerTree);
    function GetPopupExplorerTree: TPopupExplorerTree;
    function GetAutoComplete: TVirtualShellAutoComplete;
    function GetOnAutoCompleteAddItem: TVirtualAutoCompleteAddItem;
    procedure SetOnAutoCompleteAddItem(const Value: TVirtualAutoCompleteAddItem);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetFlat(const Value: Boolean);
  protected
    function BackGroundRect(ItemRect: TComboItemRect): TRect;
    procedure ButtonClicked(Sender: TObject);
    function CalculateEditHeight: integer;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeLinkDispatch(PIDL: PItemIDList); virtual;
    function CreatePopupAutoCompleteOptions: TPopupAutoCompleteOptions; virtual;
    function CreatePopupAutoCompleteDropDown: TPopupAutoCompleteDropDown; virtual;
    function CreatePopupExplorerOptions: TPopupExplorerOptions; virtual;
    function CreatePopupExplorerDropDown: TPopupExplorerDropDown; virtual;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoDecodeSpecialVariable(Variable: WideString; var NS: TNamespace); virtual;
    procedure DoDraw(ACanvas: TCanvas; AClientRect, AButtonRect: TRect; AComboState: TVETComboStates; Stage: TCustomDrawStage; var DefaultDraw: Boolean); virtual;
    procedure DoFontChange(NewFont: TFont);
    procedure DoPathChange(SelectedNamespace: TNamespace); virtual;
    procedure DoPathChanging(NS: TNamespace; var Allow: Boolean); virtual;
    procedure FontChange(Sender: TObject);
    procedure FreeThemes;
    function MouseInDropDownButton: Boolean;
    procedure MouseTimerCallback(Sender: TObject);
    procedure Paint; override;
    procedure PaintCombo(PaintDC: HDC);
    procedure RealignControls;
    procedure RefreshComboEdit(SelectText: Boolean);
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetComboEditColor(NewColor: TColor);
    procedure SetEnabled(Value: Boolean); override;
    procedure SetName(const Value: TComponentName); override;
    procedure UpdateDropDownButtonState;

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;

    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

    property Active: Boolean read FActive write SetActive default False;
    property AutoComplete: TVirtualShellAutoComplete read GetAutoComplete;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color: TColor read GetColor write SetColor default clWindow;
    property EditNamespace: TNamespace read FEditNamespace write SetEditNamespace;
    property Flat: Boolean read FFlat write SetFlat default False;
    property ImageIndex: integer read GetImageIndex;
    property MouseTimer: TTimer read FMouseTimer write FMouseTimer;
    property OldFontOnChange: TNotifyEvent read FOldFontOnChange write FOldFontOnChange;
    property OnDecodeSpecialVariable: TVETComboBoxDecodeSpecialVariableEvent read FOnDecodeSpecialVariable write FOnDecodeSpecialVariable;
    property OnInvalidEntry: TVETOnComboInvalidEntry read FOnInvalidEntry write FOnInvalidEntry;
    property OnAutoCompleteRollDown: TOnPopupRollDown read GetOnAutoCompleteRollDown write SetOnAutoCompleteRollDown;
    property OnAutoCompleteRollUp: TOnPopupRollUp read GetOnAutoCompleteRollUp write SetOnAutoCompleteRollUp;
    property OnAutoCompleteAddPath: TVirtualAutoCompleteAddItem read GetOnAutoCompleteAddItem write SetOnAutoCompleteAddItem;
    property OnAutoCompleteUpdateList: TOnAutoCompleteUpdateList read GetOnAutoCompleteUpdateList write SetOnAutoCompleteUpdateList;
    property OnComboRollDown: TOnPopupRollDown read GetOnComboRollDown write SetOnComboRollDown;
    property OnComboRollUp: TOnPopupRollUp read GetOnComboRollUp write SetOnComboRollUp;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnDraw: TVETOnComboDraw read FOnDraw write FOnDraw;
    property OnEnter: TNotifyEvent read GetOnEnter write SetOnEnter;
    property OnEnumFolder: TVETOnEnumFolder read GetOnEnumFolder write SetOnEnumFolder;
    property OnExit: TNotifyEvent read GetOnExit write SetOnExit;
    property OnPathChange: TOnComboPathChange read FOnPathChange write FOnPathChange;
    property OnPathChanging: TOnComboPathChanging read FOnPathChanging write FOnPathChanging;
    property Options: TVETComboOptions read FOptions write SetOptions default DefaultExplorerComboOptions;
    property Path: WideString read GetPath write SetPath;
    property PopupAutoCompleteDropDown: TPopupAutoCompleteDropDown read FPopupAutoCompleteDropDown;
    property PopupAutoCompleteOptions: TPopupAutoCompleteOptions read FPopupAutoCompleteOptions write FPopupAutoCompleteOptions;
    property PopupExplorerDropDown: TPopupExplorerDropDown read FPopupExplorerDropDown;
    property PopupExplorerOptions: TPopupExplorerOptions read FPopupExplorerOptions write FPopupExplorerOptions;
    property PopupExplorerTree: TPopupExplorerTree read GetPopupExplorerTree;
    property Style: TShellComboStyle read FStyle write SetStyle default scsDropDown;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
    property TextType: TExplorerComboboxText read FTextType write SetTextType default ecbtNameOnly;
    property ThemeButton: HTHEME read FThemeButton write FThemeButton;
    property ThemeCombo: HTHEME read FThemeCombo write FThemeCombo;
    property ThemeEdit: HTHEME read FThemeEdit write FThemeEdit;
    property ThemesActive: Boolean read FThemesActive write FThemesActive;
    property VETComboState: TVETComboStates read FVETComboState write FVETComboState;
    property VirtualExplorerTree: TCustomVirtualExplorerTree read FVirtualExplorerTree write SetVirtualExplorerTree;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ChangeLinkChanging(Server: TObject; NewPIDL: PItemIDList); dynamic; // ChangeLink method
    procedure ChangeLinkFreeing(ChangeLink: IVETChangeLink); dynamic;
    procedure Loaded; override;
    property ComboEdit: TComboEdit read GetComboEdit;
    property HotTracking: Boolean read FHotTracking;
  end;


  TVirtualExplorerCombobox = class(TCustomVirtualExplorerCombobox)
  public
    property AutoComplete;
    property EditNamespace;
    property ComboEdit;
    property PopupExplorerTree;
    property PopupAutoCompleteDropDown;
    property PopupExplorerDropDown;
    property VETComboState;
  published
    property Active;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Enabled;
    property Flat;
    property Font;
    property Height default 23;
    property Options;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Path;
    property PopupAutoCompleteOptions;
    property PopupExplorerOptions;
    property TextType;
    property Visible;
    property VirtualExplorerTree;

    property OnAutoCompleteAddPath;
    property OnAutoCompleteRollDown;
    property OnAutoCompleteRollUp;
    property OnAutoCompleteUpdateList;
    property OnChange;
    property OnComboRollDown;
    property OnComboRollUp;
    property OnDecodeSpecialVariable;
    property OnDraw;
    property OnEnter;
    property OnEnumFolder;
    property OnExit;
    property OnInvalidEntry;
    property OnPathChange;
    property OnPathChanging;
  end;

  TExplorerComboBox = class(TVirtualExplorerCombobox)
  end;

  TVirtualBkGndEnumThreadList = class
  private
    FList: TThreadList;
  protected
    property List: TThreadList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Thread: TCommonThread);
    procedure Flush;
    procedure Remove(Thread: TCommonThread);
  end;

  PTVirtualBackGndEnumThread = ^TVirtualBackGndEnumThread;
  TVirtualBackGndEnumThread = class(TCommonThread)
  private
    FEnumBkGndList: PCommonPIDLList;
    FEnumLock: TRTLCriticalSection;
    FEnumThread: PVirtualBackGndEnumThread;
    FHideRecyleBin: Boolean;
    FList: TCommonPIDLList;
    FParentPIDL: PItemIDList;
    FFileObjects: TFileObjects;
    FWndHandle: THandle;  // Is it ok this window is in a different thread? .. Apparently it is...
  protected
    procedure Execute; override;
    procedure FinalizeThread; override;
    procedure InitializeThread; override;
    property EnumBkGndList: PCommonPIDLList read FEnumBkGndList write FEnumBkGndList;
    property EnumLock: TRTLCriticalSection read FEnumLock write FEnumLock;
    property EnumThread: PVirtualBackGndEnumThread read FEnumThread write FEnumThread;
    property FileObject: TFileObjects read FFileObjects write FFileObjects;
    property HideRecycleBin: Boolean read FHideRecyleBin write FHideRecyleBin;
    property List: TCommonPIDLList read FList write FList;
    property ParentPIDL: PItemIDList read FParentPIDL write FParentPIDL;
    property WndHandle: THandle read FWndHandle write FWndHandle;
  public
    constructor Create(Suspended: Boolean; ForceHideRecycleBin: Boolean;
      AParentPIDL: PItemIDList; AFileObjects: TFileObjects; AWndHandle: THandle;
      AnEnumLock: TRTLCriticalSection; APIDLList: PCommonPIDLList;
      AnEnumThread: PVirtualBackGndEnumThread); reintroduce; virtual;
    destructor Destroy; override;
  end;

  procedure ReadFolder(Folder: IShellFolder; Flags: DWORD; var APIDLArray: TPIDLArray; Sorted: Boolean; var PIDLsRead: Integer);

var
  NodeCount: integer = 0;
  VETChangeDispatch: TVETChangeDispatch = nil;
  ViewManager: TGlobalViewManager = nil;
  BkGndEnumThreadList: TVirtualBkGndEnumThreadList = nil;
  VETChangeObjects: integer = 0;
  TestCount: integer = 0;


implementation

uses
  TypInfo,
  {$IFDEF SpTBX}
  ColumnFormSpTBX,
  {$ELSE}
    {$IFDEF TBX}
    ColumnFormTBX,
    {$ELSE}
      {$IFDEF TNTSUPPORT}
      ColumnFormTNT,
      {$ELSE}
      ColumnForm,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  Imm;

 {$R VirtualExplorerTreeExt.res}


type
  TAnimateWindow = function(hWnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; stdcall;
  
var
  // Set if any VET has set the wait cursor.  If one VET set the cursor to the hourglass
  // a second may save that as the old cursor.  When done the second maybe the last
  // to finish and reset the the cursor back to the old cursor, in this case the hourglass
  // the first set!  This is my first time wanting a class variable!
  GlobalWaitCursorSet: Boolean;
  // The expand marks can be very time consuming so they have their own thread
  ExpandMarkThread: TCommonThreadManager;


function ExpandMarkThreadManager: TCommonThreadManager;
begin
  if not Assigned(ExpandMarkThread) then
  begin
    ExpandMarkThread := TCommonThreadManager.Create(nil);
    ExpandMarkThread.Thread.Priority := tpLower;
    ExpandMarkThread.Enabled := True;
  end;
  Result := ExpandMarkThread
end;

procedure NodeNamespaceQuickSort(NodeArray: TNodeSearchArray;
  const ParentFolder: IShellFolder; L, R: Integer);
///
/// NOTE:  Make sure any changes to this method are reflected in both VirtualExplorerTree.pas
//         and VirtualExplorerListview.pas
///
var
  I, J: Integer;
  P, T: TNodeSearchRec;
begin
  if L < R then
  repeat
    I := L;
    J := R;
    P := NodeArray[(L + R) shr 1];
    repeat
      while ShortInt(ParentFolder.CompareIDs(0,
        NodeArray[I].NS.RelativePIDL, P.NS.RelativePIDL))< 0 do
        Inc(I);
      while ShortInt(ParentFolder.CompareIDs(0,
        NodeArray[J].NS.RelativePIDL, P.NS.RelativePIDL)) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := NodeArray[I];
        NodeArray[I] := NodeArray[J];
        NodeArray[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      NodeNamespaceQuickSort(NodeArray, ParentFolder, L, J);
    L := I;
  until I >= R;
end;

function NodeListBinarySearch(Target: PItemIDList; List: TNodeStorageList;
  const ParentFolder: IShellFolder; Min, Max: Longint) : Longint;
var
  Middle : LongInt;
  CompareResult: ShortInt;
begin
  // During the search the target's index will be between
  // min and max: min <= target index <= max
  while (Min <= Max) do
  begin
    Middle := (Min + Max) shr 1;
    CompareResult := ShortInt(ParentFolder.CompareIDs(0, List[Middle].RelativePIDL, Target));
    if CompareResult = 0 then
    begin
      Result := Middle;
      exit;
    end else
    begin
      if CompareResult > 0 then
        // Search the left half.
        Max := Middle - 1
      else
        // Search the right half.
        Min := Middle + 1;
    end
  end;

  // If we get here the target is not in the list.
  Result := -1;
end;

function NodeListInsertPt(Target: PItemIDList; List: TNodeStorageList;
  const ParentFolder: IShellFolder; Min, Max: Longint): integer;
{ IMPORTANT................                                                     }
{ This function assumes that the List is already sorted.                        }
var
  Middle : LongInt;
  CompareResult: ShortInt;
begin
  // During the search the target's index will be between
  // min and max: min <= target index <= max
  while (Min <= Max) do
  begin
    Middle := (Min + Max) shr 1;
    CompareResult := ShortInt(ParentFolder.CompareIDs(0, List[Middle].RelativePIDL, Target));
    if CompareResult = 0 then
      raise Exception.Create('Error in NodeListBinarySearch. Node unexpectedly found')
    else begin
      if CompareResult > 0 then
        // Search the left half.
        Max := Middle - 1
      else
        // Search the right half.
        Min := Middle + 1;
    end
  end;

  if Min > Max then
    Result := Min
  else
    Result := Max
end;

procedure ReadFolder(Folder: IShellFolder; Flags: DWORD; var APIDLArray: TPIDLArray; Sorted: Boolean; var PIDLsRead: Integer);
var
  EnumIDList: IEnumIDList;
  ArrayLength: Integer;
  pceltFetched: LongWord;
  TempPIDL: PItemIDList;
  OldWow64: Pointer;
begin          
  if Assigned(Folder) then
  begin
    OldWow64 := Wow64RedirectDisable;
    try
      PIDLsRead := 0;
      { Make sure we have some room }
      if Length(APIDLArray) = 0 then
        SetLength(APIDLArray, DEFAULTPIDLARRAYSIZE); // Thats a lot of files!
      ArrayLength := Length(APIDLArray);

      { Enumerate the target folder. Note: The new items have been created by   }
      { the time we get notified so they all should be found.                   }
      if Folder.EnumObjects(0, Flags, EnumIDList) = NOERROR  then
      begin
        while (EnumIDList.Next(1, TempPIDL, pceltFetched) = NOERROR) do
        begin
          APIDLArray[PIDLsRead] := TempPIDL;
          Inc(PIDLsRead);
          { Grow the array if necessary }
          if PIDLsRead >= ArrayLength then
          begin
            SetLength(APIDLArray, ArrayLength  + DEFAULTPIDLARRAYSIZE);
            ArrayLength := Length(APIDLArray);
          end
        end;
        SetLength(APIDLArray, PIDLsRead);
        if Sorted and (PIDLsRead > 0) then
          PIDLQuickSort(APIDLArray, Folder, 0, PIDLsRead - 1);
      end;
    finally
      Wow64RedirectRevert(OldWow64);
    end   
  end else
    PIDLsRead := 0;
end;

{ TCustomVirtualExplorerTree }

function TCustomVirtualExplorerTree.BuildEnumFlags: DWORD;
begin
  Result := 0;
  if foFolders in FileObjects then
    Result := Result or SHCONTF_FOLDERS;
  if foNonFolders in FileObjects then
    Result := Result or SHCONTF_NONFOLDERS;
  if foHidden in FileObjects then
    Result := Result or SHCONTF_INCLUDEHIDDEN;
  if IsUnicode and not IsWinNT4 then
  begin
    if foShareable in FileObjects then
      Result := Result or SHCONTF_SHAREABLE;
    if foNetworkPrinters in FileObjects then
      Result := Result or SHCONTF_NETPRINTERSRCH;
  end;
end;

function TCustomVirtualExplorerTree.CanShowDragImage: Boolean;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
    Result := False
  else
    Result := inherited CanShowDragImage;
end;

function TCustomVirtualExplorerTree.GetAnimateWndParent: TWinControl;
begin
  Result := Self
end;

function TCustomVirtualExplorerTree.GetContextMenuShown: Boolean;
begin
  Result := ContextMenuManager.MenuShown
end;

function TCustomVirtualExplorerTree.GetOkToShellNotifyDispatch: Boolean;
begin
  if Assigned(ContextMenuManager) then
    Result := not(IsAnyEditing or Dragging or ShellNotifySuspended or ContextMenuManager.MenuShown)
  else
    Result := not(IsAnyEditing or Dragging or ShellNotifySuspended);
end;

function TCustomVirtualExplorerTree.GetSelectedPIDL: PItemIDList;
var
  NS: TNamespace;
begin
  Result := nil;
  if ValidateNamespace(GetFirstSelected, NS) then
    if NS.FileSystem then
      Result := PIDLMgr.CopyPIDL( NS.AbsolutePIDL)
end;

function TCustomVirtualExplorerTree.GetThreadedExpandMarkEnabled: Boolean;
begin
  Result := FThreadedExpandMarkEnabled
end;

function TCustomVirtualExplorerTree.NextSelectedNode(Node: PVirtualNode; DoSelectNext: Boolean; var ResultIsParent: Boolean): PVirtualNode;
//
// Selects the next node in line for selection if the passed node is being deleted, etc
//
var
  NS: TNamespace;
  Found: Boolean;
begin
  Result := nil;
  ResultIsParent := False;
  if (SelectedCount = 1) and Selected[Node] then
  begin
    Result := Node.PrevSibling;
    if Assigned(Result) then
    begin
      Found := False;
      while (Result <> nil) and not Found do
      begin
        if ValidateNamespace(Result, NS) then
          Found := NS.Valid;
        if not Found then
          Result := Result.PrevSibling;
      end
    end else
    begin
      Result := Node.NextSibling;
      if Assigned(Result) then
      begin
        Found := False;
        while (Result <> nil) and not Found do
        begin
          if ValidateNamespace(Result, NS) then
            Found := NS.Valid;
          if not Found then
            Result := Result.NextSibling;
        end
      end else
      begin
        Found := False;
        Result := Node.Parent;
        while (Result <> nil) and not Found do
        begin
          if ValidateNamespace(Result, NS) then
            Found := NS.Valid;
          if not Found then
            Result := Result.Parent;
        end;
        ResultIsParent := Assigned(Result)
      end;
    end;

    if DoSelectNext then
    begin
      FocusedNode := nil;
      ClearSelection;
      Selected[Result] := True;
      FocusedNode := Result
    end;
  end
end;

function TCustomVirtualExplorerTree.ShowBkGndContextMenu(Point: TPoint): Integer;
begin
  Result := 0; // Do the default
end;

procedure TCustomVirtualExplorerTree.ActivateTree(Activate: Boolean);
begin
  if not (csLoading in ComponentState) then
  begin
    if Activate and not FActivated then
    begin
      if Assigned(FRootFolderNamespace) then
        RebuildRootNamespace;
      FActivated := True
    end else
    if not Activate and FActivated then
    begin
      FActivated := False;
      Clear;
    end
  end else
    if (csDesigning in ComponentState) then
      RebuildRootNamespace;
end;

function TCustomVirtualExplorerTree.AddCustomNode(ParentNode: PVirtualNode;
  CustomNamespace: TNamespace; UsesCheckBoxes: Boolean; CheckBoxType: TCheckType = ctTriStateCheckBox): PVirtualNode;
var
  NewNodeData: PNodeData;
begin
  NewNodeData := nil;
  Result := AddNodeToTree(ParentNode);
  if Assigned(Result) then
    NewNodeData := InternalData(Result);
  if Assigned(NewNodeData) then
    NewNodeData.Namespace := CustomNamespace;
  if UsesCheckBoxes then
  begin
    CheckType[Result] := CheckBoxType;
    CheckState[Result] := csUncheckedNormal
  end;
  DoNamespaceStructureChange(Result, NewNodeData.Namespace, nscAdd);
end;

procedure TCustomVirtualExplorerTree.AddMyDocumentsFolder(
  FolderNode: PVirtualNode; DesktopFolderOnly: Boolean);
var
  MyDocuments, NS: TNamespace;
  Desktop: IShellFolder;
  LS: WideString;
  chEaten, Attrib: ULONG;
  PIDL: PItemIDList;
  Child: PVirtualNode;
  Duplicate, AddMyDocuments: Boolean;
begin
  Duplicate := False;
  if ValidateNamespace(FolderNode, NS) then
  begin
    AddMyDocuments := (DesktopFolderOnly and NS.IsDesktop) or not DesktopFolderOnly;
    if AddMyDocuments then
    begin

      if NS.Folder then
      begin
        LS := '::{450d8fba-ad25-11d0-98a8-0800361b1103}';
        SHGetDesktopFolder(Desktop);
        Attrib := 0;
        if Desktop.ParseDisplayName(0, nil, PWideChar(LS), chEaten, PIDL,
          Attrib) = S_OK
        then begin
          MyDocuments := TNamespace.Create(PIDL, nil);
          Child := GetFirstChild(FolderNode);
          while not Duplicate and Assigned(Child) do
          begin
            if ValidateNamespace(Child, NS) then
            begin
              Duplicate := ILIsEqual(MyDocuments.AbsolutePIDL, NS.AbsolutePIDL) = True;
              Child := GetNextSibling(Child);
            end
          end;
          if not Duplicate then
            AddCustomNode(FolderNode, MyDocuments, toCheckSupport in TreeOptions.MiscOptions)
          else
            MyDocuments.Free
        end
      end
    end
  end
end;

function TCustomVirtualExplorerTree.AddNodeToTree(
  ParentNode: PVirtualNode): PVirtualNode;
begin
  Result := AddChild(ParentNode);
 { Fixes a hard to track problem when restoring checks from a previous session }
//  if toCheckSupport in TreeOptions.MiscOptions then
//    CheckType[Result] := ctTriStateCheckBox;
//    Result.CheckType := ctTriStateCheckBox; // Tell VT what we did it so siblings are right when expanded
end;

procedure TCustomVirtualExplorerTree.AfterValidEnumIDList(Sender: TObject);
var
  i: Integer;
begin
  // Called from TNamespace.EnumerateFolderEx
// reset to the last UpdateCount
  for i := 0 to CurrentUpdateCount - 1 do
    BeginUpdate;
end;

procedure TCustomVirtualExplorerTree.Assign(Source: TPersistent);
var
  VET: TCustomVirtualExplorerTree;
begin
  inherited;
  if Source is TCustomVirtualExplorerTree then
  begin
    VET := TCustomVirtualExplorerTree(Source);
    ColumnDetails := VET.ColumnDetails;
    ColumnMenuItemCount := VET.ColumnMenuItemCount;
    FileObjects := VET.FileObjects;
    FileSizeFormat := VET.FileSizeFormat;
    FileSort := VET.FileSort;
    if VET.RootFolderCustomPath <> '' then
      RootFolderCustomPath := VET.RootFolderCustomPath
    else
    if Assigned(VET.RootFolderCustomPIDL) then
      RootFolderCustomPIDL := VET.RootFolderCustomPIDL
    else
      RootFolder := VET.RootFolder;
    TreeOptions.Assign(VET.TreeOptions);
    VETColors.Assign(VET.VETColors);
    Active := VET.Active;
  end
end;

procedure TCustomVirtualExplorerTree.AssignTo(Dest: TPersistent);
var
  VET: TCustomVirtualExplorerTree;
begin
  inherited;
  if Dest is TCustomVirtualExplorerTree then
  begin
    VET := TCustomVirtualExplorerTree(Dest);
    VET.ColumnDetails := ColumnDetails;
    VET.ColumnMenuItemCount := ColumnMenuItemCount;
    VET.FileObjects := FileObjects;
    VET.FileSizeFormat := FileSizeFormat;
    VET.FileSort := FileSort;
    if RootFolderCustomPath <> '' then
      VET.RootFolderCustomPath := RootFolderCustomPath
    else
    if Assigned(RootFolderCustomPIDL) then
      VET.RootFolderCustomPIDL := RootFolderCustomPIDL
    else
      VET.RootFolder := RootFolder;
    VET.TreeOptions.Assign(TreeOptions);
    VET.VETColors.Assign(VETColors);
    VET.Active := Active;
  end
end;

function TCustomVirtualExplorerTree.BrowseTo(APath: WideString; ExpandTarget,
  SelectTarget, SetFocusToVET, CollapseAllFirst: Boolean): Boolean;
var
  PIDL: PItemIdList;
begin
  Result := False;
  PIDL := PathToPIDL(APath);
  try
    if Assigned(PIDL) then
      Result := BrowseToByPIDL(PIDL, ExpandTarget, SelectTarget, SetFocusToVET, CollapseAllFirst);
  finally
    PIDLMgr.FreePIDL(PIDL);
  end
end;

function TCustomVirtualExplorerTree.BrowseTo(APath: WideString;
  SetFocusToVET: Boolean): Boolean;
// Browse to path using the SyncOptions of the Tree as defaults
var
  Op: TVETSyncOptions;
begin
  Op := TreeOptions.VETSyncOptions;
  Result := BrowseTo(APath, toExpandTarget in Op, toSelectTarget in Op,
            SetFocusToVET, toCollapseTargetFirst in Op);
end;

function TCustomVirtualExplorerTree.BrowseToByPIDL(APIDL: PItemIDList;
  ExpandTarget, SelectTarget, SetFocusToVET, CollapseAllFirst: Boolean;
  ShowAllSiblings: Boolean = True): Boolean;
var
  TargetNode: PVirtualNode;
begin
  if OkToBrowseTo(APIDL) then
  begin
    Include(FVETState, vsBrowsing);
    // Stop the Notifier from firing during all the interm manipulations
    EnableChangeNotifier(False);
    try
      TargetNode := nil;
      BeginUpdate;
      try
        Result := False;
        TargetNode := nil;
        if Assigned(APIDL) then
          TargetNode := WalkPIDLToNode(APIDL, False, True, False, ShowAllSiblings);
        if Assigned(TargetNode) then
        begin
          if CollapseAllFirst then
            FullCollapse;
          if ExpandTarget then
            Expanded[TargetNode] := ExpandTarget;
          if SetFocusToVET and CanFocus then
            SetFocus;
          Result := True
        end;
      finally
        if SelectTarget and Assigned(TargetNode) then
        begin
          FocusedNode := nil;
          ClearSelection;  
          Selected[TargetNode] := True;
          FocusedNode := TargetNode;
        end;
        EndUpdate;
        // MUST do this after the EndUpdate or VT does not update its internal view rectangle
        if Assigned(TargetNode) then
          ScrollIntoView(TargetNode, False, toAutoScrollHorz in TreeOptions.VETMiscOptions);
        Exclude(FVETState, vsBrowsing);
      end
    finally
      EnableChangeNotifier(True)
    end
  end else
    Result := False
end;


procedure TCustomVirtualExplorerTree.ChangeLinkChanging(Server: TObject; NewPIDL: PItemIDList);
{ This method is called when ever we have installed a VETChangeLink to another  }
{ Control.  When the other control changes its selection or root it will send   }
{ this notification.                                                            }
var
  NS: TNamespace;
  DoBrowse: Boolean;
  Desktop: IShellFolder;
begin
  { Keep from recursively trying to respond to a notify if more than one        }
  { control has been registered with this instance as the client. Once is       }
  { enough and necessary.  VT can get out of wack if you try to call selection  }
  { methods recursively.                                                        }
  if not(vsNotifyChanging in VETState) then
  begin
    Include(FVETState, vsNotifyChanging);
    try
      if Assigned(NewPIDL) and not(csDesigning in ComponentState) then
      begin
      {$IFDEF EXPLORERLISTVIEW_L}
        if Self is TVirtualExplorerListview then
          ValidateNamespace(RootNode, NS)
        else
      {$ENDIF}
          ValidateNamespace(GetFirstSelected, NS);

        DoBrowse := True;
        if Assigned(NS) then
        begin
          SHGetDesktopFolder(Desktop);
          DoBrowse := ShortInt(Desktop.CompareIDs(0, NS.AbsolutePIDL, NewPIDL)) <> 0;
        end;
        if DoBrowse then
          BrowseToByPIDL(NewPIDL,
                       toExpandTarget in TreeOptions.VETSyncOptions,
                       toSelectTarget in TreeOptions.VETSyncOptions,
                       False,
                       toCollapseTargetFirst in TreeOptions.VETSyncOptions);
      end;
    finally
      Exclude(FVETState, vsNotifyChanging);
    end
  end
end;

procedure TCustomVirtualExplorerTree.ChangeLinkDispatch;
var
  NS: TNamespace;
  Node: PVirtualNode;
  s: string;
begin
  s := Name;
  if not(vsLockChangeNotifier in VETState) then
  begin
    if Assigned(VETChangeDispatch) and (not (tsDrawSelecting in TreeStates)) then
    begin
      Node := GetFirstSelected;
      if not Assigned(Node) and (SelectedCount = 0) then
        Node := FocusedNode;
      if ValidateNamespace(Node, NS) then
        VETChangeDispatch.DispatchChange(Self, NS.AbsolutePIDL)
    end
  end
end;

procedure TCustomVirtualExplorerTree.ChangeLinkFreeing(ChangeLink: IVETChangeLink);
begin
  if ChangeLink.ChangeLinkClient = Self then
  begin
  {$IFDEF EXPLORERTREE_L}
    if ChangeLink.ChangeLinkServer = FVirtualExplorerTree then
      FVirtualExplorerTree := nil
    else
  {$ENDIF}
  {$IFDEF EXPLORERCOMBOBOX_L}
    if ChangeLink.ChangeLinkServer = FExplorerComboBox then
      FExplorerComboBox := nil;
  {$ENDIF}
  end;
end;

procedure TCustomVirtualExplorerTree.Clear;
const
  ClipboardStates = [tsCopyPending, tsCutPending];
begin
  // Do not clear the clipboard when changing views
  TreeStates := TreeStates - ClipboardStates;
  inherited
end;

procedure TCustomVirtualExplorerTree.CollapseNamespaceFolder(Node: PVirtualNode);
{ If any selected nodes are in the collapsing tree then unselect them all and   }
{ select and focus the collapsed node.                                          }
var
  SelNode: PVirtualNode;
begin
  if Assigned(Node) {and not (toAutoExpand in TreeOptions.AutoOptions)} then
  begin
    // First Check to see if the selection is within the collapsing branch
    // if it is then we need to do something about it
    SelNode := GetFirstSelected;
    if HasAsParent(SelNode, Node) then
    begin
      // This use to cause a recursion problem
 //     if not (toAutoExpand in TreeOptions.AutoOptions) then
 //     begin
 //       ClearSelection;
 //        // This use to cause a recursion problem
 //       Selected[Node] := True;
 //     end else
 //     ClearSelection;
      FocusedNode := nil;
      ClearSelection;
      Selected[Node] := True;
      FocusedNode := Node
    end;
  end
end;

procedure TCustomVirtualExplorerTree.CopyToClipBoard;
var
  Node: PVirtualNode;
  NS: TNamespace;
  Handled: Boolean;
begin
  WaitCursor(True);
  try
    Handled := False;
    DoClipboardCopy(Handled);
    if not Handled then
    begin
      Node := GetFirstSelected;
      if Assigned(Node) then
        if ValidateNamespace(Node, NS) then
          if NS.Copy(Self, SelectedToNamespaceArray) then
            MarkNodesCopied
    end
  finally
    WaitCursor(False)
  end
end;

constructor TCustomVirtualExplorerTree.Create(AOwner: TComponent);
var
  CF: VirtualTrees.TClipboardFormats;
begin
  inherited;
  InitializeCriticalSection(FEnumLock);
  ContextMenuManager := TContextMenuManager.Create(Self);
  ShellNotifyManager.RegisterExplorerWnd(Self);
  ControlState := ControlState + [csCreating];
  Include(FVETState, vsCreateNotificationLock);
  EnableChangeNotifier(False);  // Will Unlock after Loaded is done
  Active := False;
  ColumnMenu := TColumnMenu.Create(Self);
  ColumnMenuItemCount := 8;
  AutoFireChangeLink := True;
  FShellNotifyQueue := TList.Create;
  RootFolder := rfDesktop;
  OnDragOver := DummyOnDragOver;
  FFileObjects := [foFolders];
  DefaultNodeHeight := 17;
  Header.Height := 17;
  DragHeight := 250;
  DragWidth := 150;
  HintMode := hmHint;
  FViewManager := TViewManager.Create;
  FVETPersistent := TVETPersistent.Create;
  VETColors := TVETColors.Create(Self);
  {$IFDEF TNTSUPPORT}
  FSelectedPaths := TTntStringList.Create;
  FSelectedFiles := TtntStringList.Create;
  {$ELSE}
  FSelectedPaths := TStringList.Create;
  FSelectedFiles := TStringList.Create;
  {$ENDIF}

  { Remove any weird clipboard formats.  The IDataObject will handle that.      }
  { Still need the virtual tree internal formats though.                        }
  CF := VirtualTrees.TClipboardFormats.Create(Self);
  CF.Add(CFSTR_VIRTUALTREE);
  CF.Add(CFSTR_VTREFERENCE);
  ClipboardFormats := CF;
  CF.Free;
  UnknownFolderIconIndex := DefaultSystemImageIndex(diNormalFolder);
  UnknownFileIconIndex := DefaultSystemImageIndex(diUnknownFile);
  { Hold off numerous Rebuilds until CreateWnd is called }
  RebuildRootNamespaceBeginUpdate;
  LoadDefaultOptions;
  RebuildRootNamespaceEndUpdate;
  FInternalDataOffset := AllocateInternalDataArea( SizeOf(TNodeData));
  FSortHelper := TShellSortHelper.Create;
  ControlState := ControlState - [csCreating];
  SHGetMalloc(FMalloc);
  GlobalThreadManager.RegisterControl(Self);
end;

function TCustomVirtualExplorerTree.CreateNewFolder(TargetPath: WideString): Boolean;
{ Creates a new folder.  Note you do NOT pass the name of the new folder, only pass the }
{ the path up to where the new folder is to be created.  VET will then create the new   }
{ folder like Explorer does, in the "New Folder (X)" fashion (this constant is redefinable }
{ in the VETResources.pas file).  The folder will be created and immediatly selected so    }
{ the user may edit it.                                                                    }
var
  PIDL: PItemIDList;
begin
  PIDL := PathToPIDL(TargetPath);
  Result := InternalCreateNewFolder(PIDL, '') <> '';
  PIDLMgr.FreePIDL(PIDL);
end;

function TCustomVirtualExplorerTree.CreateNewFolder(TargetPath: WideString; var NewFolder: WideString): Boolean;
var
  PIDL: PItemIDList;
begin
  PIDL := PathToPIDL(TargetPath);
  NewFolder := InternalCreateNewFolder(PIDL, '');
  PIDLMgr.FreePIDL(PIDL);
  Result := NewFolder <> ''
end;

function TCustomVirtualExplorerTree.CreateNewFolder(TargetPath,
  SuggestedFolderName: WideString; var NewFolder: WideString): Boolean;
var
  PIDL: PItemIDList;
begin
  PIDL := PathToPIDL(TargetPath);
  NewFolder := InternalCreateNewFolder(PIDL, SuggestedFolderName);
  PIDLMgr.FreePIDL(PIDL);
  Result := NewFolder <> ''
end;

function TCustomVirtualExplorerTree.CreateNewFolderByNode(Node: PVirtualNode): Boolean;
var
  NS: TNamespace;
begin
  Result := False;
  if ValidateNamespace(Node, NS) then
    Result := InternalCreateNewFolder(NS.AbsolutePIDL, '') <> ''
end;

procedure TCustomVirtualExplorerTree.CreateWnd;
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  inherited;
  ThreadedImagesEnabled := toThreadedImages in TreeOptions.VETImageOptions;
  ChangeNotifierEnabled := toChangeNotifierThread in TreeOptions.VETMiscOptions;
  ThreadedExpandMarkEnabled := toThreadedExpandMark in TreeOptions.VETFolderOptions;

  RebuildHeader(FRootFolderNamespace);
  // If the window is recreated it is possible some of the threaded icons where
  // already cached and then lost during the DestroyWnd so we need to force a
  // refresh of the tree.  This is because the VCL does not destroy the
  // TWinControl object is stores an image of it to a Memory stream then reads
  // it back.
  Node := GetFirst;
  while Assigned(Node) do
  begin
    if ValidateNamespace(Node, NS) then
      NS.InvalidateNamespace(True);
    Node := GetNextNoInit(Node)
  end;
  if vsCreateNotificationLock in VETState then
  begin
    Exclude(FVETState, vsCreateNotificationLock);
    EnableChangeNotifier(True);  // Cleared in constructor
  end
end;

procedure TCustomVirtualExplorerTree.CutToClipBoard;
var
  Node: PVirtualNode;
  NS: TNamespace;
  Handled, Mark: Boolean;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Handled := False;
      Mark := True;
      DoClipboardCut(Mark, Handled);
      if not Handled then
      begin
        Node := GetFirstSelected;
        if Assigned(Node) then
          if ValidateNamespace(Node, NS) then
            if NS.Cut(Self, SelectedToNamespaceArray) and (toMarkCutAndCopy in TreeOptions.VETImageOptions) then
              MarkNodesCut;
      end else
        if Mark then
          MarkNodesCut;
    finally
      WaitCursor(False)
    end
  end
end;

procedure TCustomVirtualExplorerTree.DefineProperties(Filer: TFiler);
begin
//  Filer.DefineProperty('ExplorerComboBox', LoadExplorerComboBox, nil, False);
  inherited;
end;

procedure TCustomVirtualExplorerTree.DeleteNode(Node: PVirtualNode; Reindex: Boolean = True);
//var
//  NS: TNamespace;
begin
  { Selects the nodes parent if it a folder being deleted.  The main reason     }
  { for this is to force the ExplorerTreeview to change the view of the         }
  { ExplorerListview if the Folder selected is deleted leaving the contents of  }
  { the folder left if the Listview after the folder is deleted.                }
    //Change the focus if the deleted node is focused

  // Improved by Robert 11.26.02
  // Selecting parent is not a good idea on the listview. This works
  // more like Explorer
  if (Node <> nil) and (Node = FocusedNode) then
  begin
    //Focus the next sibling or the parent
    if Node.NextSibling <> nil then
      FocusedNode := Node.NextSibling
    else
      FocusedNode := Node.Parent;
    //Select the focused node
    if FocusedNode <> nil then
      Selected[FocusedNode] := true;
  end;
  inherited DeleteNode(Node, Reindex);

{  if Selected[Node] and ValidateNamespace(Node, NS) then
  begin
    if NS.Folder then
    begin
      ClearSelection;
      FocusedNode := nil;
      if Node.Parent <> RootNode then
      begin
        Selected[Node.Parent] := True;
        FocusedNode := Node.Parent
      end
    end
  end;
  inherited DeleteNode(Node, Reindex);    }
end;

procedure TCustomVirtualExplorerTree.DeleteNodeByPIDL(PIDL: PItemIDList);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  BeginUpdate;
  try
    Node := WalkPIDLToNode(PIDL, False, False, False, True);
    if ValidateNamespace(Node, NS) then
    begin
      if toCheckSupport in TreeOptions.MiscOptions then
        if ValidateNamespace(Node, NS) then
          Storage.Delete(NS.AbsolutePIDL, [], True);
      DeleteNode(Node)
    end;
  finally
    EndUpdate
  end
end;

procedure TCustomVirtualExplorerTree.DeleteSelectedNodes(ShiftKeyState: TExecuteVerbShift = evsCurrent);
begin
  SelectedFilesDelete(ShiftKeyState)
end;

destructor TCustomVirtualExplorerTree.Destroy;
begin
  // Clear the Tree in case the app accesses any of the helper objects
  ShellNotifyManager.UnRegisterExplorerWnd(Self);
  FreeAndNil(FEnumBkGndList);
  ThreadedImagesEnabled := False;
  ChangeNotifierEnabled := False;
  ThreadedExpandMarkEnabled := False;
  Clear; // Must be after threaded are killed
  // Support Halt( );
  if Assigned(PIDLMgr) then
    PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);
  FreeAndNil(FRootFolderNamespace);
  FreeAndNil(FVETPersistent);
  FreeAndNil(FViewManager);
  FreeAndNil(FVETColors);
  FreeAndNil(FColumnMenu);
  FreeAndNil(FContextMenuManager);
  FreeAndNil(FSelectedPaths);
  FreeAndNil(FSelectedFiles);
  FreeAndNil(FShellNotifyQueue);
  { In case we were using the hidden root node with toHideRootNode }
  FreeAndNil(PNodeData(InternalData(RootNode))^.Namespace);
  // Support Halt( );
  if Assigned(VETChangeDispatch) then
    VETChangeDispatch.UnRegisterChangeLink(Self, Self, utAll);
  FSortHelper.Free;
  Malloc := nil;
  GlobalThreadManager.UnRegisterControl(Self);
  inherited;
  // Has to be after inherited where the window handle is destroyed
  DeleteCriticalSection(FEnumLock);
end;

procedure TCustomVirtualExplorerTree.DestroyWnd;
begin
  inherited;
end;

function TCustomVirtualExplorerTree.DoBeforeDrag(Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  NS: TNamespace;
begin
  Result := False;
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    with TreeOptions do
      if (toDragDrop in VETShellOptions) and (toAcceptOLEDrop in MiscOptions) then
      begin
        if ValidateNamespace(Node, NS) then
        begin
          if not NS.IsDesktop then
          begin
            Result := True;
            if Assigned(OnDragAllowed) then
              OnDragAllowed(Self, Node, Column, Result);
            if Result then
              DragOperations := [doCopy, doMove, doLink]; // The namespace will sort this out
          end
        end
      end
  end
end;

procedure TCustomVirtualExplorerTree.DoCanEdit(Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  VETColumn: TVETColumn;
begin
  // If no columns assume we are showing files
  if Allowed and (Column > -1) then
  begin
    VETColumn := TVETColumn(Header.Columns[Column]);
    // if ShellColumns only allow an edit on a column greater than the shell supplied columns
    // and if that column is cdCustom or if the column is the default, -1, or the first 0
    // which in cdShellColumn mode will always be the file name
    if ColumnDetails = cdShellColumns then
      Allowed := (Column >= ShellBaseColumnCount) and (VETColumn.ColumnDetails = cdCustom) or (Column < 1)
    else
    // if any other Column type then only allow editing on a Custom column type or
    // if the column contains the filename
      Allowed := (VETColumn.ColumnDetails = cdCustom) or (VETColumn.ColumnDetails = cdFileName);
  end;
  if toVETReadOnly in TreeOptions.VETMiscOptions then
    Allowed := False;
  // Let application also define what is editable or not
  inherited;
end;

procedure TCustomVirtualExplorerTree.DoChange(Node: PVirtualNode);
begin
  if not ContextMenuManager.MenuPending and Assigned(Node) then
    ChangeLinkDispatch;
  // This will likely break some apps.  I was trapping nil node events
  // so they were never passed to the event.  My mistake.
  inherited;
end;

procedure TCustomVirtualExplorerTree.DoChecked(Node: PVirtualNode);
var
  NS: TNamespace;
  StorageNode: TNodeStorage;
begin
  if ValidateNamespace(Node, NS) then
  begin
    if Node.CheckState <> csUncheckedNormal then
    begin
      StorageNode := Storage.Store(NS.AbsolutePIDL, [stChecks]);
      if Assigned(StorageNode) then
      begin
        StorageNode.Storage.Check.CheckState := Node.CheckState;
        StorageNode.Storage.Check.CheckType := Node.CheckType;
      end
    end else
    begin
      { Clear the Checks data from the node }
      Storage.Delete(NS.AbsolutePIDL, [stChecks]);
    end
  end;
  inherited;
end;

procedure TCustomVirtualExplorerTree.DoClipboardCopy(var Handled: Boolean);
begin
  if Assigned(FOnClipboardCopy) then
    OnClipboardCopy(Self, Handled);
end;

procedure TCustomVirtualExplorerTree.DoClipboardCut(
  var MarkSelectedCut: Boolean; var Handled: Boolean);
begin
  if Assigned(FOnClipboardCut) then
    OnClipboardCut(Self, MarkSelectedCut, Handled);
end;

procedure TCustomVirtualExplorerTree.DoClipboardPaste(
  var Handled: Boolean);
begin
  if Assigned(FOnClipboardPaste) then
    OnClipboardPaste(Self, Handled);
end;

procedure TCustomVirtualExplorerTree.DoCollapsed(Node: PVirtualNode);
begin
  inherited;
  { Is a Thread Safe Call, BrowseTo can call a full collapse cutting off the images}
  if ThreadedImagesEnabled and not (vsBrowsing in VETState) then
    GlobalThreadManager.FlushMessageCache(Self, TID_ICON);
  if ThreadedExpandMarkEnabled and not (vsBrowsing in VETState) then
    ExpandMarkThreadManager.FlushMessageCache(Self, TID_EXPANDMARK);
end;

function TCustomVirtualExplorerTree.DoCollapsing(Node: PVirtualNode): Boolean;
begin
  Result := inherited DoCollapsing(Node);
  CollapseNamespaceFolder(Node)
end;

procedure TCustomVirtualExplorerTree.DoColumnResize(Column: TColumnIndex);
var
  PrevCode: UINT;
begin
  PrevCode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    inherited;
    if not CreatingHeaders then
     StoreColumnState;
  finally
    SetErrorMode(PrevCode);
  end
end;

function TCustomVirtualExplorerTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
{ Called by VT for each child node when a parent node is expanded in order to   }
{ sort them.                                                                    }
var
  NS1, NS2: TNamespace;
begin
  Result := 0;
  if toUserSort in TreeOptions.VETMiscOptions then
    Result := inherited DoCompare(Node1, Node2, Column)
  else begin
    SortHelper.FileSort := FileSort;
    if Assigned(Node1) and Assigned(Node2) then
    begin
      NS1 := PNodeData( InternalData(Node1)).Namespace;
      NS2 := PNodeData( InternalData(Node2)).Namespace;
      if Assigned(NS1) and Assigned(NS2) then
      begin
        if Column > -1 then
        begin
          if not (ColumnDetails = cdShellColumns) then
          begin
            case TVETColumn(Header.Columns[Column]).ColumnDetails of
              cdFileName:   Result := SortHelper.CompareIDSort(0, NS1, NS2);
              cdSize:       Result := SortHelper.SortFileSize(NS1, NS2);
              cdType:       Result := SortHelper.SortType(NS1, NS2);
              cdModified:   Result := SortHelper.SortFileTime(NS1.LastWriteTimeRaw, NS2.LastWriteTimeRaw, NS1, NS2);
              cdAccessed:   Result := SortHelper.SortFileTime(NS1.LastAccessTimeRaw, NS2.LastAccessTimeRaw, NS1, NS2);
              cdCreated:    Result := SortHelper.SortFileTime(NS1.CreationTimeRaw, NS2.CreationTimeRaw, NS1, NS2);
              cdAttributes: Result := SortHelper.SortString(NS1.AttributesString, NS2.AttributesString, NS1, NS2);
              cdPath:       Result := SortHelper.SortString(NS1.NameParseAddress, NS2.NameParseAddress, NS1, NS2);
              cdDOSName:    Result := SortHelper.SortString(NS1.ShortFileName, NS2.ShortFileName, NS1, NS2);
              cdCustom:     DoCustomColumnCompare(Column, Node1, Node2, Result);
            end;
          end else
          begin
            { If the columns were created through the interfaces then the index   }
            { can be assumed to be valid.                                         }
            if Assigned(NS1.Parent.ShellDetailsInterface) or
               Assigned(NS1.Parent.ShellFolder2)
            then
            begin
              if (Column = 2) and (FileSort = fsFileExtension) then
                Result := SortHelper.SortType(NS1, NS2)
              else
              if Column > ShellBaseColumnCount - 1 then
                DoCustomColumnCompare(Column, Node1, Node2, Result)
              else
                Result := SortHelper.CompareIDSort(Column, NS1, NS2)
            end else
            begin
              { The namespace did not support Details so VET must be showing the  }
              { default columns so sort them based on that assumption.            }
              case Column of
                -1: Result := SortHelper.CompareIDSort(0, NS1, NS2);
                 0: Result := SortHelper.CompareIDSort(0, NS1, NS2);
                 1: Result := SortHelper.SortFileSize(NS1, NS2);
                 2: Result := SortHelper.SortType(NS1, NS2);
                 3: Result := SortHelper.SortFileTime(NS1.LastWriteTimeRaw, NS2.LastWriteTimeRaw, NS1, NS2);
                else
                  Result := SortHelper.CompareIDSort(0, NS1, NS2);
              end;
            end
          end
        end else
          Result := SortHelper.CompareIDSort(0, NS1, NS2);
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.DoColumnUserChangedVisibility;
begin
  if Assigned(OnColumnUserChangedVisiblility) then
     OnColumnUserChangedVisiblility(Self)
end;

procedure TCustomVirtualExplorerTree.DoContextMenuAfterCmd(
  Namespace: TNamespace; Verb: WideString; MenuItemID: Integer;
  Successful: Boolean);
var
  AVerb: WideString;
begin
  if Assigned(OnContextMenuAfterCmd) then
    OnContextMenuAfterCmd(Self, Namespace, Verb, MenuItemID, Successful);
  if Successful then
  begin
    AVerb := WideLowerCase(PWideChar( Verb));
    if AVerb = 'cut' then
      MarkNodesCut;
    if AVerb = 'copy' then
      MarkNodesCopied;
  end
end;

function TCustomVirtualExplorerTree.DoContextMenuCmd(
  Namespace: TNamespace; Verb: WideString; MenuItemID: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnContextMenuCmd) then
    OnContextMenuCmd(Self, Namespace, Verb, MenuItemID, Result);
      { Rename is a special case and MUST be handled by the application.            }
  if (Verb = 'rename') and not Result then
  begin
    ContextMenuManager.IsEditingNode := True;
    Result := True;
    if SelectedCount = 1 then
      if Header.Columns.Count = 0 then
        EditNode(GetFirstSelected, -1)
      else
        EditNode(GetFirstSelected, 0);
     ContextMenuManager.IsEditingNode := False;
  end;
end;

procedure TCustomVirtualExplorerTree.DoContextMenuSelect(
  Namespace: TNamespace; MenuItemID: Integer; SubMenuID: hMenu;
  MouseSelect: Boolean);
begin
  if Assigned(OnContextMenuItemChange) then
    OnContextMenuItemChange(Self, Namespace, MenuItemID, SubMenuID, MouseSelect);
end;

function TCustomVirtualExplorerTree.DoContextMenuShow(
  Namespace: TNamespace; Menu: hMenu): Boolean;
begin
  Result := True;
  if Assigned(OnContextMenuShow) then
    OnContextMenuShow(Self, Namespace, Menu, Result);
end;

function TCustomVirtualExplorerTree.DoCreateDataObject: IDataObject;
var
  NS: TNamespace;
  Node: PVirtualNode;
  DataObjectObj: TVETDataObject;
begin
  Result := inherited DoCreateDataObject;
  if not Assigned(Result) then
  begin
    Node := GetFirstSelected;
    if ValidateNamespace(Node, NS) then
    begin
      DataObjectObj := TVETDataObject.Create(Self, False);
      Result := DataObjectObj as IDataObject;
      DataObjectObj.ShellDataObject := NS.DataObjectMulti(SelectedToNamespaceArray);
    end
  end
end;

procedure TCustomVirtualExplorerTree.DoCustomColumnCompare(Column: TColumnIndex;
  Node1, Node2: PVirtualNode; var Result: integer);
begin
  if Assigned(OnCustomColumnCompare) then
    OnCustomColumnCompare(Self, Column, Node1, Node2, Result);
end;

procedure TCustomVirtualExplorerTree.DoCustomNamespace(AParentNode: PVirtualNode);
begin
  if (toForceShowMyDocuments in TreeOptions.VETFolderOptions) then
    AddMyDocumentsFolder(AParentNode, True);
  if Assigned(OnCustomNamespace) then
    OnCustomNamespace(Self, AParentNode)
end;

procedure TCustomVirtualExplorerTree.DoEndDrag(Target: TObject; X,
  Y: Integer);
begin
  inherited;
  Invalidate;
  Update
end;

function TCustomVirtualExplorerTree.DoEndEdit: Boolean;
var
  Msg: TMsg;
  NS: TNamespace;
  RePostQuitCode: Integer;
  RePostQuit: Boolean;
begin
  // Refresh the cached information to reflect the edited name
  ValidateNamespace(FocusedNode, NS); 
  if Assigned(NS) then 
    NS.InvalidateNamespace;
  // This allows Windows to send its change notifications and for VET to receive
  // them.  Since we are still in edit mode the WM_SHELLNOTIFY will not update
  // the listview so the newly created node will stay on the bottom and not
  // get sorted.
  Sleep(AFTEREDITDELAY);

  RePostQuit := False;
  RePostQuitCode := 0;
  // Pluck out any notification messages
  while PeekMessage(Msg, Handle, WM_SHELLNOTIFY, WM_SHELLNOTIFY, PM_REMOVE) do
  begin
    if Msg.Message = WM_QUIT then
    begin
      RePostQuit := True;
      RePostQuitCode := Msg.WParam
    end else
    begin
      // Still dispatch them eventhough the handler will ignore them
      TranslateMessage(Msg);
      DispatchMessage(Msg)
    end;
    if RePostQuit then
      PostQuitMessage(RePostQuitCode)
  end;
  Result := inherited DoEndEdit;
end;

procedure TCustomVirtualExplorerTree.DoEnumFolder(
  const Namespace: TNamespace; var AllowAsChild: Boolean);
begin
  if Assigned(OnEnumFolder) then
    OnEnumFolder(Self, Namespace, AllowAsChild);
end;

function TCustomVirtualExplorerTree.DoExpanding(Node: PVirtualNode): Boolean;
begin
  // Only allow if user wants to browse as default execute to a dbl click or Enter
  // or if the node is the First (allows a root node and one expanded level)
  // and if the inhertied is successful
  Result := ((toBrowseExecuteFolder in TreeOptions.VETMiscOptions) or
             (Node = GetFirst) or ExpandingByButtonClick) and
             inherited DoExpanding(Node);
end;

procedure TCustomVirtualExplorerTree.DoEnumThreadLengthyOperation(var ShowAnimation: Boolean);
begin
  ShowAnimation := True;
  if Assigned(OnEnumThreadLengthyOperation) then
    OnEnumThreadLengthyOperation(Self, ShowAnimation)
end;

procedure TCustomVirtualExplorerTree.DoFreeNode(Node: PVirtualNode);
{ Called by VT when the node is being deleted.                                  }
var
  NewNodeData: PNodeData;
  NS: TNamespace;
  CM: TColumnManager;
begin
  NS := nil;
  CM := nil;
  if ThreadedImagesEnabled then
    GlobalThreadManager.FlushMessageCache(Self, TID_ICON);
  if ThreadedExpandMarkEnabled then
    ExpandMarkThreadManager.FlushMessageCache(Self, TID_EXPANDMARK);
  if Assigned(Node) then
  begin
    NewNodeData := InternalData(Node);
    if Assigned(NewNodeData) then
    begin
      NS := NewNodeData.Namespace;
      CM := NewNodeData.ColumnManager;
    end
  end;
  inherited;
  // Keep the TNamespace and TColumnManager valid in the FreeNode Event
  if Assigned(NS) then
    NS.Free;
  if Assigned(CM) then
    CM.Free;
end;

function TCustomVirtualExplorerTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList;
{ Called by VT when it needs the image index for the node.                       }
var
  NS: TNamespace;
  Request: TShellIconThreadRequest;
begin
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);

  if ValidateNamespace(Node, NS) then
  begin
    if not Assigned(Result) and (Column = Header.MainColumn) then
    begin
      if not NS.ThreadedIconLoaded and ThreadedImagesEnabled
        and not (csDesigning in ComponentState)
      then begin
        if (Kind = ikNormal) or (Kind = ikSelected) then
        begin
          if not NS.ThreadIconLoading then
          begin
            NS.ThreadIconLoading := True;
            Request := TShellIconThreadRequest.Create;
            Request.ID := TID_ICON;
            Request.PIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
            Request.Window := Self;
            Request.Item := Node;
            Request.Large := toLargeImages in TreeOptions.VETImageOptions;
            Request.Open := Expanded[Node];
            GlobalThreadManager.AddRequest(Request, True);
          end;
          if NS.IconCache > -1 then
            Index := NS.IconCache
          else begin
            if NS.Folder and NS.FileSystem then
              Index := UnknownFolderIconIndex
            else
              Index := UnknownFileIconIndex
          end
        end else
          Index := -1
      end else
      begin
        Ghosted := NS.Ghosted and not(toDisableGhostedFolders in TreeOptions.VETFolderOptions);
        if Kind = ikOverlay then
        begin
          if not (toHideOverlay in TreeOptions.VETImageOptions) and Assigned(NS.ShellIconOverlayInterface) then
            Index := NS.OverlayIndex - 1
          else begin
            if NS.Link then
              Index := 1
            else
            if NS.Share then
              Index := 0
          end
        end else
        if Kind = ikNormal then
          Index := NS.GetIconIndex(Expanded[Node], icSmall, False)
        else
        if Kind = ikSelected then
          Index := NS.GetIconIndex(
            (toShowOpenIconOnSelect in TreeOptions.VETFolderOptions) or
            (Expanded[Node]), icSmall, False);
      end;
    end;
  end
end;

function TCustomVirtualExplorerTree.DoGetNodeHint(Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  NS: TNamespace;
begin
  if toShellHints in TreeOptions.VETShellOptions then
  begin
    if ValidateNamespace(Node, NS) then
      Result := NS.InfoTip
  end else
    Result := inherited DoGetNodeHint(Node, Column, LineBreakStyle);
end;

function TCustomVirtualExplorerTree.DoGetPopupMenu(Node: PVirtualNode;
  Column: TColumnIndex; Position: TPoint): TPopupMenu;
var
  HitInfo: THitInfo;
begin
  Result := inherited DoGetPopupMenu(Node, Column, Position);
  // if ContextMenu is on then don't allow a per node menu when clicking on the item
  // because it will popup the Context Menu rather then the user defined menu
  if (toContextMenus in TreeOptions.VETShellOptions) then
  begin
    GetHitTestInfoAt(Position.X, Position.Y, True, HitInfo);
    if  HitInfo.HitPositions * [hiOnStateIcon, hiOnNormalIcon, hiOnItemLabel] <> [] then
      Result := nil
  end;
  if Assigned(Result) then
    Result.PopupComponent := Self
end;

procedure TCustomVirtualExplorerTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
// 7.2.02 changed method so that any columns text may be changed via the
// OnGetVETText event.  This is a questionable practice in my opinion but it has
// been asked for a lot.  The danger is you don't always know what column is the
// column you are after, all you get is the column number and you have to assume
// you know what column it is. The caption will be language dependant
var
  NS: TNamespace;
  VETColumn: TVETColumn;
begin     
  if ValidateNamespace(Node, NS) then
  begin
    if Column > -1 then
    begin
      VETColumn := TVETColumn( Header.Columns[Column]);
      if ColumnDetails = cdShellColumns then
      begin
        if Column < ShellBaseColumnCount then
        begin
          { The IShellDetails and IShellFolder2 interfaces show // in front of    }
          { network resources and use things like Win_ObjectNew for the           }
          { 'Add Printer' text so use the usual way to get the object name.       }
          if Column < 1 then
            Text := NS.NameInFolder
          else
            Text := NS.DetailsOf(Column);
        end;
        // Allow the program to override any column text
        DoGetVETText(Column, Node, NS, Text);
      end else
      begin
        case VETColumn.ColumnDetails of
          cdFileName: Text := NS.NameInFolder;
          cdSize:
            case FileSizeFormat of
              fsfExplorer: Text := NS.SizeOfFileKB;
              fsfActual: Text := NS.SizeOfFile;
              fsfDiskUsage: Text := NS.SizeOfFileDiskUsage;
            end;
          cdType: Text := NS.FileType;
          cdModified: Text := NS.LastWriteTime;
          cdAccessed: Text := NS.LastAccessTime;
          cdCreated: Text := NS.CreationTime;
          cdAttributes: Text := NS.AttributesString;
          cdPath: Text := NS.NameParseAddress;
          cdDOSName: Text := NS.ShortFileName;
         // cdCustom: DoGetVETText(Column, Node, NS, Text);
        end;
        // Allow the program to override any column text
        DoGetVETText(Column, Node, NS, Text);
      end
    end else
    begin
      Text := NS.NameInFolder;
       // When the VET has no columns the Column param is -1 we should fire the event
      DoGetVETText(Column, Node, NS, Text)
    end
  end else
  begin
    Text := '';
    DoGetVETText(Column, Node, nil, Text);
  end
end;

procedure TCustomVirtualExplorerTree.DoGetVETText(Column: TColumnIndex; Node: PVirtualNode;
 Namespace: TNamespace; var Text: UnicodeString);
begin
  if Assigned(OnGetVETText) then
    OnGetVETText(Self, Column, Node, Namespace, Text)
end;

procedure TCustomVirtualExplorerTree.DoInvalidRootNamespace;
begin
  if Assigned(OnInvalidRootNamespace) then
    OnInvalidRootNamespace(Self)
end;

procedure TCustomVirtualExplorerTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
var
  Node: PVirtualNode;
  CM: TColumnManager;
begin
  inherited;
  if HitInfo.Button <> mbRight then
  begin
    Node := GetFirst;
    WaitCursor(True);
    try
      if GetKeyState(VK_CONTROL) and $8000 <> 0 then
      begin
        if (HitInfo.Column > NoColumn) and ValidateColumnManager(Node, CM) then
          CM.ToggleWidthAutoFit(HitInfo.Column);
      end else
      if GetKeyState(VK_SHIFT) and $8000 <> 0 then
      begin
        if (HitInfo.Column > NoColumn) and ValidateColumnManager(Node, CM) then
          CM.ToggleWidthMinimize(HitInfo.Column);
      end else
      begin
        if HitInfo.Column > NoColumn then
        begin
          if Header.SortColumn <> HitInfo.Column then
            Header.SortColumn := HitInfo.Column
          else begin
            if Header.SortDirection = sdAscending then
              Header.SortDirection := sdDescending
            else
              Header.SortDirection := sdAscending
          end;
          SortTree(HitInfo.Column, Header.SortDirection, False);
        end
      end
    finally
      WaitCursor(False)
    end
  end
end;

procedure TCustomVirtualExplorerTree.DoHeaderMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  inherited;
  { Let user defined menu have priority }
  if not Assigned(Header.PopupMenu) and (Button = mbRight) and
    (toShellColumnMenu in TreeOptions.VETShellOptions)
  then begin
    Pt.x := X;
    Pt.y := Y;
    Pt := ClientToScreen(Pt);
    ColumnMenu.PopupComponent := Self;
    ColumnMenu.Popup(Pt.X, Pt.Y - Integer(Header.Height));
  end;
  if (hsDragging in Header.States) then
    StoreColumnState
end;

procedure TCustomVirtualExplorerTree.DoHeaderRebuild;
begin
  if Assigned(OnHeaderRebuild) then OnHeaderRebuild(Self, Header);
end;

procedure TCustomVirtualExplorerTree.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  ChildNode: PVirtualNode;
begin
  ChildCount := ExpandNamespaceFolder(Node);

  // Unfortunately it is necessary to force all the nodes to be initialized
  // in order to ensure that the check state is save for nodes that have never
  // been scrolled into view (hence never initialized)
  if toCheckSupport in TreeOptions.MiscOptions then
  begin
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      InitNode(ChildNode);
      ChildNode := ChildNode.NextSibling
    end
  end;
  // This allows uses to override the count in case custom namespace are being used
  inherited;
end;

procedure TCustomVirtualExplorerTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
{ Called by VT when the node needs to be initialized in order to display it or  }
{ if it has been forced to be initialized when saving to the tree, etc.         }
var
  Data: PNodeData;
{  WS: WideString; }
  ParentNode: PVirtualNode;
  NS: TNamespace;
  StorageNode: TNodeStorage;
  Request: TExpandMarkThreadRequest;
 { DriveType: UINT;    }
begin
  Data := InternalData(Node);
  if Assigned(Data) then
  begin
    if not Assigned(Parent) and (RootNodeCount < 2) and
      not(toHideRootFolder in TreeOptions.VETFolderOptions)
    then begin
      if Assigned(Data.Namespace) then
        FreeAndNil(Data.Namespace);
      if Assigned(Data.ColumnManager) then
        FreeAndNil(Data.ColumnManager);
      { If it is the root node create the desktop namespace }
      if Assigned(FRootFolderNamespace) then
        Data.Namespace := FRootFolderNamespace.Clone(True)
      else
        Data.Namespace := TNamespace.Create(nil, nil);
      Data.ColumnManager := TColumnManager.Create(Self);
      Include(InitStates, ivsExpanded);
    end;

    if Assigned(Data.Namespace) then
    begin
      // Namespace.Valid will pump on the disk drive so don't use it if
      // the disk is removable.

      if ThreadedExpandMarkEnabled then
      begin
        // PIDL is not valid until the mapped drive is logged into
        if {not Data.Namespace.Browsable and} not Data.Namespace.Removable {and Data.Namespace.Folder} and not Data.Namespace.Link then // Vista Websites on MSDN is annoying and show a log in dialog, it is a Folder and a Link....
        begin
          Include(InitStates, ivsHasChildren);
          //  In Vista all "+" are threaded
          Request := TExpandMarkThreadRequest.Create;
          Request.ID := TID_EXPANDMARK;
          Request.Window := Self;
          Request.PIDL := PIDLMgr.CopyPIDL(Data.Namespace.AbsolutePIDL);
          Request.EnumFlags := BuildEnumFlags;
          Request.Priority := 1; // Icons are most important
          Request.Item := Node;
          ExpandMarkThreadManager.AddRequest(Request, True)
        end else
          Include(InitStates, ivsHasChildren);
      end else
      begin
        if Data.Namespace.Removable and (toFoldersExpandable in TreeOptions.VETFolderOptions) then
        begin
          if Data.Namespace.HasSubFolder then
            Include(InitStates, ivsHasChildren)
          else
          if foNonFolders in FileObjects then
          begin
            if Data.Namespace.SubItemsEx then
              Include(InitStates, ivsHasChildren);
          end
        end else
        begin
          if (ItemHasChildren(Data.Namespace, Node, Parent) and (toFoldersExpandable in TreeOptions.VETFolderOptions)) then
            Include(InitStates, ivsHasChildren);
        end
      end
    end
  end;
  inherited;

  { Persistent checkbox support. Do it after inhertied in case the Application  }
  { manually changed the check state.                                           }
  if (toCheckSupport in TreeOptions.MiscOptions) then
  begin
    // This is to fix a bug with VT 3.04 not checking newly initialized nodes when
    // they are expanded. Only effects the last initialized level.
    if (toAutoTristateTracking in TreeOptions.AutoOptions) and Assigned(Parent) and (Parent <> RootNode) then
      if CheckState[Parent] = csCheckedNormal then
      begin
        Node.CheckState := csCheckedNormal;
        if ValidateNamespace(Node, NS) then
        begin
          StorageNode := Storage.Store(NS.AbsolutePIDL, [stChecks]);
          StorageNode.Storage.Check.CheckState := Node.CheckState;
          // Next step is already done
          Exit
        end;
      end;

    { See if there is a stored state for the checkbox  }
    if ValidateNamespace(Node, NS) then
    begin
       StorageNode := Storage.Find(NS.AbsolutePIDL, [stChecks]);
       if Assigned(StorageNode) then
       begin
         Node.CheckState := StorageNode.Storage.Check.CheckState;
         Node.CheckType := StorageNode.Storage.Check.CheckType;
         ParentNode := Node.Parent;
         if Assigned(ParentNode) and (toAutoTristateTracking in TreeOptions.AutoOptions) then
           if ParentNode.CheckState = csUncheckedNormal then
           while Assigned(ParentNode) do
           begin
             ParentNode.CheckState := csMixedNormal;
             ParentNode := ParentNode.Parent
           end
       end;
    end
  end
end;

function TCustomVirtualExplorerTree.DoKeyAction(var CharCode: Word;
  var Shift: TShiftState): Boolean;

    function FindLastDisplayed: PVirtualNode;
    var
      i: Cardinal;
    begin
      Result := TopNode;
      if Assigned(Result) then
      begin
        i := NodeHeight[Result];
        while Assigned(Result) and (i < Cardinal(ClientHeight)) do
        begin
          i := i + NodeHeight[Result];
          Result := Result.NextSibling
        end;
        if not Assigned(Result) then
          Result := TopNode.Parent.LastChild
        else
        if Result <> Result.Parent.LastChild then
          Result := Result.PrevSibling
      end
    end;

var
  Node: PVirtualNode;
  OldOffset: Integer;
begin
  Result := inherited DoKeyAction(CharCode, Shift);
  if Result then
    case CharCode of
      VK_F5: RefreshTree(toRestoreTopNodeOnRefresh in TreeOptions.VETMiscOptions);
      VK_F3: RefreshNode(GetFirstSelected);
      VK_DELETE: DeleteSelectedNodes;
      VK_SPACE:
        if Assigned(FocusedNode) and (Shift * [ssShift, ssAlt] = []) then
          if Selected[FocusedNode] then
            Selected[FocusedNode] := not (ssCtrl in Shift)
          else
            Selected[FocusedNode] := True;
      Ord('A'), Ord('a'):
        if (ssShift in Shift) and (ssCtrl in Shift) then
        begin
          InvertSelection(False);
          Result := False // VT default is select all with Shift-A
        end;
      Ord('C'), Ord('c'):
        begin
          // Look for Ctrl + 'C' Copy
          if (ssCtrl in Shift) then
            CopyToClipboard
        end;
      Ord('X'), Ord('x'):
        begin
          { Look for Ctrl + 'X' cut }
          if ssCtrl in Shift then
            CutToClipboard;
        end;
      Ord('V'), Ord('v'):
        begin
          { Look for Ctrl + 'V' paste }
          if ssCtrl in Shift then
            PasteFromClipboard;
        end;
      VK_DIVIDE:
        begin
          WaitCursor(True);
          try
            if Assigned(FocusedNode) then
               FullCollapse(FocusedNode);
            CharCode := 0; // We handled this key
          finally
              WaitCursor(False)
          end
        end;
      VK_INSERT:
      begin
        if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
        begin
          // Lefties favorate keys!
          if ssShift in Shift then
            PasteFromClipboard
          else
          if ssCtrl in Shift then
            CopyToClipboard;
        end;
        Result := False
      end;
      VK_UP:
      begin
        if ssCtrl in Shift then
        begin
          if not Assigned(FocusedNode) then
            FocusedNode := GetFirst
          else begin
            if Assigned(FocusedNode) then
              if Assigned(FocusedNode.PrevSibling) then
                FocusedNode := FocusedNode.PrevSibling;
          end;
          Result := False
        end
      end;
      VK_DOWN:
      begin
        if ssCtrl in Shift then
        begin
          if not Assigned(FocusedNode) then
            FocusedNode := GetFirst
          else begin
            if Assigned(FocusedNode) then
              if Assigned(FocusedNode.NextSibling) then
                FocusedNode := FocusedNode.NextSibling;
          end;
          Result := False
        end;
      end;
      VK_NEXT:
      begin
        if ssCtrl in Shift then
        begin
          if not Assigned(FocusedNode) then
            FocusedNode := GetFirst
          else begin
            if Assigned(FocusedNode) then
            begin
              Node := FindLastDisplayed;
              if (FocusedNode <> Node) and (FocusedNode <> GetFirst.Parent.LastChild) then
                FocusedNode := Node
              else begin
                OldOffset := OffsetY;
                OffsetY := OffsetY - ClientHeight;
                // This make sure the top is aligned with a node, looks better
                ScrollIntoView(TopNode, False, toAutoScrollHorz in TreeOptions.VETMiscOptions);
                if OldOffset = OffsetY then
                  FocusedNode := GetFirst.Parent.LastChild
                else
                  FocusedNode := FindLastDisplayed
              end
            end
          end;
          Result := False
        end;
      end;
      VK_PRIOR:
      begin
        if ssCtrl in Shift then
        begin
          if not Assigned(FocusedNode) then
            FocusedNode := GetFirst
          else begin
            if Assigned(FocusedNode) then
            begin
              Node := TopNode;
              if FocusedNode <> Node then
                FocusedNode := Node
              else begin
                OffsetY := OffsetY + ClientHeight;
                FocusedNode := TopNode
              end
            end
          end;
          Result := False
        end;
      end;
      VK_ESCAPE:
      begin
        CancelCutOrCopy;
        Invalidate;
      end;
    end;
end;

procedure TCustomVirtualExplorerTree.DoNewText(Node: PVirtualNode;
  Column: TColumnIndex; Text: UnicodeString);
var
  NS: TNamespace;
  VETColumn: TVETColumn;
  StorageNode: TNodeStorage;
begin
  if (tsEditPending in TreeStates) then
    beep;
  VETColumn := nil;
  // Moved the check to DoCanEdit.  If does not meet critera to be editable it
  // never makes it to here
  if Column > -1 then
    VETColumn := TVETColumn(Header.Columns[Column]);

  if (((ColumnDetails <> cdShellColumns) and (Assigned(VETColumn) and (VETColumn.ColumnDetails = cdCustom)) or
    ((ColumnDetails = cdShellColumns) and (Column >= ShellBaseColumnCount)))) then
    inherited
  else
  begin
    if ValidateNamespace(Node, NS) then
    begin
      StorageNode := Storage.Find(NS.AbsolutePIDL, [stUser, stChecks, stColumns]);
      if not NS.SetNameOf(Text) then
        PostMessage(Handle, WM_INVALIDFILENAME, Integer( Node), 0)
      else begin
        NS.InvalidateCache;
        // Update the storage with the new name
        if Assigned(StorageNode) then
        begin
          PIDLMgr.FreeAndNilPIDL(StorageNode.FAbsolutePIDL);
          StorageNode.FRelativePIDL := nil;;
          StorageNode.FShellFolder := nil;
          StorageNode.FAbsolutePIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
          StorageNode.FRelativePIDL := PIDLMgr.GetPointerToLastID(StorageNode.AbsolutePIDL);
        end;
        InvalidateNode(Node);
        inherited;
      end
    end
  end;
end;

procedure TCustomVirtualExplorerTree.DoPaintText(Node: PVirtualNode;
  const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var
  NS: TNamespace;
begin
  if ValidateNamespace(Node, NS) then
  begin
    if (Column < 1) and not((toHotTrack in TreeOptions.PaintOptions) and
    (Node = HotNode)) and not(toNoUseVETColorsProp in TreeOptions.VETFolderOptions) then
    begin
      if not(vsSelected in Node.States) and not(Node = DropTargetNode) then
      begin
        if (Column < 1) and NS.Compressed then
          Canvas.Font.Color := VETColors.CompressedTextColor
        else
        if (Column < 1) and NS.Encrypted then
          Canvas.Font.Color := VETColors.EncryptedTextColor
        else
        if NS.Folder then
          Canvas.Font.Color := VETColors.FolderTextColor
        else
          Canvas.Font.Color := VETColors.FileTextColor
      end
    end;
    if (Column >= 1) and NS.Compressed then
      Canvas.Font.Color := VETColors.CompressedTextColor
    else
    if (Column >= 1) and NS.Encrypted then
      Canvas.Font.Color := VETColors.EncryptedTextColor
  end;
  inherited;
end;

procedure TCustomVirtualExplorerTree.DoPopupMenu(Node: PVirtualNode;
  Column: TColumnIndex; Position: TPoint);
begin
  inherited
end;

procedure TCustomVirtualExplorerTree.DoRootChange;
begin
  if Assigned(OnRootChange) then
    OnRootChange(Self)
end;

procedure TCustomVirtualExplorerTree.DoRootChanging(const NewRoot: TRootFolder;
  Namespace: TNamespace; var Allow: Boolean);
begin
  if Assigned(OnRootChanging) then
    OnRootChanging(Self, NewRoot, FRootFolderNamespace, Namespace, Allow);
end;

procedure TCustomVirtualExplorerTree.DoRootRebuild;

// Fired when the Tree is rebuilt

begin
  if Assigned(OnRootRebuild) then
    OnRootRebuild(Self)
end;

procedure TCustomVirtualExplorerTree.DoShellExecute(Node: PVirtualNode);
{ Fired when the Namespace associated with Node is to be used as a ShellExecuteEx }
{ parameter.                                                                      }
var
  WorkingDir, CmdLineArgument: WideString;
  Allow: Boolean;
  NS: TNamespace;
begin
  if ValidateNamespace(Node, NS) and (toExecuteOnDblClk in TreeOptions.VETMiscOptions) then
  begin
    CmdLineArgument := '';
    WorkingDir := '';
    Allow := True;
    if Assigned(OnShellExecute) then
      OnShellExecute(Self, NS, WorkingDir, CmdLineArgument, Allow);
    if Allow then
    begin
      with TreeOptions do
      begin
        ExecuteNamespace(NS, WorkingDir, CmdLineArgument);
        if NS.Link and (toBrowseExecuteFolderShortcut in VETMiscOptions) then
          ShellExecuteFolderLink(NS, WorkingDir, CmdLineArgument);
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.DoAfterShellNotify(ShellEvent: TVirtualShellEvent);
begin
  if Assigned(OnAfterShellNotify) then
    OnAfterShellNotify(Self, ShellEvent)
end;

procedure TCustomVirtualExplorerTree.DoShellNotify(ShellEvent: TVirtualShellEvent);
begin
  if Assigned(OnShellNotify) then
    OnShellNotify(Self, ShellEvent)
end;

procedure TCustomVirtualExplorerTree.DoTreeDblClick(Button: TMouseButton;
  Position: TPoint);
var
  x: integer;
begin
  if Assigned(OnTreeDblClick) then
    OnTreeDblClick(Self, GetNodeAt(Position.X, Position.Y, True, x), Button, Position);
end;

procedure TCustomVirtualExplorerTree.DoUpdating(State: TVTUpdateState);
begin
  inherited;
  { tsExpanding is for a full expand only }
  case State of
    usBegin:
      begin
        WaitCursor(True);
        EnableChangeNotifier(False);
      end;
    usEnd:
      begin
        WaitCursor(False);
        EnableChangeNotifier(True)
      end
  end
end;

function TCustomVirtualExplorerTree.DragDrop(const DataObject: IDataObject;
  KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult;
var
  NS: TNamespace;
  LocalDropTargetNode: PVirtualNode;
begin
  Result := S_OK;
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    FDropping := True;
    try
      WaitCursor(True);
      { Save a copy, inhertited DoDragDrop nil's the property                       }
      LocalDropTargetNode := DropTargetNode;

      { Have to call inhertied, it does a lot of cleanup of scroll and expand       }
      { timers and setting of tree states.                                          }
      Result := inherited DragDrop(DataObject, KeyState, Pt, Effect);

      if Effect <> DROPEFFECT_NONE then
      begin
       { Special case where there is nothing to drop on to put file in the folder!  }
        if toHideRootFolder in TreeOptions.VETFolderOptions then
          if LocalDropTargetNode = nil then
            LocalDropTargetNode := RootNode;

        if ValidateNamespace(LocalDropTargetNode, NS) then
        begin
          if DragMouseButton and MK_RBUTTON <> 0 then
            Effect := DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK
          else
            Effect := LastDragEffect; // We always get all DROPEFFECT constants for some reason?????
          Result := NS.Drop(DataObject, KeyState, Pt, Effect);
        end;
      end
    finally
      LastDropTargetNode := nil;
      FDropping := False;
      WaitCursor(False);
      DragDataObject := nil; // Bug Fix
    end
  end else
    Effect := DROPEFFECT_NONE
end;

function TCustomVirtualExplorerTree.DragEnter(KeyState: Integer; Pt: TPoint;
  var Effect: Integer): HResult;
{ Keep track of the current DropTargetNode and the last in order for DragEnter  }
{ and DragLeave notifictaions to be given to each IDragTarget inteface for the  }
{ namespaces as the cursor is dragged over them. Called by VT in response to    }
{ the OLE IDropTarger inteface.                                                 }
var
  NS: TNamespace;
  LocalDropTargetNode: PVirtualNode;
  SourceSuggestedDropEffect: integer;
begin
  Result := S_OK;
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    SourceSuggestedDropEffect := Effect;
    LocalDropTargetNode := DropTargetNode;
    if KeyState and MK_LBUTTON <> 0 then
      DragMouseButton := MK_LBUTTON
    else
    if KeyState and MK_RBUTTON <> 0 then
      DragMouseButton := MK_RBUTTON
    else begin
      Result := E_INVALIDARG;
      Exit
    end;
    DragDataObject := DragManager.DataObject;
    Result := inherited DragEnter(KeyState, Pt, Effect);

    if Effect <> DROPEFFECT_NONE then
    begin
      // Source knowns better what it wants (necessary for WinZip)
      Effect := SourceSuggestedDropEffect;

      { Special case where there is nothing to drop on to put file in the folder!  }
      if toHideRootFolder in TreeOptions.VETFolderOptions then
        if LocalDropTargetNode = nil then
          LocalDropTargetNode := RootNode;

      if LastDropTargetNode <> LocalDropTargetNode then
      begin
        if ValidateNamespace(LocalDropTargetNode, NS) then
          Result := NS.DragEnter(DragDataObject, KeyState, Pt, Effect);
        if ValidateNamespace(LastDropTargetNode, NS) then
            NS.DragLeave;
        LastDropTargetNode := LocalDropTargetNode
      end;
    end
  end else
    Effect := DROPEFFECT_NONE
end;

procedure TCustomVirtualExplorerTree.DragAndDrop(AllowedEffects: Integer;
  DataObject: IDataObject; DragEffect: Integer);
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
    SHDoDragDrop_MP(Handle, DataObject, nil, AllowedEffects, DragEffect)
  else
    inherited DragAndDrop(AllowedEffects, DataObject, DragEffect);
end;

procedure TCustomVirtualExplorerTree.DragLeave;
{ Called by VT in response to the OLE IDropTarger interface.                    }
var
  NS: TNamespace;
  LocalDropTargetNode: PVirtualNode;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    LocalDropTargetNode := DropTargetNode;
    inherited;

    { Special case where there is nothing to drop on to put file in the folder!  }
    if toHideRootFolder in TreeOptions.VETFolderOptions then
      if LocalDropTargetNode = nil then
        LocalDropTargetNode := RootNode;

    if ValidateNamespace(LocalDropTargetNode, NS)  then
      NS.DragLeave;
    LastDropTargetNode := nil;
    DragDataObject := nil; // Bug Fix
  end
end;

function TCustomVirtualExplorerTree.DragOver(Source: TObject;
  KeyState: Integer; DragState: TDragState; Pt: TPoint;
  var Effect: Integer): HResult;
{ Keep track of the current DropTargetNode and the last in order for DragEnter  }
{ and DragLeave notifictaions to be given to each IDragTarget inteface for the  }
{ namespaces as the cursor is dragged over them. Lastly execute the DragMove on }
{ the current node. Called by VT in response to the OLE IDropTarger inteface.   }
var
  NS: TNamespace;
  LocalDropTargetNode: PVirtualNode;
  SourceSuggestedDropEffect: integer;
begin
  Result := S_OK;
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    SourceSuggestedDropEffect := Effect;
    LocalDropTargetNode := DropTargetNode;

    Result := inherited DragOver(Source, KeyState, DragState, Pt, Effect);

    if Effect <> DROPEFFECT_NONE then
    begin
      // Source knowns better what it wants (necessary for WinZip)
      Effect := SourceSuggestedDropEffect;

      { VT calls DoDragOver in the Drop OLE call. This is not a good thing for the shell }
      if not Dropping then
      begin
          { Special case where there is nothing to drop on to put file in the folder!  }
        if toHideRootFolder in TreeOptions.VETFolderOptions then
          if LocalDropTargetNode = nil then
            LocalDropTargetNode := RootNode;

        if LastDropTargetNode <> LocalDropTargetNode then
        begin
          if ValidateNamespace(LocalDropTargetNode, NS) and Assigned(DragDataObject) then
            Result := NS.DragEnter(DragDataObject, KeyState, Pt, Effect);
          if ValidateNamespace(LastDropTargetNode, NS) then
            NS.DragLeave;
          LastDropTargetNode := LocalDropTargetNode
        end;

        if  ValidateNamespace(LocalDropTargetNode, NS) and Assigned(DragDataObject) then
          Result := NS.DragOver(KeyState, Pt, Effect)
        else
          Effect := DROPEFFECT_NONE;
        { For some reason when drop is called it recieves all DROPEFFECT constants  }
        { eventhough the IDropSource recieves the result of the DragMove just       }
        { before the drop occurs. Pass this to the DoDragDrop to handle it correctly}
        LastDragEffect := Effect;
      end;
    end
  end else
    Effect := DROPEFFECT_NONE
end;

procedure TCustomVirtualExplorerTree.DummyOnDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
{ Since the Namespaces decide if the drag is valid fake the VirtualTreeview     }
{ into believing we accept anything by creating an Event to assign to           }
{ OnDragOver                                                                    }
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
    Accept := True;
end;

function TCustomVirtualExplorerTree.EnumerateFolderCallback(MessageWnd: HWnd;
  APIDL: PItemIDList; AParent: TNamespace; Data: pointer; var Terminate: Boolean): Boolean;
{ This is what the TNamespace objects callback when they are enumerating a      }
{ folder.                                                                       }
var
  NewNodeData: PNodeData;
  Allow: Boolean;
  NS: TNamespace;
  Node: PVirtualNode;
begin
  Result := False;
  NS := TNamespace.Create(APIDL, AParent);
  if (toForceHideRecycleBin in TreeOptions.VETFolderOptions) and NS.IsRecycleBin then
  begin
    NS.Free;
    Exit
  end;
  Allow := True;
  DoEnumFolder(NS, Allow);
  if Allow then
  begin
    { Called from within ExpandNamespaceFolder so BeginUpdate/EndUpdate called }
    Node := AddNodeToTree(Data);
    NewNodeData := InternalData(Node);
    if Assigned(NewNodeData) then
    begin    
      NewNodeData.Namespace := NS;
      NewNodeData.ColumnManager := TColumnManager.Create(Self);
      Result := True
    end
  end else
    NS.Free
end;

function TCustomVirtualExplorerTree.ExpandNamespaceFolder(Node: PVirtualNode): integer;
var
  NS: TNamespace;
begin
  Result := 0;
  if OkToExpandNode(Node) then
  begin
    WaitCursor(True);
    try
      BeginUpdate;
      if ChildCount[Node] = 0 then
      begin
        if ValidateNamespace(Node, NS) then
        begin
          if ThreadedEnum and not (csDesigning in ComponentState) then
          begin
            TerminateEnumThread;
            HandleNeeded;
            EnumThreadStart;
            EnumThread := TVirtualBackGndEnumThread.Create(True,
              toForceHideRecycleBin in TreeOptions.VETFolderOptions,
              NS.AbsolutePIDL, FileObjects, Handle, EnumLock, @FEnumBkGndList,
              @FEnumThread);
            EnumThread.Priority := tpNormal;
            EnumThread.FreeOnTerminate := True;
            EnumThread.Suspended := False;
            EnumThreadTimer(True);
            Result := 0;
          end else
          begin
            // Let the tree redraw itself if a dialog is popped 
            // get reset when the NS callback in the AfterValidEnumIDList event
            CurrentUpdateCount := UpdateCount;
            while UpdateCount > 0 do
              EndUpdate;

            Result := NS.EnumerateFolderEx(Handle, FileObjects, EnumerateFolderCallback, Node, AfterValidEnumIDList);

            DoCustomNamespace(Node);
            { Sort calls InitChildren which can lead to endless recursion problems if }
            { there is no children.                                                   }
            if Node.ChildCount > 0 then
              Sort(Node, Header.SortColumn, Header.SortDirection, False);
            Result := ChildCount[Node];

            if ThreadedEnum and not (csDesigning in ComponentState) then
              EnumThreadFinished;
          end;
        end
      end
    finally 
      EndUpdate;
      WaitCursor(False)
    end
  end
end;

procedure TCustomVirtualExplorerTree.EnableChangeNotifier(Enable: Boolean);
begin
  if Enable then
  begin
  //  if not (csLoading in ComponentState) {and not(csCreating in ControlState) }then
    begin
      Dec(FChangeNotifierCount);
      if ChangeNotifierCount <= 0 then
      begin
        Exclude(FVETState, vsLockChangeNotifier);
        ChangeNotifierCount := 0;
        if AutoFireChangeLink then
          ChangeLinkDispatch;
      end
    end
  end else
  begin
    Include(FVETState, vsLockChangeNotifier);
    Inc(FChangeNotifierCount);
  end
end;

procedure TCustomVirtualExplorerTree.EnumThreadFinished;
begin
  Cursor := crDefault;
  if Assigned(OnEnumFinished) then OnEnumFinished(Self);
end;

procedure TCustomVirtualExplorerTree.EnumThreadStart;
begin
  Cursor := crHourglass;
end;

procedure TCustomVirtualExplorerTree.EnumThreadTimer(Enable: Boolean);
var
  Msg: TMsg;
  RePostQuitCode: Integer;
  RePostQuit: Boolean;
begin
  if HandleAllocated then
  begin
    if Enable then
    begin
      EnumTimer := SetTimer(Handle, ID_TIMER_ENUMBKGND, 50, nil);
      EnumBkGndTime := 0;
    end else
    begin
      if EnumTimer <> 0 then
        KillTimer(Handle, EnumTimer);
      EnumTimer := 0;

      RePostQuit := False;
      RePostQuitCode := 0;
      // Flush the cache of Timer Messages
      while PeekMessage(Msg, Handle, WM_TIMER, WM_TIMER, PM_REMOVE) do
      begin
        if Msg.Message = WM_QUIT then
        begin
          RePostQuit := True;
          RePostQuitCode := Msg.WParam
        end else
        if Msg.wParam <> ID_TIMER_ENUMBKGND then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg)
        end
      end;
      if RePostQuit then
        PostQuitMessage(RePostQuitCode)
    end
  end
end;

procedure TCustomVirtualExplorerTree.ExecuteNamespace(
  Namespace: TNamespace; var WorkingDir, CmdLineArgument: WideString);
// Excutes the Namespace and allows special cases to be handled
var
  ExecuteFolder: Boolean;
begin
  if Assigned(Namespace) then
    with TreeOptions do
    begin
      // ExecuteFolder is true only when the folder is NOT browsable
      ExecuteFolder := True;
      if (toBrowseExecuteFolder in VETMiscOptions) and Namespace.Folder then
        if WideStrIComp(PWideChar(Namespace.Extension), '.zip') = 0 then
          ExecuteFolder := not (toBrowseExecuteZipFolder in VETMiscOptions)
        else
          ExecuteFolder := False;

      Namespace.ShellExecuteNamespace(WorkingDir, CmdLineArgument,
        ExecuteFolder, not (toBrowseExecuteFolderShortcut in VETMiscOptions), MP_ThreadedShellExecute)
    end;
end;

function TCustomVirtualExplorerTree.FindDesktopFolderByName(
  AName: WideString; var Namespace: TNamespace): Boolean;
{ Finds a special folder by passing the localize name shown in the treeview.    }
{ It is limited to the immediate children of the Desktop and the immediate      }
{ children of My Computer to keep the search times reasonable.                  }
{ CAUTION:  Make sure you initialize the Namespace variable.                    }
var
  FindData: TFindSpecialFolderByNameData;
begin
  Result := False;
  AName := WideUpperCase(AName);
  if Assigned(Namespace) then
    FreeAndNil(Namespace);
  if WideStrIComp(PWideChar(AName), PWideChar(DesktopFolder.NameNormal)) = 0 then
  begin
    // Create a Desktop Folder
    Namespace := TNamespace.Create(nil, nil);
    Result := True
  end else
  begin
    FindData.Name := AName;
    FindData.PIDL := nil;
    // Don't show dialog messages
    DesktopFolder.EnumerateFolder(0, True, True, False, FindFolderByNameCallback, @FindData);
    if Assigned(FindData.PIDL) then
    begin
      Namespace := TNamespace.Create(FindData.PIDL, nil);
      Result := True
    end;
  end
end;

function TCustomVirtualExplorerTree.FindNode(APath: WideString): PVirtualNode;
var
  PIDL: PItemIdList;
begin
  PIDL := PathToPIDL(APath);
  Result := FindNodeByPIDL(PIDL);
  PIDLMgr.FreePIDL(PIDL);
end;

function TCustomVirtualExplorerTree.FindNodeByPIDL(APIDL: PItemIDList): PVirtualNode;
begin
  Result := WalkPIDLToNode(APIDL, False, False, False, True);
end;

function TCustomVirtualExplorerTree.FindFolderByName(AName: WideString; var Namespace: TNamespace): Boolean;
{ Finds a special folder by passing the localized name shown in the treeview.    }
{ It is limited to the immediate children of the Desktop and the immediate      }
{ children of My Computer to keep the search times reasonable.                  }
{ CAUTION:  Make sure you initialize the Namespace variable.                    }
var
  FindData: TFindSpecialFolderByNameData;
begin
  Result := False;
  if Assigned(Namespace) then
    FreeAndNil(Namespace);
  FindData.Name := AName;
  FindData.PIDL := nil;
  // Don't pop any dialogs
  DesktopFolder.EnumerateFolder(0, True, False, False, FindFolderByNameCallback, @FindData);
  if not Assigned(FindData.PIDL) then
    DrivesFolder.EnumerateFolder(0, True, False, False, FindFolderByNameCallback, @FindData);
  if Assigned(FindData.PIDL) then
  begin
    Namespace := TNamespace.Create(FindData.PIDL, nil);
    Result := True
  end;
end;

function TCustomVirtualExplorerTree.ForceNode(APath: WideString; Expand: Boolean): PVirtualNode;
var
  PIDL: PItemIdList;
begin
  PIDL := PathToPIDL(APath);
  Result := ForceNodeByPIDL(PIDL, Expand);
  PIDLMgr.FreePIDL(PIDL);
end;

function TCustomVirtualExplorerTree.ForceNodeByPIDL(APIDL: PItemIDList; Expand: Boolean;
 ShowAllSiblings: Boolean = True): PVirtualNode;
begin
  Result := WalkPIDLToNode(APIDL, Expand, True, not Expand, ShowAllSiblings);
end;

function TCustomVirtualExplorerTree.FindFolderByNameCallback(MessageWnd: HWnd; 
  APIDL: PItemIDList; AParent: TNamespace; Data: Pointer;
  var Terminate: Boolean): Boolean;
var
  FindData: PFindSpecialFolderByNameData;
  NS: TNamespace;
begin
  Result := True;
  FindData := PFindSpecialFolderByNameData(Data);
  NS := TNamespace.Create(PIDLMgr.CopyPIDL(APIDL), AParent);
  try
    Terminate := WideStrIComp(PWideChar(FindData.Name), PWideChar(NS.NameNormal)) = 0;
    if Terminate then
      FindData.PIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
  finally
    NS.Free
  end;
end;

procedure TCustomVirtualExplorerTree.FlushChangeNotifier;
begin
  FChangeNotifierCount := 0;
end;

procedure TCustomVirtualExplorerTree.ForceIconCachRebuild;
var
  Reg: TRegistry;
  LargeIconSize: integer;
begin
  if not (toNoRebuildIconListOnAssocChange in TreeOptions.VETMiscOptions) then
  begin
    Reg := TRegistry.Create;
    try
      try
        { This depends on the user having enough access rights under NT}
        {$ifdef COMPILER_5_UP}
        Reg.Access := KEY_READ or KEY_WRITE;
        {$endif}
        Reg.RootKey := HKEY_CURRENT_USER;
        if Reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False) then
        begin
          FlushImageLists;
          { Flush the Icon Cache by changing the size of the icons }
          if Reg.ValueExists('Shell Icon Size') then
            LargeIconSize := StrToInt(Reg.ReadString('Shell Icon Size'))
          else
            LargeIconSize := GetSystemMetrics(SM_CXICON);
          Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize + 1));
          SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
          FileIconInit(True); // Flush the cached Icons
          Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize));
          SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
          FileIconInit(True); // Flush the cached Icons
        end;
      except // Quiet failure
      end
    finally
      Reg.Free;
      RefreshTree(toRestoreTopNodeOnRefresh in TreeOptions.VETMiscOptions);
    end
  end
end;

function TCustomVirtualExplorerTree.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TVETColumn
end;

function TCustomVirtualExplorerTree.GetColumnMenu: TColumnMenu;
begin
  Result := FColumnMenu;
end;

function TCustomVirtualExplorerTree.GetHeaderClass: TVTHeaderClass;
begin
  Result := TVETHeader
end;

function TCustomVirtualExplorerTree.GetNewStorage: TRootNodeStorage;
begin
  Result := VETPersistent.Storage
end;

function TCustomVirtualExplorerTree.GetNodeDataSize: Integer;
begin
  Result := inherited NodeDataSize;
end;

function TCustomVirtualExplorerTree.GetOptions: TVirtualExplorerTreeOptions;
begin
  { Unsafe but faster }
  Result := TVirtualExplorerTreeOptions( inherited TreeOptions)
end;

function TCustomVirtualExplorerTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TVirtualExplorerTreeOptions;
end;

function TCustomVirtualExplorerTree.GetRecycleBinNode: PVirtualNode;
begin
  Result := nil;
  if Assigned(RecycleBinFolder) then
    Result := InternalWalkPIDLToNode(RecycleBinFolder.AbsolutePIDL);
end;

function TCustomVirtualExplorerTree.GetSelectedFile: WideString;
var
  NS: TNamespace;
begin
  Result := '';
  if ValidateNamespace(GetFirstSelected, NS) then
    if NS.FileSystem then
      Result := NS.NameParseAddressInFolder
end;

{$IFDEF TNTSUPPORT}
function TCustomVirtualExplorerTree.GetSelectedFiles: TWideStrings;
var
  NS: TNamespace;
  Node: PVirtualNode;
begin
  FSelectedFiles.Clear;
  Node := GetFirstSelected;
  while Assigned(Node) do
  begin
    if ValidateNamespace(Node, NS) then
      if NS.FileSystem then
        FSelectedFiles.Add(NS.NameParseAddressInFolder);
    Node := GetNextSelected(Node)
  end;
  Result := FSelectedFiles;
end;
{$ELSE}
function TCustomVirtualExplorerTree.GetSelectedFiles: TStrings;
var
  NS: TNamespace;
  Node: PVirtualNode;
begin
  FSelectedFiles.Clear;
  Node := GetFirstSelected;
  while Assigned(Node) do
  begin
    if ValidateNamespace(Node, NS) then
      if NS.FileSystem then
        FSelectedFiles.Add(NS.NameParseAddressInFolder);
    Node := GetNextSelected(Node)
  end;
  Result := FSelectedFiles;
end;
{$ENDIF}

function TCustomVirtualExplorerTree.GetSelectedPath: WideString;
var
  NS: TNamespace;
begin
  Result := '';
  if ValidateNamespace(GetFirstSelected, NS) then
    if NS.FileSystem then
      Result := NS.NameForParsing
end;


{$IFDEF TNTSUPPORT}
function TCustomVirtualExplorerTree.GetSelectedPaths: TWideStrings;
var
  NS: TNamespace;
  Node: PVirtualNode;
begin
  FSelectedPaths.Clear;
  Node := GetFirstSelected;
  while Assigned(Node) do
  begin
    if ValidateNamespace(Node, NS) then
      if NS.FileSystem then
        FSelectedPaths.Add(NS.NameForParsing);
    Node := GetNextSelected(Node)
  end;
  Result := FSelectedPaths;
end;
{$ELSE}
function TCustomVirtualExplorerTree.GetSelectedPaths: TStrings;
var
  NS: TNamespace;
  Node: PVirtualNode;
begin
  FSelectedPaths.Clear;
  Node := GetFirstSelected;
  while Assigned(Node) do
  begin
    if ValidateNamespace(Node, NS) then
      if NS.FileSystem then
        FSelectedPaths.Add(NS.NameForParsing);
    Node := GetNextSelected(Node)
  end;
  Result := FSelectedPaths;
end;
{$ENDIF}

function TCustomVirtualExplorerTree.HasPopupMenu(Node: PVirtualNode;
  Column: TColumnIndex; Pos: TPoint): Boolean;
var
  NS: TNamespace;
begin
  Result := inherited HasPopupMenu(Node, Column, Pos);
  { If it is a namespace node then it has a context menu }
  if not Result then
    Result := (toContextMenus in TreeOptions.VETShellOptions) and ValidateNamespace(Node, NS);
end;

procedure TCustomVirtualExplorerTree.HideAnimateFolderWnd;
begin
  if Assigned(AnimateFolderEnum) then
  begin
    FreeAndNil(FAnimateFolderEnum);
  end;
end;

procedure TCustomVirtualExplorerTree.InitAllChildren(Node: PVirtualNode);

    procedure RunInitNodes(RootNode, RunNode: PVirtualNode);
    begin
      while Assigned(RunNode) do
      begin
        if Assigned(RunNode.FirstChild) then
          RunInitNodes(RootNode, RunNode.FirstChild);
          if not (vsInitialized in RunNode.States) then
            InitNode(RunNode);
        if RunNode <> RootNode then
          RunNode := RunNode.NextSibling
        else
          RunNode := nil
      end;
    end;

begin
  RunInitNodes(Node, Node);
end;

procedure TCustomVirtualExplorerTree.InitAllNodes;
{ Initializes all nodes that currently exist in the tree, In contrast to VT's   }
{ ReInitNode recursive that forces all nodes to expand then initialize them     }
  procedure DoInitAllInCurrentTree(Node: PVirtualNode);
  begin
    while Assigned(Node) do
    begin
      if Node.ChildCount > 0 then
        DoInitAllInCurrentTree(Node.FirstChild);
      if not (vsInitialized in Node.States) then
        InitNode(Node);
      Node := Node.NextSibling;
    end
  end;

begin
  DoInitAllInCurrentTree(GetFirst);
end;

function TCustomVirtualExplorerTree.InternalCreateNewFolder(TargetPIDL: PItemIDList;
  SuggestedFolderName: WideString): WideString;
// Creates a new folder in TargetPath.  The name of the new folder is returned by
// the function


   function FindChildByName(Parent: TNamespace; ChildName: WideString): PItemIDList;
   // Searches the Parent for the Child Name, returns AbsolutePIDL to the Child if found
   var
     EnumIDList:  IEnumIDList;
     Found: Boolean;
     PIDL: PItemIdList;
     Fetched: LongWord;
     ChildNS: TNamespace;
     EnumFlags: Cardinal;
     OldWow64: Pointer;
   begin
     Result := nil;
     EnumFlags := FileObjectsToFlags(FileObjects);
     if Parent.Folder then
     begin
        OldWow64 := Wow64RedirectDisable;
       try
         Found := False;
         Parent.ShellFolder.EnumObjects(0, EnumFlags, EnumIDList);
           while not Found and (EnumIDList.Next(1, PIDL, Fetched) = NOERROR) do
           begin
             ChildNS := TNamespace.Create(PIDLMgr.AppendPIDL(Parent.AbsolutePIDL, PIDL), nil);
             if ChildName = ChildNS.NameForParsing then
             begin
               Result := PIDLMgr.CopyPIDL(ChildNS.AbsolutePIDL);
               Found := True;
             end;
             ChildNS.Free;
             PIDLMgr.FreePIDL(PIDL);
           end;
       finally
         Wow64RedirectRevert(OldWow64)
       end
     end
   end;

const
  SAFETYVALVE = 200;

var
  Node, NewNode: PVirtualNode;
  NewName: WideString;
  TargetPath: WideString;
  NS, ParentNS: TNamespace;
  NewChildPIDL: PItemIDList;
  i: Integer;
begin
  Assert((toEditable in TreeOptions.MiscOptions), 'You must set the toEditable option to Edit paths');
  Result := '';
  TargetPath := PIDLToPath(TargetPIDL);
  TargetPath := WideStripTrailingBackslash(TargetPath);

  if WideDirectoryExists(TargetPath) then
  begin
    Node := WalkPIDLToNode(TargetPIDL, False, True, False, True);

    // Make sure we can get to the desired folder so we can select the new item
    if Assigned(Node) then
    begin
      // Generate a Unique Name
      NewName := WideNewFolderName(TargetPath, SuggestedFolderName);

      if WideCreateDir(NewName) then
      begin
        ParentNS := TNamespace.Create(TargetPIDL, nil);
        ParentNS.FreePIDLOnDestroy := False; // We don't own the PIDL

        // May need to spend some time waiting for windows to create the file,
        // especially in Win9x
        NewChildPIDL := nil;
        i := 0;
        while not Assigned(NewChildPIDL) and (i < SAFETYVALVE) do
        begin
          NewChildPIDL := FindChildByName(ParentNS, NewName);
          Inc(i);
          Sleep(10)
        end;

        if Assigned(NewChildPIDL) then
        begin
          ClearSelection;
          FocusedNode := nil;
          NS := TNamespace.Create(NewChildPIDL, nil);
          // The namespace is given to the node, don't free it
          NewNode := AddCustomNode(Node, NS, toCheckSupport in TreeOptions.MiscOptions);
          FocusedNode := NewNode;
          Selected[NewNode] := True;
          if Assigned(NewNode) then
          begin
            if Header.Columns.Count = 0 then
              EditNode(NewNode, -1)
            else
              EditNode(NewNode, 0);
          end;

          if ValidateNamespace(NewNode, NS) then
            Result := NS.NameForEditing;
        end;
        FreeAndNil(ParentNS);
      end
    end
  end;
end;

function TCustomVirtualExplorerTree.InternalData(Node: PVirtualNode): Pointer;
begin
  Result := PAnsiChar(Node) + FInternalDataOffset;
end;

function TCustomVirtualExplorerTree.InternalWalkPIDLToNode(PIDL: PItemIDList): PVirtualNode;
{ Walks the PIDL looking for a matching node in the fastest possible way.       }
{ eliminates all function calls and extra variables assoicatied with the        }
{ WalkPILDToPath function.                                                      }
var
  Root, Child: PVirtualNode;
  Found: Boolean;
  Oldcb: Word;
  Head, Tail: PItemIDList;
  NS: TNamespace;
  NewNodeData: PNodeData;
  i, RootIDCount: Integer;
begin
  Result := nil;
  if Assigned(PIDL) then
  begin
    { Get the node that is the root of our tree, depends on toHideRootFolder option }
    if toHideRootFolder in TreeOptions.VETFolderOptions then
      Root := RootNode
    else
      Root := GetFirst;

    if ValidateNamespace(Root, NS) then
    begin
      // See if it is the desktop
      if PIDL.mkid.cb = 0 then
        Result := Root
      else begin
        { The root may be to where we have to add the new file, if we are lucky!      }
        if not ILIsEqual(NS.AbsolutePIDL, PIDL) then
        begin
         // Use to have a test for ILIsParent for the RootNode, removed 4-5-06 makes
         // Kernel Notifier test very difficult for Listview
          Head := PIDL;

          RootIDCount := PIDLMgr.IDCount(NS.AbsolutePIDL);
          { The tree is not rooted off the desktop but off some sub-folder }
          { We know it is a child of the root PIDL from above so it is ok  }
          { to skip past that portion of the PIDL.                         }
          if RootIDCount > 0 then
            for i := 0 to RootIDCount - 1 do
              Head := PIDLMgr.NextID(Head);

          { Walk to the next tree level }
          Child := Root.FirstChild;
          Tail := Head;

          { Do until we are at the end of PIDL or the tree does not exist down the branch }
          while (Head.mkid.cb > 0) and Assigned(Child) do
          begin
            { Initialize Variables }
            Found := False;

            { Walk to the next level of the PIDL so search tree level }
            Inc(PAnsiChar( Tail), Head.mkid.cb);
            OldCB := Tail.mkid.cb;
            Tail.mkid.cb := 0;

            { Walk to the next tree level }
            Child := Root.FirstChild;

            { Search until a match of the trimmed PIDL is found in the branch or no more }
            { nodes exist in the level.                                                  }
            while Assigned(Child) and not Found  do
            begin
              NewNodeData := InternalData(Child);
              NS := NewNodeData.Namespace;
              if ILIsEqual(NS.AbsolutePIDL, PIDL) then
                Found := True
              else
                Child := Child.NextSibling
            end;

            { Increment the tree level and PIDL level and restore the current           }
            Root := Child;
            Tail.mkid.cb := OldCB;
            Head := Tail;
          end;
          Result := Root;
        end else
          Result := Root
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.InvalidateChildNamespaces(
  Node: PVirtualNode; RefreshIcon: Boolean);
var
  Child: PVirtualNode;
  NS: TNamespace;
begin
  if Assigned(Node) then
  begin
    BeginUpdate;
    WaitCursor(True);
    try
      Child := Node.FirstChild;
      while Assigned(Child) do
      begin
        if ValidateNamespace(Child, NS) then
        begin
          NS.InvalidateNamespace(RefreshIcon);
          InvalidateNode(Child)
        end;
        Child := Child.NextSibling
      end;
    finally
      EndUpdate;
      WaitCursor(False)
    end
  end
end;

procedure TCustomVirtualExplorerTree.InvalidateImageByIndex(ImageIndex: integer);

        procedure RecurseTree(ParentNode: PVirtualNode);
        var
          Child: PVirtualNode;
          NS: TNamespace;
        begin
          Child := ParentNode.FirstChild;
          while Assigned(Child) do
          begin
            if Child.ChildCount > 0 then
              RecurseTree(Child);
            if ValidateNamespace(Child, NS) then
              if (NS.GetIconIndex(False, icSmall, False) = ImageIndex) or
                 (NS.GetIconIndex(True, icSmall, False) = ImageIndex)
              then begin
                NS.InvalidateNamespace;
                InvalidateNode(Child)
              end;
            Child := Child.NextSibling;
          end;
        end;

begin
  WaitCursor(True);
  try
    RecurseTree(RootNode);
  finally
    WaitCursor(False);
  end;
end;

procedure TCustomVirtualExplorerTree.InvalidateNodeByPIDL(PIDL: PItemIDList);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  BeginUpdate;
  try
    Node := WalkPIDLToNode(PIDL, False, False, False, True);
    if Assigned(Node) then
      if ValidateNamespace(Node, NS) then
        if Assigned(Node.Parent) then
          RefreshNode(Node.Parent)
        else
          if NS.Folder then
            RefreshNode(Node)
  finally
    EndUpdate
  end
end;

function TCustomVirtualExplorerTree.ItemHasChildren(NS: TNamespace; Node, ParentNode: PVirtualNode): Boolean;
//
// Node is not used anymore .....
//
var
  Flags: Longword;
begin
  Result := True;
  if Assigned(NS) then
  begin
    Flags := BuildEnumFlags;

    Result := ((toFoldersExpandable in TreeOptions.VETFolderOptions) or
      ((ParentNode = nil) and not(toHideRootFolder in TreeOptions.VETFolderOptions)));

    // RecycleBin is reluctant to let us in and it is dog slow so always
    // set the children check mark if tree is expandable
    if Result and not NS.IsRecycleBin then
    begin
      Result := NS.HasSubFolder;
      if not Result then
      begin
        if (foFolders in FileObjects) then
          Result := NS.SubFoldersEx(Flags and not SHCONTF_NONFOLDERS);
        if not Result and (foNonFolders in FileObjects) then
          Result := NS.SubItemsEx(Flags);   // necessary
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.LiveColumnUpdate(Sender: TObject);
{ This is called from the Column Editor Form when the end user is changing      }
{ column widths with the right click dialog box.                                }
begin
  RebuildHeader(FRootFolderNamespace);
  Invalidate;
end;

procedure TCustomVirtualExplorerTree.LoadDefaultOptions;
begin
  TreeOptions.PaintOptions := DefaultVETPaintOptions;
  TreeOptions.VETFolderOptions := DefaultVETFolderOptions;
  TreeOptions.VETShellOptions := DefaultVETShellOptions;
  TreeOptions.VETMiscOptions := DefaultVETMiscOptions;
  TreeOptions.VETImageOptions := DefaultVETImageOptions; // Need to call prop setter
  TreeOptions.VETSyncOptions := DefaultVETSyncOptions;
end;

{$IFDEF EXPLORERCOMBOBOX_L}
procedure TCustomVirtualExplorerTree.LoadExplorerComboBox(Reader: TReader);
begin
  if Reader.ReadValue = vaNil then
    MessageBox(0, 'It is a vsNIL', 'It is a vsNIL', mb_OK);
end;
{$ENDIF}

procedure TCustomVirtualExplorerTree.Loaded;
begin
  inherited;
  if Active then
    ActivateTree(True);
end;

procedure TCustomVirtualExplorerTree.LoadFromStream(Stream: TStream);
begin
  { LoadFromStream is unsupported in VET.  Use the Views property }
    Assert(not Assigned(Stream), 'LoadFromStream is not supported in VET. Use the ViewManager Property');
end;

procedure TCustomVirtualExplorerTree.MarkNodesCopied;
begin
  if toMarkCutAndCopy in TreeOptions.VETImageOptions then
  begin
    CancelCutOrCopy;
    TreeStates := TreeStates + [tsCopyPending];
    MarkCutCopyNodes;
    Invalidate;
  end
end;

procedure TCustomVirtualExplorerTree.MarkNodesCut;
begin
  if toMarkCutAndCopy in TreeOptions.VETImageOptions then
  begin
    CancelCutOrCopy;
    TreeStates := TreeStates + [tsCutPending];
    MarkCutCopyNodes;
    Invalidate;
  end
end;

procedure TCustomVirtualExplorerTree.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = ShellContextSubMenu then
      ShellContextSubMenu := nil;
    if AComponent = BackGndMenu then
      BackGndMenu := nil;
  end
end;

function TCustomVirtualExplorerTree.OkToBrowseTo(PIDL: PItemIDList): Boolean;
begin
  Result := True
end;

function TCustomVirtualExplorerTree.OkToExpandNode(Node: PVirtualNode): Boolean;
var
  NS: TNamespace;
  S: WideString;
begin
  Result := False;
  if ((toFoldersExpandable in TreeOptions.VETFolderOptions) or (Node = GetFirst) or
    (Node = RootNode)) and ValidateNamespace(Node, NS)
  then begin
    if NS.Valid then
    begin
      Result := True;
      S := NS.NameParseAddressInFolder;
      if (Length(S) = 3) and (S[2] = ':') and (S[3] = '\') and NS.Removable then
        Result := DiskInDrive(AnsiChar(AnsiString(S)[1]));
      if not Result then
      begin
        BeginUpdate;
        try
          HasChildren[Node] := False;
          if Node <> RootNode then
            InvalidateNode(Node)
        finally
          EndUpdate
        end
      end
    end
  end
end;

function TCustomVirtualExplorerTree.PathofNameSpace(
  NS: TNameSpace): WideString;
begin
  // DEPRICIATED:  NameForParsing now contains old PathOfNamespaceLogic
  Result := NS.NameForParsing
end;

function TCustomVirtualExplorerTree.PasteFromClipboard: Boolean;

  // If you want the ability to paste into mulitple folders use SelectedFilesPaste
  // This will only paste to one folder if and only if one folder is selected

var
  NS: TNamespace;
  NSA: TNamespaceArray;
  Handled: Boolean;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Handled := False;
      DoClipboardPaste(Handled);
      if not Handled then
      begin
        Result := False;
        if SelectedCount = 1 then
        begin
          SetLength(NSA, 1);
          if ValidateNamespace(GetFirstSelected, NS) then
          begin
            NSA[0] := NS;
            NS.Paste(Self, NSA);
            Result := True
          end
        end
      end else
        Result := True
    finally
      WaitCursor(False)
    end
  end else
    Result := False
end;

procedure TCustomVirtualExplorerTree.Notify(var Msg: TMessage);
begin
  if HandleAllocated then
    WMShellNotify(Msg)
end;


procedure TCustomVirtualExplorerTree.ReadChildNodes(Node: PVirtualNode; var ANodeArray: TNodeSearchArray; Sorted: Boolean; var ValidNodesRead: Integer);
{ Fills the Array with the child nodes if the target node is expanded. The      }
{ array is then sorted  by Name if required with the Quicksort function.  This  }
{ sort appears to be about 20% faster than the Merge sort routine.              }
///
/// NOTE:  Make sure any changes to this method are reflected in both VirtualExplorerTree.pas
//         and VirtualExplorerListview.pas
///
var
  Child: PVirtualNode;
  NS: TNamespace;
begin
  ValidNodesRead := 0;
  if Assigned(Node) then
  begin
    ValidNodesRead := 0;
    SetLength(ANodeArray, Node.ChildCount);
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      NS := PNodeData(InternalData(Child)).Namespace;
      ANodeArray[ValidNodesRead].Node := Child;
      ANodeArray[ValidNodesRead].NS := NS;
      Inc(ValidNodesRead);
      Child := Child.NextSibling;
    end;
    if Sorted then
    begin
      if ValidateNamespace(Node, NS) and (Length(ANodeArray) > 0) then
        NodeNamespaceQuickSort(ANodeArray, NS.ShellFolder, 0, Length(ANodeArray) - 1)
    end
  end
end;

procedure TCustomVirtualExplorerTree.RebuildHeader(BasedOnNamespace: TNamespace);
begin
  if ColumnDetails = cdShellColumns then
    RebuildShellHeader(BasedOnNamespace)
  else
  if ColumnDetails = cdVETColumns then
    RebuildVETHeader
end;

procedure TCustomVirtualExplorerTree.RebuildRootNamespace;
{ This will call InitNode for the root which will look at the FRootFolderNamespace }
{ and rebuild with that as its root after clearing the tree.                       }
var
  NewNodeData: PNodeData;
begin
  if (RebuildRootNamespaceCount = 0) and ShellNotifyManager.OkToDispatch then
  begin
    if not (csLoading in ComponentState) and Assigned(FRootFolderNamespace) then
    begin
      if Active then
      begin
        BeginUpdate;
        try
          if ThreadedImagesEnabled then
            GlobalThreadManager.FlushMessageCache(Self, TID_ICON);
          if ThreadedExpandMarkEnabled then
            ExpandMarkThreadManager.FlushMessageCache(Self, TID_EXPANDMARK);

          // Stupid, clear AFTER flushing! July 8, 2008
          Clear;

          if not(toHideRootFolder in TreeOptions.VETFolderOptions) then
          begin
            NewNodeData := InternalData(RootNode);
            FreeAndNil(NewNodeData.Namespace);
            RootNodeCount := 1;
          end else
          begin
            NewNodeData := InternalData(RootNode);
            if Assigned(NewNodeData) then
            begin
              FreeAndNil(NewNodeData.Namespace);
              NewNodeData.Namespace := TNamespace.Create(PIDLMgr.CopyPIDL(FRootFolderNamespace.AbsolutePIDL), nil);
              ExpandNamespaceFolder(RootNode);
            end
          end;
        finally
          EndUpdate;
          FocusedNode := GetFirst;
          ChangeLinkDispatch;
          DoRootRebuild
        end;
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.RebuildRootNamespaceBeginUpdate;
begin
  Inc(FRebuildRootNamespaceCount)
end;

procedure TCustomVirtualExplorerTree.RebuildRootNamespaceEndUpdate;
begin
  Dec(fRebuildRootNamespaceCount);
  if RebuildRootNamespaceCount = 0 then
    RebuildRootNamespace;
end;

procedure TCustomVirtualExplorerTree.RebuildShellHeader(BasedOnNamespace: TNamespace);
var
  i: integer;
  Column: TVETColumn;
  Title: WideString;
  ColStates: TSHColumnStates;
  SortCol: TColumnIndex;
  SortDir: TSortDirection;
begin
  if Assigned(BasedOnNamespace) then
  begin

    if toPersistListviewColumnSort in TreeOptions.VETMiscOptions then
    begin
      SortCol := Header.SortColumn;
      SortDir := Header.SortDirection;
    end else
    begin
      if Header.Columns.Count > 0 then
        SortCol := 0
      else
        SortCol := -1;
      SortDir := sdAscending;
    end;

    if HandleAllocated and (ColumnDetails = cdShellColumns) {and (BasedOnNamespace.Folder)} then
    begin
      CreatingHeaders := True;
      BeginUpdate;
      try
        ShellBaseColumnCount := 0;
        Header.Columns.Clear;
        Header.Options := Header.Options + [hoVisible];
        for i := 0 to BasedOnNamespace.DetailsSupportedColumns - 1 do
        begin
          Title := BasedOnNamespace.DetailsColumnTitle(i);
          { Win2k give some mysterious blank columns and duplicate header titles }
          // Need to include them and take them out of the choices later or the
          // indexes of Column Handlers can be messed up
      //    if (Title <> '') and not DuplicateTitle(i, Title) then
          begin
            Column := TVETColumn( Header.Columns.Add);
            Column.Text := BasedOnNamespace.DetailsColumnTitle(i);

            // This is the only way I can think of to find the Size column in a non language dependant way
            // The Hidden parameter comes from the fact that the hidden Recycle folders
            // are not detectable and the second column is not size but "Delete From" and should not be right justified
            // I know of no reliable way to detect these folders since their Name varies by local
            // 6.18.05; Hidden is a bad idea cause any folder can be hidden not just the
            // Recyle Folder.  Need to use IShellFolder2 to do this.
            if (i = 1) and (toRightAlignSizeColumn in TreeOptions.VETShellOptions) and
              RootFolderNamespace.FileSystem {and (not RootFolderNamespace.Hidden)}  then
              Column.Alignment := taRightJustify;

            if BasedOnNamespace.IsMyComputer and
              (Column.Index <= High(VET_DEFAULT_DRIVES_COLUMNWIDTHS))
            then
              Column.Width := VET_DEFAULT_DRIVES_COLUMNWIDTHS[Column.Index]
            else
            if BasedOnNamespace.IsControlPanel and
              (Column.Index <= High(VET_DEFAULT_CONTROLPANEL_COLUMNWIDTHS))
            then
              Column.Width := VET_DEFAULT_CONTROLPANEL_COLUMNWIDTHS[Column.Index]
            else
            if (BasedOnNamespace.IsNetworkNeighborhood or
               BasedOnNamespace.IsNetworkNeighborhoodChild) and
               ((BasedOnNamespace.DetailsSupportedColumns < 3) and
               (Column.Index <= High(VET_DEFAULT_NETWORK_COLUMNWIDTHS)))
            then
              Column.Width := VET_DEFAULT_NETWORK_COLUMNWIDTHS[Column.Index]
            else
            if (Column.Index <= High(VET_ColumnWidths)) then
              Column.Width := VET_ColumnWidths[Column.Index]
            else
              Column.Width := 120;
            Column.FColumnDetails := cdCustom;

            ColStates := BasedOnNamespace.DetailsGetDefaultColumnState(Column.Index);

            {$IFNDEF ALWAYS_SHOW_ALL_COLUMNS}
            if not (csOnByDefault in ColStates) then
              Column.Options := Column.Options - [coVisible];
            {$ENDIF ALWAYS_SHOW_ALL_COLUMNS}
            Inc(FShellBaseColumnCount);
          end
        end;
        DoHeaderRebuild;
        ReStoreColumnState
      finally
        EndUpdate;
        if Header.Columns.Count > SortCol then
        begin
          Header.SortColumn := SortCol;
          Header.SortDirection := SortDir;
        end;
        SortTree(Header.SortColumn, Header.SortDirection, False);
        CreatingHeaders := False
      end
    end
  end
end;

function TCustomVirtualExplorerTree.RebuildTree: Boolean;
begin   
  if ShellNotifyManager.OkToDispatch then
  begin
    RebuildRootNamespace;
    Result := True
  end else
    Result := False
end;

procedure TCustomVirtualExplorerTree.RebuildVETHeader;
var
  VETColumn: TVETColumn;
begin
  if HandleAllocated then
  begin
    CreatingHeaders := True;
    Header.Columns.Clear;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdFileName;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdSize;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdType;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdModified;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdAttributes;
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdAccessed;
    VETColumn.Options := VETColumn.Options - [coVisible];
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdCreated;
    VETColumn.Options := VETColumn.Options - [coVisible];
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdDOSName;
    VETColumn.Options := VETColumn.Options - [coVisible];
    VETColumn := TVETColumn( Header.Columns.Add);
    VETColumn.ColumnDetails := cdPath;
    VETColumn.Options := VETColumn.Options - [coVisible];
    DoHeaderRebuild;
    CreatingHeaders := False;
  end
end;

function TCustomVirtualExplorerTree.RefreshNode(Node: PVirtualNode): Boolean;
var
  NS: TNamespace;
begin
  Result := False;
  if ShellNotifyManager.OkToDispatch then
  begin
    if ValidateNamespace(Node, NS) then
    begin
      BeginUpdate;
      try
        if Assigned(Node) then
        begin
          if Node = RootNode then
            RefreshTree(True)
          else begin
            { A lot of stuff has to be saved to restore VET to its original state   }
            { under all scenarios.                                                  }
            VETPersistent.SaveTreeBranch(Self, Node, True);
            if ChildrenInitialized[Node] then
              DeleteChildren(Node, True);
            NS.InvalidateCache;
            HasChildren[Node] := ItemHasChildren(NS, Node, Node.Parent);
            VETPersistent.RestoreTreeBranch(Self, Node, True);
            if Node <> RootNode then
              InvalidateNode(Node);
          end
        end
      finally
        Result := True;
        EndUpdate;
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.RefreshNodeByPIDL(aPIDL: PItemIDList;
  ForceExpand, SaveSelection: Boolean);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  if Assigned(aPIDL) then
  try
    BeginUpdate;
    Node := WalkPIDLToNode(aPIDL, False, ForceExpand, False, True);
    if Assigned(Node) then
    begin
      if ValidateNamespace(Node, NS) and (Node <> RootNode) then
      begin
        { A lot of stuff has to be saved to restore VET to its original state   }
        { under all scenarios.                                                  }
        VETPersistent.SaveTreeBranch(Self, Node, SaveSelection);
        if ChildrenInitialized[Node] then
          DeleteChildren(Node, True);
        NS.InvalidateCache;
        HasChildren[Node] := ItemHasChildren(NS, Node, Node.Parent);
        VETPersistent.RestoreTreeBranch(Self, Node, SaveSelection);
        InvalidateNode(Node);
      end else
        RefreshTree(toRestoreTopNodeOnRefresh in TreeOptions.VETMiscOptions)
    end
  finally
    EndUpdate
  end
end;

function TCustomVirtualExplorerTree.RefreshTree(RestoreTopNode: Boolean = False): Boolean;
var
  WasThreadedEnum: Boolean;
begin
  if ShellNotifyManager.OkToDispatch then
  begin
    WaitCursor(True);
    // None of the restores will work if enumerated
    WasThreadedEnum := ThreadedEnum;
    try
      ThreadedEnum := False;
      VETPersistent.SaveTree(Self, True, True);
      { RestoreTree will implicitly rebuild the tree when setting the RootFolder }
      VETPersistent.RestoreTree(Self, True, True, RestoreTopNode);
    finally
      WaitCursor(False);
      ThreadedEnum := WasThreadedEnum;
      Result := True;
    end
  end else
    Result := False;
end;

procedure TCustomVirtualExplorerTree.ReReadAndRefreshNode(Node: PVirtualNode; SortNode: Boolean);
///
/// NOTE:  Make sure any changes to this method are reflected in both VirtualExplorerTree.pas
//         and VirtualExplorerListview.pas
///
var
  i, j, PIDLsRead, NodesRead, PIDLArrayLen, NodeArrayLen: Integer;
  PIDLArray: TPIDLArray;
  NS, NewNS: TNamespace;
  NodeArray: TNodeSearchArray;
  Compare: ShortInt;
  Allow: Boolean;
  CheckSupport: Boolean;
  ResultIsParent: Boolean;
begin
  if ValidateNamespace(Node, NS) then
  begin
    // First see if the namespace is valid
    if NS.Valid then
    begin
      // Smarter to read child nodes that are currently cached in the tree
      // first so ReadFolder does not trigger more events
      ReadChildNodes(Node, NodeArray, True, NodesRead);
      // Need to invalidate namespace as if a new item is added it may not be recognized by
      // the cached IShellFolder!
      NS.InvalidateNamespace(True);
      ReadFolder(NS.ShellFolder, FileObjectsToFlags(FileObjects), PIDLArray, True, PIDLsRead);
      CheckSupport := toCheckSupport in TreeOptions.MiscOptions;  // Local variable for speed
      BeginUpdate;
      try
        PIDLArrayLen := PIDLsRead;
        NodeArrayLen := NodesRead;
        j := 0;
        i := 0;
        // Run the current nodes in the tree with the nodes read from the folder in
        // parallel to see what is missing/added
        while (i < PIDLArrayLen) and (j < NodeArrayLen) do
        begin
          Compare := ShortInt(NS.ShellFolder.CompareIDs(0, PIDLArray[i], NodeArray[j].NS.RelativePIDL));
          if Compare = 0 then
          begin
            Inc(j);  // Node exists move on
            Inc(i)
          end else
          if Compare > 0 then
          begin
            // Must be a new node, don't Inc j
            Allow := True;
            NewNS := TNamespace.Create(PIDLMgr.CopyPIDL(PIDLArray[i]), NS);
            // Need to make sure any additions are ok'ed by the application
            if Assigned(OnEnumFolder) then
              OnEnumFolder(Self, NewNS, Allow);
            // Add it to the bottom of the list
            if Allow then
              AddCustomNode(Node, NewNS, CheckSupport)
            else
              NewNS.Free;
            Inc(i)
          end else
          begin
            GlobalThreadManager.FlushAllMessageCache(Self, NodeArray[j].Node);
            // Must be a removed node, don't Inc i
            if ValidateNamespace(NodeArray[j].Node, NewNS) then
            begin
              if CheckSupport then
                Storage.Delete(NewNS.AbsolutePIDL, [], True);
              DoNamespaceStructureChange(NodeArray[j].Node, NewNS, nscDelete);
            end;
            DeleteNode(NodeArray[j].Node);
            Inc(j)
          end
        end;

        // Add any new items
        while i < PIDLArrayLen do
        begin
          Allow := True;
          NewNS := TNamespace.Create(PIDLMgr.CopyPIDL(PIDLArray[i]), NS);
          { Need to make sure any additions are ok'ed by the application }
          if Assigned(OnEnumFolder) then
            OnEnumFolder(Self, NewNS, Allow);
          if Allow then
            AddCustomNode(Node, NewNS, CheckSupport)
          else
            NewNS.Free;
          Inc(i)
        end;

        while j < NodeArrayLen do
        begin
          GlobalThreadManager.FlushAllMessageCache(Self, NodeArray[j].Node);
          // Must be a removed node, don't Inc i
          if ValidateNamespace(NodeArray[j].Node, NewNS) then
          begin
            if CheckSupport then
              Storage.Delete(NewNS.AbsolutePIDL, [], True);
            DoNamespaceStructureChange(NodeArray[j].Node, NewNS, nscDelete);
          end;
          DeleteNode(NodeArray[j].Node);
          Inc(j)
        end;

        for i := 0 to Length(PIDLArray) - 1 do
          PIDLMgr.FreePIDL(PIDLArray[i])
      finally
        EndUpdate
      end;

      if SortNode then
        Sort(Node, Header.SortColumn, Header.SortDirection, False);

      if (Node.ChildCount > 0) and (toFoldersExpandable in TreeOptions.VETFolderOptions) then
        HasChildren[Node] := True;

    end else
    begin
      // The passed node is invalid itself
      if ValidateNamespace(Node, NS) then
        DoNamespaceStructureChange(Node, NS, nscDelete);
      NextSelectedNode(Node, True, ResultIsParent);
      DeleteNode(Node)
    end
  end
end;

procedure TCustomVirtualExplorerTree.ReStoreColumnState;
var
  NS: TNamespace;
  StorageNode: TNodeStorage;
  i: integer;
  Root: PVirtualNode;
begin
  if (Header.Columns.Count > 0) and (toPersistentColumns in TreeOptions.VETMiscOptions) and
    (ColumnDetails = cdShellColumns)
  then begin
    { Only makes sense for ListView type VET's }
    Root := RootNode;
    CreatingHeaders := True;
    try
      if ValidateNamespace(Root, NS) then
      begin
       if NS.Folder and Storage.Find(NS.AbsolutePIDL, [stColumns], StorageNode) then
        if (Header.Columns.Count) = Length(StorageNode.Storage.Column.Width) then
        begin
          for i := 0 to Header.Columns.Count - 1 do
          begin
            Header.Columns.Items[i].Width := StorageNode.Storage.Column.Width[i];
            Header.Columns.Items[i].Position := StorageNode.Storage.Column.Position[i];
            if StorageNode.Storage.Column.Visible[i] then
              Header.Columns.Items[i].Options := Header.Columns.Items[i].Options + [coVisible]
            else
              Header.Columns.Items[i].Options := Header.Columns.Items[i].Options - [coVisible]
          end;
        end else
          { Something is wrong }
          Storage.Delete(NS.AbsolutePIDL, [stColumns]);
      end;
    finally
      CreatingHeaders := False;
    end
  end
end;

procedure TCustomVirtualExplorerTree.SaveToStream(Stream: TStream;
  Node: PVirtualNode);
begin
  { SaveToStream is unsupported in VET.  Use the Views property }
  Assert(not Assigned(Stream), 'SaveToStream is not supported in VET. Use the ViewManager Property');
end;

procedure TCustomVirtualExplorerTree.SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift = evsCurrent);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Node := GetFirstSelected;
      if Assigned(Node) then
        if ValidateNamespace(Node, NS) then
          NS.Delete(Self, SelectedToNamespaceArray, ShiftKeyState)
    finally
      WaitCursor(False)
    end
  end
end;

procedure TCustomVirtualExplorerTree.SelectedFilesPaste(AllowMultipleTargets: Boolean);

{ Allows multiple folders to be selected and the same items to be pasted into   }
{ them from the clipboard if AllowMultipleTargets is true.                      }

var
  Node: PVirtualNode;
  NS: TNamespace;
  NSA: TNamespaceArray;
  Done: Boolean;
begin
  Done := False;
  if not AllowMultipleTargets and (SelectedCount > 1) then Exit;
  if AllowMultipleTargets or (SelectedCount = 1) then
  begin
    SetLength(NSA, 1);
    Node := GetFirstSelected;
    while not Done and Assigned(Node) do
    begin
      if ValidateNamespace(Node, NS) then
      begin
        if NS.Valid then
        begin
          NSA[0] := NS;
          NS.Paste(Self, NSA)
        end
      end;
      if not AllowMultipleTargets then
        Done := True
      else
        Node := GetNextSelected(Node)
    end
  end
end;

procedure TCustomVirtualExplorerTree.SelectedFilesShowProperties;
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  Node := GetFirstSelected;
  if Assigned(Node) then
    if ValidateNamespace(Node, NS) then
      NS.ShowPropertySheetMulti(Self, SelectedToNamespaceArray)
end;

function TCustomVirtualExplorerTree.SelectedToDataObject: IDataObject;
var
  NS: TNamespace;
begin
  if ValidateNamespace(GetFirstSelected, NS) then
    Result := NS.DataObjectMulti(SelectedToNamespaceArray)
end;

function TCustomVirtualExplorerTree.SelectedToNamespaceArray: TNamespaceArray;
var
  NS: TNamespace;
  Node: PVirtualNode;
  i: integer;
begin
  i := 0;
  Result := nil;
  if SelectedCount > 0 then
  begin
    SetLength(Result, SelectedCount);
    Node := GetFirstSelected;
    while Assigned(Node) do
    begin
      if ValidateNamespace(Node, NS) then
      begin
        if NS.Valid then
        begin
          // Move the focused node to the first position on the array
          // The context menu depends on this.
          if Node = FocusedNode then
          begin
            Result[i] := Result[0];
            Result[0] := NS;
          end
          else
            Result[i] := NS;
          Inc(i)
        end
      end;
      Node := GetNextSelected(Node);
    end;
    SetLength(Result, i);
  end
end;

function TCustomVirtualExplorerTree.SelectedToPIDLArray: TRelativePIDLArray;
var
  NS: TNamespace;
  Node: PVirtualNode;
  i: integer;
begin
  i := 0;
  Result := nil;
  if SelectedCount > 0 then
  begin
    SetLength(Result, SelectedCount);
    Node := GetFirstSelected;
    while Assigned(Node) do
    begin
      if ValidateNamespace(Node, NS) then
      begin
        if NS.Valid then
        begin
          Result[i] := NS.RelativePIDL;
          Inc(i)
        end
      end;
      Node := GetNextSelected(Node);
    end;
    SetLength(Result, i)
  end
end;

procedure TCustomVirtualExplorerTree.SetThreadedExpandMarkEnabled(Value: Boolean);
begin
  if (ComponentState * [csDesigning, csLoading] = []) and not (csCreating in ControlState) then
  begin
    if FThreadedExpandMarkEnabled <> Value then
    begin
      if Value then
      begin
        FThreadedExpandMarkEnabled := True
      end else
      begin
        ExpandMarkThreadManager.FlushMessageCache(Self, TID_EXPANDMARK);
        FThreadedExpandMarkEnabled := False
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.ShowAnimateFolderWnd;
begin
  if not Assigned(AnimateFolderEnum) then
  begin
    Header.Columns.Clear;
    if hoVisible in Header.Options then
    begin
      Include(FVETState, vsHeaderWasShown);
      Header.Options := Header.Options - [hoVisible];
    end else
      Exclude(FVETState, vsHeaderWasShown);
    
    AnimateFolderEnum := TAnimate.Create(Parent);
    AnimateFolderEnum.Visible := False;
    AnimateFolderEnum.Parent := GetAnimateWndParent;
    AnimateFolderEnum.Align := alClient;
    AnimateFolderEnum.CommonAVI := aviFindFolder;
    AnimateFolderEnum.Visible := True;
    AnimateFolderEnum.Active := True;
  end;
end;

procedure TCustomVirtualExplorerTree.ShowColumnDialog;
var
  Dummy: TColumnMenu;
  Item: TColumnMenuItem;
begin
  if (hoVisible in Header.Options) and (Header.Columns.Count > 0) then
  begin
    Dummy := TColumnMenu.Create(Self);
    Item := TColumnMenuItem.Create(Dummy);
    try
      Item.ColumnIndex := -2;
      Item.Click;
    finally
      Dummy.Free
    end;
  end
end;

procedure TCustomVirtualExplorerTree.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if ThreadedImagesEnabled then
      GlobalThreadManager.FlushMessageCache(Self, TID_ICON);
    ActivateTree(Value)
  end
end;

procedure TCustomVirtualExplorerTree.SetChangeNotiferEnabled(const Value: Boolean);
var
  Msg: TMsg;
  RePostQuitCode: Integer;
  RePostQuit: Boolean;
begin
  if (ComponentState * [csDesigning, csLoading] = [] )and not (csCreating in ControlState) then
  begin
    TerminateEnumThread;
    if Value <> FChangeNotifierEnabled then
    begin
      if Value then
      begin
        if (toChangeNotifierThread in TreeOptions.VETMiscOptions) then
        begin
          ChangeNotifier.RegisterShellChangeNotify(Self);
          FChangeNotifierEnabled := True
        end
      end else
      begin
        RePostQuit := False;
        RePostQuitCode := 0;
        if HandleAllocated then
          // First flush out any pending messages and let them be processed
          while PeekMessage(Msg, Handle, WM_SHELLNOTIFY, WM_SHELLNOTIFY, PM_REMOVE) do
          begin
            if Msg.Message = WM_QUIT then
            begin
              RePostQuitCode := Msg.WParam;
              RePostQuit := True
            end else
            begin
              TranslateMessage(Msg);
              DispatchMessage(Msg)
            end
          end;
          if RePostQuit then
            PostQuitMessage(RePostQuitCode);

        ChangeNotifier.UnRegisterShellChangeNotify(Self);
        FChangeNotifierEnabled := False;
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.SetColumnDetails(
  const Value: TColumnDetailType);
begin
 if FColumnDetails <> Value then
 begin
   FColumnDetails := Value;
   case Value of
    cdUser:
      begin
        Header.Columns.Clear;
        Header.Options := Header.Options - [hoVisible];
      end;
    cdVETColumns:
      begin
        Header.Columns.Clear;
        Header.Options := Header.Options + [hoVisible];
        RebuildHeader(FRootFolderNamespace);
      end;
    cdShellColumns:
      begin
        Header.Columns.Clear;
        Header.Options := Header.Options + [hoVisible];
        RebuildHeader(FRootFolderNamespace);
      end;
   end;
 end;
end;

procedure TCustomVirtualExplorerTree.SetDisableWaitCursors(
  const Value: Boolean);
begin
  if FDisableWaitCursors <> Value then
  begin
    { Reset the cursor if in the middle of a wait }
    if FWaitCursorRef <> 0 then
    begin
      FWaitCursorRef := 0;
      Screen.Cursor := crDefault
    end;
    FDisableWaitCursors := Value;
  end;
end;

{$IFDEF EXPLORERCOMBOBOX_L}
procedure TCustomVirtualExplorerTree.SetExplorerComboBox(const Value: TCustomVirtualExplorerCombobox);
begin
  if FExplorerComboBox <> Value then
  begin
    if Assigned(FExplorerComboBox) then
      VETChangeDispatch.UnRegisterChangeLink(FExplorerComboBox, Self, utLink );
    FExplorerComboBox := Value;
    if Assigned(FExplorerComboBox) then
      VETChangeDispatch.RegisterChangeLink(FExplorerComboBox, Self, ChangeLinkChanging, ChangeLinkFreeing);
  end;
end;
{$ENDIF}

procedure TCustomVirtualExplorerTree.SetFileObjects(const Value: TFileObjects);
{ Setter for FileObjects which is a set that dictates what objects are          }
{ enumerated for the tree, i.e. Folders, nonFolders, Hidden.                    }
begin
  if FFileObjects <> Value then
  begin
    FFileObjects := Value;
    ValidateFileObjects(FFileObjects);
    if not (toChangeNotifierThread in TreeOptions.VETMiscOptions) then
      FFileObjects := FFileObjects - [foEnableAsync];   // Need to have change notifier to ensure all data is shown
    RefreshTree;
  end;
end;

procedure TCustomVirtualExplorerTree.SetFileSizeFormat(
  const Value: TFileSizeFormat);
begin
  if FFileSizeFormat <> Value then
  begin
    FFileSizeFormat := Value;
    Invalidate;
  end
end;

procedure TCustomVirtualExplorerTree.SetFileSort(const Value: TFileSort);
begin
  if FFileSort <> Value then
  begin
    FFileSort := Value;
    if Header.SortColumn = 2 then
      SortTree(2, Header.SortDirection, False);
  end;
end;

procedure TCustomVirtualExplorerTree.SetNodeDataSize(const Value: Integer);
var
  WasActive: Boolean;
begin
  WasActive := Active;
  Active := False;
  inherited NodeDataSize := Value;
  Active := WasActive;
end;

procedure TCustomVirtualExplorerTree.SetOptions(const Value: TVirtualExplorerTreeOptions);
begin
  Assert(toReadOnly in Value.MiscOptions, 'Please use the TreeOptions.VETMiscOptions ReadOnly property and not the TreeOptions.MiscOptions ReadOnly property');
  inherited TreeOptions := Value;
end;


procedure TCustomVirtualExplorerTree.SetRootFolder(const Value: TRootFolder);

    function NewRootNamespace(Value: TRootFolder): TNamespace;
    begin
      case Value of
        rfAdminTools: Result := CreateSpecialNamespace(CSIDL_ADMINTOOLS);
        rfAltStartup: Result := CreateSpecialNamespace(CSIDL_ALTSTARTUP);
        rfAppData: Result := CreateSpecialNamespace(CSIDL_APPDATA);
        rfBitBucket: Result := CreateSpecialNamespace(CSIDL_BITBUCKET);
        rfCommonAdminTools: Result := CreateSpecialNamespace(CSIDL_COMMON_ADMINTOOLS);
        rfCommonAltStartup: Result := CreateSpecialNamespace(CSIDL_COMMON_ALTSTARTUP);
        rfCommonAppData: Result := CreateSpecialNamespace(CSIDL_COMMON_APPDATA);
        rfCommonDesktopDirectory: Result := CreateSpecialNamespace(CSIDL_COMMON_DESKTOPDIRECTORY);
        rfCommonDocuments: Result := CreateSpecialNamespace(CSIDL_COMMON_DOCUMENTS);
        rfCommonFavorties: Result := CreateSpecialNamespace(CSIDL_COMMON_FAVORITES);
        rfCommonPrograms: Result := CreateSpecialNamespace(CSIDL_COMMON_PROGRAMS);
        rfCommonStartMenu: Result := CreateSpecialNamespace(CSIDL_COMMON_STARTMENU);
        rfCommonStartup: Result := CreateSpecialNamespace(CSIDL_COMMON_STARTUP);
        rfCommonTemplates: Result := CreateSpecialNamespace(CSIDL_COMMON_TEMPLATES);
        rfControlPanel: Result := CreateSpecialNamespace(CSIDL_CONTROLS );
        rfCookies: Result := CreateSpecialNamespace(CSIDL_COOKIES );
        rfDesktop: Result := CreateSpecialNamespace(CSIDL_DESKTOP);
        rfDesktopDirectory: Result := CreateSpecialNamespace(CSIDL_DESKTOPDIRECTORY);
        rfDrives: Result := CreateSpecialNamespace(CSIDL_DRIVES);
        rfFavorites: Result := CreateSpecialNamespace(CSIDL_FAVORITES);
        rfFonts: Result := CreateSpecialNamespace(CSIDL_FONTS);
        rfHistory: Result := CreateSpecialNamespace(CSIDL_HISTORY);
        rfInternet: Result := CreateSpecialNamespace(CSIDL_INTERNET);
        rfInternetCache: Result := CreateSpecialNamespace(CSIDL_INTERNET_CACHE);
        rfLocalAppData: Result := CreateSpecialNamespace(CSIDL_LOCAL_APPDATA);
        rfMyPictures: Result := CreateSpecialNamespace(CSIDL_MYPICTURES);
        rfNetHood: Result := CreateSpecialNamespace(CSIDL_NETHOOD);
        rfNetwork: Result := CreateSpecialNamespace(CSIDL_NETWORK);
        rfPersonal: Result := CreateSpecialNamespace(CSIDL_PERSONAL);
        rfPrinters: Result := CreateSpecialNamespace(CSIDL_PRINTERS);
        rfPrintHood: Result := CreateSpecialNamespace(CSIDL_PRINTHOOD);
        rfProfile: Result := CreateSpecialNamespace(CSIDL_PROFILE);
        rfProgramFiles: Result := CreateSpecialNamespace(CSIDL_PROGRAM_FILES);
        rfCommonProgramFiles: Result := CreateSpecialNamespace(CSIDL_PROGRAM_FILES_COMMON);
        rfPrograms: Result := CreateSpecialNamespace(CSIDL_PROGRAMS);
        rfRecent: Result := CreateSpecialNamespace(CSIDL_RECENT);
        rfSendTo: Result := CreateSpecialNamespace(CSIDL_SENDTO);
        rfStartMenu: Result := CreateSpecialNamespace(CSIDL_STARTMENU);
        rfStartUp: Result := CreateSpecialNamespace(CSIDL_STARTUP);
        rfSystem: Result := CreateSpecialNamespace(CSIDL_SYSTEM);
        rfTemplate: Result := CreateSpecialNamespace(CSIDL_TEMPLATES);
        rfWindows: Result := CreateSpecialNamespace(CSIDL_WINDOWS);
        rfCustom: Result := TNamespace.Create(PathToPIDL(RootFolderCustomPath), nil);
        rfCustomPIDL: Result := TNamespace.Create(PIDLMgr.CopyPIDL(RootFolderCustomPIDL), nil);
      else
        Result := nil;
      end;
    end;

var
  NewNodeData: PNodeData;
  Allow: Boolean;
  s: string;
begin
  Allow := True;
  { This has already been handled in the setters for a custom PIDL or custom Path }
  if (Value <> rfCustomPIDL) and (Value <> rfCustom) then
  begin
    TempRootNamespace := NewRootNamespace(Value);
    DoRootChanging(Value, TempRootNamespace, Allow);
  end;
  try
    if Allow then
    begin
      BeginUpdate;
      try
        TerminateEnumThread;
        Clear;
        FRootFolder := Value;
        if not (Value = rfCustom) then
          FRootFolderCustomPath := '';
        if not (Value = rfCustomPIDL) then
          PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);

        try
        FreeAndNil(FRootFolderNamespace);
        except
          s := Classname
        end;
        s := Classname;

        if toHideRootFolder in TreeOptions.VETFolderOptions then
        begin
          NewNodeData := InternalData(RootNode);
          FreeAndNil(NewNodeData.Namespace);
        end;
          { TempRootNamespace was created in the property setters for the custom  }
          { path and pidl selections.                                             }
          FRootFolderNamespace := TempRootNamespace;
          TempRootNamespace := nil;
      finally
        EndUpdate;
        if Assigned(FRootFolderNamespace) then
          RebuildRootNamespace
      end;
      DoRootChange;
    end;
  finally
    { Always clean up the property }
    FreeAndNil(FTempRootNamespace)
  end
end;

procedure TCustomVirtualExplorerTree.SetRootFolderCustomPath(const Value: WideString);
var
  Allow: Boolean;
  CustomPath: WideString;
begin
 // if Value <> FRootFolderCustomPath then
  begin
    Allow := True;
    if Value <> '' then
      CustomPath := IncludeTrailingBackslashW(Value)
    else
      CustomPath := Value;

    TempRootNamespace := TNamespace.Create(PathToPIDL(CustomPath), nil);
    try
      DoRootChanging(rfCustom, TempRootNamespace, Allow);
      if Allow then
      begin
        TerminateEnumThread;
        if WideDirectoryExists(CustomPath) then
        begin
          FRootFolderCustomPath := CustomPath;
          { TempRootNamespace will be used in RootFolder Setter }
          RootFolder := rfCustom;
        end
      end
    finally
      { If all goes well this should be nil after returning from RootFolder :=    }
      { but in case.                                                              }
      FreeAndNil(FTempRootNamespace);
    end
  end
end;

procedure TCustomVirtualExplorerTree.SetRootFolderCustomPIDL(
  const Value: PItemIDList);
var
  Allow: Boolean;
begin
  if Assigned(Value) then
  begin
    Allow := True;
    TempRootNamespace := TNamespace.Create(PIDLMgr.CopyPIDL(Value), nil);
    try
      DoRootChanging(rfCustomPIDL, TempRootNamespace, Allow);
      if Allow then
      begin
        TerminateEnumThread;
        if FRootFolderCustomPIDL <> Value then
        begin
          PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);
          FRootFolderCustomPIDL := PIDLMgr.CopyPIDL(Value);
          { TempRootNamespace will be used in RootFolder Setter }
          RootFolder := rfCustomPIDL
        end
      end
    finally
      { If all goes well this should be nil after returning from RootFolder :=    }
      { but in case.                                                              }
      FreeAndNil(FTempRootNamespace);
    end
  end
end;

procedure TCustomVirtualExplorerTree.SetThreadedImagesEnabled(const Value: Boolean);
begin
  if (ComponentState * [csDesigning, csLoading] = []) and not (csCreating in ControlState) then
  begin
    if FThreadedImagesEnabled <> Value then
    begin
      if Value then
      begin
        FThreadedImagesEnabled := True
      end else
      begin
        GlobalThreadManager.FlushMessageCache(Self, TID_ICON);
        FThreadedImagesEnabled := False
      end
    end
  end
end;

{$IFDEF EXPLORERTREE_L}
procedure TCustomVirtualExplorerTree.SetVirtualExplorerTree(
  const Value: TCustomVirtualExplorerTree);
begin
  if FVirtualExplorerTree <> Value then
  begin
    if Assigned(FVirtualExplorerTree) then
      VETChangeDispatch.UnRegisterChangeLink(FVirtualExplorerTree, Self, utLink );
    FVirtualExplorerTree := Value;
    if Assigned(FVirtualExplorerTree) then
      VETChangeDispatch.RegisterChangeLink(FVirtualExplorerTree, Self, ChangeLinkChanging, ChangeLinkFreeing);
  end;
end;
{$ENDIF}

procedure TCustomVirtualExplorerTree.ShellExecuteFolderLink(NS: TNamespace;
  WorkingDir, CmdLineArgument: WideString);
var
  ShellLink: TVirtualShellLink;
  LinkTarget: WideString;
begin
  if Assigned(NS) then
  begin
    if NS.Link then
    begin
      ShellLink := TVirtualShellLink.Create(nil);
      try
        ShellLink.ReadLink(NS.NameParseAddress);
        LinkTarget := ShellLink.TargetPath;
        if WideDirectoryExists(LinkTarget) and OkToBrowseTo(ShellLink.TargetIDList) then
          with TreeOptions do
            if BrowseToByPIDL(ShellLink.TargetIDList, toExpandTarget in VETSyncOptions,
              toSelectTarget in VETSyncOptions, False, toCollapseTargetFirst in VETSyncOptions) then
      finally
        ShellLink.Free
      end
    end
  end
end;

procedure TCustomVirtualExplorerTree.StoreColumnState;
var
  NS: TNamespace;
  StorageNode: TNodeStorage;
  i: integer;
  Root: PVirtualNode;
begin
  { Right now only ExplorerTree has persistent columns since it is well defined }
  { how it should work.  Using persistent columns in classic VET mode is a bit  }
  { less clear how it should work.                                              }
  if (Header.Columns.Count > 0) and (toPersistentColumns in TreeOptions.VETMiscOptions) and
    (ColumnDetails = cdShellColumns)
  then begin
   { Only makes sense for ShellColumns }
    Root := RootNode;
    if ValidateNamespace(Root, NS) then
    begin
      if NS.Folder then
      begin
        StorageNode := Storage.Store(NS.AbsolutePIDL, [stColumns]);
        if Assigned(StorageNode) then
        begin
          { Change the column count in Storage if necessary }
          if Length(StorageNode.Storage.Column.Width) < Header.Columns.Count then
          begin
            SetLength(StorageNode.Storage.Column.Width, Header.Columns.Count);
            SetLength(StorageNode.Storage.Column.Position, Header.Columns.Count);
            SetLength(StorageNode.Storage.Column.Visible, Header.Columns.Count);
          end;
          for i := 0 to Header.Columns.Count - 1 do
          begin
            StorageNode.Storage.Column.Width[i] := Header.Columns.Items[i].Width;
            StorageNode.Storage.Column.Position[i] := Header.Columns.Items[i].Position;
            StorageNode.Storage.Column.Visible[i] := coVisible in Header.Columns.Items[i].Options;
          end;
        end
      end
    end
  end
end;

function TCustomVirtualExplorerTree.SuggestDropEffect(Source: TObject;
  Shift: TShiftState; Pt: TPoint; AllowedEffects: Integer): Integer;
{ Override this and return all possible actions.  The Namespace will eventually }
{ make up its own mind about what actions are possible.                         }
begin
  Result := DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE;
end;

function TCustomVirtualExplorerTree.ValidateColumnManager(
  Node: PVirtualNode; var ColumnManager: TColumnManager): Boolean;
{ By making the ColumnManager a var it eliminates the compiler warning of       }
{ variable not initialized when calling this function.                          }
{ Validates and assigns the TColumnManager object assiciated with the TreeNode. }
var
  NewNodeData: PNodeData;
begin
  ColumnManager := nil;
  Result := False;
  if Assigned(Node) then
  begin
    NewNodeData := InternalData(Node);
    if Assigned(NewNodeData) then
      ColumnManager := NewNodeData.ColumnManager;
    Result := Assigned(ColumnManager)
  end
end;

function TCustomVirtualExplorerTree.ValidateNamespace(Node: PVirtualNode;
  var Namespace: TNamespace): Boolean;
{ By making the Namespace a var it eliminates the compiler warning of variable  }
{ not initialized when calling this function.                                   }
{ Validates and assigns the TNamespace object assiciated with the TreeNode.     }
var
  NewNodeData: PNodeData;
begin
  Namespace := nil;
  Result := False;
  if Assigned(Node) then
  begin
    NewNodeData := InternalData(Node);
    if Assigned(NewNodeData) then
      Namespace := NewNodeData.Namespace;
    Result := Assigned(Namespace)
  end
end;

function TCustomVirtualExplorerTree.ValidateParentNamespace(
  Node: PVirtualNode; var Namespace: TNamespace): Boolean;
{ By making the Namespace a var it eliminates the compiler warning of variable  }
{ not initialized when calling this function.                                   }
{ Validates and assigns the TNamespace object assiciated with the TreeNode.     }
var
  NewNodeData: PNodeData;
begin
  Namespace := nil;
  Result := False;
  if Assigned(Node) then
    if Assigned(Node.Parent) then
    begin
      NewNodeData := InternalData(Node.Parent);
      if Assigned(NewNodeData) then
        Namespace := NewNodeData.Namespace;
      Result := Assigned(Namespace)
    end
end;

procedure TCustomVirtualExplorerTree.TerminateEnumThread;
begin
  try
    EnterCriticalSection(FEnumLock);
    EnumThreadTimer(False);
    HideAnimateFolderWnd;
    FreeAndNil(FEnumBkGndList);
    // Frees itself
    if Assigned(EnumThread) then
      EnumThread.Terminate;
  finally
    EnumThread := nil;
    LeaveCriticalSection(FEnumLock)
  end
end;

procedure TCustomVirtualExplorerTree.WaitCursor(Show: Boolean);
begin
  if not DisableWaitCursors then
  begin
    if Show then
    begin
      if (FWaitCursorRef = 0) and not GlobalWaitCursorSet then
      begin
        Screen.Cursor := crHourGlass;
        GlobalWaitCursorSet := True;
      end;
      Inc(FWaitCursorRef);
    end else
    begin
      Dec(FWaitCursorRef);
      if (FWaitCursorRef = 0) and GlobalWaitCursorSet then
      begin
        Screen.Cursor := crDefault;
        GlobalWaitCursorSet := False
      end
    end
  end;
end;

function TCustomVirtualExplorerTree.WalkPIDLToNode(AnAbsolutePIDL: PItemIDList;
  SelectNode, ForceExpand, QuietExpand, ShowAllSiblings: Boolean): PVirtualNode;
{ Walks down the passed PIDL enumerating necessary namespace objects.           }
var
  Root, Child: PVirtualNode;
  Match: Boolean;
  Done: Boolean;
  Oldcb: Word;
  Head, Tail: PItemIDList;
  NS: TNamespace;
  i, RootIDCount: integer;
begin
  Result := nil;
  if toHideRootFolder in TreeOptions.VETFolderOptions then
    Root := RootNode
  else
    Root := GetFirst;
  Done := False;
  Head := AnAbsolutePIDL;
  if ValidateNamespace(Root, NS) then
  begin
    if Assigned(NS.AbsolutePIDL) and Assigned(AnAbsolutePIDL) then
    begin
      { If they are equal then stop now, the result is the root node. }
      { Note that the "odd" PILDs sent by the ShellNotify thread (through the  }
      { undocumented Shell Notification system) will not compare relative to    }
      { the parent of a "proper" PIDL.  It must by compared from the desktop to }
      { work.                                                                   }
      if not ILIsEqual(NS.AbsolutePIDL, AnAbsolutePIDL) then
      begin
        { If the PIDL is not a child of the root node it is a hopeless attempt }
        if ILIsParent(NS.AbsolutePIDL, AnAbsolutePIDL, False) then
        begin
          RootIDCount := PIDLMgr.IDCount(NS.AbsolutePIDL);
          { The tree is not rooted off the desktop but off some sub-folder }
          { We know it is a child of the root PIDL from above so it is ok  }
          { to skip past that portion of the PIDL.                         }
          if RootIDCount > 0 then
            for i := 0 to RootIDCount - 1 do
              Head := PIDLMgr.NextID(Head);

          { Head should now point to the PIDL that represents the root of the }
          { tree regardless of where in the namspace it is rooted from.       }
          while (Root <> nil) and not Done do
          begin
            Tail := PIDLMgr.NextID(Head); { Temporarily trim the PIDL so it can be compared }
            Oldcb := Tail.mkid.cb;
            Tail.mkid.cb := 0;
            Done := Oldcb = 0;           { cb = 0 terminates the PIDL so this the the last ItemID }
            Match := False;              { Reset the Match Flag }
            Child := nil;                { Reset the Child node }
            if ForceExpand then          { Enumerate the Node if necessary }
            begin
              if QuietExpand then
                ReInitChildren(Root, False)
              else begin
                if ShowAllSiblings then
                  Expanded[Root] := True;
              end
            end;
            { Don't force the initialization of the childern if not requested }
            { GetFirstChild will try to enumerate the node.                   }
            if Root.ChildCount > 0 then
              Child := GetFirstChild(Root); { Start looking at each child }
            { Search the child nodes for a PIDL match }
            while (Child <> nil) and not Match do
            begin
              if ValidateNamespace(Child, NS) then
              begin
                Match := NS.ComparePIDL(AnAbsolutePIDL, True) = 0; { Test the PIDLs }
                if not Match then
                  Child := GetNextSibling(Child)
              end else
                Done := True;
            end;
            // Ignoring the checkbox stuff is ok as the app will fill it in when
            // VT calls OnInitNode
            if not Match and ILIsParent(NetworkNeighborhoodFolder.AbsolutePIDL, AnAbsolutePIDL, False) then
              Child := AddCustomNode(Root, TNamespace.Create(PIDLMgr.CopyPIDL(AnAbsolutePIDL), nil), False);
            Root := Child; { Make the child node the new Root }
            Tail.mkid.cb := Oldcb;
            Head := Tail;
          end;
          { If caller wants node selected and it was found do it. }
          if SelectNode and Assigned(Root) then
          begin
            ClearSelection;
            Selected[Root] := True;
            ScrollIntoView(Root, False, False);
            FocusedNode := Root
          end;
          Result := Root;
        end
      end else
      begin
        Result := Root;
        if SelectNode and Assigned(Result) then
        begin
          ClearSelection;
          Selected[Result] := True;
          ScrollIntoView(Root, False, False);
          FocusedNode := Result
        end;
      end
    end
  end;
end;

procedure TCustomVirtualExplorerTree.WMCommonThreadCallback(var Msg: TWMThreadRequest);
var
  NS: TNamespace;
  IsDragging: Boolean;
  MarkRequest: TExpandMarkThreadRequest;
  IconRequest: TShellIconThreadRequest;
begin
  try
    case Msg.RequestID of
      TID_ICON:
        begin
          IconRequest := Msg.Request as TShellIconThreadRequest;
          if ValidateNamespace(IconRequest.Item, NS) then
          begin
            NS.SetIconIndexByThread(IconRequest.ImageIndex, IconRequest.OverlayIndex, True);
            IsDragging := Dragging;
            InvalidateNode(IconRequest.Item);
            { The window has changed make sure drag image knows about it.}
            if IsDragging then
              UpdateWindowAndDragImage(Self, GetClientRect, False, True);
          end
        end;
      TID_EXPANDMARK:
        begin
          MarkRequest := Msg.Request as TExpandMarkThreadRequest;
          if ValidateNamespace(MarkRequest.Item, NS) then
          begin
            HasChildren[MarkRequest.Item] := MarkRequest.ShowMark;
            InvalidateNode(MarkRequest.Item)
          end;
        end
    end;
  finally
    Msg.Request.Release
  end
end;

procedure TCustomVirtualExplorerTree.WMContextMenu(var Msg: TWMContextMenu);
var
  HitInfo: THitInfo;
  Pt: TPoint;
  ShowByKeyEvent,
  NodeHit: Boolean;
  Node: PVirtualNode;
begin
  if not (tsOLEDragging in TreeStates) and not(toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    // If the message is sent due to a Shift - F10 press or a Menu Key Press (WM_APPS)
    // then the position will be -1, -1
    ShowByKeyEvent := (Msg.XPos = -1) and (Msg.YPos = -1);

    Pt := ScreenToClient(SmallPointToPoint(Msg.Pos));
    GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
    NodeHit := Assigned(HitInfo.HitNode) and ((HitInfo.HitPositions * [hiOnItemLabel, hiOnNormalIcon] <> []) and
      (HitInfo.HitColumn < 1)) or (toFullRowContextMenuActivate in TreeOptions.VETShellOptions);

    try
      if ShowByKeyEvent then
      begin
        // Fired by a shortcut keystroke
        if ContextMenuManager.ShowContextMenuOfSelectedItem then
        begin
          // Set a flag so VET can bypass VT's Node Dependant PopupMenu implementation
          // It must stay set until after the WM_RBUTTONUP message where it is reset
          Msg.Result := 1;  // Don't let it propogate to parent windows
        end else
        begin
          // If the shell menu failed is it because they are disabled? If so use the
          // hit node to show the Popup property menu
          Node := GetFirstSelected;
          if not(ContextMenuManager.Enabled) and Assigned(Node) then
            Msg.Pos := PointToSmallPoint(ContextMenuManager.CalculatePopupPoint(Node));
          inherited;
        end;
      end else
      // Fired by a right mouse click
      begin
        if NodeHit then
        begin
          if ContextMenuManager.ShowContextMenuOfActiveNode(SmallPointToPoint(Msg.Pos)) then
            // Set a flag so VET can bypass VT's Node Dependant PopupMenu implementation
            // It must stay set until after the WM_RBUTTONUP message where it is reset
            Msg.Result := 1; // Don't let it propogate to parent windows
        end else
          Msg.Result := ShowBkGndContextMenu(SmallPointToPoint(Msg.Pos));
        inherited;
      end;
    finally
      // The RButtonUp will clear the state
      if ShowByKeyEvent then
        ContextMenuManager.ResetState
    end;
  end else
    Msg.Result := 1; // Don't let it propogate to parent windows
end;

procedure TCustomVirtualExplorerTree.WMDeviceChange(var Msg: TMessage);
begin
  inherited;
  ValidRootNamespace // fire the event if NS is not valid
end;

procedure TCustomVirtualExplorerTree.WMDrawItem(var Msg: TMessage);
begin
  inherited;
  ContextMenuManager.HandleContextMenuMsg(Msg.Msg, Msg.wParam, Msg.lParam, Msg.Result);
end;

procedure TCustomVirtualExplorerTree.WMDestroy(var Msg: TWMDestroy);
begin
  inherited;
end;

procedure TCustomVirtualExplorerTree.WMInitMenuPopup(var Msg: TMessage);
begin
  inherited;
  ContextMenuManager.HandleContextMenuMsg(Msg.Msg, Msg.wParam, Msg.lParam, Msg.Result);
end;

procedure TCustomVirtualExplorerTree.WMInvalidFileName(var Msg: TMessage);
begin
  Beep;
  if Header.Columns.Count = 0 then
    EditNode(PVirtualNode( Msg.wParam), -1)
  else
    EditNode(PVirtualNode( Msg.wParam), 0)
end;

procedure TCustomVirtualExplorerTree.WMKeyDown(var Message: TWMKeyDown);
begin
  if not Assigned(FocusedNode) then
    FocusedNode := GetFirst;
  // Override VT's default handling of VK_APPS
  if Message.CharCode = VK_APPS then
    DefaultHandler(TMessage(Message))
  else
    inherited
end;

procedure TCustomVirtualExplorerTree.WMKillFocus(var Message: TWMKillFocus);
begin
  // A Goofy Shell issue.  The Namespace will send a kill focus to the window
  // because it normally opens a dialog for the file transfer progress.  In
  // VT's kill focus it clears all the flag I need later on!  Need to bypass
  // VT's kill focus code if this happens during a drag/drop
  if not (tsOLEDragging in TreeStates) then
    inherited
  else
    with TMessage(Message) do
      DefWindowProc(Handle, Msg, wParam, lParam);
  AltKeyDown := False;
end;

procedure TCustomVirtualExplorerTree.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  Info: THitInfo;
  NS: TNamespace;
begin
  if not AltKeyDown then
  begin
    GetHitTestInfoAt(Message.XPos, Message.YPos, True, Info);

    // Call the OnDblClick event before the tree is expanded
    DoTreeDblClick(mbLeft, SmallPointToPoint(Message.Pos));

    // Set flag so clicking on the button still expands the node in DoExpanding
    FExpandingByButtonClick := hiOnItemButton in Info.HitPositions;
    inherited;
    FExpandingByButtonClick := False;

    { We can get Double Clicks if a node is expanded to rapidly and then it we    }
    { execute the currently selected node, not good.  Do some checks to know where}
    { we are.                                                                     }
    if ((Info.HitNode = GetFirstSelected) and (Info.HitPositions * [hiOnItemLabel, hiOnNormalIcon] <> [])) or
      ((toExecuteOnDblClkFullRow in TreeOptions.VETMiscOptions) and (toFullRowSelect in TreeOptions.SelectionOptions) and (hiOnItem in Info.HitPositions))
    then
      DoShellExecute(GetFirstSelected);
  end else
  begin
    if ValidateNamespace(GetFirstSelected, NS) then
      NS.ShowPropertySheetMulti(Self, SelectedToNamespaceArray);
  end;
end;

procedure TCustomVirtualExplorerTree.WMLButtonDown(var Message: TWMLButtonDown);
var
  Info: THitInfo;
begin
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, Info);    
  // Set flag so clicking on the button still expands the node in DoExpanding
  FExpandingByButtonClick := hiOnItemButton in Info.HitPositions;
  inherited;
  FExpandingByButtonClick := False;
end;

procedure TCustomVirtualExplorerTree.WMMeasureItem(var Msg: TMessage);
begin
  inherited;
  ContextMenuManager.HandleContextMenuMsg(Msg.Msg, Msg.wParam, Msg.lParam, Msg.Result);
end;

procedure TCustomVirtualExplorerTree.WMMenuChar(var Msg: TMessage);
{ Called when owner window recieves wm_InitPopupMenu, wm_DrawItem,             }
{ wm_MeasureItem messages to support IContextMenu2 SendTo: ownerdraw Items     }
begin
  inherited;
  ContextMenuManager.HandleContextMenuMsg(Msg.Msg, Msg.wParam, Msg.lParam, Msg.Result)
end;

procedure TCustomVirtualExplorerTree.WMMenuSelect(var Msg: TMessage);
begin
  ContextMenuManager.MenuSelect(Msg.Msg, Msg.wParam, Msg.lParam, Msg.Result)
end;

procedure TCustomVirtualExplorerTree.WMNCDestroy(var Message: TWMNCDestroy);
begin
  TerminateEnumThread;
  ThreadedImagesEnabled := False;
  ChangeNotifierEnabled := False;
  ThreadedExpandMarkEnabled := False;
  inherited;
  // The Handle Property is set to 0 after this inherited call
end;

procedure TCustomVirtualExplorerTree.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
  DoTreeDblClick(mbRight, SmallPointToPoint(Message.Pos));
end;

procedure TCustomVirtualExplorerTree.WMRButtonDown(var Msg: TWMRButtonDown);
var
  HitInfo: THitInfo;
  AutoDrag: Boolean;
begin
  ContextMenuManager.RightClick(Msg.XPos, Msg.YPos, bsDown, ctClient);
  inherited;


  // ****************************************************************
  // It is necessary to add a property to TBaseVirtualTree in
  // VirtualTrees.pas  I have a request in to
  // Mike to make this change.  Without it there is an inconsistancy in the
  // Right Click context menus and Right Button Drag and Drop.
  //
  // TBaseVirtualTree = ....
  //  private
  //    FLastClickPos: TPoint;
  //    ....
  //  protected
  //    //  ADD THIS PROPERTY
  //    property LastClickPos: TPoint read FLastClickPos write FLastClickPos;
  LastClickPos := Point(Msg.XPos, Msg.YPos);
  // This will be added to VT in version 4.0.2
  // ****************************************************************     

  GetHitTestInfoAt(Msg.XPos, Msg.YPos, True, HitInfo);
  AutoDrag := DoBeforeDrag(HitInfo.HitNode, HitInfo.HitColumn);
  if AutoDrag and (TreeStates * [tsLeftButtonDown, tsRightButtonDown, tsMiddleButtonDown] <> []) then
    BeginDrag(False);

end;

procedure TCustomVirtualExplorerTree.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  ContextMenuManager.RightClick(Msg.XPos, Msg.YPos, bsUp, ctClient);
  ReleaseCapture //  Workaround for Bug in VT 3.6.2
end;

procedure TCustomVirtualExplorerTree.WMShellNotify(var Msg: TMessage);
{ WinZip does not follow the rules when creating a zip file.  It sends an       }
{ UpdateDir eventhough it really has not created the file yet!  Once you add    }
{ the new files to the zip it sends an UpdateItem to the file it did not create }
{ yet.  It appears it is sending the UpdateItem instead of the CreateItem like  }
{ the documentation clearly states.  This is WinZip 8.0.                        }
///
/// NOTE:  Make sure any changes to this method are reflected in both VirtualExplorerTree.pas
//         and VirtualExplorerListview.pas
///

var
  Count: integer;
  Node: PVirtualNode;
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  i: integer;
  NS: TNamespace;
  S: string;
  WS: WideString;
  MappedDriveNotification: Boolean;
  List: TList;
  PIDL: PItemIDList;
  Flags: LongWord;
  ValidNS, WasSelected: Boolean;
begin
  if ValidRootNamespace then
  begin
    try
      {$IFDEF GXDEBUG}
      SendDebug('  ');
      SendDebug('.........................................');
      SendDebug('............. New Message ...............');
      SendDebug('WM_SHELLNOTIFY');
      {$ENDIF}
      if not ShellNotifyManager.OkToDispatch then
      begin
        {$IFDEF GXDEBUG}
        SendDebug('Resending Packet.....................');
        {$ENDIF}
        ShellNotifyManager.ReDispatchShellNotify(TVirtualShellEventList( Msg.wParam));
      end else
      begin
        {$IFDEF GXDEBUG}
        SendDebug('Processing Packet.....................');
        {$ENDIF}
        ShellEventList := TVirtualShellEventList( Msg.wParam);
        List := ShellEventList.LockList;
        try
          // Put the Rename's in the front of the list so we don't rebuild and loose the old node
          List.Sort(ShellEventSort);
          try
            begin
              ValidNS := True;
              Count := List.Count;
              for i := 0 to Count - 1 do
              begin
                if ValidNS then
                begin
                  MappedDriveNotification := False;
                  ShellEvent := TVirtualShellEvent(List.Items[i]);

                  DoShellNotify(ShellEvent);

                  if not(ShellEvent.Handled) then
                  begin
                    // Mapped network drive get an UpdateDir for any event(s) on the drive
                    // keeps from being swamped with notifications from other machines
                    if (toTrackChangesInMappedDrives in TreeOptions.VETMiscOptions) and
                       (ShellEvent.ShellNotifyEvent in [vsneUpdateDir]) then
                    begin
                      NS := TNamespace.Create(ShellEvent.PIDL1, nil);
                      NS.FreePIDLOnDestroy := False;
                      if NS.Folder then
                      begin
                        ValidNS := NS.Valid;
                        if IsUnicode then
                        begin
                          WS := WideExtractFileDrive(NS.NameForParsing);
                          MappedDriveNotification := WideIsDrive(WS) and (GetDriveTypeW_MP(PWideChar(WS)) = DRIVE_REMOTE)
                        end else
                        begin   
                          S := ExtractFileDrive(NS.NameForParsing);
                          MappedDriveNotification := WideIsDrive(S) and (GetDriveType(PChar(S)) = DRIVE_REMOTE)
                        end;
                        if MappedDriveNotification then
                          RereadAndRefreshNode(FindNodeByPIDL(ShellEvent.PIDL1), not(foNonFolders in FileObjects))
                      end;
                      NS.Free
                    end;

                    if not MappedDriveNotification then
                    begin
                      {$IFDEF LUCION} Application.ProcessMessages; {$ENDIF LUCION}
                      case ShellEvent.ShellNotifyEvent of
                        // The notification thread maps these to UpdateDir notifications
                        vsneCreate,           // Creating a File
                        vsneDelete,           // Deleting a File
                        vsneMkDir,            // Creating a Directory
                        vsneRmDir:            // Deleting a Directory
                          begin
                          // It is now possible to recieve all the notification raw
                          // Don't expect the change notifictaions to work right but
                          // it is now possible
                          //  Assert(True=False, 'Unexpected Shell Notification');
                          end;

                        // Both PIDLs in the Rename notifications are valid. The thread ensures
                        // that these are truly renames and not moves so we don't have to check
                        // here.  (NT4 calls a move a Rename) The thread checks the parent PIDL
                        // and if they are different then it must be a move and it maps both
                        // directories to UpdateDir events. If it makes it here it means that
                        // the parent pidls of bother items/folders are the same and it is a true
                        // rename.
                        vsneRenameFolder,
                        vsneRenameItem:
                          begin
                            // We have already filtered out move operations that fire this so
                            //  it is for sure this is a pure rename so handle it as a special case

                            // Find the old node
                            Node := InternalWalkPIDLToNode(ShellEvent.PIDL1);
                            if Assigned(Node) then
                            begin
                              WasSelected := Selected[Node];
                              Node := InternalWalkPIDLToNode(ShellEvent.ParentPIDL1);
                              RereadAndRefreshNode(Node, not(foNonFolders in FileObjects));
                              Selected[InternalWalkPIDLToNode(ShellEvent.PIDL2)] := WasSelected
                            end else
                              RereadAndRefreshNode(Node, not(foNonFolders in FileObjects));
                          end;
                        vsneDriveAdd,         // Mapping a network drive
                        vsneDriveAddGUI,      // CD inserted shell should create new window
                        vsneDriveRemoved:     // UnMapping a network drive
                          begin
                            Node := InternalWalkPIDLToNode(ShellEvent.ParentPIDL1);
                            if Assigned(Node) then
                              RereadAndRefreshNode(Node, not(foNonFolders in FileObjects))
                          end;
                        vsneMediaInserted,    // New CD, Jazz Drive, Memory card etc. inserted.
                        vsneMediaRemoved:     // New CD, Jazz Drive, Memory card etc. removed
                          begin
                            // M$ Hack to get Win9x to change the image and name of removable
                            // drives when the media changes
                            NS := TNamespace.Create(ShellEvent.PIDL1, nil);
                            NS.FreePIDLOnDestroy := False;
                            PIDL := NS.RelativePIDL;
                            Flags := SFGAO_VALIDATE;
                            NS.ParentShellFolder.GetAttributesOf(0, PIDL, Flags);
                            try
                              Node := InternalWalkPIDLToNode(ShellEvent.PIDL1);
                              if Assigned(Node) then
                                RefreshNode(Node)  // The Node is aways the Root of the drive
                              else begin
                                if WideExtractFileDrive(NS.NameForParsing) = WideExtractFileDrive(RootFolderNamespace.NameForParsing) then
                                  RefreshTree(toRestoreTopNodeOnRefresh in TreeOptions.VETMiscOptions);
                              end
                            finally
                              NS.Free
                            end;
                          end;
                        // A lot of the different notifications are mapped to this event in
                        // the thread. This minimizes the number of times we have to refresh
                        // the tree.
                        vsneUpdateDir:
                          if Assigned(ShellEvent.PIDL1) then
                          begin
                            if ShellEvent.InvalidNamespace then
                              Node := InternalWalkPIDLToNode(ShellEvent.ParentPIDL1)
                            else
                              Node := InternalWalkPIDLToNode(ShellEvent.PIDL1);
                            RereadAndRefreshNode(Node, not(foNonFolders in FileObjects))
                          end;

                        // This notification is sent when a namespace has been mapped to a
                        // different image.
                        vsneUpdateImage:   // New image has been mapped to the item
                          begin
                            FlushImageLists;
                            InvalidateImageByIndex(Integer(ShellEvent.DoubleWord1));
                          end;

                        { This group of notifications is based on an existing namespace that   }
                        { has had its properties changed.  As such the PIDL must be refreshed  }
                        { to read in the new properties stored in the PIDL.                    }
                        vsneNetShare,         // Folder being shared or unshared
                        vsneNetUnShare,       //  ?? Should be the opposite of NetShare
                        vsneServerDisconnect,
                        vsneUpdateItem:       // Properties of file OR dir changed }
                          begin
                            Node := InternalWalkPIDLToNode(ShellEvent.PIDL1);

                            if ValidateNamespace(Node, NS) then
                            begin
                              BeginUpdate;
                              { Must flush the PIDL since it stores info used in the details   }
                              NS.InvalidateRelativePIDL(FileObjects);
                              InvalidateNode(Node);
                              EndUpdate;
                            end
                          end;
                        vsneAttributes:       // Printer properties changed and ???
                          begin
                            Node := InternalWalkPIDLToNode(ShellEvent.ParentPIDL1);
                              InvalidateChildNamespaces(Node, False);
                          end;
                        { This notification is sent when the freespace on a drive has changed. }
                        { for now it appears the only thing this may impact is the disk size   }
                        { details under MyComputer.  Don't update the image as it should be    }
                        { same.                                                                }
                        vsneFreeSpace:
                          begin
                            Node := InternalWalkPIDLToNode(DrivesFolder.AbsolutePIDL);
                            InvalidateChildNamespaces(Node, False);
                          end;
                        { This notification is sent when the shell has changed an assocciation }
                        { of a file type.                                                      }
                        vsneAssoccChanged:  // File association changed need new images
                          begin
                            ForceIconCachRebuild;
                          end
                      end
                    end
                  end; // Handled
                  DoAfterShellNotify(ShellEvent);   //Added by Jeff
                end
              end
            end
          except
            // The shell sometimes sends bogus PIDLs (likely applications with bugs sending bad PIDLs with SHChangeNotify)
          end
        finally
          ShellEventList.UnlockList;
          ShellEventList.Release;
        end
      end
    except
      raise
    end
  end else
    TVirtualShellEventList( Msg.wParam).Release;
end;

procedure TCustomVirtualExplorerTree.WMSysChar(var Msg: TWMSysChar);
var
  NS: TNamespace;
begin
  case Msg.CharCode of
    VK_RETURN:
      begin
        if (Msg.CharCode = VK_RETURN) and AltKeyDown then
          if ValidateNamespace(GetFirstSelected, NS) then
          begin
            NS.ShowPropertySheetMulti(Self, SelectedToNamespaceArray);
            Msg.CharCode := Ord(#0)
          end
      end
    else   // Stop the beep
      inherited
  end
end;

procedure TCustomVirtualExplorerTree.WMSysKeyDown(var Msg: TWMSysKeyDown);
begin
  if (Msg.KeyData and $20000000 > 0) then
    AltKeyDown := True;
  inherited;
end;

procedure TCustomVirtualExplorerTree.WMSysKeyUp(var Msg: TWMSysKeyUp);
begin
  inherited;
  AltKeyDown := False;
end;

procedure TCustomVirtualExplorerTree.WMQueryEndSession(
  var Msg: TWMQueryEndSession);
begin
  Msg.Result := 1;
  ChangeNotifierEnabled := False;
end;

procedure TCustomVirtualExplorerTree.WMTimer(var Msg: TWMTimer);
var
  ShowAnimation: Boolean;
  i: Integer;
  Node: PVirtualNode;
  NewNodeData : PNodeData;
  Allow: Boolean;
  NS: TNamespace;
  LocalList: TCommonPIDLList;
begin
  inherited;
  if (Msg.TimerID = ID_TIMER_ENUMBKGND)then
  begin
    Inc(FEnumBkGndTime);
    if Assigned(EnumBkGndList) and not (vsNotifyChanging in VETState) and HandleAllocated then
    begin
      EnterCriticalSection(FEnumLock);
      try
        LocalList := EnumBkGndList;
        EnumBkGndList := nil;
        TerminateEnumThread; // Must do this first to make sure we don't get another timer message, VT will dispatch messages if it validate a node apparently
        Node := RootNode;
        BeginUpdate;
        try
          LocalList.SharePIDLs := True;
          if vsHeaderWasShown in FVETState then
            Header.Options := Header.Options + [hoVisible];
          RebuildHeader(FRootFolderNamespace);  
          DeleteChildren(Node);
          for i := 0 to LocalList.Count - 1 do
          begin
            NS := TNamespace.Create(LocalList[i], RootFolderNamespace);
            if (toForceHideRecycleBin in TreeOptions.VETFolderOptions) and NS.IsRecycleBin then
            begin
              NS.Free;
              Exit
            end;
            Allow := True;
            DoEnumFolder(NS, Allow);
            if Allow then
            begin
              { Called from within ExpandNamespaceFolder so BeginUpdate/EndUpdate called }
              NewNodeData := InternalData(AddNodeToTree(Node));
              if Assigned(NewNodeData) then
              begin
                NewNodeData.Namespace := NS;
                NewNodeData.ColumnManager := TColumnManager.Create(Self);
              end
            end else
              NS.Free;
          end;
          DoCustomNamespace(Node);
          { Sort calls InitChildren which can lead to endless recursion problems if }
          { there is no children.                                                   }
          if Node.ChildCount > 0 then
            Sort(Node, Header.SortColumn, Header.SortDirection, False);
        finally
          FreeAndNil(LocalList);
          EndUpdate;
          EnumThreadFinished;
        end
      finally
        LeaveCriticalSection(FEnumLock)
      end
    end else
    if EnumBkGndTime > BKGNDLENTHYOPERATIONTIME then
    begin
      DoEnumThreadLengthyOperation(ShowAnimation);
      if ShowAnimation then
        ShowAnimateFolderWnd;
    end
  end
end;

{ TVETPersistent }

procedure TVETPersistent.Clear;
begin
  { TCommonPIDLLists know how to free the PIDL's automaticlly }
  SelectedPIDLs.Clear;
  LeafNodes.Clear;
  PIDLMgr.FreePIDL(SelectedPIDLsParent);
  PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);
  PIDLMgr.FreeAndNilPIDL(FFocusPIDL);
  PIDLMgr.FreeAndNilPIDL(FTopNodePIDL);
  RootFolderCustomPath := '';
  Storage.Clear;
end;

constructor TVETPersistent.Create;
begin
  inherited;
  FSelectedPIDLs := TCommonPIDLList.Create;
  FLeafNodes := TLeafNodeList.Create;
  Storage := TRootNodeStorage.Create;
end;

destructor TVETPersistent.Destroy;
begin
  { TCommonPIDLLists know how to free the PIDL's automaticlly }
  SelectedPIDLs.Free;
  LeafNodes.Free;
  // Support for Halt()
  if Assigned(PIDLMgr) then
  begin
    PIDLMgr.FreePIDL(SelectedPIDLsParent);
    PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);
    PIDLMgr.FreeAndNilPIDL(FFocusPIDL);
    PIDLMgr.FreeAndNilPIDL(FTopNodePIDL);
  end;
  Storage.Free;
  inherited;
end;

procedure TVETPersistent.FullInitTree(VET: TCustomVirtualExplorerTree; DoInit: Boolean);
begin
  if DoInit then
  begin
    VET.InitAllNodes;
    Include(FStates, vpsFullInit);
  end else
    Exclude(FStates, vpsFullInit);
end;

procedure TVETPersistent.LoadFromFile(FileName: WideString;
  Version: integer; ReadVerFromStream: Boolean);
begin
  inherited;
end;

procedure TVETPersistent.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
var
  Count: integer;
begin
  inherited;
  { BaseLine Stream, always read }
  LeafNodes.LoadFromStream(S, Version, ReadVerFromStream);
  PIDLMgr.FreePIDL(FSelectedPIDLsParent);
  FSelectedPIDLsParent := PIDLMgr.LoadFromStream(S);
  SelectedPIDLs.LoadFromStream(S);
  PIDLMgr.FreePIDL(FRootFolderCustomPIDL);
  FRootFolderCustomPIDL := PIDLMgr.LoadFromStream(S);
  FTopNodePIDL := PIDLMgr.LoadFromStream(S);
  FFocusPIDL := PIDLMgr.LoadFromStream(S);
  S.ReadBuffer(FRootFolder, SizeOf(RootFolder));

  // Bug fix. If the stream is earlier than version 3 read in the path the
  // old way else use the new correct way
  if StreamVersion < VETStreamStorageVer_3 then
  begin
    S.read(Count, SizeOf(Count));
    SetLength(FRootFolderCustomPath, Count);
    S.read(PWideChar( FRootFolderCustomPath)^, Count);
  end else
  begin
    S.read(Count, SizeOf(Count));
    SetLength(FRootFolderCustomPath, Count);
    S.read(PWideChar( FRootFolderCustomPath)^, Count * 2);
  end;

  Storage.LoadFromStream(S, Version, ReadVerFromStream);
  { Add new stream data here }
  { if Version >= PersistentStreamVersion_0 then }
  {   read new data                     }
end;

procedure TVETPersistent.ReStoreLeafPIDLs(VET: TCustomVirtualExplorerTree;
  RootNode: PVirtualNode);
{ This function takes the stored PIDLs that were saved from the old VET and     }
{ tries to find matches in the new VET.  These PIDLs are the minimum number of  }
{ expanded nodes that are necessary to take the tree to its origial expanded    }
{ state.  It uses the VET function WalkPIDLToNode to accomplish this then does  }
{ the final expand.                                                             }
var
  i: integer;
  Node: PVirtualNode;
begin
  VET.BeginUpdate;
  try
    for i := 0 to LeafNodes.Count - 1 do
    begin
      Node := VET.WalkPIDLToNode(LeafNodes[i].PIDL, False, True, False, True);
      // The special case of a hidden root folder can cause this not to work as
      // VT won't expand the RootNode
      if Assigned(Node) and (Node <> VET.RootNode) then
        VET.Expanded[Node] := LeafNodes[i].Expanded
      else begin
        if LeafNodes[i].Expanded then
          VET.ExpandNamespaceFolder(Node)
      end
    end
  finally
    VET.EndUpdate;
  end
end;

procedure TVETPersistent.ReStoreSelectedPIDLs(VET: TCustomVirtualExplorerTree;
  RootNode: PVirtualNode);
{ Simply starts the process to reselect the same nodes if they are available.  }
var
  Node: PVirtualNode;
  NodeArray: TNodeSearchArray;
  NodesRead: Integer;
  Desktop, Folder: IShellFolder;
  i, j, PIDLArrayLen, NodeArrayLen: Integer;
  Compare: ShortInt;
begin
  VET.ClearSelection;
  VET.FocusedNode := nil;
  VET.BeginUpdate;
  try
    SHGetDesktopFolder(Desktop);
    if SelectedPIDLs.Count > 0 then
    begin
      if Assigned(SelectedPIDLsParent) then
      begin
        if not PIDLMgr.IsDesktopFolder(SelectedPIDLsParent) then
          Desktop.BindToObject(SelectedPIDLsParent, nil, IShellFolder, Pointer(Folder))
        else
          Folder := Desktop;

        if Assigned(Folder) then
        begin
          Node := VET.WalkPIDLToNode(SelectedPIDLsParent, False, True, False, True);
          VET.ReadChildNodes(Node, NodeArray, True, NodesRead);

          if SelectedPIDLs.Count > 0 then
           PIDLListQuickSort(SelectedPIDLs, Folder, 0, SelectedPIDLs.Count - 1);
          { Now the PIDLArray is sorted and the NodeArray is Sorted }

          PIDLArrayLen := SelectedPIDLs.Count;
          NodeArrayLen := NodesRead;
          j := 0;
          i := 0;
          while (i < PIDLArrayLen) and (j < NodeArrayLen) do
          begin
            Compare := ShortInt(Folder.CompareIDs(0, SelectedPIDLs[i], NodeArray[j].NS.RelativePIDL));
            if Compare = 0 then
            begin
              VET.Selected[NodeArray[j].Node] := True;
              Inc(i)
            end;
            Inc(j);

            // This needs a more complete search to find the nodes..........

          end
        end
      end else
        { We know there is a selected item so it must be the RootNode }
        VET.Selected[VET.GetFirst] := True;
    end
  finally
    VET.EndUpdate;
    VET.FocusedNode := VET.GetFirstSelected;
  end
end;

procedure TVETPersistent.RestoreTree(VET: TCustomVirtualExplorerTree; RestoreSelection,
  RestoreFocus: Boolean; ScrollToOldTopNode: Boolean = False);
var
  Node: PVirtualNode;
begin
  Node := nil;
  VET.BeginUpdate;
  try
    VET.EnableChangeNotifier(False);
    if VET.ThreadedImagesEnabled then
      GlobalThreadManager.FlushMessageCache(VET, TID_ICON);
    if VET.ThreadedExpandMarkEnabled then
      ExpandMarkThreadManager.FlushMessageCache(VET, TID_EXPANDMARK);
    VET.ClearSelection;
    VET.RootFolderCustomPath := RootFolderCustomPath;
    VET.RootFolderCustomPIDL := PIDLMgr.CopyPIDL(RootFolderCustomPIDL);
    { Custom Path or PIDL will implicitly set the RootFolder and trigger events }
    if (RootFolderCustomPath = '') and not Assigned(RootFolderCustomPIDL) then
      VET.RootFolder := RootFolder;
   RestoreTreeBranch(VET, VET.GetFirst, RestoreSelection);
   if RestoreFocus then
    begin
      Node := VET.WalkPIDLToNode(FocusPIDL, False, True, False, True);
      VET.FocusedNode := Node;
    end
  finally
    VET.EnableChangeNotifier(True);
    VET.EndUpdate;

    { VT is a bit flaky if this is done within a Begin/End Update block }
    if ScrollToOldTopNode and Assigned(FTopNodePIDL) then
    begin
      Node := VET.WalkPIDLToNode(FTopNodePIDL, False, True, False, True);
      if Assigned(Node) and (Node <> VET.RootNode) then
        VET.TopNode := Node;
    end else
    begin
      if Assigned(Node) and (Node <> VET.RootNode) then
        VET.TopNode := Node;
    end
  end
end;

procedure TVETPersistent.RestoreTreeBranch(VET: TCustomVirtualExplorerTree;
  Node: PVirtualNode; RestoreSelection: Boolean);
{ RestoreTreeBranch is a minimal implementation.  It is not intended to be      }
{ Persistent, it is mainly for a fast way to store the state of a node          }
{ decandants so it can quickly be restored.                                     }
begin
  ReStoreLeafPIDLs(VET, Node);
  if RestoreSelection then
    ReStoreSelectedPIDLs(VET, Node);
end;

procedure TVETPersistent.SaveToFile(FileName: WideString; Version: integer;
  ReadVerFromStream: Boolean);
begin
  inherited;   
end;

procedure TVETPersistent.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
var
  Count: integer;
begin
  inherited;
  LeafNodes.SaveToStream(S, Version, WriteVerToStream);
  PIDLMgr.SaveToStream(S, FSelectedPIDLsParent);
  SelectedPIDLs.SaveToStream(S);
  PIDLMgr.SaveToStream(S, FRootFolderCustomPIDL);
  PIDLMgr.SaveToStream(S, TopNodePIDL);
  PIDLMgr.SaveToStream(S, FocusPIDL);
  S.WriteBuffer(FRootFolder, SizeOf(RootFolder));
  Count := Length(RootFolderCustomPath);
  S.WriteBuffer(Count, SizeOf(Count));
  S.WriteBuffer(PWideChar( FRootFolderCustomPath)^, Count * 2);
  Storage.SaveToStream(S, Version, WriteVerToStream);
end;

procedure TVETPersistent.SaveTree(VET: TCustomVirtualExplorerTree; SaveSelection, SaveFocus: Boolean);
{ Initializes the lists then stores the necessasry PIDL information to be able  }
{ to reconstruct the tree, depending on what had changed in the tree.           }
var
  NS, NextNS: TNamespace;
  NextNode: PVirtualNode;
  ResultIsParent: Boolean;
begin
  if VET.ValidRootNamespace then
  begin
    FullInitTree(VET, True);
    try
      PIDLMgr.FreeAndNilPIDL(FFocusPIDL);
      PIDLMgr.FreeAndNilPIDL(FTopNodePIDL);
      PIDLMgr.FreeAndNilPIDL(FRootFolderCustomPIDL);

      // Save the top node
      if VET.ValidateNamespace(VET.TopNode, NS) then
      begin
        if NS.Valid then
          FTopNodePIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL)
        else begin
          NextNode := VET.NextSelectedNode(VET.TopNode, False, ResultIsParent);
          if VET.ValidateNamespace(NextNode, NextNS) then
            FTopNodePIDL := PIDLMgr.CopyPIDL(NextNS.AbsolutePIDL)
        end
      end;

      // Save the focused node.  If the node is invalid then don't save anything
      if SaveFocus and VET.ValidateNamespace(VET.FocusedNode, NS) then
        if NS.Valid then
          FocusPIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL);

      if toHideRootFolder in VET.TreeOptions.VETFolderOptions then
        SaveTreeBranch(VET, VET.RootNode, SaveSelection)
      else
        SaveTreeBranch(VET, VET.GetFirst, SaveSelection);

      FRootFolderCustomPIDL := PIDLMgr.CopyPIDL(VET.RootFolderCustomPIDL);
      FRootFolder := VET.RootFolder;
      FRootFolderCustomPath := VET.RootFolderCustomPath;
    finally
      FullInitTree(VET, False);  // Clear the state
    end
  end
end;

procedure TVETPersistent.SaveTreeBranch(VET: TCustomVirtualExplorerTree;
  Node: PVirtualNode; SaveSelection: Boolean);
{ SaveTreeBranch is a minimal implementation.  It is not intended to be         }
{ Persistent, it is mainly for a fast way to store the state of a node          }
{ so it can quickly be restored.                                                }
begin
  if not (vpsFullInit in States) then
    VET.InitAllChildren(Node);
  if SaveSelection then
    StoreSelectedPIDLs(VET, Node);
  StoreLeafPIDLs(VET, Node);
end;

function TVETPersistent.StoreLeafPIDLs(VET: TCustomVirtualExplorerTree;
  RootNode: PVirtualNode): PVirtualNode;
{ Walks the children of Node passed and stores the AbsolutePIDLs of the nodes   }
{ that are necessary to expand the Treeview to its current state.  This stores  }
{ the absolute minimum number of PIDLs.                                         }
{ Return is undefined.                                                          }

  function WalkTreeLeafs(VET: TCustomVirtualExplorerTree; RootNode: PVirtualNode): PVirtualNode;
  var
    NS: TNamespace;
    ChildNode: PVirtualNode;
    ChildIsLeaf: Boolean;
  begin
    Result := RootNode;
    if VET.Expanded[RootNode] then
      ChildNode := VET.GetFirstChild(RootNode)
    else begin
      Result := nil;
      ChildNode := nil;
    end;

    while Assigned(ChildNode) do
    begin
      if VET.Expanded[ChildNode] then
      begin
        ChildIsLeaf := Assigned(WalkTreeLeafs(VET, ChildNode));
        if ChildIsLeaf and VET.ValidateNamespace(ChildNode, NS) then
          if NS.Valid then
            LeafNodes.AddLeafNode(NS.AbsolutePIDL, VET.Expanded[ChildNode]);
        Result := nil; // This can't be a leaf since it has an expanded node
      end;
      ChildNode := VET.GetNextSibling(ChildNode);
    end;
  end;

var
  NS: TNamespace;
begin
  LeafNodes.Clear;
  Result := WalkTreeLeafs(VET, RootNode);
  { If no leafs are found get the state of the root node at least to be restored }
  if (LeafNodes.Count = 0) and VET.ValidateNamespace(RootNode, NS) then
    if NS.Valid then
      LeafNodes.AddLeafNode(NS.AbsolutePIDL, VET.Expanded[RootNode]);
end;

procedure TVETPersistent.StoreSelectedPIDLs(VET: TCustomVirtualExplorerTree;
  RootNode: PVirtualNode);
{ This function walk VT's selected structure to extract the PIDLs of the nodes  }
{ that are selected and makes copies of them.                                   }
var
  i: integer;
  SelNode: PVirtualNode;
  NS: TNamespace;
begin
  SelectedPIDLs.Clear;
  PIDLMgr.FreeAndNilPIDL(FSelectedPIDLsParent);
  SelNode := VET.GetFirstSelected;

  if VET.ValidateNamespace(SelNode, NS) then
  begin
    // We make the assumption the parent is valid even if the child is not
    if not NS.IsDesktop then
      FSelectedPIDLsParent := PIDLMgr.StripLastID(PIDLMgr.CopyPIDL(NS.AbsolutePIDL))
    else
      FSelectedPIDLsParent := nil
  end;

  for i := 0 to VET.SelectedCount - 1 do
  begin
    if VET.ValidateNamespace(SelNode, NS) then
      if NS.Valid then
        SelectedPIDLs.CopyAdd(NS.RelativePIDL);
      SelNode := VET.GetNextSelected(SelNode);
  end
end;

procedure TCustomVirtualExplorerTree.DoNamespaceStructureChange(
  Node: PVirtualNode; NS: TNamespace;
  ChangeType: TNamespaceStructureChange);
begin
  if Assigned(OnNamespaceStructureChange) then
    OnNamespaceStructureChange(Self, Node, NS, ChangeType)
end;

function TCustomVirtualExplorerTree.IsAnyEditing: Boolean;
begin
  Result := IsEditing
end;

procedure TCustomVirtualExplorerTree.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
end;

function TCustomVirtualExplorerTree.DoCancelEdit: Boolean;
begin
  Result := inherited DoCancelEdit;
end;

procedure TCustomVirtualExplorerTree.DoEdit;
begin
  inherited;
end;

function TCustomVirtualExplorerTree.ValidRootNamespace: Boolean;
begin
  Result := False;
  if Assigned(RootFolderNamespace) then
    Result := RootFolderNamespace.Valid;
  if not Result then
    DoInvalidRootNamespace;
end;

{ TViewList }

destructor TViewList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TObject( List[i]).Free;
  inherited;
end;

function TViewList.GetItems(Index: Integer): TView;
begin
  Result := TView( inherited Items[Index])
end;

procedure TViewList.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer;
  ReadVerFromStream: Boolean = False);
{ Loads the TView objects from the stream S.                                    }
var
  i: integer;
  ViewCount, NewViewIndex: integer;
begin
  inherited;
  S.ReadBuffer(ViewCount, SizeOf(ViewCount));
  for i := 0 to ViewCount - 1 do
  begin
    NewViewIndex := Add(TView.Create(''));
    TView( Items[NewViewIndex]).LoadFromStream(S, Version, ReadVerFromStream);
  end;
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TViewList.SetItems(Index: Integer; const Value: TView);
begin
  inherited Items[Index] := Value
end;

procedure TViewList.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer;
  WriteVerToStream: Boolean = False);
{ Write the TView objects to the stream S                                       }
var
  i: integer;
begin
  inherited;
  S.WriteBuffer(Count, SizeOf(Count));
  for i := 0 to Count - 1 do
    TView( Items[i]).SaveToStream(S, Version, WriteVerToStream);
end;

{ ----------------------------------------------------------------------------- }
{ TViewManager                                                                  }
{ ----------------------------------------------------------------------------- }

procedure TViewList.LoadFromFile(FileName: WideString; Version: integer;
  ReadVerFromStream: Boolean);
begin
  inherited;

end;

procedure TViewList.SaveToFile(FileName: WideString; Version: integer;
  ReadVerFromStream: Boolean);
begin
  inherited;

end;

{ TViewManager }
constructor TViewManager.Create;
begin
  Views := TViewList.Create;
end;

procedure TViewManager.DeleteView(ViewName: WideString);
{ Finds and removes the view that has the same name as ViewName.                }
var
  View: TView;
begin
  View := GetView(ViewName);
  if Assigned(View) then
  begin
    Views.Remove(View);
    View.Free
  end;
end;

destructor TViewManager.Destroy;
begin
  Views.Free;
  inherited;
end;

function TViewManager.GetView(ViewName: WideString): TView;
{ Locates the View by the name ViewName.                                        }
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Views.Count - 1 do
    if WideStrIComp(PWideChar( ViewName), PWideChar( Views.Items[i].ViewName)) = 0 then
      Result := Views[i]
end;

function TViewManager.GetViewCount: Integer;
begin
  Result := Views.Count
end;

function TViewManager.GetViewName(Index: integer): WideString;
{ Retrieves the name of a view at the passed index.                             }
begin
  Result := Views.Items[Index].ViewName
end;

procedure TViewManager.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
begin
  inherited;
  Views.LoadFromStream(S, Version, ReadVerFromStream);
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TViewManager.SetViewName(Index: integer; NewViewName: WideString);
{ Changes the name of the view at Index to NewViewName.                         }
begin
  Views.Items[Index].ViewName := NewViewName
end;

procedure TViewManager.ShowView(ViewName: WideString; VET: TCustomVirtualExplorerTree);
{ Finds the view associated with ViewName and restores it to the passed VET.    }
var
  View: TView;
begin
  VET.WaitCursor(True);
  try
    View := GetView(ViewName);
    if Assigned(View) then
      View.RestoreTree(VET, True, True);
  finally
    VET.WaitCursor(False)
  end
end;

procedure TViewManager.Snapshot(NewViewName: WideString; VET: TCustomVirtualExplorerTree);
{ Creates a new view with the name of NewViewName and save the current state of }
{ the passed VET.                                                               }
var
  View: TView;
  Reuse: Boolean;
begin
  Reuse := False;
  View := GetView(NewViewName);
  if not Assigned(View) then
    View := TView.Create(NewViewName)
  else
    Reuse := True;
  View.SaveTree(VET, True, True);
  if not Reuse then
    Views.Add(View);
end;

procedure TViewManager.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
begin
  inherited;
  Views.SaveToStream(S, Version, WriteVerToStream);
end;

{ ----------------------------------------------------------------------------- }
{ TView }
{ ----------------------------------------------------------------------------- }

constructor TView.Create(AViewName: WideString);
begin
  inherited Create;
  ViewName := AViewName;
end;

procedure TView.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
var
  Len: integer;
begin
  inherited;
  S.ReadBuffer(Len, SizeOf(Len));
  SetLength(FViewName, Len);
  S.ReadBuffer(PWideChar(FViewName)^, Len * 2);
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TView.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
var
  Len: integer;
begin
  inherited;
  Len := Length(ViewName);
  S.WriteBuffer(Len, SizeOf(Len));
  S.WriteBuffer(PWideChar( ViewName)^, Len * 2)
end;

procedure TViewManager.Clear;
var
  i: integer;
begin
  for i := Views.Count - 1 downto 0 do
  begin
    TObject( Views[i]).Free;
    Views.Delete(i);
  end;
end;

procedure TViewManager.LoadFromFile(FileName: WideString; Version: integer;
  ReadVerFromStream: Boolean);
begin
  inherited;
end;

procedure TViewManager.SaveToFile(FileName: WideString; Version: integer;
  ReadVerFromStream: Boolean);
begin
  inherited;   
end;

{ TLeafNodeList }

procedure TLeafNodeList.AddLeafNode(LeafPIDL: PItemIDList;
  IsExpanded: Boolean);
var
  Leaf: TLeafNode;
begin
  Leaf := TLeafNode.Create;
  if Assigned(Leaf) then
  begin
    Leaf.PIDL := PIDLMgr.CopyPIDL(LeafPIDL);
    Leaf.Expanded := IsExpanded;
    Leaf.Owner := Self;
    Add(Leaf);
  end;
end;

procedure TLeafNodeList.Clear;
var
  i: integer;
begin
  if not ShareNodes then
    for i := 0 to Count - 1 do
      TObject( Items[i]).Free;
  inherited;
end;

function TLeafNodeList.GetItems(Index: Integer): TLeafNode;
begin
  Result := TLeafNode( inherited Items[Index])
end;

procedure TLeafNodeList.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
var
  i: integer;
  ItemCount: integer;
begin
  inherited;
  S.ReadBuffer(ItemCount, SizeOf(ItemCount));
  for i := 0 to ItemCount - 1 do
  begin
    AddLeafNode(nil, False);
    Items[i].LoadFromStream(S, Version, ReadVerFromStream)
  end;
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TLeafNodeList.SetItems(Index: Integer;
  const Value: TLeafNode);
begin
  inherited Items[Index] := Value
end;

procedure TLeafNodeList.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
var
  i: integer;
  ItemCount: integer;
begin
  inherited;
  ItemCount := Count;
  S.WriteBuffer(ItemCount, SizeOf(ItemCount));
  for i := 0 to ItemCount - 1 do
    Items[i].SaveToStream(S, Version, WriteVerToStream)
end;


{ TLeafNode }

procedure TLeafNode.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
begin
  inherited;
  { Baseline Stream format PersistentVersion_0}
  S.ReadBuffer(FExpanded, SizeOf(FExpanded));
  FPIDL := PIDLMgr.LoadFromStream(S);
  { Add new stream data here }
  { if StreamVersion >= PersistentVersion_0 then }
  {   read new data                              }
end;

procedure TLeafNode.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
begin
  inherited;
  { No conditional statement needed, always write the stream in the latest format }
  S.WriteBuffer(FExpanded, SizeOf(FExpanded));
  PIDLMgr.SaveToStream(S, PIDL);
end;

{ TColumnManager }

constructor TColumnManager.Create(AnOwner: TCustomVirtualExplorerTree);
begin
  FVET := AnOwner;
end;

destructor TColumnManager.Destroy;
begin
  inherited;
end;

procedure TColumnManager.StoreColumnWidth(Column: integer);
begin
  ValidateColumnWidths;
  { Save the default widths }
  ColumnWidths[Column].Width := VET.Header.Columns[Column].Width;
end;

procedure TColumnManager.ToggleWidthAutoFit(ColumnIndex: integer);
begin
  ValidateColumnWidths;
  if (ColumnWidths[ColumnIndex].WidthView = cwv_Default) or
     (ColumnWidths[ColumnIndex].WidthView = cwv_Minimize)
   then
  begin
    StoreColumnWidth(ColumnIndex);
    VET.Header.Columns.Items[ColumnIndex].Width := VET.GetMaxColumnWidth(ColumnIndex);
    ColumnWidths[ColumnIndex].WidthView := cwv_AutoFit
  end else
  begin
    VET.Header.Columns.Items[ColumnIndex].Width := ColumnWidths[ColumnIndex].Width;
    ColumnWidths[ColumnIndex].WidthView := cwv_Default
  end
end;

procedure TColumnManager.ToggleWidthMinimize(ColumnIndex: integer);
begin
  ValidateColumnWidths;
  if (ColumnWidths[ColumnIndex].WidthView = cwv_Default) or
     (ColumnWidths[ColumnIndex].WidthView = cwv_AutoFit)
  then begin
    StoreColumnWidth(ColumnIndex);
    VET.Header.Columns.Items[ColumnIndex].Width := 0;
    ColumnWidths[ColumnIndex].WidthView := cwv_Minimize
  end else
  begin
    VET.Header.Columns.Items[ColumnIndex].Width := ColumnWidths[ColumnIndex].Width;
    ColumnWidths[ColumnIndex].WidthView := cwv_Default
  end
end;

procedure TColumnManager.ValidateColumnWidths;
begin
  if Length(FColumnWidths) < VET.Header.Columns.Count then
    SetLength(FColumnWidths, VET.Header.Columns.Count);
end;

{ TVETColors }

procedure TVETColors.Assign(Source: TPersistent);
begin
  if Source is TVETColors then
    FVETColors := TVETColors(Source).FVETColors
  else
    inherited
end;

procedure TVETColors.AssignTo(Dest: TPersistent);
begin
  if Dest is TVETColors then
    TVETColors(Dest).FVETColors := FVETColors;
end;                 

constructor TVETColors.Create(AnOwner: TCustomVirtualExplorerTree);
begin
  Owner := AnOwner;
  FVETColors[0] := ExplorerTextColor(vetcCompressed);
  FVETColors[1] := clExplorerFolder;
  FVETColors[2] := clExplorerFile;
  FVETColors[3] :=  ExplorerTextColor(vetcEncrypted);
end;

destructor TVETColors.Destroy;
begin
  inherited;
end;

function TVETColors.GetVETColor(const Index: Integer): TColor;
begin
  Result := FVETColors[Index];
end;

procedure TVETColors.SetVETColor(const Index: Integer;
  const Value: TColor);
begin
  FVETColors[Index] := Value;
  if Assigned(Owner) then
    Owner.Invalidate
end;

{ TContextMenuManager }

function TContextMenuManager.CalculatePopupPoint(Node: PVirtualNode): TPoint;
var
  R: TRect;
begin
  ZeroMemory(@Result, SizeOf(Result));
  if Assigned(Node) then
  begin
    R := Owner.GetDisplayRect(Node, -1, True);
    R.TopLeft := Owner.ClientToScreen(R .TopLeft);
    R.BottomRight := Owner.ClientToScreen(R.BottomRight);
    Result.x := R.Left + Owner.Images.Width div 2;
    Result.y := R.Top + (R.Bottom - R.Top) div 2
  end
end;

procedure TContextMenuManager.ContextMenuAfterCmdCallback(
  Namespace: TNamespace; Verb: WideString; MenuItemID: Integer;
  Successful: Boolean);
begin
  if Assigned(Owner) then
    Owner.DoContextMenuAfterCmd(Namespace, Verb, MenuItemID, Successful);
end;

procedure TContextMenuManager.ContextMenuCmdCallback(Namespace: TNamespace;
  Verb: WideString; MenuItemID: Integer; var Handled: Boolean);
begin
  if Assigned(Owner) then
    Handled := Owner.DoContextMenuCmd(Namespace, Verb, MenuItemID)
  else
    Handled := False
end;

procedure TContextMenuManager.ContextMenuShowCallback(
  Namespace: TNamespace; Menu: hMenu; var Allow: Boolean);

  function IndexIsSeparator(Index: Integer): Boolean;
  var
    MenuInfo: TMenuItemInfo;
  begin
    ZeroMemory(@MenuInfo, SizeOf(MenuInfo));
    MenuInfo.cbSize := SizeOf(MenuInfo);
    MenuInfo.fMask := MIIM_TYPE;
    GetMenuItemInfo(Menu, Index, True, MenuInfo);
    Result :=  MenuInfo.fType and MFT_SEPARATOR  <> 0
  end;

var
  i: Integer;
  S: string;
  Done: Boolean;

begin
  if Assigned(Owner) then
  begin
    if toRemoveContextMenuShortCut in Owner.TreeOptions.VETMiscOptions then
    begin
      Done := False;
      i := 0;
      while not Done and (i < GetMenuItemCount(Menu)) do
      begin
        S := Namespace.ContextMenuVerb(GetMenuItemID(Menu, i));
        if StrComp(PChar(S), 'link') = 0 then
        begin
          DeleteMenu(Menu, i, MF_BYPOSITION);
          if IndexIsSeparator(i - 1) then
          begin
            if (GetMenuItemCount(Menu) = i) or IndexIsSeparator(i) then
              DeleteMenu(Menu, i - 1, MF_BYPOSITION)
          end;
          Done := True
        end;
        Inc(i)
      end
    end;
    Allow := Owner.DoContextMenuShow(Namespace, Menu);
  end else
    Allow := False
end;

constructor TContextMenuManager.Create(AnOwner: TCustomVirtualExplorerTree);
begin
  FOwner := AnOwner;
end;

procedure TContextMenuManager.HandleContextMenuMsg(Msg, wParam,
  lParam: Integer; var Result: LRESULT);
var
  NS: TNamespace;
begin
  if Assigned(Owner) then
    if Owner.ValidateNamespace(ActiveNode, NS) then
      NS.HandleContextMenuMsg(Msg, wParam, lParam, Result);
end;

procedure TContextMenuManager.MenuSelect(Msg, wParam, lParam: integer;
  var Result: LRESULT);
var
  NS: TNamespace;
  ChildMenu: hMenu;
begin
  if Assigned(Owner) then
  begin
    if MenuShown then
      if Owner.ValidateNamespace(ActiveNode, NS) then
      begin
        if HiWord(Longword( wParam)) and MF_POPUP <> 0 then
          ChildMenu := GetSubMenu(LongWord( lParam), LoWord(Longword( wParam)))
        else
          ChildMenu := 0;
        Owner.DoContextMenuSelect(NS, LoWord(Longword( wParam)), ChildMenu,
          HiWord(Longword( wParam)) and MF_MOUSESELECT <> 0);
      end
  end
end;

procedure TContextMenuManager.ResetState;
begin
  FPreviousSelectedNode := nil;
  FPreviousFocusNode := nil;
  FActiveNode := nil;
  IsEditingNode := False;
  FMenuPending := False;
end;

procedure TContextMenuManager.RightClick(XPos, YPos: Integer;
  ButtonState: TButtonState; Coordinates: TCoordType);

// This handles the Explorer like dealing with a right click on other nodes when
// on or several nodes are already selected

var
  Pt: TPoint;
  Info: THitInfo;
  Selection: PVirtualNode;
begin
  if Assigned(Owner) then
  begin
    if ButtonState = bsDown then
    begin
      ResetState;
      Pt.x := XPos;
      Pt.y := YPos;
      if Coordinates = ctScreen then
        Pt := Owner.ScreenToClient(Pt);
      Owner.GetHitTestInfoAt(Pt.x, Pt.y, True, Info);
      if Assigned(Info.HitNode) and ((Info.HitPositions * [hiOnItemLabel, hiOnNormalIcon] <> []) or
        (toFullRowContextMenuActivate in Owner.TreeOptions.VETShellOptions)) then
      begin
        FMenuPending := True;
        FActiveNode := Info.HitNode;
        FPreviousSelectedNode := Owner.GetFirstSelected;
        if not Assigned(FPreviousSelectedNode) then
          FPreviousFocusNode := Owner.FocusedNode
        else
          FPreviousFocusNode := FPreviousSelectedNode;
        // If the node is already selected then we don't need to do much
        if not Owner.Selected[Info.HitNode] then
        begin
          if toRightButtonSelect in Owner.TreeOptions.VETMiscOptions then
          begin
            // Listview is always Right Click Select
            FPreviousSelectedNode := Info.HitNode;
            FPreviousFocusNode := Info.HitNode;
            Owner.ClearSelection;
            Owner.Selected[FActiveNode] := True;
            Owner.FocusedNode := FActiveNode
          end else
          begin
            Owner.ClearSelection;
            Owner.Selected[FActiveNode] := True;
            Owner.FocusedNode := FActiveNode
          end
        end
      end
    end else  // ButtonState = bsUp
    try
      if not IsEditingNode then
      begin
        if toRightButtonSelect in Owner.TreeOptions.VETMiscOptions then
        begin
          Selection := Owner.GetFirstSelected;
          if Assigned(Selection) then
          begin
            // Force a Node Change event since we have held it off with the
            // MenuPending Property
            Owner.Selected[Selection] := False;
            FMenuPending := False;
            Owner.Selected[Selection] := True;
          end;
          ResetState;
        end else
        begin
          if not Owner.Selected[PreviousSelectedNode] then
          begin
            Owner.ClearSelection;
            if Assigned(PreviousSelectedNode) then
              Owner.Selected[PreviousSelectedNode] := True;
            if Assigned(PreviousFocusNode) then
              Owner.FocusedNode := PreviousFocusNode;
          end
        end
      end
    finally
      ResetState;
    end
  end
end;

procedure TContextMenuManager.ShowContextMenu(NS: TNamespace; Position: PPoint = nil);
begin
  if Assigned(NS) then
  begin
    FMenuShown := True;
    try
      if Owner.SelectedCount = 1 then
        NS.ShowContextMenu(Owner, ContextMenuCmdCallback, ContextMenuShowCallback,
          ContextMenuAfterCmdCallback, Position, Owner.ShellContextSubMenu,
          Owner.ShellContextSubMenuCaption)
      else
        NS.ShowContextMenuMulti(Owner, ContextMenuCmdCallback,
            ContextMenuShowCallback, ContextMenuAfterCmdCallback,
            Owner.SelectedToNamespaceArray, Position,
            Owner.ShellContextSubMenu, Owner.ShellContextSubMenuCaption,
            NS);
    finally
      FMenuShown := False
    end
  end
end;

function TContextMenuManager.ShowContextMenuOfActiveNode(Point: TPoint): Boolean;
var
  NS: TNamespace;
begin
  Result := False;
  if Enabled and Owner.ValidateNamespace(ActiveNode, NS) then
  begin
    ShowContextMenu(NS, @Point);
    Result := True
  end;
end;

function TContextMenuManager.ShowContextMenuOfSelectedItem: Boolean;
// Shows the context menu of any item that is selected with it left aligned to the
// center of the left of the node + 1/2 of the image width.  Vertically aligned
// the the center of the node.

var
  NS: TNamespace;
  Point: TPoint;
  Node: PVirtualNode;
begin
  Result := False;
  Node := Owner.GetFirstSelected;
  if Enabled and Assigned(Node) then
  begin
    Node := Owner.GetFirstSelected;
    Point := CalculatePopupPoint(Node);
    if Owner.ValidateNamespace(Node, NS) then
    begin
      ShowContextMenu(NS, @Point);
      Result := True
    end
  end;
end;

{ TVETDataObject }

function TVETDataObject.DAdvise(const FormatEtc: TFormatEtc; advf: Integer;
  const advSink: IAdviseSink; out dwConnection: Integer): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.DAdvise(FormatEtc, advf, advSink, dwConnection);
  if Result <> S_OK then
    Result := inherited DAdvise(FormatEtc, advf, advSink, dwConnection);
end;

function TVETDataObject.DUnadvise(dwConnection: Integer): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.DUnadvise(dwConnection); 
  if Result <> S_OK then
    Result := inherited DUnadvise(dwConnection);
end;

function TVETDataObject.EnumDAdvise(
  out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
      Result := ShellDataObject.EnumDAdvise(enumAdvise);
  if Result <> S_OK then
    Result := inherited EnumDAdvise(enumAdvise);
end;

function TVETDataObject.EnumFormatEtc(Direction: Integer;
  out EnumFormatEtc: IEnumFormatEtc): HResult;
var
  Temp: VirtualTrees.TFormatEtcArray;
  TempEnumFormatEtc: IEnumFormatEtc;
  pceltFetched: Longint;
  elt: TeltArray;
begin
  Result := E_FAIL;
  Temp := FormatEtcArray;
  if Assigned(ShellDataObject) then
  begin
    if Succeeded(ShellDataObject.EnumFormatEtc(0, TempEnumFormatEtc)) then
    begin
      TempEnumFormatEtc.Reset;
      while TempEnumFormatEtc.Next(1, elt, @pceltFetched) = NOERROR do
      begin
        SetLength(Temp, Length(Temp) + 1);
        Temp[Length(Temp) - 1] := elt[0]
      end
    end
  end;
  EnumFormatEtc := TEnumFormatEtc.Create(Owner, Temp);
  if Assigned(EnumFormatEtc) then
    Result := S_OK
end;

function TVETDataObject.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcOut: TFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.GetCanonicalFormatEtc(FormatEtc, FormatEtcOut);
  if Result <> S_OK then
    Result := inherited GetCanonicalFormatEtc(FormatEtc, FormatEtcOut);
end;

function TVETDataObject.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.GetData(FormatEtcIn, Medium);
  if Result <> S_OK then
    Result := inherited GetData(FormatEtcIn, Medium);
end;

function TVETDataObject.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.GetDataHere(FormatEtc, Medium);
  if Result <> S_OK then
    Result := inherited GetDataHere(FormatEtc, Medium);
end;

function TVETDataObject.QueryGetData(const FormatEtc: TFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(ShellDataObject) then
    Result := ShellDataObject.QueryGetData(FormatEtc);
  if Result <> S_OK then
    Result := inherited QueryGetData(FormatEtc);
end;

function TVETDataObject.SetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; DoRelease: BOOL): HResult;
begin
  Result := inherited SetData(FormatEtc, Medium, DoRelease);
end;

{ TVETHeader }

function TVETHeader.CanWriteColumns: Boolean;
begin
  Result := not (TCustomVirtualExplorerTree( Treeview).ColumnDetails = cdShellColumns)
end;

{ TVETColumn }

constructor TVETColumn.Create(Collection: TCollection);
begin
  inherited;
  FColumnDetails := cdFileName;
  Text := STR_COLUMN_NAMES[0];
  Width := 200;
end;

procedure TVETColumn.SetColumnDetails(const Value: TColumnDetails);
var
  VET: TCustomVirtualExplorerTree;

  procedure InitColumn(AText: WideString; AWidth: integer);
  begin
    Text := AText;
    if not(csLoading in Owner.Header.Treeview.ComponentState) then
      Width := AWidth;
    if (Value = cdSize) and (toRightAlignSizeColumn in VET.TreeOptions.VETShellOptions) then
      Alignment := taRightJustify
  end;

begin
  if Value <> FColumnDetails then
  begin
    VET := TCustomVirtualExplorerTree(Owner.Header.Treeview);
    { Shell generated header is automatic, if messed with exit mode }
    if not(VET.ColumnDetails = cdShellColumns) then
    begin
      FColumnDetails := Value;
      case FColumnDetails of
        cdFileName   : InitColumn(STR_COLUMN_NAMES[0], VET_ColumnWidths[0]);
        cdSize       : InitColumn(STR_COLUMN_NAMES[1], VET_ColumnWidths[1]);
        cdType       : InitColumn(STR_COLUMN_NAMES[2], VET_ColumnWidths[2]);
        cdModified   : InitColumn(STR_COLUMN_NAMES[3], VET_ColumnWidths[3]);
        cdAttributes : InitColumn(STR_COLUMN_NAMES[4], VET_ColumnWidths[4]);
        cdAccessed   : InitColumn(STR_COLUMN_NAMES[6], VET_ColumnWidths[5]);
        cdCreated    : InitColumn(STR_COLUMN_NAMES[5], VET_ColumnWidths[6]);
        cdPath       : InitColumn(STR_COLUMN_NAMES[7], 150);
        cdDOSName    : InitColumn(STR_COLUMN_NAMES[8], 80);
        cdCustom     : InitColumn(STR_COLUMN_NAMES[9], 80);
     end;
     VET.Invalidate;
    end else
      FColumnDetails := cdCustom
  end
end;

{ TCustomVirtualExplorerTreeOptions }

procedure TCustomVirtualExplorerTreeOptions.Assign(Source: TPersistent);
var
  VETOptions: TCustomVirtualExplorerTreeOptions;
begin
  inherited;
  if Source is TCustomVirtualExplorerTreeOptions then
  begin
    VETOptions := TCustomVirtualExplorerTreeOptions(Source);
    AutoOptions := VETOptions.AutoOptions;
    SelectionOptions := VETOptions.SelectionOptions;
    VETFolderOptions := VETOptions.VETFolderOptions;
    VETShellOptions := VETOptions.VETShellOptions;
    VETMiscOptions := VETOptions.VETMiscOptions;
    VETImageOptions := VETOptions.VETImageOptions;
    VETSyncOptions := VETOptions.VETSyncOptions;
  end;
end;

procedure TCustomVirtualExplorerTreeOptions.AssignTo(Dest: TPersistent);
var
  VETOptions: TCustomVirtualExplorerTreeOptions;
begin
  inherited;
  if Dest is TCustomVirtualExplorerTreeOptions then
  begin
    VETOptions := TCustomVirtualExplorerTreeOptions(Dest);
    VETOptions.AutoOptions := AutoOptions;
    VETOptions.SelectionOptions := SelectionOptions;
    VETOptions.VETFolderOptions := VETFolderOptions;
    VETOptions.VETShellOptions := VETShellOptions;
    VETOptions.VETMiscOptions := VETMiscOptions;
    VETOptions.VETImageOptions := VETImageOptions;
    VETOptions.VETSyncOptions := VETSyncOptions;
  end
end;

function TCustomVirtualExplorerTreeOptions.GetAutoOptions: TVTAutoOptions;
begin
  Result := inherited AutoOptions
end;

function TCustomVirtualExplorerTreeOptions.GetOwner: TCustomVirtualExplorerTree;
begin
  Result := inherited Owner as TCustomVirtualExplorerTree;
end;

function TCustomVirtualExplorerTreeOptions.GetSelectionOptions: TVTSelectionOptions;
begin
  Result := inherited SelectionOptions
end;

procedure TCustomVirtualExplorerTreeOptions.SetAutoOptions(
  const Value: TVTAutoOptions);
var
  Temp: TVTAutoOptions;
begin
  Temp := Value;
  Exclude(Temp, toAutoSort);
  inherited AutoOptions := Temp;
end;

procedure TCustomVirtualExplorerTreeOptions.SetSelectionOptions(
  const Value: TVTSelectionOptions);
var
  Temp: TVTSelectionOptions;
begin
  Temp := Value;
  if (toMultiSelect in Temp) and not ForceMultiLevelMultiSelect then
  begin
    Exclude(Temp, toLevelSelectConstraint);
    Include(Temp, toSiblingSelectConstraint);
  end;
 // Exclude(Temp, toRightClickSelect);       {JIM}
  inherited SelectionOptions := Temp
end;

procedure TCustomVirtualExplorerTreeOptions.SetVETFolderOptions(
  const Value: TVETFolderOptions);

  function BitChanged(TestOptions1, TestOptions2: TVETFolderOptions; OptionBit: TVETFolderOption): Boolean;
  begin
    Result := ((OptionBit in TestOptions1) and not(OptionBit in TestOptions2)) or
              (not(OptionBit in TestOptions1) and (OptionBit in TestOptions2))
  end;

var
  OldOptions: TVETFolderOptions;
  NeedRebuild: Boolean;
begin
  OldOptions := VETFolderOptions;
  FVETFolderOptions := Value; // Set this in case any function call rely on it
  if Assigned(Owner) then
  begin
    if not (csLoading in Owner.ComponentState) then
    begin
      NeedRebuild := False;
      if BitChanged(Value, OldOptions, toFoldersExpandable) then
         NeedRebuild := True;
      if BitChanged(Value, OldOptions, toHideRootFolder) then
         NeedRebuild := True;
      if BitChanged(Value, OldOptions, toForceShowMyDocuments) then
         NeedRebuild := True;
      if BitChanged(Value, OldOptions, toForceHideRecycleBin) then
         NeedRebuild := True;
      if BitChanged(Value, OldOptions, toThreadedExpandMark) then
        Owner.ThreadedExpandMarkEnabled := toThreadedExpandMark in Value;
      if NeedRebuild then
        Owner.RebuildRootNamespace;
    end;
  end
end;

procedure TCustomVirtualExplorerTreeOptions.SetVETImageOptions(
  const Value: TVETImageOptions);

  function BitChanged(TestOptions1, TestOptions2: TVETImageOptions; OptionBit: TVETImageOption): Boolean;
  begin
    Result := ((OptionBit in TestOptions1) and not(OptionBit in TestOptions2)) or
              (not(OptionBit in TestOptions1) and (OptionBit in TestOptions2))
  end;

var
  OldOptions: TVETImageOptions;
begin
  OldOptions := VETImageOptions;
  FVETImageOptions := Value; // Set this in case any function call rely on it
  if Assigned(Owner) then
  begin
    if BitChanged(Value, OldOptions, toImages) then
    begin
      if toImages in Value then
        Owner.Images := SmallSysImages
      else
        Owner.Images := nil
    end;

    if BitChanged(Value, OldOptions, toLargeImages) then
      if toLargeImages in Value then
        Owner.Images := LargeSysImages
      else
       Owner.Images := SmallSysImages;

    if not (csLoading in Owner.ComponentState) then
      Owner.RebuildRootNamespace;

    if BitChanged(Value, OldOptions, toThreadedImages) then
    begin
      if toThreadedImages in Value then
        Owner.ThreadedImagesEnabled := True
      else
        Owner.ThreadedImagesEnabled := False;
    end
  end
end;

procedure TCustomVirtualExplorerTreeOptions.SetVETMiscOptions(const Value: TVETMiscOptions);

  function BitChanged(TestOptions1, TestOptions2: TVETMiscOptions; OptionBit: TVETMiscOption): Boolean;
  begin
    Result := ((OptionBit in TestOptions1) and not(OptionBit in TestOptions2)) or
              (not(OptionBit in TestOptions1) and (OptionBit in TestOptions2))
  end;

var
  OldOptions: TVETMiscOptions;
begin
  OldOptions := VETMiscOptions;
  FVETMiscOptions := Value; // Set this in case any function call rely on it
  begin
    if BitChanged(FVETMiscOptions, OldOptions, toTrackChangesinMappedDrives) then
    begin
      // The ChangeNotifier must be enabled for this to work
      // Also this check must come before the change notifer check!
      if (toTrackChangesinMappedDrives in Value) then
      begin
        if not (toChangeNotifierThread in OldOptions) then
          // Setting this here will force the next BitChanged to be true so it will
          // actually set the notifier there
          Include(FVETMiscOptions, toChangeNotifierThread);
      end
    end;

    if BitChanged(FVETMiscOptions, OldOptions, toChangeNotifierThread) then
    begin
      if toChangeNotifierThread in Value then
      begin
        Owner.ChangeNotifierEnabled := True;
        if IsWinVista then
          Owner.FileObjects := Owner.FileObjects + [foEnableAsync];    // Eliminate freezes during enumeration
      end else
      begin
        Owner.ChangeNotifierEnabled := False;
         if IsWinVista then
          Owner.FileObjects := Owner.FileObjects - [foEnableAsync];    // Eliminate freezes during enumeration
        Exclude(FVETMiscOptions, toTrackChangesinMappedDrives)
      end
    end
  end
end;

procedure TCustomVirtualExplorerTreeOptions.SetVETShellOptions(
  const Value: TVETShellOptions);

  function BitChanged(TestOptions1, TestOptions2: TVETShellOptions; OptionBit: TVETShellOption): Boolean;
  begin
    Result := ((OptionBit in TestOptions1) and not(OptionBit in TestOptions2)) or
              (not(OptionBit in TestOptions1) and (OptionBit in TestOptions2))
  end;

var
  OldOptions: TVETShellOptions;
begin
  OldOptions := VETShellOptions;
  FVETShellOptions := Value; // Set this in case any function call relies on it
  if Assigned(Owner) then
  begin
    if BitChanged(Value, OldOptions, toContextMenus) then
    begin
      if Assigned(Owner.ContextMenuManager) then
      begin
        if toContextMenus in Value then
          Owner.ContextMenuManager.Enabled := True
        else
          Owner.ContextMenuManager.Enabled := False;
      end
    end;
    if BitChanged(Value, OldOptions, toRightAlignSizeColumn) then
      Owner.RebuildRootNamespace;
  end
end;

{ TVirtualExplorerListview}

function TVirtualExplorerListview.ShowBkGndContextMenu(Point: TPoint): Integer;
begin
  if Assigned(BackGndMenu) then
  begin
    BackGndMenu.ShowContextMenu(Self, RootFolderNamespace, @Point);
    Result := 1; // We handled it.
  end else
    Result := 0
end;


procedure TVirtualExplorerListview.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVirtualExplorerListview then
    FBackBrowseRoot := TVirtualExplorerListview(Source).FBackBrowseRoot.Clone(True)
end;

procedure TVirtualExplorerListview.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TVirtualExplorerListview then
    TVirtualExplorerListview(Dest).Assign(Self)
end;

function TVirtualExplorerListview.BrowseToByPIDL(APIDL: PItemIDList;
  ExpandTarget, SelectTarget, SetFocusToVET,
  CollapseAllFirst: Boolean; ShowAllSiblings: Boolean = True): Boolean;
var
  NS: TNamespace;
  P, OldPIDL: PItemIDList;
begin
  if OkToBrowseTo(APIDL) then
  begin
    // If the path was a file strip the PIDL back to the folder
    NS := TNamespace.Create(APIDL, nil);
    try
      NS.FreePIDLOnDestroy := False;
      if not NS.Folder then
        P := NS.Parent.AbsolutePIDL
      else
        P := APIDL;

      // Don't change the rootfolder if it's not necessary
      if not PIDLMgr.EqualPIDL(FRootFolderCustomPIDL, P) then
      begin
        OldPIDL := FRootFolderCustomPIDL;
        FRootFolderCustomPIDL := nil;
        RootFolderCustomPIDL := P;
        if Assigned(RootFolderCustomPIDL) then
          PIDLMgr.FreeAndNilPIDL(OldPIDL)
        else
          FRootFolderCustomPIDL := OldPIDL;
      end;

      // If the path was a file then select the file if desired
      if not NS.Folder and SelectTarget then
        WalkPIDLToNode(NS.AbsolutePIDL, SelectTarget, False, False, ShowAllSiblings);
      Result := Assigned(RootFolderCustomPIDL);
    finally
      NS.Free
    end
  end else
    Result := False
end;

function TVirtualExplorerListview.BrowseToNextLevel: Boolean;
var
  NS: TNamespace;
  PIDL: PItemIDList;
begin
  Result := False;
  if ValidateNamespace(GetFirstSelected, NS) then
    if NS.Folder then
    begin
      PIDL := PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
      try
        Result := BrowseToByPIDL(PIDL, False, False, False, False)
      finally
        PIDLMgr.FreePIDL(PIDL);
      end;
    end
end;

procedure TVirtualExplorerListview.BrowseToPrevLevel;
var
  PIDL: PItemIDList;
begin
  if Assigned(FRootFolderNamespace) then
  begin
    if not FRootFolderNamespace.IsDesktop then
    begin
      PIDL := PIDLMgr.CopyPIDL(FRootFolderNamespace.AbsolutePIDL);
      try
        PIDLMgr.StripLastID(PIDL);
        BrowseToByPIDL(PIDL, False, False, False, False)
      finally
        PIDLMgr.FreePIDL(PIDL);
      end
    end
  end;
end;

procedure TVirtualExplorerListview.ChangeLinkDispatch;
var
  NS: TNamespace;
begin
  if ValidateNamespace(RootNode, NS) then
    VETChangeDispatch.DispatchChange(Self, NS.AbsolutePIDL);
end;

procedure TVirtualExplorerListview.ChangeLinkFreeing(ChangeLink: IVETChangeLink);
begin
  inherited;
  if Assigned(ChangeLink) then
  {$IFDEF EXPLORERTREEVIEW_L}
    if (ChangeLink.ChangeLinkClient = Self) and (ChangeLink.ChangeLinkServer = FVirtualExplorerTreeview) then
      FVirtualExplorerTreeview := nil;
  {$ENDIF}
end;

procedure TVirtualExplorerListview.CreateWnd;
begin
  inherited;
  // BackBrowse Clones the Namespace
  BackBrowseRoot := FRootFolderNamespace;
end;

destructor TVirtualExplorerListview.Destroy;
begin
  FreeAndNil(FBackBrowseRoot);
  inherited;
end;

function TVirtualExplorerListview.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  // Don't let the Listview use Open Folder images;
  if Kind = ikSelected then
    Kind := ikNormal;
  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

function TVirtualExplorerListview.DoKeyAction(var CharCode: Word;
  var Shift: TShiftState): Boolean;
var
  Handled: Boolean;
begin
  Result := True;
  case CharCode of
    VK_BACK:
      begin
        BrowseToPrevLevel;
        Result := False;  // Override VT's BackSpace handling
      end;
    VK_RETURN:
      begin
        if SelectedCount = 1 then
        begin
          // The tree will not know of a newly create folder so  even if the LV
          // browses into the new folder before refresh when the notify come by and
          // refreshed the treeview it will restore itself to the original folder
          // not into the new folder
        {$IFDEF EXPLORERTREEVIEW_L}
          if Assigned(VirtualExplorerTreeview) then
            VirtualExplorerTreeview.RefreshNode(VirtualExplorerTreeview.FocusedNode);
        {$ENDIF}
          DoShellExecute(GetFirstSelected);
        end
      end;
    Ord('V'), Ord('v'):
      begin
        // Look for Ctrl + 'V' paste that TCustomVirtualExplorerTree would not
        // have handled.
        // Explorer ALWAYS pastes into the current view???? Weird but....
        if ssCtrl in Shift then
        begin
          WaitCursor(True);
          try
            Handled := False;
            DoClipboardPaste(Handled);
            if not Handled then
              PasteFromClipboard;
          finally
            // Don't let ancestors process this keystroke
            Result := False;
            WaitCursor(False)
          end
        end
      end;
  end;
  if Result then
      Result := inherited DoKeyAction(CharCode, Shift);
end;

procedure TVirtualExplorerListview.DoShellExecute(Node: PVirtualNode);
var
  NS: TNamespace;
  DefaultExecution: Boolean;
begin
  DefaultExecution := True;
  if ValidateNamespace(Node, NS) then
    if (toBrowseExecuteFolder in TreeOptions.VETMiscOptions) and NS.Folder then
      if WideStrIComp(PWideChar(NS.Extension), '.zip') = 0 then
      begin
        if toBrowseExecuteZipFolder in TreeOptions.VETMiscOptions then
          DefaultExecution := not BrowseToNextLevel; // This works because the click has selected the node
      end else
        DefaultExecution := not BrowseToNextLevel; // This works because the click has selected the node
  if DefaultExecution then
    inherited;
end;

procedure TVirtualExplorerListview.LoadDefaultOptions;
begin
  Header.Height := 17;
  Header.Options := DefaultExplorerListHeaderOptions;
  Indent := 0;
  Margin := 4;
  TreeOptions.AutoOptions := DefaultExplorerListAutoOptions;
  TreeOptions.MiscOptions := DefaultExplorerListMiscOptions;
  TreeOptions.PaintOptions := DefaultExplorerListPaintOptions;
  TreeOptions.SelectionOptions := DefaultExplorerListVETSelectionOptions;
  TreeOptions.VETFolderOptions := DefaultExplorerListVETFolderOptions;
  TreeOptions.VETImageOptions := DefaultExplorerListVETImageOptions;
  TreeOptions.VETShellOptions := DefaultExplorerListVETShellOptions;
  TreeOptions.VETMiscOptions := DefaultExplorerListVETMiscOptions;
  TreeOptions.VETSyncOptions := DefaultExplorerListVETSyncOptions;
  FFileObjects := DefaultExplorerListFileObjects;
  ColumnDetails := cdShellColumns
end;

function TVirtualExplorerListview.OkToBrowseTo(PIDL: PItemIDList): Boolean;

// check the PIDL to make sure it does not violate the BackBrowseRoot namespace limit
// for browsing up the ShellTree.

begin
  Result := True;
  if Assigned(BackBrowseRoot) and (toListviewLimitBrowseToRoot in TreeOptions.VETMiscOptions) then
  // If the backbrowse lock is unassigned everything goes
  begin
  {$IFDEF EXPLORERTREEVIEW_L}
    // Make the listview respect any other shell controls Root
    if Assigned(VirtualExplorerTreeview) then
    begin
      if Assigned(VirtualExplorerTreeview.FRootFolderNameSpace) then
        Result := ILIsParent(VirtualExplorerTreeview.FRootFolderNameSpace.AbsolutePIDL, PIDL, False)
    end else
  {$ENDIF}
  {$IFDEF EXPLORERTREE_L}
    if Assigned(VirtualExplorerTree) then
    begin
      if Assigned(VirtualExplorerTree.FRootFolderNameSpace) then
        Result := ILIsParent(VirtualExplorerTree.FRootFolderNameSpace.AbsolutePIDL, PIDL, False)
    end else
  {$ENDIF}
      Result := ILIsParent(FBackBrowseRoot.AbsolutePIDL, PIDL, False)
  end
end;

function TVirtualExplorerListview.PasteFromClipboard: Boolean;
var
  NS: TNamespace;
  NSA: TNamespaceArray;
begin
  // Explorer ALWAYS pastes into the current view???? Weird but....
  Result := False;
  if ValidateNamespace(RootNode, NS) then
  begin
    SetLength(NSA, 1);
    NSA[0] := NS;
    NS.Paste(Self, NSA);
    Result := True;
  end;
  //don't call inherited
end;

procedure TVirtualExplorerListview.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = BackGndMenu then
      BackGndMenu := nil
  end
end;

procedure TVirtualExplorerListview.RebuildRootNamespace;
begin
  if RebuildRootNamespaceCount = 0 then
  begin
    { Sort of a Begin/EndUpdate to hold off rebuilding }
    inherited;
    if ColumnDetails = cdShellColumns then
      RebuildShellHeader(FRootFolderNamespace);
  end;
end;

procedure TVirtualExplorerListview.ReReadAndRefreshNode(Node: PVirtualNode;
  SortNode: Boolean);
begin
  // Don't waste time on subnode refreshes that don't matter
  if Node = RootNode then
    inherited;   
end;

procedure TVirtualExplorerListview.SelectedFilesPaste(
  AllowMultipleTargets: Boolean);
{ Allows multiple folders to be selected and the same items to be pasted into   }
{ them from the clipboard if AllowMultipleTargets is true.                      }

var
  Node: PVirtualNode;
  NS: TNamespace;
  NSA: TNamespaceArray;
  Done: Boolean;
begin
  Done := False;
  if not AllowMultipleTargets and (SelectedCount > 1) then Exit;
  if AllowMultipleTargets or (SelectedCount = 1) then
  begin
    SetLength(NSA, 1);
    Node := GetFirstSelected;
    if Assigned(Node) then
    begin
      while not Done and Assigned(Node) do
      begin
        if ValidateNamespace(Node, NS) then
        begin
          NSA[0] := NS;
          NS.Paste(Self, NSA)
        end;
        if not AllowMultipleTargets then
          Done := True
        else
          Node := GetNextSelected(Node)
      end
    end else
    begin
      if ValidateNamespace(RootNode, NS) then
      begin
        NSA[0] := NS;
        NS.Paste(Self, NSA)
      end
    end
  end
end;

procedure TVirtualExplorerListview.SetBackBrowseRoot(const Value: TNamespace);
begin
  if FBackBrowseRoot <> Value then
  begin
    FreeAndNil(FBackBrowseRoot);
    if Assigned(Value) then
      FBackBrowseRoot := Value.Clone(True);
  end
end;

{$IFDEF EXPLORERTREEVIEW_L}
procedure TVirtualExplorerListview.SetVirtualExplorerTreeview(
  const Value: TVirtualExplorerTreeview);
begin
  if FVirtualExplorerTreeview <> Value then
  begin
    if Assigned(FVirtualExplorerTreeview) then
      VETChangeDispatch.UnRegisterChangeLink(FVirtualExplorerTreeview, Self, utLink );
    FVirtualExplorerTreeview := Value;
    if Assigned(FVirtualExplorerTreeview) then
      VETChangeDispatch.RegisterChangeLink(FVirtualExplorerTreeview, Self, ChangeLinkChanging, ChangeLinkFreeing);
  end;
end;
{$ENDIF}

procedure TVirtualExplorerListview.ShellExecuteFolderLink(NS: TNamespace;
  WorkingDir, CmdLineArgument: WideString);
var
  ShellLink: TVirtualShellLink;
  LinkTarget: WideString;
begin
  if Assigned(NS) then
  begin
    if NS.Link then
    begin
      ShellLink := TVirtualShellLink.Create(nil);
      try
        ShellLink.ReadLink(NS.NameParseAddress);
        LinkTarget := ShellLink.TargetPath;
        if WideDirectoryExists(LinkTarget) and OkToBrowseTo(ShellLink.TargetIDList) then
          with TreeOptions do
            if BrowseToByPIDL(ShellLink.TargetIDList, toExpandTarget in VETSyncOptions,
              toSelectTarget in VETSyncOptions, False, toCollapseTargetFirst in VETSyncOptions) then
      finally
        ShellLink.Free
      end
    end
  end
end;

procedure TVirtualExplorerListview.WMShellNotify(var Msg: TMessage);
begin
  // I use to filter the messages somewhat here but the new way the undocumented
  // notifications change from OS to OS it has gotten to difficult to keep
  // it working correctly all the time.  XP has a major change, any desktop
  // folder or sub folder only gets a "Desktop" refresh, likely why the treeview
  // in Explorer flickers a lot.
  inherited;
end;

{ TVirtualExplorerTreeview }


procedure TVirtualExplorerTreeview.ChangeLinkFreeing(ChangeLink: IVETChangeLink);
begin
  inherited;
  if ChangeLink.ChangeLinkClient = Self then
  begin
  {$IFDEF EXPLORERLISTVIEW_L}
    if ChangeLink.ChangeLinkServer = FVirtualExplorerListview then
      FVirtualExplorerListview := nil;
  {$ENDIF}
  end
end;

procedure TVirtualExplorerTreeview.LoadDefaultOptions;
begin
  FileObjects := DefaultExplorerTreeFileObjects;
  TreeOptions.AutoOptions := DefaultExplorerTreeAutoOptions;
  TreeOptions.SelectionOptions := DefaultExplorerTreeVETSelectionOptions;
  TreeOptions.MiscOptions := DefaultExplorerTreeMiscOptions;
  TreeOptions.PaintOptions := DefaultExplorerTreePaintOptions;
  TreeOptions.VETFolderOptions := DefaultExplorerTreeVETFolderOptions;
  TreeOptions.VETShellOptions := DefaultExplorerTreeVETShellOptions;
  TreeOptions.VETMiscOptions := DefaultExplorerTreeVETMiscOptions;
  TreeOptions.VETImageOptions := DefaultExplorerTreeVETImageOptions;
  TreeOptions.VETSyncOptions := DefaultExplorerTreeVETSyncOptions;
end;

{$IFDEF EXPLORERLISTVIEW_L}
procedure TVirtualExplorerTreeview.SetVirtualExplorerListview(
  const Value: TVirtualExplorerListview);
begin
  if FVirtualExplorerListview <> Value then
  begin
    if Assigned(FVirtualExplorerListview) then
      VETChangeDispatch.UnRegisterChangeLink(FVirtualExplorerListview, Self, utLink );
    FVirtualExplorerListview := Value;
    if Assigned(FVirtualExplorerListview) then
      VETChangeDispatch.RegisterChangeLink(FVirtualExplorerListview, Self, ChangeLinkChanging, ChangeLinkFreeing);
  end;
end;
{$ENDIF}

procedure TVirtualExplorerTreeview.WMRButtonDown(
  var Msg: TWMRButtonDown);
begin
  RightButtonDown := True;
  inherited;
end;

procedure TVirtualExplorerTreeview.WMRButtonUp(
  var Msg: TWMRButtonUp);
begin
  inherited;
  RightButtonDown := False;
end;

{ TColumnMenu }

constructor TColumnMenu.Create(AOwner: TCustomVirtualExplorerTree);
begin
  inherited Create(AOwner);
  FVET := AOwner
end;

procedure TColumnMenu.Popup(X, Y: Integer);

    function DuplicateTitle(HeaderTitle: WideString): Boolean;
    var
      i: integer;
    begin
      i := 0;
      Result := False;
      while not Result and (i < Items.Count) do
      begin
        Result := HeaderTitle = Items.Items[i].Caption;
        Inc(i)
      end;
    end;

var
  i: integer;
  NewMenuItem: TColumnMenuItem;
  ColState: TSHColumnStates;
  NeedsMore: Boolean;
begin
  {$ifdef COMPILER_4}
  // Items notify their parent so they are removed from the list automaticlly
  for i := Items.Count - 1 downto 0 do
    Items[i].Free;
  {$endif}
  {$ifdef COMPILER_5_UP}
  Items.Clear;
  {$endif}
  if VET.ColumnDetails = cdShellColumns then
  begin
    NeedsMore := False;
    for i := 0 to VET.Header.Columns.Count - 1 do
    begin
      ColState := VET.FRootFolderNamespace.DetailsGetDefaultColumnState(i);
      if not (csSecondaryUI in ColState) and (Items.Count < VET.ColumnMenuItemCount) and
      (VET.Header.Columns[i].Text <> '') and not DuplicateTitle(VET.Header.Columns[i].Text) then
      begin
        NewMenuItem := TColumnMenuItem.Create(Self);
        Items.Add(NewMenuItem);
        NewMenuItem.Caption := VET.Header.Columns[i].Text;
        NewMenuItem.Checked := coVisible in VET.Header.Columns[i].Options;
        NewMenuItem.ColumnIndex := i
      end else
        NeedsMore := True;
    end;
    if NeedsMore then
    begin
      NewMenuItem := TColumnMenuItem.Create(Self);
      NewMenuItem.Caption := '-';
      Items.Add(NewMenuItem);
      NewMenuItem := TColumnMenuItem.Create(Self);
      Items.Add(NewMenuItem);
      NewMenuItem.Caption := STR_COLUMNMENU_MORE;
      NewMenuItem.ColumnIndex := -2
    end
   end else
  begin
    { The old 8 columns from the original VET }
    for i := 0 to VET.Header.Columns.Count - 1 do
    begin
      NewMenuItem := TColumnMenuItem.Create(Self);
      Items.Add(NewMenuItem);
      NewMenuItem.Caption := VET.Header.Columns[i].Text;
      NewMenuItem.Checked := coVisible in VET.Header.Columns[i].Options;
      NewMenuItem.ColumnIndex := i
    end
  end;
  inherited;
end;

{ TColumnMenuItem }

procedure TColumnMenuItem.Click;

  function IsDuplicate(VST: TVirtualStringTree; Text: WideString): Boolean;
  var
    ColData: PColumnData;
    Node: PVirtualNode;
  begin
    Result := False;
    Node := VST.GetFirst;
    while not Result and Assigned(Node) do
    begin
      ColData := VST.GetNodeData(Node);
      Result := WideStrComp(PWideChar(ColData^.Title), PWideChar( Text)) = 0;
      Node := VST.GetNext(Node)
    end
  end;

var
  j: integer;
  i: LongWord;
  VET: TCustomVirtualExplorerTree;
  ColData: PColumnData;
  VST: TVirtualStringTree;
  BackupHeader: TMemoryStream;
begin
  VET := (Owner as TColumnMenu).VET;
  if ColumnIndex < -1 then
  begin
    BackupHeader := TMemoryStream.Create;
    try
      FormColumnSettings := TFormColumnSettings.Create(Self);
      try
        VST := FormColumnSettings.VSTColumnNames;
        VST.BeginUpdate;
        try
          for i := 0 to VET.Header.Columns.Count - 1 do
          begin
            j := 0;
            { Create the nodes ordered in columns items relative position }
            while (j < VET.Header.Columns.Count) and (VET.Header.Columns[j].Position <> i) do
              Inc(j);
            if (VET.Header.Columns[j].Text <> '') and not IsDuplicate(VST, VET.Header.Columns[j].Text) then
            begin
              ColData := VST.GetNodeData(VST.AddChild(nil));
              ColData.Title := VET.Header.Columns[j].Text;
              ColData.Enabled := coVisible in VET.Header.Columns[j].Options;
              ColData.Width := VET.Header.Columns[j].Width;
              ColData.ColumnIndex := VET.Header.Columns[j].Index;
            end
          end;
          VET.Header.SaveToStream(BackupHeader);
          BackupHeader.Seek(0, soFromBeginning);
        finally
          VST.EndUpdate;
        end;
        FormColumnSettings.OnVETUpdate := LiveVETUpdate;
        if FormColumnSettings.ShowModal = mrOk then
        begin
          UpdateColumns(VET, VST);
          VET.StoreColumnState;
          VET.DoColumnUserChangedVisibility;
        end else
        begin
          { Canceled restore the Header to original state before modifications }
          VET.BeginUpdate;
          try
            VET.Header.LoadFromStream(BackupHeader);
            VET.Invalidate;
          finally
            VET.EndUpdate
          end
        end;
      finally
        FormColumnSettings.Free;
        FormColumnSettings := nil
      end
    finally
      BackupHeader.Free
    end
  end else
  begin
    if not Checked then
      VET.Header.Columns[ColumnIndex].Options := VET.Header.Columns[ColumnIndex].Options + [coVisible]
    else
      VET.Header.Columns[ColumnIndex].Options := VET.Header.Columns[ColumnIndex].Options - [coVisible];
    VET.StoreColumnState;
    VET.DoColumnUserChangedVisibility;
  end;
  inherited;
end;

procedure TColumnMenuItem.LiveVETUpdate(Sender: TObject);
begin
  UpdateColumns((Owner as TColumnMenu).VET, FormColumnSettings.VSTColumnNames);
end;

procedure TColumnMenuItem.UpdateColumns(VET: TCustomVirtualExplorerTree; VST: TVirtualStringTree);
var
  LocalColData: PColumnData;
  i: integer;
  LocalChildNode: PVirtualNode;
begin
  VET.BeginUpdate;
  try
    i := 0;
    { Reposition columns based on order of Tree nodes and update data }
    LocalChildNode := VST.GetFirst;
    while Assigned(LocalChildNode) do
    begin
      LocalColData := VST.GetNodeData(LocalChildNode);
      VET.Header.Columns[LocalColData.ColumnIndex].Position := i;
      if LocalColData.Enabled then
        VET.Header.Columns[LocalColData.ColumnIndex].Options :=
           VET.Header.Columns[LocalColData.ColumnIndex].Options + [coVisible]
      else
        VET.Header.Columns[LocalColData.ColumnIndex].Options :=
           VET.Header.Columns[LocalColData.ColumnIndex].Options - [coVisible];
      VET.Header.Columns[LocalColData.ColumnIndex].Width := LocalColData.Width;
      LocalChildNode := LocalChildNode.NextSibling;
      Inc(i)
    end;
  finally
    VET.EndUpdate
  end
end;

{ TVETChangeLink }

function TVETChangeLink.GetChangeLinkClient: TObject;
begin
  Result := FChangeLinkClient
end;

function TVETChangeLink.GetChangeLinkServer: TObject;
begin
   Result := FChangeLinkServer
end;

function TVETChangeLink.GetOnChangeLinkFree: TVETChangeLinkFreeEvent;
begin
  Result := FOnChangeLinkFree
end;

function TVETChangeLink.GetOnChangeLink: TVETChangeLinkEvent;
begin
  Result := FOnChangeLink;
end;

procedure TVETChangeLink.SetChangeLinkClient(const Value: TObject);
begin
  FChangeLinkClient := Value;
end;

procedure TVETChangeLink.SetChangeLinkServer(const Value: TObject);
begin
  FChangeLinkServer := Value; 
end;

procedure TVETChangeLink.SetOnChangeLinkFree(const Value: TVETChangeLinkFreeEvent);
begin
  FOnChangeLinkFree := Value
end;

procedure TVETChangeLink.SetOnChangeLink(const Value: TVETChangeLinkEvent);
begin
  FOnChangeLink := Value;
end;


{ TVETChangeDispatch }

constructor TVETChangeDispatch.Create;
begin
  inherited Create;
  ChangeLinkCache := TInterfaceList.Create;
end;

destructor TVETChangeDispatch.Destroy;
begin
  // Support Halt( );
  if Assigned(FChangeLinkCache) then
    FChangeLinkCache.Free;
  inherited;
end;

procedure TVETChangeDispatch.DispatchChange(Server: TObject; NewPIDL: PItemIDList);
var
  i: integer;
  ChangeLink: IVETChangeLink;
 // Found: Boolean;
begin
  { Keep track of the initiatior of the Change Dispatching to keep unnecessary  }
  { recursing }
  if InitialDispatcher = nil then
  begin
    InitialDispatcher := Server;
    ReduceServerSet(Server);
    i := 0;
    try
      while (i < Count) do
      begin
        ChangeLink := Items[i] as IVETChangeLink;
        if (ChangeLink.ChangeLinkServer = Server) and Assigned(ChangeLink.OnChangeLink) then
        begin
          if (ChangeLink.ChangeLinkClient <> InitialDispatcher) then
            ChangeLink.OnChangeLink(Server, NewPIDL);
        end;
        Inc(i);
      end
    finally
      InitialDispatcher := nil
    end
  end else
  begin
    i := 0;
    try
      while (i < ChangeLinkCache.Count) do
      begin
        ChangeLink := ChangeLinkCache[i] as IVETChangeLink;
        if (ChangeLink.ChangeLinkServer = Server) and Assigned(ChangeLink.OnChangeLink) then
        begin
          if (ChangeLink.ChangeLinkClient <> InitialDispatcher) then
            ChangeLink.OnChangeLink(Server, NewPIDL);
        end;
        Inc(i);
      end
    finally
    end
  end;
end;

procedure TVETChangeDispatch.DispatchLinks(Server: TObject; NewPIDL: PItemIDList);
var
  i: integer;
begin
  for i := 0 to FChangeLinkCache.Count - 1 do
    (FChangeLinkCache[i] as IVETChangeLink).OnChangeLink(Server, NewPIDL);
end;

function TVETChangeDispatch.FindLink(Server, Client: TObject): integer;
var
  i: integer;
  Found: Boolean;
  Link: IVETChangeLink;
begin
  Result := -1;
  Found := False;
  i := 0;
  while not Found and (i < Count) do
  begin
    Link := Items[i] as IVETChangeLink;
    Found := (Link.ChangeLinkServer = Server) and (Link.ChangeLinkClient = Client);
    if Found then
      Result := i;
    Inc(i)
  end;
end;

procedure TVETChangeDispatch.ReduceServerSet(Server: TObject);
var
  i, j: integer;
  Unique: Boolean;
begin
  FChangeLinkCache.Clear;

  // Toss out any links that the passed Server will handle directly. This eliminates
  // other links to Clients by other Servers
  for i := 0 to Count - 1 do
  begin
    if (Items[i] as IVETChangeLink).ChangeLinkServer <> Server then
    begin
      Unique := True;
      j := 0;
      while Unique and (j < Count) do
      begin
        Unique := not(((Items[j] as IVETChangeLink).ChangeLinkServer = Server) and
           ((Items[i] as IVETChangeLink).ChangeLinkClient = (Items[j] as IVETChangeLink).ChangeLinkClient));
        Inc(j)
      end;
      if Unique then
        ChangeLinkCache.Add((Items[i] as IVETChangeLink))
    end
  end
end;

procedure TVETChangeDispatch.RegisterChangeLink(Server, Client: TObject;
  ClientOnChangeEvent: TVETChangeLinkEvent; ChangeLinkFreeEvent: TVETChangeLinkFreeEvent);

// Called to register a link between the server and the client.  Two methods can
// be specified, one to be called when the server dispatches a change and one to
// called when the link is being broken, or the Server or Client is being freed.
// NOTE it is imperitive that the Server or Client call UnregisterChangeLink with
// the reason for the unregistration

var
  ChangeLink: IVETChangeLink;
begin
  Lock;
  try
    ChangeLink := TVETChangeLink.Create;
    ChangeLink.ChangeLinkServer := Server;
    ChangeLink.ChangeLinkClient := Client;
    ChangeLink.OnChangeLink := ClientOnChangeEvent;
    ChangeLink.OnChangeLinkFree := ChangeLinkFreeEvent;
    Add(ChangeLink);
    Inc(VETChangeObjects)
  finally
    Unlock
  end;
end;

function TVETChangeDispatch.UnRegisterChangeLink(Server, Client: TObject; UnRegisterType: TUnRegisterType): Boolean;

// Called to unregister a change link for a varity of reasons.  If the server object
// is begin destroyed then the server should call like this
//    UnRegisterChangeLink(Self, nil, utServer)
// assuming it is called from the servers Destroy method. Each link will be searched
// for an entry that references the 

var
  i, j: integer;
  ChangeLink: IVETChangeLink;
  Found: Boolean;
  Done: Boolean;
begin
  i := Count - 1;
  Done := False;
  Lock;
  try
    while (i > -1) and not Done do
    begin
      ChangeLink := Items[i] as IVETChangeLink;
      if UnRegisterType = utServer then
        Found := Server = ChangeLink.ChangeLinkServer
      else
      if UnRegisterType = utClient then
        Found := Client = ChangeLink.ChangeLinkClient
      else
      if UnRegisterType = utLink then
        Found := (Client = ChangeLink.ChangeLinkClient) and (Server = ChangeLink.ChangeLinkServer)
      else
        Found := (Client = ChangeLink.ChangeLinkClient) or (Server = ChangeLink.ChangeLinkServer);
      if Found then
      begin
        if Assigned(ChangeLink.OnChangeLinkFree) then
          for j := 0 to Count - 1 do
            ChangeLink.OnChangeLinkFree(ChangeLink);
        ChangeLink.OnChangeLink := nil;
        ChangeLink.OnChangeLinkFree := nil;
        ChangeLink.ChangeLinkServer := nil;
        ChangeLink.ChangeLinkClient := nil;
        Delete(i);
        Dec(VETChangeObjects);
        { utAll must look at every object }
        if not (UnRegisterType = utAll) then
          Done := True
      end;
      Dec(i);
    end;
  finally
    Result := Done;
    Unlock
  end
end;

{ TUserDataStorage }

procedure TUserDataStorage.Assign(Source: TPersistent);
begin
  //override this method
end;

procedure TUserDataStorage.LoadFromStream(S: TStream; Version: integer;
  ReadVerFromStream: Boolean);
begin
  //override this method
  inherited;
end;

procedure TUserDataStorage.SaveToStream(S: TStream; Version: integer;
  WriteVerToStream: Boolean);
begin
  //override this method
  inherited;
end;

{ TNodeStorage }

procedure TNodeStorage.Assign(Source: TNodeStorage);
var
  i: integer;
  Node: TNodeStorage;
begin
  if Assigned(Source) then
  begin
    if (Source is TRootNodeStorage) and (Self is TRootNodeStorage) then
      TRootNodeStorage(Self).Stored := TRootNodeStorage( Source).Stored;

    PIDLMgr.FreePIDL(AbsolutePIDL);
    FreeAndNil(FChildNodeList);
    AbsolutePIDL := nil;
    AbsolutePIDL := nil;
    FShellFolder := nil;
    ParentNode := nil;

    AbsolutePIDL := PIDLMgr.CopyPIDL(Source.AbsolutePIDL);
    RelativePIDL := PIDLMgr.GetPointerToLastID(AbsolutePIDL);
    if Assigned(Source.ChildNodeList) then
    begin
      ChildNodeList := TNodeStorageList.Create;
      for i := 0 to Source.ChildNodeList.Count - 1 do
      begin
        Node := TNodeStorage.Create(nil, nil);
        Node.Assign(Source.ChildNodeList[i]);
        Node.ParentNode := Self;
        ChildNodeList.Add(Node);
      end;
    end;
    Storage.Types := Source.Storage.Types;
    Storage.Check := Source.Storage.Check;
    Storage.Column := Source.Storage.Column;

    if Assigned(Source.Storage.UserData) then
    begin
      if not Assigned(Storage.UserData) then
        Storage.UserData := TUserDataStorage( Source.Storage.UserData.ClassType.Create);
      Storage.UserData.Assign(Source.Storage.UserData);
    end
  end
end;

procedure TNodeStorage.AssignTo(Destination: TNodeStorage);
begin
  if Assigned(Destination) then
    Destination.Assign(Self)
end;

procedure TNodeStorage.Clear(FreeUserData: Boolean = False);
begin
  // Support for Halt()
  if Assigned(PIDLMgr) then
    PIDLMgr.FreePIDL(AbsolutePIDL);
  AbsolutePIDL := nil;
  RelativePIDL := nil;
  FShellFolder := nil;
  if Assigned(ChildNodeList) then
    ChildNodeList.Clear;
  if FreeUserData then
    FreeAndNil(Storage.UserData); // free user data, TNodeStorage owns the data
end;

constructor TNodeStorage.Create(AnAbsolutePIDL: PItemIDList; AnOwnerNode: TNodeStorage);
begin
  AbsolutePIDL := AnAbsolutePIDL;
  RelativePIDL := PIDLMgr.GetPointerToLastID(AbsolutePIDL);
  ParentNode := AnOwnerNode;
  Inc(NodeCount);
end;

destructor TNodeStorage.Destroy;
var
  Node: TRootNodeStorage;
begin
  Node := RootNode;
  if Assigned(Node) then
    if Node.CacheNode = Self then
      Node.CacheNode := nil;
  Clear;
  Dec(NodeCount);
  FreeAndNil(FChildNodeList);
  inherited;
end;

function TNodeStorage.GetRootNode: TRootNodeStorage;
var
  Temp: TNodeStorage;
  Done: Boolean;
begin
  Result := nil;
  Done := False;
  Temp := ParentNode;
  while Assigned(Temp) and not Done do
  begin
    if Assigned(Temp.ParentNode) then
      Temp := Temp.ParentNode
    else
      Done := True
  end;
  if Temp is TRootNodeStorage then
    Result := TRootNodeStorage( Temp)
end;

function TNodeStorage.GetShellFolder: IShellFolder;
begin
  if not Assigned(FShellFolder) then
  begin
     if Assigned(ParentNode) then
      ParentNode.ShellFolder.BindToObject(RelativePIDL, nil, IID_IShellFolder, Pointer(FShellFolder))
     else
      SHGetDesktopFolder(FShellFolder);
  end;
  Result := FShellFolder;
end;

procedure TNodeStorage.LoadFromFile(FileName: WideString; Version: integer = VETStreamStorageVer;
  ReadVerFromStream: Boolean = False);
begin
  // Just to get the right StreamVersion
  inherited LoadFromFile(FileName, Version, ReadVerFromStream);
end;

procedure TNodeStorage.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer;
  ReadVerFromStream: Boolean = False);
var
  ChildNodes, StoreUserData: Boolean;
  Size: integer;
  i: integer;
  Str: AnsiString;
  UserClass: TPersistentClass;
begin
  inherited LoadFromStream(S, Version, ReadVerFromStream);
  PIDLMgr.FreePIDL(AbsolutePIDL);
  FAbsolutePIDL := PIDLMgr.LoadFromStream(S);
  FRelativePIDL := PIDLMgr.GetPointerToLastID(AbsolutePIDL);

  S.read(ChildNodes, SizeOf(ChildNodes));
  if ChildNodes then
  begin
    if not Assigned(ChildNodeList) then
      ChildNodeList := TNodeStorageList.Create;
    ChildNodeList.LoadFromStream(S, Version, ReadVerFromStream);
    for i := 0 to ChildNodeList.Count - 1 do
      ChildNodeList.Items[i].ParentNode := Self;
  end;

  S.read(Storage.Types, SizeOf(Storage.Types));

  // Upgraded the TCheckStorage to include the Checkbox type in Version 2 of the stream
  if StreamVersion > VETStreamStorageVer_1 then
    S.read(Storage.Check, SizeOf(Storage.Check))
  else
    S.read(Storage.Check, SizeOf(TVer1CheckStorage));

  S.read(Size, SizeOf(Size));
  SetLength(Storage.Column.Width, Size);
  if Size > 0 then
    S.read(Storage.Column.Width[0], Size * SizeOf(Word));

  S.read(Size, SizeOf(Size));
  SetLength(Storage.Column.Position, Size);
  if Size > 0 then
    S.read(Storage.Column.Position[0], Size * SizeOf(Word));

  S.read(Size, SizeOf(Size));
  SetLength(Storage.Column.Visible, Size);
  if Size > 0 then
    S.read(Storage.Column.Visible[0], Size * SizeOf(Boolean));

  // New Stream Data added 03.01.02
  if StreamVersion >= VETStreamStorageVer_1 then
  begin
    S.read(StoreUserData, SizeOf(StoreUserData));
    if StoreUserData then
    begin
      // Read back the class Name
      S.read(Size, SizeOf(Size));
      SetLength(Str, Size);
      S.read(PAnsiChar(Str)^, Size);
      // Find it in the registered classes
      UserClass := FindClass(Str);
      // Create an instance of it and load it
      Storage.UserData := TUserDataStorage( UserClass.Create);
      Storage.UserData.LoadFromStream(S, Version, ReadVerFromStream);
    end;
  end;

  if StreamVersion >= VETStreamStorageVer_4 then
  begin
    S.Read(Storage.Grouping.Enabled, SizeOf(Storage.Grouping));
    S.Read(Storage.Grouping.GroupColumn, SizeOf(Storage.Grouping.GroupColumn));
    S.Read(Storage.Grouping.View, SizeOf(Storage.Grouping.View));
    S.Read(Storage.Column.SortColumn, SizeOf(Storage.Column.SortColumn));
    S.Read(Storage.Column.SortDir, SizeOf(Storage.Column.SortDir));
    S.Read(Storage.Column.DefaultSortDir, SizeOf(Storage.Column.DefaultSortDir));  // Depreciated
    S.Read(Storage.Tag, SizeOf(Storage.Tag));
  end;

  if StreamVersion >= VETStreamStorageVer_6 then
  begin
    S.Read(Storage.Thumbnails.Size, SizeOf(Storage.Thumbnails.Size));
  end;

  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer_X then }
  {   read new data                     }

end;

procedure TNodeStorage.SaveToFile(FileName: WideString; Version: integer = VETStreamStorageVer;
  ReadVerFromStream: Boolean = False);
begin
  inherited SaveToFile(FileName, Version, ReadVerFromStream);
end;

procedure TNodeStorage.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer;
  WriteVerToStream: Boolean = False);
var
  ChildNodes, StoreUserData: Boolean;
  Size: integer;
  Str: AnsiString;
begin
  inherited;
  PIDLMgr.SaveToStream(S, FAbsolutePIDL);

  ChildNodes := Assigned(ChildNodeList);
  S.write(ChildNodes, SizeOf(ChildNodes));
  if ChildNodes then
    ChildNodeList.SaveToStream(S, Version, WriteVerToStream);

  S.write(Storage.Types, SizeOf(Storage.Types));
  S.write(Storage.Check, SizeOf(Storage.Check));

  Size := Length(Storage.Column.Width);
  S.write(Size, SizeOf(Size));
  if Size > 0 then
    S.write( Storage.Column.Width[0], Size * SizeOf(Word));

  Size := Length(Storage.Column.Position);
  S.write(Size, SizeOf(Size));
  if Size > 0 then
    S.write( Storage.Column.Position[0], Size * SizeOf(Word));

  Size := Length(Storage.Column.Visible);
  S.write(Size, SizeOf(Size));
  if Size > 0 then
    S.write( Storage.Column.Visible[0], Size * SizeOf(Boolean));

  // New Stream Data added 03.01.02
  if StreamVersion >= VETStreamStorageVer_1 then
  begin
    StoreUserData := Assigned(Storage.UserData);
    S.write(StoreUserData, SizeOf(StoreUserData));
    if StoreUserData then
    begin
      // Jim
      // Save the Classname so we can construct a class of this type later
      Size := Length(Storage.UserData.ClassName);
      S.write(Size, SizeOf(Size));
      Str := Storage.UserData.ClassName;
      S.write(PAnsiChar(Str)^, Size);
      Storage.UserData.SaveToStream(S, Version, WriteVerToStream);
    end
  end;
  /// New Stream Data added 9-13-06
  S.Write(Storage.Grouping.Enabled, SizeOf(Storage.Grouping));
  S.Write(Storage.Grouping.GroupColumn, SizeOf(Storage.Grouping.GroupColumn));
  S.Write(Storage.Grouping.View, SizeOf(Storage.Grouping.View));
  S.Write(Storage.Column.SortColumn, SizeOf(Storage.Column.SortColumn));
  S.Write(Storage.Column.SortDir, SizeOf(Storage.Column.SortDir));
  S.Write(Storage.Column.DefaultSortDir, SizeOf(Storage.Column.DefaultSortDir)); // Depreciated
  S.Write(Storage.Tag, SizeOf(Storage.Tag));

  /// New Stream Data added 5-6-07
  S.Write(Storage.Thumbnails.Size, SizeOf(Storage.Thumbnails.Size));
end;

{ TNodeStorageList }

procedure TNodeStorageList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;      
end;

function TNodeStorageList.GetItems(Index: integer): TNodeStorage;
begin
  Result := TNodeStorage( inherited Items[Index]);
end;

procedure TNodeStorageList.LoadFromFile(FileName: WideString;
  Version: integer; ReadVerFromStream: Boolean);
begin
  inherited LoadFromFile(FileName, Version, ReadVerFromStream);
end;

procedure TNodeStorageList.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer;
  ReadVerFromStream: Boolean = False);
var
  C, i: integer;
  Node: TNodeStorage;
  Desktop, Folder: IShellFolder;
  Flags: Longword;
  Exists: Boolean;
  OldCB: Word;
  OldPIDL: PItemIDList;
begin
  inherited;
  SHGetDesktopFolder(Desktop);
  S.read(C, SizeOf(C));
  Capacity := C;
  for i := 0 to C - 1 do
  begin
    Node := TNodeStorage.Create(nil, nil);
    Node.LoadFromStream(S, Version, ReadVerFromStream);
    Exists := True;
    if not PIDLMgr.IsDesktopFolder(Node.AbsolutePIDL) then
    begin
      { Flush out objects that no longer exist }
      PIDLMgr.StripLastID(Node.AbsolutePIDL, OldCB, OldPIDL);
      try

        { Node is direct decendant of desktop, special case }
        if OldPIDL = Node.AbsolutePIDL then
          Folder := Desktop
        else begin
          Desktop.BindToObject(Node.AbsolutePIDL, nil, IID_IShellFolder, Pointer(Folder));
          Exists := Assigned(Folder)
        end;
        if Exists then
        begin
          // Note:  This will cause PIDLs of removeable drive to fail. That means
          // removeable drives will not have persistent checking
          OldPIDL.mkid.cb := OldCB;
          Flags := SFGAO_FOLDER or SFGAO_VALIDATE;
          Exists := Succeeded(Folder.GetAttributesOf(1, OldPIDL, Flags));
        end;
      finally
        OldPIDL.mkid.cb := OldCB;
      end;
    end;
    if Exists then
      Add(Node)
    else
      Node.Free
  end;
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TNodeStorageList.SaveToFile(FileName: WideString;
  Version: integer; ReadVerFromStream: Boolean);
begin
  inherited SaveToFile(FileName, Version, ReadVerFromStream)
end;

procedure TNodeStorageList.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer;
  WriteVerToStream: Boolean = False);
var
  C, i: integer;
begin
  inherited; 
  C := Count;
  S.write(C, SizeOf(C));
  for i := 0 to Count - 1 do
    Items[i].SaveToStream(S, Version, WriteVerToStream);
end;

procedure TNodeStorageList.SetItems(Index: integer; const Value: TNodeStorage);
begin
  inherited Items[Index] := Value
end;

{ TRootNodeStorage }

procedure TRootNodeStorage.Clear(FreeUserData: Boolean = False);
begin
  if Assigned(FResolvedFileNames) then
    FResolvedFileNames.Clear;
  if Assigned(FCheckedFileNames) then
    FCheckedFileNames.Clear;
  if Assigned(FCheckedPIDLs) then
    FCheckedPIDLs.Clear;
  Stored := False;
  inherited;
end;

constructor TRootNodeStorage.Create;
var
  PIDL: PItemIDList;
begin
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_DESKTOP, PIDL);
  inherited Create(PIDL, nil);
  ChildNodeList := TNodeStorageList.Create;
  FCheckedPIDLs := TCommonPIDLList.Create;
  {$IFDEF TNTSUPPORT}
  FCheckedFileNames := TTntStringList.Create;
  FResolvedFileNames := TTntStringList.Create;
  {$ELSE}
  FCheckedFileNames := TStringList.Create;
  FResolvedFileNames := TStringList.Create;
  {$ENDIF}
end;

procedure TRootNodeStorage.Delete(APIDL: PItemIDList; StorageTypes: TStorageTypes;
  Force: Boolean = False; FreeUserData: Boolean = False);
var
  Node: TNodeStorage;
begin
  Node := WalkPIDLToStorageNode(APIDL, False);
  if Assigned(Node) then
  begin
    Node.Storage.Types := Node.Storage.Types - StorageTypes;
    if ((Node.Storage.Types = []) or Force) and (Node <> Self) then
    begin
      { Delete the node if there is not a valid storage type for the node }
      if Node = CacheNode then
        CacheNode := nil;
      Node.ParentNode.ChildNodeList.Remove(Node);
      if FreeUserData then
        FreeAndNil(Node.Storage.UserData);
      Node.Free;
    end else
    begin
      if (stUser in StorageTypes) and FreeUserData then
        FreeAndNil(Node.Storage.UserData);

      if Node = Self then
      begin
        Node.Storage.Check.CheckState := csUncheckedNormal;
        Stored := False
      end
    end
  end
end;

destructor TRootNodeStorage.Destroy;
begin
  FreeAndNil(FResolvedFileNames);
  FreeAndNil(FCheckedFileNames);
  FreeAndNil(FCheckedPIDLs);
  Clear(True);
  inherited;
end;

function TRootNodeStorage.Find(APIDL: PItemIDList; StorageTypes: TStorageTypes): TNodeStorage;
begin
  Result := nil;
  if Assigned(APIDL) then
  begin
    Result := WalkPIDLToStorageNode(APIDL, False);
    if Assigned(Result) and Stored then
    begin
      if StorageTypes * Result.Storage.Types = [] then
        Result := nil
    end else
  end
end;

function TRootNodeStorage.Find(APIDL: PItemIDList; StorageTypes: TStorageTypes;
  var StorageNode: TNodeStorage): Boolean;
begin
  Result := False;
  StorageNode := WalkPIDLToStorageNode(APIDL, False);
  if Assigned(StorageNode) and Stored then
  begin
    if StorageTypes * StorageNode.Storage.Types <> [] then
      Result := True
    else
      StorageNode := nil
  end else
    StorageNode := nil
end;

{$IFDEF TNTSUPPORT}
function TRootNodeStorage.GetCheckedFileNames: TWideStrings;

  procedure RecurseStorage(S: TNodeStorage; Strings: TWideStrings);
  var
    NS: TNamespace;
    i: integer;
    Str: string;
  begin
    NS := TNamespace.Create(S.AbsolutePIDL, nil);
    NS.FreePIDLOnDestroy := False;
    // Need to do this to get the real path to special folders
    Str := NS.NameForParsing;
    { The items must - be in the file system, a valid file or directory, have a }
    { full check (not mixed)    }
    if NS.FileSystem and (FileExistsW(Str) or WideDirectoryExists(Str) or WideIsDrive(Str)) and
      (S.Storage.Check.CheckState = csCheckedNormal) then
      Strings.Add(Str);
    if Assigned(S.ChildNodeList) then
      for i := 0 to S.ChildNodeList.Count - 1 do
        RecurseStorage(S.ChildNodeList[i], Strings);
    NS.Free;
  end;

var
  OldErrorMode: integer;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    FCheckedFileNames.Clear;
    RecurseStorage(Self, FCheckedFileNames);
  finally
    Result := FCheckedFileNames;
    SetErrorMode(OldErrorMode)
  end
end;
{$ELSE}
function TRootNodeStorage.GetCheckedFileNames: TStrings;

  procedure RecurseStorage(S: TNodeStorage; Strings: TStrings);
  var
    NS: TNamespace;
    i: integer;
    Str: string;
  begin
    NS := TNamespace.Create(S.AbsolutePIDL, nil);
    NS.FreePIDLOnDestroy := False;
    // Need to do this to get the real path to special folders
    Str := NS.NameForParsing;
    { The items must - be in the file system, a valid file or directory, have a }
    { full check (not mixed)    }
    if NS.FileSystem and (FileExistsW(Str) or WideDirectoryExists(Str) or WideIsDrive(Str)) and
      (S.Storage.Check.CheckState = csCheckedNormal) then
      Strings.Add(Str);
    if Assigned(S.ChildNodeList) then
      for i := 0 to S.ChildNodeList.Count - 1 do
        RecurseStorage(S.ChildNodeList[i], Strings);
    NS.Free;
  end;

var
  OldErrorMode: integer;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    FCheckedFileNames.Clear;
    RecurseStorage(Self, FCheckedFileNames);
  finally
    Result := FCheckedFileNames;
    SetErrorMode(OldErrorMode)
  end
end;
{$ENDIF}

function TRootNodeStorage.GetCheckedPIDLs: TCommonPIDLList;

  procedure RecurseStorage(S: TNodeStorage; PIDLs: TCommonPIDLList);
  var
    i: integer;
  begin
    if S.Storage.Check.CheckState = csCheckedNormal then
      PIDLs.Add(S.AbsolutePIDL);
    if Assigned(S.ChildNodeList) then
      for i := 0 to S.ChildNodeList.Count - 1 do
        RecurseStorage(S.ChildNodeList[i], PIDLs);
  end;

begin
  FCheckedPIDLs.Clear;
  RecurseStorage(Self, FCheckedPIDLs);
  Result := FCheckedPIDLs;
end;

{$IFDEF TNTSUPPORT}
function TRootNodeStorage.GetResolvedFileNames: TWideStrings;

    function HasAsParentFolder(Parent, Child: string): Boolean;
    begin
      {$ifdef COMPILER_6_UP}
      Parent := IncludeTrailingPathDelimiter(Parent);
      {$else}
      if Parent[Length(Parent)] <> '\' then
        Parent := Parent + '\';
      {$endif}
      Result := (Length(Parent) < Length(Child)) and (Pos(Parent, Child) = 1);
    end;
var
  i: Integer;
begin
  FResolvedFileNames.Assign(CheckedFileNames);
  {$ifdef COMPILER_6_UP}
    TStringList(FResolvedFileNames).CaseSensitive := false;
  {$endif}
  { D5 is case insensitive anyway }
  TStringList(FResolvedFileNames).Sort;
  i := 1;
  while i < FResolvedFileNames.Count do
    if HasAsParentFolder(FResolvedFileNames[i - 1], FResolvedFileNames[i]) then
      FResolvedFileNames.Delete(i)
    else
      Inc(i);
  Result := FResolvedFileNames
end;
{$ELSE}
function TRootNodeStorage.GetResolvedFileNames: TStrings;

    function HasAsParentFolder(Parent, Child: string): Boolean;
    begin
      {$ifdef COMPILER_6_UP}
      Parent := IncludeTrailingPathDelimiter(Parent);
      {$else}
      if Parent[Length(Parent)] <> '\' then
        Parent := Parent + '\';
      {$endif}
      Result := (Length(Parent) < Length(Child)) and (Pos(Parent, Child) = 1);
    end;
var
  i: Integer;
begin
  FResolvedFileNames.Assign(CheckedFileNames);
  {$ifdef COMPILER_6_UP}
    TStringList(FResolvedFileNames).CaseSensitive := false;
  {$endif}
  { D5 is case insensitive anyway }
  TStringList(FResolvedFileNames).Sort;
  i := 1;
  while i < FResolvedFileNames.Count do
    if HasAsParentFolder(FResolvedFileNames[i - 1], FResolvedFileNames[i]) then
      FResolvedFileNames.Delete(i)
    else
      Inc(i);
  Result := FResolvedFileNames
end;
{$ENDIF}

function TRootNodeStorage.ProcessNode(RelativePIDL: PItemIDList; CurrentNode: TNodeStorage; Force,
  MarkCheckMixed: Boolean): TNodeStorage;
{ RelativePIDL is the PIDL of an item that is a child of the CurrentNode.  Force }
{ Forces the node to be returned, creating any parent nodes as it needs along    }
{ the way.                                                                       }
var
  i: integer;
  NewNode: TNodeStorage;
begin
  { Create the ChildList if necessary }
  if not Assigned(CurrentNode.ChildNodeList) then
    CurrentNode.ChildNodeList := TNodeStorageList.Create;
  { See if the node is in the ChildList }
  if CurrentNode.ChildNodeList.Count > 0 then
    i := NodeListBinarySearch(RelativePIDL, CurrentNode.ChildNodeList, CurrentNode.ShellFolder, 0, CurrentNode.ChildNodeList.Count - 1)
  else
    i := -1;
  if i > -1 then
    Result := CurrentNode.ChildNodeList[i]  // Yes it is in the list return it
  else begin
    { No it is not in the list, should we force create it? }
    if Force then
    begin
      { Yes force it, create a new node }
      NewNode := TNodeStorage.Create(PIDLMgr.AppendPIDL(CurrentNode.AbsolutePIDL, RelativePIDL), CurrentNode);
      if MarkCheckMixed and (Storage.Check.CheckType = ctTriStateCheckBox) then
      begin
        { These are intermediate levels so they will be checked as mixed }
        NewNode.Storage.Check.CheckType := ctTriStateCheckBox;  
        NewNode.Storage.Check.CheckState := csMixedNormal;
        NewNode.Storage.Types := [stChecks];
      end;
      { Use the fact the list is always sorted to find a insert point throught a BinarySearch }
      i := NodeListInsertPt(RelativePIDL, CurrentNode.ChildNodeList, CurrentNode.ShellFolder, 0, CurrentNode.ChildNodeList.Count - 1);
      { Just make sure i is a valid insert point }
      if (i > -1) and (i <= CurrentNode.ChildNodeList.Count) then
      begin
        CurrentNode.ChildNodeList.Insert(i, NewNode);
        Result := NewNode
      end else
        Result := nil  // Error
    end else
      Result := nil
  end;
end;

procedure TRootNodeStorage.LoadFromStream(S: TStream; Version: integer = 0; ReadVerFromStream: Boolean = False);
begin
  inherited LoadFromStream(S, Version, ReadVerFromStream);
  Stored := True;
end;

procedure TRootNodeStorage.SetCheckedPIDLs(const Value: TCommonPIDLList);
var
  i: integer;
  Storage: TNodeStorage;
begin
  Clear;
  for i := 0 to Value.Count - 1 do
  begin
    Storage := Store(Value[i], [stChecks]);
    if Assigned(Storage) then
    begin
      Storage.Storage.Check.CheckState := csCheckedNormal;
      Storage.Storage.Check.CheckType := ctTriStateCheckBox;
    end
  end;
end;

{$IFDEF TNTSUPPORT}
procedure TRootNodeStorage.SetCheckFileNames(const Value: TWideStrings);
var
  Desktop: IShellFolder;
  i: integer;
  WS: WideString;
  pchEaten, pdwAttributes: LongWord;
  PIDL: PItemIdList;
  Storage: TNodeStorage;
begin
  Clear;
  SHGetDesktopfolder(Desktop);
  pdwAttributes := 0;
  for i := 0 to Value.Count - 1 do
  begin
    WS := Value[i];
    if Succeeded(Desktop.ParseDisplayName(Application.Handle, nil, PWideChar(WS), pchEaten, PIDL, pdwAttributes)) then
    begin
      Storage := Store(PIDL, [stChecks]);
      if Assigned(Storage) then
      begin
        Storage.Storage.Check.CheckState := csCheckedNormal;
        Storage.Storage.Check.CheckType := ctTriStateCheckBox;
      end;
      PIDLMgr.FreePIDL(PIDL);
    end
  end;
end;
{$ELSE}
procedure TRootNodeStorage.SetCheckFileNames(const Value: TStrings);
var
  Desktop: IShellFolder;
  i: integer;
  WS: WideString;
  pchEaten, pdwAttributes: LongWord;
  PIDL: PItemIdList;
  Storage: TNodeStorage;
begin
  Clear;
  SHGetDesktopfolder(Desktop);
  pdwAttributes := 0;
  for i := 0 to Value.Count - 1 do
  begin
    WS := Value[i];
    if Succeeded(Desktop.ParseDisplayName(Application.Handle, nil, PWideChar(WS), pchEaten, PIDL, pdwAttributes)) then
    begin
      Storage := Store(PIDL, [stChecks]);
      if Assigned(Storage) then
      begin
        Storage.Storage.Check.CheckState := csCheckedNormal;
        Storage.Storage.Check.CheckType := ctTriStateCheckBox;
      end;
      PIDLMgr.FreePIDL(PIDL);
    end
  end;
end;
{$ENDIF}

function TRootNodeStorage.SetFileChecked(FileName: WideString;
  CheckBoxType: TCheckType): Boolean;
var
  Desktop: IShellFolder;
  pchEaten, pdwAttributes: LongWord;
  PIDL: PItemIdList;
  Storage: TNodeStorage;
begin
  Result := False;
  SHGetDesktopfolder(Desktop);
  pdwAttributes := 0;
  if Succeeded(Desktop.ParseDisplayName(Application.Handle, nil, PWideChar(FileName), pchEaten, PIDL, pdwAttributes)) then
  begin
    Storage := Store(PIDL, [stChecks]);
    if Assigned(Storage) then
    begin
      Storage.Storage.Check.CheckState := csCheckedNormal;
      Storage.Storage.Check.CheckType := CheckBoxType;
      Result := True
    end;
    PIDLMgr.FreePIDL(PIDL);
  end
end;

function TRootNodeStorage.SetPIDLChecked(PIDL: PItemIDList;
  CheckBoxType: TCheckType): Boolean;
var
  Storage: TNodeStorage;
begin
  Result := False;
  Storage := Store(PIDL, [stChecks]);
  if Assigned(Storage) then
  begin
    Storage.Storage.Check.CheckState := csCheckedNormal;
    Storage.Storage.Check.CheckType := CheckBoxType;
    Result := True
  end
end;

function TRootNodeStorage.Store(APIDL: PItemIDList; StorageTypes: TStorageTypes): TNodeStorage;
begin
  Result := WalkPIDLToStorageNode(APIDL, True);
  if Assigned(Result) then
  begin
 //   if Result = Self then
      FStored := True;      // Any time something is stored the storage is valid
    Result.Storage.Types := Result.Storage.Types + StorageTypes
  end
end;

function TRootNodeStorage.WalkPIDLToStorageNode(PIDL: PItemIDList; Force: Boolean): TNodeStorage;
var
  OldCB: Word;
  Head, Tail: PItemIDList;
  CurrentNode: TNodeStorage;
begin
  Result := nil;
  if PIDLMgr.IsDesktopFolder(PIDL) then
    Result := Self
  else
  begin
    { The CachedNode is the parent of the last node accessed or added }
    if Assigned(CacheNode) then
    begin
      { Strip off the last ID }
      Tail := PIDLMgr.GetPointerToLastID(PIDL);
      OldCB := Tail.mkid.cb;
      Tail.mkid.cb := 0;
      try
        if ILIsEqual(PIDL, CacheNode.AbsolutePIDL) then
        begin
          Tail.mkid.cb := OldCB;
          Result := ProcessNode(Tail, CacheNode, Force, False);
        end
      finally
        Tail.mkid.cb := OldCB;
      end
    end;
    if not Assigned(Result) then
    begin
      CacheNode := nil;
      if Assigned(PIDL) then
      begin
        Head := PIDL;
        Tail := Head;
        CurrentNode := Self;
        OldCB := 1;
        while (OldCB <> 0) and Assigned(CurrentNode) do
        begin
          Inc( PAnsiChar(Tail), Head.mkid.cb);
          OldCB := Tail.mkid.cb;
          Tail.mkid.cb := 0;
          try
            CurrentNode := ProcessNode(Head, CurrentNode, Force, OldCB <> 0);
          finally
            Tail.mkid.cb := OldCB;
            Head := Tail;
          end;
        end;
        Result := CurrentNode;
        if Assigned(Result) then
          CacheNode := Result.ParentNode
      end;
    end
  end;
end;

{ TGlobalViewManager }

procedure TGlobalViewManager.LoadFromStream(S: TStream; Version: integer = VETStreamStorageVer; ReadVerFromStream: Boolean = False);
begin
  Clear;
  inherited LoadFromStream(S, Version, True);
  { Add new stream data here }
  { if StreamVersion >= StreamStorageVer then }
  {   read new data                     }
end;

procedure TGlobalViewManager.SaveToStream(S: TStream; Version: integer = VETStreamStorageVer; WriteVerToStream: Boolean = False);
begin
  inherited SaveToStream(S, VETStreamStorageVer, True);
end;

procedure TGlobalViewManager.ShowView(ViewName: WideString;
  VET: TCustomVirtualExplorerTree);
var
  View: TView;
begin
  VET.WaitCursor(True);
  try
    View := GetView(ViewName);
    if Assigned(View) then
    begin
      VET.Storage.Assign(View.Storage);
      View.RestoreTree(VET, True, True);
      { Must initialize all node to get the tree in sync with the Storage }
      VET.InitAllNodes;
    end;
  finally
    VET.WaitCursor(False)
  end
end;

procedure TGlobalViewManager.Snapshot(NewViewName: WideString;
  VET: TCustomVirtualExplorerTree);
var
  View: TView;
  Reuse: Boolean;
begin
  Reuse := False;
  View := GetView(NewViewName);
  if not Assigned(View) then
    View := TView.Create(NewViewName)
  else
    Reuse := True;
  View.Clear;
  View.SaveTree(VET, True, True);
  View.Storage.Assign(VET.Storage);
  if not Reuse then
    Views.Add(View);
end;


{ TCustomVirtualExplorerCombobox }

function TCustomVirtualExplorerCombobox.BackGroundRect(ItemRect: TComboItemRect): TRect;
var
  Size: TSize;
  R: TRect;
  WS: WideString;
begin
  case ItemRect of
    crBackGround:
      begin
        Result := ClientRect;
        if UseThemes and ThemesActive then
          GetThemeBackgroundContentRect(ThemeEdit, Canvas.Handle, EP_EDITTEXT, ETS_NORMAL, Result, @Result)
        else
          InflateRect(Result, -2, -2);
      end;
    crClient:  Result := ClientRect;
    crDropDownButton:
      begin
        Result := BackGroundRect(crBackGround);
        Result.Bottom := Result.Top + (Result.Bottom - Result.Top);
        Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
      end;
    crImage:
      begin
        Result := BackGroundRect(crBackGround);
        Result.Top := (Height - SmallSysImages.Height) div 2;
        Result.Left := Result.Left + 2;
        SetRect(Result,
                Result.Left,
                Result.Top,
                Result.Left + SmallSysImages.Height,
                Result.Top + SmallSysImages.Height);
      end;
    crComboEdit:
      begin
        Result := BackGroundRect(crBackGround);
        if ComboEdit.Text = '' then
          WS := 'C:\'  // Just a dummy to fill a valid rectangle
        else
          WS := ComboEdit.Text;
        Size := TextExtentW(WS, Font);
        if Size.cy < Result.Bottom - Result.Top then
        begin
          Result.Top := Result.Top + (Result.Bottom - Result.Top - Size.cy) div 2;
          Result.Bottom := Result.Top + Size.cy;
        end;
        R := BackGroundRect(crImage);
        Result.Left := Result.Left + (R.Right - R.Left) + 4; // 4 Pixel margin between image and WideEdit
        R := BackGroundRect(crDropDownButton);
        Result.Right := Result.Right - (R.Right - R.Left) - 1;
      end;
  end
end;

function TCustomVirtualExplorerCombobox.GetComboEdit: TComboEdit;
begin
  Result := FComboEdit
end;

procedure TCustomVirtualExplorerCombobox.ButtonClicked(Sender: TObject);
begin
  if psRolledDown in PopupAutoCompleteDropDown.PopupStates then
    PopupAutoCompleteDropDown.RollUp(False)
  else begin
    if Active then
    begin
      HideCaret(ComboEdit.Handle);
      try
        // Clear the selection
        ComboEdit.SelLength := 0;

        PopupExplorerDropDown.SelectOnDropDown := vcboSelectPathOnDropDown in Options;
        if Assigned(EditNamespace) then
          PopupExplorerDropDown.TargetPIDL := PIDLMgr.CopyPIDL(EditNamespace.AbsolutePIDL)
        else
          PopupExplorerDropDown.TargetPIDL := nil;

        PopupExplorerDropDown.RollDown(ComboEdit, Self, nil)
      finally
        if ComboEdit.CanFocus then
        begin
          Windows.SetFocus(ComboEdit.Handle);
          ComboEdit.SelectAll;
          ShowCaret(ComboEdit.Handle)
        end
      end
    end
  end
end;

function TCustomVirtualExplorerCombobox.CalculateEditHeight: integer;

// Calculates the height the ExplorerCombo should be

var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Result := Metrics.tmHeight + I;
end;

procedure TCustomVirtualExplorerCombobox.ChangeLinkChanging(Server: TObject; NewPIDL: PItemIDList);
var
  NS: TNamespace;
  ChangeText: Boolean;
begin
  { Keep from recursively trying to respond to a notify if more than one        }
  { control has been registered with this instance as the client. Once is       }
  { enough.  VT can get out of wack if you try to call selection                }
  { methods recursively.                                                        }
  if not(vcbsNotifyChanging in VETComboState) then
  begin
    Include(FVETComboState, vcbsNotifyChanging);
    try
      if Assigned(NewPIDL) and not(csDesigning in ComponentState) then
      begin
        ChangeText := True;
        if Assigned(EditNamespace) then
          ChangeText := not ILIsEqual(EditNamespace.AbsolutePIDL, NewPIDL);
        if ChangeText then
        begin
          NS := TNamespace.Create(NewPIDL, nil);
          NS.FreePIDLOnDestroy := False;
          EditNamespace := NS;
          NS.Free;
        end
      end else
        EditNamespace := nil;
    finally
      Exclude(FVETComboState, vcbsNotifyChanging);
    end
  end
end;

function TCustomVirtualExplorerCombobox.CanResize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
{ JIM if NewHeight < SmallSysImages.Height  + 4 then
    NewHeight :=  SmallSysImages.Height + 4 }
end;

procedure TCustomVirtualExplorerCombobox.ChangeLinkDispatch(PIDL: PItemIDList);
begin
  VETChangeDispatch.DispatchChange(Self, PIDL);
end;

procedure TCustomVirtualExplorerCombobox.ChangeLinkFreeing(ChangeLink: IVETChangeLink);
{ Called from the ChangeLink manager when a control is unregistering itself     }
{ with the manager.  This is where we can unlink any reference we have to the   }
{ the control being unregistered.                                               }
begin
  if ChangeLink.ChangeLinkClient = Self then
  begin
    if ChangeLink.ChangeLinkServer = FVirtualExplorerTree then
      FVirtualExplorerTree := nil;
  end
end;

procedure TCustomVirtualExplorerCombobox.CMExit(var Message: TCMExit);
begin
  inherited;
  FHotTracking := False;
  Invalidate;
end;

procedure TCustomVirtualExplorerCombobox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if Message.Sender = FComboEdit then begin
    FHotTracking := True;
    Invalidate;
  end;
end;

procedure TCustomVirtualExplorerCombobox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHotTracking := True;
  if not Assigned(MouseTimer) then
  begin
    MouseTimer := TTimer.Create(nil);
    MouseTimer.Enabled := False;
    MouseTimer.Interval := 125;
    MouseTimer.OnTimer := MouseTimerCallback;
    MouseTimer.Enabled := True;
  end;
  Invalidate;
end;

procedure TCustomVirtualExplorerCombobox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (vcbsOverDropDownButton in FVETComboState) then
    Exclude(FVETComboState, vcbsOverDropDownButton);
  if not FComboEdit.Focused then
    FHotTracking := False;
  Invalidate;
end;

procedure TCustomVirtualExplorerCombobox.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  DoFontChange(Font);
end;

constructor TCustomVirtualExplorerCombobox.Create(AOwner: TComponent);
begin
  inherited;
  ControlState := ControlState + [csCreating];

  // Make sure the Icon will fit
 { JIM Constraints.MinHeight := 23; }
  Height := 23;

  FComboEdit := TComboEdit.Create(Self);
  ComboEdit.Parent := Self;
  ComboEdit.BorderStyle := bsNone;
  ComboEdit.ExplorerComboBox := Self;
  ComboEdit.OwnerControl := Self;
  ComboEdit.Enabled := False; // Wait for Activate

  FPopupExplorerDropDown := CreatePopupExplorerDropDown;
  PopupExplorerDropDown.ExplorerCombobox := Self;
  PopupExplorerDropDown.Parent := Self;
  PopupExplorerDropDown.PopupExplorerTree.TreeOptions.PaintOptions :=
    PopupExplorerDropDown.PopupExplorerTree.TreeOptions.PaintOptions + [toShowBackGround];
  PopupExplorerDropDown.PopupExplorerTree.DisableWaitCursors := True;
  PopupExplorerDropDown.Visible := False;

  FPopupExplorerOptions := CreatePopupExplorerOptions;
  PopupExplorerOptions.PopupExplorerDropDown := PopupExplorerDropDown;

  FPopupAutoCompleteDropDown := CreatePopupAutoCompleteDropDown;
  PopupAutoCompleteDropDown.ExplorerCombobox := Self;
  PopupAutoCompleteDropDown.Visible := False;
  PopupAutoCompleteDropDown.Parent := Self;

  FPopupAutoCompleteOptions := CreatePopupAutoCompleteOptions;
  PopupAutoCompleteOptions.PopupAutoCompleteDropDown := PopupAutoCompleteDropDown;

  Visible := True;
  Width := 300;
  ParentColor := False;
  Ctl3D := True;
  ControlState := ControlState - [csCreating];
  FImageIndex := -1;
  Options := DefaultExplorerComboOptions;
  TabStop := True;
  ControlStyle := ControlStyle - [csCaptureMouse];
  FBorderStyle := bsSingle
end;

function TCustomVirtualExplorerCombobox.CreatePopupAutoCompleteOptions: TPopupAutoCompleteOptions;

// Overridable to create a custom version of TPopupAutoCompleteOptions

begin
  Result := TPopupAutoCompleteOptions.Create;
end;

function TCustomVirtualExplorerCombobox.CreatePopupAutoCompleteDropDown: TPopupAutoCompleteDropDown;

// Overridable to create a custom version of TPopupAutoCompleteDropDown

begin
  Result := TPopupAutoCompleteDropDown.Create(nil)
end;

function TCustomVirtualExplorerCombobox.CreatePopupExplorerOptions: TPopupExplorerOptions;

// Overridable to create a custom version of TPopupExplorerOptions

begin
  Result := TPopupExplorerOptions.Create
end;

function TCustomVirtualExplorerCombobox.CreatePopupExplorerDropDown: TPopupExplorerDropDown;

// Overridable to create a custom version of TPopupExplorerOptions

begin
   Result := TPopupExplorerDropDown.Create(nil)
end;

procedure TCustomVirtualExplorerCombobox.CreateWnd;
var
  validDir : Boolean;
begin
  inherited;
  validDir := Assigned (EditNameSpace) and (WideDirectoryExists(EditNamespace.nameParseAddress));
  if (csDesigning in ComponentState) and not (validDir) then
    ComboEdit.Text := Name
  else begin
    if ValidDir then
      ComboEdit.Text := EditNamespace.nameParseAddress
    else
      EditNamespace := DesktopFolder;
  end;
  SetComboEditColor(Color);
  OldFontOnChange := Font.OnChange;
  Font.OnChange := FontChange;
  Perform(WM_THEMECHANGED, 0, 0);
  RefreshComboEdit(False);
end;

destructor TCustomVirtualExplorerCombobox.Destroy;
begin
  // Support Halt( );
  if Assigned(MouseTimer) then
  begin
    MouseTimer.Enabled := False;
    FreeAndNil(FMouseTimer)
  end;
  if Assigned(VETChangeDispatch) then
    VETChangeDispatch.UnRegisterChangeLink(Self, Self, utAll);
  RemoveComponent(ComboEdit);
  ComboEdit.Parent := nil;  // Break the link first, causes weird AV's
  FreeAndNil(FComboEdit); // Make sure nothing references this (like a sizing message)
  PopupAutoCompleteDropDown.Parent := nil;
  FreeAndNil(FPopupAutoCompleteDropDown);
  PopupExplorerDropDown.Parent := nil;
  FreeAndNil(FPopupExplorerDropDown);
  FreeAndNil(FPopupAutoCompleteOptions);   // Make sure popup is freed before Options container
  FreeAndNil(FPopupExplorerOptions);      // Make sure popup is freed before Options container
  FreeAndNil(FEditNamespace);
  inherited;
end;

procedure TCustomVirtualExplorerCombobox.DestroyWnd;
begin
  Font.OnChange := OldFontOnChange;
  FreeThemes;
  inherited;
end;

procedure TCustomVirtualExplorerCombobox.DoDecodeSpecialVariable(
  Variable: WideString; var NS: TNamespace);
begin
  if Assigned(OnDecodeSpecialVariable) then
    OnDecodeSpecialVariable(Self, Variable, NS)
end;

procedure TCustomVirtualExplorerCombobox.DoDraw(ACanvas: TCanvas;
  AClientRect, AButtonRect: TRect; AComboState: TVETComboStates;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self, ACanvas, AClientRect, AButtonRect, AComboState, Stage, DefaultDraw);
end;

procedure TCustomVirtualExplorerCombobox.DoFontChange(NewFont: TFont);
var
  TempHeight: Integer;
begin
  TempHeight := CalculateEditHeight;
  if TempHeight > Height then
    Height := TempHeight;
  // Still need to do this to recenter the icon if necessary
  RealignControls;
  PopupExplorerDropDown.Font.Assign(NewFont);
  PopupAutoCompleteDropDown.Font.Assign(NewFont);
  Invalidate   
end;

procedure TCustomVirtualExplorerCombobox.DoPathChange(SelectedNamespace: TNamespace);
begin
  if Assigned(OnPathChange) then
    OnPathChange(Self, SelectedNamespace);
end;

procedure TCustomVirtualExplorerCombobox.DoPathChanging(NS: TNamespace; var Allow: Boolean);
begin
  Allow := True;
  if Assigned(OnPathChanging) then
    OnPathChanging(Self, NS, Allow);
end;

procedure TCustomVirtualExplorerCombobox.FontChange(Sender: TObject);
{ This is the redirected Change notifier from the TCustomControls Font property }
begin
  if Assigned(OldFontOnChange) then
    OldFontOnChange(Sender);
  DoFontChange(Font);
end;

procedure TCustomVirtualExplorerCombobox.FreeThemes;
begin
  if ThemeCombo <> 0 then
  begin
    CloseThemeData(FThemeCombo);
    ThemeCombo := 0
  end;
  if ThemeButton <> 0 then
  begin
    CloseThemeData(FThemeButton);
    ThemeButton := 0
  end;
  if ThemeEdit <> 0 then
  begin
    CloseThemeData(FThemeEdit);
    ThemeEdit := 0
  end;
end;

function TCustomVirtualExplorerCombobox.GetColor: TColor;
begin
  Result := inherited Color;
end;

function TCustomVirtualExplorerCombobox.GetImageIndex: integer;
begin
  if Assigned(EditNamespace) then
  begin
    Result := EditNamespace.GetIconIndex(False, icSmall, True);
    FImageIndex := -1  // Reset
  end
  else begin
    if csDesigning in ComponentState then
      Result := DefaultSystemImageIndex(diUnknownFile)
    else
      Result := FImageIndex;
  end
end;

function TCustomVirtualExplorerCombobox.GetOnAutoCompleteRollDown: TOnPopupRollDown;
begin
  Result := PopupAutoCompleteDropDown.OnRollDown
end;

function TCustomVirtualExplorerCombobox.GetOnAutoCompleteRollUp: TOnPopupRollUp;
begin
  Result := PopupAutoCompleteDropDown.OnRollUp
end;

function TCustomVirtualExplorerCombobox.GetOnAutoCompleteUpdateList: TOnAutoCompleteUpdateList;
begin
   Result := PopupAutoCompleteDropDown.OnAutoCompleteUpdateList
end;

function TCustomVirtualExplorerCombobox.GetOnChange: TNotifyEvent;
begin
  Result := ComboEdit.OnChange;
end;

function TCustomVirtualExplorerCombobox.GetOnComboRollDown: TOnPopupRollDown;
begin
  Result := PopupExplorerOptions.OnRollDown
end;

function TCustomVirtualExplorerCombobox.GetOnComboRollUp: TOnPopupRollUp;
begin
  Result := PopupExplorerOptions.OnRollUp
end;

function TCustomVirtualExplorerCombobox.GetOnEnter: TNotifyEvent;
begin
  Result := ComboEdit.OnEnter
end;

function TCustomVirtualExplorerCombobox.GetOnExit: TNotifyEvent;
begin
  Result := ComboEdit.OnExit
end;

function TCustomVirtualExplorerCombobox.GetPath: WideString;
begin
  if Assigned(EditNamespace) then
//    if IsWinNT4 then      {1/10/02  Some one has problems with Win2k and this}
      Result := EditNamespace.NameForParsing
//    else
//      Result := EditNamespace.NameParseAddress
  else begin
    if csDesigning in ComponentState then
      Result := Name
    else
      Result := ''
  end
end;

function TCustomVirtualExplorerCombobox.GetTabStop: Boolean;
begin
  Result := ComboEdit.TabStop
end;

procedure TCustomVirtualExplorerCombobox.Loaded;
begin
  inherited;
  RealignControls;
  Invalidate
end;

function TCustomVirtualExplorerCombobox.MouseInDropDownButton: Boolean;
var
  PtTL, PtBR: TPoint;
  R: TRect;
begin
  PtTL := ClientToScreen(FButtonRect.TopLeft);
  PtBR := ClientToScreen(FButtonRect.BottomRight);
  R.TopLeft := PtTL;
  R.BottomRight := PtBR;
  Result := PtInRect(R, Mouse.CursorPos)
end;

procedure TCustomVirtualExplorerCombobox.MouseTimerCallback(Sender: TObject);
var
  P: TPoint;
  R: TRect;
  InControl: Boolean;
begin
  if not (vcbsDropDownButtonPressed in FVETComboState) and GetCursorPos(P) then
  begin
    GetWindowRect(Handle, R);
    InControl := PtInRect(R, P);
    if InControl <> HotTracking then
    begin
      FHotTracking := InControl;
      Invalidate;
    end;

    if not InControl then
    begin
      FMouseTimer.Enabled := False;
      FreeAndNil(FMouseTimer);
    end;
  end;
end;

procedure TCustomVirtualExplorerCombobox.Paint;
begin
  PaintCombo(Canvas.Handle);
end;

procedure TCustomVirtualExplorerCombobox.PaintCombo(PaintDC: HDC);
var
  R: TRect;
  CtlType, CtlState: Longword;
  OldRgn, Region: HRgn;
  OldColor: TColor;
  rgbBk: Longword;
  ACanvas: TCanvas;
  DefaultDraw: Boolean;
  {$IFDEF SpTBX}
  IsHotTracked: Boolean;
  ComboButtonR: TRect;
  {$ENDIF SpTBX}
begin
  // In NT4 the same Region is passed the back from the BeginPaint function as was
  // created in WM_EraseBkgnd.  Newer OS's seem to reset the region after the
  // WMEraseBkgnd and on returning from BeginPaint
  R := ClientRect;
  FButtonRect := BackGroundRect(crDropDownButton);
  ACanvas := TCanvas.Create;
  OldRgn := 0;
  Region := CreateRectRgnIndirect(R);
  try
    OldRgn := SelectObject(PaintDC, Region);
    ACanvas.Handle := PaintDC;

    { PrePaint }
    DefaultDraw := True;
    DoDraw(ACanvas, ClientRect, FButtonRect, FVETComboState, cdPrePaint, DefaultDraw);
    if DefaultDraw then
    begin
      {$IFDEF SpTBX}
      if SkinManager.CurrentSkinName <> 'Default' then
      begin
       OldColor := Brush.Color;
       Brush.Color := Color;
       FillRect(PaintDC, R, Brush.Handle);
       Brush.Color := OldColor;
       IsHotTracked := HotTracking or Focused or PopupExplorerDropDown.Visible;
       ComboButtonR := Rect(FButtonRect.Left, FButtonRect.Top + 1, FButtonRect.Right - 1, FButtonRect.Bottom - 1);
       SpDrawXPComboButton(ACanvas, ComboButtonR, Enabled, IsHotTracked,
         IsHotTracked, PopupExplorerDropDown.Visible, not UseRightToLeftReading, sknSkin);
       SpDrawXPEditFrame(Self, IsHotTracked, sknSkin);
      end else
      {$ENDIF SpTBX}
      if UseThemes and ThemesActive then
      begin
        { Draw the Edit }
        if Enabled then
          DrawThemeBackground(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_NORMAL, R, nil)
        else
          DrawThemeBackground(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_DISABLED, R, nil);
        GetThemeBackgroundContentRect(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_NORMAL, R, @R);
        OldColor := Brush.Color;
        Brush.Color := Color;
        FillRect(PaintDC, R, Brush.Handle);
        Brush.Color := OldColor;

        { Draw the DropDown Button }
        CtlType := CP_DROPDOWNBUTTON;
        if Enabled then
          CtlState := CBXS_NORMAL
        else
          CtlState := CBXS_DISABLED;
        if (vcbsDropDownButtonPressed in FVETComboState) then
          CtlState := CBXS_PRESSED;
        if vcbsOverDropDownButton in FVETComboState then
          CtlState := CBXS_HOT;
        DrawThemeBackground(ThemeCombo, PaintDC, CtlType, CtlState, FButtonRect, nil)
      end else
      begin
        { Draw the Edit }
        if FBorderStyle = bsNone then
        begin
          OldColor := Brush.Color;
          Brush.Color := Color;
          FillRect(PaintDC, R, Brush.Handle);
          Brush.Color := OldColor;
        end else
        begin
          if Flat then
            DrawEdge(PaintDC, R, EDGE_SUNKEN, BF_RECT or BF_FLAT)
          else
            DrawEdge(PaintDC, R, EDGE_SUNKEN, BF_RECT)
        end;
        { Draw the DropDown Button }
        CtlType := DFC_SCROLL;
        CtlState := DFCS_SCROLLCOMBOBOX;
        if (vcbsDropDownButtonPressed in FVETComboState) or Flat then
          CtlState := CtlState or DFCS_FLAT;
        DrawFrameControl(PaintDC, FButtonRect, CtlType, CtlState);
      end;
    end;

    { PostPaint }
    DefaultDraw := True;
    DoDraw(ACanvas, ClientRect, FButtonRect, FVETComboState, cdPostPaint, DefaultDraw);
    if DefaultDraw then begin
      if UseThemes and ThemesActive then
      begin
        { Draw the focus }
        if (Style = scsDropDownList) and (ComboEdit.Focused) then
        begin
          GetThemeBackgroundContentRect(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_NORMAL, R, @R);
          SubtractRect(R, R, BackGroundRect(crDropDownButton));
          InflateRect(R, -2, -2);
          OldColor := Brush.Color;
          Brush.Color := clHighLight;
          FillRect(PaintDC, R, Brush.Handle);
          Brush.Color := OldColor;
          DrawFocusRect(PaintDC, R);
        end;
        { Draw the Image }
        if Active and (not ComboEdit.IsEditing or (csDesigning in ComponentState)) then
        begin
          R := BackGroundRect(crImage);
          if Enabled then
            DrawThemeIcon(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_NORMAL, R, SmallSysImages.Handle, ImageIndex)
          else
            DrawThemeIcon(ThemeEdit, PaintDC, EP_EDITTEXT, ETS_DISABLED, R, SmallSysImages.Handle, ImageIndex);
        end;
      end else
      begin
        { Draw the focus }
        R := BackGroundRect(crBackGround);
        SubtractRect(R, R, BackGroundRect(crDropDownButton));
        InflateRect(R, -1, -1);
        Inc(R.Bottom);
        if (Style = scsDropDownList) and (ComboEdit.Focused) then
        begin
          OldColor := Brush.Color;
          Brush.Color := clHighLight;
          FillRect(PaintDC, R, Brush.Handle);
          Brush.Color := OldColor;
          DrawFocusRect(PaintDC, R);
          rgbBk := ColorToRGB(clHighLight);
        end else
          rgbBk := ColorToRGB(Color);

        { Draw the Image }
        if Active and (not ComboEdit.IsEditing or (csDesigning in ComponentState)) then
        begin
          R := BackGroundRect(crImage);
          ImageList_DrawEx(SmallSysImages.Handle, ImageIndex, PaintDC,
            R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, rgbBk, CLR_NONE, ILD_NORMAL);
        end;
      end;
    end;

  finally
    if OldRgn <> 0 then
    begin
      SelectObject(PaintDC, OldRgn);
      DeleteObject(Region);
    end;
    ACanvas.Handle := 0;
    ACanvas.Free;
  end
end;

procedure TCustomVirtualExplorerCombobox.RealignControls;
var
  R: TRect;
begin
  if Assigned(ComboEdit) then
  begin
    R := BackGroundRect(crComboEdit);
    if not EqualRect(R, BoundsRect) then
      ComboEdit.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  end
end;

procedure TCustomVirtualExplorerCombobox.RefreshComboEdit(SelectText: Boolean);
var
  WS: WideString;
begin
  if Assigned(EditNamespace) then
  begin
    case TextType of
      ecbtNameOnly: WS := EditNamespace.NameNormal;
      ecbtFullPath:
        begin
          WS := EditNamespace.NameParseAddress;
          // Network paths may return nothing
          if WS = '' then
            WS := EditNamespace.NameNormal
        end;
    end;
  end else
    WS := '';

  if (csDesigning in ComponentState) and (WS = '') then
    WS := Name
  else begin
    if not FActive then
      WS := '';
  end;

  ComboEdit.Text := WS;
  if SelectText and (Style = scsDropDown) then
    ComboEdit.SelectAll;
//    ComboEdit.SelectText(0, -1);
  ComboEdit.Invalidate
end;

procedure TCustomVirtualExplorerCombobox.SetColor(const Value: TColor);
begin
  inherited Color := Value;
  if not (csCreating in ControlState) then
    SetComboEditColor(Value);
  PopupExplorerDropDown.Color := Color;
end;

procedure TCustomVirtualExplorerCombobox.SetComboEditColor(NewColor: TColor);
begin
  if Assigned(ComboEdit) then
    ComboEdit.Color := NewColor;
end;

procedure TCustomVirtualExplorerCombobox.SetEditNamespace(const Value: TNamespace);
var
  Allow: Boolean;
begin
  if Value <> FEditNamespace then
  begin
    DoPathChanging(EditNamespace, Allow);
    if Allow then
    begin
      if Assigned(Value) then
      begin
        if not Value.OkToBrowse(False) then
          Exit;
      end;

      FreeAndNil(FEditNamespace);
      if Assigned(Value) then
        FEditNamespace := Value.Clone(True);

      if Active then
      begin
        RefreshComboEdit(True);
        Invalidate;
        Update;
        if Assigned(FEditNamespace) {and not(vcbsNotifyChanging in VETComboState)} then
          ChangeLinkDispatch(FEditNamespace.AbsolutePIDL);
      end;

      DoPathChange(FEditNamespace);
    end
  end;
end;

procedure TCustomVirtualExplorerCombobox.SetEnabled(Value: Boolean);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if ComboEdit.Enabled <> Value then
    begin
      ComboEdit.Enabled := Value;
      Invalidate
    end
  end
end;

procedure TCustomVirtualExplorerCombobox.SetName(const Value: TComponentName);
begin
  inherited;
  if (csDesigning in ComponentState) and Assigned(ComboEdit) then
    ComboEdit.Text := Value; // VCL is limited to ANSI character so this is ok (and necessary)
end;

procedure TCustomVirtualExplorerCombobox.SetOnAutoCompleteRollDown(
  const Value: TOnPopupRollDown);
begin
  PopupAutoCompleteDropDown.OnRollDown := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnAutoCompleteRollUp(const Value: TOnPopupRollUp);
begin
  PopupAutoCompleteDropDown.OnRollUp := Value
end;

function TCustomVirtualExplorerCombobox.GetOnAutoCompleteAddItem: TVirtualAutoCompleteAddItem;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.AutoComplete.OnAutoCompleteAddItem
end;

procedure TCustomVirtualExplorerCombobox.SetOnAutoCompleteAddItem(
  const Value: TVirtualAutoCompleteAddItem);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.AutoComplete.OnAutoCompleteAddItem := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnAutoCompleteUpdateList(
  const Value: TOnAutoCompleteUpdateList);
begin
  PopupAutoCompleteDropDown.OnAutoCompleteUpdateList := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnChange(const Value: TNotifyEvent);
begin
  ComboEdit.OnChange := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnComboRollDown(const Value: TOnPopupRollDown);
begin
  PopupExplorerDropDown.OnRollDown := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnComboRollUp(const Value: TOnPopupRollUp);
begin
  PopupExplorerDropDown.OnRollUp := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnEnter(const Value: TNotifyEvent);
begin
  ComboEdit.OnEnter := Value
end;

procedure TCustomVirtualExplorerCombobox.SetOnExit(const Value: TNotifyEvent);
begin
  ComboEdit.OnExit:= Value
end;

procedure TCustomVirtualExplorerCombobox.SetOptions(const Value: TVETComboOptions);

   function BitChanged(Old, New: TVETComboOptions; Bit: TVETComboOption): Boolean;
   begin
     Result := ((Bit in Old) and not(Bit in New)) or ((Bit in New) and not(Bit in Old))
   end;

var
  TempOld: TVETComboOptions;
begin
  if FOptions <> Value then
  begin
    TempOld := FOptions; // Save old and set FOptions to new value incase called methods need to know new setting
    FOptions := Value;
    if BitChanged(TempOld, Value, vcboThemeAware) then
    begin
      if vcboThemeAware in Value then
      begin
        PopupAutoCompleteOptions.Options := PopupAutoCompleteOptions.Options + [poThemeAware];
        PopupExplorerOptions.Options := PopupExplorerOptions.Options + [poThemeAware]
      end else
      begin
        PopupAutoCompleteOptions.Options := PopupAutoCompleteOptions.Options - [poThemeAware];
        PopupExplorerOptions.Options := PopupExplorerOptions.Options - [poThemeAware]
      end;
      ThemesActive := UseThemes and (vcboThemeAware in Options);
      Invalidate
    end;

    if BitChanged(TempOld, Value, vcboThreadedImages) then
    begin
      if vcboThreadedImages in Value then
        PopupExplorerDropDown.PopupExplorerTree.TreeOptions.VETImageOptions :=
          PopupExplorerDropDown.PopupExplorerTree.TreeOptions.VETImageOptions + [toThreadedImages]
      else
        PopupExplorerDropDown.PopupExplorerTree.TreeOptions.VETImageOptions :=
          PopupExplorerDropDown.PopupExplorerTree.TreeOptions.VETImageOptions - [toThreadedImages]
    end;
  end;
end;

procedure TCustomVirtualExplorerCombobox.SetStyle(const Value: TShellComboStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    ComboEdit.Style := Value
  end;
end;

procedure TCustomVirtualExplorerCombobox.SetTabStop(const Value: Boolean);
begin
  ComboEdit.TabStop := Value
end;

procedure TCustomVirtualExplorerCombobox.SetTextType(const Value: TExplorerComboboxText);
begin
  if FTextType <> Value then
  begin
    FTextType := Value;
    RefreshComboEdit(False)
  end;
end;

procedure TCustomVirtualExplorerCombobox.SetVirtualExplorerTree(const Value: TCustomVirtualExplorerTree);
begin
  if FVirtualExplorerTree <> Value then
  begin
    if Assigned(FVirtualExplorerTree) then
      VETChangeDispatch.UnRegisterChangeLink(FVirtualExplorerTree, Self, utLink );
    FVirtualExplorerTree := Value;
    if Assigned(FVirtualExplorerTree) then
      VETChangeDispatch.RegisterChangeLink(FVirtualExplorerTree, Self, ChangeLinkChanging, ChangeLinkFreeing);
  end;
end;

procedure TCustomVirtualExplorerCombobox.UpdateDropDownButtonState;
begin
  if MouseInDropDownButton then
  begin
    if not (vcbsOverDropDownButton in FVETComboState) then
    begin
      if vcbsDropDownButtonPressPending in FVETComboState then
      begin
        Exclude(FVETComboState, vcbsDropDownButtonPressPending);
        Include(FVETComboState, vcbsDropDownButtonPressed);
      end;
      Include(FVETComboState, vcbsOverDropDownButton);
      Invalidate
    end
  end else
  begin
    if (vcbsOverDropDownButton in FVETComboState) then
    begin
      if vcbsDropDownButtonPressed in FVETComboState then
      begin
        Include(FVETComboState, vcbsDropDownButtonPressPending);
        Exclude(FVETComboState, vcbsDropDownButtonPressed);
      end;
      Exclude(FVETComboState, vcbsOverDropDownButton);
      Invalidate;
    end
  end
end;

procedure TCustomVirtualExplorerCombobox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  R: TRect;    
begin
  // Don't paint the background in the border area
  R := BackGroundRect(crBackGround);
  IntersectClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);

  // Don't paint where the button is
  R := BackGroundRect(crDropDownButton);
  ExcludeClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);

  if Assigned(ComboEdit) then
    // Don't paint where the icon is, unless it should be cleared
    if not ComboEdit.IsEditing or (csDesigning in ComponentState) then
    begin
      R := BackGroundRect(crImage);
      ExcludeClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);
    end;

  // Don't paint where the ComboEdit is
  R := BackGroundRect(crComboEdit);
  ExcludeClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);
  inherited;
end;

procedure TCustomVirtualExplorerCombobox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if Message.XPos < ComboEdit.Left then
    ButtonClicked(Self);
end;

procedure TCustomVirtualExplorerCombobox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if MouseInDropDownButton then
  begin
    if psRolledDown in PopupExplorerDropDown.PopupStates then
      PopupExplorerDropDown.RollUp(False)
    else begin
      Include(FVETComboState, vcbsDropDownButtonPressed);
      Invalidate;
      Mouse.Capture := Handle;
      Invalidate;
      Update;
      ButtonClicked(Self);
    end
  end
end;

procedure TCustomVirtualExplorerCombobox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  Exclude(FVETComboState, vcbsOverDropDownButton);
  Exclude(FVETComboState, vcbsDropDownButtonPressed);
  Exclude(FVETComboState, vcbsDropDownButtonPressPending);
  ReleaseCapture;
end;
procedure TCustomVirtualExplorerCombobox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  UpdateDropDownButtonState;
end;

procedure TCustomVirtualExplorerCombobox.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  UpdateDropDownButtonState
end;

procedure TCustomVirtualExplorerCombobox.WMPrintClient(var Message: TWMPrintClient);
begin
  inherited;
  if Message.DC <> 0 then
    PaintCombo(Message.DC);
end;

procedure TCustomVirtualExplorerCombobox.WMSetFocus(
  var Message: TWMSetFocus);
begin
  Message.Result := 0;
  if ComboEdit.CanFocus then
    ComboEdit.SetFocus
end;

procedure TCustomVirtualExplorerCombobox.WMSize(var Message: TWMSize);
begin   
  inherited;
{ JIM  if Message.Height < SmallSysImages.Height + 4 then
    Message.Height := SmallSysImages.Height + 4;  }
  RealignControls;
end;

procedure TCustomVirtualExplorerCombobox.WMThemeChanged(var Message: TMessage);
begin
  inherited;
  FreeThemes;
  ThemesActive := UseThemes and (vcboThemeAware in Options);
  if ThemesActive then
  begin
    ThemeCombo := OpenThemeData(Handle, 'combobox');
    ThemeButton := OpenThemeData(Handle, 'button');
    ThemeEdit := OpenThemeData(Handle, 'edit');
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
  end
end;

procedure TCustomVirtualExplorerCombobox.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
begin
  inherited;
{JIM  if Message.WindowPos.flags and SWP_NOSIZE = 0 then
   if Message.WindowPos.cy < SmallSysImages.Height + 6 then
      Message.WindowPos.cy := SmallSysImages.Height + 6};
end;


procedure TCustomVirtualExplorerCombobox.SetPath(const Value: WideString);
var
  PIDL: PItemIdList;
  NS: TNamespace;
begin
  if (csDesigning in ComponentState) and (Value = '') then
  begin
    EditNamespace := nil;
    ComboEdit.Text := Name
  end else
  begin
    PIDL := PathToPIDL(Value);
    if Assigned(PIDL) then
    begin
      NS := TNamespace.Create(PIDL, nil);
      EditNamespace := NS;
      NS.Free
    end else
      PIDLMgr.FreeAndNilPIDL(PIDL)
  end
end;

function TCustomVirtualExplorerCombobox.GetPopupExplorerTree: TPopupExplorerTree;
begin
  if Assigned(PopupExplorerDropDown) then
    Result := PopupExplorerDropDown.PopupExplorerTree
  else
    Result := nil;
end;

function TCustomVirtualExplorerCombobox.GetOnEnumFolder: TVETOnEnumFolder;
begin
  Result := PopupExplorerOptions.OnEnumFolder
end;

procedure TCustomVirtualExplorerCombobox.SetOnEnumFolder(
  const Value: TVETOnEnumFolder);
begin
  PopupExplorerOptions.OnEnumFolder := Value
end;

function TCustomVirtualExplorerCombobox.GetAutoComplete: TVirtualShellAutoComplete;
begin
  Result := nil;
  if Assigned(PopupAutoCompleteDropDown) then
    if Assigned(PopupAutoCompleteDropDown.PopupAutoCompleteTree) then
      Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.AutoComplete;
end;

procedure TCustomVirtualExplorerCombobox.SetBorderStyle(
  const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end
end;

procedure TCustomVirtualExplorerCombobox.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    PopupExplorerDropDown.RemoteScrollbar.Flat := Value;
    PopupAutoCompleteDropDown.RemoteScrollbar.Flat := Value;
    Invalidate;
  end
end;

procedure TCustomVirtualExplorerCombobox.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    RefreshComboEdit(True);
    Invalidate;
    Update;
    if FActive and Assigned(FEditNamespace) then
      ChangeLinkDispatch(FEditNamespace.AbsolutePIDL);
    ComboEdit.Enabled := Value
  end
end;

{ TComboEdit }

procedure TComboEdit.CMSysColorChange(var Message: TWMSysColorChange);
begin
  inherited;
  { Hack to make the Edit change color when the system colors change }
  Color := clNone;
  Color := OwnerControl.Color
end;

procedure TComboEdit.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    EN_CHANGE:
      begin
        if not IsEditing then
        begin
          {$IFDEF GXDEBUG_ADDRESSBAR}
          SendDebug('EN_CHANGE: Editing Beginning');
          {$ENDIF}

          IsEditing := True;
          UndoBufferSave;
          if Assigned(OwnerControl) then
            OwnerControl.Invalidate
        end ;
        // This is notifiction is fired anytime the text changes only respond if the
        // text changed due to a key stroke
        if KeyPressed and ExplorerCombobox.Active and not SelectingAll then
        begin
          {$IFDEF GXDEBUG_ADDRESSBAR}
          SendDebug('EN_CHANGE: Keypressed, ComboActive, not SelectingAll');
          {$ENDIF}
          
          HandleDropDowns(ddAutoComplete);
        end;
        FSelectingAll := False
      end;   
  end;
  inherited;
end;

constructor TComboEdit.Create(AOwner: TComponent);
begin
  inherited;          
end;

procedure TComboEdit.CreateWnd;
begin
  inherited;
  IsEditing := False;
end;

procedure TComboEdit.DefaultOnInvalidEntry(InvalidText: WideString);
var
  H: THandle;
begin
  if Assigned(Application) then
  begin
    if Assigned(Application.MainForm) then
      H := Application.MainForm.Handle
    else
      H := Application.Handle
  end else
    H := 0;
  WideShowMessage(H, S_PATH_ERROR, PWideChar('"' + InvalidText + '"' + S_COMBOEDIT_DEFAULT_ERROR1))
end;

destructor TComboEdit.Destroy;
begin
  FOldNameSpace.Free;
  inherited;   
end;

procedure TComboEdit.DoOnInvalidEntry(InvalidPath: WideString);
begin
  if Assigned(OwnerControl.OnInvalidEntry) then
    OwnerControl.OnInvalidEntry(OwnerControl, InvalidPath)
  else
    DefaultOnInvalidEntry(InvalidPath)
end;

function TComboEdit.GetText: WideString;
begin
  Result := inherited Text
end;

procedure TComboEdit.HandleDropDowns(DropDown: TDropDown);
begin       
  if DropDown = ddAutoComplete then
  begin
    if not ({(Text = '') or} ReadOnly) then
    begin
      ExplorerComboBox.PopupExplorerDropDown.RollUp(False);
      ExplorerComboBox.PopupAutoCompleteDropDown.RollDown(Self, nil, nil);
  //    ExplorerComboBox.PopupAutoCompleteDropDown.PopupAutoCompleteTree.UpdateList(Text);
    end else
    begin
      // Don't show the dropdowns if the edit is empty or readonly
      ExplorerComboBox.PopupExplorerDropDown.RollUp(False);
      ExplorerComboBox.PopupAutoCompleteDropDown.RollUp(False);
    end;

    ExplorerComboBox.PopupAutoCompleteDropDown.PopupAutoCompleteTree.UpdateList(Text);
  end else
  begin
    ExplorerComboBox.PopupAutoCompleteDropDown.RollUp(False);
    ExplorerComboBox.PopupExplorerDropDown.RollDown(OwnerControl, nil, nil);
  end;
end;

procedure TComboEdit.PaintWindow(DC: HDC);
begin
  inherited;
  DrawFocusRect(DC, Parent.ClientRect);
end;

procedure TComboEdit.SelectEnteredPath;


    function AutoCompleteDrive(TestPath: WideString): WideString;
    // Fills in the drive if the user only entered a drive lettter or the letter and semicolon
    // C -> C:\
    // C: -> C:\
    begin
      if (Length(TestPath) < 3) and (Length(TestPath) > 0) then
      begin
        if Length(TestPath) = 1 then  // Just assume the first char is ok
          Result := TestPath + WideString(':\')
        else
          Result := TestPath + WideString('\')
      end else
        Result := TestPath
    end;

    procedure HandleInvalidText(TestPath: WideString);
    var
      WP: PWideChar;
      OldWP: WideChar;
      Index: integer;
      Done: Boolean;
    begin
      DoOnInvalidEntry(TestPath);
      Done := False;
      Index := -1;
      WP := WideStrScan(PWideChar(TestPath), WideChar('\'));
      while Assigned(WP) and not Done do
      begin
        OldWP := WP^;
        WP^ := WideNull;
        try
          Done := not WideDirectoryExists(TestPath);
          if not Done then
            Index := lstrlenW(PWideChar(TestPath));
        finally
          WP^ := OldWP;
        end;
        if WP^ <> WideNull then
          Inc(WP, 2);
        WP := WideStrScan(WP, WideChar('\'));
      end;
      SelStart := Index + 1;
      SelLength := Length(Text);
    end;

var
  PIDL: PItemIDList;
  WS: WideString;
  NS: TNamespace;
begin
  if Style = scsDropDown then
  begin
    { Make a copy of where we are in case we must restore it on an error }
    UndoBufferSave;

    NS := nil;
    { Get what is entered }
    WS := Text;
    if WS <> '' then
    begin
      WS := AutoCompleteDrive(WS);
      // Lets see if it is a UNC Path First
      if (Length(WS) > 2) and (WS[1] = '\') and (WS[2] = '\') then
      begin
          PIDL := PathToPIDL(WS, Application.MainForm.Handle, True);
          if Assigned(PIDL) then
          begin
            NS := TNamespace.Create(PIDL, nil);
            OwnerControl.EditNamespace := NS;  // EditNamespace makes a clone
          end else
            HandleInvalidText(WS);
      end else
      { If it is an true folder we can do it the easy way }
      if WideDirectoryExists(WS) then
      begin
        PIDL := PathToPIDL(WS);
        if Assigned(PIDL) then
        begin
          NS := TNamespace.Create(PIDL, nil);
          OwnerControl.EditNamespace := NS;  // EditNamespace makes a clone
        end;
        SelectAll;
      end else
      { If it is a special folder name it is also easy }
      if OwnerControl.VirtualExplorerTree.FindFolderByName(WS, NS) then
      begin
        OwnerControl.EditNamespace := NS ;
        SelectAll;
      end else
      if FileExistsW(WS) then
      { If it is a file then it is a bit different.  We execute the file but don't }
      { notify any linked controls or tell the Combobox about it                   }
      begin
        PIDL := PathToPIDL(WS);
        if Assigned(PIDL) then
        begin
          OwnerControl.EditNamespace := nil;
          OwnerControl.FImageIndex := DefaultSystemImageIndex(diMyDocuments);
          NS := TNamespace.Create(PIDL, nil);
          NS.ShellExecuteNamespace('', '');
          FreeAndNil(NS);
          Text := WS;
          SelectAll;
        end;
      end else
      if OwnerControl.VirtualExplorerTree.FindDesktopFolderByName(WS, NS) then
      { See if is an item on the desktop }
      begin
        if NS.Folder then
        { If it is a folder then it is easy }
        begin
          OwnerControl.EditNamespace := NS;
          SelectAll;
        end else
        if FileExistsW(NS.NameParseAddress) then
        { If it is a file then it is a bit different.  We execute the file but don't }
        { notify any linked controls or tell the Combobox about it                   }
        begin
          OwnerControl.EditNamespace := nil;
          OwnerControl.FImageIndex := DefaultSystemImageIndex(diMyDocuments);
          NS.ShellExecuteNamespace('', '');
          FreeAndNil(NS);
          Text := WS;
          SelectAll;
        end;
      end else
      if IsSpecialVariable(WS, NS) then
         OwnerControl.EditNamespace := NS  // EditNamespace makes a clone
      else begin
        OwnerControl.DoDecodeSpecialVariable(WS, NS);
        if Assigned(NS) then
          OwnerControl.EditNamespace := NS  // EditNamespace makes a clone
        else
          HandleInvalidText(WS);
      end
    end;
    NS.Free;  // Assigning to EditNamespace always clones the namespace so free it
  end
end;

procedure TComboEdit.SetFocus;
begin
  inherited;
end;

procedure TComboEdit.SetOldNamespace(const Value: TNamespace);
begin
  if Assigned(FOldNamespace) then
    FreeAndNil(FOldNamespace);
  FOldNamespace := Value;
end;

procedure TComboEdit.SetStyle(const Value: TShellComboStyle);
begin
  FStyle := Value;
  if Value = scsDropDownList then
  begin
    ReadOnly := True;
    Cursor := crArrow;
  end else
  begin
    ReadOnly := False;
    Cursor := crDefault;
  end
end;

procedure TComboEdit.SetText(const Value: WideString);
begin
  inherited Text := Value;
  IsEditing := False;
end;

procedure TComboEdit.UndoBufferRestore;
begin
  if Assigned(OwnerControl) and Assigned(OldNamespace) then
    OwnerControl.EditNamespace := OldNamespace.Clone(True)
end;

procedure TComboEdit.UndoBufferSave;
begin
  FreeAndNil(FOldNamespace);
  if Assigned(OwnerControl) and Assigned(OwnerControl.EditNamespace) then
    OldNamespace := OwnerControl.EditNamespace.Clone(True)
end;

procedure TComboEdit.WMChar(var Message: TWMChar);
begin
  {$IFDEF GXDEBUG_ADDRESSBAR}
  SendDebug('WM_CHAR: Code = ' + IntToStr(Message.CharCode) + ' = ' + Char(Message.CharCode));
  {$ENDIF}
  if Message.CharCode in [VK_F16] then    // F16 seems to be Control-Backspace
  begin
    SelectingAll := True; // don't let the autocomplete drop down
    Text := WideExtractFilePath(WideStripTrailingBackslash(Text));
    SelStart := Length(Text);
    SelLength := 0;
  end else
  if Message.CharCode in [VK_RETURN] then
  begin
    Message.CharCode := Ord(#0); // No beep
    inherited;
    Message.CharCode := VK_RETURN
  end else
  if Message.CharCode in [VK_LBUTTON] then // Ctrl-A send this code????
  begin
    SelectingAll := True;
    inherited;
  end else
    inherited;

  if ExplorerCombobox.Active then
  begin
    if Message.CharCode in [VK_RETURN] then
    begin
      // If there is no Autocomplete dropdown then the Return key will propogate to here
      SelectEnteredPath
    end else
    begin
      // VK_BACK does not trigger an EN_CHANGE in the rich edit
      if Message.CharCode in [VK_BACK, VK_DELETE] then
        HandleDropDowns(ddAutoComplete);
    end;
  end
end;

procedure TComboEdit.WMComboSelectAll(var Msg: TMessage);
begin
  SelStart := 0;
  SelLength := Length(Text);
end;

procedure TComboEdit.WMKeyDown(var Message: TWMKeyDown);
begin
{  if Message.CharCode in [VK_RETURN] then
    // If there is no Autocomplete dropdown then the Return key will propogate to here
    SelectEnteredPath
  else begin
    inherited;
    // VK_BACK does not trigger an EN_CHANGE in the rich edit
    if Message.CharCode in [VK_BACK, VK_DELETE] then
      HandleDropDowns(ddAutoComplete)
    else
      KeyPressed := True;
  end     }     

  KeyPressed := True;
  if Message.CharCode = VK_F4 then
    OwnerControl.PopupExplorerDropDown.RollDown(Self, OwnerControl, nil);
  inherited;
end;

procedure TComboEdit.WMKeyUp(var Message: TWMKeyUp);
begin
  inherited;
  KeyPressed := False;
end;

procedure TComboEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Color := ColorOldWindow;
  Font.Color := ColorOldFont;
  Parent.Invalidate;
end;

procedure TComboEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Style = scsDropDownList then
    HandleDropDowns(ddExplorer);
end;

procedure TComboEdit.WMPaint(var Message: TWMPaint);
begin
  if (Style = scsDropDownList) and Focused then
    HideCaret(Handle);
  inherited;
end;

procedure TComboEdit.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if CanFocus then
  begin
    ColorOldWindow := Color;
    ColorOldFont := Font.Color;
    CursorOld := Cursor;
    if Style = scsDropDown then
    begin
      PostMessage(Handle, WM_VETCOMBO_SELECTALL, 0, 0);
     // SelectAll // does not work with Tnt
    end
    else begin
      Cursor := crArrow;
      Font.Color := clHighlightText;
      Color := clHighlight;
      Parent.Invalidate;
    end
  end
end;

procedure TComboEdit.WMSysKeyDown(var Message: TWMSysKeyDown);
begin
  case Message.CharCode of
    VK_DOWN:
       if Assigned(OwnerControl) then
         HandleDropDowns(ddExplorer);
  end;
  inherited;
end;


{ TPopupExplorerDropDown }

constructor TPopupExplorerDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FPopupExplorerTree := CreatePopupExplorerTree;
  PopupExplorerTree.PopupExplorerDropDown := Self;
  AutoScrollWindow := PopupExplorerTree;
  PopupExplorerTree.Parent := Self;
  PopupExplorerTree.ScrollbarOptions.ScrollBars := ssNone;
  PopupExplorerTree.BevelEdges := [];
  PopupExplorerTree.BevelInner := bvNone;
  PopupExplorerTree.BevelOuter := bvNone;
  PopupExplorerTree.BorderStyle := bsNone;
  PopupExplorerTree.FileObjects := DefaultExplorerComboFileObjects;
  PopupExplorerTree.DefaultNodeHeight := 17;
  WheelMouseTarget := PopupExplorerTree;
  Visible := False;
  DropDownCount := 8;
  ComboBoxStyle := cbsClassic;
end;

destructor TPopupExplorerDropDown.Destroy;
begin
  // Support Halt( );
  if Assigned(PIDLMgr) then
    PIDLMgr.FreeAndNilPIDL(FTargetPIDL);
  inherited;
end;

procedure TPopupExplorerDropDown.DoRollUp(Selected: Boolean);
var
  NS: TNamespace;
begin
  PIDLMgr.FreeAndNilPIDL(FTargetPIDL);
  if Selected then
  begin
    if PopupExplorerTree.ValidateNamespace(PopupExplorerTree.GetFirstSelected, NS) then
      ExplorerCombobox.EditNamespace := NS
  end;
  PopupExplorerTree.Active := False;
  inherited;
end;

procedure TPopupExplorerDropDown.RealignChildWindows(NewWidth, NewHeight: integer);

// Reposition any new child windows we added in the decentant, namely the VET here

var
  X, Y, W, H: integer;
begin
  inherited;
  Y := 0;

  if RemoteScrollbar.Visible then
    W := NewWidth - RemoteScrollbar.Width - BORDER * 2
  else begin
    if GetTopWindow(Handle) <> Grabber.Handle then
       BringWindowToTop(Grabber.Handle);
    W := NewWidth - BORDER * 2;
  end;

  H := NewHeight - BORDER * 2;
  if (psLeftScrollbar in PopupStates) and RemoteScrollbar.Visible then
    X := RemoteScrollbar.Width
  else
    X := 0;
  PopupExplorerTree.SetBounds(X, Y, W, H);
end;

procedure TPopupExplorerDropDown.RefreshScrollbar;

// Keep VET in sync with the Scrollbar component

var
  R: TRect;
begin
  inherited;
  if PopupExplorerTree.Height > 0 then
  begin
    R := PopupExplorerTree.GetTreeRect;
    RemoteScrollbar.Min := 0;
    RemoteScrollbar.Max := R.Bottom;
    RemoteScrollbar.PageSize := PopupExplorerTree.ClientHeight;
    RemoteScrollbar.SmallChange := PopupExplorerTree.ScrollBarOptions.VerticalIncrement;
    RemoteScrollbar.LargeChange := PopupExplorerTree.ClientHeight;
    RemoteScrollbar.Position := Abs(PopupExplorerTree.OffsetY);
    // RedrawWindow leaves some "residue" when expanding an extended Combobox's nodes
    // around the thumb as is shrinks
    InvalidateRect(RemoteScrollbar.Handle, nil, True);
    UpdateWindow(RemoteScrollbar.Handle);
    if R.Bottom - PopupExplorerTree.ClientHeight = 0 then
    begin
      RemoteScrollbar.Visible := False;
      Grabber.Transparent := True
    end else
    begin
       RemoteScrollbar.Visible := True;
       Grabber.Transparent := False
    end
  end;
end;

function TPopupExplorerDropDown.RowHeight: Cardinal;

// Used to calculate the initial size based on the number of Rows. The Rows property
// times the result of the function will be used for the initial height

begin
  Result := PopupExplorerTree.DefaultNodeHeight
end;

procedure TPopupExplorerDropDown.SetComboBoxStyle(const Value: TComboBoxStyle);
begin
  if FComboBoxStyle <> Value then
  begin
    FComboBoxStyle := Value;
    with PopupExplorerTree.TreeOptions do
    if Value = cbsVETEnhanced then
      PaintOptions := PaintOptions + [toShowButtons, toShowTreeLines]
    else
      PaintOptions := PaintOptions - [toShowButtons, toShowTreeLines];
  end
end;

procedure TPopupExplorerDropDown.WMVScroll(var message: TWMVScroll);
begin
  inherited;
  if (Message.ScrollCode = SB_THUMBTRACK) or (Message.ScrollCode = SB_THUMBPOSITION) then
  begin
    PopupExplorerTree.OffsetY := -Message.Pos
  end else
    PopupExplorerTree.Perform(WM_VSCROLL, TMessage(Message).wParam, TMessage(Message).lParam);
  RefreshScrollbar
end;

function TPopupExplorerDropDown.CreatePopupExplorerTree: TPopupExplorerTree;

// Overridable so a decendant of TPopupExplorerTree may be created

begin
  Result := TPopupExplorerTree.Create(Self);
end;

procedure TPopupExplorerDropDown.KeyPressDispatch(var Message: TMessage; var Handled: Boolean);

// Handle some Keystrokes and pass them on to the VET for scrolling with the keyboard

var
  Node: PVirtualNode;
begin
  with TWMKey(Message) do
  begin
    // Explorer seems to map Tab to Arrow Down
    if CharCode = VK_TAB then
      CharCode := VK_DOWN;
    if (CharCode in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_RIGHT, VK_LEFT, VK_ADD,
      VK_SUBTRACT]) then
    begin
      Node := PopupExplorerTree.GetFirstSelected;
      if not Assigned(Node) then
      begin
        Node := PopupExplorerTree.GetFirst;
        PopupExplorerTree.Selected[Node] := True;
      end else
      begin
        // Let VT handle the scrolling
        PopupExplorerTree.Perform(Message.Msg, Message.wParam, Message.lParam);
        Node := PopupExplorerTree.GetFirstSelected;
        PopupExplorerTree.ScrollIntoView(Node, False, toAutoScrollHorz in PopupExplorerTree.TreeOptions.VETMiscOptions);
        RefreshScrollbar;
        Handled := True
      end
    end else
    if CharCode in [VK_RETURN] then
      RollUp(True)
    else
      RollUp(False)
  end
end;

function TPopupExplorerDropDown.AllowClickInWindow(Window: HWnd;
  Point: TPoint): Boolean;
begin
  // This will be handled by the ExplorerCombo box as so not to allow a second click
  // to redrop the window.  It is closed in the ButtonClick method of the ExplorerCombo
  Result := ExplorerCombobox.MouseInDropDownButton
end;

procedure TPopupExplorerDropDown.DoRollDownInit;
begin
  inherited;
  PopupExplorerTree.Active := True;
  if Assigned(TargetPIDL) then
  begin
    PopupExplorerTree.BrowseToByPIDL(TargetPIDL, True, SelectOnDropDown, False, False);
    if not SelectOnDropDown then
      PopupExplorerTree.ScrollIntoView(PopupExplorerTree.GetFirst, False, toAutoScrollHorz in PopupExplorerTree.TreeOptions.VETMiscOptions)
  end else
    // Select the Desktop
    PopupExplorerTree.BrowseToByPIDL(DesktopFolder.AbsolutePIDL, SelectOnDropDown, True, False, False);
  // Keyboard interface works better with a focused node (we are suppressing the painting of the focus rect though)
  PopupExplorerTree.FocusedNode := PopupExplorerTree.GetFirstSelected;
end;

function TPopupExplorerDropDown.GetPopupOptions: TPopupOptions;
begin
  Result := inherited PopupOptions
end;

procedure TPopupExplorerDropDown.SetPopupOptions(const Value: TPopupOptions);
begin
  inherited PopupOptions := Value;
  with PopupExplorerTree.TreeOptions do
    if poThemeAware in Value then
    begin
      PaintOptions := PaintOptions + [toThemeAware];
      RemoteScrollbar.Options := RemoteScrollbar.Options + [soThemeAware]
    end else
    begin
      PaintOptions := PaintOptions - [toThemeAware];
      RemoteScrollbar.Options := RemoteScrollbar.Options - [soThemeAware]
    end
end;

{ TDropDownWnd }

function TDropDownWnd.AllowClickInWindow(Window: HWnd; Point: TPoint): Boolean;
begin
  Result := False;
  // Return true to pervent TDropDownWnd from closing the poupup if the mouse is
  // clicked in the passed window
end;

procedure TDropDownWnd.AnimateRollDown;
var
  BackBits: TBitmap;
  i, StepSize: integer;
  ScreenDC: hDC;
  R: TRect;
  Flags: Longword;
  Animate: BOOL;
begin
  { Respect the systems settings for animation }
  if poRespectSysAnimationFlag in PopupOptions then
    SystemParametersInfo(SPI_GETCOMBOBOXANIMATION, 0, @Animate, 0)
  else
    Animate := True;
  if (poAnimated in PopupOptions) and Animate then
  begin
    if AnimationSpeed > 0 then
    begin
      if Assigned(AnimateWindow) then
      begin
        if psAboveHostControl in PopupStates then
          Flags := AW_SLIDE or AW_VER_NEGATIVE
        else
          Flags := AW_SLIDE or AW_VER_POSITIVE;
        AnimateWindow(Handle, AnimationSpeed, Flags)
      end else
      begin
        BackBits := TBitmap.Create;
        ScreenDC := GetDC(0);
        BackBits.Canvas.Lock;
        try
          BackBits.Width := Width;
          BackBits.Height := Height;
          { Fill the Canvas with the background color of the form.                  }
          BackBits.Canvas.Brush.Color := Color;
          SetRect(R, 0, 0, BackBits.Width, BackBits.Height);
          BackBits.Canvas.FillRect(R);
          Perform(WM_PRINT, BackBits.Canvas.Handle, PRF_ERASEBKGND or PRF_NONCLIENT or PRF_CLIENT or PRF_CHILDREN);
          StepSize := BackBits.Height div 16;
          if psAboveHostControl in PopupStates then
          begin
            for i := 1 to 16 do
            begin
              BitBlt(ScreenDC, Left, Top + BackBits.Height - I * StepSize,
                BackBits.Width, I * StepSize, BackBits.Canvas.Handle, 0, 0, SRCCOPY);
              Sleep(AnimationSpeed div 16);
            end
          end else
          begin
            for i := 1 to 16 do
            begin
              BitBlt(ScreenDC, Left, Top, BackBits.Width, i * StepSize,
                BackBits.Canvas.Handle, 0, BackBits.Height - i * StepSize, SRCCOPY);
              Sleep(AnimationSpeed div 16);
            end;
          end;
          SetWindowPos(Handle, 0, Left, Top, Width, Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
        finally
          ReleaseDC(Handle, ScreenDC);
          BackBits.Canvas.Unlock;
          BackBits.Free;
        end
      end
    end
  end;
  Visible := True;
  Include(FPopupStates, psRolledDown);
end;

procedure TDropDownWnd.AutoPositionPopup(AControl: TWinControl;
  InitialExtents: PPoint);
var
  Pt: TPoint;
  OldTop: integer;
  BorderW: Cardinal;
  PotentialW,
  PotentialH: integer;
begin
  Exclude(FPopupStates, psBelowHostControl);
  Exclude(FPopupStates, psAboveHostControl);
  { Handle the Width and Height for the first time or if always should  }
  if not ((poPersistentSizing in PopupOptions) and (psDroppedOnce in PopupStates)) then
  begin
    PotentialW := AControl.Width;
    if (poSizeable in PopupOptions) then
      BorderW := GetSystemMetrics(SM_CYSIZEFRAME)
    else
      BorderW := GetSystemMetrics(SM_CYFRAME);
    PotentialH := RowHeight * DropDownCount + BorderW;
    if Assigned(InitialExtents) then
    begin
      if InitialExtents.x > 0 then
        Width := InitialExtents.x
      else
        Width := PotentialW;
      if InitialExtents.y > 0 then
        Height := InitialExtents.y
      else
        Height := PotentialH
    end else
    begin
      Width := PotentialW;
      Height := PotentialH
    end
  end;
  { Now handle the height }
  if Assigned(AControl) then
  begin
    Pt.x := 0;
    Pt.y := AControl.Height;
    Pt := AControl.ClientToScreen(Pt);
    if Pt.y + Height > Screen.Height then
    begin
      { Won't fit under Host Control }
      if Height > Pt.y - AControl.Height then
      begin
        { Won't fit over Host Control either }
        if Pt.y - AControl.Height > Screen.Height - Pt.y then
        begin
          { More room on top, make it fit}
          Height := Pt.y - AControl.Height;
          Pt.y := Pt.y - AControl.Height - Height;
          Include(FPopupStates, psAboveHostControl)
        end else
        begin
          { More room on bottom make it fit}
          Height := Screen.Height - Pt.y;
          Include(FPopupStates, psBelowHostControl)
        end
      end else
      begin
        { Will fit over Host Control }
        Include(FPopupStates, psAboveHostControl);
        Pt.y := Pt.y - AControl.Height - Height
      end
    end else
    begin
      { Will fit under Host Control }
      Include(FPopupStates, psBelowHostControl)
    end;
    Top := Pt.y;
    Left := Pt.x;
  end;
  { Bit of a hack.  When using ScrollIntoView on an invisible window it         }
  { appears to have difficulty deciding what to do. Eventually during a         }
  { GetDisplayRect call in VT it asks for an IconIndex and the node data        }
  { retrieved from the node is garbage (the namespace) and it crashes           }
  { To work around move the window off the screen, make it visible then call    }
  { the methods that fire ScrollIntoView.                                       }
  OldTop := Top;
  Top := Screen.Height;
  Visible := True;
  DoRollDownInit;
  Visible := False;
  Top := OldTop;
end;

procedure TDropDownWnd.AutoScrollTimerCallback(Window: hWnd; Msg,
  idEvent: integer; dwTime: Longword);
var
  Dir: Word;
begin
  if Assigned(AutoScrollWindow) then
  begin
    Dir := $FFFF;
    if psScrollingUp in FPopupStates then
      Dir := SB_LINEUP
    else
    if psScrollingDown in FPopupStates then
      Dir := SB_LINEDOWN;
    if Dir <> $FFFF then
      AutoScrollWindow.Perform(WM_VSCROLL, MAKELONG(Dir, 0), 0);
    RefreshScrollbar
  end
end;

procedure TDropDownWnd.BitBltGrabber(Canvas: TCanvas; Host: TWinControl; BkGndColor: TColor);

  // BitBlts the image of the passed Grabber bits passed in BackBits (assumes 0, 0)
  // to the Windows Canvas taking into account which corner the Grabber should be in

var
  BitsOrigin: TPoint;
  BackBits: TBitmap;
  R: TRect;
  Pt: TPoint;
begin
  if (Grabber.Width > 0) and (Grabber.Height > 0) then
  begin
    BackBits := TBitmap.Create;
    try
      BackBits.Width := Grabber.Width;
      BackBits.Height := Grabber.Height;
      if psLeftScrollbar in PopupStates then
      begin
        if psBelowHostControl in PopupStates then
        begin
          BitsOrigin.x := 0;
          BitsOrigin.y := Host.Height - Grabber.Height;
        end else
        begin
          BitsOrigin.x := 0;
          BitsOrigin.y := 0;
        end
      end else
      begin
        if psBelowHostControl in PopupStates then
        begin
          BitsOrigin.x := Host.Width - Grabber.Width;
          BitsOrigin.y := Host.Height - Grabber.Height;
        end else
        begin
          BitsOrigin.x := Host.Width - Grabber.Width;
          BitsOrigin.y := -1; // I don't understand why this must be -1
        end
      end;

      Backbits.Canvas.Brush.Color := BkGndColor;
      SetRect(R, 0, 0, Grabber.Width, Grabber.Height);

      SetViewportOrgEx(BackBits.Canvas.Handle, -BitsOrigin.x, -BitsOrigin.y, @Pt);
      Host.Perform(WM_PRINTCLIENT, wParam(BackBits.Canvas.Handle), PRF_ERASEBKGND);
      SetViewportOrgEx(BackBits.Canvas.Handle, Pt.x, Pt.y, @Pt);

      Grabber.PaintGrabber(Backbits.Canvas.Handle);

      BitBlt(Canvas.Handle, BitsOrigin.x, BitsOrigin.y, Grabber.Width, Grabber.Height, Backbits.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      BackBits.Free
    end
  end
end;

function TDropDownWnd.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if NewWidth < Grabber.Width then
    NewWidth := Grabber.Width;
  if NewHeight < Grabber.Height then
    NewHeight := Grabber.Height;
  Result := inherited CanResize(NewWidth, NewHeight);
end;

constructor TDropDownWnd.Create(AOwner: TComponent);
begin
  inherited;
 // DoubleBuffered := True;
  BevelEdges := [];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  RemoteScrollbar := TOwnerDrawScrollbar.Create(Self);
  RemoteScrollbar.Parent := Self;
  RemoteScrollbar.OwnerControl := Self;
  { The RemoteScrollbar's parent must be set in the decentant class }
  RemoteScrollbar.Width := GetSystemMetrics(SM_CYVSCROLL);
  Grabber := TSizeGrabber.Create(Self);
  Grabber.Parent := Self;
  Grabber.Width := GetSystemMetrics(SM_CYVSCROLL);
  Grabber.Height := GetSystemMetrics(SM_CYVSCROLL);
  PopupOptions := [poThemeAware, poRespectSysAnimationFlag];
  FAutoScrollTimerStub := CreateStub(Self, @TDropDownWnd.AutoScrollTimerCallback);
  AutoScrollSlowTime := 200;
  AutoScrollFastTime := 10;
  AnimationSpeed := 200;
  PopupOptions := DefaultPopupOptions;
end;

procedure TDropDownWnd.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER and not WS_VISIBLE;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST or WS_EX_NOPARENTNOTIFY;
    AddBiDiModeExStyle(ExStyle);
    if UseRightToLeftScrollbar then
      Include(FPopupStates, psLeftScrollbar);
    // Can't use CS_SAVEBITS on systems that don't support AnimateWindow
    // The bitmap animation used in TDropDownWnd causes the window to save the
    // slid down window bitmap under the real window after it is shown instead of
    // the true image below
    if Assigned(AnimateWindow) then
      Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS
  end;
end;

destructor TDropDownWnd.Destroy;
begin
  inherited;
  if Assigned(FAutoScrollTimerStub) then
    DisposeStub(FAutoScrollTimerStub);
end;

procedure TDropDownWnd.DoRollDown(var Allow: Boolean);
begin
  if Assigned(OnRollDown) then
    OnRollDown(Self, Allow);
end;

procedure TDropDownWnd.DoRollDownInit;
begin
  if Assigned(OnRollDownInit) then
    OnRollDownInit(Self)
end;

procedure TDropDownWnd.DoRollUp(Selected: Boolean);
begin
  ScrollTimerDestroy(True);
  if Assigned(OnRollUp) then
    OnRollUp(Self, Selected)
end;

procedure TDropDownWnd.DropDownMessageLoop;
// This needs to be done so in an ActiveX control the drop down will catch
// NC Mouse messages and roll up.  The messages are not caught by the TApplication
// or the Parent Controls message loop.
var
  Msg: TMsg;
  ScreenRect: TRect;
  Handled: Boolean;
  AMsg: TMessage;
  DoRollUp, DoSelect, ScrollbarDirty: Boolean;
  Form: TCustomForm;
begin
  ScrollbarDirty := False;
  DoRollUp := False;
  DoSelect := False;

  {$IFDEF GXDEBUG_ADDRESSBAR}
  SendDebug('Entering Message Loop');
  {$ENDIF}

  // Get the Focused window right so the mouse wheel works
  Form := GetParentForm(Self);
  if Assigned(Form) and Assigned(WheelMouseTarget) then
  begin
    if WheelMouseTarget.CanFocus then
      Form.SetFocusedControl(WheelMouseTarget);
  end;
  // Won't cancel dropdown in ocx if focus WheelMouseTarget
//  WheelMouseTarget.SetFocus;

  while GetMessage(Msg, 0, 0, 0) and (psRolledDown in FPopupStates) do
  begin
    Handled := False;
    case Msg.message of
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
        begin
          Windows.GetClientRect(Handle, ScreenRect);
          Windows.ClientToScreen(Handle, ScreenRect.TopLeft);
          Windows.ClientToScreen(Handle, ScreenRect.BottomRight);
          if not PtInRect(ScreenRect, Msg.pt) then
          begin
            RollUp(DoSelect);
            Handled := True
          end
        end;
      WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK:
        begin
          // Must do it before calling Dispatch or the container may be moved
          // before the dropdown is hidden
          RollUp(DoSelect)
        end;
      WM_CANCELMODE, WM_SYSKEYDOWN:
        RollUp(DoSelect);
      WM_KEYDOWN, WM_CHAR:
      begin
        if Msg.wParam in [VK_ESCAPE, VK_RETURN] then
        begin
          DoRollUp := True;
          DoSelect := Msg.wParam = VK_RETURN;
          Handled := True
        end;
        AMsg.Msg := Msg.message;
        AMsg.lParam := Msg.lParam;
        AMsg.wParam := Msg.wParam;
        KeyPressDispatch(AMsg, Handled);
      end;
      WM_MOUSEWHEEL:
      begin
        ScrollbarDirty := True;
      end;
      // WM_ACTIVATEAPP is SENT to the Windows so there is a message handler
      // there for this possiblility
     end;

    if not Handled then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg)
    end;

    if ScrollbarDirty then
    begin
      ScrollbarDirty := False;
      RefreshScrollbar
    end;

    if DoRollUp then
      RollUp(DoSelect)
  end;

  {$IFDEF GXDEBUG_ADDRESSBAR}
  SendDebug('Exiting Message Loop');
  {$ENDIF}
end;

function TDropDownWnd.GetScrolling: Boolean;
begin
  Result := [psFastScroll, psSlowScroll] * PopupStates <> []
end;

function TDropDownWnd.GrabberPolyRgn(Grabber: TSizeGrabber; Host: TWinControl): HRgn;

// Creates a triangular Clipping region in the appropriate corner of the
// window to either exclude it from drawing or clip to it when drawing the
// image
// The Host is assumed to be a child of the TDropDownWnd

var
  R: TRect;
  PtArray: array[0..2] of TPoint;
begin
  R := Grabber.BoundsRect;

  if psLeftScrollbar in PopupStates then
  begin
    if psBelowHostControl in PopupStates then
    begin
      PtArray[0].x := R.Left;
      PtArray[0].y := R.Top;
      PtArray[1].x := R.Right;
      PtArray[1].y := R.Bottom;
      PtArray[2].x := R.Left;
      PtArray[2].y := R.Bottom
    end else
    begin
      PtArray[0].x := R.Left;
      PtArray[0].y := R.Top;
      PtArray[1].x := R.Right;
      PtArray[1].y := R.Top;
      PtArray[2].x := R.Left;
      PtArray[2].y := R.Bottom
    end
  end else
  begin
    if psBelowHostControl in PopupStates then
    begin
      PtArray[0].x := R.Left;
      PtArray[0].y := R.Bottom;
      PtArray[1].x := R.Right;
      PtArray[1].y := R.Top;
      PtArray[2] := R.BottomRight
    end else
    begin
      PtArray[0].x := R.Right;
      PtArray[0].y := R.Top;
      PtArray[1].x := R.Right;
      PtArray[1].y := R.Bottom;
      PtArray[2].x := R.Left;
      PtArray[2].y := R.Top
    end
  end;
  Result := CreatePolygonRgn(PtArray, 3, WINDING);
end;

procedure TDropDownWnd.KeyPressDispatch(var Message: TMessage; var Handled: Boolean);
begin
  // Called on every WM_KEYDOWN, WMCHAR, and WM_KEYUP message.  If decendants want
  // to handle these keys in a new child window without causing the control that owns
  // the popup to loose focus override this then pass the message to the child window
  // using SendMessage or Perform
  // If Handled then the default window procedure for the control parent window is not
  // called.
  // Note the Return and Escape key will never be seen here.  These call Roll up and
  // the result will be seen in DoRollUp Accept parameter

  Handled := False
end;

procedure TDropDownWnd.RealignChildWindows(NewWidth, NewHeight: integer);
var
  X, Y, W, H: integer;
begin
  if psBelowHostControl in PopupStates then
  begin
    if psLeftScrollbar in PopupStates then
    begin  // Below - Left
      // Common dimensions
      X := 0;
      W := GetSystemMetrics(SM_CXVSCROLL);
      // Grabber dimensions
      if poSizeable in PopupOptions then
        H := GetSystemMetrics(SM_CYVSCROLL)
      else
        H := 0;
      Y := NewHeight - H - BORDER * 2;
      Grabber.SetBounds(X, Y, W, H);
      // RemoteScrollbar dimensions
      Y := 0;
      H := NewHeight - Grabber.Height - BORDER * 2;
      RemoteScrollbar.SetBounds(X, Y, W, H);
    end else
    begin  // Below - Right
      // Common dimensions
      X := NewWidth - GetSystemMetrics(SM_CXVSCROLL) - BORDER * 2;
      W := GetSystemMetrics(SM_CXVSCROLL);
      // Grabber dimensions
      if poSizeable in PopupOptions then
        H := GetSystemMetrics(SM_CYVSCROLL)
      else
        H := 0;
      Y := NewHeight - H - BORDER * 2;
      Grabber.SetBounds(X, Y, W, H);
      // RemoteScrollbar dimensions
      Y := 0;
      H := NewHeight - Grabber.Height - BORDER * 2;
      RemoteScrollbar.SetBounds(X, Y, W, H);
    end
  end else
  // Above
  begin
    if psLeftScrollbar in PopupStates then
    begin  // Above - Left
      X := 0;
      W := GetSystemMetrics(SM_CXVSCROLL);
      // Grabber dimensions
      if poSizeable in PopupOptions then
        H := GetSystemMetrics(SM_CYVSCROLL)
      else
        H := 0;
      Y := -1;
      Grabber.SetBounds(X, Y, W, H);
      // RemoteScrollbar dimensions
      Y := Grabber.Height - BORDER;
      H := NewHeight - Grabber.Height - BORDER;
      RemoteScrollbar.SetBounds(X, Y, W, H);
    end else
    begin  // Above - Right
      X := NewWidth - GetSystemMetrics(SM_CXVSCROLL) - BORDER * 2;
      W := GetSystemMetrics(SM_CXVSCROLL);
      // Grabber dimensions
      if poSizeable in PopupOptions then
        H := GetSystemMetrics(SM_CYVSCROLL)
      else
        H := 0;
      Y := -1;
      Grabber.SetBounds(X, Y, W, H);
      // RemoteScrollbar dimensions
      Y := Grabber.Height - BORDER;
      H := NewHeight - Grabber.Height - BORDER;
      RemoteScrollbar.SetBounds(X, Y, W, H);
    end
  end;
end;

procedure TDropDownWnd.RefreshScrollbar;
begin     
end;

procedure TDropDownWnd.RollDown(AnOwnerControl, AlignmentControl: TWinControl; Extents: PPoint);
var
  Allow: Boolean;
  OldCursor: TCursor;
begin
  if Assigned(AnOwnerControl) and not(psRolledDown in PopupStates) and (poEnabled in PopupOptions) then
  begin
    Allow := True;
    DoRollDown(Allow);
    if Allow then
    try
      {$IFDEF GXDEBUG_ADDRESSBAR}
      SendDebug('Rolling Down DropDown Window');
      {$ENDIF}
      FOwnerControl := AnOwnerControl;
      Include(FPopupStates, psRolledDown);
      if not Assigned(AlignmentControl) then
        AlignmentControl := AnOwnerControl;
      // Hook the Parent Control and the Application
      if not (csDesigning in ComponentState) then
      begin
        OldCursor := Screen.Cursor;
        Screen.Cursor := crHourglass;
        try
          AutoPositionPopup(AlignmentControl, Extents);
          AnimateRollDown;
          Include(FPopupStates, psDroppedOnce);
        finally
          Screen.Cursor := OldCursor;
        end;
        DropDownMessageLoop
      end
    except
      RollUp(False);
      Exclude(FPopupStates, psRolledDown);
      Exclude(FPopupStates, psAboveHostControl);
      Exclude(FPopupStates, psBelowHostControl);
      Exclude(FPopupStates, psDroppedOnce);
    end
  end
end;

procedure TDropDownWnd.RollUp(Select: Boolean);
var
  ParentForm: TCustomForm;
begin
  {$IFDEF GXDEBUG_ADDRESSBAR}
  SendDebug('Rolling Up DropDown Window');
  if psRolledDown in PopupStates then
    SendDebug('psRolledDown in PopupStates')
  else
    SendDebug('psRolledDown not in PopupStates');
  {$ENDIF}
  if psRolledDown in PopupStates then
  begin
    Mouse.Capture := 0;
    Exclude(FPopupStates, psRolledDown);
    Visible := False;
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) then
    begin
      ParentForm.Invalidate;
      ParentForm.Update;
      if Assigned(OwnerControl) and OwnerControl.CanFocus then
        ParentForm.SetFocusedControl(OwnerControl)
    end;
    if OwnerControl.CanFocus then
      OwnerControl.SetFocus;
    DoRollUp(Select);
    if not (csDesigning in ComponentState) then
    begin
      // Force a repaint to remove any lingering reminants of the dropdown
      // (Problem in Win9x)
      if Assigned(ParentForm) and (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
      begin
        SetWindowPos(ParentForm.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
        RedrawWindow(ParentForm.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_ALLCHILDREN);
      end;
    end;
    // Just in case someone forgets
    if Scrolling then
      ScrollTimerDestroy(True);
  end
end;

function TDropDownWnd.RowHeight: Cardinal;
begin
  Result := 17
end;

procedure TDropDownWnd.ScrollTimerCreate(FastScroll: Boolean);
begin
  if FastScroll then
  begin
    Include(FPopupStates, psFastScroll);
    Exclude(FPopupStates, psSlowScroll);
    FAutoScrollTimer := SetTimer(Handle, ID_TIMER_AUTOSCROLL, AutoScrollFastTime, FAutoScrollTimerStub);
  end else
  begin
    Include(FPopupStates, psSlowScroll);
    Exclude(FPopupStates, psFastScroll);
    FAutoScrollTimer := SetTimer(Handle, 100, AutoScrollSlowTime, FAutoScrollTimerStub);
  end
end;

procedure TDropDownWnd.ScrollTimerDestroy(ClearFlags: Boolean);
begin
  if FAutoScrollTimer <> 0 then
    KillTimer(Handle, FAutoScrollTimer);
  FAutoScrollTimer := 0;
  if ClearFlags then
  begin
    Exclude(FPopupStates, psScrollingUp);
    Exclude(FPopupStates, psScrollingDown);
    Exclude(FPopupStates, psSlowScroll);
    Exclude(FPopupStates, psFastScroll);
  end
end;

procedure TDropDownWnd.SetDropDownCount(const Value: Cardinal);
begin
  if FDropDownCount <> Value then
  begin
    FDropDownCount := Value;
    if HandleAllocated then
      RefreshScrollBar
  end
end;

procedure TDropDownWnd.SetPopupOptions(const Value: TPopupOptions);
begin
  FPopupOptions := Value;
end;

procedure TDropDownWnd.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE) and (Message.ActiveWindow <> 0) then
    SendMessage(Message.ActiveWindow, WM_NCACTIVATE, 1, 0);
end;

procedure TDropDownWnd.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
    RollUp(False);
end;

procedure TDropDownWnd.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  // Don't draw the background;
  Message.Result := 1;
end;

procedure TDropDownWnd.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TDropDownWnd.WMPrint(var Message: TWMPrint);
var
  i: integer;
  Size: TPoint;
begin
  // In XP the inhertied should call all child windows but after the window is
  // dropped down once and resized AnimateWindow seems to forget to call the
  // Scrollbar? So lets do it ourselves.
  if Message.Flags and PRF_CHILDREN <> 0 then
  begin
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i] is TWinControl then
      begin
        SetViewportOrgEx(Message.DC, Controls[i].Left + BORDER, Controls[i].Top + BORDER, @Size);
        with TMessage( Message) do
          Controls[i].Perform(Msg, wParam, lParam);
        SetViewportOrgEx(Message.DC, Size.x, Size.y, @Size);
      end
    end
  end
end;

procedure TDropDownWnd.WMUpdateScrollbar(var Message: TMessage);
begin
  RefreshScrollbar
end;

procedure TDropDownWnd.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  // Make sure the cy and cy parameters are valid
  if (Message.WindowPos.Flags and SWP_NOSIZE) = 0 then
    RealignChildWindows(Message.WindowPos.cx, Message.WindowPos.cy);
  RefreshScrollbar
end;

{ TSizeGrabber }

constructor TSizeGrabber.Create(AOwner: TComponent);
begin
  inherited;
  OwnerDropDown := AOwner as TDropDownWnd;
end;

procedure TSizeGrabber.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Tabstop := False;
  with Params do
  begin
    Style := WS_CHILD {or SBS_SIZEBOXBOTTOMRIGHTALIGN or SBS_SIZEGRIP};
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
  ControlStyle := ControlStyle - [csFramed];
end;

procedure TSizeGrabber.CreateWnd;
begin
  inherited;
  Perform(WM_THEMECHANGED, 0, 0);
end;

destructor TSizeGrabber.Destroy;
begin
  if FThemeScrollbar <> 0 then
    CloseThemeData(FThemeScrollbar);
  inherited;
end;

procedure TSizeGrabber.Paint;
begin
  inherited;
  // If the grabber is not accompanied by the scrollbar the window that the
  // Grabber is drawn over needs to paint the grabber so the transparncy works
  // right
  if not Transparent then
    PaintGrabber(Canvas.Handle);
end;

procedure TSizeGrabber.PaintGrabber(DC: hDC);
var
  Flags: Longword;
  X1, X2: integer;
  Bitmap: TBitmap;
  ImageList: TImageList;
  {$IFDEF SpTBX}
  C1, C2: TColor;
  ACanvas: TCanvas;
  {$ENDIF}
begin
  if (Height > 0) and (Width > 0) then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      {$IFDEF SpTBX}
      if SkinManager.CurrentSkinName <> 'Default' then
      begin
        ACanvas := TCanvas.Create;
        ACanvas.Handle := DC;
        try
          CurrentSkin.PaintBackground(ACanvas, ClientRect, skncSplitter, sknsNormal, True, False, False);
          C1 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color1;
          C2 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color2;
          BeginPath(DC);
          MoveToEx(ACanvas.Handle, ClientRect.Right, ClientRect.Bottom, nil);
          LineTo(ACanvas.Handle, ClientRect.Right, ClientRect.Top);
          LineTo(ACanvas.Handle, ClientRect.Left, ClientRect.Bottom);
          LineTo(ACanvas.Handle, ClientRect.Right, ClientRect.Bottom);
          EndPath(ACanvas.Handle);
          SelectClipPath(ACanvas.Handle, RGN_COPY);
          SpDrawXPGrip(ACanvas, ClientRect, C1, C2);
        finally
          ACanvas.Handle := 0;
          ACanvas.Free
        end
      end else
      {$ENDIF}
      if ThemesActive then
      begin
        X1 := 0;
        X2 := Height;
        if psLeftScrollbar in OwnerDropDown.PopupStates then
          Flags := SZB_LEFTALIGN
        else
          Flags := SZB_RIGHTALIGN;
        if psAboveHostControl in OwnerDropDown.PopupStates then
        begin
          X1 := Height;
          X2 := -Height;
        end;

        if Transparent then
        begin
          DrawThemeBackground(ThemeScrollbar, Bitmap.Canvas.Handle, SBP_SIZEBOX, Flags, ClientRect, nil);

          // Flip horizontally if necessary
          StretchBlt(Bitmap.Canvas.Handle, 0, X1, Width, X2, Bitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);

          // The only transparent bit blasting I could get working
          ImageList := TImageList.Create(nil);
          try
            ImageList.Width := Width;
            ImageList.Height := Height;
            ImageList.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, 0]);
            ImageList_Draw(ImageList.Handle, 0, DC, 0, 0, ILD_TRANSPARENT);
          finally
            ImageList.Free;
          end
        end else
        begin
          DrawThemeBackground(ThemeScrollbar, Bitmap.Canvas.Handle, SBP_SIZEBOX, Flags, ClientRect, nil);
          // StretchBlt can flip a bitmap with a '-' sign between rectangles
          StretchBlt(DC, 0, X1, Width, X2, Bitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
        end
      end else
      begin
        X1 := 0;
        X2 := Height;
        if psLeftScrollbar in OwnerDropDown.PopupStates then
          Flags := DFCS_SCROLLSIZEGRIPRIGHT
        else
          Flags := DFCS_SCROLLSIZEGRIP;
        if psAboveHostControl in OwnerDropDown.PopupStates then
        begin
          X1 := Height;
          X2 := -Height;
        end;

        if Transparent then
        begin
          // StretchBlt can flip a bitmap with a - sign between rectangles!
          DrawFrameControl(Bitmap.Canvas.Handle, ClientRect, DFC_SCROLL, Flags);

          // Flip horizontally if necessary
          StretchBlt(Bitmap.Canvas.Handle, 0, X1, Width, X2, Bitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);

          // The only transparent bit blasting I could get working
          ImageList := TImageList.Create(nil);
          try
            ImageList.Width := Width;
            ImageList.Height := Height;
            ImageList.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, 0]);
            ImageList_Draw(ImageList.Handle, 0, DC, 0, 0, ILD_TRANSPARENT);
          finally
            ImageList.Free;
          end
        end else
        begin
          // StretchBlt can flip a bitmap with a - sign between rectangles!
          DrawFrameControl(Bitmap.Canvas.Handle, ClientRect, DFC_SCROLL, Flags);

          // Flip horizontally if necessary
          StretchBlt(DC, 0, X1, Width, X2, Bitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
        end
      end
    finally
      Bitmap.Free;
    end
  end
end;

procedure TSizeGrabber.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  // Don't draw the background;
  Message.Result := 1;
end;

procedure TSizeGrabber.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  Captured := True;
  DragStartPos := SmallPointToPoint(Message.Pos);
end;

procedure TSizeGrabber.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  Captured := False;
end;

procedure TSizeGrabber.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TSizeGrabber.WMMouseMove(var Message: TWMMouseMove);
var
  DeltaW,
  DeltaH: integer;
  ParentForm: TCustomForm;
begin
  inherited;
  if Captured then
  begin
    DeltaW := Message.XPos - DragStartPos.x;
    DeltaH := Message.YPos - DragStartPos.Y;

    // MoveWindow works much cleaner than SetWindowPos as far as not flickering as much

    if psBelowHostControl in OwnerDropDown.PopupStates then
    begin
      if psLeftScrollbar in OwnerDropDown.PopupStates then   // Bottom - Left
        MoveWindow(Parent.Handle, Parent.Left + DeltaW, Parent.Top, Parent.Width - DeltaW, Parent.Height + DeltaH, True)
      else                                                   // Bottom - Right
        MoveWindow(Parent.Handle, Parent.Left, Parent.Top, Parent.Width + DeltaW, Parent.Height + DeltaH, True)
    end else
    begin
       if psLeftScrollbar in OwnerDropDown.PopupStates then  // Left - Top
         MoveWindow(Parent.Handle, Parent.Left + DeltaW, Parent.Top + DeltaH, Parent.Width - DeltaW, Parent.Height - DeltaH, True)
       else                                                  // Right - Top
         MoveWindow(Parent.Handle, Parent.Left, Parent.Top + DeltaH, Parent.Width + DeltaW, Parent.Height - DeltaH, True)
    end;

    // Keeps scrollbar refreshed quickly, else it blurs
    Parent.Invalidate;
    Parent.Update;

    // The popup leaves reminents of itself on the form and controls that linger for
    // up to several seconds if it is not refreshed.
    ParentForm := GetParentForm(Self);
    ParentForm.Invalidate;
    ParentForm.Update;
    Sleep(10);  // Let other application repaint
  end;
end;


procedure TSizeGrabber.WMPrintClient(var Message: TWMPrintClient);
begin
  inherited;
  PaintGrabber(Message.DC);
end;

procedure TSizeGrabber.WMSetCursor(var Message: TWMSetCursor);
var
  Cursor: hCursor;
begin
  if psAboveHostControl in OwnerDropDown.PopupStates then
  begin
    if (psLeftScrollbar in OwnerDropDown.PopupStates) then
      Cursor := LoadCursor(0, IDC_SIZENWSE)
    else
      Cursor := LoadCursor(0, IDC_SIZENESW);
  end else
    if (psLeftScrollbar in OwnerDropDown.PopupStates) then
      Cursor := LoadCursor(0, IDC_SIZENESW)
    else
      Cursor := LoadCursor(0, IDC_SIZENWSE);
  if Cursor <> 0 then
    SetCursor(Cursor);
end;

procedure TSizeGrabber.WMThemeChanged(var Message: TMessage);
begin
  if FThemeScrollbar <> 0 then
    CloseThemeData(FThemeScrollbar);
  ThemesActive := (poThemeAware in OwnerDropDown.PopupOptions) and UseThemes;
  if ThemesActive then
  begin
    FThemeScrollbar := OpenThemeData(Handle, 'scrollbar');
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
  end
end;


{ TPopupExplorerTree }

function TPopupExplorerTree.BrowseToByPIDL(APIDL: PItemIDList;
  ExpandTarget, SelectTarget, SetFocusToVET,
  CollapseAllFirst: Boolean; ShowAllSiblings: Boolean = True): Boolean;
var
  DriveNode: PVirtualNode;
begin
  BeginUpdate;
  try
    Active := True;
    { Collapse the whole tree and free all nodes }
    ResetNode(GetFirst);
   { Now actually find and select the target Node }
    Result := inherited BrowseToByPIDL(APIDL, ExpandTarget, SelectTarget,
      SetFocusToVET, False);
    FocusedNode := nil;
    { Always expand the Drives Node (My Computer) }
    if Assigned(DrivesFolder) then
    begin
      DriveNode := WalkPIDLToNode(DrivesFolder.AbsolutePIDL, False, True, False, ShowAllSiblings);
      if Assigned(DriveNode) then
        Expanded[DriveNode] := True;
    end;
  finally
    EndUpdate;
    ScrollIntoView(InternalWalkPIDLToNode(APIDL), False);
  end
end;

constructor TPopupExplorerTree.Create(AOwner: TComponent);
begin
  inherited;
  Indent := 10;
  // Use the Font.Color Property
  TreeOptions.VETFolderOptions := TreeOptions.VETFolderOptions + [toNoUseVETColorsProp];
end;

procedure TPopupExplorerTree.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Don't clip the Grabber sibling when it is to be transparent
  Params.Style := Params.Style and not WS_CLIPSIBLINGS
end;

destructor TPopupExplorerTree.Destroy;
begin
  inherited;

end;

procedure TPopupExplorerTree.DoCollapsed(Node: PVirtualNode);
begin
  inherited;
  // We can't just update it here as the tree has not quite finished updating its
  // new Rect size yet and we calcualate wrong
  PostMessage(PopupExplorerDropDown.Handle, WM_UPDATESCROLLBAR, 0, 0);
end;

procedure TPopupExplorerTree.DoEnumFolder(const Namespace: TNamespace;
  var AllowAsChild: Boolean);
begin
  inherited;
  if AllowAsChild then
  begin
    if Assigned(PopupExplorerDropDown) and Assigned(PopupExplorerDropDown.TargetPIDL) then
      AllowAsChild := (PIDLMgr.IDCount(Namespace.AbsolutePIDL) < 3) or
        PIDLMgr.IsSubPIDL(PopupExplorerDropDown.TargetPIDL, Namespace.AbsolutePIDL) or
        (PopupExplorerDropDown.ComboBoxStyle = cbsVETEnhanced)
    else
      AllowAsChild := (PIDLMgr.IDCount(Namespace.AbsolutePIDL) < 3) or
        (PopupExplorerDropDown.ComboBoxStyle = cbsVETEnhanced)
  end
end;

procedure TPopupExplorerTree.DoExpanded(Node: PVirtualNode);
begin
  inherited;
  // We can't just update it here as the tree has not quite finished updating its
  // new Rect size yet and we calcualate wrong
  PostMessage(PopupExplorerDropDown.Handle, WM_UPDATESCROLLBAR, 0, 0);
end;

procedure TPopupExplorerTree.LoadDefaultOptions;
begin
  TreeOptions.AutoOptions := DefaultPopupAutoOptions;
  TreeOptions.MiscOptions := DefaultPopupMiscOptions;
  TreeOptions.PaintOptions := DefaultPopupPaintOptions;
  TreeOptions.SelectionOptions := DefaultPopupSelectionOptions;
  TreeOptions.VETFolderOptions := DefaultExplorerComboVETFolderOptions;
  TreeOptions.VETImageOptions := DefaultExplorerComboVETImageOptions;
  TreeOptions.VETShellOptions := DefaultExplorerComboVETShellOptions;
  TreeOptions.VETMiscOptions := DefaultExplorerComboVETMiscOptions;
  TreeOptions.VETSyncOptions := DefaultExplorerComboVETSyncOptions;
end;

procedure TPopupExplorerTree.MouseMove(Shift: TShiftState; X, Y: Integer);

// Handles the automatic scrolling.  Note that the TDropDownWnd class has all the
// Timer logic and methods handled. All that is needed is to Create and Destroy then
// timers.  The scroll messages are sent to the window defined in the AutoScrollWindow
// property of the TDropDownWnd class as well
// Also no check is needed for mouse down since the only way the coordinates could
// be realized is if the mouse is captured anyway. Also note it is up to us to set
// the Scrolling Flag since only we know when we are scrolling

var
  Node: PVirtualNode;
begin
  inherited;
  { Deal with the autoscroll timer }
  if Y < 0 then
  begin
    Include(PopupExplorerDropDown.FPopupStates, psScrollingUp);
    Exclude(PopupExplorerDropDown.FPopupStates, psScrollingDown);
    if (Y < -20) then
    begin
       if not (psFastScroll in PopupExplorerDropDown.PopupStates) then
      begin
        PopupExplorerDropDown.ScrollTimerDestroy(False);
        PopupExplorerDropDown.ScrollTimerCreate(True);
      end
    end else
    begin
       if not (psSlowScroll in PopupExplorerDropDown.PopupStates) then
      begin
        PopupExplorerDropDown.ScrollTimerDestroy(False);
        PopupExplorerDropDown.ScrollTimerCreate(False);
      end;
    end
  end else
  if Y > ClientHeight then
  begin
     Include(PopupExplorerDropDown.FPopupStates, psScrollingDown);
     Exclude(PopupExplorerDropDown.FPopupStates, psScrollingUp);
    if (Y > ClientHeight + 20) then
    begin
      if not (psFastScroll in PopupExplorerDropDown.PopupStates) then
      begin
        PopupExplorerDropDown.ScrollTimerDestroy(False);
        PopupExplorerDropDown.ScrollTimerCreate(True);
      end
    end else
    begin
      if not (psSlowScroll in PopupExplorerDropDown.PopupStates) then
      begin
        PopupExplorerDropDown.ScrollTimerDestroy(False);
        PopupExplorerDropDown.ScrollTimerCreate(False);
      end
    end
  end else
    PopupExplorerDropDown.ScrollTimerDestroy(True);

  if not PopupExplorerDropDown.Scrolling then
  begin
    { Deal with the hot track selection of the node }
    Node := GetNodeAt(X, Y);
    { Windows sends a mouse move message even if we are using the keyboard to     }
    { change the focus.  The two collide if we don't detect and account for it.   }
    if (Node <> GetFirstSelected) and
      ((PopupExplorerDropDown.FLastMousePos.X <> X) or (PopupExplorerDropDown.FLastMousePos.Y <> Y)) then
    begin
      BeginUpdate;
      try
        ClearSelection;
        Selected[Node] := True;
        FocusedNode := Node
      finally
        EndUpdate
      end;
    end;
  end;
  PopupExplorerDropDown.FLastMousePos.x := X;
  PopupExplorerDropDown.FLastMousePos.y := Y;
end;

procedure TPopupExplorerTree.Paint;
var
  Grabber: TSizeGrabber;
  GrabRegion, ClientRegion, WindowRegion: HRgn;
  R: TRect;
begin
  ClientRegion := 0;
  GrabRegion := 0;
  WindowRegion := 0;

  if Assigned(PopupExplorerDropDown) then
  begin
    Grabber := PopupExplorerDropDown.Grabber;
    if Grabber.Transparent then
    begin
      try
       // Clip out the grabber area to eliminate flickering, make sure the base window
       // does not have WS_CLIPCHILDREN windows style set!!!
       // If it is set then Windows will automaticlly clear any children regions so
       // we can't paint in the area regardless of the clipping area we choose
        R := ClientRect;
        ClientRegion := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
        WindowRegion := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);

        GrabRegion := PopupExplorerDropDown.GrabberPolyRgn(Grabber, Self);

        CombineRgn(WindowRegion, ClientRegion, GrabRegion, RGN_XOR);
        SelectClipRgn(Canvas.Handle, WindowRegion);
       // Paint the window with the grabber area clipped
        inherited;

        // Now clip everything but the grabber area
        CombineRgn(WindowRegion, ClientRegion, GrabRegion, RGN_AND);
        SelectClipRgn(Canvas.Handle, WindowRegion);

        // Paint the grabber double buffered to eliminate flicker
        PopupExplorerDropDown.BitBltGrabber(Canvas, Self, Color);

      finally
        if WindowRegion <> 0 then
          DeleteObject(WindowRegion);
        if GrabRegion <> 0 then
          DeleteObject(GrabRegion);
        if ClientRegion <> 0 then
          DeleteObject(ClientRegion);
      end;
    end else
      inherited;
  end else
    inherited
end;

procedure TPopupExplorerTree.WMLButtonDown(var Message: TWMLButtonDown);

var
  HitInfo: THitInfo;

begin
  // Make the VETInhanced mode work
  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);
  if hiOnItemButton in HitInfo.HitPositions then
    ToggleNode(HitInfo.HitNode);
  Mouse.Capture := Handle;
  // Don't call inhertied or VT will force the focus to the popup and make the VCL
  // get a little weird.  It is better just to never let any Popup to get the focus
  // Note also that WM_MOUSEACTIVATE returns MA_NOACTIVATE as well
end;

procedure TPopupExplorerTree.WMLButtonUp(var Message: TWMLButtonUp);
var
  HitInfo: THitInfo;
begin
  inherited;
  if PopupExplorerDropDown.Scrolling then
    PopupExplorerDropDown.ScrollTimerDestroy(True);

  if PopupExplorerDropDown.ComboBoxStyle = cbsVETEnhanced then
  begin
    GetHitTestInfoAt(Message.xPos, Message.yPos, True, HitInfo);
    if Assigned(HitInfo.HitNode) then
      if (HitInfo.HitPositions = [hiOnItem]) or (hiOnItemButton in HitInfo.HitPositions)  then
        Exit
  end;
  if Assigned(GetNodeAt(Message.xPos, Message.yPos)) then
    PopupExplorerDropDown.RollUp(True); // We are committed; make the selection
end;

procedure TPopupExplorerTree.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TPopupExplorerTree.WMRButtonDown(var Message: TWMRButtonDown);
begin
  // Don't call inhertied or VT will force the focus to the popup and make the VCL
  // get a little weird.  It is better just to never let any Popup to get the focus
  // Note also that WM_MOUSEACTIVATE returns MA_NOACTIVATE as well
end;

{ TPopupAutoCompleteTree }

constructor TPopupAutoCompleteTree.Create(AOwner: TComponent);
begin
  inherited;
  DefaultNodeHeight := 17;
  Indent := 0;
  Colors.DisabledColor := Colors.FocusedSelectionColor; // Make the disabled focus look like it is enabled
  Colors.DisabledColor := Colors.UnfocusedSelectionColor;
  NodeDataSize := 4;
  AutoComplete := TVirtualShellAutoComplete.Create(Self);
  {$IFDEF TNTSUPPORT}
  Strings := TTntStringList.Create;
  {$ELSE}
  Strings := TStringList.Create;
  {$ENDIF}
end;

procedure TPopupAutoCompleteTree.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Don't clip the Grabber sibling when it is to be transparent
  Params.Style := Params.Style and not WS_CLIPSIBLINGS
end;

destructor TPopupAutoCompleteTree.Destroy;
begin
  AutoComplete.Free;
  Strings.Free;
  inherited;
end;

procedure TPopupAutoCompleteTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
var
  P: integer;
begin
  inherited;
  P := integer( GetNodeData(Node)^);
  Text := Strings[P];
end;

{$IFDEF TNTSUPPORT}
procedure TPopupAutoCompleteTree.DoUpdateList(const CurrentEditContents:
  WideString; EnumList: TTntStringList; var Handled: Boolean);
begin
  if Assigned(PopupAutoCompleteDropDown.OnAutoCompleteUpdateList) then
    PopupAutoCompleteDropDown.OnAutoCompleteUpdateList(
      PopupAutoCompleteDropDown.ExplorerCombobox, CurrentEditContents, EnumList, Handled);
end;
{$ELSE}
procedure TPopupAutoCompleteTree.DoUpdateList(const CurrentEditContents:
  WideString; EnumList: TStringList; var Handled: Boolean);
begin
  if Assigned(PopupAutoCompleteDropDown.OnAutoCompleteUpdateList) then
    PopupAutoCompleteDropDown.OnAutoCompleteUpdateList(
      PopupAutoCompleteDropDown.ExplorerCombobox, CurrentEditContents, EnumList, Handled);
end;
{$ENDIF}

procedure TPopupAutoCompleteTree.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  inherited;
  // Auto scroll the Win2k way, by wiggling the mouse near the top and bottom of
  // the autocomplete window
  if Y < 20 then
  begin
    if ( Abs(PopupAutoCompleteDropDown.FLastMousePos.x - X) > 4) or
       ( Abs(PopupAutoCompleteDropDown.FLastMousePos.y - Y) > 4) then
      SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
  end else
  if Y > Height - 20 then
  begin
    if ( Abs(PopupAutoCompleteDropDown.FLastMousePos.x - X) > 4) or
       ( Abs(PopupAutoCompleteDropDown.FLastMousePos.y - Y) > 4) then
    SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
  end;

  begin
    { Deal with the hot track selection of the node }
    Node := GetNodeAt(X, Y);
    { Windows sends a mouse move message even if we are using the keyboard to     }
    { change the focus.  The two collide if we don't detect and account for it.   }
    if (Node <> GetFirstSelected) and ((PopupAutoCompleteDropDown.FLastMousePos.X <> X) or
      (PopupAutoCompleteDropDown.FLastMousePos.Y <> Y)) then
    begin
      BeginUpdate;
      try
        ClearSelection;
        Selected[Node] := True;
        FocusedNode := nil
      finally
        EndUpdate
      end;
    end;
  end;
  PopupAutoCompleteDropDown.FLastMousePos.x := X;
  PopupAutoCompleteDropDown.FLastMousePos.y := Y;
end;

procedure TPopupAutoCompleteTree.Paint;
var
  Grabber: TSizeGrabber;
  GrabRegion, ClientRegion, WindowRegion: HRgn;
  R: TRect;
  BackBits: TBitmap;
begin
  ClientRegion := 0;
  GrabRegion := 0;
  WindowRegion := 0;

  if Assigned(PopupAutoCompleteDropDown) then
  begin
    Grabber := PopupAutoCompleteDropDown.Grabber;
    if Grabber.Transparent then
    begin
      BackBits := TBitmap.Create;
      try
       // Clip out the grabber area to eliminate clipping, make sure the base window
       // does not have WS_CLIPCHILDREN windows style set!!!
       // If it is set then Windows will automaticlly clear any children regions so
       // we can't paint the transparent grabber.
        R := ClientRect;
        ClientRegion := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
        WindowRegion := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);

        GrabRegion := PopupAutoCompleteDropDown.GrabberPolyRgn(Grabber, Self);

        CombineRgn(WindowRegion, ClientRegion, GrabRegion, RGN_XOR);
        SelectClipRgn(Canvas.Handle, WindowRegion);
       // Paint the window with the grabber area clipped
        inherited;

        // Paint the grabber double buffered to eliminate flicker
        Backbits.Width := Grabber.Width;
        Backbits.Height := Grabber.Height;
        Backbits.Canvas.Brush.Color := Canvas.Brush.Color;
        SetRect(R, 0, 0, Grabber.Width, Grabber.Height);
        Backbits.Canvas.FillRect(Grabber.BoundsRect);
        Grabber.PaintGrabber(Backbits.Canvas.Handle);

        CombineRgn(WindowRegion, ClientRegion, GrabRegion, RGN_AND);
        SelectClipRgn(Canvas.Handle, WindowRegion);

        PopupAutoCompleteDropDown.BitBltGrabber(Canvas, Self, Color);
      finally
        if WindowRegion <> 0 then
          DeleteObject(WindowRegion);
        if GrabRegion <> 0 then
          DeleteObject(GrabRegion);
        if ClientRegion <> 0 then
          DeleteObject(ClientRegion);
        BackBits.Free;
      end;
    end else
      inherited;
  end else
    inherited
end;

procedure TPopupAutoCompleteTree.UpdateList(CurrentEditStr: WideString);
var
  Handled: Boolean;
  i: integer;
  TestString: WideString;
begin
  Handled := False;
  Clear;
  Strings.Clear;
  DoUpdateList(CurrentEditStr, Strings, Handled);
  if Handled then
  begin
    for i := 0 to Strings.Count - 1 do
      AddChild(nil, Pointer(i));  // Store the index in the list for the GetText event
  end else
  begin
    { Default Handling of ListUpdate here }
    BeginUpdate;
    try
      // Strip what is being typed back to the parent folder
      if Length(CurrentEditStr) = 1 then
        TestString := CurrentEditStr + ':\'
      else
        TestString := WideExtractFileDir(CurrentEditStr);

      if TestString <> '' then
      begin
        TestString := IncludeTrailingBackslashW(TestString);
        AutoComplete.CurrentDir := TestString;
        AutoComplete.Refresh;
        Strings.Assign(AutoComplete.Strings);
        CurrentEditStr := WideLowerCase(CurrentEditStr);
       { Add the filtered nodes to the VT }
        for i := 0 to Strings.Count - 1 do
        begin
           TestString := Strings[i];
           TestString := WideLowerCase(TestString);
          { See if the CurrentEditStr is a substring of the test string starting from position 1 }
          if (WideStrPos(PWideChar( TestString), PWideChar( CurrentEditStr)) = @TestString[1]) or (CurrentEditStr[Length(CurrentEditStr)] = '\') then
            AddChild(nil, Pointer(i));  // Store the index in the list for the GetText event
        end
      end;
    finally
      EndUpdate
     end
  end;
  PopupAutoCompleteDropDown.RefreshScrollbar
end;

procedure TPopupAutoCompleteTree.WMLButtonDown(var Message: TWMLButtonDown);
begin
  // Eat this message so VT won't force its focus will on us
  // But capture it so we can autoscroll
end;

procedure TPopupAutoCompleteTree.WMLButtonUp(var Message: TWMLButtonUp);
begin
   if PopupAutoCompleteDropDown.Scrolling then
    PopupAutoCompleteDropDown.ScrollTimerDestroy(True);
  // We are committed; make the selection if hit a node
  if Assigned(GetNodeAt(Message.xPos, Message.yPos)) then
    PopupAutoCompleteDropDown.RollUp(True);
  inherited;
end;

procedure TPopupAutoCompleteTree.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ TComboDropOptions }

function TPopupExplorerOptions.GetAnimationSpeed: integer;
begin
  Result := PopupExplorerDropDown.AnimationSpeed
end;

function TPopupExplorerOptions.GetAutoScrollTimeFast: integer;
begin
   Result := PopupExplorerDropDown.AutoScrollFastTime
end;

function TPopupExplorerOptions.GetAutoScrollTimeSlow: integer;
begin
   Result := PopupExplorerDropDown.AutoScrollSlowTime
end;

function TPopupExplorerOptions.GetBackground: TPicture;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.Background
end;

function TPopupExplorerOptions.GetBackgroundOffsetX: integer;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.BackgroundOffsetX
end;

function TPopupExplorerOptions.GetBackgroundOffsetY: integer;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.BackgroundOffsetY
end;

function TPopupExplorerOptions.GetColor: TColor;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.Color;
end;

function TPopupExplorerOptions.GetComboBoxStyle: TComboBoxStyle;
begin
  Result := PopupExplorerDropDown.ComboBoxStyle
end;

function TPopupExplorerOptions.GetDefaultNodeHeight: Cardinal;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.DefaultNodeHeight
end;

function TPopupExplorerOptions.GetDropDownCount: integer;
begin
  Result := PopupExplorerDropDown.DropDownCount
end;

function TPopupExplorerOptions.GetIndent: integer;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.Indent
end;

function TPopupExplorerOptions.GetOnEnumFolder: TVETOnEnumFolder;
begin
  Result := PopupExplorerDropDown.PopupExplorerTree.OnEnumFolder
end;

function TPopupExplorerOptions.GetOnRollDown: TOnPopupRollDown;
begin
  Result := PopupExplorerDropDown.OnRollDown
end;

function TPopupExplorerOptions.GetOnRollUp: TOnPopupRollUp;
begin
  Result := PopupExplorerDropDown.OnRollUp
end;

function TPopupExplorerOptions.GetOptions: TPopupOptions;
begin
  Result := PopupExplorerDropDown.PopupOptions
end;

procedure TPopupExplorerOptions.SetAnimationSpeed(const Value: integer);
begin
  PopupExplorerDropDown.AnimationSpeed := Value
end;

procedure TPopupExplorerOptions.SetAutoScrollTimeFast(const Value: integer);
begin
  PopupExplorerDropDown.AutoScrollFastTime := Value
end;

procedure TPopupExplorerOptions.SetAutoScrollTimeSlow(const Value: integer);
begin
  PopupExplorerDropDown.AutoScrollSlowTime := Value
end;

procedure TPopupExplorerOptions.SetBackground(const Value: TPicture);
begin
  PopupExplorerDropDown.PopupExplorerTree.Background := Value;
end;

procedure TPopupExplorerOptions.SetBackgroundOffsetX(const Value: integer);
begin
  PopupExplorerDropDown.PopupExplorerTree.BackgroundOffsetX := Value
end;

procedure TPopupExplorerOptions.SetBackgroundOffsetY(const Value: integer);
begin
  PopupExplorerDropDown.PopupExplorerTree.BackgroundOffsetY := Value
end;

procedure TPopupExplorerOptions.SetColor(const Value: TColor);
begin
  PopupExplorerDropDown.PopupExplorerTree.Color := Value
end;

procedure TPopupExplorerOptions.SetComboBoxStyle(const Value: TComboBoxStyle);
begin
  PopupExplorerDropDown.ComboBoxStyle := Value
end;

procedure TPopupExplorerOptions.SetDefaultNodeHeight(const Value: Cardinal);
begin
  PopupExplorerDropDown.PopupExplorerTree.DefaultNodeHeight := Value
end;

procedure TPopupExplorerOptions.SetDropDownCount(const Value: integer);
begin
  PopupExplorerDropDown.DropDownCount := Value
end;

procedure TPopupExplorerOptions.SetIndent(const Value: integer);
begin
  PopupExplorerDropDown.PopupExplorerTree.Indent := Value
end;

procedure TPopupExplorerOptions.SetOnEnumFolder(
  const Value: TVETOnEnumFolder);
begin
  PopupExplorerDropDown.PopupExplorerTree.OnEnumFolder := Value
end;

procedure TPopupExplorerOptions.SetOnRollDown(const Value: TOnPopupRollDown);
begin
  PopupExplorerDropDown.OnRollDown := Value
end;

procedure TPopupExplorerOptions.SetOnRollUp(const Value: TOnPopupRollUp);
begin
  PopupExplorerDropDown.OnRollUp := Value
end;

procedure TPopupExplorerOptions.SetOptions(const Value: TPopupOptions);
begin
  PopupExplorerDropDown.PopupOptions := Value
end;

{ TAutoCompleteOptions }

procedure TPopupAutoCompleteOptions.AlwaysShowReader(Reader: TReader);
begin
  // Depreciated Property
  Reader.ReadBoolean;
end;

procedure TPopupAutoCompleteOptions.DefineProperties(Filer: TFiler);
begin
  // Depreciated Property
  inherited;
  Filer.DefineProperty('AlwaysShow', AlwaysShowReader, nil, False);
end;

function TPopupAutoCompleteOptions.GetAnimationSpeed: integer;
begin
  Result := PopupAutoCompleteDropDown.AnimationSpeed
end;

function TPopupAutoCompleteOptions.GetAutoScrollTimeFast: integer;
begin
  Result := PopupAutoCompleteDropDown.AutoScrollFastTime
end;

function TPopupAutoCompleteOptions.GetAutoScrollTimeSlow: integer;
begin
  Result := PopupAutoCompleteDropDown.AutoScrollSlowTime
end;

function TPopupAutoCompleteOptions.GetBackground: TPicture;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.Background
end;

function TPopupAutoCompleteOptions.GetBackgroundOffsetX: integer;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.BackgroundOffsetX
end;

function TPopupAutoCompleteOptions.GetBackgroundOffsetY: integer;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.BackgroundOffsetY
end;

function TPopupAutoCompleteOptions.GetColor: TColor;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.Color
end;

function TPopupAutoCompleteOptions.GetContents: TAutoCompleteContents;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.AutoComplete.Contents
end;

function TPopupAutoCompleteOptions.GetDefaultNodeHeight: Cardinal;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.DefaultNodeHeight
end;

function TPopupAutoCompleteOptions.GetDropDownCount: integer;
begin
  Result := PopupAutoCompleteDropDown.DropDownCount
end;

function TPopupAutoCompleteOptions.GetIndent: integer;
begin
  Result := PopupAutoCompleteDropDown.PopupAutoCompleteTree.Indent
end;

function TPopupAutoCompleteOptions.GetOnRollDown: TOnPopupRollDown;
begin
  Result := PopupAutoCompleteDropDown.OnRollDown
end;

function TPopupAutoCompleteOptions.GetOnRollUp: TOnPopupRollUp;
begin
  Result := PopupAutoCompleteDropDown.OnRollUp
end;

function TPopupAutoCompleteOptions.GetOnAutoCompleteUpdateList: TOnAutoCompleteUpdateList;
begin
  Result := PopupAutoCompleteDropDown.OnAutoCompleteUpdateList
end;

function TPopupAutoCompleteOptions.GetOptions: TPopupOptions;
begin
  Result := PopupAutoCompleteDropDown.PopupOptions
end;

procedure TPopupAutoCompleteOptions.SetAnimationSpeed(const Value: integer);
begin
  PopupAutoCompleteDropDown.AnimationSpeed := Value
end;

procedure TPopupAutoCompleteOptions.SetAutoScrollTimeFast(const Value: integer);
begin
  PopupAutoCompleteDropDown.AutoScrollFastTime := Value
end;

procedure TPopupAutoCompleteOptions.SetAutoScrollTimeSlow(const Value: integer);
begin
  PopupAutoCompleteDropDown.AutoScrollSlowTime := Value
end;

procedure TPopupAutoCompleteOptions.SetBackground(const Value: TPicture);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.Background := Value;
end;

procedure TPopupAutoCompleteOptions.SetBackgroundOffsetX(const Value: integer);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.BackgroundOffsetX := Value
end;

procedure TPopupAutoCompleteOptions.SetBackgroundOffsetY(const Value: integer);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.BackgroundOffsetY := Value
end;

procedure TPopupAutoCompleteOptions.SetColor(const Value: TColor);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.Color := Value
end;

procedure TPopupAutoCompleteOptions.SetContents(const Value: TAutoCompleteContents);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.AutoComplete.Contents := Value
end;

procedure TPopupAutoCompleteOptions.SetDefaultNodeHeight(const Value: Cardinal);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.DefaultNodeHeight := Value
end;

procedure TPopupAutoCompleteOptions.SetDropDownCount(const Value: integer);
begin
  PopupAutoCompleteDropDown.DropDownCount := Value
end;

procedure TPopupAutoCompleteOptions.SetIndent(const Value: integer);
begin
  PopupAutoCompleteDropDown.PopupAutoCompleteTree.Indent := Value
end;

procedure TPopupAutoCompleteOptions.SetOnRollDown(const Value: TOnPopupRollDown);
begin
  PopupAutoCompleteDropDown.OnRollDown := Value
end;

procedure TPopupAutoCompleteOptions.SetOnRollUp(const Value: TOnPopupRollUp);
begin
  PopupAutoCompleteDropDown.OnRollUp := Value
end;

procedure TPopupAutoCompleteOptions.SetOnAutoCompleteUpdateList(const Value: TOnAutoCompleteUpdateList);
begin
  PopupAutoCompleteDropDown.OnAutoCompleteUpdateList := Value
end;

procedure TPopupAutoCompleteOptions.SetOptions(const Value: TPopupOptions);
begin
  PopupAutoCompleteDropDown.PopupOptions := Value
end;


{ TAutoCompleteDropDown }

function TPopupAutoCompleteDropDown.AllowClickInWindow(Window: HWnd;
  Point: TPoint): Boolean;

//  Allow a click in the AutoCompleteCombo or the ComboEdit with out closing the popup

begin
  if ExplorerCombobox.HandleAllocated then
    Result := (Window = ExplorerCombobox.Handle) or (Window = ExplorerCombobox.ComboEdit.Handle)
  else
    Result := False
end;

constructor TPopupAutoCompleteDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FPopupAutoCompleteTree := CreatePopupAutoCompleteTree;
  PopupAutoCompleteTree.PopupAutoCompleteDropDown := Self;
  AutoScrollWindow := PopupAutoCompleteTree;
  PopupAutoCompleteTree.Parent := Self;
  PopupAutoCompleteTree.ScrollbarOptions.ScrollBars := ssNone;
  PopupAutoCompleteTree.BevelEdges := [];
  PopupAutoCompleteTree.BevelInner := bvNone;
  PopupAutoCompleteTree.BevelOuter := bvNone;
  PopupAutoCompleteTree.BorderStyle := bsNone;
  PopupAutoCompleteTree.DefaultNodeHeight := 17;
  PopupAutoCompleteTree.TreeOptions.AutoOptions := DefaultPopupAutoOptions;
  PopupAutoCompleteTree.TreeOptions.MiscOptions := DefaultPopupMiscOptions;
  PopupAutoCompleteTree.TreeOptions.PaintOptions := DefaultPopupPaintOptions;
  PopupAutoCompleteTree.TreeOptions.SelectionOptions := DefaultPopupSelectionOptions;
  WheelMouseTarget := PopupAutoCompleteTree;
  DropDownCount := 8;
end;

function TPopupAutoCompleteDropDown.CreatePopupAutoCompleteTree: TPopupAutoCompleteTree;

// Overridable so a decendant of TPopupAutoCompleteTree may be created and used

begin
  Result := TPopupAutoCompleteTree.Create(Self);
end;

destructor TPopupAutoCompleteDropDown.Destroy;
begin
  inherited;
end;

procedure TPopupAutoCompleteDropDown.DoRollDown(var Allow: Boolean);
begin
  inherited DoRollDown(Allow);
end;

procedure TPopupAutoCompleteDropDown.DoRollDownInit;
begin
  inherited;
  // Keyboard interface works better with a focused node (we are suppressing the painting of the focus rect though)
  PopupAutoCompleteTree.FocusedNode := PopupAutoCompleteTree.GetFirstSelected;
  RefreshScrollbar;
end;

procedure TPopupAutoCompleteDropDown.DoRollUp(Selected: Boolean);
var
  Node: PVirtualNode;
begin
  inherited;
  if Selected then
  begin
    Node := PopupAutoCompleteTree.GetFirstSelected;
    // If a node is selected place the text in the Wide, if not use what is already there
    if Assigned(Node) then
      ExplorerCombobox.ComboEdit.Text := PopupAutoCompleteTree.Text[Node, -1];
    ExplorerCombobox.ComboEdit.SelectEnteredPath;
  end;
  PopupAutoCompleteTree.Clear
end;

function TPopupAutoCompleteDropDown.GetPopupOptions: TPopupOptions;
begin
  Result := inherited PopupOptions
end;

procedure TPopupAutoCompleteDropDown.KeyPressDispatch(var Message: TMessage;
  var Handled: Boolean);
var
  Node: PVirtualNode;
begin
  with TWMKey(Message) do
  begin
    // Explorer seems to map Tab to Arrow Down
    if CharCode = VK_TAB then
      CharCode := VK_DOWN;
    if (CharCode in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) then
    begin
      Node := PopupAutoCompleteTree.GetFirstSelected;
      if not Assigned(Node) then
      begin
        Node := PopupAutoCompleteTree.GetFirst;
        PopupAutoCompleteTree.Selected[Node] := True;
        PopupAutoCompleteTree.FocusedNode := Node;
      end else
      begin
        // Let VT handle the scrolling
        PopupAutoCompleteTree.Perform(Message.Msg, Message.wParam, Message.lParam);
        Node := PopupAutoCompleteTree.GetFirstSelected;
        PopupAutoCompleteTree.ScrollIntoView(Node, False);
        RefreshScrollbar;
       end;
       TWMKey(Message).CharCode := 0;
       if Assigned(Node) then
       begin
         ExplorerCombobox.ComboEdit.Text := PopupAutoCompleteTree.Text[Node, -1];
         // Move cursor to end
         ExplorerCombobox.ComboEdit.SelStart := Length(ExplorerCombobox.ComboEdit.Text);
       end;
       Handled := True
    end
  end
end;

procedure TPopupAutoCompleteDropDown.RealignChildWindows(NewWidth, NewHeight: integer);

// Reposition any new child windows we added in the decentant, namely the VET here

var
  X, Y, W, H: integer;
begin
  inherited;
  Y := 0;

  if RemoteScrollbar.Visible then
    W := NewWidth - RemoteScrollbar.Width - BORDER * 2
  else begin
    if GetTopWindow(Handle) <> Grabber.Handle then
       BringWindowToTop(Grabber.Handle);
    W := NewWidth - BORDER * 2;
  end;

  H := NewHeight - BORDER * 2;
  if (psLeftScrollbar in PopupStates) and RemoteScrollbar.Visible then
    X := RemoteScrollbar.Width
  else
    X := 0;
  PopupAutoCompleteTree.SetBounds(X, Y, W, H);
end;

procedure TPopupAutoCompleteDropDown.RefreshScrollbar;

// Keep VET in sync with the Scrollbar component

var
  R: TRect;
begin
  inherited;
  if PopupAutoCompleteTree.Height > 0 then
  begin
    R := PopupAutoCompleteTree.GetTreeRect;
    RemoteScrollbar.Min := 0;
    RemoteScrollbar.Max := R.Bottom;
    RemoteScrollbar.PageSize := PopupAutoCompleteTree.ClientHeight;
    RemoteScrollbar.SmallChange := PopupAutoCompleteTree.ScrollBarOptions.VerticalIncrement;
    RemoteScrollbar.LargeChange := PopupAutoCompleteTree.ClientHeight;
    RemoteScrollbar.Position := Abs(PopupAutoCompleteTree.OffsetY);
    if R.Bottom - PopupAutoCompleteTree.ClientHeight = 0 then
    begin
      RemoteScrollbar.Visible := False;
      Grabber.Transparent := True
    end else
    begin
       RemoteScrollbar.Visible := True;
       Grabber.Transparent := False
    end
  end;
end;

function TPopupAutoCompleteDropDown.RowHeight: Cardinal;
begin
   Result := PopupAutoCompleteTree.DefaultNodeHeight;
end;

procedure TPopupAutoCompleteDropDown.SetPopupOptions(const Value: TPopupOptions);
begin
  inherited PopupOptions := Value;
  with PopupAutoCompleteTree.TreeOptions do
    if poThemeAware in Value then
    begin
      PaintOptions := PaintOptions + [toThemeAware];
      RemoteScrollbar.Options := RemoteScrollbar.Options + [soThemeAware]
    end else
    begin
      PaintOptions := PaintOptions - [toThemeAware];
      RemoteScrollbar.Options := RemoteScrollbar.Options - [soThemeAware]
    end
end;

procedure TPopupAutoCompleteDropDown.WMVScroll(var message: TWMVScroll);
begin
  inherited;
  if (Message.ScrollCode = SB_THUMBTRACK) or (Message.ScrollCode = SB_THUMBPOSITION) then
  begin
    PopupAutoCompleteTree.OffsetY := -Message.Pos
  end else
    PopupAutoCompleteTree.Perform(WM_VSCROLL, TMessage(Message).wParam, TMessage(Message).lParam);
  RefreshScrollbar
end;


procedure TPopupAutoCompleteTree.WMRButtonDown(var Message: TWMRButtonDown);
begin
  // Don't call inhertied or VT will force the focus to the popup and make the VCL
  // get a little weird.  It is better just to never let any Popup to get the focus
  // Note also that WM_MOUSEACTIVATE returns MA_NOACTIVATE as well
end;


{ TVirtualBackGndEnumThread}
constructor TVirtualBackGndEnumThread.Create(Suspended: Boolean; ForceHideRecycleBin: Boolean;
  AParentPIDL: PItemIDList; AFileObjects: TFileObjects; AWndHandle: THandle;
  AnEnumLock: TRTLCriticalSection; APIDLList: PCommonPIDLList; AnEnumThread: PVirtualBackGndEnumThread);
begin
  inherited Create(Suspended);
  // Given to the Namespace from within the thread context
  ParentPIDL := PIDLMgr.CopyPIDL(AParentPIDL);
  FileObject := AFileObjects;
  ValidateFileObjects(FFileObjects);
  List := TCommonPIDLList.Create;
  HideRecycleBin := ForceHideRecycleBin;
  WndHandle := AWndHandle;
  EnumLock := AnEnumLock;
  EnumBkGndList := APIDLList;
  EnumThread := AnEnumThread;
  Priority := tpLower;
  BkGndEnumThreadList.Add(Self)
end;

destructor TVirtualBackGndEnumThread.Destroy;
begin
  BkGndEnumThreadList.Remove(Self);
  PIDLMgr.FreePIDL(FParentPIDL);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TVirtualBackGndEnumThread.Execute;
var
  Flags, Fetched: Cardinal;
  APIDL: PItemIDList;
  Desktop, TargetFolder: IShellFolder;
  EnumIDList: IEnumIDList;
  OldWow64: Pointer;
begin
  OldWow64 := Wow64RedirectDisable;
  try
    Flags := FileObjectsToFlags(FileObject);
    if SHGetDesktopFolder(Desktop) = S_OK then
    begin
      if ParentPIDL.mkID.cb = 0 then
        TargetFolder := Desktop
      else
        Desktop.BindToObject(ParentPIDL, nil, IShellFolder, Pointer(TargetFolder));
      if Assigned(TargetFolder) then
      begin
        if TargetFolder.EnumObjects(WndHandle, Flags, EnumIDList) = S_OK then
        begin
          while not Terminated and (EnumIDList.Next(1, APIDL, Fetched) = NOERROR)  do
          begin
            {$IFDEF DELAY_THREADED_ENUM}
            Sleep(100);
            {$ENDIF}
            List.Add(APIDL)
          end;
        end
      end
    end;
  finally
    if not Terminated then
    begin
   //   May be more trouble than it is worth.  Need to deal with groups, sort direction, columns, folders first, etc, etc....
  //    PIDLListQuickSort(List, TargetFolder, 0, List.Count - 1, SortColumn);

      EnterCriticalSection(FEnumLock);
      // First see if we are the thread that the control is waiting for
      if EnumThread^ = Self then
      begin
        // If so then we give the control the list else the list will be destroyed
        // in the destructor
        EnumThread^.Terminate;
        EnumThread^ := nil;
        EnumBkGndList^ := List;
        List := nil;
      end;
      LeaveCriticalSection(FEnumLock);
    end;
    Wow64RedirectRevert(OldWow64)
  end;
end;

procedure TVirtualBackGndEnumThread.FinalizeThread;
begin
  inherited FinalizeThread;
  CoUninitialize;
end;

procedure TVirtualBackGndEnumThread.InitializeThread;
begin
  inherited InitializeThread;
  CoInitialize(nil);
end;

{ TExpandMarkThreadRequest }
function TExpandMarkThreadRequest.HandleRequest: Boolean;
var
  NS: TNamespace;
begin
  // If we fail we assume something is wrong and don't keep trying in the thread
  Result := True;
  FShowMark := True;
  NS := TNamespace.Create(PIDL, nil);
  try
    NS.FreePIDLOnDestroy := False;
      // Tag contains the SHCONTF_XXXX flags
    FShowMark := NS.SubItemsEx(EnumFlags)
  finally
    NS.Free
  end
end;

procedure TExpandMarkThreadRequest.Assign(Source: TPersistent);
var
  S: TExpandMarkThreadRequest;
begin
  inherited Assign(Source);
  if Source is TExpandMarkThreadRequest then
  begin
    S := TExpandMarkThreadRequest( Source);
    EnumFlags := S.EnumFlags
  end
end;

{ TVirtualBkGndEnumThreadList }
constructor TVirtualBkGndEnumThreadList.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
end;

destructor TVirtualBkGndEnumThreadList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TVirtualBkGndEnumThreadList.Add(Thread: TCommonThread);
var
  L: TList;
begin
  L := List.LockList;
  try
    L.Add(Thread);
  finally
    List.UnLockList
  end
end;

procedure TVirtualBkGndEnumThreadList.Flush;
var
  L: TList;
  Done: Boolean;
  i, Count: Integer;
begin
  Done := False;
  Count := 0;
  while not Done do
  begin
    L := List.LockList;
    try
      // if over 2 minutes then get tough on these threads.
      if Count > Int(120/50e-3) then
      begin
        for i := 0 to L.Count - 1 do
        begin
          TerminateThread( TCommonThread( L[i]).ThreadID, 0);
          Remove( TCommonThread( L[i]))
        end
      end;
      Done := L.Count = 0
    finally
      List.UnLockList
    end;
    if not Done then
      Sleep(50);
    Inc(Count)
  end;
end;

procedure TVirtualBkGndEnumThreadList.Remove(Thread: TCommonThread);
var
  L: TList;
begin
  L := List.LockList;
  try
    L.Remove(Thread)
  finally
    List.UnLockList
  end
end;

initialization
  VETChangeDispatch := TVETChangeDispatch.Create;
  ViewManager := TGlobalViewManager.Create;
  BkGndEnumThreadList := TVirtualBkGndEnumThreadList.Create;
  InitThemeLibrary;
  LoadWideFunctions;
  LoadShell32Functions;

finalization
  FreeAndNil(ViewManager);
  FreeThemeLibrary;
  FreeAndNil(VETChangeDispatch);
  // Wait until all enum threads are finished.
  BkGndEnumThreadList.Flush;
  FreeAndNil(BkGndEnumThreadList);
  FreeAndNil(ExpandMarkThread)

end.










