unit MPShellUtilities;

// Version 2.1.0
//
// The contents of this file are subject to the Mozilla Public License
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
// Portions provided by Derek Moore of Alastria Software
//
//----------------------------------------------------------------------------
//
//  Credits for valuable information and code gathered from newsgroups and
//  websites:
//      Angus Johnson for his GetDiskFreeSpaceFAT32 function from UNDO
//
//----------------------------------------------------------------------------
//
//  June 8, 2010
//    - Added a redirection to TNamespace when items are in the new Windows 7 Libraries.  Creates a new
//       TNamespace based on the real disk object and bypass calls for object Details to this proxy TNamespace.
//       This is because of a bug that mixes up the Size and Date Modified column information.  Also access to
//       the IShellFolder directly in the Library is SLOOOOOOOW compared to access the IShellFolder of the
//       actual file object.  Note this does NOT work on the root library category folders.  The columns are still
//       wrong and enumeration of details is slow but only have this problem at the top level is dramatically better
//       than before.
//
//  May 30, 2010
//    - Fixed bug with ContextMenuCmdCallBack and if the user returns Handled = True from the event
//    - Fixed rare memory leak in TNamespace.GetParent
//

interface

{$I ..\Include\Addins.inc}
{$I Compilers.inc}

{$ifdef COMPILER_12_UP}
  {$WARN IMPLICIT_STRING_CAST OFF}
 {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$endif COMPILER_12_UP}

{.$DEFINE GXDEBUG_DEFMENUCREATE_CALLBACK}
{.$DEFINE GXDEBUG_VIRTUALCONTEXTMENU}
{.$DEFINE GXDEBUG_EXPLORERTHREADINSTANCE}
{.$DEFINE GXDEBUG_EXPLORERTHREADINSTANCE_REFCOUNT}

{$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE_REFCOUNT}
  {$DEFINE GX_DEBUG}
{$ENDIF}
{$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE}
  {$DEFINE GX_DEBUG}
{$ENDIF}
{$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
  {$DEFINE GX_DEBUG}
{$ENDIF}
{$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
  {$DEFINE GX_DEBUG}
{$ENDIF}


{$B-}

{$include Compilers.inc}
{$include Options.inc}

uses
  {$IFDEF GX_DEBUG}
  DbugIntf,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ImgList,
  ShlObj,
  ShellAPI,
  ActiveX,
  Registry,
  MPShellTypes,
  MPCommonObjects,
  MPCommonUtilities,
  MPThreadManager,
  MPResources,
  MPDataObject,
  {$IFDEF TNTSUPPORT}
  TntSysUtils,
  TntClasses,
  TntRegistry,
  TntMenus,
  {$ENDIF}
  {$IFDEF COMPILER_6_UP}
  Variants,
  {$ENDIF}
  {$IFDEF COMPILER_5_UP}
  Contnrs,
  {$ENDIF}
  Menus;

const
  VET_DEFAULT_COLUMN_ARRAY_WIDTH = 37;    // XP Had 37 Columns;

  COMMON_FOLDERTYPE_STRING: array[0..4] of WideString = (
    'cftUnknown',
    'cftFileSystem',
    'cftMyComputer',
    'cftNetwork',
    'cftControlPanel'
  );

  ID_DEFAULT_FILESYSTEM_COLUMNS = '{C5A439E7-960A-495D-AE85-D7662508ACF2}';
  ID_DEFAULT_MYCOMPUTER_COLUMNS = '{19748E93-57DA-4DDB-A80E-C797D178EDCE}';
  ID_DEFAULT_NETWORK_COLUMNS = '{98990F23-9B5D-43A6-9840-40411462028B}';
  ID_DEFAULT_CONTROLPANEL_COLUMNS = '{08312F91-C1B0-4DAA-89F4-CEB331C21010}';
  SCID_DEFAULT_FILESYSTEM_COLUMNS: TGUID = ID_DEFAULT_FILESYSTEM_COLUMNS;
  SCID_DEFAULT_MYCOMPUTER_COLUMNS: TGUID = ID_DEFAULT_MYCOMPUTER_COLUMNS;
  SCID_DEFAULT_NETWORK_COLUMNS: TGUID = ID_DEFAULT_NETWORK_COLUMNS;
  SCID_DEFAULT_CONTROLPANEL_COLUMNS: TGUID = ID_DEFAULT_CONTROLPANEL_COLUMNS;

  PATH_CONTROLPANEL = '::{26EE0668-A00A-44D7-9371-BEB064C98683}';
  PATH_NETWORK = '::';

  ILLEGAL_FILENAME_CHARS: array[0..7] of WideString = (
    '\',
    '/',
    ':',
    '*',
    '?',
    '<',
    '>',
    '|'
  );
  STR_ILLEGAL_FILENAME_CHAR_REPLACEMENT_CHAR = WideChar('-');  // Any illegal character will get replaced with this character

type
  TMPFileAction = (
    mpfaCopy,
    mpfaMove,
    mpfaDelete
  );

  TColumnWidthArray = array[0..VET_DEFAULT_COLUMN_ARRAY_WIDTH - 1] of Integer;
  PColumnWidthArray = ^TColumnWidthArray;


const
  DefaultDetailColumns = 5;

  ID_TIMER_NOTIFY = 100;
  ID_TIMER_ENUMBKGND = 101;
  ID_TIMER_AUTOSCROLL = 102;
  ID_TIMER_SHELLNOTIFY = 103;

  SHORTCUT_ICON_INDEX = 29;  // This is cheezy crappy stupid and dumb but
                             // I can't find a way to get the link index

  SHELL_NAMESPACE_ID = -1;   // ID of a basic Shell Namespace based TNamespace

  SHGDN_FOREDITING = $1000;

  DEFAULTPIDLARRAYSIZE = 8192;     // Default size for the TPIDLArray

  STREAM_VERSION_DEFAULT = -1;  // Default Stream version for TStreamableClass.LoadFromStream
                                // if this value is seen the LoadFromStream method should read the version
                                // from the stream else it should use the passed version in the method

  STR_IMAGE_THREAD_EVENT = 'jdkImageThreadEvent';

  VET_DEFAULT_COLUMNWIDTH = 180;
  VET_DEFAULT_COLUMNWIDTHS: TColumnWidthArray = (
    180,                // Name
    96,                 // Size
    120,                // Type
    120,                // Modified
    60,                 // Attributes
    180,                // Comment
    120,                // Created
    120,                // Accessed
    120,                // Owner
    120,                // Author
    120,                // Title
    120,                // Subject
    120,                // Catagory
    60,                 // Pages
    120,                // Copywrite
    120,                // Company Name
    120,                // Module Description
    120,                // Module Version
    120,                // Product Name
    120,                // Product Version
    72,                 // Sender Name
    90,                 // Recipient Name
    102,                // Recipient Number
    30,                 // Csid
    30,                 // Tsid
    108,                // Transmission Time
    60,                 // Caller ID
    48,                 // Routing
    180,                // Audio Format
    180,                // Sample Rate
    180,                // Audio Sample Size
    180,                // Channels
    180,                // Play Length
    180,                // Frame Count
    180,                // Frame Rate
    180,                // Video Sample Size
    180                 // Video Compression
  );

  VET_DEFAULT_DRIVES_COLUMNWIDTHS: array[0..3] of integer = (
    180,                // Name
    120,                // Type
    96,                 // Total Size
    96                  // Free Space
  );

  VET_DEFAULT_CONTROLPANEL_COLUMNWIDTHS: array[0..1] of integer = (
    180,                // Name
    300                 // Description
  );

  // Control Panel on Vista and up
  VET_DEFAULT_CONTROLPANELEX_COLUMNWIDTHS: array[0..0] of integer = (
    300                 // Name
  );

  VET_DEFAULT_NETWORK_COLUMNWIDTHS: array[0..1] of integer = (
    180,                // Name
    300                 // Description
  );

  MYCOMPUTER_NSE_PATH = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\MyComputer\NameSpace\';


  clExplorerCompressed = TColor($0000FF);
  clExplorerEncrypted = TColor($008040);
  clExplorerFolder = clWindowText;
  clExplorerFile = clWindowText;


{-------------------------------------------------------------------------------}
{ Custom enumerated types                                                       }
{-------------------------------------------------------------------------------}

type
  TExecuteVerbShift = (
    evsCurrent,         // use the current state of the Shift key
    evsUp,              // force the key state of Shift to be up
    evsDown             // force the key state of Shift to be down
  );

  TDefaultFolderIcon = (
    diNormalFolder,        // Retrieve the index for a normal file folder icon
    diOpenFolder,          // Retrieve the index for a normal file folder icon in the open state
    diUnknownFile,         // Retrieve the index for an icon for a file that has no association
    diLink,                // Retrieve the index for link overlay icon
    diMyDocuments          // Index of MyDocuments icon
  );

  TMPBrowseFlag = (
    mpbfComputers,         // Only allow computers to be returned
    mpbfPrinters,          // Only allow printers to be returned
    mpbfDontGoBelowDomain, // Don't allow browsing into other network domains
    mpbfFileSysAncestors,  // Only allow File system ancestors to be returned
    mpbfFileSysFolder,     // Only allow file system folders to be returned
//    mpbfIncludeStatusText, // Includes status test in the dialog
    mpbfIncludeFiles,      // Include files
    mpbfNewStyleDialog,    // New style dialog
    mpbfEditBox,           // Add a edtibox for the use to type into
    mpbfIncludeURLs,       // Include URLs
    mpbfSharable,          // sharable folders
    mpbfMustExist          // Returned folder must exist or cancel pressed before dialog will close
  );
  TMPBrowseFlags = set of TMPBrowseFlag;

type
  TNamespaceState = (
    nsFreePIDLOnDestroy,       // If true the object free the PIDL with the system allocator when freed
    nsIsRecycleBin,            // Recyclebin does not cooperate very well so we have do extra checks for various reasons so cache this
    nsRecycleBinChecked,       // Flag to see if above is valid.
    nsOwnsParent,              // If a namespace is created from a complex PIDL for some methods the parent is needed.  If so a Parent namespace is created so this instance owns it and must free it.
    nsShellDetailsSupported,   // Instead of a costly call to see if interface exists when it does not we only check once then cache the result
    nsShellFolder2Supported,   // Same idea as ShellDetailsSupported, if it does not exist don't waste time constantly checking
    nsShellOverlaySupported,   // Same idea as ShellDetailsSupported, if it does not exist don't waste time constantly checking
    nsThreadedIconLoaded,      // To keep the threaded icon option fast and interuptable need to track if a thread is currently trying to extract the namespace icon index. Retrieved.
    nsThreadedIconLoading,     // To keep the threaded icon option fast and interuptable need to track if a thread is currently trying to extract the namespace icon index. In queue.
    nsThreadedImageLoaded,     // To keep the threaded image option fast and interuptable need to track if a thread is currently trying to extract the namespace thumbnail. Retrieved.
    nsThreadedImageLoading,    // To keep the threaded image option fast and interuptable need to track if a thread is currently trying to extract the namespace thumbnail. In queue.
    nsThreadedImageResizing,   // To keep the threaded image option fast and interuptable need to track if a thread is currently trying to resize the namespace thumbnail. In queue.
    nsThreadedTileInfoLoaded,  // To keep the threaded Tile Info option fast and interuptable need to track if a thread is currently trying to extract Tile Info for the namespace, Threaded Tile Info is retrieved
    nsThreadedTileInfoLoading, // To keeping the threaded Tile Info option fast and interuptable need to track if a thread is currently trying to extract Tile Info for the namespace, it is in the queue to be retieved
    nsIconIndexChanged         // Sets a flag to track when the IconIndex changed between calls to GetIconIndex.  Usually caused by a Thread Setting the icon index
  );
  TNamespaceStates = set of TNamespaceState;

type
//  {$HPPEMIT 'class TNamespace;'}

  TSHLimitInputEdit = function(hWndEdit: HWND; psf: IShellFolder): HRESULT; stdcall;

  IVETShellDetails = IShellDetailsBCB6;

  TShellCache = set of (     // Valid entries in the Namespace data cache
    scInFolderName,
    scNormalName,
    scParsedName,
    scSmallIcon,
    scSmallOpenIcon,
    scOverlayIndex,
    scCreationTime,
    scLastAccessTime,
    scLastWriteTime,
    scFileSize,
    scFileSizeKB,
    scFileSizeInt64,
    scFileType,
    scInvalidIDListData,   // If SHGetDataFromIDList fails flag it so we won't try again.
    scFileSystem,
    scFolder,
    scCanDelete,
    scCanRename,
    scGhosted,
    scCanCopy,
    scCanMove,
    scCanLink,
    scLink,
    scFileSysAncestor,
    scCompressed,
    scFileTimes,
    scSupportedColumns,
    scFolderSize,            // Recursivly calculated size of folder contents
    scDetailsOfCache,        // The DetailsOf Cache for the total number of supported columns is allocated and initialized
    scBrowsable,
    scInLibrary               // Namespace is in the Win7 and up Library Structure
    );

{ This stores the state of the cached folder attribute.                         }
  TCacheAttributes = set of (
    caFileSystem,      // Namespace is part of the file system
    caFolder,          // Namespace is a Folder (not necessarily a directory)
    caCanDelete,       // Namespace can be deleted
    caCanRename,       // Namespace can be renamed
    caGhosted,         // Namespace is should display a ghosted icon
    caCanCopy,         // Namespace can be copied
    caCanMove,         // Namespace can be moved to a different location
    caCanLink,         // Namespace can create a link to itself
    caLink,            // Namespace *is* a link
    caFileSysAncestor, // Namespace is an ancestor of a file system namespace
    caCompressed,      // Namespace represents a compressed folder
    caVirtualHook,           // Namespace is the invisible parent of the RootVirtualNamespace
    caHookedNamespace,       // Namespace has custom sub items injected through SubFolderHook property
    caVirtualNamespace,      // Namespace is a virtual namespace
    caRootVirtualNamespace,  // Namespace is the Root of the Custom Namespace Branch. If a real NS is hooked it is the namespace under the caSubItemHook namespace
    caHardHookedNamespace,   // Namespace is a hard hooked, this means it will show user defined details if defined instead of using the real namespaces parent to create the details
    caBrowsable,
    caInLibrary              // Namespace is in the Win7 and up Library Structure
    );

  { Used in IShellFolder2.GetDefaultColumnState }
  TSHColumnState = (
    csTypeString,     // A string.
    csTypeInt,        // An integer.
    csTypeDate,       // A date.
    csOnByDefault,    // Should be shown by default in the Microsoft® Windows® Explorer Details view.
    csSlow,           // Extracting information about the column can be time consuming.
    csExtended,       // Provided by a handler, not the folder object.
    csSecondaryUI,    // Not displayed in the context menu, but listed in the More dialog box.
    csHidden          // Not displayed in the user interface.
  );
  TSHColumnStates = set of TSHColumnState;

  THotKeyModifier = (      // For IShellLink
    hkmAlt,                // HOTKEYF_ALT
    hkmControl,            // HOTKEYF_CONTROL
    hkmExtendedKey,        // HOTKEYF_EXT
    hkmShift               // HOTKEYF_SHIFT
  );
  THotKeyModifiers = set of THotKeyModifier;

  TCmdShow = (           // For IShellLink
    swHide,              // Hides the window and activates another window.
    swMaximize,          // Maximizes the specified window.
    swMinimize,          // Minimizes the specified window and activates the next top-level window in the Z order.
    swRestore,           // Activates and displays the window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when restoring a minimized window.
    swShow,              // Activates the window and displays it in its current size and position.
    swShowDefault,       // Sets the show state based on the SW_ flag specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application.
    swShowMinimized,     // Activates the window and displays it as a minimized window.
    swShowMinNoActive,   // Displays the window as a minimized window. The active window remains active.
    swShowNA,            // Displays the window in its current state. The active window remains active.
    swShowNoActive,      // Displays a window in its most recent size and position. The active window remains active.
    swShowNormal         // Activates and displays a window. If the window is minimized or maximized, Windows
  );                     //    restores it to its original size and position. An application should specify this flag
                         //    when displaying the window for the first time.

  TIconSize = (
    icSmall,             // Small Shell size icon, usually 16x16
    icLarge              // Large TListview size Icon, usually 32x32
  );

  TFileSort = (       // Used in the ShellSortHelper class
    fsFileType,       // Sort by the File Type name
    fsFileExtension   // Sort by the file extenstion
  );


  TObjectDescription = ( // Return from SHGetDataFromIDList with SHGDFIL_DESCRIPTIONID param
    odError,          // The call Failed for some reason
    odRootRegistered, // The item is a registered item on the desktop.
    odFile,           // The item is a file.
    odDirectory,      // The item is a folder.
    odUnidentifiedFileItem, // The item is an unidentified item in the file system.
    od35Floppy,       // The item is a 3.5-inch floppy drive.
    od525Floppy,      // The item is a 5.25-inch floppy drive.
    odRemovableDisk,  // The item is a removable disk drive.
    odFixedDrive,     // The item is a fixed disk drive.
    odMappedDrive,    // The item is a drive that is mapped to a network share.
    odCDROMDrive,     // The item is a CD-ROM drive.
    odRAMDisk,        // The item is a RAM disk.
    odUnidentifiedDevice, // The item is an unidentified system device.
    odNetworkDomain,  // The item is a network domain.
    odNetworkServer,  // The item is a network server.
    odNetworkShare,   // The item is a network share.
    odNetworkRestOfNet, // Not currently used.
    odUnidentifiedNetwork, // The item is an unidentified network resource.
    odComputerImaging, // Not currently used.
    odComputerAudio,   // Not currently used.
    odShareDocuments  // The item is the system shared documents folder.
  );

  TDetailsColumnTitleInfo = (
    tiCenterAlign,      // The header title is Center Aligned
    tiLeftAlign,        // The header title is Left Aligned
    tiRightAlign,       // The header title is Right Aligned
    tiContainsImage     // The header title is Contains an Image (were do you get the image???)
  );


 { Selects what type of namespaces are enumerated and displayed in VET.          }
  PFileObjects = ^TFileObjects;
  TFileObjects = set of (
    foFolders,
    foNonFolders,
    foHidden,
    foShareable,          // Don't use this flag, not sure what it does but it can cause problems
    foNetworkPrinters,    // Don't use this flag, not sure what it does but it can cause problems
    foInitOnFirstNext,    // allow EnumObject() to return before validating enum, Vista only, in Win 7 this is true by default
    foStorage,            // Include items with accessible storage and their ancestors, including hidden items.
    foNavigationEnum,     // Windows 7 and later. Child folders should provide a navigation enumeration.
    foFastItems,          // Windows Vista and later. The calling application is looking for resources that can be enumerated quickly.
    foFlatList,           // Windows Vista and later. Enumerate items as a simple list even if the folder itself is not structured in that way.
    foEnableAsync,        // Windows Vista and later. The calling application is monitoring for change notifications. This means that the enumerator does not have to return all results. Items can be reported through change notifications.
                          // NOTE: VirtualShellTools maintains the state of this flag as it is dependant of if Change Notification is used or not
    foIncludeSuperHidden  //Windows 7 and later. Include hidden system items in the enumeration. This value does not include hidden non-system items. (To include hidden non-system items, use SHCONTF_INCLUDEHIDDEN.)
   );

  TSHColumnIDArray = array of TSHColumnID;
  TGUIDArray = array of TGUID;
  TWideStringArray = array of WideString;
  TCategoryArray = array of ICategorizer;

  TCategoryInfo = record
    Description: WideString;
    Collapsed: Boolean;
    Hidden: Boolean;
  end;

  TCategoryInfoArray = array of TCategoryInfo;
  TBooleanArray = array of Boolean;

  TCategoriesInfo = record
    ColumnID: TSHColumnIDArray;
    CatGUID: TGUIDArray;
    CategoryNames: TWideStringArray;
    Categories: TCategoryInfoArray;
    CanCatatorize: TBooleanArray;
    DefaultColumn: Integer; // Index into above arrays to the default grouping column
    CategoryCount: Integer;
  end;               

  TBtyeSize = (
    bsKiloBytes,
    bsMegaBytes,
    bsGigiBytes,
    bsTereBytes,
    bsCustom
  );

{-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------}
{ Custom Data structures                                                        }
{-------------------------------------------------------------------------------}

type
  TNamespace = class;       // Forward
  TExtractImage = class;    // Forward
  TCommonShellContextMenu = class; // Forward

  TMenuItemIDArray = array of cardinal;

  TVisibleColumnIndexArray = array of Word; // Array of the column indexes that are currently visible for a namespace

{ Array that contains the cached information for the folder.                    }

  TDetailsOfCacheFlag = (
    docCaptionValid,
    docStatesValid,
    docThreadLoading,
    docThreadLoaded
  );
  TDetailsOfCacheFlags = set of TDetailsOfCacheFlag;

  PDetailsOfCacheRec = ^TDetailsOfCacheRec;
  TDetailsOfCacheRec = packed record
    Cached: TDetailsOfCacheFlags;
    Caption: WideString;
    States: TSHColumnStates;
  end;

  TDetailsOfCacheArray = array of TDetailsOfCacheRec;

  TCacheData = packed record
    Attributes: TCacheAttributes;  // Boolean attributes for the namespace are saved as bits
    SmallIcon,                     // Index in the ShellImageList of the normal icon
    SmallOpenIcon: integer;        // Index in the ShellImageList of the open or selected icon
    InFolderName,                  // InFolder display name for the namespace
    NormalName,                    // Normal display name for the namespace
    ParsedName,                    // The Path of the namespace if it is a file object, if not it is usually the same as NameNormal
    CreationTime,                  // String of the object creation time in details mode
    LastAccessTime,                // String of the last accessed time in details mode
    LastWriteTime,                 // String of the last write time in details mode
    FileSize,                      // String of the file size "23,0000"
    FileSizeKB,                    // String of the file size ala Explorer style i.e. "23 KB"
    FileType: WideString;          // @@@@ FileType shown in Explorer details mode
    FileSizeInt64: Int64;          // Actual File Size
    SupportedColumns: integer;     // Number of supported columns in details mode
    FolderSize: Int64;             // Recursivly calcuated size of folder contents
    OverlayIndex,                  // Cache the Index of the Overlay
    OverlayIconIndex: Integer;      // Cache the Index of the Overlay Icon
    DetailsOfCache: TDetailsOfCacheArray // Cached strings for report view
  end;

{ Cache record tracks which information in the Data structure is valid with the }
{ ShellCacheFlags.                                                              }

  TShellCacheRec = packed record
    ShellCacheFlags: TShellCache;     // If flag is set the corresponding data stored in Data is valid
    Data: TCacheData;                 // Cached data for fast retrieval
  end;

  PSHGetFileInfoRec = ^TSHGetFileInfoRec;
  TSHGetFileInfoRec = packed record
    FileType: WideString;             // Holds the File Type column detail if not using ShellColumns (using VET or custom columns)
  end;


  TShellContextMenuFlag = (
    cmfCanRename,   // This flag is set if the calling application supports renaming of items. A shortcut menu extension or drag-and-drop handler should ignore this flag. A namespace extension should add a rename item to the menu if applicable.
    cmfDefaultOnly, // This flag is set when the user is activating the default action, typically by double-clicking. This flag provides a hint for the shortcut menu extension to add nothing if it does not modify the default item in the menu. A shortcut menu extension or drag-and-drop handler should not add any menu items if this value is specified. A namespace extension should add only the default item (if any).
 //   cmfDisableVerbs, // Microsoft Windows Vista and later: This flag is set when the calling application wants to invoke verbs that are otherwise disabled, such as legacy menus.
    cmfExplore,     // This flag is set when the Microsoft Windows Explorer tree window is present.
    cmfExtendedVerbs, // This flag is set when the calling application wants extended verbs. Normal verbs are displayed when the user right-clicks an object. To display extended verbs, the user must right-click while pressing the SHIFT key.
    cmfIncludeStatic, // Deprecated, do not use.
    cmfItemMenu,     // Microsoft Windows Vista and later: This flag is set when the calling application is invoking a context menu on an item in the view (as opposed to the background of the view).
    cmfNoDefault,  // This flag is set if no item in the menu has been set as the default. A drag-and-drop handler should ignore this flag. A namespace extension should not set any of the menu items to the default.
    cmfNormal,     // Indicates normal operation. A shortcut menu extension, namespace extension, or drag-and-drop handler can add all menu items.
    cmfNoVerbs,   // This flag is set for items displayed in the Send To menu. Shortcut menu handlers should ignore this value.
    cmfVerbsOnly  // This flag is set if the shortcut menu is for a shortcut object. Shortcut menu handlers should ignore this value.
  );
  TShellContextMenuFlags = set of TShellContextMenuFlag;


  TNamespaceArray = array of TNamespace;

  TCommonShellMenuEvent = procedure(Sender: TCommonShellContextMenu) of object;
  {$IFDEF TNTSUPPORT}
  TCommonShellMenuInvokeEvent = procedure(Sender: TCommonShellContextMenu; MenuItem: TTntMenuItem; InvokeInfo: PCMInvokeCommandInfo; var Handled: Boolean) of object;
  {$ELSE}
  TCommonShellMenuInvokeEvent = procedure(Sender: TCommonShellContextMenu; MenuItem: TMenuItem; InvokeInfo: PCMInvokeCommandInfo; var Handled: Boolean) of object;
  {$ENDIF}
  TCommonShellMenuMergeEvent = procedure(Sender: TCommonShellContextMenu; Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags) of object;
  TCommonShellMenuItemEvent = procedure(Sender: TCommonShellContextMenu; ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var Handled: Boolean) of object;
  TCommonShellMenuNewItemEvent = procedure(Sender: TCommonShellContextMenu; NS: TNamespace) of object;
  TCommonShellMenuCopyEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuCreateShortcutEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuCutEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuDeleteEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuNewFolderEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuPasteEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuPasteLinkEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuPasteShortCutEvent = TCommonShellMenuItemEvent;
  TCommonShellMenuProperitesEvent = TCommonShellMenuItemEvent;

  TSHShellFolderView_Message = function(hWndMain : HWND; uMsg : UINT; lParm : LParam) : LResult; stdcall;

{-------------------------------------------------------------------------------}


// Custom Exceptions
  EVSTInvalidFileName = class(Exception)
  end;
  EWow64RevertException = class(Exception)
  end;

  
{-------------------------------------------------------------------------------}
{ Persistent Storing and Recreating VET                                         }
{-------------------------------------------------------------------------------}

  TStreamableClass = class(TPersistent)
  private
    FStreamVersion: integer;
  public
    constructor Create; 
    procedure LoadFromFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure LoadFromStream(S: TStream; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure SaveToFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure SaveToStream(S: TStream; Version: integer = 0; WriteVerToStream: Boolean = False); virtual;

    property StreamVersion: integer read FStreamVersion;
  end;

  TStreamableList = class(TList)
  private
    FStreamVersion: integer;
  public
    constructor Create;
    procedure LoadFromFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure LoadFromStream(S: TStream; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure SaveToFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False); virtual;
    procedure SaveToStream(S: TStream; Version: integer = 0; WriteVerToStream: Boolean = False); virtual;

    property StreamVersion: integer read FStreamVersion;
  end;
{-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------}
{ Holds and indexes TGUIDs that represent columns in windows version that       }
{ IShellFolder2 (Win98 up)                                                      }
{-------------------------------------------------------------------------------}

  TCommonFolderMapType = (
    cftUnknown,            // Unknown FolderType
    cftFileSystem,         // Normal File Folder
    cftMyComputer,         // My Computer (Drives)
    cftNetwork,            // Network Folder (Network Neighborhood}
    cftControlPanel        // Control Panel Folder
    // Make sure to update the follwing if adding new types
    //   [Class Method] TColumnMap.MapNamespaceToFolderMapType
    //   [Constant]     COMMON_FOLDERTYPE_STRING
  );

  TColumnItem = class
  private
 //   FFolderName: WideString;
 //   FFolderType: TCommonFolderMapType;
    FFriendlyName: WideString;
    FGUID: WideString;
    FKey: TPropertyKey;
    FPosition: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FFolderType: TCommonFolderMapType;
    FFolderName: WideString;
  public
    property FolderName: WideString read FFolderName write FFolderName;
    property FolderType: TCommonFolderMapType read FFolderType write FFolderType;
    property FriendlyName: WideString read FFriendlyName write FFriendlyName;
    property GUID: WideString read FGUID write FGUID;
    property Key: TPropertyKey read FKey write FKey;
    property Position: Integer read FPosition write FPosition;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
  end;

  TColumnMap = class
  private
    FList: TList;
    function GetColumnItem(Index: Integer): TColumnItem;
    function GetCount: Integer;
    procedure RestoreDefaultNamespaceColumnMap(FolderName: WideString; ColumnNS: TNamespace; ColumnWidths: TColumnWidthArray);
    procedure SetColumnItem(Index: Integer; Value: TColumnItem);
  protected
    function DefaultControlPanelColumnWidth(iColumn: Integer): Integer;
    function DefaultFileSystemColumnWidth(iColumn: Integer): Integer;
    function DefaultMyComputerColumnWidth(iColumn: Integer): Integer;
    function DefaultNetworkColumnWidth(iColumn: Integer): Integer;
    function MapColumnToDefaultSCID(FolderType: TCommonFolderMapType; i: Integer): TPropertyKey;
    procedure DefaultColumnWidth(FolderType: TCommonFolderMapType; i: Integer);
    procedure ForceandLoadMapItem(FolderNS: TNamespace; ColumnWidths: TColumnWidthArray; FolderName: WideString; FolderType: TCommonFolderMapType; i: Integer; Key: TPropertyKey);
    procedure MimicColumnSCID(ColumnNS: TNamespace; ColumnWidths: TColumnWidthArray; FolderName: WideString; FolderType: TCommonFolderMapType; i: Integer; Key: TPropertyKey);
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    function FindByColumnIndex(FolderNS: TNamespace; iColumn: Integer): TColumnItem;
    function FindByPropertyKey(FolderNS: TNamespace; Key: TPropertyKey): TColumnItem;
    function MapNamespaceToFolderMapType(NS: TNamespace): TCommonFolderMapType;
    procedure Add(Item: TColumnItem);
    procedure Clear;
    procedure RestoreDefaults;
    procedure Sort;
    property ColumnItem[Index: Integer]: TColumnItem read GetColumnItem write SetColumnItem; default;
    property Count: Integer read GetCount;
  end;


{-------------------------------------------------------------------------------}
{ Our own COM like referenced classes                                           }
{-------------------------------------------------------------------------------}

  TReferenceCounted = class
  protected
    FRefCount: integer;
  public
    procedure AddRef;
    procedure Release;
  end;

{ Reference counted TList, much like a COM object but the compiler does not     }
{ add the AddRef and Release call automaticlly.                                 }
  TReferenceCountedList = class(TList)
  protected
    FRefCount: integer;
  public
    procedure AddRef;
    procedure Release;
    property RefCount: integer read FRefCount;
  end;

{-------------------------------------------------------------------------------}
{ Encapsulates IExtractImage, ASCI and Unicode                                  }
{-------------------------------------------------------------------------------}

  TExtractImage = class
  private
    FFlags: Longword;          // Sets how the image is to be handled see IEIFLAG_xxxx
    FPriority: Longword;       // Returns from GetLocation call the priority if IEIFLAG_ASYNC is used above
    FHeight: Longword;         // Desired image height
    FWidth: Longword;          // Desired image Width
    FColorDepth: Longword;     // Desired color depth
    FExtractImageInterface: IExtractImage;    // The interface
    FExtractImage2Interface: IExtractImage2;  // The interface for image2
    FOwner: TNamespace;                       // The Owner namespace
    FPathExtracted: Boolean;
    function GetImage: TBitmap;
    function GetImagePath: WideString;
    function GetExtractImageInterface: IExtractImage;
    function GetExtractImageInterface2: IExtractImage2;
  protected
    property PathExtracted: Boolean read FPathExtracted write FPathExtracted;
  public
    constructor Create;
    property ColorDepth: Longword read FColorDepth write FColorDepth;
    property ImagePath: WideString read GetImagePath;
    property Image: TBitmap read GetImage;
    property ExtractImageInterface: IExtractImage read GetExtractImageInterface;
    property ExtractImage2Interface: IExtractImage2 read GetExtractImageInterface2;
    property Flags: Longword read FFlags write FFlags;
    property Height: Longword read FHeight write FHeight;
    property Owner: TNamespace read FOwner write FOwner;
    property Priority: Longword read FPriority;
    property Width: Longword read FWidth write FWidth;
  end;
{-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------}
{ Encapsulates IShellLink, ASCI and Unicode                                     }
{-------------------------------------------------------------------------------}

  TVirtualShellLink = class(TComponent)
  private
    FFileName: WideString;            // File name of the lnk file
    FShellLinkA: IShellLinkA;           // ShellLink interface
    FShellLinkW: IShellLinkW;         // ShellLinkW interface
    FIconIndex: integer;              // Index of the icon to be used with the link
    FTargetIDList: PItemIDList;       // If the Target is a virtual object the PIDL is the only way to make the link
    FShowCmd: TCmdShow;               // How to show the window of the target application
    FHotKeyModifiers: THotKeyModifiers;  // The key modifiers for short cuts
    FTargetPath: WideString;          // The target that will be executed
    FArguments: WideString;           // Any arguments to be passed to the target
    FDescription: WideString;         // A description that will be shown in the properties dialog
    FWorkingDirectory: WideString;    // The directory the target application will have set as its current directory
    FIconLocation: WideString;        // The file that has the icon for the link
    FHotKey: Word;                    // The HotKey to execute the link, used with the FHotKeyModifiers
    FSilentWrite: Boolean;            // Do not check parameters before writing lnk file and show a warning
    function GetShellLinkAInterface: IShellLinkA;
    function GetShellLinkWInterface: IShellLinkW;
  protected
    procedure FreeTargetIDList;

  public
    destructor Destroy; override;
    function ReadLink(LinkFileName: WideString): Boolean;
    function WriteLink(LinkFileName: WideString): Boolean;

    property Arguments: WideString read FArguments write FArguments;
    property Description: WideString read FDescription write FDescription;
    property FileName: WideString read FFileName write FFileName;
    property HotKey: Word read FHotKey write FHotKey;
    property HotKeyModifiers: THotKeyModifiers read FHotKeyModifiers write FHotKeyModifiers;
    property IconIndex: integer read FIconIndex write FIconIndex;
    property IconLocation: WideString read FIconLocation write FIconLocation;
    property TargetIDList: PItemIDList read FTargetIDList write FTargetIDList;
    property ShellLinkAInterface: IShellLinkA read GetShellLinkAInterface;
    property ShellLinkWInterface: IShellLinkW read GetShellLinkWInterface;
    property ShowCmd: TCmdShow read FShowCmd write FShowCmd; // SW_XXXX contants
    property SilentWrite: Boolean read FSilentWrite write FSilentWrite;
    property TargetPath: WideString read FTargetPath write FTargetPath;
    property WorkingDirectory: WideString read FWorkingDirectory write FWorkingDirectory;
  end;
{-------------------------------------------------------------------------------}

  // General helper class to sort Shell related objects.  Uses mainly to sort
  // columns in details mode
  TShellSortHelper = class
  private
    FFileSort: TFileSort;  // Defines if SortType sorts by the type string or the file extension
  public
    function CompareIDSort(SortColumn: integer; NS1, NS2: TNamespace): Integer; virtual;
    function DiscriminateFolders(NS1, NS2: TNamespace): Integer; virtual;
    function SortFileSize(NS1, NS2: TNamespace): Integer; virtual;
    function SortFileTime(FT1, FT2: TFileTime; NS1, NS2: TNamespace): Integer; virtual;
    function SortString(S1, S2: WideString; NS1, NS2: TNamespace): Integer; virtual;
    function SortType(NS1, NS2: TNamespace): Integer; virtual;

    property FileSort: TFileSort read FFileSort write FFileSort;
  end;
{-------------------------------------------------------------------------------}
{ Function definitions                                                          }
{-------------------------------------------------------------------------------}

  // Return True if VT adds a node to the tree this keeps the item count returned
  // by TNamespace.EnumFolder correct.  To stop the enumeration set Terminate to true
  TEnumFolderCallback = function(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace;
    Data: Pointer; var Terminate: Boolean): Boolean of object;

  TContextMenuCmdCallback = procedure(Namespace: TNamespace; Verb: WideString;
    MenuItemID: Integer;  var Handled: Boolean) of object;
  TContextMenuShowCallback = procedure(Namespace: TNamespace; Menu: hMenu;
    var Allow: Boolean) of object;
  TContextMenuAfterCmdCallback = procedure(Namespace: TNamespace; Verb: WideString;
    MenuItemID: Integer; Successful: Boolean) of object;
{-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------}
{ TNamespace, encapsulates the Windows Shell Namespace                          }
{-------------------------------------------------------------------------------}

  { TNamespace is a class that encapsulates the IShellFolder interface.  It     }
  { simplifies shell interfaces by hiding the overhead of PIDLs and COM.        }
  { Most properties and methods have a direct corrolation to the functions      }
  { exposed by IShellFolder.                                                    }

  TNamespace = class
  private
    FAbsolutePIDL: PItemIDList;            // The Absolute PIDL of that represents the namespace
    FCatInfo: TCategoriesInfo;
    FCurrentContextMenu: IContextMenu;     // The basic interface to create a shell context menu, need to save because of ownerdraw callbacks (maybe this one is not necessary IContextMenu2 only supports this)
    FCurrentContextMenu2: IContextMenu2;   // Extends the context menu interface to include ownerdraw items, need to save because of ownerdraw callbacks
    FDelagateNS: TNamespace;
    FDropTargetInterface: IDropTarget;
    FExtractImage: TExtractImage;          // Encapsulate the seldom used IExtractImage inteface saving memory allocation in the TNamespace when not used
    FIconCache: Integer;                    // Tracks the last known icon that was extracted allows for not flickering from unknown to known icons when refreshing controls
    FImage: TBitmap;                       // The image extracted from the IExtractImage interface
    FNamespaceID: integer;                 // ID of the namespace. Used to pick out any custom namespace objects from real shell supplied ones
    FOldWndProcForContextMenu: TWndMethod; // OldWndProc of the ContextMenu owner used in InternalShowContextMenu
    FParent: TNamespace;                   // The parent of this namespace, may be owned by this decenant see OwnsParent property
    FPropertyStoreInterface: IPropertyStore;
    FRelativePIDL: PItemIDList;            // The relative PILD that can be used the the ParentNamespace.  It is a pointer to the last ID of of AbsolutePILD so *don't* free it
    FShellDetailsInterface: IVETShellDetails; // Interface to deal with the information in the columns in details view (superceded by IShellFolder2 )
    FShellFolder: IShellFolder;            // IShellFolder is the building block interface that defines the namespaces attributes
    FShellFolder2: IShellFolder2;          // Expands IShellFolder handling the column details in Details mode, only works on Win2k-WinMe and up
    FShellIconInterface: IShellIcon;       // Interface to extract only the index of the icon in the system imagelist
    FShellLink: TVirtualShellLink;         // Object to read and write attributes to shortcut namespaces (files)
    FSHGetFileInfoRec: PSHGetFileInfoRec;  // Stores cached info from a call to SHGetFileInfo(A or W)
    FStates: TNamespaceStates;             // Dynamic state of the TNamespace
    FTag: integer;
    FTileDetail: TCommonIntegerDynArray; //
    FQueryInfoInterface: IQueryInfo;       // Interface for the popup InfoTips on folders in Win2k-WinME and up
    FWin32FindDataA: PWin32FindDataA;      // pointer to an allocated structure for an ASCI window file information if is is a file object
    FWin32FindDataW: PWin32FindDataW;      // pointer to an allocated structure for an Unicode window file information if is is a file object
    FSystemIsSuperHidden: Boolean;         // Holds the result of if the system has the SuperHiddenFile flag set in the registry
    FShellIconOverlayInterface: IShellIconOverlay;
    FCategoryProviderInterface: ICategoryProvider;
    FBrowserFrameOptionsInterface: IBrowserFrameOptions;
    FQueryAssociationsInterface: IQueryAssociations;

    function GetCategoryCount: Integer;
    function GetCurrentContextMenu: IContextMenu;
    function GetCurrentContextMenu2: IContextMenu2;
    function GetDelagateNS: TNamespace;
    function GetIsLibraryChild: Boolean;
    function GetJunctionPoint: Boolean;
    function GetJunctionPointResolvePath: WideString;
    function GetParent: TNamespace;
    function GetPropertyStoreInterface: IPropertyStore;
    function GetSymbolicLink: Boolean;
    function GetSymbolicLinkResolvePath: WideString;
    procedure SetCurrentContextMenu(const Value: IContextMenu);
    procedure SetCurrentContextMenu2(const Value: IContextMenu2);
  protected
    { Make the Cache Data and property getters available to decendants. This    }
    { will allow decendants of TNamespace to be created so "virtual namespaces" }
    { can be created.  It is possible to create a "namespace extension" without }
    { really doing it!                                                          }
    { None of interface properties are here because they only make sense for    }
    { actual COM namespaces.                                                    }
    FShellCache: TShellCacheRec;

    function DelegateNamespace: Boolean;
    { Virtual Property Setters }
    function GetArchive: Boolean; virtual;
    function GetAttributesString: WideString; virtual;
    function GetBrowsable: Boolean; virtual;
    function GetBrowserFrameOptionsInterface: IBrowserFrameOptions; virtual;
    function GetCanCopy: Boolean; virtual;
    function GetCanDelete: Boolean; virtual;
    function GetCanLink: Boolean; virtual;
    function GetCanMoniker: Boolean; virtual;
    function GetCanMove: Boolean; virtual;
    function GetCanRename: Boolean; virtual;
    function GetCategoryProviderInterface: ICategoryProvider; virtual;
    function GetCLSID: TGUID; virtual;
    function GetCompressed: Boolean; virtual;
    function GetContextMenu2Interface: IContextMenu2; virtual;
    function GetContextMenu3Interface: IContextMenu3; virtual;
    function GetContextMenuInterface: IContextMenu; virtual;
    function GetCreationDateTime: TDateTime; virtual;
    function GetCreationTime: WideString; virtual;
    function GetCreationTimeRaw: TFileTime; virtual;
    function GetDataObjectInterface: IDataObject; virtual;
    function GetDescription: TObjectDescription; virtual;
    function GetDetailsSupported: Boolean; virtual;
    function GetDirectory: Boolean; virtual;
    function GetDropTarget: Boolean; virtual;
    function GetDropTargetInterface: IDropTarget; virtual;
    function GetEncrypted: Boolean; virtual;
    function GetExtension: WideString; virtual;
    function GetExtractIconAInterface: IExtractIconA; virtual;
    function GetExtractIconWInterface: IExtractIconW; virtual;
    function GetExtractImage: TExtractImage; virtual;
    function GetFileName: WideString; virtual;
    function GetFileSysAncestor: Boolean; virtual;
    function GetFileSystem: Boolean; virtual;
    function GetFileType: WideString; virtual;
    function GetFolder: Boolean; virtual;
    function GetFreePIDLOnDestroy: Boolean; virtual;
    function GetGhosted: Boolean; virtual;
    function GetHasPropSheet: Boolean; virtual;
    function GetHasStorage: Boolean; virtual;
    function GetHasSubFolder: Boolean; virtual;
    function GetHidden: Boolean; virtual;
    function GetIconIndexChanged: Boolean; virtual;
    function GetInfoTip: WideString; virtual;
    function GetIsSlow: Boolean; virtual;
    function GetLastAccessDateTime: TDateTime; virtual;
    function GetLastAccessTime: WideString; virtual;
    function GetLastAccessTimeRaw: TFileTime; virtual;
    function GetLastWriteDateTime: TDateTime; virtual;
    function GetLastWriteTime: WideString; virtual;
    function GetLastWriteTimeRaw: TFileTime; virtual;
    function GetLink: Boolean; virtual;
    function GetNameAddressbar: WideString; virtual;
    function GetNameAddressbarInFolder: WideString; virtual;
    function GetNameForEditing: WideString; virtual;
    function GetNameForEditingInFolder: WideString; virtual;
    function GetNameForParsing: WideString; virtual;
    function GetNameForParsingInFolder: WideString; virtual;
    function GetNameInFolder: WideString; virtual;
    function GetNameNormal: WideString; virtual;
    function GetNameParseAddress: WideString; virtual;
    function GetNameParseAddressInFolder: WideString; virtual;
    function GetNewContent: Boolean; virtual;
    function GetNonEnumerated: Boolean; virtual;
    function GetNormal: Boolean; virtual;
    function GetOffLine: Boolean; virtual;
    function GetOverlayIconIndex: Integer; virtual;
    function GetOverlayIndex: Integer; virtual;
    function GetParentShellDetailsInterface: IVETShellDetails; virtual;
    function GetParentShellFolder: IShellFolder; virtual;
    function GetParentShellFolder2: IShellFolder2; virtual;
    function GetQueryAssociationsInterface: IQueryAssociations; virtual;
    function GetQueryInfoInterface: IQueryInfo;virtual;
    function GetReadOnly: Boolean; virtual;
    function GetReadOnlyFile: Boolean; virtual;
    function GetReparsePoint: Boolean; virtual;
    function GetRemovable: Boolean; virtual;
    function GetShare: Boolean; virtual;
    function GetShellDetailsInterface: IVETShellDetails; virtual;
    function GetShellFolder: IShellFolder; virtual;
    function GetShellFolder2: IShellFolder2; virtual;
    function GetShellIconInterface: IShellIcon; virtual;
    function GetShellIconOverlayInterface: IShellIconOverlay; virtual;
    function GetShellLink: TVirtualShellLink; virtual;
    function GetShortFileName: WideString; virtual;
    function GetSizeOfFile: WideString; virtual;
    function GetSizeOfFileDiskUsage: WideString; virtual;
    function GetSizeOfFileInt64: Int64; virtual;
    function GetSizeOfFileKB: WideString; virtual;
    function GetSparseFile: Boolean; virtual;
    function GetStorage: Boolean; virtual;
    function GetStorageAncestor: Boolean; virtual;
    function GetStream: Boolean; virtual;
    function GetSubFolders: Boolean; virtual;
    function GetSubItems: Boolean; virtual;
    function GetSystem: Boolean; virtual;
    function GetTemporary: Boolean; virtual;
    function GetThreadedDetailLoaded(ColumnIndex: Integer): Boolean; virtual;
    function GetThreadedDetailLoading(ColumnIndex: Integer): Boolean; virtual;
    function GetThreadedIconLoaded: Boolean; virtual;
    function GetThreadedImageLoaded: Boolean; virtual;
    function GetThreadedImageLoading: Boolean; virtual;
    function GetThreadIconLoading: Boolean; virtual;
    function GetValid: Boolean; virtual;
    function ParentWnd: HWnd;
    procedure EnsureDetailCache;
    procedure ExecuteContextMenuVerbMultiPath(Owner: TWinControl; Verb: WideString; Namespaces: TNamespaceArray; ShiftKeyState: TExecuteVerbShift = evsCurrent);
    procedure LoadCategoryInfo;
    procedure SetFreePIDLOnDestroy(const Value: Boolean); virtual;
    procedure SetIconIndexChanged(const Value: Boolean); virtual;
    procedure SetThreadedDetailLoaded(ColumnIndex: Integer; Value: Boolean); virtual;
    procedure SetThreadedDetailLoading(ColumnIndex: Integer; Value: Boolean); virtual;
    procedure SetThreadIconLoading(const Value: Boolean); virtual;
    procedure SetThreadImageLoading(const Value: Boolean); virtual;

    function CreateCategory(GUID: TGUID): ICategorizer;
    function EnumFuncDummy(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace; Data: Pointer; var Terminate: Boolean): Boolean;
    function ExplorerStyleAttributeStringList(CapitalLetters: Boolean): WideString;
    function DisplayNameOf(Flags: Longword): WideString;
    procedure GetDataFromIDList;
    procedure GetFileTimes;
    procedure GetSHFileInfo;
    function InjectCustomSubMenu(Menu: HMenu; Caption: string; PopupMenu: TPopupMenu; var SubMenu: HMenu): TMenuItemIDArray;
    function InternalGetContextMenuInterface(PIDLArray: TRelativePIDLArray): IContextMenu;
    function InternalGetDataObjectInterface(PIDLArray: TRelativePIDLArray): IDataObject;
    function InternalShowContextMenu(Owner: TWinControl; ContextMenuCmdCallback: TContextMenuCmdCallback;
      ContextMenuShowCallback: TContextMenuShowCallback; ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback;
      PIDLArray: TRelativePIDLArray; Position: PPoint;
      CustomShellSubMenu: TPopupMenu; CustomSubMenuCaption: WideString): Boolean;
    function InternalSubItems(Flags: Longword): Boolean;
    procedure ReplacePIDL(NewPIDL: PItemIDList; AParent: TNamespace);
    function ShowContextMenuMultiPath(Owner: TWinControl; Focused: TNamespace; Namespaces: TNamespaceArray; ShowPasteItem, ShowRenameItem, ShowShortCutMenuItem: Boolean; Position: PPoint = nil): Boolean;
    procedure WindowProcForContextMenu(var Message: TMessage);

    property CatInfo: TCategoriesInfo read FCatInfo write FCatInfo;
    // Must ALWAYS release these when they are assigned (they are assigned automatically so be careful)
    // so the ExplorerThread Reference stays in sync
    property CurrentContextMenu: IContextMenu read GetCurrentContextMenu write SetCurrentContextMenu;
    property CurrentContextMenu2: IContextMenu2 read GetCurrentContextMenu2 write SetCurrentContextMenu2;
    property ShellCache: TShellCacheRec read FShellCache write FShellCache;
    property SystemIsSuperHidden: Boolean read FSystemIsSuperHidden write FSystemIsSuperHidden;

  public
    constructor Create(PIDL: PItemIdList; AParent: TNamespace);
    destructor Destroy; override;

    constructor CreateCustomNamespace(CustomID: Integer; AParent: TNamespace); virtual;
    constructor CreateFromFileName(FileName: WideString); virtual;
    function CanCopyAll(NamespaceArray: TNamespaceArray): Boolean; virtual;
    function CanCutAll(NamespaceArray: TNamespaceArray): boolean; virtual;
    function CanDeleteAll(NamespaceArray: TNamespaceArray): Boolean; virtual;
    function CanPasteToAll(NamespaceArray: TNamespaceArray): Boolean; virtual;
    function CanRenameAll(NamespaceArray: TNamespaceArray): Boolean; virtual;
    function CanShowPropertiesOfAll(NamespaceArray: TNamespaceArray): Boolean; virtual;
    function Clone(ReleasePIDLOnDestroy: Boolean): TNameSpace; virtual;
    function ComparePIDL(PIDLToCompare: PItemIDList; IsAbsolutePIDL: Boolean; Column: Integer = 0): ShortInt; virtual;
    function ContextMenuItemHelp(MenuItemID: LongWord): WideString; virtual;
    function ContextMenuVerb(MenuItemID: LongWord): WideString; virtual;
    function Copy(Owner: TWinControl; NamespaceArray: TNamespaceArray): Boolean; virtual;
    function Cut(Owner: TWinControl; NamespaceArray: TNamespaceArray): Boolean; virtual;
    function DataObjectMulti(NamespaceArray: TNamespaceArray): IDataObject; virtual;
    function Delete(Owner: TWinControl; NamespaceArray: TNamespaceArray; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean; virtual;
    function DetailsAlignment(ColumnIndex: Integer): TAlignment; virtual;
    function DetailsColumnTitle(ColumnIndex: integer): WideString; virtual;
    function DetailsDefaultColumnTitle(ColumnIndex: integer): WideString; virtual;
    function DetailsDefaultOf(ColumnIndex: integer): WideString; virtual;
    function DetailsDefaultSupportedColumns: integer; virtual;
    function DetailsGetDefaultColumnState(ColumnIndex: integer): TSHColumnStates; virtual;
    function DetailsOf(ColumnIndex: integer): WideString; virtual;
    function DetailsOfEx(ColumnIndex: integer): WideString; virtual;
    function DetailsSupportedColumns: integer; virtual;
    function DetailsSupportedVisibleColumns: TVisibleColumnIndexArray; virtual;
    function DetailsValidIndex(DetailsIndex: integer): Boolean; virtual;
    function DragEffect(grfKeyState: integer): HRESULT; virtual;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult; virtual;
    function DragLeave: HResult;  virtual;
    function DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;  virtual;
    function Drop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;  virtual;
    function EnumerateFolder(MessageWnd: HWnd; Folders, NonFolders, IncludeHidden: Boolean; EnumFunc: TEnumFolderCallback; UserData: pointer): integer;  virtual;
    function EnumerateFolderEx(MessageWnd: HWnd; FileObjects: TFileObjects; EnumFunc: TEnumFolderCallback; UserData: pointer; AfterValidEnumIDList: TNotifyEvent = nil): integer; virtual;
    function ExecuteContextMenuVerb(Owner: TWinControl; AVerb: WideString; APIDLArray: TRelativePIDLArray; ShiftKeyState: TExecuteVerbShift = evsCurrent; WorkingDir: WideString = ''): Boolean; virtual;
    function FolderSize(Invalidate: Boolean; RecurseFolder: Boolean = False): Int64;  virtual;
    function GetIconIndex(OpenIcon: Boolean; IconSize: TIconSize; ForceLoad: Boolean = True): integer; virtual;
    function GetImage: TBitmap;  virtual;
    function VerifyPIDLRelationship(NamespaceArray: TNamespaceArray; Silent: Boolean = False): Boolean;
    procedure HandleContextMenuMsg(Msg, wParam, lParam: Longint; var Result: LRESULT);  virtual;
    procedure InvalidateCache;  virtual;
    procedure InvalidateDetailsOfCache(FlushStrings: Boolean);
    procedure InvalidateNamespace(RefreshIcon: Boolean = True);  virtual;
    procedure InvalidateRelativePIDL(FileObjects: TFileObjects);  virtual;
    procedure InvalidateThumbImage;  virtual;
    function IsChildByNamespace(TestNamespace: TNamespace; Immediate: Boolean): Boolean;  virtual;
    function IsChildByPIDL(TestPIDL: PItemIDList; Immediate: Boolean): Boolean;  virtual;
    function IsChildOfRemovableDrive: Boolean;  virtual;
    function IsControlPanel: Boolean;  virtual;
    function IsControlPanelChildFolder: Boolean;  virtual;
    function IsDesktop: Boolean; virtual;

    function IsMyComputer: Boolean;  virtual;
    function IsNetworkNeighborhood: Boolean;  virtual;
    function IsNetworkNeighborhoodChild: Boolean;  virtual;
    function IsParentByNamespace(TestNamespace: TNamespace; Immediate: Boolean): Boolean;  virtual;
    function IsParentByPIDL(TestPIDL: PItemIDList; Immediate: Boolean): Boolean;  virtual;
    function IsRecycleBin: Boolean;
    function OkToBrowse(ShowExplorerMsg: Boolean): Boolean;  virtual;
    function ParseDisplayName: PItemIDList;  overload;  virtual;
    function ParseDisplayName(Path: WideString): PItemIDList; overload;  virtual;
    function Paste(Owner: TWinControl; NamespaceArray: TNamespaceArray; AsShortCut: Boolean = False): Boolean; virtual;
    procedure SetDetailByThread(ColumnIndex: Integer; Detail: WideString);
    procedure SetIconIndexByThread(IconIndex: Integer; OverlayIndex: Integer; ClearThreadLoading: Boolean); virtual;
    procedure SetImageByThread(Bitmap: TBitmap; ClearThreadLoading: Boolean);  virtual;
    function SetNameOf(NewName: WideString): Boolean;  virtual;
    function ShellExecuteNamespace(WorkingDir, CmdLineArguments: WideString; ExecuteFolder: Boolean = False;
      ExecuteFolderShortCut: Boolean = False; RunInThread: Boolean = False): Boolean;  virtual;
    function ShowContextMenu(Owner: TWinControl;
      ContextMenuCmdCallback: TContextMenuCmdCallback; ContextMenuShowCallback: TContextMenuShowCallback;
      ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback; Position: PPoint = nil;
      CustomShellSubMenu: TPopupMenu = nil; CustomSubMenuCaption: WideString = ''): Boolean; virtual;
    function ShowContextMenuMulti(Owner: TWinControl; ContextMenuCmdCallback: TContextMenuCmdCallback;
         ContextMenuShowCallback: TContextMenuShowCallback; ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback;
         NamespaceArray: TNamespaceArray; Position: PPoint = nil; CustomShellSubMenu: TPopupMenu = nil;
         CustomSubMenuCaption: WideString = ''; Focused: TNamespace = nil; ShowPasteItem: Boolean = False;
         ShowRenameItem: Boolean = False; ShowShortCutMenuItem: Boolean = False): Boolean; virtual;
    procedure ShowPropertySheet(Owner: TWinControl); virtual;
    procedure ShowPropertySheetMulti(Owner: TWinControl; NamespaceArray: TNamespaceArray; UseSHMultiFileProperties: Boolean = True; ForceNonMultiPath: Boolean = False); virtual;
    function SubFoldersEx(Flags: Longword = SHCONTF_FOLDERS): Boolean;  virtual;
    function SubItemsEx(Flags: Longword = SHCONTF_NONFOLDERS): Boolean;  virtual;
    function TestAttributesOf(Flags: Longword; FlushCache: Boolean; SoftFlush: Boolean = False): Boolean;  virtual;

    property AbsolutePIDL: PItemIDList read FAbsolutePIDL write FAbsolutePIDL;
    property AdvDetailsSupported: Boolean read GetDetailsSupported;
    property Browsable: Boolean read GetBrowsable;
    property BrowserFrameOptionsInterface: IBrowserFrameOptions read GetBrowserFrameOptionsInterface;
    property CanCopy: Boolean read GetCanCopy;
    property CanDelete: Boolean read GetCanDelete;
    property CanLink: Boolean read GetCanLink;
    property CanMoniker: Boolean read GetCanMoniker;
    property CanMove: Boolean read GetCanMove;
    property CanRename: Boolean read GetCanRename;
    property CategoryCount: Integer read GetCategoryCount;
    property CategoryProviderInterface: ICategoryProvider read GetCategoryProviderInterface;
    property CLSID: TGUID read GetCLSID;
    property ContextMenuInterface: IContextMenu read GetContextMenuInterface;
    property ContextMenu2Interface: IContextMenu2 read GetContextMenu2Interface;
    property ContextMenu3Interface: IContextMenu3 read GetContextMenu3Interface;
    property DataObjectInterface: IDataObject read GetDataObjectInterface;
    property DelagateNS: TNamespace read GetDelagateNS;
    property Description: TObjectDescription read GetDescription;
    property DropTarget: Boolean read GetDropTarget;
    property DropTargetInterface: IDropTarget read GetDropTargetInterface;
    property Encrypted: Boolean read GetEncrypted;
    property ExtractImage: TExtractImage read GetExtractImage;
    property ExtractIconAInterface: IExtractIconA read GetExtractIconAInterface;
    property ExtractIconWInterface: IExtractIconW read GetExtractIconWInterface;
    property FileSystem: Boolean read GetFileSystem;
    property FileSysAncestor: Boolean read GetFileSysAncestor;
    property Folder: Boolean read GetFolder;
    property FreePIDLOnDestroy: Boolean read GetFreePIDLOnDestroy write SetFreePIDLOnDestroy;
    property Ghosted: Boolean read GetGhosted;
    property HasPropSheet: Boolean read GetHasPropSheet;
    property HasStorage: Boolean read GetHasStorage;
    property HasSubFolder: Boolean read GetHasSubFolder;
    property IconCache: Integer read FIconCache write FIconCache;
    property IconIndexChanged: Boolean read GetIconIndexChanged write SetIconIndexChanged;
    property IsLibraryChild: Boolean read GetIsLibraryChild;        // Win7 only
    property IsSlow: Boolean read GetIsSlow;
    property JunctionPoint: Boolean read GetJunctionPoint;
    property JunctionPointResolvePath: WideString read GetJunctionPointResolvePath;
    property Link: Boolean read GetLink;
    property InfoTip: WideString read GetInfoTip;
    property NameAddressbar: WideString read GetNameAddressbar;
    property NameAddressbarInFolder: WideString read GetNameAddressbarInFolder;
    property NameForEditing: WideString read GetNameForEditing;
    property NameForEditingInFolder: WideString read GetNameForEditingInFolder;
    property NameForParsing: WideString read GetNameForParsing;
    property NameForParsingInFolder: WideString read GetNameForParsingInFolder;
    property NameInFolder: WideString read GetNameInFolder;
    property NameNormal: WideString read GetNameNormal;
    property NameParseAddress: WideString read GetNameParseAddress;
    property NameParseAddressInFolder: WideString read GetNameParseAddressInFolder;
    property NamespaceID: integer read FNamespaceID;
    property NewContent: Boolean read GetNewContent;
    property NonEnumerated: Boolean read GetNonEnumerated;
    property Parent: TNamespace read GetParent;
    property ParentShellFolder: IShellFolder read GetParentShellFolder;
    property ParentShellFolder2: IShellFolder2 read GetParentShellFolder2;
    property ParentShellDetailsInterface: IVETShellDetails read GetParentShellDetailsInterface;
    property PropertyStoreInterface: IPropertyStore read GetPropertyStoreInterface;
    property QueryAssociationsInterface: IQueryAssociations read GetQueryAssociationsInterface;
    property ReadOnly: Boolean read GetReadOnly;
    property RelativePIDL: PItemIDList read FRelativePIDL;  // Single Item ID of this namespace
    property Removable: Boolean read GetRemovable;
    property Share: Boolean read GetShare;
    property ShellFolder: IShellFolder read GetShellFolder;
    property ShellFolder2: IShellFolder2 read GetShellFolder2;
    property ShellDetailsInterface: IVETShellDetails read GetShellDetailsInterface;
    property ShellLink: TVirtualShellLink read GetShellLink;
    property ShellIconInterface: IShellIcon read GetShellIconInterface;
    property ShellIconOverlayInterface: IShellIconOverlay read GetShellIconOverlayInterface;
    property ShortFileName: WideString read GetShortFileName;
    property SparseFile: Boolean read GetSparseFile;
    property States: TNamespaceStates read FStates write FStates;
    property Storage: Boolean read GetStorage;
    property StorageAncestor: Boolean read GetStorageAncestor;
    property Stream: Boolean read GetStream;
    property SubFolders: Boolean read GetSubFolders;
    property SubItems: Boolean read GetSubItems;
    property SymbolicLink: Boolean read GetSymbolicLink;
    property SymbolicLinkResolvePath: WideString read GetSymbolicLinkResolvePath;
    property Tag: integer read FTag write FTag;
    property ThreadedDetailLoaded[Column: Integer]: Boolean read GetThreadedDetailLoaded write SetThreadedDetailLoaded;
    property ThreadedDetailLoading[Column: Integer]: Boolean read GetThreadedDetailLoading write SetThreadedDetailLoading;
    property ThreadedIconLoaded: Boolean read GetThreadedIconLoaded;
    property ThreadIconLoading: Boolean read GetThreadIconLoading write SetThreadIconLoading;
    property ThreadImageLoaded: Boolean read GetThreadedImageLoaded;
    property ThreadImageLoading: Boolean read GetThreadedImageLoading write SetThreadImageLoading;
    property TileDetail: TCommonIntegerDynArray read FTileDetail write FTileDetail;
    property QueryInfoInterface: IQueryInfo read GetQueryInfoInterface;
    property Win32FindDataA: PWin32FindDataA read FWin32FindDataA;
    property Win32FindDataW: PWin32FindDataW read FWin32FindDataW;
    { Information on namespaces that are actual files.                          }
    property AttributesString: WideString read GetAttributesString; // Explorer type 'RHSA'
    property Archive: Boolean read GetArchive;
    property Compressed: Boolean read GetCompressed;
    property CreationTime: WideString read GetCreationTime;
    property CreationDateTime: TDateTime read GetCreationDateTime;
    property CreationTimeRaw: TFileTime read GetCreationTimeRaw;
    property Directory: Boolean read GetDirectory;
    property Extension: WideString read GetExtension;
    property FileName: WideString read GetFileName;
    property FileType: WideString read GetFileType;
    property Hidden: Boolean read GetHidden;
    property LastAccessTime: WideString read GetLastAccessTime;
    property LastAccessDateTime: TDateTime read GetLastAccessDateTime;
    property LastAccessTimeRaw: TFileTime read GetLastAccessTimeRaw;
    property LastWriteTime: WideString read GetLastWriteTime;
    property LastWriteDateTime: TDateTime read GetLastWriteDateTime;
    property LastWriteTimeRaw: TFileTime read GetLastWriteTimeRaw;
    property Normal: Boolean read GetNormal;
    property OffLine: Boolean read GetOffLine;
    property OverlayIndex: Integer read GetOverlayIndex;
    property OverlayIconIndex: Integer read GetOverlayIconIndex;
    property ReadOnlyFile: Boolean read GetReadOnlyFile;
    property ReparsePoint: Boolean read GetReparsePoint;
    property SizeOfFile: WideString read GetSizeOfFile;
    property SizeOfFileInt64: Int64 read GetSizeOfFileInt64;
    property SizeOfFileKB: WideString read GetSizeOfFileKB;
    property SizeOfFileDiskUsage: WideString read GetSizeOfFileDiskUsage;
    property SystemFile: Boolean read GetSystem;
    property Temporary: Boolean read GetTemporary;
    property Valid: Boolean read GetValid;
 end;
{-------------------------------------------------------------------------------}

  {$IFNDEF COMPILER_5_UP}
  TObjectList = class(TList)
  private
    FOwnsObjects: Boolean;
  protected
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;

    function Add(AObject: TObject): Integer;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer;
    procedure Insert(Index: Integer; AObject: TObject);
    function First: TObject;
    function Last: TObject;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;
  {$ENDIF}

  TVirtualNameSpaceList  = class(TObjectList)
  private
    function GetItems(Index: Integer): TNamespace;
    procedure SetItems(Index: Integer; ANamespace: TNamespace);
  public
    function Add(ANamespace: TNamespace): Integer;
    procedure FillArray(var NamespaceArray: TNamespaceArray);
    function IndexOf(ANamespace: TNamespace): Integer;
    procedure Insert(Index: Integer; ANamespace: TNamespace);
    property Items[Index: Integer]: TNamespace read GetItems write SetItems; default;
  end;

 //
  // Used to operate on files that are not the immediate children of the parent
  // for instance in a search list
  //
  // This class will only work on a system that has Shell32.dll version 4.71 and
  // higher.  If the system has at least IE 4.0 it should be compatible
  //
  // Use this class as the starting point for a custom Shell Context Menu.  It
  // will handle all of the localization of names for you.
  // It will also handle objects that do not have the same immediate parent such
  // as in a search list.
  //
  // There is some danger to this as the app could free the object while
  // the shell still has a connection to it through IShellFolder or IDropTarget.
  // It should be virtually impossible to do this as the shell only connects to
  // the interfaces during the time the menu is open.
  PMenuItemLink = ^TMenuItemLink;
  TMenuItemLink = record
    MenuID: UINT;
    {$IFDEF TNTSUPPORT}
    Item: TTntMenuItem;
    {$ELSE}
    Item: TMenuItem
    {$ENDIF}
  end;

  TMenuItemMap = class(TList)
  protected
    function Get(Index: Integer): PMenuItemLink;
    procedure Put(Index: Integer; Item: PMenuItemLink);
  public
    function Add: PMenuItemLink;
    function First: PMenuItemLink;
    function IndexOf(Item: PMenuItemLink): Integer;
    procedure Clear; override;
    function Insert(Index: Integer): PMenuItemLink; reintroduce;
    function Last: PMenuItemLink;
    function Remove(Item: PMenuItemLink): Integer;
    property Items[Index: Integer]: PMenuItemLink read Get write Put; default;
  end;

  TCommonShellContextMenu = class(TComponent, IUnknown, IShellFolder, IDropTarget)
  private
    FActiveFolder: IShellFolder;
    FActivePIDLs: TCommonPIDLList;
    FCopyValidated: Boolean;
    FCurrentContextMenu: IContextMenu;
    FCurrentContextMenu2: IContextMenu2;
    FCutValidated: Boolean;
    FDeleteValidated: Boolean;
    FExtensions: TCommonShellContextMenuExtensions;
    FFromDesktop: Boolean;
    FLocalFocused: TNamespace;
    FLocalNamespaces: TNamespaceArray;
    FMenuMap: TMenuItemMap;
    FMsgWnd: TWinControl;
    FOldWndProcForContextMenu: TWndMethod;
    {$IFDEF TNTSUPPORT}
    FKeyStrings: TTntStringList;
    {$ELSE}
    FKeyStrings: TStringList;
    {$ENDIF}
    FOnHide: TCommonShellMenuEvent;
    FOnInvokeCommand: TCommonShellMenuInvokeEvent;
    FOnMenuMerge: TCommonShellMenuMergeEvent;
    FOnMenuMergeBottom: TCommonShellMenuMergeEvent;
    FOnMenuMergeTop: TCommonShellMenuMergeEvent;
    FOnShellMenuCopy: TCommonShellMenuCopyEvent;
    FOnShellMenuCreateShortcut: TCommonShellMenuCreateShortcutEvent;
    FOnShellMenuCut: TCommonShellMenuCutEvent;
    FOnShellMenuDelete: TCommonShellMenuDeleteEvent;
    FOnShellMenuNewFolder: TCommonShellMenuNewFolderEvent;
    FOnShellMenuPaste: TCommonShellMenuPasteEvent;
    FOnShellMenuPasteLink: TCommonShellMenuPasteLinkEvent;
    FOnShellMenuPasteShortCut: TCommonShellMenuPasteShortCutEvent;
    FOnShellMenuProperites: TCommonShellMenuProperitesEvent;
    FOnShow: TCommonShellMenuEvent;
    FRefCount: Integer;
    FReferenceCounted: Boolean;
    FRenameMenuItem: Boolean;
    FShortCutMenuItem: Boolean;
    FStub: Pointer;
    FPasteMenuItem: Boolean;
    FUIObjectOfDataObject: IDataObject;
    FUIObjectOfDropTarget: IDropTarget;
  protected
  // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    
  // IShellFolder
    function ParseDisplayName(hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR; out pchEaten: ULONG; out ppidl: PItemIDList; var dwAttributes: ULONG): HResult; stdcall;
    function EnumObjects(hwndOwner: HWND; grfFlags: DWORD; out EnumIDList: IEnumIDList): HResult; stdcall;
    function BindToObject(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult; stdcall;
    function BindToStorage(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvObj{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult; stdcall;
    function CompareIDs(lParam: LPARAM; pidl1, pidl2: PItemIDList): HResult; stdcall;
    function CreateViewObject(hwndOwner: HWND; const riid: TIID; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult; stdcall;
    function GetAttributesOf(cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT): HResult; stdcall;
    function GetUIObjectOf(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList; const riid: TIID; prgfInOut: Pointer; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult; stdcall;
    function GetDisplayNameOf(pidl: PItemIDList; uFlags: DWORD; var lpName: TStrRet): HResult; stdcall;
    function SetNameOf(hwndOwner: HWND; pidl: PItemIDList; lpszName: POLEStr; uFlags: DWORD; var ppidlOut: PItemIDList): HResult; stdcall;
    // IDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;

    function DefMenuCreateCallback(const psf: IShellfolder; wnd: HWND; const pdtObj: IDataObject; uMsg: UINT; WParm: WParam; lParm: LParam): HResult; stdcall;

    procedure AddMenuKey(Key: WideString);
    {$IFDEF TNTSUPPORT}
    procedure AddMenuKeys(Keys: TTntStringList);
    {$ELSE}
    procedure AddMenuKeys(Keys: TStringList);
    {$ENDIF}
    procedure ClearKeys;
    procedure DoCopy(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoCreateShortCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoDelete(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoHide; virtual;
    {$IFDEF TNTSUPPORT}
    procedure DoInvokeCommand(MenuItem: TTntMenuItem; InvokeInfo: PCMInvokeCommandInfo); virtual;
    {$ELSE}
    procedure DoInvokeCommand(MenuItem: TMenuItem; InvokeInfo: PCMInvokeCommandInfo); virtual;
    {$ENDIF}
    procedure DoMenuMerge(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); virtual;
    procedure DoMenuMergeBottom(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); virtual;
    procedure DoMenuMergeTop(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); virtual;
    procedure DoNewFolder(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoPaste(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoPasteLink(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoPasteShortCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoProperties(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean); virtual;
    procedure DoShow; virtual;
    function DuplicateKey(Key: HKEY): HKEY;
    {$IFDEF TNTSUPPORT}
    function FindCommandId(CmdID: UINT; var MenuItem: TTntMenuItem): Boolean;
    {$ELSE}
    function FindCommandId(CmdID: UINT; var MenuItem: TMenuItem): Boolean;
    {$ENDIF}
    procedure HandleContextMenuMsg(Msg, wParam, lParam: Longint; var Result: LRESULT); stdcall;
    function InternalShowContextMenu(Owner: TWinControl; ParentPIDL: PItemIDList; ChildPIDLs: TAbsolutePIDLArray; Verb: WideString; Position: PPoint = nil; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean;
    procedure LoadMultiFolderPIDLArray(Namespaces: TNamespaceArray; var PIDLs: TAbsolutePIDLArray);
    procedure LoadRegistryKeyStrings(Focused: TNamespace); virtual; abstract;
    procedure WindowProcForContextMenu(var Message: TMessage);

    property ActiveFolder: IShellFolder read FActiveFolder write FActiveFolder;
    property ActivePIDLs: TCommonPIDLList read FActivePIDLs write FActivePIDLs;
    property CopyValidated: Boolean read FCopyValidated write FCopyValidated;
    property CurrentContextMenu2: IContextMenu2 read FCurrentContextMenu2 write FCurrentContextMenu2;
    property CurrentContextMenu: IContextMenu read FCurrentContextMenu write FCurrentContextMenu;
    property CutValidated: Boolean read FCutValidated write FCutValidated;
    property Extensions: TCommonShellContextMenuExtensions read FExtensions write FExtensions;
    property FromDesktop: Boolean read FFromDesktop write FFromDesktop;
    {$IFDEF TNTSUPPORT}
    property KeyStrings: TTntStringList read FKeyStrings write FKeyStrings;
    {$ELSE}
    property KeyStrings: TStringList read FKeyStrings write FKeyStrings;
    {$ENDIF}
    property LocalFocused: TNamespace read FLocalFocused write FLocalFocused;
    property LocalNamespaces: TNamespaceArray read FLocalNamespaces write FLocalNamespaces;
    property MenuMap: TMenuItemMap read FMenuMap write FMenuMap;
    property MsgWnd: TWinControl read FMsgWnd write FMsgWnd;
    property OnHide: TCommonShellMenuEvent read FOnHide write FOnHide;
    property OnInvokeCommand: TCommonShellMenuInvokeEvent read FOnInvokeCommand write FOnInvokeCommand;
    property OnMenuMerge: TCommonShellMenuMergeEvent read FOnMenuMerge write FOnMenuMerge;
    property OnMenuMergeBottom: TCommonShellMenuMergeEvent read FOnMenuMergeBottom write FOnMenuMergeBottom;
    property OnMenuMergeTop: TCommonShellMenuMergeEvent read FOnMenuMergeTop write FOnMenuMergeTop;
    property OnShellMenuCopy: TCommonShellMenuCopyEvent read FOnShellMenuCopy write FOnShellMenuCopy;
    property OnShellMenuCreateShortCut: TCommonShellMenuCreateShortcutEvent read FOnShellMenuCreateShortcut write FOnShellMenuCreateShortcut;
    property OnShellMenuCut: TCommonShellMenuCutEvent read FOnShellMenuCut write FOnShellMenuCut;
    property OnShellMenuDelete: TCommonShellMenuDeleteEvent read FOnShellMenuDelete write FOnShellMenuDelete;
    property OnShellMenuNewFolder: TCommonShellMenuNewFolderEvent read FOnShellMenuNewFolder write FOnShellMenuNewFolder;
    property OnShellMenuPaste: TCommonShellMenuPasteEvent read FOnShellMenuPaste write FOnShellMenuPaste;
    property OnShellMenuPasteLink: TCommonShellMenuPasteLinkEvent read FOnShellMenuPasteLink write FOnShellMenuPasteLink;
    property OnShellMenuPasteShortCut: TCommonShellMenuPasteShortCutEvent read FOnShellMenuPasteShortCut write FOnShellMenuPasteShortCut;
    property OnShellMenuProperites: TCommonShellMenuProperitesEvent read FOnShellMenuProperites write FOnShellMenuProperites;
    property OnShow: TCommonShellMenuEvent read FOnShow write FOnShow;
    property RefCount: Integer read FRefCount write FRefCount;
    property ShortCutMenuItem: Boolean read FShortCutMenuItem write FShortCutMenuItem default True;
    property Stub: Pointer read FStub write FStub;
    property RenameMenuItem: Boolean read FRenameMenuItem write FRenameMenuItem default True;
    property PasteMenuItem: Boolean read FPasteMenuItem write FPasteMenuItem default True;
    property UIObjectOfDataObject: IDataObject read FUIObjectOfDataObject write FUIObjectOfDataObject;
    property UIObjectOfDropTarget: IDropTarget read FUIObjectOfDropTarget write FUIObjectOfDropTarget;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF TNTSUPPORT}
    function MergeMenuIntoContextMenu(Menu: TTntPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
    {$ELSE}
    function MergeMenuIntoContextMenu(Menu: TPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
    {$ENDIF}
    procedure ClearMenuMap;
    property DeleteValidated: Boolean read FDeleteValidated write FDeleteValidated;
    property ReferenceCounted: Boolean read FReferenceCounted write FReferenceCounted;
  published
  end;

{*******************************************************************************}
{  Shell ContextMenu                                                            }
{*******************************************************************************}
  TCommonShellBackgroundContextMenu = class(TCommonShellContextMenu)
  private
    FAutoDetectNewItem: Boolean;
    FFinalItemList: TCommonPIDLList;
    FInitialItemList: TCommonPIDLList;
    FOnNewItem: TCommonShellMenuNewItemEvent;
    {$IFDEF TNTSUPPORT}
    FPaste: TTntMenuItem;
    FPasteShortCut: TTntMenuItem;
    FPopupMenuProperties: TTntPopupMenu;
    FPopupMenuPaste: TTntPopupMenu;
    FPopupMenuPasteShortCut: TTntPopupMenu;
    FProperties: TTntMenuItem;
    {$ELSE}
    FPaste: TMenuItem;
    FPasteShortCut: TMenuItem;
    FPopupMenuProperties: TPopupMenu;
    FPopupMenuPaste: TPopupMenu;
    FPopupMenuPasteShortCut: TPopupMenu;
    FProperties: TMenuItem;
    {$ENDIF}
    FShowPasteItem: Boolean;
    FShowPasteShortCutItem: Boolean;
    FShowPropertiesItem: Boolean;
  protected
    procedure ClickPaste(Sender: TObject);
    procedure ClickPasteShortCut(Sender: TObject);
    procedure ClickProperties(Sender: TObject);
    procedure DoHide; override;
    procedure DoMenuMerge(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); override;
    procedure DoMenuMergeBottom(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); override;
    procedure DoMenuMergeTop(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags); override;
    procedure DoNewItem(NS: TNamespace); virtual;
    procedure DoShow; override;
    procedure LoadRegistryKeyStrings(Focused: TNamespace); override;
    property FinalItemList: TCommonPIDLList read FFinalItemList write FFinalItemList;
    property InitialItemList: TCommonPIDLList read FInitialItemList write FInitialItemList;
    {$IFDEF TNTSUPPORT}
    property Paste: TTntMenuItem read FPaste write FPaste;
    property PasteShortCut: TTntMenuItem read FPasteShortCut write FPasteShortCut;
    property PopupMenuProperties: TTntPopupMenu read FPopupMenuProperties write FPopupMenuProperties;
    property PopupMenuPaste: TTntPopupMenu read FPopupMenuPaste write FPopupMenuPaste;
    property PopupMenuPasteShortCut: TTntPopupMenu read FPopupMenuPasteShortCut write FPopupMenuPasteShortCut;
    property Properties: TTntMenuItem read FProperties write FProperties;
    {$ELSE}
    property Paste: TMenuItem read FPaste write FPaste;
    property PasteShortCut: TMenuItem read FPasteShortCut write FPasteShortCut;
    property PopupMenuProperties: TPopupMenu read FPopupMenuProperties write FPopupMenuProperties;
    property PopupMenuPaste: TPopupMenu read FPopupMenuPaste write FPopupMenuPaste;
    property PopupMenuPasteShortCut: TPopupMenu read FPopupMenuPasteShortCut write FPopupMenuPasteShortCut;
    property Properties: TMenuItem read FProperties write FProperties;
    {$ENDIF}
    property RenameMenuItem default False;
    property PasteMenuItem default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EnumCallback(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace; Data: Pointer; var Terminate: Boolean): Boolean;
    function ShowContextMenu(Owner: TWinControl; Root: TNamespace; Position: PPoint = nil): Boolean; virtual;
  published
    property AutoDetectNewItem: Boolean read FAutoDetectNewItem write FAutoDetectNewItem;
    property OnHide;
    property OnInvokeCommand;
    property OnMenuMerge;
    property OnMenuMergeBottom;
    property OnMenuMergeTop;
    property OnNewItem: TCommonShellMenuNewItemEvent read FOnNewItem write FOnNewItem;
    property OnShow;
    property ShowPasteItem: Boolean read FShowPasteItem write FShowPasteItem default True;
    property ShowPasteShortCutItem: Boolean read FShowPasteShortCutItem write FShowPasteShortCutItem default True;
    property ShowPropertiesItem: Boolean read FShowPropertiesItem write FShowPropertiesItem default True;
  end;

  TCommonShellMultiParentContextMenu = class(TCommonShellContextMenu)
  protected
    procedure LoadRegistryKeyStrings(Focused: TNamespace); override;
  public
    function ExecuteContextMenuVerb(Owner: TWinControl; Namespaces: TNamespaceArray; Verb: string; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean; virtual;
    function ShowContextMenu(Owner: TWinControl; Focused: TNamespace; Namespaces: TNamespaceArray; Position: PPoint = nil): Boolean; virtual;
  published
    property OnHide;
    property OnShellMenuCopy;
    property OnShellMenuCreateShortCut;
    property OnShellMenuCut;
    property OnShellMenuDelete;
    property OnShellMenuNewFolder;
    property OnShellMenuPaste;
    property OnShellMenuPasteLink;
    property OnShellMenuPasteShortCut;
    property OnShellMenuProperites;
    property ShortCutMenuItem;
    property RenameMenuItem;
    property PasteMenuItem;
    property OnShow;
  end;

  TExplorerThreadInstance = class(TObject, IUnknown)
  private
    FMaxRef: Integer;
    FRefCount: Integer;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property MaxRef: Integer read FMaxRef;
    property RefCount: Integer read FRefCount write FRefCount;
  end;


  TVirtualExplorerTextColor = (
    vetcEncrypted,
    vetcCompressed,
    vetcFile,
    vetcFolder
  );

{-------------------------------------------------------------------------------}
{ Exported Functions                                                            }
{-------------------------------------------------------------------------------}

  function ExplorerTextColor(TextColor: TVirtualExplorerTextColor): TColor;

  // File Functions
  function DeleteFiles_MP(DataObject: IDataObject; ParentWnd: HWnd; AllowUndo, MultiFolderSource: Boolean): Boolean;

  // Rectange Functions
  function RectWidth(ARect: TRect): integer;
  function RectHeight(ARect: TRect): integer;

  // PIDL Functions
  function FindBrowseableRootPIDL(NS: TNamespace): PItemIDList;
  function IsParentBrowseable(NS: TNamespace): Boolean;
  function NamespaceToAbsolutePIDLArray(Namespaces: TNamespaceArray): TAbsolutePIDLArray;
  function NamespaceToRelativePIDLArray(Namespaces: TNamespaceArray): TRelativePIDLArray;
  function NamespaceToAbsolutePIDLList(Namespaces: TNamespaceArray): TCommonPIDLList;
  function NamespaceToNamespaceList(Namespaces: TNamespaceArray): TList;
  function PathToPIDL(APath: WideString; ParentWindowHandle: HWND = 0; ForceApplicationToTop: Boolean = False): PItemIDList;
  function PIDLToPath(APIDL: PItemIDList): WideString;
  function DirExistsVET(APath: WideString; ShowSystemMessages: Boolean): Boolean; overload;
  function DirExistsVET(NS: TNamespace; ShowSystemMessages: Boolean): Boolean; overload;
  procedure PIDLListQuickSort(PIDLList: TCommonPIDLList; const ParentFolder: IShellFolder; L, R: Integer; SortColumn: Integer = 0);
  procedure PIDLQuickSort(PIDLList: TPIDLArray; const ParentFolder: IShellFolder; L, R: Integer);

  // Time Conversions
  //** NOTE these are not WideString functions they will use ANSI strings internally
  function ConvertLocalStrToTFileTime(LocalStr: WideString; var FileTime: TFileTime): Boolean;
  function ConvertTFileTimeToLocalStr(AFileTime: TFILETIME): WideString;
  function ConvertFileTimetoDateTime(AFileTime : TFileTime): TDateTime;

  // Various Functions
  function CreateSpecialNamespace(FolderID: integer): TNamespace;
  function CreateKnownFolderNamespace(FolderID: TGUID; ForceCreation: Boolean = False; NoAlias: Boolean = True): TNamespace;   // Use the FOLDERID_xxxxx constant in MPShellTypes
  function DefaultSystemImageIndex(FolderType: TDefaultFolderIcon): integer;
  function DefaultSystemImageForFileExt(FileExt: WideString): Integer;
  function FileIconInit(FullInit: BOOL): BOOL; stdcall;
  function IENamespaceShown(PerUser: Boolean): Boolean;
  function GUIDToInterfaceStr(riid: TGUID): String;
  function CFM_FlagsToShellContextMenuFlags(Flags: DWORD): TShellContextMenuFlags;
  function ClipboardContainsShellFormats: Boolean;
  function MapVerbToIntResource(ContextMenu: IContextMenu; Menu: HMenu; Verb: WideString; var IntResVerbW: LPCWSTR; var IntResVerbA: LPCSTR): Boolean;
  {$IFDEF TNTSUPPORT}
  function WideStringListToNullSeparatedDoubleNullEndedString(WideStringList:TTntStringList): PWideChar;
  {$ENDIF}
  function StringListToNullSeparatedDoubleNullEndedString(StringList: TStringList): PChar;

// IShellLink (ShortCut) helpers
  function CreateShellLink(
                           ALnkFilePath,
                           ATargetFilePath: WideString;
                           AnArguments: WideString = '';
                           AWorkingDir: WideString = '';
                           ADescription: WideString = '';
                           AShowCmd: TCmdShow = swShowNormal;
                           AHotKey: Word = 0;
                           AHotKeyModifier: THotKeyModifiers = [];
                           AnIconLocation: WideString = '';
                           AnIconIndex: integer = 0
                         ): Boolean;
  function HotKeyModifiersToStr(HotKeyMod: THotKeyModifiers): WideString;
  function PotentialMappedDrive(NS: TNamespace): Boolean;
  function FileObjectsToFlags(FileObjects: TFileObjects): DWORD;
  function FileObjectsToString(FileObjects: TFileObjects): WideString;

  {$ifdef COMPILER_4}
  procedure FreeAndNil(var Obj);
  function Supports(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
  {$endif}

  function GetDiskFreeSpaceMP(Drive: PWideChar; var SectorsperCluster, BytesperSector, FreeClusters, TotalClusters: DWORD): boolean;
  function DriveSize(Drive: PWideChar): Int64; overload;
  function DriveSize(Drive: PWideChar; ByteSize: TBtyeSize): WideString; overload;
  function DriveFreeSpace(Drive: PWideChar): Int64; overload;
  function DriveFreeSpace(Drive: PWideChar; ByteSize: TBtyeSize): WideString; overload;

  function IsSpecialVariable(TestPath: WideString; var NS: TNamespace): Boolean;
  function SpecialVariableReplacePath(var Path: WideString): Boolean;

  function PIDLIsFolder(APIDL: PItemIDList): Boolean;

//  function MPBrowseForFolder(Title, InitialPath: WideString; BrowseFlags: TMPBrowseFlags): WideString; overload;
  function MPBrowseForFolder(Title, RootFolder, InitialPath: WideString; BrowseFlags: TMPBrowseFlags; var SelectedPath: WideString): Boolean; overload;
  function MPBrowseForFolder(Title: WideString; RootFolder, InitialPath: PItemIDList; BrowseFlags: TMPBrowseFlags; var SelectedPath: PItemIDList): Boolean; overload;

   // Merges a TVirtualShellPopupMenu object into a Shell Context Menu
  {$IFDEF TNTSUPPORT}
  function MergeMenuIntoContextMenu(Menu: TTntPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
  {$ELSE}
  function MergeMenuIntoContextMenu(Menu: TPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
  {$ENDIF}

  // For saving/restoring the Default Column Widths for the VET or Listview
  procedure SaveDefaultColumnWidths(S: TStream);
  procedure LoadDefaultColumnWidths(S: TStream);
  procedure RestoreDefaultColumnWidths;
  procedure ValidateFileObjects(var FileObjects: TFileObjects);

var
 { A few global common Namespaces to be used for various purposes.               }
  PIDLMgr: TCommonPIDLManager;
  GamesFolder: TNamespace = nil;
  DesktopFolder: TNamespace = nil;
  RecycleBinFolder: TNamespace = nil;
  PhysicalDesktopFolder: TNamespace = nil;
  DrivesFolder: TNamespace = nil;
  HistoryFolder: TNamespace = nil;
  PrinterFolder: TNamespace = nil;
  ControlPanelFolder: TNamespace = nil;
  ControlPanelFolderEx: TNamespace = nil;    // WARNING: This may be nil! This is the true Control Panel on Win Vista and up.  The ControlPanelFolder on these OS's seems to be defined as a folder in the folder that is more the CP folder
  NetworkNeighborHoodFolder: TNamespace = nil;
  TemplatesFolder: TNamespace = nil;
  MyDocumentsFolder: TNamespace = nil;
  FavoritesFolder: TNamespace = nil;
  ProgramFilesFolder: TNamespace = nil;
  UserDocumentsFolder: TNamespace = nil;
  UserLibraryFolder: TNamespace = nil;  // Win7 only
  SHLimitInputEdit: TSHLimitInputEdit;
  VET_ColumnWidths: TColumnWidthArray;
  SHShellFolderView_Message: TSHShellFolderView_Message = nil;

  ExplorerThreadInstance: TExplorerThreadInstance;

  AnimateWindow: function(Wnd: HWND; dwTime: Cardinal; dwFlags: Cardinal): WordBool; stdcall;

  // Fundamental Change in Version 2.0.  Should be more correct in what shell dialogs
  // are Modal and if they stay on top of the correct window.
  MP_UseModalDialogs: Boolean = True; // Causes calls to GetUIIObject to use a parent window
  MP_UseSpecialReparsePointOverlay: Boolean = False; // Shows an overlay on Reparse points
  MP_ThreadedShellExecute: Boolean = False; // Causes shell Execute to be launched in a separate thread
  MP_SHSetThreadRef: TSHSetThreadRef = nil;
  MP_SHGetThreadRef: TSHGetThreadRef = nil;
  MP_SHCreateThread: TSHCreateThread = nil;
  MP_SHCreateThreadRef: TSHCreateThreadRef = nil;
  MP_SHReleaseThreadRef: TSHReleaseThreadRef = nil;
  MP_SHSetInstanceExplorer: TSHSetInstanceExplorer = nil;
  MP_SHGetInstanceExplorer: TSHGetInstanceExplorer = nil;


  MP_WNetGetResourceInformationA: TWNetGetResourceInformationA = nil;
  MP_WNetGetResourceInformationW: TWNetGetResourceInformationW = nil;

  // Set this in your application to use as the parent to allow the shell to know
  // what window to use to make the dialogs it shown modal (need to set MP_UseModalDialogs
  // as well)
  GlobalParentWnd: HWnd = 0;


  function Wow64RedirectDisable: Pointer;
  procedure Wow64RedirectRevert(OldWow64: Pointer);

  function LoadShell32Functions: Boolean;

implementation

uses
  Dialogs;

type
  TShellILIsParent = function(PIDL1: PItemIDList; PIDL2: PItemIDList;
    ImmediateParent: LongBool): LongBool; stdcall;
  TShellILIsEqual = function(PIDL1: PItemIDList; PIDL2: PItemIDList): LongBool; stdcall;

var
  ShellILIsParent: TShellILIsParent;
  ShellILIsEqual: TShellILIsEqual;
  MprDLL: THandle = 0;
  ShellFunctionsLoaded: Boolean = False;

function GUIDBinarySearch(FolderType: TCommonFolderMapType; TargetGUID: WideString; pid: Integer; List: TColumnMap; Min, Max: Longint) : Longint;
//
// List must be sorted first
//
var
  Middle, CompareResult : LongInt;
begin
  // During the search the target's index will be between
  // min and max: min <= target index <= max
  if List.Count > 1 then
  begin
    while (Min <= Max) do
    begin
      Middle := (Min + Max) shr 1;
      CompareResult := Integer( List[Middle].FolderType) - Integer( FolderType);
      if CompareResult = 0 then
        CompareResult :=  WideCompareText(List[Middle].GUID, TargetGUID);
      if CompareResult = 0 then
        CompareResult := Integer(List[Middle].Key.pid) - Integer(pid);    // Could cause problems if pid is > 2e6 but that is unlikely
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
  end;

  // If we get here the target is not in the list.
  Result := -1;
end;

function ColumnMapSortByGUID(Item1, Item2: Pointer): Integer;
begin
  Result := Integer( TColumnItem( Item1).FolderType) - Integer( TColumnItem( Item2).FolderType);
  if Result = 0 then
    Result := WideCompareText(TColumnItem( Item1).GUID, TColumnItem( Item2).GUID);
  if Result = 0 then
    Result := TColumnItem( Item1).Key.pid - TColumnItem( Item2).Key.pid;
  if (Result = 0) and (Item1 <> Item2) then
    Assert(True = False, 'Duplicate Column Map Item');
end;

function Wow64RedirectDisable: Pointer;
begin
  // Disable the redirection into the 32bit folder when enumerating a folder with 64bit system files
  Result := nil;
  if Wow64Enabled and Assigned(Wow64DisableWow64FsRedirection_MP) and Assigned(Wow64RevertWow64FsRedirection_MP) then
  begin
    if not Wow64DisableWow64FsRedirection_MP(Result) then
      Result := nil;
  end
end;

procedure Wow64RedirectRevert(OldWow64: Pointer);
begin
  if Wow64Enabled and Assigned(Wow64DisableWow64FsRedirection_MP) and Assigned(Wow64RevertWow64FsRedirection_MP) then
  begin
    if not Wow64RevertWow64FsRedirection_MP(OldWow64) then
      raise EWow64RevertException.Create('Can not reverst 64 Bit Redirection, recommend restarting application');
  end
end;

procedure ValidateFileObjects(var FileObjects: TFileObjects);
begin
  FileObjects := FileObjects - [foShareable, foNetworkPrinters];  //  Don't use this flag, not sure what it does but it can cause problems
  if not IsWinVista then
    FileObjects := FileObjects - [foFastItems, foFlatList, foEnableAsync];
  if not IsWin7 then
    FileObjects := FileObjects - [foIncludeSuperHidden, foNavigationEnum]
end;

////////////////////////////////////////////////////////////////////////////////
// Global Functions
////////////////////////////////////////////////////////////////////////////////

function ExplorerTextColor(TextColor: TVirtualExplorerTextColor): TColor;
var
  Reg: TRegistry;
  DataSize: Integer;
  DataArray: array [0..3] of BYTE;
begin
  // Defaults
  case TextColor of
    vetcCompressed: Result := clExplorerCompressed;
    vetcEncrypted:  Result := clExplorerEncrypted;
    vetcFile: Result := clExplorerFile;
    vetcFolder: Result := clExplorerFolder;
  else
    Result := clExplorerFolder
  end;

  Reg := TRegistry.Create;
  try
    try
      {$IFDEF COMPILER_5_UP}
      Reg.Access := KEY_READ;
      {$ENDIF}
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer', False) then
      begin
        try
          case TextColor of
            vetcCompressed:
              begin
                 if Reg.ValueExists('AltColor') then
                  begin
                    DataSize := Reg.GetDataSize('AltColor');
                    if DataSize = 4 then
                    begin
                      if Reg.ReadBinaryData('AltColor', DataArray, 4) = 4 then
                        Result := TColor(RGB(DataArray[0], DataArray[1], DataArray[2]));
                    end;
                  end;
              end;

            vetcEncrypted:
              begin
                if Reg.ValueExists('AltEncryptionColor') then
                begin
                  DataSize := Reg.GetDataSize('AltEncryptionColor');
                  if DataSize = 4 then
                  begin
                    if Reg.ReadBinaryData('AltEncryptionColor', DataArray, 4) = 4 then
                     Result := TColor(RGB(DataArray[0], DataArray[1], DataArray[2]));
                  end;
                end;
              end
           end;
        finally
          Reg.CloseKey();
        end;
      end
    except
    end
  finally
    Reg.Free;
  end
end;

function DeleteFiles_MP(DataObject: IDataObject; ParentWnd: HWnd; AllowUndo, MultiFolderSource: Boolean): Boolean;
var
  SHFileOpStructA: TSHFileOpStructA;
  SHFileOpStructW: TSHFileOpStructW;
  ShellIDList: TCommonShellIDList;
  NSList: TList;
  i, Len, CharSize: Integer;
  NS: TNamespace;
  PFiles, Head: Pointer;
  CommonDataObj2: ICommonDataObject2;
begin
  try
    ShellIDList := TCommonShellIDList.Create;
    NSList := TList.Create;
    try
      ShellIDList.LoadFromDataObject(DataObject);

      for i := 0 to ShellIDList.PIDLCount - 1 do
        NSList.Add(TNamespace.Create(ShellIDList.AbsolutePIDL(i), nil));

      // Don't call this a MultiFolder Source if we created the data object and prepared it correctly
      // this saves some time with not dealing with formating the information as not to crash shell functions
      if Supports(DataObject, ICommonDataObject2, CommonDataObj2) then
        MultiFolderSource := (MultiFolderSource or CommonDataObj2.MultiFolder) and not CommonDataObj2.MultiFolderVerified;

      if MultiFolderSource then
        StripDuplicatesAndDesktops(NSList);

      Len := 0;
      if IsUnicode then
      begin
        {$IFDEF TNTSUPPORT}
        CharSize := SizeOf(WideChar);
        {$ELSE}
        CharSize := SizeOf(Char);  // Works with D2009 as well
        {$ENDIF}
      end else
        CharSize := SizeOf(AnsiChar);

      for i := 0 to NSList.Count - 1 do
      begin
        NS := TNamespace( NSList[i]);
        if NS.FileSystem then
          Len := Len + (Length( TNamespace( NSList[i]).NameForParsing) + 1) * CharSize; // File name + #0
      end;
      Inc(Len, CharSize);  // Add the second final #0

      GetMem(PFiles, Len);
      FillChar(PFiles^, Len, #0);
      Head := PFiles;

      for i := 0 to NSList.Count - 1 do
      begin
        NS := TNamespace( NSList[i]);
        if CharSize = 2 then
          MoveMemory(Head, PWideChar( NS.NameForParsing), Length(NS.NameForParsing) * CharSize)
        else
          MoveMemory(Head, PAnsiChar( AnsiString( NS.NameForParsing)), Length(NS.NameForParsing) * CharSize);
        Inc(PAnsiChar( Head), (Length(NS.NameForParsing) + 1) * CharSize);
      end;
    finally
      for i := 0 to NSList.Count - 1 do
        TObject(NSList[i]).Free;
      NSList.Free;
      ShellIDList.Free;
    end;


    if Assigned(SHFileOperationW_MP) and (CharSize = 2) then
    begin
      FillChar(SHFileOpStructW, SizeOf(SHFileOpStructW), #0);
      SHFileOpStructW.pFrom := PFiles;
      SHFileOpStructW.pTo := #0;
      SHFileOpStructW.fFlags := 0;
      if AllowUndo then
        SHFileOpStructW.fFlags := SHFileOpStructW.fFlags or FOF_ALLOWUNDO;
      SHFileOpStructW.Wnd := ParentWnd;
      SHFileOpStructW.wFunc := FO_DELETE;
      Result := SHFileOperationW_MP(SHFileOpStructW) = 0;
      FreeMem(SHFileOpStructW.pFrom);
    end
    else
    begin
      FillChar(SHFileOpStructA, SizeOf(SHFileOpStructA), #0);
      SHFileOpStructA.pFrom := PFiles;
      SHFileOpStructA.pTo := #0;
      if AllowUndo then
        SHFileOpStructA.fFlags := SHFileOpStructA.fFlags or FOF_ALLOWUNDO;
      SHFileOpStructA.Wnd := ParentWnd;
      SHFileOpStructA.wFunc := FO_DELETE;
      Result := SHFileOperationA(SHFileOpStructA) = 0;
      FreeMem(SHFileOpStructA.pFrom);
    end;
  except
    Result := False;
  end
end;

procedure SaveDefaultColumnWidths(S: TStream);
var
  i: Integer;
begin
  StreamHelper.WriteInteger(S, Length( VET_ColumnWidths));
  for i := 0 to Length(VET_ColumnWidths) - 1 do
    StreamHelper.WriteInteger(S, VET_ColumnWidths[i])
end;

procedure LoadDefaultColumnWidths(S: TStream);
var
  i, Count: Integer;
begin
  Count := StreamHelper.ReadInteger(S);
  for i := 0 to Count - 1 do
    VET_ColumnWidths[i] := StreamHelper.ReadInteger(S)
end;

procedure RestoreDefaultColumnWidths;
begin
  VET_ColumnWidths := VET_DEFAULT_COLUMNWIDTHS;
end;

{$IFDEF TNTSUPPORT}
function WideStringListToNullSeparatedDoubleNullEndedString(WideStringList: TTntStringList): PWideChar;
//
// Must free the memory assigned to the result with FreeMem when finished
//
var
  Len, i: Integer;
  Head: PWideChar;
begin
  Len := 0;
  for i := 0 to WideStringList.Count - 1 do
    Len := Len + Length(WideStringList[i]) + 1;  // Add the null
  Inc(Len); // Add the second double null
  GetMem(Result, Len * SizeOf(Result^));
  FillChar(Result^, Len * SizeOf(Result^), #0);
  Head := Result;
  for i := 0 to WideStringList.Count - 1 do
  begin
    MoveMemory(Head, PWideChar( WideStringList[i]), Length(WideStringList[i]) * SizeOf(Result^));
    Inc(Head, Length(WideStringList[i]) * SizeOf(Result^) + SizeOf(Result^))
  end;
end;
{$ENDIF}


// Let this be a wide string in D2009 and up
function StringListToNullSeparatedDoubleNullEndedString(StringList: TStringList): PChar;
//
// Must free the memory assigned to the result with FreeMem when finished
// Will work automatically with D2009 and up when PChar is Unicode
//
var
  Len, i: Integer;
  Head: PChar;
begin
  Len := 0;
  for i := 0 to StringList.Count - 1 do
    Len := Len + Length(StringList[i]) + 1;  // Add the null
  Inc(Len); // Add the second double null
  GetMem(Result, Len * SizeOf(Result^));
  FillChar(Result^, Len * SizeOf(Result^), #0);
  Head := Result;
  for i := 0 to StringList.Count - 1 do
  begin
    MoveMemory(Head, PChar( StringList[i]), Length(StringList[i]) * SizeOf(Result^));
    Inc(Head, Length(StringList[i]) * SizeOf(Result^) + SizeOf(Result^))
  end;
end;

// Tests to see if the passed PIDL is a folder or not
function PIDLIsFolder(APIDL: PItemIDList): Boolean;
var
  Parent, Desktop: IShellFolder;
  Flags: UINT;
  Last_CB: Word;
  LastPIDL: PItemIDList;

begin
  Result := False;
  Flags := SFGAO_FOLDER;
  if PIDLMgr.IsDesktopFolder(APIDL) then
    Result := True
  else begin
    SHGetDesktopFolder(Desktop);
    if PIDLMgr.IDCount(APIDL) = 1 then
    begin
      if Succeeded( Desktop.GetAttributesOf(1, APIDL, Flags)) then
        Result := Flags and SFGAO_FOLDER <> 0
    end else
    begin
      PIDLMgr.StripLastID(APIDL, Last_CB, LastPIDL);
      try
        if Succeeded( Desktop.BindToObject(APIDL, nil, IShellFolder, Pointer( Parent))) then
        begin
          LastPIDL^.mkid.cb := Last_CB;
          if Succeeded( Parent.GetAttributesOf(1, LastPIDL, Flags)) then
            Result := Flags and SFGAO_FOLDER <> 0
        end
      finally
        if LastPIDL^.mkid.cb = 0 then
          LastPIDL^.mkid.cb := Last_CB;
      end
    end
  end
end;

function SpecialVariableReplacePath(var Path: WideString): Boolean;

  function ReplacePath(Path, Variable, VarPath: WideString): WideString;
  begin
    Result := WideStringReplace(Path, Variable, VarPath, [rfReplaceAll, rfIgnoreCase]); 
  end;

var
  OldPath: WideString;
begin
  OldPath := Path;

  // Psudo Variables
  Path := ReplacePath(Path, '%sysdir%', WideLowerCase(WideStripTrailingBackslash(SystemDirectory)));
  Path := ReplacePath(Path, '%temp%', WideLowerCase(WideStripTrailingBackslash(WideGetTempDir)));
  Path := ReplacePath(Path, '%appdata%', WideLowerCase(WideStripTrailingBackslash(UserDocumentsFolder.NameForParsing)));
  Path := ReplacePath(Path, '%favorites%', WideLowerCase(WideStripTrailingBackslash(FavoritesFolder.NameForParsing)));
  Path := ReplacePath(Path, '%personal%', WideLowerCase(WideStripTrailingBackslash(MyDocumentsFolder.NameForParsing)));
  Path := ReplacePath(Path, '%templates%', WideLowerCase(WideStripTrailingBackslash(TemplatesFolder.NameForParsing)));
  Path := ReplacePath(Path, '%history%', WideLowerCase(WideStripTrailingBackslash(HistoryFolder.NameForParsing)));
  Path := ReplacePath(Path, '%desktopfolder%', WideLowerCase(WideStripTrailingBackslash(PhysicalDesktopFolder.NameForParsing)));

  // Environment variables
  Path := ReplacePath(Path, Path, WideStripTrailingBackslash(WideExpandEnviromentStringForUser(Path)));
  Path := ReplacePath(Path, Path, WideStripTrailingBackslash(WideExpandEnviromentString(Path)));

{
  Path := ReplacePath(Path, '%userprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%USERPROFILE%')));
  Path := ReplacePath(Path, '%allusersprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%ALLUSERSPROFILE%')));
  Path := ReplacePath(Path, '%programfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramFiles%')));
  Path := ReplacePath(Path, '%systemroot%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemRoot%')));
  Path := ReplacePath(Path, '%systemdrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemDrive%')));
  Path := ReplacePath(Path, '%windir%', WideStripTrailingBackslash(WideExpandEnviromentString('%windir%')));
  Path := ReplacePath(Path, '%tmp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TMP%')));
  Path := ReplacePath(Path, '%temp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TEMP%')));
  Path := ReplacePath(Path, '%public%', WideStripTrailingBackslash(WideExpandEnviromentString('%PUBLIC%')));
  Path := ReplacePath(Path, '%programdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramData%')));
  Path := ReplacePath(Path, '%homedrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEDRIVE%')));
  Path := ReplacePath(Path, '%homepath%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEPATH%')));
  Path := ReplacePath(Path, '%commonprogramfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%CommonProgramFiles%')));
  Path := ReplacePath(Path, '%appdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%APPDATA%')));             }

  Result := OldPath <> Path
end;

function IsSpecialVariable(TestPath: WideString; var NS: TNamespace): Boolean;
var
  PIDL: PItemIDList;
begin
  NS := nil;

  PIDL := nil;

  if WideLowerCase(TestPath) = '%desktop%' then
    PIDL := PIDLMgr.CopyPIDL(DesktopFolder.AbsolutePIDL)
  else
  if WideLowerCase(TestPath) = '%network%' then
    PIDL := PIDLMgr.CopyPIDL(NetworkNeighborHoodFolder.AbsolutePIDL)
  else
  if WideLowerCase(TestPath) = '%printer%' then
    PIDL := PIDLMgr.CopyPIDL(PrinterFolder.AbsolutePIDL)
  else
  if (WideLowerCase(TestPath) = '%drives%') or (WideLowerCase(TestPath) = '%mycomputer%') then
    PIDL := PIDLMgr.CopyPIDL(DrivesFolder.AbsolutePIDL)
  else begin
    if SpecialVariableReplacePath(TestPath) then
      if WideDirectoryExists(TestPath) then
        PIDL := PathToPIDL(TestPath)
  end;

  if Assigned(PIDL) then
    NS := TNamespace.Create(PIDL, nil)
  else

  if not Assigned(NS) and not WideDirectoryExists(TestPath) then
  begin
    // See if it a specially formated CLSID
    PIDL := PathToPIDL(TestPath);
    if Assigned(PIDL) then
      NS := TNamespace.Create(PIDL, nil);
  end;
  Result := Assigned(NS)
end;

type
  IdentityUnmarshal = interface(IUnknown)
    ['{0000001B-0000-0000-C000-000000000046}']
  end;
  
function GUIDToInterfaceStr(riid: TGUID): String;
begin
  if IsEqualGUID(riid, IUnknown) then
     Result := 'IUnknown'
  else
  if IsEqualGUID(riid, IStdMarshalInfo) then
     Result := 'IStdMarshalInfo'
  else
  if IsEqualGUID(riid, IExternalConnection) then
     Result := 'IExternalConnection'
  else
  if IsEqualGUID(riid, IdentityUnmarshal) then
     Result := 'IdentityUnmarshal'
  else
  if IsEqualGUID(riid, IMarshal) then
     Result := 'IMarshal'
  else
  if IsEqualGUID(riid, ICommonDataObject) then
     Result := 'ICommonDataObject'
  else
  if IsEqualGUID(riid, ICommonExtractObj) then
     Result := 'ICommonExtractObj'
  else
  if IsEqualGUID(riid, IOleCommandTarget) then
     Result := 'IOleCommandTarget'
  else
  if IsEqualGUID(riid, IID_IPersistFreeThreadedObject) then
     Result := 'IPersistFreeThreadedObject'
  else
  if IsEqualGUID(riid, IShellView) then
     Result := 'IShellView'
  else
  if IsEqualGUID(riid, IID_ICategoryProvider) then
     Result := 'ICategoryProvider'
  else
  if IsEqualGUID(riid, IID_IQueryAssociations) then
     Result := 'IQueryAssociations'
  else
  if IsEqualGUID(riid, IID_IAssociationArray) then
     Result := 'IAssociationArray'
  else
  if IsEqualGUID(riid, IPersistIDList) then
     Result := 'IPersistIDList'
  else
  if IsEqualGUID(riid, IID_IInternetSecurityManager) then
      Result := 'IInternetSecurityManager'
   else
   if IsEqualGUID(riid, IObjectWithSite) then
      Result := 'IObjectWithSite'
   else
   if IsEqualGUID(riid, IPersist) then
      Result := 'IPersist'
   else
   if IsEqualGUID(riid, IPersistFolder) then
      Result := 'IPersistFolder'
   else
   if IsEqualGUID(riid, IPersistFolder2) then
      Result := 'IPersistFolder2'
   else
   if IsEqualGUID(riid, IShellFolder) then
      Result := 'IShellFolder'
   else
   if IsEqualGUID(riid, IShellFolder2) then
     Result := 'IShellFolder2'
   else
   if IsEqualGUID(riid, IShellFolderViewCB) then
     Result := 'IShellFolderViewCB'
   else
   if IsEqualGUID(riid, IContextMenu) then
     Result := 'IContextMenu'
   else
   if IsEqualGUID(riid, IContextMenu2) then
     Result := 'IContextMenu2'
   else
   if IsEqualGUID(riid, IContextMenu3) then
     Result := 'IContextMenu3'
   else
   {$IFDEF CPPB_6}
     if IsEqualGUID(riid, IBCB6ShellDetails) then
   {$ELSE}
     if IsEqualGUID(riid, IShellDetails) then
   {$ENDIF}
     Result := 'IShellDetails'
   else
   if IsEqualGUID(riid, IStream) then
     Result := 'IStream'
   else
   if IsEqualGUID(riid, IDataObject) then
     Result := 'IDataObject'
   else
   if IsEqualGUID(riid, IDropSource) then
     Result := 'IDropSource'
   else
   if IsEqualGUID(riid, IPropertyDescriptionList) then
     Result := 'IPropertyDescriptionList'
   else
   if IsEqualGUID(riid, IPropertyStore) then
     Result := 'IPropertyStore'
   else
   if IsEqualGUID(riid, IDropTarget) then
     Result := 'IDropTarget'
   else
   if IsEqualGUID(riid, IExtractIconA) then
     Result := 'IExtractIconA'
   else
   if IsEqualGUID(riid, IExtractIconW) then
     Result := 'IExtractIconW'
   else
   if IsEqualGUID(riid, IExtractImage) then
     Result := 'IExtractImage'
   else
   if IsEqualGUID(riid, IQueryInfo) then
     Result := 'IQueryInfo'
   else
   if IsEqualGUID(riid, IShellIcon) then
     Result := 'IShellIcon'
   else
   if IsEqualGUID(riid, IBrowserFrameOptions) then
     Result := 'IBrowserFrameOptions'
   else
   if IsEqualGUID(riid, IBindHost) then
     Result := 'IBindHost'
   else
   if IsEqualGUID(riid, IID_IBindProtocol) then
     Result := 'IBindProtocol'
   else
   if IsEqualGUID(riid, IClassFactory) then
     Result := 'IClassFactory'
   else
   if IsEqualGUID(riid, IShellExtInit) then
     Result := 'IShellExtInit'
   else
   if IsEqualGUID(riid, IShellLinkW) then
     Result := 'IShellLinkW'
   else
   if IsEqualGUID(riid, IShellLinkA) then
     Result := 'IShellLinkW'  
   else
     Result := 'Unknown GUID: ' + GUIDToString(riid)
end;

function CFM_FlagsToShellContextMenuFlags(Flags: DWORD): TShellContextMenuFlags;
begin
  Result := [];
  if CMF_CANRENAME and Flags <> 0 then
    Include(Result, cmfCanRename);
  if CMF_CANRENAME and Flags <> 0 then
    Include(Result, cmfCanRename);
//  if CMF_DISABLEVERBS and Flags <> 0 then
 //   Include(Result, cmfDisableVerbs);
  if CMF_EXPLORE and Flags <> 0 then
    Include(Result, cmfExplore);
  if CMF_INCLUDESTATIC and Flags <> 0 then
    Include(Result, cmfIncludeStatic);
  if CMF_NODEFAULT and Flags <> 0 then
    Include(Result, cmfNoDefault);
  if CMF_NORMAL and Flags <> 0 then
    Include(Result, cmfNormal);
  if CMF_NOVERBS and Flags <> 0 then
    Include(Result, cmfNoVerbs);
  if CMF_VERBSONLY and Flags <> 0 then
    Include(Result, cmfVerbsOnly);
end;

function ClipboardContainsShellFormats: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_SHELLIDLIST) or IsClipboardFormatAvailable(CF_HDROP) or
     IsClipboardFormatAvailable(CF_FILEDESCRIPTORW) or IsClipboardFormatAvailable(CF_FILEDESCRIPTORA)
end;

function MPBrowseForFolderCallback(Wnd: hWnd; uMsg: UINT; lParam: LPARAM; lpData: LPARAM): Integer; stdcall;
begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        // This is the PIDL of the starting folder
        if lpData <> 0 then
          SendMessage(Wnd, BFFM_SETSELECTION, 0, lpData);
      end;
    BFFM_SELCHANGED:
      begin
      end;
    BFFM_VALIDATEFAILEDA:
      begin
      end;
    BFFM_VALIDATEFAILEDW:
      begin
      end;
  end;
  Result := 0;
end;

function MapVerbToIntResource(ContextMenu: IContextMenu; Menu: HMenu; Verb: WideString; var IntResVerbW: LPCWSTR; var IntResVerbA: LPCSTR): Boolean;
//
// Pass the Verb "VIRTUAL_DEFAULT" to select the Default Menu Item
//

const
  LEN_MAXVERB = 128;

var
  VerbFound: Boolean;
  i: Integer;
  MenuID: LongWord;
  VerbW: WideString;
  VerbA: AnsiString;
  StrFound: Boolean;
  defaultID: Longword;
begin
  Result := False;
  if Verb <> '' then
  begin

    if IsUnicode then
      SetLength(VerbW, LEN_MAXVERB)
    else
      SetLength(VerbA, LEN_MAXVERB);

    defaultID := $FFFFFFFF;
    if Verb = 'VIRTUAL_DEFAULT' then
      defaultID := GetMenuDefaultItem(Menu, 0, 0 );


    VerbFound := False;
    i := 0;
    { The result of using the 'verb' string and the MakeIntResource is      }
    { different expecially on system folders.  This forces it to use        }
    { MakeIntResource if it can.                                            }
    while (i < GetMenuItemCount(Menu)) and not VerbFound do
    begin
      MenuID := GetMenuItemID(Menu, i);
      if (MenuID <> $FFFFFFFF) and (MenuID > 0) then
      begin
        if IsUnicode then
        begin
          FillChar(PWideChar(VerbW)^, Length(VerbW) * 2, #0);
          StrFound := Succeeded(ContextMenu.GetCommandString(MenuID-1, GCS_VERBW, nil, Pointer(@VerbW[1]), LEN_MAXVERB));
          if StrFound or (defaultID = MenuID) then
          begin
            SetLength(VerbW, lstrlenW(PWideChar( VerbW)));
            if defaultID = MenuID then
              Verb := VerbW;
            if lstrcmpiW_MP(PWideChar(VerbW), PWideChar(Verb)) = 0 then
            begin
              // Here we directly modify the pointer since the resource is not
              // a pointer to a string but a flag (upper bits = 0) that this is
              // a menu command
              IntResVerbA := MakeIntResourceA(MenuID-1);
              IntResVerbW := MakeIntResourceW(MenuID-1);
              VerbFound := True
            end;
            SetLength(VerbW, LEN_MAXVERB);
          end
        end else
        begin
          FillChar(PAnsiChar(VerbA)^, Length(VerbA), #0);
          StrFound := Succeeded(ContextMenu.GetCommandString(MenuID-1, GCS_VERB, nil, Pointer(@VerbA[1]), LEN_MAXVERB));
          if StrFound or (defaultID = MenuID) then
          begin
            SetLength(VerbA, StrLen(PAnsiChar( VerbA)));
            if defaultID = MenuID then
              Verb := VerbA;
            if lstrcmpiA(PAnsiChar( VerbA), PAnsiChar(AnsiString(Verb))) = 0 then
            begin
              // Here we directly modify the pointer since the resource is not
              // a pointer to a string but a flag (upper bits = 0) that this is
              // a menu command
              IntResVerbA := MakeIntResourceA(MenuID-1);
              VerbFound := True
            end;
            SetLength(VerbA, LEN_MAXVERB);
          end
        end
      end;
      Inc(i)
    end;
    if not VerbFound then
    begin
      // move the string into the supplied buffer
      lstrcpyA(IntResVerbA, PAnsiChar( AnsiString( Verb)));
      if IsUnicode then
        lstrcpyW_MP(IntResVerbW, PWideChar( Verb));
    end;
  end;
end;

function MPBrowseForFolder(Title: WideString; RootFolder, InitialPath: PItemIDList; BrowseFlags: TMPBrowseFlags; var SelectedPath: PItemIDList): Boolean;

  function FlagsToTMPBrowseFlags(Flags: TMPBrowseFlags): DWORD;
  begin
    Result := 0;
    if mpbfComputers in Flags then
      Result := Result or BIF_BROWSEFORCOMPUTER;
    if mpbfPrinters in Flags then
      Result := Result or BIF_BROWSEFORPRINTER;
    if mpbfDontGoBelowDomain in Flags then
      Result := Result or BIF_DONTGOBELOWDOMAIN;
    if mpbfFileSysAncestors in Flags then
      Result := Result or BIF_RETURNFSANCESTORS;
    if mpbfFileSysFolder in Flags then
      Result := Result or BIF_RETURNONLYFSDIRS;
  //  if mpbfIncludeStatusText in Flags then
 //     Result := Result or BIF_STATUSTEXT;
    if mpbfIncludeFiles in Flags then
      Result := Result or BIF_BROWSEINCLUDEFILES;
    if mpbfNewStyleDialog in Flags then
      Result := Result or BIF_NEWDIALOGSTYLE;
    if mpbfEditBox in Flags then
      Result := Result or BIF_EDITBOX;
    if mpbfIncludeURLs in Flags then
      Result := Result or BIF_BROWSEINCLUDEURLS;
    if mpbfSharable in Flags then
      Result := Result or BIF_SHAREABLE;
    if mpbfMustExist in Flags then
      Result := Result or BIF_VALIDATE;
  end;

var
  BrowseInfoW: TBrowseInfoW;
  BrowseInfoA: TBrowseInfoA;
  DisplayNameA: array [0..MAX_PATH] of AnsiChar;
  DisplayNameW: array [0..MAX_PATH] of WideChar;
begin
  if IsUnicode then
  begin
    FillChar(BrowseInfoW, SizeOf(BrowseInfoW), #0);
    BrowseInfoW.hwndOwner := GetActiveWindow;
    BrowseInfoW.pidlRoot := RootFolder;
    BrowseInfoW.lParam := Integer( InitialPath);
    BrowseInfoW.pszDisplayName := DisplayNameW;
    {$IFDEF CPPB}
    BrowseInfoW.lpfn := MPBrowseForFolderCallback;
    {$ELSE}
    BrowseInfoW.lpfn := @MPBrowseForFolderCallback;
    {$ENDIF}
    BrowseInfoW.lpszTitle := PWideChar(Title);
    BrowseInfoW.ulFlags := FlagsToTMPBrowseFlags(BrowseFlags);
    SelectedPath := SHBrowseForFolderW_MP(BrowseInfoW);
  end else
  begin
    FillChar(BrowseInfoA, SizeOf(BrowseInfoA), #0);
    BrowseInfoA.hwndOwner := GetActiveWindow;
    BrowseInfoW.pidlRoot := RootFolder;
    BrowseInfoA.lParam := Integer( InitialPath);
    BrowseInfoA.pszDisplayName := DisplayNameA;
    {$IFDEF CPPB}
    BrowseInfoA.lpfn := MPBrowseForFolderCallback;
    {$ELSE}
    BrowseInfoA.lpfn := @MPBrowseForFolderCallback;
    {$ENDIF}
    BrowseInfoA.lpszTitle := PAnsiChar(AnsiString(Title));
    BrowseInfoA.ulFlags := FlagsToTMPBrowseFlags(BrowseFlags);
    SelectedPath := SHBrowseForFolderA(BrowseInfoA);
  end;
  Result := Assigned(SelectedPath)
end;

function MPBrowseForFolder(Title, RootFolder, InitialPath: WideString; BrowseFlags: TMPBrowseFlags; var SelectedPath: WideString): Boolean; overload;
var
  NS: TNamespace;
  RootPIDL, InitialPathPIDL, ReturnPIDL: PItemIDList;
begin
  if RootFolder = '' then
    RootPIDL := Desktopfolder.AbsolutePIDL
  else
    RootPIDL := PathToPIDL(RootFolder);
  InitialPathPIDL := PathToPIDL(InitialPath);
  ReturnPIDL := nil;
  Result := MPBrowseForFolder(Title, RootPIDL, InitialPathPIDL, BrowseFlags, ReturnPIDL);
  if Result then
  begin
    NS := TNamespace.Create(ReturnPIDL, nil);
    SelectedPath := NS.NameParseAddress;
    NS.Free;
  end;
  PIDLMgr.FreePIDL(RootPIDL);
  PIDLMgr.FreePIDL(InitialPathPIDL);
end;

{$IFDEF TNTSUPPORT}
function MergeMenuIntoContextMenu(Menu: TTntPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(Menu) and (ContextMenu <> 0) then
  begin
    Result := idStart;
    for i := Menu.Items.Count - 1 downto 0 do
    begin
      AddContextMenuItem(ContextMenu, Menu.Items[i].Caption, Index, Result);
      Inc(Result);
      if Menu.Items[i].Count > 0 then
        beep;
    end
  end
end;
{$ELSE}
function MergeMenuIntoContextMenu(Menu: TPopupMenu; ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(Menu) and (ContextMenu <> 0) then
  begin
    Result := idStart;
    for i := Menu.Items.Count - 1 downto 0 do
    begin
      AddContextMenuItem(ContextMenu, Menu.Items[i].Caption, Index, Result);
      Inc(Result);
      if Menu.Items[i].Count > 0 then
        beep;
    end
  end
end;
{$ENDIF}



// PIDL Functions

{ ----------------------------------------------------------------------------- }
function NamespaceToRelativePIDLArray(Namespaces: TNamespaceArray): TRelativePIDLArray;
var
  i: integer;
begin
  Result := nil;
  if Assigned(Namespaces) then
  begin
    SetLength(Result, Length(Namespaces));
    for i := 0 to Length(Namespaces) - 1 do
      Result[i] := Namespaces[i].RelativePIDL;
  end
end;

function NamespaceToAbsolutePIDLList(Namespaces: TNamespaceArray): TCommonPIDLList;
var
  i: integer;
begin
  Result := TCommonPIDLList.Create;
  Result.SharePIDLs := True;
  if Assigned(Namespaces) then
  begin
    for i := 0 to Length(Namespaces) - 1 do
      Result.Add(PIDLMgr.CopyPIDL(Namespaces[i].AbsolutePIDL))
  end
end;

function NamespaceToNamespaceList(Namespaces: TNamespaceArray): TList;
var
  i: Integer;
begin
  Result := TList.Create;
  for i := 0 to Length(Namespaces) - 1 do
    Result.Add(Namespaces[i])
end;

function FindBrowseableRootPIDL(NS: TNamespace): PItemIDList;
var
  Found: Boolean;
begin
  Result := nil;
  Found := False;
  if Assigned(NS) then
  begin
    if not NS.Folder then
      NS := NS.Parent;
    if IsParentBrowseable(NS) then
    begin
      while not Found and not NS.IsDesktop do
      begin
        if NS.Browsable then
          Found := True
        else
          NS := NS.Parent
      end
    end;
    Result := PIDLMgr.CopyPIDL(NS.AbsolutePIDL)
  end
end;

function IsParentBrowseable(NS: TNamespace): Boolean;
begin
  Result := False;
  if Assigned(NS) then
  begin
    while not Result and not NS.IsDesktop do
    begin
      Result := NS.Browsable;
      if not Result then
        NS := NS.Parent
    end
  end
end;

function NamespaceToAbsolutePIDLArray(Namespaces: TNamespaceArray): TAbsolutePIDLArray;
var
  i: integer;
begin
  Result := nil;
  if Assigned(Namespaces) then
  begin
    SetLength(Result, Length(Namespaces));
    for i := 0 to Length(Namespaces) - 1 do
      Result[i] := Namespaces[i].AbsolutePIDL;
  end
end;
{ ----------------------------------------------------------------------------- }

function PathToPIDL(APath: WideString; ParentWindowHandle: HWND = 0; ForceApplicationToTop: Boolean = False): PItemIDList;
// Takes the passed Path and attempts to convert it to the equavalent PIDL
var
  Desktop: IShellFolder;
  pchEaten, dwAttributes: ULONG;
begin
  Result := nil;
  begin
    SHGetDesktopFolder(Desktop);
    dwAttributes := 0;
    if Assigned(Desktop) then
      Desktop.ParseDisplayName(ParentWindowHandle, nil, PWideChar(APath), pchEaten, Result, dwAttributes);
    // Message boxes are set as children of the Application window
    // Appears ParseDisplayName looks for the top level and first in the owner chain regardless of the ParentWindowHandle
    if Assigned(Application) and ForceApplicationToTop then
      BringWindowToTop(Application.Handle);
  end
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function PIDLToPath(APIDL: PItemIDList): WideString;
var
  Folder: TNamespace;
begin
  Result := '';
  Folder := TNamespace.Create(APIDL, nil);
  try
    Folder.FreePIDLOnDestroy := False;
    if Assigned(Folder) then
      Result := Folder.NameForParsing;
  finally
    Folder.Free
  end
end;


{ ----------------------------------------------------------------------------- }

function IENamespaceShown(PerUser: Boolean): Boolean;
const
  IE_KEYPATH = 'Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel';
  IE_KEYVALUE = '{871C5380-42A0-1069-A2EA-08002B30309D}';
var
  Reg: TRegistry;
  KeyFound: Boolean;
  KeyType: DWORD;
  KeyValueStr: string;
  KeyValueInt: Integer;
begin
  Result := True;
  KeyFound := False;
  Reg := TRegistry.Create;
  try
    if PerUser then
      Reg.RootKey := HKEY_CURRENT_USER
    else
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.KeyExists(IE_KEYPATH) then
    begin
      if Reg.OpenKeyReadOnly(IE_KEYPATH) then
      begin
        if Reg.ValueExists(IE_KEYVALUE) then
          if RegQueryValueExA(Reg.CurrentKey, PAnsiChar( AnsiString(IE_KEYVALUE)), nil, @KeyType, nil, nil) = ERROR_SUCCESS then
          begin
            // Once in while there is a system that does not have an integer in this slot
            if KeyType = REG_DWORD then
            begin
              Result := Reg.ReadInteger(IE_KEYVALUE) = 0;
              KeyFound := True
            end;
            if KeyType = REG_SZ then
            begin
              KeyValueStr := Reg.ReadString(IE_KEYVALUE);
              if TryStrToInt(KeyValueStr, KeyValueInt) then
              begin
                Result := KeyValueInt = 0;
                KeyFound := True
              end
            end
          end
      end
    end
  finally
    Reg.Free;
    // if the key does not exist per user then fall back to machine
    if PerUser and not KeyFound then
      Result := IENamespaceShown(False)
  end;
end;

{ ----------------------------------------------------------------------------- }
function DirExistsVET(APath: WideString; ShowSystemMessages: Boolean): Boolean; overload;
const
  FLAGS = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
var
  Desktop, Folder: IShellFolder;
  TempPIDL, PIDL: PItemIDList;
  EnumIDList: IEnumIDList;
  hWndOwner: THandle;
  TempPath: WideString;
begin
  Result := False;
  PIDL := nil;
  if ShowSystemMessages then
    hWndOwner := GetActiveWindow //Application.Handle
  else
    hWndOwner := 0;

  TempPath := WideExtractFileDrive(APath) + '\';
  // First make sure the drive is available, it may be a remoted password protected drive
  TempPIDL := PathToPIDL(TempPath);
  if Succeeded(SHGetDesktopFolder(Desktop)) then
    if Succeeded(Desktop.BindToObject(TempPIDL, nil, IShellFolder, Pointer(Folder))) then
      if Folder.EnumObjects(hWndOwner, FLAGS, EnumIDList)= NOERROR then
      begin
        PIDL := PathToPIDL(APath);
        if Succeeded(Desktop.BindToObject(PIDL, nil, IShellFolder, Pointer(Folder))) then
          Result := Folder.EnumObjects(hWndOwner, FLAGS, EnumIDList)= NOERROR
      end;
  if Assigned(TempPIDL) then
    coTaskMemFree(TempPIDL);
  if Assigned(PIDL) then
    coTaskMemFree(PIDL);
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function DirExistsVET(NS: TNamespace; ShowSystemMessages: Boolean): Boolean; overload;
begin
  Result := DirExistsVET(NS.NameForParsing, ShowSystemMessages)
end;

{ ----------------------------------------------------------------------------- }
function RectWidth(ARect: TRect): integer;
begin
  Result := ARect.Right - ARect.Left
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function RectHeight(ARect: TRect): integer;
begin
  Result := ARect.Bottom - ARect.Top
end;
{ ----------------------------------------------------------------------------- }

function PotentialMappedDrive(NS: TNamespace): Boolean;
// A mapped drive will not return valid information, other then
// its display name under some conditions so always try it.
var
  DriveType: DWORD;
begin
  Result := False;
  if WideIsDrive(NS.NameForParsing) then
  begin
    if Assigned(GetDriveTypeW_MP) then
      DriveType := GetDriveTypeW_MP(PWideChar(NS.NameForParsing))
    else
      DriveType := GetDriveTypeA(PAnsiChar(AnsiString(NS.NameForParsing)));
    Result := (DriveType = DRIVE_NO_ROOT_DIR) or (DriveType = DRIVE_REMOTE)
  end
end;
{ ----------------------------------------------------------------------------- }

function FileObjectsToFlags(FileObjects: TFileObjects): DWORD;
begin
  ValidateFileObjects(FileObjects);
  Result := 0;
  if foFolders in FileObjects then
    Result := Result or SHCONTF_FOLDERS;
  if foNonFolders in FileObjects then
    Result := Result or SHCONTF_NONFOLDERS;
  if foHidden in FileObjects then
    Result := Result or SHCONTF_INCLUDEHIDDEN;
  if foStorage in FileObjects then
    Result := Result or SHCONTF_STORAGE;

  if IsWinVista then
  begin
    if foInitOnFirstNext in FileObjects then
      Result := Result or SHCONTF_INIT_ON_FIRST_NEXT;
    if foFlatList in FileObjects then
      Result := Result or SHCONTF_FLATLIST;
    if foFastItems in FileObjects then
      Result := Result or SHCONTF_FASTITEMS;
    if foEnableAsync in FileObjects then
      Result := Result or SHCONTF_ENABLE_ASYNC;
    if IsWin7 then
    begin
      if foNavigationEnum in FileObjects then
        Result := Result or SHCONTF_NAVIGATION_ENUM;
      if foIncludeSuperHidden in FileObjects then
        Result := Result or SHCONTF_INCLUDESUPERHIDDEN;
    end;
  end;
  if IsUnicode and not IsWinNT4 then
  begin
    if foShareable in FileObjects then
      Result := Result or SHCONTF_SHAREABLE;
    if foNetworkPrinters in FileObjects then
      Result := Result or SHCONTF_NETPRINTERSRCH;
  end;
end;

function FileObjectsToString(FileObjects: TFileObjects): WideString;
begin
  Result := '';
  if foFolders in FileObjects then
    Result := Result + 'Folders, ';
  if foNonFolders in FileObjects then
    Result := Result + 'NonFolders, ';
  if foHidden in FileObjects then
    Result := Result + 'Hidden, ';
  if foShareable in FileObjects then
    Result := Result + 'Shareable, ';
  if foNetworkPrinters in FileObjects then
    Result := Result + 'NetworkPrinters, ';
  if foStorage in FileObjects then
    Result := Result + 'Storage, ';
  if foInitOnFirstNext in FileObjects then
    Result := Result + 'Init On First Next, ';
  if foFlatList in FileObjects then
    Result := Result + 'Flat List, ';
  if foFastItems in FileObjects then
    Result := Result + 'Fast Items, ';
  if foEnableAsync in FileObjects then
    Result := Result + 'Enable Async, ';
  if foNavigationEnum in FileObjects then
    Result := Result + 'Navigation Enumeration, ';
  if foIncludeSuperHidden in FileObjects then
    Result := Result + 'Include Super Hidden, ';

  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 2)
end;

// Time Conversions

{ ----------------------------------------------------------------------------- }
// ANSI
function ConvertLocalStrToTFileTime(LocalStr: WideString;
  var FileTime: TFileTime): Boolean;
var
  SystemTime: TSystemTime;
begin
  Result := True;
  try
     DateTimeToSystemTime(StrToDateTime(LocalStr), SystemTime)
  except
    on EConvertError do Result := False;
  end;
  if Result then
    Result := SystemTimeToFileTime(SystemTime, FileTime);
end;
{ ----------------------------------------------------------------------------- }

function ValidFileTime(FileTime: TFileTime): Boolean;
begin
 Result := (FileTime.dwLowDateTime <> 0) or (FileTime.dwHighDateTime <> 0);
end;

{ ----------------------------------------------------------------------------- }
// Converts a TFileTime structure into a local Time/Date String.  This requires
// a check to make sure the TFileTime structure contains some info through
// the local function ValidFileTime then trying to convert the UTC time to Local
// UTC time.  Then finally changing the UTC time to System time.
// ANSI/
function ConvertTFileTimeToLocalStr(AFileTime: TFILETIME): WideString;
var
  SysTime: TSystemTime;
  LocalFileTime: TFILETIME;
begin
  if ValidFileTime(AFileTime)
  and FileTimeToLocalFileTime(AFileTime, LocalFileTime)
  and FileTimeToSystemTime(LocalFileTime, SysTime) then
  try
    Result := DateTimeToStr(SystemTimeToDateTime(SysTime))
  except
    Result := '';
  end
  else
     Result := '';
end;
{ ----------------------------------------------------------------------------- }

         
function ConvertFileTimetoDateTime(AFileTime : TFileTime): TDateTime;
var
  SysTime: TSystemTime;
  LocalFileTime: TFILETIME;
begin
  if ValidFileTime(AFileTime)
  and FileTimeToLocalFileTime(AFileTime, LocalFileTime)
  and FileTimeToSystemTime(LocalFileTime, SysTime) then
  try
    Result := SystemTimeToDateTime(SysTime);
  except
    Result := 0;
  end
  else
     Result := 0;
end;

  // Various Functions

{ -----------------------------------------------------------------------------}
function CreateKnownFolderNamespace(FolderID: TGUID; ForceCreation: Boolean = False; NoAlias: Boolean = True): TNamespace;
// Creates a TNamespace based on the SpecialFolders defined by
// SHSHGetKnownFolderIDList.
//
//  - If Force Creation is true then the folder will be created
//  - If NoAlias then the namespace represents the folder on the physical file path (if applicable
//       to the folder) else it will be a folder on the virtual namespace path
//       (with GUIDs in the path name)
//
//   Only avaialble in Windows Vista and UP
//
var
  PIDL: PItemIDList;
  F: IShellFolder;
  Flags: DWORD;
begin
  Result := nil;
  Flags := 0;
  if Assigned(SHGetKnownFolderIDList_MP) then
  begin
    if ForceCreation then
      Flags := Flags or (KF_FLAG_CREATE or KF_FLAG_INIT);
    if NoAlias then
      Flags := Flags or KF_FLAG_NO_ALIAS;  
    if SHGetKnownFolderIDList_MP(FolderID, Flags, 0, PIDL) = S_OK then
     if Assigned(PIDL) then
      begin
        Result := TNamespace.Create(PIDL, nil);
        F := Result.ParentShellFolder // just force the namespace to have Parent
      end
  end
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function CreateSpecialNamespace(FolderID: integer): TNamespace;
{ Creates a TNamespace based on the SpecialFolders defined by                   }
{ SHGetSpecialFolderLocation.                                                   }
var
  PIDL: PItemIDList;
  F: IShellFolder;
begin
  Result := nil;
  if SHGetspecialFolderLocation(0, FolderID, PIDL) = S_OK then
   if Assigned(PIDL) then
    begin
      Result := TNamespace.Create(PIDL, nil);
      F := Result.ParentShellFolder // just force the namespace to have Parent
    end
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function DefaultSystemImageForFileExt(FileExt: WideString): Integer;
// FileExt is the file extension in this format '*.xxx'
var
  FileInfoA: TSHFileInfoA;
  FileInfoW: TSHFileInfoW;
  Attrib, Flags: DWORD;
begin
  if FileExt <> '' then
  begin
    Attrib := FILE_ATTRIBUTE_NORMAL;
    Flags := SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX;
    if IsUnicode then
    begin
      FillChar(FileInfoW, SizeOf(FileInfoW), #0);
      SHGetFileInfoW_MP(PWideChar(FileExt), Attrib, FileInfoW, SizeOf(TSHFileInfoW), Flags);
      Result := FileInfoW.iIcon;
    end else
    begin
      FillChar(FileInfoA, SizeOf(FileInfoA), #0);
      SHGetFileInfoA(PAnsiChar( AnsiString(FileExt)), Attrib, FileInfoA, SizeOf(TSHFileInfoA), Flags);
      Result := FileInfoA.iIcon;
    end
  end else
    Result := DefaultSystemImageIndex(diUnknownFile)
end;

{ ----------------------------------------------------------------------------- }
function DefaultSystemImageIndex(FolderType: TDefaultFolderIcon): integer;

{ Extracts the default Icon for the given folder type passed to it.             }

var
  FileInfoA: TSHFileInfoA;
  FileInfoW: TSHFileInfoW;
  FileExampleW: WideString;
  FileExampleA: AnsiString;
  Attrib, Flags: DWORD;
{  PIDL: PItemIDList;
  NS: TNamespace;   }
begin
  Result := -1;
  Attrib := 0;
  Flags := 0;
  case FolderType of
    diNormalFolder:
      begin
        FileExampleW := '*.*';
        Attrib := FILE_ATTRIBUTE_DIRECTORY;
        Flags := SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX
      end;
    diOpenFolder:
      begin
        FileExampleW := '*.*';
        Attrib := FILE_ATTRIBUTE_DIRECTORY;
        Flags := SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX or SHGFI_OPENICON
      end;
    diUnknownFile:
      begin
        FileExampleW := '*.zyxwv';
        Attrib := FILE_ATTRIBUTE_NORMAL;
        Flags := SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE or SHGFI_SYSICONINDEX
      end;
    diLink:
      begin
        FileExampleW := '';
        Result := SHORTCUT_ICON_INDEX;
      end;
    diMyDocuments:
      begin
        if Assigned(DesktopFolder) then
        begin
          Result := MyDocumentsFolder.GetIconIndex(False, icSmall, True);
      (*    PIDL := DesktopFolder.ParseDisplayName('::{450d8fba-ad25-11d0-98a8-0800361b1103}');
          if Assigned(PIDL) then
          begin
            NS := TNamespace.Create(PIDL, nil);
            Result := NS.GetIconIndex(False, icSmall, True);
            NS.Free
          end   *)
        end else
          Result := DefaultSystemImageIndex(diNormalFolder)
      end
  else
    FileExampleW := ''
  end;
  if FileExampleW <> '' then
  begin
    if IsUnicode then
    begin
      FillChar(FileInfoW, SizeOf(FileInfoW), #0);
      SHGetFileInfoW_MP(PWideChar(FileExampleW), Attrib, FileInfoW, SizeOf(TSHFileInfoW), Flags);
      Result := FileInfoW.iIcon;
    end else
    begin
      FileExampleA := FileExampleW;
      FillChar(FileInfoA, SizeOf(FileInfoA), #0);
      SHGetFileInfoA(PAnsiChar(FileExampleA), Attrib, FileInfoA, SizeOf(TSHFileInfoA), Flags);
      Result := FileInfoA.iIcon;
    end
  end
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
// Forces the correct icons for the Common Program Groups on Windows NT 4.0.
// Borrowed from John T and GXExplorer <g>
function FileIconInit(FullInit: BOOL): BOOL; stdcall;
type
  TFileIconInit = function(FullInit: BOOL): BOOL; stdcall;
var
  ShellDLL: HMODULE;
  PFileIconInit: TFileIconInit;
begin
  Result := False;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    ShellDLL := GetModuleHandleA(PAnsiChar( AnsiString(Shell32)));
    PFileIconInit := GetProcAddress(ShellDLL, PAnsiChar(660));
    if (Assigned(PFileIconInit)) then
      Result := PFileIconInit(FullInit);
  end;
end;
{ ----------------------------------------------------------------------------- }

// IShellLink (ShortCut) helpers
{ ----------------------------------------------------------------------------- }
function CreateShellLink(ALnkFilePath, ATargetFilePath: WideString; AnArguments: WideString = '';
 AWorkingDir: WideString = ''; ADescription: WideString = ''; AShowCmd: TCmdShow = swShowNormal;
 AHotKey: Word = 0; AHotKeyModifier: THotKeyModifiers = []; AnIconLocation: WideString = '';
 AnIconIndex: integer = 0): Boolean;
var
  ShellLink: TVirtualShellLink;
begin
  Result := True;
  ShellLink := TVirtualShellLink.Create(nil);
  if Assigned(ShellLink) then
  try
    try
      ShellLink.FileName := ALnkFilePath;
      ShellLink.TargetPath := ATargetFilePath;
      if AnArguments <> '' then
        ShellLink.Arguments := AnArguments;
      if AWorkingDir <> '' then
        ShellLink.WorkingDirectory := AWorkingDir;
      if ADescription <> '' then
        ShellLink.Description := ADescription;
      if AShowCmd <> swShowNormal then
        ShellLink.ShowCmd := AShowCmd;
      if (AHotKey <> 0) then
        ShellLink.HotKey := AHotKey;
      if AHotKeyModifier <> [] then
        ShellLink.HotKeyModifiers := AHotKeyModifier;
      if AnIconLocation <> '' then
        ShellLink.IconLocation := AnIconLocation;
      if AnIconIndex <> 0 then
        ShellLink.IconIndex := AnIconIndex;
      ShellLink.WriteLink(ShellLink.FileName);
    except
      Result := False;
      raise;
    end
  finally
    ShellLink.Free
  end
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function HotKeyModifiersToStr(HotKeyMod: THotKeyModifiers): WideString;
begin
  Result := '';
  if hkmAlt in HotKeyMod then
    Result := Result + ' Alt';
  if hkmControl in HotKeyMod then
    Result := Result + ' Control';
  if hkmExtendedKey in HotKeyMod then
    Result := Result + ' ExtendedKey';
  if hkmShift in HotKeyMod then
    Result := Result + ' Shift';
end;
{ ----------------------------------------------------------------------------- }

{ Some Stuff D4 lacks.
                                                }
{$ifdef COMPILER_4}
{ ----------------------------------------------------------------------------- }
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;
{ ----------------------------------------------------------------------------- }

{ ----------------------------------------------------------------------------- }
function Supports(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(Intf, Inst) = 0);
end;
{ ----------------------------------------------------------------------------- }
{$endif}


////////////////////////////////////////////////////////////////////////////////
// Local Functions
////////////////////////////////////////////////////////////////////////////////

procedure PIDLQuickSort(PIDLList: TPIDLArray; const ParentFolder: IShellFolder; L, R: Integer);
var
  I, J: Integer;
  P, T: PItemIDList;
  CompareResult: HResult;
begin
  if L < R then
  repeat
    I := L;
    J := R;
    P := PIDLList[(L + R) shr 1];
    repeat
      CompareResult := ParentFolder.CompareIDs(0, PIDLList[I], P);
      while ShortInt(HResultCode(CompareResult)) < 0 do
      begin
        if not Succeeded(CompareResult) or (I > Length(PIDLList) - 1) then
          CompareResult := 0
        else begin
          Inc(I);
          CompareResult := ParentFolder.CompareIDs(0, PIDLList[I], P);
        end
      end;

      CompareResult := ParentFolder.CompareIDs(0, PIDLList[J], P);
      while ShortInt(HResultCode(CompareResult)) > 0 do
      begin
        if not Succeeded(CompareResult) or (J < L) then
          CompareResult := 0
        else begin
          Dec(J);
          CompareResult := ParentFolder.CompareIDs(0, PIDLList[J], P)
        end
      end;

      if I > Length(PIDLList) - 1 then
        I := Length(PIDLList) - 1;
      if J < L then
        J := L;

      if I <= J then
      begin
        T := PIDLList[I];
        PIDLList[I] := PIDLList[J];
        PIDLList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      PIDLQuickSort(PIDLList, ParentFolder, L, J);
    L := I;
  until I >= R;
end;


procedure PIDLListQuickSort(PIDLList: TCommonPIDLList; const ParentFolder: IShellFolder; L, R: Integer; SortColumn: Integer = 0);
//
// Ensure this stays ThreadSafe
//
var
  I, J: Integer;
  P, T: PItemIDList;
  CompareResult: HResult;
begin
  if L < R then
  repeat
    I := L;
    J := R;
    P := PIDLList[(L + R) shr 1];
    repeat
      CompareResult := ParentFolder.CompareIDs(SortColumn, PIDLList[I], P);
      while ShortInt(HResultCode(CompareResult)) < 0 do
      begin
        if not Succeeded(CompareResult) or (I > PIDLList.Count - 1) then
          CompareResult := 0
        else begin
          Inc(I);
          CompareResult := ParentFolder.CompareIDs(SortColumn, PIDLList[I], P);
        end
      end;

      CompareResult := ParentFolder.CompareIDs(SortColumn, PIDLList[J], P);
      while ShortInt(HResultCode(CompareResult)) > 0 do
      begin
        if not Succeeded(CompareResult) or (J < L) then
          CompareResult := 0
        else begin
          Dec(J);
          CompareResult := ParentFolder.CompareIDs(SortColumn, PIDLList[J], P);
        end
      end;

      if I > PIDLList.Count - 1 then
        I := PIDLList.Count - 1;
      if J < L then
        J := L;

      if I <= J then
      begin
        T := PIDLList[I];
        PIDLList[I] := PIDLList[J];
        PIDLList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      PIDLListQuickSort(PIDLList, ParentFolder, L, J, SortColumn);
    L := I;
  until I >= R;
end;


{ ----------------------------------------------------------------------------- }
function RequestedDragEffect(grfKeyState: integer): HResult;
{ Looks at the KeyState during a IDragTarget notification. The return value}
{ is the expected behavior common in Windows. Note this does not mean that }
{ the DragSource is actually capable of this action.                       }
begin
  // Strip off the mouse button information keep only Ctrl and Shift information
  grfKeyState := grfKeyState and (MK_CONTROL or MK_SHIFT);
  // Standard Windows Shell behavior
  if grfKeyState = 0 then Result := DROPEFFECT_MOVE  // Windows default
  else
  if grfKeyState = MK_CONTROL then Result := DROPEFFECT_COPY
  else
  if grfKeyState = (MK_CONTROL or MK_SHIFT) then Result := DROPEFFECT_LINK
  else
  Result := DROPEFFECT_NONE;
end;
{ ----------------------------------------------------------------------------- }


{ ----------------------------------------------------------------------------- }
{ Thank you Angus Johnson for this article in UNDO          }
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
//Structures used in GetDiskFreeSpaceFAT32
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
type
  //DeviceIoControl registers structure...
  TDevIoCtl_Registers = packed record
    Reg_EBX   : DWord;
    Reg_EDX   : DWord;
    Reg_ECX   : DWord;
    Reg_EAX   : DWord;
    Reg_EDI   : DWord;
    Reg_ESI   : DWord;
    Reg_Flags : DWord;
  end;

  //Structure passed in Get_ExtFreeSpace ...
  TExtGetDskFreSpcStruc = packed record
    ExtFree_Size                      : Word;
    ExtFree_Level                     : Word;
    ExtFree_SectorsPerCluster         : Integer;
    ExtFree_BytesPerSector            : Integer;
    ExtFree_AvailableClusters         : Integer;
    ExtFree_TotalClusters             : Integer;
    ExtFree_AvailablePhysSectors      : Integer;
    ExtFree_TotalPhysSectors          : Integer;
    ExtFree_AvailableAllocationUnits  : Integer;
    ExtFree_TotalAllocationUnits      : Integer;
    ExtFree_Rsvd                      : array [0..1] of Integer;
  end;

{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
//Angus Johnson's Delphi implimentation of - Int 21h Function 7303h Get_ExtFreeSpace (FAT32)
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}
function GetDiskFreeSpaceFAT32(Drive: PAnsiChar; var SectorsperCluster,
  BytesperSector, FreeClusters, TotalClusters: DWORD): boolean;
const
  VWIN32_DIOC_DOS_IOCTL = 6;
var
  DevIoHandle         : THandle;
  BytesReturned       : DWord;
  Reg                 : TDevIoCtl_Registers;
  ExtGetDskFreSpcStruc: TExtGetDskFreSpcStruc;
begin
  Result := false;
  FillChar(ExtGetDskFreSpcStruc, sizeof(TExtGetDskFreSpcStruc),0);
  FillChar(Reg, sizeof(TDevIoCtl_Registers),0);
  with Reg do begin
    reg_EAX :=  $7303;
    reg_EDX := DWord(Drive); //DS:DX
    Reg_EDI := DWord(@ExtGetDskFreSpcStruc); //ES:DI
    reg_ECX := sizeof(TExtGetDskFreSpcStruc);
    reg_Flags := 1; //set carry flag to assume error.
  end;

  if IsUnicode then
    DevIoHandle := CreateFileW_MP( '\\.\vwin32', Generic_Read,
      File_Share_Read or File_Share_Write, nil, Open_Existing, File_Attribute_Normal, 0)
  else
    DevIoHandle := CreateFile( '\\.\vwin32', Generic_Read,
      File_Share_Read or File_Share_Write, nil, Open_Existing, File_Attribute_Normal, 0);

  if DevIoHandle <> Invalid_Handle_Value then begin
    result := DeviceIoControl(DevIoHandle, VWIN32_DIOC_DOS_IOCTL,
        @Reg, SizeOf(Reg), @Reg, SizeOf(Reg), BytesReturned, nil);
    CloseHandle(DevIoHandle);
    if not result then
    begin
      exit //error
    end
    else if (Reg.reg_Flags and 1 <> 0) then begin
      result := false; //If carry flag not cleared then => error
      exit;
      end
    else with ExtGetDskFreSpcStruc do begin
      BytesperSector := ExtFree_BytesPerSector;
      SectorsperCluster := ExtFree_SectorsPerCluster;
      TotalClusters := ExtFree_TotalClusters;
      FreeClusters :=  ExtFree_AvailableClusters;
      result := true;
    end;
  end;
end; {GetDiskFreeSpaceFAT32}
{ ----------------------------------------------------------------------------- }

function GetDiskFreeSpaceMP(Drive: PWideChar; var SectorsperCluster,
    BytesperSector, FreeClusters, TotalClusters: DWORD): boolean;
begin
  if Assigned(GetDiskFreeSpaceW_MP) then
    Result := GetDiskFreeSpaceW_MP(
      PWideChar( Drive), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters)
  else
  if not IsWin95_SR1 then
    Result := GetDiskFreeSpaceFAT32(
      PAnsiChar( AnsiString(Drive)), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters)
  else
    Result := GetDiskFreeSpaceA(
      PAnsiChar( AnsiString(Drive)), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters);
end;

function DriveSize(Drive: PWideChar): Int64;
var
  SectorsperCluster, BytesperSector, FreeClusters, TotalClusters: DWORD;
begin
  Result := 0;
  if GetDiskFreeSpaceMP(Drive, SectorsperCluster, BytesperSector, FreeClusters, TotalClusters) then
      Result := Int64(BytesperSector) * Int64(SectorsperCluster) * Int64(TotalClusters)
end;

function DriveSize(Drive: PWideChar; ByteSize: TBtyeSize): WideString;
begin
  case ByteSize of
    bsKiloBytes: Result := Format('%0.0n '+ 'KB', [DriveSize(Drive) / 1024]);
    bsMegaBytes: Result := Format('%0.0n '+ 'MB', [DriveSize(Drive) / 1048576]);
    bsGigiBytes: Result := Format('%0.1n '+ 'GB', [DriveSize(Drive) / 1073741824]);
    bsTereBytes: Result := Format('%0.1n '+ 'TB', [DriveSize(Drive) / 1099511627776]);
  end
end;

function DriveFreeSpace(Drive: PWideChar): Int64;
var
  SectorsperCluster, BytesperSector, FreeClusters, TotalClusters: DWORD;
begin
  Result := 0;
  if GetDiskFreeSpaceMP(Drive, SectorsperCluster, BytesperSector, FreeClusters, TotalClusters) then
      Result := Int64(BytesperSector) * Int64(SectorsperCluster) * Int64(FreeClusters)
end;

function DriveFreeSpace(Drive: PWideChar; ByteSize: TBtyeSize): WideString; 
begin
  case ByteSize of
    bsKiloBytes: Result := Format('%0.0n '+ 'KB', [DriveFreeSpace(Drive) / 1024]);
    bsMegaBytes: Result := Format('%0.0n '+ 'MB', [DriveFreeSpace(Drive) / 1048576]);
    bsGigiBytes: Result := Format('%0.1n '+ 'GB', [DriveFreeSpace(Drive) / 1073741824]);
    bsTereBytes: Result := Format('%0.1n '+ 'TB', [DriveFreeSpace(Drive) / 1099511627776]);
  end
end;


{ ----------------------------------------------------------------------------- }
function LoadShell32Functions: Boolean;
var
  Shell32DLL, ShlWapiDLL, User32DLL: HMODULE;
begin
  if not ShellFunctionsLoaded then
  begin
    Result := False;
    { Don't see a point in making this all WideString compatible }
    Shell32DLL := GetModuleHandleA(PAnsiChar( AnsiString(Shell32)));
    if Shell32DLL <> 0 then
    begin
      SHGetKnownFolderIDList_MP := GetProcAddress(Shell32DLL, 'SHGetKnownFolderIDList');
      ShellILIsEqual := GetProcAddress(Shell32DLL, PAnsiChar(21));
      ShellILIsParent := GetProcAddress(Shell32DLL, PAnsiChar(23));
      SHLimitInputEdit := GetProcAddress(Shell32DLL, PAnsiChar(747));
      MP_SHSetInstanceExplorer := GetProcAddress(Shell32DLL, 'SHSetInstanceExplorer');
      MP_SHGetInstanceExplorer := GetProcAddress(Shell32DLL, 'SHGetInstanceExplorer');
      Result := Assigned(ShellILIsEqual) and Assigned(ShellILIsParent)
    end;
    ShlWapiDLL := GetModuleHandleA(PAnsiChar( AnsiString(shlwapi)));
    if ShlWapiDLL <> 0 then
    begin
      MP_SHSetThreadRef := GetProcAddress(ShlWapiDLL, 'SHSetThreadRef');
      MP_SHGetThreadRef := GetProcAddress(ShlWapiDLL, 'SHGetThreadRef');
      MP_SHCreateThread := GetProcAddress(ShlWapiDLL, 'SHCreateThread');
      MP_SHCreateThreadRef := GetProcAddress(ShlWapiDLL, 'SHCreateThreadRef');
      MP_SHReleaseThreadRef := GetProcAddress(ShlWapiDLL, 'SHReleaseThreadRef');
    end;
    User32DLL := GetModuleHandle('user32');
    if User32DLL <> 0 then
    begin
      AnimateWindow := GetProcAddress(User32DLL, 'AnimateWindow');
    end;
    MprDLL := CommonLoadLibrary(AnsiString(Mpr));
    if MprDLL <> 0 then
    begin
      MP_WNetGetResourceInformationA := GetProcAddress(MprDLL, 'WNetGetResourceInformationA');
      MP_WNetGetResourceInformationW := GetProcAddress(MprDLL, 'WNetGetResourceInformationW');
    end;
    ShellFunctionsLoaded := True;
  end else
    Result := True
end;
{ ----------------------------------------------------------------------------- }


{ TNamespace }

function TNamespace.CanCopyAll(NamespaceArray: TNamespaceArray): Boolean;
var
  i: integer;
begin
  if Assigned(NamespaceArray) then
  begin
    Result := True;
    i := 0;
    while Result and (i < Length(NamespaceArray)) do
    begin
      Result := NamespaceArray[i].CanCopy;
      Inc(i)
    end
  end else
    Result := False
end;

function TNamespace.CanCutAll(NamespaceArray: TNamespaceArray): boolean;
begin
  Result := CanDeleteAll(NamespaceArray)
end;

function TNamespace.CanDeleteAll(NamespaceArray: TNamespaceArray): Boolean;
var
  i: integer;
begin
  if Assigned(NamespaceArray) then
  begin
    Result := True;
    i := 0;
    while Result and (i < Length(NamespaceArray)) do
    begin
      Result := NamespaceArray[i].CanDelete;
      Inc(i)
    end
  end else
    Result := False
end;

function TNamespace.CanPasteToAll(NamespaceArray: TNamespaceArray): Boolean;
begin
  Result := False;
  if Assigned(NamespaceArray) then
    if Length(NamespaceArray) > 0 then
      Result := True  // Can try to paste to anything?
end;

function TNamespace.CanRenameAll(NamespaceArray: TNamespaceArray): Boolean;
var
  i: integer;
begin
  if Assigned(NamespaceArray) then
  begin
    Result := True;
    i := 0;
    while Result and (i < Length(NamespaceArray)) do
    begin
      Result := NamespaceArray[i].CanRename;
      Inc(i)
    end
  end else
    Result := False
end;

function TNamespace.CanShowPropertiesOfAll(NamespaceArray: TNamespaceArray): Boolean;
var
  i: integer;
begin
  if Assigned(NamespaceArray) then
  begin
    Result := True;
    i := 0;
    while Result and (i < Length(NamespaceArray)) do
    begin
      Result := NamespaceArray[i].HasPropSheet;
      Inc(i)
    end
  end else
    Result := False
end;

function TNamespace.Clone(ReleasePIDLOnDestroy: Boolean): TNameSpace;
begin
// This is not really a true clone since we don't copy the parent, but it is
// dangerous to do that.  Be careful using this function since things can
// potentially change in the shell.
  Result := TNamespace.Create(PIDLMgr.CopyPIDL(AbsolutePIDL), nil);
  Result.FreePIDLOnDestroy := ReleasePIDLOnDestroy;
end;

function TNamespace.ComparePIDL(PIDLToCompare: PItemIDList;
  IsAbsolutePIDL: Boolean; Column: Integer = 0): ShortInt;
// Encapsulation of the CompareID Function of IShellFolder
// Returns    > 0 if PIDLToCompare > RelativePIDL
//            0 if PIDLToCompare = RelativePIDL
//            < 0 if PIDLToCompare < RelativePIDL
var
  PIDL: PItemIDList;
  CompareResult: HRESULT;
  Folder: IShellFolder;
begin
  if Assigned(PIDLToCompare) then
  begin
    if Column < 0 then
      Column := 0;

    if PIDLMgr.IsDesktopFolder(PIDLToCompare) and IsDesktop then
      Result := 0
    else begin
      Folder := Parent.ShellFolder;
      if Assigned(Folder) then
      begin
        if IsAbsolutePIDL then
        begin
          Result := -1;
          PIDL := PIDLMgr.GetPointerToLastID(PIDLToCompare);
          // First test is if the PIDL length is the same
          if PIDLMgr.IDCount(PIDLToCompare) = PIDLMgr.IDCount(AbsolutePIDL) then
          begin
            if Assigned(Parent) then
            begin
              // Desktop items won't have a valid parent
              if ILIsParent(Parent.AbsolutePIDL, PIDLToCompare, True) then
              begin
                CompareResult := Folder.CompareIDs(Column, PIDL, RelativePIDL);
                if Succeeded(CompareResult) then
                  Result := ShortInt(HResultCode(CompareResult))
                else
                  Result := 0;
              end
            end else
            begin
              CompareResult := Folder.CompareIDs(Column, PIDL, RelativePIDL);
              if Succeeded(CompareResult) then
                Result := ShortInt(HResultCode(CompareResult))
              else
                Result := 0
            end
          end
        end else
        begin
          CompareResult := Folder.CompareIDs(Column, PIDLToCompare, RelativePIDL);
          if Succeeded(CompareResult) then
            Result := ShortInt(HResultCode(CompareResult))
          else
            Result := 0
        end
      end else
        Result := 0
    end
  end else
    Result := -1 // If the pidl is not assigned then we clearly are greater!
end;

function TNamespace.ContextMenuItemHelp(MenuItemID: LongWord): WideString;
const
  BufferLen = 128;
var
  S: AnsiString;
  Found: Boolean;
  P: Pointer;
begin
  Found := False;
  if Assigned(CurrentContextMenu) and (MenuItemID <> $FFFFFFFF) and (MenuItemID > 0)then
  begin
    if IsUnicode then
    begin
      SetLength(Result, BufferLen);
      { Keep D6 from complaining about suspicious PChar cast }
      P := @Result[1];
      Found := CurrentContextMenu.GetCommandString(MenuItemID-1, GCS_HELPTEXTW, nil, PAnsiChar(P),
        BufferLen) = NOERROR
    end;
    if not Found then
    begin
      SetLength(S, BufferLen);
      if CurrentContextMenu.GetCommandString(MenuItemID-1, GCS_HELPTEXTA, nil, PAnsiChar(S),
        BufferLen) <> NOERROR
      then
        Result := ''
      else begin
        SetLength(S, StrLen( PAnsiChar(S)));
        Result := S
      end
    end else
      SetLength(Result,  lstrlenW(PWideChar( Result)))
  end;
end;

function TNamespace.ContextMenuVerb(MenuItemID: Longword): WideString;
{ Returns the cononical (or not) verb that is equal to the MenuItemID, which is }
{ the HMenu identifer for a menu item.                                          }
const
  BufferLen = 128;
var
  S: AnsiString;
  Found: Boolean;
  P: Pointer;
begin
  Found := False;
  if Assigned(CurrentContextMenu) and (MenuItemID <> $FFFFFFFF) and (MenuItemID > 0) then
  begin
    if IsUnicode then
    begin
      SetLength(Result, BufferLen);
     { Keep D6 from complaining about suspicious PChar cast }
      P := @Result[1];
      Found := CurrentContextMenu.GetCommandString(MenuItemID-1, GCS_VERBW, nil, PAnsiChar(P),
        BufferLen) = NOERROR
    end;
    if not Found then
    begin
      SetLength(S, BufferLen);
      if CurrentContextMenu.GetCommandString(MenuItemID-1, GCS_VERBA, nil, PAnsiChar(S),
        BufferLen) <> NOERROR
      then
        Result := ''
      else begin
        SetLength(S, StrLen( PAnsiChar(S)));
        Result := S
      end
    end else
      SetLength(Result,  lstrlenW(PWideChar( Result)))
  end;
end;

function TNamespace.Copy(Owner: TWinControl; NamespaceArray: TNamespaceArray): Boolean;
begin
  Result := False;
  if CanCopyAll(NamespaceArray) then
  begin
    if VerifyPIDLRelationship(NamespaceArray, True) then
      Result := ExecuteContextMenuVerb(Owner, 'copy', NamespaceToRelativePIDLArray(NamespaceArray))
    else
      ExecuteContextMenuVerbMultiPath(Owner, 'copy', NamespaceArray)
  end
end;

constructor TNamespace.Create(PIDL: PItemIdList; AParent: TNamespace);
{ Pass the PIDL of a Namespace Object Folder to create along with its parent    }
{ to create a new TNamespace.                                                   }
begin
  inherited Create;

  FParent := AParent;
  FShellCache.Data.SmallIcon := -1;
  FShellCache.Data.SmallOpenIcon := -1;
  FShellCache.Data.OverlayIndex := -1;
  FShellCache.Data.OverlayIconIndex := -1;
  IconCache := -1;
  Include(FStates, nsShellDetailsSupported);  // Be optomistic
  Include(FStates, nsShellFolder2Supported);  // Be optomistic
  Include(FStates, nsShellOverlaySupported);  // Be optomistic
  FreePIDLOnDestroy := True;
  FNamespaceID := SHELL_NAMESPACE_ID;
  { It is the Root Folder since it has no parent }
  if not Assigned(AParent) then
  begin
    { Either a nil for PID or if the PID is the Desktop PIDL means a full tree }
    if not Assigned(PIDL) or PIDLMgr.IsDesktopFolder(PIDL) then
    begin
      { If PID is already assigned then use it }
      if not Assigned(PIDL) then
        SHGetSpecialFolderLocation(ParentWnd, CSIDL_DESKTOP, FRelativePIDL)
      else
        FRelativePIDL := PIDL;
      FAbsolutePIDL := FRelativePIDL;
    end else
    { The PIDL is the Root PIDL but is NOT the Desktop namespace  it is a }
    { FULLY QUALIFIED PIDL to a namespace that is to be the Root.         }
    begin
      FAbsolutePIDL := PIDL;
      FRelativePIDL := PIDLMgr.GetPointerToLastID(FAbsolutePIDL);
    end;
  end else
  { If the folder is a child of the desktop special conditions apply see above }
  if PIDLMgr.IsDesktopFolder(AParent.AbsolutePIDL) then
  begin
    FRelativePIDL := PIDL;
    FAbsolutePIDL := PIDL;
  end else
  { Normal building of the PIDLs and Shells }
  begin
    FAbsolutePIDL := PIDLMgr.AppendPIDL(AParent.FAbsolutePIDL, PIDL);
    FRelativePIDL := PIDLMgr.GetPointerToLastID(FAbsolutePIDL);
    PIDLMgr.FreePIDL(PIDL);
  end;
end;

constructor TNamespace.CreateCustomNamespace(CustomID: Integer; AParent: TNamespace);
begin
  FShellCache.Data.SmallIcon := -1;
  FShellCache.Data.SmallOpenIcon := -1;
  IconCache := -1;
  Exclude(FStates, nsShellDetailsSupported);
  Exclude(FStates, nsShellFolder2Supported);
  FreePIDLOnDestroy := False;
  FNamespaceID := CustomID;
  FParent := AParent
end;

constructor TNamespace.CreateFromFileName(FileName: WideString);
var
  PIDL: PItemIDList;
begin
  PIDL := PathToPIDL(FileName);
  if Assigned(PIDL) then
    Create(PIDL, nil)
  else
    // This will be called often with the autocomplete component while debugging
    // in the IDE
    // To turn off exception break go to Tools>Debugger Options>Add and type in
    //  "EVSTInvalidFileName" without the quotes.  Make sure that is is checked.
    // This will keep delphi from breaking on this exception
    raise EVSTInvalidFileName.Create('Trying to create a TNamespace on a non existant File object');
end;

function TNamespace.Cut(Owner: TWinControl; NamespaceArray: TNamespaceArray): Boolean;
begin
  Result := False;
  if CanCutAll(NamespaceArray) then
  begin
    if VerifyPIDLRelationship(NamespaceArray, True) then
      Result := ExecuteContextMenuVerb(Owner, 'cut', NamespaceToRelativePIDLArray(NamespaceArray))
    else
      ExecuteContextMenuVerbMultiPath(Owner, 'cut', NamespaceArray)
  end
end;

function TNamespace.DataObjectMulti(NamespaceArray: TNamespaceArray): IDataObject;
var
  NSList: TList;
begin
  Result := nil;
  if VerifyPIDLRelationship(NamespaceArray, True) then
    Result := InternalGetDataObjectInterface(NamespaceToRelativePIDLArray(NamespaceArray))
  else begin
    NSList := NamespaceToNamespaceList(NamespaceArray);
    CreateFullyQualifiedShellDataObject(NSList, False, Result);
    FreeAndNil(NSList)
  end
end;

function TNamespace.DelegateNamespace: Boolean;
begin
  Result := IsWin7 and FileSystem and IsLibraryChild
end;

function TNamespace.Delete(Owner: TWinControl; NamespaceArray: TNamespaceArray; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean;
begin
  Result := False;
  if CanDeleteAll(NamespaceArray) then
  begin
    if VerifyPIDLRelationship(NamespaceArray, True) then
      Result := ExecuteContextMenuVerb(Owner, 'delete', NamespaceToRelativePIDLArray(NamespaceArray), ShiftKeyState)
    else
      ExecuteContextMenuVerbMultiPath(Owner, 'delete', NamespaceArray, ShiftKeyState)
  end
end;

destructor TNamespace.Destroy;
begin
  // Remember RelativePIDL points to end of AbsolutePIDL so only 1 actual PIDL.
  if FreePIDLOnDestroy and Assigned(PIDLMgr) then
    PIDLMgr.FreePIDL(FAbsolutePIDL);
  if IsUnicode then
  begin
    if Assigned(FWin32FindDataW) then
      FreeMem(FWin32FindDataW, SizeOf(TWin32FindDataW));
  end else
    if Assigned(FWin32FindDataA) then
      FreeMem(FWin32FindDataA, SizeOf(TWin32FindDataA));
  begin
  end;
  if Assigned(FSHGetFileInfoRec) then
  begin
    Finalize(FSHGetFileInfoRec^);
    FreeMem(FSHGetFileInfoRec, SizeOf(TSHGetFileInfoRec));
  end;
  FreeAndNil(FExtractImage);
  FreeAndNil(FShellLink);
  FShellFolder := nil;
  FreeAndNIL(FImage);
  FreeAndNil(FDelagateNS);

  inherited;
  if (nsOwnsParent in States) then
    FreeAndNil(FParent);
end;

function TNamespace.DetailsAlignment(ColumnIndex: Integer): TAlignment;
{ Returns the Text that is in the Header of the Explorer Listview based on what }
{ the folder in the Treeview is displaying.  Only implemented partially on      }
{ different versions of Windows.  It was undocumented until about Win98.        }
{ Win2k implements this using IShellFolder2                                     }
{ Be careful of the reference point using DetailsXXXX functions.  This method   }
{ get the header titles a folder will show for its children.                    }
var
  Details: TShellDetails;
  Found: Boolean;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsAlignment(ColumnIndex)
  else begin
    // Default
    Result := taLeftJustify;

    FillChar(Details, SizeOf(Details), #0);
    Found := False;
    if Parent.DetailsValidIndex(ColumnIndex) then
    begin
      if Assigned(ShellFolder2) then
        Found := ShellFolder2.GetDetailsOf(nil, UINT(ColumnIndex), Details) = S_OK;

      try
        // Some 3rd party NSEs don't implement IShellDetails right
        if not Found and Assigned(ShellDetailsInterface) then
          Found :=  ShellDetailsInterface.GetDetailsOf(nil, UINT(ColumnIndex), Details) = S_OK;
      except
        Found := False;
      end;

      if Found then
      begin
        case Details.Fmt of
          LVCFMT_CENTER: Result := taCenter;
          LVCFMT_LEFT:   Result := taLeftJustify;
          LVCFMT_RIGHT:  Result := taRightJustify;
  //        LVCFMT_COL_HAS_IMAGES: Result := tiContainsImage
        end;
        if (Details.str.uType = STRRET_WSTR) and Assigned(Details.str.pOleStr) then
          PIDLMgr.FreeOLEStr(Details.str.pOLEStr);
      end
    end
  end
end;

function TNamespace.DetailsColumnTitle(ColumnIndex: integer): WideString;
{ Returns the Text that is in the Header of the Explorer Listview based on what }
{ the folder in the Treeview is displaying.  Only implemented partially on      }
{ different versions of Windows.  It was undocumented until about Win98.        }
{ Win2k implements this using IShellFolder2                                     }

{ Be careful of the reference point using DetailsXXXX functions.  This method   }
{ gets the header titles a folder will show for its children.                   }
var
  Details: TShellDetails;
  Found: Boolean;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsColumnTitle(ColumnIndex)
  else begin
    FillChar(Details, SizeOf(Details), #0);
    Found := False;
    if DetailsValidIndex(ColumnIndex) then
    begin
      if Assigned(ShellFolder2) then
        Found := ShellFolder2.GetDetailsOf(nil, UINT(ColumnIndex), Details) = S_OK;

      try
        // Some 3rd party NSEs don't implement IShellDetails right
        if not Found and Assigned(ShellDetailsInterface) then
          Found :=  ShellDetailsInterface.GetDetailsOf(nil, UINT(ColumnIndex), Details) = S_OK;
      except
        Found := False;
      end;
      
      if Found then
        Result := StrRetToStr(Details.str, RelativePIDL)
      else
        Result := DetailsDefaultColumnTitle(ColumnIndex)
    end else
      Result := ''
  end
end;

function TNamespace.DetailsDefaultColumnTitle(ColumnIndex: integer): WideString;
{ If IShellDetails is not implemented then these are returned for the Header    }
{ text as a default.  Can be overridden.                                        }
begin
  case ColumnIndex of
   -1, 0:  Result := STR_COLUMN_NAMES[0];
    1:  Result := STR_COLUMN_NAMES[1];
    2:  Result := STR_COLUMN_NAMES[2];
    3:  Result := STR_COLUMN_NAMES[3];
    4:  Result := STR_COLUMN_NAMES[4];
    5:  Result := STR_COLUMN_NAMES[5];
    6:  Result := STR_COLUMN_NAMES[6];
    7:  Result := STR_COLUMN_NAMES[7];
    8:  Result := STR_COLUMN_NAMES[8];
    9:  Result := STR_COLUMN_NAMES[9];
  end;
end;

function TNamespace.DetailsDefaultOf(ColumnIndex: integer): WideString;
{ If IShellDetail is not implemented the call to DetailsOf calls this and       }
{ returns what it can to mimic the values in columns for a plain file, Name,    }
{ size, type, date, attributes.                                                 }
var
  IsSystemFolder: Boolean;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsDefaultOf(ColumnIndex)
  else begin
    Result := '';
    if IsUnicode then
    begin
      if not Assigned(FWin32FindDataW) then
        GetDataFromIDList;
      if Assigned(FWin32FindDataW) then
        { This is totally undocumented. It works on Win98 will test on NT 4 soon }
        { Not a valid file so it has no size.  #8 appears to mean "System File" }
        IsSystemFolder := not ((FWin32FindDataW^.cFileName[0] = WideChar(#8)) or
                              (FWin32FindDataW^.cFileName[0] = WideChar(#0)) or
                               not FileSystem)
      else
        IsSystemFolder := False;
    end else
    begin
      if not Assigned(FWin32FindDataA) then
        GetDataFromIDList;
      if Assigned(FWin32FindDataA) then
        { This is totally undocumented. It works on Win98 will test on NT 4 soon }
        { Not a valid file so it has no size.  #8 appears to mean "System File" }
        IsSystemFolder := not ((FWin32FindDataA^.cFileName[0] = #8) or
                              (FWin32FindDataA^.cFileName[0] = #0) or
                               not FileSystem)
      else
        IsSystemFolder := False;
    end;
    case ColumnIndex of
      -1, 0:  Result := NameInFolder;
      1:  Result := SizeOfFileKB;
      2:  if IsSystemFolder then
            Result := FileType
          else
            Result := STR_SYSTEMFOLDER;
      3:  Result := LastWriteTime;
      4:  Result := AttributesString;
    else
      Result := ''
    end;
  end
end;

function TNamespace.DetailsDefaultSupportedColumns: integer;
{ If IShellDetail is not implemented the call to SupportedColumns calls this    }
{ and returns 5.  It mimics the titles in the header for a plain file, Name,    }
{ size, type, date, attributes.                                                 }
begin
  Result := DefaultDetailColumns;
end;

function TNamespace.DetailsGetDefaultColumnState(ColumnIndex: integer): TSHColumnStates;
{ Be careful of the reference point using DetailsXXXX functions.  This function }
{ gets the GetDefaultColumnState of the folder for its children if it exposes   }
{ IShellFolder2.  If it does not it returns csOnByDefault so it will be shown   }
var
  Flags: Longword;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsGetDefaultColumnState(ColumnIndex)
  else begin
    Result := [];
    if Parent.DetailsValidIndex(ColumnIndex) then
    begin
      EnsureDetailCache;
      if (docStatesValid in ShellCache.Data.DetailsOfCache[ColumnIndex].Cached) then
        Result := ShellCache.Data.DetailsOfCache[ColumnIndex].States
      else begin
        if Assigned(ShellFolder2) then
        begin
          Flags := 0;
          if ShellFolder2.GetDefaultColumnState(ColumnIndex, Flags) = NOERROR then
          begin
            if SHCOLSTATE_TYPE_STR and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csTypeString);
            if SHCOLSTATE_TYPE_INT and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csTypeInt);
            if SHCOLSTATE_TYPE_DATE and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csTypeDate);
            if SHCOLSTATE_ONBYDEFAULT and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csOnByDefault);
            if SHCOLSTATE_TYPE_SLOW and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csSlow);
            if SHCOLSTATE_EXTENDED and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csExtended);
            if SHCOLSTATE_SECONDARYUI and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csSecondaryUI);
            if SHCOLSTATE_HIDDEN and Flags <> 0 then Include(ShellCache.Data.DetailsOfCache[ColumnIndex].States, csHidden);
          end else
            { Some of the old namespaces will expose ShellFolder2 but don't support    }
            { it completely.  These resort to IShellDetails so assume this is the case }
            ShellCache.Data.DetailsOfCache[ColumnIndex].States := [csOnByDefault];
        end else
        { Some of the old namespaces don't expose ShellFolder2.  These resort to  }
        { IShellDetails so assume this is the case                                }
          ShellCache.Data.DetailsOfCache[ColumnIndex].States := [csOnByDefault];

        Result := ShellCache.Data.DetailsOfCache[ColumnIndex].States;
        Include(ShellCache.Data.DetailsOfCache[ColumnIndex].Cached, docStatesValid)
      end
    end
  end
end;

function TNamespace.DetailsOf(ColumnIndex: integer): WideString;
{ Returns the text for the desired column (detail view in the listview in       }
{ Explorer) using IShellDetail or using information pulled from the namespace   }
{ by other means.                                                               }
{ Be careful of the reference point using DetailsXXXX functions.  This function }
{ gets the Details of the current namespace using its parent folder.            }
// Threading only works on Namespaces that support IShellFolder2 (WinME, Win2k and up)
var
  Details: TShellDetails;
  OldError: Integer;   
  TempCache: PDetailsOfCacheRec;
  PIDL: PItemIDList;
  NS: TNamespace;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsOf(ColumnIndex)
  else begin
    Result := '';
    { The parent is responsible for the columns }
    if Parent.DetailsValidIndex(ColumnIndex) then
    begin
      OldError := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
      try
        { Force parent namespace creation if necessary }
        if Assigned(ParentShellFolder) and not IsDesktop then
        begin
          EnsureDetailCache;

          if (docCaptionValid in ShellCache.Data.DetailsOfCache[ColumnIndex].Cached) then
             Result := ShellCache.Data.DetailsOfCache[ColumnIndex].Caption
          else begin
            TempCache := @ShellCache.Data.DetailsOfCache[ColumnIndex];
            FillChar(Details, SizeOf(Details), #0);

            if IsWin7 and IsLibraryChild and FileSystem then
            begin
              if ColumnIndex = 0 then
                TempCache^.Caption := NameInFolder
              else begin
                 PIDL := PathToPIDL(NameForParsing);
                 if Assigned(PIDL) then                       // This still won't line up the column correctly....
                 begin
                   NS := TNamespace.Create(PIDL, nil);
                   TempCache^.Caption := NS.DetailsOf(ColumnIndex);
                   NS.Free;
                 end else
                   TempCache^.Caption := DetailsOfEx(ColumnIndex);   // Last resort
              end;
              Include(TempCache^.Cached, docCaptionValid);
            end else
            begin
              if ColumnIndex = 0 then
                TempCache^.Caption := NameInFolder
              else
              if Assigned(ParentShellFolder2) then
              begin
                if ParentShellFolder2.GetDetailsOf(RelativePIDL, UINT(ColumnIndex), Details) <> S_OK then
                begin
                  if Assigned(ParentShellDetailsInterface) and (ParentShellDetailsInterface.GetDetailsOf(RelativePIDL, UINT(ColumnIndex), Details) = S_OK) then
                    TempCache^.Caption := StrRetToStr(Details.Str, RelativePIDL)
                  else
                    TempCache^.Caption := DetailsDefaultOf(ColumnIndex)
                end else
                  TempCache^.Caption := StrRetToStr(Details.Str, RelativePIDL);
                Include(TempCache^.Cached, docCaptionValid)
              end else
              begin
                if Assigned(ParentShellDetailsInterface) then
                begin
                  if ParentShellDetailsInterface.GetDetailsOf(RelativePIDL, UINT(ColumnIndex), Details) = S_OK then
                    TempCache^.Caption := StrRetToStr(Details.Str, RelativePIDL);
                end else
                  TempCache^.Caption := DetailsDefaultOf(ColumnIndex);
                Include(TempCache^.Cached, docCaptionValid);
              end;

              // Don't show information in My Computer about system Folders past types (XP problem)
              if Assigned(Parent) then
              begin
                if Parent.IsMyComputer and (ColumnIndex > 1) then
                  if not WideIsDrive(NameforParsing) then
                    TempCache^.Caption := '';
              end;
            end; // Not Win7
            Result := TempCache^.Caption;
          end   // Not Cached
        end    // assigned ParentFolder
      finally
        SetErrorMode(OldError);
      end
    end
  end
end;

function TNamespace.DetailsOfEx(ColumnIndex: integer): WideString;
var
  ColumnID: TSHColumnID;
  V, V2: OLEVariant;
  i: Integer;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsOfEx(ColumnIndex)
  else begin
    Result := '';
    V := Null;   
    if Assigned(ParentShellFolder2) then
    begin
      FillChar(ColumnID, SizeOf(ColumnID), #0);
      if ParentShellFolder2.MapColumnToSCID(ColumnIndex, ColumnID) = NOERROR then
      begin
        VarClear(V);
        if ParentShellFolder2.GetDetailsEx(RelativePIDL, ColumnID, V) = S_OK then
        begin
          if not(VarIsEmpty(V) or VarIsNull(V)) then
          begin
           // D4 and D5 do not have VarArrayGet need to work on this someday.. or not support D4 and D5 anymore.
            {$IFDEF COMPILER_6_UP}
            VarArrayLock(V);
            try
              for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
              begin
                V2 := VarArrayGet(V, [i]);
                Result := VarToWideStr(V2)
              end
            finally
              VarArrayUnlock(V);
            end;
            {$ENDIF}
          end else
             Result := VarToWideStr(V);
        end
      end  
    end
  end
end;

function TNamespace.DetailsSupportedColumns: integer;
{ If IShellDetail or IShellFolder2 is implemented the call to                   }
{ DetailsSupportedColumns returns total number of columns the namespace         }
{ supports.  This allows the header to change dynamiclly.                       }

{ Be careful of the reference point using DetailsXXXX functions.  This function }
{ gets number of columns this folder will display for its children.             }
const
      { ShellFolder2 is broken on WinME for "Scanners and Cameras" folders.     }
      { It goes into an infinate loop.                                          }
      { WinXP is just as broken.                                                }
  COLUMNLIMIT = 1000;  // Safely valve for namespaces that don't follow the rules
var
  Details: TShellDetails;
  Found, ShellFolder2Succeeded, ShellDetailsSucceeded: Boolean;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsSupportedColumns
  else begin
    FillChar(Details, SizeOf(Details), #0);

    if not (scSupportedColumns in ShellCache.ShellCacheFlags) then
    begin
      FShellCache.Data.SupportedColumns := 0;
      ShellFolder2Succeeded := False;
      ShellDetailsSucceeded := False;
      
      if Assigned(ShellFolder2) then
      begin
        try
          while (ShellFolder2.GetDetailsOf(nil, UINT(FShellCache.Data.SupportedColumns), Details) = S_OK) and
            (FShellCache.Data.SupportedColumns < COLUMNLIMIT) do
          begin
            if (Details.str.uType = STRRET_WSTR) and Assigned(Details.str.pOleStr) then
              PIDLMgr.FreeOLEStr(Details.str.pOLEStr);
            FillChar(Details, SizeOf(Details), #0);
            Inc(FShellCache.Data.SupportedColumns);
          end;
          ShellFolder2Succeeded := True; // It may return 0
        except
        end;

        // Error detected, the namespace does not follow the rules
        if FShellCache.Data.SupportedColumns = COLUMNLIMIT then
          FShellCache.Data.SupportedColumns := 0;

      end;

      { Some folders support both but only work right with IShellDetials          }
      { The History Folder is an example.                                         }
      Found := FShellCache.Data.SupportedColumns > 0;
      { DO NOT PASS A UNINITALIZED TSHELLDETAIL STRUCTURE TO THIS FUNCTION }
      { IT WILL CAUSE THE RESULT TO BE CORRECT BUT INTERLACED WITH GARBAGE.         }
      try
        // Some 3rd party NSEs don't implement IShellDetails right
        if not Found and Assigned(ShellDetailsInterface) then
        begin
          while ShellDetailsInterface.GetDetailsOf(nil, UINT(ShellCache.Data.SupportedColumns), Details) = S_OK do
            Inc(FShellCache.Data.SupportedColumns);

          ShellDetailsSucceeded := True
        end
      except
      end;

      // Zero is fine as long as we were told, without and error, the column count was 0
      if (ShellCache.Data.SupportedColumns = 0) and not (ShellDetailsSucceeded or ShellFolder2Succeeded) then
        FShellCache.Data.SupportedColumns := DetailsDefaultSupportedColumns;

      Include(FShellCache.ShellCacheFlags, scSupportedColumns);
    end;
    Result := ShellCache.Data.SupportedColumns
  end
end;

function TNamespace.DetailsSupportedVisibleColumns: TVisibleColumnIndexArray;
// Returns and array of currently visible columns in details mode.  Two bits of info
// are returned with this method.
// 1) The number of visible column:   Length(DetailsSupportedVisibleColumns)
// 2) The indicies of visible columns: [0, 2, 4, 6] Details index 0, 2, 4, 6 are shown\
var
  i: integer;
begin
  if DelegateNamespace then
    Result := DelagateNS.DetailsSupportedVisibleColumns
  else begin
    Result := nil;
    for i := 0 to Parent.DetailsSupportedColumns - 1 do
    begin
      if csOnByDefault in DetailsGetDefaultColumnState(i) then
      begin
        if DetailsColumnTitle(i) <> '' then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := i
        end
      end
    end;
  end
end;

function TNamespace.DetailsValidIndex(DetailsIndex: integer): Boolean;
{ Test to see if the passed index is in the range of the number of detail       }
{ columns the namespace has.                                                    }
begin
  Result := (DetailsIndex > -1) and (DetailsIndex < DetailsSupportedColumns)
end;

function TNamespace.DragEffect(grfKeyState: integer): HRESULT;
{ Looks at the KeyState during a IDragDrop notification.  The return value }
{ is the Effect that is desired by the user, using the GetDesiredDragEffect}
{ function, and what Effects are supported by the IDragSource              }

  function AvailableEffects: LongInt;
  begin
    Result := DROPEFFECT_NONE;
    if CanMove then Result := DROPEFFECT_MOVE;
    if CanCopy then Result := Result or DROPEFFECT_COPY;
    if CanLink then Result := Result or DROPEFFECT_LINK;
  end;

var
  KeyEffect: HResult;
  ValidEffects: Longword;
begin
  // See what the user is requesting by looking at the key board
  KeyEffect := RequestedDragEffect(grfKeyState);
  // What effects do the namespace support?
  ValidEffects := AvailableEffects;
  // Let the users desires prevail
  if KeyEffect and ValidEffects > 0 then Result := KeyEffect
  else  // If the users desires are undo-able pick the first effect avaiable
  if ValidEffects and DROPEFFECT_MOVE > 1 then Result := DROPEFFECT_MOVE
  else  // Windows default is MOVE so check it first
  if ValidEffects and DROPEFFECT_COPY > 1 then Result := DROPEFFECT_COPY
  else
  if ValidEffects and DROPEFFECT_LINK > 1 then Result := DROPEFFECT_LINK
  else
    Result := DROPEFFECT_NONE;
end;

function TNamespace.DisplayNameOf(Flags: Longword): WideString;
var
  StrRet: TSTRRET;
begin
  if Assigned(ParentShellFolder) then
  begin
    FillChar(StrRet, SizeOf(StrRet), #0);
    if ParentShellFolder.GetDisplayNameOf(RelativePIDL, Flags, StrRet) = NOERROR
    then
      Result := StrRetToStr(StrRet, RelativePIDL)
    else
      Result := '';
  end else
    Result := ''
end;

function TNamespace.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
{ Called when there is a pending COM drop on the namespace.  The namespace will }
{ decide if it can handle the information passed.                               }
begin
  if DropTarget and Assigned(DropTargetInterface) then
    Result := DropTargetInterface.DragEnter(dataObj, grfKeyState, pt, dwEffect)
  else begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK
  end
end;

function TNamespace.DragLeave: HResult;
{ Called when there is a pending COM drop on the namespace.  The namespace will }
{ decide if it can handle the information passed.                               }
begin
  if DropTarget and Assigned(DropTargetInterface) then
  begin
    Result := DropTargetInterface.DragLeave;
    FDropTargetInterface := nil;  // Don't recycle the interface
  end else
    Result := S_OK
end;

function TNamespace.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
{ Called when there is a pending COM drop on the namespace.  The namespace will }
{ decide if it can handle the information passed.                               }
begin
  if DropTarget and Assigned(DropTargetInterface) then
    Result := DropTargetInterface.DragOver(grfKeyState, pt, dwEffect)
  else begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK
  end
end;

function TNamespace.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HResult;
{ Called when there is a COM object is dropped on the namespace.  The namespace }
{ will handle the action as well.                                               }
begin
  if DropTarget and Assigned(DropTargetInterface) then
  begin
    Result := DropTargetInterface.Drop(dataObj, grfKeyState, pt, dwEffect);
    FDropTargetInterface := nil;  // don't recycle the interface
  end else
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK
  end
end;

function TNamespace.EnumerateFolder(MessageWnd: HWnd; Folders, NonFolders,
  IncludeHidden: Boolean; EnumFunc: TEnumFolderCallback;
  UserData: Pointer): integer;
{ Enumerate a folder to get its subfolders.  For each subfolder the the         }
{ callback function is called so a new TNamespace may be created.               }
{ You have a choice to receive Folders, NonFolders (files), and Hidden          }
{ objects,  UserData is useful to pass info back to the callback function.      }
{ Encapsulates the IShellFolder.EnumObjects function                            }
{ The reciever of the Callback function is responsible for Freeing the PIDLs!   }
{ Returns the number of objects in the folder.                                  }
var
  Enum: IEnumIDList;
  Flags: Longword;
  Fetched: Longword;
  Item: PItemIDList;
  Terminate: Boolean;
  OldError: integer;
  OldWow64: Pointer;
begin
  Result := 0;
  { This fixed a problem Rik Baker had:                                         }
  { "The error message is "C:\WINDOWS\SYSTEM\ODBCINST.DLL is not a valid        }
  { Windows Image", however the file appears fine and I've now seen the same    }
  { message on 9 different 2000 boxes spread across the country.                }

  OldError := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    if Folder and Assigned(ShellFolder) then
    begin
      if Assigned(EnumFunc) then
      begin
        Terminate := False;
        Flags := 0;
        if Folders then
          Flags := Flags or SHCONTF_FOLDERS;
        if NonFolders then
          Flags := Flags or SHCONTF_NONFOLDERS;
        if IncludeHidden then
          Flags := Flags or SHCONTF_INCLUDEHIDDEN;


        if Valid then
        begin
          OldWow64 := Wow64RedirectDisable;
          try

            // Right now you can't mix custom items and real shell items in the same folder
            if ShellFolder.EnumObjects(MessageWnd, Flags, Enum) = NOERROR  then
            begin
              // Vista Enum is nil every once in a while
              if Assigned(Enum) then
              begin
                while (Enum.Next(1, Item, Fetched) = NOERROR) and not Terminate do
                begin
                  if EnumFunc(MessageWnd, Item, Self, UserData, Terminate) then
                   Inc(Result)
                end
              end
            end
          finally
            Wow64RedirectRevert(OldWow64)
          end
        end
      end
    end
  finally
    SetErrorMode(OldError);
  end
end;

function TNamespace.EnumerateFolderEx(MessageWnd: HWnd; FileObjects: TFileObjects;
      EnumFunc: TEnumFolderCallback; UserData: pointer; AfterValidEnumIDList: TNotifyEvent = nil): integer;
{ Enumerate a folder to get its subfolders.  For each subfolder the the         }
{ callback function is called so a new TNamespace may be created.               }
{ You have a choice to receive Folders, NonFolders (files), and Hidden          }
{ objects,  UserData is useful to pass info back to the callback function.      }
{ Encapsulates the IShellFolder.EnumObjects function                            }
{ The reciever of the Callback function is responsible for Freeing the PIDLs!   }
{ Returns the number of objects in the folder.                                  }
var
  Enum: IEnumIDList;
  Flags: Longword;
  Fetched: Longword;
  Item: PItemIDList;
  Terminate: Boolean;
  OldError: integer;
  OldWow64: Pointer;
begin
  Result := 0;
  { This fixed a problem Rik Baker had:                                         }
  { "The error message is "C:\WINDOWS\SYSTEM\ODBCINST.DLL is not a valid        }
  { Windows Image", however the file appears fine and I've now seen the same    }
  { message on 9 different 2000 boxes spread across the country.                }    
  OldError := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    if not MP_UseModalDialogs then
      MessageWnd := 0;

    if Folder and Assigned(ShellFolder)  then
    begin
      ValidateFileObjects(FileObjects);
      Flags := FileObjectsToFlags(FileObjects);
      Terminate := False;
      if Valid then
      begin
        OldWow64 := Wow64RedirectDisable;
        try
          // Right now you can't mix custom items and real shell items in the same folder
          if ShellFolder.EnumObjects(MessageWnd, Flags, Enum) = NOERROR then
          begin
            // Vista Enum is nil every once in a while
            if Assigned(Enum) then
            begin
              if Assigned(AfterValidEnumIDList) then
                AfterValidEnumIDList(Self);
              // Allows calling with a nil EnumFunc to pop messages in the EnumObjects call above
              if not Assigned(EnumFunc) then
                EnumFunc := EnumFuncDummy;
              if Assigned(Enum) then
                while (Enum.Next(1, Item, Fetched) = NOERROR) and not Terminate do
                begin
                  if EnumFunc(MessageWnd, Item, Self, UserData, Terminate) then
                   Inc(Result)
                end
            end
          end
        finally
          Wow64RedirectRevert(OldWow64)
        end
      end
    end
  finally
    SetErrorMode(OldError);
  end
end;

function TNamespace.ExecuteContextMenuVerb(Owner: TWinControl; AVerb: WideString;
  APIDLArray: TRelativePIDLArray; ShiftKeyState: TExecuteVerbShift = evsCurrent;
  WorkingDir: WideString = ''): Boolean;
//
// Pass the Verb "VIRTUAL_DEFAULT" to select the Default Menu Item
//
const
  MaxVerbLen = 128;

var
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  ContextMenu3: IContextMenu3;
  Menu: hMenu;
  InvokeInfo: TCMInvokeCommandInfoEx;
  ShiftDown, ControlDown: Boolean;
  VerbStrW: array[0..128] of WideChar;
  VerbStrA: array[0..128] of AnsiChar;
  lpVerbStrW: PWideChar;
  lpVerbStrA: PAnsiChar;
begin
  if Assigned(ParentShellFolder) then
  begin
    case ShiftKeyState of
      evsCurrent: ShiftDown := GetKeyState(VK_SHIFT) and $8000 <> 0;
      evsUp: ShiftDown := False;
    else
      ShiftDown := True
    end;
    ControlDown := GetKeyState(VK_CONTROL) and $8000 <> 0;

    if Assigned(APIDLArray) then
      ContextMenu := InternalGetContextMenuInterface(APIDLArray)
    else
      ContextMenu := ContextMenuInterface;

    if Assigned(ContextMenu) then
    begin
      Menu := CreatePopupMenu;
      try
        if not Succeeded(ContextMenu.QueryInterface(IContextMenu3, ContextMenu3)) then
          ContextMenu3 := nil;

        if not Assigned(ContextMenu3) then
          if not Succeeded(ContextMenu.QueryInterface(IContextMenu2, ContextMenu2)) then
            ContextMenu2 := nil;

        if Assigned(ContextMenu3) then
          ContextMenu3.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_NORMAL or CMF_EXPLORE or CMF_DEFAULTONLY)
        else
        if Assigned(ContextMenu2) then
          ContextMenu2.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_NORMAL or CMF_EXPLORE or CMF_DEFAULTONLY)
        else
          ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_NORMAL or CMF_EXPLORE or CMF_DEFAULTONLY);

        FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);

        { The result of using the 'verb' string and the MakeIntResource is      }
        { different expecially on system folders.  This forces it to use        }
        { MakeIntResource if it can.                                            }


        lpVerbStrW := @VerbStrW[0];
        lpVerbStrA := @VerbStrA[0];

        if Assigned(ContextMenu3) then
          MapVerbToIntResource(ContextMenu3, Menu, AVerb, lpVerbStrW, lpVerbStrA)
        else
        if Assigned(ContextMenu2) then
          MapVerbToIntResource(ContextMenu2, Menu, AVerb, lpVerbStrW, lpVerbStrA)
        else
          MapVerbToIntResource(ContextMenu, Menu, AVerb, lpVerbStrW, lpVerbStrA);

        InvokeInfo.lpVerbW := lpVerbStrW;
        InvokeInfo.lpVerb := lpVerbStrA;

        if IsUnicode then
          InvokeInfo.cbSize := SizeOf(TCMInvokeCommandInfoEx)
        else
          InvokeInfo.cbSize := SizeOf(TCMInvokeCommandInfo);

        InvokeInfo.hWnd := Owner.Handle;

        InvokeInfo.nShow := SW_SHOWNORMAL;

        InvokeInfo.lpDirectory := PAnsiChar( AnsiString( WorkingDir));
        InvokeInfo.lpDirectoryW := PWideChar( WorkingDir);

        InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_ASYNCOK;

        if ShiftDown then
          InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_SHIFT_DOWN;
        if ControlDown then
          InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_CONTROL_DOWN;

        if Assigned(ContextMenu3) then
          Result := Succeeded(ContextMenu3.InvokeCommand(InvokeInfo))
        else
        if Assigned(ContextMenu2) then
          Result := Succeeded(ContextMenu2.InvokeCommand(InvokeInfo))
        else
          Result := Succeeded(ContextMenu.InvokeCommand(InvokeInfo));
      finally
        if Menu <> 0 then
          DestroyMenu(Menu);
        ContextMenu := nil;
        ContextMenu2 := nil;
        ContextMenu3 := nil;
        CurrentContextMenu := nil;
        CurrentContextMenu2 := nil;  // not sure it is available yet
      end;
    end else
      Result := False
  end else
    Result := False
end;

function TNamespace.FolderSize(Invalidate: Boolean; RecurseFolder: Boolean = False): Int64;
begin
  if not(scFolderSize in ShellCache.ShellCacheFlags) or Invalidate then
  begin
    if Folder and FileSystem then
      FShellCache.Data.FolderSize := CalcuateFolderSize(NameForParsing, RecurseFolder);
    Include(FShellCache.ShellCacheFlags, scFolderSize);
  end;
  Result := FShellCache.Data.FolderSize
end;

function TNamespace.ExplorerStyleAttributeStringList(CapitalLetters: Boolean): WideString;
begin
  Result := '';
  if Archive then
    Result := Result + STR_ARCHIVE;
  if Hidden then
    Result := Result + STR_HIDDEN;
  if ReadOnlyFile then
    Result := Result + STR_READONLY;
  if SystemFile then
    Result := Result + STR_SYSTEM;
  if Compressed then
    Result := Result + STR_COMPRESS;
  if not CapitalLetters then
    Result := WideLowerCase(Result)

end;

function TNamespace.GetArchive: Boolean;
{ GETTER: Does the file attributes contain Archive?                             }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetAttributesString: WideString;
begin
  if FileSystem then
    Result := ExplorerStyleAttributeStringList(True)
  else
    Result := ''
end;

function TNamespace.GetBrowsable: Boolean;
begin
  if not (scBrowsable in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_BROWSABLE, False) then
      Include(FShellCache.Data.Attributes, caBrowsable);
    Include(FShellCache.ShellCacheFlags, scBrowsable);
  end;
  Result := caBrowsable in ShellCache.Data.Attributes
end;

function TNamespace.GetCanCopy: Boolean;
begin
  if not (scCanCopy in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_CANCOPY, False) then
      Include(FShellCache.Data.Attributes, caCanCopy);
    Include(FShellCache.ShellCacheFlags, scCanCopy);
  end;
  Result := caCanCopy in ShellCache.Data.Attributes
end;

function TNamespace.GetCanDelete: Boolean;
{ GETTER: Can we delete the namespace?                                          }
begin
  if not (scCanDelete in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_CANDELETE, False) then
      Include(FShellCache.Data.Attributes, caCanDelete);
    Include(FShellCache.ShellCacheFlags, scCanDelete);
  end;
  Result := caCanDelete in ShellCache.Data.Attributes
end;

function TNamespace.GetCanLink: Boolean;
begin
  if not (scCanLink in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_CANLINK, False) then
      Include(FShellCache.Data.Attributes, caCanLink);
    Include(FShellCache.ShellCacheFlags, scCanLink);
  end;
  Result := caCanLink in ShellCache.Data.Attributes
end;

function TNamespace.GetCanMove: Boolean;
begin
  if not (scCanMove in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_CANMOVE, False) then
      Include(FShellCache.Data.Attributes, caCanMove);
    Include(FShellCache.ShellCacheFlags, scCanMove);
  end;
  Result := caCanMove in ShellCache.Data.Attributes
end;

function TNamespace.GetCanRename: Boolean;
{ GETTER: Can we Rename the namespace?                                         }
begin
  if not (scCanRename in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_CANRENAME, False) then
      Include(FShellCache.Data.Attributes, caCanRename);
    Include(FShellCache.ShellCacheFlags, scCanRename);
  end;
  Result := caCanRename in ShellCache.Data.Attributes
end;

function TNamespace.GetCategoryCount: Integer;
begin
  LoadCategoryInfo;
  Result := 0;
end;

function TNamespace.GetCLSID: TGUID;
var
  DescriptionID: TSHDESCRIPTIONID;
  PersistFolder: IPersistFolder;
begin
  Result := GUID_NULL;
  if Assigned(ParentShellFolder) then
    if Succeeded(SHGetDataFromIDList(ParentShellFolder, RelativePIDL, SHGDFIL_DESCRIPTIONID, @DescriptionID, SizeOf(TSHDESCRIPTIONID))) then
      Result := DescriptionID.Id;
  if IsEqualGUID(Result, GUID_NULL) then
  begin
    if Succeeded(ShellFolder.QueryInterface(IPersistFolder, PersistFolder)) then
      if not Succeeded(PersistFolder.GetClassID(Result)) then
        Result := GUID_NULL;
  end;
end;

function TNamespace.GetCompressed: Boolean;
{ GETTER: Does the file attributes contain Compressed?                          }
begin
  if not (scCompressed in ShellCache.ShellCacheFlags) then
  begin
    if IsUnicode then
    begin
      if not Assigned(FWin32FindDataW) then
        GetDataFromIDList;
      if Assigned(FWin32FindDataW) and FileSystem then
        if FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED <> 0 then
          Include(FShellCache.Data.Attributes, caCompressed)
    end else
    begin
      if not Assigned(FWin32FindDataA) then
        GetDataFromIDList;
      if Assigned(FWin32FindDataA) and FileSystem then
        if FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED <> 0 then
          Include(FShellCache.Data.Attributes, caCompressed)
    end;
    Include(FShellCache.ShellCacheFlags, scCompressed);
  end;
  Result := caCompressed in ShellCache.Data.Attributes;
end;

function TNamespace.GetContextMenuInterface: IContextMenu;
var
  PIDLArray: TRelativePIDLArray;
begin
  if not Assigned(Result) then
  begin
    SetLength(PIDLArray, 1);
    PIDLArray[0] := RelativePIDL;
    Result := InternalGetContextMenuInterface(PIDLArray);
  end
end;

function TNamespace.GetContextMenu2Interface: IContextMenu2;
var
  Found: Boolean;
  ContextMenu: IContextmenu;
begin
  Found := False;
  ContextMenu := ContextMenuInterface;
  if Assigned(ContextMenu) then
  begin
    Found := ContextMenu.QueryInterface(IID_IContextMenu2, Pointer(Result)) <>  E_NOINTERFACE;
    CurrentContextMenu2 := Result
  end;
  if not Found then
    Result := nil
end;

function TNamespace.GetContextMenu3Interface: IContextMenu3;
var
  Found: Boolean;
  ContextMenu: IContextmenu;
begin
  Found := False;
  ContextMenu := ContextMenuInterface;
  if Assigned(ContextMenu) then
  begin
    Found := ContextMenu.QueryInterface(IContextMenu3, Pointer(Result)) <>  E_NOINTERFACE;
    CurrentContextMenu2 := Result
  end;
  if not Found then
    Result := nil
end;

function TNamespace.GetCreationTime: WideString;
{ GETTER: Creation time of the file.                                            }
begin
  if not (scCreationTime in ShellCache.ShellCacheFlags) then
  begin
    { Don't use Win32FindData cache, re-read the file times }
    GetFileTimes;

    if IsUnicode then
    begin
      if Assigned(FWin32FindDataW) and FileSystem then
        FShellCache.Data.CreationTime := ConvertTFileTimeToLocalStr(FWin32FindDataW^.ftCreationTime)
      else
        FShellCache.Data.CreationTime := '';
    end else
    begin
      if Assigned(FWin32FindDataA) and FileSystem then
        FShellCache.Data.CreationTime := ConvertTFileTimeToLocalStr(FWin32FindDataA^.ftCreationTime)
      else
        FShellCache.Data.CreationTime := '';
    end;
    Include(FShellCache.ShellCacheFlags, scCreationTime);
  end;
  Result := ShellCache.Data.CreationTime
end;

function TNamespace.GetCreationDateTime: TDateTime;
begin
  Result :=  ConvertFileTimetoDateTime(CreationTimeRaw)
end;

function TNamespace.GetCreationTimeRaw: TFileTime;
begin
  { Don't use Win32FindData cache, re-read the file times }
  GetFileTimes;

  if IsUnicode then
  begin
    if Assigned(FWin32FindDataW) then
      Result := FWin32FindDataW^.ftCreationTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end else
  begin
    if Assigned(FWin32FindDataA) then
      Result := FWin32FindDataA^.ftCreationTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end
end;

function TNamespace.GetCurrentContextMenu: IContextMenu;
begin
  Result := FCurrentContextMenu;
end;

function TNamespace.GetCurrentContextMenu2: IContextMenu2;
begin
  Result := FCurrentContextMenu2;
end;

function TNamespace.GetDelagateNS: TNamespace;
begin
  if not Assigned(FDelagateNS) then
    FDelagateNS := TNamespace.Create( PathToPIDL(NameForParsing, 0, False), nil);
  Result := FDelagateNS
end;

function TNamespace.GetIsLibraryChild: Boolean;
begin
  // Tests to see if the folder is contained within the Library structure in Win7 and up
  if not (scInLibrary in ShellCache.ShellCacheFlags) then
  begin
   if IsWin7 and Assigned(UserLibraryFolder) then
     if ILIsParent(UserLibraryFolder.AbsolutePIDL, AbsolutePIDL, False) then
        Include(FShellCache.Data.Attributes, caInLibrary);    // Set the flag
    Include(FShellCache.ShellCacheFlags, scInLibrary);        // Set that the flag is valid
  end;
  Result := caInLIbrary in ShellCache.Data.Attributes
end;

function TNamespace.GetJunctionPoint: Boolean;
begin
  Result := False;
  if FileSystem then
  begin
    if IsUnicode then
    begin
      if not Assigned(FWin32FindDataW) then
        GetDataFromIDList;
      if Assigned(Win32FindDataW) then
        Result := (Win32FindDataW.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT) and
                  (Win32FindDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
                  (Win32FindDataW.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0)
    end else
    begin
      if not Assigned(FWin32FindDataA) then
        GetDataFromIDList;
      if Assigned(Win32FindDataA) then
        Result := (Win32FindDataA.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT) and
                  (Win32FindDataA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
                  (Win32FindDataA.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0)
    end
  end
end;

function TNamespace.GetJunctionPointResolvePath: WideString;
var
  hLink, hMem: THandle;
  ReparseDataBuffer: PReparseDataBuffer;
  BytesReturned: DWORD;
begin
  Result := '';
  if JunctionPoint then
  begin
    if IsUnicode then
      hLink := CreateFileW(PWideChar( NameForParsing), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0)
    else
      hLink := CreateFileA(PAnsiChar( AnsiString( NameForParsing)), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
    if hLink <> INVALID_HANDLE_VALUE then
    try
      BytesReturned := 0;
      hMem := GlobalAlloc(GHND, MAXIMUM_REPARSE_DATA_BUFFER_SIZE);
      try
        if hMem <> 0 then
        try
          ReparseDataBuffer := GlobalLock(hMem);
          if DeviceIoControl(hLink, FSCTL_GET_REPARSE_POINT, nil, 0, ReparseDataBuffer, MAXIMUM_REPARSE_DATA_BUFFER_SIZE, BytesReturned, nil) then
          begin
            if ReparseDataBuffer^.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT then
            begin
              SetLength(Result, MAX_PATH);
              ZeroMemory(Pointer( @Result[1]), MAX_PATH * 2);
              MoveMemory(Pointer( @Result[1]), Pointer( @ReparseDataBuffer^.PathBuffer[ReparseDataBuffer^.SubstituteNameOffset * 2]), ReparseDataBuffer^.SubstituteNameLength);
              SetLength(Result, lstrlenW(PWideChar(Result)));
              if Pos(WideString( '\??\'), Result) > 0 then
              begin
                MoveMemory(Pointer( @Result[1]), @Result[5], (Length(Result) - 4) * 2);
                SetLength(Result, Length(Result) - 4);
              end
            end
          end
        finally
          GlobalUnLock(hMem)
        end;
      finally
        GlobalFree(hMem)
      end
    finally
      Windows.CloseHandle(hLink)
    end
  end
end;

function TNamespace.GetParent: TNamespace;
var
  P: PItemIDList;
begin
  if not Assigned(FParent) then
  begin
    P := PIDLMgr.CopyPIDL(AbsolutePIDL);
    if PIDLMgr.IDCount(P) > 1 then
      FParent := TNamespace.Create(PIDLMgr.StripLastID(P), nil)
    else begin
      PIDLMgr.FreePIDL(P);
      FParent := TNamespace.Create(nil, nil);
    end;
    Include(FStates, nsOwnsParent);
  end;
  Result := FParent;
end;

function TNamespace.GetPropertyStoreInterface: IPropertyStore;
begin
  if not Assigned(FPropertyStoreInterface) then
    ParentShellFolder.GetUIObjectOf(0, 1, FRelativePIDL, IPropertyStore, nil, Pointer(FPropertyStoreInterface));
  Result := FPropertyStoreInterface;
end;

function TNamespace.GetSymbolicLink: Boolean;
begin
  Result := False;
  if FileSystem then
  begin
    if IsUnicode then
    begin
      if not Assigned(FWin32FindDataW) then
        GetDataFromIDList;
      Result := (Win32FindDataW.dwReserved0 = IO_REPARSE_TAG_SYMLINK) and
                (Win32FindDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
                (Win32FindDataW.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0)
    end else
    begin
      if not Assigned(FWin32FindDataA) then
        GetDataFromIDList;
      Result := (Win32FindDataA.dwReserved0 = IO_REPARSE_TAG_SYMLINK) and
                (Win32FindDataA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
                (Win32FindDataA.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0)
    end
  end
end;

function TNamespace.GetSymbolicLinkResolvePath: WideString;
var
  hLink, hMem: THandle;
  ReparseDataBuffer: PReparseDataBuffer;
  BytesReturned: DWORD;
begin
  Result := '';
  if SymbolicLink then
  begin
    if IsUnicode then
      hLink := CreateFileW(PWideChar( NameForParsing), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0)
    else
      hLink := CreateFileA(PAnsiChar( AnsiString( NameForParsing)), 0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
    if hLink <> INVALID_HANDLE_VALUE then
    try
      BytesReturned := 0;
      hMem := GlobalAlloc(GHND, MAXIMUM_REPARSE_DATA_BUFFER_SIZE);
      try
        if hMem <> 0 then
        try
          ReparseDataBuffer := GlobalLock(hMem);
          if DeviceIoControl(hLink, FSCTL_GET_REPARSE_POINT, nil, 0, ReparseDataBuffer, MAXIMUM_REPARSE_DATA_BUFFER_SIZE, BytesReturned, nil) then
          begin
            if ReparseDataBuffer^.ReparseTag = IO_REPARSE_TAG_SYMLINK then
            begin
              SetLength(Result, MAX_PATH);
              ZeroMemory(Pointer( @Result[1]), MAX_PATH * 2);
              MoveMemory(Pointer( @Result[1]), Pointer( @ReparseDataBuffer^.PathBuffer[ReparseDataBuffer^.SubstituteNameOffset * 2]), ReparseDataBuffer^.SubstituteNameLength);
              SetLength(Result, lstrlenW(PWideChar(Result)));
              if Pos(WideString( '\??\'), Result) > 0 then
              begin
                MoveMemory(Pointer( @Result[1]), @Result[5], (Length(Result) - 4) * 2);
                SetLength(Result, Length(Result) - 4);
              end
            end
          end
        finally
          GlobalUnLock(hMem)
        end;
      finally
        GlobalFree(hMem)
      end
    finally
      Windows.CloseHandle(hLink)
    end
  end
end;

function TNamespace.GetThreadedDetailLoaded(ColumnIndex: Integer): Boolean;
begin
  Result := False;
  EnsureDetailCache;
  // See if the Folder that contains this object has ColumnIndex columns
  if Parent.DetailsValidIndex(ColumnIndex) then
    Result := docThreadLoaded in ShellCache.Data.DetailsOfCache[ColumnIndex].Cached
end;

function TNamespace.GetThreadedDetailLoading(ColumnIndex: Integer): Boolean;
begin
  Result := False;
  EnsureDetailCache;
  if Parent.DetailsValidIndex(ColumnIndex) then
    Result := docThreadLoading in ShellCache.Data.DetailsOfCache[ColumnIndex].Cached
end;

function TNamespace.ParentWnd: HWnd;
var
  dwProcessID, dwThreadID: Cardinal;
begin
  Result := 0;
  // GetActiveWindow has strange results like sending the application using
  // TNamespace to the background (actually bringing other apps to the forground)
  if MP_UseModalDialogs and (GlobalParentWnd <> 0) then
  begin
    // Onlcy allow TNamespaces created in the context of the thread that
    // created the window use the window
    {$IFDEF COMPILER_6_UP}
    dwThreadID := GetWindowThreadProcessId(GlobalParentWnd, dwProcessID);
    {$ELSE}
    dwThreadID := GetWindowThreadProcessId(GlobalParentWnd, @dwProcessID);
    {$ENDIF}
    if dwThreadID = GetCurrentThreadId then
      Result := GlobalParentWnd
  end
end;

procedure TNamespace.EnsureDetailCache;
var
  i: Integer;
  TempCache: PDetailsOfCacheRec;
begin
  if not (scDetailsOfCache in ShellCache.ShellCacheFlags) then
  begin
    SetLength(FShellCache.Data.DetailsOfCache, Parent.DetailsSupportedColumns);
    for i := 0 to Length(FShellCache.Data.DetailsOfCache) - 1 do
    begin
      TempCache := @ShellCache.Data.DetailsOfCache[i];
      TempCache^.Cached := [];
      TempCache^.Caption := '';
      TempCache^.States := [];
    end;
    Include(FShellCache.ShellCacheFlags, scDetailsOfCache);
  end;
end;

procedure TNamespace.ExecuteContextMenuVerbMultiPath(Owner: TWinControl;
  Verb: WideString; Namespaces: TNamespaceArray;
  ShiftKeyState: TExecuteVerbShift = evsCurrent);
var
  Menu: TCommonShellMultiParentContextMenu;
  ShellFolder: IShellFolder;
begin
  Menu := TCommonShellMultiParentContextMenu.Create(Owner);
  Menu.ReferenceCounted := True;
  ShellFolder := Menu;
  Menu.ExecuteContextMenuVerb(Menu.MsgWnd, Namespaces, Verb, ShiftKeyState);
  ShellFolder := nil;
end;

procedure TNamespace.GetDataFromIDList;
{ Retrieves and caches the Data stored by the shell PIDL.                       }
var
  Error: Boolean;
  hFile: THandle;
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) and not IsDesktop then
    begin
      if not (scInvalidIDListData in ShellCache.ShellCacheFlags) then
      begin
        Error := True;
        try
          if Assigned(ParentShellFolder) then
          begin
            GetMem(FWin32FindDataW, SizeOf(TWin32FindDataW));
            FillChar(FWin32FindDataW^, SizeOf(FWin32FindDataW^), #0);
            { Children of the Desktop won't work if accessed from the Desktop       }
            { ShellFolder, they must use the physical Desktop folder.               }
            if Assigned(Parent) and (Parent.IsDesktop) and Assigned(PhysicalDesktopFolder) then
            begin
              Error := SHGetDataFromIDListW_MP(PhysicalDesktopFolder.ShellFolder, RelativePIDL,
                SHGDFIL_FINDDATA, FWin32FindDataW, SizeOf(TWin32FindDataW)) <> NOERROR;
            end else
              Error := SHGetDataFromIDListW_MP(ParentShellFolder, RelativePIDL, SHGDFIL_FINDDATA,
                         FWin32FindDataW, SizeOf(TWin32FindDataW)) <> NOERROR;
          end
        finally
          if Error and not Removable then  // Wait for when the data is really needed for removable drives
          begin
            hFile := FindFirstFileW_MP(PWideChar( NameForParsing), FWin32FindDataW^);
            if hFile = INVALID_HANDLE_VALUE then
            begin
              if Assigned(FWin32FindDataW) then
                FreeMem(FWin32FindDataW, SizeOf(TWin32FindDataW));
              FWin32FindDataW := nil;
            end else
              Windows.FindClose(hFile);
            Include(FShellCache.ShellCacheFlags, scInvalidIDListData)
          end
        end;
      end
    end
  end else
  begin
    if not Assigned(FWin32FindDataA) and not IsDesktop then
    begin
      if not (scInvalidIDListData in ShellCache.ShellCacheFlags) then
      begin
        Error := True;
        try
          if Assigned(ParentShellFolder) then
          begin
            GetMem(FWin32FindDataA, SizeOf(TWin32FindDataA));
            FillChar(FWin32FindDataA^, SizeOf(TWin32FindDataA), #0);
            { Children of the Desktop won't work if accessed from the Desktop       }
            { ShellFolder, they must use the physical Desktop folder.               }
            if Assigned(Parent) and (Parent.IsDesktop) and Assigned(PhysicalDesktopFolder) then
            begin
              Error := SHGetDataFromIDListA(PhysicalDesktopFolder.ShellFolder, RelativePIDL,
                SHGDFIL_FINDDATA, FWin32FindDataA, SizeOf(TWin32FindDataA)) <> NOERROR;
            end else
              Error := SHGetDataFromIDListA(ParentShellFolder, RelativePIDL, SHGDFIL_FINDDATA,
                         FWin32FindDataA, SizeOf(TWin32FindDataA)) <> NOERROR;
          end
        finally
          if Error and not Removable then  // Wait for when the data is really needed for removable drives
          begin
            hFile := FindFirstFileA(PAnsiChar( AnsiString( NameForParsing)), FWin32FindDataA^);
            if hFile = INVALID_HANDLE_VALUE then
            begin
              if Assigned(FWin32FindDataA) then
                FreeMem(FWin32FindDataA, SizeOf(TWin32FindDataA));
              FWin32FindDataA := nil;
            end else
              Windows.FindClose(hFile);
            Include(FShellCache.ShellCacheFlags, scInvalidIDListData)
          end
        end;
      end
    end
  end
end;

function TNamespace.GetDataObjectInterface: IDataObject;
begin
  Result := InternalGetDataObjectInterface(nil)
end;

function TNamespace.GetDescription: TObjectDescription;
var
  DescriptionID: TSHDESCRIPTIONID;
begin
  Result := odError;
  if Assigned(ParentShellFolder) then
  begin
    if Succeeded(SHGetDataFromIDList(ParentShellFolder, RelativePIDL, SHGDFIL_DESCRIPTIONID, @DescriptionID, SizeOf(TSHDESCRIPTIONID))) then
      Result := TObjectDescription(DescriptionID.dwDescriptionId)
  end
end;

function TNamespace.GetDetailsSupported: Boolean;
begin
  { IShellDetails depends on the parent folder implementing the interface }
  if Assigned(Parent) then
    Result := Assigned(Parent.ShellFolder2) or Assigned(ParentShellDetailsInterface)
  else
    Result := False
end;

function TNamespace.GetDirectory: Boolean;
{ GETTER: Does the file attributes contain Directory?                           }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetDropTarget: Boolean;
{ GETTER: Can we drop another object on this namespace?  Note the Desktop is    }
{ handled as a special case.  The IDropTarget is mapped to the physical folder  }
{ location in the DropTargetInterface property.                                 }
begin
  Result := TestAttributesOf(SFGAO_DROPTARGET, False) or
    PIDLMgr.IsDesktopFolder(RelativePIDL);
end;

function TNamespace.GetDropTargetInterface: IDropTarget;
var
  Found: Boolean;
begin
  if not Assigned(FDropTargetInterface) then
  begin
    Found := False;
    if Assigned(ParentShellFolder) then
    begin
      Found := ParentShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL,
        IID_IDropTarget, nil, Pointer(FDropTargetInterface)) = NOERROR;
    end;
    if not Found and IsDesktop then
      FDropTargetInterface := PhysicalDesktopFolder.DropTargetInterface;
  end;
  Result := FDropTargetInterface
end;

function TNamespace.GetExtension: WideString;
begin
  Result := WideExtractFileExt(NameForParsingInFolder);
end;

function TNamespace.GetExtractImage: TExtractImage;
begin
  if not Assigned(FExtractImage) then
  begin
    FExtractImage := TExtractImage.Create;
    FExtractImage.Owner := Self
  end;
  Result := FExtractImage
end;

function TNamespace.GetExtractIconAInterface: IExtractIconA;
var
  Found: Boolean;
begin
  if Assigned(ParentShellFolder) then
  begin
    Found := Succeeded(ParentShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL, IExtractIconA, nil, Pointer(Result)));
    if not Found and Assigned(ShellFolder) then
      Found := Succeeded(ShellFolder.CreateViewObject(ParentWnd, IExtractIconA, Pointer(Result)));
    if not Found then
      Result := nil
  end
end;

function TNamespace.GetExtractIconWInterface: IExtractIconW;
var
  Found: Boolean;
begin
  if Assigned(ParentShellFolder) then
  begin
    Found := Succeeded(ParentShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL, IExtractIconW, nil, Pointer(Result)));
    if not Found and Assigned(ShellFolder) then
      Found := Succeeded(ShellFolder.CreateViewObject(ParentWnd, IExtractIconW, Pointer(Result)));
    if not Found then
      Result := nil
  end
end;

function TNamespace.GetFileName: WideString;
{ GETTER: FileName from the file system (FindFirst)                             }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.cFileName
    else
      Result := '';
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.cFileName
    else
      Result := '';
  end
end;

function TNamespace.GetFileSysAncestor: Boolean;
// Only works reliablely on Win2k and above
begin
  if not (scFileSysAncestor in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_FILESYSANCESTOR, False) then
      Include(FShellCache.Data.Attributes, caFileSysAncestor);
    Include(FShellCache.ShellCacheFlags, scFileSysAncestor);
  end;
  Result := caFileSysAncestor in ShellCache.Data.Attributes
end;

function TNamespace.GetFileSystem: Boolean;
{ GETTER: Is the namespace part of the physical file system?                    }
begin
  if not (scFileSystem in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_FILESYSTEM, False) then
      Include(FShellCache.Data.Attributes, caFileSystem);
    Include(FShellCache.ShellCacheFlags, scFileSystem);
  end;
  Result := caFileSystem in ShellCache.Data.Attributes
end;

procedure TNamespace.GetFileTimes;
var
  Handle: THandle;
  FileDataA: TWin32FindDataA;
  FileDataW: TWin32FindDataW;
  S: AnsiString;
  OldMode: UINT;
begin
  if not (scFileTimes in ShellCache.ShellCacheFlags) then
  begin
    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
    try
      if IsUnicode then
      begin
        if not Assigned(FWin32FindDataW) then
          GetDataFromIDList;
        if FileSystem and Assigned(FWin32FindDataW)  then
        begin
          FillChar(FileDataW, SizeOf(FileDataW), #0);
          Handle := FindFirstFileW_MP(PWideChar( NameParseAddress), FileDataW);
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            Windows.FindClose(Handle); // There is no FindCloseW
            FWin32FindDataW.ftLastAccessTime := FileDataW.ftLastAccessTime;
            FWin32FindDataW.ftCreationTime := FileDataW.ftCreationTime;
            FWin32FindDataW.ftLastWriteTime := FileDataW.ftLastWriteTime
          end
        end;
      end else
      begin
        if not Assigned(FWin32FindDataA) then
          GetDataFromIDList;
        if FileSystem and Assigned(FWin32FindDataA)  then
        begin
          FillChar(FileDataA, SizeOf(FileDataA), #0);
          S := NameParseAddress;
          Handle := FindFirstFileA(PAnsiChar( S), FileDataA);
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            Windows.FindClose(Handle);  // There is no ASCI and Wide version
            FWin32FindDataA.ftLastAccessTime := FileDataA.ftLastAccessTime;
            FWin32FindDataA.ftCreationTime := FileDataA.ftCreationTime;
            FWin32FindDataA.ftLastWriteTime := FileDataA.ftLastWriteTime
          end
        end;
      end;
      Include(FShellCache.ShellCacheFlags, scFileTimes);
    finally
      SetErrorMode(OldMode);
    end;
  end;
end;

function TNamespace.GetFileType: WideString;
// File type string shown in column 3 of Explorer Listview
begin
  if not (scFileType in ShellCache.ShellCacheFlags) then
  begin
    if not Assigned(FSHGetFileInfoRec) then
      GetSHFileInfo;
    if Assigned(FSHGetFileInfoRec) then
    begin
      FShellCache.Data.FileType := FSHGetFileInfoRec^.FileType;
     { NT only half-assed supports the SHGetFileInfo...only if the ext is      }
     { associated with a program. So we build it ourselves                     }
      if FShellCache.Data.FileType = '' then
        FShellCache.Data.FileType := WideUpperCase(WideExtractFileExt(NameForParsing)) + STR_FILE;
    end else
      FShellCache.Data.FileType := '';
    Include(FShellCache.ShellCacheFlags, scFileType);
  end;
  Result := ShellCache.Data.FileType
end;

function TNamespace.GetFolder: Boolean;
// Ask the Folder if it is a Folder, as opposed to files.  Folders can  contain
// other objects.                                                        
begin
  if not (scFolder in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_FOLDER, False) then
      Include(FShellCache.Data.Attributes, caFolder);
    Include(FShellCache.ShellCacheFlags, scFolder);
  end;
  Result := caFolder in ShellCache.Data.Attributes;
end;

function TNamespace.GetFreePIDLOnDestroy: Boolean;
begin
  Result := nsFreePIDLOnDestroy in States
end;

function TNamespace.GetGhosted: Boolean;
// Ask the Folder if it is a ghosted file object. Partially encapsulates the
// IShellFolder.GetAttributesOf function.
begin
  if not (scGhosted in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_GHOSTED, False) then
      Include(FShellCache.Data.Attributes, caGhosted);
    Include(FShellCache.ShellCacheFlags, scGhosted);
  end;
  Result := caGhosted in ShellCache.Data.Attributes
end;

function TNamespace.GetHasPropSheet: Boolean;
begin
  Result := TestAttributesOf(SFGAO_HASPROPSHEET, False);
end;

function TNamespace.GetHasSubFolder: Boolean;
begin
  Result := TestAttributesOf(SFGAO_HASSUBFOLDER, False);
end;

function TNamespace.GetHidden: Boolean;
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetIconIndexChanged: Boolean;
begin
  Result := nsIconIndexChanged in States
end;

function TNamespace.GetIconIndex(OpenIcon: Boolean; IconSize: TIconSize; ForceLoad: Boolean = True): integer;
{ Retrieve the Icon Index either selected or not selected (open folder or       }
{ closed folder)                                                                }

  function GetIconByIShellIcon(AnOpenIcon: Boolean; Size: TIconSize; var Index: integer): Boolean;
  var
    Flags: Longword;
  begin
    Result := False;
    if Assigned(ShellIconInterface) then
    begin
      Flags := 0;
      if Size = icLarge then
        Flags := GIL_FORSHELL;
      if AnOpenIcon then
        Flags := GIL_OPENICON or Flags;
      Result := ShellIconInterface.GetIconOf(RelativePIDL, Flags, Index) = NOERROR
    end
  end;

  procedure GetIconBySHGetFileInfo(AnOpenIcon: Boolean; Size: TIconSize; var Index: Integer);
  { A little undocumented trick.  If you use the SHGFI_USEFILEATTRIBUTES flags  }
  { the SHGetFileInfo function does not fully access the object and is much     }
  { faster.                                                                     }
  { UPDATE: Unfortunatly this does not work well in Win98 :^(                   }
  var
    Flags: integer;
    InfoA: TSHFileInfoA;
    InfoW: TSHFileInfoW;
  begin
    Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SHELLICONSIZE;
    if IconSize = icLarge then
      Flags := Flags or SHGFI_LARGEICON
    else
      Flags := Flags or SHGFI_SMALLICON;
    if AnOpenIcon then
      Flags := Flags or SHGFI_OPENICON;
    if IsUnicode then
    begin
      FillChar(InfoW, SizeOf(InfoW), #0);
      if SHGetFileInfoW_MP(PWideChar(AbsolutePIDL), 0, InfoW, SizeOf(InfoW), Flags) <> 0 then
        Index := InfoW.iIcon
      else
        Index := 0
    end else
    begin
      FillChar(InfoA, SizeOf(InfoA), #0);
      if SHGetFileInfoA(PAnsiChar(AbsolutePIDL), 0, InfoA, SizeOf(InfoA), Flags) <> 0 then
        Index := InfoA.iIcon
      else
        Index := 0
    end
  end;

  function GetIcon(IsOpen: Boolean; IconSize: TIconSize): integer;
  begin
    if not GetIconByIShellIcon(IsOpen, IconSize, Result) then
      GetIconBySHGetFileInfo(IsOpen, IconSize, Result);
  end;

begin
  if not OpenIcon then
  begin
    if not (scSmallIcon in ShellCache.ShellCacheFlags) or ForceLoad then
    begin
      FShellCache.Data.SmallIcon := GetIcon(False, icSmall);
      IconCache := FShellCache.Data.SmallIcon;
      Include(FShellCache.ShellCacheFlags, scSmallIcon);
    end;
    Result := FShellCache.Data.SmallIcon;
  end else
  begin
    if not (scSmallOpenIcon in ShellCache.ShellCacheFlags) or ForceLoad then
    begin
      { Some Control panel icons return 0 for open but have icons for not open }
      { and it looks bad to show the default icon when the item is selected.   }
      { In NT4 some ControlPanel icons are the Mouse icons when selected! }
      if Assigned(Parent) and Parent.IsControlPanel then
        FShellCache.Data.SmallOpenIcon := GetIcon(False, icSmall)
      else begin
        FShellCache.Data.SmallOpenIcon := GetIcon(True, icSmall);
        { If it is 0 then try the normal icon }
        if FShellCache.Data.SmallOpenIcon = 0 then
          FShellCache.Data.SmallOpenIcon := GetIcon(False, icSmall);
      end;
      Include(FShellCache.ShellCacheFlags, scSmallOpenIcon)
    end;
    Result := ShellCache.Data.SmallOpenIcon;
  end;
end;

function TNamespace.GetInfoTip: WideString;
{ Retrieves the text from the IInfoTip interface in Win2k.                      }
var
  Buffer: PWideChar;
begin
  Result := '';
  try
    Buffer := nil;
    if Assigned(QueryInfoInterface) then
    begin
      if QueryInfoInterface.GetInfoTip(0, Buffer) = S_OK then
      begin
        Result := Buffer;
        PIDLMgr.FreeOLEStr(Buffer);
      end;
    end;
  except
    // Vista gives a floating point exception here once in a while
    Result := ''             
  end
end;

function TNamespace.GetLastAccessTime: WideString;
{ GETTER: Last Access time of the file.                                         }
begin
  if not (scLastAccessTime in ShellCache.ShellCacheFlags) then
  begin
    { Don't use Win32FindData cache, re-read the file times }
    GetFileTimes;
    
    if IsUnicode then
    begin
      if Assigned(FWin32FindDataW) and FileSystem then
        FShellCache.Data.LastAccessTime := ConvertTFileTimeToLocalStr(FWin32FindDataW^.ftLastAccessTime)
      else
        FShellCache.Data.LastAccessTime := '';
    end else
    begin
      if Assigned(FWin32FindDataA) and FileSystem then
        FShellCache.Data.LastAccessTime := ConvertTFileTimeToLocalStr(FWin32FindDataA^.ftLastAccessTime)
      else
        FShellCache.Data.LastAccessTime := '';
    end;
    Include(FShellCache.ShellCacheFlags, scLastAccessTime);
  end;
  Result := FShellCache.Data.LastAccessTime
end;

function TNamespace.GetLastAccessDateTime: TDateTime;
begin
  Result := ConvertFileTimetoDateTime(LastAccessTimeRaw)
end;

function TNamespace.GetLastAccessTimeRaw: TFileTime;
begin
  { Don't use Win32FindData cache, re-read the file times }
  GetFileTimes;

  if IsUnicode then
  begin
    if Assigned(FWin32FindDataW) then
      Result := FWin32FindDataW^.ftLastAccessTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end else
  begin
    if Assigned(FWin32FindDataA) then
      Result := FWin32FindDataA^.ftLastAccessTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end
end;


function TNamespace.GetLastWriteTime: WideString;
{ GETTER: Last write time for the file.                                         }
begin
  if not (scLastWriteTime in ShellCache.ShellCacheFlags) then
  begin
    { Don't use Win32FindData cache, re-read the file times }
    GetFileTimes;

    if IsUnicode then
    begin
      if Assigned(FWin32FindDataW) and FileSystem then
        FShellCache.Data.LastWriteTime := ConvertTFileTimeToLocalStr(FWin32FindDataW^.ftLastWriteTime)
      else
        FShellCache.Data.LastWriteTime := '';
    end else
    begin
      if Assigned(FWin32FindDataA) and FileSystem then
        FShellCache.Data.LastWriteTime := ConvertTFileTimeToLocalStr(FWin32FindDataA^.ftLastWriteTime)
      else
        FShellCache.Data.LastWriteTime := '';
    end;
    Include(FShellCache.ShellCacheFlags, scLastWriteTime);
  end;
  Result := FShellCache.Data.LastWriteTime
end;

function TNamespace.GetLastWriteDateTime: TDateTime;
begin
  Result :=  ConvertFileTimetoDateTime(LastWriteTimeRaw)
end;

function TNamespace.GetLastWriteTimeRaw: TFileTime;
{ GETTER: Last Write time for the file in raw TFileTime format.                 }
begin
  { Don't use Win32FindData cache, re-read the file times }
  GetFileTimes;

  if IsUnicode then
  begin
    if Assigned(FWin32FindDataW) then
      Result := FWin32FindDataW^.ftLastWriteTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end else
  begin
    if Assigned(FWin32FindDataA) then
      Result := FWin32FindDataA^.ftLastWriteTime
    else
      FillChar(Result, SizeOf(Result), #0);
  end
end;

function TNamespace.GetLink: Boolean;
begin
  if not (scLink in ShellCache.ShellCacheFlags) then
  begin
    if TestAttributesOf(SFGAO_LINK, False) then
      Include(FShellCache.Data.Attributes, caLink);
    Include(FShellCache.ShellCacheFlags, scLink);
  end;
  Result := caLink in ShellCache.Data.Attributes
end;

function TNamespace.GetNameAddressbar: WideString;
begin
  Result := DisplayNameOf(SHGDN_FORADDRESSBAR or SHGDN_NORMAL)
end;

function TNamespace.GetNameAddressbarInFolder: WideString;
begin
  Result := DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORADDRESSBAR)
end;

function TNamespace.GetNameForEditing: WideString;
begin
  Result := DisplayNameOf(SHGDN_FOREDITING)
end;

function TNamespace.GetNameForEditingInFolder: WideString;
begin
  Result := DisplayNameOf(SHGDN_FOREDITING or SHGDN_INFOLDER)
end;

function TNamespace.GetNameForParsing: WideString;
begin
  // Early versions of Windows returned "Desktop" instead of the full path
  if IsDesktop then
    Result := PhysicalDesktopFolder.NameForParsing
  else
    Result := DisplayNameOf(SHGDN_FORPARSING or SHGDN_NORMAL)
end;

function TNamespace.GetNameForParsingInFolder: WideString;
begin
// Early versions of Windows returned "Desktop" instead of the full path
  if IsDesktop then
    Result := PhysicalDesktopFolder.NameForParsingInFolder
  else
  Result := DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORPARSING)
end;

function TNamespace.GetNameInFolder: WideString;
begin
  if not (scInFolderName in ShellCache.ShellCacheFlags) then
  begin
    FShellCache.Data.InFolderName := DisplayNameOf(SHGDN_INFOLDER);
    Include(FShellCache.ShellCacheFlags, scInFolderName)
  end;
  Result := FShellCache.Data.InFolderName
end;

function TNamespace.GetNameNormal: WideString;
begin
  if not (scNormalName in ShellCache.ShellCacheFlags) then
  begin
    FShellCache.Data.NormalName := DisplayNameOf(SHGDN_NORMAL);
    Include(FShellCache.ShellCacheFlags, scNormalName)
  end;
  Result := FShellCache.Data.NormalName
end;

function TNamespace.GetNameParseAddress: WideString;
begin
  if not (scParsedName in ShellCache.ShellCacheFlags) then
  begin
    FShellCache.Data.ParsedName := DisplayNameOf(SHGDN_FORADDRESSBAR or SHGDN_FORPARSING);
    Include(FShellCache.ShellCacheFlags, scParsedName)
  end;
  Result := FShellCache.Data.ParsedName
end;

function TNamespace.GetNameParseAddressInFolder: WideString;
begin
  Result := DisplayNameOf(SHGDN_FORADDRESSBAR or SHGDN_FORPARSING or SHGDN_INFOLDER)
end;

function TNamespace.GetNewContent: Boolean;
{ GETTER: Does this namespace contain new content?                             }
begin
  Result := TestAttributesOf(SFGAO_NEWCONTENT, False);
end;

function TNamespace.GetNonEnumerated: Boolean;
{ GETTER: Is this namespace able to be enumerated?                              }
begin
  Result := TestAttributesOf(SFGAO_NONENUMERATED, False);
end;

function TNamespace.GetNormal: Boolean;
{ GETTER: Does the file attributes contain Normal?                             }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_NORMAL <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_NORMAL <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetOffLine: Boolean;
{ GETTER: Does the file attributes contain OffLine?                             }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_OFFLINE <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_OFFLINE <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetImage: TBitmap;
begin
 Result := FImage;
end;

procedure TNamespace.InvalidateDetailsOfCache(FlushStrings: Boolean);
var
  i: Integer;
begin
  for i := 0 to Length( ShellCache.Data.DetailsOfCache) - 1 do
  begin
    ShellCache.Data.DetailsOfCache[i].Cached := [];
    if FlushStrings then
      ShellCache.Data.DetailsOfCache[i].Caption := ''
  end;
  FShellCache.ShellCacheFlags := ShellCache.ShellCacheFlags - [scDetailsOfCache];
  FShellCache.ShellCacheFlags := ShellCache.ShellCacheFlags - [scInFolderName];
end;

procedure TNamespace.InvalidateThumbImage;
begin
  FreeAndNIL(FImage);
  Exclude(FStates, nsThreadedImageLoaded);
  Exclude(FStates, nsThreadedImageLoading);
end;

function TNamespace.SubFoldersEx(Flags: Longword = SHCONTF_FOLDERS): Boolean;
begin
  Result := InternalSubItems(Flags)
end;

function TNamespace.SubItemsEx(Flags: Longword = SHCONTF_NONFOLDERS): Boolean;
begin
  Result := InternalSubItems(Flags)
end;

function TNamespace.GetOverlayIconIndex: Integer;
begin
  if Assigned(Parent) then
  begin
    if Assigned(Parent.ShellIconOverlayInterface) then
    begin
      if FShellCache.Data.OverlayIconIndex < 0 then
      begin
        if Parent.ShellIconOverlayInterface.GetOverlayIconIndex(FRelativePIDL, FShellCache.Data.OverlayIconIndex) <> S_OK then
          FShellCache.Data.OverlayIconIndex := -1;
      end
    end
  end;
  Result := FShellCache.Data.OverlayIconIndex
end;

function TNamespace.GetOverlayIndex: Integer;
begin
  if Assigned(Parent) then
  begin
    if not (scOverlayIndex in FShellCache.ShellCacheFlags) then
    begin
      if Assigned(Parent.ShellIconOverlayInterface) then
      begin
        if FShellCache.Data.OverlayIndex < 0 then
        begin
          if Parent.ShellIconOverlayInterface.GetOverlayIndex(FRelativePIDL, FShellCache.Data.OverlayIndex) <> S_OK then
          begin
            if MP_UseSpecialReparsePointOverlay and IsUnicode and not IsWinNT4 and ReparsePoint then
              FShellCache.Data.OverlayIndex := 4
            else
              FShellCache.Data.OverlayIndex := -1;
          end
        end
      end;
      Include(FShellCache.ShellCacheFlags, scOverlayIndex)
    end
  end;
  Result := FShellCache.Data.OverlayIndex;
end;

function TNamespace.GetCanMoniker: Boolean;
begin
  Result := TestAttributesOf(SFGAO_CANMONIKER, False)
end;

function TNamespace.GetEncrypted: Boolean;
begin
  Result := TestAttributesOf(SFGAO_ENCRYPTED, False)
end;

function TNamespace.GetHasStorage: Boolean;
begin
  Result := TestAttributesOf(SFGAO_HASSTORAGE, False)
end;

function TNamespace.GetIsSlow: Boolean;
begin
  Result := TestAttributesOf(SFGAO_ISSLOW, False)
end;

function TNamespace.GetStorage: Boolean;
begin
  Result := TestAttributesOf(SFGAO_STORAGE, False)
end;

function TNamespace.GetStorageAncestor: Boolean;
begin
  Result := TestAttributesOf(SFGAO_STORAGEANCESTOR, False)
end;

function TNamespace.GetStream: Boolean;
begin
  Result := TestAttributesOf(SFGAO_STREAM, False)
end;

function TNamespace.GetParentShellDetailsInterface: IVETShellDetails;
begin
  { This forces the Parent to be created if necessary }
  if Assigned(ParentShellFolder) then
    Result := Parent.ShellDetailsInterface
  else
    Result := ShellDetailsInterface
end;

function TNamespace.GetParentShellFolder: IShellFolder;
begin
  Result  := nil;                                 
  if Assigned(Parent) then
    Result := Parent.ShellFolder
end;

function TNamespace.GetParentShellFolder2: IShellFolder2;
begin
  { This flag keeps us from constantly trying to get FShellFolder2 if it is not }
  { supported by the namespace.                                                 }
  { This forces the Parent to be created if necessary }
  if Assigned(ParentShellFolder) then
    Result := Parent.ShellFolder2
  else
    Result := ShellFolder2
end;

function TNamespace.GetQueryInfoInterface: IQueryInfo;
var
  Found: Boolean;
begin
  if not Assigned(FQueryInfoInterface) then
  begin
    Found := False;
    if Assigned(ParentShellFolder) then
    begin
      Found := ParentShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL,
        IQueryInfo, nil, Pointer(FQueryInfoInterface)) = NOERROR;
    end;
    if not Found and Assigned(ShellFolder) then
    begin
      Found := ShellFolder.CreateViewObject(ParentWnd, IQueryInfo,
        Pointer(FQueryInfoInterface)) = NOERROR;
    end;
    if not Found and IsDesktop then
      FQueryInfoInterface := PhysicalDesktopFolder.QueryInfoInterface;
  end;
  Result := FQueryInfoInterface
end;

function TNamespace.GetReadOnly: Boolean;
{ GETTER: Is this namespace ReadOnly?                                          }
begin
  Result := TestAttributesOf(SFGAO_READONLY, False);
end;

function TNamespace.GetReadOnlyFile: Boolean;
{ GETTER: Does the file attributes contain ReadOnly?                           }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_READONLY <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_READONLY <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetReparsePoint: Boolean;
{ GETTER: Does the file attributes contain ReadOnly?                           }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetRemovable: Boolean;
{  GETTER: Is this a removeable object?                                         }
begin
  Result := TestAttributesOf(SFGAO_REMOVABLE, False);
end;

function TNamespace.GetShellDetailsInterface: IVETShellDetails;
var
  Found: Boolean;
begin
  { This flag keeps us from constantly trying to get IShellDetails if it is not }
  { supported by the namespace.                                                 }
  if (nsShellDetailsSupported in States) and not Assigned(FShellDetailsInterface) then
  begin
    Found := False;
    // IShellDetails not supported in Vista
    if not IsWinVista then
    begin
      try
        if not Found and Assigned(ShellFolder) then
          Found := ShellFolder.CreateViewObject({ParentWnd} 0, IID_IShellDetails, Pointer(FShellDetailsInterface)) = NOERROR;
        if not Found and Assigned(ParentShellFolder) then
          Found := ParentShellFolder.GetUIObjectOf({ParentWnd} 0, 1, FRelativePIDL, IID_IShellDetails, nil,
            Pointer(FShellDetailsInterface)) = NOERROR;
      except
        // Newer NSE will crash if quering for IShellDetails this way so trap them
        // and set the flag to false
        Exclude(FStates, nsShellDetailsSupported);
        FShellDetailsInterface := nil
      end;
    end;

    if Found then
      Include(FStates, nsShellDetailsSupported)
    else
    begin
      Exclude(FStates, nsShellDetailsSupported);
      FShellDetailsInterface := nil
    end
  end;
  Result := FShellDetailsInterface;
end;

function TNamespace.GetShellIconInterface: IShellIcon;
var
  Found: Boolean;
begin
  if not Assigned(FShellIconInterface) then
  begin
    Found := False;
    if Assigned(ParentShellFolder) then
      Found :=  ParentShellFolder.QueryInterface(IID_IShellIcon,
        Pointer(FShellIconInterface)) <> E_NOINTERFACE;
    if not Found then
      FShellIconInterface := nil
  end;
  Result := FShellIconInterface
end;

function TNamespace.GetShellFolder: IShellFolder;
var
  P: PItemIDList;
  ParentFolder, Desktop: IShellFolder;
begin
  if not Assigned(FShellFolder) then
  begin
    SHGetDesktopFolder(Desktop);
    if PIDLMgr.IDCount(AbsolutePIDL) > 1 then
    begin
      P := PIDLMgr.StripLastID(PIDLMgr.CopyPIDL(AbsolutePIDL));
      Desktop.BindToObject(P, nil, IID_IShellFolder, Pointer(ParentFolder));
      PIDLMgr.FreePIDL(P)
    end else
      ParentFolder := Desktop;
      
    if Assigned(ParentFolder) then
    begin
      // 12.9.2007 The call to Folder caused a cascade of GetParent?'s causing delay and unnecesary objects being created
      if IsDesktop or not PIDLIsFolder(AbsolutePIDL) then
        FShellFolder := ParentFolder
      else begin
        if not Succeeded(ParentFolder.BindToObject(FRelativePIDL, nil, IID_IShellFolder, Pointer(FShellFolder))) then
          FShellFolder := nil
      end
    end else
      FShellFolder := nil
  end;
  Result := FShellFolder
end;

function TNamespace.GetShellFolder2: IShellFolder2;
begin
  { This flag keeps us from constantly trying to get FShellFolder2 if it is not }
  { supported by the namespace.                                                 }
  if (nsShellFolder2Supported in States) and not Assigned(FShellFolder2) then
  begin
    if Assigned(ShellFolder) then
      if ShellFolder.QueryInterface(IID_IShellFolder2, Pointer(FShellFolder2)) = E_NOINTERFACE
    then begin
      FShellFolder2 := nil;
      Exclude(FStates, nsShellFolder2Supported)
    end else
      Include(FStates, nsShellFolder2Supported)
  end;
  Result := FShellFolder2;
end;

function TNamespace.GetShellLink: TVirtualShellLink;
begin
  if Link then
  begin
    if not Assigned(FShellLink) then
    begin
      FShellLink := TVirtualShellLink.Create(nil);
      FShellLink.ReadLink(NameParseAddress);
    end
  end;
  Result := FShellLink
end;

function TNamespace.GetSizeOfFile: WideString;
{ GETTER: Get the size of the file in string format}
begin
  if not (scFileSize in ShellCache.ShellCacheFlags) then
  begin
    if (not Folder or Browsable) and FileSystem then
    begin
      FShellCache.Data.FileSize := Format('%0.0n', [SizeOfFileInt64 + 0.0]);
  //    FShellCache.Data.FileSize := AddCommas(WideIntToStr(SizeOfFileInt64));
      Include(FShellCache.ShellCacheFlags, scFileSize);
    end else
      FShellCache.Data.FileSize := ''
  end;
  Result := ShellCache.Data.FileSize
end;

function TNamespace.GetSizeOfFileDiskUsage: WideString;
var
  Size, BytesPerCluster: Int64;
  Drive: string;
  DriveW: WideString;
  SectorsPerCluster,
  BytesPerSector,
  FreeClusters,
  TotalClusters,
  i : DWORD;
  ValidData: Boolean;
begin
  if (not Folder or Browsable) and FileSystem then
  begin
    Size := SizeOfFileInt64;
    DriveW := WideExtractFileDrive(Self.NameForParsing) + '\';
    Drive := DriveW;
    if WideDirectoryExists(Drive) then
    begin
      ValidData := GetDiskFreeSpaceMP(PWideChar( DriveW), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters);

      if ValidData then
      begin
        BytesPerCluster := Int64(SectorsPerCluster) * Int64(BytesPerSector);
        if BytesPerCluster <> 0 then
        begin
          { In the *rare* instance where the actual size is equal to multiple of  }
          { the sector size don't do the math :^)                                 }
          if Size mod BytesPerCluster <> 0 then
            i := 1
          else
            i := 0;
            Result := Format('%0.0n', [Int64(BytesPerCluster) *(Size div Int64(BytesPerCluster) + i) + 0.0])
        end else
          Result := SizeOfFile
      end else
        Result := SizeOfFile
    end else
      Result := SizeOfFile
  end;
end;

function TNamespace.GetSizeOfFileInt64: Int64;
var
  H: THandle;
  FindDataW: TWin32FindDataW;
  FindDataA: TWin32FindDataA;
{ GETTER: Get the file size in bytes.                                           }
// The PIDL does not store the file size for > 4G files, need to use FindFirstFile
begin
  if not (scFileSizeInt64 in ShellCache.ShellCacheFlags) then
  begin
    if (not Folder or Browsable) and FileSystem then
    begin
      if IsUnicode then
      begin
        H := FindFirstFileW_MP(PWideChar(NameForParsing), FindDataW);
        if H <> INVALID_HANDLE_VALUE then
        begin
          Windows.FindClose(H);
          FShellCache.Data.FileSizeInt64 := FindDataW.nFileSizeLow;
          if FShellCache.Data.FileSizeInt64 < 0 then
            FShellCache.Data.FileSizeInt64 := FShellCache.Data.FileSizeInt64 + 4294967296;
          if FindDataW.nFileSizeHigh > 0 then
              FShellCache.Data.FileSizeInt64 := FShellCache.Data.FileSizeInt64 + (FindDataW.nFileSizeHigh * 4294967296)
        end
      end else
      begin
        H := FindFirstFileA(PAnsiChar(AnsiString(NameForParsing)), FindDataA);
        if H <> INVALID_HANDLE_VALUE then
        begin
          Windows.FindClose(H);
          FShellCache.Data.FileSizeInt64 := FindDataA.nFileSizeLow;
          if FShellCache.Data.FileSizeInt64 < 0 then
            FShellCache.Data.FileSizeInt64 := FShellCache.Data.FileSizeInt64 + 4294967296;
          if FindDataA.nFileSizeHigh > 0 then
            FShellCache.Data.FileSizeInt64 := FShellCache.Data.FileSizeInt64 + (FindDataA.nFileSizeHigh * 4294967296)
        end
      end
    end;
    Include(FShellCache.ShellCacheFlags, scFileSizeInt64)
  end;
  Result := FShellCache.Data.FileSizeInt64
end;

function TNamespace.GetSizeOfFileKB: WideString;
{ GETTER: Get the file  size in Explorer KiloByte format.                       }
begin
  if not (scFileSizeKB in ShellCache.ShellCacheFlags) then
  begin
    if ((not Folder or Browsable) and FileSystem) then
    begin
      if SizeOfFileInt64 > 0 then
      begin
        FShellCache.Data.FileSizeKB := Format('%0.0n '+ 'KB', [SizeOfFileInt64/1024]);
        if (FShellCache.Data.FileSizeKB = STR_ZERO_KB) then
          FShellCache.Data.FileSizeKB := STR_ONE_KB;
      end else
        FShellCache.Data.FileSizeKB := STR_ONE_KB;
      Include(FShellCache.ShellCacheFlags, scFileSizeKB)
    end else
      FShellCache.Data.FileSizeKB := '';
  end;
  Result := FShellCache.Data.FileSizeKB;
end;

function TNamespace.GetSparseFile: Boolean;
{ GETTER: Does the file attributes contain ReadOnly?                           }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetShare: Boolean;
begin
  Result := TestAttributesOf(SFGAO_SHARE, False);
end;

procedure TNamespace.GetSHFileInfo;
{ Retrieves and caches the some information about the namespace with            }
{ ShGetFileInfo.                                                                }
var
  InfoA: TSHFileInfoA;
  InfoW: TSHFileInfoW;
begin
  if not Assigned(FSHGetFileInfoRec) then
  begin
    if IsUnicode then
    begin
      GetMem(FSHGetFileInfoRec, SizeOf(FSHGetFileInfoRec^));
      Initialize(FSHGetFileInfoRec^.FileType);
      if Assigned(FSHGetFileInfoRec) then
      begin
        SHGetFileInfoW_MP(PWideChar(AbsolutePIDL), 0, InfoW, SizeOf(InfoW), SHGFI_TYPENAME or SHGFI_PIDL);
        FSHGetFileInfoRec^.FileType := InfoW.szTypeName;
        { NT only half-assed supports the SHGetFileInfo...only if the ext is      }
        { associated with a program. So we build it ourselves                     }
        if FSHGetFileInfoRec^.FileType = '' then
          FSHGetFileInfoRec^.FileType := WideUpperCase(WideExtractFileExt(NameForParsing)) + STR_FILE;
      end
    end else
    begin
      GetMem(FSHGetFileInfoRec, SizeOf(FSHGetFileInfoRec^));
      Initialize(FSHGetFileInfoRec^.FileType);
      if Assigned(FSHGetFileInfoRec) then
      begin
        SHGetFileInfoA(PAnsiChar(AbsolutePIDL), 0, InfoA, SizeOf(InfoA), SHGFI_TYPENAME or SHGFI_PIDL);
        FSHGetFileInfoRec^.FileType := InfoA.szTypeName;
        { NT only half-assed supports the SHGetFileInfo...only if the ext is      }
        { associated with a program. So we build it ourselves                     }
        if FSHGetFileInfoRec^.FileType = '' then
          FSHGetFileInfoRec^.FileType := WideUpperCase(WideExtractFileExt(NameForParsing)) + STR_FILE;
      end
    end;
  end;
end;

function TNamespace.GetShortFileName: WideString;
{ GETTER: Get the 8:3 short file name (DOS)                                     }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
    begin
      Result := FWin32FindDataW^.cAlternateFileName;
      if Result = '' then
        Result := WideUpperCase(FWin32FindDataW^.CFileName)
    end else
      Result := '';
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
    begin
      Result := FWin32FindDataA^.cAlternateFileName;
      if Result = '' then
        Result := WideUpperCase(FWin32FindDataA^.CFileName)
    end else
      Result := '';
  end
end;

function TNamespace.GetSubFolders: Boolean;
{ Tests to see if a namespace is a true folder and has at least one             }
{ sub-namespace within it.                                                      }
begin
  Result := InternalSubItems(SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN)
end;

function TNamespace.GetSubItems: Boolean;
{ Tests to see if a namespace is a true folder and has at least one             }
{ sub-namespace within it.                                                      }
begin
  Result := InternalSubItems(SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN)
end;

function TNamespace.GetSystem: Boolean;
{ GETTER: Does the file attributes contain System?                             }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM <> 0
    else
      Result := False;
  end else
  begin
if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetTemporary: Boolean;
{ GETTER: Does the file attributes contain Temporary?                           }
begin
  if IsUnicode then
  begin
    if not Assigned(FWin32FindDataW) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataW) and FileSystem then
      Result := FWin32FindDataW^.dwFileAttributes and FILE_ATTRIBUTE_TEMPORARY <> 0
    else
      Result := False;
  end else
  begin
    if not Assigned(FWin32FindDataA) then
      GetDataFromIDList;
    if Assigned(FWin32FindDataA) and FileSystem then
      Result := FWin32FindDataA^.dwFileAttributes and FILE_ATTRIBUTE_TEMPORARY <> 0
    else
      Result := False;
  end
end;

function TNamespace.GetThreadedIconLoaded: Boolean;
begin
  Result := nsThreadedIconLoaded in States
end;

function TNamespace.GetThreadIconLoading: Boolean;
begin
  Result := nsThreadedIconLoading in States
end;

function TNamespace.GetShellIconOverlayInterface: IShellIconOverlay;
var
  Found: Boolean;
begin
  if (nsShellOverlaySupported in States) and not Assigned(FShellIconOverlayInterface) then
  begin
    Found := False;
    if Assigned(ShellFolder) then
      Found := ShellFolder.QueryInterface(IShellIconOverlay,
        Pointer(FShellIconOverlayInterface)) <> E_NOINTERFACE;
    if not Found then         // Here we have to check again
    begin
      IF Assigned(ParentShellFolder) then
        Found :=  ParentShellFolder.QueryInterface(IShellIconOverlay,
          Pointer(FShellIconOverlayInterface)) <> E_NOINTERFACE;
    end;
    if not Found then
    begin
      Exclude(FStates, nsShellOverlaySupported);
      FShellIconOverlayInterface := nil
    end
  end;
  Result := FShellIconOverlayInterface
end;

procedure TNamespace.HandleContextMenuMsg(Msg, wParam, lParam: Longint; var Result: LRESULT);
{ This is called when the ContextMenu calls back to its owner window to ask     }
{ questions to implement the addition of icons to the menu.  The messages sent  }
{ to the owner window are:  WM_INITMENUPOPUP, WM_DRAWITEM, or WM_MEASUREITEM.   }
{ Which must be passed on to the ContextMenu2 interface to display items with   }
{ icons.                                                                        }
var
  ContextMenu3: IContextMenu3;
begin
  if Assigned(CurrentContextMenu2) then
    if CurrentContextMenu2.QueryInterface(IContextMenu3, ContextMenu3) <> E_NOINTERFACE then
      ContextMenu3.HandleMenuMsg2(Msg, wParam, lParam, Result)
    else
      CurrentContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
end;


function TNamespace.InjectCustomSubMenu(Menu: HMenu; Caption: string; PopupMenu: TPopupMenu;
  var SubMenu: HMenu): TMenuItemIDArray;
const
  MENUMASK = MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_TYPE;


{ Searchs through the passed menu looking for an item identifer that is not   }
{ currently being used.                                                       }

  function FindUniqueMenuID(AMenu: HMenu; StartID: cardinal): cardinal;
  var
    ItemCount, i: integer;
    Duplicate, Done: Boolean;
  begin
    ItemCount := GetMenuItemCount(Menu);
    Duplicate := True;
    Result := StartID;
    while Duplicate do
    begin
      i := 0;
      Done := False;
      while (i < ItemCount) and not Done do
      begin
        Done := GetMenuItemID(Menu, i) = Result;
        Inc(i);
      end;
      Duplicate := Done;
      if Duplicate then
        Inc(Result)
    end;
  end;

var
  ItemCount, i: integer;
  ItemInfoA: TMenuItemInfoA;
  ItemInfoW: TMenuItemInfoW;
  LastID: cardinal;
begin
  Result := nil;
  SubMenu := 0;
  LastID := 0;
  ItemCount := GetMenuItemCount(Menu);
  SubMenu := CreatePopupMenu;

  if IsUnicode then
  begin
    FillChar(ItemInfoW, SizeOf(ItemInfoW), #0);
    ItemInfoW.cbSize := SizeOf(ItemInfoW);
    ItemInfoW.fmask := MIIM_TYPE;
    ItemInfoW.fType := MFT_SEPARATOR;
    InsertMenuItemW(Menu, ItemCount, True, ItemInfoW);

    FillChar(ItemInfoW, SizeOf(ItemInfoW), #0);
    ItemInfoW.cbSize := SizeOf(ItemInfoW);
    ItemInfoW.fmask := MIIM_SUBMENU or MIIM_TYPE;
    ItemInfoW.hSubMenu := SubMenu;
    ItemInfoW.dwTypeData := PWideChar( WideString(Caption));
    // Insert the Root Menu Item
    if InsertMenuItemW(Menu, ItemCount + 1, True, ItemInfoW) then
    begin
      SetLength(Result, PopupMenu.Items.Count);

      for i := PopupMenu.Items.Count - 1 downto 0 do
      begin
        FillChar(ItemInfoW, SizeOf(ItemInfoW), #0);

        ItemInfoW.cbSize := SizeOf(ItemInfoW);

        ItemInfoW.fmask := MENUMASK;

        if PopupMenu.Items[i].Caption <> '-' then
          ItemInfoW.fType := MFT_STRING
        else
          ItemInfoW.fType := MFT_SEPARATOR;


        if PopupMenu.Items[i].RadioItem then
          ItemInfoW.fType := ItemInfoW.fType or MFT_RADIOCHECK;
        if PopupMenu.BiDiMode = bdRightToLeft then
          ItemInfoW.fType := ItemInfoW.fType or MFT_RIGHTJUSTIFY;
        if PopupMenu.Items[i].Break = mbBreak then
          ItemInfoW.fType := ItemInfoW.fType or MFT_MENUBREAK;
        if PopupMenu.Items[i].Break = mbBarBreak then
          ItemInfoW.fType := ItemInfoW.fType or MFT_MENUBARBREAK;

        if PopupMenu.Items[i].Checked then
          ItemInfoW.fState := ItemInfoW.fState or MFS_CHECKED
        else
          ItemInfoW.fState := ItemInfoW.fState or MFS_UNCHECKED;
        if PopupMenu.Items[i].Default then
          ItemInfoW.fState := ItemInfoW.fState or MFS_DEFAULT;
        if PopupMenu.Items[i].Enabled then
          ItemInfoW.fState := ItemInfoW.fState or MFS_ENABLED
        else
          ItemInfoW.fState := ItemInfoW.fState or MFS_DISABLED;

        ItemInfoW.wID := FindUniqueMenuID(Menu, LastID + 1);
        LastID := ItemInfoW.wID;
        Result[i] := ItemInfoW.wID;

        // Store the TMenuItem so we can get it later
        ItemInfoW.dwItemData := Cardinal( PopupMenu.Items[i]);

        if not( ItemInfoW.fType and MFT_SEPARATOR <> 0) then
          ItemInfoW.dwTypeData := PWideChar( WideString(PopupMenu.Items[i].Caption));

        InsertMenuItemW(SubMenu, 0, True, ItemInfoW)
      end
    end
  end else
  begin
    FillChar(ItemInfoA, SizeOf(ItemInfoA), #0);
    ItemInfoA.cbSize := SizeOf(ItemInfoA);
    ItemInfoA.fmask := MIIM_TYPE;
    ItemInfoA.fType := MFT_SEPARATOR;
    InsertMenuItemA(Menu, ItemCount, True, ItemInfoA);

    FillChar(ItemInfoA, SizeOf(ItemInfoA), #0);
    ItemInfoA.cbSize := SizeOf(ItemInfoA);
    ItemInfoA.fmask := MIIM_SUBMENU or MIIM_TYPE;
    ItemInfoA.hSubMenu := SubMenu;
    ItemInfoA.dwTypeData := PAnsiChar( AnsiString(Caption));
    // Insert the Root Menu Item
    if InsertMenuItemA(Menu, ItemCount + 1, True, ItemInfoA) then
    begin
      SetLength(Result, PopupMenu.Items.Count);

      for i := PopupMenu.Items.Count - 1 downto 0 do
      begin
        FillChar(ItemInfoA, SizeOf(ItemInfoA), #0);

        ItemInfoA.cbSize := SizeOf(ItemInfoA);

        ItemInfoA.fmask := MENUMASK;

        if PopupMenu.Items[i].Caption <> '-' then
          ItemInfoA.fType := MFT_STRING
        else
          ItemInfoA.fType := MFT_SEPARATOR;


        if PopupMenu.Items[i].RadioItem then
          ItemInfoA.fType := ItemInfoA.fType or MFT_RADIOCHECK;
        if PopupMenu.BiDiMode = bdRightToLeft then
          ItemInfoA.fType := ItemInfoA.fType or MFT_RIGHTJUSTIFY;
        if PopupMenu.Items[i].Break = mbBreak then
          ItemInfoA.fType := ItemInfoA.fType or MFT_MENUBREAK;
        if PopupMenu.Items[i].Break = mbBarBreak then
          ItemInfoA.fType := ItemInfoA.fType or MFT_MENUBARBREAK;

        if PopupMenu.Items[i].Checked then
          ItemInfoA.fState := ItemInfoA.fState or MFS_CHECKED
        else
          ItemInfoA.fState := ItemInfoA.fState or MFS_UNCHECKED;
        if PopupMenu.Items[i].Default then
          ItemInfoA.fState := ItemInfoA.fState or MFS_DEFAULT;
        if PopupMenu.Items[i].Enabled then
          ItemInfoA.fState := ItemInfoA.fState or MFS_ENABLED
        else
          ItemInfoA.fState := ItemInfoA.fState or MFS_DISABLED;

        ItemInfoA.wID := FindUniqueMenuID(Menu, LastID + 1);
        LastID := ItemInfoA.wID;
        Result[i] := ItemInfoA.wID;

        // Store the TMenuItem so we can get it later
        ItemInfoA.dwItemData := Cardinal( PopupMenu.Items[i]);

        if not( ItemInfoA.fType and MFT_SEPARATOR <> 0) then
          ItemInfoA.dwTypeData := PAnsiChar( AnsiString(PopupMenu.Items[i].Caption));

        InsertMenuItemA(SubMenu, 0, True, ItemInfoA)
      end
    end
  end
end;

function TNamespace.InternalGetContextMenuInterface(PIDLArray: TRelativePIDLArray): IContextMenu;
var
  Found: Boolean;
begin
  Found := False;
  CurrentContextMenu2 := nil;  // Clear since not sure if it is avaiable yet
  if Assigned(PIDLArray) then
  begin
    if Assigned(ParentShellFolder)  then
    begin
      Found := Succeeded(ParentShellFolder.GetUIObjectOf(ParentWnd,
        Length(PIDLArray), PItemIDList( PIDLArray[0]),
        IID_IContextMenu, nil, Pointer(Result)))
    end;
    if not Found and Assigned(ShellFolder) and (Length(PIDLArray) = 1) then
    begin
      Found := ShellFolder.CreateViewObject(ParentWnd, IID_IContextMenu,
        Pointer(Result)) = NOERROR;
    end;
    if not Found then
      Result := nil
  end else
    Result := nil
end;

function TNamespace.InternalGetDataObjectInterface(PIDLArray: TRelativePIDLArray): IDataObject;
{ Creates an IDataObject using the passed relative PIDLs (actually siblings of }
{ the TNamespace) If a nil is passed for PIDLArray a single object based on    }
{ TNamespace is created.                                                       }
var
  Found: Boolean;
begin
  if not Assigned(PIDLArray) then
  begin
    SetLength(PIDLArray, 1);
    PIDLArray[0] := RelativePIDL
  end;
  Found := False;
  if Assigned(PIDLArray) then
  begin
    if Assigned(ParentShellFolder)  then
    begin
      Found := ParentShellFolder.GetUIObjectOf(ParentWnd,
        Length(PIDLArray), PItemIDList( PIDLArray[0]),
        IDataObject, nil, Pointer(Result)) = NOERROR;
    end;
    if not Found and Assigned(ShellFolder) and (Length(PIDLArray) = 1) then
    begin
      Found := ShellFolder.CreateViewObject(ParentWnd, IDataObject,
        Pointer(Result)) = NOERROR;
    end;
    if not Found then
      Result := nil
  end else
    Result := nil
end;

function TNamespace.InternalShowContextMenu(Owner: TWinControl;
  ContextMenuCmdCallback: TContextMenuCmdCallback;
  ContextMenuShowCallback: TContextMenuShowCallback;
  ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback;
  PIDLArray: TRelativePIDLArray; Position: PPoint; CustomShellSubMenu: TPopupMenu;
  CustomSubMenuCaption: WideString): Boolean;
// Displays the ContextMenu of the namespace.
const
  MaxVerbLen = 128;
var
  Menu: hMenu;
  InvokeInfo: TCMInvokeCommandInfoEx;
  MenuCmd: Cardinal;
  x, y, i: integer;
  OldErrorMode: integer;
  VerbA: AnsiString;
  VerbW: WideString;
  GenericVerb: Pointer;
  Handled, AllowShow: Boolean;
  Flags: Longword;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  ContextMenu3: IContextMenu3;
  MenuIDs: TMenuItemIDArray;
  ItemInfo: TMenuItemInfo;
  SubMenu: HMenu;
  OldMode: UINT;
  ShiftDown, ControlDown: Boolean;
begin
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    MenuIDs := nil;
    Result := False;
    Assert(Assigned(Owner), 'To show a Context Menu using TNamespace you must pass a valid Owner TWinControl');
    if Assigned(Owner) then
    begin
      ShiftDown := GetKeyState(VK_SHIFT) and $8000 <> 0;
      ControlDown := GetKeyState(VK_CONTROL) and $8000 <> 0;
      try
        if Assigned(PIDLArray) then
        begin
          ContextMenu := nil;
          ContextMenu2 := nil;
          ContextMenu3 := nil;
          Result := False;
          if Assigned(Position) then
          begin
            x := Position.x;
            y := Position.y
          end else
          begin
            x := Mouse.CursorPos.X;  // Snag these fast. The mouse can move a fair amount
            y := Mouse.CursorPos.Y;  // before the popup menu is shown.
          end;
          FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);
          Menu := CreatePopupMenu;
          OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
          try
            { The application must handle a rename, rename makes no sense for more than 1 item }
            if Assigned(ContextMenuCmdCallback) and (Length(PIDLArray) = 1) then
              Flags :=  CMF_CANRENAME or CMF_NORMAL or CMF_EXPLORE
            else
              Flags := CMF_NORMAL or CMF_EXPLORE;

            if ShiftDown then
              Flags := Flags or CMF_EXTENDEDVERBS;

            if Assigned(PIDLArray) then
              ContextMenu := InternalGetContextMenuInterface(PIDLArray)
            else
              ContextMenu := ContextMenuInterface;

            CurrentContextMenu := ContextMenu;

            CurrentContextMenu2 := nil;  // not sure it is available yet
            if Assigned(ContextMenu) then
            begin
              if ContextMenu.QueryInterface(IContextMenu3, Pointer(ContextMenu3)) = E_NOINTERFACE then
              begin
                if ContextMenu.QueryInterface(IID_IContextMenu2, Pointer(ContextMenu2)) <> E_NOINTERFACE then
                  CurrentContextMenu2 := ContextMenu2;
              end else
                CurrentContextMenu2 := ContextMenu3;

              if Assigned(ContextMenu3) then
                ContextMenu3.QueryContextMenu(Menu, 0, 1, $7FFF, Flags)
              else
              if Assigned(ContextMenu2) then
                ContextMenu2.QueryContextMenu(Menu, 0, 1, $7FFF, Flags)
              else
              if Assigned(ContextMenu) then
                ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, Flags);

              // Inject our custom menu item
              if Assigned(CustomShellSubMenu) then
                MenuIDs := InjectCustomSubMenu(Menu, CustomSubMenuCaption, CustomShellSubMenu, SubMenu);

              AllowShow := True;
              if Assigned(ContextMenuShowCallback) then
                ContextMenuShowCallback(Self, Menu, AllowShow);

              if AllowShow then
              begin
                FOldWndProcForContextMenu := Owner.WindowProc;
                try
                  // Hook the owner for the Window message for owner draw menus like
                  // Send To..
                  Owner.WindowProc := WindowProcForContextMenu;
                  MenuCmd := Cardinal( TrackPopupMenuEx(
                                       Menu,
                                       TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON,
                                       x, y, Owner.Handle, nil))
                finally
                  // Try it again to see if the user pressed it while the menu was shown
                  ShiftDown := GetKeyState(VK_SHIFT) and $8000 <> 0;
                  ControlDown := GetKeyState(VK_CONTROL) and $8000 <> 0;
                  Owner.WindowProc := FOldWndProcForContextMenu;
                  FOldWndProcForContextMenu := nil;
                end
              end else
                MenuCmd := 0;      

              if MenuCmd <> 0 then
              begin
                if IsUnicode then
                begin
                  SetLength(VerbW, MaxVerbLen);
                  FillChar(VerbW[1], MaxVerbLen*2, #0);
                  GenericVerb := @VerbW[1];
                  Flags := GCS_VERBW
                end else
                begin
                  SetLength(VerbA, MaxVerbLen);
                  FillChar(VerbA[1], MaxVerbLen, #0);
                  GenericVerb := @VerbA[1];
                  Flags := GCS_VERBA
                end;
                if Assigned(ContextMenu3) then
                  Result := Succeeded(ContextMenu3.GetCommandString(MenuCmd-1, Flags, nil, GenericVerb, MaxVerbLen))
                else
                if Assigned(ContextMenu2) then
                  Result := Succeeded(ContextMenu2.GetCommandString(MenuCmd-1, Flags, nil, GenericVerb, MaxVerbLen))
                else
                if Assigned(ContextMenu) then
                  Result := Succeeded(ContextMenu.GetCommandString(MenuCmd-1, Flags, nil, GenericVerb, MaxVerbLen));

                if IsUnicode then
                  SetLength(VerbW, lstrlenW(PWideChar( VerbW)))
                else begin
                  SetLength(VerbA, StrLen(PAnsiChar( VerbA)));
                  VerbW := VerbA
                end;

                if not Result then
                  VerbW := STR_UNKNOWNCOMMAN;

                Handled := False;
                // See if it is one of our injected menu items and call the Click event
                for i := 0 to Length(MenuIDs) - 1 do
                begin
                  if MenuCmd = MenuIDs[i] then
                  begin
                    if SubMenu <> 0 then
                    begin
                      Handled := True;
                      FillChar(ItemInfo, SizeOf(ItemInfo), #0);
                      ItemInfo.cbSize := SizeOf(TMenuItemInfo);
                      ItemInfo.fMask := MIIM_DATA;
                      GetMenuItemInfo(SubMenu, i, True, ItemInfo);
                      if ItemInfo.dwItemData <> 0 then
                        TMenuItem(ItemInfo.dwItemData).Click
                    end
                  end
                end;

                if Assigned(ContextMenuCmdCallback) then
                  ContextMenuCmdCallBack(Self, VerbW, MenuCmd, Handled);

                if not Handled then
                begin      
                  FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);
                  with InvokeInfo do
                  begin
                    { For some reason the lpVerbW won't work }
                    lpVerb := MakeIntResourceA(MenuCmd-1);
                    if IsUnicode then
                    begin
                      fMask := CMIC_MASK_UNICODE;
                      lpVerbW := MakeIntResourceW(MenuCmd-1)
                    end;
                    // Win95 get confused if size = TCMInvokeCommandInfoEx
                    if IsUnicode then
                      cbSize := SizeOf(TCMInvokeCommandInfoEx)
                    else
                      cbSize := SizeOf(TCMInvokeCommandInfo);

                    hWnd := Owner.Handle;
                    nShow := SW_SHOWNORMAL;
                  end;

                  InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_ASYNCOK;

                  if ShiftDown then
                    InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_SHIFT_DOWN;
                  if ControlDown then
                    InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_CONTROL_DOWN;

                  if Assigned(ContextMenu3) then
                    Result := Succeeded(ContextMenu3.InvokeCommand(InvokeInfo))
                  else
                  if Assigned(ContextMenu2) then
                    Result := Succeeded(ContextMenu2.InvokeCommand(InvokeInfo))
                  else
                  if Assigned(ContextMenu) then
                    Result := Succeeded(ContextMenu.InvokeCommand(InvokeInfo));
                end
              end;
              if Assigned(ContextMenuAfterCmdCallback) then
                ContextMenuAfterCmdCallback(Self, VerbW, MenuCmd, Result);
            end;
          finally
            { Don't access any properties or field of the object.  If the verb is     }
            { 'delete' the component using this class could have freed the instance   }
            { of the object through a ShellNotifyRegister or some other way.          }
            DestroyMenu(Menu);
            SetErrorMode(OldErrorMode);
          end
        end
      finally
        // Don't nil until after the hook is unset
        ContextMenu := nil;
        ContextMenu2 := nil;
        ContextMenu3 := nil;
        CurrentContextMenu := nil;
        CurrentContextMenu2 := nil;  // not sure it is available yet
      end
    end
  finally
    SetErrorMode(OldMode)
  end
end;

function TNamespace.OkToBrowse(ShowExplorerMsg: Boolean): Boolean;
var
  S: WideString;
begin
  Result := True;
  S := NameForParsing;
  if ((Length(S) = 3) and (S[2] = ':') and (S[3] = '\')) then
    Result := DiskInDrive(AnsiChar(AnsiString( S)[1]));
  if not Result then
  begin
    if ShowExplorerMsg then
      EnumerateFolderEx(ParentWnd, [foFolders, foNonFolders, foHidden], nil, nil)
    else
      EnumerateFolderEx(0, [foFolders, foNonFolders, foHidden], nil, nil)
  end
end;

function TNamespace.InternalSubItems(Flags: Longword): Boolean;
{ Tests to see if a namespace is a true folder and has at least one             }
{ sub-namespace within it.                                                      }
var
  Enum: IEnumIDList;
  Fetched: Longword;
  Item: PItemIDList;
  OldError: DWORD;
begin
  Result := False;
  OldError := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    { The recycle bin enumerates slow if it is full.  VT will InitNode for various }
    { reasons eventhough the node is not expanded.  We will always assume there is }
    { is something in the bin.  If not when it is clicked it will clear the "+"    }
    if IsRecycleBin then
      Result := True
    else 
   // if ILIsParent(HistoryFolder.AbsolutePIDL, AbsolutePIDL, False) then
   //   Result := True
   // else
    begin
      if Folder and Assigned(ShellFolder) then
      begin
        Item := nil;
        Enum := nil;
        // Since this is just an internal test don't show any dialogs so send a 0
        if ShellFolder.EnumObjects(0, Flags, Enum) = NOERROR then
        begin
          // Vista Enum is nil every once in a while
          if Assigned(Enum) then
          begin
            Result := Enum.Next(1, Item, Fetched) = NOERROR;
            if Assigned(Item) then
              PIDLMgr.FreePIDL(Item)
          end
        end
      end
    end
   finally
     SetErrorMode(OldError);
   end
end;

procedure TNamespace.InvalidateCache;
{ Forces the class to reload any cached data the next time it is retrieved.     }
begin
  FShellCache.Data.InFolderName := '';
  FShellCache.Data.NormalName := '';
  FShellCache.Data.ParsedName := '';
  FShellCache.Data.SmallIcon := IconCache;
  FShellCache.Data.OverlayIndex := -1;
  FShellCache.Data.OverlayIconIndex := -1;
  FShellCache.Data.CreationTime := '';
  FShellCache.Data.LastAccessTime := '';
  FShellCache.Data.LastWriteTime := '';
  FShellCache.Data.FileSize := '';
  FShellCache.Data.FileSizeKB := '';
  FShellCache.Data.FileType := '';
  FShellCache.Data.FileSizeInt64 := 0;
  FShellCache.Data.SupportedColumns := 0;
  FShellCache.Data.Attributes := [];
  SetLength(FShellCache.Data.DetailsOfCache, 0);
  FShellCache.ShellCacheFlags := [];
  FreeAndNil(FExtractImage);
  FreeAndNil(FShellLink);
  InvalidateThumbImage
end;

procedure TNamespace.InvalidateNamespace(RefreshIcon: Boolean = True);
var
  Icon1, Icon2: integer;
  Icon1Initialized, Icon2Initialized: Boolean;
begin
  Icon1 := 0;
  Icon2 := 0;
  Icon1Initialized := False;
  Icon2Initialized := False;
  if not RefreshIcon then
  begin
    if scSmallIcon in FShellCache.ShellCacheFlags then
    begin
      Icon1Initialized := True;
      Icon1 := FShellCache.Data.SmallIcon;
    end;
    if scSmallOpenIcon in FShellCache.ShellCacheFlags then
    begin
      Icon2Initialized := True;
      Icon2 := FShellCache.Data.SmallOpenIcon;
    end;
  end else
  begin
    // Flush the thread state so the icon is reloaded by the thread
    States := States - [nsThreadedIconLoading];
    States := States - [nsThreadedIconLoaded];
  end;

  InvalidateDetailsOfCache(True);
  InvalidateCache;
  if nsOwnsParent in States then
    FreeAndNil(FParent);
  FShellDetailsInterface := nil;
  FShellIconOverlayInterface := nil;
  FShellFolder := nil;
  if Assigned(Parent) then
    if Parent.IsDesktop then
      PhysicalDesktopFolder.InvalidateNamespace;
  if IsUnicode then
  begin
    if Assigned(FWin32FindDataW) then
      FreeMem(FWin32FindDataW, SizeOf(TWin32FindDataW));
    FWin32FindDataW := nil;
  end else
  begin
    if Assigned(FWin32FindDataA) then
      FreeMem(FWin32FindDataA, SizeOf(TWin32FindDataA));
    FWin32FindDataA := nil;
  end;
  if Assigned(FSHGetFileInfoRec) then
  begin
    Finalize(FSHGetFileInfoRec^);
    FreeMem(FSHGetFileInfoRec, SizeOf(TSHGetFileInfoRec));
  end;
  FSHGetFileInfoRec := nil;
  Include(FStates, nsShellDetailsSupported);  // Be optomistic
  Include(FStates, nsShellFolder2Supported);  // Be optomistic
  FQueryInfoInterface := nil;
  FShellIconInterface := nil;
  FCurrentContextMenu2 := nil;

  if not RefreshIcon then
  begin
    if Icon1Initialized then
    begin
      Include(FShellCache.ShellCacheFlags, scSmallIcon);
      FShellCache.Data.SmallIcon := Icon1;
    end;
    if Icon2Initialized then
    begin
      Include(FShellCache.ShellCacheFlags, scSmallOpenIcon);
      FShellCache.Data.SmallOpenIcon := Icon2;
    end;
  end;
end;

procedure TNamespace.InvalidateRelativePIDL(FileObjects: TFileObjects);
var
  Enum: IEnumIDList;
  Flags: Longword;
  Fetched: Longword;
  Item: PItemIDList;
  Done: Boolean;
begin
  if Assigned(ParentShellFolder) then
  begin
    ValidateFileObjects(FileObjects);
    Flags := FileObjectsToFlags(FileObjects);
    Done := False;
    if ParentShellFolder.EnumObjects(ParentWnd, Flags, Enum) = NOERROR then
    begin
      // Vista Enum is nil every once in a while
      if Assigned(Enum) then
      begin
        while (Enum.Next(1, Item, Fetched) = NOERROR) and not Done do
        begin
          if ComparePIDL(Item, False) = 0 then
          begin
            PIDLMgr.FreePIDL(FAbsolutePIDL);
            FAbsolutePIDL := PIDLMgr.AppendPIDL(Parent.AbsolutePIDL, Item);
            FRelativePIDL := PIDLMgr.GetPointerToLastID(FAbsolutePIDL);
            InvalidateNamespace;
            Done := True
          end;
          PIDLMgr.FreePIDL(Item)
        end
      end
    end
  end
end;

function TNamespace.IsChildByNamespace(TestNamespace: TNamespace;
  Immediate: Boolean): Boolean;
{ Returns True if the TestNamespace is a child of the namespace.  Immediate     }
{ forces function to be true only of the passed PIDL is the immidiate child     }
{ of the namespace.                                                             }
begin
  Result := Boolean( ILIsParent(FAbsolutePIDL, TestNamespace.FAbsolutePIDL, Immediate));
end;

function TNamespace.IsChildByPIDL(TestPIDL: PItemIDList;
  Immediate: Boolean): Boolean;
{ Returns True if the TestPIDL is a child of the namespace.  Immediate forces   }
{ function to be true only of the passed PIDL is the immidiate child of the     }
{ namespace.                                                                    }
begin
  Result := Boolean( ILIsParent(FAbsolutePIDL, TestPIDL, Immediate));
end;

function TNamespace.IsChildOfRemovableDrive: Boolean;
// Checks to see if the namespace is a child of a removable drive.  If the drive
// is removed then ILIsParent fails because the drive is no longer valid so any
// PIDL walking routines will fail and the PIDL is orphaned
var
  NS: TNamespace;
  PIDL, NewPIDL: PItemIDList;
  OldCB: Word;
begin
  Result := False;
  if PIDLMgr.IDCount(AbsolutePIDL) > 1 then
  begin
    PIDL := PIDLMgr.NextID(FAbsolutePIDL);
    PIDL := PIDLMgr.NextID(PIDL);  // Now we have the Drive
    PIDL := PIDLMgr.NextID(PIDL);  // Now we have the one past the Drive
    OldCb := PIDL.mkid.cb;
    PIDL.mkid.cb := 0;
    NewPIDL := PIDLMgr.CopyPIDL(FAbsolutePIDL);
    PIDL.mkid.cb := OldCB;
    // NS is now a TNamespace to the Drive
    NS := TNamespace.Create(NewPIDL, nil);
    Result := NS.Removable;
    NS.Free
  end
end;

function TNamespace.IsControlPanel: Boolean;
begin
  // Windows Vista and up don't give you the real Control Panel Folder
  if Assigned(ControlPanelFolderEx) then
    Result := ILIsEqual(AbsolutePIDL, ControlPanelFolderEx.AbsolutePIDL)
  else
  if Assigned(ControlPanelFolder) then
    Result := ILIsEqual(AbsolutePIDL, ControlPanelFolder.AbsolutePIDL)
  else
    Result := False
end;

function TNamespace.IsControlPanelChildFolder: Boolean;
begin
  if Assigned(ControlPanelFolder) then
    Result := ILIsParent(ControlPanelFolder.AbsolutePIDL, AbsolutePIDL, True)
  else
    Result := False
end;

function TNamespace.IsDesktop: Boolean;
begin
  Result := PIDLMgr.IsDesktopFolder(AbsolutePIDL)
end;

function TNamespace.IsMyComputer: Boolean;
begin
  if Assigned(DrivesFolder) then
    Result := ILIsEqual(DrivesFolder.AbsolutePIDL, AbsolutePIDL)
  else
    Result := False;
end;

function TNamespace.IsNetworkNeighborhood: Boolean;
begin
  if Assigned(NetworkNeighborHoodFolder) then
    Result := ILIsEqual(NetworkNeighborHoodFolder.AbsolutePIDL, AbsolutePIDL)
  else
    Result := False;
end;

function TNamespace.IsNetworkNeighborhoodChild: Boolean;
begin
  if Assigned(NetworkNeighborHoodFolder) then
    Result := ILIsParent(NetworkNeighborHoodFolder.AbsolutePIDL, AbsolutePIDL, False)
  else
    Result := False;
end;

function TNamespace.IsParentByNamespace(TestNamespace: TNamespace;
  Immediate: Boolean): Boolean;
{ Returns True if the TestNamespace is a parent of the namespace.  Immediate    }
{ forces function to be true only of the passed PIDL is the immidiate parent    }
{ of the namespace.                                                             }
begin
  Result := Boolean( ILIsParent(TestNamespace.FAbsolutePIDL, FAbsolutePIDL, Immediate));
end;

function TNamespace.IsParentByPIDL(TestPIDL: PItemIDList;
  Immediate: Boolean): Boolean;
{ Returns True if the TestPIDL is a parent of the namespace.  Immediate forces  }
{ function to be true only of the passed PIDL is the immidiate parent of the    }
{ namespace.                                                                    }
begin
   Result := Boolean( ILIsParent(TestPIDL, FAbsolutePIDL, Immediate));
end;

function TNamespace.IsRecycleBin: Boolean;
begin
  { RecycleBin may not be avaiable if System Administrator has removed it in Win2k at least }
  if Assigned(RecycleBinFolder) and not (nsRecycleBinChecked in States) then
  begin
    if ILIsEqual(AbsolutePIDL, RecycleBinFolder.AbsolutePIDL) then
      Include(FStates, nsIsRecycleBin)
    else
      Exclude(FStates, nsIsRecycleBin);
    Include(FStates, nsRecycleBinChecked);
  end;
  Result := nsIsRecycleBin in States;
end;

function TNamespace.ParseDisplayName: PItemIDList;
begin
  Result := ParseDisplayName(NameForParsing)
end;

function TNamespace.ParseDisplayName(Path: WideString): PItemIDList;
var
  chEaten: ULONG;
  Attrib: ULONG;
  Desktop: IShellFolder;
begin
  Result := nil;
  Attrib := 0;
  SHGetDesktopFolder(Desktop);
  if Assigned(Desktop) then
  begin
    if Desktop.ParseDisplayName(ParentWnd, nil, PWideChar( Path),
      chEaten, Result, Attrib) <> NOERROR
    then
      Result := nil;
  end
end;

function TNamespace.Paste(Owner: TWinControl; NamespaceArray: TNamespaceArray; AsShortCut: Boolean = False): Boolean;
var
  NSA: TNamespaceArray;
  i: integer;
begin
  Result := False;
  if CanPasteToAll(NamespaceArray) then
  begin
    if IsDesktop then
    begin
      SetLength(NSA, Length(NamespaceArray));
      // Convert the virtual Desktop based TNamespaces to the Physical Desktop Folder based TNamespaces
      for i := 0 to Length(NSA) - 1 do
        NSA[i] := TNamespace.Create(PathToPIDL(NamespaceArray[i].NameForParsing), nil);
      Result := PhysicalDesktopFolder.Paste(Owner, NSA, AsShortCut);
    end else
    begin
      if VerifyPIDLRelationship(NamespaceArray, True) then
      begin
        if AsShortCut then
          Result := ExecuteContextMenuVerb(Owner, 'pastelink', NamespaceToRelativePIDLArray(NamespaceArray))
        else
          Result := ExecuteContextMenuVerb(Owner, 'paste', NamespaceToRelativePIDLArray(NamespaceArray))
      end else
      begin
        if AsShortCut then
          ExecuteContextMenuVerbMultiPath(Owner, 'pastelink', NamespaceArray)
        else
          ExecuteContextMenuVerbMultiPath(Owner, 'paste', NamespaceArray)
      end
    end
  end
end;

procedure TNamespace.LoadCategoryInfo;
var
  ColumnID: TSHColumnID;
  CatGUID: TGUID;
  Buffer: WideString;
  i: Integer;
  Done: Boolean;
begin
  if (CatInfo.CategoryCount = 0) and Assigned(ShellFolder2) and Assigned(CategoryProviderInterface) then
  begin
    CatGUID := GUID_NULL;
    FCatInfo.CategoryCount := Parent.DetailsSupportedColumns;
    SetLength(FCatInfo.Categories, Parent.DetailsSupportedColumns);
    SetLength(FCatInfo.CategoryNames, Parent.DetailsSupportedColumns);
    SetLength(FCatInfo.CatGUID, Parent.DetailsSupportedColumns);
    SetLength(FCatInfo.ColumnID, Parent.DetailsSupportedColumns);
    SetLength(FCatInfo.CanCatatorize, Parent.DetailsSupportedColumns);
    SetLength(Buffer, 256);
    for i := 0 to Parent.DetailsSupportedColumns - 1 do
    begin
      FillChar(ColumnID, SizeOf(ColumnID), #0);
      FillChar(PWideChar( Buffer)^, 256 * 2, #0);
      CatGUID := GUID_NULL;
      FCatInfo.Categories[i].Description := '';
      FCatInfo.Categories[i].Collapsed := False;
      FCatInfo.Categories[i].Hidden := False;
      FCatInfo.CategoryNames[i] := '';
      FCatInfo.ColumnID[i].pid := 0;
      FCatInfo.ColumnID[i].fmtid := GUID_NULL;
      FCatInfo.CatGUID[i] := GUID_NULL;
      FCatInfo.CanCatatorize[i] := False;
      FCatInfo.DefaultColumn := -1;
      if Succeeded(ShellFolder2.MapColumnToSCID(i, ColumnID)) then
      begin
        CatInfo.ColumnID[i] := ColumnID;
        if Succeeded(CategoryProviderInterface.CanCategorizeOnSCID(ColumnID)) then
        begin
          ColumnID := CatInfo.ColumnID[i];
          CatInfo.CanCatatorize[i] := True;
          if Succeeded(CategoryProviderInterface.GetCategoryForSCID(ColumnID, CatGUID)) then
          begin
            ColumnID := CatInfo.ColumnID[i];
            CatInfo.CatGUID[i] := CatGUID;
            if not IsEqualGUID(CatGUID, GUID_NULL) then
              if Succeeded(CategoryProviderInterface.GetCategoryName(CatGUID, PWideChar(Buffer), 256)) then
              begin
                FCatInfo.CategoryNames[i] := Buffer;
                SetLength(FCatInfo.CategoryNames[i], lstrlenW(PWideChar(FCatInfo.CategoryNames[i])));
              end
          end
        end
      end
    end;
    CatGUID := GUID_NULL;
    if Succeeded(CategoryProviderInterface.GetDefaultCategory(CatGUID, ColumnID)) then
    begin
      i := 0;
      Done := False;
      while (i < Length(CatInfo.ColumnID)) and not Done do
      begin
        if IsEqualGUID(ColumnID.fmtid, CatInfo.ColumnID[i].fmtid) and (ColumnID.pid = CatInfo.ColumnID[i].pid) then
        begin
          FCatInfo.DefaultColumn := i;
          Done := True
        end;
        Inc(i)
      end
    end
  end
end;

procedure TNamespace.SetCurrentContextMenu(const Value: IContextMenu);
begin
  FCurrentContextMenu := Value;
end;

procedure TNamespace.SetCurrentContextMenu2(const Value: IContextMenu2);
begin
  FCurrentContextMenu2 := Value;
end;

procedure TNamespace.SetDetailByThread(ColumnIndex: Integer; Detail: WideString);
var
  TempCache: PDetailsOfCacheRec;
begin
  EnsureDetailCache;
  if Parent.DetailsValidIndex(ColumnIndex) then
  begin
    TempCache := @ShellCache.Data.DetailsOfCache[ColumnIndex];
    TempCache.Caption := Detail;
    Include(TempCache.Cached, docCaptionValid);
    Include(TempCache.Cached, docThreadLoaded);
    Exclude(TempCache.Cached, docThreadLoading);
  end
end;

procedure TNamespace.SetFreePIDLOnDestroy(const Value: Boolean);
begin
  if Value then
    Include(FStates, nsFreePIDLOnDestroy)
  else
    Exclude(FStates, nsFreePIDLOnDestroy)
end;

procedure TNamespace.SetIconIndexChanged(const Value: Boolean);
// Sets or resets if the index changed.  Currently the SetIconIndexByThread method sets
// this flag.  It is not reset automaticlly it is up to the application to reset then
// when it has detected and used it.
begin
  if Value then
    Include(FStates, nsIconIndexChanged)
  else
    Exclude(FStates, nsIconIndexChanged);
end;

procedure TNamespace.SetIconIndexByThread(IconIndex: Integer; OverlayIndex: Integer; ClearThreadLoading: Boolean);
begin
  Include(FStates, nsThreadedIconLoaded); // Small Normal Icon is now Cached
  FShellCache.Data.SmallIcon := IconIndex;
  FSHellCache.Data.OverlayIndex := OverlayIndex;
  IconCache := FShellCache.Data.SmallIcon;
  Include(FShellCache.ShellCacheFlags, scSmallIcon);
  Include(FShellCache.ShellCacheFlags, scOverlayIndex);
  if ClearThreadLoading then
    Exclude(FStates, nsThreadedIconLoading);
  IconIndexChanged := True;
end;

procedure TNamespace.SetImageByThread(Bitmap: TBitmap;
  ClearThreadLoading: Boolean);
begin
  Include(FStates, nsThreadedImageLoaded);
  FImage := Bitmap;
  if ClearThreadLoading then
    Exclude(FStates, nsThreadedImageLoading);
end;

function TNamespace.SetNameOf(NewName: WideString): Boolean;
const
  ALL_FOLDERS = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
var
  P, NewPIDL, NewAbsPIDL: PItemIDList;
  Oldcb: Word;
  OldCursor: TCursor;
begin
  Result := False;
  P := nil;
  if CanRename and Assigned(ParentShellFolder) then
  begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      { The shell frees the PIDL so we need a copy }
      P := PIDLMgr.CopyPIDL(FRelativePIDL);
      NewPIDL := nil;
      // If the user cancels out of a duplicate rename this STILL succeeds so we need the Valid test below
      if Succeeded(ParentShellFolder.SetNameOf(ParentWnd, P, PWideChar(NewName), ALL_FOLDERS, NewPIDL)) then
      begin
        // Win98 will return success but never touch NewPIDL when trying to change name
        // of dialup connection.  Not sure how do it if this fails though??
        if Assigned(NewPIDL) then
        begin

          // Lets see if the rename actually took place, if it did the original object will be gone and the PIDL will
          // be invalid
          if not Valid then
          begin
            Result := True;
            { Temporary shortening of AbsolutePIDL }
            Oldcb := RelativePIDL.mkid.cb;
            RelativePIDL.mkid.cb := 0;
            NewAbsPIDL := PIDLMgr.AppendPIDL(AbsolutePIDL, NewPIDL);
            RelativePIDL.mkid.cb := Oldcb;
            PIDLMgr.FreePIDL(FAbsolutePIDL); // Remember Relative PIDL overlays AbsPIDL
            FAbsolutePIDL := NewAbsPIDL;
            FRelativePIDL := PIDLMgr.GetPointerToLastID(AbsolutePIDL);
          end;
        end
      end
    finally
      Screen.Cursor := OldCursor;
      PIDLMgr.FreePIDL(P)
    end
  end;
end;

procedure TNamespace.SetThreadedDetailLoaded(ColumnIndex: Integer; Value: Boolean);
begin
  EnsureDetailCache;
  if Parent.DetailsValidIndex(ColumnIndex) then
    Include(ShellCache.Data.DetailsOfCache[ColumnIndex].Cached, docThreadLoaded)
end;

procedure TNamespace.SetThreadedDetailLoading(ColumnIndex: Integer; Value: Boolean);
begin
  EnsureDetailCache;
  if Parent.DetailsValidIndex(ColumnIndex) then
    Include(ShellCache.Data.DetailsOfCache[ColumnIndex].Cached, docThreadLoading)
end;

procedure TNamespace.SetThreadIconLoading(const Value: Boolean);
begin
  if Value then
    Include(FStates, nsThreadedIconLoading)
  else
    Exclude(FStates, nsThreadedIconLoading)
end;

function TNamespace.GetThreadedImageLoaded: Boolean;
begin
  Result := nsThreadedImageLoaded in States
end;

function TNamespace.GetThreadedImageLoading: Boolean;
begin
  Result := nsThreadedImageLoading in States
end;

procedure TNamespace.SetThreadImageLoading(const Value: Boolean);
begin
  if Value then
    Include(FStates, nsThreadedImageLoading)
  else
    Exclude(FStates, nsThreadedImageLoading)
end;

function TNamespace.ShellExecuteNamespace(WorkingDir, CmdLineArguments: WideString;
  ExecuteFolder: Boolean = False; ExecuteFolderShortCut: Boolean = False;
  RunInThread: Boolean = False): Boolean;
{ Attempts execute the object that the namespace is representing.  WorkingDir   }
{ is the directory that will be the current directory of the application that   }
{ is being executed.  If the directory does not exist the directory where the   }
{ file being executed resides.  CmdLineArguments are any switches or parameters }
{ that can be added to the file being executed.                                 }
{ ExecuteFolder stops the call from being performed if the namespace is a folder}
{ Doing so usually opens an explorer window to Explore the folder.              }
var
  ShellExecuteInfoA: TShellExecuteInfoA;
  ShellExecuteInfoW: TShellExecuteInfoW;
  ShortWorkingDir, ShortCmdLine: AnsiString;
  DoExecute: Boolean;
  ShellLink: TVirtualShellLink;
  ShellExecuteThread: TCommonShellExecuteThread;
begin
  Result := False;
  DoExecute := True;

  if not ExecuteFolder then
    DoExecute := not Folder;

  if not ExecuteFolderShortCut and DoExecute then
  begin
    if Link then
    begin
      ShellLink := TVirtualShellLink.Create(nil);
      try
        ShellLink.ReadLink(NameParseAddress);
        DoExecute := not WideDirectoryExists(ShellLink.TargetPath);
      finally
        ShellLink.Free
      end
    end
  end;

  if DoExecute then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      FillChar(ShellExecuteInfoW, SizeOf(TShellExecuteInfoW), #0);
      if WideDirectoryExists(WorkingDir) then
        ShellExecuteInfoW.lpDirectory := PWideChar(WorkingDir)
      else // This should always be a file not a folder so this is ok
        ShellExecuteInfoW.lpDirectory := PWideChar(WideExtractFileDir(NameParseAddress));
        ShellExecuteInfoW.cbSize := SizeOf(TShellExecuteInfoW);
        ShellExecuteInfoW.fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_NOCLOSEPROCESS;
        if RunInThread then
          ShellExecuteInfoW.fMask := ShellExecuteInfoW.fMask or SEE_MASK_FLAG_DDEWAIT;
        ShellExecuteInfoW.Wnd:= ParentWnd;
        ShellExecuteInfoW.nShow := SW_SHOWNORMAL;
        ShellExecuteInfoW.lpIDList:= AbsolutePIDL;
        ShellExecuteInfoW.lpParameters := PWideChar(CmdLineArguments);
        if RunInThread then
        begin
          ShellExecuteThread := TCommonShellExecuteThread.Create(True);
          ShellExecuteThread.ShellExecuteInfoW.cbSize := ShellExecuteInfoW.cbSize;
          ShellExecuteThread.ShellExecuteInfoW.fMask := ShellExecuteInfoW.fMask;
          ShellExecuteThread.ShellExecuteInfoW.Wnd := ShellExecuteInfoW.Wnd;
          ShellExecuteThread.ShellExecuteInfoW.nShow := ShellExecuteInfoW.nShow;
          ShellExecuteThread.ShellExecuteInfoW.hInstApp := ShellExecuteInfoW.hInstApp;
          ShellExecuteThread.ShellExecuteInfoW.hkeyClass := ShellExecuteInfoW.hkeyClass;
          ShellExecuteThread.ShellExecuteInfoW.dwHotKey := ShellExecuteInfoW.dwHotKey;
          ShellExecuteThread.ShellExecuteInfoW.hIcon := ShellExecuteInfoW.hIcon;
          ShellExecuteThread.ShellExecuteInfoW.hProcess := ShellExecuteInfoW.hProcess;
          ShellExecuteThread.lpDirectory := ShellExecuteInfoW.lpDirectory;
          ShellExecuteThread.lpParameters := ShellExecuteInfoW.lpParameters;
          ShellExecuteThread.PIDL := PIDLMgr.CopyPIDL(ShellExecuteInfoW.lpIDList);
          ShellExecuteThread.Resume;
          Result := True;
        end else
          Result := ShellExecuteExW_MP(@ShellExecuteInfoW);
    end else
    begin
      FillChar(ShellExecuteInfoA, SizeOf(TShellExecuteInfo), #0);
      if WideDirectoryExists(WorkingDir) then
        ShortWorkingDir := WorkingDir
      else
        ShortWorkingDir := ExtractFileDir(NameParseAddress);
      ShellExecuteInfoA.lpDirectory := PAnsiChar(ShortWorkingDir);
      ShellExecuteInfoA.cbSize := SizeOf(TShellExecuteInfo);
      ShellExecuteInfoA.fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_NOCLOSEPROCESS;
      if RunInThread then
        ShellExecuteInfoA.fMask := ShellExecuteInfoA.fMask or SEE_MASK_FLAG_DDEWAIT;
      ShellExecuteInfoA.Wnd:= ParentWnd;
      ShellExecuteInfoA.nShow := SW_SHOWNORMAL;
      ShellExecuteInfoA.lpIDList:= AbsolutePIDL;
      ShortCmdLine := CmdLineArguments;
      ShellExecuteInfoA.lpParameters := PAnsiChar(ShortCmdLine);
      if RunInThread then
      begin
        ShellExecuteThread := TCommonShellExecuteThread.Create(True);
        ShellExecuteThread.ShellExecuteInfoA.cbSize := ShellExecuteInfoA.cbSize;
        ShellExecuteThread.ShellExecuteInfoA.fMask := ShellExecuteInfoA.fMask;
        ShellExecuteThread.ShellExecuteInfoA.Wnd := ShellExecuteInfoA.Wnd;
        ShellExecuteThread.ShellExecuteInfoA.nShow := ShellExecuteInfoA.nShow;
        ShellExecuteThread.ShellExecuteInfoA.hInstApp := ShellExecuteInfoA.hInstApp;
        ShellExecuteThread.ShellExecuteInfoA.hkeyClass := ShellExecuteInfoA.hkeyClass;
        ShellExecuteThread.ShellExecuteInfoA.dwHotKey := ShellExecuteInfoA.dwHotKey;
        ShellExecuteThread.ShellExecuteInfoA.hIcon := ShellExecuteInfoA.hIcon;
        ShellExecuteThread.ShellExecuteInfoA.hProcess := ShellExecuteInfoA.hProcess;
        ShellExecuteThread.lpDirectory := ShellExecuteInfoA.lpDirectory;
        ShellExecuteThread.lpParameters := ShellExecuteInfoA.lpParameters;
        ShellExecuteThread.PIDL := PIDLMgr.CopyPIDL(ShellExecuteInfoA.lpIDList);
        ShellExecuteThread.Resume;
        Result := True;
      end else
        Result := ShellExecuteEx(@ShellExecuteInfoA);
    end
  end
end;

function TNamespace.ShowContextMenu(Owner: TWinControl;
  ContextMenuCmdCallback: TContextMenuCmdCallback;
  ContextMenuShowCallback: TContextMenuShowCallback;
  ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback;
  Position: PPoint = nil;
  CustomShellSubMenu: TPopupMenu = nil;
  CustomSubMenuCaption: WideString = ''): Boolean;
{ Displays the ContextMenu of the namespace.                                    }
var
  PIDLArray: TRelativePIDLArray;
begin
  SetLength(PIDLArray, 1);
  PIDLArray[0] := RelativePIDL;
  Result := InternalShowContextMenu(Owner, ContextMenuCmdCallback, ContextMenuShowCallback,
    ContextMenuAfterCmdCallback, PIDLArray, Position, CustomShellSubMenu, CustomSubMenuCaption);
end;

function TNamespace.ShowContextMenuMulti(Owner: TWinControl; ContextMenuCmdCallback: TContextMenuCmdCallback; ContextMenuShowCallback: TContextMenuShowCallback; ContextMenuAfterCmdCallback: TContextMenuAfterCmdCallback; NamespaceArray: TNamespaceArray; Position: PPoint = nil; CustomShellSubMenu: TPopupMenu = nil; CustomSubMenuCaption: WideString = ''; Focused: TNamespace = nil; ShowPasteItem: Boolean = False; ShowRenameItem: Boolean = False; ShowShortCutMenuItem: Boolean = False): Boolean;
//
// Focused, ShowPasteItem, ShowRenameItem, ShowShortCutMenuItem only have meaning if
// the Multi is the result of a namespace array with items from different folders
begin
  Result := False;
  if VerifyPIDLRelationship(NamespaceArray, True) then
  begin
      Result := InternalShowContextMenu(Owner, ContextMenuCmdCallBack,
        ContextMenuShowCallback, ContextMenuAfterCmdCallback,
        NamespaceToRelativePIDLArray(NamespaceArray), Position, CustomShellSubMenu,
        CustomSubMenuCaption)
  end else
  begin
    if Length(NamespaceArray) > 0 then
      ShowContextMenuMultiPath(Owner, Focused, NamespaceArray, ShowPasteItem, ShowRenameItem, ShowShortCutMenuItem, Position)
  end
end;

function TNamespace.ShowContextMenuMultiPath(Owner: TWinControl; Focused: TNamespace; Namespaces: TNamespaceArray; ShowPasteItem, ShowRenameItem, ShowShortCutMenuItem: Boolean; Position: PPoint = nil): Boolean;
var
  Menu: TCommonShellMultiParentContextMenu;
  ShellFolder: IShellFolder;
begin
  Menu := TCommonShellMultiParentContextMenu.Create(Owner);
  Menu.ReferenceCounted := True;
  ShellFolder := Menu;
  Menu.RenameMenuItem := ShowRenameItem;
  Menu.ShortCutMenuItem := ShowShortCutMenuItem;
  Menu.PasteMenuItem := ShowPasteItem;
  Result := Menu.ShowContextMenu(Menu.MsgWnd, Focused, Namespaces, Position);
end;

procedure TNamespace.ShowPropertySheet(Owner: TWinControl);
var
  NamespaceArray: TNamespaceArray;
begin
  if HasPropSheet then
  begin
    SetLength(NamespaceArray, 1);
    NamespaceArray[0] := Self;
    if VerifyPIDLRelationship(NamespaceArray, True) then
      ExecuteContextMenuVerb(Owner, 'properties', NamespaceToRelativePIDLArray(NamespaceArray))
    else
      ExecuteContextMenuVerbMultiPath(Owner, 'properties', NamespaceArray)
  end
end;

procedure TNamespace.ShowPropertySheetMulti(Owner: TWinControl; NamespaceArray: TNamespaceArray; UseSHMultiFileProperties: Boolean = True; ForceNonMultiPath: Boolean = False);
var
  IDO: IDataObject;
  NSList: TList;
begin
  if CanShowPropertiesOfAll(NamespaceArray) then
  begin
    // Call SHMultiFileProperties_MP to show the property sheet when the
    // APIDLArray items are from different folders.
    // Minimum OS: Win2k
    if not ForceNonMultiPath and (UseSHMultiFileProperties and Assigned(SHMultiFileProperties_MP)) then
    begin
      IDO := nil;
      NSList := NamespaceToNamespaceList(NamespaceArray);
      CreateFullyQualifiedShellDataObject(NSList, False, IDO);
      FreeAndNil(NSList);
      if Assigned(IDO) then
        SHMultiFileProperties_MP(IDO, 0);
  //    IDO._Release // I did this in DefMenuCreateCallback, I don't know why I have to but I do
    end else
    begin
      if VerifyPIDLRelationship(NamespaceArray, True) or ForceNonMultiPath then
        ExecuteContextMenuVerb(Owner, 'properties', NamespaceToRelativePIDLArray(NamespaceArray))
      else
        ExecuteContextMenuVerbMultiPath(Owner, 'properties', NamespaceArray)
    end
  end
end;

function TNamespace.TestAttributesOf(Flags: Longword; FlushCache: Boolean; SoftFlush: Boolean = False): Boolean;
// Pass any of the flags for IShellFolder.GetAttributesOf to see if they exist
// for the Folder. FlushCache forces the shell to reload the information on the
// namespace.  Useful to handle the bug where the shell caches the icon for a
// CD drive and never changes it.  Flushing it will force it to reload the Index
// Note this is dangerous with 3rd party namespaces.  M$ suggests this method for
// their namespaces but at least Hummingbird network namespace crashes with this
// they apparently don't check for 0 PIDL's
// Soft Flush add the SFGAO_VALIDATE flag to get the fresh info
var
  x: Longword;
begin
  if Assigned(ParentShellFolder) then
  begin
    x := Flags;
    if FlushCache then
    begin
      x := x or SFGAO_VALIDATE;
      ParentShellFolder.GetAttributesOf(0, FRelativePIDL, x);
    end else
    if SoftFlush then
    begin
      x := x or SFGAO_VALIDATE;
      ParentShellFolder.GetAttributesOf(1, FRelativePIDL, x)
    end else
      ParentShellFolder.GetAttributesOf(1, FRelativePIDL, x);
    Result := Flags and x = Flags;
  end else
    Result := False;
end;

function TNamespace.VerifyPIDLRelationship(NamespaceArray: TNamespaceArray;
  Silent: Boolean = False): Boolean;
var
  i: integer;
begin
  Result := True;
  i := 0;
  while (i < Length(NamespaceArray)) and Result do
  begin
    { TNamespace is based off using the parent to access the data so it is      }
    { correct to do the test for childPIDLs relative from the parent.           }
    if IsDesktop and (Length(NamespaceArray) = 1) then
    begin
      Result := (PIDLMgr.IDCount(NamespaceArray[i].AbsolutePIDL) = 1) or (NamespaceArray[0].IsDesktop)
    end else
    begin
      if Assigned(Parent) then
        Result := ILIsParent(Parent.AbsolutePIDL, NamespaceArray[i].AbsolutePIDL, True)
      else begin
        if (Length(NamespaceArray) = 1) and NamespaceArray[0].IsDesktop then
          Result := True
        else
          Result := False;
      end
    end;
    Inc(i)
  end;
  if not Silent and (not Result and not IsDesktop) then
    WideShowMessage(ParentWnd, STR_ERROR, STR_ERR_BAD_PIDL_RELATIONSHIP);
end;

procedure TNamespace.WindowProcForContextMenu(var Message: TMessage);
begin
  if Assigned(FOldWndProcForContextMenu) then
    FOldWndProcForContextMenu(Message); // Call the OldWindProc of the ContextMenu owner
  case Message.Msg of
    WM_DRAWITEM, WM_INITMENUPOPUP, WM_MEASUREITEM, WM_MENUCHAR:
      HandleContextMenuMsg(Message.Msg, Message.WParam, Message.LParam, Message.Result);
  end;
end;

function TNamespace.GetCategoryProviderInterface: ICategoryProvider;
begin
  if not Assigned(FCategoryProviderInterface) and Folder and Assigned(ShellFolder) then
  begin
    if not Succeeded(ShellFolder.CreateViewObject(ParentWnd, IID_ICategoryProvider, Pointer(FCategoryProviderInterface))) then
      FCategoryProviderInterface := nil
  end;
  Result := FCategoryProviderInterface
end;

function TNamespace.CreateCategory(GUID: TGUID): ICategorizer;
begin
  Result := nil;
  if Assigned(CategoryProviderInterface) then
  begin
    if not Succeeded(CategoryProviderInterface.CreateCategory(GUID, IID_ICategorizer, Result)) then
      Result := nil;
  end
end;

function TNamespace.EnumFuncDummy(MessageWnd: HWnd; APIDL: PItemIDList; AParent: TNamespace; Data: Pointer; var Terminate: Boolean): Boolean;
begin
  PIDLMgr.FreePIDL(APIDL);
  Result := True;
end;

function TNamespace.GetBrowserFrameOptionsInterface: IBrowserFrameOptions;
var
  Found: Boolean;
begin
  if not Assigned(FBrowserFrameOptionsInterface) then
  begin
    Found := False;
    if Assigned(Parent) then
    begin
       Found := Succeeded(Parent.ShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL, IBrowserFrameOptions, nil, Pointer(FBrowserFrameOptionsInterface)));
       if not Found and Folder then
       begin
           Found := Succeeded(ShellFolder.CreateViewObject(ParentWnd, IBrowserFrameOptions, Pointer(FBrowserFrameOptionsInterface)));
         if not Found then
           Found := Succeeded(ShellFolder.QueryInterface(IBrowserFrameOptions, Pointer(FBrowserFrameOptionsInterface)))
       end
    end;
    if not Found then
      FBrowserFrameOptionsInterface := nil
  end;
  Result := FBrowserFrameOptionsInterface
end;

function TNamespace.GetQueryAssociationsInterface: IQueryAssociations;
var
  Found: Boolean;
begin
  if not Assigned(FQueryAssociationsInterface) then
  begin
    Found := False;
    if Assigned(Parent) then
    begin
       Found := Succeeded(Parent.ShellFolder.GetUIObjectOf(ParentWnd, 1, FRelativePIDL, IQueryAssociations, nil, Pointer(FQueryAssociationsInterface)));
       if not Found and Folder then
       begin
           Found := Succeeded(ShellFolder.CreateViewObject(ParentWnd, IQueryAssociations, Pointer(FQueryAssociationsInterface)));
         if not Found then
           Found := Succeeded(ShellFolder.QueryInterface(IQueryAssociations, Pointer(FQueryAssociationsInterface)))
       end
    end;
    if not Found then
      FQueryAssociationsInterface := nil
  end;
  Result := FQueryAssociationsInterface
end;

function TNamespace.GetValid: Boolean;
var
  rgfInOut: UINT;
begin
  // Does not work on floppy drives and such
  // password proctected network drives also return false regardless if they have
  // not been logged into yet so return true for those too.
  if (not Removable and (Assigned(ParentShellFolder))) and not PotentialMappedDrive(Self) then
  begin
    rgfInOut := SFGAO_VALIDATE;
    // This returns false on a password protected network folder
    Result := ParentShellFolder.GetAttributesOf(1, FRelativePIDL, rgfInOut) = NOERROR
  end else
    Result := True
end;

procedure TNamespace.ReplacePIDL(NewPIDL: PItemIDList; AParent: TNamespace);
begin
  InvalidateNamespace(True);
  PIDLMgr.FreeAndNilPIDL(FAbsolutePIDL);
  if Assigned(Parent) then
    FAbsolutePIDL := PIDLMgr.AppendPIDL(AParent.FAbsolutePIDL, PIDLMgr.CopyPIDL(NewPIDL))
  else
    FAbsolutePIDL := PIDLMgr.CopyPIDL(NewPIDL);
  FRelativePIDL := PIDLMgr.GetPointerToLastID(FAbsolutePIDL);
end;



{ TExtractImage }

constructor TExtractImage.Create;
begin
  FWidth := 200;
  FHeight := 200;
  FColorDepth := 32;
  FFlags := IEIFLAG_SCREEN;
end;

function TExtractImage.GetImage: TBitmap;

   function BitsToPixelFormat(Bits: Windows.TBITMAP): TPixelFormat;
   begin
     case Bits.bmBitsPixel of
       32: Result := pf32Bit;
       24: Result := pf24Bit;
       16: Result := pf16Bit;
       15: Result := pf15Bit;
       8:  Result := pf8Bit;
       4:  Result := pf4Bit;
       1:  Result := pf1Bit;
     else
       Result := pfDevice
     end
   end;

var
  Bits: HBITMAP;
begin
  Bits := 0;
  Result := nil;
  if Assigned(ExtractImageInterface) then
    if Succeeded(ExtractImageInterface.Extract(Bits)) then
    begin
      // Can't just assign the bitmap handle to the canvas because the bitmap
      // may not be a DIB.  If not then if the bitmap is written to as stream then
      // TBitmap will be created as a DIB when it is read back from the stream and
      // it will be displayed upside down
      Result := TBitmap.Create;
      Result.PixelFormat := pf32Bit; //BitsToPixelFormat(BitInfo);
      Result.Transparent := True;
      Result.Handle := Bits;
    end
end;

function TExtractImage.GetExtractImageInterface2: IExtractImage2;
var
  Found: Boolean;
begin
  if not Assigned(FExtractImage2Interface) then
  begin
    Found := False;
    if Assigned(ExtractImageInterface) then
      Found :=  ExtractImageInterface.QueryInterface(IID_IExtractImage2,
        Pointer(FExtractImage2Interface)) <> E_NOINTERFACE;
    if not Found then
      FExtractImage2Interface := nil
  end;
  Result := FExtractImage2Interface
end;


function TExtractImage.GetExtractImageInterface: IExtractImage;
var
  Found: Boolean;
begin
  if not Assigned(FExtractImageInterface) then
  begin
    Found := False;
    if Assigned(Owner.ParentShellFolder) then
    begin
      Found := Owner.ParentShellFolder.GetUIObjectOf(0, 1, Owner.FRelativePIDL,
        IExtractImage, nil, Pointer(FExtractImageInterface)) = NOERROR;
    end;
    if not Found and Assigned(Owner.ShellFolder) then
    begin
      Found := Owner.ShellFolder.CreateViewObject(0, IExtractImage,
        Pointer(FExtractImageInterface)) = NOERROR;
    end;
    if not Found then
      FExtractImageInterface := nil
  end;
  Result := FExtractImageInterface

end;

function TExtractImage.GetImagePath: WideString;
var
  Size: TSize;
  Buffer: PWideChar;
begin
  if Assigned(ExtractImageInterface) then
  begin
    GetMem(Buffer, MAX_PATH * 4);
    try
      try
        Size.cx := Width;
        Size.cy := Height;
        if ExtractImageInterface.GetLocation(Buffer, MAX_PATH, FPriority, Size,
          ColorDepth, FFlags) = NOERROR then
        begin
          Result := Buffer;
          PathExtracted := True
        end else
          Result := '';
      finally
        FreeMem(Buffer);
      end except
      Result := ''
    end
  end;
end;



{ ----------------------------------------------------------------------------- }
{ Encapsulation of IShellLink                                                   }
{ ----------------------------------------------------------------------------- }

{ TVirtualShellLink }

destructor TVirtualShellLink.Destroy;
begin
  FreeTargetIDList;
  inherited;
end;

procedure TVirtualShellLink.FreeTargetIDList;
var
  Malloc: IMalloc;
  PIDL: PItemIDList;
begin
  if Assigned(TargetIDList) then
  begin
    PIDL := TargetIDLIst;
    TargetIDList := nil;
    SHGetMalloc(Malloc);
    Malloc.Free(PIDL);
  end;
end;

function TVirtualShellLink.GetShellLinkAInterface: IShellLinkA;
begin
  if not Assigned(FShellLinkA) then
  begin
    if not Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IShellLinkA, FShellLinkA))
    then
      FShellLinkA := nil;
  end;
  Result := FShellLinkA
end;

function TVirtualShellLink.GetShellLinkWInterface: IShellLinkW;
begin   
  if not Assigned(FShellLinkW) then
  begin
    if not Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
      IShellLinkW, FShellLinkW))
    then
      FShellLinkW := nil
  end;
  Result := FShellLinkW
end;

function TVirtualShellLink.ReadLink(LinkFileName: WideString): Boolean;
const
  BUFFERSIZE = 1024;
var
  Success: Boolean;
  S: AnsiString;
  PersistFile: IPersistFile;
  pwHotKey: Word;
  Cmd: integer;
  FindData: WIN32_FIND_DATAA;
  FindDataW: WIN32_FIND_DATAW;
begin
  Result := False;
  Success := False;
  if Assigned(ShellLinkWInterface) then
  begin
    if CommonSupports(ShellLinkWInterface, IPersistFile, PersistFile) then
    begin
      FFileName := LinkFileName;
      Success := Succeeded(PersistFile.Load(PWideChar(FileName), STGM_READWRITE));
      if Success then
      begin
        Result := True;

        SetLength(FTargetPath, BUFFERSIZE);
        Success := Succeeded(ShellLinkWInterface.GetPath(PWideChar( FTargetPath), MAX_PATH, FindDataW, SLGP_UNCPRIORITY));
        if Success then
          SetLength(FTargetPath, lstrlenW(PWideChar( FTargetPath)));

        SetLength(FArguments, BUFFERSIZE);
        Success := Succeeded(ShellLinkWInterface.GetArguments(PWideChar( FArguments), BUFFERSIZE));
        if Success then
          SetLength(FArguments, lstrlenW(PWideChar( FArguments)));

        SetLength(FDescription, BUFFERSIZE);
        Success := Succeeded(ShellLinkWInterface.GetDescription(PWideChar( FDescription), BUFFERSIZE));
        if Success then
          SetLength(FDescription, lstrlenW(PWideChar( FDescription)));

        SetLength(FWorkingDirectory, BUFFERSIZE);
        Success := Succeeded(ShellLinkWInterface.GetWorkingDirectory(PWideChar( FWorkingDirectory), BUFFERSIZE));
        if Success then
          SetLength(FWorkingDirectory, lstrlenW(PWideChar( FWorkingDirectory)));

        SetLength(FIconLocation, BUFFERSIZE);
        Success := Succeeded(ShellLinkWInterface.GetIconLocation(PWideChar( FIconLocation), BUFFERSIZE, FIconIndex));
        if Success then
          SetLength(FIconLocation, lstrlenW(PWideChar( FIconLocation)));

        FreeTargetIDList;
        ShellLinkWInterface.GetIDList(FTargetIDList);

        Success := Succeeded(ShellLinkWInterface.GetHotKey(pwHotKey));
        if Success then
        begin
          FHotKey := LoByte(pwHotKey);
          FHotKeyModifiers := [];
          if HiByte(pwHotKey) and HOTKEYF_ALT <> 0 then Include(FHotKeyModifiers, hkmAlt);
          if HiByte(pwHotKey) and HOTKEYF_CONTROL <> 0 then Include(FHotKeyModifiers, hkmControl);
          if HiByte(pwHotKey) and HOTKEYF_EXT <> 0 then Include(FHotKeyModifiers, hkmExtendedKey);
          if HiByte(pwHotKey) and HOTKEYF_SHIFT <> 0 then Include(FHotKeyModifiers, hkmShift);
        end;

        Success := Succeeded(ShellLinkWInterface.GetShowCmd(Cmd));
        if Success then
        case Cmd of
          SW_HIDE:            ShowCmd := swHide;
          SW_MAXIMIZE:        ShowCmd := swMaximize;
          SW_MINIMIZE:        ShowCmd := swMinimize;
          SW_RESTORE:         ShowCmd := swRestore;
          SW_SHOW:            ShowCmd := swShow;
          SW_SHOWDEFAULT:     ShowCmd := swShowDefault;
          SW_SHOWMINIMIZED:   ShowCmd := swShowMinimized;
          SW_SHOWMINNOACTIVE: ShowCmd := swShowMinNoActive;
          SW_SHOWNA :         ShowCmd := swShowNA;
          SW_SHOWNOACTIVATE : ShowCmd := swShowNoActive;
          SW_SHOWNORMAL:      ShowCmd := swShowNormal;
        end;
 // Why was that here?  Removed 11.12.02
 //       PersistFile.Save(PWideChar(FileName), True)
      end else
        FFileName := ''
    end
  end;
  if not Success and Assigned(ShellLinkAInterface) then
  begin
    if CommonSupports(ShellLinkAInterface, IPersistFile, PersistFile) then
    begin
      FFileName := LinkFileName;
      Success := Succeeded(PersistFile.Load(PWideChar(FileName), STGM_READWRITE));
      if Success then
      begin
        Result := True;

        SetLength(S, BUFFERSIZE);
        Success := Succeeded(ShellLinkAInterface.GetPath(PAnsiChar( S), MAX_PATH, FindData, SLGP_UNCPRIORITY));
        if Success then
        begin
          SetLength(S, lstrlenA(PAnsiChar( S)));
          FTargetPath := S
        end;

        SetLength(S, BUFFERSIZE);
        Success := Succeeded(ShellLinkAInterface.GetArguments(PAnsiChar( S), BUFFERSIZE));
        if Success then
        begin
          SetLength(S, lstrlenA(PAnsiChar( S)));
          FArguments := S
        end;

        SetLength(S, BUFFERSIZE);
        Success := Succeeded(ShellLinkAInterface.GetDescription(PAnsiChar( S), BUFFERSIZE));
        if Success then
        begin
          SetLength(S, lstrlenA(PAnsiChar( S)));
          FDescription := S
        end;

        SetLength(S, BUFFERSIZE);
        Success := Succeeded(ShellLinkAInterface.GetWorkingDirectory(PAnsiChar( S), BUFFERSIZE));
        if Success then
        begin
          SetLength(S, lstrlenA(PAnsiChar( S)));
          FWorkingDirectory := S
        end;

        SetLength(S, BUFFERSIZE);
        Success := Succeeded(ShellLinkAInterface.GetIconLocation(PAnsiChar( S), BUFFERSIZE, FIconIndex));
        if Success then
        begin
          SetLength(S, lstrlenA(PAnsiChar( S)));
          FIconLocation := S
        end;

        FreeTargetIDList;
        ShellLinkAInterface.GetIDList(FTargetIDList);

        Success := Succeeded(ShellLinkAInterface.GetHotKey(pwHotKey));
        if Success then
        begin
          FHotKey := LoByte(pwHotKey);
          FHotKeyModifiers := [];
          if HiByte(pwHotKey) and HOTKEYF_ALT <> 0 then Include(FHotKeyModifiers, hkmAlt);
          if HiByte(pwHotKey) and HOTKEYF_CONTROL <> 0 then Include(FHotKeyModifiers, hkmControl);
          if HiByte(pwHotKey) and HOTKEYF_EXT <> 0 then Include(FHotKeyModifiers, hkmExtendedKey);
          if HiByte(pwHotKey) and HOTKEYF_SHIFT <> 0 then Include(FHotKeyModifiers, hkmShift);
        end;

        Success := Succeeded(ShellLinkAInterface.GetShowCmd(Cmd));
        if Success then
        case Cmd of
          SW_HIDE:            ShowCmd := swHide;
          SW_MAXIMIZE:        ShowCmd := swMaximize;
          SW_MINIMIZE:        ShowCmd := swMinimize;
          SW_RESTORE:         ShowCmd := swRestore;
          SW_SHOW:            ShowCmd := swShow;
          SW_SHOWDEFAULT:     ShowCmd := swShowDefault;
          SW_SHOWMINIMIZED:   ShowCmd := swShowMinimized;
          SW_SHOWMINNOACTIVE: ShowCmd := swShowMinNoActive;
          SW_SHOWNA :         ShowCmd := swShowNA;
          SW_SHOWNOACTIVATE : ShowCmd := swShowNoActive;
          SW_SHOWNORMAL:      ShowCmd := swShowNormal;
        end;

        PersistFile.Save(PWideChar(FileName), True)
      end else
        FFileName := '';
    end
  end
end;

function TVirtualShellLink.WriteLink(LinkFileName: WideString): Boolean;
var
  S: AnsiString;
  PersistFile: IPersistFile;
  pwHotKey, pwHotKeyHi: Word;
  KeyModifier: THotKeyModifiers;
  Cmd: integer;
begin
  Result := False;
  if (TargetPath = '') and not Assigned(TargetIDList) and not SilentWrite then
    WideShowMessage(Application.Handle, STR_NOTARGETDEFINED, STR_ERROR)
  else begin
    if Assigned(ShellLinkWInterface) then
    begin
      if CommonSupports(ShellLinkWInterface, IPersistFile, PersistFile) then
      begin
        FFileName := LinkFileName;
        ShellLinkWInterface.SetPath(PWideChar( FTargetPath));
        ShellLinkWInterface.SetArguments(PWideChar( FArguments));
        ShellLinkWInterface.SetDescription(PWideChar( FDescription));
        ShellLinkWInterface.SetPath(PWideChar( FTargetPath));
        ShellLinkWInterface.SetWorkingDirectory(PWideChar( FWorkingDirectory));
        ShellLinkWInterface.SetIconLocation(PWideChar( FIconLocation), FIconIndex);
        if Assigned(FTargetIDList) then
          ShellLinkWInterface.SetIDList(FTargetIDList);

        pwHotKey := HotKey;
        pwHotKeyHi := 0;
        KeyModifier := HotKeyModifiers;
        if hkmAlt in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_ALT;
        if hkmControl in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_CONTROL;
        if hkmExtendedKey in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_EXT;
        if hkmShift in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_SHIFT;

        pwHotKeyHi := pwHotKeyHi shl 8;     // Make lower 8 bits the upper 8 bits
        pwHotKeyHi := pwHotKeyHi and $FF00;  // Make sure lower 8 bits clear
        pwHotKey := pwHotKey or pwHotKeyHi;
        ShellLinkWInterface.SetHotkey(pwHotKey);

        case ShowCmd of
          swHide:             Cmd := SW_HIDE;
          swMaximize:         Cmd := SW_MAXIMIZE;
          swMinimize:         Cmd := SW_MINIMIZE;
          swRestore:          Cmd := SW_RESTORE;
          swShow:             Cmd := SW_SHOW;
          swShowDefault:      Cmd := SW_SHOWDEFAULT;
          swShowMinimized:    Cmd := SW_SHOWMINIMIZED;
          swShowMinNoActive:  Cmd := SW_SHOWMINNOACTIVE;
          swShowNA:           Cmd := SW_SHOWNA;
          swShowNoActive:     Cmd := SW_SHOWNOACTIVATE;
          swShowNormal:       Cmd := SW_SHOWNORMAL;
        else
          Cmd := SW_SHOWNORMAL
        end;
        ShellLinkWInterface.SetShowCmd(Cmd);

        Result := Succeeded(PersistFile.Save(PWideChar(FileName), True))
      end;
    end;
    if not Result and Assigned(ShellLinkAInterface) then
    begin
      if CommonSupports(ShellLinkAInterface, IPersistFile, PersistFile) then
      begin
        FFileName := LinkFileName;
        S := FTargetPath;
        ShellLinkAInterface.SetPath(PAnsiChar( S));
        S := FArguments;
        ShellLinkAInterface.SetArguments(PAnsiChar(S));
        S := FDescription;
        ShellLinkAInterface.SetDescription(PAnsiChar( S));
        S := FTargetPath;
        ShellLinkAInterface.SetPath(PAnsiChar( S));
        S := FWorkingDirectory;
        ShellLinkAInterface.SetWorkingDirectory(PAnsiChar( S));
        S := FIconLocation;
        ShellLinkAInterface.SetIconLocation(PAnsiChar( S), FIconIndex);

        if Assigned(FTargetIDList) then
          ShellLinkAInterface.SetIDList(FTargetIDList);

        pwHotKey := HotKey;
        pwHotKeyHi := 0;
        KeyModifier := HotKeyModifiers;
        if hkmAlt in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_ALT;
        if hkmControl in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_CONTROL;
        if hkmExtendedKey in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_EXT;
        if hkmShift in KeyModifier then pwHotKeyHi := pwHotKeyHi or HOTKEYF_SHIFT;

        pwHotKeyHi := pwHotKeyHi shl 8;     // Make lower 8 bits the upper 8 bits
        pwHotKeyHi := pwHotKeyHi and $FF00;  // Make sure lower 8 bits clear
        pwHotKey := pwHotKey or pwHotKeyHi;
        ShellLinkAInterface.SetHotkey(pwHotKey);

        case ShowCmd of
          swHide:             Cmd := SW_HIDE;
          swMaximize:         Cmd := SW_MAXIMIZE;
          swMinimize:         Cmd := SW_MINIMIZE;
          swRestore:          Cmd := SW_RESTORE;
          swShow:             Cmd := SW_SHOW;
          swShowDefault:      Cmd := SW_SHOWDEFAULT;
          swShowMinimized:    Cmd := SW_SHOWMINIMIZED;
          swShowMinNoActive:  Cmd := SW_SHOWMINNOACTIVE;
          swShowNA:           Cmd := SW_SHOWNA;
          swShowNoActive:     Cmd := SW_SHOWNOACTIVATE;
          swShowNormal:       Cmd := SW_SHOWNORMAL;
        else
          Cmd := SW_SHOWNORMAL
        end;
        ShellLinkAInterface.SetShowCmd(Cmd);

        Result := Succeeded(PersistFile.Save(PWideChar(FileName), True))
      end
    end
  end
end;

{ ----------------------------------------------------------------------------- }
{ TList that implements basic streaming                                         }
{ ----------------------------------------------------------------------------- }

{ TStreamableList }

constructor TStreamableList.Create;
begin
  FStreamVersion := STREAM_VERSION_DEFAULT
end;

procedure TStreamableList.LoadFromFile(FileName: WideString; Version: integer = 0;
  ReadVerFromStream: Boolean = False);
var
  {$IFDEF TNTSUPPORT}
  FileStream: TTntFileStream;
  {$ELSE}
  FileStream: TFileStream;
  {$ENDIF}
begin
  FileStream := nil;
  try
    {$IFDEF TNTSUPPORT}
    FileStream := TTntFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    {$ELSE}
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    {$ENDIF}
    LoadFromStream(FileStream);
  finally
    FileStream.Free
  end;
end;

procedure TStreamableList.LoadFromStream(S: TStream; Version: integer;
  ReadVerFromStream: Boolean);
begin
  Clear;  
  if ReadVerFromStream then
    S.ReadBuffer(FStreamVersion, Sizeof(FStreamVersion))
  else
    FStreamVersion := Version;
end;

procedure TStreamableList.SaveToFile(FileName: WideString; Version: integer = 0;
  ReadVerFromStream: Boolean = False);
var
  {$IFDEF TNTSUPPORT}
  FileStream: TTntFileStream;
  {$ELSE}
  FileStream: TFileStream;
  {$ENDIF}

begin
  FileStream := nil;
  try
   {$IFDEF TNTSUPPORT}
   FileStream := TTntFileStream.Create(FileName, fmCreate or fmShareExclusive);
   {$ELSE}
   FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
   {$ENDIF}
    SaveToStream(FileStream);
  finally
    FileStream.Free
  end;
end;

procedure TStreamableList.SaveToStream(S: TStream; Version: integer;
  WriteVerToStream: Boolean);
begin
  if WriteVerToStream then
    S.WriteBuffer(Version, Sizeof(Version));
  FStreamVersion := Version;
end;


{ ----------------------------------------------------------------------------- }
{ TClass that implements basic streaming                                        }
{ ----------------------------------------------------------------------------- }

{ TStreamableClass }

constructor TStreamableClass.Create;
begin
  FStreamVersion := STREAM_VERSION_DEFAULT
end;

procedure TStreamableClass.LoadFromFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False);
var
  {$IFDEF TNTSUPPORT}
  FileStream: TTntFileStream;
  {$ELSE}
  FileStream: TFileStream;
  {$ENDIF}
begin
  FileStream := nil;
  try
    {$IFDEF TNTSUPPORT}
    FileStream := TTntFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    {$ELSE}
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    {$ENDIF}
    LoadFromStream(FileStream, Version, ReadVerFromStream);
  finally
    FileStream.Free
  end;
end;

procedure TStreamableClass.LoadFromStream(S: TStream; Version: integer;
  ReadVerFromStream: Boolean);
begin
  if ReadVerFromStream then
    S.ReadBuffer(FStreamVersion, Sizeof(FStreamVersion))
  else
    FStreamVersion := Version;
end;

procedure TStreamableClass.SaveToFile(FileName: WideString; Version: integer = 0; ReadVerFromStream: Boolean = False);
var
  {$IFDEF TNTSUPPORT}
  FileStream: TTntFileStream;
  {$ELSE}
  FileStream: TFileStream;
  {$ENDIF}
begin
  FileStream := nil;
  try
    {$IFDEF TNTSUPPORT}
    FileStream := TTntFileStream.Create(FileName, fmCreate or fmShareExclusive);
    {$ELSE}
    FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    {$ENDIF}
    SaveToStream(FileStream, Version, ReadVerFromStream);
  finally
    FileStream.Free
  end;
end;

procedure TStreamableClass.SaveToStream(S: TStream; Version: integer;
  WriteVerToStream: Boolean);
begin
  if WriteVerToStream then
    S.WriteBuffer(Version, Sizeof(Version));
  FStreamVersion := Version;
end;


{ ----------------------------------------------------------------------------- }
{ Class that frees it self when the reference count goes to 0.  Like a com      }
{ object but the compiler does not inc/dec automaticlly                         }
{ ----------------------------------------------------------------------------- }

{ TReferenceCounted }

procedure TReferenceCounted.AddRef;
begin
  InterlockedIncrement(FRefCount)
end;

procedure TReferenceCounted.Release;
begin
  InterlockedDecrement (FRefCount);
  if FRefCount <= 0 then
    Free;
end;


{ ----------------------------------------------------------------------------- }
{ TList that frees it self when the reference count goes to 0.  Like a com      }
{ object but the compiler does not inc/dec automaticlly                         }
{ ----------------------------------------------------------------------------- }

{ TReferenceCountedList }

procedure TReferenceCountedList.AddRef;
begin
  InterlockedIncrement(FRefCount)
end;

procedure TReferenceCountedList.Release;
begin
  InterlockedDecrement (FRefCount);
  if FRefCount <= 0 then
    Free;
end;

{ TShellSortHelper }

function TShellSortHelper.CompareIDSort(SortColumn: integer; NS1,
  NS2: TNamespace): Integer;
begin
  if Assigned(NS1.ParentShellFolder) then
  begin
    Result := NS2.ComparePIDL(NS1.RelativePIDL, False, SortColumn);
    { If we are not sorting the Name column then do a sub-sort on the name if   }
    { the items are equal.                                                      }
    if (SortColumn > 0) and (Result = 0) then
       Result := WideCompareText(NS1.NameInFolder, NS2.NameInFolder)
  end else
    Result := 0;
end;

function TShellSortHelper.DiscriminateFolders(NS1,
  NS2: TNamespace): Integer;
begin
  Result := 0;
  if NS1.Folder xor NS2.Folder then
  begin
    if NS1.Folder and not NS2.Folder then
      Result := -1
    else
    if not NS1.Folder and NS2.Folder then
      Result := 1
  end
end;

function TShellSortHelper.SortFileSize(NS1, NS2: TNamespace): Integer;
begin
  Result := DiscriminateFolders(NS1, NS2);
  if Result = 0 then
  begin
    if NS1.SizeOfFileInt64 > NS2.SizeOfFileInt64 then
      Result := 1
    else
    if NS1.SizeOfFileInt64 < NS2.SizeOfFileInt64 then
      Result := -1
    else
       Result := CompareIDSort(0, NS1, NS2)
  end
end;

function TShellSortHelper.SortFileTime(FT1, FT2: TFileTime; NS1,
  NS2: TNamespace): Integer;
begin
  Result := DiscriminateFolders(NS1, NS2);
  if Result = 0 then
  begin
    Result := CompareFileTime(FT1, FT2);
    if Result = 0 then
       Result := CompareIDSort(0, NS1, NS2)
  end
end;

function TShellSortHelper.SortString(S1, S2: WideString; NS1,
  NS2: TNamespace): Integer;
begin
  Result := DiscriminateFolders(NS1, NS2);
  if Result = 0 then
  begin
    Result := WideCompareText(S1, S2);
    if Result = 0 then
      Result := CompareIDSort(0, NS1, NS2)
  end
end;

function TShellSortHelper.SortType(NS1, NS2: TNamespace): Integer;
begin
  if FileSort = fsFileType then
    Result := SortString(NS1.FileType, NS2.FileType, NS1, NS2)
  else begin
    { Must be  fsFileExtension }
    Result := DiscriminateFolders(NS1, NS2);
    if Result = 0 then
    begin
      if NS1.FileSystem and NS2.FileSystem then
      begin
        Result := SortString(ExtractFileExt(NS1.NameParseAddress), ExtractFileExt(NS2.NameParseAddress), NS1, NS2);
        if Result = 0 then
          CompareIDSort(0, NS1, NS2);  // Secondary sort
      end;
    end
  end
end;

{$IFNDEF COMPILER_5_UP}
{ TObjectList }

function TObjectList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
end;

constructor TObjectList.Create;
begin
  inherited Create;
  FOwnsObjects := True;
end;

constructor TObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartAt to Count - 1 do
    if (AExact and
        (Items[I].ClassType = AClass)) or
       (not AExact and
        Items[I].InheritsFrom(AClass)) then
    begin
      Result := I;
      break;
    end;
end;

function TObjectList.First: TObject;
begin
  Result := TObject(inherited First);
end;

function TObjectList.GetItem(Index: Integer): TObject;
begin
  Result := inherited Items[Index];
end;

function TObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TObjectList.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
end;

function TObjectList.Last: TObject;
begin
  Result := TObject(inherited Last);
end;

function TObjectList.Remove(AObject: TObject): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  inherited Items[Index] := AObject;
end;
{$ENDIF}

{ TVirtualNamespaceList }

function TVirtualNamespaceList.Add(ANamespace: TNamespace): Integer;
begin
  Result := inherited Add(ANamespace);
end;

procedure TVirtualNamespaceList.FillArray(var NamespaceArray: TNamespaceArray);
var
  I: Integer;
begin
  SetLength(NamespaceArray, Count);
  for I := 0 to Count - 1 do
    NamespaceArray[0] := Items[I];
end;

function TVirtualNamespaceList.GetItems(Index: Integer): TNamespace;
begin
  Result := TNamespace(inherited Items[Index]);
end;

function  TVirtualNamespaceList.IndexOf(ANamespace: TNamespace): Integer;
begin
  Result := inherited IndexOf(ANamespace);
end;

procedure TVirtualNamespaceList.Insert(Index: Integer; ANamespace: TNamespace);
begin
  inherited Insert(Index, ANamespace);
end;

procedure TVirtualNamespaceList.SetItems(Index: Integer; ANamespace: TNamespace);
begin
  inherited Items[Index] := ANamespace;
end;

{ TCommonShellContextMenu }
constructor TCommonShellContextMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Assert(AOwner is TWinControl, 'The Owner of a Shell Context Menu component must be a descendent of TWinControl');
  FMsgWnd := AOwner as TWinControl;
  {$IFDEF TNTSUPPORT}
  KeyStrings := TTntStringList.Create;
  {$ELSE}
  KeyStrings := TStringList.Create;
  {$ENDIF}
  MenuMap := TMenuItemMap.Create;
  KeyStrings.Duplicates := dupIgnore;
  KeyStrings.Sorted := True;
  Stub := CreateStub(Self, @TCommonShellContextMenu.DefMenuCreateCallback);
  ActivePIDLs := TCommonPIDLList.Create;
  ActivePIDLs.SharePIDLs := True;
  FRenameMenuItem := True;
  FPasteMenuItem := True;
  FShortCutMenuItem := True;
end;

destructor TCommonShellContextMenu.Destroy;
begin
  ActiveFolder := nil;
  CurrentContextMenu := nil;
  CurrentContextMenu2 := nil;
  DisposeStub(Stub);
  FreeAndNil(FActivePIDLs);
  FreeAndNil(FKeyStrings);
  FreeAndNil(FMenuMap);
  inherited Destroy;
end;

function TCommonShellContextMenu.BindToObject(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult;
begin
  Result := ActiveFolder.BindToObject(pidl, pbcReserved, riid, ppvOut);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('BindToObject - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.BindToStorage(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvObj{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult;
begin
  Result := ActiveFolder.BindToStorage(pidl, pbcReserved, riid, ppvObj);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('BindToStorage - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.CompareIDs(lParam: LPARAM; pidl1, pidl2: PItemIDList): HResult;
begin
  Result := ActiveFolder.CompareIDs(lParam, pidl1, pidl2);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('CompareIDs - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.CreateViewObject(hwndOwner: HWND; const riid: TIID; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult;
begin
  Result := ActiveFolder.CreateViewObject(hwndOwner, riid, ppvOut);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('CreateViewObject - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.DefMenuCreateCallback(const psf: IShellfolder; wnd: HWND; const pdtObj: IDataObject; uMsg: UINT; WParm: WParam; lParm: LParam): HResult;
var
  DoDefault: Boolean;
  IDO: IDataObject;
  QCMInfo: PQCMINFO;
  DFMICS: PDFMICS;
  MapCount, i: Integer;
  MergeOffset: UINT;
  {$IFDEF TNTSUPPORT}
  MenuItem: TTntMenuItem;
  {$ELSE}
  MenuItem: TMenuItem;
  {$ENDIF}
begin
  Result := E_NOTIMPL;
  DoDefault := True;
  case uMsg of
    DFM_MERGECONTEXTMENU:
      begin
        if cmeShellDefault in Extensions then
          Result := S_OK
        else
          Result := S_FALSE;
        QCMInfo := PQCMINFO( lParm);
        // This seems to be broken.  The items added "normally" (using idCmdFirst as is)
        // don't get registered and send a 0 when selected. DFM_MERGECONTEXTMENUTOP works
        // but the return in DFM_INVOKECOMMAND is the OFFSET of the idCmdFirst passed
        // so we have to make sure that the offset does not overlap the idCmdFirst we
        // use here so choose a large enough number.   If we increase idCmdFirst from
        // its passed value it seems to make it work and in DFM_INVOKECOMMAND we get that
        // the (Value - 1) rather then then Offset.
        MapCount := MenuMap.Count;
        MergeOffset := QCMInfo^.idCmdFirst;
        QCMInfo^.idCmdFirst := QCMInfo^.idCmdFirst + 200; // Allow up to 200 items to be added in DFM_MERGECONTEXTMENUTOP;
        DoMenuMerge(QCMInfo^.Menu, QCMInfo^.IndexMenu, QCMInfo^.idCmdFirst, QCMInfo^.idCmdLast, CFM_FLAGSToShellContextMenuFlags(WParm));
        // Fix the MenuIDs to be commonly offset once all merging is done
        if MenuMap.Count > MapCount then
          for i := MapCount to MenuMap.Count - 1 do
            MenuMap[i].MenuID := MenuMap[i].MenuID - MergeOffset;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_MERGECONTEXTMENU');
        {$ENDIF}
      end;
    DFM_INVOKECOMMAND:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_INVOKECOMMAND');
        {$ENDIF}
        case Cardinal( wParm) of
          DFM_CMD_DELETE:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_DELETE');
              {$ENDIF}
            end;
          DFM_CMD_CUT:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_CUT');
              {$ENDIF}
            end;
          DFM_CMD_COPY:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_COPY');
              {$ENDIF}
            end;
          DFM_CMD_CREATESHORTCUT:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_CREATESHORTCUT');
              {$ENDIF}
            end;
          DFM_CMD_PROPERTIES:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PROPERTIES');
              {$ENDIF}
            end;
          DFM_CMD_NEWFOLDER:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_NEWFOLDER');
              {$ENDIF}
            end;
          DFM_CMD_PASTE:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PASTE');
              {$ENDIF}
            end;
          DFM_CMD_VIEWLIST:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_VIEWLIST');
              {$ENDIF}
            end;
          DFM_CMD_VIEWDETAILS:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_VIEWDETAILS');
              {$ENDIF}
            end;
          DFM_CMD_PASTELINK:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PASTELINK');
              {$ENDIF}
            end;
          DFM_CMD_PASTESPECIAL:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PasteShortCut');
              {$ENDIF}
            end;
          DFM_CMD_MODALPROP:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_MODALPROP');
              {$ENDIF}
            end;
        end;
        if DoDefault then
          Result := S_FALSE
        else
          Result := S_OK
      end;
    DFM_CREATE:
      begin
        DoShow;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_CREATE');
        {$ENDIF}
      end;
    DFM_DESTROY:
      begin
        DoHide;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_DESTROY');
        {$ENDIF}
      end;
    DFM_GETHELPTEXTA:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_GETHELPTEXTA');
        {$ENDIF}
      end;
    DFM_MEASUREITEM:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_MEASUREITEM');
        {$ENDIF}
      end;
    DFM_DRAWITEM:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_DRAWITEM');
        {$ENDIF}
      end;
    DFM_INITMENUPOPUP:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_INITMENUPOPUP');
        {$ENDIF}
      end;
    DFM_VALIDATECMD:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_VALIDATECMD');
        {$ENDIF}
      end;
    DFM_MERGECONTEXTMENU_TOP:
      begin
       if cmeShellDefault in Extensions then
          Result := S_OK
        else
          Result := S_FALSE; 
        QCMInfo := PQCMINFO( lParm);
        // This seems to be broken.  The items added "normally" (using idCmdFirst as is)
        // don't get registered and send a 0 when selected. DFM_MERGECONTEXTMENUTOP works
        // but the return in DFM_INVOKECOMMAND is the OFFSET of the idCmdFirst passed
        // so we have to make sure that the offset does not overlap the idCmdFirst we
        // use here so choose a large enough number.   If we increase idCmdFirst from
        // its passed value it seems to make it work and in DFM_INVOKECOMMAND we get that
        // the (Value - 1) rather then then Offset.
        MapCount := MenuMap.Count;
        MergeOffset := QCMInfo^.idCmdFirst;
        QCMInfo^.idCmdFirst := QCMInfo^.idCmdFirst + 600; // Allow up to 200 items to be added in DFM_MERGECONTEXTMENU;
        DoMenuMergeTop(QCMInfo^.Menu, QCMInfo^.IndexMenu, QCMInfo^.idCmdFirst, QCMInfo^.idCmdLast, CFM_FLAGSToShellContextMenuFlags(WParm));
        DoMenuMergeBottom(QCMInfo^.Menu, GetMenuItemCount(QCMInfo^.Menu), QCMInfo^.idCmdFirst, QCMInfo^.idCmdLast, CFM_FLAGSToShellContextMenuFlags(WParm));
        // Fix the MenuIDs to be commonly offset once all merging is done
        if MenuMap.Count > MapCount then
          for i := MapCount to MenuMap.Count - 1 do
            MenuMap[i].MenuID := MenuMap[i].MenuID - MergeOffset;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_MERGECONTEXTMENU_TOP');
        {$ENDIF}
      end;
    DFM_MERGECONTEXTMENU_BOTTOM:
      begin
      // Only works in XP and up.  Faked in DFM_MERGECONTEXTMENU_TOP
       if cmeShellDefault in Extensions then
          Result := S_OK
        else
          Result := S_FALSE;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_MERGECONTEXTMENU_BOTTOM');
        {$ENDIF}
      end;
    DFM_GETHELPTEXTW:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_GETHELPTEXTW');
        {$ENDIF}
      end;
    DFM_INVOKECOMMANDEX:
      begin
        DFMICS := PDFMICS( lparm);
        if FindCommandId(wParm, MenuItem) then
          DoInvokeCommand(MenuItem, DFMICS^.pici);
        case Cardinal( wParm) of
          DFM_CMD_DELETE:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMANDEX: DFM_CMD_PROPERTIES');
              {$ENDIF}
              DoDelete(psf, pdtObj, DFMICS, DoDefault);
              // According to Jim Barry this is not implemented in the default handler
              // lower than Vista.  It seems Win98 OS1 does but lets be cautious
              if DoDefault and not IsWinVista then
              begin
                DeleteFiles_MP(pdtObj, MsgWnd.Handle, DFMICS.fMask and CMIC_MASK_SHIFT_DOWN = 0, True);
                DoDefault := False
              end
            end;
          DFM_CMD_PROPERTIES:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMANDEX: DFM_CMD_PROPERTIES');
              {$ENDIF}
              DoProperties(psf, pdtObj, DFMICS, DoDefault);
              if DoDefault and not IsWinVista then
              begin
                if Assigned(SHMultiFileProperties_MP) then
                begin
                  IDO := nil;
                  CreateFullyQualifiedShellDataObject(ActivePIDLs, False, IDO);
                  if Assigned(IDO) then
                    SHMultiFileProperties_MP(IDO, 0);
                end else
                  DesktopFolder.ShowPropertySheetMulti(MsgWnd, LocalNamespaces, False, True);
                DoDefault := False
              end
            end;
          DFM_CMD_CUT:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_CUT');
              {$ENDIF}
              DoCut(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_COPY:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_COPY');
              {$ENDIF}
              DoCopy(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_CREATESHORTCUT:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_CREATESHORTCUT');
              {$ENDIF}
              DoCreateShortCut(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_NEWFOLDER:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_NEWFOLDER');
              {$ENDIF}
              DoNewFolder(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_PASTE:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PASTE');
              {$ENDIF}
              DoPaste(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_PASTELINK:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PASTELINK');
              {$ENDIF}
              DoPasteLink(psf, pdtObj, DFMICS, DoDefault)
            end;
          DFM_CMD_PASTESPECIAL:
            begin
              {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
              SendDebug('DFM_INVOKECOMMAND: DFM_CMD_PasteShortCut');
              {$ENDIF}
              DoPasteShortCut(psf, pdtObj, DFMICS, DoDefault)
            end;
        end;
        if DoDefault then
          Result := S_FALSE
        else
          Result := S_OK;
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_INVOKECOMMANDEX');
        {$ENDIF}
      end;
    DFM_MAPCOMMANDNAME:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_MAPCOMMANDNAME');
        {$ENDIF}
      end;
    DFM_GETDEFSTATICID:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_GETDEFSTATICID');
        {$ENDIF}
      end;
    DFM_GETVERBW:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_GETVERBW');
        {$ENDIF}
      end;
    DFM_GETVERBA:
      begin
        {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
         SendDebug('DFM_GETVERBA');
        {$ENDIF}
      end;
  else
    {$IFDEF GXDEBUG_DEFMENUCREATE_CALLBACK}
    SendDebug('Undefined: ' + IntToStr(UMsg));
    {$ENDIF}
  end;
end;

function TCommonShellContextMenu.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
 Result := S_OK;
 {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('DragEnter - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.DragLeave: HResult;
begin
  Result := S_OK;
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('DragLeave - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := E_NOTIMPL;
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('DragOver - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := E_NOTIMPL;
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('Drop - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.DuplicateKey(Key: HKEY): HKEY;
begin
  if Assigned(RegOpenKeyExW_MP) then
    RegOpenKeyExW_MP(Key, '', 0, MAXIMUM_ALLOWED, Result)
  else
    RegOpenKeyExA(Key, '', 0, MAXIMUM_ALLOWED, Result)
end;

function TCommonShellContextMenu.EnumObjects(hwndOwner: HWND; grfFlags: DWORD; out EnumIDList: IEnumIDList): HResult;
begin
  Result := ActiveFolder.EnumObjects(hwndOwner, grfFlags, EnumIDList);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('EnumObjects - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

{$IFDEF TNTSUPPORT}
function TCommonShellContextMenu.FindCommandId(CmdID: UINT; var MenuItem: TTntMenuItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  MenuItem := nil;
  i := 0;
  while not Result and (i < MenuMap.Count) do
  begin
    Result := MenuMap[i].MenuID = CmdID;
    if Result then
      MenuItem := MenuMap[i].Item
    else
      Inc(i)
  end
end;
{$ELSE}
function TCommonShellContextMenu.FindCommandId(CmdID: UINT; var MenuItem: TMenuItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  MenuItem := nil;
  i := 0;
  while not Result and (i < MenuMap.Count) do
  begin
    Result := MenuMap[i].MenuID = CmdID;
    if Result then
      MenuItem := MenuMap[i].Item
    else
      Inc(i)
  end
end;
{$ENDIF}

function TCommonShellContextMenu.GetAttributesOf(cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT): HResult;
var
  RequestedFlags: UINT;
begin
  RequestedFlags := rgfInOut;
  Result := ActiveFolder.GetAttributesOf(cidl, apidl, rgfInOut);
  if RequestedFlags and SFGAO_CANCOPY <> 0 then
    if CopyValidated then
      rgfInOut := rgfInOut or SFGAO_CANCOPY;
  if RequestedFlags and SFGAO_CANMOVE <> 0 then
    if CutValidated then
      rgfInOut := rgfInOut or SFGAO_CANMOVE;
  if RequestedFlags and SFGAO_CANDELETE <> 0 then
    if CutValidated then
      rgfInOut := rgfInOut or SFGAO_CANDELETE;
  if not ShortCutMenuItem then
    rgfInOut := rgfInOut and not SFGAO_CANLINK
end;

function TCommonShellContextMenu.GetDisplayNameOf(pidl: PItemIDList; uFlags: DWORD; var lpName: TStrRet): HResult;
begin
  Result := ActiveFolder.GetDisplayNameOf(pidl, uFlags, lpName);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('GetDisplayNameOf - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.GetUIObjectOf(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList; const riid: TIID; prgfInOut: Pointer; out ppvOut{$IFNDEF COMPILER_5_UP}: Pointer{$ENDIF}): HResult;
var
  DataObject: IDataObject;
  NSList: TList;
  NS: TNamespace;
  NSArray: TNamespaceArray;
  i: Integer;
begin
  Result := E_NOINTERFACE;
  if FromDesktop then
  begin
    // Need to create a correctly defined fully qualified HDROP in the DataObject
    if IsEqualGUID(riid, IDataObject) then
    begin
      DataObject := nil;
      // Lots of extra work to call CreateFullyQualifiedShellDataObject but want to keep a common interface with that function
      SetLength(NSArray, ActivePIDLs.Count);  
      for i := 0 to ActivePIDLs.Count - 1 do
      begin
        NS := TNamespace.Create(ActivePIDLs[i], nil);
        NS.FreePIDLOnDestroy := False;
        NSArray[i] := NS;
      end;
      NSList := NamespaceToNamespaceList(NSArray);
      CreateFullyQualifiedShellDataObject(NSList, False, DataObject);
      NSList.Free;
      for i := 0 to Length(NSArray) - 1 do
        NSArray[i].Free;
      NSArray := nil;
      IDataObject( ppvOut) := DataObject;
      UIObjectOfDataObject := IDataObject( ppvOut);
      if Assigned(IDataObject( ppvOut)) then
        Result := S_OK
    end else
    if IsEqualGUID(riid, IDropTarget) then
    begin
      if  PasteMenuItem then
      begin
        // Need to support this to get a Paste menu item
        IDropTarget( ppvOut) := Self;
        UIObjectOfDropTarget := IDropTarget( ppvOut);
        Result := S_OK
      end
    end else
      Result := ActiveFolder.GetUIObjectOf(hwndOwner, cidl, apidl, riid, prgfInOut, ppvOut)
  end else
    Result := ActiveFolder.GetUIObjectOf(hwndOwner, cidl, apidl, riid, prgfInOut, ppvOut);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('GetUIObjectOf - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.ParseDisplayName(hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR; out pchEaten: ULONG; out ppidl: PItemIDList; var dwAttributes: ULONG): HResult;
begin
  Result := ActiveFolder.ParseDisplayName(hwndOwner, pbcReserved, lpszDisplayName, pchEaten, ppidl, dwAttributes);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('ParseDisplayName - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.SetNameOf(hwndOwner: HWND; pidl: PItemIDList; lpszName: POLEStr; uFlags: DWORD; var ppidlOut: PItemIDList): HResult;
begin
  Result := ActiveFolder.SetNameOf(hwndOwner, pidl, lpszName, uFlags, ppidlOut);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('SetNameOf - Result = ' + InttoStr(Result));
  {$ENDIF}
end;

function TCommonShellContextMenu.InternalShowContextMenu(Owner: TWinControl; ParentPIDL: PItemIDList; ChildPIDLs: TAbsolutePIDLArray; Verb: WideString; Position: PPoint = nil; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean;
//
// If ParentPIDL is nil the assumption is that the Parent is the
// Desktop and the PIDLs are fully qualified PIDLs.  If ParentPIDL is assigned then
// the PIDLs MUST be immediate children of the ParentPIDL
//
// Pass the Verb "VIRTUAL_DEFAULT" to select the Default Menu Item
//
//
const
  MaxVerbLen = 128;
type
  THKeyArray = array of HKey;

  procedure AddKey(Key: HKey; var KeyArray: THKeyArray);
  begin
    SetLength(KeyArray, Length(KeyArray) + 1);
    KeyArray[Length(KeyArray)-1] := Key
  end;

  procedure AddExtKey({$IFDEF TNTSUPPORT}Reg: TTntRegistry;{$ELSE}Reg: TRegistry;{$ENDIF} KeyName: String; var Keys: THKeyArray);
  begin
    if Reg.OpenKeyReadOnly(KeyName) then
    begin
      AddKey(DuplicateKey(Reg.CurrentKey), Keys);
      Reg.CloseKey;
    end
  end;

var
  Menu: hMenu;
  InvokeInfo: TCMInvokeCommandInfoEx;
  MenuCmd: Cardinal;
  x, y, i: integer;
  ShowConextMenu, Success: Boolean;
  Flags: Longword;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  OldMode: UINT;
  Keys: THKeyArray;
  Desktop: IShellFolder;
  DesktopPIDL, ChildrenPIDLs: PItemIDList;
  {$IFDEF TNTSUPPORT}
  Reg: TTntRegistry;
  {$ELSE}
  Reg: TRegistry;
  {$ENDIF}
  WS, CurVer: WideString;
  UnknownAdded: Boolean;
  ShiftDown, ControlDown: Boolean;
  VerbStrW: array[0..128] of WideChar;
  VerbStrA: array[0..128] of AnsiChar;
  lpVerbStrW: PWideChar;
  lpVerbStrA: PAnsiChar;
begin
  Result := False;
  DesktopPIDL := nil;
  Desktop := nil;
  begin
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    ActivePIDLs.Clear;
    for i := 0 to Length(ChildPIDLs) - 1 do
      ActivePIDLs.Add(ChildPIDLs[i]);     // These are shared
    try
      case ShiftKeyState of
        evsCurrent: ShiftDown := GetKeyState(VK_SHIFT) and $8000 <> 0;
        evsUp: ShiftDown := False;
      else
        ShiftDown := True
      end;
      ControlDown := GetKeyState(VK_CONTROL) and $8000 <> 0;

      Result := False;
      Assert(Assigned(Owner), 'To show a Context Menu using TNamespace you must pass a valid Owner TWinControl');
      if Assigned(Owner) then
      begin
        if Assigned(CDefFolderMenu_Create2_MP) then
        begin
          ContextMenu := nil;
          ContextMenu2 := nil;
          Result := False;
          if Assigned(Position) then
          begin
            x := Position.x;
            y := Position.y
          end else
          begin
            x := Mouse.CursorPos.X;  // Snag these fast. The mouse can move a fair amount
            y := Mouse.CursorPos.Y;  // before the popup menu is shown.
          end;
          FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);
          Menu := CreatePopupMenu;
          try
            Flags := CMF_NORMAL or CMF_EXPLORE;
            if RenameMenuItem then
               Flags := Flags or CMF_CANRENAME;

            if ShiftDown then
              Flags := Flags or CMF_EXTENDEDVERBS;

            if cmeDirectory in Extensions then
              AddMenuKey('Directory');
            if cmeDirBackground in Extensions then
              AddMenuKey('Directory\Background');
            if cmeFolder in Extensions then
              AddMenuKey('Folder');
            if cmeAsterik in Extensions then
              AddMenuKey('*');
            if cmeAllFilesystemObjects in Extensions then
              AddMenuKey('AllFilesystemObjects');


            // Need to rework this to do it right
            // 1) look at the extension under HKCR, read default value
            // 2) open key pointed to by above default value (file class)
            // 3) see if file class has a "CurVer" key
            // 4) if so open that key as the current version of the file class
            // 5) open HKCR/SystemFileAssociations for the file extension
            // 6) if 1 was unsuccessful (no association) use the "Unknown" key
            // 7) see if the extension or file class key have \PerceivedType key
            // 8) if so look in HKCR/SystemFileAssociations for a key of the perceived type from above
            //
            //  Aug 25th - If I just pass the ext OR the file class then the Merge Extended items works ("Open" for instance)

            UnknownAdded := False;
            {$IFDEF TNTSUPPORT}
            Reg := TTntRegistry.Create;
            {$ELSE}
            Reg := TRegistry.Create;
            {$ENDIF}
            try
              Reg.RootKey := HKEY_CLASSES_ROOT;
              for i := 0 to KeyStrings.Count - 1 do
              begin
                // Per discussion on Nikos messageboard by TiKo only add the .ext key IF the class type does not exist
                if (KeyStrings[i] <> '') then
                begin
                  if Reg.OpenKeyReadOnly(KeyStrings[i]) then
                  begin
                    // The extension key is open {.ext}
                    WS := Reg.ReadString('');
                    Reg.CloseKey;
                    if WS <> '' then
                    begin
                      if Reg.OpenKeyReadOnly(WS) then
                      begin
                        // The FileClass is open, see if there is mulitiple version and a current version
                        if Reg.KeyExists('CurVer') then
                        begin
                          if Reg.OpenKeyReadOnly('CurVer') then
                          begin
                            CurVer := Reg.ReadString('');
                            if Reg.OpenKeyReadOnly('\' + CurVer) then
                              AddKey(DuplicateKey(Reg.CurrentKey), Keys);
                          end;
                          Reg.CloseKey;
                        end else
                        begin
                          AddKey(DuplicateKey(Reg.CurrentKey), Keys);
                          Reg.CloseKey;
                        end
                      end else
                        AddExtKey(Reg, KeyStrings[i], Keys)
                    end else
                      AddExtKey(Reg, KeyStrings[i], Keys)
                  end else
                  begin
                    if not UnknownAdded and Reg.OpenKeyReadOnly('Unknown') then
                    begin
                      AddKey(DuplicateKey(Reg.CurrentKey), Keys);
                      Reg.CloseKey;
                      UnknownAdded := True
                    end
                  end;

                  if cmePerceivedType in Extensions then
                  begin
                    if Reg.OpenKeyReadOnly(KeyStrings[i]) then
                    begin
                      WS := Reg.ReadString('PerceivedType');
                      if WS <> '' then
                      begin
                        if Reg.OpenKeyReadOnly('\SystemFileAssociations\' + WS) then
                          AddKey(DuplicateKey(Reg.CurrentKey), Keys);
                      end;
                      Reg.CloseKey;
                    end
                  end
                end
              end
            finally
              Reg.CloseKey;
              Reg.Free
            end;

            Success := False;
            if not Assigned(ParentPIDL) and Assigned(ChildPIDLs) then
            begin
              FromDesktop := True;
              SHGetDesktopFolder(FActiveFolder);
              SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopPIDL);
              Success := CDefFolderMenu_Create2_MP(DesktopPIDL, Owner.Handle,
                Length(ChildPIDLs), PItemIDList(ChildPIDLs[0]), Self,
                Stub, Length(Keys), PHKey(@Keys[0]), ContextMenu) = S_OK;
            end else
            begin
              FromDesktop := False;
              SHGetDesktopFolder(Desktop);
              if PIDLMgr.IsDesktopFolder(ParentPIDL) then
                FActiveFolder := Desktop
              else
                Desktop.BindToObject(ParentPIDL, nil, IShellFolder, Pointer( FActiveFolder));

                if Assigned(ActiveFolder) then
                begin
                  if Assigned(ParentPIDL) and Assigned(ChildPIDLs) then
                    Success := CDefFolderMenu_Create2_MP(nil {ParentPIDL}, Owner.Handle,
                      Length(ChildPIDLs), PItemIDList(ChildPIDLs[0]),
                      Self, Stub, Length(Keys), PHKey(@Keys[0]), ContextMenu) = S_OK
                  else begin
                    // Must be a background menu call
                    ChildrenPIDLs := nil;
                    if Assigned(ParentPIDL) and not Assigned(ChildPIDLs) then
                      Success := CDefFolderMenu_Create2_MP(ParentPIDL, Owner.Handle,
                        0, ChildrenPIDLs, Self, Stub, Length(Keys),
                        PHKey(@Keys[0]), ContextMenu) = S_OK
                  end
                end
            end;

            if Success then
            begin

              CurrentContextMenu := ContextMenu;

              CurrentContextMenu2 := nil;  // not sure it is available yet
              if Assigned(ContextMenu) then
                if ContextMenu.QueryInterface(IID_IContextMenu2, Pointer(ContextMenu2)) <> E_NOINTERFACE then
                  CurrentContextMenu2 := ContextMenu2;

              if Assigned(ContextMenu2) then
                ContextMenu2.QueryContextMenu(Menu, 0, 1, $7FFF, Flags)
              else
              if Assigned(ContextMenu) then
                ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, Flags);

              // May just be executing a verb
              ShowConextMenu := Verb = '';

              if ShowConextMenu then
              begin
                FOldWndProcForContextMenu := Owner.WindowProc;
                // Hook the owner for the Window message for owner draw menus like
                // Send To..
                Owner.WindowProc := WindowProcForContextMenu;
                try
                  MenuCmd := Cardinal( TrackPopupMenuEx(
                                       Menu,
                                       TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON,
                                       x, y, Owner.Handle, nil))
                finally
                  // Try it again to see if the user pressed it while the menu was shown
                  case ShiftKeyState of
                    evsCurrent: ShiftDown := GetKeyState(VK_SHIFT) and $8000 <> 0;
                    evsUp: ShiftDown := False;
                    evsDown: ShiftDown := True
                  end;
                  ControlDown := GetKeyState(VK_CONTROL) and $8000 <> 0;
                  // Unhook the window ASAP
                  Owner.WindowProc := FOldWndProcForContextMenu;
                  FOldWndProcForContextMenu := nil;
                end
              end else
                MenuCmd := 0;

              // If the menu was shown and the user did not select anything then do nothing
              if not (ShowConextMenu and (MenuCmd = 0)) then
              begin
                FillChar(InvokeInfo, SizeOf(InvokeInfo), #0);
                 // Win95 get confused if size = TCMInvokeCommandInfoEx
                if IsUnicode then
                  InvokeInfo.cbSize := SizeOf(TCMInvokeCommandInfoEx)
                else
                  InvokeInfo.cbSize := SizeOf(TCMInvokeCommandInfo);
                if IsUnicode then
                  InvokeInfo.fMask := CMIC_MASK_UNICODE;
                InvokeInfo.hWnd := Owner.Handle;
                InvokeInfo.nShow := SW_SHOWNORMAL;

                if (MenuCmd > 0) and ShowConextMenu then
                begin
                  // The menu was shown and an item was selected
                  InvokeInfo.lpVerb := MakeIntResourceA(MenuCmd-1);
                  if IsUnicode then
                    InvokeInfo.lpVerbW := MakeIntResourceW(MenuCmd-1);
                end else
                if (Verb <> '') and not ShowConextMenu then
                begin
                  lpVerbStrW := @VerbStrW[0];
                  lpVerbStrA := @VerbStrA[0];
                  // Passed in a verb to execute
                  if Assigned(CurrentContextMenu2) then
                    MapVerbToIntResource(CurrentContextMenu2, Menu, Verb, lpVerbStrW, lpVerbStrA)
                  else
                    MapVerbToIntResource(CurrentContextMenu, Menu, Verb, lpVerbStrW, lpVerbStrA);
                  InvokeInfo.lpVerbW := lpVerbStrW;
                  InvokeInfo.lpVerb := lpVerbStrA;
                end;

                InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_ASYNCOK;
                if ShiftDown then
                  InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_SHIFT_DOWN;
                if ControlDown then
                  InvokeInfo.fMask := InvokeInfo.fMask or CMIC_MASK_CONTROL_DOWN;
                if Assigned(CurrentContextMenu2) then
                  Result := Succeeded(CurrentContextMenu2.InvokeCommand(InvokeInfo))
                else
                if Assigned(CurrentContextMenu) then
                  Result := Succeeded(CurrentContextMenu.InvokeCommand(InvokeInfo))
              end
            end
          finally
            PIDLMgr.FreePIDL(DesktopPIDL);
            ContextMenu := nil;
            ContextMenu2 := nil;
            CurrentContextMenu := nil;
            CurrentContextMenu2 := nil;
            FActiveFolder := nil;
            Desktop := nil;
            // Special handling because this is a non reference counted Component
            // If we select Properites the data object will be released
            // if not then the shell won't call that last _Release and the
            // data object is leaked.  If we try to compensate for it then
            // we will get a crash if we show the properties sheet.
            // Have to live with a small random memory leak.
            UIObjectOfDataObject := nil;
            UIObjectOfDropTarget := nil;
            { Don't access any properties or field of the object.  If the verb is     }
            { 'delete' the component using this class could have freed the instance   }
            { of the object through a ShellNotifyRegister or some other way.          }
            DestroyMenu(Menu);
            for i := 0 to Length(Keys) - 1 do
              RegCloseKey(Keys[i]);
            Keys := nil;
          end;
        end
      end;
      finally
        ActivePIDLs.Clear;
        SetErrorMode(OldMode);
      end
    except
    end
  end
end;

procedure TCommonShellContextMenu.AddMenuKey(Key: WideString);
begin
  KeyStrings.Add(Key)
end;

{$IFDEF TNTSUPPORT}
procedure TCommonShellContextMenu.AddMenuKeys(Keys: TTntStringList);
begin
  KeyStrings.Assign(Keys)
end;
{$ELSE}
procedure TCommonShellContextMenu.AddMenuKeys(Keys: TStringList);
begin
  KeyStrings.Assign(Keys)
end;
{$ENDIF}

procedure TCommonShellContextMenu.ClearKeys;
begin
  KeyStrings.Clear
end;

procedure TCommonShellContextMenu.ClearMenuMap;
begin
  MenuMap.Clear
end;

procedure TCommonShellContextMenu.DoCopy(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuCopy) then
    OnShellMenuCopy(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoCreateShortCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuCreateShortCut) then
    OnShellMenuCreateShortCut(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuCut) then
    OnShellMenuCut(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoDelete(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuDelete) then
    OnShellMenuDelete(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoHide;
begin
  if Assigned(OnHide) then
    OnHide(Self);
end;

{$IFDEF TNTSUPPORT}
procedure TCommonShellContextMenu.DoInvokeCommand(MenuItem: TTntMenuItem; InvokeInfo: PCMInvokeCommandInfo);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(OnInvokeCommand) then
    OnInvokeCommand(Self, MenuItem, InvokeInfo, Handled);
  if not Handled then
    MenuItem.Click;
end;
{$ELSE}
procedure TCommonShellContextMenu.DoInvokeCommand(MenuItem: TMenuItem; InvokeInfo: PCMInvokeCommandInfo);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(OnInvokeCommand) then
    OnInvokeCommand(Self, MenuItem, InvokeInfo, Handled);
  if not Handled then
    MenuItem.Click;
end;
{$ENDIF}

procedure TCommonShellContextMenu.DoMenuMerge(Menu: HMENU; IndexMenu: UINT;
  var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags);
begin
  if Assigned(OnMenuMerge) then
    OnMenuMerge(Self, Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellContextMenu.DoMenuMergeBottom(Menu: HMENU;
  IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT;
  Flags: TShellContextMenuFlags);
begin
  if Assigned(OnMenuMergeBottom) then
    OnMenuMergeBottom(Self, Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellContextMenu.DoMenuMergeTop(Menu: HMENU; IndexMenu: UINT;
  var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags);
begin
  if Assigned(OnMenuMergeTop) then
    OnMenuMergeTop(Self, Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellContextMenu.DoNewFolder(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuNewFolder) then
    OnShellMenuNewFolder(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoPaste(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuPaste) then
    OnShellMenuPaste(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoPasteLink(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuPasteLink) then
    OnShellMenuPasteLink(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoPasteShortCut(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuPasteShortCut) then
    OnShellMenuPasteShortCut(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoProperties(ShellFolder: IShellFolder; DataObject: IDataObject; DFMICS: PDFMICS; var DoDefault: Boolean);
begin
  if Assigned(OnShellMenuProperites) then
    OnShellMenuProperites(Self, ShellFolder, DataObject, DFMICS, DoDefault)
end;

procedure TCommonShellContextMenu.DoShow;
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TCommonShellContextMenu.HandleContextMenuMsg(Msg, wParam, lParam: Longint; var Result: LRESULT);
{ This is called when the ContextMenu calls back to its owner window to ask     }
{ questions to implement the addition of icons to the menu.  The messages sent  }
{ to the owner window are:  WM_INITMENUPOPUP, WM_DRAWITEM, or WM_MEASUREITEM.   }
{ Which must be passed on to the ContextMenu2 interface to display items with   }
{ icons.                                                                        }
var
  ContextMenu3: IContextMenu3;
begin   
  if Assigned(CurrentContextMenu2) then
    if CurrentContextMenu2.QueryInterface(IContextMenu3, ContextMenu3) <> E_NOINTERFACE then
      Result := ContextMenu3.HandleMenuMsg2(Msg, wParam, lParam, Result)
    else
      Result := CurrentContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
end;

procedure TCommonShellContextMenu.LoadMultiFolderPIDLArray(Namespaces: TNamespaceArray; var PIDLs: TAbsolutePIDLArray);
var
  i: Integer;
begin
  CopyValidated := True;
  CutValidated := True;
  DeleteValidated := True;
  SetLength(PIDLs, Length(Namespaces));
  for i := 0 to Length(Namespaces) - 1 do
  begin
    PIDLs[i] := Namespaces[i].AbsolutePIDL;
 //   if Namespaces[i].Directory and not Namespaces[i].Browsable then
 //     Include(FExtensions, cmeDirectory);
    if CopyValidated then
      CopyValidated := Namespaces[i].CanCopy;
    if CutValidated then
      CutValidated := Namespaces[i].CanMove;
    if DeleteValidated then
      DeleteValidated := Namespaces[i].CanDelete;
 //   if Namespaces[i].Folder and not Namespaces[i].Directory then
 //     Include(FExtensions, cmeFolder);
  end;
  if Assigned(LocalFocused) then
  begin
    if not LocalFocused.Browsable and LocalFocused.Folder then
    begin
      if LocalFocused.Directory then
        Include(FExtensions, cmeDirectory)
      else
       Include(FExtensions, cmeFolder)
    end
  end
end;

procedure TCommonShellContextMenu.WindowProcForContextMenu(var Message: TMessage);
begin
  FOldWndProcForContextMenu(Message); // Call the OldWindProc of the ContextMenu owner
  case Message.Msg of
    WM_DRAWITEM, WM_INITMENUPOPUP, WM_MEASUREITEM, WM_MENUCHAR:
      HandleContextMenuMsg(Message.Msg, Message.WParam, Message.LParam, Message.Result);
  end;
end;

function TCommonShellContextMenu._AddRef: Integer;
begin
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('_AddRef');
  {$ENDIF}
  if ReferenceCounted then
   Result := InterlockedIncrement(FRefCount)
  else
    Result := -1
end;

function TCommonShellContextMenu._Release: Integer;
begin
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('_Release');
  {$ENDIF}
  if ReferenceCounted then
  begin
    Result := InterlockedDecrement(FRefCount);
    if FRefCount = 0 then
      Destroy
  end else
    Result := -1
end;

function TCommonShellContextMenu.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  {$IFDEF GXDEBUG_VIRTUALCONTEXTMENU}
    SendDebug('QueryInterface - ' + GUIDToInterfaceStr(IID));
  {$ENDIF}
end;

{$IFDEF TNTSUPPORT}
function TCommonShellContextMenu.MergeMenuIntoContextMenu(Menu: TTntPopupMenu;
  ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
  //
  // Returns the ItemID of the last item it added to the ContextMenu
  //

  function RunMenu(MenuItem: TTntMenuItem; hPopupMenu: hMenu; MenuID: UINT): Integer;
  var
    i: Integer;
    SubMenu: hMenu;
    Map: PMenuItemLink;
    NewIndex: Integer;
  begin
    Result := MenuID;
    if MenuItem.Count > 0 then
    begin
      // Item has sub-items and won't take a MenuID
      SubMenu := CreatePopupMenu;
      NewIndex := AddContextMenuItem(hPopupMenu, MenuItem.Caption, Index, Result, SubMenu);
      for i := MenuItem.Count - 1 downto 0 do
        Result := RunMenu(MenuItem.Items[i] as TTntMenuItem, SubMenu, Result);
    end else
      NewIndex := AddContextMenuItem(hPopupMenu, MenuItem.Caption, Index, Result, 0);
    if NewIndex <> $FFFF then
    begin
      Map := MenuMap.Add;
      Map.MenuID := Result;
      Map.Item := MenuItem;
      Inc(Result)
    end;
  end;

var
  i: Integer;
begin
  Result := idStart;
  for i := Menu.Items.Count - 1 downto 0 do
    Result := RunMenu(Menu.Items[i] as TTntMenuItem, ContextMenu, Result)
end;
{$ELSE}
function TCommonShellContextMenu.MergeMenuIntoContextMenu(Menu: TPopupMenu;
  ContextMenu: HMenu; Index: Integer; idStart: UINT): Integer;
  //
  // Returns the ItemID of the last item it added to the ContextMenu
  //

  function RunMenu(MenuItem: TMenuItem; hPopupMenu: hMenu; MenuID: UINT): Integer;
  var
    i: Integer;
    SubMenu: hMenu;
    Map: PMenuItemLink;
    NewIndex: Integer;
  begin
    Result := MenuID;
    if MenuItem.Count > 0 then
    begin
      // Item has sub-items and won't take a MenuID
      SubMenu := CreatePopupMenu;
      NewIndex := AddContextMenuItem(hPopupMenu, MenuItem.Caption, Index, Result, SubMenu, MenuItem.Enabled, MenuItem.Checked, MenuItem.Default);
      for i := MenuItem.Count - 1 downto 0 do
        Result := RunMenu(MenuItem.Items[i], SubMenu, Result);
    end else
      NewIndex := AddContextMenuItem(hPopupMenu, MenuItem.Caption, Index, Result, 0, MenuItem.Enabled, MenuItem.Checked, MenuItem.Default);
    if NewIndex <> $FFFF then
    begin
      Map := MenuMap.Add;
      Map.MenuID := Result;
      Map.Item := MenuItem;
      Inc(Result)
    end;
  end;

var
  i: Integer;
begin
  Result := idStart;
  for i := Menu.Items.Count - 1 downto 0 do
    Result := RunMenu(Menu.Items[i], ContextMenu, Result)
end;
{$ENDIF}

{ TCommonShellMultiParentContextMenu}
function TCommonShellMultiParentContextMenu.ExecuteContextMenuVerb(Owner: TWinControl; Namespaces: TNamespaceArray; Verb: string; ShiftKeyState: TExecuteVerbShift = evsCurrent): Boolean;
var
  PIDLs: TAbsolutePIDLArray;
begin
  PIDLs := nil;
  LocalFocused := nil;
  LocalNamespaces := Namespaces;
  RenameMenuItem := True;
  ShortCutMenuItem := False;
  PasteMenuItem := False;
  LoadRegistryKeyStrings(Namespaces[0]);
  LoadMultiFolderPIDLArray(Namespaces, PIDLs);
  Result := InternalShowContextMenu(Owner, nil, PIDLs, Verb, nil, ShiftKeyState);
end;

function TCommonShellMultiParentContextMenu.ShowContextMenu(Owner: TWinControl; Focused: TNamespace; Namespaces: TNamespaceArray; Position: PPoint = nil): Boolean;
var
  PIDLs: TAbsolutePIDLArray;
begin
  PIDLs := nil;
  LocalFocused := Focused;
  LocalNamespaces := Namespaces;
  LoadRegistryKeyStrings(Focused);
  LoadMultiFolderPIDLArray(Namespaces, PIDLs);
  Result := InternalShowContextMenu(Owner, nil, PIDLs, '', Position);
  LocalFocused := nil;
end;

procedure TCommonShellMultiParentContextMenu.LoadRegistryKeyStrings(Focused: TNamespace);
begin
  KeyStrings.Clear;
  Extensions := [cmeShellDefault, cmeAsterik, cmeAllFilesystemObjects];

  // Only add the menu handlers for the focused item class
  if Assigned(Focused) then
  begin
    if Focused.Folder then
      Extensions := [cmeShellDefault, {cmeDirectory,} cmeFolder]
    else
      AddMenuKey(WideExtractFileExt(Focused.NameForParsing));
  end
end;

{ TCommonShellBackgroundContextMenu }
constructor TCommonShellBackgroundContextMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialItemList := TCommonPIDLList.Create;
  FFinalItemList := TCommonPIDLList.Create;
  FRenameMenuItem := False;
  FPasteMenuItem := False;
  FShowPasteItem := True;
  FShowPasteShortCutItem := True;
  FShowPropertiesItem := True;
  FAutoDetectNewItem := True;
  {$IFDEF TNTSUPPORT}
  PopupMenuProperties := TTntPopupMenu.Create(Self);
  PopupMenuPasteShortCut := TTntPopupMenu.Create(Self);
  PopupMenuPaste := TTntPopupMenu.Create(Self);
  Paste := TTntMenuItem.Create(PopupMenuPaste);
  PasteShortCut := TTntMenuItem.Create(PopupMenuPasteShortCut);
  Properties := TTntMenuItem.Create(PopupMenuProperties);
  {$ELSE}
  PopupMenuProperties := TPopupMenu.Create(Self);
  PopupMenuPasteShortCut := TPopupMenu.Create(Self);
  PopupMenuPaste := TPopupMenu.Create(Self);
  Paste := TMenuItem.Create(PopupMenuPaste);
  PasteShortCut := TMenuItem.Create(PopupMenuPasteShortCut);
  Properties := TMenuItem.Create(PopupMenuProperties);
  {$ENDIF}
  Paste.Caption := STR_PASTE;
  PasteShortCut.Caption := STR_PASTELINK;
  Properties.Caption := STR_PROPERTIES;
  Paste.OnClick := ClickPaste;
  PasteShortCut.OnClick := ClickPasteShortCut;
  Properties.OnClick := ClickProperties;
  PopupMenuProperties.Items.Add(Properties);
  PopupMenuPasteShortCut.Items.Add(PasteShortCut);
  PopupMenuPaste.Items.Add(Paste);
end;

destructor TCommonShellBackgroundContextMenu.Destroy;
begin
  FreeAndNil(FInitialItemList);
  FreeAndNil(FFinalItemList);
  inherited Destroy;
end;

function TCommonShellBackgroundContextMenu.EnumCallback(MessageWnd: HWnd;
  APIDL: PItemIDList; AParent: TNamespace; Data: Pointer; var Terminate: Boolean): Boolean;
begin
  // copy the relative PIDLs
  TCommonPIDLList( Data).Add(APIDL);
  Result := True;
end;


function TCommonShellBackgroundContextMenu.ShowContextMenu(
  Owner: TWinControl; Root: TNamespace; Position: PPoint): Boolean;
begin
  Result := False;
  LocalFocused := Root;
  LocalNamespaces := nil;
  if Assigned(Root) then
  begin
    if Root.Folder then
    begin
      RenameMenuItem := True;
      LoadRegistryKeyStrings(Root);
      Result := InternalShowContextMenu(Owner, Root.AbsolutePIDL, nil, '', Position);
    end
  end;
  LocalFocused := nil;
end;

procedure TCommonShellBackgroundContextMenu.ClickPaste(Sender: TObject);
var
  NSA: TNamespaceArray;
begin
  if Assigned(LocalFocused) then
  begin
    SetLength(NSA, 1);
    NSA[0] := LocalFocused;
    LocalFocused.Paste(MsgWnd, NSA, False)
  end
end;

procedure TCommonShellBackgroundContextMenu.ClickPasteShortCut(Sender: TObject);
var
  NSA: TNamespaceArray;
begin
  if Assigned(LocalFocused) then
  begin
    SetLength(NSA, 1);
    NSA[0] := LocalFocused;
    LocalFocused.Paste(MsgWnd, NSA, True)
  end
end;

procedure TCommonShellBackgroundContextMenu.ClickProperties(Sender: TObject);
begin
  if Assigned(LocalFocused) then
    LocalFocused.ShowPropertySheet(MsgWnd)
end;

procedure TCommonShellBackgroundContextMenu.DoHide;
var
  i: Integer;
  Found: Boolean;
  NS: TNamespace;
begin
  inherited DoHide;
  if AutoDetectNewItem and Assigned(LocalFocused) then
  begin
    LocalFocused.EnumerateFolder(0, True, True, False, EnumCallback, FinalItemList);
    if (InitialItemList.Count + 1) = FinalItemList.Count then
    begin
      // One item was added, time to find it.
      PIDLListQuickSort(InitialItemList, LocalFocused.ShellFolder, 0, InitialItemList.Count - 1);
      PIDLListQuickSort(FinalItemList, LocalFocused.ShellFolder, 0, FinalItemList.Count - 1);
      Found := False;
      i := 0;
      while not Found and (i < FinalItemList.Count) do
      begin
        if i = InitialItemList.Count then
          Found := True   // It is the last one in the FinalItemList
        else
          Found := LocalFocused.ShellFolder.CompareIDs(0, InitialItemList[i], FinalItemList[i]) <> 0;
        if Found then
        begin
          NS := TNamespace.Create(PIDLMgr.CopyPIDL(FinalItemList[i]), LocalFocused);
          DoNewItem(NS);
          NS.Free
        end;
        Inc(i)
      end
    end
  end;
  if Assigned(FinalItemList) then
    FinalItemList.Clear;
  if Assigned(InitialItemList) then
  InitialItemList.Clear;
end;

procedure TCommonShellBackgroundContextMenu.DoMenuMerge(Menu: HMENU;
  IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT;
  Flags: TShellContextMenuFlags);
begin
  inherited DoMenuMerge(Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellBackgroundContextMenu.DoMenuMergeBottom(Menu: HMENU;
  IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT;
  Flags: TShellContextMenuFlags);
begin
  if ShowPropertiesItem then
  begin
    if Assigned(LocalFocused) then
      Properties.Enabled := LocalFocused.HasPropSheet
    else
      Properties.Enabled := False;
    CmdFirst := MergeMenuIntoContextMenu(PopupMenuProperties, Menu, IndexMenu, CmdFirst);
    AddContextMenuItem(Menu, '-', IndexMenu);
  end;
  inherited DoMenuMergeBottom(Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellBackgroundContextMenu.DoMenuMergeTop(Menu: HMENU; IndexMenu: UINT; var CmdFirst: UINT; CmdLast: UINT; Flags: TShellContextMenuFlags);
begin
  Paste.Enabled := ClipboardContainsShellFormats;
  PasteShortCut.Enabled := ClipboardContainsShellFormats;
  if ShowPasteItem or ShowPasteShortCutItem then
    AddContextMenuItem(Menu, '-', IndexMenu);
  if ShowPasteItem then
    CmdFirst := MergeMenuIntoContextMenu(PopupMenuPaste, Menu, IndexMenu, CmdFirst);
  if ShowPasteShortCutItem then
    CmdFirst := MergeMenuIntoContextMenu(PopupMenuPasteShortCut, Menu, IndexMenu, CmdFirst);
  if ShowPasteItem or ShowPasteShortCutItem then
    AddContextMenuItem(Menu, '-', IndexMenu);

  inherited DoMenuMergeTop(Menu, IndexMenu, CmdFirst, CmdLast, Flags);
end;

procedure TCommonShellBackgroundContextMenu.DoNewItem(NS: TNamespace);
begin
  if Assigned(OnNewItem) then
    OnNewItem(Self, NS)
end;

procedure TCommonShellBackgroundContextMenu.DoShow;

begin
  inherited DoShow;
  if AutoDetectNewItem and Assigned(LocalFocused) then
  begin
    InitialItemList.Clear;
    LocalFocused.EnumerateFolder(0, True, True, False, EnumCallback, InitialItemList)
  end
end;

procedure TCommonShellBackgroundContextMenu.LoadRegistryKeyStrings(Focused: TNamespace);
begin
  CopyValidated := False;
  CutValidated := False;
  KeyStrings.Clear;
  PasteMenuItem := True;
  Extensions := [cmeShellDefault, cmeDirBackground];
end;

{ TMenuItemMap }
function TMenuItemMap.Add: PMenuItemLink;
begin
  New(Result);
  if Assigned(Result) then
    inherited Insert(Count, Result)
end;

function TMenuItemMap.First: PMenuItemLink;
begin
  Result := PMenuItemLink( inherited First)
end;

function TMenuItemMap.Get(Index: Integer): PMenuItemLink;
begin
  Result := PMenuItemLink( inherited Get(Index))
end;

function TMenuItemMap.IndexOf(Item: PMenuItemLink): Integer;
begin
  Result := inherited IndexOf(Item)
end;

function TMenuItemMap.Last: PMenuItemLink;
begin
  Result := PMenuItemLink( inherited Last)
end;

function TMenuItemMap.Remove(Item: PMenuItemLink): Integer;
begin
  Result := inherited Remove(Item)
end;

procedure TMenuItemMap.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      Dispose(  Items[i]);
  finally
    SetCount(0);
    SetCapacity(0);
  end;
end;

function TMenuItemMap.Insert(Index: Integer): PMenuItemLink;
begin
  New(Result);
  if Assigned(Result) then
    inherited Insert(Index, Result)
end;

procedure TMenuItemMap.Put(Index: Integer; Item: PMenuItemLink);
begin
  inherited Put(Index, Item)
end;

{ TExplorerThreadInstance }
function TExplorerThreadInstance.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  {$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE}
  SendDebug('TExplorerThreadInstance.QueryInterface' + GUIDToString(IID));
  {$ENDIF}
  if GetInterface(IID, Obj) then
  begin
    _AddRef;
    Result := S_OK
  end else
    Result := E_NOINTERFACE
end;

function TExplorerThreadInstance._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
  if RefCount > MaxRef then
    FMaxRef := RefCount;
  {$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE}
  SendDebug('TExplorerThreadInstance._AddRef: ' + IntToStr(FRefCount));
  {$ENDIF}
end;

function TExplorerThreadInstance._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  {$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE}
  SendDebug('TExplorerThreadInstance._Release: ' + IntToStr(FRefCount));
  {$ENDIF}
end;

{$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE_REFCOUNT}
var
  StringList: TStringList;
{$ENDIF}

{ TCommonColumnMap }
constructor TColumnMap.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TColumnMap.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TColumnMap.DefaultControlPanelColumnWidth(iColumn: Integer): Integer;
begin
  if Assigned(ControlPanelFolderEx) then
  begin
    if iColumn < Length(VET_DEFAULT_CONTROLPANELEX_COLUMNWIDTHS) then
      Result := VET_DEFAULT_CONTROLPANELEX_COLUMNWIDTHS[iColumn]
    else
      Result := VET_DEFAULT_COLUMNWIDTH
  end else
  begin
    if iColumn < Length(VET_DEFAULT_CONTROLPANEL_COLUMNWIDTHS) then
      Result := VET_DEFAULT_CONTROLPANEL_COLUMNWIDTHS[iColumn]
    else
      Result := VET_DEFAULT_COLUMNWIDTH
  end
end;

function TColumnMap.DefaultFileSystemColumnWidth(iColumn: Integer): Integer;
begin
  if iColumn < Length(VET_DEFAULT_COLUMNWIDTHS) then
    Result := VET_DEFAULT_COLUMNWIDTHS[iColumn]
  else
    Result := VET_DEFAULT_COLUMNWIDTH
end;

function TColumnMap.DefaultMyComputerColumnWidth(iColumn: Integer): Integer;
begin
  if iColumn < Length(VET_DEFAULT_DRIVES_COLUMNWIDTHS) then
    Result := VET_DEFAULT_DRIVES_COLUMNWIDTHS[iColumn]
  else
    Result := VET_DEFAULT_COLUMNWIDTH
end;

function TColumnMap.DefaultNetworkColumnWidth(iColumn: Integer): Integer;
begin
  if iColumn < Length(VET_DEFAULT_NETWORK_COLUMNWIDTHS) then
    Result := VET_DEFAULT_NETWORK_COLUMNWIDTHS[iColumn]
  else
    Result := VET_DEFAULT_COLUMNWIDTH
end;

function TColumnMap.FindByColumnIndex(FolderNS: TNamespace; iColumn: Integer): TColumnItem;
var
  Key: TPropertyKey;
  ShellFolder2: IShellFolder2;
begin
  Result := nil;
  if Assigned(FolderNS) then
  begin
    if FolderNS.DelegateNamespace then
      ShellFolder2 := FolderNS.DelagateNS.ShellFolder2
    else
      ShellFolder2 := FolderNS.ShellFolder2;
    if Assigned(ShellFolder2) then
    begin
      if not Succeeded( ShellFolder2.MapColumnToSCID(iColumn, TSHCOLUMNID( Key))) then
        Key := MapColumnToDefaultSCID( MapNamespaceToFolderMapType(FolderNS), iColumn)

    end else
      Key := MapColumnToDefaultSCID( MapNamespaceToFolderMapType(FolderNS), iColumn);
    Result := FindByPropertyKey(FolderNS, Key);
  end
end;

function TColumnMap.FindByPropertyKey(FolderNS: TNamespace; Key: TPropertyKey): TColumnItem;
var
  i: Integer;
  FolderType: TCommonFolderMapType;
begin
  Result := nil;
  FolderType := MapNamespaceToFolderMapType(FolderNS);
  i := GuidBinarySearch(FolderType, GUIDToString(Key.fmtid), Key.pid, Self, 0, Count - 1);
  if i > -1 then
    Result := ColumnItem[i]
end;

function TColumnMap.GetColumnItem(Index: Integer): TColumnItem;
begin
  Result := TColumnItem( List[Index])
end;

function TColumnMap.GetCount: Integer;
begin
  Result := List.Count
end;

function TColumnMap.MapColumnToDefaultSCID(FolderType: TCommonFolderMapType; i: Integer): TPropertyKey;
begin
  case FolderType of
    cftFileSystem: Result.fmtid := SCID_DEFAULT_FILESYSTEM_COLUMNS;
    cftMyComputer: Result.fmtid := SCID_DEFAULT_MYCOMPUTER_COLUMNS;
    cftNetwork: Result.fmtid := SCID_DEFAULT_NETWORK_COLUMNS;
    cftControlPanel: Result.fmtid := SCID_DEFAULT_CONTROLPANEL_COLUMNS;
  else
    Result.fmtid := SCID_DEFAULT_FILESYSTEM_COLUMNS;
  end;
  Result.pid := i;
end;

function TColumnMap.MapNamespaceToFolderMapType(NS: TNamespace): TCommonFolderMapType;
begin
  Result := cftUnknown;
  if Assigned(NS) then
  begin
    if NS.IsControlPanel then
      Result := cftControlPanel
    else
    if NS.IsNetworkNeighborhood then
      Result := cftNetwork
    else
    if NS.IsMyComputer then
      Result := cftMyComputer
    else
    if NS.FileSystem then
      Result := cftFileSystem
  end
end;

procedure TColumnMap.Add(Item: TColumnItem);
begin
  List.Add(Item); // Don't forget to sort after adding for the binary search to work
end;

procedure TColumnMap.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free
  finally
    List.Count := 0;
  end;
end;

procedure TColumnMap.DefaultColumnWidth(FolderType: TCommonFolderMapType; i: Integer);
begin
  case FolderType of
    cftFileSystem: DefaultFileSystemColumnWidth(i);
    cftMyComputer: DefaultMyComputerColumnWidth(i);
    cftNetwork: DefaultNetworkColumnWidth(i);
    cftControlPanel: DefaultControlPanelColumnWidth(i);
  end;
end;

procedure TColumnMap.ForceandLoadMapItem(FolderNS: TNamespace; ColumnWidths: TColumnWidthArray; FolderName: WideString; FolderType: TCommonFolderMapType; i: Integer; Key: TPropertyKey);
var
  Item: TColumnItem;
begin
  Item := FindByPropertyKey(FolderNS, Key);
  if not Assigned(Item) then
  begin
    Item := TColumnItem.Create;
    Item.Key := Key;
    Add(Item);
  end;
  Item.GUID := GUIDToString(Key.fmtid);
  if i < Length(ColumnWidths) then
    Item.Width := ColumnWidths[i]
  else
    Item.Width := 120;
  Item.Position := i;
  Item.Visible := csOnByDefault in FolderNS.DetailsGetDefaultColumnState(i);
  Item.FriendlyName := FolderNS.DetailsColumnTitle(i);
  Item.FolderName := FolderName;
  Item.FolderType := FolderType;
  Sort; // Need to keep it sorted for Find to work;
end;

procedure TColumnMap.MimicColumnSCID(ColumnNS: TNamespace; ColumnWidths: TColumnWidthArray; FolderName: WideString; FolderType: TCommonFolderMapType; i: Integer; Key: TPropertyKey);
begin
  ForceandLoadMapItem(ColumnNS, ColumnWidths, FolderName, FolderType, i, MapColumnToDefaultSCID(FolderType, i));
end;

procedure TColumnMap.RestoreDefaultNamespaceColumnMap(FolderName: WideString; ColumnNS: TNamespace; ColumnWidths: TColumnWidthArray);
var
  i: Integer;
  Key: TPropertyKey;
  ShellFolder2: IShellFolder2;
  FolderType: TCommonFolderMapType;
begin
  FolderType := MapNamespaceToFolderMapType(ColumnNS);
  if ColumnNS.DelegateNamespace then
    ShellFolder2 := ColumnNS.DelagateNS.ShellFolder2
  else
    ShellFolder2 := ColumnNS.ShellFolder2;
  for i := 0 to ColumnNS.DetailsSupportedColumns - 1 do
  begin
    if Assigned(ShellFolder2) then
    begin
      if Succeeded(ShellFolder2.MapColumnToSCID(i, TSHColumnID(Key))) then
        ForceandLoadMapItem(ColumnNS, ColumnWidths, FolderName, FolderType, i, Key)
      else
        MimicColumnSCID(ColumnNS, ColumnWidths, FolderName, FolderType, i, Key);
    end else
      MimicColumnSCID(ColumnNS, ColumnWidths, FolderName, FolderType, i, Key);
  end;
end;

procedure TColumnMap.RestoreDefaults;
var
  WidthArray: TColumnWidthArray;
  i: Integer;
begin
  Clear;
  
  for i := 0 to Length(WidthArray) - 1 do
    WidthArray[i] := DefaultNetworkColumnWidth(i);
  RestoreDefaultNamespaceColumnMap('Network Neighborhood', NetworkNeighborHoodFolder, WidthArray);

  for i := 0 to Length(WidthArray) - 1 do
    WidthArray[i] := DefaultMyComputerColumnWidth(i);
  RestoreDefaultNamespaceColumnMap('My Computer', DrivesFolder, WidthArray);

  for i := 0 to Length(WidthArray) - 1 do
    WidthArray[i] := DefaultFileSystemColumnWidth(i);
  RestoreDefaultNamespaceColumnMap('File Folder', DesktopFolder, WidthArray);

  for i := 0 to Length(WidthArray) - 1 do
    WidthArray[i] := DefaultControlPanelColumnWidth(i);
  if Assigned(ControlPanelFolderEx) then
    RestoreDefaultNamespaceColumnMap('Control Panel', ControlPanelFolderEx, WidthArray)
  else
    RestoreDefaultNamespaceColumnMap('Control Panel', ControlPanelFolder, WidthArray);

end;

procedure TColumnMap.SetColumnItem(Index: Integer; Value: TColumnItem);
begin
  List[Index] := Value
end;

procedure TColumnMap.Sort;
begin
  FList.Sort(@ColumnMapSortByGUID)
end;

var
  PIDL: PItemIDList = nil;

initialization
  CoInitialize(nil);
  FileIconInit(True);  // This MUST be before the Namespaces are created or it won't work because the IconCache may have an icon in from the namespace
  VET_ColumnWidths := VET_DEFAULT_COLUMNWIDTHS;
  if not LoadShell32Functions then
    Halt(0);
  LoadWideFunctions;
  PIDLMgr := TCommonPIDLManager.Create;
  if IsWinVista then
  begin
    DesktopFolder := CreateKnownFolderNamespace(FOLDERID_Desktop, False, False);
    RecycleBinFolder := CreateKnownFolderNamespace(FOLDERID_RecycleBinFolder, False, False);
    PhysicalDesktopFolder := CreateKnownFolderNamespace(FOLDERID_Desktop, False, True);
    DrivesFolder := CreateKnownFolderNamespace(FOLDERID_ComputerFolder, False, False);
    PrinterFolder := CreateKnownFolderNamespace(FOLDERID_PrintersFolder, False, False);
    HistoryFolder := CreateKnownFolderNamespace(FOLDERID_History, False, False);
    ControlPanelFolder :=  CreateKnownFolderNamespace(FOLDERID_ControlPanelFolder, False, False); // In Vista/Win7 this gets you the "All Control Panel Items" folder inside the main Control Panel
    PIDL := PathToPIDL(PATH_CONTROLPANEL);  // Try to get the real Control Panel main folder in Vista/Win7
    if Assigned(PIDL) then
      ControlPanelFolderEx := TNamespace.Create(PIDL, nil);
    NetworkNeighborHoodFolder :=  CreateKnownFolderNamespace(FOLDERID_NetworkFolder, False, False);
    TemplatesFolder := CreateKnownFolderNamespace(FOLDERID_Templates, False, False);
    MyDocumentsFolder := CreateKnownFolderNamespace(FOLDERID_Documents, False, False);
    FavoritesFolder := CreateKnownFolderNamespace(FOLDERID_Favorites, False, False);
    UserDocumentsFolder := CreateKnownFolderNamespace(FOLDERID_RoamingAppData, False, False);
    ProgramFilesFolder := CreateKnownFolderNamespace(FOLDERID_ProgramFiles, False, False);
    GamesFolder := CreateKnownFolderNamespace(FOLDERID_Games);  // an example of CreateKnownFolderNamespace  WARNING this can be NIL if not on Vista or up
    if IsWin7 then
    begin
      UserLibraryFolder := CreateKnownFolderNamespace(FOLDERID_UsersLibraries); // Only valid on Windows 7
    end;
  end else
  begin
    DesktopFolder := CreateSpecialNamespace(CSIDL_DESKTOP);
    RecycleBinFolder := CreateSpecialNamespace(CSIDL_BITBUCKET);
    PhysicalDesktopFolder := CreateSpecialNamespace(CSIDL_DESKTOPDIRECTORY);
    DrivesFolder := CreateSpecialNamespace(CSIDL_DRIVES);
    PrinterFolder := CreateSpecialNamespace(CSIDL_PRINTERS);
    HistoryFolder := CreateSpecialNamespace(CSIDL_HISTORY);
    ControlPanelFolder :=  CreateSpecialNamespace(CSIDL_CONTROLS);
    NetworkNeighborHoodFolder := CreateSpecialNamespace(CSIDL_NETWORK);
    TemplatesFolder := CreateSpecialNamespace(CSIDL_TEMPLATES);
    MyDocumentsFolder := CreateSpecialNamespace(CSIDL_PERSONAL);
    FavoritesFolder := CreateSpecialNamespace(CSIDL_FAVORITES);
    UserDocumentsFolder := CreateSpecialNamespace(CSIDL_APPDATA);
    ProgramFilesFolder := CreateSpecialNamespace(CSIDL_PROGRAMS);
  end;
  ExplorerThreadInstance := TExplorerThreadInstance.Create;

finalization
  FreeAndNil(GamesFolder);
  FreeAndNil(DesktopFolder);
  FreeAndNil(RecycleBinFolder);
  FreeAndNil(PhysicalDesktopFolder);
  FreeAndNil(DrivesFolder);
  FreeAndNil(HistoryFolder);
  FreeAndNil(PrinterFolder);
  FreeAndNil(ControlPanelFolder);
  FreeAndNil(NetworkNeighborHoodFolder);
  FreeAndNil(TemplatesFolder);
  FreeAndNil(MyDocumentsFolder);
  FreeAndNil(FavoritesFolder);
  FreeAndNil(UserDocumentsFolder);
  FreeAndNil(ProgramFilesFolder);
  FreeAndNil(UserLibraryFolder);
  FreeAndNil(ControlPanelFolderEx);
  FreeAndNil(PIDLMgr);
  {$IFDEF GXDEBUG_EXPLORERTHREADINSTANCE_REFCOUNT}
  StringList := TStringList.Create;
  if Assigned(MP_SHSetInstanceExplorer) then
    StringList.Add('MP_SHSetInstanceExplorer assigned')
  else
    StringList.Add('MP_SHSetInstanceExplorer not assigned');
  if Assigned(MP_SHGetInstanceExplorer) then
    StringList.Add('MP_SHGetInstanceExplorer assigned')
  else
    StringList.Add('MP_SHGetInstanceExplorer not assigned');
  if Assigned(MP_SHSetThreadRef) then
    StringList.Add('MP_SHSetThreadRef assigned')
  else
    StringList.Add('MP_SHSetThreadRef not assigned');
  if Assigned(MP_SHGetThreadRef) then
    StringList.Add('MP_SHGetThreadRef assigned')
  else
    StringList.Add('MP_SHGetThreadRef not assigned');
  if Assigned(MP_SHCreateThread) then
    StringList.Add('MP_SHCreateThread assigned')
  else
    StringList.Add('MP_SHCreateThread not assigned');
  if Assigned(MP_SHCreateThreadRef) then
    StringList.Add('MP_SHCreateThreadRef assigned')
  else
    StringList.Add('MP_SHCreateThreadRef not assigned');
  if Assigned(MP_SHReleaseThreadRef) then
    StringList.Add('MP_SHReleaseThreadRef assigned')
  else
    StringList.Add('MP_SHReleaseThreadRef not assigned');

  StringList.Add('Max ReferenceCount = ' + IntToStr(ExplorerThreadInstance.MaxRef));
  StringList.Add('ReferenceCount = ' + IntToStr(ExplorerThreadInstance.RefCount));
  StringList.SaveToFile('ExplorerInstance.txt');
  StringList.Free;
  ShellExecute(0, 'open', 'ExplorerInstance.txt', nil, nil, SW_NORMAL);
  {$ENDIF}

  FreeAndNil(ExplorerThreadInstance);
  CommonUnloadLibrary(Mpr);
  CoUninitialize;
end.










