{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  General storage unit - provides with a basic storage backend component to store application
  specific data. Descendants can provide specific backends for registry, INI-files, DB, XML,
  etc. Should be used to provide a common interface for storing data as is done in some of
  the JVCL components (eg. JvFormPlacement/JvFormStorage).

  This was requested in one of the comments of the JVCL 3.0 Survey Results.

  Paths
  =====
  Paths are relative to the current path. Paths are specified using backslashes (\) between
  individual folders and the value. Paths starting with a backslash are always relative to the root
  storage (application specific root, absolute root path).

  Dots (.) are used to reference parent folders with the following rules:
  * a single dot (.) refers to the current folder
  * each additional dot moves up a level in the folder hierarchie, ie. "....\Here" refers to a
    folder three levels up from the current where a sub folder/value name "Here" is searched. Of
    course the normal (OS path) specification can be used as well ("..\..\..\Here" would be the
    same as the first example).

  Multiple backslashes without names between them are ignored ("Root\\Here" is the same as
  "Root\Here").

  Storage hierarchies
  ===================
  Each storage allows you add an unlimited number of sub storages. A sub storage is a symbolic
  link between a path in a storage to another storage (which in turn can also provide sub storages).

  Suppose you want to store both generic as well as user specific settings. This can be accomplished
  with two stores, one for the generic settings and one specific for the current user. The generic
  store (referred to as 'asRegBackend' from now on) will link to the user specific store (referred
  to as 'asUserIniBackend' from now on) using asRegBackend.SubStorages. The RootPath for the
  asUserIniBackend sub-store link will be set to 'UserSettings'. From that point on, any reference
  to a sub path of '\UserSettings' from the asRegBackend storage will be handed over to the
  asUserIniBackend storage. Examples:

  Path                          Target
  ====                          ======
  \WinPath                      asRegBackend:'\WinPath'
  \Generic\UserSettings\Me      asRegBackend:'\Generic\UserSettings\Me'
  \UserSettings                 asRegBackend:'\UserSettings'
  \UserSettings\FirstName       asUserIniBackend:'\FirstName'
  \UserSettings\Sub1\Sub1.1     asUserIniBackend:'\Sub1\Sub1.1'

  Because all settings can be read from a single store (from the application's perspective) you have
  created the option to keep your settings storage and retrieval code simple and easy to understand.
  Upon startup you can set asUserIniBackend to the correct INI file for the user that has logged on,
  and you are ready to read in the settings of that user.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppStorage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  JclStrings, // must be included before WideStrings
  {$IFDEF COMPILER10_UP}
  WideStrings,
  {$ENDIF COMPILER10_UP}
  SysUtils, Classes, TypInfo, Variants,
  JclBase,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvComponentBase, JvTypes, JvTranslateString;

const
  // (rom) this name is shared in several units and should be made global
  cItem = 'Item';
  cVersionCheckName = 'Version';

type
  TJvCustomAppStorage = class;
  TJvAppStorage = class;
  TJvCustomAppStorageOptions = class;
  TJvAppSubStorages = class;
  TJvAppSubStorage = class;

  EJVCLAppStorageError = class(EJVCLException);

  { TAppStorage does not automatically store published properties of a class that
    supports the IJvAppStorageHandler interface. Instead it invokes the Read and
    Write methods. }
  IJvAppStorageHandler = interface
    ['{E3754817-49A3-4612-A228-5D44A088681D}']
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  end;

  { TAppStorage automatically stores published properties of a class that
    supports the IJvAppStoragePublishedProps interface, even if the class
    supports the IJvAppStorageHandler interface, too. }
  IJvAppStoragePublishedProps = interface
    ['{0211AEF7-CCE9-4F13-B3CE-287251C89182}']
  end;

  TJvAppStorageListItemEvent = procedure(Sender: TJvCustomAppStorage; const Path: string;
    const List: TObject; const Index: Integer; const ItemName: string) of object;
  TJvAppStorageListDeleteEvent = procedure(Sender: TJvCustomAppStorage; const Path: string;
    const List: TObject; const First, Last: Integer; const ItemName: string) of object;
  TJvAppStoragePropTranslateEvent = procedure(Sender: TJvCustomAppStorage; Instance: TPersistent;
    var Name: string; const Reading: Boolean) of object;
  TJvAppStorageCryptEvent = procedure(var Value: string) of object;
  TJvAppStorageGetFileNameEvent = procedure(Sender: TJvCustomAppStorage;
    var FileName: TFileName) of object;
  TJvAppStorageObjectListItemCreateEvent = function(Sender: TJvCustomAppStorage; const Path: string; Index: Integer):
    TPersistent of object;
  TJvAppStorageErrorEvent = procedure(Sender: TObject; const Value: string) of object;

  TJvAppStorageOptionsClass = class of TJvCustomAppStorageOptions;

  TJvAppStorageEnumOption = (
    aeoFolders, // report folders
    aeoValues, // report values
    aeoReportListAsValue, // report list as value (a list is actually a folder containing a Count and Item? values)
    aeoReportRelative, // report all found folders and values relative to the requested path (otherwise relative to the Root path)
    aeoRecursive); // scan sub folders as well
  TJvAppStorageEnumOptions = set of TJvAppStorageEnumOption;

  TSynchronizeMethod = procedure of object;

  TFileLocation = (
    flCustom, // FileName property will contain full path
{$IFDEF MSWINDOWS}
    flWindows, // Store in %WINDOWS%; only use file name part of FileName property.
{$ENDIF MSWINDOWS}
    flTemp, // Store in %TEMP%; only use file name part of FileName property.
    flExeFile, // Store in same folder as application's exe file; only use file name part of FileName property.
    flUserFolder);
  // Store in %USER%\Application Data. Use the FileName property if it's a relative path or only the file name part of FileName property.

  TJvCustomAppStorage = class(TJvComponent)
  private
    CachedFormatSettings: TFormatSettings;
    FRoot: string;
    FCurPath: string;
    FStorageOptions: TJvCustomAppStorageOptions;
    FSubStorages: TJvAppSubStorages;
    FOnTranslatePropertyName: TJvAppStoragePropTranslateEvent;
    FOnEncryptPropertyValue: TJvAppStorageCryptEvent;
    FOnDecryptPropertyValue: TJvAppStorageCryptEvent;
    FCryptEnabledStatus: Integer;
    FAutoFlush: Boolean;
    FUpdateCount: Integer;
    FAutoReload: Boolean;
    FCurrentInstanceCreateEvent: TJvAppStorageObjectListItemCreateEvent;
    FInternalTranslateStringEngine: TJvTranslateString;
    FReadOnly: Boolean;
    FOnError: TJvAppStorageErrorEvent;
    FTranslateStringEngine: TJvTranslateString;
    FSynchronizeFlushReload: Boolean;
    function GetActiveTranslateStringEngine: TJvTranslateString;
    function GetUpdating: Boolean;
    procedure SetTranslateStringEngine(const Value: TJvTranslateString);
  protected
    FFlushOnDestroy: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    //1 Synchronize the Flush and Reload procedure
    /// Defines if the execution of flush and reload for the current
    /// AppStoragePath should be synchronized via a global mutex
    /// This property should be published in the dependent classes
    /// The procedure Synchronize could be used in the dependent class
    /// to implement the synchronisation
    property SynchronizeFlushReload: Boolean read FSynchronizeFlushReload write
        FSynchronizeFlushReload default False;
    //1 Synchronize the execution of an method using a JclMutex
    procedure Synchronize(AMethod: TSynchronizeMethod; AIdentifier: String);

    { Sets the value of FFlushOnDestroy. Derived classes may override this
      method to prevent it from changing or add extra behaviour to it. }
    procedure SetFlushOnDestroy(Value: Boolean); virtual;

    //Returns the property count of an instance
    function GetPropCount(Instance: TPersistent): Integer;
    //Returns the property name of an instance at a certain index
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetStorageOptionsClass: TJvAppStorageOptionsClass; virtual;
    { Split the specified path into an absolute path and a value name (the last item in the path
      string). Just a helper for all the storage methods. }
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); virtual;
    { SubStorages property set method. Does nothing. }
    procedure SetSubStorages(Value: TJvAppSubStorages);
    { Retrieve application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    function GetRoot: string;
    { Set application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    procedure SetRoot(const Value: string);
    { Retrieves currently set path (including the Root path). }
    function GetCurrentPath: string;
    { Returns the path as an absolute path (including the Root path). If the given path does not
      start with a backslash (\) the path is appended to the Root path, resolving any references to
      parent folders. }
    function GetAbsPath(const Path: string): string;
    { StringList item reader used by ReadStringList in the call to ReadList. }
    procedure ReadStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item writer used by WriteStringList in the call to WriteList. }
    procedure WriteStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item deleter used by WriteStringList in the call to WriteList. }
    procedure DeleteStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Default Function for creating a new Object. The classname could be received from the AppStorage using the Path "Classname" }
    function DefaultObjectListItemCreateEvent(Sender: TJvCustomAppStorage; const Path: string; Index: Integer):
      TPersistent;

    { ObjectList item reader used by ReadObjectList in the call to ReadList. }
    procedure ReadObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { ObjectList item writer used by WriteObjectList in the call to WriteList. }
    procedure WriteObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { ObjectList item deleter used by WriteObjectList in the call to WriteList. }
    procedure DeleteObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);
    {$IFDEF COMPILER10_UP}
    { StringList item reader used by ReadWideStringList in the call to ReadList. }
    procedure ReadWideStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item writer used by WriteStringList in the call to WriteList. }
    procedure WriteWideStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item deleter used by WriteStringList in the call to WriteList. }
    procedure DeleteWideStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);
    {$ENDIF}

    { StringList item reader used by ReadStringObjectList in the call to ReadList. }
    procedure ReadStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item writer used by WriteStringObjectList in the call to WriteList. }
    procedure WriteStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item deleter used by WriteStringObjectList in the call to WriteList. }
    procedure DeleteStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Collection item reader used by ReadCollection in the call to ReadList. }
    procedure ReadCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { Collection item writer used by WriteCollection in the call to WriteList. }
    procedure WriteCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { Collection item deleter used by WriteCollection in the call to WriteList. }
    procedure DeleteCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Enum all folders in the specified folder. }
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Enum all values below in the specified folder. }
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Internal retrieval of GetStoredValues. Is used to handle recursiveness. }
    procedure InternalGetStoredValues(const PrefixPath, SearchPath: string;
      const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
    { Current root path for storage. Paths used in other methods are relative to this path. }
    function GetPath: string;
    { Specify a new root. Given path is relative to the current path. Se remarks above }
    procedure SetPath(const Path: string);
    { Determines if the specified name belongs to a list value. }
    class function NameIsListItem(const Name: string): Boolean;
    { Application specific root. Path is prepended to any specified path and serves as an absolute
      root for any reading/writing. Not all implementation will use it. Generally it's used for
      storages not specific to an application (such as the registry). }
    property Root: string read GetRoot write SetRoot;
    { Set the StorageOptions Property }
    procedure SetStorageOptions(Value: TJvCustomAppStorageOptions);
    { Invokes the OnTranslatePropertyName event if one is assigned. }
    procedure DoTranslatePropertyName(Instance: TPersistent; var Name: string;
      const Reading: Boolean);
    { Determines if the specified is a sub store of this storage (will scan the entire sub storage
      hierarchy. }
    function HasSubStorage(AStore: TJvCustomAppStorage): Boolean;

    { Determines if the path represents a folder (ignores sub stores) }
    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; virtual; abstract;
    { Determines if the specified path exists (ignores sub stores) }
    function PathExistsInt(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified value is stored (ignores sub stores) }
    function ValueStoredInt(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified list is stored (ignores sub stores) }
    function ListStoredInt(const Path: string; const ItemName: string = cItem):
        Boolean; virtual;
    { Deletes the specified value. If the value wasn't stored, nothing will happen (ignores sub
      stores). }
    procedure DeleteValueInt(const Path: string); virtual; abstract;
    { Deletes all values and sub folders of the specified folder including the folder itself
      (ignores sub stores). }
    procedure DeleteSubTreeInt(const Path: string); virtual; abstract;
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function DoReadInteger(const Path: string; Default: Integer): Integer; virtual; abstract;
    { Stores an Integer value. }
    procedure DoWriteInteger(const Path: string; Value: Integer); virtual; abstract;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function DoReadFloat(const Path: string; Default: Extended): Extended; virtual; abstract;
    { Stores an Extended value. }
    procedure DoWriteFloat(const Path: string; Value: Extended); virtual; abstract;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function DoReadString(const Path: string; const Default: string): string; virtual; abstract;
    { Stores an string value. }
    procedure DoWriteString(const Path: string; const Value: string); virtual; abstract;

    { Retrieves the specified widestring value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function DoReadWideString(const Path: string; const Default: Widestring): Widestring; virtual;
    { Stores an widestring value. }
    procedure DoWriteWideString(const Path: string; const Value: Widestring); virtual;

    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function DoReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer; virtual; abstract;
    { Stores a buffer. }
    procedure DoWriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer); virtual; abstract;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function DoReadDateTime(const Path: string; Default: TDateTime): TDateTime; virtual;
    { Stores a TDateTime value (ignores sub stores). }
    procedure DoWriteDateTime(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not a Boolean (or can't be converted to an Boolean an
      EConvertError exception will be raised. }
    function DoReadBoolean(const Path: string; Default: Boolean): Boolean; virtual;
    { Stores a Boolean value. }
    procedure DoWriteBoolean(const Path: string; Value: Boolean); virtual;

    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadIntegerInt(const Path: string; Default: Integer): Integer; virtual;
    { Stores an Integer value (ignores sub stores). }
    procedure WriteIntegerInt(const Path: string; Value: Integer); virtual;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadFloatInt(const Path: string; Default: Extended): Extended; virtual;
    { Stores an Extended value (ignores sub stores). }
    procedure WriteFloatInt(const Path: string; Value: Extended); virtual;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised (ignores sub stores). }
    function ReadStringInt(const Path: string; const Default: string): string; virtual;
    { Stores an string value (ignores sub stores). }
    procedure WriteStringInt(const Path: string; const Value: string); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved (ignores sub stores). }
    function ReadBinaryInt(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer; virtual;
    { Stores a buffer (ignores sub stores). }
    procedure WriteBinaryInt(const Path: string; const Buf: TJvBytes; BufSize: Integer); virtual;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime; virtual;
    { Stores a TDateTime value (ignores sub stores). }
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not a Boolean (or can't be converted to an Boolean an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadBooleanInt(const Path: string; Default: Boolean): Boolean; virtual;
    { Stores a Boolean value (ignores sub stores). }
    procedure WriteBooleanInt(const Path: string; Value: Boolean); virtual;
    { Retrieves an enumeration. If the value is not found, the Default will be returned (ignores sub
      stores). }
    procedure ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores an enumeration (ignores sub stores). }
    procedure WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Value); virtual;
    { Retrieves a set. If the value is not found, the Default will be returned (ignores sub
      stores). }
    procedure ReadSetInt(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value); virtual;
    { Stores a set (ignores sub stores). }
    procedure WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value); virtual;

    function EncryptPropertyValue(Value: string): string;
    function DecryptPropertyValue(Value: string): string;

    procedure SetReadOnly(Value: Boolean);
    function GetReadOnly: Boolean;
    function GetPhysicalReadOnly: Boolean; virtual;

    property SubStorages: TJvAppSubStorages read FSubStorages write SetSubStorages;
    function DecodeStrToDateTime(Value: string): TDateTime; virtual;
    function EncodeDateTimeToStr(Value: TDateTime): string; virtual;
    procedure Loaded; override;
    procedure DoError(const msg: string);
    function GetFormatSettings: TFormatSettings;
    function ReadListItemCount(const Path: string; const ItemName: string = cItem):
        Integer; virtual;
    procedure WriteListItemCount(const Path: string; const ItemCount: Integer;
        const ItemName: string = cItem); virtual;
    // Change the ReadOnly CurrentInstanceCreateEvent Event
    procedure SetCurrentInstanceCreateEvent(const Value:
        TJvAppStorageObjectListItemCreateEvent);
    property CurrentInstanceCreateEvent: TJvAppStorageObjectListItemCreateEvent
        read FCurrentInstanceCreateEvent;
  public
    {$IFDEF SUPPORTS_CLASS_CTORDTORS}
    class destructor Destroy;
    {$ENDIF SUPPORTS_CLASS_CTORDTORS}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // (p3) moved Flush, Reload and AutoFlush to the base storage because users
    // should be able to call Flush and Reload as needed without being dependant on whether
    // the spcific storage implements it or not. Also made them virtual - if Flush and Reload
    // doesn't make sense for a specific storage, it shouldn't have to implement them
    procedure Flush; virtual;
    procedure Reload; virtual;
    // Do a Reload if the function ReloadNeeded returns true
    procedure ReloadIfNeeded;
    function ReloadNeeded: Boolean; virtual;
    //1 Do a Flush if the function FlushNeeded returns true
    procedure FlushIfNeeded;
    function FlushNeeded: Boolean; virtual;
    //1 Disables all AutoFlush/AutoReload activities
    procedure BeginUpdate;
    //1 Reanables all AutoFlush/AutoReload activities
    procedure EndUpdate;
    //1 Property to show whether the storage is in a beginupdate/endupdate block or not
    property IsUpdating: Boolean read GetUpdating;
    // Property to define that after every change in the data this
    // changes should automaticly stored
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush default False;
    // Property to define that before any data is read from the storage
    // the contents should be reread
    property AutoReload: Boolean read FAutoReload write FAutoReload default False;
    { This procedure gives the possibility to delete a tree out of the Appstorage
      depending on a stored value named "Version".
      Path is the path of the storage, VersionNumber is the Value which is compared.
      DeleteIfNotEqual when the Subtree is delete:
         TRUE : The tree is deleted when the stored value is not equal the parameter
                VersionNumber
         FALSE : The tree is deleted when the stored value is less the parameter
                VersionNumber
      WriteVersionNumber: This parameter determines if the Value of VersionNumber
         should be stored in the subtree after the check.
      VersionName is the Name of the stored version number in the path }
    procedure CheckDeletePathByVersion(const Path: string; VersionNumber: Integer;
      DeleteIfNotEqual: Boolean = False; WriteVersionNumber: Boolean = True;
      const VersionName: string = cVersionCheckName);
    class function ConcatPaths(const Paths: array of string): string;
    { Resolve a path to it's actual used storage backend and root path. }
    procedure ResolvePath(const InPath: string; out TargetStore: TJvCustomAppStorage; out TargetPath: string);
    { Determines if the path represents a folder }
    function IsFolder(const Path: string; ListIsValue: Boolean = True): Boolean;
    { Determines if the specified path exists }
    function PathExists(const Path: string): Boolean;
    { Determines if the specified value is stored }
    function ValueStored(const Path: string): Boolean;
    { Determines if the specified list is stored }
    function ListStored(const Path: string; const ItemName: string = cItem):
        Boolean;
    { Deletes the specified value. If the value wasn't stored, nothing will happen. }
    procedure DeleteValue(const Path: string);
    { Deletes all values and sub folders of the specified folder including the folder itself. }
    procedure DeleteSubTree(const Path: string);
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadInteger(const Path: string; Default: Integer = 0): Integer;
    { Stores an Integer value. }
    procedure WriteInteger(const Path: string; Value: Integer);
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function ReadFloat(const Path: string; Default: Extended = 0): Extended;
    { Stores an Extended value. }
    procedure WriteFloat(const Path: string; Value: Extended);
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function ReadString(const Path: string; const Default: string = ''): string;
    { Stores an string value. }
    procedure WriteString(const Path: string; const Value: string);
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTime(const Path: string; Default: TDateTime = 0): TDateTime;
    { Stores a TDateTime value. }
    procedure WriteDateTime(const Path: string; Value: TDateTime);
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
    { Stores a buffer. }
    procedure WriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer);
    { Retrieves the specified list. Caller provides a callback method that will read the individual
      items. ReadList will first determine the number of items to read and calls the specified
      method for each item. }
    function ReadList(const Path: string;
      const List: TObject; const OnReadItem: TJvAppStorageListItemEvent;
      const ItemName: string = cItem): Integer;
    { Stores a list of items. The number of items is stored first. For each item the provided
      item write method is called. Any additional items in the list (from a previous write) will be
      removed by the optionally provided delete method. }
    procedure WriteList(const Path: string; const List: TObject; const ItemCount: Integer;
      const OnWriteItem: TJvAppStorageListItemEvent;
      const OnDeleteItems: TJvAppStorageListDeleteEvent = nil;
      const ItemName: string = cItem);

    { Retrieves a list of objects. The list is optionally cleared before before reading starts.
      The ObjectType of the Object is retrieved from the stored "Classname" value.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadObjectList(const Path: string; List: TList;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer; overload;
    { Retrieves a list of objects. The list is optionally cleared before before reading starts.
      The ObjectType of the Object is defined by the ItemCreator-Event.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadObjectList(const Path: string; List: TList;
      ItemCreator: TJvAppStorageObjectListItemCreateEvent;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer; overload;
    { Stores a list of objects. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteObjectList(const Path: string; List: TList; const ItemName: string = cItem);

    { Retrieves a list of collection items . The list is optionally cleared before before reading starts.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadCollection(const Path: string; List: TCollection;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
    { Stores all items of a collection. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteCollection(const Path: string; List: TCollection; const ItemName: string = cItem);

    { Retrieves a string list with addition objects.
      The ObjectType of the Object is retrieved from the stored "Classname" value.
      The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringObjectList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True;
      const ItemName: string = cItem): Integer; overload;
    { Retrieves a string list with addition objects.
      The ObjectType of the Object is defined by the ItemCreator-Event.
      The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringObjectList(const Path: string; const SL: TStrings;
      ItemCreator: TJvAppStorageObjectListItemCreateEvent;
      const ClearFirst: Boolean = True;
      const ItemName: string = cItem): Integer; overload;

    { Stores and also the attached object informations of a string list.
      Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringObjectList(const Path: string; const SL: TStrings; const ItemName: string = cItem);
    { Retrieves a string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
    { Stores a string list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringList(const Path: string; const SL: TStrings; const ItemName: string = cItem);
    {$IFDEF COMPILER10_UP}
    { Retrieves a wide string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadWideStringList(const Path: string; const SL: TWideStrings;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
    { Stores a WideString list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteWideStringList(const Path: string; const SL: TWideStrings; const ItemName: string = cItem);
    {$ENDIF}
    { Retrieves an enumeration. If the value is not found, the Default will be returned. }
    procedure ReadEnumeration(const Path: string; TypeInfo: PTypeInfo;
      const Default; out Value);
    { Stores an enumeration }
    procedure WriteEnumeration(const Path: string; TypeInfo: PTypeInfo;
      const Value);
    procedure ReadSet(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value);
    { Stores a set. }
    procedure WriteSet(const Path: string; ATypeInfo: PTypeInfo; const Value);
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not an Boolean (or can't be converted to a Boolean an EConvertError
      exception will be raised. }
    function ReadBoolean(const Path: string; Default: Boolean = True): Boolean;
    { Stores an Boolean value
      The value is stored as string TRUE/FALSE. }
    procedure WriteBoolean(const Path: string; Value: Boolean);
    { Retrieves an Property. If the value is not found, the Property is not changed. }
    procedure ReadProperty(const Path: string; const PersObj: TPersistent;
      const PropName: string; const Recursive, ClearFirst: Boolean);
    { Stores an Property }
    procedure WriteProperty(const Path: string; const PersObj: TPersistent;
      const PropName: string; const Recursive: Boolean);
    { Retrieves a set. If the value is not found, the Default will be returned. }
    { Retrieves a TPersistent-Object with all of its published properties }
    procedure ReadPersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const ClearFirst: Boolean = True; const IgnoreProperties: TStrings = nil);
    { Stores a TPersistent-Object with all of its published properties}
    procedure WritePersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const IgnoreProperties: TStrings = nil);

    { Translates a Char value to a (valid) key name. Used by the set storage methods. }
    function GetCharName(Ch: Char): string; virtual;
    { Translates an Integer value to a key name. Used by the set storage methods. }
    function GetIntName(Value: Integer): string; virtual;
    { Translates between a property name and it's storage name. If Reading is True, AName is
      interpreted as a storage name to be translated to a real property name. If Reading is False,
      AName is interpreted as a property name to be translated to a storage name. Will invoke the
      OnTranslatePropertyName event if one is assigned, or return AName if no handler is assigned. }
    function TranslatePropertyName(Instance: TPersistent; const AName: string;
      const Reading: Boolean): string;
    { Enumerate a list of stored values and/or folder below the specified path, optionally scanning
      sub folders as well. The associated object is an integer specifying what the string
      represents: 1: Folder; 2: Value; 3: Both }
    procedure GetStoredValues(const Path: string; const Strings: TStrings;
      const Options: TJvAppStorageEnumOptions = [aeoValues, aeoReportListAsValue, aeoRecursive]);
    { Enables the Cryption of Property-Values (Only String-Values) }
    procedure EnablePropertyValueCrypt;
    { Disables the Cryption of Property-Values (Only String-Values) }
    procedure DisablePropertyValueCrypt;
    { Returns the current state if Property-Value Cryption is enabled }
    function IsPropertyValueCryptEnabled: Boolean;
    function ItemNameIndexPath(const ItemName: string; const Index: Integer):
        string; virtual;
    function ReadWideString(const Path: string; const Default: WideString = ''):
        WideString;
    procedure WriteWideString(const Path: string; const Value: WideString);
    { Root of any values to be read/written. This value is combined with the path given in one of
      the Read*/Write* methods to determine the actual key used. It's always relative to the value
      of Root (which is an absolute path) }
    property Path: string read GetPath write SetPath;
    { Defines if the Storage-Component is readonly or not.
      If Readonly is true all Calls to an Write*-Procedure will be ignored.
      The property is calulated by a combination of setting the
      property ReadOnly and Result of the function GetPhysicalReadOnly }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { If True, the destructor will call Flush as its first instruction.
      This property was added following Mantis 3168 and is True by default
      to keep backward compatibility }
    property FlushOnDestroy: Boolean read FFlushOnDestroy write SetFlushOnDestroy default True;
  published
    //1 The current Translateengine which should be used for all operations. It's the internal translateengine, or the assigned property TranslateStringEngine
    property ActiveTranslateStringEngine: TJvTranslateString read
        GetActiveTranslateStringEngine;
    property StorageOptions: TJvCustomAppStorageOptions read FStorageOptions write SetStorageOptions;
    //1 This engine gives you the possibility to translate Strings with %-Replacements
    property TranslateStringEngine: TJvTranslateString read FTranslateStringEngine write SetTranslateStringEngine;
    property OnTranslatePropertyName: TJvAppStoragePropTranslateEvent read FOnTranslatePropertyName
      write FOnTranslatePropertyName;
    property OnEncryptPropertyValue: TJvAppStorageCryptEvent read FOnEncryptPropertyValue
      write FOnEncryptPropertyValue;
    property OnDecryptPropertyValue: TJvAppStorageCryptEvent read FOnDecryptPropertyValue
      write FOnDecryptPropertyValue;

    // called when an error occured in one of the methods.
    property OnError: TJvAppStorageErrorEvent read FOnError write FOnError;
  end;

  { Generic store that can only be used to combine various other storages (only storages in the
    SubStorages collection are usable; any references to paths not specified in this collection
    will raise an exception). Can be used for example to provide access to the entire registry
    hive from a single app store component by adding a number of TJvAppRegistryStorage storages,
    each referencing a specific root key and link them to a suitable root key path:

    RootPath              Store
    ========              =====
    HKCR                  asRegStoreHKCR
    HKEY_CLASSES_ROOT     asRegStoreHKCR
    HKCU                  asRegStoreHKCU
    HKEY_CURRENT_USER     asRegStoreHKCU
    HKLM                  asRegStoreHKLM
    HKEY_LOCAL_MACHINE    asRegStoreHKLM

    In the above scheme, both 'HKCU\<path>' as well as 'HKEY_CURRENT_USER'<path>' will link to
    asRegStoreHKCU, ie. HKCU and HKEY_CURRENT_USER are aliases of each other. }
  TJvAppStorage = class(TJvCustomAppStorage)
  protected
    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; override;
    function PathExistsInt(const Path: string): Boolean; override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;
    function ReadIntegerInt(const Path: string; Default: Integer = 0): Integer; override;
    procedure WriteIntegerInt(const Path: string; Value: Integer); override;
    function ReadFloatInt(const Path: string; Default: Extended = 0): Extended; override;
    procedure WriteFloatInt(const Path: string; Value: Extended); override;
    function ReadStringInt(const Path: string; const Default: string = ''): string; override;
    procedure WriteStringInt(const Path: string; const Value: string); override;
    function ReadBinaryInt(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer; override;
    procedure WriteBinaryInt(const Path: string; const Buf: TJvBytes; BufSize: Integer); override;
    function ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime; override;
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); override;
    function ReadBooleanInt(const Path: string; Default: Boolean): Boolean; override;
    procedure WriteBooleanInt(const Path: string; Value: Boolean); override;
    procedure ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default; out Value); override;
    procedure WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Value); override;
    procedure ReadSetInt(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value); override;
    procedure WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value); override;
    property ReadOnly;
  published
    property FlushOnDestroy;
    property SubStorages;
  end;

  TJvCustomAppStorageOptions = class(TPersistent)
  private
    FBooleanAsString: Boolean;
    FBooleanStringTrueValues: string;
    FBooleanStringFalseValues: string;
    FEnumAsStr: Boolean;
    FIntAsStr: Boolean;
    FSetAsStr: Boolean;
    FDateTimeAsString: Boolean;
    FFloatAsString: Boolean;
    FDefaultIfReadConvertError: Boolean;
    FDefaultIfValueNotExists: Boolean;
    FStoreDefaultValues: Boolean;
    FStoreStringListAsSingleString: Boolean;
    FUseOldItemNameFormat: Boolean;
    FUseTranslateStringEngineDateTimeFormats: Boolean;
  protected
    procedure SetBooleanAsString(Value: Boolean); virtual;
    procedure SetBooleanStringTrueValues(Value: string); virtual;
    procedure SetBooleanStringFalseValues(Value: string); virtual;
    procedure SetEnumAsStr(Value: Boolean); virtual;
    procedure SetIntAsStr(Value: Boolean); virtual;
    procedure SetSetAsStr(Value: Boolean); virtual;
    procedure SetDateTimeAsStr(Value: Boolean); virtual;
    procedure SetFloatAsStr(Value: Boolean); virtual;
    procedure SetDefaultIfReadConvertError(Value: Boolean); virtual;
    procedure SetDefaultIfValueNotExists(Value: Boolean); virtual;
    function IsValueListString(const AValue, AList: string): Boolean; virtual;
    procedure SetStoreStringListAsSingleString(const Value: Boolean); virtual;
    procedure SetUseOldItemNameFormat(const Value: Boolean); virtual;
    procedure SetStoreDefaultValues(const Value: Boolean); virtual;
    //Flag to determine if a stringlist should be stored as single string and not as list of string items
    property StoreStringListAsSingleString: Boolean read
        FStoreStringListAsSingleString write SetStoreStringListAsSingleString
        default False;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    function DefaultTrueString: string;
    function DefaultFalseString: string;
    function IsValueTrueString(Value: string): Boolean;
    function IsValueFalseString(Value: string): Boolean;

    property BooleanStringTrueValues: string read FBooleanStringTrueValues
      write SetBooleanStringTrueValues;
    property BooleanStringFalseValues: string read FBooleanStringFalseValues
      write SetBooleanStringFalseValues;
    property BooleanAsString: Boolean read FBooleanAsString write SetBooleanAsString default True;
    property EnumerationAsString: Boolean read FEnumAsStr write SetEnumAsStr default True;
    property TypedIntegerAsString: Boolean read FIntAsStr write SetIntAsStr default True;
    property SetAsString: Boolean read FSetAsStr write SetSetAsStr default False;
    property DateTimeAsString: Boolean read FDateTimeAsString write SetDateTimeAsStr default True;
    property FloatAsString: Boolean read FFloatAsString write SetFloatAsStr default False;
    property DefaultIfReadConvertError: Boolean read FDefaultIfReadConvertError
      write SetDefaultIfReadConvertError default False;
    property DefaultIfValueNotExists: Boolean read FDefaultIfValueNotExists
      write SetDefaultIfValueNotExists default True;
    property StoreDefaultValues: Boolean read FStoreDefaultValues
      write SetStoreDefaultValues default True;
    //1 Property to define the format of list entries, the new format is <item>[<nr>], the old format is <item><nr>.
    /// Property to define the format of list entries, the new format is <item>[<nr>],
    /// the old format is <item><nr>.
    /// The advantage of the new format for xml-appstorage is that the brackets will be
    /// removed.
    property UseOldItemNameFormat: Boolean read FUseOldItemNameFormat write
        SetUseOldItemNameFormat default True;
    //1 Property to define that the TranslateEngine DateFormat and TimeFormat Property Values will be used to read/write DateTime values
    property UseTranslateStringEngineDateTimeFormats: Boolean read
        FUseTranslateStringEngineDateTimeFormats write
        FUseTranslateStringEngineDateTimeFormats default False;
  end;

  TJvAppStorageOptions = class(TJvCustomAppStorageOptions)
  published
    property BooleanStringTrueValues;
    property BooleanStringFalseValues;
    property BooleanAsString;
    property EnumerationAsString;
    property TypedIntegerAsString;
    property SetAsString;
    property DateTimeAsString;
    property FloatAsString;
    property DefaultIfReadConvertError;
    property DefaultIfValueNotExists;
    property StoreDefaultValues;
    property UseOldItemNameFormat;
    property UseTranslateStringEngineDateTimeFormats;
  end;

  TJvAppSubStorages = class(TOwnedCollection)
  private
    function GetRootStorage: TJvCustomAppStorage;
    function GetItem(I: Integer): TJvAppSubStorage;
    procedure SetItem(I: Integer; Value: TJvAppSubStorage);
  protected
    { Notify sub storages of a change in the options of the root storage. This allows sub storage
      to be kept in sync with the root storage. }
    procedure RootOptionsChanged;
    { Check if the given root path is unique, optionally ignoring a specific sub storage (eg. when
      modifying the root path of a storage, that storage's RootPath is irrelavant in determining
      if the new name will be unique). }
    function CheckUniqueBase(const APath: string; IgnoreIndex: Integer): Boolean;
    { Retrieves the sub storage for the given root path, optionally ignoring a specific sub storage.
      The specified path is assumed to be at root level (regardless whether the paths starts with
      a backslash (\) or not) and leading and trailing backslashes are removed automatically.
      The last element in the path string is ignored to avoid returning a sub storage for the root
      path itself. To search for a sub store for a root path, simply add '\*' at the end of the
      path. }
    function MatchFor(APath: string; IgnoreIndex: Integer = -1): TJvAppSubStorage;

    property RootStorage: TJvCustomAppStorage read GetRootStorage;
  public
    constructor Create(AOwner: TJvCustomAppStorage);
    procedure Add(RootPath: string; AppStorage: TJvCustomAppStorage);
    procedure Delete(Index: Integer); overload;
    procedure Delete(RootPath: string; const IncludeSubPaths: Boolean = False); overload;
    procedure Delete(AppStorage: TJvCustomAppStorage); overload;

    property Items[I: Integer]: TJvAppSubStorage read GetItem write SetItem; default;
  end;

  TJvAppSubStorage = class(TCollectionItem)
  private
    FRootPath: string;
    FAppStorage: TJvCustomAppStorage;
  protected
    function GetOwnerStore: TJvCustomAppStorage;
    function GetDisplayName: string; override;
    procedure SetRootPath(Value: string);
    procedure SetAppStorage(Value: TJvCustomAppStorage);

    property OwnerStore: TJvCustomAppStorage read GetOwnerStore;
  published
    property RootPath: string read FRootPath write SetRootPath;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
  end;

  // Base class for all in memory file storage classes.
  // All descendents implement a file storage, but all changes
  // are left in memory until the Flush method is called.
  // Flush is automatically called by the destructor, but
  // you can override Flush to write the file on a support
  // different from a disk, such as database record.
  // Please note that in the derived class, if you use an object
  // to represent the file in memory, this object MUST be freed
  // AFTER the call to inherited in the destructor of your
  // derived class or Flush would access a deleted object
  TJvCustomAppMemoryFileStorage = class(TJvCustomAppStorage)
  private
    FFullFileName: TFileName;
  protected
    FFileName: TFileName;
    FLocation: TFileLocation;
    FOnGetFileName: TJvAppStorageGetFileNameEvent;
    FPhysicalReadOnly: Boolean;
    FFileLoaded: Boolean;

    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;

    procedure SetFileName(const Value: TFileName);
    procedure SetOnGetFileName(Value: TJvAppStorageGetFileNameEvent);
    procedure SetLocation(const Value: TFileLocation);
    function DefaultExtension: string; virtual;

    function DoGetFileName: TFileName; virtual;
    property AsString: string read GetAsString write SetAsString;

    // OnGetFileName triggered on Location = flCustom
    property OnGetFileName: TJvAppStorageGetFileNameEvent read FOnGetFileName write SetOnGetFileName;

    function GetPhysicalReadOnly: Boolean; override;
    procedure RecalculateFullFileName;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Reload; override;
    function ReloadNeeded: Boolean; override;

    property FileName: TFileName read FFileName write SetFileName;
    property FullFileName: TFileName read FFullFileName;
    property Location: TFileLocation read FLocation write SetLocation default
      flExeFile;
  published
    property ReadOnly;
  end;

  { This Engine implements the possibility to implement special property handlers
    for TObject-based properties for storing/restoring them with the
    functions read/writeproperty.
    New engines could be registered using the method RegisterAppStoragePropertyEngine
  }
  TJvAppStoragePropertyBaseEngine = class(TObject)
  public
    constructor Create; virtual;
    function Supports(AObject: TObject; AProperty: TObject): Boolean; virtual;
    procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean); virtual;
    procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive: Boolean); virtual;
  end;

  TJvAppStoragePropertyBaseEngineClass = class of TJvAppStoragePropertyBaseEngine;

procedure RegisterAppStoragePropertyEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
procedure UnregisterAppStoragePropertyEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);

// (marcelb) moved back; the constants are useful to the outside world after a call to GetStoredValues
// (rom) give it better names and delete these comments :-)
const
  aptFolder = 1;
  aptValue = 2;

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
  StrUtils,
  JclFileUtils, JclSysInfo, JclRTTI, JclMime,
  JvPropertyStore, JvConsts, JvResources, JvStrings, JclSynch, JvJVCLUtils;

type
  TJvAppStoragePropertyEngineList = class(TList)
  public
    destructor Destroy; override;
    procedure RegisterEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
    procedure UnregisterEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
    function GetEngine(AObject: TObject; AProperty: TObject): TJvAppStoragePropertyBaseEngine;
    function ReadProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean): Boolean;
    function WriteProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive: Boolean): Boolean;
  end;

var
  GlobalRegisteredAppStoragePropertyEngineList: TJvAppStoragePropertyEngineList;
  GlobalRegisteredAppStoragePropertyEngineListDestroyed: Boolean;

function RegisteredAppStoragePropertyEngineList: TJvAppStoragePropertyEngineList;
begin
  if (GlobalRegisteredAppStoragePropertyEngineList = nil) and
     not GlobalRegisteredAppStoragePropertyEngineListDestroyed then
    GlobalRegisteredAppStoragePropertyEngineList := TJvAppStoragePropertyEngineList.Create;
  Result := GlobalRegisteredAppStoragePropertyEngineList;
end;

//=== Global Engine Handling =================================================

procedure RegisterAppStoragePropertyEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
begin
  if RegisteredAppStoragePropertyEngineList <> nil then
    RegisteredAppStoragePropertyEngineList.RegisterEngine(AEngineClass);
end;

procedure UnregisterAppStoragePropertyEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
begin
  if RegisteredAppStoragePropertyEngineList <> nil then
    RegisteredAppStoragePropertyEngineList.UnregisterEngine(AEngineClass);
end;

procedure DestroyAppStoragePropertyEngineList;
begin
  GlobalRegisteredAppStoragePropertyEngineListDestroyed := True;
  GlobalRegisteredAppStoragePropertyEngineList.Free;
  GlobalRegisteredAppStoragePropertyEngineList := nil;
end;

const
  // (rom) this name is shared in several units and should be made global
  cCount = 'Count';
  cObject = 'Object';
  cItemName = 'Itemname';
  cClassName = 'Classname';
  cInvalidIdentifier = ' #!@not known@!# ';
  // (rom) should this be PathDelim + '*' as implemented before i changed it
  // (rom) or \* as comments say?
  cSubStorePath = PathDelim + '*';

function OptimizePaths(const Paths: array of string): string;
var
  PathIndex: Integer;
  Head, Tail, ResultIndex: Integer;
  AllDots: Boolean;
  MaxLength: Integer;
  I: Integer;
  DotCount: Integer;
  L: Integer;
begin
  PathIndex := High(Paths);

  if PathIndex < 0 then
  begin
    Result := '';
    Exit;
  end;

  while (PathIndex > 0) and (StrLeft(Paths[PathIndex], 1) <> PathDelim) do
    Dec(PathIndex);

  MaxLength := 0;
  for I := PathIndex to High(Paths) do
    Inc(MaxLength, Length(Paths[I]) + 1);

  SetLength(Result, MaxLength);

  ResultIndex := 1;

  repeat
    Head := 1;
    // L is only used for optimalization
    L := Length(Paths[PathIndex]);
    repeat
      // skip first path delimiters
      while (Head <= L) and (Paths[PathIndex][Head] = PathDelim) do
        Inc(Head);
      Tail := Head;
      // search for a path delimiter
      AllDots := True;
      while (Head <= L) and (Paths[PathIndex][Head] <> PathDelim) do
      begin
        AllDots := AllDots and (Paths[PathIndex][Head] = '.');
        Inc(Head);
      end;
      // Chunk [Tail..Head) is without a path delimiter, it can be either empty (Head=Tail)
      // be full with dots or be a regular path.
      if Head <> Tail then
      begin
        if AllDots then
        begin
          // [Tail..Head) are all dots
          DotCount := Head - Tail;
          if (DotCount > 1) and (ResultIndex > 1) then
          begin
            // Go back to the previous path delimiter; Current path delimiter is
            // at Result[ResultIndex - 1]
            Dec(ResultIndex, 2);
            while DotCount > 1 do
            begin
              while (ResultIndex > 1) and (Result[ResultIndex] <> PathDelim) do
                Dec(ResultIndex);
              if ResultIndex = 1 then
                Break;
              // Result[ResultIndex] = PathDelim
              Dec(ResultIndex);
              Dec(DotCount);
            end;
            if ResultIndex > 1 then
              Inc(ResultIndex, 2);
          end;
        end
        else
        begin
          // copy [Tail..Head) to Result..
          MoveChar(Paths[PathIndex], Tail - 1, Result, ResultIndex - 1, Head - Tail); // from JclBase.pas
          Inc(ResultIndex, Head - Tail);
          // ..and add a path delimiter to Result
          Result[ResultIndex] := PathDelim;
          Inc(ResultIndex);
        end;
      end;
    until Head > L;
    Inc(PathIndex);
  until PathIndex > High(Paths);

  // skip the last added delimiter (if it exists)
  if ResultIndex > 1 then
    Dec(ResultIndex);

  SetLength(Result, ResultIndex - 1);
end;

procedure CopyEnumValue(const Source; var Target; const Kind: TOrdType);
begin
  case Kind of
    otSByte, otUByte:
      Byte(Target) := Byte(Source);
    otSWord, otUWord:
      Word(Target) := Word(Source);
    otSLong, otULong:
      Longword(Target) := Longword(Source);
  end;
end;

function OrdOfEnum(const Value; OrdType: TOrdType): Integer;
begin
  case OrdType of
    otSByte:
      Result := Shortint(Value);
    otUByte:
      Result := Byte(Value);
    otSWord:
      Result := Smallint(Value);
    otUWord:
      Result := Word(Value);
    otSLong, otULong:
      Result := Longint(Value);
  else
    Result := -1;
  end;
end;

//=== { TJvCustomAppStorageOptions } =========================================

constructor TJvCustomAppStorageOptions.Create;
begin
  inherited Create;
  BooleanStringTrueValues := 'TRUE, YES, Y';
  BooleanStringFalseValues := 'FALSE, NO, N';
  BooleanAsString := True;
  EnumerationAsString := True;
  TypedIntegerAsString := True;
  SetAsString := False;
  DateTimeAsString := True;
  DefaultIfReadConvertError := False;
  DefaultIfValueNotExists := True;
  StoreDefaultValues := True;
  StoreStringListAsSingleString := False;
  UseOldItemNameFormat := True;
  FUseTranslateStringEngineDateTimeFormats := False;
end;

procedure TJvCustomAppStorageOptions.Assign(Source: TPersistent);
begin
  if (Source = Self) then
    Exit;
  if Source is TJvCustomAppStorageOptions then
  begin
    BooleanStringTrueValues := TJvCustomAppStorageOptions(Source).BooleanStringTrueValues;
    BooleanStringFalseValues := TJvCustomAppStorageOptions(Source).BooleanStringFalseValues;
    BooleanAsString := TJvCustomAppStorageOptions(Source).BooleanAsString;
    EnumerationAsString := TJvCustomAppStorageOptions(Source).EnumerationAsString;
    TypedIntegerAsString := TJvCustomAppStorageOptions(Source).TypedIntegerAsString;
    SetAsString := TJvCustomAppStorageOptions(Source).SetAsString;
    DateTimeAsString := TJvCustomAppStorageOptions(Source).DateTimeAsString;
    DefaultIfReadConvertError := TJvCustomAppStorageOptions(Source).DefaultIfReadConvertError;
    DefaultIfValueNotExists := TJvCustomAppStorageOptions(Source).DefaultIfValueNotExists;
    StoreDefaultValues := TJvCustomAppStorageOptions(Source).StoreDefaultValues;
    StoreStringListAsSingleString := TJvCustomAppStorageOptions(Source).StoreStringListAsSingleString;
    UseOldItemNameFormat := TJvCustomAppStorageOptions(Source).UseOldItemNameFormat;
    UseTranslateStringEngineDateTimeFormats := TJvCustomAppStorageOptions(Source).UseTranslateStringEngineDateTimeFormats;
  end
  else
    inherited assign(Source);
end;

function TJvCustomAppStorageOptions.IsValueListString(const AValue, AList: string): Boolean;
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    st.CommaText := UpperCase(AList);
    Result := st.IndexOf(UpperCase(AValue)) >= 0;
  finally
    st.Free;
  end;
end;

function TJvCustomAppStorageOptions.DefaultTrueString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringTrueValues);
  if I = 0 then
    I := Length(FBooleanStringTrueValues) + 1;
  Result := Trim(Copy(FBooleanStringTrueValues, 1, I - 1));
end;

function TJvCustomAppStorageOptions.DefaultFalseString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringFalseValues);
  if I = 0 then
    I := Length(FBooleanStringFalseValues) + 1;
  Result := Trim(Copy(FBooleanStringFalseValues, 1, I - 1));
end;

function TJvCustomAppStorageOptions.IsValueTrueString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringTrueValues);
end;

function TJvCustomAppStorageOptions.IsValueFalseString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringFalseValues);
end;

procedure TJvCustomAppStorageOptions.SetBooleanAsString(Value: Boolean);
begin
  FBooleanAsString := Value and (DefaultTrueString <> '') and (DefaultFalseString <> '');
end;

procedure TJvCustomAppStorageOptions.SetBooleanStringTrueValues(Value: string);
begin
  FBooleanStringTrueValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultTrueString <> '')
end;

procedure TJvCustomAppStorageOptions.SetBooleanStringFalseValues(Value: string);
begin
  FBooleanStringFalseValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultFalseString <> '')
end;

procedure TJvCustomAppStorageOptions.SetEnumAsStr(Value: Boolean);
begin
  FEnumAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetIntAsStr(Value: Boolean);
begin
  FIntAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetSetAsStr(Value: Boolean);
begin
  FSetAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetStoreDefaultValues(
  const Value: Boolean);
begin
  FStoreDefaultValues := Value;
end;

procedure TJvCustomAppStorageOptions.SetDateTimeAsStr(Value: Boolean);
begin
  FDateTimeAsString := Value;
end;

procedure TJvCustomAppStorageOptions.SetFloatAsStr(Value: Boolean);
begin
  FFloatAsString := Value;
end;

procedure TJvCustomAppStorageOptions.SetDefaultIfReadConvertError(Value: Boolean);
begin
  FDefaultIfReadConvertError := Value;
end;

procedure TJvCustomAppStorageOptions.SetDefaultIfValueNotExists(Value: Boolean);
begin
  FDefaultIfValueNotExists := Value;
end;

procedure TJvCustomAppStorageOptions.SetStoreStringListAsSingleString(const
    Value: Boolean);
begin
  FStoreStringListAsSingleString := Value;
end;

procedure TJvCustomAppStorageOptions.SetUseOldItemNameFormat(const Value:
    Boolean);
begin
  FUseOldItemNameFormat := Value;
end;

//=== { TJvCustomAppStorage } ================================================

{$IFDEF SUPPORTS_CLASS_CTORDTORS}
class destructor TJvCustomAppStorage.Destroy;
begin
  DestroyAppStoragePropertyEngineList;
end;
{$ENDIF SUPPORTS_CLASS_CTORDTORS}

constructor TJvCustomAppStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlushOnDestroy := True;
  FAutoFlush := False;
  FAutoReload := False;
  FStorageOptions := GetStorageOptionsClass.Create;
  FSubStorages := TJvAppSubStorages.Create(Self);
  FCryptEnabledStatus := 0;
  FReadOnly := False;
  FInternalTranslateStringEngine := TJvTranslateString.Create(Self);
  FSynchronizeFlushReload := False;
end;

destructor TJvCustomAppStorage.Destroy;
begin
  if FlushOnDestroy then
    Flush;
  FreeAndNil(FInternalTranslateStringEngine);
  FreeAndNil(FSubStorages);
  FreeAndNil(FStorageOptions);
  inherited Destroy;
end;

procedure TJvCustomAppStorage.Flush;
begin
  // do nothing
end;

procedure TJvCustomAppStorage.Reload;
begin
  // do nothing
end;

procedure TJvCustomAppStorage.ReloadIfNeeded;
begin
  if ReloadNeeded then
    Reload;
end;

function TJvCustomAppStorage.ReloadNeeded: Boolean;
begin
  Result := AutoReload and not IsUpdating;
end;

procedure TJvCustomAppStorage.FlushIfNeeded;
begin
  if FlushNeeded then
    Flush;
end;

function TJvCustomAppStorage.FlushNeeded: Boolean;
begin
  Result := AutoFlush and not IsUpdating;
end;

procedure TJvCustomAppStorage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (AComponent is TJvCustomAppStorage) and Assigned(SubStorages) then
      SubStorages.Delete(AComponent as TJvCustomAppStorage)
    else if (AComponent = FTranslateStringEngine) then
      FTranslateStringEngine := nil;
end;

procedure TJvCustomAppStorage.SetFlushOnDestroy(Value: Boolean);
begin
  FFlushOnDestroy := Value;
end;

function TJvCustomAppStorage.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data.PropCount;
end;

function TJvCustomAppStorage.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name);
  finally
    FreeMem(PropList);
  end;
end;

class function TJvCustomAppStorage.GetStorageOptionsClass: TJvAppStorageOptionsClass;
begin
  Result := TJvAppStorageOptions;
end;

procedure TJvCustomAppStorage.SplitKeyPath(const Path: string; out Key, ValueName: string);
var
  AbsPath: string;
  ValueNamePos: Integer;
begin
  AbsPath := GetAbsPath(Path);
  ValueNamePos := LastDelimiter(PathDelim, AbsPath);
  Key := StrLeft(AbsPath, ValueNamePos - 1);
  ValueName := StrRestOf(AbsPath, ValueNamePos + 1);
end;

procedure TJvCustomAppStorage.SetSubStorages(Value: TJvAppSubStorages);
begin
end;

function TJvCustomAppStorage.GetRoot: string;
begin
  Result := FRoot;
end;

procedure TJvCustomAppStorage.SetRoot(const Value: string);
begin
  FRoot := OptimizePaths([Value]);
end;

function TJvCustomAppStorage.GetCurrentPath: string;
begin
  Result := GetAbsPath('');
end;

function TJvCustomAppStorage.GetAbsPath(const Path: string): string;
begin
  Result := GetRoot + PathDelim + OptimizePaths([GetPath, Path]);
  while (Result <> '') and (Result[1] = PathDelim) do
    Delete(Result, 1, 1);
end;

procedure TJvCustomAppStorage.ReadStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TStrings then
    TStrings(List).Add(Sender.ReadString(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)])));
end;

procedure TJvCustomAppStorage.WriteStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TStrings then
    Sender.WriteString(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]), TStrings(List)[Index]);
end;

procedure TJvCustomAppStorage.DeleteStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TStrings then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

{$IFDEF COMPILER10_UP}
procedure TJvCustomAppStorage.ReadWideStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is WideStrings.TWideStrings then
    WideStrings.TWideStrings(List).Add(Sender.ReadWideString(ConcatPaths([Path, ItemName + IntToStr(Index)])));
end;

procedure TJvCustomAppStorage.WriteWideStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is WideStrings.TWideStrings then
    Sender.WriteWideString(ConcatPaths([Path, ItemName + IntToStr(Index)]), WideStrings.TWideStrings(List)[Index]);
end;

procedure TJvCustomAppStorage.DeleteWideStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is WideStrings.TWideStrings then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;
{$ENDIF}

function TJvCustomAppStorage.DefaultObjectListItemCreateEvent(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
var
  NewClassName: string;
begin
  NewClassName := Sender.ReadString(ConcatPaths([Path, cClassName]));
  Result := GetClass(NewClassName).Create;
end;

procedure TJvCustomAppStorage.ReadObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  if List is TList then
  begin
    NewPath := ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]);
    ResolvePath(NewPath, TargetStore, TargetPath); // Only needed for assigning the event
    NewItem := TargetStore.CurrentInstanceCreateEvent(Sender, NewPath, Index);
    TList(List).Add(NewItem);
    Sender.ReadPersistent(NewPath, NewItem);
  end;
end;

procedure TJvCustomAppStorage.WriteObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TList then
    if Assigned(TList(List)[Index]) then
      Sender.WritePersistent(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]), TPersistent(TList(List)[Index]));
end;

procedure TJvCustomAppStorage.DeleteObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TList then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

procedure TJvCustomAppStorage.ReadStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
  NewName: string;
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  if List is TStrings then
  begin
    NewPath := ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]);
    ResolvePath(NewPath, TargetStore, TargetPath); // Only needed for assigning the event
    NewItem := TargetStore.CurrentInstanceCreateEvent(Sender, ConcatPaths([NewPath, cObject]), Index);
    NewName := Sender.ReadString(ConcatPaths([NewPath, cItemName]));
    TStrings(List).AddObject(NewName, NewItem);
    if NewItem is TJvCustomPropertyStore then
      Sender.ReadPersistent(ConcatPaths([NewPath, cObject]), NewItem,
        True, True, TJvCustomPropertyStore(NewItem).CombinedIgnoreProperties)
    else
      Sender.ReadPersistent(ConcatPaths([NewPath, cObject]), NewItem);
  end;
end;

procedure TJvCustomAppStorage.WriteStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  Obj: TObject;
begin
  if List is TStrings then
  begin
    Sender.WriteString(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index), cItemName]), TStrings(List)[Index]);
    Obj := TStrings(List).Objects[Index];
    if Assigned(Obj) then
      if (Obj is TJvCustomPropertyStore) then
        if not TJvCustomPropertyStore(Obj).ReadOnly then
          Sender.WritePersistent(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index), cObject]), TPersistent(Obj),
            True, TJvCustomPropertyStore(Obj).CombinedIgnoreProperties)
        else
      else
        Sender.WritePersistent(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index), cObject]), TPersistent(Obj));
  end;
end;

procedure TJvCustomAppStorage.DeleteStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TStrings then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

procedure TJvCustomAppStorage.ReadCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
begin
  if List is TCollection then
  begin
    NewPath := ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]);
    NewItem := TCollection(List).Add;
    if NewItem is TJvCustomPropertyStore then
      Sender.ReadPersistent(NewPath, NewItem, True, True,
        TJvCustomPropertyStore(NewItem).CombinedIgnoreProperties)
    else
      Sender.ReadPersistent(NewPath, NewItem);
  end;
end;

procedure TJvCustomAppStorage.WriteCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  Item: TObject;
begin
  if List is TCollection then
  begin
    Item := TCollection(List).Items[Index];
    if Assigned(Item) then
      if Item is TJvCustomPropertyStore then
        if not TJvCustomPropertyStore(Item).ReadOnly then
          Sender.WritePersistent(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]), TPersistent(Item),
            True, TJvCustomPropertyStore(Item).CombinedIgnoreProperties)
        else
      else
        Sender.WritePersistent(ConcatPaths([Path, ItemNameIndexPath (ItemName, Index)]), TPersistent(Item));
  end;
end;

procedure TJvCustomAppStorage.DeleteCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TCollection then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

procedure TJvCustomAppStorage.InternalGetStoredValues(const PrefixPath, SearchPath: string;
  const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
var
  TempList: TStrings;
  I: Integer;
  S: string;
  PrevIdx: Integer;
  InsertPath : string;
begin
  if PrefixPath = PathDelim then
    InsertPath := ''
  else
    InsertPath := PrefixPath;
  TempList := TStringList.Create;
  try
    if aeoValues in Options then
    begin
      EnumValues(SearchPath, TempList, aeoReportListAsValue in Options);
      for I := 0 to TempList.Count - 1 do
      begin
        if TempList[I] = '' then
          S := Copy(PrefixPath, 1, Length(PrefixPath) - 1)
        else
          S := InsertPath + TempList[I];
        if S <> '' then
        begin
          PrevIdx := Strings.IndexOf(S);
          if PrevIdx > -1 then
            Strings.Objects[PrevIdx] :=
              TObject(Integer(Strings.Objects[PrevIdx]) or aptValue)
          else
            Strings.AddObject(S, TObject(aptValue));
        end;
      end;
    end;
    if (aeoFolders in Options) or (aeoRecursive in Options) then
    begin
      TempList.Clear;
      EnumFolders(SearchPath, TempList, False);
      for I := 0 to TempList.Count - 1 do
      begin
        if (aeoFolders in Options) and IsFolder(SearchPath + PathDelim +
          TempList[I], aeoReportListAsValue in Options) then
        begin
          PrevIdx := Strings.IndexOf(InsertPath + TempList[I]);
          if PrevIdx > -1 then
            Strings.Objects[PrevIdx] :=
              TObject(Integer(Strings.Objects[PrevIdx]) or aptFolder)
          else
            Strings.AddObject(InsertPath + TempList[I], TObject(aptFolder));
        end;
        if aeoRecursive in Options then
          InternalGetStoredValues(ConcatPaths([PrefixPath, TempList[I]]) + PathDelim,
            ConcatPaths([SearchPath, TempList[I]]),
            Strings, Options);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

function TJvCustomAppStorage.GetPath: string;
begin
  Result := FCurPath;
end;

procedure TJvCustomAppStorage.SetPath(const Path: string);
begin
  FCurPath := OptimizePaths([Path]);
end;

procedure TJvCustomAppStorage.SetStorageOptions(Value: TJvCustomAppStorageOptions);
begin
  if (Value <> nil) and (Value <> FStorageOptions) then
    FStorageOptions.Assign(Value);
end;

procedure TJvCustomAppStorage.DoTranslatePropertyName(Instance: TPersistent; var Name: string;
  const Reading: Boolean);
begin
  if Assigned(FOnTranslatePropertyName) then
    FOnTranslatePropertyName(Self, Instance, Name, Reading);
end;

function TJvCustomAppStorage.HasSubStorage(AStore: TJvCustomAppStorage): Boolean;
var
  I: Integer;
begin
  I := SubStorages.Count - 1;
  Result := False;
  while not Result and (I >= 0) do
  begin
    Result := (SubStorages[I].AppStorage = AStore) or
      ((SubStorages[I].AppStorage <> nil) and SubStorages[I].AppStorage.HasSubStorage(AStore));
    Dec(I);
  end;
end;

function TJvCustomAppStorage.ListStoredInt(const Path: string; const ItemName:
    string = cItem): Boolean;
begin
  Result := ValueStoredInt(StrEnsureSuffix(PathDelim, Path) + cCount);
end;

function TJvCustomAppStorage.DoReadDateTime(const Path: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadFloat(Path, Default);
end;

function TJvCustomAppStorage.DoReadWideString(const Path: string;
  const Default: Widestring): Widestring;
begin
  {$IFDEF COMPILER12_UP}
  Result := UTF8ToWideString(RawByteString(ReadString(Path, string(UTF8Encode(Default)))));
  {$ELSE}
  Result := UTF8Decode(ReadString(Path, UTF8Encode(Default)));
  {$ENDIF COMPILER12_UP}
end;

procedure TJvCustomAppStorage.DoWriteDateTime(const Path: string; Value: TDateTime);
begin
  DoWriteFloat(Path, Value);
end;

procedure TJvCustomAppStorage.DoWriteWideString(const Path: string;
  const Value: Widestring);
begin
  DoWriteString(Path,string(UTF8Encode(Value)));
end;

procedure TJvCustomAppStorage.DoError(const msg: string);
begin
  if Assigned(OnError) then
    OnError(Self, msg);
end;

function TJvCustomAppStorage.DoReadBoolean(const Path: string; Default: Boolean): Boolean;
begin
  Result := DoReadInteger(Path, Ord(Default)) <> Ord(False);
end;

procedure TJvCustomAppStorage.DoWriteBoolean(const Path: string; Value: Boolean);
begin
  DoWriteInteger(Path, Ord(Value));
end;

function TJvCustomAppStorage.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
  try
    Result := DoReadInteger(Path, Default);
  except
    on E: EConvertError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStorage.WriteIntegerInt(const Path: string; Value: Integer);
begin
  DoWriteInteger(Path, Value);
end;

function TJvCustomAppStorage.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
  try
    if StorageOptions.FloatAsString then
    try
      Result := StrToFloat(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(FloatToStr(Default)))));
    except
      on E: EConvertError do
        Result := DoReadFloat(Path, Default);
    end
    else
    try
      Result := DoReadFloat(Path, Default);
    except
      on E: EConvertError do
        Result := StrToFloat(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(FloatToStr(Default)))));
    end
  except
    on E: EConvertError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStorage.WriteFloatInt(const Path: string; Value: Extended);
begin
  if StorageOptions.FloatAsString then
    DoWriteString(Path, EncryptPropertyValue(FloatToStr(Value)))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStorage.ReadStringInt(const Path: string; const Default: string): string;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
  try
    Result := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(Default)));
  except
    on E: EConvertError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStorage.WriteStringInt(const Path: string; const Value: string);
begin
  DoWriteString(Path, EncryptPropertyValue(Value));
end;

function TJvCustomAppStorage.ReadBinaryInt(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
begin
  Result := DoReadBinary(Path, Buf, BufSize);
end;

procedure TJvCustomAppStorage.WriteBinaryInt(const Path: string; const Buf: TJvBytes; BufSize: Integer);
begin
  DoWriteBinary(Path, Buf, BufSize);
end;

function TJvCustomAppStorage.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
  try
    if StorageOptions.DateTimeAsString then
    try
      Result := DecodeStrToDateTime(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(EncodeDateTimeToStr(Default)))));
    except
      on E: EConvertError do
        Result := DoReadDateTime(Path, Default);
    end
    else
    try
      Result := DoReadDateTime(Path, Default);
    except
      on E: EConvertError do
        Result := DecodeStrToDateTime(DecryptPropertyValue(DoReadString(Path,
          EncryptPropertyValue(EncodeDateTimeToStr(Default)))));
    end
  except
    on E: EConvertError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStorage.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  if StorageOptions.DateTimeAsString then
    DoWriteString(Path, EncryptPropertyValue(EncodeDateTimeToStr(Value)))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStorage.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
var
  Value: string;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
  try
    if StorageOptions.BooleanAsString then
    try
      if Default then
        Value := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(StorageOptions.DefaultTrueString)))
      else
        Value := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(StorageOptions.DefaultFalseString)));
      if StorageOptions.IsValueTrueString(Value) then
        Result := True
      else
        if StorageOptions.IsValueFalseString(Value) then
          Result := False
        else
          Result := DoReadBoolean(Path, Default);
    except
      on E: EConvertError do
        Result := DoReadBoolean(Path, Default);
    end
    else
      Result := DoReadBoolean(Path, Default);
  except
    on E: EConvertError do
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStorage.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  if StorageOptions.BooleanAsString then
    if Value then
      DoWriteString(Path, EncryptPropertyValue(StorageOptions.DefaultTrueString))
    else
      DoWriteString(Path, EncryptPropertyValue(StorageOptions.DefaultFalseString))
  else
    DoWriteBoolean(Path, Value);
end;

class function TJvCustomAppStorage.NameIsListItem(const Name: string): Boolean;
var
  NameStart: PChar;
begin
  NameStart := AnsiStrRScan(PChar(Name), PathDelim);
  if NameStart = nil then
    NameStart := PChar(Name);
  Result := (AnsiStrLIComp(NameStart, cItem, 4) = 0) and CharInSet(NameStart[4], DigitSymbols);
end;

class function TJvCustomAppStorage.ConcatPaths(const Paths: array of string): string;
begin
  Result := OptimizePaths(Paths);
end;

procedure TJvCustomAppStorage.ResolvePath(const InPath: string; out TargetStore: TJvCustomAppStorage;
  out TargetPath: string);
var
  SubStorageItem: TJvAppSubStorage;
begin
  TargetPath := PathDelim + ConcatPaths([Path, InPath]);
  TargetStore := Self;
  SubStorageItem := SubStorages.MatchFor(TargetPath);
  if (SubStorageItem <> nil) and (SubStorageItem.AppStorage <> nil) then
  begin
    TargetStore := SubStorageItem.AppStorage;
    Delete(TargetPath, 1, Length(SubStorageItem.RootPath) + 1);
    TargetPath := PathDelim + OptimizePaths([TargetPath]);
    if TargetPath = PathDelim then
      raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
  end;
end;

function TJvCustomAppStorage.IsFolder(const Path: string; ListIsValue: Boolean): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.IsFolderInt(TargetPath, ListIsValue);
end;

function TJvCustomAppStorage.PathExists(const Path: string): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.PathExistsInt(TargetPath);
end;

function TJvCustomAppStorage.ValueStored(const Path: string): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ValueStoredInt(TargetPath);
end;

function TJvCustomAppStorage.ListStored(const Path: string; const ItemName:
    string = cItem): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ListStoredInt(TargetPath, ItemName);
end;

procedure TJvCustomAppStorage.DeleteValue(const Path: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.DeleteValueInt(TargetPath);
end;

procedure TJvCustomAppStorage.DeleteSubTree(const Path: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.DeleteSubTreeInt(Path);
end;

function TJvCustomAppStorage.ReadInteger(const Path: string; Default: Integer): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadIntegerInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteInteger(const Path: string; Value: Integer);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteIntegerInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadFloat(const Path: string; Default: Extended): Extended;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadFloatInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteFloat(const Path: string; Value: Extended);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteFloatInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadString(const Path: string; const Default: string): string;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadStringInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteString(const Path: string; const Value: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteStringInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadBinaryInt(TargetPath, Buf, BufSize);
end;

procedure TJvCustomAppStorage.WriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteBinaryInt(TargetPath, Buf, BufSize);
end;

function TJvCustomAppStorage.ReadDateTime(const Path: string; Default: TDateTime): TDateTime;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadDateTimeInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteDateTime(const Path: string; Value: TDateTime);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteDateTimeInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadBoolean(const Path: string; Default: Boolean): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadBooleanInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteBoolean(const Path: string; Value: Boolean);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteBooleanInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadList(const Path: string; const List: TObject;
  const OnReadItem: TJvAppStorageListItemEvent;
  const ItemName: string = cItem): Integer;
var
  I: Integer;
  ItemCount: Integer;
begin
  ItemCount := ReadListItemCount (Path, ItemName);
  for I := 0 to ItemCount - 1 do
    OnReadItem(Self, Path, List, I, ItemName);
  Result := ItemCount;
end;

procedure TJvCustomAppStorage.WriteList(const Path: string; const List: TObject;
  const ItemCount: Integer; const OnWriteItem: TJvAppStorageListItemEvent;
  const OnDeleteItems: TJvAppStorageListDeleteEvent = nil; const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  PrevListCount: Integer;
  I: Integer;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath); // Only Needed for ReadOnly
  if not TargetStore.ReadOnly then
  begin
    PrevListCount := ReadListItemCount (Path, ItemName);
    for I := 0 to ItemCount - 1 do
      OnWriteItem(Self, Path, List, I, ItemName);
    if (PrevListCount > ItemCount) and Assigned(OnDeleteItems) then
      OnDeleteItems(Self, Path, List, ItemCount, PrevListCount - 1, ItemName);
    WriteListItemCount (Path, ItemCount, ItemName);
  end;
end;

function TJvCustomAppStorage.ReadObjectList(const Path: string; List: TList;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  Result := ReadObjectList(Path, List, DefaultObjectListItemCreateEvent, ClearFirst, ItemName);
end;

function TJvCustomAppStorage.ReadObjectList(const Path: string; List: TList;
  ItemCreator: TJvAppStorageObjectListItemCreateEvent;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  FOldInstanceCreateEvent: TJvAppStorageObjectListItemCreateEvent;
begin
  if not ListStored(Path, ItemName) and StorageOptions.DefaultIfValueNotExists then
    Result := List.Count
  else
  begin
    if ClearFirst then
      List.Clear;
    ResolvePath(Path + cSubStorePath, TargetStore, TargetPath); // Only needed for assigning the event
    FOldInstanceCreateEvent := TargetStore.CurrentInstanceCreateEvent;
    try
      TargetStore.SetCurrentInstanceCreateEvent(ItemCreator);
      Result := ReadList(Path, List, ReadObjectListItem, ItemName);
    finally
      TargetStore.SetCurrentInstanceCreateEvent(FOldInstanceCreateEvent);
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteObjectList(const Path: string; List: TList;
  const ItemName: string = cItem);
begin
  WriteList(Path, List, List.Count, WriteObjectListItem, DeleteObjectListItem, ItemName);
end;

function TJvCustomAppStorage.ReadCollection(const Path: string; List: TCollection;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  if not ListStored(Path, ItemName) and StorageOptions.DefaultIfValueNotExists then
    Result := List.Count
  else
  try
    List.BeginUpdate;
    if ClearFirst then
      List.Clear;
    ReadPersistent(Path,List,True,False);
    Result := ReadList(Path, List, ReadCollectionItem, ItemName);
  finally
    List.EndUpdate;
  end;
end;

procedure TJvCustomAppStorage.WriteCollection(const Path: string;
  List: TCollection; const ItemName: string = cItem);
begin
  WriteList(Path, List, List.Count, WriteCollectionItem, DeleteCollectionItem, ItemName);
  WritePersistent(Path,List);
end;

function TJvCustomAppStorage.ReadStringList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  if ClearFirst then
    SL.Clear;
  if not ListStored(Path, ItemName) then
  begin
    if ValueStored(Path) then
      Sl.Text := ReadString(Path);
    Result := SL.Count
  end
  else
  begin
    SL.BeginUpdate;
    try
      ReadPersistent(Path,SL,True,False);
      Result := ReadList(Path, SL, ReadStringListItem, ItemName);
    finally
      SL.EndUpdate;
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteStringList(const Path: string;
  const SL: TStrings; const ItemName: string = cItem);
begin
  if StorageOptions.StoreStringListAsSingleString then
  begin
    if ListStored(Path, ItemName) then
      DeleteSubTree(Path);
    WriteString(Path, SL.Text);
  end
  else
  begin
    WriteList(Path, SL, SL.Count, WriteStringListItem, DeleteStringListItem, ItemName);
    WritePersistent(Path,SL);
  end;
end;

{$IFDEF COMPILER10_UP}
function TJvCustomAppStorage.ReadWideStringList(const Path: string; const SL: WideStrings.TWideStrings;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  if not ListStored(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := SL.Count
  else
  begin
    SL.BeginUpdate;
    try
      if ClearFirst then
        SL.Clear;
      ReadPersistent(Path,SL,True,False);
      Result := ReadList(Path, SL, ReadWideStringListItem, ItemName);
    finally
      SL.EndUpdate;
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteWideStringList(const Path: string;
  const SL: WideStrings.TWideStrings; const ItemName: string = cItem);
begin
  WriteList(Path, SL, SL.Count, WriteWideStringListItem, DeleteWideStringListItem, ItemName);
  WritePersistent(Path,SL);
end;
{$ENDIF}

function TJvCustomAppStorage.ReadStringObjectList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  Result := ReadStringObjectList(Path, SL, DefaultObjectListItemCreateEvent, ClearFirst, ItemName);
end;

function TJvCustomAppStorage.ReadStringObjectList(const Path: string;
  const SL: TStrings; ItemCreator: TJvAppStorageObjectListItemCreateEvent;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  FOldInstanceCreateEvent: TJvAppStorageObjectListItemCreateEvent;
begin
  if not ListStoredInt(Path, ItemName) and StorageOptions.DefaultIfValueNotExists then
    Result := SL.Count
  else
  begin
    SL.BeginUpdate;
    try
      ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
      Delete(TargetPath, Length(TargetPath) - 1, 2);
      if ClearFirst then
        SL.Clear;
      ReadPersistent(Path,SL,True,False);
      FOldInstanceCreateEvent := TargetStore.CurrentInstanceCreateEvent;
      try
        TargetStore.SetCurrentInstanceCreateEvent(ItemCreator);
        Result := TargetStore.ReadList(TargetPath, SL, TargetStore.ReadStringObjectListItem, ItemName);
      finally
        TargetStore.SetCurrentInstanceCreateEvent(FOldInstanceCreateEvent);
      end;
    finally
      SL.EndUpdate;
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteStringObjectList(const Path: string;
  const SL: TStrings; const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  TargetStore.WriteList(TargetPath, SL, SL.Count,
    TargetStore.WriteStringObjectListItem, TargetStore.DeleteStringObjectListItem, ItemName);
  WritePersistent(Path,SL);
end;

procedure TJvCustomAppStorage.ReadEnumerationInt(const Path: string;
  TypeInfo: PTypeInfo; const Default; out Value);
var
  OrdValue: Integer;
  Conv: TIdentToInt;
  S: string;
  TmpDefReadError: Boolean;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    CopyEnumValue(Default, Value, GetTypeData(TypeInfo).OrdType)
  else
  begin
    OrdValue := 0;
    CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType);
    if (TypeInfo = System.TypeInfo(Boolean)) or ((TypeInfo.Kind = tkEnumeration) and
      (GetTypeData(GetTypeData(TypeInfo).BaseType^).MinValue < 0)) then
      OrdValue := Ord(ReadBooleanInt(Path, OrdValue <> 0))
    else
    begin
      try
        if TypeInfo.Kind in [tkChar, tkWChar] then
          OrdValue := ReadIntegerInt(Path, OrdValue)
        else
          if TypeInfo.Kind = tkInteger then
          begin
            { Could be stored as a normal int or as an identifier.
              Try identifier first as that will not raise an exception }
            Conv := FindIdentToInt(TypeInfo);
            if Assigned(Conv) then
            begin
              TmpDefReadError := StorageOptions.DefaultIfReadConvertError;
              StorageOptions.DefaultIfReadConvertError := True;
              try
                S := ReadStringInt(Path, '');
              finally
                StorageOptions.DefaultIfReadConvertError := TmpDefReadError;
              end;
              if (S = '') or not (Conv(S, OrdValue)) then
                OrdValue := ReadIntegerInt(Path, OrdValue);
            end
            else
              OrdValue := ReadIntegerInt(Path, OrdValue);
          end
          else
            if TypeInfo.Kind = tkEnumeration then
            begin
              // Usage of an invalid identifier to signal the value does not exist
              OrdValue := GetEnumValue(TypeInfo, ReadStringInt(Path, cInvalidIdentifier));
              if OrdValue = -1 then
              begin
                OrdValue := ReadIntegerInt(Path, OrdValue);
                if OrdValue = -1 then
                  CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType)
              end;
            end
            else
              raise EJVCLAppStorageError.CreateRes(@RsEInvalidType);
      except
        on E: EConvertError do
          if StorageOptions.DefaultIfReadConvertError then
            CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType)
          else
            raise;
      end;
    end;
    CopyEnumValue(OrdValue, Value, GetTypeData(TypeInfo).OrdType);
  end;
end;

procedure TJvCustomAppStorage.WriteEnumerationInt(const Path: string;
  TypeInfo: PTypeInfo; const Value);
var
  Conv: TIntToIdent;
  S: string;
begin
  if TypeInfo = System.TypeInfo(Boolean) then
    WriteBooleanInt(Path, Boolean(Value))
  else
    if (TypeInfo.Kind = tkEnumeration) and
      (GetTypeData(GetTypeData(TypeInfo).BaseType^).MinValue < 0) then
      WriteBooleanInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType) <> 0)
    else
      if TypeInfo.Kind in [tkChar, tkWChar] then
        WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType))
      else
        if TypeInfo.Kind = tkInteger then
        begin
          if StorageOptions.TypedIntegerAsString then
          begin
            Conv := FindIntToIdent(TypeInfo);
            if Assigned(Conv) and Conv(OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType), S) then
              WriteStringInt(Path, S)
            else
              WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
          end
          else
            WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
        end
        else
          if TypeInfo.Kind = tkEnumeration then
          begin
            if StorageOptions.EnumerationAsString then
              WriteStringInt(Path, GetEnumName(TypeInfo, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType)))
            else
              WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
          end
          else
            raise EJVCLAppStorageError.CreateRes(@RsEInvalidType);
end;

procedure TJvCustomAppStorage.ReadEnumeration(const Path: string;
  TypeInfo: PTypeInfo; const Default; out Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  TargetStore.ReadEnumerationInt(TargetPath, TypeInfo, Default, Value);
end;

procedure TJvCustomAppStorage.WriteEnumeration(const Path: string;
  TypeInfo: PTypeInfo; const Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteEnumerationInt(TargetPath, TypeInfo, Value);
end;

procedure TJvCustomAppStorage.ReadSetInt(const Path: string;
  ATypeInfo: PTypeInfo; const Default; out Value);
var
  Lst: TStrings;
  I: Integer;
  JclOrdinalRangeTypeInfo : IJclOrdinalRangeTypeInfo;
  JclEnumerationTypeInfo : IJclEnumerationTypeInfo;
begin
  if IsFolder(Path) then
  begin
    Lst := TStringList.Create;
    try
      JclOrdinalRangeTypeInfo := (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclOrdinalRangeTypeInfo;
      case JclOrdinalRangeTypeInfo.GetTypeKind of
        tkEnumeration:
          begin
            JclEnumerationTypeInfo := ((JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclEnumerationTypeInfo);
              for I := JclEnumerationTypeInfo.GetMinValue to JclEnumerationTypeInfo.GetMaxValue do
                if ReadBooleanInt(ConcatPaths([Path, JclEnumerationTypeInfo.GetNames(I)]), False) then
                  Lst.Add(JclEnumerationTypeInfo.GetNames(I));
            (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
          end;
        tkChar:
          begin
            JclStrToSet(ATypeInfo, Value, ''); // empty out value
            for I := JclOrdinalRangeTypeInfo.GetMinValue to JclOrdinalRangeTypeInfo.GetMaxValue do
              if ReadBooleanInt(ConcatPaths([Path, GetCharName(Chr(I))]), False) then
                Include(TIntegerSet(Value), I);
          end;
        tkInteger:
          begin
            for I := JclOrdinalRangeTypeInfo.GetMinValue to JclOrdinalRangeTypeInfo.GetMaxValue do
              if ReadBooleanInt(ConcatPaths([Path, GetIntName(I)]), False) then
                Lst.Add(IntToStr(I));
            (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
          end;
      else
        raise EJVCLAppStorageError.CreateRes(@RsEUnknownBaseType);
      end;
    finally
      FreeAndNil(Lst);
    end;
  end
  else
    // It's stored as a string value or not stored at all
    JclStrToSet(ATypeInfo, Value, ReadStringInt(Path, JclSetToStr(ATypeInfo, Default, True)));
end;

procedure TJvCustomAppStorage.WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value);
var
  Lst: TStrings;
  I: Integer;
  JclOrdinalRangeTypeInfo : IJclOrdinalRangeTypeInfo;
  JclEnumerationTypeInfo : IJclEnumerationTypeInfo;
begin
  if StorageOptions.SetAsString then
    WriteStringInt(Path, JclSetToStr(ATypeInfo, Value, True))
  else
  begin
    Lst := TStringList.Create;
    try
      JclOrdinalRangeTypeInfo := (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclOrdinalRangeTypeInfo;
      begin
        case JclOrdinalRangeTypeInfo.GetTypeKind of
          tkEnumeration:
            begin
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).GetAsList(Value, False, Lst);
              JclEnumerationTypeInfo :=((JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclEnumerationTypeInfo);
              for I := JclEnumerationTypeInfo.GetMinValue to JclEnumerationTypeInfo.GetMaxValue do
                WriteBooleanInt(ConcatPaths([Path, JclEnumerationTypeInfo.GetNames(I)]),
                  Lst.IndexOf(JclEnumerationTypeInfo.GetNames(I)) > -1);
            end;
          tkChar:
            begin
              for I := JclOrdinalRangeTypeInfo.GetMinValue to JclOrdinalRangeTypeInfo.GetMaxValue do
                WriteBooleanInt(ConcatPaths([Path, GetCharName(Chr(I))]), I in TIntegerSet(Value));
            end;
          tkInteger:
            begin
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).GetAsList(Value, False, Lst);
              for I := JclOrdinalRangeTypeInfo.GetMinValue to JclOrdinalRangeTypeInfo.GetMaxValue do
                WriteBooleanInt(ConcatPaths([Path, GetIntName(I)]),
                  Lst.IndexOf(IntToStr(I)) > -1);
            end;
        else
          raise EJVCLAppStorageError.CreateRes(@RsEUnknownBaseType);
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end;
end;

procedure TJvCustomAppStorage.ReadSet(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  TargetStore.ReadSetInt(TargetPath, ATypeInfo, Default, Value);
end;

procedure TJvCustomAppStorage.WriteSet(const Path: string; ATypeInfo: PTypeInfo; const Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteSetInt(TargetPath, ATypeInfo, Value);
end;

procedure TJvCustomAppStorage.ReadProperty(const Path: string;
  const PersObj: TPersistent; const PropName: string; const Recursive, ClearFirst: Boolean);
var
  //Index: Integer;
  TmpValue: Integer;
  SubObj: TObject;
  P: PPropInfo;
begin
  if not Assigned(PersObj) then
    Exit;
  case PropType(PersObj, PropName) of
    {$IFDEF UNICODE} tkUString, {$ENDIF}
    tkLString, tkString:
      SetStrProp(PersObj, PropName, ReadString(Path, GetStrProp(PersObj, PropName)));
    tkWString:
      SetWideStrProp(PersObj, PropName, ReadWideString(Path, GetWideStrProp(PersObj, PropName)));
    tkEnumeration:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkVariant:
      SetVariantProp(PersObj, PropName, ReadString(Path, VarToStr(GetVariantProp(PersObj, PropName))));
    tkSet:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadSet(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkChar, tkWChar, tkInteger:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkInt64:
      SetInt64Prop(PersObj, PropName, StrToInt64(ReadString(Path,
        IntToStr(GetInt64Prop(PersObj, PropName)))));
    tkFloat:
      begin
        P := GetPropInfo(PersObj, PropName, tkAny);
        if (P <> nil) and (P.PropType <> nil) and (P.PropType^ = TypeInfo(TDateTime)) then
          SetFloatProp(PersObj, PropName, ReadDateTime(Path, GetFloatProp(PersObj, PropName)))
        else
          SetFloatProp(PersObj, PropName, ReadFloat(Path, GetFloatProp(PersObj, PropName)));
      end;
    tkClass:
      begin
        SubObj := GetObjectProp(PersObj, PropName);
        if (RegisteredAppStoragePropertyEngineList <> nil) and
          Recursive and
          RegisteredAppStoragePropertyEngineList.ReadProperty(Self, Path, PersObj, SubObj, Recursive, ClearFirst) then
          // Do nothing else, the handling is done in the ReadProperty procedure
        else
          if SubObj is TStrings then
            ReadStringList(Path, TStrings(SubObj), ClearFirst)
          else
            if (SubObj is TPersistent) and Recursive then
              if SubObj is TJvCustomPropertyStore then
              begin
                TJvCustomPropertyStore(SubObj).AppStoragePath := Path;
                TJvCustomPropertyStore(SubObj).AppStorage := Self;
                TJvCustomPropertyStore(SubObj).LoadProperties;
              end
              else
                if SubObj is TCollection then
                  ReadCollection(Path, TCollection(SubObj), ClearFirst)
                else
                  ReadPersistent(Path, TPersistent(SubObj), True, ClearFirst);
      end;
  end;
end;

procedure TJvCustomAppStorage.WriteProperty(const Path: string;
  const PersObj: TPersistent; const PropName: string; const Recursive: Boolean);
var
  TmpValue: Integer;
  SubObj: TObject;
  P: PPropInfo;

  function IsDefaultOrdProp(PropInfo: PPropInfo): Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Value := GetOrdProp(PersObj, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultStrProp(PropInfo: PPropInfo): Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(PersObj, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultInt64Prop(PropInfo: PPropInfo): Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(PersObj, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultFloatProp(PropInfo: PPropInfo): Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(PersObj, PropInfo);
    Result := Value = 0;
  end;

begin
  if not Assigned(PersObj) then
    Exit;

  P := GetPropInfo(PersObj, PropName, tkAny);

  // If not storing the default values, then do not do anything if the property
  // is read only, write only or its "stored" function returns False.
  // Note: we do not add a call to IsDefaultPropertyValue here because it would
  // return True for any sub component which is not desirable as we want to
  // always store sub classes whether they are components or not.
  if not StorageOptions.StoreDefaultValues and (
    not Assigned(P.GetProc) or not Assigned(P.SetProc) or
    not IsStoredProp(PersObj, P)) then
    Exit;

  case PropType(PersObj, PropName) of
    {$IFDEF UNICODE} tkUString, {$ENDIF}
    tkLString, tkString:
      if StorageOptions.StoreDefaultValues or not IsDefaultStrProp(P) then
        WriteString(Path, GetStrProp(PersObj, PropName));
    tkWString:
      if StorageOptions.StoreDefaultValues or not IsDefaultStrProp(P) then
        WriteWideString(Path, GetWideStrProp(PersObj, PropName));
    tkVariant:
      if StorageOptions.StoreDefaultValues or not IsDefaultStrProp(P) then
        WriteString(Path, VarToStr(GetVariantProp(PersObj, PropName)));
    tkEnumeration:
      begin
        if StorageOptions.StoreDefaultValues or not IsDefaultOrdProp(P) then
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          WriteEnumeration(Path, P.PropType^, TmpValue);
        end;
      end;
    tkSet:
      begin
        if StorageOptions.StoreDefaultValues or not IsDefaultOrdProp(P) then
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          WriteSet(Path, P.PropType^, TmpValue);
        end;
      end;
    tkChar, tkWChar, tkInteger:
      begin
        if StorageOptions.StoreDefaultValues or not IsDefaultOrdProp(P) then
        begin
          if StorageOptions.TypedIntegerAsString then
          begin
            TmpValue := GetOrdProp(PersObj, PropName);
            WriteEnumeration(Path, P.PropType^, TmpValue);
          end
          else
          begin
            WriteInteger(Path, GetOrdProp(PersObj, PropName));
          end;
        end;
      end;
    tkInt64:
      if StorageOptions.StoreDefaultValues or not IsDefaultInt64Prop(P) then
        WriteString(Path, IntToStr(GetInt64Prop(PersObj, PropName)));
    tkFloat:
      begin
        if StorageOptions.StoreDefaultValues or not IsDefaultFloatProp(P) then
        begin
          if (P <> nil) and (P.PropType <> nil) and (P.PropType^ = TypeInfo(TDateTime)) then
            WriteDateTime(Path, GetFloatProp(PersObj, PropName))
          else
            WriteFloat(Path, GetFloatProp(PersObj, PropName));
        end;
      end;
    tkClass:
      begin
        SubObj := GetObjectProp(PersObj, PropName);
        if (RegisteredAppStoragePropertyEngineList <> nil) and
          Recursive and
          RegisteredAppStoragePropertyEngineList.WriteProperty(Self, Path, PersObj, SubObj, Recursive) then
        begin
          // Do nothing else, the handling is done in the WriteProperty procedure
        end
        else
        begin
          if SubObj is TStrings then
          begin
            WriteStringList(Path, TStrings(SubObj))
          end
          else
          begin
            if (SubObj is TPersistent) and Recursive then
            begin
              if SubObj is TJvCustomPropertyStore then
              begin
                TJvCustomPropertyStore(SubObj).AppStoragePath := Path;
                TJvCustomPropertyStore(SubObj).AppStorage := Self;
                TJvCustomPropertyStore(SubObj).StoreProperties;
              end
              else
              begin
                if SubObj is TCollection then
                  WriteCollection(Path, TCollection(SubObj))
                else
                  WritePersistent(Path, TPersistent(SubObj), Recursive, nil);
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TJvCustomAppStorage.ReadPersistent(const Path: string; const PersObj: TPersistent;
  const Recursive, ClearFirst: Boolean; const IgnoreProperties: TStrings);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  JvAppStorageHandler: IJvAppStorageHandler;
begin
  if not Assigned(PersObj) then
    Exit;
  if Supports(PersObj, IJvAppStorageHandler, JvAppStorageHandler) then
    JvAppStorageHandler.ReadFromAppStorage(Self, Path);
  if not Supports(PersObj, IJvAppStorageHandler) or
    Supports(PersObj, IJvAppStoragePublishedProps) then
    for Index := 0 to GetPropCount(PersObj) - 1 do
    begin
      PropName := GetPropName(PersObj, Index);
      KeyName := TranslatePropertyName(PersObj, PropName, False);
      PropPath := ConcatPaths([Path, KeyName]);
      if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
        ReadProperty(PropPath, PersObj, PropName, Recursive, ClearFirst);
    end;
end;

procedure TJvCustomAppStorage.WritePersistent(const Path: string;
  const PersObj: TPersistent; const Recursive: Boolean; const IgnoreProperties: TStrings);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  JvAppStorageHandler: IJvAppStorageHandler;
begin
  if not Assigned(PersObj) then
    Exit;
  if Supports(PersObj, IJvAppStorageHandler, JvAppStorageHandler) then
    JvAppStorageHandler.WriteToAppStorage(Self, Path);
  if not Supports(PersObj, IJvAppStorageHandler) or Supports(PersObj, IJvAppStoragePublishedProps) then
    for Index := 0 to GetPropCount(PersObj) - 1 do
    begin
      PropName := GetPropName(PersObj, Index);
      KeyName := TranslatePropertyName(PersObj, PropName, False);
      PropPath := ConcatPaths([Path, KeyName]);
      if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
        WriteProperty(PropPath, PersObj, PropName, Recursive);
    end;
end;

function TJvCustomAppStorage.GetCharName(Ch: Char): string;
begin
  if CharInSet(Ch, ['!'..'z']) then
    Result := 'Char_' + Ch
  else
    Result := 'Char#' + IntToStr(Ord(Ch));
end;

function TJvCustomAppStorage.GetIntName(Value: Integer): string;
begin
  Result := 'Int_' + IntToStr(Value);
end;

function TJvCustomAppStorage.EncryptPropertyValue(Value: string): string;
begin
  if Assigned(FOnEncryptPropertyValue) and IsPropertyValueCryptEnabled then
  begin
    FOnEncryptPropertyValue(Value);
    Value := string(MimeEncodeString(AnsiString(Value)));
  end;
  Result := Value;
end;

function TJvCustomAppStorage.DecryptPropertyValue(Value: string): string;
begin
  if Assigned(FOnDecryptPropertyValue) and IsPropertyValueCryptEnabled then
  begin
    Value := string(MimeDecodeString(AnsiString(Value)));
    FOnDecryptPropertyValue(Value);
  end;
  Result := Value;
end;

function TJvCustomAppStorage.TranslatePropertyName(Instance: TPersistent;
  const AName: string; const Reading: Boolean): string;
begin
  Result := AName;
  if Instance is TJvCustomPropertyStore then
    Result := TJvCustomPropertyStore(Instance).TranslatePropertyName(Result)
  else
    DoTranslatePropertyName(Instance, Result, Reading);
end;

procedure TJvCustomAppStorage.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

function TJvCustomAppStorage.GetReadOnly: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FReadOnly
  else
    Result := FReadOnly or GetPhysicalReadOnly;
end;

function TJvCustomAppStorage.GetPhysicalReadOnly: Boolean;
begin
  Result := False;
end;

procedure TJvCustomAppStorage.GetStoredValues(const Path: string;
  const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
var
  SearchPath: string;
  I: Integer;
  OptimizedSearchPath: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    SearchPath := OptimizePaths([Path]);

    if aeoReportRelative in Options then
    begin
      InternalGetStoredValues('', SearchPath, Strings, Options);
    end
    else
    begin
      OptimizedSearchPath := OptimizePaths([Self.Path, SearchPath]);
      InternalGetStoredValues(OptimizedSearchPath +
        PathDelim, SearchPath, Strings, Options);

      // Mantis 3803: Only remove the path if ReportRelative was not asked.
      // If not, then with \F1\R1 and \F1\F1 we would only return the values
      // in \F1\R1 in "relative mode" which is not correct
      I := Strings.IndexOf(OptimizedSearchPath);
      if I > -1 then
        Strings.Delete(I);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ Enables the Cryption of Property-Values (Only String-Values) }

procedure TJvCustomAppStorage.EnablePropertyValueCrypt;
begin
  Inc(FCryptEnabledStatus);
end;

{ Disables the Cryption of Property-Values (Only String-Values) }

procedure TJvCustomAppStorage.DisablePropertyValueCrypt;
begin
  Dec(FCryptEnabledStatus);
end;

{ Returns the current state if Property-Value Cryption is enabled }

function TJvCustomAppStorage.IsPropertyValueCryptEnabled: Boolean;
begin
  Result := (FCryptEnabledStatus > 0);
end;

function TJvCustomAppStorage.ItemNameIndexPath(const ItemName: string; const
    Index: Integer): string;
begin
  if StorageOptions.UseOldItemNameFormat then
    Result := ItemName + IntToStr(Index)
  else
    Result := ItemName + '['+IntToStr(Index)+']';
end;

procedure TJvCustomAppStorage.Loaded;
begin
  inherited Loaded;
  if not IsUpdating then
    Reload;
end;

procedure TJvCustomAppStorage.BeginUpdate;
var i : Integer;
begin
  GetFormatSettings;
  ReloadIfNeeded;
  Inc(FUpdateCount);
  for i  := 0 to SubStorages.Count - 1 do
    if Assigned(SubStorages[i].AppStorage) then
      SubStorages[i].AppStorage.BeginUpdate;
end;

procedure TJvCustomAppStorage.CheckDeletePathByVersion(const Path: string;
  VersionNumber: Integer; DeleteIfNotEqual: Boolean = False;
  WriteVersionNumber: Boolean = True; const VersionName: string = 'Version');
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  OldVersionNumber: Integer;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly and (Versionname <> '') then
  begin
    TargetStore.BeginUpdate;
    try
      OldVersionNumber := Targetstore.ReadInteger(TargetStore.ConcatPaths([Path, VersionName]));
      if DeleteIfNotEqual and (OldVersionNumber <> VersionNumber) then
        Targetstore.DeleteSubTree(Path)
      else
        if (OldVersionNumber < VersionNumber) then
          Targetstore.DeleteSubTree(Path);
      if (OldVersionNumber <> VersionNumber) and WriteVersionNumber then
        TargetStore.WriteInteger(TargetStore.ConcatPaths([Path, VersionName]), VersionNumber);
    finally
      TargetStore.EndUpdate;
    end;
  end;
end;

function TJvCustomAppStorage.DecodeStrToDateTime(Value: string): TDateTime;
begin
  if StorageOptions.UseTranslateStringEngineDateTimeFormats then
    try
      Result := StrToDateTime(Value{$IFDEF COMPILER7_UP}, GetFormatSettings{$ENDIF COMPILER7_UP});
    except
      on E: EConvertError do
        Result := StrToDateTime(Value);
    end
  else
    Result := StrToDateTime(Value);
end;

function TJvCustomAppStorage.EncodeDateTimeToStr(Value: TDateTime): string;
begin
  if StorageOptions.UseTranslateStringEngineDateTimeFormats then
    Result := DateTimeToStr(Value{$IFDEF COMPILER7_UP}, GetFormatSettings{$ENDIF COMPILER7_UP})
  else
    Result := DateTimeToStr(Value);
end;

procedure TJvCustomAppStorage.EndUpdate;
var i : Integer;
begin
  for i  := 0 to SubStorages.Count - 1 do
    if Assigned(SubStorages[i].AppStorage) then
      SubStorages[i].AppStorage.EndUpdate;
  Dec(FUpdateCount);
  FlushIfNeeded;
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

function TJvCustomAppStorage.GetActiveTranslateStringEngine: TJvTranslateString;
begin
  if Assigned(TranslateStringEngine) then
    Result := TranslateStringEngine
  else
    Result := FInternalTranslateStringEngine;
end;

function TJvCustomAppStorage.GetFormatSettings: TFormatSettings;
begin
  {$IFDEF COMPILER7_UP}
  if Not IsUpdating then
  begin
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, CachedFormatSettings);
    if Assigned(ActiveTranslateStringEngine) then
    begin
      if (ActiveTranslateStringEngine.DateFormat <> '') then
      begin
        CachedFormatSettings.ShortDateFormat := Self.ActiveTranslateStringEngine.DateFormat;
        CachedFormatSettings.LongDateFormat := Self.ActiveTranslateStringEngine.DateFormat;
      end;
      if (ActiveTranslateStringEngine.TimeFormat <> '') then
      begin
        CachedFormatSettings.ShortTimeFormat := Self.ActiveTranslateStringEngine.TimeFormat;
        CachedFormatSettings.LongTimeFormat := Self.ActiveTranslateStringEngine.TimeFormat;
      end;
    end;
  end;
  {$ENDIF COMPILER7_UP}
  Result := CachedFormatSettings;
end;

function TJvCustomAppStorage.GetUpdating: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

function TJvCustomAppStorage.ReadListItemCount(const Path: string; const
    ItemName: string = cItem): Integer;
begin
  Result := ReadInteger(ConcatPaths([Path, cCount]), 0);
end;

procedure TJvCustomAppStorage.Synchronize(AMethod: TSynchronizeMethod;
    AIdentifier: String);
var
  JclMutex: TJclMutex;
begin
  if Assigned(AMethod) then
  begin
    JclMutex := TJclMutex.Create(nil, False,
      string(B64Encode(AnsiString(RsJvAppStorageSynchronizeProcedureName + AIdentifier))));
    try
      if JclMutex.WaitForever = wrSignaled then
      try
        AMethod;
      finally
        JclMutex.Release;
      end
      else
        raise Exception.CreateResFmt(@RsJvAppStorageSynchronizeTimeout, [RsJvAppStorageSynchronizeProcedureName+AIdentifier]);
    finally
      FreeAndNil(JclMutex);
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteListItemCount(const Path: string; const
    ItemCount: Integer; const ItemName: string = cItem);
begin
  WriteInteger(ConcatPaths([Path, cCount]), ItemCount);
end;

procedure TJvCustomAppStorage.SetTranslateStringEngine(const Value: TJvTranslateString);
begin
  ReplaceComponentReference(Self, Value, TComponent(FTranslateStringEngine));
end;

function TJvCustomAppStorage.ReadWideString(const Path: string;
  const Default: WideString = ''): WideString;
begin
  Result := DoReadWideString(Path,Default);
end;

procedure TJvCustomAppStorage.SetCurrentInstanceCreateEvent(const Value:
    TJvAppStorageObjectListItemCreateEvent);
begin
  FCurrentInstanceCreateEvent := Value;
end;

procedure TJvCustomAppStorage.WriteWideString(const Path: string;
  const Value: WideString);
begin
  DoWriteWideString(Path,Value);
end;

//=== { TJvAppStorage } ======================================================

function TJvAppStorage.IsFolderInt(const Path: string; ListIsValue: Boolean): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.PathExistsInt(const Path: string): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ValueStoredInt(const Path: string): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.DeleteValueInt(const Path: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.DeleteSubTreeInt(const Path: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteIntegerInt(const Path: string; Value: Integer);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteFloatInt(const Path: string; Value: Extended);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadStringInt(const Path: string; const Default: string): string;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteStringInt(const Path: string; const Value: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadBinaryInt(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteBinaryInt(const Path: string; const Buf: TJvBytes; BufSize: Integer);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.ReadSetInt(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

//=== { TJvAppSubStorages } ==================================================

constructor TJvAppSubStorages.Create(AOwner: TJvCustomAppStorage);
begin
  inherited Create(AOwner, TJvAppSubStorage);
end;

function TJvAppSubStorages.GetRootStorage: TJvCustomAppStorage;
begin
  Result := TJvCustomAppStorage(GetOwner);
end;

function TJvAppSubStorages.GetItem(I: Integer): TJvAppSubStorage;
begin
  Result := TJvAppSubStorage(inherited GetItem(I));
end;

procedure TJvAppSubStorages.SetItem(I: Integer; Value: TJvAppSubStorage);
begin
  inherited SetItem(I, Value);
end;

procedure TJvAppSubStorages.RootOptionsChanged;
begin
end;

function TJvAppSubStorages.CheckUniqueBase(const APath: string; IgnoreIndex: Integer): Boolean;
begin
  Result := MatchFor(OptimizePaths([APath]) + cSubStorePath, IgnoreIndex) = nil;
end;

function TJvAppSubStorages.MatchFor(APath: string; IgnoreIndex: Integer): TJvAppSubStorage;
var
  I: Integer;
begin
  Result := nil;
  APath := OptimizePaths([APath]);
  // APath is now a valid path, stripped from it's leading/trailing backslashes
  for I := 0 to Count - 1 do
    if I <> IgnoreIndex then
      if AnsiStartsText(Items[I].RootPath, APath) then
        // Possible match. Check if next char is a \
        if APath[Length(Items[I].RootPath) + 1] = PathDelim then
          { Next char in APath is a backslash, so we have a valid match. Check with any previous
            to see if it is better than that one. }
          if (Result = nil) or (Length(Result.RootPath) < Length(Items[I].RootPath)) then
            Result := Items[I]; // no previous match or new match is close to what we searched for
end;

procedure TJvAppSubStorages.Add(RootPath: string; AppStorage: TJvCustomAppStorage);
var
  Tmp: TJvAppSubStorage;
begin
  Tmp := TJvAppSubStorage.Create(Self);
  try
    Tmp.RootPath := RootPath;
    Tmp.AppStorage := AppStorage;
  except
    FreeAndNil(Tmp);
    raise;
  end;
end;

procedure TJvAppSubStorages.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TJvAppSubStorages.Delete(RootPath: string; const IncludeSubPaths: Boolean);
var
  I: Integer;
  SubPath: string;
begin
  RootPath := OptimizePaths([RootPath]);
  if RootPath <> '' then
  begin
    SubPath := RootPath + PathDelim;
    I := Count - 1;
    while I >= 0 do
    begin
      if AnsiSameText(RootPath, Items[I].RootPath) or
        (IncludeSubPaths and (AnsiStartsText(SubPath, Items[I].RootPath))) then
        Delete(I);
      Dec(I);
    end;
  end;
end;

procedure TJvAppSubStorages.Delete(AppStorage: TJvCustomAppStorage);
var
  I: Integer;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    if Items[I].AppStorage = AppStorage then
      Delete(I);
    Dec(I);
  end;
end;

//=== { TJvAppSubStorage } ===================================================

function TJvAppSubStorage.GetOwnerStore: TJvCustomAppStorage;
begin
  Result := TJvAppSubStorages(Collection).RootStorage;
end;

function TJvAppSubStorage.GetDisplayName: string;
begin
  if (RootPath <> '') and (AppStorage <> nil) then
    Result := PathDelim + RootPath + '=' + AppStorage.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TJvAppSubStorage.SetRootPath(Value: string);
begin
  Value := OptimizePaths([Value]);
  if Value <> RootPath then
    if TJvAppSubStorages(Collection).CheckUniqueBase(Value, Index) then
      FRootPath := Value
    else
      raise EJVCLAppStorageError.CreateResFmt(@RsENotAUniqueRootPath, [Value]);
end;

procedure TJvAppSubStorage.SetAppStorage(Value: TJvCustomAppStorage);
begin
  if Value <> AppStorage then
  begin
    if (Value <> nil) and (Value.HasSubStorage(OwnerStore) or (Value = OwnerStore)) then
      raise EJVCLAppStorageError.CreateRes(@RsECircularReferenceOfStorages);
    ReplaceComponentReference(OwnerStore, Value, TComponent(FAppstorage));
  end;
end;

//=== { TJvAppStorageFileName } ==============================================

{procedure TJvAppStorageFileName.SetLocation(Value: TFileLocation);
begin
  if Location <> Value then
  begin
    FLocation := Value;
    DoChange;
  end;
end;

procedure TJvAppStorageFileName.SetFileName(Value: TFileName);
begin
  if FileName <> Value then
  begin
    FFileName := Value;
    DoChange;
  end;
end;

procedure TJvAppStorageFileName.DoChange;
begin
  if Assigned(FOnChange) then
    OnChange(Self);
end;

function TJvAppStorageFileName.GetFileName: TFileName;
var
  NameOnly: string;
  RelPathName: string;
begin
  if FileName = '' then
    Result := ''
  else
  begin
    NameOnly := ExtractFileName(FileName);
    if PathIsAbsolute(FileName) then
      RelPathName := NameOnly
    else
      RelPathName := FileName;
    case Location of
      flCustom:
        Result := FileName;
      flTemp:
        Result := PathAddSeparator(GetWindowsTempFolder) + NameOnly;
      flWindows:
        Result := PathAddSeparator(GetWindowsFolder) + NameOnly;
      flExeFile:
        Result := ExtractFilePath(Application.ExeName) + NameOnly;
      flUserFolder:
        Result := PathAddSeparator(GetAppdataFolder) + RelPathName;
    end;
  end;
end;

constructor TJvAppStorageFileName.Create(ADefaultExtension: string);
begin
  inherited Create;
  FLocation := flExeFile;
  FFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.' + ADefaultExtension);
end;  }

//=== { TJvCustomAppMemoryFileStorage } ======================================

constructor TJvCustomAppMemoryFileStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocation := flExeFile;
  FPhysicalReadOnly := False;
  FFileLoaded := False;
end;

procedure TJvCustomAppMemoryFileStorage.RecalculateFullFileName;
var
  NameOnly: string;
  RelPathName: string;
  TransFileName: string;
begin
  if (FileName = '') and (Location <> flCustom) then
  begin
    FFullFileName := '';
  end
  else
  begin
    TransFileName := ActiveTranslateStringEngine.TranslateString(FileName);
    NameOnly := ExtractFileName(TransFileName);
    if PathIsAbsolute(TransFileName) then
      RelPathName := NameOnly
    else
      RelPathName := TransFileName;
    case Location of
      flCustom:
        FFullFileName := DoGetFileName;
      flExeFile:
        FFullFileName := PathAddSeparator(ExtractFilePath(ParamStr(0))) + NameOnly;
      {$IFDEF MSWINDOWS}
      flTemp:
        FFullFileName := PathAddSeparator(GetWindowsTempFolder) + NameOnly;
      flWindows:
        FFullFileName := PathAddSeparator(GetWindowsFolder) + NameOnly;
      flUserFolder:
        FFullFileName := PathAddSeparator(GetAppdataFolder) + RelPathName;
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      flTemp:
        FFullFileName := PathAddSeparator(PathGetTempPath) + NameOnly;
      flUserFolder:
        FFullFileName := PathAddSeparator(GetEnvironmentVariable('HOME')) + RelPathName;
      {$ENDIF UNIX}
    end;
  end;
  FPhysicalReadOnly := FileExists(FullFileName) and FileIsReadOnly(FullFileName);
end;

procedure TJvCustomAppMemoryFileStorage.Reload;
begin
  FFileLoaded := True;
  FPhysicalReadOnly := FileExists(FullFileName) and FileIsReadOnly(FullFileName);
  inherited Reload;
end;

function TJvCustomAppMemoryFileStorage.ReloadNeeded: Boolean;
begin
  Result := (not FFileLoaded or AutoReload) and not IsUpdating;
end;

function TJvCustomAppMemoryFileStorage.GetPhysicalReadOnly: Boolean;
begin
  Result := FPhysicalReadOnly;
end;

function TJvCustomAppMemoryFileStorage.DoGetFileName: TFileName;
begin
  Result := ActiveTranslateStringEngine.TranslateString(FileName);
  if Assigned(FOnGetFileName) then
    FOnGetFileName(Self, Result);
end;

procedure TJvCustomAppMemoryFileStorage.SetFileName(const Value: TFileName);
begin
  if Value <> FileName then
  begin
    if not (csLoading in ComponentState) and not IsUpdating then
      Flush;

    // Mantis 3680: only add an extension if there is not already one.
    if (Length(ExtractFileExt(Value)) = 0) then
    begin
      FFileName := PathAddExtension(Value, DefaultExtension);
    end
    else
    begin
      FFileName := Value;
    end;

    RecalculateFullFileName;
    if not (csLoading in ComponentState) and not IsUpdating then
      Reload;
  end;
end;

procedure TJvCustomAppMemoryFileStorage.SetOnGetFileName(Value: TJvAppStorageGetFileNameEvent);
begin
  if not (csLoading in ComponentState) and not IsUpdating then
    Flush;
  FOnGetFileName := Value;
  RecalculateFullFileName;
  if not (csLoading in ComponentState) and not IsUpdating then
    Reload;
end;

procedure TJvCustomAppMemoryFileStorage.SetLocation(const Value: TFileLocation);
begin
  if FLocation <> Value then
  begin
    if not (csLoading in ComponentState) and not IsUpdating then
      Flush;
    FLocation := Value;
    RecalculateFullFileName;
    if not (csLoading in ComponentState) and not IsUpdating then
      Reload;
  end;
end;

function TJvCustomAppMemoryFileStorage.DefaultExtension: string;
begin
  Result := '';
end;

//=== { TJvAppStoragePropertyBaseEngine } ====================================

constructor TJvAppStoragePropertyBaseEngine.Create;
begin
  inherited Create;
  // virtual constructor
end;

function TJvAppStoragePropertyBaseEngine.Supports(AObject: TObject; AProperty: TObject): Boolean;
begin
  Result := False;
end;

procedure TJvAppStoragePropertyBaseEngine.ReadProperty(AStorage: TJvCustomAppStorage;
  const APath: string; AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean);
begin
end;

procedure TJvAppStoragePropertyBaseEngine.WriteProperty(AStorage: TJvCustomAppStorage;
  const APath: string; AObject: TObject; AProperty: TObject; const Recursive: Boolean);
begin
end;

//=== { TJvAppStoragePropertyEngineList } ====================================

destructor TJvAppStoragePropertyEngineList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    TJvAppStoragePropertyBaseEngine(Items[I]).Free;
    Delete(I);
  end;
  inherited Destroy;
end;

procedure TJvAppStoragePropertyEngineList.RegisterEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
begin
  Add(AEngineClass.Create);
end;

procedure TJvAppStoragePropertyEngineList.UnregisterEngine(AEngineClass: TJvAppStoragePropertyBaseEngineClass);
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  I := 0;
  while (I < Count) and not Found do
  begin
    if TObject(Items[I]).ClassType = AEngineClass then
    begin
      TJvAppStoragePropertyBaseEngine(Items[I]).Free;
      Delete(I);
      Found := True;
    end;
    Inc(I);
  end;
end;

function TJvAppStoragePropertyEngineList.GetEngine(AObject: TObject;
  AProperty: TObject): TJvAppStoragePropertyBaseEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if TJvAppStoragePropertyBaseEngine(Items[Ind]).Supports(AObject, AProperty) then
    begin
      Result := TJvAppStoragePropertyBaseEngine(Items[Ind]);
      Break;
    end;
end;

function TJvAppStoragePropertyEngineList.ReadProperty(AStorage: TJvCustomAppStorage; const APath: string;
  AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean): Boolean;
var
  Engine: TJvAppStoragePropertyBaseEngine;
begin
  Engine := GetEngine(AObject, AProperty);
  Result := Assigned(Engine);
  if Result then
    Engine.ReadProperty(AStorage, APath, AObject, AProperty, Recursive, ClearFirst);
end;

function TJvAppStoragePropertyEngineList.WriteProperty(AStorage: TJvCustomAppStorage;
  const APath: string; AObject: TObject; AProperty: TObject; const Recursive: Boolean): Boolean;
var
  Engine: TJvAppStoragePropertyBaseEngine;
begin
  Engine := GetEngine(AObject, AProperty);
  Result := Assigned(Engine);
  if Result then
    Engine.WriteProperty(AStorage, APath, AObject, AProperty, Recursive);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFNDEF SUPPORTS_CLASS_CTORDTORS}
  DestroyAppStoragePropertyEngineList;
  {$ENDIF ~SUPPORTS_CLASS_CTORDTORS}

  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
