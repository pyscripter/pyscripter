{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgramVersionCheck.PAS, released on 2004-12-16.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott com]
Contributor : Ed Blanchard
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgramVersionCheck;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF USE_3RDPARTY_INDY}
  IdHTTP, IdFTP,
  {$ENDIF USE_3RDPARTY_INDY}
  {$IFDEF USE_3RDPARTY_ICS}
  {$IFDEF DELPHI7_UP}
  OverbyteIcsHttpProt, OverbyteIcsFtpCli,
  {$ELSE}
  HttpProt, FtpCli,
  {$ENDIF DELPHI7_UP}
  {$ENDIF USE_3RDPARTY_ICS}
  JvPropertyStore, JvAppStorage, JvAppIniStorage, JvAppXMLStorage,
  JvParameterList, JvThread, JvThreadDialog, SysUtils;

type
  { Type of release of a Program Version }
  TJvProgramReleaseType = (prtProduction, prtBeta, prtAlpha);

  TJvRemoteVersionOperation = (rvoIgnore, rvoCopy, rvoCopyInstall);

  { List class to collect and sort version infos }
  TJvProgramVersionsStringList = class(TStringList)
  public
    procedure Sort; override;
  end;

  { Class to collect all informations about a program version
    These informations will be stored in the ini-file on the remote site}
  TJvProgramVersionInfo = class(TJvCustomPropertyStore)
  private
    FDownloadPasswordRequired: Boolean;
    FLocalInstallerParams: string;
    FVersionDescription: TStringList;
    FProgramSize: Integer;
    FProgramVersion: string;
    FProgramLocationPath: string;
    FProgramLocationFileName: string;
    FProgramReleaseType: TJvProgramReleaseType;
    FProgramReleaseDate: TDateTime;
    function GetVersionDescription: TStrings;
    procedure SetVersionDescription(Value: TStrings);
  protected
    procedure StoreXMLProperties; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function EditIntf_GetObjectHint: string; override;
    function EditIntf_GetPropertyHint(const PropertyName: string): string; override;
    //IJvPropertyEditorHandler = interface
    function EditIntf_GetVisibleObjectName: string; override;
    { Combination of ProgramVersion and ReleaseType }
    function ProgramVersionReleaseType: string;
    function ProgramSizeString: string;
    function ProgramVersionInfo: string;
  published
    //1 Flag to define whether a password is required for the download or not
    property DownloadPasswordRequired: Boolean read FDownloadPasswordRequired write FDownloadPasswordRequired default False;
    { List of parameters for the execution of the installer file }
    property LocalInstallerParams: string read FLocalInstallerParams write FLocalInstallerParams;
    { Path where the installer of the version could be found. This could be
    a absolute path or a relative path to the location of the version list file }
    property ProgramLocationPath: string read FProgramLocationPath write FProgramLocationPath;
    { File name of the installer file }
    property ProgramLocationFileName: string read FProgramLocationFileName write FProgramLocationFileName;
    { Program version in the format <main>.<sub>.<release>.<build>
    This property is compared with the fileversion properties of the current
    application. }
    property ProgramVersion: string read FProgramVersion write FProgramVersion;
    { This is a description field which could be shown in the update dialog via
      the version info button }
    property VersionDescription: TStrings read GetVersionDescription write SetVersionDescription;
    { Release type of the version.
    In the update dialog there are only the highest version numbers for each type
    visible. The type must be higher then AllowedReleaseType property of the
    TJvProgramVersionCheck component }
    property ProgramReleaseType: TJvProgramReleaseType read FProgramReleaseType write FProgramReleaseType;
    { Size of the installer in bytes }
    property ProgramSize: Integer read FProgramSize write FProgramSize;
    { Date of Release }
    property ProgramReleaseDate: TDateTime read FProgramReleaseDate write FProgramReleaseDate;
  end;

  TJvProgramVersionInfoReleaseArray = array[TJvProgramReleaseType] of TJvProgramVersionInfo;

  { List of all Program version stored in a remote file via TJvAppStorage }
  TJvProgramVersionHistory = class(TJvCustomPropertyListStore)
  private
    FCurrentProductionVersion: string;
    FCurrentBetaVersion: string;
    FCurrentAlphaVersion: string;
    FCurrentProgramVersion: TJvProgramVersionInfoReleaseArray;
  protected
    function CreateObject: TPersistent; override;
    function CreateItemList: TStringList; override;
    function GetProgramVersion(Index: Integer): TJvProgramVersionInfo;
    function GetCurrentProgramVersion(Index: TJvProgramReleaseType): TJvProgramVersionInfo;
    function SearchCurrentProgramVersion(AProgramReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
    function GetCurrentProductionProgramVersion: string;
    function GetCurrentBetaProgramVersion: string;
    function GetCurrentAlphaProgramVersion: string;
    property ProgramVersion[Index: Integer]: TJvProgramVersionInfo read GetProgramVersion;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadData; override;
    procedure RecalculateCurrentProgramVersions;
    function AllowedCurrentProgramVersion(AAllowedReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
    procedure Assign(Source: TPersistent); override;
    function EditIntf_GetObjectHint: string; override;
    function EditIntf_GetPropertyHint(const PropertyName: string): string; override;
    function EditIntf_GetVisibleObjectName: string; override;
    function GetVersionsDescription(const AFromVersion, AToVersion: string): string;
    property CurrentProgramVersion[Index: TJvProgramReleaseType]: TJvProgramVersionInfo read GetCurrentProgramVersion;
  published
    //1 Auto calculated version number of the highest production version
    property CurrentProductionProgramVersion: string read GetCurrentProductionProgramVersion write FCurrentProductionVersion;
    //1 Auto calculated version number of the highest beta version
    property CurrentBetaProgramVersion: string read GetCurrentBetaProgramVersion write FCurrentBetaVersion;
    //1 Auto calculated version number of the highest alpha version
    property CurrentAlphaProgramVersion: string read GetCurrentAlphaProgramVersion write FCurrentAlphaVersion;
  end;

  { Base class for all location
    A Location is the class which defines where the remote files could be found and
    manages all communications to these files. }
  TJvCustomProgramVersionLocation = class(TJvCustomPropertyStore)
  private
    FDownloadError: string;
    FDownloadStatus: string;
    FDownloadThreaded: Boolean;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; virtual;
    procedure SetDownloadError(const Value: string); virtual;
    property DownloadStatus: string read FDownloadStatus write FDownloadStatus;
    property DownloadThreaded: Boolean read FDownloadThreaded write FDownloadThreaded default False;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadFileFromRemote(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; virtual;
    function LoadInstallerFileFromRemote(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; virtual;
    function LoadVersionInfoFromRemote(const ALocalDirectory, ALocalVersionInfoFileName: string;
      ABaseThread: TJvBaseThread): string; virtual;
    property DownloadError: string read FDownloadError write SetDownloadError;
  end;

  { Base class for all file based Locations like Network, FTP and HTTP }
  TJvCustomProgramVersionFileBasedLocation = class(TJvCustomProgramVersionLocation)
  private
    FVersionInfoLocationPathList: TStringList;
    FVersionInfoFileName: string;
    FValidLocationPath: string;
    function GetVersionInfoLocationPathList: TStrings;
    procedure SetVersionInfoLocationPathList(Value: TStrings);
    { If the location has a list of possible pathes, this property contains
    the path where the last valid download has happend}
    property ValidLocationPath: string read FValidLocationPath;
    { List of locations-path where the remote files could be found
    The application loops throuh all path from the top }
    property VersionInfoLocationPathList: TStrings read GetVersionInfoLocationPathList
      write SetVersionInfoLocationPathList;
    { Name of the VersionInfofile at the remote location }
    property VersionInfoFileName: string read FVersionInfoFileName write FVersionInfoFileName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadVersionInfoFromRemote(const ALocalDirectory,
      ALocalVersionInfoFileName: string; ABaseThread: TJvBaseThread): string; override;
    function LoadInstallerFileFromRemote(const ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName: string; ABaseThread: TJvBaseThread): string; override;
  end;

  { Location Class for Local Network Location }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionNetworkLocation = class(TJvCustomProgramVersionFileBasedLocation)
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
  public
    property ValidLocationPath;
  published
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;

  { Class for Proxy Settings for FTP and HTTP locations }
  TJvProgramVersionProxySettings = class(TPersistent)
  private
    FServer: string;
    FPort: Integer;
    FUserName: string;
    FPassword: string;
  public
    constructor Create;
  published
    property Server: string read FServer write FServer;
    property Port: Integer read FPort write FPort default 80;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  { Base class for all Internet locations  }
  TJvCustomProgramVersionInternetLocation = class(TJvCustomProgramVersionFileBasedLocation)
  private
    FProxySettings: TJvProgramVersionProxySettings;
    FPasswordRequired: Boolean;
    FUserName: string;
    FPassword: string;
    FPort: Integer;
  protected
    property ProxySettings: TJvProgramVersionProxySettings read FProxySettings;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property PasswordRequired: Boolean read FPasswordRequired write FPasswordRequired default False;
    property Port: Integer read FPort write FPort default 80;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvProgramVersionHTTPLocation = class;
  TJvLoadFileFromRemoteHTTPEvent = function(AProgramVersionLocation: TJvProgramVersionHTTPLocation;
    const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string): string of object;

  { Simple HTTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionHTTPLocation = class(TJvCustomProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
  public
    property ValidLocationPath;
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
    property ProxySettings;
    property UserName;
    property Password;
    property PasswordRequired;
    property Port;
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionHTTPLocationIndy = class(TJvProgramVersionHTTPLocation)
  private
    FIdHttp: TIdHttp;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ValidLocationPath;
  published
    property ProxySettings;
    property UserName;
    property Password;
    property PasswordRequired;
    property Port;
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;
  {$ENDIF USE_3RDPARTY_INDY}

  {$IFDEF USE_3RDPARTY_ICS}
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionHTTPLocationICS = class(TJvProgramVersionHTTPLocation)
  private
    FHttpCli: THttpCli;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ValidLocationPath;
  published
    property ProxySettings;
    property UserName;
    property Password;
    property PasswordRequired;
    property Port;
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;
  {$ENDIF USE_3RDPARTY_ICS}

  TJvProgramVersionFTPLocation = class;
  TJvLoadFileFromRemoteFTPEvent = function(AProgramVersionLocation: TJvProgramVersionFTPLocation;
    const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string): string of object;

  { Simple FTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionFTPLocation = class(TJvCustomProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
    property ProxySettings;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  TJvProgramVersionFTPLocationIndy = class(TJvProgramVersionFTPLocation)
  private
    FIdFtp: TIdFtp;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ValidLocationPath;
  published
    property ProxySettings;
    property UserName;
    property Password;
    property PasswordRequired;
    property Port;
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;
  {$ENDIF USE_3RDPARTY_INDY}

  {$IFDEF USE_3RDPARTY_ICS}
  TJvProgramVersionFTPLocationICS = class(TJvProgramVersionFTPLocation)
  private
    FFtpClient: TFtpClient;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ValidLocationPath;
  published
    property ProxySettings;
    property UserName;
    property Password;
    property PasswordRequired;
    property Port;
    property VersionInfoLocationPathList;
    property VersionInfoFileName;
  end;
  {$ENDIF USE_3RDPARTY_ICS}

  TJvProgramVersionDatabaseLocation = class;
  TJvLoadFileFromRemoteDatabaseEvent = function(AProgramVersionLocation: TJvProgramVersionDatabaseLocation;
    const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string): string of object;

  { Simple Database location class with no http logic.
    The logic must be implemented manually in the OnLoadFileFromRemote event }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionDatabaseLocation = class(TJvCustomProgramVersionLocation)
  private
    FServerName: string;
    FUserName: string;
    FPassword: string;
    FSelectStatementVersion: string;
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent;
  protected
    function LoadFileFromRemoteInt(const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
      ABaseThread: TJvBaseThread): string; override;
  public
    function LoadVersionInfoFromRemote(const ALocalDirectory, ALocalVersionInfoFileName: string;
      ABaseThread: TJvBaseThread): string; override;
  published
    property ServerName: string read FServerName write FServerName;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property SelectStatementVersion: string read FSelectStatementVersion write FSelectStatementVersion;
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
  end;

  { Location Type for the TJvProgramVersionCheck class}
  TJvProgramVersionLocationType = (pvltNetwork, pvltDatabase, pvltFTP, pvltHTTP);
  {Set of TJvProgramVersionLocationTypes}
  TJvProgramVersionLocationTypes = set of TJvProgramVersionLocationType;

  { Type for User Customizing options to the JvProgramVersionCheck
  The settings of the Programversioncheck are stored via JvAppStorage. With
  these types could be defined which settings are stored and restored and so
  customisable by the end user}
  TJvProgramVersionUserOption = (uoCheckFrequency, uoLocalDirectory,
    uoAllowedReleaseType, uoLocationType, uoLocationNetwork,
    uoLocationHTTP, uoLocationFTP, uoLocationDatabase);
  { set of TJvProgramVersionUserOption }
  TJvProgramVersionUserOptions = set of TJvProgramVersionUserOption;

  TjvProgramVersionHistoryFileFormat = (hffIni, hffXML);

  TJvProgramVersionHistoryAppStorageOptions = class(TPersistent)
  private
    FFileFormat: TjvProgramVersionHistoryFileFormat;
    FINIOptions: TJvAppIniStorageOptions;
    FXMLOptions: TJvAppXMLStorageOptions;
    procedure SetINIOptions(const Value: TJvAppIniStorageOptions);
    procedure SetXMLOptions(const Value: TJvAppXMLStorageOptions);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property FileFormat: TjvProgramVersionHistoryFileFormat read FFileFormat write FFileFormat default hffIni;
    property INIOptions: TJvAppIniStorageOptions read FINIOptions write SetINIOptions;
    property XMLOptions: TJvAppXMLStorageOptions read FXMLOptions write SetXMLOptions;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvProgramVersionCheck = class(TJvCustomPropertyStore)
  private
    FAllowedReleaseType: TJvProgramReleaseType;
    FCheckFrequency: Integer;
    FExecuteDownloadInstallFileName: string;
    FExecuteOperation: TJvRemoteVersionOperation;
    FExecuteVersionInfo: TJvProgramVersionInfo;
    FLastCheck: TDateTime;
    FLocalDirectory: string;
    FLocalInstallerFileName: string;
    FLocalVersionInfoFileName: string;
    FLocationDatabase: TJvProgramVersionDatabaseLocation;
    FLocationFTP: TJvProgramVersionFTPLocation;
    FLocationHTTP: TJvProgramVersionHTTPLocation;
    FLocationNetwork: TJvProgramVersionNetworkLocation;
    FLocationType: TJvProgramVersionLocationType;
    FRemoteProgramVersionHistory: TJvProgramVersionHistory;
    FThread: TJvThread;
    FThreadDialog: TJvThreadAnimateDialog;
    FThreadExceptionMessage : String;
    FThreadExceptionClass: ExceptClass;
    FThreadExceptionAddr: Pointer;
    FUserOptions: TJvProgramVersionUserOptions;
    FVersionHistoryFileOptions: TJvProgramVersionHistoryAppStorageOptions;
    function CreateVersionHistoryAppstorage(aFileFormat: TjvProgramVersionHistoryFileFormat): TJvCustomAppMemoryFileStorage;
    function GetDownloadError: string;
    function GetSelectedLocation: TJvCustomProgramVersionLocation;
    procedure SetLocationDatabase(const Value: TJvProgramVersionDatabaseLocation);
    procedure SetLocationFTP(const Value: TJvProgramVersionFTPLocation);
    procedure SetLocationHTTP(const Value: TJvProgramVersionHTTPLocation);
    procedure SetLocationNetwork(const Value: TJvProgramVersionNetworkLocation);
    procedure SetVersionHistoryFileOptions(const Value: TJvProgramVersionHistoryAppStorageOptions);
  protected
    procedure CheckLocalDirectory;
    function CurrentApplicationName: string;
    function CurrentFileVersion: string;
    procedure DownloadThreadOnException(Sender: TObject; E: Exception; EAddr: Pointer);
    procedure DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
    procedure DownloadThreadOnFinishAll(Sender: TObject);
    function GetAllowedRemoteProgramVersion: string;
    function GetAllowedRemoteProgramVersionReleaseType: string;
    function GetLocationTypesSupported: TJvProgramVersionLocationTypes;
    function IsRemoteProgramVersionReleaseTypeNewer(AReleaseType: TJvProgramReleaseType): Boolean;
    procedure LoadData; override;
    function LoadRemoteInstallerFile(const ALocalDirectory, ALocalInstallerFileName: string;
      AProgramVersionInfo: TJvProgramVersionInfo; ABaseThread: TJvBaseThread): string;
    function LoadRemoteVersionInfoFile(const ALocalDirectory: string; const ALocalVersionInfoFileName: string): string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetThreadInfo(const Info: string);
    procedure SetUserOptions(Value: TJvProgramVersionUserOptions);
    procedure StoreData; override;
    procedure VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DownloadInstallerFromRemote;
    procedure Execute;
    function GetRemoteVersionOperation(var ReleaseType: TJvProgramReleaseType): TJvRemoteVersionOperation;
    function IsRemoteProgramVersionNewer: Boolean;
    function LoadRemoteVersionHistoryFromFile(aFileFormat: TjvProgramVersionHistoryFileFormat; const aFileName: string): Boolean;
    procedure ShowProgramVersionsDescription(const AFromVersion, AToVersion: string);
    function StoreRemoteVersionHistoryToFile(aFileFormat: TjvProgramVersionHistoryFileFormat; const aFilename: string): Boolean;
    property DownloadError: string read GetDownloadError;
    property LastCheck: TDateTime read FLastCheck write FLastCheck;
    property LocationTypesSupported: TJvProgramVersionLocationTypes read GetLocationTypesSupported;
    property RemoteProgramVersionHistory: TJvProgramVersionHistory read FRemoteProgramVersionHistory write FRemoteProgramVersionHistory;
    property SelectedLocation: TJvCustomProgramVersionLocation read GetSelectedLocation;
    property Thread: TJvThread read FThread;
    property ThreadDialog: TJvThreadAnimateDialog read FThreadDialog;
  published
    { Defines which release types will be shown in the update dialog }
    property AllowedReleaseType: TJvProgramReleaseType read FAllowedReleaseType write FAllowedReleaseType default prtProduction;
    property AppStorage;
    property AppStoragePath;
    { Defines how often the check for a new version is executed (in days) }
    property CheckFrequency: Integer read FCheckFrequency write FCheckFrequency;
    { Defines the local directory where the remote files where stored }
    property LocalDirectory: string read FLocalDirectory write FLocalDirectory;
    { Defines the local name of the program installer. If it is empty the name
      of the remote file is used }
    property LocalInstallerFileName: string read FLocalInstallerFileName write FLocalInstallerFileName;
    { Defines the name of the local version info file. If it is empty the name
      of the remote file is used }
    property LocalVersionInfoFileName: string read FLocalVersionInfoFileName write FLocalVersionInfoFileName;
    { Database Location }
    property LocationDatabase: TJvProgramVersionDatabaseLocation read FLocationDatabase write SetLocationDatabase;
    { FTP Location }
    property LocationFTP: TJvProgramVersionFTPLocation read FLocationFTP write SetLocationFTP;
    { HTTP Location }
    property LocationHTTP: TJvProgramVersionHTTPLocation read FLocationHTTP write SetLocationHTTP;
    { Network Location }
    property LocationNetwork: TJvProgramVersionNetworkLocation read FLocationNetwork write SetLocationNetwork;
    { Defines location which is used for the version check,
    only assigned locations are supported }
    property LocationType: TJvProgramVersionLocationType read FLocationType write FLocationType;
    {Defines which options of the component are stored/restored via AppStorage }
    property UserOptions: TJvProgramVersionUserOptions read FUserOptions write SetUserOptions
      default [uoCheckFrequency, uoLocalDirectory, uoAllowedReleaseType,
      uoLocationType, uoLocationNetwork, uoLocationHTTP, uoLocationFTP, uoLocationDatabase];
    property VersionHistoryFileOptions: TJvProgramVersionHistoryAppStorageOptions
        read FVersionHistoryFileOptions write SetVersionHistoryFileOptions;
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
  Dialogs, Controls, ComCtrls, StdCtrls, Forms,
  {$IFDEF USE_3RDPARTY_INDY}
  IdURI,
  {$ENDIF USE_3RDPARTY_INDY}
  JclBase, JclFileUtils, JclShell,
  JvDSADialogs, JvParameterListParameter, JvResources, Windows, Messages, JvJVCLUtils;

const
  SParamNameVersionButtonInfo = 'VersionButtonInfo';
  SParamNameMemo = 'Memo';
  SParamNameNewVersionLabel = 'New Version Label';
  SParamNameGroupBox = 'GroupBox';
  SParamNameOperation = 'Operation';
  SParamNameRadioButton = 'RadioButton';
  SAppStorageDefaultSection = 'Version';
  SProgramVersion = 'Program Version ';
  SLastCheck = 'LastCheck';

//=== Common Functions =======================================================

function CompareVersionNumbers(AVersion1, AVersion2: string): Integer;
var
  N1, N2: Integer;

  function GetNextNumber(var Version: string): Integer;
  var
    P: Integer;
    S: string;
  begin
    P := Pos('.', Version);
    if P > 0 then
    begin
      S := Copy(Version, 1, P - 1);
      Version := Copy(Version, P + 1, Length(Version) - P);
    end
    else
    begin
      S := Version;
      Version := '';
    end;
    if S = '' then
      Result := -1
    else
    try
      Result := StrToInt(S);
    except
      Result := -1;
    end;
  end;

begin
  Result := 0;
  repeat
    N1 := GetNextNumber(AVersion1);
    N2 := GetNextNumber(AVersion2);
    if N2 > N1 then
    begin
      Result := 1;
      Exit;
    end
    else
    if N2 < N1 then
    begin
      Result := -1;
      Exit;
    end
  until (AVersion1 = '') and (AVersion2 = '');
end;

//=== { TJvProgramVersionsStringList } =======================================

function VersionNumberSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  Info1, Info2: TJvProgramVersionInfo;
begin
  Info1 := TJvProgramVersionInfo(List.Objects[Index1]);
  Info2 := TJvProgramVersionInfo(List.Objects[Index2]);
  Result := CompareVersionNumbers(Info1.ProgramVersion, Info2.ProgramVersion);
end;

procedure TJvProgramVersionsStringList.Sort;
begin
  CustomSort(VersionNumberSortCompare);
end;

function TJvProgramVersionCheck.CreateVersionHistoryAppstorage(aFileFormat: TjvProgramVersionHistoryFileFormat):
    TJvCustomAppMemoryFileStorage;
begin
  if aFileFormat = hffIni then
  begin
    Result := TJvCustomAppMemoryFileStorage(TJvAppIniFileStorage.Create(Self));
    TJvAppIniFileStorage(Result).DefaultSection := SAppStorageDefaultSection;
    Result.StorageOptions.Assign(VersionHistoryFileOptions.INIOptions);
  end
  else
  begin
    Result := TJvCustomAppMemoryFileStorage(TJvAppXMLFileStorage.Create(Self));
    Result.StorageOptions.Assign(VersionHistoryFileOptions.XMLOptions);
  end;
  Result.Location := flCustom;
  Result.ReadOnly := True;
  Result.AutoReload := True;
end;

//=== { TJvProgramVersionInfo } ==============================================

constructor TJvProgramVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionDescription := TStringList.Create;
  IgnoreLastLoadTime := True;
  FDownloadPasswordRequired := False;
end;

destructor TJvProgramVersionInfo.Destroy;
begin
  FreeAndNil(FVersionDescription);
  inherited Destroy;
end;

procedure TJvProgramVersionInfo.Assign(Source: TPersistent);
begin
  if (Source = Self) then
    Exit;
  if Source is TJvProgramVersionInfo then
  begin
    DownloadPasswordRequired := TJvProgramVersionInfo (Source).DownloadPasswordRequired;
    LocalInstallerParams := TJvProgramVersionInfo (Source).LocalInstallerParams;
    VersionDescription := TJvProgramVersionInfo (Source).VersionDescription;
    ProgramSize := TJvProgramVersionInfo (Source).ProgramSize;
    ProgramVersion := TJvProgramVersionInfo (Source).ProgramVersion;
    ProgramLocationPath := TJvProgramVersionInfo (Source).ProgramLocationPath;
    ProgramLocationFileName := TJvProgramVersionInfo (Source).ProgramLocationFileName;
    ProgramReleaseType := TJvProgramVersionInfo (Source).ProgramReleaseType;
    ProgramReleaseDate := TJvProgramVersionInfo (Source).ProgramReleaseDate;
  end
  else
    inherited assign(Source);
end;

function TJvProgramVersionInfo.GetVersionDescription: TStrings;
begin
  Result := FVersionDescription;
end;

procedure TJvProgramVersionInfo.SetVersionDescription(Value: TStrings);
begin
  FVersionDescription.Assign(Value);
end;

procedure TJvProgramVersionInfo.Clear;
begin
  if Assigned(FVersionDescription) then
    FVersionDescription.Clear;
  FProgramVersion := '';
  FProgramReleaseType := prtProduction;
end;

function TJvProgramVersionInfo.EditIntf_GetObjectHint: string;
begin
  Result := RSProgramVersionInfo_ObjectHint;
end;

function TJvProgramVersionInfo.EditIntf_GetPropertyHint(const PropertyName:
    string): string;
begin
  Result := '';
  if PropertyName = 'DownloadPasswordRequired' then
    Result := RSProgramVersionInfo_PropertyHint_DownloadPassword
  else if PropertyName = 'LocalInstallerParams' then
    Result := RSProgramVersionInfo_PropertyHint_LocalInstallerParams
  else if PropertyName = 'ProgramLocationPath' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramLocationPath
  else if PropertyName = 'ProgramLocationFileName' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramLocationFileName
  else if PropertyName = 'ProgramVersion' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramVersion
  else if PropertyName = 'VersionDescription' then
    Result := RSProgramVersionInfo_PropertyHint_VersionDescription
  else if PropertyName = 'ProgramReleaseType' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramReleaseType
  else if PropertyName = 'ProgramSize' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramSize
  else if PropertyName = 'ProgramReleaseDate' then
    Result := RSProgramVersionInfo_PropertyHint_ProgramReleaseDate
  else
    Result := '';
end;

function TJvProgramVersionInfo.EditIntf_GetVisibleObjectName: string;
begin
  Result := ProgramVersionInfo;
end;

function TJvProgramVersionInfo.ProgramVersionReleaseType: string;
begin
  case ProgramReleaseType of
    prtBeta:
      Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeBeta);
    prtAlpha:
      Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeAlpha);
  else
    Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeProduction);
  end;
end;

function TJvProgramVersionInfo.ProgramSizeString: string;
begin
  if ProgramSize <= 0 then
    Result := ''
  else
  if ProgramSize >= 1024 * 1024 * 1024 then
    Result := Format(RsPVSiceGB, [ProgramSize / 1024 / 1024 / 1024])
  else
  if ProgramSize >= 1024 * 1024 then
    Result := Format(RsPVSiceMB, [ProgramSize / 1024 / 1024])
  else
  if ProgramSize >= 1024 then
    Result := Format(RsPVSiceKB, [ProgramSize / 1024])
  else
    Result := Format(RsPVSiceB, [ProgramSize])
end;

function TJvProgramVersionInfo.ProgramVersionInfo: string;
begin
  Result := ProgramVersionReleaseType;
  if ProgramSize > 0 then
    Result := Result + ' (' + ProgramSizeString + ')';
end;

procedure TJvProgramVersionInfo.StoreXMLProperties;
begin
  AppStorage.SetXMLProperty(AppStoragePath, 'Version', ProgramVersionReleaseType);
  Inherited;
end;

//=== { TJvProgramVersionHistory } ===========================================

constructor TJvProgramVersionHistory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DeleteBeforeStore := True;
  ItemName := SProgramVersion;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('Duplicates');
  IgnoreProperties.Add('Sorted');
end;

procedure TJvProgramVersionHistory.RecalculateCurrentProgramVersions;
var
  I: TJvProgramReleaseType;
begin
  Items.Sort;
  for I := Low(TJvProgramReleaseType) to High(TJvProgramReleaseType) do
    FCurrentProgramVersion[I] := SearchCurrentProgramVersion(I);
end;

procedure TJvProgramVersionHistory.LoadData;
begin
  inherited LoadData;
  RecalculateCurrentProgramVersions;
end;

function TJvProgramVersionHistory.AllowedCurrentProgramVersion(
  AAllowedReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
var
  I: TJvProgramReleaseType;
begin
  Result := nil;
  I := Low(TJvProgramReleaseType);
  while I <= AAllowedReleaseType do
  begin
    if Result = nil then
      Result := CurrentProgramVersion[I]
    else
    if Assigned(CurrentProgramVersion[I]) and
      (CompareVersionNumbers(Result.ProgramVersion, CurrentProgramVersion[I].ProgramVersion) > 0) then
      Result := CurrentProgramVersion[I];
    Inc(I);
  end;
end;

procedure TJvProgramVersionHistory.Assign(Source: TPersistent);
var
  i: Integer;
  VersionInfo: TJvProgramVersionInfo;
begin
  if (Source = Self) then
    Exit;
  if Source is TJvProgramVersionHistory then
  begin
    clear;
    CurrentBetaProgramVersion := TJvProgramVersionHistory(Source).CurrentBetaProgramVersion;
    CurrentAlphaProgramVersion := TJvProgramVersionHistory(Source).CurrentAlphaProgramVersion;
    for i := 0 to TJvProgramVersionHistory(Source).count - 1 do
    begin
      VersionInfo := TJvProgramVersionInfo(TJvProgramVersionInfo(TJvProgramVersionHistory(Source).Items.Objects[i]).Clone(Owner));
      Items.AddObject(VersionInfo.ProgramVersion, VersionInfo);
    end;
    Items.Sort;
  end
  else
    inherited assign(Source);
end;

function TJvProgramVersionHistory.GetProgramVersion(Index: Integer): TJvProgramVersionInfo;
begin
  if Assigned(Objects[Index]) and (Objects[Index] is TJvProgramVersionInfo) then
    Result := TJvProgramVersionInfo(Objects[Index])
  else
    Result := nil;
end;

function TJvProgramVersionHistory.SearchCurrentProgramVersion(
  AProgramReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Assigned(ProgramVersion[I]) then
      if ProgramVersion[I].ProgramReleaseType = AProgramReleaseType then
        if Result = nil then
          Result := ProgramVersion[I]
        else
        if CompareVersionNumbers(Result.ProgramVersion, ProgramVersion[I].ProgramVersion) = 1 then
          Result := ProgramVersion[I];
end;

function TJvProgramVersionHistory.GetCurrentProgramVersion(Index: TJvProgramReleaseType): TJvProgramVersionInfo;
begin
  Result := FCurrentProgramVersion[Index];
end;

function TJvProgramVersionHistory.CreateObject: TPersistent;
begin
  Result := TJvProgramVersionInfo.Create(Self);
end;

function TJvProgramVersionHistory.CreateItemList: TStringList;
begin
  Result := TJvProgramVersionsStringList.Create;
end;

function TJvProgramVersionHistory.EditIntf_GetObjectHint: string;
begin
  Result := RSProgramVersionHistory_ObjectHint;
end;

function TJvProgramVersionHistory.EditIntf_GetPropertyHint(const PropertyName:
    string): string;
begin
  Result := '';
  if PropertyName = 'CurrentProductionProgramVersion' then
    Result := RSProgramVersionHistory_PropertyHint_Production
  else if PropertyName = 'CurrentBetaProgramVersion' then
    Result := RSProgramVersionHistory_PropertyHint_beta
  else if PropertyName = 'CurrentAlphaProgramVersion' then
    Result := RSProgramVersionHistory_PropertyHint_alpha
  else
    Result := Inherited EditIntf_GetPropertyHint(PropertyName);
end;

function TJvProgramVersionHistory.EditIntf_GetVisibleObjectName: string;
begin
  Result := RSProgramVersionHistory;
end;

function TJvProgramVersionHistory.GetCurrentProductionProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtProduction]) then
    Result := CurrentProgramVersion[prtProduction].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentBetaProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtBeta]) then
    Result := CurrentProgramVersion[prtBeta].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentAlphaProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtAlpha]) then
    Result := CurrentProgramVersion[prtAlpha].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetVersionsDescription(const AFromVersion, AToVersion: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if (CompareVersionNumbers(AFromVersion, ProgramVersion[I].ProgramVersion) >= 0) and
      (CompareVersionNumbers(AToVersion, ProgramVersion[I].ProgramVersion) <= 0) then
    begin
      Result := Result + ProgramVersion[I].ProgramVersionReleaseType;
      if ProgramVersion[I].ProgramReleaseDate > 0 then
        Result := Result + ' - ' + DateTimeToStr(ProgramVersion[I].ProgramReleaseDate);
      if ProgramVersion[I].VersionDescription.Count > 0 then
        Result := Result + NativeLineBreak + ProgramVersion[I].VersionDescription.Text;
      Result := Result + NativeLineBreak + NativeLineBreak;
    end;
end;

//=== { TJvProgramVersionCustomLocation } ====================================

constructor TJvCustomProgramVersionLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownloadThreaded := False;
  FDownloadStatus := '';
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('DownloadThreaded');
end;

function TJvCustomProgramVersionLocation.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
end;

function TJvCustomProgramVersionLocation.LoadFileFromRemote(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  TemporaryLocalFileName: string;
  LocalFileName: string;
  TemporaryLocalFileName2: string;
begin
  DownloadStatus := RsPVCDownloading;
  DownloadError := '';
  if ALocalFileName = '' then
    LocalFileName := ARemoteFileName
  else
    LocalFileName := ALocalFileName;
  TemporaryLocalFileName := LocalFileName + RsPVTempFileNameExtension;
  if FileExists(PathAppend(ALocalPath, TemporaryLocalFileName)) then
    SysUtils.DeleteFile(PathAppend(ALocalPath, TemporaryLocalFileName));
  Result := LoadFileFromRemoteInt(ARemotePath, ARemoteFileName,
    ALocalPath, TemporaryLocalFileName, ABaseThread);
  if FileExists(Result) then // if we successfully copied the remote file to <local>.temp
  begin
    if FileExists(PathAppend(ALocalPath, LocalFileName)) then
    begin // if <local> exists
      if SysUtils.DeleteFile(PathAppend(ALocalPath, LocalFileName)) then
      begin // if we deleted <local>
        if RenameFile(Result, PathAppend(ALocalPath, LocalFileName)) then // if we renamed <local>.temp to <local>
          Result := PathAppend(ALocalPath, LocalFileName) // we can return <local>
        else
          Result := ''; // failed rename, return blank
      end // try to delete <local>
      else
      begin // so, we failed delete <local>  try renaming <local>
        TemporaryLocalFileName2 := LocalFileName + RsPVTempFileNameExtension + '.bak'; // <local>.temp.bak
        if FileExists(PathAppend(ALocalPath, TemporaryLocalFileName2)) then // if <local>.temp.bak exists
          SysUtils.DeleteFile(PathAppend(ALocalPath, TemporaryLocalFileName2)); // get rid of it

        // rename <local> to <local>.temp.bak  (you can't delete live file, but CAN rename in recent Win OS)
        if RenameFile(PathAppend(ALocalPath, LocalFileName), PathAppend(ALocalPath, TemporaryLocalFileName2)) then
        begin
          // try to rename <local>.temp to <local>
          if RenameFile(Result, PathAppend(ALocalPath, LocalFileName)) then
          begin
            // if rename <local>.temp to <local> successful set <local> as return
            Result := PathAppend(ALocalPath, LocalFileName);
            // delete <local>.temp.bak  if possible - we don't care if fails -
            // leaving a backup of live exe is a "feature" anyway <g>
            if FileExists(PathAppend(ALocalPath, TemporaryLocalFileName2)) then
              SysUtils.DeleteFile(PathAppend(ALocalPath, TemporaryLocalFileName2));
          end // rename <local>.temp to <local>
          else
            Result := ''; // rename of <local>.temp to <local> failed, return blank
        end // try to rename <local> to <local>.temp.bak
        else
          Result := ''; // rename of <local> to <local>.temp.bak failed, return blank
      end; // try renaming <local>
    end // <local> file exists
    else
    begin
      // if <local> doesn't exist, just rename <local>.temp to <local>
      if RenameFile(Result, PathAppend(ALocalPath, LocalFileName)) then
        Result := PathAppend(ALocalPath, LocalFileName)
      else
        Result := ''; // if rename failed return blank...
    end; // <local> doesn't exist
  end // <we successfully copied the remote file to <local>.temp
  else
    Result := ''; // if we did not copy remote file to <local>.tempreturn blank
end;

function TJvCustomProgramVersionLocation.LoadInstallerFileFromRemote(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(ARemotePath, ARemoteFileName,
    ALocalPath, ALocalFileName, ABaseThread);
end;

function TJvCustomProgramVersionLocation.LoadVersionInfoFromRemote(
  const ALocalDirectory, ALocalVersionInfoFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
end;

procedure TJvCustomProgramVersionLocation.SetDownloadError(const Value: string);
begin
  FDownloadError := Value;
end;

//=== { TJvProgramVersionCustomFileBasedLocation } ===========================

constructor TJvCustomProgramVersionFileBasedLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionInfoLocationPathList := TStringList.Create;
end;

destructor TJvCustomProgramVersionFileBasedLocation.Destroy;
begin
  FreeAndNil(FVersionInfoLocationPathList);
  inherited Destroy;
end;

function TJvCustomProgramVersionFileBasedLocation.GetVersionInfoLocationPathList: TStrings;
begin
  Result := FVersionInfoLocationPathList;
end;

procedure TJvCustomProgramVersionFileBasedLocation.SetVersionInfoLocationPathList(Value: TStrings);
begin
  FVersionInfoLocationPathList.Assign(Value);
end;

function TJvCustomProgramVersionFileBasedLocation.LoadVersionInfoFromRemote(
  const ALocalDirectory, ALocalVersionInfoFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  I: Integer;
begin
  for I := 0 to VersionInfoLocationPathList.Count - 1 do
  begin
    Result := LoadFileFromRemote(VersionInfoLocationPathList[I], VersionInfoFileName,
      ALocalDirectory, ALocalVersionInfoFileName, ABaseThread);
    if Result <> '' then
    begin
      FValidLocationPath := VersionInfoLocationPathList[I];
      Exit;
    end;
  end;
  if Result = '' then
  begin
    Result := LoadFileFromRemote('', VersionInfoFileName,
      ALocalDirectory, ALocalVersionInfoFileName, ABaseThread);
    if Result <> '' then
      FValidLocationPath := '';
  end;
end;

function TJvCustomProgramVersionFileBasedLocation.LoadInstallerFileFromRemote(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(ARemotePath, ARemoteFileName,
    ALocalPath, ALocalFileName, ABaseThread);
  if Result = '' then
    Result := LoadFileFromRemote(ValidLocationPath + ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName, ABaseThread);
end;

//=== { TJvProgramVersionNetworkLocation } ===================================

function TJvProgramVersionNetworkLocation.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;

  function FileExistsNoDir(AFileName: string): Boolean;
  begin
    Result := FileExists(AFileName) and not DirectoryExists(AFileName);
  end;

begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) and
    (DirectoryExists(ARemotePath) or (ARemotePath = '')) then
    if FileExistsNoDir(PathAppend(ARemotePath, ARemoteFileName)) then
      if (ARemotePath = ALocalPath) and (ARemoteFileName = ALocalFileName) then
        Result := PathAppend(ARemotePath, ARemoteFileName)
      else
      if FileCopy(PathAppend(ARemotePath, ARemoteFileName), PathAppend(ALocalPath, ALocalFileName), True) then
        if FileExistsNoDir(PathAppend(ALocalPath, ALocalFileName)) then
          Result := PathAppend(ALocalPath, ALocalFileName)
        else
        if FileExistsNoDir(PathAppend(ALocalPath, ARemoteFileName)) then
          Result := PathAppend(ALocalPath, ARemoteFileName)
        else
        if FileExistsNoDir(PathAppend(ALocalPath, ExtractFileName(ARemotePath))) then
          Result := PathAppend(ALocalPath, ExtractFileName(ARemotePath));
end;

//=== { TJvProgramVersionInternetLocation } ==================================

constructor TJvCustomProgramVersionInternetLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProxySettings := TJvProgramVersionProxySettings.Create;
  FPasswordRequired := False;
  FPort := 80;
end;

destructor TJvCustomProgramVersionInternetLocation.Destroy;
begin
  FreeAndNil(FProxySettings);
  inherited Destroy;
end;

//=== { TJvProgramVersionHTTPLocation } ======================================

function TJvProgramVersionHTTPLocation.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName);
end;

//=== { TJvProgramVersionFTPLocation } =======================================

function TJvProgramVersionFTPLocation.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName);
end;

//=== { TJvProgramVersionDatabaseLocation } ==================================

function TJvProgramVersionDatabaseLocation.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName);
end;

function TJvProgramVersionDatabaseLocation.LoadVersionInfoFromRemote(
  const ALocalDirectory, ALocalVersionInfoFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(SelectStatementVersion, '', ALocalDirectory,
    ALocalVersionInfoFileName, ABaseThread);
end;

//=== { TJvProgramVersionCheck } =============================================

constructor TJvProgramVersionCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemoteProgramVersionHistory := TJvProgramVersionHistory.Create(Self);
  FRemoteProgramVersionHistory.IgnoreLastLoadTime := True;
  FVersionHistoryFileOptions := TJvProgramVersionHistoryAppStorageOptions.Create;
  FThread := TJvThread.Create(Self);
  FThread.ThreadName := Format('%s: %s',[ClassName, Name]);
  FThread.Exclusive := True;
  FThread.RunOnCreate := True;
  FThread.FreeOnTerminate := True;
  FThreadDialog := TJvThreadAnimateDialog.Create(Self);
  FThreadDialog.DialogOptions.ShowDialog := True;
  FThreadDialog.DialogOptions.ShowCancelButton := True;
  FThreadDialog.DialogOptions.ShowElapsedTime := True;
  TJvThreadAnimateDialogOptions(FThreadDialog.DialogOptions).CommonAvi := aviCopyFile;
  FThread.ThreadDialog := FThreadDialog;

  DeleteBeforeStore := True;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('LocalInstallerFileName');
  IgnoreProperties.Add('LocalVersionInfoFileName');
  IgnoreProperties.Add('VersionHistoryAppStorage');
  IgnoreProperties.Add('UserOptions');
  IgnoreProperties.Add('VersionHistoryFileOptions');

  FUserOptions := [uoCheckFrequency, uoLocalDirectory,
    uoAllowedReleaseType, uoLocationType, uoLocationNetwork,
    uoLocationHTTP, uoLocationFTP, uoLocationDatabase];

  FAllowedReleaseType := prtProduction;
  FLocalInstallerFileName := '';
  FLocalVersionInfoFileName := RsPVDefaultVersioninfoFileName;
  FLocationType := pvltNetWork;
end;

destructor TJvProgramVersionCheck.Destroy;
begin
  FreeAndNil(FVersionHistoryFileOptions);
  FreeAndNil(FRemoteProgramVersionHistory);
  FreeAndNil(FThreadDialog);
  FreeAndNil(FThread);
  inherited Destroy;
end;

procedure TJvProgramVersionCheck.CheckLocalDirectory;
begin
  LocalDirectory := Trim(LocalDirectory);
  if LocalDirectory <> '' then
    if not DirectoryExists(LocalDirectory) then
      if not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
end;

function TJvProgramVersionCheck.CurrentApplicationName: string;
var
  FileVersionInfo: TJclFileVersionInfo;
begin
  FileVersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
  try
    try
      Result := FileVersionInfo.ProductName;
    except
      Result := '';
    end;
    if Result = '' then
      Result := PathExtractFileNameNoExt(ParamStr(0));
  finally
    FileVersionInfo.Free;
  end;
end;

function TJvProgramVersionCheck.CurrentFileVersion: string;
var
  FileVersionInfo: TJclFileVersionInfo;
begin
  FileVersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
  try
    try
      Result := FileVersionInfo.FileVersion;
    except
      Result := '';
    end;
  finally
    FileVersionInfo.Free;
  end;
end;

procedure TJvProgramVersionCheck.DownloadInstallerFromRemote;
begin
  if Assigned(FExecuteVersionInfo) then
  begin
    FThreadExceptionClass := niL;
    FThreadExceptionAddr := nil;
    FThread.OnException := DownloadThreadOnException;
    FThread.OnExecute := DownloadThreadOnExecute;
    FThread.OnFinishAll := DownloadThreadOnFinishAll;
    FThread.ExecuteAndWait(self);
    if Assigned(FThreadExceptionClass) then
      raise FThreadExceptionClass.Create(FThreadExceptionMessage);
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnException(Sender: TObject; E: Exception; EAddr: Pointer);
begin
  FThreadExceptionClass := ExceptClass(E.ClassType);
  FThreadExceptionMessage := E.Message;
  FThreadExceptionAddr := EAddr;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
begin
  if Assigned(FExecuteVersionInfo) then
  begin
    FExecuteDownloadInstallFileName :=
      LoadRemoteInstallerFile(LocalDirectory, LocalInstallerFileName,
      FExecuteVersionInfo, FThread.LastThread);
    if (FExecuteDownloadInstallFileName <> '') and
      not FileExists(FExecuteDownloadInstallFileName) then
      FExecuteDownloadInstallFileName := '';
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnFinishAll(Sender: TObject);
begin
  Application.ProcessMessages;
  if Assigned(FThreadExceptionClass) then
    exit;
  if DownloadError <> '' then
    JvDSADialogs.MessageDlg(DownloadError, mtError, [mbOK], 0)
  else
  if FExecuteDownloadInstallFileName = '' then
    JvDSADialogs.MessageDlg(RsPVCFileDownloadNotSuccessful, mtError, [mbOK], 0)
  else
  if FExecuteOperation = rvoCopy then
    JvDSADialogs.MessageDlg(Format(RsPVCDownloadSuccessfulInstallManually,
      [FExecuteDownloadInstallFileName]), mtInformation, [mbOK], 0)
  else
  if JvDSADialogs.MessageDlg(RsPVCDownloadSuccessfullInstallNow,
    mtWarning, [mbYes, mbNo], 0) = mrYes then
    if ShellExecEx(FExecuteDownloadInstallFileName, FExecuteVersionInfo.LocalInstallerParams) then
      PostMessage(Application.Handle, WM_CLOSE, 0, 0)
    else
      JvDSADialogs.MessageDlg(RsPVCErrorStartingSetup, mtError, [mbOK], 0);
end;

procedure TJvProgramVersionCheck.Execute;
var
  ReleaseType: TJvProgramReleaseType;
begin
  FExecuteVersionInfo := nil;
  if Assigned(Appstorage) and not Appstorage.PathExists(AppStoragePath) then
    StoreProperties;
  LoadProperties;
  if (LastCheck < Now - CheckFrequency) and (LocationTypesSupported <> []) then
  begin
    LastCheck := Now;
    if not DirectoryExists(LocalDirectory) then
      if (LocalDirectory <> '') and not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
    if LoadRemoteVersionHistoryFromFile(VersionHistoryFileOptions.FileFormat,
             LoadRemoteVersionInfoFile(LocalDirectory, LocalVersionInfoFileName)) then
    begin
      StoreProperties;
      StoreRemoteVersionHistoryToFile (VersionHistoryFileOptions.FileFormat,
          LoadRemoteVersionInfoFile(LocalDirectory, LocalVersionInfoFileName));
      if IsRemoteProgramVersionNewer then
      begin
        FExecuteOperation := GetRemoteVersionOperation(ReleaseType);
        FExecuteVersionInfo :=
          RemoteProgramVersionHistory.CurrentProgramVersion[ReleaseType];
        if FExecuteOperation in [rvoCopy, rvoCopyInstall] then
          DownloadInstallerFromRemote;
      end;
    end;
  end;
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersion: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersionReleaseType: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersionReleaseType
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetDownloadError: string;
begin
  if Assigned(SelectedLocation) then
    Result := SelectedLocation.DownloadError
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetLocationTypesSupported: TJvProgramVersionLocationTypes;
begin
  Result := [];
  if Assigned(FLocationNetwork) then
    Result := Result + [pvltNetWork];
  if Assigned(FLocationDatabase) then
    Result := Result + [pvltDatabase];
  if Assigned(FLocationHTTP) then
    Result := Result + [pvltHTTP];
  if Assigned(FLocationFTP) then
    Result := Result + [pvltFTP];
end;

function TJvProgramVersionCheck.GetRemoteVersionOperation(
  var ReleaseType: TJvProgramReleaseType): TJvRemoteVersionOperation;
var
  ParameterList: TJvParameterList;
  GroupParameter: TJvGroupBoxParameter;
  Parameter: TJvBaseParameter;
  I: TJvProgramReleaseType;
begin
  Result := rvoIgnore;
  ParameterList := TJvParameterList.Create(Self);
  try
    ParameterList.MaxWidth := 460;
    ParameterList.Messages.Caption :=
      Format(RsPVCDialogCaption, [CurrentApplicationName]);
    ParameterList.Messages.OkButton := RsPVCDialogExecuteButton;

    Parameter := TJvBaseParameter(TJvLabelParameter.Create(ParameterList));
    Parameter.SearchName := SParamNameNewVersionLabel;
    Parameter.Caption := Format(RsPVCNewVersionAvailable,
      [GetAllowedRemoteProgramVersionReleaseType, CurrentApplicationName]);
    Parameter.Width := 350;
    Parameter.Height := 45;
    ParameterList.AddParameter(Parameter);

    GroupParameter := TJvGroupBoxParameter.Create(ParameterList);
    GroupParameter.SearchName := SParamNameGroupBox;
    GroupParameter.Caption := RsPVCChooseWhichVersion;
    GroupParameter.Width := 350;
    GroupParameter.Height := 10;
    ParameterList.AddParameter(GroupParameter);

    for I := High(I) downto Low(I) do
      if (I <= AllowedReleaseType) and
        Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[I]) then
        if CompareVersionNumbers(CurrentFileVersion,
          RemoteProgramVersionHistory.CurrentProgramVersion[I].ProgramVersion) > 0 then
        begin
          Parameter := TJvBaseParameter(TJvRadioButtonParameter.Create(ParameterList));
          Parameter.ParentParameterName := SParamNameGroupBox;
          Parameter.SearchName := SParamNameRadioButton + IntToStr(Ord(I));
          Parameter.Caption := RemoteProgramVersionHistory.CurrentProgramVersion[I].ProgramVersionInfo;
          Parameter.Width := 250;
          Parameter.AsBoolean := GroupParameter.Height <= 10;
          ParameterList.AddParameter(Parameter);

          Parameter := TJvBaseParameter(TJvButtonParameter.Create(ParameterList));
          Parameter.ParentParameterName := SParamNameGroupBox;
          Parameter.SearchName := SParamNameVersionButtonInfo + IntToStr(Ord(I));
          Parameter.Caption := RsPVInfoButtonCaption;
          Parameter.Width := 80;
          Parameter.Tag := Ord(I);
          TJvButtonParameter(Parameter).OnClick := VersionInfoButtonClick;
          ParameterList.AddParameter(Parameter);

          GroupParameter.Height := GroupParameter.Height + 25;
        end;
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    Parameter.SearchName := SParamNameOperation;
    Parameter.Caption := RsPVCChooseOperation;
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsPVCOperationIgnore);
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsPVCOperationDownloadOnly);
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsPVCOperationDownloadInstall);
    TJvRadioGroupParameter(Parameter).ItemIndex := 2;
    Parameter.Width := 350;
    Parameter.Height := 79;
    ParameterList.AddParameter(Parameter);

    if ParameterList.ShowParameterDialog then
    begin
      case TJvRadioGroupParameter(ParameterList.ParameterByName(SParamNameOperation)).ItemIndex of
        0:
          Result := rvoIgnore;
        1:
          Result := rvoCopy;
        2:
          Result := rvoCopyInstall;
      end;
      ReleaseType := prtProduction;
      for I := High(I) downto Low(I) do
        if IsRemoteProgramVersionReleaseTypeNewer(I) then
        begin
          Parameter := ParameterList.ParameterByName(SParamNameRadioButton + IntToStr(Ord(I)));
          if Assigned(Parameter) then
            if Parameter.AsBoolean then
            begin
              ReleaseType := I;
              Break;
            end;
        end;
    end;
  finally
    ParameterList.Free;
  end;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionNewer: Boolean;
begin
  Result := CompareVersionNumbers(CurrentFileVersion, GetAllowedRemoteProgramVersion) = 1;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionReleaseTypeNewer(AReleaseType: TJvProgramReleaseType): Boolean;
begin
  if Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[AReleaseType]) then
    Result := CompareVersionNumbers(CurrentFileVersion,
      RemoteProgramVersionHistory.CurrentProgramVersion[AReleaseType].ProgramVersion) = 1
  else
    Result := False;
end;

procedure TJvProgramVersionCheck.LoadData;
begin
  inherited LoadData;
  try
    LastCheck := AppStorage.ReadDateTime(AppStorage.ConcatPaths([AppStoragePath, SLastCheck]), LastCheck);
  except
    on e:EConvertError do
      LastCheck := 0;
  end;
end;

function TJvProgramVersionCheck.LoadRemoteInstallerFile(const ALocalDirectory, ALocalInstallerFileName: string;
  AProgramVersionInfo: TJvProgramVersionInfo; ABaseThread: TJvBaseThread): string;
begin
  if Assigned(AProgramVersionInfo) and (SelectedLocation <> nil) then
  begin
    Result := SelectedLocation.LoadInstallerFileFromRemote(AProgramVersionInfo.ProgramLocationPath,
      AProgramVersionInfo.ProgramLocationFileName, ALocalDirectory, ALocalInstallerFileName, ABaseThread);
  end
  else
    Result := '';
end;

function TJvProgramVersionCheck.LoadRemoteVersionInfoFile(const ALocalDirectory: string;
  const ALocalVersionInfoFileName: string): string;
begin
  if SelectedLocation <> nil then
    Result := SelectedLocation.LoadVersionInfoFromRemote(ALocalDirectory, ALocalVersionInfoFileName, nil)
  else
    Result := '';
end;

procedure TJvProgramVersionCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if AComponent = FLocationNetwork then
      FLocationNetwork := nil
    else
    if AComponent = FLocationDatabase then
      FLocationDatabase := nil
    else
    if AComponent = FLocationHTTP then
      FLocationHTTP := nil
    else
    if AComponent = FLocationFTP then
      FLocationFTP := nil
end;

function TJvProgramVersionCheck.GetSelectedLocation: TJvCustomProgramVersionLocation;
begin
  case LocationType of
    pvltDatabase:
      Result := LocationDatabase;
    pvltHTTP:
      Result := LocationHTTP;
    pvltFTP:
      Result := LocationFTP;
    pvltNetwork:
      Result := LocationNetwork;
  else
    Result := nil;
  end
end;

procedure TJvProgramVersionCheck.SetThreadInfo(const Info: string);
begin
  if Assigned(FThreadDialog) then
    FThreadDialog.DialogOptions.InfoText := Info;
end;

procedure TJvProgramVersionCheck.SetUserOptions(Value: TJvProgramVersionUserOptions);
begin
  FUserOptions := Value;
  IgnoreProperties.AddDelete('CheckFrequency', (uoCheckFrequency in Value));
  IgnoreProperties.AddDelete('LocalDirectory', (uoLocalDirectory in Value));
  IgnoreProperties.AddDelete('AllowedReleaseType', (uoAllowedReleaseType in Value));
  IgnoreProperties.AddDelete('LocationType', (uoLocationType in Value));
  IgnoreProperties.AddDelete('LocationNetwork', (uoLocationNetwork in Value));
  IgnoreProperties.AddDelete('LocationHTTP', (uoLocationHTTP in Value));
  IgnoreProperties.AddDelete('LocationFTP', (uoLocationFTP in Value));
  IgnoreProperties.AddDelete('LocationDatabase', (uoLocationDatabase in Value));
end;

procedure TJvProgramVersionCheck.SetVersionHistoryFileOptions(const Value: TJvProgramVersionHistoryAppStorageOptions);
begin
  FVersionHistoryFileOptions.Assign(Value);
end;

procedure TJvProgramVersionCheck.ShowProgramVersionsDescription(const AFromVersion, AToVersion: string);
var
  ParameterList: TJvParameterList;
  Parameter: TJvMemoParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  try
    ParameterList.Messages.Caption := Format(RsPVCWhatNewInS, [CurrentApplicationName]);
    ParameterList.CancelButtonVisible := False;
    Parameter := TJvMemoParameter.Create(ParameterList);
    Parameter.SearchName := SParamNameMemo;
    Parameter.Caption := Format(RsPVCChangesBetween, [AFromVersion, AToVersion]);
    Parameter.Width := 340;
    Parameter.Height := 200;
    Parameter.AsString := RemoteProgramVersionHistory.GetVersionsDescription(AFromVersion, AToVersion);
    Parameter.Scrollbars := ssBoth;
    Parameter.ReadOnly := True;
    ParameterList.AddParameter(Parameter);
    ParameterList.ShowParameterDialog
  finally
    ParameterList.Free;
  end;
end;

procedure TJvProgramVersionCheck.StoreData;
begin
  inherited StoreData;
  AppStorage.WriteDateTime(AppStorage.ConcatPaths([AppStoragePath, SLastCheck]), LastCheck);
end;

function TJvProgramVersionCheck.StoreRemoteVersionHistoryToFile(aFileFormat: TjvProgramVersionHistoryFileFormat; const
    aFilename: string): Boolean;
var
  VersionHistoryAppStorage: TJvCustomAppMemoryFileStorage;
begin
  Result := false;
  VersionHistoryAppStorage := CreateVersionHistoryAppstorage (aFileFormat);
  try
    RemoteProgramVersionHistory.AppStorage := VersionHistoryAppStorage;
    VersionHistoryAppStorage.FileName := aFileName;
    if VersionHistoryAppStorage.FileName <> '' then
    begin
      VersionHistoryAppStorage.ReadOnly := False;
      RemoteProgramVersionHistory.StoreProperties;
      VersionHistoryAppStorage.Flush;
    end;
  finally
    RemoteProgramVersionHistory.AppStorage := nil;
    VersionHistoryAppStorage.Free;
  end;
end;

function TJvProgramVersionCheck.LoadRemoteVersionHistoryFromFile(aFileFormat: TjvProgramVersionHistoryFileFormat; const
    aFileName: string): Boolean;
var
  VersionHistoryAppStorage: TJvCustomAppMemoryFileStorage;
begin
  VersionHistoryAppStorage := CreateVersionHistoryAppstorage (aFileFormat);
  try
    RemoteProgramVersionHistory.AppStorage := VersionHistoryAppStorage;
    VersionHistoryAppStorage.FileName := aFileName;
    if VersionHistoryAppStorage.FileName <> '' then
    begin
      RemoteProgramVersionHistory.LoadProperties;
      Result := true;
    end
    else
      Result := false;
  finally
    RemoteProgramVersionHistory.AppStorage := nil;
    VersionHistoryAppStorage.Free;
  end;
end;

procedure TJvProgramVersionCheck.SetLocationDatabase(const Value: TJvProgramVersionDatabaseLocation);
begin
  ReplaceComponentReference(Self, Value, TComponent(FLocationDatabase));
end;

procedure TJvProgramVersionCheck.SetLocationFTP(const Value: TJvProgramVersionFTPLocation);
begin
  ReplaceComponentReference(Self, Value, TComponent(FLocationFTP));
end;

procedure TJvProgramVersionCheck.SetLocationHTTP(const Value: TJvProgramVersionHTTPLocation);
begin
  ReplaceComponentReference(Self, Value, TComponent(FLocationHTTP));
end;

procedure TJvProgramVersionCheck.SetLocationNetwork(const Value: TJvProgramVersionNetworkLocation);
begin
  ReplaceComponentReference(Self, Value, TComponent(FLocationNetwork));
end;


procedure TJvProgramVersionCheck.VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter:
    TJvBaseParameter);
var
  I: TJvProgramReleaseType;
begin
  I := Low(I);
  Inc(I, Parameter.Tag);
  if Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[I]) then
    ShowProgramVersionsDescription(CurrentFileVersion, RemoteProgramVersionHistory.CurrentProgramVersion[I].ProgramVersion);
end;

{$IFDEF USE_3RDPARTY_INDY}

//=== { TJvProgramVersionHTTPLocationIndy } ==================================

constructor TJvProgramVersionHTTPLocationIndy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdHTTP := TIdHTTP.Create(Self);
end;

destructor TJvProgramVersionHTTPLocationIndy.Destroy;
begin
  FIdHttp.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName)
  else
    Result := LoadFileFromRemoteIndy(ARemotePath, ARemoteFileName, ALocalPath,
      ALocalFileName, ABaseThread);
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteIndy(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
  RemoteURL: string;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      ResultName := PathAppend(ALocalPath, ARemoteFileName)
    else
      ResultName := PathAppend(ALocalPath, ALocalFileName)
  else
    Exit;

  if (ARemotePath <> '') and (ARemotePath[Length(ARemotePath)] <> '/') then
    RemoteURL := ARemotePath + '/' + ARemoteFileName
  else
    RemoteURL := ARemotePath + ARemoteFileName;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    {$IFDEF USE_3RDPARTY_INDY10}
    FIdHTTP.URL.URI := RemoteURL;
    if (FIdHTTP.URL.Port = '') and (Port <> 0) then
    begin
      FIdHTTP.URL.Port := IntToStr(Port);
      RemoteURL := FIdHTTP.URL.URI;
    end;
    {$ELSE}
    FIdHTTP.Port := Port;
    {$ENDIF USE_3RDPARTY_INDY10}
    FIdHTTP.ProxyParams.ProxyPort := ProxySettings.Port;
    FIdHTTP.ProxyParams.ProxyServer := ProxySettings.Server;
    FIdHTTP.ProxyParams.ProxyUsername := ProxySettings.UserName;
    FIdHTTP.ProxyParams.ProxyPassword := ProxySettings.Password;
    if UserName <> '' then
      FIdHTTP.Request.UserName := UserName;
    if Password <> '' then
      FIdHTTP.Request.Password := Password;
    FIdHTTP.Request.BasicAuthentication := PasswordRequired;
    try
      FIdHTTP.Get(RemoteURL, ResultStream)
    except
      on E: Exception do
        DownloadError := E.Message;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIndy } ===================================

constructor TJvProgramVersionFTPLocationIndy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdFTP := TIdFTP.Create(Self);
end;

destructor TJvProgramVersionFTPLocationIndy.Destroy;
begin
  FIdFtp.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName)
  else
    Result := LoadFileFromRemoteIndy(ARemotePath, ARemoteFileName, ALocalPath,
      ALocalFileName, ABaseThread);
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteIndy(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      ResultName := PathAppend(ALocalPath, ARemoteFileName)
    else
      ResultName := PathAppend(ALocalPath, ALocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    FIdFTP.Port := Port;
    FIdFTP.ProxySettings.Port := ProxySettings.Port;
    FIdFTP.ProxySettings.Host := ProxySettings.Server;
    FIdFTP.ProxySettings.UserName := ProxySettings.UserName;
    FIdFTP.ProxySettings.Password := ProxySettings.Password;
      try
        if Copy(ARemotePath, Length(ARemotePath), 1) <> '/' then
          FIdFTP.Get(ARemotePath + '/' + ARemoteFileName, ResultStream)
        else
          FIdFTP.Get(ARemotePath + ARemoteFileName, ResultStream);
      except
        on E: Exception do
          DownloadError := E.Message;
      end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_INDY}

{$IFDEF USE_3RDPARTY_ICS}

//=== { TJvProgramVersionHTTPLocationIcs } ===================================

constructor TJvProgramVersionHTTPLocationIcs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpCli := THttpCli.Create(Self);
end;

destructor TJvProgramVersionHTTPLocationIcs.Destroy;
begin
  FHttpCli.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName)
  else
    Result := LoadFileFromRemoteIcs(ARemotePath, ARemoteFileName, ALocalPath,
      ALocalFileName, ABaseThread);
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteIcs(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      ResultName := PathAppend(ALocalPath, ARemoteFileName)
    else
      ResultName := PathAppend(ALocalPath, ALocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    //FHttpCli.Port := Port;
    with FHttpCli do
    begin
      MultiThreaded := False;
      ProxyPort := inttostr(ProxySettings.Port);
      Proxy := ProxySettings.Server;
      ProxyUsername := ProxySettings.UserName;
      ProxyPassword := ProxySettings.Password;
      RcvdStream := ResultStream;
      if Copy(ARemotePath, Length(ARemotePath), 1) <> '/' then
        Url := ARemotePath + '/' + ARemoteFileName
      else
        Url := ARemotePath + ARemoteFileName;
      try
        Get
      except
        on E: EHttpException do
          DownloadError := Format(RsPVDownloadFailed, [IntToStr(StatusCode) + ' ' + ReasonPhrase]);
      else
        raise;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIcs } ====================================

constructor TJvProgramVersionFTPLocationIcs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFtpClient := TFtpClient.Create(Self);
end;

destructor TJvProgramVersionFTPLocationIcs.Destroy;
begin
  FFtpClient.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteInt(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(FOnLoadFileFromRemote) then
    Result := FOnLoadFileFromRemote(Self, ARemotePath, ARemoteFileName,
      ALocalPath, ALocalFileName)
  else
    Result := LoadFileFromRemoteIcs(ARemotePath, ARemoteFileName, ALocalPath,
      ALocalFileName, ABaseThread);
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteIcs(
  const ARemotePath, ARemoteFileName, ALocalPath, ALocalFileName: string;
  ABaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
  P: Integer;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      ResultName := PathAppend(ALocalPath, ARemoteFileName)
    else
      ResultName := PathAppend(ALocalPath, ALocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    //  FFtpClient.Port := inttostr(Port);
    FFtpClient.DataPortRangeStart := Port;
    FFtpClient.DataPortRangeEnd := Port;
    FFtpClient.UserName := UserName;
    FFtpClient.Password := Password;
    with FFtpClient do
    begin
      //    FtpClient1.HostName           := HostNameEdit.Text;
      //    FtpClient1.Port               := PortEdit.Text;
      //    FtpClient1.DataPortRangeStart := StrToInt(Trim(DataPortRangeStartEdit.Text));
      //    FtpClient1.DataPortRangeEnd   := Port;
      //    FtpClient1.UserName           := UserNameEdit.Text;
      //    FtpClient1.Password           := PasswordEdit.Text;
      //    FtpClient1.Account            := AccountEdit.Text;
      //    FtpClient1.HostDirName        := HostDirEdit.Text;
      //    FtpClient1.HostFileName       := HostFileEdit.Text;
      //    FtpClient1.LocalFileName      := LocalFileEdit.Text;
      //    FtpClient1.Passive            := PassiveCheckBox.Checked;
      //    FtpClient1.Binary             := BinaryCheckBox.Checked;
      MultiThreaded := False;
      Binary := True;
      ProxyPort := IntToStr(ProxySettings.Port);
      ProxyServer := ProxySettings.Server;
      //      ProxyUsername := ProxySettings.UserName;
      //      ProxyPassword := ProxySettings.Password;
      //      Port := 'ftp';
      //      RcvdStream := ResultStream;
      LocalFileName := ResultName;
      P := Pos('://', ARemotePath);
      if P > 0 then
      begin
        HostName := Copy(ARemotePath, P + 3, Length(ARemotePath) - P - 2);
        P := Pos('/', HostName);
        HostDirName := Copy(HostName, P + 1, Length(HostName) - P);
        HostName := Copy(HostName, 1, P - 1);
      end
      else
      begin
        P := Pos('/', ARemotePath);
        HostName := Copy(ARemotePath, 1, P - 1);
        HostDirName := Copy(ARemotePath, P + 1, Length(ARemotePath) - P);
      end;
      if Copy(HostDirName, Length(HostDirName), 1) = '/' then
        HostDirName := Copy(HostDirName, 1, Length(HostDirName) - 1);
      if HostDirName = '' then
        HostDirName := '/';
      if Copy(HostDirName, 1, 1) <> '/' then
        HostDirName := '/' + HostDirName;
      HostFileName := ARemoteFileName;
      try
        try
          if not Open then
          begin
            DownloadError := Format(RsPVFailedUnableToConnectTo, [HostName]);
            Exit;
          end;
          if not Get then
          begin
            DownloadError := Format(RsPVFailedUnableToGet, [HostDirName + '/' + HostFileName]);
            Exit;
          end;
        except
          on E: Exception do
            DownloadError := Format(RsPVDownloadFailed, [E.Message]);
        else
          raise;
        end;
      finally
        if Connected then
          Quit;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_ICS}

//=== { TJvProgramVersionProxySettings } =====================================

constructor TJvProgramVersionProxySettings.Create;
begin
  inherited Create;
  FPort := 80;
end;

constructor TJvProgramVersionHistoryAppStorageOptions.Create;
begin
  inherited Create;
  FXMLOptions := TJvAppXMLStorageOptions.Create;
  FXMLOptions.WhiteSpaceReplacement := '_';
  FXMLOptions.UseOldItemNameFormat := False;
  FXMLOptions.SetAsString := True;
  FXMLOptions.FloatAsString := True;
  FXMLOptions.DefaultIfReadConvertError := True;
  FXMLOptions.DateTimeAsString := True;
  FINIOptions := TJvAppIniStorageOptions.Create;
  FINIOptions.SetAsString := True;
  FINIOptions.FloatAsString := True;
  FINIOptions.DefaultIfReadConvertError := True;
  FINIOptions.DateTimeAsString := True;
  FFileFormat := hffIni;
end;

destructor TJvProgramVersionHistoryAppStorageOptions.Destroy;
begin
  FreeAndNil(FINIOptions);
  FreeAndNil(FXMLOptions);
  inherited Destroy;
end;

procedure TJvProgramVersionHistoryAppStorageOptions.SetINIOptions(const Value: TJvAppIniStorageOptions);
begin
  INIOptions.Assign(Value);
end;

procedure TJvProgramVersionHistoryAppStorageOptions.SetXMLOptions(const Value: TJvAppXMLStorageOptions);
begin
  FXMLOptions.Assign(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
