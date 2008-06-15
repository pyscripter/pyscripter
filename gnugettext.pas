{*------------------------------------------------------------------------------
  GNU gettext translation system for Delphi, Kylix, C++ Builder and others.
  All parts of the translation system are kept in this unit.

  @author Lars B. Dybdahl and others
  @version $LastChangedRevision: 153 $
  @see http://dybdahl.dk/dxgettext/
-------------------------------------------------------------------------------}
unit gnugettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and others               *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*                                                            *)
(*  Contributors: Peter Thornqvist, Troy Wolbrink,            *)
(*                Frank Andreas de Groot, Igor Siticov,       *)
(*                Jacques Garcia Vazquez                      *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

// Information about this file:
// $LastChangedDate: 2008-03-10 10:29:54 +0100 (ma, 10 mar 2008) $
// $LastChangedRevision: 153 $
// $HeadURL: https://dybdahl@svn.berlios.de/svnroot/repos/dxgettext/trunk/dxgettext/sample/gnugettext.pas $

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// The names of any contributor may not be used to endorse or promote
// products derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

// If the conditional define DXGETTEXTDEBUG is defined, debugging log is activated.
// Use DefaultInstance.DebugLogToFile() to write the log to a file.
{ $define DXGETTEXTDEBUG}


uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  Libc,
{$ifdef FPC}
  CWString,
{$endif}
{$endif}
  Classes, SysUtils, TypInfo;

(*****************************************************************************)
(*                                                                           *)
(*  MAIN API                                                                 *)
(*                                                                           *)
(*****************************************************************************)

// Main GNU gettext functions. See documentation for instructions on how to use them.
function _(const szMsgId: widestring): widestring;
function gettext(const szMsgId: widestring): widestring;
function dgettext(const szDomain: string; const szMsgId: widestring): widestring;
function dngettext(const szDomain: string; const singular,plural: widestring; Number:longint): widestring;
function ngettext(const singular,plural: widestring; Number:longint): widestring;
procedure textdomain(const szDomain: string);
function getcurrenttextdomain: string;
procedure bindtextdomain(const szDomain: string; const szDirectory: string);

// Set language to use
procedure UseLanguage(LanguageCode: string);
function GetCurrentLanguage:string;

// Translates a component (form, frame etc.) to the currently selected language.
// Put TranslateComponent(self) in the OnCreate event of all your forms.
// See the manual for documentation on these functions
type
  TTranslator=procedure (obj:TObject) of object;

procedure TP_Ignore(AnObject:TObject; const name:string);
procedure TP_IgnoreClass (IgnClass:TClass);
procedure TP_IgnoreClassProperty (IgnClass:TClass;const propertyname:string);
procedure TP_GlobalIgnoreClass (IgnClass:TClass);
procedure TP_GlobalIgnoreClassProperty (IgnClass:TClass;const propertyname:string);
procedure TP_GlobalHandleClass (HClass:TClass;Handler:TTranslator);
procedure TranslateComponent(AnObject: TComponent; const TextDomain:string='');
procedure RetranslateComponent(AnObject: TComponent; const TextDomain:string='');

// Add more domains that resourcestrings can be extracted from. If a translation
// is not found in the default domain, this domain will be searched, too.
// This is useful for adding mo files for certain runtime libraries and 3rd
// party component libraries
procedure AddDomainForResourceString (const domain:string);
procedure RemoveDomainForResourceString (const domain:string);

// Unicode-enabled way to get resourcestrings, automatically translated
// Use like this: ws:=LoadResStringW(@NameOfResourceString);
function LoadResString(ResStringRec: PResStringRec): widestring;
function LoadResStringA(ResStringRec: PResStringRec): ansistring;
function LoadResStringW(ResStringRec: PResStringRec): widestring;

// This returns an empty string if not translated or translator name is not specified.
function GetTranslatorNameAndEmail:widestring;


(*****************************************************************************)
(*                                                                           *)
(*  ADVANCED FUNCTIONALITY                                                   *)
(*                                                                           *)
(*****************************************************************************)

const
  DefaultTextDomain = 'default';

var
  ExecutableFilename:string;    // This is set to paramstr(0) or the name of the DLL you are creating.

const
  PreferExternal=false;       // Set to true, to prefer external *.mo over embedded translation

const
  // Subversion source code version control version information
  VCSVersion='$LastChangedRevision: 153 $';

type
  EGnuGettext=class(Exception);
  EGGProgrammingError=class(EGnuGettext);
  EGGComponentError=class(EGnuGettext);
  EGGIOError=class(EGnuGettext);
  EGGAnsi2WideConvError=class(EGnuGettext);

// This function will turn resourcestring hooks on or off, eventually with BPL file support.
// Please do not activate BPL file support when the package is in design mode.
const AutoCreateHooks=true;
procedure HookIntoResourceStrings (enabled:boolean=true; SupportPackages:boolean=false);




(*****************************************************************************)
(*                                                                           *)
(*  CLASS based implementation.                                              *)
(*  Use TGnuGettextInstance to have more than one language                   *)
(*  in your application at the same time                                     *)
(*                                                                           *)
(*****************************************************************************)

{$ifdef MSWINDOWS}
{$ifndef VER140}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$endif}
{$endif}

type
  TOnDebugLine = Procedure (Sender: TObject; const Line: String; var Discard: Boolean) of Object;  // Set Discard to false if output should still go to ordinary debug log
  TGetPluralForm=function (Number:Longint):Integer;
  TDebugLogger=procedure (line: ansistring) of object;

{*------------------------------------------------------------------------------
  Handles .mo files, in separate files or inside the exe file.
  Don't use this class. It's for internal use.
-------------------------------------------------------------------------------}
  TMoFile= 
    class /// Threadsafe. Only constructor and destructor are writing to memory
    private
      doswap: boolean;
    public
      Users:Integer; /// Reference count. If it reaches zero, this object should be destroyed.
      constructor Create (filename:string;Offset,Size:int64);
      destructor Destroy; override;
      function gettext(const msgid: ansistring;var found:boolean): ansistring; // uses mo file
      property isSwappedArchitecture:boolean read doswap;
    private
      N, O, T: Cardinal; /// Values defined at http://www.linuxselfhelp.com/gnu/gettext/html_chapter/gettext_6.html
      startindex,startstep:integer;
      {$ifdef mswindows}
      mo: THandle;
      momapping: THandle;
      {$endif}
      momemoryHandle:PChar;
      momemory: PChar;
      function autoswap32(i: cardinal): cardinal;
      function CardinalInMem(baseptr: PChar; Offset: Cardinal): Cardinal;
    end;

{*------------------------------------------------------------------------------
  Handles all issues regarding a specific domain.
  Don't use this class. It's for internal use.
-------------------------------------------------------------------------------}
  TDomain=
    class
    private
      Enabled:boolean;
      vDirectory: string;
      procedure setDirectory(const dir: string);
    public
      DebugLogger:TDebugLogger;
      Domain: string;
      property Directory: string read vDirectory write setDirectory;
      constructor Create;
      destructor Destroy; override;
      // Set parameters
      procedure SetLanguageCode (const langcode:string);
      procedure SetFilename (const filename:string); // Bind this domain to a specific file
      // Get information
      procedure GetListOfLanguages(list:TStrings);
      function GetTranslationProperty(Propertyname: string): WideString;
      function gettext(const msgid: ansistring): ansistring; // uses mo file
    private
      mofile:TMoFile;
      SpecificFilename:string;
      curlang: string;
      OpenHasFailedBefore: boolean;
      procedure OpenMoFile;
      procedure CloseMoFile;
    end;

{*------------------------------------------------------------------------------
  Helper class for invoking events.
-------------------------------------------------------------------------------}
  TExecutable=
    class
      procedure Execute; virtual; abstract;
    end;
    
{*------------------------------------------------------------------------------
  The main translation engine.
-------------------------------------------------------------------------------}
  TGnuGettextInstance=
    class
    private
      fOnDebugLine:TOnDebugLine;
      CreatorThread:Cardinal;  /// Only this thread can use LoadResString
    public
      Enabled:Boolean;      /// Set this to false to disable translations
      DesignTimeCodePage:Integer;  /// See MultiByteToWideChar() in Win32 API for documentation
      constructor Create;
      destructor Destroy; override;
      procedure UseLanguage(LanguageCode: string);
      procedure GetListOfLanguages (const domain:string; list:TStrings); // Puts list of language codes, for which there are translations in the specified domain, into list
      function gettext(const szMsgId: ansistring): widestring; overload; virtual;
      function gettext(const szMsgId: widestring): widestring; overload; virtual;
      function ngettext(const singular,plural:ansistring;Number:longint):widestring; overload; virtual;
      function ngettext(const singular,plural:widestring;Number:longint):widestring; overload; virtual;
      function GetCurrentLanguage:string;
      function GetTranslationProperty (const Propertyname:string):WideString;
      function GetTranslatorNameAndEmail:widestring;

      // Form translation tools, these are not threadsafe. All TP_ procs must be called just before TranslateProperites()
      procedure TP_Ignore(AnObject:TObject; const name:string);
      procedure TP_IgnoreClass (IgnClass:TClass);
      procedure TP_IgnoreClassProperty (IgnClass:TClass;propertyname:string);
      procedure TP_GlobalIgnoreClass (IgnClass:TClass);
      procedure TP_GlobalIgnoreClassProperty (IgnClass:TClass;propertyname:string);
      procedure TP_GlobalHandleClass (HClass:TClass;Handler:TTranslator);
      procedure TranslateProperties(AnObject: TObject; textdomain:string='');
      procedure TranslateComponent(AnObject: TComponent; const TextDomain:string='');
      procedure RetranslateComponent(AnObject: TComponent; const TextDomain:string='');

      // Multi-domain functions
      function dgettext(const szDomain: string; const szMsgId: ansistring): widestring; overload; virtual;
      function dgettext(const szDomain: string; const szMsgId: widestring): widestring; overload; virtual;
      function dngettext(const szDomain: string; const singular,plural:ansistring;Number:longint):widestring; overload; virtual;
      function dngettext(const szDomain: string; const singular,plural:widestring;Number:longint):widestring; overload; virtual;
      procedure textdomain(const szDomain: string);
      function getcurrenttextdomain: string;
      procedure bindtextdomain(const szDomain: string; const szDirectory: string);
      procedure bindtextdomainToFile (const szDomain: string; const filename: string); // Also works with files embedded in exe file

      // Windows API functions
      function LoadResString(ResStringRec: PResStringRec): widestring;

      // Output all log info to this file. This may only be called once.
      procedure DebugLogToFile (const filename:string; append:boolean=false);
      procedure DebugLogPause (PauseEnabled:boolean);
      property  OnDebugLine: TOnDebugLine read fOnDebugLine write fOnDebugLine; // If set, all debug output goes here

      // Conversion according to design-time character set
      function ansi2wideDTCP (const s:ansistring):widestring;  // Convert using Design Time Code Page
    protected
      procedure TranslateStrings (sl:TStrings;const TextDomain:string);

      // Override these three, if you want to inherited from this class
      // to create a new class that handles other domain and language dependent
      // issues
      procedure WhenNewLanguage (const LanguageID:string); virtual;         // Override to know when language changes
      procedure WhenNewDomain (const TextDomain:string); virtual; // Override to know when text domain changes. Directory is purely informational
      procedure WhenNewDomainDirectory (const TextDomain,Directory:string); virtual; // Override to know when any text domain's directory changes. It won't be called if a domain is fixed to a specific file.
    private
      curlang: string;
      curGetPluralForm:TGetPluralForm;
      curmsgdomain: string;
      savefileCS: TMultiReadExclusiveWriteSynchronizer;
      savefile: TextFile;
      savememory: TStringList;
      DefaultDomainDirectory:string;
      domainlist: TStringList;     /// List of domain names. Objects are TDomain.
      TP_IgnoreList:TStringList;   /// Temporary list, reset each time TranslateProperties is called
      TP_ClassHandling:TList;      /// Items are TClassMode. If a is derived from b, a comes first
      TP_GlobalClassHandling:TList;      /// Items are TClassMode. If a is derived from b, a comes first
      TP_Retranslator:TExecutable; /// Cast this to TTP_Retranslator
      DebugLogCS:TMultiReadExclusiveWriteSynchronizer;
      DebugLog:TStream;
      DebugLogOutputPaused:Boolean;
      function TP_CreateRetranslator:TExecutable;  // Must be freed by caller!
      procedure FreeTP_ClassHandlingItems;
      procedure DebugWriteln(line: ansistring);
      procedure TranslateProperty(AnObject: TObject; PropInfo: PPropInfo;
        TodoList: TStrings; const TextDomain:string);
      function Getdomain(const domain, DefaultDomainDirectory, CurLang: string): TDomain;  // Translates a single property of an object
    end;

const
  LOCALE_SISO639LANGNAME = $59;    // Used by Lazarus software development tool
  LOCALE_SISO3166CTRYNAME = $5A;   // Used by Lazarus software development tool

var
  DefaultInstance:TGnuGettextInstance;  /// Default instance of the main API for singlethreaded applications.

implementation

{$ifndef MSWINDOWS}
{$ifndef LINUX}
  'This version of gnugettext.pas is only meant to be compiled with Kylix 3,'
  'Delphi 6, Delphi 7 and later versions. If you use other versions, please'
  'get the gnugettext.pas version from the Delphi 5 directory.'
{$endif}
{$endif}

(**************************************************************************)
// Some comments on the implementation:
// This unit should be independent of other units where possible.
// It should have a small footprint in any way.
(**************************************************************************)
// TMultiReadExclusiveWriteSynchronizer is used instead of TCriticalSection
// because it makes this unit independent of the SyncObjs unit
(**************************************************************************)

{$B-,R+,I+,Q+}

type
  TTP_RetranslatorItem=
    class
      obj:TObject;
      Propname:string;
      OldValue:WideString;
    end;
  TTP_Retranslator=
    class (TExecutable)
      TextDomain:string;
      Instance:TGnuGettextInstance;
      constructor Create;
      destructor Destroy; override;
      procedure Remember (obj:TObject; PropName:String; OldValue:WideString);
      procedure Execute; override;
    private
      list:TList;
    end;
  TEmbeddedFileInfo=
    class
      offset,size:int64;
    end;
  TFileLocator=
    class // This class finds files even when embedded inside executable
      constructor Create;
      destructor Destroy; override;
      procedure Analyze;  // List files embedded inside executable
      function FileExists (filename:string):boolean;
      function GetMoFile (filename:string;DebugLogger:TDebugLogger):TMoFile;
      procedure ReleaseMoFile (mofile:TMoFile);
    private
      basedirectory:string;
      filelist:TStringList; //Objects are TEmbeddedFileInfo. Filenames are relative to .exe file
      MoFilesCS:TMultiReadExclusiveWriteSynchronizer;
      MoFiles:TStringList; // Objects are filenames+offset, objects are TMoFile
      function ReadInt64 (str:TStream):int64;
    end;
  TGnuGettextComponentMarker=
    class (TComponent)
    public
      LastLanguage:string;
      Retranslator:TExecutable;
      destructor Destroy; override;
    end;
  TClassMode=
    class
      HClass:TClass;
      SpecialHandler:TTranslator;
      PropertiesToIgnore:TStringList; // This is ignored if Handler is set
      constructor Create;
      destructor Destroy; override;
    end;
  TRStrinfo = record
    strlength, stroffset: cardinal;
  end;
  TStrInfoArr = array[0..10000000] of TRStrinfo;
  PStrInfoArr = ^TStrInfoArr;
  TCharArray5=array[0..4] of ansichar;
  THook=  // Replaces a runtime library procedure with a custom procedure
    class
    public
      constructor Create (OldProcedure, NewProcedure: pointer; FollowJump:boolean=false);
      destructor Destroy; override;  // Restores unhooked state
      procedure Reset (FollowJump:boolean=false); // Disables and picks up patch points again
      procedure Disable;
      procedure Enable;
    private
      oldproc,newproc:Pointer;
      Patch:TCharArray5;
      Original:TCharArray5;
      PatchPosition:PChar;
      procedure Shutdown; // Same as destroy, except that object is not destroyed
    end;

var
  // System information
  Win32PlatformIsUnicode:boolean=False;
  
  // Information about files embedded inside .exe file
  FileLocator:TFileLocator;

  // Hooks into runtime library functions
  ResourceStringDomainListCS:TMultiReadExclusiveWriteSynchronizer;
  ResourceStringDomainList:TStringList;
  HookLoadResString:THook;
  HookLoadStr:THook;
  HookFmtLoadStr:THook;

function GGGetEnvironmentVariable(const Name:string):string;
var
  Len: integer;
  W : String;
begin
  Result := '';
  SetLength(W,1);
  Len := Windows.GetEnvironmentVariable(PChar(Name), PChar(W), 1);
  if Len > 0 then begin
    SetLength(Result, Len - 1);
    Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Len);
  end;
end;

function StripCR (s:string):string;
var
  i:integer;
begin
  i:=1;
  while i<=length(s) do begin
    if s[i]=#13 then delete (s,i,1) else inc (i);
  end;
  Result:=s;
end;

function LF2LineBreakA (s:string):string;
{$ifdef MSWINDOWS}
var
  i:integer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  Assert (sLinebreak=#13#10);
  i:=1;
  while i<=length(s) do begin
    if (s[i]=#10) and (copy(s,i-1,1)<>#13) then begin
      insert (#13,s,i);
      inc (i,2);
    end else
      inc (i);
  end;
  {$endif}
  Result:=s;
end;

function IsWriteProp(Info: PPropInfo): Boolean;
begin
  Result := Assigned(Info) and (Info^.SetProc <> nil);
end;

function string2csyntax(s: string): string;
// Converts a string to the syntax that is used in .po files
var
  i: integer;
  c: char;
begin
  Result := '';
  for i := 1 to length(s) do begin
    c := s[i];
    case c of
      #32..#33, #35..#255: Result := Result + c;
      #13: Result := Result + '\r';
      #10: Result := Result + '\n"'#13#10'"';
      #34: Result := Result + '\"';
    else
      Result := Result + '\0x' + IntToHex(ord(c), 2);
    end;
  end;
  Result := '"' + Result + '"';
end;

function ResourceStringGettext(MsgId: widestring): widestring;
var
  i:integer;
begin
  if (MsgID='') or (ResourceStringDomainListCS=nil) then begin
    // This only happens during very complicated program startups that fail,
    // or when Msgid=''
    Result:=MsgId;
    exit;
  end;
  ResourceStringDomainListCS.BeginRead;
  try
    for i:=0 to ResourceStringDomainList.Count-1 do begin
      Result:=dgettext(ResourceStringDomainList.Strings[i], MsgId);
      if Result<>MsgId then
        break;
    end;
  finally
    ResourceStringDomainListCS.EndRead;
  end;
end;

function gettext(const szMsgId: widestring): widestring;
begin
  Result:=DefaultInstance.gettext(szMsgId);
end;

{*------------------------------------------------------------------------------
  This is the main translation procedure used in programs. It takes a parameter,
  looks it up in the translation dictionary, and returns the translation.
  If no translation is found, the parameter is returned.

  @param szMsgId The text, that should be displayed if no translation is found.
-------------------------------------------------------------------------------}
function _(const szMsgId: widestring): widestring;
begin
  Result:=DefaultInstance.gettext(szMsgId);
end;

{*------------------------------------------------------------------------------
  Translates a text, using a specified translation domain.
  If no translation is found, the parameter is returned.

  @param szDomain Which translation domain that should be searched for a translation.
  @param szMsgId The text, that should be displayed if no translation is found.
-------------------------------------------------------------------------------}
function dgettext(const szDomain: string; const szMsgId: widestring): widestring;
begin
  Result:=DefaultInstance.dgettext(szDomain, szMsgId);
end;

function dngettext(const szDomain: string; const singular,plural: widestring; Number:longint): widestring;
begin
  Result:=DefaultInstance.dngettext(szDomain,singular,plural,Number);
end;

function ngettext(const singular,plural: widestring; Number:longint): widestring;
begin
  Result:=DefaultInstance.ngettext(singular,plural,Number);
end;

procedure textdomain(const szDomain: string);
begin
  DefaultInstance.textdomain(szDomain);
end;

procedure SetGettextEnabled (enabled:boolean);
begin
  DefaultInstance.Enabled:=enabled;
end;

function getcurrenttextdomain: string;
begin
  Result:=DefaultInstance.getcurrenttextdomain;
end;

procedure bindtextdomain(const szDomain: string; const szDirectory: string);
begin
  DefaultInstance.bindtextdomain(szDomain, szDirectory);
end;

procedure TP_Ignore(AnObject:TObject; const name:string);
begin
  DefaultInstance.TP_Ignore(AnObject, name);
end;

procedure TP_GlobalIgnoreClass (IgnClass:TClass);
begin
  DefaultInstance.TP_GlobalIgnoreClass(IgnClass);
end;

procedure TP_IgnoreClass (IgnClass:TClass);
begin
  DefaultInstance.TP_IgnoreClass(IgnClass);
end;

procedure TP_IgnoreClassProperty (IgnClass:TClass;const propertyname:string);
begin
  DefaultInstance.TP_IgnoreClassProperty(IgnClass,propertyname);
end;

procedure TP_GlobalIgnoreClassProperty (IgnClass:TClass;const propertyname:string);
begin
  DefaultInstance.TP_GlobalIgnoreClassProperty(IgnClass,propertyname);
end;

procedure TP_GlobalHandleClass (HClass:TClass;Handler:TTranslator);
begin
  DefaultInstance.TP_GlobalHandleClass (HClass, Handler);
end;

procedure TranslateComponent(AnObject: TComponent; const TextDomain:string='');
begin
  DefaultInstance.TranslateComponent(AnObject, TextDomain);
end;

procedure RetranslateComponent(AnObject: TComponent; const TextDomain:string='');
begin
  DefaultInstance.RetranslateComponent(AnObject, TextDomain);
end;

{$ifdef MSWINDOWS}

// These constants are only used in Windows 95
// Thanks to Frank Andreas de Groot for this table
const
  IDAfrikaans                 = $0436;  IDAlbanian                  = $041C;
  IDArabicAlgeria             = $1401;  IDArabicBahrain             = $3C01;
  IDArabicEgypt               = $0C01;  IDArabicIraq                = $0801;
  IDArabicJordan              = $2C01;  IDArabicKuwait              = $3401;
  IDArabicLebanon             = $3001;  IDArabicLibya               = $1001;
  IDArabicMorocco             = $1801;  IDArabicOman                = $2001;
  IDArabicQatar               = $4001;  IDArabic                    = $0401;
  IDArabicSyria               = $2801;  IDArabicTunisia             = $1C01;
  IDArabicUAE                 = $3801;  IDArabicYemen               = $2401;
  IDArmenian                  = $042B;  IDAssamese                  = $044D;
  IDAzeriCyrillic             = $082C;  IDAzeriLatin                = $042C;
  IDBasque                    = $042D;  IDByelorussian              = $0423;
  IDBengali                   = $0445;  IDBulgarian                 = $0402;
  IDBurmese                   = $0455;  IDCatalan                   = $0403;
  IDChineseHongKong           = $0C04;  IDChineseMacao              = $1404;
  IDSimplifiedChinese         = $0804;  IDChineseSingapore          = $1004;
  IDTraditionalChinese        = $0404;  IDCroatian                  = $041A;
  IDCzech                     = $0405;  IDDanish                    = $0406;
  IDBelgianDutch              = $0813;  IDDutch                     = $0413;
  IDEnglishAUS                = $0C09;  IDEnglishBelize             = $2809;
  IDEnglishCanadian           = $1009;  IDEnglishCaribbean          = $2409;
  IDEnglishIreland            = $1809;  IDEnglishJamaica            = $2009;
  IDEnglishNewZealand         = $1409;  IDEnglishPhilippines        = $3409;
  IDEnglishSouthAfrica        = $1C09;  IDEnglishTrinidad           = $2C09;
  IDEnglishUK                 = $0809;  IDEnglishUS                 = $0409;
  IDEnglishZimbabwe           = $3009;  IDEstonian                  = $0425;
  IDFaeroese                  = $0438;  IDFarsi                     = $0429;
  IDFinnish                   = $040B;  IDBelgianFrench             = $080C;
  IDFrenchCameroon            = $2C0C;  IDFrenchCanadian            = $0C0C;
  IDFrenchCotedIvoire         = $300C;  IDFrench                    = $040C;
  IDFrenchLuxembourg          = $140C;  IDFrenchMali                = $340C;
  IDFrenchMonaco              = $180C;  IDFrenchReunion             = $200C;
  IDFrenchSenegal             = $280C;  IDSwissFrench               = $100C;
  IDFrenchWestIndies          = $1C0C;  IDFrenchZaire               = $240C;
  IDFrisianNetherlands        = $0462;  IDGaelicIreland             = $083C;
  IDGaelicScotland            = $043C;  IDGalician                  = $0456;
  IDGeorgian                  = $0437;  IDGermanAustria             = $0C07;
  IDGerman                    = $0407;  IDGermanLiechtenstein       = $1407;
  IDGermanLuxembourg          = $1007;  IDSwissGerman               = $0807;
  IDGreek                     = $0408;  IDGujarati                  = $0447;
  IDHebrew                    = $040D;  IDHindi                     = $0439;
  IDHungarian                 = $040E;  IDIcelandic                 = $040F;
  IDIndonesian                = $0421;  IDItalian                   = $0410;
  IDSwissItalian              = $0810;  IDJapanese                  = $0411;
  IDKannada                   = $044B;  IDKashmiri                  = $0460;
  IDKazakh                    = $043F;  IDKhmer                     = $0453;
  IDKirghiz                   = $0440;  IDKonkani                   = $0457;
  IDKorean                    = $0412;  IDLao                       = $0454;
  IDLatvian                   = $0426;  IDLithuanian                = $0427;
  IDMacedonian                = $042F;  IDMalaysian                 = $043E;
  IDMalayBruneiDarussalam     = $083E;  IDMalayalam                 = $044C;
  IDMaltese                   = $043A;  IDManipuri                  = $0458;
  IDMarathi                   = $044E;  IDMongolian                 = $0450;
  IDNepali                    = $0461;  IDNorwegianBokmol           = $0414;
  IDNorwegianNynorsk          = $0814;  IDOriya                     = $0448;
  IDPolish                    = $0415;  IDBrazilianPortuguese       = $0416;
  IDPortuguese                = $0816;  IDPunjabi                   = $0446;
  IDRhaetoRomanic             = $0417;  IDRomanianMoldova           = $0818;
  IDRomanian                  = $0418;  IDRussianMoldova            = $0819;
  IDRussian                   = $0419;  IDSamiLappish               = $043B;
  IDSanskrit                  = $044F;  IDSerbianCyrillic           = $0C1A;
  IDSerbianLatin              = $081A;  IDSesotho                   = $0430;
  IDSindhi                    = $0459;  IDSlovak                    = $041B;
  IDSlovenian                 = $0424;  IDSorbian                   = $042E;
  IDSpanishArgentina          = $2C0A;  IDSpanishBolivia            = $400A;
  IDSpanishChile              = $340A;  IDSpanishColombia           = $240A;
  IDSpanishCostaRica          = $140A;  IDSpanishDominicanRepublic  = $1C0A;
  IDSpanishEcuador            = $300A;  IDSpanishElSalvador         = $440A;
  IDSpanishGuatemala          = $100A;  IDSpanishHonduras           = $480A;
  IDMexicanSpanish            = $080A;  IDSpanishNicaragua          = $4C0A;
  IDSpanishPanama             = $180A;  IDSpanishParaguay           = $3C0A;
  IDSpanishPeru               = $280A;  IDSpanishPuertoRico         = $500A;
  IDSpanishModernSort         = $0C0A;  IDSpanish                   = $040A;
  IDSpanishUruguay            = $380A;  IDSpanishVenezuela          = $200A;
  IDSutu                      = $0430;  IDSwahili                   = $0441;
  IDSwedishFinland            = $081D;  IDSwedish                   = $041D;
  IDTajik                     = $0428;  IDTamil                     = $0449;
  IDTatar                     = $0444;  IDTelugu                    = $044A;
  IDThai                      = $041E;  IDTibetan                   = $0451;
  IDTsonga                    = $0431;  IDTswana                    = $0432;
  IDTurkish                   = $041F;  IDTurkmen                   = $0442;
  IDUkrainian                 = $0422;  IDUrdu                      = $0420;
  IDUzbekCyrillic             = $0843;  IDUzbekLatin                = $0443;
  IDVenda                     = $0433;  IDVietnamese                = $042A;
  IDWelsh                     = $0452;  IDXhosa                     = $0434;
  IDZulu                      = $0435;

function GetWindowsLanguage: string;
var
  langid: Cardinal;
  langcode: string;
  CountryName: array[0..4] of char;
  LanguageName: array[0..4] of char;
  works: boolean;
begin
  // The return value of GetLocaleInfo is compared with 3 = 2 characters and a zero
  works := 3 = GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME, LanguageName, SizeOf(LanguageName));
  works := works and (3 = GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, CountryName,
    SizeOf(CountryName)));
  if works then begin
    // Windows 98, Me, NT4, 2000, XP and newer
    LangCode := PChar(@LanguageName[0]);
    if lowercase(LangCode)='no' then LangCode:='nb';
    LangCode:=LangCode + '_' + PChar(@CountryName[0]);
  end else begin
    // This part should only happen on Windows 95.
    langid := GetThreadLocale;
    case langid of
      IDBelgianDutch: langcode := 'nl_BE';
      IDBelgianFrench: langcode := 'fr_BE';
      IDBrazilianPortuguese: langcode := 'pt_BR';
      IDDanish: langcode := 'da_DK';
      IDDutch: langcode := 'nl_NL';
      IDEnglishUK: langcode := 'en_GB';
      IDEnglishUS: langcode := 'en_US';
      IDFinnish: langcode := 'fi_FI';
      IDFrench: langcode := 'fr_FR';
      IDFrenchCanadian: langcode := 'fr_CA';
      IDGerman: langcode := 'de_DE';
      IDGermanLuxembourg: langcode := 'de_LU';
      IDGreek: langcode := 'el_GR';
      IDIcelandic: langcode := 'is_IS';
      IDItalian: langcode := 'it_IT';
      IDKorean: langcode := 'ko_KO';
      IDNorwegianBokmol: langcode := 'nb_NO';
      IDNorwegianNynorsk: langcode := 'nn_NO';
      IDPolish: langcode := 'pl_PL';
      IDPortuguese: langcode := 'pt_PT';
      IDRussian: langcode := 'ru_RU';
      IDSpanish, IDSpanishModernSort: langcode := 'es_ES';
      IDSwedish: langcode := 'sv_SE';
      IDSwedishFinland: langcode := 'sv_FI';
    else
      langcode := 'C';
    end;
  end;
  Result := langcode;
end;
{$endif}

function LoadResStringA(ResStringRec: PResStringRec): string;
begin
  Result:=DefaultInstance.LoadResString(ResStringRec);
end;

function GetTranslatorNameAndEmail:widestring;
begin
  Result:=DefaultInstance.GetTranslatorNameAndEmail;
end;

procedure UseLanguage(LanguageCode: string);
begin
  DefaultInstance.UseLanguage(LanguageCode);
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: string;
  end;
  
function SysUtilsEnumStringModules(Instance: Longint; Data: Pointer): Boolean;
{$IFDEF MSWINDOWS}
var
  Buffer: array [0..1023] of char;
begin
  with PStrData(Data)^ do begin
    SetString(Str, Buffer,
      LoadString(Instance, Ident, Buffer, sizeof(Buffer)));
    Result := Str = '';
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  rs:TResStringRec;
  Module:HModule;
begin
  Module:=Instance;
  rs.Module:=@Module;
  with PStrData(Data)^ do begin
    rs.Identifier:=Ident;
    Str:=System.LoadResString(@rs);
    Result:=Str='';
  end;
end;
{$ENDIF}

function SysUtilsFindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  EnumResourceModules(SysUtilsEnumStringModules, @StrData);
  Result := StrData.Str;
end;

function SysUtilsLoadStr(Ident: Integer): string;
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln ('Sysutils.LoadRes('+IntToStr(ident)+') called');
  {$endif}
  Result := ResourceStringGettext(SysUtilsFindStringResource(Ident));
end;

function SysUtilsFmtLoadStr(Ident: Integer; const Args: array of const): string;
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln ('Sysutils.FmtLoadRes('+IntToStr(ident)+',Args) called');
  {$endif}
  FmtStr(Result, SysUtilsFindStringResource(Ident), Args);
  Result:=ResourceStringGettext(Result);
end;

function LoadResString(ResStringRec: PResStringRec): widestring;
begin
  Result:=DefaultInstance.LoadResString(ResStringRec);
end;

function LoadResStringW(ResStringRec: PResStringRec): widestring;
begin
  Result:=DefaultInstance.LoadResString(ResStringRec);
end;



function GetCurrentLanguage:string;
begin
  Result:=DefaultInstance.GetCurrentLanguage;
end;

{ TDomain }

procedure TDomain.CloseMoFile;
begin
  if mofile<>nil then begin
    FileLocator.ReleaseMoFile(mofile);
    mofile:=nil;
  end;
  OpenHasFailedBefore:=False;
end;

destructor TDomain.Destroy;
begin
  CloseMoFile;
  inherited;
end;

{$ifdef mswindows}
function GetLastWinError:string;
var
  errcode:Cardinal;
begin
  SetLength (Result,2000);
  errcode:=GetLastError();
  Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,errcode,0,PChar(Result),2000,nil);
  Result:=StrPas(PChar(Result));
end;
{$endif}

procedure TDomain.OpenMoFile;
var
  filename: string;
begin
  // Check if it is already open
  if mofile<>nil then
    exit;

  // Check if it has been attempted to open the file before
  if OpenHasFailedBefore then
    exit;

  if SpecificFilename<>'' then begin
    filename:=SpecificFilename;
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger ('Domain '+domain+' is bound to specific file '+filename);
    {$endif}
  end else begin
    filename := Directory + curlang + PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
    if (not FileLocator.FileExists(filename)) and (not fileexists(filename)) then begin
      {$ifdef DXGETTEXTDEBUG}
      DebugLogger ('Domain '+domain+': File does not exist, neither embedded or in file system: '+filename);
      {$endif}
      filename := Directory + copy(curlang, 1, 2) + PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
      {$ifdef DXGETTEXTDEBUG}
      DebugLogger ('Domain '+domain+' will attempt to use this file: '+filename);
      {$endif}
    end else begin
      {$ifdef DXGETTEXTDEBUG}
      if FileLocator.FileExists(filename) then
        DebugLogger ('Domain '+domain+' will attempt to use this embedded file: '+filename)
      else
        DebugLogger ('Domain '+domain+' will attempt to use this file that was found on the file system: '+filename);
      {$endif}
    end;
  end;
  if (not FileLocator.FileExists(filename)) and (not fileexists(filename)) then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger ('Domain '+domain+' failed to locate the file: '+filename);
    {$endif}
    OpenHasFailedBefore:=True;
    exit;
  end;
  {$ifdef DXGETTEXTDEBUG}
  DebugLogger ('Domain '+domain+' now accesses the file.');
  {$endif}
  mofile:=FileLocator.GetMoFile(filename, DebugLogger);

  {$ifdef DXGETTEXTDEBUG}
  if mofile.isSwappedArchitecture then
    DebugLogger ('.mo file is swapped (comes from another CPU architecture)');
  {$endif}

  // Check, that the contents of the file is utf-8
  if pos('CHARSET=UTF-8',uppercase(GetTranslationProperty('Content-Type')))=0 then begin
    CloseMoFile;
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger ('The translation for the language code '+curlang+' (in '+filename+') does not have charset=utf-8 in its Content-Type. Translations are turned off.');
    {$endif}
    {$ifdef MSWINDOWS}
    MessageBox(0,PChar('The translation for the language code '+curlang+' (in '+filename+') does not have charset=utf-8 in its Content-Type. Translations are turned off.'),'Localization problem',MB_OK);
    {$else}
    writeln (stderr,'The translation for the language code '+curlang+' (in '+filename+') does not have charset=utf-8 in its Content-Type. Translations are turned off.');
    {$endif}
    Enabled:=False;
  end;
end;

function TDomain.GetTranslationProperty(
  Propertyname: string): WideString;
var
  sl:TStringList;
  i:integer;
  s:string;
begin
  Propertyname:=uppercase(Propertyname)+': ';
  sl:=TStringList.Create;
  try
    sl.Text:=gettext('');  // Everything is UTF-8
    for i:=0 to sl.Count-1 do begin
      s:=sl.Strings[i];
      if uppercase(copy(s,1,length(Propertyname)))=Propertyname then begin
        Result:=utf8decode(trim(copy(s,length(PropertyName)+1,maxint)));
        {$ifdef DXGETTEXTDEBUG}
        DebugLogger ('GetTranslationProperty('+PropertyName+') returns '''+Result+'''.');
        {$endif}
        exit;
      end;
    end;
  finally
    FreeAndNil (sl);
  end;
  Result:='';
  {$ifdef DXGETTEXTDEBUG}
  DebugLogger ('GetTranslationProperty('+PropertyName+') did not find any value. An empty string is returned.');
  {$endif}
end;

procedure TDomain.setDirectory(const dir: string);
begin
  vDirectory := IncludeTrailingPathDelimiter(dir);
  SpecificFilename:='';
  CloseMoFile;
end;

procedure AddDomainForResourceString (const domain:string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln ('Extra domain for resourcestring: '+domain);
  {$endif}
  ResourceStringDomainListCS.BeginWrite;
  try
    if ResourceStringDomainList.IndexOf(domain)=-1 then
      ResourceStringDomainList.Add (domain);
  finally
    ResourceStringDomainListCS.EndWrite;
  end;
end;

procedure RemoveDomainForResourceString (const domain:string);
var
  i:integer;
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln ('Remove domain for resourcestring: '+domain);
  {$endif}
  ResourceStringDomainListCS.BeginWrite;
  try
    i:=ResourceStringDomainList.IndexOf(domain);
    if i<>-1 then
      ResourceStringDomainList.Delete (i);
  finally
    ResourceStringDomainListCS.EndWrite;
  end;
end;

procedure TDomain.SetLanguageCode(const langcode: string);
begin
  CloseMoFile;
  curlang:=langcode;
end;

function GetPluralForm2EN(Number: Integer): Integer;
begin
  Number:=abs(Number);
  if Number=1 then Result:=0 else Result:=1;
end;

function GetPluralForm1(Number: Integer): Integer;
begin
  Result:=0;
end;

function GetPluralForm2FR(Number: Integer): Integer;
begin
  Number:=abs(Number);
  if (Number=1) or (Number=0) then Result:=0 else Result:=1;
end;

function GetPluralForm3LV(Number: Integer): Integer;
begin
  Number:=abs(Number);
  if (Number mod 10=1) and (Number mod 100<>11) then
    Result:=0
  else
    if Number<>0 then Result:=1
                 else Result:=2;
end;

function GetPluralForm3GA(Number: Integer): Integer;
begin
  Number:=abs(Number);
  if Number=1 then Result:=0
  else if Number=2 then Result:=1
  else Result:=2;
end;

function GetPluralForm3LT(Number: Integer): Integer;
var
  n1,n2:byte;
begin
  Number:=abs(Number);
  n1:=Number mod 10;
  n2:=Number mod 100;
  if (n1=1) and (n2<>11) then
    Result:=0
  else
    if (n1>=2) and ((n2<10) or (n2>=20)) then Result:=1
    else Result:=2;
end;

function GetPluralForm3PL(Number: Integer): Integer;
var
  n1,n2:byte;
begin
  Number:=abs(Number);
  n1:=Number mod 10;
  n2:=Number mod 100;

  if Number=1 then Result:=0 
  else if (n1>=2) and (n1<=4) and ((n2<10) or (n2>=20)) then Result:=1
  else Result:=2;
end;

function GetPluralForm3RU(Number: Integer): Integer;
var
  n1,n2:byte;
begin
  Number:=abs(Number);
  n1:=Number mod 10;
  n2:=Number mod 100;
  if (n1=1) and (n2<>11) then
    Result:=0
  else
    if (n1>=2) and (n1<=4) and ((n2<10) or (n2>=20)) then Result:=1
    else Result:=2;
end;

function GetPluralForm3SK(Number: Integer): Integer;
begin
  Number:=abs(Number);
  if number=1 then Result:=0
  else if (number<5) and (number<>0) then Result:=1
  else Result:=2;
end;

function GetPluralForm4SL(Number: Integer): Integer;
var
  n2:byte;
begin
  Number:=abs(Number);
  n2:=Number mod 100;
  if n2=1 then Result:=0
  else
  if n2=2 then Result:=1
  else
  if (n2=3) or (n2=4) then Result:=2
  else
    Result:=3;
end;

procedure TDomain.GetListOfLanguages(list: TStrings);
var
  sr:TSearchRec;
  more:boolean;
  filename, path, langcode:string;
  i, j:integer;
begin
  list.Clear;

  // Iterate through filesystem
  more:=FindFirst (Directory+'*',faAnyFile,sr)=0;
  try
    while more do begin
      if (sr.Attr and faDirectory<>0) and (sr.name<>'.') and (sr.name<>'..') then begin
        filename := Directory + sr.Name + PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
        if fileexists(filename) then begin
          langcode:=lowercase(sr.name);
          if list.IndexOf(langcode)=-1 then
            list.Add(langcode);
        end;
      end;
      more:=FindNext (sr)=0;
    end;
  finally
    FindClose (sr);
  end;

  // Iterate through embedded files
  for i:=0 to FileLocator.filelist.Count-1 do begin
    filename:=FileLocator.basedirectory+FileLocator.filelist.Strings[i];
    path:=Directory;
    {$ifdef MSWINDOWS}
    path:=uppercase(path);
    filename:=uppercase(filename);
    {$endif}
    j:=length(path);
    if copy(filename,1,j)=path then begin
      path:=PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
      {$ifdef MSWINDOWS}
      path:=uppercase(path);
      {$endif}
      if copy(filename,length(filename)-length(path)+1,length(path))=path then begin
        langcode:=lowercase(copy(filename,j+1,length(filename)-length(path)-j));
        langcode:=copy(langcode,1,3)+uppercase(copy(langcode,4,maxint));
        if list.IndexOf(langcode)=-1 then
          list.Add(langcode);
      end;
    end;
  end;
end;

procedure TDomain.SetFilename(const filename: string);
begin
  CloseMoFile;
  vDirectory := '';
  SpecificFilename:=filename;
end;

function TDomain.gettext(const msgid: ansistring): ansistring;
var
  found:boolean;
begin
  if not Enabled then begin
    Result:=msgid;
    exit;
  end;
  if (mofile=nil) and (not OpenHasFailedBefore) then
    OpenMoFile;
  if mofile=nil then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger('.mo file is not open. Not translating "'+msgid+'"');
    {$endif}
    Result := msgid;
  end else begin
    Result:=mofile.gettext(msgid,found);
    {$ifdef DXGETTEXTDEBUG}
    if found then
      DebugLogger ('Found in .mo ('+Domain+'): "'+utf8encode(msgid)+'"->"'+utf8encode(Result)+'"')
    else
      DebugLogger ('Translation not found in .mo file ('+Domain+') : "'+utf8encode(msgid)+'"');
    {$endif}
  end;
end;

constructor TDomain.Create;
begin
  inherited Create;
  Enabled:=True;
end;

{ TGnuGettextInstance }

procedure TGnuGettextInstance.bindtextdomain(const szDomain,
  szDirectory: string);
var
  dir:string;
begin
  dir:=IncludeTrailingPathDelimiter(szDirectory);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Text domain "'+szDomain+'" is now located at "'+dir+'"');
  {$endif}
  getdomain(szDomain,DefaultDomainDirectory,CurLang).Directory := dir;
  WhenNewDomainDirectory (szDomain, szDirectory);
end;

constructor TGnuGettextInstance.Create;
begin
  CreatorThread:=GetCurrentThreadId;
  {$ifdef MSWindows}
  DesignTimeCodePage:=CP_ACP;
  {$endif}
  {$ifdef DXGETTEXTDEBUG}
  DebugLogCS:=TMultiReadExclusiveWriteSynchronizer.Create;
  DebugLog:=TMemoryStream.Create;
  DebugWriteln('Debug log started '+DateTimeToStr(Now));
  DebugWriteln('GNU gettext module version: '+VCSVersion);
  DebugWriteln('');
  {$endif}
  curGetPluralForm:=GetPluralForm2EN;
  Enabled:=True;
  curmsgdomain:=DefaultTextDomain;
  savefileCS := TMultiReadExclusiveWriteSynchronizer.Create;
  domainlist := TStringList.Create;
  TP_IgnoreList:=TStringList.Create;
  TP_IgnoreList.Sorted:=True;
  TP_GlobalClassHandling:=TList.Create;
  TP_ClassHandling:=TList.Create;

  // Set some settings
  DefaultDomainDirectory := IncludeTrailingPathDelimiter(extractfilepath(ExecutableFilename))+'locale';

  UseLanguage('');

  bindtextdomain(DefaultTextDomain, DefaultDomainDirectory);
  textdomain(DefaultTextDomain);

  // Add default properties to ignore
  TP_GlobalIgnoreClassProperty(TComponent,'Name');
  TP_GlobalIgnoreClassProperty(TCollection,'PropName');
end;

destructor TGnuGettextInstance.Destroy;
begin
  if savememory <> nil then begin
    savefileCS.BeginWrite;
    try
      CloseFile(savefile);
    finally
      savefileCS.EndWrite;
    end;
    FreeAndNil(savememory);
  end;
  FreeAndNil (savefileCS);
  FreeAndNil (TP_IgnoreList);
  while TP_GlobalClassHandling.Count<>0 do begin
    TObject(TP_GlobalClassHandling.Items[0]).Free;
    TP_GlobalClassHandling.Delete(0);
  end;
  FreeAndNil (TP_GlobalClassHandling);
  FreeTP_ClassHandlingItems;
  FreeAndNil (TP_ClassHandling);
  while domainlist.Count <> 0 do begin
    domainlist.Objects[0].Free;
    domainlist.Delete(0);
  end;
  FreeAndNil(domainlist);
  {$ifdef DXGETTEXTDEBUG}
  FreeAndNil (DebugLog);
  FreeAndNil (DebugLogCS);
  {$endif}
  inherited;
end;

function TGnuGettextInstance.dgettext(const szDomain: string; const szMsgId: ansistring): widestring;
begin
  Result:=dgettext(szDomain, ansi2wideDTCP(szMsgId));
end;

function TGnuGettextInstance.dgettext(const szDomain: string;
  const szMsgId: widestring): widestring;
begin
  if not Enabled then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('Translation has been disabled. Text is not being translated: '+szMsgid);
    {$endif}
    Result:=szMsgId;
  end else begin
    Result:=UTF8Decode(LF2LineBreakA(getdomain(szDomain,DefaultDomainDirectory,CurLang).gettext(StripCR(utf8encode(szMsgId)))));
    {$ifdef DXGETTEXTDEBUG}
    if (szMsgId<>'') and (Result='') then
      DebugWriteln (Format('Error: Translation of %s was an empty string. This may never occur.',[szMsgId]));
    {$endif}
  end;
end;

function TGnuGettextInstance.GetCurrentLanguage: string;
begin
  Result:=curlang;
end;

function TGnuGettextInstance.getcurrenttextdomain: string;
begin
  Result := curmsgdomain;
end;

function TGnuGettextInstance.gettext(
  const szMsgId: ansistring): widestring;
begin
  Result := dgettext(curmsgdomain, szMsgId);
end;

function TGnuGettextInstance.gettext(
  const szMsgId: widestring): widestring;
begin
  Result := dgettext(curmsgdomain, szMsgId);
end;

procedure TGnuGettextInstance.textdomain(const szDomain: string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Changed text domain to "'+szDomain+'"');
  {$endif}
  curmsgdomain := szDomain;
  WhenNewDomain (szDomain);
end;

function TGnuGettextInstance.TP_CreateRetranslator : TExecutable;
var
  ttpr:TTP_Retranslator;
begin
  ttpr:=TTP_Retranslator.Create;
  ttpr.Instance:=self;
  TP_Retranslator:=ttpr;
  Result:=ttpr;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('A retranslator was created.');
  {$endif}
end;

procedure TGnuGettextInstance.TP_GlobalHandleClass(HClass: TClass;
  Handler: TTranslator);
var
  cm:TClassMode;
  i:integer;
begin
  for i:=0 to TP_GlobalClassHandling.Count-1 do begin
    cm:=TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
    if cm.HClass=HClass then
      raise EGGProgrammingError.Create ('You cannot set a handler for a class that has already been assigned otherwise.');
    if HClass.InheritsFrom(cm.HClass) then begin
      // This is the place to insert this class
      cm:=TClassMode.Create;
      cm.HClass:=HClass;
      cm.SpecialHandler:=Handler;
      TP_GlobalClassHandling.Insert(i,cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('A handler was set for class '+HClass.ClassName+'.');
      {$endif}
      exit;
    end;
  end;
  cm:=TClassMode.Create;
  cm.HClass:=HClass;
  cm.SpecialHandler:=Handler;
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('A handler was set for class '+HClass.ClassName+'.');
  {$endif}
end;

procedure TGnuGettextInstance.TP_GlobalIgnoreClass(IgnClass: TClass);
var
  cm:TClassMode;
  i:integer;
begin
  for i:=0 to TP_GlobalClassHandling.Count-1 do begin
    cm:=TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
    if cm.HClass=IgnClass then
      raise EGGProgrammingError.Create ('You cannot add a class to the ignore list that is already on that list: '+IgnClass.ClassName+'. You should keep all TP_Global functions in one place in your source code.');
    if IgnClass.InheritsFrom(cm.HClass) then begin
      // This is the place to insert this class
      cm:=TClassMode.Create;
      cm.HClass:=IgnClass;
      TP_GlobalClassHandling.Insert(i,cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Globally, class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
  end;
  cm:=TClassMode.Create;
  cm.HClass:=IgnClass;
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Globally, class '+IgnClass.ClassName+' is being ignored.');
  {$endif}
end;

procedure TGnuGettextInstance.TP_GlobalIgnoreClassProperty(
  IgnClass: TClass; propertyname: string);
var
  cm:TClassMode;
  i,idx:integer;
begin
  propertyname:=uppercase(propertyname);
  for i:=0 to TP_GlobalClassHandling.Count-1 do begin
    cm:=TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
    if cm.HClass=IgnClass then begin
      if Assigned(cm.SpecialHandler) then
        raise EGGProgrammingError.Create ('You cannot ignore a class property for a class that has a handler set.');
      if not cm.PropertiesToIgnore.Find(propertyname,idx) then
        cm.PropertiesToIgnore.Add(propertyname);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Globally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
    if IgnClass.InheritsFrom(cm.HClass) then begin
      // This is the place to insert this class
      cm:=TClassMode.Create;
      cm.HClass:=IgnClass;
      cm.PropertiesToIgnore.Add(propertyname);
      TP_GlobalClassHandling.Insert(i,cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Globally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
  end;
  cm:=TClassMode.Create;
  cm.HClass:=IgnClass;
  cm.PropertiesToIgnore.Add(propertyname);
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Globally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
  {$endif}
end;

procedure TGnuGettextInstance.TP_Ignore(AnObject: TObject;
  const name: string);
begin
  TP_IgnoreList.Add(uppercase(name));
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('On object with class name '+AnObject.ClassName+', ignore is set on '+name);
  {$endif}
end;

procedure TGnuGettextInstance.TranslateComponent(AnObject: TComponent;
  const TextDomain: string);
var
  comp:TGnuGettextComponentMarker;
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('======================================================================');
  DebugWriteln ('TranslateComponent() was called for a component with name '+AnObject.Name+'.');
  {$endif}
  comp:=AnObject.FindComponent('GNUgettextMarker') as TGnuGettextComponentMarker;
  if comp=nil then begin
    comp:=TGnuGettextComponentMarker.Create (nil);
    comp.Name:='GNUgettextMarker';
    comp.Retranslator:=TP_CreateRetranslator;
    TranslateProperties (AnObject, TextDomain);
    AnObject.InsertComponent(comp);
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('This is the first time, that this component has been translated. A retranslator component has been created for this component.');
    {$endif}
  end else begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('This is not the first time, that this component has been translated.');
    {$endif}
    if comp.LastLanguage<>curlang then begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('ERROR: TranslateComponent() was called twice with different languages. This indicates an attempt to switch language at runtime, but by using TranslateComponent every time. This API has changed - please use RetranslateComponent() instead.');
      {$endif}
      {$ifdef mswindows}
      MessageBox (0,'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.','Error',MB_OK);
      {$else}
      writeln (stderr,'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.');
      {$endif}
    end else begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('ERROR: TranslateComponent has been called twice, but with the same language chosen. This is a mistake, but in order to prevent that the application breaks, no exception is raised.');
      {$endif}
    end;
  end;
  comp.LastLanguage:=curlang;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('======================================================================');
  {$endif}
end;

procedure TGnuGettextInstance.TranslateProperty (AnObject:TObject; PropInfo:PPropInfo; TodoList:TStrings; const TextDomain:string);
var
  ppi:PPropInfo;
  ws: WideString;
  old: WideString;
  compmarker:TComponent;
  obj:TObject;
  Propname:string;
begin
  PropName:=PropInfo^.Name;
  try
    // Translate certain types of properties
    case PropInfo^.PropType^.Kind of
      tkString, tkLString, tkWString:
        begin
          {$ifdef DXGETTEXTDEBUG}
          DebugWriteln ('Translating '+AnObject.ClassName+'.'+PropName);
          {$endif}
          if PropInfo^.PropType^.Kind<>tkWString then
            old := ansi2wideDTCP(GetStrProp(AnObject, PropName))
          else
            old := GetWideStrProp(AnObject, PropName);
          {$ifdef DXGETTEXTDEBUG}
          if old='' then
            DebugWriteln ('(Empty, not translated)')
          else
            DebugWriteln ('Old value: "'+old+'"');
          {$endif}
          if (old <> '') and (IsWriteProp(PropInfo)) then begin
            if TP_Retranslator<>nil then
              (TP_Retranslator as TTP_Retranslator).Remember(AnObject, PropName, old);
            ws := dgettext(textdomain,old);
            if ws <> old then begin
              ppi:=GetPropInfo(AnObject, Propname);
              if ppi<>nil then begin
                SetWideStrProp(AnObject, ppi, ws);
              end else begin
                DebugWriteln ('ERROR: Property disappeared: '+Propname+' for object of type '+AnObject.ClassName);
              end;
            end;
          end;
        end { case item };
      tkClass:
        begin
          obj:=GetObjectProp(AnObject, PropName);
          if obj<>nil then begin
            if obj is TComponent then begin
              compmarker := TComponent(obj).FindComponent('GNUgettextMarker');
              if Assigned(compmarker) then
                exit;
            end;
            TodoList.AddObject ('',obj);
          end;
        end { case item };
      end { case };
  except
    on E:Exception do
      raise EGGComponentError.Create ('Property cannot be translated.'+sLineBreak+
        'Add TP_GlobalIgnoreClassProperty('+AnObject.ClassName+','''+PropName+''') to your source code or use'+sLineBreak+
        'TP_Ignore (self,''.'+PropName+''') to prevent this message.'+sLineBreak+
        'Reason: '+e.Message);
  end;
end;

procedure TGnuGettextInstance.TranslateProperties(AnObject: TObject; textdomain:string='');
var
  TodoList:TStringList; // List of Name/TObject's that is to be processed
  DoneList:TStringList; // List of hex codes representing pointers to objects that have been done
  i, j, Count: integer;
  PropList: PPropList;
  UPropName: string;
  PropInfo: PPropInfo;
  compmarker,
  comp:TComponent;
  cm,
  currentcm:TClassMode; // currentcm is nil or contains special information about how to handle the current object
  ObjectPropertyIgnoreList:TStringList;
  objid, Name:string;
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('----------------------------------------------------------------------');
  DebugWriteln ('TranslateProperties() was called for an object of class '+AnObject.ClassName+' with domain "'+textdomain+'".');
  {$endif}
  if textdomain='' then
    textdomain:=curmsgdomain;
  if TP_Retranslator<>nil then
    (TP_Retranslator as TTP_Retranslator).TextDomain:=textdomain;
  {$ifdef FPC}
  DoneList:=TCSStringList.Create;
  TodoList:=TCSStringList.Create;
  ObjectPropertyIgnoreList:=TCSStringList.Create;
  {$else}
  DoneList:=TStringList.Create;
  TodoList:=TStringList.Create;
  ObjectPropertyIgnoreList:=TStringList.Create;
  {$endif}
  try
    TodoList.AddObject('', AnObject);
    DoneList.Sorted:=True;
    ObjectPropertyIgnoreList.Sorted:=True;
    ObjectPropertyIgnoreList.Duplicates:=dupIgnore;
    ObjectPropertyIgnoreList.CaseSensitive:=False;
    DoneList.Duplicates:=dupError;
    DoneList.CaseSensitive:=True;

    while TodoList.Count<>0 do begin
      AnObject:=TodoList.Objects[0];
      Name:=TodoList.Strings[0];
      TodoList.Delete(0);
      if (AnObject<>nil) and (AnObject is TPersistent) then begin
        // Make sure each object is only translated once
        Assert (sizeof(integer)=sizeof(TObject));
        objid:=IntToHex(integer(AnObject),8);
        if DoneList.Find(objid,i) then begin
          continue;
        end else begin
          DoneList.Add(objid);
        end;

        ObjectPropertyIgnoreList.Clear;

        // Find out if there is special handling of this object
        currentcm:=nil;
        // First check the local handling instructions
        for j:=0 to TP_ClassHandling.Count-1 do begin
          cm:=TObject(TP_ClassHandling.Items[j]) as TClassMode;
          if AnObject.InheritsFrom(cm.HClass) then begin
            if cm.PropertiesToIgnore.Count<>0 then begin
              ObjectPropertyIgnoreList.AddStrings(cm.PropertiesToIgnore);
            end else begin
              // Ignore the entire class
              currentcm:=cm;
              break;
            end;
          end;
        end;
        // Then check the global handling instructions
        if currentcm=nil then
        for j:=0 to TP_GlobalClassHandling.Count-1 do begin
          cm:=TObject(TP_GlobalClassHandling.Items[j]) as TClassMode;
          if AnObject.InheritsFrom(cm.HClass) then begin
            if cm.PropertiesToIgnore.Count<>0 then begin
              ObjectPropertyIgnoreList.AddStrings(cm.PropertiesToIgnore);
            end else begin
              // Ignore the entire class
              currentcm:=cm;
              break;
            end;
          end;
        end;
        if currentcm<>nil then begin
          ObjectPropertyIgnoreList.Clear;
          // Ignore or use special handler
          if Assigned(currentcm.SpecialHandler) then begin
            currentcm.SpecialHandler (AnObject);
            {$ifdef DXGETTEXTDEBUG}
            DebugWriteln ('Special handler activated for '+AnObject.ClassName);
            {$endif}
          end else begin
            {$ifdef DXGETTEXTDEBUG}
            DebugWriteln ('Ignoring object '+AnObject.ClassName);
            {$endif}
          end;
          continue;
        end;

        Count := GetPropList(AnObject, PropList);
        try
          for j := 0 to Count - 1 do begin
            PropInfo := PropList[j];
            if not (PropInfo^.PropType^.Kind in [tkString, tkLString, tkWString, tkClass]) then
              continue;
            UPropName:=uppercase(PropInfo^.Name);
            // Ignore properties that are meant to be ignored
            if ((currentcm=nil) or (not currentcm.PropertiesToIgnore.Find(UPropName,i))) and
               (not TP_IgnoreList.Find(Name+'.'+UPropName,i)) and
               (not ObjectPropertyIgnoreList.Find(UPropName,i)) then begin
              TranslateProperty (AnObject,PropInfo,TodoList,TextDomain);
            end;  // if
          end;  // for
        finally
          if Count<>0 then
            FreeMem (PropList);
        end;
        if AnObject is TStrings then begin
          if ((AnObject as TStrings).Text<>'') and (TP_Retranslator<>nil) then
            (TP_Retranslator as TTP_Retranslator).Remember(AnObject, 'Text', (AnObject as TStrings).Text);
          TranslateStrings (AnObject as TStrings,TextDomain);
        end;
        // Check for TCollection
        if AnObject is TCollection then begin
          for i := 0 to (AnObject as TCollection).Count - 1 do begin
            // Only add the object if it's not totally ignored already
            if not Assigned(currentcm) or not AnObject.InheritsFrom(currentcm.HClass) then
              TodoList.AddObject('',(AnObject as TCollection).Items[i]);
          end;
        end;
        if AnObject is TComponent then begin
          for i := 0 to TComponent(AnObject).ComponentCount - 1 do begin
            comp:=TComponent(AnObject).Components[i];
            if (not TP_IgnoreList.Find(uppercase(comp.Name),j)) then begin
              // Only add the object if it's not totally ignored or translated already
              if not Assigned(currentcm) or not AnObject.InheritsFrom(currentcm.HClass) then begin
                compmarker := comp.FindComponent('GNUgettextMarker');
                if not Assigned(compmarker) then
                  TodoList.AddObject(uppercase(comp.Name),comp);
              end;
            end;
          end;
        end;
      end { if AnObject<>nil };
    end { while todolist.count<>0 };
  finally
    FreeAndNil (todolist);
    FreeAndNil (ObjectPropertyIgnoreList);
    FreeAndNil (DoneList);
  end;
  FreeTP_ClassHandlingItems;
  TP_IgnoreList.Clear;
  TP_Retranslator:=nil;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('----------------------------------------------------------------------');
  {$endif}
end;

procedure TGnuGettextInstance.UseLanguage(LanguageCode: string);
var
  i,p:integer;
  dom:TDomain;
  l2:string[2];
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('UseLanguage('''+LanguageCode+'''); called');
  {$endif}

  if LanguageCode='' then begin
    LanguageCode:=GGGetEnvironmentVariable('LANG');
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('LANG env variable is '''+LanguageCode+'''.');
    {$endif}
    {$ifdef MSWINDOWS}
    if LanguageCode='' then begin
      LanguageCode:=GetWindowsLanguage;
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Found Windows language code to be '''+LanguageCode+'''.');
      {$endif}
    end;
    {$endif}
    p:=pos('.',LanguageCode);
    if p<>0 then
      LanguageCode:=copy(LanguageCode,1,p-1);
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('Language code that will be set is '''+LanguageCode+'''.');
    {$endif}
  end;

  curlang := LanguageCode;
  for i:=0 to domainlist.Count-1 do begin
    dom:=domainlist.Objects[i] as TDomain;
    dom.SetLanguageCode (curlang);
  end;

  l2:=lowercase(copy(curlang,1,2));
  if (l2='en') or (l2='de') then curGetPluralForm:=GetPluralForm2EN else
  if (l2='hu') or (l2='ko') or (l2='zh') or (l2='ja') or (l2='tr') then curGetPluralForm:=GetPluralForm1 else
  if (l2='fr') or (l2='fa') or (lowercase(curlang)='pt_br') then curGetPluralForm:=GetPluralForm2FR else
  if (l2='lv') then curGetPluralForm:=GetPluralForm3LV else
  if (l2='ga') then curGetPluralForm:=GetPluralForm3GA else
  if (l2='lt') then curGetPluralForm:=GetPluralForm3LT else
  if (l2='ru') or (l2='uk') or (l2='hr') then curGetPluralForm:=GetPluralForm3RU else
  if (l2='cs') or (l2='sk') then curGetPluralForm:=GetPluralForm3SK else
  if (l2='pl') then curGetPluralForm:=GetPluralForm3PL else
  if (l2='sl') then curGetPluralForm:=GetPluralForm4SL else begin
    curGetPluralForm:=GetPluralForm2EN;
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('Plural form for the language was not found. English plurality system assumed.');
    {$endif}
  end;

  WhenNewLanguage (curlang);

  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('');
  {$endif}
end;

procedure TGnuGettextInstance.TranslateStrings(sl: TStrings;const TextDomain:string);
var
  line: string;
  i: integer;
  s:TStringList;
begin
  if sl.Count > 0 then begin
    sl.BeginUpdate;
    try
      s:=TStringList.Create;
      try
        s.Assign (sl);
        for i:=0 to s.Count-1 do begin
          line:=s.Strings[i];
          if line<>'' then
            s.Strings[i]:=dgettext(TextDomain,line);
        end;
        sl.Assign(s);
      finally
        FreeAndNil (s);
      end;
    finally
      sl.EndUpdate;
    end;
  end;
end;

function TGnuGettextInstance.GetTranslatorNameAndEmail: widestring;
begin
  Result:=GetTranslationProperty('LAST-TRANSLATOR');
end;

function TGnuGettextInstance.GetTranslationProperty(
  const Propertyname: string): WideString;
begin
  Result:=getdomain(curmsgdomain,DefaultDomainDirectory,CurLang).GetTranslationProperty (Propertyname);
end;

function TGnuGettextInstance.dngettext(const szDomain: string; const singular, plural: widestring;
  Number: Integer): widestring;
var
  org,trans:widestring;
  idx:integer;
  p:integer;
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('dngettext translation (domain '+szDomain+', number is '+IntTostr(Number)+') of '+singular+'/'+plural);
  {$endif}
  org:=singular+#0+plural;
  trans:=dgettext(szDomain,org);
  if org=trans then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('Translation was equal to english version. English plural forms assumed.');
    {$endif}
    idx:=GetPluralForm2EN(Number)
  end else
    idx:=curGetPluralForm(Number);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Index '+IntToStr(idx)+' will be used');
  {$endif}
  while true do begin
    p:=pos(#0,trans);
    if p=0 then begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Last translation used: '+utf8encode(trans));
      {$endif}
      Result:=trans;
      exit;
    end;
    if idx=0 then begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Translation found: '+utf8encode(trans));
      {$endif}
      Result:=copy(trans,1,p-1);
      exit;
    end;
    delete (trans,1,p);
    dec (idx);
  end;
end;

function TGnuGettextInstance.ngettext(const singular, plural: ansistring;
  Number: Integer): widestring;
begin
  Result := dngettext(curmsgdomain, singular, plural, Number);
end;

function TGnuGettextInstance.ngettext(const singular, plural: widestring;
  Number: Integer): widestring;
begin
  Result := dngettext(curmsgdomain, singular, plural, Number);
end;

procedure TGnuGettextInstance.WhenNewDomain(const TextDomain: string);
begin
  // This is meant to be empty.
end;

procedure TGnuGettextInstance.WhenNewLanguage(const LanguageID: string);
begin
  // This is meant to be empty.
end;

procedure TGnuGettextInstance.WhenNewDomainDirectory(const TextDomain,
  Directory: string);
begin
  // This is meant to be empty.
end;

procedure TGnuGettextInstance.GetListOfLanguages(const domain: string;
  list: TStrings);
begin
  getdomain(Domain,DefaultDomainDirectory,CurLang).GetListOfLanguages(list);
end;

procedure TGnuGettextInstance.bindtextdomainToFile(const szDomain,
  filename: string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Text domain "'+szDomain+'" is now bound to file named "'+filename+'"');
  {$endif}
  getdomain(szDomain,DefaultDomainDirectory,CurLang).SetFilename (filename);
end;

procedure TGnuGettextInstance.DebugLogPause(PauseEnabled: boolean);
begin
  DebugLogOutputPaused:=PauseEnabled;
end;

procedure TGnuGettextInstance.DebugLogToFile(const filename: string; append:boolean=false);
{$ifdef DXGETTEXTDEBUG}
var
  fs:TFileStream;
  marker:string;
{$endif}
begin
  {$ifdef DXGETTEXTDEBUG}
  // Create the file if needed
  if (not fileexists(filename)) or (not append) then
    fileclose (filecreate (filename));

  // Open file
  fs:=TFileStream.Create (filename,fmOpenWrite or fmShareDenyWrite);
  if append then
    fs.Seek(0,soFromEnd);

  // Write header if appending
  if fs.Position<>0 then begin
    marker:=sLineBreak+'==========================================================================='+sLineBreak;
    fs.WriteBuffer(marker[1],length(marker));
  end;

  // Copy the memorystream contents to the file
  DebugLog.Seek(0,soFromBeginning);
  fs.CopyFrom(DebugLog,0);

  // Make DebugLog point to the filestream
  FreeAndNil (DebugLog);
  DebugLog:=fs;
{$endif}
end;

procedure TGnuGettextInstance.DebugWriteln(line: ansistring);
Var
  Discard: Boolean;
begin
  Assert (DebugLogCS<>nil);
  Assert (DebugLog<>nil);

  DebugLogCS.BeginWrite;
  try
    if DebugLogOutputPaused then
      exit;

    if Assigned (fOnDebugLine) then begin
      Discard := True;
      fOnDebugLine (Self, Line, Discard);
      If Discard then Exit;
    end;

    line:=line+sLineBreak;

    // Ensure that memory usage doesn't get too big.
    if (DebugLog is TMemoryStream) and (DebugLog.Position>1000000) then begin
      line:=sLineBreak+sLineBreak+sLineBreak+sLineBreak+sLineBreak+
            'Debug log halted because memory usage grew too much.'+sLineBreak+
            'Specify a filename to store the debug log in or disable debug loggin in gnugettext.pas.'+
            sLineBreak+sLineBreak+sLineBreak+sLineBreak+sLineBreak;
      DebugLogOutputPaused:=True;
    end;
    DebugLog.WriteBuffer(line[1],length(line));
  finally
    DebugLogCS.EndWrite;
  end;
end;

function TGnuGettextInstance.Getdomain(const domain, DefaultDomainDirectory, CurLang: string): TDomain;
// Retrieves the TDomain object for the specified domain.
// Creates one, if none there, yet.
var
  idx: integer;
begin
  idx := domainlist.IndexOf(Domain);
  if idx = -1 then begin
    Result := TDomain.Create;
    Result.DebugLogger:=DebugWriteln;
    Result.Domain := Domain;
    Result.Directory := DefaultDomainDirectory;
    Result.SetLanguageCode(curlang);
    domainlist.AddObject(Domain, Result);
  end else begin
    Result := domainlist.Objects[idx] as TDomain;
  end;
end;

function TGnuGettextInstance.LoadResString(
  ResStringRec: PResStringRec): widestring;
{$ifdef MSWINDOWS}
var
  Len: Integer;
  Buffer: array [0..1023] of char;
{$endif}
{$ifdef LINUX }
const
  ResStringTableLen = 16;
type
  ResStringTable = array [0..ResStringTableLen-1] of LongWord;
var
  Handle: TResourceHandle;
  Tab: ^ResStringTable;
  ResMod: HMODULE;
{$endif }
begin
  if ResStringRec=nil then
    exit;
  if ResStringRec.Identifier>=64*1024 then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('LoadResString was given an invalid ResStringRec.Identifier');
    {$endif}
    Result:='ERROR';
    exit;
  end else begin
    {$ifdef LINUX}
    // This works with Unicode if the Linux has utf-8 character set
    // Result:=System.LoadResString(ResStringRec);
    ResMod:=FindResourceHInstance(ResStringRec^.Module^);
    Handle:=FindResource(ResMod,
      PChar(ResStringRec^.Identifier div ResStringTableLen), PChar(6));   // RT_STRING
    Tab:=Pointer(LoadResource(ResMod, Handle));
    if Tab=nil then
      Result:=''
    else
      Result:=PWideChar(PChar(Tab)+Tab[ResStringRec^.Identifier mod ResStringTableLen]);
    {$endif}
    {$ifdef MSWINDOWS}
    if not Win32PlatformIsUnicode then begin
      SetString(Result, Buffer,
        LoadString(FindResourceHInstance(ResStringRec.Module^),
          ResStringRec.Identifier, Buffer, SizeOf(Buffer)))
    end else begin
      Result := '';
      Len := 0;
      While Length(Result)<=Len+1 do begin     
        if Length(Result) = 0 then
          SetLength(Result, 1024)
        else
          SetLength(Result, Length(Result) * 2);
        Len := LoadStringW(FindResourceHInstance(ResStringRec.Module^),
          ResStringRec.Identifier, PWideChar(Result), Length(Result));
      end;
      SetLength(Result, Len);
    end;
    {$endif}
  end;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Loaded resourcestring: '+utf8encode(Result));
  {$endif}
  if CreatorThread<>GetCurrentThreadId then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('LoadResString was called from an invalid thread. Resourcestring was not translated.');
    {$endif}
  end else
    Result:=ResourceStringGettext(Result);
end;

procedure TGnuGettextInstance.RetranslateComponent(AnObject: TComponent;
  const TextDomain: string);
var
  comp:TGnuGettextComponentMarker;
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('======================================================================');
  DebugWriteln ('RetranslateComponent() was called for a component with name '+AnObject.Name+'.');
  {$endif}
  comp:=AnObject.FindComponent('GNUgettextMarker') as TGnuGettextComponentMarker;
  if comp=nil then begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln ('Retranslate was called on an object that has not been translated before. An Exception is being raised.');
    {$endif}
    raise EGGProgrammingError.Create ('Retranslate was called on an object that has not been translated before. Please use TranslateComponent() before RetranslateComponent().');
  end else begin
    if comp.LastLanguage<>curlang then begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('The retranslator is being executed.');
      {$endif}
      comp.Retranslator.Execute;
    end else begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('The language has not changed. The retranslator is not executed.');
      {$endif}
    end;
  end;
  comp.LastLanguage:=curlang;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('======================================================================');
  {$endif}
end;

procedure TGnuGettextInstance.TP_IgnoreClass(IgnClass: TClass);
var
  cm:TClassMode;
  i:integer;
begin
  for i:=0 to TP_ClassHandling.Count-1 do begin
    cm:=TObject(TP_ClassHandling.Items[i]) as TClassMode;
    if cm.HClass=IgnClass then
      raise EGGProgrammingError.Create ('You cannot add a class to the ignore list that is already on that list: '+IgnClass.ClassName+'.');
    if IgnClass.InheritsFrom(cm.HClass) then begin
      // This is the place to insert this class
      cm:=TClassMode.Create;
      cm.HClass:=IgnClass;
      TP_ClassHandling.Insert(i,cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Locally, class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
  end;
  cm:=TClassMode.Create;
  cm.HClass:=IgnClass;
  TP_ClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Locally, class '+IgnClass.ClassName+' is being ignored.');
  {$endif}
end;

procedure TGnuGettextInstance.TP_IgnoreClassProperty(IgnClass: TClass;
  propertyname: string);
var
  cm:TClassMode;
  i:integer;
begin
  propertyname:=uppercase(propertyname);
  for i:=0 to TP_ClassHandling.Count-1 do begin
    cm:=TObject(TP_ClassHandling.Items[i]) as TClassMode;
    if cm.HClass=IgnClass then begin
      if Assigned(cm.SpecialHandler) then
        raise EGGProgrammingError.Create ('You cannot ignore a class property for a class that has a handler set.');
      cm.PropertiesToIgnore.Add(propertyname);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Globally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
    if IgnClass.InheritsFrom(cm.HClass) then begin
      // This is the place to insert this class
      cm:=TClassMode.Create;
      cm.HClass:=IgnClass;
      cm.PropertiesToIgnore.Add(propertyname);
      TP_ClassHandling.Insert(i,cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln ('Locally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
      {$endif}
      exit;
    end;
  end;
  cm:=TClassMode.Create;
  cm.HClass:=IgnClass;
  cm.PropertiesToIgnore.Add(propertyname);
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln ('Locally, the '+propertyname+' property of class '+IgnClass.ClassName+' is being ignored.');
  {$endif}
end;

procedure TGnuGettextInstance.FreeTP_ClassHandlingItems;
begin
  while TP_ClassHandling.Count<>0 do begin
    TObject(TP_ClassHandling.Items[0]).Free;
    TP_ClassHandling.Delete(0);
  end;
end;

function TGnuGettextInstance.ansi2wideDTCP(const s: ansistring): widestring;
{$ifdef MSWindows}
var
  len:integer;
{$endif}
begin
{$ifdef MSWindows}
  if DesignTimeCodePage=CP_ACP then begin
    // No design-time codepage specified. Using runtime codepage instead.
{$endif}
    Result:=s;
{$ifdef MSWindows}
  end else begin
    len:=length(s);
    if len=0 then
      Result:=''
    else begin
      SetLength (Result,len);
      len:=MultiByteToWideChar(DesignTimeCodePage,0,pchar(s),len,pwidechar(Result),len);
      if len=0 then
        raise EGGAnsi2WideConvError.Create ('Cannot convert string to widestring:'+sLineBreak+s);
      SetLength (Result,len);
    end;
  end;
{$endif}
end;

{$ifndef DELPHI5OROLDER}
function TGnuGettextInstance.dngettext(const szDomain: string; const singular,
  plural: ansistring; Number: Integer): widestring;
begin
  Result:=dngettext (szDomain, ansi2wideDTCP(singular), ansi2wideDTCP(plural), Number);
end;
{$endif}

{ TClassMode }

constructor TClassMode.Create;
begin
  PropertiesToIgnore:=TStringList.Create;
  PropertiesToIgnore.Sorted:=True;
  PropertiesToIgnore.Duplicates:=dupError;
  {$ifndef DELPHI5OROLDER}
  PropertiesToIgnore.CaseSensitive:=False;
  {$endif}
end;

destructor TClassMode.Destroy;
begin
  FreeAndNil (PropertiesToIgnore);
  inherited;
end;

{ TFileLocator }

procedure TFileLocator.Analyze;
var
  s:ansistring;
  i:integer;
  offset:int64;
  fs:TFileStream;
  fi:TEmbeddedFileInfo;
  filename:string;
begin
  s:='6637DB2E-62E1-4A60-AC19-C23867046A89'#0#0#0#0#0#0#0#0;
  s:=copy(s,length(s)-7,8);
  offset:=0;
  for i:=8 downto 1 do
    offset:=offset shl 8+ord(s[i]);  
  if offset=0 then
    exit;
  BaseDirectory:=ExtractFilePath(ExecutableFilename);
  try
    fs:=TFileStream.Create(ExecutableFilename,fmOpenRead or fmShareDenyNone);
    try
      while true do begin
        fs.Seek(offset,soFromBeginning);
        offset:=ReadInt64(fs);
        if offset=0 then
          exit;
        fi:=TEmbeddedFileInfo.Create;
        try
          fi.Offset:=ReadInt64(fs);
          fi.Size:=ReadInt64(fs);
          SetLength (filename, offset-fs.position);
          fs.ReadBuffer (filename[1],offset-fs.position);
          filename:=trim(filename);
          if PreferExternal and sysutils.fileexists(basedirectory+filename) then begin
            // Disregard the internal version and use the external version instead
            FreeAndNil (fi);
          end else
            filelist.AddObject(filename,fi);
        except
          FreeAndNil (fi);
          raise;
        end;
      end;
    finally
      FreeAndNil (fs);
    end;
  except
    {$ifdef DXGETTEXTDEBUG}
    raise;
    {$endif}
  end;
end;

constructor TFileLocator.Create;
begin
  MoFilesCS:=TMultiReadExclusiveWriteSynchronizer.Create;
  MoFiles:=TStringList.Create;
  filelist:=TStringList.Create;
  {$ifdef LINUX}
  filelist.Duplicates:=dupError;
  filelist.CaseSensitive:=True;
  {$endif}
  MoFiles.Sorted:=True;
  {$ifndef DELPHI5OROLDER}
  MoFiles.Duplicates:=dupError;
  MoFiles.CaseSensitive:=False;
  {$ifdef MSWINDOWS}
  filelist.Duplicates:=dupError;
  filelist.CaseSensitive:=False;
  {$endif}
  {$endif}
  filelist.Sorted:=True;
end;

destructor TFileLocator.Destroy;
begin
  while filelist.count<>0 do begin
    filelist.Objects[0].Free;
    filelist.Delete (0);
  end;
  FreeAndNil (filelist);
  FreeAndNil (MoFiles);
  FreeAndNil (MoFilesCS);
  inherited;
end;

function TFileLocator.FileExists(filename: string): boolean;
var
  idx:integer;
begin
  if copy(filename,1,length(basedirectory))=basedirectory then begin
    // Cut off basedirectory if the file is located beneath that base directory
    filename:=copy(filename,length(basedirectory)+1,maxint);
  end;
  Result:=filelist.Find(filename,idx);
end;

function TFileLocator.GetMoFile(filename: string; DebugLogger:TDebugLogger): TMoFile;
var
  fi:TEmbeddedFileInfo;
  idx:integer;
  idxname:string;
  Offset, Size: Int64;
  realfilename:string;
begin
  // Find real filename
  offset:=0;
  size:=0;
  realfilename:=filename;
  if copy(filename,1,length(basedirectory))=basedirectory then begin
    filename:=copy(filename,length(basedirectory)+1,maxint);
    idx:=filelist.IndexOf(filename);
    if idx<>-1 then begin
      fi:=filelist.Objects[idx] as TEmbeddedFileInfo;
      realfilename:=ExecutableFilename;
      offset:=fi.offset;
      size:=fi.size;
      {$ifdef DXGETTEXTDEBUG}
      DebugLogger ('Instead of '+filename+', using '+realfilename+' from offset '+IntTostr(offset)+', size '+IntToStr(size));
      {$endif}
    end;
  end;


  {$ifdef DXGETTEXTDEBUG}
  DebugLogger ('Reading .mo data from file '''+filename+'''');
  {$endif}

  // Find TMoFile object
  MoFilesCS.BeginWrite;
  try
    idxname:=realfilename+' //\\ '+IntToStr(offset);
    if MoFiles.Find(idxname, idx) then begin
      Result:=MoFiles.Objects[idx] as TMoFile;
    end else begin
      Result:=TMoFile.Create (realfilename, Offset, Size);
      MoFiles.AddObject(idxname, Result);
    end;
    Inc (Result.Users);
  finally
    MoFilesCS.EndWrite;
  end;
end;

function TFileLocator.ReadInt64(str: TStream): int64;
begin
  Assert (sizeof(Result)=8);
  str.ReadBuffer(Result,8);
end;

procedure TFileLocator.ReleaseMoFile(mofile: TMoFile);
var
  i:integer;
begin
  Assert (mofile<>nil);
  
  MoFilesCS.BeginWrite;
  try
    dec (mofile.Users);
    if mofile.Users<=0 then begin
      i:=MoFiles.Count-1;
      while i>=0 do begin
        if MoFiles.Objects[i]=mofile then begin
          MoFiles.Delete(i);
          FreeAndNil (mofile);
          break;
        end;
        dec (i);
      end;
    end;
  finally
    MoFilesCS.EndWrite;
  end;
end;

{ TTP_Retranslator }

constructor TTP_Retranslator.Create;
begin
  list:=TList.Create;
end;

destructor TTP_Retranslator.Destroy;
var
  i:integer;
begin
  for i:=0 to list.Count-1 do
    TObject(list.Items[i]).Free;
  FreeAndNil (list);
  inherited;
end;

procedure TTP_Retranslator.Execute;
var
  i:integer;
  sl:TStrings;
  item:TTP_RetranslatorItem;
  newvalue:WideString;
  comp:TGnuGettextComponentMarker;
  {$ifndef DELPHI5OROLDER}
  ppi:PPropInfo;
  {$endif}
begin
  for i:=0 to list.Count-1 do begin
    item:=TObject(list.items[i]) as TTP_RetranslatorItem;
    if item.obj is TComponent then begin
      comp:=TComponent(item.obj).FindComponent('GNUgettextMarker') as TGnuGettextComponentMarker;
      if Assigned(comp) and (self<>comp.Retranslator) then begin
        comp.Retranslator.Execute; 
        Continue;
      end;
    end;
    if item.obj is TStrings then begin
      // Since we don't know the order of items in sl, and don't have
      // the original .Objects[] anywhere, we cannot anticipate anything
      // about the current sl.Strings[] and sl.Objects[] values. We therefore
      // have to discard both values. We can, however, set the original .Strings[]
      // value into the list and retranslate that.
      sl:=TStringList.Create;
      try
        sl.Text:=item.OldValue;
        Instance.TranslateStrings(sl,textdomain);
        (item.obj as TStrings).BeginUpdate;
        try
          (item.obj as TStrings).Text:=sl.Text;
        finally
          (item.obj as TStrings).EndUpdate;
        end;
      finally
        FreeAndNil (sl);
      end;
    end else begin
      newValue:=instance.dgettext(textdomain,item.OldValue);
      {$ifdef DELPHI5OROLDER}
      SetStrProp(item.obj, item.PropName, newValue);
      {$endif}
      {$ifndef DELPHI5OROLDER}
      ppi:=GetPropInfo(item.obj, item.Propname);
      if ppi<>nil then begin
        SetWideStrProp(item.obj, ppi, newValue);
      end else begin
        {$ifdef DXGETTEXTDEBUG}
        Instance.DebugWriteln ('ERROR: On retranslation, property disappeared: '+item.Propname+' for object of type '+item.obj.ClassName);
        {$endif}
      end;
      {$endif}
    end;
  end;
end;

procedure TTP_Retranslator.Remember(obj: TObject; PropName: String;
  OldValue: WideString);
var
  item:TTP_RetranslatorItem;
begin
  item:=TTP_RetranslatorItem.Create;
  item.obj:=obj;
  item.Propname:=Propname;
  item.OldValue:=OldValue;
  list.Add(item);
end;

{ TGnuGettextComponentMarker }

destructor TGnuGettextComponentMarker.Destroy;
begin
  FreeAndNil (Retranslator);
  inherited;
end;

{ THook }

constructor THook.Create(OldProcedure, NewProcedure: pointer; FollowJump:boolean=false);
{ Idea and original code from Igor Siticov }
{ Modified by Jacques Garcia Vazquez and Lars Dybdahl }
begin
  {$ifndef CPU386}
  raise Exception.Create ('This procedure only works on Intel i386 compatible processors.');
  {$endif}

  oldproc:=OldProcedure;
  newproc:=NewProcedure;

  Reset (FollowJump);
end;

destructor THook.Destroy;
begin
  Shutdown;
  inherited;
end;

procedure THook.Disable;
begin
  Assert (PatchPosition<>nil,'Patch position in THook was nil when Disable was called');
  PatchPosition[0]:=Original[0];
  PatchPosition[1]:=Original[1];
  PatchPosition[2]:=Original[2];
  PatchPosition[3]:=Original[3];
  PatchPosition[4]:=Original[4];
end;

procedure THook.Enable;
begin
  Assert (PatchPosition<>nil,'Patch position in THook was nil when Enable was called');
  PatchPosition[0]:=Patch[0];
  PatchPosition[1]:=Patch[1];
  PatchPosition[2]:=Patch[2];
  PatchPosition[3]:=Patch[3];
  PatchPosition[4]:=Patch[4];
end;

procedure THook.Reset(FollowJump: boolean);
var
  offset:integer;
  {$ifdef LINUX}
  p:pointer;
  pagesize:integer;
  {$endif}
  {$ifdef MSWindows}
  ov: cardinal;
  {$endif}
begin
  if PatchPosition<>nil then
    Shutdown;

  patchPosition := OldProc;
  if FollowJump and (Word(OldProc^) = $25FF) then begin
    // This finds the correct procedure if a virtual jump has been inserted
    // at the procedure address
    Inc(Integer(patchPosition), 2); // skip the jump
    patchPosition := pChar(Pointer(pointer(patchPosition)^)^);
  end;
  offset:=integer(NewProc)-integer(pointer(patchPosition))-5;

  Patch[0] := char($E9);
  Patch[1] := char(offset and 255);
  Patch[2] := char((offset shr 8) and 255);
  Patch[3] := char((offset shr 16) and 255);
  Patch[4] := char((offset shr 24) and 255);

  Original[0]:=PatchPosition[0];
  Original[1]:=PatchPosition[1];
  Original[2]:=PatchPosition[2];
  Original[3]:=PatchPosition[3];
  Original[4]:=PatchPosition[4];

  {$ifdef MSWINDOWS}
  if not VirtualProtect(Pointer(PatchPosition), 5, PAGE_EXECUTE_READWRITE, @ov) then
    RaiseLastOSError;
  {$endif}
  {$ifdef LINUX}
  pageSize:=sysconf (_SC_PAGE_SIZE);
  p:=pointer(PatchPosition);
  p:=pointer((integer(p) + PAGESIZE-1) and not (PAGESIZE-1) - pageSize);
  if mprotect (p, pageSize, PROT_READ + PROT_WRITE + PROT_EXEC) <> 0 then
    RaiseLastOSError;
  {$endif}
end;

procedure THook.Shutdown;
begin
  Disable;
  PatchPosition:=nil;
end;

procedure HookIntoResourceStrings (enabled:boolean=true; SupportPackages:boolean=false);
begin
  HookLoadResString.Reset (SupportPackages);
  HookLoadStr.Reset (SupportPackages);
  HookFmtLoadStr.Reset (SupportPackages);
  if enabled then begin
    HookLoadResString.Enable;
    HookLoadStr.Enable;
    HookFmtLoadStr.Enable;
  end;
end;

{ TMoFile }

function TMoFile.autoswap32(i: cardinal): cardinal;
var
  cnv1, cnv2:
    record
      case integer of
        0: (arr: array[0..3] of byte);
        1: (int: cardinal);
    end;
begin
  if doswap then begin
    cnv1.int := i;
    cnv2.arr[0] := cnv1.arr[3];
    cnv2.arr[1] := cnv1.arr[2];
    cnv2.arr[2] := cnv1.arr[1];
    cnv2.arr[3] := cnv1.arr[0];
    Result := cnv2.int;
  end else
    Result := i;
end;

function TMoFile.CardinalInMem(baseptr: PChar; Offset: Cardinal): Cardinal;
var pc:^Cardinal;
begin
  inc (baseptr,offset);
  pc:=Pointer(baseptr);
  Result:=pc^;
  if doswap then
    autoswap32(Result);
end;

constructor TMoFile.Create(filename: string; Offset,Size:int64);
var
  i:cardinal;
  nn:integer;
  {$ifdef linux}
  mofile:TFileStream;
  {$endif}
begin
  if sizeof(i) <> 4 then
    raise EGGProgrammingError.Create('TDomain in gnugettext is written for an architecture that has 32 bit integers.');

  {$ifdef mswindows}
  // Map the mo file into memory and let the operating system decide how to cache
  mo:=createfile (PChar(filename),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,0,0);
  if mo=INVALID_HANDLE_VALUE then
    raise EGGIOError.Create ('Cannot open file '+filename);
  momapping:=CreateFileMapping (mo, nil, PAGE_READONLY, 0, 0, nil);
  if momapping=0 then
    raise EGGIOError.Create ('Cannot create memory map on file '+filename);
  momemoryHandle:=MapViewOfFile (momapping,FILE_MAP_READ,0,0,0);
  if momemoryHandle=nil then begin
    raise EGGIOError.Create ('Cannot map file '+filename+' into memory. Reason: '+GetLastWinError);
  end;
  momemory:=momemoryHandle+offset;
  {$endif}
  {$ifdef linux}
  // Read the whole file into memory
  mofile:=TFileStream.Create (filename, fmOpenRead or fmShareDenyNone);
  try
    if size=0 then
      size:=mofile.Size;
    Getmem (momemoryHandle,size);
    momemory:=momemoryHandle;
    mofile.Seek(offset,soFromBeginning);
    mofile.ReadBuffer(momemory^,size);
  finally
    FreeAndNil (mofile);
  end;
  {$endif}

  // Check the magic number
  doswap:=False;
  i:=CardinalInMem(momemory,0);
  if (i <> $950412DE) and (i <> $DE120495) then
    raise EGGIOError.Create('This file is not a valid GNU gettext mo file: ' + filename);
  doswap := (i = $DE120495);


  // Find the positions in the file according to the file format spec
  CardinalInMem(momemory,4);       // Read the version number, but don't use it for anything.
  N:=CardinalInMem(momemory,8);    // Get string count
  O:=CardinalInMem(momemory,12);   // Get offset of original strings
  T:=CardinalInMem(momemory,16);   // Get offset of translated strings

  // Calculate start conditions for a binary search
  nn := N;
  startindex := 1;
  while nn <> 0 do begin
    nn := nn shr 1;
    startindex := startindex shl 1;
  end;
  startindex := startindex shr 1;
  startstep := startindex shr 1;
end;

destructor TMoFile.Destroy;
begin
  {$ifdef mswindows}
  UnMapViewOfFile (momemoryHandle);
  CloseHandle (momapping);
  CloseHandle (mo);
  {$endif}
  {$ifdef linux}
  FreeMem (momemoryHandle);
  {$endif}
  inherited;
end;

function TMoFile.gettext(const msgid: ansistring;var found:boolean): ansistring;
var
  i, step: cardinal;
  offset, pos: cardinal;
  CompareResult:integer;
  msgidptr,a,b:PChar;
  abidx:integer;
  size, msgidsize:integer;
begin
  found:=false;
  msgidptr:=PChar(msgid);
  msgidsize:=length(msgid);

  // Do binary search
  i:=startindex;
  step:=startstep;
  while true do begin
    // Get string for index i
    pos:=O+8*(i-1);
    offset:=CardinalInMem (momemory,pos+4);
    size:=CardinalInMem (momemory,pos);
    a:=msgidptr;
    b:=momemory+offset;
    abidx:=size;
    if msgidsize<abidx then
      abidx:=msgidsize;
    CompareResult:=0;
    while abidx<>0 do begin
      CompareResult:=integer(byte(a^))-integer(byte(b^));
      if CompareResult<>0 then
        break;
      dec (abidx);
      inc (a);
      inc (b);
    end;
    if CompareResult=0 then 
      CompareResult:=msgidsize-size;
    if CompareResult=0 then begin  // msgid=s
      // Found the msgid
      pos:=T+8*(i-1);
      offset:=CardinalInMem (momemory,pos+4);
      size:=CardinalInMem (momemory,pos);
      SetString (Result,momemory+offset,size);
      found:=True;
      break;
    end;
    if step=0 then begin
      // Not found
      Result:=msgid;
      break;
    end;
    if CompareResult<0 then begin  // msgid<s
      if i < 1+step then
        i := 1
      else
        i := i - step;
      step := step shr 1;
    end else begin  // msgid>s
      i := i + step;
      if i > N then
        i := N;
      step := step shr 1;
    end;
  end;
end;

var
  param0:string;

initialization
  {$ifdef DXGETTEXTDEBUG}
  {$ifdef MSWINDOWS}
  MessageBox (0,'gnugettext.pas debugging is enabled. Turn it off before releasing this piece of software.','Information',MB_OK);
  {$endif}
  {$ifdef LINUX}
  writeln (stderr,'gnugettext.pas debugging is enabled. Turn it off before releasing this piece of software.');
  {$endif}
  {$endif}
  {$ifdef FPC}
    {$ifdef LINUX}
      SetLocale(LC_ALL, '');
      SetCWidestringManager;
    {$endif LINUX}
  {$endif FPC}
  if IsLibrary then begin
    // Get DLL/shared object filename
    SetLength (ExecutableFilename,300);
    {$ifdef MSWINDOWS}
    SetLength (ExecutableFilename,GetModuleFileName(FindClassHInstance(TGnuGettextInstance), PChar(ExecutableFilename), length(ExecutableFilename)));
    {$else}
    SetLength (ExecutableFilename,GetModuleFileName(0, PChar(ExecutableFilename), length(ExecutableFilename)));
    {$endif}
  end else
    ExecutableFilename:=Paramstr(0);
  FileLocator:=TFileLocator.Create;
  FileLocator.Analyze;
  ResourceStringDomainList:=TStringList.Create;
  ResourceStringDomainList.Add(DefaultTextDomain);
  ResourceStringDomainListCS:=TMultiReadExclusiveWriteSynchronizer.Create;
  DefaultInstance:=TGnuGettextInstance.Create;
  {$ifdef MSWINDOWS}
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
  {$endif}

  // replace Borlands LoadResString with gettext enabled version:
  HookLoadResString:=THook.Create (@system.LoadResString, @LoadResStringA);
  HookLoadStr:=THook.Create (@sysutils.LoadStr, @SysUtilsLoadStr);
  HookFmtLoadStr:=THook.Create (@sysutils.FmtLoadStr, @SysUtilsFmtLoadStr);
  param0:=lowercase(extractfilename(paramstr(0)));
  if (param0<>'delphi32.exe') and (param0<>'kylix') and (param0<>'bds.exe') then
    HookIntoResourceStrings (AutoCreateHooks,false);
  param0:='';

finalization
  FreeAndNil (DefaultInstance);
  FreeAndNil (ResourceStringDomainListCS);
  FreeAndNil (ResourceStringDomainList);
  FreeAndNil (HookFmtLoadStr);
  FreeAndNil (HookLoadStr);
  FreeAndNil (HookLoadResString);
  FreeAndNil (FileLocator);

end.

