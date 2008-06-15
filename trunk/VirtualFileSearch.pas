unit VirtualFileSearch;

// Version 2.2.1
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
//----------------------------------------------------------------------------

interface
{$define TNTSUPPORT}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  ActiveX,
  ShlObj,
  MPCommonUtilities,
  MPThreadManager,
  {$IFDEF TNTSUPPORT}
  TntClasses,
  TntSysUtils,
  TntWideStrUtils,
  TntWindows,
  {$ENDIF}
  MPShellTypes,
  ExtCtrls,
  MPShellUtilities,
  VirtualResources,
  MPCommonObjects;

type
  TVirtualFileSearch = class;  // Forward

  TVirtualSearchAttrib = (
      vsaArchive,    // File is an archive file
      vsaCompressed, // File is a compressed file
      vsaEncrypted,  // File is a encrypted file
      vsaHidden,     // File is a hidden file
      vsaNormal,     // File is a normal file
      vsaOffline,    // File is an offline file
      vsaReadOnly,   // File is a Read Only file
      vsaSystem,     // File is a system file
      vsaTemporary,   // File is a temporary file
      vsaDirectory   // File is a Directory
    );
    TVirtualSearchAttribs = set of TVirtualSearchAttrib;
  TFileSearchProgressEvent = procedure(Sender: TObject; Results: TCommonPIDLList; var Handled: Boolean) of object;
  TFileSearchFinishedEvent = procedure(Sender: TObject; Results: TCommonPIDLList) of object;
   // WARNING CALLED IN CONTEXT OF THREAD
  {$IFDEF TNTSUPPORT}
  TFileSearchCompareEvent = procedure(Sender: TObject; const FilePath: WideString; FindFileData: TWIN32FindDataW; var UseFile: Boolean) of object;
  {$ELSE}
  TFileSearchCompareEvent = procedure(Sender: TObject; const FilePath: WideString; FindFileData: TWIN32FindDataA; var UseFile: Boolean) of object;
  {$ENDIF}
  TVirtualFileSearchThread = class(TCommonThread)
  private
    FCaseSensitive: Boolean;
    FFileMask: DWORD;      // The mask of attributes of a file to use in the search
//    FSearchCriteriaContent: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF};
    FSearchCriteriaFileName: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF};
    FSearchPaths: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF};
    FSearchManager: TVirtualFileSearch;

    FSearchResults: TCommonPIDLList;
    //FTemporary: Boolean;
    FFileExcludeMask: DWORD;
    FSubFolders: Boolean;
  protected
    procedure Execute; override;
    procedure ProcessFiles(Path: WideString; Masks: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF});
    property SearchManager: TVirtualFileSearch read FSearchManager write FSearchManager;
    //property Temporary: Boolean read FTemporary write FTemporary;
  public
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;
    procedure BuildFolderList(const Path: WideString; FolderList: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF});
    property Event;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property FileMask: DWORD read FFileMask write FFileMask;
    property FileExcludeMask: DWORD read FFileExcludeMask write FFileExcludeMask;
    property SubFolders: Boolean read FSubFolders write FSubFolders;
//    property SearchCriteriaContent: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF} read FSearchCriteriaContent write FSearchCriteriaContent;
    property SearchCriteriaFileName: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF} read FSearchCriteriaFileName write FSearchCriteriaFileName;
    property SearchPaths: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF} read FSearchPaths write FSearchPaths;
    property SearchResults: TCommonPIDLList read FSearchResults write FSearchResults;
  end;

  TVirtualFileSearch = class(TComponent)
  private
    FCaseSensitive: Boolean;
    FFileFindThread: TVirtualFileSearchThread;
    FFinished: Boolean;
    FOnProgress: TFileSearchProgressEvent;
    FOnSearchCompare: TFileSearchCompareEvent;  // WARNING CALLED IN CONTEXT OF THREAD
    FOnSearchEnd: TFileSearchFinishedEvent;
    FSearchAttribs: TVirtualSearchAttribs;
    {$IFDEF TNTSUPPORT}
  //  FSearchCriteriaContent: TTntStringList;
    FSearchCriteriaFilename: TTntStringList;
    FSearchPath: TTntStringList;
    {$ELSE}
//    FSearchCriteriaContent: TStringList;
    FSearchCriteriaFilename: TStringList;
    FSearchPath: TStringList;
    {$ENDIF}
    FSubFolders: Boolean;
    FThreadPriority: TThreadPriority;
    FTimer: TTimer;
    FUpdateRate: Integer;
    FSearchExcludeAttribs: TVirtualSearchAttribs;
  protected
    function BuildMask(Attribs : TVirtualSearchAttribs): Integer;
    procedure DoProgress(Results: TCommonPIDLList; var Handled: Boolean); virtual;
    {$IFDEF TNTSUPPORT}
    procedure DoSearchCompare(const FilePath: WideString; FindFileData: TWIN32FindDataW; var UseFile: Boolean);
    {$ELSE}
    procedure DoSearchCompare(const FilePath: WideString; FindFileData: TWIN32FindDataA; var UseFile: Boolean);
    {$ENDIF}
    procedure DoSearchEnd(Results: TCommonPIDLList);
    procedure TimerTick(Sender: TObject);
    property FileFindThread: TVirtualFileSearchThread read FFileFindThread write FFileFindThread;
    property Timer: TTimer read FTimer write FTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Run: Boolean; virtual;
    // Runs and waits until the search is finished.  OnProgress events are not called
    procedure RunAndWait; virtual;
    procedure Stop; virtual;
    property Finished: Boolean read FFinished;
    {$IFDEF TNTSUPPORT}
//    property SearchCriteriaContent: TTntStringList read FSearchCriteriaContent write FSearchCriteriaContent;
    property SearchCriteriaFilename: TTntStringList read FSearchCriteriaFilename write FSearchCriteriaFilename;
    property SearchPaths: TTntStringList read FSearchPath write FSearchPath;
    {$ELSE}
//    property SearchCriteriaContent: TStringList read FSearchCriteriaContent write FSearchCriteriaContent;
    property SearchCriteriaFilename: TStringList read FSearchCriteriaFilename write FSearchCriteriaFilename;
    property SearchPaths: TStringList read FSearchPath write FSearchPath;
    {$ENDIF}
  published
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property OnProgress: TFileSearchProgressEvent read FOnProgress write FOnProgress;
     // WARNING CALLED IN CONTEXT OF THREAD
    property OnSearchCompare: TFileSearchCompareEvent read FOnSearchCompare write FOnSearchCompare;
    property OnSearchEnd: TFileSearchFinishedEvent read FOnSearchEnd write FOnSearchEnd;
    property SearchAttribs: TVirtualSearchAttribs read FSearchAttribs write FSearchAttribs default [vsaArchive, vsaCompressed, vsaEncrypted, vsaHidden, vsaNormal, vsaOffline, vsaReadOnly, vsaSystem, vsaTemporary];
    property SearchExcludeAttribs: TVirtualSearchAttribs read FSearchExcludeAttribs write FSearchExcludeAttribs default [vsaDirectory];
    property SubFolders: Boolean read FSubFolders write FSubFolders default False;
    property ThreadPriority: TThreadPriority read FThreadPriority write FThreadPriority default tpNormal;
    property UpdateRate: Integer read FUpdateRate write FUpdateRate default 5000;
  end;

implementation
Uses
  WideStrUtils;

{ TVirtualFileSearchThread }
constructor TVirtualFileSearchThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  {$IFDEF TNTSUPPORT}
  SearchPaths := TTntStringList.Create;
  SearchCriteriaFileName := TTntStringList.Create;
//  SearchCriteriaContent := TTntStringList.Create;
  {$ELSE}
  SearchPaths := TStringList.Create;
  SearchCriteriaFileName := TStringList.Create;
//  SearchCriteriaContent := TStringList.Create;
  {$ENDIF}
  SearchResults := TCommonPIDLList.Create;
end;

destructor TVirtualFileSearchThread.Destroy;
begin
  SearchPaths.Free;
  SearchCriteriaFileName.Free;
//  SearchCriteriaContent.Free;
  SearchResults.Free;
  inherited Destroy;
end;

procedure TVirtualFileSearchThread.BuildFolderList(const Path: WideString;
  FolderList: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF});
//
// Builds a list of folders that are contained in the Path
//
var
  FindHandle: THandle;
  {$IFDEF TNTSUPPORT}
  FindFileDataW: TWIN32FindDataW;
  {$ELSE}
  FindFileDataA: TWIN32FindDataA;
  {$ENDIF}   
begin
  {$IFDEF TNTSUPPORT}
  FindHandle := Tnt_FindFirstFileW(PWideChar(WideString( Path + '\*.*')), FindFileDataW);
  if FindHandle <> INVALID_HANDLE_VALUE then
  begin
    repeat
      // Recurse SubFolder if desired, don't get into an endless loop with ReparsePoints
      if not Terminated and
        ((FFileMask and FILE_ATTRIBUTE_DIRECTORY <> 0) and
         (FindFileDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
         (FindFileDataW.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT = 0)
        ) then begin
        // This is very ugly but it saves from testing filename lengths
        if FindFileDataW.cFileName[0] <> #0 then
        begin
          if FindFileDataW.cFileName[1] = #0 then
          begin
            // One character long name, if '.' then skip it
            if FindFileDataW.cFileName[0] <> '.' then
              FolderList.Add(Path + '\' + FindFileDataW.cFileName)
          end else
          begin
            if FindFileDataW.cFileName[2] = #0 then
            begin
              // Two character long name, if '..' then skip it
              if not ((FindFileDataW.cFileName[0] = '.') and (FindFileDataW.cFileName[1] = '.')) then
                FolderList.Add(Path + '\' + FindFileDataW.cFileName)
            end else
              FolderList.Add(Path + '\' + FindFileDataW.cFileName)
          end
        end
      end
    until Terminated or not Tnt_FindNextFileW(FindHandle, FindFileDataW);
    Windows.FindClose(FindHandle);
  end
  {$ELSE}
  FindHandle := FindFirstFileA(PChar(string( Path + '\*.*')), FindFileDataA);
  if FindHandle <> INVALID_HANDLE_VALUE then
  begin
    repeat
      // Recurse SubFolder if desired, don't get into an endless loop with ReparsePoints
      if not Terminated and
        ((FFileMask and FILE_ATTRIBUTE_DIRECTORY <> 0) and
         (FindFileDataA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
         (FindFileDataA.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT = 0)
        ) then begin
        // This is very ugly but it saves from testing filename lengths
        if FindFileDataA.cFileName[0] <> #0 then
        begin
          if FindFileDataA.cFileName[1] = #0 then
          begin
            // One character long name, if '.' then skip it
            if FindFileDataA.cFileName[0] <> '.' then
              FolderList.Add(Path + '\' + FindFileDataA.cFileName)
          end else
          begin
            if FindFileDataA.cFileName[2] = #0 then
            begin
              // Two character long name, if '..' then skip it
              if not ((FindFileDataA.cFileName[0] = '.') and (FindFileDataA.cFileName[1] = '.')) then
                FolderList.Add(Path + '\' + FindFileDataA.cFileName)
            end else
              FolderList.Add(Path + '\' + FindFileDataA.cFileName)
          end
        end
      end
    until Terminated or not FindNextFileA(FindHandle, FindFileDataA);
    Windows.FindClose(FindHandle);
  end
  {$ENDIF}
end;

procedure TVirtualFileSearchThread.Execute;
var
  i: Integer;
begin
  SearchResults.Clear;
  i := 0;
  while not Terminated and (i < SearchPaths.Count) do
  begin
    if WideDirectoryExists(SearchPaths[i]) then
      ProcessFiles(WideExcludeTrailingBackslash( SearchPaths[i]), SearchCriteriaFileName);
    Inc(i)
  end
end;

procedure TVirtualFileSearchThread.ProcessFiles(Path: WideString;
  Masks: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF});
var
  {$IFDEF TNTSUPPORT}
  FindFileDataW: TWIN32FindDataW;
  {$ENDIF}
  FindFileDataA: TWIN32FindDataA;
  FolderList: {$IFDEF TNTSUPPORT}TTntStringList{$ELSE}TStringList{$ENDIF};
  FindHandle: THandle;
  i: Integer;
  UseFile, Done: Boolean;
  PIDL: PItemIDList;
  CurrentPath, CurrentPathSpec: WideString;
  IsDotPath, IsDotDotPath: Boolean;
begin
  i := 0;
  {$IFDEF TNTSUPPORT}
  FolderList := TTntStringList.Create;
  {$ELSE}
  FolderList := TStringList.Create;
  {$ENDIF}
  try
    while not Terminated and (i < Masks.Count) do
    begin
      // Find all files in the folder
      CurrentPathSpec := Path + '\*.*';

     {$IFDEF TNTSUPPORT}
      if IsUnicode then
        FindHandle := Tnt_FindFirstFileW(PWideChar( CurrentPathSpec), FindFileDataW)
      else begin
        FindHandle := FindFirstFileA(PChar(String( CurrentPathSpec)), FindFileDataA);
        CopyMemory(@FindFileDataW, @FindFileDataA, Integer(@FindFileDataW.cFileName) - Integer(@FindFileDataW));
        WStrPCopy(FindFileDataW.cFileName, FindFileDataA.cFileName);
        WStrPCopy(FindFileDataW.cAlternateFileName, FindFileDataA.cAlternateFileName);
      end;
    {$ELSE}
      FindHandle := FindFirstFileA(PChar(String( CurrentPathSpec)), FindFileDataA);
    {$ENDIF}


      if (FindHandle <> INVALID_HANDLE_VALUE) then
      begin
        repeat
          {$IFDEF TNTSUPPORT}

          // .............  ANSII or UNICODE .......................

          // We filled in the FileFileDataW above even if on Win9x
          CurrentPath := Path + '\' + FindFileDataW.cFileName;
          IsDotPath := (WideStrIComp(@FindFileDataW.cFileName, '.') = 0);
          IsDotDotPath := (WideStrIComp(@FindFileDataW.cFileName, '..') = 0);

          if not (IsDotPath or IsDotDotPath) then
          begin

            // Decide if we use the file or not
            UseFile := FileMask and FindFileDataW.dwFileAttributes <> 0;
            UseFile := UseFile and (FileExcludeMask and FindFileDataW.dwFileAttributes = 0);
            if WidePathMatchSpecExists then
              UseFile := UseFile and WidePathMatchSpec(CurrentPath, Masks[i]);

            // Let the program override us
            if UseFile then
              SearchManager.DoSearchCompare(Path, FindFileDataW, UseFile);

            // Using the file
            if UseFile then
            begin
              PIDL := PathToPIDL(CurrentPath);
              if Assigned(PIDL) then
              begin
                LockThread;
                SearchResults.Add(PIDL);
                UnlockThread;
              end
            end;
          end;

          // Add the folders if we are recursing the folder, but only on the first runthrough
          if (i = 0) and SubFolders and (FindFileDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0)
            and not(IsDotPath or IsDotDotPath)
          then
            FolderList.Add(CurrentPath);

          // Get the next file
          if IsUnicode then
            Done := not Tnt_FindNextFileW(FindHandle, FindFileDataW)
          else begin
            Done := not FindNextFileA(FindHandle, FindFileDataA);
            CopyMemory(@FindFileDataW, @FindFileDataA, Integer(@FindFileDataW.cFileName) - Integer(@FindFileDataW));
            WStrPCopy(FindFileDataW.cFileName, FindFileDataA.cFileName);
            WStrPCopy(FindFileDataW.cAlternateFileName, FindFileDataA.cAlternateFileName);
          end;

          {$ELSE}

          // .............  ANSII Only .......................
          CurrentPath := Path + '\' + FindFileDataA.cFileName;
          IsDotPath := (lstrcmpiA(@FindFileDataA.cFileName, '.') = 0);
          IsDotDotPath := (lstrcmpiA(@FindFileDataA.cFileName, '..') = 0);

          if not(IsDotPath or IsDotDotPath) then
          begin

            // Decide if we use the file or not
            UseFile := FileMask and FindFileDataA.dwFileAttributes <> 0;
            UseFile := UseFile and (FileExcludeMask and FindFileDataA.dwFileAttributes = 0);
            if WidePathMatchSpecExists then
              UseFile := UseFile and WidePathMatchSpec(CurrentPath, Masks[i]);

            // Let the program override us
            if UseFile then
              SearchManager.DoSearchCompare(Path, FindFileDataA, UseFile);

            // Using the file
            if UseFile then
            begin
              PIDL := PathToPIDL(CurrentPath);
              if Assigned(PIDL) then
              begin
                LockThread;
                SearchResults.Add(PIDL);
                UnlockThread;
              end
            end;
          end;

          // Add the folders if we are recursing the folder, but only on the first runthrough
          if (i = 0) and SubFolders and (FindFileDataA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0)
            and not(IsDotPath or IsDotDotPath)
          then
            FolderList.Add(CurrentPath);

          // Get the next file
          Done := not FindNextFileA(FindHandle, FindFileDataA)
          {$ENDIF}

        until Terminated or Done;
        Windows.FindClose(FindHandle);
      end;
      Inc(i)
    end;

    // Recurse into sub folders if necessary
    if SubFolders then
    begin
      for i := 0 to FolderList.Count - 1 do
        ProcessFiles(FolderList[i], Masks)
    end   
  finally
    FolderList.Free
  end
end;

{ TVirtualFileSearch }
constructor TVirtualFileSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Timer := TTimer.Create(Self);
  Timer.OnTimer := TimerTick;
  {$IFDEF TNTSUPPORT}
  SearchPaths := TTntStringList.Create;
  SearchCriteriaFilename := TTntStringList.Create;
 // SearchCriteriaContent := TTntStringList.Create;
  {$ELSE}
  SearchPaths := TStringList.Create;
  SearchCriteriaFilename := TStringList.Create;
 // SearchCriteriaContent := TStringList.Create;
  {$ENDIF}
  FUpdateRate := 5000;
  FThreadPriority := tpLower;
  SearchAttribs := [vsaArchive, vsaCompressed, vsaEncrypted, vsaHidden, vsaNormal, vsaOffline, vsaReadOnly, vsaSystem, vsaTemporary];
  SearchExcludeAttribs := [vsaDirectory];
end;

destructor TVirtualFileSearch.Destroy;
begin
  Stop;
  SearchPaths.Free;
  SearchCriteriaFilename.Free;
//  SearchCriteriaContent.Free;
  inherited Destroy;
end;

function TVirtualFileSearch.BuildMask(Attribs : TVirtualSearchAttribs): Integer;
begin
  Result := 0;
  if vsaReadOnly in Attribs then
    Result := Result or FILE_ATTRIBUTE_READONLY;
  if vsaHidden in Attribs then
    Result := Result or FILE_ATTRIBUTE_HIDDEN;
  if vsaSystem in Attribs then
    Result := Result or FILE_ATTRIBUTE_SYSTEM;
  if vsaDirectory in Attribs then
    Result := Result or FILE_ATTRIBUTE_DIRECTORY;
  if vsaArchive in Attribs then
    Result := Result or FILE_ATTRIBUTE_ARCHIVE;
  if vsaNormal in Attribs then
    Result := Result or FILE_ATTRIBUTE_NORMAL;
  if vsaCompressed in Attribs then
    Result := Result or FILE_ATTRIBUTE_COMPRESSED;
  if vsaOffline in Attribs then
    Result := Result or FILE_ATTRIBUTE_OFFLINE;
  if vsaTemporary in Attribs then
    Result := Result or FILE_ATTRIBUTE_TEMPORARY;
  if vsaEncrypted in Attribs then
    Result := Result or FILE_ATTRIBUTE_ENCRYPTED;
end;

procedure TVirtualFileSearch.DoProgress(Results: TCommonPIDLList; var Handled: Boolean);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, Results, Handled)
end;

{$IFDEF TNTSUPPORT}
procedure TVirtualFileSearch.DoSearchCompare(const FilePath: WideString;
  FindFileData: TWIN32FindDataW; var UseFile: Boolean);
begin
  // WARNING CALLED IN CONTEXT OF THREAD
  if Assigned(OnSearchCompare) then
    OnSearchCompare(Self, FilePath, FindFileData, UseFile)
end;
{$ELSE}
procedure TVirtualFileSearch.DoSearchCompare(const FilePath: WideString;
  FindFileData: TWIN32FindDataA; var UseFile: Boolean);
begin
  // WARNING CALLED IN CONTEXT OF THREAD
  if Assigned(OnSearchCompare) then
    OnSearchCompare(Self, FilePath, FindFileData, UseFile)
end;
{$ENDIF}


procedure TVirtualFileSearch.DoSearchEnd(Results: TCommonPIDLList);
begin
  if Assigned(OnSearchEnd) then
    OnSearchEnd(Self, Results)
end;

function TVirtualFileSearch.Run: Boolean;
begin
  FFinished := False;
  FileFindThread := TVirtualFileSearchThread.Create(True);
  FileFindThread.SearchManager := Self;
  FileFindThread.SearchPaths.Assign(SearchPaths);
  FileFindThread.SearchCriteriaFileName.Assign(SearchCriteriaFilename);
//  FileFindThread.SearchCriteriaContent.Assign(SearchCriteriaContent);
  FileFindThread.CaseSensitive := CaseSensitive;
  FileFindThread.FileMask := BuildMask(SearchAttribs);
  FileFindThread.FileExcludeMask := BuildMask(SearchExcludeAttribs);
  FIleFindThread.SubFolders := SubFolders;
  FileFindThread.Priority := ThreadPriority;
  FileFindThread.Resume;
  Timer.Interval := UpdateRate;
  Timer.Enabled := True;
  Result := True;
end;

procedure TVirtualFileSearch.RunAndWait;
begin
  FFinished := False;
  FileFindThread := TVirtualFileSearchThread.Create(True);
  FileFindThread.SearchManager := Self;
  FileFindThread.SearchPaths.Assign(SearchPaths);
  FileFindThread.SearchCriteriaFileName.Assign(SearchCriteriaFilename);
//  FileFindThread.SearchCriteriaContent.Assign(SearchCriteriaContent);
  FileFindThread.CaseSensitive := CaseSensitive;
  FileFindThread.FileMask := BuildMask(SearchAttribs);
  FileFindThread.FileExcludeMask := BuildMask(SearchExcludeAttribs);
  FIleFindThread.SubFolders := SubFolders;
  FileFindThread.Priority := ThreadPriority;
  FileFindThread.Resume;
  WaitForSingleObject(FileFindThread.Handle, INFINITE);
  DoSearchEnd(FileFindThread.SearchResults);
  FileFindThread.SearchResults.Clear;
  Stop;
end;

procedure TVirtualFileSearch.Stop;
begin
  if Assigned(FileFindThread) then
  begin
    Timer.Enabled := False;
    FileFindThread.Terminate;
    while not FileFindThread.Finished do
      Sleep(50);
    FreeAndNil(FFileFindThread);
  end;
  FFinished := True;
end;

procedure TVirtualFileSearch.TimerTick(Sender: TObject);
var
  Handled: Boolean;
begin
  if Assigned(FileFindThread) and (FileFindThread.Finished or Assigned(FOnProgress)) then
  // no reason to lock the thread if the above conditions are not met
  begin
    FileFindThread.LockThread;
    try
      Handled := False;
      DoProgress(FileFindThread.SearchResults, Handled);
      if Handled then
        FileFindThread.SearchResults.Clear;
    finally
      if FileFindThread.Finished then
      begin
        Timer.Enabled := False;
        DoSearchEnd(FileFindThread.SearchResults);
        FileFindThread.SearchResults.Clear;
        FFinished := True;
      end;
      FileFindThread.UnlockThread;

      if FileFindThread.Finished then
        FreeAndNil(FFileFindThread);
    end
  end
end;

end.
