{-----------------------------------------------------------------------------
 Unit Name: uHighlighterProcs
 Author:    PyScripter
 Date:      06-Jul-2023
 Purpose:   THighlighterList class and highlighter support routines
 History:
-----------------------------------------------------------------------------}

unit uHighlighterProcs;

interface

uses
  System.Classes,
  Generics.Defaults,
  Generics.Collections,
  SynEditHighlighter;

type
  THighlighterList = class(TList<TSynCustomHighlighter>)
    function FileFilters: string;
    procedure GetHighlighters(AOwner: TComponent; AppendToList: Boolean = True);
    function HighlighterFromFriendlyName(const FriendlyName: string): TSynCustomHighlighter;
    function HighlighterFromName(const Name: string): TSynCustomHighlighter;
    function HighlighterFromClass(HighlighterClass: TSynCustomHighlighterClass):
      TSynCustomHighlighter;
    function HighlighterFromFileExt(const FileExt: string): TSynCustomHighlighter;
    function HighlighterFromFileName(const FileName: string): TSynCustomHighlighter;
  end;

function FileMaskFromFileFilter(const Filter : string) : string;
function DefaultExtensionFromFilter(const Filter : string) : string;
function FileExtInFileFilter(FileExt, FileFilter: string): Boolean;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  JvGNUGetText;

function FileMaskFromFileFilter(const Filter : string) : string;
Var
  j : integer;
begin
  Result := '';
  j := Pos('|', Filter);
  if j > 0 then begin
    Result := Filter;
    Delete(Result, 1, j);
  end;
end;

function DefaultExtensionFromFilter(const Filter : string) : string;
begin
  var Mask := FileMaskFromFileFilter(Filter);
  var Len := Mask.Length;
  var I := 1;
  while (I <= Len) and CharInSet(Mask[I], ['*', '.']) do
    Inc(I);
  var J := I;
  while (J <= Len) and not CharInSet(Mask[J], [';', '|'])  do
    Inc(J);
  Result := Copy(Mask, I, J - I);
end;

function FileExtInFileFilter(FileExt, FileFilter: string): Boolean;
var
  j, ExtLen: Integer;
begin
  Result := False;
  ExtLen := FileExt.Length;
  if ExtLen = 0 then
    Exit;
  FileExt := LowerCase(FileExt);
  FileFilter := LowerCase(FileFilter);
  j := Pos('|', FileFilter);
  if j > 0 then begin
    Delete(FileFilter, 1, j);
    j := Pos(FileExt, FileFilter);
    if (j > 0) and
       ((j + ExtLen > Length(FileFilter)) or (FileFilter[j + ExtLen] = ';'))
    then
      Exit(True);
  end;
end;

{ TSynHighlighterList }

function THighlighterList.FileFilters: string;
begin
  Result := '';
  for var Highlighter in Self do
  begin
    if Highlighter.DefaultFilter = '' then
      Continue;
    Result := Result + _(Highlighter.DefaultFilter);
    if not Result.EndsWith('|') then
      Result := Result + '|';
  end;
end;

procedure THighlighterList.GetHighlighters(AOwner: TComponent;
  AppendToList: Boolean);
begin
  if not AppendToList then
    Clear;
  if Assigned(AOwner) then
    for var Component in AOwner do
      if (Component is TSynCustomHighlighter) and
        not Contains(TSynCustomHighlighter(Component))
      then
        Add(TSynCustomHighlighter(Component))
      else
        Continue;
  //Sort alphabetically
  Sort(TComparer<TSynCustomHighlighter>.Construct(
    function(const Left, Right: TSynCustomHighlighter): Integer
    begin
      Result := AnsiCompareText(Left.FriendlyLanguageName,
        Right.FriendlyLanguageName);
    end));
end;

function THighlighterList.HighlighterFromClass(
  HighlighterClass: TSynCustomHighlighterClass): TSynCustomHighlighter;
begin
  for var Highlighter in Self do
    if Highlighter is HighlighterClass then
      Exit(Highlighter);
  Result := nil;
end;

function THighlighterList.HighlighterFromFriendlyName(
  const FriendlyName: string): TSynCustomHighlighter;
begin
  for var Highlighter in Self do
    if Highlighter.FriendlyLanguageName = FriendlyName then
      Exit(Highlighter);
  Result := nil;
end;

function THighlighterList.HighlighterFromName(const Name: string):
  TSynCustomHighlighter;
begin
  for var Highlighter in Self do
    if Highlighter.LanguageName = Name then
      Exit(Highlighter);
  Result := nil;
end;

function THighlighterList.HighlighterFromFileExt(
  const FileExt: string): TSynCustomHighlighter;
begin
  if FileExt <> '' then
    for var Highlighter in Self do
      if FileExtInFileFilter(FileExt, Highlighter.DefaultFilter) then
        Exit(Highlighter);
  Result := nil;
end;

function THighlighterList.HighlighterFromFileName(
  const FileName: string): TSynCustomHighlighter;
begin
  Result := HighlighterFromFileExt(TPath.GetExtension(FileName));
end;

end.

