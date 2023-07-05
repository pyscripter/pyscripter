{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uHighlighterProcs.pas, released 2000-06-23.

The Initial Author of the Original Code is Michael Hieke.
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

$Id: uHighlighterProcs.pas,v 1.3 2002/06/15 06:57:24 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uHighlighterProcs;

interface

uses
  System.Classes,
  SynEditHighlighter;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStrings;
  AppendToList: boolean);
function GetHighlightersFilter(AHighlighters: TStrings): string;
function GetHighlighterFromFileExt(AHighlighters: TStrings;
  const Extension: string): TSynCustomHighlighter;
function GetHighlighterFromFileName(AHighlighters: TStrings;
  const FileName: string): TSynCustomHighlighter;
function GetHighlighterFromLanguageName(LanguageName : string;
  AHighlighters: TStrings) : TSynCustomHighlighter;
function FileMaskFromFileFilter(const Filter : string) : string;
function DefaultExtensionFromFilter(const Filter : string) : string;
function FileExtInFileFilter(FileExt, FileFilter: string): Boolean;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  JvGNUGetText,
  uCommonFunctions;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStrings;
  AppendToList: boolean);
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
begin
  if Assigned(AOwner) and Assigned(AHighlighters) then begin
    if not AppendToList then
      AHighlighters.Clear;
    for i := AOwner.ComponentCount - 1 downto 0 do begin
      if not (AOwner.Components[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := AOwner.Components[i] as TSynCustomHighlighter;
      // only one highlighter for each language
      if AHighlighters.IndexOf(Highlighter.FriendlyLanguageName) = -1 then
        AHighlighters.AddObject(Highlighter.FriendlyLanguageName, Highlighter);
    end;
  end;
end;

function GetHighlightersFilter(AHighlighters: TStrings): string;
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
begin
  Result := '';
  if Assigned(AHighlighters) then
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      if Highlighter.DefaultFilter = '' then
        continue;
      Result := Result + _(Highlighter.DefaultFilter);
      if Result[Length(Result)] <> '|' then
        Result := Result + '|';
    end;
end;

function GetHighlighterFromFileExt(AHighlighters: TStrings;
  const Extension: string): TSynCustomHighlighter;
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
begin
  if Assigned(AHighlighters) and (Extension.Length > 0) then begin
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      if FileExtInFileFilter(Extension, Highlighter.DefaultFilter) then
        Exit(Highlighter);
    end;
  end;
  Result := nil;
end;

function GetHighlighterFromFileName(AHighlighters: TStrings;
  const FileName: string): TSynCustomHighlighter;
var
  Len: integer;
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  Filter, Mask: string;
begin
  Len := Length(FileName);
  if Assigned(AHighlighters) and (Len > 0) then begin
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      Filter := Trim(LowerCase(Highlighter.DefaultFilter));
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        repeat
          Mask := StrToken(Filter, ';').Trim;
          if (Mask <> '') and TPath.MatchesPattern(TPath.GetFileName(FileName), Mask, False) then
          begin
            Result := Highlighter;
            exit;
          end;
        until Filter = '';
      end;
    end;
  end;
  Result := nil;
end;

function GetHighlighterFromLanguageName(LanguageName : string;
  AHighlighters: TStrings) : TSynCustomHighlighter;
Var
  Index : integer;
begin
  Result := nil;
  Index := AHighlighters.IndexOf(LanguageName);
  if Index >= 0 then
    Result := AHighlighters.Objects[Index] as TSynCustomHighlighter
end;

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


end.

