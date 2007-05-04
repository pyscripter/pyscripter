{-----------------------------------------------------------------------------
 Unit Name: cFileSearch
 Author:    Kiriakos Vlahos
 Date:      29-May-2005
 Purpose:
 History:   Based on GExperts (www.gexperts.org) unit and covered by its licence

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement only covers code written by GExperts, Inc and Erik Berry. You should contact the other authors concerning their respective copyrights and conditions.

The rules governing the use of GExperts and the GExperts source code are derived
from the official Open Source Definition, available at http://www.opensource.org.
The conditions and limitations are as follows:

    * Usage of GExperts binary distributions is permitted for all developers.
      You may not use the GExperts source code to develop proprietary or
      commercial products including plugins or libraries for those products.
      You may use the GExperts source code in an Open Source project, under the
      terms listed below.
    * You may not use the GExperts source code to create and distribute custom
      versions of GExperts under the "GExperts" name. If you do modify and
      distribute custom versions of GExperts, the binary distribution must be
      named differently and clearly marked so users can tell they are not using
      the official GExperts distribution. A visible and unmodified version of
      this license must appear in any modified distribution of GExperts.
    * Custom distributions of GExperts must include all of the custom changes
      as a patch file that can be applied to the original source code. This
      restriction is in place to protect the integrity of the original author's
      source code. No support for modified versions of GExperts will be provided
      by the original authors or on the GExperts mailing lists.
    * All works derived from GExperts must be distributed under a license
      compatible with this license and the official Open Source Definition,
      which can be obtained from http://www.opensource.org/.
    * Please note that GExperts, Inc. and the other contributing authors hereby
      state that this package is provided "as is" and without any express or
      implied warranties, including, but not without limitation, the implied
      warranties of merchantability and fitness for a particular purpose. In
      other words, we accept no liability for any damage that may result from
      using GExperts or programs that use the GExperts source code.
 -----------------------------------------------------------------------------}

unit cFileSearch;

interface

uses
  SysUtils, Classes, SynRegExpr;

type

  TSearchOption = (soCaseSensitive, soWholeWord, soRegEx);

  TSearchOptions = set of TSearchOption;

  TFoundEvent = procedure(Sender: TObject; LineNo: Integer; const Line: string; SPos, EPos: Integer) of object;

  ELineTooLong = class(Exception);

  // We separate the grep code from the file management code in TSearcher
  TBaseSearcher = class(TObject)
  private
    procedure SetBufSize(New: Integer);
  protected
    FOnFound: TFoundEvent;
    FOnStartSearch: TNotifyEvent;
    procedure SignalStartSearch; virtual;
    procedure SignalFoundMatch(LineNo: Integer; const Line: string; SPos, EPos: Integer); virtual;
  protected
    BLine: PChar; // The current search line,
    FLineNo: Integer;
    FEof: Boolean;
    FSearchBuffer: PChar;
    FBufSize: Integer;
    FBufferSearchPos: Integer;
    FBufferDataCount: Integer;
    FNoComments: Boolean;
    FSearchOptions: TSearchOptions;
    FCommentActive: Boolean;
    FPattern: string;
    FFileName: string;
    FRegExpr : TRegExpr;
    procedure DoSearch;
    procedure FillBuffer;
    procedure PatternMatch;
    procedure ReadIntoBuffer(AmountOfBytesToRead: Cardinal); virtual; abstract;
    procedure Seek(Offset: Longint; Direction: Integer); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPattern(const Value: string);
    property BufSize: Integer read FBufSize write SetBufSize;
    property NoComments: Boolean read FNoComments write FNoComments;
    property Pattern: string read FPattern write SetPattern;
    property SearchOptions: TSearchOptions read FSearchOptions write FSearchOptions;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property OnStartSearch: TNotifyEvent read FOnStartSearch write FOnStartSearch;
  end;

  TSearcher = class(TBaseSearcher)
  private
    FSearchStream: TStream;
    procedure Reset;
  protected
    procedure SetFileName(const Value: string);
    procedure FreeObjects;
  protected
    procedure ReadIntoBuffer(AmountOfBytesToRead: Cardinal); override;
    procedure Seek(Offset: Longint; Direction: Integer); override;
  public
    constructor Create(const SearchFileName: string);
    destructor Destroy; override;
    procedure Execute;
  published
    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses
  Windows, uEditAppIntfs, JclStrings, Dialogs;

const
  SearchLineSize = 1024;
  DefaultBufferSize = 2048;

{ TSearcher }

constructor TSearcher.Create(const SearchFileName: string);
begin
  inherited Create;

  if SearchFileName <> '' then
    SetFileName(SearchFileName);
end;

destructor TSearcher.Destroy;
begin
  FreeAndNil(FSearchStream);
  inherited Destroy;
end;

procedure TSearcher.FreeObjects;
begin
  if FFileName <> '' then
  begin
    FreeAndNil(FSearchStream);
  end;
end;

procedure TSearcher.SetFileName(const Value: string);

  function GetFileInterface: Boolean;
  begin
    Result := False;
    if not FileExists(FFileName) then
      Exit;

    try
      FSearchStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      Result := True;
    except
      // We cannot open the file for some reason
      Result := False;
    end;
  end;

  function GetModuleInterface: Boolean;
  var
    Editor : IEditor;
  begin
    Result := False;
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FFileName);
    if Assigned(Editor) then begin
      FSearchStream := TStringStream.Create(Editor.SynEdit.Text);
      Result := True;
    end;
  end;

begin
  FreeObjects;
  FFileName := Value;

  if not GetModuleInterface and not GetFileInterface then
    FFileName := '';
  if FFileName <> '' then
    Reset;
end;

procedure TSearcher.Reset;
resourcestring
  SSearcherReset = 'Reset exception:' + sLineBreak;
begin
  if FFileName = '' then
    Exit;

  FBufferSearchPos := 0;
  FBufferDataCount := 0;
  FLineNo := 0;
  FEof := False;
  FSearchStream.Position := 0;
  FCommentActive := False;
end;

procedure TSearcher.Execute;
begin
  Reset;
  DoSearch;
end;

procedure TSearcher.ReadIntoBuffer(AmountOfBytesToRead: Cardinal);
begin
  FBufferDataCount := FSearchStream.Read(FSearchBuffer^, AmountOfBytesToRead)
end;

procedure TSearcher.Seek(Offset, Direction: Integer);
begin
  FSearchStream.Seek(Offset, Direction);
end;

{ TBaseSearcher }

constructor TBaseSearcher.Create;
begin
  inherited Create;

  FBufSize := DefaultBufferSize;
  BLine := StrAlloc(SearchLineSize);
  FRegExpr := nil;
end;

destructor TBaseSearcher.Destroy;
begin
  StrDispose(FSearchBuffer);
  FSearchBuffer := nil;

  StrDispose(BLine);
  BLine := nil;

  FreeAndNil(FRegExpr);

  inherited Destroy;
end;

procedure TBaseSearcher.FillBuffer;
resourcestring
  SLineLengthError = 'File Search detected a line longer than %d characters in:'+sLineBreak+
                     '%s.' +sLineBreak+
                     'Likely, this is an unsupported binary file type.';
var
  AmountOfBytesToRead: Integer;
  SkippedCharactersCount: Integer;
  LineEndScanner: PChar;
begin
  if FSearchBuffer = nil then
    FSearchBuffer := StrAlloc(FBufSize);
  FSearchBuffer[0] := #0;

  // Read at most (FBufSize - 1) bytes
  AmountOfBytesToRead := FBufSize - 1;

  ReadIntoBuffer(AmountOfBytesToRead);

  FEof := (FBufferDataCount = 0);

  // Reset buffer position to zero
  FBufferSearchPos := 0;

  // If we filled our buffer completely, there is a chance that
  // the last line was read only partially.
  // Since our search algorithm is line-based,
  // skip back to the end of the last completely read line.
  if FBufferDataCount = AmountOfBytesToRead then
  begin
    // Get pointer on last character of read data
    LineEndScanner := FSearchBuffer + FBufferDataCount - 1;
    // We have not skipped any characters yet
    SkippedCharactersCount := 0;
    // While we still have data in the buffer,
    // do scan for a line break as characterised
    // by a #13#10 or #10#13 or a single #10.
    // Which sequence exactly we hit is not important,
    // we just need to find and line terminating
    // sequence.
    while FBufferDataCount > 0 do
    begin
      if LineEndScanner^ = #10 then
      begin
        Seek(-SkippedCharactersCount, soFromCurrent);

        // Finished with finding last complete line
        Break;
      end;

      Inc(SkippedCharactersCount);
      Dec(FBufferDataCount);
      Dec(LineEndScanner);
    end;

    // With FBufferPos = 0 we have scanned back in our
    // buffer and not found any line break; this means
    // that we cannot employ our pattern matcher on a
    // complete line -> Internal Error.
    if FBufferDataCount = 0 then
      raise ELineTooLong.CreateResFmt(@SLineLengthError, [FBufSize - 1, FFileName]);
  end;

  // Cut off everything beyond the line break
  // Assert(FBufferDataCount >= 0);
  FSearchBuffer[FBufferDataCount] := #0;
end;

procedure TBaseSearcher.DoSearch;
var
  i: Integer;
  LPos: Integer;
  UseChar : boolean;
begin
  SignalStartSearch;

  LPos := 0;
  while not FEof do
  begin
    // Read new data in
    if (FBufferSearchPos >= FBufferDataCount) or (FBufferDataCount = 0) then
    begin
      try
        FillBuffer;
      except on E: ELineTooLong do
        begin
           // Just ingnore Issue 74
          //MessageDlg(E.Message, mtWarning, [mbOK], 0);
          Exit;
        end;
      end;
    end;
    if FEof then Exit;
    i := FBufferSearchPos;
    while i < FBufferDataCount do
    begin
      UseChar := False;
      case FSearchBuffer[i] of
        #0:
          begin
            FBufferSearchPos := FBufferDataCount + 1;
            Break;
          end;
        #10:
          begin
            FBufferSearchPos := i + 1;
            FCommentActive := False;
            Break;
          end;
        #13:
          begin
            FBufferSearchPos := i + 1;
            if FSearchBuffer[FBufferSearchPos] = #10 then Inc(FBufferSearchPos);
            FCommentActive := False;
            Break;
          end;
      else
        if FNoComments then begin
          if not FCommentActive then begin
            if FSearchBuffer[i] = '#' then
              FCommentActive := True
            else
              UseChar := True;
          end;
        end else
          UseChar := True;
      end;
      if UseChar then
      begin
        //if not (soCaseSensitive in SearchOptions) then
        BLine[LPos] := FSearchBuffer[i];
        Inc(LPos);
        if LPos >= SearchLineSize-1 then // Enforce maximum line length constraint
          Exit; // Binary, not text file
      end;
      Inc(i);
    end;
    if FSearchBuffer[i] <> #0 then Inc(FLineNo);
    BLine[LPos] := #0;
    if BLine[0] <> #0 then PatternMatch;
    LPos := 0;
    if FBufferSearchPos < i then FBufferSearchPos := i;
  end;
end;

procedure TBaseSearcher.SetBufSize(New: Integer);
begin
  if (FSearchBuffer = nil) and (New <> FBufSize) then
    FBufSize := New;
end;

procedure TBaseSearcher.SetPattern(const Value: string);
begin
  fPattern := Value;
  if soRegEx in SearchOptions then begin
    if not Assigned(fRegExpr) then
      fRegExpr := TRegExpr.Create();
    fRegExpr.ModifierI := soCaseSensitive in SearchOptions;
    try
      fRegExpr.Expression := Value;
      fRegExpr.Compile;
    except
      on E: ERegExpr do
        raise Exception.Create('Invalid Regular expression: ' + E.Message);
    end;
  end;
end;

procedure TBaseSearcher.PatternMatch;
{
  Use RegExpr if RegExpr soRegEx in SearchOptions.
  Else use JclFind or JclSearch depending on whether the search is
  case sensitive.
}
var
  LinePos: Integer;
  Found : Boolean;
  EndPos : integer;

  procedure IsFound;
  //  Deals with soWholeWord Search option
  var
    Start: Integer;
    TestChar: Char;
  begin
    if soWholeWord in SearchOptions then
    begin
      Start := LinePos - 2; // Point to previous character (-2 since PChars are zero based)
      if (Start >= 0) then
      begin
        TestChar := BLine[Start];
        if IsCharAlphaNumeric(TestChar) or (TestChar = '_') then
          Exit;
      end;
      TestChar := BLine[EndPos];  // Next Character
      if TestChar <> #0 then
      begin
        if IsCharAlphaNumeric(TestChar) or (TestChar = '_') then
          Exit;
      end;
    end;
    SignalFoundMatch(FLineNo, BLine, LinePos, EndPos)
  end;

begin
  if soRegEx in SearchOptions then begin
    Found := fRegExpr.Exec(BLine);
    while Found do begin
      LinePos := fRegExpr.MatchPos[0];
      EndPos := LinePos + fRegExpr.MatchLen[0] - 1;
      IsFound;
      Found := fRegExpr.ExecNext;
    end;
  end else begin
    EndPos := 0;
    Repeat
      if soCaseSensitive in SearchOptions then
        LinePos := StrSearch(fPattern, BLine, EndPos + 1)
      else
        LinePos := StrFind(fPattern, BLine, EndPos + 1);
      Found := LinePos > 0;
      if Found then begin
        EndPos := LinePos + Length(fPattern) - 1;
        IsFound;
      end;
    Until not Found;
  end;
end;

procedure TBaseSearcher.SignalStartSearch;
begin
  if Assigned(FOnStartSearch) then
    FOnStartSearch(Self);
end;

procedure TBaseSearcher.SignalFoundMatch(LineNo: Integer; const Line: string;
  SPos, EPos: Integer);
begin
  if Assigned(FOnFound) then
    FOnFound(Self, LineNo, Line, SPos, EPos);
end;

end.


