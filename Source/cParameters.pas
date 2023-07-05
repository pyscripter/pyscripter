{
  syn
  Copyright © 2002, Danail Traichev. All rights reserved.
  neum@developer.bg

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is cEdParam.pas, released Sun, 8 Sep 2002 03:44:18 UTC.

  The Initial Developer of the Original Code is Danail Traichev.
  Portions created by Danail Traichev are Copyright © 2002 Danail Traichev.
  All Rights Reserved.

  Contributor(s): .

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  You may retrieve the latest version of this file at the syn home page,
  located at http://syn.sourceforge.net/

  parameters related functions and classes

 $Id: cEdParam.pas,v 1.22 2004/03/02 13:20:33 seier Exp $

 Simplified version for the purposes of PyScripter by Kiriakos Vlahos

}

unit cParameters;

interface

uses
  Winapi.Windows,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Controls;

type
  (* function, that returns value of a system parameter *)
  TGetParameterProc = function : string;

  (* function, that will be replaced in texts with its value *)
  TParameterFunction = function (const AParameters: string): string;

  (* function, that will return property value for given oobject *)
  TObjectPropertyFunction = function (AObject: TObject;const AObjectName,
                                      APropertyName: string): string;

  (* function, that will be called if parameter or modifier value is not found *)
  TUnknownParameterFunction = function (Sender: TObject; const AName: string;
                                        var AValue: string): Boolean of object;

  (* function, that will be called if object is not found *)
  TUnknownObjectFunction = function (Sender: TObject; const AName: string;
                                     var AObject: TObject): Boolean of object;

  (* function, that will be called if object property is not found *)
  TUnknownPropertyFunction = function (Sender, AObject: TObject;
                                        const AObjectName, APropertyName: string;
                                        var AValue: string): Boolean of object;

  (* list of all parameters *)
  TParameterList = class(TStringList)
  private
    FOnUnknownParameter: TUnknownParameterFunction;
    FOnUnknownModifier: TUnknownParameterFunction;
    FProperties: TStrings;
    FObjectNames: TStrings;
    FModifiers: TStrings;
    FOnUnknownObject: TUnknownObjectFunction;
    FOnUnknownProperty: TUnknownPropertyFunction;
    FStartMask: string;
    FStopMask: string;
    FUsedParameters: TStrings;
    function GetValue(const Name: string): string;
    procedure SetModifiers(const Value: TStrings);
    procedure SetObjectNames(const Value: TStrings);
    procedure SetProperties(const Value: TStrings);
  protected
    procedure SkipParameter(var AText: PChar);
    procedure SkipParameterValue(var AText: PChar; ASeparators: TSysCharSet);
    function ReadParameterValue(var AText: PChar; ASeparators: TSysCharSet): string;
    function ReadParameters(var AText: PChar): string;
    function ReadCondition(var AText: PChar): Boolean;
    procedure DoAddParameter(const AName, AValue: string;
                             GetProc: TGetParameterProc);
    procedure DoChangeParameter(const AName, AValue: string;
                                GetProc: TGetParameterProc; CanAdd: Boolean);
    procedure DoRemoveParameter(const AName: string);
    property UsedParameters: TStrings read FUsedParameters;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  public
    (* system parameters *)
    procedure RegisterParameter(const AName, AValue: string; GetProc: TGetParameterProc);
    procedure UnRegisterParameter(const AName: string);
    procedure ChangeParameter(const AName, AValue: string;
                              GetProc: TGetParameterProc = nil;
                              CanAdd: Boolean = False);
    property OnUnknownParameter: TUnknownParameterFunction
              read FOnUnknownParameter write FOnUnknownParameter;
  public
    (* parameter modifiers *)
    procedure RegisterModifier(const AName, Comment: string; AFunc: TParameterFunction);
    procedure UnRegisterModifier(const AName: string);
    property Modifiers: TStrings read FModifiers write SetModifiers;
    property OnUnknownModifier: TUnknownParameterFunction
              read FOnUnknownModifier write FOnUnknownModifier;
  public
    (* objects and their properties *)
    procedure RegisterObject(const AName: string; AObject: TObject);
    procedure UnRegisterObject(const AName: string);
    procedure RegisterProperty(const AObjectName, APropertyName: string;
                                GetProc: TObjectPropertyFunction = nil);
    procedure UnRegisterProperty(const AObjectName, APropertyName: string);
    property ObjectNames: TStrings read FObjectNames write SetObjectNames;
    property Properties: TStrings read FProperties write SetProperties;
    property OnUnknownObject: TUnknownObjectFunction
              read FOnUnknownObject write FOnUnknownObject;
    property OnUnknownProperty: TUnknownPropertyFunction
              read FOnUnknownProperty write FOnUnknownProperty;
  public
    (* parameter usage *)
    function ReplaceInText(const AText: string): string;
    function ReplaceInTextEx(const AText, AStartMask, AStopMask: string): string;
    function EvaluteCondition(const ACondition: string): Boolean;
    function CalcValue(const AParams: string): string;
    function FindValue(const AName: string; var AValue: string): Boolean;
    procedure ExtractParameters(const AText: string; AParams: TStrings);
    procedure Split(AIndex: Integer; var AName, AValue: string;
                    DoCalc: Boolean = True);
    function MakeParameter(const AName: string): string;
    property StartMask: string read FStartMask write FStartMask;
    property StopMask: string read FStopMask write FStopMask;
    property Values[const Name: string]: string read GetValue;
  end;

(* returns string value for given property *)
function GetPropertyValue(AObject: TObject;
                          const AObjectName, APropertyName: string): string;

(* adds markers for finding replaced text later *)
function SetMarkers(const AParameters: string): string;

(* returns positions of the markers in the text *)
function FindMarkers(var AText: string; out Start, Stop: Integer): Boolean;

(* clears markers, set before *)
procedure ClearMarkers(var AText: string);

var
  (* moment state of all parameters *)
  Parameters: TParameterList;

implementation

uses
  System.TypInfo,
  System.Variants,
  JvGnugettext,
  StringResources,
  uCommonFunctions;

const
  WhiteSpaces: TSysCharSet = [#1..' '];

function GetPropertyValue(AObject: TObject; const AObjectName,
                                                  APropertyName: string): string;
(* returns string value for given property *)
var
  AValue: Variant;
begin
  AValue:= GetPropValue(AObject, APropertyName, True);
  if VarIsNull(AValue) then begin
    StyledMessageDlg(Format(_(SInvalidObjectProperty),
                                    [AObjectName, APropertyName]), mtError, [mbOK], 0);
    Abort;
  end else Result:= AValue;
end;

function SetMarkers(const AParameters: string): string;
(* adds markers for finding replaced text later *)
begin
  Result:= Concat('$[>]', AParameters, '$[<]');
end;

function FindMarkers(var AText: string; out Start, Stop: Integer): Boolean;
(* returns positions of the markers in the text *)
begin
  Start:= Pos('$[>]', AText);
  Result:= Start > 0;
  if Result then begin
    Delete(AText, Start, 4);
    Stop:= Pos('$[<]', Copy(AText, Start, MaxInt));
    if Stop > 0 then begin
      Inc(Stop, Start - 1);
      Delete(AText, Stop, 4);
    end;
  end;
end;

procedure ClearMarkers(var AText: string);
(* clears markers, set before *)
var
  T1, T2: Integer;
begin
  while FindMarkers(AText, T1, T2) do ;
end;

{ TParameterList }

procedure TParameterList.Assign(Source: TPersistent);
begin
  if Source is TParameterList then
    with TParameterList(Source) do begin
      inherited;
      Self.FStartMask:= StartMask;
      Self.FStopMask:= StopMask;
      Self.Modifiers.Assign(FModifiers);
      Self.ObjectNames.Assign(FObjectNames);
      Self.Properties.Assign(FProperties);
      Self.UsedParameters.Assign(FUsedParameters);
    end
  else inherited;
end;

function TParameterList.CalcValue(const AParams: string): string;
(* calculates parameter value *)
var
  P: PChar;
begin
  P:= PChar(AParams);
  Result:= ReadParameters(P);
  (* check if we have something after parameter *)
  if P^ <> #0 then begin
    if StrIsLeft(P, PChar(StopMask)) then begin
      Inc(P, Length(StopMask));
      if P^ = #0 then Exit;
    end;
    Result:= Result + ReplaceInText(P);
  end;
end;

procedure TParameterList.ChangeParameter(const AName, AValue: string;
  GetProc: TGetParameterProc; CanAdd: Boolean);
(* changes system parameter value *)
begin
  DoChangeParameter(AName, AValue, GetProc, CanAdd);
end;

constructor TParameterList.Create;

  function CreateSortedList: TStringList;
  begin
    Result:= TStringList.Create;
    with Result do begin
      Duplicates:= dupError;
      Sorted:= True;
    end;
  end;

begin
  inherited;
  FStartMask:= '$[';
  FStopMask:= ']';
  FModifiers:= CreateSortedList;
  FObjectNames:= CreateSortedList;
  FProperties:= CreateSortedList;
  FUsedParameters:= CreateSortedList;
end;

destructor TParameterList.Destroy;
begin
  FUsedParameters.Free;
  FProperties.Free;
  FObjectNames.Free;
  FModifiers.Free;
  inherited;
end;

procedure TParameterList.DoAddParameter(const AName,
  AValue: string; GetProc: TGetParameterProc);
begin
  AddObject(Concat(AName, '=', AValue), TObject(@GetProc));
end;

procedure TParameterList.DoChangeParameter(const AName, AValue: string;
  GetProc: TGetParameterProc; CanAdd: Boolean);
var
  i, L : Integer;
  Param: string;
  DoAdd: Boolean;
begin
  Param:= AName + '=';
  L:= Length(Param);
  DoAdd := False;
  for i:= Count - 1 downto 0 do
    if SameText(Param, Copy(Strings[i], 1, L)) then begin
      Delete(I);
      DoAdd := True;
      Break;
    end;
  if DoAdd or CanAdd then
    AddObject(Concat(AName, '=', AValue), TObject(@GetProc));
end;

procedure TParameterList.DoRemoveParameter(const AName: string);
var
  i, L: Integer;
  Param: string;
begin
  Param:= AName + '=';
  L:= Length(Param);
  for i:= Count - 1 downto 0 do
    if SameText(Param, Copy(Strings[i], 1, L)) then begin
      Delete(i);
      Break;
    end;
end;

function TParameterList.EvaluteCondition(const ACondition: string): Boolean;
(* evalutes simple paramater condition *)
var
  P: PChar;
begin
  P:= PChar(ACondition);
  Result:= ReadCondition(P);
  if P^ <> #0 then begin
    StyledMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TParameterList.ExtractParameters(const AText: string;
  AParams: TStrings);
(* extracts parameters from AText to AParams *)
var
  PParam: PChar;
  AParam, AValue: string;
  Delimiters: TSysCharSet;
begin
  Delimiters:= [];
  Include(Delimiters, AnsiChar(StartMask[1]));
  Include(Delimiters, AnsiChar(StopMask[1]));
  with AParams do try
    BeginUpdate;
    PParam:= PChar(AText);
    repeat
      PParam:= StrPos(PParam, PChar(StartMask));
      (* maybe parameter is found *)
      if Assigned(PParam) then begin
        Inc(PParam, Length(StartMask));
        (* we want only real parameters *)
        AValue:= '';
        AParam:= StrGetToken(PParam, Delimiters + ['(', '-', '.', '=', '?'], [], ['''']);
        if CharInSet(PChar(AParam)^, ['(', '''']) or CharInSet(PParam^, ['.', '=']) then
          AParam:= '';
        (* skip to the end of block *)
        SkipParameter(PParam);
        if PParam^ = #0 then Exit;
        (* get parameter values *)
        if (AParam <> '') and (IndexOfName(AParam) < 0) then begin
          try
            FindValue(AParam, AValue);
          except ;
          end;
          Add(Concat(AParam, '=', AValue));
        end;
        Inc(PParam, Length(StopMask));
      end
    until PParam = nil;
  finally
    EndUpdate;
  end;
end;

function TParameterList.FindValue(const AName: string;
   var AValue: string): Boolean;
var
  Temp: string;
  i: Integer;
begin
  i:= IndexOfName(AName);
  Result:= I >= 0;
  if Result then
    Split(i, Temp, AValue, True);
end;

function TParameterList.GetValue(const Name: string): string;
begin
  if not FindValue(Name, Result) then
    Result:=  '';
end;

function TParameterList.MakeParameter(const AName: string): string;
begin
  Result:= Concat(StartMask, AName, StopMask);
end;

function TParameterList.ReadCondition(var AText: PChar): Boolean;
(* reads parameter condition and evalutes it *)
const
  Signs: TSysCharSet = ['<', '>', '='];

(* evalutes simple paramater condition *)

  function CompareValue(ALeft, ARight: Extended): integer;
  begin
    if Abs(ALeft - ARight) < 0.0000001 then
      Result:= 0
    else if ALeft < ARight then
      Result:= -1
    else Result:= 1;
  end;

var
  i: Integer;
  ALeft, AOperation, ARight: string;
  ELeft, ERight: Extended;
begin
  if AText^ = '(' then Inc(AText);
  while CharInSet(AText^, WhiteSpaces) do Inc(AText);
  (* read left value *)
  ALeft:= ReadParameterValue(AText, Signs + [')']);
  while CharInSet(AText^, WhiteSpaces) do Inc(AText);
  (* read operation *)
  if CharInSet(AText^, Signs) then begin
    if CharInSet((AText + 1)^, Signs) then begin
      SetString(AOperation, AText, 2);
      Inc(AText, 2);
    end
    else begin
      AOperation:= AText^;
      Inc(AText);
    end;
    while CharInSet(AText^, WhiteSpaces) do Inc(AText);
    (* read right value *)
    ARight:= ReadParameterValue(AText, [')']);
  end
  else begin
    if AText^ = '?' then begin
      Result:= True;
      Inc(AText);
      while CharInSet(AText^, WhiteSpaces) do Inc(AText);
      case MessageDlg(ALeft, mtConfirmation, mbYesNoCancel, 0) of
        mrYes:  Result:= True;
        mrNo:   Result:= False;
        else    Abort;
      end;
      Exit;
    end
    else if SameText(Copy(AText, 1, 7), 'IS NULL') then begin
      AOperation:= '=';
      Inc(AText, 7);
    end
    else if SameText(Copy(AText, 1, 11), 'IS NOT NULL') then begin
      AOperation:= '<>';
      Inc(AText, 11);
    end
    else begin
      StyledMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
      Abort;
    end;
    ARight:= '';
  end;
  while CharInSet(AText^, WhiteSpaces) do Inc(AText);
  (* evalute condition *)
  if AOperation = '' then begin
    StyledMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
    Abort;
  end;
  (* compare numbers *)
  if Extended.TryParse(ALeft, ELeft) and Extended.TryParse(ARight, ERight) then
    i:= CompareValue(ELeft, ERight)
  (* compare strings *)
  else
    i:= AnsiCompareText(ALeft, ARight);
  Result:= ((AOperation = '=')  and  (i = 0)) or
           ((AOperation = '>')  and  (i > 0)) or
           ((AOperation = '<')  and  (i < 0)) or
           ((AOperation = '>=') and ((i = 0) or (i > 0))) or
           ((AOperation = '<=') and ((i = 0) or (i < 0))) or
           ((AOperation = '<>') and ((i < 0) or (i > 0)));
end;

function TParameterList.ReadParameters(var AText: PChar): string;
(* reads parameters and modifiers and calculates value *)
const
  Delimiters: TSysCharSet = ['.', '-'];
var
  Separators: TSysCharSet;

  procedure CalcCondition;
  (* calculates conditional parameter *)
  var
    IsTrue: Boolean;
  begin
    (* find the result *)
    IsTrue:= ReadCondition(AText);
    if AText^ <> ')' then begin
      StyledMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
      Abort;
    end;

    Inc(AText);
    (* returns true value *)
    if IsTrue then begin
      if AText^ = ':' then Result:= ''
      else Result:= ReadParameterValue(AText, [':']);
      if AText^ = ':' then Inc(AText);
      SkipParameterValue(AText, ['-']);
    end
    (* returns false value *)
    else begin
      SkipParameterValue(AText, [':']);
      if (AText^ = #0) or StrIsLeft(AText, PChar(StopMask)) then
        Result:= ''
      else begin
        Inc(AText);
        Result:= ReadParameterValue(AText, Separators)
      end;
    end;
  end;

  procedure CalcParameterValue(const AParam: string);
  (* calculates parameter value *)
  var
    P: PChar;
    AName, AQuestion, ExistingValue: string;
    ValueExists, HasValue, HasQuestion: Boolean;
  begin
    P:= PChar(AParam);
    (* empty parameter are special case *)
    if P^ = #0 then Result:= ''
    (* quoted parameter is returned as it is *)
    else if P^ = '''' then
      Result:= ReplaceInText(AnsiExtractQuotedStr(P, ''''))
    else begin
      (* split parameter to components *)
      AName:= StrGetToken(P, ['=', '?'], [], ['''']);
      HasValue:= P^ = '=';
      if HasValue then begin
        Inc(P);
        Result:= ReadParameterValue(P, Separators +  ['?']);
      end
      else Result:= '';
      HasQuestion:= P^ = '?';
      (* search in parameters first for actual value *)
      ValueExists:= ((AName <> '') and FindValue(AName, ExistingValue));
      (* if we have assigned question - query for parameter value *)
      if HasQuestion then begin
        Inc(P);
        AQuestion:= P;
        if AQuestion = '' then
          AQuestion:= _(SEnterParameterText)
        (* AQuestion can contain parameters *)
        else if PChar(AQuestion)^ = '''' then
          AQuestion:= ReplaceInText(AnsiExtractQuotedStr(P, ''''));
        if not InputQuery(_(SEnterParameterCaption), _(AQuestion), Result) then
          Abort;
      end;
      if HasValue and (AName <> '') then begin
        (* Register/Remove parameter*)
        if ValueExists and not HasQuestion and (Result = '') then begin
          DoRemoveParameter(AName);
          Exit;
        end else if not ValueExists and (Result <> '') then
          RegisterParameter(AName, Result, nil);
      end;
      if not HasQuestion and ValueExists then
        Result := ExistingValue;
      (* check if someone can help us *)
      if not (ValueExists or HasQuestion or HasValue) then
        if not Assigned(FOnUnknownParameter) then begin
          StyledMessageDlg(_(SParameterNotFound), mtError, [mbOK], 0);
          Abort;
        end else if not FOnUnknownParameter(Self, AName, Result) then
          Abort; // quiet exit after helper event
    end;
  end;

  procedure CalcModifierValue(const AModifier: string);
  (* modifies calculated parameter value *)
  var
    i: Integer;
  begin
    (* quoted modifier is returned as it is *)
    if PChar(AModifier)^ = '''' then
      Result:= Concat(Result, '-''', ReplaceInText(StrUnquote(AModifier)), '''')
    else begin
      i:= Modifiers.IndexOfName(AModifier);
      (* modify parameter value *)
      if i >= 0 then
        Result:= TParameterFunction(Modifiers.Objects[i])(Result)
      (* check if someone can help us *)
      else if not Assigned(FOnUnknownModifier) then begin
        StyledMessageDlg(Format(_(SModifierNotFound), [AModifier]), mtError, [mbOK], 0);
        Abort;
      end else if not FOnUnknownModifier(Self, AModifier, Result) then
        Abort; // quiet exit after helper event
    end;
  end;

var
  AObjectName: string;
  AObject: TObject;
  StartOfText, EndOfText: Boolean;

  procedure CalcPropertyValue(const APropertyName: string);
  (* calculates property value *)
  var
    i: Integer;
  begin
    Result:= Concat(AObjectName, '.', APropertyName);
    i:= FProperties.IndexOf(Result);
    (* get value, if it's registered *)
    if i >= 0 then begin
      Result:= TObjectPropertyFunction(FProperties.Objects[i])(AObject,
                                                               AObjectName,
                                                               APropertyName);
      AObject:= nil;
      AObjectName:= '';
    end
    (* maybe someone can help us *)
    else if Assigned(FOnUnknownProperty) and
            FOnUnknownProperty(Self, AObject, AObjectName,
                                     APropertyName, Result) then begin
      AObject:= nil;
      AObjectName:= '';
    end
    (* maybe this is a subobject *)
    else if not EndOfText then begin
      if TStringList(ObjectNames).Find(Result, i) then
        AObject:= ObjectNames.Objects[i]
      (* or even sub-sub-object :) *)
      else if not Assigned(FOnUnknownObject) or
              not FOnUnknownObject(Self, Result, AObject) then
        if (i >= 0) and (i < ObjectNames.Count) and SameText(Result + '.',
                                Copy(ObjectNames[i], 1, Length(Result)+1)) then
          AObject:= nil
        else if Assigned(FOnUnknownObject) then Abort // quiet exit after helper event
        else begin
          StyledMessageDlg(Format(_(SPropertyNotFound),
            [AObjectName, APropertyName]), mtError, [mbOK], 0);
          Abort;
        end;
      AObjectName:= Result;
      StartOfText:= AObject = nil;
    end
    else if Assigned(FOnUnknownProperty) then Abort // quiet exit after helper event
    else begin
      StyledMessageDlg(Format(_(SPropertyNotFound),
        [AObjectName, APropertyName]), mtError, [mbOK], 0);
      Abort;
    end;
  end;

var
  AName: string;
  i: Integer;
begin
  Result:= '';
  (* empty parameter *)
  if AText = '' then Exit;
  StartOfText:= True;
  (* calculate conditional parameter *)
  Separators:= Delimiters;
  Include(Separators, AnsiChar(StopMask[1]));
  if AText^ = '(' then begin
    CalcCondition;
    if (AText^ = #0) or StrIsLeft(AText, PChar(StopMask)) then Exit;
    StartOfText:= False;
  end;
  AObject:= nil;
  AObjectName:= '';
  EndOfText:= AText^ = #0;
  while not EndOfText do begin
    (* reads next object, property, parameter or modifier *)
    AName:= '';
    repeat
      AName:= AName + StrGetToken(AText, Separators, [], ['''']);
      EndOfText:= (AText^ = #0) or StrIsLeft(AText, PChar(StopMask));
      if not EndOfText then Inc(AText);
    until EndOfText or CharInSet((AText-1)^, Delimiters);
    (* find it's value *)
    if StartOfText then begin
      (* this is parameter *)
      if (EndOfText and (AObjectName = '')) or ((AText-1)^ = '-') then begin
        CalcParameterValue(AName);
        StartOfText:= False;
      end
      (* this is object name *)
      else begin
        (* get object or subobject name *)
        if AObjectName = '' then AObjectName:= AName
        else AObjectName:= Concat(AObjectName, '.', AName);
        (* is it registered? *)
        if not TStringList(ObjectNames).Find(AObjectName, i) then begin
          if not Assigned(FOnUnknownObject) or
             not FOnUnknownObject(Self, AObjectName, AObject) then begin
            (* check if there is subobject of this object *)
            if (i >= 0) and (i < ObjectNames.Count) and
               SameText(AObjectName + '.',
                        Copy(ObjectNames[i], 1, Length(AObjectName)+1)) then
              AObject:= nil
            else if Assigned(FOnUnknownObject) then Abort // quiet exit after helper event
            else begin
              StyledMessageDlg(Format(_(SObjectNotFound), [AObjectName]), mtError, [mbOK], 0);
              Abort;
            end;
          end
        end
        else AObject:= ObjectNames.Objects[i];
        StartOfText:= AObject = nil;
      end;
    end
    (* calculate property value *)
    else if Assigned(AObject) then
      CalcPropertyValue(AName)
    (* calculate modified value *)
    else CalcModifierValue(AName);
  end;
end;

function TParameterList.ReadParameterValue(var AText: PChar;
                                           ASeparators: TSysCharSet): string;
(* reads parameter value *)
begin
  (* parameter value is text and parameters *)
  if AText^ = '''' then
    Result:= ReplaceInText(AnsiExtractQuotedStr(AText, ''''))
  (* parameter value is other parameter *)
  else if StrIsLeft(AText, PChar(StartMask)) then begin
    Inc(AText, Length(StartMask));
    Result:= ReadParameters(AText);
    if StrIsLeft(AText, PChar(StopMask)) then
      Inc(AText, Length(StopMask));
  end
  (* parameter value is simple value *)
  else Result:= StrGetToken(AText, ASeparators, WhiteSpaces, ['''']);
end;

procedure TParameterList.RegisterModifier(const AName, Comment: string;
  AFunc: TParameterFunction);
(* registers parameter modifier - small name after the parameter,
   that can change parameter value - for example:
      ActiveDoc-Path  - returns active document path
      ActiveDoc-Ext   - returns active document extension
      Project-Long    - returns long project filename
*)
begin
  if Modifiers.IndexOfName(AName) >= 0  then
    raise Exception.CreateFmt(SDuplicateModifier, [AName]);
  Modifiers.AddObject(AName + '=' + Comment, TObject(@AFunc))
end;

procedure TParameterList.RegisterObject(const AName: string; AObject: TObject);
begin
  ObjectNames.AddObject(AName, AObject);
end;

procedure TParameterList.RegisterParameter(const AName, AValue: string;
  GetProc: TGetParameterProc);
begin
  DoAddParameter(AName, AValue, GetProc);
end;

procedure TParameterList.RegisterProperty(const AObjectName,
  APropertyName: string; GetProc: TObjectPropertyFunction = nil);
begin
  if not Assigned(GetProc) then GetProc:= GetPropertyValue;
  FProperties.AddObject(Concat(AObjectName, '.', APropertyName), TObject(@GetProc))
end;

function TParameterList.ReplaceInText(const AText: string): string;
(* replaces parameters in AText with their values *)
var
  PText, PParam: PChar;
  AValue: string;
begin
  Result:= '';
  PText:= PChar(AText);
  repeat
    PParam:= StrPos(PText, PChar(StartMask));
    // maybe parameter is found
    if Assigned(PParam) then begin
      Result:= Result + Copy(PText, 1, PParam - PText);
      Inc(PParam, Length(StartMask));
      // reads parameter value
      PText:= PParam;
      AValue:= ReadParameters(PText);
      // invalid parameter
      if PText^ = #0 then begin
        Dec(PParam, Length(StartMask));
        Result:= Result + PParam;
        PParam:= nil;
      end
      // read parameter values
      else begin
        Result:= Result + AValue;
        Inc(PText, Length(StopMask));
      end;
    end
    else Result:= Result + PText;
  until PParam = nil;
end;

function TParameterList.ReplaceInTextEx(const AText, AStartMask,
  AStopMask: string): string;
(* like ReplaceInText, but uses different parameter start and stop masks *)
var
  OldStartMask, OldStopMask: string;
begin
  OldStartMask:= StartMask;
  OldStopMask:= StopMask;
  try
    StartMask:= AStartMask;
    StopMask:= AStopMask;
    Result:= ReplaceInText(AText);
  finally
    StartMask:= OldStartMask;
    StopMask:= OldStopMask;
  end;
end;

procedure TParameterList.SetModifiers(const Value: TStrings);
begin
  FModifiers.Assign(Value);
end;

procedure TParameterList.SetObjectNames(const Value: TStrings);
begin
  FObjectNames.Assign(Value);
end;

procedure TParameterList.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

procedure TParameterList.SkipParameter(var AText: PChar);
var
  Level: Integer;
  Delimiters: TSysCharSet;
begin
  Delimiters:= [];
  Include(Delimiters, AnsiChar(StartMask[1]));
  Include(Delimiters, AnsiChar(StopMask[1]));
  Level:= Ord(not StrIsLeft(AText, PChar(StartMask)));
  repeat
    if StrIsLeft(AText, PChar(StartMask)) then Inc(Level);
    if StrIsLeft(AText, PChar(StopMask)) then begin
      Dec(Level);
      if Level = 0 then Break;
    end;
    StrGetToken(AText, Delimiters, [], ['''']);
  until AText^ = #0;
  if AText^ <> #0 then Inc(AText, Length(StopMask));
end;

procedure TParameterList.SkipParameterValue(var AText: PChar; ASeparators: TSysCharSet);
begin
  (* parameter value is text and parameters *)
  if AText^ = '''' then
    AnsiExtractQuotedStr(AText, '''')
  (* parameter value is other parameter *)
  else if StrIsLeft(AText, PChar(StartMask)) then
    SkipParameter(AText)
  (* parameter value is simple value *)
  else begin
    Include(ASeparators, AnsiChar(StopMask[1]));
    StrGetToken(AText, ASeparators, WhiteSpaces, ['''']);
  end;
end;

procedure TParameterList.Split(AIndex: Integer; var AName, AValue: string;
  DoCalc: Boolean);
var
  i, ui: Integer;
  P: PChar;
begin
  // get parameter name and value
  AName:= Strings[AIndex];
  i:= Pos('=', AName);
  if i > 0 then begin
    AValue:= Copy(AName, i+1, MaxInt);
    System.Delete(AName, i, MaxInt);
  end
  else AValue:= '';
  if not DoCalc then Exit;

  // check for circular references
  try
    ui:= FUsedParameters.Add(AName);
  except
    raise Exception.CreateFmt(_(SParamCircularReference), [AName]);
  end;
  try
    // dynamic parameter
    if Assigned(Objects[AIndex]) then
      AValue:= TGetParameterProc(Objects[AIndex])

    // static parameter
    else if (AValue <> '') then
      // parameter can point to other parameters
      if AValue[1] = '''' then begin
        P:= PChar(AValue);
        AValue:= ReplaceInText(AnsiExtractQuotedStr(P, ''''));
      end
      else if StrIsLeft(PChar(AValue), PChar(StartMask)) then
        AValue:= CalcValue(Copy(AValue, Length(StartMask)+1, MaxInt));
  finally
    FUsedParameters.Delete(ui);
  end;
end;

procedure TParameterList.UnRegisterModifier(const AName: string);
(* unregisters parameter modifier *)
var
  i: Integer;
begin
  with Modifiers do begin
    i:= IndexOfName(AName);
    if i >= 0 then Delete(i);
  end;
end;

procedure TParameterList.UnRegisterObject(const AName: string);
(* unregisters parameter modifier *)
var
  i: Integer;
begin
  with ObjectNames do begin
    i:= IndexOf(AName);
    if i >= 0 then Delete(i);
  end;
end;

procedure TParameterList.UnRegisterParameter(const AName: string);
begin
  DoRemoveParameter(AName);
end;

procedure TParameterList.UnRegisterProperty(const AObjectName,
  APropertyName: string);
var
  i: Integer;
begin
  with Properties do begin
    i:= IndexOf(Concat(AObjectName, '.', APropertyName));
    if i >= 0 then Delete(i);
  end;
end;

initialization
  Parameters:= TParameterList.Create;
finalization
  FreeAndNil(Parameters);
end.
