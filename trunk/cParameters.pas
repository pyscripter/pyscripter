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

{$i jedi.inc}

uses
  Windows, Classes, SysUtils, Dialogs, Controls, WideStrings;

type
  (* function, that returns value of a system parameter *)
  TGetParameterProc = function : WideString;

  (* function, that will be replaced in texts with its value *)
  TParameterFunction = function (const AParameters: WideString): WideString;

  (* function, that will return property value for given oobject *)
  TObjectPropertyFunction = function (AObject: TObject;const AObjectName,
                                      APropertyName: WideString): WideString;

  (* function, that will be called if parameter or modifier value is not found *)
  TUnknownParameterFunction = function (Sender: TObject; const AName: WideString;
                                        var AValue: WideString): Boolean of object;

  (* function, that will be called if object is not found *)
  TUnknownObjectFunction = function (Sender: TObject; const AName: WideString;
                                     var AObject: TObject): Boolean of object;

  (* function, that will be called if object property is not found *)
  TUnknownPropertyFunction = function (Sender, AObject: TObject;
                                        const AObjectName, APropertyName: WideString;
                                        var AValue: WideString): Boolean of object;

  (* list of all parameters *)
  TParameterList = class(TWideStringList)
  private
    FOnUnknownParameter: TUnknownParameterFunction;
    FOnUnknownModifier: TUnknownParameterFunction;
    FProperties: TWideStrings;
    FObjectNames: TWideStrings;
    FModifiers: TWideStrings;
    FOnUnknownObject: TUnknownObjectFunction;
    FOnUnknownProperty: TUnknownPropertyFunction;
    FStartMask: WideString;
    FStopMask: WideString;
    FUsedParameters: TWideStrings;
    function GetValue(const Name: WideString): WideString;
    procedure SetModifiers(const Value: TWideStrings);
    procedure SetObjectNames(const Value: TWideStrings);
    procedure SetProperties(const Value: TWideStrings);
  protected
    procedure SkipParameter(var AText: PWideChar);
    procedure SkipParameterValue(var AText: PWideChar; ASeparators: TSysCharSet);
    function ReadParameterValue(var AText: PWideChar; ASeparators: TSysCharSet): WideString;
    function ReadParameters(var AText: PWideChar): WideString;
    function ReadCondition(var AText: PWideChar): Boolean;
    procedure DoAddParameter(const AName, AValue: WideString;
                             GetProc: TGetParameterProc);
    procedure DoChangeParameter(const AName, AValue: WideString;
                                GetProc: TGetParameterProc; CanAdd: Boolean);
    procedure DoRemoveParameter(const AName: WideString);
    property UsedParameters: TWideStrings read FUsedParameters;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  public
    (* system parameters *)
    procedure RegisterParameter(const AName, AValue: WideString; GetProc: TGetParameterProc);
    procedure UnRegisterParameter(const AName: WideString);
    procedure ChangeParameter(const AName, AValue: WideString;
                              GetProc: TGetParameterProc = nil;
                              CanAdd: Boolean = False);
    property OnUnknownParameter: TUnknownParameterFunction
              read FOnUnknownParameter write FOnUnknownParameter;
  public
    (* parameter modifiers *)
    procedure RegisterModifier(const AName, Comment: WideString; AFunc: TParameterFunction);
    procedure UnRegisterModifier(const AName: WideString);
    property Modifiers: TWideStrings read FModifiers write SetModifiers;
    property OnUnknownModifier: TUnknownParameterFunction
              read FOnUnknownModifier write FOnUnknownModifier;
  public
    (* objects and their properties *)
    procedure RegisterObject(const AName: WideString; AObject: TObject);
    procedure UnRegisterObject(const AName: WideString);
    procedure RegisterProperty(const AObjectName, APropertyName: WideString;
                                GetProc: TObjectPropertyFunction = nil);
    procedure UnRegisterProperty(const AObjectName, APropertyName: WideString);
    property ObjectNames: TWideStrings read FObjectNames write SetObjectNames;
    property Properties: TWideStrings read FProperties write SetProperties;
    property OnUnknownObject: TUnknownObjectFunction
              read FOnUnknownObject write FOnUnknownObject;
    property OnUnknownProperty: TUnknownPropertyFunction
              read FOnUnknownProperty write FOnUnknownProperty;
  public
    (* parameter usage *)
    function ReplaceInText(const AText: WideString): WideString;
    function ReplaceInTextEx(const AText, AStartMask, AStopMask: WideString): WideString;
    function EvaluteCondition(const ACondition: WideString): Boolean;
    function CalcValue(const AParams: WideString): WideString;
    function FindValue(const AName: WideString; var AValue: WideString): Boolean;
    procedure ExtractParameters(const AText: WideString; AParams: TWideStrings);
    procedure Split(AIndex: Integer; var AName, AValue: WideString;
                    DoCalc: Boolean = True);
    function MakeParameter(const AName: WideString): WideString;
    property StartMask: WideString read FStartMask write FStartMask;
    property StopMask: WideString read FStopMask write FStopMask;
    property Values[const Name: WideString]: WideString read GetValue;
  end;

(* returns WideString value for given property *)
function GetPropertyValue(AObject: TObject;
                          const AObjectName, APropertyName: WideString): WideString;

(* adds markers for finding replaced text later *)
function SetMarkers(const AParameters: WideString): WideString; 

(* returns positions of the markers in the text *)
function FindMarkers(var AText: WideString; out Start, Stop: Integer): Boolean;

(* clears markers, set before *)
procedure ClearMarkers(var AText: WideString);

var
  (* moment state of all parameters *)
  Parameters: TParameterList;

implementation

uses
  TypInfo{$IFDEF DELPHI6_UP}, Variants{$ENDIF}, uCommonFunctions, gnugettext,
  StringResources, WideStrUtils, TntDialogs;

const
  WhiteSpaces: TSysCharSet = [#1..' '];

function GetPropertyValue(AObject: TObject; const AObjectName,
                                                  APropertyName: WideString): WideString;
(* returns WideString value for given property *)
var
  AValue: Variant;
begin
  AValue:= GetPropValue(AObject, APropertyName, True);
  if VarIsNull(AValue) then begin
    WideMessageDlg(WideFormat(_(SInvalidObjectProperty),
                                    [AObjectName, APropertyName]), mtError, [mbOK], 0);
    Abort;
  end else Result:= AValue;
end;

function SetMarkers(const AParameters: WideString): WideString;
(* adds markers for finding replaced text later *)
begin
  Result:= Concat('$[>]', AParameters, '$[<]');
end;

function FindMarkers(var AText: WideString; out Start, Stop: Integer): Boolean;
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

procedure ClearMarkers(var AText: WideString);
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

function TParameterList.CalcValue(const AParams: WideString): WideString;
(* calculates parameter value *)
var
  P: PWideChar;
begin
  P:= PWideChar(AParams);
  Result:= ReadParameters(P);
  (* check if we have something after parameter *)
  if P^ <> #0 then begin
    if WideStrIsLeft(P, PWideChar(StopMask)) then begin
      Inc(P, Length(StopMask));
      if P^ = #0 then Exit;
    end;
    Result:= Result + ReplaceInText(P);
  end;
end;

procedure TParameterList.ChangeParameter(const AName, AValue: WideString;
  GetProc: TGetParameterProc; CanAdd: Boolean);
(* changes system parameter value *)
begin
  DoChangeParameter(AName, AValue, GetProc, CanAdd);
end;

constructor TParameterList.Create;

  function CreateSortedList: TWideStringList;
  begin
    Result:= TWideStringList.Create;
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
  AValue: WideString; GetProc: TGetParameterProc);
begin
  AddObject(Concat(AName, '=', AValue), TObject(@GetProc));
end;

procedure TParameterList.DoChangeParameter(const AName, AValue: WideString;
  GetProc: TGetParameterProc; CanAdd: Boolean);
var
  i, L : Integer;
  Param: WideString;
begin
  Param:= AName + '=';
  L:= Length(Param);
  for i:= Count - 1 downto 0 do
    if WideSameText(Param, Copy(Strings[i], 1, L)) then begin
      Strings[i]:= Concat(AName, '=', AValue);
      Objects[i]:= TObject(@GetProc);
      Exit;
    end;
  if CanAdd then
    AddObject(Concat(AName, '=', AValue), TObject(@GetProc));
end;

procedure TParameterList.DoRemoveParameter(const AName: WideString);
var
  i, L: Integer;
  Param: WideString;
begin
  Param:= AName + '=';
  L:= Length(Param);
  for i:= Count - 1 downto 0 do
    if WideSameText(Param, Copy(Strings[i], 1, L)) then begin
      Delete(i);
      Break;
    end;
end;

function TParameterList.EvaluteCondition(const ACondition: WideString): Boolean;
(* evalutes simple paramater condition *)
var
  P: PWideChar;
begin
  P:= PWideChar(ACondition);
  Result:= ReadCondition(P);
  if P^ <> #0 then begin
    WideMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TParameterList.ExtractParameters(const AText: WideString;
  AParams: TWideStrings);
(* extracts parameters from AText to AParams *)
var
  PParam: PWideChar;
  AParam, AValue: WideString;
  Delimiters: TSysCharSet;
begin
  Delimiters:= [];
  Include(Delimiters, Char(PWideChar(StartMask)^));
  Include(Delimiters, Char(PWideChar(StopMask)^));
  with AParams do try
    BeginUpdate;
    PParam:= PWideChar(AText);
    repeat
      PParam:= WStrPos(PParam, PWideChar(StartMask));
      (* maybe parameter is found *)
      if Assigned(PParam) then begin
        Inc(PParam, Length(StartMask));
        (* we want only real parameters *)
        AValue:= '';
        AParam:= StrGetToken(PParam, Delimiters + ['(', '-', '.', '=', '?'], [], ['''']);
        if InOpSet(PWideChar(AParam)^, ['(', '''']) or InOpSet(PParam^, ['.', '=']) then
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

function TParameterList.FindValue(const AName: WideString;
   var AValue: WideString): Boolean;
var
  Temp: WideString;
  i: Integer;
begin
  i:= IndexOfName(AName);
  Result:= I >= 0;
  if Result then
    Split(i, Temp, AValue, True);
end;

function TParameterList.GetValue(const Name: WideString): WideString;
begin
  if not FindValue(Name, Result) then
    Result:=  '';
end;

function TParameterList.MakeParameter(const AName: WideString): WideString;
begin
  Result:= Concat(StartMask, AName, StopMask);
end;

function TParameterList.ReadCondition(var AText: PWideChar): Boolean;
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
  ALeft, AOperation, ARight: WideString;
begin
  if AText^ = '(' then Inc(AText);
  while InOpSet(AText^, WhiteSpaces) do Inc(AText);
  (* read left value *)
  ALeft:= ReadParameterValue(AText, Signs + [')']);
  while InOpSet(AText^, WhiteSpaces) do Inc(AText);
  (* read operation *)
  if InOpSet(AText^, Signs) then begin
    if InOpSet((AText + 1)^, Signs) then begin
      SetString(AOperation, AText, 2);
      Inc(AText, 2);
    end
    else begin
      AOperation:= AText^;
      Inc(AText);
    end;
    while InOpSet(AText^, WhiteSpaces) do Inc(AText);
    (* read right value *)
    ARight:= ReadParameterValue(AText, [')']);
  end
  else begin
    if AText^ = '?' then begin
      Result:= True;
      Inc(AText);
      while InOpSet(AText^, WhiteSpaces) do Inc(AText);
      case MessageDlg(ALeft, mtConfirmation, mbYesNoCancel, 0) of
        mrYes:  Result:= True;
        mrNo:   Result:= False;
        else    Abort;
      end;
      Exit;
    end
    else if WideSameText(Copy(AText, 1, 7), 'IS NULL') then begin
      AOperation:= '=';
      Inc(AText, 7);
    end
    else if WideSameText(Copy(AText, 1, 11), 'IS NOT NULL') then begin
      AOperation:= '<>';
      Inc(AText, 11);
    end
    else begin
      WideMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
      Abort;
    end;
    ARight:= '';
  end;
  while InOpSet(AText^, WhiteSpaces) do Inc(AText);
  (* evalute condition *)
  if AOperation = '' then begin
    WideMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
    Abort;
  end;
  (* compare numbers *)
  if WideStrConsistsOfNumberChars(ALeft) and WideStrConsistsOfNumberChars(ARight) then
    i:= CompareValue(StrToFloat(ALeft), StrToFloat(ARight))
  (* compare strings *)
  else i:= AnsiCompareText(ALeft, ARight);
  Result:= ((AOperation = '=')  and  (i = 0)) or
           ((AOperation = '>')  and  (i > 0)) or
           ((AOperation = '<')  and  (i < 0)) or
           ((AOperation = '>=') and ((i = 0) or (i > 0))) or
           ((AOperation = '<=') and ((i = 0) or (i < 0))) or
           ((AOperation = '<>') and ((i < 0) or (i > 0)));
end;

function TParameterList.ReadParameters(var AText: PWideChar): WideString;
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
      WideMessageDlg(_(SInvalidConditionFormat), mtError, [mbOK], 0);
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
      if (AText^ = #0) or WideStrIsLeft(AText, PWideChar(StopMask)) then
        Result:= ''
      else begin
        Inc(AText);
        Result:= ReadParameterValue(AText, Separators)
      end;
    end;
  end;

  procedure CalcParameterValue(const AParam: WideString);
  (* calculates parameter value *)
  var
    P: PWideChar;
    AName, AQuestion: WideString;
    ValueFound, HasValue, HasQuestion: Boolean;
  begin
    P:= PWideChar(AParam);
    (* empty parameter are special case *)
    if P^ = #0 then Result:= ''
    (* quoted parameter is returned as it is *)
    else if P^ = '''' then
      Result:= ReplaceInText(WideExtractQuotedStr(P, ''''))
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
      Inc(P);
      AQuestion:= P;
      (* search in parameters first for actual value *)
      ValueFound:= ((AName <> '') and FindValue(AName, Result)) or HasValue;
      (* if we have assigned question - query for parameter value *)
      if HasQuestion then begin
        if AQuestion = '' then
          AQuestion:= _(SEnterParameterText)
        (* AQuestion can contain parameters *)
        else if PWideChar(AQuestion)^ = '''' then
          AQuestion:= ReplaceInText(WideExtractQuotedStr(P, ''''));
        ValueFound:= WideInputQuery(_(SEnterParameterCaption), AQuestion, Result);
      end;
      (* check if someone can help us *)
      if not (HasQuestion or HasValue) and not ValueFound then
        if not Assigned(FOnUnknownParameter) then begin
          WideMessageDlg(_(SParameterNotFound), mtError, [mbOK], 0);
          Abort;
        end else if not FOnUnknownParameter(Self, AName, Result) then
          Abort; // quiet exit after helper event
    end;
  end;

  procedure CalcModifierValue(const AModifier: WideString);
  (* modifies calculated parameter value *)
  var
    i: Integer;
  begin
    (* quoted modifier is returned as it is *)
    if PWideChar(AModifier)^ = '''' then
      Result:= Concat(Result, '-''', ReplaceInText(StrUnquote(AModifier)), '''')
    else begin
      i:= Modifiers.IndexOfName(AModifier);
      (* modify parameter value *)
      if i >= 0 then
        Result:= TParameterFunction(Modifiers.Objects[i])(Result)
      (* check if someone can help us *)
      else if not Assigned(FOnUnknownModifier) then begin
        WideMessageDlg(WideFormat(_(SModifierNotFound), [AModifier]), mtError, [mbOK], 0);
        Abort;
      end else if not FOnUnknownModifier(Self, AModifier, Result) then
        Abort; // quiet exit after helper event
    end;
  end;

var
  AObjectName: WideString;
  AObject: TObject;
  StartOfText, EndOfText: Boolean;

  procedure CalcPropertyValue(const APropertyName: WideString);
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
      if TWideStringList(ObjectNames).Find(Result, i) then
        AObject:= ObjectNames.Objects[i]
      (* or even sub-sub-object :) *)
      else if not Assigned(FOnUnknownObject) or
              not FOnUnknownObject(Self, Result, AObject) then
        if (i >= 0) and (i < ObjectNames.Count) and WideSameText(Result + '.',
                                Copy(ObjectNames[i], 1, Length(Result)+1)) then
          AObject:= nil
        else if Assigned(FOnUnknownObject) then Abort // quiet exit after helper event
        else begin
          WideMessageDlg(WideFormat(_(SPropertyNotFound),
            [AObjectName, APropertyName]), mtError, [mbOK], 0);
          Abort;
        end;
      AObjectName:= Result;
      StartOfText:= AObject = nil;
    end
    else if Assigned(FOnUnknownProperty) then Abort // quiet exit after helper event
    else begin
      WideMessageDlg(WideFormat(_(SPropertyNotFound),
        [AObjectName, APropertyName]), mtError, [mbOK], 0);
      Abort;
    end;
  end;

var
  AName: WideString;
  i: Integer;
begin
  Result:= '';
  (* empty parameter *)
  if AText = '' then Exit;
  StartOfText:= True;
  (* calculate conditional parameter *)
  Separators:= Delimiters;
  Include(Separators, Char(PWideChar(StopMask)^));
  if AText^ = '(' then begin
    CalcCondition;
    if (AText^ = #0) or WideStrIsLeft(AText, PWideChar(StopMask)) then Exit;
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
      EndOfText:= (AText^ = #0) or WideStrIsLeft(AText, PWideChar(StopMask));
      if not EndOfText then Inc(AText);
    until EndOfText or InOpSet((AText-1)^, Delimiters);
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
        if not TWideStringList(ObjectNames).Find(AObjectName, i) then begin
          if not Assigned(FOnUnknownObject) or
             not FOnUnknownObject(Self, AObjectName, AObject) then begin
            (* check if there is subobject of this object *)
            if (i >= 0) and (i < ObjectNames.Count) and
               WideSameText(AObjectName + '.',
                        Copy(ObjectNames[i], 1, Length(AObjectName)+1)) then
              AObject:= nil
            else if Assigned(FOnUnknownObject) then Abort // quiet exit after helper event
            else begin
              WideMessageDlg(WideFormat(_(SObjectNotFound), [AObjectName]), mtError, [mbOK], 0);
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

function TParameterList.ReadParameterValue(var AText: PWideChar;
                                           ASeparators: TSysCharSet): WideString;
(* reads parameter value *)
begin
  (* parameter value is text and parameters *)
  if AText^ = '''' then
    Result:= ReplaceInText(WideExtractQuotedStr(AText, ''''))
  (* parameter value is other parameter *)
  else if WideStrIsLeft(AText, PWideChar(StartMask)) then begin
    Inc(AText, Length(StartMask));
    Result:= ReadParameters(AText);
    Inc(AText, Length(StopMask));
  end
  (* parameter value is simple value *)
  else Result:= StrGetToken(AText, ASeparators, WhiteSpaces, ['''']);
end;

procedure TParameterList.RegisterModifier(const AName, Comment: WideString;
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

procedure TParameterList.RegisterObject(const AName: WideString; AObject: TObject);
begin
  ObjectNames.AddObject(AName, AObject);
end;

procedure TParameterList.RegisterParameter(const AName, AValue: WideString;
  GetProc: TGetParameterProc);
begin
  DoAddParameter(AName, AValue, GetProc);
end;

procedure TParameterList.RegisterProperty(const AObjectName,
  APropertyName: WideString; GetProc: TObjectPropertyFunction = nil);
begin
  if not Assigned(GetProc) then GetProc:= GetPropertyValue;
  FProperties.AddObject(Concat(AObjectName, '.', APropertyName), TObject(@GetProc))
end;

function TParameterList.ReplaceInText(const AText: WideString): WideString;
(* replaces parameters in AText with their values *)
var
  PText, PParam: PWideChar;
  AValue: WideString;
begin
  Result:= '';
  PText:= PWideChar(AText);
  repeat
    PParam:= WStrPos(PText, PWideChar(StartMask));
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
  AStopMask: WideString): WideString;
(* like ReplaceInText, but uses different parameter start and stop masks *)
var
  OldStartMask, OldStopMask: WideString;
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

procedure TParameterList.SetModifiers(const Value: TWideStrings);
begin
  FModifiers.Assign(Value);
end;

procedure TParameterList.SetObjectNames(const Value: TWideStrings);
begin
  FObjectNames.Assign(Value);
end;

procedure TParameterList.SetProperties(const Value: TWideStrings);
begin
  FProperties.Assign(Value);
end;

procedure TParameterList.SkipParameter(var AText: PWideChar);
var
  Level: Integer;
  Delimiters: TSysCharSet;
begin
  Delimiters:= [];
  Include(Delimiters, Char(PWideChar(StartMask)^));
  Include(Delimiters, Char(PWideChar(StopMask)^));
  Level:= Ord(not WideStrIsLeft(AText, PWideChar(StartMask)));
  repeat
    if WideStrIsLeft(AText, PWideChar(StartMask)) then Inc(Level);
    if WideStrIsLeft(AText, PWideChar(StopMask)) then begin
      Dec(Level);
      if Level = 0 then Break;
    end;
    StrGetToken(AText, Delimiters, [], ['''']);
  until AText^ = #0;
  if AText^ <> #0 then Inc(AText, Length(StopMask));
end;

procedure TParameterList.SkipParameterValue(var AText: PWideChar; ASeparators: TSysCharSet);
begin
  (* parameter value is text and parameters *)
  if AText^ = '''' then
    WideExtractQuotedStr(AText, '''')
  (* parameter value is other parameter *)
  else if WideStrIsLeft(AText, PWideChar(StartMask)) then
    SkipParameter(AText)
  (* parameter value is simple value *)
  else begin
    Include(ASeparators, Char(PWideChar(StopMask)^));
    StrGetToken(AText, ASeparators, WhiteSpaces, ['''']);
  end;
end;

procedure TParameterList.Split(AIndex: Integer; var AName, AValue: WideString;
  DoCalc: Boolean);
var
  i, ui: Integer;
  P: PWideChar;
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
        P:= PWideChar(AValue);
        AValue:= ReplaceInText(WideExtractQuotedStr(P, ''''));
      end
      else if WideStrIsLeft(PWideChar(AValue), PWideChar(StartMask)) then
        AValue:= CalcValue(Copy(AValue, Length(StartMask)+1, MaxInt));
  finally
    FUsedParameters.Delete(ui);
  end;
end;

procedure TParameterList.UnRegisterModifier(const AName: WideString);
(* unregisters parameter modifier *)
var
  i: Integer;
begin
  with Modifiers do begin
    i:= IndexOf(AName);
    if i >= 0 then Delete(i);
  end;
end;

procedure TParameterList.UnRegisterObject(const AName: WideString);
(* unregisters parameter modifier *)
var
  i: Integer;
begin
  with ObjectNames do begin
    i:= IndexOf(AName);
    if i >= 0 then Delete(i);
  end;
end;

procedure TParameterList.UnRegisterParameter(const AName: WideString);
begin
  DoRemoveParameter(AName);
end;

procedure TParameterList.UnRegisterProperty(const AObjectName,
  APropertyName: WideString);
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

