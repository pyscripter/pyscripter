unit VarPyth;

(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'VarPyth'          Copyright (c) 2001                    *)
(*                                                                        *)
(* Version: 1.0                     Morgan Martinet                       *)
(* Sub-Version: 0.7                 4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*  look our page at: http://mmm-experts.com/                             *)
(*      and our Wiki at: http://py4d.pbwiki.com/                          *)
(**************************************************************************)
(*  Functionality:  This allows you to use Python objects like COM        *)
(*                  automation objects, inside your Delphi source code.   *)
(*                  This is a replacement of the former PythonAtom.pas    *)
(*                  that uses the new custom variant types introduced     *)
(*                  in Delphi6.                                           *)
(**************************************************************************)
(*  Contributors:                                                         *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)

{$I Definition.Inc}

{$IFNDEF DELPHI6_OR_HIGHER}
  This unit requires Delphi6 or later to compile!!!
{$ENDIF}

interface
uses
  Variants, PythonEngine;

type
  TSequenceType = (stTuple, stList);

{ Python variant creation utils }
function VarPythonCreate( AObject : PPyObject ) : Variant; overload;
function VarPythonCreate( const AValue : Variant ) : Variant; overload;
function VarPythonCreate( const AValues : array of const; ASequenceType : TSequenceType = stList ) : Variant; overload;
function VarPythonEval( const APythonExpression : String) : Variant;
function GetAtom( AObject : PPyObject ) : Variant; // compatibility function with PythonAtom.pas

{ Python variant helper functions }
function VarPython: TVarType;
function VarIsPython(const AValue: Variant): Boolean;
function VarAsPython(const AValue: Variant): Variant;
function ExtractPythonObjectFrom(const AValue : Variant) : PPyObject;
function VarIsSame(const A, B : Variant ) : Boolean; // checks if 2 variants share the same Python object.
function VarIsSameType(const A, B : Variant ) : Boolean; // checks if 2 variants are of the same Python type.
function VarIsPythonSequence(const AValue: Variant): Boolean;
function VarIsPythonMapping(const AValue: Variant): Boolean;
function VarIsPythonNumber(const AValue: Variant): Boolean;
function VarIsPythonString(const AValue: Variant): Boolean;
function VarIsPythonInteger(const AValue: Variant): Boolean;
function VarIsPythonFloat(const AValue: Variant): Boolean;
function VarIsPythonTuple(const AValue: Variant): Boolean;
function VarIsPythonList(const AValue: Variant): Boolean;
function VarIsPythonDict(const AValue: Variant): Boolean;
function VarIsPythonClass(const AValue: Variant): Boolean;
function VarIsPythonInstance(const AValue: Variant): Boolean;
function VarIsPythonMethod(const AValue: Variant): Boolean;
function VarIsPythonFunction(const AValue: Variant): Boolean;
function VarIsPythonModule(const AValue: Variant): Boolean;
function VarIsPythonCallable(const AValue: Variant): Boolean;
function VarIsPythonIterator(const AValue: Variant): Boolean;
{$IFDEF UNICODE_SUPPORT}
function VarIsPythonUnicode(const AValue: Variant): Boolean;
{$ENDIF}
function VarIsPythonDateTime(const AValue: Variant): Boolean;
function VarIsPythonDate(const AValue: Variant): Boolean;
function VarIsPythonTime(const AValue: Variant): Boolean;
function VarIsPythonDateTimeDelta(const AValue: Variant): Boolean;
function VarIsPythonTZInfo(const AValue: Variant): Boolean;
{$IFDEF PYTHON23_OR_HIGHER}
function VarIsBool(const AValue: Variant): Boolean;
function VarIsEnum(const AValue: Variant): Boolean;
{$ENDIF}
function VarIsInstanceOf(const AInstance, AClass : Variant): Boolean;
function VarIsSubclassOf(const ADerived, AClass : Variant): Boolean;
function VarIsSubtypeOf(const ADerived, AType : Variant): Boolean;
function VarIsNone(const AValue : Variant): Boolean;
function VarIsTrue(const AValue : Variant): Boolean;

function VarModuleHasObject(const AModule : Variant; aObj: String): Boolean;

function NewPythonList( const ASize : Integer = 0 ): Variant;
function NewPythonTuple( const ASize : Integer ): Variant;
function NewPythonDict: Variant;

function None : Variant;
function Ellipsis : Variant;
function MainModule : Variant; // return the main module that's used for executing a script.
function BuiltinModule : Variant; // return the builtin module 
function SysModule : Variant; // return the builtin module 'sys'
function DatetimeModule : Variant; // return the builtin module 'datetime'
function Import( const AModule : String ) : Variant; // import a Python module and return the module object.
function len(const AValue : Variant ) : Integer; // return the length of a Python collection.
function _type(const AValue : Variant ) : Variant; // return the type object of a Python object.
function iter(const AValue : Variant ) : Variant; // return an iterator for the container AValue. You can call the 'next' method of the iterator until you catch the EPyStopIteration exception.

implementation

uses
  VarUtils, SysUtils, SysConst, TypInfo, Classes;

type
  TNamedParamDesc = record
    Index : Integer;
    Name : String;
  end;
  TNamedParamArray = array of TNamedParamDesc;
  {$IFDEF FPC}
  PString = ^AnsiString;
  {$ELSE}
  PString = System.PString;
  {$ENDIF}

  { Python variant type handler }
  TPythonVariantType = class(TInvokeableVariantType, IVarInstanceReference)
  protected
    fNamedParams : TNamedParamArray;
    function LeftPromotion(const V: TVarData; const AOperator: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
    function RightPromotion(const V: TVarData; const AOperator: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
    function GetInstance(const V: TVarData): TObject;
    function EvalPython(const V: TVarData; const AName: string;
      const Arguments: TVarDataArray): PPyObject;
    function  VarDataToPythonObject( AVarData : TVarData ) : PPyObject;
    procedure PythonObjectToVarData( var Dest : TVarData; AObject : PPyObject; APythonAtomCompatible : Boolean );
    procedure PyhonVarDataCreate( var Dest : TVarData; AObject : PPyObject );
    procedure DoDispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); virtual;
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    procedure BinaryOp(var Left: TVarData; const Right: TVarData;
      const AOperator: TVarOp); override;
    procedure UnaryOp(var Right: TVarData; const AOperator: TVarOp); override;
    {$IFDEF FPC}
      {$HINT CompareOp not supported in FPC}
    {$ELSE}
    function CompareOp(const Left: TVarData; const Right: TVarData;
      const AOperator: Integer): Boolean; override;
    {$ENDIF}
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const AName: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const AName: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const AName: string): Boolean; override;
    function GetPropertyWithArg(var Dest: TVarData; const V: TVarData;
      const AName: string; AArg : TVarData): Boolean; virtual;
    function SetProperty(const V: TVarData; const AName: string;
      const Value: TVarData): Boolean; override;
{$IFDEF HAS_MODIFIED_DISPINVOKE}
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
{$ELSE}
    procedure DispInvoke(var Dest: TVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
{$ENDIF}
  end;

var
  { Python variant type handler instance }
  PythonVariantType: TPythonVariantType = nil;

type
  { Python data that the Python variant points to }
  TPythonData = class(TObject)
  private
    fPyObject: PPyObject;
    fPythonAtomCompatible: Boolean;
    function GetAsString: String;
    procedure SetPyObject(const Value: PPyObject);
    function GetAsVariant: Variant;
    function GetAsWideString: WideString;
  public
    constructor Create(AObject : PPyObject); overload;
    constructor Create(AObject : PPyObject; APythonAtomCompatible : Boolean); overload;
    destructor Destroy; override;

    // query state
    function IsNone : Boolean;

    // non-destructive operations
    function Equal(const Right: TPythonData): Boolean;
    function Compare(const Right: TPythonData): Integer;
    function LessThan(const Right: TPythonData): Boolean;
    function LessOrEqualThan(const Right: TPythonData): Boolean;
    function GreaterThan(const Right: TPythonData): Boolean;
    function GreaterOrEqualThan(const Right: TPythonData): Boolean;

    // destructive operations
    procedure DoAdd(const Right: TPythonData);
    procedure DoSubtract(const Right: TPythonData);
    procedure DoMultiply(const Right: TPythonData);
    procedure DoDivide(const Right: TPythonData);
    procedure DoIntDivide(const Right: TPythonData);
    procedure DoModulus(const Right: TPythonData);
    procedure DoShiftLeft(const Right: TPythonData);
    procedure DoShiftRight(const Right: TPythonData);
    procedure DoAnd(const Right: TPythonData);
    procedure DoOr(const Right: TPythonData);
    procedure DoXor(const Right: TPythonData);
    procedure DoNegate;
    procedure DoNot;

    // conversion
    property AsString: String read GetAsString;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: WideString read GetAsWideString;

    // data
    property PyObject : PPyObject read fPyObject write SetPyObject;
    property PythonAtomCompatible : Boolean read fPythonAtomCompatible;
  end;

type
  { Helper record that helps crack open TVarData }
  TPythonVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VPython: TPythonData;
    Reserved4: LongInt;
  end;


resourcestring
  SMultiDimensionalPropsNotSupported = 'Multi-dimensional sequences or mappings are not supported in Python';
  SCantConvertArg = 'Can''t convert argument #%d of %s into a Python object';
  SBothOperandsOfIntDivideMustBeIntegers = 'Both operands of an integer division must be of type integer';
  SCantConvertKeyToPythonObject = 'Can''t convert Key into a Python object';
  SCantConvertValueToPythonObject = 'Can''t convert Value into a Python object';
  SCantCreateNewSequenceObject = 'Can''t create a new sequence object';
  SExpectedPythonVariant = 'Expected a Python variant';
  SNoIterators = 'This version of Python does not support iterators';

//------------------------------------------------------------------------------
{ Helper functions for porting VarPyth to FPC (Free Pascal Compiler) }
{$IFDEF FPC}
  procedure VarClear(var V : Variant);
  begin
    V := Unassigned;
  end;

  procedure VarCast(var Dest: Variant; const Source: Variant; VarType: Integer);
  begin
    if VarType = varPython then
      PythonVariantType.Cast(TVarData(Dest), TVarData(Source))
    else
      Dest := Source;
  end;
{$ENDIF}

//------------------------------------------------------------------------------
{ Python variant creation utils }

function VarPythonCreate( AObject : PPyObject ) : Variant;
begin
  VarClear(Result);
  if Assigned(AObject) then
  begin
    TPythonVarData(Result).VType := VarPython;
    TPythonVarData(Result).VPython := TPythonData.Create(AObject);
  end; // of if
end;

function VarPythonCreate( const AValue : Variant ) : Variant;
var
  _value : PPyObject;
begin
  if VarIsPython(AValue) then
    Result := AValue
  else
    with GetPythonEngine do
    begin
      _value := VariantAsPyObject(AValue);
      try
        Result := VarPythonCreate( _value );
      finally
        Py_XDecRef(_value);
      end;
    end; // of with
end;

function VarPythonCreate( const AValues : array of const; ASequenceType : TSequenceType = stList ) : Variant;
var
  i : Integer;
  _seq, _item : PPyObject;
begin
  with GetPythonEngine do
  begin
    if ASequenceType = stTuple then
      _seq := PyTuple_New( High(AValues)-Low(AValues)+1 )
    else
      _seq := PyList_New( High(AValues)-Low(AValues)+1 );
    if not Assigned(_seq) then
      raise Exception.Create(SCantCreateNewSequenceObject);
    try
      for i := Low(AValues) to High(AValues) do
      begin
        if (AValues[i].VType = vtVariant) and VarIsPython(AValues[i].VVariant^) then
        begin
          _item := ExtractPythonObjectFrom( AValues[i].VVariant^ );
          Py_XIncRef(_item);
        end
        else
          _item := VarRecAsPyObject( AValues[i] );
        if ASequenceType = stTuple then
          PyTuple_SetItem( _seq, i, _item )
        else
          PyList_SetItem( _seq, i, _item );
      end; // of for
      Result := VarPythonCreate( _seq );
    finally
      Py_XDecRef(_seq);
    end; // of try
  end; // of with
end;

function VarPythonEval( const APythonExpression : String) : Variant;
var
  _obj : PPyObject;
begin
  with GetPythonEngine do
  begin
    _obj := EvalString(APythonExpression);
    try
      Result := VarPythonCreate( _obj  );
    finally
      Py_XDecRef(_obj);
    end;
  end;
end;

// compatibility function with PythonAtom.pas
function GetAtom( AObject : PPyObject ) : Variant;
begin
  VarClear(Result);
  if Assigned(AObject) then
  begin
    TPythonVarData(Result).VType := VarPython;
    TPythonVarData(Result).VPython := TPythonData.Create(AObject, True);
  end; // of if
end;

function VarPython: TVarType;
begin
  Result := PythonVariantType.VarType;
end;

function VarIsPython(const AValue: Variant): Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = VarPython;
end;

function VarAsPython(const AValue: Variant): Variant;
begin
  if not VarIsPython(AValue) then
    VarCast(Result, AValue, VarPython)
  else
    Result := AValue;
end;

// note that the returned reference to the Python object is a borrowed reference (not pre-incremented).
function ExtractPythonObjectFrom(const AValue : Variant) : PPyObject;
begin
  if VarIsPython(AValue) then
    Result := TPythonVarData(AValue).VPython.PyObject
  else
    Result := nil;
end;

function VarIsSame(const A, B : Variant ) : Boolean;
begin
  Result := ExtractPythonObjectFrom(A) = ExtractPythonObjectFrom(B);
end;

function VarIsSameType(const A, B : Variant ) : Boolean;
var
  _obj1, _obj2 : PPyObject;
begin
  _obj1 := ExtractPythonObjectFrom(A);
  _obj2 := ExtractPythonObjectFrom(B);
  Result := Assigned(_obj1) and Assigned(_obj2) and (_obj1^.ob_type = _obj2^.ob_type);
end;

//------------------------------------------------------------------------------
{ Python variant helper functions }
function VarIsPythonSequence(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PySequence_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonMapping(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyMapping_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonNumber(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyNumber_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonString(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyString_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonInteger(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyInt_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonFloat(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyFloat_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTuple(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTuple_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonList(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyList_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDict(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDict_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonClass(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyClass_Check(ExtractPythonObjectFrom(AValue))
             or (GetPythonEngine.PyObject_HasAttrString(ExtractPythonObjectFrom(AValue), '__bases__') <> 0));
end;

function VarIsPythonInstance(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyInstance_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonMethod(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyMethod_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonFunction(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyFunction_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonModule(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyModule_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonCallable(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyCallable_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonIterator(const AValue: Variant): Boolean;
begin
{$IFDEF PYTHON20_OR_HIGHER}
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyIter_Check(ExtractPythonObjectFrom(AValue)));
{$ELSE}
  Result := False;
{$ENDIF}
end;

{$IFDEF UNICODE_SUPPORT}
function VarIsPythonUnicode(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyUnicode_Check(ExtractPythonObjectFrom(AValue));
end;
{$ENDIF}

function VarIsPythonDateTime(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDateTime_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDate(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDate_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTime(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTime_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDateTimeDelta(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDelta_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTZInfo(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTZInfo_Check(ExtractPythonObjectFrom(AValue));
end;

{$IFDEF PYTHON23_OR_HIGHER}
function VarIsBool(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyBool_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsEnum(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyEnum_Check(ExtractPythonObjectFrom(AValue));
end;
{$ENDIF}

function VarIsInstanceOf(const AInstance, AClass : Variant): Boolean;
begin
{$IFDEF PYTHON21_OR_HIGHER}
  with GetPythonEngine do
  begin
    Result := VarIsPython(AInstance) and VarIsPython(AClass) and
              (PyObject_IsInstance( ExtractPythonObjectFrom(AInstance),
                                    ExtractPythonObjectFrom(AClass)) <> 0);
    CheckError;
  end; // of with
{$ELSE}
  raise Exception.Create('Not implemented in this version of Python');
{$ENDIF}
end;

function VarIsSubclassOf(const ADerived, AClass : Variant): Boolean;
begin
{$IFDEF PYTHON21_OR_HIGHER}
  with GetPythonEngine do
  begin
    Result := VarIsPython(ADerived) and VarIsPython(AClass) and
              (PyObject_IsSubclass( ExtractPythonObjectFrom(ADerived),
                                    ExtractPythonObjectFrom(AClass)) <> 0);
    CheckError;
  end; // of with
{$ELSE}
  raise Exception.Create('Not implemented in this version of Python');
{$ENDIF}
end;

function VarIsSubtypeOf(const ADerived, AType : Variant): Boolean;
begin
{$IFDEF PYTHON22_OR_HIGHER}
  with GetPythonEngine do
  begin
    Result := VarIsPython(ADerived) and VarIsPython(AType) and
              (PyType_IsSubtype( ExtractPythonObjectFrom(ADerived)^.ob_type,
                                 PPyTypeObject(ExtractPythonObjectFrom(AType))) <> 0);
    CheckError;
  end; // of with
{$ELSE}
  raise Exception.Create('Not implemented in this version of Python');
{$ENDIF}
end;

function VarIsNone(const AValue : Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (ExtractPythonObjectFrom(AValue) = GetPythonEngine.Py_None);
end;

function VarIsTrue(const AValue : Variant): Boolean;
begin
  Result := AValue; // the cast into a boolean will call the PyObject_IsTrue API.
end;

function VarModuleHasObject(const AModule : Variant; aObj: String): Boolean;
begin
  with GetPythonEngine do
    Result := VarIsPython(AModule) and
              PyModule_Check(ExtractPythonObjectFrom(AModule)) and
              Assigned(PyDict_GetItemString(
                PyModule_GetDict(ExtractPythonObjectFrom(AModule)),PChar(aObj)));
end;

function NewPythonList( const ASize : Integer = 0 ): Variant;
var
  _list : PPyObject;
begin
  with GetPythonEngine do
  begin
    _list := PyList_New(ASize);
    try
      Result := VarPythonCreate( _list );
    finally
      Py_XDecRef(_list);
    end; // of try
  end; // of with
end;

function NewPythonTuple( const ASize : Integer ): Variant;
var
  _tuple : PPyObject;
begin
  with GetPythonEngine do
  begin
    _tuple := PyTuple_New(ASize);
    try
      Result := VarPythonCreate( _tuple );
    finally
      Py_XDecRef(_tuple);
    end; // of try
  end; // of with
end;

function NewPythonDict: Variant;
var
  _dict : PPyObject;
begin
  with GetPythonEngine do
  begin
    _dict := PyDict_New;
    try
      Result := VarPythonCreate( _dict );
    finally
      Py_XDecRef(_dict);
    end; // of try
  end; // of with
end;

function None : Variant;
begin
  with GetPythonEngine do
    Result := VarPythonCreate(Py_None);
end;

function Ellipsis : Variant;
begin
  with GetPythonEngine do
    Result := VarPythonCreate(Py_Ellipsis);
end;

function MainModule : Variant;
var
  _main : PPyObject;
begin
  _main := GetPythonEngine.GetMainModule; // the refcount is not pre-incremented
  Assert(Assigned(_main));
  Result := VarPythonCreate(_main);
end;

function BuiltinModule : Variant;
begin
  Result := Import(GetPythonEngine.BuiltInModuleName);
end;

function SysModule : Variant;
begin
  Result := Import('sys');
end;

function DatetimeModule : Variant; // return the builtin module 'datetime'
begin
  Result := Import('datetime');
end;

function Import( const AModule : String ) : Variant;
var
  _module : PPyObject;
  _module_name : PPyObject;
begin
  with GetPythonEngine do
  begin
    _module_name := PyString_FromString(PChar(AModule));
    try
      _module := PyImport_Import(_module_name);
      CheckError;
    finally
      Py_XDecRef(_module_name);
    end; // of try
    Assert(Assigned(_module));
    try
      Result := VarPythonCreate(_module);
    finally
      Py_XDecRef(_module);
    end; // of try
  end; // of with
end;

function GetObjectLength(AObject: PPyObject): Integer;
begin
  with GetPythonEngine do
  begin
    PyErr_Clear;
    Result := PyObject_Length(AObject);
    CheckError;
  end; // of with
end;

// returns the length of a Python collection.
function len(const AValue : Variant ) : Integer;
begin
  if VarIsPython(AValue) then
    Result := GetObjectLength( ExtractPythonObjectFrom(AValue) )
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

function _type(const AValue : Variant ) : Variant;
begin
  if VarIsPython(AValue) then
    Result := VarPythonCreate( PPyObject( ExtractPythonObjectFrom(AValue)^.ob_type ) )
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

function iter(const AValue : Variant ) : Variant;
var
  _iter : PPyObject;
begin
  if VarIsPython(AValue) then
{$IFDEF PYTHON20_OR_HIGHER}
    with GetPythonEngine do
    begin
      PyErr_Clear;
      _iter := PyObject_GetIter(ExtractPythonObjectFrom(AValue));
      CheckError;
      try
        Result := VarPythonCreate(_iter);
      finally
        Py_XDecRef(_iter);
      end;
    end
{$ELSE}
    raise Exception.Create(SNoIterators);
{$ENDIF}
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

//------------------------------------------------------------------------------
{ TPythonVariantType }

procedure TPythonVariantType.BinaryOp(var Left: TVarData;
  const Right: TVarData; const AOperator: TVarOp);
begin
  if Right.VType = VarType then
    case Left.VType of
      varString:
        case AOperator of
          opAdd:
            Variant(Left) := Variant(Left) + TPythonVarData(Right).VPython.AsString;
        else
          RaiseInvalidOp;
        end; // of varString
    else
      if Left.VType = VarType then
        case AOperator of
          opAdd:
            TPythonVarData(Left).VPython.DoAdd(TPythonVarData(Right).VPython);
          opSubtract:
            TPythonVarData(Left).VPython.DoSubtract(TPythonVarData(Right).VPython);
          opMultiply:
            TPythonVarData(Left).VPython.DoMultiply(TPythonVarData(Right).VPython);
          opDivide:
            TPythonVarData(Left).VPython.DoDivide(TPythonVarData(Right).VPython);
          opIntDivide:
            TPythonVarData(Left).VPython.DoIntDivide(TPythonVarData(Right).VPython);
          opModulus:
            TPythonVarData(Left).VPython.DoModulus(TPythonVarData(Right).VPython);
          opShiftLeft:
            TPythonVarData(Left).VPython.DoShiftLeft(TPythonVarData(Right).VPython);
          opShiftRight:
            TPythonVarData(Left).VPython.DoShiftRight(TPythonVarData(Right).VPython);
          opAnd:
            TPythonVarData(Left).VPython.DoAnd(TPythonVarData(Right).VPython);
          opOr:
            TPythonVarData(Left).VPython.DoOr(TPythonVarData(Right).VPython);
          opXor:
            TPythonVarData(Left).VPython.DoXor(TPythonVarData(Right).VPython);
        else
          RaiseInvalidOp;
        end // of case
      else
        RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;

procedure TPythonVariantType.Cast(var Dest: TVarData;
  const Source: TVarData);
var
  _object : PPyObject;
begin
  _object := VarDataToPythonObject(Source);
  try
    PyhonVarDataCreate( Dest, _object );
  finally
    GetPythonEngine.Py_XDECREF(_object);
  end;
end;

procedure TPythonVariantType.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);
var
  V : Variant;
begin
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TPythonVarData(Source).VPython.AsWideString);

      varString:
        VarDataFromStr(Dest, TPythonVarData(Source).VPython.AsString);
    else
      if AVarType and varTypeMask = varBoolean then
      begin
        Dest.VType := varBoolean;
        Dest.VBoolean := GetPythonEngine.PyObject_IsTrue( TPythonVarData(Source).VPython.PyObject ) = 1;
      end
      else
      begin
        V := TPythonVarData(Source).VPython.AsVariant;
        VarDataCastTo(Dest, TVarData(V), AVarType);
      end;
    end // of case
  else
    inherited;
end;

procedure TPythonVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TPythonVarData(V).VPython);
end;

{$IFNDEF FPC}
function TPythonVariantType.CompareOp(const Left, Right: TVarData;
  const AOperator: Integer): Boolean;
begin
  Result := False;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    case AOperator of
      opCmpEQ:
        Result := TPythonVarData(Left).VPython.Equal(TPythonVarData(Right).VPython);
      opCmpNE:
        Result := not TPythonVarData(Left).VPython.Equal(TPythonVarData(Right).VPython);
      opCmpLT:
        Result := TPythonVarData(Left).VPython.LessThan(TPythonVarData(Right).VPython);
      opCmpLE:
        Result := TPythonVarData(Left).VPython.LessOrEqualThan(TPythonVarData(Right).VPython);
      opCmpGT:
        Result := TPythonVarData(Left).VPython.GreaterThan(TPythonVarData(Right).VPython);
      opCmpGE:
        Result := TPythonVarData(Left).VPython.GreaterOrEqualThan(TPythonVarData(Right).VPython);
    else
      RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;
{$ENDIF}

procedure TPythonVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    PyhonVarDataCreate( Dest, TPythonVarData(Source).VPython.PyObject );
    // propagate compatibility mode
    TPythonVarData(Dest).VPython.fPythonAtomCompatible := TPythonVarData(Source).VPython.PythonAtomCompatible;
  end; // of if
end;

procedure SetClearVarToEmptyParam(var V: TVarData);
begin
{$IFDEF DELPHI7_OR_HIGHER}
  VarClear(Variant(V));
  V.VType := varError;
  V.VError := VAR_PARAMNOTFOUND;
{$ELSE}
  V.VType := varError;
  V.VError := $80020004; {DISP_E_PARAMNOTFOUND}
{$ENDIF}
end;

{$IFDEF HAS_MODIFIED_DISPINVOKE}
  // Note that DispInvoke has a different interface in Kylix2 only!
procedure TPythonVariantType.DispInvoke(Dest: PVarData;
  const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
begin
  DoDispInvoke(Dest, Source, CallDesc, Params);
end;
{$ELSE}
procedure TPythonVariantType.DispInvoke(var Dest: TVarData;
  const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
begin
  DoDispInvoke(@Dest, Source, CallDesc, Params);
end;
{$ENDIF}

procedure TPythonVariantType.DoDispInvoke(Dest: PVarData;
  const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
type
  PParamRec = ^TParamRec;
  TParamRec = array[0..3] of LongInt;
  TStringDesc = record
    BStr: WideString;
    PStr: {$IFNDEF FPC}System.{$ENDIF}PString;
  end;
const
  CDoMethod    = $01;
  CPropertyGet = $02;
  CPropertySet = $04;
var
  LArguments: TVarDataArray;
  LStrings: array of TStringDesc;
  LStrCount: Integer;
  LParamPtr: Pointer;
  LNamedArgStart : Integer;     //arg position of 1st named argument (if any)
  LNamePtr: PChar;

  procedure ParseParam(I: Integer);
  const
    CArgTypeMask    = $7F;
    CArgByRef       = $80;
  var
    LArgType: Integer;
    LArgByRef: Boolean;
  begin
    LArgType := CallDesc^.ArgTypes[I] and CArgTypeMask;
    LArgByRef := (CallDesc^.ArgTypes[I] and CArgByRef) <> 0;

    if I >= LNamedArgStart then
    begin
      LNamePtr := LNamePtr + Succ(StrLen(LNamePtr));
      fNamedParams[I-LNamedArgStart].Index := I;
      fNamedParams[I-LNamedArgStart].Name  := String(LNamePtr);
    end;

    // error is an easy expansion
    if LArgType = varError then
      SetClearVarToEmptyParam(LArguments[I])

    // literal string
    else if LArgType = varStrArg then
    begin
      with LStrings[LStrCount] do
        if LArgByRef then
        begin
          //BStr := StringToOleStr(PString(ParamPtr^)^);
          BStr := System.Copy(PString(LParamPtr^)^, 1, MaxInt);
          PStr := PString(LParamPtr^);
          LArguments[I].VType := varOleStr or varByRef;
          LArguments[I].VOleStr := @BStr;
        end
        else
        begin
          //BStr := StringToOleStr(PString(LParamPtr)^);
          BStr := System.Copy(PString(LParamPtr)^, 1, MaxInt);
          PStr := nil;
          LArguments[I].VType := varOleStr;
          if BStr = '' then
            LArguments[I].VOleStr := nil
          else
            LArguments[I].VOleStr := PWideChar(BStr);
        end;
      Inc(LStrCount);
    end

    // value is by ref
    else if LArgByRef then
    begin
      if (LArgType = varVariant) and
         (PVarData(LParamPtr^)^.VType = varString) then
        //VarCast(PVariant(ParamPtr^)^, PVariant(ParamPtr^)^, varOleStr);
        VarDataCastTo(PVarData(LParamPtr^)^, PVarData(LParamPtr^)^, varOleStr);
      LArguments[I].VType := LArgType or varByRef;
      LArguments[I].VPointer := Pointer(LParamPtr^);
    end

    // value is a variant
    else if LArgType = varVariant then
      if PVarData(LParamPtr)^.VType = varString then
      begin
        with LStrings[LStrCount] do
        begin
          //BStr := StringToOleStr(string(PVarData(LParamPtr)^.VString));
          BStr := System.Copy(string(PVarData(LParamPtr)^.VString), 1, MaxInt);
          PStr := nil;
          LArguments[I].VType := varOleStr;
          LArguments[I].VOleStr := PWideChar(BStr);
        end;
        Inc(LStrCount);
        Inc(Integer(LParamPtr), SizeOf(TVarData) - SizeOf(Pointer));
      end
      else
      begin
        LArguments[I] := PVarData(LParamPtr)^;
        Inc(Integer(LParamPtr), SizeOf(TVarData) - SizeOf(Pointer));
      end
    else
    begin
      {$IFDEF FPC}
        {$HINT CVarTypeToElementInfo not yet implemented}
        RaiseDispError;
      {$ELSE}
      LArguments[I].VType := LArgType;
      case CVarTypeToElementInfo[LArgType].Size of
        1, 2, 4:
        begin
          LArguments[I].VLongs[1] := PParamRec(LParamPtr)^[0];
        end;
        8:
        begin
          LArguments[I].VLongs[1] := PParamRec(LParamPtr)^[0];
          LArguments[I].VLongs[2] := PParamRec(LParamPtr)^[1];
          Inc(Integer(LParamPtr), 8 - SizeOf(Pointer));
        end;
      else
        RaiseDispError;
      end;
      {$ENDIF}
    end;
    Inc(Integer(LParamPtr), SizeOf(Pointer));
  end;

var
  I, LArgCount: Integer;
  LIdent: string;
  LTemp: TVarData;
begin
  //------------------------------------------------------------------------------------
  // Note that this method is mostly a copy&paste from  TInvokeableVariantType.DispInvoke
  // because Borland assumes that the names are not case sensitive, whereas Python has
  // case sensitive symbols.
  // We modified the property get to allow the use of indexed properties.
  //------------------------------------------------------------------------------------

  // Grab the identifier
  LArgCount := CallDesc^.ArgCount;
  //After arg types, method name and named arg names are stored
  //Position pointer on method name
  LNamePtr := PChar(@CallDesc^.ArgTypes[LArgCount]);
  LIdent := String(LNamePtr);
  //Named params must be after positional params
  LNamedArgStart := CallDesc^.ArgCount - CallDesc^.NamedArgCount;
  SetLength(fNamedParams, CallDesc^.NamedArgCount);

  // Parse the arguments
  LParamPtr := Params;
  SetLength(LArguments, LArgCount);
  LStrCount := 0;
  SetLength(LStrings, LArgCount);
  for I := 0 to LArgCount - 1 do
    ParseParam(I);

  // What type of invoke is this?
  case CallDesc^.CallType of
    CDoMethod:
      // procedure with N arguments
      if Dest = nil then
      begin
        if not DoProcedure(Source, LIdent, LArguments) then
        begin

          // ok maybe its a function but first we must make room for a result
          VarDataInit(LTemp);
          try

            // notate that the destination shouldn't be bothered with
            // functions can still return stuff, we just do this so they
            //  can tell that they don't need to if they don't want to
            SetClearVarToEmptyParam(LTemp);

            // ok lets try for that function
            if not DoFunction(LTemp, Source, LIdent, LArguments) then
              RaiseDispError;
          finally
            VarDataClear(LTemp);
          end;
        end
      end

      // property get or function with 0 argument
      else if LArgCount = 0 then
      begin
        if not GetProperty(Dest^, Source, LIdent) and
           not DoFunction(Dest^, Source, LIdent, LArguments) then
          RaiseDispError;
      end

      // function with N arguments
      else if not DoFunction(Dest^, Source, LIdent, LArguments) then
        RaiseDispError;

    CPropertyGet:
    begin
      // here that code has been changed to allow the indexed properties.

      if Dest = nil then // there must be a dest
        RaiseDispError;
      if LArgCount = 0 then // no args
      begin
        if not GetProperty(Dest^, Source, LIdent) then   // get op be valid
          RaiseDispError;
      end
      else if LArgCount = 1 then // only one arg
      begin
        if not GetPropertyWithArg(Dest^, Source, LIdent, LArguments[0]) then   // get op be valid
          RaiseDispError;
      end
      else
        raise Exception.Create( SMultiDimensionalPropsNotSupported );
    end;

    CPropertySet:
      if not ((Dest = nil) and                         // there can't be a dest
              (LArgCount = 1) and                       // can only be one arg
              SetProperty(Source, LIdent, LArguments[0])) then // set op be valid
        RaiseDispError;
  else
    RaiseDispError;
  end;

  // copy back the string info
  I := LStrCount;
  while I <> 0 do
  begin
    Dec(I);
    with LStrings[I] do
      if Assigned(PStr) then
        PStr^ := System.Copy(BStr, 1, MaxInt);
  end;
end;

function TPythonVariantType.DoFunction(var Dest: TVarData;
  const V: TVarData; const AName: string;
  const Arguments: TVarDataArray): Boolean;
var
  _PyResult : PPyObject;
begin
  // eval the function call
  _PyResult := EvalPython(V, AName, Arguments);
  try
    Result := Assigned(_PyResult);
    // if the evaluation returned a result
    if Result then
      // convert it into a variant
      PythonObjectToVarData( Dest, _PyResult, TPythonVarData(V).VPython.PythonAtomCompatible );
  finally
    GetPythonEngine.Py_XDecRef( _PyResult );
  end; // of try
end;

function TPythonVariantType.DoProcedure(const V: TVarData;
  const AName: string; const Arguments: TVarDataArray): Boolean;
var
  _PyResult : PPyObject;
begin
  _PyResult := EvalPython(V, AName, Arguments);
  Result := Assigned(_PyResult);
  GetPythonEngine.Py_XDecRef( _PyResult );
end;

function TPythonVariantType.EvalPython(const V: TVarData;
  const AName: string; const Arguments: TVarDataArray): PPyObject;

  function ArgAsPythonObject( AArgIndex : Integer ) : PPyObject;
  begin
    Result := VarDataToPythonObject(Arguments[AArgIndex]);
    // if conversion failed, then too bad ;-)
    if not Assigned(Result) then
      raise Exception.CreateFmt(SCantConvertArg, [AArgIndex, AName]);
  end; // of function

  function GetObjectItem( AObject : PPyObject; const AKey : TVarData ) : PPyObject;
  var
    _key : PPyObject;
  begin
    _key := VarDataToPythonObject(AKey);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        Result := PyObject_GetItem( AObject, _key );
        CheckError;
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  function SetObjectItem( AObject : PPyObject; const AKey, AValue : TVarData ) : PPyObject;
  var
    _key, _value : PPyObject;
    _result : Integer;
  begin
    Result := nil;
    with GetPythonEngine do
    begin
      PyErr_Clear;
      _key := VarDataToPythonObject(AKey);
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        _value := VarDataToPythonObject(AValue);
        if not Assigned(_value) then
          raise Exception.Create(SCantConvertValueToPythonObject);
          if PyList_Check(AObject) then
            _result := PyList_SetItem( AObject, Variant(AKey), _value )
          else if PyTuple_Check(AObject) then
            _result := PyTuple_SetItem( AObject, Variant(AKey), _value )
          else
            try
              if PySequence_Check(AObject) <> 0 then
                _result := PySequence_SetItem(AObject, Variant(AKey), _value)
              else
                _result := PyObject_SetItem( AObject, _key, _value );
            finally
              Py_XDecRef(_value);
            end; // of try
          CheckError;
          Result := PyInt_FromLong(_result);
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  function DeleteObjectItem( AObject : PPyObject; const AKey : TVarData ) : PPyObject;
  var
    _key : PPyObject;
  begin
    _key := VarDataToPythonObject(AKey);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        PyObject_DelItem( AObject, _key );
        CheckError;
        Result := ReturnNone;
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  procedure ExtractSliceIndexes(AObject : PPyObject; const AStart, AEnd: TVarData; var ASliceStart, ASliceEnd : Integer );
  begin
    with GetPythonEngine do
    begin
      if VarIsSame(Variant(AStart), Ellipsis) then
        ASliceStart := 0
      else
        ASliceStart := Variant(AStart);
      if VarIsSame(Variant(AEnd), Ellipsis) then
        ASliceEnd := PySequence_Length(AObject)
      else
        ASliceEnd := Variant(AEnd);
      CheckError;
    end; // of with
  end;

  function GetSequenceSlice( AObject : PPyObject; const AStart, AEnd: TVarData ) : PPyObject;
  var
    _start, _end : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      Result := PySequence_GetSlice( AObject, _start, _end);
      CheckError;
    end; // of with
  end; // of function

  function SetSequenceSlice( AObject : PPyObject; const AStart, AEnd, AValue : TVarData ) : PPyObject;
  var
    _start, _end : Integer;
    _value : PPyObject;
    _result : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      _value := VarDataToPythonObject(AValue);
      if not Assigned(_value) then
        raise Exception.Create(SCantConvertValueToPythonObject);
      try
        _result := PySequence_SetSlice( AObject, _start, _end, _value);
        CheckError;
        Result := PyInt_FromLong(_result);
      finally
        Py_XDecRef(_value);
      end; // of try
    end; // of with
  end; // of function

  function DelSequenceSlice( AObject : PPyObject; const AStart, AEnd: TVarData ) : PPyObject;
  var
    _start, _end, _result : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      _result := PySequence_DelSlice( AObject, _start, _end);
      CheckError;
      Result := PyInt_FromLong(_result);
    end; // of with
  end; // of function

  function SequenceContains( AObject : PPyObject; const AValue : TVarData ) : PPyObject;
  var
    _value : PPyObject;
    _result : Integer;
  begin
    _value := VarDataToPythonObject(AValue);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_value) then
        raise Exception.Create(SCantConvertValueToPythonObject);
      try
{$IFDEF PYTHON20_OR_HIGHER}
        _result := PySequence_Contains( AObject, _value );
{$ELSE}
        Result := nil;
        raise Exception.Create('Not implemented in this version of Python');
{$ENDIF}
        CheckError;
        Result := PyInt_FromLong(_result);
      finally
        Py_XDecRef(_value);
      end; // of try
    end; // of with
  end; // of function

var
  i : Integer;
  _container : PPyObject;
  _obj : PPyObject;
  _Arg : PPyObject;
  _Args : PPyObject;
  _ArgLen : Integer;
  _KW : PPyObject;
begin
  Result := nil;
  with GetPythonEngine do
  begin
    // extract the associated Python object
    _container := TPythonVarData(V).VPython.PyObject;

    // extract the key from the container object
    _obj := PyObject_GetAttrString(_container, PChar(AName));
    try
      try
        // if the container object does not have the key AName
        if PyErr_Occurred <> nil then
        begin
          // here we handle a special case: COM (or Delphi?) doesn't allow you to have index properties
          // on a variant except from a true array variant! So if myVar is a Python variant that holds
          // a Python list, I can't write something like: myVar[0]! But if the list is a member of a
          // Python instance, it will work fine! Like: myInst.myList[0]
          // So, to handle this problem we detect some special names: GetItem, SetItem and Length
          // that will do the same as:
          // myList[0] <-> myList.GetItem(0)
          // myDict['Hello'] := 1 <-> myDict.SetItem('Hello', 1)
          // len(myList) <-> myList.Length()
          // we get some bonus with the slices and in operators also:
          // myList = [0, 1, 2, 3]; myList.GetSlice(1, 2) --> [1, 2]
          // myList.Contains(2) <-> 2 in myList

          if (Length(Arguments) = 1) and SameText(AName, 'GetItem') then
            Result := GetObjectItem(_container, Arguments[0])
          else if (Length(Arguments) = 2) and SameText(AName, 'SetItem') then
            Result := SetObjectItem(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 1) and SameText(AName, 'DeleteItem') then
            Result := DeleteObjectItem(_container, Arguments[0])
          else if (Length(Arguments) = 2) and SameText(AName, 'GetSlice') then
            Result := GetSequenceSlice(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 3) and SameText(AName, 'SetSlice') then
            Result := SetSequenceSlice(_container, Arguments[0], Arguments[1], Arguments[2])
          else if (Length(Arguments) = 2) and SameText(AName, 'DelSlice') then
            Result := DelSequenceSlice(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 1) and SameText(AName, 'Contains') then
            Result := SequenceContains(_container, Arguments[0])
          else if SameText(AName, 'Length') then
            Result := PyInt_FromLong( GetObjectLength(_container) );
        end; // of if
      finally
        // if the key did not exist, Python generated an exception that we must propagate through CheckError
        CheckError;
      end; // of try
      // exit now if our special functions could apply
      if Assigned(Result) then
        Exit;
      // if we found the key
      if Assigned(_obj) then
        // if the associated object is callable (a method or a function)
        if PyCallable_Check(_obj) <> 0 then
        begin
          // Prepare the Python arguments for calling the func or method.

          // there's a special case: if we call a Python method without any argument (like myList.sort()),
          // the arguments list contains a single error variant.
          if (Length(Arguments) = 1) and VarDataIsEmptyParam(Arguments[0]) then
            _ArgLen := 0
          else
            _ArgLen := Length(Arguments);
          if Length(fNamedParams) > 0 then
          begin
            _KW := PyDict_New;
            _ArgLen := fNamedParams[0].Index;
          end
          else
            _KW := nil;
          try
            _Args := PyTuple_New(_ArgLen);
            try
              for i := 0 to _ArgLen-1 do
                PyTuple_SetItem( _Args, i, ArgAsPythonObject(i) );
              for i := 0 to Length(fNamedParams)-1 do
                PyDict_SetItemString(_KW, PChar(fNamedParams[i].Name), ArgAsPythonObject(fNamedParams[i].Index));

              // call the func or method, with or without named parameters (KW)
              if Assigned(_KW) then
                Result := PyEval_CallObjectWithKeywords(_obj, _Args, _KW)
              else
                Result := PyEval_CallObject(_obj, _Args);
              CheckError(True);
            finally
              Py_XDecRef(_Args);
            end; // of try
          finally
            Py_XDecRef(_KW);
          end;
        end // of if
        // if we have one argument and our associated object is not callable
        // it could mean that we're trying to access an item of a sequence or
        // a mapping, like myInst.MyList(1)
        else if Length(Arguments) = 1 then
        begin
          // if we have a sequence or a mapping object
          if (PySequence_Check(_obj) <> 0) or (PyMapping_Check(_obj) <> 0) then
          begin
            // convert the variant argument into a Python object
            _Arg := VarDataToPythonObject(Arguments[0]);
            try
              // get the item
              Result := PyObject_GetItem(_obj, _Arg);
              CheckError;
            finally
              Py_XDecRef(_Arg);
            end; // of try
          end; // of if
        end // of else if
        else
          raise Exception.Create( SMultiDimensionalPropsNotSupported );
    finally
      Py_XDecRef(_obj);
    end; // of try
  end; // of with
end;

function TPythonVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TPythonVarData(V).VPython;
end;

function TPythonVariantType.GetProperty(var Dest: TVarData;
  const V: TVarData; const AName: string): Boolean;
var
  _prop : PPyObject;
  _len : Integer;
  _args : PPyObject;
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _prop := PyObject_GetAttrString(TPythonVarData(V).VPython.PyObject, PChar(AName));
    // if we could not find the property
    if (PyErr_Occurred <> nil) or not Assigned(_prop) then
    begin
      // test a special property name for sequences only
      if SameText(AName, 'Length') then
      begin
        // if it's a sequence or a mapping
        if (PySequence_Check(TPythonVarData(V).VPython.PyObject) <> 0) or
           (PyMapping_Check(TPythonVarData(V).VPython.PyObject) <> 0) then
        begin
          // clear the error state of Python
          PyErr_Clear;
          // get the length
          _len := PyObject_Length(TPythonVarData(V).VPython.PyObject);
          CheckError;
          // convert the length into a Python integer
          _prop := PyInt_FromLong( _len );
        end; // of if
      end
      else if SameText(AName, '__asPPyObject__') then // compatibility with PythonAtom.
      begin                                           // you should use ExtractPythonObjectFrom instead.
        // clear the error state of Python
        PyErr_Clear;
        // return the Python object pointer as an Integer;
        _prop := PyInt_FromLong( Integer(TPythonVarData(V).VPython.PyObject) );
      end;
    end // of if
    // if we found a property that's a callable object and if we're in
    // compatibility mode with PythonAtom, then return the result of the
    // execution of the callable object.
    else if TPythonVarData(V).VPython.PythonAtomCompatible and
            Assigned(_prop) and (PyErr_Occurred = nil) then
    begin
      if PyFunction_Check(_prop) or PyMethod_Check(_prop) then
      begin
        _args := PyTuple_New(0);
        try
          // call the func or method
          _result := PyObject_CallObject(_prop, _args);
          if Assigned(_result) and (PyErr_Occurred = nil) then
          begin
            Py_XDecRef(_prop);
            _prop := _result;
          end; // of if
        finally
          Py_XDecRef(_args);
        end; // of try
      end; // of if
    end; // of if
    CheckError;
    Result := Assigned(_prop);
    if Result then
      try
        PythonObjectToVarData(Dest, _prop, TPythonVarData(V).VPython.PythonAtomCompatible);
      finally
        Py_XDecRef(_prop);
      end; // of try
  end; // of with
end;

function TPythonVariantType.GetPropertyWithArg(var Dest: TVarData;
  const V: TVarData; const AName: string; AArg: TVarData): Boolean;
var
  _prop, _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := nil;
    _prop := PyObject_GetAttrString(TPythonVarData(V).VPython.PyObject, PChar(AName));
    CheckError;
    if Assigned(_prop) then
    begin
      // here we check only sequences, as Delphi does not allow a type different from Integer
      // to be used within brackets.
      // But you can still access a dictionary with parenthesis, like: myObj.MyDict('MyKey')
      // Note that we can't use the brackets on a Python variant that contains a list,
      // because Delphi thinks it's a variant array, whereas it is not, of course!
      // So: myList[0] won't work, but myObj.MyList[0] will!!!
      if PySequence_Check(_prop) <> 0 then
      begin
        _result := PySequence_GetItem(_prop, Variant(AArg));
        CheckError;
      end; // of if
    end; // of if
    Result := Assigned(_result);
    if Result then
      try
        PythonObjectToVarData(Dest, _result, TPythonVarData(V).VPython.PythonAtomCompatible);
      finally
        Py_XDecRef(_prop);
      end; // of try
  end; // of with
end;

function TPythonVariantType.IsClear(const V: TVarData): Boolean;
begin
  Result := (TPythonVarData(V).VPython = nil) or
            TPythonVarData(V).VPython.IsNone;
end;

function TPythonVariantType.LeftPromotion(const V: TVarData;
  const AOperator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  { TypeX Op Python }
  if (AOperator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;

  Result := True;
end;

// this method increases the ref count of AObject
procedure TPythonVariantType.PyhonVarDataCreate(var Dest: TVarData;
  AObject: PPyObject);
begin
  VarDataClear(Dest);
  if Assigned(AObject) then
  begin
    TPythonVarData(Dest).VType := VarPython;
    TPythonVarData(Dest).VPython := TPythonData.Create(AObject);
  end; // of if
end;

// this method increases the refcount of AObject
procedure TPythonVariantType.PythonObjectToVarData(var Dest: TVarData;
  AObject: PPyObject; APythonAtomCompatible : Boolean);

  function IsBasicType( AObject : PPyObject ) : Boolean;
  begin
    with GetPythonEngine do
      Result := PyInt_Check(AObject) or
                PyFloat_Check(AObject) or
                PyString_Check(AObject) or
                PyList_Check(AObject) or
                PyTuple_Check(AObject);
  end;

begin
  if APythonAtomCompatible and (AObject = GetPythonEngine.Py_None) then
    VarDataClear(Dest)
  // if the result is a basic type (integer, float, string or list)
  else if APythonAtomCompatible and IsBasicType( AObject ) then
  begin
    VarDataClear(Dest);
    // convert it into its associated variant type
    Variant(Dest) := GetPythonEngine.PyObjectAsVariant(AObject);
  end
  else
  begin
    // otherwise wrap the result into a new Python variant
    PyhonVarDataCreate(Dest, AObject);
    // propagate compatibility mode
    TPythonVarData(Dest).VPython.fPythonAtomCompatible := APythonAtomCompatible;
  end; // of else
end;

function TPythonVariantType.RightPromotion(const V: TVarData;
  const AOperator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  { Python Op TypeX }
  // Note that the Int64 doesn't is an exception, because the expr Python op Int64,
  // won't cast the Int64 to Python.
  // See unit Variants, function VarCastRare.
  if V.VType <> varInt64 then
  begin
    RequiredVarType := VarType;
    Result := True;
  end
  else
    Result := False;
end;

function TPythonVariantType.SetProperty(const V: TVarData;
  const AName: string; const Value: TVarData): Boolean;
var
  _newValue : PPyObject;
begin
  with GetPythonEngine do
  begin
    _newValue := VarDataToPythonObject(Value);
    try
      PyObject_SetAttrString(TPythonVarData(V).VPython.PyObject, PChar(AName), _newValue );
      CheckError;
    finally
      Py_XDecRef(_newValue);
    end; // of try
  end; // of with
  Result := True;
end;

procedure TPythonVariantType.UnaryOp(var Right: TVarData;
  const AOperator: TVarOp);
begin
  if Right.VType = VarType then
    case AOperator of
      opNegate:
        TPythonVarData(Right).VPython.DoNegate;
      opNot:
        TPythonVarData(Right).VPython.DoNot;
    else
      RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;

function TPythonVariantType.VarDataToPythonObject(AVarData: TVarData): PPyObject;
var
  _PVarData : PVarData;
begin
  Result := nil;
  // first detect if we have a Python variant as argument and extract its associated Python object
  if AVarData.VType = VarType then
  begin
    Result := TPythonVarData(AVarData).VPython.PyObject;
    GetPythonEngine.Py_XINCREF(Result);
  end // of if
  else if VarDataIsByRef(AVarData) and
          ((AVarData.VType and varTypeMask) = varVariant) then
  begin
    _PVarData := AVarData.VPointer;
    if _PVarData^.VType = VarType then
    begin
      Result := TPythonVarData(_PVarData^).VPython.PyObject;
      GetPythonEngine.Py_XINCREF(Result);
    end; // of if
  end // of if
  else if (AVarData.VType and varTypeMask) = varNull then
    Result := GetPythonEngine.ReturnNone
  else if (AVarData.VType and varTypeMask) = varEmpty then
    Result := GetPythonEngine.ReturnNone;
  // else try to convert the variant into a Python object
  if not Assigned(Result) then
    Result := GetPythonEngine.VariantAsPyObject(Variant(AVarData));
end;

//------------------------------------------------------------------------------
{ TPythonData }

function TPythonData.Compare(const Right: TPythonData): Integer;
begin
  with GetPythonEngine do
  begin
    Result := PyObject_Compare(PyObject, Right.PyObject);
    CheckError;
  end; // of with
end;

constructor TPythonData.Create(AObject: PPyObject);
begin
  PyObject := AObject;
  fPythonAtomCompatible := False;
end;

constructor TPythonData.Create(AObject : PPyObject; APythonAtomCompatible : Boolean);
begin
  Create(AObject);
  fPythonAtomCompatible := APythonAtomCompatible;
end;

destructor TPythonData.Destroy;
begin
  if PythonOK then
    PyObject := nil;
  inherited;
end;

procedure TPythonData.DoAdd(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Add(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoAnd(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_And(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoDivide(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
{$IFDEF PYTHON22_OR_HIGHER}
    _result := PyNumber_TrueDivide(PyObject, Right.PyObject);
{$ELSE}
    _result := PyNumber_Divide(PyObject, Right.PyObject);
{$ENDIF}
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoIntDivide(const Right: TPythonData);
var
  _result : PPyObject;
{$IFNDEF PYTHON22_OR_HIGHER}
  _op1, _op2 : Integer;
{$ENDIF}
begin
  with GetPythonEngine do
  begin
{$IFDEF PYTHON22_OR_HIGHER}
    _result := PyNumber_FloorDivide( PyObject, Right.PyObject );
{$ELSE}
    //TODO: check for long objects
    if not PyInt_Check(PyObject) or not PyInt_Check(Right.PyObject) then
      raise Exception.Create(SBothOperandsOfIntDivideMustBeIntegers);
    _op1 := PyInt_AsLong(PyObject);
    _op2 := PyInt_AsLong(Right.PyObject);
    _result := PyInt_FromLong( _op1 div _op2 );
{$ENDIF}
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoModulus(const Right: TPythonData);
var
  _item, _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Divmod(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) and (PySequence_Check(_result) <> 0) and
       (PySequence_Length(_result) >= 2) then
    begin
      _item := PySequence_GetItem(_result, 1);
      PyObject := _item;
      Py_XDecRef(_item);
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoMultiply(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Multiply(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoNegate;
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Negative(PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoNot;
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Invert(PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoOr(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Or(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoShiftLeft(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_LShift(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoShiftRight(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_RShift(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoSubtract(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Subtract(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoXor(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Xor(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

function TPythonData.Equal(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsPython3000 then
      Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_EQ) = 1
    else
      Result := PyObject_Compare(PyObject, Right.PyObject) = 0;
    CheckError;
  end; // of with
end;

function TPythonData.GetAsString: String;
begin
  if Assigned(PyObject) then
    Result := GetPythonEngine.PyObjectAsString(PyObject)
  else
    result := '';
end;

function TPythonData.GetAsVariant: Variant;
begin
  if Assigned(PyObject) then
    Result := GetPythonEngine.PyObjectAsVariant(PyObject)
  else
    Result := Null;
end;

function TPythonData.GetAsWideString: WideString;
begin
{$IFDEF UNICODE_SUPPORT}
  if Assigned(PyObject) and GetPythonEngine.PyUnicode_Check(PyObject) then
    Result := GetPythonEngine.PyUnicode_AsWideString(PyObject)
  else
{$ENDIF}
    Result := GetAsString;
end;

function TPythonData.GreaterOrEqualThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsPython3000 then
      Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_GE) = 1
    else
      Result := Self.Compare(Right) >= 0;
  end;
end;

function TPythonData.GreaterThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsPython3000 then
      Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_GT) = 1
    else
      Result := Self.Compare(Right) > 0;
  end;
end;

function TPythonData.IsNone: Boolean;
begin
  Result := PyObject = GetPythonEngine.Py_None;
end;

function TPythonData.LessOrEqualThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsPython3000 then
      Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_LE) = 1
    else
      Result := Self.Compare(Right) <= 0;
  end;
end;

function TPythonData.LessThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsPython3000 then
      Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_LT) = 1
    else
      Result := Self.Compare(Right) < 0;
  end;
end;

procedure TPythonData.SetPyObject(const Value: PPyObject);
begin
  with GetPythonEngine do
  begin
    Py_XDecRef(fPyObject);
    fPyObject := Value;
    Py_XIncRef(fPyObject);
  end;
end;

initialization
  PythonVariantType := TPythonVariantType.Create;
finalization
  FreeAndNil(PythonVariantType);
end.
