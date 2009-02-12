(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonEngine'     Copyright (c) 1997                    *)
(*                                                                        *)
(* Version: 3.0                     Dr. Dietmar Budelsky                  *)
(* Sub-Version: 0.33                dbudelsky@web.de                      *)
(*                                  Germany                               *)
(*                                                                        *)
(*                                  Morgan Martinet                       *)
(*                                  4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*  look at our page at: http://mmm-experts.com/                          *)
(*      and our Wiki at: http://py4d.pbwiki.com/                          *)
(**************************************************************************)
(*  Functionality:  Delphi Components that provide an interface to the    *)
(*                  Python language (see python.txt for more infos on     *)
(*                  Python itself).                                       *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Andrew Robinson (andy@hps1.demon.co.uk)                           *)
(*      Mark Watts(mark_watts@hotmail.com)                                *)
(*      Olivier Deckmyn (olivier.deckmyn@mail.dotcom.fr)                  *)
(*      Sigve Tjora (public@tjora.no)                                     *)
(*      Mark Derricutt (mark@talios.com)                                  *)
(*      Igor E. Poteryaev (jah@mail.ru)                                   *)
(*      Yuri Filimonov (fil65@mail.ru)                                    *)
(*      Stefan Hoffmeister (Stefan.Hoffmeister@Econos.de)                 *)
(*      Michiel du Toit (micdutoit@hsbfn.com) - Lazarus Port              *)
(*      Chris Nicolai (nicolaitanes@gmail.com)                            *)
(*      Kiriakos Vlahos (kvlahos@london.edu)                              *)
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
(* Dr. Dietmar Budelsky, 1997-11-17                                       *)
(**************************************************************************)

unit PythonEngine;

{ TODO -oMMM : implement tp_as_buffer slot }
{ TODO -oMMM : implement Attribute descriptor and subclassing stuff }

{$I Definition.Inc}

{$IFDEF PYTHON30_OR_HIGHER}
  {$IFNDEF UNICODE_SUPPORT}
    Error! Python 3.0 only supports UNICODE!
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  Libc,
{$ENDIF}
  Classes,
  SysUtils,
{$IFDEF HAS_SYNCOBJS_UNIT}
  SyncObjs,
{$ENDIF}
{$IFDEF DELPHI6_OR_HIGHER}
  Variants,
{$ELSE}
  {$IFDEF FPC}
    Variants,
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI2005_OR_HIGHER}
  WideStrings,
{$ELSE}
  TinyWideStrings,
{$ENDIF}
  MethodCallBack;

//#######################################################
//##                                                   ##
//##           PYTHON specific constants               ##
//##                                                   ##
//#######################################################

type
  TPythonVersionProp = packed record
    DllName      : String;
    RegVersion   : String;
    APIVersion   : Integer;
    CanUseLatest : Boolean;
  end;
const
{$IFDEF MSWINDOWS}
  PYTHON_KNOWN_VERSIONS: array[1..11] of TPythonVersionProp =
  ( (DllName: 'python14.dll'; RegVersion: '1.4'; APIVersion: 1006; CanUseLatest: False),
    (DllName: 'python15.dll'; RegVersion: '1.5'; APIVersion: 1007; CanUseLatest: False),
    (DllName: 'python16.dll'; RegVersion: '1.6'; APIVersion: 1008; CanUseLatest: False),
    (DllName: 'python20.dll'; RegVersion: '2.0'; APIVersion: 1009; CanUseLatest: True),
    (DllName: 'python21.dll'; RegVersion: '2.1'; APIVersion: 1010; CanUseLatest: True),
    (DllName: 'python22.dll'; RegVersion: '2.2'; APIVersion: 1011; CanUseLatest: True),
    (DllName: 'python23.dll'; RegVersion: '2.3'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'python24.dll'; RegVersion: '2.4'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'python25.dll'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python26.dll'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python30.dll'; RegVersion: '3.0'; APIVersion: 1013; CanUseLatest: True) );
{$ENDIF}
{$IFDEF LINUX}
  PYTHON_KNOWN_VERSIONS: array[1..11] of TPythonVersionProp =
  ( (DllName: 'libpython1.4.so'; RegVersion: '1.4'; APIVersion: 1006; CanUseLatest: False),
    (DllName: 'libpython1.5.so'; RegVersion: '1.5'; APIVersion: 1007; CanUseLatest: False),
    (DllName: 'libpython1.6.so'; RegVersion: '1.6'; APIVersion: 1008; CanUseLatest: False),
    (DllName: 'libpython2.0.so'; RegVersion: '2.0'; APIVersion: 1009; CanUseLatest: True),
    (DllName: 'libpython2.1.so'; RegVersion: '2.1'; APIVersion: 1010; CanUseLatest: True),
    (DllName: 'libpython2.2.so'; RegVersion: '2.2'; APIVersion: 1011; CanUseLatest: True),
    (DllName: 'libpython2.3.so'; RegVersion: '2.3'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'libpython2.4.so'; RegVersion: '2.4'; APIVersion: 1012; CanUseLatest: True),
    (DllName: 'libpython2.5.so'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.6.so'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.0.so'; RegVersion: '3.0'; APIVersion: 1013; CanUseLatest: True) );
{$ENDIF}
{$IFDEF PYTHON14}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 1;
{$ENDIF}
{$IFDEF PYTHON15}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 2;
{$ENDIF}
{$IFDEF PYTHON16}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 3;
{$ENDIF}
{$IFDEF PYTHON20}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 4;
{$ENDIF}
{$IFDEF PYTHON21}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 5;
{$ENDIF}
{$IFDEF PYTHON22}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 6;
{$ENDIF}
{$IFDEF PYTHON23}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 7;
{$ENDIF}
{$IFDEF PYTHON24}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 8;
{$ENDIF}
{$IFDEF PYTHON25}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 9;
{$ENDIF}
{$IFDEF PYTHON26}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 10;
{$ENDIF}
{$IFDEF PYTHON30}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 11;
{$ENDIF}

  PYT_METHOD_BUFFER_INCREASE = 10;
  PYT_MEMBER_BUFFER_INCREASE = 10;
  PYT_GETSET_BUFFER_INCREASE = 10;

  METH_VARARGS  = $0001;
  METH_KEYWORDS = $0002;

  // Masks for the co_flags field of PyCodeObject
  CO_OPTIMIZED   = $0001;
  CO_NEWLOCALS   = $0002;
  CO_VARARGS     = $0004;
  CO_VARKEYWORDS = $0008;

  // Rich comparison opcodes introduced in version 2.1
  Py_LT = 0;
  Py_LE = 1;
  Py_EQ = 2;
  Py_NE = 3;
  Py_GT = 4;
  Py_GE = 5;
type
  // Delphi equivalent used by TPyObject
  TRichComparisonOpcode = (pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE);
const
{Type flags (tp_flags) introduced in version 2.0

These flags are used to extend the type structure in a backwards-compatible
fashion. Extensions can use the flags to indicate (and test) when a given
type structure contains a new feature. The Python core will use these when
introducing new functionality between major revisions (to avoid mid-version
changes in the PYTHON_API_VERSION).

Arbitration of the flag bit positions will need to be coordinated among
all extension writers who publically release their extensions (this will
be fewer than you might expect!)..

Python 1.5.2 introduced the bf_getcharbuffer slot into PyBufferProcs.

Type definitions should use Py_TPFLAGS_DEFAULT for their tp_flags value.

Code can use PyType_HasFeature(type_ob, flag_value) to test whether the
given type object has a specified feature.
}

// PyBufferProcs contains bf_getcharbuffer
  Py_TPFLAGS_HAVE_GETCHARBUFFER = (1 shl 0);

// PySequenceMethods contains sq_contains
  Py_TPFLAGS_HAVE_SEQUENCE_IN = (1 shl 1);

// Objects which participate in garbage collection (see objimp.h)
  Py_TPFLAGS_GC = (1 shl 2);

// PySequenceMethods and PyNumberMethods contain in-place operators
  Py_TPFLAGS_HAVE_INPLACEOPS = (1 shl 3);

// PyNumberMethods do their own coercion */
  Py_TPFLAGS_CHECKTYPES = (1 shl 4);

  Py_TPFLAGS_HAVE_RICHCOMPARE = (1 shl 5);

// Objects which are weakly referencable if their tp_weaklistoffset is >0
// XXX Should this have the same value as Py_TPFLAGS_HAVE_RICHCOMPARE?
// These both indicate a feature that appeared in the same alpha release.

  Py_TPFLAGS_HAVE_WEAKREFS = (1 shl 6);

{$IFDEF PYTHON22_OR_HIGHER}
// tp_iter is defined
  Py_TPFLAGS_HAVE_ITER = (1 shl 7);

// New members introduced by Python 2.2 exist
  Py_TPFLAGS_HAVE_CLASS = (1 shl 8);

// Set if the type object is dynamically allocated
  Py_TPFLAGS_HEAPTYPE = (1 shl 9);

// Set if the type allows subclassing
  Py_TPFLAGS_BASETYPE = (1 shl 10);

// Set if the type is 'ready' -- fully initialized
  Py_TPFLAGS_READY = (1 shl 12);

// Set while the type is being 'readied', to prevent recursive ready calls
  Py_TPFLAGS_READYING = (1 shl 13);

// Objects support garbage collection (see objimp.h)
  Py_TPFLAGS_HAVE_GC = (1 shl 14);
{$ENDIF}

  Py_TPFLAGS_DEFAULT  =      Py_TPFLAGS_HAVE_GETCHARBUFFER
                             or Py_TPFLAGS_HAVE_SEQUENCE_IN
                             or Py_TPFLAGS_HAVE_INPLACEOPS
                             or Py_TPFLAGS_HAVE_RICHCOMPARE
                             or Py_TPFLAGS_HAVE_WEAKREFS
{$IFDEF PYTHON22_OR_HIGHER}
                             or Py_TPFLAGS_HAVE_ITER
                             or Py_TPFLAGS_HAVE_CLASS
{$ENDIF}
                             ;

// See function PyType_HasFeature below for testing the flags.

// Delphi equivalent used by TPythonType
type
  TPFlag = (tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfGC, tpfHaveInplaceOps,
            tpfCheckTypes, tpfHaveRichCompare, tpfHaveWeakRefs
{$IFDEF PYTHON22_OR_HIGHER}
            ,tpfHaveIter, tpfHaveClass, tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC
{$ENDIF}
            );
  TPFlags = set of TPFlag;
const
  TPFLAGS_DEFAULT = [tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfHaveInplaceOps,
                     tpfHaveRichCompare, tpfHaveWeakRefs
{$IFDEF PYTHON22_OR_HIGHER}
                     , tpfHaveIter, tpfHaveClass
{$ENDIF}
                    ];
//-------  Python opcodes  ----------//
Const
{$IFDEF PYTHON20_OR_HIGHER}
   single_input                 = 256;
   file_input                   = 257;
   eval_input                   = 258;
   funcdef                      = 259;
   parameters                   = 260;
   varargslist                  = 261;
   fpdef                        = 262;
   fplist                       = 263;
   stmt                         = 264;
   simple_stmt                  = 265;
   small_stmt                   = 266;
   expr_stmt                    = 267;
   augassign                    = 268;
   print_stmt                   = 269;
   del_stmt                     = 270;
   pass_stmt                    = 271;
   flow_stmt                    = 272;
   break_stmt                   = 273;
   continue_stmt                = 274;
   return_stmt                  = 275;
   raise_stmt                   = 276;
   import_stmt                  = 277;
   import_as_name               = 278;
   dotted_as_name               = 279;
   dotted_name                  = 280;
   global_stmt                  = 281;
   exec_stmt                    = 282;
   assert_stmt                  = 283;
   compound_stmt                = 284;
   if_stmt                      = 285;
   while_stmt                   = 286;
   for_stmt                     = 287;
   try_stmt                     = 288;
   except_clause                = 289;
   suite                        = 290;
   test                         = 291;
   and_test                     = 291;
   not_test                     = 293;
   comparison                   = 294;
   comp_op                      = 295;
   expr                         = 296;
   xor_expr                     = 297;
   and_expr                     = 298;
   shift_expr                   = 299;
   arith_expr                   = 300;
   term                         = 301;
   factor                       = 302;
   power                        = 303;
   atom                         = 304;
   listmaker                    = 305;
   lambdef                      = 306;
   trailer                      = 307;
   subscriptlist                = 308;
   subscript                    = 309;
   sliceop                      = 310;
   exprlist                     = 311;
   testlist                     = 312;
   dictmaker                    = 313;
   classdef                     = 314;
   arglist                      = 315;
   argument                     = 316;
   list_iter                    = 317;
   list_for                     = 318;
   list_if                      = 319;
{$ENDIF}
{$IFDEF PYTHON15}
   single_input                 = 256;
   file_input                   = 257;
   eval_input                   = 258;
   funcdef                      = 259;
   parameters                   = 260;
   varargslist                  = 261;
   fpdef                        = 262;
   fplist                       = 263;
   stmt                         = 264;
   simple_stmt                  = 265;
   small_stmt                   = 266;
   expr_stmt                    = 267;
   print_stmt                   = 268;
   del_stmt                     = 269;
   pass_stmt                    = 270;
   flow_stmt                    = 271;
   break_stmt                   = 272;
   continue_stmt                = 273;
   return_stmt                  = 274;
   raise_stmt                   = 275;
   import_stmt                  = 276;
   dotted_name                  = 277;
   global_stmt                  = 278;
   exec_stmt                    = 279;
   compound_stmt                = 280;
   if_stmt                      = 281;
   while_stmt                   = 282;
   for_stmt                     = 283;
   try_stmt                     = 284;
   except_clause                = 285;
   suite                        = 286;
   test                         = 287;
   and_test                     = 288;
   not_test                     = 289;
   comparison                   = 290;
   comp_op                      = 291;
   expr                         = 292;
   xor_expr                     = 293;
   and_expr                     = 294;
   shift_expr                   = 295;
   arith_expr                   = 296;
   term                         = 297;
   factor                       = 298;
   power                        = 299;
   atom                         = 300;
   lambdef                      = 301;
   trailer                      = 302;
   subscriptlist                = 303;
   subscript                    = 304;
   sliceop                      = 305;
   exprlist                     = 306;
   testlist                     = 307;
   dictmaker                    = 308;
   classdef                     = 309;
   arglist                      = 310;
   argument                     = 311;
{$ENDIF}

{$IFDEF PYTHON22_OR_HIGHER}
  // structmember.h
const
//* Types */
  T_SHORT                       = 0;
  T_INT                         = 1;
  T_LONG                        = 2;
  T_FLOAT                       = 3;
  T_DOUBLE                      = 4;
  T_STRING                      = 5;
  T_OBJECT                      = 6;
//* XXX the ordering here is weird for binary compatibility */
  T_CHAR                        = 7;	//* 1-character string */
  T_BYTE                        = 8;	//* 8-bit signed int */
//* unsigned variants: */
  T_UBYTE                       = 9;
  T_USHORT                      = 10;
  T_UINT                        = 11;
  T_ULONG                       = 12;

//* Added by Jack: strings contained in the structure */
  T_STRING_INPLACE= 13;

  T_OBJECT_EX                   = 16;{* Like T_OBJECT, but raises AttributeError
                                        when the value is NULL, instead of
                                        converting to None. *}

//* Flags */
  READONLY                      = 1;
  RO                            = READONLY;		//* Shorthand */
  READ_RESTRICTED               = 2;
  WRITE_RESTRICTED              = 4;
  RESTRICTED                    = (READ_RESTRICTED or WRITE_RESTRICTED);
type
  TPyMemberType = (mtShort, mtInt, mtLong, mtFloat, mtDouble, mtString, mtObject,
                   mtChar, mtByte, mtUByte, mtUShort, mtUInt, mtULong,
                   mtStringInplace, mtObjectEx);
  TPyMemberFlag = (mfDefault, mfReadOnly, mfReadRestricted, mfWriteRestricted, mfRestricted);
{$ENDIF}

//#######################################################
//##                                                   ##
//##           Non-Python specific constants           ##
//##                                                   ##
//#######################################################

const
  ErrInit         = -300;
  CR              = #13;
  LF              = #10;
  TAB             = #09;
  CRLF            = CR+LF;



//#######################################################
//##                                                   ##
//##    Global declarations, nothing Python specific   ##
//##                                                   ##
//#######################################################

type
   TPChar     = array[0..16000] of PChar;
   TPWideChar = array[0..16000] of PWideChar;
   PPChar     = ^TPChar;
   PPWideChar = ^TPWideChar;
   PInt	      = ^Integer;
   PDouble    = ^Double;
   PFloat     = ^Real;
   PLong      = ^LongInt;
   PShort     = ^ShortInt;
   PString    = ^PChar;


//#######################################################
//##                                                   ##
//##            Python specific interface              ##
//##                                                   ##
//#######################################################

type
  PP_frozen	    = ^P_frozen;
  P_frozen	    = ^_frozen;
  PPyObject	    = ^PyObject;
  PPPyObject	    = ^PPyObject;
  PPPPyObject	    = ^PPPyObject;
  PPyIntObject	    = ^PyIntObject;
  PPyTypeObject     = ^PyTypeObject;
  PPySliceObject    = ^PySliceObject;

  AtExitProc        = procedure;

  PyCFunction       = function( self, args:PPyObject): PPyObject; cdecl;
  PyCFunctionWithKW = function( self, args, keywords:PPyObject): PPyObject; cdecl;

  unaryfunc         = function( ob1 : PPyObject): PPyObject; cdecl;
  binaryfunc        = function( ob1,ob2 : PPyObject): PPyObject; cdecl;
  ternaryfunc       = function( ob1,ob2,ob3 : PPyObject): PPyObject; cdecl;
  inquiry           = function( ob1 : PPyObject): integer; cdecl;
  coercion          = function( ob1,ob2 : PPPyObject): integer; cdecl;
  intargfunc        = function( ob1 : PPyObject; i: integer): PPyObject; cdecl;
  intintargfunc     = function( ob1 : PPyObject; i1, i2: integer):
                                PPyObject; cdecl;
  intobjargproc     = function( ob1 : PPyObject; i: integer; ob2 : PPyObject):
                                integer; cdecl;
  intintobjargproc  = function( ob1: PPyObject; i1, i2: integer;
                                ob2: PPyObject): integer; cdecl;
  objobjargproc     = function( ob1,ob2,ob3 : PPyObject): integer; cdecl;

  pydestructor      = procedure(ob: PPyObject); cdecl;
  printfunc         = function( ob: PPyObject; var f: file; i: integer): integer; cdecl;
  getattrfunc       = function( ob1: PPyObject; name: PChar): PPyObject; cdecl;
  setattrfunc       = function( ob1: PPyObject; name: PChar; ob2: PPyObject): integer; cdecl;
  cmpfunc           = function( ob1,ob2: PPyObject): integer; cdecl;
  reprfunc          = function( ob: PPyObject): PPyObject; cdecl;
  hashfunc          = function( ob: PPyObject): LongInt; cdecl;
  getattrofunc      = function( ob1,ob2: PPyObject): PPyObject; cdecl;
  setattrofunc      = function( ob1,ob2,ob3: PPyObject): integer; cdecl;

{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   added from object.h
  getreadbufferproc = function ( ob1: PPyObject; i: integer; ptr: Pointer): integer; cdecl;
  getwritebufferproc= function ( ob1: PPyObject; i: integer; ptr: Pointer): integer; cdecl;
  getsegcountproc   = function ( ob1: PPyObject; i: integer): integer; cdecl;
  getcharbufferproc = function ( ob1: PPyObject; i: integer; const pstr: PChar): integer; cdecl;
  objobjproc        = function ( ob1, ob2: PPyObject): integer; cdecl;
  visitproc         = function ( ob1: PPyObject; ptr: Pointer): integer; cdecl;
  traverseproc      = function ( ob1: PPyObject; proc: visitproc; ptr: Pointer): integer; cdecl;
{$ENDIF}

{$IFDEF PYTHON21_OR_HIGHER}
  richcmpfunc       = function ( ob1, ob2 : PPyObject; i : Integer) : PPyObject; cdecl;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  getiterfunc       = function ( ob1 : PPyObject) : PPyObject; cdecl;
  iternextfunc      = function ( ob1 : PPyObject) : PPyObject; cdecl;
  descrgetfunc      = function ( ob1, ob2, ob3 : PPyObject) : PPyObject; cdecl;
  descrsetfunc      = function ( ob1, ob2, ob3 : PPyObject) : Integer; cdecl;
  initproc          = function ( self, args, kwds : PPyObject) : Integer; cdecl;
  newfunc           = function ( subtype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
  allocfunc         = function ( self: PPyTypeObject; nitems : integer) : PPyObject; cdecl;
{$ENDIF}

  PyNumberMethods = packed record
     nb_add           : binaryfunc;
     nb_substract     : binaryfunc;
     nb_multiply      : binaryfunc;
     nb_divide        : binaryfunc;
     nb_remainder     : binaryfunc;
     nb_divmod        : binaryfunc;
     nb_power         : ternaryfunc;
     nb_negative      : unaryfunc;
     nb_positive      : unaryfunc;
     nb_absolute      : unaryfunc;
     nb_nonzero       : inquiry;
     nb_invert        : unaryfunc;
     nb_lshift        : binaryfunc;
     nb_rshift        : binaryfunc;
     nb_and           : binaryfunc;
     nb_xor           : binaryfunc;
     nb_or            : binaryfunc;
     nb_coerce        : coercion;
     nb_int           : unaryfunc;
     nb_long          : unaryfunc;
     nb_float         : unaryfunc;
     nb_oct           : unaryfunc;
     nb_hex           : unaryfunc;
{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     nb_inplace_add       : binaryfunc;
     nb_inplace_subtract  : binaryfunc;
     nb_inplace_multiply  : binaryfunc;
     nb_inplace_divide    : binaryfunc;
     nb_inplace_remainder : binaryfunc;
     nb_inplace_power     : ternaryfunc;
     nb_inplace_lshift    : binaryfunc;
     nb_inplace_rshift    : binaryfunc;
     nb_inplace_and       : binaryfunc;
     nb_inplace_xor       : binaryfunc;
     nb_inplace_or        : binaryfunc;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
     // Added in release 2.2
     // The following require the Py_TPFLAGS_HAVE_CLASS flag
     nb_floor_divide         : binaryfunc;
     nb_true_divide          : binaryfunc;
     nb_inplace_floor_divide : binaryfunc;
     nb_inplace_true_divide  : binaryfunc;
{$ENDIF}
  end;
  PPyNumberMethods = ^PyNumberMethods;

  PySequenceMethods = packed record
     sq_length    : inquiry;
     sq_concat    : binaryfunc;
     sq_repeat    : intargfunc;
     sq_item      : intargfunc;
     sq_slice     : intintargfunc;
     sq_ass_item  : intobjargproc;
     sq_ass_slice : intintobjargproc;
{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     sq_contains        : objobjproc;
     sq_inplace_concat  : binaryfunc;
     sq_inplace_repeat  : intargfunc;
{$ENDIF}
  end;
  PPySequenceMethods = ^PySequenceMethods;

  PyMappingMethods = packed record
     mp_length	      : inquiry;
     mp_subscript     : binaryfunc;
     mp_ass_subscript : objobjargproc;
  end;
  PPyMappingMethods = ^PyMappingMethods;

{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
  PyBufferProcs = packed record
     bf_getreadbuffer   : getreadbufferproc;
     bf_getwritebuffer  : getwritebufferproc;
     bf_getsegcount     : getsegcountproc;
     bf_getcharbuffer   : getcharbufferproc;
  end;
  PPyBufferProcs = ^PyBufferProcs;
{$ENDIF}

  Py_complex =  packed record
     real : double;
     imag : double;
  end;

  PyObject = packed record
    ob_refcnt: Integer;
    ob_type:   PPyTypeObject;
  end;

  PyIntObject = packed record
    ob_refcnt : Integer;
    ob_type   : PPyTypeObject;
    ob_ival   : LongInt;
  end;

  _frozen = packed record
     name	: PChar;
     code	: PByte;
     size	: Integer;
  end;

  PySliceObject = packed record
    ob_refcnt:          Integer;
    ob_type:            PPyTypeObject;
    start, stop, step:  PPyObject;
  end;

  PPyMethodDef = ^PyMethodDef;
  PyMethodDef  = packed record
     ml_name:  PChar;
     ml_meth:  PyCFunction;
     ml_flags: Integer;
     ml_doc:   PChar;
  end;

{$IFDEF PYTHON22_OR_HIGHER}
  // structmember.h
  PPyMemberDef = ^PyMemberDef;
  PyMemberDef = packed record
    name : PChar;
    _type : integer;
    offset : integer;
    flags : integer;
    doc : PChar;
  end;

  // descrobject.h

  // Descriptors

  getter = function ( obj : PPyObject; context : Pointer) : PPyObject; cdecl;
  setter = function ( obj, value : PPyObject; context : Pointer) : integer; cdecl;

  PPyGetSetDef = ^PyGetSetDef;
  PyGetSetDef = packed record
    name : PChar;
    get : getter;
    _set : setter;
    doc : PChar;
    closure : Pointer;
  end;

  wrapperfunc = function (self, args: PPyObject; wrapped : Pointer) : PPyObject; cdecl;

  pwrapperbase = ^wrapperbase;
  wrapperbase = packed record
    name : PChar;
    wrapper : wrapperfunc;
    doc : PChar;
  end;

  // Various kinds of descriptor objects

  {#define PyDescr_COMMON \
          PyObject_HEAD \
          PyTypeObject *d_type; \
          PyObject *d_name
  }

  PPyDescrObject = ^PyDescrObject;
  PyDescrObject = packed record
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
  end;

  PPyMethodDescrObject = ^PyMethodDescrObject;
  PyMethodDescrObject = packed record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_method : PPyMethodDef;
  end;

  PPyMemberDescrObject = ^PyMemberDescrObject;
  PyMemberDescrObject = packed record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_member : PPyMemberDef;
  end;

  PPyGetSetDescrObject = ^PyGetSetDescrObject;
  PyGetSetDescrObject = packed record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_getset : PPyGetSetDef;
  end;

  PPyWrapperDescrObject = ^PyWrapperDescrObject;
  PyWrapperDescrObject = packed record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_base : pwrapperbase;
    d_wrapped : Pointer; // This can be any function pointer
  end;

  PPyModuleDef_Base = ^PyModuleDef_Base;
  PyModuleDef_Base = packed record
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    m_init     : function( ) : PPyObject; cdecl;
    m_index     : Integer;
    m_copy : PPyObject;
  end;

  PPyModuleDef = ^PyModuleDef;
  PyModuleDef = packed record
    m_base : PyModuleDef_Base;
    m_name : PChar;
    m_doc : PChar;
    m_size : Integer;
    m_methods : PPyMethodDef;
    m_reload : inquiry;
    m_traverse : traverseproc;
    m_clear : inquiry;
    m_free : inquiry;
  end;

{$ENDIF}

  // object.h
  PyTypeObject = packed record
    ob_refcnt:      Integer;
    ob_type:        PPyTypeObject;
    ob_size:        Integer; // Number of items in variable part
    tp_name:        PChar;   // For printing
    tp_basicsize, tp_itemsize: Integer; // For allocation

    // Methods to implement standard operations

    tp_dealloc:     pydestructor;
    tp_print:       printfunc;
    tp_getattr:     getattrfunc;
    tp_setattr:     setattrfunc;
    tp_compare:     cmpfunc;
    tp_repr:        reprfunc;

    // Method suites for standard classes

    tp_as_number:   PPyNumberMethods;
    tp_as_sequence: PPySequenceMethods;
    tp_as_mapping:  PPyMappingMethods;

    // More standard operations (here for binary compatibility)

    tp_hash:        hashfunc;
    tp_call:        ternaryfunc;
    tp_str:         reprfunc;
    tp_getattro:    getattrofunc;
    tp_setattro:    setattrofunc;

{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0

    // Functions to access object as input/output buffer
    tp_as_buffer:   PPyBufferProcs;
    // Flags to define presence of optional/expanded features
    tp_flags:       LongInt;

    tp_doc:         PChar; // Documentation string

    // call function for all accessible objects
    tp_traverse:    traverseproc;

    // delete references to contained objects
    tp_clear:       inquiry;
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
    // rich comparisons
    tp_richcompare: richcmpfunc;

    // weak reference enabler
    tp_weaklistoffset: Longint;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    // Iterators
    tp_iter : getiterfunc;
    tp_iternext : iternextfunc;

    // Attribute descriptor and subclassing stuff
    tp_methods          : PPyMethodDef;
    tp_members          : PPyMemberDef;
    tp_getset           : PPyGetSetDef;
    tp_base             : PPyTypeObject;
    tp_dict             : PPyObject;
    tp_descr_get        : descrgetfunc;
    tp_descr_set        : descrsetfunc;
    tp_dictoffset       : longint;
    tp_init             : initproc;
    tp_alloc            : allocfunc;
    tp_new              : newfunc;
    tp_free             : pydestructor; // Low-level free-memory routine
    tp_is_gc            : inquiry; // For PyObject_IS_GC
    tp_bases            : PPyObject;
    tp_mro              : PPyObject; // method resolution order
    tp_cache            : PPyObject;
    tp_subclasses       : PPyObject;
    tp_weaklist         : PPyObject;
{$ENDIF}
//{$IFDEF PYTHON20}
{$IFDEF PYTHON22_OR_HIGHER}
    //More spares
    tp_xxx7:        LongInt;
    tp_xxx8:        LongInt;
{$ENDIF}
{$IFDEF PYTHON15}
    //More spares
    tp_xxx7:        LongInt;
    tp_xxx8:        LongInt;
{$ENDIF}
  end;

  PPyMethodChain = ^PyMethodChain;
  PyMethodChain = packed record
    methods: PPyMethodDef;
    link:    PPyMethodChain;
  end;

  PPyClassObject = ^PyClassObject;
  PyClassObject = packed record
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    cl_bases   : PPyObject;       // A tuple of class objects
    cl_dict    : PPyObject;       // A dictionary
    cl_name    : PPyObject;       // A string
    // The following three are functions or NULL
    cl_getattr : PPyObject;
    cl_setattr : PPyObject;
    cl_delattr : PPyObject;
  end;

  PPyInstanceObject = ^PyInstanceObject;
  PyInstanceObject = packed record
    // Start of the Head of an object
    ob_refcnt : Integer;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    in_class  : PPyClassObject;      // The class object
    in_dict   : PPyObject;           // A dictionary
  end;

{ Instance method objects are used for two purposes:
   (a) as bound instance methods (returned by instancename.methodname)
   (b) as unbound methods (returned by ClassName.methodname)
   In case (b), im_self is NULL
}

  PPyMethodObject = ^PyMethodObject;
  PyMethodObject = packed record
    // Start of the Head of an object
    ob_refcnt : Integer;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    im_func  : PPyObject;      // The function implementing the method
    im_self  : PPyObject;      // The instance it is bound to, or NULL
    im_class : PPyObject;      // The class that defined the method
  end;


  // Bytecode object, compile.h
  PPyCodeObject = ^PyCodeObject;
  PyCodeObject = packed record
    ob_refcnt      : Integer;
    ob_type        : PPyTypeObject;
    co_argcount    : Integer;         // #arguments, except *args
    co_nlocals     : Integer;         // #local variables
    co_stacksize   : Integer;          // #entries needed for evaluation stack
    co_flags       : Integer;         // CO_..., see below
    co_code        : PPyObject;       // instruction opcodes (it hides a PyStringObject)
    co_consts      : PPyObject;       // list (constants used)
    co_names       : PPyObject;       // list of strings (names used)
    co_varnames    : PPyObject;       // tuple of strings (local variable names)
{$IFDEF PYTHON21_OR_HIGHER}
    co_freevars    : PPyObject;	      // tuple of strings (free variable names)
    co_cellvars    : PPyObject;       // tuple of strings (cell variable names)
{$ENDIF}
    // The rest doesn't count for hash/cmp
    co_filename    : PPyObject;       // string (where it was loaded from)
    co_name        : PPyObject;       // string (name, for reference)
    co_firstlineno : Integer;         // first source line number
    co_lnotab      : PPyObject;       // string (encoding addr<->lineno mapping)
  end;


  // from pystate.h
  PPyInterpreterState = ^PyInterpreterState;
  PPyThreadState = ^PyThreadState;
  PPyFrameObject = ^PyFrameObject;

  // Interpreter environments
  PyInterpreterState = packed record
    next           : PPyInterpreterState;
    tstate_head    : PPyThreadState;

    modules        : PPyObject;
    sysdict        : PPyObject;
    builtins       : PPyObject;

    checkinterval  : integer;
  end;

  // Thread specific information
  PyThreadState = packed record
    next           : PPyThreadState;
    interp         : PPyInterpreterState;

    frame          : PPyFrameObject;
    recursion_depth: integer;
    ticker         : integer;
    tracing        : integer;

{$IFDEF PYTHON23_OR_HIGHER}
    sys_profilefn  : Pointer;           // c-functions for profile/trace
    sys_tracefn    : Pointer;
{$ENDIF}
    sys_profilefunc: PPyObject;
    sys_tracefunc  : PPyObject;

    curexc_type    : PPyObject;
    curexc_value   : PPyObject;
    curexc_traceback: PPyObject;

    exc_type       : PPyObject;
    exc_value      : PPyObject;
    exc_traceback  : PPyObject;

    dict           : PPyObject;
{$IFDEF PYTHON23_OR_HIGHER}
    tick_counter      :Integer;
    gilstate_counter  :Integer;

    async_exc         :PPyObject; { Asynchronous exception to raise }
    thread_id         :LongInt;   { Thread id where this tstate was created }

    { XXX signal handlers should also be here }
{$ENDIF}
  end;

  // from frameobject.h

  PPyTryBlock = ^PyTryBlock;
  PyTryBlock = packed record
    b_type    : Integer;       // what kind of block this is
    b_handler : Integer;       // where to jump to find handler
    b_level   : Integer;       // value stack level to pop to
  end;

  CO_MAXBLOCKS  = 0..19;
  PyFrameObject = packed record
    // Start of the VAR_HEAD of an object.
    ob_refcnt    : Integer;
    ob_type      : PPyTypeObject;
{$IFDEF PYTHON22_OR_HIGHER}
    ob_size      : Integer;           // Number of items in variable part
{$ENDIF}
    // End of the Head of an object
    f_back       : PPyFrameObject;    // previous frame, or NULL
    f_code       : PPyCodeObject;     // code segment
    f_builtins   : PPyObject;         // builtin symbol table (PyDictObject)
    f_globals    : PPyObject;         // global symbol table (PyDictObject)
    f_locals     : PPyObject;         // local symbol table (PyDictObject)
    f_valuestack : PPPyObject;        // points after the last local
{$IFDEF PYTHON22_OR_HIGHER}
    (* Next free slot in f_valuestack.  Frame creation sets to f_valuestack.
       Frame evaluation usually NULLs it, but a frame that yields sets it
       to the current stack top. *)
    f_stacktop   : PPPyObject;
{$ENDIF}
    f_trace      : PPyObject;         // Trace function
    f_exc_type, f_exc_value, f_exc_traceback: PPyObject;
    f_tstate     : PPyThreadState;
    f_lasti      : Integer;           // Last instruction if called
    f_lineno     : Integer;           // Current line number
    f_restricted : Integer;           // Flag set if restricted operations
                                      // in this scope
    f_iblock     : Integer;           // index in f_blockstack
    f_blockstack : array[CO_MAXBLOCKS] of PyTryBlock; // for try and loop blocks
    f_nlocals    : Integer;           // number of locals
{$IFDEF PYTHON22_OR_HIGHER}
    f_ncells     : Integer;
    f_nfreevars  : Integer;
{$ENDIF}
    f_stacksize  : Integer;           // size of value stack
    f_localsplus : array[0..0] of PPyObject; // locals+stack, dynamically sized
  end;

  // From traceback.c
  PPyTraceBackObject = ^PyTraceBackObject;
  PyTraceBackObject = packed record
    // Start of the Head of an object
    ob_refcnt : Integer;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    tb_next   : PPyTraceBackObject;
    tb_frame  : PPyFrameObject;
    tb_lasti  : Integer;
    tb_lineno : Integer;
  end;

  // Parse tree node interface

  PNode = ^node;
  node = packed record
    n_type      : smallint;
    n_str       : PChar;
    n_lineno    : smallint;
    n_nchildren : smallint;
    n_child     : PNode;
  end;

  // From weakrefobject.h

  PPyWeakReference = ^PyWeakReference;
  PyWeakReference = packed record
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    wr_object   : PPyObject;
    wr_callback : PPyObject;
    hash        : longint;
    wr_prev     : PPyWeakReference;
    wr_next     : PPyWeakReference;
  end;

  // from datetime.h


{* Fields are packed into successive bytes, each viewed as unsigned and
 * big-endian, unless otherwise noted:
 *
 * byte offset
 *  0 		year     2 bytes, 1-9999
 *  2	  	month    1 byte,  1-12
 *  3 		day      1 byte,  1-31
 *  4     hour     1 byte,  0-23
 *  5 		minute   1 byte,  0-59
 *  6 		second   1 byte,  0-59
 *  7 		usecond  3 bytes, 0-999999
 * 10
 *}

const
  { # of bytes for year, month, and day. }
  _PyDateTime_DATE_DATASIZE = 4;

  { # of bytes for hour, minute, second, and usecond. }
  _PyDateTime_TIME_DATASIZE = 6;

  { # of bytes for year, month, day, hour, minute, second, and usecond. }
  _PyDateTime_DATETIME_DATASIZE = 10;
type
  PyDateTime_Delta = packed record
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode    : Integer;  // -1 when unknown
    days        : Integer;  // -MAX_DELTA_DAYS <= days <= MAX_DELTA_DAYS
    seconds     : Integer;  // 0 <= seconds < 24*3600 is invariant
    microseconds: Integer;  // 0 <= microseconds < 1000000 is invariant
  end;
  PPyDateTime_Delta = ^PyDateTime_Delta;

  PyDateTime_TZInfo = packed record // a pure abstract base clase
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
  end;
  PPyDateTime_TZInfo = ^PyDateTime_TZInfo;

{
/* The datetime and time types have hashcodes, and an optional tzinfo member,
 * present if and only if hastzinfo is true.
 */
#define _PyTZINFO_HEAD		\
	PyObject_HEAD		\
	long hashcode;		\
	char hastzinfo;		/* boolean flag */
}

{* No _PyDateTime_BaseTZInfo is allocated; it's just to have something
 * convenient to cast to, when getting at the hastzinfo member of objects
 * starting with _PyTZINFO_HEAD.
 *}
  _PyDateTime_BaseTZInfo = packed record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
  end;
  _PPyDateTime_BaseTZInfo = ^_PyDateTime_BaseTZInfo;

{* All time objects are of PyDateTime_TimeType, but that can be allocated
 * in two ways, with or without a tzinfo member.  Without is the same as
 * tzinfo == None, but consumes less memory.  _PyDateTime_BaseTime is an
 * internal struct used to allocate the right amount of space for the
 * "without" case.
 *}
{#define _PyDateTime_TIMEHEAD	\
	_PyTZINFO_HEAD		\
	unsigned char data[_PyDateTime_TIME_DATASIZE];
}

  _PyDateTime_BaseTime = packed record // hastzinfo false
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
  end;
  _PPyDateTime_BaseTime = ^_PyDateTime_BaseTime;

  PyDateTime_Time = packed record // hastzinfo true
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
    tzinfo     : PPyObject;
  end;
  PPyDateTime_Time = ^PyDateTime_Time;



{* All datetime objects are of PyDateTime_DateTimeType, but that can be
 * allocated in two ways too, just like for time objects above.  In addition,
 * the plain date type is a base class for datetime, so it must also have
 * a hastzinfo member (although it's unused there).
 *}
  PyDateTime_Date = packed record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATE_DATASIZE)] of Byte;
  end;
  PPyDateTime_Date = ^PyDateTime_Date;

 {
#define _PyDateTime_DATETIMEHEAD	\
	_PyTZINFO_HEAD			\
	unsigned char data[_PyDateTime_DATETIME_DATASIZE];
}

  _PyDateTime_BaseDateTime = packed record // hastzinfo false
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : Integer;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
  end;
  _PPyDateTime_BaseDateTime = ^_PyDateTime_BaseDateTime;

  PyDateTime_DateTime = packed record // hastzinfo true
    // Start of _PyDateTime_DATETIMEHEAD
      // Start of _PyTZINFO_HEAD
        // Start of the Head of an object
        ob_refcnt  : Integer;
        ob_type    : PPyTypeObject;
        // End of the Head of an object
      hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
      data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
    // End of _PyDateTime_DATETIMEHEAD
    tzinfo : PPyObject;
  end;
  PPyDateTime_DateTime = ^PyDateTime_DateTime;


//#######################################################
//##                                                   ##
//##         New exception classes                     ##
//##                                                   ##
//#######################################################

  // Components' exceptions
  EDLLLoadError  = class(Exception);
  EDLLImportError = class(Exception)
    public
      WrongFunc : String;
      ErrorCode : Integer;
  end;

  // Python's exceptions
  EPythonError   = class(Exception)
    public
      EName : String;
      EValue : String;
  end;
  EPyExecError   = class(EPythonError);


  // Standard exception classes of Python

{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   base classes updated according python documentation

{ Hierarchy of Python exceptions, Python 2.3, copied from <INSTALL>\Python\exceptions.c

Exception\n\
 |\n\
 +-- SystemExit\n\
 +-- StopIteration\n\
 +-- StandardError\n\
 |    |\n\
 |    +-- KeyboardInterrupt\n\
 |    +-- ImportError\n\
 |    +-- EnvironmentError\n\
 |    |    |\n\
 |    |    +-- IOError\n\
 |    |    +-- OSError\n\
 |    |         |\n\
 |    |         +-- WindowsError\n\
 |    |         +-- VMSError\n\
 |    |\n\
 |    +-- EOFError\n\
 |    +-- RuntimeError\n\
 |    |    |\n\
 |    |    +-- NotImplementedError\n\
 |    |\n\
 |    +-- NameError\n\
 |    |    |\n\
 |    |    +-- UnboundLocalError\n\
 |    |\n\
 |    +-- AttributeError\n\
 |    +-- SyntaxError\n\
 |    |    |\n\
 |    |    +-- IndentationError\n\
 |    |         |\n\
 |    |         +-- TabError\n\
 |    |\n\
 |    +-- TypeError\n\
 |    +-- AssertionError\n\
 |    +-- LookupError\n\
 |    |    |\n\
 |    |    +-- IndexError\n\
 |    |    +-- KeyError\n\
 |    |\n\
 |    +-- ArithmeticError\n\
 |    |    |\n\
 |    |    +-- OverflowError\n\
 |    |    +-- ZeroDivisionError\n\
 |    |    +-- FloatingPointError\n\
 |    |\n\
 |    +-- ValueError\n\
 |    |    |\n\
 |    |    +-- UnicodeError\n\
 |    |        |\n\
 |    |        +-- UnicodeEncodeError\n\
 |    |        +-- UnicodeDecodeError\n\
 |    |        +-- UnicodeTranslateError\n\
 |    |\n\
 |    +-- ReferenceError\n\
 |    +-- SystemError\n\
 |    +-- MemoryError\n\
 |\n\
 +---Warning\n\
      |\n\
      +-- UserWarning\n\
      +-- DeprecationWarning\n\
      +-- PendingDeprecationWarning\n\
      +-- SyntaxWarning\n\
      +-- RuntimeWarning\n\
      +-- FutureWarning"
}
   EPyException = class (EPythonError);
   EPyStandardError = class (EPyException);
   EPyArithmeticError = class (EPyStandardError);
   EPyLookupError = class (EPyStandardError);
   EPyAssertionError = class (EPyStandardError);
   EPyAttributeError = class (EPyStandardError);
   EPyEOFError = class (EPyStandardError);
   EPyFloatingPointError = class (EPyArithmeticError);
   EPyEnvironmentError = class (EPyStandardError);
   EPyIOError = class (EPyEnvironmentError);
   EPyOSError = class (EPyEnvironmentError);
   EPyImportError = class (EPyStandardError);
   EPyIndexError = class (EPyLookupError);
   EPyKeyError = class (EPyLookupError);
   EPyKeyboardInterrupt = class (EPyStandardError);
   EPyMemoryError = class (EPyStandardError);
   EPyNameError = class (EPyStandardError);
   EPyOverflowError = class (EPyArithmeticError);
   EPyRuntimeError = class (EPyStandardError);
   EPyNotImplementedError = class (EPyRuntimeError);
   EPySyntaxError = class (EPyStandardError)
   public
      EFileName: string;
      ELineStr: string;
      ELineNumber: Integer;
      EOffset: Integer;
   end;
   EPyIndentationError = class (EPySyntaxError);
   EPyTabError = class (EPyIndentationError);
   EPySystemError = class (EPyStandardError);
   EPySystemExit = class (EPyException);
   EPyTypeError = class (EPyStandardError);
   EPyUnboundLocalError = class (EPyNameError);
   EPyValueError = class (EPyStandardError);
   EPyUnicodeError = class (EPyValueError);
{$IFDEF PYTHON23_OR_HIGHER}
   UnicodeEncodeError = class (EPyUnicodeError);
   UnicodeDecodeError = class (EPyUnicodeError);
   UnicodeTranslateError = class (EPyUnicodeError);
{$ENDIF}
   EPyZeroDivisionError = class (EPyArithmeticError);
   EPyStopIteration = class(EPyException);
   EPyWarning = class (EPyException);
   EPyUserWarning = class (EPyWarning);
   EPyDeprecationWarning = class (EPyWarning);
{$IFDEF PYTHON23_OR_HIGHER}
   PendingDeprecationWarning = class (EPyWarning);
   FutureWarning = class (EPyWarning);
{$ENDIF}
   EPySyntaxWarning = class (EPyWarning);
   EPyRuntimeWarning = class (EPyWarning);
   EPyReferenceError = class (EPyStandardError);
 {$IFDEF MSWINDOWS}
   EPyWindowsError = class (EPyOSError);
 {$ENDIF}
{$ENDIF}
{$IFDEF PYTHON15}
   EPyException = class (EPythonError);
   EPyStandardError = class (EPyException);

   EPyAttributeError = class (EPythonError);
   EPyEOFError = class (EPyException);
   EPyIOError = class (EPyException);
   EPyImportError = class (EPyException);
   EPyKeyboardInterrupt = class (EPyException);
   EPyMemoryError = class (EPyException);
   EPyNameError = class (EPyException);
   EPyRuntimeError = class (EPyException);
   EPySystemError = class (EPyException);
   EPySystemExit = class (EPyException);
   EPyTypeError = class (EPyException);
   EPyValueError = class (EPyException);
   EPySyntaxError = class (EPyException)
   public
      EFileName: string;
      ELineStr: string;
      ELineNumber: Integer;
      EOffset: Integer;
   end;

   EPyArithmeticError = class (EPyStandardError);
   EPyOverflowError = class (EPyArithmeticError);
   EPyZeroDivisionError = class (EPyArithmeticError);
   EPyFloatingPointError = class (EPyArithmeticError);

   EPyLookupError = class (EPyStandardError);
   EPyIndexError = class (EPyLookupError);
   EPyKeyError = class (EPyLookupError);
{$ENDIF}


{$IFNDEF HAS_SYNCOBJS_UNIT}
  {$IFDEF MSWINDOWS}
    TCriticalSection = class
    protected
      FSection: TRTLCriticalSection;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Acquire;
      procedure Release;
      procedure Enter;
      procedure Leave;
    end;
  {$ENDIF}
{$ENDIF}

//#######################################################
//##                                                   ##
//##                   Components                      ##
//##                                                   ##
//#######################################################

//-------------------------------------------------------
//--                                                   --
//--      class:  TPythonInputOutput                   --
//--      Works as a console for Python outputs        --
//--      It's a virtual Base class                    --
//-------------------------------------------------------

const
  kMaxLines = 1000;
  kMaxLineLength = 256;

type
  TSendDataEvent = procedure (Sender: TObject; const Data : String ) of object;
  TReceiveDataEvent = procedure (Sender: TObject; var Data : String ) of object;
{$IFDEF UNICODE_SUPPORT}
  TSendUniDataEvent = procedure (Sender: TObject; const Data : WideString ) of object;
  TReceiveUniDataEvent = procedure (Sender: TObject; var Data : WideString ) of object;
  IOChar = WideChar;
  IOString = WideString;
  TIOStringList = TWideStringList;
{$ELSE}
  IOChar = Char;
  IOString = String;
  TIOStringList = TStringList;
{$ENDIF}

  TPythonInputOutput = class(TComponent)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
    FMaxLines        : Integer;
    FLine_Buffer     : IOString;
    FLinesPerThread  : TIOStringList;
    FLock            : TCriticalSection;
    FQueue           : TIOStringList;
    FDelayWrites     : Boolean;
    FMaxLineLength   : Integer;
    FOnSendData      : TSendDataEvent;
    FOnReceiveData   : TReceiveDataEvent;
{$IFDEF UNICODE_SUPPORT}
    FOnSendUniData   : TSendUniDataEvent;
    FOnReceiveUniData: TReceiveUniDataEvent;
{$ENDIF}
    FUnicodeIO       : Boolean;
    FRawOutput       : Boolean;

    procedure Lock;
    procedure Unlock;
    procedure AddWrite( const str : IOString );
    // Virtual methods for handling the input/output of text
    procedure SendData( const Data : String ); virtual;
    function  ReceiveData : String; virtual;
{$IFDEF UNICODE_SUPPORT}
    procedure SendUniData( const Data : WideString ); virtual;
    function  ReceiveUniData : WideString; virtual;
{$ENDIF}
    procedure AddPendingWrite; virtual;
    function  GetCurrentThreadSlotIdx : Integer;
    function  GetCurrentThreadLine : IOString;
    procedure UpdateCurrentThreadLine;

  public
    { Déclarations publiques }
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Write( const str : IOString );
    procedure WriteLine( const str : IOString );

  published
    { Déclarations publiées }
    property MaxLines : Integer read FMaxLines write FMaxLines default kMaxLines;
    property MaxLineLength : Integer read FMaxLineLength write FMaxLineLength default kMaxLineLength;
    property DelayWrites : Boolean read FDelayWrites write FDelayWrites default False;
    property OnSendData    : TSendDataEvent read FOnSendData write FOnSendData;
    property OnReceiveData : TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
{$IFDEF UNICODE_SUPPORT}
    property OnSendUniData    : TSendUniDataEvent read FOnSendUniData write FOnSendUniData;
    property OnReceiveUniData : TReceiveUniDataEvent read FOnReceiveUniData write FOnReceiveUniData;
    property UnicodeIO: Boolean read FUnicodeIO write FUnicodeIO;
    property RawOutput: Boolean read FRawOutput write FRawOutput;
{$ENDIF}
  end;

//-------------------------------------------------------
//--                                                   --
//--      Base class:  TDynamicDll                     --
//--                                                   --
//-------------------------------------------------------

type
  TDynamicDll = class(TComponent)
  private
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
    procedure SetDllName(const Value: String);
  protected
    FDllName            : String;
    FDllPath            : String;
    FAPIVersion         : Integer;
    FRegVersion         : String;
    FAutoLoad           : Boolean;
    FAutoUnload         : Boolean;
    FFatalMsgDlg        : Boolean;
    FFatalAbort         : Boolean;
    FDLLHandle          : THandle;
    FUseLastKnownVersion: Boolean;
    FOnBeforeLoad       : TNotifyEvent;
    FOnAfterLoad        : TNotifyEvent;
    FOnBeforeUnload     : TNotifyEvent;

    function  Import(const funcname: String; canFail : Boolean = True): Pointer;
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function  GetQuitMessage : String; virtual;
    procedure DoOpenDll(const aDllName : String); virtual;
    function  GetDllPath : String;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // Public methods
    procedure OpenDll(const aDllName : String);
    function  IsHandleValid : Boolean;
    procedure LoadDll;
    procedure UnloadDll;
    procedure Quit;

    // Public properties
  published
    property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload : Boolean read FAutoUnload write FAutoUnload default True;
    property DllName : String read FDllName write SetDllName stored IsDllNameStored;
    property DllPath : String read FDllPath write FDllPath;
    property APIVersion : Integer read FAPIVersion write FAPIVersion stored IsAPIVersionStored;
    property RegVersion : String read FRegVersion write FRegVersion stored IsRegVersionStored;
    property FatalAbort :  Boolean read FFatalAbort write FFatalAbort default True;
    property FatalMsgDlg : Boolean read FFatalMsgDlg write FFatalMsgDlg default True;
    property UseLastKnownVersion: Boolean read FUseLastKnownVersion write FUseLastKnownVersion default True;
    property OnAfterLoad : TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad : TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnBeforeUnload : TNotifyEvent read FOnBeforeUnload write FOnBeforeUnload;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class:  TPythonInterface derived from TDynamicDll--
//--      This class maps the functions imported       --
//--      from the Python Dll, and adds some           --
//--      Delphi implementations.                      --
//-------------------------------------------------------

type
  TPythonInterface=class(TDynamicDll)
  private
    DLL_PyArg_Parse: function( args: PPyObject; format: PChar {;....}) :
                     Integer; cdecl;
    DLL_PyArg_ParseTuple:
                     function( args: PPyObject; format: PChar {;...}):
                     Integer; cdecl;
    DLL_Py_BuildValue:
                     function( format: PChar {;...}): PPyObject; cdecl;
    DLL_PyCode_Addr2Line:
                     function ( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
    DLL_Py_GetBuildInfo:
                     function : PChar; cdecl;
    DLL_PyImport_ExecCodeModule:
                     function ( const name : String; codeobject : PPyObject) : PPyObject; cdecl;

    DLL_PyString_FromString:  function( str: PChar): PPyObject; cdecl;
    DLL_Py_FlushLine:procedure; cdecl;

  protected
    FInitialized:    Boolean;
    FFinalizing:     Boolean;
    FIsPython3000:   Boolean;
    FBuiltInModuleName: String;
    function GetInitialized: Boolean;

    procedure AfterLoad; override;
    function  GetQuitMessage : String; override;
    procedure CheckPython;
    function  GetUnicodeTypeSuffix : String;

  public
    // define Python flags. See file pyDebug.h
    Py_DebugFlag: PInt;
    Py_VerboseFlag: PInt;
    Py_InteractiveFlag: PInt;
    Py_OptimizeFlag: PInt;
    Py_NoSiteFlag: PInt;
    Py_UseClassExceptionsFlag: PInt;
    Py_FrozenFlag: PInt;
    Py_TabcheckFlag: PInt;
{$IFDEF PYTHON20_OR_HIGHER}
    Py_UnicodeFlag: PInt;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    Py_IgnoreEnvironmentFlag: PInt;
    Py_DivisionWarningFlag: PInt;
{$ENDIF}
    //_PySys_TraceFunc:    PPPyObject;
    //_PySys_ProfileFunc: PPPPyObject;

    PyImport_FrozenModules: PP_frozen;

    Py_None:            PPyObject;
    Py_Ellipsis:        PPyObject;
    Py_False:           PPyIntObject;
    Py_True:            PPyIntObject;
{$IFDEF PYTHON21_OR_HIGHER}
    Py_NotImplemented:  PPyObject;
{$ENDIF}

    PyExc_AttributeError: PPPyObject;
    PyExc_EOFError: PPPyObject;
    PyExc_IOError: PPPyObject;
    PyExc_ImportError: PPPyObject;
    PyExc_IndexError: PPPyObject;
    PyExc_KeyError: PPPyObject;
    PyExc_KeyboardInterrupt: PPPyObject;
    PyExc_MemoryError: PPPyObject;
    PyExc_NameError: PPPyObject;
    PyExc_OverflowError: PPPyObject;
    PyExc_RuntimeError: PPPyObject;
    PyExc_SyntaxError: PPPyObject;
    PyExc_SystemError: PPPyObject;
    PyExc_SystemExit: PPPyObject;
    PyExc_TypeError: PPPyObject;
    PyExc_ValueError: PPPyObject;
    PyExc_ZeroDivisionError: PPPyObject;
    PyExc_ArithmeticError: PPPyObject;
    PyExc_Exception: PPPyObject;
    PyExc_FloatingPointError: PPPyObject;
    PyExc_LookupError: PPPyObject;
    PyExc_StandardError: PPPyObject;
{$IFDEF PYTHON20_OR_HIGHER}
    PyExc_AssertionError: PPPyObject;
    PyExc_EnvironmentError: PPPyObject;
    PyExc_IndentationError: PPPyObject;
    PyExc_MemoryErrorInst: PPPyObject;
    PyExc_NotImplementedError: PPPyObject;
    PyExc_OSError: PPPyObject;
    PyExc_TabError: PPPyObject;
    PyExc_UnboundLocalError: PPPyObject;
    PyExc_UnicodeError: PPPyObject;
 {$IFDEF MSWINDOWS}
    PyExc_WindowsError: PPPyObject;
 {$ENDIF}
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
    PyExc_Warning: PPPyObject;
    PyExc_DeprecationWarning: PPPyObject;
    PyExc_RuntimeWarning: PPPyObject;
    PyExc_SyntaxWarning: PPPyObject;
    PyExc_UserWarning: PPPyObject;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    PyExc_ReferenceError: PPPyObject;
    PyExc_StopIteration: PPPyObject;
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
    PyExc_FutureWarning: PPPyObject;
    PyExc_PendingDeprecationWarning: PPPyObject;
    PyExc_UnicodeDecodeError: PPPyObject;
    PyExc_UnicodeEncodeError: PPPyObject;
    PyExc_UnicodeTranslateError: PPPyObject;
{$ENDIF}

    PyType_Type: PPyTypeObject;
    PyCFunction_Type: PPyTypeObject;
    PyCObject_Type: PPyTypeObject;
    PyClass_Type: PPyTypeObject;
    PyCode_Type: PPyTypeObject;
    PyComplex_Type: PPyTypeObject;
    PyDict_Type: PPyTypeObject;
    PyFile_Type: PPyTypeObject;
    PyFloat_Type: PPyTypeObject;
    PyFrame_Type: PPyTypeObject;
    PyFunction_Type: PPyTypeObject;
    PyInstance_Type: PPyTypeObject;
    PyInt_Type: PPyTypeObject;
    PyList_Type: PPyTypeObject;
    PyLong_Type: PPyTypeObject;
    PyMethod_Type: PPyTypeObject;
    PyModule_Type: PPyTypeObject;
    PyObject_Type: PPyTypeObject;
    PyRange_Type: PPyTypeObject;
    PySlice_Type: PPyTypeObject;
    PyString_Type: PPyTypeObject;
    PyTuple_Type: PPyTypeObject;
{$IFDEF PYTHON20_OR_HIGHER}
    PyBaseObject_Type: PPyTypeObject;
    PyBuffer_Type: PPyTypeObject;
    PyCallIter_Type: PPyTypeObject;
    PyCell_Type: PPyTypeObject;
    PyClassMethod_Type: PPyTypeObject;
    PyProperty_Type: PPyTypeObject;
    PySeqIter_Type: PPyTypeObject;
    PyStaticMethod_Type: PPyTypeObject;
    PySuper_Type: PPyTypeObject;
  {$IFNDEF PYTHON25_OR_HIGHER}
    PySymtableEntry_Type: PPyTypeObject;
  {$ENDIF}
    PyTraceBack_Type: PPyTypeObject;
    PyUnicode_Type: PPyTypeObject;
    PyWrapperDescr_Type: PPyTypeObject;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    _PyWeakref_RefType: PPyTypeObject;
    _PyWeakref_ProxyType: PPyTypeObject;
    _PyWeakref_CallableProxyType: PPyTypeObject;
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
    PyBaseString_Type: PPyTypeObject;
    PyBool_Type: PPyTypeObject;
    PyEnum_Type: PPyTypeObject;
{$ENDIF}

    //PyArg_GetObject: function(args : PPyObject; nargs, i: integer; p_a: PPPyObject): integer; cdecl;
    //PyArg_GetLong:   function(args : PPyObject; nargs, i: integer; p_a: PLong): integer; cdecl;
    //PyArg_GetShort:  function(args : PPyObject; nargs, i: integer; p_a: PShort): integer; cdecl;
    //PyArg_GetFloat:  function(args : PPyObject; nargs, i: integer; p_a: PFloat): integer; cdecl;
    //PyArg_GetString: function(args : PPyObject; nargs, i: integer; p_a: PString): integer; cdecl;
    //PyArgs_VaParse:  function (args : PPyObject; format: PChar; va_list: array of const): integer; cdecl;
    // Does not work!
    // Py_VaBuildValue: function (format: PChar; va_list: array of const): PPyObject; cdecl;
    //PyBuiltin_Init:     procedure; cdecl;

    PyComplex_FromCComplex: function(c: Py_complex):PPyObject; cdecl;
    PyComplex_FromDoubles: function(realv,imag : double):PPyObject; cdecl;
    PyComplex_RealAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_ImagAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_AsCComplex: function(op : PPyObject ): Py_complex; cdecl;
    PyCFunction_GetFunction: function(ob : PPyObject): Pointer; cdecl;
    PyCFunction_GetSelf: function(ob : PPyObject): PPyObject; cdecl;
    PyCallable_Check: function(ob	: PPyObject): integer; cdecl;
    PyCObject_FromVoidPtr: function(cobj, destruct : Pointer): PPyObject; cdecl;
    PyCObject_AsVoidPtr: function(ob : PPyObject): Pointer; cdecl;
    PyClass_New: function (ob1,ob2,ob3 :  PPyObject): PPyObject; cdecl;
    PyClass_IsSubclass: function (ob1, ob2 : PPyObject): integer cdecl;

    Py_InitModule4: function( name: PChar; methods: PPyMethodDef; doc: PChar;
                              passthrough: PPyObject; Api_Version: Integer):PPyObject; cdecl;
    PyModule_Create2:   function(moduledef: PPyModuleDef; Api_Version: Integer):PPyObject; cdecl;
    PyErr_BadArgument:  function: integer; cdecl;
    PyErr_BadInternalCall: procedure; cdecl;
    PyErr_CheckSignals: function: integer; cdecl;
    PyErr_Clear:        procedure; cdecl;
    PyErr_Fetch:        procedure( errtype, errvalue, errtraceback: PPPyObject); cdecl;
    PyErr_NoMemory:     function: PPyObject; cdecl;
    PyErr_Occurred:     function: PPyObject; cdecl;
    PyErr_Print:        procedure; cdecl;
    PyErr_Restore:      procedure  (errtype, errvalue, errtraceback: PPyObject); cdecl;
    PyErr_SetFromErrno: function (ob :  PPyObject):PPyObject; cdecl;
    PyErr_SetNone:      procedure(value: PPyObject); cdecl;
    PyErr_SetObject:    procedure  (ob1, ob2	: PPyObject); cdecl;
    PyErr_SetString:    procedure( ErrorObject: PPyObject; text: PChar); cdecl;
    PyImport_GetModuleDict: function: PPyObject; cdecl;
    PyInt_FromLong:     function( x: LongInt):PPyObject; cdecl;
    Py_Initialize:      procedure; cdecl;
    Py_Exit:            procedure( RetVal: Integer); cdecl;
    PyEval_GetBuiltins: function: PPyObject; cdecl;
    PyDict_Copy:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_GetItem:     function(mp, key : PPyObject):PPyObject; cdecl;
    PyDict_SetItem:     function(mp, key, item :PPyObject ):integer; cdecl;
    PyDict_DelItem:     function(mp, key : PPyObject ):integer; cdecl;
    PyDict_Clear:       procedure(mp : PPyObject); cdecl;
    PyDict_Next:        function(mp : PPyObject; pos: PInt; key, value: PPPyObject):integer; cdecl;
    PyDict_Keys:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Values:      function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Items:       function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Size:        function(mp: PPyObject):integer; cdecl;
    PyDict_DelItemString: function(dp : PPyObject;key : PChar ):integer; cdecl;
    PyDict_New: function: PPyObject; cdecl;
    PyDict_GetItemString: function( dp: PPyObject; key: PChar): PPyObject; cdecl;
    PyDict_SetItemString: function( dp: PPyObject; key: PChar; item: PPyObject):
                          Integer; cdecl;
{$IFDEF PYTHON22_OR_HIGHER}
    PyDictProxy_New: function (obj : PPyObject) : PPyObject; cdecl;
{$ENDIF}
    PyModule_GetDict:     function( module:PPyObject): PPyObject; cdecl;
    PyObject_Str:         function( v: PPyObject): PPyObject; cdecl;
    PyRun_String:         function( str: PChar; start: Integer; globals: PPyObject;
                                    locals: PPyObject): PPyObject; cdecl;
    PyRun_SimpleString:   function( str: PChar): Integer; cdecl;
    PyString_AsString:    function( ob: PPyObject): PChar; cdecl;
    PySys_SetArgv:        procedure( argc: Integer; argv: PPChar); cdecl;
    PySys_SetArgv3000:    procedure( argc: Integer; argv: PPWideChar); cdecl;

{+ means, Grzegorz or me has tested his non object version of this function}
{+} PyCFunction_New: function(md:PPyMethodDef;ob:PPyObject):PPyObject; cdecl;
{+} PyEval_CallObject: function(callable_obj, args:PPyObject):PPyObject; cdecl;
{-} PyEval_CallObjectWithKeywords:function (callable_obj, args, kw:PPyObject):PPyObject; cdecl;
{-} PyEval_GetFrame:function :PPyObject; cdecl;
{-} PyEval_GetGlobals:function :PPyObject; cdecl;
{-} PyEval_GetLocals:function :PPyObject; cdecl;
{-} //PyEval_GetOwner:function :PPyObject; cdecl;
{-} PyEval_GetRestricted:function :integer; cdecl;

{-} PyEval_InitThreads:procedure; cdecl;
{-} PyEval_RestoreThread:procedure( tstate: PPyThreadState); cdecl;
{-} PyEval_SaveThread:function :PPyThreadState; cdecl;

{-} PyFile_FromString:function (pc1,pc2:PChar):PPyObject; cdecl;
{-} PyFile_GetLine:function (ob:PPyObject;i:integer):PPyObject; cdecl;
{-} PyFile_Name:function (ob:PPyObject):PPyObject; cdecl;
{-} PyFile_SetBufSize:procedure(ob:PPyObject;i:integer); cdecl;
{-} PyFile_SoftSpace:function (ob:PPyObject;i:integer):integer; cdecl;
{-} PyFile_WriteObject:function (ob1,ob2:PPyObject;i:integer):integer; cdecl;
{-} PyFile_WriteString:procedure(s:PChar;ob:PPyObject); cdecl;
{+} PyFloat_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
{+} PyFloat_FromDouble:function (db:double):PPyObject; cdecl;
{-} PyFunction_GetCode:function (ob:PPyObject):PPyObject; cdecl;
{-} PyFunction_GetGlobals:function (ob:PPyObject):PPyObject; cdecl;
{-} PyFunction_New:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyImport_AddModule:function (name:PChar):PPyObject; cdecl;
{-} PyImport_Cleanup:procedure; cdecl;
{-} PyImport_GetMagicNumber:function :LONGINT; cdecl;
{+} PyImport_ImportFrozenModule:function (key:PChar):integer; cdecl;
{+} PyImport_ImportModule:function (name:PChar):PPyObject; cdecl;
{+} PyImport_Import:function (name:PPyObject):PPyObject; cdecl;
{-} //PyImport_Init:procedure; cdecl;
{-} PyImport_ReloadModule:function (ob:PPyObject):PPyObject; cdecl;
{-} PyInstance_New:function (obClass, obArg, obKW:PPyObject):PPyObject; cdecl;
{+} PyInt_AsLong:function (ob:PPyObject):LONGINT; cdecl;
{-} PyList_Append:function (ob1,ob2:PPyObject):integer; cdecl;
{-} PyList_AsTuple:function (ob:PPyObject):PPyObject; cdecl;
{+} PyList_GetItem:function (ob:PPyObject;i:integer):PPyObject; cdecl;
{-} PyList_GetSlice:function (ob:PPyObject;i1,i2:integer):PPyObject; cdecl;
{-} PyList_Insert:function (dp:PPyObject;idx:Integer;item:PPyObject):integer; cdecl;
{-} PyList_New:function (size:integer):PPyObject; cdecl;
{-} PyList_Reverse:function (ob:PPyObject):integer; cdecl;
{-} PyList_SetItem:function (dp:PPyObject;idx:Integer;item:PPyObject):integer; cdecl;
{-} PyList_SetSlice:function (ob:PPyObject;i1,i2:integer;ob2:PPyObject):integer; cdecl;
{+} PyList_Size:function (ob:PPyObject):integer; cdecl;
{-} PyList_Sort:function (ob:PPyObject):integer; cdecl;
{-} PyLong_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
{+} PyLong_AsLong:function (ob:PPyObject):LONGINT; cdecl;
{+} PyLong_FromDouble:function (db:double):PPyObject; cdecl;
{+} PyLong_FromLong:function (l:longint):PPyObject; cdecl;
{-} PyLong_FromString:function (pc:PChar;var ppc:PChar;i:integer):PPyObject; cdecl;
{-} PyLong_FromUnsignedLong:function(val:cardinal) : PPyObject; cdecl;
{-} PyLong_AsUnsignedLong:function(ob:PPyObject) : Cardinal; cdecl;
{$IFDEF UNICODE_SUPPORT}
{-} PyLong_FromUnicode:function(ob:PPyObject; a, b : integer) : PPyObject; cdecl;
{$ENDIF}
{$IFDEF DELPHI6_OR_HIGHER}
{-} PyLong_FromLongLong:function(val:Int64) : PPyObject; cdecl;
{-} PyLong_AsLongLong:function(ob:PPyObject) : Int64; cdecl;
{$ENDIF}
{-} PyMapping_Check:function (ob:PPyObject):integer; cdecl;
{-} PyMapping_GetItemString:function (ob:PPyObject;key:PChar):PPyObject; cdecl;
{-} PyMapping_HasKey:function (ob,key:PPyObject):integer; cdecl;
{-} PyMapping_HasKeyString:function (ob:PPyObject;key:PChar):integer; cdecl;
{-} PyMapping_Length:function (ob:PPyObject):integer; cdecl;
{-} PyMapping_SetItemString:function (ob:PPyObject; key:PChar; value:PPyObject):integer; cdecl;
{-} PyMethod_Class:function (ob:PPyObject):PPyObject; cdecl;
{-} PyMethod_Function:function (ob:PPyObject):PPyObject; cdecl;
{-} PyMethod_New:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
{-} PyMethod_Self:function (ob:PPyObject):PPyObject; cdecl;
{-} PyModule_GetName:function (ob:PPyObject):PChar; cdecl;
{-} PyModule_New:function (key:PChar):PPyObject; cdecl;
{-} PyNumber_Absolute:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Add:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_And:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Check:function (ob:PPyObject):integer; cdecl;
{-} PyNumber_Coerce:function (var ob1,ob2:PPyObject):integer; cdecl;
{-} PyNumber_Divide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{$IFDEF PYTHON22_OR_HIGHER}
{-} PyNumber_FloorDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_TrueDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{$ENDIF}
{-} PyNumber_Divmod:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Float:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Int:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Invert:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Long:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Lshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Multiply:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Negative:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Or:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Positive:function (ob:PPyObject):PPyObject; cdecl;
{-} PyNumber_Power:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
{-} PyNumber_Remainder:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Rshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Subtract:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyNumber_Xor:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyOS_InitInterrupts:procedure; cdecl;
{-} PyOS_InterruptOccurred:function :integer; cdecl;
{+} PyObject_CallObject:function (ob,args:PPyObject):PPyObject; cdecl;
    PyObject_CallMethodStr: function ( obj : PPyObject; method, format, value : PChar ) : PPyObject; cdecl;
    PyObject_Compare: function (ob1,ob2:PPyObject):integer; cdecl;
    PyObject_RichCompare:function (ob1,ob2:PPyObject;opid:integer):PPyObject; cdecl;
    PyObject_RichCompareBool:function (ob1,ob2:PPyObject;opid:integer):Integer; cdecl;
{-} PyObject_GetAttr:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{+} PyObject_GetAttrString:function (ob:PPyObject;c:PChar):PPyObject; cdecl;
{-} PyObject_GetItem:function (ob,key:PPyObject):PPyObject; cdecl;
{-} PyObject_DelItem:function (ob,key:PPyObject):PPyObject; cdecl;
{-} PyObject_HasAttrString:function (ob:PPyObject;key:PChar):integer; cdecl;
{-} PyObject_Hash:function (ob:PPyObject):LONGINT; cdecl;
{-} PyObject_IsTrue:function (ob:PPyObject):integer; cdecl;
{-} PyObject_Length:function (ob:PPyObject):integer; cdecl;
{-} PyObject_Repr:function (ob:PPyObject):PPyObject; cdecl;
{-} PyObject_SetAttr:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
{-} PyObject_SetAttrString:function (ob:PPyObject;key:Pchar;value:PPyObject):integer; cdecl;
{-} PyObject_SetItem:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
{$IFDEF PYTHON20_OR_HIGHER}
{-} PyObject_Init:function (ob:PPyObject; t:PPyTypeObject):PPyObject; cdecl;
{-} PyObject_InitVar:function (ob:PPyObject; t:PPyTypeObject; size:integer):PPyObject; cdecl;
{-} PyObject_New:function (t:PPyTypeObject):PPyObject; cdecl;
{-} PyObject_NewVar:function (t:PPyTypeObject; size:integer):PPyObject; cdecl;
    PyObject_Free:procedure (ob:PPyObject); cdecl;
    PyObject_GetIter: function (obj: PPyObject) : PPyObject; cdecl;
    PyIter_Next: function (obj: PPyObject) : PPyObject; cdecl;
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
{-} PyObject_IsInstance:function (inst, cls:PPyObject):integer; cdecl;
{-} PyObject_IsSubclass:function (derived, cls:PPyObject):integer; cdecl;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    PyObject_Call:function (ob, args, kw:PPyObject):PPyObject; cdecl;
    PyObject_GenericGetAttr:function (obj, name : PPyObject) : PPyObject; cdecl;
    PyObject_GenericSetAttr:function (obj, name, value : PPyObject) : Integer; cdecl;
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
{-} PyObject_GC_Malloc:function (size:integer):PPyObject; cdecl;
{-} PyObject_GC_New:function (t:PPyTypeObject):PPyObject; cdecl;
{-} PyObject_GC_NewVar:function (t:PPyTypeObject; size:integer):PPyObject; cdecl;
{-} PyObject_GC_Resize:function (t:PPyObject; newsize:integer):PPyObject; cdecl;
{-} PyObject_GC_Del:procedure (ob:PPyObject); cdecl;
{-} PyObject_GC_Track:procedure (ob:PPyObject); cdecl;
{-} PyObject_GC_UnTrack:procedure (ob:PPyObject); cdecl;
{$ENDIF}
{$IFNDEF PYTHON25_OR_HIGHER}
{-} PyRange_New:function (l1,l2,l3:longint;i:integer):PPyObject; cdecl;
{$ENDIF}
{-} PySequence_Check:function (ob:PPyObject):integer; cdecl;
{-} PySequence_Concat:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PySequence_Count:function (ob1,ob2:PPyObject):integer; cdecl;
{-} PySequence_GetItem:function (ob:PPyObject;i:integer):PPyObject; cdecl;
{-} PySequence_GetSlice:function (ob:PPyObject;i1,i2:integer):PPyObject; cdecl;
{-} PySequence_In:function (ob1,ob2:PPyObject):integer; cdecl;
{-} PySequence_Index:function (ob1,ob2:PPyObject):integer; cdecl;
{-} PySequence_Length:function (ob:PPyObject):integer; cdecl;
{-} PySequence_Repeat:function (ob:PPyObject;count:integer):PPyObject; cdecl;
{-} PySequence_SetItem:function (ob:PPyObject;i:integer;value:PPyObject):integer; cdecl;
{-} PySequence_SetSlice:function (ob:PPyObject;i1,i2:integer;value:PPyObject):integer; cdecl;
{-} PySequence_DelSlice:function (ob:PPyObject;i1,i2:integer):integer; cdecl;
{-} PySequence_Tuple:function (ob:PPyObject):PPyObject; cdecl;
{$IFDEF PYTHON20_OR_HIGHER}
{-} PySequence_Contains:function (ob, value:PPyObject):integer; cdecl;
    PySeqIter_New: function(obj : PPyObject) : PPyObject; cdecl;
{$ENDIF}
{-} PySlice_GetIndices:function (ob:PPySliceObject;length:integer;var start,stop,step:integer):integer; cdecl;
{$IFDEF PYTHON23_OR_HIGHER}
{-} PySlice_GetIndicesEx:function (ob:PPySliceObject;length:integer;var start,stop,step,slicelength:integer):integer; cdecl;
{$ENDIF}
{-} PySlice_New:function (start,stop,step:PPyObject):PPyObject; cdecl;
{-} PyString_Concat:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
{-} PyString_ConcatAndDel:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
{-} PyString_Format:function (ob1,ob2:PPyObject):PPyObject; cdecl;
{-} PyString_FromStringAndSize:function (s:PChar;i:integer):PPyObject; cdecl;
{-} PyString_Size:function (ob:PPyObject):integer; cdecl;
{$IFDEF PYTHON23_OR_HIGHER}
{-} PyString_DecodeEscape:function(s:PChar; len:integer; errors:PChar; unicode:integer; recode_encoding:PChar):PPyObject; cdecl;
{-} PyString_Repr:function(ob:PPyObject; smartquotes:integer):PPyObject; cdecl;
{$ENDIF}
{+} PySys_GetObject:function (s:PChar):PPyObject; cdecl;
{-} //PySys_Init:procedure; cdecl;
{-} PySys_SetObject:function (s:PChar;ob:PPyObject):integer; cdecl;
{-} PySys_SetPath:procedure(path:PChar); cdecl;
{-} //PyTraceBack_Fetch:function :PPyObject; cdecl;
{-} PyTraceBack_Here:function (p:pointer):integer; cdecl;
{-} PyTraceBack_Print:function (ob1,ob2:PPyObject):integer; cdecl;
{-} //PyTraceBack_Store:function (ob:PPyObject):integer; cdecl;
{+} PyTuple_GetItem:function (ob:PPyObject;i:integer):PPyObject; cdecl;
{-} PyTuple_GetSlice:function (ob:PPyObject;i1,i2:integer):PPyObject; cdecl;
{+} PyTuple_New:function (size:Integer):PPyObject; cdecl;
{+} PyTuple_SetItem:function (ob:PPyObject;key:integer;value:PPyObject):integer; cdecl;
{+} PyTuple_Size:function (ob:PPyObject):integer; cdecl;
{$IFDEF PYTHON22_OR_HIGHER}
{+} PyType_IsSubtype:function (a, b : PPyTypeObject):integer; cdecl;
    PyType_GenericAlloc:function(atype: PPyTypeObject; nitems:Integer) : PPyObject; cdecl;
    PyType_GenericNew:function(atype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
    PyType_Ready:function(atype: PPyTypeObject) : integer; cdecl;
{$ENDIF}
{$IFDEF UNICODE_SUPPORT}
{+} PyUnicode_FromWideChar:function (const w:PWideChar; size:integer):PPyObject; cdecl;
{+} PyUnicode_AsWideChar:function (unicode: PPyObject; w:PWideChar; size:integer):integer; cdecl;
    PyUnicode_Decode:function (const s:PChar; size: integer; const encoding : PChar; const errors: PChar):PPyObject; cdecl;
    PyUnicode_AsEncodedString:function (unicode:PPyObject; const encoding:PChar; const errors:PChar):PPyObject; cdecl;
{$IFDEF PYTHON23_OR_HIGHER}
{-} PyUnicode_FromOrdinal:function (ordinal:integer):PPyObject; cdecl;
{$ENDIF}
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    PyWeakref_GetObject: function ( ref : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewProxy: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewRef: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWrapper_New: function ( ob1, ob2 : PPyObject) : PPyObject; cdecl;
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
    PyBool_FromLong: function ( ok : Integer) : PPyObject; cdecl;
    PyThreadState_SetAsyncExc: function(t_id :LongInt; exc :PPyObject) : Integer; cdecl;   
{$ENDIF}
{-} Py_AtExit:function (proc: AtExitProc):integer; cdecl;
{-} //Py_Cleanup:procedure; cdecl;
{-} Py_CompileString:function (s1,s2:PChar;i:integer):PPyObject; cdecl;
{-} Py_FatalError:procedure(s:PChar); cdecl;
{-} Py_FindMethod:function (md:PPyMethodDef;ob:PPyObject;key:PChar):PPyObject; cdecl;
{-} Py_FindMethodInChain:function (mc:PPyMethodChain;ob:PPyObject;key:PChar):PPyObject; cdecl;
{-} _PyObject_New:function (obt:PPyTypeObject;ob:PPyObject):PPyObject; cdecl;
{-} _PyString_Resize:function (var ob:PPyObject;i:integer):integer; cdecl;
{+} Py_Finalize                     : procedure; cdecl;
{-} PyErr_ExceptionMatches          : function ( exc : PPyObject) : Integer; cdecl;
{-} PyErr_GivenExceptionMatches     : function ( raised_exc, exc : PPyObject) : Integer; cdecl;
{-} PyEval_EvalCode                 : function ( co : PPyCodeObject; globals, locals : PPyObject) : PPyObject; cdecl;
{+} Py_GetVersion                   : function : PChar; cdecl;
{+} Py_GetCopyright                 : function : PChar; cdecl;
{+} Py_GetExecPrefix                : function : PChar; cdecl;
{+} Py_GetPath                      : function : PChar; cdecl;
{+} Py_GetPrefix                    : function : PChar; cdecl;
{+} Py_GetProgramName               : function : PChar; cdecl;

{-} PyParser_SimpleParseString      : function ( str : PChar; start : Integer) : PNode; cdecl;
{-} PyNode_Free                     : procedure( n : PNode ); cdecl;
{-} PyErr_NewException              : function ( name : PChar; base, dict : PPyObject ) : PPyObject; cdecl;
{-} Py_Malloc                       : function ( size : Integer ) : Pointer;
{-} PyMem_Malloc                    : function ( size : Integer ) : Pointer;
{-} PyObject_CallMethod             : function ( obj : PPyObject; method, format : PChar ) : PPyObject; cdecl;

{New exported Objects in Python 1.5}
    Py_SetProgramName               : procedure( name: PChar); cdecl;
    Py_SetProgramName3000           : procedure( name: PWideChar); cdecl;
    Py_IsInitialized                : function : integer; cdecl;
    Py_GetProgramFullPath           : function : PChar; cdecl;
    Py_NewInterpreter               : function : PPyThreadState; cdecl;
    Py_EndInterpreter               : procedure( tstate: PPyThreadState); cdecl;
    PyEval_AcquireLock              : procedure; cdecl;
    PyEval_ReleaseLock              : procedure; cdecl;
    PyEval_AcquireThread            : procedure( tstate: PPyThreadState); cdecl;
    PyEval_ReleaseThread            : procedure( tstate: PPyThreadState); cdecl;
    PyInterpreterState_New          : function : PPyInterpreterState; cdecl;
    PyInterpreterState_Clear        : procedure( interp: PPyInterpreterState); cdecl;
    PyInterpreterState_Delete       : procedure( interp: PPyInterpreterState); cdecl;
    PyThreadState_New               : function ( interp: PPyInterpreterState): PPyThreadState; cdecl;
    PyThreadState_Clear             : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Delete            : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Get               : function : PPyThreadState; cdecl;
    PyThreadState_Swap              : function ( tstate: PPyThreadState): PPyThreadState; cdecl;
    PyErr_SetInterrupt              : procedure; cdecl;

{Further exported Objects, may be implemented later}
{
    PyCode_New: Pointer;
    PyErr_SetInterrupt: Pointer;
    PyFile_AsFile: Pointer;
    PyFile_FromFile: Pointer;
    PyFloat_AsString: Pointer;
    PyFrame_BlockPop: Pointer;
    PyFrame_BlockSetup: Pointer;
    PyFrame_ExtendStack: Pointer;
    PyFrame_FastToLocals: Pointer;
    PyFrame_LocalsToFast: Pointer;
    PyFrame_New: Pointer;
    PyGrammar_AddAccelerators: Pointer;
    PyGrammar_FindDFA: Pointer;
    PyGrammar_LabelRepr: Pointer;
    PyInstance_DoBinOp: Pointer;
    PyInt_GetMax: Pointer;
    PyMarshal_Init: Pointer;
    PyMarshal_ReadLongFromFile: Pointer;
    PyMarshal_ReadObjectFromFile: Pointer;
    PyMarshal_ReadObjectFromString: Pointer;
    PyMarshal_WriteLongToFile: Pointer;
    PyMarshal_WriteObjectToFile: Pointer;
    PyMember_Get: Pointer;
    PyMember_Set: Pointer;
    PyNode_AddChild: Pointer;
    PyNode_Compile: Pointer;
    PyNode_New: Pointer;
    PyOS_GetLastModificationTime: Pointer;
    PyOS_Readline: Pointer;
    PyOS_strtol: Pointer;
    PyOS_strtoul: Pointer;
    PyObject_CallFunction: Pointer;
    PyObject_CallMethod: Pointer;
    PyObject_Print: Pointer;
    PyParser_AddToken: Pointer;
    PyParser_Delete: Pointer;
    PyParser_New: Pointer;
    PyParser_ParseFile: Pointer;
    PyParser_ParseString: Pointer;
    PyParser_SimpleParseFile: Pointer;
    PyRun_AnyFile: Pointer;
    PyRun_File: Pointer;
    PyRun_InteractiveLoop: Pointer;
    PyRun_InteractiveOne: Pointer;
    PyRun_SimpleFile: Pointer;
    PySys_GetFile: Pointer;
    PyToken_OneChar: Pointer;
    PyToken_TwoChars: Pointer;
    PyTokenizer_Free: Pointer;
    PyTokenizer_FromFile: Pointer;
    PyTokenizer_FromString: Pointer;
    PyTokenizer_Get: Pointer;
    Py_Main: Pointer;
    _PyObject_NewVar: Pointer;
    _PyParser_Grammar: Pointer;
    _PyParser_TokenNames: Pointer;
    _PyThread_Started: Pointer;
    _Py_c_diff: Pointer;
    _Py_c_neg: Pointer;
    _Py_c_pow: Pointer;
    _Py_c_prod: Pointer;
    _Py_c_quot: Pointer;
    _Py_c_sum: Pointer;
}
  // functions redefined in Delphi
  procedure   Py_INCREF   ( op: PPyObject);
  procedure   Py_DECREF   ( op: PPyObject);
  procedure   Py_XINCREF  ( op: PPyObject);
  procedure   Py_XDECREF  ( op: PPyObject);

  function    Py_GetPlatform: PChar; cdecl;

  function PyArg_Parse     ( args: PPyObject; format: PChar;
                             argp: array of Pointer): Integer; cdecl;
  function PyArg_ParseTuple( args: PPyObject; format: PChar;
                             argp: array of Pointer): Integer; cdecl;
// This function handles all cardinals, pointer types (with no adjustment of pointers!)
// (Extended) floats, which are handled as Python doubles and currencies, handled
// as (normalized) Python doubles.
  function Py_BuildValue( format: PChar; args: array of const): PPyObject; cdecl;
  function PyObject_CallMethodWithArgs( obj : PPyObject; method,
                                        format: PChar; args: array of const): PPyObject; cdecl;
  function PyCode_Addr2Line( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
  function Py_GetBuildInfo: PChar; cdecl;
  function PyImport_ExecCodeModule( const AName : String; codeobject : PPyObject) : PPyObject;
  function PyString_Check( obj : PPyObject ) : Boolean;
  function PyString_CheckExact( obj : PPyObject ) : Boolean;
  function PyFloat_Check( obj : PPyObject ) : Boolean;
  function PyFloat_CheckExact( obj : PPyObject ) : Boolean;
  function PyInt_Check( obj : PPyObject ) : Boolean;
  function PyInt_CheckExact( obj : PPyObject ) : Boolean;
  function PyLong_Check( obj : PPyObject ) : Boolean;
  function PyLong_CheckExact( obj : PPyObject ) : Boolean;
  function PyTuple_Check( obj : PPyObject ) : Boolean;
  function PyTuple_CheckExact( obj : PPyObject ) : Boolean;
  function PyInstance_Check( obj : PPyObject ) : Boolean;
  function PyClass_Check( obj : PPyObject ) : Boolean;
  function PyType_CheckExact( obj : PPyObject ) : Boolean;
  function PyMethod_Check( obj : PPyObject ) : Boolean;
  function PyList_Check( obj : PPyObject ) : Boolean;
  function PyList_CheckExact( obj : PPyObject ) : Boolean;
  function PyDict_Check( obj : PPyObject ) : Boolean;
  function PyDict_CheckExact( obj : PPyObject ) : Boolean;
  function PyModule_Check( obj : PPyObject ) : Boolean;
  function PyModule_CheckExact( obj : PPyObject ) : Boolean;
  function PySlice_Check( obj : PPyObject ) : Boolean;
  function PyFunction_Check( obj : PPyObject ) : Boolean;
  function PyIter_Check( obj : PPyObject ) : Boolean;
{$IFDEF UNICODE_SUPPORT}
  function PyUnicode_Check( obj : PPyObject ) : Boolean;
  function PyUnicode_CheckExact( obj : PPyObject ) : Boolean;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  function PyType_IS_GC(t : PPyTypeObject ) : Boolean;
  function PyObject_IS_GC( obj : PPyObject ) : Boolean;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  function PyWeakref_Check( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckRef( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckProxy( obj : PPyObject ) : Boolean;
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
  function PyBool_Check( obj : PPyObject ) : Boolean;
  function PyBaseString_Check( obj : PPyObject ) : Boolean;
  function PyEnum_Check( obj : PPyObject ) : Boolean;
{$ENDIF}
  function PyObject_TypeCheck(obj:PPyObject; t:PPyTypeObject) : Boolean;
  function Py_InitModule( const AName : PChar; md : PPyMethodDef) : PPyObject;
  function PyString_FromString( str: PChar): PPyObject; virtual; abstract;
  function PyString_AsDelphiString( ob: PPyObject): string;  virtual; abstract;
  procedure Py_FlushLine; cdecl;

  // Constructors & Destructors
  constructor Create(AOwner: TComponent); override;

  // Public methods
  procedure MapDll;

  // Public properties
  property Initialized : Boolean read GetInitialized;
  property Finalizing : Boolean read FFinalizing;
  property IsPython3000 : Boolean read FIsPython3000;
  property BuiltInModuleName: String read FBuiltInModuleName write FBuiltInModuleName;

end;

//--------------------------------------------------------
//--                                                    --
//-- class:  TPythonEngine derived from TPythonInterface--
//-- Pytrunobject providing interface for               --
//-- running Python into Delphi                         --
//--------------------------------------------------------
type
  TDatetimeConversionMode = (dcmToTuple, dcmToDatetime);
const
  DEFAULT_DATETIME_CONVERSION_MODE = dcmToTuple;
type
  TEngineClient = class;
  TPathInitializationEvent = procedure ( Sender : TObject; var Path : String ) of Object;
  TSysPathInitEvent = procedure ( Sender : TObject; PathList : PPyObject ) of Object;
  TPythonFlag = (pfDebug, pfInteractive, pfNoSite, pfOptimize, pfTabcheck, pfUnicode, pfVerbose,
                 pfUseClassExceptionsFlag, pfFrozenFlag, pfIgnoreEnvironmentFlag, pfDivisionWarningFlag);
  TPythonFlags = set of TPythonFlag;


  TTracebackItem = class
    FileName : String;
    LineNo : Integer;
    Context : String;
  end;

  TPythonTraceback = class
    protected
      FItems : TList;
      FLimit : Integer;

      function GetItemCount : Integer;
      function GetItem( idx : Integer ) : TTracebackItem;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;
      procedure Refresh;

      property ItemCount : Integer read GetItemCount;
      property Items[ idx : Integer ] : TTracebackItem read GetItem;
      property Limit : Integer read FLimit write FLimit;
  end;

  TPythonEngine = class(TPythonInterface)
  private
    FInitScript:                 TStrings;
    FIO:                         TPythonInputOutput;
    FRedirectIO:                 Boolean;
    FOnAfterInit:                TNotifyEvent;
    FClients:                    TList;
    FLock:                       TCriticalSection;
    FExecModule:                 String;
    FAutoFinalize:               Boolean;
    FProgramName:                String;
    FProgramNameW:               WideString;
    FInitThreads:                Boolean;
    FOnPathInitialization:       TPathInitializationEvent;
    FOnSysPathInit:              TSysPathInitEvent;
    FTraceback:                  TPythonTraceback;
    FUseWindowsConsole:          Boolean;
    FGlobalVars:                 PPyObject;
    FLocalVars:                  PPyObject;
    FPyFlags:                    TPythonFlags;
    FIORedirected:               Boolean;
    FIOPythonModule:             TObject;
    FDatetimeConversionMode:     TDatetimeConversionMode;
    FTimeStruct:                 PPyObject;
    FPyDateTime_DateType:        PPyObject;
    FPyDateTime_DateTimeType:    PPyObject;
    FPyDateTime_DeltaType:       PPyObject;
    FPyDateTime_TimeType:        PPyObject;
    FPyDateTime_TZInfoType:      PPyObject;
    FPyDateTime_TimeTZType:      PPyObject;
    FPyDateTime_DateTimeTZType:  PPyObject;
    function  GetVersion: String;
    procedure SetVersion(const Value: String);

  protected
    procedure AfterLoad; override;
    procedure BeforeLoad; override;
    procedure DoOpenDll(const aDllName : String); override;
    procedure SetInitScript(Value: TStrings);
    function  GetThreadState: PPyThreadState;
    function  GetInterpreterState: PPyInterpreterState;
    procedure SetInitThreads(Value: Boolean);
    function  GetClientCount : Integer;
    function  GetClients( idx : Integer ) : TEngineClient;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CheckRegistry;
    procedure SetProgramArgs;
    procedure InitWinConsole;
    procedure SetUseWindowsConsole( const Value : Boolean );
    procedure SetGlobalVars(const Value: PPyObject);
    procedure SetLocalVars(const Value: PPyObject);
    procedure SetPyFlags(const Value: TPythonFlags);
    procedure AssignPyFlags;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // Public methods
    procedure  Initialize;
    procedure  Finalize;
    procedure  Lock;
    procedure  Unlock;
    function   IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
    function   GetAttrString(obj: PPyObject; AName: PChar):PChar;
    function   CleanString(const s : string) : string;
    function   Run_CommandAsString(const command : String; mode : Integer) : String;
    function   Run_CommandAsObject(const command : String; mode : Integer) : PPyObject;
    function   Run_CommandAsObjectWithDict(const command : String; mode : Integer; locals, globals : PPyObject) : PPyObject;
    procedure  ExecString(const command : String); {$IFDEF DELPHI4_OR_HIGHER}overload;{$ENDIF}
    procedure  ExecStrings( strings : TStrings ); {$IFDEF DELPHI4_OR_HIGHER}overload;{$ENDIF}
    function   EvalString(const command : String) : PPyObject; {$IFDEF DELPHI4_OR_HIGHER}overload;{$ENDIF}
    function   EvalStringAsStr(const command : String) : String;
    function   EvalStrings( strings : TStrings ) : PPyObject; {$IFDEF DELPHI4_OR_HIGHER}overload;{$ENDIF}
{$IFDEF DELPHI4_OR_HIGHER}
    procedure  ExecString(const command : String; locals, globals : PPyObject ); overload;
    procedure  ExecStrings( strings : TStrings; locals, globals : PPyObject ); overload;
    function   EvalString( const command : String; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject; overload;
{$ENDIF}
    function   EvalStringsAsStr( strings : TStrings ) : String;
    function   EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
    function   EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
    function   EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
    function   CheckEvalSyntax( const str : String ) : Boolean;
    function   CheckExecSyntax( const str : String ) : Boolean;
    function   CheckSyntax( const str : String; mode : Integer ) : Boolean;
    procedure  RaiseError;
    function   PyObjectAsString( obj : PPyObject ) : String;
    procedure  DoRedirectIO;
    procedure  AddClient( client : TEngineClient );
    procedure  RemoveClient( client : TEngineClient );
    function   FindClient( const aName : String ) : TEngineClient;
    function   TypeByName( const aTypeName : String ) : PPyTypeObject;
    function   ModuleByName( const aModuleName : String ) : PPyObject;
    function   MethodsByName( const aMethodsContainer: String ) : PPyMethodDef;
    function   VariantAsPyObject( const V : Variant ) : PPyObject; virtual;
    function   PyObjectAsVariant( obj : PPyObject ) : Variant; virtual;
    function   VarRecAsPyObject( v : TVarRec ) : PPyObject;
    function   MakePyTuple( const objects : array of PPyObject ) : PPyObject;
    function   MakePyList( const objects : array of PPyObject ) : PPyObject;
    function   ArrayToPyTuple( items : array of const) : PPyObject;
    function   ArrayToPyList( items : array of const) : PPyObject;
    function   ArrayToPyDict( items : array of const) : PPyObject;
    function   StringsToPyList( strings : TStrings ) : PPyObject;
    function   StringsToPyTuple( strings : TStrings ) : PPyObject;
    procedure  PyListToStrings( list : PPyObject; strings : TStrings );
    procedure  PyTupleToStrings( tuple: PPyObject; strings : TStrings );
{$IFDEF UNICODE_SUPPORT}
    function   PyUnicode_AsWideString( obj : PPyObject ) : WideString;
    function   PyUnicode_FromWideString( const AString : WideString) : PPyObject;
{$ENDIF}
    function   ReturnNone : PPyObject;
    function   FindModule( const ModuleName : String ) : PPyObject;
    function   FindFunction(ModuleName,FuncName: String): PPyObject;
    function   SetToList( data : Pointer; size : Integer ) : PPyObject;
    procedure  ListToSet( List : PPyObject; data : Pointer; size : Integer );
    procedure  CheckError(ACatchStopEx : Boolean {$IFDEF DELPHI4_OR_HIGHER}= False{$ENDIF});
    function   GetMainModule : PPyObject;
    function   PyTimeStruct_Check( obj : PPyObject ) : Boolean;
    { Date, Time, DateTime and related objects check functions }
    function   PyDate_Check( obj : PPyObject ) : Boolean;
    function   PyDate_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDateTime_Check( obj : PPyObject ) : Boolean;
    function   PyDateTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTime_Check( obj : PPyObject ) : Boolean;
    function   PyTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDelta_Check( obj : PPyObject ) : Boolean;
    function   PyDelta_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTZInfo_Check( obj : PPyObject ) : Boolean;
    function   PyTZInfo_CheckExact( obj : PPyObject ) : Boolean;
    { Apply for date and datetime instances. }
    function   PyDateTime_GET_YEAR( obj : PPyObject ) : Integer;
    function   PyDateTime_GET_MONTH( obj : PPyObject ) : Integer;
    function   PyDateTime_GET_DAY( obj : PPyObject ) : Integer;
    function   PyDateTime_DATE_GET_HOUR( obj : PPyObject ) : Integer;
    function   PyDateTime_DATE_GET_MINUTE( obj : PPyObject ) : Integer;
    function   PyDateTime_DATE_GET_SECOND( obj : PPyObject ) : Integer;
    function   PyDateTime_DATE_GET_MICROSECOND( obj : PPyObject ) : Integer;
    { Apply for time instances. }
    function   PyDateTime_TIME_GET_HOUR( obj : PPyObject ) : Integer;
    function   PyDateTime_TIME_GET_MINUTE( obj : PPyObject ) : Integer;
    function   PyDateTime_TIME_GET_SECOND( obj : PPyObject ) : Integer;
    function   PyDateTime_TIME_GET_MICROSECOND( obj : PPyObject ) : Integer;
    { end date/time functions }
    function   PyString_FromString( str: PChar): PPyObject; override;
    function PyString_AsDelphiString( ob: PPyObject): string; override;

    // Public Properties
    property ClientCount : Integer read GetClientCount;
    property Clients[ idx : Integer ] : TEngineClient read GetClients;
    property ExecModule : String read FExecModule write FExecModule;
    property ThreadState: PPyThreadState read GetThreadState;
    property InterpreterState: PPyInterpreterState read GetInterpreterState;
    property Traceback : TPythonTraceback read FTraceback;
    property LocalVars : PPyObject read FLocalVars Write SetLocalVars;
    property GlobalVars : PPyObject read FGlobalVars Write SetGlobalVars;
  published
    property AutoFinalize: Boolean read FAutoFinalize write FAutoFinalize default True;
    property DatetimeConversionMode: TDatetimeConversionMode read FDatetimeConversionMode write FDatetimeConversionMode default DEFAULT_DATETIME_CONVERSION_MODE;
    property InitScript: TStrings read FInitScript write SetInitScript;
    property InitThreads: Boolean read FInitThreads write SetInitThreads default False;
    property IO: TPythonInputOutput read FIO write FIO;
    property PyFlags: TPythonFlags read FPyFlags write SetPyFlags default [];
    property RedirectIO: Boolean read FRedirectIO write FRedirectIO default True;
    property UseWindowsConsole: Boolean read FUseWindowsConsole write FUseWindowsConsole default False;
    property Version : String read GetVersion write SetVersion stored False;
    property OnAfterInit: TNotifyEvent read FOnAfterInit write FOnAfterInit;
    property OnPathInitialization: TPathInitializationEvent read FOnPathInitialization write FOnPathInitialization;
    property OnSysPathInit: TSysPathInitEvent read FOnSysPathInit write FOnSysPathInit;
  end;


//-------------------------------------------------------
//--                                                   --
//--      Base class:  TEngineClient                   --
//--                                                   --
//-------------------------------------------------------

  TEngineClient = class(TComponent)
    protected
      FEngine : TPythonEngine;
      FOnInitialization : TNotifyEvent;
      FOnFinalization : TNotifyEvent;
      FOnCreate : TNotifyEvent;
      FOnDestroy : TNotifyEvent;
      FInitialized : Boolean;

      procedure SetEngine( val : TPythonEngine ); virtual;
      procedure Loaded; override;
      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      procedure ModuleReady(Sender : TObject); virtual;
    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public Methods
      procedure Initialize; virtual;
      procedure Finalize; virtual;
      procedure ClearEngine;
      procedure CheckEngine;

      // Public Properties
      property Initialized: Boolean read FInitialized;

    published
      property Engine : TPythonEngine read FEngine write SetEngine;
      property OnCreate : TNotifyEvent read FOnCreate write FOnCreate;
      property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;
      property OnFinalization : TNotifyEvent read FOnFinalization write FOnFinalization;
      property OnInitialization : TNotifyEvent read FOnInitialization write FOnInitialization;
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TMethodsContainer derived from TEngineClient--
//--                                                   --
//-------------------------------------------------------

  TMethodArray = array[ 0 .. 16000 ] of PyMethodDef;
  PMethodArray = ^TMethodArray;
  TDelphiMethod = function ( self, args : PPyObject ) : PPyObject of object; cdecl;
  TDelphiMethodWithKW = function ( self, args, keywords : PPyObject ) : PPyObject of object; cdecl;
  TPythonEvent = procedure(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject) of object;
  TMethodsContainer = class; // forward declaration
  TEventDefs = class; // forward declaration

  // Event Collection Item
  TEventDef = class(TCollectionItem)
  private
    FName: String;
    FTmpDocString: String;
    FOnExecute: TPythonEvent;
    FDocString: TStringList;
 	 procedure SetDocString(const Value: TStringList);
  protected
    function  GetDisplayName: String; {$IFNDEF FPC} override; {$ENDIF}
    procedure SetDisplayName(const Value: String); {$IFNDEF FPC} override; {$ENDIF}
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function  GetDocString : String;
    function  PythonEvent(pself, args: PPyObject): PPyObject; cdecl;
    function  Owner : TEventDefs;
  published
    property Name: string read FName write SetDisplayName;
    property OnExecute: TPythonEvent read FOnExecute write FOnExecute;
    property DocString: TStringList read FDocString write SetDocString;
  end;

  // Event Collection
  TEventDefs = class(TCollection)
  protected
    FMethodsContainer : TMethodsContainer;

    function  GetItems( idx : Integer ) : TEventDef;
    procedure SetItems( idx : Integer; Value : TEventDef );
    function  GetOwner: TPersistent; override;
  public
    constructor Create( AMethodsContainer : TMethodsContainer );

    function  Add : TEventDef;
    procedure RegisterEvents;

    property Items[ idx : Integer ] : TEventDef read GetItems;
    property Container : TMethodsContainer read FMethodsContainer;
  end;

  // class TMethodsContainer
  TMethodsContainer = class(TEngineClient)
    private
      FMethodCount : Integer;
      FAllocatedMethodCount : Integer;
      FMethods : PPyMethodDef;
      FEventDefs: TEventDefs;

      procedure AllocMethods;
      procedure FreeMethods;
      function  GetMethods( idx : Integer ) : PPyMethodDef;
      function  StoreEventDefs: Boolean;

    protected
      procedure ReallocMethods; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure Initialize; override;
      procedure Finalize; override;

      function  AddMethod( AMethodName  : PChar;
                           AMethod  : PyCFunction;
                           ADocString : PChar ) : PPyMethodDef;
      function  AddMethodWithKeywords( AMethodName  : PChar;
                                       AMethod  : PyCFunctionWithKW;
                                       ADocString : PChar ) : PPyMethodDef;
      function  AddDelphiMethod( AMethodName  : PChar;
                                 ADelphiMethod: TDelphiMethod;
                                 ADocString : PChar ) : PPyMethodDef;
      function  AddDelphiMethodWithKeywords(  AMethodName  : PChar;
                                              ADelphiMethod: TDelphiMethodWithKW;
                                              ADocString : PChar ) : PPyMethodDef;
      procedure ClearMethods;

      // properties
      property MethodCount : Integer read FMethodCount;
      property Methods[ idx : Integer ] : PPyMethodDef read GetMethods;
      property MethodsData : PPyMethodDef read FMethods;

    published
      property Events: TEventDefs read fEventDefs write fEventDefs stored StoreEventDefs;
  end;


//------------------------------------------------------------
//--                                                        --
//--class: TMembersContainer derived from TMethodsContainer --
//--                                                        --
//------------------------------------------------------------

{$IFDEF PYTHON22_OR_HIGHER}
  TMemberArray = array[ 0 .. 16000 ] of PyMemberDef;
  PMemberArray = ^TMemberArray;
{$ENDIF}

  // class TMembersContainer
  TMembersContainer = class(TMethodsContainer)
    protected
      function  GetMembersStartOffset : Integer; virtual;
{$IFDEF PYTHON22_OR_HIGHER}
    private
      FMemberCount : Integer;
      FAllocatedMemberCount : Integer;
      FMembers : PPyMemberDef;

      procedure AllocMembers;
      procedure FreeMembers;
      function  GetMembers( idx : Integer ) : PPyMemberDef;

    protected
      procedure ReallocMembers; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddMember( MemberName  : PChar;
                           MemberType  : TPyMemberType;
                           MemberOffset : Integer;
                           MemberFlags : TPyMemberFlag;
                           MemberDoc : PChar );
      procedure ClearMembers;
      procedure Finalize; override;

      // properties
      property MemberCount : Integer read FMemberCount;
      property Members[ idx : Integer ] : PPyMemberDef read GetMembers;
      property MembersData : PPyMemberDef read FMembers;
{$ENDIF}
  end;

//------------------------------------------------------------
//--                                                        --
//--class: TGetSetContainer derived from TMembersContainer  --
//--                                                        --
//------------------------------------------------------------

{$IFDEF PYTHON22_OR_HIGHER}
  TGetSetArray = array[ 0 .. 16000 ] of PyGetSetDef;
  PGetSetArray = ^TGetSetArray;
{$ENDIF}

  // class TGetSetContainer
  TGetSetContainer = class(TMembersContainer)
    private
{$IFDEF PYTHON22_OR_HIGHER}
      FGetSetCount : Integer;
      FAllocatedGetSetCount : Integer;
      FGetSets : PPyGetSetDef;

      procedure AllocGetSets;
      procedure FreeGetSets;
      function  GetGetSet( idx : Integer ) : PPyGetSetDef;

    protected
      procedure ReallocGetSets; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddGetSet( AName  : PChar;
                           AGet : getter;
                           ASet : setter;
                           ADoc : PChar;
                           AClosure : Pointer);
      procedure ClearGetSets;
      procedure Finalize; override;

      // properties
      property GetSetCount : Integer read FGetSetCount;
      property GetSet[ idx : Integer ] : PPyGetSetDef read GetGetSet;
      property GetSetData : PPyGetSetDef read FGetSets;
{$ENDIF}
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TPythonModule derived from TMethodsContainer--
//--                                                   --
//-------------------------------------------------------

  TPythonModule = class; // forward declaration
  TErrors = class; // forward declaration

  TErrorType = (etString, etClass);

  TParentClassError = class(TPersistent)
    protected
      FName : String;
      FModule : String;
    public
      procedure AssignTo( Dest: TPersistent ); override;
    published
      property Module : String read FModule write FModule;
      property Name : String read FName write FName;
  end;

  TError = class(TCollectionItem)
  protected
    FName        : String;
    FText        : String;
    FError       : PPyObject;
    FErrorType   : TErrorType;
    FParentClass : TParentClassError;

    function GetDisplayName: string; {$IFNDEF FPC} override; {$ENDIF}
    procedure SetName( const Value : String );
    procedure SetText( const Value : String );
    procedure SetErrorType( Value : TErrorType );
    procedure SetParentClass( Value : TParentClassError );
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildError( const ModuleName : String );
    procedure RaiseError( const msg : String );
    procedure RaiseErrorObj( const msg : String; obj : PPyObject );
    function  Owner : TErrors;
    property Error : PPyObject read FError write FError;
  published
    property Name : String read FName write SetName;
    property Text : String read FText write SetText;
    property ErrorType : TErrorType read FErrorType write SetErrorType;
    property ParentClass : TParentClassError read FParentClass write SetParentClass;
  end;

  TErrors = class(TCollection)
  private
    FModule: TPythonModule;
    function GetError(Index: Integer): TError;
    procedure SetError(Index: Integer; Value: TError);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Module: TPythonModule);
    function  Add: TError;
    function  Owner : TPythonModule;
    property Items[Index: Integer]: TError read GetError write SetError; default;
  end;

  TPythonModule = class(TMethodsContainer)
    protected
      FModuleName : String;
      FModule : PPyObject;
      FClients : TList;
      FErrors : TErrors;
      FOnAfterInitialization : TNotifyEvent;
      FDocString : TStringList;

      function GetClientCount : Integer;
      function GetClients( idx : Integer ) : TEngineClient;
      procedure SetErrors( val : TErrors );
      procedure SetModuleName( const val : String );
      procedure SetDocString( value : TStringList );

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public methods
      procedure MakeModule;
      procedure DefineDocString;
      procedure Initialize; override;
      procedure InitializeForNewInterpreter;
      procedure AddClient( client : TEngineClient );
      function  ErrorByName( const AName : String ) : TError;
      procedure RaiseError( const error, msg : String );
      procedure RaiseErrorFmt( const error, format : String; Args : array of const );
      procedure RaiseErrorObj( const error, msg : String; obj : PPyObject );
      procedure BuildErrors;
      procedure SetVar( const varName : String; value : PPyObject );
      function  GetVar( const varName : String ) : PPyObject;
      procedure DeleteVar( const varName : String );
      procedure SetVarFromVariant( const varName : String; const value : Variant );
      function  GetVarAsVariant( const varName: String ) : Variant;

      // Public properties
      property Module : PPyObject read FModule;
      property Clients[ idx : Integer ] : TEngineClient read GetClients;
      property ClientCount : Integer read GetClientCount;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property ModuleName : String read FModuleName write SetModuleName;
      property Errors : TErrors read FErrors write SetErrors;
      property OnAfterInitialization : TNotifyEvent read FOnAfterInitialization write FOnAfterInitialization;
  end;


//-------------------------------------------------------
//--                                                   --
//--class:  TPythonType  derived from TGetSetContainer --
//--                                                   --
//-------------------------------------------------------

type
  TPythonType = class; //forward declaration

{
        A                    B                                                      C
        +-------------------++------------------------------------------------------+
        | PyObject header   ||             TPyObject class                          |
        +----------+--------++-----------------+------------+----------+------------+
        |ob_refcnt |ob_type ||hidden Class Ptr |PythonType  |IsSubType |PythonAlloc |
        |integer   |pointer ||pointer          |TPythonType |Boolean   |Boolean     |
        |4 bytes   |4 bytes ||4 bytes          |4 bytes     |1 byte    |1 byte      |
        +----------+--------++-----------------+------------+----------+------------+

        ^                    ^
        |                    |
        ptr returned         ptr returned by Adjust
        by GetSelf

        - a Python object must start at A.
        - a Delphi class class must start at B
        - TPyObject.InstanceSize will return C-B
        - Sizeof(TPyObject) will return C-B
        - The total memory allocated for a TPyObject instance will be C-A,
          even if its InstanceSize is C-B.
        - When turning a Python object pointer into a Delphi instance pointer, PythonToDelphi
          will offset the pointer from A to B.
        - When turning a Delphi instance into a Python object pointer, GetSelf will offset
          Self from B to A.
        - Properties ob_refcnt and ob_type will call GetSelf to access their data.
}
  // The base class of all new Python types
  TPyObject = class
  private
    function  Get_ob_refcnt: Integer;
    function  Get_ob_type: PPyTypeObject;
    procedure Set_ob_refcnt(const Value: Integer);
    procedure Set_ob_type(const Value: PPyTypeObject);
  public
    PythonType     : TPythonType;
    IsSubtype      : Boolean;
    PythonAlloc    : Boolean;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); virtual;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); virtual;
    destructor  Destroy; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;

    // Misc
    function  GetSelf : PPyObject;
    procedure IncRef;
    procedure Adjust(PyPointer: Pointer);
    function  GetModule : TPythonModule;

    property ob_refcnt : Integer read Get_ob_refcnt write Set_ob_refcnt;
    property ob_type   : PPyTypeObject read Get_ob_type write Set_ob_type;

    // Type services
    ////////////////

    // Basic services
    function  Print( var f: file; i: integer) : Integer; virtual;
    function  GetAttr(key : PChar) : PPyObject; virtual;
    function  SetAttr(key : PChar; value : PPyObject) : Integer; virtual;
    function  Repr : PPyObject; virtual;
    function  Compare( obj: PPyObject) : Integer; virtual;
    function  Hash : Integer; virtual;
    function  Str: PPyObject; virtual;
    function  GetAttrO( key: PPyObject) : PPyObject; virtual;
    function  SetAttrO( key, value: PPyObject) : Integer; virtual;
    function  Call( ob1, ob2 : PPyObject) : PPyObject; virtual;
{$IFDEF PYTHON20_OR_HIGHER}
    function  Traverse( proc: visitproc; ptr: Pointer) : integer; virtual;
{$ENDIF}
    function  Clear: integer; virtual;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; virtual;
    function  Iter : PPyObject; virtual;
    function  IterNext : PPyObject; virtual;
    function  Init( args, kwds : PPyObject ) : Integer; virtual;

    // Number services
    function  NbAdd( obj : PPyObject) : PPyObject; virtual;
    function  NbSubstract( obj : PPyObject) : PPyObject; virtual;
    function  NbMultiply( obj : PPyObject) : PPyObject; virtual;
    function  NbDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbRemainder( obj : PPyObject) : PPyObject; virtual;
    function  NbDivmod( obj : PPyObject) : PPyObject; virtual;
    function  NbPower( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  NbNegative : PPyObject; virtual;
    function  NbPositive : PPyObject; virtual;
    function  NbAbsolute : PPyObject; virtual;
    function  NbNonZero : Integer; virtual;
    function  NbInvert : PPyObject; virtual;
    function  NbLShift( obj : PPyObject) : PPyObject; virtual;
    function  NbRShift( obj : PPyObject) : PPyObject; virtual;
    function  NbAnd( obj : PPyObject) : PPyObject; virtual;
    function  NbXor( obj : PPyObject) : PPyObject; virtual;
    function  NbOr( obj : PPyObject) : PPyObject; virtual;
    function  NbCoerce( obj : PPPyObject) : Integer; virtual;
    function  NbInt : PPyObject; virtual;
    function  NbLong : PPyObject; virtual;
    function  NbFloat : PPyObject; virtual;
    function  NbOct : PPyObject; virtual;
    function  NbHex : PPyObject; virtual;
    function  NbInplaceAdd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceSubtract( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceMultiply( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceDivide( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceRemainder( obj : PPyObject): PPyObject; virtual;
    function  NbInplacePower( ob1, ob2 : PPyObject): PPyObject; virtual;
    function  NbInplaceLshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceRshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceAnd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceXor( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceOr( obj : PPyObject): PPyObject; virtual;
    // Sequence services
    function  SqLength : Integer; virtual;
    function  SqConcat( obj : PPyObject) : PPyObject; virtual;
    function  SqRepeat( val : Integer ) : PPyObject; virtual;
    function  SqItem( idx : Integer ) : PPyObject; virtual;
    function  SqSlice( idx1, idx2 : Integer ) : PPyObject; virtual;
    function  SqAssItem( idx : integer; obj : PPyObject) : Integer; virtual;
    function  SqAssSlice( idx1, idx2 : Integer; obj : PPyObject): integer; virtual;
    function  SqContains( obj: PPyObject): integer; virtual;
    function  SqInplaceConcat( obj : PPyObject): PPyObject; virtual;
    function  SqInplaceRepeat( i: integer): PPyObject; virtual;
    // Mapping services
    function  MpLength : Integer; virtual;
    function  MpSubscript( obj : PPyObject) : PPyObject; virtual;
    function  MpAssSubscript( obj1, obj2 : PPyObject) : Integer; virtual;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); virtual;
    class procedure RegisterMembers( APythonType : TPythonType ); virtual;
    class procedure RegisterGetSets( APythonType : TPythonType ); virtual;
    class procedure SetupType( APythonType : TPythonType ); virtual;
  end;
  TPyObjectClass = class of TPyObject;

  TBasicServices     = set of (bsPrint, bsGetAttr, bsSetAttr,
                               bsRepr, bsCompare, bsHash,
                               bsStr, bsGetAttrO, bsSetAttrO,
                               bsCall,
                               // since version 2.0
                               bsTraverse, bsClear,
                               // since version 2.1
                               bsRichCompare,
                               // since version 2.2
                               bsIter, bsIterNext);
  TNumberServices    = set of (nsAdd, nsSubstract, nsMultiply,
                               nsDivide, nsRemainder, nsDivmod,
                               nsPower, nsNegative, nsPositive,
                               nsAbsolute, nsNonZero, nsInvert,
                               nsLShift, nsRShift, nsAnd,
                               nsXor, nsOr, nsCoerce,
                               nsInt, nsLong, nsFloat,
                               nsOct, nsHex,
                               // since version 2.2
                               nsFloorDivide, nsTrueDivide);

  // TInplaceNumberServices exists since version 2.0
  TInplaceNumberServices = set of (nsInplaceAdd, nsInplaceSubtract,
                                   nsInplaceMultiply, nsInplaceDivide,
                                   nsInplaceRemainder, nsInplacePower,
                                   nsInplaceLShift, nsInplaceRShift,
                                   nsInplaceAnd, nsInplaceXor, nsInplaceOr,
                                   // since version 2.2
                                   nsInplaceFloorDivide, nsInplaceTrueDivide);

  TSequenceServices  = set of (ssLength, ssConcat, ssRepeat,
                               ssItem, ssSlice, ssAssItem,
                               ssAssSlice,
                               // since version 2.0
                               ssContains,
                               ssInplaceConcat,
                               ssInplaceRepeat
                               );

  TMappingServices   = set of (msLength, msSubscript, msAssSubscript);

  TTypeServices = class(TPersistent)
    protected
      FBasic          : TBasicServices;
      FNumber         : TNumberServices;
      FSequence       : TSequenceServices;
      FMapping        : TMappingServices;
      FInplaceNumber  : TInplaceNumberServices;

    public
      constructor Create;
      procedure AssignTo( Dest: TPersistent ); override;

    published
      property Basic : TBasicServices read FBasic write FBasic;
      property InplaceNumber : TInplaceNumberServices read FInplaceNumber Write FInplaceNumber;
      property Number : TNumberServices read FNumber write FNumber;
      property Sequence : TSequenceServices read FSequence write FSequence;
      property Mapping : TMappingServices read FMapping write FMapping;
  end;

  // The component that initializes the Python type and
  // that creates instances of itself.
  TPythonType = class(TGetSetContainer)
    protected
      FType : PyTypeObject;
      FTypeName : String;
      FModule : TPythonModule;
      FPyObjectClass : TPyObjectClass;
      FPrefix : String;
      FCreateFuncName : String;
      FServices : TTypeServices;
      FNumber:   PyNumberMethods;
      FSequence: PySequenceMethods;
      FMapping:  PyMappingMethods;
      FCurrentDocString: String;
      FDocString: TStringList;
      FCreateFuncDoc : String;
      FInstanceCount : Integer;
      FCreateHits : Integer;
      FDeleteHits : Integer;
      FTypeFlags : TPFlags;
      FCreateFunc : PPyObject;
      FCreateFuncDef : PyMethodDef;
      FGenerateCreateFunction: Boolean;

      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      function  GetTypePtr : PPyTypeObject;
      procedure SetPyObjectClass( val : TPyObjectClass );
      procedure SetModule( val : TPythonModule );
      procedure SetServices( val : TTypeServices );
      procedure SetTypeName( const val : String );
      function  CreateMethod( pSelf, args : PPyObject ) : PPyObject; cdecl;
      procedure InitServices;
      procedure SetDocString( value : TStringList );
      function  TypeFlagsAsInt : LongInt;
      function  GetMembersStartOffset : Integer; override;
      procedure ModuleReady(Sender : TObject); override;
      procedure ReallocMethods; override;
      procedure ReallocMembers; override;
      procedure ReallocGetSets; override;

      // Type services
      // They will be all forwarded to the Delphi class that
      // implements the object through the use of virtual
      // methods
      ///////////////////////////////////////
      function  NewSubtypeInst( aType: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;

    public
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      procedure Initialize; override;
      procedure Finalize; override;
      function  CreateInstance : PPyObject;
      function  CreateInstanceWith( args : PPyObject ) : PPyObject;
      procedure AddTypeVar;

      property TheType : PyTypeObject read FType write FType;
      property TheTypePtr : PPyTypeObject read GetTypePtr;
      property PyObjectClass : TPyObjectClass read FPyObjectClass write SetPyObjectClass stored False;
      property InstanceCount : Integer read FInstanceCount;
      property CreateHits : Integer read FCreateHits;
      property DeleteHits : Integer read FDeleteHits;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property TypeName : String read FTypeName write SetTypeName;
      property TypeFlags : TPFlags read FTypeFlags write FTypeFlags default TPFLAGS_DEFAULT;
      property Prefix : String read FPrefix write FPrefix;
      property Module : TPythonModule read FModule write SetModule;
      property Services : TTypeServices read FServices write SetServices;
      property GenerateCreateFunction : Boolean read fGenerateCreateFunction write fGenerateCreateFunction default True;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class: TPythonVar derived from TEngineClient     --
//--                                                   --
//-------------------------------------------------------

  TGetDataEvent = procedure ( Sender : TObject; var Data : Variant ) of Object;
  TSetDataEvent = procedure ( Sender : TObject; Data : Variant ) of Object;
  TExtGetDataEvent = procedure ( Sender : TObject; var Data : PPyObject ) of Object;
  TExtSetDataEvent = procedure ( Sender : TObject; Data : PPyObject) of Object;

  TPythonDelphiVar = class( TEngineClient )
    protected
      FModule    : String;
      FVarName   : String;
      FVarObject : PPyObject;
      FOnGetData : TGetDataEvent;
      FOnSetData : TSetDataEvent;
      FOnExtGetData : TExtGetDataEvent;
      FOnExtSetData : TExtSetDataEvent;
      FOnChange  : TNotifyEvent;

      procedure CreateVarType;
      procedure CreateVar;
      function  GetValue : Variant;
      procedure SetValue( const val : Variant );
      function  GetValueAsPyObject : PPyObject;
      procedure SetValueFromPyObject( val : PPyObject );
      function  GetValueAsString : String;
      procedure SetVarName( const val : String );

    public
      // Constructors & Destructors
      constructor Create( AOwner : TComponent ); override;

      // Public methods
      procedure Initialize; override;
      procedure Finalize; override;
      function  IsVariantOk( const v : Variant ) : Boolean;

      // Public properties
      property Value : Variant read GetValue write SetValue;
      // Warning: ValueObject returns a preincremented object !
      property ValueObject : PPyObject read GetValueAsPyObject write SetValueFromPyObject;
      property ValueAsString : String read GetValueAsString;
      property VarObject : PPyObject read FVarObject write FVarObject;

    published
      property Module    : String read FModule write FModule;
      property VarName   : String read FVarName write SetVarName;
      property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
      property OnSetData : TSetDataEvent read FOnSetData write FOnSetData;
      property OnExtGetData : TExtGetDataEvent read FOnExtGetData write FOnExtGetData;
      property OnExtSetData : TExtSetDataEvent read FOnExtSetData write FOnExtSetData;
      property OnChange  : TNotifyEvent read FOnChange write FOnChange;
  end;

  TPyVar = class(TPyObject)
    dv_var         : Variant;
    dv_component   : TPythonDelphiVar;
    dv_object      : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor  Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PChar) : PPyObject; override;
    function  SetAttr(key : PChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); override;

    // Methods of TPyVar
    function GetValue : PPyObject;
    function GetValueAsVariant : Variant;
    procedure SetValue( value : PPyObject );
    procedure SetValueFromVariant( const value : Variant );

    // Interface methods
  end;

//#######################################################
//##                                                   ##
//##  Thread Object with Python interpreter lock       ##
//##                                                   ##
//#######################################################
  TThreadExecMode = (emNewState, emNewInterpreter);

{$HINTS OFF}
  TPythonThread = class(TThread)
  private
    f_savethreadstate: PPyThreadState;
    fInterpreterState: PPyInterpreterState;
    fThreadState:      PPyThreadState;
    fThreadExecMode:   TThreadExecMode;

// Do not overwrite Execute! Use ExecuteWithPython instead!
    procedure Execute; override;
  protected
    procedure ExecuteWithPython; virtual; abstract;

    procedure Py_Begin_Allow_Threads;
    procedure Py_End_Allow_Threads;
// The following procedures are redundant and only for
// compatibility to the C API documentation.
    procedure Py_Begin_Block_Threads;
    procedure Py_Begin_Unblock_Threads;

  public
    property InterpreterState: PPyInterpreterState read  fInterpreterState
                                                   write fInterpreterState
                                                   default nil;
    property ThreadState: PPyThreadState read  fThreadState
                                         write fThreadState;
    property ThreadExecMode: TThreadExecMode read fThreadExecMode;
  end;
{$HINTS ON}

//#######################################################
//##                                                   ##
//##        New Python objects                         ##
//##                                                   ##
//#######################################################

//#######################################################
//##                                                   ##
//##    Methods for new Python objects or modules      ##
//##                                                   ##
//#######################################################

// Module pyio for Python Input/Outputs
function  pyio_write(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_read(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetDelayWrites(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetMaxLines(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_GetTypesStats(self, args : PPyObject) : PPyObject; cdecl;


//#######################################################
//##                                                   ##
//##        Global procedures                          ##
//##                                                   ##
//#######################################################

function  GetPythonEngine : TPythonEngine;
function  PythonOK : Boolean;
function  PythonToDelphi( obj : PPyObject ) : TPyObject;
function  IsDelphiObject( obj : PPyObject ) : Boolean;
procedure PyObjectDestructor( pSelf : PPyObject); cdecl;
procedure FreeSubtypeInst(ob:PPyObject); cdecl;
procedure Register;
{$IFDEF PYTHON20_OR_HIGHER}
function  PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;
{$ENDIF}

//#######################################################
//##                                                   ##
//##        Global variables                           ##
//##                                                   ##
//#######################################################


implementation

{$IFDEF MSWINDOWS}
uses Registry;
{$ENDIF}


(*******************************************************)
(**                                                   **)
(**            Globals                                **)
(**                                                   **)
(*******************************************************)

var
  gPythonEngine : TPythonEngine;
  gVarType : TPythonType;

(*******************************************************)
(**                                                   **)
(**            class TCriticalSection                 **)
(**                                                   **)
(*******************************************************)
{$IFNDEF HAS_SYNCOBJS_UNIT}
  {$IFDEF MSWINDOWS}
  // Note that this default implementation is needed to compile under Delphi3.
constructor TCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FSection);
end;

destructor TCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TCriticalSection.Acquire;
begin
  EnterCriticalSection(FSection);
end;

procedure TCriticalSection.Release;
begin
  LeaveCriticalSection(FSection);
end;

procedure TCriticalSection.Enter;
begin
  Acquire;
end;

procedure TCriticalSection.Leave;
begin
  Release;
end;
  {$ENDIF}
{$ENDIF}

(*******************************************************)
(**                                                   **)
(**            class TPythonInputOutput               **)
(**                                                   **)
(*******************************************************)

constructor TPythonInputOutput.Create( AOwner : TComponent );
begin
  inherited;
  FMaxLines      := kMaxLines;
  FQueue         := TIOStringList.Create;
  FDelayWrites   := False;
  FMaxLineLength := kMaxLineLength;
  FLinesPerThread:= TIOStringList.Create;
  FLock          := TCriticalSection.Create;
end;

destructor TPythonInputOutput.Destroy;
begin
  FLinesPerThread.Free;
  FQueue.Free;
  FLock.Free;
  inherited;
end;

procedure TPythonInputOutput.Lock;
begin
  FLock.Enter;
end;

procedure TPythonInputOutput.Unlock;
begin
  FLock.Leave;
end;

procedure TPythonInputOutput.Write( const str : IOString );

  procedure DropLine;
  begin
{$IFDEF MSWINDOWS}
    if DelayWrites then
      AddWrite( FLine_Buffer )
    else
{$ENDIF}
{$IFDEF UNICODE_SUPPORT}
      if UnicodeIO then
        SendUniData( FLine_Buffer )
      else
{$ENDIF}
        SendData( FLine_Buffer );
    FLine_Buffer := '';
    UpdateCurrentThreadLine;
  end;

var
  i : Integer;
  c : IOChar;
begin
  Lock;
  try
    FLine_Buffer := GetCurrentThreadLine;
{$IFDEF UNICODE_SUPPORT}
    if FRawOutput then begin
      FLine_Buffer := FLine_Buffer  + str;
      DropLine;
    end else begin
{$ENDIF}
      for i := 1 to length(str) do
        begin
          c := str[i];
          if c = #10 then
            DropLine
          else if (c >= ' ') or (c = #09) then
            begin
              Insert( c, FLine_Buffer, length(FLine_Buffer)+1 );
              if Length(FLine_Buffer) > MaxLineLength then
                DropLine;
            end;
        end;
{$IFDEF UNICODE_SUPPORT}
    end;
{$ENDIF}
    UpdateCurrentThreadLine;
  finally
    Unlock;
  end;
end;

procedure TPythonInputOutput.WriteLine( const str : IOString );
begin
  Write( str+#10 );
end;

procedure TPythonInputOutput.AddWrite( const str : IOString );
begin
  FQueue.Add( str );
  if FQueue.Count > FMaxLines then
    FQueue.Delete(0)
  else
    AddPendingWrite;
end;

procedure TPythonInputOutput.SendData( const Data : String );
begin
  if Assigned(FOnSendData) then
    FOnSendData( Self, Data );
end;

{$IFDEF UNICODE_SUPPORT}
procedure TPythonInputOutput.SendUniData(const Data: WideString);
begin
  if Assigned(FOnSendUniData) then
    FOnSendUniData( Self, Data );
end;
{$ENDIF}

function  TPythonInputOutput.ReceiveData : String;
begin
  Result := '';
  if Assigned(FOnReceiveData) then
    FOnReceiveData( Self, Result );
end;

{$IFDEF UNICODE_SUPPORT}
function TPythonInputOutput.ReceiveUniData: WideString;
begin
  Result := '';
  if Assigned(FOnReceiveUniData) then
    FOnReceiveUniData( Self, Result );
end;
{$ENDIF}

procedure TPythonInputOutput.AddPendingWrite;
begin
end;

function  TPythonInputOutput.GetCurrentThreadSlotIdx : Integer;
var
  thread_id : Longint;
  i : Integer;
begin
  thread_id := GetCurrentThreadId;
  for i := 0 to FLinesPerThread.Count-1 do
    if Longint(FLinesPerThread.Objects[i]) = thread_id then
      begin
        Result := i;
        Exit;
      end;
  Result := FLinesPerThread.AddObject( '', TObject(thread_id) );
end;

function  TPythonInputOutput.GetCurrentThreadLine : IOString;
begin
  Result := FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ];
end;

procedure TPythonInputOutput.UpdateCurrentThreadLine;
begin
  FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ] := FLine_Buffer;
end;

(*******************************************************)
(**                                                   **)
(**            class TDynamicDll                      **)
(**                                                   **)
(*******************************************************)

procedure TDynamicDll.DoOpenDll(const aDllName : String);
begin
  if not IsHandleValid then
  begin
    FDllName := aDllName;
    FDLLHandle := SafeLoadLibrary(
      {$IFDEF FPC}
        PChar(AnsiString(GetDllPath+DllName))
      {$ELSE}
        PChar(GetDllPath+DllName)
      {$ENDIF}
    );
  end;
end;

function  TDynamicDll.GetDllPath : String;
{$IFDEF MSWINDOWS}
var
  key : String;
  AllUserInstall : Boolean;
{$ENDIF}
begin
  Result := DllPath;

  {$IFDEF MSWINDOWS}

  // Python provides for All user and Current user installations
  // All User installations place the Python DLL in the Windows System directory
  // and write registry info to HKEY_LOCAL_MACHINE
  // Current User installations place the DLL in the install path and
  // the registry info in HKEY_CURRENT_USER.
  // Hence, for Current user installations we need to try and find the install path
  // since it may not be on the system path.

  if DLLPath = '' then begin
    AllUserInstall := False;
    try
      key := Format('\Software\Python\PythonCore\%s\InstallPath', [RegVersion]);
      with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if KeyExists( key ) then
            AllUserInstall := True;
        finally
          Free;
        end;
    except
      // under WinNT, with a user without admin rights, the access to the
      // LocalMachine keys would raise an exception.
    end;
    // We do not seem to have an All User Python Installation.
    // Check whether we have a current user installation
    if not AllUserInstall then
      with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
        try
          RootKey := HKEY_CURRENT_USER;
          if OpenKey(Key, False) then
            Result := ReadString('');
        finally
          Free;
        end;
  end;
{$ENDIF}

  if Result <> '' then
  begin
{$IFDEF DELPHI6_OR_HIGHER}
    Result := IncludeTrailingPathDelimiter(Result);
{$ELSE}
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
{$ENDIF}
  end;
end;

procedure  TDynamicDll.OpenDll(const aDllName : String);
var
  s : String;
begin
  UnloadDll;

  BeforeLoad;

  FDLLHandle := 0;

  DoOpenDll(aDllName);

  if not IsHandleValid then begin
{$IFDEF MSWINDOWS}
    s := Format('Error %d: Could not open Dll "%s"',[GetLastError, DllName]);
{$ENDIF}
{$IFDEF LINUX}
    s := Format('Error: Could not open Dll "%s"',[DllName]);
{$ENDIF}
    if FatalMsgDlg then
{$IFDEF MSWINDOWS}
      MessageBox( GetActiveWindow, PChar(s), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ENDIF}
{$IFDEF LINUX}
      WriteLn(ErrOutput, s);
{$ENDIF}

    if FatalAbort then
      Quit;
  end else
    AfterLoad;
end;

constructor TDynamicDll.Create(AOwner: TComponent);
begin
  inherited;
  FFatalMsgDlg          := True;
  FFatalAbort           := True;
  FAutoLoad             := True;
  FUseLastKnownVersion  := True;
end;

destructor TDynamicDll.Destroy;
begin
  if AutoUnload then
    UnloadDll;
  inherited;
end;

function TDynamicDll.Import(const funcname: String; canFail : Boolean = True): Pointer;
var
  E : EDllImportError;
begin
  Result := GetProcAddress( FDLLHandle, PChar(funcname) );
  if (Result = nil) and canFail then begin
    E := EDllImportError.CreateFmt('Error %d: could not map symbol "%s"', [GetLastError, funcname]);
    E.ErrorCode := GetLastError;
    E.WrongFunc := funcname;
    raise E;
  end;
end;

procedure TDynamicDll.Loaded;
begin
  inherited;
  if AutoLoad and not (csDesigning in ComponentState) then
    LoadDll;
end;

function  TDynamicDll.IsHandleValid : Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FDLLHandle >= 32);
{$ENDIF}
{$IFDEF LINUX}
  Result := FDLLHandle <> 0;
{$ENDIF}
end;

procedure TDynamicDll.LoadDll;
begin
  OpenDll( DllName );
end;

procedure TDynamicDll.UnloadDll;
begin
  if IsHandleValid then begin
    BeforeUnload;
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
  end;
end;

procedure TDynamicDll.BeforeLoad;
begin
  if Assigned( FOnBeforeLoad ) then
    FOnBeforeLoad( Self );
end;

procedure TDynamicDll.AfterLoad;
begin
  if Assigned( FOnAfterLoad ) then
    FOnAfterLoad( Self );
end;

procedure TDynamicDll.BeforeUnload;
begin
  if Assigned( FOnBeforeUnload ) then
    FOnBeforeUnload( Self );
end;

function  TDynamicDll.GetQuitMessage : String;
begin
  Result := Format( 'Dll %s could not be loaded. We must quit.', [DllName]);
end;

procedure TDynamicDll.Quit;
begin
  if not( csDesigning in ComponentState ) then begin
{$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(GetQuitMessage), 'Error', MB_TASKMODAL or MB_ICONSTOP );
    ExitProcess( 1 );
{$ENDIF}
{$IFDEF LINUX}
    WriteLn(ErrOutput, GetQuitMessage);
    __exit( 1 );
{$ENDIF}
  end;
end;

function TDynamicDll.IsAPIVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsDllNameStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsRegVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

procedure TDynamicDll.SetDllName(const Value: String);
begin
  FDllName := Value;
end;


(*******************************************************)
(**                                                   **)
(**            class TPythonInterface                 **)
(**                                                   **)
(*******************************************************)

constructor TPythonInterface.Create(AOwner: TComponent);
var
  i : Integer;
begin
  inherited;
  i := COMPILED_FOR_PYTHON_VERSION_INDEX;
  DllName     := PYTHON_KNOWN_VERSIONS[i].DllName;
  FAPIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
  FRegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
  FAutoUnload := True;
end;

procedure TPythonInterface.AfterLoad;
begin
  inherited;
  FIsPython3000 := Pos('PYTHON3', UpperCase(DLLName)) = 1;
  if FIsPython3000 then
    FBuiltInModuleName := 'builtins'
  else
    FBuiltInModuleName := '__builtin__';

  try
    MapDll;
  except
    on E: Exception do begin
      if FatalMsgDlg then
{$IFDEF MSWINDOWS}
        MessageBox( GetActiveWindow, PChar(E.Message), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ENDIF}
{$IFDEF LINUX}
        WriteLn( ErrOutput, E.Message );
{$ENDIF}
      if FatalAbort then Quit;
    end;
  end;
end;

function  TPythonInterface.GetQuitMessage : String;
begin
  Result := Format( 'Python could not be properly initialized. We must quit.', [DllName]);
end;

function TPythonInterface.GetInitialized: Boolean;
begin
  if Assigned(Py_IsInitialized) then
    Result := Py_IsInitialized{$IFDEF FPC}(){$ENDIF} <> 0
  else
    Result := FInitialized;
end;

procedure TPythonInterface.CheckPython;
begin
  if not Initialized then
    raise Exception.Create('Python is not properly initialized' );
end;

function  TPythonInterface.GetUnicodeTypeSuffix : String;
begin
  if APIVersion >= 1011 then
    Result := 'UCS2'
  else
    Result := '';
end;

procedure TPythonInterface.MapDll;
begin
  Py_DebugFlag               := Import('Py_DebugFlag');
  Py_VerboseFlag             := Import('Py_VerboseFlag');
  Py_InteractiveFlag         := Import('Py_InteractiveFlag');
  Py_OptimizeFlag            := Import('Py_OptimizeFlag');
  Py_NoSiteFlag              := Import('Py_NoSiteFlag');
  Py_UseClassExceptionsFlag  := Import('Py_UseClassExceptionsFlag');
  Py_FrozenFlag              := Import('Py_FrozenFlag');
{$IFDEF PYTHON20_OR_HIGHER}
  if not IsPython3000 then begin
    Py_TabcheckFlag            := Import('Py_TabcheckFlag');
    Py_UnicodeFlag             := Import('Py_UnicodeFlag');
  end;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  Py_IgnoreEnvironmentFlag   := Import('Py_IgnoreEnvironmentFlag');
  Py_DivisionWarningFlag     := Import('Py_DivisionWarningFlag');
{$ENDIF}

  //_PySys_TraceFunc           := Import('_PySys_TraceFunc');
  //_PySys_ProfileFunc         := Import('_PySys_ProfileFunc');

  Py_None                    := Import('_Py_NoneStruct');
  Py_Ellipsis                := Import('_Py_EllipsisObject');
  if IsPython3000 then
    Py_False                   := Import('_Py_FalseStruct')
  else
    Py_False                   := Import('_Py_ZeroStruct');
  Py_True                    := Import('_Py_TrueStruct');
{$IFDEF PYTHON21_OR_HIGHER}
  Py_NotImplemented          := Import('_Py_NotImplementedStruct');
{$ENDIF}

  PyImport_FrozenModules     := Import('PyImport_FrozenModules');

  PyExc_AttributeError       := Import('PyExc_AttributeError');
  PyExc_EOFError             := Import('PyExc_EOFError');
  PyExc_IOError              := Import('PyExc_IOError');
  PyExc_ImportError          := Import('PyExc_ImportError');
  PyExc_IndexError           := Import('PyExc_IndexError');
  PyExc_KeyError             := Import('PyExc_KeyError');
  PyExc_KeyboardInterrupt    := Import('PyExc_KeyboardInterrupt');
  PyExc_MemoryError          := Import('PyExc_MemoryError');
  PyExc_NameError            := Import('PyExc_NameError');
  PyExc_OverflowError        := Import('PyExc_OverflowError');
  PyExc_RuntimeError         := Import('PyExc_RuntimeError');
  PyExc_SyntaxError          := Import('PyExc_SyntaxError');
  PyExc_SystemError          := Import('PyExc_SystemError');
  PyExc_SystemExit           := Import('PyExc_SystemExit');
  PyExc_TypeError            := Import('PyExc_TypeError');
  PyExc_ValueError           := Import('PyExc_ValueError');
  PyExc_ZeroDivisionError    := Import('PyExc_ZeroDivisionError');
  PyExc_ArithmeticError      := Import('PyExc_ArithmeticError');
  PyExc_Exception            := Import('PyExc_Exception');
  PyExc_FloatingPointError   := Import('PyExc_FloatingPointError');
  PyExc_LookupError          := Import('PyExc_LookupError');
  if not IsPython3000 then
    PyExc_StandardError        := Import('PyExc_StandardError');
{$IFDEF PYTHON20_OR_HIGHER}
  PyExc_AssertionError       := Import('PyExc_AssertionError');
  PyExc_EnvironmentError     := Import('PyExc_EnvironmentError');
  PyExc_IndentationError     := Import('PyExc_IndentationError');
  PyExc_MemoryErrorInst      := Import('PyExc_MemoryErrorInst');
  PyExc_NotImplementedError  := Import('PyExc_NotImplementedError');
  PyExc_OSError              := Import('PyExc_OSError');
  PyExc_TabError             := Import('PyExc_TabError');
  PyExc_UnboundLocalError    := Import('PyExc_UnboundLocalError');
  PyExc_UnicodeError         := Import('PyExc_UnicodeError');
  {$IFDEF MSWINDOWS}
    PyExc_WindowsError       := Import('PyExc_WindowsError');
  {$ENDIF}
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
  PyExc_Warning              := Import('PyExc_Warning');
  PyExc_DeprecationWarning   := Import('PyExc_DeprecationWarning');
  PyExc_RuntimeWarning       := Import('PyExc_RuntimeWarning');
  PyExc_SyntaxWarning        := Import('PyExc_SyntaxWarning');
  PyExc_UserWarning          := Import('PyExc_UserWarning');
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  PyExc_ReferenceError       := Import('PyExc_ReferenceError');
  PyExc_StopIteration        := Import('PyExc_StopIteration');
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
  PyExc_FutureWarning        := Import('PyExc_FutureWarning');
  PyExc_PendingDeprecationWarning:= Import('PyExc_PendingDeprecationWarning');
  PyExc_UnicodeDecodeError   := Import('PyExc_UnicodeDecodeError');
  PyExc_UnicodeEncodeError   := Import('PyExc_UnicodeEncodeError');
  PyExc_UnicodeTranslateError:= Import('PyExc_UnicodeTranslateError');
{$ENDIF}
  PyType_Type                := Import('PyType_Type');
  PyCFunction_Type           := Import('PyCFunction_Type');
  PyCObject_Type             := Import('PyCObject_Type');
  if not IsPython3000 then
    PyClass_Type               := Import('PyClass_Type');
  PyCode_Type                := Import('PyCode_Type');
  PyComplex_Type             := Import('PyComplex_Type');
  PyDict_Type                := Import('PyDict_Type');
  if not IsPython3000 then
    PyFile_Type                := Import('PyFile_Type');
  PyFloat_Type               := Import('PyFloat_Type');
  PyFrame_Type               := Import('PyFrame_Type');
  PyFunction_Type            := Import('PyFunction_Type');
  if not IsPython3000 then
    PyInstance_Type            := Import('PyInstance_Type');
  if not IsPython3000 then
    PyInt_Type                 := Import('PyInt_Type');
  PyList_Type                := Import('PyList_Type');
  PyLong_Type                := Import('PyLong_Type');
  PyMethod_Type              := Import('PyMethod_Type');
  PyModule_Type              := Import('PyModule_Type');
  PyObject_Type              := Import('PyObject_Type');
  PyRange_Type               := Import('PyRange_Type');
  PySlice_Type               := Import('PySlice_Type');
  if not IsPython3000 then
    PyString_Type              := Import('PyString_Type')
  else
    PyString_Type              := Import('PyBytes_Type');
  PyTuple_Type               := Import('PyTuple_Type');
{$IFDEF UNICODE_SUPPORT}
  PyUnicode_Type             := Import('PyUnicode_Type');
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  PyBaseObject_Type          := Import('PyBaseObject_Type');
  if not IsPython3000 then
    PyBuffer_Type              := Import('PyBuffer_Type');
  PyCallIter_Type            := Import('PyCallIter_Type');
  PyCell_Type                := Import('PyCell_Type');
  PyClassMethod_Type         := Import('PyClassMethod_Type');
  PyProperty_Type            := Import('PyProperty_Type');
  PySeqIter_Type             := Import('PySeqIter_Type');
  PyStaticMethod_Type        := Import('PyStaticMethod_Type');
  PySuper_Type               := Import('PySuper_Type');
  {$IFNDEF PYTHON25_OR_HIGHER}
    PySymtableEntry_Type     := Import('PySymtableEntry_Type', False);
  {$ENDIF}
  PyTraceBack_Type           := Import('PyTraceBack_Type');
  PyWrapperDescr_Type        := Import('PyWrapperDescr_Type');
  _PyWeakref_RefType         := Import('_PyWeakref_RefType');
  _PyWeakref_ProxyType       := Import('_PyWeakref_ProxyType');
  _PyWeakref_CallableProxyType:=Import('_PyWeakref_CallableProxyType');
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
  if not IsPython3000 then
    PyBaseString_Type          := Import('PyBaseString_Type');
  PyBool_Type                := Import('PyBool_Type');
  PyEnum_Type                := Import('PyEnum_Type');
{$ENDIF}

  //@PyArg_GetObject           := Import('PyArg_GetObject');
  //@PyArg_GetLong             := Import('PyArg_GetLong');
  //@PyArg_GetShort            := Import('PyArg_GetShort');
  //@PyArg_GetFloat            := Import('PyArg_GetFloat');
  //@PyArg_GetString           := Import('PyArg_GetString');
  //@PyArgs_VaParse            := Import('PyArgs_VaParse');
  //@Py_VaBuildValue           := Import('Py_VaBuildValue');
  //@PyBuiltin_Init            := Import('PyBuiltin_Init');
  @PyComplex_FromCComplex    := Import('PyComplex_FromCComplex');
  @PyComplex_FromDoubles     := Import('PyComplex_FromDoubles');
  @PyComplex_RealAsDouble    := Import('PyComplex_RealAsDouble');
  @PyComplex_ImagAsDouble    := Import('PyComplex_ImagAsDouble');
  @PyComplex_AsCComplex      := Import('PyComplex_AsCComplex');
  @PyCFunction_GetFunction   := Import('PyCFunction_GetFunction');
  @PyCFunction_GetSelf       := Import('PyCFunction_GetSelf');
  @PyCallable_Check          := Import('PyCallable_Check');
  @PyCObject_FromVoidPtr     := Import('PyCObject_FromVoidPtr');
  @PyCObject_AsVoidPtr       := Import('PyCObject_AsVoidPtr');
  if not IsPython3000 then
    @PyClass_New               := Import('PyClass_New');
  if not IsPython3000 then
    @PyClass_IsSubclass        := Import('PyClass_IsSubclass');
  @PyDict_GetItem            := Import('PyDict_GetItem');
  @PyDict_SetItem            := Import('PyDict_SetItem');
  @PyDict_DelItem            := Import('PyDict_DelItem');
  @PyDict_Clear              := Import('PyDict_Clear');
  @PyDict_Next               := Import('PyDict_Next');
  @PyDict_Keys               := Import('PyDict_Keys');
  @PyDict_Values             := Import('PyDict_Values');
  @PyDict_Items              := Import('PyDict_Items');
  @PyDict_Size               := Import('PyDict_Size');
  @PyDict_DelItemString      := Import('PyDict_DelItemString');
{$IFDEF PYTHON22_OR_HIGHER}
  @PyDict_Copy               := Import('PyDict_Copy');
  @PyDictProxy_New           := Import('PyDictProxy_New');
{$ENDIF}
  if not IsPython3000 then
    @Py_InitModule4            := Import('Py_InitModule4')
  else
    @PyModule_Create2           := Import('PyModule_Create2');
  @PyErr_Print               := Import('PyErr_Print');
  @PyErr_SetNone             := Import('PyErr_SetNone');
  @PyErr_SetObject           := Import('PyErr_SetObject');
  @PyErr_Restore             := Import('PyErr_Restore');
  @PyErr_BadArgument         := Import('PyErr_BadArgument');
  @PyErr_NoMemory            := Import('PyErr_NoMemory');
  @PyErr_SetFromErrno        := Import('PyErr_SetFromErrno');
  @PyErr_BadInternalCall     := Import('PyErr_BadInternalCall');
  @PyErr_CheckSignals        := Import('PyErr_CheckSignals');
  @PyErr_Occurred            := Import('PyErr_Occurred');
  @PyErr_Clear               := Import('PyErr_Clear');
  @PyErr_Fetch               := Import('PyErr_Fetch');
  @PyErr_SetString           := Import('PyErr_SetString');
  @PyEval_GetBuiltins        := Import('PyEval_GetBuiltins');
  @PyImport_GetModuleDict    := Import('PyImport_GetModuleDict');
  if IsPython3000 then
    @PyInt_FromLong            := Import('PyLong_FromLong')
  else
    @PyInt_FromLong            := Import('PyInt_FromLong');
  @DLL_PyArg_ParseTuple      := Import('PyArg_ParseTuple');
  @DLL_PyArg_Parse           := Import('PyArg_Parse');
  @DLL_Py_BuildValue         := Import('Py_BuildValue');
  @Py_Initialize             := Import('Py_Initialize');
  @PyDict_New                := Import('PyDict_New');
  @PyDict_SetItemString      := Import('PyDict_SetItemString');
  @PyModule_GetDict          := Import('PyModule_GetDict');
  @PyObject_Str              := Import('PyObject_Str');
  @PyRun_String              := Import('PyRun_String');
  @PyRun_SimpleString        := Import('PyRun_SimpleString');
  @PyDict_GetItemString      := Import('PyDict_GetItemString');
  if not IsPython3000 then
    @PyString_AsString         := Import('PyString_AsString')
  else
    @PyString_AsString         := Import('PyBytes_AsString');
  if not IsPython3000 then
    @DLL_PyString_FromString   := Import('PyString_FromString');
  if not IsPython3000 then
    @PySys_SetArgv             := Import('PySys_SetArgv')
  else
    @PySys_SetArgv3000         := Import('PySys_SetArgv');
  @Py_Exit                   := Import('Py_Exit');

  @PyCFunction_New           :=Import('PyCFunction_New');
  @PyEval_CallObject         :=Import('PyEval_CallObject');
  @PyEval_CallObjectWithKeywords:=Import('PyEval_CallObjectWithKeywords');
  @PyEval_GetFrame           :=Import('PyEval_GetFrame');
  @PyEval_GetGlobals         :=Import('PyEval_GetGlobals');
  @PyEval_GetLocals          :=Import('PyEval_GetLocals');
  //@PyEval_GetOwner           :=Import('PyEval_GetOwner');
  if not IsPython3000 then
    @PyEval_GetRestricted      :=Import('PyEval_GetRestricted');
  @PyEval_InitThreads        :=Import('PyEval_InitThreads');
  @PyEval_RestoreThread      :=Import('PyEval_RestoreThread');
  @PyEval_SaveThread         :=Import('PyEval_SaveThread');
  if not IsPython3000 then
    @PyFile_FromString         :=Import('PyFile_FromString');
  @PyFile_GetLine            :=Import('PyFile_GetLine');
  if not IsPython3000 then
    @PyFile_Name               :=Import('PyFile_Name');
  if not IsPython3000 then
    @PyFile_SetBufSize         :=Import('PyFile_SetBufSize');
  if not IsPython3000 then
    @PyFile_SoftSpace          :=Import('PyFile_SoftSpace');
  @PyFile_WriteObject        :=Import('PyFile_WriteObject');
  @PyFile_WriteString        :=Import('PyFile_WriteString');
  @PyFloat_AsDouble          :=Import('PyFloat_AsDouble');
  @PyFloat_FromDouble        :=Import('PyFloat_FromDouble');
  @PyFunction_GetCode        :=Import('PyFunction_GetCode');
  @PyFunction_GetGlobals     :=Import('PyFunction_GetGlobals');
  @PyFunction_New            :=Import('PyFunction_New');
  @PyImport_AddModule        :=Import('PyImport_AddModule');
  @PyImport_Cleanup          :=Import('PyImport_Cleanup');
  @PyImport_GetMagicNumber   :=Import('PyImport_GetMagicNumber');
  @PyImport_ImportFrozenModule:=Import('PyImport_ImportFrozenModule');
  @PyImport_ImportModule     :=Import('PyImport_ImportModule');
  @PyImport_Import           :=Import('PyImport_Import');
  //@PyImport_Init             :=Import('PyImport_Init');
  @PyImport_ReloadModule     :=Import('PyImport_ReloadModule');
  if not IsPython3000 then
    @PyInstance_New            :=Import('PyInstance_New');
  if IsPython3000 then
    @PyInt_AsLong              :=Import('PyLong_AsLong')
  else
    @PyInt_AsLong              :=Import('PyInt_AsLong');
  @PyList_Append             :=Import('PyList_Append');
  @PyList_AsTuple            :=Import('PyList_AsTuple');
  @PyList_GetItem            :=Import('PyList_GetItem');
  @PyList_GetSlice           :=Import('PyList_GetSlice');
  @PyList_Insert             :=Import('PyList_Insert');
  @PyList_New                :=Import('PyList_New');
  @PyList_Reverse            :=Import('PyList_Reverse');
  @PyList_SetItem            :=Import('PyList_SetItem');
  @PyList_SetSlice           :=Import('PyList_SetSlice');
  @PyList_Size               :=Import('PyList_Size');
  @PyList_Sort               :=Import('PyList_Sort');
  @PyLong_AsDouble           :=Import('PyLong_AsDouble');
  @PyLong_AsLong             :=Import('PyLong_AsLong');
  @PyLong_FromDouble         :=Import('PyLong_FromDouble');
  @PyLong_FromLong           :=Import('PyLong_FromLong');
  @PyLong_FromString         :=Import('PyLong_FromString');
  @PyLong_FromString         :=Import('PyLong_FromString');
  @PyLong_FromUnsignedLong   :=Import('PyLong_FromUnsignedLong');
  @PyLong_AsUnsignedLong     :=Import('PyLong_AsUnsignedLong');
{$IFDEF UNICODE_SUPPORT}
  @PyLong_FromUnicode        :=Import('PyLong_FromUnicode');
{$ENDIF}
{$IFDEF DELPHI6_OR_HIGHER}
  @PyLong_FromLongLong       :=Import('PyLong_FromLongLong');
  @PyLong_AsLongLong         :=Import('PyLong_AsLongLong');
{$ENDIF}
  @PyMapping_Check           :=Import('PyMapping_Check');
  @PyMapping_GetItemString   :=Import('PyMapping_GetItemString');
  @PyMapping_HasKey          :=Import('PyMapping_HasKey');
  @PyMapping_HasKeyString    :=Import('PyMapping_HasKeyString');
  @PyMapping_Length          :=Import('PyMapping_Length');
  @PyMapping_SetItemString   :=Import('PyMapping_SetItemString');
  if not IsPython3000 then
    @PyMethod_Class            :=Import('PyMethod_Class');
  @PyMethod_Function         :=Import('PyMethod_Function');
  @PyMethod_New              :=Import('PyMethod_New');
  @PyMethod_Self             :=Import('PyMethod_Self');
  @PyModule_GetName          :=Import('PyModule_GetName');
  @PyModule_New              :=Import('PyModule_New');
  @PyNumber_Absolute         :=Import('PyNumber_Absolute');
  @PyNumber_Add              :=Import('PyNumber_Add');
  @PyNumber_And              :=Import('PyNumber_And');
  @PyNumber_Check            :=Import('PyNumber_Check');
  if not IsPython3000 then
    @PyNumber_Coerce           :=Import('PyNumber_Coerce');
  if IsPython3000 then
    @PyNumber_Divide           :=Import('PyNumber_TrueDivide')
  else
    @PyNumber_Divide           :=Import('PyNumber_Divide');
{$IFDEF PYTHON22_OR_HIGHER}
  @PyNumber_FloorDivide      :=Import('PyNumber_FloorDivide');
  @PyNumber_TrueDivide       :=Import('PyNumber_TrueDivide');
{$ENDIF}
  @PyNumber_Divmod           :=Import('PyNumber_Divmod');
  @PyNumber_Float            :=Import('PyNumber_Float');
  if not IsPython3000 then
    @PyNumber_Int              :=Import('PyNumber_Int');
  @PyNumber_Invert           :=Import('PyNumber_Invert');
  @PyNumber_Long             :=Import('PyNumber_Long');
  @PyNumber_Lshift           :=Import('PyNumber_Lshift');
  @PyNumber_Multiply         :=Import('PyNumber_Multiply');
  @PyNumber_Negative         :=Import('PyNumber_Negative');
  @PyNumber_Or               :=Import('PyNumber_Or');
  @PyNumber_Positive         :=Import('PyNumber_Positive');
  @PyNumber_Power            :=Import('PyNumber_Power');
  @PyNumber_Remainder        :=Import('PyNumber_Remainder');
  @PyNumber_Rshift           :=Import('PyNumber_Rshift');
  @PyNumber_Subtract         :=Import('PyNumber_Subtract');
  @PyNumber_Xor              :=Import('PyNumber_Xor');
  @PyOS_InitInterrupts       :=Import('PyOS_InitInterrupts');
  @PyOS_InterruptOccurred    :=Import('PyOS_InterruptOccurred');
  @PyObject_CallObject       :=Import('PyObject_CallObject');
  @PyObject_CallMethodStr    :=Import('PyObject_CallMethod');
  @PyObject_Compare          :=Import('PyObject_Compare');
{$IFDEF PYTHON21_OR_HIGHER}
  @PyObject_RichCompare      :=Import('PyObject_RichCompare');
  @PyObject_RichCompareBool  :=Import('PyObject_RichCompareBool');
{$ENDIF}
  @PyObject_GetAttr          :=Import('PyObject_GetAttr');
  @PyObject_GetAttrString    :=Import('PyObject_GetAttrString');
  @PyObject_GetItem          :=Import('PyObject_GetItem');
  @PyObject_DelItem          :=Import('PyObject_DelItem');
  @PyObject_HasAttrString    :=Import('PyObject_HasAttrString');
  @PyObject_Hash             :=Import('PyObject_Hash');
  @PyObject_IsTrue           :=Import('PyObject_IsTrue');
  @PyObject_Length           :=Import('PyObject_Length');
  @PyObject_Repr             :=Import('PyObject_Repr');
  @PyObject_SetAttr          :=Import('PyObject_SetAttr');
  @PyObject_SetAttrString    :=Import('PyObject_SetAttrString');
  @PyObject_SetItem          :=Import('PyObject_SetItem');
{$IFDEF PYTHON20_OR_HIGHER}
  @PyObject_Init             :=Import('PyObject_Init');
  @PyObject_InitVar          :=Import('PyObject_InitVar');
  @PyObject_New              :=Import('_PyObject_New');
  @PyObject_NewVar           :=Import('_PyObject_NewVar');
  @PyObject_Free             :=Import('PyObject_Free');
  @PyObject_GetIter          :=Import('PyObject_GetIter');
  @PyIter_Next               :=Import('PyIter_Next');
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
  @PyObject_IsInstance       :=Import('PyObject_IsInstance');
  @PyObject_IsSubclass       :=Import('PyObject_IsSubclass');
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  @PyObject_Call             :=Import('PyObject_Call');
  @PyObject_GenericGetAttr   :=Import('PyObject_GenericGetAttr');
  @PyObject_GenericSetAttr   :=Import('PyObject_GenericSetAttr');
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
  PyObject_GC_Malloc         :=Import('_PyObject_GC_Malloc');
  PyObject_GC_New            :=Import('_PyObject_GC_New');
  PyObject_GC_NewVar         :=Import('_PyObject_GC_NewVar');
  PyObject_GC_Resize         :=Import('_PyObject_GC_Resize');
  PyObject_GC_Del            :=Import('PyObject_GC_Del');
  PyObject_GC_Track          :=Import('PyObject_GC_Track');
  PyObject_GC_UnTrack        :=Import('PyObject_GC_UnTrack');
{$ENDIF}
{$IFNDEF PYTHON25_OR_HIGHER}
  @PyRange_New               :=Import('PyRange_New', False);
{$ENDIF}
  @PySequence_Check          :=Import('PySequence_Check');
  @PySequence_Concat         :=Import('PySequence_Concat');
  @PySequence_Count          :=Import('PySequence_Count');
  @PySequence_GetItem        :=Import('PySequence_GetItem');
  @PySequence_GetSlice       :=Import('PySequence_GetSlice');
  @PySequence_In             :=Import('PySequence_In');
  @PySequence_Index          :=Import('PySequence_Index');
  @PySequence_Length         :=Import('PySequence_Length');
  @PySequence_Repeat         :=Import('PySequence_Repeat');
  @PySequence_SetItem        :=Import('PySequence_SetItem');
  @PySequence_SetSlice       :=Import('PySequence_SetSlice');
  @PySequence_DelSlice       :=Import('PySequence_DelSlice');
  @PySequence_Tuple          :=Import('PySequence_Tuple');
{$IFDEF PYTHON20_OR_HIGHER}
  @PySequence_Contains       :=Import('PySequence_Contains');
{$ENDIF}
  @PySlice_GetIndices        :=Import('PySlice_GetIndices');
  @PySeqIter_New             :=Import('PySeqIter_New');
{$IFDEF PYTHON23_OR_HIGHER}
  @PySlice_GetIndicesEx      :=Import('PySlice_GetIndicesEx');
{$ENDIF}
  @PySlice_New               :=Import('PySlice_New');
  if not IsPython3000 then begin
    @PyString_Concat           :=Import('PyString_Concat');
    @PyString_ConcatAndDel     :=Import('PyString_ConcatAndDel');
    @PyString_Format           :=Import('PyString_Format');
    @PyString_FromStringAndSize:=Import('PyString_FromStringAndSize');
    @PyString_Size             :=Import('PyString_Size');
    {$IFDEF PYTHON23_OR_HIGHER}
      @PyString_DecodeEscape     :=Import('PyString_DecodeEscape');
      @PyString_Repr             :=Import('PyString_Repr');
    {$ENDIF}
  end else begin
    @PyString_Concat           :=Import('PyBytes_Concat');
    @PyString_ConcatAndDel     :=Import('PyBytes_ConcatAndDel');
    @PyString_FromStringAndSize:=Import('PyBytes_FromStringAndSize');
    @PyString_Size             :=Import('PyBytes_Size');
    @PyString_DecodeEscape     :=Import('PyBytes_DecodeEscape');
    @PyString_Repr             :=Import('PyBytes_Repr');
  end;
  @PySys_GetObject           :=Import('PySys_GetObject');
  //@PySys_Init                :=Import('PySys_Init');
  @PySys_SetObject           :=Import('PySys_SetObject');
  @PySys_SetPath             :=Import('PySys_SetPath');
  //@PyTraceBack_Fetch         :=Import('PyTraceBack_Fetch');
  @PyTraceBack_Here          :=Import('PyTraceBack_Here');
  @PyTraceBack_Print         :=Import('PyTraceBack_Print');
  //@PyTraceBack_Store         :=Import('PyTraceBack_Store');
  @PyTuple_GetItem           :=Import('PyTuple_GetItem');
  @PyTuple_GetSlice          :=Import('PyTuple_GetSlice');
  @PyTuple_New               :=Import('PyTuple_New');
  @PyTuple_SetItem           :=Import('PyTuple_SetItem');
  @PyTuple_Size              :=Import('PyTuple_Size');
{$IFDEF PYTHON22_OR_HIGHER}
  @PyType_IsSubtype          :=Import('PyType_IsSubtype');
  @PyType_GenericAlloc       :=Import('PyType_GenericAlloc');
  @PyType_GenericNew         :=Import('PyType_GenericNew');
  @PyType_Ready              :=Import('PyType_Ready');
{$ENDIF}
{$IFDEF UNICODE_SUPPORT}
  @PyUnicode_FromWideChar    :=Import(Format('PyUnicode%s_FromWideChar',[GetUnicodeTypeSuffix]));
  @PyUnicode_AsWideChar      :=Import(Format('PyUnicode%s_AsWideChar',[GetUnicodeTypeSuffix]));
  @PyUnicode_Decode          :=Import(Format('PyUnicode%s_Decode',[GetUnicodeTypeSuffix]));
  @PyUnicode_AsEncodedString :=Import(Format('PyUnicode%s_AsEncodedString',[GetUnicodeTypeSuffix]));
{$IFDEF PYTHON23_OR_HIGHER}
  @PyUnicode_FromOrdinal     :=Import(Format('PyUnicode%s_FromOrdinal',[GetUnicodeTypeSuffix]));
{$ENDIF}
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  @PyWeakref_GetObject       :=Import('PyWeakref_GetObject');
  @PyWeakref_NewProxy        :=Import('PyWeakref_NewProxy');
  @PyWeakref_NewRef          :=Import('PyWeakref_NewRef');
  @PyWrapper_New             :=Import('PyWrapper_New');
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
  @PyBool_FromLong           :=Import('PyBool_FromLong');
  @PyThreadState_SetAsyncExc :=Import('PyThreadState_SetAsyncExc');
{$ENDIF}
  @Py_AtExit                 :=Import('Py_AtExit');
  //@Py_Cleanup                :=Import('Py_Cleanup');
  @Py_CompileString          :=Import('Py_CompileString');
  @Py_FatalError             :=Import('Py_FatalError');
  if not IsPython3000 then begin
    @Py_FindMethod             :=Import('Py_FindMethod');
    @Py_FindMethodInChain      :=Import('Py_FindMethodInChain');
    @DLL_Py_FlushLine        :=Import('Py_FlushLine');
  end;
  @_PyObject_New             :=Import('_PyObject_New');
  if not IsPython3000 then
    @_PyString_Resize          :=Import('_PyString_Resize')
  else
    @_PyString_Resize          :=Import('_PyBytes_Resize');
  @Py_Finalize                :=Import('Py_Finalize');
  if getProcAddress( FDLLHandle, 'PyCode_Addr2Line' ) <> nil then
    @DLL_PyCode_Addr2Line     := Import('PyCode_Addr2Line');
  if getProcAddress( FDLLHandle, 'PyImport_ExecCodeModule' ) <> nil then
    @DLL_PyImport_ExecCodeModule := Import('PyImport_ExecCodeModule');
  //@PyClass_IsSubclass         :=Import('PyClass_IsSubclass');
  @PyErr_ExceptionMatches     :=Import('PyErr_ExceptionMatches');
  @PyErr_GivenExceptionMatches:=Import('PyErr_GivenExceptionMatches');
  @PyEval_EvalCode            :=Import('PyEval_EvalCode');
  @Py_GetVersion              :=Import('Py_GetVersion');
  @Py_GetCopyright            :=Import('Py_GetCopyright');
  @Py_GetExecPrefix           :=Import('Py_GetExecPrefix');
  @Py_GetPath                 :=Import('Py_GetPath');
  @Py_GetPrefix               :=Import('Py_GetPrefix');
  @Py_GetProgramName          :=Import('Py_GetProgramName');
  @PyParser_SimpleParseString :=Import('PyParser_SimpleParseString');
  @PyNode_Free                :=Import('PyNode_Free');
  @PyErr_NewException         :=Import('PyErr_NewException');
{$IFDEF PYTHON20_OR_HIGHER}
/// jah 29-sep-2000 : updated for python 2.0
///                   replaced Py_Malloc with PyMem_Malloc
///---   @Py_Malloc := Import ('Py_Malloc');
///+++   @Py_Malloc := Import ('PyMem_Malloc');
   try
     @Py_Malloc := Import ('PyMem_Malloc');
     @PyMem_Malloc := Import ('PyMem_Malloc');
   except
   end;
{$ENDIF}
{$IFDEF PYTHON15}
   @Py_Malloc := Import ('Py_Malloc');
{$ENDIF}
  @PyObject_CallMethod        :=Import('PyObject_CallMethod');
  if (APIVersion < 1007) or
     (DllName = PYTHON_KNOWN_VERSIONS[1].DllName) then
  begin
    @Py_SetProgramName         := nil;
    @Py_SetProgramName3000     := nil;
    @Py_IsInitialized          := nil;
    @Py_GetProgramFullPath     := nil;
    @DLL_Py_GetBuildInfo       := nil;
    @Py_NewInterpreter         := nil;
    @Py_EndInterpreter         := nil;
    @PyEval_AcquireLock        := nil;
    @PyEval_ReleaseLock        := nil;
    @PyEval_AcquireThread      := nil;
    @PyEval_ReleaseThread      := nil;
    @PyInterpreterState_New    := nil;
    @PyInterpreterState_Clear  := nil;
    @PyInterpreterState_Delete := nil;
    @PyThreadState_New         := nil;
    @PyThreadState_Clear       := nil;
    @PyThreadState_Delete      := nil;
    @PyThreadState_Get         := nil;
    @PyThreadState_Swap        := nil;
    @PyErr_SetInterrupt        := nil; 
  end else
  begin
    if not IsPython3000 then
      @Py_SetProgramName        := Import('Py_SetProgramName')
    else
      @Py_SetProgramName3000    := Import('Py_SetProgramName');
    @Py_IsInitialized         := Import('Py_IsInitialized');
    @Py_GetProgramFullPath    := Import('Py_GetProgramFullPath');
    if getProcAddress( FDLLHandle, 'Py_GetBuildInfo' ) <> nil then
      @DLL_Py_GetBuildInfo    := Import('Py_GetBuildInfo');
    @Py_NewInterpreter        := Import('Py_NewInterpreter');
    @Py_EndInterpreter        := Import('Py_EndInterpreter');
    @PyEval_AcquireLock       := Import('PyEval_AcquireLock');
    @PyEval_ReleaseLock       := Import('PyEval_ReleaseLock');
    @PyEval_AcquireThread     := Import('PyEval_AcquireThread');
    @PyEval_ReleaseThread     := Import('PyEval_ReleaseThread');
    @PyInterpreterState_New   := Import('PyInterpreterState_New');
    @PyInterpreterState_Clear := Import('PyInterpreterState_Clear');
    @PyInterpreterState_Delete:= Import('PyInterpreterState_Delete');
    @PyThreadState_New        := Import('PyThreadState_New');
    @PyThreadState_Clear      := Import('PyThreadState_Clear');
    @PyThreadState_Delete     := Import('PyThreadState_Delete');
    @PyThreadState_Get        := Import('PyThreadState_Get');
    @PyThreadState_Swap       := Import('PyThreadState_Swap');
    @PyErr_SetInterrupt       := Import('PyErr_SetInterrupt'); 
  end;
end;

procedure TPythonInterface.Py_INCREF(op: PPyObject);
begin
  Inc(op^.ob_refcnt);
end;

procedure TPythonInterface.Py_DECREF(op: PPyObject);
begin
  with op^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(op);
    end;
  end;
end;

procedure TPythonInterface.Py_XINCREF(op: PPyObject);
begin
  if op <> nil then Py_INCREF(op);
end;

procedure TPythonInterface.Py_XDECREF(op: PPyObject);
begin
  if op <> nil then Py_DECREF(op);
end;

function TPythonInterface.Py_GetPlatform: PChar; cdecl;
begin
  Py_GetPlatform := 'win32';
end;

function TPythonInterface.Py_BuildValue( format: PChar; args: array of const):
         PPyObject; {$IFNDEF FPC} assembler; {$ENDIF} cdecl;
const tenthousand:double = 10000.0;
var temp: Double;
{$IFDEF FPC} begin {$ENDIF}
asm
        push ebx             // save ebx, four registers needed
        mov eax,[args]       // gets args pointer
        mov edx,[args+$4]    // gets invisible argument count-1 parameter
@loop1: lea ebx,[eax+edx*8]  // get argument address
        cmp byte ptr [ebx+$4], vtExtended // Is Extended?
        jz  @IsDouble
        cmp byte ptr [ebx+$4], vtCurrency // Is Currency, 64bit integer?
        jnz @NoDouble
@IsCurrency:
        mov ebx,[ebx]        // get 64 bit integer
        fild qword ptr [ebx] // put on float stack
        wait
        fdiv tenthousand     // normalize to double float
        jmp @PushDouble      // Handle as double value
@IsDouble:
        mov ebx,[ebx]        // get extended float
        fld tbyte ptr [ebx]  // put on float stack
@PushDouble:
        wait
        fstp temp            //get double from float stack
        wait
        lea ebx,temp         // get temp
        mov ecx,[ebx+$4]     // push 4 high bytes of temp
        push ecx
@NoDouble:
        mov ecx,[ebx]        // get one argument
        push ecx             // push 4 bytes of the argument
        dec edx              // go to the next argument
        jns  @loop1          // next argument, if any
        mov  eax,self        // get invisible self parameter
        mov  edx,format      // call DLL Py_Builtin function
        push edx
        call [eax+DLL_Py_BuildValue]
        mov ebx,[ebp-$c]       // get saved ebx from ebp-based offset
                               // (current esp could not be used!)
end;
{$IFDEF FPC} end; {$ENDIF}

function TPythonInterface.PyObject_CallMethodWithArgs(obj: PPyObject; method,
  format: PChar; args: array of const): PPyObject;
 {$IFNDEF FPC} assembler; {$ENDIF} cdecl;
const tenthousand:double = 10000.0;
var temp: Double;
{$IFDEF FPC} begin {$ENDIF}
asm
        push ebx             // save ebx, four registers needed
        mov eax,[args]       // gets args pointer
        mov edx,[args+$4]    // gets invisible argument count-1 parameter
@loop1: lea ebx,[eax+edx*8]  // get argument address
        cmp byte ptr [ebx+$4], vtExtended // Is Extended?
        jz  @IsDouble
        cmp byte ptr [ebx+$4], vtCurrency // Is Currency, 64bit integer?
        jnz @NoDouble
@IsCurrency:
        mov ebx,[ebx]        // get 64 bit integer
        fild qword ptr [ebx] // put on float stack
        wait
        fdiv tenthousand     // normalize to double float
        jmp @PushDouble      // Handle as double value
@IsDouble:
        mov ebx,[ebx]        // get extended float
        fld tbyte ptr [ebx]  // put on float stack
@PushDouble:
        wait
        fstp temp            //get double from float stack
        wait
        lea ebx,temp         // get temp
        mov ecx,[ebx+$4]     // push 4 high bytes of temp
        push ecx
@NoDouble:
        mov ecx,[ebx]        // get one argument
        push ecx             // push 4 bytes of the argument
        dec edx              // go to the next argument
        jns  @loop1          // next argument, if any
        mov  eax,self        // get invisible self parameter
        mov  edx,format      // call DLL Py_Builtin function
        push edx
        mov  edx,method      // call DLL Py_Builtin function
        push edx
        mov  edx,obj         // call DLL Py_Builtin function
        push edx
        call [eax+PyObject_CallMethod]
        mov ebx,[ebp-$c]       // get saved ebx from ebp-based offset
                               // (current esp could not be used!)
end;
{$IFDEF FPC} end; {$ENDIF}

{DELPHI does the right thing here. It automatically generates
 a copy of the argp on the stack}
function TPythonInterface.PyArg_Parse ( args: PPyObject; format: PChar;
                       argp: array of Pointer): Integer; cdecl;
begin
{$IFDEF DELPHI6_OR_HIGHER}
  Result := 0;
  { Do not optimize this to a "pure" assembler routine, because such
    a routine does not copy the array arguments in the prologue code }
  asm
    lea edx, format
    push [edx]

    sub edx, TYPE PChar
    push [edx]

    mov eax, Self
    mov eax, [eax].DLL_PyArg_Parse
    call eax

    pop edx
    pop edx
    mov Result, eax
  end;
{$ELSE}
  Result := DLL_PyArg_Parse( args, format );
{$ENDIF}
end;

function TPythonInterface.PyArg_ParseTuple ( args: PPyObject; format: PChar;
                            argp: array of Pointer): Integer; cdecl;
begin
{$IFDEF DELPHI6_OR_HIGHER}
  Result := 0;
  { Do not optimize this to a "pure" assembler routine, because such
    a routine does not copy the array arguments in the prologue code }
  asm
    lea edx, format
    push [edx]

    sub edx, TYPE PChar
    push [edx]

    mov eax, Self
    mov eax, [eax].DLL_PyArg_ParseTuple
    call eax

    pop edx
    pop edx
    mov Result, eax
  end;
{$ELSE}
  Result := DLL_PyArg_ParseTuple( args, format );
{$ENDIF}
end;

// This function is copied from compile.c because it was not
// exported in the Dll
function TPythonInterface.PyCode_Addr2Line( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
var
  size : Integer;
  p    : PChar;
  line : Integer;
  addr : Integer;
  cpt  : Integer;
begin
  if Assigned(DLL_PyCode_Addr2Line) then
    begin
      Result := DLL_PyCode_Addr2Line( co, addrq );
      Exit;
    end;
  size := PyString_Size(co^.co_lnotab) div 2;
  p    := PyString_AsString(co^.co_lnotab);
  line := co^.co_firstlineno;
  addr := 0;
  cpt  := 0;
  while (size-1) >= 0 do
    begin
      Dec(size);
      Inc( addr, Ord(p[cpt]) );
      Inc(cpt);
      if addr > addrq then
        Break;
      Inc( line, Ord(p[cpt]) );
      Inc(cpt);
    end;
  Result := line;
end;

function TPythonInterface.Py_GetBuildInfo : PChar; cdecl;
begin
  if Assigned(DLL_Py_GetBuildInfo) then
    begin
      Result := DLL_Py_GetBuildInfo;
      Exit;
    end;
  Result := 'No build info';
end;

function TPythonInterface.PyImport_ExecCodeModule( const AName : String; codeobject : PPyObject) : PPyObject;
var
  m, d, v, modules : PPyObject;
begin
  if Assigned(DLL_PyImport_ExecCodeModule) then
  begin
    Result := DLL_PyImport_ExecCodeModule(PChar(AName), codeobject);
    Exit;
  end;
  CheckPython;
  m := PyImport_AddModule(PChar(AName));
  if not Assigned(m) then
    begin
      Result := nil;
      Exit;
    end;
  d := PyModule_GetDict(m);
  if PyDict_GetItemString(d, '__builtins__') = nil then
    begin
      if PyDict_SetItemString(d, '__builtins__', PyEval_GetBuiltins) <> 0 then
        begin
          Result := nil;
          Exit;
        end;
    end;
  // Remember the fielname as the __file__ attribute
  if PyDict_SetItemString(d, '__file__', PPyCodeObject(codeobject)^.co_filename) <> 0 then
    PyErr_Clear; // Not important enough to report
  v := PyEval_EvalCode(PPyCodeObject(codeobject), d, d); // XXX owner ?
  if not Assigned(v) then
    begin
      Result := nil;
      Exit;
    end;
  Py_XDECREF(v);
  modules := PyImport_GetModuleDict;
  if PyDict_GetItemString(modules, PChar(AName)) = nil then
    begin
      PyErr_SetString(PyExc_ImportError^, PChar(Format('Loaded module %.200s not found in sys.modules', [AName])));
      Result := nil;
      Exit;
    end;
  Py_XINCREF(m);
  Result := m;
end;

function TPythonInterface.PyString_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyString_Type);
{$IFDEF UNICODE_SUPPORT}
  if not Result then
    Result := PyObject_TypeCheck(obj, PyUnicode_Type);
{$ENDIF}
end;

function TPythonInterface.PyString_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyString_Type));
end;

function TPythonInterface.PyFloat_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyFloat_Type);
end;

function TPythonInterface.PyFloat_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyFloat_Type));
end;

function TPythonInterface.PyInt_Check( obj : PPyObject ) : Boolean;
begin
  if IsPython3000 then
    Result := PyObject_TypeCheck(obj, PyLong_Type)
  else
    Result := PyObject_TypeCheck(obj, PyInt_Type);
end;

function TPythonInterface.PyInt_CheckExact(obj: PPyObject): Boolean;
begin
  if IsPython3000 then
    Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyLong_Type))
  else
    Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyInt_Type));
end;

function TPythonInterface.PyLong_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyLong_Type);
end;

function TPythonInterface.PyLong_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyLong_Type));
end;

function TPythonInterface.PyTuple_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyTuple_Type);
end;

function TPythonInterface.PyTuple_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyTuple_Type));
end;

function TPythonInterface.PyInstance_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (not IsPython3000) and (obj^.ob_type = PPyTypeObject(PyInstance_Type));
end;

function TPythonInterface.PyClass_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and not IsPython3000 and (obj^.ob_type = PPyTypeObject(PyClass_Type));
end;

function TPythonInterface.PyType_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyType_Type));
end;

function TPythonInterface.PyMethod_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyMethod_Type));
end;

function TPythonInterface.PyList_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyList_Type);
end;

function TPythonInterface.PyList_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyList_Type));
end;

function TPythonInterface.PyDict_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyDict_Type);
end;

function TPythonInterface.PyDict_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyDict_Type));
end;

function TPythonInterface.PyModule_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyModule_Type);
end;

function TPythonInterface.PyModule_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyModule_Type));
end;

function TPythonInterface.PySlice_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PySlice_Type));
end;

function TPythonInterface.PyFunction_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and
    ((obj^.ob_type = PPyTypeObject(PyCFunction_Type)) or
     (obj^.ob_type = PPyTypeObject(PyFunction_Type)));
end;

function TPythonInterface.PyIter_Check( obj : PPyObject ) : Boolean;
begin
{$IFDEF PYTHON20_OR_HIGHER}
  Result := Assigned( obj ) and
            (IsPython3000 or (PyType_HasFeature(obj^.ob_type, Py_TPFLAGS_HAVE_ITER))
              and Assigned(obj^.ob_type^.tp_iternext));
{$ELSE}
  Result := False;
{$ENDIF}
end;

{$IFDEF UNICODE_SUPPORT}
function TPythonInterface.PyUnicode_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyUnicode_Type);
end;

function TPythonInterface.PyUnicode_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyUnicode_Type));
end;
{$ENDIF}

{$IFDEF PYTHON22_OR_HIGHER}
function TPythonInterface.PyType_IS_GC(t : PPyTypeObject ) : Boolean;
begin
  Result := PyType_HasFeature(t, Py_TPFLAGS_HAVE_GC);
end;

function TPythonInterface.PyObject_IS_GC( obj : PPyObject ) : Boolean;
begin
  Result := PyType_IS_GC(obj^.ob_type) and
            (not Assigned(obj^.ob_type^.tp_is_gc) or (obj^.ob_type^.tp_is_gc(obj) = 1));
end;
{$ENDIF}

{$IFDEF PYTHON22_OR_HIGHER}
function TPythonInterface.PyWeakref_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (PyWeakref_CheckRef(obj) or PyWeakref_CheckProxy(obj));
end;

function TPythonInterface.PyWeakref_CheckRef( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(_PyWeakref_RefType));
end;

function TPythonInterface.PyWeakref_CheckProxy( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and
            ( (obj^.ob_type = PPyTypeObject(_PyWeakref_ProxyType)) or
              (obj^.ob_type = PPyTypeObject(_PyWeakref_CallableProxyType)) );
end;
{$ENDIF}

{$IFDEF PYTHON23_OR_HIGHER}
function TPythonInterface.PyBool_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyBool_Type);
end;

function TPythonInterface.PyBaseString_Check( obj : PPyObject ) : Boolean;
begin
  if IsPython3000 then
    Result:= PyObject_TypeCheck(obj, PyUnicode_Type)
  else
    Result := PyObject_TypeCheck(obj, PyBaseString_Type);
end;

function TPythonInterface.PyEnum_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyEnum_Type));
end;
{$ENDIF}

function TPythonInterface.PyObject_TypeCheck(obj : PPyObject; t : PPyTypeObject) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = t);
{$IFDEF PYTHON23_OR_HIGHER}
  if not Result and Assigned(obj) and Assigned(t) then
    Result := PyType_IsSubtype(obj^.ob_type, t) = 1;
{$ENDIF}
end;

function TPythonInterface.Py_InitModule( const AName : PChar; md : PPyMethodDef) : PPyObject;
Var
  moduledef : PyModuleDef;
  modules  : PPyObject;
begin
  CheckPython;
  if IsPython3000 then begin
    FillChar(moduledef, SizeOf(moduledef), 0);
    moduledef.m_base.ob_refcnt := 1;
    moduledef.m_name := AName;
    moduledef.m_methods := md;
    moduledef.m_size := -1;
    Result:= PyModule_Create2(@moduledef, APIVersion);
    if not Assigned(Result) then
      GetPythonEngine.CheckError;
    // To emulate Py_InitModule4 we need to add the module to sys.modules
    modules := PyImport_GetModuleDict;
    if PyDict_SetItemString(modules, AName, Result) <> 0 then
      GetPythonEngine.CheckError;
  end else
    Result := Py_InitModule4( AName, md, nil, nil, APIVersion );
end;

procedure TPythonInterface.Py_FlushLine; cdecl;
begin
  if Assigned(DLL_Py_FlushLine) then
    DLL_Py_FlushLine;
end;

(*******************************************************)
(**                                                   **)
(**            class TPythonTraceback                 **)
(**                                                   **)
(*******************************************************)

function TPythonTraceback.GetItemCount : Integer;
begin
  Result := FItems.Count;
end;

function TPythonTraceback.GetItem( idx : Integer ) : TTracebackItem;
begin
  Result := TTracebackItem(FItems.Items[idx]);
end;

constructor TPythonTraceback.Create;
begin
  inherited;
  FLimit := 1000;
  FItems := TList.Create;
end;

destructor TPythonTraceback.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TPythonTraceback.Clear;
var
  i : Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items[i].Free;
  FItems.Clear;
end;

{******
 * Warning !
 * This method must be called after the PyErr_Print function,
 * otherwise it can't extract the traceback informations.
 *
 * This method is automatically called by the Exec/Eval methods of
 * TPythonEngine. But if you use the Python core API, then don't
 * forget to refresh the traceback yourself. Or much better,
 * simply use the method CheckError wich will call PyErr_Print,
 * Traceback.Refresh and RaiseError for you.
}
procedure TPythonTraceback.Refresh;
var
//  tb, tb1  : PPyTraceBackObject;
  tb, tb1  : PPyObject;
  obj      : PPyObject;
  frame    : PPyObject;
  code     : PPyObject;
  depth    : Integer;
  limitv   : PPyObject;
  aLimit   : Integer;
  item     : TTracebackItem;
begin
  Clear;
  with GetPythonEngine do
    begin
      // get the limit of the traceback
      alimit := FLimit;
      limitv := PySys_GetObject('tracebacklimit');
      if Assigned(limitv) and PyInt_Check(limitv) then
        alimit := PyInt_AsLong(limitv);
      tb := PySys_GetObject('last_traceback');
      tb1 := tb;
      Py_XIncRef(tb1);
      depth := 0;
      // Evaluate the depth of the traceback
      while Assigned(tb1) and (tb1 <> Py_None) do
        begin
          Inc(depth);
          Py_XDecRef(tb1);
          tb1 := PyObject_GetAttrString(tb1, 'tb_next');
          CheckError(False);
        end;
      Py_XDecRef(tb1);
      // build the trace back
      Py_XIncRef(tb);
      while Assigned(tb) and (tb <> Py_None) do
        begin
          try
            if depth <= alimit then
              begin
                item     := TTracebackItem.Create;
                try
                  obj := PyObject_GetAttrString(tb, 'tb_lineno');
                  CheckError(False);
                  try
                    item.LineNo   := PyObjectAsVariant(obj);
                  finally
                    Py_XDecRef(obj);
                  end;
                  frame := PyObject_GetAttrString(tb, 'tb_frame');
                  CheckError(False);
                  try
                    if Assigned(frame) and (frame <> Py_None) then
                    begin
                      code := PyObject_GetAttrString(frame, 'f_code');
                      CheckError(False);
                      try
                        obj := PyObject_GetAttrString(code, 'co_filename');
                        CheckError(False);
                        try
                          item.Filename := PyObjectAsVariant( obj );
                        finally
                          Py_XDecRef(obj);
                        end;
                        obj := PyObject_GetAttrString(code, 'co_name');
                        CheckError(False);
                        try
                          item.Context  := PyObjectAsVariant( obj );
                        finally
                          Py_XDecRef(obj);
                        end;
                      finally
                        Py_XDecRef(code);
                      end;
                    end;
                  finally
                    Py_XDecRef(frame);
                  end;
                except
                  item.Free;
                  raise;
                end;
                FItems.Add( item );
              end;
            Dec( depth );
          finally
            Py_XDecRef(tb);
          end;
          tb := PyObject_GetAttrString(tb, 'tb_next');
          CheckError(False);
        end;
      Py_XDecRef(tb);
    end;
end;


(*******************************************************)
(**                                                   **)
(**            class TPythonEngine                    **)
(**                                                   **)
(*******************************************************)


constructor TPythonEngine.Create(AOwner: TComponent);
var
  i : Integer;
begin
  inherited;
  FLock                    := TCriticalSection.Create;
  FInitialized             := False;
  FInitScript              := TstringList.Create;
  FClients                 := TList.Create;
  FRedirectIO              := True;
  FExecModule              := '__main__';
  FAutoFinalize            := True;
  FInitThreads             := False;
  FTraceback               := TPythonTraceback.Create;
  FUseWindowsConsole       := False;
  FPyFlags                 := [];
  FDatetimeConversionMode  := DEFAULT_DATETIME_CONVERSION_MODE;
  if csDesigning in ComponentState then
    begin
      for i := 0 to AOwner.ComponentCount - 1 do
        if (AOwner.Components[i] is TPythonEngine) and
           (AOwner.Components[i] <> Self) then
          raise Exception.Create('You can''t drop more than one TPythonEngine component');
    end;
end;

destructor TPythonEngine.Destroy;
begin
  LocalVars := nil;
  GlobalVars := nil;
  Destroying;
  Finalize;
  // Free our objects
  FClients.Free;
  FInitScript.Free;
  FTraceback.Free;
  FLock.Free;
  inherited;
end;

procedure TPythonEngine.Finalize;
var
  i: integer;
  canDetachClients : Boolean;
begin
  // switch off redirection when the component is destroying,
  // because the form or datamodule is beeing closed, and
  // redirecting output may crash the application.
  if FIORedirected and not (csDestroying in ComponentState) and Initialized then
  begin
    RedirectIO := False;
    // restore the initial streams also.
    ExecString('import sys'+LF+
               'if hasattr(sys, "old_stdin"):  sys.stdin=sys.old_stdin'+LF+
               'if hasattr(sys, "old_stdout"): sys.stdout=sys.old_stdout'+LF+
               'if hasattr(sys, "old_stderr"): sys.stderr=sys.old_stderr' );
  end;
  // First finalize our clients
  if Initialized then
    for i := 0 to ClientCount - 1 do
      with Clients[i] do
        begin
          if Initialized then
            Finalize;
        end;
  // Then finalize Python, if we have to
  if Initialized and FAutoFinalize then
    try
      FFinalizing := True;
      Py_Finalize;
    finally
      FFinalizing := False;
    end;
  // Detach our clients, when engine is beeing destroyed or one of its clients.
  canDetachClients := csDestroying in ComponentState;
  if not canDetachClients then
    for i := 0 to ClientCount - 1 do
      if csDestroying in Clients[i].ComponentState then
      begin
        canDetachClients := True;
        Break;
      end;
  if canDetachClients then
  begin
    for i := 0 to ClientCount - 1 do
      Clients[i].ClearEngine;
    FClients.Clear;
  end;
  // Free our reference
  gPythonEngine               := nil;
  FTimeStruct                 := nil;
  FPyDateTime_DateType        := nil;
  FPyDateTime_DateTimeType    := nil;
  FPyDateTime_DeltaType       := nil;
  FPyDateTime_TimeType        := nil;
  FPyDateTime_TZInfoType      := nil;
  FPyDateTime_TimeTZType      := nil;
  FPyDateTime_DateTimeTZType  := nil;
end;

procedure TPythonEngine.Lock;
begin
  FLock.Enter;
end;

procedure TPythonEngine.Unlock;
begin
  FLock.Leave;
end;

procedure TPythonEngine.AfterLoad;
begin
  inherited;
  Initialize;
end;

procedure TPythonEngine.BeforeLoad;
begin
  if UseWindowsConsole then
    InitWinConsole;
  inherited;
end;

procedure TPythonEngine.DoOpenDll(const aDllName : String);
var
  i : Integer;
begin
  if UseLastKnownVersion then
    for i:= Integer(COMPILED_FOR_PYTHON_VERSION_INDEX) to High(PYTHON_KNOWN_VERSIONS) do
    begin
      FDLLHandle := SafeLoadLibrary(PChar(GetDllPath+PYTHON_KNOWN_VERSIONS[i].DllName));
      if IsHandleValid then
      begin
        DllName := PYTHON_KNOWN_VERSIONS[i].DllName;
        APIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
        RegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
        Exit;
      end;
      if not PYTHON_KNOWN_VERSIONS[i].CanUseLatest then
        Break;
    end;
  inherited;
end;

procedure TPythonEngine.AssignPyFlags;

  procedure SetFlag( AFlag: PInt; AValue : Boolean );
  begin
    if AValue then
      AFlag^ := 1
    else
      AFlag^ := 0;
  end;

begin
  // define each Python flag. See file pyDebug.h
  SetFlag(Py_DebugFlag,       pfDebug in FPyFlags);
  SetFlag(Py_VerboseFlag,     pfVerbose in FPyFlags);
  SetFlag(Py_InteractiveFlag, pfInteractive in FPyFlags);
  SetFlag(Py_OptimizeFlag,    pfOptimize in FPyFlags);
  SetFlag(Py_NoSiteFlag,      pfNoSite in FPyFlags);
  SetFlag(Py_UseClassExceptionsFlag, pfUseClassExceptionsFlag in FPyFlags);
  SetFlag(Py_FrozenFlag,      pfFrozenFlag in FPyFlags);
{$IFDEF PYTHON20_OR_HIGHER}
  if not IsPython3000 then begin
    SetFlag(Py_UnicodeFlag,     pfUnicode in FPyFlags);
    SetFlag(Py_TabcheckFlag,    pfTabcheck in FPyFlags);
  end;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
  SetFlag(Py_IgnoreEnvironmentFlag, pfIgnoreEnvironmentFlag in FPyFlags);
  SetFlag(Py_DivisionWarningFlag, pfDivisionWarningFlag in FPyFlags);
{$ENDIF}
end;

procedure TPythonEngine.Initialize;

  procedure InitSysPath;
  var
    _path : PPyObject;
  begin
    _path := PySys_GetObject('path');
    if Assigned(FOnSysPathInit) then
      FOnSysPathInit(Self, _path);
  end;

  function GetVal(AModule : PPyObject; AVarName : String) : PPyObject;
  begin
    Result := PyObject_GetAttrString(AModule, PChar(AVarName));
    if PyErr_Occurred <> nil then
      PyErr_Clear
    else
      Py_XDecRef(Result); // keep a borrowed reference.
  end;

  procedure GetTimeStructType;
  var
    timeModule : PPyObject;
  begin
    if APIVersion >= 1011 then // from Python 2.2
    begin
      timeModule := PyImport_ImportModule('time');
      try
        if Assigned(timeModule) then
          FTimeStruct := GetVal(timeModule, 'struct_time')
        else
          PyErr_Clear;
      finally
        Py_XDecRef(timeModule);
      end;
    end
    else
      FTimeStruct := nil;
  end;

  procedure GetDateTimeTypes;
  var
    dateTimeModule : PPyObject;
  begin
    if APIVersion >= 1012 then // from Python 2.3
    begin
      dateTimeModule := PyImport_ImportModule('datetime');
      try
        if Assigned(dateTimeModule) then
        begin
          FPyDateTime_DateType        := GetVal(dateTimeModule, 'date');
          FPyDateTime_DateTimeType    := GetVal(dateTimeModule, 'datetime');
          FPyDateTime_DeltaType       := GetVal(dateTimeModule, 'timedelta');
          FPyDateTime_TimeType        := GetVal(dateTimeModule, 'time');
          FPyDateTime_TZInfoType      := GetVal(dateTimeModule, 'tzinfo');
          FPyDateTime_TimeTZType      := GetVal(dateTimeModule, 'timetz');
          FPyDateTime_DateTimeTZType  := GetVal(dateTimeModule, 'datetimetz');
        end
        else
          PyErr_Clear;
      finally
        Py_XDecRef(dateTimeModule);
      end;
    end
    else
    begin
      FPyDateTime_DateType        := nil;
      FPyDateTime_DateTimeType    := nil;
      FPyDateTime_DeltaType       := nil;
      FPyDateTime_TimeType        := nil;
      FPyDateTime_TZInfoType      := nil;
      FPyDateTime_TimeTZType      := nil;
      FPyDateTime_DateTimeTZType  := nil;
    end;
  end;

var
  i : Integer;
begin
  if Assigned(gPythonEngine) then
    raise Exception.Create('There is already one instance of TPythonEngine running' );

  {$IFDEF FPC}
  //this allows you to just call Initialize to create a non-IDE instance with
  //Lazarus.
  if(AutoLoad and not IsHandleValid) then begin
    LoadDLL;//Calls initialize
    exit;
  end;
  {$ENDIF}

  gPythonEngine := Self;
  CheckRegistry;
  if IsPython3000 then begin
    if Assigned(Py_SetProgramName3000) then
    begin
      FProgramNameW := ParamStr(0);
      Py_SetProgramName3000(PWideChar(FProgramNameW));
    end
  end else begin
    if Assigned(Py_SetProgramName) then
    begin
      FProgramName := ParamStr(0);
      Py_SetProgramName(PChar(FProgramName));
    end
  end;
  AssignPyFlags;
  Py_Initialize;
  FInitialized := True;
  FIORedirected := False;
  InitSysPath;
  SetProgramArgs;
  GetTimeStructType;
  GetDateTimeTypes;
  if InitThreads and Assigned(PyEval_InitThreads) then
    PyEval_InitThreads;
  if RedirectIO and Assigned(FIO) then
    DoRedirectIO;
  for i := 0 to ClientCount - 1 do
    with Clients[i] do
      if not Initialized then
        Initialize;
  if InitScript.Count > 0 then
    ExecStrings( InitScript );
  if Assigned(FOnAfterInit) then
    FOnAfterInit(Self);
end;

procedure TPythonEngine.SetInitScript(Value: TStrings);
begin
  FInitScript.Assign(Value);
end;

function TPythonEngine.GetInterpreterState: PPyInterpreterState;
var
  res: PPyThreadState;
begin
  if Assigned(PyThreadState_Get) then begin
    res:= PyThreadState_Get;
    Result := res^.interp;
  end else
    Result := nil;
end;

function TPythonEngine.GetThreadState: PPyThreadState;
begin
  if Assigned(PyThreadState_Get) then
    Result := PyThreadState_Get
  else
    Result := nil;
end;

procedure TPythonEngine.SetInitThreads(Value: Boolean);
begin
  if Value <> FInitThreads then
  begin
    if Value and Assigned(PyEval_InitThreads) then
      PyEval_InitThreads;
    FInitThreads := Value;
  end;
end;

function TPythonEngine.GetClientCount : Integer;
begin
  Result := FClients.Count;
end;

function TPythonEngine.GetClients( idx : Integer ) : TEngineClient;
begin
  Result := TEngineClient( FClients.Items[idx] );
end;

procedure TPythonEngine.Notification( AComponent: TComponent;
                                      Operation: TOperation);
var
  i : Integer;
begin
  inherited;
  if Operation = opRemove then
    begin
      if AComponent = IO then
        IO := nil
      else
        begin
          for i := 0 to ClientCount - 1 do
            if Clients[i] = AComponent then
              begin
                RemoveClient( Clients[i] );
                Break;
              end;
        end;
    end;
end;

procedure TPythonEngine.CheckRegistry;
{$IFDEF MSWINDOWS}
var
  key : String;
  path : String;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  try
    with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
      try
        //Access := KEY_READ; // works only with Delphi5 or greater
        RootKey := HKEY_LOCAL_MACHINE;
        key := Format('\Software\Python\PythonCore\%s\PythonPath', [RegVersion]);
        if not KeyExists( key ) then
          begin
            // try a current user installation
            RootKey := HKEY_CURRENT_USER;
            if not KeyExists( key ) then
            begin
              if Assigned( FOnPathInitialization ) then
                begin
                  path := '';
                  FOnPathInitialization( Self, path );
                  if path <> '' then
                    begin
                      //Access := KEY_ALL_ACCESS; // works only with Delphi5 or greater
                      OpenKey( key, True );
                      WriteString( '', path );
                      CloseKey;
                    end;
                end;
            end;
          end;
      finally
        Free;
      end;
  except
    // under WinNT, with a user without admin rights, the access to the
    // LocalMachine keys would raise an exception.
  end;
{$ENDIF}
end;

procedure TPythonEngine.SetProgramArgs;
var
  buff : PChar;
  argv : PPChar;
  i, argc : Integer;
  L : array of String;
  wbuff : PWideChar;
  wargv : PPWideChar;
  WL : array of WideString;
begin
  // we build a string list of the arguments, because ParamStr returns a volatile string
  // and we want to build an array of PChar, pointing to valid strings.
  argc := ParamCount;
  if not IsPython3000 then begin
    SetLength(L, argc+1);
    GetMem( buff, sizeof(PChar)*(argc+1) );
    try
      argv := PPChar(buff);
      // get the strings
      // build the PChar array
      for i := 0 to argc do begin
        L[i] := ParamStr(i);
        argv^[i] := PChar(L[i]);
      end;
      // set the argv list of the sys module with the application arguments
      PySys_SetArgv( argc+1, argv );
    finally
      FreeMem( buff );
    end;
  end else begin
    SetLength(WL, argc+1);
    GetMem( wbuff, sizeof(PWideChar)*(argc+1) );
    try
      wargv := PPWideChar(wbuff);
      // get the strings
      // build the PChar array
      for i := 0 to argc do begin
        WL[i] := ParamStr(i);
        wargv^[i] := PWideChar(WL[i]);
      end;
      // set the argv list of the sys module with the application arguments
      PySys_SetArgv3000( argc + 1, wargv );
    finally
      FreeMem( wbuff );
    end;
  end;
end;

procedure TPythonEngine.InitWinConsole;
begin
{$IFDEF MSWINDOWS}
  FreeConsole;
  AllocConsole;
  SetConsoleTitle( 'Python console' );
{$ENDIF}
end;

procedure TPythonEngine.SetUseWindowsConsole( const Value : Boolean );
begin
  FUseWindowsConsole := Value;
  if (csDesigning in ComponentState) then
    RedirectIO := False;
end;

// GlobalVars contains a dictionary object used by the Run_CommandAsObject method, if not nil.
// Warning ! SetGlobalVars increments the reference count of the dictionary object !
procedure TPythonEngine.SetGlobalVars(const Value: PPyObject);
begin
  Py_XDecRef(FGlobalVars);
  if Assigned(Value) then
    if PyDict_Check(Value) then
      FGlobalVars := Value
    else
      begin
        FGlobalVars := nil;
        raise Exception.Create('You must set a Python dictionary in the GlobalVars property');
      end
  else
    FGlobalVars := nil;
  Py_XIncRef(FGlobalVars);
end;

// LocalVars contains a dictionary object used by the Run_CommandAsObject method, if not nil.
// Warning ! SetLocalVars increments the reference count of the dictionary object !
procedure TPythonEngine.SetLocalVars(const Value: PPyObject);
begin
  Py_XDecRef(FLocalVars);
  if Assigned(Value) then
    if PyDict_Check(Value) then
      FLocalVars := Value
    else
      begin
        FLocalVars := nil;
        raise Exception.Create('You must set a Python dictionary in the LocalVars property');
      end
  else
    FLocalVars := nil;
  Py_XIncRef(FLocalVars);
end;

procedure TPythonEngine.SetPyFlags(const Value: TPythonFlags);
begin
  if FPyFlags <> Value then
  begin
    if Initialized then
      raise Exception.Create('You can''t modify Python flags after it has been initialized');
    FPyFlags := Value;
  end; // of if
end;

function TPythonEngine.IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
begin
  result := ob^.ob_type = obt;
end;

function TPythonEngine.GetAttrString(obj: PPyObject; AName: PChar):PChar;
var
   attr: PPyObject;
begin
  CheckPython;
  result := nil;
  attr := PyObject_GetAttrString(obj, AName);
  if attr <> nil then begin
    result := PyString_AsString(attr);
    Py_XDECREF(attr);
  end;
  PyErr_Clear;
end;

function TPythonEngine.CleanString(const s : string) : string;
var
  i : Integer;
begin
  result := s;
  if s = '' then
    Exit;
  i := Pos(CR,s);
  while i > 0 do
    begin
      Delete( result, i, 1 );
      i := Pos(CR,result);
    end;
  if result[length(result)] <> LF then
    Insert( LF, result, length(result)+1 );
end;

function   TPythonEngine.EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
var presult :PPyObject;
begin
  CheckPython;
  Result := -1;
  if pyfunc = nil then exit;
  try
    presult := PyEval_CallObject(pyfunc,pyargs);
    CheckError(False);
    if presult = nil then
      begin
        PyErr_Print;
        RaiseError;
      end
    else
      begin
        try
          if presult = Py_None then
            Result := 0
          else
            Result := PyObjectAsVariant( presult );
        finally
          Py_DECREF(presult);
        end;
      end;
    Py_FlushLine;
  except
    Py_FlushLine;
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise;
  end;
end;

function   TPythonEngine.EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
var pargs: PPyObject;
begin
  CheckPython;
  pargs := ArrayToPyTuple(args);
  try
    Result := EvalPyFunction(pyfunc,pargs);
  finally
    Py_DECREF(pargs);
  end;
end;

function   TPythonEngine.EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
var pargs: PPyObject;
begin
  CheckPython;
  pargs := PyTuple_New(0);
  try
    Result := EvalPyFunction(pyfunc, pargs);
  finally
    Py_DECREF(pargs);
  end;
end;

function   TPythonEngine.EvalStringAsStr(const command : String) : String;
begin
  Result := Run_CommandAsString( command, eval_input );
end;

function   TPythonEngine.EvalString(const command : String) : PPyObject;
begin
  Result := Run_CommandAsObject( command, eval_input );
end;

procedure TPythonEngine.ExecString(const command : String);
begin
  Py_XDecRef( Run_CommandAsObject( command, file_input ) );
end;

function   TPythonEngine.Run_CommandAsString(const command : String; mode : Integer) : String;
var
  v : PPyObject;
begin
  Result := '';
  v := Run_CommandAsObject( command, mode );
  Result := PyObjectAsString( v );
  Py_XDECREF(v);
end;

function   TPythonEngine.Run_CommandAsObject(const command : String; mode : Integer) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict(command, mode, nil, nil);
end;

function TPythonEngine.Run_CommandAsObjectWithDict(const command : String; mode : Integer; locals, globals : PPyObject) : PPyObject;
var
  m : PPyObject;
  _locals, _globals : PPyObject;
begin
  CheckPython;
  Result := nil;
  Traceback.Clear;
  CheckError(False);

  m := GetMainModule;
  if m = nil then
    raise EPythonError.Create('Run_CommandAsObject: can''t create __main__');

  if Assigned(locals) then
    _locals  := locals
  else if Assigned(FLocalVars) then
    _locals  := LocalVars
  else
    _locals  := PyModule_GetDict(m);

  if Assigned(globals) then
    _globals := globals
  else if Assigned(FGlobalVars) then
    _globals := GlobalVars
  else
    _globals := PyModule_GetDict(m);

  try
    Result := PyRun_String(PChar(CleanString(command)), mode, _globals, _locals);
    if Result = nil then
      CheckError(False);
    Py_FlushLine;
  except
    Py_FlushLine;
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise;
  end;
end;

procedure TPythonEngine.ExecStrings( strings : TStrings );
begin
  Py_XDecRef( Run_CommandAsObject( CleanString( strings.Text ), file_input ) );
end;

function TPythonEngine.EvalStrings( strings : TStrings ) : PPyObject;
begin
  Result := Run_CommandAsObject( CleanString( strings.Text ), eval_input );
end;

{$IFDEF DELPHI4_OR_HIGHER}

procedure TPythonEngine.ExecString(const command : String; locals, globals : PPyObject );
begin
  Py_XDecRef( Run_CommandAsObjectWithDict( command, file_input, locals, globals ) );
end;

procedure TPythonEngine.ExecStrings( strings : TStrings; locals, globals : PPyObject );
begin
  Py_XDecRef( Run_CommandAsObjectWithDict( CleanString( strings.Text ), file_input, locals, globals ) );
end;

function TPythonEngine.EvalString( const command : String; locals, globals : PPyObject ) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict( command, eval_input, locals, globals );
end;

function TPythonEngine.EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict( CleanString( strings.Text ), eval_input, locals, globals );
end;

{$ENDIF}

function TPythonEngine.EvalStringsAsStr( strings : TStrings ) : String;
begin
  Result := Run_CommandAsString( CleanString( strings.Text ), eval_input );
end;

function TPythonEngine.CheckEvalSyntax( const str : String ) : Boolean;
begin
  result := CheckSyntax( str, eval_input );
end;

function TPythonEngine.CheckExecSyntax( const str : String ) : Boolean;
begin
  result := CheckSyntax( str, file_input );
end;

function TPythonEngine.CheckSyntax( const str : String; mode : Integer ) : Boolean;
var
  n : PNode;
begin
  n := PyParser_SimpleParseString( PChar(str), mode );
  result := Assigned(n);
  if Assigned( n ) then
    PyNode_Free(n);
end;

procedure TPythonEngine.RaiseError;

  function Define( E : EPythonError; const sType, sValue : String ) : EPythonError;
  begin
    E.EName  := sType;
    E.EValue := sValue;
    if sValue <> '' then
      E.Message := Format('%s: %s',[sType,sValue])
    else
      E.Message := sType;
    Result := E;
  end;

  function DefineSyntaxError( E : EPySyntaxError; const sType, sValue : String; err_type, err_value : PPyObject ) : EPySyntaxError;
  var
    s_value       : String;
    s_line        : String;
    s_filename    : String;
    i_line_number : Integer;
    i_offset      : Integer;
    tmp           : PPyObject;
  begin
    Result := E;
    Result.EName  := sType;
    Result.EValue := sValue;
    s_value := '';
    s_line        := '';
    s_filename    := '';
    i_line_number := 0;
    i_offset      := 0;
    // Sometimes there's a tuple instead of instance...
    if PyTuple_Check( err_value )  and (PyTuple_Size( err_value) >= 2) then
    begin
      s_value := PyString_AsDelphiString(PyTuple_GetItem( err_value, 0));
      err_value := PyTuple_GetItem( err_value, 1);
      if PyTuple_Check( err_value )  and (PyTuple_Size( err_value) >= 4) then
      begin
        i_line_number := PyInt_AsLong(PyTuple_GetItem( err_value, 1));
        i_offset      := PyInt_AsLong(PyTuple_GetItem( err_value, 2));
        s_line := Trim(PyString_AsDelphiString(PyTuple_GetItem( err_value, 3)));
      end;
    end else
    // Is it an instance of the SyntaxError class ?
    if (PyInstance_Check( err_value ) and (PyClass_IsSubclass( PPyObject(PPyInstanceObject(err_value)^.in_class), err_type ) <> 0)) or
       ((PyType_IsSubtype(PPyTypeObject(err_type), PPyTypeObject(PyExc_SyntaxError^)) = 1) and IsType(err_value, PPyTypeObject(err_type))) then
      begin
        // Get the filename
        tmp := PyObject_GetAttrString(err_value, 'filename');
        if tmp <> nil then begin
          if PyString_Check(tmp) then
            s_filename := PyString_AsDelphiString(tmp)
          else if tmp = Py_None then
            s_filename := '???';
          Py_XDECREF(tmp);
        end;
        // Get the text containing the error, cut of carriage return
        tmp := PyObject_GetAttrString(err_value, 'text');
        if Assigned(tmp) and PyString_Check(tmp) then
          s_line := Trim(PyString_AsDelphiString(tmp));
        Py_XDECREF(tmp);
        // Get the offset where the error should appear
        tmp := PyObject_GetAttrString(err_value, 'offset' );
        if Assigned(tmp) and PyInt_Check(tmp) then
          i_offset := PyInt_AsLong(tmp);
        Py_XDECREF(tmp);
        // Get the line number of the error
        tmp := PyObject_GetAttrString(err_value, 'lineno' );
        if Assigned(tmp) and PyInt_Check(tmp) then
          i_line_number := PyInt_AsLong(tmp);
        Py_XDECREF(tmp);
        // Get the message of the error
        tmp := PyObject_GetAttrString(err_value, 'msg' );
        if Assigned(tmp) and PyString_Check(tmp) then
          s_value := PyString_AsDelphiString(tmp);
        Py_XDECREF(tmp);
      end;
    // If all is ok
    if s_value <> '' then
      begin
        with Result do
          begin
            Message     := Format('%s: %s (line %d, offset %d): ''%s''', [sType,s_value,i_line_number, i_offset,s_line]);
            EName       := sType;
            EValue      := s_value;
            EFileName   := s_filename;
            ELineNumber := i_line_number;
            EOffset     := i_offset;
            ELineStr    := s_line;
          end;
      end
    else
      Result.Message := sType;
  end;

  function GetTypeAsString( obj : PPyObject ) : String;
  begin 
    if PyClass_Check( obj ) then
      with PPyClassObject(obj)^ do
        Result := PyString_AsDelphiString(cl_name)
    else if PyType_CheckExact( obj ) then
        Result := PPyTypeObject(obj).tp_name
    else
      Result := PyObjectAsString(obj);
  end;

var
  err_type, err_value : PPyObject;
  s_type        : String;
  s_value       : String;
begin
  s_value       := '';

  if PyErr_Occurred <> nil then
    PyErr_Print;
  err_type := PySys_GetObject('last_type');
  err_value := PySys_GetObject('last_value');
  if Assigned(err_type) then
    begin
      s_type := GetTypeAsString(err_type);
      s_value := PyObjectAsString(err_value);

      if (PyErr_GivenExceptionMatches(err_type, PyExc_SystemExit^) <> 0) then
        raise Define( EPySystemExit.Create(''), s_type, s_value )
{$IFDEF PYTHON22_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_StopIteration^) <> 0) then
        raise Define( EPyStopIteration.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_KeyboardInterrupt^) <> 0) then
        raise Define( EPyKeyboardInterrupt.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ImportError^) <> 0) then
        raise Define( EPyImportError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
  {$IFDEF MSWINDOWS}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_WindowsError^) <> 0) then
        raise Define( EPyWindowsError.Create(''), s_type, s_value )
  {$ENDIF}
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IOError^) <> 0) then
        raise Define( EPyIOError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_OSError^) <> 0) then
        raise Define( EPyOSError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_EnvironmentError^) <> 0) then
        raise Define( EPyEnvironmentError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_EOFError^) <> 0) then
        raise Define( EPyEOFError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_NotImplementedError^) <> 0) then
        raise Define( EPyNotImplementedError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_RuntimeError^) <> 0) then
        raise Define( EPyRuntimeError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnboundLocalError^) <> 0) then
        raise Define( EPyUnboundLocalError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_NameError^) <> 0) then
        raise Define( EPyNameError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_AttributeError^) <> 0) then
        raise Define( EPyAttributeError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_TabError^) <> 0) then
        raise DefineSyntaxError( EPyTabError.Create(''), s_type, s_value, err_type, err_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IndentationError^) <> 0) then
        raise DefineSyntaxError( EPyIndentationError.Create(''), s_type, s_value, err_type, err_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SyntaxError^) <> 0) then
        raise DefineSyntaxError( EPySyntaxError.Create(''), s_type, s_value, err_type, err_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_TypeError^) <> 0) then
        raise Define( EPyTypeError.Create(''), s_type, s_value )
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_AssertionError^) <> 0) then
        raise Define( EPyAssertionError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IndexError^) <> 0) then
        raise Define( EPyIndexError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_KeyError^) <> 0) then
        raise Define( EPyKeyError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_LookupError^) <> 0) then
        raise Define( EPyLookupError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_OverflowError^) <> 0) then
        raise Define( EPyOverflowError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ZeroDivisionError^) <> 0) then
        raise Define( EPyZeroDivisionError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_FloatingPointError^) <> 0) then
        raise Define( EPyFloatingPointError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ArithmeticError^) <> 0) then
        raise Define( EPyArithmeticError.Create(''), s_type, s_value )
{$IFDEF PYTHON23_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeEncodeError^) <> 0) then
        raise Define( UnicodeEncodeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeDecodeError^) <> 0) then
        raise Define( UnicodeDecodeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeTranslateError^) <> 0) then
        raise Define( UnicodeTranslateError.Create(''), s_type, s_value )
{$ENDIF}
{$IFDEF PYTHON20_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeError^) <> 0) then
        raise Define( EPyUnicodeError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ValueError^) <> 0) then
        raise Define( EPyValueError.Create(''), s_type, s_value )
{$IFDEF PYTHON22_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ReferenceError^) <> 0) then
        raise Define( EPyReferenceError.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SystemError^) <> 0) then
        raise Define( EPySystemError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_MemoryError^) <> 0) then
        raise Define( EPyMemoryError.Create(''), s_type, s_value )
      else if (not IsPython3000) and (PyErr_GivenExceptionMatches(err_type, PyExc_StandardError^) <> 0) then
        raise Define( EPyStandardError.Create(''), s_type, s_value )
{$IFDEF PYTHON21_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UserWarning^) <> 0) then
        raise Define( EPyUserWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_DeprecationWarning^) <> 0) then
        raise Define( EPyDeprecationWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SyntaxWarning^) <> 0) then
        raise Define( EPySyntaxWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_RuntimeWarning^) <> 0) then
        raise Define( EPyRuntimeWarning.Create(''), s_type, s_value )
{$ENDIF}
{$IFDEF PYTHON23_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_FutureWarning^) <> 0) then
        raise Define( FutureWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_PendingDeprecationWarning^) <> 0) then
        raise Define( PendingDeprecationWarning.Create(''), s_type, s_value )
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_Warning^) <> 0) then
        raise Define( EPyWarning.Create(''), s_type, s_value )
{$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_Exception^) <> 0) then
        raise Define( EPyException.Create(''), s_type, s_value )
      else  // Else if no known exception was detected,
            // then build an ExecError exception
        raise Define( EPyExecError.Create(''), s_type, s_value );
    end
  else
    raise EPythonError.Create('RaiseError: could''nt fetch last exception');
end;

function TPythonEngine.PyObjectAsString( obj : PPyObject ) : String;
var
  s : PPyObject;
//  i : Integer;
//  tmp : PChar;
{$IFDEF UNICODE_SUPPORT}
  w : WideString;
{$ENDIF}
begin
  CheckPython;
  Result := '';
  if not Assigned( obj ) then
    Exit;

{$IFDEF UNICODE_SUPPORT}
  if PyUnicode_Check(obj) then
  begin
    w := PyUnicode_AsWideString(obj);
    Result := w;
    Exit;
  end;
{$ENDIF}
  s := PyObject_Str( obj );
  if Assigned(s) and PyString_Check(s) then
    begin
      Result := PyString_AsDelphiString(s);
//      tmp := PyString_AsString(s);
//      SetLength( Result, PyString_Size(s)+1 );
//      Result := '';
//      for i := 0 to PyString_Size(s) - 1 do
//        Insert( tmp[i], Result, i+1 );
    end;
  Py_XDECREF(s);
end;

procedure TPythonEngine.DoRedirectIO;
const
  code = 'import sys'+LF+
         'class DebugOutput:'+LF+
         '  pyio = __import__("pyio")'+LF+
         '  softspace=0'+LF+
         '  encoding=None'+LF+
         '  def write(self,message):'+LF+
         '     self.pyio.write(message)'+LF+
         '  def readline(self, size=None):'+LF+
         '     return self.pyio.read(size)'+LF+
         '  def flush(self):' + LF +
         '     pass' + LF +
         'sys.old_stdin=sys.stdin'+LF+
         'sys.old_stdout=sys.stdout'+LF+
         'sys.old_stderr=sys.stderr'+LF+
         'sys.stdin=sys.stderr=sys.stdout=DebugOutput()'+LF+#0;
begin
  if csDesigning in ComponentState then
    Exit;
  CheckPython;
  if not Assigned(FIOPythonModule) then
  begin
    // create a new module called pyio
    FIOPythonModule := TPythonModule.Create( Self );
    with FIOPythonModule as TPythonModule do
      begin
        Engine := Self;
        ModuleName := 'pyio';
        AddMethod( 'write', pyio_write, 'write(String) -> None' );
        AddMethod( 'read',  pyio_read,  'read() -> String' );
        AddMethod( 'SetDelayWrites',  pyio_SetDelayWrites,  'SetDelayWrites(Boolean) -> None' );
        AddMethod( 'SetMaxLines',  pyio_SetMaxLines,  'SetMaxLines(Integer) -> None' );
        AddMethod( 'GetTypesStats',  pyio_GetTypesStats,  'GetTypesStats( [type name] ) -> a list of tuple (TypeName, InstanceCount, CreateHits, DeleteHits)' );
      end;
  end;
  with FIOPythonModule as TPythonModule do
    if not Initialized then
      Initialize;
  // execute the code
  ExecString(code);
  FIORedirected := True;
end;

procedure  TPythonEngine.AddClient( client : TEngineClient );
begin
  FClients.Add( client );
end;

procedure  TPythonEngine.RemoveClient( client : TEngineClient );
begin
  // We finalize the PythonEngine, as soon as a client should
  // be freed, because the destroy order of the components
  // is not predictable and may cause some memory crashes !
  if (csDesigning in ComponentState) then
    FClients.Remove( client )
  else if Initialized or (ClientCount > 0) then
    Finalize;
end;

function   TPythonEngine.FindClient( const aName : String ) : TEngineClient;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ClientCount - 1 do
    with TPythonType( Clients[i] ) do
      if Name = aName then
        begin
          Result := Clients[i];
          Break;
        end;
end;

function   TPythonEngine.TypeByName( const aTypeName : String ) : PPyTypeObject;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TPythonType then
      with TPythonType( Clients[i] ) do
        if TypeName = aTypeName then
          begin
            Result := TheTypePtr;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find type: %s', [aTypeName]);
end;

function   TPythonEngine.ModuleByName( const aModuleName : String ) : PPyObject;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TPythonModule then
      with TPythonModule( Clients[i] ) do
        if ModuleName = aModuleName then
          begin
            Result := Module;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find module: %s', [aModuleName]);
end;

function   TPythonEngine.MethodsByName( const aMethodsContainer: String ) : PPyMethodDef;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TMethodsContainer then
      with TMethodsContainer( Clients[i] ) do
        if Name = aMethodsContainer then
          begin
            Result := MethodsData;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find component: %s', [aMethodsContainer]);
end;

function TPythonEngine.VariantAsPyObject( const V : Variant ) : PPyObject;
Var
  DeRefV : Variant;

  function ArrayVarDim1 : PPyObject;
  var
    i, cpt : Integer;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        PyList_SetItem( Result, cpt, VariantAsPyObject(DeRefV[i]) );
        Inc(cpt);
      end;
  end;

  function ArrayVarDim2 : PPyObject;
  var
    i, j, cpt, cpt2 : Integer;
    L : PPyObject;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        L := PyList_New( VarArrayHighBound( DeRefV, 2 ) - VarArrayLowBound( DeRefV, 2 ) + 1 );
        PyList_SetItem( Result, cpt, L );
        cpt2 := 0;
        for j := VarArrayLowBound( DeRefV, 2 ) to VarArrayHighBound( DeRefV, 2 ) do
          begin
            PyList_SetItem( L, cpt2, VariantAsPyObject(DeRefV[i, j]) );
            Inc(cpt2);
          end;
        Inc(cpt);
      end;
  end;

  function ArrayVarDim3 : PPyObject;
  var
    i, j, k, cpt, cpt2, cpt3 : Integer;
    L, L2 : PPyObject;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        L := PyList_New( VarArrayHighBound( DeRefV, 2 ) - VarArrayLowBound( DeRefV, 2 ) + 1 );
        PyList_SetItem( Result, cpt, L );
        cpt2 := 0;
        for j := VarArrayLowBound( DeRefV, 2 ) to VarArrayHighBound( DeRefV, 2 ) do
          begin
            L2 := PyList_New( VarArrayHighBound( DeRefV, 3 ) - VarArrayLowBound( DeRefV, 3 ) + 1 );
            PyList_SetItem( Result, cpt2, L );
            cpt3 := 0;
            for k := VarArrayLowBound( DeRefV, 3 ) to VarArrayHighBound( DeRefV, 3 ) do
              begin
                PyList_SetItem( L2, cpt3, VariantAsPyObject(DeRefV[i, j, k]) );
                Inc(cpt3);
              end;
            Inc(cpt2);
          end;
        Inc(cpt);
      end;
  end;

const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}'; // copied from ActiveX.pas
var
  s : String;
  y, m, d, h, mi, sec, ms, jd, wd : WORD;
  dt : TDateTime;
  dl : Integer;
  myInt : Integer;
  wStr : WideString;
  Disp : IDispatch;
  DispID : Integer;
  args : PPyObject;
begin
  Disp := nil;
  //Dereference Variant
  DerefV := V;
  while VarType(DeRefV) = varByRef or varVariant do
    DeRefV := Variant(PVarData(TVarData(DeRefV).VPointer)^);

  case VarType(DeRefV) and VarTypeMask of
    varBoolean: begin
      if DeRefV = true then
        Result := PPyObject(Py_True)
      else
        Result := PPyObject(Py_False);
      Py_XIncRef(Result);
    end;
    varSmallint,
    varByte,
{$IFDEF DELPHI6_OR_HIGHER}
    varShortInt,
    varWord,
    varLongWord,
{$ENDIF}
    varInteger:  Result := PyInt_FromLong( DeRefV );
{$IFDEF DELPHI6_OR_HIGHER}
    varInt64:    Result := PyLong_FromLongLong( DeRefV );
{$ENDIF}
    varSingle,
    varDouble,
    varCurrency: Result := PyFloat_FromDouble( DeRefV );
    varDate:
      begin
        dt := DeRefV;
        DecodeDate( dt, y, m, d );
        DecodeTime( dt, h, mi, sec, ms );
        if (DatetimeConversionMode = dcmToTuple) then
        begin
          wd := (DayOfWeek( dt ) + 7 - 2) mod 7; // In Python, Monday is the first day (=0)
          jd := Round(EncodeDate(y,m,d)-EncodeDate(y,1,1))+1; // This shoud be the Julian day, the day in a year (0-366)
          dl := -1; // This is daylight save... ?¿?¿? I don't know what it is...
          Result := ArrayToPyTuple( [y, m, d, h, mi, sec, wd, jd, dl] );
        end
        else if (DatetimeConversionMode = dcmToDatetime) then
        begin
          if not Assigned(FPyDateTime_DateTimeType) then
            raise EPythonError.Create('dcmToDatetime DatetimeConversionMode cannot be used with this version of python. Missing module datetime');
          args := ArrayToPyTuple([y, m, d, h, mi, sec, ms*1000]);
          try
            Result := PyEval_CallObject(FPyDateTime_DateTimeType, args);
            CheckError(False);
          finally
            Py_DecRef(args);
          end;
        end
        else
          raise EPythonError.Create('Invalid DatetimeConversionMode');
      end;
    varOleStr:
      begin
        if (TVarData(DeRefV).VOleStr = nil) or (TVarData(DeRefV).VOleStr^ = #0) then
          wStr := ''
        else
          wStr := DeRefV;
      {$IFDEF PREFER_UNICODE}
        Result := PyUnicode_FromWideChar( PWideChar(wStr), Length(wStr) );
      {$ELSE}
        s := wStr;
        Result := PyString_FromString( PChar(s) );
      {$ENDIF}
      end;
    varString:
      begin
        s := DeRefV;
        Result := PyString_FromString( PChar(s) );
      end;
  else
    if VarType(DeRefV) and varArray <> 0 then
      begin
        case VarArrayDimCount(DeRefV) of
        1: Result := ArrayVarDim1;
        2: Result := ArrayVarDim2;
        3: Result := ArrayVarDim3;
        else
          raise Exception.Create('Can''t convert a variant array of more than 3 dimensions to a Python sequence');
        end;
      end
    else if VarIsNull(DeRefV) or VarIsEmpty(DeRefV) then
      begin
        Result := ReturnNone;
      end
    else
      try
{$IFDEF DELPHI4_OR_HIGHER}
        Disp := DeRefV;
{$ELSE}
        Disp := IUnknown(DeRefV) as IDispatch;

{$ENDIF}
        wStr := '__asPPyObject__';
        // detect if the variant supports this special property
        if Disp.GetIDsOfNames(GUID_NULL, @wStr, 1, 0, @DispID) = S_OK then
        begin
          {$IFDEF FPC} {./Uncertain}
            myInt := Integer(DeRefV);  //Returns the address to PPyObject as integer. (See impl. in PythonAtom.pas)
          {$ELSE}
          myInt := DeRefV.__asPPyObject__;  //Returns the address to PPyObject as integer. (See impl. in PythonAtom.pas)
          {$ENDIF}
          Result := PPyObject(myInt);
          Py_XIncRef(Result);
        end
        else //If variant don't implement __asPPyObject__, then we have to return nothing.
          Result := ReturnNone;
      except
        // if something went wrong, just return none!
        Result := ReturnNone;
      end; // of try
  end; // of case
end;

function TPythonEngine.PyObjectAsVariant( obj : PPyObject ) : Variant;

  function ExtractDate( var date : Variant ) : Boolean;

    function GetStructMember( obj : PPyObject; const AMember : String ) : Word;
    var
      member : PPyObject;
    begin
      member := PyObject_GetAttrString( obj, PChar(AMember) );
      CheckError(False);
      if PyInt_Check(member) then
        Result := PyInt_AsLong(member)
      else
        raise EPythonError.CreateFmt('Unexpected type found in member %s of a time_struct object', [AMember]);
      Py_XDecRef(member);
    end;

  var
    i, wd, jd, dl : Integer;
    dt : TDateTime;
    y, m, d, h, mi, sec, msec : WORD;
    delta : PPyDateTime_Delta;
  begin
    Result := False;
    if PyTimeStruct_Check( obj ) then
    begin
        y   := GetStructMember( obj, 'tm_year' );
        m   := GetStructMember( obj, 'tm_mon' );
        d   := GetStructMember( obj, 'tm_mday' );
        h   := GetStructMember( obj, 'tm_hour' );
        mi  := GetStructMember( obj, 'tm_min' );
        sec := GetStructMember( obj, 'tm_sec' );
        //wd  := GetStructMember( obj, 'tm_wday' );
        //jd  := GetStructMember( obj, 'tm_yday' );
        //dl  := GetStructMember( obj, 'tm_isdst' );
        dt := EncodeDate( y, m, d ) + EncodeTime( h, mi, sec, 0 );
        Date := dt;
        Result := True;
    end
    else if PyDateTime_Check( obj ) then
    begin
        y   := PyDateTime_GET_YEAR(obj);
        m   := PyDateTime_GET_MONTH(obj);
        d   := PyDateTime_GET_DAY(obj);
        h   := PyDateTime_DATE_GET_HOUR(obj);
        mi  := PyDateTime_DATE_GET_MINUTE(obj);
        sec := PyDateTime_DATE_GET_SECOND(obj);
        msec:= PyDateTime_DATE_GET_MICROSECOND(obj) div 1000;
        dt := EncodeDate( y, m, d ) + EncodeTime( h, mi, sec, msec );
        Date := dt;
        Result := True;
    end
    else if PyDate_Check( obj ) then
    begin
        y   := PyDateTime_GET_YEAR(obj);
        m   := PyDateTime_GET_MONTH(obj);
        d   := PyDateTime_GET_DAY(obj);
        dt  := EncodeDate( y, m, d );
        Date := dt;
        Result := True;
    end
    else if PyTime_Check( obj ) then
    begin
        h   := PyDateTime_TIME_GET_HOUR(obj);
        mi  := PyDateTime_TIME_GET_MINUTE(obj);
        sec := PyDateTime_TIME_GET_SECOND(obj);
        msec:= PyDateTime_TIME_GET_MICROSECOND(obj) div 1000;
        dt  := EncodeTime( h, mi, sec, msec );
        Date := dt;
        Result := True;
    end
    else if PyDelta_Check( obj ) then
    begin
      delta := PPyDateTime_Delta(obj);
      dt := delta^.days + (delta^.seconds / (24*60*60)) + ((delta^.microseconds div 1000) / (24*60*60*1000));
      Date := dt;
      Result := True;
    end
    else if PyTuple_Check( obj ) and (PyTuple_Size(obj) = 9) then
      begin
        for i := 0 to 8 do
          if not PyInt_Check(PyTuple_GetItem(obj, i)) then
            Exit;
        y   := PyInt_AsLong( PyTuple_GetItem(obj, 0) );
        m   := PyInt_AsLong( PyTuple_GetItem(obj, 1) );
        d   := PyInt_AsLong( PyTuple_GetItem(obj, 2) );
        h   := PyInt_AsLong( PyTuple_GetItem(obj, 3) );
        mi  := PyInt_AsLong( PyTuple_GetItem(obj, 4) );
        sec := PyInt_AsLong( PyTuple_GetItem(obj, 5) );
        wd  := PyInt_AsLong( PyTuple_GetItem(obj, 6) );
        jd  := PyInt_AsLong( PyTuple_GetItem(obj, 7) );
        dl  := PyInt_AsLong( PyTuple_GetItem(obj, 8) );
        if not (m   in [1..12]) or
           not (d   in [1..31]) or
           not (h   in [0..23]) or
           not (mi  in [0..59]) or
           not (sec in [0..59]) or
           not (wd  in [0..6]) or
           not ((jd>=0) and (jd<=366)) or
           not ((dl>=-1) and (dl<=1)) then
          Exit;
        try
          dt := EncodeDate( y, m, d );
          dt := dt + EncodeTime( h, mi, sec, 0 );
          Date := dt;
          Result := True;
        except
        end;
      end;
  end;

  function GetSequenceItem( sequence : PPyObject; idx : Integer ) : Variant;
  var
    val : PPyObject;
  begin
    val := PySequence_GetItem( sequence, idx );
    try
      Result := PyObjectAsVariant( val );
    finally
      Py_XDecRef( val );
    end;
  end;

var
  i, seq_length : Integer;
begin
  if PyFloat_Check(obj) then
    Result := PyFloat_AsDouble(obj)
{$IFDEF PYTHON23_OR_HIGHER}
  else if PyBool_Check(obj) then // we must check Bool before Int, as Boolean type inherits from Int.
    Result := PyObject_IsTrue(obj) = 1
{$ENDIF}
{$IFDEF DELPHI6_OR_HIGHER}
  else if PyLong_Check(obj) then
    Result := PyLong_AsLongLong(obj)
{$ENDIF}
  // changed the order of Long and int check (KV)
  else if PyInt_Check(obj) then
    Result := PyInt_AsLong(obj)
{$IFDEF UNICODE_SUPPORT}
  else if PyUnicode_Check(obj) then
    Result := PyUnicode_AsWideString(obj)
{$ENDIF}
  else if PyString_Check(obj) then
    Result := PyObjectAsString(obj)
  else if ExtractDate( Result ) then
    begin
      // Nothing to do
    end
  else if PySequence_Check( obj ) = 1 then
    begin
      seq_length := PySequence_Length( obj );
      // if we have at least one object in the sequence,
      if seq_length > 0 then
        // we try to get the first one, simply to test if the sequence API
        // is really implemented.
        Py_XDecRef( PySequence_GetItem( obj, 0 ) );
      // check if the Python object did really implement the sequence API
      if PyErr_Occurred = nil then
        begin
          // Convert a Python sequence into an array of Variant
          Result := VarArrayCreate( [0, seq_length-1], varVariant );
          for i := 0 to PySequence_Length( obj )-1 do
            Result[i] := GetSequenceItem( obj, i );
        end
      else // the object didn't implement the sequence API, so we return Null
        begin
          PyErr_Clear;
          Result := Null;
        end;
    end
  else
    Result := Null;
end;

function TPythonEngine.VarRecAsPyObject( v : TVarRec ) : PPyObject;
var
  buff : array [0..256] of Char;
begin
  case v.VType of
    vtInteger:       Result := PyInt_FromLong( v.VInteger );
    vtBoolean:       Result := PyInt_FromLong( Integer(v.VBoolean) );
    vtChar:          Result := PyString_FromString( PChar(String(v.VChar)) );
    vtExtended:      Result := PyFloat_FromDouble( v.VExtended^ );
    vtString:
    begin
      if Assigned(v.VString) then
        Result := PyString_FromString( StrPCopy( buff, v.VString^) )
      else
        Result := PyString_FromString( '' );
    end;
    vtPChar:         Result := PyString_FromString( v.VPChar );
    vtAnsiString:
    begin
      if Assigned(v.VAnsiString) then
        Result := PyString_FromString( v.VAnsiString )
      else
        Result := PyString_FromString('');
    end;
    vtCurrency:      Result := PyFloat_FromDouble( v.VCurrency^ );
    vtVariant:       Result := VariantAsPyObject( v.VVariant^ );
    vtPointer:       Result := v.VPointer;
{$IFDEF DELPHI6_OR_HIGHER}
    vtInt64:         Result := PyLong_FromLongLong( v.VInt64^ );
{$ENDIF}
{$IFDEF UNICODE_SUPPORT}
    vtWideChar:      Result := PyUnicode_FromWideString( v.VWideChar );
    vtPWideChar:
    begin
      if Assigned(v.VPWideChar) then
        Result := PyUnicode_FromWideString( WideString(v.VPWideChar) )
      else
        Result := PyUnicode_FromWideString( '' );
    end;
    vtWideString:
    begin
      if Assigned(v.VWideString) then
        Result := PyUnicode_FromWideString( WideString(v.VWideString) )
      else
        Result := PyUnicode_FromWideString( '' );
    end;
{$ENDIF}
  else
    Raise Exception.Create('Argument type not allowed');
  end;
end;

// This function prevents Python from deleting the objects contained
// when the container will be freed, because we increment each
// object's refcount.
function TPythonEngine.MakePyTuple( const objects : array of PPyObject ) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( High(objects)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := Low(objects) to High(objects) do
    begin
      Py_XINCREF( objects[i] );
      PyTuple_SetItem( Result, i, objects[i] );
    end;
end;

// This function prevents Python from deleting the objects contained
// when the container will be freed, because we increment each
// object's refcount.
function TPythonEngine.MakePyList( const objects : array of PPyObject ) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( High(objects)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := Low(objects) to High(objects) do
    begin
      Py_XIncRef( objects[i] );
      PyList_SetItem( Result, i, objects[i] );
    end;
end;

function TPythonEngine.ArrayToPyTuple( items : array of const) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( High(items)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := Low(items) to High(items) do
    PyTuple_SetItem( Result, i, VarRecAsPyObject( items[i] ) );
end;

function TPythonEngine.ArrayToPyList( items : array of const) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( High(items)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := Low(items) to High(items) do
    PyList_SetItem( Result, i, VarRecAsPyObject( items[i] ) );
end;

// You must give each entry as a couple key(string)/value
function TPythonEngine.ArrayToPyDict( items : array of const) : PPyObject;

  function VarRecAsString( v : TVarRec ) : String;
  begin
    case v.VType of
      vtChar:          Result := v.VChar;
      vtString:
      begin
        if Assigned(v.VString) then
          Result := v.VString^
        else
          Result := '';
      end;
      vtPChar:
      begin
        Result := v.VPChar;
      end;
      vtAnsiString:
      begin
        if Assigned(v.VAnsiString) then
          Result := StrPas(v.VAnsiString)
        else
          Result := '';
      end;
      vtVariant:
      begin
        if Assigned(v.VVariant) then
          Result := v.VVariant^
        else
          Result := '';
      end;
    else
      Raise Exception.Create('Argument type not allowed');
    end;
  end;

var
  i : Integer;
  s : String;
  obj : PPyObject;
begin
  if ((High(items)+1) mod 2) <> 0 then
    raise Exception.Create('You must provide an even number of arguments');
  Result := PyDict_New;
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new dict object');
  i := Low(items);
  try
    while i <= High(items) do
      begin
        s := VarRecAsString( items[i] );
        obj := VarRecAsPyObject( items[i+1] );
        if s = '' then
          PyDict_SetItemString( Result, '', obj )
        else
          PyDict_SetItemString( Result, PChar(s), obj );
        Py_XDecRef(obj);
        Inc( i, 2 );
      end;
  except
    Py_XDECREF( Result );
  end;
end;

function TPythonEngine.StringsToPyList( strings : TStrings ) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( strings.Count );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := 0 to strings.Count - 1 do
    PyList_SetItem( Result, i, PyString_FromString( PChar(strings.Strings[i]) ) );
end;

function TPythonEngine.StringsToPyTuple( strings : TStrings ) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( strings.Count );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := 0 to strings.Count - 1 do
    PyTuple_SetItem( Result, i, PyString_FromString( PChar(strings.Strings[i]) ) );
end;

procedure TPythonEngine.PyListToStrings( list : PPyObject; strings : TStrings );
var
  i : Integer;
begin
  if not PyList_Check(list) then
    raise EPythonError.Create('the python object is not a list');
  strings.Clear;
  for i := 0 to PyList_Size( list ) - 1 do
    strings.Add( PyObjectAsString( PyList_GetItem( list, i ) ) );
end;

procedure TPythonEngine.PyTupleToStrings( tuple: PPyObject; strings : TStrings );
var
  i : Integer;
begin
  if not PyTuple_Check(tuple) then
    raise EPythonError.Create('the python object is not a tuple');
  strings.Clear;
  for i := 0 to PyTuple_Size( tuple ) - 1 do
    strings.Add( PyObjectAsString( PyTuple_GetItem( tuple, i ) ) );
end;

{$IFDEF UNICODE_SUPPORT}
function TPythonEngine.PyUnicode_AsWideString( obj : PPyObject ) : WideString;
var
  _size : Integer;
{$IFDEF LINUX}
  _ucs4Str : UCS4String;
{$ENDIF}
begin
  if PyUnicode_Check(obj) then
  begin
    _size := PySequence_Length(obj);
    if _size > 0 then
    begin
{$IFDEF LINUX}
      // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
      SetLength(_ucs4Str, _size+1);
      if PyUnicode_AsWideChar(obj, @_ucs4Str[0], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
      Result := UCS4StringToWideString(_ucs4Str);
      // remove trailing zeros (needed by Kylix1)
      while (Length(Result) > 0) and (Result[Length(Result)] = #0) do
        Delete(Result, Length(Result), 1);
{$ELSE}
      SetLength(Result, _size);
      if PyUnicode_AsWideChar(obj, @Result[1], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
{$ENDIF}
    end
    else
      Result := '';
  end
  else
    raise EPythonError.Create('PyUnicode_AsWideString expects a Unicode Python object');
end;

function TPythonEngine.PyUnicode_FromWideString( const AString : WideString) : PPyObject;
{$IFDEF LINUX}
var
  _ucs4Str : UCS4String;
{$ENDIF}
begin
{$IFDEF LINUX}
  // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
  _ucs4Str := WideStringToUCS4String(AString);
  Result := PyUnicode_FromWideChar( {PWideChar}(@_ucs4Str[0]), Length(AString) );
{$ELSE}
  Result := PyUnicode_FromWideChar( PWideChar(AString), Length(AString) );
{$ENDIF}
end;
{$ENDIF}

function TPythonEngine.ReturnNone : PPyObject;
begin
  Result := Py_None;
  Py_INCREF( Result );
end;

function TPythonEngine.FindModule( const ModuleName : String ) : PPyObject;
var
  modules, m : PPyObject;
begin
  modules := PyImport_GetModuleDict;
  m := PyDict_GetItemString(modules, PChar(ModuleName) );
  if (m <> nil) and (PyModule_Check(m)) then
    Result := m
  else
    Result := nil;
end;

function TPythonEngine.FindFunction(ModuleName,FuncName: String): PPyObject;
var
  module,func: PPyObject;
begin
  module := FindModule(ModuleName);
  if module = nil then result := nil
  else begin
    func := PyObject_GetAttrString(module, PChar(FuncName));
    if Assigned(func) then begin
       if PyFunction_Check(func) then
         Result := func
       else
       begin
         Py_XDecRef(func);
         Result := nil;
       end;
    end else begin
      Result := nil;
      PyErr_Clear;
    end;
  end;
end;

function TPythonEngine.SetToList( data : Pointer; size : Integer ) : PPyObject;

  function GetBit( idx : Integer ) : Boolean;
  var
    tmp : PChar;
  begin
    if idx >= size*8 then
      begin
        Result := False;
        Exit;
      end;
    tmp := PChar(data);
    tmp := tmp + (idx div 8);
    Result := (Ord(tmp^) and (1 shl (idx mod 8))) <> 0;
  end;

var
  i, cpt : Integer;
begin
  cpt := 0;
  for i := 0 to size*8-1 do
    if GetBit(i) then
      Inc(cpt);
  Result := PyList_New( cpt );
  cpt := 0;
  for i := 0 to size*8-1 do
    if GetBit(i) then
      begin
        PyList_SetItem( Result, cpt, PyInt_FromLong(i) );
        Inc(cpt);
      end;
end;

procedure TPythonEngine.ListToSet( List : PPyObject; data : Pointer; size : Integer );

  procedure SetBit( idx : Integer );
  var
    tmp : PChar;
  begin
    if idx >= size*8 then
      Exit;
    tmp := PChar(data);
    tmp := tmp + (idx div 8);
    tmp^ := Chr((Ord(tmp^) or (1 shl (idx mod 8))));
  end;

var
  i : Integer;
begin
  FillChar( PChar(data)^, size, #0 );
  for i := 0 to PyList_Size(list)-1 do
    SetBit( PyObjectAsVariant( PyList_GetItem(list, i) ) );
end;

procedure TPythonEngine.CheckError(ACatchStopEx : Boolean {$IFDEF DELPHI4_OR_HIGHER}= False{$ENDIF});
begin
  if PyErr_Occurred <> nil then
  begin
{$IFDEF PYTHON22_OR_HIGHER}
    if ACatchStopEx and (PyErr_GivenExceptionMatches(PyErr_Occurred, PyExc_StopIteration^) <> 0) then
    begin
      PyErr_Clear;
      raise EPyStopIteration.Create('Stop iteration');
    end
    else
{$ENDIF}
    begin
      PyErr_Print;
      Traceback.Refresh;
      RaiseError;
    end;
  end;
end;

function TPythonEngine.GetMainModule : PPyObject;
begin
  Result := PyImport_AddModule(PChar(ExecModule));
end;

function TPythonEngine.PyTimeStruct_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FTimeStruct) and (Pointer(obj^.ob_type) = FTimeStruct);
end;

function TPythonEngine.PyDate_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DateType));
end;

function TPythonEngine.PyDate_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DateType);
end;

function TPythonEngine.PyDateTime_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DateTimeType));
end;

function TPythonEngine.PyDateTime_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DateTimeType);
end;

function TPythonEngine.PyTime_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_TimeType));
end;

function TPythonEngine.PyTime_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_TimeType);
end;

function TPythonEngine.PyDelta_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DeltaType));
end;

function TPythonEngine.PyDelta_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DeltaType);
end;

function TPythonEngine.PyTZInfo_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_TZInfoType));
end;

function TPythonEngine.PyTZInfo_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_TZInfoType);
end;

function TPythonEngine.PyDateTime_GET_YEAR( obj : PPyObject ) : Integer;
begin
  Result := (PPyDateTime_Date(obj)^.data[0] Shl 8) or
            PPyDateTime_Date(obj)^.data[1];
end;

function TPythonEngine.PyDateTime_GET_MONTH( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_Date(obj)^.data[2];
end;

function TPythonEngine.PyDateTime_GET_DAY( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_Date(obj)^.data[3];
end;

function TPythonEngine.PyDateTime_DATE_GET_HOUR( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_DateTime(obj)^.data[4];
end;

function TPythonEngine.PyDateTime_DATE_GET_MINUTE( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_DateTime(obj)^.data[5];
end;

function TPythonEngine.PyDateTime_DATE_GET_SECOND( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_DateTime(obj)^.data[6];
end;

function TPythonEngine.PyDateTime_DATE_GET_MICROSECOND( obj : PPyObject ) : Integer;
begin
	Result := (PPyDateTime_DateTime(obj)^.data[7] Shl 16) or
            (PPyDateTime_DateTime(obj)^.data[8] Shl 8)  or
            PPyDateTime_DateTime(obj)^.data[9];
end;

function TPythonEngine.PyDateTime_TIME_GET_HOUR( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_Time(obj)^.data[0];
end;

function TPythonEngine.PyDateTime_TIME_GET_MINUTE( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_Time(obj)^.data[1];
end;

function TPythonEngine.PyDateTime_TIME_GET_SECOND( obj : PPyObject ) : Integer;
begin
  Result := PPyDateTime_Time(obj)^.data[2];
end;

function TPythonEngine.PyDateTime_TIME_GET_MICROSECOND( obj : PPyObject ) : Integer;
begin
	Result := (PPyDateTime_Time(obj)^.data[3] Shl 16) or
            (PPyDateTime_Time(obj)^.data[4] Shl 8)  or
            PPyDateTime_Time(obj)^.data[5];
end;

function TPythonEngine.GetVersion: String;
begin
  Result := '3.32';
end;

procedure TPythonEngine.SetVersion(const Value: String);
begin
  // do nothing
end;

function TPythonEngine.PyString_AsDelphiString(ob: PPyObject): string;
begin
  if PyUnicode_Check(ob) then
    Result := PyUnicode_AsWideString(ob)
  else
    Result := PyString_AsString(ob);
end;

function TPythonEngine.PyString_FromString( str: PChar): PPyObject;
var
  _text : WideString;
begin
  if IsPython3000 then
  begin
    _text := str;
    Result := PyUnicode_FromWideString(_text);
  end
  else
    Result := DLL_PyString_FromString(str);
end;



(*******************************************************)
(**                                                   **)
(**     class TEngineClient                           **)
(**                                                   **)
(*******************************************************)

procedure  TEngineClient.SetEngine( val : TPythonEngine );
begin
  if val <> FEngine then
    begin
      if Assigned(FEngine) {and not(csDesigning in ComponentState)} then
        FEngine.RemoveClient( Self );
      FEngine := val;
      if Assigned(FEngine) {and not(csDesigning in ComponentState)} then
        FEngine.AddClient( Self );
    end;
end;

procedure TEngineClient.ModuleReady(Sender : TObject);
begin
end;

constructor TEngineClient.Create( AOwner : TComponent );
var
  i : Integer;
begin
  inherited;
  if (csDesigning in ComponentState) and Assigned(AOwner) then
    with AOwner do
      for i := 0 to ComponentCount - 1 do
        if Components[i] is TPythonEngine then
          begin
            Self.Engine := TPythonEngine(Components[i]);
            Break;
          end;
end;

destructor TEngineClient.Destroy;
begin
  Engine := nil; // This detaches the client from the Engine.
  if Assigned( FOnDestroy ) then
    FOnDestroy( Self );
  inherited;
end;

procedure TEngineClient.Loaded;
begin
  inherited;
  if Assigned( FOnCreate ) then
    FOnCreate( Self );
end;

procedure TEngineClient.Notification( AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FEngine then
      FEngine := nil;
end;

procedure  TEngineClient.Initialize;
begin
  if FInitialized then
    Exit;
  if Assigned( FOnInitialization ) then
     FOnInitialization( Self );
  FInitialized := True;
end;

procedure TEngineClient.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned( FOnFinalization ) then
     FOnFinalization( Self );
  FInitialized := False;
end;

procedure  TEngineClient.ClearEngine;
begin
  FEngine := nil;
end;

procedure  TEngineClient.CheckEngine;
begin
  if not Assigned(FEngine) then
    raise Exception.CreateFmt('No Engine defined for component "%s"', [Name]);
end;


(*******************************************************)
(**                                                   **)
(**     class TMethodsContainer                       **)
(**                                                   **)
(*******************************************************)

////////////////////////////////////////
// class TEventDef

constructor TEventDef.Create(ACollection: TCollection);
begin
  inherited;
  FDocString := TStringList.Create;
  Name := Format('PythonEvent%d',[Collection.Count - 1]);
end;

destructor TEventDef.Destroy;
begin
  FDocString.Free;
  inherited;
end;

function TEventDef.GetDisplayName: string;
begin
  Result := FName;
end;

function TEventDef.GetDocString : String;
begin
  Owner.Container.CheckEngine;
  FTmpDocString := Owner.Container.Engine.CleanString(FDocString.Text);
  Result := fTmpDocString;
end;

function TEventDef.PythonEvent(pself,	args: PPyObject): PPyObject;
begin
  Owner.Container.CheckEngine;
  with Owner.Container.Engine do
  begin
    if Assigned(fOnExecute) then
      fOnExecute(Self, pself, args, Result);
  end;
end;

function  TEventDef.Owner : TEventDefs;
begin
  Result := Collection as TEventDefs;
end;

procedure TEventDef.SetDisplayName(const Value: String);
begin
  FName := Value;
  inherited;
end;

procedure TEventDef.Assign(Source: TPersistent);
begin
  if Source is TEventDef then
    begin
      Name := TEventDef(Source).Name;
      DocString := TEventDef(Source).DocString;
    end
  else
    inherited Assign(Source);
end;

procedure TEventDef.SetDocString(const Value: TStringList);
begin
	FDocString.Assign(Value);
end;

////////////////////////////////////////
// class TEventDefs

constructor TEventDefs.Create(AMethodsContainer : TMethodsContainer );
begin
  inherited Create(TEventDef);
  FMethodsContainer := AMethodsContainer;
end;

function TEventDefs.GetItems( idx : Integer ) : TEventDef;
begin
  Result := TEventDef(inherited GetItem(idx));
end;

procedure TEventDefs.SetItems( idx : Integer; Value : TEventDef );
begin
  inherited SetItem( idx, Value );
end;

function  TEventDefs.GetOwner: TPersistent;
begin
  Result := FMethodsContainer;
end;

function TEventDefs.Add : TEventDef;
begin
  Result := TEventDef(inherited Add);
end;

procedure TEventDefs.RegisterEvents;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      FMethodsContainer.AddDelphiMethod(PChar(Name), PythonEvent, PChar(GetDocString));
end;

////////////////////////////////////////
// class TMethodsContainer

procedure TMethodsContainer.AllocMethods;
begin
  Assert(FMethods = nil);
  FAllocatedMethodCount := PYT_METHOD_BUFFER_INCREASE;
  FMethodCount := 0;
  FMethods := PPyMethodDef(AllocMem(SizeOf(PyMethodDef)*(FAllocatedMethodCount+1)));
end;

procedure TMethodsContainer.FreeMethods;
begin
  if Assigned(FMethods) then
  begin
    FreeMem(FMethods);
    FMethods := nil;
  end;
  FAllocatedMethodCount := 0;
  FMethodCount := 0;
end;

procedure TMethodsContainer.ReallocMethods;
var
  MethodPtr : PPyMethodDef;
begin
  Inc( FAllocatedMethodCount, PYT_METHOD_BUFFER_INCREASE );
  ReAllocMem( FMethods, SizeOf(PyMethodDef)*(FAllocatedMethodCount+1));
  MethodPtr :=@(PMethodArray(FMethods)^[MethodCount+1]);
  FillChar( MethodPtr^,SizeOf(PyMethodDef)*PYT_METHOD_BUFFER_INCREASE,0);
end;

function TMethodsContainer.GetMethods( idx : Integer ) : PPyMethodDef;
begin
  if (idx < 0) or (idx > MethodCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PMethodArray(FMethods)^[idx] );
end;

function TMethodsContainer.StoreEventDefs: Boolean;
begin
  Result := (FEventDefs <> nil) and (FEventDefs.Count > 0);
end;

constructor TMethodsContainer.Create( AOwner : TComponent );
begin
  inherited;
  AllocMethods;
  fEventDefs := TEventDefs.Create(Self);
end;

destructor  TMethodsContainer.Destroy;
begin
  fEventDefs.Free;
  fEventDefs := nil;
  FreeMethods;
  inherited;
end;

procedure TMethodsContainer.Initialize;
begin
  inherited;
  Events.RegisterEvents;
end;

procedure TMethodsContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearMethods;
  inherited;
end;

function TMethodsContainer.AddMethod( AMethodName  : PChar;
                                      AMethod  : PyCFunction;
                                      ADocString : PChar ) : PPyMethodDef;
begin
  if FMethodCount = FAllocatedMethodCount then
    ReallocMethods;
  Result := Methods[ MethodCount ];
  Result^.ml_name  := AMethodName;
  Result^.ml_meth  := AMethod;
  Result^.ml_flags := METH_VARARGS;
  Result^.ml_doc   := ADocString;
  Inc( FMethodCount );
end;

function  TMethodsContainer.AddMethodWithKeywords( AMethodName  : PChar;
                                                   AMethod  : PyCFunctionWithKW;
                                                   ADocString : PChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       PyCFunction(AMethod),
                       ADocString );
  Result^.ml_flags := Result^.ml_flags or METH_KEYWORDS;
end;

function  TMethodsContainer.AddDelphiMethod( AMethodName  : PChar;
                                             ADelphiMethod: TDelphiMethod;
                                             ADocString : PChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       GetOfObjectCallBack( TCallBack(ADelphiMethod), 2, ctCDECL),
                       ADocString );
end;

function  TMethodsContainer.AddDelphiMethodWithKeywords(  AMethodName  : PChar;
                                                          ADelphiMethod: TDelphiMethodWithKW;
                                                          ADocString : PChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       GetOfObjectCallBack( TCallBack(ADelphiMethod), 3, ctCDECL),
                       ADocString );
  Result^.ml_flags := Result^.ml_flags or METH_KEYWORDS;
end;

procedure TMethodsContainer.ClearMethods;
begin
  FMethodCount := 0;
  FillChar(FMethods^, Sizeof(FMethods^)*FAllocatedMethodCount, 0);
end;

////////////////////////////////////////
// class TMembersContainer

function  TMembersContainer.GetMembersStartOffset : Integer;
begin
  Result := 0;
end;

{$IFDEF PYTHON22_OR_HIGHER}
procedure TMembersContainer.AddMember(MemberName: PChar; MemberType : TPyMemberType;
  MemberOffset : Integer; MemberFlags: TPyMemberFlag; MemberDoc: PChar);
begin
  if FMemberCount = FAllocatedMemberCount then
    ReallocMembers;
  with Members[ MemberCount ]^ do
    begin
      name      := MemberName;
      case MemberType of
      mtShort:          _type := T_Short;
      mtInt:            _type := T_Int;
      mtLong:           _type := T_Long;
      mtFloat:          _type := T_Float;
      mtDouble:         _type := T_Double;
      mtString:         _type := T_String;
      mtObject:         _type := T_Object;
      mtChar:           _type := T_Char;
      mtByte:           _type := T_Byte;
      mtUByte:          _type := T_UByte;
      mtUShort:         _type := T_UShort;
      mtUInt:           _type := T_UInt;
      mtULong:          _type := T_ULong;
      mtStringInplace:  _type := T_STRING_INPLACE;
      mtObjectEx:       _type := T_OBJECT_EX;
      else
        raise Exception.Create('Unknown member type');
      end;
      offset    := MemberOffset + GetMembersStartOffset;
      case MemberFlags of
      mfDefault:                flags := 0;
      mfReadOnly:               flags := READONLY;
      mfReadRestricted:         flags := READ_RESTRICTED;
      mfWriteRestricted:        flags := WRITE_RESTRICTED;
      mfRestricted:             flags := RESTRICTED;
      else
        raise Exception.Create('Unknown member flag');
      end;
      doc       := MemberDoc;
    end;
  Inc( FMemberCount );
end;

procedure TMembersContainer.AllocMembers;
begin
  FAllocatedMemberCount := PYT_MEMBER_BUFFER_INCREASE;
  Assert(not Assigned(FMembers));
  FMembers := PPyMemberDef(AllocMem(SizeOf(PyMemberDef)*(FAllocatedMemberCount+1)));
end;

procedure TMembersContainer.ClearMembers;
begin
  FMemberCount := 0;
  FillChar(FMembers^, Sizeof(FMembers^)*FAllocatedMemberCount, 0);
end;

constructor TMembersContainer.Create(AOwner: TComponent);
begin
  inherited;
  AllocMembers;
end;

destructor TMembersContainer.Destroy;
begin
  FreeMembers;
  inherited;
end;

procedure TMembersContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearMembers;
  inherited;
end;

procedure TMembersContainer.FreeMembers;
begin
  if Assigned(FMembers) then
  begin
    FreeMem(FMembers);
    FMembers := nil;
  end;
  FMemberCount := 0;
  FAllocatedMemberCount := 0;
end;

function TMembersContainer.GetMembers(idx: Integer): PPyMemberDef;
begin
  if (idx < 0) or (idx > MemberCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PMemberArray(FMembers)^[idx] );
end;

procedure TMembersContainer.ReallocMembers;
var
  MemberPtr : PPyMemberDef;
begin
  Inc( FAllocatedMemberCount, PYT_MEMBER_BUFFER_INCREASE );
  ReAllocMem( FMembers, SizeOf(PyMemberDef)*(FAllocatedMemberCount+1));
  MemberPtr :=@(PMemberArray(FMembers)^[MemberCount+1]);
  FillChar( MemberPtr^,SizeOf(PyMemberDef)*PYT_MEMBER_BUFFER_INCREASE,0);
end;
{$ENDIF}

////////////////////////////////////////
// class TGetSetContainer

{$IFDEF PYTHON22_OR_HIGHER}

procedure TGetSetContainer.AddGetSet(AName: PChar; AGet: getter;
  ASet: setter; ADoc: PChar; AClosure: Pointer);
begin
  if FGetSetCount = FAllocatedGetSetCount then
    ReallocGetSets;
  with GetSet[ GetSetCount ]^ do
    begin
      name      := AName;
      get       := AGet;
      _set      := ASet;
      doc       := ADoc;
      closure   := AClosure;
    end;
  Inc( FGetSetCount );
end;

procedure TGetSetContainer.AllocGetSets;
begin
  FAllocatedGetSetCount := PYT_GETSET_BUFFER_INCREASE;
  Assert(not Assigned(FGetSets));
  FGetSets := PPyGetSetDef(AllocMem(SizeOf(PyGetSetDef)*(FAllocatedGetSetCount+1)));
end;

procedure TGetSetContainer.ClearGetSets;
begin
  FGetSetCount := 0;
  FillChar(FGetSets^, Sizeof(FGetSets^)*FAllocatedGetSetCount, 0);
end;

constructor TGetSetContainer.Create(AOwner: TComponent);
begin
  inherited;
  AllocGetSets;
end;

destructor TGetSetContainer.Destroy;
begin
  FreeGetSets;
  inherited;
end;

procedure TGetSetContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearGetSets;
  inherited;
end;

procedure TGetSetContainer.FreeGetSets;
begin
  if Assigned(FGetSets) then
  begin
    FreeMem(FGetSets);
    FGetSets := nil;
  end;
  FGetSetCount := 0;
  FAllocatedGetSetCount := 0;
end;

function TGetSetContainer.GetGetSet(idx: Integer): PPyGetSetDef;
begin
  if (idx < 0) or (idx > GetSetCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PGetSetArray(FGetSets)^[idx] );
end;

procedure TGetSetContainer.ReallocGetSets;
var
  GetSetPtr : PPyGetSetDef;
begin
  Inc( FAllocatedGetSetCount, PYT_GETSET_BUFFER_INCREASE );
  ReAllocMem( FGetSets, SizeOf(PyGetSetDef)*(FAllocatedGetSetCount+1));
  GetSetPtr :=@(PGetSetArray(FGetSets)^[GetSetCount+1]);
  FillChar( GetSetPtr^,SizeOf(PyGetSetDef)*PYT_GETSET_BUFFER_INCREASE,0);
end;
{$ENDIF}

(*******************************************************)
(**                                                   **)
(**     class TPythonModule                           **)
(**                                                   **)
(*******************************************************)

////////////////////////////////////////
// class TParentClassError

procedure TParentClassError.AssignTo( Dest: TPersistent );
begin
  if Dest is TParentClassError then
     with TParentClassError( Dest ) do
       begin
         FName   := Self.FName;
         FModule := Self.FModule;
       end;
  inherited;
end;

////////////////////////////////////////
// class TError

function TError.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TError.SetName( const Value : String );

  procedure CheckName;
  var
    i : Integer;
    m : TPythonModule;
  begin
    with Collection as TErrors do
      begin
        if GetOwner = nil then
          Exit;
        m := GetOwner as TPythonModule;
        for i := 0 to Count - 1 do
          with Items[i] do
            if Name = Value then
              raise Exception.CreateFmt( 'In module "%s", there''s already an error named "%s"',
                                         [m.ModuleName, Value]);
      end;
  end;

  procedure UpdateDependencies;
  var
    i, j : Integer;
    m : TPythonModule;
  begin
    if FName = '' then
      Exit;
    with Collection as TErrors do
      with GetOwner as TPythonModule do
        begin
          if not Assigned(Engine) then
            Exit;
          m := TPythonModule( TErrors(Self.Collection).GetOwner );
          with Engine do
            begin
              for i := 0 to ClientCount - 1 do
                if Clients[i] is TPythonModule then
                  with TPythonModule(Clients[i]) do
                    begin
                      for j := 0 to Errors.Count - 1 do
                        with Errors.Items[j] do
                          if (ParentClass.Module = m.ModuleName) and
                             (ParentClass.Name = Self.Name) then
                            ParentClass.Name := Value;
                    end;
            end;
        end;
  end;

begin
  if (FName <> Value) and (Value <> '') then
  begin
    CheckName;
    if ErrorType = etClass then
      UpdateDependencies;
    FName := Value;
    Changed(False);
  end;
end;

procedure TError.SetText( const Value : String );
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure TError.SetErrorType( Value : TErrorType );
begin
  if FErrorType <> Value then
  begin
    FErrorType := Value;
    if FErrorType = etString then
      FParentClass.Name := '';
    Changed(False);
  end;
end;

procedure TError.SetParentClass( Value : TParentClassError );
begin
  FParentClass.Assign( Value );
  Changed(False);
end;

constructor TError.Create(ACollection: TCollection);
begin
  inherited;
  FErrorType := etString;
  FParentClass := TParentClassError.Create;
end;

destructor TError.Destroy;
begin
  FParentClass.Free;
  inherited;
end;

procedure TError.Assign(Source: TPersistent);
begin
  if Source is TError then
  begin
    Name := TError(Source).Name;
    Text := TError(Source).Text;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TError.BuildError( const ModuleName : String );

  function FindParentClass : PPyObject;
  var
    m, d : PPyObject;
  begin
    Owner.Owner.CheckEngine;
    with Owner.Owner.Engine do
      begin
        if ParentClass.Module <> '' then
          //m := PyImport_ImportModule( PChar(ParentClass.Module) )
          m := PyImport_AddModule( PChar(ParentClass.Module) )
        else
          m := FindModule( ModuleName );
        if not Assigned(m) then
          raise Exception.CreateFmt('Could not find module containing the parent class of error "%s"', [Self.Name]);
        d := PyModule_GetDict(m);
        Result := PyDict_GetItemString( d, PChar(ParentClass.Name) );
        if not Assigned(Result) then
          raise Exception.CreateFmt('Could not find the parent class "%s" of error "%s"', [ParentClass.Name, Self.Name]);
        if not PyClass_Check( Result ) and not PyType_CheckExact( Result ) then
          raise Exception.CreateFmt('The object "%s" in module "%s" is not a class', [ParentClass.Name, ParentClass.Module] );
      end;
  end;

var
  parent : PPyObject;
begin
  if Assigned(Error) then
    Exit;
  if Name = '' then
    with GetOwner as TPythonModule do
      raise Exception.CreateFmt( 'Error without name in module "%s"', [ModuleName] );
  if Text = '' then
    Text := Name;
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    begin
      if ErrorType = etString then
        Error := PyString_FromString( PChar(Text) )
      else if ErrorType = etClass then
        begin
          if FParentClass.Name <> '' then
            parent := FindParentClass
          else
            parent := nil;
          Error := PyErr_NewException( PChar(Format('%s.%s', [ModuleName, Self.Name])),
                                       parent, nil );
        end;
    end;
  if not Assigned(Error) then
    raise Exception.CreateFmt( 'Could not create error "%s"', [Name] );
end;

procedure TError.RaiseError( const msg : String );
begin
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    PyErr_SetString( Error, PChar(msg) );
end;

procedure TError.RaiseErrorObj( const msg : String; obj : PPyObject );
var
  args, res, str : PPyObject;
  inst : PPyInstanceObject;
{$IFDEF PYTHON23_OR_HIGHER}
  i : Integer;
  keys : PPyObject;
  key : PPyObject;
  val : PPyObject;
{$ENDIF}
begin
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    // if we give a dictionary as argument, then we use it for the
    // instance.
    if PyDict_Check( obj ) then
      begin
        args := PyTuple_New(0);
        if not Assigned(args) then
          raise Exception.Create('TError.RaiseErrorObj: Could not create an empty tuple');
        res := PyEval_CallObject(Error, args);
        Py_DECREF(args);
        if not Assigned(res) then
          raise Exception.CreateFmt('TError.RaiseErrorObj: Could not create an instance of "%s"', [Self.Name]);
        if PyInstance_Check( res ) then
          begin
            inst := PPyInstanceObject(res);
            Py_XDECREF( inst^.in_dict );
            inst^.in_dict := obj;
            str := PyString_FromString( PChar(msg) );
            PyDict_SetItemString( obj, 'args', str );
            Py_XDecRef(str);
          end
{$IFDEF PYTHON23_OR_HIGHER}
        else if PyObject_TypeCheck(res, PPyTypeObject(PyExc_Exception^)) then
          begin
            args := PyTuple_New(1);
            if not Assigned(args) then
              raise Exception.Create('TError.RaiseErrorObj: Could not create an empty tuple');
            str := PyString_FromString( PChar(msg) );
            PyTuple_SetItem(args, 0, str);
            res := PyEval_CallObject(Error, args);
            Py_DECREF(args);
            if not Assigned(res) then
              raise Exception.CreateFmt('TError.RaiseErrorObj: Could not create an instance of "%s"', [Self.Name]);
            keys := PyDict_Keys(obj);
            for i := 0 to PySequence_Length(keys)-1 do
            begin
              key := PySequence_GetItem(keys, i);
              val := PyDict_GetItem(obj, key);
              if Assigned(val) then
              begin
                PyObject_SetAttr(res, key, val);
                Py_DECREF(val);
              end;
              Py_XDECREF(key);
            end;
            Py_XDECREF(keys);
          end
{$ENDIF}
        else
          raise Exception.Create('TError.RaiseErrorObj: I didn''t get an instance' );
        PyErr_SetObject( Error, res );
      end
    else
      PyErr_SetObject( Error, obj );
end;

function  TError.Owner : TErrors;
begin
  Result := GetOwner as TErrors;
end;

////////////////////////////////////////
// class TErrors

function TErrors.GetError(Index: Integer): TError;
begin
  Result := TError(inherited GetItem(Index));
end;

procedure TErrors.SetError(Index: Integer; Value: TError);
begin
  inherited SetItem(Index, Value);
end;

function TErrors.GetOwner: TPersistent;
begin
  Result := FModule;
end;

procedure TErrors.Update(Item: TCollectionItem);
begin
  inherited;
end;

constructor TErrors.Create(Module: TPythonModule );
begin
  inherited Create( TError );
  FModule := Module;
end;

function  TErrors.Add: TError;
begin
  Result := TError(inherited Add);
end;

function  TErrors.Owner : TPythonModule;
begin
  Result := GetOwner as TPythonModule;
end;

////////////////////////////////////////
// class TPythonModule

function TPythonModule.GetClientCount : Integer;
begin
  Result := FClients.Count;
end;

function TPythonModule.GetClients( idx : Integer ) : TEngineClient;
begin
  Result := TEngineClient(FClients.Items[idx]);
end;

procedure TPythonModule.SetErrors( val : TErrors );
begin
  FErrors.Assign( val );
end;

procedure TPythonModule.SetModuleName( const val : String );

  procedure UpdateDependencies;
  var
    i, j : Integer;
  begin
    if not Assigned(Engine) then
      Exit;
    if FModuleName = '' then
      Exit;
    with Engine do
      for i := 0 to ClientCount - 1 do
        if Clients[i] is TPythonModule then
          with TPythonModule(Clients[i]) do
            for j := 0 to Errors.Count - 1 do
              with Errors.Items[j] do
                if ParentClass.Module = Self.FModuleName then
                  ParentClass.Module := val;
  end;

begin
  if (FModuleName <> val) and (val <> '') then
    begin
      UpdateDependencies;
      FModuleName := val;
    end;
end;

constructor TPythonModule.Create( AOwner : TComponent );
begin
  inherited;
  FClients := TList.Create;
  FErrors  := TErrors.Create(Self);
  FDocString := TStringList.Create;
end;

destructor  TPythonModule.Destroy;
begin
  FDocString.Free;
  FClients.Free;
  FErrors.Free;
  inherited;
end;


procedure TPythonModule.SetDocString( value : TStringList );
begin
  FDocString.Assign( value );
end;

procedure TPythonModule.DefineDocString;
var
  doc : PPyObject;
begin
  with Engine do
    begin
      if DocString.Text <> '' then
        begin
          doc := PyString_FromString( PChar(CleanString(FDocString.Text)) );
          PyObject_SetAttrString( FModule, '__doc__', doc );
          Py_XDecRef(doc);
          CheckError(False);
        end;
    end;
end;

procedure TPythonModule.MakeModule;
begin
  CheckEngine;
  if Assigned(FModule) then
    Exit;
  with Engine do
    begin
      FModule := Py_InitModule( PChar(ModuleName), MethodsData );
      DefineDocString;
    end;
end;

procedure TPythonModule.Initialize;
var
  i : Integer;
begin
  inherited;
  FModule := nil;
  MakeModule;
  for i := 0 to ClientCount - 1 do
    Clients[i].ModuleReady(Self);
  BuildErrors;
  if Assigned(FOnAfterInitialization) then
    FOnAfterInitialization( Self );
end;

procedure TPythonModule.InitializeForNewInterpreter;
var
  initialized : Boolean;
  oldModule : PPyObject;
begin
  initialized := FInitialized;
  oldModule := FModule;
  FModule := nil;
  FInitialized := False;
  try
    Initialize;
  finally
    FInitialized := initialized;
    FModule := oldModule;
  end;
end;

procedure TPythonModule.AddClient( client : TEngineClient );
begin
  FClients.Add( client );
end;

function TPythonModule.ErrorByName( const AName : String ) : TError;
var
  i : Integer;
begin
  for i := 0 to Errors.Count - 1 do
    if CompareText( Errors.Items[i].Name, AName ) = 0 then
      begin
        Result := Errors.Items[i];
        Exit;
      end;
  raise Exception.CreateFmt( 'Could not find error "%s"', [AName] );
end;

procedure TPythonModule.RaiseError( const error, msg : String );
begin
  ErrorByName( error ).RaiseError( msg );
end;

procedure TPythonModule.RaiseErrorFmt( const error, format : String; Args : array of const );
begin
  RaiseError( error, SysUtils.Format( format, Args ) );
end;

procedure TPythonModule.RaiseErrorObj( const error, msg : String; obj : PPyObject );
begin
  ErrorByName( error ).RaiseErrorObj( msg, obj );
end;

procedure TPythonModule.BuildErrors;
var
  i : Integer;
  d : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      d := PyModule_GetDict( Module );
      if not Assigned(d) then
        Exit;
      for i := 0 to Errors.Count - 1 do
        with Errors.Items[i] do
          begin
            BuildError( ModuleName );
            PyDict_SetItemString( d, PChar(Name), Error );
          end;
    end;
end;

// warning, this function will increase the refcount of value,
// so, if you don't want to keep a link, don't forget to decrement
// the refcount after the SetVar method.
procedure TPythonModule.SetVar( const varName : String; value : PPyObject );
begin
  if Assigned(FEngine) and Assigned( FModule ) then
    begin
      if Engine.PyObject_SetAttrString(Module, PChar(varName), value ) <> 0 then
        raise EPythonError.CreateFmt( 'Could not set var "%s" in module "%s"', [varName, ModuleName] );
    end
  else
    raise EPythonError.CreateFmt( 'Can''t set var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

// warning, this function will increase the refcount of value,
// so, if you don't want to keep a link, don't forget to decrement
// the refcount after the GetVar method.
function  TPythonModule.GetVar( const varName : String ) : PPyObject;
begin
  if Assigned(FEngine) and Assigned( FModule ) then
  begin
    Result := Engine.PyObject_GetAttrString(Module, PChar(varName) );
    Engine.PyErr_Clear;
  end
  else
    raise EPythonError.CreateFmt( 'Can''t get var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

procedure TPythonModule.DeleteVar( const varName : String );
var
  dict : PPyObject;
begin
  if Assigned(FEngine) and Assigned( FModule ) then
    with Engine do
    begin
      dict := PyModule_GetDict( Module );
      if not Assigned(dict) then raise EPythonError.CreateFmt( 'Can''t get __dict__ of module "%s"', [ModuleName] );
      PyDict_DelItemString( dict, PChar(varName) );
    end
  else
    raise EPythonError.CreateFmt( 'Can''t delete var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

procedure TPythonModule.SetVarFromVariant( const varName : String; const value : Variant );
var
  obj : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := VariantAsPyObject( value );
      try
        SetVar( varName, obj );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

function  TPythonModule.GetVarAsVariant( const varName : String ) : Variant;
var
  obj : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := GetVar( varName );
      try
        Result := PyObjectAsVariant( obj );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonType                             **)
(**                                                   **)
(*******************************************************)

//////////////////////////////
//  TPyObject

// Constructors & Destructors
constructor TPyObject.Create( APythonType : TPythonType );
begin
  inherited Create;
  if Assigned(APythonType) then
  begin
    ob_refcnt := 1;
    PythonType := APythonType;
    with APythonType do
    begin
      Inc(FInstanceCount);
      Inc(FCreateHits);
    end;
  end;
end;

constructor TPyObject.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  Create( APythonType );
end;

destructor TPyObject.Destroy;
begin
  if Assigned(PythonType) then
  begin
    Dec(PythonType.FInstanceCount);
    Inc(PythonType.FDeleteHits);
  end;
  inherited;
end;

class function TPyObject.NewInstance: TObject;
var
  mem : PChar;
begin
  GetMem(mem, InstanceSize + Sizeof(PyObject));
  PPyObject(mem)^.ob_refcnt := 1;
  PPyObject(mem)^.ob_type := nil;
  Result := InitInstance(Mem+Sizeof(PyObject));
end;

procedure TPyObject.FreeInstance;
begin
  CleanupInstance;
  if not PythonAlloc then
    FreeMem(PChar(Self)-Sizeof(PyObject));
end;

// Misc
function  TPyObject.GetSelf : PPyObject;
begin
  Result := PPyObject( PChar(Self)-Sizeof(PyObject) )
end;

procedure TPyObject.IncRef;
begin
  Inc(GetSelf^.ob_refcnt);
end;

procedure TPyObject.Adjust(PyPointer: Pointer);
var
  ptr : PInteger;
begin
  ptr := PyPointer;
  ptr^ := Integer(PythonToDelphi(PPyObject(ptr^)));
end;

function  TPyObject.GetModule : TPythonModule;
begin
  if Assigned(PythonType) then
    Result := PythonType.Module
  else
    Result := nil;
end;

function TPyObject.Get_ob_refcnt: Integer;
begin
  Result := GetSelf^.ob_refcnt;
end;

function TPyObject.Get_ob_type: PPyTypeObject;
begin
  Result := GetSelf^.ob_type;
end;

procedure TPyObject.Set_ob_refcnt(const Value: Integer);
begin
  GetSelf^.ob_refcnt := Value;
end;

procedure TPyObject.Set_ob_type(const Value: PPyTypeObject);
begin
  GetSelf^.ob_type := Value;
end;

// Type services
////////////////

// Basic services
function  TPyObject.Print( var f: file; i: integer) : Integer;
begin
  Result := -1;
end;

function  TPyObject.GetAttr(key : PChar) : PPyObject;
var
  PyKey : PPyObject;
begin
  with GetPythonEngine do
    begin
      PyKey := PyString_FromString(key);
      try
        Result := PyObject_GenericGetAttr(GetSelf, PyKey)
      finally
        Py_XDecRef(PyKey);
      end;

//      // check for a method
//      if IsPython3000 then
//        // I think Python 3000 from beta 2 gets the methods from the tp_methods field
//        Result := nil
//      else
//        Result := Py_FindMethod( PythonType.MethodsData, GetSelf, key);
//      if not Assigned(Result) then
//        PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
    end;
end;

function  TPyObject.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  with GetPythonEngine do
    begin
      Result := -1;
      PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
    end;
end;

function  TPyObject.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString( PChar(Format('<%s at %x>', [PythonType.TypeName, Integer(self)])) );
end;

function  TPyObject.Compare( obj: PPyObject) : Integer;
begin
  Result := 0;
end;

function  TPyObject.Hash : Integer;
begin
  Result := Integer(Self);
end;

function  TPyObject.Str: PPyObject;
begin
  Result := Repr;
end;

function  TPyObject.GetAttrO( key: PPyObject) : PPyObject;
begin
{$IFDEF PYTHON22_OR_HIGHER}
  Result := GetPythonEngine.PyObject_GenericGetAttr(GetSelf, key);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function  TPyObject.SetAttrO( key, value: PPyObject) : Integer;
begin
{$IFDEF PYTHON22_OR_HIGHER}
  Result := GetPythonEngine.PyObject_GenericSetAttr(GetSelf, key, value);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

function  TPyObject.Call( ob1, ob2 : PPyObject) : PPyObject;
begin
  Result := nil;
end;

{$IFDEF PYTHON20_OR_HIGHER}
function  TPyObject.Traverse( proc: visitproc; ptr: Pointer) : integer;
begin
  Result := 0;
end;
{$ENDIF}

function  TPyObject.Clear: integer;
begin
  Result := 0;
end;

function  TPyObject.RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.Iter : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.IterNext : PPyObject;
begin
  Result := nil;
end;

{ Called when an instance of a subtype has been created. Same as __init__ in a class }
function TPyObject.Init( args, kwds : PPyObject ) : Integer;
begin
  Result := 0;
end;

// Number services
function  TPyObject.NbAdd( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbSubstract( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbMultiply( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbFloorDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbTrueDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbRemainder( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbDivmod( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbPower( ob1, ob2 : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbNegative : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbPositive : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbAbsolute : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbNonZero : Integer;
begin
  Result := -1;
end;

function  TPyObject.NbInvert : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbLShift( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbRShift( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbAnd( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbXor( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbOr( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbCoerce( obj : PPPyObject) : Integer;
begin
  Result := 0;
end;

function  TPyObject.NbInt : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbLong : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbFloat : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbOct : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbHex : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceAdd(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceAnd(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceDivide(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceFloorDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceTrueDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceLshift(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceMultiply(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceOr(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplacePower(ob1, ob2: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceRemainder(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceRshift(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceSubtract(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceXor(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

// Sequence services
function  TPyObject.SqLength : Integer;
begin
  Result := 0;
end;

function  TPyObject.SqConcat( obj : PPyObject) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqRepeat( val : Integer ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqItem( idx : Integer ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqSlice( idx1, idx2 : Integer ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqAssItem( idx : integer; obj : PPyObject) : Integer;
begin
  Result := -1;
end;

function  TPyObject.SqAssSlice( idx1, idx2 : Integer; obj : PPyObject): integer;
begin
  Result := -1;
end;

function TPyObject.SqContains(obj: PPyObject): integer;
begin
  Result := -1;
end;

function TPyObject.SqInplaceConcat(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.SqInplaceRepeat(i: integer): PPyObject;
begin
  Result := nil;
end;

// Mapping services
function  TPyObject.MpLength : Integer;
begin
  Result := 0;
end;

function  TPyObject.MpSubscript( obj : PPyObject) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.MpAssSubscript( obj1, obj2 : PPyObject) : Integer;
begin
  Result := -1;
end;


// Class methods
class procedure TPyObject.RegisterMethods( APythonType : TPythonType );
begin
end;

class procedure TPyObject.RegisterMembers( APythonType : TPythonType );
begin
end;

class procedure TPyObject.RegisterGetSets( APythonType : TPythonType );
begin
end;

class procedure TPyObject.SetupType(APythonType: TPythonType);
begin

end;


//////////////////////////////
//  TTypeServices

constructor TTypeServices.Create;
begin
  inherited;
  FBasic := [bsGetAttr, bsSetAttr, bsRepr, bsStr];
end;

procedure TTypeServices.AssignTo( Dest: TPersistent );
begin
  if Dest is TTypeServices then
     with TTypeServices( Dest ) do
       begin
         FBasic         := Self.FBasic;
         FNumber        := Self.FNumber;
         FSequence      := Self.FSequence;
         FMapping       := Self.FMapping;
         FInplaceNumber := Self.FInplaceNumber;
       end;
  inherited;
end;

//////////////////////////////
//  TPythonType

function  PythonToDelphi( obj : PPyObject ) : TPyObject;
begin
  if IsDelphiObject( obj ) then
    Result := TPyObject(PChar(obj)+Sizeof(PyObject))
  else
    raise EPythonError.CreateFmt( 'Python object "%s" is not a Delphi class', [GetPythonEngine.PyObjectAsString(obj)] );
end;

procedure PyObjectDestructor( pSelf : PPyObject); cdecl;
var
{$IFDEF PYTHON22_OR_HIGHER}
  call_tp_free : Boolean;
{$ENDIF}
  obj : TPyObject;
begin
  obj := PythonToDelphi(pSelf);
{$IFDEF PYTHON22_OR_HIGHER}
  call_tp_free := obj.PythonAlloc;
{$ENDIF}
  if PythonOk then
    obj.Free;
{$IFDEF PYTHON22_OR_HIGHER}
  if call_tp_free and Assigned(pSelf.ob_type) and Assigned(pSelf.ob_type^.tp_free) then
    pSelf.ob_type^.tp_free(pSelf);
{$ENDIF}
end;

procedure TPythonType.Notification( AComponent: TComponent;
                                    Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FModule then
      FModule := nil;
end;

procedure TPythonType.SetPyObjectClass( val : TPyObjectClass );
begin
  if val <> FPyObjectClass then
    begin
      if Assigned(FPyObjectClass) then
      begin
        ClearMethods;
{$IFDEF PYTHON22_OR_HIGHER}
        ClearMembers;
        ClearGetSets;
{$ENDIF}
      end;
      FPyObjectClass := val;
      if Assigned(val) then
        begin
          FType.tp_basicsize := val.InstanceSize + Sizeof(PyObject);
          val.SetupType( Self );
          val.RegisterMethods( Self );
          val.RegisterMembers( Self );
          val.RegisterGetSets( Self );
        end;
    end;
end;

procedure TPythonType.SetModule( val : TPythonModule );
begin
  if val <> FModule then
    begin
      FModule := val;
      if Assigned(val) then
        if Initialized and not (csLoading in ComponentState) then
          if val.Initialized then
            AddTypeVar
          else
            val.AddClient(Self);
    end;
end;

procedure TPythonType.ModuleReady(Sender : TObject);
begin
  inherited;
  AddTypeVar;
end;

procedure TPythonType.SetServices( val : TTypeServices );
begin
  FServices.Assign( val );
end;

procedure TPythonType.SetTypeName( const val : String );
begin
  if (FTypeName <> val) and (val <> '') then
    begin
      FTypeName := val;
    end;
end;

function  TPythonType.CreateMethod( pSelf, args : PPyObject ) : PPyObject;
begin
  Result := CreateInstanceWith( args );
end;

procedure TPythonType.ReallocGetSets;
begin
  inherited;
{$IFDEF PYTHON22_OR_HIGHER}
  if tpfBaseType in TypeFlags then
    FType.tp_getset := GetSetData;
{$ENDIF}
end;

procedure TPythonType.ReallocMembers;
begin
  inherited;
{$IFDEF PYTHON22_OR_HIGHER}
  if tpfBaseType in TypeFlags then
    FType.tp_members := MembersData;
{$ENDIF}
end;

procedure TPythonType.ReallocMethods;
begin
  inherited;
exit;
{$IFDEF PYTHON22_OR_HIGHER}
  if tpfBaseType in TypeFlags then
    FType.tp_methods := MethodsData;
{$ENDIF}
end;

procedure TPythonType.SetDocString( value : TStringList );
begin
  FDocString.Assign( value );
end;

function  TPythonType.TypeFlagsAsInt : LongInt;
begin
  Result := 0;
  if tpfHaveGetCharBuffer in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_GETCHARBUFFER;
  if tpfHaveSequenceIn in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_SEQUENCE_IN;
  if tpfGC in TypeFlags then
    Result := Result or Py_TPFLAGS_GC;
  if tpfHaveInplaceOps in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_INPLACEOPS;
  if tpfCheckTypes in TypeFlags then
    Result := Result or Py_TPFLAGS_CHECKTYPES;
  if tpfHaveRichCompare in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_RICHCOMPARE;
  if tpfHaveWeakRefs in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_WEAKREFS;
{$IFDEF PYTHON22_OR_HIGHER}
  if tpfHaveIter in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_ITER;
  if tpfHaveClass in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_CLASS;
  if tpfHeapType in TypeFlags then
    Result := Result or Py_TPFLAGS_HEAPTYPE;
  if tpfBaseType in TypeFlags then
    Result := Result or Py_TPFLAGS_BASETYPE;
  if tpfReady in TypeFlags then
    Result := Result or Py_TPFLAGS_READY;
  if tpfReadying in TypeFlags then
    Result := Result or Py_TPFLAGS_READYING;
  if tpfHaveGC in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_GC;
{$ENDIF}
end;

// Type services
// They will be all forwarded to the Delphi class that
// implements the object through the use of virtual
// methods
///////////////////////////////////////

// Basic services

function  TPythonType_Print( pSelf : PPyObject; var f: file; i: integer) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Print( f, i );
end;

function  TPythonType_GetAttr( pSelf : PPyObject; key : PChar) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).GetAttr( key );
end;

function  TPythonType_SetAttr( pSelf : PPyObject; key : PChar; value : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SetAttr( key, value );
end;

function  TPythonType_Repr( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Repr;
end;

function  TPythonType_Compare( pSelf, obj : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Compare( obj );
end;

function  TPythonType_Hash( pSelf : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Hash;
end;

function  TPythonType_Str( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Str;
end;

function  TPythonType_GetAttrO( pSelf, key: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).GetAttrO( key );
end;

function  TPythonType_SetAttrO( pSelf, key, value: PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SetAttrO( key, value );
end;

function  TPythonType_Call( pSelf, ob1, ob2 : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Call( ob1, ob2 );
end;

{$IFDEF PYTHON20_OR_HIGHER}
function  TPythonType_Traverse( pSelf: PPyObject; proc: visitproc; ptr: Pointer) : integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Traverse( proc, ptr );
end;
{$ENDIF}

function  TPythonType_Clear( pSelf: PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Clear;
end;

function  TPythonType_RichCmp( pSelf, obj : PPyObject; i : Integer) : PPyObject; cdecl;
begin
  Assert(i >= Ord(Low(TRichComparisonOpcode)));
  Assert(i <= Ord(High(TRichComparisonOpcode)));
  Result := PythonToDelphi(pSelf).RichCompare( obj, TRichComparisonOpcode(i) );
end;

function  TPythonType_Iter( pSelf: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Iter;
end;

function  TPythonType_IterNext( pSelf: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).IterNext;
end;

function  TPythonType_InitSubtype( pSelf, args, kwds : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Init(args, kwds);
end;

function  TPythonType.NewSubtypeInst( aType: PPyTypeObject; args, kwds : PPyObject) : PPyObject;
{$IFDEF PYTHON22_OR_HIGHER}
var
  obj : TPyObject;
{$ENDIF}
begin
{$IFDEF PYTHON22_OR_HIGHER}
  Result := aType^.tp_alloc(aType, 0);
  if Assigned(Result) then
  begin
    obj := PythonToDelphi(Result);
    PyObjectClass.InitInstance(obj);
    obj.ob_type := aType;
    obj.IsSubtype := aType <> @FType;
    obj.PythonAlloc := True;
    obj.CreateWith(Self, args);
    if Engine.PyErr_Occurred <> nil then
    begin
      Engine.Py_DECREF(Result);
      Result := nil;
    end;
  end;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function  TPythonType_AllocSubtypeInst( pSelf: PPyTypeObject; nitems : integer) : PPyObject; cdecl;
begin
{$IFDEF PYTHON22_OR_HIGHER}
  Result := GetPythonEngine.PyType_GenericAlloc(pSelf, nitems);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

procedure FreeSubtypeInst(ob:PPyObject);
begin
{$IFDEF PYTHON22_OR_HIGHER}
  GetPythonEngine.PyObject_Free(ob);
{$ENDIF}
end;


// Number services

function  TPythonType_NbAdd( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAdd( obj );
end;

function  TPythonType_NbSubstract( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbSubstract( obj );
end;

function  TPythonType_NbMultiply( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbMultiply( obj );
end;

function  TPythonType_NbDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbDivide( obj );
end;

function  TPythonType_NbFloorDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloorDivide( obj );
end;

function  TPythonType_NbTrueDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbTrueDivide( obj );
end;

function  TPythonType_NbRemainder( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbRemainder( obj );
end;

function  TPythonType_NbDivmod( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbDivmod( obj );
end;

function  TPythonType_NbPower( pSelf, ob1, ob2 : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbPower( ob1, ob2 );
end;

function  TPythonType_NbNegative( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbNegative;
end;

function  TPythonType_NbPositive( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbPositive;
end;

function  TPythonType_NbAbsolute( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAbsolute;
end;

function  TPythonType_NbNonZero( pSelf : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbNonZero;
end;

function  TPythonType_NbInvert( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInvert;
end;

function  TPythonType_NbLShift( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbLShift( obj );
end;

function  TPythonType_NbRShift( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbRShift( obj );
end;

function  TPythonType_NbAnd( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAnd( obj );
end;

function  TPythonType_NbXor( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbXor( obj );
end;

function  TPythonType_NbOr( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbOr( obj );
end;

function  TPythonType_NbCoerce( pSelf, obj : PPPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf^).NbCoerce( obj );
end;

function  TPythonType_NbInt( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInt;
end;

function  TPythonType_NbLong( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbLong;
end;

function  TPythonType_NbFloat( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloat;
end;

function  TPythonType_NbOct( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbOct;
end;

function  TPythonType_NbHex( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbHex;
end;

function TPythonType_NbInplaceAdd(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceAdd( obj );
end;

function TPythonType_NbInplaceAnd(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceAnd( obj );
end;

function TPythonType_NbInplaceDivide(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceDivide( obj );
end;

function  TPythonType_NbInplaceFloorDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceFloorDivide( obj );
end;

function  TPythonType_NbInplaceTrueDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceTrueDivide( obj );
end;

function TPythonType_NbInplaceLshift(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceLshift( obj );
end;

function TPythonType_NbInplaceMultiply(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceMultiply( obj );
end;

function TPythonType_NbInplaceOr(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceOr( obj );
end;

function TPythonType_NbInplacePower(pSelf, ob1, ob2: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplacePower( ob1, ob2 );
end;

function TPythonType_NbInplaceRemainder(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceRemainder( obj );
end;

function TPythonType_NbInplaceRshift(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceRshift( obj );
end;

function TPythonType_NbInplaceSubtract(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceSubtract( obj );
end;

function TPythonType_NbInplaceXor(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceXor( obj );
end;

// Sequence services

function  TPythonType_SqLength( pSelf : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqLength;
end;

function  TPythonType_SqConcat( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqConcat( obj );
end;

function  TPythonType_SqRepeat( pSelf : PPyObject; val : Integer ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqRepeat( val );
end;

function  TPythonType_SqItem( pSelf : PPyObject; idx : Integer ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqItem( idx );
end;

function  TPythonType_SqSlice( pSelf : PPyObject; idx1, idx2 : Integer ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqSlice( idx1, idx2 );
end;

function  TPythonType_SqAssItem( pSelf : PPyObject; idx : integer; obj : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqAssItem( idx, obj );
end;

function  TPythonType_SqAssSlice( pSelf : PPyObject; idx1, idx2 : Integer; obj : PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqAssSlice( idx1, idx2, obj );
end;


// Mapping services

function  TPythonType_MpLength( pSelf : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpLength;
end;

function  TPythonType_MpSubscript( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpSubscript( obj );
end;

function  TPythonType_MpAssSubscript( pSelf, obj1, obj2 : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpAssSubscript( obj1, obj2 );
end;

function TPythonType_SqContains(pSelf, obj : PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqContains( obj );
end;

function TPythonType_SqInplaceConcat(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqInplaceConcat( obj );
end;

function TPythonType_SqInplaceRepeat(pSelf : PPyObject; i: integer): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqInplaceRepeat( i );
end;

procedure TPythonType.InitServices;
begin
  with FType do
    begin
      // Basic services
      if FDocString.Count > 0 then
        begin
          FCurrentDocString := GetPythonEngine.CleanString(FDocString.Text);
{$IFDEF PYTHON20_OR_HIGHER}
          tp_doc := PChar(FCurrentDocString);
{$ENDIF}
        end;
      tp_dealloc   := @PyObjectDestructor;
      if bsGetAttr in Services.Basic then
        tp_getattr   := TPythonType_GetAttr;
      if bsSetAttr in Services.Basic then
        tp_setattr   := TPythonType_SetAttr;
      if bsRepr in Services.Basic then
        tp_repr      := TPythonType_Repr;
      if bsStr in Services.Basic then
        tp_str       := TPythonType_Str;
      if bsCompare in Services.Basic then
        tp_compare   := TPythonType_Compare;
      if bsHash in Services.Basic then
        tp_hash      := TPythonType_Hash;
      if bsGetAttrO in Services.Basic then
        tp_getattro  := TPythonType_GetAttrO;
      if bsSetAttrO in Services.Basic then
        tp_setattro  := TPythonType_SetAttrO;
      if bsCall in Services.Basic then
        tp_call      := TPythonType_Call;
{$IFDEF PYTHON20_OR_HIGHER}
      if bsTraverse in Services.Basic then
        tp_traverse := TPythonType_Traverse;
      if bsClear in Services.Basic then
        tp_clear := TPythonType_Clear;
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
      if bsRichCompare in Services.Basic then
        tp_richcompare := TPythonType_RichCmp;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
      if bsIter in Services.Basic then
        tp_iter := TPythonType_Iter;
      if bsIterNext in Services.Basic then
        tp_iternext := TPythonType_IterNext;
      if tpfBaseType in TypeFlags then
      begin
        tp_init             := TPythonType_InitSubtype;
        tp_alloc            := TPythonType_AllocSubtypeInst;
        tp_new              := GetCallBack( Self, @TPythonType.NewSubtypeInst, 3, ctCDECL);
        tp_free             := FreeSubtypeInst;
        tp_methods          := MethodsData;
        tp_members          := MembersData;
        tp_getset           := GetSetData;
      end;
{$ENDIF}

      // Number services
      if Services.Number <> [] then
        tp_as_number := @FNumber;
      with FNumber do
        begin
          if nsAdd in Services.Number then
            nb_add := TPythonType_NbAdd;
          if nsSubstract in Services.Number then
            nb_substract := TPythonType_NbSubstract;
          if nsMultiply in Services.Number then
            nb_multiply := TPythonType_NbMultiply;
          if nsDivide in Services.Number then
            nb_divide := TPythonType_NbDivide;
{$IFDEF PYTHON22_OR_HIGHER}
          if nsFloorDivide in Services.Number then
            nb_floor_divide := TPythonType_NbFloorDivide;
          if nsTrueDivide in Services.Number then
            nb_true_divide := TPythonType_NbTrueDivide;
{$ENDIF}
          if nsRemainder in Services.Number then
            nb_remainder := TPythonType_NbRemainder;
          if nsDivmod in Services.Number then
            nb_divmod := TPythonType_NbDivmod;
          if nsPower in Services.Number then
            nb_power := TPythonType_NbPower;
          if nsNegative in Services.Number then
            nb_negative := TPythonType_NbNegative;
          if nsPositive in Services.Number then
            nb_positive := TPythonType_NbPositive;
          if nsAbsolute in Services.Number then
            nb_absolute := TPythonType_NbAbsolute;
          if nsNonZero in Services.Number then
            nb_nonzero := TPythonType_NbNonZero;
          if nsInvert in Services.Number then
            nb_invert := TPythonType_NbInvert;
          if nsLShift in Services.Number then
            nb_lshift := TPythonType_NbLShift;
          if nsRShift in Services.Number then
            nb_rshift := TPythonType_NbRShift;
          if nsAnd in Services.Number then
            nb_and := TPythonType_NbAnd;
          if nsXor in Services.Number then
            nb_xor := TPythonType_NbXor;
          if nsOr in Services.Number then
            nb_or := TPythonType_NbOr;
          if nsCoerce in Services.Number then
            nb_coerce := TPythonType_NbCoerce;
          if nsInt in Services.Number then
            nb_int := TPythonType_NbInt;
          if nsLong in Services.Number then
            nb_long := TPythonType_NbLong;
          if nsFloat in Services.Number then
            nb_float := TPythonType_NbFloat;
          if nsOct in Services.Number then
            nb_oct := TPythonType_NbOct;
          if nsHex in Services.Number then
            nb_hex := TPythonType_NbHex;
{$IFDEF PYTHON20_OR_HIGHER}
          if nsInplaceAdd in Services.InplaceNumber then
            nb_inplace_add       := TPythonType_NbInplaceAdd;
          if nsInplaceSubtract in Services.InplaceNumber then
            nb_inplace_subtract  := TPythonType_NbInplaceSubtract;
          if nsInplaceMultiply in Services.InplaceNumber then
            nb_inplace_multiply  := TPythonType_NbInplaceMultiply;
          if nsInplaceDivide in Services.InplaceNumber then
            nb_inplace_divide    := TPythonType_NbInplaceDivide;
{$IFDEF PYTHON22_OR_HIGHER}
          if nsInplaceFloorDivide in Services.InplaceNumber then
            nb_inplace_floor_divide := TPythonType_NbInplaceFloorDivide;
          if nsInplaceTrueDivide in Services.InplaceNumber then
            nb_inplace_true_divide := TPythonType_NbInplaceTrueDivide;
{$ENDIF}
          if nsInplaceRemainder in Services.InplaceNumber then
            nb_inplace_remainder := TPythonType_NbInplaceRemainder;
          if nsInplacePower in Services.InplaceNumber then
            nb_inplace_power     := TPythonType_NbInplacePower;
          if nsInplaceLShift in Services.InplaceNumber then
            nb_inplace_lshift    := TPythonType_NbInplaceLShift;
          if nsInplaceRShift in Services.InplaceNumber then
            nb_inplace_rshift    := TPythonType_NbInplaceRShift;
          if nsInplaceAnd in Services.InplaceNumber then
            nb_inplace_and       := TPythonType_NbInplaceAnd;
          if nsInplaceXor in Services.InplaceNumber then
            nb_inplace_xor       := TPythonType_NbInplaceXor;
          if nsInplaceOr in Services.InplaceNumber then
            nb_inplace_or        := TPythonType_NbInplaceOr;
{$ENDIF}
        end;

      // Sequence services
      if Services.Sequence <> [] then
        tp_as_sequence := @FSequence;

      with FSequence do
        begin
          if ssLength in Services.Sequence then
            sq_length := TPythonType_SqLength;
          if ssConcat in Services.Sequence then
            sq_concat := TPythonType_SqConcat;
          if ssRepeat in Services.Sequence then
            sq_repeat := TPythonType_SqRepeat;
          if ssItem in Services.Sequence then
            sq_item := TPythonType_SqItem;
          if ssSlice in Services.Sequence then
            sq_slice := TPythonType_SqSlice;
          if ssAssItem in Services.Sequence then
            sq_ass_item := TPythonType_SqAssItem;
          if ssAssSlice in Services.Sequence then
            sq_ass_slice := TPythonType_SqAssSlice;
{$IFDEF PYTHON20_OR_HIGHER}
          if ssContains in Services.Sequence then
            sq_contains := TPythonType_SqContains;
          if ssInplaceConcat in Services.Sequence then
            sq_inplace_concat  := TPythonType_SqInplaceConcat;
          if ssInplaceRepeat in Services.Sequence then
            sq_inplace_repeat  := TPythonType_SqInplaceRepeat;
{$ENDIF}
        end;

      // Mapping services
      if Services.Mapping <> [] then
        tp_as_mapping := @FMapping;

      with FMapping do
        begin
          if msLength in Services.Mapping then
            mp_length := TPythonType_MpLength;
          if msSubScript in Services.Mapping then
            mp_subscript := TPythonType_MpSubscript;
          if msAssSubscript in Services.Mapping then
            mp_ass_subscript := TPythonType_MpAssSubscript;
        end;
    end;
end;

// Public methods

constructor TPythonType.Create( AOwner : TComponent );
begin
  inherited;
  FPrefix := 'Create';
  FServices := TTypeServices.Create;
  FDocString := TStringList.Create;
  FTypeFlags := TPFLAGS_DEFAULT;
  FGenerateCreateFunction := True;
end;

destructor  TPythonType.Destroy;
begin
  if gVarType = Self then
    gVarType := nil;
  FDocString.Free;
  FServices.Free;
  inherited;
end;

function  TPythonType.GetTypePtr : PPyTypeObject;
begin
  Result := PPyTypeObject(@FType);
end;

procedure TPythonType.Initialize;
begin
  CheckEngine;
  with Engine, TheType do
    begin
      ob_type   := PPyTypeObject(PyType_Type);
      ob_refcnt := 1;
      tp_name   := PChar(FTypeName);
{$IFDEF PYTHON20_OR_HIGHER}
      tp_flags  := TypeFlagsAsInt;
{$ENDIF}
    end;
  if Assigned(FModule) then
    begin
      if Module.Initialized then
        AddTypeVar
      else
        Module.AddClient( Self );
    end;
  InitServices;
  inherited;
end;

procedure TPythonType.Finalize;
begin
  Engine.Py_XDECREF(FCreateFunc);
  FCreateFunc := nil;
  inherited;
end;

function TPythonType.CreateInstance : PPyObject;
var
  obj : TPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := PyObjectClass.Create( Self );
      obj.ob_type := @FType;
      if PyErr_Occurred <> nil then
      begin
        obj.Free;
        Result := nil;
      end
      else
        Result := obj.GetSelf;
    end;
end;

function TPythonType.CreateInstanceWith( args : PPyObject ) : PPyObject;
var
  obj : TPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := PyObjectClass.CreateWith( Self, args );
      obj.ob_type := @FType;
      if PyErr_Occurred <> nil then
      begin
        obj.Free;
        Result := nil;
      end
      else
        Result := obj.GetSelf;
    end;
end;

procedure TPythonType.AddTypeVar;
var
  d : PPyObject;
  meth : TDelphiMethod;
begin
  CheckEngine;
  Assert(Module <> nil);
  Assert(Module.Module <> nil);
  if FGenerateCreateFunction then
  begin
    FCreateFuncName := FPrefix+FTypeName;
    FCreateFuncDoc := Format('Creates a new instance of type %s', [TypeName]);
    if not Assigned(FCreateFunc) then
    begin
      meth := CreateMethod;
      FCreateFuncDef.ml_name  := PChar(FCreateFuncName);
      FCreateFuncDef.ml_meth  := GetOfObjectCallBack( TCallBack(meth), 2, ctCDECL);
      FCreateFuncDef.ml_flags := METH_VARARGS;
      FCreateFuncDef.ml_doc   := PChar(FCreateFuncDoc);
      FCreateFunc := Engine.PyCFunction_New(@FCreateFuncDef, nil);
    end;
    Assert(Assigned(FCreateFunc));
  end;
  with Engine do
    begin
      d := PyModule_GetDict( Module.Module );
      Assert(Assigned(d));
      PyDict_SetItemString( d, PChar(TypeName), PPyObject(TheTypePtr) );
      if FGenerateCreateFunction then
        PyDict_SetItemString( d, PChar(FCreateFuncName), FCreateFunc );
    end;
end;

function TPythonType.GetMembersStartOffset : Integer;
begin
  Result := Sizeof(PyObject);
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonDelphiVar                        **)
(**                                                   **)
(*******************************************************)

procedure TPythonDelphiVar.CreateVarType;
begin
  if not Assigned(gVarType) then
  begin
    gVarType := TPythonType.Create( Self.Engine );
    with gVarType do
      begin
        TypeName := 'PythonDelphiVar';
        Engine := Self.Engine;
        PyObjectClass := TPyVar;
        Initialize;
      end;
  end;
end;

procedure TPythonDelphiVar.CreateVar;
var
  v : TPyVar;
  m, d : PPyObject;
begin
  if not Assigned(Engine) then
    Exit;
  Assert(Assigned(gVarType), 'missing TPythonType for TPythonDelphiVar');
  with Engine do
    begin
      // Create an instance of PythonDelphiVar
      FVarObject := gVarType.CreateInstance;
      CheckError(False);
      v := TPyVar(PythonToDelphi(FVarObject));
      v.dv_component := Self;
      // Add a reference to this var in the module
      m := PyImport_AddModule(PChar(Module));
      if m = nil then
        raise EPythonError.CreateFmt('CreateVar: can''t create module "%s"', [Module]);
      d := PyModule_GetDict(m);
      if @PyDict_SetItemString = nil then
        raise Exception.Create('nil');
      PyDict_SetItemString( d, PChar(VarName), FVarObject );
    end;
end;

function  TPythonDelphiVar.GetValue : Variant;
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      Result := GetValueAsVariant
  else
    raise Exception.Create('No variable was created' );
end;

procedure TPythonDelphiVar.SetValue( const val : Variant );
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      SetValueFromVariant(val)
  else
    raise Exception.Create('No variable was created' );
end;

// Warning: GetValueAsPyObject returns a preincremented object !
function  TPythonDelphiVar.GetValueAsPyObject : PPyObject;
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      Result := GetValue
  else
    raise Exception.Create('No variable was created' );
end;

procedure TPythonDelphiVar.SetValueFromPyObject( val : PPyObject );
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      SetValue(val)
  else
    raise Exception.Create('No variable was created' );
end;

function  TPythonDelphiVar.IsVariantOk( const v : Variant ) : Boolean;
var
  t : Integer;
begin
  t := VarType(v) and VarTypeMask;
  Result := (t = varSmallint) or
            (t = varInteger) or
            (t = varSingle) or
            (t = varDouble) or
            (t = varCurrency) or
            (t = varDate) or
            (t = varOleStr) or
            (t = varBoolean) or
            (t = varByte) or
            (t = varString);
end;

function  TPythonDelphiVar.GetValueAsString : String;
var
  v : Variant;
  obj : PPyObject;
begin
  v := Value;
  if IsVariantOk( v ) then
    Result := v
  else
    begin
      CheckEngine;
      obj := GetValueAsPyObject;
      try
        Result := Engine.PyObjectAsString( obj );
      finally
        Engine.Py_XDecRef(obj);
      end;
    end;
end;

procedure TPythonDelphiVar.SetVarName( const val : String );

  procedure CheckVarName;
  var
    i : Integer;
  begin
    if Owner = nil then Exit;
    if (val = FVarName) or (val = '') then Exit;
    for i := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[i] is TPythonDelphiVar then
        with TPythonDelphiVar(Owner.Components[i]) do
          if (VarName = val) and (Module = Self.Module) then
            raise Exception.CreateFmt('A variable "%s" already exists in the module "%s"',[val, Module]);
  end;

begin
  if val <> FVarName then
    begin
      CheckVarName;
      FVarName := val;
    end;
end;

constructor TPythonDelphiVar.Create( AOwner : TComponent );

  procedure AdjustName;
  var
    i, cpt : Integer;
    done : Boolean;
  begin
    if AOwner = nil then Exit;
    cpt := 1;
    done := False;
    while not done do
      begin
        done := True;
        for i := 0 to AOwner.ComponentCount - 1 do
          if AOwner.Components[i] is TPythonDelphiVar then
            with TPythonDelphiVar(AOwner.Components[i]) do
              if (VarName = Self.FVarName+IntToStr(cpt)) and
                 (Module = Self.Module) then
                begin
                  Inc(cpt);
                  done := False;
                  Break;
                end;
      end;
    FVarName := FVarName + IntToStr(cpt);
  end;

begin
  inherited;
  FModule := '__main__';
  FVarName := 'varname';
  if csDesigning in ComponentState then
    AdjustName;
end;

procedure TPythonDelphiVar.Initialize;
begin
  if csDesigning in ComponentState then
    Exit;
  CheckEngine;
  CreateVarType;
  CreateVar;
  inherited;
end;

procedure TPythonDelphiVar.Finalize;
begin
  inherited;
  if not PythonOK then
    Exit;
  if Assigned(FVarObject) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      begin
        dv_component := nil;
        SetValue( nil );
      end;
  with Engine do
    Py_XDECREF( FVarObject );
  FVarObject := nil;
end;


constructor TPyVar.Create( APythonType : TPythonType );
begin
  inherited;
end;

// Don't call the Create constructor of TPyVar, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyVar.Create will be automatically be called.

constructor TPyVar.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'O:CreateVar', [@dv_object] ) = 0 then
        exit;
    end;
end;

destructor TPyVar.Destroy;
begin
  with GetPythonEngine do
    begin
      if Assigned(dv_object) then
        begin
          Py_DecRef(dv_object);
          dv_object := nil;
        end;
    end;
  inherited;
end;

// Then we override the needed services

function  TPyVar.GetAttr(key : PChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( key, 'Value') = 0 then
        Result := GetValue
      else
        Result := inherited GetAttr(key);
    end;
end;

function  TPyVar.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  Result := 0;
  with GetPythonEngine do
    begin
      if CompareText( key, 'Value' ) = 0 then
        SetValue( value )
      else
        Result := inherited SetAttr(key, value);
    end;
end;

function  TPyVar.Repr : PPyObject;
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := GetValue;
      try
        Result := PyString_FromString( PChar(Format('<%s: %s>', [PythonType.TypeName, PyObjectAsString(obj)])) );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

// Class methods
// We register the methods of our type

class procedure TPyVar.RegisterMethods( APythonType : TPythonType );
begin
  inherited;
  with APythonType do
    begin
      //AddMethod( 'OffsetBy', @TPyPoint.DoOffsetBy, 'Point.OffsetBy( dx, dy )' );
    end;
end;

// Methods of TPyVar


// Warning: GetValue returns a preincremented object !
function TPyVar.GetValue : PPyObject;
var
  v : Variant;
begin
  Result := nil;
  with GetPythonEngine do
    begin
      if Assigned( dv_component ) and
         (@dv_component.OnExtGetData <> nil) then
         begin
           dv_component.OnExtGetData( dv_component, Result );
         end
      else if Assigned( dv_component ) and
         (@dv_component.OnGetData <> nil) then
        begin
           dv_component.OnGetData( dv_component, v );
           Result := VariantAsPyObject(v);
        end
      else if Assigned(dv_object) then
        begin
          Result := dv_object;
          Py_XIncRef(Result);
        end;
      if Result = nil then
        Result := ReturnNone;
    end;
end;

function TPyVar.GetValueAsVariant : Variant;
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := GetValue;
      try
        try
          Result := PyObjectAsVariant( obj );
        except
          Result := PyObjectAsString(obj);
        end;
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

procedure TPyVar.SetValue( value : PPyObject );
begin
  with GetPythonEngine do
    begin
      if Assigned( dv_component ) and
         (@dv_component.OnExtSetData <> nil) then
        begin
          dv_component.OnExtSetData( dv_component, value);
        end
      else if Assigned( dv_component ) and
         (@dv_component.OnSetData <> nil) then
        begin
          dv_component.OnSetData( dv_component, PyObjectAsVariant(value) );
        end;
      Py_XDecRef(dv_object);
      dv_object := value;
      Py_XIncRef(dv_object);
      if Assigned( dv_component ) and
         (@dv_component.OnChange <> nil) then
        dv_component.OnChange( dv_component );
    end;
end;

procedure TPyVar.SetValueFromVariant( const value : Variant );
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := VariantAsPyObject( value );
      SetValue(obj);
      Py_XDecRef(obj);
    end;
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonThread                           **)
(**                                                   **)
(*******************************************************)

procedure TPythonThread.Execute;
var
  withinterp: Boolean;
begin
  withinterp := Assigned( fInterpreterState);
  with GetPythonEngine do
  begin
    if withinterp then
    begin
      fThreadExecMode := emNewState;
      fThreadState := PyThreadState_New( fInterpreterState);
      if Assigned(fThreadState) then
      begin
        PyEval_AcquireThread(fThreadState);
        ExecuteWithPython;
        PyEval_ReleaseThread( fThreadState);
        PyThreadState_Clear(  fThreadState);
        PyThreadState_Delete( fThreadState);
      end else
        raise EPythonError.Create( 'Could not create a new thread state');
    end else {withinterp}
    begin
      fThreadExecMode := emNewInterpreter;
      PyEval_AcquireLock;
      fThreadState := Py_NewInterpreter;
      if Assigned( fThreadState) then
      begin
        ExecuteWithPython;
        Py_EndInterpreter( fThreadState);
        PyEval_ReleaseLock;
      end else
        raise EPythonError.Create( 'Could not create a new thread state');
    end; {withinterp}
  end;
end;


procedure TPythonThread.Py_Begin_Allow_Threads;
begin
  with GetPythonEngine do
    f_savethreadstate := PyEval_SaveThread;
end;

procedure TPythonThread.Py_End_Allow_Threads;
begin
  with GetPythonEngine do
    PyEval_RestoreThread( f_savethreadstate);
end;

procedure TPythonThread.Py_Begin_Block_Threads;
begin
  Py_End_Allow_Threads;
end;

procedure TPythonThread.Py_Begin_Unblock_Threads;
begin
  Py_Begin_Allow_Threads;
end;


(*******************************************************)
(**                                                   **)
(**     Methods for new Python objects or modules     **)
(**                                                   **)
(*******************************************************)

/////////////////////////////////////////////////////////
// Module pyio for Python Input/Outputs
//

function pyio_write(self, args : PPyObject) : PPyObject;
var
  a1 : PPyObject;
begin
  // Forbid printing for any other thread than the main one
  {$IFNDEF FPC}
  if GetCurrentThreadId <> MainThreadId then
    with GetPythonEngine do
      begin
        if RedirectIO and (IO <> nil) and (IO.ClassName <> 'TPythonInputOutput') then
          begin
            Result := GetPythonEngine.ReturnNone;
            Exit;
          end;
      end;
  {$ENDIF}
  with GetPythonEngine do
    begin
      if Assigned(args) and (PyTuple_Size(args) > 0) then
        begin
          a1 := PyTuple_GetItem(args, 0);
          if RedirectIO and (IO <> nil) and Assigned(a1) then
          begin
{$IFDEF UNICODE_SUPPORT}
            if PyUnicode_Check(a1) then
              IO.Write(PyUnicode_AsWideString(a1))
            else
{$ENDIF}
              if PyString_Check(a1) then
                IO.Write(PyObjectAsString(a1));
          end;
          Result := ReturnNone;
        end
      else
        begin
          PyErr_BadArgument;
          Result := nil;
        end;
    end;
end;

function pyio_read(self, args : PPyObject) : PPyObject;
var
  txt, msg : String;
{$IFDEF UNICODE_SUPPORT}
  Widetxt : WideString;
{$ENDIF}
begin
  with GetPythonEngine do
    begin
      if RedirectIO  then
        begin
          txt := '';
          msg := 'Enter text';
          if Assigned(IO) then
{$IFDEF UNICODE_SUPPORT}
            if IO.UnicodeIO then begin
              Widetxt := IO.ReceiveUniData;
              // KV!!!!!!
              if PyErr_Occurred <> nil then
                Result := nil
              else
              // KV!!!!!!
                Result := PyUnicode_FromWideString(PWideChar(Widetxt));
            end else begin
              txt := IO.ReceiveData;
              // KV!!!!!!
              if PyErr_Occurred <> nil then
                Result := nil
              else
              // KV!!!!!!
                Result := PyString_FromString(PChar(txt));
            end
          else
            Result := PyString_FromString(PChar(txt));
{$ELSE}
            txt := IO.ReceiveData;
          Result := PyString_FromString(PChar(txt));
{$ENDIF}
        end
      else
        Result := ReturnNone;
    end;
end;

function pyio_SetDelayWrites(self, args : PPyObject) : PPyObject;
var
  val : Integer;
begin
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'i:SetDelayWrites', [@val] ) <> 0 then
        begin
          if IO <> nil then
            IO.DelayWrites := val <> 0;
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

function pyio_SetMaxLines(self, args : PPyObject) : PPyObject;
var
  val : Integer;
begin
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'i:SetMaxLines', [@val] ) <> 0 then
        begin
          if IO <> nil then
            IO .MaxLines := val;
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

// With no args, it will look at all types
// With args, it will look only at the types listed in the args.

// It returns a list of tuples. Each tuple contains:
// the Type name, the InstanceCount, the CreateHits and the DeleteHits

function pyio_GetTypesStats(self, args : PPyObject) : PPyObject;

  function HandleType( T : TPythonType ) : PPyObject;
  begin
    with GetPythonEngine do
      begin
        Result := PyTuple_New(4);
        PyTuple_SetItem( Result, 0, PyString_FromString(PChar(T.TypeName)) );
        PyTuple_SetItem( Result, 1, PyInt_FromLong(T.InstanceCount) );
        PyTuple_SetItem( Result, 2, PyInt_FromLong(T.CreateHits) );
        PyTuple_SetItem( Result, 3, PyInt_FromLong(T.DeleteHits) );
      end;
  end;

  function FindType( const TName : String ) : TPythonType;
  var
    i : Integer;
  begin
    Result := nil;
    with GetPythonEngine do
      for i := 0 to ClientCount - 1 do
        if Clients[i] is TPythonType then
          with TPythonType(Clients[i]) do
            if TypeName = TName then
              begin
                Result := TPythonType(Clients[i]);
                Break;
              end;
  end;

var
  i : Integer;
  T : TPythonType;
  obj : PPyObject;
  str : String;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(0);
      if PyTuple_Size(args) > 0 then
        for i := 0 to PyTuple_Size(args)-1 do
          begin
            str := PyObjectAsString( PyTuple_GetItem(args, i) );
            T := FindType( str );
            if Assigned(T) then
              begin
                obj := HandleType( T );
                PyList_Append( Result, obj );
                Py_XDecRef(obj);
              end;
          end
      else
        for i := 0 to ClientCount - 1 do
          if Clients[i] is TPythonType then
            begin
              obj := HandleType( TPythonType(Clients[i]) );
              PyList_Append( Result, obj );
              Py_XDecRef(obj);
            end;
    end;
end;





(*******************************************************)
(**                                                   **)
(**            Global procedures                      **)
(**                                                   **)
(*******************************************************)

function  GetPythonEngine : TPythonEngine;
begin
  if not Assigned( gPythonEngine ) then
    raise Exception.Create( 'No Python engine was created' );
  if not gPythonEngine.Finalizing then
    if not gPythonEngine.Initialized then
      raise Exception.Create( 'The Python engine is not properly initialized' );
  Result := gPythonEngine;
end;

function  PythonOK : Boolean;
begin
  Result := Assigned( gPythonEngine ) and
            (gPythonEngine.Initialized or gPythonEngine.Finalizing);
end;

function IsDelphiObject( obj : PPyObject ) : Boolean;
var
  t : PPyTypeObject;
begin
  Result := False;
  // Here's a simple trick: we compare the object destructor to
  // our special destructor for Delphi objects, or
  // we check if one of the parent types of obj has a Delphi destructor.
  if Assigned(obj) then
  begin
    t := obj^.ob_type;
    while Assigned(t) do
    begin
      if @t^.tp_dealloc = @PyObjectDestructor then
      begin
        Result := True;
        Break;
      end;
{$IFDEF PYTHON22_OR_HIGHER}
      t := t^.tp_base;
{$ELSE}
      t := nil;
{$ENDIF}
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Python',[ TPythonEngine, TPythonInputOutput,
                                TPythonType, TPythonModule, TPythonDelphiVar]);
end;

{$IFDEF PYTHON20_OR_HIGHER}
function PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;
begin
  //(((t)->tp_flags & (f)) != 0)
  Result := (((AType)^.tp_flags and (AFlag)) <> 0);
end;
{$ENDIF}

end.







