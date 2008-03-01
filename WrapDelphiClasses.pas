unit WrapDelphiClasses;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi;

type
  {
     PyObject wrapping TPersistent
     Exposes Assign Method
  }
  TPyDelphiPersistent = class (TPyDelphiObject)
  private
    function  GetDelphiObject: TPersistent;
    procedure SetDelphiObject(const Value: TPersistent);
  protected
    // Exposed Methods
    function Assign_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function GetNamePath_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Virtual Methods
    function Assign(ASource : PPyObject) : PPyObject; virtual;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TPersistent read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the TCollectionItem items of a TCollection.
  }
  TCollectionAccess = class(TContainerAccess)
  private
    function GetContainer: TCollection;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;

    property Container : TCollection read GetContainer;
  end;

  {
     PyObject wrapping TCollection
  }
  TPyDelphiCollection = class (TPyDelphiPersistent)
  private
    function  GetDelphiObject: TCollection;
    procedure SetDelphiObject(const Value: TCollection);
  protected
    // Exposed Methods
    function Insert_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Add_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Clear_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Delete_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BeginUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function EndUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_Count( AContext : Pointer) : PPyObject; cdecl;
    function Get_Items( AContext : Pointer) : PPyObject; cdecl;
{$IFDEF DELPHI6_OR_HIGHER}
    function Get_Owner( AContext : Pointer) : PPyObject; cdecl;
{$ENDIF}
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Properties
    property DelphiObject: TCollection read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the items owned by a component.
  }
  TComponentsAccess = class(TContainerAccess)
  private
    function GetContainer: TComponent;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;

    property Container : TComponent read GetContainer;
  end;

  {
     PyObject wrapping TComponent
     Exposes read-only properties ComponentCount and Owner as well as
     sub-components as pseudo-properties
  }
  TPyDelphiComponent = class (TPyDelphiPersistent)
  private
    fFreeNotificationComp : TComponent;
    function  GetDelphiObject: TComponent;
    procedure SetDelphiObject(const Value: TComponent);
    procedure HandleFreeNotificationEvent(Sender: TObject; AComponent: TComponent);
  protected
    function  CreateComponent(AOwner : TComponent) : TComponent; virtual;
    procedure SubscribeToFreeNotification; override;
    procedure UnSubscribeToFreeNotification; override;
    // Exposed Methods
    function GetParentComponent_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HasParent_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BindMethodsToEvents(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_ComponentCount( AContext : Pointer) : PPyObject; cdecl;
    function Get_Owner( AContext : Pointer) : PPyObject; cdecl;
    function Get_Components( AContext : Pointer) : PPyObject; cdecl;
  public
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor Destroy; override;

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Mapping services
    function  MpLength : Integer; override;
    function  MpSubscript( obj : PPyObject) : PPyObject; override;
    // Properties
    property DelphiObject: TComponent read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the string items of the TStrings collection.
  }
  TStringsAccess = class(TContainerAccess)
  private
    function GetContainer: TStrings;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function SupportsWrite : Boolean; override;

    property Container : TStrings read GetContainer;
  end;

  {
    Access to the TObject items of the TStrings.Objects collection.
  }
  TStringsObjectsAccess = class(TStringsAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function IndexOf(AValue : PPyObject) : Integer; override;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; override;
    class function Name : String; override;
  end;

  {
     PyObject wrapping TStrings
     Note that you can assign a Python sequence to a TStrings (X.Assign([1, 2, 3]))
     Note that X[1] will return a string, where as X['key'] will return the object associated
     with the string 'key'.
     Provides a mapping interface to a Delphi strings object
     Exposes Methods Add, AddObject, Delete, IndexOf and Clear
  }
  TPyDelphiStrings = class (TPyDelphiPersistent)
  private
    function  GetDelphiObject: TStrings;
    procedure SetDelphiObject(const Value: TStrings);
  protected
    // Exposed Methods
    function Add_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function AddObject_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Clear_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Delete_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function IndexOf_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BeginUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function EndUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_Capacity( AContext : Pointer) : PPyObject; cdecl;
    function Get_Text( AContext : Pointer) : PPyObject; cdecl;
    function Get_Objects( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_Capacity( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Text( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    // Virtual Methods
    function Assign(ASource : PPyObject) : PPyObject; override;
  public
    function  Repr : PPyObject; override;
    // Mapping services
    function  MpLength : Integer; override;
    function  MpSubscript( obj : PPyObject) : PPyObject; override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Properties
    property DelphiObject: TStrings read GetDelphiObject write SetDelphiObject;
  end;

  {
     PyObject wrapping TBasicAction
     Exposes methods Execute, Update
     Exposes property ActionComponent
  }
  TPyDelphiBasicAction = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TBasicAction;
    procedure SetDelphiObject(const Value: TBasicAction);
  protected
    // Exposed Methods
    function Execute_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Update_Wrapper(args : PPyObject) : PPyObject; cdecl;
{$IFDEF DELPHI6_OR_HIGHER}
    // Property Getters
    function Get_ActionComponent( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_ActionComponent( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
{$ENDIF}
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TBasicAction read GetDelphiObject write SetDelphiObject;
  end;

  { Helper functions }

  function ShiftToPython(AShift : TShiftState) : PPyObject;

implementation

uses
  TypInfo;

{ Register the wrappers, the globals and the constants }
type
  TClassesRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TClassesRegistration }

procedure TClassesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('ssShift',  'ssShift');
  APyDelphiWrapper.DefineVar('ssAlt',    'ssAlt');
  APyDelphiWrapper.DefineVar('ssCtrl',   'ssCtrl');
  APyDelphiWrapper.DefineVar('ssLeft',   'ssLeft');
  APyDelphiWrapper.DefineVar('ssRight',  'ssRight');
  APyDelphiWrapper.DefineVar('ssMiddle', 'ssMiddle');
  APyDelphiWrapper.DefineVar('ssDouble', 'ssDouble');
end;

function TClassesRegistration.Name: String;
begin
  Result := 'Classes';
end;

procedure TClassesRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPersistent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCollection);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStrings);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBasicAction);
end;

{ Helper functions }

function ShiftToPython(AShift : TShiftState) : PPyObject;

  procedure Append(AList : PPyObject; const AString : String);
  var
    _item : PPyObject;
  begin
    with GetPythonEngine do
    begin
      _item := PyString_FromString(PChar(AString));
      PyList_Append(AList, _item);
      Py_XDecRef(_item);
    end;
  end;

begin
  with GetPythonEngine do
  begin
    Result := PyList_New(0);
    if ssShift in AShift then
      Append(Result, 'ssShift');
    if ssAlt in AShift then
      Append(Result, 'ssAlt');
    if ssCtrl in AShift then
      Append(Result, 'ssCtrl');
    if ssLeft in AShift then
      Append(Result, 'ssLeft');
    if ssRight in AShift then
      Append(Result, 'ssRight');
    if ssMiddle in AShift then
      Append(Result, 'ssMiddle');
    if ssDouble in AShift then
      Append(Result, 'ssDouble');
  end;
end;

{ TPyDelphiPersistent }

function TPyDelphiPersistent.Assign(ASource: PPyObject): PPyObject;
var
  _object : TObject;
begin
  if CheckObjAttribute(ASource, 'First parameter', TPersistent, _object) then
  begin
    DelphiObject.Assign(TPersistent(_object));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

function TPyDelphiPersistent.Assign_Wrapper(args: PPyObject): PPyObject;
var
  _obj : PPyObject;
begin
  with GetPythonEngine do
  begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, 'O:Assign', [@_obj] ) <> 0 then
      Result := Self.Assign(_obj)
    else
      Result := nil;
  end;
end;

class function TPyDelphiPersistent.DelphiObjectClass: TClass;
begin
  Result := TPersistent;
end;

function TPyDelphiPersistent.GetDelphiObject: TPersistent;
begin
  Result := TPersistent(inherited DelphiObject);
end;

function TPyDelphiPersistent.GetNamePath_Wrapper(
  args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':GetNamePath', [] ) <> 0 then begin
      Result := PyString_FromString(PChar(DelphiObject.GetNamePath))
    end else
      Result := nil;
  end;
end;

class procedure TPyDelphiPersistent.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Assign', @TPyDelphiPersistent.Assign_Wrapper,
    'TPersistent.Assign(persistent)'#10 +
    'Assigns to this object the values of another TPersistent object');
  PythonType.AddMethod('GetNamePath', @TPyDelphiPersistent.GetNamePath_Wrapper,
    'TPersistent.GetNamePath()'#10 +
    'Returns the name of the object as it appears in the Object Inspector.');
end;

procedure TPyDelphiPersistent.SetDelphiObject(const Value: TPersistent);
begin
  inherited DelphiObject := Value;
end;

{ TCollectionAccess }

class function TCollectionAccess.ExpectedContainerClass: TClass;
begin
  Result := TCollection;
end;

function TCollectionAccess.GetContainer: TCollection;
begin
  Result := TCollection(inherited Container);
end;

function TCollectionAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Items[AIndex] );
end;

function TCollectionAccess.GetSize: Integer;
begin
  Result := Container.Count;
end;

function TCollectionAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  _obj : TPyObject;
  _item : TCollectionItem;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if PyInt_Check(AValue) then
    begin
      _item := Container.FindItemID(PyInt_AsLong(AValue));
      if Assigned(_item) then
        Result := _item.Index;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and (TPyDelphiObject(_obj).DelphiObject is TCollectionItem) then
      begin
        _item := TCollectionItem(TPyDelphiObject(_obj).DelphiObject);
        for i := 0 to Container.Count-1 do
          if Container.Items[i] = _item then
          begin
            Result := i;
            Break;
          end;
      end;
    end;
  end;
end;

class function TCollectionAccess.Name: String;
begin
  Result := 'TCollection.Items';
end;

class function TCollectionAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiCollection }

function TPyDelphiCollection.Add_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Add', [] ) <> 0 then
    Result := Wrap(DelphiObject.Add)
  else
    Result := nil;
end;

function TPyDelphiCollection.BeginUpdate_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':BeginUpdate', [] ) <> 0 then begin
    DelphiObject.BeginUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiCollection.Clear_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Clear', [] ) <> 0 then begin
    (DelphiObject as TCollection).Clear;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiCollection.Delete_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Delete', [@Index] ) <> 0 then
  begin
    if not CheckIndex(Index, DelphiObject.Count) then
      Result := nil
    else
    begin
      DelphiObject.Delete(Index);
      Result := GetPythonEngine.ReturnNone;
    end;
  end
  else
    Result := nil;
end;

class function TPyDelphiCollection.DelphiObjectClass: TClass;
begin
  Result := TCollection;
end;

function TPyDelphiCollection.EndUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':EndUpdate', [] ) <> 0 then begin
    DelphiObject.EndUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

class function TPyDelphiCollection.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TCollectionAccess;
end;

function TPyDelphiCollection.GetDelphiObject: TCollection;
begin
  Result := TCollection(inherited DelphiObject);
end;

function TPyDelphiCollection.Get_Count(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := PyInt_FromLong(DelphiObject.Count);
  end;
end;

function TPyDelphiCollection.Get_Items(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiContainer do
      Setup(Self.PyDelphiWrapper, Self.ContainerAccess.Clone);
  end;
end;

{$IFDEF DELPHI6_OR_HIGHER}
function TPyDelphiCollection.Get_Owner(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Wrap(DelphiObject.Owner);
  end;
end;
{$ENDIF}

function TPyDelphiCollection.Insert_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Insert', [@Index] ) <> 0 then
    Result := Wrap(DelphiObject.Insert(Index))
  else
    Result := nil;
end;

class procedure TPyDelphiCollection.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Count', @TPyDelphiCollection.Get_Count, nil,
        'Returns the count of collection items', nil);
      AddGetSet('Items', @TPyDelphiCollection.Get_Items, nil,
        'Returns an iterator over the collection items', nil);
{$IFDEF DELPHI6_OR_HIGHER}
      AddGetSet('Owner', @TPyDelphiCollection.Get_Owner, nil,
        'Returns the Owner of the collection', nil);
{$ENDIF}
    end;
end;

class procedure TPyDelphiCollection.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Insert', @TPyDelphiCollection.Insert_Wrapper,
    'TCollection.Insert(Index)'#10 +
    'Inserts a new collection item to the collection at the Index position');
  PythonType.AddMethod('Add', @TPyDelphiCollection.Add_Wrapper,
    'TCollection.Add()'#10 +
    'Adds a collection item to the collection');
  PythonType.AddMethod('Clear', @TPyDelphiCollection.Clear_Wrapper,
    'TCollection.Clear()'#10 +
    'Clears all collection items');
  PythonType.AddMethod('Delete', @TPyDelphiCollection.Delete_Wrapper,
    'TCollection.Delete(Index)'#10 +
    'Deletes a single item from the collection.');
  PythonType.AddMethod('BeginUpdate', @TPyDelphiCollection.BeginUpdate_Wrapper,
    'TCollection.BeginUpdate()'#10 +
    'Suspends screen repainting.');
  PythonType.AddMethod('EndUpdate', @TPyDelphiCollection.EndUpdate_Wrapper,
    'TCollection.EndUpdate()'#10 +
    'Re-enables screen repainting.');
end;

procedure TPyDelphiCollection.SetDelphiObject(const Value: TCollection);
begin
  inherited DelphiObject := Value;
end;

{ TComponentsAccess }

class function TComponentsAccess.ExpectedContainerClass: TClass;
begin
  Result := TComponent;
end;

function TComponentsAccess.GetContainer: TComponent;
begin
  Result := TComponent(inherited Container);
end;

function TComponentsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Components[AIndex] );
end;

function TComponentsAccess.GetSize: Integer;
begin
  Result := Container.ComponentCount;
end;

function TComponentsAccess.IndexOf(AValue: PPyObject): Integer;
Var
  i : Integer;
  S : string;
  _obj : TPyObject;
  _value : TObject;
  _comp : TComponent;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if PyString_Check(AValue) then
    begin
      S := PyString_AsDelphiString(AValue);
      for i := 0 to Container.ComponentCount-1 do
        if SameText( Container.Components[i].Name, S) then
        begin
          Result := i;
          Break;
        end;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if _obj is TPyDelphiObject then
      begin
        _value := TPyDelphiObject(_obj).DelphiObject;
        if _value is TComponent then
        begin
          _comp := TComponent(_value);
          for i := 0 to Container.ComponentCount-1 do
            if Container.Components[i] = _comp then
            begin
              Result := i;
              Break;
            end;
        end;
      end;
    end;
  end;
end;

class function TComponentsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiObjectNexus }
{ used by TPyDelphiObject to get free notification }

type
  TPyDelphiObjectNexusEvent = procedure(Sender: TObject; AComponent: TComponent) of object;
  TPyDelphiObjectNexus = class(TComponent)
  private
    FOnFreeNotify: TPyDelphiObjectNexusEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property OnFreeNotify: TPyDelphiObjectNexusEvent read FOnFreeNotify write FOnFreeNotify;
  end;

{ TPyDelphiObjectNexus }

procedure TPyDelphiObjectNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(FOnFreeNotify) then
    FOnFreeNotify(Self, AComponent);
  inherited Notification(AComponent, Operation);
end;

{ TPyDelphiComponent }

function TPyDelphiComponent.BindMethodsToEvents(args: PPyObject): PPyObject;
var
  i : Integer;
  j : Integer;
  d : PPyObject;
  s : PChar;
  obj : PPyObject;
  objMethod : PPyObject;
  objComp : PPyObject;
  key : PPyObject;
  keys : PPyObject;
  _idx : Integer;
  _name : String;
  _prefix : String;
  _compName : String;
  _eventName : String;
  _comp : TComponent;
  _pair : PPyObject;
  _bindings : PPyObject;
  _type : PPyTypeObject;
begin
  _prefix := 'handle_';
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    Result := nil;
    s := nil;
    if PyArg_ParseTuple( args, '|s:BindMethodsToEvents', [@s] ) <> 0 then
    begin
      if (s <> nil) and (s^ <> #0) then
        _prefix := s;
      _bindings := PyList_New(0);
      try
        _type := GetSelf.ob_type;
        while _type <> nil do
        begin
          d := _type.tp_dict;
          if Assigned(d) and PyDict_Check(d) then
          begin
            keys := PyDict_Keys(d);
            try
              if PySequence_Check(keys) = 1 then
                for i := 0 to PySequence_Length(keys)-1 do
                begin
                  key := PySequence_GetItem(keys, i);
                  obj := PyDict_GetItem(d, key); // borrowed ref
                  objComp := nil;
                  try
                    if PyCallable_Check(obj) = 1 then
                    begin
                      _name := PyObjectAsString(key);
                      if SameText(Copy(_name, 1, Length(_prefix)), _prefix) then
                      begin
                        System.Delete(_name, 1, Length(_prefix));
                        _idx := -1;
                        for j := Length(_name) downto 1 do
                          if _name[j] = '_' then
                          begin
                            _idx := j;
                            Break;
                          end;
                        if _idx > -1 then
                        begin
                          _compName := Copy(_name, 1, _idx-1);
                          _eventName := Copy(_name, _idx+1, MaxInt);
                          if SameText(_compName, 'Self') then
                          begin
                            _comp := Self.DelphiObject;
                            objComp := GetSelf;
                            Py_IncRef(objComp);
                          end
                          else
                          begin
                            _comp := Self.DelphiObject.FindComponent(_compName);
                            if Assigned(_comp) then
                              objComp := Wrap(_comp);
                          end;
                          if not Assigned(_comp) and not Assigned(objComp) then
                          begin
                            objComp := PyObject_GetAttrString(GetSelf, PChar(_compName));
                            if Assigned(objComp) then
                            begin
                              if IsDelphiObject(objComp) and (PythonToDelphi(objComp) is TPyDelphiComponent) then
                                _comp := TPyDelphiComponent(PythonToDelphi(objComp)).DelphiObject;
                            end
                            else
                              PyErr_Clear;
                          end;
                          if Assigned(_comp) and Assigned(objComp) and IsPublishedProp(_comp, _eventName) then
                          begin
                            objMethod := PyObject_GenericGetAttr(GetSelf, key);
                            if PyErr_Occurred <> nil then
                              Exit;
                            PyObject_SetAttrString(objComp, PChar(_eventName), objMethod);
                            if PyErr_Occurred <> nil then
                              Exit
                            else
                            begin
                              _pair := PyTuple_New(3);
                              PyTuple_SetItem(_pair, 0, PyString_FromString(PChar(_compName)));
                              PyTuple_SetItem(_pair, 1, PyString_FromString(PChar(_eventName)));
                              PyTuple_SetItem(_pair, 2, objMethod);
                              PyList_Append(_bindings, _pair);
                            end;
                          end;
                        end;
                      end;
                    end;
                  finally
                    Py_XDecRef(objComp);
                    Py_DecRef(key);
                  end;
                end; // for
            finally
              Py_DecRef(keys);
            end;
          end;
          _type := _type.tp_base;
        end;
        Result := _bindings;
        _bindings := nil;
      finally
        Py_XDecRef(_bindings);
      end;
    end;
  end;
end;

function TPyDelphiComponent.Get_ComponentCount(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := PyInt_FromLong(DelphiObject.ComponentCount);
  end;
end;

function TPyDelphiComponent.Get_Components(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiContainer do
      Setup(Self.PyDelphiWrapper, Self.ContainerAccess.Clone);
  end;
end;

function TPyDelphiComponent.Get_Owner(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Wrap(DelphiObject.Owner);
  end;
end;

function TPyDelphiComponent.GetParentComponent_Wrapper(
  args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':GetParentComponent', [] ) <> 0 then begin
      Result := Wrap(DelphiObject.GetParentComponent)
    end else
      Result := nil;
  end;
end;

function TPyDelphiComponent.HasParent_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':HasParent', [] ) <> 0 then begin
      Result := VariantAsPyObject(DelphiObject.HasParent)
    end else
      Result := nil;
  end;
end;

function TPyDelphiComponent.GetAttrO(key: PPyObject): PPyObject;
Var
  Component: TComponent;
  Name: string;
begin
  Result := nil;
  if Assigned(DelphiObject) then
  begin
    if GetPythonEngine.PyString_Check(Key) then
    begin
      Name := GetPythonEngine.PyString_AsDelphiString(Key);
      // try a sub component
      Component := DelphiObject.FindComponent(Name);
      if Component <> nil then
        Result := Wrap(Component);
    end;
  end;

  if not Assigned(Result) then
    Result := inherited GetAttrO(key);
end;

function TPyDelphiComponent.GetDelphiObject: TComponent;
begin
  Result := TComponent(inherited DelphiObject);
end;

procedure TPyDelphiComponent.SetDelphiObject(const Value: TComponent);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiComponent.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('ComponentCount', @TPyDelphiComponent.Get_ComponentCount, nil,
        'Returns the owned component count', nil);
      AddGetSet('Owner', @TPyDelphiComponent.Get_Owner, nil,
        'Returns the Component Owner', nil);
      AddGetSet('Components', @TPyDelphiComponent.Get_Components, nil,
        'Returns an iterator over the owned components', nil);
    end;
end;

class procedure TPyDelphiComponent.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('GetParentComponent', @TPyDelphiComponent.GetParentComponent_Wrapper,
    'TComponent.GetParentComponent()'#10 +
    'Returns the parent of a component.');
  PythonType.AddMethod('HasParent', @TPyDelphiComponent.HasParent_Wrapper,
    'TComponent.HasParent()'#10 +
    'Indicates whether the component has a parent to handle its filing.');
  PythonType.AddMethod('BindMethodsToEvents', @TPyDelphiComponent.BindMethodsToEvents,
    'TComponent.BindMethodsToEvents(prefix)'#10 +
    'Connects methods to component events if they are named using the following patter: Prefix_ComponentName_EventName.'+#10+
    'Example: def handle_button1_OnClick(Sender): pass'+#10+
    'The function returns a list of tuples. Each tuple contains the name of the component, the name of the event and the method object assigned to the event.'+#10+
    'Note that the prefix parameter is optional and will default to "handle_".');
end;

class procedure TPyDelphiComponent.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Services.Mapping  := PythonType.Services.Mapping + [msLength, msSubscript];
end;

class function TPyDelphiComponent.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TComponentsAccess;
end;

function TPyDelphiComponent.MpLength: Integer;
begin
  Result := SqLength;
end;

function TPyDelphiComponent.MpSubscript(obj: PPyObject): PPyObject;
var
  _name : String;
  _comp : TComponent;
begin
  with GetPythonEngine do
  begin
    if PyInt_Check(obj) then
      Result := SqItem(PyInt_AsLong(obj))
    else if PyString_Check(obj) then
    begin
      _name := String(PyString_AsDelphiString(obj));
      _comp := DelphiObject.FindComponent(_name);
      if Assigned(_comp) then
        Result := Wrap(_comp)
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_KeyError^, PChar(_name));
      end;
    end
    else
    begin
      Result := nil;
      PyErr_SetString (PyExc_KeyError^, 'Key must be a string');
    end;
  end;
end;

procedure TPyDelphiComponent.SubscribeToFreeNotification;
begin
  Assert(Assigned(DelphiObject));
  if not Assigned(fFreeNotificationComp) then
  begin
    fFreeNotificationComp := TPyDelphiObjectNexus.Create(nil);
    TPyDelphiObjectNexus(fFreeNotificationComp).OnFreeNotify := HandleFreeNotificationEvent;
  end;
  DelphiObject.FreeNotification(fFreeNotificationComp);
end;

procedure TPyDelphiComponent.UnSubscribeToFreeNotification;
begin
  Assert(Assigned(DelphiObject));
  if Assigned(fFreeNotificationComp) then
    DelphiObject.RemoveFreeNotification(fFreeNotificationComp);
end;

function TPyDelphiComponent.CreateComponent(AOwner: TComponent): TComponent;
begin
  Result := TComponentClass(DelphiObjectClass).Create(AOwner);
end;

constructor TPyDelphiComponent.CreateWith(APythonType: TPythonType;
  args: PPyObject);
var
  _obj : PPyObject;
  _owner : TObject;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple( args, 'O:Create', [@_obj] ) <> 0 then
  begin
    _owner := nil;
    if CheckObjAttribute(_obj, 'Owner', TComponent, _owner) then
    begin
      DelphiObject := CreateComponent(TComponent(_owner));
      Owned := not Assigned(_owner);
      fCanFreeOwnedObject := True;
    end;
  end;
end;

destructor TPyDelphiComponent.Destroy;
begin
  inherited;
  fFreeNotificationComp.Free; // Free fFreeNotificationComp after inherited, because inherited will do DelphiObject := nil which call UnsubscribeFreeNotification
end;

procedure TPyDelphiComponent.HandleFreeNotificationEvent(Sender: TObject;
  AComponent: TComponent);
begin
  Notify(AComponent);
end;

class function TPyDelphiComponent.DelphiObjectClass: TClass;
begin
  Result := TComponent;
end;

{ TStringsAccess }

class function TStringsAccess.ExpectedContainerClass: TClass;
begin
  Result := TStrings;
end;

function TStringsAccess.GetContainer: TStrings;
begin
  Result := TStrings(inherited Container);
end;

function TStringsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := GetPythonEngine.PyString_FromString( PChar(Container[AIndex]) );
end;

function TStringsAccess.GetSize: Integer;
begin
  Result := Container.Count;
end;

function TStringsAccess.IndexOf(AValue: PPyObject): Integer;
begin
  Result := Container.IndexOf(GetPythonEngine.PyObjectAsString(AValue));
end;

function TStringsAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  with GetPythonEngine do
  begin
    if PyString_Check(AValue) then
    begin
      Container[AIndex] := PyString_AsDelphiString(AValue);
      Result := True;
    end
    else
    begin
      Result := False;
      PyErr_SetString (PyExc_AttributeError^, 'You can only assign strings to TStrings items');
    end;
  end
end;

class function TStringsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

class function TStringsAccess.SupportsWrite: Boolean;
begin
  Result := True;
end;

{ TStringsObjectsAccess }

function TStringsObjectsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Objects[AIndex] );
end;

function TStringsObjectsAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  _obj : TPyObject;
  _value : TObject;
begin
  Result := -1;
  if IsDelphiObject(AValue) then
  begin
    _obj := PythonToDelphi(AValue);
    if _obj is TPyDelphiObject then
    begin
      _value := TPyDelphiObject(_obj).DelphiObject;
      for i := 0 to Container.Count-1 do
      begin
        if Container.Objects[i] = _value then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

class function TStringsObjectsAccess.Name: String;
begin
  Result := 'Objects';
end;

function TStringsObjectsAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) and (PythonToDelphi(AValue) is TPyDelphiObject) then
    begin
      Container.Objects[AIndex] := TPyDelphiObject(PythonToDelphi(AValue)).DelphiObject;
      Result := True;
    end
    else
    begin
      Result := False;
      PyErr_SetString (PyExc_AttributeError^, 'You can only assign Delphi wrappers to Objects items');
    end;
  end
end;

{ TPyDelphiStrings }

function TPyDelphiStrings.AddObject_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PChar;
  _obj : PPyObject;
  _value : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:AddObject', [@PStr, @_obj] ) <> 0 then
    begin
      if CheckObjAttribute(_obj, 'The second argument of AddObject', TObject, _value) then
          Result := PyInt_FromLong(DelphiObject.AddObject(PStr, _value))
      else
        Result := nil;
    end
    else
      Result := nil;
end;

function TPyDelphiStrings.Add_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:Add', [@PStr] ) <> 0 then
      Result := PyInt_FromLong(DelphiObject.Add(PStr))
    else
      Result := nil;
end;

function TPyDelphiStrings.Assign(ASource: PPyObject): PPyObject;
var
  i : Integer;
  _item : PPyObject;
begin
  with GetPythonEngine do
  begin
    if not IsDelphiObject(ASource) and (PySequence_Check(ASource) <> 0) then
    begin
      DelphiObject.BeginUpdate;
      try
        DelphiObject.Clear;
        DelphiObject.Capacity := PySequence_Length(ASource);
        for i := 0 to PySequence_Length(ASource)-1 do
        begin
          _item := PySequence_GetItem(ASource, i);
          try
            DelphiObject.Add(PyObjectAsString(_item));
          finally
            Py_DecRef(_item);
          end;
        end;
      finally
        DelphiObject.EndUpdate;
      end;
      Result := ReturnNone;
    end
    else
      Result := inherited Assign(ASource);
  end;
end;

function TPyDelphiStrings.BeginUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':BeginUpdate', [] ) <> 0 then begin
    DelphiObject.BeginUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiStrings.Clear_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Clear', [] ) <> 0 then begin
    DelphiObject.Clear;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiStrings.Delete_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Delete', [@Index] ) <> 0 then
  begin
    if CheckIndex(Index, DelphiObject.Count) then
    begin
      DelphiObject.Delete(Index);
      Result := GetPythonEngine.ReturnNone;
    end
    else
      Result := nil
  end
  else
    Result := nil;
end;

class function TPyDelphiStrings.DelphiObjectClass: TClass;
begin
  Result := TStrings;
end;

function TPyDelphiStrings.EndUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':EndUpdate', [] ) <> 0 then begin
    DelphiObject.EndUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

class function TPyDelphiStrings.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TStringsAccess;
end;

function TPyDelphiStrings.GetDelphiObject: TStrings;
begin
  Result := TStrings(inherited DelphiObject);
end;

function TPyDelphiStrings.Get_Capacity(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := PyInt_FromLong(DelphiObject.Capacity);
  end;
end;

function TPyDelphiStrings.Get_Objects(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiContainer do
      Setup(Self.PyDelphiWrapper, TStringsObjectsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
  end;
end;

function TPyDelphiStrings.Get_Text(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := PyString_FromString(PChar(GetPythonEngine.CleanString(DelphiObject.Text)));
  end;
end;

function TPyDelphiStrings.IndexOf_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 's:IndexOf', [@PStr] ) <> 0 then
    Result := GetPythonEngine.PyInt_FromLong(DelphiObject.IndexOf(PStr))
  else
    Result := nil;
end;

function TPyDelphiStrings.LoadFromFile_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 's:LoadFromFile', [@PStr] ) <> 0 then
  begin
    DelphiObject.LoadFromFile(String(PStr));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

function TPyDelphiStrings.MpLength: Integer;
begin
  Result := DelphiObject.Count;
end;

function TPyDelphiStrings.MpSubscript(obj: PPyObject): PPyObject;
Var
  S : string;
  Index : integer;
begin
  with GetPythonEngine do
  begin
    if PyInt_Check(obj) then
      Result := SqItem(PyInt_AsLong(obj))
    else
    begin
      S := PyObjectAsString(obj);
      if S <> '' then begin
        Index := DelphiObject.IndexOf(S);
        if Index >= 0 then begin
          if Assigned(DelphiObject.Objects[Index]) then
            Result := Wrap(DelphiObject.Objects[Index])
          else
            Result := GetPythonEngine.ReturnNone;
        end else with GetPythonEngine do begin
          PyErr_SetString (PyExc_KeyError^, PChar(S));
          Result := nil;
        end;
      end else with GetPythonEngine do begin
        PyErr_SetString (PyExc_KeyError^, '<Empty String>');
        Result := nil;
      end;
    end;
  end;
end;

class procedure TPyDelphiStrings.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Capacity', @TPyDelphiStrings.Get_Capacity, @TPyDelphiStrings.Set_Capacity,
        'Indicates the number of strings the TStrings object can hold.', nil);
      AddGetSet('Text', @TPyDelphiStrings.Get_Text, @TPyDelphiStrings.Set_Text,
        'Lists the strings in the TStrings object as a single string with the individual strings delimited by carriage returns and line feeds.', nil);
      AddGetSet('Objects', @TPyDelphiStrings.Get_Objects, nil,
        'Represents a set of objects that are associated one with each of the strings in the Strings property.', nil);
    end;
end;

class procedure TPyDelphiStrings.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Add', @TPyDelphiStrings.Add_Wrapper,
    'TStrings.Add(s)'#10 +
    'Adds a string to the TStrings object and returns the index position');
  PythonType.AddMethod('AddObject', @TPyDelphiStrings.AddObject_Wrapper,
    'TStrings.AddObject(s, delphiobject)'#10 +
    'Adds a string and an associated Delphi object to the Strings and returns the index position');
  PythonType.AddMethod('Clear', @TPyDelphiStrings.Clear_Wrapper,
    'TStrings.Clear()'#10 +
    'Clears all strings from a TStrings (and the associated objects');
  PythonType.AddMethod('Delete', @TPyDelphiStrings.Delete_Wrapper,
    'TStrings.Delete(i)'#10 +
    'Deletes the string at Index i (and the associated object');
  PythonType.AddMethod('IndexOf', @TPyDelphiStrings.IndexOf_Wrapper,
    'TStrings.IndexOf(s)'#10 +
    'Returns the Index of a string s or -1 if not found');
  PythonType.AddMethod('BeginUpdate', @TPyDelphiStrings.BeginUpdate_Wrapper,
    'TStrings.BeginUpdate()'#10 +
    'Enables the TStrings object to track when the list of strings is changing.');
  PythonType.AddMethod('EndUpdate', @TPyDelphiStrings.EndUpdate_Wrapper,
    'TStrings.EndUpdate()'#10 +
    'Enables the TStrings object to keep track of when the list of strings has finished changing.');
  PythonType.AddMethod('LoadFromFile', @TPyDelphiStrings.LoadFromFile_Wrapper,
    'TStrings.LoadFromFile(filename)'#10 +
    'Fills the list with the lines of text in a specified file.');
  PythonType.AddMethod('SaveToFile', @TPyDelphiStrings.SaveToFile_Wrapper,
    'TStrings.SaveToFile(filename)'#10 +
    'Saves the strings in the list to the specified file.');
end;

function TPyDelphiStrings.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString( PChar(Format('<Delphi TStrings at %x>',
         [Integer(self)])) );
end;

function TPyDelphiStrings.SaveToFile_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 's:SaveToFile', [@PStr] ) <> 0 then
  begin
    DelphiObject.SaveToFile(String(PStr));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

procedure TPyDelphiStrings.SetDelphiObject(const Value: TStrings);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiStrings.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Services.Mapping  := PythonType.Services.Mapping  + [msLength, msSubscript];
end;

{ TPyDelphiBasicAction }

class function TPyDelphiBasicAction.DelphiObjectClass: TClass;
begin
  Result := TBasicAction;
end;

function TPyDelphiBasicAction.Execute_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':Execute', [] ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.Execute );
    end else
      Result := nil;
  end;
end;

function TPyDelphiBasicAction.GetDelphiObject: TBasicAction;
begin
  Result := TBasicAction(inherited DelphiObject);
end;

{$IFDEF DELPHI6_OR_HIGHER}
function TPyDelphiBasicAction.Get_ActionComponent(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Wrap(DelphiObject.ActionComponent);
  end;
end;
{$ENDIF}

class procedure TPyDelphiBasicAction.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
{$IFDEF DELPHI6_OR_HIGHER}
      AddGetSet('ActionComponent', @TPyDelphiBasicAction.Get_ActionComponent, @TPyDelphiBasicAction.Set_ActionComponent,
        'Indicates the client component that caused this action to execute.', nil);
{$ENDIF}
    end;
end;

class procedure TPyDelphiBasicAction.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Execute', @TPyDelphiBasicAction.Execute_Wrapper,
    'TBasicAction.Execute()'#10 +
    'Generates an OnExecute event.');
  PythonType.AddMethod('Update', @TPyDelphiBasicAction.Update_Wrapper,
    'TBasicAction.Update()'#10 +
    'Provides an opportunity to execute centralized code when an application is idle.');
end;

procedure TPyDelphiBasicAction.SetDelphiObject(const Value: TBasicAction);
begin
  inherited DelphiObject := Value;
end;

{$IFDEF DELPHI6_OR_HIGHER}
function TPyDelphiBasicAction.Set_ActionComponent(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _actionComponent : TObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckObjAttribute(AValue, 'ActionComponent', TComponent, _actionComponent) then
    begin
      Self.DelphiObject.ActionComponent := TComponent(_actionComponent);
      Result := 0;
    end
    else
      Result := -1;
  end;
end;
{$ENDIF}

function TPyDelphiBasicAction.Update_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':Update', [] ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.Update );
    end else
      Result := nil;
  end;
end;

function TPyDelphiStrings.Set_Capacity(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _capacity : Integer;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckIntAttribute(AValue, 'Capacity', _capacity) then
    begin
      DelphiObject.Capacity := _capacity;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

function TPyDelphiStrings.Set_Text(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _text : String;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckStrAttribute(AValue, 'Text', _text) then
    begin
      DelphiObject.Text := _text;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

initialization
  RegisteredUnits.Add(TClassesRegistration.Create);
end.
