(*-----------------------------------------------------------------------------
 Unit Name: WrapDelphi
 Author:    Kiriakos Vlahos
 Date:      24-Feb-2005
 Purpose:   Provide automatic wrapping of Delphi variables utilising RTTI
 Version:   1.11

 Contributors:
   Morgan Martinet (mmm@free.fr)

 Features:
   Check our Wiki at: http://py4d.pbwiki.com/.                          

   Published properties and methods compiled with {$METHODINFO ON} are
   handled automatically (Note that METHODINFO can be used only with Delphi7
   or later, but all the other wrapping features will work with previous
   versions of Delphi starting from Delphi5).
   Moreover common methods and properties of
   the following frequently used Delphi classes are also exported
   (Note that this list is not exhaustive):

     TObject (ClassName, Free, InheritsFrom)
       TPersistent (Assign)
         TCollection (sequence interface, Items, Count, Insert, Add, Clear)
         TStrings (mapping interface, Text, Add, AddObject, Delete, IndexOf, Clear)
         TComponent (Event properties, Subproperties, Owner, ComponentCount, Components)
           TControl (Parent)
             TWinControl (ControlCount, Controls)
               TForm (Show, ShowModal, Release)


   TStrings, TCollection.Items, TComponent.Components and
   TWinControl.Controls are exposed as sequence/mapping interfaces.
   You can also access the Screen and Application objects, and some other
   constants like mrOk, mrCancel...

   PyDelphiWrapper.RegisterDelphiWrapper allows the customized wrapping of
   additional Delphi classes over which you do not have direct control.
   PyDelphiWrapper.EventHandlers.RegisterHandler() can be used to add event handling
   functionality.  TNotify events are handled out-of-the-box.  To handle
   other types of events you need to write a TEventHandler descendent and
   register the EventHandler.

   A Module level function CreateComponent(ClassName, Owner) is also exported.
   For this function to work, the class needs to be registered using
   Classes.RegisterClass (Some classes are already pre-registered like TForm,
   TApplication, TScreen, TButton, TCheckBox...).

   You can subclass TForm as you would do in Delphi, but you are not able to
   override the Delphi methods in Python. There is also a helper
   function BindMethodsToEvents that can connect your method handlers to the
   component events if you respect a specific pattern for naming your methods:
   handle_ComponentName_EventName  --> handle_Button1_OnClick
   This function is especially useful when you subclass an existing Delphi form,
   as the form will already have all the necessary components setup, but you'll 
   be missing the events to your Python code.
   If you subclass Form in Python and name your class with the same name as
   an existing Delphi form (that must be registered with RegisterClass), 
   then this class will be used to instanciate the form instead of the regular empty TForm.

class TTestForm(Form):
  def __init__(self, Owner):
    self.Caption = self.Caption + ' - changed by Python subclass'
    self.BindMethodsToEvents() # this will connect handle_btnAdd_OnClick to btnAdd.OnClick

  def handle_btnAdd_OnClick(self, Sender):
    self.ListBox1.Items.Add(self.Edit1.Text)
   
  There is also a helper method named SetProps at the TPyDelphiObject level, 
  allowing any wrapped object to do:
     button.SetProps(Left=10, Top=20, Caption='Clickme!)

  You can inspect the published properties of any wrapped object by inspecting the
  __published__ property.
       
  Note that events requiring var parameters like OnCloseQuery will provide a specific object
  containing a single Value property that will hold the actual value of the parameter,
  because Python does not allow modifying the parameters:
    def handle_close_query(self, sender, accept):
      accept.Value = False  # accept = False would have not effect!
       
 Usage:
   Drop a PyDelphiWrapper component on a form, set its engine and module
   properties to a PythonEngine and PythonModule.

   Note that it is very important to add each wrapped Delphi unit to your uses
   clause or you won't access the specific wrappers as they would not be
   registered.
   To make it easier, you can simply add the unit WrapDelphiVCL to your uses
   clause.

   Alternatively create a PyDelphiWrapper component using code,
   set its engine and module properties and initialize e.g.

    PyDelphiWrapper := TPyDelphiWrapper.Create(Self);
    PyDelphiWrapper.Engine := PyEngine;
    PyDelphiWrapper.Module := PythonModule;
    PyDelphiWrapper.Initialize;  // Should only be called if PyDelphiWrapper is created at run time

    Use PyDelphiWrapper.Wrap to wrap a given object
      var
        p : PPyObject;
      begin
        // Wrap the Form itself.
        p := PyDelphiWrapper.Wrap(Form1);
        PythonModule.SetVar( 'Form', p );
        PyEngine.Py_DecRef(p);
      end;

     Look at the demos 31 and 32 for further examples of usage.

 History:
   1.00  24-Feb-2005  Kiriakos Vlahos
     Initial release

   1.01  12-May-2005 Morgan Martinet

  - inherit TPyDelphiWrapper from TPythonClient
  - removed type TPythonTypeCustomCreate as TPythonType now has a new attribute GenerateCreateFunction
    the custom types didn't use the former CanCreate property and thus CreateComponent conflicted
    with the function exposed by TPyDelphiWrapper.
  - changed the boolean parameter of TObjectToPyObject into an enumeration, to help understand
    when you read the code, if the object is owned or not.
  - added property __bound__ to TPyDelphiObject, to know if the wrapper is still bound to the instance.
  - added property __owned__ to the base Delphi wrapper, to know if the wrapper owns the underlying
    object or not.
  - added SqAssItem and SqSlice to the TStringsWrapper
  - moved method Show of the Form wrapper to the Control wrapper
  - added Exception's message to the exception raised in TPyDelphiMethodObject.Call
  - fixed bug in Collection iterator (method Iter and IterNext were swapped)
  - refactored iterators with a common base class
  - added automatic support of sequences and iterators if the wrapper overrides the GetContainerAccessClass method.
  - refactored index checking
  - implemented sequence protocol with more collections
  - used new class method SetupType for configuring the services exposed by the python type
    allowing better polymorphism.
  - TStrings wrapper now inherits from TPersistent wrapper.
  - Fixed bug in TStrings.SqItem that returned a string instead of a wrapped TObject.
  - Changed DelphiObject member field to a property and redefined its type for each subclass,
    in order to avoid casting DelphiObject each time need to invoke an attribute.
    This was too much error prone, especially with Copy&Paste.
  - Added various helper functions to check parameter types.
  - Allowed events with TObject subclasses, using an interfaces IFreeNotification/IFreeNotificationSubscriber
  - Added helper class TFreeNotificationImpl handling the details of the IFreeNotification implementation.
  - Fixed bug when accessing attributes of an unbound wrapper
  - Renamed TPyStringsObject into TPyDelphiStrings for consistency
  - Changed the TForm wrapper into a TCustomForm wrapper
  - Added helper methods ToTuple, ToList to any wrapper supporting sequences (TStrings, TComponent...)
  - Added Objects property to TStrings
  - TStrings can be accessed with an integer to get the string item or with a key string to get
    the associated object.

   1.02  23-May-2005 Morgan Martinet

  - Wrapped TBasicAction
  - Wrapped TActionList
  - Wrapped Screen object
  - Defined TModalResult constants
  - fixed bug when exiting application with Python events still attached -> crash
  - fixed bug in event handlers: when destroying an event, only set the handler to nil if it is our handler!
  - created TEventHandlers collection
  - Moved code that gets/sets events outside of GetAttrO/SetAttrO into TEventHandlers
  - return the associated Python handler of an event (in TPyDelphiObject.GetAttrO)

   1.03  30-May-2005 Morgan Martinet

  - Wrapped TMonitor
  - Wrapped TApplication
  - The wrappers now will try to receive a free notification from the wrapped object. This will always
    work with components and may work with classes that implement IFreeNotification.
  - Refactored the registration of wrappers and helper types.
    Now you don't have to create your TPythonType instance. This will be done automatically in the
    RegisterDelphiWrapper and RegisterHelperType methods.
    You can setup the new type by overriding the SetupType class method of TPyObject.
      procedure RegisterDelphiWrapper(AWrapperClass : TPyDelphiObjectClass);
      RegisterHelperType(APyObjectClass : TPyObjectClass);
    Also, note that RegisterDelphiClass as been renamed RegisterDelphiWrapper and there's no
    ne need to give the associated Delphi class, as the wrapper class will override a new
    class function named DelphiObjectClass that must return the wrapped delphi class.
  - Moved wrappers into new dedicated units for each Delphi VCL unit:
     WrapDelphiClasses, WrapDelphiControls, WrapDelphiForms, WrapDelphiActnList
  - Added a new registration system at the unit level, to allow each dedicated unit to register
    the wrappers of the unit's classes.
  - New way to define getters/setters by using Class methods instead of global functions,
    thanks to Michiel du Toit.

   1.04  30-May-2005 Morgan Martinet

  - Made WrapDelphi compatible with previous versions of Delphi (below 7):
    all the wrapping features are available, and only the dynamic method invocation
    relying on {$METHODINFO ON} is disabled. Have to check compilation with D5 to D6.
  - Allowed subclassing of components. Introduced new wrappers for TForm and TButton.
  - Added new unit WrapDelphiStdCtrls

   1.05  11-June-2005 Morgan Martinet

  - renamed method TObjectToPyObject into Wrap
  - stored default wrapper types pointers into public properties of TPyDelphiWrapper,
    for immediate access (instead of doing a lookup in the list).
  - added class TPyDelphiVarParameter for handling Delphi var Parameters.
  - Defined event for TForm.OnCloseQuery
  - Defined event for TForm.OnClose

   1.06  13-June-2005 Morgan Martinet

  - Created wrappers for all controls of the StdCtrls unit.
  - Created wrappers for all controls of the ExtCtrls unit in new unit WrapDelphiExtCtrls.
  - Added property __published__ to TPyDelphiObject, that will return the list of all published properties
    of the wrapped class. This can be use to know which properties can be accessed and for documenting...
  - Made Helper types visible at a module level, because Point, Rect... are helper types.
  - Added wrapper for TPoint
  - Implemented method TScreen.MonitorFromPoint using Point object.

   1.07  25-June-2005 Morgan Martinet

  - When creating an instance of a form (with a Python subclass of Form), if the Owner is Application,
    then we use Application.CreateForm instead of just instanciating the metaclass, otherwise the Application
    will never have a Main form.
  - Started making a Python dll module hosting the Delphi wrappers.
  - fixed a declaration error of the property setters in TApplication wrapper
  - Added method RegisterFunction to TPyDelphiWrapper
  - Wrapped api FreeConsole in WrapDelphiForms
  - Added method SetProps at the TPyDelphiObject level, allowing any wrapped object to do:
     button.SetProps(Left=10, Top=20, Caption='Clickme!)
  - Wrapped procedure Abort
  - Created new type for wrapping TRect records.
  - New behaviour with forms: if you subclass Form in Python and name your class with the same name as
    a Delphi form (that must be registered with RegisterClass), then this class will be used to instanciate
    the form instead of the regular empty TForm.
  - Added a fake get/set method to TPyDelphiObject and create get/set definitions for each published property, using
    those fake methods that won't do anything, because the property value will be fetched in the GetAttr method,
    before even trying to use the python properties.
    This will help a lot documenting existing wrappers, using regular python tools, and it will also allow the
    use of the code insight provided by the IDE.

    1.08  16-July-2005 Morgan Martinet

  - Added method BindMethodsToEvents to TComponent wrapper. It will allow a subclassed form
    to automatically bind its controls to the form's methods, if you respect a specific naming
    convention. Each method must be named like:
    def handle_MyComponent_OnClick(self, sender): pass

    Note that for the hooking the form's properties, you have to use a special component name "Self":
    def handle_Self_OnCloseQuery(self, sender, CanClose): pass

    Note that BindMethodsToEvents accepts a default parameter for specifying the expected prefix,
    which defaults to "handle_".

    Note that BindMethodsToEvents returns a list of tuples. Each tuple contains:
    ComponentName, EventName, MethodObject

    This method is especially useful if you create a base form in Delphi, using the form designer,
    with no code (or not much), then you subclass this form in Python, provide events that will
    be automatically be connected when you invoke BindMethodsToEvents in the __init__ handler.

  - Finished cleanup of the property getters (global function --> method)

    1.09  18-Dec-2005 Morgan Martinet

  - Added new unit WrapDelphiWindows (to define a couple of symbols only)
  - Added new unit WrapDelphiComCtrls
  - Added new unit WrapDelphiGrids
  - Added new unit WrapDelphiGraphics
  - Added new unit WrapDelphiButtons
  - Wrapped TSize
  - Wrapped TCanvas, TGraphic, TBitmap, TMetaFile, TIcon, TPicture
  - Wrapped TKeyPressEvent and TKeyEvent
  - Made a breaking change when dealing with property sets:
    now we expect a sequence of strings. Each string should have the name as the enumeration in the set.
    Ex:  MainForm.Anchors = ['akLeft', 'akTop']
    Of course, a set property will now return a list of strings.
    In the past, it would have returned an integer containing all the bits of the set,
    and it would have accepted to assign either the same kind of integer value or
    a string like "[akLeft, akTop]".
  - Made a breaking change when dealing with property enumerations:
    return a string representing its value instead of the ordinal value.
  - You don't need to call explicitely RegisterClass for your registered Python types as it will be done
    automatically for you in RegisterDelphiWrapper. But it is still usefull if you need to create VCL objects
    that have no wrapper, using the CreateComponent helper function.

    1.10  24-Feb-2006 Morgan Martinet

  - Wrapped TPageControl and TTabSheet

    1.11  14-Mar-2006 Morgan Martinet

  - Added methods Repaint and Invalidate to the TControl wrapper 
  - Fixed bug when running WrapDelphi without Assertions in the compiler options
    thanks to a report from Dominique Whali
  - made fields fDefaultIterType and fDefaultContainerType of TPyDelphiWrapper protected 
    
 TODO:

  - Extend SetProps: if property receiving the value is a TStrings and the value a sequence,
    then assign the sequence content to the TStrings.
  - what do we do if the Delphi code raises an exception when invoked by Python?
    Shouldn't we resurface it as a Python exception?
  - can we debug the Python code executed from a triggered event? Presently not, as we directly ask Python
    to execute a specific callable...
  - Create a simple app that just initializes Python and executes a script? To avoid having a console...
  - Bug with Delphi pyd: can't change the application title, because TApplication creates its own handle
  - Wrap TApplicationEvents. In fact define the events used by TApplicationEvents.
  - Wrap TMenu and Toolbar
  - Wrap TObjectList
  - Unit Test all exposed attributes
  - Wrap simple types like TMessage
  - Generate Documentation from available metainformation (see __members__, ClassName...)
  - Allow Wrappers to handle IFreeNotification for the wrapped object when the object does not
    support it, only when the wrapper knows that it is safe enough (singleton delphi object)
  - Handle interfaces?
  - Be able to return an object containing the current event handler of any Delphi object that was hooked by Delphi,
    and not by Python, as presently, if a button has a Delphi OnClick event, inspecting this event from Python
    will return None.

 -----------------------------------------------------------------------------*)

unit WrapDelphi;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes, PythonEngine,  TypInfo,
{$IFNDEF DELPHI6_OR_HIGHER}
  Windows,
{$ENDIF}
{$IFDEF DELPHI6_OR_HIGHER}
  Variants,
{$ENDIF}
{$IFDEF DELPHI7_OR_HIGHER}
  ObjAuto,
{$ENDIF}
  Contnrs;

Type
  TObjectOwnership = (soReference, soOwned);

  // forward declaration
  TPyDelphiWrapper = class;

  {
    If you want to benefit from subscribing to events from Python when your
    wrapped class does not inherit from TComponent, then you can simply
    implement the IFreeNotification, store the subscriber event sink and
    trigger its Notify method in your destructor.
    Note that TFreeNotificationImpl does all the plumbing for you.
  }
  IFreeNotificationSubscriber = interface
    ['{F08FB6EA-3D8B-43C0-8343-77C8E06DE401}']
    procedure Notify(ADeletedObject : TObject);
  end;

  IFreeNotification = interface
    ['{085FD1BB-44FC-457A-B357-4E06071BBEA5}']
    procedure Subscribe(const ASubscriber: IFreeNotificationSubscriber);
    procedure UnSubscribe(const ASubscriber: IFreeNotificationSubscriber);
  end;

  { Helper class that handles the detail of implementing IFreeNotification.
    Usage:

    TMyClass = class(TInterfacedObject, IFreeNotification)
    private
      fFreeNotifImpl : TFreeNotificationImpl;
    protected
      property FreeNotifImpl : TFreeNotificationImpl read fFreeNotifImpl implements IFreeNotification;
    public
      constructor Create;
      destructor Destroy; override;
    end;

    constructor TMyClass.Create;
    begin
      fFreeNotifImpl := TFreeNotificationImpl.Create(Self);
    end;

    destructor TMyClass.Destroy;
    begin
      fFreeNotifImpl.Free;
      inherited;
    end;
  }
  TFreeNotificationImpl = class(TObject)
  private
    fSubscribers : TInterfaceList;
    fOwner: TObject;

    function GetSubscribers : TInterfaceList;
  protected
    // implementation of IFreeNotification
    procedure Subscribe(const ASubscriber: IFreeNotificationSubscriber);
    procedure UnSubscribe(const ASubscriber: IFreeNotificationSubscriber);
  public
    constructor Create(AOwner : TObject);
    destructor Destroy; override;

    property Owner : TObject read fOwner;
  end;

  {
    This class helps wrappers to implement sequence and iterator protocols.
    You must subclass it, override GetItem, GetSize.
    If you override IndexOf, then you must override SupportsIndexOf and return True.
    If you override SetItem, then you must override SupportsWrite and return True.
    You can give a specific name to the container if you override the Name function.

    Note that an instance of this class must belong to a single owner, if you want
    to give it to another class (like a container to an iterator, then you must
    clone it).
  }
  TContainerAccess = class
  private
    fContainer: TObject;
    fWrapper: TPyDelphiWrapper;
  protected
    function Wrap(Obj : TObject; Ownership: TObjectOwnership = soReference) : PPyObject;
  public
    constructor Create(AWrapper : TPyDelphiWrapper; AContainer: TObject); virtual;

    function Clone : TContainerAccess; virtual;
    function GetItem(AIndex : Integer) : PPyObject; virtual; abstract;
    function GetSize : Integer; virtual; abstract;
    function IndexOf(AValue : PPyObject) : Integer; virtual;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; virtual;

    class function ExpectedContainerClass : TClass; virtual; abstract;
    class function Name : String; virtual;
    class function SupportsWrite : Boolean; virtual;
    class function SupportsIndexOf : Boolean; virtual;

    property Container : TObject read fContainer;
    property Wrapper : TPyDelphiWrapper read fWrapper;
  end;
  TContainerAccessClass = class of TContainerAccess;


  {
    Abstract sequence relying on the container access protocol.
    This will help us support the VCL way to access elements,
    for instance: form.Components[i]
    Note that we could simply write form[i], but then we might use it for
    form.Controls[i] (as Components would be the default sequence).
    As the sequence supports iterators, you can also write:
    for i in form: pass
    for i in form.Components: pass
    for i in form.Controls: pass
  }
  TPyDelphiContainer = class(TPyObject)
  private
    fContainerAccess: TContainerAccess;
    fPyDelphiWrapper: TPyDelphiWrapper;
  public
    destructor Destroy; override;

    procedure Setup(APyDelphiWrapper : TPyDelphiWrapper; AContainerAccess : TContainerAccess);
    class procedure SetupType( PythonType : TPythonType ); override;

    function  Repr : PPyObject; override;
    function  Iter : PPyObject; override;

    // Sequence services
    function  SqLength : Integer; override;
    function  SqItem( idx : Integer ) : PPyObject; override;
    function  SqAssItem( idx : integer; obj : PPyObject) : Integer; override;
    function  SqSlice( idx1, idx2 : Integer ) : PPyObject; override;
    function  SqContains( obj: PPyObject): integer; override;

    // Properties
    property ContainerAccess : TContainerAccess read fContainerAccess;
    property PyDelphiWrapper : TPyDelphiWrapper read fPyDelphiWrapper;
  end;

  {
    Abstract iterator relying on the container access protocol.
  }
  TPyDelphiIterator = class(TPyObject)
  private
    fPosition: Integer;
    fContainerAccess: TContainerAccess;
  public
    destructor Destroy; override;

    procedure Setup(AContainerAccess : TContainerAccess);
    class procedure SetupType( PythonType : TPythonType ); override;

    function  Repr : PPyObject; override;
    function  Iter : PPyObject; override;
    function  IterNext : PPyObject; override;

    // Properties
    property Position : Integer read fPosition;
    property ContainerAccess : TContainerAccess read fContainerAccess;
  end;

  {
    Base class allowing us to implement interfaces.
  }
  TPyInterfacedObject = class(TPyObject, {$IFDEF DELPHI6_OR_HIGHER}IInterface{$ELSE}IUnknown{$ENDIF})
  private
    // implementation of interface IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {
    PyObject wrapping TObject
    Exposes published properties and methods
    Also exposes the property ClassName and methods InheritesFrom and Free
    Do not create  TPyDelphi or its subclasses directly - Instead use
    PyDelphiWrapper.TObjectToPyObject
  }
  TPyDelphiObject = class (TPyInterfacedObject, IFreeNotificationSubscriber)
  private
    fDelphiObject: TObject;
    fContainerAccess : TContainerAccess;
    function  GetContainerAccess: TContainerAccess;
    procedure SetDelphiObject(const Value: TObject);
  protected
    fCanFreeOwnedObject : Boolean;

    function CheckBound : Boolean;
    function HasContainerAccessClass : Boolean;
    procedure SubscribeToFreeNotification; virtual;
    procedure UnSubscribeToFreeNotification; virtual;
    class function GetTypeName : String; virtual;
    // Exposed Methods
    function Free_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function InheritsFrom_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ToTuple_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ToList_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SetProps(args, keywords : PPyObject) : PPyObject; cdecl;
    // Exposed Getters
    function Get_ClassName(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Owned(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Bound(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Published(AContext : Pointer) : PPyObject; cdecl;
    function Dummy_Getter(AContext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Dummy_Setter(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    // implementation of interface IFreeNotificationSubscriber
    procedure Notify(ADeletedObject : TObject);
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    Owned: Boolean;

    constructor Create( APythonType : TPythonType ); override;
    destructor Destroy; override;

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    function  SetAttrO( key, value: PPyObject) : Integer; override;
    // Objects are equal when they refer to the same DelphiObject
    function  Compare( obj: PPyObject) : Integer; override;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; override;
    function  Repr : PPyObject; override;
    // automatic iterator support when the wrapper implements IContainerAccessProvider
    function  Iter : PPyObject; override;
    // Sequence services
    function  SqLength : Integer; override;
    function  SqItem( idx : Integer ) : PPyObject; override;
    function  SqSlice( idx1, idx2 : Integer ) : PPyObject; override;
    function  SqContains( obj: PPyObject): integer; override;
    function  SqAssItem( idx : integer; obj : PPyObject) : Integer; override;

    class function  DelphiObjectClass : TClass; virtual;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    // if the class is a container (TStrings, TComponent, TCollection...),
    // then return the class implementing the access to the contained items.
    class function  GetContainerAccessClass : TContainerAccessClass; virtual;
    // creates a container access object using the class returned by GetContainerAccess.
    function  CreateContainerAccess : TContainerAccess; virtual;

    // helper methods
    function Wrap(AObject : TObject; AOwnership: TObjectOwnership = soReference) : PPyObject;
    // Properties
    property DelphiObject: TObject read fDelphiObject write SetDelphiObject;
    property ContainerAccess : TContainerAccess read GetContainerAccess;
  end;
  TPyDelphiObjectClass = class of TPyDelphiObject;

  { This class will simply hold a Python object in its Value property.
    This is required for Delphi var parameters because Python  won't let you
    replace a parameter value with another one, so, we will provide a container
    and you'll be able to change its content. }
  TPyDelphiVarParameter = class(TPyObject)
  private
    fValue: PPyObject;
    procedure SetValue(const Value: PPyObject);
  protected
    // Exposed Getters
    function Get_Value(Acontext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Set_Value(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    destructor Destroy; override;

    function  Compare( obj: PPyObject) : Integer; override;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; override;
    function  Repr : PPyObject; override;

    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;

    property Value : PPyObject read fValue write SetValue;
  end;

  TEventHandler = class
  private
    fComponent: TObject;
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    PropertyInfo : PPropInfo;
    EventType : PTypeInfo;
    Callable : PPyObject;
    // connects to the event on creation
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); virtual;
    // Disconnects from the event on destruction
    destructor Destroy; override;
    // Disconnects from the free notification event now
    procedure Unsubscribe;
    // returns the type info of the supported event
    class function GetTypeInfo : PTypeInfo; virtual; abstract;
    // properties
    property Component : TObject read fComponent;
  end;
  TEventHandlerClass = class of TEventHandler;

  TEventHandlers = class
  private
    fItems : TObjectList;
    fRegisteredClasses : TClassList;
    fPyDelphiWrapper: TPyDelphiWrapper;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TEventHandler;
    function GetRegisteredClass(AIndex: Integer): TEventHandlerClass;
    function GetRegisteredClassCount: Integer;
  protected
    function FindHandler(ATypeInfo : PTypeInfo) : TEventHandlerClass;
    property RegisteredClasses[AIndex : Integer] : TEventHandlerClass read GetRegisteredClass;
    property RegisteredClassCount : Integer read GetRegisteredClassCount;
  public
    constructor Create(APyDelphiWrapper : TPyDelphiWrapper);
    destructor Destroy; override;

    function  Add(AEventHandler : TEventHandler) : Boolean;
    procedure Clear;
    procedure Delete(AIndex : Integer);
    function  GetCallable(AComponent : TObject; APropInfo : PPropInfo) : PPyObject; overload;
    function  GetCallable(AComponent : TObject; const APropName : String) : PPyObject; overload;
    function  Link(AComponent : TObject; APropInfo : PPropInfo; ACallable : PPyObject) : Boolean;
    function  IndexOf(AComponent : TObject; APropInfo : PPropInfo) : Integer;
    procedure RegisterHandler(AEventHandlerClass : TEventHandlerClass);
    function  Unlink(AComponent : TObject; APropInfo : PPropInfo) : Boolean;

    property Count : Integer read GetCount;
    property Items[AIndex : Integer] : TEventHandler read GetItem; default;
    property PyDelphiWrapper : TPyDelphiWrapper read fPyDelphiWrapper;
  end;

  TNotifyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  { Subclass TRegisteredUnit to register your wrappers for a specific unit.
    See WrapDelphiForms which will wrapp some of the classes of the Forms.pas unit.

      type
        TFormsRegistration = class(TRegisteredUnit)
        public
          function Name : String; override;
          procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
          procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
        end;

      procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
      begin
        inherited;
        // Singletons
        APyDelphiWrapper.DefineVar('Application', Application);
        APyDelphiWrapper.DefineVar('Screen',      Screen);

        // MessageBox flags
        APyDelphiWrapper.DefineVar('MB_ABORTRETRYIGNORE', MB_ABORTRETRYIGNORE);
        APyDelphiWrapper.DefineVar('MB_OK',               MB_OK);
      end;

      function TFormsRegistration.Name: String;
      begin
        Result := 'Forms';
      end;

      procedure TFormsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
      begin
        inherited;
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScreen);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMonitor);

        APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseQueryEventHandler);
      end;

    You must also register this class to the RegisteredUnits singleton like this:
      initialization
        RegisteredUnits.Add(TFormsRegistration.Create);
  }
  TRegisteredUnit = class
  public
    function Name : String; virtual; abstract;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); virtual;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); virtual;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); virtual;
  end;

  { Singleton containing all registered units.
    This will be used by TPyDelphiWrapper for registering the wrappers of
    classes contained in those units.
    The advantage is that we can select what we want to wrap simply by
    including the Wrapped units into the project, and thus avoid code bloating
    if we don't need those units.
  }
  TRegisteredUnits = class
  private
    fItems : TObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TRegisteredUnit;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ARegisteredModule : TRegisteredUnit);

    property Count : Integer read GetCount;
    property Items[AIndex : Integer] : TRegisteredUnit read GetItem; default;
  end;

  {
     The main component of this unit.
     Method TObjectToPyObject wraps Delphi objects into Python objects
     Method RegisterDelphiClass can be used to extend its functionality.
     Method RegisterEventHandler can be used to add event handling functionality
  }
  TPyDelphiWrapper = class(TEngineClient, IFreeNotificationSubscriber)
  private
    // Stores Delphi class registration information
    fClassRegister : TObjectList;
    // Stores registration for Helper Types (do not correspond to Delphi classes)
    fHelperClassRegister : TStringList;
    // Stores Created Event Handlers
    fEventHandlerList : TEventHandlers;
    fVarParamType: TPythonType;
{$IFDEF DELPHI7_OR_HIGHER}
    fDelphiMethodType: TPythonType;
{$ENDIF}
    // Exposed Module level function CreateComponent(ComponentClass, Owner)
    function  CreateComponent( pself, args : PPyObject ) : PPyObject; cdecl;
    // Implementation of interface IFreeNotificationSubscriber
    procedure Notify(ADeletedObject : TObject);
  protected
    FModule : TPythonModule;
    fDefaultIterType: TPythonType;
    fDefaultContainerType: TPythonType;
    procedure CreateWrappers; virtual;
    procedure CreateModuleVars; virtual;
    procedure CreateModuleFunctions; virtual;
    procedure SetEngine(Value : TPythonEngine ); override;
    procedure SetModule(const Value: TPythonModule);
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
    procedure ModuleReady(Sender : TObject); override;
    procedure UnsubscribeFreeNotifications;
    procedure CreatePyFunc(AModule : TPythonModule; AMethodDef : PPyMethodDef);
  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Initialize; override;
    procedure Finalize; override;
    procedure DefineVar(const AName : String; const AValue : Variant); overload;
    procedure DefineVar(const AName : String; AValue : TObject); overload;
    procedure RegisterDelphiWrapper(AWrapperClass : TPyDelphiObjectClass);
    function  RegisterHelperType(APyObjectClass : TPyObjectClass) : TPythonType;
    function  RegisterFunction(AFuncName : PChar; AFunc : PyCFunction; ADocString : PChar ) : PPyMethodDef; overload;
    function  RegisterFunction(AFuncName : PChar; AFunc : TDelphiMethod; ADocString : PChar ) : PPyMethodDef; overload;
    function  GetHelperType(TypeName : string) : TPythonType;
    //  Function that provides a Python object wrapping a Delphi object
    function Wrap(AObj : TObject; AOwnership: TObjectOwnership = soReference) : PPyObject;
    // properties
    property EventHandlers : TEventHandlers read fEventHandlerList;
    // Helper types
    property DefaultContainerType : TPythonType read fDefaultContainerType;
    property DefaultIterType : TPythonType read fDefaultIterType;
{$IFDEF DELPHI7_OR_HIGHER}
    property DelphiMethodType : TPythonType read fDelphiMethodType;
{$ENDIF}
    property VarParamType : TPythonType read fVarParamType;
  published
    property Module : TPythonModule read FModule write SetModule;
  end;

  { Singletons }
  function RegisteredUnits : TRegisteredUnits;

  { Misc }
  procedure Register;

  { Helper Functions }

  function  CheckIndex(AIndex, ACount : Integer; const AIndexName : String = 'Index') : Boolean;
  function  CheckIntAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : Integer) : Boolean;
  function  CheckBoolAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : Boolean) : Boolean;
  function  CheckStrAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : String) : Boolean;
  function  CheckObjAttribute(AAttribute : PPyObject; const AAttributeName : String;
                              AExpectedClass : TClass;
                              var AValue : TObject) : Boolean;
  function  CheckCallableAttribute(AAttribute : PPyObject; const AAttributeName : String) : Boolean;
  function  CheckEnum(const AEnumName : String; AValue, AMinValue, AMaxValue : Integer) : Boolean;
  function  CreateSlice(ASequence : TPyObject; AIndex1, AIndex2 : Integer) : PPyObject;
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; const AValue : Variant) : PPyObject;
  function  SetToPython(ATypeInfo: PTypeInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(APropInfo: PPropInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(AInstance: TObject; APropInfo: PPropInfo) : PPyObject; overload;
  function  PythonToSet(ASet : PPyObject; APropInfo: PPropInfo) : Integer;
  function  SupportsFreeNotification(AObject : TObject) : Boolean;
  procedure RaiseNotifyEvent(PyDelphiWrapper : TPyDelphiWrapper; ACallable : PPyObject; Sender: TObject);

implementation

Uses
  Math
  {$IFDEF DELPHI6_OR_HIGHER}, RTLConsts {$ELSE}, Consts{$ENDIF};

var
  gRegisteredUnits : TRegisteredUnits;
function RegisteredUnits : TRegisteredUnits;
begin
  if not Assigned(gRegisteredUnits) then
    gRegisteredUnits := TRegisteredUnits.Create;
  Result := gRegisteredUnits;
end;

var
  gNames : TStringList;
function AddName(const AName : String) : PChar;
begin
  gNames.Add(AName);
  Result := PChar(gNames[gNames.Count-1]);
end;

procedure Register;
begin
  RegisterComponents('Python', [TPyDelphiWrapper]);
end;

{ Helper functions }

function CheckIndex(AIndex, ACount : Integer; const AIndexName : String = 'Index') : Boolean;
begin
  if (AIndex < 0) or (AIndex >= ACount) then
    with GetPythonEngine do
    begin
      Result := False;
      PyErr_SetString (PyExc_IndexError^,
          PChar(Format('%s "%d" out of range',[AIndexName, AIndex])));
    end
  else
    Result := True;
end;

function CheckIntAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : Integer) : Boolean;
begin
  if GetPythonEngine.PyInt_Check(AAttribute) then
  begin
    AValue := GetPythonEngine.PyInt_AsLong(AAttribute);
    Result := True;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PChar(Format('%s receives only integer values', [AAttributeName])));
  end;
end;

function CheckBoolAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : Boolean) : Boolean;
begin
  AValue := GetPythonEngine.PyObject_IsTrue(AAttribute) <> 0;
  Result := True;
end;

function CheckStrAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : String) : Boolean;
begin
  if GetPythonEngine.PyString_Check(AAttribute) then
  begin
    AValue := GetPythonEngine.PyString_AsDelphiString(AAttribute);
    Result := True;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PChar(Format('%s receives only string values', [AAttributeName])));
  end;
end;

function CheckCallableAttribute(AAttribute : PPyObject; const AAttributeName : String) : Boolean;
begin
  if (AAttribute = GetPythonEngine.Py_None) or (GetPythonEngine.PyCallable_Check(AAttribute) <> 0) then
    Result := True
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PChar(Format('%s accepts only None or Callable values', [AAttributeName])));
  end;
end;

function  CheckEnum(const AEnumName : String; AValue, AMinValue, AMaxValue : Integer) : Boolean;
begin
  if (AValue >= AMinValue) and (AValue <= AMaxValue) then
    Result := True
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PChar(Format('Enum %s accepts values between %d and %d. Received %d.', [AEnumName, AMinValue, AMaxValue, AValue])));
  end;
end;

function CheckObjAttribute(AAttribute : PPyObject; const AAttributeName : String;
                           AExpectedClass : TClass;
                           var AValue : TObject) : Boolean;
var
  PyObject : TPyObject;
begin
  if AAttribute = GetPythonEngine.Py_None then
  begin
    Result := True;
    AValue := nil;
  end
  else if IsDelphiObject(AAttribute) then
  begin
    PyObject := PythonToDelphi(AAttribute);
    if not (PyObject is TPyDelphiObject) or
       not (TPyDelphiObject(PyObject).DelphiObject.InheritsFrom(AExpectedClass)) then
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PChar(Format('%s receives only Delphi objects of type %s', [AAttributeName, AExpectedClass.ClassName])));
    end
    else
    begin
      Result := True;
      AValue := TPyDelphiObject(PyObject).DelphiObject;
    end;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PChar(Format('%s receives only Delphi objects', [AAttributeName])));
  end;
end;

function CreateSlice(ASequence : TPyObject; AIndex1, AIndex2 : Integer) : PPyObject;
var
  i : Integer;
  tmp : Integer;
begin
  if not CheckIndex(AIndex1, ASequence.SqLength, 'Index1') then
    Result := nil
  else if not CheckIndex(AIndex2, ASequence.SqLength, 'Index2') then
    Result := nil
  else with GetPythonEngine do
  begin
    if AIndex1 > AIndex2 then
    begin
      tmp := AIndex2;
      AIndex2 := AIndex1;
      AIndex1 := tmp;
    end;

    Result := PyTuple_New(AIndex2-AIndex1+1);
    for i := 0 to PyTuple_Size(Result)-1 do
      PyTuple_SetItem(Result, i, ASequence.SqItem(AIndex1));
  end;
end;

function CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; const AValue : Variant) : PPyObject;
var
  tmp : PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Result := PyDelphiWrapper.VarParamType.CreateInstance;
  _varParam := PythonToDelphi(Result) as TPyDelphiVarParameter;
  tmp := GetPythonEngine.VariantAsPyObject(AValue);
  _varParam.Value := tmp; // refcount was incremented
  GetPythonEngine.Py_DECREF(tmp);
end;

function SupportsFreeNotification(AObject : TObject) : Boolean;
var
  _FreeNotification : IFreeNotification;
begin
  Result := (AObject is TComponent) or AObject.GetInterface(IFreeNotification, _FreeNotification);
end;

procedure RaiseNotifyEvent(PyDelphiWrapper : TPyDelphiWrapper; ACallable : PPyObject; Sender: TObject);
Var
  PyObject, PyTuple, PyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(ACallable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyTuple := PyTuple_New(1);
      try
        GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
        PyResult := PyObject_CallObject(ACallable, PyTuple);
        if Assigned(PyResult) then Py_DECREF(PyResult);
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

function SetToPython(ATypeInfo: PTypeInfo; AValue : Integer) : PPyObject; overload;
var
  S: TIntegerSet;
  I: Integer;
  _name : PPyObject;
begin
  Result := GetPythonEngine.PyList_New(0);
  Integer(S) := AValue;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      _name := GetPythonEngine.PyString_FromString(PChar(GetEnumName(ATypeInfo, I)));
      GetPythonEngine.PyList_Append(Result, _name);
      GetPythonEngine.Py_XDecRef(_name);
    end;
end;

function SetToPython(APropInfo: PPropInfo; AValue : Integer) : PPyObject; overload;
begin
  Result := SetToPython(GetTypeData(APropInfo^.PropType^)^.CompType^, AValue);
end;

function SetToPython(AInstance: TObject; APropInfo: PPropInfo) : PPyObject; overload;
begin
  Result := SetToPython(APropInfo, GetOrdProp(AInstance, APropInfo));
end;

function PythonToSet(ASet : PPyObject; APropInfo: PPropInfo) : Integer;
var
  i : Integer;
  EnumObj: PPyObject;
  EnumName: string;
  EnumValue: Longint;
  EnumInfo: PTypeInfo;

begin
  Result := 0;
  with GetPythonEngine do
  begin
    Assert(PySequence_Check(ASet) <> 0, 'PythonToSet expects a Python sequence as first parameter');

    EnumInfo := GetTypeData(APropInfo^.PropType^)^.CompType^;
    for i := 0 to PySequence_Length(ASet)-1 do
    begin
      EnumObj := PySequence_GetItem(ASet, i);
      try
        EnumName := PyObjectAsString(EnumObj);
      finally
        Py_XDecRef(EnumObj);
      end;
      EnumValue := GetEnumValue(EnumInfo, EnumName);
      if EnumValue < 0 then
        raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyElement, [EnumName]);
      Include(TIntegerSet(Result), EnumValue);
    end;
  end;
end;

{$IFNDEF DELPHI7_OR_HIGHER}

{$IFNDEF DELPHI6_OR_HIGHER}
resourcestring
  SInvalidPropertyType = 'Invalid property type: %s';
procedure AssignWideStr(var Dest: WideString; const Source: WideString);
begin
  Dest := Source;
end;

procedure _GetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  var Value: WideString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to result string    }

        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EDX

        MOV     EDX,[EDI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     ESI,[EDI].TPropInfo.GetProc
        CMP     [EDI].TPropInfo.GetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   ESI,SI                          { sign extend slot offset }
        ADD     ESI,[EAX]                       { vmt + slot offset }
        CALL    DWORD PTR [ESI]
        JMP     @@exit

@@isStaticMethod:
        CALL    ESI
        JMP     @@exit

@@isField:
  AND  ESI,$00FFFFFF
  MOV  EDX,[EAX+ESI]
  MOV  EAX,ECX
  CALL  AssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;

function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo) : WideString;
begin
  _GetWideStrProp(Instance, PropInfo, Result);
end;

procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: WideString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to string value     }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX

        MOV     EDX,[ESI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     EDI,[ESI].TPropInfo.SetProc
        CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   EDI,DI
        ADD     EDI,[EAX]
        CALL    DWORD PTR [EDI]
        JMP     @@exit

@@isStaticMethod:
        CALL    EDI
        JMP     @@exit

@@isField:
  AND  EDI,$00FFFFFF
  ADD  EAX,EDI
  MOV  EDX,ECX
  CALL  AssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;

function VarToWideStrDef(const V: Variant; const ADefault: WideString): WideString;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := ADefault;
end;

var
  // This is the value returned when a NULL is converted into a string.  Other
  //  environments return 'NULL' instead of Delphi's default of an empty string.
  NullAsStringValue: string = '';
function VarToWideStr(const V: Variant): WideString;
begin
  Result := VarToWideStrDef(V, NullAsStringValue);
end;

function Sign(AValue : Integer) : Integer;
begin
  Result := 0;
  if AValue < 0 then
    Result := -1
  else if AValue > 0 then
    Result := 1;
end;

function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer; overload;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function GetPropList(AObject: TObject; out PropList: PPropList): Integer; overload;
begin
  Result := GetPropList(PTypeInfo(AObject.ClassInfo), PropList);
end;

{$ENDIF}

// Copied from unit ObjAuto.pas, available only in Delphi7 or later
function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  // assume failure
  Result := Null;

  // return the right type
  case PropInfo^.PropType^^.Kind of
    tkInteger, tkChar, tkWChar, tkClass:
      Result := GetOrdProp(Instance, PropInfo);
    tkEnumeration:
      if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(Instance, PropInfo))
      else
        Result := GetOrdProp(Instance, PropInfo);
    tkSet:
      Result := GetOrdProp(Instance, PropInfo);
    tkFloat:
      Result := GetFloatProp(Instance, PropInfo);
    tkMethod:
      Result := PropInfo^.PropType^.Name;
    tkString, tkLString:
      Result := GetStrProp(Instance, PropInfo);
    tkWString:
      Result := GetWideStrProp(Instance, PropInfo);
    tkVariant:
      Result := GetVariantProp(Instance, PropInfo);
{$IFDEF DELPHI6_OR_HIGHER}
    tkInt64:
      Result := GetInt64Prop(Instance, PropInfo);
{$ENDIF}
    tkDynArray:
      DynArrayToVariant(Result, Pointer(GetOrdProp(Instance, PropInfo)), PropInfo^.PropType^);
  else
    raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType, [PropInfo.PropType^^.Name]);
  end;
end;

// Copied from unit ObjAuto.pas, available only in Delphi7 or later
procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);

  function RangedValue(const AMin, AMax: Int64): Int64;
  begin
    Result := Trunc(Value);
    if Result < AMin then
      Result := AMin;
    if Result > AMax then
      Result := AMax;
  end;

var
  TypeData: PTypeData;
  DynArray: Pointer;
begin
  // get the prop info
  TypeData := GetTypeData(PropInfo^.PropType^);

  // set the right type
  case PropInfo.PropType^^.Kind of
    tkInteger, tkChar, tkWChar:
      if TypeData^.MinValue < TypeData^.MaxValue then
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
          TypeData^.MaxValue))
      else
        // Unsigned type
        SetOrdProp(Instance, PropInfo,
          RangedValue(LongWord(TypeData^.MinValue),
          LongWord(TypeData^.MaxValue)));
    tkEnumeration:
      if VarType(Value) = varString then
        SetEnumProp(Instance, PropInfo, VarToStr(Value))
      else if VarType(Value) = varBoolean then
        // Need to map variant boolean values -1,0 to 1,0
        SetOrdProp(Instance, PropInfo, Abs(Trunc(Value)))
      else
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
          TypeData^.MaxValue));
    tkSet:
      if VarType(Value) = varInteger then
        SetOrdProp(Instance, PropInfo, Value)
      else
        SetSetProp(Instance, PropInfo, VarToStr(Value));
    tkFloat:
      SetFloatProp(Instance, PropInfo, Value);
    tkString, tkLString:
      SetStrProp(Instance, PropInfo, VarToStr(Value));
    tkWString:
      SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
    tkVariant:
      SetVariantProp(Instance, PropInfo, Value);
    tkInt64:
      SetInt64Prop(Instance, PropInfo, RangedValue(TypeData^.MinInt64Value,
        TypeData^.MaxInt64Value));
    tkDynArray:
      begin
        DynArrayFromVariant(DynArray, Value, PropInfo^.PropType^);
        SetOrdProp(Instance, PropInfo, Integer(DynArray));
      end;
  else
    raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType,
      [PropInfo.PropType^^.Name]);
  end;
end;
{$ENDIF}

function Abort_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
begin
  Result := nil;
  Abort;
end;

Type
  //  Used for class registration by TPyDelphiWrapper fClassRegister
  TRegisteredClass = class
    DelphiClass : TClass;
    PythonType : TPythonType;
  end;

{$IFDEF DELPHI7_OR_HIGHER}
Type
  //  PyObject wrapping TObject method call
  //  Helper object used by TPyDelphiObject
  TPyDelphiMethodObject = class (TPyObject)
  public
    DelphiObject: TObject;
    MethodInfo : TMethodInfoHeader;
    function  Call( ob1, ob2 : PPyObject) : PPyObject; override;
    function  Repr : PPyObject; override;
    class procedure SetupType( PythonType : TPythonType ); override;
  end;
{$ENDIF}

{ TFreeNotificationImpl }

constructor TFreeNotificationImpl.Create(AOwner: TObject);
begin
  Assert(Assigned(AOwner));
  fOwner := AOwner;
end;

destructor TFreeNotificationImpl.Destroy;
var
  i : Integer;
begin
  if Assigned(fSubscribers) then
  begin
    for i := 0 to fSubscribers.Count-1 do
      (fSubscribers[i] as IFreeNotificationSubscriber).Notify(Owner);
    fSubscribers.Free;
  end;
  inherited;
end;

function TFreeNotificationImpl.GetSubscribers: TInterfaceList;
begin
  if not Assigned(fSubscribers) then
    fSubscribers := TInterfaceList.Create;
  Result := fSubscribers;
end;

procedure TFreeNotificationImpl.Subscribe(
  const ASubscriber: IFreeNotificationSubscriber);
begin
  Assert(Assigned(ASubscriber));
  if not Assigned(fSubscribers) or (fSubscribers.IndexOf(ASubscriber) < 0) then
    GetSubscribers.Add(ASubscriber);
end;

procedure TFreeNotificationImpl.UnSubscribe(
  const ASubscriber: IFreeNotificationSubscriber);
begin
  if Assigned(fSubscribers) then
  begin
    fSubscribers.Remove(ASubscriber);
    if fSubscribers.Count = 0 then
      FreeAndNil(fSubscribers);
  end;
end;

{ TContainerAccess }

function TContainerAccess.Clone: TContainerAccess;
begin
  Result := TContainerAccessClass(ClassType).Create(Wrapper, Container);
end;

constructor TContainerAccess.Create(AWrapper: TPyDelphiWrapper;
  AContainer: TObject);
begin
  inherited Create;
  Assert(Assigned(AWrapper));
  Assert(Assigned(AContainer));
  Assert(AContainer.InheritsFrom(ExpectedContainerClass), Format('Class %s expects a container of class %s', [ClassName, ExpectedContainerClass.ClassName]));
  fWrapper := AWrapper;
  fContainer := AContainer;
end;

function TContainerAccess.IndexOf(AValue: PPyObject): Integer;
begin
  Result := -1;
end;

class function TContainerAccess.Name: String;
begin
  Result := ExpectedContainerClass.ClassName;
end;

function TContainerAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  Result := False;
end;

class function TContainerAccess.SupportsIndexOf: Boolean;
begin
  Result := False;
end;

class function TContainerAccess.SupportsWrite: Boolean;
begin
  Result := False;
end;

function TContainerAccess.Wrap(Obj: TObject;
  Ownership: TObjectOwnership): PPyObject;
begin
  Result := Wrapper.Wrap(Obj, Ownership);
end;

{ TPyDelphiContainer }

destructor TPyDelphiContainer.Destroy;
begin
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiContainer.Iter: PPyObject;
begin
  Result := PyDelphiWrapper.DefaultIterType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiIterator do
    Setup(Self.ContainerAccess.Clone);
end;

function TPyDelphiContainer.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString( PChar(Format('<Delphi %s at %x>',
         [ContainerAccess.Name, Integer(Self)])) );
end;

procedure TPyDelphiContainer.Setup(APyDelphiWrapper : TPyDelphiWrapper;
  AContainerAccess : TContainerAccess);
begin
  Assert(Assigned(APyDelphiWrapper));
  Assert(Assigned(AContainerAccess));
  fPyDelphiWrapper := APyDelphiWrapper;
  fContainerAccess := AContainerAccess;
end;

class procedure TPyDelphiContainer.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Name := 'DefaultContainerType';
  PythonType.TypeName := 'DelphiDefaultContainer';
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Abstract Container type for Delphi';
  PythonType.Services.Basic    := PythonType.Services.Basic + [bsRepr, bsIter];
  PythonType.Services.Sequence := PythonType.Services.Sequence + [ssLength, ssItem, ssAssItem, ssContains, ssSlice];
end;

function TPyDelphiContainer.SqAssItem(idx: integer;
  obj: PPyObject): Integer;
begin
  if ContainerAccess.SupportsWrite then
  begin
    if not CheckIndex(idx, ContainerAccess.GetSize) then
      Result := -1
    else
      if ContainerAccess.SetItem(idx, obj) then
        Result := 0
      else
        Result := -1;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Container %s does not support indexed write (f[i] = x)', [fContainerAccess.Name])) );
  end;
end;

function TPyDelphiContainer.SqContains(obj: PPyObject): integer;
begin
  if ContainerAccess.SupportsIndexOf then
  begin
    if ContainerAccess.IndexOf(Obj) > -1 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Container %s does not support the Contains protocol', [fContainerAccess.Name])) );
  end;
end;

function TPyDelphiContainer.SqItem(idx: Integer): PPyObject;
begin
  if CheckIndex(idx, SqLength) then
    Result := ContainerAccess.GetItem(idx)
  else
    Result := nil;
end;

function TPyDelphiContainer.SqLength: Integer;
begin
  Result := ContainerAccess.GetSize;
end;

function TPyDelphiContainer.SqSlice(idx1, idx2: Integer): PPyObject;
begin
  Result := CreateSlice(Self, idx1, idx2);
end;

{ TPyDelphiIterator }

destructor TPyDelphiIterator.Destroy;
begin
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiIterator.Iter: PPyObject;
begin
  Result := GetSelf;
  GetPythonEngine.Py_XINCREF(Result);
end;

function TPyDelphiIterator.IterNext: PPyObject;
begin
  if fPosition >= ContainerAccess.GetSize then with GetPythonEngine do begin
    Result := nil;
    PyErr_SetString (PyExc_StopIteration^, 'StopIteration');
  end else begin
    Result := ContainerAccess.GetItem(fPosition);
    Inc(fPosition);
  end;
end;

function TPyDelphiIterator.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString( PChar(Format('<Delphi %sIterator at %x>',
         [ContainerAccess.Name, Integer(Self)])) );
end;

procedure TPyDelphiIterator.Setup(AContainerAccess : TContainerAccess);
begin
  Assert(Assigned(AContainerAccess));
  fContainerAccess := AContainerAccess;
  fPosition := 0;
end;

class procedure TPyDelphiIterator.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Name := 'DefaultIterType';
  PythonType.TypeName := 'DelphiDefaultIterator';
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Iterator for Abstract Containers';
  PythonType.Services.Basic := [bsRepr, bsStr, bsIter, bsIterNext];
end;

{ TPyInterfacedObject }

function TPyInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TPyInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

function TPyInterfacedObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ TPyDelphiObject }

function TPyDelphiObject.CheckBound: Boolean;
begin
  Result := Assigned(DelphiObject);
  if not Result then
    with GetPythonEngine do
      PyErr_SetString(PyExc_AttributeError^, PChar(Format('Delphi wrapper %s is not bound', [ClassName])));
end;

function TPyDelphiObject.Compare(obj: PPyObject): Integer;
Var
  PyObject : TPyObject;
begin
  if IsDelphiObject(obj) then begin
    PyObject := PythonToDelphi(obj);
    if PyObject is TPyDelphiObject then
      Result := Sign(Integer(TPyDelphiObject(PyObject).DelphiObject) - Integer(DelphiObject))
    else
      Result := -1;  // not equal
  end else
    Result := -1;  // not equal
end;

constructor TPyDelphiObject.Create(APythonType: TPythonType);
begin
  inherited;
  if Assigned(APythonType) and (APythonType.Owner is TPyDelphiWrapper) then
    PyDelphiWrapper := TPyDelphiWrapper(APythonType.Owner);
end;

function TPyDelphiObject.CreateContainerAccess: TContainerAccess;
var
  _ContainerAccessClass : TContainerAccessClass;
begin
  _ContainerAccessClass := GetContainerAccessClass;
  if Assigned(_ContainerAccessClass) then
    Result := _ContainerAccessClass.Create(PyDelphiWrapper, DelphiObject)
  else
    raise Exception.CreateFmt('Wrapper class %s does not provide a container access', [ClassName]);
end;

class function TPyDelphiObject.DelphiObjectClass: TClass;
begin
  Result := TObject;
end;

destructor TPyDelphiObject.Destroy;
begin
  DelphiObject := nil; // will free the object if owned
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiObject.Dummy_Getter(AContext: Pointer): PPyObject;
begin
  // Note that this code should never execute as TPyDelphiObject overrides GetAttrO
  // to intercept an access to a published property.
  // This getter was only registered to help document the wrapped object and
  // benefit from the code insight of the Python IDE.
  Result := nil;
  with GetPythonEngine do
    PyErr_SetString (PyExc_AttributeError^,
      PChar('Fatal error. The Delphi wrapper did not find the published property you were trying to access'));
end;

function TPyDelphiObject.Dummy_Setter(AValue: PPyObject;
  AContext: Pointer): Integer;
begin
  // Note that this code should never execute as TPyDelphiObject overrides GetAttrO
  // to intercept an access to a published property.
  // This getter was only registered to help document the wrapped object and
  // benefit from the code insight of the Python IDE.
  Result := -1;
  with GetPythonEngine do
    PyErr_SetString (PyExc_AttributeError^,
      PChar('Fatal error. The Delphi wrapper did not find the published property you were trying to access'));
end;

function TPyDelphiObject.Free_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do
  begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':Free', [] ) <> 0 then
    begin
      if Owned or fCanFreeOwnedObject then begin
        DelphiObject := nil; // this will free the object automatically
        Owned := False;
        Result := ReturnNone;
      end else begin
        PyErr_SetString (PyExc_AttributeError^,
          PChar('The Delphi object cannot be freed, since it is not Owned'));
        Result := nil;
      end;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiObject.GetAttrO(key: PPyObject): PPyObject;
(*
    First Look for "published" methods compiled with {$METHODINFO ON}
    Then look for published properties
    Finally call inherited which calls PyObject_GenericGetAttr
*)
var
  Name: ShortString;
{$IFDEF DELPHI7_OR_HIGHER}
  Info: PMethodInfoHeader;
{$ENDIF}
  PropInfo: PPropInfo;
  Obj : TObject;
begin
  Result := nil;
  if GetPythonEngine.PyString_Check(Key) then
    Name := GetPythonEngine.PyString_AsDelphiString(Key)
  else
    Name := '';

{$IFDEF DELPHI7_OR_HIGHER}
  if Assigned(DelphiObject) and (Name <> '') then
    Info := GetMethodInfo(DelphiObject, Name)
  else
    Info := nil;

  if Info <> nil then // We have a method
  begin
    // Ensure the method information has enough type information
    if Info.Len <= SizeOf(Info^) - SizeOf(ShortString) + 1 + Length(Info.Name) then
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[Name])))
    else
    begin
      Result := PyDelphiWrapper.DelphiMethodType.CreateInstance;
      with PythonToDelphi(Result) as TPyDelphiMethodObject do begin
        MethodInfo := Info^;
        DelphiObject := self.DelphiObject;
      end;
    end;
  end
  else{$ENDIF} if Assigned(DelphiObject) and (Name <> '') then
  begin
    // Not a  method, try a property.
    PropInfo := GetPropInfo(DelphiObject, Name);
    if PropInfo <> nil then
    begin
      // we have a property
      if PropInfo^.PropType^.Kind = tkClass then begin
        Obj := TObject(GetOrdProp(Self.DelphiObject, PropInfo));
        Result := Wrap(Obj);
      end else if PropInfo^.PropType^.Kind = tkMethod then begin
        Result := PyDelphiWrapper.fEventHandlerList.GetCallable(Self.DelphiObject, PropInfo)
      end else if PropInfo^.PropType^.Kind = tkSet then begin
        Result := SetToPython(Self.DelphiObject, PropInfo)
      end else if PropInfo^.PropType^.Kind = tkEnumeration then begin
      begin
        if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
          Result := GetPythonEngine.VariantAsPyObject(Boolean(GetOrdProp(Self.DelphiObject, PropInfo)))
        else
          Result := GetPythonEngine.PyString_FromString(PChar(GetEnumName(PropInfo^.PropType^, GetOrdProp(Self.DelphiObject, PropInfo))));
      end
      end else
         Result := GetPythonEngine.VariantAsPyObject(GetPropValue(DelphiObject, PropInfo));
    end;
  end;

  if not Assigned(Result) then
    Result := inherited GetAttrO(key);
end;

function TPyDelphiObject.GetContainerAccess: TContainerAccess;
begin
  if not Assigned(fContainerAccess) then
    fContainerAccess := CreateContainerAccess;
  Result := fContainerAccess;
end;

class function TPyDelphiObject.GetContainerAccessClass : TContainerAccessClass;
begin
  Result := nil;
end;

function TPyDelphiObject.Get_Bound(Acontext: Pointer): PPyObject;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    Result := VariantAsPyObject(Assigned(DelphiObject));
  end;
end;

function TPyDelphiObject.Get_ClassName(Acontext: Pointer): PPyObject;
Var
  S : String;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    if CheckBound then
    begin
      S := DelphiObject.ClassName;
      Result := PyString_FromString(PChar(S));
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiObject.Get_Owned(Acontext: Pointer): PPyObject;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    if CheckBound then
      Result := VariantAsPyObject(Owned)
    else
      Result := nil;
  end;
end;

function TPyDelphiObject.Get_Published(AContext: Pointer): PPyObject;
var
  i : Integer;
  _PropList: PPropList;
  _propCount : Integer;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    _propCount := GetPropList(DelphiObject, _PropList);
    Result := PyList_New(_propCount);
    if _propCount > 0 then
    begin
      try
        for i := 0 to _propCount-1 do
          PyList_SetItem(Result, i, PyString_FromString(PChar(String(_PropList^[i].Name))));
      finally
        FreeMem(_PropList);
      end;
      PyList_Sort(Result);
    end;
  end;
end;

class function TPyDelphiObject.GetTypeName : String;
begin
  Result := Copy(DelphiObjectClass.ClassName, 2, MaxInt);
end;

function TPyDelphiObject.HasContainerAccessClass: Boolean;
begin
  Result := GetContainerAccessClass <> nil;
end;

function TPyDelphiObject.InheritsFrom_Wrapper(args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  AClass: TClass;
  KlassName: string;
  IsSubClass: Boolean;
begin
  with GetPythonEngine do
  begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, 'O:InheritsFrom', [@_obj] ) <> 0 then begin
      if CheckBound then begin
        KlassName := PyObjectAsString(_obj);
        AClass := DelphiObject.ClassType;
        repeat
          IsSubClass := SameText(AClass.ClassName, KlassName);
          if IsSubClass then Break;
          AClass := AClass.ClassParent;
        until AClass = nil;
        if IsSubClass then
          Result := PPyObject(Py_True)
        else
          Result := PPyObject(Py_False);
        Py_INCREF( Result );
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiObject.Iter: PPyObject;
begin
  if HasContainerAccessClass then
  begin
    Result := PyDelphiWrapper.DefaultIterType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiIterator do
      Setup(Self.ContainerAccess.Clone);
  end
  else
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Wrapper %s does not support iterators', [Self.ClassName])) );
  end;
end;

procedure TPyDelphiObject.Notify(ADeletedObject: TObject);
begin
  if fDelphiObject = ADeletedObject then
    fDelphiObject := nil;
end;

class procedure TPyDelphiObject.RegisterGetSets(PythonType: TPythonType);
var
  i : Integer;
  _doc : String;
  _propCount : Integer;
  _PropList: PPropList;
  _typeInfo : PTypeInfo;
begin
  inherited;
  // first register get/set for each published property, to allow easier documenting
  // and use of code insight within a Python IDE.
  // Note that the Delphi type info contains short strings which are not directly compatible with PChar.
  // So, we have to convert them to regular strings, store them in a list, to maintain them alive, then
  // we can cast them into PChars.
  _typeInfo := DelphiObjectClass.ClassInfo;
  if _typeInfo <> nil then
  begin
    _propCount := GetPropList(_typeInfo, _PropList);
    if _propCount > 0 then
      try
        for i := 0 to _propCount-1 do
        begin
          _doc := Format('Published property %s : %s', [_PropList^[i].Name, _PropList^[i].PropType^.Name]);
          PythonType.AddGetSet(AddName(_PropList^[i].Name), @TPyDelphiObject.Dummy_Getter, @TPyDelphiObject.Dummy_Setter,
          AddName(_doc), nil);
        end;
      finally
        FreeMem(_PropList);
      end;
  end;
  // then register TObject + custom getters/setters.
  with PythonType do
    begin
      AddGetSet('ClassName', @TPyDelphiObject.Get_ClassName, nil,
        'Returns the TObject.ClassName', nil);
      AddGetSet('__bound__', @TPyDelphiObject.Get_Bound, nil,
        'Returns True if the wrapper is still bound to the Delphi instance.', nil);
      AddGetSet('__owned__', @TPyDelphiObject.Get_Owned, nil,
        'Returns True if the wrapper owns the Delphi instance.', nil);
      AddGetSet('__published__', @TPyDelphiObject.Get_published, nil,
        'Returns the list of all published properties of this instance.', nil);
    end;
end;

class procedure TPyDelphiObject.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethodWithKeywords('SetProps', @TPyDelphiObject.SetProps,
    'TObject.SetProps(prop1=val1, prop2=val2...)'#10 +
    'Sets several properties in one call');
  PythonType.AddMethod('Free', @TPyDelphiObject.Free_Wrapper,
    'TObject.Free()'#10 +
    'Frees the Wrapped Delphi Object');
  PythonType.AddMethod('InheritsFrom', @TPyDelphiObject.InheritsFrom_Wrapper,
    'TObject.InheritsFrom(ClassName)'#10 +
    'Returns True if Delphi Object is or inherits from ClassName');
  PythonType.AddMethod('ToTuple', @TPyDelphiObject.ToTuple_Wrapper,
    'TStrings.ToTuple()'#10 +
    'If the object is a container (TStrings, TComponent...), it returns the content of the sequence as a Python tuple object.');
  PythonType.AddMethod('ToList', @TPyDelphiObject.ToList_Wrapper,
    'TStrings.ToList()'#10 +
    'If the object is a container (TStrings, TComponent...), it returns the content of the sequence as a Python list object.');
end;

function TPyDelphiObject.Repr: PPyObject;
begin
  with GetPythonEngine do
    if Assigned(DelphiObject) then
      Result := PyString_FromString( PChar(Format('<Delphi object of type %s at %x>',
           [DelphiObject.ClassName, Integer(Self)])) )
    else
      Result := PyString_FromString( PChar(Format('<Unbound Delphi wrapper of type %s at %x>',
           [DelphiObjectClass.ClassName, Integer(Self)])) );
end;

function TPyDelphiObject.RichCompare(obj: PPyObject;
  Op: TRichComparisonOpcode): PPyObject;
Var
  Res : Boolean;
begin
  Res := False;
  case Op of
    pyLT: Res := Compare(obj) < 0;
    pyLE: Res := Compare(obj) <= 0;
    pyEQ: Res := Compare(obj) = 0;
    pyNE: Res := Compare(obj) <> 0;
    pyGT: Res := Compare(obj) > 0;
    pyGE: Res := Compare(obj) >= 0;
  end;
  if Res then
    Result := PPyObject(GetPythonEngine.Py_True)
  else
    Result := PPyObject(GetPythonEngine.Py_False);
  GetPythonEngine.Py_INCREF( Result );
end;

function TPyDelphiObject.SetAttrO(key, value: PPyObject): Integer;

  function HandleEvent(PropInfo: PPropInfo) : Integer;
  begin
    if PyDelphiWrapper.EventHandlers.Link(DelphiObject, PropInfo, value) then
      Result := 0
    else
      Result := -1;
  end;

  function HandleClass(PropInfo: PPropInfo) : Integer;
  var
    PyObject : TPyObject;
    Obj : TObject;
  begin
    Result := -1;
    if IsDelphiObject(value) then
    begin
      PyObject := PythonToDelphi(value);
      if PyObject is TPyDelphiObject then
      begin
        Obj := TPyDelphiObject(PyObject).DelphiObject;
        if Obj.ClassType.InheritsFrom(GetTypeData(PropInfo^.PropType^).ClassType) then
        begin
          SetOrdProp(DelphiObject, PropInfo, Integer(Obj));
          Result := 0;
        end
        else
          with GetPythonEngine do
            PyErr_SetString (PyExc_AttributeError^, 'Incompatible classes');
      end
      else
        with GetPythonEngine do
          PyErr_SetString (PyExc_AttributeError^, 'Class property - expected a Delphi object');
    end
    else
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^, 'Class property - expected a Delphi object');
  end;

  function HandleSet(PropInfo: PPropInfo) : Integer;
  begin
    try
      SetPropValue(DelphiObject, PropInfo, PythonToSet(Value, PropInfo));
      Result := 0;
    except
      on E: Exception do with GetPythonEngine do
      begin
        Result := -1;
        PyErr_SetString (PyExc_AttributeError^, PChar(E.Message));
      end;
    end;
  end;

  function HandleOtherTypes(PropInfo: PPropInfo) : Integer;
  Var
    V : Variant;
  begin
    try
      V := GetPythonEngine.PyObjectAsVariant(Value);
      if (PropInfo.PropType^^.Kind = tkEnumeration) and (VarType(V) = varOleStr) then
        // Special case that occurs in Python3000
        V := VarAsType(V, varString);  //Downcast to string
      SetPropValue(DelphiObject, PropInfo, V);
      Result := 0;
    except
      on E: Exception do with GetPythonEngine do
      begin
        Result := -1;
        PyErr_SetString (PyExc_AttributeError^, PChar(E.Message));
      end;
    end;
  end;

var
  PropInfo: PPropInfo;
  Name : string;
begin

  if Assigned(DelphiObject) then
  begin
    Name := GetPythonEngine.PyString_AsDelphiString(Key);
    PropInfo := GetPropInfo(DelphiObject, Name);
  end
  else
    PropInfo := nil;

  if PropInfo <> nil then
  begin
    if PropInfo^.PropType^.Kind = tkMethod then
      Result := HandleEvent(PropInfo)
    else if PropInfo^.PropType^.Kind = tkClass then
      Result := HandleClass(PropInfo)
    else if PropInfo^.PropType^.Kind = tkSet then
      Result := HandleSet(PropInfo)
    else
      Result := HandleOtherTypes(PropInfo);
  end
  else
    Result := inherited SetAttrO(key, value);
end;

procedure TPyDelphiObject.SetDelphiObject(const Value: TObject);
begin
  if fDelphiObject <> Value then
  begin
    if Assigned(Value) then
      Assert(Value.InheritsFrom(DelphiObjectClass));
    if Assigned(fDelphiObject)then
    begin
      UnSubscribeToFreeNotification;
      if Owned then
        fDelphiObject.Free;
    end;
    fDelphiObject := Value;
    if Assigned(fDelphiObject) then
      SubscribeToFreeNotification;
  end;
end;

function TPyDelphiObject.SetProps(args, keywords: PPyObject): PPyObject;
var
  i : Integer;
  _key : PPyObject;
  _keys : PPyObject;
  _value : PPyObject;
begin
  Result := nil;
  with GetPythonEngine do
  begin
    Adjust(@Self);
    _keys := PyDict_Keys(keywords);
    try
      for i := 0 to PySequence_Length(_keys)-1 do
      begin
        _key := PySequence_GetItem(_keys, i);
        if _key = nil then
          Exit;
        try
          _value := PyDict_GetItem(keywords, _key); // returns a borrowed reference
          if PyObject_SetAttr(GetSelf, _key, _value) = -1 then
            Exit;
        finally
          Py_DECREF(_key);
        end;
      end;
    finally
      Py_DECREF(_keys);
    end;
    Result := ReturnNone;
  end;
end;

class procedure TPyDelphiObject.SetupType(PythonType: TPythonType);
var
  _ContainerAccessClass : TContainerAccessClass;
begin
  inherited;
  PythonType.TypeName := GetTypeName;
  PythonType.Name := PythonType.TypeName + 'Type';
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType, tpfHaveRichCompare];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Wrapper for Delphi ' + DelphiObjectClass.ClassName;
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsCompare, bsRichCompare];
  _ContainerAccessClass := GetContainerAccessClass;
  if Assigned(_ContainerAccessClass) then
  begin
    PythonType.Services.Basic := PythonType.Services.Basic + [bsIter];
    PythonType.Services.Sequence := PythonType.Services.Sequence + [ssLength, ssItem, ssSlice];
    if _ContainerAccessClass.SupportsWrite then
      PythonType.Services.Sequence := PythonType.Services.Sequence + [ssAssItem];
    if _ContainerAccessClass.SupportsIndexOf then
      PythonType.Services.Sequence := PythonType.Services.Sequence + [ssContains];
  end;
end;

function TPyDelphiObject.SqAssItem(idx: integer; obj: PPyObject): Integer;
begin
  if HasContainerAccessClass then
  begin
    if not CheckIndex(idx, GetContainerAccess.GetSize) then
      Result := -1
    else
      if GetContainerAccess.SetItem(idx, obj) then
        Result := 0
      else
        Result := -1;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Wrapper %s does not support indexed write (f[i] = x)', [Self.ClassName])) );
  end;
end;

function TPyDelphiObject.SqContains(obj: PPyObject): integer;
begin
  if HasContainerAccessClass then
  begin
    if ContainerAccess.IndexOf(obj) > -1 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function TPyDelphiObject.SqItem(idx: Integer): PPyObject;
begin
  if HasContainerAccessClass then
  begin
    if not CheckIndex(idx, ContainerAccess.GetSize) then
      Result := nil
    else
      Result := ContainerAccess.GetItem(idx);
  end
  else
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Wrapper %s does not support sequences', [Self.ClassName])) );
  end;
end;

function TPyDelphiObject.SqLength: Integer;
begin
  if HasContainerAccessClass then
    Result := ContainerAccess.GetSize
  else
    Result := 0;
end;

function TPyDelphiObject.SqSlice(idx1, idx2: Integer): PPyObject;
begin
  Result := CreateSlice(Self, idx1, idx2);
end;

procedure TPyDelphiObject.SubscribeToFreeNotification;
var
  _FreeNotification : IFreeNotification;
begin
  Assert(Assigned(fDelphiObject));
  if fDelphiObject.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.Subscribe(Self);
end;

function TPyDelphiObject.ToList_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if PythonType.Services.Sequence * [ssItem, ssLength] <> [ssItem, ssLength] then
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Wrapper %s does not support sequences', [Self.ClassName])) );
  end
  else if GetPythonEngine.PyArg_ParseTuple( args, ':ToList', [] ) <> 0 then
    with GetPythonEngine do
    begin
      Result := PyList_New(SqLength);
      for i := 0 to PyList_Size(Result)-1 do
        PyList_SetItem(Result, i, SqItem(i));
    end
  else
    Result := nil;
end;

function TPyDelphiObject.ToTuple_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if PythonType.Services.Sequence * [ssItem, ssLength] <> [ssItem, ssLength] then
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetString( PyExc_SystemError^, PChar(Format('Wrapper %s does not support sequences', [Self.ClassName])) );
  end
  else if GetPythonEngine.PyArg_ParseTuple( args, ':ToTuple', [] ) <> 0 then
    with GetPythonEngine do
    begin
      Result := PyTuple_New(SqLength);
      for i := 0 to PyTuple_Size(Result)-1 do
        PyTuple_SetItem(Result, i, SqItem(i));
    end
  else
    Result := nil;
end;

procedure TPyDelphiObject.UnSubscribeToFreeNotification;
var
  _FreeNotification : IFreeNotification;
begin
  Assert(Assigned(fDelphiObject));
  if fDelphiObject.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.UnSubscribe(Self);
end;

function TPyDelphiObject.Wrap(AObject: TObject;
  AOwnership: TObjectOwnership): PPyObject;
begin
  Result := PyDelphiWrapper.Wrap(AObject, AOwnership);
end;

{$IFDEF DELPHI7_OR_HIGHER}
{ TPyDelphiMethodObject }

function TPyDelphiMethodObject.Call(ob1, ob2: PPyObject): PPyObject;
Var
  V : Variant;
  ParamIndexes: array of Integer;
  Params: array of Variant;
  i, ArgCount: integer;
begin
  //  Ignore keyword arguments ob2
  // ob1 is a tuple with zero or more elements
  V := GetPythonEngine.PyObjectAsVariant(ob1);
  //  V is a VarArray

  ArgCount :=  VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
  SetLength(ParamIndexes, 0);
  SetLength(Params, ArgCount);

  // Params should contain arguments in reverse order!
  for i := 0 to ArgCount - 1 do
    Params[i] := VarArrayGet(V, [ArgCount - i - 1]);

  with GetPythonEngine do
    try
      Result := VariantAsPyObject(ObjectInvoke(DelphiObject,
                                          @MethodInfo, ParamIndexes, Params));
    except
      on E: Exception do
      begin
        Result := nil;
        PyErr_SetString (PyExc_TypeError^,
          PChar(Format('"%s" called with invalid arguments: %s',[MethodInfo.Name, E.Message])));
      end;
    end;
end;

function TPyDelphiMethodObject.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString( PChar(Format('<Delphi method %s of class %s at %x>',
         [MethodInfo.Name, DelphiObject.ClassName, Integer(Self)])) );
end;

class procedure TPyDelphiMethodObject.SetupType( PythonType : TPythonType );
begin
  inherited;
  PythonType.Name := 'DelphiMethodType';
  PythonType.TypeName := 'DelphiMethod';
  PythonType.DocString.Text := 'Wrapper for Delphi methods';
  PythonType.Services.Basic := [bsRepr, bsStr, bsCall];
  PythonType.GenerateCreateFunction := False;
end;

{$ENDIF}

{ TPyDelphiVarParameter }

function TPyDelphiVarParameter.Compare(obj: PPyObject): Integer;
var
  _value : PPyObject;
begin
  with GetPythonEngine do
  begin
    if Self.Value = nil then
      _value := Py_None
    else
      _value := Self.Value;
    if IsPython3000 then begin
      if PyObject_RichCompareBool(_value, obj, PY_LT) = 1 then
        Result := -1
      else if PyObject_RichCompareBool(_value, obj, PY_EQ) = 1 then
        Result := 0
      else
        Result := 1;
      PyErr_Clear;
    end else
      Result := PyObject_Compare(_value, obj);
  end;
end;

destructor TPyDelphiVarParameter.Destroy;
begin
  Value := nil;
  inherited;
end;

function TPyDelphiVarParameter.Get_Value(Acontext: Pointer): PPyObject;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    Result := Self.Value;
    if not Assigned(Result) then
      Result := Py_None;
    Py_XIncRef(Result);
  end;
end;

class procedure TPyDelphiVarParameter.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Value', @TPyDelphiVarParameter.Get_Value, @TPyDelphiVarParameter.Set_Value,
        'Provides access to the Value associated with the Var parameter', nil);
    end;
end;

function TPyDelphiVarParameter.Repr: PPyObject;
var
  _value : PPyObject;
begin
  if not Assigned(Value) then
    _value := GetPythonEngine.ReturnNone
  else
    _value := GetPythonEngine.PyObject_Repr(Value);
  try
    Result := GetPythonEngine.PyString_FromString(PChar(Format('<VarParameter containing: %s>', [GetPythonEngine.PyObjectAsString(_value)])));
  finally
    GetPythonEngine.Py_DECREF(_value);
  end;
end;

function TPyDelphiVarParameter.RichCompare(obj: PPyObject;
  Op: TRichComparisonOpcode): PPyObject;
var
  _value : PPyObject;
begin
  with GetPythonEngine do
  begin
    if Self.Value = nil then
      _value := Py_None
    else
      _value := Self.Value;
    Result := PyObject_RichCompare(_value, obj, Ord(Op));
  end;
end;

class procedure TPyDelphiVarParameter.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'VarParameter';
  PythonType.Name := PythonType.TypeName + 'Type';
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Container object allowing modification of Delphi var parameters from Python';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsCompare];
end;

procedure TPyDelphiVarParameter.SetValue(const Value: PPyObject);
begin
  if (fValue <> Value) and (Value <> GetSelf) then
  begin
    if Assigned(Value) and (Value.ob_type = PythonType.TheTypePtr) then // don't let embedding a var param into another one.
      Exit;
    if Assigned(fValue) and PythonOK then
      GetPythonEngine.Py_DECREF(fValue);
    fValue := Value;
    if PythonOK then
      GetPythonEngine.Py_XINCREF(fValue);
  end;
end;

function TPyDelphiVarParameter.Set_Value(AValue: PPyObject;
  Acontext: Pointer): Integer;
begin
  with GetPythonEngine do
  begin
    Adjust(@Self);
    Self.Value := AValue;
    Result := 0;
  end;
end;

{ TEventHandler }

constructor TEventHandler.Create(PyDelphiWrapper : TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  _FreeNotification : IFreeNotification;
  _Valid : Boolean;
begin
  _Valid := (Component is TComponent) or Component.GetInterface(IFreeNotification, _FreeNotification);
  if not _Valid then
    raise Exception.CreateFmt('Class %s must inherit from TComponent or implement IFreeNotification to work with events', [Component.ClassName]);
  Self.fComponent := Component;
  Self.PropertyInfo := PropertyInfo;
  Self.Callable := Callable;
  Self.PyDelphiWrapper := PyDelphiWrapper;
  GetPythonEngine.Py_INCREF(Self.Callable);
  if Assigned(_FreeNotification) then
    _FreeNotification.Subscribe(PyDelphiWrapper)
  else
    (Component as TComponent).FreeNotification(PyDelphiWrapper);
end;

destructor TEventHandler.Destroy;
var
  Method : TMethod;
begin
  Method := GetMethodProp(Component, PropertyInfo);
  if Method.Data = Self then
  begin
    Method.Code := nil;
    Method.Data := nil;
    // Set the event property to nil, only if it we hooked it
    SetMethodProp(Component, PropertyInfo, Method);
    if PythonOK then
      GetPythonEngine.Py_DECREF(Self.Callable);
  end;
  inherited;
end;

procedure TEventHandler.Unsubscribe;
var
  _FreeNotification : IFreeNotification;
begin
  if Component.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.UnSubscribe(PyDelphiWrapper);
end;

{ TNotifyEventHandler }

constructor TNotifyEventHandler.Create(PyDelphiWrapper : TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TNotifyEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TNotifyEventHandler.DoEvent(Sender: TObject);
begin
  RaiseNotifyEvent(PyDelphiWrapper, Callable, Sender);
end;

class function TNotifyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TNotifyEvent);
end;

{ TEventHandlers }

function TEventHandlers.Add(AEventHandler: TEventHandler) : Boolean;
begin
  fItems.Add(AEventHandler);
  Result := True;
end;

procedure TEventHandlers.Clear;
begin
  fItems.Clear;
end;

constructor TEventHandlers.Create(APyDelphiWrapper : TPyDelphiWrapper);
begin
  inherited Create;
  fPyDelphiWrapper := APyDelphiWrapper;
  fItems := TObjectList.Create;
  fRegisteredClasses := TClassList.Create;
end;

procedure TEventHandlers.Delete(AIndex: Integer);
begin
  fItems.Delete(AIndex);
end;

destructor TEventHandlers.Destroy;
begin
  fRegisteredClasses.Free;
  fItems.Free;
  inherited;
end;

function TEventHandlers.GetCallable(AComponent: TObject;
  APropInfo: PPropInfo): PPyObject;
var
  _idx : Integer;
begin
  _idx := IndexOf(AComponent, APropInfo);
  if _idx > -1 then
  begin
    Result := Items[_idx].Callable;
    GetPythonEngine.Py_XIncRef(Result);
  end
  else
    Result := GetPythonEngine.ReturnNone;
end;

function TEventHandlers.FindHandler(ATypeInfo: PTypeInfo): TEventHandlerClass;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to RegisteredClassCount-1 do
    if RegisteredClasses[i].GetTypeInfo = ATypeInfo then
    begin
      Result := RegisteredClasses[i];
      Break;
    end;
end;

function TEventHandlers.GetCallable(AComponent: TObject;
  const APropName: String): PPyObject;
var
  _propInfo : PPropInfo;
begin
  _propInfo := GetPropInfo(AComponent, APropName, [tkMethod]);
  if Assigned(_propInfo) then
    Result := GetCallable(AComponent, _propInfo)
  else
    Result := GetPythonEngine.ReturnNone;
end;

function TEventHandlers.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TEventHandlers.GetItem(AIndex: Integer): TEventHandler;
begin
  Result := TEventHandler(fItems[AIndex]);
end;

function TEventHandlers.GetRegisteredClass(
  AIndex: Integer): TEventHandlerClass;
begin
  Result := TEventHandlerClass(fRegisteredClasses[AIndex]);
end;

function TEventHandlers.GetRegisteredClassCount: Integer;
begin
  Result := fRegisteredClasses.Count;
end;

function TEventHandlers.IndexOf(AComponent: TObject;
  APropInfo: PPropInfo): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Component = AComponent) and (Items[i].PropertyInfo = APropInfo) then
    begin
      Result := i;
      Break;
    end;
end;

function TEventHandlers.Link(AComponent: TObject; APropInfo: PPropInfo;
  ACallable: PPyObject) : Boolean;
var
  _handlerClass : TEventHandlerClass;
begin
  Assert(Assigned(AComponent));
  Assert(Assigned(APropInfo));
  Assert(Assigned(ACallable));
  Result := False;
  with GetPythonEngine do
  begin
    if ACallable = Py_None then
    begin
      Unlink(AComponent, APropInfo); // it will assign nil to the event
      Result := True;
    end
    else if PyCallable_Check(ACallable) <> 0 then
    begin
      if SupportsFreeNotification(AComponent) then
      begin
        _handlerClass := FindHandler(APropInfo^.PropType^);
        if Assigned(_handlerClass) then
        begin
          Unlink(AComponent, APropInfo);
          Add(_handlerClass.Create(PyDelphiWrapper, AComponent, APropInfo, ACallable));
          Result := True;
        end
        else
          PyErr_SetString (PyExc_AttributeError^,
            PChar('No Registered EventHandler for events of type ' + APropInfo^.PropType^.Name));
      end
      else
        PyErr_SetString (PyExc_AttributeError^,
          PChar(Format('Class %s does not support events because it must either inherit from TComponent or implement interface IFreeNotification', [AComponent.ClassName])));
    end
    else
      PyErr_SetString (PyExc_AttributeError^,
        PChar('You can only assign a callable to a method property'));
  end;
end;

procedure TEventHandlers.RegisterHandler(AEventHandlerClass: TEventHandlerClass);
begin
  if fRegisteredClasses.IndexOf(AEventHandlerClass) < 0 then
    fRegisteredClasses.Add(AEventHandlerClass);
end;

function TEventHandlers.Unlink(AComponent: TObject;
  APropInfo: PPropInfo): Boolean;
var
  _idx : Integer;
begin
  _idx := IndexOf(AComponent, APropInfo);
  Result := _idx > -1;
  if Result then
    Delete(_idx);
end;

{ TRegisteredUnit }

procedure TRegisteredUnit.DefineFunctions(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

procedure TRegisteredUnit.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

procedure TRegisteredUnit.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

{ TRegisteredUnits }

procedure TRegisteredUnits.Add(ARegisteredModule: TRegisteredUnit);
begin
  fItems.Add(ARegisteredModule);
end;

constructor TRegisteredUnits.Create;
begin
  inherited Create;
  fItems := TObjectList.Create;
end;

destructor TRegisteredUnits.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TRegisteredUnits.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TRegisteredUnits.GetItem(AIndex: Integer): TRegisteredUnit;
begin
  Result := TRegisteredUnit(fItems[AIndex]);
end;

{ TPyDelphiWrapper }

constructor TPyDelphiWrapper.Create(AOwner: TComponent);
begin
  inherited;
  fClassRegister := TObjectList.Create(True);
  fHelperClassRegister := TStringList.Create;
  fEventHandlerList := TEventHandlers.Create(Self);

  if not (csDesigning in ComponentState) then
    CreateWrappers;

  // Enable sorting to benefit from binary search
  fHelperClassRegister.Sorted := True;
end;

function TPyDelphiWrapper.CreateComponent(pself, args: PPyObject): PPyObject;
//  Exposed function at the Module level
//  CreateComponent(ClassName, Owner)
var
  KlassName : PChar;
  _obj : PPyObject;
  OwnerComponent : TComponent;
  Klass : TClass;
  _comp : TObject;
  Component : TComponent;
  Ownership : TObjectOwnership;
begin
  Result := nil;
  CheckEngine;
  with Engine do begin
    if PyArg_ParseTuple( args, 'sO:CreateDelphiComponent', [@KlassName, @_obj] ) <> 0 then begin
      try
        Klass := GetClass(KlassName);
      except
        Klass := nil;
      end;
      if (Klass = nil) or not Klass.InheritsFrom(TComponent) then begin
        PyErr_SetString (PyExc_AttributeError^,
          PChar('Invalid class'));
        Exit;
      end;

      if CheckObjAttribute(_obj, 'Owner', TComponent, _comp) then
        OwnerComponent := TComponent(_comp)
      else
        Exit;
      Component := TComponentClass(Klass).Create(OwnerComponent);
      if Assigned(OwnerComponent) then
        Ownership := soReference
      else
        Ownership := soOwned;
      Result := Self.Wrap(Component, Ownership);
    end else
      PyErr_SetString (PyExc_AttributeError^,
        PChar('Invalid Arguments'));
  end;
end;

procedure TPyDelphiWrapper.CreateModuleFunctions;
var
  i : Integer;
begin
  if Assigned(FModule) then
  begin
    RegisterFunction(PChar('CreateComponent'), CreateComponent,
       PChar('CreateComponent(ComponentClass, Owner)'#10 +
       'Creates a component of type ComponentClass owned by Owner'));
    RegisterFunction(PChar('Abort'), Abort_Wrapper,
       PChar('Abort()'#10 +
       'Raises a silent exception.'));
    for i := 0 to RegisteredUnits.Count-1 do
      RegisteredUnits[i].DefineFunctions(Self);
  end;
end;

procedure TPyDelphiWrapper.CreateModuleVars;
var
  i : Integer;
begin
  Assert(Assigned(Module) and Module.Initialized);
  for i := 0 to RegisteredUnits.Count-1 do
    RegisteredUnits[i].DefineVars(Self);
end;

procedure TPyDelphiWrapper.CreatePyFunc(AModule: TPythonModule; AMethodDef: PPyMethodDef);
var
  d : PPyObject;
begin
  if Assigned(FModule) and FModule.Initialized then
    with GetPythonEngine do
    begin
      d := PyModule_GetDict(FModule.Module);
      Assert(Assigned(d));
      PyDict_SetItemString( d, AMethodDef^.ml_name, PyCFunction_New(AMethodDef, nil));
    end;
end;

procedure TPyDelphiWrapper.CreateWrappers;
var
  i : Integer;
begin
  // Helper Types
{$IFDEF DELPHI7_OR_HIGHER}
  fDelphiMethodType := RegisterHelperType(TPyDelphiMethodObject);
{$ENDIF}
  fDefaultIterType      := RegisterHelperType(TPyDelphiIterator);
  fDefaultContainerType := RegisterHelperType(TPyDelphiContainer);
  fVarParamType         := RegisterHelperType(TPyDelphiVarParameter);

  // Create and Register Wrapper for TObject
  RegisterDelphiWrapper(TPyDelphiObject);

  // Register Notify event Handler
  EventHandlers.RegisterHandler(TNotifyEventHandler);

  // Register wrappers for each Delphi unit
  for i := 0 to RegisteredUnits.Count-1 do
    RegisteredUnits[i].RegisterWrappers(Self);
end;

procedure TPyDelphiWrapper.DefineVar(const AName: String; const AValue: Variant);
var
  _obj : PPyObject;
begin
  CheckEngine;
  Assert(Assigned(Module));
  _obj := Engine.VariantAsPyObject(AValue);
  Module.SetVar(AName, _obj);
  Engine.Py_DECREF(_obj);
end;

procedure TPyDelphiWrapper.DefineVar(const AName: String; AValue: TObject);
var
  _obj : PPyObject;
begin
  Assert(Assigned(Module));
  _obj := Wrap(AValue);
  Module.SetVar(AName, _obj);
  Engine.Py_DECREF(_obj);
end;

destructor TPyDelphiWrapper.Destroy;
begin
  UnsubscribeFreeNotifications;
  // note that those pointers MUST be set to nil, otherwise it will crash
  // when calling inherited, as we have overridden SetEngine that tries to
  // assign the new engine value to the registered types.
  FreeAndNil(fClassRegister);
  FreeAndNil(fHelperClassRegister);
  FreeAndNil(fEventHandlerList);

  //No need to free PythonType objects since they are owned;
  inherited;
end;

procedure TPyDelphiWrapper.Finalize;
begin
  inherited;
  UnsubscribeFreeNotifications;
  if Assigned(fEventHandlerList) then
    fEventHandlerList.Clear;
end;

function TPyDelphiWrapper.GetHelperType(TypeName: string): TPythonType;
var
  Index : integer;
begin
  Index := fHelperClassRegister.IndexOf(TypeName);
  if Index >= 0 then
    Result := fHelperClassRegister.Objects[Index] as TPythonType
  else
    Result := nil;
end;

procedure TPyDelphiWrapper.Initialize;
var
  i : integer;
begin
  if Initialized then
    Exit;
  inherited;
  // Initialize Wrapper Types
  for i := 0 to fClassRegister.Count - 1 do
    with TRegisteredClass(fClassRegister[i]).PythonType do
      if not Initialized then Initialize;
  // Initialize Helper Types
  for i := 0 to fHelperClassRegister.Count - 1 do
    with TPythonType(fHelperClassRegister.Objects[i]) do
      if not Initialized then Initialize;
  // Initialize module
  if Assigned(FModule) then begin
    if Module.Initialized then
    begin
      CreateModuleFunctions;
      CreateModuleVars;
    end
    else
      Module.AddClient( Self );
  end;
end;

procedure TPyDelphiWrapper.ModuleReady(Sender : TObject);
begin
  inherited;
  CreateModuleFunctions;
  CreateModuleVars;
end;

procedure TPyDelphiWrapper.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FModule then
      FModule := nil
    else
      Notify(AComponent);
  end;
end;

procedure TPyDelphiWrapper.Notify(ADeletedObject: TObject);
Var
  i : integer;
begin
  //Free EventHandlers of Component
  if Assigned(fEventHandlerList) then
    for i := fEventHandlerList.Count - 1 downto 0 do
      if fEventHandlerList[i].Component = ADeletedObject then
        fEventHandlerList.Delete(i);
end;

procedure TPyDelphiWrapper.RegisterDelphiWrapper(
  AWrapperClass: TPyDelphiObjectClass);
Var
  RegisteredClass : TRegisteredClass;
begin
  Assert(Assigned(AWrapperClass));

  RegisteredClass := TRegisteredClass.Create;
  RegisteredClass.DelphiClass := AWrapperClass.DelphiObjectClass;
  RegisteredClass.PythonType := TPythonType.Create(Self);
  RegisteredClass.PythonType.Engine := Engine;
  RegisteredClass.PythonType.Module := fModule;
  RegisteredClass.PythonType.PyObjectClass := AWrapperClass;
  fClassRegister.Add(RegisteredClass);
  if AWrapperClass.DelphiObjectClass.InheritsFrom(TPersistent) then
    Classes.RegisterClass(TPersistentClass(AWrapperClass.DelphiObjectClass));
end;

function TPyDelphiWrapper.RegisterFunction(AFuncName: PChar;
  AFunc: PyCFunction; ADocString: PChar): PPyMethodDef;
begin
  Assert(Assigned(Module));
  Result := FModule.AddMethod(AFuncName, AFunc, ADocString);
  CreatePyFunc(FModule, Result);
end;

function TPyDelphiWrapper.RegisterFunction(AFuncName : PChar; AFunc : TDelphiMethod; ADocString : PChar ) : PPyMethodDef;
begin
  Assert(Assigned(Module));
  Result := FModule.AddDelphiMethod(AFuncName, AFunc, ADocString);
  CreatePyFunc(FModule, Result);
end;

function TPyDelphiWrapper.RegisterHelperType(APyObjectClass : TPyObjectClass) : TPythonType;
begin
  Result := TPythonType.Create(Self);
  Result.Engine := Engine;
  Result.Module := fModule;
  Result.PyObjectClass := APyObjectClass;
  fHelperClassRegister.AddObject(Result.Name, Result);
end;

procedure TPyDelphiWrapper.SetEngine(Value : TPythonEngine);
Var
  i : integer;
begin
  if Value <> Engine then begin
    inherited;
    // Delphi Types
    if Assigned(fClassRegister) then
      for i := 0 to fClassRegister.Count - 1 do
        TRegisteredClass(fClassRegister[i]).PythonType.Engine := Value;
    // Helper Types
    if Assigned(fHelperClassRegister) then
      for i := 0 to fHelperClassRegister.Count - 1 do
        TPythonType(fHelperClassRegister.Objects[i]).Engine := Value;
  end;
end;

procedure TPyDelphiWrapper.SetModule(const Value: TPythonModule);
Var
  i : integer;
begin
  if Value <> FModule then begin
    if Assigned(FModule) then
      FModule.RemoveFreeNotification(Self);
    FModule := Value;
    if Assigned(FModule) then
      FModule.FreeNotification(Self);
    if Assigned(fClassRegister) then
      for i := 0 to fClassRegister.Count - 1 do
        TRegisteredClass(fClassRegister[i]).PythonType.Module := Value;
    if Assigned(fHelperClassRegister) then
      for i := 0 to fHelperClassRegister.Count - 1 do
        TPythonType(fHelperClassRegister.Objects[i]).Module := Value;
    if Assigned(FModule) then
      if Initialized and (ComponentState * [csDesigning, csLoading] = []) then
        if FModule.Initialized then
        begin
          CreateModuleFunctions;
          CreateModuleVars;
        end
        else
          FModule.AddClient(Self);
  end;
end;

procedure TPyDelphiWrapper.UnsubscribeFreeNotifications;
Var
  i : integer;
begin
  if Assigned(fEventHandlerList) then
    for i := fEventHandlerList.Count - 1 downto 0 do
      fEventHandlerList[i].Unsubscribe;
end;


function TPyDelphiWrapper.Wrap(AObj: TObject;
  AOwnership: TObjectOwnership): PPyObject;
Var
  i : integer;
  DelphiClass : TClass;
  Index : integer;
begin
  CheckEngine;
  // We cast the python object to the right delphi type
  if not Assigned(AObj) then
    Result := Engine.ReturnNone
  else begin
    // find nearest registered ancestor
    Index := -1;
    DelphiClass := AObj.ClassType;
    while Assigned(DelphiClass) do begin
      for i := 0 to fClassRegister.Count - 1 do
        if TRegisteredClass(fClassRegister[i]).DelphiClass = DelphiClass then begin
          Index := i;
          break;
        end;
      if Index >= 0 then break;
      DelphiClass := DelphiClass.ClassParent;
    end;
    Assert(Index >= 0, 'Internal Error in PyDelphiWrapper.TObjectToPyObject'); // shouldn't happen

    Result := TRegisteredClass(fClassRegister[Index]).PythonType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiObject do begin
      DelphiObject := AObj;
      PyDelphiWrapper := Self;
      Owned := AOwnership = soOwned;
    end;
  end;
end;

initialization
  gNames := TStringList.Create;
finalization
  FreeAndNil(gNames);
  FreeAndNil(gRegisteredUnits);
end.

