{-----------------------------------------------------------------------------
 Unit Name: cPyRemoteDebugger
 Author:    Kiriakos Vlahos
 Date:      23-April-2006
 Purpose:
 History:   Remote debugger based on rppdb2
-----------------------------------------------------------------------------}

unit cPyRemoteDebugger;

interface

implementation

uses
  Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine, Forms, Contnrs,
  cPyBaseDebugger;

type
  TRemFrameInfo = class(TBaseFrameInfo)
  private
    fPyFrame: Variant;
  protected
  // Implementation of the Base class for the internal debugger
    function GetFunctionName : string; override;
    function GetFileName : string; override;
    function GetLine : integer; override;
  published
  public
    constructor Create(Frame : Variant);
    property PyFrame : Variant read fPyFrame;
  end;

  TRemNameSpaceItem = class(TBaseNameSpaceItem)
  // Implementation of the Base class for the internal debugger
  private
    fChildNodes : TObjectList;
    fName : string;
  protected
    function GetName : string; override;
    function GetObjectType : string; override;
    function GetValue : string; override;
    function GetDocString : string; override;
    function GetChildCount : integer; override;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; override;
  public
    constructor Create(aName : string; aPyObject : Variant);
    destructor Destroy; override;
    function IsDict : Boolean; override;
    function IsModule : Boolean; override;
    function IsFunction : Boolean; override;
    function IsMethod : Boolean; override;
    function Has__dict__ : Boolean; override;
    procedure GetChildNodes; override;
  end;

  TPyRemDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  protected
    procedure SetDebuggerBreakpoints; override;
  public
    constructor Create;

    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto(Editor : IEditor); override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Pause; override;
    procedure Abort; override;
    procedure Evaluate(const Expr : string; out ObjType, Value : string); override;
    procedure GetCallStack(CallStackList : TObjectList); override;
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
  end;

{ TRemFrameInfo }

constructor TRemFrameInfo.Create(Frame: Variant);
begin

end;

function TRemFrameInfo.GetFileName: string;
begin
  Result := '';
end;

function TRemFrameInfo.GetFunctionName: string;
begin
  Result := '';
end;

function TRemFrameInfo.GetLine: integer;
begin
  Result := 0;
end;

{ TRemNameSpaceItem }

constructor TRemNameSpaceItem.Create(aName: string; aPyObject: Variant);
begin

end;

destructor TRemNameSpaceItem.Destroy;
begin
  if Assigned(fChildNodes) then
    fChildNodes.Free;
  inherited;
end;

function TRemNameSpaceItem.GetChildCount: integer;
begin
  Result := 0;
end;

function TRemNameSpaceItem.GetChildNode(Index: integer): TBaseNameSpaceItem;
begin
  Result := nil;
end;

procedure TRemNameSpaceItem.GetChildNodes;
begin

end;

function TRemNameSpaceItem.GetDocString: string;
begin
  Result := '';
end;

function TRemNameSpaceItem.GetName: string;
begin
  Result := fName;
end;

function TRemNameSpaceItem.GetObjectType: string;
begin
  Result := '';
end;

function TRemNameSpaceItem.GetValue: string;
begin
  Result := '';
end;

function TRemNameSpaceItem.Has__dict__: Boolean;
begin
  Result := False;
end;

function TRemNameSpaceItem.IsDict: Boolean;
begin
  Result := False;
end;

function TRemNameSpaceItem.IsFunction: Boolean;
begin
  Result := False;
end;

function TRemNameSpaceItem.IsMethod: Boolean;
begin
  Result := False;
end;

function TRemNameSpaceItem.IsModule: Boolean;
begin
  Result := False;
end;

{ TPyRemDebugger }

procedure TPyRemDebugger.Abort;
begin

end;

constructor TPyRemDebugger.Create;
begin

end;

procedure TPyRemDebugger.Evaluate(const Expr: string; out ObjType,
  Value: string);
begin

end;

procedure TPyRemDebugger.GetCallStack(CallStackList: TObjectList);
begin

end;

function TPyRemDebugger.GetFrameGlobals(
  Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
end;

function TPyRemDebugger.GetFrameLocals(
  Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
end;

procedure TPyRemDebugger.Pause;
begin

end;

procedure TPyRemDebugger.Run(Editor: IEditor; InitStepIn: Boolean);
begin

end;

procedure TPyRemDebugger.RunToCursor(Editor: IEditor; ALine: integer);
begin

end;

procedure TPyRemDebugger.SetDebuggerBreakpoints;
begin

end;

procedure TPyRemDebugger.StepInto(Editor: IEditor);
begin

end;

procedure TPyRemDebugger.StepOut;
begin

end;

procedure TPyRemDebugger.StepOver;
begin

end;

end.
