unit TinyWideStrings;
//  Tiny WideStringList implementation for versions of Delphi prior to BDS2006

interface

uses Classes, SysUtils;

{ TWideStringList class }
type
  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

  TWideStringList = class(TPersistent)
  private
    FList: PWideStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
  protected
    procedure Error(const Msg: WideString; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function Get(Index: Integer): WideString;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetObject(Index: Integer): TObject;
    procedure Put(Index: Integer; const S: WideString);
    procedure PutObject(Index: Integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: Integer);
    procedure InsertItem(Index: Integer; const S: WideString; AObject: TObject); 
  public
    destructor Destroy; override;
    function Add(const S: WideString): Integer;
    function AddObject(const S: WideString; AObject: TObject): Integer;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const S: WideString);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: WideString read Get write Put; default;
  end;

implementation

uses Windows, RTLConsts;

{ TWideStringList }

procedure TWideStringList.Error(const Msg: WideString; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TWideStringList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

destructor TWideStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TWideStringList.Add(const S: WideString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TWideStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := FCount;
  InsertItem(Result, S, AObject);
end;

procedure TWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
end;

function TWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  InsertObject(Index, S, nil);
end;

procedure TWideStringList.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString; AObject: TObject);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
end;

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  FList^[Index].FString := S;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  FList^[Index].FObject := AObject;
end;

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

end.
