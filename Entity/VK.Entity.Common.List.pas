unit VK.Entity.Common.List;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Types, VK.Entity.Common;

type
  TVkEntityListSimple<T> = class(TVkEntity)
  protected
    FCount: Integer;
    FItems: TArray<T>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<T> read FItems write FItems;
    procedure Append(Items: TVkEntityListSimple<T>);
  end;

  TVkEntityList<T: TVkEntity> = class(TVkEntityListSimple<T>)
  protected
    [JSONMarshalledAttribute(False)]
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Items: TVkEntityList<T>);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkObjectList<T: TVkObject> = class(TVkObject)
  private
    FCount: Integer;
    FItems: TArray<T>;
    [JSONMarshalledAttribute(False)]
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<T> read FItems write FItems;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Items: TVkObjectList<T>);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkIdList = TVkEntityListSimple<Integer>;

  TVkBasicIndexItems = TVkEntityListSimple<Integer>;

implementation

uses
  VK.CommonUtils;

{ TVkEntityList<T> }

procedure TVkEntityList<T>.Append(Items: TVkEntityList<T>);
var
  OldLen: Integer;
begin
  OldLen := Length(Items.Items);
  SetLength(FItems, OldLen + Length(Items.Items));
  Move(Items.Items[0], FItems[OldLen], Length(Items.Items) * SizeOf(T));
end;

constructor TVkEntityList<T>.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkEntityList<T>.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<T>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

procedure TVkEntityList<T>.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkObjectList<T> }

procedure TVkObjectList<T>.Append(Items: TVkObjectList<T>);
var
  OldLen: Integer;
begin
  OldLen := Length(Items.Items);
  SetLength(FItems, OldLen + Length(Items.Items));
  Move(Items.Items[0], FItems[OldLen], Length(Items.Items) * SizeOf(T));
end;

constructor TVkObjectList<T>.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkObjectList<T>.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<T>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

procedure TVkObjectList<T>.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkEntityListSimple<T> }

procedure TVkEntityListSimple<T>.Append(Items: TVkEntityListSimple<T>);
var
  OldLen: Integer;
begin
  OldLen := Length(Items.Items);
  SetLength(FItems, OldLen + Length(Items.Items));
  Move(Items.Items[0], FItems[OldLen], Length(Items.Items) * SizeOf(T));
end;

end.

