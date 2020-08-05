unit VK.Entity.Storage;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkStorageItem = class
  private
    FKey: string;
    FValue: string;
  public
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStorageItem;
  end;

  TVkStorageItems = class
  private
    FItems: TArray<TVkStorageItem>;
  public
    property Items: TArray<TVkStorageItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStorageItems;
  end;

  TVkStorageKey = string;

  TVkStorageKeys = class
  private
    FItems: TArray<TVkStorageKey>;
  public
    property Items: TArray<TVkStorageKey> read FItems write FItems;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStorageKeys;
  end;

implementation

{TVkStorageItem}

function TVkStorageItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStorageItem.FromJsonString(AJsonString: string): TVkStorageItem;
begin
  result := TJson.JsonToObject<TVkStorageItem>(AJsonString)
end;

{TVkStorageItems}

destructor TVkStorageItems.Destroy;
var
  LresponseItem: TVkStorageItem;
begin

  for LresponseItem in FItems do
    LresponseItem.Free;

  inherited;
end;

function TVkStorageItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStorageItems.FromJsonString(AJsonString: string): TVkStorageItems;
begin
  result := TJson.JsonToObject<TVkStorageItems>(AJsonString)
end;

{TVkStorageKeys}

function TVkStorageKeys.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStorageKeys.FromJsonString(AJsonString: string): TVkStorageKeys;
begin
  result := TJson.JsonToObject<TVkStorageKeys>(AJsonString)
end;

end.

