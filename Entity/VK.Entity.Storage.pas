unit VK.Entity.Storage;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkStorageItem = class(TVkEntity)
  private
    FKey: string;
    FValue: string;
  public
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
  end;

  TVkStorageItems = TVkEntityList<TVkStorageItem>;

  TVkStorageKey = string;

  TVkStorageKeys = TVkEntityListSimple<TVkStorageKey>;

implementation

end.

