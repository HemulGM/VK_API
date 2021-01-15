unit VK.Entity.Storage;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkStorageItem = class
  private
    FKey: string;
    FValue: string;
  public
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
  end;

  TVkStorageItems = class(TVkEntity)
  private
    FItems: TArray<TVkStorageItem>;
  public
    property Items: TArray<TVkStorageItem> read FItems write FItems;
    destructor Destroy; override;
  end;

  TVkStorageKey = string;

  TVkStorageKeys = class(TVkEntity)
  private
    FItems: TArray<TVkStorageKey>;
  public
    property Items: TArray<TVkStorageKey> read FItems write FItems;
  end;

implementation

uses
  VK.CommonUtils;

{TVkStorageItems}

destructor TVkStorageItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStorageItem>(FItems);
  inherited;
end;

end.

