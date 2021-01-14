unit VK.Entity.Doc.Types;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkDocType = class(TVkObject)
  private
    FCount: Integer;
    FName: string;
  public
    property Count: Integer read FCount write FCount;
    property Name: string read FName write FName;
  end;

  TVkDocTypes = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkDocType>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkDocType> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkDocTypes}

destructor TVkDocTypes.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkDocType>(FItems);
  inherited;
end;

end.

