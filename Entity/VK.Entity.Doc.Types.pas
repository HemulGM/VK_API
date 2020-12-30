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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDocType;
  end;

  TVkDocTypes = class
  private
    FCount: Integer;
    FItems: TArray<TVkDocType>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkDocType> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDocTypes;
  end;

implementation

uses
  VK.CommonUtils;

{TVkDocType}

function TVkDocType.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocType.FromJsonString(AJsonString: string): TVkDocType;
begin
  result := TJson.JsonToObject<TVkDocType>(AJsonString)
end;

{TVkDocTypes}

destructor TVkDocTypes.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkDocType>(FItems);
  inherited;
end;

function TVkDocTypes.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocTypes.FromJsonString(AJsonString: string): TVkDocTypes;
begin
  result := TJson.JsonToObject<TVkDocTypes>(AJsonString)
end;

end.

