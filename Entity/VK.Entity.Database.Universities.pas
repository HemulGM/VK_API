unit VK.Entity.Database.Universities;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkUniversity = class
  private
    FId: Integer;
    FTitle: string;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUniversity;
  end;

  TVkUniversities = class
  private
    FCount: Integer;
    FItems: TArray<TVkUniversity>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkUniversity> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkUniversities;
  end;

implementation

{TVkUniversity}

function TVkUniversity.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUniversity.FromJsonString(AJsonString: string): TVkUniversity;
begin
  result := TJson.JsonToObject<TVkUniversity>(AJsonString)
end;

{TVkUniversities}

destructor TVkUniversities.Destroy;
var
  LitemsItem: TVkUniversity;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkUniversities.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkUniversities.FromJsonString(AJsonString: string): TVkUniversities;
begin
  result := TJson.JsonToObject<TVkUniversities>(AJsonString)
end;

end.

