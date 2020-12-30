unit VK.Entity.Database.Universities;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkUniversity = class(TVkObject)
  private
    FTitle: string;
  public
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

uses
  VK.CommonUtils;

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
begin
  TArrayHelp.FreeArrayOfObject<TVkUniversity>(FItems);
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

