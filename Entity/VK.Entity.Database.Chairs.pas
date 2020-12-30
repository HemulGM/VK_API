unit VK.Entity.Database.Chairs;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkChair = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChair;
  end;

  TVkChairs = class
  private
    FCount: Integer;
    FItems: TArray<TVkChair>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkChair> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChairs;
  end;

implementation

uses
  VK.CommonUtils;

{TVkChair}

function TVkChair.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChair.FromJsonString(AJsonString: string): TVkChair;
begin
  result := TJson.JsonToObject<TVkChair>(AJsonString)
end;

{TVkChairs}

destructor TVkChairs.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkChair>(FItems);
  inherited;
end;

function TVkChairs.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChairs.FromJsonString(AJsonString: string): TVkChairs;
begin
  result := TJson.JsonToObject<TVkChairs>(AJsonString)
end;

end.

