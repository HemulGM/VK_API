unit VK.Entity.Database.Faculties;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkFaculty = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaculty;
  end;

  TVkFaculties = class
  private
    FCount: Integer;
    FItems: TArray<TVkFaculty>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFaculty> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaculties;
  end;

implementation

uses
  VK.CommonUtils;

{TVkFaculty}

function TVkFaculty.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaculty.FromJsonString(AJsonString: string): TVkFaculty;
begin
  result := TJson.JsonToObject<TVkFaculty>(AJsonString)
end;

{TVkFaculties}

destructor TVkFaculties.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaculty>(FItems);
  inherited;
end;

function TVkFaculties.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaculties.FromJsonString(AJsonString: string): TVkFaculties;
begin
  result := TJson.JsonToObject<TVkFaculties>(AJsonString)
end;

end.

