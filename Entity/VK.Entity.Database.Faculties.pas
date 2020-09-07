unit VK.Entity.Database.Faculties;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkFaculty = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaculty;
  end;

  TVkFaculties = class
  private
    FCount: Extended;
    FItems: TArray<TVkFaculty>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TVkFaculty> read FItems write FItems;
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

