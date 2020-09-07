unit VK.Entity.Database.Schools;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkSchoolClass = record
    Id: Integer;
    Text: string;
    class function Create(Id: Integer; Text: string): TVkSchoolClass; static;
  end;

  TVkSchoolClasses = class
  private
    FItems: TArray<TVkSchoolClass>;
  public
    property Items: TArray<TVkSchoolClass> read FItems write FItems;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchoolClasses;
  end;

  TVkSchool = class
  private
    FId: Integer;
    FTitle: string;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchool;
  end;

  TVkSchools = class
  private
    FCount: Integer;
    FItems: TArray<TVkSchool>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkSchool> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchools;
  end;

implementation

uses
  System.Json, VK.CommonUtils;

{TVkSchool}

function TVkSchool.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSchool.FromJsonString(AJsonString: string): TVkSchool;
begin
  result := TJson.JsonToObject<TVkSchool>(AJsonString)
end;

{TVkSchools}

destructor TVkSchools.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSchool>(FItems);
  inherited;
end;

function TVkSchools.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSchools.FromJsonString(AJsonString: string): TVkSchools;
begin
  result := TJson.JsonToObject<TVkSchools>(AJsonString)
end;

{ TVkSchoolClasses }

class function TVkSchoolClasses.FromJsonString(AJsonString: string): TVkSchoolClasses;
var
  JArray, JAItem: TJSONArray;
  i: Integer;
begin
  Result := TVkSchoolClasses.Create;
  JArray := TJSONArray(TJSONObject.ParseJSONValue(AJsonString));
  SetLength(Result.FItems, JArray.Count);
  for i := 0 to JArray.Count - 1 do
  begin
    JAItem := TJSONArray(JArray.Items[i]);
    Result.Items[i] := TVkSchoolClass.Create(JAItem.Items[0].AsType<Integer>, JAItem.Items[1].AsType<string>);
  end;
  JArray.Free;
end;

function TVkSchoolClasses.ToJsonString: string;
var
  JArray, JAItem: TJSONArray;
  i: Integer;
begin
  JArray := TJSONArray.Create;
  for i := Low(Items) to High(Items) do
  begin
    JAItem := TJSONArray.Create;
    JAItem.AddElement(TJSONNumber.Create(Items[i].Id));
    JAItem.AddElement(TJSONString.Create(Items[i].Text));
    JArray.AddElement(JAItem);
  end;
  Result := JArray.ToJSON;
  JArray.Free;
end;

{ TVkSchoolClass }

class function TVkSchoolClass.Create(Id: Integer; Text: string): TVkSchoolClass;
begin
  Result.Id := Id;
  Result.Text := Text;
end;

end.

