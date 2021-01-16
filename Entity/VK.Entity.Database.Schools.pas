unit VK.Entity.Database.Schools;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkSchoolClass = record
    Id: Integer;
    Text: string;
    class function Create(Id: Integer; Text: string): TVkSchoolClass; static;
  end;

  TVkSchoolClasses = class(TVkEntity)
  private
    FItems: TArray<TVkSchoolClass>;
  public
    property Items: TArray<TVkSchoolClass> read FItems write FItems;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSchoolClasses;
  end;

  TVkSchool = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkSchools = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkSchool>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkSchool> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  System.Json, VK.CommonUtils;

{TVkSchools}

destructor TVkSchools.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkSchool>(FItems);
  {$ENDIF}
  inherited;
end;

{ TVkSchoolClasses }

class function TVkSchoolClasses.FromJsonString(AJsonString: string): TVkSchoolClasses;
var
  JArray, JAItem: TJSONArray;
  i: Integer;
begin
  JArray := TJSONArray(TJSONObject.ParseJSONValue(AJsonString));
  try
    Result := TVkSchoolClasses.Create;
    SetLength(Result.FItems, JArray.Count);
    for i := 0 to JArray.Count - 1 do
    begin
      JAItem := TJSONArray(JArray.Items[i]);
      Result.Items[i] := TVkSchoolClass.Create(JAItem.Items[0].AsType<Integer>, JAItem.Items[1].AsType<string>);
    end;
  finally
    JArray.Free;
  end;
end;

function TVkSchoolClasses.ToJsonString: string;
var
  JArray, JAItem: TJSONArray;
  i: Integer;
begin
  JArray := TJSONArray.Create;
  try
    for i := Low(Items) to High(Items) do
    begin
      JAItem := TJSONArray.Create;
      JAItem.AddElement(TJSONNumber.Create(Items[i].Id));
      JAItem.AddElement(TJSONString.Create(Items[i].Text));
      JArray.AddElement(JAItem);
    end;
    Result := JArray.ToJSON;
  finally
    JArray.Free;
  end;
end;

{ TVkSchoolClass }

class function TVkSchoolClass.Create(Id: Integer; Text: string): TVkSchoolClass;
begin
  Result.Id := Id;
  Result.Text := Text;
end;

end.

