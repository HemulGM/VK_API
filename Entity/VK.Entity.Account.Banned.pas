unit VK.Entity.Account.Banned;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile;

type
  TVkBannedList = class
  private
    FCount: Integer;
    FItems: TArray<Integer>;
    FProfiles: TArray<TVkProfile>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBannedList;
  end;

implementation

uses
  VK.CommonUtils;

{TVkBannedList}

function TVkBannedList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

destructor TVkBannedList.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkBannedList.FromJsonString(AJsonString: string): TVkBannedList;
begin
  result := TJson.JsonToObject<TVkBannedList>(AJsonString)
end;

end.

