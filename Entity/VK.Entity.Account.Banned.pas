unit VK.Entity.Account.Banned;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.User;

type
  TVkBannedList = class
  private
    FCount: Integer;
    FItems: TArray<Integer>;
    FProfiles: TArray<TVkUser>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
    property Profiles: TArray<TVkUser> read FProfiles write FProfiles;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBannedList;
  end;

implementation

{TVkBannedList}

function TVkBannedList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

destructor TVkBannedList.Destroy;
var
  User: TVkUser;
begin

  for User in FProfiles do
    User.Free;

  inherited;
end;

class function TVkBannedList.FromJsonString(AJsonString: string): TVkBannedList;
begin
  result := TJson.JsonToObject<TVkBannedList>(AJsonString)
end;

end.

