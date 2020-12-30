unit VK.Entity.Event;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkEvent = class(TVkObject)
  private
    FAddress: string;
    FButton_text: string;
    FFriends: TArray<Integer>;
    FIs_favorite: Boolean;
    FMember_status: Integer;
    FText: string;
    FTime: Int64;
  public
    property Address: string read FAddress write FAddress;
    property ButtonText: string read FButton_text write FButton_text;
    property Friends: TArray<Integer> read FFriends write FFriends;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Text: string read FText write FText;
    property Time: Int64 read FTime write FTime;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkEvent;
  end;

implementation

{TVkEvent}

function TVkEvent.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkEvent.FromJsonString(AJsonString: string): TVkEvent;
begin
  result := TJson.JsonToObject<TVkEvent>(AJsonString)
end;

end.

