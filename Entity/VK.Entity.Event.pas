unit VK.Entity.Event;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkEvent = class
  private
    FAddress: string;
    FButton_text: string;
    FFriends: TArray<Extended>;
    FId: Extended;
    FIs_favorite: Boolean;
    FMember_status: Extended;
    FText: string;
    FTime: Extended;
  public
    property Address: string read FAddress write FAddress;
    property ButtonText: string read FButton_text write FButton_text;
    property Friends: TArray<Extended> read FFriends write FFriends;
    property Id: Extended read FId write FId;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property MemberStatus: Extended read FMember_status write FMember_status;
    property Text: string read FText write FText;
    property Time: Extended read FTime write FTime;
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

