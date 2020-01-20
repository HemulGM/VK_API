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
    property address: string read FAddress write FAddress;
    property button_text: string read FButton_text write FButton_text;
    property friends: TArray<Extended> read FFriends write FFriends;
    property id: Extended read FId write FId;
    property is_favorite: Boolean read FIs_favorite write FIs_favorite;
    property member_status: Extended read FMember_status write FMember_status;
    property text: string read FText write FText;
    property time: Extended read FTime write FTime;
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

