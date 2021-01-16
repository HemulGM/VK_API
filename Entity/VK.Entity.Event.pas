unit VK.Entity.Event;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common;

type
  TVkEvent = class(TVkObject)
  private
    FAddress: string;
    FButton_text: string;
    FFriends: TArray<Integer>;
    FIs_favorite: Boolean;
    FMember_status: Integer;
    FText: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTime: TDateTime;
  public
    property Address: string read FAddress write FAddress;
    property ButtonText: string read FButton_text write FButton_text;
    property Friends: TArray<Integer> read FFriends write FFriends;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Text: string read FText write FText;
    property Time: TDateTime read FTime write FTime;
  end;

implementation

end.

