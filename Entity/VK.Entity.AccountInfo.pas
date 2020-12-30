unit VK.Entity.AccountInfo;

interface

uses
  Generics.Collections, REST.Json.Types, REST.Json, VK.Types;

type
  TVkAccountInfo = class
  private
    F2fa_required: Integer;
    FCountry: string;
    FHttps_required: Integer;
    FIntro: Integer;
    FLang: Integer;
    FNo_wall_replies: Integer;
    FOwn_posts_default: Integer;
  public
    property FA2Required: Integer read F2fa_required write F2fa_required;
    property Country: string read FCountry write FCountry;
    property HttpsRequired: Integer read FHttps_required write FHttps_required;
    property Intro: Integer read FIntro write FIntro;
    property Lang: Integer read FLang write FLang;
    property NoWallReplies: Integer read FNo_wall_replies write FNo_wall_replies;
    property OwnPostsDefault: Integer read FOwn_posts_default write FOwn_posts_default;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAccountInfo;
  end;

implementation

{TVkAccountInfo}

function TVkAccountInfo.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(self);
end;

class function TVkAccountInfo.FromJsonString(AJsonString: string): TVkAccountInfo;
begin
  Result := TJson.JsonToObject<TVkAccountInfo>(AJsonString)
end;

end.

