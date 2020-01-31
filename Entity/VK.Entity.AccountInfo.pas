unit VK.Entity.AccountInfo;

interface

uses
  Generics.Collections, REST.Json.Types, REST.Json, VK.Types;

type
  TVkAccountInfo = class
  private
    F2fa_required: Extended;
    FCountry: string;
    FHttps_required: Extended;
    FIntro: Extended;
    FLang: Extended;
    FNo_wall_replies: Extended;
    FOwn_posts_default: Extended;
  public
    property FA2Required: Extended read F2fa_required write F2fa_required;
    property Country: string read FCountry write FCountry;
    property HttpsRequired: Extended read FHttps_required write FHttps_required;
    property Intro: Extended read FIntro write FIntro;
    property Lang: Extended read FLang write FLang;
    property NoWallReplies: Extended read FNo_wall_replies write FNo_wall_replies;
    property OwnPostsDefault: Extended read FOwn_posts_default write FOwn_posts_default;
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

