unit VK.Entity.AccountInfo;

interface

uses
  Generics.Collections, REST.Json.Types, REST.Json, VK.Types;

type
  TAccountInfoClass = class
  private
    [JSONName('2fa_required')]
    F2fa_required: Extended;
    [JSONName('country')]
    FCountry: string;
    [JSONName('https_required')]
    FHttps_required: Extended;
    [JSONName('intro')]
    FIntro: Extended;
    [JSONName('lang')]
    FLang: Extended;
    [JSONName('no_wall_replies')]
    FNo_wall_replies: Extended;
    [JSONName('own_posts_default')]
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
    class function FromJsonString(AJsonString: string): TAccountInfoClass;
  end;

implementation

{TAccountInfoClass}

function TAccountInfoClass.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(self);
end;

class function TAccountInfoClass.FromJsonString(AJsonString: string): TAccountInfoClass;
begin
  Result := TJson.JsonToObject<TAccountInfoClass>(AJsonString)
end;

end.

