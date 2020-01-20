unit VK.Entity.ClientInfo;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkClientInfo = class
  private
    FButton_actions: TArray<string>;
    FCarousel: Boolean;
    FInline_keyboard: Boolean;
    FKeyboard: Boolean;
    FLang_id: Extended;
  public
    property button_actions: TArray<string> read FButton_actions write FButton_actions;
    property carousel: Boolean read FCarousel write FCarousel;
    property inline_keyboard: Boolean read FInline_keyboard write FInline_keyboard;
    property keyboard: Boolean read FKeyboard write FKeyboard;
    property lang_id: Extended read FLang_id write FLang_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkClientInfo;
  end;

implementation

{TVkClientInfo}

function TVkClientInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkClientInfo.FromJsonString(AJsonString: string): TVkClientInfo;
begin
  result := TJson.JsonToObject<TVkClientInfo>(AJsonString)
end;

end.

