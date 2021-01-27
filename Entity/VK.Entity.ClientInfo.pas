unit VK.Entity.ClientInfo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkClientInfo = class(TVkEntity)
  private
    FButton_actions: TArray<string>;
    FCarousel: Boolean;
    FInline_keyboard: Boolean;
    FKeyboard: Boolean;
    FLang_id: Integer;
  public
    property ButtonActions: TArray<string> read FButton_actions write FButton_actions;
    property Carousel: Boolean read FCarousel write FCarousel;
    property InlineKeyboard: Boolean read FInline_keyboard write FInline_keyboard;
    property Keyboard: Boolean read FKeyboard write FKeyboard;
    property LangId: Integer read FLang_id write FLang_id;
  end;

implementation

end.

