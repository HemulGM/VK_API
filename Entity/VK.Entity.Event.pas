unit VK.Entity.Event;

interface

uses
  Generics.Collections, REST.JsonReflect, VK.Wrap.Interceptors, Rest.Json,
  VK.Entity.Common, VK.Types;

type
  TVkEvent = class(TVkObject)
  private
    FAddress: string;
    FButton_text: string;
    FFriends: TArray<TVkPeerId>;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_favorite: Boolean;
    FMember_status: Integer;
    FText: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FTime: TDateTime;
    FAccess_key: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Address: string read FAddress write FAddress;
    property ButtonText: string read FButton_text write FButton_text;
    property Friends: TArray<TVkPeerId> read FFriends write FFriends;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    {1 -- точно идЄт;
      2 -- возможно пойдЄт;
      3 -- не идЄт.}
    { TODO -oHemulGM -c : —делать тип 16.02.2021 13:56:39 }
    property MemberStatus: Integer read FMember_status write FMember_status;
    property Text: string read FText write FText;
    property Time: TDateTime read FTime write FTime;
  end;

implementation

end.

