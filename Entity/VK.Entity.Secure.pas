unit VK.Entity.Secure;

interface

uses
  Generics.Collections, REST.JsonReflect, VK.Wrap.Interceptors, Rest.Json,
  VK.Entity.Common, VK.Types;

type
  TVkSecureCheckToken = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FExpire: TDateTime;
    FSuccess: Boolean;
    FUser_id: TVkPeerId;
  public
    property Date: TDateTime read FDate write FDate;
    property Expire: TDateTime read FExpire write FExpire;
    property Success: Boolean read FSuccess write FSuccess;
    property UserId: TVkPeerId read FUser_id write FUser_id;
  end;

implementation

end.

