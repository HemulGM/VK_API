unit VK.Entity.Secure;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common;

type
  TVkSecureCheckToken = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FExpire: TDateTime;
    FSuccess: Boolean;
    FUser_id: Integer;
  public
    property Date: TDateTime read FDate write FDate;
    property Expire: TDateTime read FExpire write FExpire;
    property Success: Boolean read FSuccess write FSuccess;
    property UserId: Integer read FUser_id write FUser_id;
  end;

implementation

end.

