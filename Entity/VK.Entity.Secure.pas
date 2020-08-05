unit VK.Entity.Secure;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkSecureCheckToken = class
  private
    FDate: Int64;
    FExpire: Int64;
    FSuccess: Boolean;
    FUser_id: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property Expire: Int64 read FExpire write FExpire;
    property Success: Boolean read FSuccess write FSuccess;
    property UserId: Integer read FUser_id write FUser_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSecureCheckToken;
  end;

implementation

{TVkSecureCheckToken}

function TVkSecureCheckToken.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSecureCheckToken.FromJsonString(AJsonString: string): TVkSecureCheckToken;
begin
  result := TJson.JsonToObject<TVkSecureCheckToken>(AJsonString)
end;

end.

