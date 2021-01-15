unit VK.Entity.Secure;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkSecureCheckToken = class(TVkEntity)
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
  end;

implementation

end.

