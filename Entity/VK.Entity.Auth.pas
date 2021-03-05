unit VK.Entity.Auth;

interface

uses
  REST.Json, REST.Json.Types, VK.Entity.Common;

type
  TVkAuthRestore = class(TVkEntity)
  private
    FSid: string;
    FSuccess: Boolean;
  public
    property Sid: string read FSid write FSid;
    property Success: Boolean read FSuccess write FSuccess;
  end;

  TVkAuthSignup = class(TVkEntity)
  private
    FSid: string;
    FSuccess: Boolean;
  public
    property Sid: string read FSid write FSid;
    property Success: Boolean read FSuccess write FSuccess;
  end;

implementation

end.

