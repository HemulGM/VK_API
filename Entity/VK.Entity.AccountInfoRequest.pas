unit VK.Entity.AccountInfoRequest;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkAccountInfoRequest = class(TVkObject)
  private
    FChanged: Integer;
    FFirst_name: string;
    FLast_name: string;
    FStatus: string;
  public
    property Changed: Integer read FChanged write FChanged;
    property FirstName: string read FFirst_name write FFirst_name;
    property Last_name: string read FLast_name write FLast_name;
    property Status: string read FStatus write FStatus;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAccountInfoRequest;
  end;

implementation

{TVkAccountInfoRequest}

function TVkAccountInfoRequest.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAccountInfoRequest.FromJsonString(AJsonString: string): TVkAccountInfoRequest;
begin
  result := TJson.JsonToObject<TVkAccountInfoRequest>(AJsonString)
end;

end.

