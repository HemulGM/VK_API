unit VK.Entity.AccountInfoRequest;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkAccountInfoRequest = class
  private
    FChanged: Extended;
    FFirst_name: string;
    FId: Extended;
    FLast_name: string;
    FStatus: string;
  public
    property changed: Extended read FChanged write FChanged;
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property last_name: string read FLast_name write FLast_name;
    property status: string read FStatus write FStatus;
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

