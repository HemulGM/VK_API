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
    property LastName: string read FLast_name write FLast_name;
    property Status: string read FStatus write FStatus;
  end;

implementation

end.

