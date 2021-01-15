unit VK.Entity.OldApp;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkOldApp = class(TVkObject)
  private
    FPhoto_604: string;
    FPhoto_130: string;
    FName: string;
  public
    property Name: string read FName write FName;
    property Photo130: string read FPhoto_130 write FPhoto_130;
    property Photo604: string read FPhoto_604 write FPhoto_604;
  end;

implementation

end.

