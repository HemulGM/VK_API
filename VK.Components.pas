unit VK.Components;

interface

uses
  System.SysUtils, System.Classes, VK.API;

type
  TVK = class(TCustomVK)
  published
    property OnLogin;
    property AppID;
    property AppKey;
    property EndPoint;
    property Permissions;
    property APIVersion;
    property BaseURL;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HGM Components', [TVK]);
end;

end.
