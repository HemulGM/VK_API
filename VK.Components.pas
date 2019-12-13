unit VK.Components;

interface

uses
  System.SysUtils, System.Classes, VK.API;

type
  TVK = class(TCustomVK)
  published
    property OnLogin;
    property OnError;
    property AppID;
    property AppKey;
    property EndPoint; // default 'https://oauth.vk.com/authorize';
    property Permissions; // default 'groups,friends,wall,photos,video,docs,notes,market,messages';
    property APIVersion; // default '5.101';
    property BaseURL; // default 'https://api.vk.com/method';
    property ServiceKey;
    property UseServiceKeyOnly default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HGM Components', [TVK]);
end;

end.

