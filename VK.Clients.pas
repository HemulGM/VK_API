unit VK.Clients;

interface

type
  /// <summary>
  /// Данные клиента AppId (client_id) + AppKey (client_secret)
  /// Имеются данные оф. клиентов для использования
  /// </summary>
  TVkApplicationData = record
    AppId: string;
    AppKey: string;
    //With AppKey
    class function Android: TVkApplicationData; static;
    class function VKAdmin: TVkApplicationData; static;
    class function IPhone: TVkApplicationData; static;
    class function IPad: TVkApplicationData; static;
    class function WindowsDesktop: TVkApplicationData; static;
    class function WindowsPhone: TVkApplicationData; static;
    //Without AppKey
    class function VKAdminIOS: TVkApplicationData; static;
    class function VKLive: TVkApplicationData; static;
    class function VKLiveAndroid: TVkApplicationData; static;
    class function Vinci: TVkApplicationData; static;
    class function Prisma: TVkApplicationData; static;
    class function Clever: TVkApplicationData; static;
    class function CleverGame: TVkApplicationData; static;
    class function Instagram: TVkApplicationData; static;
    class function KateMobile: TVkApplicationData; static;
    class function VFeed: TVkApplicationData; static;
    class function VKME: TVkApplicationData; static;
    class function VKAPI: TVkApplicationData; static;
  end;

implementation

{ TVkApplicationData }

class function TVkApplicationData.Android: TVkApplicationData;
begin
  Result.AppId := '2274003';
  Result.AppKey := 'hHbZxrka2uZ6jB1inYsH';
end;

class function TVkApplicationData.IPad: TVkApplicationData;
begin
  Result.AppId := '3682744';
  Result.AppKey := 'mY6CDUswIVdJLCD3j15n';
end;

class function TVkApplicationData.IPhone: TVkApplicationData;
begin
  Result.AppId := '3140623';
  Result.AppKey := 'VeWdmVclDCtn6ihuP1nt';
end;

class function TVkApplicationData.VKAdmin: TVkApplicationData;
begin
  Result.AppId := '6121396';
  Result.AppKey := '';
end;

class function TVkApplicationData.VKAdminIOS: TVkApplicationData;
begin
  Result.AppId := '5776857';
  Result.AppKey := '';
end;

class function TVkApplicationData.VKLive: TVkApplicationData;
begin
  Result.AppId := '5256902';
  Result.AppKey := '';
end;

class function TVkApplicationData.VKLiveAndroid: TVkApplicationData;
begin
  Result.AppId := '5676187';
  Result.AppKey := '';
end;

class function TVkApplicationData.Vinci: TVkApplicationData;
begin
  Result.AppId := '5554806';
  Result.AppKey := '';
end;

class function TVkApplicationData.Prisma: TVkApplicationData;
begin
  Result.AppId := '5530956';
  Result.AppKey := '';
end;

class function TVkApplicationData.Clever: TVkApplicationData;
begin
  Result.AppId := '6334949';
  Result.AppKey := '';
end;

class function TVkApplicationData.CleverGame: TVkApplicationData;
begin
  Result.AppId := '6378721';
  Result.AppKey := '';
end;

class function TVkApplicationData.Instagram: TVkApplicationData;
begin
  Result.AppId := '3698024';
  Result.AppKey := '';
end;

class function TVkApplicationData.KateMobile: TVkApplicationData;
begin
  Result.AppId := '2685278';
  Result.AppKey := '';
end;

class function TVkApplicationData.VFeed: TVkApplicationData;
begin
  Result.AppId := '4083558';
  Result.AppKey := '';
end;

class function TVkApplicationData.VKME: TVkApplicationData;
begin
  Result.AppId := '6146827';
  Result.AppKey := '';
end;

class function TVkApplicationData.VKAPI: TVkApplicationData;
begin
  Result.AppId := '3116505';
  Result.AppKey := '';
end;

class function TVkApplicationData.WindowsDesktop: TVkApplicationData;
begin
  Result.AppId := '3697615';
  Result.AppKey := 'AlVXZFMUqyrnABp8ncuU';
end;

class function TVkApplicationData.WindowsPhone: TVkApplicationData;
begin
  Result.AppId := '3502557';
  Result.AppKey := 'PEObAuQi6KloPM4T30DV';
end;

end.

