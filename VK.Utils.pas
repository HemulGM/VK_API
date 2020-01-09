unit VK.Utils;

interface

uses
  System.Classes, System.Net.HttpClient;

function DownloadURL(URL: string): TMemoryStream;

implementation

function DownloadURL(URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  HTTP := THTTPClient.Create;
  try
    try
      HTTP.HandleRedirects := True;
      HTTP.Get(URL, Result);
      Result.Position := 0;
    except
      //Ну, ошибка... Поток всё равно создан и ошибки не должно возникнуть,
      //если проверить размер потока перед его использованием
    end;
  finally
    HTTP.Free;
  end;
end;

end.

