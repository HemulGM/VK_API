unit VK.Utils;

interface

{$INCLUDE include.inc}

uses
  System.Classes, System.Net.HttpClient;

function DownloadURL(URL: string): TMemoryStream;

function GetRandomId: Int64;

function BoolToString(Value: Boolean): string; overload;

function BoolToString(Value: Boolean; TrueValue, FalseValue: string): string; overload;

implementation

uses
  System.DateUtils, System.SysUtils;

function BoolToString(Value: Boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

function BoolToString(Value: Boolean; TrueValue, FalseValue: string): string;
begin
  if Value then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function GetRandomId: Int64;
begin

  {$IFDEF OLD_VERSION}
  Result := DateTimeToUnix(Now) + 1234567;
  {$ELSE}
  Result := DateTimeToMilliseconds(Now) + 1234567;
  {$ENDIF}

end;

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

