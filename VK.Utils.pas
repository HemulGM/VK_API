unit VK.Utils;

interface

uses
  System.Classes, System.Net.HttpClient;

function DownloadURL(URL: string): TMemoryStream;

function GetRandomId: Int64;

implementation

uses
  System.DateUtils, System.SysUtils;

function GetRandomId: Int64;
begin
  Result := DateTimeToMilliseconds(Now + 1234567654321);
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
      //��, ������... ����� �� ����� ������ � ������ �� ������ ����������,
      //���� ��������� ������ ������ ����� ��� ��������������
    end;
  finally
    HTTP.Free;
  end;
end;

end.

