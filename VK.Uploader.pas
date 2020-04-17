unit VK.Uploader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, REST.Client, System.JSON,
  System.Net.HttpClient, VK.Types, System.Net.Mime, VK.Entity.Photo.Upload, VK.Entity.Audio.Upload;

type
  TUploader = class
    function Upload(UploadUrl: string; FileName: string; var Response: string): Boolean; overload;
    function UploadPhotos(UploadUrl, FileName: string; var Response: TVkPhotoUploadResponse): Boolean; overload;
    function UploadAudio(UploadUrl, FileName: string; var Response: TVkAudioUploadResponse): Boolean; overload;
    function UploadPhotos(UploadUrl: string; Stream: TStream; FileName: string; var Response: TVkPhotoUploadResponse):
      Boolean; overload;
  end;

implementation

{ TUploader }

function TUploader.Upload(UploadUrl, FileName: string; var Response: string): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
  JSON: TJSONValue;
begin
  Result := False;
  if not FileName.IsEmpty then
    if not FileExists(FileName) then
      Exit;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    if not FileName.IsEmpty then
      Data.AddFile('file', FileName);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        JSON := TJSONObject.ParseJSONValue(ResStream.DataString);
        try
          Response := JSON.GetValue<string>('file');
        finally
          JSON.Free;
        end;
      except
        Response := '';
      end;
      Result := not Response.IsEmpty;
      if not Result then
        Response := ResStream.DataString;
    end
    else
      Result := False;
  finally
    Data.Free;
    HTTP.Free;
    ResStream.Free;
  end;
end;

function TUploader.UploadAudio(UploadUrl, FileName: string; var Response: TVkAudioUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddFile('file', FileName);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        Response := TVkAudioUploadResponse.FromJsonString(ResStream.DataString);
        Result := True;
      except
        Response := nil;
        Result := False;
      end;
    end
    else
      Result := False;
  finally
    Data.Free;
    HTTP.Free;
    ResStream.Free;
  end;
end;

function TUploader.UploadPhotos(UploadUrl, FileName: string; var Response: TVkPhotoUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddFile('file', FileName);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        Response := TVkPhotoUploadResponse.FromJsonString(ResStream.DataString);
        Result := True;
      except
        Response := nil;
        Result := False;
      end;
    end
    else
      Result := False;
  finally
    Data.Free;
    HTTP.Free;
    ResStream.Free;
  end;
end;

function TUploader.UploadPhotos(UploadUrl: string; Stream: TStream; FileName: string; var Response:
  TVkPhotoUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddStream('file', Stream, ExtractFileName(FileName));
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        Response := TVkPhotoUploadResponse.FromJsonString(ResStream.DataString);
        Result := True;
      except
        Response := nil;
        Result := False;
      end;
    end
    else
      Result := False;
  finally
    Data.Free;
    HTTP.Free;
    ResStream.Free;
  end;
end;

end.

