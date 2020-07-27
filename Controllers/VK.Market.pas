unit VK.Market;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Market;

type
  TVkParamsMarketGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TMarketController = class(TVkController)
  public
    function Get(var Products: TVkProducts; Params: TParams): Boolean; overload;
    function Get(var Products: TVkProducts; Params: TVkParamsMarketGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMarketController }

function TMarketController.Get(var Products: TVkProducts; Params: TParams): Boolean;
begin
  with Handler.Execute('market.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Products := TVkProducts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
   { if (not Result) and GenerateException then
      raise TVkMethodException.Create(Error.Text, Error.Code);     }
  end;
end;

function TMarketController.Get(var Products: TVkProducts; Params: TVkParamsMarketGet): Boolean;
begin
  Result := Get(Products, Params.List);
end;

{ TVkParamsMarketGet }

function TVkParamsMarketGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

end.

