unit VK.Fave;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Fave;

type
  TVkFaveTypeGet = (ftPost, ftVideo, ftProduct, ftArticle, ftLink);

  TVkFaveTypeGetHelper = record helper for TVkFaveTypeGet
    function ToString: string; inline;
  end;

  TVkParamsFaveGet = record
    List: TParams;
    function TagId(Value: Integer): Integer;
    function ItemType(Value: TVkFaveTypeGet): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(Value: TArrayOfString): Integer;
    function IsFromSnackbar(Value: Boolean): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  TFaveController = class(TVkController)
  public
    function Get(var Faves: TVkFaves; Params: TParams): Boolean; overload;
    function Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TFaveController }

function TFaveController.Get(var Faves: TVkFaves; Params: TParams): Boolean;
begin
  with Handler.Execute('fave.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Faves := TVkFaves.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFaveController.Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean;
begin
  Result := Get(Faves, Params.List);
end;

{ TVkGetFaveParams }

function TVkParamsFaveGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFaveGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsFaveGet.Fields(Value: TArrayOfString): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFaveGet.IsFromSnackbar(Value: Boolean): Integer;
begin
  Result := List.Add('is_from_snackbar', Value);
end;

function TVkParamsFaveGet.ItemType(Value: TVkFaveTypeGet): Integer;
begin
  Result := List.Add('item_type', Value.ToString);
end;

function TVkParamsFaveGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFaveGet.TagId(Value: Integer): Integer;
begin
  Result := List.Add('tag_id', Value);
end;

{ TVkFaveTypeGetHelper }

function TVkFaveTypeGetHelper.ToString: string;
begin
  case Self of
    ftPost:
      Result := 'post';
    ftVideo:
      Result := 'video';
    ftProduct:
      Result := 'product';
    ftArticle:
      Result := 'article';
    ftLink:
      Result := 'link';
  end;
end;

end.

