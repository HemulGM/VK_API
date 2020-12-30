unit VK.Podcasts;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Podcast;

type
  TPodcastsController = class(TVkController)
  public
    /// <summary>
    /// ClearRecentSearches
    /// </summary>
    function ClearRecentSearches: Boolean;
    /// <summary>
    /// GetPopular
    /// </summary>
    function GetPopular(var Items: TVkPodcasts): Boolean;
    /// <summary>
    /// GetRecentSearchRequests
    /// </summary>
    function GetRecentSearchRequests(var Items: TVkPodcasts): Boolean;
    /// <summary>
    /// Search
    /// </summary>
    function Search(var Items: TVkPodcastSearch; SearchString: string; Offset: Integer = 0; Count: Integer = 10): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPodcastsController }

function TPodcastsController.ClearRecentSearches: Boolean;
begin
  with Handler.Execute('podcasts.clearRecentSearches') do
    Result := Success and ResponseIsTrue;
end;

function TPodcastsController.GetPopular(var Items: TVkPodcasts): Boolean;
begin
  with Handler.Execute('podcasts.getPopular') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPodcasts.FromJsonString(ResponseAsItems);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPodcastsController.GetRecentSearchRequests(var Items: TVkPodcasts): Boolean;
begin
  with Handler.Execute('podcasts.getRecentSearchRequests') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPodcasts.FromJsonString(ResponseAsItems);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPodcastsController.Search(var Items: TVkPodcastSearch; SearchString: string; Offset, Count: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('search_string', SearchString);
  if Offset <> 0 then
    Params.Add('offset', Offset);
  if Count <> 0 then
    Params.Add('count', Count);
  with Handler.Execute('podcasts.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPodcastSearch.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

end.

