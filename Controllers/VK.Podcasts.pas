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
    /// <summary>
    /// SearchPodcast
    /// </summary>
    function SearchPodcast(var Items: TVkPodcastSearch; SearchString: string; Offset: Integer = 0; Count: Integer = 10): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPodcastsController }

function TPodcastsController.ClearRecentSearches: Boolean;
begin
  Result := Handler.Execute('podcasts.clearRecentSearches').ResponseIsTrue;
end;

function TPodcastsController.GetPopular(var Items: TVkPodcasts): Boolean;
begin
  Result := Handler.Execute('podcasts.getPopular').GetObject(Items);
end;

function TPodcastsController.GetRecentSearchRequests(var Items: TVkPodcasts): Boolean;
begin
  Result := Handler.Execute('podcasts.getRecentSearchRequests').GetObjects(Items);
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
  Result := Handler.Execute('podcasts.search', Params).GetObject(Items);
end;

function TPodcastsController.SearchPodcast(var Items: TVkPodcastSearch; SearchString: string; Offset, Count: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('search_string', SearchString);
  if Offset <> 0 then
    Params.Add('offset', Offset);
  if Count <> 0 then
    Params.Add('count', Count);
  Result := Handler.Execute('podcasts.searchPodcast', Params).GetObject(Items);
end;

end.

