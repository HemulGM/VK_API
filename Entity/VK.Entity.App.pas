unit VK.Entity.App;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkApp = class
  private
    FAuthor_owner_id: Extended;
    FAuthor_url: string;
    FBanner_1120: string;
    FBanner_560: string;
    FGenre: string;
    FGenre_id: Extended;
    FHide_tabbar: Extended;
    FIcon_139: string;
    FIcon_150: string;
    FIcon_278: string;
    FIcon_75: string;
    FId: Extended;
    FInternational: Boolean;
    FIs_in_catalog: Extended;
    FIs_installed: Boolean;
    FLeaderboard_type: Extended;
    FMembers_count: Extended;
    FMobile_controls_type: Extended;
    FMobile_view_support_type: Extended;
    FPublished_date: Extended;
    FSection: string;
    FTitle: string;
    FType: string;
  public
    property author_owner_id: Extended read FAuthor_owner_id write FAuthor_owner_id;
    property author_url: string read FAuthor_url write FAuthor_url;
    property banner_1120: string read FBanner_1120 write FBanner_1120;
    property banner_560: string read FBanner_560 write FBanner_560;
    property genre: string read FGenre write FGenre;
    property genre_id: Extended read FGenre_id write FGenre_id;
    property hide_tabbar: Extended read FHide_tabbar write FHide_tabbar;
    property icon_139: string read FIcon_139 write FIcon_139;
    property icon_150: string read FIcon_150 write FIcon_150;
    property icon_278: string read FIcon_278 write FIcon_278;
    property icon_75: string read FIcon_75 write FIcon_75;
    property id: Extended read FId write FId;
    property international: Boolean read FInternational write FInternational;
    property is_in_catalog: Extended read FIs_in_catalog write FIs_in_catalog;
    property is_installed: Boolean read FIs_installed write FIs_installed;
    property leaderboard_type: Extended read FLeaderboard_type write FLeaderboard_type;
    property members_count: Extended read FMembers_count write FMembers_count;
    property mobile_controls_type: Extended read FMobile_controls_type write FMobile_controls_type;
    property mobile_view_support_type: Extended read FMobile_view_support_type write FMobile_view_support_type;
    property published_date: Extended read FPublished_date write FPublished_date;
    property section: string read FSection write FSection;
    property title: string read FTitle write FTitle;
    property&type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkApp;
  end;

implementation

{TRootClass}

function TVkApp.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkApp.FromJsonString(AJsonString: string): TVkApp;
begin
  result := TJson.JsonToObject<TVkApp>(AJsonString)
end;

end.

