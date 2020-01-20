unit VK.Entity.Page;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkPage = class
  private
    FCreated: Extended;
    FEdited: Extended;
    FGroup_id: Extended;
    FId: Extended;
    FParent2: string;
    FTitle: string;
    FView_url: string;
    FViews: integer;
    FWho_can_edit: Extended;
    FWho_can_view: Extended;
    FCreator_id: Extended;
    FCurrent_user_can_edit: Integer;
    FCurrent_user_can_edit_access: Integer;
    FEditor_id: Extended;
    FParent: string;
    FSource: string;
    FHtml: string;
  public
    property id: Extended read FId write FId;
    property group_id: Extended read FGroup_id write FGroup_id;
    property creator_id: Extended read FCreator_id write FCreator_id;
    property title: string read FTitle write FTitle;
    property current_user_can_edit: Integer read FCurrent_user_can_edit write FCurrent_user_can_edit;
    property current_user_can_edit_access: Integer read FCurrent_user_can_edit_access write
      FCurrent_user_can_edit_access;
    property who_can_view: Extended read FWho_can_view write FWho_can_view;
    property who_can_edit: Extended read FWho_can_edit write FWho_can_edit;
    property edited: Extended read FEdited write FEdited;
    property created: Extended read FCreated write FCreated;
    property editor_id: Extended read FEditor_id write FEditor_id;
    property views: Integer read FViews write FViews;
    property parent: string read FParent write FParent;
    property parent2: string read FParent2 write FParent2;
    property source: string read FSource write FSource;
    property html: string read FHtml write FHtml;
    property view_url: string read FView_url write FView_url;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPage;
  end;

implementation

{TRootClass}

function TVkPage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPage.FromJsonString(AJsonString: string): TVkPage;
begin
  result := TJson.JsonToObject<TVkPage>(AJsonString)
end;

end.

