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
    property Id: Extended read FId write FId;
    property GroupId: Extended read FGroup_id write FGroup_id;
    property CreatorId: Extended read FCreator_id write FCreator_id;
    property Title: string read FTitle write FTitle;
    property CurrentUserCanEdit: Integer read FCurrent_user_can_edit write FCurrent_user_can_edit;
    property CurrentUserCanEditAccess: Integer read FCurrent_user_can_edit_access write FCurrent_user_can_edit_access;
    property WhoCanView: Extended read FWho_can_view write FWho_can_view;
    property WhoCanEdit: Extended read FWho_can_edit write FWho_can_edit;
    property Edited: Extended read FEdited write FEdited;
    property Created: Extended read FCreated write FCreated;
    property EditorId: Extended read FEditor_id write FEditor_id;
    property Views: Integer read FViews write FViews;
    property Parent: string read FParent write FParent;
    property Parent2: string read FParent2 write FParent2;
    property Source: string read FSource write FSource;
    property Html: string read FHtml write FHtml;
    property ViewUrl: string read FView_url write FView_url;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPage;
  end;

implementation

{TVkPage}

function TVkPage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPage.FromJsonString(AJsonString: string): TVkPage;
begin
  result := TJson.JsonToObject<TVkPage>(AJsonString)
end;

end.

