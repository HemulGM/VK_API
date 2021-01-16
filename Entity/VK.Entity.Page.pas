unit VK.Entity.Page;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkPage = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEdited: TDateTime;
    FGroup_id: Integer;
    FParent2: string;
    FTitle: string;
    FView_url: string;
    FViews: integer;
    FWho_can_edit: Integer;
    FWho_can_view: Integer;
    FCreator_id: Integer;
    FCurrent_user_can_edit: Integer;
    FCurrent_user_can_edit_access: Integer;
    FEditor_id: Integer;
    FParent: string;
    FSource: string;
    FHtml: string;
  public
    property GroupId: Integer read FGroup_id write FGroup_id;
    property CreatorId: Integer read FCreator_id write FCreator_id;
    property Title: string read FTitle write FTitle;
    property CurrentUserCanEdit: Integer read FCurrent_user_can_edit write FCurrent_user_can_edit;
    property CurrentUserCanEditAccess: Integer read FCurrent_user_can_edit_access write FCurrent_user_can_edit_access;
    property WhoCanView: Integer read FWho_can_view write FWho_can_view;
    property WhoCanEdit: Integer read FWho_can_edit write FWho_can_edit;
    property Edited: TDateTime read FEdited write FEdited;
    property Created: TDateTime read FCreated write FCreated;
    property EditorId: Integer read FEditor_id write FEditor_id;
    property Views: Integer read FViews write FViews;
    property Parent: string read FParent write FParent;
    property Parent2: string read FParent2 write FParent2;
    property Source: string read FSource write FSource;
    property Html: string read FHtml write FHtml;
    property ViewUrl: string read FView_url write FView_url;
  end;

  TVkPages = TVkEntityList<TVkPage>;

  TVkPageVersion = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FEditor_id: Integer;
    FEditor_name: string;
    FLength: Integer;
  public
    property Date: TDateTime read FDate write FDate;
    property EditorId: Integer read FEditor_id write FEditor_id;
    property EditorName: string read FEditor_name write FEditor_name;
    property Length: Integer read FLength write FLength;
  end;

  TVkPageVersions = TVkEntityList<TVkPageVersion>;

implementation

end.

