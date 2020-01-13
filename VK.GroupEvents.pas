unit VK.GroupEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, REST.Client,
  System.JSON, VK.Types, System.Generics.Collections, VK.LongPollServer, VK.API, VK.Wall.Comment;

type
  TCustomGroupEvents = class(TComponent)
  private
    FLongPollServer: TLongPollServer;
    FVK: TCustomVK;
    FGroupID: Integer;
    FOnWallReplyNew: TOnWallReplyAction;
    FOnWallReplyEdit: TOnWallReplyAction;
    FOnWallReplyRestore: TOnWallReplyAction;
    FOnWallReplyDelete: TOnWallReplyDelete;
    procedure FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
    procedure DoEvent(Sender: TObject; Update: TJSONValue);
    procedure SetVK(const Value: TCustomVK);
    procedure FOnError(Sender: TObject; Code: Integer; Text: string);
    procedure SetGroupID(const Value: Integer);
    procedure DoAudioNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupJoin(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupLeave(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupOfficersEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageAllow(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageDeny(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageReply(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPollVoteNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoUserBlock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoUserUnblock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallRepost(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure SetOnWallReplyNew(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyEdit(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyRestore(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyDelete(const Value: TOnWallReplyDelete);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop;
    function Start: Boolean;
    property VK: TCustomVK read FVK write SetVK;
    property GroupID: Integer read FGroupID write SetGroupID;
    //
    property OnWallReplyNew: TOnWallReplyAction read FOnWallReplyNew write SetOnWallReplyNew;
    property OnWallReplyEdit: TOnWallReplyAction read FOnWallReplyEdit write SetOnWallReplyEdit;
    property OnWallReplyRestore: TOnWallReplyAction read FOnWallReplyRestore write SetOnWallReplyRestore;
    property OnWallReplyDelete: TOnWallReplyDelete read FOnWallReplyDelete write SetOnWallReplyDelete;
  end;

  TGroupEventsItems = TList<TCustomGroupEvents>;

  TCustomGroupEventControl = class(TComponent)
  private
    FReaded: Boolean;
    FItems: TGroupEventsItems;
    FGroups: TStrings;
    FVK: TCustomVK;
    FOnWallReplyNew: TOnWallReplyAction;
    FOnWallReplyEdit: TOnWallReplyAction;
    FOnWallReplyRestore: TOnWallReplyAction;
    FOnWallReplyDelete: TOnWallReplyDelete;
    procedure SetItems(const Value: TGroupEventsItems);
    procedure SetGroups(const Value: TStrings);
    procedure SetVK(const Value: TCustomVK);
    procedure SetOnWallReplyNew(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyEdit(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyRestore(const Value: TOnWallReplyAction);
    procedure SetOnWallReplyDelete(const Value: TOnWallReplyDelete);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    destructor Destroy; override;
    procedure Stop;
    function Start: Boolean;
    procedure FillFromList;
    function Add(GroupId: Integer): TCustomGroupEvents;
    property Items: TGroupEventsItems read FItems write SetItems;
    property VK: TCustomVK read FVK write SetVK;
    property Groups: TStrings read FGroups write SetGroups;
    property OnWallReplyNew: TOnWallReplyAction read FOnWallReplyNew write SetOnWallReplyNew;
    property OnWallReplyEdit: TOnWallReplyAction read FOnWallReplyEdit write SetOnWallReplyEdit;
    property OnWallReplyRestore: TOnWallReplyAction read FOnWallReplyRestore write SetOnWallReplyRestore;
    property OnWallReplyDelete: TOnWallReplyDelete read FOnWallReplyDelete write SetOnWallReplyDelete;
  end;

implementation

uses
  System.DateUtils;

{ TUserEvents }

constructor TCustomGroupEvents.Create(AOwner: TComponent);
begin
  inherited;
  FLongPollServer := TLongPollServer.Create;
  FLongPollServer.OnUpdate := FOnLongPollUpdate;
  FLongPollServer.OnError := FOnError;
end;

destructor TCustomGroupEvents.Destroy;
begin
  FLongPollServer.Free;
  inherited;
end;

procedure TCustomGroupEvents.DoEvent(Sender: TObject; Update: TJSONValue);
var
  GroupId: Integer;
  EventType: string;
  EventId: string;
  EventObject: TJSONValue;

  function NormalizePeerId(Value: Integer): Integer;
  begin
    if Value > 2000000000 then
      Exit(Value - 2000000000);
    if Value > 1000000000 then
      Exit(-(Value - 1000000000));
    Result := Value;
  end;

begin
  EventType := Update.GetValue<string>('type', '');
  EventObject := Update.GetValue<TJSONValue>('object', nil);
  GroupId := Update.GetValue<Integer>('group_id', 0);
  EventId := Update.GetValue<string>('event_id', '');

  if EventType = 'message_new' then
    DoMessageNew(GroupId, EventObject, EventId)
  else if (EventType = 'message_reply') then
    DoMessageReply(GroupId, EventObject, EventId)
  else if (EventType = 'message_edit') then
    DoMessageEdit(GroupId, EventObject, EventId)
  else if EventType = 'message_allow' then
    DoMessageAllow(GroupId, EventObject, EventId)
  else if EventType = 'message_deny' then
    DoMessageDeny(GroupId, EventObject, EventId)
  else if EventType = 'photo_new' then
    DoPhotoNew(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_new' then
    DoPhotoCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_edit' then
    DoPhotoCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_restore' then
    DoPhotoCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_delete' then
    DoPhotoCommentDelete(GroupId, EventObject, EventId)
  else if EventType = 'audio_new' then
    DoAudioNew(GroupId, EventObject, EventId)
  else if EventType = 'video_new' then
    DoVideoNew(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_new' then
    DoVideoCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_edit' then
    DoVideoCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_restore' then
    DoVideoCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'wall_post_new' then
    DoWallPostNew(GroupId, EventObject, EventId)
  else if EventType = 'wall_repost' then
    DoWallRepost(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_new' then
    DoWallReplyNew(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_edit' then
    DoWallReplyEdit(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_restore' then
    DoWallReplyRestore(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_delete' then
    DoWallReplyDelete(GroupId, EventObject, EventId)
  else if EventType = 'board_post_new' then
    DoBoardPostNew(GroupId, EventObject, EventId)
  else if EventType = 'board_post_edit' then
    DoBoardPostEdit(GroupId, EventObject, EventId)
  else if EventType = 'board_post_restore' then
    DoBoardPostRestore(GroupId, EventObject, EventId)
  else if EventType = 'board_post_delete' then
    DoBoardPostDelete(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_new' then
    DoMarketCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_edit' then
    DoMarketCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_restore' then
    DoMarketCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_delete' then
    DoMarketCommentDelete(GroupId, EventObject, EventId)
  else if EventType = 'group_leave' then
    DoGroupLeave(GroupId, EventObject, EventId)
  else if EventType = 'group_join' then
    DoGroupJoin(GroupId, EventObject, EventId)
  else if EventType = 'group_officers_edit' then
    DoGroupOfficersEdit(GroupId, EventObject, EventId)
  else if EventType = 'user_block' then
    DoUserBlock(GroupId, EventObject, EventId)
  else if EventType = 'user_unblock' then
    DoUserUnblock(GroupId, EventObject, EventId)
  else if EventType = 'poll_vote_new' then
    DoPollVoteNew(GroupId, EventObject, EventId);
end;

procedure TCustomGroupEvents.DoAudioNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoBoardPostDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoBoardPostEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoBoardPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoBoardPostRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoGroupJoin(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoGroupLeave(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoGroupOfficersEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMarketCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMarketCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMarketCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMarketCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMessageAllow(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMessageDeny(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMessageEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMessageNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMessageReply(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPhotoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPhotoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPhotoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPhotoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPhotoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoPollVoteNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoUserBlock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoUserUnblock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoVideoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoVideoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoVideoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoVideoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoWallPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoWallReplyDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TWallCommentDeleted;
begin
  if Assigned(FOnWallReplyDelete) then
  begin
    Comment := TWallCommentDeleted.FromJsonString(EventObject.ToString);
    FOnWallReplyDelete(Self, GroupId, Comment, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TWallComment;
begin
  if Assigned(FOnWallReplyEdit) then
  begin
    Comment := TWallComment.FromJsonString(EventObject.ToString);
    FOnWallReplyEdit(Self, GroupId, Comment, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TWallComment;
begin
  if Assigned(FOnWallReplyNew) then
  begin
    Comment := TWallComment.FromJsonString(EventObject.ToString);
    FOnWallReplyNew(Self, GroupId, Comment, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TWallComment;
begin
  if Assigned(FOnWallReplyRestore) then
  begin
    Comment := TWallComment.FromJsonString(EventObject.ToString);
    FOnWallReplyRestore(Self, GroupId, Comment, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallRepost(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.FOnError(Sender: TObject; Code: Integer; Text: string);
begin
  FVK.DoLog(Self, Text);
end;

procedure TCustomGroupEvents.FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
begin
  FVK.DoLog(Self, Update.ToString);
  DoEvent(Sender, Update);
end;

procedure TCustomGroupEvents.SetGroupID(const Value: Integer);
begin
  if Value > 0 then
    raise Exception.Create('Идентификатор группы может быть только отрицательным цислом');
  FGroupID := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyDelete(const Value: TOnWallReplyDelete);
begin
  FOnWallReplyDelete := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyEdit(const Value: TOnWallReplyAction);
begin
  FOnWallReplyEdit := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyNew(const Value: TOnWallReplyAction);
begin
  FOnWallReplyNew := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyRestore(const Value: TOnWallReplyAction);
begin
  FOnWallReplyRestore := Value;
end;

procedure TCustomGroupEvents.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

function TCustomGroupEvents.Start: Boolean;
begin
  if not Assigned(FVK) then
    raise Exception.Create('Для работы необходим VK контроллер (Свойство VK)');
  if FGroupID >= 0 then
    raise Exception.Create('Идентификатор группы не указан или указан не верно');
  FLongPollServer.Client := FVK.Handler.RESTClient;
  FLongPollServer.Method := 'groups.getLongPollServer';
  FLongPollServer.Params := [['lp_version', '3'], ['group_id', (FGroupID *  - 1).ToString]];
  Result := FLongPollServer.Start;
  if Result then
    FVK.DoLog(FLongPollServer, FGroupID.ToString + ' started')
  else
    FVK.DoLog(FLongPollServer, FGroupID.ToString + ' not start');
end;

procedure TCustomGroupEvents.Stop;
begin
  FLongPollServer.Stop;
end;

{ TCustomGroupEventControl }

function TCustomGroupEventControl.Add(GroupId: Integer): TCustomGroupEvents;
begin
  Result := TCustomGroupEvents.Create(Self);
  FItems.Add(Result);
  Result.GroupID := GroupId;
  Result.VK := FVK;
  Result.OnWallReplyNew := FOnWallReplyNew;
  Result.OnWallReplyEdit := FOnWallReplyEdit;
  Result.OnWallReplyRestore := FOnWallReplyRestore;
  Result.OnWallReplyDelete := FOnWallReplyDelete;
end;

constructor TCustomGroupEventControl.Create(AOwner: TComponent);
begin
  inherited;
  FReaded := False;
  FItems := TGroupEventsItems.Create;
  FGroups := TStringList.Create;
end;

procedure TCustomGroupEventControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  if (FGroups.Count > 0) and (not FReaded) then
  begin
    FillFromList;
    FReaded := True;
  end;
end;

destructor TCustomGroupEventControl.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    FItems[i].Free;
  end;
  FItems.Free;
  FGroups.Free;
  inherited;
end;

procedure TCustomGroupEventControl.FillFromList;
var
  i, Id: Integer;
begin
  for i := 0 to FGroups.Count - 1 do
    if TryStrToInt(FGroups[i], Id) then
      if Id < 0 then
        Add(Id);
end;

procedure TCustomGroupEventControl.SetGroups(const Value: TStrings);
begin
  FGroups.Assign(Value);
end;

procedure TCustomGroupEventControl.SetItems(const Value: TGroupEventsItems);
begin
  FItems := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyDelete(const Value: TOnWallReplyDelete);
begin
  FOnWallReplyDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyEdit(const Value: TOnWallReplyAction);
begin
  FOnWallReplyEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyNew(const Value: TOnWallReplyAction);
begin
  FOnWallReplyNew := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyRestore(const Value: TOnWallReplyAction);
begin
  FOnWallReplyRestore := Value;
end;

procedure TCustomGroupEventControl.SetVK(const Value: TCustomVK);
var
  i: Integer;
begin
  FVK := Value;
  for i := 0 to FItems.Count - 1 do
    FItems[i].VK := FVK;
end;

function TCustomGroupEventControl.Start: Boolean;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    Result := FItems[i].Start;
end;

procedure TCustomGroupEventControl.Stop;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Stop;
end;

end.

