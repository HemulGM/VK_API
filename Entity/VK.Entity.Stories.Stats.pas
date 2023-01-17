unit VK.Entity.Stories.Stats;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkStoryStatCounter = class(TVkCounterEntity)
  private
    FState: string;
  public
    /// <summary>
    /// on — доступно, off — недоступно, hidden — недоступно
    /// </summary>
    property State: string read FState write FState;
  end;

  TVkStoryStat = class(TVkEntity)
  private
    FAnswer: TVkStoryStatCounter;
    FBans: TVkStoryStatCounter;
    FLikes: TVkStoryStatCounter;
    FOpen_link: TVkStoryStatCounter;
    FReplies: TVkStoryStatCounter;
    FShares: TVkStoryStatCounter;
    FSubscribers: TVkStoryStatCounter;
    FViews: TVkStoryStatCounter;
  public
    property Answer: TVkStoryStatCounter read FAnswer write FAnswer;
    property Bans: TVkStoryStatCounter read FBans write FBans;
    property Likes: TVkStoryStatCounter read FLikes write FLikes;
    property OpenLink: TVkStoryStatCounter read FOpen_link write FOpen_link;
    property Replies: TVkStoryStatCounter read FReplies write FReplies;
    property Shares: TVkStoryStatCounter read FShares write FShares;
    property Subscribers: TVkStoryStatCounter read FSubscribers write FSubscribers;
    property Views: TVkStoryStatCounter read FViews write FViews;
    destructor Destroy; override;
  end;

implementation

{TVkStoryStat}

destructor TVkStoryStat.Destroy;
begin
  if Assigned(FAnswer) then
    FAnswer.Free;
  if Assigned(FBans) then
    FBans.Free;
  if Assigned(FOpen_link) then
    FOpen_link.Free;
  if Assigned(FReplies) then
    FReplies.Free;
  if Assigned(FShares) then
    FShares.Free;
  if Assigned(FSubscribers) then
    FSubscribers.Free;
  if Assigned(FViews) then
    FViews.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  inherited;
end;

end.

