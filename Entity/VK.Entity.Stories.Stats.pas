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
    /// on Ч доступно, off Ч недоступно, hidden Ч недоступно
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
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{TRootClass}

constructor TVkStoryStat.Create;
begin
  inherited;
  FAnswer := TVkStoryStatCounter.Create();
  FBans := TVkStoryStatCounter.Create();
  FOpen_link := TVkStoryStatCounter.Create();
  FReplies := TVkStoryStatCounter.Create();
  FShares := TVkStoryStatCounter.Create();
  FSubscribers := TVkStoryStatCounter.Create();
  FViews := TVkStoryStatCounter.Create();
  FLikes := TVkStoryStatCounter.Create();
end;

destructor TVkStoryStat.Destroy;
begin
  FAnswer.Free;
  FBans.Free;
  FOpen_link.Free;
  FReplies.Free;
  FShares.Free;
  FSubscribers.Free;
  FViews.Free;
  FLikes.Free;
  inherited;
end;

end.

