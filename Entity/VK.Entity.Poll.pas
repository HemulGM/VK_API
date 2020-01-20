unit VK.Entity.Poll;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo;

type
  TVkPollFriends = class
  private
    FId: Extended;
  public
    property id: Extended read FId write FId;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPollFriends;
  end;

  TVkPollPoints = class
  private
    FColor: string;
    FPosition: Extended;
  public
    property color: string read FColor write FColor;
    property position: Extended read FPosition write FPosition;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPollPoints;
  end;

  TVkPollBackground = class
  private
    FAngle: Extended;
    FColor: string;
    FId: Extended;
    FName: string;
    FPoints: TArray<TVkPollPoints>;
    FType: string;
  public
    property angle: Extended read FAngle write FAngle;
    property color: string read FColor write FColor;
    property id: Extended read FId write FId;
    property name: string read FName write FName;
    property points: TArray<TVkPollPoints> read FPoints write FPoints;
    property&type: string read FType write FType;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPollBackground;
  end;

  TVkPollAnswer = class
  private
    FId: Extended;
    FRate: Extended;
    FText: string;
    FVotes: Extended;
  public
    property id: Extended read FId write FId;
    property rate: Extended read FRate write FRate;
    property text: string read FText write FText;
    property votes: Extended read FVotes write FVotes;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPollAnswer;
  end;

  TVkPoll = class
  private
    FAnonymous: Boolean;
    FAnswer_ids: TArray<Extended>;
    FAnswers: TArray<TVkPollAnswer>;
    FAuthor_id: Extended;
    FBackground: TVkPollBackground;
    FCan_edit: Boolean;
    FCan_report: Boolean;
    FCan_share: Boolean;
    FCan_vote: Boolean;
    FClosed: Boolean;
    FCreated: Extended;
    FEnd_date: Extended;
    FId: Extended;
    FIs_board: Boolean;
    FMultiple: Boolean;
    FOwner_id: Extended;
    FQuestion: string;
    FVotes: Extended;
    FPhoto: TVkPhoto;
    FFriends: TArray<TVkPollFriends>;
  public
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property created: Extended read FCreated write FCreated;
    property question: string read FQuestion write FQuestion;
    property votes: Extended read FVotes write FVotes;
    property answers: TArray<TVkPollAnswer> read FAnswers write FAnswers;
    property anonymous: Boolean read FAnonymous write FAnonymous;
    property multiple: Boolean read FMultiple write FMultiple;
    property answer_ids: TArray<Extended> read FAnswer_ids write FAnswer_ids;
    property end_date: Extended read FEnd_date write FEnd_date;
    property closed: Boolean read FClosed write FClosed;
    property is_board: Boolean read FIs_board write FIs_board;
    property can_edit: Boolean read FCan_edit write FCan_edit;
    property can_vote: Boolean read FCan_vote write FCan_vote;
    property can_report: Boolean read FCan_report write FCan_report;
    property can_share: Boolean read FCan_share write FCan_share;
    property author_id: Extended read FAuthor_id write FAuthor_id;
    property photo: TVkPhoto read FPhoto write FPhoto;
    property background: TVkPollBackground read FBackground write FBackground;
    property friends: TArray<TVkPollFriends> read FFriends write FFriends;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPoll;
  end;

implementation

{TPointsClass}

function TVkPollPoints.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPollPoints.FromJsonString(AJsonString: string): TVkPollPoints;
begin
  result := TJson.JsonToObject<TVkPollPoints>(AJsonString)
end;

{TBackgroundClass}

destructor TVkPollBackground.Destroy;
var
  LpointsItem: TVkPollPoints;
begin

  for LpointsItem in FPoints do
    LpointsItem.Free;

  inherited;
end;

function TVkPollBackground.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPollBackground.FromJsonString(AJsonString: string): TVkPollBackground;
begin
  result := TJson.JsonToObject<TVkPollBackground>(AJsonString)
end;

{TAnswersClass}

function TVkPollAnswer.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPollAnswer.FromJsonString(AJsonString: string): TVkPollAnswer;
begin
  result := TJson.JsonToObject<TVkPollAnswer>(AJsonString)
end;

{TRootClass}

constructor TVkPoll.Create;
begin
  inherited;
  FBackground := TVkPollBackground.Create();
end;

destructor TVkPoll.Destroy;
var
  LanswersItem: TVkPollAnswer;
  LfriendsItem: TVkPollFriends;
begin

  for LanswersItem in FAnswers do
    LanswersItem.Free;
  for LfriendsItem in FFriends do
    LfriendsItem.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  FBackground.Free;
  inherited;
end;

function TVkPoll.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPoll.FromJsonString(AJsonString: string): TVkPoll;
begin
  result := TJson.JsonToObject<TVkPoll>(AJsonString)
end;

{ TFriendsClass }

function TVkPollFriends.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPollFriends.FromJsonString(AJsonString: string): TVkPollFriends;
begin
  result := TJson.JsonToObject<TVkPollFriends>(AJsonString)
end;

end.

