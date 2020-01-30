unit VK.Entity.Link;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo;

type
  TVkLinkAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkAction;
  end;

  TVkLinkButton = class
  private
    FAction: TVkLinkAction;
    FTitle: string;
  public
    property Action: TVkLinkAction read FAction write FAction;
    property Title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkButton;
  end;

  TVkLink = class
  private
    FButton: TVkLinkButton;
    FCaption: string;
    FDescription: string;
    FPhoto: TVkPhoto;
    FTitle: string;
    FUrl: string;
  public
    property Button: TVkLinkButton read FButton write FButton;
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLink;
  end;

implementation

{TVkLinkAction}

function TVkLinkAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkAction.FromJsonString(AJsonString: string): TVkLinkAction;
begin
  result := TJson.JsonToObject<TVkLinkAction>(AJsonString)
end;

{TButtonClass}

constructor TVkLinkButton.Create;
begin
  inherited;
  FAction := TVkLinkAction.Create();
end;

destructor TVkLinkButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

function TVkLinkButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkButton.FromJsonString(AJsonString: string): TVkLinkButton;
begin
  result := TJson.JsonToObject<TVkLinkButton>(AJsonString)
end;

{TLinkClass}

constructor TVkLink.Create;
begin
  inherited;
  FPhoto := TVkPhoto.Create();
  FButton := TVkLinkButton.Create();
end;

destructor TVkLink.Destroy;
begin
  FPhoto.Free;
  FButton.Free;
  inherited;
end;

function TVkLink.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLink.FromJsonString(AJsonString: string): TVkLink;
begin
  result := TJson.JsonToObject<TVkLink>(AJsonString)
end;

end.

