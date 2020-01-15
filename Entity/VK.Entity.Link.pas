unit VK.Entity.Link;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo;

type
  TVkAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&type: string read FType write FType;
    property url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAction;
  end;

  TVkButton = class
  private
    FAction: TVkAction;
    FTitle: string;
  public
    property action: TVkAction read FAction write FAction;
    property title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkButton;
  end;

  TVkLink = class
  private
    FButton: TVkButton;
    FCaption: string;
    FDescription: string;
    FPhoto: TVkPhoto;
    FTitle: string;
    FUrl: string;
  public
    property button: TVkButton read FButton write FButton;
    property caption: string read FCaption write FCaption;
    property description: string read FDescription write FDescription;
    property photo: TVkPhoto read FPhoto write FPhoto;
    property title: string read FTitle write FTitle;
    property url: string read FUrl write FUrl;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLink;
  end;

implementation

{TActionClass}

function TVkAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAction.FromJsonString(AJsonString: string): TVkAction;
begin
  result := TJson.JsonToObject<TVkAction>(AJsonString)
end;

{TButtonClass}

constructor TVkButton.Create;
begin
  inherited;
  FAction := TVkAction.Create();
end;

destructor TVkButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

function TVkButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkButton.FromJsonString(AJsonString: string): TVkButton;
begin
  result := TJson.JsonToObject<TVkButton>(AJsonString)
end;

{TLinkClass}

constructor TVkLink.Create;
begin
  inherited;
  FPhoto := TVkPhoto.Create();
  FButton := TVkButton.Create();
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

