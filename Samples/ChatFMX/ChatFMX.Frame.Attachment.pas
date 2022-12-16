unit ChatFMX.Frame.Attachment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VK.API;

type
  TFrameAttachment = class(TFrame)
  private
    FVK: TCustomVK;
    FId: Int64;
    procedure SetVK(const Value: TCustomVK);
    procedure SetId(const Value: Int64);

  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce; virtual;
    property VK: TCustomVK read FVK write SetVK;
    procedure SetVisibility(const Value: Boolean); virtual;
    property Id: Int64 read FId write SetId;
  end;

implementation

{$R *.fmx}

{ TFrameAttachment }

constructor TFrameAttachment.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

procedure TFrameAttachment.SetId(const Value: Int64);
begin
  FId := Value;
end;

procedure TFrameAttachment.SetVisibility(const Value: Boolean);
begin
  //
end;

procedure TFrameAttachment.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

end.

