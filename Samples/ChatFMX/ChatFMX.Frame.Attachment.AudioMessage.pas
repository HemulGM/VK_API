unit ChatFMX.Frame.Attachment.AudioMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API,
  VK.Entity.AudioMessage;

type
  TFrameAttachmentAudioMessage = class(TFrame)
    LabelTime: TLabel;
    LayoutControl: TLayout;
    CircleControl: TCircle;
    PathControl: TPath;
    PaintBoxWave: TPaintBox;
    Layout1: TLayout;
    CircleListened: TCircle;
    procedure PaintBoxWavePaint(Sender: TObject; Canvas: TCanvas);
    procedure CircleControlClick(Sender: TObject);
    procedure PaintBoxWaveResize(Sender: TObject);
  private
    FIsPlay: Boolean;
    FVK: TCustomVK;
    FWave: TArray<Integer>;
    FWaveMax: Integer;
    FDuration: Int64;
    FIsPause: Boolean;
    FDurationPlayed: Int64;
    FDrawWave: TArray<Single>;
    FWasListened: Boolean;
    procedure SetIsPlay(const Value: Boolean);
    procedure SetWave(const Value: TArray<Integer>);
    procedure SetDuration(const Value: Int64);
    procedure SetIsPause(const Value: Boolean);
    procedure SetDurationPlayed(const Value: Int64);
    procedure UpdateTime;
    procedure CalcWave;
    procedure SetWasListened(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(AudioMessage: TVkAudioMessage; Listened: Boolean);
    property IsPlay: Boolean read FIsPlay write SetIsPlay;
    property IsPause: Boolean read FIsPause write SetIsPause;
    property Wave: TArray<Integer> read FWave write SetWave;
    property Duration: Int64 read FDuration write SetDuration;
    property DurationPlayed: Int64 read FDurationPlayed write SetDurationPlayed;
    property WasListened: Boolean read FWasListened write SetWasListened;
  end;

const
  PlayPath = 'm 2.5 0.5 v 9 l 7 -4.5 Z';
  PausePath = 'M224,435.8V76.1c0-6.7-5.4-12.1-12.2-12.1h-71.6c-6.8,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6   C218.6,448,224,442.6,224,435.8z'#13#10 +
    'M371.8,64h-71.6c-6.7,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6c6.7,0,12.2-5.4,12.2-12.2V76.1   C384,69.4,378.6,64,371.8,64z';

implementation

uses
  System.Math, HGM.Common.DateUtils;

{$R *.fmx}

procedure TFrameAttachmentAudioMessage.CircleControlClick(Sender: TObject);
begin
  IsPlay := not IsPlay;
end;

constructor TFrameAttachmentAudioMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

procedure TFrameAttachmentAudioMessage.Fill(AudioMessage: TVkAudioMessage; Listened: Boolean);
begin
  WasListened := Listened;
  Wave := AudioMessage.WaveForm;
  Duration := AudioMessage.Duration;
  DurationPlayed := Duration div 2;
end;

procedure TFrameAttachmentAudioMessage.PaintBoxWavePaint(Sender: TObject; Canvas: TCanvas);
begin
  if Length(FDrawWave) <= 0 then
    Exit;
  var H := PaintBoxWave.Height;
  Canvas.BeginScene;
  try
    if (DurationPlayed < Duration) and (DurationPlayed <> 0) then
    begin
      Canvas.Fill.Color := $FF577CA1;
      for var i := Low(FDrawWave) to High(FDrawWave) do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;

      var ITo := Trunc((((100 / Duration) * DurationPlayed) / 100) * High(FDrawWave));
      Canvas.Fill.Color := $FF71AAEB;
      for var i := Low(FDrawWave) to ITo do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;
    end
    else
    begin
      Canvas.Fill.Color := $FF71AAEB;
      for var i := Low(FDrawWave) to High(FDrawWave) do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;
    end;
  finally
    Canvas.EndScene;
  end;
end;

procedure LargestTriangleThreeBuckets(const Data: TArray<Integer>; Threshold: Integer; var Output: TArray<Single>);
begin
  var Count := Length(Data);

  if ((Threshold >= Count) or (Threshold = 0)) then
    Exit;

  SetLength(Output, Threshold);

  var OutIndex: Integer := 1;
  var Every: Single :=(Count - 2) / (Threshold - 2);
  var Index: Integer := 0;  // Initially a is the first point in the triangle
  var NextIndex: Integer := 0;
  var MaxAreaPoint: Integer := 0;
  var MaxArea: Single;
  var Area: Single;
  Output[0] := Data[0]; // Always add the first point

  for var i := 0 to Threshold - 2 - 1 do
  begin
    var AvgX: Single := 0;
    var AvgY: Single := 0;
    var AvgRangeStart: Integer := Floor((i + 1) * Every) + 1;
    var AvgRangeEnd: Integer := Floor((i + 2) * Every) + 1;

    if AvgRangeEnd >= Count then
      AvgRangeEnd := Count;

    var AvgRangeLength: Integer := AvgRangeEnd - AvgRangeStart;

    for var m := AvgRangeStart to AvgRangeEnd - 1 do
    begin
      AvgX := AvgX + m;
      AvgY := AvgY + Data[m];
    end;

    AvgX := AvgX / AvgRangeLength;
    AvgY := AvgY / AvgRangeLength;

    // Get the range for this bucket
    var RangeOffs: Integer := Floor((i + 0) * Every) + 1;
    var RangeTo: Integer := Floor((i + 1) * Every) + 1;

    // Point a (Reference data point of previous bucket)
    var PointX: Single := Index;
    var PointY: Single := Data[Index];
    MaxArea := NegInfinity;

    for var n := RangeOffs to RangeTo - 1 do
    begin
      Area := Abs((PointX - AvgX) * (Data[n] - PointY) - (PointX - n) * (AvgY - PointY)) * 0.5;
      if (Area > MaxArea) then
      begin
        MaxArea := Area;
        MaxAreaPoint := Data[n];
        NextIndex := n;
      end;
    end;

    Output[OutIndex] := MaxAreaPoint;
    Inc(OutIndex);
    Index := NextIndex;
  end;

  Output[OutIndex] := Data[Count - 1];
end;

procedure TFrameAttachmentAudioMessage.CalcWave;
begin
  var Cnt := Length(FWave);
  if Cnt <= 0 then
    Exit;

  var NCnt :=(Trunc(PaintBoxWave.Width) + 1) div 3;
  if NCnt <= 0 then
    Exit;

  LargestTriangleThreeBuckets(FWave, NCnt, FDrawWave);
end;

procedure TFrameAttachmentAudioMessage.PaintBoxWaveResize(Sender: TObject);
begin
  CalcWave;
end;

procedure TFrameAttachmentAudioMessage.SetIsPause(const Value: Boolean);
begin
  FIsPause := Value;
end;

procedure TFrameAttachmentAudioMessage.SetIsPlay(const Value: Boolean);
begin
  FIsPlay := Value;
  if FIsPlay then
    PathControl.Data.Data := PausePath
  else
    PathControl.Data.Data := PlayPath;
end;

procedure TFrameAttachmentAudioMessage.SetDuration(const Value: Int64);
begin
  FDuration := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudioMessage.SetDurationPlayed(const Value: Int64);
begin
  FDurationPlayed := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudioMessage.UpdateTime;
begin
  if FDurationPlayed <= 0 then
    LabelTime.Text := SecondsToMinFormat(FDuration)
  else
    LabelTime.Text := SecondsToMinFormat(FDurationPlayed - FDuration);
end;

procedure TFrameAttachmentAudioMessage.SetWasListened(const Value: Boolean);
begin
  FWasListened := Value;
  CircleListened.Visible := not FWasListened;
end;

procedure TFrameAttachmentAudioMessage.SetWave(const Value: TArray<Integer>);
begin
  FWave := Value;
  FWaveMax := 1;
  for var Item in FWave do
    if Item > FWaveMax then
      FWaveMax := Item;
  CalcWave;
  PaintBoxWave.Repaint;
end;

end.

