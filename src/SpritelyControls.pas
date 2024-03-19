unit SpritelyControls;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Styles,
  FMX.Controls.Presentation,
  CastleApp, FMX.Layouts, FMX.StdCtrls,
  FMX.ListBox,
  FMX.Edit,
  FMX.Objects,
  SpritelyTypes;
type
  TPDXRadialDial = class(TLayout)
    FTopPanel: TLayout;
    FBottomPanel: TLayout;
    FDial: TCircle;
    FKnob: TCircle;
    FCaptionLabel: TLabel;
    FStepsLabel: TLabel;
    FStepsComboBox: TComboBox;
  private
    { Private declarations }
    FChangedByCode: Boolean;
    FKnobAngle: Single;
    FKnobSize: Integer;
    FRadius: Integer;
    FActive: Boolean;
    FMoving: Boolean;
    FSteps: Integer;
//    FAngleText: String;
    FMin: Single;
    FMax: Single;
    FDoExtMessage: TPDXMessageEvent;
    FDoExtRotate: TPDXRotationEvent;
//    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    procedure DoRotate(const ARotation: Single);
    procedure SetDoExtRotate(const AProc: TPDXRotationEvent);
    procedure SetKnobAngle(const AValue: Single);
    procedure DoSetKnobAngle(const AValue: Single);
    procedure DialMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DialMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DialMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseKnob(X, Y: Single);
    procedure ComboBoxChange(Sender: TObject);
    procedure SetSteps(const AValue: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property OnExtMessage: TPDXMessageEvent read FDoExtMessage write SetDoExtMessage;
    property OnRotate: TPDXRotationEvent read FDoExtRotate write SetDoExtRotate;
    property Angle: Single read FKnobAngle write SetKnobAngle;
    property Active: Boolean read FActive;
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property Steps: Integer read FSteps write SetSteps;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave;
  end;

  TPDXArcDial = class(TLayout)
    FTopPanel: TLayout;
    FDial: TArc;
    FKnob: TCircle;
    FCaptionLabel: TLabel;
  private
    { Private declarations }
    FChangedByCode: Boolean;
    FKnobAngle: Single;
    FKnobSize: Integer;
    FRadius: Single;
    FActive: Boolean;
    FMoving: Boolean;
//    FAngleText: String;
    FMin: Single;
    FMax: Single;
    FOffset: Single;
    FMultiplier: Single;
    FDoExtMessage: TPDXMessageEvent;
    FDoExtRotate: TPDXRotationEvent;
//    procedure SendMessage(const AMsg: String);
    procedure SetDoExtMessage(const AProc: TPDXMessageEvent);
    procedure DoRotate(const ARotation: Single);
    procedure SetDoExtRotate(const AProc: TPDXRotationEvent);
    procedure SetKnobAngle(const AValue: Single);
//    function  GetKnobAngle: Single;
    function  GetKnobAngleScaled: Single;
    procedure DoSetKnobAngle(const AValue: Single);
    procedure DialMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DialMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DialMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseKnob(X, Y: Single);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property OnExtMessage: TPDXMessageEvent read FDoExtMessage write SetDoExtMessage;
    property OnRotate: TPDXRotationEvent read FDoExtRotate write SetDoExtRotate;
    property Angle: Single read GetKnobAngleScaled write SetKnobAngle;
    property Active: Boolean read FActive;
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property Multiplier: Single read FMultiplier write FMultiplier;
    property Offset: Single read FOffset write FOffset;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave;
  end;

const
  RotationSteps: Array[0..17] of Integer = (4, 5, 6, 8, 10, 12, 15, 16, 24, 30, 32, 45, 64, 90, 128, 180, 256, 360);

implementation

uses
  Math;

{ TRadialDial }

procedure TPDXRadialDial.ComboBoxChange(Sender: TObject);
begin
//  SendMessage('Item Index = ' + IntToStr(FStepsComboBox.ItemIndex));
  if (FStepsComboBox.ItemIndex >= 0) and (FStepsComboBox.ItemIndex < Length(RotationSteps)) then
    begin
      fSteps := RotationSteps[FStepsComboBox.ItemIndex];
      DoSetKnobAngle(fKnobAngle);
    end;
end;

constructor TPDXRadialDial.Create(AOwner: TComponent);
var
  I: Integer;
  Item: TListBoxItem;
begin
  inherited;
  if AOwner is TStyledControl then
    Raise Exception.Create('TRedial must be owned by a TStyledControl');

  Parent := TStyledControl(AOwner);
  Width := TStyledControl(Parent).Width;
  Height := TStyledControl(Parent).Height;
  FChangedByCode := False;

  FMin := 0;
  FMax := PI * 2;

  FRadius := 100;
  FKnobSize := 16;
  FSteps := 64;
  FKnobAngle := -1;

  Align := TAlignLayout.Client;

  Top := 0;
  Left := 0;
//  Width := FRadius * 2 + FKnobSize;
//  Height := FRadius * 2 + FKnobSize + 52;
  HitTest := False;

//  BevelOuter := bvNone;

  FTopPanel := TLayout.Create(AOwner);
  FTopPanel.HitTest := False;

  FTopPanel.Position.X := 0;
  FTopPanel.Position.Y := 0;
  FTopPanel.Width := FRadius * 2 + FKnobSize;
  FTopPanel.Height := FRadius * 2 + FKnobSize;

//  FTopPanel.Height := FTopPanel.Width;
  FTopPanel.Parent := Self;

  FDial := TCircle.Create(FTopPanel);
  FDial.HitTest := False;
  FDial.Position.X := FKnobSize;
  FDial.Position.Y := FKnobSize;
  FDial.Width := (FRadius * 2) - FKnobSize;
  FDial.Height := (FRadius * 2) - FKnobSize;
  FDial.Fill.Kind := TBrushKind.Resource;
  FDial.Stroke.Color := TAlphaColorRec.Black;
  FDial.Parent := FTopPanel;

  FKnob := TCircle.Create(FTopPanel);
  FKnob.HitTest := True;
  FKnob.Position.X := 0;
  FKnob.Position.Y := 0;
  FKnob.Width := FKnobSize;
  FKnob.Height := FKnobSize;
  FKnob.Fill.Color := TAlphaColorRec.Red;
  FKnob.OnMouseDown := DialMouseDown;
  FKnob.OnMouseUp := DialMouseUp;
  FKnob.OnMouseMove := DialMouseMove;
  FKnob.Parent := FTopPanel;

  FCaptionLabel := TLabel.Create(FTopPanel);
  FCaptionLabel.HitTest := True;
  FCaptionLabel.Align := TAlignLayout.Client;
  FCaptionLabel.Font.Size := 14;
  FCaptionLabel.TextSettings.FontColor := TAlphaColorRec.Black;
  FCaptionLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FCaptionLabel.TextSettings.VertAlign := TTextAlign.Center;
  FCaptionLabel.OnMouseUp := DialMouseUp;
  FCaptionLabel.OnMouseDown := DialMouseDown;
  FCaptionLabel.OnMouseMove := DialMouseMove;

  FCaptionLabel.Parent := FTopPanel;

  FBottomPanel := TLayout.Create(AOwner);
  FBottomPanel.Align := TAlignLayout.Bottom;
  FBottomPanel.Height := 48;
  FBottomPanel.Parent := Self;

  FStepsLabel := TLabel.Create(FBottomPanel);
  FStepsLabel.Position.X := 16;
  FStepsLabel.Position.Y := 16;
  FStepsLabel.Parent := FBottomPanel;
  FStepsLabel.Text := 'Steps';

  FStepsComboBox := TComboBox.Create(FBottomPanel) ;
  FStepsComboBox.Position.X := 100;
  FStepsComboBox.Position.Y := 8;
  FStepsComboBox.TabStop := False;
  FStepsComboBox.Width  := FBottomPanel.Width / 2;
  FStepsComboBox.Parent := FBottomPanel;

  for I := 0 to Length(RotationSteps) - 1 do
    begin
      Item := TListBoxItem.Create(FStepsComboBox);
      Item.Parent := FStepsComboBox;
      Item.TextSettings.HorzAlign :=  TTextAlign.Trailing;
      Item.Text := IntToStr(RotationSteps[I]); // set filename
    end;

  FStepsComboBox.OnChange := ComboBoxChange;
  FStepsComboBox.ItemIndex := 10;

  SetKnobAngle(0);

  FActive := True;
  FMoving := False;

end;

procedure TPDXRadialDial.DialMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FActive and not FMoving and (Button = TMouseButton.mbLeft) then
  begin
    FMoving := True;
    MouseKnob(X, Y);
  end;
end;

procedure TPDXRadialDial.DialMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FActive and FMoving then
   begin
     MouseKnob(X, Y);
   end;
end;

procedure TPDXRadialDial.DialMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FActive and FMoving and (Button = TMouseButton.mbLeft) then
  begin
    FMoving := False;
    MouseKnob(X, Y);
  end;
end;

procedure TPDXRadialDial.DoRotate(const ARotation: Single);
begin
  if Assigned(FDoExtRotate) and not(FChangedByCode) then
    FDoExtRotate(Self, ARotation);
  FChangedByCode := False;
end;

procedure TPDXRadialDial.DoSetKnobAngle(const AValue: Single);
begin
  if FSteps > 0 then
    FKnobAngle := AValue - fmod(AValue, ((2 * Pi) / FSteps))
  else
   FKnobAngle := AValue;
  while FKnobAngle < 0 do FKnobAngle := FKnobAngle + (2 * Pi);
  while FKnobAngle > (2 * Pi) do FKnobAngle := FKnobAngle - (2 * Pi);

  FKnob.Position.X := Round(((FRadius - (FKnobSize / 2)) * cos(FKnobAngle + (Pi * 1.5))) + FRadius);
  FKnob.Position.Y := Round(((FRadius - (FKnobSize / 2)) * sin(FKnobAngle + (Pi * 1.5))) + FRadius);

  FCaptionLabel.Text := FormatFloat('##0.000', RadToDeg(FKnobAngle)) + #186;

  DoRotate(FKnobAngle);
end;

procedure TPDXRadialDial.MouseKnob(X, Y: Single);
var
  OX, OY: Single;
  Theta: Single;
begin
  OX := X - (FDial.Position.X + (FDial.Width / 2));
  OY := Y - (FDial.Position.Y + (FDial.Height / 2));
  Theta := arctan2(OY, OX);
//  SendMessage('Theta = ' + FloatToStr(Theta));
  SetKnobAngle((Theta) + (Pi * 0.5));
end;

{
procedure TPDXRadialDial.SendMessage(const AMsg: String);
begin
  if Assigned(FDoExtMessage) then
    FDoExtMessage(Self, AMsg);
end;
}

procedure TPDXRadialDial.SetDoExtMessage(const AProc: TPDXMessageEvent);
begin
  FDoExtMessage := AProc;
end;

procedure TPDXRadialDial.SetDoExtRotate(const AProc: TPDXRotationEvent);
begin
  FDoExtRotate := AProc;
end;

procedure TPDXRadialDial.SetKnobAngle(const AValue: Single);
begin
  if FKnobAngle <> AValue then
    begin
      DoSetKnobAngle(AValue);
    end;
end;

procedure TPDXRadialDial.SetSteps(const AValue: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(RotationSteps) do
    begin
      if RotationSteps[I] = AValue then
        begin
          FSteps := I;
          FChangedByCode := True;
          FStepsComboBox.ItemIndex := I;
          Break;
        end;
    end;
end;

{ TArcDial }

constructor TPDXArcDial.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TStyledControl then
    Raise Exception.Create('TRedial must be owned by a TStyledControl');

  Parent := TStyledControl(AOwner);
  Width := TStyledControl(Parent).Width;
  Height := TStyledControl(Parent).Width;
  FChangedByCode := False;

  FMin := 0;
  FMax := 1;
  FOffset := -0.5;
  FMultiplier := pi;
  FKnobSize := 16;
  FKnobAngle := -1;
  FRadius := (Width / 2) - FKnobSize; //100;

  Align := TAlignLayout.Client;

  Top := 0;
  Left := 0;
//  Width := FRadius * 2 + FKnobSize;
//  Height := FRadius * 2 + FKnobSize + 52;
  HitTest := False;

//  BevelOuter := bvNone;

  FTopPanel := TLayout.Create(AOwner);
  FTopPanel.HitTest := False;

  FTopPanel.Position.X := 0;
  FTopPanel.Position.Y := 0;
  FTopPanel.Width := (FRadius * 2) + FKnobSize;
  FTopPanel.Height := (FRadius * 2) + FKnobSize;

//  FTopPanel.Height := FTopPanel.Width;
  FTopPanel.Parent := Self;

  FDial := TArc.Create(FTopPanel);
  FDial.HitTest := False;
  FDial.Position.X := FKnobSize;
  FDial.Position.Y := FKnobSize;
  FDial.StartAngle := 0;
  FDial.EndAngle := 180;
  FDial.RotationAngle := -90;

  FDial.Width := (FRadius * 2) - FKnobSize;
  FDial.Height := (FRadius * 2) - FKnobSize;
  FDial.Fill.Kind := TBrushKind.Resource;
  FDial.Stroke.Color := TAlphaColorRec.Black;
  FDial.Parent := FTopPanel;

  FKnob := TCircle.Create(FTopPanel);
  FKnob.HitTest := True;
  FKnob.Position.X := 0;
  FKnob.Position.Y := 0;
  FKnob.Width := FKnobSize;
  FKnob.Height := FKnobSize;
  FKnob.Fill.Color := TAlphaColorRec.Red;
  FKnob.OnMouseDown := DialMouseDown;
  FKnob.OnMouseUp := DialMouseUp;
  FKnob.OnMouseMove := DialMouseMove;
  FKnob.Parent := FTopPanel;

  FCaptionLabel := TLabel.Create(FTopPanel);
  FCaptionLabel.HitTest := True;
  FCaptionLabel.Align := TAlignLayout.Client;
  FCaptionLabel.Font.Size := 14;
  FCaptionLabel.TextSettings.FontColor := TAlphaColorRec.Black;
  FCaptionLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FCaptionLabel.TextSettings.VertAlign := TTextAlign.Center;
  FCaptionLabel.OnMouseUp := DialMouseUp;
  FCaptionLabel.OnMouseDown := DialMouseDown;
  FCaptionLabel.OnMouseMove := DialMouseMove;

  FCaptionLabel.Parent := FTopPanel;

  SetKnobAngle(Pi/2);

  FActive := True;
  FMoving := False;
end;

procedure TPDXArcDial.DialMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FActive and not FMoving and (Button = TMouseButton.mbLeft) then
  begin
    FMoving := True;
    MouseKnob(X, Y);
  end;
end;

procedure TPDXArcDial.DialMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FActive and FMoving then
   begin
     MouseKnob(X, Y);
   end;
end;

procedure TPDXArcDial.DialMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FActive and FMoving and (Button = TMouseButton.mbLeft) then
  begin
    FMoving := False;
    MouseKnob(X, Y);
  end;
end;

procedure TPDXArcDial.DoRotate(const ARotation: Single);
begin
  if Assigned(FDoExtRotate) and not(FChangedByCode) then
    FDoExtRotate(Self, ARotation);
  FChangedByCode := False;
end;

procedure TPDXArcDial.DoSetKnobAngle(const AValue: Single);
var
  KAS: Single;
begin
  FKnobAngle := AValue;
  if FKnobAngle < 0 then FKnobAngle := 0;
  if FKnobAngle >= Pi then FKnobAngle := Pi;

  FKnob.Position.X := Round(((FRadius - (FKnobSize / 2)) * cos(FKnobAngle + (Pi * 1.5))) + FRadius);
  FKnob.Position.Y := Round(((FRadius - (FKnobSize / 2)) * sin(FKnobAngle + (Pi * 1.5))) + FRadius);

  KAS := GetKnobAngleScaled;
  FCaptionLabel.Text := FormatFloat('##0.000', RadToDeg(-KAS)) + #186;
  DoRotate(KAS);
end;
{
function TPDXArcDial.GetKnobAngle: Single;
begin
  Result := fKnobAngle;
end;
}
procedure TPDXArcDial.MouseKnob(X, Y: Single);
var
  OX, OY: Single;
  Theta: Single;
begin
  OX := X - (FDial.Position.X + (FDial.Width / 2));
  OY := Y - (FDial.Position.Y + (FDial.Height / 2));
  Theta := arctan2(OY, OX);
  SetKnobAngle((Theta) + (Pi * 0.5));
end;
{
procedure TPDXArcDial.SendMessage(const AMsg: String);
begin
  if Assigned(FDoExtMessage) then
    FDoExtMessage(Self, AMsg);
end;
}

procedure TPDXArcDial.SetDoExtMessage(const AProc: TPDXMessageEvent);
begin
  FDoExtMessage := AProc;
end;

procedure TPDXArcDial.SetDoExtRotate(const AProc: TPDXRotationEvent);
begin
  FDoExtRotate := AProc;
end;

procedure TPDXArcDial.SetKnobAngle(const AValue: Single);
begin
  if FKnobAngle <> AValue then
    begin
      DoSetKnobAngle(AValue);
    end;
end;

function TPDXArcDial.GetKnobAngleScaled: Single;
begin
  Result := ((fKnobAngle / Pi) + FOffset) * FMultiplier;
end;

end.
