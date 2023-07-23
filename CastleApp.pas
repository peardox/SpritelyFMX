unit CastleApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation,
  fmx.castlecontrol, CastleUIControls, CastleVectors,
  CastleGLUtils, CastleColors, FMX.Layouts;

type
  TCastleAppView = class(TCastleView)
    procedure Render; override; // TCastleUserInterface
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCastleApp = class(TComponent)
    fGLWin: TCastleControl;
    fGLView: TCastleAppView;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fGLWin := TCastleControl.Create(AOwner);
  fGLWin.Parent := AOwner as TFMXObject;
  fGLWin.Align := TAlignLayout.Client;
  fGLView := TCastleAppView.Create(fGLWin);
  fGLWin.Container.View := fGLView;

end;

procedure TCastleAppView.Render;
var
  Points: array[0..3] of TVector2;
begin
  inherited;
  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  DrawPrimitive2D(pmLines, Points, Green);

end;
end.
