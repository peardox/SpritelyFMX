program Spritely;



uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  MainGui in 'src\MainGui.pas' {Form1},
  CastleApp in 'src\CastleApp.pas',
  SpritelySettings in 'src\SpritelySettings.pas',
  SpritelyTypes in 'src\SpritelyTypes.pas',
  CastleModel in 'src\CastleModel.pas',
  CastleHelpers in 'src\CastleHelpers.pas',
  SpritelyControls in 'src\SpritelyControls.pas',
  SpritelyDebug in 'src\SpritelyDebug.pas',
  SphericalCamera in 'src\SphericalCamera.pas',
  SpritelyCheckTextures in 'src\SpritelyCheckTextures.pas',
  SpritelyAxisGrid in 'src\SpritelyAxisGrid.pas',
  FrameToImage in 'src\FrameToImage.pas',
  LoadingForm in 'src\LoadingForm.pas' {frmLoadingDialog};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
