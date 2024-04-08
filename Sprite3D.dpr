program Sprite3D;



uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  MainGui in 'src\MainGui.pas' {Form1},
  CastleApp in 'src\CastleApp.pas',
  Sprite3DSettings in 'src\Sprite3DSettings.pas',
  Sprite3DTypes in 'src\Sprite3DTypes.pas',
  CastleModel in 'src\CastleModel.pas',
  CastleHelpers in 'src\CastleHelpers.pas',
  Sprite3DDebug in 'src\Sprite3DDebug.pas',
  SphericalCamera in 'src\SphericalCamera.pas',
  Sprite3DCheckTextures in 'src\Sprite3DCheckTextures.pas',
  FrameToImage in 'src\FrameToImage.pas',
  LoadingForm in 'src\LoadingForm.pas' {frmLoadingDialog},
  InfoForm in 'src\InfoForm.pas',
  ModelManager in 'src\ModelManager.pas',
  SettingsForm in 'src\SettingsForm.pas' {frmSettingsDialog},
  Sprite3DCollection in 'src\Sprite3DCollection.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
