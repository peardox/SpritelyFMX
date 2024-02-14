program Spritely;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainGui in 'src\MainGui.pas' {Form1},
  CastleApp in 'src\CastleApp.pas',
  SpritelySettings in 'src\SpritelySettings.pas',
  SpritelyTypes in 'src\SpritelyTypes.pas',
  CastleModel in 'src\CastleModel.pas',
  CastleHelpers in 'src\CastleHelpers.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
