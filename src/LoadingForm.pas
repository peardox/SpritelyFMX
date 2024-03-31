unit LoadingForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation,
  SpritelyTypes, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
// TPDXMessageEvent = procedure (Sender: TObject; const Msg: String) of object;
  TfrmLoadingDialog = class(TForm)
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    Label1: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
    ProcessedFiles: Integer;
    TotalFiles: Integer;
    fOnFile: TPDXMessageEvent;
    procedure FormReset;
    procedure SetDoOnFile(const AProc: TPDXMessageEvent);
  public
    { Public declarations }
    procedure Setup(const NumFiles: Integer);
    procedure AddMessage(const AMessage: String; const MessageType: Integer = 0);
    property OnFile: TPDXMessageEvent read fOnFile write SetDoOnFile;
  end;

implementation

{$R *.fmx}

procedure TfrmLoadingDialog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmLoadingDialog.AddMessage(const AMessage: String; const MessageType: Integer = 0);
begin
  Memo1.Lines.Add(AMessage);
  Memo1.GoToTextEnd;
  if MessageType = 0 then
    begin
      ProgressBar1.Value := ProgressBar1.Value + 1;
      Label1.Text := Format('(%3.0f of %3.0f)',[ProgressBar1.Value, ProgressBar1.Max]);
    end;
  Application.ProcessMessages;
end;

procedure TfrmLoadingDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmLoadingDialog.SetDoOnFile(const AProc: TPDXMessageEvent);
begin
  fOnFile := AProc;
end;

procedure TfrmLoadingDialog.Setup(const NumFiles: Integer);
begin
  FormReset;
  ProcessedFiles := 0;
  TotalFiles := NumFiles;
  ProgressBar1.Value := 0;
  ProgressBar1.Max := NumFiles;
  Label1.Text := Format('(%3.0f of %3.0f)',[ProgressBar1.Value, ProgressBar1.Max]);

end;

procedure TfrmLoadingDialog.FormReset;
begin
  ModalResult := mrNone;
end;


end.
