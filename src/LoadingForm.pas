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
    FormText: TLabel;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
    fOnFile: TPDXMessageEvent;
    procedure FormReset;
    procedure SetDoOnFile(const AProc: TPDXMessageEvent);
  public
    { Public declarations }
    procedure Setup(const MessageText: String);
    procedure AddingFile(const AFile: String);
    property OnFile: TPDXMessageEvent read fOnFile write SetDoOnFile;
  end;

implementation

{$R *.fmx}

procedure TfrmLoadingDialog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmLoadingDialog.AddingFile(const AFile: String);
begin
  Memo1.Lines.Add(AFile);
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

procedure TfrmLoadingDialog.Setup(const MessageText: String);
begin
  FormReset;
  FormText.Text := MessageText;
end;

procedure TfrmLoadingDialog.FormReset;
begin
  ModalResult := mrNone;
end;


end.
