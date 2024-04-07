unit InfoForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmInfoDialog = class(TForm)
    FormText: TLabel;
    Panel1: TPanel;
    BtnOK: TButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure FormReset;
  public
    { Public declarations }
    procedure Setup(const MessageText: String);
  end;

implementation

{$R *.fmx}

uses
  Sprite3DSettings;

procedure TfrmInfoDialog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmInfoDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmInfoDialog.Setup(const MessageText: String);
begin
  FormReset;
  FormText.Text := MessageText;
end;

procedure TfrmInfoDialog.FormCreate(Sender: TObject);
begin
  Caption := APPNAME;
end;

procedure TfrmInfoDialog.FormReset;
begin
  BtnOK.Enabled := True;
  ModalResult := mrNone;
end;


procedure TfrmInfoDialog.FormShow(Sender: TObject);
begin
  Application.ProcessMessages;
end;

end.
