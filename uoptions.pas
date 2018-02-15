unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,  ActnList,Buttons;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Bevel1: TBevel;
    Bevel3: TBevel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    ButtonCancel: TButton;
    ButtonReplace: TButton;
    EditFind: TEdit;
    EditReplaceWith: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText10: TStaticText;
    StaticText3: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    TabSheetReplace: TTabSheet;
    TabSheetWelcome: TTabSheet;
    TabSheetAbout: TTabSheet;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ButtonReplaceClick(Sender: TObject);
    procedure EditFindChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure StaticText6Click(Sender: TObject);
  private

  public
    closeAction:TAction;
    droppedFiles:TStringList;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses UMain;

{$R *.lfm}

{ TfrmOptions }


procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  self.AllowDropFiles:=true;
  droppedFiles := TStringList.Create;
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  droppedFiles.Free;
end;

procedure TfrmOptions.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  FileName : String;
begin
  droppedFiles.Clear;
  for FileName in FileNames do
  begin
    droppedFiles.Add(FileName);
    //todo
    //if filename count 1 and ext m3u,m3u8,xspf,or txt, then open playlist,
    //otherwise new files.

  end;

  frmMain.CreateNew(droppedFiles);

  closeAction:=nil;
  self.ModalResult:=mrCancel;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  ButtonReplace.Enabled:=false;

end;

procedure TfrmOptions.StaticText6Click(Sender: TObject);
begin

end;

procedure TfrmOptions.BitBtn3Click(Sender: TObject);
begin
  closeAction := frmMain.actOpenTxt;
  self.ModalResult:=mrOK;
end;

procedure TfrmOptions.BitBtn4Click(Sender: TObject);
begin
  closeAction := frmMain.actOpenM3u;
  self.ModalResult:=mrOK;
end;

procedure TfrmOptions.BitBtn5Click(Sender: TObject);
begin
  closeAction := frmMain.actOpenXSPF;
  self.ModalResult:=mrOK;
end;

procedure TfrmOptions.BitBtn6Click(Sender: TObject);
begin
  closeAction := frmMain.actNewFile;
  self.ModalResult:=mrOK;
end;

procedure TfrmOptions.ButtonReplaceClick(Sender: TObject);
begin
  self.ModalResult:=mrOK;
end;

procedure TfrmOptions.EditFindChange(Sender: TObject);
begin
  if EditFind.Text <> '' then begin
    ButtonReplace.Enabled:=true;
  end else
  begin
    ButtonReplace.Enabled:=false;
  end;
end;

procedure TfrmOptions.FormActivate(Sender: TObject);
begin
  if EditFind.Visible then begin
    EditFind.SelectAll;
    EditFind.SetFocus;
  end;

end;


end.

