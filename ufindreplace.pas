unit UFindReplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,  ActnList,Buttons;

type

  { TfrmFindReplace }

  TfrmFindReplace = class(TForm)
    ButtonCancel: TButton;
    ButtonReplace: TButton;
    EditFind: TEdit;
    EditReplaceWith: TEdit;
    LabelFindCaption: TLabel;
    LabelReplaceCaption: TLabel;
    StaticTextA: TStaticText;
    StaticTextC: TStaticText;
    StaticTextEG: TStaticText;
    StaticTextB: TStaticText;
    procedure ButtonReplaceClick(Sender: TObject);
    procedure EditFindChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public
    closeAction:TAction;
    droppedFiles:TStringList;
  end;

var
  frmFindReplace: TfrmFindReplace;

implementation


{$R *.lfm}

{ TfrmFindReplace }



procedure TfrmFindReplace.ButtonReplaceClick(Sender: TObject);
begin
  self.ModalResult:=mrOK;
end;

procedure TfrmFindReplace.EditFindChange(Sender: TObject);
begin
  if EditFind.Text <> '' then begin
    ButtonReplace.Enabled:=true;
  end else
  begin
    ButtonReplace.Enabled:=false;
  end;
end;

procedure TfrmFindReplace.FormActivate(Sender: TObject);
begin
  EditFind.SelectAll;
  EditFind.SetFocus;
end;


end.

