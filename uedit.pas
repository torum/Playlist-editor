unit UEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmEdit }

  TfrmEdit = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    EditEdit: TEdit;
    LabelEditCaption: TLabel;
    StaticTextB: TStaticText;
    StaticTextA: TStaticText;
    StaticTextC: TStaticText;
    StaticTextEG: TStaticText;
  private

  public

  end;

var
  frmEdit: TfrmEdit;

implementation

{$R *.lfm}

end.

