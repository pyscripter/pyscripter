{
  Syn
  Copyright © 2002, Danail Traichev. All rights reserved.
  neum@developer.bg,

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is dlgAskParam.pas, released Thu, 19 Sep 2002 21:38:36 UTC.

  The Initial Developer of the Original Code is Danail Traichev.
  Portions created by Danail Traichev are Copyright © 2002 Danail Traichev.
  All Rights Reserved.

  Contributor(s): .

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  You may retrieve the latest version of this file at the Syn home page,
  located at http://syn.sourceforge.net/

 $Id: dlgAskParam.pas,v 1.8 2004/03/02 13:20:33 seier Exp $
 }
unit dlgAskParam;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  StdCtrls, SpTBXControls, dlgPyIDEBase, SpTBXEditors, SpTBXItem;

type
  (* asks for parameter value and optionally stores parameter to file *)
  TAskParamForm = class(TPyIDEDlgBase)
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    chkSaveToFile: TSpTBXCheckBox;
    txtParamValue: TSpTBXEdit;
    Label1: TSpTBXLabel;
    SpTBXPanel1: TSpTBXPanel;
  private
    FParamName: string;
    procedure SetParamName(const Value: string);
  public
    class function AskForParameter(Sender: TObject; const AName: string;
      var AValue: string): Boolean;
    property ParamName: string read FParamName write SetParamName;
  end;

var
  (* parameters, added just for current session *)
  SessionParamsCount: Integer;

implementation

uses
  uParams, cParameters;

{$R *.DFM}

class function TAskParamForm.AskForParameter(Sender: TObject; const AName: string;
                                             var AValue: string): Boolean;
(* ask for parameter value and optionally stores parameter to file *)
begin
  with TAskParamForm.Create(Application) do try
    ParamName:= AName;
    Result:= ShowModal = mrOK;
    if Result then
      with Parameters do begin
        AValue:= ReplaceInText(txtParamValue.Text);
        (* valid only for current session - store current value *)
        if not chkSaveToFile.Checked then begin
          RegisterParameter(AName, AValue, nil);
          Inc(SessionParamsCount);
        end
        (* this will be made permanent - store with parameters *)
        else begin
          CustomParams.Add(AName + '=' +txtParamValue.Text);
          RegisterCustomParams;
        end;
      end;
  finally
    Free;
  end;
end;

{ TAskParamForm }

procedure TAskParamForm.SetParamName(const Value: string);
begin
  FParamName := Value;
  Label1.Caption:= Label1.Caption + Value;
end;

var
  OldUnknownParameterProc: TUnknownParameterFunction;

initialization
  OldUnknownParameterProc:= Parameters.OnUnknownParameter;
  Parameters.OnUnknownParameter:= TAskParamForm.AskForParameter;
finalization
  Parameters.OnUnknownParameter:= OldUnknownParameterProc;
end.
