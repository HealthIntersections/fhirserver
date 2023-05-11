unit VitalSignsGeneratorDialog;

{
Copyright (c) 2016+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_utilities,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Client, FHIR.Version.Utilities,
  ToolkitSettings, FMX.Edit;

type
  TDataPoint = class (TFslObject)
  private
    FDiastolic: String;
    FSaturation: String;
    FMinutes: integer;
    FSystolic: String;
    FTemperature: String;
    FHeartRate : Integer;
  public
    property Minutes : integer read FMinutes write FMinutes;
    property Diastolic : String read FDiastolic write FDiastolic;
    property Systolic : String read FSystolic write FSystolic;
    property Saturation : String read FSaturation write FSaturation;
    property Temperature : String read FTemperature write FTemperature;
    property HeartRate : integer read FHeartRate write FHeartRate;
  end;

  TVitalSignsGeneratorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtPatientId: TEdit;
    edtSourceLength: TEdit;
    edtFrequency: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    chkRealTime: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FClient: TFHIRClient;
    FSettings: TFHIRToolkitSettings;
    FData : TFslList<TDataPoint>;
    FObservations : TFhirBundle;
    procedure SetClient(const Value: TFHIRClient);
    procedure SetSettings(const Value: TFHIRToolkitSettings);

    procedure loadData;
    procedure makeObs(i : integer; when : TDateTime; data : TDataPoint);
    function ProduceData : boolean;
    function baseVitals(when: TDateTime; min: integer; name, lCode, text: String): TFHIRObservation;
    function component(obs: TFHIRObservation; lCode, text: string): TFhirObservationComponent;
    function makeQty(value: String; human, ucum: String): TFHIRQuantity; overload;
  public
    destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
  end;

var
  VitalSignsGeneratorForm: TVitalSignsGeneratorForm;

implementation

{$R *.fmx}

{ TVitalSignsGeneratorForm }

procedure TVitalSignsGeneratorForm.Button1Click(Sender: TObject);
begin
  Settings.storeValue('vitals', 'patient', edtPatientId.Text);
  Settings.storeValue('vitals', 'length', edtSourceLength.Text);
  Settings.storeValue('vitals', 'frequeny', edtFrequency.Text);
  if ProduceData then
    ModalResult := mrOk;
end;

destructor TVitalSignsGeneratorForm.Destroy;
begin
  FObservations.Free;
  FSettings.Free;
  FClient.Free;
  FData.Free;
  inherited;
end;

procedure TVitalSignsGeneratorForm.FormShow(Sender: TObject);
begin
  edtPatientId.Text := Settings.getValue('vitals', 'patient', 'example');
  edtSourceLength.Text := Settings.getValue('vitals', 'length', '150');
  edtFrequency.Text := Settings.getValue('vitals', 'frequency', '60');
  chkRealTime.IsChecked := false;
end;

procedure TVitalSignsGeneratorForm.loadData;
  procedure dp(Minutes : integer; Diastolic,Systolic,Saturation,Temperature : String; hr : integer);
  var
    d : TDataPoint;
  begin
    d := TDataPoint.Create;
    FData.Add(d);
    d.Diastolic := Diastolic;
    d.Systolic := Systolic;
    d.Saturation := Saturation;
    d.Temperature := Temperature;
    d.HeartRate := hr;
  end;
begin
  FData := TFslList<TDataPoint>.create;
  dp(0, '80', '120', '95', '37.1', 72);
  dp(1, '80.23134274', '121.3510661', '95.12530267', '37.09470397', 72);
  dp(2, '80.46225173', '122.6983105', '95.25041128', '37.0894059', 73);
  dp(3, '80.69229322', '124.0379114', '95.37513176', '37.08410372', 72);
  dp(4, '80.92103344', '125.366047', '95.49927007', '37.0787954', 73);
  dp(5, '81.14803865', '126.6788956', '95.62263213', '37.07347887', 74);
  dp(6, '81.3728751', '127.9726355', '95.74502389', '37.06815209', 72);
  dp(7, '81.59510903', '129.2434448', '95.86625128', '37.06281301', 69);
  dp(8, '81.81430669', '130.4875018', '95.98612025', '37.05745957', 68);
  dp(9, '82.03003433', '131.7009849', '96.10443674', '37.05208974', 72);
  dp(10, '82.24185819', '132.8800721', '96.22100668', '37.04670145', 76);
  dp(11, '82.44934452', '134.0209419', '96.33563601', '37.04129266', 78);
  dp(12, '82.65205957', '135.1197723', '96.44813067', '37.03586131', 82);
  dp(13, '82.84956958', '136.1727417', '96.5582966', '37.03040536', 85);
  dp(14, '83.04144081', '137.1760284', '96.66593975', '37.02492276', 84);
  dp(15, '83.22723949', '138.1258105', '96.77086604', '37.01941146', 82);
  dp(16, '83.40653188', '139.0182663', '96.87288143', '37.0138694', 83);
  dp(17, '83.57888422', '139.849574', '96.97179184', '37.00829453', 85);
  dp(18, '83.74386277', '140.615912', '97.06740322', '37.00268481', 84);
  dp(19, '83.90103376', '141.3134584', '97.15952151', '36.99703818', 86);
  dp(20, '84.04996344', '141.9383915', '97.24795265', '36.9913526', 73);
  dp(21, '84.19021807', '142.4868896', '97.33250257', '36.98562601', 72);
  dp(22, '84.32136388', '142.9551308', '97.41297722', '36.97985637', 73);
  dp(23, '84.44296713', '143.3392935', '97.48918253', '36.97404162', 75);
  dp(24, '84.55459406', '143.6355559', '97.56092444', '36.96817971', 77);
  dp(25, '84.65581092', '143.8400962', '97.6280089', '36.9622686', 79);
  dp(26, '84.74618396', '143.9490927', '97.69024184', '36.95630623', 81);
  dp(27, '84.82527942', '143.9587236', '97.7474292', '36.95029055', 82);
  dp(28, '84.89266355', '143.8651672', '97.79937693', '36.94421951', 79);
  dp(29, '84.9479026', '143.6646017', '97.84589095', '36.93809106', 78);
  dp(30, '84.99056281', '143.3532054', '97.88677721', '36.93190316', 76);
  dp(31, '85.02021044', '142.9271565', '97.92184166', '36.92565375', 72);
  dp(32, '85.03641172', '142.3826332', '97.95089022', '36.91934078', 69);
  dp(33, '85.03873291', '141.7158139', '97.97372883', '36.91296219', 68);
  dp(34, '85.02674026', '140.9228768', '97.99016345', '36.90651595', 72);
  dp(35, '85', '140', '98', '36.9', 78);
  dp(36, '84.95696345', '138.9456233', '98.00290253', '36.89331482', 80);
  dp(37, '84.89162212', '137.7672316', '97.99796747', '36.88597099', 81);
  dp(38, '84.79685259', '136.4745713', '97.98414937', '36.87738164', 83);
  dp(39, '84.66553146', '135.077389', '97.96040276', '36.8669599', 84);
  dp(40, '84.49053528', '133.5854309', '97.92568219', '36.85411886', 79);
  dp(41, '84.26474064', '132.0084435', '97.8789422', '36.83827167', 76);
  dp(42, '83.98102413', '130.3561732', '97.81913733', '36.81883143', 73);
  dp(43, '83.63226232', '128.6383664', '97.74522211', '36.79521126', 72);
  dp(44, '83.21133178', '126.8647695', '97.65615109', '36.76682429', 75);
  dp(45, '82.7111091', '125.0451289', '97.55087881', '36.73308364', 77);
  dp(46, '82.12447086', '123.1891911', '97.42835981', '36.69340241', 81);
  dp(47, '81.44429363', '121.3067023', '97.28754863', '36.64719374', 82);
  dp(48, '80.663454', '119.4074091', '97.12739981', '36.59387074', 79);
  dp(49, '79.77482854', '117.5010578', '96.94686788', '36.53284653', 78);
  dp(50, '78.77129383', '115.5973948', '96.7449074', '36.46353424', 77);
  dp(51, '77.64572645', '113.7061666', '96.5204729', '36.38534697', 77);
  dp(52, '76.39100298', '111.8371195', '96.27251892', '36.29769785', 77);
  dp(53, '75', '110', '96', '36.2', 77);
  dp(54, '73.47303297', '108.2036583', '95.7028407', '36.09226874', 76);
  dp(55, '71.8401729', '106.4533604', '95.38484562', '35.9769282', 73);
  dp(56, '70.13892968', '104.7534758', '95.05078938', '35.85700473', 72);
  dp(57, '68.4068132', '103.1083744', '94.70544662', '35.73552464', 71);
  dp(58, '66.68133334', '101.5224259', '94.35359195', '35.61551429', 75);
  dp(59, '65', '100', '94', '35.5', 76);
  dp(60, '63.39392247', '98.54460946', '93.64874623', '35.39147997', 81);
  dp(61, '61.86860765', '97.15633914', '93.30110947', '35.29033979', 79);
  dp(62, '60.42316184', '95.83441692', '92.9576694', '35.19643693', 80);
  dp(63, '59.05669133', '94.57807066', '92.61900569', '35.10962885', 84);
  dp(64, '57.76830244', '93.38652825', '92.28569802', '35.02977301', 85);
  dp(65, '56.55710146', '92.25901756', '91.95832605', '34.95672686', 86);
  dp(66, '55.42219469', '91.19476647', '91.63746948', '34.89034786', 90);
  dp(67, '54.36268844', '90.19300285', '91.32370795', '34.83049348', 91);
  dp(68, '53.377689', '89.25295458', '91.01762117', '34.77702117', 88);
  dp(69, '52.46630267', '88.37384953', '90.71978879', '34.72978838', 86);
  dp(70, '51.62763576', '87.55491559', '90.43079048', '34.68865259', 84);
  dp(71, '50.86079456', '86.79538062', '90.15120594', '34.65347124', 81);
  dp(72, '50.16488538', '86.0944725', '89.88161482', '34.62410181', 79);
  dp(73, '49.53901452', '85.45141911', '89.62259681', '34.60040173', 77);
  dp(74, '48.98228827', '84.86544833', '89.37473158', '34.58222848', 76);
  dp(75, '48.49381294', '84.33578803', '89.13859879', '34.56943951', 74);
  dp(76, '48.07269484', '83.86166608', '88.91477813', '34.56189229', 75);
  dp(77, '47.71804025', '83.44231036', '88.70384928', '34.55944426', 77);
  dp(78, '47.42895549', '83.07694875', '88.50639189', '34.5619529', 76);
  dp(79, '47.20454684', '82.76480912', '88.32298566', '34.56927565', 75);
  dp(80, '47.04392062', '82.50511936', '88.15421025', '34.58126998', 78);
  dp(81, '46.94618313', '82.29710733', '88.00064533', '34.59779335', 79);
  dp(82, '46.91044065', '82.14000091', '87.86287059', '34.61870321', 78);
  dp(83, '46.9357995', '82.03302797', '87.74146569', '34.64385702', 76);
  dp(84, '47.02136598', '81.9754164', '87.63701031', '34.67311225', 74);
  dp(85, '47.16624638', '81.96639407', '87.55008412', '34.70632635', 72);
  dp(86, '47.36954701', '82.00518885', '87.4812668', '34.74335677', 71);
  dp(87, '47.63037417', '82.09102862', '87.43113803', '34.78406099', 72);
  dp(88, '47.94783415', '82.22314126', '87.40027747', '34.82829646', 73);
  dp(89, '48.32103327', '82.40075464', '87.3892648', '34.87592063', 73);
  dp(90, '48.74907781', '82.62309664', '87.39867969', '34.92679097', 73);
  dp(91, '49.23107409', '82.88939514', '87.42910182', '34.98076493', 73);
  dp(92, '49.7661284', '83.198878', '87.48111087', '35.03769997', 74);
  dp(93, '50.35334704', '83.55077311', '87.5552865', '35.09745356', 75);
  dp(94, '50.99183631', '83.94430834', '87.65220839', '35.15988314', 76);
  dp(95, '51.68070251', '84.37871156', '87.77245622', '35.22484619', 78);
  dp(96, '52.41905195', '84.85321066', '87.91660966', '35.29220016', 79);
  dp(97, '53.20599093', '85.36703351', '88.08524838', '35.3618025', 81);
  dp(98, '54.04062574', '85.91940798', '88.27895206', '35.43351068', 83);
  dp(99, '54.92206269', '86.50956196', '88.49830037', '35.50718215', 82);
  dp(100, '55.84940807', '87.13672331', '88.74387299', '35.58267438', 71);
  dp(101, '56.8217682', '87.80011991', '89.01624958', '35.65984482', 70);
  dp(102, '57.83824936', '88.49897965', '89.31600984', '35.73855093', 65);
  dp(103, '58.89795786', '89.23253038', '89.64373342', '35.81865017', 64);
  dp(104, '60', '90', '90', '35.9', 63);
  dp(105, '61.1345706', '90.84606352', '90.38355113', '35.98389373', 59);
  dp(106, '62.25621851', '91.99718453', '90.78577581', '36.07736804', 54);
  dp(107, '63.31058113', '93.72527379', '91.19622493', '36.1888955', 50);
  dp(108, '64.24329584', '96.30224203', '91.60444937', '36.32694864', 54);
  dp(109, '65', '100', '92', '36.5', 45);
  dp(110, '65.57240564', '104.9719183', '92.37705229', '36.71102798', 40);
  dp(111, '66.13652329', '110.8972073', '92.74828005', '36.94103435', 56);
  dp(112, '66.91443812', '117.3365372', '93.13098166', '37.16552673', 71);
  dp(113, '68.1282353', '123.850578', '93.54245551', '37.36001274', 72);
  dp(114, '70', '130', '94', '37.5', 73);
  dp(115, '72.67086981', '135.4165981', '94.51312897', '37.56783003', 77);
  dp(116, '75.95819205', '140.0166658', '95.06021814', '37.57317999', 79);
  dp(117, '79.59836644', '143.7876216', '95.61185866', '37.53256091', 81);
  dp(118, '83.32779274', '146.7168836', '96.1386417', '37.46248386', 83);
  dp(119, '86.88287068', '148.7918703', '96.61115842', '37.37945987', 86);
  dp(120, '90', '150', '97', '37.3', 87);
  dp(121, '92.47148837', '150.3500547', '97.28294126', '37.23748144', 84);
  dp(122, '94.31327523', '149.9362719', '97.46649166', '37.19274595', 83);
  dp(123, '95.59720795', '148.8742527', '97.56434434', '37.16350142', 81);
  dp(124, '96.39513389', '147.2795984', '97.59019244', '37.14745574', 79);
  dp(125, '96.77890043', '145.2679101', '97.55772908', '37.14231683', 78);
  dp(126, '96.82035494', '142.9547889', '97.48064739', '37.14579257', 77);
  dp(127, '96.59134477', '140.4558362', '97.37264051', '37.15559086', 75);
  dp(128, '96.16371732', '137.886653', '97.24740156', '37.1694196', 74);
  dp(129, '95.60931994', '135.3628405', '97.11862368', '37.18498668', 77);
  dp(130, '95', '133', '97', '37.2', 78);
  dp(131, '94.39548927', '130.892161', '96.90274273', '37.21257319', 77);
  dp(132, '93.80705712', '129.0470664', '96.82814043', '37.22244282', 75);
  dp(133, '93.2338573', '127.4508877', '96.77500073', '37.22975117', 74);
  dp(134, '92.67504357', '126.0897961', '96.74213126', '37.23464054', 76);
  dp(135, '92.1297697', '124.949963', '96.72833965', '37.23725322', 78);
  dp(136, '91.59718943', '124.0175597', '96.73243354', '37.2377315', 76);
  dp(137, '91.07645653', '123.2787576', '96.75322056', '37.23621768', 75);
  dp(138, '90.56672477', '122.7197281', '96.78950834', '37.23285405', 74);
  dp(139, '90.06714789', '122.3266424', '96.84010452', '37.22778291', 73);
  dp(140, '89.57687965', '122.085672', '96.90381674', '37.22114654', 72);
  dp(141, '89.09507383', '121.9829881', '96.97945262', '37.21308723', 71);
  dp(142, '88.62088417', '122.0047621', '97.0658198', '37.2037473', 72);
  dp(143, '88.15346444', '122.1371653', '97.16172591', '37.19326901', 73);
  dp(144, '87.69196839', '122.3663692', '97.26597859', '37.18179468', 74);
  dp(145, '87.23554978', '122.678545', '97.37738546', '37.16946658', 75);
  dp(146, '86.78336238', '123.059864', '97.49475417', '37.15642703', 76);
  dp(147, '86.33455995', '123.4964977', '97.61689234', '37.1428183', 75);
  dp(148, '85.88829623', '123.9746174', '97.74260762', '37.12878269', 74);
  dp(149, '85.44372499', '124.4803944', '97.87070763', '37.11446249', 73);
  dp(150, '85', '125', '98', '37.1', 74);
  dp(151, '84.55627501', '125.5196056', '98.12929237', '37.08553751', 73);
end;

function TVitalSignsGeneratorForm.baseVitals(when : TDateTime; min : integer; name, lCode, text : String) : TFHIRObservation;
begin
  result := TFHIRObservation.Create;
  result.Id := 'obs-vitals-'+name+'-'+inttostr(min);
  with FObservations.entryList.Append do
  begin
    resource := result;
    fullUrl := FClient.address+'/Observation/'+result.id;
  end;
  result.Subject := TFHIRReference.create('Patient/'+edtPatientId.Text);
  result.Status := ObservationStatusFinal;
  result.Effective := TFhirDateTime.Create(TFslDateTime.makeLocal(when));
  with result.categoryList.Append do
  begin
    text := 'Vital Signs';
    with CodingList.append do
    begin
     System := 'http://hl7.org/fhir/observation-category';
     Code := 'vital-signs';
     Display := 'Vital Signs';
    end;
  end;
  result.Code := TFhirCodeableConcept.Create('http://loinc.org', lCode);
  result.code.text := text;
End;

function TVitalSignsGeneratorForm.component(obs : TFHIRObservation; lCode, text : string) : TFHIRObservationComponent;
begin
  result := obs.componentList.Append;
  result.Code := TFhirCodeableConcept.Create('http://loinc.org', lCode);
  result.code.text := text;
end;

function TVitalSignsGeneratorForm.makeQty(value : String; human, ucum : String) : TFHIRQuantity;
begin
  result := TFhirQuantity.Create;
  result.Code := ucum;
  result.System := 'http://unitsofmeasure.org';
  result.unit_ := human;
  result.value := value;
end;

procedure TVitalSignsGeneratorForm.makeObs(i : integer; when: TDateTime; data: TDataPoint);
var
  obs : TFHIRObservation;
begin
  baseVitals(when, i, 'sat', '59408-5', 'O2 Saturation').Value := makeQty(data.Saturation, '%', '%');
  baseVitals(when, i, 'temp', '8310-5', 'Body temperature').Value := makeQty(data.Temperature, '\u00b0C', 'Cel');
  baseVitals(when, i, 'hr', '8867-4', 'Heart Rate').Value := makeQty(inttostr(data.HeartRate), 'BPM', '/min');
  obs := baseVitals(when, i, 'bp', '85354-9', 'Blood pressure');
  component(obs, '8480-6', 'Systolic').Value := makeQty(data.systolic, 'mmhg', 'mm[Hg]');
  component(obs, '8462-4', 'Diastolic').Value := makeQty(data.diastolic, 'mmhg', 'mm[Hg]');
end;

function TVitalSignsGeneratorForm.ProduceData: boolean;
var
  len, freq, points, skip, i, offset : integer;
  start : TDateTime;
  obs : TFhirObservation;
begin
  result := false;
  if not isid(edtPatientId.Text) then
    showMessage('invalid patient id')
  else if not StringIsInteger16(edtSourceLength.Text) then
    showMessage('invalid run length time')
  else if not StringIsInteger16(edtFrequency.Text) then
    showMessage('invalid frequency')
  else
  begin
    loadData;
    len := StrToInt(edtSourceLength.Text);
    freq := StrToInt(edtFrequency.Text);
    points := (len * freq) div 60;
    if points > 150 then
      showMessage('too many data points (150 limit)')
    else
    begin
      FObservations := TFhirBundle.create;
      FObservations.type_ := BundleTypeTransaction;
      start := now;

      for i := 0 to points do
      begin
        offset := trunc((points / 150)) * i;
        if (offset < FData.Count) then
          makeObs(i, start + freq * DATETIME_SECOND_ONE * i, FData[offset]);
      end;

      FClient.transaction(FObservations);
      result := true;
    end;
  end;
end;

procedure TVitalSignsGeneratorForm.SetClient(const Value: TFHIRClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TVitalSignsGeneratorForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
