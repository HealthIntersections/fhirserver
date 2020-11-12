Unit v2_dictionary_v21;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{ WARNING: This is generated code. Do not make any changes here }
{$I fhir.inc}

Interface

Uses
  v2_dictionary;

procedure Definitions21LoadTables(oTables : THL7V2ModelTables);
procedure Definitions21LoadComponents(oComponents : THL7V2ModelComponents);
procedure Definitions21LoadDataElements(oDataElements : THL7V2ModelDataElements);
procedure Definitions21LoadDataTypes(oDataTypes : THL7V2ModelDataTypes);
procedure Definitions21LoadEvents(oEvents : THL7V2ModelEvents);
procedure Definitions21LoadStructures(oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
procedure Definitions21LoadSegments(oSegments : THL7V2ModelSegments);
procedure Definitions21LoadMessageStructures(oStructures : THL7V2ModelMessageStructures);

Implementation

procedure LoadTable0(oTables : THL7V2ModelTables);
Begin
  oTables.Add(0, 'no table');
End;

procedure LoadTable1(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(1, 'SEX');
    oTable.Items.Add(1, 'F', 'Female');
    oTable.Items.Add(2, 'M', 'Male');
    oTable.Items.Add(3, 'O', 'Other');
    oTable.Items.Add(4, 'U', 'Unknown');
End;

procedure LoadTable2(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(2, 'MARITAL STATUS');
    oTable.Items.Add(1, 'A', 'Separated');
    oTable.Items.Add(2, 'D', 'Divorced');
    oTable.Items.Add(3, 'M', 'Married');
    oTable.Items.Add(4, 'S', 'Single');
    oTable.Items.Add(5, 'W', 'Widowed');
End;

procedure LoadTable3(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(3, 'EVENT TYPE CODE');
    oTable.Items.Add(1, 'A01', 'Admit a patient');
    oTable.Items.Add(2, 'A02', 'Transfer a Patient');
    oTable.Items.Add(3, 'A03', 'Discharge a Patient');
    oTable.Items.Add(4, 'A04', 'Register a Patient');
    oTable.Items.Add(5, 'A05', 'Pre-admit a Patient');
    oTable.Items.Add(6, 'A06', 'Transfer an outpatient to inpatient');
    oTable.Items.Add(7, 'A07', 'Transfer an Inpatient to Outpatient');
    oTable.Items.Add(8, 'A08', 'Update patient information');
    oTable.Items.Add(9, 'A09', 'Patient departing');
    oTable.Items.Add(10, 'A10', 'Patient arriving');
    oTable.Items.Add(11, 'A11', 'Cancel admit');
    oTable.Items.Add(12, 'A12', 'Cancel transfer');
    oTable.Items.Add(13, 'A13', 'Cancel discharge');
    oTable.Items.Add(14, 'A14', 'Pending admit');
    oTable.Items.Add(0, 'A15', 'Pending transfer');
    oTable.Items.Add(16, 'A16', 'Pending discharge');
    oTable.Items.Add(17, 'A17', 'Swap Patients');
    oTable.Items.Add(18, 'A18', 'Merge patient information');
    oTable.Items.Add(0, 'A19', 'Patient query');
    oTable.Items.Add(20, 'A20', 'Bed status updates');
    oTable.Items.Add(21, 'A21', 'Leave of Absence - Out (leaving)');
    oTable.Items.Add(22, 'A22', 'Leave of Absence - In (returning)');
    oTable.Items.Add(23, 'A23', 'Delete a Patient Record');
    oTable.Items.Add(24, 'A24', 'Link Patient Records');
    oTable.Items.Add(0, 'O01', 'Order message');
    oTable.Items.Add(42, 'O02', 'Order response');
    oTable.Items.Add(43, 'P01', 'Add and update patient account');
    oTable.Items.Add(0, 'P02', 'Purge Patient Accounts');
    oTable.Items.Add(45, 'P03', 'Post detail financial transaction');
    oTable.Items.Add(46, 'P04', 'Generate bills and A/R statements');
    oTable.Items.Add(0, 'Q01', 'Immediate access');
    oTable.Items.Add(48, 'Q02', 'Deferred Access');
    oTable.Items.Add(51, 'R01', 'Unsolicited transmission of requested Observ.');
    oTable.Items.Add(53, 'R03', 'Display oriented results, query/unsol. update');
End;

procedure LoadTable4(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(4, 'PATIENT CLASS');
    oTable.Items.Add(1, 'E', 'Emergency');
    oTable.Items.Add(2, 'I', 'Inpatient');
    oTable.Items.Add(3, 'O', 'Outpatient');
    oTable.Items.Add(4, 'P', 'Preadmit');
End;

procedure LoadTable5(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(5, 'ETHNIC GROUP');
    oTable.Items.Add(0, 'B', 'Black');
    oTable.Items.Add(0, 'C', 'Caucasian');
    oTable.Items.Add(0, 'H', 'Hispanic');
    oTable.Items.Add(0, 'R', 'Oriental');
End;

procedure LoadTable6(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(6, 'RELIGION');
    oTable.Items.Add(0, 'A', 'Atheist');
    oTable.Items.Add(0, 'B', 'Baptist');
    oTable.Items.Add(0, 'C', 'Catholic');
    oTable.Items.Add(0, 'E', 'Episcopalian');
    oTable.Items.Add(0, 'J', 'Judaism');
    oTable.Items.Add(0, 'L', 'Lutheran');
    oTable.Items.Add(0, 'M', 'Church of Latter Day Saints (Mormon)');
    oTable.Items.Add(0, 'N', 'Hindu');
    oTable.Items.Add(0, 'P', 'Protestant');
End;

procedure LoadTable7(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(7, 'ADMISSION TYPE');
    oTable.Items.Add(1, 'A', 'Accident');
    oTable.Items.Add(2, 'E', 'Emergency');
    oTable.Items.Add(3, 'L', 'Labor and Delivery');
    oTable.Items.Add(4, 'R', 'Routine');
End;

procedure LoadTable8(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(8, 'ACKNOWLEDGMENT CODE');
    oTable.Items.Add(1, 'AA', 'Application Accept');
    oTable.Items.Add(2, 'AE', 'Application Error');
    oTable.Items.Add(3, 'AR', 'Application Reject');
End;

procedure LoadTable9(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(9, 'AMBULATORY STATUS');
    oTable.Items.Add(1, 'A0', 'No functional limitations');
    oTable.Items.Add(2, 'A1', 'Ambulates with assistive device');
    oTable.Items.Add(2, 'A2', 'Wheelchair/stretcher bound');
    oTable.Items.Add(4, 'A3', 'Comatose; non-responsive');
    oTable.Items.Add(5, 'A4', 'Disoriented');
    oTable.Items.Add(6, 'A5', 'Vision impaired');
    oTable.Items.Add(8, 'A7', 'Speech impaired');
    oTable.Items.Add(9, 'A8', 'Non-English Speaking');
    oTable.Items.Add(10, 'A9', 'Functional level unknown');
    oTable.Items.Add(11, 'B1', 'Oxygen Therapy');
    oTable.Items.Add(12, 'B2', 'Special Equipment (tunes, IV''s, Catheters)');
    oTable.Items.Add(13, 'B3', 'Amputee');
    oTable.Items.Add(14, 'B4', 'Mastectomy');
    oTable.Items.Add(15, 'B5', 'Paraplegic');
    oTable.Items.Add(16, 'B6', 'Pregnant');
End;

procedure LoadTable10(oTables : THL7V2ModelTables);
Begin
  oTables.Add(10, 'PHYSICIAN ID');
End;

procedure LoadTable11(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(11, 'CHARGING SYSTEM');
    oTable.Items.Add(1, 'R', 'System that received and processed the order');
    oTable.Items.Add(2, 'S', 'System that sent the order');
End;

procedure LoadTable12(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(12, 'STOCK LOCATION');
    oTable.Items.Add(2, 'FL', 'Filled from floor stock');
End;

procedure LoadTable13(oTables : THL7V2ModelTables);
Begin
  oTables.Add(13, 'CHARGEABLE OR NON-CHARGEABLE');
End;

procedure LoadTable14(oTables : THL7V2ModelTables);
Begin
  oTables.Add(14, 'HEIGHT UNIT OF MEASURE');
End;

procedure LoadTable15(oTables : THL7V2ModelTables);
Begin
  oTables.Add(15, 'WEIGHT UNIT OF MEASURE');
End;

procedure LoadTable16(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(16, 'ISOLATION');
    oTable.Items.Add(1, '0', 'Antibiotic Resistance Precautions');
    oTable.Items.Add(2, '1', 'Blood and Needle Precautions');
    oTable.Items.Add(4, '3', 'Neutropenic Precautions');
    oTable.Items.Add(6, '5', 'Respiratory Isolation');
    oTable.Items.Add(7, '6', 'Secretion/Excretion Precautions');
    oTable.Items.Add(9, '8', 'Wound and skin Precautions');
    oTable.Items.Add(10, '9', 'Precautions');
End;

procedure LoadTable17(oTables : THL7V2ModelTables);
Begin
  oTables.Add(17, 'TRANSACTION TYPE');
End;

procedure LoadTable18(oTables : THL7V2ModelTables);
Begin
  oTables.Add(18, 'PATIENT TYPE');
End;

procedure LoadTable19(oTables : THL7V2ModelTables);
Begin
  oTables.Add(19, 'ANESTHESIA CODE');
End;

procedure LoadTable20(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(20, 'UNUSED TABLE');
    oTable.Items.Add(1, '2', 'Moderate Manifestation');
    oTable.Items.Add(3, '3', 'Major Manifestation');
End;

procedure LoadTable21(oTables : THL7V2ModelTables);
Begin
  oTables.Add(21, 'BAD DEBT AGENCY CODE');
End;

procedure LoadTable22(oTables : THL7V2ModelTables);
Begin
  oTables.Add(22, 'BILLING STATUS');
End;

procedure LoadTable23(oTables : THL7V2ModelTables);
Begin
  oTables.Add(23, 'ADMIT SOURCE');
End;

procedure LoadTable24(oTables : THL7V2ModelTables);
Begin
  oTables.Add(24, 'FEE SCHEDULE');
End;

procedure LoadTable25(oTables : THL7V2ModelTables);
Begin
  oTables.Add(25, 'CONFIRMATION CODE');
End;

procedure LoadTable26(oTables : THL7V2ModelTables);
Begin
  oTables.Add(26, 'ORDER EVENT TYPE');
End;

procedure LoadTable27(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(27, 'PRIORITY (COMPONENT 6 QTY/TIMING[735])');
    oTable.Items.Add(2, 'A', 'ASAP. Fill after STAT orders.');
    oTable.Items.Add(11, 'S', 'Stat. Required immediately.');
    oTable.Items.Add(0, 'T', 'Timed collection');
End;

procedure LoadTable28(oTables : THL7V2ModelTables);
Begin
  oTables.Add(28, 'UNUSED TABLE');
End;

procedure LoadTable29(oTables : THL7V2ModelTables);
Begin
  oTables.Add(29, 'UNIT OF MEASURE');
End;

procedure LoadTable30(oTables : THL7V2ModelTables);
Begin
  oTables.Add(30, 'FREQUENCY CODE');
End;

procedure LoadTable31(oTables : THL7V2ModelTables);
Begin
  oTables.Add(31, 'BLOOD BANK - COMPONENT TYPE');
End;

procedure LoadTable32(oTables : THL7V2ModelTables);
Begin
  oTables.Add(32, 'CHARGE/PRICE INDICATOR');
End;

procedure LoadTable33(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(33, 'ROUTE');
    oTable.Items.Add(0, 'AP', 'Apply externally');
    oTable.Items.Add(0, 'CH', 'Chew');
    oTable.Items.Add(0, 'DU', 'Dust');
    oTable.Items.Add(0, 'EA', 'Ear');
    oTable.Items.Add(0, 'EY', 'Eye');
    oTable.Items.Add(0, 'IA', 'Intro-arterial');
    oTable.Items.Add(0, 'ID', 'Intra-dermal');
    oTable.Items.Add(0, 'IF', 'Infiltrate');
    oTable.Items.Add(0, 'IH', 'Inhalation');
    oTable.Items.Add(0, 'IM', 'Intra-muscular');
    oTable.Items.Add(0, 'IN', 'Intra-nasal');
    oTable.Items.Add(0, 'IR', 'Irrigate');
    oTable.Items.Add(0, 'IS', 'Inserted');
    oTable.Items.Add(0, 'IT', 'Intrathecal');
    oTable.Items.Add(0, 'IV', 'Intravenous');
    oTable.Items.Add(0, 'NB', 'Nebulized');
    oTable.Items.Add(0, 'NG', 'Nathogasic');
    oTable.Items.Add(0, 'PA', 'Peri-anally');
    oTable.Items.Add(0, 'PT', 'Paint');
    oTable.Items.Add(0, 'PU', 'IV push');
    oTable.Items.Add(0, 'RC', 'Rectally');
    oTable.Items.Add(0, 'SH', 'Shampoo');
    oTable.Items.Add(0, 'SL', 'Sublingual');
    oTable.Items.Add(0, 'SO', 'Soak');
    oTable.Items.Add(0, 'SS', 'IV soluset');
    oTable.Items.Add(0, 'TP', 'Topically');
    oTable.Items.Add(0, 'WA', 'Wash');
    oTable.Items.Add(0, 'WI', 'Wipe');
End;

procedure LoadTable34(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(34, 'SITE ADMINISTERED');
    oTable.Items.Add(0, 'B', 'Buttock');
    oTable.Items.Add(0, 'L', 'Left arm');
End;

procedure LoadTable35(oTables : THL7V2ModelTables);
Begin
  oTables.Add(35, 'UNIT OF MEASURE FOR THE SOLUTION RATE');
End;

procedure LoadTable36(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(36, 'UNITS OF MEASURE - ISO528,1977');
    oTable.Items.Add(0, 'BT', 'Bottle');
    oTable.Items.Add(0, 'EA', 'Each');
    oTable.Items.Add(0, 'GM', 'Grams');
    oTable.Items.Add(0, 'KG', 'Kilograms');
    oTable.Items.Add(0, 'MEQ', 'Milliequivalent');
    oTable.Items.Add(0, 'MG', 'Milligrams');
    oTable.Items.Add(0, 'OZ', 'Ounces');
    oTable.Items.Add(0, 'SC', 'Square centimeters');
    oTable.Items.Add(0, 'TB', 'Tablet');
    oTable.Items.Add(0, 'VL', 'Vial');
End;

procedure LoadTable37(oTables : THL7V2ModelTables);
Begin
  oTables.Add(37, 'DRUG DOSE UNIT OF MEASURE');
End;

procedure LoadTable38(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(38, 'ORDER STATUS');
    oTable.Items.Add(0, 'CA', 'Order was canceled');
    oTable.Items.Add(0, 'CM', 'Order is completed');
    oTable.Items.Add(0, 'DC', 'Order was discontinued');
    oTable.Items.Add(0, 'ER', 'Error, order not found');
    oTable.Items.Add(0, 'HD', 'Order is on hold');
    oTable.Items.Add(0, 'IP', 'In process, unspecified');
    oTable.Items.Add(0, 'SC', 'In process, scheduled');
End;

procedure LoadTable39(oTables : THL7V2ModelTables);
Begin
  oTables.Add(39, 'UNUSED TABLE');
End;

procedure LoadTable40(oTables : THL7V2ModelTables);
Begin
  oTables.Add(40, 'UNUSED TABLE');
End;

procedure LoadTable41(oTables : THL7V2ModelTables);
Begin
  oTables.Add(41, 'VOLUME UNIT OF MEASURE');
End;

procedure LoadTable42(oTables : THL7V2ModelTables);
Begin
  oTables.Add(42, 'INS. COMPANY PLAN CODE');
End;

procedure LoadTable43(oTables : THL7V2ModelTables);
Begin
  oTables.Add(43, 'CONDITION');
End;

procedure LoadTable44(oTables : THL7V2ModelTables);
Begin
  oTables.Add(44, 'CONTRACT CODE');
End;

procedure LoadTable45(oTables : THL7V2ModelTables);
Begin
  oTables.Add(45, 'COURTESY CODE');
End;

procedure LoadTable46(oTables : THL7V2ModelTables);
Begin
  oTables.Add(46, 'CREDIT RATING');
End;

procedure LoadTable47(oTables : THL7V2ModelTables);
Begin
  oTables.Add(47, 'DANGER CODE');
End;

procedure LoadTable48(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(48, 'WHAT SUBJECT FILTER');
    oTable.Items.Add(0, 'ADV', 'Advice/Diagnosis');
    oTable.Items.Add(0, 'ANU', 'Nursing Unit Look up');
    oTable.Items.Add(0, 'APN', 'Patient name look up');
    oTable.Items.Add(0, 'CAN', 'Cancel. Used to cancel a query');
    oTable.Items.Add(0, 'DEM', 'Demographics');
    oTable.Items.Add(0, 'MRI', 'Most recent inpatient');
    oTable.Items.Add(0, 'MRO', 'Most recent outpatient');
    oTable.Items.Add(0, 'OTH', 'Other');
    oTable.Items.Add(0, 'PRO', 'Procedure');
    oTable.Items.Add(0, 'RES', 'Result');
    oTable.Items.Add(0, 'STA', 'Status');
End;

procedure LoadTable49(oTables : THL7V2ModelTables);
Begin
  oTables.Add(49, 'DEPARTMENT CODE');
End;

procedure LoadTable50(oTables : THL7V2ModelTables);
Begin
  oTables.Add(50, 'ACCIDENT CODE');
End;

procedure LoadTable51(oTables : THL7V2ModelTables);
Begin
  oTables.Add(51, 'DIAGNOSIS CODE');
End;

procedure LoadTable52(oTables : THL7V2ModelTables);
Begin
  oTables.Add(52, 'DIAGNOSIS TYPE');
End;

procedure LoadTable53(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(53, 'DIAGNOSIS CODING METHOD');
    oTable.Items.Add(0, 'I9', 'ICD9');
End;

procedure LoadTable55(oTables : THL7V2ModelTables);
Begin
  oTables.Add(55, 'DRG CODE');
End;

procedure LoadTable56(oTables : THL7V2ModelTables);
Begin
  oTables.Add(56, 'DRG GROUPER REVIEW CODE');
End;

procedure LoadTable57(oTables : THL7V2ModelTables);
Begin
  oTables.Add(57, 'DRUG CODE');
End;

procedure LoadTable58(oTables : THL7V2ModelTables);
Begin
  oTables.Add(58, 'UNUSED TABLE');
End;

procedure LoadTable59(oTables : THL7V2ModelTables);
Begin
  oTables.Add(59, 'CONSENT CODE');
End;

procedure LoadTable60(oTables : THL7V2ModelTables);
Begin
  oTables.Add(60, 'ERROR CODE');
End;

procedure LoadTable61(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(61, 'CHECK DIGIT SCHEME');
    oTable.Items.Add(0, 'M11', 'Mod 11 check digit scheme');
End;

procedure LoadTable62(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(62, 'EVENT REASON');
    oTable.Items.Add(0, '01', 'Patient Request');
    oTable.Items.Add(0, '02', 'Physician Order');
End;

procedure LoadTable63(oTables : THL7V2ModelTables);
Begin
  oTables.Add(63, 'RELATIONSHIP');
End;

procedure LoadTable64(oTables : THL7V2ModelTables);
Begin
  oTables.Add(64, 'FINANCIAL CLASS');
End;

procedure LoadTable65(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(65, 'ACTION CODE');
    oTable.Items.Add(0, 'A', 'Add ordered tests to the existing specimen');
    oTable.Items.Add(0, 'C', 'Cancel order for battery or tests named');
    oTable.Items.Add(0, 'G', 'Generated order');
    oTable.Items.Add(0, 'L', 'Lab to obtain specimen from patient.');
    oTable.Items.Add(0, 'O', 'Specimen obtained by service other than Lab');
    oTable.Items.Add(0, 'P', 'Pending specimen-Order sent prior to delivery');
    oTable.Items.Add(0, 'S', 'Schedule the tests specified below');
End;

procedure LoadTable66(oTables : THL7V2ModelTables);
Begin
  oTables.Add(66, 'EMPLOYMENT STATUS');
End;

procedure LoadTable67(oTables : THL7V2ModelTables);
Begin
  oTables.Add(67, 'UNUSED TABLE');
End;

procedure LoadTable68(oTables : THL7V2ModelTables);
Begin
  oTables.Add(68, 'GUARANTOR TYPE');
End;

procedure LoadTable69(oTables : THL7V2ModelTables);
Begin
  oTables.Add(69, 'HOSPITAL SERVICE');
End;

procedure LoadTable70(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(70, 'SOURCE OF SPECIMEN');
    oTable.Items.Add(0, 'BLD', 'Blood');
    oTable.Items.Add(0, 'BON', 'Bone');
    oTable.Items.Add(0, 'BRN', 'Burn');
    oTable.Items.Add(0, 'CNJT', 'Conjunctiva');
    oTable.Items.Add(0, 'CSF', 'Cerebral spinal fluid');
    oTable.Items.Add(0, 'CVX', 'Cervix');
    oTable.Items.Add(0, 'EAR', 'Ear');
    oTable.Items.Add(0, 'FIB', 'Fibroblood');
    oTable.Items.Add(0, 'HAR', 'Hair');
    oTable.Items.Add(0, 'MN', 'Amniotic Fluid');
    oTable.Items.Add(0, 'NOS', 'Nose');
    oTable.Items.Add(0, 'OTH', 'Other');
    oTable.Items.Add(0, 'PLAS', 'Plasma');
    oTable.Items.Add(0, 'PRT', 'Peritoneal Fluid');
    oTable.Items.Add(0, 'RBC', 'Erythrocytes');
    oTable.Items.Add(0, 'SAL', 'Saliva');
    oTable.Items.Add(0, 'SEM', 'Seminal Fluid');
    oTable.Items.Add(0, 'SER', 'Serum');
    oTable.Items.Add(0, 'SKN', 'Skin');
    oTable.Items.Add(0, 'SNV', 'Synovial Fluid');
    oTable.Items.Add(0, 'STL', 'Stool');
    oTable.Items.Add(0, 'SWT', 'Sweat');
    oTable.Items.Add(0, 'THRT', 'Throat');
    oTable.Items.Add(0, 'TIS', 'Tissue');
    oTable.Items.Add(0, 'UMB', 'Umbilical Blood');
    oTable.Items.Add(0, 'UR', 'Urine');
    oTable.Items.Add(0, 'URTH', 'Urethra');
    oTable.Items.Add(0, 'WBC', 'Leukocytes');
    oTable.Items.Add(0, 'WND', 'Wound');
End;

procedure LoadTable71(oTables : THL7V2ModelTables);
Begin
  oTables.Add(71, 'UNUSED TABLE');
End;

procedure LoadTable72(oTables : THL7V2ModelTables);
Begin
  oTables.Add(72, 'INS. PLAN ID');
End;

procedure LoadTable73(oTables : THL7V2ModelTables);
Begin
  oTables.Add(73, 'INTEREST RATE CODE');
End;

procedure LoadTable74(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(74, 'DIAGNOSTIC SERVICE SECTION ID');
    oTable.Items.Add(0, 'BG', 'Blood gases');
    oTable.Items.Add(0, 'CH', 'Chemistry');
    oTable.Items.Add(0, 'CP', 'Cytopathology');
    oTable.Items.Add(0, 'CT', 'CAT scan');
    oTable.Items.Add(0, 'CUS', 'Cardiac Ultrasound');
    oTable.Items.Add(0, 'EC', 'Electrocardiac (e.g., EKG, EEC, Holter)');
    oTable.Items.Add(0, 'HM', 'Hematology');
    oTable.Items.Add(0, 'IMM', 'Immunology');
    oTable.Items.Add(0, 'MB', 'Microbiology');
    oTable.Items.Add(0, 'MCB', 'Mycobacteriology');
    oTable.Items.Add(0, 'MYC', 'Mycology');
    oTable.Items.Add(0, 'NMR', 'Nuclear magnetic resonance');
    oTable.Items.Add(0, 'NMS', 'Nuclear medicine scan');
    oTable.Items.Add(0, 'NRS', 'Nursing service measures');
    oTable.Items.Add(0, 'OT', 'Occupational Therapy');
    oTable.Items.Add(0, 'OTH', 'Other');
    oTable.Items.Add(0, 'OUS', 'OB Ultrasound');
    oTable.Items.Add(0, 'PHR', 'Pharmacy');
    oTable.Items.Add(0, 'PT', 'Physical Therapy');
    oTable.Items.Add(0, 'RC', 'Respiratory Care');
    oTable.Items.Add(0, 'RT', 'Radiation Therapy');
    oTable.Items.Add(0, 'RUS', 'Radiology ultrasound');
    oTable.Items.Add(0, 'SP', 'Surgical Pathology');
    oTable.Items.Add(0, 'SR', 'Serology');
    oTable.Items.Add(0, 'TX', 'Toxicology');
    oTable.Items.Add(0, 'VUS', 'Vascular Ultrasound');
    oTable.Items.Add(0, 'XRC', 'Cineradiography');
End;

procedure LoadTable75(oTables : THL7V2ModelTables);
Begin
  oTables.Add(75, 'REPORT TYPES');
End;

procedure LoadTable76(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(76, 'MESSAGE TYPE');
    oTable.Items.Add(0, 'ACK', 'General Acknowledgment       CNT       II');
    oTable.Items.Add(0, 'ARD', 'Ancillary RPT (display)      ANR       VII');
    oTable.Items.Add(0, 'BAR', 'Add/change billing account   BLN       VI');
    oTable.Items.Add(0, 'DSR', 'Display response             QRY       V');
    oTable.Items.Add(0, 'MCF', 'Delayed acknowledgment       CNT       II');
    oTable.Items.Add(0, 'ORF', 'Observ. Result/record resp.  ANR       VII');
    oTable.Items.Add(0, 'ORM', 'Order                        ORD       IV');
    oTable.Items.Add(0, 'ORR', 'Order response message       ORD       IV');
    oTable.Items.Add(0, 'ORU', 'Observ. result/unsolicited   ANR       VII');
    oTable.Items.Add(0, 'OSQ', 'Order status query           ORD       IV');
    oTable.Items.Add(0, 'UDM', 'Unsolicited display          QRY       V');
End;

procedure LoadTable77(oTables : THL7V2ModelTables);
Begin
  oTables.Add(77, 'UNUSED TABLE');
End;

procedure LoadTable78(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(78, 'ABNORMAL FLAGS');
    oTable.Items.Add(0, '<', 'Below absolute low-off instrument scale');
    oTable.Items.Add(0, 'A', 'Abnormal (applies to non-numeric results)');
    oTable.Items.Add(0, 'AA', 'Very abnormal');
    oTable.Items.Add(0, 'D', 'Significant change down');
    oTable.Items.Add(0, 'H', 'Above high normal');
    oTable.Items.Add(0, 'HH', 'Above upper panic limits');
    oTable.Items.Add(0, 'I', 'Interval');
    oTable.Items.Add(0, 'LL', 'Below lower panic limits');
    oTable.Items.Add(0, 'MS', 'Moderately sensitive');
    oTable.Items.Add(0, 'null', 'No range defined,or normal ranges don''t apply');
    oTable.Items.Add(0, 'R', 'Resists');
    oTable.Items.Add(0, 'S', 'Sensitive');
    oTable.Items.Add(0, 'U', 'Significant change up');
    oTable.Items.Add(0, 'VS', 'Very sensitive');
End;

procedure LoadTable79(oTables : THL7V2ModelTables);
Begin
  oTables.Add(79, 'LOCATION');
End;

procedure LoadTable80(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(80, 'NATURE OF ABNORMAL TESTING');
    oTable.Items.Add(0, 'A', 'An aged based population');
    oTable.Items.Add(0, 'N', 'None - generic normal range');
    oTable.Items.Add(0, 'R', 'A race based population');
    oTable.Items.Add(0, 'S', 'A sexed based population');
End;

procedure LoadTable81(oTables : THL7V2ModelTables);
Begin
  oTables.Add(81, 'NOTICE OF ADMISSION');
End;

procedure LoadTable82(oTables : THL7V2ModelTables);
Begin
  oTables.Add(82, 'UNUSED TABLE');
End;

procedure LoadTable83(oTables : THL7V2ModelTables);
Begin
  oTables.Add(83, 'OUTLIER TYPE');
End;

procedure LoadTable84(oTables : THL7V2ModelTables);
Begin
  oTables.Add(84, 'PERFORMED BY CODE');
End;

procedure LoadTable85(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(85, 'OBSERVATION RESULT STATUS');
    oTable.Items.Add(0, 'D', 'Delete previously transmitted observation');
    oTable.Items.Add(0, 'F', 'Complete/final results (entered and verified)');
    oTable.Items.Add(0, 'I', 'Specimen in lab--results pending');
    oTable.Items.Add(0, 'R', 'Results entered - not verified');
    oTable.Items.Add(0, 'S', 'Partial results');
End;

procedure LoadTable86(oTables : THL7V2ModelTables);
Begin
  oTables.Add(86, 'INS. PLAN TYPE');
End;

procedure LoadTable87(oTables : THL7V2ModelTables);
Begin
  oTables.Add(87, 'PRE-ADMIT TESTING');
End;

procedure LoadTable88(oTables : THL7V2ModelTables);
Begin
  oTables.Add(88, 'PROCEDURE CODE');
End;

procedure LoadTable89(oTables : THL7V2ModelTables);
Begin
  oTables.Add(89, 'PROCEDURE CODING METHOD');
End;

procedure LoadTable90(oTables : THL7V2ModelTables);
Begin
  oTables.Add(90, 'PROCEDURE TYPE');
End;

procedure LoadTable91(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(91, 'QUERY PRIORITY');
    oTable.Items.Add(0, 'D', 'Deferred');
    oTable.Items.Add(0, 'I', 'Immediate');
End;

procedure LoadTable92(oTables : THL7V2ModelTables);
Begin
  oTables.Add(92, 'RE-ADMISSION INDICATOR');
End;

procedure LoadTable93(oTables : THL7V2ModelTables);
Begin
  oTables.Add(93, 'RELEASE OF INFORMATION');
End;

procedure LoadTable94(oTables : THL7V2ModelTables);
Begin
  oTables.Add(94, 'REPORT OF ELIGIBILITY');
End;

procedure LoadTable95(oTables : THL7V2ModelTables);
Begin
  oTables.Add(95, 'UNUSED TABLE');
End;

procedure LoadTable96(oTables : THL7V2ModelTables);
Begin
  oTables.Add(96, 'FINANCIAL TRANSACTION CODE');
End;

procedure LoadTable97(oTables : THL7V2ModelTables);
Begin
  oTables.Add(97, 'UNUSED TABLE');
End;

procedure LoadTable98(oTables : THL7V2ModelTables);
Begin
  oTables.Add(98, 'TYPE OF AGREEMENT CODE');
End;

procedure LoadTable99(oTables : THL7V2ModelTables);
Begin
  oTables.Add(99, 'VIP INDICATOR');
End;

procedure LoadTable100(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(100, 'WHEN TO CHARGE');
    oTable.Items.Add(0, 'D', 'On discharge');
    oTable.Items.Add(0, 'O', 'On receipt of order');
    oTable.Items.Add(0, 'R', 'At time service is completed');
    oTable.Items.Add(0, 'S', 'At time service is started');
End;

procedure LoadTable101(oTables : THL7V2ModelTables);
Begin
  oTables.Add(101, 'DISPLAY LEVEL');
End;

procedure LoadTable102(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(102, 'DELAYED ACKNOWLEDGMENT TYPE');
    oTable.Items.Add(0, 'D', 'Message Received, stored for later processing');
End;

procedure LoadTable103(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(103, 'PROCESSING ID');
    oTable.Items.Add(0, 'D', 'Debugging');
    oTable.Items.Add(0, 'P', 'Production');
    oTable.Items.Add(0, 'T', 'Training');
End;

procedure LoadTable104(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(104, 'VERSION CONTROL TABLE');
    oTable.Items.Add(0, '2.0', 'Release 2.0  September 1988');
    oTable.Items.Add(0, '2.0D', 'Demo    2.0  October 1988');
    oTable.Items.Add(0, '2.1', 'Release 2.1  March 1990');
End;

procedure LoadTable105(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(105, 'SOURCE OF COMMENT');
    oTable.Items.Add(0, 'L', 'Ancillary department is source of comment');
    oTable.Items.Add(0, 'P', 'Orderer is source of comment');
End;

procedure LoadTable106(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(106, 'QUERY FORMAT CODE');
    oTable.Items.Add(0, 'R', 'Response in Record-oriented format');
End;

procedure LoadTable107(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(107, 'DEFERRED RESPONSE TYPE');
    oTable.Items.Add(0, 'L', 'Later than the DATE/TIME specified');
End;

procedure LoadTable108(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(108, 'QUERY RESULTS LEVEL');
    oTable.Items.Add(0, 'O', 'Order plus order status');
    oTable.Items.Add(0, 'S', 'Status only');
    oTable.Items.Add(0, 'T', 'Full Results');
End;

procedure LoadTable109(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(109, 'REPORT PRIORITY');
    oTable.Items.Add(0, 'R', 'Routine');
    oTable.Items.Add(0, 'S', 'Stat');
End;

procedure LoadTable110(oTables : THL7V2ModelTables);
Begin
  oTables.Add(110, 'TRANSFER TO BAD DEBT CODE');
End;

procedure LoadTable111(oTables : THL7V2ModelTables);
Begin
  oTables.Add(111, 'DELETE ACCOUNT CODE');
End;

procedure LoadTable112(oTables : THL7V2ModelTables);
Begin
  oTables.Add(112, 'DISCHARGED DISPOSITION');
End;

procedure LoadTable113(oTables : THL7V2ModelTables);
Begin
  oTables.Add(113, 'DISCHARGED TO LOCATION');
End;

procedure LoadTable114(oTables : THL7V2ModelTables);
Begin
  oTables.Add(114, 'DIET TYPE');
End;

procedure LoadTable115(oTables : THL7V2ModelTables);
Begin
  oTables.Add(115, 'SERVICING FACILITY');
End;

procedure LoadTable116(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(116, 'BED STATUS');
    oTable.Items.Add(0, 'C', 'Closed');
    oTable.Items.Add(0, 'H', 'Housekeeping');
    oTable.Items.Add(0, 'O', 'Occupied');
End;

procedure LoadTable117(oTables : THL7V2ModelTables);
Begin
  oTables.Add(117, 'ACCOUNT STATUS');
End;

procedure LoadTable118(oTables : THL7V2ModelTables);
Begin
  oTables.Add(118, 'MAJOR DIAGNOSTIC CATEGORY');
End;

procedure LoadTable119(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(119, 'ORDER CONTROL');
    oTable.Items.Add(0, 'CA', 'Cancel order request');
    oTable.Items.Add(0, 'CH', 'Child order');
    oTable.Items.Add(0, 'CN', 'Combined result');
    oTable.Items.Add(0, 'DC', 'Discontinue order request');
    oTable.Items.Add(0, 'DE', 'Data Errors');
    oTable.Items.Add(0, 'DR', 'Discontinued as requested');
    oTable.Items.Add(0, 'HD', 'Hold order request');
    oTable.Items.Add(0, 'HR', 'On hold as requested');
    oTable.Items.Add(0, 'NA', 'Number assigned            T');
    oTable.Items.Add(0, 'NW', 'New order                  T');
    oTable.Items.Add(0, 'OD', 'Order discontinued');
    oTable.Items.Add(0, 'OK', 'Order accepted and OK');
    oTable.Items.Add(0, 'OR', 'Released as requested');
    oTable.Items.Add(0, 'PA', 'Parent order');
    oTable.Items.Add(0, 'RE', 'Observations to follow');
    oTable.Items.Add(0, 'RO', 'Replacement order');
    oTable.Items.Add(0, 'RP', 'Order replace request');
    oTable.Items.Add(0, 'RR', 'Request received');
    oTable.Items.Add(0, 'RU', 'Replaced unsolicited');
    oTable.Items.Add(0, 'SN', 'Send filler number            F         I');
    oTable.Items.Add(0, 'SS', 'Send order status request');
    oTable.Items.Add(0, 'UD', 'Unable to discontinue');
    oTable.Items.Add(0, 'UH', 'Unable to put on hold');
    oTable.Items.Add(0, 'UR', 'Unable to release');
    oTable.Items.Add(0, 'UX', 'Unable to change');
    oTable.Items.Add(0, 'XR', 'Changed as requested');
    oTable.Items.Add(0, 'XX', 'Order changed, unsolicited');
End;

procedure LoadTable120(oTables : THL7V2ModelTables);
Begin
  oTables.Add(120, 'APPLICATION ID');
End;

procedure LoadTable121(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(121, 'RESPONSE FLAG');
    oTable.Items.Add(0, 'E', 'Report exceptions only.');
    oTable.Items.Add(0, 'F', 'Same as D, plus confirmations explicitly.');
    oTable.Items.Add(0, 'N', 'Only the MSA segment is returned.');
End;

procedure LoadTable122(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(122, 'CHARGE TYPE');
    oTable.Items.Add(0, 'CH', 'Charge');
    oTable.Items.Add(0, 'CO', 'Contract');
    oTable.Items.Add(0, 'CR', 'Credit');
    oTable.Items.Add(0, 'DP', 'Department');
    oTable.Items.Add(0, 'GR', 'Grant');
    oTable.Items.Add(0, 'NC', 'No Charge');
    oTable.Items.Add(0, 'PC', 'Professional');
    oTable.Items.Add(0, 'RS', 'Research');
End;

procedure LoadTable123(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(123, 'RESULT STATUS - OBR');
    oTable.Items.Add(0, 'C', 'Correction of previously transmitted results');
    oTable.Items.Add(0, 'F', 'Final results - results stored & verified');
    oTable.Items.Add(0, 'I', 'Specimen in lab, not yet processed.');
    oTable.Items.Add(0, 'P', 'Preliminary results');
    oTable.Items.Add(0, 'R', 'Results stored - not yet verified');
    oTable.Items.Add(0, 'S', 'Procedure scheduled, not done');
    oTable.Items.Add(0, 'Y', 'No order on record for this test');
    oTable.Items.Add(0, 'Z', 'No record of this patient');
End;

procedure LoadTable124(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(124, 'TRANSPORTATION MODE');
    oTable.Items.Add(0, 'PORT', 'The examining device goes to Patient''s Loc.');
    oTable.Items.Add(0, 'WALK', 'Patient walks to diagnostic service');
    oTable.Items.Add(0, 'WHLC', 'Wheelchair');
End;

procedure LoadTable125(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(125, 'VALUE TYPE');
    oTable.Items.Add(0, 'AD', 'Address');
    oTable.Items.Add(0, 'CK', 'Composite ID with check digit');
    oTable.Items.Add(0, 'FT', 'Formatted Text');
    oTable.Items.Add(0, 'PN', 'Person name');
    oTable.Items.Add(0, 'ST', 'String data. Used to transmit numerics.');
    oTable.Items.Add(0, 'TM', 'Time');
    oTable.Items.Add(0, 'TS', 'Time stamp');
    oTable.Items.Add(0, 'TX', 'Text');
End;

procedure LoadTable126(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(126, 'QUANTITY LIMITED REQUEST');
    oTable.Items.Add(0, 'CH', 'Characters');
    oTable.Items.Add(0, 'LI', 'Lines');
    oTable.Items.Add(0, 'PG', 'Pages');
    oTable.Items.Add(0, 'ZO', 'Locally defined');
End;

procedure LoadTable136(oTables : THL7V2ModelTables);
Begin
  oTables.Add(136, 'Y/N Indicator');
End;

procedure LoadTable145(oTables : THL7V2ModelTables);
Begin
  oTables.Add(145, 'Room Type');
End;

procedure LoadTable171(oTables : THL7V2ModelTables);
Var
  oTable : THL7V2ModelTable;
Begin
  oTable := oTables.Add(171, 'Country Code');
    oTable.Items.Add(0, 'D', 'Germany');
End;

procedure Definitions21LoadTables(oTables : THL7V2ModelTables);
Begin
  LoadTable0(oTables);
  LoadTable1(oTables);
  LoadTable2(oTables);
  LoadTable3(oTables);
  LoadTable4(oTables);
  LoadTable5(oTables);
  LoadTable6(oTables);
  LoadTable7(oTables);
  LoadTable8(oTables);
  LoadTable9(oTables);
  LoadTable10(oTables);
  LoadTable11(oTables);
  LoadTable12(oTables);
  LoadTable13(oTables);
  LoadTable14(oTables);
  LoadTable15(oTables);
  LoadTable16(oTables);
  LoadTable17(oTables);
  LoadTable18(oTables);
  LoadTable19(oTables);
  LoadTable20(oTables);
  LoadTable21(oTables);
  LoadTable22(oTables);
  LoadTable23(oTables);
  LoadTable24(oTables);
  LoadTable25(oTables);
  LoadTable26(oTables);
  LoadTable27(oTables);
  LoadTable28(oTables);
  LoadTable29(oTables);
  LoadTable30(oTables);
  LoadTable31(oTables);
  LoadTable32(oTables);
  LoadTable33(oTables);
  LoadTable34(oTables);
  LoadTable35(oTables);
  LoadTable36(oTables);
  LoadTable37(oTables);
  LoadTable38(oTables);
  LoadTable39(oTables);
  LoadTable40(oTables);
  LoadTable41(oTables);
  LoadTable42(oTables);
  LoadTable43(oTables);
  LoadTable44(oTables);
  LoadTable45(oTables);
  LoadTable46(oTables);
  LoadTable47(oTables);
  LoadTable48(oTables);
  LoadTable49(oTables);
  LoadTable50(oTables);
  LoadTable51(oTables);
  LoadTable52(oTables);
  LoadTable53(oTables);
  LoadTable55(oTables);
  LoadTable56(oTables);
  LoadTable57(oTables);
  LoadTable58(oTables);
  LoadTable59(oTables);
  LoadTable60(oTables);
  LoadTable61(oTables);
  LoadTable62(oTables);
  LoadTable63(oTables);
  LoadTable64(oTables);
  LoadTable65(oTables);
  LoadTable66(oTables);
  LoadTable67(oTables);
  LoadTable68(oTables);
  LoadTable69(oTables);
  LoadTable70(oTables);
  LoadTable71(oTables);
  LoadTable72(oTables);
  LoadTable73(oTables);
  LoadTable74(oTables);
  LoadTable75(oTables);
  LoadTable76(oTables);
  LoadTable77(oTables);
  LoadTable78(oTables);
  LoadTable79(oTables);
  LoadTable80(oTables);
  LoadTable81(oTables);
  LoadTable82(oTables);
  LoadTable83(oTables);
  LoadTable84(oTables);
  LoadTable85(oTables);
  LoadTable86(oTables);
  LoadTable87(oTables);
  LoadTable88(oTables);
  LoadTable89(oTables);
  LoadTable90(oTables);
  LoadTable91(oTables);
  LoadTable92(oTables);
  LoadTable93(oTables);
  LoadTable94(oTables);
  LoadTable95(oTables);
  LoadTable96(oTables);
  LoadTable97(oTables);
  LoadTable98(oTables);
  LoadTable99(oTables);
  LoadTable100(oTables);
  LoadTable101(oTables);
  LoadTable102(oTables);
  LoadTable103(oTables);
  LoadTable104(oTables);
  LoadTable105(oTables);
  LoadTable106(oTables);
  LoadTable107(oTables);
  LoadTable108(oTables);
  LoadTable109(oTables);
  LoadTable110(oTables);
  LoadTable111(oTables);
  LoadTable112(oTables);
  LoadTable113(oTables);
  LoadTable114(oTables);
  LoadTable115(oTables);
  LoadTable116(oTables);
  LoadTable117(oTables);
  LoadTable118(oTables);
  LoadTable119(oTables);
  LoadTable120(oTables);
  LoadTable121(oTables);
  LoadTable122(oTables);
  LoadTable123(oTables);
  LoadTable124(oTables);
  LoadTable125(oTables);
  LoadTable126(oTables);
  LoadTable136(oTables);
  LoadTable145(oTables);
  LoadTable171(oTables);
End;

procedure Definitions21LoadDataTypes(oDataTypes : THL7V2ModelDataTypes);
Begin
  oDataTypes.Add('AD', 'ADDRESS', 0);
  oDataTypes.Add('CE', 'CODED ELEMENT', 0);
  oDataTypes.Add('CK', 'COMPOSITE ID W/CHK DIGIT', 0);
  oDataTypes.Add('CM', 'COMPOSITE', 0);
  oDataTypes.Add('CN', 'COMPOSITE ID AND NAME', 0);
  oDataTypes.Add('CQ', 'COMPOSITE QUANTITY/UNITS', 0);
  oDataTypes.Add('DT', 'DATE', 8);
  oDataTypes.Add('FT', 'FORMATTED TEXT', 0);
  oDataTypes.Add('ID', 'CODED VALUE', 0);
  oDataTypes.Add('NM', 'NUMERIC', 0);
  oDataTypes.Add('PN', 'PERSON NAME', 48);
  oDataTypes.Add('SI', 'SET ID', 4);
  oDataTypes.Add('ST', 'STRING', 0);
  oDataTypes.Add('TM', 'TIME', 6);
  oDataTypes.Add('TN', 'TELEPHONE NUMBER', 0);
  oDataTypes.Add('TS', 'TIME STAMP', 19);
  oDataTypes.Add('TX', 'TEXT', 0);
End;

procedure Definitions21LoadComponents(oComponents : THL7V2ModelComponents);
Begin
  oComponents.Add('Saal', 'ST', 0, 1);
  oComponents.Add('Stuhl', 'ST', 0, 2);
  oComponents.Add('Tisch', 'ST', 0, 3);
  oComponents.Add('ACKNOWLEDGMENT CODE', 'ST', 0, 5);
  oComponents.Add('MESSAGE CONTROL ID', 'ST', 0, 6);
  oComponents.Add('TEXT MESSAGE', 'ST', 0, 7);
  oComponents.Add('FIELD SEPARATOR', 'ST', 0, 8);
  oComponents.Add('SENDING APPLICATION', 'ST', 0, 9);
  oComponents.Add('SECURITY', 'ST', 0, 11);
  oComponents.Add('RECEIVING APPLICATION', 'ST', 0, 12);
  oComponents.Add('DATE/TIME OF MESSAGE', 'ST', 0, 13);
  oComponents.Add('MESSAGE TYPE', 'ST', 0, 15);
  oComponents.Add('MESSAGE CONTROL ID', 'ST', 0, 16);
  oComponents.Add('PROCESSING ID', 'ST', 0, 17);
  oComponents.Add('VERSION ID', 'ST', 0, 18);
  oComponents.Add('PATIENT ADDRESS', 'ST', 0, 23);
  oComponents.Add('COUNTY CODE', 'ST', 0, 29);
  oComponents.Add('EVENT TYPE CODE', 'ST', 0, 32);
  oComponents.Add('DATE/TIME OF EVENT', 'ST', 0, 33);
  oComponents.Add('DATE/TIME PLANNED EVENT', 'ST', 0, 34);
  oComponents.Add('PATIENT ID (INTERNAL ID)', 'ST', 0, 35);
  oComponents.Add('PATIENT ACCOUNT NUMBER', 'ST', 0, 36);
  oComponents.Add('ALTERNATE PATIENT ID', 'ST', 0, 37);
  oComponents.Add('PATIENT NAME', 'ST', 0, 38);
  oComponents.Add('SEX', 'ST', 0, 39);
  oComponents.Add('DATE OF BIRTH', 'ST', 0, 40);
  oComponents.Add('ETHNIC GROUP', 'ST', 0, 41);
  oComponents.Add('RELIGION', 'ST', 0, 42);
  oComponents.Add('MARITAL STATUS', 'ST', 0, 43);
  oComponents.Add('NEXT OF KIN RELATIONSHIP', 'ST', 0, 44);
  oComponents.Add('NEXT OF KIN NAME', 'ST', 0, 45);
  oComponents.Add('PHONE NUMBER - HOME', 'ST', 0, 46);
  oComponents.Add('PHONE NUMBER - BUSINESS', 'ST', 0, 47);
  oComponents.Add('PATIENT CLASS', 'ST', 0, 48);
  oComponents.Add('ASSIGNED PATIENT LOCATION', 'ST', 0, 49);
  oComponents.Add('PRIOR PATIENT LOCATION', 'ST', 0, 50);
  oComponents.Add('ATTENDING DOCTOR', 'ST', 0, 51);
  oComponents.Add('HOSPITAL SERVICE', 'ST', 0, 52);
  oComponents.Add('TEMPORARY LOCATION', 'ST', 0, 53);
  oComponents.Add('ADMIT SOURCE', 'ST', 0, 54);
  oComponents.Add('AMBULATORY STATUS', 'ST', 0, 55);
  oComponents.Add('WHEN TO CHARGE', 'ST', 0, 56);
  oComponents.Add('STOCK LOCATION', 'ST', 0, 57);
  oComponents.Add('ERROR CODE AND LOCATION', 'ST', 0, 58);
  oComponents.Add('SUBSTITUTE ALLOWED', 'ST', 0, 59);
  oComponents.Add('ROUTE', 'ST', 0, 60);
  oComponents.Add('IV SOLUTION RATE', 'ST', 0, 61);
  oComponents.Add('DRUG STRENGTH', 'ST', 0, 62);
  oComponents.Add('DRUG DOSE', 'ST', 0, 63);
  oComponents.Add('FINAL CONCENTRATION', 'ST', 0, 64);
  oComponents.Add('FINAL VOLUME IN ML.', 'ST', 0, 65);
  oComponents.Add('DRUG ROLE', 'ST', 0, 66);
  oComponents.Add('DATA LINE', 'ST', 0, 67);
  oComponents.Add('LOGICAL BREAK POINT', 'ST', 0, 69);
  oComponents.Add('QUERY DATE/TIME', 'ST', 0, 70);
  oComponents.Add('QUERY FORMAT CODE', 'ST', 0, 71);
  oComponents.Add('QUERY PRIORITY', 'ST', 0, 72);
  oComponents.Add('QUERY ID', 'ST', 0, 73);
  oComponents.Add('DEFERRED RESPONSE TYPE', 'ST', 0, 74);
  oComponents.Add('DEFERRED RESPONSE DATE/TIME', 'ST', 0, 75);
  oComponents.Add('CONTINUATION POINTER', 'ST', 0, 77);
  oComponents.Add('WHO SUBJECT FILTER', 'ST', 0, 78);
  oComponents.Add('WHAT DEPARTMENT DATA CODE', 'ST', 0, 80);
  oComponents.Add('WHAT DATA CODE VALUE QUAL.', 'ST', 0, 81);
  oComponents.Add('WHERE SUBJECT FILTER', 'ST', 0, 82);
  oComponents.Add('WHEN DATA START DATE/TIME', 'ST', 0, 83);
  oComponents.Add('WHEN DATA END DATE/TIME', 'ST', 0, 84);
  oComponents.Add('WHAT USER QUALIFIER', 'ST', 0, 85);
  oComponents.Add('OTHER QRY SUBJECT FILTER', 'ST', 0, 86);
  oComponents.Add('ACCIDENT DATE/TIME', 'ST', 0, 87);
  oComponents.Add('ACCIDENT CODE', 'ST', 0, 88);
  oComponents.Add('ACCIDENT LOCATION', 'ST', 0, 89);
  oComponents.Add('ADMITTING DOCTOR', 'ST', 0, 90);
  oComponents.Add('CONTRACT CODE', 'ST', 0, 97);
  oComponents.Add('CONTRACT EFFECTIVE DATE', 'ST', 0, 98);
  oComponents.Add('CONTRACT AMOUNT', 'ST', 0, 99);
  oComponents.Add('CONTRACT PERIOD', 'ST', 0, 100);
  oComponents.Add('TRANSFER TO BAD DEBT CODE', 'ST', 0, 101);
  oComponents.Add('BAD DEBT AGENCY CODE', 'ST', 0, 102);
  oComponents.Add('DELETE ACCOUNT INDICATOR', 'ST', 0, 103);
  oComponents.Add('DELETE ACCOUNT DATE', 'ST', 0, 104);
  oComponents.Add('ADMISSION TYPE', 'ST', 0, 105);
  oComponents.Add('PRE-ADMIT NUMBER', 'ST', 0, 106);
  oComponents.Add('PRE-ADMIT TEST INDICATOR', 'ST', 0, 107);
  oComponents.Add('RE-ADMISSION INDICATOR', 'ST', 0, 108);
  oComponents.Add('NEXT OF KIN - ADDRESS', 'ST', 0, 109);
  oComponents.Add('NEXT OF KIN - PHONE NUMBER', 'ST', 0, 110);
  oComponents.Add('SET ID - INSURANCE', 'ST', 0, 111);
  oComponents.Add('INSURANCE COMPANY ID', 'ST', 0, 112);
  oComponents.Add('INSURANCE COMPANY NAME', 'ST', 0, 113);
  oComponents.Add('INSURANCE COMPANY ADDRESS', 'ST', 0, 114);
  oComponents.Add('INSURANCE CO. CONTACT PERS', 'ST', 0, 115);
  oComponents.Add('INSURANCE CO PHONE NUMBER', 'ST', 0, 116);
  oComponents.Add('GROUP NUMBER', 'ST', 0, 117);
  oComponents.Add('GROUP NAME', 'ST', 0, 118);
  oComponents.Add('INSURED''S GROUP EMP. ID', 'ST', 0, 119);
  oComponents.Add('INSURED''S GROUP EMP. NAME', 'ST', 0, 120);
  oComponents.Add('PLAN EFFECTIVE DATE', 'ST', 0, 121);
  oComponents.Add('PLAN EXPIRATION DATE', 'ST', 0, 122);
  oComponents.Add('AUTHORIZATION INFORMATION', 'ST', 0, 123);
  oComponents.Add('PLAN TYPE', 'ST', 0, 124);
  oComponents.Add('NAME OF INSURED', 'ST', 0, 125);
  oComponents.Add('INSURED''S RELATIONSHIP TO PATIENT', 'ST', 0, 126);
  oComponents.Add('ASSIGNMENT OF BENEFITS', 'ST', 0, 127);
  oComponents.Add('COORDINATION OF BENEFITS', 'ST', 0, 128);
  oComponents.Add('COORD OF BEN. PRIORITY', 'ST', 0, 129);
  oComponents.Add('NOTICE OF ADMISSION CODE', 'ST', 0, 130);
  oComponents.Add('NOTICE OF ADMISSION DATE', 'ST', 0, 131);
  oComponents.Add('RPT OF ELIGIBILITY CODE', 'ST', 0, 132);
  oComponents.Add('RPT OF ELIGIBILITY DATE', 'ST', 0, 133);
  oComponents.Add('RELEASE INFORMATION CODE', 'ST', 0, 134);
  oComponents.Add('PRE-ADMIT CERT. (PAC)', 'ST', 0, 135);
  oComponents.Add('VERIFICATION DATE', 'ST', 0, 136);
  oComponents.Add('VERIFICATION BY', 'ST', 0, 137);
  oComponents.Add('TYPE OF AGREEMENT CODE', 'ST', 0, 138);
  oComponents.Add('BILLING STATUS', 'ST', 0, 139);
  oComponents.Add('BLOOD DEDUCTIBLE', 'ST', 0, 140);
  oComponents.Add('LIFETIME RESERVE DAYS', 'ST', 0, 141);
  oComponents.Add('DELAY BEFORE L. R. DAY', 'ST', 0, 142);
  oComponents.Add('COMPANY PLAN CODE', 'ST', 0, 143);
  oComponents.Add('identifier', 'ID', 0, 144);
  oComponents.Add('text', 'ST', 0, 145);
  oComponents.Add('name of coding system', 'ST', 0, 146);
  oComponents.Add('alternate identifier', 'ST', 0, 147);
  oComponents.Add('alternate text', 'ST', 0, 148);
  oComponents.Add('name of alternate coding system', 'ST', 0, 149);
End;

procedure Definitions21LoadStructures(oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
Var
  oStructure : THL7V2ModelStructure;
Begin
  oStructures.Add('PN', 'person name', 'PN', 0);
  oStructures.Add('CQ', 'composite quantity with units', 'CQ', 0);
  oStructures.Add('COMP_QUANT', 'COMPOSITE QUANTITY/UNITS', 'CQ', 0);
  oStructures.Add('COMP_ID_NAME', 'COMPOSITE ID AND NAME', 'CN', 0);
  oStructures.Add('COMP_ID_DIGIT', 'COMPOSITE ID W/CHK DIGIT', 'CK', 0);
  oStructures.Add('CN', 'composite ID number and name', 'CN', 0);
  oStructures.Add('CM_UNDEFINED', 'undefined CM data type', 'CM', 0);
  oStructures.Add('CM', '', 'CM', 0);
  oStructures.Add('CK', 'composite ID with check digit', 'CK', 0);
  oStructures.Add('CE_0057', '', 'CE', 0);
  oStructure := oStructures.Add('CE', 'coded element', 'CE', 0);
    oStructure.Components.Add(oComponents.GetByNumber(144).Link);
    oStructure.Components.Add(oComponents.GetByNumber(145).Link);
    oStructure.Components.Add(oComponents.GetByNumber(146).Link);
    oStructure.Components.Add(oComponents.GetByNumber(147).Link);
    oStructure.Components.Add(oComponents.GetByNumber(148).Link);
    oStructure.Components.Add(oComponents.GetByNumber(149).Link);
  oStructures.Add('AD', 'address', 'AD', 0);
  oStructures.Add('TX', 'text data', 'TX', 1);
  oStructures.Add('TS', 'time stamp', 'TS', 1);
  oStructures.Add('TN', 'telephone number', 'TN', 1);
  oStructures.Add('TM', 'time', 'TM', 1);
  oStructures.Add('ST', 'string data', 'ST', 1);
  oStructures.Add('SI', 'SET ID', 'SI', 1);
  oStructures.Add('SET_ID', 'SET ID', 'SI', 1);
  oStructures.Add('NM', 'numeric', 'NM', 1);
  oStructures.Add('ID', 'CODED VALUE', 'ID', 1);
  oStructures.Add('FT', 'formatted text data', 'FT', 1);
  oStructures.Add('DT', 'DATE', 'DT', 1);
End;

procedure LoadDataElements1(oDataElements : THL7V2ModelDataElements);
Begin
  oDataElements.Add('ACKNOWLEDGMENT CODE', 2, 'ID', 2, 8);
  oDataElements.Add('MESSAGE CONTROL ID', 3, 'ST', 20, 0);
  oDataElements.Add('TEXT MESSAGE', 4, 'ST', 80, 0);
  oDataElements.Add('FIELD SEPARATOR', 5, 'ST', 1, 0);
  oDataElements.Add('SENDING APPLICATION', 6, 'ST', 15, 0);
  oDataElements.Add('Security', 8, 'ST', 40, 0);
  oDataElements.Add('RECEIVING APPLICATION', 9, 'ST', 15, 0);
  oDataElements.Add('DATE/TIME OF MESSAGE', 10, 'TS', 19, 0);
  oDataElements.Add('MESSAGE TYPE', 12, 'ID', 7, 76);
  oDataElements.Add('MESSAGE CONTROL ID', 13, 'ST', 20, 0);
  oDataElements.Add('PROCESSING ID', 14, 'ID', 1, 103);
  oDataElements.Add('VERSION ID', 15, 'NM', 8, 104);
  oDataElements.Add('PATIENT ADDRESS', 20, 'AD', 106, 0);
  oDataElements.Add('COUNTY CODE', 26, 'ID', 4, 0);
  oDataElements.Add('EVENT TYPE CODE', 29, 'ID', 3, 3);
  oDataElements.Add('DATE/TIME OF EVENT', 30, 'TS', 19, 0);
  oDataElements.Add('DATE/TIME PLANNED EVENT', 32, 'TS', 19, 0);
  oDataElements.Add('PATIENT ID INTERNAL (INTERNAL ID)', 34, 'CK', 16, 61);
  oDataElements.Add('PATIENT ACCOUNT NUMBER', 35, 'CK', 20, 61);
  oDataElements.Add('ALTERNATE PATIENT ID', 38, 'ST', 12, 0);
  oDataElements.Add('PATIENT NAME', 41, 'PN', 48, 0);
  oDataElements.Add('SEX', 42, 'ID', 1, 1);
  oDataElements.Add('DATE OF BIRTH', 43, 'DT', 8, 0);
  oDataElements.Add('ETHNIC GROUP', 44, 'ID', 1, 5);
  oDataElements.Add('RELIGION', 45, 'ID', 3, 6);
  oDataElements.Add('MARITAL STATUS', 46, 'ID', 1, 2);
  oDataElements.Add('NEXT OF KIN RELATIONSHIP', 47, 'ST', 15, 63);
  oDataElements.Add('NEXT OF KIN NAME', 48, 'PN', 48, 0);
  oDataElements.Add('PHONE NUMBER - HOME', 49, 'TN', 40, 0);
  oDataElements.Add('PHONE NUMBER - BUSINESS', 50, 'TN', 40, 0);
  oDataElements.Add('PATIENT CLASS', 52, 'ID', 1, 4);
  oDataElements.Add('ASSIGNED PATIENT LOCATION', 53, 'ID', 12, 79);
  oDataElements.Add('PRIOR PATIENT LOCATION', 56, 'ID', 12, 79);
  oDataElements.Add('ATTENDING DOCTOR', 57, 'CN', 60, 10);
  oDataElements.Add('HOSPITAL SERVICE', 59, 'ID', 3, 69);
  oDataElements.Add('TEMPORARY LOCATION', 60, 'ID', 12, 79);
  oDataElements.Add('ADMIT SOURCE', 63, 'ID', 3, 23);
  oDataElements.Add('AMBULATORY STATUS', 64, 'ID', 2, 9);
  oDataElements.Add('WHEN TO CHARGE', 66, 'CM', 15, 100);
  oDataElements.Add('STOCK LOCATION', 68, 'ID', 2, 12);
  oDataElements.Add('ERROR CODE AND LOCATION', 80, 'ID', 80, 60);
  oDataElements.Add('SUBSTITUTE ALLOWED', 120, 'ID', 1, 0);
  oDataElements.Add('ROUTE', 129, 'ST', 8, 33);
  oDataElements.Add('SITE ADMINISTERED', 130, 'ST', 20, 34);
  oDataElements.Add('IV SOLUTION RATE', 131, 'CQ', 10, 0);
  oDataElements.Add('DRUG STRENGTH', 133, 'CQ', 14, 0);
  oDataElements.Add('DRUG DOSE', 135, 'CM', 10, 0);
  oDataElements.Add('FINAL CONCENTRATION', 137, 'NM', 10, 0);
  oDataElements.Add('FINAL VOLUME IN ML.', 138, 'NM', 10, 0);
  oDataElements.Add('DRUG ROLE', 139, 'ID', 1, 0);
  oDataElements.Add('DATA LINE', 153, 'TX', 300, 0);
  oDataElements.Add('LOGICAL BREAK POINT', 154, 'ST', 2, 0);
  oDataElements.Add('QUERY DATE/TIME', 156, 'TS', 19, 0);
  oDataElements.Add('QUERY FORMAT CODE', 158, 'ID', 1, 106);
  oDataElements.Add('QUERY PRIORITY', 159, 'ID', 1, 91);
  oDataElements.Add('QUERY ID', 160, 'ST', 10, 0);
  oDataElements.Add('DEFERRED RESPONSE TYPE', 161, 'ID', 1, 107);
  oDataElements.Add('DEFERRED RESPONSE DATE/TIME', 162, 'TS', 19, 0);
  oDataElements.Add('QUANTITY LIMITED REQUEST', 164, 'CQ', 5, 126);
  oDataElements.Add('CONTINUATION POINTER', 167, 'ST', 60, 0);
  oDataElements.Add('WHO SUBJECT FILTER', 168, 'ST', 20, 0);
  oDataElements.Add('WHAT SUBJECT FILTER', 169, 'ID', 3, 48);
  oDataElements.Add('WHAT DEPARTMENT DATA CODE', 170, 'ST', 20, 0);
  oDataElements.Add('WHAT DATA CODE VALUE QUAL.', 171, 'ST', 20, 0);
  oDataElements.Add('WHERE SUBJECT FILTER', 173, 'ST', 20, 0);
  oDataElements.Add('WHEN DATA START DATE/TIME', 174, 'TS', 19, 0);
  oDataElements.Add('WHEN DATA END DATE/TIME', 176, 'TS', 19, 0);
  oDataElements.Add('WHAT USER QUALIFIER', 178, 'ST', 20, 0);
  oDataElements.Add('OTHER QRY SUBJECT FILTER', 179, 'ST', 20, 0);
  oDataElements.Add('ACCIDENT DATE/TIME', 182, 'TS', 19, 0);
  oDataElements.Add('ACCIDENT CODE', 184, 'ID', 2, 50);
  oDataElements.Add('ACCIDENT LOCATION', 185, 'ST', 25, 0);
  oDataElements.Add('ADMITTING DOCTOR', 189, 'CN', 60, 10);
  oDataElements.Add('PATIENT TYPE', 191, 'ID', 2, 18);
  oDataElements.Add('VIP INDICATOR', 193, 'ID', 2, 99);
  oDataElements.Add('VISIT NUMBER', 194, 'NM', 4, 0);
  oDataElements.Add('FINANCIAL CLASS', 195, 'ID', 11, 64);
  oDataElements.Add('CHARGE PRICE INDICATOR', 199, 'ID', 2, 32);
  oDataElements.Add('CREDIT RATING', 200, 'ID', 2, 46);
  oDataElements.Add('CONTRACT CODE', 201, 'ID', 2, 44);
  oDataElements.Add('CONTRACT EFFECTIVE DATE', 202, 'DT', 8, 0);
  oDataElements.Add('CONTRACT AMOUNT', 203, 'NM', 12, 0);
  oDataElements.Add('CONTRACT PERIOD', 204, 'NM', 3, 0);
  oDataElements.Add('TRANSFER TO BAD DEBT CODE', 205, 'ID', 1, 110);
  oDataElements.Add('BAD DEBT AGENCY CODE', 206, 'ST', 10, 21);
  oDataElements.Add('DELETE ACCOUNT INDICATOR', 207, 'ID', 1, 111);
  oDataElements.Add('DELETE ACCOUNT DATE', 208, 'DT', 8, 0);
  oDataElements.Add('ADMISSION TYPE', 218, 'ID', 2, 7);
  oDataElements.Add('PRE-ADMIT NUMBER', 219, 'ST', 20, 0);
  oDataElements.Add('PRE-ADMIT TEST INDICATOR', 220, 'ID', 2, 87);
  oDataElements.Add('RE-ADMISSION INDICATOR', 221, 'ID', 2, 92);
  oDataElements.Add('NEXT OF KIN - ADDRESS', 225, 'AD', 106, 0);
  oDataElements.Add('NEXT OF KIN - PHONE NUMBER', 230, 'TN', 40, 0);
  oDataElements.Add('SET ID - INSURANCE', 234, 'SI', 4, 0);
  oDataElements.Add('INSURANCE COMPANY ID', 235, 'ST', 6, 0);
  oDataElements.Add('INSURANCE COMPANY NAME', 236, 'ST', 45, 0);
  oDataElements.Add('INSURANCE COMPANY ADDRESS', 237, 'AD', 106, 0);
  oDataElements.Add('INSURANCE CO. CONTACT PERS', 242, 'PN', 48, 0);
  oDataElements.Add('INSURANCE CO PHONE NUMBER', 243, 'TN', 40, 0);
  oDataElements.Add('GROUP NUMBER', 248, 'ST', 12, 0);
  oDataElements.Add('GROUP NAME', 249, 'ST', 35, 0);
  oDataElements.Add('INSURED''S GROUP EMP. ID', 250, 'ST', 12, 0);
  oDataElements.Add('INSURED''S GROUP EMP. NAME', 251, 'ST', 45, 0);
  oDataElements.Add('PLAN EFFECTIVE DATE', 252, 'DT', 8, 0);
  oDataElements.Add('PLAN EXPIRATION DATE', 253, 'DT', 8, 0);
  oDataElements.Add('AUTHORIZATION INFORMATION', 254, 'ST', 55, 0);
  oDataElements.Add('PLAN TYPE', 260, 'ID', 2, 86);
  oDataElements.Add('NAME OF INSURED', 261, 'PN', 48, 0);
  oDataElements.Add('INSURED''S RELATIONSHIP TO PATIENT', 262, 'ID', 2, 63);
  oDataElements.Add('ASSIGNMENT OF BENEFITS', 263, 'ID', 2, 0);
  oDataElements.Add('COORDINATION OF BENEFITS', 264, 'ID', 2, 0);
  oDataElements.Add('COORD OF BEN. PRIORITY', 265, 'ST', 2, 0);
  oDataElements.Add('NOTICE OF ADMISSION CODE', 266, 'ID', 2, 81);
  oDataElements.Add('NOTICE OF ADMISSION DATE', 267, 'DT', 8, 0);
  oDataElements.Add('RPT OF ELIGIBILITY CODE', 268, 'ID', 2, 94);
  oDataElements.Add('RPT OF ELIGIBILITY DATE', 269, 'DT', 8, 0);
  oDataElements.Add('RELEASE INFORMATION CODE', 270, 'ID', 2, 93);
  oDataElements.Add('PRE-ADMIT CERT. (PAC)', 271, 'ST', 15, 0);
  oDataElements.Add('VERIFICATION DATE', 272, 'DT', 8, 0);
  oDataElements.Add('VERIFICATION BY', 273, 'CM', 60, 0);
  oDataElements.Add('TYPE OF AGREEMENT CODE', 277, 'ID', 2, 98);
  oDataElements.Add('BILLING STATUS', 278, 'ID', 2, 22);
  oDataElements.Add('BLOOD DEDUCTIBLE', 279, 'ST', 1, 0);
  oDataElements.Add('LIFETIME RESERVE DAYS', 280, 'NM', 4, 0);
  oDataElements.Add('DELAY BEFORE L. R. DAY', 281, 'NM', 4, 0);
  oDataElements.Add('COMPANY PLAN CODE', 282, 'ST', 8, 42);
  oDataElements.Add('POLICY NUMBER', 283, 'ST', 15, 0);
  oDataElements.Add('POLICY DEDUCTIBLE', 284, 'NM', 12, 0);
  oDataElements.Add('POLICY LIMIT - AMOUNT', 285, 'NM', 12, 0);
  oDataElements.Add('POLICY LIMIT - DAYS', 286, 'NM', 4, 0);
  oDataElements.Add('ROOM RATE - SEMI-PRIVATE', 287, 'NM', 12, 0);
  oDataElements.Add('ROOM RATE - PRIVATE', 288, 'NM', 12, 0);
  oDataElements.Add('DIAGNOSIS CODE', 293, 'ID', 8, 51);
  oDataElements.Add('DIAGNOSIS DESCRIPTION', 294, 'ST', 40, 0);
  oDataElements.Add('DIAGNOSIS DATE/TIME', 295, 'TS', 19, 0);
  oDataElements.Add('DIAGNOSIS/DRG TYPE', 297, 'ID', 2, 52);
  oDataElements.Add('MAJOR DIAGNOSTIC CATEGORY', 298, 'ST', 4, 118);
  oDataElements.Add('DIAGNOSTIC RELATED GROUP', 299, 'ID', 4, 55);
  oDataElements.Add('OUTLIER DAYS', 300, 'NM', 3, 0);
  oDataElements.Add('SET ID - PROCEDURE', 304, 'SI', 4, 0);
  oDataElements.Add('PROCEDURE CODE', 305, 'ID', 10, 88);
  oDataElements.Add('PROCEDURE DESCRIPTION', 306, 'ST', 40, 0);
  oDataElements.Add('PROCEDURE DATE/TIME', 307, 'TS', 19, 0);
  oDataElements.Add('PROCEDURE TYPE', 309, 'ID', 2, 90);
  oDataElements.Add('PROCEDURE MINUTES', 310, 'NM', 4, 0);
  oDataElements.Add('ANESTHESIOLOGIST', 311, 'CN', 60, 10);
  oDataElements.Add('ANESTHESIA CODE', 313, 'ID', 2, 19);
  oDataElements.Add('ANESTHESIA MINUTES', 314, 'NM', 4, 0);
  oDataElements.Add('SURGEON', 315, 'CN', 60, 10);
  oDataElements.Add('CONSENT CODE', 317, 'ID', 2, 59);
  oDataElements.Add('RESIDENT CODE', 318, 'CN', 60, 10);
  oDataElements.Add('SET ID - GUARANTOR', 321, 'SI', 4, 0);
  oDataElements.Add('GUARANTOR NUMBER', 322, 'ID', 20, 0);
  oDataElements.Add('GUARANTOR NAME', 323, 'PN', 48, 0);
  oDataElements.Add('GUARANTOR ADDRESS', 324, 'AD', 106, 0);
  oDataElements.Add('GUARANTOR PH. NUM.- HOME', 329, 'TN', 40, 0);
  oDataElements.Add('GUARANTOR PH. NUM-BUSINESS', 330, 'TN', 40, 0);
  oDataElements.Add('GUARANTOR DATE OF BIRTH', 331, 'DT', 8, 0);
  oDataElements.Add('GUARANTOR SEX', 332, 'ID', 1, 1);
  oDataElements.Add('GUARANTOR TYPE', 333, 'ID', 2, 68);
  oDataElements.Add('GUARANTOR RELATIONSHIP', 334, 'ID', 2, 63);
  oDataElements.Add('GUARANTOR SSN', 335, 'ST', 11, 0);
  oDataElements.Add('GUARANTOR DATE - BEGIN', 338, 'DT', 8, 0);
  oDataElements.Add('GUARANTOR DATE - END', 339, 'DT', 8, 0);
  oDataElements.Add('GUARANTOR PRIORITY', 340, 'NM', 2, 0);
  oDataElements.Add('GUARANTOR EMPLOYER NAME', 341, 'ST', 45, 0);
  oDataElements.Add('GUARANTOR EMPLOYER ADDRESS', 342, 'AD', 106, 0);
  oDataElements.Add('GUARANTOR EMPLOY PHONE #', 347, 'TN', 40, 0);
  oDataElements.Add('TRANSACTION DATE', 351, 'DT', 8, 0);
  oDataElements.Add('TRANSACTION POSTING DATE', 352, 'DT', 8, 0);
  oDataElements.Add('TRANSACTION TYPE', 353, 'ID', 8, 17);
  oDataElements.Add('TRANSACTION CODE', 354, 'ID', 20, 96);
  oDataElements.Add('DEPARTMENT CODE', 355, 'ST', 16, 49);
  oDataElements.Add('TRANSACTION DESCRIPTION', 356, 'ST', 40, 0);
  oDataElements.Add('TRANSACTION QUANTITY', 357, 'NM', 4, 0);
  oDataElements.Add('TRANSACTION AMOUNT - EXTENDED', 358, 'NM', 12, 0);
  oDataElements.Add('INSURANCE PLAN ID', 359, 'ID', 8, 72);
  oDataElements.Add('INSURANCE AMOUNT', 360, 'NM', 12, 0);
  oDataElements.Add('PATIENT LOCATION', 361, 'ST', 12, 79);
  oDataElements.Add('FEE SCHEDULE', 362, 'ID', 1, 24);
  oDataElements.Add('PATIENT TYPE', 363, 'ID', 2, 18);
  oDataElements.Add('DIAGNOSIS CODE', 364, 'ID', 8, 51);
  oDataElements.Add('TRANSACTION ID', 366, 'ST', 12, 0);
  oDataElements.Add('EVENT REASON CODE', 369, 'ID', 3, 62);
  oDataElements.Add('DRG APPROVAL INDICATOR', 373, 'ID', 2, 0);
  oDataElements.Add('DRG GROUPER REVIEW CODE', 374, 'ID', 2, 56);
  oDataElements.Add('OUTLIER TYPE', 375, 'ID', 2, 83);
  oDataElements.Add('OUTLIER COST', 376, 'NM', 12, 0);
  oDataElements.Add('PERFORMED BY CODE', 377, 'CN', 60, 84);
  oDataElements.Add('INSURANCE PLAN ID', 378, 'ID', 8, 72);
  oDataElements.Add('COURTESY CODE', 386, 'ID', 2, 45);
  oDataElements.Add('INTEREST CODE', 387, 'ID', 2, 73);
  oDataElements.Add('TRANSFER TO BAD DEBT DATE', 388, 'DT', 8, 0);
  oDataElements.Add('BAD DEBT TRANSFER AMOUNT', 389, 'NM', 12, 0);
  oDataElements.Add('BAD DEBT RECOVERY AMOUNT', 390, 'NM', 12, 0);
  oDataElements.Add('GUARANTOR EMPLOYEE ID NUM', 391, 'ST', 20, 0);
  oDataElements.Add('GUARANTOR EMPLOYMENT STATUS', 392, 'ID', 2, 66);
  oDataElements.Add('PROCEDURE CODING METHOD.', 393, 'ID', 2, 89);
  oDataElements.Add('DIAGNOSIS CODING METHOD', 394, 'ID', 2, 53);
  oDataElements.Add('BLOOD FURN.-PINTS OF (40)', 396, 'ST', 2, 0);
  oDataElements.Add('BLOOD REPLACED-PINTS (41)', 397, 'ST', 2, 0);
  oDataElements.Add('BLOOD NOT RPLCD-PINTS(42)', 398, 'ST', 2, 0);
  oDataElements.Add('CO-INSURANCE DAYS (25)', 399, 'ST', 2, 0);
  oDataElements.Add('CONDITION CODE', 400, 'ID', 2, 43);
  oDataElements.Add('COVERED DAYS - (23)', 405, 'ST', 3, 0);
  oDataElements.Add('NON COVERED DAYS - (24)', 406, 'ST', 3, 0);
  oDataElements.Add('VALUE AMOUNT & CODE', 407, 'CM', 12, 0);
  oDataElements.Add('NUMBER OF GRACE DAYS (90)', 424, 'ST', 2, 0);
  oDataElements.Add('SPEC. PROG. INDICATOR(44)', 425, 'ID', 2, 0);
  oDataElements.Add('PSRO/UR APPROVAL IND. (87)', 426, 'ID', 1, 0);
  oDataElements.Add('PSRO/UR APRVD STAY-FM(88)', 427, 'DT', 8, 0);
  oDataElements.Add('PSRO/UR APRVD STAY-TO(89)', 428, 'DT', 8, 0);
  oDataElements.Add('OCCURRENCE (28-32)', 429, 'ID', 20, 0);
  oDataElements.Add('OCCURRENCE SPAN (33)', 435, 'ID', 2, 0);
  oDataElements.Add('OCCURRENCE SPAN START DATE(33)', 446, 'DT', 8, 0);
  oDataElements.Add('OCCUR. SPAN END DATE (33)', 447, 'DT', 8, 0);
  oDataElements.Add('UB-82 LOCATOR 2', 448, 'ST', 30, 0);
  oDataElements.Add('UB-82 LOCATOR 9', 449, 'ST', 7, 0);
  oDataElements.Add('UB-82 LOCATOR 27', 450, 'ST', 8, 0);
  oDataElements.Add('UB-82 LOCATOR 45', 451, 'ST', 17, 0);
  oDataElements.Add('DRIVER''S LIC NUM - PATIENT', 453, 'CM', 25, 0);
  oDataElements.Add('SSN NUMBER - PATIENT', 457, 'ST', 16, 0);
  oDataElements.Add('SET ID - PATIENT VISIT', 458, 'SI', 4, 0);
  oDataElements.Add('SET ID - UB82', 459, 'SI', 4, 0);
  oDataElements.Add('LANGUAGE - PATIENT', 464, 'ST', 25, 0);
  oDataElements.Add('PRESCRIPTION SEQUENCE #', 469, 'NM', 3, 0);
  oDataElements.Add('QUANTITY DISPENSED', 470, 'CQ', 4, 0);
  oDataElements.Add('DRUG ID', 473, 'CE_0057', 5, 57);
  oDataElements.Add('COMPONENT DRUG IDS', 474, 'ID', 5, 0);
  oDataElements.Add('PRESCRIPTION TYPE', 479, 'ID', 2, 0);
  oDataElements.Add('SUBSTITUTION STATUS', 480, 'ID', 1, 0);
  oDataElements.Add('NUMBER OF REFILLS', 481, 'NM', 3, 0);
  oDataElements.Add('REFILLS REMAINING', 482, 'NM', 3, 0);
  oDataElements.Add('LAST REFILL DATE/TIME', 483, 'TS', 19, 0);
  oDataElements.Add('PHARMACY INSTRUCTIONS', 484, 'TX', 80, 0);
  oDataElements.Add('PATIENT INSTRUCTIONS', 489, 'TX', 80, 0);
  oDataElements.Add('TRANSACTION BATCH ID', 503, 'ST', 5, 0);
  oDataElements.Add('SET ID - DIAGNOSIS', 506, 'SI', 4, 0);
  oDataElements.Add('SET ID - FINANCIAL TRANSACTION', 507, 'SI', 4, 0);
  oDataElements.Add('ENCODING CHARACTERS', 509, 'ST', 4, 0);
  oDataElements.Add('SENDING FACILITY', 512, 'ST', 20, 0);
  oDataElements.Add('RECEIVING FACILITY', 513, 'ST', 30, 0);
  oDataElements.Add('SET ID - OBSERVATION REQUEST', 520, 'SI', 4, 0);
  oDataElements.Add('UNIVERSAL SERVICE IDENT.', 523, 'CE', 200, 0);
  oDataElements.Add('PRIORITY', 524, 'ST', 2, 0);
  oDataElements.Add('REQUESTED DATE-TIME', 529, 'TS', 19, 0);
  oDataElements.Add('OBSERVATION DATE/TIME', 530, 'TS', 19, 0);
  oDataElements.Add('OBSERVATION END DATE/TIME', 531, 'TS', 19, 0);
  oDataElements.Add('COLLECTION VOLUME', 532, 'CQ', 20, 36);
  oDataElements.Add('COLLECTOR IDENTIFIER', 533, 'CN', 60, 0);
  oDataElements.Add('SPECIMEN ACTION CODE', 534, 'ST', 1, 65);
  oDataElements.Add('DANGER CODE', 535, 'CM', 60, 47);
  oDataElements.Add('RELEVANT CLINICAL INFO.', 536, 'ST', 300, 0);
  oDataElements.Add('SPECIMEN RECEIVED DATE/TIME', 537, 'TS', 19, 0);
  oDataElements.Add('SPECIMEN SOURCE', 538, 'CM', 300, 70);
  oDataElements.Add('ORDERING PROVIDER', 539, 'CN', 60, 10);
  oDataElements.Add('ORDER CALL-BACK PHONE NUM', 540, 'TN', 40, 0);
  oDataElements.Add('PLACERS FIELD #1', 541, 'ST', 60, 0);
  oDataElements.Add('PLACERS FIELD #2', 542, 'ST', 60, 0);
  oDataElements.Add('FILLERS FIELD #1', 543, 'ST', 60, 0);
  oDataElements.Add('FILLERS FIELD #2', 544, 'ST', 60, 0);
  oDataElements.Add('RESULTS RPT/STATUS CHNG - DATE/T', 546, 'TS', 19, 0);
  oDataElements.Add('CHARGE TO PRACTICE', 547, 'CM', 40, 0);
  oDataElements.Add('DIAGNOSTIC SERV SECT ID', 548, 'ID', 10, 74);
  oDataElements.Add('LINKED RESULTS', 550, 'CE', 200, 0);
  oDataElements.Add('RESULT COPIES TO', 551, 'CN', 80, 0);
  oDataElements.Add('SET ID - OBSERVATION SIMPLE', 559, 'SI', 4, 0);
  oDataElements.Add('OBSERVATION IDENTIFIER', 560, 'CE', 80, 0);
  oDataElements.Add('OBSERVATION RESULTS', 561, 'ST', 65, 0);
  oDataElements.Add('UNITS', 562, 'ID', 20, 0);
  oDataElements.Add('REFERENCES RANGE', 563, 'ST', 60, 0);
  oDataElements.Add('ABNORMAL FLAGS', 564, 'ST', 10, 78);
  oDataElements.Add('NATURE OF ABNORMAL TEST', 565, 'ID', 5, 80);
  oDataElements.Add('OBSERV RESULT STATUS', 566, 'ID', 2, 85);
  oDataElements.Add('DATE LAST OBS NORMAL VALUES', 567, 'TS', 19, 0);
  oDataElements.Add('SET ID - DISPLAY DATA', 570, 'SI', 4, 0);
  oDataElements.Add('DISPLAY LEVEL', 571, 'SI', 4, 0);
  oDataElements.Add('SET ID - PATIENT ID', 572, 'SI', 4, 0);
  oDataElements.Add('SET ID - NOTES AND COMMENTS', 573, 'SI', 4, 0);
  oDataElements.Add('SOURCE OF COMMENT', 574, 'ID', 8, 105);
  oDataElements.Add('COMMENT', 575, 'TX', 120, 0);
  oDataElements.Add('PRIOR PATIENT ID - INTERNAL', 576, 'CK', 16, 61);
  oDataElements.Add('PRIOR ALTERNATE PATIENT ID', 577, 'CK', 16, 61);
  oDataElements.Add('PRIOR PATIENT ACCOUNT NUMBER', 578, 'CK', 20, 61);
  oDataElements.Add('REFERRING DOCTOR', 579, 'CN', 60, 10);
  oDataElements.Add('CONSULTING DOCTOR', 580, 'CN', 60, 10);
  oDataElements.Add('PATIENT ID EXTERNAL (EXTERNAL ID)', 581, 'CK', 16, 61);
  oDataElements.Add('MOTHER''S MAIDEN NAME', 582, 'ST', 30, 0);
  oDataElements.Add('RESULTS COPIES TO', 586, 'CN', 80, 0);
  oDataElements.Add('RX ORDER STATUS', 588, 'ID', 2, 38);
  oDataElements.Add('RX NUMBER', 596, 'ST', 20, 0);
  oDataElements.Add('PATIENT ALIAS', 597, 'PN', 48, 0);
  oDataElements.Add('EXPECTED SEQUENCE NUMBER', 598, 'NM', 15, 0);
  oDataElements.Add('RESULT ID', 599, 'TX', 20, 0);
  oDataElements.Add('R/U DATE/TIME', 600, 'TS', 19, 0);
  oDataElements.Add('REPORT PRIORITY', 601, 'ID', 1, 109);
  oDataElements.Add('R/U WHO SUBJECT DEFINITION', 602, 'ST', 20, 0);
  oDataElements.Add('R/U WHAT SUBJECT DEFINITION', 603, 'ID', 3, 48);
  oDataElements.Add('R/U WHAT DEPARTMENT CODE', 605, 'ST', 20, 0);
  oDataElements.Add('R/U DISPLAY/PRINT LOCATIONS', 607, 'ST', 20, 0);
  oDataElements.Add('R/U WHERE SUBJECT DEFINITION', 608, 'ST', 20, 0);
  oDataElements.Add('R/U WHEN DATA START DATE/TIME', 609, 'TS', 19, 0);
  oDataElements.Add('R/U WHEN DATA END DATE/TIME', 610, 'TS', 19, 0);
  oDataElements.Add('R/U WHAT USER QUALIFIER', 611, 'ST', 20, 0);
  oDataElements.Add('R/U OTHER RESULTS SUBJECT DEFINI', 612, 'ST', 20, 0);
  oDataElements.Add('DISCHARGE DISPOSITION', 613, 'ID', 2, 112);
  oDataElements.Add('DISCHARGED TO LOCATION', 614, 'ID', 2, 113);
  oDataElements.Add('DIET TYPE', 615, 'ID', 2, 114);
  oDataElements.Add('SERVICING FACILITY', 616, 'ID', 2, 115);
  oDataElements.Add('BED STATUS', 617, 'ID', 1, 116);
  oDataElements.Add('INSTRUCTIONS (SIG)', 618, 'TX', 500, 0);
  oDataElements.Add('DEA CLASS', 619, 'ID', 5, 0);
  oDataElements.Add('ORDERING MD''S DEA NUMBER', 620, 'NM', 10, 0);
  oDataElements.Add('PRN STATUS', 621, 'ID', 5, 0);
  oDataElements.Add('TRANSPORTATION MODE', 625, 'ID', 20, 124);
  oDataElements.Add('REASON FOR STUDY', 626, 'CE', 300, 0);
  oDataElements.Add('PRINCIPAL RESULT INTERPRETER', 627, 'CN', 60, 0);
  oDataElements.Add('ASSISTANT RESULT INTERPRETER', 628, 'CN', 60, 0);
  oDataElements.Add('TRANSCRIPTIONIST', 629, 'CN', 60, 0);
  oDataElements.Add('TECHNICIAN', 630, 'CN', 60, 0);
  oDataElements.Add('DELAYED ACKNOWLEDGMENT TYPE', 632, 'ID', 1, 102);
  oDataElements.Add('SEQUENCE NUMBER', 633, 'NM', 15, 0);
  oDataElements.Add('PROBABILITY', 639, 'NM', 5, 0);
  oDataElements.Add('ADDENDUM CONTINUATION POINTER', 641, 'ST', 60, 0);
  oDataElements.Add('BATCH CREATION DATE/TIME', 655, 'TS', 19, 0);
  oDataElements.Add('BATCH NAME/ID/TYPE', 656, 'ST', 20, 0);
  oDataElements.Add('BATCH COMMENT', 657, 'ST', 80, 0);
  oDataElements.Add('BATCH CONTROL ID', 658, 'ST', 20, 0);
  oDataElements.Add('REFERENCE BATCH CONTROL ID', 659, 'ST', 20, 0);
  oDataElements.Add('DATE/TIME OF FILE CREATION', 660, 'TS', 19, 0);
  oDataElements.Add('FILE NAME/ID', 661, 'ST', 20, 0);
  oDataElements.Add('FILE HEADER COMMENT', 662, 'ST', 80, 0);
  oDataElements.Add('FILE CONTROL ID', 663, 'ST', 20, 0);
  oDataElements.Add('BATCH MESSAGE COUNT', 664, 'ST', 10, 0);
  oDataElements.Add('BATCH COMMENT', 665, 'ST', 80, 0);
  oDataElements.Add('BATCH TOTALS', 666, 'CM', 100, 0);
  oDataElements.Add('FILE BATCH COUNT', 667, 'ST', 10, 0);
  oDataElements.Add('FILE TRAILER COMMENT', 668, 'ST', 80, 0);
  oDataElements.Add('BED STATUS', 671, 'ID', 1, 116);
  oDataElements.Add('VALUE TYPE', 676, 'ID', 2, 125);
  oDataElements.Add('BATCH FIELD SEPARATOR', 685, 'ST', 1, 0);
  oDataElements.Add('BATCH ENCODING CHARACTERS', 686, 'ST', 3, 0);
  oDataElements.Add('BATCH SENDING APPLICATION', 687, 'ST', 15, 0);
  oDataElements.Add('BATCH SENDING FACILITY', 688, 'ST', 20, 0);
  oDataElements.Add('BATCH RECEIVING APPLICATION', 689, 'ST', 15, 0);
  oDataElements.Add('BATCH RECEIVING FACILITY', 690, 'ST', 20, 0);
  oDataElements.Add('BATCH SECURITY', 691, 'ST', 40, 0);
  oDataElements.Add('FILE FIELD SEPARATOR', 692, 'ST', 1, 0);
  oDataElements.Add('FILE ENCODING CHARACTERS', 693, 'ST', 4, 0);
  oDataElements.Add('FILE SENDING APPLICATION', 694, 'ST', 15, 0);
  oDataElements.Add('FILE SENDING FACILITY', 695, 'ST', 20, 0);
  oDataElements.Add('FILE RECEIVING APPLICATION', 696, 'ST', 15, 0);
  oDataElements.Add('FILE RECEIVING FACILITY', 697, 'ST', 20, 0);
  oDataElements.Add('FILE SECURITY', 698, 'ST', 40, 0);
  oDataElements.Add('CONTINUATION POINTER', 699, 'ST', 180, 0);
  oDataElements.Add('QUERY RESULTS LEVEL', 701, 'ID', 1, 108);
  oDataElements.Add('R/U RESULTS LEVEL', 702, 'ID', 1, 108);
  oDataElements.Add('ACCOUNT STATUS', 703, 'ID', 2, 117);
  oDataElements.Add('PENDING LOCATION', 704, 'ID', 12, 79);
  oDataElements.Add('PRIOR TEMPORARY LOCATION', 705, 'ID', 12, 79);
  oDataElements.Add('TRANSACTION DESCRIPTION - ALT', 706, 'ST', 40, 0);
  oDataElements.Add('GUARANTOR SPOUSE NAME', 707, 'PN', 48, 0);
  oDataElements.Add('INSURED''S DATE OF BIRTH', 708, 'DT', 8, 0);
  oDataElements.Add('INSURED''S ADDRESS', 709, 'AD', 106, 0);
  oDataElements.Add('INSURED''S EMPLOYMENT STATUS', 710, 'ID', 1, 66);
  oDataElements.Add('INSURED''S SEX', 711, 'ID', 1, 1);
  oDataElements.Add('SET ID - NEXT OF KIN', 712, 'SI', 4, 0);
  oDataElements.Add('INSURED''S EMPLOYER ADDRESS', 713, 'AD', 106, 0);
  oDataElements.Add('ORDER CONTROL', 714, 'ST', 2, 119);
  oDataElements.Add('PLACER ORDER #', 715, 'CM', 75, 0);
  oDataElements.Add('FILLER ORDER #', 716, 'CM', 75, 0);
  oDataElements.Add('PLACER GROUP #', 717, 'CM', 75, 0);
  oDataElements.Add('ORDER STATUS', 718, 'ST', 2, 38);
  oDataElements.Add('RESPONSE FLAG', 719, 'ST', 1, 121);
  oDataElements.Add('TIMING/QUANTITY', 720, 'CM', 200, 0);
  oDataElements.Add('PARENT', 721, 'CM', 200, 0);
  oDataElements.Add('DATE/TIME OF TRANSACTION', 722, 'TS', 19, 0);
  oDataElements.Add('ENTERED BY', 723, 'CN', 80, 0);
  oDataElements.Add('VERIFIED BY', 724, 'CN', 80, 0);
  oDataElements.Add('ORDERING PROVIDER', 725, 'CN', 80, 0);
  oDataElements.Add('ENTERER''S LOCATION', 726, 'CM', 80, 0);
  oDataElements.Add('CALL BACK PHONE NUMBER', 727, 'TN', 40, 0);
  oDataElements.Add('CHARGE TYPE', 729, 'ID', 50, 122);
  oDataElements.Add('ACCOUNT ID', 730, 'CM', 100, 0);
  oDataElements.Add('ORDER ITEM ID', 731, 'CE', 200, 0);
  oDataElements.Add('PLACER ORDER #', 732, 'CM', 75, 0);
  oDataElements.Add('FILLER ORDER #', 733, 'CM', 75, 0);
  oDataElements.Add('RESULT STATUS', 734, 'ID', 1, 123);
  oDataElements.Add('QUANTITY/TIMING', 735, 'CM', 200, 0);
  oDataElements.Add('SCHEDULED - DATE/TIME', 736, 'TS', 19, 0);
  oDataElements.Add('PARENT ACCESSION #', 737, 'CM', 150, 0);
  oDataElements.Add('SYSTEM DATE/TIME', 742, 'TS', 19, 0);
  oDataElements.Add('STATISTICS AVAILABLE', 743, 'ID', 1, 0);
  oDataElements.Add('SOURCE IDENTIFIER', 744, 'ST', 30, 0);
  oDataElements.Add('SOURCE TYPE', 745, 'ID', 3, 0);
  oDataElements.Add('STATISTICS START', 746, 'TS', 19, 0);
  oDataElements.Add('STATISTICS END', 747, 'TS', 19, 0);
  oDataElements.Add('RECEIVE CHARACTER COUNT', 748, 'NM', 10, 0);
  oDataElements.Add('SEND CHARACTER COUNT', 749, 'NM', 10, 0);
  oDataElements.Add('MESSAGES RECEIVED', 750, 'NM', 10, 0);
  oDataElements.Add('MESSAGES SENT', 751, 'NM', 10, 0);
  oDataElements.Add('CHECKSUM ERRORS RECEIVED', 752, 'NM', 10, 0);
  oDataElements.Add('LENGTH ERRORS RECEIVED', 753, 'NM', 10, 0);
  oDataElements.Add('OTHER ERRORS RECEIVED', 754, 'NM', 10, 0);
  oDataElements.Add('CONNECT TIMEOUTS', 755, 'NM', 10, 0);
  oDataElements.Add('RECEIVE TIMEOUTS', 756, 'NM', 10, 0);
  oDataElements.Add('NETWORK ERRORS', 757, 'NM', 10, 0);
  oDataElements.Add('NETWORK CHANGE TYPE', 758, 'ID', 4, 0);
  oDataElements.Add('CURRENT CPU', 759, 'ST', 30, 0);
  oDataElements.Add('CURRENT FILESERVER', 760, 'ST', 30, 0);
  oDataElements.Add('CURRENT APPLICATION', 761, 'ST', 30, 0);
  oDataElements.Add('CURRENT FACILITY', 762, 'ST', 30, 0);
  oDataElements.Add('NEW CPU', 763, 'ST', 30, 0);
  oDataElements.Add('NEW FILESERVER', 764, 'ST', 30, 0);
  oDataElements.Add('NEW APPLICATION', 765, 'ST', 30, 0);
  oDataElements.Add('NEW FACILITY', 766, 'ST', 30, 0);
  oDataElements.Add('REFERENCE FILE CONTROL ID', 768, 'ST', 20, 0);
  oDataElements.Add('OBSERVATION SUB-ID', 769, 'NM', 20, 0);
  oDataElements.Add('UNUSED', 770, 'ST', 0, 0);
  oDataElements.Add('UNUSED', 771, 'ST', 0, 0);
  oDataElements.Add('UNUSED', 772, 'ST', 0, 0);
  oDataElements.Add('UNUSED', 773, 'ST', 0, 0);
  oDataElements.Add('UNUSED', 774, 'ST', 0, 0);
  oDataElements.Add('ADMIT DATE/TIME', 775, 'TS', 19, 0);
  oDataElements.Add('DISCHARGE DATE/TIME', 776, 'TS', 19, 0);
  oDataElements.Add('CURRENT PATIENT BALANCE', 777, 'NM', 12, 0);
  oDataElements.Add('TOTAL CHARGES', 778, 'NM', 12, 0);
  oDataElements.Add('TOTAL ADJUSTMENTS', 779, 'NM', 12, 0);
  oDataElements.Add('TOTAL PAYMENTS', 780, 'NM', 12, 0);
  oDataElements.Add('GROUPER VERSION AND TYPE', 781, 'ST', 4, 0);
  oDataElements.Add('TRANSACTION AMOUNT - UNIT', 782, 'NM', 12, 0);
  oDataElements.Add('ORDERED BY CODE', 783, 'CN', 60, 0);
  oDataElements.Add('UNIT COST', 784, 'NM', 12, 0);
  oDataElements.Add('BED LOCATION', 785, 'ID', 12, 79);
End;

procedure Definitions21LoadDataElements(oDataElements : THL7V2ModelDataElements);
Begin
  LoadDataElements1(oDataElements);
End;

procedure Definitions21LoadSegments(oSegments : THL7V2ModelSegments);
Var
  oSegment : THL7V2ModelSegment;
Begin
  oSegments.Add('<', 'begin choice');
  oSegments.Add('>', 'end choice');
  oSegment := oSegments.Add('ACC', 'ACCIDENT');
    oSegment.Fields.Add(182, False, 0, False, 1);
    oSegment.Fields.Add(184, False, 0, False, 2);
    oSegment.Fields.Add(185, False, 0, False, 3);
  oSegment := oSegments.Add('ADD', 'ADDENDUM');
    oSegment.Fields.Add(641, False, 0, False, 1);
  oSegment := oSegments.Add('BHS', 'BATCH HEADER');
    oSegment.Fields.Add(685, False, 0, True, 1);
    oSegment.Fields.Add(686, False, 0, True, 2);
    oSegment.Fields.Add(687, False, 0, False, 3);
    oSegment.Fields.Add(688, False, 0, False, 4);
    oSegment.Fields.Add(689, False, 0, False, 5);
    oSegment.Fields.Add(690, False, 0, False, 6);
    oSegment.Fields.Add(655, False, 0, False, 7);
    oSegment.Fields.Add(691, False, 0, False, 8);
    oSegment.Fields.Add(656, False, 0, False, 9);
    oSegment.Fields.Add(657, False, 0, False, 10);
    oSegment.Fields.Add(658, False, 0, False, 11);
    oSegment.Fields.Add(659, False, 0, False, 12);
  oSegment := oSegments.Add('BLG', 'BILLING');
    oSegment.Fields.Add(66, False, 0, False, 1);
    oSegment.Fields.Add(729, False, 0, False, 2);
    oSegment.Fields.Add(730, False, 0, False, 3);
  oSegment := oSegments.Add('BTS', 'BATCH TRAILER');
    oSegment.Fields.Add(664, False, 0, False, 1);
    oSegment.Fields.Add(665, False, 0, False, 2);
    oSegment.Fields.Add(666, False, 0, False, 3);
  oSegment := oSegments.Add('DG1', 'DIAGNOSIS');
    oSegment.Fields.Add(506, False, 0, True, 1);
    oSegment.Fields.Add(394, False, 0, True, 2);
    oSegment.Fields.Add(293, False, 0, False, 3);
    oSegment.Fields.Add(294, False, 0, False, 4);
    oSegment.Fields.Add(295, False, 0, False, 5);
    oSegment.Fields.Add(297, False, 0, True, 6);
    oSegment.Fields.Add(298, False, 0, False, 7);
    oSegment.Fields.Add(299, False, 0, False, 8);
    oSegment.Fields.Add(373, False, 0, False, 9);
    oSegment.Fields.Add(374, False, 0, False, 10);
    oSegment.Fields.Add(375, False, 0, False, 11);
    oSegment.Fields.Add(300, False, 0, False, 12);
    oSegment.Fields.Add(376, False, 0, False, 13);
    oSegment.Fields.Add(781, False, 0, False, 14);
  oSegment := oSegments.Add('DSC', 'CONTINUATION POINTER');
    oSegment.Fields.Add(167, False, 0, False, 1);
  oSegment := oSegments.Add('DSP', 'DISPLAY DATA');
    oSegment.Fields.Add(570, False, 0, False, 1);
    oSegment.Fields.Add(571, False, 0, False, 2);
    oSegment.Fields.Add(153, False, 0, True, 3);
    oSegment.Fields.Add(154, False, 0, False, 4);
    oSegment.Fields.Add(599, False, 0, False, 5);
  oSegment := oSegments.Add('ERR', 'ERROR');
    oSegment.Fields.Add(80, True, 0, True, 1);
  oSegment := oSegments.Add('EVN', 'EVENT TYPE');
    oSegment.Fields.Add(29, False, 0, True, 1);
    oSegment.Fields.Add(30, False, 0, True, 2);
    oSegment.Fields.Add(32, False, 0, False, 3);
    oSegment.Fields.Add(369, False, 0, False, 4);
  oSegment := oSegments.Add('FHS', 'FILE HEADER');
    oSegment.Fields.Add(692, False, 0, True, 1);
    oSegment.Fields.Add(693, False, 0, True, 2);
    oSegment.Fields.Add(694, False, 0, False, 3);
    oSegment.Fields.Add(695, False, 0, False, 4);
    oSegment.Fields.Add(696, False, 0, False, 5);
    oSegment.Fields.Add(697, False, 0, False, 6);
    oSegment.Fields.Add(660, False, 0, False, 7);
    oSegment.Fields.Add(698, False, 0, False, 8);
    oSegment.Fields.Add(661, False, 0, False, 9);
    oSegment.Fields.Add(662, False, 0, False, 10);
    oSegment.Fields.Add(663, False, 0, False, 11);
    oSegment.Fields.Add(768, False, 0, False, 12);
  oSegment := oSegments.Add('FT1', 'FINANCIAL TRANSACTION');
    oSegment.Fields.Add(507, False, 0, False, 1);
    oSegment.Fields.Add(366, False, 0, False, 2);
    oSegment.Fields.Add(503, False, 0, False, 3);
    oSegment.Fields.Add(351, False, 0, True, 4);
    oSegment.Fields.Add(352, False, 0, False, 5);
    oSegment.Fields.Add(353, False, 0, True, 6);
    oSegment.Fields.Add(354, False, 0, True, 7);
    oSegment.Fields.Add(356, False, 0, False, 8);
    oSegment.Fields.Add(706, False, 0, False, 9);
    oSegment.Fields.Add(358, False, 0, False, 10);
    oSegment.Fields.Add(357, False, 0, False, 11);
    oSegment.Fields.Add(782, False, 0, False, 12);
    oSegment.Fields.Add(355, False, 0, False, 13);
    oSegment.Fields.Add(359, False, 0, False, 14);
    oSegment.Fields.Add(360, False, 0, False, 15);
    oSegment.Fields.Add(361, False, 0, False, 16);
    oSegment.Fields.Add(362, False, 0, False, 17);
    oSegment.Fields.Add(363, False, 0, False, 18);
    oSegment.Fields.Add(364, False, 0, False, 19);
    oSegment.Fields.Add(377, False, 0, False, 20);
    oSegment.Fields.Add(783, False, 0, False, 21);
    oSegment.Fields.Add(784, False, 0, False, 22);
  oSegment := oSegments.Add('FTS', 'FILE TRAILER');
    oSegment.Fields.Add(667, False, 0, False, 1);
    oSegment.Fields.Add(668, False, 0, False, 2);
  oSegment := oSegments.Add('GT1', 'GUARANTOR');
    oSegment.Fields.Add(321, False, 0, True, 1);
    oSegment.Fields.Add(322, False, 0, False, 2);
    oSegment.Fields.Add(323, False, 0, True, 3);
    oSegment.Fields.Add(707, False, 0, False, 4);
    oSegment.Fields.Add(324, False, 0, False, 5);
    oSegment.Fields.Add(329, False, 0, False, 6);
    oSegment.Fields.Add(330, False, 0, False, 7);
    oSegment.Fields.Add(331, False, 0, False, 8);
    oSegment.Fields.Add(332, False, 0, False, 9);
    oSegment.Fields.Add(333, False, 0, False, 10);
    oSegment.Fields.Add(334, False, 0, False, 11);
    oSegment.Fields.Add(335, False, 0, False, 12);
    oSegment.Fields.Add(338, False, 0, False, 13);
    oSegment.Fields.Add(339, False, 0, False, 14);
    oSegment.Fields.Add(340, False, 0, False, 15);
    oSegment.Fields.Add(341, False, 0, False, 16);
    oSegment.Fields.Add(342, False, 0, False, 17);
    oSegment.Fields.Add(347, False, 0, False, 18);
    oSegment.Fields.Add(391, False, 0, False, 19);
    oSegment.Fields.Add(392, False, 0, False, 20);
  oSegment := oSegments.Add('IN1', 'INSURANCE');
    oSegment.Fields.Add(234, False, 0, True, 1);
    oSegment.Fields.Add(378, False, 0, True, 2);
    oSegment.Fields.Add(235, False, 0, True, 3);
    oSegment.Fields.Add(236, False, 0, False, 4);
    oSegment.Fields.Add(237, False, 0, False, 5);
    oSegment.Fields.Add(242, False, 0, False, 6);
    oSegment.Fields.Add(243, False, 0, False, 7);
    oSegment.Fields.Add(248, False, 0, False, 8);
    oSegment.Fields.Add(249, False, 0, False, 9);
    oSegment.Fields.Add(250, False, 0, False, 10);
    oSegment.Fields.Add(251, False, 0, False, 11);
    oSegment.Fields.Add(252, False, 0, False, 12);
    oSegment.Fields.Add(253, False, 0, False, 13);
    oSegment.Fields.Add(254, False, 0, False, 14);
    oSegment.Fields.Add(260, False, 0, False, 15);
    oSegment.Fields.Add(261, False, 0, False, 16);
    oSegment.Fields.Add(262, False, 0, False, 17);
    oSegment.Fields.Add(708, False, 0, False, 18);
    oSegment.Fields.Add(709, False, 0, False, 19);
    oSegment.Fields.Add(263, False, 0, False, 20);
    oSegment.Fields.Add(264, False, 0, False, 21);
    oSegment.Fields.Add(265, False, 0, False, 22);
    oSegment.Fields.Add(266, False, 0, False, 23);
    oSegment.Fields.Add(267, False, 0, False, 24);
    oSegment.Fields.Add(268, False, 0, False, 25);
    oSegment.Fields.Add(269, False, 0, False, 26);
    oSegment.Fields.Add(270, False, 0, False, 27);
    oSegment.Fields.Add(271, False, 0, False, 28);
    oSegment.Fields.Add(272, False, 0, False, 29);
    oSegment.Fields.Add(273, False, 0, False, 30);
    oSegment.Fields.Add(277, False, 0, False, 31);
    oSegment.Fields.Add(278, False, 0, False, 32);
    oSegment.Fields.Add(280, False, 0, False, 33);
    oSegment.Fields.Add(281, False, 0, False, 34);
    oSegment.Fields.Add(282, False, 0, False, 35);
    oSegment.Fields.Add(283, False, 0, False, 36);
    oSegment.Fields.Add(284, False, 0, False, 37);
    oSegment.Fields.Add(285, False, 0, False, 38);
    oSegment.Fields.Add(286, False, 0, False, 39);
    oSegment.Fields.Add(287, False, 0, False, 40);
    oSegment.Fields.Add(288, False, 0, False, 41);
    oSegment.Fields.Add(710, False, 0, False, 42);
    oSegment.Fields.Add(711, False, 0, False, 43);
    oSegment.Fields.Add(713, False, 0, False, 44);
  oSegment := oSegments.Add('MRG', 'MERGE PATIENT INFORMATION');
    oSegment.Fields.Add(576, False, 0, True, 1);
    oSegment.Fields.Add(577, False, 0, False, 2);
    oSegment.Fields.Add(578, False, 0, False, 3);
  oSegment := oSegments.Add('MSA', 'MESSAGE ACKNOWLEDGMENT');
    oSegment.Fields.Add(2, False, 0, True, 1);
    oSegment.Fields.Add(3, False, 0, True, 2);
    oSegment.Fields.Add(4, False, 0, False, 3);
    oSegment.Fields.Add(598, False, 0, False, 4);
    oSegment.Fields.Add(632, False, 0, False, 5);
  oSegment := oSegments.Add('MSH', 'MESSAGE HEADER');
    oSegment.Fields.Add(5, False, 0, True, 1);
    oSegment.Fields.Add(509, False, 0, True, 2);
    oSegment.Fields.Add(6, False, 0, False, 3);
    oSegment.Fields.Add(512, False, 0, False, 4);
    oSegment.Fields.Add(9, False, 0, False, 5);
    oSegment.Fields.Add(513, False, 0, False, 6);
    oSegment.Fields.Add(10, False, 0, False, 7);
    oSegment.Fields.Add(8, False, 0, False, 8);
    oSegment.Fields.Add(12, False, 0, True, 9);
    oSegment.Fields.Add(13, False, 0, True, 10);
    oSegment.Fields.Add(14, False, 0, True, 11);
    oSegment.Fields.Add(15, False, 0, True, 12);
    oSegment.Fields.Add(633, False, 0, False, 13);
    oSegment.Fields.Add(699, False, 0, False, 14);
  oSegment := oSegments.Add('NCK', 'SYSTEM CLOCK');
    oSegment.Fields.Add(742, False, 0, True, 1);
  oSegment := oSegments.Add('NK1', 'NEXT OF KIN');
    oSegment.Fields.Add(712, False, 0, True, 1);
    oSegment.Fields.Add(48, False, 0, False, 2);
    oSegment.Fields.Add(47, False, 0, False, 3);
    oSegment.Fields.Add(225, False, 0, False, 4);
    oSegment.Fields.Add(230, True, 0, False, 5);
  oSegment := oSegments.Add('NPU', 'NON-PATIENT UPDATE');
    oSegment.Fields.Add(785, False, 0, True, 1);
    oSegment.Fields.Add(671, False, 0, False, 2);
  oSegment := oSegments.Add('NSC', 'STATUS CHANGE');
    oSegment.Fields.Add(758, False, 0, True, 1);
    oSegment.Fields.Add(759, False, 0, False, 2);
    oSegment.Fields.Add(760, False, 0, False, 3);
    oSegment.Fields.Add(761, False, 0, False, 4);
    oSegment.Fields.Add(762, False, 0, False, 5);
    oSegment.Fields.Add(763, False, 0, False, 6);
    oSegment.Fields.Add(764, False, 0, False, 7);
    oSegment.Fields.Add(765, False, 0, False, 8);
    oSegment.Fields.Add(766, False, 0, False, 9);
  oSegment := oSegments.Add('NST', 'STATISTICS');
    oSegment.Fields.Add(743, False, 0, True, 1);
    oSegment.Fields.Add(744, False, 0, False, 2);
    oSegment.Fields.Add(745, False, 0, False, 3);
    oSegment.Fields.Add(746, False, 0, False, 4);
    oSegment.Fields.Add(747, False, 0, False, 5);
    oSegment.Fields.Add(748, False, 0, False, 6);
    oSegment.Fields.Add(749, False, 0, False, 7);
    oSegment.Fields.Add(750, False, 0, False, 8);
    oSegment.Fields.Add(751, False, 0, False, 9);
    oSegment.Fields.Add(752, False, 0, False, 10);
    oSegment.Fields.Add(753, False, 0, False, 11);
    oSegment.Fields.Add(754, False, 0, False, 12);
    oSegment.Fields.Add(755, False, 0, False, 13);
    oSegment.Fields.Add(756, False, 0, False, 14);
    oSegment.Fields.Add(757, False, 0, False, 15);
  oSegment := oSegments.Add('NTE', 'NOTES AND COMMENTS');
    oSegment.Fields.Add(573, False, 0, False, 1);
    oSegment.Fields.Add(574, False, 0, False, 2);
    oSegment.Fields.Add(575, True, 0, True, 3);
  oSegment := oSegments.Add('OBR', 'OBSERVATION REQUEST');
    oSegment.Fields.Add(520, False, 0, False, 1);
    oSegment.Fields.Add(732, False, 0, False, 2);
    oSegment.Fields.Add(733, False, 0, False, 3);
    oSegment.Fields.Add(523, False, 0, True, 4);
    oSegment.Fields.Add(524, False, 0, False, 5);
    oSegment.Fields.Add(529, False, 0, False, 6);
    oSegment.Fields.Add(530, False, 0, True, 7);
    oSegment.Fields.Add(531, False, 0, True, 8);
    oSegment.Fields.Add(532, False, 0, True, 9);
    oSegment.Fields.Add(533, True, 0, False, 10);
    oSegment.Fields.Add(534, False, 0, False, 11);
    oSegment.Fields.Add(535, False, 0, False, 12);
    oSegment.Fields.Add(536, False, 0, False, 13);
    oSegment.Fields.Add(537, False, 0, True, 14);
    oSegment.Fields.Add(538, False, 0, False, 15);
    oSegment.Fields.Add(539, True, 0, False, 16);
    oSegment.Fields.Add(540, True, 2, False, 17);
    oSegment.Fields.Add(541, False, 0, False, 18);
    oSegment.Fields.Add(542, False, 0, False, 19);
    oSegment.Fields.Add(543, False, 0, False, 20);
    oSegment.Fields.Add(544, False, 0, False, 21);
    oSegment.Fields.Add(546, False, 0, True, 22);
    oSegment.Fields.Add(547, False, 0, False, 23);
    oSegment.Fields.Add(548, False, 0, False, 24);
    oSegment.Fields.Add(734, False, 0, False, 25);
    oSegment.Fields.Add(550, False, 0, False, 26);
    oSegment.Fields.Add(735, True, 0, False, 27);
    oSegment.Fields.Add(551, True, 5, False, 28);
    oSegment.Fields.Add(737, False, 0, False, 29);
    oSegment.Fields.Add(625, False, 0, False, 30);
    oSegment.Fields.Add(626, True, 0, False, 31);
    oSegment.Fields.Add(627, False, 0, False, 32);
    oSegment.Fields.Add(628, False, 0, False, 33);
    oSegment.Fields.Add(630, False, 0, False, 34);
    oSegment.Fields.Add(629, False, 0, False, 35);
    oSegment.Fields.Add(736, False, 0, False, 36);
  oSegment := oSegments.Add('OBX', 'RESULT');
    oSegment.Fields.Add(559, False, 0, False, 1);
    oSegment.Fields.Add(676, False, 0, False, 2);
    oSegment.Fields.Add(560, False, 0, True, 3);
    oSegment.Fields.Add(769, False, 0, False, 4);
    oSegment.Fields.Add(561, False, 0, True, 5);
    oSegment.Fields.Add(562, False, 0, False, 6);
    oSegment.Fields.Add(563, False, 0, False, 7);
    oSegment.Fields.Add(564, True, 5, False, 8);
    oSegment.Fields.Add(639, False, 0, False, 9);
    oSegment.Fields.Add(565, False, 0, False, 10);
    oSegment.Fields.Add(566, False, 0, False, 11);
    oSegment.Fields.Add(567, False, 0, False, 12);
  oSegment := oSegments.Add('ORC', 'COMMON ORDER');
    oSegment.Fields.Add(714, False, 0, True, 1);
    oSegment.Fields.Add(715, False, 0, False, 2);
    oSegment.Fields.Add(716, False, 0, False, 3);
    oSegment.Fields.Add(717, False, 0, False, 4);
    oSegment.Fields.Add(718, False, 0, False, 5);
    oSegment.Fields.Add(719, False, 0, False, 6);
    oSegment.Fields.Add(720, False, 0, False, 7);
    oSegment.Fields.Add(721, False, 0, False, 8);
    oSegment.Fields.Add(722, False, 0, False, 9);
    oSegment.Fields.Add(723, False, 0, False, 10);
    oSegment.Fields.Add(724, False, 0, False, 11);
    oSegment.Fields.Add(725, False, 0, False, 12);
    oSegment.Fields.Add(726, False, 0, False, 13);
    oSegment.Fields.Add(727, True, 2, False, 14);
  oSegment := oSegments.Add('ORO', 'ORDER OTHER');
    oSegment.Fields.Add(731, False, 0, False, 1);
    oSegment.Fields.Add(120, False, 0, False, 2);
    oSegment.Fields.Add(586, True, 0, False, 3);
    oSegment.Fields.Add(68, False, 0, False, 4);
  oSegments.Add('PD1', 'PATIENT DEMOGRAPHICS');
  oSegment := oSegments.Add('PID', 'PATIENT IDENTIFICATION');
    oSegment.Fields.Add(572, False, 0, False, 1);
    oSegment.Fields.Add(581, False, 0, False, 2);
    oSegment.Fields.Add(34, False, 0, True, 3);
    oSegment.Fields.Add(38, False, 0, False, 4);
    oSegment.Fields.Add(41, False, 0, True, 5);
    oSegment.Fields.Add(582, False, 0, False, 6);
    oSegment.Fields.Add(43, False, 0, False, 7);
    oSegment.Fields.Add(42, False, 0, False, 8);
    oSegment.Fields.Add(597, True, 0, False, 9);
    oSegment.Fields.Add(44, False, 0, False, 10);
    oSegment.Fields.Add(20, False, 0, False, 11);
    oSegment.Fields.Add(26, False, 0, False, 12);
    oSegment.Fields.Add(49, True, 3, False, 13);
    oSegment.Fields.Add(50, True, 3, False, 14);
    oSegment.Fields.Add(464, False, 0, False, 15);
    oSegment.Fields.Add(46, False, 0, False, 16);
    oSegment.Fields.Add(45, False, 0, False, 17);
    oSegment.Fields.Add(35, False, 0, False, 18);
    oSegment.Fields.Add(457, False, 0, False, 19);
    oSegment.Fields.Add(453, False, 0, False, 20);
  oSegment := oSegments.Add('PR1', 'PROCEDURES');
    oSegment.Fields.Add(304, True, 0, True, 1);
    oSegment.Fields.Add(393, False, 0, True, 2);
    oSegment.Fields.Add(305, False, 0, True, 3);
    oSegment.Fields.Add(306, False, 0, False, 4);
    oSegment.Fields.Add(307, False, 0, True, 5);
    oSegment.Fields.Add(309, False, 0, True, 6);
    oSegment.Fields.Add(310, False, 0, False, 7);
    oSegment.Fields.Add(311, False, 0, False, 8);
    oSegment.Fields.Add(313, False, 0, False, 9);
    oSegment.Fields.Add(314, False, 0, False, 10);
    oSegment.Fields.Add(315, False, 0, False, 11);
    oSegment.Fields.Add(318, False, 0, False, 12);
    oSegment.Fields.Add(317, False, 0, False, 13);
  oSegment := oSegments.Add('PV1', 'PATIENT VISIT');
    oSegment.Fields.Add(458, False, 0, False, 1);
    oSegment.Fields.Add(52, False, 0, True, 2);
    oSegment.Fields.Add(53, False, 0, True, 3);
    oSegment.Fields.Add(218, False, 0, False, 4);
    oSegment.Fields.Add(219, False, 0, False, 5);
    oSegment.Fields.Add(56, False, 0, False, 6);
    oSegment.Fields.Add(57, False, 0, False, 7);
    oSegment.Fields.Add(579, False, 0, False, 8);
    oSegment.Fields.Add(580, True, 0, False, 9);
    oSegment.Fields.Add(59, False, 0, False, 10);
    oSegment.Fields.Add(60, False, 0, False, 11);
    oSegment.Fields.Add(220, False, 0, False, 12);
    oSegment.Fields.Add(221, False, 0, False, 13);
    oSegment.Fields.Add(63, False, 0, False, 14);
    oSegment.Fields.Add(64, False, 0, False, 15);
    oSegment.Fields.Add(193, False, 0, False, 16);
    oSegment.Fields.Add(189, False, 0, False, 17);
    oSegment.Fields.Add(191, False, 0, False, 18);
    oSegment.Fields.Add(194, False, 0, False, 19);
    oSegment.Fields.Add(195, True, 4, False, 20);
    oSegment.Fields.Add(199, False, 0, False, 21);
    oSegment.Fields.Add(386, False, 0, False, 22);
    oSegment.Fields.Add(200, False, 0, False, 23);
    oSegment.Fields.Add(201, True, 0, False, 24);
    oSegment.Fields.Add(202, True, 0, False, 25);
    oSegment.Fields.Add(203, True, 0, False, 26);
    oSegment.Fields.Add(204, True, 0, False, 27);
    oSegment.Fields.Add(387, False, 0, False, 28);
    oSegment.Fields.Add(205, False, 0, False, 29);
    oSegment.Fields.Add(388, False, 0, False, 30);
    oSegment.Fields.Add(206, False, 0, False, 31);
    oSegment.Fields.Add(389, False, 0, False, 32);
    oSegment.Fields.Add(390, False, 0, False, 33);
    oSegment.Fields.Add(207, False, 0, False, 34);
    oSegment.Fields.Add(208, False, 0, False, 35);
    oSegment.Fields.Add(613, False, 0, False, 36);
    oSegment.Fields.Add(614, False, 0, False, 37);
    oSegment.Fields.Add(615, False, 0, False, 38);
    oSegment.Fields.Add(616, False, 0, False, 39);
    oSegment.Fields.Add(617, False, 0, False, 40);
    oSegment.Fields.Add(703, False, 0, False, 41);
    oSegment.Fields.Add(704, False, 0, False, 42);
    oSegment.Fields.Add(705, False, 0, False, 43);
    oSegment.Fields.Add(775, False, 0, False, 44);
    oSegment.Fields.Add(776, False, 0, False, 45);
    oSegment.Fields.Add(777, False, 0, False, 46);
    oSegment.Fields.Add(778, False, 0, False, 47);
    oSegment.Fields.Add(779, False, 0, False, 48);
    oSegment.Fields.Add(780, False, 0, False, 49);
  oSegment := oSegments.Add('QRD', 'QUERY DEFINITION');
    oSegment.Fields.Add(156, False, 0, True, 1);
    oSegment.Fields.Add(158, False, 0, True, 2);
    oSegment.Fields.Add(159, False, 0, True, 3);
    oSegment.Fields.Add(160, False, 0, True, 4);
    oSegment.Fields.Add(161, False, 0, False, 5);
    oSegment.Fields.Add(162, False, 0, False, 6);
    oSegment.Fields.Add(164, False, 0, True, 7);
    oSegment.Fields.Add(168, True, 0, True, 8);
    oSegment.Fields.Add(169, True, 0, True, 9);
    oSegment.Fields.Add(170, True, 0, True, 10);
    oSegment.Fields.Add(171, True, 0, False, 11);
    oSegment.Fields.Add(701, False, 0, False, 12);
  oSegment := oSegments.Add('QRF', 'QUERY FILTER');
    oSegment.Fields.Add(173, True, 0, True, 1);
    oSegment.Fields.Add(174, False, 0, False, 2);
    oSegment.Fields.Add(176, False, 0, False, 3);
    oSegment.Fields.Add(178, True, 0, False, 4);
    oSegment.Fields.Add(179, True, 0, False, 5);
  oSegment := oSegments.Add('RX1', 'PHARMACY ORDER');
    oSegment.Fields.Add(770, False, 0, False, 1);
    oSegment.Fields.Add(771, False, 0, False, 2);
    oSegment.Fields.Add(129, False, 0, False, 3);
    oSegment.Fields.Add(130, False, 0, False, 4);
    oSegment.Fields.Add(131, False, 0, False, 5);
    oSegment.Fields.Add(133, False, 0, False, 6);
    oSegment.Fields.Add(137, False, 0, False, 7);
    oSegment.Fields.Add(138, False, 0, False, 8);
    oSegment.Fields.Add(135, False, 0, False, 9);
    oSegment.Fields.Add(139, False, 0, False, 10);
    oSegment.Fields.Add(469, False, 0, False, 11);
    oSegment.Fields.Add(470, False, 0, False, 12);
    oSegment.Fields.Add(772, False, 0, False, 13);
    oSegment.Fields.Add(473, False, 0, False, 14);
    oSegment.Fields.Add(474, True, 5, False, 15);
    oSegment.Fields.Add(479, False, 0, False, 16);
    oSegment.Fields.Add(480, False, 0, False, 17);
    oSegment.Fields.Add(588, False, 0, False, 18);
    oSegment.Fields.Add(481, False, 0, False, 19);
    oSegment.Fields.Add(773, False, 0, False, 20);
    oSegment.Fields.Add(482, False, 0, False, 21);
    oSegment.Fields.Add(619, False, 0, False, 22);
    oSegment.Fields.Add(620, False, 0, False, 23);
    oSegment.Fields.Add(774, False, 0, False, 24);
    oSegment.Fields.Add(483, False, 0, False, 25);
    oSegment.Fields.Add(596, False, 0, False, 26);
    oSegment.Fields.Add(621, False, 0, False, 27);
    oSegment.Fields.Add(484, True, 5, False, 28);
    oSegment.Fields.Add(489, True, 5, False, 29);
    oSegment.Fields.Add(618, True, 0, False, 30);
  oSegment := oSegments.Add('UB1', 'UB82 DATA');
    oSegment.Fields.Add(459, False, 0, False, 1);
    oSegment.Fields.Add(279, False, 0, False, 2);
    oSegment.Fields.Add(396, False, 0, False, 3);
    oSegment.Fields.Add(397, False, 0, False, 4);
    oSegment.Fields.Add(398, False, 0, False, 5);
    oSegment.Fields.Add(399, False, 0, False, 6);
    oSegment.Fields.Add(400, True, 5, False, 7);
    oSegment.Fields.Add(405, False, 0, False, 8);
    oSegment.Fields.Add(406, False, 0, False, 9);
    oSegment.Fields.Add(407, True, 8, False, 10);
    oSegment.Fields.Add(424, False, 0, False, 11);
    oSegment.Fields.Add(425, False, 0, False, 12);
    oSegment.Fields.Add(426, False, 0, False, 13);
    oSegment.Fields.Add(427, False, 0, False, 14);
    oSegment.Fields.Add(428, False, 0, False, 15);
    oSegment.Fields.Add(429, True, 5, False, 16);
    oSegment.Fields.Add(435, False, 0, False, 17);
    oSegment.Fields.Add(446, False, 0, False, 18);
    oSegment.Fields.Add(447, False, 0, False, 19);
    oSegment.Fields.Add(448, False, 0, False, 20);
    oSegment.Fields.Add(449, False, 0, False, 21);
    oSegment.Fields.Add(450, False, 0, False, 22);
    oSegment.Fields.Add(451, False, 0, False, 23);
  oSegment := oSegments.Add('URD', 'RESULTS/UPDATE DEFINITION');
    oSegment.Fields.Add(600, False, 0, False, 1);
    oSegment.Fields.Add(601, False, 0, False, 2);
    oSegment.Fields.Add(602, True, 0, True, 3);
    oSegment.Fields.Add(603, True, 0, False, 4);
    oSegment.Fields.Add(605, True, 0, False, 5);
    oSegment.Fields.Add(607, True, 0, False, 6);
    oSegment.Fields.Add(702, False, 0, False, 7);
  oSegment := oSegments.Add('URS', 'UNSOLICITED SELECTION');
    oSegment.Fields.Add(608, True, 0, True, 1);
    oSegment.Fields.Add(609, False, 0, False, 2);
    oSegment.Fields.Add(610, False, 0, False, 3);
    oSegment.Fields.Add(611, True, 0, False, 4);
    oSegment.Fields.Add(612, True, 0, False, 5);
  oSegments.Add('[', 'begin optional');
  oSegments.Add(']', 'end optional');
  oSegments.Add('{', 'begin repetition');
  oSegments.Add('|', 'next choice');
  oSegments.Add('}', 'end repetition');
End;

procedure Definitions21LoadMessageStructures(oStructures : THL7V2ModelMessageStructures);
Var
  oStructure : THL7V2ModelMessageStructure;
  oGrp1 : THL7V2ModelSegmentGroup;
  oGrp2 : THL7V2ModelSegmentGroup;
  oGrp3 : THL7V2ModelSegmentGroup;
  oGrp4 : THL7V2ModelSegmentGroup;
Begin
  oStructures.Add('?', 'unknown', '', '', '');
  oStructures.Add('ACK', 'Standard Acknowlegdement', 'A01', 'ACK', '');
  oStructure := oStructures.Add('ACK_A01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A04', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A04', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A05', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A05', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A06', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A06', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A07', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A07', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A08', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A08', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A09', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A09', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A10', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A10', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A11', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A11', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A12', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A12', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A13', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A13', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A14', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A14', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A15', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A15', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A16', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A16', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A17', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A17', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A18', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A18', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A20', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A20', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A21', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A21', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A22', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A22', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A23', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A23', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_A24', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_A24', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_P01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_P01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_P02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_P02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_P03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_P03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('ERR', True, False, gtSingle);
  oStructure := oStructures.Add('ACK_Q03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_Q03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
  oStructure := oStructures.Add('ACK_Q05', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_Q05', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
  oStructure := oStructures.Add('ACK_R01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_R01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
  oStructure := oStructures.Add('ACK_R03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ACK_R03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
  oStructure := oStructures.Add('ADR_A19', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADR_A19', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('QUERY_RESPONSE', False, True, gtGroup);
      oGrp1.Children.Add('EVN', True, False, gtSingle);
      oGrp1.Children.Add('PID', False, False, gtSingle);
      oGrp1.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NK1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A04', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A04', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NK1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A05', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A05', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NK1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A06', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A06', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A07', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A07', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A08', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A08', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NK1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A09', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A09', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A10', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A10', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A11', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A11', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A12', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A12', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A13', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A13', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A14', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A14', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PD1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NK1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A15', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A15', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A16', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A16', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DG1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A17', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A17', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT', False, True, gtGroup);
      oGrp1.Children.Add('PID', False, False, gtSingle);
      oGrp1.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A18', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A18', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MRG', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', True, False, gtSingle);
  oStructure := oStructures.Add('ADT_A20', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A20', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NPU', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A21', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A21', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A22', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A22', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A23', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A23', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
  oStructure := oStructures.Add('ADT_A24', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ADT_A24', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
  oStructure := oStructures.Add('BAR_P01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('BAR_P01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('VISIT', False, True, gtGroup);
      oGrp1.Children.Add('PV1', True, False, gtSingle);
      oGrp1.Children.Add('DG1', True, True, gtSingle);
      oGrp1.Children.Add('PR1', True, True, gtSingle);
      oGrp1.Children.Add('GT1', True, True, gtSingle);
      oGrp1.Children.Add('NK1', True, True, gtSingle);
      oGrp1.Children.Add('IN1', True, True, gtSingle);
      oGrp1.Children.Add('ACC', True, False, gtSingle);
      oGrp1.Children.Add('UB1', True, False, gtSingle);
  oStructure := oStructures.Add('BAR_P02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('BAR_P02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT', False, True, gtGroup);
      oGrp1.Children.Add('PID', False, False, gtSingle);
      oGrp1.Children.Add('PV1', True, False, gtSingle);
  oStructure := oStructures.Add('DFT_P03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('DFT_P03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('EVN', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PID', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('PV1', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('FT1', True, True, gtSingle);
  oStructure := oStructures.Add('DSR_Q01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('DSR_Q01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRF', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSP', False, True, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', False, False, gtSingle);
  oStructure := oStructures.Add('DSR_Q03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('DSR_Q03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRF', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSP', False, True, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', False, False, gtSingle);
  oStructure := oStructures.Add('MCF_Q02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('MCF_Q02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
  oStructure := oStructures.Add('ORM_O01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ORM_O01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NTE', True, True, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT', True, False, gtGroup);
      oGrp1.Children.Add('PID', False, False, gtSingle);
      oGrp1.Children.Add('NTE', True, True, gtSingle);
      oGrp1.Children.Add('PV1', True, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('ORDER', False, True, gtGroup);
      oGrp1.Children.Add('ORC', False, False, gtSingle);
      oGrp2 := oGrp1.Children.Add('ORDER_DETAIL', True, False, gtGroup);
        oGrp3 := oGrp2.Children.Add('CHOICE', False, True, gtChoice);
          oGrp3.Children.Add('OBR', False, False, gtSingle);
          oGrp3.Children.Add('ORO', False, False, gtSingle);
          oGrp3.Children.Add('RX1', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
        oGrp2.Children.Add('OBX', True, True, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
      oGrp1.Children.Add('BLG', True, False, gtSingle);
  oStructure := oStructures.Add('ORR_O02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ORR_O02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('MSA', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('NTE', True, True, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT', True, False, gtGroup);
      oGrp1.Children.Add('PID', True, False, gtSingle);
      oGrp1.Children.Add('NTE', True, True, gtSingle);
      oGrp2 := oGrp1.Children.Add('ORDER', False, True, gtGroup);
        oGrp2.Children.Add('ORC', False, False, gtSingle);
        oGrp3 := oGrp2.Children.Add('ORDER_DETAIL', True, False, gtGroup);
          oGrp4 := oGrp3.Children.Add('CHOICE', False, True, gtChoice);
            oGrp4.Children.Add('OBR', False, False, gtSingle);
            oGrp4.Children.Add('ORO', False, False, gtSingle);
            oGrp4.Children.Add('RX1', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
  oStructure := oStructures.Add('ORU_R01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ORU_R01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT_RESULT', False, True, gtGroup);
      oGrp2 := oGrp1.Children.Add('PATIENT', True, False, gtGroup);
        oGrp2.Children.Add('PID', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
        oGrp2.Children.Add('PV1', True, False, gtSingle);
      oGrp2 := oGrp1.Children.Add('ORDER_OBSERVATION', False, True, gtGroup);
        oGrp2.Children.Add('ORC', True, False, gtSingle);
        oGrp2.Children.Add('OBR', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
        oGrp3 := oGrp2.Children.Add('OBSERVATION', False, True, gtGroup);
          oGrp3.Children.Add('OBX', True, False, gtSingle);
          oGrp3.Children.Add('NTE', True, True, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', True, False, gtSingle);
  oStructure := oStructures.Add('ORU_R03', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('ORU_R03', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oGrp1 := oStructure.SegmentMap.Children.Add('PATIENT_RESULT', False, True, gtGroup);
      oGrp2 := oGrp1.Children.Add('PATIENT', True, False, gtGroup);
        oGrp2.Children.Add('PID', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
        oGrp2.Children.Add('PV1', True, False, gtSingle);
      oGrp2 := oGrp1.Children.Add('ORDER_OBSERVATION', False, True, gtGroup);
        oGrp2.Children.Add('ORC', True, False, gtSingle);
        oGrp2.Children.Add('OBR', False, False, gtSingle);
        oGrp2.Children.Add('NTE', True, True, gtSingle);
        oGrp3 := oGrp2.Children.Add('OBSERVATION', False, True, gtGroup);
          oGrp3.Children.Add('OBX', True, False, gtSingle);
          oGrp3.Children.Add('NTE', True, True, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', True, False, gtSingle);
  oStructure := oStructures.Add('QRY_A19', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('QRY_A19', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
  oStructure := oStructures.Add('QRY_Q01', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('QRY_Q01', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRF', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', False, False, gtSingle);
  oStructure := oStructures.Add('QRY_Q02', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('QRY_Q02', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRD', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('QRF', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', False, False, gtSingle);
  oStructure := oStructures.Add('UDM_Q05', '(Implicitly Created by HL7Connect)', '', '', '');
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create;
  oStructure.SegmentMap := THL7V2ModelSegmentGroup.Create('UDM_Q05', False, False, gtGroup);
    oStructure.SegmentMap.Children.Add('MSH', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('URD', False, False, gtSingle);
    oStructure.SegmentMap.Children.Add('URS', True, False, gtSingle);
    oStructure.SegmentMap.Children.Add('DSP', False, True, gtSingle);
    oStructure.SegmentMap.Children.Add('DSC', False, False, gtSingle);
End;

procedure Definitions21LoadEvents(oEvents : THL7V2ModelEvents);
Var
  oEvent : THL7V2ModelEvent;
Begin
  oEvent := oEvents.Add('A01', 'Admit a Patient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A02', 'Transfer a Patient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A03', 'Discharge a Patient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A04', 'Register a Patient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A05', 'Pre-admit a Patient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A06', 'Transfer an Outpatient to Inpatient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A07', 'Transfer an Inpatient to Outpatient');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A08', 'Update Patient Information');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A09', 'Patient Departing');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A10', 'Patient Arriving');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A11', 'Cancel Admit');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A12', 'Cancel Transfer');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A13', 'Cancel Discharge');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A14', 'Pending Admit');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A15', 'Pending Transfer');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A16', 'Pending Discharge');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A17', 'Swap Patients');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A18', 'Merge Patient Information');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A19', 'Patient Query');
    oEvent.Messages.Add('QRY', '?', 'ADR', '?');
  oEvent := oEvents.Add('A20', 'Nursing/Census Application Updates');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A21', 'A Patient Goes On A "Leave Of Absence"');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A22', 'A Patient Returns From A "Leave Of Absence"');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A23', 'Delete a Patient Record');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('A24', 'Create a Patient Link Transaction');
    oEvent.Messages.Add('ADT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('O01', 'Order Message');
    oEvent.Messages.Add('ORM', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('O02', 'Respone Message');
    oEvent.Messages.Add('ORR', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('P01', 'Add and Update Patient Accounts');
    oEvent.Messages.Add('BAR', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('P02', 'Purge Patient Accounts');
    oEvent.Messages.Add('BAR', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('P03', 'Post Detail Financial Transactions');
    oEvent.Messages.Add('DFT', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('P04', 'Generate bills and A/R statements');
    oEvent.Messages.Add('QRY', '?', 'DSR', '?');
  oEvent := oEvents.Add('Q01', 'A Query is Made for Immediate Response');
    oEvent.Messages.Add('QRY', '?', 'DSR', '?');
  oEvent := oEvents.Add('Q02', 'A Query is Sent for Deferred Response');
    oEvent.Messages.Add('QRY', '?', 'MCF', '?');
  oEvent := oEvents.Add('Q03', 'Deferred Response to A Query');
    oEvent.Messages.Add('DSR', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('Q05', 'Unsolicited Updates, ODM Message, Trigger Event Q05');
    oEvent.Messages.Add('UDM', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('R01', 'Unsolicited transmission of requested Obersvation');
    oEvent.Messages.Add('ORU', '?', 'ACK', 'ACK');
  oEvent := oEvents.Add('R02', 'Query for results of observation');
    oEvent.Messages.Add('QRY', '?', 'ORF', '?');
  oEvent := oEvents.Add('R03', 'Display oriented results, queyr/unsol. update');
    oEvent.Messages.Add('ORU', '?', 'ACK', 'ACK');
End;

End.

