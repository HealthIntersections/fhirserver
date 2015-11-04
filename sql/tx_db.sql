CREATE TABLE [dbo].[Unii](
	[UniiKey] [int] NOT NULL,
	[Code] [nchar](20) NOT NULL,
	[Display] [nchar](255) NULL,
 CONSTRAINT [PK_Unii] PRIMARY KEY CLUSTERED 
(
	[UniiKey] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

CREATE TABLE [dbo].[UniiDesc](
	[UniiDescKey] [int] NOT NULL,
	[UniiKey] [int] NOT NULL,
	[Type] [nchar](20) NOT NULL,
	[Display] [nchar](255) NULL,
 CONSTRAINT [PK_UniiDesc] PRIMARY KEY CLUSTERED 
(
	[UniiDescKey] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [dbo].[UniiDesc]  WITH CHECK ADD  CONSTRAINT [FK_ValueSetMembers_UniiKey] FOREIGN KEY([UniiKey])
REFERENCES [dbo].[Unii] ([UniiKey])
GO

ALTER TABLE [dbo].[UniiDesc] CHECK CONSTRAINT [FK_ValueSetMembers_UniiKey]
GO

CREATE TABLE [dbo].[cvx](
	[CVX Code] [nvarchar](255) NOT NULL,
	[CVX Short Description] [nvarchar](255) NOT NULL,
	[Full Vaccine Name] [nvarchar](255) NULL,
	[Note] [nvarchar](max) NULL,
	[VaccineStatus] [nvarchar](255) NULL,
	[internalID] [float] NULL,
	[nonvaccine] [bit] NOT NULL,
	[update_date] [datetime] NULL,
CONSTRAINT [PK_CVX] PRIMARY KEY CLUSTERED 
(
	[CVX Code] ASC
)
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO


INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('998','no vaccine administered','no vaccine administered','Code 998 was added for use in VXU HL7 messages where the OBX segment is nested with the RXA segment, but the message does not contain information about a vaccine administration. An example of this use is to report the vaccines due next for a patient when no vaccine administration is being reported.','Inactive','137                 ','1                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('99','RESERVED - do not use','RESERVED - do not use','Code 99 will not be used in this table to avoid confusion with code 999.','Inactive','139                 ','1                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('999','unknown','unknown vaccine or immune globulin','This CVX code has little utility and should rarely be used.','Inactive','138                 ','1                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('143','Adenovirus types 4 and 7','Adenovirus, type 4 and type 7, live, oral','This vaccine is administered as 2 tablets.','Active','161                 ','0                   ','Mar 20 2011 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('54','adenovirus, type 4','adenovirus vaccine, type 4, live, oral','','Inactive','1                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('55','adenovirus, type 7','adenovirus vaccine, type 7, live, oral','','Inactive','2                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('82','adenovirus, unspecified formulation','adenovirus vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a adenovirus vaccination when noted on a vaccination card)','Inactive','3                   ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('24','anthrax','anthrax vaccine','','Active','4                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('801','AS03 Adjuvant','AS03 Adjuvant','This is the adjuvant that is packaged with H5N1 vaccine, adjuvanted','Active','179                 ','0                   ','Feb  7 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('19','BCG','Bacillus Calmette-Guerin vaccine','','Active','5                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('27','botulinum antitoxin','botulinum antitoxin','','Active','6                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('26','cholera','cholera vaccine','','Inactive','7                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('29','CMVIG','cytomegalovirus immune globulin, intravenous','','Active','8                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('56','dengue fever','dengue fever vaccine','','Never Active','9                   ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('12','diphtheria antitoxin','diphtheria antitoxin','','Active','10                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('28','DT (pediatric)','diphtheria and tetanus toxoids, adsorbed for pediatric use','','Active','11                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('20','DTaP','diphtheria, tetanus toxoids and acellular pertussis vaccine','','Active','12                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('106','DTaP, 5 pertussis antigens','diphtheria, tetanus toxoids and acellular pertussis vaccine, 5 pertussis antigens','','Active','13                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('107','DTaP, unspecified formulation','diphtheria, tetanus toxoids and acellular pertussis vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a DTaP vaccination when noted on a vaccination card)','Inactive','14                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('146','DTaP,IPV,Hib,HepB','Diphtheria and Tetanus Toxoids and Acellular Pertussis Adsorbed, Inactivated Poliovirus, Haemophilus b Conjugate (Meningococcal Outer Membrane Protein Complex), and Hepatitis B (Recombinant) Vaccine.','Note that this vaccine is different from CVX 132.','Pending','164                 ','0                   ','Aug 31 2011 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('102','DTaP/DTP-Hib-Hep B','DTP- Haemophilus influenzae type b conjugate and hepatitis b vaccine','This non-US vaccine contained DTP prior to 2007 and now contains DTaP','Inactive','23                  ','0                   ','May 30 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('110','DTaP-Hep B-IPV','DTaP-hepatitis B and poliovirus vaccine','','Active','15                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('50','DTaP-Hib','DTaP-Haemophilus influenzae type b conjugate vaccine','','Active','16                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('120','DTaP-Hib-IPV','diphtheria, tetanus toxoids and acellular pertussis vaccine, Haemophilus influenzae type b conjugate, and poliovirus vaccine, inactivated (DTaP-Hib-IPV)','','Active','17                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('130','DTaP-IPV','Diphtheria, tetanus toxoids and acellular pertussis vaccine, and poliovirus vaccine, inactivated','','Active','19                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('132','DTaP-IPV-HIB-HEP B, historical','Historical record of vaccine containing diphtheria, tetanus toxoids and acellular pertussis, poliovirus, inactivated, Haemophilus influenzae type b conjugate, Hepatitis B (DTaP-Hib-IPV)','This is not the same as CVX 146, Hexavalent vaccine.','Inactive','148                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('01','DTP','diphtheria, tetanus toxoids and pertussis vaccine','','Inactive','21                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('22','DTP-Hib','DTP-Haemophilus influenzae type b conjugate vaccine','','Inactive','22                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('57','hantavirus','hantavirus vaccine','','Never Active','24                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('30','HBIG','hepatitis B immune globulin','','Active','31                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('52','Hep A, adult','hepatitis A vaccine, adult dosage','','Active','25                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('154','Hep A, IG','Hepatitis A immune globulin','Do not use this code. This product may be used for Hep A and other viral infections. The correct vaccine / CVX is 86 (IG).','Never Active','172                 ','0                   ','Apr  1 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('83','Hep A, ped/adol, 2 dose','hepatitis A vaccine, pediatric/adolescent dosage, 2 dose schedule','','Active','26                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('84','Hep A, ped/adol, 3 dose','hepatitis A vaccine, pediatric/adolescent dosage, 3 dose schedule','This vaccine formulation is inactive and should not be used, except to record historic vaccinations with this formulation.','Inactive','27                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('31','Hep A, pediatric, unspecified formulation','hepatitis A vaccine, pediatric dosage, unspecified formulation','Do NOT use this code.  If formulation is unknown, use CVX 85.  There is only one formulation of Hep A, peds.','Inactive','28                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('85','Hep A, unspecified formulation','hepatitis A vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a HepA vaccination when noted on a vaccination card)','Inactive','29                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('104','Hep A-Hep B','hepatitis A and hepatitis B vaccine','','Active','30                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('08','Hep B, adolescent or pediatric','hepatitis B vaccine, pediatric or pediatric/adolescent dosage','This code applies to any standard pediatric formulation of Hepatitis B vaccine. It should not be used for the 2-dose hepatitis B schedule for adolescents (11-15 year olds). It requires Merck''s Recombivax HB® adult formulation. Use code 43 for that vaccine.','Active','32                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('42','Hep B, adolescent/high risk infant','hepatitis B vaccine, adolescent/high risk infant dosage','As of August 27, 1998, Merck ceased distribution of their adolescent/high risk infant hepatitis B vaccine dosage. Code 42 should only be used to record historical records. For current administration of hepatitis B vaccine, pediatric/adolescent dosage, use code 08.','Inactive','33                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('43','Hep B, adult','hepatitis B vaccine, adult dosage','As of September 1999, a 2-dose hepatitis B schedule for adolescents (11-15 year olds) was FDA approved for Merck''s Recombivax HB® adult formulation. Use code 43 for the 2-dose. This code should be used for any use of standard adult formulation of hepatitis B vaccine.','Active','34                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('44','Hep B, dialysis','hepatitis B vaccine, dialysis patient dosage','','Active','35                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('45','Hep B, unspecified formulation','hepatitis B vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a HepB vaccination when noted on a vaccination card)','Inactive','36                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('58','Hep C','hepatitis C vaccine','','Never Active','37                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('59','Hep E','hepatitis E vaccine','','Never Active','38                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('60','herpes simplex 2','herpes simplex virus, type 2 vaccine','','Never Active','39                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('47','Hib (HbOC)','Haemophilus influenzae type b vaccine, HbOC conjugate','','Inactive','41                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('46','Hib (PRP-D)','Haemophilus influenzae type b vaccine, PRP-D conjugate','','Inactive','40                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('49','Hib (PRP-OMP)','Haemophilus influenzae type b vaccine, PRP-OMP conjugate','','Active','43                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('48','Hib (PRP-T)','Haemophilus influenzae type b vaccine, PRP-T conjugate','','Active','42                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('17','Hib, unspecified formulation','Haemophilus influenzae type b vaccine, conjugate unspecified formulation','','Inactive','44                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('51','Hib-Hep B','Haemophilus influenzae type b conjugate and Hepatitis B vaccine','','Active','45                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('61','HIV','human immunodeficiency virus vaccine','','Never Active','46                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('118','HPV, bivalent','human papilloma virus vaccine, bivalent','','Active','47                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('62','HPV, quadrivalent','human papilloma virus vaccine, quadrivalent','','Active','49                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('137','HPV, unspecified formulation','HPV, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a HPV vaccination when noted on a vaccination card)','Inactive','144                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('86','IG','immune globulin, intramuscular','','Active','51                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('14','IG, unspecified formulation','immune globulin, unspecified formulation','','Inactive','53                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('87','IGIV','immune globulin, intravenous','','Active','52                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('160','Influenza A monovalent (H5N1), ADJUVANTED-2013','Influenza A monovalent (H5N1), adjuvanted, National stockpile 2013','Approved by FDA 2013, adjuvant is mixed at point of administration.','Active','178                 ','0                   ','May 27 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('151','influenza nasal, unspecified formulation','influenza nasal, unspecified formulation','This CVX should only be used for historical records where the formulation of nasal flu vaccine is not known.','Inactive','169                 ','0                   ','Jan 28 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('123','influenza, H5N1-1203','influenza virus vaccine, H5N1, A/Vietnam/1203/2004 (national stockpile)','','Inactive','58                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('135','Influenza, high dose seasonal','influenza, high dose seasonal, preservative-free','','Active','146                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('153','Influenza, injectable, MDCK, preservative free','Influenza, injectable, Madin Darby Canine Kidney, preservative free','ccIIV3','Active','171                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('158','influenza, injectable, quadrivalent','influenza, injectable, quadrivalent, contains preservative','New in 2013.  IIV4','Active','176                 ','0                   ','Aug 20 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('150','influenza, injectable, quadrivalent, preservative free','Influenza, injectable, quadrivalent, preservative free','New in 2012.  IIV4','Active','168                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('161','Influenza, injectable,quadrivalent, preservative free, pediatric','Influenza, injectable,quadrivalent, preservative free, pediatric','','Active','180                 ','0                   ','Jul 23 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('111','influenza, live, intranasal','influenza virus vaccine, live, attenuated, for intranasal use','LAIV3','Inactive','54                  ','0                   ','Sep 22 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('149','influenza, live, intranasal, quadrivalent','influenza, live, intranasal, quadrivalent','new in 2012.  LAIV4','Active','167                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('155','influenza, recombinant, injectable, preservative free','Seasonal, trivalent, recombinant, injectable influenza vaccine, preservative free','RIV','Active','173                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('141','Influenza, seasonal, injectable','Influenza, seasonal, injectable','IIV3. This is one of two codes replacing CVX 15, which is being retired.','Active','159                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('140','Influenza, seasonal, injectable, preservative free','Influenza, seasonal, injectable, preservative free','IIV3. This vaccine code is one of two which replace CVX 15, influenza, split virus.','Active','156                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('144','influenza, seasonal, intradermal, preservative free','seasonal influenza, intradermal, preservative free','IIV3','Active','162                 ','0                   ','Jul 17 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('15','influenza, split (incl. purified surface antigen)','influenza virus vaccine, split virus (incl. purified surface antigen)-retired CODE','This code is being retired. It will still be found in older immunization records. It included both preservative free and non-preservative free.','Inactive','55                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('88','influenza, unspecified formulation','influenza virus vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a Influenza vaccination when noted on a vaccination card)','Inactive','57                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('16','influenza, whole','influenza virus vaccine, whole virus','','Inactive','56                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('10','IPV','poliovirus vaccine, inactivated','','Active','60                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('134','Japanese Encephalitis IM','Japanese Encephalitis vaccine for intramuscular administration','','Active','142                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('39','Japanese encephalitis SC','Japanese Encephalitis Vaccine SC','','Active','63                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('129','Japanese Encephalitis, unspecified formulation','Japanese Encephalitis vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a JE vaccination when noted on a vaccination card)','Inactive','157                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('63','Junin virus','Junin virus vaccine','','Never Active','64                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('64','leishmaniasis','leishmaniasis vaccine','','Never Active','65                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('65','leprosy','leprosy vaccine','','Never Active','66                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('66','Lyme disease','Lyme disease vaccine','','Inactive','67                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('04','M/R','measles and rubella virus vaccine','','Inactive','69                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('67','malaria','malaria vaccine','','Never Active','71                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('05','measles','measles virus vaccine','','Inactive','72                  ','0                   ','Aug 31 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('68','melanoma','melanoma vaccine','','Never Active','73                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('163','meningococcal B, OMV','meningococcal B vaccine, recombinant, OMV, adjuvanted','','Pending','182                 ','0                   ','Nov  3 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('162','meningococcal B, recombinant','meningococcal B vaccine, fully recombinant','','Active','181                 ','0                   ','Nov  3 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('164','meningococcal B, unspecified','meningococcal B, unspecified formulation','','Inactive','183                 ','0                   ','Nov  3 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('103','meningococcal C conjugate','meningococcal C conjugate vaccine','','Inactive','75                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('148','Meningococcal C/Y-HIB PRP','Meningococcal Groups C and Y and Haemophilus b Tetanus Toxoid Conjugate Vaccine','','Active','166                 ','0                   ','Jul 11 2012 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('147','meningococcal MCV4, unspecified formulation','Meningococcal, MCV4, unspecified formulation(groups A, C, Y and W-135)','This CVX should only be used for historical doses of meningococcal conjugate vaccine where the formulation is unknown (oligosaccharide vs polysaccharide). It is not the same as CVX 108, Meningococcal, unspecified formulation.','Inactive','165                 ','0                   ','Feb  8 2012 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('136','Meningococcal MCV4O','meningococcal oligosaccharide (groups A, C, Y and W-135) diphtheria toxoid conjugate vaccine (MCV4O)','','Active','145                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('114','meningococcal MCV4P','meningococcal polysaccharide (groups A, C, Y and W-135) diphtheria toxoid conjugate vaccine (MCV4P)','','Active','76                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('32','meningococcal MPSV4','meningococcal polysaccharide vaccine (MPSV4)','','Active','74                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('108','meningococcal, unspecified formulation','meningococcal vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a meningococcal vaccination when noted on a vaccination card)','Inactive','77                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('03','MMR','measles, mumps and rubella virus vaccine','','Active','68                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('94','MMRV','measles, mumps, rubella, and varicella virus vaccine','','Active','70                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('07','mumps','mumps virus vaccine','','Active','79                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('127','Novel influenza-H1N1-09','Novel influenza-H1N1-09, injectable','','Inactive','152                 ','0                   ','Aug 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('128','Novel Influenza-H1N1-09, all formulations','Novel influenza-H1N1-09, all formulations','This code is used whenever the actual formulation is not determined or when aggregating all Novel H1N1 Influenza-09 immunizations for reporting to CRA. It should not be used for seasonal influenza vaccine that is not otherwise specified. (NOS)','Inactive','149                 ','0                   ','Aug 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('125','Novel Influenza-H1N1-09, nasal','Novel Influenza-H1N1-09, live virus for nasal administration','','Inactive','150                 ','0                   ','Aug 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('126','Novel influenza-H1N1-09, preservative-free','Novel influenza-H1N1-09, preservative-free, injectable','','Inactive','151                 ','0                   ','Aug 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('02','OPV','poliovirus vaccine, live, oral','','Inactive','61                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('69','parainfluenza-3','parainfluenza-3 virus vaccine','','Inactive','80                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('11','pertussis','pertussis vaccine','','Inactive','81                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('23','plague','plague vaccine','','Active','82                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('133','Pneumococcal conjugate PCV 13','pneumococcal conjugate vaccine, 13 valent','','Active','141                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('100','pneumococcal conjugate PCV 7','pneumococcal conjugate vaccine, 7 valent','','Inactive','84                  ','0                   ','Jun 26 2014 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('152','Pneumococcal Conjugate, unspecified formulation','Pneumococcal Conjugate, unspecified formulation','This CVX should only be used for historical records where the formulation of pneumococcal conjugate vaccine is not known.','Inactive','170                 ','0                   ','Jan 28 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('33','pneumococcal polysaccharide PPV23','pneumococcal polysaccharide vaccine, 23 valent','','Active','83                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('109','pneumococcal, unspecified formulation','pneumococcal vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a pneumococcal vaccination when noted on a vaccination card)','Inactive','85                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('89','polio, unspecified formulation','poliovirus vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a polio vaccination when noted on a vaccination card)','Inactive','62                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('70','Q fever','Q fever vaccine','','Never Active','86                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('40','rabies, intradermal injection','rabies vaccine, for intradermal injection','','Active','88                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('18','rabies, intramuscular injection','rabies vaccine, for intramuscular injection','','Active','87                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('90','rabies, unspecified formulation','rabies vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a rabies vaccination when noted on a vaccination card)','Inactive','89                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('72','rheumatic fever','rheumatic fever vaccine','','Never Active','90                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('159','Rho(D) - Unspecified formulation','Rho(D) Unspecified formulation','','Inactive','177                 ','0                   ','Sep  5 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('157','Rho(D) -IG IM','Rho(D) Immune globulin - IM','This immune globulin may be administered IM only.','Active','175                 ','0                   ','Aug 13 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('156','Rho(D)-IG','Rho(D) Immune globulin- IV or IM','This immune globulin may be administered either IM or IV.','Active','174                 ','0                   ','Jul 26 2013 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('73','Rift Valley fever','Rift Valley fever vaccine','','Never Active','91                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('34','RIG','rabies immune globulin','','Active','92                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('119','rotavirus, monovalent','rotavirus, live, monovalent vaccine','','Active','93                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('116','rotavirus, pentavalent','rotavirus, live, pentavalent vaccine','','Active','97                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('74','rotavirus, tetravalent','rotavirus, live, tetravalent vaccine','','Inactive','99                  ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('122','rotavirus, unspecified formulation','rotavirus vaccine, unspecified formulation','','Inactive','95                  ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('71','RSV-IGIV','respiratory syncytial virus immune globulin, intravenous','','Active','101                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('93','RSV-MAb','respiratory syncytial virus monoclonal antibody (palivizumab), intramuscular','','Active','102                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('145','RSV-MAb (new)','respiratory syncytial virus monoclonal antibody (motavizumab), intramuscular','','Pending','163                 ','0                   ','Aug 31 2011 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('06','rubella','rubella virus vaccine','','Active','103                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('38','rubella/mumps','rubella and mumps virus vaccine','','Inactive','104                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('76','Staphylococcus bacterio lysate','Staphylococcus bacteriophage lysate','','Inactive','105                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('138','Td (adult)','tetanus and diphtheria toxoids, not adsorbed, for adult use','Note that this Td is not adsorbed.','Active','154                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('113','Td (adult) preservative free','tetanus and diphtheria toxoids, adsorbed, preservative free, for adult use','','Active','106                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('09','Td (adult), adsorbed','tetanus and diphtheria toxoids, adsorbed, for adult use','Note that this vaccine name has changed.  See also Td (adult). It is not adsorbed.','Active','107                 ','0                   ','Aug 20 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('139','Td(adult) unspecified formulation','Td(adult) unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a Td vaccination when noted on a vaccination card)','Inactive','155                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('115','Tdap','tetanus toxoid, reduced diphtheria toxoid, and acellular pertussis vaccine, adsorbed','','Active','108                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('35','tetanus toxoid, adsorbed','tetanus toxoid, adsorbed','','Active','110                 ','0                   ','Feb 14 2011 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('142','tetanus toxoid, not adsorbed','tetanus toxoid, not adsorbed','','Active','160                 ','0                   ','Feb 14 2011 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('112','tetanus toxoid, unspecified formulation','tetanus toxoid, unspecified formulation','','Inactive','111                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('77','tick-borne encephalitis','tick-borne encephalitis vaccine','','Inactive','112                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('13','TIG','tetanus immune globulin','','Active','113                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('98','TST, unspecified formulation','tuberculin skin test; unspecified formulation','TB Skin test is not vaccine.','Inactive','117                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('95','TST-OT tine test','tuberculin skin test; old tuberculin, multipuncture device','TB Skin test is not vaccine.','Inactive','114                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('96','TST-PPD intradermal','tuberculin skin test; purified protein derivative solution, intradermal','TB Skin test is not vaccine.','Inactive','115                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('97','TST-PPD tine test','tuberculin skin test; purified protein derivative, multipuncture device','TB Skin test is not vaccine.','Inactive','116                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('78','tularemia vaccine','tularemia vaccine','','Inactive','118                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('25','typhoid, oral','typhoid vaccine, live, oral','','Active','120                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('41','typhoid, parenteral','typhoid vaccine, parenteral, other than acetone-killed, dried','','Active','121                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('53','typhoid, parenteral, AKD (U.S. military)','typhoid vaccine, parenteral, acetone-killed, dried (U.S. military)','','Active','122                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('91','typhoid, unspecified formulation','typhoid vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a typhoid vaccination when noted on a vaccination card)','Inactive','119                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('101','typhoid, ViCPs','typhoid Vi capsular polysaccharide vaccine','','Active','123                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('131','typhus, historical','Historical record of a typhus vaccination','','Inactive','147                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('75','vaccinia (smallpox)','vaccinia (smallpox) vaccine','','Active','124                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('105','vaccinia (smallpox) diluted','vaccinia (smallpox) vaccine, diluted','','Inactive','125                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('79','vaccinia immune globulin','vaccinia immune globulin','','Active','126                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('21','varicella','varicella virus vaccine','','Active','127                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('81','VEE, inactivated','Venezuelan equine encephalitis, inactivated','','Inactive','128                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('80','VEE, live','Venezuelan equine encephalitis, live, attenuated','','Inactive','129                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('92','VEE, unspecified formulation','Venezuelan equine encephalitis vaccine, unspecified formulation','This CVX code allows reporting of a vaccination when formulation is unknown (for example, when recording a VEE vaccination when noted on a vaccination card)','Inactive','130                 ','0                   ','Sep 30 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('36','VZIG','varicella zoster immune globulin','','Active','131                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('117','VZIG (IND)','varicella zoster immune globulin (Investigational New Drug)','','Inactive','132                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('37','yellow fever','yellow fever vaccine','','Active','134                 ','0                   ','May 28 2010 12:00AM ')
GO
INSERT INTO [dbo].[cvx] ([CVX Code],[CVX Short Description],[Full Vaccine Name],[Note],[VaccineStatus],[internalID],[nonvaccine],[update_date]) VALUES ('121','zoster','zoster vaccine, live','','Active','135                 ','0                   ','May 28 2010 12:00AM ')

