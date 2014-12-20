CREATE TABLE NCIATOMARCHIVE
(
   RXAUI             varchar(8) NOT NULL,
   AUI               varchar(10),
   STR               varchar(4000) NOT NULL,
   ARCHIVE_TIMESTAMP varchar(280) NOT NULL,
   CREATED_TIMESTAMP varchar(280) NOT NULL,
   UPDATED_TIMESTAMP varchar(280) NOT NULL,
   CODE              varchar(50),
   IS_BRAND          varchar(1),
   LAT               varchar(3),
   LAST_RELEASED     varchar(30),
   SAUI              varchar(50),
   VSAB              varchar(40),
   RXCUI             varchar(8),
   SAB               varchar(20),
   TTY               varchar(20),
   MERGED_TO_RXCUI   varchar(8)
)
GO

CREATE TABLE NCICONSO
(
   RXCUI             varchar(8) NOT NULL,
   LAT               varchar (3) DEFAULT 'ENG' NOT NULL,
   TS                varchar (1),
   LUI               varchar(8),
   STT               varchar (3),
   SUI               varchar (8),
   ISPREF            varchar (1),
   RXAUI             varchar(8) NOT NULL,
   SAUI              varchar (50),
   SCUI              varchar (50),
   SDUI              varchar (50),
   SAB               varchar (20) NOT NULL,
   TTY               varchar (20) NOT NULL,
   CODE              varchar (50) NOT NULL,
   STR               varchar (3000) NOT NULL,
   SRL               varchar (10),
   SUPPRESS          varchar (1),
   CVF               varchar(50)
)

Go

CREATE TABLE NCIREL
(
   RXCUI1    varchar(8) ,
   RXAUI1    varchar(8),
   STYPE1    varchar(50),
   REL       varchar(4) ,
   RXCUI2    varchar(8) ,
   RXAUI2    varchar(8),
   STYPE2    varchar(50),
   RELA      varchar(100) ,
   RUI       varchar(10),
   SRUI      varchar(50),
   SAB       varchar(20) NOT NULL,
   SL        varchar(1000),
   DIR       varchar(1),
   RG        varchar(10),
   SUPPRESS  varchar(1),
   CVF       varchar(50)
)

Go

CREATE TABLE NCISAB
(
   VCUI           varchar (8),
   RCUI           varchar (8),
   VSAB           varchar (40),
   RSAB           varchar (20) NOT NULL,
   SON            varchar (3000),
   SF             varchar (20),
   SVER           varchar (20),
   VSTART         varchar (10),
   VEND           varchar (10),
   IMETA          varchar (10),
   RMETA          varchar (10),
   SLC            varchar (1000),
   SCC            varchar (1000),
   SRL            integer,
   TFR            integer,
   CFR            integer,
   CXTY           varchar (50),
   TTYL           varchar (300),
   ATNL           varchar (1000),
   LAT            varchar (3),
   CENC           varchar (20),
   CURVER         varchar (1),
   SABIN          varchar (1),
   SSN            varchar (3000),
   SCIT           varchar (4000)
)

Go

CREATE TABLE NCISAT
(
   RXCUI            varchar(8) ,
   LUI              varchar(8),
   SUI              varchar(8),
   RXAUI            varchar(8),
   STYPE            varchar (50),
   CODE             varchar (50),
   ATUI             varchar(11),
   SATUI            varchar (50),
   ATN              varchar (1000) NOT NULL,
   SAB              varchar (20) NOT NULL,
   ATV              varchar (4000),
   SUPPRESS         varchar (1),
   CVF              varchar (50)
)

Go

CREATE TABLE NCISTY
(
   RXCUI          varchar(8) NOT NULL,
   TUI            varchar (4),
   STN            varchar (100),
   STY            varchar (50),
   ATUI           varchar (11),
   CVF            varchar (50)
)

Go

CREATE TABLE NCIDOC (
    DOCKEY      varchar(50) NOT NULL,
    VALUE       varchar(1000),
    TYPE        varchar(50) NOT NULL,
    EXPL        varchar(1000)
)

Go

CREATE TABLE NCICUICHANGES
(
      RXAUI         varchar(8),
      CODE          varchar(50),
      SAB           varchar(20),
      TTY           varchar(20),
      STR           varchar(3000),
      OLD_RXCUI     varchar(8) NOT NULL,
      NEW_RXCUI     varchar(8) NOT NULL
)

Go

 CREATE TABLE NCICUI (
 cui1 VARCHAR(8),
 ver_start VARCHAR(40),
 ver_end   VARCHAR(40),
 cardinality VARCHAR(8),
 cui2       VARCHAR(8) 
)

Go

-- bulk imports

BULK INSERT NCIREL from 'C:\Data\terminologies\nci\META\MRREL.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCISAB from 'C:\Data\terminologies\nci\META\MRSAB.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCISAT from 'C:\Data\terminologies\nci\META\MRSAT.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCISTY from 'C:\Data\terminologies\nci\META\MRSTY.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCICONSO from 'C:\Data\terminologies\nci\META\MRCONSO.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCICUI from 'C:\Data\terminologies\nci\META\MRCUI.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

BULK INSERT NCIDOC from 'C:\Data\terminologies\nci\META\MRDOC.RRF'
  WITH (FIELDTERMINATOR = '|' , ROWTERMINATOR ='0x0a')
Go

-- create basic indexes

CREATE INDEX X_NCIREL_RXCUI1 ON NCIREL(RXCUI1);
CREATE INDEX X_NCIREL_RXCUI2 ON NCIREL(RXCUI2);
CREATE INDEX X_NCIREL_RXAUI1 ON NCIREL(RXAUI1);
CREATE INDEX X_NCIREL_RXAUI2 ON NCIREL(RXAUI2);
CREATE INDEX X_NCIREL_RELA ON NCIREL(RELA);
CREATE INDEX X_NCICONSO_RXCUI ON NCICONSO(RXCUI);
CREATE INDEX X_NCICONSO_TTY ON NCICONSO(TTY);

-- create FHIR Server Extensions
CREATE TABLE NCIstems (
  stem CHAR(20) NOT NULL,
  CUI VARCHAR(8) NOT NULL,
  PRIMARY KEY (stem, CUI));
Go


