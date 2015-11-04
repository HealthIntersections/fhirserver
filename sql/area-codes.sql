CREATE TABLE [dbo].[AreaCodes](
	[Display] [nchar](255) NOT NULL,
	[Code] [nchar](10) NOT NULL,
	[Abbrev] [nchar](255) NOT NULL,
 CONSTRAINT [PK_AreaCodes] PRIMARY KEY CLUSTERED 
(
	[Code] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

insert into AreaCodes (Code, Display, Abbrev) values ('004', 'Afghanistan', 'AFG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('248', 'Åland Islands', 'ALA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('008', 'Albania', 'ALB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('012', 'Algeria', 'DZA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('016', 'American Samoa', 'ASM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('020', 'Andorra', 'AND')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('024', 'Angola', 'AGO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('660', 'Anguilla', 'AIA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('028', 'Antigua and Barbuda', 'ATG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('032', 'Argentina', 'ARG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('051', 'Armenia', 'ARM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('533', 'Aruba', 'ABW')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('036', 'Australia', 'AUS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('040', 'Austria', 'AUT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('031', 'Azerbaijan', 'AZE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('044', 'Bahamas', 'BHS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('048', 'Bahrain', 'BHR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('050', 'Bangladesh', 'BGD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('052', 'Barbados', 'BRB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('112', 'Belarus', 'BLR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('056', 'Belgium', 'BEL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('084', 'Belize', 'BLZ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('204', 'Benin', 'BEN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('060', 'Bermuda', 'BMU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('064', 'Bhutan', 'BTN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('068', 'Bolivia (Plurinational State of)', 'BOL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('535', 'Bonaire, Sint Eustatius and Saba', 'BES')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('070', 'Bosnia and Herzegovina', 'BIH')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('072', 'Botswana', 'BWA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('076', 'Brazil', 'BRA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('092', 'British Virgin Islands', 'VGB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('096', 'Brunei Darussalam', 'BRN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('100', 'Bulgaria', 'BGR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('854', 'Burkina Faso', 'BFA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('108', 'Burundi', 'BDI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('132', 'Cabo Verde', 'CPV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('116', 'Cambodia', 'KHM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('120', 'Cameroon', 'CMR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('124', 'Canada', 'CAN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('136', 'Cayman Islands', 'CYM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('140', 'Central African Republic', 'CAF')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('148', 'Chad', 'TCD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('830', 'Channel Islands	', '')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('152', 'Chile', 'CHL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('156', 'China', 'CHN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('344', 'China, Hong Kong Special Administrative Region', 'HKG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('446', 'China, Macao Special Administrative Region', 'MAC')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('170', 'Colombia', 'COL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('174', 'Comoros', 'COM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('178', 'Congo', 'COG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('184', 'Cook Islands', 'COK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('188', 'Costa Rica', 'CRI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('384', 'Côte d''Ivoire', 'CIV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('191', 'Croatia', 'HRV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('192', 'Cuba', 'CUB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('531', 'Curaçao', 'CUW')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('196', 'Cyprus', 'CYP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('203', 'Czech Republic', 'CZE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('408', 'Democratic People''s Republic of Korea', 'PRK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('180', 'Democratic Republic of the Congo', 'COD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('208', 'Denmark', 'DNK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('262', 'Djibouti', 'DJI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('212', 'Dominica', 'DMA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('214', 'Dominican Republic', 'DOM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('218', 'Ecuador', 'ECU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('818', 'Egypt', 'EGY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('222', 'El Salvador', 'SLV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('226', 'Equatorial Guinea', 'GNQ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('232', 'Eritrea', 'ERI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('233', 'Estonia', 'EST')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('231', 'Ethiopia', 'ETH')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('234', 'Faeroe Islands', 'FRO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('238', 'Falkland Islands (Malvinas)', 'FLK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('242', 'Fiji', 'FJI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('246', 'Finland', 'FIN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('250', 'France', 'FRA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('254', 'French Guiana', 'GUF')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('258', 'French Polynesia', 'PYF')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('266', 'Gabon', 'GAB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('270', 'Gambia', 'GMB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('268', 'Georgia', 'GEO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('276', 'Germany', 'DEU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('288', 'Ghana', 'GHA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('292', 'Gibraltar', 'GIB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('300', 'Greece', 'GRC')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('304', 'Greenland', 'GRL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('308', 'Grenada', 'GRD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('312', 'Guadeloupe', 'GLP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('316', 'Guam', 'GUM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('320', 'Guatemala', 'GTM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('831', 'Guernsey', 'GGY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('324', 'Guinea', 'GIN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('624', 'Guinea-Bissau', 'GNB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('328', 'Guyana', 'GUY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('332', 'Haiti', 'HTI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('336', 'Holy See', 'VAT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('340', 'Honduras', 'HND')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('348', 'Hungary', 'HUN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('352', 'Iceland', 'ISL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('356', 'India', 'IND')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('360', 'Indonesia', 'IDN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('364', 'Iran (Islamic Republic of)', 'IRN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('368', 'Iraq', 'IRQ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('372', 'Ireland', 'IRL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('833', 'Isle of Man', 'IMN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('376', 'Israel', 'ISR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('380', 'Italy', 'ITA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('388', 'Jamaica', 'JAM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('392', 'Japan', 'JPN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('832', 'Jersey	JEY', '')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('400', 'Jordan', 'JOR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('398', 'Kazakhstan', 'KAZ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('404', 'Kenya', 'KEN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('296', 'Kiribati', 'KIR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('414', 'Kuwait', 'KWT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('417', 'Kyrgyzstan', 'KGZ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('418', 'Lao People''s Democratic Republic', 'LAO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('428', 'Latvia', 'LVA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('422', 'Lebanon', 'LBN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('426', 'Lesotho', 'LSO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('430', 'Liberia', 'LBR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('434', 'Libya', 'LBY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('438', 'Liechtenstein', 'LIE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('440', 'Lithuania', 'LTU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('442', 'Luxembourg', 'LUX')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('450', 'Madagascar', 'MDG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('454', 'Malawi', 'MWI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('458', 'Malaysia', 'MYS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('462', 'Maldives', 'MDV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('466', 'Mali', 'MLI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('470', 'Malta', 'MLT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('584', 'Marshall Islands', 'MHL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('474', 'Martinique', 'MTQ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('478', 'Mauritania', 'MRT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('480', 'Mauritius', 'MUS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('175', 'Mayotte	MYT', '')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('484', 'Mexico', 'MEX')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('583', 'Micronesia (Federated States of)', 'FSM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('492', 'Monaco', 'MCO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('496', 'Mongolia', 'MNG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('499', 'Montenegro', 'MNE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('500', 'Montserrat', 'MSR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('504', 'Morocco', 'MAR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('508', 'Mozambique', 'MOZ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('104', 'Myanmar', 'MMR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('516', 'Namibia', 'NAM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('520', 'Nauru', 'NRU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('524', 'Nepal', 'NPL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('528', 'Netherlands', 'NLD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('540', 'New Caledonia', 'NCL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('554', 'New Zealand', 'NZL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('558', 'Nicaragua', 'NIC')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('562', 'Niger', 'NER')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('566', 'Nigeria', 'NGA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('570', 'Niue', 'NIU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('574', 'Norfolk Island', 'NFK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('580', 'Northern Mariana Islands', 'MNP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('578', 'Norway', 'NOR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('512', 'Oman', 'OMN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('586', 'Pakistan', 'PAK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('585', 'Palau', 'PLW')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('591', 'Panama', 'PAN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('598', 'Papua New Guinea', 'PNG')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('600', 'Paraguay', 'PRY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('604', 'Peru', 'PER')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('608', 'Philippines', 'PHL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('612', 'Pitcairn', 'PCN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('616', 'Poland', 'POL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('620', 'Portugal', 'PRT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('630', 'Puerto Rico', 'PRI')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('634', 'Qatar', 'QAT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('410', 'Republic of Korea', 'KOR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('498', 'Republic of Moldova', 'MDA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('638', 'Réunion', 'REU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('642', 'Romania', 'ROU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('643', 'Russian Federation', 'RUS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('646', 'Rwanda', 'RWA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('652', 'Saint Barthélemy', 'BLM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('654', 'Saint Helena', 'SHN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('659', 'Saint Kitts and Nevis', 'KNA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('662', 'Saint Lucia', 'LCA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('663', 'Saint Martin (French part)	MAF', '')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('666', 'Saint Pierre and Miquelon', 'SPM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('670', 'Saint Vincent and the Grenadines', 'VCT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('882', 'Samoa', 'WSM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('674', 'San Marino', 'SMR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('678', 'Sao Tome and Principe', 'STP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('680', 'Sark', ' ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('682', 'Saudi Arabia', 'SAU')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('686', 'Senegal', 'SEN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('688', 'Serbia', 'SRB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('690', 'Seychelles', 'SYC')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('694', 'Sierra Leone', 'SLE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('702', 'Singapore', 'SGP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('534', 'Sint Maarten (Dutch part)	SXM', '')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('703', 'Slovakia', 'SVK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('705', 'Slovenia', 'SVN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('090', 'Solomon Islands', 'SLB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('706', 'Somalia', 'SOM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('710', 'South Africa', 'ZAF')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('728', 'South Sudan', 'SSD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('724', 'Spain', 'ESP')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('144', 'Sri Lanka', 'LKA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('275', 'State of Palestine', 'PSE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('729', 'Sudan', 'SDN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('740', 'Suriname', 'SUR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('744', 'Svalbard and Jan Mayen Islands', 'SJM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('748', 'Swaziland', 'SWZ')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('752', 'Sweden', 'SWE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('756', 'Switzerland', 'CHE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('760', 'Syrian Arab Republic', 'SYR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('762', 'Tajikistan', 'TJK')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('764', 'Thailand', 'THA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('807', 'The former Yugoslav Republic of Macedonia', 'MKD')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('626', 'Timor-Leste', 'TLS')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('768', 'Togo', 'TGO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('772', 'Tokelau', 'TKL')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('776', 'Tonga', 'TON')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('780', 'Trinidad and Tobago', 'TTO')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('788', 'Tunisia', 'TUN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('792', 'Turkey', 'TUR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('795', 'Turkmenistan', 'TKM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('796', 'Turks and Caicos Islands', 'TCA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('798', 'Tuvalu', 'TUV')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('800', 'Uganda', 'UGA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('804', 'Ukraine', 'UKR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('784', 'United Arab Emirates', 'ARE')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('826', 'United Kingdom of Great Britain and Northern Ireland', 'GBR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('834', 'United Republic of Tanzania', 'TZA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('840', 'United States of America', 'USA')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('850', 'United States Virgin Islands', 'VIR')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('858', 'Uruguay', 'URY')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('860', 'Uzbekistan', 'UZB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('548', 'Vanuatu', 'VUT')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('862', 'Venezuela (Bolivarian Republic of)', 'VEN')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('704', 'Viet Nam', 'VNM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('876', 'Wallis and Futuna Islands', 'WLF')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('732', 'Western Sahara', 'ESH')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('887', 'Yemen', 'YEM')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('894', 'Zambia', 'ZMB')
GO
insert into AreaCodes (Code, Display, Abbrev) values ('716', 'Zimbabwe', 'ZWE')
