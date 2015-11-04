CREATE TABLE [dbo].[CountryCodes](
	[Display] [nchar](255) NOT NULL,
	[Code] [nchar](10) NOT NULL,
	[Status] [nchar](255) NOT NULL,
 CONSTRAINT [PK_CountryCodes] PRIMARY KEY CLUSTERED 
(
	[Code] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

insert into CountryCodes (display, code, status) values ('USSR', 'SU', 'Exceptionally reserved')
GO
insert into CountryCodes (display, code, status) values ('United States Miscellaneous Pacific Islands', 'PU', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('New Hebrides', 'NH', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Johnston Island', 'JT', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Dronning Maud Land', 'NQ', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Wake Island', 'WK', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Panama Canal Zone', 'PZ', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Midway Islands', 'MI', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Yemen, Democratic', 'YD', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Dahomey', 'DY', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Southern Rhodesia', 'RH', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Canton and Enderbury Islands', 'CT', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Viet-Nam, Democratic Republic of', 'VD', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('French Southern and Antarctic Territories', 'FQ', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('German Democratic Republic', 'DD', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Pacific Islands (Trust Territory)', 'PC', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Upper Volta', 'HV', 'Formerly used')
GO
insert into CountryCodes (display, code, status) values ('Australia', 'AU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Côte d''Ivoire', 'CI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Nepal', 'NP', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Jordan', 'JO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Algeria', 'DZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Lucia', 'LC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bermuda', 'BM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Slovenia', 'SI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Malta', 'MT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Iceland', 'IS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Kyrgyzstan', 'KG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tanzania, United Republic of', 'TZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Eritrea', 'ER', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Viet Nam', 'VN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('French Guiana', 'GF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Paraguay', 'PY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Antarctica', 'AQ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Netherlands (the)', 'NL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Fiji', 'FJ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Wallis and Futuna', 'WF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Burundi', 'BI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sweden', 'SE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Northern Mariana Islands (the)', 'MP', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('British Indian Ocean Territory (the)', 'IO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Czech Republic (the)', 'CZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tuvalu', 'TV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('United Kingdom of Great Britain and Northern Ireland (the)', 'GB', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Armenia', 'AM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Canada', 'CA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Lithuania', 'LT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Swaziland', 'SZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Belgium', 'BE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saudi Arabia', 'SA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mali', 'ML', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guinea-Bissau', 'GW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cabo Verde', 'CV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Turkey', 'TR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Anguilla', 'AI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Réunion', 'RE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Belize', 'BZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('El Salvador', 'SV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bosnia and Herzegovina', 'BA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Marshall Islands (the)', 'MH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('South Georgia and the South Sandwich Islands', 'GS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Costa Rica', 'CR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tunisia', 'TN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Pierre and Miquelon', 'PM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('United Arab Emirates (the)', 'AE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Samoa', 'WS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Hong Kong', 'HK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bouvet Island', 'BV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Suriname', 'SR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Djibouti', 'DJ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Korea (the Democratic People''s Republic of)', 'KP', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Moldova (the Republic of)', 'MD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Azerbaijan', 'AZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('China', 'CN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tajikistan', 'TJ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Niue', 'NU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Brazil', 'BR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Senegal', 'SN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Malaysia', 'MY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Oman', 'OM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Qatar', 'QA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('French Southern Territories (the)', 'TF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Peru', 'PE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Japan', 'JP', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Faroe Islands (the)', 'FO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Brunei Darussalam', 'BN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Svalbard and Jan Mayen', 'SJ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mauritius', 'MU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Italy', 'IT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cambodia', 'KH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Spain', 'ES', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guernsey', 'GG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Argentina', 'AR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Central African Republic (the)', 'CF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Libya', 'LY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Panama', 'PA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mayotte', 'YT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('United States of America (the)', 'US', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Falkland Islands (the) [Malvinas]', 'FK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Benin', 'BJ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Martinique', 'MQ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Taiwan (Province of China)', 'TW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Luxembourg', 'LU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Nicaragua', 'NI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Haiti', 'HT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Burkina Faso', 'BF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cayman Islands (the)', 'KY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Solomon Islands', 'SB', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Myanmar', 'MM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Israel', 'IL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Curaçao', 'CW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Virgin Islands (British)', 'VG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Puerto Rico', 'PR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Niger (the)', 'NE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Dominican Republic (the)', 'DO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Barbados', 'BB', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guatemala', 'GT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tonga', 'TO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Egypt', 'EG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('New Zealand', 'NZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Vincent and the Grenadines', 'VC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Pitcairn', 'PN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Afghanistan', 'AF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Namibia', 'NA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Botswana', 'BW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('South Sudan', 'SS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Denmark', 'DK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Uganda', 'UG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Montenegro', 'ME', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guadeloupe', 'GP', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Indonesia', 'ID', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Rwanda', 'RW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Colombia', 'CO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Tokelau', 'TK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Ecuador', 'EC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Liechtenstein', 'LI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bahamas (the)', 'BS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Somalia', 'SO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mozambique', 'MZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Comoros (the)', 'KM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Morocco', 'MA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Greenland', 'GL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Aruba', 'AW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Serbia', 'RS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cook Islands (the)', 'CK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Togo', 'TG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Nauru', 'NR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('French Polynesia', 'PF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bolivia (Plurinational State of)', 'BO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Slovakia', 'SK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Maldives', 'MV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Kiribati', 'KI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Ethiopia', 'ET', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Ghana', 'GH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('American Samoa', 'AS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Romania', 'RO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Congo (the)', 'CG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Turks and Caicos Islands (the)', 'TC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Jamaica', 'JM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Lao People''s Democratic Republic (the)', 'LA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Singapore', 'SG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mauritania', 'MR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Iraq', 'IQ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Zambia', 'ZM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Kenya', 'KE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Grenada', 'GD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Palau', 'PW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Angola', 'AO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cocos (Keeling) Islands (the)', 'CC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Latvia', 'LV', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Hungary', 'HU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bulgaria', 'BG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Kazakhstan', 'KZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Seychelles', 'SC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mongolia', 'MN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guyana', 'GY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Isle of Man', 'IM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Christmas Island', 'CX', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Trinidad and Tobago', 'TT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Palestine, State of', 'PS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Liberia', 'LR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Norfolk Island', 'NF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Jersey', 'JE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sint Maarten (Dutch part)', 'SX', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guam', 'GU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Western Sahara*', 'EH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Antigua and Barbuda', 'AG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Heard Island and McDonald Islands', 'HM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sao Tome and Principe', 'ST', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Korea (the Republic of)', 'KR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Martin (French part)', 'MF', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Equatorial Guinea', 'GQ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Ireland', 'IE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('South Africa', 'ZA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Timor-Leste', 'TL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Pakistan', 'PK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bhutan', 'BT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Yemen', 'YE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Kitts and Nevis', 'KN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Vanuatu', 'VU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Gambia (the)', 'GM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Åland Islands', 'AX', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Chile', 'CL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Thailand', 'TH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Papua New Guinea', 'PG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Uruguay', 'UY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sierra Leone', 'SL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Malawi', 'MW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Gibraltar', 'GI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Austria', 'AT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Switzerland', 'CH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Chad', 'TD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Norway', 'NO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Lebanon', 'LB', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Micronesia (Federated States of)', 'FM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Barthélemy', 'BL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Saint Helena, Ascension and Tristan da Cunha', 'SH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Montserrat', 'MS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Iran (Islamic Republic of)', 'IR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Georgia', 'GE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Congo (the Democratic Republic of the)', 'CD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Finland', 'FI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bahrain', 'BH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sudan (the)', 'SD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Macao', 'MO', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('India', 'IN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cyprus', 'CY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Virgin Islands (U.S.)', 'VI', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Gabon', 'GA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Portugal', 'PT', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Albania', 'AL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Lesotho', 'LS', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Nigeria', 'NG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Croatia', 'HR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Syrian Arab Republic', 'SY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('United States Minor Outlying Islands (the)', 'UM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bangladesh', 'BD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Kuwait', 'KW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Macedonia (the former Yugoslav Republic of)', 'MK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cuba', 'CU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Venezuela (Bolivarian Republic of)', 'VE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('New Caledonia', 'NC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Honduras', 'HN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Belarus', 'BY', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Dominica', 'DM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Madagascar', 'MG', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Greece', 'GR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Turkmenistan', 'TM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Estonia', 'EE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Holy See (the)', 'VA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Poland', 'PL', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Andorra', 'AD', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Sri Lanka', 'LK', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Zimbabwe', 'ZW', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Monaco', 'MC', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Guinea', 'GN', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Russian Federation (the)', 'RU', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Cameroon', 'CM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Philippines (the)', 'PH', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Uzbekistan', 'UZ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('France', 'FR', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Bonaire, Sint Eustatius and Saba', 'BQ', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('San Marino', 'SM', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Germany', 'DE', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Mexico', 'MX', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Ukraine', 'UA', 'Officially assigned')
GO
insert into CountryCodes (display, code, status) values ('Netherlands Antilles', 'AN', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('Serbia and Montenegro', 'CS', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('Yugoslavia', 'YU', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('East Timor', 'TP', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('Zaire', 'ZR', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('Burma', 'BU', 'Transitionally reserved')
GO
insert into CountryCodes (display, code, status) values ('Neutral Zone', 'NT', 'Transitionally reserved')
GO
