CREATE TABLE [Unii](
	[UniiKey] int NOT NULL,
	[Code] nchar(20) NOT NULL,
	[Display] nchar(255) NULL,
 CONSTRAINT [PK_Unii] PRIMARY KEY CLUSTERED 
(
	[UniiKey] ASC
)
) 

GO

CREATE TABLE [UniiDesc](
	[UniiDescKey] int NOT NULL,
	[UniiKey] int NOT NULL,
	[Type] nchar(20) NOT NULL,
	[Display] nchar(255) NULL,
 CONSTRAINT [PK_UniiDesc] PRIMARY KEY CLUSTERED 
(
	[UniiDescKey] ASC
)
) 

GO

ALTER TABLE UniiDesc ADD  CONSTRAINT FK_ValueSetMembers_UniiKey FOREIGN KEY(UniiKey) REFERENCES Unii (UniiKey)
GO
