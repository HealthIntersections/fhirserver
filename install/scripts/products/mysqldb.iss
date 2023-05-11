[CustomMessages]
mysqldb_size=300 MB - 500 MB

[Code]
var
  mysql_url: string;
  mysqldb_title: string;
  mysql_product: string;
 
procedure mysqldb;
begin
  if (IsX86()) then begin
	mysql_url := 'https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-web-community-5.7.20.0.msi';
	mysqldb_title := 'MySQL 5.7 x86'
	mysql_product := 'mysql-installer-web-community-5.7.20.0.msi';
  end else if (IsX64()) then begin
	mysql_url := 'https://dev.mysql.com/get/Downloads/MySQLInstaller/mysql-installer-web-community-5.7.20.0.msi';      
	mysqldb_title := 'MySQL 5.7 x64'
	mysql_product := 'mysql-installer-web-community-5.7.20.0.msi';  
  end;

  AddProduct(
	mysql_product,
	' /passive /norestart',        
	mysqldb_title,
	CustomMessage('mysqldb_size'),        
	mysql_url,
	false, false, false);
end;