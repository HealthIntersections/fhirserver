[CustomMessages]
mysqlodbc_size=3 MB - 18 MB

[Code]
const
	mysqlodbc_url =     'https://dev.mysql.com/get/Downloads/Connector-ODBC/5.2/mysql-connector-odbc-5.2.7-win32.msi';
	mysqlodbc_url_x64 = 'https://dev.mysql.com/get/Downloads/Connector-ODBC/5.2/mysql-connector-odbc-5.2.7-winx64.msi';
	

var
//  mysql_url: string;
  mysqlodbc_title: string;
//  mysql_product: string;

procedure mysqlodbc(Version: string);
begin
{    if (not mysqlodbcinstalled(MySQL351)) then begin
      if (IsX86()) then begin
        mysql_url := 'https://dev.mysql.com/get/Downloads/Connector-ODBC/3.51/mysql-connector-odbc-3.51.28-win32.msi';
        mysqlodbc_title := 'MySQL ODBC Driver 3.51 x86'
        mysql_product := 'mysql-connector-odbc-3.51.28-win32.msi';
      end else if (IsX64()) then begin
        mysql_url := 'https://dev.mysql.com/get/Downloads/Connector-ODBC/3.51/mysql-connector-odbc-3.51.28-winx64.msi';      
        mysqlodbc_title := 'MySQL ODBC Driver 3.51 x64'
        mysql_product := 'mysql-connector-odbc-3.51.28-winx64.msi';  
      end;

      AddProduct(
        mysql_product,
        ' /passive /norestart',        
        mysqlodbc_title,
        CustomMessage('mysqlodbc_size'),        
        mysql_url,
        false, false);
    end;
  end;
}
end;