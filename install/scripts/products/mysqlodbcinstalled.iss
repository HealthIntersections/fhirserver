[Code]
const    
  //mysqlodbc_reg = 'SOFTWARE\ODBC\ODBCINST.INI\';
  mysqlodbc_reg = 'SOFTWARE\MySQL AB\';
type
    MySQLODBCType = (MySQL351, MySQL501, MySQL53);

function mysqlodbcinstalled(odbcversion: MySQLODBCType): boolean;
var
  MyResult: boolean;
begin
    MyResult:= False;
    case odbcversion of            
      MySQL351:
                if RegKeyExists(HKLM, mysqlodbc_reg + 'MySQL Connector/ODBC 3.51') then
        begin
          MyResult := True;
        end;
      MySQL501:
                if RegKeyExists(HKLM, mysqlodbc_reg + 'MySQL Connector/ODBC 5.01') then
        begin
          MyResult := True;
        end;      
      MySQL53:
        if RegKeyExists(HKLM, mysqlodbc_reg + 'MySQL Connector/ODBC 5.3') then
        begin
          MyResult := True;
        end; 
    end;
    Result := MyResult;
end;