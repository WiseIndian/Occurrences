@echo off
rem bbou@ac-toulouse.fr
rem 25/11/2013 

set DB=wordnetOccurrences
set DBTYPE=mysql
set DBUSER=root
set /P DBPWD=Enter %DBUSER% password:
set MODULES=wn legacy vn bnc sumo xwn glf ilfwn logs


if "%1"=="-d" call :deletedb
call :dbexists
if not %DBEXISTS%==0 call :createdb


set PLAYUSER="playOccurrences"
mysql -u %DBUSER% --password=%DBPWD% -e "DROP USER '%PLAYUSER%'@'localhost';"
mysql -u %DBUSER% --password=%DBPWD% -e "CREATE USER '%PLAYUSER%'@'localhost';"
mysql -u %DBUSER% --password=%DBPWD% -e "GRANT SELECT ON %DB%.* TO '%PLAYUSER%'@'%';"
mysql -u %DBUSER% --password=%DBPWD% -e "grant SELECT on %DB%.* to '%PLAYUSER%'@'localhost';"

for %%M in (%MODULES%) do call :process %DBTYPE%-%%M-schema.sql "schema %%M" %DB%
for %%M in (%MODULES%) do call :process %DBTYPE%-%%M-data.sql "data %%M" %DB%
for %%M in (%MODULES%) do call :process %DBTYPE%-%%M-unconstrain.sql "unconstrain %%M" %DB% --force
for %%M in (%MODULES%) do call :process %DBTYPE%-%%M-constrain.sql "constrain %%M" %DB% --force
for %%M in (%MODULES%) do call :process %DBTYPE%-%%M-views.sql "views %%M" %DB%
goto :eof

:process
setlocal
if not exist %1 goto :endprocess
echo %2
mysql -u %DBUSER% --password=%DBPWD% %4 %3 < %1
:endprocess
endlocal
goto :eof

:dbexists
setlocal
mysql -u %DBUSER% --password=%DBPWD% -e "\q" %DB% > NUL 2> NUL
endlocal & set DBEXISTS=%ERRORLEVEL% 
goto :eof

:deletedb
setlocal
echo delete %DB%
mysql -u %DBUSER% --password=%DBPWD% -e "DROP DATABASE %DB%;"
endlocal
goto :eof

:createdb
setlocal
echo create %DB%
mysql -u %DBUSER% --password=%DBPWD% -e "CREATE DATABASE %DB% DEFAULT CHARACTER SET UTF8;"
endlocal
goto :eof

:eof
