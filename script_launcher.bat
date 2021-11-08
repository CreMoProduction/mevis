TITLE mevis 
@echo off

if not exist "C:\mevis_data\" mkdir C:\mevis_data
COPY "%~dp0\config.yml" "C:\mevis_data\config.yml"
cls
setlocal enabledelayedexpansion
set "dot=."
set "msg=initializing mevis"
for /L %%A in (1,1,4) do (
	set msg=!msg!%dot%
	echo !msg!
	timeout 1 >nul
	ping 127.0.0.1 -n 1 > nul
	cls
)
echo !msg!
rem specify path and script name
set "rscript=%~dp0\script.R" 

rem define if R.exe is installed and use its path to launch script.R
for /r "d:\Program Files" %%F in (*Rscript.exe*) do (
	"%%~fF" "%rscript%" %*

	rem msg * /time:4  "Succesfully failed"
	Echo x=msgbox^("mevis succesfully finished data processing",64,"mevis"^)>"%temp%\msg.vbs"
	start %temp%\msg.vbs
	pause
	timeout 2 >nul
  	goto :eof
)
echo ---mevis error---
echo.
echo No Rscript.exe found. 
echo Maybe you need to install R or make sure it is installed in 'C:\Program Files\...'
echo Check for details on mevis webpage https://github.com/CreMoProduction/mevis
echo.  
echo Do you want to download R?
echo.
pause
call :MsgBox "Would you like to download R?"  "VBYesNo+VBQuestion" "mevis"
    if errorlevel 7 (
        echo NO - don't go to the url
    ) else if errorlevel 6 (
        echo YES - go to the url
        start "" "https://cran.r-project.org/bin/windows/base/"
    )

    exit /b

:MsgBox prompt type title
    setlocal enableextensions
    set "tempFile=%temp%\%~nx0.%random%%random%%random%vbs.tmp"
    >"%tempFile%" echo(WScript.Quit msgBox("%~1",%~2,"%~3") & cscript //nologo //e:vbscript "%tempFile%"
    set "exitCode=%errorlevel%" & del "%tempFile%" >nul 2>nul
    endlocal & exit /b %exitCode%






