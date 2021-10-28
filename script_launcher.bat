if not exist "C:\mevis_data\" mkdir C:\mevis_data
COPY "%~dp0\config.yml" "C:\mevis_data\config.yml"

@echo off

set "rscript=%~dp0\script.R" 

for /r "d:\Program Files" %%F in (*Rscript.exe*) do (
  "%%~fF" "%rscript%" %*
  goto :eof
)

echo No Rscript.exe found. Maybe you need to install R.
pause


Title "New Windows Title"
timeout 2 >nul
msg * /time:4  "Succesfully failed"
Echo x=msgbox^("I'm Done",0,"Title"^)>"%temp%\msg.vbs"
start %temp%\msg.vbs

