@echo off
set project=project.json
set outdir=%~p0out

if not "%~1" == "" (
	set project=%1
	call :normalize %1
)

echo Transmute.Help: compiling %project%...

mkdir "%outdir%"
python3 compile.py %project%
if errorlevel 1 exit /b

for /f "tokens=* USEBACKQ" %%a in (`jq -r .destination %project%`) do (set "destination=%%~fa")

echo.
echo Archiving %destination%...
set cwd=%cd%
cd "%outdir%"

copy ..\source\styles.css
copy ..\source\help.js
tar cvzf help.tar.gz *.html styles.css help.js toc.json
copy "%outdir%\help.tar.gz" "%destination%"

cd "%cwd%"
echo.
echo done!
exit /b

:normalize
	set outdir=%~p1out
	exit /b
