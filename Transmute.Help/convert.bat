@echo off

set outdir=%~p0out
set project=project.json
if not "%~1" == "" set project=%1

mkdir "%outdir%"
python3 split.py %project%
if errorlevel 1 exit /b
echo.

for /f "tokens=* USEBACKQ" %%a in (`jq -r .destination %project%`) do (set "destination=%%a")
for /f "tokens=* USEBACKQ" %%a in (`jq -r .template %project%`) do (set "template=%%a")
for /f "tokens=* USEBACKQ" %%a in (`jq -r .titlePrefix %project%`) do (set "titlePrefix=%%a")
for /f "tokens=* USEBACKQ" %%a in (`jq -r .author %project%`) do (set "author=%%a")

setlocal enabledelayedexpansion

set i=0

for %%f in ("%outdir%\*.md") do (
	for /f "tokens=* USEBACKQ" %%a in (`jq -r .sections[%i%].title %project%`) do (set "title=%%a")

	set filename=%%f
	set base=%%~nf
	echo converting !filename!...
	pandoc -f markdown -t html -M "author-meta=%author%" -M "title-prefix=%titlePrefix%" -M "pagetitle=%title%" --template "%template%" --lua-filter nowidths.lua "!filename!" > "%outdir%\!base!.html"
	if errorlevel 1 exit /b
	
	set /a i=i+1
)

setlocal disabledelayedexpansion

set cwd=%cd%
cd "%outdir%"
copy ..\source\styles.css
copy ..\source\help.js
tar czf help.tar.gz *.html styles.css help.js toc.json
cd "%cwd%"
copy "%outdir%\help.tar.gz" "%destination%"

echo.
echo done!
