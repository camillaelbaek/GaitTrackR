@echo off
cd /d "%~dp0"

where Rscript >nul 2>nul
if %errorlevel% neq 0 (
  echo Rscript not found. Please install R from https://cran.r-project.org and try again.
  pause
  exit /b 1
)

Rscript run_app.R
pause

