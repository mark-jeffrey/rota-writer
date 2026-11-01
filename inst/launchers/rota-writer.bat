@echo off
rem Double-click to open the rota-writer app. Needs R with the rotawriter package installed.
Rscript -e "rotawriter::run_app()"
if errorlevel 1 pause
