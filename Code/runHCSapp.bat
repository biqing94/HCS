set Rexe="c:\Program Files\R\R-4.0.3\bin\Rscript.exe"
set appFolder='c:\\HCS'
%Rexe% -e "shiny::runApp(%appFolder%, launch.browser=TRUE)"