if not exist bin mkdir bin
if not exist obj mkdir obj
if not exist lib mkdir lib
cd src
copy gnoga-application.windows gnoga-application.adb
gnatmake -P gnoga.gpr
cd ..
cd demo
cd snake
gnatmake -P snake.gpr
cd ..\..\ 

