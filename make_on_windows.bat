cd src
copy gnoga-application.windows gnoga-application.adb
gnatmake -P gnoga.gpr
cd ..
cd demo
cd snake
gnatmake -P snake.gpr