  var ws;
  var adr;
  var reconnects=0;
  var params={};
  var gnoga={};
  var pingerid;

  function Ping_ws() {
     if (ws.readyState == 1) {
        ws.send ("0");
     }
  }

  function Setup_ws() {
        reconnects++;
        ws.onmessage = function (event) {
           reconnects = 0;
           try {
              eval (event.data);
           } catch (e) {
              console.error (e.message);
           }
        }
        ws.onerror = function (event) {
           if (gnoga['Connection_ID'] == 'undefined') {
              location.reload(true);
           }

           if (reconnects < 2) {
              console.log ("onerror: reconnect " + reconnects);
              ws.onclose=null;
              ws = new WebSocket (adr  + "?Old_ID=" + gnoga['Connection_ID']);
              setTimeout (Setup_ws, 100);
           } else {
              clearInterval (pingerid);
              if (gnoga['html_on_close'] != "") {
                 $(document.body).html(gnoga['html_on_close']);
              } else {
                 alert ("Server connection lost " + event.reason);
              }
           }
        }
        ws.onclose = function (event) {
           if (reconnects < 2) {
              console.log ("onerror: reconnect " + reconnects);
              ws.onclose=null;
              ws = new WebSocket (adr  + "?Old_ID=" + gnoga['Connection_ID']);
              setTimeout (Setup_ws, 100);
           } else {
              clearInterval (pingerid);
              if (gnoga['html_on_close'] != "") {
                 $(document.body).html(gnoga['html_on_close']);
              } else {
                 alert ("Server connection lost " + event.reason);
              }
           }
        }
  }

  $( document ).ready(function() {
     var s = document.location.search;
     var tokens;
     var r = /[?&]?([^=]+)=([^&]*)/g;

     s = s.split("+").join(" ");

     while (tokens = r.exec(s)) {
        params[decodeURIComponent(tokens[1])]
           = decodeURIComponent(tokens[2]);
     }

     adr = "ws://" + location.hostname;
     if (location.port != "") { adr = adr + ":" + location.port; }
     adr = adr + "/gnoga";

     ws = new WebSocket (adr);

     if (ws != null) {
        setTimeout (Setup_ws, 10);
        pingerid = setInterval (function () {Ping_ws ();}, 10000);
     } else {
        document.writeln ("If you are seeing this your browser or your connection to the internet is blocking websockets.");
     }
  });
