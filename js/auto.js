  var ws;
  var new_ws=null;
  var adr;
  var xadr;
  var reconnects=0;
  var params={};
  var pingerid;

  if (typeof gnoga_debug == 'undefined') {
     gnoga_debug = false;
  }

  function Ping_ws() {
     if (ws.readyState == 1) {
        ws.send ("0");
     }
  }

  function Shutdown_ws() {
     ws = null;
     clearInterval (pingerid);
     if (gnoga['html_on_close'] != "") {
        $(document.body).html(gnoga['html_on_close']);
      } else {
        alert ("Server connection lost " + event.reason);
      }
  }

  function Setup_ws() {
     ws.onmessage = function (event) {
        try {
           if (gnoga_debug == true) {
              console.log ("eval data = " + event.data);
           }
           eval (event.data);
        } catch (e) {
           console.error (e.message);
        }
     }

     ws.onerror = function (event) {
        console.log ("onerror: reconnect");
        ws=null;
        ws = new WebSocket (adr  + "?Old_ID=" + gnoga['Connection_ID']);
        ws.onopen = function (event) {
           console.log ("onerror: reconnect successful");
           Setup_ws();
        }
        ws.onclose = function (event) {
           console.log ("onerror: reconnect failure");
           Shutdown_ws();
        }
     }

     ws.onclose = function (event) {
        console.log ("onclose: reconnect");
        ws=null;
        ws = new WebSocket (adr  + "?Old_ID=" + gnoga['Connection_ID']);
        ws.onopen = function (event) {
           console.log ("onclose: reconnect successful");
           Setup_ws();
        }
        ws.onclose = function (event) {
           console.log ("onclose: reconnect failure");
           Shutdown_ws();
        }
     }
  }

  var s = document.location.search;
  var tokens;
  var r = /[?&]?([^=]+)=([^&]*)/g;

  s = s.split("+").join(" ");

  while (tokens = r.exec(s)) {
     params[decodeURIComponent(tokens[1])]
        = decodeURIComponent(tokens[2]);
  }

  if (location.protocol == "https:") {
     adr = "wss://" + location.hostname;
     xadr = "https://" + location.hostname;
  } else {
     adr = "ws://" + location.hostname;
     xadr = "http://" + location.hostname;
  }

  if (location.port != "") {
     adr = adr + ":" + location.port;
     xadr = xadr + ":" + location.port;
  }

  adr = adr + "/gnoga";
  xadr = xadr + "/gnoga_ajax";

  try {
     console.log ("websocket connecting to " + adr + "?Old_ID=" + gnoga['Connection_ID']);
     new_ws = new WebSocket (adr + "?Old_ID=" + gnoga['Connection_ID']);
  } catch (e) {
     console.log ("trying again, connecting to " + adr);
     new_ws = new WebSocket (adr + "?Old_ID=" + gnoga['Connection_ID']);
  }

  ws = new Object;

  ws.send = function (message) {
     var xhr = new XMLHttpRequest();

     xhr.ontimeout = function () {
        ws.onerror ("The request for " + xadr + " timed out.");
     };

     xhr.onload = function() {
        if (xhr.readyState === 4) {
           if (xhr.status != 200) {
              ws.onerror (xhr.statusText);
           }
       }
    };

    xhr.onerror = function () {
       console.error ("gnoga_ajax error - " & xhr.statusText);
    };

    xhr.open ("GET", xadr + "?m=" + encodeURIComponent(message), true);
    xhr.send (null);
  }

  if (new_ws != null) {
     ws = new_ws;

     ws.onopen = function (event) {
        console.log ("websocket connection successful");
        Setup_ws();
     }

     pingerid = setInterval (function () {Ping_ws ();}, 10000);
  }
