  var ws;
  var adr;
  var reconnects=0;
  var params={};
  var gnoga={};
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

  $( document ).ready(function() {
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
     } else {
        adr = "ws://" + location.hostname;
     }

     if (location.port != "") { adr = adr + ":" + location.port; }
     adr = adr + "/gnoga";

     try {
        console.log ("connecting to " + adr);
        ws = new WebSocket (adr);
     } catch (e) {
        console.log ("trying again, connecting to " + adr);
        ws = new WebSocket (adr);
     }

     if (ws != null) {
        ws.onopen = function (event) {
           console.log ("connection successful");
           Setup_ws();
        }
        pingerid = setInterval (function () {Ping_ws ();}, 10000);
     } else {
        document.writeln ("If you are seeing this your browser or your connection to the internet is blocking websockets.");
     }
  });
