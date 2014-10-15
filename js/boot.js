  var ws;
  var params={};
  var gnoga={};

  $( document ).ready(function() {
     var s = document.location.search;
     var tokens;
     var r = /[?&]?([^=]+)=([^&]*)/g;

     s = s.split("+").join(" ");

     while (tokens = r.exec(s)) {
        params[decodeURIComponent(tokens[1])]
           = decodeURIComponent(tokens[2]);
     }

     ws = new WebSocket ("ws://" + location.hostname + ":" + location.port + "/gnoga");

     if (ws != null) {
        ws.onmessage = function (event) {
           try {
              eval (event.data);
           } catch (e) {
              console.error (e.message);
           }
        }
        ws.onerror = function (event) {
           alert ("Communication error");
        }
        ws.onclose = function (event) {
           if (gnoga['html_on_close'] != "") {
              $(document.body).html(gnoga['html_on_close']);
           } else {
              alert ("Server connection lost " + event.reason);
           }
        }
     } else {
        alert ("Websocket connection is required.");
     }
  });
