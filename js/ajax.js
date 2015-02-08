  var ws;
  var adr;
  var params={};
  var gnoga={};

  if (typeof gnoga_debug == 'undefined') {
     gnoga_debug = false;
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
     adr = "https://" + location.hostname;
  } else {
     adr = "http://" + location.hostname;
  }

  if (location.port != "") { adr = adr + ":" + location.port; }
  adr = adr + "/gnoga_ajax";

  ws = new Object;

  ws.send = function (message) {
     var xhr = new XMLHttpRequest();

     xhr.ontimeout = function () {
        ws.onerror ("The request for " + adr + " timed out.");
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

    xhr.open ("GET", adr + "?m=" + encodeURIComponent(message), true);
    xhr.send (null);
  }
