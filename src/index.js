'use strict';

require("bootstrap-loader");

// Require these files so they get copied to dist
require('../index.html');
require("../img/nav-header.png");
require('../css/imicrobe.css');
require('../css/readable.min.css');

// Load config file
var config = require('../config.json');

// Start up Elm app
var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode, {
    config: config,
    session: localStorage.session || ""
});

// Initial Google Maps and define Elm ports
var GoogleMapsLoader = require("google-maps");
GoogleMapsLoader.KEY = config.googleApiKey;
var Google;
GoogleMapsLoader.load(function(google) {
    Google = google;
});

app.ports.loadMap.subscribe(function(model) {
    var mapDiv = document.getElementsByTagName('gmap')[0];

    var myLatlng = new Google.maps.LatLng(model.lat, model.lng);
    var mapOptions = {
      zoom: 6,
      center: myLatlng
    };
    var gmap = new Google.maps.Map(mapDiv, mapOptions);
    app.ports.receiveMap.send(gmap);
});

app.ports.setCenter.subscribe(function(model) {
    var myLatlng = new Google.maps.LatLng(model.center.lat, model.center.lng);
    model.gmap.setCenter(myLatlng);
});

// Define ports for storing/watching session
app.ports.storeSession.subscribe(function(session) {
    console.log("storeSession: ", session);
    localStorage.session = session;
});

window.addEventListener("storage",
    function(event) {
        if (event.storageArea === localStorage && event.key === "session") {
            console.log("storage listener: ", event.newValue);
            app.ports.onSessionChange.send(event.newValue);
        }
    },
    false
);
