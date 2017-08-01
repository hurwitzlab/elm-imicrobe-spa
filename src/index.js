'use strict';

require("bootstrap-loader");

// Require index.html so it gets copied to dist
require('../index.html');

// Load config file
var config = require('../config.json');

// Start up Elm app
var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode, config);

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
    /*We store the Google Map object in Elm*/
    app.ports.receiveMap.send(gmap);
});

app.ports.setCenter.subscribe(function(model) {
    var myLatlng = new Google.maps.LatLng(model.center.lat, model.center.lng);
    model.gmap.setCenter(myLatlng);
});

// Define ports for saving/retrieving auth token
app.ports.saveAuthToken.subscribe(function(token) {
    console.log("saveAuthToken: ", token);
    localStorage.setItem("token", token);
});

//app.ports.getAuthToken.subscribe(function(token) {
//    var token = localStorage.getItem("token");
//    console.log("getAuthToken: ", token);
//    app.ports.receiveToken.send(token);
//});