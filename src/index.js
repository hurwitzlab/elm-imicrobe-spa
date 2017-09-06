'use strict';

require("bootstrap-loader");
import * as agave from '../node_modules/agave-file-browser/AgaveFileBrowser.js';
import 'jstree';

// Require these files so they get copied to dist
require('../index.html');
require("../img/nav-header.png");
require('../css/imicrobe.css');
//require('../css/readable.min.css');
require('../css/spinner.css');

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

var fileBrowser;
app.ports.createFileBrowser.subscribe(function(input) { // TODO get username/token from localStorage.session instead of passing in
    if (typeof fileBrowser == 'undefined') {
        fileBrowser = new agave.AgaveFileBrowser({
            elementId: "file-browser",
            baseUrl:   "https://agave.iplantc.org/files/v2/listings", // TODO move base URL into config.json
            path:      input.username,
            authToken: input.token,
            selectCallback: function(node) {
                console.log("selected: ", node);
                app.ports.onFileSelect.send({
                    id: input.id,
                    username: input.username,
                    token: input.token,
                    path: node.id
                });
            }
        });
    }

    $('#file-browser-dialog').modal('show');
});
