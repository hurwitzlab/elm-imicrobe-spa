'use strict';

require("bootstrap-loader");
import * as agave from '../node_modules/agave-file-browser/AgaveFileBrowser.js';
import * as simplots from '../node_modules/sim-plots/sim-plots.js';
import 'jstree';

// Require these files so they get copied to dist
require('../index.html');
require("../img/nav-header.png");
require('../css/imicrobe.css');
//require('../css/readable.min.css');
require('../css/spinner.css');
require('../node_modules/sim-plots/main.css');
require('../node_modules/agave-file-browser/spinner.gif');
require.context("../plugins/landing-page", true, /^\.\/.*\.(jpg|png|html|css)/);

// Load config file
var config = require('../config.json');

// Start up Elm app
var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode, {
    session: localStorage.session || ""
});


/*
 * Initialize Google Maps and define ports
 */

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


/*
 * Define ports for storing/watching session
 */

app.ports.storeSession.subscribe(function(session) {
    console.log("storeSession: ", session);
    localStorage.session = session;
});

// This event is only triggered when localStorage is modifed from another window
window.addEventListener("storage",
    function(event) {
        if (event.storageArea === localStorage && event.key === "session") {
            console.log("storage listener:", event.newValue);
            app.ports.onSessionChange.send(event.newValue);
        }
    },
    false
);


/*
 * Define ports for Agave File Browser
 */

var fileBrowser;
app.ports.createFileBrowser.subscribe(function(input) { // TODO get username/token from localStorage.session instead of passing in
    var dialog = $('#file-browser-dialog');

    if (typeof fileBrowser == 'undefined') {
        fileBrowser = new agave.AgaveFileBrowser({
            elementId:   'file-browser',
            baseUrl:     config.agaveFilesUrl,
            queryParams: 'limit=9999',
            path:        input.username,
            authToken:   input.token,
            busyIconUrl: 'img/spinner.gif'
        });
    }

    // Needs to be redefined for each call as input changes
    dialog.find('button.btn-primary').unbind().click(function() {
        var files = fileBrowser.getSelectedNodes();
        console.log('selected:', files);

        dialog.modal('hide');

        app.ports.onFileSelect.send({
            id: input.id,
            username: input.username,
            token: input.token,
            path: files.map(f => f.id).join(';')
        });
    });

    dialog.modal('show');
});


/*
 * Initialize Google Analytics and define port
 */

(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

var analyticsInitialized = false;
app.ports.updateAnalytics.subscribe(function (page) {
    if (!analyticsInitialized) {
        analyticsInitialized = true;
        ga('create', config.googleAnalyticsId, 'auto');
        console.log("updateAnalytics: Initialized Google Analytics:", config.googleAnalyticsId)
    }

    console.log("updateAnalytics:", page);
    ga('set', 'page', page);
    ga('send', 'pageview');
});


/*
 * Define ports for Sequence Similarity Plots
 */

app.ports.createSimPlot.subscribe(function(args) {
    console.log("createSimPlot: ", args);
    simplots.heatmap(args[0], args[1]);
    simplots.edgeboundary(args[0], args[1]);
});


/*
 * Define port for scrolling to top of page
 */

app.ports.scrollToTop.subscribe(function() {
    window.scrollTo(0, 0);
});
