'use strict';

require("bootstrap-loader");
import * as agave from '../node_modules/agave-file-browser/AgaveFileBrowser.js';
import * as simplots from '../node_modules/sim-plots/src/sim-plots.js';
import 'jstree';

// Require these files so they get copied to dist
require('../index.html');
require("../img/nav-header.png");
require('../css/imicrobe.css');
//require('../css/readable.min.css');
require('../css/spinner.css');
require('../node_modules/sim-plots/src/main.css');
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
 * Define ports for Agave & Syndicate File Browsers
 */

var fileBrowser = [];
app.ports.createFileBrowser.subscribe(function(params) { // TODO get username/token from localStorage.session instead of passing in
    var dialog = $('#' + params.source + '-file-browser-dialog');
    var fb = fileBrowser[params.source];

    if (typeof fb == 'undefined') {
        if (params.source == 'agave') {
            fb = fileBrowser[params.source] = new agave.AgaveFileBrowser({
                elementId:   'agave-file-browser',
                baseUrl:     config.agaveFilesUrl,
                queryParams: 'limit=9999',
                path:        params.username,
                authToken:   params.token,
                busyIconUrl: 'img/spinner.gif'
            });
        }
        else if (params.source == 'syndicate') {
            fb = fileBrowser[params.source] = new agave.AgaveFileBrowser({
                elementId:   'syndicate-file-browser',
                baseUrl:     config.syndicateRestUrl,
                queryParams: 'listdir',
                path:        '',
                authToken:   '',
                busyIconUrl: 'img/spinner.gif',
                formatCallback: function(path, response) {
                    return response.entries
                        .filter((item, index) => index >= 2) // remove first two items which are the current path and the top-level path
                        .map((item) => {
                            var type = (item.type == 2 ? 'dir' : 'file');
                            path = (path ? path.replace(/\/?$/, '/') : ''); // add trailing slash to path if defined
                            return {
                                id: path + item.name,
                                text: item.name,
                                data: { type: type },
                                icon: (type == 'dir' ? 'jstree-folder' : 'jstree-file')
                            };
                        });
                    }
            });
        }
        else {
            console.error('Unknown "source" parameter');
            return;
        }
    }

    // Needs to be redefined for each call as input changes
    dialog.find('button.btn-primary').unbind().click(function() {
        var files = fb.getSelectedNodes();
        console.log('selected:', files);

        dialog.modal('hide');

        app.ports.onFileSelect.send({
            id: params.id,
            username: params.username,
            token: params.token,
            path: files.map(f => f.id).join(';'),
            source: params.source
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
    var elementId = args[0],
        matrix = args[1];
    simplots.heatmap(elementId, matrix);
    simplots.edgeboundary(elementId, matrix);
    simplots.pcoaPlot(elementId, matrix);
});


/*
 * Define port for scrolling to top of page
 */

app.ports.scrollToTop.subscribe(function() {
    window.scrollTo(0, 0);
});
