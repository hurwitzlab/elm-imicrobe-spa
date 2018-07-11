'use strict';

require("bootstrap-loader");
import * as agave from '../node_modules/agave-file-browser/AgaveFileBrowser.js';
import * as simplots from '../node_modules/sim-plots/src/sim-plots.js';
import 'jstree';

import "blueimp-file-upload/js/vendor/jquery.ui.widget.js";
import "blueimp-file-upload/js/jquery.iframe-transport.js";
import "blueimp-file-upload/js/jquery.fileupload.js";
import "blueimp-file-upload/js/jquery.fileupload-image.js";

// Require these files so they get copied to dist // FIXME move to webpack config
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
        zoom: 8,
        center: myLatlng
    };

    var gmap = new Google.maps.Map(mapDiv, mapOptions);

    var marker = new google.maps.Marker({
        position: myLatlng,
        map: gmap
        //title: ''
    });

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
    localStorage[config.cookieName] = session;
});

// This event is only triggered when localStorage is modifed from another window
window.addEventListener("storage",
    function(event) {
        if (event.storageArea === localStorage && event.key === config.cookieName) {
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

    dialog.find('button#cyverse-ds-refresh').unbind().click(function() {
        var button = this;
        console.log('refresh');
        fb.updateCallback = function() { $(button).attr('disabled', false); };
        $(button).attr('disabled', true);
        $('#agave-file-browser').html('<div class="center"><div class="padded-xl spinner"></div></div>');
        fb.treeInit = 0;
        fb.update('');

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
        datasets = args[1];

    if (!elementId || !datasets) {
        console.error("createSimPlot: missing required args");
        return;
    }

    var element = $("#"+elementId);

    datasets.forEach(d => {
        var dataType = d[0];
        var filepath = d[1];
        var data = d[2];

        dataType = dataType.toLowerCase();
        console.log("Plot type:", dataType, filepath);

        if (dataType == "matrix") { // similarity matrix
            simplots.symmetricalHeatmap(elementId, data);
            simplots.edgeboundary(elementId, data);
            simplots.pcoaPlot(elementId, data, { padding: 0, width: 600, height: 400 } );
        }
        else if (dataType == "centrifuge") { // centrifuge format
            simplots.bubblePlot(elementId, data);
        }
        else if (dataType == "blast-tab") { // blast tabular format
            var basename = filepath.split('/').reverse()[0];
            element.append("<div>" + basename + ": Frequency of HSPs by sample and depth (m)</div>")
            simplots.heatmap(elementId, data);
        }

        element.append("<hr style='border:0;clear:both'>")
    });
});


/*
 * Define port for scrolling to top of page
 */

app.ports.scrollToTop.subscribe(function() {
    window.scrollTo(0, 0);
});


/*
 * Define port for file-upload integration
 */

app.ports.fileUploadOpenBrowser.subscribe(function(args) {
    console.log("fileUploadOpenBrowser: ", args);
    var token = args[0],
        destPath = args[1];

    var input = $('input[type=file]')[0];
    $(input).fileupload({
        dataType: 'json',
        type: "GET",
        url: "https://agave.iplantc.org/files/v2/media/" + destPath,
        headers: {
            Authorization: "Bearer " + token
        },
        cache: false,
        add: function(e, data) {
            console.log("fileUploadOpenBrowser add:", data);
            var file = data.files[0];
            var obj = {
                name: file.name,
                type: file.type,
                size: file.size
            };
            app.ports.fileUploadFileSelected.send(JSON.stringify(obj));
            data.submit();
        },
        error: function(jqXHR, textStatus, errorThrown) {
            console.log("fileUploadOpenBrowser error:", txtStatus);
            app.ports.fileUploadDone.send(""); //FIXME send proper error indicator
        },
        done: function(e, data) {
            console.log("fileUploadOpenBrowser done:", data.result.status);
            if (data && data.result && data.result.status == "success") {
                // Quick fix to account for Agave upload delay.  The fileUploadDone signal triggers a refresh of the
                // file listing.  Someday need to use a proper Agave websocket notification instead.
                setTimeout(
                    function() {
                        app.ports.fileUploadDone.send(JSON.stringify(data.result))
                    },
                    5000
                );
            }
            else {
                app.ports.fileUploadDone.send(""); //FIXME send proper error indicator
            }
        }
    })
    .click(); // open file browser
});
