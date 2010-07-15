var map;
var marker;
var infoWindow;

// Fits the canvas to the page
function fitMapCanvas() {
  $("#map_canvas").height($(window).height() - $("#map_menu").height() - $("#footer").height() - 50);
}

function initialize() {
  // Auto-select results count on focus
  $("#num_results").focus(function() {
    this.select();
  });
  
  // Resize the map canvas on window resize
  $(window).resize(fitMapCanvas);
  fitMapCanvas();


  // let's set the initial location of the map.
  var latLng = new google.maps.LatLng( 41.6326327769545, -100.1024599609375 );

  // create a Map instance.
  map = new google.maps.Map( document.getElementById( 'map_canvas' ), {
    zoom: 5,
    center: latLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  });

  // register a click event handler with our Map instance.
  google.maps.event.addListener( map, 'click', function(event) {
     searchZips(event.latLng.lat(), event.latLng.lng());           
     return false;
   });

}

function searchZips(lat, lng) {
  // remove info window from the map if it exists.  
  if ( infoWindow != null ) {
    infoWindow.close(); 
  }
     
  // remove the marker from the map if it exists.
  if ( marker != null ) {
    marker.setMap(null);
  }

  // create a new marker and place it on the map 
  // of the clicked location.
  placeMarker( new google.maps.LatLng( lat, lng ) ); 
  
  // show our current location
  $("#location_display").text("Location: " + lat + ", " + lng);
  
  // get the values for the parameter hash.
  var markerLocation = marker.getPosition();
  var latitudeS = markerLocation.lat();
  var longitudeS = markerLocation.lng();
   
  // determine how many results the user wanted (modifying the text box if there's invalid input)
  var k_results = $.trim($("#num_results").val());
  var isNumber = /^\d+$/.test(k_results);
  var numValid = k_results > 1;
  if( !isNumber || !numValid ) {
    k_results = 5; // Default to this many results if input not valid
  }
       
  $("#num_results").val(k_results);
     
  // create a parameter hash.
  var params = { lat: latitudeS, lon: longitudeS, k: k_results };
        
  // post an Ajax request to the server.
  postAjaxRequest( params );
}


function postAjaxRequest( params ) {
  host = location.hostname;
  port = location.port;
  serviceUrl = "http://" + host + ":" + port + "/nearest";

  // initiate an Ajax request to the server and store the response in 'result'.
  $.ajax({
    url: serviceUrl,
    type: 'POST',
    dataType: 'json',
    data: params,

    success: function( locations ) {
                            
      // create a new info window, open it, and extend 
      // it from the current marker on the map.
      attachInfoWindow( marker, 0, locations );

    },

    error: function( xhr, txtStatus ) {
      alert( "Something went wrong during API request to '/nearest'!  "
        + "This may be a cross-site request failure due to your browser's security policy.  "
        + "Check that the address " + host + " agrees with what is in your browser's URL bar.  "
        + "\n========\n"
        + "Details:\n" 
        + "XMLHttpRequest status: " + xhr.status + "\n"
        + "Status: " + txtStatus );
    }

  });

}

function placeMarker(location) {

  var clickedLocation = new google.maps.LatLng(location);
  marker = new google.maps.Marker({
      position: location, 
      map: map
    });

  map.setCenter(location);
  
}

function attachInfoWindow( marker, number, locations ) {

  var markerLocation = marker.getPosition();
  var latitude = markerLocation.lat();
  var longitude = markerLocation.lng();
  
  var html  = '<div id="info_window">';
      html += '  <table class="results"> <thead> <th>ZIP</th> <th>City</th>  <th>State</th>  <th>Distance</th> </thead>';
      html += '  <tbody>';
  
  for(var idx in locations) {  
    loc = locations[idx];
    
    // First row <td>s are 'first', after that it's 'other'; used to highlight first result
    tdTag = '<td class="' + ( (idx == 0) ? 'first' : 'other' ) + '">'; 
    
    // Generate a zip code link they can click
    if(loc['miles'] > 0.000001)
      zipCodeHtml = '<a href="#'+loc['zipcode']+'" onclick="searchZips('+loc['latitude']+', '+loc['longitude']+')">' + htmlEncode(loc['zipcode']) + '</a>';     
    else // Don't link to zip codes we're right on top of 
      zipCodeHtml = htmlEncode(loc['zipcode']);    

    html += "<tr>";
    html += tdTag + zipCodeHtml                                   + "</td>";
    html += tdTag + htmlEncode(loc['city'])                       + "</td>";
    html += tdTag + htmlEncode(loc['state'])                      + "</td>";
    html += tdTag + htmlEncode(loc['miles'].toFixed(1) + " mi")   + "</td>";
    html += "</tr>"
  }
  
  html += "</tbody> </table>";

  infoWindow = new google.maps.InfoWindow( { content: html, zIndex: number } );

  infoWindow.open( map, marker );
}

function htmlEncode(value){ 
  // http://stackoverflow.com/questions/1219860/javascript-jquery-html-encoding
  return $('<div/>').text(value+"").html(); 
} 

$(document).ready(function() {
  initialize();
});


