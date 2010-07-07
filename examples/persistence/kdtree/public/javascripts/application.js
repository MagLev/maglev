var map;
var marker;
var infoWindow;

function initialize() {

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
     placeMarker( event.latLng );
     
     // get the values for the parameter hash.
     var markerLocation = marker.getPosition();
     var latitudeS = markerLocation.lat();
     var longitudeS = markerLocation.lng();
     
     // create a parameter hash.
     var params = { lat: latitudeS, lon: longitudeS, k: 1 };
        
     // post an Ajax request to the server.
     postAjaxRequest( params );
      
     return false;

   });

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
     
      // evaluate the response from the server
      var location = locations[ 0 ];
                        
      // create a new info window, open it, and extend 
      // it from the current marker on the map.
      attachInfoWindow( marker, 0, location );

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

function attachInfoWindow( marker, number, location ) {

  var markerLocation = marker.getPosition();
  var latitude = markerLocation.lat();
  var longitude = markerLocation.lng();
  var zipcode = location['zipcode'];

  var contentString  = '<div id="info_window">';
      contentString += '<ul>';
      contentString += "<li>Latitude:&nbsp;"          + latitude  + "</li>";
      contentString += "<li>Longitude:&nbsp;"         + longitude + "</li>";
      contentString += "<li>Closet US Zipcode:&nbsp;" + zipcode   + "</li>";
      contentString += '</ul';
      contentString += '</div>';

  infoWindow = new google.maps.InfoWindow( { content: contentString, zIndex: number } );

  infoWindow.open( map, marker );
}

$(document).ready(function() {
  initialize();
});


