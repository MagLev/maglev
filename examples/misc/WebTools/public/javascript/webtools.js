MagLev = (function() {
  var requestCount = 0;

  $(document).ready(function() {
    console.log('document ready');
    setUpTabs();
  });

  function setUpTabs() {
    console.log('setUpTabs');
    $("#tabs").tabs();
    $("#tabs").bind('tabsselect', function(event, ui) {
      console.log("Tab Select: " + ui.panel.id);
      if (ui.panel.id == 'rubyCodeBrowser') {
        console.log('rubyCodeBrowser selected');
        getJSON('/codebrowser', null, gotData);
      }
    });
  }

  function gotData(data) {
    console.log('gotData');
    //    console.log(data);
    renderData();
    return;

    function renderData() {
      console.log('codeBrowser.renderData()');
      renderClassModuleList();
    }

    function renderClassModuleList() {
      console.log('codeBrowser.renderClassModuleList() 11');
      //      console.log(data['classNames']);
      console.log(data);
      console.log(data["classNames"]);
      /*
        var items = [];
        $.each(data['classNames'], function(){
        items.push('<div ');
        items.push(' title="' + this + '"');
        items.push(' class="clickable');
        if (this == data['class']) items.push(' selected');
        items.push('">' + this + '</div>');
        });
        console.log(items);
      */
      /*
        $('#rubyClasses').empty().append(items.join(''));
        $('#rubyClasses .clickable').click(function(event){
        if (maglevInfo.setSelectedClass(this['title']))
        rubyShow();
        });
      */
    }
  }

  function getJSON(url, data, callback) {
    var startTime = new Date().getTime();
    var statusBar = $('#statusBar');
    $(statusBar).text('Sent request #' + (++requestCount) + ' for ' + url);
    $.getJSON(url, data, success);
    return;

    function success(data) {
      console.log("getJSON success");
      (callback)(data);
    };
    
    function successX(data) {
      var serverTime = data['_time'];
      var networkTime = new Date().getTime() - startTime - serverTime;
      startTime = new Date().getTime();
      var error = data['_error'];
      if (error) {
        alert(error + ' (see console log for stack)');
        console.log(data['_stack']);
      } else {
        (callback)(data);
      };
      var elapsed = new Date().getTime() - startTime;
      $(statusBar).text(
        'Request for ' + url + ' (roundtrip #' + requestCount + ') took ' +
          serverTime + ' ms on server, ' + networkTime + ' ms on the network, and '
          + elapsed + ' ms on the client.'
      );
    };

  };
})();