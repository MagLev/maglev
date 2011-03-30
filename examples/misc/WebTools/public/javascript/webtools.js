maglevInfo = (function() {
  var requestCount       = 0;

  $(document).ready(function() {
    console.log('document ready');
    setupTabs();
    setupSelectables();
    // Since we only have one tab, we load it's JSON by hand
    updateCodeBrowser();

  });

  function setupTabs() {
    console.log('setupTabs()');
    var tabs = $("#tabs");
    tabs.tabs();
    tabs.bind('tabsselect', function(event, ui) {
      console.log("Tabs Select: " + ui.panel.id);
      if (ui.panel.id == 'rubyCodeBrowser') {
        console.log('rubyCodeBrowser selected');
        updateCodeBrowser();
      }
    });
    tabs.bind('tabsload', function(event, ui) {
      console.log("Tabs Load: " + ui.panel.id);
    });
    tabs.bind('tabsshow', function(event, ui) {
      console.log("Tabs Show : " + ui.panel.id);
    });

  }

  function setupSelectables() {
    // May have to re-run this if we reload module list?
    console.log('setupSelectables()');

    $('#rubyModuleList').selectable({
      selected: function(event, ui) {
        console.log(ui.selected.title);
        setSelectedClass(ui.selected.title);
      }
    });
    $('#rubyConstants').selectable({
      selected: function(event, ui) {
        console.log(ui.selected.title);
        setSelectedConstant(ui.selected.title);
      }
    });
    $('#rubyModuleMethods').selectable({
      selected: function(event, ui) {
        console.log(ui.selected.title);
        setSelectedModuleMethod(ui.selected.title);
      }
    });
    $('#rubyInstanceMethods').selectable({
      selected: function(event, ui) {
        console.log(ui.selected.title);
        setSelectedInstanceMethod(ui.selected.title);
      }
    });
  };

  function setSelectedConstant(constName) {
    console.log('selected constant ' + constName);
  };

  function setSelectedModuleMethod(methodName) {
    console.log('selected module method ' + methodName);
  };

  function setSelectedInstanceMethod(methodName) {
    console.log('selected instance method ' + methodName);
  };

  // Makes a JSON request, and decorates it with timing information.
  // It then passes the bundled data object to the callback.
  function getJSON(url, data, callback) {
    var startTime = new Date().getTime();
    var statusBar = $('#statusBar');
    $(statusBar).text('Sent request #' + (++requestCount) + ' for ' + url);

    $.getJSON(url, data, success);
    return;

    function success(data) {
      console.log("getJSON success");
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

  // Retrieve fresh data for the codebrowser and render it
  function updateCodeBrowser() {
    console.log('updateCodeBrowser()');
    getJSON('/codebrowser/modulelist', null, renderCodeBrowser);
  };

  function setSelectedClass(className) {
    getJSON('/codebrowser/module/' + className, null, renderCodeBrowser);
  };

  function renderCodeBrowser(data) {
    console.log('renderCodeBrowser()');
    renderClassModuleList();
    renderSelection();
    return;

    function renderSelection() {
      console.log('renderSelection()');
      console.log(data);

      renderConstants();
//      renderModuleMethods();
//      renderInstanceMethods();
    };

    function renderConstants() {
      console.log('renderConstants()');
      constants = data['constants'];
      if (constants) {
        var constantList = $('#rubyConstants').empty();
        $.each(constants, function(n, constant) {
          var li = $('<li>', { title: constant, class: 'ui-widget-content'});
          li.html(constant);
          li.appendTo(constantList);
        });
      }
    }

    function renderModuleMethods() {
      console.log('renderModuleMethods()');
      methods = data['moduleMethods'];
      if (constants) {
        var list = $('#rubyModuleMethods').empty();
        $.each(methods, function(n, method) {
          var li = $('<li>', { title: method, class: 'ui-widget-content'});
          li.html(method);
          li.appendTo(list);
        });
      }
    }

    function renderInstanceMethods() {
      renderList(data['moduleMethods'], $('#rubyInstanceMethods'));
    };
    
    function renderList(items, ui) {
      console.log('renderList()');
      if (items) {
        ui.empty();
        $.each(items, function(n, item) {
          var li = $('<li>', { title: item, class: 'ui-widget-content'});
          li.html(item);
          li.appendTo(ui);
        });
      }
    };

    function renderClassModuleList() {
      var modules = data['classNames'],
      selected = data['selected_class'];
      console.log('renderClassModuleList()');
      if (modules) {
        var moduleList = $('#rubyModuleList').empty();
        $.each(modules, function(n, mod){
          var li = $('<li>', { title: mod,
                               class: 'ui-widget-content'});
          li.html(mod);
          li.appendTo(moduleList);
        });
      };
    };
  }
})();