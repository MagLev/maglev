maglevInfo = (function() {
  var requestCount = 0, rubyEditor = null;

  $(document).ready(function() {
    console.log('document ready');
    setupTabs();
    setupSelectables();
    setupEditor();
    updateCodeBrowser();  // Not sure this is the best way to kick it off...
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
  }

  function setupSelectables() {
    // May have to re-run this if we reload module list?
    console.log('setupSelectables()');

    $('#rubyModuleList').selectable({
      selected: function(event, ui) { setSelectedClass(ui.selected.title); }
    });
    $('#rubyConstants').selectable({
      selected: function(event, ui) { setSelectedConstant(ui.selected.title); }
    });
    $('#rubyModuleMethods').selectable({
      selected: function(event, ui) { setSelectedModuleMethod(ui.selected.title); }
    });
    $('#rubyInstanceMethods').selectable({
      selected: function(event, ui) { setSelectedInstanceMethod(ui.selected.title); }
    });
  };

  function setupEditor() {
    rubyEditor = CodeMirror.fromTextArea('rubyEditor',
                                         { height: "100%",
                                           parserfile: "parseSmalltalk.js",
                                           stylesheet: "CodeMirror/css/Smalltalk.css",
                                           path: "CodeMirror/js/",
                                           lineNumbers: true});
  };

  function selectedModuleName() {
    return $('#rubyModuleList .ui-selected').attr('title');
  }

  function selectedConstantName() {
    return $('#rubyConstants .ui-selected').attr('title');
  }

  function selectedModuleMethodName() {
    return $('#rubyModuleMethods .ui-selected').attr('title');
  }

  function selectedInstanceMethodName() {
    return $('#rubyInstanceMethods .ui-selected').attr('title');
  }

  function setSelectedConstant(constName) {
    $('#rubyModuleMethods .ui-selected').removeClass('ui-selected');
    $('#rubyInstanceMethods .ui-selected').removeClass('ui-selected');

    getJSON('/constant',
            { 'moduleName': selectedModuleName(),
              'constName':  selectedConstantName() },
            function(data) { renderSource(data['const_value']); });
  };

  function setSelectedModuleMethod(methodName) {
    console.log('selected module method ' + methodName);
    $('#rubyConstants .ui-selected').removeClass('ui-selected');
    $('#rubyInstanceMethods .ui-selected').removeClass('ui-selected');

    getJSON('/method',
            { moduleName: selectedModuleName(),
              methName:   selectedModuleMethodName(),
              isInstanceMethod: false },
            function(data) { renderSource(data['method_source']); });
  };

  function setSelectedInstanceMethod(methodName) {
    console.log('selected instance method ' + methodName);
    $('#rubyConstants .ui-selected').removeClass('ui-selected');
    $('#rubyModuleMethods .ui-selected').removeClass('ui-selected');

    getJSON('/method',
            { moduleName: selectedModuleName(),
              methName:   selectedInstanceMethodName(),
              isInstanceMethod: true },
            function(data) { renderSource(data['method_source']); });
  };

  // Makes a JSON request, and decorates it with timing information.
  // It then passes the bundled data object to the callback.
  function getJSON(url, data, callback) {
    var startTime = new Date().getTime();
    var statusBar = $('#statusBar');
    $(statusBar).text('Sent request #' + (++requestCount) + ' for ' + url);

    console.log('Calling $.getJSON with data: ');
    console.log(data);
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
    getJSON('/modulelist', null, renderCodeBrowser);
  };

  function setSelectedClass(className) {
    getJSON('/module/' + className, null, renderCodeBrowser);
  };

  function renderCodeBrowser(data) {
    console.log('renderCodeBrowser()');
    renderClassModuleList();
    renderSelection();
    return;

    function renderSelection() {
      console.log('renderSelection()');
      console.log(data);

      renderList(data['constants'],        $('#rubyConstants'));
      renderList(data['module_methods'],   $('#rubyModuleMethods'));
      renderList(data['instance_methods'], $('#rubyInstanceMethods'));
    };

    function renderList(items, ui) {
      console.log('renderList('+ui+')');
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
      selected = data['selected_module'];
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

  function renderSource(string) {
    console.log('renderSource: ' + string);
    if (rubyEditor.editor) {
      rubyEditor.setCode(string);
    }
  };
})();