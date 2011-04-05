maglevInfo = (function() {
  var requestCount = 0, rubyEditor = null;

  $(document).ready(function() {
    debugAjaxCalls();
    setupSelectables();
    setupEditor();
    setupToolBar();
    updateCodeBrowser();  // Not sure this is the best way to kick it off...
  });

  function setupToolBar() {
    $('#toolBar').append(
      $('<div>').button({label: 'Refresh View'}
                       ).click(function () {
                         debugMsg('AbortTxn');
                         getJSON('/transaction/abort', null, function(data) {
                           updateCodeBrowser();
                         });
                       }));
    $('#toolBar').append($('<div>', { id: 'statusBar'}));
  }

  function debugAjaxCalls() {
    $('body').bind(
//      'ajaxStart ajaxStop ajaxSend ajaxSuccess ajaxError ajaxComplete',
      'ajaxError',
      function(event) {
        debugMsg(event.type);
      });

  }

  function setupSelectables() {
    $('#rubyModules').selectable({
      selected: function(event, ui) { selectModule(ui.selected.title); }
    });
    $('#rubyConstants').selectable({
      selected: function(event, ui) { selectConstant(ui.selected.title); }
    });
    $('#rubyModuleMethods').selectable({
      selected: function(event, ui) { selectModuleMethod(ui.selected.title); }
    });
    $('#rubyInstanceMethods').selectable({
      selected: function(event, ui) { selectInstanceMethod(ui.selected.title); }
    });
  }

  function setupEditor() {
    rubyEditor = CodeMirror.fromTextArea('rubyEditor',
                                         { height: "100%",
                                           parserfile: "parseSmalltalk.js",
                                           stylesheet: "CodeMirror/css/Smalltalk.css",
                                           path: "CodeMirror/js/",
                                           lineNumbers: true});
  }

  function selectedModuleName() {
    return $('#rubyModules .ui-selected').attr('title');
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

  function selectModule(className) {
    clearSelections(['#rubyModuleMethods',   '#rubyModuleMethods',
                     '#rubyInstanceMethods', '#rubyAncestors']);
    clearDetailView();
    getJSON('/module/' + className, null, renderCodeBrowser);
  }

  function selectConstant(constName) {
    clearSelections(['#rubyModuleMethods', '#rubyInstanceMethods']);
    getJSON('/module/' + selectedModuleName() + '/constant/' + selectedConstantName(),
            null,
            function(data) {
              clearEditArea();
              renderObject(data['const_value']);
            });
  }

  function selectModuleMethod(methodName) {
    clearSelections(['#rubyConstants', '#rubyInstanceMethods']);
    getJSON('/module/' + selectedModuleName() + '/method',
            { 'method_name': selectedModuleMethodName(),
              'is_instance_method': false },
            renderMethod);
  }

  function selectInstanceMethod(methodName) {
    clearSelections(['#rubyConstants', '#rubyModuleMethods']);
    getJSON('/module/' + selectedModuleName() + '/method',
            { 'method_name': selectedInstanceMethodName(),
              'is_instance_method': true },
            renderMethod);
  }

  function clearSelections(selections) {
    console.log('clearSelections: ' + selections);
    $.each(selections, function(i, el) {
      $(el + ' .ui-selected').removeClass('ui-selected');
    });
    clearEditArea();
  }

  // Makes a JSON request, and decorates it with timing information.
  // It then passes the bundled data object to the callback.
  function getJSON(url, data, callback) {
    var startTime = new Date().getTime();
    var statusBar = $('#statusBar');
    $(statusBar).text('Sent request #' + (++requestCount) + ' for ' + url);
    $.getJSON(url, data, success);
    return;

    function success(data) {
      debugMsg("getJSON success");
      debugMsg(data);

      var serverTime = data['_time'];
      var networkTime = new Date().getTime() - startTime - serverTime;
      startTime = new Date().getTime();
      var error = data['_error'];
      if (error) {
        alert(error + ' (see console log for stack)');
        debugMsg(data['_stack']);
      } else {
        (callback)(data);
      };
      var elapsed = new Date().getTime() - startTime;
      $(statusBar).text(
        'Request for ' + url + ' (roundtrip #' + requestCount + ') took ' +
          serverTime + ' ms on server, ' + networkTime + ' ms on the network, and '
          + elapsed + ' ms on the client.'
      );
    }
  }

  function updateCodeBrowser() {
    getJSON('/modulelist', null, renderCodeBrowser);
  }

  function renderCodeBrowser(data) {
    debugMsg('renderCodeBrowser(): clearing #rubyModules');
    renderList(data['modules'],          $('#rubyModules'));
    renderList(data['constants'],        $('#rubyConstants'));
    renderList(data['module_methods'],   $('#rubyModuleMethods'));
    renderList(data['instance_methods'], $('#rubyInstanceMethods'));
    renderList(data['ancestors'],        $('#rubyAncestors'));
    return;

    function renderList(items, ui) {
      if (items) {
        ui.empty();
        $.each(items, function(n, item) {
          $('<li>', { title: item, 'class': 'ui-widget-content' }).html(item).appendTo(ui);
        });
      }
    }
  }

  function setDetailViewCode(sym) {
    console.log('setDetailViewCode()');
    $('#objectInspector').addClass('hidden');
    $('#rubyEditArea').removeClass('hidden');
  }

  function setDetailViewObject(sym) {
    $('#rubyEditArea').addClass('hidden');
    $('#objectInspector').removeClass('hidden');
  }

  function clearDetailView() {
    $('#rubyEditArea').addClass('hidden');
    $('#objectInspector').addClass('hidden');
  }

  function clearEditArea() {
    if (rubyEditor.editor) {
      rubyEditor.setCode('');
    }
    $('#fileInfo').empty();
  }

  function renderMethod(data) {
    clearEditArea();
    renderSource(data['method_source']);
    var file = data['method_source_file'];

    if (file) {
      $('#fileInfo').html(file + ':' + data['method_line_number']);
    } else {
      $('#fileInfo').html('No file information available');
    }
  }

  // Make the rubyEditor visible in the detail area and render the source
  // code.  High level api.
  function renderSource(string) {
    if (rubyEditor.editor) {
      rubyEditor.setCode(string);
      setDetailViewCode();
    }
  }

  // Make the objectInspector visible in the detail area and render the
  // Object.  High level api.
  function renderObject(objectInfo) {
    console.log('renderObject()');
    console.log(objectInfo);
    setDetailViewObject();

    var objInfo = $('#objInfo tbody').empty();
    objInfo.append('<tr><th>Class</th><td>'     + objectInfo['class'] + '</td></tr>');
    objInfo.append('<tr><th>Object ID</th><td>' + objectInfo['object_id'] + '</td></tr>');
//    objInfo.append('<tr><th>Inspect</th><td>'   + objectInfo['inspect'] + '</td></tr>');

    renderTableData('#objInstVars',
                    objectInfo['instance_variables'],
                    // Data is an array of [name, value, objid]
                    function(idx, data) {
                      return $('<tr><td>'+ data[0] + '</td><td objectId="'
                               + data[2] + '">' + data[1] + '</td></tr>');
                    });

    renderTableData('#objEnumValues',
                    objectInfo['enumerated'],
                    function(idx, data) {
                      return $('<tr><td>' + idx + '</td><td>' + data + '</td></tr>');
                    });
    return;

    function renderTableData(tableId, vals, formatFn) {
      console.log('renderTableData: tableId: '+ tableId + ' # vals: ' + vals.length);
      if (vals && vals.length > 0) {
        $(tableId).removeClass('hidden');
        var ui = $(tableId + ' tbody');
        ui.empty();
        $.each(vals, function(idx, data) { ui.append(formatFn(idx, data)) });
        $(tableId).removeClass('hidden');
      } else {
        $(tableId).addClass('hidden');
      }
    }
  }

  // Some older browsers were complaining that console wasn't defined.
  // (old firefox on solaris), so I'm wrapping console logging in this
  // function.
  function debugMsg(obj) {
    if (console) {
      console.log(obj);
    }
  }
})();