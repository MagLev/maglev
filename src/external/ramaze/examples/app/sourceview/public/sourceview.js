var curfile = null;
var processing = false;

function setup() {
  $('ul.filetree').treeview({
    persist: 'location',
    animated: 'fast',
    unique: true
  });

  $("span.file").click(function(){
    processing = true;
    file = $('a:eq(0)', this).attr('href').substr(1);
    if (curfile != file) {
      curfile = file;
      $('a.selected').removeClass('selected');
      $('#file_contents').load('/source'+curfile, {}, function(){ processing = false; });
      $('a',this).eq(0).addClass('selected');
    } else {
      processing = false;
    }

    if (curfile) {
      $('#repourl').empty().append(
        $('<a/>').attr('href', 'http://github.com/manveru/ramaze/tree/master'+curfile)
                 .text('download '+curfile.substr(1))
      );
      urchinTracker(curfile);
    }
  });

  $('a.selected').parent('span.file').click();

  setInterval(function(){
    if (processing) return;

    curhash = document.location.hash.substr(1);
    if(curfile != curhash) {
      $("a[href='#"+curhash+"']").parents('ul, li').show().end()
                                 .parent('span.file').click();
    }
  }, 100);
}

$(function(){
  if (document.location.hash != '') {
    curfile = document.location.hash.substr(1);
    $('#file_contents').load('/source'+curfile, {}, setup);
  } else {
    setup();
  }
});
