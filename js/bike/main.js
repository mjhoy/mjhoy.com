$(function () {
  $('#content').css({minHeight: $('#bike-nav').height()});

  var path = window.location.pathname.replace(/\/$/, '');

  $('#bike-nav li').each(function () {
    var href = $('a', this).attr('href');

    if(href) {
        var cleanHref = href.replace(/\/$/, ''),
            isActive = href == path;
      if(isActive) {
        $(this).addClass('active');
      }
    }
  });
});
