(function ($) {

  // dom ready
  $(function () {
    $('a[data-confirm]').click(function () {
      var query = $(this).data('confirm');
      if (!confirm(query)) {
        return false;
      }
    });
  });

})(jQuery);
