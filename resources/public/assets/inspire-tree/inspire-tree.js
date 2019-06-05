var tree = new InspireTree({
    data: function(node, resolve, reject) {
        var id = node ? node.id : 'http://www.w3.org/2002/07/owl%23Thing';
        if (rt == null) {
          rt = 'all';
        }
        return $.getJSON(rt + '/children?iri=' + id);
    }
});

new InspireTreeDOM(tree, {
    target: '.tree'
});

tree.on('node.click', function(event, node) {
  var id = node ? node.id : 'http://www.w3.org/2002/07/owl%23Thing';
    if (rt == null) {
      rt = 'all';
    }
    $.ajax({ url: rt + '/info?iri=' + id, 
             dataType: 'html', 
             success: function(response) {
               $('.info').html(response);
             }});
});

$('.container').css({'overflow': 'hidden', 'height': '100%'});
$('.col-md-5').css({'overflow': 'auto', 'height': '99%'});