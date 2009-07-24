// forum.js

function newmessage () {
    var form = $('form');
    if (form.css("display") == "none") {
        form.show();

        $('#wysiwyg').wysiwyg(
            {
                controls : {
                    separator04 : { visible : true },

                    insertOrderedList : { visible : true },
                    insertUnorderedList : { visible : true }
                }
            }
        );

        location.hash = "#editor";
    }
}
