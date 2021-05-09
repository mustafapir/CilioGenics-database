$("#searchui").keypress(function(event) {
    var keycode = (event.keyCode ? event.keyCode : event.which);
    if (keycode == '13') {
        $("#geneName2_search").click();
    }
});