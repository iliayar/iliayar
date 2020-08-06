jQuery(function(){
    jQuery("#bgndVideo").YTPlayer();
});

$(document).ready(function() {
    $('.playpause').click(function() {
        state = jQuery("#bgndVideo").YTPGetPlayer().getPlayerState();
        if(state != 1) {
            jQuery("#bgndVideo").YTPPlay();
        } else {
            jQuery("#bgndVideo").YTPPause();
        }
    })
})