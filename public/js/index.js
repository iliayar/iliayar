function cursorHide() {
    jQuery("#cursor").hide();
    setTimeout(cursorShow, 1000);
}

function cursorShow() {
    jQuery("#cursor").show();
    setTimeout(cursorHide, 1000);
}

jQuery(function(){
    jQuery("#bgndVideo").YTPlayer();
    setTimeout(cursorHide, 1000);
});


var un_mute = jQuery("#un-mute")[0];

un_mute.onclick = function() {
    player = jQuery("#bgndVideo");
    mute = jQuery("#un-mute");
    if(player.YTPGetVolume() == 0) {
	mute.html("mute");
    } else {
	mute.html("unmute");
    }
    player.YTPToggleVolume();
};
