jQuery(function(){
    jQuery("#bgndVideo").YTPlayer();
});

var un_mute = jQuery("#un-mute")[0];

un_mute.onclick = function() {
    player = jQuery("#bgndVideo");
    mute = jQuery("#un-mute")[0];
    if(player.YTPGetVolume() == 0) {
	mute.innerHTML = "mute";
    } else {
	mute.innerHTML = "unmute";
    }
    player.YTPToggleVolume();
};
