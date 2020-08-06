jQuery(function(){
    jQuery("#bgndVideo").YTPlayer();
});

var un_mute = jQuery("#un-mute")[0];

un_mute.onclick = function() {
    jQuery("#bgndVideo").YTPToggleVolume();
};
