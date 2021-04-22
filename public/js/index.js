(() => {
    let un_mute = document.querySelector('#un-mute');
    let video = document.querySelector("#bg-video");
    video.volume = 0.3;

    un_mute.addEventListener('click', () => {
	if(video.muted) {
	    un_mute.innerHTML = 'mute';
	    video.muted = false
	} else {
	    un_mute.innerHTML = 'unmute';
	    video.muted = true
	}
    });

    var cursor = document.querySelector('#cursor')

    function cursorHide() {
	cursor.style.display = 'none';
	setTimeout(cursorShow, 1000);
    }

    function cursorShow() {
	cursor.style.display = 'inline';
	setTimeout(cursorHide, 1000);
    }

    cursorHide();

})();
