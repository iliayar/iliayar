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

    let active_cursor = '<div class="cmd" id="cursor">&#x2588;</div>'
    let active_line = '<div id="active-line"><div class="tilde cmd">~</div> <div class="lambda cmd">λ</div> <div class="cmd" id="active-cmd"></div></div>'
    let regular_line = text => `<br><div class="tilde cmd">~</div> <div class="lambda cmd">λ</div> <div class = "arbitrary-cmd">${text}</div></div>`

    document.addEventListener('keypress', e => {
	if(e.key == 'Enter') {
	    text = document.querySelector('#active-cmd').innerHTML
	    document.querySelector('#cursor').outerHTML = ''
	    document.querySelector('#active-line').outerHTML = ''
	    document.querySelector('.content .center-block').innerHTML += regular_line(text)
	    document.querySelector('.content .center-block').innerHTML += active_line
	    document.querySelector('#active-line').innerHTML += active_cursor
	    cursor = document.querySelector('#cursor')
	} else  {
	    document.querySelector('#active-cmd').append(e.key)
	}
    });

    document.addEventListener('keydown', e => {
	if(e.keyCode == 8) {
	    text = document.querySelector('#active-cmd').innerHTML
	    document.querySelector('#active-cmd').innerHTML = text.substr(0, text.length - 1)
	}
    })
    
})();
