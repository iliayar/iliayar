(function() {

    var ws = new WebSocket(`ws://${window.location.host}/ws/speedtest`);

    function displayPing(data) {
	ping = document.querySelector('#ping');
	ping.innerHTML = data.ping.latency;
    }

    function displayDownload(data) {
	download = document.querySelector('#download');

	content = data.download.bandwidth*8/1000000 + ' Mbps ' + Math.floor(data.download.progress*100) + '%'
	
	download.innerHTML = content;
    }

    function displayUpload(data) {
	upload = document.querySelector('#upload');

	content = data.upload.bandwidth*8/1000000 + ' Mbps ' + Math.floor(data.upload.progress*100) + '%'
	
	upload.innerHTML = content;
    }

    function displayResult(data) {
	img = document.querySelector('#image-result');

	img.removeAttribute('hidden');
	img.src = `https://www.speedtest.net/result/c/${data.result.id}.png`
    }
    
    ws.onmessage = function(e) {
	var data = JSON.parse(e.data);
	try {
	    ({
		ping:     displayPing,
		download: displayDownload,
		upload:   displayUpload,
		result:   displayResult
	    })[data.type](data);
	} catch(e) {
	    console.log(e)
	}
    }

})()



