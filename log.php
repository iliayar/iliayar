<?php
	if(!isset($_GET['path'])) {
		exit(0);
	}
	$fp = fopen("/var/log/nginx/ips.log", "a");

	$msg = "[".date("d.m.y H:i:s",time())."] ".$_SERVER['REMOTE_ADDR']." ".$_GET['path'];
	fwrite($fp, $msg);
	fclose($fp);
?>