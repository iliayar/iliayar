
import subprocess

def speedtest():
    p = subprocess.Popen(['/app/speedtest',
                          '--accept-license',
                          '-f', 'json',
                          '-p', 'yes'], stdout=subprocess.PIPE)
    while True:
        msg = p.stdout.readline()
        if msg == b'' and p.poll() is not None:
            break
        else:
            yield msg.decode()
    
    
