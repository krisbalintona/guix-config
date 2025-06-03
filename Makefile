CHANNELS=./channels.scm
LOCKFILE=./channels-lock.scm

pull:
	guix pull -C ${CHANNELS}

lock:
	guix describe -f channels > ${LOCKFILE}

wsl:
	sudo guix system reconfigure -L . conf/wsl.scm

home:
	guix home reconfigure -L . conf/home-configuration.scm
