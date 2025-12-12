CHANNELS=./channels
LOCKFILE=./channels-lock.scm

pull:
	guix pull -C ${CHANNELS}

lock:
	guix describe -f channels > ${LOCKFILE}

upgrade: pull lock

wsl:
	sudo guix system reconfigure --no-bootloader -L . conf/wsl.scm

home:
	guix home reconfigure -L . conf/home-configuration.scm

build:
	guix build -L . $(PACKAGE)

shell:
	guix shell -L . $(PACKAGE)
