CHANNELS=./channels.scm
LOCKFILE=./channels-lock.scm

pull:
	guix pull -C ${CHANNELS}

lock:
	guix describe -f channels.scm > ${LOCKFILE}

upgrade: pull lock

wsl:
	sudo guix system reconfigure --no-bootloader -L src config/wsl.scm

mute-home:
	guix home reconfigure -L src config/mute-home.scm

build:
	guix build -L src $(PACKAGE)

shell:
	guix shell -L src $(PACKAGE)
