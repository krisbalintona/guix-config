GUIX = time guix
GUIX_LOCKED = $(GUIX) time-machine --channels=$(LOCKFILE) --
ENV_DIR = ./env
CHANNELS = $(ENV_DIR)/channels.scm

# * Machines

MACHINES := mute sublation wsl
# Host name, stripping domain
HOSTNAME := $(firstword $(subst ., ,$(shell hostname)))
# Extract machine from CLI, otherwise fall back to hostname
MACHINE := $(or \
	$(filter $(MACHINES),$(MAKECMDGOALS)), \
	$(HOSTNAME))

# Guard against passing multiple machines
ifneq ($(filter-out 0 1,$(words $(filter $(MACHINES),$(MAKECMDGOALS)))),)
$(error Please specify at most one machine: $(MACHINES))
endif

# Prevent machines from being treated as targets
$(MACHINES):
	@:

# * Targets

.DEFAULT_GOAL := doctor

# ** Pull

.PHONY: pull
pull:
	$(GUIX) pull -C $(CHANNELS)

.PHONY: upgrade
upgrade: pull lock

# ** Channel lock files

LOCKFILE = $(ENV_DIR)/channels-lock-$(MACHINE).scm

# Create a lock target but also make each lock file be their own
# target (so that other targets can depend on them)
.PHONY: lock
lock: $(LOCKFILE)
$(LOCKFILE): $(CHANNELS)
	$(GUIX) describe --format=channels $(CHANNELS) > $@
	@echo "Created $@"

# ** System

system: SYSTEM_ACTION = reconfigure
system-build: SYSTEM_ACTION = build
system system-lock: SYSTEM_EXTRA_FLAGS =

# WSL should not be configured with a bootloader, since Windows does
# its own things for booting.  (Forgetting this flag will break the
# Guix WSL install!)
ifeq ($(MACHINE),wsl)
system system-lock: SYSTEM_EXTRA_FLAGS += --no-bootloader
endif

.PHONY: system system-build
system system-build:
	$(GUIX) system $(SYSTEM_ACTION) \
		-L src \
		$(SYSTEM_EXTRA_FLAGS) \
		config/$(MACHINE).scm

.PHONY: system-lock
system-lock: $(LOCKFILE)
	$(GUIX_LOCKED) system reconfigure \
		-L src \
		$(SYSTEM_EXTRA_FLAGS) \
		config/$(MACHINE).scm

# ** Home

home: HOME_ACTION = reconfigure
home-build: HOME_ACTION = build

.PHONY: home home-build
home home-build:
ifeq ($(MACHINE),wsl)
	$(error The wsl has no home config.)
endif
	$(GUIX) home ${HOME_ACTION} \
		-L src \
		config/$(MACHINE)-home.scm

.PHONY: home-lock
home-lock:
ifeq ($(MACHINE),wsl)
	$(error The wsl has no home config.)
endif
	$(GUIX_LOCKED) home reconfigure \
		-L src \
		config/$(MACHINE)-home.scm

# ** Development

# For the build and shell targets only:
ifneq ($(filter build shell,$(MAKECMDGOALS)),)
# Filter out the build and shell targets as well as any of MACHINES,
# too
PACKAGES := $(filter-out build shell $(MACHINES),$(MAKECMDGOALS))

# Guard against zero PACKAGES
ifeq ($(strip $(PACKAGES)),)
$(error Please specify at least one package, e.g. `make shell emacs ripgrep`)
endif

# Prevent packages names from being treated as targets
$(PACKAGES):
	@:
endif

.PHONY: build
build:
	$(GUIX) build -L src --keep-failed --verbosity=3 $(PACKAGES)

.PHONY: shell
shell:
	$(GUIX) shell -L src $(PACKAGES)

# ** Other

.PHONY: doctor status
doctor status:
	@echo "Machine			: $(MACHINE)"
	@echo "Hostname		: $(HOSTNAME)"
	@echo "Lockfile		: $(LOCKFILE)"
	@echo "Existing lockfiles	: $(shell ls env/channels-lock-*.scm 2>/dev/null | xargs -n1 basename || echo None)"
