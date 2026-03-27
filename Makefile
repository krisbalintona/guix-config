SHELL := $(shell printenv SHELL) # Run in user's shell
_GUIX = guix
GUIX = time $(_GUIX)
GUIX_LOCKED = $(GUIX) time-machine --channels=$(CHANNELS_LOCK_FILE) --
LOAD_PATHS = -L src
ENV_DIR = ./env
CHANNELS_FILE = $(ENV_DIR)/channels.scm
CHANNELS_LOCK_FILE = $(ENV_DIR)/channels-lock-$(MACHINE).scm
FAST_BUILD_ARGS = -c 0 -M 3

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
	$(GUIX) pull -C $(CHANNELS_FILE)

pull-lock:
	$(GUIX) pull -C $(CHANNELS_LOCK_FILE)

.PHONY: upgrade
upgrade: pull lock

# ** Channel lock files

# Create a lock target but also make each lock file be their own
# target (so that other targets can depend on them)
#
# We define the phony FORCE target and add it as a dependent target
# for CHANNELS_LOCK_FILE to force its regeneration (otherwise, most of
# the time Make will think the CHANNELS_LOCK_FILE is up-to-date and
# not regenerate it).
.PHONY: lock FORCE
lock: $(CHANNELS_LOCK_FILE)
$(CHANNELS_LOCK_FILE): $(CHANNELS_FILE) FORCE
	$(GUIX) describe --format=channels $(CHANNELS_FILE) > $@
	@echo "Created $@"

# ** System

system system-build system-lock: _GUIX = sudo guix
system: SYSTEM_ACTION = reconfigure
system-build: SYSTEM_ACTION = build
system system-lock: SYSTEM_EXTRA_FLAGS =

# WSL should not be configured with a bootloader, since Windows does
# its own things for booting.  (Forgetting this flag will break the
# Guix WSL install!)
ifeq ($(MACHINE),wsl)
system system-lock: SYSTEM_EXTRA_FLAGS += --no-bootloader
endif

SYSTEM-EXPRESSION := '(@ (krisb config machines $(MACHINE) system) $(MACHINE)-operating-system)'

.PHONY: system system-build
system system-build:
	$(GUIX) system $(SYSTEM_ACTION) \
		$(LOAD_PATHS) \
		${FAST_BUILD_ARGS} \
		--keep-failed --verbosity=3 \
		$(SYSTEM_EXTRA_FLAGS) \
		--expression=$(SYSTEM-EXPRESSION)

.PHONY: system-lock
system-lock: $(CHANNELS_LOCK_FILE)
	$(GUIX_LOCKED) system reconfigure \
		$(LOAD_PATHS) \
		${FAST_BUILD_ARGS} \
		$(SYSTEM_EXTRA_FLAGS) \
		--expression=$(SYSTEM-EXPRESSION)

# ** Home

home: HOME_ACTION = reconfigure
home-build: HOME_ACTION = build

HOME-EXPRESSION := '(@ (krisb config machines $(MACHINE) home) $(MACHINE)-home-environment)'

.PHONY: home home-build
home home-build:
ifeq ($(MACHINE),wsl)
	$(error The wsl has no home config.)
endif
	$(GUIX) home ${HOME_ACTION} \
		$(LOAD_PATHS) \
		${FAST_BUILD_ARGS} \
		--keep-failed --verbosity=3 \
		--expression=$(HOME-EXPRESSION)

.PHONY: home-lock
home-lock:
ifeq ($(MACHINE),wsl)
	$(error The wsl has no home config.)
endif
	$(GUIX_LOCKED) home reconfigure \
		$(LOAD_PATHS) \
		--expression=$(HOME-EXPRESSION)

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
	$(GUIX) build $(LOAD_PATHS) --keep-failed --log-file --verbosity=3 $(PACKAGES)

.PHONY: shell
shell:
	$(GUIX) shell $(LOAD_PATHS) $(PACKAGES)

.PHONY: repl
repl: repl
	guix repl $(LOAD_PATHS)


# ** Other

.PHONY: doctor status
doctor status:
	@echo "Machine              : $(MACHINE)"
	@echo "Hostname             : $(HOSTNAME)"
	@echo "CHANNELS_LOCK_FILE   : $(CHANNELS_LOCK_FILE)"
	@echo "Existing lockfiles   : $(shell ls env/channels-lock-*.scm 2>/dev/null | xargs -n1 basename || echo None)"
