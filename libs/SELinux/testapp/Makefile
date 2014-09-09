CC = gcc

CC_OPTS = -Wall -Werror -I/usr/include/selinux -lselinux

APP_NAME = testapp

APP_DIR = /usr/local/bin

.PHONY: default
default: test

.PHONY: build
build: $(APP_NAME)

$(APP_NAME): $(APP_NAME).c Makefile
	$(CC) $(CC_OPTS) -o $@ $<

.PHONY: install
install: build $(APP_DIR)/$(APP_NAME)

$(APP_DIR)/$(APP_NAME): $(APP_NAME)
	sudo cp $< $@

.PHONY: test
test: install policy-apply
	$(APP_DIR)/$(APP_NAME)

.PHONY: clean
clean:
	rm -fr $(APP_NAME) $(APP_NAME).pp .installed-policy tmp

.PHONY: policy-initial
policy-initial:
	/usr/share/selinux/devel/policygentool $(APP_NAME) $(APP_DIR)/$(APP_NAME)

.PHONY: policy
policy: policy-build policy-install policy-apply

.PHONY: policy-build
policy-build: $(APP_NAME).pp

$(APP_NAME).pp: $(APP_NAME).fc $(APP_NAME).if $(APP_NAME).te
	$(MAKE) -f /usr/share/selinux/devel/Makefile

.PHONY: policy-install
policy-install: .installed-policy policy-apply

.installed-policy: $(APP_NAME).pp
	sudo /usr/sbin/semodule -i $<
	echo -n > $@

.PHONY: policy-apply
policy-apply: .installed-policy
	sudo /sbin/restorecon -R -v $(APP_DIR)/$(APP_NAME)

.PHONY: policy-debug
policy-debug:
	audit2allow -l -i /var/log/messages

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) test
