CWD = $(shell pwd)

include env.mk
all: install-emacs install-packages

install-anaconda:
	wget -O anaconda.sh "${ANACONDA_URL}"; bash anaconda.sh; rm anaconda.sh
	conda install virtualenv
	pip install epc

install-dependencies:
	sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev libtinfo-dev mailutils libgnutls28-dev bear git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev snapd
	sudo add-apt-repository ppa:git-core/ppa
	sudo apt-get update
	sudo apt-get install git
	sudo apt-get install magit
	sudo apt-get install cscope
	sudo apt-get install clangd-9
	sudo apt-get install clang
	sudo apt-get install python3-pip
#	pip3 install python-language-server[all]
	sudo snap install bash-language-server
	sudo apt-get install openjdk-11-jdk
	echo "export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64" >> ~/.bashrc
	echo "export PATH=$$PATH:$$JAVA_HOME/bin" >> ~/.bashrc

install-emacs: install-dependencies
	#https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
	wget -O - "${EMACS_URL}" | tar -xz 
	cd emacs-${EMACS_VER}; \
	cp .emacs ~/

install-packages:
	cp .emacs ~/
	emacs

# FIXME: complete cleanup
clean: 
	for D in ./*; do if [ -d "$${D}" ]; then cd "$${D}"; make clean; fi; done 

.PHONY: clean

discard: clean
	rm -rf emacs-${EMACS_VER}

.PHONY: discard
