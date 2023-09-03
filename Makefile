CWD = $(shell pwd)

include env.mk
all: emacs

anaconda:
	wget -O anaconda.sh "${ANACONDA_URL}"; bash anaconda.sh; rm anaconda.sh
	conda install virtualenv
	pip install epc

emacs-dep:
	sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev libtinfo-dev mailutils libgnutls28-dev bear git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev software-properties-common snapd
	sudo add-apt-repository ppa:git-core/ppa
	sudo apt-get update
	sudo apt-get install git
	sudo apt-get install magit
	sudo apt-get install cscope
	sudo apt-get install clangd-9
	sudo ln /usr/bin/clangd-9 /usr/bin/clangd
	sudo apt-get install libtree-sitter-dev
	sudo apt-get install python3-pip
#	pip3 install python-language-server[all]
   # sudo snap install bash-language-server
	sudo apt-get install libmagickwand-dev

	# for eglot java
	wget https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz
	tar xvfz openjdk-17.0.2_linux-x64_bin.tar.gz
	sudo mv jdk-17.0.2 /opt
	wget https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.26.0/jdt-language-server-1.26.0-202307271613.tar.gz
	mkdir jdtls
	tar xvfz download.php\?file\=%2Fjdtls%2Fmilestones%2F1.26.0%2Fjdt-language-server-1.26.0-202307271613.tar.gz -C jdtls
	mkdir -p ~/.emacs.d
	mv jdtls ~/.emacs.d
	echo "export JAVA_HOME=/opt/jdk-17" >> ~/.bashrc
	echo "export PATH=$$PATH:$$JAVA_HOME/bin:~/.emacs.d/jdtls/bin" >> ~/.bashrc
	echo "export JAVA_HOME=/opt/jdk-17" >> ~/.bashrc

emacs: #emacs-dep
#	https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
	wget -O - "${EMACS_URL}" | tar -xz 
	cd emacs-${EMACS_VER}; \
	./configure --with-imagemagick --with-tree-sitter; \
	make; \
	sudo make install;
	cp .emacs ~/
	emacs

#M-x treesit-install-language-grammar
#M-x treesit-langs-install-grammar

# FIXME: complete cleanup
clean: 
	for D in ./*; do if [ -d "$${D}" ]; then cd "$${D}"; make clean; fi; done 

.PHONY: clean

discard: clean
	rm -rf emacs-${EMACS_VER}

.PHONY: discard
