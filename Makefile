CWD = $(shell pwd)

include env.mk
all: codex emacs

anaconda:
	wget -O anaconda.sh "${ANACONDA_URL}"; bash anaconda.sh; rm anaconda.sh
	conda install virtualenv
	pip install epc
codex:
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
	. ~/.bashrc
	nvm install --lts
	nvm use --lts
	npm i -g @openai/codex@latest
emacs-dep:
	sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev libtinfo-dev mailutils libgnutls28-dev bear git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev software-properties-common snapd python3-pip cmake libvterm-dev xclip python3-venv universal-ctags graphviz sqlite3 libsqlite3-0 libsqlite3-dev

	python3 -m venv ~/.venv
	echo "source ~/.venv/bin/activate" >> ~/.bashrc
	. ~/.bashrc
#	sudo add-apt-repository ppa:git-core/ppa
	sudo apt-get install git
	sudo apt-get install magit
	sudo apt-get install cscope
	sudo apt-get install clangd-15 clang-format
	sudo ln -sf /usr/bin/clangd-15 /usr/bin/clangd
	cp .clangd ~/.clangd    #linux style format
	git clone https://github.com/tree-sitter/tree-sitter.git
	cd tree-sitter; \
	git checkout v0.26.5; \ #ABI 15
	make; sudo make install; sudo ldconfig
	sudo apt-get install python3-pip
	pip install python-language-server[all]
#	sudo snap install bash-language-server
	sudo apt-get install libmagickwand-dev

#       for eglot java
	wget https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz
	tar xvfz openjdk-17.0.2_linux-x64_bin.tar.gz
	sudo rm -rf /opt/jdk-17.0.2
	sudo mv -f jdk-17.0.2 /opt
	wget https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz
	mkdir -p jdtls
	tar xvfz download.php?file=%2Fjdtls%2Fsnapshots%2Fjdt-language-server-latest.tar.gz -C jdtls
	rm -rf ~/.emacs.d
	mkdir -p ~/.emacs.d/.cache
	cp ./treemacs-persist ~/.emacs.d/.cache/
	mkdir -p ~/.ssh/
	cp .ssh/config ~/.ssh/config
	mv jdtls ~/.emacs.d
	echo "export JAVA_HOME=/opt/jdk-17" >> ~/.bashrc
	echo "export PATH=$$PATH:$$JAVA_HOME/bin:~/.emacs.d/jdtls/bin" >> ~/.bashrc
	echo "export JAVA_HOME=/opt/jdk-17" >> ~/.bashrc
#	for ggtags especially with python
	sudo apt-get install global
	pip3 install pygments
	pip install pygments
	#git clone https://github.com/sijk/pygments-bitbake.git
	python -m pip install --upgrade pip setuptools wheel
	git clone https://github.com/JungmoAhn/pygments-bitbake.git
	cd pygments-bitbake; \
	python setup.py install;
	echo "export GTAGSLABEL=pygments" >> ~/.bashrc
	sudo cp /etc/gtags/gtags.conf ~/.globalrc
	sed -i 's/tc=exuberant-ctags:tc=htags/tc=universal-ctags:tc=htags/' ~/.globalrc

	echo "export GTAGSCONF=~/.globalrc" >> ~/.bashrc
	sed -i "s/:tc=native:/:tc=native:tc=pygments:/" ~/.globalrc
	sed -i '/:gtags_parser=C#\\:$pygmentslib:\\/i \\t:gtags_parser=Bitbake\\:$pygmentslib:\\' ~/.globalrc
	sed -i '/:langmap=C#\\:.cs:\\/i \\t:langmap=Bitbake\\:.bb.bbappend.bbclass.conf.inc:\\' ~/.globalrc

emacs: emacs-dep
	wget -O - "${EMACS_URL}" | tar -xz
	cd emacs-${EMACS_VER}; \
	./configure --with-imagemagick --with-tree-sitter; \
	make; \
	sudo make install;
	cp .emacs ~/

	git clone https://github.com/pekingduck/emacs-sqlite3-api.git
	cd emacs-sqlite3-api; \
	make; \
	emacs --batch -l package --eval "(progn (package-initialize) (package-install-file \"sqlite3-0.16.tar\"))"
	git clone https://github.com/pekingduck/sqlite3.el.git
	cd sqlite3.el; \
	make; \
	make install
	mkdir -p ~/.emacs.d/lisp
	cp -f sqlite3.el/sqlite3.el ~/.emacs.d/lisp/
	emacs -nw

# FIXME: complete cleanup
clean:
	for D in ./*; do if [ -d "$${D}" ]; then cd "$${D}"; make clean; fi; done

.PHONY: clean

discard: clean
	rm -rf emacs-${EMACS_VER}

.PHONY: discard
