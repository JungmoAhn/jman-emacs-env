CWD = $(shell pwd)

# default values for EMACS
EMACS_VER ?= 29.3
EMACS_URL ?= http://ftp.kaist.ac.kr/gnu/emacs/emacs-${EMACS_VER}.tar.gz

CFLOW_URL ?= https://fossies.org/linux/misc/cflow-1.6.tar.xz

ANACONDA_URL ?= https://3230d63b5fc54e62148e-c95ac804525aac4b6dba79b00b39d1d3.ssl.cf1.rackcdn.com/Anaconda3-2.5.0-Linux-x86_64.sh
#ANACONDA_URL ?= https://repo.continuum.io/archive/Anaconda3-2.5.0-Linux-x86_64.sh
ANACONDA ?= Anaconda3-2.5.0-Linux-x86_64.sh

