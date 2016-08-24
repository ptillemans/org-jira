CASK ?= cask
NPM ?= npm

all: install test

test: unit ecukes

unit:
	${CASK} exec ert-runner -l jiralib.el 

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install

.PHONY:	all test unit ecukes install
