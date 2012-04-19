#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s simplechat \
	-name simplechat
