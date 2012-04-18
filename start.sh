#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s simplechat \
	-s appmon \
	-name simplechat
