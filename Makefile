all: clean build package

clean:
	rebar clean

deps:
	rebar get-deps

build: deps
	rebar compile

package: build
	rebar generate -f

console: package
	rel/chat/bin/chat console

console_clean: package
	rel/chat/bin/chat console_clean

