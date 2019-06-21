all: output/
	npx pulp run

build:
	npx pulp build

clean:
	rm -rf node_modules bower_components output

install:
	npm ci
	npx bower install

output/: node_modules/ bower_components/
	npx pulp build

node_modules/:
	npm ci

bower_components/:
	npx bower install

.PHONY: build clean install
