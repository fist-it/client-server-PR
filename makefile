server: build
	@echo "Starting server..."
	@cabal run

build:
	@echo "Building project..."
	@cabal build

connect:
	@echo "Connecting to server..."
	@nc localhost 3000
