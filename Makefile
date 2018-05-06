all: euler.jar euler

euler.jar: src/main/java/de/scravy/euler/*.java
	mvn -Dorg.slf4j.simpleLogger.defaultLogLevel=warn package
	mv target/euler.jar .

euler: euler.hs
	ghc euler.hs -o euler

clean:
	rm -rf target/ dist/ euler.o euler.hi

purge: clean
	rm -rf euler euler.jar

