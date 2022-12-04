.PHONY: clean

notarget:
	@echo "Plz specify a make target"

%: %.scala
	scala-cli $< --watch

clean:
	rm -r *.tasty *.class .scala-build
