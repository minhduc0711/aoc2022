.PHONY: clean

%: %.sc
	scalac $< -explain
	scala $@

clean:
	rm *.tasty *.class
