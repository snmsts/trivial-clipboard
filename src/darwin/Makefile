clipboard.so: clipboard.m
	$(CC) -shared -undefined dynamic_lookup -o $@ -framework Appkit $?
clean:
	$(RM) clipboard.so
.PHONY: clean
