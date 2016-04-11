default: helloworld helloworld.universal helloobjc hellorust

helloworld:
	xcrun clang helloworld.c -o $@
	xcrun otool -l $@ > $@.lc

helloworld.universal:
	xcrun clang -arch i386 -o helloworld.32 helloworld.c
	xcrun clang -arch x86_64 -o helloworld.64 helloworld.c
	xcrun lipo -create -arch i386 helloworld.32 -arch x86_64 helloworld.64 -output $@
	xcrun otool -arch i386 -l $@ > $@.i386.lc
	xcrun otool -arch x86_64 -l $@ > $@.x86_64.lc

helloobjc:
	xcrun clang -c foo.m
	xcrun clang -c helloobjc.m
	xcrun ar -cr libfoo.a foo.o
	xcrun clang -o $@ helloobjc.o libfoo.a -Wl,`xcrun --show-sdk-path`/System/Library/Frameworks/Foundation.framework/Foundation
	xcrun otool -l $@ > $@.lc

hellorust:
	rustc hellorust.rs -o $@ -g --crate-type bin
	xcrun otool -l $@ > $@.lc

clean:
	rm -r *.o *.lc helloworld helloworld.32 helloworld.64 helloworld.universal helloobjc hellorust hellorust.dSYM

.PHONY: clean helloworld helloworld.universal helloobjc hellorust
