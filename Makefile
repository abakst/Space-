# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=main
$(PROGNAME): mainc.o SDLMain.o MainWrapper.hs Main.hs
	ghc -no-hs-main -framework Cocoa -framework SDL_mixer -framework SDL_gfx -framework SDL -framework smpeg --make SDLMain.o mainc.o MainWrapper.hs -o $@
SDLMain.o:
	gcc -c SDLMain.m -I/Library/Frameworks/SDL.framework/Headers -arch i386
mainc.o: mainc.c MainWrapper_stub.h
	ghc -no-hs-main -I/Library/Frameworks/SDL.framework/Headers -Wall $*.c -c
MainWrapper_stub.h: MainWrapper.hs
	ghc -no-hs-main --make $< -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#
