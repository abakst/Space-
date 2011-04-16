# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=main
SRCS=   Boids.hs \
	KdTree.hs \
	Main.hs \
	MainWrapper.hs \
	Sprite.hs

$(PROGNAME): mainc.o SDLMain.o MainWrapper.hs Main.hs
	ghc -no-hs-main -framework Cocoa -framework SDL_mixer -framework SDL_gfx -framework SDL -framework smpeg --make SDLMain.o mainc.o MainWrapper.hs -o $@

SDLMain.o:
	gcc -c SDLMain.m -I/Library/Frameworks/SDL.framework/Headers -arch i386

mainc.o: mainc.c hs_srcs #MainWrapper_stub.h
	ghc -no-hs-main -I/Library/Frameworks/SDL.framework/Headers -Wall $*.c -c

hs_srcs:
	ghc --make -no-hs-main -c MainWrapper.hs

clean:
	rm -f *.hi *.o *_stub.c *_stub.h *~ $(PROGNAME)

depend:
	ghc -M $(SRCS)
#
# DO NOT DELETE: Beginning of Haskell dependencies
Sprite.o : Sprite.hs
KdTree.o : KdTree.hs
Boids.o : Boids.hs
Boids.o : KdTree.hi
Main.o : Main.hs
Main.o : KdTree.hi
Main.o : Boids.hi
Main.o : Sprite.hi
MainWrapper.o : MainWrapper.hs
MainWrapper.o : Main.hi
# DO NOT DELETE: End of Haskell dependencies
