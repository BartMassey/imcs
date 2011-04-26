# Copyright © 2009 Bart Massey
# ALL RIGHTS RESERVED
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -Wall -fno-warn-orphans # -O2 # -prof -auto-all # -debug # -Wall 
SRCS = Board.hs Game.hs IMCS.hs Log.hs Rating.hs \
       Service.hs SNewLine.hs State.hs Version.hs

imcs: $(SRCS)
	$(HC) $(HCFLAGS) -o imcs --make IMCS.hs

clean:
	-rm -f imcs
	-rm -f *.o *.hi *.hp *.prof
