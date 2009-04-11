# Copyright © 2009 Bart Massey
# ALL RIGHTS RESERVED
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -O2 # -prof -auto-all # -debug # -Wall 
SRCS = Board.hs State.hs Main.hs Connection.hs ParseArgs.hs

mcref: $(SRCS)
	$(HC) $(HCFLAGS) --make -o mcref Main.hs

clean:
	-rm -f mcref
	-rm -f *.o *.hi *.hp *.prof
