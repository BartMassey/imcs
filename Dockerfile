FROM haskell
ADD ./ ./src
RUN cd src && cabal update && cabal build
WORKDIR src/dist-newstyle/build/x86_64-linux/ghc-8.8.3/imcs-2.5.0.2/x/imcs/build/imcs/
RUN ./imcs --init
EXPOSE 3589
ENTRYPOINT ["./imcs"]