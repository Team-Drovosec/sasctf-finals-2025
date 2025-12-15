FROM haskell:9.4

WORKDIR /app
RUN mkdir src && touch ./src/Main.hs
COPY ./auth.cabal .
RUN cabal update && cabal install --only-dependencies

COPY ./src ./src
RUN cabal build
COPY ./index.html .

CMD ["cabal", "run", "auth"]
