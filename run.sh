#!/usr/bin/env bash

set -e

git pull

npx kill-port 3000
npx kill-port 9000

cd frontend
yarn
yarn run start-server

cd ..
cd server
yarn
rm -rf dist-newstyle
cabal clean
rm -f nohup.out
# nohup yarn run run-server &
nohup cabal run server -O2 &
