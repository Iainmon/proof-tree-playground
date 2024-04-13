#!/usr/bin/env bash

git pull;

npx kill-port 3000;
npx kill-port 9000;

cd frontend;
yarn;
yarn run start-server;

cd ..
cd server
yarn;
rm -rf dist-newstyle
rm nohup.out
# nohup yarn run run-server &
nohup cabal run server &
