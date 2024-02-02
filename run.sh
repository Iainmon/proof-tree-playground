#!/usr/bin/env bash

git pull;

npx kill-port 3000;
npx kill-port 9000;

cd frontend;
yarn run start-server;

cd ../server;
nohup cabal exec server &;
