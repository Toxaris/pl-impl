#!/bin/bash
set -ev

# decrypt deployment key
openssl aes-256-cbc -K $encrypted_19acf01d4bba_key -iv $encrypted_19acf01d4bba_iv -in deploy-key.enc -out ~/.ssh/deploy-key -d
chmod 600 ~/.ssh/deploy-key

# configure ssh
cat >> ~/.ssh/config <<EOF
Host github
  Hostname github.com
  IdentityFile ~/.ssh/deploy-key
  IdentitiesOnly yes
EOF

# clone gh-pages into a subdirectory
git clone git@github:Toxaris/pl-impl --branch gh-pages gh-pages

# generate index.md
sed 's|This repository|The [Toxaris/pl-impl](http://github.com/Toxaris/pl-impl) repository on GitHub|' - README.md > gh-pages/index.md <<EOF
---
layout: default
title: Source code for Implementation of Programming Languages course
---
EOF

# configure documentation target directory
cat >> build.sbt <<EOF
target in Compile in doc := baseDirectory.value / "gh-pages" / "api"
EOF

# generate documentation
sbt doc

# figure out hash of HEAD commit
HEAD=$(git show-ref --head --hash HEAD)

# configure git for commiting website changes
git config --global user.email "rendel@informatik.uni-tuebingen.de"
git config --global user.name "Tillmann Rendel (via Travis)"

# commit website changes
pushd gh-pages
git add index.md
git add --all api
git commit -m "Update website." -m "Based on $HEAD."

# push to github
git push git@github:Toxaris/pl-impl gh-pages
popd
