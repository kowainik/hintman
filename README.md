# hintman

[![Hackage](https://img.shields.io/hackage/v/hintman.svg)](https://hackage.haskell.org/package/hintman)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/hintman/badge/lts)](http://stackage.org/lts/package/hintman)
[![Stackage Nightly](http://stackage.org/package/hintman/badge/nightly)](http://stackage.org/nightly/package/hintman)
[![Build status](https://secure.travis-ci.org/kowainik/hintman.svg)](https://travis-ci.org/kowainik/hintman)

GitHub application to suggest hints


## How to test GitHub application?

### Prerequisites

1. `smee`:
   + `npm install --global smee-client`
2. `cabal` or `stack`

### How to run

1. Run `smee` forwarder in a separate terminal window:
    + `smee -u "https://smee.io/uTG0BCXnjq4DEff7" -p 8080`
2. Run `backend`:
    + `KEY=<SECRET_KEY> PK_PATH=<PATH_TO_PEM_FILE> cabal new-run hintman -- -p 8080`
3. Open GitHub application and press `Configure` button to add it to your account:
    + https://github.com/apps/hint-man
4. After adding access to your personal profile, choose `Select repositories`
   repository access and it this application to any of your repositories. After
   adding the repo to app and saving preferences, you should be able to see the
   following:
   + Incoming requests in the running backend
   + Terminal with `smee` shows successfull status codes
   + You can visit `smee` URL from above to see events there.

Current backend just prints to terminal all incoming requests. GitHub app
installation is one of those requests.
