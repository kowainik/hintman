# hintman

[![GitHub App](https://img.shields.io/badge/GitHub-marketplace-yellow?logo=GitHub&style=flat-square)](https://github.com/apps/hint-man)
[![Build status](https://secure.travis-ci.org/kowainik/hintman.svg)](https://travis-ci.org/kowainik/hintman)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Hintman is a GitHub application that automatically submits reviews to opened
pull requests in your repositories with inline hints. It heavily makes the use
of the [GitHub suggestions](https://github.blog/changelog/2018-10-16-suggested-changes/)
feature, so that all the hints could be applied in one click.

⚠️ __WARNING:__ Hintman is in early beta phase. ⚠️

## References

Hintman is inspired by the [Hitman video game series](https://en.wikipedia.org/wiki/Hitman_(franchise)).
The standard replies of Hintman are based on the quotes from the game.
And the current avatar is the hint as well.

## Features

Current set of features includes:

+ _Trailing spaces_ removal via GitHub suggestion mechanism
+ _Trailing newlines_ removal via GitHub suggestion mechanism
+ `[Haskell only]` Submitting inline [HLint](https://github.com/ndmitchell/hlint)
  suggestions to Haskell source code files

Stay tuned and watch for the updates!

## Motivation

If you're an open source maintainer, reviewing incoming pull requests from
various contributors can be tedious. You want to ensure excellent code quality.
But you don't want to spend your time on things that can be automated. Hintman
helps you with that! It submits inline suggestions to all open pull requests so
you can apply them immediately.

## Installation

To enable Hintman reviews, follow the link below and install it for
all accounts and repositories where you would like it to use.

* [Hintman App](https://github.com/apps/hint-man)


## Hintman's review

Based on the gathered feedback, there could be two states of the Hintman bot:

1. No hints were found for the pull request. In this case Hintman will `Approve`
   the PR and leave the "There is no place for me here... I will choose the truth I like."
   comment. You would see the following:
   ![Hintman approve](https://user-images.githubusercontent.com/8126674/67259322-3e63ff00-f4a6-11e9-8893-771dfbdefe76.png)
2. Hintman could appeal to some enhancements. Some of the supported by the app
   changes can be made. In this case, Hintman will `Comment` the PR and leave
   the "Do you know why your PR is still not approved? Because I chose not to
   approve it. But they will." comment along with the hints as GitHub suggestions.
   You would see the following:
   ![Hintman not approves](https://user-images.githubusercontent.com/8126674/67259659-255c4d80-f4a8-11e9-8e2f-ec2d21af07cf.png)
