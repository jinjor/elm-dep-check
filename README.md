# elm-dep-check

[![Build Status](https://travis-ci.org/jinjor/elm-dep-check.svg)](https://travis-ci.org/jinjor/elm-dep-check)

Visualizing module dependencies of Elm projects.

![](image.png)

## Features

* Visualize dependencies among modules/directories
* Detect circular dependencies among directories

Circular module dependencies are detected by Elm compiler, but in general, it also makes your project healthy to keep directory dependencies unidirectional!

It is also good to check your project's local rules are observed (e.g. `View.*` should not be used by `Model.*`).


## Install

```
npm install -g elm-dep-check
```

## Usage

Go to your project directory and use this command.

```
elm-dep-check
```

This creates `dep-check.html` in the current directory.

About other options, see below.

```
Usage: elm-dep-check [options]

	--help, -h
		Displays help information about this script
		'elm-dep-check -h' or 'elm-dep-check --help'

	--output, -o
		Output file (default is `dep-check.html`)
		'elm-dep-check --output=report.html' or 'elm-dep-check -o report.html

```

## LICENSE

BSD3
