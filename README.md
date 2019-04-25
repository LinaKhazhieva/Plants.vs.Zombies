![](assets/logo.jpg)

# Plants vs. Zombies in Haskell

<!-- [![Build Status](https://travis-ci.org/iu-haskell-fall-2018/project-template.svg?branch=master)](https://travis-ci.org/iu-haskell-fall-2018/project-template) -->

Haskell project 2019 spring Innopolis University.

![](assets/fight.gif)

### Members
* Kamilla Borodina
* Alina Chasova

![](assets/name.gif)

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.4.3).

## Run

This project has one executable that you can run with

```
stack exec plants-vs-zombies-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec plants-vs-zombies-exe
```


