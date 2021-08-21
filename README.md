# Solutions to some exercises from the book ANSI Common Lisp

This repository contains solutions that I have written to a few
of the exercises from the book *ANSI Common Lisp* by Paul Graham.
The book came out in 1996, and is an interesting read today as well
(in 2021), in my opinion.

## Paul Graham

pg, as you may know, was one of co-founders of
[Y Combinator](http://ycombinator.com/), the first of a new type
of startup incubator. Prior to that he co-founded
[Viaweb](https://en.wikipedia.org/wiki/Viaweb), in 1995.
They wrote Viaweb partially in Lisp.

Paul has written a couple of other books as well, including
*Hackers & Painters: Big Ideas from the Computer Age*. This other
book I have read also and I enjoyed that one too.

## The book ANSI Common Lisp

You can read reviews and stuff about the book *ANSI Common Lisp* here:

https://www.goodreads.com/book/show/41801.ANSI_Common_Lisp

And you can see the Amazon listing for the book here (affiliate link):

[![An image showing the cover of the book ANSI Common Lisp by Paul Graham](https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0133708756&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=codetrotter-20)](https://www.amazon.com/gp/product/0133708756/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0133708756&linkCode=as2&tag=codetrotter-20&linkId=79f72e2b82ad2b55bae9fbbe53ee4a65)

[ANSI Common Lisp listing on Amazon](https://www.amazon.com/gp/product/0133708756/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0133708756&linkCode=as2&tag=codetrotter-20&linkId=5c1780d903bedacafcd8ee7f8a27d3b0)

You will probably want to buy it used. That's what I did. I mean, it's
a good book, but I don't think almost any book should cost you >$100 USD
in 2021-dollars. Somewhere between $15 and $45 is a fair price
in 2021-dollars for most good books.

Buying it used from Amazon you can get it at a decent price.
Buying it new from Amazon will cost you over $100 at the moment. So yeah,
buy it used somewhere. Either at Amazon or someplace else.

## About these solutions

All the solutions include a comment at the start of the file, with a reproduction
of the assignment text so that it makes sense to the reader what the program is trying
to do without having to consult with the paperback copy of the ANSI Common Lisp book.

For example, here's the comment that you will find at the top of the file `e-10-1.cl`:

```sbcl
;
; # Chapter 10 – Exercise 1.
;
; > If x is a, y is b, and z is (c d), write backquoted expressions
; > containing only variables that yield each of the following:
; >
; > (a) ((C D) A Z)
; > (b) (X B C D)
; > (c) ((C D A) Z)
;
```

And as you can probably tell from the above, exercise solutions are named according to
chapter and exercise number.

## Running the solutions

Run any of the solutions, e.g.:

```zsh
./e-10-1.cl
```

Requires that `sbcl` is installed on the computer. Refer to *prerequisites* below.

### Special notes about some of the solutions.

* `e-11-2.cl` produces a file named `spheres.pgm` as its output.
  The PGM format is a lowest common denominator grayscale file format.
  It is designed to be extremely easy to learn and write programs for.
  See http://netpbm.sourceforge.net/doc/pgm.html for details
  about the PGM format.  On macOS you can `open spheres.pgm` or
  double-click on it and by default it will open in Preview where you can
  view it. On other platforms there are programs that allow you to view
  PGM files as well. Look up the details for your platform online.

## Prerequisites

Install Steel Bank Common Lisp.

### macOS

Assuming you have [Homebrew](https://brew.sh/) installed:

```zsh
brew install sbcl
```

### FreeBSD

(The command below needs to be run as root, I recommend using `doas` to execute commands as root.)

```zsh
pkg install sbcl
```

### Linux

Use the package manager or other type of preferred installation method that they use on your distro.
Here's how to do it for some of the most common Linux distros.

#### Debian / Ubuntu / KDE Neon / most Debian derivatives:

```zsh
sudo apt-get install sbcl
```

#### Fedora

```zsh
sudo dnf install sbcl
```

#### openSUSE Tumbleweed

```zsh
sudo zypper install sbcl
```

#### openSUSE Leap

Not available on the current version of openSUSE Leap (15.3) at the time of this writing
according to https://software.opensuse.org/package/sbcl

#### Arch Linux

```zsh
sudo pacman -S sbcl
```

#### Gentoo Linux

```zsh
emerge --ask dev-lisp/sbcl
```
