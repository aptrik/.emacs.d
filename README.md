# :sparkles: .emacs.d :sparkles:

This is where I store all my carefully crafted micro-optimizations for
Emacs.

## Installation

**Note:** The code is designed to be installed in the
[standard](http://www.emacswiki.org/emacs/DotEmacsDotD) Emacs user
directory.

Fetch the source.

    git clone  git://github.com/aptrik/.emacs.d.git  ~/.emacs.d

Make sure you have Emacs version 25.1 or higher [installed](#install-emacs).

### Install Emacs Lisp packages

First install [Cask](http://cask.readthedocs.org/):

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    ...
    Successfully installed Cask!  Now, add the cask binary to your $PATH:
    export PATH="$HOME/.cask/bin:$PATH"

Lock version of Cask (newer version does not support package and pallet)

    cd ~/.cask
    git checkout v0.8.8

Let `cask` install all dependencies.

    cd ~/.emacs.d
    cask install  # or `cask update` to get the latest versions of packages

### Compile pdf-tools

    (setenv "PKG_CONFIG_PATH" (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))
    (pdf-tools-install)

### Install python-mode dependencies

    pip install -r ~/.emacs.d/python_requirements.txt

### User customization

If there is a file named `user.el` in the `~/.emacs.d`-directory then it
will be loaded at the end of the set up phase.

Example `~/.emacs.d/user.el`:

```lisp
(setq debug-on-error t)

(setq user-full-name    "Ed Xample"
      user-mail-address "ed.xample@eg.fake")
```

## Install Emacs

NOTE: Do not forget to run `cask install` after changing Emacs version.

### Ubuntu

    sudo apt-add-repository -y ppa:adrozdoff/emacs
    sudo apt update && sudo apt install emacs25
    sudo update-alternatives --config emacs

### MacOS

    brew install --cask emacs

Or the bleeding edge version.

    brew install emacs --srgb --with-cocoa --with-gnuls --use-git-head --HEAD && brew linkapps

### Build from source

    curl -LO http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz
    tar xfJ emacs-27.2.tar.xz
    cd emacs-27.2
    ./configure --prefix=/usr/local --without-toolkit-scroll-bars
    make
    ./src/emacs -q
    sudo make install

#### RHEL 7 dependencies

    sudo yum-builddep -y emacs
    sudo yum install -y libX11-devel gtk2-devel gtk+-devel
    sudo yum install -y ncurses-devel dbus-devel gpm-devel librsvg2-devel GConf2-devel
    sudo yum install -y libtiff-devel giflib-devel libotf-devel m17n-lib-devel libXpm-devel libjpeg-devel ImageMagick-devel

#### Fedora & RHEL8 dependencies

    sudo dnf builddep -y emacs
    sudo dnf install -y giflib-devel gnutls-devel gpm-devel gtk2-devel gtk3-devel jansson-devel lcms2-devel libX11-devel libXpm-devel libjpeg-devel librsvg2-devel libtiff-devel libxml2-devel ncurses-devel systemd-devel

#### Ubuntu/LinuxMint dependencies

    sudo apt-get build-dep emacs24
    sudo apt-get install \
        libacl1-dev \
        libgconf2-dev \
        libgif-dev \
        libgpm-dev \
        libjpeg-dev \
        libm17n-dev \
        libmagickcore-dev libmagickwand-dev \
        libotf-dev \
        libpng-dev \
        librsvg2-dev \
        libtiff-dev \
        libxml2-dev \
        libxpm-dev


## Measure startup time

    emacs -Q -nw --eval='(message "%s" (emacs-init-time))'

## Emacs Help

* `F1 t`  Basic tutorial.
* `F1 k`  Help for a keybinding.
* `F1 r`  Emacs' extensive documentation.
