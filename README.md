# :sparkles: .emacs.d :sparkles:

This is where I store all my carefully crafted micro-optimizations for
Emacs.

## Installation

**Note:** The code is designed to be installed in the
[standard](http://www.emacswiki.org/emacs/DotEmacsDotD) Emacs user
directory.

Fetch the source.

    git clone  git://github.com/aptrik/.emacs.d.git  ~/.emacs.d

Make sure you have Emacs version 27.1 or higher [installed](#install-emacs).

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

    pip install -r ~/.emacs.d/requirements.in

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

### MacOS

    brew install --cask emacs

or [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus)

    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-native-comp
    ln -s /opt/homebrew/opt/emacs-plus@28/Emacs.app /Applications

### Build from source

    curl -LO http://ftpmirror.gnu.org/emacs/emacs-28.1.tar.xz
    tar xfJ emacs-28.1.tar.xz
    cd emacs-28.1
    ./configure --prefix=/usr/local --without-toolkit-scroll-bars
    make
    ./src/emacs -q
    sudo make install

#### Fedora & RHEL8 dependencies

    sudo dnf builddep -y emacs
    sudo dnf install -y giflib-devel gnutls-devel gpm-devel gtk2-devel gtk3-devel jansson-devel lcms2-devel libX11-devel
    sudo dnf install -y libXpm-devel libjpeg-devel librsvg2-devel libtiff-devel libxml2-devel ncurses-devel systemd-devel

## Measure startup time

    emacs -Q -nw --eval='(message "%s" (emacs-init-time))'

## Emacs Help

* `F1 t`  Basic tutorial.
* `F1 k`  Help for a keybinding.
* `F1 r`  Emacs' extensive documentation.
