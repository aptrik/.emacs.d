# :sparkles: .emacs.d :sparkles:

This is where I store all my carefully crafted micro-optimizations for
Emacs.

## Installation

**Note:** The code is designed to be installed in the
[standard](https://www.emacswiki.org/emacs/DotEmacsDotD) Emacs user
directory.

Fetch the source.

    git clone  git://github.com/aptrik/.emacs.d.git  ~/.emacs.d

Make sure you have Emacs version 29.1 or higher [installed](#install-emacs).

### Install Emacs Lisp packages

Packages will be installed automatically. Use M-x list-packages to update.

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

### Build from source

    curl -LO https://ftpmirror.gnu.org/emacs/emacs-29.2.tar.xz
    tar xfJ emacs-29.2.tar.xz
    cd emacs-29.2
    ./configure --prefix=/usr/local --without-toolkit-scroll-bars
    make
    ./src/emacs -q
    sudo make install

#### Fedora & RHEL8 dependencies

    sudo dnf builddep -y emacs
    sudo dnf install -y giflib-devel gnutls-devel gpm-devel gtk2-devel gtk3-devel jansson-devel lcms2-devel libX11-devel
    sudo dnf install -y libXpm-devel libjpeg-devel librsvg2-devel libtiff-devel libxml2-devel ncurses-devel systemd-devel

## Measure startup time

    emacs -nw --eval='(message "%s" (emacs-init-time))'

## Emacs Help

* `F1 t`  Basic tutorial.
* `F1 k`  Help for a keybinding.
* `F1 r`  Emacs' extensive documentation.
