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

### Package Management

Packages are managed by [straight.el](https://github.com/radian-software/straight.el)
with a version lockfile at `straight/versions/default.el`. This ensures reproducible
installs across machines.

On first launch, straight.el clones all packages from git (takes a few minutes).
Subsequent startups are fast since packages are cached locally.

#### Installing a new package

1. Add a `use-package` declaration in `setup.el`:
   ```elisp
   (use-package some-package
     :config
     ...)
   ```
2. Restart Emacs (or eval the declaration) — straight.el clones the package automatically.
3. Freeze the new state:
   ```
   M-x straight-freeze-versions
   ```
4. Commit both `setup.el` and `straight/versions/default.el`.

#### Locking packages (reproducible install)

The lockfile `straight/versions/default.el` pins every package to an exact git commit.

- On a fresh clone, start Emacs — straight reads the lockfile and checks out pinned commits.
- To regenerate the lockfile after any change: `M-x straight-freeze-versions`
- To restore packages to their locked versions: `M-x straight-thaw-versions`

#### Upgrading packages

    M-x straight-pull-all          ; fetch latest commits for all packages

Then test your config. If everything works:

    M-x straight-freeze-versions   ; update the lockfile

Commit the updated lockfile. To upgrade a single package instead:

    M-x straight-pull-package      ; prompts for package name
    M-x straight-freeze-versions

#### Upgrading a tag-pinned package

Some packages (e.g. org) are pinned to a release tag in `setup.el`:

```elisp
:straight (:tag "release_9.8.5")
```

To upgrade to a new release:

1. Update the `:tag` value in `setup.el`
2. In Emacs:
   ```
   M-x straight-pull-package      ; fetches new tags from remote
   M-x straight-normalize-package ; checks out the new tag
   M-x straight-freeze-versions   ; updates the lockfile
   ```
3. Commit `setup.el` and `straight/versions/default.el`

#### Rolling back a bad upgrade

    git checkout straight/versions/default.el

Restart Emacs — packages revert to the previously committed versions.

### Compile pdf-tools

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
