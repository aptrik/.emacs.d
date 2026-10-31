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

Packages are managed by [elpaca](https://github.com/progfolio/elpaca)
with a version lockfile at `elpaca-lock.el`. This ensures reproducible
installs across machines.

On first launch, elpaca bootstraps itself and clones all packages (takes a
few minutes). Subsequent startups are fast since packages are built and
cached under `elpaca/builds/`.

#### Installing a new package

1. Add a `use-package` declaration with `:ensure t` in `setup.el`:
   ```elisp
   (use-package some-package
     :ensure t
     :config
     ...)
   ```
   For packages that need a custom recipe (non-MELPA source, specific repo, etc.)
   use the `:elpaca` keyword instead:
   ```elisp
   (use-package some-package
     :elpaca (:host github :repo "owner/repo")
     :config
     ...)
   ```
2. Restart Emacs (or eval the declaration) — elpaca clones and builds the package automatically.
3. Lock the new state:
   ```
   M-x elpaca-lock
   ```
4. Commit both `setup.el` and `elpaca-lock.el`.

#### Locking packages (reproducible install)

The lockfile `elpaca-lock.el` pins every package to an exact git commit.

- On a fresh clone, start Emacs — elpaca reads the lockfile and checks out pinned commits.
- To regenerate the lockfile after any change: `M-x elpaca-lock`
- To restore packages to their locked versions: `M-x elpaca-unlock`

#### Upgrading packages

    M-x elpaca-merge-all           ; fetch and merge latest commits for all packages

Then rebuild and test your config:

    M-x elpaca-rebuild-all         ; rebuild all packages after merging

If everything works:

    M-x elpaca-lock                ; update the lockfile

Commit the updated lockfile. To upgrade a single package instead:

    M-x elpaca-merge               ; prompts for package name
    M-x elpaca-rebuild             ; rebuild that package
    M-x elpaca-lock

#### Rolling back a bad upgrade

    git checkout elpaca-lock.el

Restart Emacs — packages revert to the previously committed versions.

#### Browsing installed packages

    M-x elpaca-manager

Opens a buffer listing all installed packages with their status, allowing
you to browse, rebuild, or delete individual packages interactively.

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

    brew tap d12frosted/emacs-plus
    brew trust d12frosted/emacs-plus
    brew install --cask emacs-plus-app

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
