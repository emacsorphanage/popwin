popwin.el
=========

Overview
--------

popwin is a popup window manager for Emacs which makes you free from
the hell of annoying buffers such like `*Help*`, `*Completions*`,
`*compilation*`, and etc.

Take an example. When you complete file names during `find-file`, the
(annoying) `*Completions*` buffer will appear in a newly splitted
window. You might understand the necessity of the window, but you may
wonder why the window still remains after completion...

popwin resolves there problems. Windows of such temporary buffers will
be shown as a popup window, and you can close them smoothly by typing
`C-g` in anytime.

Screenshots
-----------

**Before Popup Window**

![](http://cx4a.org/software/popwin/popwin1.png)

**After Popup Window**

![](http://cx4a.org/software/popwin/popwin2.png)

Installation
------------

Install `popwin.el` into your `load-path` directory. If you have
`install-elisp` or `auto-install`, you also be able to install
`popwin.el` like:

    ;; install-elisp
    (install-elisp "https://github.com/m2ym/popwin-el/raw/master/popwin.el")
    ;; auto-install
    (auto-install-from-url "https://github.com/m2ym/popwin-el/raw/master/popwin.el")

And then add the following code into your `.emacs`:

    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)

There is an alternative way using `special-display-function` like:

    (require 'popwin)
    (setq special-display-function 'popwin:special-display-popup-window)

In this case, you need to change `special-display-buffer-names` or
`special-display-regexps` so that popwin sould handle such buffers.

popwin is tested under GNU Emacs 22 or later.

Basic Usage
-----------

Special buffers, for example `*Help*`, specified in
`popwin:special-display-config` will be shown in a popup window. You
can close the popup window by typing `C-g` or selecting other windows.

By default, `*Help*`, `*Completions*`, `*compilation*`, and `*Occur*`
buffers will be shown in a popup window. Try `M-x find-file` and type
`TAB TAB`. You may see a popup window at the bottom of the frame.

**File Name Completion**

![](http://cx4a.org/software/popwin/popwin-find-file.png)

Let me show other examples.

**`M-x occur`**

![](http://cx4a.org/software/popwin/popwin-occur.png)

**`M-x compile`**

![](http://cx4a.org/software/popwin/popwin-compile.png)

Customization
-------------

Please do `M-x customize-group RET popwin RET`. See the header of
`popwin.el`, source code, and docstrings for more information.

Special Display Config
----------------------

When you want to show buffers in a popup window as you like, you need
to write a configuration about `popwin:special-display-config`.

This variable is a list of a form like `(pattern :regexp REGEXP :width
WIDTH :height HEIGHT :position POS :noselect NOSEL :stick
STICK)`. Only `pattern` is necessary and other keywords are
optional. `PATTERN` is string or symbol. If string, it indicates which
buffers should be shown in a popup window. If symbol, it indicates
which buffers of the major mode of the symbol should be shown in a
popup window.

Take an example. If you want to show `*scratch*` buffer, write the following code:

    (setq popwin:special-display-config '(("*scratch*")))

And then display `*scratch*` like:

    (display-buffer "*scratch*")

You may see the buffer at the bottom of the frame.

If you specify `t` to `REGEXP`, you can specify a regexp to `PATTERN`
for matching a buffer name.

If you specify a number to `WIDTH`, the value will be used instead of
`popwin:popup-window-width`. `HEIGHT` and `POS` are same.

If you specify `t` to `NOSEL`, a popup window will not be selected
when it is shown. If you specify `t` to `STICK`, a popup window will be
stuck by default.

Remember that popwin can handle `display-buffer` only. So popwin can't
handle the behaviors like switching buffer. This is NOT a bug but a
feature.

### Examples

#### Anything

Show `*anything*` in a popup window.

    (setq anything-samewindow nil)
    (push '("*anything*" :height 20) popwin:special-display-config)

![](http://cx4a.org/software/popwin/popwin-anything.png)

#### Dired

Show dired buffers in a popup window by `M-x dired-jump-other-window`.

    (push '(dired-mode :position top) popwin:special-display-config)

![](http://cx4a.org/software/popwin/popwin-dired.png)

API
---

Introduce basic API of popwin. See source code for more information.

### Function: `popwin:create-popup-window`

    popwin:create-popup-window &optional size position adjust => (master-window popup-window)

`popwin:create-popup-window` creates a popup window and return it with
a master window. Master window is a window which is splitted when
creating the popup window. A resposibility of closing the popup window
is on developers.

### Function: `popwin:popup-buffer`

    popwin:popup-buffer buffer &key width height position noselect stick => popup-window

`popwin:popup-buffer` displays the buffer in a popup window. The popup
window will be closed automatically. Keywords arguments are same
meanings to an element of `popwin:special-display-config`.

### Function: `popwin:display-buffer`

Same as `popwin:popup-buffer` except `popwin:display-buffer` refers to
`popwin:special-display-config` and uses its configuration. If no
entry is found in `popwin:special-display-config`, the buffer will be
displayed as usual way.

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
