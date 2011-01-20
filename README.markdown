popwin.el
=========

popwin is popup window manager for Emacs which makes you free from the
hell of annoying buffers such like `*Help*`, `*Completions*`,
`*compilation*`, and etc.

Installation
------------

Install `popwin.el` and add the following code into your `.emacs`:

    (setq display-buffer-function 'popwin:display-buffer)

There is an alternative way using `special-display-function` like:

    (setq special-display-function 'popwin:special-display-popup-window)

In this case, you need to change `special-display-buffer-names` or
`special-display-regexps` so that popwin takes care of such buffers.

popwin is tested under GNU Emacs 22 or later.

Usage
-----

Special buffers, for example `*Help*`, specified in
`popwin:special-display-config` will be shown in a popup window. You
can close the popup window by typing `C-g` or selecting other windows.

Customization
-------------

Please do `M-x customize-group RET popwin RET`. See the header of
`popwin.el`, source code, and docstring for more information.

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
