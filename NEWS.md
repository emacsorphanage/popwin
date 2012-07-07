v0.4
====

New Features
------------

- GNU Emacs 24 support
- Dedicated popup windows
- Stacked popup windows
- Universal displaying

New Commands
------------

- `popwin:pop-to-buffer`

New Variables
-------------

- `popwin:before-popup-hook`
- `popwin:after-popup-book`

Fixed Bugs
----------

- Keep minibuffer window selected when closing popup windows
- Restore `window-point` when closing popup windows
- Fixed corrupted popup windows in some situations

Compatibilities
---------------

- Added `misc/popwin-pp.el`
- Added `misc/popwin-term.el`
- Added `misc/popwin-browse-kill-ring.el`
- Deprecated `popwin:special-display-buffer`

Contributors
------------

- IRIE Shinsuke
- rubikitch
