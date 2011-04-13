;;; popwin.el --- Popup Window Manager.

;; Copyright (C) 2011  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Popwin makes you free from the hell of annoying buffers such as
;; *Help*, *Completions*, *compilation*, etc.
;;
;; The recommended way to use popwin is to add the following code
;; to your .emacs:
;;
;;     (require 'popwin)
;;     (setq display-buffer-function 'popwin:display-buffer)
;;
;; Then try to show a buffer, for example *Help* or *Completions*.
;; Instead of following standard behavior, the buffer should appear as
;; a transient popup window, most likely at the bottom of the
;; frame. You can close the popup window directly by typing C-g or
;; implicitly by selecting a different window.
;;
;; The function `popwin:display-buffer' displays special buffers in a
;; popup window and displays normal buffers as usual. Special buffers
;; and their manner of display are specified in the variable
;; `popwin:special-display-config'. See the docstring of
;; `popwin:special-display-config' for more information.
;;
;; Instead of the recommended method above, you can also use popwin
;; by setting `special-display-function' like this
;;
;;     (require 'popwin)
;;     (setq special-display-function
;;           'popwin:special-display-popup-window)
;;
;; When using this method, you also need to change
;; `special-display-buffer-names' or `special-display-regexps' so that
;; popwin knows to take care of such buffers.
;;
;; The default width/height/position of the popup window can be
;; changed by setting `popwin:popup-window-width',
;; `popwin:popup-window-height', and `popwin:popup-window-position'.
;; You can also change the behavior for a specific buffer. See the
;; docstring of `popwin:special-display-config'.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup popwin nil
  "Popup Window Manager."
  :group 'convenience
  :prefix "popwin:")



;;; Common API

(defun popwin:last-selected-window ()
  "Return the currently selected window, or most-recently
selected window if the minibuffer window is selected."
  (if (minibufferp)
      (minibuffer-selected-window)
    (selected-window)))

(defun popwin:buried-buffer-p (buffer)
  "Return t if BUFFER might be thought of as a buried buffer."
  (eq (car (last (buffer-list))) buffer))

(defun* popwin:adjust-window-edges (window edges
                                           &optional
                                           (offset '(0 0))
                                           (hfactor 1)
                                           (vfactor 1))
  "Adjust the edges of WINDOW to EDGES according to OFFSET, horizontal
factor HFACTOR, and vertical factor VFACTOR."
  (destructuring-bind ((left top right bottom)
                       (cur-left cur-top cur-right cur-bottom)
                       (left-offset top-offset))
      (list edges (window-edges window) offset)
    (let ((hdelta (floor (- cur-left (* left hfactor) left-offset)))
          (vdelta (floor (- cur-top (* top vfactor) top-offset))))
      (with-selected-window window
        (if (/= hdelta 0)
            (enlarge-window hdelta t))
        (if (/= vdelta 0)
            (enlarge-window vdelta))))))

(defun popwin:window-config-tree-1 (node)
  (if (windowp node)
      (list 'window
            (window-buffer node)
            (window-edges node)
            (eq (selected-window) node))
    (destructuring-bind (dir edges . windows) node
      (append (list dir edges)
              (mapcar 'popwin:window-config-tree-1 windows)))))

(defun popwin:window-config-tree ()
  "Return `window-tree', replacing window values in the tree
with persistent representations."
  (destructuring-bind (root mini)
      (window-tree)
    (list (popwin:window-config-tree-1 root) mini)))

(defun popwin:replicate-window-config (window node offset hfactor vfactor)
  "Replicate NODE of window configuration on WINDOW with OFFSET,
horizontal factor HFACTOR, and vertical factor VFACTOR."
  (if (eq (car node) 'window)
      (destructuring-bind (buffer edges selected)
          (cdr node)
        (popwin:adjust-window-edges window edges offset hfactor vfactor)
        (with-selected-window window
          (switch-to-buffer buffer))
        (if selected
            (select-window window)))
    (destructuring-bind (dir edges . windows) node
      (loop while windows
            for w1 = (pop windows) then w2
            for w2 = (pop windows)
            do
            (let ((new-window (split-window window nil (not dir))))
              (popwin:replicate-window-config window w1 offset hfactor vfactor)
              (popwin:replicate-window-config new-window w2 offset hfactor vfactor)
              (setq window new-window))))))

(defun popwin:restore-window-outline (node outline)
  "Restore window outline according to the structures of NODE
which is a node of `window-tree', and OUTLINE which is a node
of `popwin:window-config-tree'."
  (cond
   ((and (windowp node)
         (eq (car outline) 'window))
    ;; same window
    (let ((edges (nth 2 outline)))
      (popwin:adjust-window-edges node edges)))
   ((or (windowp node)
        (not (eq (car node) (car outline))))
    ;; different structure
    ;; nothing to do
    )
   (t
    (let ((child-nodes (cddr node))
          (child-outlines (cddr outline)))
      (if (eq (length child-nodes) (length child-outlines))
          ;; same structure
          (loop for child-node in child-nodes
                for child-outline in child-outlines
                do (popwin:restore-window-outline child-node child-outline)))))))

(defun popwin:position-horizontal-p (position)
  "Return t if POSITION is horizontal."
  (memq position '(left right)))

(defun popwin:position-vertical-p (position)
  "Return t if POSITION is vertical."
  (memq position '(top bottom)))

(defun popwin:create-popup-window-1 (window size position)
  "Create a new window with SIZE at POSITION of WINDOW. The
return value is a list of a master window, the popup window,
and offsets of the master window in the form (left-offset
top-offset)."
  (let ((width (window-width window))
        (height (window-height window)))
    (ecase position
      (left   (list (split-window window size t)
                    window
                    (list size 0)))
      (top    (list (split-window window size nil)
                    window
                    (list 0 size)))
      (right  (list window
                    (split-window window (- width size) t)
                    (list 0 0)))
      (bottom (list window
                    (split-window window (- height size) nil)
                    (list 0 0))))))

(defun* popwin:create-popup-window (&optional (size 15) (position 'bottom) (adjust t))
  "Create a popup window with SIZE on the current frame.  If SIZE
is an integer, the size of the popup window will be SIZE.  If
SIZE is a floating point number, the size of the popup window
will be the product of SIZE and frame-size.  POSITION must be one
of (left top right bottom).  If ADJUST is non-nil, all windows
will be adjusted to fit the frame.  The return value is a list
containing the master window and the popup window.  To close the
popup window properly, get `current-window-configuration' before
calling this function, and call `set-window-configuration' with
the window-configuration."
  (let* ((root (car (popwin:window-config-tree)))
         (root-win (popwin:last-selected-window))
         (hfactor 1)
         (vfactor 1))
    (delete-other-windows root-win)
    (let ((root-width (window-width root-win))
          (root-height (window-height root-win)))
      (when adjust
        (if (floatp size)
            (if (popwin:position-horizontal-p position)
                (setq hfactor size
                      size (round (* root-width size)))
              (setq vfactor size
                    size (round (* root-height size))))
          (if (popwin:position-horizontal-p position)
              (setq hfactor (/ (float (- root-width size)) root-width))
            (setq vfactor (/ (float (- root-height size)) root-height)))))
      (destructuring-bind (master-win popup-win offset)
          (popwin:create-popup-window-1 root-win size position)
        (popwin:replicate-window-config master-win root offset hfactor vfactor)
        (list master-win popup-win)))))



;;; Common User Interface

(defcustom popwin:popup-window-position 'bottom
  "Default popup window position. This must be one of (left top right
bottom)."
  :type 'symbol
  :group 'popwin)

(defcustom popwin:popup-window-width 30
  "Default popup window width. If `popwin:popup-window-position'
is top or bottom, this setting will be ignored. If this value is
a floating point number, the popup window width will be the
product of this value and the width of the frame."
  :type 'number
  :group 'popwin)

(defcustom popwin:popup-window-height 15
  "Default popup window height. If `popwin:popup-window-position'
is left or right, this setting will be ignored. If this value is
a floating point number, the popup window height will be the
product of this value and the height of the frame."
  :type 'number
  :group 'popwin)

(defcustom popwin:adjust-other-windows t
  "Non-nil means all other windows will be adjusted to fit the
frame when a popup window is shown."
  :type 'boolean
  :group 'popwin)

(defvar popwin:popup-window nil
  "Main popup window instance.")

(defvar popwin:popup-buffer nil
  "Buffer currently shown in the popup window.")

(defvar popwin:master-window nil
  "Master window of a popup window.")

(defvar popwin:focus-window nil
  "Focused window which is used to check whether or not to close
the popup window.")

(defvar popwin:selected-window nil
  "Most-recently selected window at the time when a popup window is shown.")

(defvar popwin:popup-window-stuck-p nil
  "Non-nil means the popup window is sticky.")

(defvar popwin:popup-window-keyb-p nil
  "Non-nil means clear pending keyboard commands when closing a popup window.")

(defvar popwin:window-outline nil
  "Original window outline which is obtained by
`popwin:window-config-tree'.")

(defvar popwin:close-popup-window-timer nil
  "Timer for closing the popup window.")

(defvar popwin:close-popup-window-timer-interval 0.01
  "Interval of `popwin:close-popup-window-timer'.")

(defun popwin:popup-window-live-p ()
  "Returns t if `popwin:popup-window' is alive."
  (window-live-p popwin:popup-window))

(defun popwin:start-close-popup-window-timer ()
  (or popwin:close-popup-window-timer
      (setq popwin:close-popup-window-timer
            (run-with-timer popwin:close-popup-window-timer-interval
                            popwin:close-popup-window-timer-interval
                            'popwin:close-popup-window-timer))))

(defun popwin:stop-close-popup-window-timer ()
  (when popwin:close-popup-window-timer
    (cancel-timer popwin:close-popup-window-timer)
    (setq popwin:close-popup-window-timer nil)))

(defun popwin:close-popup-window-timer ()
  (condition-case var
      (popwin:close-popup-window-if-necessary
       (popwin:should-close-popup-window-p))
    (error (message "popwin:close-popup-window-timer: error: %s" var))))

(defun popwin:close-popup-window (&optional keep-selected)
  "Close the popup window and restore to the previous window
configuration. If KEEP-SELECTED is non-nil, the most-recently
selected window will not be selected."
  (interactive)
  (unwind-protect
      (when popwin:popup-window
        (popwin:stop-close-popup-window-timer)
	(if popwin:popup-window-keyb-p
	    (call-interactively 'popwin:keyboard-escape))
        (if (and (popwin:popup-window-live-p)
                 (window-live-p popwin:master-window))
            (delete-window popwin:popup-window))
        (popwin:restore-window-outline (car (window-tree))
                                       popwin:window-outline)
        (if (and (not keep-selected)
                 (window-live-p popwin:selected-window))
            (select-window popwin:selected-window)))
    (setq popwin:popup-buffer nil
          popwin:popup-window nil
          popwin:focus-window nil
          popwin:selected-window nil
          popwin:popup-window-stuck-p nil
          popwin:popup-window-keyb-p nil
          popwin:window-outline nil)))

(defun popwin:should-close-popup-window-p ()
  "Return t if popwin should close the popup window
immediately. It might be useful for this to be a customizable
function."
  (and popwin:popup-window
       (or (and (eq last-command 'keyboard-quit)
                (eq last-command-event ?\C-g))
           (not (buffer-live-p popwin:popup-buffer))
           (popwin:buried-buffer-p popwin:popup-buffer))))

(defun popwin:close-popup-window-if-necessary (&optional force)
  "Close the popup window if a different window has been
selected. If FORCE is non-nil, this function tries to close the
popup window immediately and return selection to the
most-recently selected window."
  (when popwin:popup-window
    (let* ((window (selected-window))
           (minibuf-window-p (eq window (minibuffer-window)))
           (other-window-selected
            (and (not (eq window popwin:focus-window))
                 (not (eq window popwin:popup-window))))
           (popup-buffer-still-working
            (and (buffer-live-p popwin:popup-buffer)
                 (not (popwin:buried-buffer-p popwin:popup-buffer))))
           (not-stuck-or-closed
            (or (not popwin:popup-window-stuck-p)
                (not (popwin:popup-window-live-p)))))
      (if (and (not minibuf-window-p)
               (or force
                   (and not-stuck-or-closed
                        other-window-selected)))
          (popwin:close-popup-window
           (and other-window-selected
                popup-buffer-still-working))))))

(defun* popwin:popup-buffer (buffer
                             &key
                             (width popwin:popup-window-width)
                             (height popwin:popup-window-height)
                             (position popwin:popup-window-position)
                             noselect
                             stick
                             keyb)
  "Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be sticky. If KEYB is
non-nil, popwin:keyboard-escape will be called at close of the
popup, clearing pending actions in the minibuffer. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER."
  (interactive "BPopup buffer:\n")
  (setq buffer (get-buffer buffer))
  (unless (popwin:popup-window-live-p)
    (let ((win-outline (car (popwin:window-config-tree))))
      (destructuring-bind (master-win popup-win)
          (let ((size (if (popwin:position-horizontal-p position) width height))
                (adjust popwin:adjust-other-windows))
            (popwin:create-popup-window size position adjust))
        (setq popwin:popup-window popup-win
              popwin:master-window master-win
              popwin:window-outline win-outline)
        (popwin:start-close-popup-window-timer))))
  (setq popwin:popup-buffer buffer
        popwin:selected-window (selected-window)
        popwin:focus-window (if noselect
                                popwin:selected-window
                              popwin:popup-window)
        popwin:popup-window-stuck-p stick
        popwin:popup-window-keyb-p keyb)
  (with-selected-window popwin:popup-window
    (switch-to-buffer buffer))
  (select-window popwin:focus-window)
  popwin:popup-window)

(defun popwin:select-popup-window ()
  "Select the currently shown popup window."
  (interactive)
  (if (popwin:popup-window-live-p)
      (select-window popwin:popup-window)
    (error "No popup window displayed")))

(defun popwin:stick-popup-window ()
  "Make the currently shown popup window sticky. The sticky popup
window can still be closed by `popwin:close-popup-window', but
will not be closed implicitly by actions such as selecting a
different window."
  (interactive)
  (if (popwin:popup-window-live-p)
      (setq popwin:popup-window-stuck-p t)
    (error "No popup window displayed")))

(defun popwin:keyboard-escape ()
  "A limited version of keyboard-escape from simple.el.
Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))))



;;; Special Display

(defcustom popwin:special-display-config
  '(("*Help*")
    ("*Completions*" :noselect t)
    ("*compilation*" :noselect t)
    ("*Occur*" :noselect t))
  "Configuration of special display buffers for
`popwin:display-buffer' and
`popwin:special-display-popup-window'. The value is a list
of (PATTERN . KEYWORDS) where PATTERN is a pattern specifying the
buffer and KEYWORDS is a list of key/value pairs. PATTERN is a
buffer name (or regexp, see below), but may also be a symbol
specifying the major-mode of a buffer. Keywords are as follows:

  regexp: If the paired value is non-nil, PATTERN will be used as
    a regexp to match the buffer name.

  width, height: Specify the width or height of the popup
    window. If no size is specified, `popwin:popup-window-width'
    or `popwin:popup-window-height' will be used. See also
    the position keyword.

  position: The value must be one of (left top right bottom). The
    popup window will shown at the given position within the
    frame.  If no position is specified,
    `popwin:popup-window-position' will be used.

  noselect: If the value is non-nil, the popup window will not be
    selected when it is shown.

  stick: If the value is non-nil, the popup window will be sticky,
    and must be closed explicitly.

  keyb: If the value is non-nil, popwin:keyboard-escape will be
    called at the close of the popup window, clearing pending
    keyboard actions.

Examples: With '(\"*scratch*\" :height 30 :position top), the
*scratch* buffer will be shown at the top of the frame with
height 30. With '(dired-mode :width 80 :position left), dired
buffers will be shown at the left of the frame with width 80."
  :group 'popwin)

(defvar popwin:last-display-buffer nil
  "The most-recently displayed buffer created by popwin.")

(defun popwin:original-display-buffer (buffer &optional not-this-window)
  "Call `display-buffer' for BUFFER without special display."
  (let (display-buffer-function special-display-function)
    ;; Close the popup window here so that the popup window won't to
    ;; be splitted.
    (if (eq (selected-window) popwin:popup-window)
        (popwin:close-popup-window))
    (display-buffer buffer not-this-window)))

(defun* popwin:display-buffer-1 (buffer-or-name &key default-config-keywords if-buffer-not-found if-config-not-found)
  "Display BUFFER-OR-NAME, if possible, in a popup
window. Otherwise call IF-CONFIG-NOT-FOUND with BUFFER-OR-NAME if
it is non-nil. If IF-CONFIG-NOT-FOUND is nil, `display-buffer'
will be called with `special-display-function' nil. If
IF-BUFFER-NOT-FOUND is :create, create a buffer named
BUFFER-OR-NAME if there is not such a buffer
already. DEFAULT-CONFIG-KEYWORDS is a property list which
specifies default values of the selected config."
  (loop with buffer = (if (eq if-buffer-not-found :create)
                          (get-buffer-create buffer-or-name)
                        (get-buffer buffer-or-name))
        with name = (buffer-name buffer)
        with mode = (with-current-buffer buffer major-mode)
        with win-width = popwin:popup-window-width
        with win-height = popwin:popup-window-height
        with win-position = popwin:popup-window-position
        with win-noselect
        with win-stick
        with win-keyb
        with found
        until found
        for (pattern . keywords) in popwin:special-display-config do
        (destructuring-bind (&key regexp width height position noselect stick keyb)
            (append keywords default-config-keywords)
          (let ((matched
                 (cond
                  ((and (stringp pattern) regexp)
                   (string-match pattern name))
                  ((stringp pattern)
                   (string= pattern name))
                  ((symbolp pattern)
                   (eq pattern mode))
                  (t (error "Invalid pattern: %s" pattern)))))
            (if matched
                (setq found t
                      win-width (or width win-width)
                      win-height (or height win-height)
                      win-position (or position win-position)
                      win-noselect noselect
                      win-stick stick
                      win-keyb keyb))))
        finally return
        (if (or found
                (null if-config-not-found))
            (progn
              (setq popwin:last-display-buffer buffer)
              (popwin:popup-buffer buffer
                                   :width win-width
                                   :height win-height
                                   :position win-position
                                   :noselect (or (minibufferp) win-noselect)
                                   :stick win-stick
                                   :keyb win-keyb))
          (funcall if-config-not-found buffer))))

(defun popwin:display-buffer (buffer-or-name &optional not-this-window)
  "Display BUFFER-OR-NAME in a popup window if possible,
otherwise display as usual. This function can be used as a value
of `display-buffer-function'."
  (interactive "BDisplay buffer:\n")
  (popwin:display-buffer-1
   buffer-or-name
   :if-config-not-found
   (unless (interactive-p)
     (lambda (buffer-or-name)
       (popwin:original-display-buffer buffer-or-name not-this-window)))))

(defun popwin:special-display-popup-window (buffer &rest ignore)
  "The `special-display-function' for a popup window."
  (popwin:display-buffer-1 buffer))

(defun popwin:display-last-buffer ()
  "Select the most-recently displayed buffer created by
`popwin:display-buffer' or`popwin:special-display-popup-window'."
  (interactive)
  (if (bufferp popwin:last-display-buffer)
      (popwin:display-buffer-1 popwin:last-display-buffer)
    (error "No popup window displayed")))



;;; Extensions

(defun popwin:popup-buffer-tail (&rest same-as-popwin:popup-buffer)
  "Same as `popwin:popup-buffer' except that the buffer will be
`recenter'ed at the bottom."
  (interactive "bPopup buffer:\n")
  (let ((popup-win (apply 'popwin:popup-buffer same-as-popwin:popup-buffer)))
    (set-window-point popup-win (point-max))
    (recenter -2)
    popup-win))

(defun popwin:find-file (filename &optional wildcards)
  "Edit file FILENAME in a popup window created by
`popwin:popup-buffer'."
  (interactive
   (find-file-read-args "Find file in popup window: "
                        (if (fboundp 'confirm-nonexistent-file-or-buffer)
                            (confirm-nonexistent-file-or-buffer))))
  (popwin:popup-buffer (find-file-noselect filename wildcards)))

(defun popwin:find-file-tail (file &optional wildcard)
  "Edit file FILENAME in a popup window create by
`popwin:popup-buffer-tail'."
  (interactive
   (find-file-read-args "Find file in popup window: "
                        (if (fboundp 'confirm-nonexistent-file-or-buffer)
                            (confirm-nonexistent-file-or-buffer))))
  (popwin:popup-buffer-tail (find-file-noselect file wildcard)))

(defun popwin:messages ()
  "Display the *Messages* buffer in a popup window."
  (interactive)
  (popwin:popup-buffer-tail "*Messages*"))



;;; Keymaps

(defvar popwin:keymap
  (let ((map (make-keymap)))
    (define-key map "b" 'popwin:popup-buffer)
    (define-key map "\C-b" 'popwin:popup-buffer)
    (define-key map "\M-b" 'popwin:popup-buffer-tail)
    (define-key map "o" 'popwin:display-buffer)
    (define-key map "\C-o" 'popwin:display-buffer)
    (define-key map "p" 'popwin:display-last-buffer)
    (define-key map "\C-p" 'popwin:display-last-buffer)
    (define-key map "f" 'popwin:find-file)
    (define-key map "\C-f" 'popwin:find-file)
    (define-key map "\M-f" 'popwin:find-file-tail)
    (define-key map "s" 'popwin:select-popup-window)
    (define-key map "\C-s" 'popwin:select-popup-window)
    (define-key map "\M-s" 'popwin:stick-popup-window)
    (define-key map "0" 'popwin:close-popup-window)
    (define-key map "m" 'popwin:messages)
    (define-key map "\C-m" 'popwin:messages)
    map)
  "Default keymap for popwin commands. Example setup:
\(global-set-key (kbd \"C-x C-p\") 'popwin:keymap\)")

(provide 'popwin)
;;; popwin.el ends here
