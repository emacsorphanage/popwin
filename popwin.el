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

;; Popwin makes you free from the hell of annoying buffers such like
;; *Help*, *Completions*, *compilation*, and etc.
;; 
;; To use popwin, just add the following code into your .emacs:
;; 
;;     (require 'popwin)
;;     (setq display-buffer-function 'popwin:display-buffer)
;; 
;; Then try to show some buffer, for example *Help* or
;; *Completeions*. Unlike standard behavior, their buffers may be
;; shown in a popup window at the bottom of the frame. And you can
;; close the popup window seamlessly by typing C-g or selecting other
;; windows.
;;
;; `popwin:display-buffer' displays special buffers in a popup window
;; and displays normal buffers as unsual. Special buffers are
;; specified in `popwin:special-display-config', which tells popwin
;; how to display such buffers. See docstring of
;; `popwin:special-display-config' for more information.
;;
;; Instead of a recommended way, you can also use popwin by setting
;; `special-display-function' like:
;;
;;     (require 'popwin)
;;     (setq special-display-function
;;           'popwin:special-display-popup-window)
;;
;; In this case, you need to change `special-display-buffer-names' or
;; `special-display-regexps' so that popwin takes care of such
;; buffers.
;; 
;; The default width/height/position of popup window can be changed by
;; setting `popwin:popup-window-width', `popwin:popup-window-height',
;; and `popwin:popup-window-position'.  You can also change the
;; behavior for a specific buffer. See docstring of
;; `popwin:special-display-config'.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup popwin nil
  "Popup Window Manager."
  :group 'convenience
  :prefix "popwin:")



;;; Common API

(defun popwin:last-selected-window ()
  "Return currently selected window or lastly selected window if
minibuffer window is selected."
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
  "Adjust edges of WINDOW to EDGES accoring to OFFSET, horizontal
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
              (mapcar #'popwin:window-config-tree-1 windows)))))

(defun popwin:window-config-tree ()
  "Return `window-tree' with replacing window values in the tree
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
  "Restore window outline accoding to the structures of NODE
which is a node of `window-tree' and OUTLINE which is a node of
`popwin:window-config-tree'."
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
  "Return t if POSITION is hozirontal."
  (memq position '(left right)))

(defun popwin:position-vertical-p (position)
  "Return t if POSITION is vertical."
  (memq position '(top bottom)))

(defun popwin:create-popup-window-1 (window size position)
  "Create a new window with SIZE at POSITION of WINDOW. The
return value is a list of a master window, the popup window,
offsets of the master window in a form of (left-offset
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
  "Create a popup window with SIZE on the frame.  If SIZE
isinteger, the size of the popup window will be SIZE. If SIZE is
float, the size of popup window will be a multiplier of SIZE and
frame-size. can be an integer and a float. If ADJUST is t, all of
windows will be adjusted to fit the frame. POSITION must be one
of (left top right bottom). The return value is a pair of a
master window and the popup window. To close the popup window
properly, get `current-window-configuration' before calling this
function, and call `set-window-configuration' with the
window-configuration."
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
is top or bottom, this configuration will be ignored. If this
variable is float, the popup window width will be a multiplier of
the value and frame-size."
  :type 'number
  :group 'popwin)

(defcustom popwin:popup-window-height 15
  "Default popup window height. If `popwin:popup-window-position'
is left or right, this configuration will be ignored. If this
variable is float, the popup window height will be a multiplier
of the value and frame-size."
  :type 'number
  :group 'popwin)

(defcustom popwin:adjust-other-windows t
  "Non-nil means all of other windows will be adjusted to fit the
frame when a popup window is shown."
  :type 'boolean
  :group 'popwin)

(defvar popwin:popup-window nil
  "Main popup window instance.")

(defvar popwin:popup-buffer nil
  "Buffer of currently shown in the popup window.")

(defvar popwin:master-window nil
  "Master window of a popup window.")

(defvar popwin:focus-window nil
  "Focused window which is used to check whether or not to close
the popup window.")

(defvar popwin:selected-window nil
  "Last selected window when the popup window is shown.")

(defvar popwin:popup-window-stuck-p nil
  "Non-nil means the popup window has been stuck.")

(defvar popwin:window-outline nil
  "Original window outline which is obtained by
`popwin:window-config-tree'.")

(defvar popwin:close-popup-window-timer nil
  "Timer of closing the popup window.")

(defvar popwin:close-popup-window-timer-interval 0.01
  "Interval of `popwin:close-popup-window-timer'.")

(defun popwin:popup-window-live-p ()
  "Return t if `popwin:popup-window' is alive."
  (window-live-p popwin:popup-window))

(defun popwin:start-close-popup-window-timer ()
  (or popwin:close-popup-window-timer
      (setq popwin:close-popup-window-timer
            (run-with-timer popwin:close-popup-window-timer-interval
                            popwin:close-popup-window-timer-interval
                            #'popwin:close-popup-window-timer))))

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
configuration. If KEEP-SELECTED is non-nil, the lastly selected
window will not be selected."
  (interactive)
  (unwind-protect
      (when popwin:popup-window
        (popwin:stop-close-popup-window-timer)
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
          popwin:window-outline nil)))

(defun popwin:should-close-popup-window-p ()
  "Return t if popwin should close the popup window
immediately. It might be useful if this is customizable
function."
  (and popwin:popup-window
       (or (eq last-command-event ?\C-g)
           (popwin:buried-buffer-p popwin:popup-buffer))))

(defun popwin:close-popup-window-if-necessary (&optional force)
  "Close the popup window if another window has been selected. If
FORCE is non-nil, this function tries to close the popup window
immediately if possible, and the lastly selected window will be
selected again."
  (when popwin:popup-window
    (let* ((window (selected-window))
           (minibuf-window-p (eq window (minibuffer-window)))
           (other-window-selected
            (and (not (eq window popwin:focus-window))
                 (not (eq window popwin:popup-window))))
           (not-stuck-or-closed
            (or (not popwin:popup-window-stuck-p)
                (not (popwin:popup-window-live-p)))))
      (if (and (not minibuf-window-p)
               (or force
                   (and not-stuck-or-closed
                        other-window-selected)))
          (popwin:close-popup-window
           other-window-selected)))))

(defun* popwin:popup-buffer (buffer
                             &key
                             (width popwin:popup-window-width)
                             (height popwin:popup-window-height)
                             (position popwin:popup-window-position)
                             noselect
                             stick)
  "Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER."
  (interactive "BPopup buffer:\n")
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
        popwin:popup-window-stuck-p stick)
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
  "Stick the currently shown popup window. The popup window can
be cloesd by `popwin:close-popup-window'."
  (interactive)
  (if (popwin:popup-window-live-p)
      (setq popwin:popup-window-stuck-p t)
    (error "No popup window displayed")))



;;; Special Display

(defcustom popwin:special-display-config
  '(("*Help*")
    ("*Completions*" :noselect t)
    ("*compilation*" :noselect t)
    ("*Occur*" :noselect t))
  "Configuration of special displaying buffer for
`popwin:display-buffer' and
`popwin:special-display-popup-window'. The value is a list
of (PATTERN . KEYWORDS) where PATTERN is a pattern of specifying
buffer and KEYWORDS is a list of a pair of key and value. PATTERN
is in general a buffer name, otherwise a symbol specifying
major-mode of buffer. Available keyword are following:

  regexp: If the value is non-nil, PATTERN will be used as regexp
    to matching buffer.

  width, height: Specify width or height of the popup window. If
    no size specified, `popwin:popup-window-width' or
    `popwin:popup-window-height' will be used. See also position
    keyword.

  position: The value must be one of (left top right bottom). The
    popup window will shown at the position of the frame.  If no
    position specified, `popwin:popup-window-position' will be
    used.

  noselect: If the value is non-nil, the popup window will not be
    selected when it is shown.

  stick: If the value is non-nil, the popup window will be stuck
    when it is shown.

Examples: With '(\"*scratch*\" :height 30 :position top),
*scratch* buffer will be shown at the top of the frame with
height 30. With '(dired-mode :width 80 :position left), dired
buffers will be shown at the left of the frame with width 80."
  :group 'popwin)

(defvar popwin:last-display-buffer nil
  "The lastly displayed buffer.")

(defun popwin:original-display-buffer (buffer &optional not-this-window)
  "Call `display-buffer' for BUFFER without special displaying."
  (let (display-buffer-function special-display-function)
    (popwin:close-popup-window)
    (display-buffer buffer not-this-window)))

(defun* popwin:display-buffer-1 (buffer &key if-config-not-found)
  (loop with buffer = (get-buffer buffer)
        with name = (buffer-name buffer)
        with mode = (with-current-buffer buffer major-mode)
        with win-width = popwin:popup-window-width
        with win-height = popwin:popup-window-height
        with win-position = popwin:popup-window-position
        with win-noselect
        with win-stick
        with found
        until found
        for (pattern . keywords) in popwin:special-display-config do
        (destructuring-bind (&key regexp width height position noselect stick)
            keywords
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
                      win-stick stick))))
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
                                   :stick win-stick))
          (funcall if-config-not-found buffer))))

(defun popwin:display-buffer (buffer &optional not-this-window)
  "Display BUFFER, if possible, in a popup window, or as
usual. This function can be used as a value of
`display-buffer-function'."
  (interactive "BDisplay buffer:\n")
  (popwin:display-buffer-1
   buffer
   :if-config-not-found
   (unless (interactive-p)
     (lambda (buffer)
       (popwin:original-display-buffer buffer not-this-window)))))

(defun popwin:special-display-popup-window (buffer &rest ignore)
  "The `special-display-function' with a popup window."
  (popwin:display-buffer-1 buffer))

(defun popwin:display-last-buffer ()
  "Display the lastly shown buffer by `popwin:display-buffer' and
`popwin:special-display-popup-window'."
  (interactive)
  (if (bufferp popwin:last-display-buffer)
      (popwin:display-buffer-1 popwin:last-display-buffer)
    (error "No popup window displayed")))

(provide 'popwin)
;;; popwin.el ends here
