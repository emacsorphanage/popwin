;;; popwin.el --- Popup Window Manager.

;; Copyright (C) 2011  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

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

;; 

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

(defun* popwin:adjust-window-edges (window edges &optional (hfactor 1) (vfactor 1))
  "Adjust edges of WINDOW to EDGES regarding of horizontal factor
HFACTOR and vertical factor VFACTOR."
  (destructuring-bind ((left top right bottom)
                       (cur-left cur-top cur-right cur-bottom))
      (list edges (window-edges window))
    (let ((hdelta (floor (- cur-left (* left hfactor))))
          (vdelta (floor (- cur-top (* top vfactor)))))
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
  "Return `window-tree' with replacing window values in the tree
with persistent representations."
  (destructuring-bind (root mini)
      (window-tree)
    (list (popwin:window-config-tree-1 root) mini)))

(defun popwin:replicate-window-config (window node hfactor vfactor)
  "Replicate NODE of window configuration on WINDOW with
horizontal factor HFACTOR and vertical factor VFACTOR."
  (if (eq (car node) 'window)
      (destructuring-bind (buffer edges selected)
          (cdr node)
        (popwin:adjust-window-edges window edges hfactor vfactor)
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
              (popwin:replicate-window-config window w1 hfactor vfactor)
              (popwin:replicate-window-config new-window w2 hfactor vfactor)
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

(defun* popwin:popup-window (&optional (size 15) (position 'bottom) (adjust t))
  "Create a popup window with SIZE on the frame. If ADJUST is t,
all of windows will be factord to fit the frame. POSITION must be
one of (left top right bottom). The return value is the popup
window. To close the popup window properly, get
`current-window-configuration' before calling this function, and
call `set-window-configuration' with the window-configuration for
closing."
  (let* ((root-win (popwin:last-selected-window))
         (root (car (popwin:window-config-tree))))
    (delete-other-windows root-win)
    (let ((horizontal (popwin:position-horizontal-p position))
          (root-edges (window-edges root-win))
          root-size master-size hfactor vfactor)
      (when horizontal
        (setq root-size (- (nth 0 root-edges) (nth 2 root-edges)))
        (setq master-size (- root-size (min root-size size)))
        (setq hfactor adjust vfactor 1)
        (if (eq hfactor t)
            (setq hfactor (/ (float master-size) root-size))))
      (unless horizontal
        (setq root-size (- (nth 3 root-edges) (nth 1 root-edges)))
        (setq master-size (- root-size (min root-size size)))
        (setq hfactor 1 vfactor adjust)
        (if (eq vfactor t)
            (setq vfactor (/ (float master-size) root-size))))
      (let ((win (split-window root-win master-size horizontal)))
        (popwin:replicate-window-config root-win root hfactor vfactor)
        win))))



;;; Common User Interface

(defcustom popwin:popup-window-position 'bottom
  "Default popup window position. This must be one of (left top right
bottom)."
  :type 'symbol
  :group 'popwin)

(defcustom popwin:popup-window-width 15
  "Default popup window width. If `popwin:popup-window-position'
is top or bottom, this configuration will be ignored."
  :type 'integer
  :group 'popwin)

(defcustom popwin:popup-window-height 15
  "Default popup window height. If `popwin:popup-window-position'
is left or right, this configuration will be ignored."
  :type 'integer
  :group 'popwin)

(defcustom popwin:adjust-other-windows t
  "Non-nil means all of other windows will be adjusted to fit the
frame when a popup window is shown."
  :type 'boolean
  :group 'popwin)

(defvar popwin:popup-window nil
  "Main popup window instance.")

(defvar popwin:popup-buffer nil
  "Buffer of lastly shown in the popup window.")

(defvar popwin:focus-window nil
  "Focused window which is used to check whether or not to close
the popup window.")

(defvar popwin:window-outline nil
  "Original window outline which is obtained by
`popwin:window-config-tree'.")

(defvar popwin:close-popup-window-timer nil
  "Timer of closing the popup window.")

(defvar popwin:close-popup-window-timer-interval 0.01
  "Interval of `popwin:close-popup-window-timer'.")

(defun popwin:popup-window-live-p ()
  "Return t if `popwin:popup-window' is alive."
  (and popwin:popup-window
       (window-live-p popwin:popup-window)))

(defun popwin:should-close-popup-window-p ()
  "Return t if popwin should close the popup window
immediately. It might be useful if this is customizable
function."
  (and popwin:popup-window
       (or (eq last-command 'keyboard-quit)
           (popwin:buried-buffer-p popwin:popup-buffer))))

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

(defun popwin:close-popup-window ()
  "Close the popup window and restore to the previous window
configuration."
  (when popwin:popup-window
    (popwin:stop-close-popup-window-timer)
    (if (window-live-p popwin:popup-window)
        (delete-window popwin:popup-window))
    (popwin:restore-window-outline (car (window-tree))
                                   popwin:window-outline)
    (setq popwin:popup-buffer nil
          popwin:popup-window nil
          popwin:focus-window nil
          popwin:window-outline nil)))

(defun popwin:close-popup-window-if-necessary (&optional force)
  "Close the popup window if another window has been selected. If
FORCE is non-nil, this function tries to close the popup window
immediately if possible."
  (if (and popwin:popup-window
           (not (minibufferp))
           (or force
               (not (eq (selected-window) popwin:focus-window))))
      (popwin:close-popup-window)))

(defun popwin:popup-buffer (buffer &optional no-focus)
  "Show BUFFER in a popup window and return the popup window. If
NO-FOCUS is non-nil, the popup window will not be
selected. Calling `popwin:popup-buffer' during
`popwin:popup-buffer' is allowed. In that case, the buffer of the
popup window will be replaced with BUFFER."
  (unless (popwin:popup-window-live-p)
    (let* ((win-outline (car (popwin:window-config-tree)))
           (win (popwin:popup-window popwin:popup-window-height
                                     popwin:popup-window-position
                                     popwin:adjust-other-windows)))
      (setq popwin:popup-window win
            popwin:window-outline win-outline)
      (popwin:start-close-popup-window-timer)))
  (setq popwin:popup-buffer buffer
        popwin:focus-window (if no-focus
                                (selected-window)
                              popwin:popup-window))
  (with-selected-window popwin:popup-window
    (switch-to-buffer buffer))
  (select-window popwin:focus-window)
  popwin:popup-window)



;;; Special Display

(defcustom popwin:special-display-no-focus-buffer-names
  '("*Completions*" "*compilation*" "*Occur*" "*vc-diff*")
  "A list of buffer names where the windows of buffers should not
be selected when displaying the buffer by using
`special-display-function'."
  :type '(repeat string)
  :group 'popwin)

(defun popwin:special-display-popup-window (buffer &rest ignore)
  "The `special-display-function' with a popup window."
  (if (popwin:popup-window-live-p)
      ;; call original display-function
      (let (special-display-function)
        (display-buffer buffer))
    (let* ((name (buffer-name buffer))
           (no-focus (or (minibufferp)
                         (member name popwin:special-display-no-focus-buffer-names))))
      (popwin:popup-buffer buffer no-focus))))

(provide 'popwin)
;;; popwin.el ends here
