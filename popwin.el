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



;; Common API

(defun popwin:last-selected-window ()
  "Return currently selected window or lastly selected window if
minibuffer window is selected."
  (if (minibufferp)
      (minibuffer-selected-window)
    (selected-window)))

(defun popwin:adjust-window-edges (window edges hscale vscale)
  (destructuring-bind ((left top right bottom)
                       (cur-left cur-top cur-right cur-bottom))
      (list edges (window-edges window))
    (setq left (round (* left hscale))
          top (round (* top vscale))
          right (round (* right hscale))
          bottom (round (* bottom vscale)))
    (with-selected-window window
      (if (/= left cur-left)
          (enlarge-window-horizontally (- cur-left left)))
      (if (/= top cur-top)
        (enlarge-window (- cur-top top))))))

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

(defun popwin:replicate-window-config (window node hscale vscale)
  "Replicate NODE of windows configuration on WINDOW with
horizontal scale HSCALE and vertical scale VSCALE."
  (if (eq (car node) 'window)
      (destructuring-bind (buffer edges selected)
          (cdr node)
        (popwin:adjust-window-edges window edges hscale vscale)
        (with-selected-window window
          (switch-to-buffer buffer))
        selected)
    (destructuring-bind (dir edges . windows) node
      (loop while windows
            for w1 = (pop windows) then w2
            for w2 = (pop windows)
            do
            (popwin:adjust-window-edges window edges hscale vscale)
            (let ((new-window (split-window window nil (not dir))))
              (if (popwin:replicate-window-config window w1 hscale vscale)
                  (select-window window))
              (if (popwin:replicate-window-config new-window w2 hscale vscale)
                  (select-window new-window)))))))

(defun popwin:create-popup-window (height &optional window-scalable)
  "Create a popup window with HEIGHT on the frame. If
WINDOW-SCALABLE is non-nil, all of windows will be scaled
vertically to fit the frame. The return value is a pair of the
popup window and an original window-configuration."
  (let* ((win-config (current-window-configuration))
         (root-win (popwin:last-selected-window)))
    (destructuring-bind (root mini)
        (popwin:window-config-tree)
      (delete-other-windows root-win)
      (let* ((root-edges (window-edges root-win))
             (root-height (- (nth 3 root-edges) (nth 1 root-edges)))
             (master-height (- root-height (min root-height height)))
             (win (split-window root-win master-height))
             (hscale 1)
             (vscale (if window-scalable
                         (/ (float master-height) root-height)
                       1)))
        (popwin:replicate-window-config root-win root hscale vscale)
        (list win win-config)))))



;; Common User Interface

(defcustom popwin:window-height 15
  "Height of popup window."
  :type 'integer
  :group 'popwin)

(defcustom popwin:window-scalable t
  "Non-nil means all of windows will be scaled vertically to fit
the frame when a popup window is shown."
  :type 'booean
  :group 'popwin)

(defcustom popwin:special-display-buffer-names
  '("*Help*" "*Completions*" "*compilation*" "*interpretation*" "*Occur*")
  "Sample configuration of `special-display-buffer-names', which
are well tested with working on popwin. You need to set
`special-display-buffer-names' by yourself."
  :type '(repeat string)
  :group 'popwin)

(defcustom popwin:special-display-no-focus-buffer-names
  '("*Completions*" "*compilation*" "*Occur*")
  "A list of buffer names where the windows of buffers should not
be selected when displaying the buffer by using
`special-display-function'."
  :type '(repeat string)
  :group 'popwin)

(defvar popwin:popup-window nil
  "Main popup window instance.")

(defvar popwin:popup-buffer nil
  "Buffer of lastly shown in popup window.")

(defvar popwin:focus-window nil
  "Focused window which is used to check whether or not to keep
popup window.")

(defvar popwin:window-configuration nil
  "Original window configuration.")

(defvar popwin:close-timer nil
  "Timer for closing popup window.")

(defvar popwin:close-timer-interval 0.01
  "Interval of `popwin:close-timer'.")

(defun popwin:popup-window-live-p ()
  "Return t if `popwin:popup-window' is alive."
  (and popwin:popup-window
       (window-live-p popwin:popup-window)))

(defun popwin:buried-buffer-p (buffer)
  "Return t if `buffer' might be thought of as a buried buffer."
  (eq (car (last (buffer-list))) buffer))

(defun popwin:should-close-window-p ()
  "Return t if popwin should close window immediately. It is
useful if this is customizable function."
  (and popwin:popup-window
       (or (eq last-command 'keyboard-quit)
           (popwin:buried-buffer-p popwin:popup-buffer))))

(defun popwin:start-close-timer ()
  (or popwin:close-timer
      (setq popwin:close-timer
            (run-with-timer popwin:close-timer-interval popwin:close-timer-interval
                            'popwin:close-timer))))

(defun popwin:stop-close-timer ()
  (when popwin:close-timer
    (cancel-timer popwin:close-timer)
    (setq popwin:close-timer nil)))

(defun popwin:close-timer ()
  (popwin:close-window-if-necessary (popwin:should-close-window-p)))

(defun popwin:close-window ()
  "Close popup window and restore to the previous window
configuration. If the window configuration has been changed, the
last focused buffer will be selected at the end."
  (when popwin:popup-window
    (popwin:stop-close-timer)
    (let ((buffer (current-buffer)))
      (set-window-configuration popwin:window-configuration)
      (when (and (not (minibufferp))
                 (null (get-buffer-window buffer)))
        (switch-to-buffer buffer)))
    (setq popwin:popup-buffer nil
          popwin:popup-window nil
          popwin:focus-window nil
          popwin:window-configuration nil)))

(defun popwin:close-window-if-necessary (&optional force)
  "Close popup window if another window has been selected. If
FORCE is non-nil, this function tries to close popup window if
possible."
  (if (and popwin:popup-window
           (not (minibufferp))
           (or force
               (not (eq (selected-window) popwin:focus-window))))
      (popwin:close-window)))

(defun popwin:popup-buffer (buffer &optional no-focus)
  "Show buffer in a popup window and return the popup window. If
NO-FOCUS is non-nil, the popup window will not be
selected. Calling `popwin:popup-buffer' during
`popwin:popup-buffer' is allowed. In that case, the buffer of
popup window will be replaced with BUFFER."
  (unless (popwin:popup-window-live-p)
    (destructuring-bind (win win-config)
        (popwin:create-popup-window popwin:window-height popwin:window-scalable)
      (popwin:start-close-timer)
      (setf popwin:popup-window win
            popwin:window-configuration win-config)))
  (setq popwin:popup-buffer buffer
        popwin:focus-window (if no-focus (selected-window) popwin:popup-window))
  (select-window popwin:popup-window)
  (switch-to-buffer buffer)
  (select-window popwin:focus-window)
  popwin:popup-window)



;; Special Display

(defun popwin:special-display-popup-window (buffer list)
  "The `special-display-function' with popup window."
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
