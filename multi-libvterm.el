;;; multi-libvterm.el --- Like multi-term.el but for vterm -*- lexical-binding: t; -*-
;;
;; Authors: Minh Nguyen-Hue <minh.nh1989@gmail.com>
;; URL: https://github.com/suonlight/multi-libvterm
;; Keywords: terminals, processes
;; Version: 1.0
;; Package-Requires: ((emacs "26.3") (vterm "0.0") (projectile "1.2.0"))
;;
;;; Commentary:
;; Managing multiple libvterm (vterm) buffers in Emacs
;; This package is inspired by multi-term.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;  `vterm'
;;  `projectile'
;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'projectile)

(defgroup multi-libvterm nil
  "Multi term manager"
  :group 'vterm)

(defcustom multi-libvterm-program nil
  "The shell program run by vterm.
If nil, this defaults to the SHELL environment variable."
  :type 'string
  :group 'multi-libvterm)

(defcustom multi-libvterm-buffer-name "vterminal"
  "The vterm buffer name."
  :type 'string
  :group 'multi-libvterm)

(defcustom multi-libvterm-dedicated-window-height 30
  "The height of the `multi-libvterm' dedicated window in rows."
  :type 'integer
  :group 'multi-libvterm)

;; Contants
(defconst multi-libvterm-dedicated-buffer-name "vterm-dedicated"
  "The dedicated vterm buffer name.")

;; Variables
(defvar multi-libvterm-dedicated-window nil
  "The dedicated `multi-libvterm' window.")

(defvar multi-libvterm-dedicated-buffer nil
  "The dedicated `multi-libvterm' buffer.")

(defvar multi-libvterm-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `multi-libvterm'.")

;; Interactive Functions
;;;###autoload
(defun multi-libvterm (&optional _buffer-name)
  "Create new vterm buffer.
Will prompt you for the shell name when you type `C-u' before this command."
  (interactive)
  (let* ((vterm-buffer (multi-libvterm-get-buffer))
         (multi-libvterm-buffer-list (nconc multi-libvterm-buffer-list (list vterm-buffer))))
    (set-buffer vterm-buffer)
    (multi-libvterm-internal)
    (switch-to-buffer vterm-buffer)))

;;;###autoload
(defun multi-libvterm-projectile ()
  "Create new vterm buffer.
Will prompt you for the shell name when you type `C-u' before this command."
  (interactive)
  (if (projectile-project-p)
      (if (buffer-live-p (get-buffer (multi-libvterm-projectile-get-buffer-name)))
          (if (string-equal (buffer-name (current-buffer)) (multi-libvterm-projectile-get-buffer-name))
              (delete-window (selected-window))
            (switch-to-buffer-other-window (multi-libvterm-projectile-get-buffer-name)))
        (let* ((vterm-buffer (multi-libvterm-get-buffer 'projectile))
               (multi-libvterm-buffer-list (nconc multi-libvterm-buffer-list (list vterm-buffer))))
          (set-buffer vterm-buffer)
          (multi-libvterm-internal)
          (switch-to-buffer-other-window vterm-buffer)))
    (message "This file is not in a project")))

;;;###autoload
(defun multi-libvterm-dedicated-open ()
  "Open dedicated `multi-libvterm' window.
Will prompt you for the shell name when you type `C-u' before this command."
  (interactive)
  (if (not (multi-libvterm-dedicated-exist-p))
      (if (multi-libvterm-buffer-exist-p multi-libvterm-dedicated-buffer)
          (unless (multi-libvterm-window-exist-p multi-libvterm-dedicated-window)
            (multi-libvterm-dedicated-get-window))
        (setq multi-libvterm-dedicated-buffer (multi-libvterm-get-buffer 'dedicated))
        (set-buffer (multi-libvterm-dedicated-get-buffer-name))
        (multi-libvterm-dedicated-get-window)
        (multi-libvterm-internal))
    (set-window-buffer multi-libvterm-dedicated-window (get-buffer (multi-libvterm-dedicated-get-buffer-name)))
    (set-window-dedicated-p multi-libvterm-dedicated-window t)
    (select-window multi-libvterm-dedicated-window)
    (message "`multi-libvterm' dedicated window has exist.")))

;;;###autoload
(defun multi-libvterm-dedicated-close ()
  "Close dedicated `multi-libvterm' window."
  (interactive)
  (if (multi-libvterm-dedicated-exist-p)
      (let ((current-window (selected-window)))
        (multi-libvterm-dedicated-select)
        (delete-window multi-libvterm-dedicated-window)
        (if (multi-libvterm-window-exist-p current-window)
            (select-window current-window)))
    (message "`multi-libvterm' window does not exist.")))

;;;###autoload
(defun multi-libvterm-dedicated-toggle ()
  "Toggle dedicated `multi-libvterm' window."
  (interactive)
  (if (multi-libvterm-dedicated-exist-p)
      (multi-libvterm-dedicated-close)
    (multi-libvterm-dedicated-open)))

;;;###autoload
(defun multi-libvterm-dedicated-select ()
  "Select the `multi-libvterm' dedicated window."
  (interactive)
  (if (multi-libvterm-dedicated-exist-p)
      (select-window multi-libvterm-dedicated-window)
    (message "`multi-libvterm' window does not exist.")))

(defun multi-libvterm-get-buffer (&optional dedicated-window)
  "Get vterm buffer name based on DEDICATED-WINDOW.
Optional argument DEDICATED-WINDOW: There are three types of DEDICATED-WINDOW: dedicated, projectile, default."
  (with-temp-buffer
    (let ((index 1)
          vterm-name)
      (cond ((eq dedicated-window 'dedicated) (setq vterm-name (multi-libvterm-dedicated-get-buffer-name)))
            ((eq dedicated-window 'projectile) (progn
                                                 (setq vterm-name (multi-libvterm-projectile-get-buffer-name))
                                                 (setq default-directory (projectile-project-root))))
            (t (progn
                 (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-libvterm-buffer-name index)))
                   (setq index (1+ index)))
                 (setq vterm-name (format "*%s<%s>*" multi-libvterm-buffer-name index)))))
      (let ((buffer (get-buffer vterm-name)))
        (if buffer
            buffer
          (let ((buffer (generate-new-buffer vterm-name)))
            (set-buffer buffer)
            (vterm-mode)
            buffer))))))

(defun multi-libvterm-projectile-get-buffer-name ()
  "Get projectile buffer name."
  (format "*vterm - %s*" (projectile-project-root)))

(defun multi-libvterm-handle-close ()
  "Close current vterm buffer when `exit' from vterm buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-libvterm-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-libvterm-switch 'NEXT (or offset 1)))

(defun multi-libvterm-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-libvterm-switch 'PREVIOUS (or offset 1)))

(defun multi-libvterm-switch (direction offset)
  "Internal `multi-libvterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (multi-libvterm-switch-internal direction offset)
    (multi-libvterm)))

;; Utility Functions
(defun multi-libvterm-internal ()
  "Internal handle for `multi-libvterm' buffer."
  (multi-libvterm-handle-close)
  (add-hook 'kill-buffer-hook #'multi-libvterm-kill-buffer-hook))

(defun multi-libvterm-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'vterm-mode)
    (let ((killed-buffer (current-buffer)))
      (setq multi-libvterm-buffer-list
            (delq killed-buffer multi-libvterm-buffer-list)))))

(defun multi-libvterm-shell-name ()
  "Get shell-name based on var `multi-libvterm-program' or env SHELL or default `shell-file-name'."
  (or multi-libvterm-program
      (getenv "SHELL")
      shell-file-name))

(defun multi-libvterm-dedicated-get-window ()
  "Get `multi-libvterm' dedicated window."
  (setq multi-libvterm-dedicated-window
        (split-window
         (selected-window)
         (- (multi-libvterm-current-window-height) multi-libvterm-dedicated-window-height))))

(defun multi-libvterm-current-window-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))


(defun multi-libvterm-dedicated-get-buffer-name ()
  "Get the buffer name of `multi-libvterm' dedicated window."
  (format "*%s*" multi-libvterm-dedicated-buffer-name))

(defun multi-libvterm-dedicated-exist-p ()
  "Return non-nil if `multi-libvterm' dedicated window exists."
  (and (multi-libvterm-buffer-exist-p multi-libvterm-dedicated-buffer)
       (multi-libvterm-window-exist-p multi-libvterm-dedicated-window)))

(defun multi-libvterm-window-exist-p (window)
  "Return non-nil if WINDOW exist."
  (and window (window-live-p window)))

(defun multi-libvterm-buffer-exist-p (buffer)
  "Return non-nil if BUFFER exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun multi-libvterm-switch-internal (direction offset)
  "Internal `multi-libvterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (when multi-libvterm-buffer-list
    (let ((buffer-list-len (length multi-libvterm-buffer-list))
          (my-index (cl-position (current-buffer) multi-libvterm-buffer-list)))
      (if my-index
          (let ((target-index (if (eq direction 'NEXT)
                                  (mod (+ my-index offset) buffer-list-len)
                                (mod (- my-index offset) buffer-list-len))))
            (switch-to-buffer (nth target-index multi-libvterm-buffer-list)))
        (switch-to-buffer (car multi-libvterm-buffer-list))))))

(provide 'multi-libvterm)
;;; multi-libvterm.el ends here
