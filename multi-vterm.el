;;; multi-vterm.el --- Like multi-term.el but for vterm -*- lexical-binding: t; -*-
;;
;; Authors: Minh Nguyen-Hue <minh.nh1989@gmail.com>
;; URL: https://github.com/suonlight/multi-libvterm
;; Keywords: terminals, processes
;; Version: 1.0
;; Package-Requires: ((emacs "26.3") (vterm "0.0") (project "0.3.0"))
;;
;;; Commentary:
;; Managing multiple vterm buffers in Emacs
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
;;  `project'
;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'project)

(defgroup multi-vterm nil
  "Multi term manager"
  :group 'vterm)

(defcustom multi-vterm-program nil
  "The shell program run by vterm.
If nil, this defaults to the SHELL environment variable."
  :type 'string
  :group 'multi-vterm)

(defcustom multi-vterm-buffer-name "vterminal"
  "The vterm buffer name."
  :type 'string
  :group 'multi-vterm)

(defcustom multi-vterm-dedicated-window-height 30
  "The height of the `multi-vterm' dedicated window in rows."
  :type 'integer
  :group 'multi-vterm)

(defcustom multi-vterm-dedicated-window-height-percent nil
  "The height of the `multi-vterm' dedicated window in percent of rows."
  :type 'integer
  :group 'multi-vterm)

;; Contants
(defconst multi-vterm-dedicated-buffer-name "dedicated"
  "The dedicated vterm buffer name.")

(defconst multi-vterm-dedicated-window-height-percent-limits '(10 90)
  "The dedicated vterm buffer height boundaries in percent")

;; Variables
(defvar multi-vterm-dedicated-window nil
  "The dedicated `multi-vterm' window.")

(defvar multi-vterm-dedicated-buffer nil
  "The dedicated `multi-vterm' buffer.")

(defvar multi-vterm-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `multi-vterm'.")

;; Interactive Functions
;;;###autoload
(defun multi-vterm (&optional vterm-name)
  "Create new vterm buffer."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer vterm-name)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (switch-to-buffer vterm-buffer)))

;;;###autoload
(defun multi-vterm-project ()
  "Create new vterm buffer."
  (interactive)
  (if (multi-vterm-project-root)
      (if (buffer-live-p (get-buffer (multi-vterm-project-get-buffer-name)))
          (if (string-equal (buffer-name (current-buffer)) (multi-vterm-project-get-buffer-name))
              (delete-window (selected-window))
            (switch-to-buffer-other-window (multi-vterm-project-get-buffer-name)))
        (let* ((vterm-buffer (multi-vterm-get-buffer 'project))
               (multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer))))
          (set-buffer vterm-buffer)
          (multi-vterm-internal)
          (switch-to-buffer-other-window vterm-buffer)))
    (message "This file is not in a project")))

;;;###autoload
(defun multi-vterm-dedicated-open ()
  "Open dedicated `multi-vterm' window."
  (interactive)
  (if (not (multi-vterm-dedicated-exist-p))
      (if (multi-vterm-buffer-exist-p multi-vterm-dedicated-buffer)
          (unless (multi-vterm-window-exist-p multi-vterm-dedicated-window)
            (multi-vterm-dedicated-get-window))
        (setq multi-vterm-dedicated-buffer (multi-vterm-get-buffer 'dedicated))
        (set-buffer (multi-vterm-dedicated-get-buffer-name))
        (multi-vterm-dedicated-get-window)
        (multi-vterm-internal)))
  (set-window-buffer multi-vterm-dedicated-window (get-buffer (multi-vterm-dedicated-get-buffer-name)))
  (set-window-dedicated-p multi-vterm-dedicated-window t)
  (select-window multi-vterm-dedicated-window)
  (message "`multi-vterm' dedicated window has exist."))

;;;###autoload
(defun multi-vterm-dedicated-close ()
  "Close dedicated `multi-vterm' window."
  (interactive)
  (if (multi-vterm-dedicated-exist-p)
      (let ((current-window (selected-window)))
        (multi-vterm-dedicated-select)
        (delete-window multi-vterm-dedicated-window)
        (if (multi-vterm-window-exist-p current-window)
            (select-window current-window)))
    (message "`multi-vterm' window does not exist.")))

;;;###autoload
(defun multi-vterm-dedicated-toggle ()
  "Toggle dedicated `multi-vterm' window."
  (interactive)
  (if (multi-vterm-dedicated-exist-p)
      (multi-vterm-dedicated-close)
    (multi-vterm-dedicated-open)))

;;;###autoload
(defun multi-vterm-dedicated-select ()
  "Select the `multi-vterm' dedicated window."
  (interactive)
  (if (multi-vterm-dedicated-exist-p)
      (select-window multi-vterm-dedicated-window)
    (message "`multi-vterm' window does not exist.")))

(defun multi-vterm-get-buffer (&optional dedicated-window)
  "Get vterm buffer name based on DEDICATED-WINDOW.
Optional argument DEDICATED-WINDOW: There are three types of DEDICATED-WINDOW: dedicated, project, default."
  (with-temp-buffer
    (let ((index 1)
          vterm-name)
      (cond ((eq dedicated-window 'dedicated) (setq vterm-name (multi-vterm-dedicated-get-buffer-name)))
            ((eq dedicated-window 'project) (progn
                                              (setq vterm-name (multi-vterm-project-get-buffer-name))
                                              (setq default-directory (multi-vterm-project-root))))
            ((stringp dedicated-window) (setq vterm-name dedicated-window))
            (t (progn
                 (while (buffer-live-p (get-buffer (multi-vterm-format-buffer-index index)))
                   (setq index (1+ index)))
                 (setq vterm-name (multi-vterm-format-buffer-index index)))))
      (let ((buffer (get-buffer vterm-name)))
        (if buffer
            buffer
          (let ((buffer (generate-new-buffer vterm-name)))
            (set-buffer buffer)
            (vterm-mode)
            buffer))))))

(defun multi-vterm-project-root ()
  "Get `default-directory' for project using projectile or project.el."
  (unless (boundp 'multi-vterm-projectile-installed-p)
    (setq multi-vterm-projectile-installed-p (require 'projectile nil t)))
  (if multi-vterm-projectile-installed-p
      (projectile-project-root)
    (project-root
     (or (project-current) `(transient . ,default-directory)))))

(defun multi-vterm-project-get-buffer-name ()
  "Get project buffer name."
  (multi-vterm-format-buffer-name (multi-vterm-project-root)))

(defun multi-vterm-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (rename-buffer (multi-vterm-format-buffer-name name)))

(defun multi-vterm-format-buffer-name (name)
  "Format vterm buffer NAME."
  (format "*%s - %s*" multi-vterm-buffer-name name))

(defun multi-vterm-format-buffer-index (index)
  "Format vterm buffer name with INDEX."
  (format "*%s<%s>*" multi-vterm-buffer-name index))

(defun multi-vterm-handle-close ()
  "Close current vterm buffer when `exit' from vterm buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-vterm-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-vterm-switch 'NEXT (or offset 1)))

(defun multi-vterm-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-vterm-switch 'PREVIOUS (or offset 1)))

(defun multi-vterm-switch (direction offset)
  "Internal `multi-vterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (multi-vterm-switch-internal direction offset)
    (multi-vterm)))

;; Utility Functions
(defun multi-vterm-internal ()
  "Internal handle for `multi-vterm' buffer."
  (multi-vterm-handle-close)
  (add-hook 'kill-buffer-hook #'multi-vterm-kill-buffer-hook))

(defun multi-vterm-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'vterm-mode)
    (let ((killed-buffer (current-buffer)))
      (setq multi-vterm-buffer-list
            (delq killed-buffer multi-vterm-buffer-list)))))

(defun multi-vterm-shell-name ()
  "Get shell-name based on var `multi-vterm-program' or env SHELL or default `shell-file-name'."
  (or multi-vterm-program
      (getenv "SHELL")
      shell-file-name))

(defun multi-vterm-dedicated-get-window ()
  "Get `multi-vterm' dedicated window."
  (setq multi-vterm-dedicated-window
	(split-window
	 (selected-window)
	 (- (multi-vterm-current-window-height) (multi-vterm-dedicated-calc-window-height)))))

(defun multi-vterm-current-window-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun multi-vterm-dedicated-calc-window-height ()
  "Return the height the dedicated `multi-vterm' window should have"
  (if multi-vterm-dedicated-window-height-percent
      (let* ((percent (min multi-vterm-dedicated-window-height-percent
			      (nth 1 multi-vterm-dedicated-window-height-percent-limits)))
	     (percent (max percent
			      (nth 0 multi-vterm-dedicated-window-height-percent-limits))))
	(ceiling (* (float (multi-vterm-current-window-height))
		    (/ (float percent)
		       100))))
	multi-vterm-dedicated-window-height))

(defun multi-vterm-dedicated-get-buffer-name ()
  "Get the buffer name of `multi-vterm' dedicated window."
  (multi-vterm-format-buffer-name multi-vterm-dedicated-buffer-name))

(defun multi-vterm-dedicated-exist-p ()
  "Return non-nil if `multi-vterm' dedicated window exists."
  (and (multi-vterm-buffer-exist-p multi-vterm-dedicated-buffer)
       (multi-vterm-window-exist-p multi-vterm-dedicated-window)))

(defun multi-vterm-window-exist-p (window)
  "Return non-nil if WINDOW exist."
  (and window (window-live-p window)))

(defun multi-vterm-buffer-exist-p (buffer)
  "Return non-nil if BUFFER exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun multi-vterm-switch-internal (direction offset)
  "Internal `multi-vterm' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (when multi-vterm-buffer-list
    (let ((buffer-list-len (length multi-vterm-buffer-list))
          (my-index (cl-position (current-buffer) multi-vterm-buffer-list)))
      (if my-index
          (let ((target-index (if (eq direction 'NEXT)
                                  (mod (+ my-index offset) buffer-list-len)
                                (mod (- my-index offset) buffer-list-len))))
            (switch-to-buffer (nth target-index multi-vterm-buffer-list)))
        (switch-to-buffer (car multi-vterm-buffer-list))))))

(provide 'multi-vterm)
;;; multi-vterm.el ends here
