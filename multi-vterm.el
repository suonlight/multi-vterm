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

(defun multi-vterm--dedicated-term-matcher (buffer _action)
  "Match the dedicated multi-vterm buffer for `display-buffer-alist'."
  (string-equal (multi-vterm--dedicated-get-buffer-name) (if (bufferp buffer) (buffer-name buffer) buffer)))

(defun multi-vterm--term-matcher (buffer _action)
  "Match a multi-vterm buffer for `display-buffer-alist'."
  (string-prefix-p (concat "*" multi-vterm-buffer-name) (if (bufferp buffer) (buffer-name buffer) buffer)))

(defcustom multi-vterm-dedicated-window-dimensions
  '(:target-width 70
    :target-height 30
    :min-width 50
    :min-height 10)
  "The size hints of the dedicated window"
  :type '(plist :value-type integer)
  :options '(:target-width :target-height :min-width :min-height)
  :set (lambda (_name new-value)
         (customize-set-variable
          'display-buffer-alist
          `((multi-vterm--dedicated-term-matcher
             (display-buffer-in-side-window)
             (dedicated . t)
             (window-min-height . ,(plist-get new-value :min-height))
             (window-height . ,(plist-get new-value :target-height))
             (window-min-width . ,(plist-get new-value :min-width))
             (window-width . ,(plist-get new-value :target-width))))))
  :group 'multi-vterm)

(defcustom multi-vterm-dedicated-window-side 'bottom
  "The side of the dedicated window"
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Top" top)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :set (lambda (_name new-value)
         (customize-set-variable
          'display-buffer-alist
          `((multi-vterm--dedicated-term-matcher
             (display-buffer-in-side-window)
             (dedicated . t)
             (side . ,new-value)))))
  :group 'multi-vterm)

(defcustom multi-vterm-window-dimensions
  '(:target-width 70
    :target-height 30
    :min-width 50
    :min-height 10)
  "The size hints of the vterm windows"
  :type '(plist :value-type integer)
  :options '(:target-width :target-height :min-width :min-height)
  :set (lambda (_name new-value)
         (customize-set-variable
          'display-buffer-alist
          `((multi-vterm--term-matcher
             (display-buffer-reuse-window display-buffer-pop-up-window display-buffer-use-some-window display-buffer-use-some-frame display-buffer-pop-up-frame)
             (dedicated . t)
             (mode . vterm)
             (window-min-height . ,(plist-get new-value :min-height))
             (window-height . ,(plist-get new-value :target-height))
             (window-min-width . ,(plist-get new-value :min-width))
             (window-width . ,(plist-get new-value :target-width))))))
  :group 'multi-vterm)

(defcustom multi-vterm-dedicated-buffer-name "dedicated"
  "The dedicated vterm buffer name."
  :type 'string)

;;;; Variables
(defvar multi-vterm-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `multi-vterm'.")

;;;; Interactive Functions
;;;###autoload
(defun multi-vterm ()
  "Create new vterm buffer."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer-create)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (pop-to-buffer vterm-buffer)))

;;;###autoload
(defun multi-vterm-project ()
  "Create new project vterm buffer."
  (interactive)
  (if-let ((project-name (multi-vterm-project-root)))
      (if (buffer-live-p (get-buffer (multi-vterm-project-get-buffer-name)))
          (if (string-equal (buffer-name (current-buffer)) (multi-vterm-project-get-buffer-name))
              (progn
                (delete-window (selected-window))
                (message "Deleted terminal window for %s" project-name))
            (pop-to-buffer (multi-vterm-project-get-buffer-name))
            (message "Switched terminal window for %s" project-name))
        (let* ((vterm-buffer (multi-vterm-get-buffer-create 'project))
               (multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer))))
          (pop-to-buffer vterm-buffer)
          (message "Added terminal window for %s" project-name)))
    (user-error "This file is not in a project")))

;;;###autoload
(defun multi-vterm-dedicated-open ()
  "Open dedicated `multi-vterm' window."
  (interactive)
  (pop-to-buffer (multi-vterm-get-buffer-create 'dedicated))
  (message "`multi-vterm' dedicated window exists."))

;;;###autoload
(defun multi-vterm-dedicated-close ()
  "Close dedicated `multi-vterm' window."
  (interactive)
  (if (multi-vterm--dedicated-exist-p)
      (let ((current-window (selected-window)))
        (multi-vterm-dedicated-select)
        (delete-window (multi-vterm--dedicated-get-buffer-window))
        (if (multi-vterm--window-exist-p current-window)
            (select-window current-window)))
    (message "`multi-vterm' window does not exist.")))

;;;###autoload
(defun multi-vterm-dedicated-toggle ()
  "Toggle dedicated `multi-vterm' window."
  (interactive)
  (if (multi-vterm--dedicated-exist-p)
      (multi-vterm-dedicated-close)
    (multi-vterm-dedicated-open)))

;;;###autoload
(defun multi-vterm-dedicated-select ()
  "Select the `multi-vterm' dedicated window."
  (interactive)
  (if (multi-vterm--dedicated-exist-p)
      (select-window (multi-vterm--dedicated-get-buffer-window))
    (message "`multi-vterm' window does not exist.")))

(defun multi-vterm-get-buffer-create (&optional dedicated-window)
  "Get vterm buffer based on DEDICATED-WINDOW, creating it if necessary.
 Optional argument DEDICATED-WINDOW: There are three types of DEDICATED-WINDOW: dedicated, project, default."
  (with-temp-buffer
    (let ((index 1)
          vterm-name)
      (cond ((eq dedicated-window 'dedicated) (setq vterm-name (multi-vterm--dedicated-get-buffer-name)))
            ((eq dedicated-window 'project) (progn
                                              (setq vterm-name (multi-vterm-project-get-buffer-name))
                                              (setq default-directory (multi-vterm-project-root))))
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
            (multi-vterm--internal buffer)
            buffer))))))

(defun multi-vterm-project-root ()
  "Get `default-directory' for project using projectile or project.el."
  (unless (boundp 'multi-vterm--projectile-installed-p)
    (setq multi-vterm--projectile-installed-p (require 'projectile nil t)))
  (if multi-vterm--projectile-installed-p
      (projectile-project-root)
    (project-root
     (or (project-current) `(transient . ,default-directory)))))

(defun multi-vterm-project-get-buffer-name ()
  "Get project buffer name."
  (multi-vterm-format-buffer-name (multi-vterm-project-root)))

(defun multi-vterm-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (if (string-equal (buffer-name) (multi-vterm--dedicated-get-buffer-name))
      (user-error "Cannot rename the dedicated terminal buffer!")
    (rename-buffer (multi-vterm-format-buffer-name name))))

(defun multi-vterm-format-buffer-name (name &optional tag)
  "Format vterm buffer NAME."
  (cond
   ((eq tag 'dedicated) (format "*%s*" multi-vterm-dedicated-buffer-name))
   (t (format "*%s - %s*" multi-vterm-buffer-name name))))

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
  (multi-vterm--switch 'NEXT (or offset 1)))

(defun multi-vterm-prev (&optional offset)
  "Go to the previous term buffer.
 If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-vterm--switch 'PREVIOUS (or offset 1)))

(defun multi-vterm--switch (direction offset)
  "Internal `multi-vterm' buffers switch function.
 If DIRECTION is `NEXT', switch to the next term.
 If DIRECTION `PREVIOUS', switch to the previous term.
 Option OFFSET for skip OFFSET number term buffer."
  (unless (multi-vterm--switch-internal direction offset)
    (multi-vterm)))

;;;; Utility Functions
(defun multi-vterm--internal (buffer)
  "Internal handle for `multi-vterm' buffer."
  (with-current-buffer buffer
    (multi-vterm-handle-close)
    (add-hook 'kill-buffer-hook #'multi-vterm--kill-buffer-hook)))

(defun multi-vterm--kill-buffer-hook ()
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

(defun multi-vterm--dedicated-get-buffer-name ()
  "Get the buffer name of `multi-vterm' dedicated window."
  (multi-vterm-format-buffer-name nil 'dedicated))

(defun multi-vterm--dedicated-get-buffer ()
  "Get the buffer of `multi-vterm' dedicated window, or nil if it does not exist."
  (get-buffer (multi-vterm--dedicated-get-buffer-name)))

(defun multi-vterm--dedicated-get-buffer-window ()
  "Get the `multi-vterm' dedicated window, or nil if it does not exist."
  (get-buffer-window (multi-vterm--dedicated-get-buffer)))

(defun multi-vterm--dedicated-exist-p ()
  "Return non-nil if `multi-vterm' dedicated window exists."
  (let ((dedicated-buffer (multi-vterm--dedicated-get-buffer)))
    (and (multi-vterm--buffer-exist-p dedicated-buffer)
         (multi-vterm--window-exist-p (get-buffer-window dedicated-buffer)))))

(defun multi-vterm--window-exist-p (window)
  "Return non-nil if WINDOW exist."
  (and window (window-live-p window)))

(defun multi-vterm--buffer-exist-p (buffer)
  "Return non-nil if BUFFER exist.
 Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun multi-vterm--switch-internal (direction offset)
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
            (pop-to-buffer (nth target-index multi-vterm-buffer-list))
            (message "Terminal %s / %s" (1+ target-index) buffer-list-len)
            t)
        (pop-to-buffer (car multi-vterm-buffer-list))
        (message "Terminal 1 / %s" buffer-list-len)
        t))))


(provide 'multi-vterm)
;;; multi-vterm.el ends here
