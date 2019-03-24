# multi-libvterm.el

This is similiar with [multi-term.el](https://github.com/milkypostman/multi-term) but it integrates with [emacs-libvterm](https://github.com/akermu/emacs-libvterm)

# Installation

Follow [emacs-libvterm](https://github.com/akermu/emacs-libvterm) to install `emacs-libvterm`

## Install by source code

```
(use-package multi-libvterm
  :load-path "/path/to/multi-libvterm")
```

# Usage

| Command                         | Description                                     |
|---------------------------------|-------------------------------------------------|
| multi-libvterm                  | Create new terminal                             |
| multi-libvterm-next             | Switch to next terminal                         |
| multi-libvterm-prev             | Switch to previous terminal                     |
| multi-libvterm-dedicated-toggle | Toggle dedicated terminal                       |
| multi-libvterm-projectile       | Create/toggle terminal based on current project |

# For Evil users

I'm using Evil. This is my personal config to use libvterm with `evil`


```elisp
(use-package vterm
	:load-path "/path/to/emacs-libvterm"
	:config
	(setq vterm-keymap-exceptions nil)
	(defvar bash-shortcuts '("C-e"
				 "C-f"
				 "C-a"
				 "C-v"
				 "C-b"
				 "C-w"
				 "C-u"
				 "C-d"
				 "C-n"
				 "C-m"
				 "C-p"
				 "C-j"
				 "C-k"
				 "C-r"
				 "C-t"
				 "C-g"
				 "C-SPC"
				 "C-c"))

	(cl-loop for key in bash-shortcuts
		 do (evil-declare-key 'insert vterm-mode-map (kbd key) 'vterm--self-insert))

	(add-hook 'vterm-mode-hook
		  (lambda ()
		    (evil-insert-state)))

	(evil-declare-key 'normal vterm-mode-map (kbd ",c") #'multi-libvterm)
	(evil-declare-key 'normal vterm-mode-map (kbd ",n") #'multi-libvterm-next)
	(evil-declare-key 'normal vterm-mode-map (kbd ",p") #'multi-libvterm-prev)
	(evil-declare-key 'normal vterm-mode-map (kbd "o") 'evil-insert-state))
	(evil-declare-key 'normal vterm-mode-map (kbd "C-d") 'vterm--self-insert)
```
