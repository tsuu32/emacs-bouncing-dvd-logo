# bouncing-dvd-logo.el
bouncing-dvd-logo.el is a Emacs Lisp port of Bouncing DVD Logo.

## Screenshot
![bouncing-dvd-logo-mode.gif](bouncing-dvd-logo-mode.gif)

## Usage
Call command `M-x bouncing-dvd-logo-mode`.

## Customize
To prevent turning child frame background color:
```elisp
(setq bouncing-dvd-logo-random-color-p nil)
```

To specify child frame background color (`bouncing-dvd-logo-random-color-p` must be nil):
```elisp
(setq bouncing-dvd-logo-fixed-color "red")
```

To customize inserting child frame contents:
```elisp
;; Set an S-exp to insert contents
;; To insert string
(setq bouncing-dvd-logo-insert-form '(insert "Hey"))

;; To insert image
(setq bouncing-dvd-logo-insert-form '(insert-image (create-image "~/foo.png")))
```

Caveat: don't insert too large image.

## Advanced
Bouncing child frame is one [posframe](https://github.com/tumashu/posframe) frame, so you can dynamically modify contents.
```elisp
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (when bouncing-dvd-logo-mode
		(with-current-buffer (get-buffer bouncing-dvd-logo--buf-name)
		  (erase-buffer)
		  (insert "  ___
 / _ \\ _ __ __ _
| | | | '__/ _` |
| |_| | | | (_| |
 \\___/|_|  \\__, |
           |___/
")
		  (posframe-refresh bouncing-dvd-logo--buf-name)))))
```
