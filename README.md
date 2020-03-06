# bouncing-dvd-logo.el
bouncing-dvd-logo.el is a Emacs Lisp port of Bouncing DVD Logo.

## Screenshot
![bouncing-dvd-logo-mode.gif](bouncing-dvd-logo-mode.gif)

## Usage
Call command `M-x bouncing-dvd-logo-mode`.

## Customize
Prevent turning child frame background color:
```elisp
(setq bouncing-dvd-logo-random-color-p nil)
```
Specify child frame background color (`bouncing-dvd-logo-random-color-p` must be nil):
```elisp
(setq bouncing-dvd-logo-fixed-color "red")
```

Customize inserted child frame contents:
```elisp
;; Set an S-exp to insert contents
;; To insert string
(setq bouncing-dvd-logo-insert-form '(insert "Hey"))

;; To insert image
(setq bouncing-dvd-logo-insert-form '(insert-image (create-image "~/foo.png")))
```

Caveat: don't insert too large image.

## Advanced
Bouncing child frame is one posframe frame, so you can dynamically modify contents.
```elisp
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (when bouncing-dvd-logo-mode
		(with-current-buffer (get-buffer bouncing-dvd-logo--buf-name)
		  (erase-buffer)
		  (dotimes (_ 3)
		    (insert "¯\\_(ツ)_/¯\n"))
		  (posframe-refresh bouncing-dvd-logo--buf-name)))))
```
