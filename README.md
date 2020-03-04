# bouncing-dvd-logo.el
bouncing-dvd-logo.el is a Emacs Lisp port of Bouncing DVD Logo.

## Screenshot
![bouncing-dvd-logo-mode.gif](bouncing-dvd-logo-mode.gif)

## Usage
Call command `M-x bouncing-dvd-logo-mode`.

## Customize
If you don't want child frame to turn background color, set `bouncing-dvd-logo-random-color-p` to nil.
```elisp
(setq bouncing-dvd-logo-random-color-p nil)
```
You can specify fixed color when `bouncing-dvd-logo-random-color-p` is nil.
```elisp
(setq bouncing-dvd-logo-fixed-color "red")
```

If you want to customize child frame contents, set `bouncing-dvd-logo-insert-form` to an S-exp to insert contents.
```elisp
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
