;;; bouncing-dvd-logo.el --- Bouncing DVD Logo for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 by Masahiro Nakamura

;; Author: Masahiro Nakamura <tsuucat@icloud.com>
;; Version: 0.0.2
;; URL: https://github.com/tsuu32/emacs-bouncing-dvd-logo
;; Package-Requires: ((emacs "26.1") (posframe "0.7.0"))
;; Keywords: convenience game

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Bouncing DVD Logo for Emacs
;; Try M-x bouncing-dvd-logo-mode

;;; Code:

(require 'cl-lib)
(require 'posframe)

(defgroup bouncing-dvd-logo nil
  "Display Bouncing DVD Logo"
  :group 'game
  :prefix "bouncing-dvd-logo-")

(defcustom bouncing-dvd-logo-item "DVD\n(o)"
  "Buffer contents displayed by the bouncing frame.
If string set, the string is just inserted.
If function set, the function is called."
  :group 'bouncing-dvd-logo
  :type '(choice (string :tag "Buffer string")
                 (function :tag "Altenative functon to insert item")))

(defcustom bouncing-dvd-logo-background-color 'randomize
  "Bouncing frame's background color.
If symbol randomize set, background color turns randomly.
If string set, the string is set as frame's background."
  :group 'bouncing-dvd-logo
  :type '(choice (const :tag "Turn randomly" randomize)
                 (string :tag "Color string")))


(defvar bouncing-dvd-logo--buf-name " *bouncing-dvd-logo*")
(defvar bouncing-dvd-logo--update-timer nil)

(cl-defstruct bouncing-dvd-logo
  (posframe nil)
  (x nil)
  (y nil)
  (xspeed 1)
  (yspeed 1))

(defvar bouncing-dvd-logo nil)

(defun bouncing-dvd-logo--update ()
  "Update position of bouncing child frame."
  (let* ((posframe (bouncing-dvd-logo-posframe bouncing-dvd-logo))
         (parent-frame (frame-parent posframe))
         (posframe-width (frame-pixel-width posframe))
         (posframe-height (frame-pixel-height posframe))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame)))

    ;; error when parent-frame is resized to too small
    (when (or (> posframe-width parent-frame-width)
              (> posframe-height parent-frame-height))
      (bouncing-dvd-logo-mode 0)
      (error "Child frame is too large"))

    ;; move
    (posframe--set-frame-position posframe
                                  (cons (bouncing-dvd-logo-x bouncing-dvd-logo)
                                        (bouncing-dvd-logo-y bouncing-dvd-logo))
                                  parent-frame-width
                                  parent-frame-height)

    ;; increment position
    (cl-incf (bouncing-dvd-logo-x bouncing-dvd-logo)
             (bouncing-dvd-logo-xspeed bouncing-dvd-logo))
    (cl-incf (bouncing-dvd-logo-y bouncing-dvd-logo)
             (bouncing-dvd-logo-yspeed bouncing-dvd-logo))

    ;; posframe should be on parent-frame (This is to deal parent-frame resizing)
    (when (>= (+ (bouncing-dvd-logo-x bouncing-dvd-logo) posframe-width)
              parent-frame-width)
      (setf (bouncing-dvd-logo-x bouncing-dvd-logo)
            (- parent-frame-width posframe-width)))
    (when (>= (+ (bouncing-dvd-logo-y bouncing-dvd-logo) posframe-height)
              parent-frame-height)
      (setf (bouncing-dvd-logo-y bouncing-dvd-logo)
            (- parent-frame-height posframe-height)))

    ;; reflection
    (when (or (>= (+ (bouncing-dvd-logo-x bouncing-dvd-logo) posframe-width)
                  parent-frame-width)
              (<= (bouncing-dvd-logo-x bouncing-dvd-logo) 0))
      (setf (bouncing-dvd-logo-xspeed bouncing-dvd-logo)
            (- (bouncing-dvd-logo-xspeed bouncing-dvd-logo)))
      (when (eq bouncing-dvd-logo-background-color 'randomize)
        (set-frame-parameter posframe
                             'background-color
                             (bouncing-dvd-logo--make-random-color))))
    (when (or (>= (+ (bouncing-dvd-logo-y bouncing-dvd-logo) posframe-height)
                  parent-frame-height)
              (<= (bouncing-dvd-logo-y bouncing-dvd-logo) 0))
      (setf (bouncing-dvd-logo-yspeed bouncing-dvd-logo)
            (- (bouncing-dvd-logo-yspeed bouncing-dvd-logo)))
      (when (eq bouncing-dvd-logo-background-color 'randomize)
        (set-frame-parameter posframe
                             'background-color
                             (bouncing-dvd-logo--make-random-color))))))

;; From: https://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149#%e3%83%a9%e3%83%b3%e3%83%80%e3%83%a0%e3%81%ab%e8%89%b2%e3%82%92%e7%94%9f%e6%88%90
(defun bouncing-dvd-logo--make-random-color ()
  "Make random 24bit color."
  (format "#%02x%02x%02x"
          (cl-random 255)
          (cl-random 255)
          (cl-random 255)))

;;;###autoload
(define-minor-mode bouncing-dvd-logo-mode
  "Toggle displaying bouncing child frame (Bouncing DVD Logo mode).
With a prefix argument ARG, enable Bouncing DVD Logo mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Bouncing DVD Logo mode is enabled, bouncing child frame starts
moving."
  :lighter " DVDlogo"
  :global t
  (when bouncing-dvd-logo--update-timer
    (cancel-timer bouncing-dvd-logo--update-timer))
  (setq bouncing-dvd-logo--update-timer nil)
  (cond
   (bouncing-dvd-logo-mode
    (with-current-buffer (get-buffer-create bouncing-dvd-logo--buf-name)
      (erase-buffer)
      (pcase bouncing-dvd-logo-item
        ((pred stringp)
         (insert bouncing-dvd-logo-item))
        ((pred functionp)
         (condition-case err
             (funcall bouncing-dvd-logo-item)
           (error
            (setq bouncing-dvd-logo-mode nil)
            (error "%s" (error-message-string err)))))
        (_ (error "Option bouncing-dvd-logo-item must be string or function"))))
      (let ((init-x (/ (frame-pixel-width (selected-frame)) 2))
            (init-y (/ (frame-pixel-height (selected-frame)) 2)))
        (setq bouncing-dvd-logo
              (make-bouncing-dvd-logo
               :x init-x
               :y init-y
               :posframe
               (posframe-show bouncing-dvd-logo--buf-name
                              :background-color
                              (if (eq bouncing-dvd-logo-background-color 'randomize)
                                  (bouncing-dvd-logo--make-random-color)
                                bouncing-dvd-logo-background-color)
                              :position (cons init-x init-y))))
        (posframe-refresh bouncing-dvd-logo--buf-name)
        (setq bouncing-dvd-logo--update-timer
              (run-at-time t 0.01 #'bouncing-dvd-logo--update))))
   (t
    (posframe-delete bouncing-dvd-logo--buf-name)
    (setq bouncing-dvd-logo nil))))

(provide 'bouncing-dvd-logo)

;;; bouncing-dvd-logo.el ends here
