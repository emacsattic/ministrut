;;; ministrut.el -- Dedicated minibuffer frame as X11 strut

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100304
;; Updated: 20100304
;; Version: 0.1_alpha1
;; Homepage: http://github.com/tarsius/ministrut
;; Keywords: minibuffer

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

;; Emacs supports using a `default-minibuffer-frame' whose minibuffer
;; window is used by all frames that do not have such a window.  This
;; frame does not need to contain any other windows but when created
;; as described in the elisp info pages behaves like any other X11
;; window.

;; This package creates a dedicated minibuffer frame and also removes
;; it's X11 window decoration and sets it up as an X11 strut.

;; This is an alpha release and not all desired features have been
;; implemented yet.  If you would like to try it anyway add something
;; like this to your init file:

;; (add-to-list 'load-path "/path/to/ministrut/")
;; (require 'ministrut)
;; (setq mstrut-minibuffer-frame-alist
;;       '((left         .  853)
;;         (top          . 1600)
;;         (width        .  190)
;;         (height       .    1)
;;         (strut-bottom .   18)
;;         (left-fringe  .    0)
;;         (right-fringe .    0)))
;; (ministrut-mode 1)

;; I use this with a screen resolution of 2560x1600 and the window
;; manager wmii.  Also note that I do not use the whole wide of the
;; screen to display the minibuffer.

;; Your window manager might have to be configure too.  For wmii add
;; this to the tagrules:

;; /Emacs Minibuffer/ -> ~

;; Previously I have created `xmonad.el' which had more features but was
;; tailored towards xmonad only.  Since I have switched back to wmii and
;; would like to try other window managers as well I have decided to write
;; this library instead, which theoretically should be usable with any
;; window manager.

;; However at least xmonad does focus the minibuffer frame when reading
;; input from it which actually is not correct.  Similar problems are to
;; be expected with other window managers as well.

;;; Code:

(defgroup ministrut nil
  "Dedicated minibuffer frame as X11 strut."
  :group 'environment
  :group 'X)

(defcustom mstrut-minibuffer-frame-alist nil
  "Alist of additinal parameters for the minibuffer frame.

While in Ministrut mode these parameters are prepended to the value of
`minibuffer-frame-alist' which in turn supersede the values given in
`default-frame-alist'.

The properties `left', `top', `width' and `height' are used to specify the
geometry of the frame.  Additionally at least two of the strut properties
have to be specified.  See the variable `mstrut-strut-properties' and
http://standards.freedesktop.org/wm-spec/1.3/ar01s05.html#id2523368."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'ministrut)

(defcustom mstrut-default-frame-alist nil
  "Alist of additional parameters for regular frames.

While in Ministrut mode these parameters are prepended to the value of
`minibuffer-frame-alist' which in turn supersede the values given in
`default-frame-alist'.


The parameters specified here supersede the values given in the associated
value of the key `x' of the alist `window-system-default-frame-alist'
which in turn supersede the values given in `default-frame-alist'."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'ministrut)

(defconst mstrut--net-wm-strut-partial
  '(strut-left strut-right strut-top strut-bottom
    strut-left-start-y   strut-left-end-y
    strut-right-start-y  strut-right-end-y
    strut-top-start-x    strut-top-end-x
    strut-bottom-start-x strut-bottom-end-x)
  "Ordered list of values in the respecitve X11 window property.
DO NOT EDIT.")

(define-minor-mode ministrut-mode
  "Toggle Ministrut mode.

In Ministrut mode all frames (or those configured to) use the default
minibuffer frame.  This frame contains no other windows and is displayed
as an X11 strut without any window decoration.

Currently turing of this mode does remove that frame yet and frames using
it for minibuffer input will keep doing so.  However new frames will
behave as expected."
  :global t
  (cond (ministrut-mode
	 (put 'minibuffer-auto-raise 'default-value minibuffer-auto-raise)
	 (setq minibuffer-auto-raise t)
	 (unless (assq 'mstrut-end minibuffer-frame-alist)
	   (setq minibuffer-frame-alist
		 (append '((title . "Emacs Minibuffer"))
			 mstrut-minibuffer-frame-alist
			 '((minibuffer . only)
			   (mstrut-end))
			 minibuffer-frame-alist)))
	 (mstrut-make-minibuffer-frame)
	 (let* ((ass (assq 'x window-system-default-frame-alist))
	 	(old (cdr ass))
	 	(new (append mstrut-default-frame-alist
			     '((minibuffer . nil)
			       (mstrut-end))
			     old)))
	   (if ass
	       (setcdr ass new)
	     (push (cons 'x new) window-system-default-frame-alist))))
	(t
	 (setq minibuffer-frame-alist
	       (cdr (member '(mstrut-end) minibuffer-frame-alist)))
	 (setq window-system-default-frame-alist
	       (cdr (member '(mstrut-end) window-system-default-frame-alist)))
	 (modify-all-frames-parameters '((minibuffer . t)))
	 ;; TODO Replace existing minibuffer-less frames so that
	 ;; the dedicated minibuffer frame can be removed and all
	 ;; frames have their own minibuffer again.
	 ;; (delete-frame default-minibuffer-frame)
	 )))

(defun mstrut-make-minibuffer-frame (&optional display)
  "Setup the default minibuffer frame."
  (let* ((parms (append minibuffer-frame-alist
			'((minibuffer . only))))
	 (frame (if display
		    (make-frame-on-display display parms)
		  (make-frame parms)))
	 geometry make-strut-p)
    (dolist (key (reverse mstrut--net-wm-strut-partial))
      (let ((val (cdr (assoc key mstrut-minibuffer-frame-alist))))
	(push (or val 0) geometry)
	(when val
	  (setq make-strut-p t))))
    (when make-strut-p
      (x-change-window-property "_NET_WM_STRUT_PARTIAL" geometry frame
				"CARDINAL" 32 t))
    (x-change-window-property "_MOTIF_WM_HINTS" '(2 0 0 0 0) frame
			      "_MOTIF_WM_HINTS" 32 t)
    (setq default-minibuffer-frame frame)))

(provide 'ministrut)
;;; ministrut.el ends here
