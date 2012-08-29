;;; ministrut.el -- Dedicated minibuffer frame as X11 strut

;; Copyright (C) 2010-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100304
;; Version: 0.1.0
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

;; The `ministrut-mode' provided by this package creates a dedicated
;; minibuffer frame and then removes it's X11 window decoration and
;; turns it up as an X11 strut.  Your mileage may vary as not all
;; window managers have good support for struts and because Emacs does
;; not allow creating a X11 window which is born as a strut (instead a
;; regular window is created and only later turned into a strut).

;;; Code:

(require 'revive nil t)
(declare-function current-window-configuration-printable "revive" ())
(declare-function restore-window-configuration "revive" (config))

(defgroup ministrut nil
  "Dedicated minibuffer frame as X11 strut."
  :group 'environment
  :group 'X)

(lexical-let
    ((font-width     (frame-char-width))
     (font-height    (aref (font-info (face-attribute 'default :font)) 3))
     (display-width  (x-display-pixel-width))
     (display-height (x-display-pixel-height)))
  (defcustom mstrut-minibuffer-frame-alist
    `((strut-buttom . ,font-height)
      (height       . 1)
      (left         . 0)
      (top          . ,(- display-height font-height))
      (width        . ,(/ display-width font-width)))
    "Alist of additinal parameters for the minibuffer frame.

While in Ministrut mode these parameters are prepended to the
value of `minibuffer-frame-alist' which in turn supersede the
values given in `default-frame-alist'."
    :group 'ministrut
    :type
    `(choice
      (const :tag "top"
             ((strut-top    . ,font-height)
              (height       . 1)
              (left         . 0)
              (top          . 0)
              (width        . ,(/ display-width font-width))))
      (const :tag "buttom"
             ((strut-buttom . ,font-height)
              (height       . 1)
              (left         . 0)
              (top          . ,(- display-height font-height))
              (width        . ,(/ display-width font-width))))
      (const :tag "buttom, offset 1/3, width 1/3"
             ((strut-buttom . ,font-height)
              (height       . 1)
              (left         . ,(/ display-width 3))
              (top          . ,(- display-height font-height))
              (width        . ,(/ display-width 3 font-width))))
      (const :tag "buttom, offset 1/3, width 2/3"
             ((strut-buttom . ,font-height)
              (height       . 1)
              (left         . ,(/ display-width 3))
              (top          . ,(- display-height font-height))
              (width        . ,(/ (* display-width 2) 3 font-width))))
      (repeat :tag "custom"
              (cons :format "%v"
                    (symbol :tag "Parameter")
                    (sexp :tag "Value"))))))

(defcustom mstrut-default-frame-alist nil
  "Alist of additional parameters for regular frames.

While in Ministrut mode these parameters are prepended to the
value of `minibuffer-frame-alist' which in turn supersede the
values given in `default-frame-alist'.

The parameters specified here supersede the values given in the
associated value of the key `x' of the alist
`window-system-default-frame-alist' which in turn supersede the
values given in `default-frame-alist'."
  :group 'ministrut
  :type '(repeat (cons :format "%v"
                       (symbol :tag "Parameter")
                       (sexp :tag "Value"))))

(defcustom mstrut-convert-frames t
  "Whether toggling Ministrut Mode adds/removes minibuffers.

This option controls what happens to regular frames that already
exist when Ministrut mode is turned on or off.  Frames can either
be left alone; or they can have their minibuffer windows removed
when the mode is turned off, respectively have a minibuffer
window added when the mode on.

Actually it is not possible to add and remove minibuffer windows,
instead they are deleted and new frames are created.  This
requires library `revive'.

remove  Turning the mode on removes minibuffer windows.
add     Turning the mode off adds minibuffer windows.
t       Turning the mode on/off removes/adds minibuffer windows.
nil     Leave existing frames alone.")

(defconst mstrut--net-wm-strut-partial
  '(strut-left strut-right strut-top strut-bottom
    strut-left-start-y   strut-left-end-y
    strut-right-start-y  strut-right-end-y
    strut-top-start-x    strut-top-end-x
    strut-bottom-start-x strut-bottom-end-x))

;;;###autoload
(define-minor-mode ministrut-mode
  "Toggle Ministrut mode.

In Ministrut mode all frames (or those configured to) use the
default minibuffer frame.  This frame contains no other windows
and is displayed as an X11 strut without any window decoration.

Currently turing of this mode does remove that frame yet and
frames using it for minibuffer input will keep doing so.  However
new frames will behave as expected."
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
             (push (cons 'x new) window-system-default-frame-alist)))
         ;; (add-to-list 'special-display-buffer-names
         ;;              '("*Completions*" mstrut-display-completions))
         (add-to-list 'special-display-buffer-names
                      '("*Completions*" ((same-frame . t))))
         )
        (t
         (setq minibuffer-frame-alist
               (cdr (member '(mstrut-end) minibuffer-frame-alist)))
         (setq window-system-default-frame-alist
               (cdr (member '(mstrut-end) window-system-default-frame-alist)))
         (setq special-display-buffer-names
               (delete '("*Completions*" mstrut-display-completions)
                       special-display-buffer-names))
         (when (featurep 'revive)
           (let ((ministrut-window
                  (minibuffer-window
                   default-minibuffer-frame)))
             (dolist (frame (frame-list))
               (when (and (not (eq frame default-minibuffer-frame))
                          (not (frame-parameter frame 'minibuffer)))
                 (let ((config (with-selected-frame frame
                                 (current-window-configuration-printable))))
                   (delete-frame frame)
                   (with-selected-frame (make-frame)
                     (restore-window-configuration config)))))))
         (when (or (featurep 'revive)
                   (yes-or-no-p
                    "There are still frames using the ministrut; delete it anyway? "))
           (delete-frame default-minibuffer-frame)))))

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
    (if make-strut-p
        (x-change-window-property "_NET_WM_STRUT_PARTIAL" geometry frame
                                  "CARDINAL" 32 t)
      (message "No strut property has been set; using a regular frame"))
    (x-change-window-property "_MOTIF_WM_HINTS" '(2 0 0 0 0) frame
                              "_MOTIF_WM_HINTS" 32 t)
    (setq default-minibuffer-frame frame)
    (run-hooks 'mstrut-after-make-minibuffer-frame-hook)))

;; (defun mstrut-handle-switch-frame (event)
;;   (interactive "e")
;;   (message "1 %s %s" (selected-frame) (last-nonminibuffer-frame))
;;   (call-interactively 'handle-switch-frame nil (cadr event))
;;   (message "2 %s %s" (selected-frame) (last-nonminibuffer-frame)))

;; (global-set-key [switch-frame] 'mstrut-handle-switch-frame)
;; (global-set-key [switch-frame] 'handle-switch-frame)

(defadvice handle-switch-frame (around mstrut-handle-switch-frame activate)
  (message "1 %-35s %s" (selected-frame) (last-nonminibuffer-frame))
  ad-do-it
  ;; Variable `last-nonminibuffer-frame' is unusable; when selecting
  ;; the minibuffer-only frame it is set to some random frame not the
  ;; one that was previously selected.  So use our own.
  (unless (eq (selected-frame) default-minibuffer-frame)
    (setq mstrut-last-nonminibuffer-frame (selected-frame)))
  (message "2 %-35s %s" (selected-frame) (last-nonminibuffer-frame)))

(defvar mstrut-after-make-minibuffer-frame-hook nil)

(defun mstrut-display-completions (buffer &rest args)
  ;; FIXME when using a minibuffer frame `last-nonminibuffer-frame'
  ;; just returns some random frame, which defeats it's purpose.
  (message "*: %s %s %s" (selected-frame) (last-nonminibuffer-frame) mstrut-last-frame)
  (let (display-buffer-reuse-frames pop-up-frames
        special-display-regexps
        special-display-buffer-names
        )
    (display-buffer buffer nil mstrut-last-frame)))


'(
  (display-buffer "*Completions*")
  (add-to-list 'special-display-buffer-names
               '("*Completions*" mstrut-display-completions nil))
  (add-to-list 'special-display-buffer-names
               '("*Completions*" ((same-frame . t))))
  (add-to-list 'special-display-buffer-names
               '("*Completions*" ((same-window . t))))
  (setq special-display-buffer-names nil)
  (special-display-p (get-buffer "*Completions*"))
  special-display-function
  )
(provide 'ministrut)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ministrut.el ends here

(defun display-buffer (buffer-or-name &optional action frame)
  "Display BUFFER-OR-NAME in some window, without selecting it.
BUFFER-OR-NAME must be a buffer or the name of an existing
buffer.  Return the window chosen for displaying BUFFER-OR-NAME,
or nil if no such window is found.

Optional argument ACTION should have the form (FUNCTION . ALIST).
FUNCTION is either a function or a list of functions.
ALIST is an arbitrary association list (alist).

Each such FUNCTION should accept two arguments: the buffer to
display and an alist.  Based on those arguments, it should either
display the buffer and return the window, or return nil if unable
to display the buffer.

The `display-buffer' function builds a function list and an alist
by combining the functions and alists specified in
`display-buffer-overriding-action', `display-buffer-alist', the
ACTION argument, `display-buffer-base-action', and
`display-buffer-fallback-action' (in order).  Then it calls each
function in the combined function list in turn, passing the
buffer as the first argument and the combined alist as the second
argument, until one of the functions returns non-nil.

Available action functions include:
 `display-buffer-same-window'
 `display-buffer-reuse-window'
 `display-buffer-pop-up-frame'
 `display-buffer-pop-up-window'
 `display-buffer-use-some-window'

Recognized alist entries include:

 `inhibit-same-window' -- A non-nil value prevents the same
                          window from being used for display.

 `reusable-frames' -- Value specifies frame(s) to search for a
                      window that already displays the buffer.
                      See `display-buffer-reuse-window'.

The ACTION argument to `display-buffer' can also have a non-nil
and non-list value.  This means to display the buffer in a window
other than the selected one, even if it is already displayed in
the selected window.  If called interactively with a prefix
argument, ACTION is t.

Optional argument FRAME, if non-nil, acts like an additional
ALIST entry (reusable-frames . FRAME), specifying the frame(s) to
search for a window that is already displaying the buffer.  See
`display-buffer-reuse-window'."
  (interactive (list (read-buffer "Display buffer: " (other-buffer))
		     (if current-prefix-arg t)))
  (message "display-buffer(%s) %s" buffer-or-name display-buffer-function)
  (let ((buffer (if (bufferp buffer-or-name)
		    buffer-or-name
		  (get-buffer buffer-or-name)))
	;; Handle the old form of the first argument.
	(inhibit-same-window (and action (not (listp action)))))
    (unless (listp action) (setq action nil))
    (if display-buffer-function
	;; If `display-buffer-function' is defined, let it do the job.
	(funcall display-buffer-function buffer inhibit-same-window)
      ;; Otherwise, use the defined actions.
      (let* ((user-action
	      (display-buffer-assq-regexp (buffer-name buffer)
					  display-buffer-alist))
             (special-action (display-buffer--special-action buffer))
	     ;; Extra actions from the arguments to this function:
	     (extra-action
	      (cons nil (append (if inhibit-same-window
				    '((inhibit-same-window . t)))
				(if frame
				    `((reusable-frames . ,frame))))))
	     ;; Construct action function list and action alist.
	     (actions (list display-buffer-overriding-action
			    user-action special-action action extra-action
			    display-buffer-base-action
			    display-buffer-fallback-action))
	     (functions (apply 'append
			       (mapcar (lambda (x)
					 (setq x (car x))
					 (if (functionp x) (list x) x))
				       actions)))
	     (alist (apply 'append (mapcar 'cdr actions)))
	     window)
        (message "functions: %S" functions)
	(unless (buffer-live-p buffer)
	  (error "Invalid buffer"))
	(while (and functions (not window))
          (message "  try: %s" (car functions))
	  (setq window (funcall (car functions) buffer alist)
	  	functions (cdr functions)))
	window))))
