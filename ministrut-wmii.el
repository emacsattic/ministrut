;;; ministrut.el -- ministrut kludges for wmii

;; Copyright (C) 2010-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Code:

(require 'ministrut)

(defun mstrut-wmii-after-make-minibuffer-frame ()
  ;; Wmii seems to be confused about what window is selected at this
  ;; point.  Selecting the window "above" and then come back to what
  ;; was previously "selected" gives as a usable selection: the
  ;; ministrut frame.
  (call-process "wmiir" nil nil nil "xwrite" "/tag/sel/ctl" "select" "up")
  (call-process "wmiir" nil nil nil "xwrite" "/tag/sel/ctl" "select" "down")
  ;; Now that wmii isn't confused anymore about what window is
  ;; selected we can toogle from the floating layer the ministrut is
  ;; known to be in to the managed layer we want to be in (otherwise
  ;; we wouldn't be using wmii, or would we).
  (call-process "wmiir" nil nil nil "xwrite" "/tag/sel/ctl" "select" "toggle")
  ;; Moving away and coming back "touched" the ministrut frame causing
  ;; wmii to removed the window decoration.  While doing so it has
  ;; also increased the size of the X11 window's content area, adding
  ;; the space originally used by the window decoration to the window
  ;; area.  So we have to resize.
  (set-frame-parameter default-minibuffer-frame
                       'height (cdr (assq 'height minibuffer-frame-alist))))

(add-hook 'mstrut-after-make-minibuffer-frame-hook
          'mstrut-wmii-after-make-minibuffer-frame)

;; (setq special-display-buffer-names '("*Completions*"))
;; (setq special-display-buffer-names nil)
;; (last-nonminibuffer-frame)

;; (add-to-list 'special-display-buffer-names
;;              `("*Completions*" ministrut-completion-magic))

;; (defun ministrut-completion-magic (buf &optional args)
;;   (let ((old (last-nonminibuffer-frame))
;;         return-window)
;;     (setq return-window (select-window (funcall special-display-function buf args)))
;;     (raise-frame)
;;     (redirect-frame-focus (selected-frame) default-minibuffer-frame)
;;     return-window))

;; (defun ministrut-completion-magic (buf &optional args)
;;   (let ((sel
;;          ;;(selected-frame)
;;          (last-nonminibuffer-frame)
;;          )
;;         (dis (funcall special-display-function buf args)))
;;     (message "sel: %s" sel)
;;     (message "dis: %s" dis)
;;     (setq minibuffer-scroll-window dis)
;;     (select-frame sel)
;;     dis))

(defun my-display-completions2 (buf)
  (let ((pop-up-windows t)
        (pop-up-frames nil)
        special-display-buffer-names
        special-display-regexps)
      (display-buffer buf)))

(defun my-display-completions (buf)
  "put the *completions* buffer in the right spot"
  (let ((windows (delete (minibuffer-window) (window-list))))
    (if (eq 1 (length windows))
        (progn 
          (select-window (car windows))
          (split-window-vertically)))
    (let ((target-window (window-at 0 (- (frame-height) 2)))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window)))

(provide 'ministrut-wmii)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ministrut-wmii.el ends here
