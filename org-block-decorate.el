;;; init-org-block.el --- -*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Filename: init-org-block.el
;; Description:
;; Author: devbin
;; Maintainer:
;; Copyright (C) 2019 devbin
;; Created: Wed Oct 21 23:23:15 2026 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 31
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'org-element)
(require 'font-lock)

(defgroup org-block-decorate nil
  "Custom font-lock based brackets for Org source blocks."
  :group 'org-faces)

(defface org-block-bracket-face
  '((t :inherit shadow :weight light))
  "Face for the block bracket characters."
  :group 'org-block-decorate)

(defcustom org-block-bracket-top "╭─"
  "String used for the top-left corner of a block bracket."
  :type 'string
  :group 'org-block-decorate)

(defcustom org-block-bracket-middle "│ "
  "String used for the left vertical line of a block bracket."
  :type 'string
  :group 'org-block-decorate)

(defcustom org-block-bracket-bottom "╰─"
  "String used for the bottom-left corner of a block bracket."
  :type 'string
  :group 'org-block-decorate)

(defconst org-block-decorate--overlay-prop 'org-block-decorate
  "Overlay property marking decorations owned by this mode.")

(defun org-block-decorate--clear (start end)
  "Delete `org-block-decorate' overlays between START and END."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov org-block-decorate--overlay-prop)
      (delete-overlay ov))))

(defun org-block-decorate--put (beg end string)
  "Overlay [BEG END] so STRING (a bracket) shows just before BEG.
The bracket is drawn purely as display (`before-string'), so it never
touches or hides the buffer's real indentation.
A blank line gives BEG = END, and `evaporate' would delete such a
zero-width overlay immediately, so cover the following newline instead;
either way the bracket is display-only and the text is untouched."
  (let* ((end (if (= beg end) (min (point-max) (1+ beg)) end))
         (ov  (make-overlay beg end nil t nil)))
    (overlay-put ov org-block-decorate--overlay-prop t)
    (overlay-put ov 'before-string
                 (propertize string 'face 'org-block-bracket-face))
    ;; `evaporate' removes the overlay when its text is deleted.  A
    ;; zero-width overlay can only survive at `point-max', where it cannot
    ;; be widened and `evaporate' would delete it at once.
    (unless (= beg end)
      (overlay-put ov 'evaporate t))
    ov))

(defun org-block-decorate--bracket-pos (line-beg base-indent)
  "Buffer position of the block's left edge on the line starting at LINE-BEG.
That is BASE-INDENT columns in from LINE-BEG, clamped to that line's end.
`line-end-position' is evaluated at LINE-BEG, not at the caller's point,
so it works whether or not point is on that line."
  (min (+ line-beg base-indent)
       (save-excursion (goto-char line-beg) (line-end-position))))

(defun org-block-decorate--fontify (start end)
  "Draw block brackets for Org blocks overlapping [START END].
Registered as a `jit-lock' function.  It only manages overlays and the
display-only `wrap-prefix' property; it never modifies or hides the
buffer's real indentation, so editing, `TAB'/`org-cycle' and cursor
movement keep behaving normally."
  (save-excursion
    (save-restriction
      (widen)
      ;; jit-lock often hands us a partial region that begins in the middle
      ;; of a block.  Move START back to that block's beginning so we always
      ;; clear and redraw whole blocks (otherwise stale overlays survive and
      ;; brackets accumulate on every refontification, e.g. fold/unfold).
      (goto-char start)
      (let ((b (save-excursion
                 (re-search-backward
                  "^[ \t]*#\\+begin_\\(src\\|example\\|export\\|quote\\)\\>" nil t))))
        (when b
          (save-excursion
            (goto-char b)
            (let ((el (org-element-at-point)))
              (when (and el (org-element-property :end el)
                         (>= (org-element-property :end el) start))
                (setq start b))))))
      (org-block-decorate--clear start end)
      (goto-char start)
      (while (re-search-forward
              "^[ \t]*#\\+begin_\\(src\\|example\\|export\\|quote\\)\\>"
              end t)
        (save-excursion
          (goto-char (match-beginning 0))
          (let ((element (org-element-at-point)))
            (when (and (memq (org-element-type element)
                             '(src-block example-block export-block quote-block))
                       (org-element-property :end element))
              (let* ((block-start (org-element-property :begin element))
                     (end-pos     (org-element-property :end element))
                     ;; `:end' includes the blank lines following the block, so
                     ;; back them out before looking for the `#+end_...' line.
                     (last-pos    (max block-start
                                       (- end-pos
                                          (or (org-element-property
                                               :post-blank element)
                                              0))))
                     (end-line-beg (save-excursion
                                     (goto-char (max (point-min) (1- last-pos)))
                                     (line-beginning-position)))
                     (end-line-end (save-excursion
                                     (goto-char (max (point-min) (1- last-pos)))
                                     (line-end-position)))
                     (base-indent (save-excursion
                                    (goto-char block-start)
                                    (current-indentation)))
                     (wrap (concat (make-string base-indent ?\s)
                                   (propertize org-block-bracket-middle
                                               'face 'org-block-bracket-face))))
                ;; Clear this block's full range first: the region we were
                ;; asked to fontify may be only part of it, so clearing just
                ;; [start end] would leave stale overlays behind and let
                ;; brackets accumulate on each refontification.
                (org-block-decorate--clear block-start end-pos)
                (remove-text-properties block-start end-pos '(wrap-prefix nil))
                ;; 1. 起始行：括号画在块起始列（缩进之后），与块左缘对齐。
                (let ((lb (line-beginning-position)))
                  (org-block-decorate--put
                   (org-block-decorate--bracket-pos lb base-indent)
                   (line-end-position)
                   org-block-bracket-top)
                  (put-text-property lb (line-end-position) 'wrap-prefix wrap))
                ;; 2. 中间行
                (save-excursion
                  (goto-char block-start)
                  (forward-line 1)
                  (while (< (point) end-line-beg)
                    (let* ((lb  (line-beginning-position))
                           (le  (line-end-position))
                           (pos (org-block-decorate--bracket-pos lb base-indent)))
                      ;; The bracket position is clamped to the line's end, so
                      ;; on a blank line (shorter than BASE-INDENT) it would
                      ;; land in column 0 instead of the block's left column.
                      ;; Pad the display-only string to keep the vertical line
                      ;; aligned with the rest of the block.
                      (org-block-decorate--put
                       pos le
                       (concat (make-string (- (+ lb base-indent) pos) ?\s)
                               org-block-bracket-middle))
                      (put-text-property lb le 'wrap-prefix wrap))
                    (forward-line 1)))
                ;; 3. 结束行
                (org-block-decorate--put
                 (org-block-decorate--bracket-pos end-line-beg base-indent)
                 end-line-end
                 org-block-bracket-bottom)
                (put-text-property end-line-beg end-line-end
                                   'wrap-prefix wrap)))))))))

(defun org-block-decorate--setup ()
  "Set up bracket decorations for the current Org buffer."
  (jit-lock-register #'org-block-decorate--fontify)
  ;; Force an initial pass over the whole buffer.
  (org-block-decorate--fontify (point-min) (point-max)))

(defun org-block-decorate--teardown ()
  "Remove bracket decorations from the current Org buffer."
  (jit-lock-unregister #'org-block-decorate--fontify)
  (org-block-decorate--clear (point-min) (point-max))
  (save-restriction
    (widen)
    (remove-text-properties (point-min) (point-max)
                            '(wrap-prefix nil))))

;;;###autoload
(define-minor-mode org-block-decorate-mode
  "Toggle bracket-line decoration for Org source blocks."
  :group 'org-block-decorate
  :lighter " block-dc"
  (if org-block-decorate-mode
      (org-block-decorate--setup)
    (org-block-decorate--teardown)))

;;;###autoload
(defun org-block-decorate-mode-enable ()
  "Enable `org-block-decorate-mode' in the current buffer."
  (org-block-decorate-mode 1))

;;;###autoload
(defun org-block-decorate-mode-disable ()
  "Disable `org-block-decorate-mode' in the current buffer."
  (org-block-decorate-mode -1))

;;;###autoload
(define-global-minor-mode global-org-block-decorate-mode
  org-block-decorate-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-block-decorate-mode 1))))

(provide 'org-block-decorate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-block.el ends here
