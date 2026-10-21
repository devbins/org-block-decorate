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
;;     Update #: 16
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

(defun org-block-decorate--hide-indent (base-indent)
  "Hide up to BASE-INDENT leading whitespace on the current line."
  (when (> base-indent 0)
    (let ((indent (min base-indent (current-indentation))))
      (when (> indent 0)
        (put-text-property (line-beginning-position)
                           (+ (line-beginning-position) indent)
                           'display "")))))

(defun org-block-decorate--fontify (limit)
  "Fontify Org blocks with bracket prefixes, searching up to LIMIT."
  (while (re-search-forward "^[ \t]*#\\+begin_\\(src\\|example\\|export\\|quote\\)\\>"
                            limit t)
    ;; 用 save-excursion 包住整块处理：
    ;; 处理中的 goto-char 不会把 point 推到 limit 之外，
    ;; 循环结束时 point 回到匹配末尾，while 安全推进。
    (save-excursion
      (goto-char (match-beginning 0))
      (let ((element (org-element-at-point)))
        (when (and (memq (org-element-type element)
                         '(src-block example-block export-block quote-block))
                   (org-element-property :end element))
          (let* ((block-start (org-element-property :begin element))
                 (end-pos     (org-element-property :end element))
                 (end-line-beg (save-excursion
                                 (goto-char (max (point-min) (1- end-pos)))
                                 (line-beginning-position)))
                 (end-line-end (save-excursion
                                 (goto-char (max (point-min) (1- end-pos)))
                                 (line-end-position)))
                 (base-indent (save-excursion
                                (goto-char block-start)
                                (current-indentation)))
                 (indent-str (make-string base-indent ?\s))
                 (top (concat indent-str
                              (propertize org-block-bracket-top
                                          'face 'org-block-bracket-face)))
                 (mid (concat indent-str
                              (propertize org-block-bracket-middle
                                          'face 'org-block-bracket-face)))
                 (bot (concat indent-str
                              (propertize org-block-bracket-bottom
                                          'face 'org-block-bracket-face))))
            ;; ★ 关键：标记整个块为"多行区域"，
            ;; 让 font-lock 在块内任何文本变化时重新字体化整个块。
            (put-text-property block-start end-pos
                               'font-lock-multiline t)
            ;; 1. 起始行
            (save-excursion
              (goto-char block-start)
              (org-block-decorate--hide-indent base-indent)
              (put-text-property (line-beginning-position) (line-end-position)
                                 'line-prefix top)
              (put-text-property (line-beginning-position) (line-end-position)
                                 'wrap-prefix mid))
            ;; 2. 中间行
            (save-excursion
              (goto-char block-start)
              (forward-line 1)
              (while (< (point) end-line-beg)
                (org-block-decorate--hide-indent base-indent)
                (put-text-property (line-beginning-position) (line-end-position)
                                   'line-prefix mid)
                (put-text-property (line-beginning-position) (line-end-position)
                                   'wrap-prefix mid)
                (forward-line 1)))
            ;; 3. 结束行
            (save-excursion
              (goto-char end-line-beg)
              (org-block-decorate--hide-indent base-indent)
              (put-text-property end-line-beg end-line-end
                                 'line-prefix bot)
              (put-text-property end-line-beg end-line-end
                                 'wrap-prefix mid))))))))

(defun org-block-decorate--setup ()
  "Set up font-lock for block brackets in the current Org buffer."
  (setq-local font-lock-extra-managed-props
              (append '(line-prefix wrap-prefix display font-lock-multiline)
                      font-lock-extra-managed-props))
  (font-lock-add-keywords nil
                          '((org-block-decorate--fontify))
                          'append)
  (font-lock-flush))


(provide 'org-block-decorate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-block.el ends here
