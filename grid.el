;;; grid.el --- Simple grid layout -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, docs, layout
;; URL: https://github.com/ichernyshovvv/grid

;;; License:

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

;; USAGE:
;; (grid-insert-rows '(ROW ROW ...))
;; ROW: '(BOX BOX ...)
;; BOX: plist.  Acceptable properties:

;;   :border
;;   nil or t

;;   :width
;;   "50%"
;;   integer (number of characters)

;;   :padding - horizontal padding
;;   integer (number of characters)

;;   :content - string to be inserted in the box

;;; Code:

(require 'subr-x)
(require 'map)

(defvar grid-margin 1)

(defun grid-content-not-empty-p (box)
  "Non-nil if content of BOX is empty."
  (not (string-empty-p (plist-get box :content))))

(defvar grid-overline '(:overline t) "Overline face.")

(defvar grid-underline '(:underline (:position -3)) "Underline face.")

(defvar grid-vertical-borders '(:box (:line-width (-1 . 0)))
  "Vertical borders face.")

(defun grid--apply-face (string face)
  "Apply FACE to STRING."
  (declare (indent 1))
  (add-face-text-property
   0 (length string)
   face t string))

(defun grid--normalize-width (width)
  "Normalize WIDTH."
  (if (stringp width)
      (floor
       (* (window-width)
          (/ (string-to-number width) 100.0)))
    width))

(defun grid--reformat-content (content width align)
  "Reformat CONTENT for a box with WIDTH and align it accoring to ALIGN."
  (let (indent-tabs-mode sentence-end-double-space)
    (with-current-buffer (get-buffer-create " *grid-fill*")
      (erase-buffer)
      (setq fill-column width)
      (insert content)
      (goto-char (point-min))
      (grid--align-lines align)
      (buffer-string))))

(defun grid--longest-line-length (string)
  "Get the length of the longest line in STRING."
  (thread-last (split-string string "\n")
               (seq-map #'length)
               (seq-max)))

(defalias #'grid--merge-plists
  (apply-partially #'map-merge-with 'plist (lambda (_ x) x))
  "Merge plists, the last one takes precedence.")

(defun grid--fill-box (box)
  "Calculate and fill in the missing fields in BOX."
  ;; `map-let' doesn't provide access to keywords directly
  (map-let ((:content content)
            (:align align)
            (:padding padding)
            (:width width))
      box
    (let* ((padding (* (or padding 0) 2))
           (width-raw (or width (grid--longest-line-length content)))
           (width (- (grid--normalize-width width-raw) padding))
           (content (grid--reformat-content content width align))
           (box-extra (list :width width
                            :content content
                            :length (length content))))
      (when (< width 0)
        (user-error "Horizonal padding %s must be less than width %s"
                    padding width-raw))
      (grid--merge-plists box box-extra))))

(defun grid--uuid ()
  "Return string with random (version 4) UUID."
  ;; This is a copy of `org-id-uuid'.
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (current-time)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun grid--normalize-box (box)
  "Normalize BOX to plist."
  (let* ((box (cond
               ((plistp box) (copy-tree box))
               ((stringp box) (list :content box))))
         (uuid (grid--uuid)))
    (unless (get-text-property 0 'grid-box-uuid
                               (plist-get box :content #'equal))
      (plist-put box :uuid uuid #'equal)
      (plist-put box :content
                 (propertize (plist-get box :content)
                             'grid-box-uuid uuid)
                 #'equal))
    box))

(defun grid--format-box (box)
  "Insert BOX in the current buffer."
  (map-let ((:content content)
            (:padding padding)
            (:width width)
            (:border border)
            (:length length))
      box
    (let* ((content-len (length content))
           ;; isn't it filled with zero by default?
           (line-len (min width content-len))
           (padding (make-string (or padding 0) ? ))
           (fmt (format "%s%% -%ds%s" padding width padding))
           (line (format fmt (substring content 0 line-len)))
           (new-content (substring content
                                   (min content-len (1+ width))))
           (combined-face (append
                           ;; first line?
                           (and (= length content-len) grid-overline)
                           ;; in body?
                           (and (/= content-len 0) grid-vertical-borders)
                           ;; last line?
                           (and (not (string-empty-p content))
                                (string-empty-p new-content) grid-underline))))
      (when (and border combined-face)
        (grid--apply-face line combined-face))
      (setq box (plist-put box :content new-content))
      line)))

(defun grid--insert-row (row)
  "Insert ROW in the current buffer."
  (let ((normalized-row (seq-map (lambda (box)
                                   (grid--fill-box (grid--normalize-box box)))
                                 row)))
    (while (seq-some #'grid-content-not-empty-p normalized-row)
      (mapc (lambda (box)
              (insert (grid--format-box box))
              (insert-char ?  grid-margin))
            normalized-row)
      (delete-char (* grid-margin -1))
      (insert ?\n)))
  (insert ?\n))

(defsubst grid--trim-line ()
  (beginning-of-line)
  (delete-horizontal-space)
  (end-of-line)
  (delete-horizontal-space))

(defsubst grid--align-line (align space)
  (pcase align
    (`center
     (beginning-of-line)
     (insert-char ?  (ceiling space 2))
     (end-of-line)
     (insert-char ?  (floor space 2)))
    ((or `nil `left)
     (end-of-line)
     (insert-char ?  space))
    (`right
     (beginning-of-line)
     (insert-char ?  space))))

(defun grid--align-lines (align)
  "Align lines in the current buffer with ALIGN.
ALIGN values: `left' (default), `right', `center', `full'."
  (interactive "P")
  (let (space)
    (while (not (eobp))
      (if align
          (grid--trim-line)
        (end-of-line))
      (setq space (- fill-column (current-column)))
      (if (>= space 0)
          (grid--align-line align space)
        (let ((beg (line-beginning-position)))
          (fill-region beg (line-end-position) align)
          (goto-char beg)
          (grid--trim-line)
          (setq space (- fill-column (current-column)))
          (when (< space 0)
            (forward-char space)
            (insert ?\n)
            (setq space (+ fill-column space)))
          (grid--align-line align space)))
      (forward-line 1))))

(defun grid--extract-content (arg)
  (pcase arg
    (`nil
     (when-let* (((region-active-p))
                 (start (region-beginning))
                 (end (region-end)))
       (save-excursion
         (goto-char start)
         (let ((uuid (get-text-property start 'grid-box-uuid)) prop string)
           (while (setq prop (text-property-search-forward
                              'grid-box-uuid t
                              (lambda (_ uuid-at-point)
                                (and (equal uuid uuid-at-point)
                                     (<= (point) (line-end-position))
                                     (<= (point) end)))))
             (setq string
                   (concat string
                           (buffer-substring
                            (prop-match-beginning prop)
                            (min end (prop-match-end prop))))))
           (substring-no-properties string)))))))

(defun grid-redisplay--select (start end window overlay)
  "Update the overlay OVERLAY in WINDOW with FACE in range START-END."
  (if (not (overlayp overlay))
      (let ((nrol (make-overlay start end)))
        (funcall redisplay-unhighlight-region-function overlay)
        (overlay-put nrol 'window window)
        (overlay-put nrol 'priority nil)
        nrol)
    (unless (and (eq (overlay-buffer overlay) (current-buffer))
                 (eq (overlay-start overlay) start)
                 (eq (overlay-end overlay) end))
      (move-overlay overlay start end (current-buffer)))
    (when (/= (line-number-at-pos start)
              (line-number-at-pos end))
      (overlay-put overlay 'face nil)
      (save-excursion
        (goto-char start)
        (let ((uuid (get-text-property start 'grid-box-uuid)) prop)
          (while (setq prop (text-property-search-forward
                             'grid-box-uuid t
                             (lambda (_ uuid-at-point)
                               (and (equal uuid uuid-at-point)
                                    (<= (point) (line-end-position))
                                    (<= (point) end)))))
            (let ((ov (make-overlay
                       (prop-match-beginning prop)
                       (min end (prop-match-end prop)))))
              (overlay-put ov 'face 'region)
              (overlay-put ov 'grid-box-active-region t)
              (overlay-put ov 'window window))))))
    overlay))

(defun grid-redisplay--unselect (rol)
  "If ROL is an overlay, call `delete-overlay'."
  (when-let* ((overlayp rol)
              (start (overlay-start rol))
              (end (overlay-end rol)))
    (dolist (overlay (overlays-in start end))
      (when (overlay-get overlay 'grid-box-active-region)
        (delete-overlay overlay)))))

;;; API

(defun grid-insert-box (box)
  "Insert BOX in the current buffer."
  (grid-insert-row (list box)))

(defun grid-get-box (box)
  "Return BOX as a string."
  (grid-get-row (list box)))

(defun grid-insert-row (row)
  "Insert ROW in the current buffer."
  (grid--insert-row row))

(defun grid-insert-column (column)
  "Insert COLUMN in the current buffer."
  (grid-insert-rows (mapcar #'list column)))

(defun grid-get-column (column)
  "Return COLUMN as a string."
  (grid-get-rows (mapcar #'list column)))

(defun grid-insert-rows (rows)
  "Insert ROWS in the current buffer."
  (mapc #'grid-insert-row rows))

(defun grid-get-row (row)
  "Return ROW as a string."
  (with-temp-buffer
    (grid-insert-row row)
    (buffer-string)))

(defun grid-get-rows (rows)
  "Return ROWS as a string."
  (with-temp-buffer
    (grid-insert-rows rows)
    (buffer-string)))

(provide 'grid)

;;; grid.el ends here
