;;; grid.el --- Two-dimensional layout system -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, docs, layout
;; URL: https://github.com/ichernyshovvv/grid.el

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

;; grid.el provides a two-dimensional layout system that you can use
;; to arrange text into blocks, rows or columns of blocks.

;;; Code:

(require 'subr-x)
(require 'map)
(require 'cl-lib)

(defvar-local grid--prev-states nil)

(defun grid-content-not-empty-p (box)
  "Non-nil if content of BOX is empty."
  (/= (plist-get box :start-marker)
      (plist-get box :end-marker)))

(defvar grid-overline '(:overline t) "Overline face.")

(defvar grid-underline '(:underline (:position -3)) "Underline face.")

(defvar grid-vertical-borders '(:box (:line-width (-1 . 0)))
  "Vertical borders face.")

(defun grid--apply-face (string face)
  "Apply FACE to STRING."
  (add-face-text-property
   0 (length string)
   face t string))

(defun grid--normalize-width (width)
  "Normalize WIDTH."
  (cond
   ((floatp width)
    (floor (* (window-width (get-buffer-window))
              width)))
   ((integerp width) width)
   (t (error "Wrong width format"))))

(defun grid--reformat-content (box)
  "Reformat CONTENT for a box with CONTENT-WIDTH and align it accoring to ALIGN."
  (map-let ((:content content) (:content-width content-width)
            (:width width) (:padding padding) (:margin margin))
      box
    (with-current-buffer (get-buffer-create "*grid-fill*")
      (pcase-let* ((`(,ptop ,_ ,pbottom ,_) padding)
                   (`(,mtop ,mright ,mbottom ,mleft) margin)
                   (vmargin-length (+ (car mright) (car mleft) width))
                   (indent-tabs-mode) (sentence-end-double-space))
        (erase-buffer)
        (setq fill-column content-width)
        (grid--insert-vspacing ptop content-width)
        (insert content)
        (grid--insert-vspacing pbottom content-width t)
        (goto-char (point-min))
        (grid--align-lines box)
        (put-text-property 1 2 'grid-box-filled t)
        (goto-char (point-min))
        (grid--insert-vspacing mtop vmargin-length)
        (goto-char (point-max))
        (grid--insert-vspacing mbottom vmargin-length t)
        (buffer-string)))))

(defun grid--longest-line-length (string)
  "Get the length of the longest line in STRING.
If the length of the longest line is 0, return 1."
  (thread-last (split-string string "\n")
               (seq-map #'length)
               (seq-max)
               (max 1)))

(defalias #'grid--merge-plists
  (apply-partially #'map-merge-with 'plist (lambda (_ x) x))
  "Merge plists, the last one takes precedence.")

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

(defun grid--normalize-field (field)
  (cond
   ((integerp field) `(,field . ?\s))
   ((nlistp (cdr-safe field)) field)
   ((not field) '(0 . ?\s))))

(defun grid--insert-vspacing (property width &optional newline)
  "Insert vertical spacing."
  (unless (zerop (car property))
    (and newline (insert-char ?\n))
    (dotimes (_ (car property))
      (insert-char (cdr property) width)
      (insert-char ?\n))
    (and newline (delete-char -1))))

(defun grid--insert-hspacing (property)
  "Insert horizontal spacing."
  (insert-char (cdr property) (car property)))

(defun grid--normalize-box (box)
  "Normalize BOX to plist."
  (let ((box (cond ((plistp box) (copy-tree box))
                   ((stringp box) (list :content box)))))
    (dolist (p '(:padding :margin))
      (let ((l (plist-get box p)))
        (setf (plist-get box p)
              (cl-loop
               for side in '(top right bottom left)
               for i in (pcase (proper-list-p l)
                          (4 l)
                          (2 `(,@l ,@l))
                          (0 '(nil nil nil nil))
                          ('nil (make-list 4 l))
                          (_ (error "Wrong `%s' format" p)))
               collect
               (grid--normalize-field
                (or (plist-get box (intern (format "%s-%s" p side)))
                    i))))))
    (map-let ((:content content) (:width width) (:padding padding))
        box
      (pcase-let* ((`(,_ (,pright . ,_) ,_ (,pleft . ,_)) padding)
                   (id-of-box-inside
                    (with-temp-buffer
                      (insert content)
                      (goto-char (point-min))
                      (text-property-search-forward 'grid-box-uuid)))
                   (uuid (or id-of-box-inside (grid--uuid)))
                   (width (max
                           (grid--normalize-width
                            (or width
                                (+ (grid--longest-line-length content)
                                   pleft pright)))
                           2)))
        (setq box
              (grid--merge-plists
               box
               (list
                :width width
                :content-width (max 2 (- width pleft pright))
                :uuid uuid)))
        (let ((new-content
               (progn
                 (unless id-of-box-inside
                   (setf (plist-get box :content)
                         (propertize content 'grid-box-uuid uuid)))
                 (grid--reformat-content box))))
          (grid--merge-plists
           box
           (list
            :start-marker 0
            :end-marker (length new-content)
            :content new-content)))))))

(defun grid--insert-box-line (box)
  "Format line from BOX and insert it."
  (map-let ((:content content)
            (:width width)
            (:margin margin)
            (:start-marker start-marker)
            (:end-marker end-marker))
      box
    (let* ((margin-width (+ width
                            (car (nth 1 margin))
                            (car (nth 3 margin))))
           (end (min (+ start-marker margin-width)
                     end-marker)))
      (insert (format (format "%% -%ds" margin-width)
                      (substring content start-marker end)))
      (plist-put box :start-marker (min (1+ end) end-marker)))))

(defun grid--insert-row (row)
  "Insert ROW in the current buffer."
  (let ((normalized-row (seq-map #'grid--normalize-box row)))
    (while (seq-some #'grid-content-not-empty-p normalized-row)
      (mapc #'grid--insert-box-line normalized-row)
      (insert ?\n))
    (delete-char -1)))

(defsubst grid--trim-line ()
  (beginning-of-line)
  (delete-horizontal-space)
  (end-of-line)
  (delete-horizontal-space))

(defsubst grid--align-line (align space)
  (pcase align
    (`center
     (beginning-of-line)
     (insert-char ?\s (ceiling space 2))
     (end-of-line)
     (insert-char ?\s (floor space 2)))
    ((or `nil `left)
     (end-of-line)
     (insert-char ?\s space))
    (`right
     (beginning-of-line)
     (insert-char ?\s space))))

(defun grid--align-lines (box)
  "Align lines in the current buffer with ALIGN.
ALIGN values: `left' (default), `right', `center', `full'."
  (interactive "P")
  (map-let ((:align align) (:padding padding)
            (:margin margin) (:border border))
      box
    (pcase-let ((`(,_ ,pright ,_ ,pleft) padding)
                (`(,_ ,mright ,_ ,mleft) margin)
                (space) (last-line))
      ;; mark newlines from original text
      (unless (or (eobp) (get-text-property 1 'grid-box-filled))
        (while (search-forward "\n" (1- (point-max)) t)
          (put-text-property (point) (1+ (point)) 'grid-box-newline t)
          (when (eolp)
            (insert-char ?\s)
            (add-text-properties
             (1- (point)) (point)
             `( grid-box-emptyline t
                grid-box-newline t
                grid-box-uuid ,(get-text-property (point-min) 'grid-box-uuid))))))
      (goto-char (point-min))
      (while (progn
               (if align (grid--trim-line) (end-of-line))
               (setq space (- fill-column (current-column)))
               (when (< space 0)
                 (let ((beg (line-beginning-position)))
                   (fill-region beg (line-end-position) align)
                   (goto-char beg)
                   (grid--trim-line)
                   (setq space (- fill-column (current-column)))
                   (when (< space 0)
                     (forward-char space)
                     (insert ?\n)
                     (setq space (+ fill-column space)))))
               (grid--align-line align space)
               (forward-line 1)
               (not (eobp))))
      (goto-char (point-min))
      (setq last-line (line-number-at-pos (1- (point-max))))
      (while (progn
               (grid--insert-hspacing mleft)
               (let* ((beg (point))
                      (current-line (line-number-at-pos))
                      (border-face
                       (append
                        (and (= current-line 1) grid-overline)
                        grid-vertical-borders
                        (and (= current-line last-line) grid-underline))))
                 (grid--insert-hspacing pleft)
                 (end-of-line)
                 (grid--insert-hspacing pright)
                 (and border
                      border-face
                      (add-face-text-property beg (point) border-face t)))
               (grid--insert-hspacing mright)
               (forward-line 1)
               (not (eobp)))))))

(defun grid--extract-content (arg)
  (pcase arg
    (`bounds
     (if-let* ((main-overlay
                (seq-find
                 (lambda (o) (overlay-get o 'grid-box-uuid))
                 (overlays-in (region-beginning) (region-end))))
               (overlays (overlay-get main-overlay 'overlays))
               (ends (mapcar #'overlay-end overlays))
               (starts (mapcar #'overlay-start overlays)))
         (list (cons (apply #'min starts)
                     (apply #'max ends)))
       (funcall grid-prev-region-extract-function 'bounds)))
    (`nil
     (when-let* ((start (region-beginning))
                 (end (region-end)))
       (save-excursion
         (goto-char start)
         (let ((uuid (get-text-property start 'grid-box-uuid)) prop string)
           (while (setq prop (text-property-search-forward
                              'grid-box-uuid t
                              (lambda (_ uuid-at-point)
                                (and (equal uuid uuid-at-point)
                                     (<= (point) end)))))
             (setq string
                   (concat string " "
                           (buffer-substring
                            (prop-match-beginning prop)
                            (min end (prop-match-end prop))))))
           (with-current-buffer (get-buffer-create " *grid-extract*")
             (erase-buffer)
             (insert string)
             (goto-char (point-min))
             (delete-char 1)
             (while (text-property-search-forward 'grid-box-newline)
               (backward-char 1)
               (delete-char -1)
               (insert-char ?\n)
               (forward-char 1))
             (goto-char (point-min))
             (while (text-property-search-forward 'grid-box-emptyline)
               (delete-char -1))
             (remove-text-properties
              (point-min) (point-max)
              '( face grid-overline
                 face grid-vertical-borders
                 face grid-underline))
             (remove-list-of-text-properties
              (point-min) (point-max)
              '(grid-box-uuid grid-box-newline grid-box-filled))
             (buffer-string))))))))

(defun grid-redisplay--select (start end window overlay)
  "Update the overlay OVERLAY in WINDOW with FACE in range START-END."
  (cond
   ((not (overlayp overlay))
    (let ((nrol (make-overlay start end)))
      (funcall redisplay-unhighlight-region-function overlay)
      (overlay-put nrol 'window window)
      (overlay-put nrol 'priority nil)
      (overlay-put nrol 'grid-box-uuid
                   (get-text-property start 'grid-box-uuid))
      (overlay-put nrol 'grid-active-region-start 0)
      (overlay-put nrol 'grid-active-region-end 0)
      nrol))
   ((and (eq start (overlay-get overlay 'grid-active-region-start))
         (eq end (overlay-get overlay 'grid-active-region-end)))
    overlay)
   (t
    (unless (and (eq (overlay-buffer overlay) (current-buffer))
                 (eq (overlay-start overlay) start)
                 (eq (overlay-end overlay) end))
      (move-overlay overlay start end (current-buffer)))

    (overlay-put overlay 'grid-active-region-start start)
    (overlay-put overlay 'grid-active-region-end end)
    (overlay-put overlay 'face nil)

    (save-excursion
      (goto-char start)
      (let ((uuid (or (overlay-get overlay 'grid-box-uuid)
                      (progn
                        (overlay-put overlay 'grid-box-uuid
                                     (get-text-property start 'grid-box-uuid))
                        (get-text-property start 'grid-box-uuid))))
            (overlays (or (overlay-get overlay 'overlays) (list)))
            prop)
        (while (setq prop (text-property-search-forward
                           'grid-box-uuid t
                           (lambda (_ uuid-at-point)
                             (and (equal uuid uuid-at-point)
                                  (<= (point) end)))))
          (let ((new-candidate-start (prop-match-beginning prop))
                (new-candidate-end (min end (prop-match-end prop))))
            (or
             (catch 'found
               (dolist (o overlays)
                 (when (or (and (= (overlay-start o) new-candidate-start)
                                (/= (overlay-end o) new-candidate-end))
                           (and (= (overlay-end o) new-candidate-end)
                                (/= (overlay-start o) new-candidate-start)))
                   (move-overlay o new-candidate-start new-candidate-end)
                   (throw 'found t))))
             (unless (catch 'found
                       (dolist (o overlays)
                         (when (<= (overlay-start o)
                                   new-candidate-start
                                   new-candidate-end
                                   (overlay-end o))
                           (throw 'found t))))
               (let ((ov (make-overlay new-candidate-start
                                       new-candidate-end)))
                 (overlay-put ov 'face 'region)
                 (overlay-put ov 'grid-box-active-region t)
                 (overlay-put ov 'window window)
                 (push ov overlays))))))
        (overlay-put overlay 'overlays
                     (seq-remove
                      (lambda (o)
                        (when (or (and (> (overlay-end o) end)
                                       (< start (overlay-start o)))
                                  (and (< (overlay-start o) start)
                                       (> end (overlay-end o))))
                          (delete-overlay o)
                          t))
                      overlays))))
    overlay)))

(defun grid-redisplay--unselect (rol)
  "If ROL is an overlay, call `delete-overlay'."
  (when (overlayp rol)
    (dolist (overlay (overlay-get rol 'overlays))
      (delete-overlay overlay))
    (overlay-put rol 'overlays nil)
    (overlay-put rol 'grid-box-uuid nil)
    (overlay-put rol 'grid-active-region-start nil)
    (overlay-put rol 'grid-active-region-end nil)))

(defcustom grid-revert-delay 0.3
  "Seconds to wait before redisplaying buffers with grid blocks."
  :type 'float)

(defvar grid--timer
  (let ((timer (timer-create)))
    (timer-set-function timer #'grid--do-revert)
    (timer-set-time timer (time-add nil grid-revert-delay))
    (timer-activate timer)
    timer))

(defun grid--set-revert-on-width-change (symbol value)
  "Set SYMBOL's value to VALUE."
  (if value
      (progn
        (add-hook 'window-state-change-hook #'grid--revert-maybe)
        (cl-pushnew #'grid--delayed-revert window-size-change-functions))
    (remove-hook 'window-state-change-hook #'grid--revert-maybe)
    (setq window-size-change-functions
          (delq #'grid--delayed-revert
                window-size-change-functions)))
  (set symbol value))

(defcustom grid-revert-on-width-change t
  "Whether to revert displayed buffers with grid blocks if window size changed."
  :type 'boolean
  :set #'grid--set-revert-on-width-change)

(defun grid--delayed-revert (&optional window)
  "Revert currently displayed grid buffers with delay of `grid-revert-delay' seconds."
  (cancel-timer grid--timer)
  (timer-activate grid--timer)
  (timer-set-time grid--timer (time-add nil grid-revert-delay)))

(defun grid--revert-maybe ()
  "Revert if windows count changed in the current frame."
  (unless (eq (frame-parameter (window-frame) 'grid--windows-count)
              (count-windows))
    (grid--do-revert)
    (set-frame-parameter (window-frame) 'grid--windows-count (count-windows))))

(defvar-local grid--window-width 0)

(defun grid--do-revert (&rest _)
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (and-let* (((memq 'grid-text-selection-mode local-minor-modes))
                 (width (window-pixel-width window))
                 ((/= width grid--window-width)))
        (condition-case err (revert-buffer nil t)
          (error
           (unless (equal "Buffer does not seem to be associated with any file"
                          (error-message-string err))
             (error (error-message-string err)))))
        (setq grid--window-width width)))))

;;; API

(define-minor-mode grid-text-selection-mode
  "Toggle the ability to select and copy from grid content."
  :global nil
  (if grid-text-selection-mode
      (setq
       grid--window-width (window-pixel-width)
       grid--prev-states
       (buffer-local-set-state
        redisplay-highlight-region-function #'grid-redisplay--select
        redisplay-unhighlight-region-function #'grid-redisplay--unselect
        region-extract-function #'grid--extract-content))
    (deactivate-mark)
    (buffer-local-restore-state grid--prev-states)))

(defun grid-insert-box (box)
  "Insert BOX in the current buffer."
  (let ((box (grid--normalize-box box)))
    (while (grid-content-not-empty-p box)
      (grid--insert-box-line box)
      (insert-char ?\n))
    (delete-char -1)))

(defun grid-make-box (box)
  "Return BOX as a string."
  (with-current-buffer (get-buffer-create " *grid-insert*")
    (erase-buffer)
    (grid-insert-box box)
    (buffer-string)))

(defun grid-insert-row (row)
  "Insert ROW in the current buffer."
  (grid--insert-row row)
  (insert ?\n))

(defun grid-insert-column (column)
  "Insert COLUMN in the current buffer."
  (grid-insert-rows (mapcar #'list column)))

(defun grid-make-column (column)
  "Return COLUMN as a string."
  (grid-make-rows (mapcar #'list column)))

(defun grid-insert-rows (rows)
  "Insert ROWS in the current buffer."
  (mapc #'grid-insert-row rows))

(defun grid-make-row (row)
  "Return ROW as a string."
  (with-current-buffer (get-buffer-create " *grid-insert*")
    (erase-buffer)
    (grid--insert-row row)
    (buffer-string)))

(defun grid-make-rows (rows)
  "Return ROWS as a string."
  (with-current-buffer (get-buffer-create " *grid-insert*")
    (erase-buffer)
    (grid-insert-rows rows)
    (buffer-string)))

(provide 'grid)

;;; grid.el ends here
