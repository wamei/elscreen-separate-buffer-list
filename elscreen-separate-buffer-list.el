;;; elscreen-separate-buffer-list.el --- Separate buffer list manager for elscreen

;; Author: wamei <wamei.cho@gmail.com>
;; Keywords: elscreen
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.4") (elscreen "1.4.6"))

;; License:

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

;; This makes elscreen can manage buffer list for each screen.
;;
;; To use this, add the following line somewhere in your init file:
;;
;;      (require 'elscreen-separate-buffer-list)
;;      (elscreen-separate-buffer-list-mode)
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'elscreen)

(defvar esbl-separate-buffer-list-default '("*scratch*" "*Messages*"))
(defvar esbl-separate-buffer-list '())
(defvar esbl-separate-buffer-count-list '())

(defun esbl-get-separate-window-history (screen)
  "SCREENに保存されているWINDOW-HISTORYを取得する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-window-history screen-property)))

(defun esbl-set-separate-window-history (screen winHistory)
  "SCREENにWINHISTORYで与えられるWINDOW-HISTORYを格納する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-window-history winHistory)
    (elscreen-set-screen-property screen screen-property)))

(defun esbl-save-separate-buffer-list (&optional screen)
  "SCREENに現在のSEPARATE-BUFFER-LISTを保存する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-buffer-list (esbl-get-separate-buffer-list))
    (elscreen-set-screen-property screen screen-property)))

(defun esbl-restore-separate-buffer-list (&optional screen)
  "SCREENに保存されているSEPARATE-BUFFER-LISTを復元する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (screen-property (elscreen-get-screen-property screen))
         (buffList (assoc-default 'separate-buffer-list screen-property)))
    (if buffList
        (setq esbl-separate-buffer-list buffList)
      (esbl-set-default-separate-buffer-list))))

(defun esbl-add-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTにBUFFERを加える."
  (unless (member buffer (esbl-get-separate-buffer-list))
    (setq esbl-separate-buffer-list (append (list buffer) (esbl-get-separate-buffer-list)))
    (esbl-separate-buffer-list-count-inc buffer)))

(defun esbl-remove-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTからBUFFERを取り除く."
  (esbl-separate-buffer-list-count-dec buffer)
  (setq esbl-separate-buffer-list (loop for i in (esbl-get-separate-buffer-list)
                                            unless (equal i buffer)
                                            collect i)))

(defun esbl-update-separate-buffer-list ()
  "SEPARATE-BUFFER-LISTを更新する."
  (esbl-separate-buffer-list-count-clean)
  (setq esbl-separate-buffer-list (loop for i in (esbl-get-separate-buffer-list)
                                            if (buffer-live-p i)
                                            collect i)))

(defun esbl-get-separate-buffer-list ()
  "SEPARATE-BUFFER-LISTを取得する."
  (when (equal 0 (length esbl-separate-buffer-list))
    (esbl-set-default-separate-buffer-list))
  esbl-separate-buffer-list)

(defun esbl-set-default-separate-buffer-list ()
  "デフォルトのバッファリストを設定する."
  (setq esbl-separate-buffer-list   (loop for i in esbl-separate-buffer-list-default
                                          collect (get-buffer i))))

(defun esbl-separate-buffer-list-count-inc (buffer)
  "BUFFERのカウントを上げる."
  (loop for i in esbl-separate-buffer-count-list
           if (equal (car i) buffer)
           do (setcdr i  (+ 1 (cdr i)))
           and return nil
           finally (push (cons buffer 1) esbl-separate-buffer-count-list)))

(defun esbl-separate-buffer-list-count-dec (buffer)
  "BUFFERのカウントを下げる."
  (setq esbl-separate-buffer-count-list   (loop for i in esbl-separate-buffer-count-list
                                                       if (equal (car i) buffer)
                                                       do (setcdr i  (- (cdr i) 1))
                                                       if (< 0 (cdr i))
                                                       collect i)))

(defun esbl-separate-buffer-list-count (buffer)
  "BUFFERのカウントを返す."
  (loop for i in esbl-separate-buffer-count-list
           if (equal (car i) buffer)
           return (cdr i)
           finally return 0))

(defun esbl-separate-buffer-list-count-clean ()
  "BUFFER-COUNTの掃除をする."
  (setq esbl-separate-buffer-count-list   (loop for i in esbl-separate-buffer-count-list
                                                       if (buffer-live-p (car i))
                                                       collect i)))

(defun esbl-goto:elscreen-goto (origin &rest args)
  "SCREENの切替時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを復元する."
  (esbl-set-separate-window-history (elscreen-get-current-screen) (esbl-get-all-window-history-alist))
  (apply origin args)
  (esbl-restore-all-window-history-alist (esbl-get-separate-window-history (elscreen-get-current-screen)))
  (when (elscreen-screen-live-p (elscreen-get-previous-screen))
    (esbl-save-separate-buffer-list (elscreen-get-previous-screen)))
  (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))

(defun esbl-clone:elscreen-clone (&rest _)
  "SCREENの複製時にSEPARATE-BUFFER-LISTも複製する."
  (esbl-restore-separate-buffer-list (elscreen-get-previous-screen)))

(defun esbl-kill:elscreen-kill (origin &rest args)
  "SCREENの削除時にBUFFERの削除、SEPARATE-BUFFER-LISTの復元をする."
  (mapc (lambda (buffer)
          (unless (member (buffer-name buffer) esbl-separate-buffer-list-default)
            (kill-buffer buffer)))
        (buffer-list))
  (apply origin args)
  (esbl-restore-separate-buffer-list (elscreen-get-current-screen)))

(defun esbl-kill-buffer-hook ()
  "BUFFER削除時にSEPARATE-BUFFER-LISTからも削除する."
  (let ((buffer (current-buffer)))
    (when (member buffer (esbl-get-separate-buffer-list))
      (esbl-remove-separate-buffer-list buffer)
      (if elscreen-separate-buffer-list-mode
          (if (> 1 (esbl-separate-buffer-list-count buffer))
              t
            (bury-buffer)
            nil)
        t))))

(defun esbl-buffer-list-update-hook ()
  "BUFFER-LIST更新時にSEPARATE-BUFFER-LISTも更新する."
  (esbl-update-separate-buffer-list))

(defun esbl-add-separate-buffer-list:switch-to-buffer (buffer &rest _)
  "BUFFER切り替え時にSEPARATE-BUFFER-LISTに追加する."
  (esbl-add-separate-buffer-list (get-buffer buffer)))

(defun esbl-add-separate-buffer-list:get-buffer-create (buffer &rest _)
  "GET-BUFFER-CREATE時にSEPARATE-BUFFER-LISTに追加する."
  (esbl-add-separate-buffer-list (get-buffer buffer)))

(defun esbl-return-separate-buffer-list:buffer-list (origin &rest _)
  "BUFFER-LISTが呼ばれた際にSEPARATE-BUFFER-LISTでフィルタリングを行う."
  (loop for i in (apply origin _)
           if (member (get-buffer i) (esbl-get-separate-buffer-list))
           collect i))

;; elscreenのパッチからパクってきた
(defun esbl-window-history-supported-p ()
  "WINDOW-HISTORYに対応しているかどうか."
  (and (fboundp 'window-prev-buffers)
       (fboundp 'window-next-buffers)
       (fboundp 'set-window-prev-buffers)
       (fboundp 'set-window-next-buffers)))

(defun esbl-get-all-window-history-alist ()
  "全てのウィンドウのWINDOW-HISTORYをALISTにして取得する."
  (when (esbl-window-history-supported-p)
    (mapcar (lambda (window)
              (let ((prevs (window-prev-buffers window))
                    (nexts (window-next-buffers window)))
                (cons window (cons prevs nexts))))
            (window-list))))

(defun esbl-restore-all-window-history-alist (history-alist)
  "HISTORY-ALISTからWINDOW-HISTORYを復元する."
  (when (esbl-window-history-supported-p)
    (mapc (lambda (entry)
            (let* ((window (car entry))
                   (histories (cdr entry))
                   (prevs (car histories))
                   (nexts (cdr histories)))
              (when (window-valid-p window)
                (set-window-prev-buffers window prevs)
                (set-window-next-buffers window nexts))))
          history-alist)))

(advice-add 'elscreen-goto :around 'esbl-goto:elscreen-goto)
(advice-add 'elscreen-clone :after 'esbl-clone:elscreen-clone)
(advice-add 'elscreen-kill :around 'esbl-kill:elscreen-kill)
(advice-add 'switch-to-buffer :after 'esbl-add-separate-buffer-list:switch-to-buffer)
(advice-add 'get-buffer-create :after 'esbl-add-separate-buffer-list:get-buffer-create)
(add-hook 'kill-buffer-query-functions 'esbl-kill-buffer-hook)
(add-hook 'buffer-list-update-hook 'esbl-buffer-list-update-hook)

;;;###autoload
(define-minor-mode elscreen-separate-buffer-list-mode
  "Toggle elscreen separate buffer list mode."
  :group 'elscreen
  :global t
  (if elscreen-separate-buffer-list-mode
        (advice-add 'buffer-list :around 'esbl-return-separate-buffer-list:buffer-list)
    (advice-remove 'buffer-list 'esbl-return-separate-buffer-list:buffer-list)))

(provide 'elscreen-separate-buffer-list)

;;; elscreen-separate-buffer-list.el ends here
