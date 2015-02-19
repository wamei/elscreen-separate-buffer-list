;;; elscreen-separate-buffer-list.el --- Separate buffer list manager for elscreen

;; Author: wamei <wamei.cho@gmail.com>
;; Keywords: elscreen
;; Version: 0.1.1
;; Package-Requires: ((elscreen "1.4.6"))

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
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'elscreen)

(defvar elscreen-separate-buffer-list-default '("*scratch*" "*Messages*"))
(defvar elscreen-separate-buffer-list '())
(defvar elscreen-separate-buffer-count-list '())

(defun elscreen-get-separate-buffer-list (screen)
  "SCREENに保存されているSEPARATE-BUFFER-LISTを取得する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-buffer-list screen-property)))

(defun elscreen-set-separate-buffer-list (screen buflist)
  "SCREENにBUFLISTで与えられるSEPARATE-BUFFER-LISTを格納する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-buffer-list buflist)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-get-separate-window-history (screen)
  "SCREENに保存されているWINDOW-HISTORYを取得する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (assoc-default 'separate-window-history screen-property)))

(defun elscreen-set-separate-window-history (screen winHistory)
  "SCREENにWINHISTORYで与えられるWINDOW-HISTORYを格納する."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (elscreen--set-alist 'screen-property 'separate-window-history winHistory)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-save-separate-buffer-list (&optional screen)
  "SCREENに現在のSEPARATE-BUFFER-LISTを保存する."
  (let ((screen (or screen (elscreen-get-current-screen))))
    (elscreen-set-separate-buffer-list screen elscreen-separate-buffer-list)))

(defun elscreen-restore-separate-buffer-list (&optional screen)
  "SCREENに保存されているSEPARATE-BUFFER-LISTを復元する."
  (let* ((screen (or screen (elscreen-get-current-screen)))
         (buffList (elscreen-get-separate-buffer-list screen)))
    (if buffList
        (setq elscreen-separate-buffer-list buffList)
      (setq elscreen-separate-buffer-list (elscreen-make-default-separate-buffer-list)))))

(defun elscreen-add-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTにBUFFERを加える."
  (unless (member buffer elscreen-separate-buffer-list)
    (setq elscreen-separate-buffer-list (append (list buffer) elscreen-separate-buffer-list))
    (elscreen-separate-buffer-list-count-inc buffer)))

(defun elscreen-remove-separate-buffer-list (buffer)
  "SEPARATE-BUFFER-LISTからBUFFERを取り除く."
  (elscreen-separate-buffer-list-count-dec buffer)
  (setq elscreen-separate-buffer-list (loop for i in elscreen-separate-buffer-list
                                            unless (equal i buffer)
                                            collect i)))

(defun elscreen-update-separate-buffer-list ()
  "SEPARATE-BUFFER-LISTを更新する."
  (elscreen-separate-buffer-list-count-clean)
  (setq elscreen-separate-buffer-list (loop for i in elscreen-separate-buffer-list
                                            if (buffer-live-p i)
                                            collect i)))

(defun elscreen-separate-buffer-list-count-inc (buffer)
  "BUFFERのカウントを上げる."
  (loop for i in elscreen-separate-buffer-count-list
           if (equal (car i) buffer)
           do (setcdr i  (+ 1 (cdr i)))
           and return nil
           finally (push (cons buffer 1) elscreen-separate-buffer-count-list)))

(defun elscreen-separate-buffer-list-count-dec (buffer)
  "BUFFERのカウントを下げる."
  (setq elscreen-separate-buffer-count-list   (loop for i in elscreen-separate-buffer-count-list
                                                       if (equal (car i) buffer)
                                                       do (setcdr i  (- (cdr i) 1))
                                                       if (< 0 (cdr i))
                                                       collect i)))

(defun elscreen-separate-buffer-list-count (buffer)
  "BUFFERのカウントを返す."
  (loop for i in elscreen-separate-buffer-count-list
           if (equal (car i) buffer)
           return (cdr i)
           finally return 0))

(defun elscreen-separate-buffer-list-count-clean ()
  "BUFFER-COUNTの掃除をする."
  (setq elscreen-separate-buffer-count-list   (loop for i in elscreen-separate-buffer-count-list
                                                       if (buffer-live-p (car i))
                                                       collect i)))

(defun elscreen-goto:restore-separate-buffer-list (origin &rest args)
  "SCREENの切替時にSEPARATE-BUFFER-LIST,WINDOW-HISTORYを復元する."
  (elscreen-set-separate-window-history (elscreen-get-current-screen) (elscreen-get-all-window-history-alist))
  (apply origin args)
  (elscreen-restore-all-window-history-alist (elscreen-get-separate-window-history (elscreen-get-current-screen)))
  (when (elscreen-screen-live-p (elscreen-get-previous-screen))
    (elscreen-save-separate-buffer-list (elscreen-get-previous-screen)))
  (elscreen-restore-separate-buffer-list (elscreen-get-current-screen)))
(advice-add 'elscreen-goto :around 'elscreen-goto:restore-separate-buffer-list)

(defun elscreen-clone:clone-separate-buffer-list (&rest _)
  "SCREENの複製時にSEPARATE-BUFFER-LISTも複製する."
  (let ((stack (elscreen-get-separate-buffer-list (elscreen-get-previous-screen))))
    (elscreen-set-separate-buffer-list (elscreen-get-current-screen) stack)
    (elscreen-restore-separate-buffer-list (elscreen-get-current-screen))))
(advice-add 'elscreen-clone :after 'elscreen-clone:clone-separate-buffer-list)

(defun elscreen-kill:restore-separate-buffer-list (origin &rest args)
  "SCREENの削除時にBUFFERの削除、SEPARATE-BUFFER-LISTの復元をする."
  (mapc (lambda (buffer)
          (unless (member (buffer-name buffer) elscreen-separate-buffer-list-default)
            (kill-buffer buffer)))
        (buffer-list))
  (apply origin args)
  (elscreen-restore-separate-buffer-list (elscreen-get-current-screen)))
(advice-add 'elscreen-kill :around 'elscreen-kill:restore-separate-buffer-list)

(defun elscreen-separate-buffer-list-kill-buffer-hook ()
  "BUFFER削除時にSEPARATE-BUFFER-LISTからも削除する."
  (let ((buffer (current-buffer)))
    (when (member buffer elscreen-separate-buffer-list)
      (elscreen-remove-separate-buffer-list buffer)
      (if (> 1 (elscreen-separate-buffer-list-count buffer))
          t
        (bury-buffer)
        nil))))
(add-hook 'kill-buffer-query-functions 'elscreen-separate-buffer-list-kill-buffer-hook)

(defun elscreen-separate-buffer-list-buffer-list-update-hook ()
  "BUFFER-LIST更新時にSEPARATE-BUFFER-LISTも更新する."
  (elscreen-update-separate-buffer-list))
(add-hook 'buffer-list-update-hook 'elscreen-separate-buffer-list-buffer-list-update-hook)

(defun switch-to-buffer:elscreen-separate-buffer-list (buffer &rest _)
  "BUFFER切り替え時にSEPARATE-BUFFER-LISTに追加する."
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'switch-to-buffer :after 'switch-to-buffer:elscreen-separate-buffer-list)

(defun get-buffer-create:elscreen-separate-buffer-list (buffer &rest _)
  "GET-BUFFER-CREATE時にSEPARATE-BUFFER-LISTに追加する."
  (elscreen-add-separate-buffer-list (get-buffer buffer)))
(advice-add 'get-buffer-create :after 'get-buffer-create:elscreen-separate-buffer-list)

(defun buffer-list:return-separate-buffer-list (origin &rest _)
  "BUFFER-LISTが呼ばれた際にSEPARATE-BUFFER-LISTでフィルタリングを行う."
  (loop for i in (apply origin _)
           if (member (get-buffer i) elscreen-separate-buffer-list)
           collect i))
(advice-add 'buffer-list :around 'buffer-list:return-separate-buffer-list)

;; elscreenのパッチからパクってきた
(defun elscreen-window-history-supported-p ()
  (and (fboundp 'window-prev-buffers)
       (fboundp 'window-next-buffers)
       (fboundp 'set-window-prev-buffers)
       (fboundp 'set-window-next-buffers)))

(defun elscreen-get-all-window-history-alist ()
  (when (elscreen-window-history-supported-p)
    (mapcar (lambda (window)
              (let ((prevs (window-prev-buffers window))
                    (nexts (window-next-buffers window)))
                (cons window (cons prevs nexts))))
            (window-list))))

(defun elscreen-restore-all-window-history-alist (history-alist)
  (when (elscreen-window-history-supported-p)
    (mapc (lambda (entry)
            (let* ((window (car entry))
                   (histories (cdr entry))
                   (prevs (car histories))
                   (nexts (cdr histories)))
              (when (window-valid-p window)
                (set-window-prev-buffers window prevs)
                (set-window-next-buffers window nexts))))
          history-alist)))

;; 初期化

(defun elscreen-make-default-separate-buffer-list ()
  "デフォルトのバッファリストを作成する."
  (loop for i in elscreen-separate-buffer-list-default
           collect (get-buffer i)))

(defun elscreen-separate-buffer-list-init-hook ()
  "初期化関数.デフォルトのバッファリストを設定する."
  (setq elscreen-separate-buffer-list (elscreen-make-default-separate-buffer-list)))
(add-hook 'after-init-hook 'elscreen-separate-buffer-list-init-hook)

(provide 'elscreen-separate-buffer-list)

;;; elscreen-separate-buffer-list.el ends here
