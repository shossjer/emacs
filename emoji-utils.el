(require 'ht)

;; https://emacs.stackexchange.com/a/14716
(defun in-comment-p (pos)
  "Checks whether the position is at a comment."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (or (member 'font-lock-comment-face fontfaces)
        (member 'font-lock-comment-delimiter-face fontfaces))))

(defun parse-possible-emojis (fname)
  "Parse file for emojis."
  (let ((json-array-type 'list)
        (json-object-type 'hash-table))
    (mapcar (lambda (x) (list (string-trim x ":" ":"))) (ht-keys (json-read-file fname))))
  )

(defun get-possible-emojis ()
  "Get a list of all possible GitHub emojis.

Downloads the list of support emojis from githubs own API."
  (let* ((downloaddir "~/.emacs.d/downloads")
         (fname (concat downloaddir "/github-emojis.json")))
    (unless (file-readable-p fname)
      (mkdir downloaddir t)
      (url-copy-file "https://api.github.com/emojis" fname t))
    (parse-possible-emojis fname)))
