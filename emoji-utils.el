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
  (let ((emojis nil))
    (with-temp-buffer
      (insert-file-contents fname)
      (beginning-of-buffer)
      (while (re-search-forward "<span id=\"[^\"]*\" class=\"emoji\" data-src=\"[^\"]*\"></span>:<span class=\"name\" data-alternative-name=\"\\([^\"]*\\)\">\\([^<]*\\)</span>:</div></li>" nil t)
        (setf emojis (cons (cons (match-string 2) (split-string (match-string 1) ", ")) emojis)))
      emojis)))

(defun get-possible-emojis ()
  "Get a list of all possible GitHub emojis.

According to GitHub's own blog (see
https://github.blog/2012-10-12-emoji-autocomplete/) the official
list of emojis supported are given by WebFX (at
https://www.webfx.com/tools/emoji-cheat-sheet/). This function
fetches that list and parses it."
  (let* ((downloaddir "~/.emacs.d/downloads")
         (fname (concat downloaddir "/emoji-cheat-sheet")))
    (unless (file-readable-p fname)
      (mkdir downloaddir t)
      (url-copy-file "https://www.webfx.com/tools/emoji-cheat-sheet/" fname t))
    (parse-possible-emojis fname)))
