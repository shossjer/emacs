(defun find-cmake-root--impl (dirpath)
  "Recursive implementation step."
  (let* ((parent-directory (file-name-directory (directory-file-name dirpath)))
         (maybe-cmake-root (unless (string-equal dirpath parent-directory)
                             (find-cmake-root--impl parent-directory))))
    (if maybe-cmake-root
        maybe-cmake-root
      (when (file-exists-p (concat dirpath "CMakeLists.txt"))
        dirpath))))

(defun find-cmake-root (filepath)
  "Find the top most cmake directory."
  (let ((dirpath (file-name-directory filepath)))
    (find-cmake-root--impl dirpath)))

(defun get-project-folder (&optional filepath)
  "Return project directory for the given file (defaults to buffer-file-name)."
  (let ((filepath-or-bufferpath (or filepath (buffer-file-name))))
    (when filepath-or-bufferpath
      (find-cmake-root filepath-or-bufferpath))))

(defun get-project-or-local-folder (&optional filepath)
  "Return project directory for the given file (defaults to buffer-file-name), or the local directory."
  (let ((filepath-or-bufferpath (or filepath (buffer-file-name))))
    (when filepath-or-bufferpath
      (or (get-project-folder filepath-or-bufferpath)
          (file-name-directory filepath-or-bufferpath)))))
