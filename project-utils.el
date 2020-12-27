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

(defun get-project-or-local-folder ()
  "Return project dir if applicable and local dir otherwise."
  (let ((filepath (buffer-file-name)))
    (when filepath
      (let ((project-folder (find-cmake-root filepath)))
        (if project-folder
            project-folder
          (file-name-directory filepath))))))
