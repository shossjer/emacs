(require 'cl-extra)

(defcustom project-toolchains
  nil
  "Alist of available toolchains."
  :group 'environment
  :type '(alist :key-type (symbol :tag "Name")
                :value-type (list (file :must-match t :tag "CC")
                                  (file :must-match t :tag "CXX")))
  )


(defun project--prompt-buildtype (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "BuildType (default %s): " cached)
                   "BuildType: "))
         (buildtype (read-string prompt nil nil cached)))
    (plist-put settings prop buildtype))
  )

(defun project--prompt-toolchain (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "Toolchain (default %s): " cached)
                   "Toolchain: "))
         (toolchain (completing-read prompt project-toolchains nil t nil nil cached))
         (data (alist-get (intern toolchain) project-toolchains)))
    (plist-put (plist-put (plist-put settings prop toolchain) 'tcc (nth 0 data)) 'tcxx (nth 1 data)))
  )

(defun project--prompt-executable (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "Executable (default %s): " cached)
                   "Executable: "))
         (pdir (plist-get settings 'pdir))
         (executable (read-file-name prompt pdir nil t cached 'file-executable-p)))
    (plist-put settings prop executable))
  )

;; cmake-project:
;;   configure:   toolchain btype
;;                CC=<CC> CXX=<CXX> cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=<btype> -S <pdir> -B <bdir>
;;   build:       (target)
;;                cmake --build <bdir> --target <target>
;;                cmake --build <bdir>

(defun project--configure-cmake (settings)
  (let* ((settings-with-toolchain (project--prompt-toolchain settings 'tname))
         (settings-with-buildtype (project--prompt-buildtype settings-with-toolchain 'btype))
         (tcc (plist-get settings-with-buildtype 'tcc))
         (tcxx (plist-get settings-with-buildtype 'tcxx))
         (pdir (plist-get settings-with-buildtype 'pdir))
         (tname (plist-get settings-with-buildtype 'tname))
         (btype (plist-get settings-with-buildtype 'btype))
         (bdir (file-name-as-directory (concat pdir "build-" tname "-" btype)))
         (ccmd (concat "CC=" tcc " CXX=" tcxx " cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=" btype " -S " pdir " -B " bdir)))
    (plist-put (plist-put (plist-put settings-with-buildtype 'bdir bdir) 'ccmd ccmd) 'cstat t))
  )

(defun project--build-cmake (settings)
  (let* ((bdir (plist-get settings 'bdir))
         (bcmd (concat "cmake --build " bdir)))
    (plist-put settings 'bcmd bcmd))
  )

;; make-project:
;;   configure:   toolchain
;;   build:       (target)
;;                make CC=<CC> CXX=<CXX> -C <pdir> <target>
;;                make CC=<CC> CXX=<CXX> -C <pdir>

(defun project--configure-make (settings)
  (let* ((settings-with-toolchain (project--prompt-toolchain settings 'tname)))
    (plist-put settings-with-toolchain 'cstat t))
  )

(defun project--build-make (settings)
  (let* ((tcc (plist-get settings 'tcc))
         (tcxx (plist-get settings 'tcxx))
         (pdir (plist-get settings 'pdir))
         (bcmd (concat "make CC=" tcc " CXX=" tcxx " -C " pdir)))
    (plist-put (plist-put settings 'bcmd bcmd) 'bstat t))
  )


(defun project--run-binary (settings)
  (let* ((settings-with-executable (project--prompt-executable settings 'rbin))
         (rbin (plist-get settings-with-executable 'rbin))
         (rcmd (concat rbin "")))
    (plist-put (plist-put settings-with-executable 'rcmd rcmd) 'rstat t)))

(setq project-types '((ptype cmake pfiles ("CMakeLists.txt") pconf project--configure-cmake pbuild project--build-cmake)
                      (ptype make pfiles ("Makefile") pconf project--configure-make pbuild project--build-make)
                      (pfiles (".git"))))


(defun project-find--impl (dir)
  "Recursive implementation step."
  (let* ((parentdir (file-name-directory (directory-file-name dir)))
         (parentproj (unless (string-equal dir parentdir)
                       (project-find--impl parentdir))))
    (or parentproj
        (cl-some (lambda (type)
                   (let ((files (plist-get type 'pfiles)))
                     (when (cl-every (lambda (file)
                                       (file-exists-p (concat dir file)))
                                     files)
                       (cons 'pdir (cons dir type)))))
                 project-types)))
  )

(defun project-find ()
  "Find the top-most project for the active buffer."
  (interactive)
  (let ((guessdir (or (and (vc-root-dir) (file-truename (vc-root-dir)))
                      (file-name-directory (buffer-file-name)))))
    (project-find--impl guessdir))
  )


(setq project-configure--cached-settings nil)

(defun project-configure--get-cached-settings (project)
  (let ((pdir (plist-get project 'pdir)))
    (cl-some (lambda (x)
               (when (equal pdir (plist-get x 'pdir))
                 x))
             project-configure--cached-settings))
  )

(defun project-configure--set-cached-settings (settings)
  (let ((pdir (plist-get settings 'pdir)))
    (setq project-configure--cached-settings (cons settings (cl-delete-if (lambda (x) (equal pdir (plist-get x 'pdir)))
                                                                          project-configure--cached-settings))))
  )


(defun project-configure (settings)
  "Configure project.

Interactively configures the project found by project-find."
  (interactive
   (let* ((project (or (project-find) (error "Cannot find project")))
          (cached-settings (project-configure--get-cached-settings project))
          (current-settings (or cached-settings project)))
     (when (or current-prefix-arg
               (not (eq (plist-get cached-settings 'ptype) (plist-get project 'ptype))))
       (setq current-settings (plist-put current-settings 'cstat nil)))
     (unless (plist-get current-settings 'cstat)
       (let ((conffn (or (plist-get current-settings 'pconf) (error "Cannot configure project"))))
         (setq current-settings (funcall conffn current-settings))))
     (project-configure--set-cached-settings current-settings)
     (list
      current-settings)))
  (let ((bdir (plist-get settings 'bdir))
        (ccmd (plist-get settings 'ccmd)))
    (when bdir
      (unless (file-accessible-directory-p bdir)
        (message "Making directory %s" bdir)
        (make-directory bdir)))
    (when ccmd
      (compile ccmd)))
  )
(define-key global-map (kbd "C-c b c") 'project-configure)

(defun project-build (settings)
  "Build project."
  (interactive
   (let* ((project (or (project-find) (error "Cannot find project")))
          (cached-settings (project-configure--get-cached-settings project))
          (current-settings (or cached-settings project)))
     (when (or current-prefix-arg
               (not (eq (plist-get cached-settings 'ptype) (plist-get project 'ptype))))
       (setq current-settings (plist-put current-settings 'bstat nil)))
     (unless (plist-get current-settings 'bstat)
       (let ((buildfn (or (plist-get current-settings 'pbuild) (error "Cannot build project"))))
         (setq current-settings (funcall buildfn current-settings))))
     (project-configure--set-cached-settings current-settings)
     (list
      current-settings)))
  (let ((bcmd (plist-get settings 'bcmd)))
    (when bcmd
      (compile bcmd)))
  )
(define-key global-map (kbd "C-c b b") 'project-build)

(defun project-run (settings)
  "Run project."
  (interactive
   (let* ((project (or (project-find) (error "Cannot find project")))
          (cached-settings (project-configure--get-cached-settings project))
          (current-settings (or cached-settings project)))
     (when current-prefix-arg
       (setq current-settings (plist-put current-settings 'rstat nil)))
     (unless (plist-get current-settings 'rstat)
       (setq current-settings (project--run-binary current-settings)))
     (project-configure--set-cached-settings current-settings)
     (list
      current-settings)))
  (let ((rcmd (plist-get settings 'rcmd)))
    (when rcmd
      (compile rcmd)))
  )
(define-key global-map (kbd "C-c b r") 'project-run)
