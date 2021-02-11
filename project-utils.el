(require 'cl-extra)

(defcustom projext-toolchains
  nil
  "Alist of available toolchains."
  :group 'environment
  :type '(alist :key-type (symbol :tag "Name")
                :value-type (list (file :must-match t :tag "CC")
                                  (file :must-match t :tag "CXX")))
  )


(defun projext--prompt-buildtype (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "BuildType (default %s): " cached)
                   "BuildType: "))
         (buildtype (read-string prompt nil nil cached)))
    (plist-put settings prop buildtype))
  )

(defun projext--prompt-toolchain (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "Toolchain (default %s): " cached)
                   "Toolchain: "))
         (toolchain (completing-read prompt projext-toolchains nil t nil nil cached))
         (data (alist-get (intern toolchain) projext-toolchains)))
    (plist-put (plist-put (plist-put settings prop toolchain) 'tcc (nth 0 data)) 'tcxx (nth 1 data)))
  )

(defun projext--prompt-executable (settings prop)
  (let* ((cached (plist-get settings prop))
         (prompt (if cached
                     (format "Executable (default %s): " cached)
                   "Executable: "))
         (pdir (plist-get settings 'pdir))
         (executable (read-file-name prompt pdir nil t cached 'file-executable-p)))
    (plist-put settings prop executable))
  )

;; cmake-projext:
;;   configure:   toolchain btype
;;                CC=<CC> CXX=<CXX> cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=<btype> -S <pdir> -B <bdir>
;;   build:       (target)
;;                cmake --build <bdir> --target <target>
;;                cmake --build <bdir>

(defun projext--configure-cmake (settings)
  (let* ((settings-with-toolchain (projext--prompt-toolchain settings 'tname))
         (settings-with-buildtype (projext--prompt-buildtype settings-with-toolchain 'btype))
         (tcc (plist-get settings-with-buildtype 'tcc))
         (tcxx (plist-get settings-with-buildtype 'tcxx))
         (pdir (plist-get settings-with-buildtype 'pdir))
         (tname (plist-get settings-with-buildtype 'tname))
         (btype (plist-get settings-with-buildtype 'btype))
         (bdir (file-name-as-directory (concat pdir "build-" tname "-" btype)))
         (ccmd (concat "CC=" tcc " CXX=" tcxx " cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=" btype " -S " pdir " -B " bdir)))
    (plist-put (plist-put (plist-put settings-with-buildtype 'bdir bdir) 'ccmd ccmd) 'cstat t))
  )

(defun projext--build-cmake (settings)
  (let* ((bdir (plist-get settings 'bdir))
         (bcmd (concat "cmake --build " bdir)))
    (plist-put settings 'bcmd bcmd))
  )

;; make-projext:
;;   configure:   toolchain
;;   build:       (target)
;;                make CC=<CC> CXX=<CXX> -C <pdir> <target>
;;                make CC=<CC> CXX=<CXX> -C <pdir>

(defun projext--configure-make (settings)
  (let* ((settings-with-toolchain (projext--prompt-toolchain settings 'tname)))
    (plist-put settings-with-toolchain 'cstat t))
  )

(defun projext--build-make (settings)
  (let* ((tcc (plist-get settings 'tcc))
         (tcxx (plist-get settings 'tcxx))
         (pdir (plist-get settings 'pdir))
         (bcmd (concat "make CC=" tcc " CXX=" tcxx " -C " pdir)))
    (plist-put (plist-put settings 'bcmd bcmd) 'bstat t))
  )


(defun projext--run-binary (settings)
  (let* ((settings-with-executable (projext--prompt-executable settings 'rbin))
         (rbin (plist-get settings-with-executable 'rbin))
         (rcmd (concat rbin "")))
    (plist-put (plist-put settings-with-executable 'rcmd rcmd) 'rstat t)))

(setq projext-types '((ptype cmake pfiles ("CMakeLists.txt") pconf projext--configure-cmake pbuild projext--build-cmake)
                      (ptype make pfiles ("Makefile") pconf projext--configure-make pbuild projext--build-make)
                      (pfiles (".git"))))


(defun projext-find--impl (dir)
  "Recursive implementation step."
  (let* ((parentdir (file-name-directory (directory-file-name dir)))
         (parentproj (unless (string-equal dir parentdir)
                       (projext-find--impl parentdir))))
    (or parentproj
        (cl-some (lambda (type)
                   (let ((files (plist-get type 'pfiles)))
                     (when (cl-every (lambda (file)
                                       (file-exists-p (concat dir file)))
                                     files)
                       (cons 'pdir (cons dir type)))))
                 projext-types)))
  )

(defun projext-find ()
  "Find the top-most projext for the active buffer."
  (interactive)
  (let ((guessdir (or (and (vc-root-dir) (file-truename (vc-root-dir)))
                      (file-name-directory (buffer-file-name)))))
    (projext-find--impl guessdir))
  )


(setq projext-configure--cached-settings nil)

(defun projext-configure--get-cached-settings (projext)
  (let ((pdir (plist-get projext 'pdir)))
    (cl-some (lambda (x)
               (when (equal pdir (plist-get x 'pdir))
                 x))
             projext-configure--cached-settings))
  )

(defun projext-configure--set-cached-settings (settings)
  (let ((pdir (plist-get settings 'pdir)))
    (setq projext-configure--cached-settings (cons settings (cl-delete-if (lambda (x) (equal pdir (plist-get x 'pdir)))
                                                                          projext-configure--cached-settings))))
  )


(defun projext-configure (settings)
  "Configure projext.

Interactively configures the projext found by projext-find."
  (interactive
   (let* ((projext (or (projext-find) (error "Cannot find projext")))
          (cached-settings (projext-configure--get-cached-settings projext))
          (current-settings (or cached-settings projext)))
     (when (or current-prefix-arg
               (not (eq (plist-get cached-settings 'ptype) (plist-get projext 'ptype))))
       (setq current-settings (plist-put current-settings 'cstat nil)))
     (unless (plist-get current-settings 'cstat)
       (let ((conffn (or (plist-get current-settings 'pconf) (error "Cannot configure projext"))))
         (setq current-settings (funcall conffn current-settings))))
     (projext-configure--set-cached-settings current-settings)
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
(define-key global-map (kbd "C-c b c") 'projext-configure)

(defun projext-build (settings)
  "Build projext."
  (interactive
   (let* ((projext (or (projext-find) (error "Cannot find projext")))
          (cached-settings (projext-configure--get-cached-settings projext))
          (current-settings (or cached-settings projext)))
     (when (or current-prefix-arg
               (not (eq (plist-get cached-settings 'ptype) (plist-get projext 'ptype))))
       (setq current-settings (plist-put current-settings 'bstat nil)))
     (unless (plist-get current-settings 'bstat)
       (let ((buildfn (or (plist-get current-settings 'pbuild) (error "Cannot build projext"))))
         (setq current-settings (funcall buildfn current-settings))))
     (projext-configure--set-cached-settings current-settings)
     (list
      current-settings)))
  (let ((bcmd (plist-get settings 'bcmd)))
    (when bcmd
      (compile bcmd)))
  )
(define-key global-map (kbd "C-c b b") 'projext-build)

(defun projext-run (settings)
  "Run projext."
  (interactive
   (let* ((projext (or (projext-find) (error "Cannot find projext")))
          (cached-settings (projext-configure--get-cached-settings projext))
          (current-settings (or cached-settings projext)))
     (when current-prefix-arg
       (setq current-settings (plist-put current-settings 'rstat nil)))
     (unless (plist-get current-settings 'rstat)
       (setq current-settings (projext--run-binary current-settings)))
     (projext-configure--set-cached-settings current-settings)
     (list
      current-settings)))
  (let ((rcmd (plist-get settings 'rcmd)))
    (when rcmd
      (compile rcmd)))
  )
(define-key global-map (kbd "C-c b r") 'projext-run)
