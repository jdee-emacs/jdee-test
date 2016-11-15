(require 'cl)

(defun slurp (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun expand-file-name-up (file dir)
  (when dir
    (let ((rtnval (expand-file-name file dir)))
      (if (file-exists-p rtnval)
	  rtnval 
	(expand-file-name-up file (file-name-directory (directory-file-name dir)))))))

(defvar maven-local-repo (expand-file-name "~/.m2/repository"))

(defvar maven-repo "http://repo1.maven.org/maven2")

(defun maven-gavl-filename (artifact version extension &optional classifier)
  (format "%s-%s%s%s"
	  artifact version
	  (if classifier (format "-%s" classifier) "")
	  extension))
	  

(defun maven-gavl-to-url (repo gavl+ &optional extension  classifier)
  (let ((group-slashed (car gavl+))
	(group-dotted (cadr gavl+))
	(artifact (caddr gavl+))
	(version (cadddr gavl+)))
		   
    (format "%s/%s/%s/%s/%s" repo
	    group-slashed
	    artifact version (maven-gavl-filename artifact version
						  (or extension ".jar")
						  classifier))))


(defun slashify (group)
  (replace-regexp-in-string "[.]" "/" group))

(defun split-gav (gav)
  "Return (group-slashed group-dotted artifact version)"
  (let ((rtnval (split-string gav)))
    rtnval))
    
    
(defvar maven-extra-extensions '("asc" "asc.md5" "asc.sha1" "md5" "sha1"))

(defvar maven-extension-alist
  '(("pom")
    ("jar" "sources" "javadoc")))


(defun maven-extensions* (base &optional classifier)
  "Return extension"
  
  (let ((rtnval  (cons (format ".%s" base)
		       
		       (mapcar (lambda (e)
				 (format "%s%s%s"
				       (if classifier (format "-%s" classifier) "")
				       (format ".%s" base)
				       (if e (format ".%s" e) ""))) maven-extra-extensions))))
    (if classifier
	(cons (format "-%s.%s" classifier base) rtnval)
      rtnval)))

(defun maven-extensions (base &rest classifiers)
  (if (and classifiers (not (equal classifiers '(nil))))
      (append (maven-extensions* base (car classifiers))
	      (apply #'maven-extensions base (cdr classifiers)))
    (maven-extensions* base)))

(defun maven-default-alist (classifiers)
  (list
   (list "pom")
   (cons "jar" classifiers)))

(defun maven-get-dep (group artifact version &rest classifiers)
  (let* ((slashed (slashify group))
	 (gavl+ (list slashed group artifact version)))
    (dolist (ext (cl-reduce #'append (mapcar (lambda (a) (apply #'maven-extensions a))
					  (maven-default-alist classifiers))))
      (let* ((source (maven-gavl-to-url maven-repo gavl+ ext))
	     (url-request-method "GET")
	     (dest (maven-gavl-to-url maven-local-repo gavl+ ext)))
	(unless (file-exists-p dest)
	  (make-directory (file-name-directory dest) t)
	  (if (url-file-exists-p source)
	      (url-copy-file source dest t)
	    (when (file-exists-p dest)
	      (delete-file dest))))))))
  

(defun maven-get-deps (gavls)
  ""
  (let ((reporter (make-progress-reporter "Fetching artifacts: " 0 (length gavls) 0))
	(i 0))
    (dolist (gavl gavls)
      (setq i (+ 1 i))
      (progress-reporter-update reporter i)
      (apply #'maven-get-dep (append gavl '("sources" "javadoc"))))
    (progress-reporter-done reporter)))


        ;; <dependency>
        ;;     <groupId>org.beanshell</groupId>
        ;;     <artifactId>bsh</artifactId>
        ;;     <version>2.0b5</version>
        ;; </dependency>

        ;; <dependency>
        ;;     <groupId>junit</groupId>
        ;;     <artifactId>junit</artifactId>
        ;;     <version>4.12</version>
        ;; </dependency>
(defun gavl-to-dependency (gavl)
  (let ((group (car gavl))
	(artifact (cadr gavl))
	(version (caddr gavl)))
    (format "<dependency>\n<groupId>%s</groupId>\n<artifactId>%s</artifactId>\n<version>%s</version>\n</dependency>\n" group artifact version)))

(defun gavl-to-jar (gavl)
  (let ((gavl+ (cons (slashify (car gavl)) gavl)))
    (format "\"%s\"\n"
	    (maven-gavl-to-url maven-local-repo gavl+))))

  
(defun apply-template (project-dir fn template dest gavls)
  (let ((jars (cl-reduce #'concat (mapcar fn gavls) :initial-value "")))
    (with-temp-buffer
      (insert
       (format-spec template (format-spec-make ?c jars
					       ?p (directory-file-name project-dir))))
      (write-file (expand-file-name dest project-dir) nil)
      (set-auto-mode)
      (indent-region (point-min) (point-max))
      (write-file (expand-file-name dest project-dir) nil))))

      

(defun make-prj-el (&optional prj-dir)
  "Create a prj.el from global-classpath.gav"
  (let* ((README (expand-file-name-up "README.md" (or prj-dir default-directory)))
	 (elisp-dir (expand-file-name-up "src/main/elisp" (or prj-dir default-directory)))
	 (project-dir (file-name-directory README))
	 (gavs (read-lines (expand-file-name "global-classpath.gav" elisp-dir)))
	 (template (slurp (expand-file-name "prj.el.template" project-dir)))
	 (gavls (mapcar #'split-gav gavs)))
    (apply-template project-dir #'gavl-to-jar template "prj.el" gavls) 
    (maven-get-deps gavls)))

    
	
(defun make-pom-xml (&optional prj-dir)
  "Create a pom.xml from global-classpath.gav"
  (let* ((README (expand-file-name-up "README.md" (or prj-dir default-directory)))
	 (elisp-dir (expand-file-name-up "src/main/elisp" (or prj-dir default-directory)))
	 (project-dir (file-name-directory README))
	 (gavs (read-lines (expand-file-name "global-classpath.gav" elisp-dir)))
	 (template (slurp (expand-file-name "pom.xml.template" project-dir)))
	 (gavls (mapcar #'split-gav gavs)))
    (apply-template project-dir #'gavl-to-dependency template "pom.xml" gavls)))


    
	
    
